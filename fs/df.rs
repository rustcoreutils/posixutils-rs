//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::ffi::{CStr, CString};
use std::io;

#[cfg(target_os = "linux")]
const _PATH_MOUNTED: &'static str = "/etc/mtab";

/// df - report free storage space
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Use 1024-byte units, instead of the default 512-byte units, when writing space figures.
    #[arg(short, long)]
    kilo: bool,

    /// Write information in a portable output format
    #[arg(short = 'P', long)]
    portable: bool,

    /// Include total allocated-space figures in the output.
    #[arg(short, long)]
    total: bool,

    /// A pathname of a file within the hierarchy of the desired file system.
    files: Vec<String>,
}

#[cfg(target_os = "macos")]
fn to_cstr(array: &[libc::c_char]) -> &CStr {
    unsafe {
        // Assuming the array is null-terminated, as it should be for C strings.
        CStr::from_ptr(array.as_ptr())
    }
}

fn stat(filename_str: &str) -> io::Result<libc::stat> {
    let filename = CString::new(filename_str).unwrap();

    unsafe {
        let mut st: libc::stat = std::mem::zeroed();
        let rc = libc::stat(filename.as_ptr(), &mut st);
        if rc == 0 {
            Ok(st)
        } else {
            Err(io::Error::last_os_error())
        }
    }
}

struct Mount {
    devname: String,
    dir: String,
    dev: i64,
    masked: bool,
    cached_statfs: libc::statfs,
}

struct MountList {
    mounts: Vec<Mount>,
    has_masks: bool,
}

impl MountList {
    fn new() -> MountList {
        MountList {
            mounts: Vec::new(),
            has_masks: false,
        }
    }

    fn mask_all(&mut self) {
        for mount in &mut self.mounts {
            mount.masked = true;
        }
    }

    fn ensure_masked(&mut self) {
        if !self.has_masks {
            self.mask_all();
            self.has_masks = true;
        }
    }

    fn push(&mut self, fsstat: &libc::statfs, devname: &CStr, dirname: &CStr) {
        let dev = {
            if let Ok(st) = stat(devname.to_str().unwrap()) {
                st.st_rdev as i64
            } else if let Ok(st) = stat(dirname.to_str().unwrap()) {
                st.st_dev as i64
            } else {
                -1
            }
        };

        self.mounts.push(Mount {
            devname: String::from(devname.to_str().unwrap()),
            dir: String::from(dirname.to_str().unwrap()),
            dev,
            masked: false,
            cached_statfs: *fsstat,
        });
    }
}

#[cfg(target_os = "macos")]
fn read_mount_info() -> io::Result<MountList> {
    let mut info = MountList::new();

    unsafe {
        let mut mounts: *mut libc::statfs = std::ptr::null_mut();
        let n_mnt = libc::getmntinfo(&mut mounts, libc::MNT_WAIT);
        if n_mnt < 0 {
            return Err(io::Error::last_os_error());
        }

        let mounts: &[libc::statfs] = std::slice::from_raw_parts(mounts as _, n_mnt as _);
        for mount in mounts {
            let devname = to_cstr(&mount.f_mntfromname);
            let dirname = to_cstr(&mount.f_mntonname);
            info.push(mount, devname, dirname);
        }
    }

    Ok(info)
}

#[cfg(target_os = "linux")]
fn read_mount_info() -> io::Result<MountList> {
    let mut info = MountList::new();

    unsafe {
        let path_mnt = CString::new(_PATH_MOUNTED).unwrap();
        let mnt_mode = CString::new("r").unwrap();
        let f = libc::setmntent(path_mnt.as_ptr(), mnt_mode.as_ptr());
        if f.is_null() {
            return Err(io::Error::last_os_error());
        }

        loop {
            let me = libc::getmntent(f);
            if me.is_null() {
                break;
            }

            let me_devname = (*me).mnt_fsname;
            let me_dirname = (*me).mnt_dir;
            let devname = CStr::from_ptr(me_devname);
            let dirname = CStr::from_ptr(me_dirname);

            let mut mount: libc::statfs = std::mem::zeroed();
            let rc = libc::statfs(dirname.as_ptr(), &mut mount);
            if rc < 0 {
                eprintln!(
                    "{}: {}",
                    dirname.to_str().unwrap(),
                    io::Error::last_os_error()
                );
                continue;
            }

            info.push(&mount, devname, dirname);
        }

        libc::endmntent(f);
    }

    Ok(info)
}

fn mask_fs_by_file(info: &mut MountList, filename: &str) -> io::Result<()> {
    let stat_res = stat(filename);
    if let Err(e) = stat_res {
        eprintln!("{}: {}", filename, e);
        return Err(e);
    }
    let stat = stat_res.unwrap();

    for mount in &mut info.mounts {
        if stat.st_dev as i64 == mount.dev {
            info.has_masks = true;
            mount.masked = true;
        }
    }

    Ok(())
}

fn show_mount(args: &Args, block_size: u64, mount: &Mount) {
    let sf = &mount.cached_statfs;

    let blksz = sf.f_bsize as u64;

    let total = (sf.f_blocks * blksz) / block_size;
    let avail = (sf.f_bavail * blksz) / block_size;
    let free = (sf.f_bfree * blksz) / block_size;
    let used = total - free;

    if total == 0 {
        return;
    }

    let pct = ((total - avail) * 100) / total;

    if args.portable {
        println!(
            "{:>20} {:>9} {:>9} {:>9} {:>7} {}",
            mount.devname, total, used, avail, pct, mount.dir
        );
    } else {
        println!(
            "{:>20} {:>9} {:>9} {:>9} {:>3} {}",
            mount.devname, total, used, avail, pct, mount.dir
        );
    }
}

fn show_info(args: &Args, info: &MountList) {
    let block_size: u64 = match args.kilo {
        true => 1024,
        false => 512,
    };

    if args.portable {
        println!(
            "Filesystem         {:>4}-blocks      Used Available Capacity Mounted on",
            block_size
        );
    } else {
        println!(
            "Filesystem         {:>4}-blocks      Used Available Use % Mounted on",
            block_size
        );
    }

    for mount in &info.mounts {
        if mount.masked {
            show_mount(args, block_size, mount);
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut info = read_mount_info()?;

    if args.files.is_empty() {
        info.mask_all();
    } else {
        for file in &args.files {
            mask_fs_by_file(&mut info, file)?;
        }
    }

    info.ensure_masked();
    show_info(&args, &info);

    Ok(())
}
