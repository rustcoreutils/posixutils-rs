//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate libc;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::ffi::{CStr, CString};
use std::io;

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
            Err(io::Error::from_raw_os_error(rc))
        }
    }
}

struct Mount {
    devname: String,
    dir: String,
    dev: libc::dev_t,
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
                st.st_rdev
            } else if let Ok(st) = stat(dirname.to_str().unwrap()) {
                st.st_dev
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

fn read_mount_info() -> io::Result<MountList> {
    let mut info = MountList::new();

    unsafe {
        let mut mounts: *mut libc::statfs = std::ptr::null_mut();
        let n_mnt = libc::getmntinfo(&mut mounts, libc::MNT_WAIT);
        if n_mnt < 0 {
            return Err(io::Error::from_raw_os_error(n_mnt));
        }

        let mounts: &[libc::statfs] = std::slice::from_raw_parts(mounts as _, n_mnt as _);
        for mount in mounts {
            let devname = to_cstr(&mount.f_mntfromname);
            let dirname = to_cstr(&mount.f_mntonname);
            info.push(&mount, devname, dirname);
        }
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
        if stat.st_dev == mount.dev {
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
            show_mount(args, block_size, &mount);
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

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
