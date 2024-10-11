//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

#[cfg(target_os = "linux")]
mod mntent;

#[cfg(target_os = "linux")]
use crate::mntent::MountTable;

use ftw::{metadata, symlink_metadata_cstr, FilesystemStatistics};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
#[cfg(target_os = "macos")]
use std::ffi::CStr;
use std::os::unix::fs::MetadataExt;
use std::{cmp, ffi::CString, fmt::Display, io};

#[derive(Parser)]
#[command(version, about = gettext("df - report free storage space"))]
struct Args {
    #[arg(
        short,
        long,
        help = gettext("Use 1024-byte units, instead of the default 512-byte units, when writing space figures")
    )]
    kilo: bool,

    #[arg(
        short = 'P',
        long,
        help = gettext("Write information in a portable output format")
    )]
    portable: bool,

    #[arg(
        short,
        long,
        help = gettext("Include total allocated-space figures in the output")
    )]
    total: bool,

    #[arg(
        help = gettext("A pathname of a file within the hierarchy of the desired file system")
    )]
    files: Vec<String>,
}

#[derive(Debug)]
pub struct Mount {
    /// mount point
    pub target: CString,
    /// file system
    pub source: CString,
    pub fsstat: FilesystemStatistics,
    pub dev: i64,
    pub masked: bool,
}

impl Mount {
    pub fn new(dir: CString, fsname: CString, fsstat: FilesystemStatistics) -> Self {
        Self {
            target: dir,
            source: fsname,
            fsstat,
            dev: -1,
            masked: true,
        }
    }
}

#[cfg(target_os = "macos")]
fn to_cstr(array: &[libc::c_char]) -> &CStr {
    unsafe {
        // Assuming the array is null-terminated, as it should be for C strings.
        CStr::from_ptr(array.as_ptr())
    }
}

pub struct MountList {
    info: Vec<Mount>,
}

impl MountList {
    #[cfg(target_os = "linux")]
    pub fn new() -> io::Result<Self> {
        let mut info = vec![];

        let mounts = MountTable::open_system()?;
        for mount in mounts {
            let fsstat = match FilesystemStatistics::new_cstr(mount.dir.as_c_str()) {
                Ok(f) => f,
                Err(e) => {
                    eprintln!("{}: {}", mount.dir.to_str().unwrap(), e);
                    continue;
                }
            };
            info.push(Mount::new(mount.dir, mount.fsname, fsstat));
        }

        Ok(MountList { info })
    }

    #[cfg(target_os = "macos")]
    pub fn new() -> io::Result<Self> {
        let mut info = vec![];

        unsafe {
            let mut mounts: *mut libc::statfs = std::ptr::null_mut();
            let n_mnt = libc::getmntinfo(&mut mounts, libc::MNT_WAIT);
            if n_mnt < 0 {
                return Err(io::Error::last_os_error());
            }

            let mounts: &[libc::statfs] = std::slice::from_raw_parts(mounts as _, n_mnt as _);
            for mount in mounts {
                let dir = to_cstr(&mount.f_mntonname).into();
                let fsname = to_cstr(&mount.f_mntfromname).into();
                let fsstat = FilesystemStatistics::from(mount);
                info.push(Mount::new(dir, fsname, fsstat));
            }
        }

        Ok(MountList { info })
    }

    pub fn mask_by_files(&mut self, files: Files) {
        if files.devs.is_empty() {
            return;
        }

        for mount in &mut self.info {
            mount.dev = if let Ok(m) = symlink_metadata_cstr(&mount.source) {
                m.rdev() as i64
            } else if let Ok(m) = symlink_metadata_cstr(&mount.target) {
                m.dev() as i64
            } else {
                -1
            };
            mount.masked = false;
        }

        for dev in files.devs {
            for mount in &mut self.info {
                if mount.dev == dev {
                    mount.masked = true;
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Files {
    pub devs: Vec<i64>,
}

impl Files {
    pub fn new(files: Vec<String>) -> Self {
        let mut devs = vec![];

        for path in files {
            let meta = match metadata(&path) {
                Ok(m) => m,
                Err(e) => {
                    eprintln!("{}: {}", path, e);
                    continue;
                }
            };
            devs.push(meta.dev() as i64);
        }

        Self { devs }
    }
}

pub enum FieldType {
    Str,
    Num,
    Pcent,
}

pub struct Field {
    caption: String,
    width: usize,
    typ: FieldType,
}

impl Field {
    pub fn new(caption: String, min_width: usize, typ: FieldType) -> Self {
        let width = cmp::max(caption.len(), min_width);
        Self {
            caption,
            width,
            typ,
        }
    }

    pub fn format<T: Display>(&self, value: &T) -> String {
        match self.typ {
            FieldType::Str => format!("{value: <width$}", width = self.width),
            FieldType::Num => format!("{value: >width$}", width = self.width),
            FieldType::Pcent => format!("{value: >width$}", width = self.width - 1),
        }
    }
}

/// Print header
impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format(&self.caption))
    }
}

pub struct Fields {
    /// file system
    pub source: Field,
    /// FS size
    pub size: Field,
    /// FS size used
    pub used: Field,
    /// FS size available
    pub avail: Field,
    /// percent used
    pub pcent: Field,
    /// mount point
    pub target: Field,
    // /// specified file name
    // file: Field,
}

impl Fields {
    pub fn new(block_size: u64) -> Self {
        let size_caption = format!("{}-{}", block_size, gettext("blocks"));
        Self {
            source: Field::new(gettext("Filesystem"), 14, FieldType::Str),
            size: Field::new(size_caption, 10, FieldType::Num),
            used: Field::new(gettext("Used"), 10, FieldType::Num),
            avail: Field::new(gettext("Available"), 10, FieldType::Num),
            pcent: Field::new(gettext("Capacity"), 5, FieldType::Pcent),
            target: Field::new(gettext("Mounted on"), 0, FieldType::Str),
        }
    }
}

/// Print header
impl Display for Fields {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {} {} {} {}",
            self.source, self.size, self.used, self.avail, self.pcent, self.target
        )
    }
}

pub struct FieldsData<'a> {
    fields: &'a Fields,
    source: String,
    size: u64,
    used: u64,
    avail: u64,
    pcent: u32,
    target: String,
}

impl<'a> FieldsData<'a> {
    pub fn new(fields: &'a Fields, mount: &Mount, block_size: u64) -> Self {
        let blksz = mount.fsstat.bsize();

        let total = (mount.fsstat.blocks() * blksz) / block_size;
        let avail = (mount.fsstat.bavail() * blksz) / block_size;
        let free = (mount.fsstat.bfree() * blksz) / block_size;
        let used = total - free;

        // The percentage value shall be expressed as a positive integer,
        // with any fractional result causing it to be rounded to the next highest integer.
        let percentage_used = f64::from(used as u32) / f64::from((used + free) as u32);
        let percentage_used = percentage_used * 100.0;
        let percentage_used = percentage_used.ceil() as u32;

        FieldsData {
            fields,
            source: String::from(mount.source.to_str().unwrap()),
            size: total,
            used,
            avail,
            pcent: percentage_used,
            target: String::from(mount.target.to_str().unwrap()),
        }
    }
}

impl Display for FieldsData<'_> {
    // The remaining output with -P shall consist of one line of information
    // for each specified file system. These lines shall be formatted as follows:
    // "%s %d %d %d %d%% %s\n", <file system name>, <total space>,
    //     <space used>, <space free>, <percentage used>,
    //     <file system root>
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {} {} {}% {}",
            self.fields.source.format(&self.source),
            self.fields.size.format(&self.size),
            self.fields.used.format(&self.used),
            self.fields.avail.format(&self.avail),
            self.fields.pcent.format(&self.pcent),
            self.fields.target.format(&self.target)
        )
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    // The format of the default output from df is unspecified,
    // but all space figures are reported in 512-byte units
    let block_size: u64 = if args.kilo { 1024 } else { 512 };

    let mut info = MountList::new()?;
    let files = Files::new(args.files);
    info.mask_by_files(files);

    let fields = Fields::new(block_size);
    // Print header
    println!("{}", fields);

    for mount in &info.info {
        if mount.masked {
            let row = FieldsData::new(&fields, mount, block_size);
            println!("{}", row);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_only_one_row() {
        let mut info = MountList::new().unwrap();
        let files = Files::new(vec!["/tmp/".into()]);
        dbg!(&files);
        info.mask_by_files(files);

        let mut count = 0;
        for mount in &info.info {
            if mount.masked {
                dbg!(&mount);
                count += 1;
            }
        }
        assert_eq!(count, 1);
    }
}
