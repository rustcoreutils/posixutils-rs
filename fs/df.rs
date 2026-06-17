//
// Copyright (c) 2024-2026 Jeff Garzik
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

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
#[cfg(target_os = "macos")]
use std::ffi::CStr;
use std::ffi::{CString, OsStr, OsString};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::{cmp, fmt::Display, io};

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
        short = 't',
        long,
        conflicts_with = "portable",
        help = gettext("Include total allocated-space figures in the output")
    )]
    total: bool,

    #[arg(
        help = gettext("A pathname of a file within the hierarchy of the desired file system")
    )]
    files: Vec<PathBuf>,
}

/// Display modes
pub enum OutputMode {
    /// When both the -k and -P options are specified
    Posix,
    /// When the -P option is specified without the -k option
    PosixLegacy,
    /// The format of the default output from df is unspecified,
    /// but all space figures are reported in 512-byte units
    Unspecified,
    Unspecified1K,
}

impl OutputMode {
    pub fn new(kilo: bool, portable: bool) -> Self {
        match (kilo, portable) {
            (true, true) => Self::Posix,
            (true, false) => Self::Unspecified1K,
            (false, true) => Self::PosixLegacy,
            (false, false) => Self::Unspecified,
        }
    }

    pub fn get_block_size(&self) -> u64 {
        match self {
            OutputMode::Posix | OutputMode::Unspecified1K => 1024,
            OutputMode::PosixLegacy | OutputMode::Unspecified => 512,
        }
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
    pub mode: OutputMode,
    /// Whether the inode columns are shown (default & -t modes, not -k/-P).
    pub inodes: bool,
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
    /// total inodes (file slots)
    pub itotal: Field,
    /// used inodes
    pub iused: Field,
    /// free inodes
    pub ifree: Field,
    /// percent inodes used
    pub ipcent: Field,
    /// mount point
    pub target: Field,
}

impl Fields {
    pub fn new(mode: OutputMode, inodes: bool) -> Self {
        let size_caption = format!("{}-{}", mode.get_block_size(), gettext("blocks"));
        Self {
            mode,
            inodes,
            source: Field::new(gettext("Filesystem"), 14, FieldType::Str),
            size: Field::new(size_caption, 10, FieldType::Num),
            used: Field::new(gettext("Used"), 10, FieldType::Num),
            avail: Field::new(gettext("Available"), 10, FieldType::Num),
            pcent: Field::new(gettext("Capacity"), 5, FieldType::Pcent),
            itotal: Field::new(gettext("Inodes"), 10, FieldType::Num),
            iused: Field::new(gettext("IUsed"), 10, FieldType::Num),
            ifree: Field::new(gettext("IFree"), 10, FieldType::Num),
            ipcent: Field::new(gettext("IUse%"), 5, FieldType::Pcent),
            target: Field::new(gettext("Mounted on"), 0, FieldType::Str),
        }
    }
}

/// Print header
impl Display for Fields {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {} {} {}",
            self.source, self.size, self.used, self.avail, self.pcent
        )?;
        if self.inodes {
            write!(
                f,
                " {} {} {} {}",
                self.itotal, self.iused, self.ifree, self.ipcent
            )?;
        }
        write!(f, " {}", self.target)
    }
}

pub struct FieldsData<'a> {
    pub fields: &'a Fields,
    pub source: String,
    pub size: u64,
    pub used: u64,
    pub avail: u64,
    pub pcent: u32,
    pub itotal: u64,
    pub iused: u64,
    pub ifree: u64,
    pub ipcent: u32,
    pub target: String,
}

impl Display for FieldsData<'_> {
    // The remaining output with -P shall consist of one line of information
    // for each specified file system. These lines shall be formatted as follows:
    // "%s %d %d %d %d%% %s\n", <file system name>, <total space>,
    //     <space used>, <space free>, <percentage used>,
    //     <file system root>
    //
    // In default and -t modes the inode (file-slot) columns are inserted
    // before <file system root>; -k and -P keep the fixed six-column format.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {} {} {}%",
            self.fields.source.format(&self.source),
            self.fields.size.format(&self.size),
            self.fields.used.format(&self.used),
            self.fields.avail.format(&self.avail),
            self.fields.pcent.format(&self.pcent),
        )?;
        if self.fields.inodes {
            write!(
                f,
                " {} {} {} {}%",
                self.fields.itotal.format(&self.itotal),
                self.fields.iused.format(&self.iused),
                self.fields.ifree.format(&self.ifree),
                self.fields.ipcent.format(&self.ipcent),
            )?;
        }
        write!(f, " {}", self.fields.target.format(&self.target))
    }
}

#[cfg(target_os = "macos")]
fn to_cstr(array: &[libc::c_char]) -> &CStr {
    unsafe {
        // Assuming the array is null-terminated, as it should be for C strings.
        CStr::from_ptr(array.as_ptr())
    }
}

fn stat(filename: &CString) -> io::Result<libc::stat> {
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

/// Scale a count of `bsize`-byte blocks into whole `unit`-byte output units.
///
/// A 128-bit intermediate is used so the `count * bsize` product cannot
/// overflow on exabyte-scale filesystems (audit #10).
fn scale_blocks(count: u64, bsize: u64, unit: u64) -> u64 {
    ((count as u128 * bsize as u128) / unit as u128) as u64
}

/// Percentage of normally-available space in use, rounded up to the next
/// integer (POSIX STDOUT, df: "any fractional result causing it to be rounded
/// to the next highest integer").
///
/// `avail` is the space available to unprivileged users (statfs `f_bavail`),
/// matching the spec's `<space free>` figure — NOT total free space
/// (`f_bfree`), so reserved blocks are excluded from the denominator (audit
/// #4). A zero denominator (e.g. a zero-block pseudo-filesystem) yields 0
/// rather than a NaN (audit #11).
fn capacity_percent(used: u64, avail: u64) -> u32 {
    let denom = used + avail;
    if denom == 0 {
        return 0;
    }
    ((used as f64 / denom as f64) * 100.0).ceil() as u32
}

struct Mount {
    devname: OsString,
    dir: OsString,
    dev: i64,
    masked: bool,
    cached_statfs: libc::statfs,
}

impl Mount {
    fn to_row<'a>(&'a self, fields: &'a Fields) -> FieldsData<'a> {
        let sf = self.cached_statfs;

        let block_size = fields.mode.get_block_size();
        let blksz = sf.f_bsize as u64;

        let total = scale_blocks(sf.f_blocks, blksz, block_size);
        let avail = scale_blocks(sf.f_bavail, blksz, block_size);
        let free = scale_blocks(sf.f_bfree, blksz, block_size);
        let used = total - free;

        let percentage_used = capacity_percent(used, avail);

        // Inode (file-slot) figures. Counts are unscaled (they are not space).
        let itotal = sf.f_files;
        let ifree = sf.f_ffree;
        let iused = itotal.saturating_sub(ifree);
        let ipcent = capacity_percent(iused, ifree); // iused / (iused + ifree) = iused / itotal

        FieldsData {
            fields,
            source: self.devname.to_string_lossy().into_owned(),
            size: total,
            used,
            avail,
            pcent: percentage_used,
            itotal,
            iused,
            ifree,
            ipcent,
            target: self.dir.to_string_lossy().into_owned(),
        }
    }
}

struct MountList {
    mounts: Vec<Mount>,
}

impl MountList {
    fn new() -> MountList {
        MountList { mounts: Vec::new() }
    }

    fn mask_all(&mut self) {
        for mount in &mut self.mounts {
            mount.masked = true;
        }
    }

    fn push(&mut self, fsstat: &libc::statfs, devname: &CString, dirname: &CString) {
        let dev = {
            if let Ok(st) = stat(devname) {
                st.st_rdev as i64
            } else if let Ok(st) = stat(dirname) {
                st.st_dev as i64
            } else {
                -1
            }
        };

        self.mounts.push(Mount {
            devname: OsStr::from_bytes(devname.to_bytes()).to_os_string(),
            dir: OsStr::from_bytes(dirname.to_bytes()).to_os_string(),
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
            let devname = to_cstr(&mount.f_mntfromname).into();
            let dirname = to_cstr(&mount.f_mntonname).into();
            info.push(mount, &devname, &dirname);
        }
    }

    Ok(info)
}

#[cfg(target_os = "linux")]
fn read_mount_info() -> io::Result<MountList> {
    let mut info = MountList::new();

    let mounts = MountTable::open_system()?;
    for mount in mounts {
        unsafe {
            let mut buf: libc::statfs = std::mem::zeroed();
            let rc = libc::statfs(mount.dir.as_ptr(), &mut buf);
            if rc < 0 {
                eprintln!(
                    "{}: {}",
                    Path::new(OsStr::from_bytes(mount.dir.to_bytes())).display(),
                    io::Error::last_os_error()
                );
                continue;
            }

            info.push(&buf, &mount.fsname, &mount.dir);
        }
    }

    Ok(info)
}

fn mask_fs_by_file(info: &mut MountList, path: &Path) -> io::Result<()> {
    let c_filename = match CString::new(path.as_os_str().as_bytes()) {
        Ok(c) => c,
        Err(_) => {
            eprintln!("{}: {}", path.display(), gettext("invalid pathname"));
            return Err(io::Error::from(io::ErrorKind::InvalidInput));
        }
    };
    let stat = match stat(&c_filename) {
        Ok(st) => st,
        Err(e) => {
            eprintln!("{}: {}", path.display(), e);
            return Err(e);
        }
    };

    for mount in &mut info.mounts {
        if stat.st_dev as i64 == mount.dev {
            mount.masked = true;
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut info = read_mount_info()?;
    let mut exit_code = 0;

    if args.files.is_empty() {
        info.mask_all();
    } else {
        for file in &args.files {
            if mask_fs_by_file(&mut info, file).is_err() {
                exit_code = 1;
            }
        }
    }

    // POSIX DESCRIPTION: "if no options other than -t are specified, the number
    // of free file slots, or inodes, available" shall be reported. So inodes are
    // shown for bare `df` and `-t`, but not when -k or -P is given.
    let show_inodes = !args.kilo && !args.portable;

    // -t mandates that the output contain the total allocated space. df always
    // reports the total (the "N-blocks" column), so -t needs no further action
    // beyond the -P|-t mutual exclusion enforced by clap.
    let _ = args.total;

    let mode = OutputMode::new(args.kilo, args.portable);
    let fields = Fields::new(mode, show_inodes);
    // Print header
    println!("{}", fields);

    for mount in &info.mounts {
        if mount.masked {
            // FUTURE DIRECTIONS / Austin Group Defect 251: treat a pathname
            // containing a <newline> (a separator in the output format) as an
            // error rather than emitting a corrupt, unparsable line.
            if mount.devname.as_bytes().contains(&b'\n') || mount.dir.as_bytes().contains(&b'\n') {
                eprintln!(
                    "{}: {}",
                    mount.dir.to_string_lossy(),
                    gettext("pathname contains newline; skipping")
                );
                exit_code = 1;
                continue;
            }
            let row = mount.to_row(&fields);
            println!("{}", row);
        }
    }

    std::process::exit(exit_code);
}

#[cfg(test)]
mod tests {
    use super::{capacity_percent, scale_blocks};

    #[test]
    fn capacity_percent_reserved_blocks() {
        // total=100, f_bfree=10, f_bavail=5, used=90 -> 90/(90+5)=94.7% -> 95.
        // (The old f_bfree denominator gave 90/(90+10)=90% — audit #4.)
        assert_eq!(capacity_percent(90, 5), 95);
    }

    #[test]
    fn capacity_percent_exact_and_bounds() {
        assert_eq!(capacity_percent(50, 50), 50);
        assert_eq!(capacity_percent(0, 100), 0);
        assert_eq!(capacity_percent(100, 0), 100);
    }

    #[test]
    fn capacity_percent_rounds_up() {
        assert_eq!(capacity_percent(1, 99), 1); // 1.00% exact
        assert_eq!(capacity_percent(2, 99), 2); // 1.98% -> 2
    }

    #[test]
    fn capacity_percent_zero_denominator() {
        // zero-block pseudo-filesystem: no NaN, no panic (audit #11).
        assert_eq!(capacity_percent(0, 0), 0);
    }

    #[test]
    fn scale_blocks_basic() {
        // 2048 * 512 bytes = 1 MiB.
        assert_eq!(scale_blocks(2048, 512, 1024), 1024);
        assert_eq!(scale_blocks(2048, 512, 512), 2048);
    }

    #[test]
    fn scale_blocks_no_overflow() {
        // A count*bsize product that exceeds u64::MAX must not overflow (#10).
        let big = u64::MAX / 1000;
        let expected = ((big as u128 * 4096) / 1024) as u64;
        assert_eq!(scale_blocks(big, 4096, 1024), expected);
    }
}
