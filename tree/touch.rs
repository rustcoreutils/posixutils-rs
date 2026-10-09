//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{Datelike, Local, NaiveDate, TimeZone};
use clap::Parser;
use gettextrs::gettext;
use plib::date_arg;
use std::ffi::{CStr, CString};
use std::io;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
use std::os::unix::ffi::OsStrExt;
use std::time::{SystemTime, UNIX_EPOCH};

/// touch - change file access and modification times
#[derive(Parser)]
#[command(version, about = gettext("touch - change file access and modification times"))]
struct Args {
    #[arg(short, long, help = gettext("Change the access time of file"))]
    access: bool,

    #[arg(short = 'c', long, help = gettext("Do not create a specified file if it does not exist"))]
    no_create: bool,

    #[arg(short, long, help = gettext("Change the modification time of file"))]
    mtime: bool,

    #[arg(short, long, alias = "date", allow_hyphen_values = true, group = "timefmt", help = gettext("Use the specified ISO 8601:2000 date-time format (a trailing ' UTC' or ' GMT' means 'Z'), an RFC 5322 date as printed by 'date -R', or @SECONDS, instead of the current time"))]
    datetime: Option<String>,

    #[arg(short, long, allow_hyphen_values = true, group = "timefmt", help = gettext("Use the specified POSIX [[CC]YY]MMDDhhmm[.SS] format, instead of the current time"))]
    time: Option<String>,

    #[arg(short, long, allow_hyphen_values = true, group = "timefmt", help = gettext("Use the corresponding time of the file named by the pathname ref_file instead of the current time"))]
    ref_file: Option<String>,

    #[arg(help = gettext("A pathname of a file whose times shall be modified"))]
    files: Vec<String>,
}

fn mk_ts(sec: i64, nsec: u32) -> libc::timespec {
    libc::timespec {
        tv_sec: sec as libc::time_t,
        tv_nsec: nsec as libc::c_long,
    }
}

fn omit() -> libc::timespec {
    libc::timespec {
        tv_sec: 0,
        tv_nsec: libc::UTIME_OMIT,
    }
}

fn now() -> libc::timespec {
    libc::timespec {
        tv_sec: 0,
        tv_nsec: libc::UTIME_NOW,
    }
}

fn systemtime_to_ts(t: SystemTime) -> libc::timespec {
    match t.duration_since(UNIX_EPOCH) {
        Ok(d) => mk_ts(d.as_secs() as i64, d.subsec_nanos()),
        Err(e) => {
            // Before the epoch (rare for a real reference file).
            let d = e.duration();
            mk_ts(-(d.as_secs() as i64), 0)
        }
    }
}

/// Parse the `-t` POSIX `[[CC]YY]MMDDhhmm[.SS]` form, interpreted in the local timezone.
fn parse_posix_time(input: &str) -> Result<libc::timespec, String> {
    let (date_str, secs) = match input.split_once('.') {
        Some((d, s)) => {
            let s: u32 = s
                .parse()
                .map_err(|_| gettext!("invalid time format: '{}'", input))?;
            (d, s)
        }
        None => (input, 0),
    };

    if !date_str.bytes().all(|b| b.is_ascii_digit()) {
        return Err(gettext!("invalid time format: '{}'", input));
    }

    let g = |a: usize, b: usize| date_str[a..b].parse::<u32>().unwrap();
    let (year, month, day, hour, minute) = match date_str.len() {
        8 => (Local::now().year(), g(0, 2), g(2, 4), g(4, 6), g(6, 8)),
        10 => {
            let yy = g(0, 2);
            let year = if yy <= 68 { 2000 + yy } else { 1900 + yy } as i32;
            (year, g(2, 4), g(4, 6), g(6, 8), g(8, 10))
        }
        12 => (g(0, 4) as i32, g(4, 6), g(6, 8), g(8, 10), g(10, 12)),
        _ => return Err(gettext!("invalid time format: '{}'", input)),
    };

    // POSIX allows SS in [00,60]; clamp a leap second to 59 (chrono has no plain leap instant).
    let secs = secs.min(59);

    let naive = NaiveDate::from_ymd_opt(year, month, day)
        .and_then(|d| d.and_hms_opt(hour, minute, secs))
        .ok_or_else(|| gettext!("invalid time: '{}'", input))?;
    let dt = Local
        .from_local_datetime(&naive)
        .single()
        .ok_or_else(|| gettext!("invalid time: '{}'", input))?;
    Ok(mk_ts(dt.timestamp(), 0))
}

/// The (atime, mtime) the time options request. For `-r`, the two come from the reference file's
/// corresponding fields; for `-d`/`-t` both are the parsed instant; otherwise both are "now".
fn time_source(args: &Args) -> Result<(libc::timespec, libc::timespec), String> {
    if let Some(d) = &args.datetime {
        let (secs, nsec) = date_arg::parse(d, date_arg::Zoneless::Local)?;
        let ts = mk_ts(secs, nsec);
        Ok((ts, ts))
    } else if let Some(t) = &args.time {
        let ts = parse_posix_time(t)?;
        Ok((ts, ts))
    } else if let Some(rf) = &args.ref_file {
        let md = std::fs::metadata(rf)
            .map_err(|e| format!("{rf}: {}", plib::diag::io_error_text(&e)))?;
        let atime = md
            .accessed()
            .map(systemtime_to_ts)
            .unwrap_or_else(|_| now());
        let mtime = md
            .modified()
            .map(systemtime_to_ts)
            .unwrap_or_else(|_| now());
        Ok((atime, mtime))
    } else {
        Ok((now(), now()))
    }
}

fn touch_file(
    args: &Args,
    source: &(libc::timespec, libc::timespec),
    filename: &str,
) -> io::Result<()> {
    let c_path =
        CString::new(filename).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    // Set only the requested field(s); leave the other unchanged (UTIME_OMIT). On a just-created
    // file the omitted field keeps its creation-time value.
    let atime = if args.access { source.0 } else { omit() };
    let mtime = if args.mtime { source.1 } else { omit() };
    let times = [atime, mtime];

    if args.no_create {
        return match set_times_path(&c_path, &times) {
            // POSIX -c: do not create, and write no diagnostic; exit success.
            Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(()),
            result => result,
        };
    }

    // A pathname ending in a slash names a directory (POSIX pathname resolution), so no file is
    // ever created for it: it is an existing directory, given its times by name, or an error.
    // Deciding this here rather than through the create keeps it the same on every system --
    // macOS resolves a dangling symlink followed by a slash differently from Linux.
    if filename.ends_with('/') {
        return set_times_path(&c_path, &times);
    }

    let open_err = match create_new(&c_path) {
        Ok(fd) => return set_times_fd(&fd, &times),
        Err(e) => e,
    };
    // Whatever kept the file from being created (it exists; or the name ends in a slash, which
    // Linux refuses with EISDIR before it looks at O_EXCL), an existing file gets its times by
    // name.
    match set_times_path(&c_path, &times) {
        Err(e) if e.kind() == io::ErrorKind::NotFound => not_found(&c_path, &times, open_err, e),
        result => result,
    }
}

/// The file could neither be created (`open_err`) nor found (`not_found`).
fn not_found(
    path: &CStr,
    times: &[libc::timespec; 2],
    open_err: io::Error,
    not_found: io::Error,
) -> io::Result<()> {
    match open_err.raw_os_error() {
        // A dangling symlink: the file does not exist, and POSIX creates it with creat(),
        // which follows the link and creates its target.
        Some(libc::EEXIST) if is_symlink(path) => match create_link_target(path)? {
            LinkTarget::Created(fd) => set_times_fd(&fd, times),
            // The target has appeared since: an existing file, given its times by name.
            LinkTarget::Exists => set_times_path(path, times).map_err(|e| {
                if e.kind() == io::ErrorKind::NotFound {
                    not_found
                } else {
                    e
                }
            }),
        },
        // The name existed when the create was tried and is gone now: POSIX would have created
        // the file, so try once more.
        Some(libc::EEXIST) => match create_new(path) {
            Ok(fd) => set_times_fd(&fd, times),
            Err(e) if e.raw_os_error() == Some(libc::EEXIST) => Err(not_found),
            Err(e) => Err(e),
        },
        // A trailing slash on a name that does not exist: it is the missing file to report.
        Some(libc::EISDIR) => Err(not_found),
        // Why the file could not be created, e.g. a directory without write permission.
        _ => Err(open_err),
    }
}

/// Create `path` as a new empty file; EEXIST if something already has that name.
///
/// `O_EXCL` makes the existence check and the creation one step, so a file planted between a
/// check and the open is never opened, and a symlink is not followed. `O_NONBLOCK` and
/// `O_NOCTTY` keep the open from waiting on a FIFO or acquiring a terminal all the same.
fn create_new(path: &CStr) -> io::Result<OwnedFd> {
    let flags = libc::O_CREAT
        | libc::O_EXCL
        | libc::O_WRONLY
        | libc::O_NONBLOCK
        | libc::O_NOCTTY
        | libc::O_CLOEXEC;
    let fd = unsafe { libc::open(path.as_ptr(), flags, 0o666 as libc::c_int) };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(unsafe { OwnedFd::from_raw_fd(fd) })
}

/// Whether `path` names a symlink itself.
fn is_symlink(path: &CStr) -> bool {
    let path = std::ffi::OsStr::from_bytes(path.to_bytes());
    std::fs::symlink_metadata(path).is_ok_and(|md| md.file_type().is_symlink())
}

/// What creating a dangling symlink's target came to.
#[derive(Debug)]
enum LinkTarget {
    /// The target was created by this run, and is open on this descriptor.
    Created(OwnedFd),
    /// Something has the target's name now (or its last component is itself a symlink): it is
    /// an existing file, never opened here.
    Exists,
}

/// Create the file the symlink `link` names, as creat() does through a dangling link.
///
/// The link is read once, with `readlinkat` in its directory, and the target is created
/// relative to that directory with `O_EXCL|O_NOFOLLOW`: if the link has been repointed since it
/// was found dangling, or its target made, at a file or a device, nothing is opened, so no
/// existing file is emptied and no device sees an open. Components of the target before the last
/// are resolved as usual.
fn create_link_target(link: &CStr) -> io::Result<LinkTarget> {
    let (dir_path, name) = split_parent(link.to_bytes())?;
    let dir = open_dir(&dir_path)?;
    let target = read_link_at(&dir, &name)?;
    let flags = libc::O_CREAT
        | libc::O_EXCL
        | libc::O_NOFOLLOW
        | libc::O_WRONLY
        | libc::O_NONBLOCK
        | libc::O_NOCTTY
        | libc::O_CLOEXEC;
    let fd = unsafe {
        libc::openat(
            dir.as_raw_fd(),
            target.as_ptr(),
            flags,
            0o666 as libc::c_int,
        )
    };
    if fd >= 0 {
        return Ok(LinkTarget::Created(unsafe { OwnedFd::from_raw_fd(fd) }));
    }
    let err = io::Error::last_os_error();
    match err.raw_os_error() {
        Some(libc::EEXIST) | Some(libc::ELOOP) => Ok(LinkTarget::Exists),
        _ => Err(err),
    }
}

/// The directory part and the last component of `path`, which does not end in a slash.
fn split_parent(path: &[u8]) -> io::Result<(CString, CString)> {
    let (dir, name): (&[u8], &[u8]) = match path.iter().rposition(|&b| b == b'/') {
        Some(0) => (b"/", &path[1..]),
        Some(i) => (&path[..i], &path[i + 1..]),
        None => (b".", path),
    };
    let to_c =
        |b: &[u8]| CString::new(b).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e));
    Ok((to_c(dir)?, to_c(name)?))
}

/// Open the directory `path` to work relative to it.
fn open_dir(path: &CStr) -> io::Result<OwnedFd> {
    let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC;
    let fd = unsafe { libc::open(path.as_ptr(), flags) };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(unsafe { OwnedFd::from_raw_fd(fd) })
}

/// The target of the symlink `name` in `dir`.
fn read_link_at(dir: &OwnedFd, name: &CStr) -> io::Result<CString> {
    let mut buf = vec![0u8; libc::PATH_MAX as usize];
    let len = unsafe {
        libc::readlinkat(
            dir.as_raw_fd(),
            name.as_ptr(),
            buf.as_mut_ptr().cast(),
            buf.len(),
        )
    };
    if len < 0 {
        return Err(io::Error::last_os_error());
    }
    buf.truncate(len as usize);
    CString::new(buf).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

/// Set the times of the file open on `fd`, the one this run created.
fn set_times_fd(fd: &OwnedFd, times: &[libc::timespec; 2]) -> io::Result<()> {
    if unsafe { libc::futimens(fd.as_raw_fd(), times.as_ptr()) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// Set the times of the existing file `path` names, following a symlink.
fn set_times_path(path: &CStr, times: &[libc::timespec; 2]) -> io::Result<()> {
    if unsafe { libc::utimensat(libc::AT_FDCWD, path.as_ptr(), times.as_ptr(), 0) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("touch");

    let mut args = Args::parse();

    // Default to changing both access and modification times.
    if !args.access && !args.mtime {
        args.access = true;
        args.mtime = true;
    }

    let source = match time_source(&args) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("touch: {e}");
            std::process::exit(1);
        }
    };

    let mut exit_code = 0;
    for filename in &args.files {
        if let Err(e) = touch_file(&args, &source, filename) {
            exit_code = 1;
            eprintln!("touch: {filename}: {}", plib::diag::io_error_text(&e));
        }
    }

    std::process::exit(exit_code)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::os::unix::fs::FileTypeExt;
    use std::path::Path;

    fn c(path: &Path) -> CString {
        CString::new(path.as_os_str().as_bytes()).unwrap()
    }

    /// A dangling link's target is created, relative to the link's own directory.
    #[test]
    fn create_link_target_creates_a_relative_target() {
        let dir = plib::tmp::tempdir().unwrap();
        let link = dir.path().join("link");
        std::os::unix::fs::symlink("target", &link).unwrap();

        let made = create_link_target(&c(&link)).unwrap();

        assert!(matches!(made, LinkTarget::Created(_)), "{made:?}");
        assert!(fs::symlink_metadata(dir.path().join("target"))
            .unwrap()
            .is_file());
    }

    /// A target made since the link was found dangling is not opened: a
    /// regular file keeps its contents.
    #[test]
    fn create_link_target_leaves_an_existing_file() {
        let dir = plib::tmp::tempdir().unwrap();
        let (link, target) = (dir.path().join("link"), dir.path().join("target"));
        std::os::unix::fs::symlink(&target, &link).unwrap();
        fs::write(&target, b"contents").unwrap();

        let made = create_link_target(&c(&link)).unwrap();

        assert!(matches!(made, LinkTarget::Exists), "{made:?}");
        assert_eq!(fs::read(&target).unwrap(), b"contents");
    }

    /// A FIFO target with no reader is not opened, so it neither waits nor
    /// fails with ENXIO.
    #[test]
    fn create_link_target_leaves_a_fifo() {
        let dir = plib::tmp::tempdir().unwrap();
        let (link, target) = (dir.path().join("link"), dir.path().join("fifo"));
        std::os::unix::fs::symlink(&target, &link).unwrap();
        assert_eq!(unsafe { libc::mkfifo(c(&target).as_ptr(), 0o600) }, 0);

        let made = create_link_target(&c(&link)).unwrap();

        assert!(matches!(made, LinkTarget::Exists), "{made:?}");
        assert!(fs::symlink_metadata(&target).unwrap().file_type().is_fifo());
    }

    /// A target whose last component is itself a symlink is not followed.
    #[test]
    fn create_link_target_does_not_follow_a_second_link() {
        let dir = plib::tmp::tempdir().unwrap();
        let (link, middle, file) = (
            dir.path().join("link"),
            dir.path().join("middle"),
            dir.path().join("file"),
        );
        std::os::unix::fs::symlink(&middle, &link).unwrap();
        std::os::unix::fs::symlink(&file, &middle).unwrap();
        fs::write(&file, b"contents").unwrap();

        let made = create_link_target(&c(&link)).unwrap();

        assert!(matches!(made, LinkTarget::Exists), "{made:?}");
        assert_eq!(fs::read(&file).unwrap(), b"contents");
    }

    #[test]
    fn split_parent_cases() {
        let split = |p: &[u8]| {
            let (d, n) = split_parent(p).unwrap();
            (d.into_bytes(), n.into_bytes())
        };
        assert_eq!(split(b"a"), (b".".to_vec(), b"a".to_vec()));
        assert_eq!(split(b"/a"), (b"/".to_vec(), b"a".to_vec()));
        assert_eq!(split(b"x/y/a"), (b"x/y".to_vec(), b"a".to_vec()));
    }
}
