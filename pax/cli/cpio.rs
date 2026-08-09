//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The `cpio` command line, translated into pax's internal options.
//!
//! Scope is the set of options that appear in real scripts, not everything GNU
//! cpio accepts. Anything outside it is rejected by name rather than ignored.

use super::{parse_number, read_name_list, unknown, unsupported, usage, ArgCursor};
use crate::error::{PaxError, PaxResult};
use crate::{Args, Format};
use std::path::PathBuf;

const PROG: &str = "cpio";

/// Short options that consume an argument.
fn takes_arg(c: char) -> bool {
    matches!(c, 'C' | 'H' | 'F' | 'I' | 'O' | 'E' | 'R' | 'M')
}

/// The operation a cpio command line selects.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    /// -o: read pathnames from stdin, write an archive
    CopyOut,
    /// -i: read an archive, extract (or list, with -t)
    CopyIn,
    /// -p: read pathnames from stdin, copy the files to a directory
    PassThrough,
}

/// Options accumulated while walking the command line.
struct State {
    mode: Option<Mode>,
    args: Args,
    /// -t: list rather than extract
    list: bool,
    /// -m: keep the archived modification times
    preserve_mtime: bool,
    /// -u: replace files unconditionally, not only when the archive is newer
    unconditional: bool,
    /// -0: the pathname list on stdin is NUL-separated
    null: bool,
    /// --quiet: do not report the block count
    quiet: bool,
    format: Option<Format>,
    operands: Vec<String>,
}

impl State {
    fn new() -> Self {
        State {
            mode: None,
            args: Args::default(),
            list: false,
            preserve_mtime: false,
            unconditional: false,
            null: false,
            quiet: false,
            format: None,
            operands: Vec::new(),
        }
    }

    fn set_mode(&mut self, mode: Mode) -> PaxResult<()> {
        match self.mode {
            Some(existing) if existing != mode => {
                Err(usage(PROG, "only one of -o, -i or -p may be given"))
            }
            _ => {
                self.mode = Some(mode);
                Ok(())
            }
        }
    }
}

/// Parse a cpio command line into pax's internal options.
pub fn parse(argv: Vec<String>) -> PaxResult<Args> {
    let mut st = State::new();
    let mut cur = ArgCursor::new(PROG, argv, 1);

    while let Some(arg) = cur.next() {
        if arg == "--" {
            st.operands.extend(cur.rest());
            break;
        } else if let Some(long) = arg.strip_prefix("--") {
            apply_long(long, &mut st, &mut cur)?;
        } else if arg.len() > 1 && arg.starts_with('-') {
            apply_cluster(&arg[1..], &mut st, &mut cur)?;
        } else {
            st.operands.push(arg);
        }
    }

    finish(st)
}

/// Apply one short option. `glued` is the rest of its cluster, if any.
fn apply_short(
    c: char,
    glued: Option<String>,
    st: &mut State,
    cur: &mut ArgCursor,
) -> PaxResult<()> {
    if takes_arg(c) {
        let name = format!("-{}", c);
        let value = cur.value(&name, glued)?;
        return apply_value(c, &value, st);
    }

    match c {
        'o' => st.set_mode(Mode::CopyOut)?,
        'i' => st.set_mode(Mode::CopyIn)?,
        'p' => st.set_mode(Mode::PassThrough)?,
        't' => st.list = true,
        'v' => st.args.verbose = true,
        // pax always creates the leading directories a member needs, so this is
        // accepted and does nothing. The difference is permissive: cpio without
        // -d would have failed on those members instead.
        'd' => {}
        'm' => st.preserve_mtime = true,
        'u' => st.unconditional = true,
        'B' => st.args.blocksize = Some(5120),
        'a' => st.args.reset_atime = true,
        'l' => st.args.link = true,
        'L' => st.args.dereference = true,
        'c' => st.format = Some(Format::Cpio),
        'f' => st.args.exclude = true,
        'r' => st.args.interactive = true,
        'A' => st.args.append = true,
        '0' => st.null = true,
        // GNU accepts and ignores -k for historic compatibility.
        'k' => {}
        's' | 'S' | 'b' => {
            return Err(unsupported(
                PROG,
                &format!("-{}", c),
                "byte and half-word swapping are not implemented",
            ))
        }
        'V' => return Err(unsupported(PROG, "-V", "no per-file progress dots")),
        _ => return Err(unknown(PROG, &format!("-{}", c))),
    }
    Ok(())
}

/// Apply a short option that took an argument.
fn apply_value(c: char, value: &str, st: &mut State) -> PaxResult<()> {
    match c {
        // -F names one archive for either direction; -I and -O are the
        // input-only and output-only spellings of the same thing.
        'F' | 'I' | 'O' => st.args.archive = Some(PathBuf::from(value)),
        'C' => {
            let size = parse_number(PROG, "-C", value)?;
            if size == 0 || size > u32::MAX as u64 {
                return Err(usage(
                    PROG,
                    format!("I/O block size '{}' is out of range", value),
                ));
            }
            st.args.blocksize = Some(size as u32);
        }
        'H' => st.format = Some(archive_format(value)?),
        'E' => st
            .args
            .files_and_patterns
            .extend(read_name_list(value, false)?),
        'R' => return Err(unsupported(PROG, "-R", "ownership cannot be reassigned")),
        'M' => return Err(unsupported(PROG, "-M", "no multi-volume media prompts")),
        _ => unreachable!("takes_arg and apply_value disagree about -{}", c),
    }
    Ok(())
}

/// Map a `-H` name onto the archive format pax will write.
fn archive_format(name: &str) -> PaxResult<Format> {
    match name {
        "bin" => Ok(Format::Bcpio),
        "odc" => Ok(Format::Cpio),
        "newc" => Ok(Format::Sv4cpio),
        "crc" => Ok(Format::Sv4crc),
        "tar" | "ustar" => Ok(Format::Ustar),
        "hpbin" | "hpodc" => Err(unsupported(
            PROG,
            "-H hpbin/hpodc",
            "the HP variants are not implemented",
        )),
        other => Err(usage(PROG, format!("unknown archive format '{}'", other))),
    }
}

/// Apply a cluster of short options from a single `-xyz` argument.
fn apply_cluster(cluster: &str, st: &mut State, cur: &mut ArgCursor) -> PaxResult<()> {
    for (i, c) in cluster.char_indices() {
        if takes_arg(c) {
            let glued = cluster[i + c.len_utf8()..].to_string();
            return apply_short(c, Some(glued), st, cur);
        }
        apply_short(c, None, st, cur)?;
    }
    Ok(())
}

/// Apply one long option, with the leading `--` already stripped.
fn apply_long(long: &str, st: &mut State, cur: &mut ArgCursor) -> PaxResult<()> {
    let (name, inline) = match long.split_once('=') {
        Some((n, v)) => (n, Some(v.to_string())),
        None => (long, None),
    };

    let short = match name {
        "create" => Some('o'),
        "extract" => Some('i'),
        "pass-through" => Some('p'),
        "list" => Some('t'),
        "verbose" => Some('v'),
        "make-directories" => Some('d'),
        "preserve-modification-time" => Some('m'),
        "unconditional" => Some('u'),
        "reset-access-time" => Some('a'),
        "link" => Some('l'),
        "dereference" => Some('L'),
        "nonmatching" => Some('f'),
        "rename" => Some('r'),
        "append" => Some('A'),
        "null" => Some('0'),
        "format" => Some('H'),
        "file" => Some('F'),
        "io-size" | "block-size" => Some('C'),
        "pattern-file" => Some('E'),
        "owner" => Some('R'),
        _ => None,
    };
    if let Some(c) = short {
        return apply_short(c, inline, st, cur);
    }

    match name {
        "quiet" => st.quiet = true,
        // Already the default: extraction never writes above the current
        // directory, and stored names keep whatever the archive holds.
        "no-absolute-filenames" => {}
        "absolute-filenames" => {
            return Err(unsupported(
                PROG,
                "--absolute-filenames",
                "members are always extracted below the current directory",
            ))
        }
        "help" => {
            print!("{}", USAGE);
            return Err(PaxError::EarlyExit);
        }
        "version" => {
            println!("{} (posixutils-rs) {}", PROG, env!("CARGO_PKG_VERSION"));
            return Err(PaxError::EarlyExit);
        }
        _ => return Err(unknown(PROG, &format!("--{}", name))),
    }
    Ok(())
}

/// Turn the accumulated state into the internal options pax runs on.
fn finish(mut st: State) -> PaxResult<Args> {
    let Some(mode) = st.mode else {
        return Err(usage(PROG, "one of -o, -i or -p is required"));
    };

    // cpio's default I/O block is one 512-byte block, where pax defaults the
    // cpio format to 5120 per POSIX. Keeping cpio's default matters because the
    // archive is padded out to it: at 5120 a two-block archive would be written
    // -- and reported -- as ten.
    if st.args.blocksize.is_none() {
        st.args.blocksize = Some(512);
    }

    // cpio archives exactly the names it is given and never descends: the
    // caller's `find` has already produced every path, so recursing would
    // archive each subtree a second time under its own name.
    if mode != Mode::CopyIn {
        st.args.dir_no_follow = true;
    }

    // cpio names files on standard input for copy-out and pass-through. With
    // -0 that list is NUL-separated, which pax cannot read for itself, so it is
    // collected here and handed over as operands instead.
    let names = if st.null && mode != Mode::CopyIn {
        read_name_list("-", true)?
    } else {
        Vec::new()
    };

    match mode {
        Mode::CopyOut => {
            if !st.operands.is_empty() {
                return Err(usage(PROG, "-o takes its file list on standard input"));
            }
            st.args.write_mode = true;
            st.args.files_and_patterns.extend(names);
            // GNU cpio writes the old binary format unless told otherwise.
            st.args.format = Some(st.format.unwrap_or(Format::Bcpio));
        }
        Mode::CopyIn => {
            if st.list {
                // List mode is neither read nor write in pax's dispatch.
            } else {
                st.args.read_mode = true;
            }
            st.args.files_and_patterns.extend(st.operands);
        }
        Mode::PassThrough => {
            let [dest] = st.operands.as_slice() else {
                return Err(usage(PROG, "-p requires exactly one destination directory"));
            };
            st.args.read_mode = true;
            st.args.write_mode = true;
            // Copy mode reads the sources from stdin when the destination is
            // the only operand; with -0 they were read above and go in front.
            let dest = dest.clone();
            st.args.files_and_patterns.extend(names);
            st.args.files_and_patterns.push(dest);
        }
    }

    if st.list && mode != Mode::CopyIn {
        return Err(usage(PROG, "-t applies only to copy-in (-i)"));
    }

    // cpio restores modification times only when asked; pax preserves them by
    // default, so the absence of -m is what has to be expressed.
    if !st.preserve_mtime && mode != Mode::CopyOut {
        st.args.privs = Some("m".to_string());
    }

    // Without -u, cpio refuses to replace a file with an older archived copy --
    // which is exactly what pax's -u does.
    if !st.unconditional && mode != Mode::CopyOut {
        st.args.update = true;
    }

    // The block total goes to stderr for the two modes that move an archive.
    st.args.report_blocks = !st.quiet && mode != Mode::PassThrough;

    Ok(st.args)
}

const USAGE: &str = "\
Usage: cpio -o [OPTION...] < name-list > archive     copy out
       cpio -i [OPTION...] [PATTERN...] < archive    copy in
       cpio -p [OPTION...] directory < name-list     pass through

Operation:
  -o, --create               write an archive built from the names on stdin
  -i, --extract              read an archive and extract the members
  -p, --pass-through         copy the named files into a directory
  -t, --list                 with -i, list the archive instead of extracting

Options:
  -F, --file=ARCHIVE         use ARCHIVE instead of standard input or output
  -I ARCHIVE                 read from ARCHIVE
  -O ARCHIVE                 write to ARCHIVE
  -H, --format=FMT           bin, odc, newc, crc, tar or ustar
  -c                         write the odc format (same as -H odc)
  -v, --verbose              list each file processed
  -d, --make-directories     accepted; leading directories are always created
  -m, --preserve-modification-time  keep the archived modification times
  -u, --unconditional        replace files even with an older copy
  -B                         use a 5120-byte I/O block size
  -C, --io-size=N            use an N-byte I/O block size
  -a, --reset-access-time    restore access times of the files read
  -l, --link                 with -p, link instead of copying where possible
  -L, --dereference          follow symbolic links
  -f, --nonmatching          process the members that do not match a pattern
  -r, --rename               prompt for a new name for each file
  -A, --append               append to an existing archive
  -0, --null                 the name list on stdin is NUL-separated
  -E, --pattern-file=FILE    read match patterns from FILE
      --quiet                do not print the block count
      --help                 print this message
      --version              print the version

A count of 512-byte blocks is written to standard error after -o and -i.
Unsupported options are diagnosed rather than ignored.
";

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    /// Parse a cpio command line given without the leading program name.
    fn cpio(args: &[&str]) -> PaxResult<Args> {
        let mut argv = vec!["cpio".to_string()];
        argv.extend(args.iter().map(|s| s.to_string()));
        parse(argv)
    }

    #[test]
    fn test_modes_map_onto_pax_dispatch() {
        let out = cpio(&["-o"]).unwrap();
        assert!(out.write_mode && !out.read_mode);

        let inn = cpio(&["-i"]).unwrap();
        assert!(inn.read_mode && !inn.write_mode);

        // -t lists, which in pax is neither read nor write.
        let list = cpio(&["-it"]).unwrap();
        assert!(!list.read_mode && !list.write_mode);

        // -p is copy mode, with the destination as its only operand.
        let pass = cpio(&["-p", "dest"]).unwrap();
        assert!(pass.read_mode && pass.write_mode);
        assert_eq!(pass.files_and_patterns, vec!["dest".to_string()]);

        assert!(
            cpio(&["-o", "-p", "dest"]).is_err(),
            "two operations conflict"
        );
        assert!(cpio(&["-v"]).is_err(), "an operation is required");
        assert!(cpio(&["-ot"]).is_err(), "-t needs -i");
        assert!(cpio(&["-p"]).is_err(), "-p needs a destination");
        assert!(cpio(&["-p", "a", "b"]).is_err(), "-p takes only one");
        assert!(
            cpio(&["-o", "file"]).is_err(),
            "-o reads its list from stdin"
        );
    }

    #[test]
    fn test_never_descends_into_directories() {
        // The caller's `find` already listed every path, so recursing would
        // store each subtree a second time.
        assert!(cpio(&["-o"]).unwrap().dir_no_follow);
        assert!(cpio(&["-p", "dest"]).unwrap().dir_no_follow);
        // Copy-in takes patterns, not pathnames, so this does not apply.
        assert!(!cpio(&["-i"]).unwrap().dir_no_follow);
    }

    #[test]
    fn test_format_selection() {
        // cpio's own default is the old binary format.
        assert_eq!(cpio(&["-o"]).unwrap().format, Some(Format::Bcpio));
        assert_eq!(cpio(&["-oc"]).unwrap().format, Some(Format::Cpio));
        for (name, want) in [
            ("bin", Format::Bcpio),
            ("odc", Format::Cpio),
            ("newc", Format::Sv4cpio),
            ("crc", Format::Sv4crc),
            ("ustar", Format::Ustar),
        ] {
            assert_eq!(cpio(&["-o", "-H", name]).unwrap().format, Some(want));
        }
        assert!(cpio(&["-o", "-H", "hpodc"]).is_err());
        assert!(cpio(&["-o", "-H", "zip"]).is_err());
    }

    #[test]
    fn test_mtime_and_overwrite_defaults_are_inverted_from_pax() {
        // cpio restores timestamps only under -m, and refuses to replace a
        // newer file unless -u; pax's defaults are the other way round.
        let plain = cpio(&["-i"]).unwrap();
        assert_eq!(plain.privs.as_deref(), Some("m"));
        assert!(plain.update);

        let both = cpio(&["-imu"]).unwrap();
        assert_eq!(both.privs, None);
        assert!(!both.update);

        // Neither applies when writing an archive.
        let out = cpio(&["-o"]).unwrap();
        assert_eq!(out.privs, None);
        assert!(!out.update);
    }

    #[test]
    fn test_block_size_options() {
        // Default to cpio's own 512-byte block, not the 5120 pax uses for the
        // cpio format: the archive is padded out to whatever this says.
        assert_eq!(cpio(&["-o"]).unwrap().blocksize, Some(512));
        assert_eq!(cpio(&["-oB"]).unwrap().blocksize, Some(5120));
        assert_eq!(cpio(&["-o", "-C", "1024"]).unwrap().blocksize, Some(1024));
        assert!(cpio(&["-o", "-C", "0"]).is_err());
    }

    #[test]
    fn test_archive_file_spellings_are_equivalent() {
        for opt in ["-F", "-I", "-O"] {
            assert_eq!(
                cpio(&["-o", opt, "a.cpio"]).unwrap().archive.as_deref(),
                Some(Path::new("a.cpio"))
            );
        }
        assert_eq!(
            cpio(&["-o", "--file=a.cpio"]).unwrap().archive.as_deref(),
            Some(Path::new("a.cpio"))
        );
    }

    #[test]
    fn test_block_count_is_reported_except_for_pass_through() {
        assert!(cpio(&["-o"]).unwrap().report_blocks);
        assert!(cpio(&["-i"]).unwrap().report_blocks);
        assert!(!cpio(&["-o", "--quiet"]).unwrap().report_blocks);
        // Pass-through moves no archive, so it has no blocks to count.
        assert!(!cpio(&["-p", "dest"]).unwrap().report_blocks);
    }

    #[test]
    fn test_selection_and_prompt_flags() {
        assert!(cpio(&["-i", "-f"]).unwrap().exclude);
        assert!(cpio(&["-i", "-r"]).unwrap().interactive);
        assert!(cpio(&["-i", "-L"]).unwrap().dereference);
        assert!(cpio(&["-o", "-a"]).unwrap().reset_atime);
        assert!(cpio(&["-p", "-l", "dest"]).unwrap().link);
        assert!(cpio(&["-i"]).unwrap().files_and_patterns.is_empty());
        assert_eq!(
            cpio(&["-i", "*.txt"]).unwrap().files_and_patterns,
            vec!["*.txt".to_string()]
        );
    }

    #[test]
    fn test_unsupported_options_are_refused() {
        assert!(cpio(&["-i", "--absolute-filenames"]).is_err());
        assert!(cpio(&["-o", "-s"]).is_err());
        assert!(cpio(&["-i", "-V"]).is_err());
        assert!(cpio(&["-i", "--frobnicate"]).is_err());
        // -d and -k are accepted and do nothing, as GNU does for -k.
        assert!(cpio(&["-idk"]).is_ok());
    }
}
