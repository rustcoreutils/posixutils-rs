//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The `tar` command line, translated into pax's internal options.
//!
//! Scope is the set of options that appear in real scripts, not everything GNU
//! tar accepts. Anything outside it is rejected by name rather than ignored.

use super::{parse_number, read_name_list, unknown, unsupported, usage, ArgCursor};
use crate::error::{PaxError, PaxResult};
use crate::{Args, Format};
use std::path::PathBuf;

const PROG: &str = "tar";

/// Every short option this parser recognizes, whether it is supported or
/// explicitly refused. `apply_short` has an arm for each of these; a letter
/// listed here but unhandled there would report itself as "unrecognized",
/// which is the one thing this set exists to avoid.
///
/// The set doubles as the test for tar's old-style first operand (`tar cvf`):
/// a leading argument is a bundle only if *every* character is one of these,
/// so a name containing anything else -- `src`, `readme`, `build` -- is an
/// operand. It cannot do better than that: a first operand spelled entirely
/// in option letters, such as `cat`, is read as options, and every historic
/// tar has the same ambiguity, because that is what the convention costs.
const OPTION_LETTERS: &str = "cxtrufvzCpmhkOTXPbjJZAwSWaolNgGMRVdin";

/// Short options that consume an argument.
fn takes_arg(c: char) -> bool {
    matches!(c, 'f' | 'C' | 'T' | 'X' | 'b')
}

/// The operation a tar command line selects.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Create,
    Extract,
    List,
    Append,
    Update,
}

/// Options accumulated while walking the command line.
struct State {
    mode: Option<Mode>,
    args: Args,
    /// Characters for pax's `-p`, built up from tar's preservation flags
    privs: String,
    /// `-P`: keep pathnames absolute
    absolute: bool,
    /// `--null`: names in a `-T` file are NUL-separated
    null: bool,
    format: Option<Format>,
}

impl State {
    fn new() -> Self {
        State {
            mode: None,
            args: Args::default(),
            privs: String::new(),
            absolute: false,
            null: false,
            format: None,
        }
    }

    fn set_mode(&mut self, mode: Mode) -> PaxResult<()> {
        match self.mode {
            Some(existing) if existing != mode => {
                Err(usage(PROG, "only one of -c, -x, -t, -r or -u may be given"))
            }
            _ => {
                self.mode = Some(mode);
                Ok(())
            }
        }
    }
}

/// Parse a tar command line into pax's internal options.
pub fn parse(argv: Vec<String>) -> PaxResult<Args> {
    let mut st = State::new();

    // tar's oldest calling convention puts the option letters in the first
    // operand with no leading dash -- `tar cvf out.tar dir`. Their arguments
    // are then taken, in order, from the arguments that follow.
    let old_style = argv
        .get(1)
        .filter(|first| !first.is_empty() && first.chars().all(|c| OPTION_LETTERS.contains(c)))
        .cloned();

    let start = if old_style.is_some() { 2 } else { 1 };
    let mut cur = ArgCursor::new(PROG, argv, start);

    if let Some(bundle) = old_style {
        for c in bundle.chars() {
            apply_short(c, None, &mut st, &mut cur)?;
        }
    }

    let mut operands: Vec<String> = Vec::new();
    while let Some(arg) = cur.next() {
        if arg == "--" {
            operands.extend(cur.rest());
            break;
        } else if let Some(long) = arg.strip_prefix("--") {
            apply_long(long, &mut st, &mut cur)?;
        } else if arg.len() > 1 && arg.starts_with('-') {
            apply_cluster(&arg[1..], &mut st, &mut cur)?;
        } else {
            // A bare "-" is a pathname operand, not an option.
            operands.push(arg);
        }
    }

    st.args.files_and_patterns.extend(operands);
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
        'c' => st.set_mode(Mode::Create)?,
        'x' => st.set_mode(Mode::Extract)?,
        't' => st.set_mode(Mode::List)?,
        'r' => st.set_mode(Mode::Append)?,
        'u' => st.set_mode(Mode::Update)?,
        'v' => st.args.verbose = true,
        'z' => st.args.gzip = true,
        'p' => st.privs.push('p'),
        'm' => st.privs.push('m'),
        'h' => st.args.dereference = true,
        'k' => st.args.no_clobber = true,
        'O' => st.args.to_stdout = true,
        'P' => st.absolute = true,
        // GNU spells --no-same-owner as -o on extract, which is already the
        // default here; BSD's create-time meaning (write the V7 format) is not
        // offered, so this is accepted and does nothing either way.
        'o' => {}
        'j' | 'J' | 'Z' => {
            return Err(unsupported(
                PROG,
                &format!("-{}", c),
                "only gzip (-z) compression is built in",
            ))
        }
        'A' => return Err(unsupported(PROG, "-A", "archives cannot be concatenated")),
        'w' => return Err(unsupported(PROG, "-w", "no interactive confirmation")),
        'S' => return Err(unsupported(PROG, "-S", "sparse files are not detected")),
        'W' => return Err(unsupported(PROG, "-W", "archives are not verified")),
        'a' => {
            return Err(unsupported(
                PROG,
                "-a",
                "the compressor is not chosen from the archive suffix",
            ))
        }
        'd' => {
            return Err(unsupported(
                PROG,
                "-d",
                "archives are not compared against the filesystem",
            ))
        }
        'i' => {
            return Err(unsupported(
                PROG,
                "-i",
                "zero blocks are not ignored mid-archive",
            ))
        }
        'l' => {
            return Err(unsupported(
                PROG,
                "-l",
                "hard-link completeness is not checked",
            ))
        }
        'n' => return Err(unsupported(PROG, "-n", "the archive is not seekable")),
        'g' | 'G' | 'N' => {
            return Err(unsupported(
                PROG,
                &format!("-{}", c),
                "no incremental or date-based selection",
            ))
        }
        'M' => return Err(unsupported(PROG, "-M", "no multi-volume archives")),
        'R' => return Err(unsupported(PROG, "-R", "block numbers are not reported")),
        'V' => return Err(unsupported(PROG, "-V", "no archive labels")),
        _ => return Err(unknown(PROG, &format!("-{}", c))),
    }
    Ok(())
}

/// Apply a short option that took an argument.
fn apply_value(c: char, value: &str, st: &mut State) -> PaxResult<()> {
    match c {
        'f' => set_archive(st, value),
        'C' => set_chdir(st, value)?,
        'T' => st
            .args
            .files_and_patterns
            .extend(read_name_list(value, st.null)?),
        'X' => st
            .args
            .exclude_patterns
            .extend(read_name_list(value, false)?),
        'b' => set_blocking_factor(st, value)?,
        _ => unreachable!("takes_arg and apply_value disagree about -{}", c),
    }
    Ok(())
}

/// Apply a cluster of short options from a single `-xyz` argument.
fn apply_cluster(cluster: &str, st: &mut State, cur: &mut ArgCursor) -> PaxResult<()> {
    for (i, c) in cluster.char_indices() {
        if takes_arg(c) {
            // Whatever follows the letter in this argument is its value.
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

    // Long options that map straight onto a short one keep a single
    // implementation by delegating.
    let as_short = |c: char| -> Option<char> { Some(c) };
    let short = match name {
        "create" => as_short('c'),
        "extract" | "get" => as_short('x'),
        "list" => as_short('t'),
        "append" => as_short('r'),
        "update" => as_short('u'),
        "verbose" => as_short('v'),
        "gzip" | "gunzip" | "ungzip" => as_short('z'),
        "preserve-permissions" | "same-permissions" => as_short('p'),
        "touch" => as_short('m'),
        "dereference" => as_short('h'),
        "keep-old-files" => as_short('k'),
        "to-stdout" => as_short('O'),
        "absolute-names" => as_short('P'),
        "file" => as_short('f'),
        "directory" => as_short('C'),
        "files-from" => as_short('T'),
        "exclude-from" => as_short('X'),
        "blocking-factor" => as_short('b'),
        _ => None,
    };
    if let Some(c) = short {
        return apply_short(c, inline, st, cur);
    }

    match name {
        "same-owner" => st.privs.push('o'),
        // Already the default: ownership is only restored when asked for.
        "no-same-owner" => {}
        "null" => st.null = true,
        "no-null" => st.null = false,
        "no-recursion" => st.args.dir_no_follow = true,
        "recursion" => st.args.dir_no_follow = false,
        "exclude" => {
            let value = cur.value("--exclude", inline)?;
            st.args.exclude_patterns.push(value);
        }
        "strip-components" => {
            let value = cur.value("--strip-components", inline)?;
            st.args.strip_components = parse_number(PROG, "--strip-components", &value)? as usize;
        }
        "format" => {
            let value = cur.value("--format", inline)?;
            st.format = Some(match value.as_str() {
                // v7 and the GNU formats are close enough to ustar that the
                // ustar writer is the honest choice; anything a member needs
                // beyond it is an error rather than a silent GNU extension.
                "v7" | "ustar" | "gnu" | "oldgnu" => Format::Ustar,
                "pax" | "posix" => Format::Pax,
                other => return Err(usage(PROG, format!("unknown archive format '{}'", other))),
            });
        }
        "bzip2" | "xz" | "lzma" | "compress" | "zstd" | "lzip" | "lzop" => {
            return Err(unsupported(
                PROG,
                &format!("--{}", name),
                "only gzip (-z) compression is built in",
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

fn set_archive(st: &mut State, value: &str) {
    // "-" is tar's spelling of standard input or output, which is what pax does
    // when no archive is named at all.
    st.args.archive = if value == "-" {
        None
    } else {
        Some(PathBuf::from(value))
    };
}

fn set_chdir(st: &mut State, value: &str) -> PaxResult<()> {
    if st.args.chdir.is_some() {
        // GNU applies each -C at the point it appears among the operands. That
        // is not implemented, and quietly honoring only one of several would
        // archive the wrong files, so a second -C is refused outright.
        return Err(usage(PROG, "only one -C directory is supported"));
    }
    st.args.chdir = Some(PathBuf::from(value));
    Ok(())
}

fn set_blocking_factor(st: &mut State, value: &str) -> PaxResult<()> {
    // tar counts 512-byte blocks where pax counts bytes.
    let factor = parse_number(PROG, "-b", value)?;
    let bytes = factor.checked_mul(512).filter(|b| *b <= u32::MAX as u64);
    match bytes {
        Some(b) => {
            st.args.blocksize = Some(b as u32);
            Ok(())
        }
        None => Err(usage(
            PROG,
            format!("blocking factor '{}' is out of range", value),
        )),
    }
}

/// Turn the accumulated state into the internal options pax runs on.
fn finish(mut st: State) -> PaxResult<Args> {
    let Some(mode) = st.mode else {
        return Err(usage(PROG, "one of -c, -x, -t, -r or -u is required"));
    };

    match mode {
        Mode::Create => st.args.write_mode = true,
        Mode::Extract => st.args.read_mode = true,
        Mode::List => {}
        Mode::Append => {
            st.args.write_mode = true;
            st.args.append = true;
        }
        Mode::Update => {
            st.args.write_mode = true;
            st.args.append = true;
            st.args.update = true;
        }
    }

    let extracting = matches!(mode, Mode::Extract | Mode::List);
    if st.absolute && extracting {
        // Honoring -P here would mean letting a member write outside the
        // extraction directory, which is the one guarantee read mode makes.
        return Err(unsupported(
            PROG,
            "-P",
            "members are always extracted below the current directory",
        ));
    }

    if st.args.to_stdout && mode != Mode::Extract {
        return Err(usage(PROG, "-O applies only to extraction (-x)"));
    }

    if !st.absolute && !extracting {
        // Without -P, tar stores pathnames relative: a leading slash run is
        // dropped from the *stored* name only. An empty result (the operand was
        // "/") makes pax skip the member, which is the right outcome.
        st.args.substitutions.insert(0, ",^//*,,".to_string());
    }

    // tar always writes an interchange format; only the flavor is selectable.
    st.args.format = Some(st.format.unwrap_or(Format::Ustar));

    if !st.privs.is_empty() {
        st.args.privs = Some(st.privs.clone());
    }

    Ok(st.args)
}

const USAGE: &str = "\
Usage: tar -c [OPTION...] [FILE...]     create an archive
       tar -x [OPTION...] [MEMBER...]   extract from an archive
       tar -t [OPTION...] [MEMBER...]   list an archive
       tar -r [OPTION...] [FILE...]     append to an archive
       tar -u [OPTION...] [FILE...]     append files newer than their member

The option letters of the first operand may be written without a leading
dash, as in `tar cvf archive.tar dir`.

Operation:
  -c, --create               create a new archive
  -x, --extract, --get       extract members from an archive
  -t, --list                 list the members of an archive
  -r, --append               append files to the end of an archive
  -u, --update               append only files newer than their archive member

Options:
  -f, --file=ARCHIVE         use ARCHIVE; \"-\" means standard input or output
  -v, --verbose              list each file processed
  -z, --gzip                 filter the archive through gzip
  -C, --directory=DIR        change to DIR before operating
  -b, --blocking-factor=N    use N 512-byte blocks per record
      --format=FMT           write v7, ustar, gnu (as ustar), or pax/posix
  -T, --files-from=FILE      take the file list from FILE (\"-\" is stdin)
      --null                 the -T list is NUL-separated
      --exclude=PATTERN      do not process names matching PATTERN
  -X, --exclude-from=FILE    read exclusion patterns from FILE
      --no-recursion         do not descend into directories
  -h, --dereference          archive the file a symlink points to
  -k, --keep-old-files       do not overwrite existing files
  -m, --touch                do not restore modification times
  -O, --to-stdout            write member contents to standard output
  -p, --preserve-permissions restore permissions exactly
      --same-owner           restore owner and group (needs privilege)
      --strip-components=N   drop N leading pathname components on extraction
      --help                 print this message
      --version              print the version

Unsupported options are diagnosed rather than ignored. Notably -j/-J and the
other compressors are not built in, and -P is refused when extracting so that
no member can be written outside the current directory.
";

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    /// Parse a tar command line given without the leading program name.
    fn tar(args: &[&str]) -> PaxResult<Args> {
        let mut argv = vec!["tar".to_string()];
        argv.extend(args.iter().map(|s| s.to_string()));
        parse(argv)
    }

    #[test]
    fn test_old_style_bundle_takes_its_arguments_in_order() {
        // `tar cvf out.tar dir`: the letters come first with no dash, and -f's
        // argument is the next operand rather than something glued to it.
        let args = tar(&["cvf", "out.tar", "dir"]).unwrap();
        assert!(args.write_mode);
        assert!(args.verbose);
        assert_eq!(args.archive.as_deref(), Some(Path::new("out.tar")));
        assert_eq!(args.files_and_patterns, vec!["dir".to_string()]);
    }

    #[test]
    fn test_every_option_letter_reports_itself() {
        // OPTION_LETTERS is what decides whether a bare first operand is an
        // old-style bundle, so a letter listed there but missing from
        // apply_short would answer "unrecognized option" -- exactly the
        // answer the closed set exists to prevent.
        for c in OPTION_LETTERS.chars() {
            let err = match tar(&[&format!("-{}", c), "x"]) {
                Ok(_) => continue,
                Err(e) => e.to_string(),
            };
            assert!(
                !err.contains("unrecognized"),
                "-{} is in OPTION_LETTERS but apply_short does not handle it: {}",
                c,
                err
            );
        }
    }

    #[test]
    fn test_first_operand_that_is_not_all_option_letters_stays_an_operand() {
        // "readme" contains letters this parser does not know, so it cannot be
        // an old-style bundle and must be treated as a pathname.
        let args = tar(&["-cf", "out.tar", "readme"]).unwrap();
        assert_eq!(args.files_and_patterns, vec!["readme".to_string()]);
        assert!(tar(&["readme"]).is_err(), "no operation was selected");
    }

    #[test]
    fn test_option_argument_may_be_glued_or_separate() {
        assert_eq!(
            tar(&["-cfout.tar", "."]).unwrap().archive.as_deref(),
            Some(Path::new("out.tar"))
        );
        assert_eq!(
            tar(&["-c", "-f", "out.tar", "."])
                .unwrap()
                .archive
                .as_deref(),
            Some(Path::new("out.tar"))
        );
        assert_eq!(
            tar(&["-c", "--file=out.tar", "."])
                .unwrap()
                .archive
                .as_deref(),
            Some(Path::new("out.tar"))
        );
    }

    #[test]
    fn test_dash_f_means_standard_input_or_output() {
        assert!(tar(&["-cf", "-", "."]).unwrap().archive.is_none());
    }

    #[test]
    fn test_preservation_flags_become_pax_privileges() {
        assert_eq!(tar(&["-xpf", "a.tar"]).unwrap().privs.as_deref(), Some("p"));
        assert_eq!(tar(&["-xmf", "a.tar"]).unwrap().privs.as_deref(), Some("m"));
        assert_eq!(
            tar(&["-xf", "a.tar", "--same-owner"])
                .unwrap()
                .privs
                .as_deref(),
            Some("o")
        );
        // The pax defaults already match tar's, so nothing is asked for.
        assert_eq!(tar(&["-xf", "a.tar"]).unwrap().privs, None);
        assert_eq!(
            tar(&["-xf", "a.tar", "--no-same-owner"]).unwrap().privs,
            None
        );
    }

    #[test]
    fn test_blocking_factor_is_counted_in_blocks_not_bytes() {
        assert_eq!(
            tar(&["-cf", "a.tar", "-b", "4", "."]).unwrap().blocksize,
            Some(2048)
        );
        assert!(tar(&["-cf", "a.tar", "-b", "x", "."]).is_err());
    }

    #[test]
    fn test_modes_map_onto_pax_dispatch() {
        let list = tar(&["-tf", "a.tar"]).unwrap();
        assert!(!list.read_mode && !list.write_mode, "list is neither");

        let append = tar(&["-rf", "a.tar", "x"]).unwrap();
        assert!(append.write_mode && append.append && !append.update);

        let update = tar(&["-uf", "a.tar", "x"]).unwrap();
        assert!(update.write_mode && update.append && update.update);

        assert!(tar(&["-cxf", "a.tar"]).is_err(), "two operations conflict");
    }

    #[test]
    fn test_create_strips_leading_slashes_unless_dash_p() {
        let relative = tar(&["-cf", "a.tar", "/etc"]).unwrap();
        assert_eq!(
            relative.substitutions.first().map(String::as_str),
            Some(",^//*,,")
        );

        let absolute = tar(&["-cPf", "a.tar", "/etc"]).unwrap();
        assert!(absolute.substitutions.is_empty());

        // Extraction is another matter: -P there would let a member escape.
        assert!(tar(&["-xPf", "a.tar"]).is_err());
    }

    #[test]
    fn test_format_selection() {
        assert_eq!(
            tar(&["-cf", "a.tar", "."]).unwrap().format,
            Some(Format::Ustar)
        );
        assert_eq!(
            tar(&["-cf", "a.tar", "--format=pax", "."]).unwrap().format,
            Some(Format::Pax)
        );
        // v7 and the GNU flavors are written as ustar rather than approximated.
        assert_eq!(
            tar(&["-cf", "a.tar", "--format=gnu", "."]).unwrap().format,
            Some(Format::Ustar)
        );
        assert!(tar(&["-cf", "a.tar", "--format=zip", "."]).is_err());
    }

    #[test]
    fn test_double_dash_ends_option_parsing() {
        let args = tar(&["-cf", "a.tar", "--", "-x", "--exclude"]).unwrap();
        assert_eq!(
            args.files_and_patterns,
            vec!["-x".to_string(), "--exclude".to_string()]
        );
    }

    #[test]
    fn test_missing_option_argument_is_diagnosed() {
        assert!(tar(&["-cf"]).is_err());
        assert!(tar(&["-cf", "a.tar", "--strip-components"]).is_err());
    }
}
