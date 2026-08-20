//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Order-sensitive link-line reconstruction.
//
// c17 departs from XBD 12.2 to say the order of -L, -l, -R and the pathname
// operands is significant (c17.md 87866-87867):
//
//     "A library shall be searched when its name is encountered"
//
// and EXAMPLE 3 relies on it:
//
//     c17 -L /a/b/c main.o a.c -l Q b.c -l p
//
// Here `-l Q` must be searched after main.o and a.o but before b.o, because a
// definition pulled from libQ can only satisfy references seen so far. An
// argument parser that collects each flag into its own list (as clap does)
// loses the interleaving entirely: it can only emit every object, then every
// -L, then every -l.
//
// So the argument vector is rescanned here to recover one ordered stream.
// Scanning happens *after* `preprocess_args()` has normalized the vector, so
// this only has to understand the separated spellings (`-L dir`, `-l lib`) —
// the attached forms (`-L.`, `-lz`) are split there, and that stays the single
// place that knows the raw GCC-compatible spellings.

use std::path::Path;
use std::process::Command;

/// One item on the link line, in the order it appeared.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LinkArg {
    /// `-L dir` — a library search path.
    LibPath(String),
    /// `-l library` — a library to search at this point.
    Library(String),
    /// `-R dir` — a runtime (dynamic linker) search path.
    RunPath(String),
    /// A pathname operand: a source, object, archive, or shared library.
    Operand(String),
}

/// Options that consume the argument that follows them, in the vector as
/// normalized by `preprocess_args()`.
///
/// This table must track the `Args` struct: an option added there that takes a
/// separate value and is missing here would have its value mistaken for a
/// pathname operand, putting a stray entry on the link line.
///
/// `--dump-ir` is deliberately absent. It is optional-valued, so its presence
/// is ambiguous here — and it suppresses the link phase anyway, so a
/// misclassification cannot reach a link line.
const VALUE_OPTIONS: &[&str] = &[
    "-D",
    "-U",
    "-I",
    "-o",
    "-W",
    "-B",
    "--target",
    "--rtlib",
    "--dump-ir-func",
    "--c17-fno-builtin-func",
    "--c17-linker-flag",
    "--c17-unsupported-mflag",
    "--sysroot",
    "-isystem",
    "-idirafter",
];

/// The libraries `-l` is required to find, c17.md 88057-88093.
///
/// The obligation is stronger than "forward the name and hope": 88089-88093
/// says these *shall be found* when named, and that — except for the shared C
/// library — they "need not exist as regular files". A conforming
/// implementation therefore has to satisfy `-l xnet` on a system that puts the
/// socket interfaces in libc and ships no `libxnet` at all, which is every
/// glibc system and every macOS.
pub const POSIX_STANDARD_LIBRARIES: &[&str] = &["c", "l", "m", "pthread", "rt", "xnet", "y"];

/// The file forms a library can take, in the order worth probing.
///
/// `.so` and `.a` are the two POSIX names (88924-88929). `.dylib` is
/// macOS's shared form and `.tbd` its SDK link-time stub, both of which the
/// linker resolves for `-l<name>`; a user who puts one in a `-L` directory has
/// supplied that library, and probing only the POSIX pair would drop the `-l`
/// and silently ignore it.
///
/// Probed unconditionally rather than under `#[cfg(target_os)]`: the `-L`
/// directories are the user's, and the search is performed by the host linker
/// whatever target c17 was asked for.
const LIBRARY_SUFFIXES: &[&str] = &["so", "a", "dylib", "tbd"];

/// Whether `-l <name>` must be dropped rather than handed to the linker.
///
/// Only the seven standard names are ever dropped, and only when the host has
/// no such library in either form. Anything else — a name the user chose, or a
/// standard name the host really does provide — is forwarded untouched, so
/// this cannot mask a genuine "cannot find -lfoo".
///
/// Dropping is the whole mechanism: on the platforms where these come up
/// missing, the interfaces are in the C library, which the host driver links
/// unconditionally. `-l xnet` and `-l y` used to reach `ld` verbatim and fail.
///
/// `lib_paths` is the `-L` directories seen *before* this `-l`, per 88925-88929
/// — a later `-L` cannot satisfy an earlier `-l`.
pub fn drop_standard_library(name: &str, lib_paths: &[String]) -> bool {
    // 88090-88091 exempts one of the seven: "except for the shared library
    // version of the c library, need not exist as regular files". libc is the
    // one that does exist, so `-l c` is always forwarded and never decided by
    // a probe. That matters on macOS, where none of the seven can be found as
    // a file at all -- /usr/lib/libc.dylib has not been on disk since the
    // dyld shared cache, only an SDK `.tbd` stub the driver resolves itself --
    // so probing would have dropped even this one.
    if name == "c" {
        return false;
    }
    POSIX_STANDARD_LIBRARIES.contains(&name) && !standard_library_exists(name, lib_paths)
}

/// Whether a library file for `name` can be found, in `lib_paths` or on the
/// host driver's own default search path.
fn standard_library_exists(name: &str, lib_paths: &[String]) -> bool {
    let files = LIBRARY_SUFFIXES
        .iter()
        .map(|suffix| format!("lib{}.{}", name, suffix))
        .collect::<Vec<_>>();

    for dir in lib_paths {
        if files.iter().any(|f| Path::new(dir).join(f).exists()) {
            return true;
        }
    }

    // The default search path belongs to the host driver, so ask it rather
    // than guessing at directory layouts. `-print-file-name` echoes the bare
    // name back when it finds nothing, which is the "absent" answer.
    //
    // Every suffix has to be tried, not just `.so`: glibc 2.34 folded librt
    // and libpthread into libc and left empty `librt.a` / `libpthread.a` stubs
    // behind with no `.so` of either, so probing only `.so` would drop a
    // `-l rt` that links perfectly well.
    files.iter().any(|f| {
        Command::new("cc")
            .arg(format!("-print-file-name={}", f))
            .output()
            .ok()
            .filter(|out| out.status.success())
            .map(|out| String::from_utf8_lossy(&out.stdout).trim().to_string())
            .is_some_and(|found| found != *f && Path::new(&found).exists())
    })
}

/// Recover the ordered link line from a normalized argument vector.
///
/// `argv` includes `argv[0]`, which is skipped. Everything after a `--`
/// terminator is an operand.
pub fn scan<I>(argv: I) -> Vec<LinkArg>
where
    I: IntoIterator<Item = String>,
{
    let mut out = Vec::new();
    let mut it = argv.into_iter();
    let _ = it.next(); // argv[0]
    let mut operands_only = false;

    while let Some(arg) = it.next() {
        if operands_only {
            out.push(LinkArg::Operand(arg));
            continue;
        }
        if arg == "--" {
            operands_only = true;
            continue;
        }

        match arg.as_str() {
            "-L" => {
                if let Some(v) = it.next() {
                    out.push(LinkArg::LibPath(v));
                }
            }
            "-l" => {
                if let Some(v) = it.next() {
                    out.push(LinkArg::Library(v));
                }
            }
            "-R" => {
                if let Some(v) = it.next() {
                    out.push(LinkArg::RunPath(v));
                }
            }
            _ if VALUE_OPTIONS.contains(&arg.as_str()) => {
                let _ = it.next();
            }
            // A bare `-` is the stdin operand, not an option.
            _ if arg.starts_with('-') && arg != "-" => {}
            _ => out.push(LinkArg::Operand(arg)),
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn argv(items: &[&str]) -> Vec<String> {
        std::iter::once("c17")
            .chain(items.iter().copied())
            .map(String::from)
            .collect()
    }

    #[test]
    fn spec_example_3_interleaving_is_preserved() {
        // c17 -L /a/b/c main.o a.c -l Q b.c -l p
        // (as normalized by preprocess_args: -L and -l already separated)
        assert_eq!(
            scan(argv(&[
                "-L", "/a/b/c", "main.o", "a.c", "-l", "Q", "b.c", "-l", "p"
            ])),
            vec![
                LinkArg::LibPath("/a/b/c".into()),
                LinkArg::Operand("main.o".into()),
                LinkArg::Operand("a.c".into()),
                LinkArg::Library("Q".into()),
                LinkArg::Operand("b.c".into()),
                LinkArg::Library("p".into()),
            ]
        );
    }

    #[test]
    fn option_values_are_not_mistaken_for_operands() {
        assert_eq!(
            scan(argv(&[
                "-o", "out", "-D", "FOO=1", "-I", "/inc", "-U", "BAR", "-W", "all", "prog.c",
            ])),
            vec![LinkArg::Operand("prog.c".into())]
        );
    }

    #[test]
    fn valueless_options_are_skipped() {
        assert_eq!(
            scan(argv(&["-c", "-g", "-O2", "--shared", "x.c"])),
            vec![LinkArg::Operand("x.c".into())]
        );
    }

    #[test]
    fn runpath_is_recorded_in_place() {
        assert_eq!(
            scan(argv(&["a.o", "-R", "/opt/lib", "-l", "m"])),
            vec![
                LinkArg::Operand("a.o".into()),
                LinkArg::RunPath("/opt/lib".into()),
                LinkArg::Library("m".into()),
            ]
        );
    }

    #[test]
    fn double_dash_makes_everything_an_operand() {
        assert_eq!(
            scan(argv(&["-c", "--", "-weird.c", "b.c"])),
            vec![
                LinkArg::Operand("-weird.c".into()),
                LinkArg::Operand("b.c".into()),
            ]
        );
    }

    #[test]
    fn bare_dash_is_an_operand() {
        assert_eq!(
            scan(argv(&["-", "-l", "m"])),
            vec![LinkArg::Operand("-".into()), LinkArg::Library("m".into())]
        );
    }

    /// Only the seven standard names are ever dropped. A name the user chose
    /// must reach the linker even when nothing on the host provides it, or a
    /// typo turns into a silent link against nothing.
    #[test]
    fn only_standard_libraries_are_ever_dropped() {
        assert!(!drop_standard_library("c17nosuchlibrary", &[]));
        assert!(!drop_standard_library("Q", &[]));
        assert!(!drop_standard_library("stdc++", &[]));
    }

    /// 88090-88091 exempts libc from "need not exist as regular files": it is
    /// the one of the seven the spec says does exist, so `-l c` is forwarded
    /// unconditionally and never decided by a probe.
    ///
    /// Asserted as the rule rather than by probing, because the probe is
    /// exactly what gets this wrong. On macOS none of the seven can be found
    /// as a file — /usr/lib/libc.dylib has not been on disk since the dyld
    /// shared cache — so this test failed on macOS CI while passing on Linux.
    #[test]
    fn libc_is_never_dropped() {
        assert!(!drop_standard_library("c", &[]));
        // Not even when a -L directory is given and holds nothing.
        let dir = tempfile::tempdir().unwrap();
        let paths = vec![dir.path().to_string_lossy().into_owned()];
        assert!(!drop_standard_library("c", &paths));
    }

    /// Every file form a library can take is probed, not just the two POSIX
    /// spells. A user who puts a `.dylib` in a `-L` directory has supplied
    /// that library, and dropping the `-l` would silently ignore it.
    #[test]
    fn a_supplied_library_is_found_under_any_suffix() {
        for suffix in LIBRARY_SUFFIXES {
            let dir = tempfile::tempdir().unwrap();
            let paths = vec![dir.path().to_string_lossy().into_owned()];
            // `xnet` exists on no glibc system and no macOS: the drop case.
            assert!(drop_standard_library("xnet", &paths));
            std::fs::write(dir.path().join(format!("libxnet.{}", suffix)), b"").unwrap();
            assert!(
                !drop_standard_library("xnet", &paths),
                "libxnet.{} was not recognized",
                suffix
            );
        }
    }

    /// A `-L` directory holding the library settles it without asking the host
    /// driver, and it settles it for a name that would otherwise be dropped.
    #[test]
    fn a_supplied_library_is_found_in_a_lib_path() {
        let dir = tempfile::tempdir().unwrap();
        let paths = vec![dir.path().to_string_lossy().into_owned()];

        // `xnet` exists on no glibc system, so this is the drop case...
        assert!(drop_standard_library("xnet", &paths));

        // ...until the user puts one there.
        std::fs::write(dir.path().join("libxnet.a"), b"").unwrap();
        assert!(!drop_standard_library("xnet", &paths));
    }

    #[test]
    fn a_trailing_option_missing_its_value_is_not_a_panic() {
        assert_eq!(
            scan(argv(&["a.c", "-l"])),
            vec![LinkArg::Operand("a.c".into())]
        );
    }
}
