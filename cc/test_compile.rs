//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// In-process compilation for unit tests: one translation unit from source
// text to assembly through `pipeline`, the same stages `c17 -S` runs, with
// its diagnostics captured instead of printed.
//
// Spawning `c17` costs a process per case; a diagnostic test needs none, so
// these run here.
//

use crate::diag;
use crate::opt::Optimization;
use crate::pipeline::{self, CodegenOptions, Quiet};
use crate::strings::StringTable;
use crate::target::{self, Target};
use crate::token::preprocess::SystemSearch;
use crate::token::{preprocess_collecting, PreprocessConfig};

/// The stack a compile runs on, as the driver's own compiler thread: the
/// front end recurses per nesting level, and the test harness's default
/// stack is too small for the deep cases.
const STACK_BYTES: usize = 256 * 1024 * 1024;

/// What compiling one translation unit produced, shaped like the
/// integration suite's `C17Run` so a case reads the same in either.
pub struct Compiled {
    /// Whether it compiled.
    pub success: bool,
    /// Every diagnostic line, as `c17` would print it to stderr, including
    /// the driver's final `c17: <name>.c: ...` line on failure.
    pub stderr: String,
    /// The assembly, if it compiled.
    pub asm: Option<String>,
}

/// The command-line options a test may pass. Anything else is a test bug and
/// panics, so an option is never silently ignored.
#[derive(Default)]
struct Options {
    optimization: Optimization,
    debug: bool,
    pic: bool,
    pie: bool,
    no_pie: bool,
    shared: bool,
    no_unwind_tables: bool,
    verbose_asm: bool,
    math_errno: bool,
    trapping_math: bool,
    defines: Vec<String>,
    undefines: Vec<String>,
    include_paths: Vec<String>,
    target: Option<String>,
    /// `-finline` / `-fno-inline`, last one winning, applied to whatever
    /// level the `-O` options leave, as the driver does.
    inlining: Option<bool>,
    /// `-fsigned-char` / `-funsigned-char` (and the `-fno-` inverses), last
    /// one winning, overriding the target's plain `char`.
    plain_char: Option<target::CharSignedness>,
    mflags: Vec<String>,
}

/// Apply `flags` the way the driver does: the switches that live in
/// thread-local state are set on the calling thread, the rest returned.
fn apply_flags(flags: &[&str]) -> Options {
    let mut o = Options {
        optimization: Optimization::from_flag("0").unwrap(),
        math_errno: true,
        trapping_math: true,
        ..Options::default()
    };
    let mut no_groups = std::collections::HashSet::new();
    let mut no_builtin_funcs = std::collections::HashSet::new();
    for &flag in flags {
        match flag {
            "-w" => diag::suppress_warnings(),
            "-fpermissive" => diag::set_permissive(),
            "-fno-builtin" => crate::builtins::set_no_builtin(),
            "-fgnu89-inline" => crate::builtins::set_gnu89_inline(true),
            "-g" => o.debug = true,
            "-O" => o.optimization = Optimization::from_flag("1").unwrap(),
            "-fPIC" | "-fpic" => o.pic = true,
            "-fPIE" | "-fpie" => o.pie = true,
            "-fno-pie" => o.no_pie = true,
            "-shared" | "--shared" | "-G" => o.shared = true,
            "--fno-unwind-tables" => o.no_unwind_tables = true,
            "-fverbose-asm" => o.verbose_asm = true,
            "-fmath-errno" => o.math_errno = true,
            "-fno-math-errno" => o.math_errno = false,
            "-fno-trapping-math" => o.trapping_math = false,
            "-fno-inline" => o.inlining = Some(false),
            "-finline" => o.inlining = Some(true),
            "-fsigned-char" | "-fno-unsigned-char" => {
                o.plain_char = Some(target::CharSignedness::Signed)
            }
            "-funsigned-char" | "-fno-signed-char" => {
                o.plain_char = Some(target::CharSignedness::Unsigned)
            }
            _ => {
                if let Some(level) = flag.strip_prefix("-O") {
                    o.optimization = Optimization::from_flag(level).unwrap();
                } else if let Some(name) = flag.strip_prefix("-Wno-") {
                    no_groups.insert(name.to_string());
                } else if let Some(name) = flag.strip_prefix("-fno-builtin-") {
                    no_builtin_funcs.insert(name.to_string());
                } else if let Some(d) = flag.strip_prefix("-D") {
                    o.defines.push(d.to_string());
                } else if let Some(u) = flag.strip_prefix("-U") {
                    o.undefines.push(u.to_string());
                } else if let Some(i) = flag.strip_prefix("-I") {
                    o.include_paths.push(i.to_string());
                } else if let Some(t) = flag.strip_prefix("--target=") {
                    o.target = Some(t.to_string());
                } else if flag.starts_with("-m") && flag.len() > 2 {
                    o.mflags.push(flag.to_string());
                } else {
                    panic!("test_compile: unsupported option {flag}");
                }
            }
        }
    }
    if let Some(enabled) = o.inlining {
        o.optimization.set_inlining(enabled);
    }
    diag::suppress_warning_groups(no_groups);
    crate::builtins::set_no_builtin_funcs(no_builtin_funcs);
    o
}

/// The body of [`compile`], on the compile thread.
fn compile_here(name: &str, src: &str, flags: &[&str]) -> Compiled {
    diag::reset_counts();
    diag::clear_streams();
    diag::capture_diagnostics();

    let source_name = format!("{name}.c");
    let o = apply_flags(flags);
    let mut target = match &o.target {
        Some(triple) => Target::from_triple(triple).expect("unsupported target"),
        None => Target::host(),
    };
    // As the driver: the `-m` ISA options select x86-64's instructions.
    if target.arch == target::Arch::X86_64 {
        target.x86_isa = target::X86Isa::from_flags(&o.mflags);
    }
    if let Some(signedness) = o.plain_char {
        target.plain_char = signedness;
    }
    // As the driver's `position_independence`: PIE is the Linux default
    // unless a shared object or `-fno-pie` asks otherwise, and implies PIC.
    let pie = !(o.shared || o.no_pie) && (o.pie || target.os == target::Os::Linux);
    let position = target::PositionIndependence {
        pic: o.pic || o.shared || pie,
        pie,
    };

    let mut strings = StringTable::new();
    let (tokens, _) =
        pipeline::source_tokens(src.as_bytes(), &source_name, false, false, &mut strings);
    let (preprocessed, _) = preprocess_collecting(
        tokens,
        &target,
        &mut strings,
        &source_name,
        &PreprocessConfig {
            defines: &o.defines,
            undefines: &o.undefines,
            include_paths: &o.include_paths,
            search: SystemSearch::default(),
            no_std_inc: false,
            no_builtin_inc: false,
            trigraphs: false,
            preprocessed: false,
            pre_includes: &[],
            dump_macros: false,
            collect_dependencies: false,
            optimization: o.optimization,
            position,
            isa: target::X86Isa::from_flags(&o.mflags),
        },
    );

    let opts = CodegenOptions {
        optimization: o.optimization,
        math_errno: o.math_errno,
        debug: o.debug,
        trapping_math: o.trapping_math,
        default_visibility: None,
        shared_mode: o.shared || o.pic,
        pic: position.pic,
        unwind_tables: !o.no_unwind_tables,
        verbose_asm: o.verbose_asm,
        source_name: &source_name,
    };
    let result = pipeline::compile_tokens(preprocessed, &strings, &target, &opts, &mut Quiet);

    let mut diags = diag::take_captured_diagnostics();
    let asm = match result {
        Ok(asm) => asm,
        Err(e) => {
            diags.push(format!(
                "c17: {source_name}: {}",
                plib::diag::io_error_text(&e)
            ));
            None
        }
    };
    let mut stderr = diags.join("\n");
    if !stderr.is_empty() {
        stderr.push('\n');
    }
    Compiled {
        success: asm.is_some(),
        stderr,
        asm,
    }
}

/// Compile `src` as `<name>.c` with `flags`, as `c17 -S` would, on a thread
/// of its own so that no switch or count leaks between tests.
pub fn compile(name: &str, src: &str, flags: &[&str]) -> Compiled {
    let name = name.to_string();
    let src = src.to_string();
    let flags: Vec<String> = flags.iter().map(|f| f.to_string()).collect();
    std::thread::Builder::new()
        .stack_size(STACK_BYTES)
        .spawn(move || {
            let flags: Vec<&str> = flags.iter().map(String::as_str).collect();
            compile_here(&name, &src, &flags)
        })
        .expect("failed to start the compile thread")
        .join()
        .unwrap_or_else(|panic| std::panic::resume_unwind(panic))
}

// The assertions below have the names and signatures of the integration
// suite's `tests/common` helpers they replace, so a case moves between the
// two without being rewritten.

/// Compile `content` and require it to be rejected with a diagnostic
/// mentioning `expected`.
#[track_caller]
pub fn compile_expect_error(name: &str, content: &str, expected: &str) {
    let stderr = compile_rejected(name, content);
    assert!(
        stderr.contains(expected),
        "'{name}' was rejected, but no diagnostic mentioned {expected:?}.\nstderr:\n{stderr}"
    );
}

/// Compile `content`, require it to be rejected, and return every diagnostic.
#[track_caller]
pub fn compile_rejected(name: &str, content: &str) -> String {
    compile_rejected_with(name, content, &[])
}

/// [`compile_rejected`] with command-line options.
#[track_caller]
pub fn compile_rejected_with(name: &str, content: &str, extra: &[&str]) -> String {
    let c = compile(name, content, extra);
    let stderr = c.stderr;
    assert!(
        !c.success,
        "'{name}' should have been rejected but compiled cleanly.\nSource:\n{content}\nstderr:\n{stderr}"
    );
    stderr
}

/// Compile `content` and require it to be accepted.
#[track_caller]
pub fn compile_expect_ok(name: &str, content: &str) {
    compile_accepted(name, content, &[]);
}

/// Compile `content`, require it to be accepted, and return every diagnostic.
#[track_caller]
pub fn compile_accepted(name: &str, content: &str, extra: &[&str]) -> String {
    let c = compile(name, content, extra);
    let stderr = c.stderr;
    assert!(
        c.success,
        "'{name}' should have compiled, but was rejected.\nSource:\n{content}\nstderr:\n{stderr}"
    );
    stderr
}

/// Compile `content` and require it to be accepted with a diagnostic
/// mentioning `expected`.
#[track_caller]
pub fn compile_expect_warning(name: &str, content: &str, expected: &str) {
    let stderr = compile_accepted(name, content, &[]);
    assert!(
        stderr.contains(expected),
        "'{name}' compiled, but no diagnostic mentioned {expected:?}.\nstderr:\n{stderr}"
    );
}

/// Compile `content` with options, require it to be accepted, and return
/// every diagnostic.
#[track_caller]
pub fn compile_expect_warning_with(name: &str, content: &str, extra: &[String]) -> String {
    let extra: Vec<&str> = extra.iter().map(String::as_str).collect();
    compile_accepted(name, content, &extra)
}

/// Compile `content` and require it to be accepted without a diagnostic
/// mentioning `forbidden`.
#[track_caller]
pub fn compile_expect_no_diagnostic(name: &str, content: &str, forbidden: &str) {
    let stderr = compile_accepted(name, content, &[]);
    assert!(
        !stderr.contains(forbidden),
        "'{name}' compiled, but a diagnostic mentioned {forbidden:?}.\nstderr:\n{stderr}"
    );
}

/// The assembly of a translation unit that must compile.
#[track_caller]
pub fn asm_for(name: &str, src: &str, flags: &[&str]) -> String {
    let c = compile(name, src, flags);
    match c.asm {
        Some(asm) => asm,
        None => panic!(
            "'{name}' should have compiled:\n{}\nSource:\n{src}",
            c.stderr
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn captures_errors_and_warnings() {
        let c = compile("t", "int f(void) { return undeclared; }\n", &[]);
        assert!(!c.success);
        assert!(c.stderr.contains("t.c:1:"), "{}", c.stderr);
        assert!(c.stderr.contains("error: "), "{}", c.stderr);
        assert!(c.stderr.contains("c17: t.c: "), "{}", c.stderr);

        let src = "int f(void) { int a[2] = {1, 2, 3}; return a[0]; }\n";
        let warned = compile("w", src, &[]);
        let quiet = compile("w", src, &["-w"]);
        assert!(warned.success && quiet.success);
        assert!(warned.stderr.contains("warning: "), "{}", warned.stderr);
        assert!(quiet.stderr.is_empty(), "{}", quiet.stderr);
    }

    #[test]
    fn switches_stay_on_their_thread() {
        // -fpermissive turns an implicit declaration into a warning, and must
        // not leak into the next compile.
        let src = "int main(void) { return f(); }\n";
        assert!(compile("p", src, &["-fpermissive"]).success);
        assert!(!compile("p", src, &[]).success);
    }

    #[test]
    fn produces_assembly() {
        let asm = asm_for("a", "int add(int a, int b) { return a + b; }\n", &["-O2"]);
        assert!(asm.contains("add"), "{asm}");
    }
}
