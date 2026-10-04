//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Common test utilities for c17 integration tests
//

use plib::testing::run_test_base;
use plib::tmp::NamedTempFile;
use std::io::Write;
use std::process::Command;

// ============================================================================
// Compile Matrix Configuration
// ============================================================================

/// Full compile matrix (enabled with --features test_matrix).
/// Tests all 4 configurations: default, debug-only, optimized-only, and both.
#[cfg(feature = "test_matrix")]
pub const COMPILE_MATRIX: &[(&str, &[&str])] = &[
    ("default", &[]),
    ("debug", &["-g"]),
    ("optimized", &["-O"]),
    ("debug_opt", &["-g", "-O"]),
];

/// Default compile config: -g -O (that is, -O1) only, the fastest that still
/// catches optimization bugs.
#[cfg(not(feature = "test_matrix"))]
pub const COMPILE_MATRIX: &[(&str, &[&str])] = &[("debug_opt", &["-g", "-O"])];

// ============================================================================
// File Path Utilities
// ============================================================================

/// Create a temporary C file with the given content
/// Returns NamedTempFile which auto-deletes on drop
pub fn create_c_file(name: &str, content: &str) -> NamedTempFile {
    let mut file = plib::tmp::Builder::new()
        .prefix(&format!("c17_test_{}_", name))
        .suffix(".c")
        .tempfile()
        .expect("failed to create temp file");
    file.write_all(content.as_bytes())
        .expect("failed to write test file");
    file
}

// ============================================================================
// Matrix-aware Compile and Run (main API)
// ============================================================================

/// Internal: Compile and run with a single specific configuration (no matrix loop)
fn compile_and_run_single(
    name: &str,
    content: &str,
    extra_opts: &[String],
    config_name: &str,
) -> i32 {
    let c_file = create_c_file(name, content);
    let c_path = c_file.path().to_path_buf();

    // Use thread ID and config name to make exe path unique for parallel test execution
    let thread_id = format!("{:?}", std::thread::current().id());
    let exe_path = std::env::temp_dir().join(format!(
        "c17_exe_{}_{}_{}",
        name,
        config_name,
        thread_id.replace(|c: char| !c.is_alphanumeric(), "_")
    ));

    // The source operand goes before `extra_opts`, because those may contain
    // `-l` and a library is searched where its name is encountered — naming it
    // ahead of the object that references it resolves nothing. This is the
    // ordinary `c17 prog.c -lm` shape.
    let mut args = vec!["-o".to_string(), exe_path.to_string_lossy().to_string()];
    args.push(c_path.to_string_lossy().to_string());
    args.extend(extra_opts.iter().cloned());

    let output = run_test_base("c17", &args, &[]);

    if !output.status.success() {
        eprintln!(
            "c17 compilation failed for {} [config: {}]:\n{}",
            name,
            config_name,
            String::from_utf8_lossy(&output.stderr)
        );
        return -1;
    }

    let run_output = Command::new(&exe_path)
        .output()
        .expect("failed to run executable");

    let exit_code = run_output.status.code().unwrap_or(-1);

    // `-1` is also what a failed build returns, so say which happened and, if
    // the program died, on what signal. Without this a segfault and a compiler
    // error are the same number, and on a target that cannot be run locally
    // that is the difference between a diagnosis and a guess.
    report_abnormal_exit(name, config_name, &run_output);

    // On failure, dump generated assembly for diagnosis
    if exit_code != 0 {
        let asm_path = std::env::temp_dir().join(format!("c17_asm_{}_{}.s", name, config_name));
        let mut asm_args = vec![
            "-S".to_string(),
            "-o".to_string(),
            asm_path.to_string_lossy().to_string(),
        ];
        asm_args.push(c_path.to_string_lossy().to_string());
        asm_args.extend(extra_opts.iter().cloned());
        let asm_output = run_test_base("c17", &asm_args, &[]);
        if asm_output.status.success() {
            if let Ok(asm) = std::fs::read_to_string(&asm_path) {
                eprintln!(
                    "=== Generated assembly for '{}' [{}] ===",
                    name, config_name
                );
                eprintln!("{}", asm);
                eprintln!("=== End assembly ===");
            }
        }
        let _ = std::fs::remove_file(&asm_path);
    }

    // Cleanup exe (c_file auto-cleaned by NamedTempFile drop)
    let _ = std::fs::remove_file(&exe_path);

    exit_code
}

/// Compile two translation units together, link them, and run the result.
///
/// Every other helper here compiles a single file, which cannot see the class
/// of defect that only appears at link time: a definition emitted into two
/// objects that should have been emitted into neither. `inline` in a shared
/// header is the ordinary way to hit that.
///
/// Returns the program's exit status, or -1 if compiling or linking failed
/// (with the toolchain's own message on stderr, since a duplicate-symbol error
/// is the interesting outcome and is worth reading).
pub fn compile_and_run_two_units(
    name: &str,
    unit_a: &str,
    unit_b: &str,
    extra_opts: &[String],
) -> i32 {
    let a = create_c_file(&format!("{}_a", name), unit_a);
    let b = create_c_file(&format!("{}_b", name), unit_b);

    let thread_id = format!("{:?}", std::thread::current().id());
    let exe_path = std::env::temp_dir().join(format!(
        "c17_exe2_{}_{}",
        name,
        thread_id.replace(|c: char| !c.is_alphanumeric(), "_")
    ));

    let mut args = vec!["-o".to_string(), exe_path.to_string_lossy().to_string()];
    args.push(a.path().to_string_lossy().to_string());
    args.push(b.path().to_string_lossy().to_string());
    args.extend(extra_opts.iter().cloned());

    let output = run_test_base("c17", &args, &[]);
    if !output.status.success() {
        eprintln!(
            "c17 failed to build {} from two units:\n{}",
            name,
            String::from_utf8_lossy(&output.stderr)
        );
        return -1;
    }

    let run_output = Command::new(&exe_path)
        .output()
        .expect("failed to run executable");
    let exit_code = run_output.status.code().unwrap_or(-1);
    report_abnormal_exit(name, "single", &run_output);
    let _ = std::fs::remove_file(&exe_path);
    exit_code
}

/// Build `lib_src` into a shared object with c17, then run `main_src` (also
/// built with c17) which is expected to `dlopen` it.
///
/// The library is written to a private directory and always named `lib.so`, so
/// `main_src` can hardcode `"./lib.so"` and the test can `chdir`-free by
/// running the executable with that directory as its working directory.
///
/// This exists for the class of defect that only appears at load time. A
/// thread-local model that is wrong for a dynamically loaded library links
/// perfectly and then fails in `dlopen`, and does so only once the library's
/// TLS block outgrows the loader's static-TLS surplus -- so neither compiling
/// nor linking nor a small test case reveals it.
///
/// Returns the program's exit status, or -1 if any build step failed. `dlopen`
/// diagnostics are on stdout/stderr and are echoed on failure, since the
/// loader's message is the interesting part.
pub fn compile_and_dlopen(name: &str, lib_src: &str, main_src: &str, extra_opts: &[String]) -> i32 {
    let dir = plib::tmp::Builder::new()
        .prefix(&format!("c17_dl_{}_", name))
        .tempdir()
        .expect("failed to create work dir");

    let lib_c = dir.path().join("lib.c");
    let main_c = dir.path().join("main.c");
    std::fs::write(&lib_c, lib_src).expect("failed to write library source");
    std::fs::write(&main_c, main_src).expect("failed to write driver source");
    let so = dir.path().join("lib.so");
    let exe = dir.path().join("driver");

    let mut lib_args = vec![
        "-fPIC".to_string(),
        "--shared".to_string(),
        "-o".to_string(),
        so.to_string_lossy().into_owned(),
        lib_c.to_string_lossy().into_owned(),
    ];
    lib_args.extend(extra_opts.iter().cloned());
    let r = run_test_base("c17", &lib_args, &[]);
    if !r.status.success() {
        eprintln!(
            "c17 failed to build the shared object for {}:\n{}",
            name,
            String::from_utf8_lossy(&r.stderr)
        );
        return -1;
    }

    let main_args = vec![
        "-o".to_string(),
        exe.to_string_lossy().into_owned(),
        main_c.to_string_lossy().into_owned(),
        "-ldl".to_string(),
    ];
    let r = run_test_base("c17", &main_args, &[]);
    if !r.status.success() {
        eprintln!(
            "c17 failed to build the dlopen driver for {}:\n{}",
            name,
            String::from_utf8_lossy(&r.stderr)
        );
        return -1;
    }

    let out = Command::new(&exe)
        .current_dir(dir.path())
        .output()
        .expect("failed to run dlopen driver");
    let code = out.status.code().unwrap_or(-1);
    if code != 0 {
        eprintln!(
            "dlopen driver for {} exited {}:\n{}{}",
            name,
            code,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
    }
    code
}

/// Compile inline C code and run with all matrix configurations.
/// Returns 0 if all configurations pass, or the first non-zero exit code on failure.
pub fn compile_and_run(name: &str, content: &str, extra_opts: &[String]) -> i32 {
    for (config_name, matrix_flags) in COMPILE_MATRIX {
        // Combine matrix flags with caller's extra options
        let mut combined: Vec<String> = matrix_flags.iter().map(|s| s.to_string()).collect();
        combined.extend(extra_opts.iter().cloned());

        let result = compile_and_run_single(name, content, &combined, config_name);
        if result != 0 {
            eprintln!(
                "Test '{}' FAILED with config '{}': exit code {}",
                name, config_name, result
            );
            return result;
        }
    }
    0
}

/// Run `src` on the host at the [`COMPILE_MATRIX`] levels, at -O0 and at -O2,
/// and on aarch64 under qemu at -O0 and -O2 when the cross toolchain is
/// present; every run must exit 0.
///
/// The -O0 level is named explicitly: the matrix passes `-O`, so a host run
/// with no level of its own is -O1, never -O0.
pub fn compile_and_run_everywhere(name: &str, src: &str) {
    assert_eq!(
        compile_and_run(name, src, &[]),
        0,
        "{name} at the matrix levels"
    );
    for opt in ["-O0", "-O2"] {
        let level = vec![opt.to_string()];
        assert_eq!(compile_and_run(name, src, &level), 0, "{name} at {opt}");
        if let Some(code) = compile_and_run_aarch64(name, src, opt) {
            assert_eq!(code, 0, "{name} on aarch64 at {opt}");
        }
    }
}

/// Compile inline C code at -O1 and run it (single config, skips matrix).
/// This is used by tests that specifically test optimization behavior; a
/// defect that needs -O2 to appear needs an explicit `-O2` run instead.
pub fn compile_and_run_optimized(name: &str, content: &str) -> i32 {
    compile_and_run_single(name, content, &["-O1".to_string()], "optimized_only")
}

// ============================================================================
// Arbitrary-argv driver invocation
// ============================================================================

/// The result of invoking `c17` with an arbitrary argument vector.
pub struct C17Run {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

/// Invoke `c17` with exactly `args`.
///
/// `compile_and_run` always builds one source with one `-o`, which cannot
/// express the multi-operand and option-ordering cases the POSIX spec's
/// EXAMPLE 1 and EXAMPLE 3 describe. Driver tests need the raw vector.
pub fn run_c17(args: &[&str]) -> C17Run {
    let owned: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    let output = run_test_base("c17", &owned, &[]);
    C17Run {
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        success: output.status.success(),
    }
}

/// Compile `content` with `-S`, killing c17 if it has not finished within
/// `secs` seconds. For an input that once made c17 loop: an unbounded run of
/// a regression would not fail, it would hang the suite and eat memory.
///
/// Stderr goes to a file rather than a pipe, so a chatty run cannot block on
/// a full pipe and be mistaken for a hang.
pub fn compile_bounded(name: &str, content: &str, secs: u64) -> C17Run {
    let c_file = create_c_file(name, content);
    let err = plib::tmp::Builder::new()
        .prefix(&format!("c17_bounded_{}_", name))
        .suffix(".err")
        .tempfile()
        .expect("failed to create temp file");
    let mut child = Command::new(plib::testing::get_binary_path("c17"))
        .args(["-S", "-o", "/dev/null"])
        .arg(c_file.path())
        .env("LC_ALL", "C")
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(err.as_file().try_clone().expect("clone stderr file"))
        .spawn()
        .expect("spawn c17");
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(secs);
    let status = loop {
        if let Some(status) = child.try_wait().expect("wait for c17") {
            break status;
        }
        if std::time::Instant::now() >= deadline {
            let _ = child.kill();
            let _ = child.wait();
            panic!("'{}': c17 did not finish within {}s", name, secs);
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    };
    C17Run {
        stdout: String::new(),
        stderr: std::fs::read_to_string(err.path()).unwrap_or_default(),
        success: status.success(),
    }
}

/// Compile one translation unit with c17 and the other with the system C
/// compiler, link them together and run the result. Returns the exit status,
/// or `None` when no system compiler is available.
///
/// This is the only shape of test that can see c17 being wrong in the *same
/// way* on both sides of a call. Three ABI defects found in one day were
/// invisible to every c17-only test because the caller and the callee shifted
/// together: a zero-sized argument charged a register on aarch64, an
/// over-aligned argument placed in an area whose base was not aligned, and
/// `va_arg` rounding that argument to 16 rather than to its own alignment.
/// Each passed a c17-built program and failed against gcc.
///
/// `which_unit` says which source c17 compiles, so a single pair of units
/// gives both directions.
pub fn compile_with_host_cc(name: &str, c17_unit: &str, host_unit: &str) -> Option<i32> {
    let host_cc = ["cc", "gcc"].into_iter().find(|tool| {
        Command::new("sh")
            .arg("-c")
            .arg(format!("command -v {tool} >/dev/null 2>&1"))
            .status()
            .map(|st| st.success())
            .unwrap_or(false)
    })?;

    let c17_src = create_c_file(&format!("{name}_c17"), c17_unit);
    let host_src = create_c_file(&format!("{name}_host"), host_unit);
    let asm = plib::tmp::Builder::new()
        .prefix(&format!("c17_hostcc_{name}_"))
        .suffix(".s")
        .tempfile()
        .expect("failed to create temp file");
    let asm_path = asm.path().to_string_lossy().to_string();
    // A plain path, not a `NamedTempFile`: the handle a temp file keeps open
    // makes the linked binary "Text file busy" when it is executed. This is
    // the same reason `compile_and_run_two_units` builds its path by hand.
    let thread_id = format!("{:?}", std::thread::current().id());
    let exe_path = std::env::temp_dir()
        .join(format!(
            "c17_hostcc_{name}_{}",
            thread_id.replace(|c: char| !c.is_alphanumeric(), "_")
        ))
        .to_string_lossy()
        .to_string();

    let run = run_c17(&[
        "-O0",
        "-S",
        "-o",
        &asm_path,
        &c17_src.path().to_string_lossy(),
    ]);
    assert!(
        run.success,
        "c17 failed to compile the {name} unit:\n{}",
        run.stderr
    );

    let linked = Command::new(host_cc)
        .arg("-w")
        .arg("-o")
        .arg(&exe_path)
        .arg(&asm_path)
        .arg(host_src.path())
        .output()
        .expect("failed to run the host C compiler");
    assert!(
        linked.status.success(),
        "linking {name} with {host_cc} failed:\n{}",
        String::from_utf8_lossy(&linked.stderr)
    );

    let status = Command::new(&exe_path)
        .output()
        .expect("failed to run the linked program")
        .status
        .code()
        .unwrap_or(-1);
    let _ = std::fs::remove_file(&exe_path);
    Some(status)
}

/// Whether an aarch64 program built here can actually be run.
///
/// Needs a cross assembler/linker and a user-mode emulator. Both are present
/// on the development machine; CI may not have them, so every caller skips
/// rather than fails when this is false -- and says so, because a silent skip
/// is how an unverified backend looks exactly like a verified one.
pub fn aarch64_cross_available() -> bool {
    ["aarch64-linux-gnu-gcc", "qemu-aarch64-static"]
        .iter()
        .all(|tool| {
            Command::new("sh")
                .arg("-c")
                .arg(format!("command -v {tool} >/dev/null 2>&1"))
                .status()
                .map(|st| st.success())
                .unwrap_or(false)
        })
}

/// The c17 options that compile for linux-aarch64 against the target's own C
/// library headers, which the cross toolchain installs outside any sysroot.
/// Without the `-isystem`, a system header comes from the host's glibc, whose
/// `<bits/...>` headers live in the host's multiarch directory and are not
/// found for aarch64. `cc/scripts/c17_torture.sh -t aarch64` passes the same.
pub const AARCH64_TARGET_ARGS: [&str; 4] = [
    "--target",
    "aarch64-unknown-linux-gnu",
    "-isystem",
    "/usr/aarch64-linux-gnu/include",
];

/// Compile `content` for linux-aarch64 with c17, assemble and link it with the
/// cross toolchain, and run it under qemu. Returns the exit status, or `None`
/// when the cross toolchain is absent.
///
/// This exists because `compile_and_run` always targets the host, so an
/// aarch64-only defect cannot fail a test here at all -- it fails in CI, or
/// not at all if CI is x86-64 too. Two ABI bugs were found this way that no
/// host test and no assembly assertion could have caught: c17's aarch64 caller
/// and callee both charged a zero-sized argument a register, so they agreed
/// with each other while disagreeing with gcc. Only running c17 code against
/// gcc-compiled code shows that, which `cross_link_with` below is for.
pub fn compile_and_run_aarch64(name: &str, content: &str, opt: &str) -> Option<i32> {
    compile_and_run_aarch64_with(name, content, &[opt], &[])
}

/// [`compile_and_run_aarch64`] with any number of c17 options, and `libs`
/// (`-lm`) given to the link after the program.
pub fn compile_and_run_aarch64_with(
    name: &str,
    content: &str,
    opts: &[&str],
    libs: &[&str],
) -> Option<i32> {
    compile_and_capture_aarch64(name, content, opts, libs)
        .map(|out| out.status.code().unwrap_or(-1))
}

/// [`compile_and_run_aarch64_with`], handing back everything the program
/// did: its status and what it wrote.
pub fn compile_and_capture_aarch64(
    name: &str,
    content: &str,
    opts: &[&str],
    libs: &[&str],
) -> Option<std::process::Output> {
    if !aarch64_cross_available() {
        eprintln!(
            "SKIP {name}: no aarch64 cross toolchain (aarch64-linux-gnu-gcc, qemu-aarch64-static)"
        );
        return None;
    }
    let c_file = create_c_file(name, content);
    let asm = plib::tmp::Builder::new()
        .prefix(&format!("c17_a64_{name}_"))
        .suffix(".s")
        .tempfile()
        .expect("failed to create temp file");
    let asm_path = asm.path().to_string_lossy().to_string();

    let src = c_file.path().to_string_lossy().to_string();
    let mut args = AARCH64_TARGET_ARGS.to_vec();
    args.extend_from_slice(opts);
    args.extend_from_slice(&["-S", "-o", &asm_path, &src]);
    let run = run_c17(&args);
    assert!(
        run.success,
        "c17 failed to compile {name} for aarch64 with {opts:?}:\n{}",
        run.stderr
    );

    Some(cross_link_and_capture(name, &[&asm_path], libs))
}

/// Assemble/link the given aarch64 sources (`.c` or `.s`) with the cross
/// toolchain and run the result under qemu, returning its exit status.
///
/// Mixing a c17-produced `.s` with a gcc-compiled `.c` is the point: that is
/// the only way to test that c17 agrees with gcc about the ABI rather than
/// merely with itself.
pub fn cross_link_and_run(name: &str, inputs: &[&str]) -> i32 {
    cross_link_and_run_with(name, inputs, &[])
}

/// [`cross_link_and_run`], with `libs` given to the link after the inputs.
pub fn cross_link_and_run_with(name: &str, inputs: &[&str], libs: &[&str]) -> i32 {
    let run = cross_link_and_capture(name, inputs, libs);
    run.status.code().unwrap_or(-1)
}

/// [`cross_link_and_run_with`], handing back everything the program did.
fn cross_link_and_capture(name: &str, inputs: &[&str], libs: &[&str]) -> std::process::Output {
    let exe = plib::tmp::Builder::new()
        .prefix(&format!("c17_a64_{name}_"))
        .suffix(".bin")
        .tempfile()
        .expect("failed to create temp file");
    let exe_path = exe.path().to_string_lossy().to_string();

    let mut link = Command::new("aarch64-linux-gnu-gcc");
    link.arg("-static").arg("-w").arg("-o").arg(&exe_path);
    for input in inputs {
        link.arg(input);
    }
    link.args(libs);
    let linked = link.output().expect("failed to run the cross linker");
    assert!(
        linked.status.success(),
        "cross link of {name} failed:\n{}",
        String::from_utf8_lossy(&linked.stderr)
    );

    Command::new("qemu-aarch64-static")
        .env("QEMU_LD_PREFIX", "/usr/aarch64-linux-gnu")
        .arg(&exe_path)
        .output()
        .expect("failed to run qemu-aarch64-static")
}

/// The host's own C compiler, the other half of every host interop test:
/// Apple clang on macOS, gcc elsewhere.
pub const HOST_CC: &str = if cfg!(target_os = "macos") {
    "cc"
} else {
    "gcc"
};

/// Compile `src` with c17 to a host object in `dir`, returning its path.
pub fn c17_object(name: &str, src: &str, opt: &str, dir: &std::path::Path) -> String {
    let c = create_c_file(name, src);
    let o = dir.join(format!("{name}.o"));
    let run = run_c17(&[
        opt,
        "-w",
        "-c",
        "-o",
        o.to_str().unwrap(),
        c.path().to_str().unwrap(),
    ]);
    assert!(run.success, "c17 failed on {name}:\n{}", run.stderr);
    o.to_string_lossy().into_owned()
}

/// Link `objs` with the C sources `c_srcs` using [`HOST_CC`] at `opt`, and run
/// it.
///
/// `opt` is the platform compiler's own optimization level, and it matters to
/// what the test can see. At `-O0` gcc and clang re-extend a narrow integer
/// argument on entry, which hides a caller that never extended it; at `-O1`
/// and above `int take(signed char)` is a bare `movl %edi, %eax` that reads
/// whatever the caller left above the byte. Building this side unoptimized is
/// what let a wrong narrowing cross every one of these tests untouched.
pub fn host_link_and_run(
    name: &str,
    objs: &[&str],
    c_srcs: &[&str],
    opt: &str,
    dir: &std::path::Path,
) -> i32 {
    let exe = dir.join(name);
    let files: Vec<_> = c_srcs.iter().map(|s| create_c_file(name, s)).collect();
    let mut cmd = Command::new(HOST_CC);
    cmd.arg("-w").arg(opt).arg("-o").arg(&exe);
    for f in &files {
        cmd.arg(f.path());
    }
    cmd.args(objs);
    let out = cmd.output().expect("run the host C compiler");
    assert!(
        out.status.success(),
        "{HOST_CC} link of {name} failed:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    Command::new(&exe)
        .status()
        .expect("run test binary")
        .code()
        .unwrap_or(-1)
}

/// Every pairing of c17 and [`HOST_CC`] on the host -- c17 on both sides,
/// then each compiler as the caller of the other -- at -O0 and -O2.
///
/// `callee` and `caller` are separate translation units, so each side reads
/// what the other wrote only through the calling convention.
pub fn interop_host(tag: &str, callee: &str, caller: &str) {
    let dir = plib::tmp::Builder::new()
        .prefix(&format!("{tag}_"))
        .tempdir()
        .unwrap();
    for opt in ["-O0", "-O2"] {
        let callee_o = c17_object(&format!("{tag}_callee"), callee, opt, dir.path());
        let caller_o = c17_object(&format!("{tag}_caller"), caller, opt, dir.path());
        assert_eq!(
            host_link_and_run("cc", &[&caller_o, &callee_o], &[], opt, dir.path()),
            0,
            "c17 both, {opt}"
        );
        assert_eq!(
            host_link_and_run("gc", &[&callee_o], &[caller], opt, dir.path()),
            0,
            "{HOST_CC} caller, c17 callee, {opt}"
        );
        assert_eq!(
            host_link_and_run("cg", &[&caller_o], &[callee], opt, dir.path()),
            0,
            "c17 caller, {HOST_CC} callee, {opt}"
        );
    }
}

/// Every pairing of c17 and aarch64 `gcc`, under qemu, at -O0 and -O2: the
/// linux-aarch64 counterpart of [`interop_host`]. The caller checks
/// [`aarch64_cross_available`] first.
pub fn interop_aarch64(tag: &str, callee: &str, caller: &str) {
    let dir = plib::tmp::Builder::new()
        .prefix(&format!("{tag}_a64_"))
        .tempdir()
        .unwrap();
    let callee_c = create_c_file(&format!("{tag}_callee_a64"), callee);
    let caller_c = create_c_file(&format!("{tag}_caller_a64"), caller);
    let callee_src = callee_c.path().to_string_lossy().into_owned();
    let caller_src = caller_c.path().to_string_lossy().into_owned();
    for opt in ["-O0", "-O2"] {
        let asm = |src: &str, n: &str| {
            let s = dir.path().join(format!("{n}{opt}.s"));
            let mut args = AARCH64_TARGET_ARGS.to_vec();
            args.extend_from_slice(&[opt, "-w", "-S", "-o", s.to_str().unwrap(), src]);
            let run = run_c17(&args);
            assert!(run.success, "c17 failed on {n}:\n{}", run.stderr);
            s.to_string_lossy().into_owned()
        };
        let callee_s = asm(&callee_src, "callee");
        let caller_s = asm(&caller_src, "caller");
        assert_eq!(
            cross_link_and_run(&format!("{tag}_cc"), &[&caller_s, &callee_s]),
            0,
            "c17 both, {opt}"
        );
        assert_eq!(
            cross_link_and_run(&format!("{tag}_gc"), &[&caller_src, &callee_s]),
            0,
            "gcc caller, c17 callee, {opt}"
        );
        assert_eq!(
            cross_link_and_run(&format!("{tag}_cg"), &[&caller_s, &callee_src]),
            0,
            "c17 caller, gcc callee, {opt}"
        );
    }
}

/// Compile `content` and require it to be **rejected** with a diagnostic
/// containing `expected`.
///
/// Nothing could express this before: `compile_and_run` collapses a compile
/// failure into the sentinel `-1`, indistinguishable from a program that exits
/// `-1`, and discards stderr to the test log. So the suites proved that
/// accepted programs run correctly, never that invalid programs are diagnosed
/// — which is how a dozen missing constraint checks went unnoticed.
///
/// Compiles with `-S` to a scratch file: that path runs the whole front end and
/// passes both error checkpoints (after parsing and after linearization), while
/// `--dump-ast` returns before linearization and would miss anything the
/// linearizer diagnoses. There is no `-fsyntax-only`.
pub fn compile_expect_error(name: &str, content: &str, expected: &str) {
    let stderr = compile_rejected(name, content);
    assert!(
        stderr.contains(expected),
        "'{}' was rejected, but no diagnostic mentioned {:?}.\nstderr:\n{}",
        name,
        expected,
        stderr
    );
}

/// Compile `content`, require it to be rejected, and return what was written
/// to stderr -- for a test that must see *every* diagnostic, such as one
/// proving that an error is not followed by a cascade of others.
pub fn compile_rejected(name: &str, content: &str) -> String {
    compile_rejected_with(name, content, &[])
}

/// [`compile_rejected`] with extra driver flags, such as an `-O` level for a
/// diagnostic that depends on what the optimizer leaves behind.
pub fn compile_rejected_with(name: &str, content: &str, extra: &[&str]) -> String {
    let c_file = create_c_file(name, content);
    let asm = plib::tmp::Builder::new()
        .prefix(&format!("c17_reject_{}_", name))
        .suffix(".s")
        .tempfile()
        .expect("failed to create temp file");

    let mut args: Vec<String> = extra.iter().map(|s| s.to_string()).collect();
    args.extend([
        "-S".to_string(),
        "-o".to_string(),
        asm.path().to_string_lossy().to_string(),
        c_file.path().to_string_lossy().to_string(),
    ]);
    let output = run_test_base("c17", &args, &[]);
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();

    assert!(
        !output.status.success(),
        "'{}' should have been rejected but compiled cleanly.\nSource:\n{}\nstderr:\n{}",
        name,
        content,
        stderr
    );
    stderr
}

/// Compile `content` and require it to be **accepted**.
///
/// The companion to `compile_expect_error`: every new constraint check needs a
/// case proving it does not fire on legal code.
pub fn compile_expect_ok(name: &str, content: &str) {
    let c_file = create_c_file(name, content);
    let asm = plib::tmp::Builder::new()
        .prefix(&format!("c17_accept_{}_", name))
        .suffix(".s")
        .tempfile()
        .expect("failed to create temp file");

    let args = vec![
        "-S".to_string(),
        "-o".to_string(),
        asm.path().to_string_lossy().to_string(),
        c_file.path().to_string_lossy().to_string(),
    ];
    let output = run_test_base("c17", &args, &[]);
    assert!(
        output.status.success(),
        "'{}' should have compiled, but was rejected.\nSource:\n{}\nstderr:\n{}",
        name,
        content,
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Compile `content` and require it to be **accepted with a diagnostic**.
///
/// C17 5.1.1.3 asks only for a diagnostic message, not a failure, so a
/// constraint gcc warns about is a warning here too. Neither
/// `compile_expect_error` nor `compile_expect_ok` can express that: the first
/// demands a non-zero exit, the second says nothing about stderr.
pub fn compile_expect_warning(name: &str, content: &str, expected: &str) {
    compile_expect_warning_named(name, content, expected, &[]);
}

fn compile_expect_warning_named(
    name: &str,
    content: &str,
    expected: &str,
    extra: &[String],
) -> String {
    let c_file = create_c_file(name, content);
    let asm = plib::tmp::Builder::new()
        .prefix(&format!("c17_warn_{}_", name))
        .suffix(".s")
        .tempfile()
        .expect("failed to create temp file");

    let mut args = extra.to_vec();
    args.extend([
        "-S".to_string(),
        "-o".to_string(),
        asm.path().to_string_lossy().to_string(),
        c_file.path().to_string_lossy().to_string(),
    ]);
    let output = run_test_base("c17", &args, &[]);
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert!(
        output.status.success(),
        "'{}' should have compiled with a warning, but was rejected.\nSource:\n{}\nstderr:\n{}",
        name,
        content,
        stderr
    );
    assert!(
        stderr.contains(expected),
        "'{}' compiled, but no diagnostic mentioned {:?}.\nstderr:\n{}",
        name,
        expected,
        stderr
    );
    stderr
}

/// Compile `content` and require it to be accepted **without** a diagnostic
/// mentioning `forbidden`.
///
/// The inverse of `compile_expect_warning`, and the only way to pin a
/// *spurious* diagnostic: `compile_expect_ok` checks the exit status, which a
/// warning does not change, so a wrongly-warned program passes it.
pub fn compile_expect_no_diagnostic(name: &str, content: &str, forbidden: &str) {
    let c_file = create_c_file(name, content);
    let asm = plib::tmp::Builder::new()
        .prefix(&format!("c17_nodiag_{}_", name))
        .suffix(".s")
        .tempfile()
        .expect("failed to create temp file");

    let args = vec![
        "-S".to_string(),
        "-o".to_string(),
        asm.path().to_string_lossy().to_string(),
        c_file.path().to_string_lossy().to_string(),
    ];
    let output = run_test_base("c17", &args, &[]);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "'{}' should have compiled cleanly, but was rejected.\nSource:\n{}\nstderr:\n{}",
        name,
        content,
        stderr
    );
    assert!(
        !stderr.contains(forbidden),
        "'{}' compiled, but emitted a diagnostic mentioning {:?}.\nstderr:\n{}",
        name,
        forbidden,
        stderr
    );
}

/// Preprocess `content` with `-E` and return the run.
///
/// Every other preprocessor test asserts on the exit code of a compiled
/// program, which cannot see spacing, stringification, or which branch of a
/// `#if` survived. This looks at the text.
pub fn preprocess_text(name: &str, content: &str, extra_opts: &[&str]) -> C17Run {
    let c_file = create_c_file(name, content);
    let mut args = vec!["-E".to_string()];
    args.extend(extra_opts.iter().map(|s| s.to_string()));
    args.push(c_file.path().to_string_lossy().to_string());

    let output = run_test_base("c17", &args, &[]);
    C17Run {
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        success: output.status.success(),
    }
}

/// Print why a test program produced no exit code.
///
/// A program killed by a signal has `code() == None`, which the callers turn
/// into `-1` -- the same value a failed build returns. Reporting the signal,
/// and whatever the program managed to write first, is what tells a segfault
/// apart from a compiler error when the target cannot be run locally.
fn report_abnormal_exit(name: &str, config_name: &str, out: &std::process::Output) {
    if out.status.code().is_some() {
        return;
    }
    #[cfg(unix)]
    let detail = {
        use std::os::unix::process::ExitStatusExt;
        match out.status.signal() {
            Some(sig) => format!("killed by signal {sig}"),
            None => "terminated without an exit code".to_string(),
        }
    };
    #[cfg(not(unix))]
    let detail = "terminated without an exit code".to_string();

    eprintln!(
        "c17 test {name} [config: {config_name}] {detail}\n\
         --- program stdout ---\n{}\n--- program stderr ---\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
}

/// Compile `src` to assembly with `extra` options and return the text.
///
/// The default is `-O0`, so a test about an optimizer decision has to name
/// the level it is asking about.
pub fn asm_for_at(prefix: &str, src: &str, extra: &[&str]) -> String {
    let dir = plib::tmp::Builder::new()
        .prefix(prefix)
        .tempdir()
        .expect("tempdir");
    let c = dir.path().join("t.c");
    let s = dir.path().join("t.s");
    std::fs::write(&c, src).expect("write source");
    let mut args = vec!["-S"];
    args.extend_from_slice(extra);
    args.extend_from_slice(&[c.to_str().unwrap(), "-o", s.to_str().unwrap()]);
    let out = run_c17(&args);
    assert!(out.success, "compile failed: {}", out.stderr);
    std::fs::read_to_string(&s).expect("read asm")
}

/// The symbol prefix `asm` uses, read off a symbol it is known to define.
///
/// Mach-O spells every C identifier with a leading underscore. Reading the
/// prefix off the output is right both for a test that names a `--target`
/// and for one compiled for the host; `cfg!(target_os)` is wrong for the
/// first and a hardcoded `""` is wrong for the second. The
/// `-fgnu89-inline` test has now been wrong in both directions, each time
/// passing on Linux and failing only on macOS CI.
pub fn asm_prefix(asm: &str, defined: &str) -> &'static str {
    let mangled = format!("_{defined}:");
    if asm.lines().any(|l| l.trim_start() == mangled) {
        "_"
    } else {
        ""
    }
}

/// `name` as the assembler spells it on this host.
///
/// Mach-O prefixes every C identifier with an underscore, so a test that
/// looks for a label or a call by its C name finds nothing on macOS -- and
/// usually finds nothing in the *negative* direction either, so it passes
/// vacuously and reports a failure only on the platform it was never run on.
///
/// Only for a test that compiles for the *host*. One that names its target
/// should spell the prefix from the target, and is better off naming both
/// formats so the Mach-O shape is exercised on every run.
pub fn asm_symbol(name: &str) -> String {
    if cfg!(target_os = "macos") {
        format!("_{name}")
    } else {
        name.to_string()
    }
}
