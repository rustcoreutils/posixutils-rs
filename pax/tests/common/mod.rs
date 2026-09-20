//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Common test helpers for pax integration tests

use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use std::process::{Command, Output};

/// Run pax with given arguments and return output
pub fn run_pax(args: &[&str]) -> Output {
    run_pax_with_stdin(args, None)
}

/// Run pax with given arguments and optional stdin, return output
pub fn run_pax_with_stdin(args: &[&str], stdin_data: Option<&str>) -> Output {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_pax"));
    cmd.args(args);

    if let Some(data) = stdin_data {
        use std::process::Stdio;
        cmd.stdin(Stdio::piped());
        let mut child = cmd.spawn().expect("Failed to spawn pax");
        if let Some(ref mut stdin) = child.stdin {
            stdin
                .write_all(data.as_bytes())
                .expect("Failed to write stdin");
        }
        child.wait_with_output().expect("Failed to wait for pax")
    } else {
        cmd.output().expect("Failed to run pax")
    }
}

/// Run pax with extra environment variables set.
///
/// pax calls `tzset()` at startup so that `$TZ` selects the zone its `-v` and
/// `listopt` times are rendered in; a test that pins an hour has to pin the
/// zone too, or it only passes where local time happens to equal the one the
/// fixture was written in.
pub fn run_pax_with_env(args: &[&str], vars: &[(&str, &str)]) -> Output {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_pax"));
    cmd.args(args);
    for (name, value) in vars {
        cmd.env(name, value);
    }
    cmd.output().expect("Failed to run pax")
}

/// Run pax with given arguments and binary stdin data, return output
pub fn run_pax_with_stdin_bytes(args: &[&str], stdin_data: &[u8]) -> Output {
    use std::process::Stdio;
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_pax"));
    cmd.args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = cmd.spawn().expect("Failed to spawn pax");
    if let Some(ref mut stdin) = child.stdin {
        stdin.write_all(stdin_data).expect("Failed to write stdin");
    }

    child.wait_with_output().expect("Failed to wait for pax")
}

/// Run pax with raw stdin bytes, in a specific directory. Extraction targets
/// the working directory, so a test that feeds a hand-built archive to `-r`
/// needs both at once.
pub fn run_pax_with_stdin_bytes_in_dir(args: &[&str], stdin_data: &[u8], dir: &Path) -> Output {
    use std::process::Stdio;
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_pax"));
    cmd.args(args)
        .current_dir(dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = cmd.spawn().expect("Failed to spawn pax");
    if let Some(ref mut stdin) = child.stdin {
        stdin.write_all(stdin_data).expect("Failed to write stdin");
    }

    child.wait_with_output().expect("Failed to wait for pax")
}

/// Run pax with given arguments in a specific directory
pub fn run_pax_in_dir(args: &[&str], dir: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(args)
        .current_dir(dir)
        .output()
        .expect("Failed to run pax")
}

/// Run pax with stdin input in a specific directory
pub fn run_pax_in_dir_with_stdin(args: &[&str], dir: &Path, stdin_data: &str) -> Output {
    use std::process::Stdio;
    let mut child = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(args)
        .current_dir(dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn pax");

    if let Some(ref mut stdin) = child.stdin {
        stdin
            .write_all(stdin_data.as_bytes())
            .expect("Failed to write stdin");
    }

    child.wait_with_output().expect("Failed to wait for pax")
}

/// Path of a compatibility front-end (`tar` or `cpio`).
///
/// Both are symlinks to the pax binary created by `pax/build.rs`, and the
/// binary picks its command-line parser from argv[0], so they live beside the
/// pax executable cargo built for this test run.
pub fn front_end(name: &str) -> std::path::PathBuf {
    let path = Path::new(env!("CARGO_BIN_EXE_pax")).with_file_name(name);
    assert!(
        path.exists(),
        "{} is missing; pax/build.rs should have symlinked it to pax",
        path.display()
    );
    path
}

/// Run `tar` or `cpio` in `dir`, feeding `stdin_data` if given.
pub fn run_front_end(name: &str, args: &[&str], dir: &Path, stdin_data: Option<&[u8]>) -> Output {
    use std::process::Stdio;
    let mut child = Command::new(front_end(name))
        .args(args)
        .current_dir(dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("failed to spawn {}: {}", name, e));

    {
        // Dropping stdin closes it, so a child reading a name list from a pipe
        // sees EOF instead of blocking forever.
        let mut stdin = child.stdin.take().expect("stdin was piped");
        if let Some(data) = stdin_data {
            match stdin.write_all(data) {
                Ok(()) => {}
                // A front-end that rejects its command line exits before it
                // ever reads the name list, which closes the read end of this
                // pipe. That is the behavior under test, so losing the write is
                // the expected outcome, not a harness failure. Whether the
                // write lands at all is a race the child usually loses on
                // macOS and usually wins on Linux.
                Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => {}
                Err(e) => panic!("failed to write stdin to {}: {}", name, e),
            }
        }
    }

    child
        .wait_with_output()
        .unwrap_or_else(|e| panic!("failed to wait for {}: {}", name, e))
}

/// Run `tar` in `dir`.
pub fn run_tar(args: &[&str], dir: &Path) -> Output {
    run_front_end("tar", args, dir, None)
}

/// Run `cpio` in `dir` with `stdin_data` on standard input.
pub fn run_cpio(args: &[&str], dir: &Path, stdin_data: &[u8]) -> Output {
    run_front_end("cpio", args, dir, Some(stdin_data))
}

/// Whether a system tool of this name can be run, for the cross-tool checks.
pub fn have_tool(name: &str) -> bool {
    Command::new(name)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Create a test directory with standard test files
pub fn create_test_files(dir: &Path) {
    // Create regular file
    let file_path = dir.join("file.txt");
    let mut f = File::create(&file_path).unwrap();
    writeln!(f, "Hello, world!").unwrap();

    // Create subdirectory
    let subdir = dir.join("subdir");
    fs::create_dir(&subdir).unwrap();

    // Create file in subdirectory
    let subfile = subdir.join("nested.txt");
    let mut f = File::create(&subfile).unwrap();
    writeln!(f, "Nested file content").unwrap();

    // Create symlink (Unix only)
    #[cfg(unix)]
    {
        let link_path = dir.join("link.txt");
        std::os::unix::fs::symlink("file.txt", &link_path).unwrap();
    }
}

/// Verify extracted files match original test files
pub fn verify_files_match(original: &Path, extracted: &Path) {
    // Check file.txt
    let orig_content = fs::read_to_string(original.join("file.txt")).unwrap();
    let extr_content = fs::read_to_string(extracted.join("file.txt")).unwrap();
    assert_eq!(orig_content, extr_content, "file.txt content mismatch");

    // Check subdir/nested.txt
    let orig_nested = fs::read_to_string(original.join("subdir/nested.txt")).unwrap();
    let extr_nested = fs::read_to_string(extracted.join("subdir/nested.txt")).unwrap();
    assert_eq!(orig_nested, extr_nested, "nested.txt content mismatch");

    // Check symlink (Unix only)
    #[cfg(unix)]
    {
        let orig_link = fs::read_link(original.join("link.txt")).unwrap();
        let extr_link = fs::read_link(extracted.join("link.txt")).unwrap();
        assert_eq!(orig_link, extr_link, "symlink target mismatch");
    }
}

/// Assert command succeeded
pub fn assert_success(output: &Output, context: &str) {
    assert!(
        output.status.success(),
        "{} failed with status {:?}\nstderr: {}",
        context,
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Assert command failed
pub fn assert_failure(output: &Output, context: &str) {
    assert!(
        !output.status.success(),
        "{} should have failed but succeeded\nstdout: {}",
        context,
        String::from_utf8_lossy(&output.stdout)
    );
}

/// Assert the command exited with exactly `code`. `assert_failure` only
/// distinguishes zero from non-zero; POSIX pins specific statuses, and a
/// process killed by a signal (a panic reaching abort) has no code at all --
/// which this reports as such rather than as a plain inequality.
pub fn assert_exit_code(output: &Output, code: i32, context: &str) {
    match output.status.code() {
        Some(actual) => assert_eq!(
            actual,
            code,
            "{} exited {} (wanted {})\nstderr: {}",
            context,
            actual,
            code,
            String::from_utf8_lossy(&output.stderr)
        ),
        None => panic!(
            "{} was killed by a signal rather than exiting {}\nstderr: {}",
            context,
            code,
            String::from_utf8_lossy(&output.stderr)
        ),
    }
}

// ============================================================================
// Hand-built archives
//
// Several suites need archives pax itself would refuse to write: a member name
// that escapes, a length field that lies, a typeflag with no meaning. These
// build the bytes directly. Names are `&[u8]` rather than `&str` because a
// member name is a byte string -- some fixtures are deliberately not UTF-8.
// ============================================================================

/// A tar block, and the unit every ustar length is rounded to.
pub const BLOCK: usize = 512;

/// The fields of one ustar member, for a test that needs to build it by hand.
///
/// `Default` gives a plain 0644 regular file, so a fixture names only what it
/// is actually testing.
pub struct Ustar<'a> {
    pub name: &'a [u8],
    pub typeflag: u8,
    pub linkname: &'a [u8],
    pub mode: u32,
    pub body: &'a [u8],
    /// What to write in the size field. `None` writes `body.len()`, which is
    /// what a well-formed member has; `Some` is how a fixture makes the header
    /// lie about how much data follows.
    pub size: Option<u64>,
}

impl Default for Ustar<'_> {
    fn default() -> Self {
        Ustar {
            name: b"",
            typeflag: b'0',
            linkname: b"",
            mode: 0o644,
            body: b"",
            size: None,
        }
    }
}

impl Ustar<'_> {
    /// The 512-byte header block, with a correct checksum over whatever the
    /// other fields ended up as.
    pub fn header(&self) -> [u8; BLOCK] {
        let mut h = [0u8; BLOCK];
        h[..self.name.len()].copy_from_slice(self.name);
        h[100..108].copy_from_slice(format!("{:07o}\0", self.mode).as_bytes());
        h[108..116].copy_from_slice(b"0000000\0"); // uid
        h[116..124].copy_from_slice(b"0000000\0"); // gid
        let size = self.size.unwrap_or(self.body.len() as u64);
        h[124..136].copy_from_slice(format!("{:011o}\0", size).as_bytes());
        h[136..148].copy_from_slice(b"00000000000\0"); // mtime
        h[148..156].copy_from_slice(b"        "); // spaces while summing
        h[156] = self.typeflag;
        h[157..157 + self.linkname.len()].copy_from_slice(self.linkname);
        h[257..263].copy_from_slice(b"ustar\0");
        h[263..265].copy_from_slice(b"00");

        let sum: u32 = h.iter().map(|&b| b as u32).sum();
        h[148..156].copy_from_slice(format!("{:06o}\0 ", sum).as_bytes());
        h
    }

    /// Header plus block-padded body, and no trailer -- so members concatenate.
    pub fn member(&self) -> Vec<u8> {
        let mut out = self.header().to_vec();
        if !self.body.is_empty() {
            let mut data = self.body.to_vec();
            pad_to_block(&mut data);
            out.extend_from_slice(&data);
        }
        out
    }

    /// A complete one-member archive.
    pub fn archive(&self) -> Vec<u8> {
        let mut out = self.member();
        out.extend_from_slice(&ustar_trailer());
        out
    }
}

/// The end-of-archive indicator: two 512-byte blocks of zeros (POSIX).
pub fn ustar_trailer() -> [u8; 2 * BLOCK] {
    [0u8; 2 * BLOCK]
}

/// Zero-fill `data` out to a block boundary.
pub fn pad_to_block(data: &mut Vec<u8>) {
    let rem = data.len() % BLOCK;
    if rem != 0 {
        data.resize(data.len() + (BLOCK - rem), 0);
    }
}

/// One pax extended-header record: `"%d keyword=value\n"`, where the length
/// counts itself.
///
/// That self-reference is why this exists: writing the length by hand gets it
/// wrong by one as soon as the record crosses a power of ten, and a fixture
/// that means to be well-formed has to actually be well-formed or it tests the
/// error path by accident.
pub fn pax_record(keyword: &str, value: &[u8]) -> Vec<u8> {
    let mut body = Vec::new();
    body.push(b' ');
    body.extend_from_slice(keyword.as_bytes());
    body.push(b'=');
    body.extend_from_slice(value);
    body.push(b'\n');

    let mut len = body.len() + 1;
    loop {
        let digits = len.to_string().len();
        if digits + body.len() == len {
            break;
        }
        len = digits + body.len();
    }

    let mut out = len.to_string().into_bytes();
    out.extend_from_slice(&body);
    out
}

/// A pax archive whose single member is preceded by an `x` extended header
/// carrying exactly `records` as its data.
///
/// `records` is the raw record stream, so a fixture can concatenate several
/// records -- which is the only way to reach the parser's behaviour at a
/// non-zero offset into the block.
pub fn archive_with_ext_records(records: &[u8]) -> Vec<u8> {
    let mut a = Ustar {
        name: b"PaxHeaders/f",
        typeflag: b'x',
        body: records,
        ..Default::default()
    }
    .member();
    a.extend_from_slice(
        &Ustar {
            name: b"f",
            ..Default::default()
        }
        .member(),
    );
    a.extend_from_slice(&ustar_trailer());
    a
}

/// The fields of one cpio "newc" member, for a test that needs to build it by
/// hand. `Default` gives a plain 0644 regular file.
pub struct CpioNewc<'a> {
    pub name: &'a [u8],
    pub mode: u32,
    pub body: &'a [u8],
    /// `c_namesize`. `None` writes `name.len() + 1` and appends the NUL
    /// terminator; `Some` writes the value given and appends nothing, which is
    /// how a fixture makes the field disagree with the name that follows.
    pub namesize: Option<u32>,
    /// `c_filesize`. `None` writes `body.len()`.
    pub filesize: Option<u32>,
}

impl Default for CpioNewc<'_> {
    fn default() -> Self {
        CpioNewc {
            name: b"",
            mode: 0o100644,
            body: b"",
            namesize: None,
            filesize: None,
        }
    }
}

impl CpioNewc<'_> {
    /// Header, name and body, each padded to the 4-byte alignment newc uses.
    pub fn member(&self) -> Vec<u8> {
        let mut out = b"070701".to_vec();
        for v in [
            1u32,                                                // c_ino
            self.mode,                                           // c_mode
            0,                                                   // c_uid
            0,                                                   // c_gid
            1,                                                   // c_nlink
            0,                                                   // c_mtime
            self.filesize.unwrap_or(self.body.len() as u32),     // c_filesize
            0,                                                   // c_devmajor
            0,                                                   // c_devminor
            0,                                                   // c_rdevmajor
            0,                                                   // c_rdevminor
            self.namesize.unwrap_or(self.name.len() as u32 + 1), // c_namesize
            0,                                                   // c_check
        ] {
            out.extend_from_slice(format!("{v:08X}").as_bytes());
        }
        out.extend_from_slice(self.name);
        if self.namesize.is_none() {
            out.push(0);
        }
        while out.len() % 4 != 0 {
            out.push(0);
        }
        out.extend_from_slice(self.body);
        while out.len() % 4 != 0 {
            out.push(0);
        }
        out
    }
}

/// Get stdout as string
pub fn stdout_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).to_string()
}

/// Get stderr as string
pub fn stderr_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).to_string()
}
