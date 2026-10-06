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
use std::time::{Duration, Instant};

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
    run_program(&front_end(name), args, dir, stdin_data)
}

/// Run `program` in `dir` with its standard input fed from `stdin_data` (or
/// empty), collecting its output.
pub fn run_program(program: &Path, args: &[&str], dir: &Path, stdin_data: Option<&[u8]>) -> Output {
    use std::process::Stdio;
    let name = program.display();
    let mut child = Command::new(program)
        .args(args)
        .current_dir(dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("failed to spawn {}: {}", name, e));

    // Fed from a thread of its own: a child that writes its output while
    // still reading -- cpio archiving a long name list -- would otherwise fill
    // its stdout pipe while this side is still blocked filling its stdin.
    // Dropping stdin closes it, so a child reading a name list from a pipe
    // sees EOF instead of blocking forever.
    let mut stdin = child.stdin.take().expect("stdin was piped");
    let data = stdin_data.map(<[u8]>::to_vec);
    let feeder = std::thread::spawn(move || match data {
        Some(data) => match stdin.write_all(&data) {
            Ok(()) => Ok(()),
            // A front-end that rejects its command line exits before it ever
            // reads the name list, which closes the read end of this pipe.
            // That is the behavior under test, so losing the write is the
            // expected outcome, not a harness failure. Whether the write lands
            // at all is a race the child usually loses on macOS and usually
            // wins on Linux.
            Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => Ok(()),
            Err(e) => Err(e),
        },
        None => Ok(()),
    });

    let output = child
        .wait_with_output()
        .unwrap_or_else(|e| panic!("failed to wait for {}: {}", name, e));
    if let Err(e) = feeder.join().expect("stdin feeder panicked") {
        panic!("failed to write stdin to {}: {}", name, e);
    }
    output
}

/// Whether `program` writes its first `record` bytes of output while its
/// standard input is still open, after being fed only `names`.
///
/// For the name-list readers, which must archive each name as it arrives
/// rather than wait for the producer to finish. Gives up after five seconds,
/// and kills the child either way.
pub fn writes_before_list_ends(
    program: &Path,
    args: &[&str],
    dir: &Path,
    names: &[u8],
    record: usize,
) -> bool {
    use std::io::Read;
    use std::process::Stdio;
    let mut child = Command::new(program)
        .args(args)
        .current_dir(dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();
    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(names).unwrap();
    stdin.flush().unwrap();

    let mut stdout = child.stdout.take().unwrap();
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut buf = vec![0u8; record];
        let _ = tx.send(stdout.read_exact(&mut buf).is_ok());
    });
    let got = rx.recv_timeout(Duration::from_secs(5));

    drop(stdin);
    let _ = child.kill();
    let _ = child.wait();
    got == Ok(true)
}

/// Run `tar` in `dir`.
pub fn run_tar(args: &[&str], dir: &Path) -> Output {
    run_front_end("tar", args, dir, None)
}

/// Run `cpio` in `dir` with `stdin_data` on standard input.
pub fn run_cpio(args: &[&str], dir: &Path, stdin_data: &[u8]) -> Output {
    run_front_end("cpio", args, dir, Some(stdin_data))
}

/// The system's own `name` -- the first executable of that name on `$PATH` --
/// for the cross-tool checks, or `None` when there is none.
///
/// Absence is the only reason a cross-tool test may skip. Once the tool
/// exists, anything it does wrong is the test's failure: a check that also
/// skipped when the tool failed passed whatever pax wrote. Found by looking,
/// not by running `name --version`, which BSD pax and some cpio do not
/// accept.
pub fn system_tool(name: &str) -> Option<std::path::PathBuf> {
    use std::os::unix::fs::PermissionsExt;
    let found = std::env::var_os("PATH").and_then(|path| {
        std::env::split_paths(&path)
            .map(|dir| dir.join(name))
            .find(|p| {
                fs::metadata(p).is_ok_and(|m| m.is_file() && m.permissions().mode() & 0o111 != 0)
            })
    });
    if found.is_none() {
        eprintln!("skipping cross-tool check: no system {}", name);
    }
    found
}

/// Run the system `tool` and require it to succeed.
pub fn run_system_ok(tool: &Path, args: &[&str], dir: &Path, stdin_data: Option<&[u8]>) -> Output {
    let out = run_program(tool, args, dir, stdin_data);
    assert!(
        out.status.success(),
        "system {} {:?} failed: {}",
        tool.display(),
        args,
        String::from_utf8_lossy(&out.stderr)
    );
    out
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
    /// The `prefix` field (offset 345). A member whose pathname needs more
    /// than the 100-byte `name` field carries the leading components here,
    /// joined back with a `/` on read.
    pub prefix: &'a [u8],
    /// `devmajor`/`devminor` (329, 337). Only a block or character special
    /// member uses them, and writing one by hand is how a test reaches the
    /// device keywords without `mknod` and therefore without root.
    pub devmajor: u32,
    pub devminor: u32,
    /// `uid`/`gid` (108, 116) and `uname`/`gname` (265, 297). Settable so a
    /// test can make the names disagree with the numbers, which is the state
    /// an archive carried between hosts arrives in.
    pub uid: u32,
    pub gid: u32,
    pub uname: &'a [u8],
    pub gname: &'a [u8],
    /// What to write in the size field. `None` writes `body.len()`, which is
    /// what a well-formed member has; `Some` is how a fixture makes the header
    /// lie about how much data follows.
    pub size: Option<u64>,
    /// The `mtime` field (136), seconds since the epoch.
    pub mtime: u64,
}

impl Default for Ustar<'_> {
    fn default() -> Self {
        Ustar {
            name: b"",
            typeflag: b'0',
            linkname: b"",
            mode: 0o644,
            body: b"",
            prefix: b"",
            devmajor: 0,
            devminor: 0,
            uid: 0,
            gid: 0,
            uname: b"",
            gname: b"",
            size: None,
            mtime: 0,
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
        h[108..116].copy_from_slice(format!("{:07o}\0", self.uid).as_bytes());
        h[116..124].copy_from_slice(format!("{:07o}\0", self.gid).as_bytes());
        let size = self.size.unwrap_or(self.body.len() as u64);
        h[124..136].copy_from_slice(format!("{:011o}\0", size).as_bytes());
        h[136..148].copy_from_slice(format!("{:011o}\0", self.mtime).as_bytes());
        h[156] = self.typeflag;
        h[157..157 + self.linkname.len()].copy_from_slice(self.linkname);
        h[257..263].copy_from_slice(b"ustar\0");
        h[263..265].copy_from_slice(b"00");
        h[329..337].copy_from_slice(format!("{:07o}\0", self.devmajor).as_bytes());
        h[337..345].copy_from_slice(format!("{:07o}\0", self.devminor).as_bytes());
        h[265..265 + self.uname.len()].copy_from_slice(self.uname);
        h[297..297 + self.gname.len()].copy_from_slice(self.gname);
        h[345..345 + self.prefix.len()].copy_from_slice(self.prefix);
        reseal_header(&mut h);
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

/// Recompute the checksum of the header block that `block` begins with, for a
/// fixture that edits a field after the header was built.
pub fn reseal_header(block: &mut [u8]) {
    block[148..156].copy_from_slice(b"        "); // spaces while summing
    let sum: u32 = block[..BLOCK].iter().map(|&b| b as u32).sum();
    block[148..156].copy_from_slice(format!("{:06o}\0 ", sum).as_bytes());
}

/// Run pax on `args` in `dir`, killing it if it has not finished within
/// `limit`; `None` when it had to be killed. For a test whose regression is a
/// hang or a crawl rather than a wrong answer. Output is collected only once
/// pax exits, so it must fit in a pipe buffer.
pub fn run_pax_with_deadline(args: &[&str], dir: &Path, limit: Duration) -> Option<Output> {
    let mut child = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(args)
        .current_dir(dir)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    let deadline = Instant::now() + limit;
    while child.try_wait().unwrap().is_none() {
        if Instant::now() > deadline {
            child.kill().unwrap();
            child.wait().unwrap();
            return None;
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    Some(child.wait_with_output().unwrap())
}

/// Run pax in `dir` with a fresh pseudo-terminal as its controlling terminal,
/// so that `/dev/tty` is that terminal and `-i` prompts on it. `typed` is
/// queued as terminal input before pax starts. Returns pax's output and
/// everything it wrote to the terminal, or `None` if it had to be killed
/// after `limit` -- a pax that keeps prompting would otherwise hang the test.
pub fn run_pax_on_tty(
    args: &[&str],
    dir: &Path,
    typed: &[u8],
    limit: Duration,
) -> Option<(Output, Vec<u8>)> {
    PtyPax::spawn(args, dir, typed, PtyStdio::StdinOnly).finish(limit)
}

/// `run_pax_on_tty` with standard output and standard error on the terminal
/// as well, the way an interactive user runs pax; their output is then part of
/// the terminal's.
pub fn run_pax_on_terminal(args: &[&str], dir: &Path, limit: Duration) -> Option<Vec<u8>> {
    PtyPax::spawn(args, dir, b"", PtyStdio::All)
        .finish(limit)
        .map(|(_, tty)| tty)
}

/// Which of pax's standard streams are the terminal.
pub enum PtyStdio {
    /// Standard input only; output and errors are piped.
    StdinOnly,
    /// All three.
    All,
    /// Output and errors; standard input is a pipe the test writes to.
    OutputOnly,
}

/// A pax running with a pseudo-terminal as its controlling terminal.
pub struct PtyPax {
    pub child: std::process::Child,
    master: File,
    /// Everything pax has written to the terminal so far.
    pub seen: Vec<u8>,
}

impl PtyPax {
    pub fn spawn(args: &[&str], dir: &Path, typed: &[u8], stdio: PtyStdio) -> PtyPax {
        Self::spawn_program(
            Path::new(env!("CARGO_BIN_EXE_pax")),
            args,
            dir,
            typed,
            stdio,
        )
    }

    /// `spawn`, running `program` (a front-end) in place of pax.
    pub fn spawn_program(
        program: &Path,
        args: &[&str],
        dir: &Path,
        typed: &[u8],
        stdio: PtyStdio,
    ) -> PtyPax {
        use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
        use std::os::unix::process::CommandExt;
        use std::process::Stdio;

        let (mut master, mut slave) = (-1, -1);
        let rc = unsafe {
            libc::openpty(
                &mut master,
                &mut slave,
                std::ptr::null_mut(),
                // `*mut` on macOS, `*const` on Linux; a null `*mut` suits both.
                std::ptr::null_mut(),
                std::ptr::null_mut(),
            )
        };
        assert_eq!(rc, 0, "openpty: {}", std::io::Error::last_os_error());
        // Not inherited by the children other tests spawn meanwhile: a stray
        // copy of the slave would keep the terminal open after pax exits.
        for fd in [master, slave] {
            unsafe { libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC) };
        }
        let mut master = unsafe { File::from_raw_fd(master) };
        let slave = unsafe { OwnedFd::from_raw_fd(slave) };
        master.write_all(typed).unwrap();
        // Drained as pax runs, never with a blocking read: on macOS a read of
        // the master does not return once the slave is closed, it just waits.
        unsafe {
            let flags = libc::fcntl(master.as_raw_fd(), libc::F_GETFL);
            libc::fcntl(master.as_raw_fd(), libc::F_SETFL, flags | libc::O_NONBLOCK);
        }

        // At least one of pax's own descriptors is the slave: macOS stops
        // `/dev/tty` opening once no descriptor for the terminal is left open.
        let tty = || Stdio::from(slave.try_clone().unwrap());
        let (stdin, out, err) = match stdio {
            PtyStdio::StdinOnly => (tty(), Stdio::piped(), Stdio::piped()),
            PtyStdio::All => (tty(), tty(), tty()),
            PtyStdio::OutputOnly => (Stdio::piped(), tty(), tty()),
        };
        let mut cmd = Command::new(program);
        cmd.args(args)
            .current_dir(dir)
            .stdin(stdin)
            .stdout(out)
            .stderr(err);
        let slave_fd = slave.as_raw_fd();
        unsafe {
            cmd.pre_exec(move || {
                // A new session with the slave as its controlling terminal.
                if libc::setsid() < 0 || libc::ioctl(slave_fd, libc::TIOCSCTTY as _, 0) < 0 {
                    return Err(std::io::Error::last_os_error());
                }
                Ok(())
            });
        }
        let child = cmd.spawn().unwrap();
        // The parent's copies of the slave go here, so that pax holds the
        // only ones.
        drop(cmd);
        drop(slave);
        PtyPax {
            child,
            master,
            seen: Vec::new(),
        }
    }

    /// Collect whatever pax has written to the terminal since the last call.
    pub fn drain(&mut self) {
        use std::io::Read;
        let mut buf = [0u8; 1024];
        while let Ok(n) = self.master.read(&mut buf) {
            if n == 0 {
                break;
            }
            self.seen.extend_from_slice(&buf[..n]);
        }
    }

    /// Wait up to `limit` for `needle` to appear on the terminal.
    pub fn wait_for(&mut self, needle: &[u8], limit: Duration) -> bool {
        let deadline = Instant::now() + limit;
        loop {
            self.drain();
            if self.seen.windows(needle.len()).any(|w| w == needle) {
                return true;
            }
            if Instant::now() > deadline {
                return false;
            }
            std::thread::sleep(Duration::from_millis(20));
        }
    }

    /// Wait for pax to exit, killing it after `limit`; `None` if it had to be.
    pub fn finish(mut self, limit: Duration) -> Option<(Output, Vec<u8>)> {
        let deadline = Instant::now() + limit;
        while self.child.try_wait().unwrap().is_none() {
            self.drain();
            if Instant::now() > deadline {
                self.child.kill().unwrap();
                self.child.wait().unwrap();
                return None;
            }
            std::thread::sleep(Duration::from_millis(20));
        }
        self.drain();
        let output = self.child.wait_with_output().unwrap();
        Some((output, self.seen))
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
    /// `c_ino`, `c_nlink` and `c_mtime`. Settable so a test can tell the
    /// fields apart in a listing instead of reading four identical ones.
    pub ino: u32,
    pub nlink: u32,
    pub mtime: u32,
}

impl Default for CpioNewc<'_> {
    fn default() -> Self {
        CpioNewc {
            name: b"",
            mode: 0o100644,
            body: b"",
            namesize: None,
            filesize: None,
            ino: 1,
            nlink: 1,
            mtime: 0,
        }
    }
}

impl CpioNewc<'_> {
    /// Header, name and body, each padded to the 4-byte alignment newc uses.
    pub fn member(&self) -> Vec<u8> {
        let mut out = b"070701".to_vec();
        for v in [
            self.ino,                                            // c_ino
            self.mode,                                           // c_mode
            0,                                                   // c_uid
            0,                                                   // c_gid
            self.nlink,                                          // c_nlink
            self.mtime,                                          // c_mtime
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
        while !out.len().is_multiple_of(4) {
            out.push(0);
        }
        out.extend_from_slice(self.body);
        while !out.len().is_multiple_of(4) {
            out.push(0);
        }
        out
    }

    /// A complete one-member archive, closed by cpio's `TRAILER!!!` member.
    pub fn archive(&self) -> Vec<u8> {
        let mut out = self.member();
        out.extend_from_slice(
            &CpioNewc {
                name: b"TRAILER!!!",
                ..Default::default()
            }
            .member(),
        );
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

/// A small file system mounted at a directory, for the tests that need a
/// mount point (`-X`) or a second device (`-l` across devices). Unmounted when
/// dropped.
///
/// Only macOS can do this unprivileged (`hdiutil`); elsewhere `mount` returns
/// `None` and the caller skips.
pub struct ScratchMount {
    mount_point: std::path::PathBuf,
}

impl ScratchMount {
    /// Mount a fresh 1 MB file system at `mount_point`, which must be an
    /// empty directory. `scratch` holds the disk image.
    pub fn mount(scratch: &Path, mount_point: &Path) -> Option<ScratchMount> {
        if !cfg!(target_os = "macos") {
            return None;
        }
        let image = scratch.join("scratch.dmg");
        let hdiutil = |args: &[&std::ffi::OsStr]| {
            Command::new("hdiutil")
                .args(args)
                .output()
                .is_ok_and(|o| o.status.success())
        };
        let created = hdiutil(&[
            "create".as_ref(),
            "-size".as_ref(),
            "1m".as_ref(),
            "-fs".as_ref(),
            "HFS+".as_ref(),
            "-volname".as_ref(),
            "paxtest".as_ref(),
            image.as_os_str(),
        ]);
        let attached = created
            && hdiutil(&[
                "attach".as_ref(),
                "-nobrowse".as_ref(),
                "-mountpoint".as_ref(),
                mount_point.as_os_str(),
                image.as_os_str(),
            ]);
        attached.then(|| ScratchMount {
            mount_point: mount_point.to_path_buf(),
        })
    }
}

impl Drop for ScratchMount {
    fn drop(&mut self) {
        let _ = Command::new("hdiutil")
            .args([
                "detach".as_ref(),
                "-force".as_ref(),
                self.mount_point.as_os_str(),
            ])
            .output();
    }
}
