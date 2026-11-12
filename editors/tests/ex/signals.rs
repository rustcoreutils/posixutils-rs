//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Signals while ex waits for input (94291-94316), and `:so`/`:@` nested
//! without end.

use super::addresses::ex;
use plib::testing::get_binary_path;
use plib::tmp::TempDir;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};
use std::time::Duration;

/// Start ex on `f` (`content`) in a fresh directory with piped stdin, run
/// `script`, wait for `ready` on stdout -- the script has run and ex is
/// blocked reading -- then send `sig`, keeping stdin open (a signal
/// that is not acted on then shows as ex seeing end of file, after a
/// timeout, and exiting 0).  Returns the
/// exit code, the file afterwards and the directory.
fn signal_while_reading(
    content: &str,
    script: &str,
    ready: &str,
    sig: i32,
) -> (i32, String, TempDir) {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), content).unwrap();
    let mut child = Command::new(get_binary_path("ex"))
        .args(["-s", "f"])
        .current_dir(dir.path())
        .env("HOME", dir.path())
        .env("TMPDIR", dir.path())
        .env_remove("EXINIT")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn ex");
    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(script.as_bytes()).unwrap();
    stdin.flush().unwrap();
    let mut out = BufReader::new(child.stdout.take().unwrap());
    let mut line = String::new();
    while line.trim_end() != ready {
        line.clear();
        assert_ne!(out.read_line(&mut line).unwrap(), 0, "ex ended early");
    }
    let pid = child.id() as i32;
    // A watchdog, so a regression fails instead of hanging the suite: it
    // holds stdin, and closing it ends a read the signal failed to.  (No
    // kill, which could reach a pid reused after ex was reaped.)
    let (done, exited) = std::sync::mpsc::channel::<()>();
    let watchdog = std::thread::spawn(move || {
        let _ = exited.recv_timeout(Duration::from_secs(20));
        drop(stdin);
    });
    unsafe { libc::kill(pid, sig) };
    let status = child.wait().unwrap();
    let _ = done.send(());
    let _ = watchdog.join();
    let file = fs::read_to_string(dir.path().join("f")).unwrap();
    (status.code().unwrap_or(-1), file, dir)
}

#[test]
fn test_hangup_while_reading_preserves_and_exits() {
    let (code, file, dir) =
        signal_while_reading("a\n", "s/a/HUP-ME/\n.p\n", "HUP-ME", libc::SIGHUP);
    assert_eq!(code, 1);
    assert_eq!(file, "a\n", "a hangup does not write the file");
    let saved = fs::read_dir(
        dir.path()
            .join(format!("vi.recover.{}", unsafe { libc::geteuid() })),
    )
    .unwrap()
    .filter_map(|e| fs::read_to_string(e.unwrap().path()).ok())
    .any(|body| body.contains("HUP-ME"));
    assert!(saved, "the buffer is preserved");
}

#[test]
fn test_interrupt_with_piped_input_exits_without_writing() {
    // 94292-94293: stdin not a terminal -- no write, a non-zero exit.
    let (code, file, _dir) = signal_while_reading("a\n", "s/a/INT/\n.p\n", "INT", libc::SIGINT);
    assert_eq!(code, 1);
    assert_eq!(file, "a\n");
}

#[test]
fn test_source_nested_too_deep_is_an_error() {
    let dir = TempDir::new().unwrap();
    let g = dir.path().join("g");
    fs::write(&g, format!("so {}\n", g.display())).unwrap();
    let (_, err, code, _) = ex("a\n", &format!("so {}\nq!\n", g.display()));
    assert_eq!(code, 1, "{err}");
    assert!(err.contains("nested"), "{err}");
}

#[test]
fn test_execute_nested_too_deep_is_an_error() {
    // Buffer a holds `@a`, which runs itself.
    let (_, err, code, _) = ex("@a\n", "1ya a\n@a\nq!\n");
    assert_eq!(code, 1, "{err}");
    assert!(err.contains("nested"), "{err}");
}

#[test]
fn test_hangup_drops_a_half_entered_command() {
    // `w other\` waits for its continuation line; the hangup must not run it.
    let (code, _, dir) = signal_while_reading("a\n", "s/a/X/\n.p\nw other\\\n", "X", libc::SIGHUP);
    assert_eq!(code, 1);
    assert!(
        !dir.path().join("other").exists(),
        "the partial command ran"
    );
}

#[test]
fn test_interrupt_at_a_confirmation_prompt_aborts() {
    // The `^` marker line shows ex waiting for the answer.
    let (code, file, _dir) = signal_while_reading("a\n", "s/a/X/c\n", "^", libc::SIGINT);
    assert_eq!(code, 1);
    assert_eq!(file, "a\n");
}

#[test]
fn test_suspend_without_job_control_does_not_stop() {
    // 95403-95404.  ex runs as the leader of a session of its own, so its
    // process group is the session's -- no job-control shell made it -- and
    // it must not stop itself (it would stay stopped, and this test hang).
    use std::os::unix::process::CommandExt;
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), "a\n").unwrap();
    let mut cmd = Command::new(get_binary_path("ex"));
    cmd.args(["-s", "f"])
        .current_dir(dir.path())
        .env("HOME", dir.path())
        .env_remove("EXINIT")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null());
    // SAFETY: setsid is async-signal-safe, as pre_exec requires.
    unsafe {
        cmd.pre_exec(|| {
            libc::setsid();
            Ok(())
        });
    }
    let mut child = cmd.spawn().expect("spawn ex");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(b"suspend\n.p\nq!\n")
        .unwrap();
    let out = child.wait_with_output().unwrap();
    assert_eq!(String::from_utf8_lossy(&out.stdout), "a\n");
    assert_eq!(out.status.code(), Some(0));
}
