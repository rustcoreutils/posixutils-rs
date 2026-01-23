//! PTY-based integration tests for vi.
//!
//! These tests spawn the actual vi binary in a pseudo-terminal and verify
//! behavior by checking file contents after editing operations.

use portable_pty::{native_pty_system, CommandBuilder, PtySize};
use std::io::{Read, Write};
use std::path::Path;
use std::thread;
use std::time::Duration;
use tempfile::tempdir;

/// Write key sequence to PTY master.
fn write_keys<W: Write>(w: &mut W, s: &str) {
    w.write_all(s.as_bytes()).unwrap();
    w.flush().unwrap();
}

/// Spawn a detached thread to continuously drain output from reader.
/// The thread runs until the reader returns EOF or error.
fn spawn_reader_drain<R: Read + Send + 'static>(mut reader: R) {
    thread::spawn(move || {
        let mut buf = [0u8; 4096];
        loop {
            match reader.read(&mut buf) {
                Ok(0) => break, // EOF
                Ok(_) => {}     // Discard output
                Err(_) => break,
            }
        }
    });
}

/// Wait for child process to exit with timeout.
fn wait_with_timeout(child: &mut Box<dyn portable_pty::Child + Send + Sync>, timeout: Duration) {
    let start = std::time::Instant::now();
    while start.elapsed() < timeout {
        if let Ok(Some(_)) = child.try_wait() {
            return;
        }
        thread::sleep(Duration::from_millis(50));
    }
}

/// Helper struct for PTY-based vi tests.
struct ViPtySession {
    child: Box<dyn portable_pty::Child + Send + Sync>,
    writer: Box<dyn Write + Send>,
}

impl ViPtySession {
    /// Spawn vi with the given file in a PTY of the specified size.
    fn new(file_path: &Path, rows: u16, cols: u16) -> Self {
        let pty_system = native_pty_system();
        let pair = pty_system
            .openpty(PtySize {
                rows,
                cols,
                pixel_width: 0,
                pixel_height: 0,
            })
            .unwrap();

        let mut cmd = CommandBuilder::new(env!("CARGO_BIN_EXE_vi"));
        cmd.arg(file_path);
        cmd.env("TERM", "vt100");

        let child = pair.slave.spawn_command(cmd).unwrap();
        drop(pair.slave);

        let reader = pair.master.try_clone_reader().unwrap();
        spawn_reader_drain(reader);
        let writer = pair.master.take_writer().unwrap();

        Self { child, writer }
    }

    /// Send key sequence to vi.
    fn keys(&mut self, s: &str) {
        write_keys(&mut self.writer, s);
    }

    /// Sleep for the given number of milliseconds.
    fn sleep_ms(&self, ms: u64) {
        thread::sleep(Duration::from_millis(ms));
    }

    /// Wait for vi to exit with a timeout.
    fn wait(mut self) {
        wait_with_timeout(&mut self.child, Duration::from_secs(5));
    }
}

/// Test: Insert text and save file.
#[test]
fn test_pty_vi_insert_and_save() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("test.txt");
    std::fs::write(&file_path, "").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    vi.keys("iHello\x1b");
    vi.sleep_ms(100);
    vi.keys(":wq\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(contents.trim(), "Hello");
}

/// Test: Quit without saving preserves original content.
#[test]
fn test_pty_vi_quit_no_save() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("test.txt");
    std::fs::write(&file_path, "original\n").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    vi.keys("dd");
    vi.sleep_ms(100);
    vi.keys(":q!\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(contents, "original\n");
}

/// Test: Insert multiple lines.
#[test]
fn test_pty_vi_multiple_lines() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("test.txt");
    std::fs::write(&file_path, "").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    vi.keys("iLine1\rLine2\rLine3\x1b");
    vi.sleep_ms(100);
    vi.keys(":wq\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    let lines: Vec<&str> = contents.lines().collect();
    assert_eq!(lines.len(), 3);
    assert_eq!(lines[0], "Line1");
    assert_eq!(lines[1], "Line2");
    assert_eq!(lines[2], "Line3");
}

/// Test: Delete a line and save.
#[test]
fn test_pty_vi_delete_and_save() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("test.txt");
    std::fs::write(&file_path, "Line1\nLine2\nLine3\n").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    vi.keys("jdd:wq\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    let lines: Vec<&str> = contents.lines().collect();
    assert_eq!(lines.len(), 2);
    assert_eq!(lines[0], "Line1");
    assert_eq!(lines[1], "Line3");
}

/// Test: UTF-8 display with multibyte characters in narrow terminal.
/// Regression test for issue #536 - vi panics on UTF-8 char boundary.
#[test]
fn test_pty_vi_utf8_display() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("test_utf8.txt");
    // Cyrillic text "Привет мир" = "Hello world" - each Cyrillic char is 2 bytes
    std::fs::write(&file_path, "Привет мир\n").unwrap();

    let mut vi = ViPtySession::new(&file_path, 10, 20); // Narrow terminal to force truncation
    vi.sleep_ms(500);
    vi.keys("lll");
    vi.sleep_ms(100);
    vi.keys(":q!\r");
    vi.wait();

    // If we got here without panic, the test passed
    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(contents, "Привет мир\n");
}

/// Test: `:set number` displays line numbers without panic.
/// Regression test for issue #530.
#[test]
fn test_pty_vi_set_number() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("test_number.txt");
    std::fs::write(&file_path, "line1\nline2\nline3\n").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    vi.keys(":set number\r");
    vi.sleep_ms(200);
    vi.keys("jjk");
    vi.sleep_ms(100);
    vi.keys(":set nonumber\r");
    vi.sleep_ms(100);
    vi.keys(":q!\r");
    vi.wait();

    // File should be unchanged
    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(contents, "line1\nline2\nline3\n");
}
