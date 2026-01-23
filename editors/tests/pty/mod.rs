//! PTY-based integration tests for vi.
//!
//! These tests spawn the actual vi binary in a pseudo-terminal and verify
//! behavior by checking file contents after editing operations.

use portable_pty::{native_pty_system, CommandBuilder, PtySize};
use std::io::{Read, Write};
use std::thread;
use std::time::Duration;
use tempfile::tempdir;

/// Write key sequence to PTY master.
fn write_keys<W: Write>(w: &mut W, s: &str) {
    w.write_all(s.as_bytes()).unwrap();
    w.flush().unwrap();
}

/// Spawn a thread to continuously drain output from reader.
/// Returns a join handle. The thread runs until the reader returns EOF or error.
fn spawn_reader_drain<R: Read + Send + 'static>(mut reader: R) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        let mut buf = [0u8; 4096];
        loop {
            match reader.read(&mut buf) {
                Ok(0) => break, // EOF
                Ok(_) => {}     // Discard output
                Err(_) => break,
            }
        }
    })
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

/// Test: Insert text and save file.
#[test]
fn test_pty_vi_insert_and_save() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("test.txt");
    std::fs::write(&file_path, "").unwrap();

    let pty_system = native_pty_system();
    let pair = pty_system
        .openpty(PtySize {
            rows: 25,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        })
        .unwrap();

    let mut cmd = CommandBuilder::new(env!("CARGO_BIN_EXE_vi"));
    cmd.arg(&file_path);
    cmd.env("TERM", "vt100");

    let mut child = pair.slave.spawn_command(cmd).unwrap();
    drop(pair.slave);

    let reader = pair.master.try_clone_reader().unwrap();
    let _reader_thread = spawn_reader_drain(reader);
    let mut writer = pair.master.take_writer().unwrap();

    // Wait for vi startup
    thread::sleep(Duration::from_millis(500));

    // Insert "Hello" and save
    write_keys(&mut writer, "i");
    thread::sleep(Duration::from_millis(50));
    write_keys(&mut writer, "Hello");
    thread::sleep(Duration::from_millis(50));
    write_keys(&mut writer, "\x1b"); // ESC
    thread::sleep(Duration::from_millis(100));
    write_keys(&mut writer, ":wq\r");

    wait_with_timeout(&mut child, Duration::from_secs(5));

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(contents.trim(), "Hello");
}

/// Test: Quit without saving preserves original content.
#[test]
fn test_pty_vi_quit_no_save() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("test.txt");
    std::fs::write(&file_path, "original\n").unwrap();

    let pty_system = native_pty_system();
    let pair = pty_system
        .openpty(PtySize {
            rows: 25,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        })
        .unwrap();

    let mut cmd = CommandBuilder::new(env!("CARGO_BIN_EXE_vi"));
    cmd.arg(&file_path);
    cmd.env("TERM", "vt100");

    let mut child = pair.slave.spawn_command(cmd).unwrap();
    drop(pair.slave);

    let reader = pair.master.try_clone_reader().unwrap();
    let _reader_thread = spawn_reader_drain(reader);
    let mut writer = pair.master.take_writer().unwrap();

    // Wait for vi startup
    thread::sleep(Duration::from_millis(500));

    // Delete line and quit without saving
    write_keys(&mut writer, "dd");
    thread::sleep(Duration::from_millis(100));
    write_keys(&mut writer, ":q!\r");

    wait_with_timeout(&mut child, Duration::from_secs(5));

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(contents, "original\n");
}

/// Test: Insert multiple lines.
#[test]
fn test_pty_vi_multiple_lines() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("test.txt");
    std::fs::write(&file_path, "").unwrap();

    let pty_system = native_pty_system();
    let pair = pty_system
        .openpty(PtySize {
            rows: 25,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        })
        .unwrap();

    let mut cmd = CommandBuilder::new(env!("CARGO_BIN_EXE_vi"));
    cmd.arg(&file_path);
    cmd.env("TERM", "vt100");

    let mut child = pair.slave.spawn_command(cmd).unwrap();
    drop(pair.slave);

    let reader = pair.master.try_clone_reader().unwrap();
    let _reader_thread = spawn_reader_drain(reader);
    let mut writer = pair.master.take_writer().unwrap();

    // Wait for vi startup
    thread::sleep(Duration::from_millis(500));

    // Insert three lines
    write_keys(&mut writer, "i");
    thread::sleep(Duration::from_millis(50));
    write_keys(&mut writer, "Line1\rLine2\rLine3");
    thread::sleep(Duration::from_millis(50));
    write_keys(&mut writer, "\x1b"); // ESC
    thread::sleep(Duration::from_millis(100));
    write_keys(&mut writer, ":wq\r");

    wait_with_timeout(&mut child, Duration::from_secs(5));

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

    let pty_system = native_pty_system();
    let pair = pty_system
        .openpty(PtySize {
            rows: 25,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        })
        .unwrap();

    let mut cmd = CommandBuilder::new(env!("CARGO_BIN_EXE_vi"));
    cmd.arg(&file_path);
    cmd.env("TERM", "vt100");

    let mut child = pair.slave.spawn_command(cmd).unwrap();
    drop(pair.slave);

    let reader = pair.master.try_clone_reader().unwrap();
    let _reader_thread = spawn_reader_drain(reader);
    let mut writer = pair.master.take_writer().unwrap();

    // Wait for vi startup
    thread::sleep(Duration::from_millis(500));

    // Move down one line (j), delete line (dd), save and quit
    write_keys(&mut writer, "j");
    thread::sleep(Duration::from_millis(50));
    write_keys(&mut writer, "dd");
    thread::sleep(Duration::from_millis(100));
    write_keys(&mut writer, ":wq\r");

    wait_with_timeout(&mut child, Duration::from_secs(5));

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

    let pty_system = native_pty_system();
    let pair = pty_system
        .openpty(PtySize {
            rows: 10,
            cols: 20, // Narrow terminal to force truncation
            pixel_width: 0,
            pixel_height: 0,
        })
        .unwrap();

    let mut cmd = CommandBuilder::new(env!("CARGO_BIN_EXE_vi"));
    cmd.arg(&file_path);
    cmd.env("TERM", "vt100");

    let mut child = pair.slave.spawn_command(cmd).unwrap();
    drop(pair.slave);

    let reader = pair.master.try_clone_reader().unwrap();
    let _reader_thread = spawn_reader_drain(reader);
    let mut writer = pair.master.take_writer().unwrap();

    // Wait for vi startup
    thread::sleep(Duration::from_millis(500));

    // Move cursor right a few times (exercises display with UTF-8)
    write_keys(&mut writer, "lll");
    thread::sleep(Duration::from_millis(100));

    // Quit without saving
    write_keys(&mut writer, ":q!\r");

    wait_with_timeout(&mut child, Duration::from_secs(5));

    // If we got here without panic, the test passed
    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(contents, "Привет мир\n");
}
