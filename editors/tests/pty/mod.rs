//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! PTY-based integration tests for vi.
//!
//! These tests spawn the actual vi binary in a pseudo-terminal and verify
//! behavior by checking file contents after editing operations.

use plib::tmp::{tempdir, TempDir};
use portable_pty::{native_pty_system, CommandBuilder, PtySize};
use std::fs;
use std::io::{Read, Write};
use std::path::Path;
use std::thread;
use std::time::Duration;

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

/// Test: Long lines should wrap to multiple display rows.
/// This test verifies that vi wraps long lines for display (POSIX requirement)
/// rather than truncating them.
#[test]
fn test_pty_vi_long_line_wrapping() {
    const TEST_LINE_LENGTH: usize = 100; // Create line with numbers 1-100 concatenated

    let td = tempdir().unwrap();
    let file_path = td.path().join("test_wrap.txt");

    // Create a single very long line (200+ characters)
    let long_line = (1..=TEST_LINE_LENGTH)
        .map(|n| n.to_string())
        .collect::<Vec<_>>()
        .join("");
    std::fs::write(&file_path, format!("{}\n", long_line)).unwrap();

    // Use narrow terminal (40 cols) to force wrapping
    let mut vi = ViPtySession::new(&file_path, 25, 40);
    vi.sleep_ms(500);

    // Move cursor to the end of the line
    vi.keys("$");
    vi.sleep_ms(100);

    // If wrapping works, we should be able to navigate without panic
    vi.keys("0"); // Go to start
    vi.sleep_ms(100);
    vi.keys("$"); // Go to end
    vi.sleep_ms(100);

    // Quit without saving
    vi.keys(":q!\r");
    vi.wait();

    // File should be unchanged
    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(contents, format!("{}\n", long_line));
}

/// Test: vi survives a terminal resize (SIGWINCH) and remains responsive.
/// Regression test for audit #V1 (SIGWINCH not handled).
#[test]
fn test_pty_vi_resize_survives_and_saves() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("resize.txt");
    std::fs::write(&file_path, "alpha\nbeta\n").unwrap();

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
    spawn_reader_drain(reader);
    let mut writer = pair.master.take_writer().unwrap();

    thread::sleep(Duration::from_millis(500));
    // Resize the PTY: the kernel delivers SIGWINCH to vi.
    pair.master
        .resize(PtySize {
            rows: 40,
            cols: 120,
            pixel_width: 0,
            pixel_height: 0,
        })
        .unwrap();
    thread::sleep(Duration::from_millis(200));

    // vi must still be alive and responsive after the resize.
    write_keys(&mut writer, "oGAMMA\x1b");
    thread::sleep(Duration::from_millis(100));
    write_keys(&mut writer, ":wq\r");
    wait_with_timeout(&mut child, Duration::from_secs(5));

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert!(
        contents.contains("GAMMA"),
        "vi should survive resize and save; got {:?}",
        contents
    );
}

/// Test: ^C in command mode discards the partial command (count).
/// Regression test for audit #V8 (interrupt silently ignored).
#[test]
fn test_pty_vi_interrupt_cancels_count() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("interrupt.txt");
    std::fs::write(&file_path, "L1\nL2\nL3\n").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    // Begin a count of 99, then interrupt: the count must be discarded so the
    // following `dd` deletes exactly one line rather than the whole buffer.
    vi.keys("99\x03");
    vi.sleep_ms(100);
    vi.keys("dd");
    vi.sleep_ms(100);
    vi.keys(":wq\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(contents, "L2\nL3\n", "interrupt should cancel the count");
}

/// Test: sentence motion `)` works as a delete target (#V6).
#[test]
fn test_pty_vi_sentence_motion_delete() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("sent.txt");
    // POSIX sentences are separated by two spaces.
    std::fs::write(&file_path, "One.  Two.  Three.\n").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    vi.keys("d)"); // delete the first sentence
    vi.sleep_ms(100);
    vi.keys(":wq\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(contents, "Two.  Three.\n");
}

/// Test: `_` moves to the first non-blank of the line (#V7).
#[test]
fn test_pty_vi_underscore_first_nonblank() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("us.txt");
    std::fs::write(&file_path, "    indented\n").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    vi.keys("_rX"); // first non-blank, replace char with X
    vi.sleep_ms(100);
    vi.keys(":wq\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(contents, "    Xndented\n");
}

#[test]
fn test_pty_vi_substitute_char_saves_to_register() {
    // #V17: `s` was handled in the pre-parser fast path, so it saved nothing
    // to a register, recorded no undo, and ignored a count. Here `3s` must
    // replace three characters AND leave them in the unnamed register, so a
    // following `p` can put them back.
    let td = tempdir().unwrap();
    let file_path = td.path().join("test.txt");
    std::fs::write(&file_path, "abcdef\n").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    vi.keys("3sX\x1b");
    vi.sleep_ms(100);
    vi.keys("p");
    vi.sleep_ms(100);
    vi.keys(":wq\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(
        contents.trim_end(),
        "Xabcdef",
        "3s must delete three chars into the unnamed register so p restores them"
    );
}

#[test]
fn test_pty_vi_nul_reinserts_previous_input() {
    // #V15: a NUL in insert mode re-inserts the text from the previous insert
    // session. It needs cross-session storage, which is why it was deferred.
    let td = tempdir().unwrap();
    let file_path = td.path().join("test.txt");
    std::fs::write(&file_path, "").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    // First session inserts "abc".
    vi.keys("iabc\x1b");
    vi.sleep_ms(100);
    // Second session sends a NUL, which must replay "abc".
    vi.keys("a\x00\x1b");
    vi.sleep_ms(100);
    vi.keys(":wq\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(
        contents.trim_end(),
        "abcabc",
        "NUL in insert mode must re-insert the previous session's text"
    );
}

/// #V22: a TAB byte typed at a real terminal must reach the buffer.
///
/// This has to be a PTY test. `Editor::execute_keys` does its own
/// byte-to-`Key` translation, so a headless test never touches
/// `Key::from_byte` — which is exactly where TAB was being swallowed.
#[test]
fn test_pty_vi_tab_is_inserted() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("tab.txt");
    std::fs::write(&file_path, "").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    vi.keys("ia\tb\x1b");
    vi.sleep_ms(100);
    vi.keys(":wq\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(
        contents.trim_end(),
        "a\tb",
        "a typed TAB must land in the buffer, got {contents:?}"
    );
}

/// #V23: `^V` followed by ESC inserts a literal ESC rather than leaving
/// insert mode. Driven through the PTY so the escape byte goes through
/// `InputReader::read_key`, which headless tests bypass.
#[test]
fn test_pty_vi_ctrl_v_inserts_literal_escape() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("literal.txt");
    std::fs::write(&file_path, "").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    // The ESC must be delivered in its own read. `InputReader` decides between
    // a bare ESC and the start of an escape sequence by whether more bytes are
    // already buffered, so writing "\x16\x1bt" in one go makes the reader take
    // ESC+'t' as a sequence — which is what a human typing never produces.
    vi.keys("i\x16");
    vi.sleep_ms(100);
    vi.keys("\x1b");
    vi.sleep_ms(100);
    vi.keys("tail");
    vi.sleep_ms(100);
    vi.keys("\x1b");
    vi.sleep_ms(100);
    vi.keys(":wq\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(
        contents.trim_end(),
        "\x1btail",
        "^V ESC must insert a literal ESC, got {contents:?}"
    );
}

/// #V21: `>>` shifts by one shiftwidth, not by `shiftwidth` tab characters.
#[test]
fn test_pty_vi_shift_right_is_one_shiftwidth() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("shift.txt");
    std::fs::write(&file_path, "hello\n").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    vi.keys(">>");
    vi.sleep_ms(100);
    vi.keys(":wq\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(
        contents.trim_end(),
        "\thello",
        "one >> at shiftwidth 8 / tabstop 8 is a single tab, got {contents:?}"
    );
}

/// #V25/#V26: autoindent and `^D` driven through a real terminal, so the
/// 0x14/0x04 bytes go through `Key::from_byte` rather than the headless
/// translator.
#[test]
fn test_pty_vi_autoindent_and_ctrl_d() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("ai.txt");
    std::fs::write(&file_path, "\t\tbase\n").unwrap();

    let mut vi = ViPtySession::new(&file_path, 25, 80);
    vi.sleep_ms(500);
    vi.keys(":set ai\r");
    vi.sleep_ms(100);
    // o -> new line autoindented to "\t\t"; ^D backs up one shiftwidth (8
    // columns at the default), leaving one tab; then type X.
    vi.keys("o\x04X\x1b");
    vi.sleep_ms(100);
    vi.keys(":wq\r");
    vi.wait();

    let contents = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(
        contents, "\t\tbase\n\tX\n",
        "o must autoindent and ^D must remove one shiftwidth, got {contents:?}"
    );
}

/// `:vi[sual]` in ex command mode takes `[1addr][type][count]`
/// (ex.md §95472-95490): the addressed line becomes the current line and the
/// count sets the `window` edit option. The command used to be parsed as a bare
/// `ExCommand::Visual` with its arguments discarded, so `:5vi` entered visual
/// mode with the cursor wherever it already was.
#[test]
fn test_pty_ex_visual_honours_address_and_count() {
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("lines.txt");
    fs::write(&path, "one\ntwo\nthree\nfour\nfive\nsix\n").unwrap();

    let pty_system = native_pty_system();
    let pair = pty_system
        .openpty(PtySize {
            rows: 10,
            cols: 40,
            pixel_width: 0,
            pixel_height: 0,
        })
        .unwrap();

    let mut cmd = CommandBuilder::new(plib::testing::get_binary_path("ex"));
    cmd.arg(&path);
    cmd.env("TERM", "vt100");
    let mut child = pair.slave.spawn_command(cmd).unwrap();
    drop(pair.slave);
    spawn_reader_drain(pair.master.try_clone_reader().unwrap());
    let mut writer = pair.master.take_writer().unwrap();

    // In ex command mode there is no leading colon. `4vi3` -> current line 4,
    // window 3. Deleting the first character of the current line and then
    // writing proves which line the address selected.
    write_keys(&mut writer, "4vi3\r");
    thread::sleep(Duration::from_millis(200));
    write_keys(&mut writer, "x");
    thread::sleep(Duration::from_millis(100));
    write_keys(&mut writer, ":wq\r");

    wait_with_timeout(&mut child, Duration::from_secs(5));

    let content = fs::read_to_string(&path).unwrap();
    assert_eq!(
        content, "one\ntwo\nthree\nour\nfive\nsix\n",
        "expected `4vi3` to leave the cursor on line 4"
    );
}

/// POSIX requires vi to preserve a modified buffer on SIGHUP/SIGTERM.
///
/// The handlers were installed with `libc::signal`, whose BSD semantics
/// include SA_RESTART: an interrupted `read` is restarted by the kernel
/// rather than returning EINTR, so the visual loop -- which services signals
/// only when a read comes back interrupted -- never noticed the flag and the
/// unsaved buffer went with the process.
#[test]
fn test_pty_vi_preserves_the_buffer_on_sigterm() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("hup.txt");
    fs::write(&file_path, "alpha\n").unwrap();

    // Isolate the recovery directory: vi puts recovery files under $TMPDIR.
    let recover_home = tempdir().unwrap();

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
    cmd.env("TMPDIR", recover_home.path());
    let mut child = pair.slave.spawn_command(cmd).unwrap();
    drop(pair.slave);

    let reader = pair.master.try_clone_reader().unwrap();
    spawn_reader_drain(reader);
    let mut writer = pair.master.take_writer().unwrap();

    thread::sleep(Duration::from_millis(500));
    // Modify the buffer, then leave vi blocked in a read.
    write_keys(&mut writer, "oPRESERVE_ME\x1b");
    thread::sleep(Duration::from_millis(300));

    let pid = child.process_id().expect("child pid") as i32;
    unsafe { libc::kill(pid, libc::SIGTERM) };
    wait_with_timeout(&mut child, Duration::from_secs(5));

    // The recovery file lives under $TMPDIR/vi.recover.<uid>/.
    let dir = recover_home
        .path()
        .join(format!("vi.recover.{}", unsafe { libc::getuid() }));
    let found = fs::read_dir(&dir)
        .map(|entries| {
            entries
                .filter_map(|e| e.ok())
                .filter_map(|e| fs::read_to_string(e.path()).ok())
                .any(|body| body.contains("PRESERVE_ME"))
        })
        .unwrap_or(false);

    assert!(
        found,
        "SIGTERM must preserve the modified buffer; nothing recoverable in {:?}",
        dir
    );
}

/// `:!command` puts the terminal back in cooked mode for the child and then
/// restores it. It restored unconditionally, and `disable_raw_mode` reports
/// success whether or not it had anything to do -- so in ex, which is never
/// in raw mode, the "restore" put the terminal *into* raw mode. The tty then
/// stopped translating CR and delivering whole lines, and the next command
/// never arrived: the session hung.
#[test]
fn test_pty_ex_is_not_left_in_raw_mode_after_a_shell_command() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("raw.txt");
    fs::write(&file_path, "alpha\nbeta\n").unwrap();

    // `ex` is a sibling symlink of the vi binary.
    let ex_path = Path::new(env!("CARGO_BIN_EXE_vi"))
        .parent()
        .unwrap()
        .join("ex");
    if !ex_path.exists() {
        return; // build script could not create the symlink; nothing to test
    }

    let pty_system = native_pty_system();
    let pair = pty_system
        .openpty(PtySize {
            rows: 25,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        })
        .unwrap();

    let mut cmd = CommandBuilder::new(ex_path);
    cmd.arg(&file_path);
    cmd.env("TERM", "vt100");
    let mut child = pair.slave.spawn_command(cmd).unwrap();
    drop(pair.slave);

    let reader = pair.master.try_clone_reader().unwrap();
    spawn_reader_drain(reader);
    let mut writer = pair.master.take_writer().unwrap();

    thread::sleep(Duration::from_millis(400));
    write_keys(&mut writer, "!echo hi\r");
    thread::sleep(Duration::from_millis(400));
    // The shell path prompts "Press ENTER ... to continue" and consumes one
    // byte, so answer that before sending the next command.
    write_keys(&mut writer, "\r");
    thread::sleep(Duration::from_millis(200));
    // If the terminal was left raw, this CR is never translated to a newline
    // and ex waits for a line that will not arrive.
    write_keys(&mut writer, "q!\r");

    // `wait_with_timeout` returns quietly on timeout, so assert on the exit.
    let start = std::time::Instant::now();
    let mut exited = false;
    while start.elapsed() < Duration::from_secs(5) {
        if matches!(child.try_wait(), Ok(Some(_))) {
            exited = true;
            break;
        }
        thread::sleep(Duration::from_millis(50));
    }
    if !exited {
        let _ = child.kill();
    }
    assert!(
        exited,
        "ex did not act on `q!` after a shell command -- the terminal was \
         left in raw mode, so the CR never arrived as a line"
    );
}

/// A parameterized cursor key must be consumed whole.
///
/// `parse_csi_sequence` stopped at the first byte that was not a digit or
/// `~`, leaving the rest in the input stream to be read as ordinary commands:
/// Ctrl-Right (`ESC [ 1 ; 5 C`) delivered `5C`, and `C` changes to the end of
/// the line -- a destructive edit produced by pressing an arrow key.
#[test]
fn test_pty_vi_parameterized_cursor_key_does_not_edit() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("csi.txt");
    fs::write(&file_path, "alpha beta gamma\n").unwrap();

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
    spawn_reader_drain(reader);
    let mut writer = pair.master.take_writer().unwrap();

    thread::sleep(Duration::from_millis(500));
    // Ctrl-Right, twice.
    write_keys(&mut writer, "\x1b[1;5C\x1b[1;5C");
    thread::sleep(Duration::from_millis(200));
    write_keys(&mut writer, ":wq\r");
    wait_with_timeout(&mut child, Duration::from_secs(5));

    let saved = fs::read_to_string(&file_path).unwrap();
    assert_eq!(
        saved, "alpha beta gamma\n",
        "a cursor key must not change the buffer"
    );
}

/// An undecodable byte is bad input, not a reason to end the session.
#[test]
fn test_pty_vi_survives_invalid_utf8_input() {
    let td = tempdir().unwrap();
    let file_path = td.path().join("bad.txt");
    fs::write(&file_path, "alpha\n").unwrap();

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
    spawn_reader_drain(reader);
    let mut writer = pair.master.take_writer().unwrap();

    thread::sleep(Duration::from_millis(500));
    // A lone continuation byte and a truncated two-byte sequence.
    writer.write_all(&[0xC3, 0x28]).unwrap();
    writer.flush().unwrap();
    thread::sleep(Duration::from_millis(200));
    // vi must still be here to act on this. No leading ESC: the reader takes
    // the byte after an ESC as part of an escape sequence, so it would eat
    // the `o`.
    write_keys(&mut writer, "oZ\x1b");
    thread::sleep(Duration::from_millis(200));
    write_keys(&mut writer, ":wq\r");
    wait_with_timeout(&mut child, Duration::from_secs(5));

    let saved = fs::read_to_string(&file_path).unwrap();
    assert!(
        saved.contains('Z'),
        "vi must survive undecodable input and still save; got {:?}",
        saved
    );
}
