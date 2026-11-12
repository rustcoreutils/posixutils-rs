//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Headless vi tests: shifting, `U`, autoindent, `:file` and `:visual`,
//! crash stoppers, character boundaries, counts, and `:g`.

use super::*;

// ============================================================================
// #V21 / #V22 / #V23 / #V24 — shift width, TAB, ^V literal, ^T
// ============================================================================

/// `>>` shifts by one shiftwidth, not by `shiftwidth` tabs.
///
/// `shiftwidth` was being passed into `shift_right`'s repeat-**count**
/// parameter, so the default `shiftwidth = 8` produced `"\t".repeat(8)` — eight
/// tab characters, 64 columns — on every `>>`.
#[test]
fn test_shift_right_inserts_one_shiftwidth_not_eight_tabs() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello");

    editor.execute_keys(">>").unwrap();

    let line = editor.get_buffer_text().lines().next().unwrap().to_string();
    assert_eq!(
        line, "\thello",
        "default shiftwidth 8 at tabstop 8 is one tab, got {line:?}"
    );
    assert_eq!(
        line.chars().filter(|c| *c == '\t').count(),
        1,
        "got {line:?}"
    );
}

/// `<<` removes one shiftwidth, not 64 columns.
#[test]
fn test_shift_left_removes_one_shiftwidth() {
    let mut editor = Editor::new_headless();
    // Two tabs = 16 columns; one `<<` at shiftwidth 8 must leave 8.
    editor.set_buffer_text("\t\thello");

    editor.execute_keys("<<").unwrap();

    let line = editor.get_buffer_text().lines().next().unwrap().to_string();
    assert_eq!(line, "\thello", "got {line:?}");
}

/// `:set sw=4` must actually change the shift amount.
#[test]
fn test_shift_right_honors_set_shiftwidth() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello");

    editor.execute_keys(":set sw=4\n").unwrap();
    editor.execute_keys(">>").unwrap();

    let line = editor.get_buffer_text().lines().next().unwrap().to_string();
    assert_eq!(
        line, "    hello",
        "4 columns is below tabstop 8, so four spaces, got {line:?}"
    );
}

/// Shifting left past column 0 clamps instead of eating the text.
#[test]
fn test_shift_left_clamps_at_column_zero() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("  hello");

    editor.execute_keys("<<").unwrap();

    assert_eq!(editor.get_buffer_text().lines().next().unwrap(), "hello");
}

/// #V22: a typed TAB must reach the buffer. `Key::Tab` was unreachable —
/// `from_byte` mapped byte 9 to `Ctrl('i')`, which had no arm — so TAB fell
/// through the ignore-everything-else arm and vanished.
#[test]
fn test_tab_inserts_a_tab_in_insert_mode() {
    let mut editor = Editor::new_headless();

    editor.execute_keys("ia\tb\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "a\tb");
}

/// #V23: `^V` takes the next character literally, ESC included, rather than
/// letting it end the insert session.
#[test]
fn test_ctrl_v_inserts_literal_escape() {
    let mut editor = Editor::new_headless();

    // i ^V ESC ESC  -> one literal 0x1b in the buffer, then leave insert mode.
    editor.execute_keys("i\x16\x1b\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "\x1b");
    assert!(matches!(editor.get_mode(), Mode::Command));
}

/// A literalized control character must not trigger its normal insert-mode
/// action. `^D` would otherwise dedent.
#[test]
fn test_ctrl_v_inserts_literal_control_char() {
    let mut editor = Editor::new_headless();

    editor.execute_keys("iX\x16\x04Y\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "X\x04Y");
}

/// POSIX 121875-121877: a `^V` before a <newline> is *discarded* and the
/// newline behaves normally, so this must split the line rather than insert a
/// literal carriage return.
#[test]
fn test_ctrl_v_before_newline_is_discarded() {
    let mut editor = Editor::new_headless();

    editor.execute_keys("iA\x16\nB\x1b").unwrap();

    let text = editor.get_buffer_text();
    let lines: Vec<&str> = text.lines().collect();
    assert_eq!(lines, vec!["A", "B"], "got {text:?}");
}

/// The pending-literal flag is consumed by exactly one key.
#[test]
fn test_ctrl_v_consumes_only_one_key() {
    let mut editor = Editor::new_headless();

    editor.execute_keys("i\x16aa\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "aa");
}

/// `^Q` is the spec's synonym for `^V` (121874).
#[test]
fn test_ctrl_q_is_a_synonym_for_ctrl_v() {
    let mut editor = Editor::new_headless();

    editor.execute_keys("i\x11\x1b\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "\x1b");
}

/// A literal inserted via `^V` is part of the insert session, so `.` repeats it.
/// The old empty `^V` arm never touched `inserted_text`.
#[test]
fn test_ctrl_v_literal_is_recorded_for_dot_repeat() {
    let mut editor = Editor::new_headless();

    editor.execute_keys("i\x16\x02\x1b").unwrap();
    editor.execute_keys(".").unwrap();

    let text = editor.get_buffer_text();
    assert_eq!(
        text.trim_end().matches('\x02').count(),
        2,
        "`.` must repeat the literal, got {text:?}"
    );
}

/// #V24: `^T` inserts blanks at the *cursor* up to the next shiftwidth
/// boundary, not a single tab at column 0.
#[test]
fn test_ctrl_t_indents_at_cursor_to_shiftwidth_boundary() {
    let mut editor = Editor::new_headless();
    editor.execute_keys(":set sw=4\n").unwrap();

    // Type "ab", then ^T: the cursor is at column 2, so the next 4-column
    // boundary is 4, i.e. two blanks. Then type "c".
    editor.execute_keys("iab\x14c\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "ab  c");
}

/// `^T` goes through the insert-session record, so `u` removes it along with
/// the rest of the session and `.` can repeat it.
#[test]
fn test_ctrl_t_is_recorded_in_the_insert_session() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("base");
    editor.execute_keys(":set sw=4\n").unwrap();

    editor.execute_keys("A\x14x\x1b").unwrap();
    let after = editor.get_buffer_text().trim_end().to_string();
    assert_ne!(after, "base", "^T + x must have changed the line");

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text().trim_end(),
        "base",
        "undo must remove the ^T blanks too"
    );
}

/// #V27: `u` after an insert session must remove exactly what was typed.
///
/// `InsertState::start_pos` was captured before `enter_insert_mode`
/// repositioned the cursor, so it was only right for `i`. For `A` it named the
/// pre-command column, and the undo record then deleted that many characters
/// from the wrong place: `Axy<ESC>u` on "base" left "sexy" — two characters
/// removed from column 0 instead of the two that were appended.
#[test]
fn test_undo_after_append_removes_only_the_appended_text() {
    for (keys, typed) in [
        ("Axy\x1b", "basexy"), // A - append at end of line
        ("axy\x1b", "bxyase"), // a - append after cursor
        ("Ixy\x1b", "xybase"), // I - insert at first non-blank
        ("ixy\x1b", "xybase"), // i - insert before cursor (was already right)
    ] {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text("base");

        editor.execute_keys(keys).unwrap();
        assert_eq!(
            editor.get_buffer_text().trim_end(),
            typed,
            "typing {keys:?} produced the wrong text"
        );

        editor.execute_keys("u").unwrap();
        assert_eq!(
            editor.get_buffer_text().trim_end(),
            "base",
            "undo after {keys:?} must restore the original line exactly"
        );
    }
}

/// `^U` in insert mode deletes back to the start of the *insert*, not past it
/// into pre-existing text. This shares the `start_pos` anchor with #V27.
#[test]
fn test_ctrl_u_stops_at_the_start_of_the_insert() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("keep");

    // Append "junk", then ^U: only "junk" may go.
    editor.execute_keys("Ajunk\x15\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "keep");
}

// ============================================================================
// #V20 — U (undo current line)
// ============================================================================

/// `U` restores the line to its state when the cursor arrived on it.
///
/// It could never restore anything before: `line_original` was only written by
/// `save_line_original`, which had no production callers, so `U` always
/// reported "Nothing to undo".
#[test]
fn test_upper_u_restores_line_after_deletions() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\nsecond line");

    editor.execute_keys("xxx").unwrap();
    assert_eq!(
        editor.get_buffer_text().lines().next().unwrap(),
        "lo world",
        "three x's should have removed three characters"
    );

    editor.execute_keys("U").unwrap();
    assert_eq!(
        editor.get_buffer_text().lines().next().unwrap(),
        "hello world"
    );
    // POSIX 121571-121572: first column, not first non-blank.
    assert_eq!(editor.get_cursor().column, 0);
}

/// A whole insert session is undone by `U`, since it all happened after the
/// cursor arrived on the line.
#[test]
fn test_upper_u_restores_line_after_insert_session() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("original");

    editor.execute_keys("iABC\x1b").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "ABCoriginal");

    editor.execute_keys("U").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "original");
}

/// `U` is its own inverse — pressing it twice returns to the modified text.
#[test]
fn test_upper_u_is_its_own_inverse() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello");

    editor.execute_keys("x").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "ello");

    editor.execute_keys("U").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "hello");

    editor.execute_keys("U").unwrap();
    assert_eq!(
        editor.get_buffer_text().trim_end(),
        "ello",
        "a second U must put the change back"
    );
}

/// `U` is a buffer-modifying command, so `u` reverses it (POSIX 95442).
#[test]
fn test_lowercase_u_reverses_an_upper_u() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello");

    editor.execute_keys("xx").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "llo");

    editor.execute_keys("U").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "hello");

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text().trim_end(),
        "llo",
        "u must reverse the U"
    );
}

/// Leaving a line and coming back re-snapshots it, so `U` no longer reaches
/// back past the return.
#[test]
fn test_upper_u_resnapshots_when_the_cursor_returns_to_a_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("first line\nsecond line");

    editor.execute_keys("xx").unwrap();
    assert_eq!(editor.get_buffer_text().lines().next().unwrap(), "rst line");

    // Leave the line and come back: the snapshot is retaken from "rst line".
    editor.execute_keys("jk").unwrap();
    editor.execute_keys("U").unwrap();

    assert_eq!(
        editor.get_buffer_text().lines().next().unwrap(),
        "rst line",
        "U must not reach back past the cursor's return to the line"
    );
}

/// `U` on an untouched line changes nothing and leaves the undo stack alone.
#[test]
fn test_upper_u_on_unchanged_line_is_a_noop() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("untouched\nsecond");

    let _ = editor.execute_keys("U");
    assert_eq!(
        editor.get_buffer_text().lines().next().unwrap(),
        "untouched"
    );
}

/// After a command that changes the line count the snapshot is abandoned, so
/// `U` reports nothing to undo rather than overwriting an unrelated line.
#[test]
fn test_upper_u_refuses_after_the_line_count_changes() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree");

    // Join lines 1 and 2, then ask U to restore.
    editor.execute_keys("J").unwrap();
    let joined = editor.get_buffer_text();
    let _ = editor.execute_keys("U");

    assert_eq!(
        editor.get_buffer_text(),
        joined,
        "U must not touch the buffer once the line numbering has shifted"
    );
}

// ============================================================================
// #V25 / #V26 — autoindent and ^D
// ============================================================================

/// `o`/`O` indent the new line to match the line the command was issued on
/// (POSIX 121501-121502). `options.autoindent` was parsed but never read, so
/// none of this happened before.
#[test]
fn test_autoindent_o_derives_indent_from_originating_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\tindented");
    editor.execute_keys(":set ai\n").unwrap();

    editor.execute_keys("oNEW\x1b").unwrap();

    assert_eq!(editor.get_buffer_text(), "\tindented\n\tNEW\n");
}

/// With autoindent unset the new line gets no indent at all.
#[test]
fn test_autoindent_unset_leaves_new_line_flush() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\tindented");

    editor.execute_keys("oNEW\x1b").unwrap();

    assert_eq!(editor.get_buffer_text(), "\tindented\nNEW\n");
}

/// A <newline> in input mode carries the indent onto the next line
/// (121826-121827).
#[test]
fn test_autoindent_is_carried_across_newline() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\tbase");
    editor.execute_keys(":set ai\n").unwrap();

    editor.execute_keys("oONE\nTWO\x1b").unwrap();

    assert_eq!(editor.get_buffer_text(), "\tbase\n\tONE\n\tTWO\n");
}

/// "Any autoindent characters entered on newly created lines that have no
/// other non-<newline> characters shall be deleted" on ESC (121912-121913) —
/// so `o<ESC>` leaves an empty line, not a line full of whitespace.
#[test]
fn test_autoindent_only_line_is_emptied_on_escape() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\tbase");
    editor.execute_keys(":set ai\n").unwrap();

    editor.execute_keys("o\x1b").unwrap();

    assert_eq!(editor.get_buffer_text(), "\tbase\n\n");
}

/// `^D` moves back to the column after the previous shiftwidth boundary
/// (121781-121782), in display columns. With two tabs (16 columns at
/// tabstop 8) and shiftwidth 4, one `^D` leaves 12 columns.
#[test]
fn test_ctrl_d_backs_up_one_shiftwidth() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\t\tbase");
    editor.execute_keys(":set ai\n").unwrap();
    editor.execute_keys(":set sw=4\n").unwrap();

    editor.execute_keys("o\x04X\x1b").unwrap();

    // 12 columns at tabstop 8 renders as one tab plus four spaces.
    assert_eq!(editor.get_buffer_text(), "\t\tbase\n\t    X\n");
}

/// `0^D` discards the whole autoindent and the `0` itself (121777).
#[test]
fn test_zero_ctrl_d_clears_the_whole_indent() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\t\tbase");
    editor.execute_keys(":set ai\n").unwrap();

    editor.execute_keys("o0\x04X\x1b").unwrap();

    assert_eq!(editor.get_buffer_text(), "\t\tbase\nX\n");
}

/// The one behavior that distinguishes `^^D` from `0^D`: both clear the
/// current line's indent, but after `^^D` "the autoindent level for the next
/// input line shall be derived from the same line from which the autoindent
/// level for the current input line was derived" (121779-121780).
#[test]
fn test_caret_ctrl_d_restores_the_indent_on_the_next_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\t\tbase");
    editor.execute_keys(":set ai\n").unwrap();

    editor.execute_keys("o^\x04A\nB\x1b").unwrap();

    assert_eq!(
        editor.get_buffer_text(),
        "\t\tbase\nA\n\t\tB\n",
        "^^D must un-indent only the current line"
    );
}

/// The contrasting case: after `0^D` the next line does *not* get the indent
/// back. Paired with the test above, this is what proves the two are handled
/// differently rather than both mapped onto "clear the indent".
#[test]
fn test_zero_ctrl_d_does_not_restore_the_indent_on_the_next_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\t\tbase");
    editor.execute_keys(":set ai\n").unwrap();

    editor.execute_keys("o0\x04A\nB\x1b").unwrap();

    assert_eq!(editor.get_buffer_text(), "\t\tbase\nA\nB\n");
}

/// When the cursor does not follow autoindent characters, `^D` "shall have no
/// special meaning" (121776) — it is appended like any other input character.
#[test]
fn test_ctrl_d_after_typed_text_has_no_special_meaning() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("");

    editor.execute_keys("iabc\x04\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "abc\u{4}");
}

/// In column 1 with nothing to erase, `^D` "shall be discarded and no further
/// action taken" (121774-121775) — it must not insert a literal.
#[test]
fn test_ctrl_d_in_column_one_is_discarded() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("");

    editor.execute_keys("i\x04X\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "X");
}

// ============================================================================
// `:file` informational message (ex.md §94981-94987)
// ============================================================================

/// The message must carry the current pathname (or say there is none), the
/// current line and the line count (or say the buffer is empty), and the fact
/// that the buffer has been modified. It used to report only the name, a `[+]`
/// marker and a percentage, with no current line number at all.
#[test]
fn test_file_info_reports_position_and_state() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys(":2\n:f\n").unwrap();

    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(
        msg.contains("line 2 of 3"),
        "expected the current line and line count in {msg:?}"
    );
    assert!(
        !msg.contains("[Modified]"),
        "a buffer that was only navigated is not modified: {msg:?}"
    );
    assert!(
        msg.contains("[No file]"),
        "with no pathname the message must say so: {msg:?}"
    );
}

#[test]
fn test_file_info_reports_modification() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\n");
    editor.execute_keys("ix\x1b:f\n").unwrap();

    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(msg.contains("[Modified]"), "expected [Modified] in {msg:?}");
}

// ============================================================================
// `:vi[sual]` as `edit` (ex.md §95473-95474)
// ============================================================================

/// "If ex is currently in open or visual mode, the Synopsis and behavior of the
/// visual command shall be the same as the edit command", which includes its
/// `[+command][file]` arguments. `:vi` used to parse to a bare `Visual` with the
/// arguments thrown away.
#[test]
fn test_visual_command_in_visual_mode_edits_a_file() {
    let dir = plib::tmp::tempdir().unwrap();
    let path = dir.path().join("v.txt");
    std::fs::write(&path, "one\ntwo\nthree\n").unwrap();

    let mut editor = Editor::new_headless();
    editor
        .execute_keys(&format!(":vi +2 {}\n", path.display()))
        .unwrap();

    assert!(
        editor.get_buffer_text().contains("three"),
        "expected the file to have been loaded: {:?}",
        editor.get_buffer_text()
    );
    assert_eq!(
        editor.get_cursor().line,
        2,
        "expected the +command to have moved to line 2"
    );
}

// ============================================================================
// Phase 1 — crash & hang stoppers
// ============================================================================

/// `:s` with a `\n` in the replacement splits one line into several. The loop
/// that walks the range must step *past* the lines it just inserted; stepping
/// back onto the first part re-substitutes it forever, growing the buffer
/// without bound until the process is killed.
///
/// Run on a worker thread so a regression fails the suite instead of hanging it.
#[test]
fn test_ex_substitute_newline_terminates() {
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text("a\n");
        // POSIX 95369-95371: in vi a `^V`-escaped carriage return splits.
        let r = editor.execute_keys(":1s/^/\x16\r/\r");
        let _ = tx.send(r.map(|()| editor.get_buffer_text()));
    });

    match rx.recv_timeout(std::time::Duration::from_secs(10)) {
        Ok(Ok(text)) => assert_eq!(
            text, "\na\n",
            "the empty match at `^` should have split line 1 in two"
        ),
        Ok(Err(e)) => panic!("substitute failed: {}", e),
        Err(_) => panic!("`:1s/^/^V^M/` did not terminate within 10s"),
    }
}

/// An ex command that fails must report on the status line and leave the editor
/// running. Propagating the error out of `handle_key` unwinds to `run_editor`,
/// which prints and exits — so visual-mode `:q` on a modified buffer used to
/// quit vi and discard the unsaved work it was supposed to be protecting.
#[test]
fn test_visual_quit_on_modified_buffer_warns_without_quitting() {
    let mut editor = Editor::new_headless();
    editor.execute_keys("ihello\x1b").unwrap();

    editor
        .execute_keys(":q\n")
        .expect("`:q` on a modified buffer must not propagate an error");

    assert!(
        !editor.should_quit(),
        "vi must stay running so the unsaved buffer is not lost"
    );
    assert!(
        editor.is_error_message(),
        "expected an error on the status line, got {:?}",
        editor.get_message()
    );
    assert_eq!(editor.get_buffer_text().trim(), "hello");
}

/// Every failing ex command takes the same path out of `handle_ex_key`, so one
/// escaping error means all of them escape.
#[test]
fn test_failing_ex_commands_do_not_quit_the_editor() {
    for cmd in [":e /nonexistent/nope\n", ":n\n", ":zzzz\n"] {
        let mut editor = Editor::new_headless();
        editor.execute_keys("ihello\x1b").unwrap();

        editor
            .execute_keys(cmd)
            .unwrap_or_else(|e| panic!("{:?} propagated an error: {}", cmd, e));
        assert!(!editor.should_quit(), "{:?} quit the editor", cmd);
        assert_eq!(
            editor.get_buffer_text().trim(),
            "hello",
            "{:?} lost the buffer",
            cmd
        );
    }
}

/// `:d`/`:y` accept an explicit count. A count of zero used to reach
/// `start + count - 1` unguarded.
#[test]
fn test_ex_delete_and_yank_reject_zero_count() {
    for cmd in [":1d 0\n", ":1y 0\n"] {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text("one\ntwo\nthree\n");

        editor
            .execute_keys(cmd)
            .unwrap_or_else(|e| panic!("{:?} propagated an error: {}", cmd, e));
        assert!(
            editor.is_error_message(),
            "{:?} should be rejected, got message {:?}",
            cmd,
            editor.get_message()
        );
        assert_eq!(
            editor.get_buffer_text(),
            "one\ntwo\nthree\n",
            "{:?} must leave the buffer alone",
            cmd
        );
    }
}

// ============================================================================
// Phase 8 — the cursor and every range endpoint stay on a character boundary
// ============================================================================
//
// `Position::column` is a 0-indexed *byte* offset (buffer/position.rs), which
// is what the renderer assumes too. But `clamp_column` normalised with a bare
// `min()`, which lowers an out-of-range column and otherwise leaves it alone --
// so a mid-character byte offset was representable, and the next slice panicked.

#[test]
fn test_multibyte_toggle_case_does_not_panic() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("héllo\n");
    // Three toggles, each advancing one *character*: h, é, l.
    editor.execute_keys("~~~").unwrap();
    assert_eq!(editor.get_buffer_text(), "HÉLlo\n");
}

#[test]
fn test_multibyte_substitute_char_does_not_panic() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("éx\n");
    editor.execute_keys("sZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "Zx\n");
}

#[test]
fn test_multibyte_change_word_does_not_panic() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("café x\n");
    editor.execute_keys("cwZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "Z x\n");
}

/// `p` used to back the cursor up one *byte* to land on the last character it
/// pasted, leaving it inside a multi-byte character; the next `x` then panicked
/// in `String::remove`.
#[test]
fn test_multibyte_put_then_delete_does_not_panic() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aébc\n");
    editor.execute_keys("lxp").unwrap();
    editor.execute_keys("x").unwrap();
    assert!(
        editor.get_buffer_text().is_char_boundary(0),
        "buffer must remain valid UTF-8"
    );
}

/// Whatever the keys, the cursor must never be left inside a character.
#[test]
fn test_cursor_stays_on_a_character_boundary() {
    // `p`/`P` need something in the unnamed register, so they yank first.
    for keys in [
        "$", "0", "w", "b", "e", "x", "X", "~", "A!\x1b", "iZ\x1b", "dw", "de", "D", "ywp", "ywP",
        "yyp", "dwp",
    ] {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text("héllo wörld — naïve\n");
        editor.execute_keys("ll").unwrap();
        editor
            .execute_keys(keys)
            .unwrap_or_else(|e| panic!("{:?} errored: {}", keys, e));

        let text = editor.get_buffer_text();
        let cursor = editor.get_cursor();
        let line = text.lines().nth(cursor.line - 1).unwrap_or("");
        assert!(
            line.is_char_boundary(cursor.column.min(line.len())),
            "{:?} left the cursor at byte {} of {:?}, mid-character",
            keys,
            cursor.column,
            line
        );
    }
}

/// `r` and `~` reached into the buffer through `line_mut`, which bypasses the
/// dirty flag as well as undo -- so `rX` then `:q` exited with no warning and
/// the edit was lost silently.
#[test]
fn test_replace_char_marks_buffer_modified() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello\n");
    editor.execute_keys("rX").unwrap();
    editor.execute_keys(":q\n").unwrap();
    assert!(
        !editor.should_quit(),
        "`r` must mark the buffer modified so `:q` warns"
    );
}

#[test]
fn test_toggle_case_marks_buffer_modified() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello\n");
    editor.execute_keys("~").unwrap();
    editor.execute_keys(":q\n").unwrap();
    assert!(
        !editor.should_quit(),
        "`~` must mark the buffer modified so `:q` warns"
    );
}

/// The vi counterpart of ed's malformed-command corpus: drive the editor with
/// generated key sequences over multi-byte text and assert it neither panics
/// nor corrupts the buffer.
///
/// Deterministic (fixed-seed LCG, no `rand` dependency), so any failure is
/// reproducible from the reported case.  The committed case count is sized to
/// keep the suite fast; raising it to 200_000 with sequences of up to 40 keys
/// is what turned up the `L`, `Y` and text-object defects fixed alongside it,
/// and is clean at that depth.
#[test]
fn test_generated_keys_over_multibyte_text_never_panic() {
    // Excludes anything that can reach the filesystem or the shell: ':' (ex
    // commands), 'Z' (ZZ writes the file), and '!' -- the filter operator,
    // which with '>' and Enter in the alphabet runs shell redirections and
    // really does litter the working directory.
    const KEYS: &[char] = &[
        'h', 'j', 'k', 'l', 'w', 'W', 'b', 'B', 'e', 'E', '0', '^', '$', 'G', 'H', 'L', 'M', '%',
        '|', '(', ')', '{', '}', 'x', 'X', 'D', 'J', 'p', 'P', 'u', '.', '~', 'd', 'y', 'c', 's',
        'r', 'a', 'i', 'A', 'I', 'o', 'O', 'R', 'f', 't', 'F', 'T', 'n', 'N', '1', '2', '3', 'é',
        'ö', '\x1b', '\x1b', '"', 'q', 'm', '\'', '`', 'Y', 'C', 'S', '_', '+', '-', '\n', '\t',
        '\x7f', '&', 'g', '[', ']', '<', '>',
    ];
    const SEEDS: &[&str] = &[
        "héllo wörld — naïve\nsecond ligne aé\nthird\n",
        "αβγ δεζ\nηθι\n",
        "\ta\tb\n\né\n",
        "one\ntwo\nthree\n",
        "",
        "é",
        "\n\n\n",
        "   \n\ta\n",
    ];

    let mut state: u64 = 0x9E37_79B9_7F4A_7C15;
    let mut next = || {
        state = state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (state >> 33) as usize
    };

    for case in 0..5000 {
        let seed = SEEDS[next() % SEEDS.len()];
        let len = 1 + next() % 40;
        let keys: String = (0..len).map(|_| KEYS[next() % KEYS.len()]).collect();

        let mut editor = Editor::new_headless();
        editor.set_buffer_text(seed);
        // Errors are fine (many sequences are invalid); panics are not.
        let _ = editor.execute_keys(&keys);
        // Always leave insert mode, so the next assertion sees a settled state.
        let _ = editor.execute_keys("\x1b");

        let text = editor.get_buffer_text();
        let cursor = editor.get_cursor();
        let line = text
            .lines()
            .nth(cursor.line.saturating_sub(1))
            .unwrap_or("");
        assert!(
            line.is_char_boundary(cursor.column.min(line.len())),
            "case {} keys {:?} on {:?}: cursor at byte {} is mid-character in {:?}",
            case,
            keys,
            seed,
            cursor.column,
            line
        );
    }
}

// ============================================================================
// Phase 9 — search reports byte offsets like everything else
// ============================================================================
//
// `Position::column` is a byte offset everywhere in the editor, but
// `search_forward`/`search_backward` both read `from.column` as a character
// index and returned a character index in the same field. On any line with
// multi-byte text before the match the cursor therefore landed short, and the
// snapping added with the cursor invariant hid it as a silent off-by-N.

#[test]
fn test_search_forward_reports_a_byte_column() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aé bcd\n");
    editor.execute_keys("/bcd\n").unwrap();
    // a=0, é=1..2, space=3, b=4.
    assert_eq!(editor.get_cursor().column, 4);
}

#[test]
fn test_search_backward_reports_a_byte_column() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aé bcd é xy\n");
    editor.execute_keys("$?bcd\n").unwrap();
    assert_eq!(editor.get_cursor().column, 4);
}

/// The cursor lands on the match, so deleting a word there deletes the match.
#[test]
fn test_search_then_operate_hits_the_match() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aé bcd\n");
    editor.execute_keys("/bcd\n").unwrap();
    // The cursor is on the match, so `D` truncates exactly at it.
    editor.execute_keys("D").unwrap();
    assert_eq!(editor.get_buffer_text(), "aé \n");
}

/// An empty line has nothing after the search start, and the "is there room
/// to search" guard used a strict `<` -- so `/^$/` could never match one.
#[test]
fn test_search_finds_an_empty_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\n\nthree\n");
    editor.execute_keys("/^$\n").unwrap();
    assert_eq!(editor.get_cursor().line, 2);
}

#[test]
fn test_search_next_and_previous_on_multibyte_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aé x\nbö x\ncü x\n");
    // Each line is "<ascii><2-byte> x", so the x sits at byte 4, not char 3.
    editor.execute_keys("/x\n").unwrap();
    assert_eq!(
        (editor.get_cursor().line, editor.get_cursor().column),
        (1, 4)
    );
    editor.execute_keys("n").unwrap();
    assert_eq!(
        (editor.get_cursor().line, editor.get_cursor().column),
        (2, 4)
    );
    editor.execute_keys("n").unwrap();
    assert_eq!(
        (editor.get_cursor().line, editor.get_cursor().column),
        (3, 4)
    );
    editor.execute_keys("N").unwrap();
    assert_eq!(
        (editor.get_cursor().line, editor.get_cursor().column),
        (2, 4)
    );
}

/// `^` anchors to the start of the *line*, so a global substitute must not
/// re-anchor it at each restart: `:s/^/> /g` inserts one prefix, not one per
/// character.
#[test]
fn test_global_substitute_does_not_reanchor_caret() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abc\n");
    editor.execute_keys(":s/^/> /g\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "> abc\n");
}

/// Likewise `$`.
#[test]
fn test_global_substitute_does_not_reanchor_dollar() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abc\n");
    editor.execute_keys(":s/$/!/g\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "abc!\n");
}

// ============================================================================
// Phase 9b — counts that mean characters, not bytes
// ============================================================================

/// `3s` substitutes three *characters*. The end column added a character
/// count to a byte offset and then clamped it with a character count, so on
/// multi-byte text it consumed the wrong span.
#[test]
fn test_substitute_count_counts_characters() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("héllo\n");
    editor.execute_keys("3sZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "Zlo\n");
}

#[test]
fn test_substitute_count_past_end_of_line_is_clamped() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("éx\n");
    editor.execute_keys("9sZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "Z\n");
}

/// `cw` extends the range past the last character of the word. Adding one
/// *byte* landed inside that character when it was multi-byte.
#[test]
fn test_change_word_includes_a_multibyte_final_character() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("café x\n");
    editor.execute_keys("cwZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "Z x\n");
}

/// `^W` deletes the word before the cursor, `^U` the whole insert. Both
/// counted a byte distance and then deleted that many *characters*, so they
/// over-deleted on multi-byte text -- `^U` ate the preceding newline.
#[test]
fn test_insert_delete_word_before_cursor_on_multibyte() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("");
    editor.execute_keys("ihéllo wörld\x17").unwrap();
    editor.execute_keys("\x1b").unwrap();
    // trim only the newline -- the trailing blank is exactly what `^W` keeps.
    assert_eq!(
        editor.get_buffer_text().trim_end_matches('\n'),
        "héllo ",
        "^W deletes the word, not the blank before it"
    );
}

#[test]
fn test_insert_delete_line_does_not_eat_the_previous_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("keep\n");
    editor.execute_keys("A\nhéllo wörld\x15").unwrap();
    editor.execute_keys("\x1b").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "keep\n\n",
        "^U must clear only what was inserted on this line"
    );
}

/// POSIX (vi, "Move to Specified Column"): `|` moves to a *column*, so a tab
/// counts for as many columns as it displays. The implementation used the
/// count as a character index, which disagrees with the screen on any line
/// containing a tab.
#[test]
fn test_pipe_moves_to_a_display_column() {
    let mut editor = Editor::new_headless();
    // With tabstop 8: 'a' is column 1, the tab spans columns 2-8, 'b' is
    // column 9 and 'c' column 10. As a character index, column 9 would run
    // past the four characters and clamp to 'c'.
    editor.set_buffer_text("a\tbc\n");
    editor.execute_keys("9|").unwrap();
    assert_eq!(editor.get_cursor().column, 2, "column 9 is 'b' at byte 2");
}

#[test]
fn test_pipe_display_column_with_consecutive_tabs() {
    let mut editor = Editor::new_headless();
    // Two tabs span columns 1-8 and 9-16; 'x' is column 17.
    editor.set_buffer_text("\t\tx\n");
    editor.execute_keys("9|").unwrap();
    assert_eq!(
        editor.get_cursor().column,
        1,
        "column 9 is the second tab, at byte 1"
    );
}

/// Without tabs a column is a character, which on multi-byte text is still
/// not a byte.
#[test]
fn test_pipe_column_is_not_a_byte_offset_on_multibyte() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("héllo\n");
    // h=1, é=2, l=3 -- and 'l' sits at byte 3 because é is two bytes.
    editor.execute_keys("3|").unwrap();
    assert_eq!(editor.get_cursor().column, 3);
}

// ============================================================================
// Phase 10 — `:g` must survive the renumbering its own commands cause
// ============================================================================
//
// POSIX marks the lines matching the pattern first, then runs the command list
// against each in order. Every command that inserts, removes or relocates
// lines renumbers the marks not yet visited. The code compensated only when
// `command.trim().starts_with('d')` -- a test that misses `.d`, `.,.+1d`, `j`,
// `m` and everything else -- so the loop then addressed unrelated lines and
// destroyed them. Marks are now followed through the buffer's edit journal.

#[test]
fn test_ex_global_delete_with_explicit_current_address() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\nX\nb\nX\nc\n");
    editor.execute_keys(":g/X/.d\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "a\nb\nc\n");
}

#[test]
fn test_ex_global_bare_delete_still_works() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\nX\nb\nX\nc\n");
    editor.execute_keys(":g/X/d\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "a\nb\nc\n");
}

#[test]
fn test_ex_global_delete_two_lines_per_match() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("X\ngone\nkeep\nX\ngone\nkeep\n");
    editor.execute_keys(":g/X/.,.+1d\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "keep\nkeep\n");
}

#[test]
fn test_ex_global_join_does_not_lose_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("X\ntail\nkeep\nX\ntail\n");
    editor.execute_keys(":g/X/j\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "X tail\nkeep\nX tail\n");
}

#[test]
fn test_ex_global_move_to_top() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\nX1\nb\nX2\nc\n");
    editor.execute_keys(":g/X/m0\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "X2\nX1\na\nb\nc\n");
}

#[test]
fn test_ex_global_substitute_that_adds_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aXb\ncXd\n");
    editor.execute_keys(":g/X/s/X/-/\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "a-b\nc-d\n");
}

/// A relocating command leaves the line count alone while still renumbering
/// the marks not yet visited -- the case a count delta cannot see, and the
/// reason marks are followed through the edit journal. Matches /usr/bin/ex.
#[test]
fn test_ex_global_move_to_last_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("X1\na\nX2\nb\n");
    editor.execute_keys(":g/X/m$\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "a\nb\nX1\nX2\n");
}

#[test]
fn test_ex_global_copy_to_last_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("X1\na\nX2\nb\n");
    editor.execute_keys(":g/X/t$\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "X1\na\nX2\nb\nX1\nX2\n");
}
