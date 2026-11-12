//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Headless vi tests: `:map`, `:map!`, abbreviations, and the tag stack.

use super::*;

// ============================================================================
// :map / :ab definitions -- storage and listing
// ============================================================================

/// `:map` and `:ab` define; `:unmap` and `:una` remove; the no-argument forms
/// list (95080-95083, 94864). Expansion is not wired up yet, so what is
/// observable here is that a definition is stored and comes back.
#[test]
fn test_map_and_ab_define_and_list() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");

    editor.execute_keys(":map q dd\n").unwrap();
    assert!(!editor.is_error_message(), "{:?}", editor.get_message());

    editor.execute_keys(":map\n").unwrap();
    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(msg.contains('q') && msg.contains("dd"), "got {msg:?}");

    editor.execute_keys(":ab teh the\n").unwrap();
    editor.execute_keys(":ab\n").unwrap();
    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(msg.contains("teh") && msg.contains("the"), "got {msg:?}");
}

/// 95089-95092: the `!` form is a separate list, so one lhs can hold two
/// definitions at once, and `:map` must not show the `:map!` one.
#[test]
fn test_map_bang_is_a_separate_list() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    editor.execute_keys(":map q dd\n").unwrap();
    editor.execute_keys(":map! q xyz\n").unwrap();

    editor.execute_keys(":map\n").unwrap();
    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(msg.contains("dd") && !msg.contains("xyz"), "got {msg:?}");

    editor.execute_keys(":map!\n").unwrap();
    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(msg.contains("xyz") && !msg.contains("dd"), "got {msg:?}");

    // Removing from one list leaves the other alone.
    editor.execute_keys(":unmap q\n").unwrap();
    assert!(!editor.is_error_message(), "{:?}", editor.get_message());
    editor.execute_keys(":map!\n").unwrap();
    assert!(editor.get_message().unwrap_or_default().contains("xyz"));
}

/// Removing something that is not there is an error for both commands
/// (95457-95462 for `unmap`, 95436-95437 for `una`), and `:unmap!` addresses
/// only the text input list.
#[test]
fn test_unmap_and_una_report_a_missing_entry() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");

    editor.execute_keys(":unmap nosuch\n").unwrap();
    assert!(editor.is_error_message(), "{:?}", editor.get_message());
    editor.execute_keys(":una nosuch\n").unwrap();
    assert!(editor.is_error_message(), "{:?}", editor.get_message());

    // Defined in the command list only, so the `!` form must still fail.
    editor.execute_keys(":map q dd\n").unwrap();
    editor.execute_keys(":unmap! q\n").unwrap();
    assert!(
        editor.is_error_message(),
        "unmap! addresses the text input list; got {:?}",
        editor.get_message()
    );
    editor.execute_keys(":unmap q\n").unwrap();
    assert!(!editor.is_error_message(), "{:?}", editor.get_message());
}

/// `is_error` describes `message`, so clearing one has to clear the other.
/// Four sites assigned `message = None` on its own, so the flag outlived the
/// message it described and the next command that succeeded still read as
/// having failed. Surfaced by the `:unmap` test above, which does exactly this
/// sequence.
#[test]
fn test_the_error_flag_does_not_outlive_its_message() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");

    editor.execute_keys(":unmap nosuch\n").unwrap();
    assert!(editor.is_error_message(), "the failure is reported");

    // Any subsequent command that succeeds discards the message; the flag
    // must go with it.  (`0`: on a one-line buffer `j` fails, which is an
    // error of its own.)
    editor.execute_keys("0").unwrap();
    assert!(editor.get_message().is_none(), "the message is gone");
    assert!(
        !editor.is_error_message(),
        "and so is the flag that described it"
    );
}

/// The `^V` quoting proved at the ex-command-line level in
/// `test_ctrl_v_on_the_ex_line_quotes_the_next_key` now has somewhere to land:
/// `:map Q :wq^V^M` must store a carriage return, which the listing shows as
/// `^M` rather than ending the line.
#[test]
fn test_ctrl_v_quoted_cr_survives_into_the_stored_map() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    // The quoted Enter is a CR in the replacement; a second Enter submits.
    editor.execute_keys(":map Q :wq\x16\n").unwrap();
    editor.execute_keys("\n").unwrap();
    assert!(!editor.is_error_message(), "{:?}", editor.get_message());

    editor.execute_keys(":map\n").unwrap();
    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(
        msg.contains(":wq^M"),
        "the CR must be stored and shown in caret notation; got {msg:?}"
    );
}

// ============================================================================
// Tag stack (:pop, :tags) -- extensions, see NONPOSIX.md
// ============================================================================

/// Build a tags file plus two sources and return (dir, a.c path, b.c path).
///
/// Paths written into the tags file are absolute, and the editor is opened with
/// the same strings. `goto_tag` decides "is this file already open?" by
/// comparing the two as written, so a relative name here and an absolute one
/// there would silently take the reopen path and reset the cursor.
fn tag_fixture() -> (plib::tmp::TempDir, String, String) {
    let dir = plib::tmp::tempdir().unwrap();
    let a = dir.path().join("a.c");
    let b = dir.path().join("b.c");
    std::fs::write(&a, "one\ntwo\nint local() { }\nfour\nfive\n").unwrap();
    std::fs::write(&b, "alpha\nbravo\nint helper() { }\n").unwrap();
    std::fs::write(
        dir.path().join("tags"),
        format!(
            "local\t{}\t3\nhelper\t{}\t3\n",
            a.to_str().unwrap(),
            b.to_str().unwrap()
        ),
    )
    .unwrap();
    let (as_, bs) = (
        a.to_str().unwrap().to_string(),
        b.to_str().unwrap().to_string(),
    );
    (dir, as_, bs)
}

fn set_tags_option(editor: &mut Editor, dir: &std::path::Path) {
    let tags = dir.join("tags");
    editor
        .execute_initial_command(&format!("set tags={}", tags.to_str().unwrap()))
        .unwrap();
}

/// `:tag` within one file, then `:pop` back. A jump that does not change the
/// file still has to push: `^]` then a way back inside one source is the common
/// case, and the whole point of the stack.
#[test]
fn test_pop_returns_within_the_same_file() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();
    editor.execute_keys("3G").unwrap();
    editor.execute_keys("1G").unwrap();
    assert_eq!(editor.get_cursor().line, 1);

    editor.execute_keys(":tag local\n").unwrap();
    assert_eq!(editor.get_cursor().line, 3, "jumped to the definition");

    editor.execute_keys(":pop\n").unwrap();
    assert_eq!(editor.get_cursor().line, 1, "and back to where we started");
}

/// Across files: `:pop` reopens the origin and restores the position in it.
#[test]
fn test_pop_returns_across_files() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();
    editor.execute_keys("4G").unwrap();

    editor.execute_keys(":tag helper\n").unwrap();
    assert!(
        editor.get_buffer_text().contains("bravo"),
        "should be in b.c now, got {:?}",
        editor.get_buffer_text()
    );

    editor.execute_keys(":pop\n").unwrap();
    assert!(
        editor.get_buffer_text().contains("four"),
        "should be back in a.c, got {:?}",
        editor.get_buffer_text()
    );
    assert_eq!(editor.get_cursor().line, 4, "at the line we left from");
}

/// `^]` pushes exactly as `:tag` does -- the push lives in `goto_tag`, so both
/// entry points get it without knowing about the stack.
#[test]
fn test_ctrl_bracket_pushes_the_tag_stack() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();
    // Put the cursor on the word `local` on line 3... start from line 1 and
    // write the word there so `^]` has something to read.
    editor.execute_keys("1Gcwlocal\x1b").unwrap();
    editor.execute_keys("0").unwrap();

    editor.execute_keys("\x1d").unwrap(); // ^]
    assert_eq!(editor.get_cursor().line, 3, "^] jumped to the definition");

    editor.execute_keys(":pop\n").unwrap();
    assert_eq!(editor.get_cursor().line, 1, ":pop undid the ^] jump");
}

/// A tag that is not in the tags file moves nothing, so it must not push --
/// otherwise a later `:pop` would "return" to a place we never left.
#[test]
fn test_a_failed_tag_lookup_does_not_push() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();

    editor.execute_keys(":tag nosuchtag\n").unwrap();
    assert!(editor.is_error_message(), "a missing tag reports an error");

    editor.execute_keys(":pop\n").unwrap();
    assert!(
        editor.is_error_message(),
        "the stack must still be empty; got {:?}",
        editor.get_message()
    );
    assert!(editor
        .get_message()
        .unwrap_or_default()
        .contains("tag stack empty"));
}

/// `:pop` on an empty stack is an error, and says which.
#[test]
fn test_pop_on_an_empty_stack_reports_itself() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    editor.execute_keys(":pop\n").unwrap();
    assert!(editor.is_error_message());
    assert!(
        editor
            .get_message()
            .unwrap_or_default()
            .contains("tag stack empty"),
        "got {:?}",
        editor.get_message()
    );
}

/// A modified buffer refuses the cross-file return -- and keeps the entry, so
/// the user can write and pop again. Discarding it would make the refusal
/// unrecoverable, which is the opposite of what a guard is for.
#[test]
fn test_pop_refuses_a_modified_buffer_without_losing_the_entry() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();
    editor.execute_keys("2G").unwrap();
    editor.execute_keys(":tag helper\n").unwrap();

    editor.execute_keys("iEDITED\x1b").unwrap();
    editor.execute_keys(":pop\n").unwrap();
    assert!(editor.is_error_message(), "a modified buffer refuses");
    assert!(
        editor.get_buffer_text().contains("EDITED"),
        "and the edit survives"
    );

    // The entry is still there: undo the change and the pop now works.
    editor.execute_keys(":w\n").unwrap();
    editor.execute_keys(":pop\n").unwrap();
    assert!(
        editor.get_buffer_text().contains("one"),
        "the retry must return to a.c, so the entry was kept; got {:?}",
        editor.get_buffer_text()
    );
    assert_eq!(editor.get_cursor().line, 2);
}

/// `:tags` names the tag and where it will return to.
#[test]
fn test_tags_lists_the_stack() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();
    editor.execute_keys("2G").unwrap();
    editor.execute_keys(":tag helper\n").unwrap();

    editor.execute_keys(":tags\n").unwrap();
    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(!editor.is_error_message(), "listing is not a failure");
    assert!(
        msg.contains("helper") && msg.contains("a.c") && msg.contains("line 2"),
        "must name the tag and the position :pop returns to; got {msg:?}"
    );
}

/// An empty stack lists a line rather than erroring, so `:tags` stays usable
/// from a script running under `-s`.
#[test]
fn test_tags_on_an_empty_stack_is_not_an_error() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    editor.execute_keys(":tags\n").unwrap();
    assert!(!editor.is_error_message());
    assert!(editor
        .get_message()
        .unwrap_or_default()
        .contains("tag stack empty"));
}

/// A `^V` typed on the colon line quotes the next key, which is the only way to
/// get a CR into a map's replacement -- `:map Q :wq^V^M` is the commonest
/// mapping there is. `Key::Ctrl('v')` used to fall into `handle_ex_key`'s
/// catch-all and vanish, so the sequence could not be entered at all.
///
/// The observable half of that is here: a quoted Enter is a character in the
/// command line, not the end of it. That the quoting survives all the way into
/// the stored replacement is asserted once `:map` is implemented.
#[test]
fn test_ctrl_v_on_the_ex_line_quotes_the_next_key() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");

    // :map Q :wq^V<Enter> -- the ^V makes the Enter a quoted CR.
    editor.execute_keys(":map Q :wq\x16\n").unwrap();
    assert_eq!(
        editor.get_mode(),
        Mode::Ex,
        "a quoted Enter must not submit the command line"
    );
    assert!(
        editor.get_message().is_none(),
        "nothing should have run yet; got {:?}",
        editor.get_message()
    );

    // An unquoted Enter does submit it, and the definition takes.
    editor.execute_keys("\n").unwrap();
    assert_eq!(editor.get_mode(), Mode::Command);
    assert!(
        !editor.is_error_message(),
        "the line should have parsed as a map definition; got {:?}",
        editor.get_message()
    );
}

/// Erasing a quoted character removes the whole pair: two chars in the buffer,
/// but one keystroke to the user.
#[test]
fn test_backspace_erases_a_quoted_pair_on_the_ex_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    // Type `:ab x ` then a quoted space, erase it, then finish with a real
    // replacement. If the erase left the stray ^V behind, the lhs would not be
    // `x` and the diagnostic would name something else.
    editor.execute_keys(":ab x \x16 \x7f").unwrap();
    editor.execute_keys("y\n").unwrap();
    assert!(
        !editor.is_error_message(),
        "should have parsed as an abbreviation; got {:?}",
        editor.get_message()
    );
    // And the lhs really is `x`: removing exactly `x` succeeds, which it could
    // not if the erase had left the stray ^V attached to it.
    editor.execute_keys(":una x\n").unwrap();
    assert!(
        !editor.is_error_message(),
        "the lhs should be exactly `x`; got {:?}",
        editor.get_message()
    );
}

// ============================================================================
// :map expansion
// ============================================================================

/// The basic case, and the count case beside it. A count is part of the
/// command, not an argument to it, and 96607-96608 requires that a digit lhs
/// work at all -- so expansion is not suppressed while a count is accumulating.
#[test]
fn test_command_map_expands() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\nfour\n");
    editor.execute_keys(":map q dd\n").unwrap();

    editor.execute_keys("q").unwrap();
    assert_eq!(editor.get_buffer_text(), "two\nthree\nfour\n");

    editor.execute_keys("2q").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "four\n",
        "a count reaches the map"
    );
}

/// 96598-96600, the case POSIX pins by name: "if the character 'x' was mapped
/// to 'y', the command fx searched for the 'x' character, not the 'y'
/// character. POSIX.1-2024 requires this behavior." The same holds everywhere
/// the vi parser is waiting for an argument rather than a command.
#[test]
fn test_no_expansion_in_argument_position() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("axbycz\n");
    editor.execute_keys(":map x y\n").unwrap();

    // f takes a character argument: it must find the literal `x` at column 1.
    editor.execute_keys("0fx").unwrap();
    assert_eq!(editor.get_cursor().column, 1, "fx must find x, not y");

    // r takes one too: `rx` must write an `x`.
    editor.execute_keys("0rx").unwrap();
    assert_eq!(&editor.get_buffer_text()[..1], "x");

    // And a register name after `"` is an argument, not a command.
    editor.set_buffer_text("alpha\nbravo\n");
    editor.execute_keys("\"xyy").unwrap();
    assert_eq!(
        editor.get_register('x').map(|r| r.text.clone()),
        Some("alpha\n".to_string()),
        "the register name must be the literal x"
    );
}

/// 95097-95098: a `^V`-escaped character "shall not be part of a match to an
/// lhs". 96600-96606 makes that required from the second character on and
/// permitted for the first; it is implemented uniformly.
#[test]
fn test_ctrl_v_suppresses_a_command_map() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\n");
    editor.execute_keys(":map q dd\n").unwrap();

    editor.execute_keys("\x16q").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "one\ntwo\n",
        "a quoted q must not fire the map"
    );
}

/// A multi-key lhs holds its keys until the match resolves. POSIX leaves the
/// wait unspecified (95116-95118); under shortest-match a held prefix can only
/// be a strict prefix, so waiting for the next keystroke always resolves it.
#[test]
fn test_multi_key_map_waits_for_the_rest() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys(":map ab dd\n").unwrap();

    // `a` alone is a prefix: nothing happens yet, and in particular the `a`
    // has not entered insert mode.
    editor.execute_keys("a").unwrap();
    assert_eq!(editor.get_buffer_text(), "one\ntwo\nthree\n");
    assert_eq!(editor.get_mode(), Mode::Command, "held, not dispatched");

    editor.execute_keys("b").unwrap();
    assert_eq!(editor.get_buffer_text(), "two\nthree\n");
}

/// When a held prefix turns out to match nothing, the keys after the first go
/// back to the front of the queue rather than being dispatched where they lie
/// -- otherwise a left-hand side starting inside them could never fire.
#[test]
fn test_a_failed_prefix_releases_its_keys_for_a_later_match() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    // `ax` is a prefix of nothing, but `x` on its own is mapped.
    editor.execute_keys(":map ax dd\n").unwrap();
    editor.execute_keys(":map x dd\n").unwrap();

    // `a` holds as a prefix of `ax`; `y` fails the match. The `a` is then
    // dispatched (append, entering insert mode) and the `y` goes back to the
    // front of the queue rather than being dropped or acted on out of order,
    // so it is inserted as text after the first character.
    editor.execute_keys("ay\x1b").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "oyne\ntwo\nthree\n",
        "the released key must still be acted on, in order"
    );
}

/// vi.md 120624: "If the vi command resulted from a map expansion, all
/// characters from that map expansion shall be discarded."
#[test]
fn test_an_error_discards_the_rest_of_the_expansion() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("ab\ntwo\n");
    // `5rZ` on a two-character line is an error -- POSIX makes a count larger
    // than the characters left on the line fail with the line unchanged -- so
    // the `dd` behind it must never run.
    editor.execute_keys(":map q 5rZdd\n").unwrap();

    editor.execute_keys("q").unwrap();
    assert!(
        editor.is_error_message(),
        "the 5rZ must have failed; got {:?}",
        editor.get_message()
    );
    assert_eq!(
        editor.get_buffer_text(),
        "ab\ntwo\n",
        "the dd after the failing command must be discarded"
    );

    // The same keys typed by hand are not an expansion, so the `dd` does run:
    // it is the discarding that is conditional, not the failure.
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("ab\ntwo\n");
    editor.execute_keys("5rZ").unwrap();
    assert!(editor.is_error_message());
    editor.execute_keys("dd").unwrap();
    assert_eq!(editor.get_buffer_text(), "two\n");
}

// ============================================================================
// :map! expansion, and @ through the same queue
// ============================================================================

/// 95107-95109: in text input mode the lhs is matched "as any part of text
/// entered", and the replacement acts as if it had been entered instead. The
/// classic use is leaving insert mode without reaching for <escape>.
#[test]
fn test_text_input_map_expands() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    // jk leaves insert mode.
    editor.execute_keys(":map! jk \x16\x1b\n").unwrap();
    editor.execute_keys("\n").unwrap();

    editor.execute_keys("ifoojk").unwrap();
    assert_eq!(editor.get_mode(), Mode::Command, "jk must have left insert");
    assert_eq!(editor.get_buffer_text(), "fooalpha\n");
}

/// 95089-95092: the same lhs can mean one thing in command mode and another in
/// text input mode, and each table is consulted only in its own mode.
#[test]
fn test_the_two_map_tables_apply_in_their_own_modes() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys(":map q dd\n").unwrap();
    editor.execute_keys(":map! q XY\n").unwrap();

    // Command mode takes the `:map` definition.
    editor.execute_keys("q").unwrap();
    assert_eq!(editor.get_buffer_text(), "two\nthree\n");

    // Text input mode takes the `:map!` one.
    editor.execute_keys("iq\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "XYtwo\nthree\n");
}

/// 95110-95111: "If any character in the input text is escaped using a
/// <control>-V character, that character shall not be part of a match to an
/// lhs."
#[test]
fn test_ctrl_v_suppresses_a_text_input_map() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    editor.execute_keys(":map! q XY\n").unwrap();

    editor.execute_keys("i\x16q\x1b").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "qalpha\n",
        "the quoted q must be inserted literally"
    );
}

/// vi.md 121171: an `@` buffer behaves "as if the contents of the named buffer
/// were entered as standard input" -- which is what makes its characters
/// subject to maps. They used to run through a separate path that no map could
/// see.
#[test]
fn test_buffer_execution_is_subject_to_maps() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys(":map q dd\n").unwrap();
    // Put a `q` into register a by yanking a line that holds one.
    editor.set_buffer_text("q\none\ntwo\nthree\n");
    editor.execute_keys("\"ayy").unwrap();
    editor.execute_keys("dd").unwrap();

    // @a enters `q`, which the map turns into `dd`.
    editor.execute_keys("@a").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "two\nthree\n",
        "the buffer's q must have been mapped to dd"
    );
}

/// vi.md 121176-121177: "If a count is specified, behave as if that count were
/// entered as user input *before* the characters from the @ buffer were
/// entered." Running the buffer `count` times is a different thing: with `dw`
/// in the register, `3@a` is `3dw` -- one command over three words -- not three
/// separate `dw`s, and the two differ as soon as a count interacts.
#[test]
fn test_buffer_execution_count_is_entered_before_the_buffer() {
    let mut editor = Editor::new_headless();
    // Register a holds `x`, which deletes one character.
    editor.set_buffer_text("x\nabcdef\n");
    editor.execute_keys("\"ayy").unwrap();
    editor.execute_keys("dd").unwrap();

    // `3@a` must mean `3x`, deleting three characters with one command, so a
    // single `u` puts all three back.
    editor.execute_keys("3@a").unwrap();
    assert_eq!(editor.get_buffer_text(), "def\n");
    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "abcdef\n",
        "3x is one command, so one undo restores all three characters"
    );
}

/// vi.md 121022-121024: "Commands (other than commands that enter text input
/// mode) executed as a result of map expansions, shall not change the value of
/// the last repeatable command." So `.` repeats what the user last did by
/// hand, not what a map did on their behalf.
#[test]
fn test_a_map_expansion_does_not_become_the_dot_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abcd\nefgh\nijkl\nmnop\n");
    editor.execute_keys(":map q dd\n").unwrap();

    // Do something repeatable by hand.
    editor.execute_keys("x").unwrap();
    assert_eq!(editor.get_buffer_text(), "bcd\nefgh\nijkl\nmnop\n");

    // Now run the map, whose `dd` must not become the `.` command.
    editor.execute_keys("q").unwrap();
    assert_eq!(editor.get_buffer_text(), "efgh\nijkl\nmnop\n");

    // `.` repeats the hand-typed `x`, not the map's `dd`.
    editor.execute_keys(".").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "fgh\nijkl\nmnop\n",
        ". must repeat the x, not the mapped dd"
    );
}

// ============================================================================
// Abbreviations
// ============================================================================

/// 94870-94873: a non-word character entered after a word character triggers a
/// check against the text input so far, and a match is replaced.
#[test]
fn test_abbreviation_expands() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");
    editor.execute_keys(":ab teh the\n").unwrap();

    editor.execute_keys("iteh \x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "the \n");
}

/// <ESC> triggers the check too, and the replacement is entered before the
/// mode change takes effect.
#[test]
fn test_abbreviation_triggered_by_escape() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");
    editor.execute_keys(":ab teh the\n").unwrap();

    editor.execute_keys("iteh\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "the\n");
    assert_eq!(editor.get_mode(), Mode::Command);
}

/// 96489-96495 works these through by name. `(p`, `p` and `((p` can fire
/// because the rules can produce those sets; `(` and `(pp` never can.
#[test]
fn test_the_abbreviations_posix_says_do_and_do_not_work() {
    for (lhs, typed, expect) in [
        ("(p", "i(p \x1b", "REPL \n"),
        ("p", "ip \x1b", "REPL \n"),
        ("((p", "i((p \x1b", "REPL \n"),
        // Cannot be produced by the rules, so must never fire.
        ("(", "i( \x1b", "( \n"),
        ("(pp", "i(pp \x1b", "(pp \n"),
    ] {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text("\n");
        editor.execute_keys(&format!(":ab {lhs} REPL\n")).unwrap();
        editor.execute_keys(typed).unwrap();
        assert_eq!(
            editor.get_buffer_text(),
            expect,
            "with `:ab {lhs} REPL`, typing {typed:?}"
        );
    }
}

/// 94870-94871: a `^V`-escaped trigger is text, not a trigger.
#[test]
fn test_ctrl_v_suppresses_an_abbreviation() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");
    editor.execute_keys(":ab teh the\n").unwrap();

    editor.execute_keys("iteh\x16 \x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "teh \n");
}

/// 96471-96476: the replacement is "logically pushed onto the terminal input
/// queue", so it is itself subject to map expansion -- not a plain text
/// substitution.
#[test]
fn test_an_abbreviation_replacement_is_itself_expanded() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");
    editor.execute_keys(":ab aa b\n").unwrap();
    editor.execute_keys(":map! b ZZ\n").unwrap();

    editor.execute_keys("iaa \x1b").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "ZZ \n",
        "the replacement `b` must then be mapped to ZZ"
    );
}

/// An abbreviation whose replacement begins with its own left-hand side must
/// not re-trigger on itself. POSIX does not say this; it is the map prefix rule
/// (95120-95121) applied by analogy, and without it `:ab foo foo` hangs.
#[test]
fn test_a_self_referential_abbreviation_does_not_loop() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");
    editor.execute_keys(":ab foo foo\n").unwrap();

    editor.execute_keys("ifoo \x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "foo \n");
}

/// 96496-96509. Historical vi expanded abbreviations on the colon line, with
/// two results POSIX calls out as "not permitted ... because they clearly
/// violate the expectations of the user": `:ab foo bar` then `:ab foo baz`
/// registering baz for *bar*, and `:una foo2` deleting foo1.
#[test]
fn test_the_colon_line_behaviours_posix_forbids() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");

    // Redefining `foo` must redefine `foo`, not define an entry for `bar`.
    editor.execute_keys(":ab foo bar\n").unwrap();
    editor.execute_keys(":ab foo baz\n").unwrap();
    editor.execute_keys("ifoo \x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "baz \n");

    // `:una foo2` must delete foo2, leaving foo1 alone.
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");
    editor.execute_keys(":ab foo1 bar\n").unwrap();
    editor.execute_keys(":ab foo2 bar\n").unwrap();
    editor.execute_keys(":una foo2\n").unwrap();
    assert!(!editor.is_error_message(), "{:?}", editor.get_message());
    editor.execute_keys(":una foo1\n").unwrap();
    assert!(
        !editor.is_error_message(),
        "foo1 must still exist; got {:?}",
        editor.get_message()
    );
}

/// 95443-95445: "commands resulting from buffer executions and mapped character
/// expansions, are considered single commands" for undo. So one `u` reverses
/// everything a map did, however many commands its replacement contained.
#[test]
fn test_a_map_expansion_undoes_as_one_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\nfour\n");
    editor.execute_keys(":map q dddd\n").unwrap();

    editor.execute_keys("q").unwrap();
    assert_eq!(editor.get_buffer_text(), "three\nfour\n");

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "one\ntwo\nthree\nfour\n",
        "one u must reverse both deletes the map performed"
    );
}

/// The nesting case the depth counter exists for: a mapped command that opens
/// its own undo group. A bare flag let the inner `end_group` close the outer
/// one, leaving half the expansion outside the group.
#[test]
fn test_a_mapped_change_command_undoes_as_one_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha bravo\n");
    // `cw` opens a group for the change and the insert session it starts; the
    // trailing `x` is a second command inside the same expansion.
    editor.execute_keys(":map q cwZZ\x16\x1bx\n").unwrap();
    editor.execute_keys("\n").unwrap();

    editor.execute_keys("q").unwrap();
    assert_ne!(editor.get_buffer_text(), "alpha bravo\n", "the map ran");

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "alpha bravo\n",
        "one u must reverse the whole expansion, not just its last command"
    );
}

/// `^T` returns to where the last tag jump started, the companion to `^]`.
///
/// Not POSIX -- the only non-POSIX command-mode key in the editor. vi.md
/// 121838-121840 gives `^T` a meaning in *text input* mode only, where it
/// shifts the autoindent; that is untouched, and
/// `test_ctrl_t_indents_at_cursor_to_shiftwidth_boundary` and
/// `test_ctrl_t_is_recorded_in_the_insert_session` fail if it stops being.
#[test]
fn test_ctrl_t_pops_the_tag_stack() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();
    editor.execute_keys("2G").unwrap();

    editor.execute_keys(":tag local\n").unwrap();
    assert_eq!(editor.get_cursor().line, 3);

    editor.execute_keys("\x14").unwrap(); // ^T
    assert_eq!(editor.get_cursor().line, 2, "^T returned to the origin");

    // On an empty stack it reports rather than doing nothing silently.
    editor.execute_keys("\x14").unwrap();
    assert!(editor.is_error_message());
    assert!(editor
        .get_message()
        .unwrap_or_default()
        .contains("tag stack empty"));
}

/// ex.md 96616-96617: a map defined in terms of itself loops, and POSIX
/// "requires conformance to historical practice, and that such loops be
/// interruptible". The escape is therefore a signal, not a depth cap -- a cap
/// would refuse a mapping the spec says must work.
///
/// What this pins is that the drain polls the interrupt at all and abandons
/// the expansion when it is set. It cannot deliver the signal mid-loop from one
/// thread, so it arms the flag first; an interactive interrupt sets the same
/// flag asynchronously and reaches the same poll. Removing the poll makes this
/// test hang rather than fail, which is the honest shape of the bug.
///
/// The editor is given its own interrupt flag rather than the process-wide
/// SIGINT one. The drain consumes the flag with a `swap(false)`, and every
/// test in this binary drains on every keystroke, so arming the global raced
/// the whole suite for it -- this test failed about one run in seven, and
/// always with the expansion having run because some other test's keystroke
/// read the flag first.
#[test]
fn test_sigint_abandons_a_map_expansion() {
    use std::sync::atomic::{AtomicBool, Ordering};

    // `&'static` like the real signal flag, and named only here, so no other
    // test can reach it to consume what this one arms.
    static INTERRUPT: AtomicBool = AtomicBool::new(false);
    let interrupt: &'static AtomicBool = &INTERRUPT;

    let mut editor = Editor::new_headless();
    editor.set_interrupt_source(interrupt);
    editor.set_buffer_text("alpha\nbravo\n");
    editor.execute_keys(":map q dd\n").unwrap();

    interrupt.store(true, Ordering::SeqCst);
    editor.execute_keys("q").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "alpha\nbravo\n",
        "an armed interrupt must abandon the expansion before it runs"
    );

    // The flag is consumed, so the editor works normally again.
    assert!(!interrupt.load(Ordering::SeqCst));
    editor.execute_keys("q").unwrap();
    assert_eq!(editor.get_buffer_text(), "bravo\n");
}

/// An editor watches the process SIGINT flag unless told otherwise, so the
/// indirection above cannot quietly disconnect a real vi from its signal.
#[test]
fn test_editor_watches_the_process_sigint_flag_by_default() {
    let editor = Editor::new_headless();
    assert!(
        std::ptr::eq(editor.interrupt_source(), &vi_rs::signals::SIGINT_RECEIVED),
        "the default interrupt source must be the flag the signal handler writes"
    );
}
