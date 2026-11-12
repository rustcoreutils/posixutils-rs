//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Headless vi tests: `:m`/`:t` addresses, counts and registers, undo of
//! every change, motion classification, and other conformance items.

use super::*;

// ============================================================================
// `:m` and `:t` take an address, not a bare line number
// ============================================================================
//
// POSIX (ex, `copy`/`move`) gives the destination as an address, so `$`, `.`,
// `.+2`, a mark and a search are all valid. It was parsed with a plain integer
// parse, so everything but a decimal literal failed outright -- `:1m$`
// answered "invalid line number". Cross-checked against /usr/bin/ex.

#[test]
fn test_ex_move_to_last_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("X1\na\nX2\nb\n");
    editor.execute_keys(":1m$\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "a\nX2\nb\nX1\n");
}

#[test]
fn test_ex_copy_to_last_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("X1\na\n");
    editor.execute_keys(":1t$\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "X1\na\nX1\n");
}

#[test]
fn test_ex_move_to_zero_still_works() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\nb\nc\n");
    editor.execute_keys(":3m0\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "c\na\nb\n");
}

#[test]
fn test_ex_move_to_relative_address() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\nb\nc\nd\n");
    editor.execute_keys(":1\n").unwrap();
    editor.execute_keys(":1m.+2\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "b\nc\na\nd\n");
}

#[test]
fn test_ex_copy_to_search_address() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys(":1t/three/\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "one\ntwo\nthree\none\n");
}

/// The other half of the same rule: vi begins on the first line.
#[test]
fn test_vi_starts_on_the_first_line() {
    let dir = plib::tmp::tempdir().unwrap();
    let path = dir.path().join("f.txt");
    std::fs::write(&path, "one\ntwo\nthree\n").unwrap();

    let mut editor = Editor::new_headless();
    editor.open(path.to_str().unwrap()).unwrap();
    assert_eq!(editor.get_cursor().line, 1);
}

// ============================================================================
// Counts and registers reach the operator
// ============================================================================
//
// The parser computes `cmd.count` as count1*count2, which POSIX requires
// ("2d3w" deletes six words), but set `motion.count` to count2 alone -- and
// the operator+motion path reads only the motion's count. The doubled forms
// (`dd`, `yy`, `cc`) read `cmd.count` and were always right, which is why the
// discrepancy went unnoticed.

#[test]
fn test_count_before_operator_applies_to_the_motion() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two three four\n");
    editor.execute_keys("2dw").unwrap();
    assert_eq!(editor.get_buffer_text(), "three four\n");
}

#[test]
fn test_counts_before_and_after_operator_multiply() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a b c d e f g\n");
    editor.execute_keys("2d3w").unwrap();
    assert_eq!(editor.get_buffer_text(), "g\n");
}

#[test]
fn test_count_before_operator_applies_to_yank() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two three\n");
    editor.execute_keys("2yw").unwrap();
    assert_eq!(
        editor.get_unnamed_register().map(|r| r.text.as_str()),
        Some("one two ")
    );
}

/// `p` and `P` dropped `cmd.register` on the floor, so `"ap` always pasted
/// the unnamed register. `put_after` already accepted a register.
#[test]
fn test_put_uses_the_named_register() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\nbeta\n");
    editor.execute_keys("\"ayy").unwrap();
    editor.execute_keys("j").unwrap();
    editor.execute_keys("\"add").unwrap();
    editor.execute_keys("\"ap").unwrap();
    assert_eq!(editor.get_buffer_text(), "alpha\nbeta\n");
}

#[test]
fn test_put_before_uses_the_named_register() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\n");
    editor.execute_keys("\"byy").unwrap();
    editor.execute_keys("j\"bP").unwrap();
    assert_eq!(editor.get_buffer_text(), "one\none\ntwo\n");
}

/// A named register survives an intervening unnamed delete.
#[test]
fn test_named_register_is_not_clobbered_by_an_unnamed_delete() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("keep\ntrash\ntail\n");
    editor.execute_keys("\"kyy").unwrap();
    editor.execute_keys("jdd").unwrap();
    editor.execute_keys("\"kp").unwrap();
    assert!(
        editor.get_buffer_text().matches("keep").count() == 2,
        "expected the k register to still hold \"keep\": {:?}",
        editor.get_buffer_text()
    );
}

/// `x`, `X`, `D` and `Y` wrote straight to the small-delete or unnamed
/// register, bypassing `Registers::delete`/`yank` and so ignoring `"x`.
/// `yy` honoured it, which made `"ayy` and `"aY` disagree.
#[test]
fn test_delete_char_uses_the_named_register() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abc\n");
    editor.execute_keys("\"qx").unwrap();
    assert_eq!(editor.get_register('q').map(|r| r.text.as_str()), Some("a"));
}

#[test]
fn test_delete_char_before_uses_the_named_register() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abc\n");
    editor.execute_keys("$\"qX").unwrap();
    assert_eq!(editor.get_register('q').map(|r| r.text.as_str()), Some("b"));
}

#[test]
fn test_delete_to_end_of_line_uses_the_named_register() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\n");
    editor.execute_keys("ll\"qD").unwrap();
    assert_eq!(
        editor.get_register('q').map(|r| r.text.as_str()),
        Some("llo world")
    );
}

#[test]
fn test_yank_lines_shorthand_agrees_with_yy() {
    let mut a = Editor::new_headless();
    a.set_buffer_text("one\ntwo\n");
    a.execute_keys("\"qyy").unwrap();

    let mut b = Editor::new_headless();
    b.set_buffer_text("one\ntwo\n");
    b.execute_keys("\"qY").unwrap();

    assert_eq!(
        a.get_register('q').map(|r| r.text.as_str()),
        b.get_register('q').map(|r| r.text.as_str()),
        "`Y` must be `yy`"
    );
}

// ============================================================================
// Undo: every command that changes the buffer must be undoable
// ============================================================================
//
// `apply_inverse` rebuilds by position and length without checking what is
// actually there, so a mutation that records nothing does not merely fail to
// undo -- the next `u` pops an unrelated older change and destroys whatever
// now sits at its position (the warning on `record_removal`, audit #V19).
//
// `check_undoable` is therefore a check on the *absence* of that hazard: if a
// command changed the buffer, one `u` must put it back exactly.

/// Returns `Err(reason)` if `keys` changed the buffer and `u` did not restore
/// it, or if `keys` changed nothing at all (so the case proves nothing).
fn check_undoable(setup: &str, keys: &str) -> Result<(), String> {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(setup);
    let before = editor.get_buffer_text();

    editor
        .execute_keys(keys)
        .map_err(|e| format!("{:?} errored: {}", keys, e))?;
    // Leave insert mode, so the change is complete.
    let _ = editor.execute_keys("\x1b");
    let after = editor.get_buffer_text();
    if after == before {
        return Err(format!("{:?} did not change the buffer", keys));
    }

    editor
        .execute_keys("u")
        .map_err(|e| format!("{:?} then u errored: {}", keys, e))?;
    let undone = editor.get_buffer_text();
    if undone != before {
        return Err(format!(
            "{:?}: u gave {:?}, expected {:?} (after the edit it was {:?})",
            keys, undone, before, after
        ));
    }
    Ok(())
}

const UNDO_SETUP: &str = "alpha beta\ngamma delta\nepsilon zeta\n";

/// Run every case and report all the failures at once, so one broken command
/// does not hide the rest.
fn assert_all_undoable(cases: &[&str]) {
    let failures: Vec<String> = cases
        .iter()
        .filter_map(|keys| check_undoable(UNDO_SETUP, keys).err())
        .collect();
    assert!(
        failures.is_empty(),
        "{} of {} commands are not undoable:\n  {}",
        failures.len(),
        cases.len(),
        failures.join("\n  ")
    );
}

#[test]
fn test_undo_restores_character_edits() {
    assert_all_undoable(&[
        "x", "3x", "llX", "rZ", "~", "sZ", "cwZ", "CZ", "iZ", "AZ", "IZ", "aZ",
    ]);
}

#[test]
fn test_undo_restores_line_edits() {
    assert_all_undoable(&["dd", "2dd", "dw", "de", "cc", "S", "J", "oZ", "OZ", "D"]);
}

#[test]
fn test_undo_restores_put_and_shift() {
    // Only single commands here: `ddp` is two, and one `u` correctly undoes
    // just the put -- see test_undo_of_a_sequence_unwinds_one_command_at_a_time.
    assert_all_undoable(&["yyp", "yyP", "yy2p", ">>", "2>>"]);
}

/// `<` needs something to remove, so it gets its own indented setup.
#[test]
fn test_undo_restores_shift_left() {
    let setup = "\talpha\n\tbeta\n";
    for keys in ["<<", "2<<"] {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text(setup);
        editor.execute_keys(keys).unwrap();
        assert_ne!(
            editor.get_buffer_text(),
            setup,
            "{:?} changed nothing",
            keys
        );
        editor.execute_keys("u").unwrap();
        assert_eq!(editor.get_buffer_text(), setup, "{:?} did not undo", keys);
    }
}

/// POSIX `u` is its own inverse: the first undoes the last command, the
/// second puts it back. So after `dd` `p`, one `u` reverses the put and a
/// second replays it -- it does not keep walking back through the history.
#[test]
fn test_undo_reverses_only_the_last_command_and_is_its_own_inverse() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(UNDO_SETUP);
    editor.execute_keys("ddp").unwrap();
    let after_put = editor.get_buffer_text();
    assert_eq!(after_put, "gamma delta\nalpha beta\nepsilon zeta\n");

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "gamma delta\nepsilon zeta\n",
        "the first u must undo the put, not the whole sequence"
    );

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        after_put,
        "the second u must put it back"
    );
}

#[test]
fn test_undo_restores_ex_edits() {
    assert_all_undoable(&[
        ":2d\n",
        ":1,2d\n",
        ":s/alpha/ZZ/\n",
        ":1,2j\n",
        ":1t2\n",
        ":1m2\n",
        ":1,2>\n",
        ":g/a/s/a/Z/\n",
    ]);
}

/// `:pu` needs a register to put from, and `:<` something to unindent.
#[test]
fn test_undo_restores_ex_put_and_shift_left() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(UNDO_SETUP);
    editor.execute_keys("yy").unwrap();
    let before = editor.get_buffer_text();
    editor.execute_keys(":2pu\n").unwrap();
    assert_ne!(editor.get_buffer_text(), before);
    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text(), before);

    let indented = "\talpha\n\tbeta\n";
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(indented);
    editor.execute_keys(":1,2<\n").unwrap();
    assert_ne!(editor.get_buffer_text(), indented);
    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text(), indented);
}

#[test]
fn test_undo_restores_operator_and_motion_edits() {
    assert_all_undoable(&[
        "dw", "2dw", "d$", "de", "dj", "cwZ", "c$Z", "ceZ", "yyp", "3x",
        // These need the cursor off the very start of the buffer to have
        // anything to act on.
        "jdk", "ll2X", "lld0",
    ]);
}

#[test]
fn test_undo_restores_repeated_and_counted_edits() {
    assert_all_undoable(&["3rZ", "2~", "2dd", "3>>", "2J", "2sZ"]);
}

/// `.` after an insert used to re-enter insert mode without ever closing the
/// group it opened, so every later change joined that stale group and one `u`
/// reversed the rest of the session.
#[test]
fn test_undo_after_dot_repeat_reverses_only_the_last_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(UNDO_SETUP);
    editor.execute_keys("iX\x1b").unwrap();
    editor.execute_keys(".").unwrap();
    let after_repeat = editor.get_buffer_text();

    editor.execute_keys("dd").unwrap();
    assert_ne!(editor.get_buffer_text(), after_repeat);

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        after_repeat,
        "u after `.` must reverse the dd alone, not the whole session"
    );
}

/// The same hazard through an ex command.
#[test]
fn test_undo_after_ex_command_reverses_only_that_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(UNDO_SETUP);
    editor.execute_keys("iX\x1b").unwrap();
    let after_insert = editor.get_buffer_text();

    editor.execute_keys(":2d\n").unwrap();
    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text(), after_insert);
}

/// A global is one command: one `u` reverses every line it touched.
#[test]
fn test_undo_after_global_reverses_the_whole_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Xa\nXb\nXc\nd\n");
    editor.execute_keys(":g/X/d\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "d\n");
    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text(), "Xa\nXb\nXc\nd\n");
}

/// Generated sweep over complete single commands: whatever a command does to
/// the buffer, one `u` must put back exactly.
///
/// This is the standing check that a new mutation path cannot be added without
/// recording undo -- the failure mode being not a missing undo but a
/// *destructive* one, since `apply_inverse` rebuilds by position and length
/// without looking at what is there.
#[test]
fn test_generated_single_commands_are_undoable() {
    // Each entry is one complete command, so `u` reverses exactly it.
    const COMMANDS: &[&str] = &[
        "x",
        "2x",
        "X",
        "3X",
        "rZ",
        "2rZ",
        "~",
        "3~",
        "D",
        "J",
        "2J",
        "dd",
        "2dd",
        "3dd",
        "dw",
        "2dw",
        "de",
        "d$",
        "dj",
        "dk",
        "d0",
        "cwZ",
        "ceZ",
        "c$Z",
        "cc",
        "2cc",
        "sZ",
        "2sZ",
        "S",
        "iZ",
        "aZ",
        "IZ",
        "AZ",
        "oZ",
        "OZ",
        "CZ",
        ">>",
        "2>>",
        "<<",
        "p",
        "P",
        "2p",
        ":d\n",
        ":1,2d\n",
        ":j\n",
        ":s/a/Z/\n",
        ":s/a/Z/g\n",
        ":1t2\n",
        ":1m3\n",
        ":>\n",
        ":<\n",
        ":pu\n",
        ":g/a/s/a/Z/\n",
        ":v/a/s/e/Z/\n",
        ":1,3>\n",
    ];
    // Indented and varied, so `<<`, `p` and the shifts all have something to do.
    const SETUPS: &[&str] = &[
        "\talpha beta\n\tgamma delta\n\tepsilon zeta\neta theta\n",
        "one\ntwo\nthree\nfour\nfive\n",
        "  a\n\tb\n c\nd\n",
    ];

    let mut state: u64 = 0xDEAD_BEEF_CAFE_F00D;
    let mut next = || {
        state = state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (state >> 33) as usize
    };

    let mut failures = Vec::new();
    for _ in 0..3000 {
        let setup = SETUPS[next() % SETUPS.len()];
        let keys = COMMANDS[next() % COMMANDS.len()];
        // A cursor somewhere other than the very start, so the backward and
        // upward commands have something to act on.
        let lead = ["", "j", "jl", "jjll", "l", "G", "jj"][next() % 7];

        let mut editor = Editor::new_headless();
        editor.set_buffer_text(setup);
        // Fill the unnamed register so `p`/`P`/`:pu` are not no-ops.
        let _ = editor.execute_keys("yy");
        let _ = editor.execute_keys(lead);
        let before = editor.get_buffer_text();

        if editor.execute_keys(keys).is_err() {
            continue; // an invalid command here is not an undo failure
        }
        let _ = editor.execute_keys("\x1b");
        if editor.get_buffer_text() == before {
            continue; // nothing to undo
        }

        if editor.execute_keys("u").is_err() {
            failures.push(format!("{:?} after {:?}: u errored", keys, lead));
            continue;
        }
        if editor.get_buffer_text() != before {
            failures.push(format!(
                "{:?} after {:?} on {:?}: u gave {:?}, expected {:?}",
                keys,
                lead,
                setup,
                editor.get_buffer_text(),
                before
            ));
        }
    }
    failures.dedup();
    assert!(
        failures.is_empty(),
        "{} commands did not undo cleanly:\n  {}",
        failures.len(),
        failures
            .iter()
            .take(12)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n  ")
    );
}

// ============================================================================
// Motion classification: inclusive, exclusive, linewise
// ============================================================================
//
// An operator's region depends on how the motion is classified. The motions
// computed `linewise` correctly and `execute_motion_get_pos` threw it away by
// returning only the position, and every operator+motion path then hardcoded
// a character-mode range -- so `dj` deleted from the cursor column on one line
// to the cursor column on the next instead of both whole lines. Nothing
// tracked inclusivity at all outside a special case for `cw`, so every
// inclusive motion came up one character short.

#[test]
fn test_delete_to_end_of_word_is_inclusive() {
    // POSIX (vi, "Move to End-of-Word"): the region includes the last
    // character of the word.
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\n");
    editor.execute_keys("de").unwrap();
    assert_eq!(editor.get_buffer_text(), " two\n");
}

#[test]
fn test_delete_to_end_of_line_is_inclusive() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\n");
    editor.execute_keys("d$").unwrap();
    assert_eq!(editor.get_buffer_text(), "\n");
}

#[test]
fn test_yank_to_end_of_line_is_inclusive() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello\n");
    editor.execute_keys("y$").unwrap();
    assert_eq!(
        editor.get_unnamed_register().map(|r| r.text.as_str()),
        Some("hello")
    );
}

#[test]
fn test_find_char_forward_is_inclusive() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\n");
    editor.execute_keys("dfo").unwrap();
    assert_eq!(editor.get_buffer_text(), " world\n");
}

#[test]
fn test_till_char_forward_is_inclusive_of_the_char_before() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\n");
    editor.execute_keys("dto").unwrap();
    assert_eq!(editor.get_buffer_text(), "o world\n");
}

/// `F` and `T` search backwards, so the character under the cursor survives.
#[test]
fn test_find_char_backward_is_exclusive() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\n");
    editor.execute_keys("$dFo").unwrap();
    assert_eq!(editor.get_buffer_text(), "hello wd\n");
}

#[test]
fn test_match_bracket_is_inclusive() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("(abc)x\n");
    editor.execute_keys("d%").unwrap();
    assert_eq!(editor.get_buffer_text(), "x\n");
}

#[test]
fn test_delete_down_is_linewise() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys("ldj").unwrap();
    assert_eq!(editor.get_buffer_text(), "three\n");
}

#[test]
fn test_delete_up_is_linewise() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys("jjldk").unwrap();
    assert_eq!(editor.get_buffer_text(), "one\n");
}

#[test]
fn test_delete_to_line_is_linewise() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys("jldG").unwrap();
    assert_eq!(editor.get_buffer_text(), "one\n");
}

#[test]
fn test_yank_down_is_linewise() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys("lyj").unwrap();
    assert_eq!(
        editor.get_unnamed_register().map(|r| r.linewise),
        Some(true),
        "a linewise yank must be stored linewise, so `p` puts whole lines"
    );
}

/// POSIX (vi, "Change"): `cw` on a non-blank behaves as `ce`, leaving the
/// blanks after the word -- but only on a non-blank. On a blank it is an
/// ordinary `w`.
#[test]
fn test_change_word_on_a_blank_is_not_change_to_end_of_word() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a   bcd efg\n");
    // On a blank, `cw` is an ordinary `w`: the region runs to the start of the
    // next word, so all three blanks go. The `ce` substitution would instead
    // have consumed "bcd".
    editor.execute_keys("lcwZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "aZbcd efg\n");
}

#[test]
fn test_change_word_on_a_non_blank_keeps_the_following_blanks() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one   two\n");
    editor.execute_keys("cwZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "Z   two\n");
}

/// Every motion the parser accepts after an operator must actually be
/// dispatched. `execute_motion` fell through to `None` for a dozen of them,
/// so `dH`, `d+`, `d'a` and friends silently did nothing at all -- the worst
/// outcome, since the user sees no diagnostic.
#[test]
fn test_every_parseable_motion_works_as_an_operator_target() {
    let setup = "one\ntwo\nthree\nfour\nfive\nsix\n";
    let cases: &[(&str, &str)] = &[
        ("dH", "from the top of the screen"),
        ("dL", "to the bottom of the screen"),
        ("dM", "to the middle of the screen"),
        ("d+", "to the next line"),
        ("d-", "to the previous line"),
        ("d_", "the current line"),
        ("d'a", "to a marked line"),
        ("d`a", "to a marked position"),
        ("d|", "to a column"),
    ];

    let mut silent = Vec::new();
    for (keys, what) in cases {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text(setup);
        // Sit in the middle and set mark `a` on the first line, so backward
        // and mark motions have somewhere to go.
        editor.execute_keys("ma").unwrap();
        editor.execute_keys("jjl").unwrap();

        let before = editor.get_buffer_text();
        let outcome = editor.execute_keys(keys);
        if outcome.is_ok() && editor.get_buffer_text() == before {
            silent.push(format!("{:?} ({}) did nothing", keys, what));
        }
    }
    assert!(
        silent.is_empty(),
        "{} motions are silent no-ops after an operator:\n  {}",
        silent.len(),
        silent.join("\n  ")
    );
}

/// The newly dispatched motions must produce the *right* region, not merely
/// a non-empty one.
#[test]
fn test_line_relative_motions_are_linewise_as_operator_targets() {
    let setup = "one\ntwo\nthree\nfour\n";

    let mut editor = Editor::new_headless();
    editor.set_buffer_text(setup);
    editor.execute_keys("jl").unwrap();
    editor.execute_keys("d+").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "one\nfour\n",
        "`d+` deletes the current line and the next, whole"
    );

    let mut editor = Editor::new_headless();
    editor.set_buffer_text(setup);
    editor.execute_keys("jjl").unwrap();
    editor.execute_keys("d-").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "one\nfour\n",
        "`d-` deletes the current line and the previous, whole"
    );

    let mut editor = Editor::new_headless();
    editor.set_buffer_text(setup);
    editor.execute_keys("jl").unwrap();
    editor.execute_keys("d_").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "one\nthree\nfour\n",
        "`d_` deletes just the current line"
    );
}

#[test]
fn test_mark_motions_as_operator_targets() {
    let setup = "one\ntwo\nthree\nfour\n";

    // `'a` addresses the line, so the region is whole lines.
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(setup);
    editor.execute_keys("ma").unwrap();
    editor.execute_keys("jj").unwrap();
    editor.execute_keys("d'a").unwrap();
    assert_eq!(editor.get_buffer_text(), "four\n");

    // A backtick mark addresses the character, so the region is exclusive.
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abcdef\n");
    editor.execute_keys("ll").unwrap();
    editor.execute_keys("ma").unwrap();
    editor.execute_keys("$").unwrap();
    editor.execute_keys("d`a").unwrap();
    assert_eq!(editor.get_buffer_text(), "abf\n");
}

/// An unset mark is an error, not a silent no-op on the whole buffer.
#[test]
fn test_operator_with_an_unset_mark_changes_nothing() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\n");
    let _ = editor.execute_keys("d'z");
    assert_eq!(editor.get_buffer_text(), "one\ntwo\n");
}

/// Filtering a buffer through a command writes all of the input before
/// reading any of the output. Once the child's output fills the pipe buffer
/// (~64 KB) it blocks writing, while the editor is still blocked writing its
/// input -- both wait for the other and the editor hangs.
///
/// Run on a worker thread so a regression fails the suite instead of hanging
/// it.
#[test]
fn test_shell_filter_does_not_deadlock_on_a_large_buffer() {
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        // Comfortably past a pipe buffer in both directions.
        let text: String = (0..20_000)
            .map(|i| format!("line {} of filler text\n", i))
            .collect();
        let expected_lines = text.lines().count();

        let mut editor = Editor::new_headless();
        editor.set_buffer_text(&text);
        let r = editor
            .execute_keys(":%!cat\n")
            .map(|()| (editor.get_buffer_text().lines().count(), expected_lines));
        let _ = tx.send(r);
    });

    match rx.recv_timeout(std::time::Duration::from_secs(20)) {
        Ok(Ok((got, want))) => assert_eq!(got, want, "cat must round-trip every line"),
        Ok(Err(e)) => panic!("filter failed: {}", e),
        Err(_) => panic!("`:%!cat` deadlocked on a buffer larger than the pipe buffer"),
    }
}

/// A filter that exits without reading all its input gives the writer EPIPE.
/// That is the command's choice, not an editor error.
#[test]
fn test_shell_filter_tolerates_a_command_that_stops_reading() {
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let text: String = (0..20_000).map(|i| format!("line {}\n", i)).collect();
        let mut editor = Editor::new_headless();
        editor.set_buffer_text(&text);
        let r = editor
            .execute_keys(":%!head -1\n")
            .map(|()| editor.get_buffer_text());
        let _ = tx.send(r);
    });

    match rx.recv_timeout(std::time::Duration::from_secs(20)) {
        Ok(Ok(text)) => assert_eq!(text.trim_end(), "line 0"),
        Ok(Err(e)) => panic!("a filter that stops reading must not be an error: {}", e),
        Err(_) => panic!("`:%!head -1` deadlocked"),
    }
}

// ============================================================================
// Remaining conformance items
// ============================================================================

/// POSIX (ex, `set`): "set [option[=[value]] ...]" -- the arguments are
/// <blank>-separated, which is how an ordinary .exrc line is written.
#[test]
fn test_set_accepts_several_options_on_one_line() {
    let mut editor = Editor::new_headless();
    editor.execute_keys(":set ai number\n").unwrap();
    assert!(editor.options().autoindent, "ai should be on");
    assert!(editor.options().number, "number should be on");
    assert!(
        !editor.is_error_message(),
        "unexpected error: {:?}",
        editor.get_message()
    );
}

#[test]
fn test_set_accepts_several_options_with_values() {
    let mut editor = Editor::new_headless();
    editor.execute_keys(":set sw=4 ts=2 ai\n").unwrap();
    assert_eq!(editor.options().shiftwidth, 4);
    assert_eq!(editor.options().tabstop, 2);
    assert!(editor.options().autoindent);
}

/// `r` with a count replaces that many characters, each with the same one.
/// It called the single-character replace `count` times without moving, so
/// `3rx` rewrote the same character three times.
#[test]
fn test_replace_char_with_a_count_replaces_that_many() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abcdef\n");
    editor.execute_keys("3rZ").unwrap();
    assert_eq!(editor.get_buffer_text(), "ZZZdef\n");
}

/// POSIX: `r` with a count larger than the characters left on the line is an
/// error, and the line is unchanged.
#[test]
fn test_replace_char_past_end_of_line_changes_nothing() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("ab\n");
    let _ = editor.execute_keys("5rZ");
    assert_eq!(editor.get_buffer_text(), "ab\n");
}

/// `^` moves to the first non-blank; on an all-blank line there is none, so
/// the cursor goes to the last character rather than to column 0.
#[test]
fn test_first_non_blank_on_an_all_blank_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("    \n");
    editor.execute_keys("$").unwrap();
    editor.execute_keys("^").unwrap();
    assert_eq!(
        editor.get_cursor().column,
        3,
        "an all-blank line has no non-blank, so `^` stays on the last character"
    );
}

/// `#` in a shell command expands to the alternate file. `open` captured the
/// "previous" current file *after* `set_current_file` had already replaced
/// it, so `#` expanded to the file just opened -- the same as `%`.
#[test]
fn test_alternate_file_expands_to_the_previous_file() {
    let dir = plib::tmp::tempdir().unwrap();
    let first = dir.path().join("first.txt");
    let second = dir.path().join("second.txt");
    std::fs::write(&first, "one\n").unwrap();
    std::fs::write(&second, "two\n").unwrap();

    let mut editor = Editor::new_headless();
    editor.open(first.to_str().unwrap()).unwrap();
    editor.open(second.to_str().unwrap()).unwrap();

    editor.execute_keys(":r !echo #\n").unwrap();
    let text = editor.get_buffer_text();
    assert!(
        text.contains("first.txt"),
        "`#` must be the alternate (previous) file, got {:?}",
        text
    );
    assert!(
        !text.contains("second.txt"),
        "`#` must not be the current file, got {:?}",
        text
    );
}

/// Ctrl-^ edits the alternate file, which discards the buffer. POSIX requires
/// the same warning as `:e`; it opened unconditionally and the unsaved work
/// went with it.
#[test]
fn test_ctrl_caret_warns_before_discarding_a_modified_buffer() {
    let dir = plib::tmp::tempdir().unwrap();
    let first = dir.path().join("first.txt");
    let second = dir.path().join("second.txt");
    std::fs::write(&first, "one\n").unwrap();
    std::fs::write(&second, "two\n").unwrap();

    let mut editor = Editor::new_headless();
    editor.open(first.to_str().unwrap()).unwrap();
    editor.open(second.to_str().unwrap()).unwrap();
    editor.execute_keys("iEDITED\x1b").unwrap();

    editor.execute_keys("\x1e").unwrap(); // Ctrl-^
    assert!(
        editor.get_buffer_text().contains("EDITED"),
        "the modified buffer must survive; got {:?}",
        editor.get_buffer_text()
    );
    assert!(
        editor.is_error_message(),
        "expected a warning, got {:?}",
        editor.get_message()
    );
}

/// POSIX (vi, "Delete"): "If the motion command is `w` or `W`, and the last
/// word on the line is being deleted, the region shall end at the last
/// character of the line."
///
/// `w` on the last word has nowhere to advance to within the line, so the
/// region came out empty and `dw` deleted nothing at all.
#[test]
fn test_delete_word_on_the_last_word_of_a_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\n");
    editor.execute_keys("wdw").unwrap();
    assert_eq!(editor.get_buffer_text(), "one \n");
}

/// The same rule keeps `dw` from joining lines.
#[test]
fn test_delete_word_does_not_join_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\nthree four\n");
    editor.execute_keys("wdw").unwrap();
    assert_eq!(editor.get_buffer_text(), "one \nthree four\n");
}

#[test]
fn test_yank_word_on_the_last_word_of_a_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\nthree\n");
    editor.execute_keys("wyw").unwrap();
    assert_eq!(
        editor.get_unnamed_register().map(|r| r.text.as_str()),
        Some("two")
    );
}

// ============================================================================
// Code-review findings
// ============================================================================

/// POSIX (vi, "Delete"): the `w`/`W` end-of-line rule applies when *the last
/// word moved over* ends a line -- not whenever the motion happens to cross
/// one. With a count the motion may legitimately span lines.
#[test]
fn test_delete_word_with_a_count_may_cross_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\nthree four\n");
    editor.execute_keys("d3w").unwrap();
    assert_eq!(editor.get_buffer_text(), "four\n");
}

#[test]
fn test_yank_word_with_a_count_may_cross_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\nthree four\n");
    editor.execute_keys("y3w").unwrap();
    assert_eq!(
        editor.get_unnamed_register().map(|r| r.text.as_str()),
        Some("one two\nthree ")
    );
}

/// ...but a count whose final step still ends a line keeps the rule.
#[test]
fn test_delete_word_with_a_count_still_stops_at_end_of_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\nthree four\n");
    editor.execute_keys("d2w").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "\nthree four\n",
        "the second word ends line 1, so the region stops there"
    );
}

/// `c` with a line-wise motion has the same hazard as `cc`: `change_lines`
/// empties the first line rather than removing it, so a linewise delete
/// record makes `u` restore the lines *and* leave the emptied one behind.
#[test]
fn test_undo_after_change_with_a_linewise_motion() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aaa\nbbb\nccc\n");
    editor.execute_keys("cjX\x1b").unwrap();
    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text(), "aaa\nbbb\nccc\n");
}

/// The shift operator's undo snapshot must cover the lines the *motion*
/// selects, not `count` lines from the cursor -- otherwise `u` reverts only
/// part of the change and leaves a state that is neither before nor after.
#[test]
fn test_undo_after_shift_with_a_motion() {
    for keys in [">j", ">G", "j>k"] {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text("aaa\nbbb\nccc\n");
        editor.execute_keys(keys).unwrap();
        assert_ne!(editor.get_buffer_text(), "aaa\nbbb\nccc\n");
        editor.execute_keys("u").unwrap();
        assert_eq!(
            editor.get_buffer_text(),
            "aaa\nbbb\nccc\n",
            "{:?} did not fully undo",
            keys
        );
    }
}

/// An inclusive motion that runs *backwards* has to include the character
/// under the cursor: character ranges are end-exclusive, so extending the end
/// only helps when the motion went forward.
#[test]
fn test_match_bracket_is_inclusive_backwards_too() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("(abc)\n");
    editor.execute_keys("$d%").unwrap();
    assert_eq!(editor.get_buffer_text(), "\n");
}

#[test]
fn test_find_char_backward_inclusive_forms_keep_the_cursor_char() {
    // `F`/`T` are exclusive, so the cursor's character survives -- pinned so
    // the backward-inclusive fix does not change them.
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\n");
    editor.execute_keys("$dFo").unwrap();
    assert_eq!(editor.get_buffer_text(), "hello wd\n");
}

/// `^` anchors to the start of the line, so a search that resumes mid-line
/// must not let it match there.
#[test]
fn test_search_does_not_reanchor_caret_mid_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abc\nzzz\n");
    let r = editor.execute_keys("/^b\n");
    assert!(
        r.is_err() || editor.is_error_message(),
        "`^b` must not match mid-line; cursor went to line {} col {}",
        editor.get_cursor().line,
        editor.get_cursor().column
    );
}

#[test]
fn test_backward_search_does_not_reanchor_caret_mid_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abc\nzzz\n");
    editor.execute_keys("G$").unwrap();
    let r = editor.execute_keys("?^b\n");
    assert!(
        r.is_err() || editor.is_error_message(),
        "`^b` must not match mid-line; cursor went to line {} col {}",
        editor.get_cursor().line,
        editor.get_cursor().column
    );
}

/// A display column that falls *inside* a tab belongs to the tab.
#[test]
fn test_pipe_column_inside_a_tab_lands_on_the_tab() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\tbc\n");
    // With tabstop 8 the tab spans display columns 2-8, so 5 is inside it.
    editor.execute_keys("5|").unwrap();
    assert_eq!(
        editor.get_cursor().column,
        1,
        "column 5 is the tab, at byte 1"
    );
}
