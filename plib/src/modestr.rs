//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use libc::{S_IRWXG, S_IRWXO, S_IRWXU, S_ISGID, S_ISUID, S_ISVTX, S_IXGRP, S_IXOTH, S_IXUSR};

#[derive(PartialEq, Debug, Default)]
pub enum ChmodActionOp {
    Add,
    Remove,
    #[default]
    Set,
}

#[derive(Debug, Default)]
pub struct ChmodAction {
    pub op: ChmodActionOp,

    pub copy_user: bool,
    pub copy_group: bool,
    pub copy_others: bool,

    pub read: bool,
    pub write: bool,
    pub execute: bool,
    pub execute_dir: bool,
    pub setuid: bool,
    pub sticky: bool,

    dirty: bool,
}

#[derive(Debug, Default)]
pub struct ChmodClause {
    // wholist
    pub user: bool,
    pub group: bool,
    pub others: bool,

    // actionlist
    pub actions: Vec<ChmodAction>,

    dirty: bool,
}

#[derive(Debug, Default)]
pub struct ChmodSymbolic {
    pub clauses: Vec<ChmodClause>,
}

#[derive(Debug)]
pub enum ChmodMode {
    /// (Numeric value, number of digits in octal notation)
    Absolute(u32, u32),
    Symbolic(ChmodSymbolic),
}

#[derive(Debug, Default)]
enum ParseState {
    #[default]
    Wholist,
    Actionlist,
    ListOrCopy,
    PermCopy,
    PermList,
    NextClause,
}

pub fn parse(mode: &str) -> Result<ChmodMode, String> {
    if let Ok(m) = u32::from_str_radix(mode, 8) {
        return Ok(ChmodMode::Absolute(m, mode.len() as u32));
    }

    let mut done_with_char;
    let mut state = ParseState::default();
    let mut symbolic = ChmodSymbolic::default();
    let mut clause = ChmodClause::default();
    let mut action = ChmodAction::default();

    for c in mode.chars() {
        done_with_char = false;
        while !done_with_char {
            match state {
                ParseState::Wholist => {
                    done_with_char = true;
                    clause.dirty = true;
                    match c {
                        'u' => clause.user = true,
                        'g' => clause.group = true,
                        'o' => clause.others = true,
                        'a' => {
                            clause.user = true;
                            clause.group = true;
                            clause.others = true;
                        }
                        _ => {
                            state = ParseState::Actionlist;
                            done_with_char = false;
                            clause.dirty = false;
                        }
                    }
                }

                ParseState::Actionlist => {
                    done_with_char = true;
                    state = ParseState::ListOrCopy;
                    action.dirty = true;
                    match c {
                        '+' => action.op = ChmodActionOp::Add,
                        '-' => action.op = ChmodActionOp::Remove,
                        '=' => action.op = ChmodActionOp::Set,
                        _ => {
                            action.dirty = false;
                            done_with_char = false;
                            symbolic.clauses.push(clause);
                            clause = ChmodClause::default();
                            state = ParseState::NextClause;
                        }
                    }
                }

                ParseState::ListOrCopy => match c {
                    'u' | 'g' | 'o' => state = ParseState::PermCopy,
                    _ => state = ParseState::PermList,
                },

                ParseState::PermCopy => {
                    done_with_char = true;
                    match c {
                        'u' => action.copy_user = true,
                        'g' => action.copy_group = true,
                        'o' => action.copy_others = true,
                        _ => {
                            done_with_char = false;
                            clause.actions.push(action);
                            clause.dirty = true;
                            action = ChmodAction::default();
                            state = ParseState::Actionlist;
                        }
                    }
                }

                ParseState::PermList => {
                    done_with_char = true;
                    match c {
                        'r' => action.read = true,
                        'w' => action.write = true,
                        'x' => action.execute = true,
                        'X' => action.execute_dir = true,
                        's' => action.setuid = true,
                        't' => action.sticky = true,
                        _ => {
                            done_with_char = false;
                            clause.actions.push(action);
                            clause.dirty = true;
                            action = ChmodAction::default();
                            state = ParseState::Actionlist;
                        }
                    }
                }

                ParseState::NextClause => {
                    if c != ',' {
                        return Err("invalid mode string".to_string());
                    }
                    done_with_char = true;
                    state = ParseState::Wholist;
                }
            }
        }
    }

    if action.dirty {
        clause.actions.push(action);
        clause.dirty = true;
    }
    if clause.dirty {
        symbolic.clauses.push(clause);
    }

    Ok(ChmodMode::Symbolic(symbolic))
}

// apply symbolic mutations to the given file at path
pub fn mutate(init_mode: u32, is_dir: bool, symbolic: &ChmodSymbolic) -> u32 {
    let mut user = init_mode & S_IRWXU as u32;
    let mut group = init_mode & S_IRWXG as u32;
    let mut others = init_mode & S_IRWXO as u32;
    let mut special = init_mode & (S_ISUID | S_ISGID | S_ISVTX) as u32;

    let mut cached_umask = None;

    let mut get_umask = || -> u32 {
        match cached_umask {
            Some(m) => m,
            None => {
                // WARNING:
                // Potential umask race-condition. Tests that exercise this code path must be
                // located in tree/tree-tests-umask.rs so that they would not be run in parallel.

                let m = unsafe { libc::umask(0) };
                unsafe { libc::umask(m) }; // Immediately revert

                let mask = m as u32; // Cast for macOS
                cached_umask = Some(mask);
                mask
            }
        }
    };

    // apply each clause
    for clause in &symbolic.clauses {
        let who_is_not_specified = !(clause.user || clause.group || clause.others);

        // apply each action
        for action in &clause.actions {
            let mut rwx = 0;
            if action.read {
                rwx |= 0b100;
            }
            if action.write {
                rwx |= 0b010;
            }
            if action.execute {
                rwx |= 0b001;
            }

            // Specification says:
            // "if the current (unmodified) file mode bits have at least one of the execute bits"
            //
            // Upon testing the GNU chmod implementation, "current" here does not mean the initial
            // mode bits, but the mode bits built by the previous clauses.
            let has_any_exec_bits =
                ((user | group | others) & (S_IXUSR | S_IXGRP | S_IXOTH) as u32) != 0;

            match action.op {
                // add bits to the mode
                ChmodActionOp::Add => {
                    if clause.user {
                        user |= rwx << 6;
                    }
                    if clause.group {
                        group |= rwx << 3;
                    }
                    if clause.others {
                        others |= rwx;
                    }

                    if who_is_not_specified {
                        let umask = get_umask();

                        user |= (rwx << 6) & !umask;
                        group |= (rwx << 3) & !umask;
                        others |= rwx & !umask;
                    }

                    if action.setuid {
                        // If "who" is missing, set both `S_ISUID` and `S_ISGID`
                        if clause.user || who_is_not_specified {
                            special |= S_ISUID as u32;
                        }
                        if clause.group || who_is_not_specified {
                            special |= S_ISGID as u32;
                        }
                    }
                }

                // remove bits from the mode
                ChmodActionOp::Remove => {
                    if clause.user {
                        user &= !(rwx << 6);
                    }
                    if clause.group {
                        group &= !(rwx << 3);
                    }
                    if clause.others {
                        others &= !rwx;
                    }

                    if who_is_not_specified {
                        let umask = get_umask();

                        user &= !(rwx << 6) & !umask;
                        group &= !(rwx << 3) & !umask;
                        others &= !rwx & !umask;
                    }
                }

                // set the mode bits
                ChmodActionOp::Set => {
                    // See the EXTENDED DESCRIPTION section of
                    // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/chmod.html
                    // for the meaning of "permcopy" and "permlist"

                    // The 3 permission bits to copy from "permcopy"
                    let copy_value = match (action.copy_user, action.copy_group, action.copy_others)
                    {
                        (true, false, false) => user >> 6,
                        (false, true, false) => group >> 3,
                        (false, false, true) => others,
                        (false, false, false) => {
                            // Either a "permlist" was specified or nothing is
                            0
                        }
                        _ => panic!(
                            "Only one of 'u', 'g', or 'o' can be used as the source for copying"
                        ),
                    };

                    // Should be at most 3 bits
                    debug_assert!(copy_value <= 0b111);

                    if clause.user {
                        user = (copy_value | rwx) << 6;
                    }
                    if clause.group {
                        group = (copy_value | rwx) << 3;
                    }
                    if clause.others {
                        others = copy_value | rwx;
                    }

                    if who_is_not_specified {
                        let umask = get_umask();

                        user = ((copy_value | rwx) << 6) & !umask;
                        group = ((copy_value | rwx) << 3) & !umask;
                        others = (copy_value | rwx) & !umask;
                    }

                    // Always reset when "op" is "="
                    special = 0;
                }
            }

            if action.setuid {
                match action.op {
                    ChmodActionOp::Add | ChmodActionOp::Set => {
                        // If "who" is missing, set both `S_ISUID` and `S_ISGID`
                        if clause.user || who_is_not_specified {
                            special |= S_ISUID as u32;
                        }
                        if clause.group || who_is_not_specified {
                            special |= S_ISGID as u32;
                        }
                    }
                    ChmodActionOp::Remove => {
                        // If "who" is missing, remove both `S_ISUID` and `S_ISGID`
                        if clause.user || who_is_not_specified {
                            special &= !S_ISUID as u32;
                        }
                        if clause.group || who_is_not_specified {
                            special &= !S_ISGID as u32;
                        }
                    }
                }
            }

            if action.sticky {
                // Not affected by the umask
                match action.op {
                    ChmodActionOp::Add | ChmodActionOp::Set => {
                        special |= S_ISVTX as u32;
                    }
                    ChmodActionOp::Remove => {
                        special &= !S_ISVTX as u32;
                    }
                }
            }

            if action.execute_dir && (is_dir || has_any_exec_bits) {
                let mask = if who_is_not_specified { get_umask() } else { 0 };

                match action.op {
                    ChmodActionOp::Add | ChmodActionOp::Set => {
                        user |= S_IXUSR as u32 & !mask;
                        group |= S_IXGRP as u32 & !mask;
                        others |= S_IXOTH as u32 & !mask;
                    }
                    ChmodActionOp::Remove => {
                        user &= !S_IXUSR as u32 & !mask;
                        group &= !S_IXGRP as u32 & !mask;
                        others &= !S_IXOTH as u32 & !mask;
                    }
                }
            }
        }
    }

    user | group | others | special
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_mode() {
        let mode = parse("u=rwX,go=rX").unwrap();
        match mode {
            ChmodMode::Symbolic(s) => {
                assert_eq!(s.clauses.len(), 2);
                let clause = &s.clauses[0];
                assert_eq!(clause.user, true);
                assert_eq!(clause.group, false);
                assert_eq!(clause.others, false);
                assert_eq!(clause.actions.len(), 1);
                let action = &clause.actions[0];
                assert_eq!(action.op, ChmodActionOp::Set);
                assert_eq!(action.copy_user, false);
                assert_eq!(action.copy_group, false);
                assert_eq!(action.copy_others, false);
                assert_eq!(action.read, true);
                assert_eq!(action.write, true);
                assert_eq!(action.execute, false);
                assert_eq!(action.execute_dir, true);
                assert_eq!(action.setuid, false);
                assert_eq!(action.sticky, false);
                let clause = &s.clauses[1];
                assert_eq!(clause.user, false);
                assert_eq!(clause.group, true);
                assert_eq!(clause.others, true);
                assert_eq!(clause.actions.len(), 1);
                let action = &clause.actions[0];
                assert_eq!(action.op, ChmodActionOp::Set);
                assert_eq!(action.copy_user, false);
                assert_eq!(action.copy_group, false);
                assert_eq!(action.copy_others, false);
                assert_eq!(action.read, true);
                assert_eq!(action.write, false);
                assert_eq!(action.execute, false);
                assert_eq!(action.execute_dir, true);
                assert_eq!(action.setuid, false);
                assert_eq!(action.sticky, false);
            }
            _ => panic!("unexpected mode"),
        }
    }

    fn parse_symbolic(mode: &str) -> ChmodSymbolic {
        match parse(mode).unwrap() {
            ChmodMode::Symbolic(s) => s,
            _ => panic!("Incorrect parsing result"),
        }
    }

    #[test]
    fn test_mutate_mode_empty_who() {
        // NOTE: Potential umask race condition here
        let umask = unsafe {
            let m = libc::umask(0);
            libc::umask(m);
            m as u32
        };

        let mode = mutate(0, false, &parse_symbolic("=rwx"));

        assert_eq!(mode | umask, 0o777);
    }

    // Clears all file mode bits
    #[test]
    fn test_mutate_mode_chmod_example_1() {
        assert_eq!(mutate(0o777, false, &parse_symbolic("a+=")), 0);
        assert_eq!(mutate(0o777, false, &parse_symbolic("a+,a=")), 0);
    }

    // Clears group and other write bits
    #[test]
    fn test_mutate_mode_chmod_example_2() {
        assert_eq!(
            mutate(0b111_010_010, false, &parse_symbolic("go+-w")),
            0o700
        );
        assert_eq!(
            mutate(0b111_010_010, false, &parse_symbolic("go+,go-w")),
            0o700
        );
    }

    // Sets group bit to match other bits and then clears group write bit
    #[test]
    fn test_mutate_mode_chmod_example_3() {
        assert_eq!(
            mutate(0o007, false, &parse_symbolic("g=o-w")),
            0b000_101_111
        );
        assert_eq!(
            mutate(0o007, false, &parse_symbolic("g=o,g-w")),
            0b000_101_111
        );
    }

    // Clears group read bit and sets group write bit
    #[test]
    fn test_mutate_mode_chmod_example_4() {
        assert_eq!(
            mutate(0b000_100_000, false, &parse_symbolic("g-r+w")),
            0b000_010_000
        );
        assert_eq!(
            mutate(0b000_100_000, false, &parse_symbolic("g-r,g+w")),
            0b000_010_000
        );
    }

    // Sets owner bits to match group bits and sets other bits to match group bits
    #[test]
    fn test_mutate_mode_chmod_example_5() {
        assert_eq!(mutate(0o070, false, &parse_symbolic("uo=g")), 0o777);
    }

    #[test]
    fn test_mutate_mode_exec_dir() {
        let plus_exec_dir = parse_symbolic("+X");

        // Always apply X on directories
        assert_eq!(mutate(0o444, true, &plus_exec_dir), 0o555);

        // Ignore X on non-directories not having any execute bits
        assert_eq!(
            mutate(0o444 /* a=rw */, false, &plus_exec_dir),
            0o444 /* Still a=rw */
        );

        // Apply X when file has an execute bit
        assert_eq!(mutate(0o544, false, &plus_exec_dir), 0o555);
        assert_eq!(mutate(0o454, false, &plus_exec_dir), 0o555);
        assert_eq!(mutate(0o445, false, &plus_exec_dir), 0o555);
        assert_eq!(mutate(0o554, false, &plus_exec_dir), 0o555);
        assert_eq!(mutate(0o545, false, &plus_exec_dir), 0o555);
        assert_eq!(mutate(0o455, false, &plus_exec_dir), 0o555);
        assert_eq!(mutate(0o555, false, &plus_exec_dir), 0o555);

        // =X should clear the read permission on user
        assert_eq!(mutate(0o500, false, &parse_symbolic("=X")), 0o111);
        // +X should retain the read permission on user
        assert_eq!(mutate(0o500, false, &parse_symbolic("+X")), 0o511);

        // -X removes execute permission on everyone
        assert_eq!(mutate(0o711, false, &parse_symbolic("-X")), 0o600);

        // Add execute permission on user then +X
        assert_eq!(mutate(0o400, false, &parse_symbolic("u=x,+X")), 0o111);
    }

    #[test]
    fn test_mutate_mode_clear_set_copy_then_reset() {
        assert_eq!(
            mutate(0o111, false, &parse_symbolic("a=,u=rwx,g=u,u=")),
            0o070
        );
        assert_eq!(
            mutate(0o111, false, &parse_symbolic("a=,u=rwx,o=u,u=")),
            0o007
        );
        assert_eq!(
            mutate(0o111, false, &parse_symbolic("a=,g=rwx,u=g,g=")),
            0o700
        );
        assert_eq!(
            mutate(0o111, false, &parse_symbolic("a=,g=rwx,o=g,g=")),
            0o007
        );
        assert_eq!(
            mutate(0o111, false, &parse_symbolic("a=,o=rwx,u=o,o=")),
            0o700
        );
        assert_eq!(
            mutate(0o111, false, &parse_symbolic("a=,o=rwx,g=o,o=")),
            0o070
        );
    }
}
