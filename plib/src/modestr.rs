//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use libc::{
    S_IRGRP, S_IROTH, S_IRUSR, S_IRWXG, S_IRWXO, S_IRWXU, S_ISUID, S_ISVTX, S_IWGRP, S_IWOTH,
    S_IWUSR, S_IXGRP, S_IXOTH, S_IXUSR,
};

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
    Absolute(u32),
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
        return Ok(ChmodMode::Absolute(m));
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
pub fn mutate(cur_mode: u32, symbolic: &ChmodSymbolic) -> u32 {
    let mut new_mode = cur_mode;
    let mut user = cur_mode & S_IRWXU as u32;
    let mut group = cur_mode & S_IRWXG as u32;
    let mut others = cur_mode & S_IRWXO as u32;

    // apply each clause
    for clause in &symbolic.clauses {
        // apply each action
        for action in &clause.actions {
            match action.op {
                // add bits to the mode
                ChmodActionOp::Add => {
                    if action.copy_user {
                        user |= cur_mode & S_IRWXU as u32;
                    }
                    if action.copy_group {
                        group |= cur_mode & S_IRWXG as u32;
                    }
                    if action.copy_others {
                        others |= cur_mode & S_IRWXO as u32;
                    }
                    if action.read {
                        user |= S_IRUSR as u32;
                        group |= S_IRGRP as u32;
                        others |= S_IROTH as u32;
                    }
                    if action.write {
                        user |= S_IWUSR as u32;
                        group |= S_IWGRP as u32;
                        others |= S_IWOTH as u32;
                    }
                    if action.execute {
                        user |= S_IXUSR as u32;
                        group |= S_IXGRP as u32;
                        others |= S_IXOTH as u32;
                    }
                    if action.execute_dir {
                        user |= S_IXUSR as u32;
                        group |= S_IXGRP as u32;
                        others |= S_IXOTH as u32;
                    }
                    if action.setuid {
                        user |= S_ISUID as u32;
                    }
                    if action.sticky {
                        others |= S_ISVTX as u32;
                    }
                }

                // remove bits from the mode
                ChmodActionOp::Remove => {
                    if action.copy_user {
                        user &= !(cur_mode & S_IRWXU as u32);
                    }
                    if action.copy_group {
                        group &= !(cur_mode & S_IRWXG as u32);
                    }
                    if action.copy_others {
                        others &= !(cur_mode & S_IRWXO as u32);
                    }
                    if action.read {
                        user &= !S_IRUSR as u32;
                        group &= !S_IRGRP as u32;
                        others &= !S_IROTH as u32;
                    }
                    if action.write {
                        user &= !S_IWUSR as u32;
                        group &= !S_IWGRP as u32;
                        others &= !S_IWOTH as u32;
                    }
                    if action.execute {
                        user &= !S_IXUSR as u32;
                        group &= !S_IXGRP as u32;
                        others &= !S_IXOTH as u32;
                    }
                    if action.execute_dir {
                        user &= !S_IXUSR as u32;
                        group &= !S_IXGRP as u32;
                        others &= !S_IXOTH as u32;
                    }
                    if action.setuid {
                        user &= !S_ISUID as u32;
                    }
                    if action.sticky {
                        others &= !S_ISVTX as u32;
                    }
                }

                // set the mode bits
                ChmodActionOp::Set => {
                    if action.copy_user {
                        user = cur_mode & S_IRWXU as u32;
                    } else {
                        user = 0;
                    }
                    if action.copy_group {
                        group = cur_mode & S_IRWXG as u32;
                    } else {
                        group = 0;
                    }
                    if action.copy_others {
                        others = cur_mode & S_IRWXO as u32;
                    } else {
                        others = 0;
                    }
                    if action.read {
                        user |= S_IRUSR as u32;
                        group |= S_IRGRP as u32;
                        others |= S_IROTH as u32;
                    }
                    if action.write {
                        user |= S_IWUSR as u32;
                        group |= S_IWGRP as u32;
                        others |= S_IWOTH as u32;
                    }
                    if action.execute {
                        user |= S_IXUSR as u32;
                        group |= S_IXGRP as u32;
                        others |= S_IXOTH as u32;
                    }
                    if action.execute_dir {
                        user |= S_IXUSR as u32;
                        group |= S_IXGRP as u32;
                        others |= S_IXOTH as u32;
                    }
                    if action.setuid {
                        user |= S_ISUID as u32;
                    }
                    if action.sticky {
                        others |= S_ISVTX as u32;
                    }
                }
            }
        }

        // apply the clause
        if clause.user {
            new_mode = (new_mode & !S_IRWXU as u32) | user;
        }
        if clause.group {
            new_mode = (new_mode & !S_IRWXG as u32) | group;
        }
        if clause.others {
            new_mode = (new_mode & !S_IRWXO as u32) | others;
        }
    }

    new_mode
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
}
