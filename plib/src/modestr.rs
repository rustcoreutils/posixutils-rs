//
// Copyright (c) 2024-2026 Jeff Garzik
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
    // Try parsing as octal, but reject if it starts with a sign
    if !mode.starts_with('+') && !mode.starts_with('-') {
        if let Ok(m) = u32::from_str_radix(mode, 8) {
            return Ok(ChmodMode::Absolute(m, mode.len() as u32));
        }
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

/// The process file mode creation mask.
///
/// There is no `getumask(2)`. On Linux the mask is read from the `Umask:` line of
/// `/proc/self/status` (Linux 4.7 and later), under a `/proc` verified to be procfs
/// (`madefs::procfs_dir`): nothing is changed, so any thread may read it while others create
/// files. Elsewhere, or where that cannot be read, the only portable way is to set the mask and
/// put back what was there -- a read-modify-write on process-global state, during which a file
/// another thread creates gets a mask of 0. Where it is read that way, a test that *sets* the
/// umask must not run in parallel with one that reads or depends on it, so such tests live in
/// a test binary of their own -- `tree/tests/tree-tests-umask.rs` and
/// `plib/tests/write_atomic_umask.rs`.
pub fn umask() -> u32 {
    #[cfg(target_os = "linux")]
    if let Some(mask) = procfs_umask() {
        return mask;
    }
    let m = unsafe { libc::umask(0) };
    unsafe { libc::umask(m) }; // Immediately revert
    m as u32 // Cast for macOS
}

/// The umask as `/proc/self/status` shows it (`umask`); `None` where it cannot be read. A
/// kernel whose status has no `Umask:` line (before Linux 4.7) is remembered, and not asked
/// again.
#[cfg(target_os = "linux")]
fn procfs_umask() -> Option<u32> {
    use std::io::Read;
    use std::os::fd::{AsRawFd, FromRawFd};
    use std::sync::atomic::{AtomicBool, Ordering};
    static NO_UMASK_LINE: AtomicBool = AtomicBool::new(false);
    if NO_UMASK_LINE.load(Ordering::Relaxed) {
        return None;
    }
    let proc = crate::madefs::procfs_dir().ok()?;
    let flags = libc::O_RDONLY | libc::O_CLOEXEC | libc::O_NOFOLLOW;
    let fd = unsafe { libc::openat(proc.as_raw_fd(), c"self/status".as_ptr(), flags) };
    if fd < 0 {
        return None;
    }
    let mut status = Vec::new();
    unsafe { std::fs::File::from_raw_fd(fd) }
        .read_to_end(&mut status)
        .ok()?;
    let mask = status
        .split(|&b| b == b'\n')
        .find_map(|line| line.strip_prefix(b"Umask:"))
        .and_then(|value| std::str::from_utf8(value).ok())
        .and_then(|value| u32::from_str_radix(value.trim(), 8).ok());
    if mask.is_none() {
        NO_UMASK_LINE.store(true, Ordering::Relaxed);
    }
    mask
}

/// The mode a utility must give a file it creates, per XCU 1.1.1.4 "File Read,
/// Write, and Creation": `S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH`,
/// with the bits in the process's file mode creation mask cleared.
///
/// Utilities whose spec overrides this say so — `c17` creates an executable
/// `S_IRWXU|S_IRWXG|S_IRWXO & ~umask` — and those pass their own mode instead.
pub fn default_create_mode() -> u32 {
    0o666 & !umask()
}

// apply symbolic mutations to the given file at path
#[allow(clippy::unnecessary_cast)] // casts needed for macOS where libc constants are u16
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
                // Read once per `mutate` call: each read is a umask(2)
                // round-trip on process-global state.
                let mask = umask();
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
                    // setuid/setgid for Add are handled by the shared `if action.setuid` block
                    // after this match (#CM4 — removed a redundant duplicate set here).
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

                        // When who is not specified, umask protects bits from being removed
                        // We remove bits in rwx, except those protected by umask
                        user &= !(rwx << 6) | (umask & (S_IRWXU as u32));
                        group &= !(rwx << 3) | (umask & (S_IRWXG as u32));
                        others &= !rwx | (umask & (S_IRWXO as u32));
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
                // The execute bits of the classes the clause names -- all three, less what
                // the umask masks, where it names none.
                let bits = if who_is_not_specified {
                    (S_IXUSR | S_IXGRP | S_IXOTH) as u32 & !get_umask()
                } else {
                    let mut bits = 0;
                    if clause.user {
                        bits |= S_IXUSR as u32;
                    }
                    if clause.group {
                        bits |= S_IXGRP as u32;
                    }
                    if clause.others {
                        bits |= S_IXOTH as u32;
                    }
                    bits
                };

                match action.op {
                    ChmodActionOp::Add | ChmodActionOp::Set => {
                        user |= bits & S_IXUSR as u32;
                        group |= bits & S_IXGRP as u32;
                        others |= bits & S_IXOTH as u32;
                    }
                    ChmodActionOp::Remove => {
                        user &= !(bits & S_IXUSR as u32);
                        group &= !(bits & S_IXGRP as u32);
                        others &= !(bits & S_IXOTH as u32);
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
                assert!(clause.user);
                assert!(!clause.group);
                assert!(!clause.others);
                assert_eq!(clause.actions.len(), 1);
                let action = &clause.actions[0];
                assert_eq!(action.op, ChmodActionOp::Set);
                assert!(!action.copy_user);
                assert!(!action.copy_group);
                assert!(!action.copy_others);
                assert!(action.read);
                assert!(action.write);
                assert!(!action.execute);
                assert!(action.execute_dir);
                assert!(!action.setuid);
                assert!(!action.sticky);
                let clause = &s.clauses[1];
                assert!(!clause.user);
                assert!(clause.group);
                assert!(clause.others);
                assert_eq!(clause.actions.len(), 1);
                let action = &clause.actions[0];
                assert_eq!(action.op, ChmodActionOp::Set);
                assert!(!action.copy_user);
                assert!(!action.copy_group);
                assert!(!action.copy_others);
                assert!(action.read);
                assert!(!action.write);
                assert!(!action.execute);
                assert!(action.execute_dir);
                assert!(!action.setuid);
                assert!(!action.sticky);
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

    /// `umask` reads the mask without setting it, so it is the same under a filter that
    /// refuses umask(2) (EPERM): no other thread of the process ever sees a mask of 0 while it
    /// reads. Run in a child process of its own, given the mask 027 and then the filter.
    #[cfg(all(
        target_os = "linux",
        any(target_arch = "x86_64", target_arch = "aarch64")
    ))]
    #[test]
    fn umask_is_read_without_setting_it() {
        use crate::testing::seccomp::{install_filter, op, JEQ, LD, RET, RET_ALLOW, RET_ERRNO};
        use std::os::unix::process::CommandExt;
        #[cfg(target_arch = "x86_64")]
        const SYS_UMASK: u32 = 95;
        #[cfg(target_arch = "aarch64")]
        const SYS_UMASK: u32 = 166;
        const CHILD: &str = "PLIB_UMASK_READ_CHILD";
        if std::env::var_os(CHILD).is_none() {
            let mut command = std::process::Command::new(std::env::current_exe().unwrap());
            command
                .args([
                    "modestr::tests::umask_is_read_without_setting_it",
                    "--exact",
                    "--nocapture",
                    "--test-threads=1",
                ])
                .env(CHILD, "1");
            unsafe {
                command.pre_exec(|| {
                    libc::umask(0o027);
                    Ok(())
                });
            }
            let eperm = RET_ERRNO | u32::try_from(libc::EPERM).unwrap();
            install_filter(
                &mut command,
                vec![
                    op(LD, 0, 0, 0),
                    op(JEQ, 0, 1, SYS_UMASK),
                    op(RET, 0, 0, eperm),
                    op(RET, 0, 0, RET_ALLOW),
                ],
            );
            let out = command.output().unwrap();
            let stdout = String::from_utf8_lossy(&out.stdout);
            assert!(
                out.status.success() && stdout.contains("1 passed"),
                "child: {stdout}{}",
                String::from_utf8_lossy(&out.stderr)
            );
            return;
        }
        assert_eq!(umask(), 0o027);
        assert_eq!(default_create_mode(), 0o640);
    }

    #[test]
    fn test_mutate_mode_empty_who() {
        let umask = umask();

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
        // With no who, what X adds or removes is masked by the umask (XCU chmod): the
        // execute bits it may touch here.
        let x = 0o111 & !umask();

        // Always apply X on directories
        assert_eq!(mutate(0o444, true, &plus_exec_dir), 0o444 | x);

        // Ignore X on non-directories not having any execute bits
        assert_eq!(
            mutate(0o444 /* a=rw */, false, &plus_exec_dir),
            0o444 /* Still a=rw */
        );

        // Apply X when file has an execute bit
        for init in [0o544, 0o454, 0o445, 0o554, 0o545, 0o455, 0o555] {
            assert_eq!(mutate(init, false, &plus_exec_dir), init | x, "{init:o}");
        }

        // =X should clear the read permission on user
        assert_eq!(mutate(0o500, false, &parse_symbolic("=X")), x);
        // +X should retain the read permission on user
        assert_eq!(mutate(0o500, false, &parse_symbolic("+X")), 0o500 | x);

        // -X removes execute permission on everyone
        assert_eq!(mutate(0o711, false, &parse_symbolic("-X")), 0o711 & !x);

        // Add execute permission on user then +X
        assert_eq!(mutate(0o400, false, &parse_symbolic("u=x,+X")), 0o100 | x);
    }

    /// X touches only the execute bits of the classes the clause names, and removing it
    /// clears nothing else (GNU chmod, measured).
    #[test]
    fn test_mutate_mode_exec_dir_by_class() {
        assert_eq!(mutate(0o644, true, &parse_symbolic("u+X")), 0o744);
        assert_eq!(mutate(0o600, true, &parse_symbolic("o+X")), 0o601);
        assert_eq!(mutate(0o711, false, &parse_symbolic("g+X")), 0o711);
        assert_eq!(mutate(0o701, false, &parse_symbolic("g+X")), 0o711);
        assert_eq!(mutate(0o771, false, &parse_symbolic("u-X")), 0o671);
        assert_eq!(mutate(0o771, false, &parse_symbolic("go-X")), 0o760);
        assert_eq!(mutate(0o771, true, &parse_symbolic("a-X")), 0o660);
        assert_eq!(mutate(0o640, true, &parse_symbolic("g=X")), 0o610);
        // With no who, the umask keeps what it masks, and removal clears no other bit.
        let x = 0o111 & !umask();
        assert_eq!(mutate(0o771, false, &parse_symbolic("-X")), 0o771 & !x);
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
