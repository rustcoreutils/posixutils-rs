//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Diagnostic and stream management module for c17 C17 compiler
//

use gettextrs::{gettext, gettext_args, ngettext_args};
use std::cell::{Cell, RefCell};
use std::fmt;
use std::io::{self, Write};

use crate::warn_options::{self, Verdict};

// Source Position

/// Source position tracking for tokens and diagnostics.
///
/// A compact structure attached to every token, tracking file, line,
/// column, and preprocessor state (whitespace, newline flags).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Position {
    /// Stream/file index (which file this is in)
    pub stream: u16,
    /// Line number (1-based)
    pub line: u32,
    /// Column position (1-based, 0 means unknown), a tab reaching the next
    /// multiple of 8 as gcc's diagnostics count it.
    pub col: u16,
    /// The bytes before the token on its physical line, up to 255: gcc's
    /// byte column less one, a tab counting as one. `-E` indents a line by
    /// it, as gcc does; see `token::lexer::indentation`. Kept to a byte so
    /// that it fits where the struct had padding.
    pub line_offset: u8,
    /// Token preceded by newline
    pub newline: bool,
    /// Token preceded by whitespace
    pub whitespace: bool,
    /// Don't expand macros (for preprocessor)
    pub noexpand: bool,
}

impl Position {
    pub fn new(stream: u16, line: u32, col: u16) -> Self {
        Self {
            stream,
            line,
            col,
            line_offset: 0,
            newline: false,
            whitespace: false,
            noexpand: false,
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Get filename from global registry
        let name = STREAMS.with(|s| {
            s.borrow()
                .get_name(self.stream)
                .map(|n| n.to_string())
                .unwrap_or_else(|| "<unknown>".to_string())
        });

        if self.col > 0 {
            write!(f, "{}:{}:{}", name, self.line, self.col)
        } else {
            write!(f, "{}:{}", name, self.line)
        }
    }
}

// Stream (Input Source)

/// Input stream information for tracking source files and includes.
#[derive(Debug, Clone)]
pub struct Stream {
    /// Filename
    pub name: String,
    /// Position of #include directive that included this file
    /// None for the main file
    pub include_pos: Option<Position>,
    /// Line number offset from #line directive
    /// If Some((new_line, new_file)), positions should be adjusted
    pub line_directive: Option<(u32, Option<String>)>,
    /// Named by a `# N "file" 3` linemarker, i.e. a system header. Warnings
    /// from such a stream are suppressed, as they are in GCC: a preprocessed
    /// file carries the whole of glibc with it, and its warnings are not the
    /// user's to act on.
    pub is_system: bool,
}

impl Stream {
    pub fn new(name: String) -> Self {
        Self {
            name,
            include_pos: None,
            line_directive: None,
            is_system: false,
        }
    }

    pub fn included(name: String, include_pos: Position) -> Self {
        Self {
            name,
            include_pos: Some(include_pos),
            line_directive: None,
            is_system: false,
        }
    }
}

// Stream Registry (Global)

/// Stream registry for managing all input files
#[derive(Debug, Default)]
pub struct StreamRegistry {
    streams: Vec<Stream>,
}

impl StreamRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, name: String) -> u16 {
        let id = self.streams.len() as u16;
        self.streams.push(Stream::new(name));
        id
    }

    /// Find the stream already registered under `name`, or add one.
    ///
    /// A preprocessed file re-enters the same file many times -- every
    /// `# N "foo.h" 2` returns to one already seen -- and each name must map
    /// to one stream, or the include-chain note and the `-E` marker writer
    /// both see a file they have never met before.
    pub fn find_or_add(&mut self, name: &str) -> u16 {
        match self.streams.iter().position(|s| s.name == name) {
            Some(i) => i as u16,
            None => self.add(name.to_string()),
        }
    }

    /// Mark a stream as a system header (linemarker flag 3).
    pub fn set_system(&mut self, id: u16, is_system: bool) {
        if let Some(stream) = self.streams.get_mut(id as usize) {
            stream.is_system = is_system;
        }
    }

    /// Whether warnings from this stream are suppressed.
    pub fn is_system(&self, id: u16) -> bool {
        self.streams.get(id as usize).is_some_and(|s| s.is_system)
    }

    pub fn add_included(&mut self, name: String, include_pos: Position) -> u16 {
        let id = self.streams.len() as u16;
        self.streams.push(Stream::included(name, include_pos));
        id
    }

    pub fn get(&self, id: u16) -> Option<&Stream> {
        self.streams.get(id as usize)
    }

    pub fn get_name(&self, id: u16) -> Option<&str> {
        self.streams.get(id as usize).map(|s| s.name.as_str())
    }

    /// Get the previous stream in the include chain
    /// Returns None for the main file
    pub fn prev_stream(&self, id: u16) -> Option<u16> {
        self.streams
            .get(id as usize)
            .and_then(|s| s.include_pos)
            .map(|pos| pos.stream)
    }

    /// Get effective filename and line for a position
    /// This handles #line directives
    pub fn effective_position(&self, pos: Position) -> (String, u32, u16) {
        if let Some(stream) = self.streams.get(pos.stream as usize) {
            if let Some((line_offset, ref opt_file)) = stream.line_directive {
                let name = opt_file
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or(&stream.name);
                // Adjust line number based on #line directive
                return (name.to_string(), pos.line + line_offset, pos.col);
            }
            (stream.name.clone(), pos.line, pos.col)
        } else {
            ("<unknown>".to_string(), pos.line, pos.col)
        }
    }

    /// Clear all streams (for reuse between compilations)
    pub fn clear(&mut self) {
        self.streams.clear();
    }
}

// Thread-local stream registry
thread_local! {
    pub static STREAMS: RefCell<StreamRegistry> = RefCell::new(StreamRegistry::new());
}

pub fn init_stream(name: &str) -> u16 {
    STREAMS.with(|s| s.borrow_mut().add(name.to_string()))
}

/// Resolve `name` to its stream, registering one if this is the first sighting.
pub fn find_or_add_stream(name: &str) -> u16 {
    STREAMS.with(|s| s.borrow_mut().find_or_add(name))
}

/// Mark a stream as a system header, silencing its warnings.
/// Whether stream `id` is a system header's, whose warnings are not shown.
pub fn stream_is_system(id: u16) -> bool {
    STREAMS.with(|s| s.borrow().is_system(id))
}

pub fn set_stream_system(id: u16, is_system: bool) {
    STREAMS.with(|s| s.borrow_mut().set_system(id, is_system));
}

pub fn init_included_stream(name: &str, include_pos: Position) -> u16 {
    STREAMS.with(|s| s.borrow_mut().add_included(name.to_string(), include_pos))
}

/// Resolve a position to `(file, line, column)`, honoring any `#line`
/// directive in effect for that stream.
///
/// The `-E` line markers need this: `#line` renames the stream for the reader,
/// and a marker that ignored it would contradict the diagnostics.
pub fn effective_position(pos: Position) -> (String, u32, u16) {
    STREAMS.with(|s| s.borrow().effective_position(pos))
}

/// The name registered for a stream.
///
/// Read only by this module's own tests; production formats a name through
/// `effective_position`, which also follows `#line`.
#[cfg(test)]
pub fn stream_name(id: u16) -> String {
    STREAMS.with(|s| {
        s.borrow()
            .get_name(id)
            .map(|n| n.to_string())
            .unwrap_or_else(|| "<unknown>".to_string())
    })
}

#[cfg(test)]
pub fn stream_prev(id: u16) -> Option<u16> {
    STREAMS.with(|s| s.borrow().prev_stream(id))
}

pub fn clear_streams() {
    STREAMS.with(|s| s.borrow_mut().clear());
}

/// Get all stream names (for DWARF .file directives)
/// Returns a vector of file paths, indexed by stream ID
pub fn get_all_stream_names() -> Vec<String> {
    STREAMS.with(|s| {
        s.borrow()
            .streams
            .iter()
            .map(|stream| stream.name.clone())
            .collect()
    })
}

// Error Tracking

/// Error phase flag
pub const ERROR_CURR_PHASE: u32 = 1;

// Error state
// Per thread: the compiler runs on one thread of its own, and a unit test's
// translation unit on one of the test harness's, where a count shared with
// every concurrent test made "did this report an error" unanswerable.
thread_local! {
    static HAS_ERROR: Cell<u32> = const { Cell::new(0) };
    static ERROR_COUNT: Cell<u32> = const { Cell::new(0) };
    static WARNING_COUNT: Cell<u32> = const { Cell::new(0) };
}

// The command-line switches below are per thread for the same reason as the
// counts: the driver sets them on the compiler thread, and an in-process
// test compiles on a thread of its own with switches of its own.
thread_local! {
    /// Set by `-w`: warnings are counted but not printed.
    ///
    /// Counting them still is deliberate -- `-w` is about output, and a
    /// caller asking how many warnings a translation unit produced should get
    /// the truth.
    static SUPPRESS_WARNINGS: Cell<bool> = const { Cell::new(false) };

    /// Warning groups turned off by `-Wno-<name>`.
    ///
    /// `-w` is all-or-nothing and lives in `SUPPRESS_WARNINGS`; this is the
    /// named half. Set from the driver, because the code that emits a
    /// warning is nowhere near the code that parsed the command line.
    static SUPPRESSED_GROUPS: RefCell<std::collections::HashSet<String>> =
        RefCell::new(std::collections::HashSet::new());

    /// The groups of [`OFF_BY_DEFAULT`] the options turn on.
    static OPTED_IN_GROUPS: RefCell<std::collections::HashSet<&'static str>> =
        RefCell::new(std::collections::HashSet::new());

    /// `-fpermissive`; see [`set_permissive`].
    static PERMISSIVE: Cell<bool> = const { Cell::new(false) };

    /// `-pedantic` and its relatives; see [`Pedantic`].
    static PEDANTIC: Cell<Pedantic> = const { Cell::new(Pedantic::OFF) };

    /// `-Werror` and its relatives; see [`Werror`].
    static WERROR: RefCell<Werror> = RefCell::new(Werror::default());

    /// `-Wframe-larger-than=`'s limit; see [`frame_larger_than`].
    static FRAME_LARGER_THAN: Cell<Option<u64>> = const { Cell::new(None) };

    /// `-Wstack-usage=`'s limit; see [`stack_usage_warns_above`].
    static STACK_USAGE: Cell<Option<u64>> = const { Cell::new(None) };

    /// Warnings made errors by [`WERROR`] in this translation unit, for
    /// [`finish_unit`].
    static PROMOTED: Cell<u32> = const { Cell::new(0) };

    /// The `-Wno-<name>` options naming no warning c17 knows, in
    /// command-line order, for [`finish_unit`]'s notes.
    static UNKNOWN_NEGATIONS: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };

    /// Diagnostics shown in this translation unit, for [`finish_unit`].
    static SHOWN: Cell<u32> = const { Cell::new(0) };

    /// Where diagnostics go when not to stderr; see [`capture_diagnostics`].
    static CAPTURE: RefCell<Option<Vec<String>>> = const { RefCell::new(None) };

    /// `-fno-diagnostics-show-option` turns this off: the `[-W<name>]` tag
    /// that names the option controlling a diagnostic.
    static SHOW_OPTION: Cell<bool> = const { Cell::new(true) };

    /// Whether the last warning or error was shown, for the [`note`]s that
    /// follow it.
    static LAST_SHOWN: Cell<bool> = const { Cell::new(false) };

    /// How many [`without_warnings`] calls are running.
    static QUIET: Cell<u32> = const { Cell::new(0) };

    /// The diagnostics given so far inside [`each_once`], which gives no
    /// diagnostic twice; `None` outside it.
    static GIVEN: RefCell<Option<std::collections::HashSet<DiagKey>>> =
        const { RefCell::new(None) };
}

/// What makes two diagnostics the same one: severity, place and text.
type DiagKey = (bool, u16, u32, u16, String);

/// Run `f`, giving each distinct diagnostic it reports once, however many
/// times it is reported: for work that lowers one piece of source several
/// times, such as the versions of a `target_clones` body. A repeat is neither
/// printed nor counted; the first report already counted it.
pub fn each_once<R>(f: impl FnOnce() -> R) -> R {
    let outer = GIVEN.replace(Some(std::collections::HashSet::new()));
    let result = f();
    GIVEN.set(outer);
    result
}

/// Whether this diagnostic was given before inside [`each_once`]; the first
/// time, it is recorded.
fn given_before(level: DiagLevel, pos: Position, msg: &str) -> bool {
    if level == DiagLevel::Note {
        return false;
    }
    GIVEN.with_borrow_mut(|given| {
        given.as_mut().is_some_and(|given| {
            let key = (
                level == DiagLevel::Error,
                pos.stream,
                pos.line,
                pos.col,
                msg.to_string(),
            );
            !given.insert(key)
        })
    })
}

/// Run `f` giving none of its warnings, which are neither shown nor
/// counted: for work whose warnings belong to a later stage, as `-E`'s
/// pragmas belong to the compile that reads them. Errors are given as ever.
pub fn without_warnings<R>(f: impl FnOnce() -> R) -> R {
    QUIET.set(QUIET.get() + 1);
    let result = f();
    QUIET.set(QUIET.get() - 1);
    result
}

/// Suppress warning output for the rest of this thread's compilation (`-w`).
pub fn suppress_warnings() {
    SUPPRESS_WARNINGS.set(true);
}

/// The warning groups the `-W<name>` options in `names` leave off, folded
/// in command-line order: `-Wno-<name>` turns a group off, and `-W<name>` or
/// `-Werror=<name>` -- which gcc makes enable the group too -- turn it back
/// on.
fn suppressed_groups<'a>(
    names: impl IntoIterator<Item = &'a str>,
) -> std::collections::HashSet<String> {
    let mut off = std::collections::HashSet::new();
    for name in names {
        if name == "no-error" || name.starts_with("no-error=") {
            continue;
        }
        if let Some(group) = name.strip_prefix("no-") {
            off.insert(group.to_string());
        } else {
            off.remove(name.strip_prefix("error=").unwrap_or(name));
        }
    }
    off
}

/// The warning groups gcc leaves off until asked for, each with the options
/// that turn it on as a side effect. An explicit `-W<name>`,
/// `-Werror=<name>` or `-Wno-<name>` decides alone; failing that, the last
/// of the side-effect options and their `-Wno-` forms does, as gcc's
/// `LangEnabledBy` does (`-Wall -Wno-pedantic` leaves `-Wpointer-sign` off).
/// `-pedantic` and `-pedantic-errors` count as `-Wpedantic`.
const OFF_BY_DEFAULT: &[(&str, &[&str])] = &[("pointer-sign", &["all", "pedantic"])];

/// The groups of [`OFF_BY_DEFAULT`] the options in `names` turn on.
fn opted_in_groups<'a>(
    names: impl IntoIterator<Item = &'a str> + Clone,
) -> std::collections::HashSet<&'static str> {
    let mut on = std::collections::HashSet::new();
    for &(group, enablers) in OFF_BY_DEFAULT {
        let mut explicit = None;
        let mut implied = false;
        for name in names.clone() {
            let name = if name == "pedantic-errors" {
                "pedantic"
            } else {
                name
            };
            if name == group || name.strip_prefix("error=") == Some(group) {
                explicit = Some(true);
            } else if name.strip_prefix("no-") == Some(group) {
                explicit = Some(false);
            } else if enablers.contains(&name) {
                implied = true;
            } else if name
                .strip_prefix("no-")
                .is_some_and(|n| enablers.contains(&n))
            {
                implied = false;
            }
        }
        if explicit.unwrap_or(implied) {
            on.insert(group);
        }
    }
    on
}

/// Set every switch the `-W<name>` options in `names` select, in
/// command-line order, for the rest of this thread's compilation: the groups
/// left on, [`Pedantic`] and [`Werror`]. The driver passes `-pedantic` as the
/// name `pedantic` and `-pedantic-errors` as `pedantic-errors`.
pub fn set_warning_options(names: &[&str]) {
    UNKNOWN_NEGATIONS.replace(
        names
            .iter()
            .filter(|name| warn_options::classify(name) == Verdict::UnknownNegation)
            .map(|name| name.to_string())
            .collect(),
    );
    SUPPRESSED_GROUPS.replace(suppressed_groups(names.iter().copied()));
    OPTED_IN_GROUPS.replace(opted_in_groups(names.iter().copied()));
    PEDANTIC.set(Pedantic::from_warning_options(names.iter().copied()));
    WERROR.replace(Werror::from_warning_options(names.iter().copied()));
    FRAME_LARGER_THAN.set(frame_limit(names.iter().copied()));
    STACK_USAGE.set(stack_usage_limit(names.iter().copied()));
}

/// The warning group of "the frame size of B bytes is larger than N bytes",
/// as gcc tags it.
pub const FRAME_LARGER_THAN_WARNING: &str = "frame-larger-than=";

/// The limit the `-W<group><size>` options in `names` set, `group` being a
/// size-valued warning's (`frame-larger-than=`): the last one's,
/// `-Werror=<group><size>` setting it as well, and none after the bare
/// `-Wno-` form of the group.
fn size_limit<'a>(names: impl IntoIterator<Item = &'a str>, group: &str) -> Option<u64> {
    let negated = format!("no-{}", group.trim_end_matches('='));
    names.into_iter().fold(None, |limit, name| {
        let name = name.strip_prefix("error=").unwrap_or(name);
        match name.strip_prefix(group) {
            Some(size) => warn_options::size_value(size).or(limit),
            None if name == negated => None,
            None => limit,
        }
    })
}

/// The limit of `-Wframe-larger-than=`; see [`size_limit`].
fn frame_limit<'a>(names: impl IntoIterator<Item = &'a str>) -> Option<u64> {
    size_limit(names, FRAME_LARGER_THAN_WARNING)
}

/// The frame size `-Wframe-larger-than=` warns above, if it is in effect.
pub fn frame_larger_than() -> Option<u64> {
    FRAME_LARGER_THAN.get()
}

/// The warning group of "stack usage is N bytes" and its kin, as gcc tags
/// them.
pub const STACK_USAGE_WARNING: &str = "stack-usage=";

/// The limit of `-Wstack-usage=`; see [`size_limit`]. gcc takes a limit of
/// `PTRDIFF_MAX` or more -- which an overlong count saturates to -- as none,
/// so that not even "might be unbounded" is said.
fn stack_usage_limit<'a>(names: impl IntoIterator<Item = &'a str>) -> Option<u64> {
    size_limit(names, STACK_USAGE_WARNING).filter(|&limit| limit < i64::MAX as u64)
}

/// The stack usage `-Wstack-usage=` warns above, if it is in effect.
pub fn stack_usage_warns_above() -> Option<u64> {
    STACK_USAGE.get()
}

/// Is `-pedantic` (or `-pedantic-errors`) in effect? For a warning gcc gives
/// in its own group only when pedantic, which [`pedwarn`] cannot express.
pub fn pedantic() -> bool {
    PEDANTIC.get().enabled
}

/// Is the warning group `name` on? A diagnostic in a group goes through
/// [`group_warning`], which asks this itself. A group is on unless
/// `-Wno-<name>` turned it off -- or, for one of [`OFF_BY_DEFAULT`], unless
/// an option turned it on.
pub fn warning_group_enabled(name: &str) -> bool {
    if OFF_BY_DEFAULT.iter().any(|&(group, _)| group == name) {
        return OPTED_IN_GROUPS.with_borrow(|groups| groups.contains(name));
    }
    !SUPPRESSED_GROUPS.with_borrow(|groups| groups.contains(name))
}

/// Report a warning in the group `name` -- the one `-Wno-<name>` turns off
/// and `-Werror=<name>` makes an error -- unless the group is off.
pub fn group_warning(name: &str, pos: Position, msg: &str) {
    if warning_group_enabled(name) {
        give_warning(Some(name), pos, msg);
    } else {
        LAST_SHOWN.set(false);
    }
}

/// [`group_warning`] with a translatable template; see [`warning_args`].
pub fn group_warning_args(name: &str, pos: Position, template: &str, args: &[&str]) {
    group_warning(name, pos, &gettext_args(template, args));
}

/// Are warnings being printed?
pub fn warnings_suppressed() -> bool {
    SUPPRESS_WARNINGS.get()
}

/// Collect this thread's diagnostics instead of printing them, until
/// [`take_captured_diagnostics`]. Each line is exactly what stderr would have
/// received, so a test that reads them reads what a user sees.
pub fn capture_diagnostics() {
    CAPTURE.replace(Some(Vec::new()));
}

/// The lines collected since [`capture_diagnostics`]; printing resumes.
pub fn take_captured_diagnostics() -> Vec<String> {
    CAPTURE.take().unwrap_or_default()
}

/// Print one diagnostic line, or keep it if this thread is capturing.
fn emit_line(line: String) {
    let kept = CAPTURE.with_borrow_mut(|lines| match lines {
        Some(lines) => {
            lines.push(line.clone());
            true
        }
        None => false,
    });
    if !kept {
        let _ = writeln!(io::stderr(), "{line}");
    }
}

/// `-fpermissive`: accept as warnings the pre-C99 constructs and the
/// constraint violations gcc lets through.
///
/// The pre-C99 constructs, removed by C99 and an error here by default, are
/// implicit `int` in a declaration that names no type (6.7.2p2) and the
/// implicit declaration of a function called before it is declared
/// (6.5.1p2). The constraint violations are those [`permissive_error`]
/// reports.
///
/// This is not a dialect switch and does not make c17 a C89 compiler. The
/// language it accepts is still C17; `-std=` remains inert. gcc draws the same
/// line -- it rejects both by default too, and its own testsuite marks the
/// cases that need them with `-fpermissive` or `-std=gnu89` rather than
/// expecting them to compile.
///
/// Turns it on for the rest of this thread's compilation.
pub fn set_permissive() {
    PERMISSIVE.set(true);
}

/// Is `-fpermissive` in effect?
pub fn permissive() -> bool {
    PERMISSIVE.get()
}

/// Is a pre-C99 construct in the warning group `group` -- `implicit-int` or
/// `implicit-function-declaration` -- accepted? It is an error by default,
/// as in gcc 14; `-fpermissive` accepts it with a warning, and
/// `-Wno-<group>` accepts it in silence. If accepted, give the diagnostic
/// that remains via [`permerror_warning`].
pub fn permerror_accepted(group: &str) -> bool {
    permissive() || !warning_group_enabled(group)
}

/// The warning an accepted [`permerror_accepted`] construct still draws:
/// none once `-Wno-<group>` has turned the group off.
pub fn permerror_warning(group: &str, pos: Position, msg: &str) {
    group_pedwarn_default(group, pos, msg);
}

/// A constraint violation gcc lets through with only a warning: an error
/// here, and a warning under `-fpermissive`, which is where c17 keeps that
/// leniency.
pub fn permissive_error(pos: Position, msg: &str) {
    if permissive() {
        warning(pos, msg);
    } else {
        error(pos, msg);
    }
}

/// The `-pedantic` switch: whether the diagnostics gcc gives only under
/// `-Wpedantic` are given, and whether gcc's pedwarns are errors.
///
/// gcc has two kinds of pedwarn, and this owns both. Those it gives only
/// under `-Wpedantic` are the constraint violations it accepts in silence as
/// GNU extensions -- a function pointer against `void *`, `int f(...)` -- so
/// they are off by default here too; each goes through [`pedwarn`]. Those it
/// gives by default are warnings that `-pedantic-errors` makes errors -- a
/// struct member list with no `;` after its last member; each goes through
/// [`pedwarn_default`]. Both ask this and nothing else.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Pedantic {
    /// `-pedantic` or `-Wpedantic`, and not a later `-Wno-pedantic`.
    enabled: bool,
    /// `-pedantic-errors`. gcc keeps it through a later `-Wno-pedantic`,
    /// which silences the `-Wpedantic` diagnostics outright, and a later
    /// `-Wpedantic` brings them back as errors.
    errors: bool,
}

impl Pedantic {
    /// gcc's default: no pedantic diagnostics at all.
    const OFF: Pedantic = Pedantic {
        enabled: false,
        errors: false,
    };

    /// Fold one `-W<name>` warning option into the switch, in command-line
    /// order, as gcc does: the last of `-Wpedantic` and `-Wno-pedantic` wins.
    /// The driver passes `-pedantic` as the name `pedantic` and
    /// `-pedantic-errors` as `pedantic-errors`. Answers `None` for any other
    /// name.
    fn after(self, name: &str) -> Option<Pedantic> {
        match name {
            "pedantic" => Some(Pedantic {
                enabled: true,
                ..self
            }),
            "pedantic-errors" => Some(Pedantic {
                enabled: true,
                errors: true,
            }),
            "no-pedantic" => Some(Pedantic {
                enabled: false,
                ..self
            }),
            // gcc's `-Werror=<name>` enables the group it names.
            "error=pedantic" => Some(Pedantic {
                enabled: true,
                ..self
            }),
            _ => None,
        }
    }

    /// The switch after every `-W<name>` in `names`, in order.
    fn from_warning_options<'a>(names: impl IntoIterator<Item = &'a str>) -> Pedantic {
        names
            .into_iter()
            .fold(Pedantic::OFF, |p, name| p.after(name).unwrap_or(p))
    }
}

/// `-Werror` and its relatives: which of the warnings given are errors.
///
/// gcc keeps two things. `-Werror` asks for every warning, and a later
/// `-Wno-error` withdraws that. `-Werror=<name>` and `-Wno-error=<name>`
/// give one group a verdict of its own, which outranks the first and which
/// neither `-Werror` nor `-Wno-error` changes; the last for a group wins.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Werror {
    /// `-Werror`, and not a later `-Wno-error`.
    all: bool,
    /// Each named group's verdict: an error (`-Werror=<name>`) or not
    /// (`-Wno-error=<name>`).
    groups: std::collections::HashMap<String, bool>,
}

impl Werror {
    /// Fold one `-W<name>` warning option in, in command-line order. Answers
    /// `None` for a name that is not one of the four.
    fn after(mut self, name: &str) -> Option<Werror> {
        match name {
            "error" => self.all = true,
            "no-error" => self.all = false,
            _ => {
                let (target, verdict) = match name.strip_prefix("error=") {
                    Some(target) => (target, true),
                    None => (name.strip_prefix("no-error=")?, false),
                };
                let group = warn_options::werror_group(target);
                self.groups.insert(group.to_string(), verdict);
            }
        }
        Some(self)
    }

    /// The switch after every `-W<name>` in `names`, in order.
    fn from_warning_options<'a>(names: impl IntoIterator<Item = &'a str>) -> Werror {
        names.into_iter().fold(Werror::default(), |w, name| {
            let unchanged = w.clone();
            w.after(name).unwrap_or(unchanged)
        })
    }

    /// Is a warning in `group` (`None`: in none gcc names) an error? Plain
    /// `-Werror` does not reach a group in [`BEYOND_PLAIN_WERROR`].
    fn promotes(&self, group: Option<&str>) -> bool {
        match group.and_then(|g| self.groups.get(g).copied()) {
            Some(verdict) => verdict,
            None => self.all && !group.is_some_and(|g| BEYOND_PLAIN_WERROR.contains(&g)),
        }
    }
}

/// The warning groups only `-Werror=<name>` makes errors of. c17's
/// "'-fX' is not supported" is one: a distribution's default flags give it
/// on every compile, and a build that adds `-Werror` asks about its own
/// code, not about the compiler's.
const BEYOND_PLAIN_WERROR: &[&str] = &[crate::f_options::UNSUPPORTED_WARNING];

/// The `-W<name>` options of c17's own, which a warning does not name: gcc
/// has none of them, and its tag names an option gcc knows. The error
/// `-Werror=<name>` makes of one names it, being what made it an error.
const C17_GROUPS: &[&str] = &[crate::f_options::UNSUPPORTED_WARNING, "c17-dialect"];

/// The tag of a warning in `group`, as gcc's names its option.
fn warning_tag(group: Option<&str>) -> String {
    match group {
        Some(g) if !C17_GROUPS.contains(&g) => option_tag(&format!("-W{g}")),
        _ => String::new(),
    }
}

/// A warning in the group `name` about the command line, which has no
/// source position: given as the driver gives its own, unless `-w` or
/// `-Wno-<name>` silences it, and an error, tagged as gcc tags one, when
/// `-Werror` reaches the group. Answers whether it was an error.
pub fn command_line_group_warning(name: &str, msg: &str) -> bool {
    if warnings_suppressed() || !warning_group_enabled(name) {
        return false;
    }
    if WERROR.with_borrow(|w| w.promotes(Some(name))) {
        let tag = option_tag(&format!("-Werror={name}"));
        emit_line(format!("c17: {}: {msg}{tag}", gettext("error")));
        return true;
    }
    let tag = warning_tag(Some(name));
    emit_line(format!("c17: {}: {msg}{tag}", gettext("warning")));
    false
}

/// Is `-Werror` in effect for every warning -- what gcc calls "all warnings
/// being treated as errors"? A command-line warning answers to this alone,
/// having no group.
pub fn werror_all() -> bool {
    WERROR.with_borrow(|w| w.all)
}

/// Close a translation unit's diagnostics as gcc does. If any were shown,
/// each `-Wno-<name>` that named no known warning gets a note, last first,
/// saying it may have been meant to silence one. Then, if a warning was
/// made an error, the closing line: "all" under `-Werror`, "some" when only
/// named groups were. Given once, and the next unit starts again -- which
/// is why this, and not [`reset_counts`], clears the tallies: a unit's
/// failure path may reset the counts before its error is reported.
pub fn finish_unit() {
    if SHOWN.replace(0) > 0 {
        UNKNOWN_NEGATIONS.with_borrow(|names| {
            for name in names.iter().rev() {
                emit_line(format!(
                    "c17: {}",
                    warn_options::unknown_negation_note(name)
                ));
            }
        });
    }
    if PROMOTED.replace(0) == 0 {
        return;
    }
    let line = if werror_all() {
        gettext("all warnings being treated as errors")
    } else {
        gettext("some warnings being treated as errors")
    };
    emit_line(format!("c17: {line}"));
}

/// Report a constraint violation gcc diagnoses only under `-pedantic`: nothing
/// by default, a warning under `-pedantic` or `-Wpedantic`, an error under
/// `-pedantic-errors`.
pub fn pedwarn(pos: Position, msg: &str) {
    if PEDANTIC.get().enabled {
        give_pedwarn(Some("pedantic"), pos, msg);
    } else {
        LAST_SHOWN.set(false);
    }
}

/// [`pedwarn`] with a translatable template; see [`warning_args`].
pub fn pedwarn_args(pos: Position, template: &str, args: &[&str]) {
    pedwarn(pos, &gettext_args(template, args));
}

/// A pedwarn of gcc's preprocessor: given under `-pedantic` as [`pedwarn`]
/// is, but in no warning group -- gcc tags it with no option, plain
/// `-Werror` makes it an error and `-Werror=pedantic` does not.
pub fn cpp_pedwarn(pos: Position, msg: &str) {
    if PEDANTIC.get().enabled {
        give_pedwarn(None, pos, msg);
    } else {
        LAST_SHOWN.set(false);
    }
}

/// [`cpp_pedwarn`] with a translatable template; see [`warning_args`].
pub fn cpp_pedwarn_args(pos: Position, template: &str, args: &[&str]) {
    cpp_pedwarn(pos, &gettext_args(template, args));
}

/// Report a constraint violation gcc diagnoses by default but only warns
/// about: a warning, and an error under `-pedantic-errors`. `-Wno-pedantic`
/// does not silence it, as it does not in gcc.
pub fn pedwarn_default(pos: Position, msg: &str) {
    give_pedwarn(None, pos, msg);
}

/// [`pedwarn_default`] with a translatable template; see [`warning_args`].
pub fn pedwarn_default_args(pos: Position, template: &str, args: &[&str]) {
    pedwarn_default(pos, &gettext_args(template, args));
}

/// [`pedwarn_default`] for a pedwarn in the warning group `name`: off under
/// `-Wno-<name>`, and an error under `-Werror=<name>` as well as under
/// `-pedantic-errors`.
pub fn group_pedwarn_default(name: &str, pos: Position, msg: &str) {
    if warning_group_enabled(name) {
        give_pedwarn(Some(name), pos, msg);
    } else {
        LAST_SHOWN.set(false);
    }
}

/// [`group_pedwarn_default`] with a translatable template; see
/// [`warning_args`].
pub fn group_pedwarn_default_args(name: &str, pos: Position, template: &str, args: &[&str]) {
    group_pedwarn_default(name, pos, &gettext_args(template, args));
}

/// A pedwarn that is given: an error under `-pedantic-errors`, a warning
/// otherwise, in `group` as far as `-Werror` is concerned. As in gcc, `-w`
/// and a system header leave it a warning, which is then not shown --
/// `-pedantic-errors` does not reach into libc's headers, which use the
/// extensions it objects to. The error still names its group as `-W<name>`,
/// as gcc's does.
fn give_pedwarn(group: Option<&str>, pos: Position, msg: &str) {
    if PEDANTIC.get().errors && !hidden(pos) {
        let tag = warning_tag(group);
        do_diag(DiagLevel::Error, pos, &format!("{msg}{tag}"));
    } else {
        give_warning(group, pos, msg);
    }
}

/// Is a warning at `pos` not shown: under `-w`, or in a system header?
fn hidden(pos: Position) -> bool {
    warnings_suppressed() || STREAMS.with(|s| s.borrow().is_system(pos.stream))
}

/// Every warning goes through here: a warning in `group` (`None`: in none
/// gcc names), tagged `[-W<group>]` as gcc tags one, or the error `-Werror`
/// makes of it, tagged `[-Werror=<group>]`, or `[-Werror]` for a warning
/// with no name. A warning that is not shown is not promoted either.
fn give_warning(group: Option<&str>, pos: Position, msg: &str) {
    if QUIET.get() > 0 {
        LAST_SHOWN.set(false);
        return;
    }
    if hidden(pos) || !WERROR.with_borrow(|w| w.promotes(group)) {
        do_diag(
            DiagLevel::Warning,
            pos,
            &format!("{msg}{}", warning_tag(group)),
        );
        return;
    }
    PROMOTED.set(PROMOTED.get() + 1);
    let tag = match group {
        Some(group) => option_tag(&format!("-Werror={group}")),
        None => option_tag("-Werror"),
    };
    do_diag(DiagLevel::Error, pos, &format!("{msg}{tag}"));
}

/// `-fdiagnostics-show-option` (the default) or its `-fno-` form, for the
/// rest of this thread's compilation.
pub fn set_show_option(show: bool) {
    SHOW_OPTION.set(show);
}

/// The ` [<option>]` gcc appends to a diagnostic an option controls, or
/// nothing under `-fno-diagnostics-show-option`.
fn option_tag(option: &str) -> String {
    if SHOW_OPTION.get() {
        format!(" [{option}]")
    } else {
        String::new()
    }
}

pub fn has_error() -> u32 {
    HAS_ERROR.get()
}

fn set_error(flag: u32) {
    HAS_ERROR.set(HAS_ERROR.get() | flag);
}

/// How many errors have been reported so far.
///
/// A monotonically increasing count, unlike [`has_error`], which is a sticky
/// bitmask and so cannot distinguish "an error happened here" from "an error
/// happened at some point earlier in this process". A caller wrapping one
/// fallible step -- preprocessing a single operand, say -- snapshots this
/// before and compares after.
pub fn error_count() -> u32 {
    ERROR_COUNT.get()
}

#[cfg(test)]
pub fn warning_count() -> u32 {
    WARNING_COUNT.get()
}

/// Reset error/warning counts.
///
/// The driver compiles every source operand in one process (POSIX requires it
/// to continue past a failing operand), so this state has to be cleared between
/// translation units — otherwise the first file's errors make every later file
/// look like it failed too.
pub fn reset_counts() {
    ERROR_COUNT.set(0);
    WARNING_COUNT.set(0);
    HAS_ERROR.set(0);
}

// Diagnostic Output

/// Diagnostic severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiagLevel {
    Warning,
    Error,
    /// More about the warning or error just given; see [`note`].
    Note,
}

impl DiagLevel {
    /// The `warning: ` / `error: ` label, translated.
    ///
    /// These two words appear on every diagnostic the compiler emits, so they
    /// are where translation buys the most for the least fragmentation. The
    /// message bodies are built with `format!` at their call sites, which
    /// makes them unusable as msgids without restructuring every one — see
    /// #U7 in the cc audit (`git log --grep '#U7'`).
    fn prefix(&self) -> String {
        match self {
            DiagLevel::Warning => format!("{}: ", gettext("warning")),
            DiagLevel::Error => format!("{}: ", gettext("error")),
            DiagLevel::Note => format!("{}: ", gettext("note")),
        }
    }
}

fn show_include_chain(stream_id: u16) -> Option<String> {
    STREAMS.with(|s| {
        let streams = s.borrow();
        let mut chain = Vec::new();
        let mut current = stream_id;

        // Walk up the include chain
        while let Some(prev) = streams.prev_stream(current) {
            if let Some(stream) = streams.get(prev) {
                chain.push(prettify_path(&stream.name));
            }
            current = prev;
        }

        if chain.is_empty() {
            None
        } else {
            // The one piece of user-visible text in this function, so it
            // gets the same treatment as the "note"/"in included file" labels
            // just below rather than being the lone English fragment.
            Some(format!(" ({} {})", gettext("through"), chain.join(", ")))
        }
    })
}

/// Prettify a path by removing ./ prefix if present
fn prettify_path(path: &str) -> String {
    path.strip_prefix("./")
        .map(|s| s.to_string())
        .unwrap_or_else(|| path.to_string())
}

fn do_diag(level: DiagLevel, pos: Position, msg: &str) {
    if given_before(level, pos, msg) {
        LAST_SHOWN.set(false);
        return;
    }
    // Track errors/warnings
    match level {
        DiagLevel::Error => {
            ERROR_COUNT.set(ERROR_COUNT.get() + 1);
            set_error(ERROR_CURR_PHASE);
        }
        DiagLevel::Warning => {
            WARNING_COUNT.set(WARNING_COUNT.get() + 1);
            if hidden(pos) {
                LAST_SHOWN.set(false);
                return;
            }
        }
        DiagLevel::Note => {
            if !LAST_SHOWN.get() || pos.line == 0 {
                return;
            }
        }
    }

    // Format the message
    let (filename, line, col) = STREAMS.with(|s| s.borrow().effective_position(pos));
    let filename = prettify_path(&filename);

    // A note belongs to the diagnostic before it, which gave the include
    // chain if there was one.
    let include_note = match level {
        DiagLevel::Note => None,
        _ => {
            SHOWN.set(SHOWN.get() + 1);
            LAST_SHOWN.set(true);
            show_include_chain(pos.stream)
        }
    };

    // Print include context if present, under the name of the translation
    // unit the chain starts from. Stream 0 is only the first unit's: with
    // several operands, a later unit's header error was reported against it.
    if let Some(chain) = include_note {
        let base = STREAMS.with(|s| {
            let streams = s.borrow();
            let mut root = pos.stream;
            while let Some(prev) = streams.prev_stream(root) {
                root = prev;
            }
            streams
                .get(root)
                .map(|st| prettify_path(&st.name))
                .unwrap_or_else(|| "<unknown>".to_string())
        });
        emit_line(format!(
            "{}: {}: {}{}:",
            base,
            gettext("note"),
            gettext("in included file"),
            chain
        ));
    }

    // Print the diagnostic
    emit_line(if col > 0 {
        format!("{}:{}:{}: {}{}", filename, line, col, level.prefix(), msg)
    } else {
        format!("{}:{}: {}{}", filename, line, level.prefix(), msg)
    });
}

// Public Diagnostic Functions

pub fn warning(pos: Position, msg: &str) {
    give_warning(None, pos, msg);
}

pub fn error(pos: Position, msg: &str) {
    do_diag(DiagLevel::Error, pos, msg);
}

/// A note at `pos` saying more about the warning or error just given -- where
/// the name it is about was declared before, or what was expected -- shown
/// right after it, as gcc's are. A note follows its diagnostic: it is not
/// shown when that was not (`-w`, `-Wno-<name>`, a system header, a repeat),
/// and gives nothing for a position that is not known.
pub fn note(pos: Position, msg: &str) {
    do_diag(DiagLevel::Note, pos, msg);
}

/// [`note`] with a translatable template; see [`warning_args`].
pub fn note_args(pos: Position, template: &str, args: &[&str]) {
    note(pos, &gettext_args(template, args));
}

/// Print a warning built from a translatable template.
///
/// `template` is the msgid and must be a literal, with positional `{0}`/`{1}`
/// placeholders for the substitutions. That is the difference from passing a
/// `format!` result to [`warning`]: `format!` bakes the values in at compile
/// time, leaving the catalog to be searched for a string no extractor ever saw,
/// so the message can never be translated.
pub fn warning_args(pos: Position, template: &str, args: &[&str]) {
    give_warning(None, pos, &gettext_args(template, args));
}

/// Print an error built from a translatable template. See [`warning_args`].
pub fn error_args(pos: Position, template: &str, args: &[&str]) {
    do_diag(DiagLevel::Error, pos, &gettext_args(template, args));
}

/// Print an error whose wording depends on a count.
///
/// Both forms are msgids. English needs only two; a catalog may define its own
/// rule for others.
pub fn error_plural(pos: Position, singular: &str, plural: &str, n: usize, args: &[&str]) {
    do_diag(
        DiagLevel::Error,
        pos,
        &ngettext_args(singular, plural, n, args),
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Inside `each_once` a repeated diagnostic is neither printed nor
    /// counted, a different one still is, and outside it repeats are given
    /// again.
    #[test]
    fn each_once_gives_a_diagnostic_once() {
        clear_streams();
        let stream = init_stream("once.c");
        let (a, b) = (Position::new(stream, 3, 7), Position::new(stream, 4, 1));
        reset_counts();
        capture_diagnostics();
        each_once(|| {
            for _ in 0..3 {
                error(a, "bad goto");
                warning(b, "odd");
            }
            error(b, "bad goto");
        });
        error(a, "bad goto");
        let lines = take_captured_diagnostics();
        assert_eq!(error_count(), 3, "{lines:?}");
        assert_eq!(warning_count(), 1, "{lines:?}");
        assert_eq!(lines.len(), 4, "{lines:?}");
    }

    #[test]
    fn test_position_display() {
        clear_streams();
        let stream = init_stream("test.c");
        let pos = Position::new(stream, 10, 5);
        let s = format!("{}", pos);
        assert_eq!(s, "test.c:10:5");
    }

    #[test]
    fn test_position_no_column() {
        clear_streams();
        let stream = init_stream("test.c");
        let pos = Position::new(stream, 42, 0);
        let s = format!("{}", pos);
        assert_eq!(s, "test.c:42");
    }

    /// The chain names every file between the diagnostic and the top, in
    /// order; `init_included_stream` is what records it.
    #[test]
    fn test_include_chain_is_recorded() {
        clear_streams();
        let main = init_stream("main.c");
        let outer = init_included_stream("outer.h", Position::new(main, 1, 1));
        let inner = init_included_stream("inner.h", Position::new(outer, 1, 1));

        assert_eq!(stream_prev(inner), Some(outer));
        assert_eq!(stream_prev(outer), Some(main));
        assert_eq!(stream_prev(main), None);
        assert_eq!(
            show_include_chain(inner),
            Some(" (through outer.h, main.c)".to_string())
        );
        assert_eq!(show_include_chain(main), None);
    }

    #[test]
    fn test_stream_registry() {
        clear_streams();
        let s1 = init_stream("main.c");
        let s2 = init_stream("header.h");
        assert_eq!(stream_name(s1), "main.c");
        assert_eq!(stream_name(s2), "header.h");
    }

    #[test]
    fn test_include_chain() {
        clear_streams();
        let main_stream = init_stream("main.c");
        let main_pos = Position::new(main_stream, 5, 1);

        let header_stream = init_included_stream("header.h", main_pos);

        assert_eq!(stream_prev(header_stream), Some(main_stream));
        assert_eq!(stream_prev(main_stream), None);
    }

    #[test]
    fn test_prettify_path() {
        assert_eq!(prettify_path("./test.c"), "test.c");
        assert_eq!(prettify_path("test.c"), "test.c");
        assert_eq!(prettify_path("./src/main.c"), "src/main.c");
    }

    #[test]
    fn test_error_counting() {
        reset_counts();
        assert_eq!(error_count(), 0);
        assert_eq!(warning_count(), 0);

        clear_streams();
        let stream = init_stream("test.c");
        let pos = Position::new(stream, 1, 1);

        error(pos, "test error");
        assert_eq!(error_count(), 1);
        assert!(has_error() & ERROR_CURR_PHASE != 0);

        warning(pos, "test warning");
        assert_eq!(warning_count(), 1);

        reset_counts();
        assert_eq!(error_count(), 0);
    }

    fn werror(names: &[&str]) -> Werror {
        Werror::from_warning_options(names.iter().copied())
    }

    /// `-Werror` and `-Wno-error`: the last wins, for every warning.
    #[test]
    fn werror_all_folds_in_order() {
        assert!(!werror(&[]).promotes(None));
        assert!(werror(&["error"]).promotes(None));
        assert!(werror(&["error"]).promotes(Some("overflow")));
        assert!(!werror(&["error", "no-error"]).promotes(None));
        assert!(werror(&["no-error", "error"]).promotes(None));
        // Other options leave it alone.
        assert!(werror(&["error", "all", "no-overflow", "pedantic"]).promotes(None));
        assert_eq!(Werror::default().after("all"), None);
    }

    /// A group's own verdict outranks `-Werror` and `-Wno-error`, whichever
    /// order they come in, and the last one for the group wins.
    #[test]
    fn werror_named_groups_fold_in_order() {
        let w = werror(&["error=overflow"]);
        assert!(w.promotes(Some("overflow")));
        assert!(!w.promotes(Some("attributes")));
        assert!(!w.promotes(None));

        let w = werror(&["error", "no-error=attributes"]);
        assert!(w.promotes(Some("overflow")));
        assert!(!w.promotes(Some("attributes")));
        assert!(w.promotes(None));

        assert!(werror(&["no-error=overflow", "error"]).promotes(None));
        assert!(!werror(&["no-error=overflow", "error"]).promotes(Some("overflow")));
        assert!(werror(&["error=overflow", "no-error"]).promotes(Some("overflow")));
        assert!(!werror(&["error=overflow", "no-error=overflow"]).promotes(Some("overflow")));
        assert!(werror(&["no-error=overflow", "error=overflow"]).promotes(Some("overflow")));
    }

    /// `-Wno-<name>` turns a group off; `-W<name>` and `-Werror=<name>` turn
    /// it back on, and `-Wno-error=<name>` does neither.
    #[test]
    fn suppressed_groups_fold_in_order() {
        let off = |names: &[&str]| suppressed_groups(names.iter().copied());
        assert!(off(&["no-overflow"]).contains("overflow"));
        assert!(off(&["no-overflow", "overflow"]).is_empty());
        assert!(off(&["no-overflow", "error=overflow"]).is_empty());
        assert!(off(&["error=overflow", "no-overflow"]).contains("overflow"));
        assert!(off(&["no-overflow", "no-error=overflow"]).contains("overflow"));
        assert!(off(&["no-error=overflow", "no-error", "error"]).is_empty());
    }

    /// A group off by default is on after `-W<name>`, `-Werror=<name>`, or
    /// one of its enabling options; its own name outranks those in either
    /// order, and among them the last wins.
    #[test]
    fn opted_in_groups_fold_as_gcc_does() {
        let on = |names: &[&str]| opted_in_groups(names.iter().copied()).contains("pointer-sign");
        assert!(!on(&[]));
        assert!(!on(&["extra"]));
        for names in [
            &["all"][..],
            &["pedantic"],
            &["pedantic-errors"],
            &["pointer-sign"],
            &["error=pointer-sign"],
            &["no-all", "all"],
            &["pointer-sign", "no-all"],
            &["no-pedantic", "all"],
        ] {
            assert!(on(names), "{names:?}");
        }
        for names in [
            &["all", "no-pointer-sign"][..],
            &["no-pointer-sign", "all"],
            &["all", "no-all"],
            &["all", "no-pedantic"],
            &["error=pointer-sign", "no-pointer-sign"],
            &["no-error=pointer-sign"],
        ] {
            assert!(!on(names), "{names:?}");
        }
    }

    /// The last `-Wframe-larger-than=` sets the limit, `-Werror=` spelling
    /// included, and `-Wno-frame-larger-than` lifts it.
    #[test]
    fn frame_limit_folds_in_order() {
        let limit = |names: &[&str]| frame_limit(names.iter().copied());
        assert_eq!(limit(&[]), None);
        assert_eq!(limit(&["frame-larger-than=100"]), Some(100));
        assert_eq!(limit(&["error=frame-larger-than=2KiB"]), Some(2048));
        assert_eq!(
            limit(&["frame-larger-than=100", "frame-larger-than=200"]),
            Some(200)
        );
        assert_eq!(
            limit(&["frame-larger-than=100", "no-frame-larger-than"]),
            None
        );
        assert_eq!(
            limit(&["no-frame-larger-than", "frame-larger-than=100"]),
            Some(100)
        );
        assert!(werror(&["error=frame-larger-than=100"]).promotes(Some("frame-larger-than=")));
        assert!(
            !werror(&["error", "no-error=frame-larger-than="]).promotes(Some("frame-larger-than="))
        );
    }

    /// `-Wstack-usage=` folds as `-Wframe-larger-than=` does, apart from the
    /// other's options, and a limit of `PTRDIFF_MAX` or more is none.
    #[test]
    fn stack_usage_limit_folds_in_order() {
        let limit = |names: &[&str]| stack_usage_limit(names.iter().copied());
        assert_eq!(limit(&[]), None);
        assert_eq!(limit(&["stack-usage=100"]), Some(100));
        assert_eq!(limit(&["error=stack-usage=1KiB"]), Some(1024));
        assert_eq!(limit(&["stack-usage=100", "no-stack-usage"]), None);
        assert_eq!(
            limit(&["stack-usage=100", "no-frame-larger-than"]),
            Some(100)
        );
        assert_eq!(limit(&["frame-larger-than=100"]), None);
        assert_eq!(
            limit(&["stack-usage=9223372036854775806"]),
            Some(i64::MAX as u64 - 1)
        );
        assert_eq!(limit(&["stack-usage=9223372036854775807"]), None);
        assert_eq!(limit(&["stack-usage=99999999999999999999"]), None);
    }

    /// A note follows the diagnostic before it: shown after one that was,
    /// not after one a group or `each_once` kept back, and never counted.
    #[test]
    fn notes_follow_their_diagnostic() {
        clear_streams();
        let stream = init_stream("n.c");
        let (a, b) = (Position::new(stream, 1, 5), Position::new(stream, 2, 3));
        reset_counts();
        set_warning_options(&["no-overflow"]);
        capture_diagnostics();
        group_warning("overflow", a, "off");
        note(b, "after an unshown warning");
        group_warning("attributes", a, "on");
        note(b, "after a warning");
        note(Position::default(), "nowhere");
        error(a, "bad");
        note(b, "after an error");
        each_once(|| {
            error(a, "twice");
            error(a, "twice");
            note(b, "after a repeat");
        });
        let lines = take_captured_diagnostics();
        set_warning_options(&[]);
        assert_eq!(
            lines,
            [
                "n.c:1:5: warning: on [-Wattributes]",
                "n.c:2:3: note: after a warning",
                "n.c:1:5: error: bad",
                "n.c:2:3: note: after an error",
                "n.c:1:5: error: twice",
            ]
        );
        assert_eq!(error_count(), 2);
    }

    /// `without_warnings` gives none of its warnings and counts none, and
    /// lets an error through.
    #[test]
    fn without_warnings_gives_errors_only() {
        clear_streams();
        let stream = init_stream("q.c");
        let pos = Position::new(stream, 1, 1);
        reset_counts();
        capture_diagnostics();
        without_warnings(|| {
            warning(pos, "quiet");
            group_warning("pragmas", pos, "quiet too");
            error(pos, "loud");
        });
        let lines = take_captured_diagnostics();
        assert_eq!(lines, ["q.c:1:1: error: loud"]);
        assert_eq!(warning_count(), 0);
    }

    /// The tag names gcc's option, none for a group of c17's own, and
    /// nothing at all under `-fno-diagnostics-show-option`.
    #[test]
    fn warning_tags_name_gccs_options() {
        assert_eq!(warning_tag(Some("overflow")), " [-Woverflow]");
        assert_eq!(warning_tag(None), "");
        assert_eq!(warning_tag(Some(crate::f_options::UNSUPPORTED_WARNING)), "");
        set_show_option(false);
        assert_eq!(warning_tag(Some("overflow")), "");
        assert_eq!(option_tag("-Werror"), "");
        set_show_option(true);
    }

    /// `-Werror=pedantic` turns `-Wpedantic` on, as gcc's does, without
    /// making the default pedwarns errors.
    #[test]
    fn werror_pedantic_enables_the_pedantic_group() {
        let p = Pedantic::from_warning_options(["error=pedantic"]);
        assert!(p.enabled && !p.errors);
        let p = Pedantic::from_warning_options(["error=pedantic", "no-pedantic"]);
        assert!(!p.enabled);
        assert_eq!(Pedantic::OFF.after("no-error=pedantic"), None);
    }
}
