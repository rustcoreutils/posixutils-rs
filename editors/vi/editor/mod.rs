//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Main editor state and loop.
//!
//! This module contains the Editor struct which ties together all
//! the components of the vi editor.

mod executor;

use std::sync::atomic::AtomicBool;

use crate::buffer::{split_lines, Buffer, BufferMode, Line, Position, Range};
use crate::command::{CommandParser, ParserState};
use crate::error::{Result, ViError};
use crate::ex::cmdline::ExFlags;
use crate::ex::command::{strip_line_terminator, MapMode, SubstituteFlags, CTRL_V};
use crate::ex::{AddressRange, ExCommand, ExResult};
use crate::file::{read_file, write_file, write_range, FileManager};
use crate::input::{InputQueue, InputReader, Key, KeySource, QueuedKey};
use crate::maps::{keys_from_text, MapMatch};
use crate::mode::{enter_insert_mode, process_insert_key, InsertKind, InsertState, Mode};
use crate::options::Options;
use crate::register::{RegisterContent, Registers};
use crate::search::{SearchDirection, SearchState, SubstituteConfig, Substitutor};
use crate::shell::ShellExecutor;
use crate::ui::{Screen, Terminal, TerminalSize};
use crate::undo::UndoManager;
use std::path::{Path, PathBuf};

/// Split a `/`- or `?`-style search body into `(pattern, offset)`.
///
/// The pattern runs up to the first unescaped `delim`; `\<delim>` becomes a
/// literal delimiter in the pattern. Any `+n`/`-n` after the closing delimiter
/// is returned as a signed line offset (0 if absent).
fn split_search(body: &str, delim: char) -> (String, i64) {
    // The same scan as an ex search address, so one RE reads the same at
    // the `/` prompt as after `:` -- `\\/` and `[/]` included.
    let (pattern, rest) = crate::ex::address::scan_pattern(body, delim);
    let rest = rest.trim();
    let offset = if rest.is_empty() {
        0
    } else {
        rest.parse::<i64>().unwrap_or(0)
    };
    (pattern, offset)
}

/// Stores the last substitution for & and :& commands.
#[derive(Debug, Clone)]
pub struct LastSubstitution {
    /// The search pattern.
    pub pattern: String,
    /// The replacement string.
    pub replacement: String,
    /// Whether the substitution was global.
    pub global: bool,
}

/// How deeply `:so` and `:@` may nest.
const MAX_NESTING: usize = 100;

/// File operands as paths.
fn to_paths(files: &[String]) -> Vec<PathBuf> {
    files.iter().map(|f| crate::bytes::path(f)).collect()
}

/// What an edit finds to recover (see `Editor::take_recovery`).
enum Recovery {
    /// Recovery was not asked for.
    NotAsked,
    /// The newest saved contents, the file holding them, and how many older
    /// ones there are.
    Found {
        body: String,
        info: crate::recover::RecoverInfo,
        older: usize,
    },
    /// Nothing is saved for the file.
    None,
    /// Something is saved but cannot be read.
    Failed(std::io::Error),
}

/// Stores the last f/F/t/T find command for ; and , repeat.
#[derive(Debug, Clone, Copy)]
pub struct FindCommand {
    /// The command character (f, F, t, or T).
    pub command: char,
    /// The character to find.
    pub target: char,
}

impl FindCommand {
    /// Create a new find command.
    pub fn new(command: char, target: char) -> Self {
        Self { command, target }
    }

    /// Get the reverse command.
    pub fn reverse(&self) -> char {
        match self.command {
            'f' => 'F',
            'F' => 'f',
            't' => 'T',
            'T' => 't',
            _ => self.command,
        }
    }
}

/// Stores the last command for `.` (dot) repeat.
#[derive(Debug, Clone)]
pub enum LastCommand {
    /// Insert command with the text that was inserted.
    Insert {
        kind: InsertKind,
        text: String,
        count: usize,
    },
    /// Parsed command (delete, change, yank, etc.).
    Parsed(crate::command::ParsedCommand),
    /// A command that entered text input mode (`c`, `C`, `s`, `3i`, ...)
    /// and the text typed there: `.` runs the command again and types the
    /// text.
    Input {
        cmd: crate::command::ParsedCommand,
        text: String,
    },
    /// Simple command that doesn't go through the parser (x, X, J, etc.).
    Simple {
        command: char,
        count: usize,
        char_arg: Option<char>,
    },
}

/// The <blank> prefix of `line`, used as the autoindent level for the next
/// input line.
fn leading_blanks(line: &str) -> &str {
    let end = line
        .char_indices()
        .find(|(_, c)| !matches!(c, ' ' | '\t'))
        .map(|(i, _)| i)
        .unwrap_or(line.len());
    &line[..end]
}

/// Ex text input mode state (:a, :i, :c commands).
#[derive(Debug, Clone)]
pub enum ExInsertMode {
    /// Append text after line (:a).
    Append(usize),
    /// Insert text before line (:i).
    Insert(usize),
    /// Change lines (:c) - delete range then insert.
    Change { start: usize, end: usize },
}

/// The main editor state.
pub struct Editor {
    /// Current buffer.
    buffer: Buffer,
    /// Editor mode.
    mode: Mode,
    /// Command parser for normal mode.
    parser: CommandParser,
    /// Insert mode state.
    insert_state: Option<InsertState>,
    /// The command that began the insert session, for `.` to repeat.
    insert_origin: Option<crate::command::ParsedCommand>,
    /// An interrupt reached ex while it waited for input (see
    /// `ex_signal_ends_session`).
    ex_interrupted: bool,
    /// How deeply `:so` and `:@` are nested, against `MAX_NESTING`.
    nesting: usize,
    /// Files (as [`crate::recover::path_key`]s) to recover the first time
    /// they are edited: `-r`'s operands (94444-94451), and `:recover`'s.
    recover_pending: Vec<PathBuf>,
    /// `-r -t`: recover the next file edited, the tag's.
    recover_next_edit: bool,
    /// `-c` commands, waiting for the first edit of a file that exists
    /// (94452-94459), and the name to report their errors under.
    initial_commands: Vec<String>,
    initial_prog: &'static str,
    /// An operator waiting for the `/` or `?` prompt that is its motion.
    pending_search_op: Option<crate::command::ParsedCommand>,
    /// An operator's `/re/+n` search motion (motion `\0`): whether it goes
    /// the remembered search direction, and its line offset.
    search_motion: Option<(bool, i64)>,
    /// Ex command line input.
    ex_input: String,
    /// The command-line prompt `ex_input` was opened with: `:` for an ex
    /// command, `/` or `?` for a search.  The two share the line, and
    /// `:/b/d` and `/b/d` read the same, so only this tells them apart.
    ex_prompt: char,
    /// Editor options.
    options: Options,
    /// Register storage.
    registers: Registers,
    /// Undo manager.
    undo: UndoManager,
    /// Search state.
    search: SearchState,
    /// File manager.
    files: FileManager,
    /// Terminal interface.
    terminal: Terminal,
    /// Screen renderer.
    screen: Screen,
    /// Status/error message.
    message: Option<String>,
    /// Whether message is an error.
    is_error: bool,
    /// `:map`, `:map!` and `:ab` definitions.
    maps: crate::maps::Maps,
    /// Keys waiting to be acted on: typed, expanded, or from an `@` buffer.
    input: InputQueue,
    /// An undo group is open around the expansion currently being consumed.
    ///
    /// POSIX 95443-95445: "commands resulting from buffer executions and mapped
    /// character expansions, are considered single commands" for undo, so one
    /// `u` reverses the whole of what a map did.
    expansion_group: bool,
    /// A `^V` on the ex command line is pending, so the next key is quoted.
    ///
    /// Only the four commands POSIX gives `<control>-V` escaping to
    /// (94657-94659) make use of it, but the colon line cannot know which
    /// command is being typed until Enter, so it quotes for all of them and
    /// lets the parser decide.
    ex_pending_literal: bool,
    /// Where each outstanding `:tag` / `^]` jump started, oldest first.
    ///
    /// Pushed by `goto_tag`, so both entry points get it; walked by `:pop` and
    /// listed by `:tags`. Entries are never invalidated when `:e` or `:n`
    /// changes the file — every way one can go stale is handled at pop time
    /// instead, which is why the file is kept as a name rather than a handle.
    tag_stack: Vec<crate::tags::TagStackEntry>,
    /// The flag this editor watches for an interrupt.
    ///
    /// A signal handler can only write to a `static`, so the real one is
    /// `signals::SIGINT_RECEIVED` and that is the default. Naming it here
    /// rather than reaching for the global from inside the input drain makes
    /// the interrupt a property of *this* editor: two editors in one process
    /// no longer consume each other's interrupts, which is exactly what two
    /// tests running in parallel are.
    interrupt: &'static AtomicBool,
    /// Whether editor should quit.
    should_quit: bool,
    /// Exit code.
    exit_code: i32,
    /// Shell executor for ! commands.
    shell: ShellExecutor,
    /// Pending shell filter command (for vi ! operator).
    pending_filter: Option<String>,
    /// Last f/F/t/T command for ; and , repeat.
    last_find: Option<FindCommand>,
    /// Last substitution for & command.
    last_substitution: Option<LastSubstitution>,
    /// The last regular expression used anywhere in the editor -- by a search
    /// or by a substitute. An empty `:s//repl/` pattern reuses it (#X28).
    last_regex: Option<String>,
    /// Lines echoed by the most recent substitute's `p`/`l`/`#` flags.
    substitute_output: Vec<String>,
    /// Text from the previous insert session; a NUL in insert mode re-inserts
    /// it (#V15).
    previous_insert: String,
    /// Last command for . (dot) repeat.
    last_command: Option<LastCommand>,
    /// Last macro register for @@ repeat.
    last_macro_register: Option<char>,
    /// Ex text input mode (:a, :i, :c).
    ex_insert_mode: Option<ExInsertMode>,
    /// Whether autoindent applies to the ex text input currently being
    /// collected: the `autoindent` edit option, toggled by a `!` on the
    /// `a`/`i`/`c` command (94894-94896).
    ex_insert_autoindent: bool,
    /// The autoindent characters ex supplies as the prompt for the next input
    /// line (94705-94707). Held apart from what the user types, so the `.`
    /// terminator is still recognised on an indented line.
    ex_insert_indent: String,
    /// Accumulated text for ex insert mode.
    ex_insert_text: Vec<String>,
    /// Whether running in ex standalone mode (line-oriented).
    ex_standalone_mode: bool,
    /// Whether a `:g`/`:v` command list is running.
    in_global: bool,
    /// Counts the times the edit buffer has been replaced (`:e`, `:n`,
    /// `:recover`), so a global can tell that its marks mean nothing now.
    buffer_generation: u64,
    /// The print flags of the ex command running now, for print, number
    /// and list to fold into their output.
    pending_flags: ExFlags,
    /// Whether a `|` ended the ex command now running (94553-94557).
    ex_by_bar: bool,
    /// The flags an implied print uses (number, list): set by the last
    /// print, number or list command, or the last flags given (94561).
    remembered_print: (bool, bool),
    /// Silent/batch mode (suppress prompts and messages).
    silent_mode: bool,
}

impl Editor {
    /// Create a new editor.
    ///
    /// # Arguments
    /// * `ex_mode` - If true, start in ex (line-oriented) mode
    /// * `silent` - If true, suppress prompts and informational messages
    pub fn new(ex_mode: bool, silent: bool) -> Result<Self> {
        let mut editor = if ex_mode && silent {
            // Ex silent mode - no terminal needed, use headless-like setup
            Self::new_ex_silent()?
        } else {
            // Visual mode or Ex mode with terminal
            Self::new_with_terminal()?
        };

        editor.ex_standalone_mode = ex_mode;
        editor.silent_mode = silent;

        Ok(editor)
    }

    /// Create an editor with terminal (internal helper).
    fn new_with_terminal() -> Result<Self> {
        let terminal = Terminal::new()?;
        let size = terminal.size();
        let mut options = Options::default();
        options.set_window_size(size.rows as usize);

        let shell = ShellExecutor::new(&options.shell);

        Ok(Self {
            buffer: Buffer::new(),
            mode: Mode::Command,
            parser: CommandParser::new(),
            insert_state: None,
            insert_origin: None,
            ex_interrupted: false,
            nesting: 0,
            recover_pending: Vec::new(),
            recover_next_edit: false,
            initial_commands: Vec::new(),
            initial_prog: "vi",
            pending_search_op: None,
            search_motion: None,
            ex_input: String::new(),
            ex_prompt: ':',
            options,
            registers: Registers::new(),
            undo: UndoManager::new(),
            search: SearchState::new(),
            files: FileManager::new(),
            terminal,
            screen: Screen::new(size),
            message: None,
            is_error: false,
            maps: crate::maps::Maps::default(),
            input: InputQueue::default(),
            expansion_group: false,
            ex_pending_literal: false,
            tag_stack: Vec::new(),
            interrupt: &crate::signals::SIGINT_RECEIVED,
            should_quit: false,
            exit_code: 0,
            shell,
            pending_filter: None,
            last_find: None,
            last_substitution: None,
            last_regex: None,
            substitute_output: Vec::new(),
            previous_insert: String::new(),
            last_command: None,
            last_macro_register: None,
            ex_insert_mode: None,
            ex_insert_autoindent: false,
            ex_insert_indent: String::new(),
            ex_insert_text: Vec::new(),
            ex_standalone_mode: false,
            in_global: false,
            buffer_generation: 0,
            pending_flags: ExFlags::default(),
            ex_by_bar: false,
            remembered_print: (false, false),
            silent_mode: false,
        })
    }

    /// Create an editor for ex silent/batch mode (no terminal).
    fn new_ex_silent() -> Result<Self> {
        let size = TerminalSize { rows: 24, cols: 80 };
        let mut options = Options::default();
        options.set_window_size(size.rows as usize);

        let shell = ShellExecutor::new(&options.shell);

        Ok(Self {
            buffer: Buffer::new(),
            mode: Mode::Command,
            parser: CommandParser::new(),
            insert_state: None,
            insert_origin: None,
            ex_interrupted: false,
            nesting: 0,
            recover_pending: Vec::new(),
            recover_next_edit: false,
            initial_commands: Vec::new(),
            initial_prog: "vi",
            pending_search_op: None,
            search_motion: None,
            ex_input: String::new(),
            ex_prompt: ':',
            options,
            registers: Registers::new(),
            undo: UndoManager::new(),
            search: SearchState::new(),
            files: FileManager::new(),
            terminal: Terminal::new_dummy(),
            screen: Screen::new(size),
            message: None,
            is_error: false,
            maps: crate::maps::Maps::default(),
            input: InputQueue::default(),
            expansion_group: false,
            ex_pending_literal: false,
            tag_stack: Vec::new(),
            interrupt: &crate::signals::SIGINT_RECEIVED,
            should_quit: false,
            exit_code: 0,
            shell,
            pending_filter: None,
            last_find: None,
            last_substitution: None,
            last_regex: None,
            substitute_output: Vec::new(),
            previous_insert: String::new(),
            last_command: None,
            last_macro_register: None,
            ex_insert_mode: None,
            ex_insert_autoindent: false,
            ex_insert_indent: String::new(),
            ex_insert_text: Vec::new(),
            ex_standalone_mode: true,
            in_global: false,
            buffer_generation: 0,
            pending_flags: ExFlags::default(),
            ex_by_bar: false,
            remembered_print: (false, false),
            silent_mode: true,
        })
    }

    /// Create a new headless editor for testing.
    /// This creates an editor without a real terminal, suitable for unit/integration tests.
    pub fn new_headless() -> Self {
        let size = TerminalSize { rows: 24, cols: 80 };
        let mut options = Options::default();
        options.set_window_size(size.rows as usize);

        let shell = ShellExecutor::new(&options.shell);

        // Create a dummy terminal - it won't be used in headless mode
        // We use unsafe default initialization since we won't actually use it
        Self {
            buffer: Buffer::new(),
            mode: Mode::Command,
            parser: CommandParser::new(),
            insert_state: None,
            insert_origin: None,
            ex_interrupted: false,
            nesting: 0,
            recover_pending: Vec::new(),
            recover_next_edit: false,
            initial_commands: Vec::new(),
            initial_prog: "vi",
            pending_search_op: None,
            search_motion: None,
            ex_input: String::new(),
            ex_prompt: ':',
            options,
            registers: Registers::new(),
            undo: UndoManager::new(),
            search: SearchState::new(),
            files: FileManager::new(),
            terminal: Terminal::new_dummy(),
            screen: Screen::new(size),
            message: None,
            is_error: false,
            maps: crate::maps::Maps::default(),
            input: InputQueue::default(),
            expansion_group: false,
            ex_pending_literal: false,
            tag_stack: Vec::new(),
            interrupt: &crate::signals::SIGINT_RECEIVED,
            should_quit: false,
            exit_code: 0,
            shell,
            pending_filter: None,
            last_find: None,
            last_substitution: None,
            last_regex: None,
            substitute_output: Vec::new(),
            previous_insert: String::new(),
            last_command: None,
            last_macro_register: None,
            ex_insert_mode: None,
            ex_insert_autoindent: false,
            ex_insert_indent: String::new(),
            ex_insert_text: Vec::new(),
            ex_standalone_mode: false,
            in_global: false,
            buffer_generation: 0,
            pending_flags: ExFlags::default(),
            ex_by_bar: false,
            remembered_print: (false, false),
            silent_mode: false,
        }
    }

    /// Watch `flag` for interrupts instead of the process-wide SIGINT flag.
    ///
    /// The interrupt flag is global because a signal handler can only write to
    /// a `static`, and that is right for the one editor a vi process runs. It
    /// is wrong for a test binary, where every test shares the process: the
    /// drain consumes the flag with a `swap(false)`, so a test that arms it
    /// races every other test's keystrokes for it, and whichever reads first
    /// wins. Pointing a headless editor at its own flag removes the sharing
    /// rather than papering over it with a lock or a serialized run.
    pub fn set_interrupt_source(&mut self, flag: &'static AtomicBool) {
        self.interrupt = flag;
    }

    /// The flag this editor watches for interrupts.
    pub fn interrupt_source(&self) -> &'static AtomicBool {
        self.interrupt
    }

    /// Set read-only mode.
    ///
    /// POSIX describes `-R` as setting the `readonly` *edit option*, and the
    /// `write` rules are written against that option (ex.md §95506), so `-R`
    /// and `:set ro` must be the same state. Keeping only the `FileManager`
    /// flag left `:set ro` unable to block a write (#X27).
    pub fn set_readonly(&mut self, readonly: bool) {
        self.files.set_readonly(readonly);
        self.options.readonly = readonly;
    }

    /// True when a file is currently being edited.
    pub fn has_current_file(&self) -> bool {
        self.files.current_file().is_some()
    }

    /// True when writing is disallowed, from either `-R` or `:set readonly`.
    fn is_readonly(&self) -> bool {
        self.options.readonly || self.files.is_readonly()
    }

    /// Set the initial window size (the `-w size` option).
    pub fn set_window(&mut self, n: usize) {
        if n > 0 {
            self.options.window = n;
        }
    }

    /// Read-only access to the editor options (for tests and inspection).
    pub fn options(&self) -> &Options {
        &self.options
    }

    /// Execute an initial command (from -c or +command).
    pub fn execute_initial_command(&mut self, cmd: &str) -> Result<()> {
        self.execute_ex_input(cmd)
    }

    /// Set the buffer content directly (for testing).
    pub fn set_buffer_text(&mut self, text: &str) {
        self.replace_buffer(Buffer::from_text(text));
    }

    /// Get the buffer content as a string (for testing).
    pub fn get_buffer_text(&self) -> String {
        self.buffer.to_string()
    }

    /// Get the current cursor position (for testing).
    pub fn get_cursor(&self) -> Position {
        self.buffer.cursor()
    }

    /// Set the cursor position (for testing).
    pub fn set_cursor(&mut self, pos: Position) {
        self.buffer.set_cursor(pos);
    }

    /// Get the current mode (for testing).
    pub fn get_mode(&self) -> Mode {
        self.mode
    }

    /// Get the current message (for testing).
    pub fn get_message(&self) -> Option<&str> {
        self.message.as_deref()
    }

    /// Check if last message was an error (for testing).
    pub fn is_error_message(&self) -> bool {
        self.is_error
    }

    /// Whether the editor has been asked to exit (for testing). A failing ex
    /// command must leave this `false`, or the unsaved buffer goes with it.
    pub fn should_quit(&self) -> bool {
        self.should_quit
    }

    /// Process a single key (public for testing).
    pub fn process_key(&mut self, key: Key) -> Result<()> {
        self.handle_key(key)
    }

    /// Execute a sequence of keys from a string (for testing).
    /// Special characters: \x1b = Escape, \n = Enter
    pub fn execute_keys(&mut self, keys: &str) -> Result<()> {
        for c in keys.chars() {
            // `Key::from_map_char` is the one translation from text to
            // keystrokes; open-coding it here is how this copy came to disagree
            // with `Key::from_byte` about TAB (#V22) and DEL.
            self.handle_key(Key::from_map_char(c))?;
        }
        Ok(())
    }

    /// Get register content (for testing).
    pub fn get_register(&self, reg: char) -> Option<&crate::register::RegisterContent> {
        self.registers.get(reg)
    }

    /// Get unnamed register content (for testing).
    pub fn get_unnamed_register(&self) -> Option<&crate::register::RegisterContent> {
        self.registers.get_unnamed()
    }

    /// Open a file for editing.
    pub fn open(&mut self, path: &str) -> Result<()> {
        let path_buf = crate::bytes::path(path);
        // Read before anything is replaced, so a failure leaves the editor as
        // it was rather than half-switched to a file it could not open.
        let mut new_buffer = read_file(&path_buf)?;
        let exists = path_buf.exists();
        // -r or :recover: saved contents in place of the file's, if any.
        let recovered = self.take_recovery(&path_buf);
        let mut recover_note = None;
        let mut was_recovered = false;
        match recovered {
            Recovery::NotAsked => {}
            Recovery::Found { body, info, older } => {
                new_buffer = Buffer::from_text(&body);
                crate::recover::remove(&info);
                was_recovered = true;
                let mut note = format!("Recovered {path}");
                if older > 0 {
                    note.push_str(&format!(
                        "; {older} older version{} can be recovered too",
                        if older == 1 { "" } else { "s" }
                    ));
                }
                recover_note = Some(note);
            }
            Recovery::None => recover_note = Some(format!("No recovery information for {path}")),
            Recovery::Failed(e) => recover_note = Some(format!("{path}: cannot recover: {e}")),
        }

        // Capture the outgoing file *before* `set_current_file` replaces it:
        // reading it afterwards gave the file being opened, so `#` in a shell
        // command expanded to the same thing as `%`.
        let old_current = self.shell_current_file();

        self.replace_buffer(new_buffer);
        self.files.set_current_file(Some(path_buf.clone()));
        self.shell
            .set_current_file(Some(crate::bytes::from_path(&path_buf)));
        if old_current.is_some() {
            self.shell.set_alternate_file(old_current);
        }
        // POSIX: ex starts on the *last* line of the edit buffer, vi on the
        // first.  Starting ex on line 1 made every command with a relative or
        // defaulted address act on the wrong end of the file -- `:1t.` copied
        // after line 1 rather than after the last line.
        let start_line = if self.ex_standalone_mode {
            self.buffer.line_count().max(1)
        } else {
            1
        };
        self.buffer.set_line(start_line);
        self.buffer.move_to_first_non_blank();
        // POSIX 95159-95164: a newly loaded buffer gets its previous-context
        // mark where editing starts -- in ex mode, only if it is not empty.
        if !self.ex_standalone_mode || !self.buffer.is_empty() {
            self.set_context_mark(self.buffer.cursor());
        }
        self.undo.clear();
        if was_recovered {
            // Recovered contents are not the file's (94446-94447).
            self.buffer.mark_modified();
        }
        if let Some(note) = recover_note {
            self.set_message(&note);
        }
        // 94452-94459: the -c commands run in the first buffer loaded from
        // an existing file (or recovered), from its last line.
        if (exists || was_recovered) && !self.initial_commands.is_empty() {
            let commands = std::mem::take(&mut self.initial_commands);
            self.buffer.set_line(self.buffer.line_count().max(1));
            self.buffer.move_to_first_non_blank();
            for cmd in &commands {
                if let Err(e) = self.execute_ex_input(cmd) {
                    eprintln!("{}: {}", self.initial_prog, e);
                }
            }
        }
        Ok(())
    }

    /// Whether the edit of `path` now starting is to recover it, and what
    /// there is to recover.
    fn take_recovery(&mut self, path: &Path) -> Recovery {
        let key = crate::recover::path_key(path);
        let before = self.recover_pending.len();
        self.recover_pending.retain(|p| *p != key);
        let asked =
            std::mem::take(&mut self.recover_next_edit) || self.recover_pending.len() != before;
        if !asked {
            return Recovery::NotAsked;
        }
        let mut found = crate::recover::find_all_for(&self.options.directory, path).into_iter();
        let Some(info) = found.next() else {
            return Recovery::None;
        };
        match crate::recover::load_body(&info) {
            Ok(body) => Recovery::Found {
                body,
                info,
                older: found.count(),
            },
            Err(e) => Recovery::Failed(e),
        }
    }

    /// `-r`: recover each of `files` the first time it is edited, and with
    /// `tag`, the file the tag names.
    pub fn recover_on_first_edit(&mut self, files: &[String], tag: bool) {
        self.recover_pending = files
            .iter()
            .map(|f| crate::recover::path_key(&crate::bytes::path(f)))
            .collect();
        self.recover_next_edit = tag;
    }

    /// `-c`: commands to run in the first buffer loaded from a file that
    /// exists; errors are reported as `prog`'s.
    pub fn set_initial_commands(&mut self, prog: &'static str, commands: Vec<String>) {
        self.initial_prog = prog;
        self.initial_commands = commands;
    }

    /// Get current file as string for shell.
    fn shell_current_file(&self) -> Option<String> {
        self.files.current_file().map(crate::bytes::from_path)
    }

    /// Open multiple files.
    pub fn open_files(&mut self, paths: Vec<String>) -> Result<()> {
        if paths.is_empty() {
            return Ok(());
        }

        self.files.set_args(to_paths(&paths));

        // Open first file
        self.open(&paths[0])
    }

    /// Run the editor main loop.
    pub fn run(&mut self) -> Result<i32> {
        if self.ex_standalone_mode {
            // Ex mode: line-oriented editing
            self.run_ex_mode()
        } else {
            // Visual mode: screen-oriented editing
            self.run_visual_mode()
        }
    }

    /// Run in visual (screen) mode.
    fn run_visual_mode(&mut self) -> Result<i32> {
        self.terminal.enable_raw_mode()?;
        self.terminal.enter_alternate_screen()?;
        // React to resize / job-control resume / interrupt while editing.
        crate::signals::install_visual_handlers();

        let result = self.visual_main_loop();

        self.terminal.leave_alternate_screen()?;
        self.terminal.disable_raw_mode()?;

        // Check if we should switch to ex mode (Q was pressed)
        if self.ex_standalone_mode && !self.should_quit {
            return self.run_ex_mode();
        }

        result.map(|_| self.exit_code)
    }

    /// Run in ex (line) mode.
    fn run_ex_mode(&mut self) -> Result<i32> {
        use std::io::{self, IsTerminal, Write};

        let mut stdout = io::stdout();
        let stdin_is_tty = io::stdin().is_terminal();
        crate::signals::install_ex_handlers();

        while !self.should_quit {
            // A signal that came during the last command: a hangup ends the
            // session (94312-94316), and so does an interrupt when standard
            // input is not a terminal (94292-94293).
            if self.ex_signal_ends_session(stdin_is_tty) {
                break;
            }
            // Check if we should switch to visual mode
            if !self.ex_standalone_mode {
                // Switch to visual mode
                return self.run_visual_mode();
            }

            // Check if we're in text input mode (:a, :i, :c)
            if self.ex_insert_mode.is_some() {
                // Read text lines until we see a line containing only "."
                let mut line = String::new();
                match self.read_ex_line(&mut line, stdin_is_tty) {
                    ExRead::Eof => {
                        // EOF - finalize pending input, preserve, and quit.
                        let _ = self.finalize_ex_insert();
                        self.preserve_buffer();
                        self.should_quit = true;
                        break;
                    }
                    ExRead::Line => {}
                    // 94297-94299: the lines entered so far go into the
                    // buffer, the partial one is dropped, and it is back to
                    // the command prompt.
                    ExRead::Interrupted => {
                        let _ = self.finalize_ex_insert();
                        continue;
                    }
                    ExRead::Quit => break,
                }

                // Strip only the line terminator: in text input mode a
                // trailing <blank> the user typed is content, and trimming it
                // here discarded a line entered as blanks before
                // `accept_ex_insert_line` could tell it apart from an empty one.
                let typed = line.trim_end_matches(['\n', '\r']).to_string();
                match self.accept_ex_insert_line(&typed) {
                    Ok(true) => {
                        if !self.silent_mode {
                            if let Some(msg) = &self.message {
                                crate::bytes::println(msg);
                            }
                        }
                        self.clear_message();
                    }
                    Ok(false) => {}
                    Err(e) => {
                        if !self.silent_mode {
                            crate::bytes::eprintln(&e.to_string());
                        }
                        self.clear_message();
                    }
                }
                continue;
            }

            // Print prompt unless silent, stdin is not a tty, or `prompt` unset.
            if !self.silent_mode && stdin_is_tty && self.options.prompt {
                print!(":");
                stdout.flush()?;
            }

            // Read a line of input
            let mut line = String::new();
            match self.read_ex_line(&mut line, stdin_is_tty) {
                ExRead::Eof => {
                    // EOF on the command stream: preserve unsaved work, then quit.
                    self.preserve_buffer();
                    self.should_quit = true;
                    break;
                }
                ExRead::Line => {}
                ExRead::Interrupted => continue,
                ExRead::Quit => break,
            }

            // Remove the line terminator -- but not a <control>-V-escaped one,
            // which is an argument character (`map Q :wq^V^M`).
            let mut line = strip_line_terminator(&line).to_string();

            // A <backslash><newline> continues the command onto the next input
            // line, with the pair standing for a literal newline. This is how
            // POSIX has you enter a newline in a `substitute` replacement, e.g.
            //   s/-/\
            //   /
            // splits a line in two. Without this the reader cut the command at
            // the newline and the trailing `/` parsed as a bare search.
            let mut abandoned = false;
            while ends_with_odd_backslash(&line) {
                line.pop(); // drop the escaping backslash
                line.push('\n');
                let mut cont = String::new();
                match self.read_ex_line(&mut cont, stdin_is_tty) {
                    ExRead::Line => line.push_str(cont.trim_end_matches(['\n', '\r'])),
                    ExRead::Eof => break,
                    // A hangup or an interrupt drops the half-entered command.
                    ExRead::Interrupted | ExRead::Quit => {
                        abandoned = true;
                        break;
                    }
                }
            }
            if abandoned {
                continue;
            }
            let line = line.trim_end_matches([' ', '\t']);

            // Execute the command
            match self.execute_ex_input(line) {
                Ok(()) => {
                    // Check if we switched to visual mode
                    if !self.ex_standalone_mode {
                        continue; // Will switch on next iteration
                    }
                    // Print any message
                    if !self.silent_mode {
                        if let Some(msg) = &self.message {
                            crate::bytes::println(msg);
                        }
                    }
                }
                // 94300-94304: an interrupted command is abandoned with a
                // message; on a terminal that is not an error.
                Err(ViError::Interrupted) if stdin_is_tty => {
                    crate::bytes::eprintln("Interrupted");
                }
                Err(e) => {
                    // Errors always go to stderr, and any error makes the
                    // exit status non-zero (EXIT STATUS: ">0 An error
                    // occurred").
                    crate::bytes::eprintln(&e.to_string());
                    self.exit_code = 1;
                    // POSIX 96009-96011: with standard input not a terminal,
                    // an error ends the session -- no later write, no more
                    // commands -- whether or not -s was given.  On a
                    // terminal the user sees the message and goes on.
                    if !stdin_is_tty {
                        self.should_quit = true;
                    }
                }
            }
            self.clear_message();
        }

        Ok(self.exit_code)
    }

    /// Read a line of ex input into `line`, acting on the signals that
    /// interrupt the read: see [`ExRead`].
    fn read_ex_line(&mut self, line: &mut String, tty: bool) -> ExRead {
        loop {
            let result = read_text_line(line);
            if result.is_ok() {
                self.ex_interrupted = false;
            }
            match result {
                Ok(0) => return ExRead::Eof,
                Ok(_) => return ExRead::Line,
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {
                    if self.ex_signal_ends_session(tty) {
                        return ExRead::Quit;
                    }
                    if self.ex_interrupted {
                        self.ex_interrupted = false;
                        line.clear();
                        crate::input::lines::discard_partial();
                        return ExRead::Interrupted;
                    }
                    // Some other signal (a resize, say): read on.
                }
                Err(e) => {
                    if !self.silent_mode {
                        eprintln!("Read error: {}", e);
                    }
                    self.should_quit = true;
                    self.exit_code = 1;
                    return ExRead::Quit;
                }
            }
        }
    }

    /// Act on a hangup or interrupt received in ex mode.  A hangup preserves
    /// the buffer and ends the session, non-zero (94312-94316); so does an
    /// interrupt with standard input not a terminal, without writing
    /// (94292-94293).  An interrupt on a terminal is reported, and noted in
    /// `ex_interrupted` for the read to return to its prompt.  Returns
    /// whether the session is over.
    fn ex_signal_ends_session(&mut self, tty: bool) -> bool {
        if crate::signals::take(&crate::signals::HANGUP_RECEIVED) {
            // The text input lines entered so far are part of the buffer.
            if self.ex_insert_mode.is_some() {
                let _ = self.finalize_ex_insert();
            }
            self.preserve_buffer();
            self.should_quit = true;
            self.exit_code = 1;
            return true;
        }
        if crate::signals::take(self.interrupt) {
            crate::bytes::eprintln("Interrupted");
            if !tty {
                self.should_quit = true;
                self.exit_code = 1;
                return true;
            }
            self.ex_interrupted = true;
        }
        false
    }

    /// Visual mode main editing loop.
    fn visual_main_loop(&mut self) -> Result<()> {
        let mut reader = InputReader::new();

        while !self.should_quit && !self.ex_standalone_mode {
            self.refresh_screen()?;

            match reader.read_key() {
                Ok(key) => self.handle_key(key)?,
                // A signal interrupted the read: service it and redraw.
                Err(ViError::Interrupted) => {
                    self.handle_pending_signals()?;
                }
                // End of input: like a hangup, preserve any unsaved work.
                Err(ViError::Io(e)) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                    self.preserve_buffer();
                    self.should_quit = true;
                }
                Err(e) => return Err(e),
            }
        }

        Ok(())
    }

    /// Service any pending asynchronous signals. Returns `true` if at least one
    /// was handled (the loop should redraw and keep running).
    fn handle_pending_signals(&mut self) -> Result<bool> {
        use crate::signals::{take, HANGUP_RECEIVED, SIGCONT_RECEIVED, SIGWINCH_RECEIVED};

        if take(&HANGUP_RECEIVED) {
            // Hangup/termination: preserve the buffer and exit.
            self.preserve_buffer();
            self.should_quit = true;
            self.exit_code = 1;
            return Ok(true);
        }

        let mut handled = false;

        if take(&SIGCONT_RECEIVED) {
            // Resumed from job-control stop: re-establish terminal state.
            self.terminal.enable_raw_mode()?;
            self.terminal.enter_alternate_screen()?;
            self.terminal.refresh_size()?;
            handled = true;
        }
        if take(&SIGWINCH_RECEIVED) {
            // Window resized: refresh_screen() will redraw at the new size.
            self.terminal.refresh_size()?;
            handled = true;
        }
        if take(self.interrupt) {
            self.interrupt_command();
            handled = true;
        }
        Ok(handled)
    }

    /// Handle an interrupt in command mode: discard any partial command and
    /// alert the terminal (POSIX vi command-mode interrupt behavior).
    fn interrupt_command(&mut self) {
        self.parser.reset();
        let _ = self.terminal.bell();
    }

    /// Write the buffer to a recovery file if it has unsaved changes, mailing
    /// a notification. Returns the recovery path when a copy was saved.
    pub fn preserve_buffer(&mut self) -> Option<std::path::PathBuf> {
        if !self.buffer.is_modified() {
            return None;
        }
        let orig = self.files.current_file().map(|p| p.to_path_buf());
        match crate::recover::preserve(
            orig.as_deref(),
            &self.buffer.as_text(),
            &self.options.directory,
        ) {
            Ok(path) => {
                crate::recover::notify(orig.as_deref());
                Some(path)
            }
            Err(_) => None,
        }
    }

    /// After `-t`: the argument list is the file operands, with the tag's
    /// file put first and current -- or, if the tag gave no file, the
    /// reference before the first operand (94460-94469).
    pub fn set_args_after_tag(&mut self, files: &[String]) {
        let operands = to_paths(files);
        match self.files.current_file().map(|p| p.to_path_buf()) {
            Some(tag_file) => {
                let args = std::iter::once(tag_file).chain(operands).collect();
                self.files.set_args(args);
            }
            None => self.files.set_args_before_first(operands),
        }
    }

    /// Accept one key and run the editor until nothing is left to do.
    ///
    /// The key does not necessarily act at once: a `:map` whose left-hand side
    /// is more than one key long holds it back until the match resolves, and a
    /// map that fires puts its replacement in front of whatever was waiting.
    /// Draining here rather than in the read loop is what makes the headless
    /// harness and the real editor take the same path.
    fn handle_key(&mut self, key: Key) -> Result<()> {
        self.input.push_typed(key);
        self.drain_input()
    }

    /// Dispatch queued keys until the queue empties or a match needs more input.
    fn drain_input(&mut self) -> Result<()> {
        loop {
            // ex.md 96616-96617: a map defined in terms of itself loops, POSIX
            // "requires conformance to historical practice, and that such loops
            // be interruptible". So the escape is a signal, not a depth cap --
            // a cap would refuse a mapping the spec says must work.
            if crate::signals::take(self.interrupt) {
                self.abort_expansion();
                return Ok(());
            }

            let Some(q) = self.next_input_key() else {
                self.input.set_current(KeySource::Typed);
                self.close_expansion_group();
                return Ok(());
            };
            self.input.set_current(q.source);

            // 95443-95445: everything one expansion produces is one command for
            // `u`. Opened on the first expansion key and closed when the queue
            // drains, so nested groups an inner command opens (a change
            // operator and its insert session, say) nest rather than closing
            // this one early.
            if q.source.is_expansion() && !self.expansion_group {
                self.undo.begin_group();
                self.expansion_group = true;
            }

            // An abbreviation fires *before* its trigger is inserted, so the
            // check goes here rather than inside `process_insert_key`.
            if self.expand_abbreviation(&q)? {
                continue;
            }

            // Clear any previous message
            if !matches!(q.key, Key::Char(':')) {
                self.clear_message();
            }

            // Marks a command drops go to the undo record it makes; any left
            // over from before are not this command's.  Not between the keys
            // of an insert session: its drops belong to the record its ESC
            // makes.
            if matches!(self.mode, Mode::Command) {
                self.buffer.take_dropped_marks();
            }
            let result = match &self.mode {
                Mode::Command => self.handle_command_key(q.key),
                Mode::Insert(_) | Mode::Replace | Mode::Open => self.handle_insert_key(q.key),
                Mode::Ex => self.handle_ex_key(q.key),
            };

            // vi.md 120619-120627. `self.is_error` has to be consulted as well
            // as the `Result`: most of `execute_command` reports through
            // `set_error` rather than returning `Err`, so testing the `Result`
            // alone would miss the majority of vi command errors.
            if result.is_err() || self.is_error {
                self.abort_expansion();
            }
            if result.is_err() {
                self.close_expansion_group();
            }
            result?;
        }
    }

    /// Which `:map` table applies right now, or `None` where no map does.
    ///
    /// POSIX scopes both tables to "open or visual" mode (95090-95092): command
    /// mode takes `:map`, text input mode takes `:map!`. Two contexts are
    /// deliberately absent. The colon line is one — 96496-96509 permits not
    /// expanding there and forbids the two behaviours that expanding the first
    /// argument of `:ab`/`:una` historically caused, so not expanding is both
    /// the safe reading and the simple one. Ex text input mode (`:a`, `:i`,
    /// `:c`) is the other: it is neither open nor visual, and it does not reach
    /// this function at all, since `handle_ex_key` owns it.
    fn active_map_mode(&self) -> Option<MapMode> {
        match self.mode {
            Mode::Command => Some(MapMode::Command),
            Mode::Insert(_) | Mode::Replace | Mode::Open => Some(MapMode::Insert),
            Mode::Ex => None,
        }
    }

    /// Whether a left-hand side may be matched at this point in a command.
    ///
    /// 95094-95095 says a command-mode map fires when its lhs is entered "as
    /// any part of a vi command (but not as part of the arguments to the
    /// command)", and 96598-96600 gives the case that pins it: with `x` mapped
    /// to `y`, "the command fx searched for the 'x' character, not the 'y'
    /// character. POSIX.1-2024 requires this behavior."
    ///
    /// `CommandParser` already knows when it is waiting for an argument, so
    /// this is a question about its state and needs no bookkeeping of its own.
    /// A count is *not* an argument: 96607-96608 requires that a digit lhs
    /// work, so `Count` and `Operator` stay eligible.
    fn in_argument_position(&self) -> bool {
        matches!(
            self.parser.state(),
            ParserState::Register | ParserState::WaitingChar | ParserState::MotionWaitingChar
        )
    }

    /// The next key to act on, expanding maps along the way.
    ///
    /// `None` means the keys in hand are a strict prefix of some left-hand side
    /// and the match cannot be resolved without more input (95116-95118, where
    /// the wait is explicitly unspecified). Under shortest-match that can only
    /// happen when nothing has matched exactly, so waiting for the next
    /// keystroke resolves it and no timer is needed.
    fn next_input_key(&mut self) -> Option<QueuedKey> {
        loop {
            let Some(q) = self.input.pop() else {
                // Nothing more is coming from an expansion, so a held prefix
                // will never complete. Give it up.
                return self.input.release_partial();
            };

            // A key that cannot begin a match goes straight through -- but not
            // past keys already held, or it would overtake them.
            let literal = self.input.take_literal_next();
            let Some(mode) = self.active_map_mode() else {
                return Some(self.flush_before(q));
            };
            // 95110-95111: in text input mode too, "if any character in the
            // input text is escaped using a <control>-V character, that
            // character shall not be part of a match to an lhs".
            // `process_insert_key` owns that flag and consumes it on the very
            // next key, which is the one in hand here.
            let insert_literal = self
                .insert_state
                .as_ref()
                .is_some_and(|s| s.pending_literal);
            if literal || insert_literal || !q.remappable || self.in_argument_position() {
                return Some(self.flush_before(q));
            }

            // A command-mode `^V` means the next key matches no lhs
            // (95097-95098, permitted for the first character by 96600-96603).
            if mode == MapMode::Command
                && matches!(q.key, Key::Ctrl('v') | Key::Ctrl('q'))
                && self.input.partial().is_empty()
            {
                self.input.set_literal_next();
                continue;
            }

            self.input.hold(q);
            let keys: Vec<Key> = self.input.partial().iter().map(|k| k.key).collect();
            match self.maps.table(mode).probe(&keys) {
                MapMatch::Full(i) => {
                    self.expand_map(mode, i);
                    self.input.clear_partial();
                }
                // More lookahead is already available, so keep consuming it;
                // otherwise hold and wait for the user.
                MapMatch::Partial if self.input.has_queued() => continue,
                MapMatch::Partial => return None,
                MapMatch::NoMatch => return self.input.release_partial(),
            }
        }
    }

    /// Release any held keys before `q`, which cannot join a match.
    fn flush_before(&mut self, q: QueuedKey) -> QueuedKey {
        if self.input.partial().is_empty() {
            return q;
        }
        self.input.push_front(std::iter::once(q));
        self.input
            .release_partial()
            .expect("partial was just checked non-empty")
    }

    /// Expand an abbreviation if this key triggers one.
    ///
    /// POSIX 94870-94873: "In open and visual text input mode, if a non-word or
    /// <ESC> character that is not escaped by a <control>-V character is
    /// entered after a word character, a check shall be made for a set of
    /// characters matching lhs, in the text input entered during this command.
    /// If it is found, the effect shall be as if rhs was entered instead of
    /// lhs."
    ///
    /// Returns `true` when it fired, in which case the caller must not dispatch
    /// the trigger: it has been requeued behind the replacement.
    fn expand_abbreviation(&mut self, q: &QueuedKey) -> Result<bool> {
        if !q.abbrevable || !matches!(self.mode, Mode::Insert(_) | Mode::Replace | Mode::Open) {
            return Ok(false);
        }
        // A key consumed after `^V` is text, not a trigger (94870-94871).
        let Some(state) = self.insert_state.as_ref() else {
            return Ok(false);
        };
        if state.pending_literal {
            return Ok(false);
        }

        // Only keys that actually enter text, plus <ESC>. The erase and kill
        // commands are non-word characters by a literal reading of 94870, but
        // they are editing commands rather than entered text, and historical
        // vi does not check on them.
        let triggers = match q.key {
            Key::Escape | Key::Ctrl('[') => true,
            Key::Char(c) => !crate::command::is_word_char(c),
            Key::Tab | Key::Enter | Key::Ctrl('j') => true,
            _ => false,
        };
        if !triggers {
            return Ok(false);
        }

        let Some(word) = crate::maps::abbrev_candidate(&state.input_log) else {
            return Ok(false);
        };
        let Some(entry) = self.maps.abbrev.lookup(&word) else {
            return Ok(false);
        };
        let rhs = entry.rhs.clone();
        let exempt = entry.no_reabbrev;

        // Take the matched word back out of the buffer and the session record
        // together, so `.` repeat and the insert-session undo entry both see
        // the replacement rather than the trigger word.
        let n = word.chars().count();
        let mut state = self.insert_state.take().expect("checked above");
        crate::mode::erase_input(&mut self.buffer, &mut state, n);
        self.insert_state = Some(state);

        // 96473-96476: the replacement is "logically pushed onto the terminal
        // input queue", not substituted -- so it is itself subject to maps and
        // to further abbreviation. The trigger goes back behind it, to be
        // entered after the replacement as POSIX describes.
        let mut keys: Vec<QueuedKey> = rhs
            .iter()
            .enumerate()
            .map(|(i, &key)| QueuedKey {
                key,
                remappable: true,
                // The leading run that repeats the lhs is exempt, or `:ab foo
                // foo` would re-trigger on its own replacement.
                abbrevable: i >= exempt,
                source: KeySource::MapExpansion,
            })
            .collect();
        keys.push(QueuedKey {
            key: q.key,
            remappable: q.remappable,
            abbrevable: false,
            source: q.source,
        });
        self.input.push_front(keys.into_iter());
        Ok(true)
    }

    /// Put a matched map's replacement at the front of the queue.
    fn expand_map(&mut self, mode: MapMode, index: usize) {
        let entry = self.maps.table(mode).entry(index);
        // 95119-95121: the replacement is itself remapped, "except that if the
        // characters in lhs occur as prefix characters in rhs, those characters
        // shall not be remapped". With `remap` unset the whole replacement is
        // exempt, which is 95874-95875's "only a one-step translation".
        let exempt = if self.options.remap {
            entry.no_remap_prefix
        } else {
            entry.rhs.len()
        };
        let keys: Vec<QueuedKey> = entry
            .rhs
            .iter()
            .enumerate()
            .map(|(i, &key)| QueuedKey {
                key,
                remappable: i >= exempt,
                abbrevable: true,
                source: KeySource::MapExpansion,
            })
            .collect();
        self.input.push_front(keys.into_iter());
    }

    /// Discard everything an erroring command should not have led to.
    ///
    /// vi.md 120619-120627 in one place: alert the terminal, drop any partially
    /// entered command, drop the rest of a map expansion, and stop a buffer
    /// execution. The queue holds all three kinds of pending key, so one
    /// `discard` covers the last two.
    fn abort_expansion(&mut self) {
        if self.input.is_expanding() {
            let _ = self.terminal.bell();
        }
        self.input.discard();
        self.parser.reset();
        self.close_expansion_group();
    }

    /// Close the undo group opened around an expansion, if one is open.
    fn close_expansion_group(&mut self) {
        if self.expansion_group {
            self.expansion_group = false;
            self.undo.end_group();
        }
    }

    /// Handle a key in command mode.
    fn handle_command_key(&mut self, key: Key) -> Result<()> {
        // Snapshot the current line for `U` before this command can change it
        // (#V20). Between commands nothing touches the buffer, so the content
        // here is the content as of when the cursor arrived on the line —
        // which is exactly what POSIX 121567-121568 restores. The `:` keystroke
        // is itself a command key, so ex commands get the right pre-command
        // snapshot for free.
        self.undo.sync_line_original(&self.buffer);

        // Handle control key commands first.  The scrolling ones take the
        // count typed before them (a count left in the parser would
        // otherwise go to the next command); in the middle of any other
        // command they are an error.
        if matches!(
            key,
            Key::Ctrl('b' | 'f' | 'd' | 'u' | 'e' | 'y') | Key::PageUp | Key::PageDown
        ) {
            let has_count = matches!(self.parser.state(), ParserState::Count);
            if !self.parser.is_ready() && !has_count {
                self.parser.reset();
                let _ = self.terminal.bell();
                return Ok(());
            }
            let count = self.parser.get_count().max(1);
            self.parser.reset();
            match key {
                // A count of screens or lines; ^D and ^U make it the new
                // `scroll` amount.
                Key::Ctrl('b') | Key::PageUp => self.repeat_scroll(count, Self::page_up)?,
                Key::Ctrl('f') | Key::PageDown => self.repeat_scroll(count, Self::page_down)?,
                Key::Ctrl('d') | Key::Ctrl('u') => {
                    if has_count {
                        self.options.scroll = count;
                    }
                    if key == Key::Ctrl('d') {
                        self.scroll_down()?;
                    } else {
                        self.scroll_up()?;
                    }
                }
                Key::Ctrl('e') => self.repeat_scroll(count, Self::scroll_line_down)?,
                _ => self.repeat_scroll(count, Self::scroll_line_up)?,
            }
            return Ok(());
        }
        match key {
            Key::Ctrl('g') => {
                // Display file info
                let info = self.file_info();
                self.set_message(&info);
                return Ok(());
            }
            Key::Ctrl('c') => {
                // Interrupt (raw mode delivers ^C as a byte): discard the
                // partial command and ring the bell.
                self.interrupt_command();
                return Ok(());
            }
            Key::Ctrl('l') => {
                // ^L: clear the physical screen, then redraw on next refresh.
                let _ = self.terminal.clear_screen();
                return Ok(());
            }
            Key::Ctrl('r') => {
                // ^R: redraw (refresh happens at the top of the loop).
                return Ok(());
            }
            Key::Ctrl(']') => {
                // Tag jump - get word under cursor and jump to tag.
                //
                // The error must be reported, not discarded: `goto_tag`
                // returns `Err(FileModified)` when the tag lives in another
                // file and this one has unsaved changes, and swallowing it
                // made `^]` do nothing at all, silently. It must also not
                // escape -- an `Err` out of `handle_key` unwinds to
                // `run_editor`, which prints and exits the editor.
                if let Some(tag) = self.word_under_cursor() {
                    // 95148: ^] sets the previous-context mark -- in this
                    // buffer; a jump to another file starts afresh there.
                    let before = self.buffer.cursor();
                    let generation = self.buffer_generation;
                    let outcome = self.goto_tag(&tag, false);
                    if outcome.is_ok()
                        && self.buffer_generation == generation
                        && self.buffer.cursor() != before
                    {
                        self.set_context_mark(before);
                    }
                    if let Err(e) = outcome {
                        self.set_error(&e.to_string());
                    }
                }
                return Ok(());
            }
            Key::Ctrl('t') => {
                // ^T: return to where the last tag jump started, the companion
                // to ^] above.
                //
                // Not POSIX, and the only non-POSIX command-mode key here:
                // vi.md 121838-121840 gives ^T a meaning in *text input* mode
                // only, where it shifts the autoindent and `mode::insert`
                // implements it. This arm is reached in command mode alone, so
                // the two do not collide. See NONPOSIX.md.
                if let Err(e) = self.pop_tag() {
                    self.set_error(&e.to_string());
                }
                return Ok(());
            }
            Key::Ctrl('^') => {
                // Edit the alternate file. This discards the buffer, so it
                // needs the same warning `:e` gives -- it used to open
                // unconditionally and the unsaved work went with it.
                if self.buffer.is_modified() {
                    self.set_error("No write since last change");
                } else if let Some(alt) = self.files.alternate_file() {
                    let path = crate::bytes::from_path(alt);
                    if let Err(e) = self.open(&path) {
                        self.set_error(&e.to_string());
                    }
                } else {
                    self.set_error("No alternate file");
                }
                return Ok(());
            }
            _ => {}
        }

        // Check for mode-changing commands ONLY when parser is ready
        // (i.e., not in the middle of parsing a compound command like "fo")
        if self.parser.is_ready() {
            match key {
                Key::Char(':') => {
                    self.mode = Mode::Ex;
                    self.ex_prompt = ':';
                    self.ex_input.clear();
                    return Ok(());
                }
                Key::Char('/') => {
                    self.mode = Mode::Ex;
                    self.ex_prompt = '/';
                    self.ex_input = "/".to_string();
                    return Ok(());
                }
                Key::Char('?') => {
                    self.mode = Mode::Ex;
                    self.ex_prompt = '?';
                    self.ex_input = "?".to_string();
                    return Ok(());
                }
                _ => {}
            }
        }

        // Feed key to command parser
        if !self.parser.process_key(key) {
            if let Some(cmd) = self.parser.get_command() {
                self.execute_command(&cmd)?;
            }
            self.parser.reset();
        }

        Ok(())
    }

    /// Run `f` as one undo group, closed whether or not it fails.  An insert
    /// session `f` opens holds the group open itself until its ESC, so the
    /// operator and the text typed are one `u` (and no group leaks).
    fn grouped(&mut self, f: impl FnOnce(&mut Self) -> Result<()>) -> Result<()> {
        self.undo.begin_group();
        let result = f(self);
        self.undo.end_group();
        result
    }

    /// Enter insert mode.
    fn enter_insert(&mut self, kind: InsertKind) {
        self.enter_insert_counted(kind, 1);
    }

    /// Enter insert mode for a command given `count`: on ESC the input is
    /// repeated to make `count` copies.
    fn enter_insert_counted(&mut self, kind: InsertKind, count: usize) {
        // An insert session is one command.  When an operator opened a group
        // already (c/s/S/C), this nests inside it: the operator closes its
        // level, and ESC closes this one and with it the group, so one `u`
        // reverses the removal and the typed text together.
        self.undo.begin_group();

        // `o` and `O` create their line inside `enter_insert_mode`, which used
        // to run before the group existed -- so `oZ` then `u` removed the `Z`
        // and left the blank line behind.  Record that line as part of the
        // session.
        let opens_a_line = matches!(kind, InsertKind::OpenBelow | InsertKind::OpenAbove);
        let line = self.buffer.cursor().line;
        let entered = if opens_a_line {
            self.splice_around(line, line, |ed| {
                enter_insert_mode(&mut ed.buffer, kind, &ed.options)
            })
        } else {
            enter_insert_mode(&mut self.buffer, kind, &self.options)
        };

        if let Ok(mut state) = entered {
            // Carry the terminal's erase/kill characters and the previous
            // insertion into the session (#V12, #V15).
            state.erase_char = self.terminal.erase_char();
            state.kill_char = self.terminal.kill_char();
            state.previous_insert = self.previous_insert.clone();
            state.count = count.max(1);
            self.mode = if kind == InsertKind::Replace {
                Mode::Replace
            } else {
                Mode::Insert(kind)
            };
            self.insert_state = Some(state);
        } else {
            // Nothing will close the group otherwise, and a leaked group
            // swallows every later change into the same `u`.
            self.undo.end_group();
        }
    }

    /// Handle a key in insert mode.
    fn handle_insert_key(&mut self, key: Key) -> Result<()> {
        // See `handle_command_key` — these two are the complete set of entry
        // points, since every key reaches the editor through `drain_input`,
        // which dispatches to one of them.
        self.undo.sync_line_original(&self.buffer);

        if let Some(mut state) = self.insert_state.take() {
            let should_exit = process_insert_key(&mut self.buffer, key, &mut state, &self.options)?;

            if should_exit {
                // Exited insert mode
                self.mode = Mode::Command;
                // Remember this session's text so a NUL in the next one can
                // re-insert it (#V15).
                if !state.inserted_text.is_empty() {
                    self.previous_insert = state.inserted_text.clone();
                }
                self.record_insert_session(&state);
                self.undo.end_group();
                // Save for . (dot) repeat: the command that began the
                // session, if one did, so `.` repeats its change as well.
                let text = state.inserted_text.clone();
                self.last_command = Some(match self.insert_origin.take() {
                    Some(cmd) => LastCommand::Input { cmd, text },
                    None => LastCommand::Insert {
                        kind: state.kind,
                        text,
                        count: state.count,
                    },
                });
            } else {
                // Stay in insert mode
                self.insert_state = Some(state);
            }
        }

        Ok(())
    }

    /// Record an insert session for undo: the line it began on, against
    /// the lines that became of it -- whatever it typed, erased, overwrote
    /// (`R`) or repeated (a count), and however many lines it split into.
    fn record_insert_session(&mut self, state: &InsertState) {
        let first = state.first_line;
        let last = self.buffer.cursor().line.max(first);
        let new: Vec<String> = (first..=last)
            .filter_map(|n| self.buffer.line(n))
            .map(|l| l.content().to_string())
            .collect();
        if new.len() != 1 || new[0] != state.orig_line {
            self.undo.note_dropped(self.buffer.take_dropped_marks());
            self.undo.record_replace_lines(
                Position::new(first, state.start_pos.column),
                std::slice::from_ref(&state.orig_line),
                &new,
            );
        }
    }

    /// Handle a key in ex mode.
    fn handle_ex_key(&mut self, key: Key) -> Result<()> {
        // Check if we're in ex text input mode (:a, :i, :c)
        if self.ex_insert_mode.is_some() {
            return self.handle_ex_insert_key(key);
        }

        // A pending ^V quotes this key, whatever it is. Both the marker and the
        // quoted character go into `ex_input`: the ex parser needs the marker
        // to tell an escaped <blank> from a delimiting one (94657-94659), and
        // discards it once it has (95086-95088).
        //
        // Without this there was no way to type a ^V on the colon line at all.
        // `Key::Ctrl('v')` fell into the `_ => {}` arm below and vanished, so
        // `:map Q :wq^V^M` -- the most common mapping there is -- could not be
        // entered. In ex standalone mode the 0x16 byte is already in the line
        // read from stdin, so both paths reach the parser in the same shape.
        if self.ex_pending_literal {
            self.ex_pending_literal = false;
            if let Some(c) = key.literal_char() {
                self.ex_input.push(CTRL_V);
                self.ex_input.push(c);
            }
            return Ok(());
        }

        match key {
            Key::Ctrl('v') | Key::Ctrl('q') => {
                self.ex_pending_literal = true;
            }
            Key::Enter => {
                let input = std::mem::take(&mut self.ex_input);
                self.mode = Mode::Command;
                // A failing ex command reports on the status line; it must not
                // propagate. `handle_key` unwinds all the way to `run_editor`,
                // which prints and exits -- so letting `:q` on a modified
                // buffer return `FileModified` here quit vi and discarded the
                // very work the warning exists to protect.
                // A `/` or `?` prompt is a search; a `:` line is an ex
                // command, where `/re/` is an address like any other.
                let before = self.buffer.cursor();
                if let Some(op) = self.pending_search_op.take() {
                    self.search_as_motion(op, &input);
                    return Ok(());
                }
                let outcome = match (self.ex_prompt, input.chars().next()) {
                    ('/', Some('/')) => self.ex_search(&input[1..], '/', SearchDirection::Forward),
                    ('?', Some('?')) => self.ex_search(&input[1..], '?', SearchDirection::Backward),
                    _ => self.execute_ex_input(&input),
                };
                // POSIX 95151: a `/` or `?` that moves the cursor sets the
                // previous-context mark to where it was.
                if matches!(self.ex_prompt, '/' | '?')
                    && outcome.is_ok()
                    && self.buffer.cursor() != before
                {
                    self.set_context_mark(before);
                }
                self.ex_prompt = ':';
                if let Err(e) = outcome {
                    self.set_error(&e.to_string());
                }
            }
            Key::Escape => {
                self.ex_input.clear();
                self.ex_pending_literal = false;
                self.pending_search_op = None;
                self.ex_prompt = ':';
                self.mode = Mode::Command;
            }
            Key::Backspace | Key::Delete => {
                // Erasing the prompt itself -- nothing for `:`, the `/` or
                // `?` a search line starts with -- leaves the line.
                let at_prompt = if self.ex_prompt == ':' {
                    self.ex_input.is_empty()
                } else {
                    self.ex_input.chars().count() <= 1
                };
                if at_prompt {
                    self.ex_input.clear();
                    self.pending_search_op = None;
                    self.ex_prompt = ':';
                    self.mode = Mode::Command;
                } else {
                    // A quoted character is two chars in the buffer but one
                    // keystroke to the user, so erase both.
                    self.ex_input.pop();
                    if self.ex_input.ends_with(CTRL_V) {
                        self.ex_input.pop();
                    }
                }
            }
            Key::Char(c) => {
                self.ex_input.push(c);
            }
            _ => {}
        }
        Ok(())
    }

    /// Run operator `op` over the search typed at its `/` or `?` prompt
    /// (`input`, with the prompt character first): from the cursor to the
    /// match, exclusive -- or, with a line offset, whole lines to the line
    /// it names.  The search sets the pattern `n` repeats; the operator is
    /// recorded as `op` with the motion `n`, so `.` searches again.
    fn search_as_motion(&mut self, mut op: crate::command::ParsedCommand, input: &str) {
        let delim = self.ex_prompt;
        self.ex_prompt = ':';
        let Some(body) = input.strip_prefix(delim) else {
            return;
        };
        let dir = if delim == '/' {
            SearchDirection::Forward
        } else {
            SearchDirection::Backward
        };
        let start = self.buffer.cursor();
        let (_, offset) = split_search(body, delim);
        let outcome = self.ex_search(body, delim, dir);
        self.buffer.set_cursor(start);
        if let Err(e) = outcome {
            self.set_error(&e.to_string());
            return;
        }
        // An empty pattern reuses the last one but not its direction: the
        // prompt says which way, so the motion is `N` if that differs.
        let same_way = self.search.direction() == dir;
        if let Some(mot) = op.motion.as_mut() {
            if offset == 0 {
                mot.motion = if same_way { 'n' } else { 'N' };
            } else {
                mot.motion = '\0';
                self.search_motion = Some((same_way, offset));
            }
        }
        if let Err(e) = self.execute_command(&op) {
            self.set_error(&e.to_string());
        }
    }

    /// Enter ex text input mode, seeding the first input line's autoindent.
    ///
    /// "If the autoindent edit option is set, ex shall prompt for input using
    /// autoindent characters" (94705-94707), and a `!` on the command toggles
    /// the option for the duration of that command only.
    fn begin_ex_insert(&mut self, mode: ExInsertMode, toggle_autoindent: bool) {
        self.ex_insert_autoindent = self.options.autoindent != toggle_autoindent;
        self.ex_insert_indent.clear();
        if self.ex_insert_autoindent {
            // Indent from the line the input follows, which for `i` and `c` is
            // the line before the insertion point.
            let source = match &mode {
                ExInsertMode::Append(line) => *line,
                ExInsertMode::Insert(line) => line.saturating_sub(1),
                ExInsertMode::Change { start, .. } => start.saturating_sub(1),
            };
            self.ex_insert_indent = self
                .buffer
                .line(source)
                .map(|l| leading_blanks(l.content()).to_string())
                .unwrap_or_default();
        }
        self.ex_insert_mode = Some(mode);
    }

    /// Accept one completed line of ex text input.
    ///
    /// `typed` is what the user entered for this line; the autoindent characters
    /// are held separately in `ex_insert_indent` and prepended below, because ex
    /// supplies them as a prompt rather than the user typing them. Returns true
    /// when the terminator was seen and the collected text has been inserted.
    ///
    /// "Terminate the current line. If there are no characters other than
    /// autoindent characters on the line, all characters on the line shall be
    /// discarded. Prompt for text input on a new line ... If the autoindent edit
    /// option is set, an appropriate number of autoindent characters shall be
    /// added as a prefix to the line." (94742-94746)
    fn accept_ex_insert_line(&mut self, typed: &str) -> Result<bool> {
        // The terminator is tested against what the user actually entered: the
        // autoindent characters are a prompt ex supplied, not input, so an
        // indented `.` still ends the text.
        if typed == "." {
            self.finalize_ex_insert()?;
            return Ok(true);
        }
        if !self.ex_insert_autoindent {
            self.ex_insert_text.push(typed.to_string());
            return Ok(false);
        }
        let indent = std::mem::take(&mut self.ex_insert_indent);
        // "If there are no characters other than autoindent characters on the
        // line, all characters on the line shall be discarded." Since `typed`
        // excludes the autoindent prefix, that condition is exactly "the user
        // entered nothing" -- testing `trim()` instead also threw away a line
        // the user deliberately typed as blanks.
        let line = if typed.is_empty() {
            String::new()
        } else {
            format!("{indent}{typed}")
        };
        // Carry the level forward from the line as it was entered, so a line
        // that held nothing but autoindent does not reset it.
        self.ex_insert_indent = if line.is_empty() {
            indent
        } else {
            leading_blanks(&line).to_string()
        };
        self.ex_insert_text.push(line);
        Ok(false)
    }

    /// Handle a key in ex text input mode (:a, :i, :c).
    fn handle_ex_insert_key(&mut self, key: Key) -> Result<()> {
        match key {
            Key::Enter => {
                let typed = std::mem::take(&mut self.ex_input);
                self.accept_ex_insert_line(&typed)?;
            }
            Key::Escape => {
                // Cancel ex insert mode
                self.ex_insert_mode = None;
                self.ex_insert_autoindent = false;
                self.ex_insert_indent.clear();
                self.ex_insert_text.clear();
                self.ex_input.clear();
                self.mode = Mode::Command;
            }
            Key::Backspace | Key::Delete => {
                self.ex_input.pop();
            }
            Key::Char(c) => {
                self.ex_input.push(c);
            }
            _ => {}
        }
        Ok(())
    }

    /// Finalize ex insert mode - insert collected text into buffer.
    fn finalize_ex_insert(&mut self) -> Result<()> {
        use crate::buffer::Line;

        if let Some(mode) = self.ex_insert_mode.take() {
            self.ex_insert_autoindent = false;
            self.ex_insert_indent.clear();
            let lines = std::mem::take(&mut self.ex_insert_text);

            match mode {
                ExInsertMode::Append(line) => {
                    // Insert after specified line
                    for (i, content) in lines.iter().enumerate() {
                        self.buffer
                            .insert_line_after(line + i, Line::from(content.as_str()));
                    }
                    if !lines.is_empty() {
                        self.buffer.set_line(line + 1);
                    }
                }
                ExInsertMode::Insert(line) => {
                    // Insert before specified line
                    let insert_at = if line > 0 { line - 1 } else { 0 };
                    for (i, content) in lines.iter().enumerate() {
                        self.buffer
                            .insert_line_after(insert_at + i, Line::from(content.as_str()));
                    }
                    if !lines.is_empty() {
                        self.buffer.set_line(line);
                    }
                }
                ExInsertMode::Change { start, end } => {
                    // Delete the range first
                    for _ in start..=end {
                        if start <= self.buffer.line_count() {
                            self.buffer.delete_line(start);
                        }
                    }
                    // Insert new text
                    let insert_at = start.saturating_sub(1);
                    for (i, content) in lines.iter().enumerate() {
                        self.buffer
                            .insert_line_after(insert_at + i, Line::from(content.as_str()));
                    }
                    if !lines.is_empty() {
                        self.buffer.set_line(start);
                    }
                }
            }

            self.set_message(&format!("{} lines", lines.len()));
        }

        self.ex_input.clear();
        self.mode = Mode::Command;
        Ok(())
    }

    /// Execute a parsed command from the parser.
    fn execute_command(&mut self, cmd: &crate::command::ParsedCommand) -> Result<()> {
        // POSIX 95148-95152: these commands set the previous-context mark
        // to where the cursor was (as commands; as an operator's motion they
        // go through `execute_motion` instead).  `' G H L M z` only when the
        // current line changes (rule 6); the rest when the line or column
        // does -- for `% ( ) [ ] { }` (rule 5) a success that stays put is
        // not told apart from a failure, which must not set it.
        let line_only = match cmd.command {
            '\'' | 'G' | 'H' | 'L' | 'M' | 'z' => true,
            '%' | '(' | ')' | '{' | '}' | '[' | ']' | '`' | 'n' | 'N' => false,
            _ => {
                // An operator's f/F/t/T is the last find too (`dt,` then `;`).
                if let Some(mot) = &cmd.motion {
                    if let (Some(c), 'f' | 'F' | 't' | 'T') = (mot.char_arg, mot.motion) {
                        self.last_find = Some(FindCommand::new(mot.motion, c));
                    }
                }
                let was_inserting = self.insert_state.is_some();
                let result = self.execute_command_inner(cmd);
                if !was_inserting && self.insert_state.is_some() {
                    self.insert_origin = Some(cmd.clone());
                }
                return result;
            }
        };
        // Set once the command has moved the cursor -- not before, or `''`
        // would read back the position it was about to leave.
        let before = self.buffer.cursor();
        let generation = self.buffer_generation;
        let result = self.execute_command_inner(cmd);
        let after = self.buffer.cursor();
        let moved = if line_only {
            after.line != before.line
        } else {
            after != before
        };
        if result.is_ok() && moved && self.buffer_generation == generation {
            self.set_context_mark(before);
        }
        result
    }

    fn execute_command_inner(&mut self, cmd: &crate::command::ParsedCommand) -> Result<()> {
        let count = cmd.count;

        // An operator whose motion is a search reads it at the `/` or `?`
        // prompt first; the operator runs when the search is entered.
        if let Some(m @ ('/' | '?')) = cmd.motion.as_ref().map(|m| m.motion) {
            self.pending_search_op = Some(cmd.clone());
            self.mode = Mode::Ex;
            self.ex_prompt = m;
            self.ex_input = m.to_string();
            return Ok(());
        }

        match cmd.command {
            // Cursor motions: one table, `motion`, shared with the
            // operators' motions.
            'h' | 'l' | ' ' | 'j' | 'k' | 'w' | 'W' | 'b' | 'B' | 'e' | 'E' | '0' | '^' | '$'
            | 'G' | 'H' | 'M' | 'L' | '%' | 'f' | 'F' | 't' | 'T' | ';' | ',' | 'n' | 'N'
            | '\'' | '`' | '[' | ']' | '{' | '}' | '(' | ')' | '_' | '|' | '+' | '-' | '\r' => {
                self.move_cursor(cmd);
            }
            // Enter ex mode (Q = leave visual, enter line-oriented ex mode)
            'Q' => {
                // Set flag to exit visual loop and enter ex standalone mode
                self.ex_standalone_mode = true;
                self.mode = Mode::Command; // Reset mode
            }
            // Redraw window command
            'z' => {
                if let Some(arg) = cmd.char_arg {
                    self.redraw_window(arg, count)?;
                }
            }
            // Delete commands
            'x' => {
                // Snapshot the line rather than record a flat delete: with a
                // count that runs off the end of the line the cursor is
                // clamped part-way through the loop, so the recorded position
                // no longer described the text and `u` put it back displaced.
                let line = self.buffer.cursor().line;
                let register = cmd.register;
                self.splice_around(line, line, |ed| {
                    let mut deleted = String::new();
                    for _ in 0..count {
                        match ed.buffer.delete_char() {
                            Some(c) => deleted.push(c),
                            None => break,
                        }
                    }
                    if !deleted.is_empty() {
                        // Through the register policy, not straight to the
                        // small-delete slot: writing there directly is what
                        // made `"qx` ignore the register the user named.
                        ed.registers
                            .delete(register, RegisterContent::new(deleted, false), true);
                    }
                    Ok(())
                })?;
            }
            'X' => {
                // POSIX (vi, "Delete Character Before Cursor"): "If there are
                // no characters before the cursor in the line, it shall be an
                // error."  It used `delete_char_before`, which joins with the
                // previous line -- right for an insert-mode backspace, wrong
                // here -- so `X` at column 0 silently merged two lines and no
                // within-line undo record could express it.
                let line = self.buffer.cursor().line;
                let register = cmd.register;
                self.splice_around(line, line, |ed| {
                    let mut deleted = String::new();
                    for _ in 0..count {
                        if ed.buffer.cursor().column == 0 {
                            break;
                        }
                        if let Some(c) = ed.buffer.delete_char_before() {
                            deleted.push(c);
                        }
                    }
                    if !deleted.is_empty() {
                        // Accumulated in reverse order.
                        let del_text: String = deleted.chars().rev().collect();
                        ed.registers
                            .delete(register, RegisterContent::new(del_text, false), true);
                    }
                    Ok(())
                })?;
            }
            'd' => {
                self.execute_delete(cmd)?;
            }
            'y' => {
                self.execute_yank(cmd)?;
            }
            'c' => {
                self.execute_change(cmd)?;
            }
            // #V17: `s` and `S` are `c` over an implied range. They used to be
            // handled in the pre-parser fast path, which meant they saved
            // nothing to a register, recorded no undo, and accepted neither a
            // count (`3s`) nor a register prefix (`"as`) -- even though both
            // keys were already in the command parser's table.
            's' | 'S' => {
                self.execute_substitute_command(cmd)?;
            }
            // Text input: a count repeats the input (121196, 121355, 121426,
            // 121510).  `A` is `$[count]a`, `I` is `^[count]i`.
            'i' => self.enter_insert_counted(InsertKind::Insert, count),
            'a' => self.enter_insert_counted(InsertKind::Append, count),
            'I' => self.enter_insert_counted(InsertKind::InsertBol, count),
            'A' => self.enter_insert_counted(InsertKind::AppendEol, count),
            'o' => self.enter_insert_counted(InsertKind::OpenBelow, count),
            'O' => self.enter_insert_counted(InsertKind::OpenAbove, count),
            'R' => self.enter_insert_counted(InsertKind::Replace, count),
            // `[buffer][count] C` is `[buffer][count] c$` (121252-121254).
            'C' => {
                let mut change = cmd.clone();
                change.command = 'c';
                change.count = 1;
                change.motion = Some(crate::command::MotionCommand::new('$').with_count(cmd.count));
                self.execute_change(&change)?;
            }
            // Put commands
            'p' => {
                self.execute_put(false, count, cmd.register)?;
            }
            'P' => {
                self.execute_put(true, count, cmd.register)?;
            }
            // Undo
            // POSIX: `u` reverses the last command that modified the buffer,
            // "including undo" (ex undo, 95442), so pressing it twice returns
            // to where you started.
            'u' => match self.undo.undo_toggle(&mut self.buffer) {
                Ok(pos) => {
                    self.buffer.set_cursor(pos);
                    self.set_message("1 change undone");
                }
                Err(e) => self.set_error(&e.to_string()),
            },
            'U' => {
                let line = self.buffer.cursor().line;
                match self.undo.restore_line(&mut self.buffer, line) {
                    Ok(_) => self.set_message("Line restored"),
                    Err(e) => self.set_error(&e.to_string()),
                }
            }
            // Join
            'J' => {
                // Recorded as a line-run replacement: `ChangeKind::Join` exists
                // but both `apply_change` and `apply_inverse` fall through its
                // arm, so a join recorded that way could never be undone.
                let first = self.buffer.cursor().line;
                let mut joined = 0usize;
                let before: Vec<String> = (0..=count)
                    .filter_map(|i| self.buffer.line(first + i))
                    .map(|l| l.content().to_string())
                    .collect();
                for _ in 0..count {
                    let line = self.buffer.cursor().line;
                    if line < self.buffer.line_count() {
                        let _ = self.buffer.join_lines(line, true);
                        joined += 1;
                    }
                }
                if joined > 0 {
                    let after: Vec<String> = self
                        .buffer
                        .line(first)
                        .map(|l| vec![l.content().to_string()])
                        .unwrap_or_default();
                    self.undo.note_dropped(self.buffer.take_dropped_marks());
                    self.undo.record_replace_lines(
                        Position::new(first, 0),
                        &before[..(joined + 1).min(before.len())],
                        &after,
                    );
                }
            }
            // Mark
            'm' => {
                // 95130-95134: a lowercase letter, `` ` `` or `'`.
                if let Some(slot) = cmd.char_arg.and_then(crate::buffer::mark_slot) {
                    self.buffer.set_mark(slot, self.buffer.cursor());
                }
            }
            // ZZ - write and quit
            'Z' => {
                if cmd.char_arg == Some('Z') {
                    self.write_and_quit()?;
                }
            }
            // Replace char
            'r' => {
                if let Some(c) = cmd.char_arg {
                    // `count` *successive* characters, each replaced by `c`.
                    // Calling the single-character replace repeatedly without
                    // advancing rewrote the same character every time, so
                    // `3rZ` changed one character rather than three.
                    let line = self.buffer.cursor().line;
                    let start = self.buffer.cursor().column;
                    let remaining = self
                        .buffer
                        .line(line)
                        .map(|l| l.content()[start..].chars().count())
                        .unwrap_or(0);
                    // POSIX: a count larger than the characters left on the
                    // line is an error, and the line is unchanged.
                    if count > remaining {
                        self.set_error("Not enough characters on the line");
                    } else if c == '\r' || c == '\n' {
                        self.replace_with_newlines(count)?;
                    } else {
                        self.undo.begin_group();
                        for i in 0..count {
                            self.replace_char(c);
                            if i + 1 < count {
                                let col = self.buffer.cursor().column;
                                let next =
                                    self.buffer.line(line).and_then(|l| l.next_char_offset(col));
                                match next {
                                    Some(n) => self.buffer.set_column(n),
                                    None => break,
                                }
                            }
                        }
                        self.undo.end_group();
                    }
                }
            }
            // Toggle case
            '~' => {
                // One command, one undo: without the group each toggled
                // character was its own change, so `2~` needed two `u`s.
                self.undo.begin_group();
                for _ in 0..count {
                    let before = self.buffer.cursor();
                    self.toggle_case();
                    if self.buffer.cursor() == before {
                        break;
                    }
                }
                self.undo.end_group();
            }
            // Delete to end of line
            'D' => {
                let line = self.buffer.cursor().line;
                let register = cmd.register;
                self.splice_around(line, line, |ed| {
                    let deleted = ed.buffer.delete_to_end_of_line();
                    if !deleted.is_empty() {
                        ed.registers
                            .delete(register, RegisterContent::new(deleted, false), true);
                    }
                    Ok(())
                })?;
            }
            // Shift left/right operators
            '>' | '<' => {
                self.execute_shift(cmd)?;
            }
            // Filter through shell (! operator)
            '!' => {
                self.execute_filter_operator(cmd)?;
            }
            // Yank line (alias for yy)
            'Y' => {
                // `Y` is `yy`.  It had its own implementation that wrote
                // straight to the unnamed register, so `"aY` and `"ayy`
                // disagreed; build the equivalent command instead.
                let mut yank_cmd = crate::command::ParsedCommand::new('y')
                    .with_count(count, cmd.has_count)
                    .with_motion(crate::command::MotionCommand::new('y').with_count(count));
                if let Some(r) = cmd.register {
                    yank_cmd = yank_cmd.with_register(r);
                }
                self.execute_yank(&yank_cmd)?;
            }
            // Repeat substitution
            '&' => {
                // A failure is a message on the status line: an error out of
                // here unwinds out of the editor and ends the session.
                if let Err(e) = self.repeat_substitution() {
                    self.set_error(&e.to_string());
                }
            }
            // Repeat last command
            '.' => {
                self.execute_dot_repeat(count)?;
            }
            // Execute register (macro)
            '@' => {
                if let Some(reg) = cmd.char_arg {
                    self.execute_macro(reg, count)?;
                }
            }
            _ => {}
        }

        // Save repeatable commands for .
        self.save_last_command(cmd);

        Ok(())
    }

    /// Determine if a command should be saved for dot repeat.
    fn is_repeatable_command(cmd: &crate::command::ParsedCommand) -> bool {
        matches!(
            cmd.command,
            'x' | 'X' | 'd' | 'c' | 'p' | 'P' | 'J' | 'r' | '~' | 'D' | '>' | '<' | 'Y' | '!'
        )
    }

    /// Save the last command for dot repeat.
    ///
    /// vi.md 121022-121024: "Commands (other than commands that enter text
    /// input mode) executed as a result of map expansions, shall not change the
    /// value of the last repeatable command." So `.` after a map that deleted a
    /// line repeats whatever the user last did by hand, not the map's `dd`.
    ///
    /// The exception is not needed here: a command that enters text input
    /// mode is recorded when its session ends, as `LastCommand::Input` (or
    /// `LastCommand::Insert` for a session no vi command began), which is a
    /// different path.
    fn save_last_command(&mut self, cmd: &crate::command::ParsedCommand) {
        if Self::is_repeatable_command(cmd) && !self.input.is_expanding() {
            self.last_command = Some(LastCommand::Parsed(cmd.clone()));
        }
    }

    /// Execute dot repeat command.
    fn execute_dot_repeat(&mut self, count: usize) -> Result<()> {
        if let Some(last) = self.last_command.clone() {
            match last {
                LastCommand::Insert {
                    kind,
                    text,
                    count: orig_count,
                } => {
                    // Repeat insert: the same session again, typed from the
                    // saved text, with a new count if one was given.
                    let repeat_count = if count > 1 { count } else { orig_count };
                    self.enter_insert_counted(kind, repeat_count);
                    self.type_input(&text)?;
                }
                LastCommand::Input { mut cmd, text } => {
                    // A count replaces the command's (and its motion's).
                    if count > 1 {
                        cmd.count = count;
                        cmd.has_count = true;
                        if let Some(ref mut mot) = cmd.motion {
                            mot.count = count;
                            mot.has_count = true;
                        }
                    }
                    // The command may fail before input begins; then it must
                    // not replace what `.` repeats.
                    let save = self.last_command.take();
                    self.execute_command(&cmd)?;
                    if self.insert_state.is_some() {
                        self.type_input(&text)?;
                    } else {
                        self.last_command = save;
                    }
                }
                LastCommand::Parsed(mut cmd) => {
                    // Override count if specified
                    if count > 1 {
                        cmd.count = count;
                        cmd.has_count = true;
                        if let Some(ref mut mot) = cmd.motion {
                            // The motion now carries the full count, so the
                            // repeat has to overwrite it rather than reset it
                            // to 1 and rely on cmd.count.
                            mot.count = count;
                            mot.has_count = true;
                        }
                    }
                    // Execute the saved command (but don't save it again)
                    let save = self.last_command.take();
                    self.execute_command(&cmd)?;
                    self.last_command = save;
                }
                LastCommand::Simple {
                    command,
                    count: orig_count,
                    char_arg,
                } => {
                    let repeat_count = if count > 1 { count } else { orig_count };
                    let cmd =
                        crate::command::ParsedCommand::new(command).with_count(repeat_count, true);
                    let cmd = if let Some(c) = char_arg {
                        cmd.with_char(c)
                    } else {
                        cmd
                    };
                    // Execute the simple command
                    let save = self.last_command.take();
                    self.execute_command(&cmd)?;
                    self.last_command = save;
                }
            }
        }
        Ok(())
    }

    /// Type `text` into the open insert session as it was first entered, and
    /// end it with ESC.  Every character goes in as text: a control
    /// character or the erase or kill character in it was entered after a
    /// `^V`, and is entered that way again.
    fn type_input(&mut self, text: &str) -> Result<()> {
        let erase = self.terminal.erase_char();
        let kill = self.terminal.kill_char();
        for c in text.chars() {
            if c == '\n' {
                self.handle_insert_key(Key::Enter)?;
                continue;
            }
            if (c.is_control() && c != '\t') || Some(c) == erase || Some(c) == kill {
                self.handle_insert_key(Key::Ctrl('v'))?;
            }
            self.handle_insert_key(Key::Char(c))?;
        }
        self.handle_insert_key(Key::Escape)
    }

    /// Execute a macro (register contents as commands).
    fn execute_macro(&mut self, reg: char, count: usize) -> Result<()> {
        // Handle @@ - repeat last macro
        let register = if reg == '@' {
            match self.last_macro_register {
                Some(r) => r,
                None => {
                    self.set_error("No previous macro");
                    return Ok(());
                }
            }
        } else {
            reg
        };

        // Get register contents
        let (content, linewise) = match self.registers.get(register) {
            Some(c) => (c.text.clone(), c.linewise),
            None => {
                self.set_error(&format!("Register {} is empty", register));
                return Ok(());
            }
        };

        // Save as last macro register
        self.last_macro_register = Some(register);

        // 121171-121173: "Behave as if the contents of the named buffer were
        // entered as standard input. After each line of a line-mode buffer, and
        // all but the last line of a character mode buffer, behave as if a
        // <newline> were entered as standard input."
        let mut text = content.clone();
        if linewise {
            if !text.ends_with('\n') {
                text.push('\n');
            }
        } else {
            // A character-mode buffer separates its lines but does not end with
            // one; `content` already carries the interior newlines, so the only
            // correction is to drop a trailing one if it is there.
            while text.ends_with('\n') {
                text.pop();
            }
        }

        // 121176-121177: "If a count is specified, behave as if that count were
        // entered as user input *before* the characters from the @ buffer were
        // entered." Running the buffer `count` times is a different thing --
        // `3@a` with `a` holding `dw` means `3dw`, one command over three
        // words, not three separate `dw`s.
        let mut keys: Vec<QueuedKey> = Vec::new();
        if count > 1 {
            for c in count.to_string().chars() {
                keys.push(QueuedKey {
                    key: Key::from_map_char(c),
                    remappable: true,
                    abbrevable: true,
                    source: KeySource::BufferExecution,
                });
            }
        }
        keys.extend(text.chars().map(|c| QueuedKey {
            key: Key::from_map_char(c),
            remappable: true,
            abbrevable: true,
            source: KeySource::BufferExecution,
        }));

        // Queued rather than executed: 121171's "as if ... entered as standard
        // input" is what makes buffer contents subject to maps, and what makes
        // 121174-121175 ("no more characters resulting from the execution of
        // this command shall be processed") the same rule as a map expansion's.
        self.input.push_front(keys.into_iter());
        Ok(())
    }

    /// Run `f`, recording whatever it did to the run of lines starting at
    /// `first` as a single undoable change.
    ///
    /// `apply_inverse` rebuilds by position and length without checking what
    /// is there, so a mutation that records nothing does not merely fail to
    /// undo: the next `u` pops an unrelated older change and destroys whatever
    /// now occupies its position (audit #V19).  Snapshotting the affected run
    /// before and after means the caller does not have to describe its edit at
    /// all, which is what makes this usable for the commands whose hand-built
    /// records were wrong or missing -- `:m`, `:t`, `:j`, `:pu`, the shifts,
    /// `p`/`P`, `r` and `~`.
    ///
    /// `last` is the last line the edit is expected to touch; pass a
    /// contiguous superset when the affected region is not contiguous, as
    /// `:m` needs.
    pub(super) fn splice_around<T>(
        &mut self,
        first: usize,
        last: usize,
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let first = first.max(1);
        let snapshot = |ed: &Self, upto: usize| -> Vec<String> {
            (first..=upto)
                .filter_map(|n| ed.buffer.line(n).map(|l| l.content().to_string()))
                .collect()
        };

        let before_total = self.buffer.line_count();
        let old = snapshot(self, last.min(before_total));

        let result = f(self)?;

        // The run may have grown or shrunk; follow the change in line count.
        let delta = self.buffer.line_count() as isize - before_total as isize;
        let new_last = (last as isize + delta).max(first as isize - 1) as usize;
        let new = snapshot(self, new_last.min(self.buffer.line_count()));

        if old != new {
            self.undo.note_dropped(self.buffer.take_dropped_marks());
            self.undo
                .record_replace_lines(Position::new(first, 0), &old, &new);
        }
        Ok(result)
    }

    /// Insert `lines` after line `after` (0: before line 1) as one undoable
    /// change.
    pub(super) fn insert_lines_after(&mut self, after: usize, lines: &[&str]) -> Result<()> {
        self.splice_around(after + 1, after, |ed| {
            for (i, text) in lines.iter().enumerate() {
                ed.buffer.insert_line_after(after + i, Line::from(*text));
            }
            Ok(())
        })
    }

    /// Record what an operator removed, so `u` can put it back.
    ///
    /// Every operator that removes text must call this: `apply_inverse` rebuilds
    /// by position and length without checking what is actually there, so an
    /// unrecorded removal does not merely fail to undo — the next `u` pops an
    /// unrelated older change and destroys whatever now sits at its position
    /// (audit #V19).
    fn record_removal(&mut self, range: Range, result: &crate::command::OperatorResult) {
        if let Some(text) = result.affected_text.as_deref() {
            if !text.is_empty() {
                let linewise = range.mode == BufferMode::Line;
                self.undo.note_dropped(self.buffer.take_dropped_marks());
                self.undo.record_delete(range.start, text, linewise);
            }
        }
    }

    /// Execute delete command.
    fn execute_delete(&mut self, cmd: &crate::command::ParsedCommand) -> Result<()> {
        use crate::command::delete;

        if let Some(mot) = &cmd.motion {
            if mot.motion == 'd' {
                // dd - delete lines
                let start_line = self.buffer.cursor().line;
                let end_line = (start_line + cmd.count - 1).min(self.buffer.line_count());
                let start = Position::new(start_line, 0);
                let end = Position::new(end_line, 0);
                let range = Range::lines(start, end);
                let result = delete(&mut self.buffer, range, &mut self.registers, cmd.register)?;
                self.record_removal(range, &result);
                self.buffer.set_cursor(result.cursor);
            } else {
                // d + motion
                let start = self.buffer.cursor();
                let res = self.execute_motion(mot);
                if let Some(range) = self.operator_region(start, mot, res.as_ref()) {
                    let result =
                        delete(&mut self.buffer, range, &mut self.registers, cmd.register)?;
                    self.record_removal(range, &result);
                    self.buffer.set_cursor(result.cursor);
                }
            }
        }
        Ok(())
    }

    /// Execute yank command.
    fn execute_yank(&mut self, cmd: &crate::command::ParsedCommand) -> Result<()> {
        use crate::command::yank;

        if let Some(mot) = &cmd.motion {
            if mot.motion == 'y' {
                // yy - yank lines
                let start_line = self.buffer.cursor().line;
                let end_line = (start_line + cmd.count - 1).min(self.buffer.line_count());
                let start = Position::new(start_line, 0);
                let end = Position::new(end_line, 0);
                let range = Range::lines(start, end);
                let _ = yank(&self.buffer, range, &mut self.registers, cmd.register);
                self.set_message(&format!("{} lines yanked", cmd.count));
            } else {
                // y + motion
                let start = self.buffer.cursor();
                let res = self.execute_motion(mot);
                if let Some(range) = self.operator_region(start, mot, res.as_ref()) {
                    let _ = yank(&self.buffer, range, &mut self.registers, cmd.register);
                }
            }
        }
        Ok(())
    }

    /// Execute change command.
    /// `s` (substitute `count` characters) and `S` (substitute `count` whole
    /// lines). Both are `c` over an implied range, so they route through the
    /// same operator and inherit its register, undo and cursor handling.
    fn execute_substitute_command(&mut self, cmd: &crate::command::ParsedCommand) -> Result<()> {
        use crate::command::change;

        let cursor = self.buffer.cursor();
        let range = if cmd.command == 'S' {
            let end_line = (cursor.line + cmd.count - 1).min(self.buffer.line_count());
            Range::lines(cursor, Position::new(end_line, 0))
        } else {
            // `s` takes a count of *characters*.  Walk that many character
            // boundaries forward rather than adding the count to a byte
            // offset and clamping it with a character count -- two different
            // units, neither of which was the byte offset a Range needs.
            let end_col = self
                .buffer
                .line(cursor.line)
                .map(|l| {
                    let mut col = cursor.column;
                    for _ in 0..cmd.count {
                        match l.next_char_offset(col) {
                            Some(next) => col = next,
                            None => break,
                        }
                    }
                    col
                })
                .unwrap_or(cursor.column);
            Range::chars(cursor, Position::new(cursor.line, end_col))
        };

        // Snapshot rather than `record_removal`, for the reason given in the
        // `cc` branch: `S` empties its lines instead of removing them.
        let register = cmd.register;
        let kind = if cmd.command == 'S' {
            InsertKind::Change
        } else {
            InsertKind::Insert
        };
        self.grouped(|ed| {
            let result = ed.splice_around(range.start.line, range.end.line, |ed| {
                change(&mut ed.buffer, range, &mut ed.registers, register)
            })?;
            ed.buffer.set_cursor(result.cursor);
            if result.enter_insert {
                ed.enter_insert(kind);
            }
            Ok(())
        })
    }

    fn execute_change(&mut self, cmd: &crate::command::ParsedCommand) -> Result<()> {
        use crate::command::{change, motion};

        if let Some(mot) = &cmd.motion {
            if mot.motion == 'c' {
                // cc - change lines
                let start_line = self.buffer.cursor().line;
                let end_line = (start_line + cmd.count - 1).min(self.buffer.line_count());
                let start = Position::new(start_line, 0);
                let end = Position::new(end_line, 0);
                let range = Range::lines(start, end);
                // The operator and the insert session it opens are one command,
                // so a single `u` reverses both.
                // A snapshot, not `record_removal`: `change_lines` *empties*
                // the first line rather than removing it, so recording a
                // linewise delete made `u` re-insert the original lines and
                // leave the emptied one behind as a stray blank.
                let register = cmd.register;
                self.grouped(|ed| {
                    let result = ed.splice_around(start_line, end_line, |ed| {
                        change(&mut ed.buffer, range, &mut ed.registers, register)
                    })?;
                    ed.buffer.set_cursor(result.cursor);
                    if result.enter_insert {
                        ed.enter_insert(InsertKind::Change);
                    }
                    Ok(())
                })?;
            } else {
                // c + motion
                let start = self.buffer.cursor();
                // POSIX (vi, "Change"): "if the motion command is `w` or `W`
                // and the cursor is on a non-<blank>", `cw` behaves as `ce`,
                // so the blanks after the word survive.  The non-blank
                // precondition was missing, so `cw` on a blank wrongly
                // consumed to the end of the *next* word.
                let on_non_blank = self
                    .buffer
                    .char_at_cursor()
                    .is_some_and(|c| !c.is_whitespace());
                let res = if on_non_blank && mot.motion == 'w' {
                    motion::move_word_end(&self.buffer, mot.count).ok()
                } else if on_non_blank && mot.motion == 'W' {
                    motion::move_bigword_end(&self.buffer, mot.count).ok()
                } else {
                    self.execute_motion(mot)
                };
                if let Some(range) = self.operator_region(start, mot, res.as_ref()) {
                    // Snapshot, not `record_removal`: the region can now be
                    // line-wise (`cj`, `c'a`, `cH`), and `change_lines` empties
                    // the first line instead of removing it -- so a linewise
                    // delete record makes `u` restore the lines *and* leave the
                    // emptied one behind.  Same reasoning as the `cc` arm.
                    let register = cmd.register;
                    self.grouped(|ed| {
                        let result =
                            ed.splice_around(range.start_line(), range.end_line(), |ed| {
                                change(&mut ed.buffer, range, &mut ed.registers, register)
                            })?;
                        // The change's cursor may be clamped; text goes in
                        // where the deleted text began, which may be past
                        // the end of the line.
                        ed.buffer.set_line(range.start.line);
                        ed.buffer.set_column_for_insert(range.start.column);
                        if result.enter_insert {
                            ed.enter_insert(InsertKind::Change);
                        }
                        Ok(())
                    })?;
                } else if mot.motion == '$'
                    && self
                        .buffer
                        .line(self.buffer.cursor().line)
                        .is_none_or(|l| l.is_empty())
                {
                    // `c$` (so `C`) on an empty line has no region, and
                    // still changes it: text input begins where it stands.
                    self.enter_insert(InsertKind::Change);
                }
            }
        }
        Ok(())
    }

    /// Execute shift command (> or <).
    fn execute_shift(&mut self, cmd: &crate::command::ParsedCommand) -> Result<()> {
        // `shift_right`/`shift_left` report no affected text, so there is
        // nothing for a hand-built undo record to describe -- snapshot the
        // rewritten run instead.
        //
        // The span has to come from the *motion*, not from `count` lines below
        // the cursor: `>j`, `>G` and `>k` all shift lines the cursor-relative
        // span does not cover, and an undo record over the wrong lines leaves
        // the buffer in a state that is neither before nor after the command.
        let cursor = self.buffer.cursor().line;
        let (first, last) = match &cmd.motion {
            // `>>`/`<<`: `count` lines from the cursor.
            Some(mot) if mot.motion == cmd.command => (
                cursor,
                (cursor + cmd.count.max(1) - 1).min(self.buffer.line_count()),
            ),
            Some(mot) => {
                let end = self
                    .execute_motion(mot)
                    .map(|r| r.position.line)
                    .unwrap_or(cursor);
                (cursor.min(end), cursor.max(end))
            }
            None => (cursor, cursor),
        };
        let cmd = cmd.clone();
        self.splice_around(first, last.max(first), |ed| ed.execute_shift_inner(&cmd))
    }

    fn execute_shift_inner(&mut self, cmd: &crate::command::ParsedCommand) -> Result<()> {
        use crate::command::{shift_left, shift_right};

        if let Some(mot) = &cmd.motion {
            let is_right = cmd.command == '>';

            if mot.motion == cmd.command {
                // >> or << - shift lines
                let start_line = self.buffer.cursor().line;
                let end_line = (start_line + cmd.count - 1).min(self.buffer.line_count());
                let start = Position::new(start_line, 0);
                let end = Position::new(end_line, 0);
                let range = Range::lines(start, end);
                if is_right {
                    let result = shift_right(
                        &mut self.buffer,
                        range,
                        self.options.shiftwidth,
                        self.options.tabstop,
                        &mut self.registers,
                    )?;
                    self.buffer.set_cursor(result.cursor);
                } else {
                    let result = shift_left(
                        &mut self.buffer,
                        range,
                        self.options.shiftwidth,
                        self.options.tabstop,
                        &mut self.registers,
                    )?;
                    self.buffer.set_cursor(result.cursor);
                }
            } else {
                // > + motion or < + motion
                let start = self.buffer.cursor();
                if let Some(end) = self.execute_motion_get_pos(mot) {
                    let start_line = start.line.min(end.line);
                    let end_line = start.line.max(end.line);
                    let range =
                        Range::lines(Position::new(start_line, 0), Position::new(end_line, 0));
                    if is_right {
                        let result = shift_right(
                            &mut self.buffer,
                            range,
                            self.options.shiftwidth,
                            self.options.tabstop,
                            &mut self.registers,
                        )?;
                        self.buffer.set_cursor(result.cursor);
                    } else {
                        let result = shift_left(
                            &mut self.buffer,
                            range,
                            self.options.shiftwidth,
                            self.options.tabstop,
                            &mut self.registers,
                        )?;
                        self.buffer.set_cursor(result.cursor);
                    }
                }
            }
        }
        Ok(())
    }

    /// Execute a motion and return the resulting position.
    fn execute_motion_get_pos(&self, mot: &crate::command::MotionCommand) -> Option<Position> {
        self.execute_motion(mot).map(|r| r.position)
    }

    /// Line number for a screen-relative motion (`H`, `M`, `L`).
    ///
    /// Extracted so the operator form dispatches the same way as the bare
    /// form; `dH` and `dL` used to fall through to "no motion" and silently
    /// do nothing.
    fn screen_relative_line(&self, which: char, count: usize) -> usize {
        let top = self.screen.top_line();
        let rows = self.terminal.size().rows as usize;
        let target = match which {
            'H' => top.saturating_add(count.saturating_sub(1)),
            'M' => top + rows / 2,
            // -2 for the status line, `count` lines up from the bottom.
            _ => (top + rows.saturating_sub(2))
                .saturating_sub(count.saturating_sub(1))
                .max(top),
        };
        target.min(self.buffer.line_count()).max(1)
    }

    /// Run a motion, keeping its classification.
    ///
    /// The dispatcher used to discard everything but the position, so no
    /// operator could tell a linewise motion from a characterwise one or an
    /// inclusive one from an exclusive one.
    fn execute_motion(
        &self,
        mot: &crate::command::MotionCommand,
    ) -> Option<crate::command::motion::MotionResult> {
        self.motion(mot.motion, mot.count, mot.has_count, mot.char_arg)
            .ok()
    }

    /// The one table of vi motions: where motion `m` (with `count`, and
    /// `char_arg` for `f`, a mark, ...) takes the cursor, and how an
    /// operator treats the region.  Both the commands and the operators'
    /// motions come here, so a motion behaves the same either way.  An
    /// `Err` is the motion's error: it cannot move as asked.
    fn motion(
        &self,
        m: char,
        count: usize,
        has_count: bool,
        char_arg: Option<char>,
    ) -> Result<crate::command::motion::MotionResult> {
        use crate::command::motion::{self, Boundary, MotionResult};

        let buf = &self.buffer;
        let cur = buf.cursor();
        let last = buf.line_count().max(1);
        // Line-wise, to the first non-blank.
        let to_line = |line: usize| {
            MotionResult::pos(Position::new(line, 0))
                .linewise()
                .with_first_non_blank()
        };
        let failed = |what: &str| ViError::MotionFailed(what.to_string());
        let arg = || char_arg.ok_or_else(|| failed("missing character"));
        let paragraph = Boundary::Paragraph {
            sections: &self.options.sections,
            paragraphs: &self.options.paragraphs,
        };
        let section = Boundary::Section {
            sections: &self.options.sections,
        };
        let find = |kind: char, c: char| match kind {
            'f' => motion::find_char_forward(buf, c, count),
            'F' => motion::find_char_backward(buf, c, count),
            't' => motion::till_char_forward(buf, c, count),
            _ => motion::till_char_backward(buf, c, count),
        };
        Ok(match m {
            'h' => motion::move_left(buf, count)?,
            'l' | ' ' => motion::move_right(buf, count)?,
            'j' => motion::move_down(buf, count)?,
            'k' => motion::move_up(buf, count)?,
            'w' => motion::move_word_forward(buf, count)?,
            'W' => motion::move_bigword_forward(buf, count)?,
            'b' => motion::move_word_backward(buf, count)?,
            'B' => motion::move_bigword_backward(buf, count)?,
            'e' => motion::move_word_end(buf, count)?,
            'E' => motion::move_bigword_end(buf, count)?,
            '0' => motion::move_to_line_start(buf)?,
            '^' => motion::move_to_first_non_blank(buf)?,
            '$' => motion::move_to_line_end(buf, count)?,
            // `G` alone is the last line; with a count, that line.
            'G' => motion::move_to_line(buf, Some(if has_count { count } else { last }))?,
            'f' | 'F' | 't' | 'T' => find(m, arg()?)?,
            // Repeat the last f/F/t/T, the same way (`;`) or reversed (`,`).
            ';' | ',' => {
                let last_find = self.last_find.ok_or_else(|| failed("no previous find"))?;
                let kind = if m == ';' {
                    last_find.command
                } else {
                    last_find.reverse()
                };
                find(kind, last_find.target)?
            }
            '{' => motion::move_to_boundary(buf, count, false, paragraph)?,
            '}' => motion::move_to_boundary(buf, count, true, paragraph)?,
            '[' | ']' if char_arg != Some(m) => return Err(failed("not [[ or ]]")),
            '[' => motion::move_to_boundary(buf, count, false, section)?,
            ']' => motion::move_to_boundary(buf, count, true, section)?,
            '(' => motion::move_sentence_backward(buf, count)?,
            ')' => motion::move_sentence_forward(buf, count)?,
            '|' => motion::move_to_column(buf, count, self.options.tabstop)?,
            '%' => motion::find_matching_bracket(buf)?,
            // Screen-relative: line-wise, to the first non-blank.
            'H' | 'M' | 'L' => to_line(self.screen_relative_line(m, count)),
            // `count` lines down (`+`, <CR>) or up (`-`); too few is an
            // error.
            '+' | '\r' => match cur.line.checked_add(count) {
                Some(line) if line <= last => to_line(line),
                _ => return Err(failed("not that many lines below")),
            },
            '-' => match cur.line.checked_sub(count) {
                Some(line) if line >= 1 => to_line(line),
                _ => return Err(failed("not that many lines above")),
            },
            // The current line and `count - 1` below it.
            '_' => match cur.line.checked_add(count - 1) {
                Some(line) if line <= last => to_line(line),
                _ => return Err(failed("not that many lines below")),
            },
            // A mark: `'` its line, line-wise; `` ` `` the character,
            // exclusive.
            '\'' | '`' => {
                let name = arg()?;
                let pos = self.mark_position(name).ok_or(ViError::MarkNotSet(name))?;
                if m == '\'' {
                    to_line(pos.line)
                } else {
                    MotionResult::pos(pos)
                }
            }
            // The last search again, `count` times, the same way (`n`) or
            // the other (`N`).
            'n' | 'N' => {
                let step = |pos| {
                    if m == 'n' {
                        self.search.search(buf, pos)
                    } else {
                        self.search.search_opposite(buf, pos)
                    }
                };
                // With wrapscan the matches cycle: once back at the first
                // match, only the remainder of the count is left to go.
                let mut pos = step(cur)?;
                let first = pos;
                let mut left = count - 1;
                let mut cycle = 0usize;
                while left > 0 {
                    pos = step(pos)?;
                    left -= 1;
                    cycle += 1;
                    if pos == first {
                        left %= cycle;
                    }
                }
                MotionResult::pos(pos)
            }
            // An operator's search motion with a line offset: whole lines
            // to the line it names (see `search_as_motion`).
            '\0' => {
                let (same_way, offset) = self
                    .search_motion
                    .ok_or_else(|| failed("no search to repeat"))?;
                let found = if same_way {
                    self.search.search(buf, cur)?
                } else {
                    self.search.search_opposite(buf, cur)?
                };
                let line = i64::try_from(found.line)
                    .unwrap_or(i64::MAX)
                    .saturating_add(offset);
                match usize::try_from(line) {
                    Ok(line) if (1..=last).contains(&line) => {
                        MotionResult::pos(Position::new(line, 0)).linewise()
                    }
                    _ => return Err(failed("offset past the buffer")),
                }
            }
            _ => return Err(ViError::InvalidCommand(format!("not a motion: {m}"))),
        })
    }

    /// Move the cursor by the motion command `cmd`, as a command rather
    /// than an operator's motion; a motion that cannot move is an error.
    fn move_cursor(&mut self, cmd: &crate::command::ParsedCommand) {
        self.search.update_options(&self.options);
        match self.motion(cmd.command, cmd.count, cmd.has_count, cmd.char_arg) {
            Ok(res) => {
                self.buffer.set_cursor(res.position);
                if res.first_non_blank {
                    self.buffer.move_to_first_non_blank();
                }
                if let (Some(c), 'f' | 'F' | 't' | 'T') = (cmd.char_arg, cmd.command) {
                    self.last_find = Some(FindCommand::new(cmd.command, c));
                }
                if matches!(cmd.command, 'n' | 'N') {
                    self.screen.scroll_to_line(res.position.line, &self.buffer);
                }
            }
            Err(e) => {
                let _ = self.terminal.bell();
                self.set_error(&e.to_string());
            }
        }
    }

    /// The line a `w`/`W` motion would have reached one step short of its
    /// count -- i.e. the line holding the last word it moves over.
    ///
    /// The end-of-line rule keys off that line, not off the start line: with a
    /// count the motion may cross several.
    fn word_motion_line_before_last_step(
        &self,
        mot: &crate::command::MotionCommand,
        start: Position,
    ) -> usize {
        use crate::command::motion;
        if mot.count <= 1 {
            return start.line;
        }
        let one_short = mot.count - 1;
        let res = if mot.motion == 'W' {
            motion::move_bigword_forward(&self.buffer, one_short)
        } else {
            motion::move_word_forward(&self.buffer, one_short)
        };
        res.map(|r| r.position.line).unwrap_or(start.line)
    }

    /// The region an operator acts on, given where it started and how the
    /// motion is classified.
    ///
    /// This is the single place that turns a motion into a range.  Every
    /// operator used to build `BufferMode::Character` unconditionally, which
    /// is why `dj` deleted part of two lines instead of both of them.
    fn operator_region(
        &self,
        start: Position,
        mot: &crate::command::MotionCommand,
        res: Option<&crate::command::motion::MotionResult>,
    ) -> Option<Range> {
        use crate::command::motion::MotionClass;

        // POSIX (vi, "Delete"): "If the motion command is `w` or `W`, and the
        // last word on the line is being deleted, the region shall end at the
        // last character of the line."  So `dw` never joins lines -- and on
        // the last word of the last line it still deletes that word, even
        // though `w` itself reports failure there because it cannot advance.
        //
        // The rule is about the *last word moved over*, not about the motion
        // crossing a line at some point: with a count it may legitimately span
        // lines, so `d3w` on "one two\nthree four" leaves "four".  Compare the
        // landing line against where `count - 1` words would have ended: only
        // if the final step crossed a line did the last word end one.
        let word_runs_off_the_line = match res {
            None => true,
            Some(r) if r.class != MotionClass::Exclusive => false,
            Some(r) => {
                let prev = self.word_motion_line_before_last_step(mot, start);
                r.position.line != prev || r.position.column <= start.column
            }
        };
        if matches!(mot.motion, 'w' | 'W') && word_runs_off_the_line {
            let clamp_line = self.word_motion_line_before_last_step(mot, start);
            let end_col = self.buffer.line(clamp_line).map(|l| l.len()).unwrap_or(0);
            let end = Position::new(clamp_line, end_col);
            if end <= start {
                return None;
            }
            return Some(Range::new(start, end, BufferMode::Character));
        }

        let res = res?;
        Some(match res.class {
            MotionClass::Linewise => Range::lines(start, res.position),
            MotionClass::Inclusive => {
                // Character ranges are end-exclusive, so "inclusive" means
                // extending past whichever endpoint is the later one.  Going
                // forwards that is the landing character; going backwards it
                // is the character under the cursor, which a backward `d%`
                // would otherwise leave behind.
                let extend_past = |p: Position| {
                    let col = self
                        .buffer
                        .line(p.line)
                        .and_then(|l| l.next_char_offset(p.column))
                        .unwrap_or(p.column);
                    Position::new(p.line, col)
                };
                if res.position >= start {
                    Range::new(start, extend_past(res.position), BufferMode::Character)
                } else {
                    Range::new(res.position, extend_past(start), BufferMode::Character)
                }
            }
            MotionClass::Exclusive => Range::new(start, res.position, BufferMode::Character),
        })
    }

    /// Execute filter operator (! with motion).
    fn execute_filter_operator(&mut self, cmd: &crate::command::ParsedCommand) -> Result<()> {
        if let Some(mot) = &cmd.motion {
            let count = cmd.count;
            let (start_line, end_line) = if mot.motion == '!' {
                // !! - filter current line(s)
                let start = self.buffer.cursor().line;
                let end = (start + count - 1).min(self.buffer.line_count());
                (start, end)
            } else {
                // ! + motion - determine lines from motion
                let start_line = self.buffer.cursor().line;
                if let Some(end_pos) = self.execute_motion_get_pos(mot) {
                    let end_line = end_pos.line;
                    if start_line <= end_line {
                        (start_line, end_line)
                    } else {
                        (end_line, start_line)
                    }
                } else {
                    self.set_error("Invalid motion");
                    return Ok(());
                }
            };

            // Store the range for later use and enter ex mode with ! prefix
            self.pending_filter = Some(format!("{},{}", start_line, end_line));
            self.mode = Mode::Ex;
            self.ex_prompt = ':';
            self.ex_input = "!".to_string();
        }
        Ok(())
    }

    /// Execute put command.
    fn execute_put(&mut self, before: bool, count: usize, register: Option<char>) -> Result<()> {
        // Like the shifts, `put_after`/`put_before` report no affected text.
        let line = self.buffer.cursor().line;
        self.splice_around(line, line, |ed| {
            ed.execute_put_inner(before, count, register)
        })
    }

    fn execute_put_inner(
        &mut self,
        before: bool,
        count: usize,
        register: Option<char>,
    ) -> Result<()> {
        use crate::command::{put_after, put_before};

        // `put_after`/`put_before` already default to the unnamed register, so
        // hardcoding `None` here was the only thing stopping `"ap` from
        // reaching the register the user named.
        let result = if before {
            put_before(&mut self.buffer, &self.registers, register, count)?
        } else {
            put_after(&mut self.buffer, &self.registers, register, count)?
        };

        self.buffer.set_cursor(result.cursor);
        Ok(())
    }

    /// Run a `/`- or `?`-style search from an ex command line: set the pattern
    /// (up to the first unescaped delimiter), search, then apply any trailing
    /// `+n`/`-n` line offset.
    /// Record `pattern` as a search in direction `delim` (`/` or `?`): the
    /// pattern `n` repeats, the `/` register, and the last RE used, which
    /// spans searches as well as substitutes (what `~` and `s//` reuse).
    fn note_search(&mut self, pattern: &str, delim: char) -> Result<()> {
        let dir = if delim == '/' {
            SearchDirection::Forward
        } else {
            SearchDirection::Backward
        };
        self.search.update_options(&self.options);
        self.search.set_pattern(pattern, dir)?;
        self.registers.set_search(pattern);
        self.set_last_regex(pattern);
        Ok(())
    }

    fn ex_search(&mut self, body: &str, delim: char, dir: SearchDirection) -> Result<()> {
        self.search.update_options(&self.options);
        let (pattern, offset) = split_search(body, delim);
        if !pattern.is_empty() {
            self.note_search(&pattern, delim)?;
        }
        self.search_next(dir)?;
        if offset != 0 {
            let target = (self.buffer.cursor().line as i64 + offset)
                .clamp(1, self.buffer.line_count().max(1) as i64) as usize;
            self.buffer.set_line(target);
            self.buffer.move_to_first_non_blank();
        }
        Ok(())
    }

    /// Execute an ex command input string.
    fn execute_ex_input(&mut self, input: &str) -> Result<()> {
        // Check if this is a filter command from vi ! operator
        let actual_input = if let Some(range) = self.pending_filter.take() {
            if let Some(shell_cmd) = input.strip_prefix('!') {
                // Construct full filter command: {range}!{command}
                format!("{}!{}", range, shell_cmd)
            } else {
                input.to_string()
            }
        } else {
            input.to_string()
        };

        // One line, any number of commands (POSIX 94545-94646): `|`
        // separates them, by rules that depend on the command.  An error
        // stops the rest of the line.
        let mut after_bar = false;
        let generation = self.buffer_generation;
        for seg in crate::ex::cmdline::split_line(&actual_input) {
            if seg.comment {
                break;
            }
            // 94300-94304: an interrupt aborts the command running -- a
            // `:g`, `:so` or `:@` included, whose commands come through here.
            if self.ex_standalone_mode && crate::signals::take(self.interrupt) {
                return Err(ViError::Interrupted);
            }
            // Inside a global, nothing more runs once the buffer has been
            // replaced (95006-95008), not even the rest of this line.
            if self.in_global && self.buffer_generation != generation {
                return Err(Self::buffer_replaced());
            }
            // 94557: an empty command after a `|` runs nothing.
            if after_bar && seg.command.trim().is_empty() {
                break;
            }
            after_bar = seg.ended_by_bar;
            self.execute_one_ex_command(&seg.command, seg.text.as_deref(), seg.ended_by_bar)?;
            if self.should_quit {
                break;
            }
        }
        Ok(())
    }

    /// Run one ex command; `text` is the input text an `a`, `c` or `i`
    /// took from the rest of its line.
    ///
    /// `by_bar` says a `|` ended it: in open or visual mode a line of
    /// addresses alone moves the cursor only when a newline ends it (94555),
    /// and otherwise prints.
    fn execute_one_ex_command(
        &mut self,
        command: &str,
        text: Option<&str>,
        by_bar: bool,
    ) -> Result<()> {
        // A leading `/re/` or `?re?` is an address, not a search: what
        // follows it is the command to run there (`/b/d`).  vi's own `/` and
        // `?` prompts are searches, and are handled where the prompt ends.
        let (cmd, flags) = crate::ex::parser::parse_ex_line(command)?;
        // Marks this command drops go to the undo record it makes; any left
        // over from before are not its.
        self.buffer.take_dropped_marks();

        // A search address is a search: its RE becomes the last RE used,
        // and the search pattern for `n`, before the command runs -- so the
        // `s//X/` of `/b/s//X/` means `b`.  Only once the line has parsed:
        // a command that is never run must not change them.
        let (range, _) = crate::ex::address::parse_address_range(command);
        let mut searches = range.search_patterns();
        if let ExCommand::Copy { dest, .. } | ExCommand::Move { dest, .. } = &cmd {
            searches.extend(dest.search_pattern());
        }
        for (pattern, delim) in searches {
            self.note_search(&pattern, delim)?;
        }

        // Which of print, number and list this is: they write their lines
        // with the flags folded in, where any other command writes the
        // current line after it has run.
        let print_kind = match &cmd {
            ExCommand::Print { .. } => Some((false, false)),
            ExCommand::Number { .. } => Some((true, false)),
            ExCommand::List { .. } => Some((false, true)),
            _ => None,
        };

        // POSIX 95142-95158: the previous-context mark is set to where we
        // are before a command that addresses `$`, a line number, a search
        // or a mark -- unless that is just a move in visual mode (6.a) --
        // and before z, undo, global and v; but not if the command fails.
        let visual_move =
            matches!(cmd, ExCommand::GotoAddress { .. }) && !self.ex_standalone_mode && !by_bar;
        let dest_jumps = match &cmd {
            ExCommand::Copy { dest, .. } | ExCommand::Move { dest, .. } => dest.is_jump(),
            _ => false,
        };
        // A `:k '` sets the mark itself, after the jump would have.
        let sets_context = matches!(
            cmd,
            ExCommand::Mark {
                name: '\'' | '`',
                ..
            }
        );
        let jumps = ((range.is_jump() || dest_jumps) && !visual_move
            || matches!(
                cmd,
                ExCommand::Z { .. } | ExCommand::Undo | ExCommand::Global { .. }
            ))
            && !sets_context;
        // Set once the command has run (`''` must still read the old one),
        // to where the cursor was, followed through the command's own edits.
        let before = self.buffer.cursor();
        if jumps {
            self.buffer.hold(before);
        }
        let generation = self.buffer_generation;

        self.pending_flags = flags;
        self.ex_by_bar = by_bar;
        let result = self.execute_ex(cmd);
        self.pending_flags = ExFlags::default();
        let before_now = if jumps { self.buffer.release() } else { None };
        let result = result?;
        if self.buffer_generation == generation {
            if let Some(pos) = before_now {
                self.set_context_mark(pos);
            }
        }
        self.apply_ex_result(result)?;

        // 94640-94642: the rest of the line is the text of an `a`, `c` or
        // `i`, as if typed, and ends it.
        if let Some(text) = text {
            if self.ex_insert_mode.is_some() {
                self.accept_ex_insert_line(text)?;
                self.finalize_ex_insert()?;
            }
        }

        // 94561-94564: print, number and list set the flags a later implied
        // print uses -- to nothing, `#` and `l`, plus their own flags; any
        // other command's #, p and l flags are remembered too.
        let print_cmd = print_kind.is_some();
        if let Some((number, list)) = print_kind {
            self.remembered_print = (number || flags.number, list || flags.list);
        } else if flags.any() {
            self.remembered_print = (flags.number, flags.list);
        }
        // Print, number and list wrote their lines with the flags folded in;
        // a `+`/`-` still moves on from there and writes where it lands.
        if (!print_cmd && flags.any()) || flags.offset != 0 {
            // The flags: move the current line by the `+`/`-` count, then
            // write it.
            let cur = self.buffer.cursor().line as i64;
            let target = cur
                .checked_add(flags.offset)
                .filter(|&n| n >= 1 && n <= self.buffer.line_count() as i64)
                .ok_or_else(|| ViError::InvalidAddress("address out of range".to_string()))?
                as usize;
            self.buffer.set_line(target);
            self.buffer.move_to_first_non_blank();
            let lines = self.format_lines(
                target,
                target,
                flags.number || self.options.number,
                flags.list || self.options.list,
            );
            self.apply_ex_result(ExResult::CommandOutput(lines))?;
        }
        Ok(())
    }

    /// Act on what an ex command returned.
    fn apply_ex_result(&mut self, result: ExResult) -> Result<()> {
        match result {
            ExResult::Continue => {}
            ExResult::Quit(code) => {
                self.should_quit = true;
                self.exit_code = code;
            }
            ExResult::Edit(path) => {
                self.open(&path)?;
            }
            ExResult::StatusMessage(msg) => {
                self.set_message(&msg);
            }
            ExResult::Error(msg) => {
                return Err(ViError::InvalidCommand(msg));
            }
            ExResult::CommandOutput(lines) => {
                if self.ex_standalone_mode {
                    // In ex mode, print output directly to stdout
                    // Note: Output from :p, :nu, :l is NOT suppressed by -s
                    // (POSIX says -s suppresses "informational messages", not command output)
                    // As the bytes the text stands for: an escaped byte
                    // goes out as itself.
                    for line in &lines {
                        crate::bytes::println(line);
                    }
                } else {
                    // In visual mode, show as message
                    self.set_message(&lines.join("\n"));
                }
            }
            ExResult::Insert(line, col) => {
                self.buffer.set_cursor(Position::new(line, col));
                self.enter_insert(InsertKind::Insert);
            }
            ExResult::Pending(_) => {}
            ExResult::EnterVisual => {
                // Signal to switch to visual mode
                self.ex_standalone_mode = false;
            }
            ExResult::EnterOpen(line) => {
                // Enter open mode at specified line
                if let Some(l) = line {
                    self.buffer.set_line(l);
                }
                self.mode = Mode::Open;
                self.ex_standalone_mode = false;
            }
        }
        Ok(())
    }

    /// Execute a parsed ex command.
    fn execute_ex(&mut self, cmd: ExCommand) -> Result<ExResult> {
        match cmd {
            ExCommand::Quit { force } => {
                // POSIX: quit warns while more files remain in the argument
                // list, unless forced. The modified-buffer check is in quit().
                if !force && self.files.has_more_files() {
                    return Err(ViError::MoreFiles);
                }
                self.quit(force)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Write {
                range,
                file,
                append,
                force,
            } => {
                let lines = self.write_line_range(&range)?;
                self.write(file.as_deref(), lines, append, force)?;
                Ok(ExResult::Continue)
            }
            ExCommand::WriteQuit {
                range,
                file,
                force,
                xit,
            } => {
                // #X29: `xit` on a buffer unmodified since the last complete
                // write is just `quit` (ex.md §95537); only `wq` always writes.
                if xit && !self.buffer.is_modified() {
                    self.quit(force)?;
                } else {
                    let lines = self.write_line_range(&range)?;
                    self.write(file.as_deref(), lines, false, force)?;
                    self.quit(true)?;
                }
                Ok(ExResult::Continue)
            }
            ExCommand::Edit {
                file,
                force,
                command,
            } => {
                self.edit_file(file.as_deref(), force, command.as_deref())?;
                Ok(ExResult::Continue)
            }
            ExCommand::Set { args } => {
                if let Some(msg) = self.options.set(&args)? {
                    // Route query/display output through Output so it prints in ex mode
                    // (Message is for informational status, suppressed in -s mode)
                    Ok(ExResult::CommandOutput(
                        msg.lines().map(String::from).collect(),
                    ))
                } else {
                    Ok(ExResult::Continue)
                }
            }
            ExCommand::Substitute {
                range,
                pattern,
                replacement,
                flags,
            } => {
                self.substitute(&range, &pattern, &replacement, &flags)?;
                // The p/l/# flags echo each changed line.
                let printed = std::mem::take(&mut self.substitute_output);
                if printed.is_empty() {
                    Ok(ExResult::Continue)
                } else {
                    Ok(ExResult::CommandOutput(printed))
                }
            }
            ExCommand::GotoAddress { range } => {
                if !self.ex_standalone_mode && !self.ex_by_bar {
                    // Open or visual mode: the current line is set to the
                    // last address (94555).  A line number past either end
                    // goes to that end, as vi always has: `:0` and `:999`
                    // are the usual ways to reach the first and last lines.
                    let last = match (&range.end, &range.start) {
                        (None, Some(crate::ex::address::Address::Line(n))) => {
                            (*n).clamp(1, self.buffer.line_count().max(1))
                        }
                        _ => self.resolve_range(&range)?.1,
                    };
                    self.buffer.set_line(last.max(1));
                    self.buffer.move_to_first_non_blank();
                    return Ok(ExResult::Continue);
                }
                // POSIX 94561: the implied command is print, of every line
                // addressed.  Resolved once: a search address searches.
                let (first, last) = self.resolve_range(&range)?;
                let lines = self.lines_for_output(first, last);
                self.buffer.set_line(last.max(1));
                self.buffer.move_to_first_non_blank();
                Ok(ExResult::CommandOutput(lines))
            }
            ExCommand::Empty => {
                // POSIX 94561-94569: in ex mode an empty command prints --
                // the current line inside a global, the next line outside
                // one, failing (and leaving the current line alone) past the
                // end of the buffer.
                if !self.ex_standalone_mode {
                    return Ok(ExResult::Continue);
                }
                if self.in_global {
                    let cur = self.buffer.cursor().line;
                    return Ok(ExResult::CommandOutput(self.lines_for_output(cur, cur)));
                }
                let next = self.buffer.cursor().line + 1;
                if next > self.buffer.line_count() {
                    return Err(ViError::InvalidAddress(
                        "no line after the last".to_string(),
                    ));
                }
                let lines = self.lines_for_output(next, next);
                self.buffer.set_line(next);
                self.buffer.move_to_first_non_blank();
                Ok(ExResult::CommandOutput(lines))
            }
            ExCommand::File { new_name } => {
                if let Some(name) = new_name {
                    self.files.set_current_file(Some(crate::bytes::path(&name)));
                    // Write rule 5: a later `:w` with no operand must not
                    // clobber this newly-named file if it already exists.
                    self.files.mark_pathname_changed();
                }
                let info = self.file_info();
                Ok(ExResult::StatusMessage(info))
            }
            ExCommand::Undo => {
                // POSIX 95442: `u` undoes the last change *including an
                // undo*, so a second `u` redoes -- as vi's `u` does -- and the
                // current line is where the change was (95450).
                let pos = self.undo.undo_toggle(&mut self.buffer)?;
                self.buffer
                    .set_line(pos.line.clamp(1, self.buffer.line_count().max(1)));
                self.buffer.move_to_first_non_blank();
                Ok(ExResult::Continue)
            }
            ExCommand::Redo => {
                self.undo.redo(&mut self.buffer)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Next {
                force,
                files,
                command,
            } => {
                // "an error, unless the file is successfully written as
                // specified by the autowrite option" (95178-95180).
                if !force && self.buffer.is_modified() {
                    if self.options.autowrite {
                        let lines = self.write_line_range(&crate::ex::AddressRange::empty())?;
                        self.write(None, lines, false, false)?;
                    } else {
                        return Err(ViError::FileModified);
                    }
                }
                let path = if files.is_empty() {
                    self.files.next_file()?
                } else {
                    // "Set the argument list to the specified filenames ... set
                    // the current pathname to the first filename specified."
                    self.files.set_args(to_paths(&files));
                    crate::bytes::path(&files[0])
                };
                self.open(&crate::bytes::from_path(&path))?;
                self.run_plus_command(command.as_deref())?;
                Ok(ExResult::Continue)
            }
            ExCommand::Previous { force } => {
                if !force && self.buffer.is_modified() {
                    return Err(ViError::FileModified);
                }
                let path = self.files.prev_file()?;
                self.open(&crate::bytes::from_path(&path))?;
                Ok(ExResult::Continue)
            }
            ExCommand::Rewind { force } => {
                if !force && self.buffer.is_modified() {
                    return Err(ViError::FileModified);
                }
                let path = self.files.rewind()?;
                self.open(&crate::bytes::from_path(&path))?;
                Ok(ExResult::Continue)
            }
            ExCommand::Args => Ok(ExResult::StatusMessage(self.files.format_args())),
            ExCommand::Preserve => {
                // :preserve always saves the buffer (independent of modified).
                let orig = self.files.current_file().map(|p| p.to_path_buf());
                match crate::recover::preserve(
                    orig.as_deref(),
                    &self.buffer.as_text(),
                    &self.options.directory,
                ) {
                    Ok(_) => {
                        crate::recover::notify(orig.as_deref());
                        Ok(ExResult::StatusMessage("File preserved".to_string()))
                    }
                    Err(e) => Ok(ExResult::Error(format!("preserve failed: {}", e))),
                }
            }
            ExCommand::Recover { file, force } => {
                // 95293-95294: without `!`, a modified buffer is an error.
                if !force && self.buffer.is_modified() {
                    return Err(ViError::FileModified);
                }
                // 95295-95299: the named file, or the current one; edited as
                // by `:e`, from its saved contents if there are any.
                let path = match file {
                    Some(f) => f.clone(),
                    None => self
                        .files
                        .current_file()
                        .map(crate::bytes::from_path)
                        .ok_or(ViError::NoFileName)?,
                };
                self.recover_pending
                    .push(crate::recover::path_key(&crate::bytes::path(&path)));
                self.open(&path)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Version => Ok(ExResult::StatusMessage(format!(
                "{} {}",
                env!("CARGO_PKG_NAME"),
                env!("CARGO_PKG_VERSION")
            ))),
            ExCommand::Help => Ok(ExResult::StatusMessage(format!(
                "{}: {}",
                env!("CARGO_PKG_NAME"),
                env!("CARGO_PKG_DESCRIPTION")
            ))),
            ExCommand::Shell { command } => {
                self.execute_shell_command(&command)?;
                Ok(ExResult::Continue)
            }
            ExCommand::ShellRead { range, command } => {
                let line = self.resolve_target_line(&range, true)?;
                self.execute_shell_read(line, &command)?;
                Ok(ExResult::Continue)
            }
            ExCommand::ShellWrite { range, command } => {
                self.execute_shell_write(&range, &command)?;
                Ok(ExResult::Continue)
            }
            ExCommand::ShellFilter { range, command } => {
                self.execute_shell_filter(&range, &command)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Source { file } => {
                self.nested(|ed| ed.execute_source(&file))?;
                Ok(ExResult::Continue)
            }
            ExCommand::Append {
                range,
                toggle_autoindent,
            } => {
                let line = self
                    .resolve_target_line(&range, true)?
                    .unwrap_or_else(|| self.buffer.cursor().line);
                self.begin_ex_insert(ExInsertMode::Append(line), toggle_autoindent);
                Ok(ExResult::Continue)
            }
            ExCommand::Insert {
                range,
                toggle_autoindent,
            } => {
                let line = self
                    .resolve_target_line(&range, true)?
                    .unwrap_or_else(|| self.buffer.cursor().line);
                self.begin_ex_insert(ExInsertMode::Insert(line), toggle_autoindent);
                Ok(ExResult::Continue)
            }
            ExCommand::Change {
                range,
                count,
                toggle_autoindent,
            } => {
                let current = self.buffer.cursor().line;
                let resolved = range.resolve(&self.addr_ctx_at(current))?;
                // "count ... shall be equivalent to specifying an additional
                // address ... equal to the last address specified plus count-1"
                // (94785-94789).
                let end = match count {
                    Some(n) if n >= 1 => (resolved.1 + n - 1).min(self.buffer.line_count()),
                    _ => resolved.1,
                };
                self.begin_ex_insert(
                    ExInsertMode::Change {
                        start: resolved.0,
                        end,
                    },
                    toggle_autoindent,
                );
                Ok(ExResult::Continue)
            }
            ExCommand::Delete {
                range,
                register,
                count,
            } => {
                self.execute_ex_delete(&range, register, count)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Yank {
                range,
                register,
                count,
            } => {
                self.execute_ex_yank(&range, register, count)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Print { range, count } => self.print_command(&range, count, false, false),
            ExCommand::Number { range, count } => self.print_command(&range, count, true, false),
            ExCommand::List { range, count } => self.print_command(&range, count, false, true),
            ExCommand::Join {
                range,
                count,
                force,
            } => {
                self.execute_ex_join(&range, count, force)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Put { range, register } => {
                let line = self.resolve_target_line(&range, true)?;
                self.execute_ex_put(line, register)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Copy { range, dest } => {
                self.execute_ex_copy(&range, &dest)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Move { range, dest } => {
                self.execute_ex_move(&range, &dest)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Read { range, file } => {
                self.execute_ex_read(&range, file.as_deref())?;
                Ok(ExResult::Continue)
            }
            ExCommand::Global {
                range,
                pattern,
                command,
                invert,
            } => {
                self.execute_ex_global(&range, &pattern, &command, invert)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Cd { path, force } => {
                // 94925-94926: without `!`, changing directory with a modified
                // buffer and a relative current pathname is an error, since the
                // buffer could no longer be written back to where it came from.
                let relative_pathname = self.files.current_file().is_some_and(|p| !p.is_absolute());
                if !force && self.buffer.is_modified() && relative_pathname {
                    return Err(ViError::FileModified);
                }
                self.execute_ex_cd(path.as_deref())?;
                Ok(ExResult::Continue)
            }
            ExCommand::Pwd => {
                let cwd = std::env::current_dir()
                    .map(|p| crate::bytes::from_path(&p))
                    .unwrap_or_else(|_| "unknown".to_string());
                Ok(ExResult::StatusMessage(cwd))
            }
            ExCommand::Mark { range, name } => {
                let line = self.resolve_target_line(&range, false)?;
                self.execute_ex_mark(line, name)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Visual { range, force, args } => self.execute_ex_visual(range, force, &args),
            ExCommand::Open { range, pattern } => {
                let line = self.resolve_target_line(&range, false)?;
                // "If pattern is empty ... or not specified, the last regular
                // expression used in the editor shall be used" (95212-95214).
                if let Some(pattern) = pattern {
                    self.search
                        .set_pattern(&pattern, crate::search::SearchDirection::Forward)?;
                }
                Ok(ExResult::EnterOpen(line))
            }
            ExCommand::Z {
                range,
                ztype,
                type_count,
                count,
                full_screen,
            } => {
                let line = self.resolve_target_line(&range, false)?;
                let output = self.execute_ex_z(line, ztype, type_count, count, full_screen)?;
                Ok(ExResult::CommandOutput(output))
            }
            ExCommand::ShiftLeft {
                range,
                count,
                width,
            } => {
                self.execute_ex_shift_left(&range, count, width)?;
                Ok(ExResult::Continue)
            }
            ExCommand::ShiftRight {
                range,
                count,
                width,
            } => {
                self.execute_ex_shift_right(&range, count, width)?;
                Ok(ExResult::Continue)
            }
            ExCommand::LineNumber { range } => {
                let line = self.resolve_target_line(&range, false)?;
                let line_num = self.execute_ex_line_number(line)?;
                Ok(ExResult::CommandOutput(vec![line_num.to_string()]))
            }
            ExCommand::Execute { range, buffer } => {
                self.nested(|ed| ed.execute_ex_execute(&range, buffer))?;
                Ok(ExResult::Continue)
            }
            ExCommand::Suspend { force } => {
                self.execute_ex_suspend(force)?;
                Ok(ExResult::Continue)
            }
            ExCommand::RepeatSubstitute { range, flags } => {
                self.execute_ex_repeat_substitute(&range, &flags)?;
                Ok(ExResult::Continue)
            }
            ExCommand::TildeSubstitute { range, flags } => {
                // `~` pairs the previous *replacement* with the last RE, which
                // may have come from a search rather than a substitute -- that
                // is what distinguishes it from `&` (#X18).
                let pattern = self
                    .last_regex()
                    .ok_or(ViError::NoPreviousSearch)?
                    .to_string();
                let replacement = self
                    .last_substitution
                    .as_ref()
                    .map(|l| l.replacement.clone())
                    .ok_or(ViError::NoPreviousSubstitution)?;
                self.substitute(&range, &pattern, &replacement, &flags)?;
                let printed = std::mem::take(&mut self.substitute_output);
                if printed.is_empty() {
                    Ok(ExResult::Continue)
                } else {
                    Ok(ExResult::CommandOutput(printed))
                }
            }
            ExCommand::Tag { tag, force } => {
                self.goto_tag(&tag, force)?;
                Ok(ExResult::Continue)
            }
            // Parsed but not implemented. These returned `Continue` through the
            // wildcard below, so the editor accepted `:map x dd` in silence and
            // the user had no way to learn the mapping was never made.
            ExCommand::Map { lhs, rhs, mode } => {
                self.maps
                    .table_mut(mode)
                    .set(keys_from_text(&lhs), keys_from_text(&rhs));
                Ok(ExResult::Continue)
            }
            // 95080-95083: with no arguments, write the list for this mode and
            // do nothing more.
            ExCommand::MapList { mode } => {
                Ok(ExResult::CommandOutput(self.maps.table(mode).list()))
            }
            ExCommand::Unmap { lhs, mode } => {
                if self.maps.table_mut(mode).remove(&keys_from_text(&lhs)) {
                    Ok(ExResult::Continue)
                } else {
                    // 95457-95462: removing something that is not in that
                    // mode's list "shall be an error".
                    Err(ViError::NoSuchMap(lhs, mode == MapMode::Insert))
                }
            }
            ExCommand::Abbreviate { lhs, rhs } => {
                self.maps.abbrev.set(lhs, keys_from_text(&rhs));
                Ok(ExResult::Continue)
            }
            // 94864: with no arguments, write the list and do nothing more.
            ExCommand::AbbrevList => Ok(ExResult::CommandOutput(self.maps.abbrev.list())),
            ExCommand::Unabbreviate { lhs } => {
                if self.maps.abbrev.remove(&lhs) {
                    Ok(ExResult::Continue)
                } else {
                    // 95436-95437: "If lhs is not an entry in the current list
                    // of abbreviations ... it shall be an error."
                    Err(ViError::NoSuchAbbreviation(lhs))
                }
            }
            ExCommand::Pop => {
                self.pop_tag()?;
                Ok(ExResult::Continue)
            }
            // Listing an empty stack is not a failure, so this never errors --
            // which keeps `:tags` usable from a script running under `-s`.
            ExCommand::Tags => Ok(ExResult::CommandOutput(crate::tags::format_stack(
                &self.tag_stack,
            ))),
            // No wildcard, deliberately: every variant is named, so a new
            // `ExCommand` is a compile error rather than another command that
            // silently does nothing. A `_ =>` arm here is what let `:map`,
            // `:unmap`, `:ab`, `:una`, `:pop` and `:tags` all return success
            // while doing nothing, for as long as they did.
        }
    }

    /// `e[dit][!][+command][file]` (94946-94957).
    fn edit_file(&mut self, file: Option<&str>, force: bool, command: Option<&str>) -> Result<()> {
        if !force && self.buffer.is_modified() {
            return Err(ViError::FileModified);
        }
        if let Some(path) = file {
            self.open(path)?;
        } else if let Some(path) = self.files.current_file().map(|p| p.to_path_buf()) {
            self.open(&crate::bytes::from_path(&path))?;
        }
        self.run_plus_command(command)
    }

    /// Run a `+command` argument, "immediately after the contents of the edit
    /// buffer have been replaced and the current line and column have been set"
    /// (94956-94957).
    fn run_plus_command(&mut self, command: Option<&str>) -> Result<()> {
        let Some(command) = command else {
            return Ok(());
        };
        // A `+command` is specified, so the current line starts at the last
        // line of the buffer (94961-94963) before the command runs.
        let last = self.buffer.line_count();
        if last > 0 {
            self.buffer.set_line(last);
            self.buffer.move_to_first_non_blank();
        }
        self.execute_ex_input(command)
    }

    /// `vi[sual]`, whose synopsis depends on the current mode.
    ///
    /// "If ex is currently in open or visual mode, the Synopsis and behavior of
    /// the visual command shall be the same as the edit command" (95473-95474);
    /// otherwise it is `[1addr] vi[sual][type][count][flags]` (95472) and enters
    /// visual mode with the addressed line placed as `type` directs.
    fn execute_ex_visual(
        &mut self,
        range: crate::ex::AddressRange,
        force: bool,
        args: &str,
    ) -> Result<ExResult> {
        if !self.ex_standalone_mode {
            let (command, file) = crate::ex::parser::split_plus_command(args.trim());
            let file = (!file.is_empty()).then_some(file);
            self.edit_file(file, force, command.as_deref())?;
            return Ok(ExResult::Continue);
        }

        let line = self.resolve_target_line(&range, false)?;
        let args = args.trim();
        let vtype = args
            .chars()
            .next()
            .filter(|c| matches!(c, '+' | '-' | '.' | '^'));
        let rest = match vtype {
            Some(c) => &args[c.len_utf8()..],
            None => args,
        };
        // "If count is specified, the value of the window edit option shall be
        // set to count ... before being used by the type character"
        // (95478-95480).
        if let Ok(count) = rest.trim().parse::<usize>() {
            if count >= 1 {
                self.options.window = count;
            }
        }

        let target = line.unwrap_or_else(|| self.buffer.cursor().line).max(1);
        self.buffer.set_line(target);
        self.buffer.move_to_first_non_blank();

        // "If type is not specified, it shall be as if a type of '+' was
        // specified" (95481-95489).
        let window = self.options.window.max(1);
        let top = match vtype.unwrap_or('+') {
            // Beginning of the line at the top of the display.
            '+' => target,
            // End of the line at the bottom of the display.
            '-' => target.saturating_sub(window - 1).max(1),
            // Beginning of the line in the middle of the display.
            '.' => target.saturating_sub(window / 2).max(1),
            // "If the specified line is less than or equal to the value of the
            // window edit option, set the line to 1; otherwise, decrement the
            // line by the value of the window edit option minus 1."
            '^' => {
                if target <= window {
                    1
                } else {
                    (target - (window - 1)).max(1)
                }
            }
            _ => target,
        };
        self.screen.set_top_line(top);
        Ok(ExResult::EnterVisual)
    }

    /// Run a `:so` or `:@`, which run commands that may run more of them:
    /// past `MAX_NESTING` deep it is an error, not a stack overflow.
    fn nested(&mut self, f: impl FnOnce(&mut Self) -> Result<()>) -> Result<()> {
        if self.nesting >= MAX_NESTING {
            return Err(ViError::InvalidCommand(format!(
                "commands nested more than {MAX_NESTING} deep"
            )));
        }
        self.nesting += 1;
        let result = f(self);
        self.nesting -= 1;
        result
    }

    /// Execute :source command - read and execute ex commands from file.
    fn execute_source(&mut self, file: &str) -> Result<()> {
        use std::fs::File;
        use std::io::{BufRead, BufReader};

        let file = File::open(crate::bytes::path(file)).map_err(ViError::Io)?;
        let mut reader = BufReader::new(file);

        // Read to the `\n` and strip the terminator ourselves. `BufRead::lines`
        // removes a trailing `\r` as well, which would eat the quoted carriage
        // return in a sourced `map Q :wq^V^M`.  Bytes, escaped as editor text
        // where they are not UTF-8, as for every ex command.
        let mut raw = Vec::new();
        loop {
            raw.clear();
            if reader.read_until(b'\n', &mut raw).map_err(ViError::Io)? == 0 {
                break;
            }
            self.execute_source_line(&crate::bytes::decode(&raw))?;
        }

        Ok(())
    }

    /// Execute ex commands from a string (already-read file content).
    fn execute_source_content(&mut self, content: &str) -> Result<()> {
        // Split on `\n` alone rather than using `str::lines`, which also strips
        // a trailing `\r` -- and so would eat the quoted carriage return in a
        // `.exrc` line like `map Q :wq^V^M` before anything could see it.
        for line in content.split('\n') {
            self.execute_source_line(line)?;
        }
        Ok(())
    }

    /// Execute a single line from a source file.
    fn execute_source_line(&mut self, line: &str) -> Result<()> {
        // Leading <blank>s go unconditionally; the trailing ones belong to
        // `parse_ex_command`, which strips them by the rule the command
        // follows and so can keep a quoted one.
        let line = strip_line_terminator(line).trim_start();
        // Skip empty lines and comments
        if line.is_empty() || line.starts_with('"') {
            return Ok(());
        }
        self.execute_ex_input(line)?;
        Ok(())
    }

    /// Load startup configuration from EXINIT or .exrc.
    ///
    /// Per POSIX, the initialization sequence is:
    /// 1. If EXINIT is set, execute its commands; otherwise source $HOME/.exrc
    /// 2. If the `exrc` option is set, source ./.exrc (unless same as $HOME/.exrc)
    ///
    /// Security requirements for .exrc:
    /// - Must be owned by the real user ID
    /// - Must NOT be writable by group or others
    pub fn load_startup_config(&mut self) -> Result<()> {
        // 1. Check EXINIT environment variable
        // Per POSIX: if EXINIT is set (even to empty), $HOME/.exrc is not sourced
        if let Some(exinit_os) = std::env::var_os("EXINIT") {
            // As editor text, like every other source of ex commands.
            let exinit = crate::bytes::from_os(&exinit_os);
            if !exinit.is_empty() {
                // One command line: `|` separates its commands by the usual
                // rules, so a `map` or `g` in EXINIT keeps its `|`s --
                // splitting at every `|` broke both.  An error ends it
                // (94684-94687).
                let _ = self.execute_ex_input(&exinit);
            }
        } else {
            // EXINIT not set — check $HOME/.exrc
            if let Ok(home) = std::env::var("HOME") {
                if !home.is_empty() {
                    let exrc_path = format!("{}/.exrc", home);
                    match crate::config::read_safe_exrc(&exrc_path) {
                        Ok(Some(content)) => {
                            let _ = self.execute_source_content(&content);
                        }
                        // Absent or deliberately skipped for safety: silent.
                        Ok(None) => {}
                        // Exists but unreadable -- say so rather than start up
                        // as though the user had no configuration (#X21).
                        Err(e) => {
                            eprintln!("{}: {}", exrc_path, e);
                        }
                    }
                }
            }
        }

        // 2. If exrc option is set, source ./.exrc in current directory
        if self.options.exrc {
            let local_exrc = ".exrc";
            if !crate::config::is_same_file_as_home_exrc(local_exrc) {
                match crate::config::read_safe_exrc(local_exrc) {
                    Ok(Some(content)) => {
                        let _ = self.execute_source_content(&content);
                    }
                    Ok(None) => {}
                    Err(e) => {
                        eprintln!("{}: {}", local_exrc, e);
                    }
                }
            }
        }

        Ok(())
    }

    /// Execute a shell command (:! or :shell).
    ///
    /// "In addition, a warning message shall be written if the edit buffer has
    /// been modified since the last complete write, and the warn edit option is
    /// set." (95607-95608) The `warn` option existed but was read nowhere, so
    /// `:!cmd` on a modified buffer said nothing.
    fn execute_shell_command(&mut self, command: &str) -> Result<()> {
        let warn = self.options.warn && self.buffer.is_modified();

        // Temporarily restore terminal to cooked mode.  Remember whether it
        // was raw to begin with: ex never is, and re-enabling unconditionally
        // left it raw afterwards, where it stopped delivering whole lines.
        let was_raw = self.terminal.is_raw_mode();
        self.terminal.disable_raw_mode()?;

        if warn {
            println!();
            println!("[No write since last change]");
        }

        let result = if command.is_empty() {
            // :shell - start interactive shell
            println!(); // Blank line before shell
            self.shell.interactive()
        } else {
            // :!command - execute command
            println!(); // Blank line before output
            self.shell.execute(command)
        };

        // Wait for the user to read the output before the screen is redrawn.
        //
        // Only in visual mode on a real terminal -- `was_raw` is true exactly
        // then.  In ex there is no redraw to wait for and no user to prompt,
        // and the read *consumes a byte of the command stream*: it swallowed
        // the `q` of a following `q!`, leaving a bare `!`, which starts an
        // interactive shell and hangs the session.
        if was_raw && result.is_ok() {
            use std::io::{self, Read, Write};
            print!("\nPress ENTER or type command to continue");
            io::stdout().flush()?;
            let mut buf = [0u8; 1];
            let _ = io::stdin().read(&mut buf);
        }

        // Restore raw mode only if it was raw before.
        if was_raw {
            self.terminal.enable_raw_mode()?;
            // Force full screen redraw. Only meaningful in visual mode; in ex
            // it would write escape sequences into the command output.
            self.terminal.clear_screen()?;
        }

        match result {
            Ok(output) => {
                if !output.success {
                    self.set_error(&format!("shell returned {}", output.exit_code));
                }
                Ok(())
            }
            Err(e) => {
                self.set_error(&e.to_string());
                Err(e)
            }
        }
    }

    /// Execute :r !command - read command output into buffer.
    fn execute_shell_read(&mut self, line: Option<usize>, command: &str) -> Result<()> {
        // #V18: the child inherits our stdin now (#X23), so it must not find
        // the terminal in raw mode. `execute_shell_command` already brackets
        // its child this way; this path did not.
        // `disable_raw_mode` reports success whether or not it had anything
        // to do, so `.is_ok()` was always true -- in ex, which is never raw,
        // that switched the terminal *into* raw mode and it stopped
        // delivering whole lines.
        let was_raw = self.terminal.is_raw_mode();
        self.terminal.disable_raw_mode()?;
        let output = self.shell.execute_capture(command);
        if was_raw {
            let _ = self.terminal.enable_raw_mode();
        }
        let output = output?;

        if !output.success {
            self.set_error(&format!("shell returned {}", output.exit_code));
            return Ok(());
        }

        // Insert output after specified line (or current line)
        let insert_line = line.unwrap_or_else(|| self.buffer.cursor().line);
        let text = output.stdout_string();
        let lines: Vec<&str> = split_lines(&text).collect();
        let line_count = lines.len();
        self.insert_lines_after(insert_line, &lines)?;

        // Move cursor to first inserted line
        if line_count > 0 {
            self.buffer.set_line(insert_line + 1);
            self.buffer.move_to_first_non_blank();
        }

        self.set_message(&format!("{} lines", line_count));
        Ok(())
    }

    /// Execute :[range]!command - filter lines through command.
    fn execute_shell_filter(&mut self, range: &AddressRange, command: &str) -> Result<()> {
        // Resolve range
        let (start, end) = self.resolve_range(range)?;

        // Collect lines to filter
        let mut input = String::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                input.push_str(line.content());
                input.push('\n');
            }
        }

        // Run filter
        let output = self.shell.filter(command, &input)?;

        if !output.success {
            self.set_error(&format!("shell returned {}", output.exit_code));
            return Ok(());
        }

        // Replace the range with the output as one whole-line change, so
        // `u` swaps the lines back rather than rebuilding them character by
        // character.
        let text = output.stdout_string();
        self.splice_around(start, end, |ed| {
            for _ in start..=end {
                ed.buffer.delete_line(start);
            }
            // `insert_line_after(0, ..)` inserts before the first line.
            let insert_pos = start.saturating_sub(1);
            for (i, line) in split_lines(&text).enumerate() {
                ed.buffer
                    .insert_line_after(insert_pos + i, Line::from(line));
            }
            Ok(())
        })?;

        // Position cursor at first line of result
        self.buffer.set_line(start.max(1));
        self.buffer.move_to_first_non_blank();

        let filtered = end - start + 1;
        self.set_message(&format!("{} lines filtered", filtered));
        Ok(())
    }

    /// Execute :w !command - write buffer/range to command stdin.
    fn execute_shell_write(&mut self, range: &AddressRange, command: &str) -> Result<()> {
        // Resolve range
        let (start, end) = self.resolve_range(range)?;

        // Collect lines to write
        let mut text = String::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                text.push_str(line.content());
                text.push('\n');
            }
        }

        // Temporarily restore terminal to cooked mode, remembering whether it
        // was raw: ex never is, and re-enabling unconditionally left it raw.
        let was_raw = self.terminal.is_raw_mode();
        self.terminal.disable_raw_mode()?;
        println!(); // Blank line before output

        let result = self.shell.write_to(command, &text);

        // Wait for the user to read the output before the screen is redrawn --
        // visual mode only.  In ex there is no redraw and no user, and the
        // read consumes a byte of the *command stream*: see
        // `execute_shell_command`.
        if was_raw && result.is_ok() {
            use std::io::{self, Read, Write};
            print!("\nPress ENTER or type command to continue");
            io::stdout().flush()?;
            let mut buf = [0u8; 1];
            let _ = io::stdin().read(&mut buf);
        }

        if was_raw {
            self.terminal.enable_raw_mode()?;
            self.terminal.clear_screen()?;
        }

        match result {
            Ok(output) => {
                if !output.success {
                    self.set_error(&format!("shell returned {}", output.exit_code));
                }
                Ok(())
            }
            Err(e) => {
                self.set_error(&e.to_string());
                Err(e)
            }
        }
    }

    /// The lines a `[2addr] cmd [count]` command acts on.  POSIX: with a
    /// count, `count` lines starting at the last line addressed -- `1p 2` is
    /// lines 1 and 2 (the count used to be ignored whenever an address was
    /// given).
    fn counted_lines(&self, range: &AddressRange, count: Option<usize>) -> Result<(usize, usize)> {
        let current = self.buffer.cursor().line;
        let (start, end) = if range.explicit {
            range.resolve(&self.addr_ctx_at(current))?
        } else {
            (current, current)
        };
        Ok(match count {
            Some(0) => {
                return Err(ViError::InvalidCommand(
                    "count must be positive".to_string(),
                ))
            }
            Some(c) => (end, end.saturating_add(c - 1).min(self.buffer.line_count())),
            None => (start, end),
        })
    }

    /// Run `print`, `number` or `list` (`number`/`list` say which): write
    /// the lines as the command, its flags and the `number` and `list`
    /// options make them, and leave the current line on the last.
    fn print_command(
        &mut self,
        range: &AddressRange,
        count: Option<usize>,
        number: bool,
        list: bool,
    ) -> Result<ExResult> {
        let (start, end) = self.counted_lines(range, count)?;
        let flags = self.pending_flags;
        let lines = self.format_lines(
            start,
            end,
            number || flags.number || self.options.number,
            list || flags.list || self.options.list,
        );
        self.buffer.set_line(end.max(1));
        Ok(ExResult::CommandOutput(lines))
    }

    /// Lines `start..=end` as `print` writes them, with the `number` and
    /// `list` edit options -- or, with no command of its own, the remembered
    /// print flags (POSIX 94561-94564).
    fn lines_for_output(&self, start: usize, end: usize) -> Vec<String> {
        let (number, list) = self.remembered_print;
        self.format_lines(
            start,
            end,
            number || self.options.number,
            list || self.options.list,
        )
    }

    /// Lines `start..=end`: numbered as `"%6d  "` (POSIX 95236) and listed
    /// unambiguously as the flags ask.
    fn format_lines(&self, start: usize, end: usize, number: bool, list: bool) -> Vec<String> {
        let mut lines = Vec::new();
        // A numbered row starts with the 8 columns of `"%6d  "`.
        let width = self
            .list_width()
            .saturating_sub(if number { 8 } else { 0 })
            .max(1);
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                let rows = if list {
                    Self::list_rows(line.content(), width)
                } else {
                    vec![line.content().to_string()]
                };
                for (i, row) in rows.into_iter().enumerate() {
                    lines.push(if number && i == 0 {
                        format!("{:6}  {}", line_num, row)
                    } else {
                        row
                    });
                }
            }
        }
        lines
    }

    /// Execute :global command - execute command on matching lines.
    fn execute_ex_global(
        &mut self,
        range: &AddressRange,
        pattern: &str,
        command: &str,
        invert: bool,
    ) -> Result<()> {
        let current = self.buffer.cursor().line;
        let (start, end) = if range.explicit {
            range.resolve(&self.addr_ctx_at(current))?
        } else {
            (1, self.buffer.line_count())
        };

        // Compiled as every RE the editor matches is: the edit options
        // apply, and an empty pattern is the last RE (94995) -- compiled raw,
        // `g//d` matched every line and deleted the buffer.  The pattern then
        // becomes the last RE, so `g/re/s//X/` means `re`.
        let pattern = if pattern.is_empty() {
            self.last_regex()
                .ok_or(ViError::NoPreviousSearch)?
                .to_string()
        } else {
            pattern.to_string()
        };
        // The search pattern and the last RE, with the search direction left
        // as it was: a global is not a search forward.  Compiled once, there.
        self.search.update_options(&self.options);
        self.search.set_pattern(&pattern, self.search.direction())?;
        self.registers.set_search(&pattern);
        self.set_last_regex(&pattern);
        let regex = self
            .search
            .compiled()
            .cloned()
            .ok_or(ViError::NoPreviousSearch)?;

        // Collect matching line numbers first (to avoid mutation during iteration)
        let mut matching_lines = Vec::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                let matches = regex.is_match(line.content());
                if matches != invert {
                    matching_lines.push(line_num);
                }
            }
        }

        // Execute the command on each matching line.
        //
        // POSIX marks the matching lines first and then runs the command list
        // against each, in order.  Every command that inserts, removes or
        // relocates lines renumbers the marks not yet visited, so follow them
        // through the buffer's edit journal rather than by line number.  This
        // used to reverse the list when the command text began with 'd',
        // which missed `.d`, `.,.+1d`, `j` and everything else that changes
        // the line count -- the loop then addressed unrelated lines and
        // deleted them.  Reversing is not an option in general anyway: it
        // would reverse the output of a `p` and the resulting order of an `m`.
        //
        // A mark is `None` once the command that ran on an earlier match
        // deleted its text.
        let mut pending: Vec<Option<usize>> = matching_lines.into_iter().map(Some).collect();

        // POSIX (ex, `undo`, 95443) makes a global a single command, so one
        // `u` must reverse all of it -- not just the last line it touched.
        self.undo.begin_group();
        self.buffer.begin_line_tracking();
        let was_in_global = std::mem::replace(&mut self.in_global, true);
        let generation = self.buffer_generation;
        let outcome = (|| -> Result<()> {
            for idx in 0..pending.len() {
                let Some(line_num) = pending[idx] else {
                    continue;
                };
                if line_num == 0 || line_num > self.buffer.line_count() {
                    continue;
                }

                self.buffer.set_line(line_num);
                let mark = self.buffer.edit_log_len();
                // POSIX 95005-95008: an error, or the buffer being replaced
                // (by `:e`, say), ends the global and is reported.  Going on
                // ran the rest of the list against whatever the buffer had
                // become.  (`execute_ex_input` checks the generation between
                // the commands of one list, too.)
                self.execute_ex_input(command)?;
                if self.buffer_generation != generation {
                    return Err(Self::buffer_replaced());
                }

                let edits: Vec<_> = self.buffer.edits_since(mark).to_vec();
                if !edits.is_empty() {
                    for slot in &mut pending[idx + 1..] {
                        for edit in &edits {
                            *slot = slot.and_then(|line| edit.remap(line));
                        }
                    }
                }
            }
            Ok(())
        })();
        // One way out, whatever happened.
        self.in_global = was_in_global;
        self.buffer.end_line_tracking();
        self.undo.end_group();
        outcome
    }

    /// The error a global reports when its command list replaced the buffer.
    fn buffer_replaced() -> ViError {
        ViError::InvalidCommand("global: the edit buffer was replaced".to_string())
    }

    /// Replace the edit buffer, and count it: a global running when the
    /// buffer is replaced must stop (95006-95008), and checks this count.
    fn replace_buffer(&mut self, buffer: Buffer) {
        self.buffer = buffer;
        self.buffer_generation += 1;
    }

    /// Execute :cd command - change directory.
    fn execute_ex_cd(&mut self, path: Option<&str>) -> Result<()> {
        let target = if let Some(p) = path {
            crate::bytes::path(p)
        } else {
            // Try to get home directory from environment
            std::env::var_os("HOME")
                .map(std::path::PathBuf::from)
                .ok_or_else(|| ViError::ShellError("Cannot determine home directory".to_string()))?
        };

        std::env::set_current_dir(&target).map_err(|e| {
            ViError::ShellError(format!("Cannot change to {}: {}", target.display(), e))
        })?;

        self.set_message(&format!("{}", target.display()));
        Ok(())
    }

    /// Execute :suspend or :stop command - suspend the editor.
    /// `su[spend][!]`.
    ///
    /// "These commands shall be affected by the autowrite and writeany edit
    /// options" (95405): with `autowrite` set and a modified buffer, write it
    /// out before suspending unless `!` was given.
    fn execute_ex_suspend(&mut self, force: bool) -> Result<()> {
        if !force && self.options.autowrite && self.buffer.is_modified() {
            let lines = self.write_line_range(&crate::ex::AddressRange::empty())?;
            self.write(None, lines, false, false)?;
        }

        // Only restore terminal if we're in visual mode (raw mode active)
        if !self.ex_standalone_mode {
            self.terminal.disable_raw_mode()?;
        }

        // Send SIGTSTP to self
        #[cfg(unix)]
        unsafe {
            libc::raise(libc::SIGTSTP);
        }

        // Re-enable terminal handling if we were in visual mode
        if !self.ex_standalone_mode {
            self.terminal.enable_raw_mode()?;
            self.terminal.clear_screen()?;
        }

        Ok(())
    }

    /// Search for next match.
    fn search_next(&mut self, direction: SearchDirection) -> Result<()> {
        self.search.update_options(&self.options);

        let pos = match direction {
            SearchDirection::Forward => self
                .search
                .search_forward(&self.buffer, self.buffer.cursor())?,
            SearchDirection::Backward => self
                .search
                .search_backward(&self.buffer, self.buffer.cursor())?,
        };

        self.buffer.set_cursor(pos);
        self.screen.scroll_to_line(pos.line, &self.buffer);

        Ok(())
    }

    /// Where a mark points, if it is set.
    fn mark_position(&self, name: char) -> Option<Position> {
        self.buffer.mark(crate::buffer::mark_slot(name)?)
    }

    /// Set the previous-context mark (`'`/`` ` ``) to `pos`: done before a
    /// jump, so `''` returns to where it started (POSIX 95142-95166).
    fn set_context_mark(&mut self, pos: Position) {
        self.buffer.set_mark(crate::buffer::CONTEXT_MARK, pos);
    }

    /// Perform substitution.
    fn substitute(
        &mut self,
        range: &AddressRange,
        pattern: &str,
        replacement: &str,
        flags: &SubstituteFlags,
    ) -> Result<()> {
        // An empty pattern reuses the last RE used in the editor -- from a
        // search or a previous substitute (ex.md §95700). Only the editor can
        // resolve that, which is why the parser now forwards an empty pattern
        // instead of rejecting it.
        let pattern = if pattern.is_empty() {
            self.last_regex()
                .ok_or(ViError::NoPreviousSubstitution)?
                .to_string()
        } else {
            pattern.to_string()
        };
        let pattern = pattern.as_str();

        // `~` alone as the replacement means "the previous replacement", which
        // is how a bare `s` repeats the last substitution.
        let prev_replacement = self
            .last_substitution
            .as_ref()
            .map(|l| l.replacement.clone())
            .unwrap_or_default();

        let sub = Substitutor::new(SubstituteConfig {
            pattern,
            replacement,
            global: flags.global,
            confirm: flags.confirm,
            print: flags.print,
            list: flags.list,
            number: flags.number,
            count_only: flags.count,
            ignorecase: self.options.ignorecase,
            magic: self.options.magic,
            prev_replacement: &prev_replacement,
        })?;

        // Resolve range. A trailing numeric count re-bases the range on the
        // last addressed line and extends it (substitute synopsis).
        let (mut start, mut end) = self.resolve_range(range)?;
        if let Some(n) = flags.line_count {
            let n = n.max(1);
            start = end;
            end = (start + n - 1).min(self.buffer.line_count());
        }
        let mut total_count = 0;
        // The last line a substitution changed: the current line afterwards
        // (95396), so `1s/a/X/|p` prints the changed line.
        let mut last_changed = None;
        let mut printed: Vec<String> = Vec::new();
        // `\n` in the replacement inserts lines mid-iteration, so walk the
        // range with an explicit cursor rather than a fixed `for` range.
        let mut line_num = start;
        // A signal during a confirmation prompt aborts the substitute.
        let mut aborted = false;

        while line_num <= end && !aborted {
            if let Some(line) = self.buffer.line(line_num) {
                let content = line.content().to_string();
                let (new_content, count) = if sub.needs_confirm() && !sub.is_count_only() {
                    sub.substitute_line_confirmed(&content, |line, s, e| {
                        if aborted {
                            return false;
                        }
                        match Self::confirm_substitution(line, s, e) {
                            Some(yes) => yes,
                            None => {
                                aborted = true;
                                false
                            }
                        }
                    })
                } else {
                    sub.substitute_line(&content)
                };

                if count > 0 && !sub.is_count_only() {
                    // A replacement containing `\n` splits the line into
                    // several. `replace_line` would stuff the newline into a
                    // single Line and corrupt the buffer's invariant, and the
                    // within-line undo record cannot express a line-count
                    // change -- hence the linewise path (#X12).
                    if new_content.contains('\n') {
                        let parts: Vec<String> =
                            new_content.split('\n').map(|s| s.to_string()).collect();
                        self.undo.note_dropped(self.buffer.take_dropped_marks());
                        self.undo.record_replace_lines(
                            Position::new(line_num, 0),
                            std::slice::from_ref(&content),
                            &parts,
                        );
                        self.buffer.delete_lines(line_num, line_num);
                        for (i, part) in parts.iter().enumerate() {
                            self.buffer
                                .insert_line_after(line_num - 1 + i, Line::from(part.as_str()));
                        }
                        // Later lines in the range have shifted down by the
                        // number of extra lines introduced here. Step over
                        // every part we just wrote: landing back on the first
                        // one re-substitutes it forever, growing the buffer
                        // until the process is killed.
                        end += parts.len() - 1;
                        total_count += count;
                        last_changed = Some(line_num + parts.len() - 1);
                        line_num += parts.len();
                        continue;
                    }
                    self.undo.note_dropped(self.buffer.take_dropped_marks());
                    self.undo.record_replace(
                        Position::new(line_num, 0),
                        &content,
                        &new_content,
                        false,
                    );
                    self.buffer.replace_line(line_num, &new_content)?;
                    last_changed = Some(line_num);

                    // `p`, `l` and `#` echo each changed line; `l` uses the
                    // unambiguous form and `#` prefixes the line number.
                    if sub.should_print() || sub.should_list() || sub.should_number() {
                        printed.extend(self.format_lines(
                            line_num,
                            line_num,
                            sub.should_number(),
                            sub.should_list(),
                        ));
                    }
                }
                total_count += count;
            }
            line_num += 1;
        }

        if let Some(line) = last_changed {
            self.buffer.set_line(line);
            self.buffer.move_to_first_non_blank();
        }
        if aborted {
            return Err(ViError::Interrupted);
        }

        if total_count == 0 {
            // POSIX 95361: an error "if the substitution fails on every
            // addressed line" -- except as one pass of a global, where, as in
            // historical ex and nvi, a line without a match is no error and
            // `g/foo/s/bar/baz/` goes on to the next line.
            if !self.in_global {
                return Err(ViError::PatternNotFound(pattern.to_string()));
            }
        } else if sub.is_count_only() {
            self.set_message(&format!("{} matches", total_count));
        } else {
            self.set_message(&format!("{} substitutions", total_count));
            // Save for & command
            self.last_substitution = Some(LastSubstitution {
                pattern: pattern.to_string(),
                replacement: replacement.to_string(),
                global: flags.global,
            });
            // #X28: the substitute pattern becomes the last RE, so a following
            // bare `n` or `//` reuses it.
            self.set_last_regex(pattern);
        }

        self.substitute_output = printed;
        Ok(())
    }

    /// The last RE used by a search or substitute (#X28).
    fn last_regex(&self) -> Option<&str> {
        self.last_regex.as_deref().or_else(|| self.search.pattern())
    }

    /// Record the last RE used, so `:s//repl/` and a bare `n` agree.
    fn set_last_regex(&mut self, pattern: &str) {
        self.last_regex = Some(pattern.to_string());
    }

    /// Ask whether to apply one substitution (the `c` flag).
    ///
    /// POSIX/historical ex reads the answer from standard input, in batch as
    /// well as interactively, so a script can answer inline. A line beginning
    /// with `y` accepts; anything else (including EOF) declines.  `None`: a
    /// hangup or an interrupt came instead of an answer, and the command is
    /// to stop (the hangup is left for the ex loop to act on).
    fn confirm_substitution(line: &str, start: usize, end: usize) -> Option<bool> {
        use std::io::Write;
        let mut out = std::io::stdout();
        let _ = writeln!(out, "{}", line);
        let _ = writeln!(
            out,
            "{}{}",
            " ".repeat(line[..start].chars().count()),
            "^".repeat(line[start..end].chars().count().max(1))
        );
        let _ = out.flush();

        let mut answer = String::new();
        loop {
            match read_text_line(&mut answer) {
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {
                    use std::sync::atomic::Ordering;
                    if crate::signals::HANGUP_RECEIVED.load(Ordering::SeqCst)
                        || crate::signals::take(&crate::signals::SIGINT_RECEIVED)
                    {
                        return None;
                    }
                }
                Ok(0) | Err(_) => return Some(false),
                Ok(_) => return Some(answer.trim_start().starts_with('y')),
            }
        }
    }

    /// The width `:l` folds at: the terminal's, less the column the `\`
    /// marking a fold takes.
    fn list_width(&self) -> usize {
        let cols = self.terminal.size().cols as usize;
        if cols > 1 {
            cols - 1
        } else {
            79
        }
    }

    /// Render a line in `list` form, per ex.md §95237-95244, folded into
    /// rows of at most `width` columns, each but the last ending in `\`
    /// ("Long lines shall be folded, with the point of folding indicated by
    /// <newline> preceded by <backslash>"):
    ///
    /// 1. characters in XBD Table 5-1 are written as their escape sequence;
    /// 2. other non-printable characters as a three-digit octal escape per
    ///    byte, most significant byte first;
    /// 3. the end of the line is marked `$`, and a literal `$` is written
    ///    `\$`.
    ///
    /// Used by the `:list` command and the substitute `l` flag alike.
    ///
    /// A character the locale says is printable is written as itself, a
    /// multibyte one included (`é` in a UTF-8 locale); anything else -- a
    /// control, a byte that is not a character in the locale -- as the C
    /// escape or one octal escape per byte.
    fn list_rows(s: &str, width: usize) -> Vec<String> {
        let mut rows = Vec::new();
        let mut row = String::new();
        let mut col = 0usize;
        let mut emit = |piece: &str, w: usize, row: &mut String, col: &mut usize| {
            if *col + w > width && *col > 0 {
                row.push('\\');
                rows.push(std::mem::take(row));
                *col = 0;
            }
            row.push_str(piece);
            *col += w;
        };
        for c in s.chars() {
            let piece: String = if let Some(b) = crate::bytes::escaped_byte(c) {
                // A byte that is not UTF-8: its own octal escape.
                format!("\\{:03o}", b)
            } else {
                match c {
                    // XBD Table 5-1.
                    '\\' => "\\\\".to_string(),
                    '\x07' => "\\a".to_string(),
                    '\x08' => "\\b".to_string(),
                    '\x0c' => "\\f".to_string(),
                    '\n' => "\\n".to_string(),
                    '\r' => "\\r".to_string(),
                    '\t' => "\\t".to_string(),
                    '\x0b' => "\\v".to_string(),
                    // A literal '$' must not be mistaken for the end marker.
                    '$' => "\\$".to_string(),
                    c if plib::locale::isprint(c) => c.to_string(),
                    // Not printable here: one octal escape per byte.
                    c => {
                        let mut buf = [0u8; 4];
                        c.encode_utf8(&mut buf)
                            .bytes()
                            .map(|b| format!("\\{:03o}", b))
                            .collect()
                    }
                }
            };
            let w = if piece.chars().count() == 1 {
                plib::locale::wcwidth_char(c).max(1) as usize
            } else {
                piece.len()
            };
            emit(&piece, w, &mut row, &mut col);
        }
        emit("$", 1, &mut row, &mut col);
        rows.push(row);
        rows
    }

    /// Write buffer to file.
    /// Execute an ex `write`.
    ///
    /// `lines` is `Some((start, end))` when an explicit address range was given,
    /// i.e. only part of the buffer is being written. It used to be flattened to
    /// a bool and then discarded, so `:1,5w out` silently wrote the *whole*
    /// buffer (#X24).
    ///
    /// Implements the numbered rules in ex.md §95502-95519. Rules 2, 3 and 5 are
    /// overridable by `!` or the `writeany` option; rule 6 is overridable by
    /// neither, which is why it is checked last and ignores both.
    fn write(
        &mut self,
        path: Option<&str>,
        lines: Option<(usize, usize)>,
        append: bool,
        force: bool,
    ) -> Result<()> {
        // Rule 4: no file operand means the current pathname; absent that, fail.
        let named = path.is_some();
        let path = path
            .map(crate::bytes::path)
            .or_else(|| self.files.current_file().map(|p| p.to_path_buf()))
            .ok_or(ViError::NoFileName)?;

        let overridable = force || self.options.writeany;
        let is_current = self.files.current_file() == Some(path.as_path());
        let exists = path.exists();

        // Rule 2: the readonly edit option blocks the write.
        if !overridable && self.is_readonly() {
            return Err(ViError::ReadOnly);
        }

        // Rule 3: writing to a named file that is not the current pathname and
        // already exists would clobber an unrelated file.
        if !overridable && named && !is_current && exists {
            return Err(ViError::FileExists(path.display().to_string()));
        }

        // Rule 5: the current pathname was changed by `:f` or `:r` and the file
        // exists -- writing would clobber a file the user did not edit. A
        // successful write clears the flag, so later writes do not re-fail.
        if !overridable && !named && self.files.pathname_changed() && exists {
            return Err(ViError::FileExists(path.display().to_string()));
        }

        // Rule 6: a partial write over an existing file. Not overridable by `!`
        // or `writeany` -- it is absent from both lists in §95516-95518.
        if lines.is_some() && exists {
            return Err(ViError::PartialWriteExists(path.display().to_string()));
        }

        let stats = match lines {
            Some((start, end)) => write_range(&self.buffer, &path, start, end, append)?,
            None => write_file(&self.buffer, &path, append)?,
        };

        // Only a complete write means the buffer is saved; a partial write
        // leaves the buffer modified.
        if lines.is_none() {
            self.buffer.mark_saved();
        }
        self.files.clear_pathname_changed();

        if self.files.current_file().is_none() {
            self.files.set_current_file(Some(path.clone()));
        }

        self.set_message(&stats.message(&path));
        Ok(())
    }

    /// Resolve an ex `write` address range to the lines to write.
    ///
    /// `None` means the whole buffer, which is what an absent range denotes.
    fn write_line_range(&self, range: &AddressRange) -> Result<Option<(usize, usize)>> {
        if range.explicit {
            Ok(Some(self.resolve_range(range)?))
        } else {
            Ok(None)
        }
    }

    /// Write and quit.
    fn write_and_quit(&mut self) -> Result<()> {
        if self.buffer.is_modified() {
            self.write(None, None, false, false)?;
        }
        self.quit(true)
    }

    /// Quit the editor.
    fn quit(&mut self, force: bool) -> Result<()> {
        if !force && self.buffer.is_modified() {
            return Err(ViError::FileModified);
        }

        self.should_quit = true;
        Ok(())
    }

    /// Calculate which screen row the cursor should appear on, accounting for line wrapping.
    /// Returns the screen row (1-indexed) or 0 if the cursor is not visible.
    fn calculate_cursor_screen_row(
        &self,
        cursor_line: usize,
        cursor_col: usize,
        top_line: usize,
        top_offset: usize,
        max_rows: usize,
        cols: usize,
    ) -> usize {
        if cursor_line < top_line {
            return 0; // Cursor is above visible area
        }

        let mut screen_row = 1; // 1-indexed
        let mut buffer_line = top_line;
        let mut skip_wrapped_rows = top_offset;

        // Walk through buffer lines until we reach the cursor line
        while buffer_line < cursor_line && screen_row <= max_rows {
            if let Some(line) = self.buffer.line(buffer_line) {
                let wrapped = self.screen.expand_and_wrap_line(line.content(), cols);

                // Skip wrapped rows from top_offset for the first line
                if buffer_line == top_line && skip_wrapped_rows > 0 {
                    let visible_rows = wrapped.len().saturating_sub(skip_wrapped_rows);
                    screen_row += visible_rows;
                    skip_wrapped_rows = 0;
                } else {
                    screen_row += wrapped.len();
                }
            } else {
                screen_row += 1;
            }
            buffer_line += 1;
        }

        if buffer_line != cursor_line || screen_row > max_rows {
            return 0; // Cursor line not reached or off-screen
        }

        // Now we're at the cursor line - find which wrapped row the cursor is on
        if let Some(line) = self.buffer.line(cursor_line) {
            let display_col = self
                .screen
                .buffer_col_to_display_col(line.content(), cursor_col);
            let wrapped_row_index = display_col / cols;

            // If this is the top line, account for offset
            if cursor_line == top_line {
                if wrapped_row_index < skip_wrapped_rows {
                    return 0; // Cursor is in a wrapped row that's scrolled off
                }
                screen_row += wrapped_row_index - skip_wrapped_rows;
            } else {
                screen_row += wrapped_row_index;
            }
        }

        if screen_row > max_rows {
            0 // Off-screen
        } else {
            screen_row
        }
    }

    /// Refresh the screen.
    fn refresh_screen(&mut self) -> Result<()> {
        const LINE_NUMBER_WIDTH: usize = 8; // "%6d  " format

        let size = self.terminal.size();
        self.screen.resize(size);
        self.screen
            .scroll_to_line(self.buffer.cursor().line, &self.buffer);

        self.terminal.hide_cursor()?;
        self.terminal.move_cursor_home()?;

        // Render buffer lines with wrapping
        let top = self.screen.top_line();
        let top_offset = self.screen.top_line_offset();
        let height = (size.rows as usize).saturating_sub(1); // Leave room for status
        let avail_cols = if self.options.number {
            (size.cols as usize).saturating_sub(LINE_NUMBER_WIDTH)
        } else {
            size.cols as usize
        };

        // Guard against zero or very small terminal width
        if avail_cols == 0 {
            // Terminal too narrow to display content
            self.terminal.show_cursor()?;
            self.terminal.flush()?;
            return Ok(());
        }

        let mut screen_row = 0;
        let mut buffer_line = top;
        let mut skip_wrapped_rows = top_offset;

        while screen_row < height {
            self.terminal.move_cursor((screen_row + 1) as u16, 1)?;
            self.terminal.clear_line_to_end()?;

            if buffer_line <= self.buffer.line_count() {
                if let Some(line) = self.buffer.line(buffer_line) {
                    // Wrap the line
                    let wrapped = self.screen.expand_and_wrap_line(line.content(), avail_cols);

                    // Skip wrapped rows if we're starting mid-line
                    for (wrap_idx, wrapped_row) in wrapped.iter().enumerate() {
                        if skip_wrapped_rows > 0 {
                            skip_wrapped_rows -= 1;
                            continue;
                        }
                        if screen_row >= height {
                            break;
                        }

                        // Move to the correct position
                        self.terminal.move_cursor((screen_row + 1) as u16, 1)?;
                        self.terminal.clear_line_to_end()?;

                        // Line number prefix when option set (only on first wrapped row)
                        if self.options.number && wrap_idx == 0 {
                            self.terminal.write_str(&format!("{:6}  ", buffer_line))?;
                        } else if self.options.number {
                            // Indent continuation lines with same width as line numbers
                            self.terminal.write_str(&" ".repeat(LINE_NUMBER_WIDTH))?;
                        }

                        self.terminal.write_str(wrapped_row)?;
                        screen_row += 1;
                    }
                    buffer_line += 1;
                } else {
                    self.terminal.write_str("~")?;
                    screen_row += 1;
                    buffer_line += 1;
                }
            } else {
                self.terminal.write_str("~")?;
                screen_row += 1;
                buffer_line += 1;
            }
        }

        // Render status line
        self.terminal.move_cursor(size.rows, 1)?;
        self.terminal.clear_line_to_end()?;
        let status = self.format_status();
        self.terminal.write_str(&status)?;

        // Position cursor
        let cursor = self.buffer.cursor();

        // Calculate the actual screen row considering line wrapping
        let display_line = self.calculate_cursor_screen_row(
            cursor.line,
            cursor.column,
            top,
            top_offset,
            height,
            avail_cols,
        ) as u16;

        let cursor_line_content = self
            .buffer
            .line(cursor.line)
            .map(|l| l.content())
            .unwrap_or("");
        let display_col_in_line = self
            .screen
            .buffer_col_to_display_col(cursor_line_content, cursor.column);

        // Which wrapped row of the cursor's line are we on?
        let display_col_in_wrapped_row = display_col_in_line % avail_cols;

        // Add 1 for 1-indexed and add line number width if needed
        let display_col = if self.options.number {
            (display_col_in_wrapped_row + 1 + LINE_NUMBER_WIDTH) as u16
        } else {
            (display_col_in_wrapped_row + 1) as u16
        };

        if self.mode == Mode::Ex {
            // Cursor at command line
            self.terminal
                .move_cursor(size.rows, (self.ex_input.len() + 2) as u16)?;
        } else {
            self.terminal.move_cursor(display_line, display_col)?;

            // "For purposes of the display only, the editor shall behave as if
            // a '^' character was entered, and the cursor shall be positioned
            // as if overwriting the '^' character" (121878-121880). Nothing
            // enters the buffer, so we draw over the cell and step back onto
            // it. Safe without extra bookkeeping because this is a full redraw
            // per keystroke — the next refresh erases the '^'.
            if self
                .insert_state
                .as_ref()
                .is_some_and(|s| s.pending_literal)
            {
                self.terminal.write_str("^")?;
                self.terminal.move_cursor(display_line, display_col)?;
            }
        }

        self.terminal.show_cursor()?;
        self.terminal.flush()?;
        Ok(())
    }

    /// Format the status line.
    fn format_status(&self) -> String {
        if self.mode == Mode::Ex {
            return format!(":{}", self.ex_input);
        }

        if let Some(msg) = &self.message {
            return msg.clone();
        }

        // Build status line
        let mut status = String::new();

        // File name
        if let Some(path) = self.files.current_file() {
            status.push_str(&crate::bytes::from_path(path));
        } else {
            status.push_str("[No Name]");
        }

        // Modified flag
        if self.buffer.is_modified() {
            status.push_str(" [+]");
        }

        // Mode indicator
        if self.options.showmode {
            match &self.mode {
                Mode::Insert(_) => status.push_str(" -- INSERT --"),
                Mode::Replace => status.push_str(" -- REPLACE --"),
                Mode::Open => status.push_str(" -- INSERT --"),
                _ => {}
            }
        }

        status
    }

    /// Get file info string.
    /// The `:f` / `^G` informational message.
    ///
    /// 94981-94987 lists what it must contain: the current pathname, or that
    /// there is none; the current line number and the number of lines in the
    /// buffer, or that the buffer is empty; the fact that the buffer has been
    /// modified since the last complete write, if so; and the fact that the
    /// readonly edit option is set, if so.
    fn file_info(&self) -> String {
        let mut parts = Vec::new();
        parts.push(match self.files.current_file() {
            Some(p) => format!("\"{}\"", p.display()),
            None => "[No file]".to_string(),
        });
        if self.options.readonly {
            parts.push("[Read only]".to_string());
        }
        if self.buffer.is_modified() {
            parts.push("[Modified]".to_string());
        }

        let lines = self.buffer.line_count();
        if lines == 0 {
            parts.push("--No lines in buffer--".to_string());
        } else {
            let cursor = self.buffer.cursor();
            parts.push(format!(
                "line {} of {} --{}%--",
                cursor.line,
                lines,
                cursor.line * 100 / lines
            ));
        }
        parts.join(" ")
    }

    /// Set a message.
    fn set_message(&mut self, msg: &str) {
        self.message = Some(msg.to_string());
        self.is_error = false;
    }

    /// Set an error message.
    fn set_error(&mut self, msg: &str) {
        self.message = Some(msg.to_string());
        self.is_error = true;
    }

    /// Discard the current message.
    ///
    /// `is_error` describes `message`, so the two have to be cleared together.
    /// Four sites used to assign `self.message = None` on its own, leaving the
    /// flag set from a message that no longer existed — after which a command
    /// that succeeded silently still read as having failed.
    fn clear_message(&mut self) {
        self.message = None;
        self.is_error = false;
    }

    /// Page up (Ctrl-B).
    fn page_up(&mut self) -> Result<()> {
        let window = self.options.window.max(3);
        let scroll = window.saturating_sub(2);
        let current = self.buffer.cursor().line;
        let new_line = current.saturating_sub(scroll).max(1);
        self.buffer.set_line(new_line);
        self.buffer.move_to_first_non_blank();
        Ok(())
    }

    /// Scroll by `step` up to `count` times, stopping once it goes nowhere.
    fn repeat_scroll(&mut self, count: usize, step: fn(&mut Self) -> Result<()>) -> Result<()> {
        for _ in 0..count {
            let before = (self.buffer.cursor(), self.screen.top_line());
            step(self)?;
            if (self.buffer.cursor(), self.screen.top_line()) == before {
                break;
            }
        }
        Ok(())
    }

    /// Page down (Ctrl-F).
    fn page_down(&mut self) -> Result<()> {
        let window = self.options.window.max(3);
        let scroll = window.saturating_sub(2);
        let current = self.buffer.cursor().line;
        let max_line = self.buffer.line_count();
        let new_line = current.saturating_add(scroll).min(max_line);
        self.buffer.set_line(new_line);
        self.buffer.move_to_first_non_blank();
        Ok(())
    }

    /// Scroll down half page (Ctrl-D).
    fn scroll_down(&mut self) -> Result<()> {
        let scroll = self.options.scroll.max(1);
        let current = self.buffer.cursor().line;
        let max_line = self.buffer.line_count();
        let new_line = current.saturating_add(scroll).min(max_line);
        self.buffer.set_line(new_line);
        self.buffer.move_to_first_non_blank();
        Ok(())
    }

    /// Scroll up half page (Ctrl-U).
    fn scroll_up(&mut self) -> Result<()> {
        let scroll = self.options.scroll.max(1);
        let current = self.buffer.cursor().line;
        let new_line = current.saturating_sub(scroll).max(1);
        self.buffer.set_line(new_line);
        self.buffer.move_to_first_non_blank();
        Ok(())
    }

    /// Scroll one line down (Ctrl-E).
    fn scroll_line_down(&mut self) -> Result<()> {
        let top = self.screen.top_line();
        let max_line = self.buffer.line_count();
        if top < max_line {
            self.screen.set_top_line(top + 1);
            // Keep cursor visible
            let cursor_line = self.buffer.cursor().line;
            if cursor_line < top + 1 {
                self.buffer.set_line(top + 1);
            }
        }
        Ok(())
    }

    /// Scroll one line up (Ctrl-Y).
    fn scroll_line_up(&mut self) -> Result<()> {
        let top = self.screen.top_line();
        if top > 1 {
            self.screen.set_top_line(top - 1);
            // Keep cursor visible
            let size = self.terminal.size();
            let height = (size.rows as usize).saturating_sub(1);
            let cursor_line = self.buffer.cursor().line;
            if cursor_line >= top - 1 + height {
                self.buffer.set_line(top - 1 + height - 1);
            }
        }
        Ok(())
    }

    /// Replace character at cursor (r command).
    fn replace_char(&mut self, c: char) {
        let line = self.buffer.cursor().line;
        let _ = self.splice_around(line, line, |ed| {
            ed.replace_char_inner(c);
            Ok(())
        });
    }

    /// `[count] r<CR>`: the `count` characters at the cursor become `count`
    /// new lines, all empty but the last, which takes the rest of the line
    /// -- autoindented like it if `autoindent` is set (121498-121506).
    fn replace_with_newlines(&mut self, count: usize) -> Result<()> {
        let cursor = self.buffer.cursor();
        let Some(content) = self
            .buffer
            .line(cursor.line)
            .map(|l| l.content().to_string())
        else {
            return Ok(());
        };
        let col = cursor.column.min(content.len());
        let rest_at = content[col..]
            .char_indices()
            .nth(count)
            .map_or(content.len(), |(i, _)| col + i);
        let indent = if self.options.autoindent {
            leading_blanks(&content)
        } else {
            ""
        };
        let mut lines = vec![content[..col].to_string()];
        lines.extend(std::iter::repeat_n(String::new(), count - 1));
        lines.push(format!("{indent}{}", &content[rest_at..]));
        let line = cursor.line;
        self.splice_around(line, line, |ed| {
            ed.buffer.replace_line(line, &lines[0])?;
            for (i, text) in lines[1..].iter().enumerate() {
                ed.buffer
                    .insert_line_after(line + i, Line::from(text.as_str()));
            }
            Ok(())
        })?;
        self.buffer.set_line(line + count);
        self.buffer.move_to_first_non_blank();
        Ok(())
    }

    fn replace_char_inner(&mut self, c: char) {
        let cursor = self.buffer.cursor();
        let Some(line) = self.buffer.line(cursor.line) else {
            return;
        };
        let col = cursor.column;
        if col >= line.len() {
            return;
        }
        // Rebuild through `replace_line` rather than reaching in with
        // `line_mut`, which edits behind the buffer's back: `r` set neither
        // the modified flag nor any undo record, so `rX` then `:q` exited
        // with no warning and the edit was simply lost.
        let end = line.next_char_offset(col).unwrap_or(line.len());
        let mut updated = String::with_capacity(line.len());
        updated.push_str(&line.content()[..col]);
        updated.push(c);
        updated.push_str(&line.content()[end..]);
        let _ = self.buffer.replace_line(cursor.line, &updated);
    }

    /// Toggle case of character at cursor (~ command).
    fn toggle_case(&mut self) {
        let line = self.buffer.cursor().line;
        let _ = self.splice_around(line, line, |ed| {
            ed.toggle_case_inner();
            Ok(())
        });
    }

    fn toggle_case_inner(&mut self) {
        let cursor = self.buffer.cursor();
        let char_at = self
            .buffer
            .line(cursor.line)
            .and_then(|line| line.char_at(cursor.column));

        if let Some(c) = char_at {
            let toggled = if c.is_uppercase() {
                c.to_lowercase().next().unwrap_or(c)
            } else if c.is_lowercase() {
                c.to_uppercase().next().unwrap_or(c)
            } else {
                c
            };
            let Some(line) = self.buffer.line(cursor.line) else {
                return;
            };
            let col = cursor.column;
            let end = line.next_char_offset(col).unwrap_or(line.len());
            let mut updated = String::with_capacity(line.len());
            updated.push_str(&line.content()[..col]);
            updated.push(toggled);
            updated.push_str(&line.content()[end..]);
            let _ = self.buffer.replace_line(cursor.line, &updated);

            // Advance by the width of the character just written, not by one
            // byte: `col + 1` landed inside the next character on any line
            // with multi-byte text.  The replacement may differ in width from
            // what it replaced, so measure it in `updated`.
            let next = self
                .buffer
                .line(cursor.line)
                .and_then(|l| l.next_char_offset(col));
            if let Some(next) = next {
                if next < updated.len() {
                    self.buffer.set_cursor(Position::new(cursor.line, next));
                }
            }
        }
    }

    /// Repeat last substitution (& command).
    fn repeat_substitution(&mut self) -> Result<()> {
        if let Some(last_sub) = self.last_substitution.clone() {
            let flags = SubstituteFlags {
                global: last_sub.global,
                confirm: false,
                print: false,
                count: false,
                ignore_case: self.options.ignorecase,
                list: false,
                number: false,
                line_count: None,
            };
            let range = AddressRange::current();
            self.substitute(&range, &last_sub.pattern, &last_sub.replacement, &flags)?;
        } else {
            self.set_error("No previous substitution");
        }
        Ok(())
    }

    /// Redraw window (z command).
    fn redraw_window(&mut self, arg: char, count: usize) -> Result<()> {
        let target_line = if count > 0 {
            count.min(self.buffer.line_count())
        } else {
            self.buffer.cursor().line
        };

        let size = self.terminal.size();
        let window_height = (size.rows as usize).saturating_sub(2);

        match arg {
            '\n' | '\r' | '+' => {
                // Put target line at top of screen
                self.screen.set_top_line(target_line);
                self.buffer.set_line(target_line);
            }
            '.' => {
                // Put target line in middle of screen
                let top = target_line.saturating_sub(window_height / 2);
                self.screen.set_top_line(top.max(1));
                self.buffer.set_line(target_line);
            }
            '-' => {
                // Put target line at bottom of screen
                let top = target_line.saturating_sub(window_height - 1);
                self.screen.set_top_line(top.max(1));
                self.buffer.set_line(target_line);
            }
            _ => {}
        }

        self.buffer.move_to_first_non_blank();
        Ok(())
    }

    /// Get word under cursor (for Ctrl-]).
    fn word_under_cursor(&self) -> Option<String> {
        let cursor = self.buffer.cursor();
        let line = self.buffer.line(cursor.line)?;
        let content = line.content();

        if content.is_empty() {
            return None;
        }

        // Boundary-correct: `len() - 1` lands inside a trailing multi-byte
        // character.
        let col = cursor.column.min(line.last_char_offset());
        let chars: Vec<char> = content.chars().collect();

        // Check if cursor is on a word character
        if col >= chars.len() || !chars[col].is_alphanumeric() && chars[col] != '_' {
            return None;
        }

        // Find start of word
        let mut start = col;
        while start > 0 && (chars[start - 1].is_alphanumeric() || chars[start - 1] == '_') {
            start -= 1;
        }

        // Find end of word
        let mut end = col;
        while end < chars.len() && (chars[end].is_alphanumeric() || chars[end] == '_') {
            end += 1;
        }

        Some(chars[start..end].iter().collect())
    }

    /// Go to tag.
    ///
    /// `force` is the `!` of `:tag!`: replace a modified buffer anyway.
    fn goto_tag(&mut self, tag: &str, force: bool) -> Result<()> {
        // The `tags` option is a space-separated list of tags files.
        let tags_opt = self.options.tags.clone();
        let files: Vec<&str> = tags_opt.split_whitespace().collect();
        let taglength = self.options.taglength;

        let Some(m) = crate::tags::lookup(&files, tag, taglength) else {
            // 95413: "it shall be an error".
            return Err(ViError::InvalidCommand(format!("tag not found: {}", tag)));
        };

        // Where we are now, read before open() can replace the buffer. `None`
        // means no file is current yet -- the `-t` startup option arrives here
        // before anything is open -- and there is no position to return to, so
        // nothing is pushed. Gating on this rather than on the caller is what
        // keeps `:tag` and `^]` from each needing to know about the stack.
        let origin = self
            .files
            .current_file()
            .map(|p| (crate::bytes::from_path(p), self.buffer.cursor()));

        // Switch to the tag's file unless it is already the current file.
        let already_open = origin
            .as_ref()
            .map(|(path, _)| path.as_str() == m.file.as_str())
            .unwrap_or(false);
        if !already_open {
            // 95419-95423: only replacing the buffer with another file
            // refuses a modified one, and `!` overrides that.  A tag in the
            // current file just moves the cursor, modified or not.
            if !force && self.buffer.is_modified() {
                return Err(ViError::FileModified);
            }
            self.open(&m.file)?;
        }

        // Past both ways out that leave the cursor where it was, so the jump is
        // now certain to have moved us -- including the case where the file
        // opened but the pattern below is not found, which still changed the
        // buffer and so still needs a way back.
        if let Some((file, pos)) = origin {
            if self.tag_stack.len() == crate::tags::TAG_STACK_MAX {
                self.tag_stack.remove(0);
            }
            self.tag_stack.push(crate::tags::TagStackEntry {
                tag: tag.to_string(),
                file,
                pos,
            });
        }

        // Move to the definition.
        match m.address {
            crate::tags::TagAddress::Line(n) => {
                // 95425-95426: a line past the end is an error.
                if n == 0 || n > self.buffer.line_count() {
                    return Err(ViError::InvalidAddress(format!("tag line {}", n)));
                }
                self.buffer.set_line(n);
                self.buffer.move_to_first_non_blank();
            }
            crate::tags::TagAddress::Pattern(pat) => {
                // 95417-95418: the pattern is an RE used in the editor -- the
                // last RE from now on.  Compiled without `magic`, as
                // historical vi searched tags: ctags writes the source line
                // with only `\` and `/` escaped, so its `*` and `[` are text.
                let re = crate::search::compile_pattern(&pat, self.options.ignorecase, false)?;
                // As the last RE, in the form that means the same whatever
                // `magic` is: its specials escaped.
                self.set_last_regex(&crate::search::literal_pattern(&pat));
                let found = (1..=self.buffer.line_count()).find(|&n| {
                    self.buffer
                        .line(n)
                        .is_some_and(|line| re.is_match(line.content()))
                });
                match found {
                    Some(n) => {
                        self.buffer.set_line(n);
                        self.buffer.move_to_first_non_blank();
                    }
                    None => {
                        return Err(ViError::PatternNotFound(pat));
                    }
                }
            }
        }
        Ok(())
    }

    /// Return to where the most recent `:tag` or `^]` jump started.
    ///
    /// Behind both `:pop` and `^T`. Not POSIX -- see NONPOSIX.md.
    fn pop_tag(&mut self) -> Result<()> {
        let Some(top) = self.tag_stack.last() else {
            return Err(ViError::TagStackEmpty);
        };
        let file = top.file.clone();
        let pos = top.pos;

        let same_file = self
            .files
            .current_file()
            .map(|p| crate::bytes::from_path(p) == file.as_str())
            .unwrap_or(false);

        // Refuse before popping, so `:w` and a second `:pop` still work. This
        // is the one failure the user can recover from; every other one below
        // discards the entry.
        if !same_file && self.buffer.is_modified() {
            return Err(ViError::FileModified);
        }

        // Pop before opening. If the file has been deleted since the jump, the
        // user gets one diagnostic and the entry is gone, rather than a
        // top-of-stack that can never be popped.
        self.tag_stack.pop();
        if !same_file {
            self.open(&file)?;
        }
        // set_cursor clamps the line to the buffer and then the column to the
        // line, so an entry pointing past the end of a file that has since
        // shrunk lands on the nearest valid position instead of failing.
        self.buffer.set_cursor(pos);
        Ok(())
    }

    /// Public entry for the `-t` startup option and the `:tag` command.
    pub fn tag(&mut self, tag: &str) -> Result<()> {
        self.goto_tag(tag, false)
    }
}

impl Default for Editor {
    fn default() -> Self {
        Self::new(false, false).expect("Failed to create editor")
    }
}

/// True when `s` ends with an odd number of backslashes, i.e. the final
/// backslash is escaping whatever follows (here, the newline).
fn ends_with_odd_backslash(s: &str) -> bool {
    s.chars().rev().take_while(|c| *c == '\\').count() % 2 == 1
}

/// Read one line of ex input as editor text, escaping any byte that is not
/// UTF-8 (see [`crate::bytes`]) so that a command may name the bytes a file
/// holds.  Returns the number of bytes read, 0 at end of input.
fn read_text_line(line: &mut String) -> std::io::Result<usize> {
    let mut bytes = Vec::new();
    let n = crate::input::lines::read_line(&mut bytes)?;
    line.push_str(&crate::bytes::decode(&bytes));
    Ok(n)
}

/// What [`Editor::read_ex_line`] got.
enum ExRead {
    /// A line (or the last, unterminated, part of one).
    Line,
    /// End of file.
    Eof,
    /// SIGINT on a terminal: back to the prompt.
    Interrupted,
    /// The session is over (hangup, or an error or interrupt with standard
    /// input not a terminal).
    Quit,
}

#[cfg(test)]
mod tests {
    // Note: Most Editor tests require a terminal, so we test individual components instead.
    // Integration tests will be added in Phase 13.
}
