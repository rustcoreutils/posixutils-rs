//! Main editor state and loop.
//!
//! This module contains the Editor struct which ties together all
//! the components of the vi editor.

use crate::buffer::{Buffer, BufferMode, Line, Position, Range};
use crate::command::CommandParser;
use crate::error::{Result, ViError};
use crate::ex::command::SubstituteFlags;
use crate::ex::{parse_ex_command, AddressRange, ExCommand, ExResult};
use crate::file::{read_file, write_file, FileManager};
use crate::input::{InputReader, Key};
use crate::mode::{enter_insert_mode, process_insert_key, InsertKind, InsertState, Mode};
use crate::options::Options;
use crate::register::{RegisterContent, Registers};
use crate::search::{SearchDirection, SearchState, Substitutor};
use crate::shell::ShellExecutor;
use crate::ui::{Screen, Terminal, TerminalSize};
use crate::undo::UndoManager;
use std::path::PathBuf;

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
    /// Simple command that doesn't go through the parser (x, X, J, etc.).
    Simple {
        command: char,
        count: usize,
        char_arg: Option<char>,
    },
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
    /// Ex command line input.
    ex_input: String,
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
    /// Marks (a-z for user, other for special).
    marks: [Option<Position>; 26],
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
    last_substitution: Option<(String, String, bool)>,
    /// Last command for . (dot) repeat.
    last_command: Option<LastCommand>,
    /// Last macro register for @@ repeat.
    last_macro_register: Option<char>,
    /// Ex text input mode (:a, :i, :c).
    ex_insert_mode: Option<ExInsertMode>,
    /// Accumulated text for ex insert mode.
    ex_insert_text: Vec<String>,
    /// Whether running in headless mode (for testing).
    #[allow(dead_code)]
    headless: bool,
    /// Whether running in ex standalone mode (line-oriented).
    ex_standalone_mode: bool,
    /// Silent/batch mode (suppress prompts and messages).
    silent_mode: bool,
}

impl Editor {
    /// Create a new editor.
    pub fn new() -> Result<Self> {
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
            ex_input: String::new(),
            options,
            registers: Registers::new(),
            undo: UndoManager::new(),
            search: SearchState::new(),
            files: FileManager::new(),
            terminal,
            screen: Screen::new(size),
            message: None,
            is_error: false,
            marks: [None; 26],
            should_quit: false,
            exit_code: 0,
            shell,
            pending_filter: None,
            last_find: None,
            last_substitution: None,
            last_command: None,
            last_macro_register: None,
            ex_insert_mode: None,
            ex_insert_text: Vec::new(),
            headless: false,
            ex_standalone_mode: false,
            silent_mode: false,
        })
    }

    /// Create a new editor with specified mode options.
    ///
    /// # Arguments
    /// * `ex_mode` - If true, start in ex (line-oriented) mode
    /// * `silent` - If true, suppress prompts and informational messages
    pub fn new_with_mode(ex_mode: bool, silent: bool) -> Result<Self> {
        let mut editor = if ex_mode && !silent {
            // Ex mode with terminal - still need terminal for potential visual switch
            Self::new()?
        } else if ex_mode && silent {
            // Ex silent mode - no terminal needed, use headless-like setup
            Self::new_ex_silent()?
        } else {
            // Visual mode
            Self::new()?
        };

        editor.ex_standalone_mode = ex_mode;
        editor.silent_mode = silent;

        Ok(editor)
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
            ex_input: String::new(),
            options,
            registers: Registers::new(),
            undo: UndoManager::new(),
            search: SearchState::new(),
            files: FileManager::new(),
            terminal: Terminal::new_dummy(),
            screen: Screen::new(size),
            message: None,
            is_error: false,
            marks: [None; 26],
            should_quit: false,
            exit_code: 0,
            shell,
            pending_filter: None,
            last_find: None,
            last_substitution: None,
            last_command: None,
            last_macro_register: None,
            ex_insert_mode: None,
            ex_insert_text: Vec::new(),
            headless: false,
            ex_standalone_mode: true,
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
            ex_input: String::new(),
            options,
            registers: Registers::new(),
            undo: UndoManager::new(),
            search: SearchState::new(),
            files: FileManager::new(),
            terminal: Terminal::new_dummy(),
            screen: Screen::new(size),
            message: None,
            is_error: false,
            marks: [None; 26],
            should_quit: false,
            exit_code: 0,
            shell,
            pending_filter: None,
            last_find: None,
            last_substitution: None,
            last_command: None,
            last_macro_register: None,
            ex_insert_mode: None,
            ex_insert_text: Vec::new(),
            headless: true,
            ex_standalone_mode: false,
            silent_mode: false,
        }
    }

    /// Set read-only mode.
    pub fn set_readonly(&mut self, readonly: bool) {
        self.files.set_readonly(readonly);
    }

    /// Execute an initial command (from -c or +command).
    pub fn execute_initial_command(&mut self, cmd: &str) -> Result<()> {
        self.execute_ex_input(cmd)
    }

    /// Set the buffer content directly (for testing).
    pub fn set_buffer_text(&mut self, text: &str) {
        self.buffer = Buffer::from_text(text);
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

    /// Process a single key (public for testing).
    pub fn process_key(&mut self, key: Key) -> Result<()> {
        self.handle_key(key)
    }

    /// Execute a sequence of keys from a string (for testing).
    /// Special characters: \x1b = Escape, \n = Enter
    pub fn execute_keys(&mut self, keys: &str) -> Result<()> {
        for c in keys.chars() {
            let key = match c {
                '\x1b' => Key::Escape,
                '\n' | '\r' => Key::Enter,
                '\x7f' => Key::Backspace,
                c if c.is_ascii_control() => {
                    // Convert control characters (Ctrl-A = 0x01, etc.)
                    let ctrl_char = (c as u8 + b'@') as char;
                    Key::Ctrl(ctrl_char.to_ascii_lowercase())
                }
                c => Key::Char(c),
            };
            self.handle_key(key)?;
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
        let path_buf = PathBuf::from(path);
        self.buffer = read_file(&path_buf)?;
        self.files.set_current_file(Some(path_buf.clone()));
        // Update shell executor with current/alternate files
        let old_current = self.shell_current_file();
        self.shell
            .set_current_file(Some(path_buf.to_string_lossy().into_owned()));
        if old_current.is_some() {
            self.shell.set_alternate_file(old_current);
        }
        self.buffer.set_line(1);
        self.buffer.move_to_first_non_blank();
        self.undo.clear();
        Ok(())
    }

    /// Get current file as string for shell.
    fn shell_current_file(&self) -> Option<String> {
        self.files
            .current_file()
            .map(|p| p.to_string_lossy().into_owned())
    }

    /// Open multiple files.
    pub fn open_files(&mut self, paths: Vec<String>) -> Result<()> {
        if paths.is_empty() {
            return Ok(());
        }

        let path_bufs: Vec<PathBuf> = paths.iter().map(PathBuf::from).collect();
        self.files.set_args(path_bufs);

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
        use std::io::{self, BufRead, Write};

        let stdin = io::stdin();
        let mut stdout = io::stdout();

        while !self.should_quit {
            // Check if we should switch to visual mode
            if !self.ex_standalone_mode {
                // Switch to visual mode
                return self.run_visual_mode();
            }

            // Check if we're in text input mode (:a, :i, :c)
            if self.ex_insert_mode.is_some() {
                // Read text lines until we see a line containing only "."
                let mut line = String::new();
                match stdin.lock().read_line(&mut line) {
                    Ok(0) => {
                        // EOF - finalize and quit
                        let _ = self.finalize_ex_insert();
                        self.should_quit = true;
                        break;
                    }
                    Ok(_) => {}
                    Err(e) => {
                        if !self.silent_mode {
                            eprintln!("Read error: {}", e);
                        }
                        self.should_quit = true;
                        self.exit_code = 1;
                        break;
                    }
                }

                let line = line.trim_end();
                if line == "." {
                    // End of text input
                    if let Err(e) = self.finalize_ex_insert() {
                        if !self.silent_mode {
                            eprintln!("{}", e);
                        }
                    } else if !self.silent_mode {
                        if let Some(msg) = &self.message {
                            println!("{}", msg);
                        }
                    }
                    self.message = None;
                } else {
                    // Accumulate text
                    self.ex_insert_text.push(line.to_string());
                }
                continue;
            }

            // Print prompt unless in silent mode
            if !self.silent_mode {
                print!(":");
                stdout.flush()?;
            }

            // Read a line of input
            let mut line = String::new();
            match stdin.lock().read_line(&mut line) {
                Ok(0) => {
                    // EOF - treat as quit
                    self.should_quit = true;
                    break;
                }
                Ok(_) => {}
                Err(e) => {
                    if !self.silent_mode {
                        eprintln!("Read error: {}", e);
                    }
                    self.should_quit = true;
                    self.exit_code = 1;
                    break;
                }
            }

            // Remove trailing newline
            let line = line.trim_end();

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
                            println!("{}", msg);
                        }
                    }
                }
                Err(e) => {
                    // Errors always go to stderr
                    eprintln!("{}", e);
                    // In silent mode, errors are fatal
                    if self.silent_mode {
                        self.exit_code = 1;
                        self.should_quit = true;
                    }
                }
            }
            self.message = None;
        }

        Ok(self.exit_code)
    }

    /// Visual mode main editing loop.
    fn visual_main_loop(&mut self) -> Result<()> {
        let mut reader = InputReader::new();

        while !self.should_quit && !self.ex_standalone_mode {
            self.refresh_screen()?;

            let key = reader.read_key()?;
            self.handle_key(key)?;
        }

        Ok(())
    }

    /// Handle a key press.
    fn handle_key(&mut self, key: Key) -> Result<()> {
        // Clear any previous message
        if !matches!(key, Key::Char(':')) {
            self.message = None;
        }

        match &self.mode {
            Mode::Command => self.handle_command_key(key),
            Mode::Insert(_) | Mode::Replace | Mode::Open => self.handle_insert_key(key),
            Mode::Ex => self.handle_ex_key(key),
        }
    }

    /// Handle a key in command mode.
    fn handle_command_key(&mut self, key: Key) -> Result<()> {
        // Handle control key commands first
        match key {
            Key::Ctrl('b') | Key::PageUp => {
                // Page backward
                self.page_up()?;
                return Ok(());
            }
            Key::Ctrl('f') | Key::PageDown => {
                // Page forward
                self.page_down()?;
                return Ok(());
            }
            Key::Ctrl('d') => {
                // Scroll down half page
                self.scroll_down()?;
                return Ok(());
            }
            Key::Ctrl('u') => {
                // Scroll up half page
                self.scroll_up()?;
                return Ok(());
            }
            Key::Ctrl('e') => {
                // Scroll forward by line
                self.scroll_line_down()?;
                return Ok(());
            }
            Key::Ctrl('y') => {
                // Scroll backward by line
                self.scroll_line_up()?;
                return Ok(());
            }
            Key::Ctrl('g') => {
                // Display file info
                let info = self.file_info();
                self.set_message(&info);
                return Ok(());
            }
            Key::Ctrl('r') | Key::Ctrl('l') => {
                // Redraw screen - just return, will redraw on next refresh
                return Ok(());
            }
            Key::Ctrl(']') => {
                // Tag jump - get word under cursor and jump to tag
                if let Some(tag) = self.word_under_cursor() {
                    let _ = self.goto_tag(&tag);
                }
                return Ok(());
            }
            Key::Ctrl('^') => {
                // Edit alternate file
                if let Some(alt) = self.files.alternate_file() {
                    let path = alt.to_string_lossy().to_string();
                    let _ = self.open(&path);
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
                    self.ex_input.clear();
                    return Ok(());
                }
                Key::Char('/') => {
                    self.mode = Mode::Ex;
                    self.ex_input = "/".to_string();
                    return Ok(());
                }
                Key::Char('?') => {
                    self.mode = Mode::Ex;
                    self.ex_input = "?".to_string();
                    return Ok(());
                }
                Key::Char('i') => {
                    self.enter_insert(InsertKind::Insert);
                    return Ok(());
                }
                Key::Char('a') => {
                    self.enter_insert(InsertKind::Append);
                    return Ok(());
                }
                Key::Char('I') => {
                    self.enter_insert(InsertKind::InsertBol);
                    return Ok(());
                }
                Key::Char('A') => {
                    self.enter_insert(InsertKind::AppendEol);
                    return Ok(());
                }
                Key::Char('o') => {
                    self.enter_insert(InsertKind::OpenBelow);
                    return Ok(());
                }
                Key::Char('O') => {
                    self.enter_insert(InsertKind::OpenAbove);
                    return Ok(());
                }
                Key::Char('R') => {
                    self.mode = Mode::Replace;
                    self.insert_state = Some(InsertState::new(
                        InsertKind::Insert,
                        self.buffer.cursor(),
                        1,
                    ));
                    return Ok(());
                }
                Key::Char('s') => {
                    // Substitute char - delete char and enter insert
                    self.buffer.delete_char();
                    self.enter_insert(InsertKind::Insert);
                    return Ok(());
                }
                Key::Char('S') => {
                    // Substitute line - clear line and enter insert
                    self.buffer.clear_line();
                    self.enter_insert(InsertKind::InsertBol);
                    return Ok(());
                }
                Key::Char('C') => {
                    // Change to end of line
                    self.buffer.delete_to_end_of_line();
                    self.enter_insert(InsertKind::Append);
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

    /// Enter insert mode.
    fn enter_insert(&mut self, kind: InsertKind) {
        if let Ok(state) = enter_insert_mode(&mut self.buffer, kind) {
            self.mode = Mode::Insert(kind);
            self.insert_state = Some(state);
        }
    }

    /// Handle a key in insert mode.
    fn handle_insert_key(&mut self, key: Key) -> Result<()> {
        if let Some(mut state) = self.insert_state.take() {
            let should_exit = process_insert_key(&mut self.buffer, key, &mut state)?;

            if should_exit {
                // Exited insert mode
                self.mode = Mode::Command;
                // Record for undo
                if !state.inserted_text.is_empty() {
                    self.undo
                        .record_insert(state.start_pos, &state.inserted_text, false);
                }
                // Save for . (dot) repeat
                self.last_command = Some(LastCommand::Insert {
                    kind: state.kind,
                    text: state.inserted_text.clone(),
                    count: state.count,
                });
                // Handle repeat count
                if state.count > 1 {
                    let text = state.inserted_text.clone();
                    for _ in 1..state.count {
                        self.buffer.insert_str(&text);
                    }
                }
            } else {
                // Stay in insert mode
                self.insert_state = Some(state);
            }
        }

        Ok(())
    }

    /// Handle a key in ex mode.
    fn handle_ex_key(&mut self, key: Key) -> Result<()> {
        // Check if we're in ex text input mode (:a, :i, :c)
        if self.ex_insert_mode.is_some() {
            return self.handle_ex_insert_key(key);
        }

        match key {
            Key::Enter => {
                let input = std::mem::take(&mut self.ex_input);
                self.mode = Mode::Command;
                self.execute_ex_input(&input)?;
            }
            Key::Escape => {
                self.ex_input.clear();
                self.mode = Mode::Command;
            }
            Key::Backspace | Key::Delete => {
                if self.ex_input.is_empty() {
                    self.mode = Mode::Command;
                } else {
                    self.ex_input.pop();
                }
            }
            Key::Char(c) => {
                self.ex_input.push(c);
            }
            _ => {}
        }
        Ok(())
    }

    /// Handle a key in ex text input mode (:a, :i, :c).
    fn handle_ex_insert_key(&mut self, key: Key) -> Result<()> {
        match key {
            Key::Enter => {
                let line = std::mem::take(&mut self.ex_input);
                // Check for terminator (line containing only .)
                if line == "." {
                    self.finalize_ex_insert()?;
                } else {
                    self.ex_insert_text.push(line);
                }
            }
            Key::Escape => {
                // Cancel ex insert mode
                self.ex_insert_mode = None;
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
                    let insert_at = if start > 1 { start - 1 } else { 0 };
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
        use crate::command::motion;

        let count = cmd.count;

        match cmd.command {
            // Motion commands
            'h' => {
                if let Ok(res) = motion::move_left(&self.buffer, count) {
                    self.buffer.set_cursor(res.position);
                }
            }
            'l' => {
                if let Ok(res) = motion::move_right(&self.buffer, count) {
                    self.buffer.set_cursor(res.position);
                }
            }
            'j' => {
                if let Ok(res) = motion::move_down(&self.buffer, count) {
                    self.buffer.set_cursor(res.position);
                }
            }
            'k' => {
                if let Ok(res) = motion::move_up(&self.buffer, count) {
                    self.buffer.set_cursor(res.position);
                }
            }
            'w' => {
                if let Ok(res) = motion::move_word_forward(&self.buffer, count) {
                    self.buffer.set_cursor(res.position);
                }
            }
            'b' => {
                if let Ok(res) = motion::move_word_backward(&self.buffer, count) {
                    self.buffer.set_cursor(res.position);
                }
            }
            'e' => {
                if let Ok(res) = motion::move_word_end(&self.buffer, count) {
                    self.buffer.set_cursor(res.position);
                }
            }
            '0' => {
                if let Ok(res) = motion::move_to_line_start(&self.buffer) {
                    self.buffer.set_cursor(res.position);
                }
            }
            '^' => {
                if let Ok(res) = motion::move_to_first_non_blank(&self.buffer) {
                    self.buffer.set_cursor(res.position);
                }
            }
            '$' => {
                if let Ok(res) = motion::move_to_line_end(&self.buffer, count) {
                    self.buffer.set_cursor(res.position);
                }
            }
            'G' => {
                let line = if cmd.has_count { Some(count) } else { None };
                if let Ok(res) = motion::move_to_line(&self.buffer, line) {
                    self.buffer.set_cursor(res.position);
                }
            }
            // Screen position commands
            'H' => {
                // Move to top of screen
                let line = self.screen.top_line() + count - 1;
                let target = line.min(self.buffer.line_count());
                self.buffer.set_line(target);
                self.buffer.move_to_first_non_blank();
            }
            'M' => {
                // Move to middle of screen
                let size = self.terminal.size();
                let top = self.screen.top_line();
                let middle = top + (size.rows as usize / 2);
                let target = middle.min(self.buffer.line_count());
                self.buffer.set_line(target);
                self.buffer.move_to_first_non_blank();
            }
            'L' => {
                // Move to bottom of screen
                let size = self.terminal.size();
                let top = self.screen.top_line();
                let bottom = top + size.rows as usize - 2 - count + 1; // -2 for status line
                let target = bottom.min(self.buffer.line_count());
                self.buffer.set_line(target);
                self.buffer.move_to_first_non_blank();
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
            '%' => {
                if let Ok(res) = motion::find_matching_bracket(&self.buffer) {
                    self.buffer.set_cursor(res.position);
                }
            }
            'f' => {
                if let Some(c) = cmd.char_arg {
                    if let Ok(res) = motion::find_char_forward(&self.buffer, c, count) {
                        self.buffer.set_cursor(res.position);
                        self.last_find = Some(FindCommand::new('f', c));
                    }
                }
            }
            'F' => {
                if let Some(c) = cmd.char_arg {
                    if let Ok(res) = motion::find_char_backward(&self.buffer, c, count) {
                        self.buffer.set_cursor(res.position);
                        self.last_find = Some(FindCommand::new('F', c));
                    }
                }
            }
            't' => {
                if let Some(c) = cmd.char_arg {
                    if let Ok(res) = motion::till_char_forward(&self.buffer, c, count) {
                        self.buffer.set_cursor(res.position);
                        self.last_find = Some(FindCommand::new('t', c));
                    }
                }
            }
            'T' => {
                if let Some(c) = cmd.char_arg {
                    if let Ok(res) = motion::till_char_backward(&self.buffer, c, count) {
                        self.buffer.set_cursor(res.position);
                        self.last_find = Some(FindCommand::new('T', c));
                    }
                }
            }
            // Delete commands
            'x' => {
                let pos = self.buffer.cursor();
                let mut deleted = String::new();
                for _ in 0..count {
                    if let Some(c) = self.buffer.delete_char() {
                        deleted.push(c);
                    }
                }
                if !deleted.is_empty() {
                    self.undo.record_delete(pos, &deleted, false);
                    self.registers
                        .set_small_delete(RegisterContent::new(deleted, false));
                }
            }
            'X' => {
                let mut deleted = String::new();
                let mut start_pos = self.buffer.cursor();
                for _ in 0..count {
                    if let Some(c) = self.buffer.delete_char_before() {
                        deleted.push(c);
                    }
                }
                if !deleted.is_empty() {
                    // Reverse the deleted string since we accumulated in reverse order
                    let del_text: String = deleted.chars().rev().collect();
                    // Calculate the starting position for undo
                    start_pos.column = start_pos.column.saturating_sub(del_text.len());
                    self.undo.record_delete(start_pos, &del_text, false);
                    self.registers
                        .set_small_delete(RegisterContent::new(del_text, false));
                }
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
            // Put commands
            'p' => {
                self.execute_put(false, count)?;
            }
            'P' => {
                self.execute_put(true, count)?;
            }
            // Undo
            'u' => match self.undo.undo(&mut self.buffer) {
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
                for _ in 0..count {
                    let line = self.buffer.cursor().line;
                    if line < self.buffer.line_count() {
                        let _ = self.buffer.join_lines(line, true);
                    }
                }
            }
            // Search
            'n' => {
                let _ = self.search_next(self.search.direction());
            }
            'N' => {
                let _ = self.search_next(self.search.direction().opposite());
            }
            // Mark
            'm' => {
                if let Some(name) = cmd.char_arg {
                    if name.is_ascii_lowercase() {
                        let idx = (name as u8 - b'a') as usize;
                        self.marks[idx] = Some(self.buffer.cursor());
                    }
                }
            }
            '\'' | '`' => {
                if let Some(name) = cmd.char_arg {
                    let _ = self.goto_mark(name, cmd.command == '\'');
                }
            }
            // ZZ - write and quit
            'Z' => {
                if cmd.char_arg == Some('Z') {
                    self.write_and_quit()?;
                }
            }
            // Section motions
            '[' => {
                if cmd.char_arg == Some('[') {
                    if let Ok(res) = motion::move_section_backward(&self.buffer, count) {
                        self.buffer.set_cursor(res.position);
                    }
                }
            }
            ']' => {
                if cmd.char_arg == Some(']') {
                    if let Ok(res) = motion::move_section_forward(&self.buffer, count) {
                        self.buffer.set_cursor(res.position);
                    }
                }
            }
            // Replace char
            'r' => {
                if let Some(c) = cmd.char_arg {
                    for _ in 0..count {
                        self.replace_char(c);
                    }
                }
            }
            // Toggle case
            '~' => {
                for _ in 0..count {
                    self.toggle_case();
                }
            }
            // Delete to end of line
            'D' => {
                let deleted = self.buffer.delete_to_end_of_line();
                if !deleted.is_empty() {
                    self.registers
                        .set_small_delete(RegisterContent::new(deleted, false));
                }
            }
            // Shift left/right operators
            '>' | '<' => {
                self.execute_shift(cmd)?;
            }
            // Filter through shell (! operator)
            '!' => {
                self.execute_filter_operator(cmd)?;
            }
            // Repeat last f/F/t/T command
            ';' => {
                if let Some(find) = self.last_find {
                    let _ = self.execute_find(find.command, find.target, count);
                }
            }
            // Reverse last f/F/t/T command
            ',' => {
                if let Some(find) = self.last_find {
                    let _ = self.execute_find(find.reverse(), find.target, count);
                }
            }
            // Paragraph motions
            '{' => {
                if let Ok(res) = motion::move_paragraph_backward(&self.buffer, count) {
                    self.buffer.set_cursor(res.position);
                }
            }
            '}' => {
                if let Ok(res) = motion::move_paragraph_forward(&self.buffer, count) {
                    self.buffer.set_cursor(res.position);
                }
            }
            // Go to column
            '|' => {
                if let Ok(res) = motion::move_to_column(&self.buffer, count) {
                    self.buffer.set_cursor(res.position);
                }
            }
            // Yank line (alias for yy)
            'Y' => {
                self.execute_yank_lines(count)?;
            }
            // Repeat substitution
            '&' => {
                self.repeat_substitution()?;
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
    fn save_last_command(&mut self, cmd: &crate::command::ParsedCommand) {
        if Self::is_repeatable_command(cmd) {
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
                    // Repeat insert: enter insert mode, insert saved text, exit
                    let repeat_count = if count > 1 { count } else { orig_count };
                    self.enter_insert(kind);
                    // Insert the text
                    for _ in 0..repeat_count {
                        self.buffer.insert_str(&text);
                    }
                    // Exit insert mode and record for undo
                    if let Some(state) = self.insert_state.take() {
                        self.mode = Mode::Command;
                        if !text.is_empty() {
                            self.undo.record_insert(
                                state.start_pos,
                                &text.repeat(repeat_count),
                                false,
                            );
                        }
                    }
                    // Clamp cursor to valid position
                    let pos = self.buffer.cursor();
                    if pos.column > 0 {
                        if let Some(line) = self.buffer.line(pos.line) {
                            if !line.is_empty() && pos.column >= line.len() {
                                self.buffer.set_column(line.last_char_offset());
                            }
                        }
                    }
                }
                LastCommand::Parsed(mut cmd) => {
                    // Override count if specified
                    if count > 1 {
                        cmd.count = count;
                        if let Some(ref mut mot) = cmd.motion {
                            mot.count = 1; // Reset motion count when using new count
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
        let content = match self.registers.get(register) {
            Some(c) => c.text.clone(),
            None => {
                self.set_error(&format!("Register {} is empty", register));
                return Ok(());
            }
        };

        // Save as last macro register
        self.last_macro_register = Some(register);

        // Execute the macro `count` times
        for _ in 0..count {
            // Parse and execute each character as a command
            self.execute_keys_from_string(&content)?;
        }

        Ok(())
    }

    /// Execute a string of characters as if typed by the user.
    fn execute_keys_from_string(&mut self, keys: &str) -> Result<()> {
        // Reset parser state before executing keys to avoid interference
        // from any previously parsed command (e.g., @a calling this function)
        self.parser.reset();

        for c in keys.chars() {
            let key = if c == '\x1b' {
                Key::Escape
            } else if c == '\n' || c == '\r' {
                Key::Enter
            } else if c.is_ascii_control() {
                // Convert control characters
                let ctrl_char = (c as u8 + b'@') as char;
                Key::Ctrl(ctrl_char.to_ascii_lowercase())
            } else {
                Key::Char(c)
            };

            // Process the key based on current mode
            match self.mode {
                Mode::Command => self.handle_command_key(key)?,
                Mode::Insert(_) | Mode::Replace => self.handle_insert_key(key)?,
                Mode::Ex => self.handle_ex_key(key)?,
                Mode::Open => {}
            }
        }
        Ok(())
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
                self.buffer.set_cursor(result.cursor);
            } else {
                // d + motion
                let start = self.buffer.cursor();
                if let Some(end) = self.execute_motion_get_pos(mot) {
                    let range = Range::new(start, end, BufferMode::Character);
                    let result =
                        delete(&mut self.buffer, range, &mut self.registers, cmd.register)?;
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
                if let Some(end) = self.execute_motion_get_pos(mot) {
                    let range = Range::new(start, end, BufferMode::Character);
                    let _ = yank(&self.buffer, range, &mut self.registers, cmd.register);
                }
            }
        }
        Ok(())
    }

    /// Execute change command.
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
                let result = change(&mut self.buffer, range, &mut self.registers, cmd.register)?;
                self.buffer.set_cursor(result.cursor);
                if result.enter_insert {
                    self.enter_insert(InsertKind::Change);
                }
            } else {
                // c + motion
                let start = self.buffer.cursor();
                // Special case: cw and cW should behave like ce and cE
                // (change to end of word, not beginning of next word)
                // This is standard POSIX vi behavior.
                let (end, needs_inclusive) = if mot.motion == 'w' {
                    (
                        motion::move_word_end(&self.buffer, mot.count)
                            .ok()
                            .map(|r| r.position),
                        true,
                    )
                } else if mot.motion == 'W' {
                    (
                        motion::move_bigword_end(&self.buffer, mot.count)
                            .ok()
                            .map(|r| r.position),
                        true,
                    )
                } else {
                    (self.execute_motion_get_pos(mot), false)
                };
                if let Some(mut end) = end {
                    // For inclusive motions like 'e', we need to add 1 to include
                    // the character at the end position (since Range is exclusive on end)
                    if needs_inclusive {
                        end.column += 1;
                    }
                    let range = Range::new(start, end, BufferMode::Character);
                    let result =
                        change(&mut self.buffer, range, &mut self.registers, cmd.register)?;
                    // Use set_column_for_insert to allow cursor at end of line
                    // The change operation's cursor may be clamped, so use range.start
                    // which is where we want to insert (the start of the deleted text)
                    self.buffer.set_line(range.start.line);
                    self.buffer.set_column_for_insert(range.start.column);
                    if result.enter_insert {
                        self.enter_insert(InsertKind::Change);
                    }
                }
            }
        }
        Ok(())
    }

    /// Execute shift command (> or <).
    fn execute_shift(&mut self, cmd: &crate::command::ParsedCommand) -> Result<()> {
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
                    let result = shift_right(&mut self.buffer, range, self.options.shiftwidth)?;
                    self.buffer.set_cursor(result.cursor);
                } else {
                    let result = shift_left(&mut self.buffer, range, self.options.shiftwidth)?;
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
                        let result = shift_right(&mut self.buffer, range, self.options.shiftwidth)?;
                        self.buffer.set_cursor(result.cursor);
                    } else {
                        let result = shift_left(&mut self.buffer, range, self.options.shiftwidth)?;
                        self.buffer.set_cursor(result.cursor);
                    }
                }
            }
        }
        Ok(())
    }

    /// Execute a motion and return the resulting position.
    fn execute_motion_get_pos(&self, mot: &crate::command::MotionCommand) -> Option<Position> {
        use crate::command::motion;

        let result = match mot.motion {
            'h' => motion::move_left(&self.buffer, mot.count).ok(),
            'l' => motion::move_right(&self.buffer, mot.count).ok(),
            'j' => motion::move_down(&self.buffer, mot.count).ok(),
            'k' => motion::move_up(&self.buffer, mot.count).ok(),
            'w' => motion::move_word_forward(&self.buffer, mot.count).ok(),
            'W' => motion::move_bigword_forward(&self.buffer, mot.count).ok(),
            'b' => motion::move_word_backward(&self.buffer, mot.count).ok(),
            'B' => motion::move_bigword_backward(&self.buffer, mot.count).ok(),
            'e' => motion::move_word_end(&self.buffer, mot.count).ok(),
            'E' => motion::move_bigword_end(&self.buffer, mot.count).ok(),
            '0' => motion::move_to_line_start(&self.buffer).ok(),
            '^' => motion::move_to_first_non_blank(&self.buffer).ok(),
            '$' => motion::move_to_line_end(&self.buffer, mot.count).ok(),
            'G' => motion::move_to_line(&self.buffer, Some(self.buffer.line_count())).ok(),
            'f' => mot
                .char_arg
                .and_then(|c| motion::find_char_forward(&self.buffer, c, mot.count).ok()),
            'F' => mot
                .char_arg
                .and_then(|c| motion::find_char_backward(&self.buffer, c, mot.count).ok()),
            't' => mot
                .char_arg
                .and_then(|c| motion::till_char_forward(&self.buffer, c, mot.count).ok()),
            'T' => mot
                .char_arg
                .and_then(|c| motion::till_char_backward(&self.buffer, c, mot.count).ok()),
            '{' => motion::move_paragraph_backward(&self.buffer, mot.count).ok(),
            '}' => motion::move_paragraph_forward(&self.buffer, mot.count).ok(),
            '|' => motion::move_to_column(&self.buffer, mot.count).ok(),
            '%' => motion::find_matching_bracket(&self.buffer).ok(),
            _ => None,
        };
        result.map(|r| r.position)
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
            self.ex_input = "!".to_string();
        }
        Ok(())
    }

    /// Execute put command.
    fn execute_put(&mut self, before: bool, count: usize) -> Result<()> {
        use crate::command::{put_after, put_before};

        let result = if before {
            put_before(&mut self.buffer, &self.registers, None, count)?
        } else {
            put_after(&mut self.buffer, &self.registers, None, count)?
        };

        self.buffer.set_cursor(result.cursor);
        Ok(())
    }

    /// Execute an ex command input string.
    fn execute_ex_input(&mut self, input: &str) -> Result<()> {
        // Handle search patterns
        if let Some(pattern) = input.strip_prefix('/') {
            self.search.update_options(&self.options);
            if !pattern.is_empty() {
                self.search.set_pattern(pattern, SearchDirection::Forward)?;
                self.registers.set_search(pattern);
            }
            return self.search_next(SearchDirection::Forward);
        }

        if let Some(pattern) = input.strip_prefix('?') {
            self.search.update_options(&self.options);
            if !pattern.is_empty() {
                self.search
                    .set_pattern(pattern, SearchDirection::Backward)?;
                self.registers.set_search(pattern);
            }
            return self.search_next(SearchDirection::Backward);
        }

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

        // Parse and execute ex command
        let cmd = parse_ex_command(&actual_input)?;
        let result = self.execute_ex(cmd)?;

        match result {
            ExResult::Continue => {}
            ExResult::Quit(code) => {
                self.should_quit = true;
                self.exit_code = code;
            }
            ExResult::Edit(path) => {
                self.open(&path)?;
            }
            ExResult::Message(msg) => {
                self.set_message(&msg);
            }
            ExResult::Error(msg) => {
                self.set_error(&msg);
            }
            ExResult::Output(lines) => {
                if self.ex_standalone_mode {
                    // In ex mode, print output directly to stdout
                    // Note: Output from :p, :nu, :l is NOT suppressed by -s
                    // (POSIX says -s suppresses "informational messages", not command output)
                    for line in &lines {
                        println!("{}", line);
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
                self.quit(force)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Write {
                range,
                file,
                append,
                force,
            } => {
                self.write(file.as_deref(), range.explicit, append, force)?;
                Ok(ExResult::Continue)
            }
            ExCommand::WriteQuit { range, file, force } => {
                self.write(file.as_deref(), range.explicit, false, force)?;
                self.quit(true)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Edit { file, force } => {
                if !force && self.buffer.is_modified() {
                    return Err(ViError::FileModified);
                }
                if let Some(path) = file {
                    self.open(&path)?;
                } else if let Some(path) = self.files.current_file().map(|p| p.to_path_buf()) {
                    self.open(&path.to_string_lossy())?;
                }
                Ok(ExResult::Continue)
            }
            ExCommand::Set { args } => {
                if let Some(msg) = self.options.set(&args)? {
                    Ok(ExResult::Message(msg))
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
                Ok(ExResult::Continue)
            }
            ExCommand::Goto { line } => {
                let line = line.min(self.buffer.line_count()).max(1);
                self.buffer.set_line(line);
                self.buffer.move_to_first_non_blank();
                Ok(ExResult::Continue)
            }
            ExCommand::File { new_name } => {
                if let Some(name) = new_name {
                    self.files.set_current_file(Some(PathBuf::from(name)));
                }
                let info = self.file_info();
                Ok(ExResult::Message(info))
            }
            ExCommand::Undo => {
                self.undo.undo(&mut self.buffer)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Redo => {
                self.undo.redo(&mut self.buffer)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Next { force } => {
                if !force && self.buffer.is_modified() {
                    return Err(ViError::FileModified);
                }
                let path = self.files.next_file()?;
                self.open(&path.to_string_lossy())?;
                Ok(ExResult::Continue)
            }
            ExCommand::Previous { force } => {
                if !force && self.buffer.is_modified() {
                    return Err(ViError::FileModified);
                }
                let path = self.files.prev_file()?;
                self.open(&path.to_string_lossy())?;
                Ok(ExResult::Continue)
            }
            ExCommand::Rewind { force } => {
                if !force && self.buffer.is_modified() {
                    return Err(ViError::FileModified);
                }
                let path = self.files.rewind()?;
                self.open(&path.to_string_lossy())?;
                Ok(ExResult::Continue)
            }
            ExCommand::Args => Ok(ExResult::Message(self.files.format_args())),
            ExCommand::Version => Ok(ExResult::Message(format!(
                "{} {}",
                env!("CARGO_PKG_NAME"),
                env!("CARGO_PKG_VERSION")
            ))),
            ExCommand::Help => Ok(ExResult::Message(format!(
                "{}: {}",
                env!("CARGO_PKG_NAME"),
                env!("CARGO_PKG_DESCRIPTION")
            ))),
            ExCommand::Shell { command } => {
                self.execute_shell_command(&command)?;
                Ok(ExResult::Continue)
            }
            ExCommand::ShellRead { line, command } => {
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
                self.execute_source(&file)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Append { line } => {
                self.ex_insert_mode = Some(ExInsertMode::Append(line));
                Ok(ExResult::Continue)
            }
            ExCommand::Insert { line } => {
                self.ex_insert_mode = Some(ExInsertMode::Insert(line));
                Ok(ExResult::Continue)
            }
            ExCommand::Change { range } => {
                let current = self.buffer.cursor().line;
                let resolved = range.resolve(&self.buffer, current)?;
                self.ex_insert_mode = Some(ExInsertMode::Change {
                    start: resolved.0,
                    end: resolved.1,
                });
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
            ExCommand::Print { range, count } => {
                let lines = self.get_lines_for_output(&range, count)?;
                Ok(ExResult::Output(lines))
            }
            ExCommand::Number { range, count } => {
                let lines = self.get_lines_with_numbers(&range, count)?;
                Ok(ExResult::Output(lines))
            }
            ExCommand::List { range, count } => {
                let lines = self.get_lines_for_list(&range, count)?;
                Ok(ExResult::Output(lines))
            }
            ExCommand::Join { range, count } => {
                self.execute_ex_join(&range, count)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Put { line, register } => {
                self.execute_ex_put(line, register)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Copy { range, dest } => {
                self.execute_ex_copy(&range, dest)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Move { range, dest } => {
                self.execute_ex_move(&range, dest)?;
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
            ExCommand::Cd { path } => {
                self.execute_ex_cd(path.as_deref())?;
                Ok(ExResult::Continue)
            }
            ExCommand::Pwd => {
                let cwd = std::env::current_dir()
                    .map(|p| p.display().to_string())
                    .unwrap_or_else(|_| "unknown".to_string());
                Ok(ExResult::Message(cwd))
            }
            ExCommand::Mark { line, name } => {
                self.execute_ex_mark(line, name)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Visual => Ok(ExResult::EnterVisual),
            ExCommand::Open { line } => Ok(ExResult::EnterOpen(line)),
            ExCommand::Z { line, ztype, count } => {
                let output = self.execute_ex_z(line, ztype, count)?;
                Ok(ExResult::Output(output))
            }
            ExCommand::ShiftLeft { range, count } => {
                self.execute_ex_shift_left(&range, count)?;
                Ok(ExResult::Continue)
            }
            ExCommand::ShiftRight { range, count } => {
                self.execute_ex_shift_right(&range, count)?;
                Ok(ExResult::Continue)
            }
            ExCommand::LineNumber { line } => {
                let line_num = self.execute_ex_line_number(line)?;
                Ok(ExResult::Output(vec![line_num.to_string()]))
            }
            ExCommand::Execute { range, buffer } => {
                self.execute_ex_execute(&range, buffer)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Suspend => {
                self.execute_ex_suspend()?;
                Ok(ExResult::Continue)
            }
            ExCommand::RepeatSubstitute { range, flags } => {
                self.execute_ex_repeat_substitute(&range, &flags)?;
                Ok(ExResult::Continue)
            }
            ExCommand::Nop => Ok(ExResult::Continue),
            _ => {
                // Other commands not yet implemented
                Ok(ExResult::Continue)
            }
        }
    }

    /// Execute :source command - read and execute ex commands from file.
    fn execute_source(&mut self, file: &str) -> Result<()> {
        use std::fs::File;
        use std::io::{BufRead, BufReader};

        let file = File::open(file).map_err(ViError::Io)?;
        let reader = BufReader::new(file);

        for line in reader.lines() {
            let line = line.map_err(ViError::Io)?;
            let line = line.trim();
            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('"') {
                continue;
            }
            // Execute the command
            self.execute_ex_input(line)?;
        }

        Ok(())
    }

    /// Execute a shell command (:! or :shell).
    fn execute_shell_command(&mut self, command: &str) -> Result<()> {
        // Temporarily restore terminal to cooked mode
        self.terminal.disable_raw_mode()?;

        let result = if command.is_empty() {
            // :shell - start interactive shell
            println!(); // Blank line before shell
            self.shell.interactive()
        } else {
            // :!command - execute command
            println!(); // Blank line before output
            self.shell.execute(command)
        };

        // Wait for user to press Enter before returning
        if result.is_ok() {
            use std::io::{self, Read, Write};
            print!("\nPress ENTER or type command to continue");
            io::stdout().flush()?;
            let mut buf = [0u8; 1];
            let _ = io::stdin().read(&mut buf);
        }

        // Re-enable raw mode
        self.terminal.enable_raw_mode()?;

        // Force full screen redraw
        self.terminal.clear_screen()?;

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
        use crate::buffer::Line;

        // Capture command output
        let output = self.shell.execute_capture(command)?;

        if !output.success {
            self.set_error(&format!("shell returned {}", output.exit_code));
            return Ok(());
        }

        // Insert output after specified line (or current line)
        let insert_line = line.unwrap_or_else(|| self.buffer.cursor().line);
        let lines = output.stdout_lines();
        let line_count = lines.len();

        // Record for undo - record as insert of all the text
        let text = output.stdout_string();
        self.undo.record_insert(
            Position::new(insert_line + 1, 0),
            &text,
            true, // linewise
        );

        for (i, content) in lines.into_iter().enumerate() {
            self.buffer
                .insert_line_after(insert_line + i, Line::from(content.as_str()));
        }

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
        use crate::buffer::Line;

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

        // Record for undo - this is a replace operation
        let new_text = output.stdout_string();
        self.undo.record_replace(
            Position::new(start, 0),
            &input,
            &new_text,
            true, // linewise
        );

        // Delete original lines
        for _ in start..=end {
            self.buffer.delete_line(start);
        }

        // Insert filtered output
        let lines = output.stdout_lines();

        // Insert new lines (insert_line_after takes 0-based index for "before first line")
        let insert_pos = if start > 1 { start - 1 } else { 0 };
        for (i, content) in lines.into_iter().enumerate() {
            self.buffer
                .insert_line_after(insert_pos + i, Line::from(content.as_str()));
        }

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

        // Temporarily restore terminal
        self.terminal.disable_raw_mode()?;
        println!(); // Blank line before output

        let result = self.shell.write_to(command, &text);

        // Wait for user to press Enter
        if result.is_ok() {
            use std::io::{self, Read, Write};
            print!("\nPress ENTER or type command to continue");
            io::stdout().flush()?;
            let mut buf = [0u8; 1];
            let _ = io::stdin().read(&mut buf);
        }

        // Re-enable raw mode
        self.terminal.enable_raw_mode()?;
        self.terminal.clear_screen()?;

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

    /// Get lines for :p (print) command output.
    fn get_lines_for_output(
        &self,
        range: &AddressRange,
        count: Option<usize>,
    ) -> Result<Vec<String>> {
        let current = self.buffer.cursor().line;
        let (start, end) = if range.explicit {
            range.resolve(&self.buffer, current)?
        } else if let Some(c) = count {
            // If count given without range, start at current line
            (current, (current + c - 1).min(self.buffer.line_count()))
        } else {
            // No range, no count: print current line
            (current, current)
        };

        let mut lines = Vec::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                lines.push(line.content().to_string());
            }
        }
        Ok(lines)
    }

    /// Get lines with line numbers for :nu (number) command output.
    fn get_lines_with_numbers(
        &self,
        range: &AddressRange,
        count: Option<usize>,
    ) -> Result<Vec<String>> {
        let current = self.buffer.cursor().line;
        let (start, end) = if range.explicit {
            range.resolve(&self.buffer, current)?
        } else if let Some(c) = count {
            (current, (current + c - 1).min(self.buffer.line_count()))
        } else {
            (current, current)
        };

        let mut lines = Vec::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                lines.push(format!("{:6}\t{}", line_num, line.content()));
            }
        }
        Ok(lines)
    }

    /// Get lines in list format for :l (list) command output.
    /// This shows non-printable characters.
    fn get_lines_for_list(
        &self,
        range: &AddressRange,
        count: Option<usize>,
    ) -> Result<Vec<String>> {
        let current = self.buffer.cursor().line;
        let (start, end) = if range.explicit {
            range.resolve(&self.buffer, current)?
        } else if let Some(c) = count {
            (current, (current + c - 1).min(self.buffer.line_count()))
        } else {
            (current, current)
        };

        let mut lines = Vec::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                // Convert non-printable characters to visible form
                let mut listed = String::new();
                for ch in line.content().chars() {
                    match ch {
                        '\t' => listed.push_str("^I"),
                        c if c.is_ascii_control() => {
                            // Only convert ASCII control characters (0x00-0x1F)
                            // to ^@ through ^_ notation
                            listed.push('^');
                            listed.push((c as u8 + b'@') as char);
                        }
                        c => listed.push(c),
                    }
                }
                listed.push('$'); // End of line marker
                lines.push(listed);
            }
        }
        Ok(lines)
    }

    /// Execute :join command - join lines together.
    fn execute_ex_join(&mut self, range: &AddressRange, count: Option<usize>) -> Result<()> {
        let current = self.buffer.cursor().line;

        // Determine the range of lines to join
        let (start, mut end) = if range.explicit {
            range.resolve(&self.buffer, current)?
        } else if let Some(c) = count {
            // No address: current line and current + count
            (current, (current + c).min(self.buffer.line_count()))
        } else {
            // No address, no count: current line and next line
            (current, (current + 1).min(self.buffer.line_count()))
        };

        // Apply count to extend the range if both range and count are specified
        if range.explicit && count.is_some() {
            end = (end + count.unwrap() - 1).min(self.buffer.line_count());
        }

        if start >= end || start > self.buffer.line_count() {
            return Ok(()); // Nothing to join
        }

        // Build the joined line
        let mut result = String::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                let content = line.content();
                if result.is_empty() {
                    result = content.to_string();
                } else {
                    // Add space and trimmed content (POSIX: discard leading spaces)
                    let trimmed = content.trim_start();
                    if !trimmed.is_empty() {
                        if !result.is_empty() && !result.ends_with(' ') {
                            result.push(' ');
                        }
                        result.push_str(trimmed);
                    }
                }
            }
        }

        // Delete lines from end to start+1 (in reverse to preserve line numbers)
        for line_num in (start + 1..=end).rev() {
            self.buffer.delete_line(line_num);
        }

        // Replace the first line with the joined result
        if let Some(line) = self.buffer.line_mut(start) {
            *line = Line::from(result.as_str());
        }

        self.buffer.set_line(start);

        let join_count = end - start;
        self.set_message(&format!("{} lines joined", join_count + 1));
        Ok(())
    }

    /// Execute :put command - put text from register after line.
    fn execute_ex_put(&mut self, line: Option<usize>, register: Option<char>) -> Result<()> {
        let target_line = line.unwrap_or_else(|| self.buffer.cursor().line);
        let reg = register.unwrap_or('"');

        let content = self.registers.get(reg).ok_or(ViError::BufferEmpty(reg))?;

        // Insert lines after target
        let lines: Vec<&str> = content.text.lines().collect();
        for (i, line_text) in lines.iter().enumerate() {
            self.buffer
                .insert_line_after(target_line + i, Line::from(*line_text));
        }

        self.buffer.set_line(target_line + lines.len());
        Ok(())
    }

    /// Execute :copy command - copy lines to destination.
    fn execute_ex_copy(&mut self, range: &AddressRange, dest: usize) -> Result<()> {
        let current = self.buffer.cursor().line;
        let (start, end) = range.resolve(&self.buffer, current)?;

        // Collect the lines to copy
        let mut lines_to_copy = Vec::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                lines_to_copy.push(line.content().to_string());
            }
        }

        // Insert after destination line
        let insert_after = if dest == 0 { 0 } else { dest };
        for (i, line_text) in lines_to_copy.iter().enumerate() {
            self.buffer
                .insert_line_after(insert_after + i, Line::from(line_text.as_str()));
        }

        self.buffer.set_line(insert_after + lines_to_copy.len());

        let copy_count = end - start + 1;
        self.set_message(&format!("{} lines copied", copy_count));
        Ok(())
    }

    /// Execute :move command - move lines to destination.
    fn execute_ex_move(&mut self, range: &AddressRange, dest: usize) -> Result<()> {
        let current = self.buffer.cursor().line;
        let (start, end) = range.resolve(&self.buffer, current)?;

        // Can't move lines into themselves
        if dest >= start && dest <= end {
            return Err(ViError::InvalidRange(
                "Cannot move lines into themselves".to_string(),
            ));
        }

        // Collect the lines to move
        let mut lines_to_move = Vec::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                lines_to_move.push(line.content().to_string());
            }
        }

        // Delete original lines (in reverse order)
        for line_num in (start..=end).rev() {
            self.buffer.delete_line(line_num);
        }

        // Adjust destination if it was after the deleted lines
        let adjusted_dest = if dest > end {
            dest - (end - start + 1)
        } else {
            dest
        };

        // Insert at new location
        let insert_after = if adjusted_dest == 0 { 0 } else { adjusted_dest };
        for (i, line_text) in lines_to_move.iter().enumerate() {
            self.buffer
                .insert_line_after(insert_after + i, Line::from(line_text.as_str()));
        }

        self.buffer.set_line(insert_after + lines_to_move.len());

        let move_count = end - start + 1;
        self.set_message(&format!("{} lines moved", move_count));
        Ok(())
    }

    /// Execute :read command - read file into buffer.
    fn execute_ex_read(&mut self, range: &AddressRange, file: Option<&str>) -> Result<()> {
        let current = self.buffer.cursor().line;
        let insert_after = if range.explicit {
            let (_, end) = range.resolve(&self.buffer, current)?;
            end
        } else {
            current
        };

        let path = file
            .map(|s| s.to_string())
            .or_else(|| self.files.current_file().map(|p| p.display().to_string()))
            .ok_or(ViError::NoFileName)?;

        let content = std::fs::read_to_string(&path).map_err(ViError::Io)?;

        let lines: Vec<&str> = content.lines().collect();
        for (i, line_text) in lines.iter().enumerate() {
            self.buffer
                .insert_line_after(insert_after + i, Line::from(*line_text));
        }

        self.buffer.set_line(insert_after + lines.len());

        let bytes = content.len();
        self.set_message(&format!(
            "\"{}\" {} lines, {} bytes",
            path,
            lines.len(),
            bytes
        ));
        Ok(())
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
            range.resolve(&self.buffer, current)?
        } else {
            (1, self.buffer.line_count())
        };

        // Compile the pattern
        let regex =
            regex::Regex::new(pattern).map_err(|e| ViError::InvalidPattern(e.to_string()))?;

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

        // Execute command on each matching line
        // Process in reverse for delete commands to preserve line numbers
        let is_delete = command.trim().starts_with('d');
        if is_delete {
            matching_lines.reverse();
        }

        for line_num in matching_lines {
            // Move cursor to the line
            self.buffer.set_line(line_num);
            // Execute the command
            if let Err(e) = self.execute_ex_input(command) {
                self.set_error(&e.to_string());
            }
        }

        Ok(())
    }

    /// Execute :cd command - change directory.
    fn execute_ex_cd(&mut self, path: Option<&str>) -> Result<()> {
        let target = if let Some(p) = path {
            std::path::PathBuf::from(p)
        } else {
            // Try to get home directory from environment
            std::env::var("HOME")
                .map(std::path::PathBuf::from)
                .map_err(|_| ViError::ShellError("Cannot determine home directory".to_string()))?
        };

        std::env::set_current_dir(&target).map_err(|e| {
            ViError::ShellError(format!("Cannot change to {}: {}", target.display(), e))
        })?;

        self.set_message(&format!("{}", target.display()));
        Ok(())
    }

    /// Execute :mark command - set a mark.
    fn execute_ex_mark(&mut self, line: Option<usize>, name: char) -> Result<()> {
        let target_line = line.unwrap_or_else(|| self.buffer.cursor().line);

        if !name.is_ascii_lowercase() {
            return Err(ViError::MarkNotSet(name));
        }

        let idx = (name as u8 - b'a') as usize;
        self.marks[idx] = Some(Position::new(target_line, 0));
        Ok(())
    }

    /// Execute :z command - adjust window display.
    fn execute_ex_z(
        &mut self,
        line: Option<usize>,
        ztype: Option<char>,
        count: Option<usize>,
    ) -> Result<Vec<String>> {
        let scroll = self.options.scroll;
        let count = count.unwrap_or(2 * scroll);
        let mut target_line = line.unwrap_or_else(|| self.buffer.cursor().line);

        // If no type and no line, advance to next line
        if line.is_none() && ztype.is_none() {
            target_line = (target_line + 1).min(self.buffer.line_count());
        }

        // Adjust target based on type
        let start_line = match ztype {
            Some('+') => target_line,
            Some('-') => target_line.saturating_sub(count - 1).max(1),
            Some('.') | Some('=') => {
                // Center on this line
                let half = count / 2;
                target_line.saturating_sub(half).max(1)
            }
            Some('^') => target_line.saturating_sub(2 * count - 1).max(1),
            None => target_line,
            _ => target_line,
        };

        // Collect lines to output
        let mut output = Vec::new();
        let end_line = (start_line + count - 1).min(self.buffer.line_count());

        if let Some('=') = ztype {
            // For '=', print separator line around current line
            let cols = self.terminal.size().cols as usize;
            let separator: String = "-".repeat(40.min(cols / 2));
            let half = count / 2;
            let before_start = target_line.saturating_sub(half).max(1);

            for i in before_start..target_line {
                if let Some(line) = self.buffer.line(i) {
                    output.push(line.content().to_string());
                }
            }
            output.push(separator.clone());
            if let Some(line) = self.buffer.line(target_line) {
                output.push(line.content().to_string());
            }
            output.push(separator);
            for i in (target_line + 1)..=(target_line + half).min(self.buffer.line_count()) {
                if let Some(line) = self.buffer.line(i) {
                    output.push(line.content().to_string());
                }
            }
        } else {
            for i in start_line..=end_line {
                if let Some(line) = self.buffer.line(i) {
                    output.push(line.content().to_string());
                }
            }
        }

        // Update current line
        let new_current = if ztype == Some('=') {
            target_line
        } else {
            end_line
        };
        self.buffer.set_line(new_current);
        self.buffer.move_to_first_non_blank();

        Ok(output)
    }

    /// Execute :< command - shift lines left.
    fn execute_ex_shift_left(&mut self, range: &AddressRange, count: Option<usize>) -> Result<()> {
        let (start, end) = self.resolve_range(range)?;
        let shift_amount = count.unwrap_or(1) * self.options.shiftwidth;

        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                let content = line.content().to_string();
                // Count leading whitespace
                let leading: usize = content
                    .chars()
                    .take_while(|c| c.is_whitespace())
                    .map(|c| if c == '\t' { self.options.tabstop } else { 1 })
                    .sum();

                if leading > 0 {
                    let new_indent = leading.saturating_sub(shift_amount);
                    let trimmed = content.trim_start();
                    let new_content = format!("{}{}", " ".repeat(new_indent), trimmed);
                    let _ = self.buffer.replace_line(line_num, &new_content);
                }
            }
        }

        self.buffer.set_line(end);
        self.buffer.move_to_first_non_blank();
        Ok(())
    }

    /// Execute :> command - shift lines right.
    fn execute_ex_shift_right(&mut self, range: &AddressRange, count: Option<usize>) -> Result<()> {
        let (start, end) = self.resolve_range(range)?;
        let shift_amount = count.unwrap_or(1) * self.options.shiftwidth;

        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                let content = line.content().to_string();
                // Don't shift empty lines
                if !content.is_empty() {
                    let new_content = format!("{}{}", " ".repeat(shift_amount), content);
                    let _ = self.buffer.replace_line(line_num, &new_content);
                }
            }
        }

        self.buffer.set_line(end);
        self.buffer.move_to_first_non_blank();
        Ok(())
    }

    /// Execute := command - print line number.
    fn execute_ex_line_number(&self, line: Option<usize>) -> Result<usize> {
        // Default to last line in buffer
        let line_num = line.unwrap_or_else(|| self.buffer.line_count());
        Ok(line_num)
    }

    /// Execute :@ or :* command - execute buffer contents as ex commands.
    fn execute_ex_execute(&mut self, range: &AddressRange, buffer: Option<char>) -> Result<()> {
        // Get the buffer to execute
        let buffer_char = buffer.unwrap_or_else(|| {
            // Use last executed buffer, default to unnamed
            self.last_macro_register.unwrap_or('"')
        });

        // Get buffer contents
        let content = if buffer_char == '"' {
            // Unnamed buffer
            self.registers.get('"').map(|r| r.text.clone())
        } else if buffer_char.is_ascii_alphabetic() {
            self.registers.get(buffer_char).map(|r| r.text.clone())
        } else {
            None
        };

        let content = match content {
            Some(c) if !c.is_empty() => c,
            _ => return Err(ViError::BufferEmpty(buffer_char)),
        };

        // Remember this buffer for @@ / **
        self.last_macro_register = Some(buffer_char);

        // Execute for each line in the range (or just once if no explicit range)
        let (start, end) = if range.explicit {
            self.resolve_range(range)?
        } else {
            let current = self.buffer.cursor().line;
            (current, current)
        };

        for line_num in start..=end {
            self.buffer.set_line(line_num);
            // Execute each line of the buffer content as an ex command
            for cmd_line in content.lines() {
                let cmd_line = cmd_line.trim();
                if !cmd_line.is_empty() {
                    self.execute_ex_input(cmd_line)?;
                }
            }
        }

        Ok(())
    }

    /// Execute :suspend or :stop command - suspend the editor.
    fn execute_ex_suspend(&mut self) -> Result<()> {
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

    /// Execute :& command - repeat last substitute.
    fn execute_ex_repeat_substitute(
        &mut self,
        range: &AddressRange,
        flags: &SubstituteFlags,
    ) -> Result<()> {
        // Get last substitute pattern and replacement
        let (pattern, replacement) = match &self.last_substitution {
            Some((p, r, _)) => (p.clone(), r.clone()),
            None => return Err(ViError::NoPreviousSubstitution),
        };

        // Use provided flags or default to last flags
        self.substitute(range, &pattern, &replacement, flags)
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
        self.screen
            .scroll_to_line(pos.line, self.buffer.line_count());

        Ok(())
    }

    /// Go to a mark.
    fn goto_mark(&mut self, name: char, first_non_blank: bool) -> Result<()> {
        let pos = if name.is_ascii_lowercase() {
            let idx = (name as u8 - b'a') as usize;
            self.marks[idx].ok_or(ViError::MarkNotSet(name))?
        } else {
            return Err(ViError::MarkNotSet(name));
        };

        self.buffer.set_cursor(pos);
        if first_non_blank {
            self.buffer.move_to_first_non_blank();
        }

        Ok(())
    }

    /// Perform substitution.
    fn substitute(
        &mut self,
        range: &AddressRange,
        pattern: &str,
        replacement: &str,
        flags: &SubstituteFlags,
    ) -> Result<()> {
        let sub = Substitutor::new(
            pattern,
            replacement,
            flags.global,
            flags.confirm,
            flags.print,
            flags.count,
            self.options.ignorecase,
        )?;

        // Resolve range
        let (start, end) = self.resolve_range(range)?;
        let mut total_count = 0;

        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                let content = line.content().to_string();
                let (new_content, count) = sub.substitute_line(&content);

                if count > 0 && !sub.is_count_only() {
                    self.undo.record_replace(
                        Position::new(line_num, 0),
                        &content,
                        &new_content,
                        false,
                    );
                    self.buffer.replace_line(line_num, &new_content)?;
                }
                total_count += count;
            }
        }

        if total_count == 0 {
            self.set_error("Pattern not found");
        } else if sub.is_count_only() {
            self.set_message(&format!("{} matches", total_count));
        } else {
            self.set_message(&format!("{} substitutions", total_count));
            // Save for & command
            self.last_substitution =
                Some((pattern.to_string(), replacement.to_string(), flags.global));
        }

        Ok(())
    }

    /// Resolve address range to line numbers.
    fn resolve_range(&self, range: &AddressRange) -> Result<(usize, usize)> {
        range.resolve(&self.buffer, self.buffer.cursor().line)
    }

    /// Execute ex delete command (:d).
    fn execute_ex_delete(
        &mut self,
        range: &AddressRange,
        register: Option<char>,
        count: Option<usize>,
    ) -> Result<()> {
        use crate::command::delete;

        let (start, end) = self.resolve_range(range)?;
        let end = if let Some(c) = count {
            (start + c - 1).min(self.buffer.line_count())
        } else {
            end
        };

        let start_pos = Position::new(start, 0);
        let end_pos = Position::new(end, 0);
        let del_range = Range::lines(start_pos, end_pos);

        let result = delete(&mut self.buffer, del_range, &mut self.registers, register)?;
        self.buffer.set_cursor(result.cursor);

        let line_count = end - start + 1;
        if line_count > 1 {
            self.set_message(&format!("{} lines deleted", line_count));
        }

        Ok(())
    }

    /// Execute ex yank command (:y).
    fn execute_ex_yank(
        &mut self,
        range: &AddressRange,
        register: Option<char>,
        count: Option<usize>,
    ) -> Result<()> {
        use crate::command::yank;

        let (start, end) = self.resolve_range(range)?;
        let end = if let Some(c) = count {
            (start + c - 1).min(self.buffer.line_count())
        } else {
            end
        };

        let start_pos = Position::new(start, 0);
        let end_pos = Position::new(end, 0);
        let yank_range = Range::lines(start_pos, end_pos);

        let _ = yank(&self.buffer, yank_range, &mut self.registers, register);

        let line_count = end - start + 1;
        self.set_message(&format!("{} lines yanked", line_count));

        Ok(())
    }

    /// Write buffer to file.
    fn write(&mut self, path: Option<&str>, _range: bool, append: bool, force: bool) -> Result<()> {
        let path = path
            .map(PathBuf::from)
            .or_else(|| self.files.current_file().map(|p| p.to_path_buf()))
            .ok_or(ViError::NoFileName)?;

        if !force && self.files.is_readonly() {
            return Err(ViError::ReadOnly);
        }

        let stats = write_file(&self.buffer, &path, append)?;
        self.buffer.mark_saved();

        if self.files.current_file().is_none() {
            self.files.set_current_file(Some(path.clone()));
        }

        self.set_message(&stats.message(&path));
        Ok(())
    }

    /// Write and quit.
    fn write_and_quit(&mut self) -> Result<()> {
        if self.buffer.is_modified() {
            self.write(None, false, false, false)?;
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

    /// Refresh the screen.
    fn refresh_screen(&mut self) -> Result<()> {
        let size = self.terminal.size();
        self.screen.resize(size);
        self.screen
            .scroll_to_line(self.buffer.cursor().line, self.buffer.line_count());

        self.terminal.hide_cursor()?;
        self.terminal.move_cursor_home()?;

        // Render buffer lines
        let top = self.screen.top_line();
        let height = (size.rows as usize).saturating_sub(1); // Leave room for status

        for screen_row in 0..height {
            let line_num = top + screen_row;
            self.terminal.move_cursor((screen_row + 1) as u16, 1)?;
            self.terminal.clear_line_to_end()?;

            if line_num <= self.buffer.line_count() {
                if let Some(line) = self.buffer.line(line_num) {
                    // Expand tabs and truncate
                    let content = self.screen.expand_line(line.content(), size.cols as usize);
                    let display = if content.len() > size.cols as usize {
                        &content[..size.cols as usize]
                    } else {
                        &content
                    };
                    self.terminal.write_str(display)?;
                }
            } else {
                self.terminal.write_str("~")?;
            }
        }

        // Render status line
        self.terminal.move_cursor(size.rows, 1)?;
        self.terminal.clear_line_to_end()?;
        let status = self.format_status();
        self.terminal.write_str(&status)?;

        // Position cursor
        let cursor = self.buffer.cursor();
        let display_line = (cursor.line - top + 1) as u16;
        let display_col = self.screen.buffer_col_to_display_col(
            self.buffer
                .line(cursor.line)
                .map(|l| l.content())
                .unwrap_or(""),
            cursor.column,
        ) as u16
            + 1;

        if self.mode == Mode::Ex {
            // Cursor at command line
            self.terminal
                .move_cursor(size.rows, (self.ex_input.len() + 2) as u16)?;
        } else {
            self.terminal.move_cursor(display_line, display_col)?;
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
            status.push_str(&path.display().to_string());
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
    fn file_info(&self) -> String {
        let name = self
            .files
            .current_file()
            .map(|p| p.display().to_string())
            .unwrap_or_else(|| "[No Name]".to_string());

        let modified = if self.buffer.is_modified() { "[+]" } else { "" };
        let lines = self.buffer.line_count();
        let cursor = self.buffer.cursor();

        format!(
            "\"{}\" {} {} lines --{}%--",
            name,
            modified,
            lines,
            if lines > 0 {
                cursor.line * 100 / lines
            } else {
                0
            }
        )
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

    /// Page down (Ctrl-F).
    fn page_down(&mut self) -> Result<()> {
        let window = self.options.window.max(3);
        let scroll = window.saturating_sub(2);
        let current = self.buffer.cursor().line;
        let max_line = self.buffer.line_count();
        let new_line = (current + scroll).min(max_line);
        self.buffer.set_line(new_line);
        self.buffer.move_to_first_non_blank();
        Ok(())
    }

    /// Scroll down half page (Ctrl-D).
    fn scroll_down(&mut self) -> Result<()> {
        let scroll = self.options.scroll.max(1);
        let current = self.buffer.cursor().line;
        let max_line = self.buffer.line_count();
        let new_line = (current + scroll).min(max_line);
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
        let cursor = self.buffer.cursor();
        if let Some(line) = self.buffer.line_mut(cursor.line) {
            let col = cursor.column;
            if col < line.len() {
                // Delete char at cursor and insert new one
                line.delete_char(col);
                line.insert_char(col, c);
            }
        }
    }

    /// Toggle case of character at cursor (~ command).
    fn toggle_case(&mut self) {
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
            if let Some(line) = self.buffer.line_mut(cursor.line) {
                let col = cursor.column;
                line.delete_char(col);
                line.insert_char(col, toggled);
                // Move right if possible
                if col + 1 < line.len() {
                    self.buffer.set_cursor(Position::new(cursor.line, col + 1));
                }
            }
        }
    }

    /// Execute a find command (f/F/t/T).
    fn execute_find(&mut self, command: char, target: char, count: usize) -> Result<()> {
        use crate::command::motion;
        let result = match command {
            'f' => motion::find_char_forward(&self.buffer, target, count),
            'F' => motion::find_char_backward(&self.buffer, target, count),
            't' => motion::till_char_forward(&self.buffer, target, count),
            'T' => motion::till_char_backward(&self.buffer, target, count),
            _ => return Ok(()),
        };
        if let Ok(res) = result {
            self.buffer.set_cursor(res.position);
        }
        Ok(())
    }

    /// Yank lines (for Y command).
    fn execute_yank_lines(&mut self, count: usize) -> Result<()> {
        let start_line = self.buffer.cursor().line;
        let end_line = (start_line + count - 1).min(self.buffer.line_count());

        let mut text = String::new();
        for line_num in start_line..=end_line {
            if let Some(line) = self.buffer.line(line_num) {
                text.push_str(line.content());
                text.push('\n');
            }
        }

        self.registers.set_unnamed(RegisterContent::new(text, true));
        self.set_message(&format!("{} lines yanked", end_line - start_line + 1));
        Ok(())
    }

    /// Repeat last substitution (& command).
    fn repeat_substitution(&mut self) -> Result<()> {
        if let Some((pattern, replacement, global)) = self.last_substitution.clone() {
            let flags = SubstituteFlags {
                global,
                confirm: false,
                print: false,
                count: false,
                ignore_case: self.options.ignorecase,
            };
            let range = AddressRange::current();
            self.substitute(&range, &pattern, &replacement, &flags)?;
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

        let col = cursor.column.min(content.len().saturating_sub(1));
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
    fn goto_tag(&mut self, tag: &str) -> Result<()> {
        // TODO: Full tag support requires reading tags file
        // For now, just set an error message
        self.set_error(&format!("tag not found: {}", tag));
        Ok(())
    }
}

impl Default for Editor {
    fn default() -> Self {
        Self::new().expect("Failed to create editor")
    }
}

#[cfg(test)]
mod tests {
    // Note: Most Editor tests require a terminal, so we test individual components instead.
    // Integration tests will be added in Phase 13.
}
