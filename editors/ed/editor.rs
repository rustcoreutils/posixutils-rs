//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Main editor logic for ed.

use crate::ed::buffer::Buffer;
use crate::ed::error::{EdError, EdResult};
use crate::ed::parser::{parse, Address, AddressInfo, Command, PrintMode};
use regex::Regex;
use std::io::{self, BufRead, Write};

/// The ed editor state.
pub struct Editor<R: BufRead, W: Write> {
    /// The text buffer
    pub buf: Buffer,
    /// Input reader
    reader: R,
    /// Output writer
    writer: W,
    /// Whether to show the prompt
    pub show_prompt: bool,
    /// The prompt string
    pub prompt: String,
    /// Suppress byte count output
    pub silent: bool,
    /// Show verbose error messages
    pub help_mode: bool,
    /// Last error message
    pub last_error: Option<String>,
    /// Last search pattern
    pub last_pattern: Option<String>,
    /// Last substitute pattern
    pub last_sub_pattern: Option<String>,
    /// Last substitute replacement
    pub last_sub_replacement: Option<String>,
    /// Last substitute flags
    pub last_sub_flags: Option<String>,
    /// Last shell command (for !! repetition)
    last_shell_command: Option<String>,
    /// Lines being collected for input mode
    input_lines: Vec<String>,
    /// Whether we're in input mode
    in_input_mode: bool,
    /// The command that initiated input mode
    pending_command: Option<Command>,
    /// Whether the editor should quit
    pub should_quit: bool,
    /// Quit warning given (for modified buffer)
    quit_warning_given: bool,
}

impl<R: BufRead, W: Write> Editor<R, W> {
    /// Create a new editor with custom reader/writer.
    pub fn new(reader: R, writer: W) -> Self {
        Editor {
            buf: Buffer::new(),
            reader,
            writer,
            show_prompt: false,
            prompt: String::new(),
            silent: false,
            help_mode: false,
            last_error: None,
            last_pattern: None,
            last_sub_pattern: None,
            last_sub_replacement: None,
            last_sub_flags: None,
            last_shell_command: None,
            input_lines: Vec::new(),
            in_input_mode: false,
            pending_command: None,
            should_quit: false,
            quit_warning_given: false,
        }
    }

    /// Print the prompt if enabled.
    fn print_prompt(&mut self) -> io::Result<()> {
        if self.show_prompt && !self.prompt.is_empty() {
            write!(self.writer, "{}", self.prompt)?;
            self.writer.flush()?;
        }
        Ok(())
    }

    /// Print an error.
    fn print_error(&mut self, err: &EdError) -> io::Result<()> {
        writeln!(self.writer, "?")?;
        if self.help_mode {
            writeln!(self.writer, "{}", err)?;
        }
        self.last_error = Some(err.to_string());
        Ok(())
    }

    /// Print bytes written (if not silent).
    fn print_bytes(&mut self, bytes: usize) -> io::Result<()> {
        if !self.silent {
            writeln!(self.writer, "{}", bytes)?;
        }
        Ok(())
    }

    /// Resolve an address to a line number.
    fn resolve_address(&mut self, addr: &Address) -> EdResult<usize> {
        self.resolve_address_with_base(addr, self.buf.cur_line)
    }

    /// Resolve an address to a line number, using a specific base for current line.
    /// This is needed for semicolon separator semantics where the second address
    /// should be resolved relative to the first address.
    fn resolve_address_with_base(&mut self, addr: &Address, base_line: usize) -> EdResult<usize> {
        let mut line = match &addr.info {
            AddressInfo::Null => base_line,
            AddressInfo::Current => base_line,
            AddressInfo::Last => self.buf.last_line(),
            AddressInfo::Line(n) => *n,
            AddressInfo::Mark(c) => self.buf.get_mark(*c).ok_or(EdError::InvalidAddress)?,
            AddressInfo::RegexForward(pat) => {
                let result = self.search_forward_from(pat, base_line)?;
                // POSIX: Save pattern for subsequent null RE references
                if !pat.is_empty() {
                    self.last_pattern = Some(pat.clone());
                }
                result
            }
            AddressInfo::RegexBack(pat) => {
                let result = self.search_backward_from(pat, base_line)?;
                // POSIX: Save pattern for subsequent null RE references
                if !pat.is_empty() {
                    self.last_pattern = Some(pat.clone());
                }
                result
            }
            AddressInfo::Offset(off) => {
                let new_line = base_line as isize + off;
                if new_line < 0 {
                    return Err(EdError::AddressOutOfRange);
                }
                new_line as usize
            }
        };

        // Apply offsets
        for off in &addr.offsets {
            let new_line = line as isize + off;
            if new_line < 0 {
                return Err(EdError::AddressOutOfRange);
            }
            line = new_line as usize;
        }

        // Validate range
        if line > self.buf.last_line() {
            return Err(EdError::AddressOutOfRange);
        }

        Ok(line)
    }

    /// Resolve an address range, handling semicolon separator semantics.
    /// When addr2.relative_to_first is true (semicolon separator), addr2 is
    /// resolved using addr1's value as the current line.
    fn resolve_range(&mut self, addr1: &Address, addr2: &Address) -> EdResult<(usize, usize)> {
        let start = self.resolve_address(addr1)?;
        let end = if addr2.relative_to_first {
            // Semicolon separator: resolve second address relative to first
            self.resolve_address_with_base(addr2, start)?
        } else {
            self.resolve_address(addr2)?
        };
        Ok((start, end))
    }

    /// Search forward for a pattern from a specific starting line.
    fn search_forward_from(&self, pattern: &str, from_line: usize) -> EdResult<usize> {
        let pat = if pattern.is_empty() {
            self.last_pattern
                .as_ref()
                .ok_or(EdError::NoPreviousPattern)?
                .clone()
        } else {
            pattern.to_string()
        };

        let re = Regex::new(&pat).map_err(|e| EdError::Syntax(e.to_string()))?;

        // Search from starting line + 1 to end, then wrap to start
        let start = from_line;
        let total = self.buf.line_count();

        if total == 0 {
            return Err(EdError::NoMatch);
        }

        for i in 1..=total {
            let line_num = ((start + i - 1) % total) + 1;
            if let Some(line) = self.buf.get_line(line_num) {
                if re.is_match(line) {
                    return Ok(line_num);
                }
            }
        }

        Err(EdError::NoMatch)
    }

    /// Search backward for a pattern from a specific starting line.
    fn search_backward_from(&self, pattern: &str, from_line: usize) -> EdResult<usize> {
        let pat = if pattern.is_empty() {
            self.last_pattern
                .as_ref()
                .ok_or(EdError::NoPreviousPattern)?
                .clone()
        } else {
            pattern.to_string()
        };

        let re = Regex::new(&pat).map_err(|e| EdError::Syntax(e.to_string()))?;

        let start = from_line;
        let total = self.buf.line_count();

        if total == 0 {
            return Err(EdError::NoMatch);
        }

        for i in 1..=total {
            let line_num = if start >= i {
                start - i + 1
            } else {
                total - (i - start) + 1
            };
            if line_num == 0 || line_num > total {
                continue;
            }
            if let Some(line) = self.buf.get_line(line_num) {
                if re.is_match(line) {
                    return Ok(line_num);
                }
            }
        }

        Err(EdError::NoMatch)
    }

    /// Read a line from input.
    pub fn read_line(&mut self) -> io::Result<Option<String>> {
        let mut line = String::new();
        match self.reader.read_line(&mut line) {
            Ok(0) => Ok(None),
            Ok(_) => Ok(Some(line)),
            Err(e) => Err(e),
        }
    }

    /// Process a line of input.
    pub fn process_line(&mut self, line: &str) -> io::Result<bool> {
        if self.in_input_mode {
            return self.process_input_line(line);
        }

        let trimmed = line.trim_end();
        match parse(trimmed) {
            Ok(cmd) => {
                if let Err(e) = self.execute_command(cmd) {
                    self.print_error(&e)?;
                }
            }
            Err(e) => {
                self.print_error(&e)?;
            }
        }

        Ok(!self.should_quit)
    }

    /// Process a line during input mode.
    fn process_input_line(&mut self, line: &str) -> io::Result<bool> {
        // Check for end of input marker
        if line == ".\n" || line == "." {
            self.in_input_mode = false;
            if let Some(cmd) = self.pending_command.take() {
                let lines = std::mem::take(&mut self.input_lines);
                if let Err(e) = self.finish_input_command(cmd, lines) {
                    self.print_error(&e)?;
                }
            }
        } else {
            // Ensure line ends with newline
            let mut l = line.to_string();
            if !l.ends_with('\n') {
                l.push('\n');
            }
            self.input_lines.push(l);
        }
        Ok(true)
    }

    /// Finish executing a command that required input.
    fn finish_input_command(&mut self, cmd: Command, lines: Vec<String>) -> EdResult<()> {
        match cmd {
            Command::Append(addr) => {
                let line_num = self.resolve_address(&addr)?;
                self.buf.append(line_num, &lines);
            }
            Command::Insert(addr) => {
                let line_num = self.resolve_address(&addr)?;
                let insert_at = if line_num == 0 { 1 } else { line_num };
                self.buf.insert(insert_at, &lines);
            }
            Command::Change(addr1, addr2) => {
                let (start, end) = self.resolve_range(&addr1, &addr2)?;
                self.buf.change(start, end, &lines)?;
            }
            _ => {}
        }
        Ok(())
    }

    /// Execute a parsed command.
    fn execute_command(&mut self, cmd: Command) -> EdResult<()> {
        match cmd {
            Command::Append(ref addr)
            | Command::Insert(ref addr)
            | Command::Change(ref addr, _) => {
                // Validate address before entering input mode
                self.resolve_address(addr)?;
                if let Command::Change(_, ref addr2) = cmd {
                    self.resolve_address(addr2)?;
                }
                self.in_input_mode = true;
                self.pending_command = Some(cmd);
                self.input_lines.clear();
                Ok(())
            }
            Command::Delete(ref addr1, ref addr2) => {
                let (start, end) = self.resolve_range(addr1, addr2)?;
                self.buf.delete(start, end)?;
                Ok(())
            }
            Command::Edit(filename, force) => {
                if !force && self.buf.modified && !self.quit_warning_given {
                    self.quit_warning_given = true;
                    return Err(EdError::BufferModified);
                }
                let path = filename.as_ref().unwrap_or(&self.buf.pathname);
                if path.is_empty() {
                    return Err(EdError::NoFilename);
                }
                let path = path.clone();
                let bytes = self.buf.read_file(&path)?;
                self.print_bytes(bytes)?;
                self.quit_warning_given = false;
                Ok(())
            }
            Command::Filename(filename) => {
                if let Some(f) = filename {
                    self.buf.pathname = f;
                }
                writeln!(self.writer, "{}", self.buf.pathname)?;
                Ok(())
            }
            Command::Global(addr1, addr2, pattern, commands, _interactive) => {
                self.execute_global(addr1, addr2, &pattern, &commands, false)
            }
            Command::GlobalNot(addr1, addr2, pattern, commands, _interactive) => {
                self.execute_global(addr1, addr2, &pattern, &commands, true)
            }
            Command::GlobalInteractive(addr1, addr2, pattern) => {
                // For now, treat as non-interactive with 'p' command
                self.execute_global(addr1, addr2, &pattern, "p", false)
            }
            Command::GlobalNotInteractive(addr1, addr2, pattern) => {
                self.execute_global(addr1, addr2, &pattern, "p", true)
            }
            Command::Help => {
                if let Some(ref err) = self.last_error {
                    writeln!(self.writer, "{}", err)?;
                }
                Ok(())
            }
            Command::HelpMode => {
                self.help_mode = !self.help_mode;
                // POSIX: If help mode is being turned on, also explain the previous '?' if any
                if self.help_mode {
                    if let Some(ref err) = self.last_error {
                        writeln!(self.writer, "{}", err)?;
                    }
                }
                Ok(())
            }
            Command::Join(addr1, addr2) => {
                let (start, end) = self.resolve_range(&addr1, &addr2)?;
                self.buf.join(start, end)?;
                Ok(())
            }
            Command::Mark(addr, mark) => {
                let line = self.resolve_address(&addr)?;
                self.buf.set_mark(mark, line)?;
                Ok(())
            }
            Command::ListLines(addr1, addr2) => self.print_lines(&addr1, &addr2, PrintMode::List),
            Command::Move(addr1, addr2, dest) => {
                let (start, end) = self.resolve_range(&addr1, &addr2)?;
                let dest_line = self.resolve_address(&dest)?;
                self.buf.move_lines(start, end, dest_line)?;
                Ok(())
            }
            Command::Number(addr1, addr2) => self.print_lines(&addr1, &addr2, PrintMode::Numbered),
            Command::Print(addr1, addr2, mode) => self.print_lines(&addr1, &addr2, mode),
            Command::PromptToggle => {
                self.show_prompt = !self.show_prompt;
                Ok(())
            }
            Command::Quit(force) => {
                if !force && self.buf.modified && !self.quit_warning_given {
                    self.quit_warning_given = true;
                    return Err(EdError::BufferModified);
                }
                self.should_quit = true;
                Ok(())
            }
            Command::Read(addr, filename) => {
                let after_line = self.resolve_address(&addr)?;
                let path = filename.as_ref().unwrap_or(&self.buf.pathname);
                if path.is_empty() {
                    return Err(EdError::NoFilename);
                }
                let path = path.clone();
                let bytes = self.buf.read_file_at(&path, after_line)?;
                self.print_bytes(bytes)?;
                Ok(())
            }
            Command::Substitute(addr1, addr2, pattern, replacement, flags) => {
                self.execute_substitute(&addr1, &addr2, &pattern, &replacement, &flags)
            }
            Command::RepeatSubstitute(addr1, addr2) => {
                let pattern = self.last_sub_pattern.clone().unwrap_or_default();
                let replacement = self.last_sub_replacement.clone().unwrap_or_default();
                let flags = self.last_sub_flags.clone().unwrap_or_default();
                self.execute_substitute(&addr1, &addr2, &pattern, &replacement, &flags)
            }
            Command::Copy(addr1, addr2, dest) => {
                let (start, end) = self.resolve_range(&addr1, &addr2)?;
                let dest_line = self.resolve_address(&dest)?;
                self.buf.copy_lines(start, end, dest_line)?;
                Ok(())
            }
            Command::Undo => {
                if self.buf.undo() {
                    Ok(())
                } else {
                    Err(EdError::Generic("nothing to undo".to_string()))
                }
            }
            Command::Write(addr1, addr2, filename, append) => {
                let (start, end) = self.resolve_range(&addr1, &addr2)?;
                let path = filename.as_ref().unwrap_or(&self.buf.pathname);
                if path.is_empty() {
                    return Err(EdError::NoFilename);
                }
                let path = path.clone();
                if append {
                    // Append mode - open file for appending
                    let mut file = std::fs::OpenOptions::new()
                        .create(true)
                        .append(true)
                        .open(&path)?;
                    let bytes = self.buf.write_lines(start, end, &mut file)?;
                    self.print_bytes(bytes)?;
                } else {
                    let bytes = self.buf.write_to_file(start, end, &path)?;
                    if self.buf.pathname.is_empty() {
                        self.buf.pathname = path;
                    }
                    self.print_bytes(bytes)?;
                }
                self.quit_warning_given = false;
                Ok(())
            }
            Command::WriteQuit(addr1, addr2, filename) => {
                let (start, end) = self.resolve_range(&addr1, &addr2)?;
                let path = filename.as_ref().unwrap_or(&self.buf.pathname);
                if path.is_empty() {
                    return Err(EdError::NoFilename);
                }
                let path = path.clone();
                let bytes = self.buf.write_to_file(start, end, &path)?;
                self.print_bytes(bytes)?;
                self.should_quit = true;
                Ok(())
            }
            Command::LineNumber(addr) => {
                let line = self.resolve_address(&addr)?;
                writeln!(self.writer, "{}", line)?;
                Ok(())
            }
            Command::Shell(cmd) => {
                // POSIX: Process special characters in shell command
                let mut final_cmd = cmd.clone();
                let mut modified = false;

                // POSIX: If '!' appears as first character, replace with previous shell command
                if final_cmd.starts_with('!') {
                    if let Some(ref prev) = self.last_shell_command {
                        final_cmd = format!("{}{}", prev, &final_cmd[1..]);
                        modified = true;
                    } else {
                        return Err(EdError::Generic("no previous command".to_string()));
                    }
                }

                // POSIX: Replace unescaped '%' with remembered pathname
                if final_cmd.contains('%') {
                    if self.buf.pathname.is_empty() {
                        return Err(EdError::NoFilename);
                    }
                    let mut result = String::new();
                    let mut chars = final_cmd.chars().peekable();
                    while let Some(ch) = chars.next() {
                        if ch == '\\' {
                            if let Some(&next) = chars.peek() {
                                if next == '%' {
                                    result.push('%');
                                    chars.next();
                                } else {
                                    result.push('\\');
                                }
                            } else {
                                result.push('\\');
                            }
                        } else if ch == '%' {
                            result.push_str(&self.buf.pathname);
                            modified = true;
                        } else {
                            result.push(ch);
                        }
                    }
                    final_cmd = result;
                }

                // POSIX: If any replacements were performed, write modified line before execution
                if modified {
                    writeln!(self.writer, "{}", final_cmd)?;
                }

                // Save for !! repetition
                self.last_shell_command = Some(final_cmd.clone());

                // Execute shell command
                let output = std::process::Command::new("sh")
                    .arg("-c")
                    .arg(&final_cmd)
                    .output()?;
                self.writer.write_all(&output.stdout)?;
                self.writer.write_all(&output.stderr)?;

                // POSIX: Write "!" on completion unless -s option
                if !self.silent {
                    writeln!(self.writer, "!")?;
                }
                Ok(())
            }
            Command::Scroll(addr, count) => {
                let start = self.resolve_address(&addr)?;
                let count = count.unwrap_or(22); // Default page size
                let end = std::cmp::min(start + count - 1, self.buf.last_line());
                if start > 0 && start <= self.buf.last_line() {
                    for i in start..=end {
                        if let Some(line) = self.buf.get_line(i) {
                            write!(self.writer, "{}", line)?;
                        }
                    }
                    self.buf.set_cur_line(end)?;
                }
                Ok(())
            }
            Command::Null(addr) => {
                let line = self.resolve_address(&addr)?;
                self.buf.set_cur_line(line)?;
                Ok(())
            }
            Command::Goto(addr) => {
                let line = self.resolve_address(&addr)?;
                if line == 0 && self.buf.line_count() > 0 {
                    return Err(EdError::AddressOutOfRange);
                }
                self.buf.set_cur_line(line)?;
                // Print the line
                if let Some(content) = self.buf.get_line(line) {
                    write!(self.writer, "{}", content)?;
                }
                Ok(())
            }
        }
    }

    /// Print lines with the specified mode.
    fn print_lines(&mut self, addr1: &Address, addr2: &Address, mode: PrintMode) -> EdResult<()> {
        let (start, end) = self.resolve_range(addr1, addr2)?;

        if start == 0 || end == 0 {
            return Err(EdError::AddressOutOfRange);
        }

        for i in start..=end {
            if let Some(line) = self.buf.get_line(i) {
                match mode {
                    PrintMode::Normal => {
                        write!(self.writer, "{}", line)?;
                    }
                    PrintMode::Numbered => {
                        // Remove trailing newline for formatting, then add it back
                        let content = line.trim_end_matches('\n');
                        writeln!(self.writer, "{:6}\t{}", i, content)?;
                    }
                    PrintMode::List => {
                        // POSIX: Show non-printable characters in unambiguous form
                        // Escape sequences: \\, \a, \b, \f, \r, \t, \v
                        // Non-printable: three-digit octal with backslash
                        // $ within text: escaped with backslash
                        // End of line: marked with $
                        let content = line.trim_end_matches('\n');
                        for ch in content.chars() {
                            match ch {
                                '\\' => write!(self.writer, "\\\\")?,
                                '\x07' => write!(self.writer, "\\a")?, // bell
                                '\x08' => write!(self.writer, "\\b")?, // backspace
                                '\x0c' => write!(self.writer, "\\f")?, // form feed
                                '\r' => write!(self.writer, "\\r")?,   // carriage return
                                '\t' => write!(self.writer, "\\t")?,   // tab
                                '\x0b' => write!(self.writer, "\\v")?, // vertical tab
                                '$' => write!(self.writer, "\\$")?,    // escape $ in text
                                c if c.is_control() || !c.is_ascii() => {
                                    // Non-printable as three-digit octal per byte
                                    for byte in c.to_string().as_bytes() {
                                        write!(self.writer, "\\{:03o}", byte)?;
                                    }
                                }
                                c => write!(self.writer, "{}", c)?,
                            }
                        }
                        writeln!(self.writer, "$")?;
                    }
                }
            }
        }

        self.buf.set_cur_line(end)?;
        Ok(())
    }

    /// Convert POSIX ed replacement string to regex crate format.
    /// POSIX: & -> matched string, \1-\9 -> back-references, \& -> literal &
    /// Regex crate: $0 -> matched string, $1-$9 -> back-references, $$ -> literal $
    fn convert_replacement(&self, repl: &str) -> String {
        let mut result = String::new();
        let mut chars = repl.chars().peekable();

        while let Some(ch) = chars.next() {
            match ch {
                '\\' => {
                    if let Some(&next) = chars.peek() {
                        match next {
                            '&' => {
                                result.push('&');
                                chars.next();
                            }
                            '1'..='9' => {
                                result.push('$');
                                result.push(chars.next().unwrap());
                            }
                            '\\' => {
                                result.push('\\');
                                chars.next();
                            }
                            _ => {
                                result.push('\\');
                            }
                        }
                    } else {
                        result.push('\\');
                    }
                }
                '&' => {
                    result.push_str("$0");
                }
                '$' => {
                    // Escape $ for regex crate
                    result.push_str("$$");
                }
                _ => {
                    result.push(ch);
                }
            }
        }

        result
    }

    /// Execute a substitute command.
    fn execute_substitute(
        &mut self,
        addr1: &Address,
        addr2: &Address,
        pattern: &str,
        replacement: &str,
        flags: &str,
    ) -> EdResult<()> {
        let (start, end) = self.resolve_range(addr1, addr2)?;

        // Use previous pattern if empty
        let pat = if pattern.is_empty() {
            self.last_sub_pattern
                .as_ref()
                .ok_or(EdError::NoPreviousPattern)?
                .clone()
        } else {
            pattern.to_string()
        };

        // Handle % as sole replacement character (use previous replacement)
        let repl = if replacement == "%" {
            self.last_sub_replacement
                .as_ref()
                .ok_or(EdError::Generic("no previous substitution".to_string()))?
                .clone()
        } else {
            replacement.to_string()
        };

        // Save for repeat
        self.last_sub_pattern = Some(pat.clone());
        self.last_sub_replacement = Some(repl.clone());
        self.last_sub_flags = Some(flags.to_string());
        self.last_pattern = Some(pat.clone());

        // Convert POSIX replacement to regex crate format
        let regex_repl = self.convert_replacement(&repl);

        let re = Regex::new(&pat).map_err(|e| EdError::Syntax(e.to_string()))?;

        let global = flags.contains('g');
        let print = flags.contains('p');
        let numbered = flags.contains('n');
        let list = flags.contains('l');

        // Parse count from flags
        let count: Option<usize> = flags
            .chars()
            .filter(|c| c.is_ascii_digit())
            .collect::<String>()
            .parse()
            .ok();

        let mut any_match = false;
        let mut last_matched_line = start;

        for i in start..=end {
            if let Some(line) = self.buf.get_line(i) {
                let line_content = line.clone();
                let new_line = if global {
                    re.replace_all(&line_content, regex_repl.as_str())
                        .to_string()
                } else if let Some(n) = count {
                    // Replace nth occurrence
                    let mut result = line_content.clone();
                    if let Some(m) = re.find_iter(&line_content).nth(n - 1) {
                        let before = &line_content[..m.start()];
                        let after = &line_content[m.end()..];
                        // For nth occurrence, we need to expand the replacement manually
                        let expanded = re.replace(m.as_str(), regex_repl.as_str());
                        result = format!("{}{}{}", before, expanded, after);
                    }
                    result
                } else {
                    re.replace(&line_content, regex_repl.as_str()).to_string()
                };

                if new_line != line_content {
                    any_match = true;
                    last_matched_line = i;
                    // Ensure line ends with newline
                    let final_line = if new_line.ends_with('\n') {
                        new_line
                    } else {
                        format!("{}\n", new_line)
                    };
                    self.buf.change(i, i, &[final_line])?;
                }
            }
        }

        if !any_match {
            return Err(EdError::NoMatch);
        }

        self.buf.set_cur_line(last_matched_line)?;

        // Print if requested
        if print || numbered || list {
            if let Some(line) = self.buf.get_line(last_matched_line) {
                let content = line.trim_end_matches('\n');
                if numbered {
                    writeln!(self.writer, "{:6}\t{}", last_matched_line, content)?;
                } else if list {
                    // Use POSIX-compliant list format
                    for ch in content.chars() {
                        match ch {
                            '\\' => write!(self.writer, "\\\\")?,
                            '\x07' => write!(self.writer, "\\a")?,
                            '\x08' => write!(self.writer, "\\b")?,
                            '\x0c' => write!(self.writer, "\\f")?,
                            '\r' => write!(self.writer, "\\r")?,
                            '\t' => write!(self.writer, "\\t")?,
                            '\x0b' => write!(self.writer, "\\v")?,
                            '$' => write!(self.writer, "\\$")?,
                            c if c.is_control() || !c.is_ascii() => {
                                for byte in c.to_string().as_bytes() {
                                    write!(self.writer, "\\{:03o}", byte)?;
                                }
                            }
                            c => write!(self.writer, "{}", c)?,
                        }
                    }
                    writeln!(self.writer, "$")?;
                } else {
                    write!(self.writer, "{}", line)?;
                }
            }
        }

        Ok(())
    }

    /// Execute a global command.
    fn execute_global(
        &mut self,
        addr1: Address,
        addr2: Address,
        pattern: &str,
        commands: &str,
        invert: bool,
    ) -> EdResult<()> {
        let (start, end) = self.resolve_range(&addr1, &addr2)?;

        // Use previous pattern if empty
        let pat = if pattern.is_empty() {
            self.last_pattern
                .as_ref()
                .ok_or(EdError::NoPreviousPattern)?
                .clone()
        } else {
            pattern.to_string()
        };

        self.last_pattern = Some(pat.clone());

        let re = Regex::new(&pat).map_err(|e| EdError::Syntax(e.to_string()))?;

        // Collect matching lines first (POSIX: mark every line that matches)
        let mut matching_lines = Vec::new();
        for i in start..=end {
            if let Some(line) = self.buf.get_line(i) {
                let matches = re.is_match(line);
                if matches != invert {
                    matching_lines.push(i);
                }
            }
        }

        // POSIX: If there were no matching lines, the current line number shall not be changed
        if matching_lines.is_empty() {
            return Ok(());
        }

        // POSIX: Save one undo record for the entire global operation
        self.buf.begin_global();

        // Track the current line set by the last successfully executed command
        let original_cur_line = self.buf.cur_line;
        let mut last_successful_line = original_cur_line;

        // For delete commands, process in reverse order to avoid line number shifts
        let is_delete = commands.trim() == "d";
        if is_delete {
            matching_lines.reverse();
        }

        // Execute commands on each matching line
        for line_num in matching_lines {
            // Adjust line_num for lines that may have been deleted
            // For non-delete commands, we need to track line number changes
            let adjusted_line = if is_delete {
                line_num
            } else {
                // For other commands, check if line still exists
                if line_num > self.buf.line_count() {
                    continue;
                }
                line_num
            };

            // Set current line
            if self.buf.set_cur_line(adjusted_line).is_err() {
                continue; // Line may have been deleted
            }

            // Parse and execute the command
            match commands.trim() {
                "p" => {
                    if let Some(line) = self.buf.get_line(self.buf.cur_line) {
                        write!(self.writer, "{}", line)?;
                    }
                    last_successful_line = self.buf.cur_line;
                }
                "d" => {
                    let cur = self.buf.cur_line;
                    if self.buf.delete(cur, cur).is_ok() {
                        last_successful_line = self.buf.cur_line;
                    }
                }
                "n" => {
                    if let Some(line) = self.buf.get_line(self.buf.cur_line) {
                        let content = line.trim_end_matches('\n');
                        writeln!(self.writer, "{:6}\t{}", self.buf.cur_line, content)?;
                    }
                    last_successful_line = self.buf.cur_line;
                }
                "l" => {
                    if let Some(line) = self.buf.get_line(self.buf.cur_line) {
                        let content = line.trim_end_matches('\n');
                        for ch in content.chars() {
                            match ch {
                                '\\' => write!(self.writer, "\\\\")?,
                                '\x07' => write!(self.writer, "\\a")?,
                                '\x08' => write!(self.writer, "\\b")?,
                                '\x0c' => write!(self.writer, "\\f")?,
                                '\r' => write!(self.writer, "\\r")?,
                                '\t' => write!(self.writer, "\\t")?,
                                '\x0b' => write!(self.writer, "\\v")?,
                                '$' => write!(self.writer, "\\$")?,
                                c if c.is_control() || !c.is_ascii() => {
                                    for byte in c.to_string().as_bytes() {
                                        write!(self.writer, "\\{:03o}", byte)?;
                                    }
                                }
                                c => write!(self.writer, "{}", c)?,
                            }
                        }
                        writeln!(self.writer, "$")?;
                    }
                    last_successful_line = self.buf.cur_line;
                }
                cmd if !cmd.is_empty() => {
                    // Try to parse and execute other commands
                    if let Ok(parsed_cmd) = parse(cmd) {
                        if self.execute_command(parsed_cmd).is_ok() {
                            last_successful_line = self.buf.cur_line;
                        }
                    }
                }
                _ => {}
            }
        }

        // End global command (re-enable individual undo saves)
        self.buf.end_global();

        // POSIX: When g command completes, current line is value assigned by last command
        let _ = self.buf.set_cur_line(last_successful_line);

        Ok(())
    }

    /// Run the main editor loop.
    pub fn run(&mut self) -> io::Result<()> {
        loop {
            self.print_prompt()?;

            let line = match self.read_line()? {
                Some(l) => l,
                None => break,
            };

            if !self.process_line(&line)? {
                break;
            }
        }

        Ok(())
    }

    /// Load a file into the buffer.
    pub fn load_file(&mut self, path: &str) -> EdResult<usize> {
        let bytes = self.buf.read_file(path)?;
        Ok(bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    fn create_test_editor(input: &str) -> Editor<Cursor<&[u8]>, Vec<u8>> {
        let reader = Cursor::new(input.as_bytes());
        let writer = Vec::new();
        Editor::new(reader, writer)
    }

    #[test]
    fn test_append_and_print() {
        let mut editor = create_test_editor("");
        editor
            .buf
            .append(0, &["hello\n".to_string(), "world\n".to_string()]);
        assert_eq!(editor.buf.line_count(), 2);
    }

    #[test]
    fn test_delete() {
        let mut editor = create_test_editor("");
        editor.buf.append(
            0,
            &["a\n".to_string(), "b\n".to_string(), "c\n".to_string()],
        );
        editor.process_line("2d\n").unwrap();
        assert_eq!(editor.buf.line_count(), 2);
        assert_eq!(editor.buf.get_line(1), Some(&"a\n".to_string()));
        assert_eq!(editor.buf.get_line(2), Some(&"c\n".to_string()));
    }

    #[test]
    fn test_quit() {
        let mut editor = create_test_editor("");
        editor.process_line("q\n").unwrap();
        assert!(editor.should_quit);
    }
}
