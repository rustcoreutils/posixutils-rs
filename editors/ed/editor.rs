//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Main editor logic for ed.

use crate::ed::buffer::{as_path, body, Buffer};
use crate::ed::error::{EdError, EdResult};
use crate::ed::parser::{
    continues_replacement, parse, Address, AddressInfo, Command, Parsed, PrintFlags,
};
use plib::regex::{Match, Regex, RegexFlags};
use std::io::{self, BufRead, Write};
use std::sync::atomic::Ordering;

/// One command of a `g`/`v` command list, parsed once for the whole global,
/// with the text an `a`, `i` or `c` takes from the list itself.
struct GlobalCommand {
    parsed: Parsed,
    /// For a/i/c: the input text lines
    input_lines: Vec<Vec<u8>>,
}

/// Whether a command is one of the input-taking commands (`a`, `i`, `c`).
fn is_input(cmd: &Command) -> bool {
    matches!(
        cmd,
        Command::Append(_) | Command::Insert(_) | Command::Change(..)
    )
}

/// Parse a global command list into its commands, every one before any
/// runs: a bad command fails the global before it has changed anything.
fn parse_global_command_list(commands: &[u8]) -> EdResult<Vec<GlobalCommand>> {
    let lines: Vec<&[u8]> = commands.split(|&b| b == b'\n').collect();
    let mut result = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        // Leading blanks only: trailing ones can be text (the end of an
        // unterminated `s` replacement), as they are outside a global.
        let line = lines[i].trim_ascii_start();
        i += 1;
        if line.is_empty() {
            continue;
        }

        let mut command = line.to_vec();
        // An `s` whose replacement runs onto the next line takes that
        // line, newline included, as the rest of its replacement, as
        // historical eds read it.  That is a line split, which POSIX
        // forbids inside `g`/`v`, so the substitute then fails with `?`
        // rather than quietly running half of what was typed.
        while continues_replacement(&command) && i < lines.len() {
            command.push(b'\n');
            command.extend_from_slice(lines[i]);
            i += 1;
        }
        let parsed = parse(&command)?;
        // POSIX: g, G, v, V and ! may not appear in a command list.  Asking
        // the parsed command, not guessing from its characters: skipping
        // "address-looking" characters to find the letter mistook a pattern
        // for a command and missed any command but the first.
        if matches!(
            parsed.cmd,
            Command::Global(..)
                | Command::GlobalNot(..)
                | Command::GlobalInteractive(..)
                | Command::GlobalNotInteractive(..)
                | Command::Shell(_)
        ) {
            return Err(EdError::Generic(
                "g, G, v, V and ! are not allowed in a global command list".to_string(),
            ));
        }

        // a/i/c take the lines up to `.` (or the end of the list) as text.
        // Classifying by "first alphabetic character" mistook part of an
        // address for the command: `.,/abc/d` reported `a`.
        let mut input_lines = Vec::new();
        if is_input(&parsed.cmd) {
            while i < lines.len() {
                let input_line = lines[i];
                i += 1;
                if input_line == b"." {
                    break;
                }
                let mut text = input_line.to_vec();
                text.push(b'\n');
                input_lines.push(text);
            }
        }
        result.push(GlobalCommand {
            parsed,
            input_lines,
        });
    }

    Ok(result)
}

/// An `a`, `i` or `c` waiting for its text, its lines resolved when it was
/// given: the text can follow only once the addresses are known to be good.
#[derive(Clone, Copy, Debug)]
enum PendingInput {
    /// Append after this line (0: before the first).
    Append(usize),
    /// Insert before this line.
    Insert(usize),
    /// Replace these lines.
    Change(usize, usize),
}

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
    pub last_pattern: Option<Vec<u8>>,
    /// Last substitute pattern
    pub last_sub_pattern: Option<Vec<u8>>,
    /// Last substitute replacement
    pub last_sub_replacement: Option<Vec<u8>>,
    /// Last substitute flags
    pub last_sub_flags: Option<String>,
    /// Last shell command (for !! repetition)
    last_shell_command: Option<Vec<u8>>,
    /// Lines being collected for input mode
    input_lines: Vec<Vec<u8>>,
    /// Whether we're in input mode
    in_input_mode: bool,
    /// The command that initiated input mode, and its print suffix
    pending_input: Option<(PendingInput, PrintFlags)>,
    /// Whether the editor should quit
    pub should_quit: bool,
    /// Quit warning given (for modified buffer)
    quit_warning_given: bool,
    /// Previous command for & in interactive global (G/V)
    last_interactive_cmd: Option<Vec<u8>>,
    /// Whether any command/file error occurred (POSIX: exit status > 0).
    pub error_occurred: bool,
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
            pending_input: None,
            should_quit: false,
            quit_warning_given: false,
            last_interactive_cmd: None,
            error_occurred: false,
        }
    }

    /// Print the prompt if enabled.
    /// Per POSIX, if no prompt string was specified via -p, the default is '*'.
    fn print_prompt(&mut self) -> io::Result<()> {
        if self.show_prompt {
            let prompt = if self.prompt.is_empty() {
                "*"
            } else {
                &self.prompt
            };
            write!(self.writer, "{}", prompt)?;
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
        // POSIX EXIT STATUS: any file or command error makes ed exit > 0.
        self.error_occurred = true;
        Ok(())
    }

    /// Print bytes written (if not silent).
    fn print_bytes(&mut self, bytes: usize) -> io::Result<()> {
        if !self.silent {
            writeln!(self.writer, "{}", bytes)?;
        }
        Ok(())
    }

    /// Execute a shell command and return its stdout as a string.
    /// Used for `e !command` and `r !command`.
    ///
    /// popen(3) semantics, as historical ed: the command shares ed's standard
    /// input and error, and only its output is read.  Its exit status says
    /// nothing about whether that output is wanted -- `diff` and `grep` exit
    /// 1 as a normal answer -- so it is not an error.
    fn shell_read(&mut self, command: &[u8]) -> io::Result<Vec<u8>> {
        use std::os::unix::process::ExitStatusExt;
        use std::process::Stdio;
        let output = self
            .shell(command)?
            .stdin(Stdio::inherit())
            .stderr(Stdio::inherit())
            .output();
        self.after_shell();
        let output = output?;

        // A command that never ran (126, 127) or was killed, and wrote
        // nothing, is a failure, not an empty answer: `e !typo` must not
        // quietly replace the buffer with nothing.
        let failed =
            matches!(output.status.code(), Some(126 | 127)) || output.status.signal().is_some();
        if failed && output.stdout.is_empty() {
            return Err(io::Error::other(format!(
                "command failed: {}",
                output.status
            )));
        }

        // Byte for byte: decoding it as text changed bytes that were not.
        Ok(output.stdout)
    }

    /// Execute a shell command with input piped to its stdin.
    /// Used for `w !command`. Returns bytes written.
    fn shell_write(&mut self, command: &[u8], start: usize, end: usize) -> io::Result<usize> {
        use std::process::Stdio;

        let mut child = self.shell(command)?.stdin(Stdio::piped()).spawn()?;
        let written = self.feed_child(&mut child, start, end);
        let waited = child.wait();
        self.after_shell();
        let bytes = written?;
        waited?;
        Ok(bytes)
    }

    /// `sh -c command`, its standard streams ed's own until the caller says
    /// otherwise, as system(3) and popen(3) run it.
    ///
    /// ed's buffered output is flushed first, so a child writing to the same
    /// terminal comes after it.  SIGQUIT goes back to its default in the
    /// child: ed ignores it, and an ignored signal stays ignored across exec,
    /// so `^\` could not stop a runaway command.
    fn shell(&mut self, command: &[u8]) -> io::Result<std::process::Command> {
        use std::os::unix::ffi::OsStrExt;
        use std::os::unix::process::CommandExt;
        self.writer.flush()?;
        let mut cmd = std::process::Command::new("sh");
        cmd.arg("-c").arg(std::ffi::OsStr::from_bytes(command));
        // SAFETY: only signal(2), which is async-signal-safe, runs between
        // fork and exec.
        unsafe {
            cmd.pre_exec(|| {
                libc::signal(libc::SIGQUIT, libc::SIG_DFL);
                Ok(())
            });
        }
        Ok(cmd)
    }

    /// After a shell command: an interrupt typed while it ran was the
    /// command's (system(3) ignores SIGINT in the caller meanwhile), not one
    /// to report as ed's own `?` afterwards.
    fn after_shell(&self) {
        crate::SIGINT_RECEIVED.store(false, Ordering::SeqCst);
    }

    /// Write lines `start..=end` to a `w !command` child; the bytes written.
    fn feed_child(
        &self,
        child: &mut std::process::Child,
        start: usize,
        end: usize,
    ) -> io::Result<usize> {
        // The command may exit without reading it all (`w !head -1`). That
        // closes the pipe we own the far end of, and the default disposition
        // would kill ed -- losing the buffer -- instead of letting the write
        // fail so `?` can be reported.
        let _sigpipe = plib::io::SigPipeIgnored::new();

        let mut bytes = 0;
        if let Some(mut stdin) = child.stdin.take() {
            for i in start..=end {
                if let Some(line) = self.buf.get_line(i) {
                    stdin.write_all(line)?;
                    bytes += line.len();
                }
            }
        }
        Ok(bytes)
    }

    /// Resolve an address to a line number.
    fn resolve_address(&mut self, addr: &Address) -> EdResult<usize> {
        self.resolve_address_with_base(addr, self.buf.cur_line)
    }

    /// Resolve an address to a line number, using a specific base for current line.
    /// This is needed for semicolon separator semantics where the second address
    /// should be resolved relative to the first address.
    fn resolve_address_with_base(&mut self, addr: &Address, base_line: usize) -> EdResult<usize> {
        // Resolve the base address in signed arithmetic. Per POSIX,
        // intermediate values produced while chaining +/- offsets may legally
        // be out of range; only the final resolved address is validated.
        let base: isize = match &addr.info {
            AddressInfo::Null => base_line as isize,
            AddressInfo::Current => base_line as isize,
            AddressInfo::Last => self.buf.last_line() as isize,
            AddressInfo::Line(n) => *n as isize,
            AddressInfo::Mark(c) => self.buf.get_mark(*c).ok_or(EdError::InvalidAddress)? as isize,
            AddressInfo::RegexForward(pat) => {
                let (_, re) = self.compile_re(pat)?;
                self.search_forward_from(&re, base_line)? as isize
            }
            AddressInfo::RegexBack(pat) => {
                let (_, re) = self.compile_re(pat)?;
                self.search_backward_from(&re, base_line)? as isize
            }
            AddressInfo::Offset(off) => (base_line as isize).saturating_add(*off),
        };

        // Apply chained offsets; intermediate values may legally be out of
        // range, so saturate rather than bound-check -- but saturate rather
        // than wrap, or a chain that overflows lands on an unrelated line in
        // release and aborts the editor in debug.
        let acc = addr
            .offsets
            .iter()
            .fold(base, |a, off| a.saturating_add(*off));

        // Validate only the final resolved address.
        if acc < 0 {
            return Err(EdError::AddressOutOfRange);
        }
        let line = acc as usize;
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

    /// The RE `pattern` names, compiled, and its text: the remembered RE when
    /// `pattern` is null.  POSIX: "A null RE shall be equivalent to the last
    /// RE encountered" -- by an address, a g/v or an s alike, so every one of
    /// them resolves here.  A pattern becomes the remembered RE only once it
    /// compiles: an invalid one must not replace a good one.
    fn compile_re(&mut self, pattern: &[u8]) -> EdResult<(Vec<u8>, Regex)> {
        let pat = if pattern.is_empty() {
            self.last_pattern
                .clone()
                .ok_or(EdError::NoPreviousPattern)?
        } else {
            pattern.to_vec()
        };
        let re = Regex::new_bytes(&pat, RegexFlags::bre())
            .map_err(|e| EdError::Syntax(e.to_string()))?;
        self.last_pattern = Some(pat.clone());
        Ok((pat, re))
    }

    /// Search forward for a pattern from a specific starting line.
    fn search_forward_from(&self, re: &Regex, from_line: usize) -> EdResult<usize> {
        // Search from starting line + 1 to end, then wrap to start
        let start = from_line;
        let total = self.buf.line_count();

        if total == 0 {
            return Err(EdError::NoMatch);
        }

        for i in 1..=total {
            let line_num = ((start + i - 1) % total) + 1;
            if let Some(line) = self.buf.get_line(line_num) {
                if re.is_match_bytes(body(line)) {
                    return Ok(line_num);
                }
            }
        }

        Err(EdError::NoMatch)
    }

    /// Search backward for a pattern from a specific starting line.
    fn search_backward_from(&self, re: &Regex, from_line: usize) -> EdResult<usize> {
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
                if re.is_match_bytes(body(line)) {
                    return Ok(line_num);
                }
            }
        }

        Err(EdError::NoMatch)
    }

    /// Read a line from input.
    ///
    /// Bytes: input text and command arguments may be anything the file
    /// being edited holds.
    pub fn read_line(&mut self) -> io::Result<Option<Vec<u8>>> {
        let mut line = Vec::new();
        match self.reader.read_until(b'\n', &mut line) {
            Ok(0) => Ok(None),
            Ok(_) => Ok(Some(line)),
            Err(e) => Err(e),
        }
    }

    /// Check if a line has a continuation (ends with unescaped backslash).
    /// Returns (trimmed_line_without_backslash, needs_continuation).
    fn check_line_continuation(line: &[u8]) -> (Vec<u8>, bool) {
        let trimmed = body(line);

        // Count trailing backslashes
        let backslash_count = trimmed.iter().rev().take_while(|&&b| b == b'\\').count();

        // Odd number of trailing backslashes = continuation
        if backslash_count > 0 && backslash_count % 2 == 1 {
            // Remove the trailing backslash and indicate continuation needed
            let without_backslash = &trimmed[..trimmed.len() - 1];
            (without_backslash.to_vec(), true)
        } else {
            (trimmed.to_vec(), false)
        }
    }

    /// Process a line of input.
    pub fn process_line(&mut self, line: &[u8]) -> io::Result<bool> {
        if self.in_input_mode {
            return self.process_input_line(line);
        }

        // Handle line continuation (backslash-newline)
        let (first_part, needs_continuation) = Self::check_line_continuation(line);
        let full_line = if needs_continuation {
            let mut accumulated = first_part;
            // Keep reading lines until we get one without continuation
            while let Some(next_line) = self.read_line()? {
                let (part, more) = Self::check_line_continuation(&next_line);
                // Join with embedded newline (the backslash-newline becomes \n in content)
                accumulated.push(b'\n');
                accumulated.extend_from_slice(&part);
                if !more {
                    break;
                }
            }
            accumulated
        } else {
            first_part
        };

        match parse(&full_line) {
            Ok(parsed) => {
                // POSIX (ed, DESCRIPTION): ed exits on a repeated `e`/`q`
                // only when the repeat has "no intervening command", so any
                // other command re-arms the warning.  Without this the flag
                // latched on the first warning and every later `q` discarded
                // the buffer silently, however much had been edited since.
                if !matches!(parsed.cmd, Command::Quit(_) | Command::Edit(..)) {
                    self.quit_warning_given = false;
                }
                if let Err(e) = self.run_parsed(parsed) {
                    self.print_error(&e)?;
                }
            }
            Err(e) => {
                // A command that failed to parse still intervened.
                self.quit_warning_given = false;
                self.print_error(&e)?;
            }
        }

        Ok(!self.should_quit)
    }

    /// Process a line during input mode.
    fn process_input_line(&mut self, line: &[u8]) -> io::Result<bool> {
        // Check for end of input marker
        if body(line) == b"." {
            self.in_input_mode = false;
            if let Some(pending) = self.pending_input.take() {
                let lines = std::mem::take(&mut self.input_lines);
                if let Err(e) = self.finish_input_command(pending, lines) {
                    self.print_error(&e)?;
                }
            }
        } else {
            // Ensure line ends with newline
            let mut l = line.to_vec();
            if l.last() != Some(&b'\n') {
                l.push(b'\n');
            }
            self.input_lines.push(l);
        }
        Ok(true)
    }

    /// Finish executing a command that required input.
    fn finish_input_command(
        &mut self,
        (pending, suffix): (PendingInput, PrintFlags),
        lines: Vec<Vec<u8>>,
    ) -> EdResult<()> {
        self.apply_input(pending, &lines)?;
        self.print_suffix(suffix)
    }

    /// Put input text where an `a`, `i` or `c` said.
    fn apply_input(&mut self, pending: PendingInput, lines: &[Vec<u8>]) -> EdResult<()> {
        match pending {
            PendingInput::Append(line) => self.buf.append(line, lines),
            PendingInput::Insert(line) => self.buf.insert(line.max(1), lines),
            PendingInput::Change(start, end) => self.buf.change(start, end, lines)?,
        }
        Ok(())
    }

    /// Resolve the addresses of an `a`, `i` or `c` -- all of them, with the
    /// `;` rule, before any text is read.  Resolving `c`'s second address on
    /// its own took `1;+1` from the current line rather than from 1.
    fn resolve_input(&mut self, cmd: &Command) -> EdResult<PendingInput> {
        Ok(match cmd {
            Command::Append(addr) => PendingInput::Append(self.resolve_address(addr)?),
            Command::Insert(addr) => PendingInput::Insert(self.resolve_address(addr)?),
            Command::Change(addr1, addr2) => {
                let (start, end) = self.resolve_range(addr1, addr2)?;
                if start == 0 || start > end {
                    return Err(EdError::AddressOutOfRange);
                }
                PendingInput::Change(start, end)
            }
            _ => return Err(EdError::Generic("not an input command".to_string())),
        })
    }

    /// Run one parsed command, then write the current line as its print
    /// suffix asks.  `a`, `i` and `c` keep the suffix until their text is in.
    fn run_parsed(&mut self, parsed: Parsed) -> EdResult<()> {
        let Parsed { cmd, suffix } = parsed;
        if matches!(
            cmd,
            Command::Append(_) | Command::Insert(_) | Command::Change(..)
        ) {
            // Input mode is driven by the main loop, which does not run
            // inside `g`/`v`.  The global path handles a/i/c itself; if
            // one ever reaches here the mode would latch with nothing to
            // service it, so refuse rather than leak it to the caller.
            if self.buf.is_in_global() {
                return Err(EdError::Generic(
                    "a/i/c cannot enter input mode inside a global command".to_string(),
                ));
            }
            let pending = self.resolve_input(&cmd)?;
            self.in_input_mode = true;
            self.pending_input = Some((pending, suffix));
            self.input_lines.clear();
            return Ok(());
        }
        self.execute_command(cmd)?;
        self.print_suffix(suffix)
    }

    /// Write the current line as a print suffix asks, if it asks.
    fn print_suffix(&mut self, suffix: PrintFlags) -> EdResult<()> {
        if !suffix.any() {
            return Ok(());
        }
        let cur = self.buf.cur_line;
        if cur == 0 {
            return Err(EdError::AddressOutOfRange);
        }
        self.write_line(cur, suffix)?;
        Ok(())
    }

    /// Write line `n` the way `p`, `n` and `l` do, combined as `flags` says:
    /// numbered first, then the text listed unambiguously or as is.
    fn write_line(&mut self, n: usize, flags: PrintFlags) -> io::Result<()> {
        // `buf` and `writer` are separate fields, so the line is written
        // from where it is, not copied first.
        let Some(line) = self.buf.get_line(n) else {
            return Ok(());
        };
        let content = body(line);
        let w = &mut self.writer;
        if flags.number {
            write!(w, "{}\t", n)?;
        }
        if !flags.list {
            w.write_all(content)?;
            return w.write_all(b"\n");
        }
        // POSIX: show non-printable characters unambiguously -- the C escapes
        // for \\ \a \b \f \r \t \v, three-digit octal for any other byte that
        // is not printable ASCII, `$` escaped in the text and marking the end
        // of the line -- and fold long lines with a `\` before the newline.
        const FOLD_WIDTH: usize = 72;
        let mut col = 0;
        let mut at = 0;
        while at < content.len() {
            // One character of the locale's encoding.  A multibyte one the
            // locale calls printable is written as itself (`é` in a UTF-8
            // locale); anything else goes byte by byte, below.
            let next = plib::locale::next_char_offset(content, at).unwrap_or(at + 1);
            let ch = &content[at..next];
            if ch.len() > 1 {
                // Decoded by the locale (`mbrtowc`), so any multibyte
                // encoding works, not just UTF-8.
                let printable = match plib::locale::MbDecoder::new().decode(ch)[..] {
                    [Some(c)] if plib::locale::isprint(c) => Some(c),
                    _ => None,
                };
                if let Some(c) = printable {
                    let cw = plib::locale::wcwidth_char(c).max(1) as usize;
                    if col + cw > FOLD_WIDTH {
                        w.write_all(b"\\\n")?;
                        col = 0;
                    }
                    w.write_all(ch)?;
                    col += cw;
                    at = next;
                    continue;
                }
            }
            let b = content[at];
            at += 1;
            let octal;
            let plain = [b];
            let escaped: &[u8] = match b {
                b'\\' => b"\\\\",
                0x07 => b"\\a",
                0x08 => b"\\b",
                0x0c => b"\\f",
                b'\r' => b"\\r",
                b'\t' => b"\\t",
                0x0b => b"\\v",
                b'$' => b"\\$",
                b' '..=b'~' => &plain,
                _ => {
                    octal = [
                        b'\\',
                        b'0' + (b >> 6),
                        b'0' + ((b >> 3) & 7),
                        b'0' + (b & 7),
                    ];
                    &octal
                }
            };
            // Fold before a character that would not fit.
            if col + escaped.len() > FOLD_WIDTH {
                w.write_all(b"\\\n")?;
                col = 0;
            }
            w.write_all(escaped)?;
            col += escaped.len();
        }
        w.write_all(b"$\n")
    }

    /// Execute a parsed command.
    fn execute_command(&mut self, cmd: Command) -> EdResult<()> {
        match cmd {
            // `run_parsed` owns input mode and takes these before they get
            // here.
            Command::Append(_) | Command::Insert(_) | Command::Change(..) => Err(EdError::Generic(
                "a/i/c must go through run_parsed".to_string(),
            )),
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

                // POSIX: If path starts with !, execute as shell command
                if let Some(command) = path.strip_prefix(b"!") {
                    let content = self.shell_read(command)?;
                    // Load content into buffer
                    self.buf.load_from_bytes(&content);
                    // Don't set pathname for shell commands
                    self.print_bytes(content.len())?;
                } else {
                    let bytes = self.buf.read_file(&path)?;
                    self.print_bytes(bytes)?;
                }
                Ok(())
            }
            Command::Filename(filename) => {
                if let Some(f) = filename {
                    self.buf.pathname = f;
                }
                self.writer.write_all(&self.buf.pathname)?;
                self.writer.write_all(b"\n")?;
                Ok(())
            }
            Command::Global(addr1, addr2, pattern, commands, _interactive) => {
                self.execute_global(addr1, addr2, &pattern, &commands, false)
            }
            Command::GlobalNot(addr1, addr2, pattern, commands, _interactive) => {
                self.execute_global(addr1, addr2, &pattern, &commands, true)
            }
            Command::GlobalInteractive(addr1, addr2, pattern) => {
                self.execute_global_interactive(addr1, addr2, &pattern, false)
            }
            Command::GlobalNotInteractive(addr1, addr2, pattern) => {
                self.execute_global_interactive(addr1, addr2, &pattern, true)
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
            Command::Move(addr1, addr2, dest) => {
                let (start, end) = self.resolve_range(&addr1, &addr2)?;
                let dest_line = self.resolve_address(&dest)?;
                self.buf.move_lines(start, end, dest_line)?;
                Ok(())
            }
            Command::Print(addr1, addr2, flags) => self.print_lines(&addr1, &addr2, flags),
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

                // POSIX: If path starts with !, execute as shell command
                if let Some(command) = path.strip_prefix(b"!") {
                    let content = self.shell_read(command)?;
                    self.buf.append_from_bytes(after_line, &content);
                    self.print_bytes(content.len())?;
                } else {
                    let bytes = self.buf.read_file_at(&path, after_line)?;
                    self.print_bytes(bytes)?;
                }
                Ok(())
            }
            Command::Substitute(addr1, addr2, pattern, replacement, flags) => {
                self.execute_substitute(&addr1, &addr2, &pattern, &replacement, &flags)
            }
            Command::RepeatSubstitute(addr1, addr2) => {
                // With no earlier `s` there is nothing to repeat: an empty
                // pattern here would be the null RE and substitute with
                // whatever address RE came last.
                let pattern = self
                    .last_sub_pattern
                    .clone()
                    .ok_or_else(|| EdError::Generic("no previous substitution".to_string()))?;
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
            Command::Write {
                addr1,
                addr2,
                filename,
                append,
                quit,
            } => {
                // The default `1,$` of an empty buffer is no lines, not an
                // error: writing it empties the file, as it should, where
                // refusing kept text the user had deleted.
                let whole = |a: &Address, info: AddressInfo| a.info == info && a.offsets.is_empty();
                let (start, end) = if self.buf.line_count() == 0
                    && whole(&addr1, AddressInfo::Line(1))
                    && whole(&addr2, AddressInfo::Last)
                {
                    (1, 0)
                } else {
                    self.resolve_range(&addr1, &addr2)?
                };
                let path = filename.as_ref().unwrap_or(&self.buf.pathname);
                if path.is_empty() {
                    return Err(EdError::NoFilename);
                }
                let path = path.clone();

                // POSIX: If path starts with !, execute as shell command
                if let Some(command) = path.strip_prefix(b"!") {
                    let bytes = self.shell_write(command, start, end)?;
                    self.print_bytes(bytes)?;
                } else if append {
                    // Append mode - open file for appending
                    let mut file = std::fs::OpenOptions::new()
                        .create(true)
                        .append(true)
                        .open(as_path(&path))?;
                    let bytes = self.buf.write_lines(start, end, &mut file)?;
                    self.print_bytes(bytes)?;
                } else {
                    let bytes = self.buf.write_to_file(start, end, &path)?;
                    if self.buf.pathname.is_empty() {
                        self.buf.pathname = path;
                    }
                    self.print_bytes(bytes)?;
                }
                // Only on success: a failed write leaves the buffer unsaved,
                // so quitting would be exactly the data loss `wq` prevents.
                if quit {
                    self.should_quit = true;
                }
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
                if final_cmd.first() == Some(&b'!') {
                    if let Some(ref prev) = self.last_shell_command {
                        let mut expanded = prev.clone();
                        expanded.extend_from_slice(&final_cmd[1..]);
                        final_cmd = expanded;
                        modified = true;
                    } else {
                        return Err(EdError::Generic("no previous command".to_string()));
                    }
                }

                // POSIX: Replace unescaped '%' with remembered pathname
                if final_cmd.contains(&b'%') {
                    if self.buf.pathname.is_empty() {
                        return Err(EdError::NoFilename);
                    }
                    let mut result = Vec::new();
                    let mut bytes = final_cmd.iter().peekable();
                    while let Some(&b) = bytes.next() {
                        if b == b'\\' && bytes.peek() == Some(&&b'%') {
                            result.push(b'%');
                            bytes.next();
                        } else if b == b'%' {
                            result.extend_from_slice(&self.buf.pathname);
                            modified = true;
                        } else {
                            result.push(b);
                        }
                    }
                    final_cmd = result;
                }

                // POSIX: If any replacements were performed, write modified line before execution
                if modified {
                    self.writer.write_all(&final_cmd)?;
                    self.writer.write_all(b"\n")?;
                }

                // Save for !! repetition
                self.last_shell_command = Some(final_cmd.clone());

                // Run it on ed's own standard input, output and error, so an
                // interactive command (`!sh`) works and diagnostics stay on
                // stderr.  What ed has written so far must reach the
                // terminal first.
                let status = self.shell(&final_cmd)?.status();
                self.after_shell();
                status?;

                // POSIX: Write "!" on completion unless -s option
                if !self.silent {
                    writeln!(self.writer, "!")?;
                }
                Ok(())
            }
            Command::Scroll(addr, count) => {
                let start = self.resolve_address(&addr)?;
                let count = count.unwrap_or(22); // Default page size
                if count == 0 {
                    return Err(EdError::InvalidCommand("z: count must be positive".into()));
                }
                let end = std::cmp::min(
                    start.saturating_add(count.saturating_sub(1)),
                    self.buf.last_line(),
                );
                if start > 0 && start <= self.buf.last_line() {
                    for i in start..=end {
                        if let Some(line) = self.buf.get_line(i) {
                            self.writer.write_all(line)?;
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
                    self.writer.write_all(content)?;
                }
                Ok(())
            }
        }
    }

    /// Write the lines of a range, as `flags` says.
    fn print_lines(&mut self, addr1: &Address, addr2: &Address, flags: PrintFlags) -> EdResult<()> {
        let (start, end) = self.resolve_range(addr1, addr2)?;

        if start == 0 || end == 0 || start > end {
            return Err(EdError::AddressOutOfRange);
        }

        for i in start..=end {
            self.write_line(i, flags)?;
        }

        self.buf.set_cur_line(end)?;
        Ok(())
    }

    /// Expand an ed `s` replacement template against one set of capture
    /// matches. Supports `&` (whole match), `\1`-`\9` (back-references),
    /// `\&` (literal `&`), `\\` (literal backslash), and `\<newline>` (line
    /// split). Any other `\c` yields the literal character `c`.
    fn build_ed_replacement(template: &[u8], input: &[u8], matches: &[Match]) -> Vec<u8> {
        let mut result = Vec::with_capacity(template.len() + 16);
        let mut bytes = template.iter();

        while let Some(&b) = bytes.next() {
            match b {
                b'&' => {
                    let m = matches[0];
                    result.extend_from_slice(&input[m.start..m.end]);
                }
                b'\\' => match bytes.next() {
                    Some(&d @ b'1'..=b'9') => {
                        if let Some(m) = matches.get(usize::from(d - b'0')) {
                            if m.end > m.start {
                                result.extend_from_slice(&input[m.start..m.end]);
                            }
                        }
                    }
                    // `\&`, `\\`, `\<newline>` and any other `\c`: the byte.
                    Some(&other) => result.push(other),
                    None => result.push(b'\\'),
                },
                _ => result.push(b),
            }
        }

        result
    }

    /// Apply the substitution to a single line body (no trailing newline).
    /// Returns `Some(new_body)` if the pattern matched at least one selected
    /// occurrence (even when the resulting text is identical to the input);
    /// returns `None` if there was no match. The returned body may contain
    /// embedded newlines when the replacement used `\<newline>`.
    fn substitute_line(
        re: &Regex,
        body: &[u8],
        repl: &[u8],
        global: bool,
        count: Option<usize>,
    ) -> Option<Vec<u8>> {
        let start_idx = count.map(|n| n.saturating_sub(1)).unwrap_or(0);
        let mut result = Vec::new();
        let mut last_end = 0usize;
        let mut pos = 0usize;
        let mut occurrence = 0usize;
        let mut matched = false;
        // Where the last non-empty match ended: an empty match right there
        // is not a new occurrence (`s/b*$/X/g` on `ab` is `aX`, not `aXX`).
        let mut nonempty_end = None;

        while let Some(caps) = re.captures_at_bytes(body, pos) {
            let m = caps[0];
            let (ms, me) = (m.start, m.end);
            if ms == me && nonempty_end == Some(ms) {
                match plib::locale::next_char_offset(body, me) {
                    Some(next) => {
                        pos = next;
                        continue;
                    }
                    None => break,
                }
            }
            if me > ms {
                nonempty_end = Some(me);
            }
            let idx = occurrence;
            occurrence += 1;

            let replace_this = match (global, count.is_some()) {
                (true, true) => idx >= start_idx,  // g + count: nth..end
                (false, true) => idx == start_idx, // count: only nth
                (true, false) => true,             // g: all
                (false, false) => idx == 0,        // default: first
            };

            if replace_this {
                matched = true;
                result.extend_from_slice(&body[last_end..ms]);
                result.extend_from_slice(&Self::build_ed_replacement(repl, body, &caps));
                last_end = me;
            }

            // Advance past this match; past one whole character, in the
            // locale's encoding, after a zero-width one.
            let next = if me > ms {
                me
            } else {
                plib::locale::next_char_offset(body, me).unwrap_or(me + 1)
            };
            if next > body.len() {
                break;
            }
            pos = next;

            // Once the single required replacement is done, stop scanning.
            if matched && !global {
                break;
            }
        }

        if matched {
            result.extend_from_slice(&body[last_end..]);
            Some(result)
        } else {
            None
        }
    }

    /// Execute a substitute command.
    fn execute_substitute(
        &mut self,
        addr1: &Address,
        addr2: &Address,
        pattern: &[u8],
        replacement: &[u8],
        flags: &str,
    ) -> EdResult<()> {
        let (start, end) = self.resolve_range(addr1, addr2)?;

        // Everything that can fail is checked before anything is remembered.
        let SubstituteSuffix {
            count,
            global,
            print,
        } = Self::parse_substitute_suffix(flags)?;

        // Handle % as sole replacement character (use previous replacement)
        let repl = if replacement == b"%" {
            self.last_sub_replacement
                .as_ref()
                .ok_or(EdError::Generic("no previous substitution".to_string()))?
                .clone()
        } else {
            replacement.to_vec()
        };

        // A null RE is the last RE of any command, not just of an `s`.
        let (pat, re) = self.compile_re(pattern)?;

        // Save for repeat
        self.last_sub_pattern = Some(pat.clone());
        self.last_sub_replacement = Some(repl.clone());
        self.last_sub_flags = Some(flags.to_string());

        // POSIX (ed, `u`): one `s` is one command however many lines it
        // rewrote.  Every `buf.change()` below snapshots for undo, so without
        // a group `u` after `1,$s/../../` reverted only the last line and the
        // rest were unrecoverable.  This is `begin_undo_group` rather than
        // `begin_global` because `is_in_global()` carries the separate
        // meaning checked inside the loop.
        self.buf.begin_undo_group();
        let outcome = self.substitute_lines(&re, start, end, &repl, global, count);
        self.buf.end_undo_group();
        let (any_match, last_matched_line) = outcome?;

        if !any_match {
            return Err(EdError::NoMatch);
        }

        self.buf.set_cur_line(last_matched_line)?;

        if print.any() {
            self.write_line(last_matched_line, print)?;
        }
        Ok(())
    }

    /// Rewrite every matching line in `start..=end`, returning whether
    /// anything matched and the last line the substitution touched.
    ///
    /// Split out of `execute_substitute` so the caller can bracket it with an
    /// undo group that survives an early `?`.
    fn substitute_lines(
        &mut self,
        re: &Regex,
        start: usize,
        end: usize,
        repl: &[u8],
        global: bool,
        count: Option<usize>,
    ) -> EdResult<(bool, usize)> {
        let mut any_match = false;
        let mut last_matched_line = start;
        // Track offset when lines are split (inserted lines shift subsequent line numbers)
        let mut offset: usize = 0;

        for i in start..=end {
            // Compute actual buffer position accounting for previously inserted lines
            let actual_line = i + offset;

            let Some(line) = self.buf.get_line(actual_line) else {
                continue;
            };
            let line_content = line.to_vec();
            let body = body(&line_content);

            // A match — even one whose replacement yields identical text —
            // counts as a substitution: the line is rewritten (and the buffer
            // marked modified). Only a complete absence of matches is an error.
            let Some(new_body) = Self::substitute_line(re, body, repl, global, count) else {
                continue;
            };
            any_match = true;

            if new_body.contains(&b'\n') {
                // POSIX: line splitting via \<newline> is not allowed in g/v.
                if self.buf.is_in_global() {
                    return Err(EdError::Generic(
                        "cannot split lines in global command".to_string(),
                    ));
                }

                let lines: Vec<Vec<u8>> = new_body
                    .split(|&b| b == b'\n')
                    .map(|piece| [piece, b"\n"].concat())
                    .collect();
                let num_new_lines = lines.len();
                self.buf.change(actual_line, actual_line, &lines)?;
                last_matched_line = actual_line + num_new_lines - 1;
                offset += num_new_lines - 1;
            } else {
                let mut line = new_body;
                line.push(b'\n');
                self.buf.change(actual_line, actual_line, &[line])?;
                last_matched_line = actual_line;
            }
        }

        Ok((any_match, last_matched_line))
    }

    /// The parsed suffix of an `s` command.
    fn parse_substitute_suffix(flags: &str) -> EdResult<SubstituteSuffix> {
        let mut out = SubstituteSuffix::default();
        let mut digits = String::new();
        let mut digits_done = false;

        for ch in flags.chars() {
            match ch {
                // POSIX (ed, `s`) writes the count before the flag letters,
                // but historical eds accept either order.  What is never
                // valid is a second run of digits: `s/x/y/2p3` used to
                // collect every digit anywhere in the suffix and silently
                // become count 23.
                '0'..='9' => {
                    if digits_done {
                        return Err(EdError::Syntax(format!(
                            "invalid command suffix: {}",
                            flags
                        )));
                    }
                    digits.push(ch);
                }
                'g' | 'l' | 'n' | 'p' => {
                    if !digits.is_empty() {
                        digits_done = true;
                    }
                    if ch == 'g' {
                        out.global = true;
                    } else {
                        out.print.add(ch);
                    }
                }
                c if c.is_whitespace() => {}
                _ => {
                    return Err(EdError::Syntax(format!(
                        "invalid command suffix: {}",
                        flags
                    )))
                }
            }
        }

        if !digits.is_empty() {
            let count: usize = digits
                .parse()
                .map_err(|_| EdError::Syntax("substitute count out of range".to_string()))?;
            // There is no zeroth occurrence: `s/a/b/0` used to act as 1.
            if count == 0 {
                return Err(EdError::Syntax(
                    "substitute count must be positive".to_string(),
                ));
            }
            out.count = Some(count);
        }
        Ok(out)
    }

    /// Execute a global command.
    fn execute_global(
        &mut self,
        addr1: Address,
        addr2: Address,
        pattern: &[u8],
        commands: &[u8],
        invert: bool,
    ) -> EdResult<()> {
        // Marking first compiles the RE, so an invalid one is what is
        // reported and a valid one is remembered whatever the list holds.
        let marked = self.mark_matches(&addr1, &addr2, pattern, invert)?;

        // Parsed before anything is changed: a bad or forbidden command
        // fails the global as a whole.
        let global_commands = parse_global_command_list(commands)?;

        // POSIX: If there were no matching lines, the current line number shall not be changed
        if marked.is_empty() {
            return Ok(());
        }

        // POSIX: Save one undo record for the entire global operation.
        // Paired here rather than inside the body: the body writes to
        // `self.writer` with `?` in several places, and an I/O error there
        // used to skip `end_global()` and leave the buffer permanently
        // "inside a global", which silently disabled undo for the rest of
        // the session.
        self.buf.begin_global();
        let outcome = self.run_global_commands(&global_commands, &marked);
        self.buf.end_global();
        outcome
    }

    /// The first step of g/v/G/V: mark every line of the range that matches
    /// (or, inverted, does not).  The marks are line identities, so each
    /// pass finds its line wherever earlier passes moved it, and a marked
    /// line an earlier pass deleted or replaced is simply gone.  Counting
    /// lines added and removed could not follow a line `m` moved, nor tell
    /// which line a `+1d` had taken.
    fn mark_matches(
        &mut self,
        addr1: &Address,
        addr2: &Address,
        pattern: &[u8],
        invert: bool,
    ) -> EdResult<Vec<u64>> {
        let (start, end) = self.resolve_range(addr1, addr2)?;
        let (_, re) = self.compile_re(pattern)?;
        let mut marked = Vec::new();
        for i in start..=end {
            if let Some(line) = self.buf.get_line(i) {
                if re.is_match_bytes(body(line)) != invert {
                    marked.extend(self.buf.line_id(i));
                }
            }
        }
        Ok(marked)
    }

    /// Run a `g`/`v` command list against each marked line still there.
    fn run_global_commands(
        &mut self,
        global_commands: &[GlobalCommand],
        marked: &[u64],
    ) -> EdResult<()> {
        // POSIX: when g completes, the current line is the one the last
        // command assigned.
        let mut last_line = self.buf.cur_line;
        let mut hint = 1;

        for &id in marked {
            let Some(line) = self.buf.line_of_id_near(id, hint) else {
                continue; // deleted or replaced by an earlier pass
            };
            hint = line;
            self.buf.set_cur_line(line)?;

            for gc in global_commands {
                // a/i/c take their text from the command list, so run them
                // here even when that text is empty.  POSIX lets the `.` be
                // omitted when it would be the last line of the list, which
                // makes a bare `a` an append of nothing.  Falling through to
                // input mode instead set `in_input_mode` with nothing to
                // service it: only the last match took effect, and the mode
                // leaked out of the global and ate the following commands
                // as input text.
                if is_input(&gc.parsed.cmd) {
                    let pending = self.resolve_input(&gc.parsed.cmd)?;
                    self.apply_input(pending, &gc.input_lines)?;
                    self.print_suffix(gc.parsed.suffix)?;
                } else {
                    // Abort the global on error and propagate.
                    self.run_parsed(gc.parsed.clone())?;
                }
                last_line = self.buf.cur_line;
            }
        }

        let _ = self.buf.set_cur_line(last_line);
        Ok(())
    }

    /// Execute an interactive global command (G or V).
    /// For each matching line: print it, read a command from user, execute.
    fn execute_global_interactive(
        &mut self,
        addr1: Address,
        addr2: Address,
        pattern: &[u8],
        invert: bool,
    ) -> EdResult<()> {
        let marked = self.mark_matches(&addr1, &addr2, pattern, invert)?;
        if marked.is_empty() {
            return Ok(());
        }

        // Save one undo record for the entire operation; see the note in
        // `execute_global` for why the pairing lives out here.
        self.buf.begin_global();
        let outcome = self.run_global_interactive(&marked);
        self.buf.end_global();
        outcome
    }

    /// Prompt for and run a command per marked line still there (`G`/`V`).
    fn run_global_interactive(&mut self, marked: &[u64]) -> EdResult<()> {
        // POSIX: `&` repeats the last command "within the current invocation
        // of G", never one from an earlier G.
        self.last_interactive_cmd = None;
        let mut hint = 1;
        for &id in marked {
            // Check for SIGINT
            if crate::SIGINT_RECEIVED.swap(false, Ordering::SeqCst) {
                writeln!(self.writer, "?")?;
                self.last_error = Some("Interrupt".to_string());
                return Ok(());
            }

            let Some(line_num) = self.buf.line_of_id_near(id, hint) else {
                continue; // deleted or replaced by an earlier command
            };
            hint = line_num;

            // Show the line the command will act on; it is the current line
            // from now, whatever is typed -- a null command included.
            if let Some(line) = self.buf.get_line(line_num) {
                self.writer.write_all(line)?;
            }
            self.writer.flush()?;
            self.buf.set_cur_line(line_num)?;

            // Read command from user
            let cmd_line = match self.read_line()? {
                Some(l) => l,
                None => break, // EOF
            };

            let trimmed = body(&cmd_line);

            // Empty line = skip, no action
            if trimmed.is_empty() {
                continue;
            }

            // & = repeat previous command
            let cmd_str = if trimmed == b"&" {
                self.last_interactive_cmd
                    .clone()
                    .ok_or_else(|| EdError::Generic("no previous command".to_string()))?
            } else {
                self.last_interactive_cmd = Some(trimmed.to_vec());
                trimmed.to_vec()
            };

            // A command that fails ends the G, as in historical ed: going on
            // would read the following input as answers to prompts the user
            // never saw.
            let parsed = parse(&cmd_str)?;
            // POSIX: in the interactive G/V prompt, any one command other
            // than a, c, i, g, G, v, V may be executed.
            if matches!(
                parsed.cmd,
                Command::Append(..)
                    | Command::Change(..)
                    | Command::Insert(..)
                    | Command::Global(..)
                    | Command::GlobalNot(..)
                    | Command::GlobalInteractive(..)
                    | Command::GlobalNotInteractive(..)
            ) {
                return Err(EdError::Generic("invalid command".to_string()));
            }
            self.run_parsed(parsed)?;
        }

        Ok(())
    }

    /// Check for and handle SIGINT signal.
    /// Returns true if SIGINT was received and handled.
    fn check_sigint(&mut self) -> io::Result<bool> {
        if crate::SIGINT_RECEIVED.swap(false, Ordering::SeqCst) {
            // POSIX: Print "?" and continue
            writeln!(self.writer, "?")?;
            self.last_error = Some("Interrupt".to_string());
            // If in input mode, exit input mode without completing the command
            if self.in_input_mode {
                self.in_input_mode = false;
                self.pending_input = None;
                self.input_lines.clear();
            }
            return Ok(true);
        }
        Ok(false)
    }

    /// Check for and handle SIGHUP signal.
    /// Returns true if SIGHUP was received (caller should exit).
    fn check_sighup(&mut self) -> bool {
        if crate::SIGHUP_RECEIVED.swap(false, Ordering::SeqCst) {
            // POSIX: If buffer is modified, attempt to save to ed.hup
            if self.buf.modified && self.buf.line_count() > 0 {
                self.save_hup_file();
            }
            return true;
        }
        false
    }

    /// Save buffer to ed.hup file per POSIX requirements.
    /// Tries current directory first, then $HOME.
    fn save_hup_file(&mut self) {
        use std::os::unix::ffi::OsStrExt;
        let paths_to_try = [
            Some(b"ed.hup".to_vec()),
            std::env::var_os("HOME").map(|h| [h.as_bytes(), b"/ed.hup"].concat()),
        ];

        for path_opt in paths_to_try.iter().flatten() {
            if self
                .buf
                .write_to_file(1, self.buf.line_count(), path_opt)
                .is_ok()
            {
                // Successfully saved
                return;
            }
        }
        // If we couldn't save anywhere, nothing more we can do
    }

    /// Handle end-of-file on the command stream.
    ///
    /// POSIX: in input mode, EOF terminates input mode and finalizes the
    /// pending `a`/`i`/`c` command with the lines collected so far; then EOF
    /// in command mode is equivalent to a `q` command — so if the buffer has
    /// unsaved changes and the user has not yet been warned, emit the warning
    /// (which also sets a non-zero exit status). There is no further input, so
    /// the caller terminates the session regardless.
    fn handle_eof(&mut self) -> io::Result<()> {
        if self.in_input_mode {
            self.in_input_mode = false;
            if let Some(pending) = self.pending_input.take() {
                let lines = std::mem::take(&mut self.input_lines);
                if let Err(e) = self.finish_input_command(pending, lines) {
                    self.print_error(&e)?;
                }
            }
        }
        if self.buf.modified && !self.quit_warning_given {
            self.quit_warning_given = true;
            self.print_error(&EdError::BufferModified)?;
        }
        Ok(())
    }

    /// Run the main editor loop.
    pub fn run(&mut self) -> io::Result<()> {
        loop {
            // Check for SIGHUP - exit if received
            if self.check_sighup() {
                break;
            }

            // Check for SIGINT - print "?" and continue
            self.check_sigint()?;

            self.print_prompt()?;

            let line = match self.read_line() {
                Ok(Some(l)) => l,
                Ok(None) => {
                    // A hangup closes the terminal, so the pending read ends
                    // in EOF rather than EINTR -- and `BufRead::read_line`
                    // retries EINTR internally anyway.  Check for the hangup
                    // before treating the EOF as a `q`, or the buffer ed is
                    // required to save to `ed.hup` is discarded instead.
                    if self.check_sighup() {
                        break;
                    }
                    // POSIX: end-of-file is equivalent to a `q` command.
                    self.handle_eof()?;
                    break;
                }
                // A signal interrupted the read.  Go back to the top of the
                // loop so the SIGHUP and SIGINT checks there can run --
                // propagating the error instead meant a hangup left `run`
                // without ever saving the buffer to `ed.hup`.
                Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
                Err(e) => return Err(e),
            };

            // Check signals again after potentially blocking on input
            if self.check_sighup() {
                break;
            }
            if self.check_sigint()? {
                continue; // SIGINT during input - restart loop
            }

            if !self.process_line(&line)? {
                break;
            }
        }

        // Flush buffered output before returning: the caller may terminate via
        // process::exit (for a non-zero status), which skips Drop and would
        // otherwise lose any output still sitting in a BufWriter.
        self.writer.flush()?;
        Ok(())
    }

    /// Load a file into the buffer.
    pub fn load_file(&mut self, path: &[u8]) -> EdResult<usize> {
        let bytes = self.buf.read_file(path)?;
        Ok(bytes)
    }
}

/// Flags and count parsed from an `s` command's suffix.
#[derive(Default)]
struct SubstituteSuffix {
    count: Option<usize>,
    global: bool,
    print: PrintFlags,
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
            .append(0, &[b"hello\n".to_vec(), b"world\n".to_vec()]);
        assert_eq!(editor.buf.line_count(), 2);
    }

    #[test]
    fn test_delete() {
        let mut editor = create_test_editor("");
        editor
            .buf
            .append(0, &[b"a\n".to_vec(), b"b\n".to_vec(), b"c\n".to_vec()]);
        editor.process_line(b"2d\n").unwrap();
        assert_eq!(editor.buf.line_count(), 2);
        assert_eq!(editor.buf.get_line(1), Some(&b"a\n"[..]));
        assert_eq!(editor.buf.get_line(2), Some(&b"c\n"[..]));
    }

    #[test]
    fn test_quit() {
        let mut editor = create_test_editor("");
        editor.process_line(b"q\n").unwrap();
        assert!(editor.should_quit);
    }

    #[test]
    fn test_prompt_toggle() {
        let mut editor = create_test_editor("");
        // Initially false
        assert!(!editor.show_prompt);

        // Toggle on
        editor.process_line(b"P\n").unwrap();
        assert!(editor.show_prompt, "P should toggle prompt ON");

        // Toggle off
        editor.process_line(b"P\n").unwrap();
        assert!(!editor.show_prompt, "P should toggle prompt OFF");

        // Toggle on again
        editor.process_line(b"P\n").unwrap();
        assert!(editor.show_prompt, "P should toggle prompt ON again");
    }
}
