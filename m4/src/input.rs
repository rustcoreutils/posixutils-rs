use std::cell::RefCell;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::rc::Rc;

use crate::EOF;

#[derive(Default)]
pub struct InputState {
    pub line_synchronization: bool,
    pub input: Vec<Input>,
}

impl InputState {
    pub fn new(line_synchronization: bool) -> Self {
        Self {
            line_synchronization,
            input: Vec::new(),
        }
    }

    pub fn input_push(
        &mut self,
        mut input: Input,
        syncline_output: &mut dyn Write,
    ) -> std::io::Result<()> {
        if self.line_synchronization {
            input.emit_syncline(syncline_output, false)?;
        }

        self.input.push(input);

        Ok(())
    }

    pub fn input_pop(&mut self) -> Option<Input> {
        self.input.pop()
    }

    /// Get the next character to be parsed. First it tries to get one from the pushback buffer,
    /// otherwise it gets one from the input file.
    pub fn get_next_character(&mut self) -> crate::error::Result<u8> {
        let input = self.input.last_mut().unwrap();
        if let Some(c) = input.pushback_buffer.pop() {
            return Ok(c);
        }
        Ok(input.get_next_character()?)
    }

    pub fn pushback_character(&mut self, c: u8) {
        self.input.last_mut().unwrap().pushback_buffer.push(c);
    }

    pub fn pushback_string(&mut self, s: &[u8]) {
        let pushback_buffer = &mut self.input.last_mut().unwrap().pushback_buffer;
        for c in s.iter().rev() {
            pushback_buffer.push(*c);
        }
    }

    /// Fetch new characters attempting to match them all to token. If any character doesn't match
    /// the token, then all the characters previously fetched are placed onto the pushback buffer.
    ///
    /// **NOTE:** Will panic if token is not length of at least 1.
    ///
    /// * `c` - First character of input which is already available.
    /// * `token` - Token to match against.
    pub fn look_ahead(&mut self, mut c: u8, token: &[u8]) -> crate::error::Result<bool> {
        if c == EOF || c != token[0] {
            return Ok(false);
        }

        let mut i = 1;
        while i < token.len() {
            c = self.get_next_character()?;
            if c == EOF || c != token[i] {
                loop {
                    self.pushback_character(token[i]);
                    if i == 0 {
                        break;
                    }
                    i -= 1;
                }
                return Ok(false);
            }

            i += 1;
        }

        Ok(true)
    }

    fn emit_syncline(
        &mut self,
        output: &mut dyn Write,
        check_line_numbers: bool,
    ) -> std::io::Result<()> {
        self.input
            .last_mut()
            .expect("at least one input")
            .emit_syncline(output, check_line_numbers)
    }
}

#[derive(Clone, Default)]
pub struct InputStateRef(Rc<RefCell<InputState>>);

impl InputStateRef {
    pub fn new(input_state: InputState) -> Self {
        Self(Rc::new(RefCell::new(input_state)))
    }

    pub fn with<F, T>(&self, f: F) -> T
    where
        F: Fn(&InputState) -> T,
    {
        f(&self.0.borrow())
    }

    pub fn input_pop(&self) -> Option<Input> {
        self.0.borrow_mut().input_pop()
    }

    pub fn input_push(&self, input: Input, syncline_output: &mut dyn Write) -> std::io::Result<()> {
        self.0.borrow_mut().input_push(input, syncline_output)
    }

    pub fn input_len(&self) -> usize {
        self.0.borrow().input.len()
    }

    pub fn get_next_character(&self) -> crate::error::Result<u8> {
        self.0.borrow_mut().get_next_character()
    }

    pub fn pushback_character(&self, c: u8) {
        self.0.borrow_mut().pushback_character(c)
    }

    pub fn pushback_string(&self, s: &[u8]) {
        self.0.borrow_mut().pushback_string(s)
    }

    pub fn look_ahead(&self, c: u8, token: &[u8]) -> crate::error::Result<bool> {
        self.0.borrow_mut().look_ahead(c, token)
    }

    pub fn emit_syncline(
        &mut self,
        output: &mut dyn Write,
        check_line_numbers: bool,
    ) -> std::io::Result<()> {
        self.0
            .borrow_mut()
            .emit_syncline(output, check_line_numbers)
    }

    pub fn sync_lines(&self) -> bool {
        self.0.borrow().line_synchronization
    }
}

pub struct Input {
    pub input: InputRead,
    pub pushback_buffer: Vec<u8>,
    pub line_number: usize,
    pub syncline_line_number: usize,
}

impl Input {
    pub fn new(input: InputRead) -> Self {
        Self {
            input,
            pushback_buffer: Vec::new(),
            line_number: 1,
            syncline_line_number: 0,
        }
    }

    fn get_next_character(&mut self) -> std::io::Result<u8> {
        let mut buf: [u8; 1] = [0; 1];
        let n = match &mut self.input {
            InputRead::File { file, .. } => file.read(&mut buf),
            InputRead::Stdin(s) => s.read(&mut buf),
        }?;

        if n == 0 {
            return Ok(EOF);
        }

        let c = buf[0];

        if c == b'\n' {
            self.line_number += 1;
        }

        Ok(c)
    }

    fn emit_syncline(
        &mut self,
        output: &mut dyn Write,
        check_line_numbers: bool,
    ) -> std::io::Result<()> {
        let name = match &self.input {
            InputRead::File { path, .. } => path.as_os_str().as_encoded_bytes(),
            InputRead::Stdin(_) => b"stdin",
        };

        log::debug!(
            "Input::emit_syncline(): {} {check_line_numbers}",
            String::from_utf8_lossy(name)
        );
        if check_line_numbers {
            log::debug!(
                "Input::emit_syncline(): syncline_line_number:{},line_number:{}",
                self.syncline_line_number,
                self.line_number
            );
            self.syncline_line_number += 1;
            if self.syncline_line_number == self.line_number {
                return Ok(());
            }
        }

        output.write_all(b"#line ")?;
        write!(output, "{}", self.line_number)?;
        output.write_all(b" \"")?;
        output.write_all(name)?;
        output.write_all(b"\"\n")?;

        self.syncline_line_number = self.line_number;
        Ok(())
    }
}

#[derive(Debug)]
pub enum InputRead {
    File { file: std::fs::File, path: PathBuf },
    Stdin(std::io::Stdin),
}
