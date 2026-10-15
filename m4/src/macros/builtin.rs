//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;
use std::process::ExitStatus;
use std::{io::Write, rc::Rc};

use super::eval::parse_integer;
use super::{MacroDefinitionImplementation, MacroImplementation};
use gettextrs::gettext;
use nom::error::{ContextError, FromExternalError};
use nom::IResult;

use crate::error::Result;
use crate::input::{Input, InputRead};
use crate::lexer::{
    MacroName, DEFAULT_COMMENT_CLOSE_TAG, DEFAULT_QUOTE_CLOSE_TAG, DEFAULT_QUOTE_OPEN_TAG,
};
use crate::macros::MacroDefinition;
use crate::output::DivertBufferNumber;
use crate::state::{StackFrame, State};
use crate::EOF;
use plib::locale::mb_char_slices;

/// Convert a byte offset within `bytes` to the index of the character that
/// begins at (or after) that offset, counting multibyte characters under the
/// current `LC_CTYPE`. Used by `index` to report a character (not byte)
/// position.
fn byte_offset_to_char_index(bytes: &[u8], byte_offset: usize) -> usize {
    let mut consumed = 0;
    let mut index = 0;
    for ch in mb_char_slices(bytes) {
        if consumed >= byte_offset {
            break;
        }
        consumed += ch.len();
        index += 1;
    }
    index
}

const AT_LEAST_ONE_MACRO_DEFINITION_EXPECT: &str =
    "There should always be at least one macro definition";

/// The dnl macro shall cause m4 to discard all input characters up to and including the next
/// `<newline>`.
pub struct DnlMacro;

impl MacroImplementation for DnlMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, _f: StackFrame) -> Result<State> {
        let mut c: u8;
        loop {
            c = state.input.get_next_character()?;
            if c == b'\n' || c == EOF {
                break;
            }
        }
        Ok(state)
    }
}

/// The second argument shall become the defining text of the macro whose name is the first
/// argument. It is unspecified whether the define macro deletes all prior definitions of the macro
/// named by its first argument or preserves all but the current definition of the macro. The
/// behavior is unspecified if define is not immediately followed by a `<left-parenthesis>`.
///
/// This particular implementation matches GNU m4 behaviour, and perserves all but the current
/// definition of the macro.
pub struct DefineMacro;

impl DefineMacro {
    /// The definition `define(name, text)` or `pushdef(name, text)` makes.
    ///
    /// Any string names a macro, as in GNU m4: one that is not a name token
    /// can never be called by name, but `defn`, `indir`, `ifdef` and the rest
    /// reach it, and m4sugar keeps its state in macros such as
    /// `_m4_divert(KILL)`. A missing text defines the macro as empty.
    fn define(frame: StackFrame) -> MacroDefinition {
        // A built-in token, `defn` of a built-in, defines that built-in
        // under the new name.
        let builtin = frame.builtin_arg(1);
        let mut args = frame.args.into_iter();
        let name = MacroName(args.next().unwrap_or_default());
        match builtin {
            Some(builtin) => MacroDefinition::new_builtin(name, builtin),
            None => MacroDefinition::new_user_defined(name, args.next().unwrap_or_default()),
        }
    }
}

impl MacroImplementation for DefineMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let definition = Rc::new(DefineMacro::define(frame));
        let name = definition.parse_config.name.clone();
        match state.macro_definitions.get_mut(&name) {
            Some(stack) => {
                *stack
                    .last_mut()
                    .expect(AT_LEAST_ONE_MACRO_DEFINITION_EXPECT) = definition
            }
            None => {
                state.macro_definitions.insert(name, vec![definition]);
            }
        }
        Ok(state)
    }
}

/// The pushdef macro shall be equivalent to the define macro with the exception that it shall
/// preserve any current definition for future retrieval using the popdef macro. The behavior is
/// unspecified if pushdef is not immediately followed by a `<left-parenthesis>`.
pub struct PushdefMacro;

impl MacroImplementation for PushdefMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let definition = Rc::new(DefineMacro::define(frame));
        let name = definition.parse_config.name.clone();
        state
            .macro_definitions
            .entry(name)
            .or_default()
            .push(definition);
        Ok(state)
    }
}

/// The popdef macro shall delete the current definition of its arguments, replacing that
/// definition with the previous one. If there is no previous definition, the macro is undefined.
/// The behavior is unspecified if popdef is not immediately followed by a `<left-parenthesis>`.
pub struct PopdefMacro;

impl MacroImplementation for PopdefMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        for arg in frame.args {
            let name = MacroName(arg);
            if let Some(stack) = state.macro_definitions.get_mut(&name) {
                stack.pop();
                // That was the last definition: the macro is now undefined.
                if stack.is_empty() {
                    state.macro_definitions.remove(&name);
                }
            }
        }
        Ok(state)
    }
}

/// The undefine macro shall delete all definitions (including those preserved using the pushdef
/// macro) of the macros named by its arguments. The behavior is unspecified if undefine is not
/// immediately followed by a ``left-parenthesis>`.
pub struct UndefineMacro;

impl MacroImplementation for UndefineMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        for arg in frame.args {
            state.macro_definitions.remove(&MacroName(arg));
        }
        Ok(state)
    }
}

/// The defining text of the defn macro shall be the quoted definition (using the current quoting
/// strings) of its arguments. The behavior is unspecified if defn is not immediately followed by a
/// `<left-parenthesis>`.
pub struct DefnMacro;

impl MacroImplementation for DefnMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        // The quoted definitions of every named macro, in order; an undefined
        // name contributes nothing. A built-in has no text, so it can only be
        // the whole result: with one argument it becomes the marker that
        // define/pushdef turn back into that built-in, and with several it is
        // dropped with a warning, as in GNU m4.
        let single = frame.args.len() == 1;
        let mut text = Vec::new();
        for arg in &frame.args {
            let name = MacroName(arg.clone());
            let Some(definition) = state.macro_definitions.get(&name).and_then(|d| d.last()) else {
                continue;
            };
            match &definition.implementation {
                MacroDefinitionImplementation::UserDefined(user_defined) => {
                    text.extend_from_slice(&state.parse_config.quote_open_tag);
                    text.extend_from_slice(&user_defined.definition);
                    text.extend_from_slice(&state.parse_config.quote_close_tag);
                }
                implementation => {
                    let builtin = implementation
                        .builtin()
                        .expect("a definition that is not user-defined is a built-in");
                    if single {
                        // With only one name, the text is empty.
                        state.input.pushback_builtin(builtin);
                    } else {
                        let msg = gettext("cannot concatenate builtin `{}'").replacen(
                            "{}",
                            &String::from_utf8_lossy(arg),
                            1,
                        );
                        state.emit_warning(stderr, format_args!("{msg}"))?;
                    }
                }
            }
        }
        state.input.pushback_string(&text);
        Ok(state)
    }
}

pub struct ErrprintMacro;

impl MacroImplementation for ErrprintMacro {
    fn evaluate(&self, state: State, stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let args_len = frame.args.len();
        for (i, arg) in frame.args.into_iter().enumerate() {
            stderr.write_all(&arg)?;
            if i < (args_len - 1) {
                stderr.write_all(b" ")?;
            }
        }
        Ok(state)
    }
}

/// The defining text for the include macro shall be the contents of the file named by the first
/// argument. It shall be an error if the file cannot be read. The behavior is unspecified if
/// include is not immediately followed by a `<left-parenthesis>`.
pub struct IncludeMacro;

impl IncludeMacro {
    /// Push the file named by the first argument as the next input, searched
    /// for along the include path. Returns the error that kept it from
    /// opening. A directory opens but cannot be read, so it is refused here,
    /// where `sinclude` can still ignore it.
    fn include(
        state: &State,
        frame: StackFrame,
    ) -> std::result::Result<(), (PathBuf, std::io::Error)> {
        let path = PathBuf::from(OsString::from_vec(
            frame.args.into_iter().next().unwrap_or_default(),
        ));
        let (file, found) = state.open_input(&path).map_err(|e| (path.clone(), e))?;
        if file.metadata().is_ok_and(|m| m.is_dir()) {
            return Err((path, std::io::Error::from_raw_os_error(libc::EISDIR)));
        }
        state
            .input
            .input_push(Input::new(InputRead::File { file, path: found }));
        Ok(())
    }
}

impl MacroImplementation for IncludeMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        // POSIX: "It shall be an error if the file cannot be read." GNU m4
        // diagnoses and continues with a non-zero exit status rather than
        // aborting.
        if let Err((path, error)) = Self::include(&state, frame) {
            let msg = gettext("cannot open `{}': {}")
                .replacen("{}", &path.display().to_string(), 1)
                .replacen("{}", &plib::diag::io_error_text(&error), 1);
            state.emit_error(stderr, format_args!("{msg}"))?;
        }
        Ok(state)
    }
}

/// The sinclude macro shall be equivalent to the [`IncludeMacro`], except that it shall not be an
/// error if the file is inaccessible. The behavior is unspecified if sinclude is not immediately
/// followed by a `<left-parenthesis>`.
pub struct SincludeMacro;

impl MacroImplementation for SincludeMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let _ = IncludeMacro::include(&state, frame);
        Ok(state)
    }
}

pub struct ChangecomMacro;

impl MacroImplementation for ChangecomMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        // GNU m4: an empty or missing start string disables comments, and a
        // missing end string is the <newline> (POSIX), not the previous one.
        let mut args = frame.args.into_iter();
        let open_tag = args.next().unwrap_or_default();
        if open_tag.is_empty() {
            state.parse_config.comment_enabled = false;
            return Ok(state);
        }
        state.parse_config.comment_enabled = true;
        state.parse_config.comment_open_tag = open_tag;
        state.parse_config.comment_close_tag = match args.next() {
            Some(close_tag) if !close_tag.is_empty() => close_tag,
            _ => DEFAULT_COMMENT_CLOSE_TAG.to_vec(),
        };
        Ok(state)
    }
}

pub struct ChangequoteMacro;

impl MacroImplementation for ChangequoteMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        // As GNU m4: no arguments restore the defaults; an empty start string
        // disables quoting; a non-empty one with no, or an empty, end string
        // gets the default end string.
        let mut args = frame.args.into_iter();
        let (open_tag, close_tag) = match args.next() {
            None => (
                DEFAULT_QUOTE_OPEN_TAG.to_vec(),
                DEFAULT_QUOTE_CLOSE_TAG.to_vec(),
            ),
            Some(open_tag) => match args.next() {
                Some(close_tag) if open_tag.is_empty() || !close_tag.is_empty() => {
                    (open_tag, close_tag)
                }
                _ => (open_tag, DEFAULT_QUOTE_CLOSE_TAG.to_vec()),
            },
        };
        state.parse_config.quote_open_tag = open_tag;
        state.parse_config.quote_close_tag = close_tag;
        Ok(state)
    }
}

/// The defining text of the incr macro shall be its first argument incremented by 1. It shall be
/// an error to specify an argument containing any non-numeric characters. The behavior is
/// unspecified if incr is not immediately followed by a `<left-parenthesis>`.
pub struct IncrMacro;

impl MacroImplementation for IncrMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        if let Some(first) = frame.args.into_iter().next() {
            match super::eval::padded(super::eval::parse_integer)(&first) {
                Ok((b"", number)) => state
                    .input
                    .pushback_string(number.wrapping_add(1).to_string().as_bytes()),
                // A non-numeric argument is a recoverable error (GNU m4): warn,
                // expand to nothing, set a non-zero exit status, and continue.
                _ => state.emit_error(
                    stderr,
                    format_args!("{}", gettext("non-numeric argument to builtin `incr'")),
                )?,
            }
        }
        Ok(state)
    }
}

/// The defining text of the decr macro shall be its first argument decremented by 1. It shall be
/// an error to specify an argument containing any non-numeric characters. The behavior is
/// unspecified if decr is not immediately followed by a `<left-parenthesis>`.
pub struct DecrMacro;

impl MacroImplementation for DecrMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        if let Some(first) = frame.args.into_iter().next() {
            match super::eval::padded(super::eval::parse_integer)(&first) {
                Ok((b"", number)) => state
                    .input
                    .pushback_string(number.wrapping_sub(1).to_string().as_bytes()),
                _ => state.emit_error(
                    stderr,
                    format_args!("{}", gettext("non-numeric argument to builtin `decr'")),
                )?,
            }
        }
        Ok(state)
    }
}

/// The ifelse macro takes three or more arguments. If the first two arguments compare as equal
/// strings (after macro expansion of both arguments), the defining text shall be the third
/// argument. If the first two arguments do not compare as equal strings and there are three
/// arguments, the defining text shall be null. If the first two arguments do not compare as equal
/// strings and there are four or five arguments, the defining text shall be the fourth argument.
/// If the first two arguments do not compare as equal strings and there are six or more arguments,
/// the first three arguments shall be discarded and processing shall restart with the remaining
/// arguments. The behavior is unspecified if ifelse is not immediately followed by a
/// `<left-parenthesis>`.
///
/// ifelse (string-1, string-2, equal, [not-equal])
/// ifelse (string-1, string-2, equal-1, string-3, string-4, equal-2, …, [not-equal])
pub struct IfelseMacro;

impl MacroImplementation for IfelseMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let mut args_len = frame.args.len();
        if args_len < 3 {
            // POSIX 103940 says ifelse "takes three or more arguments" and
            // leaves fewer unspecified, so the warning is a quality question
            // rather than a conformance one -- and GNU m4 draws the line at
            // exactly two. Two comparands with no branch is a call that meant
            // to do something and cannot; one argument, or none, reads as a
            // placeholder and expands to nothing in silence.
            if args_len == 2 {
                state.emit_warning(
                    stderr,
                    format_args!("{}", gettext("too few arguments to builtin `ifelse'")),
                )?;
            }
            return Ok(state);
        }

        let mut args = frame.args.into_iter();
        let mut i = 0;
        loop {
            // Both comparands arrive already expanded: collecting an argument
            // and expanding a macro are the same pass. `main_loop` pushes a
            // `StackFrame` on seeing `name(`, which reroutes `Output::write_all`
            // into the current argument buffer, so a nested call expands through
            // the ordinary path and its result lands in the argument. A quoted
            // argument skips that by construction. So comparing the strings here
            // is comparing expansions, which is what POSIX and GNU both specify.
            let arg_0 = args.next().expect("at least 3 args");
            let arg_1 = args.next().expect("at least 3 args");
            if arg_0 == arg_1 {
                log::debug!("IfelseMacro::evaluate() evaluating argument {}", i * 3 + 2);
                let arg = args.next().expect("at least 3 args");
                state.input.pushback_string(&arg);
                return Ok(state);
            } else {
                match args_len {
                    0..=2 => panic!("at least 3 args"),
                    3 => return Ok(state),
                    4 | 5 => {
                        args.next();
                        log::debug!("IfelseMacro::evaluate() evaluating argument {}", i * 3 + 3);
                        let arg = args.next().expect("at least 4 args");
                        state.input.pushback_string(&arg);
                        if args_len == 5 {
                            state.emit_warning(
                                stderr,
                                format_args!(
                                    "{}",
                                    gettext("excess arguments to builtin `ifelse' ignored")
                                ),
                            )?;
                        }
                        return Ok(state);
                    }
                    6.. => {
                        // the first three arguments shall be discarded and processing shall
                        // restart with the remaining arguments.
                        args.next();
                        args_len -= 3;
                    }
                }
            }
            i += 1;
        }
    }
}

/// If the first argument to the ifdef macro is defined, the defining text shall be the second
/// argument. Otherwise, the defining text shall be the third argument, if specified, or the null
/// string, if not. The behavior is unspecified if ifdef is not immediately followed by a
/// `<left-parenthesis>`.
pub struct IfdefMacro;

impl MacroImplementation for IfdefMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let mut args = frame.args.into_iter();
        let first_arg = args
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        let second_arg = args
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        if state.macro_definitions.contains_key(first_arg.as_slice()) {
            state.input.pushback_string(&second_arg);
        } else if let Some(third_arg) = args.next() {
            state.input.pushback_string(&third_arg);
        }
        log::debug!("IfdefMacro::evaluate() finished");
        Ok(state)
    }
}

/// The defining text for the shift macro shall be a comma-separated list of its arguments except
/// the first one. Each argument shall be quoted using the current quoting strings. The behavior is
/// unspecified if shift is not immediately followed by a `<left-parenthesis>`.
pub struct ShiftMacro;

impl MacroImplementation for ShiftMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        // Every argument but the first, each quoted, as POSIX says: an empty
        // one stays an argument, and none is expanded again.
        let mut text = Vec::new();
        for (i, arg) in frame.args.iter().skip(1).enumerate() {
            if i > 0 {
                text.push(b',');
            }
            text.extend_from_slice(&state.parse_config.quote_open_tag);
            text.extend_from_slice(arg);
            text.extend_from_slice(&state.parse_config.quote_close_tag);
        }
        state.input.pushback_string(&text);
        Ok(state)
    }
}

/// The defining text of the len macro shall be the length (as a string) of the first argument. The
/// behavior is unspecified if len is not immediately followed by a `<left-parenthesis>`.
pub struct LenMacro;

impl MacroImplementation for LenMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let first_arg = frame
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        // POSIX: the length is in characters, not bytes (LC_CTYPE).
        let len = mb_char_slices(&first_arg).len();
        state.input.pushback_string(len.to_string().as_bytes());
        Ok(state)
    }
}

/// The defining text of the index macro shall be the first character position (as a string) in the
/// first argument where a string matching the second argument begins (zero origin), or -1 if the
/// second argument does not occur. The behavior is unspecified if index is not immediately followed
/// by a `<left-parenthesis>`.
pub struct IndexMacro;

impl MacroImplementation for IndexMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let mut args = frame.args.into_iter();
        let first_arg = args
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        let second_arg = match args.next() {
            Some(second_arg) => second_arg,
            // Already diagnosed: `index(abc)` is 0.
            None => {
                state.input.pushback_character(b'0');
                return Ok(state);
            }
        };

        // An empty search string matches at position 0 (GNU m4); `windows(0)`
        // would otherwise panic.
        if second_arg.is_empty() {
            state.input.pushback_character(b'0');
            return Ok(state);
        }

        let byte_offset = first_arg
            .windows(second_arg.len())
            .position(|window| window == second_arg);
        match byte_offset {
            // POSIX: the position is a character index (zero origin), not a byte
            // offset (LC_CTYPE).
            Some(byte_offset) => {
                let char_index = byte_offset_to_char_index(&first_arg, byte_offset);
                state
                    .input
                    .pushback_string(char_index.to_string().as_bytes());
            }
            None => {
                state.input.pushback_string(b"-1");
            }
        }
        Ok(state)
    }
}

/// The defining text of the translit macro shall be the first argument with every character that
/// occurs in the second argument replaced with the corresponding character from the third argument.
/// If no replacement character is specified for some source character because the second argument
/// is longer than the third argument, that character shall be deleted from the first argument in
/// translit's defining text. The behavior is unspecified if the '-' character appears within the
/// second or third argument anywhere besides the first or last character. The behavior is
/// unspecified if the same character appears more than once in the second argument. The behavior is
/// unspecified if translit is not immediately followed by a `<left-parenthesis>`.
pub struct TranslitMacro;

/// Expand the ranges in a `translit` character list, as GNU m4 does: `a-z`
/// is every byte from `a` to `z`, a descending `z-a` runs backwards, and a
/// `-` that starts or ends the list is itself. POSIX leaves a `-` elsewhere
/// unspecified; autoconf writes ranges. A range's ends are single bytes; a
/// multibyte character next to a `-` makes it literal.
fn expand_ranges(chars: Vec<&[u8]>) -> Vec<Vec<u8>> {
    let mut out: Vec<Vec<u8>> = Vec::with_capacity(chars.len());
    let mut i = 0;
    while i < chars.len() {
        let range = match (chars.get(i), chars.get(i + 1), chars.get(i + 2)) {
            (Some(&&[from]), Some(&dash), Some(&&[to])) if dash == b"-" => Some((from, to)),
            _ => None,
        };
        // The end of one range starts the next: `a-c-e` is `a` to `e`.
        let continues = out.last().filter(|_| i > 0 && chars[i] == b"-");
        match (range, continues) {
            (Some((from, to)), _) => {
                push_range(&mut out, from, to);
                i += 3;
            }
            (None, Some(last))
                if last.len() == 1 && chars.get(i + 1).is_some_and(|c| c.len() == 1) =>
            {
                let (from, to) = (last[0], chars[i + 1][0]);
                out.pop();
                push_range(&mut out, from, to);
                i += 2;
            }
            _ => {
                out.push(chars[i].to_vec());
                i += 1;
            }
        }
    }
    out
}

/// Push every byte from `from` to `to`, inclusive, either direction.
fn push_range(out: &mut Vec<Vec<u8>>, from: u8, to: u8) {
    if from <= to {
        out.extend((from..=to).map(|b| vec![b]));
    } else {
        out.extend((to..=from).rev().map(|b| vec![b]));
    }
}

impl MacroImplementation for TranslitMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let mut args = frame.args.into_iter();
        let first_arg = args
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;

        let second_arg = match args.next() {
            Some(second_arg) => second_arg,
            // With no "from"/"to" sets there is nothing to transliterate: the
            // result is the first argument unchanged (GNU m4).
            None => {
                state.input.pushback_string(&first_arg);
                return Ok(state);
            }
        };

        let third_arg = args.next().unwrap_or_default();

        // Transliterate by character, not byte (LC_CTYPE): each source character
        // maps to the target character at the same position, or is deleted when
        // the target list is shorter.
        let from = expand_ranges(mb_char_slices(&second_arg));
        let to = expand_ranges(mb_char_slices(&third_arg));
        let mut output_buffer: Vec<u8> = Vec::with_capacity(first_arg.len());
        for ch in mb_char_slices(&first_arg) {
            match from.iter().position(|source| source.as_slice() == ch) {
                Some(pos) => {
                    if let Some(replacement) = to.get(pos) {
                        output_buffer.extend_from_slice(replacement);
                    }
                }
                None => output_buffer.extend_from_slice(ch),
            }
        }

        state.input.pushback_string(&output_buffer);
        Ok(state)
    }
}

/// The defining text for the substr macro shall be the substring of the first argument beginning at
/// the zero-offset character position specified by the second argument. The third argument, if
/// specified, shall be the number of characters to select; if not specified, the characters from
/// the starting point to the end of the first argument shall become the defining text. It shall not
/// be an error to specify a starting point beyond the end of the first argument and the defining
/// text shall be null. It shall be an error to specify an argument containing any non-numeric
/// characters. The behavior is unspecified if substr is not immediately followed by a
/// `<left-parenthesis>`.
pub struct SubstrMacro;

impl MacroImplementation for SubstrMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let mut args = frame.args.into_iter();
        let first_arg = args
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;

        // Indices and lengths are in characters, not bytes (LC_CTYPE).
        let chars = mb_char_slices(&first_arg);
        if chars.is_empty() {
            return Ok(state);
        }

        let start_index = match args.next() {
            Some(second_arg) => match nom::combinator::all_consuming(parse_index)(&second_arg) {
                Ok((_, i)) => i,
                Err(_) => {
                    state.emit_error(
                        stderr,
                        format_args!("{}", gettext("non-numeric argument to builtin `substr'")),
                    )?;
                    return Ok(state);
                }
            },
            None => 0,
        };

        // A starting point beyond the end is not an error; the result is null.
        if start_index >= chars.len() {
            return Ok(state);
        }

        let selected: &[&[u8]] = match args.next() {
            Some(third_arg) => match nom::combinator::all_consuming(parse_index)(&third_arg) {
                Ok((_, number_of_chars)) => {
                    if number_of_chars > 0 {
                        let end_index = usize::min(start_index + number_of_chars, chars.len());
                        &chars[start_index..end_index]
                    } else {
                        return Ok(state);
                    }
                }
                Err(_) => {
                    state.emit_error(
                        stderr,
                        format_args!("{}", gettext("non-numeric argument to builtin `substr'")),
                    )?;
                    return Ok(state);
                }
            },
            None => &chars[start_index..],
        };

        state.input.pushback_string(&selected.concat());
        Ok(state)
    }
}

/// The dumpdef macro shall write the defined text to standard error for each of the macros
/// specified as arguments, or, if no arguments are specified, for all macros.
pub struct DumpdefMacro;

impl MacroImplementation for DumpdefMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        // Collect first, then print sorted by name -- with no arguments every
        // macro, otherwise the named ones -- as GNU m4 does; autom4te reads
        // the list of built-ins out of a bare `dumpdef`.
        let current = |definitions: &Vec<Rc<MacroDefinition>>| {
            definitions
                .last()
                .expect(AT_LEAST_ONE_MACRO_DEFINITION_EXPECT)
                .clone()
        };
        let mut found: Vec<(MacroName, Rc<MacroDefinition>)> = Vec::new();
        if frame.args.is_empty() {
            for (name, definitions) in state.macro_definitions.iter() {
                found.push((name.clone(), current(definitions)));
            }
        }
        for arg in frame.args {
            let name = MacroName(arg);
            match state.macro_definitions.get(&name) {
                Some(definitions) => {
                    let definition = current(definitions);
                    found.push((name, definition));
                }
                None => {
                    let arg = &name.0;
                    let msg = gettext("undefined macro `{}'").replacen(
                        "{}",
                        &String::from_utf8_lossy(arg),
                        1,
                    );
                    state.emit_message(stderr, format_args!("{msg}"))?;
                }
            }
        }
        found.sort_by(|a, b| a.0 .0.cmp(&b.0 .0));

        // To the debug stream, as GNU m4 writes it; `-dq` quotes the text.
        let quote = state.debug.has(crate::debug::QUOTE);
        let mut text = Vec::new();
        for (name, definition) in found {
            text.extend_from_slice(&name.0);
            text.extend_from_slice(b":\t");
            match &definition.implementation {
                MacroDefinitionImplementation::UserDefined(user_defined) => {
                    if quote {
                        text.extend_from_slice(&state.parse_config.quote_open_tag);
                    }
                    text.extend_from_slice(&user_defined.definition);
                    if quote {
                        text.extend_from_slice(&state.parse_config.quote_close_tag);
                    }
                }
                implementation => {
                    let builtin = implementation
                        .builtin()
                        .expect("a definition that is not user-defined is a built-in");
                    text.push(b'<');
                    text.extend_from_slice(builtin.as_ref());
                    text.push(b'>');
                }
            }
            text.push(b'\n');
        }
        state.debug.write(stderr, &text)?;

        Ok(state)
    }
}

/// The defining text shall be as if it were the resulting pathname after a successful call to the
/// [`mkstemp()`](https://pubs.opengroup.org/onlinepubs/9699919799/functions/mkstemp.html) function
/// defined in the System Interfaces volume of POSIX.1-2017 called with the first argument to the
/// macro invocation. If a file is created, that file shall be closed. If a file could not be
/// created, the m4 utility shall write a diagnostic message to standard error and shall continue
/// processing input but its final exit status shall be non-zero; the defining text of the macro
/// shall be the empty string. The behavior is unspecified if mkstemp is not immediately followed by
/// a `<left-parenthesis>`.
pub struct MkstempMacro;

/// The mkstemp() function shall replace the contents of the `template` by a unique filename which
/// is returned. The string in template should look like a filename with six trailing 'X' s;
/// mkstemp() replaces each 'X' with a character from the portable filename character set. The
/// characters are chosen such that the resulting name does not duplicate the name of an existing
/// file at the time of a call to mkstemp().
fn mkstemp(mut template: Vec<u8>) -> Result<Vec<u8>> {
    if template.len() < 6 {
        return Err(crate::Error::new(crate::ErrorKind::Io).with_source(std::io::Error::other(
            format!("Unable to create temporary file, template should be greater than 6 characters long. template: {:?}", String::from_utf8_lossy(&template))
        )));
    }
    if &template[(template.len() - 6)..] != b"XXXXXX" {
        return Err(crate::Error::new(crate::ErrorKind::Io).with_source(std::io::Error::other(
            format!("Unable to create temporary file, template does not finish with six trailing 'X's. template: {:?}", String::from_utf8_lossy(&template))
        )));
    }
    let template_pointer = template.as_mut_ptr();
    // SAFETY: According to https://man7.org/linux/man-pages/man3/mkstemp.3.html it seems like this is correct.
    let file_descriptor = unsafe {
        // Docs: https://pubs.opengroup.org/onlinepubs/009604499/functions/mkstemp.html
        libc::mkstemp(template_pointer as *mut libc::c_char)
    };
    if file_descriptor < 0 {
        let e = errno::errno();
        return Err(
            crate::Error::new(crate::ErrorKind::Io).with_source(std::io::Error::other(format!(
                "Unable to create temporary file. template: {:?}, Error {}: {}",
                String::from_utf8_lossy(&template),
                e.0,
                e
            ))),
        );
    }
    // SAFETY: According to https://linux.die.net/man/2/close it seems like this is correct.
    let result = unsafe { libc::close(file_descriptor) };
    if result < 0 {
        let e = errno::errno();
        return Err(
            crate::Error::new(crate::ErrorKind::Io).with_source(std::io::Error::other(format!(
                "Unable to close temporary file. template: {:?}, Error {}: {}",
                String::from_utf8_lossy(&template),
                e.0,
                e
            ))),
        );
    }
    Ok(template)
}

impl MacroImplementation for MkstempMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let first_arg = frame
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        match mkstemp(first_arg) {
            Ok(pathname) => state.input.pushback_string(&pathname),
            Err(error) => {
                let msg = gettext("Error evaluating `mkstemp` macro: {}").replacen(
                    "{}",
                    &error.to_string(),
                    1,
                );
                write!(stderr, "{msg}")?;
                state.exit_error = true;
            }
        }
        Ok(state)
    }
}

/// Exit from the m4 utility. If the first argument is specified, it shall be the exit code. If no
/// argument is specified, the exit code shall be zero. It shall be an error to specify an argument
/// containing any non-numeric characters. If the first argument is zero or no argument is
/// specified, and an error has previously occurred (for example, a file operand that could not be
/// opened), it is unspecified whether the exit status is zero or non-zero.
pub struct M4exitMacro;

impl MacroImplementation for M4exitMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        if let Some(first_arg) = frame.args.into_iter().next() {
            let exit_code: i32 = match nom::combinator::all_consuming(parse_index)(&first_arg) {
                Ok((_, code)) => i32::try_from(code).unwrap_or(1),
                // GNU m4: a non-numeric argument is a recoverable error, but m4
                // still exits (with a failure status).
                Err(_) => {
                    state.emit_error(
                        stderr,
                        format_args!("{}", gettext("non-numeric argument to builtin `m4exit'")),
                    )?;
                    return Err(crate::Error::new(crate::ErrorKind::Exit(1)));
                }
            };
            if exit_code == 0 && state.exit_error {
                return Err(crate::Error::new(crate::ErrorKind::Exit(1)));
            } else {
                return Err(crate::Error::new(crate::ErrorKind::Exit(exit_code)));
            }
        }

        if state.exit_error {
            Err(crate::Error::new(crate::ErrorKind::Exit(1)))
        } else {
            Err(crate::Error::new(crate::ErrorKind::Exit(0)))
        }
    }
}

/// The first argument shall be processed when `EOF` is reached. If the `m4wrap` macro is used multiple
/// times, the arguments specified shall be processed in the order in which the `m4wrap` macros were
/// processed. The behavior is unspecified if `m4wrap` is not immediately followed by a
/// `<left-parenthesis>`.
pub struct M4wrapMacro;

impl MacroImplementation for M4wrapMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let first_arg = frame
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        let location = state.location();
        state.m4wrap.push((first_arg, location));
        Ok(state)
    }
}

/// The `syscmd` macro shall interpret its first argument as a shell command line. The defining text
/// shall be the string result of that command. The string result shall not be rescanned for macros
/// while setting the defining text. No output redirection shall be performed by the m4 utility. The
/// exit status value from the command can be retrieved using the sysval macro. The behavior is
/// unspecified if syscmd is not immediately followed by a `<left-parenthesis>`.
pub struct SyscmdMacro;

fn system(command: &[u8]) -> Result<ExitStatus> {
    let command = OsStr::from_bytes(command);
    // TODO(security): check security of this, for shell injection? It seems to be what the GNU m4
    // does in https://github.com/tar-mirror/gnu-m4/blob/master/src/builtin.c#L953
    let status = std::process::Command::new("sh")
        .arg("-c")
        .arg(command)
        .status()?;

    Ok(status)
}

impl MacroImplementation for SyscmdMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let first_arg = frame
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        // The child inherits our file descriptors and writes straight to fd 1,
        // while our own output is still sitting in a buffered writer. Flush
        // first so the child's output appears after the text we already
        // produced rather than jumping ahead of it.
        state.output.output.stdout().borrow_mut().flush()?;
        stderr.flush()?;
        let status = system(&first_arg)?;
        state.last_syscmd_status = Some(status);
        Ok(state)
    }
}

/// The defining text of the `sysval` macro shall be the exit value of the utility last invoked by the
/// [`SyscmdMacro`] (as a string).
pub struct SysvalMacro;

impl MacroImplementation for SysvalMacro {
    fn evaluate(&self, state: State, stderr: &mut dyn Write, _frame: StackFrame) -> Result<State> {
        if let Some(status) = state.last_syscmd_status {
            match status.code() {
                Some(code) => state.input.pushback_string(code.to_string().as_bytes()),
                None => write!(
                    stderr,
                    "{}",
                    gettext("Last syscmd exited without exit code (process terminated by signal)")
                )?,
            }
        }
        Ok(state)
    }
}

/// The m4 utility maintains nine temporary buffers, numbered 1 to 9, inclusive. When the last of
/// the input has been processed, any output that has been placed in these buffers shall be written
/// to standard output in buffer-numerical order. The `divert` macro shall divert future output to the
/// buffer specified by its argument. Specifying no argument or an argument of 0 shall resume the
/// normal output process. Output diverted to a stream with a negative number shall be discarded.
/// Behavior is implementation-defined if a stream number larger than 9 is specified. It shall be an
/// error to specify an argument containing any non-numeric characters.
pub struct DivertMacro;

impl MacroImplementation for DivertMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let divert_number = match frame.args.into_iter().next() {
            Some(first_arg) if !first_arg.is_empty() => {
                match nom::combinator::all_consuming(parse_integer)(&first_arg) {
                    Ok((_, divert_number)) => divert_number,
                    // GNU m4: a non-numeric argument is a recoverable error; the
                    // current diversion is left unchanged.
                    Err(_) => {
                        state.emit_error(
                            stderr,
                            format_args!("{}", gettext("non-numeric argument to builtin `divert'")),
                        )?;
                        return Ok(state);
                    }
                }
            }
            Some(_) => {
                state.emit_warning(
                    stderr,
                    format_args!(
                        "{}",
                        gettext("empty first argument to builtin `divert' treated as 0")
                    ),
                )?;
                0
            }
            None => 0,
        };
        state.output.output.divert(divert_number)?;
        Ok(state)
    }
}

/// The defining text of the divnum macro shall be the number of the current output stream as a string.
pub struct DivnumMacro;

impl MacroImplementation for DivnumMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, _frame: StackFrame) -> Result<State> {
        state
            .input
            .pushback_string(state.output.output.divert_number().to_string().as_bytes());
        Ok(state)
    }
}

/// The undivert macro shall cause immediate output of any text in temporary buffers named as
/// arguments, or all temporary buffers if no arguments are specified. Buffers can be undiverted
/// into other temporary buffers. Undiverting shall discard the contents of the temporary buffer.
/// The behavior is unspecified if an argument contains any non-numeric characters.
pub struct UndivertMacro;

// TODO(performance): rewrite to be more performant and remove the need to parse as `str`.
fn parse_index(input: &[u8]) -> IResult<&[u8], usize> {
    log::trace!("parse_index() {}", String::from_utf8_lossy(input));
    let (remaining, found) = nom::bytes::complete::take_while(|c: u8| c.is_ascii_digit())(input)?;
    let s = std::str::from_utf8(found).expect("Should be valid utf8 betweeen b'0' and b'9'");
    let i: usize = s.parse().map_err(|e| {
        let e = nom::error::Error::from_external_error(found, nom::error::ErrorKind::Digit, e);
        nom::Err::Error(nom::error::Error::add_context(
            found,
            "Error parsing user defined macro argument as an index",
            e,
        ))
    })?;
    log::trace!("parse_index() successfully parsed: {i}");

    Ok((remaining, i))
}

impl MacroImplementation for UndivertMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        // No arguments: flush every diversion buffer in numerical order.
        if frame.args.is_empty() {
            state.output.output.undivert_all()?;
            return Ok(state);
        }

        for arg in frame.args.into_iter() {
            match nom::combinator::all_consuming(parse_index)(&arg) {
                Ok((_, buffer_number)) => match DivertBufferNumber::try_from(buffer_number) {
                    Ok(n) => state.output.output.undivert(n)?,
                    Err(_) => {
                        let msg = gettext("invalid diversion number {} in builtin `undivert'")
                            .replacen("{}", &buffer_number.to_string(), 1);
                        state.emit_warning(stderr, format_args!("{msg}"))?;
                    }
                },
                // GNU m4: a non-numeric argument is a recoverable error.
                Err(_) => state.emit_warning(
                    stderr,
                    format_args!("{}", gettext("non-numeric argument to builtin `undivert'")),
                )?,
            }
        }
        Ok(state)
    }
}

/// `__file__`: the quoted name of the input the call began in.
pub struct FileMacro;

impl MacroImplementation for FileMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, _frame: StackFrame) -> Result<State> {
        let (name, _) = state.location();
        state
            .input
            .pushback_string(&state.parse_config.quote_close_tag);
        state.input.pushback_string(&name);
        state
            .input
            .pushback_string(&state.parse_config.quote_open_tag);
        Ok(state)
    }
}

/// `__line__`: the line the call began on.
pub struct LineMacro;

impl MacroImplementation for LineMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, _frame: StackFrame) -> Result<State> {
        let (_, line) = state.location();
        state.input.pushback_string(line.to_string().as_bytes());
        Ok(state)
    }
}
