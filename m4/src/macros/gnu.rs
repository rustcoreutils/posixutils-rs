//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The GNU m4 built-ins autoconf's m4sugar is written in: `builtin`,
//! `indir`, `regexp`, `patsubst`, `format`, `esyscmd`, `debugmode` and
//! `debugfile`.

use std::io::{Read, Write};
use std::os::unix::ffi::OsStrExt;
use std::rc::Rc;

use super::{BuiltinMacro, MacroDefinition, MacroImplementation};
use crate::debug;
use crate::emacs_regex::{Groups, Regex};
use crate::lexer::MacroName;
use crate::state::{StackFrame, State};
use crate::Result;

/// The frame for calling `definition` on all but the first argument of
/// `frame`, as `builtin` and `indir` do.
fn inner_frame(frame: StackFrame, definition: Rc<MacroDefinition>) -> StackFrame {
    let mut inner = StackFrame::new(0, definition, frame.location);
    inner.args = frame.args.into_iter().skip(1).collect();
    inner.builtin_args = frame
        .builtin_args
        .into_iter()
        .filter(|(i, _)| *i > 0)
        .map(|(i, builtin)| (i - 1, builtin))
        .collect();
    inner
}

/// `builtin(name, args...)`: call the built-in originally named `name`,
/// whatever it is called now and even if it is undefined.
pub struct BuiltinMacroCall;

impl MacroImplementation for BuiltinMacroCall {
    fn evaluate(&self, state: &mut State, stderr: &mut dyn Write, frame: StackFrame) -> Result<()> {
        let name = frame.args[0].clone();
        let Some(builtin) = BuiltinMacro::from_name(&name) else {
            let name = String::from_utf8_lossy(&name);
            state.emit_message(stderr, format_args!("undefined builtin `{name}'"))?;
            return Ok(());
        };
        let definition = Rc::new(MacroDefinition::new_builtin(MacroName(name), builtin));
        let frame = inner_frame(frame, definition.clone());
        definition.implementation.call(state, stderr, frame)
    }
}

/// `indir(name, args...)`: call the macro named `name`, which need not be a
/// name token.
pub struct IndirMacro;

impl MacroImplementation for IndirMacro {
    fn evaluate(&self, state: &mut State, stderr: &mut dyn Write, frame: StackFrame) -> Result<()> {
        let name = frame.args[0].as_slice();
        let Some(definition) = state
            .macro_definitions
            .get(name)
            .and_then(|stack| stack.last())
            .cloned()
        else {
            let name = String::from_utf8_lossy(name).into_owned();
            state.emit_message(stderr, format_args!("undefined macro `{name}'"))?;
            return Ok(());
        };
        let frame = inner_frame(frame, definition.clone());
        definition.implementation.call(state, stderr, frame)
    }
}

/// The compiled form of `pattern`, from the cache, or the diagnostic GNU m4
/// gives for a pattern that does not compile.
fn compile(state: &mut State, pattern: &[u8]) -> std::result::Result<Rc<Regex>, String> {
    if let Some(re) = state.regex_cache.get(pattern) {
        return Ok(re.clone());
    }
    let compiled = Regex::new(pattern).map(Rc::new).map_err(|why| {
        format!(
            "bad regular expression: `{}': {why}",
            String::from_utf8_lossy(pattern)
        )
    })?;
    state.regex_cache.insert(pattern.to_vec(), compiled.clone());
    Ok(compiled)
}

/// Expand `replacement` for the match `captures` in `victim`: `\N` is group
/// N, `\&` and `\0` the whole match, `\\` a backslash, and any other escaped
/// character itself.
fn substitute(
    state: &mut State,
    stderr: &mut dyn Write,
    out: &mut Vec<u8>,
    replacement: &[u8],
    victim: &[u8],
    captures: &Groups,
) -> Result<()> {
    let mut i = 0;
    while i < replacement.len() {
        let c = replacement[i];
        i += 1;
        if c != b'\\' {
            out.push(c);
            continue;
        }
        match replacement.get(i) {
            None => {
                state.emit_warning(stderr, format_args!("trailing \\ ignored in replacement"))?;
            }
            Some(&d @ b'0'..=b'9') => {
                let group = usize::from(d - b'0');
                if group >= captures.len() {
                    state
                        .emit_warning(stderr, format_args!("sub-expression {group} not present"))?;
                } else if let Some(span) = captures.get(group) {
                    out.extend_from_slice(&victim[span]);
                }
            }
            Some(b'&') => out.extend_from_slice(&victim[captures.whole()]),
            Some(&other) => out.push(other),
        }
        i += 1;
    }
    Ok(())
}

/// `regexp(string, regexp, [replacement])`: the byte offset of the first
/// match, or -1; or, given a replacement, it expanded for that match.
pub struct RegexpMacro;

impl MacroImplementation for RegexpMacro {
    fn evaluate(&self, state: &mut State, stderr: &mut dyn Write, frame: StackFrame) -> Result<()> {
        let mut args = frame.args.into_iter();
        let victim = args.next().unwrap_or_default();
        let pattern = args.next().unwrap_or_default();
        let replacement = args.next();
        let re = match compile(state, &pattern) {
            Ok(re) => re,
            Err(message) => {
                state.emit_message(stderr, format_args!("{message}"))?;
                return Ok(());
            }
        };
        let mut out = Vec::new();
        let captures = re.captures_at(&victim, 0);
        match (replacement, captures) {
            (None, Some(captures)) => {
                out.extend_from_slice(captures.whole().start.to_string().as_bytes())
            }
            (None, None) => out.extend_from_slice(b"-1"),
            (Some(replacement), Some(captures)) => {
                substitute(state, stderr, &mut out, &replacement, &victim, &captures)?;
            }
            (Some(_), None) => {}
        }
        state.input.pushback_string(&out);
        Ok(())
    }
}

/// `patsubst(string, regexp, [replacement])`: every match replaced.
pub struct PatsubstMacro;

impl MacroImplementation for PatsubstMacro {
    fn evaluate(&self, state: &mut State, stderr: &mut dyn Write, frame: StackFrame) -> Result<()> {
        let mut args = frame.args.into_iter();
        let victim = args.next().unwrap_or_default();
        let pattern = args.next().unwrap_or_default();
        let replacement = args.next().unwrap_or_default();
        // The empty pattern matches everywhere; with nothing to put there,
        // the string is unchanged.
        if pattern.is_empty() && replacement.is_empty() {
            state.input.pushback_string(&victim);
            return Ok(());
        }
        let re = match compile(state, &pattern) {
            Ok(re) => re,
            Err(message) => {
                state.emit_message(stderr, format_args!("{message}"))?;
                return Ok(());
            }
        };
        // As GNU m4 does: search from just past each match, keeping the
        // whole string as context for anchors and word boundaries, and step
        // over one character after an empty match.
        let mut out = Vec::with_capacity(victim.len());
        let mut offset = 0;
        while offset <= victim.len() {
            let Some(captures) = re.captures_at(&victim, offset) else {
                out.extend_from_slice(&victim[offset..]);
                break;
            };
            let whole = captures.whole();
            out.extend_from_slice(&victim[offset..whole.start]);
            substitute(state, stderr, &mut out, &replacement, &victim, &captures)?;
            offset = whole.end;
            if whole.is_empty() {
                if let Some(&c) = victim.get(offset) {
                    out.push(c);
                }
                offset += 1;
            }
        }
        state.input.pushback_string(&out);
        Ok(())
    }
}

/// `esyscmd(command)`: the standard output of the shell command, rescanned.
pub struct EsyscmdMacro;

impl MacroImplementation for EsyscmdMacro {
    fn evaluate(&self, state: &mut State, stderr: &mut dyn Write, frame: StackFrame) -> Result<()> {
        // The child writes to our standard error, and may read the files our
        // output and debug streams are going to.
        state.output.output.stdout().borrow_mut().flush()?;
        state.debug.flush()?;
        stderr.flush()?;
        let command = std::ffi::OsStr::from_bytes(&frame.args[0]);
        let mut child = std::process::Command::new("sh")
            .arg("-c")
            .arg(command)
            .stdout(std::process::Stdio::piped())
            .spawn()?;
        let mut output = Vec::new();
        child
            .stdout
            .take()
            .expect("piped")
            .read_to_end(&mut output)?;
        state.last_syscmd_status = Some(child.wait()?);
        state.input.pushback_string(&output);
        Ok(())
    }
}

/// `debugmode([flags])`: set the debug flags, add them with a leading `+`,
/// remove them with `-`; with no argument, clear them all.
pub struct DebugmodeMacro;

impl MacroImplementation for DebugmodeMacro {
    fn evaluate(&self, state: &mut State, stderr: &mut dyn Write, frame: StackFrame) -> Result<()> {
        let Some(arg) = frame.args.first() else {
            state.debug.flags = 0;
            return Ok(());
        };
        let (change, flags) = match arg.split_first() {
            Some((&c @ (b'+' | b'-'), rest)) => (Some(c), rest),
            _ => (None, arg.as_slice()),
        };
        match debug::parse_flags(flags) {
            Some(level) => match change {
                Some(b'+') => state.debug.flags |= level,
                Some(_) => state.debug.flags &= !level,
                None => state.debug.flags = level,
            },
            None => {
                let arg = String::from_utf8_lossy(arg).into_owned();
                state.emit_message(stderr, format_args!("Debugmode: bad debug flags: `{arg}'"))?;
            }
        }
        Ok(())
    }
}

/// `debugfile([file])`: send debug output to `file`, appending; nowhere for
/// an empty name; back to standard error with no argument.
pub struct DebugfileMacro;

impl MacroImplementation for DebugfileMacro {
    fn evaluate(&self, state: &mut State, stderr: &mut dyn Write, frame: StackFrame) -> Result<()> {
        let sink = match frame.args.first() {
            None => Ok(debug::Sink::Stderr),
            Some(name) => debug::Sink::open(name),
        };
        match sink {
            Ok(sink) => {
                state.debug.flush()?;
                state.debug.sink = sink;
            }
            Err(error) => {
                let name = String::from_utf8_lossy(&frame.args[0]).into_owned();
                let why = plib::diag::io_error_text(&error);
                state.emit_message(
                    stderr,
                    format_args!("cannot set debug file `{name}': {why}"),
                )?;
            }
        }
        Ok(())
    }
}

/// `format(format, args...)`: the arguments formatted as by C `printf`.
pub struct FormatMacro;

impl MacroImplementation for FormatMacro {
    fn evaluate(&self, state: &mut State, stderr: &mut dyn Write, frame: StackFrame) -> Result<()> {
        let mut args = frame.args.into_iter();
        let format = args.next().unwrap_or_default();
        let args: Vec<Vec<u8>> = args.collect();
        let out = match crate::format::format(&format, &args) {
            Ok(out) => out,
            Err((out, message)) => {
                state.emit_warning(stderr, format_args!("{message}"))?;
                out
            }
        };
        state.input.pushback_string(&out);
        Ok(())
    }
}
