//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Macro tracing, as GNU m4 does it: `traceon`, `traceoff`, `-t`/`--trace`,
//! and the `m4trace:` lines the debug flags shape.  autom4te and libtoolize
//! read those lines back, so their format is GNU m4's to the byte.

use std::collections::HashSet;
use std::io::Write;

use crate::{
    debug,
    state::{StackFrame, State},
    Result,
};

use super::MacroImplementation;

/// `traceoff([names...])`: stop tracing the named macros, or every macro.
pub struct TraceoffMacro;

impl MacroImplementation for TraceoffMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        if frame.args.is_empty() {
            state.trace.traced.clear();
        } else {
            for arg in frame.args {
                state.trace.traced.remove(&arg);
            }
        }
        Ok(state)
    }
}

/// `traceon([names...])`: trace the named macros, or every macro now
/// defined.
pub struct TraceonMacro;

impl MacroImplementation for TraceonMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        if frame.args.is_empty() {
            let names: Vec<Vec<u8>> = state
                .macro_definitions
                .keys()
                .map(|n| n.0.clone())
                .collect();
            state.trace.traced.extend(names);
        } else {
            state.trace.traced.extend(frame.args);
        }
        Ok(state)
    }
}

/// The names being traced.
///
/// Tracing belongs to the name, not the definition: a name traced before it
/// is defined, or traced and then redefined, undefined and defined again,
/// stays traced. autom4te passes `--trace` for macros the input has yet to
/// define.
#[derive(Default)]
pub struct Trace {
    pub traced: HashSet<Vec<u8>>,
}

/// What a traced call shows after its arguments, under the `e` flag: where
/// the expansion will be found once the macro has run.
pub struct PendingExpansion {
    /// The input whose pushback buffer receives the expansion.
    input_depth: usize,
    pushback_len: usize,
}

impl State {
    /// Whether the call `frame` is traced.
    pub fn is_traced(&self, frame: &StackFrame) -> bool {
        self.debug.has(debug::TRACE_ALL)
            || (!self.trace.traced.is_empty()
                && self
                    .trace
                    .traced
                    .contains(&frame.definition.parse_config.name.0))
    }

    /// Write the trace of the call `frame`, about to run, in GNU m4's form:
    ///
    /// `m4trace:FILE:LINE: -DEPTH- NAME(ARGS)`
    ///
    /// with the file under `f`, the line under `l` (both where the call
    /// began), the arguments under `a`, quoted under `q`, joined by `, `.
    /// Under `e` the line is finished by [`State::trace_expansion`].
    pub fn trace_call(
        &mut self,
        stderr: &mut dyn Write,
        frame: &StackFrame,
    ) -> Result<Option<PendingExpansion>> {
        let mut line = b"m4trace:".to_vec();
        if self.debug.has(debug::SHOW_FILE) {
            line.extend_from_slice(&frame.location.0);
            line.push(b':');
        }
        if self.debug.has(debug::SHOW_LINE) {
            line.extend_from_slice(frame.location.1.to_string().as_bytes());
            line.push(b':');
        }
        let depth = self.output.stack.len() + 1;
        line.extend_from_slice(format!(" -{depth}- ").as_bytes());
        line.extend_from_slice(&frame.definition.parse_config.name.0);
        if self.debug.has(debug::SHOW_ARGS) && !frame.args.is_empty() {
            line.push(b'(');
            // A built-in token argument shows as `<name>` where the macro
            // takes one; any other macro sees it as empty text.
            let shows_builtins = frame
                .definition
                .implementation
                .builtin()
                .is_some_and(|b| b.takes_builtin_args());
            for (i, arg) in frame.args.iter().enumerate() {
                if i > 0 {
                    line.extend_from_slice(b", ");
                }
                match frame.builtin_arg(i).filter(|_| shows_builtins) {
                    Some(builtin) => {
                        line.push(b'<');
                        line.extend_from_slice(builtin.as_ref());
                        line.push(b'>');
                    }
                    None => self.trace_value(&mut line, arg),
                }
            }
            line.push(b')');
        }
        if self.debug.has(debug::SHOW_EXPANSION) {
            self.debug.write(stderr, &line)?;
            let (input_depth, pushback_len) = self.input.pushback_position();
            return Ok(Some(PendingExpansion {
                input_depth,
                pushback_len,
            }));
        }
        line.push(b'\n');
        self.debug.write(stderr, &line)?;
        Ok(None)
    }

    /// Finish the trace line of a call that has run, showing what it
    /// expanded to: ` -> EXPANSION`.
    pub fn trace_expansion(
        &mut self,
        stderr: &mut dyn Write,
        pending: PendingExpansion,
    ) -> Result<()> {
        let expansion = self
            .input
            .pushed_since(pending.input_depth, pending.pushback_len);
        // GNU m4 shows no arrow for a call that expanded to nothing.
        let mut line = Vec::new();
        if !expansion.is_empty() {
            line.extend_from_slice(b" -> ");
            self.trace_value(&mut line, &expansion);
        }
        line.push(b'\n');
        self.debug.write(stderr, &line)?;
        Ok(())
    }

    /// Append an argument or expansion to a trace line, quoted under `q`.
    fn trace_value(&self, line: &mut Vec<u8>, value: &[u8]) {
        let quote = self.debug.has(debug::QUOTE);
        if quote {
            line.extend_from_slice(&self.parse_config.quote_open_tag);
        }
        line.extend_from_slice(value);
        if quote {
            line.extend_from_slice(&self.parse_config.quote_close_tag);
        }
    }
}
