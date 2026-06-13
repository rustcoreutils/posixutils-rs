//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::Write;

use crate::error::{Error, ErrorKind};
use crate::lexer::{is_alpha, is_space};
use crate::macros::MacroImplementation;
use crate::state::{StackFrame, State};
use crate::EOF;

/// Process the current input stack until it is exhausted, expanding macros and
/// writing to the output. Does NOT run m4wrap or flush diversions — those happen
/// once, at the true end of all input, in [`finalize`]. This lets the driver in
/// `run_impl` process file operands one at a time (interspersed with `-D`/`-U`)
/// while keeping a single end-of-input finalization.
pub(crate) fn process(mut state: State, stderr: &mut dyn Write) -> crate::error::Result<State> {
    let mut token: Vec<u8> = Vec::new();

    // TODO(style): rename these to something sensible.
    let mut l: u8 = 0;
    let mut t: u8;

    'main_loop: loop {
        t = state.input.get_next_character()?;
        // Strip quotes
        if state
            .input
            .look_ahead(t, &state.parse_config.quote_open_tag)?
        {
            log::trace!("Stripping quotes");
            let mut quotation_level: usize = 1;

            'inside_quote: loop {
                l = state.input.get_next_character()?;
                if state
                    .input
                    .look_ahead(l, &state.parse_config.quote_close_tag)?
                {
                    quotation_level -= 1;
                    if quotation_level > 0 {
                        // Encountered closing quote within the quote, so we output it.
                        state
                            .output
                            .write_all(&state.parse_config.quote_close_tag)?;
                    }
                } else if state
                    .input
                    .look_ahead(l, &state.parse_config.quote_open_tag)?
                {
                    quotation_level += 1;
                    state.output.write_all(&state.parse_config.quote_open_tag)?;
                } else if l == EOF {
                    return Err(crate::Error::new(crate::ErrorKind::UnclosedQuote));
                } else if quotation_level > 0 {
                    log::trace!(
                        "Writing quoted content to output: {:?}",
                        String::from_utf8_lossy(&[l])
                    );
                    state.output.write_all(&[l])?;
                }

                if quotation_level == 0 {
                    log::trace!("Finished stripping quotes");
                    break 'inside_quote;
                }
            }
        } else if state.output.stack.is_empty() // Parse comments
            && state.parse_config.comment_enabled && state
                .input
                .look_ahead(t, &state.parse_config.comment_open_tag)?
        {
            state
                .output
                .write_all(&state.parse_config.comment_open_tag)?;

            'inside_comment: loop {
                t = state.input.get_next_character()?;
                if state
                    .input
                    .look_ahead(t, &state.parse_config.comment_close_tag)?
                {
                    state
                        .output
                        .write_all(&state.parse_config.comment_close_tag)?;
                    break 'inside_comment;
                }
                if t == EOF {
                    break 'inside_comment;
                }
                state.output.write_all(&[t])?;
            }
        } else if t == b'_' || is_alpha(t) {
            // Possibly a macro to be evaluated.
            let definition = state.parse_macro(t, &mut token)?;
            if definition.is_some() {
                l = state.input.get_next_character()?;
                state.input.pushback_character(l);
            }

            // Check to see whether it's currently defined macro or it needs some arguments but
            // there's no open bracket.
            if let Some(definition) = definition {
                if l != b'(' && definition.parse_config.min_args > 0 {
                    state.output.write_all(&token)?;
                } else {
                    let mut frame = StackFrame::new(0, definition.clone());

                    if l == b'(' {
                        // A parenthesised call always has at least one (possibly
                        // empty) argument: POSIX makes $# "0 if the macro was
                        // invoked without being followed by a <left-parenthesis>,
                        // otherwise 1 more than the number of unquoted commas".
                        // Seed that first argument so `macro()` reports $#==1.
                        frame.args.push(Vec::new());
                        state.output.stack.push(frame);
                    } else {
                        state = definition.implementation.evaluate(state, stderr, frame)?;
                    }
                }
            } else {
                state.output.write_all(&token)?;
            }
        } else if t == EOF {
            if state.input.input_len() == 1 {
                if !state.output.stack.is_empty() {
                    return Err(Error::new(ErrorKind::UnclosedParenthesis));
                }
                break 'main_loop;
            }
            state.input.input_pop();
            log::debug!("EOF synclines");
            if state.input.sync_lines() {
                state
                    .input
                    .emit_syncline(&mut *state.output.output.stdout().borrow_mut(), false)?;
            }
            continue 'main_loop;
        } else if state.output.stack.is_empty() {
            // not in a macro
            state.output.write_all(&[t])?;
        } else {
            match t {
                b'(' => {
                    if state.output.stack.last_mut().unwrap().parenthesis_level > 0 {
                        state.output.write_all(&[t])?;
                    }
                    'skip_whitespace: loop {
                        l = state.input.get_next_character()?;
                        if is_space(l) {
                            if state.output.stack.last_mut().unwrap().parenthesis_level > 0 {
                                state.output.write_all(&[l])?;
                            }
                        } else {
                            break 'skip_whitespace;
                        }
                    }
                    state.input.pushback_character(l);
                    state.output.stack.last_mut().unwrap().parenthesis_level += 1;
                }
                b')' => {
                    state.output.stack.last_mut().unwrap().parenthesis_level -= 1;
                    if state.output.stack.last_mut().unwrap().parenthesis_level > 0 {
                        state.output.write_all(&[t])?;
                    } else {
                        // end of argument list
                        let frame = state.output.stack.pop().unwrap();
                        state = frame
                            .definition
                            .clone()
                            .implementation
                            .evaluate(state, stderr, frame)?;
                    }
                }
                b',' => {
                    if state.output.stack.last().unwrap().parenthesis_level == 1 {
                        // Skip spaces after comma
                        loop {
                            l = state.input.get_next_character()?;
                            if !is_space(l) {
                                break;
                            }
                        }
                        state.input.pushback_character(l);
                        let args = &mut state.output.stack.last_mut().unwrap().args;
                        if args.is_empty() {
                            args.push(Vec::new());
                        }
                        args.push(Vec::new());
                    } else {
                        state.output.write_all(&[t])?;
                    }
                }
                _ => {
                    // Output comment
                    if state
                        .input
                        .look_ahead(t, &state.parse_config.comment_open_tag)?
                    {
                        state
                            .output
                            .write_all(&state.parse_config.comment_open_tag)?;
                        'comment: loop {
                            t = state.input.get_next_character()?;
                            if t == EOF {
                                // TODO: What happens with multiple inputs?
                                break 'comment;
                            }
                            if state
                                .input
                                .look_ahead(t, &state.parse_config.comment_close_tag)?
                            {
                                state
                                    .output
                                    .write_all(&state.parse_config.comment_close_tag)?;
                                break 'comment;
                            }
                            state.output.write_all(&[t])?;
                        }
                    } else {
                        state.output.write_all(&[t])?;
                    }
                }
            }
        }
    }

    Ok(state)
}

/// Finalize processing at the true end of all input: run queued m4wrap text
/// (rescanned, in the order the m4wrap calls were made — POSIX), then flush any
/// remaining diversion buffers to standard output in numerical order.
pub(crate) fn finalize(mut state: State, stderr: &mut dyn Write) -> crate::error::Result<State> {
    // Wrapped text may itself expand macros, divert, or queue further m4wrap
    // text, so loop until the queue is empty.
    while !state.m4wrap.is_empty() {
        let wrapped: Vec<u8> = state.m4wrap.drain(..).flatten().collect();
        state.output.output.divert(0)?;
        state.input.pushback_string(&wrapped);
        state = process(state, stderr)?;
    }

    state.output.output.divert(0)?;
    state.output.output.undivert_all()?;

    Ok(state)
}
