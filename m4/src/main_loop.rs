use std::io::Write;

use crate::error::{Error, ErrorKind};
use crate::lexer::{is_alpha, is_space};
use crate::macros::MacroImplementation;
use crate::state::{StackFrame, State};
use crate::EOF;

/// The main loop, the most important function in this program.
pub(crate) fn main_loop(
    mut state: State,
    stderr: &mut dyn Write,
) -> crate::error::Result<State> {
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
            if definition.is_none()
                || (l != b'(' && (definition.as_ref().unwrap().parse_config.min_args > 0))
            {
                state.output.write_all(&token)?;
            } else {
                let definition = definition.unwrap();

                let frame = StackFrame::new(0, definition.clone());

                if l == b'(' {
                    state.output.stack.push(frame);
                } else {
                    state = definition.implementation.evaluate(state, stderr, frame)?;
                }
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

    state.output.output.divert(0)?;
    state.output.output.undivert_all()?;

    for wrap in &state.m4wrap {
        state.output.write_all(wrap)?;
    }

    Ok(state)
}
