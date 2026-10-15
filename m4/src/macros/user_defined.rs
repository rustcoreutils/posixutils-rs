//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::Write;

use crate::{
    state::{StackFrame, State},
    Result,
};

use super::MacroImplementation;

/// Arguments are positionally defined and referenced. The string "$1" in the defining text shall
/// be replaced by the first argument. Systems shall support at least nine arguments; only the
/// first nine can be referenced, using the strings "$1" to "$9", inclusive. The string "$0" is
/// replaced with the name of the macro. The string "$#" is replaced by the number of arguments as
/// a string. The string "$*" is replaced by a list of all of the arguments, separated by <comma>
/// characters. The string "$@" is replaced by a list of all of the arguments separated by <comma>
/// characters, and each argument is quoted using the current left and right quoting strings. The
/// string "${" produces unspecified behavior.
pub struct UserDefinedMacro {
    pub definition: Vec<u8>,
}

impl MacroImplementation for UserDefinedMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let definition = &self.definition;
        let args = &frame.args;
        let mut expansion = Vec::with_capacity(definition.len());
        let mut i = 0;
        while i < definition.len() {
            let c = definition[i];
            let next = definition.get(i + 1).copied();
            if c != b'$' || next.is_none() {
                expansion.push(c);
                i += 1;
                continue;
            }
            match next.expect("checked") {
                b'#' => expansion.extend_from_slice(args.len().to_string().as_bytes()),
                b'0'..=b'9' => {
                    // As in GNU m4, every digit after the `$` counts: `$10`
                    // is the tenth argument, as m4sugar's generated macros
                    // need, where POSIX reads `$1` followed by `0`.
                    let digits = definition[i + 1..]
                        .iter()
                        .take_while(|c| c.is_ascii_digit())
                        .count();
                    let n = definition[i + 1..i + 1 + digits]
                        .iter()
                        .fold(0usize, |n, d| {
                            n.saturating_mul(10).saturating_add(usize::from(d - b'0'))
                        });
                    match n {
                        0 => expansion.extend_from_slice(&frame.definition.parse_config.name.0),
                        n => {
                            if let Some(arg) = args.get(n - 1) {
                                expansion.extend_from_slice(arg);
                            }
                        }
                    }
                    i += 1 + digits;
                    continue;
                }
                b'*' => {
                    for (index, arg) in args.iter().enumerate() {
                        if index > 0 {
                            expansion.push(b',');
                        }
                        expansion.extend_from_slice(arg);
                    }
                }
                b'@' => {
                    for (index, arg) in args.iter().enumerate() {
                        if index > 0 {
                            expansion.push(b',');
                        }
                        expansion.extend_from_slice(&state.parse_config.quote_open_tag);
                        expansion.extend_from_slice(arg);
                        expansion.extend_from_slice(&state.parse_config.quote_close_tag);
                    }
                }
                // Not a parameter: the `$` is text, and what follows it is
                // looked at afresh, so `$$1` is `$` and the first argument.
                _ => {
                    expansion.push(b'$');
                    i += 1;
                    continue;
                }
            }
            i += 2;
        }
        state.input.pushback_string(&expansion);
        Ok(state)
    }
}
