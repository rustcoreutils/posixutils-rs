use std::io::Write;

use super::MacroImplementation;
use crate::state::{StackFrame, State};
use crate::Result;

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
        log::debug!(
            "UserDefinedMacro::evaluate() evaluating {:?}: definition:{:?} args:{:?}",
            frame.definition.parse_config.name.to_string(),
            String::from_utf8_lossy(&self.definition),
            frame
                .args
                .iter()
                .map(|a| String::from_utf8_lossy(a))
                .collect::<Vec<_>>(),
        );

        if self.definition.is_empty() {
            return Ok(state);
        }

        let mut i = self.definition.len() - 1;
        loop {
            if i == 0 || self.definition[i - 1] != b'$' {
                state.input.pushback_character(self.definition[i]);
            } else {
                let t = self.definition[i];
                match t {
                    b'#' => state
                        .input
                        .pushback_string(frame.args.len().to_string().as_bytes()),
                    b'0' => state
                        .input
                        .pushback_string(&frame.definition.parse_config.name.0),
                    b'1'..=b'9' => {
                        let arg_index = (t - b'1') as usize;
                        if arg_index < frame.args.len() {
                            state.input.pushback_string(&frame.args[arg_index]);
                        }
                    }
                    b'*' => {
                        for (arg_index, arg) in frame.args.iter().enumerate().rev() {
                            state.input.pushback_string(arg);
                            if arg_index > 0 {
                                state.input.pushback_character(b',');
                            }
                        }
                    }
                    b'@' => {
                        for (arg_index, arg) in frame.args.iter().enumerate().rev() {
                            state
                                .input
                                .pushback_string(&state.parse_config.quote_close_tag);
                            state.input.pushback_string(arg);
                            state
                                .input
                                .pushback_string(&state.parse_config.quote_open_tag);
                            if arg_index > 0 {
                                state.input.pushback_character(b',');
                            }
                        }
                    }
                    _ => {
                        state.input.pushback_character(t);
                        state.input.pushback_character(b'$');
                    }
                }
                // TODO(performance): this might be able to skip an iteration with i==1?
                if i == 0 {
                    break;
                }
                i -= 1;
            }
            if i == 0 {
                break;
            }
            i -= 1;
        }

        Ok(state)
    }
}
