use std::io::Write;

use super::MacroImplementation;
use crate::lexer::MacroName;
use crate::state::{StackFrame, State};
use crate::Result;

pub struct TraceoffMacro;

impl MacroImplementation for TraceoffMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        if frame.args.is_empty() {
            state.trace = Trace::default();
        } else {
            for arg in frame.args {
                let exclude = MacroName::try_from_slice(&arg)?;
                state.trace.include.retain(|include| include != &exclude);
                state.trace.exclude.push(exclude);
            }
        }

        Ok(state)
    }
}

pub struct TraceonMacro;

impl MacroImplementation for TraceonMacro {
    fn evaluate(
        &self,
        mut state: State,
        _stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        if frame.args.is_empty() {
            state.trace.all = true;
            state.trace.exclude.clear();
        } else {
            for arg in frame.args {
                let include = MacroName::try_from_slice(&arg)?;
                state.trace.exclude.retain(|exclude| exclude != &include);
                state.trace.include.push(include);
            }
        }
        Ok(state)
    }
}

#[derive(Default)]
pub struct Trace {
    all: bool,
    exclude: Vec<MacroName>,
    include: Vec<MacroName>,
}

impl Trace {
    pub fn trace(
        &self,
        stack: &[StackFrame],
        current_frame: &StackFrame,
        stderr: &mut dyn Write,
    ) -> crate::Result<()> {
        let name = &current_frame.definition.parse_config.name;
        let level = stack.len() + 1;
        if (self.all && !self.exclude.contains(name)) || (!self.all && self.include.contains(name))
        {
            writeln!(stderr, "m4trace: -{level}- {name}")?;
        }
        Ok(())
    }
}
