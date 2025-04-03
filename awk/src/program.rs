//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::HashMap;
use std::rc::Rc;

use crate::regex::Regex;

pub type VarId = u32;

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum OpCode {
    // binary operations
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Le,
    Lt,
    Ge,
    Gt,
    Eq,
    Ne,
    Match,
    Concat,

    // checks if the key on top of the stack is inside the array
    // preceding it
    In,

    // unary operations
    Negate,
    Not,
    PostInc,
    PostDec,
    PreInc,
    PreDec,

    CreateGlobalIterator(u32),
    CreateLocalIterator(u32),
    AdvanceIterOrJump(i32),

    AsNumber,
    // push the value on top of the stack
    Dup,
    // pop the value from the stack
    Pop,

    // pushes on the stack a scalar value or an array reference
    GetGlobal(u32),
    GetLocal(u32),
    GetField,
    // uses the key on top of the stack to index the array preceding it. Pushes the value
    // of the indexed element (adding it to the array if its not present)
    IndexArrayGetValue,

    GlobalScalarRef(u32),
    LocalScalarRef(u32),
    FieldRef,
    // similar to IndexArrayGetValue, but pushes a reference to the indexed element
    IndexArrayGetRef,

    // assign the value on top of the stack to the reference
    // preceding it. Leaves the assigned value on top of the stack
    Assign,

    // deletes the key on top of the stack from the array preceding it
    DeleteElement,
    // clears the array on top of the stack
    ClearArray,

    // jump forwards or backwards by the given offset.
    // Offset 0 is the jump instruction
    JumpIfFalse(i32),
    JumpIfTrue(i32),
    Jump(i32),

    Call(u32),
    CallBuiltin {
        function: BuiltinFunction,
        argc: u16,
    },

    // Push the constant value on top of the stack
    PushConstant(u32),
    // Push 1 on top of the stack
    PushOne,
    // Push 0 on top of the stack
    PushZero,
    // Push the uninitialized value on top of the stack
    PushUninitialized,
    // Push the uninitialized scalar value on top of the stack
    PushUninitializedScalar,

    Next,
    NextFile,
    Exit,
    Return,

    // invalid opcode. Cannot be inside a valid program
    Invalid,
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq)]
pub enum Constant {
    Number(f64),
    String(Rc<str>),
    Regex(Rc<Regex>),
}

impl From<&str> for Constant {
    fn from(value: &str) -> Self {
        Constant::String(Rc::from(value))
    }
}

impl From<f64> for Constant {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<Rc<Regex>> for Constant {
    fn from(value: Rc<Regex>) -> Self {
        Self::Regex(value)
    }
}

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Clone, Copy)]
pub struct SourceLocation {
    pub line: u32,
    pub column: u32,
}

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Default)]
pub struct DebugInfo {
    pub file: Rc<str>,
    pub source_locations: Vec<SourceLocation>,
}

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq)]
pub struct Action {
    pub instructions: Vec<OpCode>,
    pub debug_info: DebugInfo,
}

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq)]
pub enum Pattern {
    Expr(Action),
    Range { start: Action, end: Action },
    All,
}

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq)]
pub struct AwkRule {
    pub pattern: Pattern,
    pub action: Action,
}

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Default)]
pub struct Function {
    pub name: Rc<str>,
    pub parameters_count: usize,
    pub instructions: Vec<OpCode>,
    pub debug_info: DebugInfo,
}

pub struct Program {
    pub constants: Vec<Constant>,
    pub globals_count: usize,
    pub globals: HashMap<String, u32>,
    pub begin_actions: Vec<Action>,
    pub rules: Vec<AwkRule>,
    pub end_actions: Vec<Action>,
    pub functions: Vec<Function>,
}

#[cfg(test)]
impl core::fmt::Debug for Program {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        writeln!(f, "Program:")?;
        for action in &self.begin_actions {
            writeln!(f, "  BEGIN {{")?;
            for instruction in &action.instructions {
                writeln!(f, "    {instruction:?}")?;
            }
            writeln!(f, "  }}")?;
        }

        for rule in &self.rules {
            match &rule.pattern {
                Pattern::Expr(action) => {
                    writeln!(f, "  /")?;
                    for instruction in &action.instructions {
                        writeln!(f, "    {instruction:?}")?;
                    }
                    writeln!(f, "  / {{")?;
                }
                Pattern::Range { start, end } => {
                    writeln!(f, "  /")?;
                    for instruction in &start.instructions {
                        writeln!(f, "    {instruction:?}")?;
                    }
                    writeln!(f, "  /, /")?;
                    for instruction in &end.instructions {
                        writeln!(f, "    {instruction:?}")?;
                    }
                    writeln!(f, "  / {{")?;
                }
                Pattern::All => {
                    writeln!(f, "  {{")?;
                }
            }
            for instruction in &rule.action.instructions {
                writeln!(f, "    {instruction:?}")?;
            }
            writeln!(f, "  }}")?;
        }

        for action in &self.end_actions {
            writeln!(f, "  END {{")?;
            for instruction in &action.instructions {
                writeln!(f, "    {instruction:?}")?;
            }
            writeln!(f, "  }}")?;
        }
        writeln!(f, "Functions:")?;
        for function in &self.functions {
            writeln!(f, "  {}({}) {{", function.name, function.parameters_count)?;
            for instruction in &function.instructions {
                writeln!(f, "    {instruction:?}")?;
            }
            writeln!(f, "  }}")?;
        }
        writeln!(f, "Constants:")?;
        for (i, constant) in self.constants.iter().enumerate() {
            writeln!(f, "  {i}: {constant:?}")?;
        }
        Ok(())
    }
}

#[repr(u32)]
#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum SpecialVar {
    Argc,
    Argv,
    Convfmt,
    Environ,
    Filename,
    Fnr,
    Fs,
    Nf,
    Nr,
    Ofmt,
    Ofs,
    Ors,
    Rlength,
    Rs,
    Rstart,
    Subsep,

    /// the total number of special variables
    Count,
}

#[repr(u32)]
#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum BuiltinFunction {
    // arithmetic functions
    Atan2,
    Cos,
    Sin,
    Exp,
    Log,
    Sqrt,
    Int,
    Rand,
    Srand,

    // string functions
    /// The order of the arguments is the following:
    /// 1. The string to be searched
    /// 2. The regular expression to search for
    /// 3. The replacement string
    ///
    /// This ordering differs from the one specified in the source
    /// code, but it was chosen for simplicity of implementation
    Gsub,
    Index,
    Length,
    Match,
    /// The array is the first argument to the function.
    /// This is different from the order specified in the source code
    /// but it was chosen for simplicity of implementation
    Split,
    Sprintf,
    /// See Gsub for the order of the arguments
    Sub,
    Substr,
    ToLower,
    ToUpper,

    // I/O functions
    Close,
    FFlush,
    GetLine,
    GetLineFromFile,
    GetLineFromPipe,
    System,
    Print,
    Printf,
    RedirectedPrintTruncate,
    RedirectedPrintAppend,
    RedirectedPrintPipe,
    RedirectedPrintfTruncate,
    RedirectedPrintfAppend,
    RedirectedPrintfPipe,
}
