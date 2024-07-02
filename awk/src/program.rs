//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use core::fmt;

pub type VarId = u32;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    NotMatch,
    Concat,

    // check if the array with reference on top of the stack contains the value
    // preceding it. Pushed the result on the stack
    In,

    // unary operations
    Negate,
    Not,
    PostInc,
    PostDec,
    PreInc,
    PreDec,

    AsNumber,
    // push the value on top of the stack
    Dup,
    // pop the value from the stack
    Pop,

    // pushes on the stack a reference to the array element
    // with the given id. The index of the element is on top
    // of the stack.
    ArrayRef(u32),
    // pushes on the stack the value variable
    VarRef(u32),
    // pushes on the stack a reference to a field. The field number is on top of the stack
    FieldRef,
    // assign the value on top of the stack to the reference
    // preceding it. Leaves the assigned value on top of the stack
    Assign,

    LocalVarRef(u32),
    LocalArrayRef(u32),

    // delete an element from the array with reference on top of the stack
    // using the index preceding it
    Delete,

    // jump forwards or backwards by the given offset.
    // Offset 0 is the jump instruction
    JumpIfFalse(i32),
    JumpIfTrue(i32),
    Jump(i32),

    Call {
        id: u32,
        argc: u16,
    },
    CallBuiltin {
        function: BuiltinFunction,
        argc: u16,
    },

    // Push the constant value on top of the stack
    PushConstant(u32),
    // Push 1 on top of the stack
    PushOne,
    // Push the uninitialized value on top of the stack
    PushUninitialized,
    // Push the uninitialized scalar value on top of the stack
    PushUninitializedScalar,

    Print,

    Next,
    Exit,
    Return,

    // invalid opcode. Cannot be inside a valid program
    Invalid,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    Number(f64),
    String(String),
    Regex(String),
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Expr(Vec<OpCode>),
    Range {
        start: Vec<OpCode>,
        end: Vec<OpCode>,
    },
    All,
}

#[derive(Debug, PartialEq)]
pub struct AwkRule {
    pub pattern: Pattern,
    pub instructions: Vec<OpCode>,
}

#[derive(Clone, Copy)]
pub enum NameType {
    Var,
    Array,
    Function,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub parameters_count: usize,
    pub instructions: Vec<OpCode>,
}

pub struct Program {
    pub constants: Vec<Constant>,
    pub globals_count: usize,

    pub begin_instructions: Vec<OpCode>,
    pub rules: Vec<AwkRule>,
    pub end_instructions: Vec<OpCode>,
    pub functions: Vec<Function>,
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Program {{\n")?;
        write!(f, "  Constants: {:?}\n", self.constants)?;
        write!(f, "  Begin instructions: {:?}\n", self.begin_instructions)?;
        write!(f, "  Rules: {:?}\n", self.rules)?;
        write!(f, "  End instructions: {:?}\n", self.end_instructions)?;
        write!(f, "  Functions: {:?}\n", self.functions)?;
        write!(f, "}}")
    }
}

#[repr(u32)]
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
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    Gsub,
    Index,
    Length,
    Match,
    Split,
    Sprintf,
    Sub,
    Substr,
    ToLower,
    ToUpper,

    // I/O functions
    Close,
    GetLine,
    System,
    Print,
    Printf,

    /// the total number of builtin functions
    Count,
}
