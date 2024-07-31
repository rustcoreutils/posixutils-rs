//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuiltinFunction {
    Length,
    Sqrt,
    Scale,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Register {
    IBase,
    OBase,
    Scale,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StmtInstruction {
    Break,
    Quit,
    Return,
    ReturnExpr(ExprInstruction),
    If {
        condition: ConditionInstruction,
        instruction_count: usize,
        body: Vec<StmtInstruction>,
    },
    While {
        condition: ConditionInstruction,
        instruction_count: usize,
        body: Vec<StmtInstruction>,
    },
    For {
        init: ExprInstruction,
        condition: ConditionInstruction,
        update: ExprInstruction,
        instruction_count: usize,
        body: Vec<StmtInstruction>,
    },
    String(String),
    Expr(ExprInstruction),
    DefineFunction {
        name: char,
        function: Function,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum NamedExpr {
    VariableNumber(char),
    ArrayItem {
        name: char,
        index: Box<ExprInstruction>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionArgument {
    Expr(ExprInstruction),
    ArrayVariable(char),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprInstruction {
    Number(String),
    Named(NamedExpr),
    GetRegister(Register),
    Builtin {
        function: BuiltinFunction,
        arg: Box<ExprInstruction>,
    },
    PreIncrement(NamedExpr),
    PreDecrement(NamedExpr),
    PostIncrement(NamedExpr),
    PostDecrement(NamedExpr),
    Call {
        name: char,
        args: Vec<FunctionArgument>,
    },
    Assignment {
        named: NamedExpr,
        value: Box<ExprInstruction>,
    },
    SetRegister {
        register: Register,
        value: Box<ExprInstruction>,
    },
    UnaryMinus(Box<ExprInstruction>),
    Add(Box<ExprInstruction>, Box<ExprInstruction>),
    Sub(Box<ExprInstruction>, Box<ExprInstruction>),
    Mul(Box<ExprInstruction>, Box<ExprInstruction>),
    Div(Box<ExprInstruction>, Box<ExprInstruction>),
    Mod(Box<ExprInstruction>, Box<ExprInstruction>),
    Pow(Box<ExprInstruction>, Box<ExprInstruction>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConditionInstruction {
    Expr(ExprInstruction),
    Eq(ExprInstruction, ExprInstruction),
    Ne(ExprInstruction, ExprInstruction),
    Lt(ExprInstruction, ExprInstruction),
    Leq(ExprInstruction, ExprInstruction),
    Gt(ExprInstruction, ExprInstruction),
    Geq(ExprInstruction, ExprInstruction),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Variable {
    Number(char),
    Array(char),
}

/// A bc function.  
/// # Body and Source Locations
/// Statements in `body` map to source locations as
/// described in the documentation for `Program`.
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: char,
    pub parameters: Rc<[Variable]>,
    pub locals: Rc<[Variable]>,
    pub body: Rc<[StmtInstruction]>,
    pub source_locations: Rc<[usize]>,
    pub file: Rc<str>,
}

impl Default for Function {
    fn default() -> Self {
        Function {
            name: '\0',
            parameters: Rc::new([]),
            locals: Rc::new([]),
            body: Rc::new([]),
            source_locations: Rc::new([]),
            file: Rc::from(""),
        }
    }
}

/// A bc program
/// # Instructions and Source Locations
/// The `source_locations` field is a vector containing the line
/// numbers of the statements in the program. The index of a statement
/// is its index inside `instructions` if these were flattened into a
/// single vector, meaning every instruction inside a compound statement
/// (`if`, `while` or `for`) is also counted. `DefineFunction` does not
/// count as a statement in this context.
#[derive(Debug)]
pub struct Program {
    pub file: Rc<str>,
    pub instructions: Vec<StmtInstruction>,
    pub source_locations: Vec<usize>,
}

// This is used only for testing purposes
#[cfg(test)]
impl From<Vec<StmtInstruction>> for Program {
    fn from(instructions: Vec<StmtInstruction>) -> Self {
        Program {
            file: Rc::from(""),
            instructions,
            source_locations: vec![],
        }
    }
}
