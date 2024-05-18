//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

#[derive(Debug, PartialEq, Eq)]
pub enum BuiltinFunction {
    Length,
    Sqrt,
    Scale,
}

#[derive(Debug, PartialEq)]
pub enum StmtInstruction {
    Break,
    Quit,
    Return,
    ReturnExpr(ExprInstruction),
    If {
        condition: ConditionInstruction,
        body: Vec<StmtInstruction>,
    },
    While {
        condition: ConditionInstruction,
        body: Vec<StmtInstruction>,
    },
    String(String),
    Expr(ExprInstruction),
    DefineFunction {
        name: char,
        function: Function,
    },
}

#[derive(Debug, PartialEq)]
pub enum NamedExpr {
    Scale,
    IBase,
    OBase,
    VariableNumber(char),
    ArrayItem {
        name: char,
        index: Box<ExprInstruction>,
    },
}

#[derive(Debug, PartialEq)]
pub enum ExprInstruction {
    Number(f64),
    Named(NamedExpr),
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
        args: Vec<ExprInstruction>,
    },
    Assignment {
        name: char,
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

#[derive(Debug, PartialEq)]
pub enum ConditionInstruction {
    Expr(ExprInstruction),
    Eq(ExprInstruction, ExprInstruction),
    Ne(ExprInstruction, ExprInstruction),
    Lt(ExprInstruction, ExprInstruction),
    Leq(ExprInstruction, ExprInstruction),
    Gt(ExprInstruction, ExprInstruction),
    Geq(ExprInstruction, ExprInstruction),
}

#[derive(Debug, PartialEq)]
pub enum Variable {
    Number(char),
    Array(char),
}

#[derive(Default, Debug, PartialEq)]
pub struct Function {
    pub name: char,
    pub parameters: Vec<Variable>,
    pub locals: Vec<Variable>,
    pub body: Vec<StmtInstruction>,
}
