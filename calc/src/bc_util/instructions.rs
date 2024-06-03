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
        body: Vec<StmtInstruction>,
    },
    While {
        condition: ConditionInstruction,
        body: Vec<StmtInstruction>,
    },
    For {
        init: ExprInstruction,
        condition: ConditionInstruction,
        update: ExprInstruction,
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

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: char,
    pub parameters: Rc<[Variable]>,
    pub locals: Rc<[Variable]>,
    pub body: Rc<[StmtInstruction]>,
}

impl Default for Function {
    fn default() -> Self {
        Function {
            name: '\0',
            parameters: Rc::new([]),
            locals: Rc::new([]),
            body: Rc::new([]),
        }
    }
}
