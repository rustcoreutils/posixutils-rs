//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::*;
use crate::regex::regex_from_str;

const FIRST_GLOBAL_VAR: u32 = SpecialVar::Count as u32;

struct TestResult {
    globals: Vec<AwkValue>,
    execution_result: ExecutionResult,
    record: Record,
}

struct Test {
    instructions: Vec<OpCode>,
    constants: Vec<Constant>,
    functions: Vec<Function>,
    record: Record,
    globals_count: usize,
}

impl Test {
    fn new(instructions: Vec<OpCode>, constants: Vec<Constant>) -> Self {
        let globals_count = instructions
            .iter()
            .filter_map(|op| match op {
                OpCode::GlobalScalarRef(i) | OpCode::GetGlobal(i) => Some(i),
                _ => None,
            })
            .max()
            .copied()
            .unwrap_or(0)
            .saturating_sub(FIRST_GLOBAL_VAR - 1) as usize;
        Self {
            instructions: instructions.to_vec(),
            constants: constants.to_vec(),
            functions: Default::default(),
            record: Default::default(),
            globals_count,
        }
    }

    fn add_function(mut self, f: Function) -> Self {
        self.functions.push(f);
        self
    }

    fn add_record(mut self, record_string: &str) -> Self {
        self.record
            .reset(record_string.to_string(), &FieldSeparator::Default)
            .expect("could not split record");
        self
    }

    fn run_correct(mut self) -> TestResult {
        let mut stack = iter::repeat_with(|| StackValue::Invalid)
            .take(250)
            .collect::<Vec<StackValue>>();
        let mut interpreter = Interpreter::new(
            Array::default(),
            Array::default(),
            self.constants,
            self.globals_count,
        );
        let action = Action {
            debug_info: Default::default(),
            instructions: self.instructions,
        };

        let execution_result = interpreter
            .run(
                &action,
                &self.functions,
                &mut self.record,
                &mut stack,
                &mut GlobalEnv::default(),
                &mut EmptyRecordReader::default(),
            )
            .expect("execution generated an error");

        let globals = interpreter
            .globals
            .into_iter()
            .map(|v| v.into_inner())
            .collect();

        TestResult {
            execution_result,
            globals,
            record: self.record,
        }
    }
}

fn interpret_expr(instructions: Vec<OpCode>, constants: Vec<Constant>) -> AwkValue {
    Test::new(instructions, constants)
        .run_correct()
        .execution_result
        .unwrap_expr()
}

fn test_global(instructions: Vec<OpCode>, constants: Vec<Constant>) -> AwkValue {
    Test::new(instructions, constants).run_correct().globals[FIRST_GLOBAL_VAR as usize].clone()
}

fn interpret_with_function(
    main: Vec<OpCode>,
    constants: Vec<Constant>,
    function: Function,
) -> AwkValue {
    Test::new(main, constants)
        .add_function(function)
        .run_correct()
        .execution_result
        .unwrap_expr()
}

fn test_sprintf(format: &str, args: Vec<Constant>) -> String {
    let mut instructions = vec![OpCode::PushConstant(0)];
    let mut constants = vec![Constant::from(format)];
    let argc = args.len() + 1;
    for (i, c) in args.into_iter().enumerate() {
        instructions.push(OpCode::PushConstant(i as u32 + 1));
        constants.push(c);
    }
    instructions.push(OpCode::CallBuiltin {
        function: BuiltinFunction::Sprintf,
        argc: argc as u16,
    });
    let result = interpret_expr(instructions, constants);
    if let AwkValueVariant::String(s) = result.value {
        s.to_string()
    } else {
        panic!("expected string, got {:?}", result);
    }
}

#[test]
fn test_push_constant() {
    let instructions = vec![OpCode::PushConstant(0)];
    let constant = vec![Constant::Number(1.0)];
    assert_eq!(
        interpret_expr(instructions.clone(), constant),
        AwkValue::from(1.0)
    );

    let constant = vec![Constant::from("hello")];
    assert_eq!(
        interpret_expr(instructions, constant),
        AwkValue::from("hello".to_string())
    );
}

#[test]
fn test_add() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::Add,
    ];
    let constant = vec![Constant::Number(1.0), Constant::Number(1.0)];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(2.0));
}

#[test]
fn test_sub() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::Sub,
    ];
    let constant = vec![Constant::Number(145.0), Constant::Number(123.0)];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(22.0));
}

#[test]
fn test_mul() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::Mul,
    ];
    let constant = vec![Constant::Number(12.0), Constant::Number(12.0)];

    assert_eq!(
        interpret_expr(instructions, constant),
        AwkValue::from(144.0)
    );
}

#[test]
fn test_div() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::Div,
    ];
    let constant = vec![Constant::Number(144.0), Constant::Number(12.0)];

    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(12.0));
}

#[test]
fn test_mod() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::Mod,
    ];
    let constant = vec![Constant::Number(144.0), Constant::Number(12.0)];

    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(0.0));
}

#[test]
fn test_pow() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::Pow,
    ];
    let constant = vec![Constant::Number(2.0), Constant::Number(3.0)];

    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(8.0));
}

#[test]
fn test_string_concat() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::Concat,
    ];
    let constant = vec![Constant::from("hello"), Constant::from("world")];

    assert_eq!(
        interpret_expr(instructions, constant),
        AwkValue::from("helloworld".to_string())
    );
}

#[test]
fn test_compare_same_operand_type() {
    let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Le];
    let constant = vec![Constant::Number(2.0), Constant::Number(3.0)];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));

    let constant = vec![Constant::from("abcd"), Constant::from("efgh")];
    let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Ge];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(0.0));
}

#[test]
fn test_compare_number_string() {
    let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Le];
    let constant = vec![Constant::Number(2.0), Constant::from("2.")];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));

    let constant = vec![Constant::from("abcd"), Constant::Number(3.0)];
    let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Ge];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));
}

#[test]
fn test_compare_number_uninitialized() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::Ge,
    ];
    let constant = vec![Constant::Number(2.0)];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));
}

#[test]
fn test_interpret_in_for_global_array() {
    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::In,
    ];
    let constant = vec![Constant::from("key")];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(0.0));

    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::PushOne,
        OpCode::Assign,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::In,
    ];
    let constant = vec![Constant::from("key")];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));
}

#[test]
fn test_interpret_in_for_local_array_ref() {
    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::GetLocal(0),
        OpCode::PushConstant(0),
        OpCode::In,
    ];
    let constant = vec![Constant::from("key")];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(0.0));

    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::PushOne,
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GetLocal(0),
        OpCode::PushConstant(0),
        OpCode::In,
    ];
    let constant = vec![Constant::from("key")];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));
}

#[test]
fn test_negate() {
    let instructions = vec![OpCode::PushConstant(0), OpCode::Negate];
    let constant = vec![Constant::Number(456.0)];
    assert_eq!(
        interpret_expr(instructions, constant),
        AwkValue::from(-456.0)
    );
}

#[test]
fn test_not() {
    let instructions = vec![OpCode::PushConstant(0), OpCode::Not];
    let constant = vec![Constant::Number(0.0)];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));
}

#[test]
fn test_postinc() {
    let instructions = vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::PostInc];
    assert_eq!(test_global(instructions, vec![]), AwkValue::from(1.0));
}

#[test]
fn test_postdec() {
    let instructions = vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::PostDec];
    assert_eq!(test_global(instructions, vec![]), AwkValue::from(-1.0));
}

#[test]
fn test_preinc() {
    let instructions = vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::PreInc];
    assert_eq!(test_global(instructions, vec![]), AwkValue::from(1.0));
}

#[test]
fn test_predec() {
    let instructions = vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::PreDec];
    assert_eq!(test_global(instructions, vec![]), AwkValue::from(-1.0));
}

#[test]
fn test_assign_to_global_var() {
    let instructions = vec![
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::Assign,
    ];
    let constant = vec![Constant::Number(123.0)];
    assert_eq!(test_global(instructions, constant), AwkValue::from(123.0));
}

#[test]
fn test_assign_to_array_element() {
    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::PushConstant(1),
        OpCode::Assign,
    ];
    let constant = vec![Constant::from("key"), Constant::Number(123.0)];
    assert_eq!(
        test_global(instructions, constant),
        Array::from_iter([("key", 123.0)]).into()
    );
}

#[test]
fn test_delete_global_array_element_after_insertion() {
    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::PushConstant(1),
        OpCode::Assign,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::DeleteElement,
    ];
    let constant = vec![Constant::from("key"), Constant::Number(123.0)];
    assert_eq!(test_global(instructions, constant), Array::default().into());
}

#[test]
fn test_delete_global_array_element_after_insertion_through_local_ref() {
    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::PushConstant(1),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::GetLocal(0),
        OpCode::PushConstant(0),
        OpCode::DeleteElement,
    ];
    let constant = vec![Constant::from("key"), Constant::Number(123.0)];
    assert_eq!(test_global(instructions, constant), Array::default().into());
}

#[test]
fn test_delete_from_empty_global_array() {
    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::DeleteElement,
    ];
    let constant = vec![Constant::from("key")];
    assert_eq!(test_global(instructions, constant), Array::default().into());
}

#[test]
fn test_clear_array() {
    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::PushZero,
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(1),
        OpCode::IndexArrayGetRef,
        OpCode::PushZero,
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(2),
        OpCode::IndexArrayGetRef,
        OpCode::PushZero,
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::ClearArray,
    ];
    let constants = vec![
        Constant::from("key1"),
        Constant::from("key2"),
        Constant::from("key3"),
    ];
    let result = Test::new(instructions, constants).run_correct();
    assert_eq!(
        result.globals[FIRST_GLOBAL_VAR as usize],
        Array::default().into()
    );
}

#[test]
fn test_assign_to_local_var() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::LocalScalarRef(0),
        OpCode::PushConstant(1),
        OpCode::Assign,
    ];
    let constant = vec![Constant::Number(0.0), Constant::from("test string")];
    assert_eq!(
        interpret_expr(instructions, constant),
        AwkValue::from("test string".to_string())
    );
}

#[test]
fn test_assign_to_array_through_local_ref() {
    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::GetLocal(0),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::PushConstant(1),
        OpCode::Assign,
    ];
    let constant = vec![Constant::from("key"), Constant::Number(123.0)];
    assert_eq!(
        test_global(instructions, constant),
        Array::from_iter([("key", 123.0)]).into()
    );
}

#[test]
fn test_jump() {
    let instructions = vec![
        OpCode::Jump(2),
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
    ];
    let constant = vec![Constant::Number(1.0), Constant::Number(2.0)];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(2.0));
}

#[test]
fn test_jump_if_false() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::JumpIfFalse(2),
        OpCode::PushConstant(1),
        OpCode::PushConstant(2),
    ];
    let constant = vec![
        Constant::from(""),
        Constant::Number(1.0),
        Constant::Number(2.0),
    ];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(2.0));
}

#[test]
fn test_jump_if_true() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::JumpIfTrue(2),
        OpCode::PushConstant(1),
        OpCode::PushConstant(2),
    ];
    let constant = vec![
        Constant::Number(1.0),
        Constant::Number(2.0),
        Constant::Number(3.0),
    ];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(3.0));
}

#[test]
fn test_call_function_without_args() {
    let main = vec![OpCode::Call(0)];
    let function = Function {
        parameters_count: 0,
        instructions: vec![OpCode::PushConstant(0), OpCode::Return],
        ..Default::default()
    };
    let constant = vec![Constant::from("test")];
    assert_eq!(
        interpret_with_function(main, constant, function),
        AwkValue::from("test".to_string())
    );
}

#[test]
fn test_call_with_uninitialized_scalar_argument() {
    let main = vec![OpCode::PushUninitialized, OpCode::Call(0)];
    let functions = Function {
        parameters_count: 1,
        instructions: vec![OpCode::LocalScalarRef(0), OpCode::Return],
        ..Default::default()
    };
    assert_eq!(
        interpret_with_function(main, vec![], functions),
        AwkValue::uninitialized_scalar()
    );
}

#[test]
fn test_call_with_uninitialized_array_argument() {
    let main = vec![OpCode::PushUninitialized, OpCode::Call(0)];
    let function = Function {
        parameters_count: 1,
        instructions: vec![
            OpCode::GetLocal(0),
            OpCode::PushConstant(0),
            OpCode::IndexArrayGetValue,
            OpCode::Return,
        ],
        ..Default::default()
    };
    let constant = vec![Constant::from("key")];
    assert_eq!(
        interpret_with_function(main, constant, function),
        AwkValue::uninitialized_scalar()
    );
}

#[test]
fn test_call_function_with_scalar_argument() {
    let main = vec![OpCode::PushConstant(0), OpCode::Call(0)];
    let function = Function {
        parameters_count: 1,
        instructions: vec![OpCode::GetLocal(0), OpCode::PushOne, OpCode::Add],
        ..Default::default()
    };
    let constant = vec![Constant::Number(0.0)];
    assert_eq!(
        interpret_with_function(main, constant, function),
        AwkValue::from(1.0)
    );
}

#[test]
fn test_call_function_with_array_argument() {
    let main = vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::Call(0)];
    let function = Function {
        parameters_count: 1,
        instructions: vec![
            OpCode::GetLocal(0),
            OpCode::PushConstant(0),
            OpCode::IndexArrayGetRef,
            OpCode::PushOne,
            OpCode::Assign,
        ],
        ..Default::default()
    };
    let constants = vec![Constant::from("key")];
    assert_eq!(
        interpret_with_function(main, constants, function),
        AwkValue::from(1.0)
    );
}

#[test]
fn test_call_function_with_multiple_scalar_arguments() {
    let main = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(0),
        OpCode::PushConstant(0),
        OpCode::PushConstant(0),
        OpCode::PushConstant(0),
        OpCode::Call(0),
    ];
    let function = Function {
        parameters_count: 5,
        instructions: vec![
            OpCode::GetLocal(0),
            OpCode::GetLocal(1),
            OpCode::GetLocal(2),
            OpCode::GetLocal(3),
            OpCode::GetLocal(4),
            OpCode::Add,
            OpCode::Add,
            OpCode::Add,
            OpCode::Add,
            OpCode::Return,
        ],
        ..Default::default()
    };
    let constants = vec![Constant::Number(1.0)];
    assert_eq!(
        interpret_with_function(main, constants, function),
        AwkValue::from(5.0)
    );
}

#[test]
fn test_access_whole_record_field() {
    let instructions = vec![OpCode::PushConstant(0), OpCode::FieldRef];
    let constant = vec![Constant::Number(0.0)];
    let value = Test::new(instructions, constant)
        .add_record("hello")
        .run_correct()
        .execution_result
        .unwrap_expr();
    assert_eq!(value, "hello".into());
}

#[test]
fn test_assign_to_out_of_bounds_field() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::FieldRef,
        OpCode::PushConstant(0),
        OpCode::Assign,
    ];
    let constants = vec![Constant::Number(9.0)];

    let result = Test::new(instructions, constants)
        .add_record("1")
        .run_correct();

    assert_eq!(
        result.globals[SpecialVar::Nf as usize],
        AwkValue::from(9.0).into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Nf))
    );
}

#[test]
fn test_builtin_index() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Index,
            argc: 2,
        },
    ];
    let constant = vec![Constant::from("hello"), Constant::from("l")];
    assert_eq!(
        interpret_expr(instructions.clone(), constant),
        AwkValue::from(3.0)
    );

    let constant = vec![Constant::from("hello"), Constant::from("z")];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(0.0));
}

#[test]
fn test_builtin_length() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Length,
            argc: 1,
        },
    ];
    let constant = vec![Constant::from("hello")];
    assert_eq!(
        interpret_expr(instructions, constant.clone()),
        AwkValue::from(5.0)
    );

    let instructions = vec![
        OpCode::PushZero,
        OpCode::GetField,
        OpCode::CallBuiltin {
            function: BuiltinFunction::Length,
            argc: 1,
        },
    ];
    let value = Test::new(instructions, constant)
        .add_record("test record")
        .run_correct()
        .execution_result
        .unwrap_expr();
    assert_eq!(value, AwkValue::from(11.0));

    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::PushZero,
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(1),
        OpCode::IndexArrayGetRef,
        OpCode::PushZero,
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(2),
        OpCode::IndexArrayGetRef,
        OpCode::PushZero,
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Length,
            argc: 1,
        },
    ];
    let constants = vec![
        Constant::from("key1"),
        Constant::from("key2"),
        Constant::from("key3"),
    ];
    assert_eq!(interpret_expr(instructions, constants), AwkValue::from(3.0));
}

#[test]
fn test_builtin_substr() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::PushConstant(2),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Substr,
            argc: 3,
        },
    ];
    let constant = vec![
        Constant::from("hello"),
        Constant::Number(2.0),
        Constant::Number(2.0),
    ];
    assert_eq!(
        interpret_expr(instructions, constant),
        AwkValue::from("el".to_string())
    );

    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Substr,
            argc: 2,
        },
    ];
    let constant = vec![Constant::from("hello"), Constant::Number(3.0)];
    assert_eq!(
        interpret_expr(instructions, constant),
        AwkValue::from("llo".to_string())
    );
}

#[test]
fn test_builtin_sprintf_with_escaped_percent_sign() {
    assert_eq!(test_sprintf("%%", vec![]), "%");
}

#[test]
fn test_builtin_sprintf_with_string_args() {
    assert_eq!(test_sprintf("hello", vec![]), "hello");
    assert_eq!(
        test_sprintf("hello %s", vec![Constant::from("world")]),
        "hello world"
    );
    assert_eq!(
        test_sprintf(
            "%s:%s:%s",
            vec![
                Constant::from("a"),
                Constant::from("b"),
                Constant::from("c")
            ]
        ),
        "a:b:c"
    );
    assert_eq!(
        test_sprintf("%10s", vec![Constant::from("test")]),
        "      test"
    );
    assert_eq!(
        test_sprintf("%-10s", vec![Constant::from("test")]),
        "test      "
    );
    assert_eq!(test_sprintf("%.2s", vec![Constant::from("test")]), "te");
    assert_eq!(test_sprintf("%.20s", vec![Constant::from("test")]), "test");
    assert_eq!(
        test_sprintf("%10.2s", vec![Constant::from("test")]),
        "        te"
    );
    assert_eq!(
        test_sprintf("%-10.2s", vec![Constant::from("test")]),
        "te        "
    );
    assert_eq!(
        test_sprintf("%10.20s", vec![Constant::from("test")]),
        "      test"
    );
}

#[test]
fn test_builtin_sprintf_with_float_args_hex_format() {
    assert_eq!(
        test_sprintf("%a", vec![Constant::Number(255.34)]),
        "0x1.feae147ae147bp+7"
    );
    assert_eq!(
        test_sprintf("%.5a", vec![Constant::Number(255.34)]),
        "0x1.feae1p+7"
    );
    assert_eq!(
        test_sprintf("%35a", vec![Constant::Number(255.34)]),
        "               0x1.feae147ae147bp+7"
    );
    assert_eq!(
        test_sprintf("%-35a", vec![Constant::Number(255.34)]),
        "0x1.feae147ae147bp+7               "
    );
    assert_eq!(
        test_sprintf("float value: %a", vec![Constant::Number(49.67)]),
        "float value: 0x1.8d5c28f5c28f6p+5"
    );
    assert_eq!(
        test_sprintf("%10.3a", vec![Constant::Number(255.34)]),
        "0x1.feap+7"
    );
}

#[test]
fn test_builtin_sprintf_with_float_args() {
    assert_eq!(
        test_sprintf("%f", vec![Constant::Number(255.34)]),
        "255.340000"
    );
    assert_eq!(
        test_sprintf("%.1f", vec![Constant::Number(255.34)]),
        "255.3"
    );
    assert_eq!(
        test_sprintf("%15f", vec![Constant::Number(255.34)]),
        "     255.340000"
    );
    assert_eq!(
        test_sprintf("%-15f", vec![Constant::Number(255.34)]),
        "255.340000     "
    );
    assert_eq!(
        test_sprintf("float value: %f", vec![Constant::Number(49.67)]),
        "float value: 49.670000"
    );
    assert_eq!(
        test_sprintf("%10.3f", vec![Constant::Number(255.34)]),
        "   255.340"
    );
}

#[test]
fn test_builtin_sprintf_scientific_float() {
    assert_eq!(
        test_sprintf("%e", vec![Constant::Number(255.34)]),
        "2.553400e+02"
    );
    assert_eq!(
        test_sprintf("%.1e", vec![Constant::Number(255.34)]),
        "2.6e+02"
    );
    assert_eq!(
        test_sprintf("%15e", vec![Constant::Number(255.34)]),
        "   2.553400e+02"
    );
    assert_eq!(
        test_sprintf("%-15e", vec![Constant::Number(255.34)]),
        "2.553400e+02   "
    );
    assert_eq!(
        test_sprintf("float value: %e", vec![Constant::Number(49.67)]),
        "float value: 4.967000e+01"
    );
    assert_eq!(
        test_sprintf("%10.3e", vec![Constant::Number(255.34)]),
        " 2.553e+02"
    );
}

#[test]
fn test_builtin_sprintf_float_general() {
    assert_eq!(test_sprintf("%g", vec![Constant::Number(255.34)]), "255.34");
    assert_eq!(
        test_sprintf("%.1g", vec![Constant::Number(255.34)]),
        "3e+02"
    );
    assert_eq!(
        test_sprintf("%15g", vec![Constant::Number(255.34)]),
        "         255.34"
    );
    assert_eq!(
        test_sprintf("%-15g", vec![Constant::Number(255.34)]),
        "255.34         "
    );
    assert_eq!(
        test_sprintf("float value: %g", vec![Constant::Number(49.67)]),
        "float value: 49.67"
    );
    assert_eq!(
        test_sprintf("%10.3g", vec![Constant::Number(255.34)]),
        "       255"
    );
}

#[test]
fn test_builtin_sprintf_char() {
    assert_eq!(test_sprintf("%c", vec![Constant::Number(55.0)]), "7");
    assert_eq!(
        test_sprintf("%c", vec![Constant::Number(548.0)]),
        "\u{0224}"
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_match_op() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::Match,
    ];
    let constant = vec![
        Constant::from("hello"),
        Constant::Regex(Rc::new(
            Regex::new(CString::new("e").unwrap()).expect("failed to compile regex"),
        )),
    ];
    assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_builtin_match() {
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Match,
            argc: 2,
        },
    ];
    let constants = vec![
        Constant::from("this is a test"),
        Constant::Regex(Rc::new(regex_from_str("is* a"))),
    ];
    let result = Test::new(instructions, constants).run_correct();

    assert_eq!(result.execution_result.unwrap_expr(), AwkValue::from(6.0));
    assert_eq!(
        result.globals[SpecialVar::Rstart as usize],
        AwkValue::from(6.0)
    );
    assert_eq!(
        result.globals[SpecialVar::Rlength as usize],
        AwkValue::from(4.0)
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_builtin_split_with_split_ere() {
    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Split,
            argc: 3,
        },
    ];
    let constants = vec![
        Constant::from("a, b, c"),
        Constant::Regex(Rc::new(regex_from_str(","))),
    ];

    let global = test_global(instructions, constants);
    assert_eq!(
        global,
        Array::from_iter([("1", "a"), ("2", " b"), ("3", " c")]).into()
    );
}

#[test]
fn test_builtin_split_with_default_fs() {
    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Split,
            argc: 2,
        },
    ];
    let constants = vec![Constant::from("a b c")];

    let global = test_global(instructions, constants);
    assert_eq!(
        global,
        Array::from_iter([("1", "a"), ("2", "b"), ("3", "c")]).into()
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_builtin_sub() {
    let instructions = vec![
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(1),
        OpCode::PushConstant(2),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Sub,
            argc: 3,
        },
    ];
    let constants = vec![
        Constant::from("aaabbbabaabb"),
        Constant::Regex(Rc::from(regex_from_str("ab+"))),
        Constant::from("x"),
    ];

    let global = test_global(instructions, constants);
    assert_eq!(global, "aaxabaabb".into());
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_builtin_sub_on_record() {
    let instructions = vec![
        OpCode::PushZero,
        OpCode::FieldRef,
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Sub,
            argc: 2,
        },
    ];
    let constants = vec![
        Constant::Regex(Rc::from(regex_from_str("ab+"))),
        Constant::from("x"),
    ];
    let mut record = Test::new(instructions, constants)
        .add_record("aaabbb ab aabb")
        .run_correct()
        .record;
    assert_eq!(
        *record.fields[0].get_mut(),
        AwkValue::field_ref("aax ab aabb", 0)
    );
    assert_eq!(*record.fields[1].get_mut(), AwkValue::field_ref("aax", 1));
    assert_eq!(*record.fields[2].get_mut(), AwkValue::field_ref("ab", 2));
    assert_eq!(*record.fields[3].get_mut(), AwkValue::field_ref("aabb", 3));
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_builtin_gsub() {
    let instructions = vec![
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(1),
        OpCode::PushConstant(2),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Gsub,
            argc: 3,
        },
    ];
    let constants = vec![
        Constant::from("aaabbbabaabb"),
        Constant::Regex(Rc::from(regex_from_str("ab+"))),
        Constant::from("x"),
    ];

    let global = test_global(instructions, constants);
    assert_eq!(global, "aaxxax".into());
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_builtin_gsub_on_record() {
    let instructions = vec![
        OpCode::PushZero,
        OpCode::FieldRef,
        OpCode::PushConstant(0),
        OpCode::PushConstant(1),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Gsub,
            argc: 2,
        },
    ];
    let constants = vec![
        Constant::Regex(Rc::from(regex_from_str("ab+"))),
        Constant::from("x"),
    ];
    let mut record = Test::new(instructions, constants)
        .add_record("aaabbb ab aabb")
        .run_correct()
        .record;
    assert_eq!(
        *record.fields[0].get_mut(),
        AwkValue::field_ref("aax x ax", 0)
    );
    assert_eq!(*record.fields[1].get_mut(), AwkValue::field_ref("aax", 1));
    assert_eq!(*record.fields[2].get_mut(), AwkValue::field_ref("x", 2));
    assert_eq!(*record.fields[3].get_mut(), AwkValue::field_ref("ax", 3));
}

#[test]
fn test_iterate_through_empty_global_array() {
    let instructions = vec![
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR + 1),
        OpCode::CreateGlobalIterator(FIRST_GLOBAL_VAR),
        OpCode::AdvanceIterOrJump(2),
        OpCode::Jump(-1),
    ];
    let result = Test::new(instructions, vec![]).run_correct();
    assert_eq!(
        result.globals[FIRST_GLOBAL_VAR as usize],
        Array::default().into()
    );
    assert_eq!(
        result.globals[FIRST_GLOBAL_VAR as usize + 1],
        AwkValue::uninitialized_scalar()
    );
}

#[test]
fn test_iterate_through_global_array() {
    // ```
    // a["key1"] = "value"
    // a["key2"] = "value"
    // a["key3"] = "value"
    // for (a in array) {}
    //```
    let instructions = vec![
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(1),
        OpCode::IndexArrayGetRef,
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(2),
        OpCode::IndexArrayGetRef,
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(3),
        OpCode::IndexArrayGetRef,
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR + 1),
        OpCode::CreateGlobalIterator(FIRST_GLOBAL_VAR),
        OpCode::AdvanceIterOrJump(2),
        OpCode::Jump(-1),
    ];
    let constants = vec![
        Constant::from("value"),
        Constant::from("key1"),
        Constant::from("key2"),
        Constant::from("key3"),
    ];
    let result = Test::new(instructions, constants).run_correct();
    assert_eq!(
        result.globals[FIRST_GLOBAL_VAR as usize],
        Array::from_iter([("key1", "value"), ("key2", "value"), ("key3", "value")]).into()
    );
    assert_eq!(result.globals[FIRST_GLOBAL_VAR as usize + 1], "key3".into());
}

#[test]
fn test_iterate_through_empty_local_array() {
    let instructions = vec![
        OpCode::PushUninitialized,
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::CreateLocalIterator(0),
        OpCode::AdvanceIterOrJump(2),
        OpCode::Jump(-1),
    ];
    let result = Test::new(instructions, vec![]).run_correct();
    assert_eq!(
        result.execution_result.unwrap_expr(),
        Array::default().into()
    );
    assert_eq!(
        result.globals[FIRST_GLOBAL_VAR as usize],
        AwkValue::uninitialized_scalar()
    );
}

#[test]
fn test_iterate_through_local_array() {
    // ```
    // a["key1"] = "value"
    // a["key2"] = "value"
    // a["key3"] = "value"
    // for (a in array) {}
    //```
    let instructions = vec![
        OpCode::PushUninitialized,
        OpCode::LocalScalarRef(0),
        OpCode::PushConstant(1),
        OpCode::IndexArrayGetRef,
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::LocalScalarRef(0),
        OpCode::PushConstant(2),
        OpCode::IndexArrayGetRef,
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::LocalScalarRef(0),
        OpCode::PushConstant(3),
        OpCode::IndexArrayGetRef,
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::CreateLocalIterator(0),
        OpCode::AdvanceIterOrJump(2),
        OpCode::Jump(-1),
    ];
    let constants = vec![
        Constant::from("value"),
        Constant::from("key1"),
        Constant::from("key2"),
        Constant::from("key3"),
    ];
    let result = Test::new(instructions, constants).run_correct();
    assert_eq!(
        result.execution_result.unwrap_expr(),
        Array::from_iter([("key1", "value"), ("key2", "value"), ("key3", "value")]).into()
    );
    assert_eq!(result.globals[FIRST_GLOBAL_VAR as usize], "key3".into());
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_ere_outside_match_matches_record() {
    let instructions = vec![OpCode::PushConstant(0)];
    let constants = vec![Constant::Regex(Rc::new(regex_from_str("test")))];
    let result = Test::new(instructions, constants)
        .add_record("this is a test")
        .run_correct();
    let value = result.execution_result.unwrap_expr();
    assert_eq!(value.scalar_as_f64(), 1.0);
    assert_eq!(value.scalar_to_string("").unwrap(), "1".to_string().into());
}

#[test]
fn test_scalars_are_passed_by_value() {
    // ```
    // a = "test value"
    // b = "test value"
    // f(a, b)
    // ```
    let instructions = vec![
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR + 1),
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::GetGlobal(FIRST_GLOBAL_VAR + 1),
        OpCode::Call(0),
    ];
    // ```
    // function f(a, b) {
    //  a = 1;
    //  b = 1;
    // }
    // ```
    let function = Function {
        parameters_count: 2,
        instructions: vec![
            OpCode::LocalScalarRef(0),
            OpCode::PushOne,
            OpCode::Assign,
            OpCode::Pop,
            OpCode::LocalScalarRef(1),
            OpCode::PushOne,
            OpCode::Assign,
            OpCode::Pop,
            OpCode::PushUninitializedScalar,
            OpCode::Return,
        ],
        ..Default::default()
    };
    let constants = vec!["test value".into()];
    let result = Test::new(instructions, constants)
        .add_function(function)
        .run_correct();
    assert_eq!(
        result.globals[FIRST_GLOBAL_VAR as usize],
        "test value".into()
    );
    assert_eq!(
        result.globals[FIRST_GLOBAL_VAR as usize + 1],
        "test value".into()
    );
}

#[test]
fn test_arrays_are_passed_by_reference() {
    // ```
    // a["key"] = "value"
    // f(a)
    // ```
    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::PushConstant(1),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::Call(0),
    ];
    // ```
    // function f(a) {
    //  a["key"] = "new value"
    // }
    // ```
    let function = Function {
        parameters_count: 1,
        instructions: vec![
            OpCode::GetLocal(0),
            OpCode::PushConstant(0),
            OpCode::IndexArrayGetRef,
            OpCode::PushConstant(2),
            OpCode::Assign,
            OpCode::PushUninitializedScalar,
            OpCode::Return,
        ],
        ..Default::default()
    };
    let constants = vec![
        Constant::from("key"),
        Constant::from("value"),
        Constant::from("new value"),
    ];
    let result = Test::new(instructions, constants)
        .add_function(function)
        .run_correct();
    assert_eq!(
        result.globals[FIRST_GLOBAL_VAR as usize],
        Array::from_iter([("key", "new value")]).into()
    );
}

#[test]
fn test_changing_a_field_recomputes_the_record() {
    let instructions = vec![
        OpCode::PushOne,
        OpCode::FieldRef,
        OpCode::PushConstant(0),
        OpCode::Assign,
    ];
    let constants = vec![Constant::from("test")];

    let mut record = Test::new(instructions, constants)
        .add_record("a b")
        .run_correct()
        .record;
    assert_eq!(
        *record.fields[0].get_mut(),
        AwkValue::field_ref("test b", 0)
    );
    assert_eq!(*record.fields[1].get_mut(), AwkValue::field_ref("test", 1));
    assert_eq!(*record.fields[2].get_mut(), AwkValue::field_ref("b", 2));
}

#[test]
fn test_changing_the_record_recomputes_fields() {
    let instructions = vec![
        OpCode::PushZero,
        OpCode::FieldRef,
        OpCode::PushConstant(0),
        OpCode::Assign,
    ];
    let constants = vec![Constant::from("test")];

    let mut result = Test::new(instructions, constants)
        .add_record("a b")
        .run_correct();
    assert_eq!(*result.record.last_field.get_mut(), 1);
    assert_eq!(
        *result.record.fields[0].get_mut(),
        AwkValue::field_ref("test", 0)
    );
    assert_eq!(
        *result.record.fields[1].get_mut(),
        AwkValue::field_ref("test", 1)
    );
    assert_eq!(
        result.globals[SpecialVar::Nf as usize],
        AwkValue::from(1.0).into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Nf))
    );
}

#[test]
fn test_push_multiple_references_to_the_same_global_variable() {
    let instructions = vec![
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::PushConstant(1),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::PushConstant(2),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::PushConstant(3),
        OpCode::Assign,
        OpCode::Pop,
    ];
    let constants = vec![
        Constant::from("test1"),
        Constant::from("test2"),
        Constant::from("test3"),
        Constant::from("test4"),
    ];
    let result = Test::new(instructions, constants).run_correct();
    assert_eq!(result.globals[FIRST_GLOBAL_VAR as usize], "test4".into());
}

#[test]
fn test_push_multiple_references_to_the_same_stack_value() {
    let instructions = vec![
        OpCode::PushUninitialized,
        OpCode::LocalScalarRef(0),
        OpCode::LocalScalarRef(0),
        OpCode::LocalScalarRef(0),
        OpCode::LocalScalarRef(0),
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::PushConstant(1),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::PushConstant(2),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::PushConstant(3),
        OpCode::Assign,
        OpCode::Pop,
    ];
    let constants = vec![
        Constant::from("test1"),
        Constant::from("test2"),
        Constant::from("test3"),
        Constant::from("test4"),
    ];
    let result = Test::new(instructions, constants).run_correct();
    assert_eq!(result.execution_result.unwrap_expr(), "test4".into());
}

#[test]
fn test_push_multiple_references_to_the_same_record_field() {
    let instructions = vec![
        OpCode::PushOne,
        OpCode::FieldRef,
        OpCode::PushOne,
        OpCode::FieldRef,
        OpCode::PushOne,
        OpCode::FieldRef,
        OpCode::PushOne,
        OpCode::FieldRef,
        OpCode::PushConstant(0),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::PushConstant(1),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::PushConstant(2),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::PushConstant(3),
        OpCode::Assign,
        OpCode::Pop,
    ];
    let constants = vec![
        Constant::from("test1"),
        Constant::from("test2"),
        Constant::from("test3"),
        Constant::from("test4"),
    ];
    let mut result = Test::new(instructions, constants).run_correct();
    assert_eq!(
        *result.record.fields[1].get_mut(),
        AwkValue::field_ref("test4", 1)
    );
}

#[test]
fn test_push_multiple_references_to_the_same_array_element() {
    let instructions = vec![
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::GetGlobal(FIRST_GLOBAL_VAR),
        OpCode::PushConstant(0),
        OpCode::IndexArrayGetRef,
        OpCode::PushConstant(1),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::PushConstant(2),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::PushConstant(3),
        OpCode::Assign,
        OpCode::Pop,
        OpCode::PushConstant(4),
        OpCode::Assign,
        OpCode::Pop,
    ];
    let constants = vec![
        Constant::from("key"),
        Constant::from("test1"),
        Constant::from("test2"),
        Constant::from("test3"),
        Constant::from("test4"),
    ];
    let result = Test::new(instructions, constants).run_correct();
    assert_eq!(
        result.globals[FIRST_GLOBAL_VAR as usize],
        Array::from_iter([("key", "test4")]).into()
    );
}

#[test]
fn test_modifying_nf_recomputes_record() {
    let instructions = vec![
        OpCode::GlobalScalarRef(SpecialVar::Nf as u32),
        OpCode::PushOne,
        OpCode::Assign,
    ];
    let mut result = Test::new(instructions, vec![])
        .add_record("1 2 3 4")
        .run_correct();
    assert_eq!(*result.record.last_field.get_mut(), 1);
    assert_eq!(
        *result.record.fields[0].get_mut(),
        AwkValue::field_ref(maybe_numeric_string("1"), 0)
    );
    assert_eq!(
        *result.record.fields[1].get_mut(),
        AwkValue::field_ref(maybe_numeric_string("1"), 1)
    );
    assert_eq!(
        *result.record.fields[2].get_mut(),
        AwkValue::field_ref(AwkValue::uninitialized_scalar(), 2)
    );

    let instructions = vec![
        OpCode::GlobalScalarRef(SpecialVar::Nf as u32),
        OpCode::PushConstant(0),
        OpCode::Assign,
    ];
    let constants = vec![Constant::from(10.0)];
    let mut result = Test::new(instructions, constants)
        .add_record("1 2 3 4")
        .run_correct();
    assert_eq!(*result.record.last_field.get_mut(), 10);
    assert_eq!(
        *result.record.fields[0].get_mut(),
        AwkValue::field_ref(maybe_numeric_string("1 2 3 4      "), 0)
    );
}

#[test]
fn test_srand_returns_the_previous_seed_value() {
    let constants = vec![Constant::from(42.0)];
    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Srand,
            argc: 1,
        },
    ];
    let result = Test::new(instructions, constants.clone()).run_correct();
    assert_eq!(result.execution_result.unwrap_expr(), AwkValue::from(0.0));

    let instructions = vec![
        OpCode::PushConstant(0),
        OpCode::CallBuiltin {
            function: BuiltinFunction::Srand,
            argc: 1,
        },
        OpCode::CallBuiltin {
            function: BuiltinFunction::Srand,
            argc: 0,
        },
    ];
    let result = Test::new(instructions, constants).run_correct();
    assert_eq!(result.execution_result.unwrap_expr(), AwkValue::from(42.0));
}
