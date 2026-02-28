//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::cell::UnsafeCell;
use std::marker::PhantomData;
use std::rc::Rc;

use super::array::{KeyIterator, ValueIndex};
use super::value::{AwkRefType, AwkValue, AwkValueVariant};
use crate::program::{Action, Function, OpCode, SourceLocation};

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq)]
pub(crate) struct ArrayIterator {
    pub(crate) array: *mut AwkValue,
    pub(crate) iter_var: *mut AwkValue,
    pub(crate) key_iter: KeyIterator,
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq)]
pub(crate) struct ArrayElementRef {
    pub(crate) array: *mut AwkValue,
    pub(crate) value_index: ValueIndex,
}

pub(crate) enum StackValue {
    Value(UnsafeCell<AwkValue>),
    ValueRef(*mut AwkValue),
    ArrayElementRef(ArrayElementRef),
    UninitializedRef(*mut AwkValue),
    Iterator(ArrayIterator),
    Invalid,
}

impl StackValue {
    /// # Safety
    /// the caller has to ensure that the value is valid and dereferencable
    pub(crate) unsafe fn value_ref(&mut self) -> &mut AwkValue {
        match self {
            StackValue::Value(val) => val.get_mut(),
            StackValue::ValueRef(val_ref) => &mut **val_ref,
            StackValue::UninitializedRef(val_ref) => &mut **val_ref,
            StackValue::ArrayElementRef(array_element_ref) => (*array_element_ref.array)
                .as_array()
                .expect("expected array")
                .index_to_value(array_element_ref.value_index)
                .expect("invalid array value index"),
            _ => unreachable!("invalid stack value"),
        }
    }

    /// # Safety
    /// if the `StackValue` is an `ArrayElementRef`, the caller has to ensure that the
    /// array is dereferencable
    pub(crate) unsafe fn unwrap_ptr(self) -> *mut AwkValue {
        match self {
            StackValue::ValueRef(ptr) => ptr,
            StackValue::UninitializedRef(ptr) => ptr,
            StackValue::ArrayElementRef(array_element_ref) => unsafe {
                // safe by type invariance
                (*array_element_ref.array)
                    .as_array()
                    .expect("invalid array")
                    .index_to_value(array_element_ref.value_index)
                    .expect("invalid array value index")
            },
            _ => unreachable!("expected lvalue"),
        }
    }

    pub(crate) fn unwrap_array_iterator(self) -> ArrayIterator {
        match self {
            StackValue::Iterator(array_iterator) => array_iterator,
            _ => unreachable!("expected iterator"),
        }
    }

    /// # Safety
    /// pointers inside the `StackValue` have to be valid and dereferencable
    pub(crate) unsafe fn into_owned(self) -> AwkValue {
        match self {
            StackValue::Value(val) => val.into_inner(),
            StackValue::ValueRef(ref_val) => (*ref_val).clone().into_ref(AwkRefType::None),
            StackValue::UninitializedRef(_) => AwkValue::uninitialized_scalar(),
            _ => unreachable!("invalid stack value"),
        }
    }

    /// # Safety
    /// pointers inside the `StackValue` have to be valid and dereferencable
    pub(crate) unsafe fn ensure_value_is_scalar(&mut self) -> Result<(), String> {
        self.value_ref().ensure_value_is_scalar()
    }

    /// # Safety
    /// `value` has to be a valid pointer at least until the value preceding it
    /// on the stack is popped
    pub(crate) unsafe fn from_var(value: *mut AwkValue) -> Self {
        let value_ref = &mut *value;
        match value_ref.value {
            AwkValueVariant::Array(_) => StackValue::ValueRef(value),
            AwkValueVariant::Uninitialized => StackValue::UninitializedRef(value),
            _ => StackValue::Value(UnsafeCell::new(value_ref.clone())),
        }
    }

    pub(crate) fn duplicate(&mut self) -> Self {
        match self {
            StackValue::Value(val) => val.get_mut().clone().into(),
            StackValue::ValueRef(val_ref) => StackValue::ValueRef(*val_ref),
            StackValue::UninitializedRef(uninitialized_ref) => {
                StackValue::UninitializedRef(*uninitialized_ref)
            }
            StackValue::ArrayElementRef(array_element_ref) => {
                StackValue::ArrayElementRef(array_element_ref.clone())
            }
            StackValue::Iterator(iterator) => StackValue::Iterator(iterator.clone()),
            StackValue::Invalid => StackValue::Invalid,
        }
    }
}

impl From<AwkValue> for StackValue {
    fn from(value: AwkValue) -> Self {
        StackValue::Value(UnsafeCell::new(value))
    }
}

pub(crate) struct CallFrame<'i> {
    pub(crate) function_name: Rc<str>,
    pub(crate) function_file: Rc<str>,
    pub(crate) source_locations: &'i [SourceLocation],
    pub(crate) bp: *mut StackValue,
    pub(crate) sp: *mut StackValue,
    pub(crate) ip: isize,
    pub(crate) instructions: &'i [OpCode],
}

/// # Invariants
/// - `sp` and `bp` are pointers into the same
///   contiguously allocated chunk of memory
/// - `stack_end` is one past the last valid pointer
///   of the allocated memory starting at `bp`
/// - values in the range [`bp`, `sp`) can be accessed safely
pub(crate) struct Stack<'i, 's> {
    pub(crate) current_function_name: Rc<str>,
    pub(crate) current_function_file: Rc<str>,
    pub(crate) ip: isize,
    pub(crate) instructions: &'i [OpCode],
    pub(crate) source_locations: &'i [SourceLocation],
    pub(crate) sp: *mut StackValue,
    pub(crate) bp: *mut StackValue,
    pub(crate) stack_end: *mut StackValue,
    pub(crate) call_frames: Vec<CallFrame<'i>>,
    pub(crate) _stack_lifetime: PhantomData<&'s ()>,
}

/// Safe interface to work with the program stack.
impl<'i, 's> Stack<'i, 's> {
    /// pops the `StackValue` on top of the stack.
    /// # Returns
    /// The top stack value if there is one. `None` otherwise
    pub(crate) fn pop(&mut self) -> Option<StackValue> {
        if self.sp != self.bp {
            let mut value = StackValue::Invalid;
            self.sp = unsafe { self.sp.sub(1) };
            unsafe { core::ptr::swap(&mut value, self.sp) };
            Some(value)
        } else {
            None
        }
    }

    /// pushes a StackValue on top of the stack
    /// # Errors
    /// returns an error in case of stack overflow
    /// # Safety
    /// `value` has to be valid at least until the value preceding it is popped
    pub(crate) unsafe fn push(&mut self, value: StackValue) -> Result<(), String> {
        if self.sp == self.stack_end {
            Err("stack overflow".to_string())
        } else {
            *self.sp = value;
            self.sp = self.sp.add(1);
            Ok(())
        }
    }

    pub(crate) fn pop_scalar_value(&mut self) -> Result<AwkValue, String> {
        let mut value = self.pop().expect("empty stack");
        // safe by type invariance
        unsafe {
            value.ensure_value_is_scalar()?;
            Ok(value.into_owned())
        }
    }

    pub(crate) fn get_mut_value_ptr(&mut self, index: usize) -> Option<*mut AwkValue> {
        if unsafe { self.sp.offset_from(self.bp) } >= index as isize {
            let value = unsafe { &*self.bp.add(index) };
            match value {
                StackValue::Value(val) => Some(val.get()),
                StackValue::ValueRef(val_ref) => Some(*val_ref),
                StackValue::UninitializedRef(val_ref) => Some(*val_ref),
                _ => unreachable!("invalid stack value"),
            }
        } else {
            None
        }
    }

    pub(crate) fn pop_value(&mut self) -> AwkValue {
        // safe by type invariance
        unsafe {
            let value = self.pop().expect("empty stack");
            value.into_owned()
        }
    }

    pub(crate) fn pop_ref(&mut self) -> &mut AwkValue {
        // safe by type invariance
        unsafe { &mut *self.pop().expect("empty stack").unwrap_ptr() }
    }

    pub(crate) fn push_value<V: Into<AwkValue>>(&mut self, value: V) -> Result<(), String> {
        // a `StackValue::Value` is always valid, so this is safe
        unsafe { self.push(StackValue::from(value.into())) }
    }

    /// pushes a reference on the stack
    /// # Safety
    /// `value_ptr` has to be safe to access at least until the value preceding it
    /// on the stack is popped.
    pub(crate) unsafe fn push_ref(&mut self, value_ptr: *mut AwkValue) -> Result<(), String> {
        self.push(StackValue::ValueRef(value_ptr))
    }

    pub(crate) fn next_instruction(&mut self) -> Option<OpCode> {
        self.instructions.get(self.ip as usize).copied()
    }

    pub(crate) fn call_function(&mut self, function: &'i Function) {
        unsafe { assert!(self.sp.offset_from(self.bp) >= function.parameters_count as isize) };
        let new_bp = unsafe { self.sp.sub(function.parameters_count) };
        let caller_frame = CallFrame {
            bp: self.bp,
            sp: new_bp,
            ip: self.ip,
            instructions: self.instructions,
            source_locations: self.source_locations,
            function_file: self.current_function_file.clone(),
            function_name: self.current_function_name.clone(),
        };
        self.current_function_file = function.debug_info.file.clone();
        self.current_function_name = function.name.clone();
        self.call_frames.push(caller_frame);
        self.bp = new_bp;
        self.ip = 0;
        self.instructions = &function.instructions;
        self.source_locations = &function.debug_info.source_locations;
    }

    pub(crate) fn restore_caller(&mut self) {
        let caller_frame = self
            .call_frames
            .pop()
            .expect("tried to restore caller when there is none");
        self.bp = caller_frame.bp;
        self.sp = caller_frame.sp;
        self.instructions = caller_frame.instructions;
        self.ip = caller_frame.ip;
    }

    pub(crate) fn new(main: &'i Action, stack: &'s mut [StackValue]) -> Self {
        let stack_len = stack.len();
        let bp = stack.as_mut_ptr();
        // one past the end pointers are safe
        let stack_end = unsafe { bp.add(stack_len) };
        Self {
            current_function_file: main.debug_info.file.clone(),
            current_function_name: "<start>".into(),
            instructions: &main.instructions,
            source_locations: &main.debug_info.source_locations,
            ip: 0,
            bp,
            sp: bp,
            stack_end,
            call_frames: Vec::new(),
            _stack_lifetime: PhantomData,
        }
    }
}

pub(crate) enum ExecutionResult {
    Expression(AwkValue),
    Next,
    NextFile,
    Exit(i32),
}

impl ExecutionResult {
    pub(crate) fn expr_to_bool(self) -> bool {
        self.unwrap_expr().scalar_as_bool()
    }

    pub(crate) fn unwrap_expr(self) -> AwkValue {
        match self {
            ExecutionResult::Expression(value) => value,
            _ => panic!("execution result is not an expression"),
        }
    }
}

macro_rules! numeric_op {
    ($stack:expr, $op:tt) => {
        let rhs = $stack.pop_scalar_value()?.scalar_as_f64();
        let lhs = $stack.pop_scalar_value()?.scalar_as_f64();
        $stack.push_value(lhs $op rhs)?;
    };
}

macro_rules! compare_op {
    ($stack:expr, $convfmt:expr, $op:tt) => {
        let rhs = $stack.pop_scalar_value()?;
        let lhs = $stack.pop_scalar_value()?;
        match (&lhs.value, &rhs.value) {
            (AwkValueVariant::Number(lhs), AwkValueVariant::Number(rhs)) => {
                $stack.push_value(bool_to_f64(lhs $op rhs))?;
            }
            (AwkValueVariant::String(lhs), AwkValueVariant::String(rhs)) => {
              	if lhs.is_numeric && rhs.is_numeric {
									$stack.push_value(bool_to_f64(strtod(lhs) $op strtod(rhs)))?;
              	} else {
                	$stack.push_value(bool_to_f64(lhs.as_str() $op rhs.as_str()))?;
              	}
            }
            (AwkValueVariant::Number(lhs), AwkValueVariant::UninitializedScalar) => {
                $stack.push_value(bool_to_f64(*lhs $op 0.0))?;
            }
            (AwkValueVariant::UninitializedScalar, AwkValueVariant::Number(rhs)) => {
                $stack.push_value(bool_to_f64(0.0 $op *rhs))?;
            }
            (AwkValueVariant::String(s), AwkValueVariant::Number(x)) if s.is_numeric => {
                $stack.push_value(bool_to_f64(lhs.scalar_as_f64() $op *x))?;
            }
            (AwkValueVariant::Number(x), AwkValueVariant::String(s)) if s.is_numeric => {
                $stack.push_value(bool_to_f64(*x $op rhs.scalar_as_f64()))?;
            }
            (_, _) => {
                $stack.push_value(bool_to_f64(lhs.scalar_to_string($convfmt)?.as_str() $op rhs.scalar_to_string($convfmt)?.as_str()))?;
            }
        }
    };
}

pub(crate) use compare_op;
pub(crate) use numeric_op;
