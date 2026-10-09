//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
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
            StackValue::ArrayElementRef(array_element_ref) => {
                let val = (*array_element_ref.array)
                    .as_array()
                    .expect("expected array")
                    .index_to_value(array_element_ref.value_index)
                    .expect("invalid array value index");
                (*val).clone().into_ref(AwkRefType::None)
            }
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

    /// Returns a pointer to local `index` for assigning it as a scalar.  A
    /// parameter that still aliases the caller's unset variable stops
    /// aliasing it here: the caller's variable becomes a scalar, as in gawk,
    /// but what is assigned to the parameter stays local.  If the caller's
    /// variable has meanwhile become an array the alias is kept, and the
    /// assignment reports an array used in scalar context.
    pub(crate) fn local_scalar_ref_ptr(&mut self, index: usize) -> Option<*mut AwkValue> {
        if unsafe { self.sp.offset_from(self.bp) } <= index as isize {
            return None;
        }
        let slot = unsafe { &mut *self.bp.add(index) };
        if let StackValue::UninitializedRef(caller_var) = slot {
            // valid by stack invariance: the caller's variable outlives this frame
            let caller_var = unsafe { &mut **caller_var };
            match caller_var.value {
                AwkValueVariant::Array(_) => return Some(caller_var),
                AwkValueVariant::Uninitialized => {
                    caller_var.value = AwkValueVariant::UninitializedScalar
                }
                _ => {}
            }
            let local = AwkValue {
                value: caller_var.value.clone(),
                ref_type: AwkRefType::None,
            };
            *slot = StackValue::Value(UnsafeCell::new(local));
        }
        self.get_mut_value_ptr(index)
    }

    pub(crate) fn pop_value(&mut self) -> AwkValue {
        // safe by type invariance
        unsafe {
            let value = self.pop().expect("empty stack");
            value.into_owned()
        }
    }

    /// Pops the reference on top of the stack, then the scalar value under it.
    pub(crate) fn pop_scalar_under_ref(&mut self) -> Result<(AwkValue, &mut AwkValue), String> {
        let reference = self.pop().expect("empty stack");
        let value = self.pop_scalar_value()?;
        // safe by type invariance: a reference points to a variable, a field
        // or an array element, never to the stack slots popped here
        Ok((value, unsafe { &mut *reference.unwrap_ptr() }))
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
        // A parameter bound to the caller's unset variable stays an
        // `UninitializedRef` to it, so that using the parameter as an array
        // makes the caller's variable that array; `local_scalar_ref_ptr`
        // ends the alias when the parameter is assigned as a scalar.
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
        self.source_locations = caller_frame.source_locations;
        self.current_function_name = caller_frame.function_name;
        self.current_function_file = caller_frame.function_file;
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
    /// `exit [status]`; no status keeps that of an earlier `exit status`
    Exit(Option<i32>),
}

impl ExecutionResult {
    /// The truth value of a pattern's result.  A `next`, `nextfile` or
    /// `exit` executed by a function the pattern called is put in `control`,
    /// and the pattern does not match.
    pub(crate) fn pattern_matched(self, control: &mut Option<ExecutionResult>) -> bool {
        match self {
            ExecutionResult::Expression(value) => value.scalar_as_bool(),
            other => {
                *control = Some(other);
                false
            }
        }
    }

    #[cfg(test)]
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
            (AwkValueVariant::UninitializedScalar, AwkValueVariant::UninitializedScalar) => {
                $stack.push_value(bool_to_f64(0.0 $op 0.0))?;
            }
            // POSIX 85481: an uninitialized value (including a nonexistent or
            // empty field) compared with a number is compared numerically.
            (AwkValueVariant::Number(n), AwkValueVariant::UninitializedScalar) => {
                $stack.push_value(bool_to_f64(*n $op 0.0))?;
            }
            (AwkValueVariant::UninitializedScalar, AwkValueVariant::Number(n)) => {
                $stack.push_value(bool_to_f64(0.0 $op *n))?;
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
