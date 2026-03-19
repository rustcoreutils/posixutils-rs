# pcc Bug Log — CPython Build Campaign

## Status: CPython 3.12.9 builds and runs with pcc at -O0. Stack coloring implemented — eval loop frame 118KB→83KB. test_copy/test_functools pass with default stack. 14/20 core tests pass with ulimit -s unlimited.

### Bugs A-N, P, Q, 1-6: ALL FIXED (see git history)

### Bug S: Floating-point codegen bugs — FIXED
- **Root cause 1:** XMM registers not spilled across function calls. x86-64 SysV ABI says ALL XMM regs are caller-saved, but the register allocator didn't check `crosses_call` for FP values. Float/double values in XMM8-XMM13 were silently clobbered by intervening calls.
- **Fix:** In `regalloc.rs`, spill FP values to stack when `crosses_call` is true.
- **Root cause 2:** `emit_cbr()` used `FpSize::Single` (ucomiss, 32-bit) for all float-to-bool conditions including doubles. Values like 0.5 have zero in their lower 32 bits, so `if(0.5)` evaluated to false.
- **Fix:** Derive `FpSize` from the condition type's bit width in both x86-64 and aarch64 backends.
- **Tests:** `codegen_xmm_spill_across_calls`, `codegen_double_to_bool`

### Bug T: chr()/string literals with bytes >= 0x80 — FIXED
- **Root cause:** pcc stored C string bytes as Rust `char` values in a Rust `String`. For bytes >= 0x80, Rust UTF-8 encodes them (e.g., `\x80` → 2-byte `\xC2\x80`), corrupting assembly output, sizeof, and zero-fill calculations.
- **Fix (5 locations):**
  - `escape_string()` in `cc/arch/codegen.rs`: emit single octal byte for chars 0-255
  - `emit_initializer_data()` in `cc/arch/codegen.rs`: use `chars().count()` for zero-fill
  - `sizeof` in `cc/parse/expression.rs`: use `chars().count()` for array size
  - Null terminator offsets in `cc/ir/linearize.rs` (2 places): use `chars().count()`
- **Test:** `codegen_string_literal_high_bytes`

### Bug U: unsigned long to double + XMM param spill — FIXED
- **Fixed:** `cvtsi2sdq` treated unsigned 64-bit values as signed. Values >= 2^63 produced negative doubles. Now uses the correct shift-convert-double sequence.
- **Fixed:** XMM function parameters not spilled to stack at function entry. Any float computation that reused the same XMM register would clobber the parameter.
- `int(37.5)` and `math.floor(37.5)` now work correctly in CPython.
- **Tests:** `codegen_unsigned_long_to_double`

### Bug W: FP compare Xmm0 clobber — FIXED
- **Root cause:** `emit_fp_compare()` loaded src1 into Xmm0, but if src2 was allocated to Xmm0, src2 was clobbered. Result: `ucomisd %xmm0, %xmm0` (self-compare, always equal), causing `d < -(double)_PyTime_MIN` to always be false.
- **Symptom:** `time.sleep(0.001)` raised `OverflowError: timestamp out of range for platform time_t`
- **Fix:** In `emit_fp_compare()`, check if src2 is in Xmm0 before loading src1; if so, save src2 to Xmm15 first.
- **Test:** `codegen_fp_compare_xmm0_clobber`

### Bug X: Inline asm 64-bit register constraints — FIXED
- **Root cause:** `format_reg_default()` in `AsmOperandFormatter` hardcoded 32-bit register names for all inline asm operands. For `uintptr_t` with `"+r"` constraint, `xchg %edx, (%r8)` was emitted instead of `xchg %rdx, (%r8)`, truncating 64-bit values to 32 bits.
- **Symptom:** CPython's `_Py_atomic_store` (seq_cst via xchg) truncated pointers. Signal handlers in `_PySignal_Fini` had corrupted addresses → segfault during finalization.
- **Fix:** Pass operand size through `substitute_asm_operands` → `format_reg_default(reg, size_bits)`. Select register width (b/w/k/q) based on operand type's bit width.
- **Test:** `codegen_inline_asm_64bit_constraint`

### Bug Y: XMM-to-GPR movd truncation — FIXED
- **Root cause:** `emit_move()` for `Loc::Xmm → Reg` hardcoded `OperandSize::B32` (movd) instead of using `op_size`. For doubles (64-bit), `movd %xmm, %r10d` copied only lower 32 bits. For `1.0` (0x3FF0000000000000), lower 32 bits = 0x00000000 → result 0.0.
- **Symptom:** `1/1` returned `0.0` in CPython (integer true division). Conditional select of double values (ternary, if/else returning double) produced 0.0.
- **Fix:** Use `op_size` instead of hardcoded `B32` in `MovXmmGp` instruction.
- **Test:** `codegen_double_xmm_to_gpr_movq`

### Bug Z: Ternary function pointer call return type — FIXED
- **Root cause:** `parse_conditional_expr()` used `self.types.pointer_to()` for function-to-pointer decay, but `pointer_to()` only does a lookup and falls back to `void*` when the type isn't in the table. This caused `(cond ? func_a : func_b)(arg)` to have type `void*`, and the Call expression handler couldn't determine the function return type, defaulting to `int` (32-bit).
- **Fix:** Use `self.types.intern(Type::pointer(...))` instead of `self.types.pointer_to(...)` in ternary decay, ensuring the pointer-to-function type is created if not found.
- **Symptom:** `string.Formatter().format()` segfaulted; indirect calls through ternary-selected function pointers returned truncated 32-bit values for pointer return types.
- **Test:** `codegen_ternary_fptr_return_type`

### Bug AA: Character literal signedness — FIXED
- **Root cause:** `'\x80'` produced 128 (unsigned) instead of -128 (signed). pcc cast `char` to `i64` via Rust `char` (unsigned), bypassing sign-extension. GCC treats `'\x80'` as -128 per implementation-defined behavior.
- **Fix:** Use `*c as u8 as i8 as i64` in all 4 CharLit-to-integer conversions (linearize.rs x3, parser.rs x1).
- **Symptom:** CPython's `_pickle` module failed — `enum { PROTO = '\x80' }` was 128, but `switch((enum)char_val)` saw -128. All opcodes >= 0x80 were unmatchable.
- **Test:** `codegen_char_literal_signedness`

### Bug AB: Static local struct variables on stack — FIXED
- **Root cause:** `linearize_local_decl()` checked `self.types.modifiers(typ)` for STATIC, but storage class modifiers are in `declarator.storage_class`, not the type system. For struct types, STATIC was never in the type, so static structs were allocated on the stack.
- **Fix:** Check `declarator.storage_class.contains(TypeModifiers::STATIC)` instead.
- **Symptom:** CPython's `_PyArg_Parser` structs (declared `static` in clinic-generated code) lived on the stack. When the function returned, the linked list in `_PyRuntime.getargs.static_parsers` contained dangling stack pointers → segfault in `_PyArg_Fini()` during finalization.
- **Test:** `codegen_static_local_struct`

### Bug R5/O: Previously listed bugs — FIXED by above fixes

### Bug AC: IEEE 754 NaN comparison — FIXED
- **Root cause:** `ucomisd` sets PF=1 for NaN, but `sete`/`setb`/`setbe`/`setne` don't check PF. All NaN comparisons gave wrong results.
- **Fix:** For ordered ==, <, <=: emit `setcc + setnp + AND`. For !=: `setne + setp + OR`. Added `CondCode::Np`/`P`.
- **Test:** `codegen_nan_comparison`, `codegen_nan_comparison_comprehensive`

### Bug AD: PreInc on dereferenced PostInc (++*s++) — FIXED
- **Root cause:** `++*s++` re-evaluated `s++` when storing back the incremented value, causing the store to go to the wrong address (s+1 instead of s). The deref operand's side effects were executed twice.
- **Fix:** Pre-compute the lvalue address via `linearize_lvalue` before loading the value, avoiding re-evaluation of PostInc side effects.
- **Symptom:** `_Py_dg_dtoa` (float-to-string) produced wrong digit strings. `'%.0f' % 1.5` returned "12" instead of "2", causing memset crash.
- **Test:** `codegen_preinc_deref_postinc`

### Bug AE: 2-SSE struct ABI param/return mismatch — PARTIALLY FIXED
- **Root cause:** struct { double, double } was passed via hidden pointer but returned in XMM0+XMM1. Crashes when return value passed directly as argument.
- **Fix:** Implemented 2-XMM parameter passing at call sites and function entry.
- **Remaining:** When a 2-SSE struct is the FIRST param followed by integer params, the callee's function entry code assigns integer args to the wrong registers (off by one). `powu(Complex x, long n)` reads `n` from rsi instead of rdi.
- **Workaround:** Complex tests work when struct is not first param, or with unlimited stack.

### Bug AF: Stack overflow from excessive frame sizes — PARTIALLY FIXED
- `_PyEval_EvalFrameDefault` frame reduced from 118KB to 83KB via stack slot reuse
- Stack coloring reuses slots for non-loop-spanning pseudos with non-overlapping intervals
- test_copy and test_functools now pass with default `ulimit -s` (8MB)
- test_io still needs `ulimit -s unlimited` (deep `__repr__` recursion * 83KB frame)
- **Remaining:** frame still ~83x larger than gcc's ~1KB; further reduction needs better register allocation or more aggressive stack coloring

### Bug AG-AK: See git log for recent fixes (small struct return, FP binop clobber, va_start overflow, long double narrowing, XMM cross-block spill)

### Test status: CPython 3.12.9 make test at -O0 (ulimit -s unlimited)
- 491 tests run (test_decimal skipped, hangs at -O0)
- 439 PASS, 19 FAIL (baseline was 70 FAIL, 51 tests fixed)
- Remaining 19: test_audioop test_buffer test_capi test_clinic test_cmd_line_script test_datetime test_descr test_fractions test_interpreters test_mailbox test_perf_profiler test_statistics test_threading test_time test_tools test_tracemalloc test_unicode test_userstring test.test_gdb.test_pretty_print

### CPython build setup at -O0
- configure CC=/tmp/pcc-bin/pcc, then fix pyconfig.h GETPGRP/SETPGRP
- Frozen deepfreeze.c generated by gcc build first, saved to /tmp
- Makefile: OPT= -DNDEBUG -g -O0 -Wall
- After make clean: restore deepfreeze.c, then make -j nproc
- Must delete all .pyc files after pcc rebuild
