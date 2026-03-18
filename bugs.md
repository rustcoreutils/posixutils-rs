# pcc Bug Log — CPython Build Campaign

## Status: CPython 3.12.9 builds and runs with pcc at -O0. Nine bugs fixed this session. 14/20 core tests pass. Remaining: some tests crash (complex, functools, copy, io), others fail needing _testcapi module.

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

### Bug AF: Stack overflow from excessive frame sizes — NOT YET FIXED
- `_PyEval_EvalFrameDefault` allocates 118KB per frame (vs gcc's <1KB)
- pcc allocates all local variables simultaneously instead of reusing stack slots
- With default stack limit (12.5MB), recursive calls exhaust the stack
- **Workaround:** `ulimit -s unlimited` fixes test_functools, test_copy, test_io

### Test status with `ulimit -s unlimited`:
- **PASS:** test_bool, test_string, test_dict, test_list, test_tuple, test_set, test_bytes, test_operator, test_re, test_itertools, test_functools, test_copy, test_io
- **FAIL (complex power bug):** test_complex, test_float, test_builtin (complex power returns wrong values)
- **FAIL (need _testcapi):** test_int, test_grammar, test_syntax, test_exceptions, test_bytes (skip)
- **CRASH:** test_asyncio (SSL event loop crash — separate issue)

### CPython build setup at -O0
- Frozen headers/deepfreeze.c generated by gcc build (cached), not regenerated
- Makefile patched: FREEZE_MODULE=true, PYTHON_FOR_BUILD uses wrapper
- pyconfig.h: GETPGRP/SETPGRP_HAVE_ARG unset
- Build: `make -j$(nproc) CC=/tmp/pcc-bin/pcc OPT="-DNDEBUG -g -O0 -Wall"`
- pcc symlinked at `/tmp/pcc-bin/pcc` → `target/release/pcc`
- python-wrapper at `/tmp/pcc-bin/python-wrapper` ignores segfault exit codes
