# pcc Bug Log — CPython Build Campaign

## Status: _freeze_module works at -O0 through -O3 with identical output to gcc. _bootstrap_python can't load frozen modules — runtime import bug.

### BUG A: Preprocessor drops line after `#define` when starts with `/* comment */` — FIXED
- **File:** `cc/token/lexer.rs` — `skip_block_comment()`
- **Fix:** Save/restore `self.newline` across block comment

### BUG B: Inline asm `+r` operand numbering and constraint handling — FIXED
- **Files:** `cc/ir/linearize.rs`, `cc/arch/x86_64/codegen.rs`

### BUG C: Float constant expressions in global initializers — FIXED
- **File:** `cc/ir/linearize.rs` — `eval_const_float_expr()`

### BUG D: Arrow expressions in `eval_static_address` — FIXED
- **File:** `cc/ir/linearize.rs` — `eval_static_address()`

### BUG E: Inliner two-register struct return — FIXED
- **File:** `cc/ir/inline.rs` — `clone_instruction()` Ret handler
- **Fix:** When `is_two_reg_return`, generate two Store instructions (offset 0 and 8) instead of single Copy
- **Test:** `codegen_inline_two_reg_struct_return` in `cc/tests/codegen/misc.rs`

### BUG F: 32-bit Copy to stack + inliner exposes width mismatch — FIXED (via Bug G)
- **Root cause:** Inlined code passes 32-bit `int` values to 64-bit `Py_ssize_t` parameters without widening. The linearizer now inserts implicit integer widening conversions at call sites when actual argument is narrower than formal parameter.
- **File:** `cc/ir/linearize.rs` — `linearize_call()` argument handling

### BUG G: Small struct assignment stores pointer instead of value — FIXED
- **File:** `cc/ir/linearize.rs` — `emit_assign()`
- **Root cause:** `emit_assign()` only did block-copy for structs > 64 bits. Structs of exactly 64 bits (like `PyCompilerFlags` with 2 ints) fell through to `linearize_expr()` which returns the ADDRESS of the struct, not its value. The address was then stored instead of the dereferenced struct data.
- **Fix:** Changed threshold from `target_size > 64` to `target_size > 0` so ALL struct assignments use the block-copy path.
- **Verified:** _freeze_module output now identical to gcc's (0 byte differences).

### BUG H: _bootstrap_python can't load frozen modules — INVESTIGATING
- **Symptom:** `_bootstrap_python` fails with "Frozen object named '_frozen_importlib' is invalid"
- **Data is correct:** The frozen module byte arrays in the binary match the .h files exactly
- **Next step:** The bug is in the import/unmarshal runtime code, not in data embedding. Debug with gdb/valgrind.

### BUGs 1-6: Initializer bugs (ALL FIXED)
1-6: Various initializer bugs (see git history)
