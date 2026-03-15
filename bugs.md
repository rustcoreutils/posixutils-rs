# pcc Bug Log — CPython Build Campaign

## Status: _freeze_module works at -O0 through -O3. _bootstrap_python segfaults.

### BUG A-D: Preprocessor, inline asm, float expr, arrow expr — ALL FIXED

### BUG E: Inliner two-register struct return — FIXED
- **File:** `cc/ir/inline.rs`
- **Fix:** Generate two Store instructions for 16-byte struct returns

### BUG F: Argument widening at call sites — FIXED
- **File:** `cc/ir/linearize.rs`
- **Fix:** Insert sign/zero extension when actual argument is narrower than formal parameter

### BUG G: Small struct assignment stores pointer instead of value — FIXED
- **File:** `cc/ir/linearize.rs`
- **Fix:** Use block-copy for ALL struct sizes (not just > 64 bits)

### BUG H: Zext codegen sign-extends instead of zero-extending — FIXED
- **File:** `cc/arch/x86_64/expression.rs` — `emit_extend()`
- **Root cause:** `emit_move(src, dst, insn.src_size.max(32))` loaded an 8-bit value at 32-bit width. Since the source had been loaded with `movsbl` (sign-extending for `char`), the register contained the sign-extended value. The Zext then did nothing because the value was already 32-bit.
- **Fix:** After loading, AND-mask to the actual source width to get the correct unsigned value.
- **Impact:** `(unsigned char)` casts on char values >= 0x80 produced wrong results (e.g., 0xF3 became -13 instead of 243). This broke CPython's marshal parser.

### BUG I: 32-bit stack slot stores leave stale upper bytes — PARTIALLY FIXED
- **Files:** `cc/arch/x86_64/codegen.rs` — `store_to_stack_slot()`, `emit_move_to_loc()`, `emit_store()`
- **Root cause:** Multiple codegen paths write 32-bit `int` values to 8-byte stack slots using `movl` (4 bytes), leaving the upper 4 bytes uninitialized. Subsequent 64-bit loads read stale upper bytes.
- **Fix (partial):** Created `store_to_stack_slot()` that always writes 64-bit. Updated `emit_move_to_loc()` and `emit_store()` (offset-0 stores to locals) to use it.
- **Remaining:** There may be more codegen paths that bypass these functions. The _bootstrap_python still segfaults after the marshal fix.
- **Ideal fix:** Propagate type info through all codegen paths; ensure all stack slot writes go through `store_to_stack_slot()`.

### BUG J: _bootstrap_python segfaults in _PyDict_GetItemWithError during LOAD_BUILD_CLASS — INVESTIGATING
- **Symptom:** After fixing Bugs H+I, _bootstrap_python loads frozen importlib, starts executing bytecode, then segfaults in `_PyDict_GetItemWithError(BUILTINS(), &_Py_ID(__build_class__))` from `LOAD_BUILD_CLASS` instruction at `bytecodes.c:989`
- **Likely cause:** Dict or builtins pointer corruption from pcc codegen. May be another instance of Bug I (stale stack bytes) or a new type of codegen bug.
- **Next step:** Check if `BUILTINS()` returns valid data; use valgrind to find the corruption source

### BUGs 1-6: Initializer bugs (ALL FIXED)
