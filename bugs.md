# pcc Bug Log — CPython Build Campaign

## Status: CPython compiles and _freeze_module runs at -O0 through -O3!

**All .c files compile successfully.** `_freeze_module` works at all optimization levels. Valgrind reports 0 errors at -O3.

### BUG A: Preprocessor drops line after `#define` when starts with `/* comment */` — FIXED
- **File:** `cc/token/lexer.rs` — `skip_block_comment()`
- **Fix:** Save/restore `self.newline` across block comment (matches sparse's `drop_stream_comment`)

### BUG B: Inline asm `+r` operand numbering and constraint handling — FIXED
- **Files:** `cc/ir/linearize.rs`, `cc/arch/x86_64/codegen.rs`
- **Root causes and fixes:**
  1. `+r` readwrite inputs had `matching_output: None` → set `Some(idx)`, skip in operand numbering
  2. `"m"` constraint with `Loc::Reg` → emit indirect `(%reg)` via `constraint_requires_memory`
  3. `"r"` constraint with `Loc::Stack` → allocate temp register via `constraint_requires_register`
  4. `Loc::Imm` as asm output → update location to temp reg instead of impossible store-to-immediate
  5. `Loc::Imm` as `"m"` input → load into temp reg, emit indirect
  6. `emit_raw_mov_to_loc` with `Loc::Imm` dest → skip (dead code from unoptimized switches)

### BUG C: Float constant expressions in global initializers — FIXED
- **File:** `cc/ir/linearize.rs` — `eval_const_float_expr()`
- **Fix:** Added float constant evaluator; catch-all tries float after int

### BUG D: Arrow expressions in `eval_static_address` — FIXED
- **File:** `cc/ir/linearize.rs` — `eval_static_address()`
- **Fix:** Handle `ExprKind::Arrow` by recursing and looking up member in pointee type

### BUG E: Inliner two-register struct return — FIXED
- **File:** `cc/ir/inline.rs` — `clone_instruction()` Ret handler
- **Symptom:** Functions returning 16-byte structs (e.g. `{long a; long b;}`) had second member corrupted when inlined at -O2+
- **Root cause:** `clone_instruction()` only copied `insn.src.first()`, dropping the second half of two-register returns
- **Fix:** When `is_two_reg_return`, generate two Store instructions (offset 0 and 8) instead of single Copy
- **Test:** `codegen_inline_two_reg_struct_return` in `cc/tests/codegen/misc.rs`

### BUG F: Inliner multi-call-site aggressive inlining — FIXED (workaround)
- **File:** `cc/ir/inline.rs` — `should_inline()`
- **Symptom:** CPython `_freeze_module` segfaulted at -O2/-O3; uninitialized stack values used as array indices in `_PySys_InitCore`
- **Root cause:** The `ALWAYS_INLINE_SIZE * 2` multi-call-site inlining rule at -O2 inlined functions of size 11-20 (like `PyTuple_SET_ITEM`, size 19) into many callers. This triggered a latent bug — possibly in block reordering, register allocation liveness, or CFG connectivity after inlining — causing uninitialized stack values.
- **Fix:** Removed the `ALWAYS_INLINE_SIZE * 2` multi-call-site rule. Functions with `inline` hint or single call sites are still aggressively inlined at O2. The underlying inliner bug remains for future investigation.
- **Verified:** O2 SUCCESS, O3 SUCCESS, Valgrind 0 errors at O3.

### BUGs 1-6: Initializer bugs (ALL FIXED)
1. Silent `Initializer::None` → hard error + pointer arithmetic + array/pointer type handling
2. Anonymous struct positional continuation → stack-based `AnonContinuation` with arbitrary nesting
3. CompoundLiteral type mismatch → use `*cl_type`
4. Brace elision (C99 6.7.8p17-20) → `consume_brace_elision` helper, string literal exclusion
5. u32 overflow in `size_bits` → u64 intermediate
6. `Directive::Zero(u32)` → `Zero(usize)`
