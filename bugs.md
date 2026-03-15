# pcc Bug Log — CPython Build Campaign

## Status: CPython compiles fully! Runtime crash at -O2+ (inliner bug).

**All .c files compile successfully.** Full pcc builds at -O0 and -O1 produce a working `_freeze_module`. The -O2/-O3 crash is confirmed as an inliner bug.

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

### BUG F: Inliner miscompilation with 10+ instruction functions — INVESTIGATING
- **Symptom:** CPython `_freeze_module` segfaults at -O2/-O3; uninitialized stack values used as array indices
- **Location:** Crash in `_PySys_InitCore` at `ob_item[pos]` where `pos` is garbage
- **Root cause:** Inliner has an additional bug triggered by functions with 10+ IR instructions. `ALWAYS_INLINE_SIZE=9` works, `=10` crashes. Disabling inlining entirely at -O2 produces a working binary.
- **Affected functions:** Common CPython header inlines — `PyList_GET_SIZE`, `_PyStructSequence_InitBuiltin`, `PyType_Check`, etc.
- **Full pcc -O0:** SUCCESS, **-O1:** SUCCESS, **-O2:** CRASH, **-O3:** CRASH
- **Next steps:** Need IR dump of a 10-instruction function before/after inlining to identify the specific mishandled pattern

### BUGs 1-6: Initializer bugs (ALL FIXED)
1. Silent `Initializer::None` → hard error + pointer arithmetic + array/pointer type handling
2. Anonymous struct positional continuation → stack-based `AnonContinuation` with arbitrary nesting
3. CompoundLiteral type mismatch → use `*cl_type`
4. Brace elision (C99 6.7.8p17-20) → `consume_brace_elision` helper, string literal exclusion
5. u32 overflow in `size_bits` → u64 intermediate
6. `Directive::Zero(u32)` → `Zero(usize)`
