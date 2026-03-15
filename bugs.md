# pcc Bug Log — CPython Build Campaign

## Status: _freeze_module clean (0 valgrind errors). _bootstrap_python executes bytecode but segfaults during opcode execution.

### Bugs A-K: ALL FIXED (see git history)

### BUG L: Deref of small struct/union returns pointer instead of value — FIXED
- **File:** `cc/ir/linearize.rs` — `UnaryOp::Deref` handler
- **Root cause:** `linearize_expr` for Deref of struct/union ALWAYS returned the pointer (address), even for small types (<= 64 bits) that fit in a register. Callers that used the result as a VALUE (e.g., `_Py_CODEUNIT word = *next_instr`) stored the pointer instead of the pointed-to data — truncating a 64-bit pointer to a 16-bit field.
- **Fix:** Only return the address for large structs (> 64 bits). For small structs/unions (<= 64 bits), emit a LOAD instruction to actually read the value through the pointer.
- **Impact:** Fixed CPython's bytecode dispatch — the switch on `uint8_t opcode` now receives the correct opcode from `*next_instr` instead of garbage.

### DESIGN FIX: Stack frame zero-initialization — IMPLEMENTED
- Zero all stack frames at function entry using `rep stosq`
- Prevents ALL instances of stale upper bytes in stack slots

### BUG M: _bootstrap_python SIGSEGV during opcode execution — INVESTIGATING
- **Symptom:** Crash in `_PyEval_EvalFrameDefault` during actual bytecode execution (no longer `__builtin_unreachable`)
- **Progress:** The bytecode dispatch switch now works correctly. The crash is during opcode handler execution — a different and more specific bug.

### BUGs 1-6: Initializer bugs (ALL FIXED)
