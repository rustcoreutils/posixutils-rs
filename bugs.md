# pcc Bug Log — CPython Build Campaign

## Status: _freeze_module works (0 valgrind errors). _bootstrap_python crashes in bytecode dispatch.

### Bugs A-K: ALL FIXED (see git history)

### DESIGN FIX: Zero-initialize stack frames at function entry
- **File:** `cc/arch/x86_64/codegen.rs` — `zero_stack_frame()`, `emit_prologue()`
- **Design:** All function prologues now zero the stack frame using `rep stosq` after allocating stack space but before storing arguments. This ensures all 8-byte regalloc stack slots start as zero, so narrow writes (8/16/32-bit) leave zero in the unwritten upper bytes instead of garbage.
- **Implementation:** Save RDI/RCX to R10/R11, set up rep stosq (RDI=RSP, RCX=qwords, RAX=0), zero, restore RDI/RCX. Called before argument spilling.
- **LIR:** Added `RepStosq` instruction type.

### BUG L: _bootstrap_python "Unreachable C code path" despite all fixes — INVESTIGATING
- **Symptom:** `_bootstrap_python` hits `__builtin_unreachable()` at line 1 of importlib._bootstrap, same as Bug K symptom
- **Verified:** _freeze_module works with 0 valgrind errors. uint8_t switch test passes. unsigned char cast test passes.
- **Key observation:** This crash persists even WITH stack frame zeroing. The bytecode dispatch switch falls through to the default case for the first opcode (151 = RESUME). This means the switch codegen itself has a bug for large case counts (~250 cases in ceval.c).
- **Next step:** Compare pcc vs gcc assembly for the switch in _PyEval_EvalFrameDefault to find the dispatch codegen bug.

### BUGs 1-6: Initializer bugs (ALL FIXED)
