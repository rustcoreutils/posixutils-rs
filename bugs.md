# pcc Bug Log — CPython Build Campaign

## Status: _freeze_module clean. _bootstrap_python segfaults during bytecode execution. Full compilation now succeeds.

### Bugs A-L: ALL FIXED (see git history)

### BUG M: Global initializer errors silently produce corrupt .o — FIXED (3 sub-fixes)
1. **Error check after linearization** (`cc/main.rs`): Added `diag::has_error()` check so compilation fails on linearization errors instead of silently producing corrupt output.
2. **Static address fallback** (`cc/ir/linearize.rs`): Added `eval_static_address()` as a last-resort evaluator in the global initializer catch-all. Handles complex `&global.field->subfield` chains used in CPython's `_PyRuntimeState_INIT` macro.
3. **Compile-time ternary** (`cc/ir/linearize.rs`): Added `Conditional` expression handling in `ast_init_to_ir()`. CPython's `_Py_LATIN1_CHR()` macro uses `cond ? &table_a[i] : &table_b[i-128]` in static initializers.

### BUG N: _bootstrap_python SIGSEGV during bytecode execution — INVESTIGATING
- **Symptom:** Crash in `_PyEval_EvalFrameDefault` during actual opcode handler execution (not dispatch — the switch works now).
- **Note:** All .c files now compile without errors. The crash is from pcc-generated code running incorrectly at runtime.

### BUGs 1-6: Initializer bugs (ALL FIXED)
