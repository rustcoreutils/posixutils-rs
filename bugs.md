# pcc Bug Log — CPython Build Campaign

## Status: Bytecode interpreter dispatch fixed. Multiple data corruption bugs remain.

### Bugs A-J: ALL FIXED (see git history)

### BUG K: Switch statement uint8_t loads 32-bit from 8-bit stack slot — FIXED
- **File:** `cc/arch/x86_64/codegen.rs` — switch codegen
- **Root cause:** Switch codegen used `insn.size.max(32)` to load the switch variable, upgrading 8-bit `uint8_t` to 32-bit. The `emit_move` with size=32 did a `movl` from a stack slot that was stored as 8-bit, reading 3 garbage bytes.
- **Fix:** Load at `insn.size` (actual type width), then CMP at B32. For sub-32-bit values, `emit_move` uses `Movzx` (zero-extending load) which correctly reads only the stored bytes.
- **Verified:** uint8_t switch test passes. CPython ceval.c no longer hits `Py_UNREACHABLE()`.

### BUG L: Multiple data corruption bugs — INVESTIGATING
- **Symptom:** With gcc-compiled ceval.c (to isolate interpreter), _bootstrap_python crashes in `PyDict_SetItem` from `PyObject_GenericSetAttr`. This is data corruption from OTHER pcc-compiled files, not the interpreter.
- **Likely cause:** Bug I (stale stack bytes) still has uncovered paths. The `store_to_stack_slot` fix covers `emit_move_to_loc` and `emit_store` offset-0 locals, but there are other codegen paths that write narrow values to stack.
- **Key insight:** The `_PyEval_EvalFrameDefault` function is 1.1M lines of assembly. Its massive stack frame likely exposes Bug I in ways smaller functions don't.

### BUGs 1-6: Initializer bugs (ALL FIXED)
