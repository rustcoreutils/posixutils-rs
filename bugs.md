# pcc Bug Log — CPython Build Campaign

## Status: _freeze_module works. _bootstrap_python crashes in bytecode interpreter.

### Bugs A-J: ALL FIXED (see git history)
- A: Preprocessor block comment newline
- B: Inline asm operand handling (6 sub-fixes)
- C: Float constant expressions in global initializers
- D: Arrow expressions in eval_static_address
- E: Inliner two-register struct return
- F: Argument widening at call sites for inlined functions
- G: Small struct assignment stores pointer instead of value
- H: Zext codegen sign-extends instead of zero-extending (unsigned char casts)
- I: 32-bit stack slot stores leave stale upper bytes (store_to_stack_slot, emit_store)
- J: __attribute__((packed)) ignored — struct layout mismatch

### BUG K: Bytecode interpreter switch dispatch fails — INVESTIGATING
- **Symptom:** `_bootstrap_python` crashes with `__builtin_unreachable()` at the end of the opcode dispatch switch in `_PyEval_EvalFrameDefault` (ceval.c:905). The switch on `uint8_t opcode` doesn't match any case for the first bytecode instruction.
- **Observation:** pcc defines `__GNUC__=4` which makes `Py_UNREACHABLE()` expand to `__builtin_unreachable()` instead of `Py_FatalError()`. So the "error message" may be from a different source.
- **Likely cause:** pcc miscompiles the large switch statement (~250 cases) in ceval.c. The opcode value (uint8_t, 0-255) may be garbled, or the jump table / comparison chain is incorrect.
- **Next step:** Compare pcc and gcc assembly for the switch dispatch; check if pcc handles `uint8_t` switch correctly; test with the bytecode interpreter compiled by gcc.

### BUGs 1-6: Initializer bugs (ALL FIXED)
