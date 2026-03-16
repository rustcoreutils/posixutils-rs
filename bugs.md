# pcc Bug Log — CPython Build Campaign

## Status: Bugs N, P, Q fixed. -O flag handling fixed. Bug O + Bug R block CPython `make test`.

### Bugs A-N, P, Q: ALL FIXED (see git history)

### Also fixed: -O flag handling (last wins, GCC convention)
- pcc kept the FIRST -O flag and discarded later ones. GCC convention is last wins.
- CPython Makefile passes `-O3 -Wall -O0` (OPT then CFLAGS). pcc was using -O3 instead of -O0.
- Fixed in `preprocess_args()` and the test helper `run_preprocess()`.

### Bug P: Integer argument sign-extension — FIXED
- **Symptom:** CPython `_bootstrap_python` hung during init. `PyUnicode_FromWideChar(w, -1)` received `size=0x00000000FFFFFFFF` instead of `-1`, tried to allocate 4GB.
- **Root cause:** When passing a 32-bit `int` literal (e.g., `-1`) to a 64-bit `long`/`Py_ssize_t` parameter, the linearizer correctly emitted a widening conversion but pushed the ORIGINAL 32-bit type into `arg_types_vec`. The codegen then used `movl` (32-bit, zero-extending) instead of `movq` (64-bit) for the register argument.
- **Fix:** In `linearize_call()`, after a widening conversion, update `arg_type` to the formal parameter type before pushing to `arg_types_vec`.
- **Test:** `c89_functions_int_arg_sign_extension` in `cc/tests/c89/functions.rs`

### Bug Q: Function pointer dereference not treated as no-op — FIXED
- **Symptom:** CPython `builtin_any()` crashed (SIGSEGV at address 0x55). `iternext = *Py_TYPE(it)->tp_iternext` read byte 0x55 (`push %rbp` opcode) from the function's code as a pointer value.
- **Root cause (two parts):**
  1. **Linearizer (`emit_unary`):** No check for `TypeKind::Function` in the Deref handler. A load instruction was emitted, reading bytes from the function code.
  2. **Parser:** When `*func_name` is used (function identifier, not pointer variable), the operand has function type (not pointer-to-function). `base_type(function_type)` returned the return type (`int`) instead of recognizing the no-op.
- **Fix:** Added `TypeKind::Function` check in `emit_unary` (Deref returns src), and in the parser's Deref type computation (if operand is Function, keep the type).
- **Test:** `c89_functions_deref_funcptr_noop` in `cc/tests/c89/functions.rs`

### Bug N: Large struct parameter ABI mismatch — FIXED
- **Root cause:** pcc passed struct parameters > 16 bytes as hidden pointers (in RDI), while the SysV AMD64 ABI requires them by value on the stack (MEMORY class).
- **Fix:** Caller pushes all qwords to stack. Callee receives via IncomingArg + SymAddr. Regalloc advances by full struct size. No GP register consumed.
- **Also fixed:** 32→64 bit store widening at offset 0 clobbered adjacent struct fields (compound literal + designated init failures). Made struct-aware via sym_type_sizes map.

### Bug R: CPython -O0 — regex parser + finalization crash — NOT YET FIXED
- **Symptom 1:** `_bootstrap_python` crashes in `_PyArg_Fini()` during `Py_FinalizeEx()`. Misaligned pointer `0x555555887f17` in the `_PyArg_Parser` linked list. Output files ARE generated before the crash.
- **Symptom 2:** Python regex patterns with parenthesized groups fail: `re.compile(r"(\w+)")` → `re.error: missing ), unterminated subpattern`. Simple patterns without groups work.
- **Status:** The `_freeze_module` steps complete (output generated). The `deepfreeze.py` step fails because it imports `argparse` → `gettext` → `re` with grouped patterns.
- **Possible root cause:** Another sign-extension or comparison bug in the bytecode interpreter affecting character/string comparisons. The regex parser is pure Python executing under pcc's compiled interpreter. Something in `ceval.c` or related C code at -O0 is mishandling integer comparisons that the regex parser relies on.
- **Workaround:** Not yet found. The finalization crash can be masked (exit code), but the regex failure blocks `deepfreeze.py`.

### Bug O: Stale caller-saved register across call in goto-dispatch loops — NOT YET FIXED
- **Symptom:** `_bootstrap_python` segfaults in `_PyEval_EvalFrameDefault` during `init_importlib`. NULL pointer dereference when reading bytecode (`movswl (%reg)` where reg=0).
- **Root cause:** In CPython's bytecode interpreter (switch/goto dispatch), the linearizer/SSA generates code that uses a caller-saved register (R8 or RDX) for `next_instr` across a call to `_Py_HandlePending`. The call clobbers the register, leaving NULL (from zero_stack_frame). The re-dispatch at `handle_eval_breaker` → `DISPATCH()` uses the stale register value instead of reloading from the local variable's stack slot.
- **Evidence:** GDB shows R8=0 or RCX=0 at crash. Same crash at parent commit e552e2be (pre-existing). Replacing `ceval.o` with gcc-compiled version works.
- **Verified NOT regalloc:** The pseudo's computed live interval doesn't span the call (interval is too narrow), so `crosses_call` returns false. Preferring callee-saved for all pseudos doesn't help — all 5 callee-saved regs are exhausted in this massive function (54KB stack frame, ~6800 qword slots).
- **Likely root cause area:** SSA phi resolution or linearizer value reuse at `goto dispatch_opcode` targets. When `DISPATCH()` macro re-reads `*next_instr`, the linearizer should emit a fresh load from the local variable's stack slot. Instead, it appears to reuse a pseudo from before the call.
- **Workaround:** Compile `Python/ceval.c` with gcc (confirmed working).
- **Impact:** Blocks CPython `make test` success gate.

### BUGs 1-6: Initializer bugs (ALL FIXED)
