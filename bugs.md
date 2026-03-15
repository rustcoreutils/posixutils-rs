# pcc Bug Log — CPython Build Campaign

## Status: Bug N fixed. Bug O (ceval regalloc) blocks CPython `make test`.

### Bugs A-N: ALL FIXED (see git history)

### Bug N: Large struct parameter ABI mismatch — FIXED
- **Root cause:** pcc passed struct parameters > 16 bytes as hidden pointers (in RDI), while the SysV AMD64 ABI requires them by value on the stack (MEMORY class).
- **Fix:** Caller pushes all qwords to stack. Callee receives via IncomingArg + SymAddr. Regalloc advances by full struct size. No GP register consumed.
- **Also fixed:** 32→64 bit store widening at offset 0 clobbered adjacent struct fields (compound literal + designated init failures). Made struct-aware via sym_type_sizes map.

### Bug O: Stale caller-saved register across call in goto-dispatch loops — NOT YET FIXED
- **Symptom:** `_bootstrap_python` segfaults in `_PyEval_EvalFrameDefault` during `init_importlib`. NULL pointer dereference when reading bytecode (`movswl (%reg)` where reg=0).
- **Root cause:** In CPython's bytecode interpreter (switch/goto dispatch), the linearizer/SSA generates code that uses a caller-saved register (R8 or RDX) for `next_instr` across a call to `_Py_HandlePending`. The call clobbers the register, leaving NULL (from zero_stack_frame). The re-dispatch at `handle_eval_breaker` → `DISPATCH()` uses the stale register value instead of reloading from the local variable's stack slot.
- **Evidence:** GDB shows R8=0 or RCX=0 at crash. Same crash at parent commit e552e2be (pre-existing). Replacing `ceval.o` with gcc-compiled version works.
- **Verified NOT regalloc:** The pseudo's computed live interval doesn't span the call (interval is too narrow), so `crosses_call` returns false. Preferring callee-saved for all pseudos doesn't help — all 5 callee-saved regs are exhausted in this massive function (54KB stack frame, ~6800 qword slots).
- **Likely root cause area:** SSA phi resolution or linearizer value reuse at `goto dispatch_opcode` targets. When `DISPATCH()` macro re-reads `*next_instr`, the linearizer should emit a fresh load from the local variable's stack slot. Instead, it appears to reuse a pseudo from before the call.
- **Workaround:** Compile `Python/ceval.c` with gcc (confirmed working).
- **Impact:** Blocks CPython `make test` success gate.

### BUGs 1-6: Initializer bugs (ALL FIXED)
