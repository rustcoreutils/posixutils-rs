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

### Bug R: Multiple sign-extension and conditional bugs — MOSTLY FIXED
- **R1 (FIXED):** Ternary `mode == 2 ? count : -1` didn't promote 32-bit `-1` to 64-bit result type. Broke `str.find()` (returned 0xFFFFFFFF instead of -1) → broke regex → blocked deepfreeze.
- **R2 (FIXED):** `cbr` instructions used `testl` (32-bit) instead of `testq` (64-bit) for condition values. Broke UTF-8 decoder's `ascii_decode` fast path.
- **R3 (PARTIALLY FIXED):** Store-widening extended to widen 32→64 for all non-struct locals. But finalization crash persists: `_PySignal_Fini()` → `compare_handler()` → `Py_TYPE()` crash. Pointer `0x55555664e3e8` truncated to `0x5664e3e8` (lower 32 bits). Root cause: pointer stored through struct field (non-local) path uses exact store size from IR; the IR generates 32-bit store for a 64-bit pointer field. Needs IR-level fix for struct member stores.
- **R4 (WORKAROUND):** Deepfreeze struct initializers produce wrong code objects. Workaround: disable deepfreeze (GET_CODE=NULL) and use marshal-based frozen loading.
- **R5 (NOT FIXED):** `str.format()` with named placeholders (`{name}`) interprets name as positional index. Likely another 32-bit truncation in the format parser C code.

### CPython build status at -O0
- **Build**: Compiles and links. Frozen modules generated. Deepfreeze disabled (workaround).
- **Python binary**: Starts, runs Python code correctly, imports modules. Crashes during finalization (Bug R3).
- **make test**: Blocked by finalization crash exit code (every test process exits 139).

### Bug O: Stale caller-saved register across call in goto-dispatch loops — NOT YET FIXED
- **Symptom:** `_bootstrap_python` segfaults in `_PyEval_EvalFrameDefault` during `init_importlib`. NULL pointer dereference when reading bytecode (`movswl (%reg)` where reg=0).
- **Root cause:** In CPython's bytecode interpreter (switch/goto dispatch), the linearizer/SSA generates code that uses a caller-saved register (R8 or RDX) for `next_instr` across a call to `_Py_HandlePending`. The call clobbers the register, leaving NULL (from zero_stack_frame). The re-dispatch at `handle_eval_breaker` → `DISPATCH()` uses the stale register value instead of reloading from the local variable's stack slot.
- **Evidence:** GDB shows R8=0 or RCX=0 at crash. Same crash at parent commit e552e2be (pre-existing). Replacing `ceval.o` with gcc-compiled version works.
- **Verified NOT regalloc:** The pseudo's computed live interval doesn't span the call (interval is too narrow), so `crosses_call` returns false. Preferring callee-saved for all pseudos doesn't help — all 5 callee-saved regs are exhausted in this massive function (54KB stack frame, ~6800 qword slots).
- **Likely root cause area:** SSA phi resolution or linearizer value reuse at `goto dispatch_opcode` targets. When `DISPATCH()` macro re-reads `*next_instr`, the linearizer should emit a fresh load from the local variable's stack slot. Instead, it appears to reuse a pseudo from before the call.
- **Workaround:** Compile `Python/ceval.c` with gcc (confirmed working).
- **Impact:** Blocks CPython `make test` success gate.

### BUGs 1-6: Initializer bugs (ALL FIXED)
