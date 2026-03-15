# pcc Bug Log — CPython Build Campaign

## Status: ROOT CAUSE of Bug N identified: ABI mismatch for large struct parameters.

### Bugs A-M: ALL FIXED (see git history)

### BUG N ROOT CAUSE: Large struct parameters passed by pointer instead of by value — NOT YET FIXED
- **Root cause confirmed:** pcc passes struct parameters > 16 bytes as hidden pointers (sret-style, in RDI), while the SysV AMD64 ABI requires them to be passed BY VALUE on the stack (MEMORY class). This is a fundamental ABI mismatch that breaks interop between pcc-compiled and gcc-compiled code.
- **Evidence:** `PyStatus_Exception(PyStatus status)` — GCC reads from `16(%rbp)` (stack arg), PCC reads from `(%rdi)` (pointer deref). `PyStatus` = 32 bytes > 16 bytes.
- **Impact:** Every file that passes/receives large structs has wrong calling convention. `PyStatus` (32 bytes) is used in 100+ functions throughout CPython.
- **Fix required (DESIGN CHANGE):**
  1. **`classify_param()` in `cc/abi/sysv_amd64.rs`:** Large aggregate parameters should NOT use `ArgClass::Indirect` (which means "pass pointer"). They need a MEMORY/stack classification.
  2. **CALLER codegen (`cc/arch/x86_64/call.rs`):** Push struct bytes to the stack argument area (multiple `push` qwords) instead of passing pointer in register.
  3. **CALLEE codegen (`cc/arch/x86_64/codegen.rs`):** Receive large struct params via `IncomingArg` (stack offset) instead of GP register. The struct starts at `rbp + return_addr_offset + stack_arg_offset`.
  4. **CALLEE linearizer (`cc/ir/linearize.rs`):** Remove the "large struct param → pointer" conversion in `linearize_function_def`. Parameters should keep their struct type and be addressed via stack offsets.

### BUGs 1-6: Initializer bugs (ALL FIXED)
