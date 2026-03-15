# pcc Bug Log — CPython Build Campaign

## Status: _freeze_module clean. _bootstrap_python crashes — 4 specific files break it when pcc-compiled.

### Bugs A-L: ALL FIXED (see git history)

### BUG L (revised): Deref of small UNION returns pointer instead of value — FIXED
- **Fix revised:** Only UNION types <= 64 bits get the value-load treatment. STRUCT types still return addresses (needed for member-offset access). Large unions (> 64 bits) still return addresses.

### DESIGN FIX: Stack frame zero-initialization
- All function prologues zero the stack frame using `rep stosq`
- Saves/restores RDI+RCX around zeroing to preserve function arguments

### BUG M: 4 specific files break _bootstrap_python — INVESTIGATING
- **Files:** `pylifecycle.c`, `import.c`, `ceval.c`, `sysmodule.c`
- **Other files (pystate.c, initconfig.c, marshal.c, Objects/*.c) work fine**
- **Key observation:** `pylifecycle.c` has ZERO union usage — the bug is NOT related to Bug L
- **Common trait:** All 4 files are in the init/execution path. They use `PyStatus` extensively (196 uses in pylifecycle.c alone) and make many function calls.
- **Next step:** Compare pcc vs gcc assembly for a small function in pylifecycle.c to find the codegen difference.

### BUGs 1-6: Initializer bugs (ALL FIXED)
