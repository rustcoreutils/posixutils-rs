# pcc Bug Log — CPython Build Campaign

## Status: Bugs A-N ALL FIXED. Testing CPython `make test`.

### Bugs A-N: ALL FIXED (see git history)

### Bug N: Large struct parameter ABI mismatch — FIXED
- **Root cause:** pcc passed struct parameters > 16 bytes as hidden pointers (in RDI), while the SysV AMD64 ABI requires them by value on the stack (MEMORY class).
- **Fix:** Caller pushes all qwords to stack. Callee receives via IncomingArg + SymAddr. Regalloc advances by full struct size. No GP register consumed.
- **Also fixed:** 32→64 bit store widening at offset 0 clobbered adjacent struct fields (compound literal + designated init failures).

### BUGs 1-6: Initializer bugs (ALL FIXED)
