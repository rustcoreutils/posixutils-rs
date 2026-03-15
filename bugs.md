# pcc Bug Log — CPython Build Campaign

## Status: _freeze_module clean. All .c files compile. _bootstrap_python crashes — systematic codegen issue.

### Bugs A-M: ALL FIXED (see git history)

### BUG N: Systematic codegen issue — every file individually breaks _bootstrap_python — INVESTIGATING
- **Pattern:** From a clean gcc baseline, swapping ANY single file to pcc (9/9 tested) causes _bootstrap_python to crash. This is the SAME pattern seen throughout this campaign.
- **NOT stale stack bytes:** Stack zeroing with `rep stosq` doesn't fix it.
- **NOT struct layout:** All struct sizes/offsets match gcc exactly.
- **NOT the linker:** The issue persists with gcc linker.
- **NOT pyconfig.h:** pyconfig.h differs only in GETPGRP_HAVE_ARG/SETPGRP_HAVE_ARG (posixmodule only).
- **Key observation:** A clean `make clean && CC=gcc make _bootstrap_python` with the SAME pyconfig.h (pcc-compatible, 5 patches applied) works perfectly.
- **Next step:** Compare pcc vs gcc assembly for a single small function in a breaking file (e.g., a simple function from initconfig.c). The systematic nature suggests a fundamental codegen pattern that's wrong — likely in how pcc handles common C constructs that appear in virtually every file.

### BUGs 1-6: Initializer bugs (ALL FIXED)
