# pcc Bug Log — CPython Build Campaign

## Status: _freeze_module clean. Bug M root cause found: pcc silently produces corrupt .o files on unsupported expressions.

### Bugs A-L: ALL FIXED (see git history)

### BUG M (root cause): pcc exits 0 on compilation errors, producing corrupt .o files — PARTIALLY FIXED
- **Files:** `cc/main.rs` — missing error check after linearization
- **Root cause:** pcc emits "error: unsupported expression in global initializer" during linearization but doesn't fail the compilation (exits 0). The error output goes to stderr but the .o file is created with corrupt/missing data.
- **Fix:** Added `diag::has_error()` check after linearization in `process_file()`. pcc now correctly fails with non-zero exit on linearization errors.
- **Remaining:** The "unsupported expression in global initializer" error itself needs to be fixed. The expression pattern is `Py_CLEAR(ptr->member)` which expands to `&((ptr)->member)` — an address-of on a member access through a pointer. This appears in `pycore_pyerrors.h` and is used by `pylifecycle.c`, `import.c`, `ceval.c`, `sysmodule.c`.
- **Next step:** Fix `eval_static_address()` or `ast_init_to_ir()` to handle the `Member { Arrow { ... } }` pattern in the global initializer evaluator, OR properly detect that this expression is in a FUNCTION BODY (not a global initializer) and use the normal codegen path.

### BUGs 1-6: Initializer bugs (ALL FIXED)
