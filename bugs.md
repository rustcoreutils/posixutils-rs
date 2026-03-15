# pcc Bug Log — CPython Build Campaign

## Status: _freeze_module works. _bootstrap_python still segfaults.

### Bugs A-I: ALL FIXED (see git history)

### BUG J: `__attribute__((packed))` ignored — FIXED
- **Files:** `cc/parse/parser.rs`, `cc/types.rs`
- **Root cause:** pcc used `skip_extensions()` to discard `__attribute__((packed))` on struct definitions. This caused `struct tracemalloc_frame` (packed: 12 bytes) to be laid out as 16 bytes, shifting ALL fields after `tracemalloc` in `_PyRuntimeState` by 8 bytes. Every runtime data access was corrupted.
- **Fix:** Parse `packed` attribute at all three positions (before tag, between tag and `{`, after `}`). Pass `packed: bool` to `compute_struct_layout()`, which suppresses alignment padding.
- **Verified:** `sizeof(packed_frame) = 12` matches gcc.

### BUG K: _bootstrap_python segfaults after packed fix — INVESTIGATING
- **Symptom:** After fixing packed struct layout (Bug J), `_bootstrap_python` still segfaults during importlib execution.
- **Previous symptoms suggest:** Either more struct layout mismatches remain, or another codegen bug (Bug I stale bytes?) is still at play.
- **Next step:** Use gdb to get crash location; compare `_PyRuntimeState` size again to verify packed fix resolved the 8-byte mismatch.

### BUGs 1-6: Initializer bugs (ALL FIXED)
