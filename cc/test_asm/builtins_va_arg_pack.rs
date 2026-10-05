//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly and compile-only cases of tests/builtins/va_arg_pack.rs, in process.
//

use crate::test_compile::compile_expect_error;

/// When the inliner refuses an `always_inline` forwarder for a reason of its
/// own -- the callee also keeps a `va_list` of its own -- the pack has nothing
/// to be resolved against and the body has already been suppressed.
///
/// The guard that was supposed to catch this asked `func.emit &&
/// forwards_caller_arguments(func)`, which no function can satisfy:
/// `suppress_forwarding_bodies` clears `emit` on exactly the set the predicate
/// accepts, and nothing sets it back. So it never fired, and the program
/// failed at *link* time with `undefined reference to 'wrap'` and no
/// diagnostic. What is wrong is a surviving call site, so that is what the
/// check looks at now.
#[test]
fn builtins_unresolvable_va_arg_pack_is_diagnosed() {
    // Refused because the callee uses a va_list of its own. gcc reports this
    // too: "can never be inlined because it uses variable argument lists".
    // `alloca` used to land here as well; it now inlines, which is why
    // `builtins_va_arg_pack_forwarder_may_use_alloca` exists.
    compile_expect_error(
        "va_arg_pack_with_va_start",
        "#include <stdarg.h>\n\
         extern int printf(const char *, ...);\n\
         __attribute__((always_inline)) static inline int wrap(const char *f, ...) {\n\
         va_list ap; va_start(ap, f); (void)va_arg(ap, int); va_end(ap);\n\
         return printf(f, __builtin_va_arg_pack()); }\n\
         int main(void) { return wrap(\"%d\\n\", 1); }\n",
        "could not be forwarded",
    );
}
