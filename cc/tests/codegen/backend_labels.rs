//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The labels a back end makes for its own branch sequences never equal a
// basic block's. x86-64's unsigned conversions and its SSE2 rounding named
// theirs as blocks 10000 and up of the function, so a function of more than
// 10000 blocks defined `.Lf_10000` twice and the assembler refused it.
//

use crate::common::compile_and_run_everywhere;

/// A function of some 12000 blocks, then each sequence that took a high
/// block number: `unsigned long` to `double` and back, `long double` to
/// `unsigned long`, and `floor` without SSE4.1.
#[test]
fn backend_labels_never_equal_a_block_label() {
    let mut src = String::from(
        "void g(int x) { (void)x; }\n\
         unsigned long f(volatile int *v, unsigned long u, double d, long double ld) {\n",
    );
    for k in 0..6000 {
        src += &format!("    if (v[{}] == {k}) g({k});\n", k % 7);
    }
    src += "    double back = (double)u;\n\
            unsigned long a = (unsigned long)d, b = (unsigned long)ld;\n\
            return (unsigned long)back + a + b + (unsigned long)__builtin_floor(d);\n\
            }\n\
            int main(void) {\n\
            static volatile int v[7];\n\
            unsigned long big = 1UL << 63;\n\
            if (f(v, big, 3.5, 2.0L) != big + 3 + 2 + 3) return 1;\n\
            return 0;\n\
            }\n";
    compile_and_run_everywhere("backend_labels", &src);
}
