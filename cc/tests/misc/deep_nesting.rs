//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Expressions nested ten thousand parentheses deep, as gcc's torture test
// compile/limits-exprparen nests them. Each level is a full trip down the
// expression grammar, so the depth the parser reaches is the nesting times
// the frames one level costs.
//

use crate::common::compile_and_run;

/// Ten thousand nested parentheses compile, and the value inside them
/// survives every operator level around it. A debug build of c17 overflowed
/// its 256 MiB compiler stack here -- a release build has smaller frames --
/// because each level went through a frame per binary precedence level, ten
/// of them, on the way to the parenthesis.
#[test]
fn misc_ten_thousand_nested_parentheses() {
    let open = "(".repeat(10_000);
    let close = ")".repeat(10_000);
    let src = format!(
        "int deep = {open}7{close};\n\
         int mixed = {open}1 + 2 * 3 - (8 >> 1) % 3 | 4 ^ 1 && 5 || 0{close};\n\
         int main(void) {{ return deep == 7 && mixed == 1 ? 0 : 1; }}\n"
    );
    assert_eq!(compile_and_run("deep_parens", &src, &[]), 0);
}
