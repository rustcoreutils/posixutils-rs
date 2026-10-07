//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU attributes on enumerators
//

use crate::common::compile_and_run;

/// System headers mark an old enumerator name deprecated between the name and
/// its `=`: systemd's <sd-journal.h> writes `SD_JOURNAL_SYSTEM_ONLY
/// __attribute__((__deprecated__)) = SD_JOURNAL_SYSTEM`, and <lz4frame.h>
/// does the same through `LZ4F_DEPRECATE`. util-linux and libzstd include
/// them, and c17 stopped at the attribute. The enumerators keep their values.
#[test]
fn misc_enumerator_attributes() {
    let code = r#"
enum flags {
    LOCAL = 1 << 0,
    SYSTEM = 1 << 2,
    SYSTEM_ONLY __attribute__((__deprecated__)) = SYSTEM,
    NEXT __attribute__((deprecated)),
    LAST __attribute__((deprecated("use NEXT"))) __attribute__((unused))
};
int main(void) {
    if (LOCAL != 1 || SYSTEM != 4) return 1;
    if (NEXT != 5 || LAST != 6) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("enumerator_attributes", code, &[]), 0);
}
