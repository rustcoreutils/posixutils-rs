//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A syntax error is reported where it is: in the file that holds it, with the
// chain of includes that reached it. It was reported against the operand with
// the header's line and column, so util-linux's `logger.c: 78:32` pointed at
// a line of logger.c that has no `__attribute__` -- the error was in
// systemd's sd-journal.h.
//

use crate::common::run_c17;

#[test]
fn parse_error_names_the_header_that_holds_it() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_parse_error_loc_")
        .tempdir()
        .unwrap();
    std::fs::write(dir.path().join("h.h"), "int a;\nint b = ;\n").unwrap();
    let src = dir.path().join("m.c");
    std::fs::write(&src, "#include \"h.h\"\nint main(void) { return 0; }\n").unwrap();
    let r = run_c17(&["-c", "-o", "/dev/null", &src.to_string_lossy()]);
    assert!(!r.success, "a syntax error compiled");
    assert!(
        r.stderr.contains("h.h:2:9: error:"),
        "the error does not name the header:\n{}",
        r.stderr
    );
    assert!(
        r.stderr.contains("in included file (through ") && r.stderr.contains("m.c)"),
        "the include chain is missing:\n{}",
        r.stderr
    );
    assert!(
        !r.stderr.contains("m.c: parse error: 2:9"),
        "the header's position was given to the operand:\n{}",
        r.stderr
    );
}
