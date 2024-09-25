//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

#[cfg(target_os = "macos")]
fn main() {
    use std::env;
    use std::path::Path;

    let bindings = bindgen::builder()
        .header_contents("libproc_rs.h", "#include <libproc.h>")
        .layout_tests(false)
        .clang_args(&[
            "-x",
            "c++",
            "-I",
            "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/",
        ])
        .generate()
        .expect("Failed to build libproc bindings");

    let output_path = Path::new(&env::var("OUT_DIR").expect("OUT_DIR env var was not defined"))
        .join("osx_libproc_bindings.rs");

    bindings
        .write_to_file(output_path)
        .expect("Failed to write libproc bindings");
}

#[cfg(target_os = "linux")]
fn main() {}
