//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((mode(M)))` through the driver: a mode of another type
// class, a mode no pointer has and a name that is no mode are gcc's errors,
// and a mode of the declared type's own class is still applied.
//

use crate::common::{compile_and_run_everywhere, compile_expect_error};

#[test]
fn mode_attribute_of_another_class_is_rejected() {
    for (name, src, msg) in [
        (
            "mode_df_on_int",
            "int x __attribute__((mode(DF)));\n",
            "mode 'DF' applied to inappropriate type",
        ),
        (
            "mode_si_on_float",
            "float x __attribute__((mode(SI)));\n",
            "mode 'SI' applied to inappropriate type",
        ),
        (
            "mode_unknown",
            "int x __attribute__((mode(FOO)));\n",
            "unknown machine mode 'FOO'",
        ),
        (
            "mode_pointer",
            "int *p __attribute__((mode(SI)));\n",
            "invalid pointer mode 'SI'",
        ),
        (
            "mode_enum",
            "enum E { A } e __attribute__((mode(SF)));\n",
            "cannot use mode 'SF' for enumerated types",
        ),
    ] {
        compile_expect_error(name, src, msg);
    }
}

#[test]
fn mode_attribute_of_the_same_class_is_applied() {
    let src = "typedef int I8 __attribute__((mode(QI)));\n\
               typedef unsigned U16 __attribute__((mode(HI)));\n\
               typedef float F64 __attribute__((mode(DF)));\n\
               typedef int *P __attribute__((mode(DI)));\n\
               enum E { A } e __attribute__((mode(QI)));\n\
               int main(void) {\n\
                   int v = 7; P p = &v;\n\
                   U16 u = 65535; u++;\n\
                   return sizeof(I8) == 1 && sizeof(F64) == 8 && sizeof(P) == 8\n\
                       && sizeof e == 1 && *p == 7 && u == 0 && (F64)0.1 == 0.1 ? 0 : 1;\n\
               }\n";
    compile_and_run_everywhere("mode_same_class", src);
}
