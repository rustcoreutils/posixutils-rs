//
// Copyright (c) 2024 Bloq Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

// Line transforms (-e and -i).
//
// It is more efficient to do these while the line is being written, but that
// makes them harder to unit test.
//
// On coreutils, tabbing is affected by numbering lines (-n), except on merge
// mode, but not by offsets (-o). It is done per column and not on the whole
// line simultaneously.

pub fn expand_tabs(output_line: &mut String, char_and_width: Option<(char, usize)>) {
    if let Some((target_char, width)) = char_and_width {
        let input = output_line.clone();
        output_line.clear();

        let mut i = 0;
        for c in input.chars() {
            if c == target_char {
                loop {
                    output_line.push(' ');
                    i += 1;
                    if i % width == 0 {
                        break;
                    }
                }
            } else {
                output_line.push(c);
                i += 1;
            }
        }
    }
}

pub fn replace_spaces(output_line: &mut String, char_and_width: Option<(char, usize)>) {
    if let Some((replacement, width)) = char_and_width {
        let input = output_line.clone();
        output_line.clear();

        let mut buffer = String::new();

        for (i, c) in input.chars().enumerate() {
            if i % width == 0 && !buffer.is_empty() {
                buffer.clear();
                output_line.push(replacement);
            }
            if c == ' ' {
                buffer.push(c);
            } else {
                output_line.push_str(&buffer);
                output_line.push(c);
                buffer.clear();
            }
        }
    }
}

#[test]
fn test_pr_expand_tabs() {
    let chars = ['\t', 'x', 'y', 'z'];

    let width = 8;
    for target_char in chars {
        let data = vec![
            (format!("a{0}b{0}c", target_char), "a       b       c"),
            (format!("a    {0}b{0}c", target_char), "a       b       c"),
            (
                format!("a               {0}b{0}c", target_char),
                "a                       b       c",
            ),
        ];

        for (mut input, result) in data {
            expand_tabs(&mut input, Some((target_char, width)));
            assert_eq!(input, *result);
        }
    }

    let width = 4;
    for target_char in chars {
        let data = vec![
            (format!("a{0}b{0}c", target_char), "a   b   c"),
            (format!("a {0}b{0}c", target_char), "a   b   c"),
            (format!("a   {0}b{0}c", target_char), "a       b   c"),
        ];

        for (mut input, result) in data {
            expand_tabs(&mut input, Some((target_char, width)));
            assert_eq!(input, *result);
        }
    }
}

#[test]
fn test_pr_replace_spaces() {
    let chars = ['\t', 'x', 'y', 'z'];

    let width = 8;
    for replacement in chars {
        let data = vec![
            ("a    b    c", format!("a    b{0}  c", replacement)),
            ("a     b     c", format!("a     b{0}    c", replacement)),
            ("a      b      c", String::from("a      b      c")),
            ("a       b       c", format!("a{0}b{0}c", replacement)),
        ];

        for (s, result) in data {
            let mut input = String::from(s);
            replace_spaces(&mut input, Some((replacement, width)));
            assert_eq!(input, *result);
        }
    }

    let width = 3;
    for replacement in chars {
        let data = vec![
            ("a    b    c", format!("a{0}  b{0} c", replacement)),
            ("a     b     c", format!("a{0}{0}b{0}{0}c", replacement)),
            (
                "a      b      c",
                format!("a{0}{0} b{0}{0}  c", replacement),
            ),
            (
                "a       b       c",
                format!("a{0}{0}  b{0}{0} c", replacement),
            ),
        ];

        for (s, result) in data {
            let mut input = String::from(s);
            replace_spaces(&mut input, Some((replacement, width)));
            assert_eq!(input, *result);
        }
    }
}
