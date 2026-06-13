//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A `tbl` preprocessor: lay out `.TS … .TE` tables as aligned columns.
//!
//! Covers the common subset: an optional options line (`tab(x)` is honored,
//! other options ignored), one or more format lines ending in `.` giving each
//! column's alignment (`l`/`r`/`c`/`n`), tab-separated data rows, and full-width
//! rule rows (`_`/`=`). Spanning, `T{`…`T}` text blocks, and box drawing are not
//! modeled — their content is rendered inline.

/// Column alignment from a tbl format key letter.
#[derive(Clone, Copy, PartialEq)]
enum Align {
    Left,
    Right,
    Center,
    Numeric,
}

/// Lay out the body of a `.TS … .TE` region (the lines strictly between the
/// markers) into aligned text rows.
pub fn format(body: &[String]) -> Vec<String> {
    let mut idx = 0;
    let mut tab = '\t';

    // Optional options line, terminated by `;`.
    if let Some(line) = body.first() {
        if line.trim_end().ends_with(';') {
            if let Some(t) = parse_tab(line) {
                tab = t;
            }
            idx = 1;
        }
    }

    // Format section: lines up to and including the one ending in `.`.
    let mut aligns: Vec<Align> = Vec::new();
    while idx < body.len() {
        let line = body[idx].trim();
        let done = line.ends_with('.');
        for ch in line.trim_end_matches('.').chars() {
            match ch.to_ascii_lowercase() {
                'l' => aligns.push(Align::Left),
                'r' => aligns.push(Align::Right),
                'c' => aligns.push(Align::Center),
                'n' => aligns.push(Align::Numeric),
                _ => {} // spaces, span/modifier keys: ignored
            }
        }
        idx += 1;
        if done {
            break;
        }
    }
    if aligns.is_empty() {
        aligns.push(Align::Left);
    }

    // Data rows.
    let mut rows: Vec<Vec<String>> = Vec::new();
    let mut rules: Vec<bool> = Vec::new();
    for line in &body[idx..] {
        let trimmed = line.trim();
        if trimmed == "_" || trimmed == "=" {
            rows.push(Vec::new());
            rules.push(true);
            continue;
        }
        rows.push(line.split(tab).map(|c| c.trim().to_string()).collect());
        rules.push(false);
    }

    // Column widths.
    let ncols = rows
        .iter()
        .map(|r| r.len())
        .max()
        .unwrap_or(0)
        .max(aligns.len());
    let mut widths = vec![0usize; ncols];
    for row in &rows {
        for (c, cell) in row.iter().enumerate() {
            widths[c] = widths[c].max(cell.chars().count());
        }
    }
    let total = widths.iter().sum::<usize>() + 2 * ncols.saturating_sub(1);

    // Render.
    let mut out = Vec::with_capacity(rows.len());
    for (row, is_rule) in rows.iter().zip(rules) {
        if is_rule {
            out.push("-".repeat(total.max(1)));
            continue;
        }
        let mut line = String::new();
        for (c, cell) in row.iter().enumerate() {
            if c > 0 {
                line.push_str("  ");
            }
            let align = *aligns.get(c).unwrap_or(aligns.last().unwrap());
            line.push_str(&pad(cell, widths[c], align));
        }
        out.push(line.trim_end().to_string());
    }
    out
}

/// Parse a `tab(x)` option from the options line.
fn parse_tab(line: &str) -> Option<char> {
    let start = line.find("tab(")? + 4;
    line[start..].chars().next().filter(|&c| c != ')')
}

/// Pad `cell` to `width` columns per `align`.
fn pad(cell: &str, width: usize, align: Align) -> String {
    let len = cell.chars().count();
    if len >= width {
        return cell.to_string();
    }
    let fill = width - len;
    match align {
        Align::Left => format!("{cell}{}", " ".repeat(fill)),
        // Numeric is approximated as right alignment.
        Align::Right | Align::Numeric => format!("{}{cell}", " ".repeat(fill)),
        Align::Center => {
            let l = fill / 2;
            format!("{}{cell}{}", " ".repeat(l), " ".repeat(fill - l))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::format;

    fn run(body: &str) -> String {
        let lines: Vec<String> = body.lines().map(|s| s.to_string()).collect();
        format(&lines).join("\n")
    }

    #[test]
    fn simple_left_columns() {
        let out = run("l l.\nName\tValue\nfoobar\t1\n");
        assert_eq!(out, "Name    Value\nfoobar  1");
    }

    #[test]
    fn custom_tab_and_right_align() {
        let out = run("tab(@);\nl r.\na@1\nbbb@22\n");
        assert_eq!(out, "a     1\nbbb  22");
    }

    #[test]
    fn rule_row_spans_width() {
        let out = run("l l.\nA\tB\n_\nc\td\n");
        let lines: Vec<&str> = out.lines().collect();
        assert_eq!(lines[0], "A  B");
        assert!(lines[1].chars().all(|c| c == '-'));
        assert_eq!(lines[2], "c  d");
    }

    #[test]
    fn center_alignment() {
        let out = run("c.\nx\nyyyy\n");
        assert_eq!(out, " x\nyyyy");
    }
}
