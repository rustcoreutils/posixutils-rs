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
//! column's alignment (`l`/`r`/`c`/`n`) and optional width (`w(N)`),
//! tab-separated data rows, full-width rule rows (`_`/`=`), and `T{`…`T}` text
//! blocks. Spanning and box drawing are not modeled.

use crate::man_util::term::style::display_width;

/// Widest rule row a table may render. See the use site.
const MAX_TABLE_WIDTH: usize = 10_000;

/// Column alignment from a tbl format key letter.
#[derive(Clone, Copy, PartialEq)]
enum Align {
    Left,
    Right,
    Center,
    Numeric,
}

/// One column's format specification.
#[derive(Clone, Copy)]
struct ColSpec {
    align: Align,
    /// An explicit `w(N)` width, which overrides the computed default.
    width: Option<usize>,
}

/// One cell of a data row.
enum Cell {
    /// An ordinary cell: one line, as wide as its content.
    Simple(String),
    /// A `T{`…`T}` text block: prose to be filled to the column's width.
    Block(String),
}

/// One line of the format section: the column specs governing one data row.
type Format = Vec<ColSpec>;

/// One row of the table.
enum Row {
    /// A `_` or `=` line: a full-width rule.
    Rule,
    Cells(Vec<Cell>),
}

/// Lay out the body of a `.TS … .TE` region (the lines strictly between the
/// markers) into aligned text rows.
///
/// `line_length` is the width available to body text, and is used only to size
/// text blocks — see [`block_width`].
pub fn format(body: &[String], line_length: usize) -> Vec<String> {
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

    let (mut formats, data_start) = parse_format_section(body, idx);
    let rows = parse_rows(body, data_start, tab, &mut formats);

    let widest_format = formats.iter().map(Vec::len).max().unwrap_or(0);
    let ncols = rows
        .iter()
        .map(|r| match r {
            Row::Cells(c) => c.len(),
            Row::Rule => 0,
        })
        .max()
        .unwrap_or(0)
        .max(widest_format);

    // Fill every text block first: a block's rendered width is a consequence of
    // the width chosen for it, so the column cannot be sized until they are laid
    // out. This is the same ordering tbl uses — it diverts the block, lets the
    // formatter fill it, then reads the diversion's measured width back.
    let filled = fill_blocks(&rows, &formats, ncols, line_length);
    let widths = column_widths(&filled, ncols);
    let total = (widths.iter().sum::<usize>() + 2 * ncols.saturating_sub(1)).min(MAX_TABLE_WIDTH);

    render(&filled, &formats, &widths, total)
}

/// Parse the format section: lines up to and including the one ending in `.`.
///
/// Each line describes **one row**, and the last line governs every remaining
/// row. Flattening them into a single column list — as this did — made
/// systemctl(1)'s fifteen-line format section into a 45-column table, which in
/// turn gave every text block a fill width of `L/46`, i.e. one word per line.
fn parse_format_section(body: &[String], start: usize) -> (Vec<Format>, usize) {
    let mut idx = start;
    let mut formats: Vec<Format> = Vec::new();
    let mut terminated = false;

    while idx < body.len() {
        let line = body[idx].trim();
        let done = line.ends_with('.');
        let mut specs: Format = Vec::new();
        let mut chars = line.trim_end_matches('.').chars().peekable();
        while let Some(ch) = chars.next() {
            let align = match ch.to_ascii_lowercase() {
                'l' => Align::Left,
                'r' => Align::Right,
                'c' => Align::Center,
                'n' => Align::Numeric,
                // `s` spans from the cell to the left, `^` from the one above.
                // Spanning is not modeled, but both occupy a column position, so
                // they must be counted or the row's column count comes out wrong.
                's' | '^' => Align::Left,
                'w' => {
                    let w = parse_width_modifier(&mut chars);
                    if let (Some(w), Some(last)) = (w, specs.last_mut()) {
                        last.width = Some(w);
                    }
                    continue;
                }
                // Spaces, font modifiers (`B`/`I` are folded away by the
                // lowercasing above) and anything else are skipped.
                _ => continue,
            };
            specs.push(ColSpec { align, width: None });
        }
        if !specs.is_empty() {
            formats.push(specs);
        }
        idx += 1;
        if done {
            terminated = true;
            break;
        }
    }

    // The format section is required, but a page that omits it should still
    // render: without this the scan consumed the entire region looking for the
    // terminating `.`, and the table's whole content was silently dropped.
    if !terminated {
        return (Vec::new(), start);
    }
    (formats, idx)
}

/// The format governing one data row: its own line if the section gave one, else
/// the last line, which repeats.
fn format_for(formats: &[Format], data_row: usize) -> &[ColSpec] {
    if formats.is_empty() {
        return &[];
    }
    formats[data_row.min(formats.len() - 1)].as_slice()
}

/// Parse the argument of a `w` column modifier: `w(N)` or a bare `wN`, with an
/// optional and (on a terminal, meaningless) scaling unit.
fn parse_width_modifier(chars: &mut std::iter::Peekable<std::str::Chars>) -> Option<usize> {
    let parenthesized = chars.peek() == Some(&'(');
    if parenthesized {
        chars.next();
    }
    let mut digits = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_ascii_digit() {
            digits.push(c);
            chars.next();
        } else {
            break;
        }
    }
    // Consume a scaling unit and, if present, the closing paren.
    if matches!(
        chars.peek(),
        Some('i' | 'c' | 'p' | 'P' | 'm' | 'n' | 'v' | 'u' | 'f')
    ) {
        chars.next();
    }
    if parenthesized && chars.peek() == Some(&')') {
        chars.next();
    }
    digits.parse().ok()
}

/// Parse the data rows, gathering `T{`…`T}` text blocks.
///
/// A `T{` ends its field; the block's text runs to a line beginning `T}`, and
/// whatever follows that marker continues the row. A row may hold several
/// blocks, so this consumes lines rather than mapping one input line to one row.
///
/// A `.T&` line introduces a replacement format section governing the rows that
/// follow; `formats` is updated in place. Unrecognized, it was treated as data
/// and the format line itself printed as a table row — visible in systemctl(1),
/// which emits `.T&` after its heading row.
fn parse_rows(body: &[String], start: usize, tab: char, formats: &mut Vec<Format>) -> Vec<Row> {
    let mut rows = Vec::new();
    let mut i = start;

    while i < body.len() {
        let trimmed = body[i].trim();
        if trimmed == "_" || trimmed == "=" {
            rows.push(Row::Rule);
            i += 1;
            continue;
        }
        if trimmed == ".T&" {
            let (new_formats, next) = parse_format_section(body, i + 1);
            if !new_formats.is_empty() {
                *formats = new_formats;
            }
            i = next.max(i + 1);
            continue;
        }

        let mut cells: Vec<Cell> = Vec::new();
        // Fields still to consume from the line currently being read.
        let mut pending: Vec<String> = body[i].split(tab).map(str::to_string).collect();
        i += 1;

        while !pending.is_empty() {
            let field = pending.remove(0);
            if field.trim() != "T{" {
                cells.push(Cell::Simple(field.trim().to_string()));
                continue;
            }
            // Collect the block body up to a line starting with `T}`.
            let mut text: Vec<String> = Vec::new();
            while i < body.len() {
                let line = &body[i];
                if let Some(rest) = line.trim_start().strip_prefix("T}") {
                    i += 1;
                    // Anything after the marker belongs to this row.
                    let rest: Vec<String> = rest.split(tab).map(str::to_string).skip(1).collect();
                    pending.splice(0..0, rest);
                    break;
                }
                text.push(line.trim().to_string());
                i += 1;
            }
            cells.push(Cell::Block(text.join(" ").trim().to_string()));
        }
        rows.push(Row::Cells(cells));
    }
    rows
}

/// The width to fill a text block to.
///
/// Unlike an ordinary cell, a block's width cannot be measured from its content
/// — the content's extent is a consequence of the width. tbl therefore picks one
/// in advance: an explicit `w(N)` if the format spec gives one, else
/// `L * C / (N + 1)` where `L` is the line length, `C` the columns spanned (1
/// here, since spanning is not modeled) and `N` the number of columns. groff
/// emits exactly this as `.ll (u;\n[3wX]>?(\n[.l]*C/(N+1)))`.
fn block_width(spec: Option<&ColSpec>, ncols: usize, line_length: usize) -> usize {
    if let Some(w) = spec.and_then(|s| s.width) {
        return w.max(1);
    }
    (line_length / (ncols + 1)).max(1)
}

/// Lay every cell out into its final lines. Simple cells are one line; blocks
/// are filled to [`block_width`].
fn fill_blocks(
    rows: &[Row],
    formats: &[Format],
    ncols: usize,
    line_length: usize,
) -> Vec<Option<Vec<Vec<String>>>> {
    let mut data_row = 0;
    rows.iter()
        .map(|row| match row {
            Row::Rule => None,
            Row::Cells(cells) => {
                let specs = format_for(formats, data_row);
                data_row += 1;
                Some(
                    cells
                        .iter()
                        .enumerate()
                        .map(|(c, cell)| match cell {
                            Cell::Simple(s) => vec![s.clone()],
                            Cell::Block(s) => {
                                wrap(s, block_width(specs.get(c), ncols, line_length))
                            }
                        })
                        .collect(),
                )
            }
        })
        .collect()
}

/// Greedy fill of `text` to `width` terminal cells. A word wider than the column
/// overflows it rather than being broken, which is what the paragraph filler in
/// the terminal backend does too.
fn wrap(text: &str, width: usize) -> Vec<String> {
    let mut lines = Vec::new();
    let mut line = String::new();
    for word in text.split_whitespace() {
        if !line.is_empty() && display_width(&line) + 1 + display_width(word) > width {
            lines.push(std::mem::take(&mut line));
        }
        if !line.is_empty() {
            line.push(' ');
        }
        line.push_str(word);
    }
    if !line.is_empty() {
        lines.push(line);
    }
    if lines.is_empty() {
        lines.push(String::new());
    }
    lines
}

/// Each column is as wide as its widest rendered line, across every row.
fn column_widths(filled: &[Option<Vec<Vec<String>>>], ncols: usize) -> Vec<usize> {
    let mut widths = vec![0usize; ncols];
    for row in filled.iter().flatten() {
        for (c, cell) in row.iter().enumerate() {
            if let Some(w) = widths.get_mut(c) {
                *w = (*w).max(cell.iter().map(|l| display_width(l)).max().unwrap_or(0));
            }
        }
    }
    widths
}

/// Emit the laid-out table. A row is as tall as its tallest cell, so a text
/// block pushes its row down while its neighbours stay on the first line.
fn render(
    filled: &[Option<Vec<Vec<String>>>],
    formats: &[Format],
    widths: &[usize],
    total: usize,
) -> Vec<String> {
    let mut out = Vec::with_capacity(filled.len());
    let mut data_row = 0;
    for row in filled {
        let Some(row) = row else {
            out.push("-".repeat(total.max(1)));
            continue;
        };
        let specs = format_for(formats, data_row);
        data_row += 1;
        let height = row.iter().map(Vec::len).max().unwrap_or(1);
        for line_idx in 0..height {
            let mut line = String::new();
            for (c, cell) in row.iter().enumerate() {
                if c > 0 {
                    line.push_str("  ");
                }
                // Columns beyond the format line inherit the last declared
                // alignment; `specs` may be empty, so do not depend on it.
                let align = specs
                    .get(c)
                    .or_else(|| specs.last())
                    .map(|s| s.align)
                    .unwrap_or(Align::Left);
                let text = cell.get(line_idx).map(String::as_str).unwrap_or("");
                line.push_str(&pad(text, widths.get(c).copied().unwrap_or(0), align));
            }
            out.push(line.trim_end().to_string());
        }
    }
    out
}

/// Parse a `tab(x)` option from the options line.
///
/// Whitespace is allowed between the option name and its argument — man(1)'s own
/// page writes `tab (@);`, which a literal `"tab("` search misses. The rows then
/// stay unsplit, and with them every `T{` marker that should have begun a text
/// block.
fn parse_tab(line: &str) -> Option<char> {
    let after = &line[line.find("tab")? + 3..];
    let after = after.trim_start();
    let mut chars = after.strip_prefix('(')?.chars();
    chars.next().filter(|&c| c != ')')
}

/// Pad `cell` to `width` columns per `align`.
fn pad(cell: &str, width: usize, align: Align) -> String {
    let len = display_width(cell);
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

    /// Body-text width for the tests. Only text blocks are sensitive to it.
    const LL: usize = 72;

    fn run(body: &str) -> String {
        let lines: Vec<String> = body.lines().map(|s| s.to_string()).collect();
        format(&lines, LL).join("\n")
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

    #[test]
    fn rule_rows_do_not_blow_up_on_a_wide_table() {
        // A rule row is `total` dashes wide, and `total` follows the widest
        // cell. 2,000 rule rows next to one 200 KB cell cost 400 MB before the
        // width was capped.
        let mut body = String::from("l.\n");
        body.push_str(&"w".repeat(200_000));
        body.push('\n');
        for _ in 0..2_000 {
            body.push_str("_\n");
        }
        let out = run(&body);
        assert!(
            out.len() < 32 << 20,
            "rule rows unbounded: {} bytes",
            out.len()
        );
    }

    #[test]
    fn table_with_no_format_line_still_renders() {
        // The format section is terminated by a line ending in `.`. When a page
        // omits it, the scan used to consume the whole region looking for that
        // terminator, and the table's entire content was silently dropped.
        let out = run("a\tb\nc\td\n");
        assert_eq!(out, "a  b\nc  d");
    }

    // ── text blocks ──────────────────────────────────────────────────────

    #[test]
    fn text_block_is_filled_and_row_grows_downward() {
        // `T{`…`T}` used to pass through as literal cell content, markers and
        // all, on one enormous line.
        let out = run("l l.\nKey\tT{\nsome fairly long prose here that needs filling and wrapping\nT}\nOther\tshort\n");
        assert!(!out.contains("T{") && !out.contains("T}"), "{out}");
        // Width is L/(N+1) = 72/3 = 24, so the block occupies three lines and
        // the row is three lines tall; `Key` sits on the first of them.
        let lines: Vec<&str> = out.lines().collect();
        assert_eq!(lines.len(), 4, "{out}");
        assert!(lines[0].starts_with("Key "), "{out}");
        assert!(lines[1].starts_with("     "), "{out}");
        assert_eq!(lines[3], "Other  short");
        for l in &lines {
            assert!(l.chars().count() <= LL, "line over the width: {l:?}");
        }
    }

    #[test]
    fn text_block_default_width_follows_l_over_n_plus_one() {
        // The classic tbl rule, which groff emits literally as `\n[.l]*1/(N+1)`.
        let prose = "aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt uu vv ww xx";
        for (spec, ncols) in [("l.", 1), ("l l.", 2), ("l l l.", 3)] {
            let cells = "\t".repeat(ncols - 1);
            let body = format!("{spec}\nT{{\n{prose}\nT}}{cells}\n");
            let widest = run(&body)
                .lines()
                .map(|l| l.trim_end().chars().count())
                .max()
                .unwrap_or(0);
            let expected = LL / (ncols + 1);
            assert!(
                widest <= expected && widest > expected - 4,
                "{ncols} columns: filled to {widest}, expected about {expected}"
            );
        }
    }

    #[test]
    fn explicit_w_modifier_overrides_the_default_width() {
        let prose = "aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt";
        for spec in ["lw(30n).", "lw(30).", "lw30."] {
            let widest = run(&format!("{spec}\nT{{\n{prose}\nT}}\n"))
                .lines()
                .map(|l| l.trim_end().chars().count())
                .max()
                .unwrap_or(0);
            assert!(
                (27..=30).contains(&widest),
                "{spec}: filled to {widest}, expected about 30"
            );
        }
    }

    #[test]
    fn several_text_blocks_in_one_row() {
        // `T}` may be followed by more fields, including another `T{`.
        let out = run("l l.\nT{\nfirst block of prose\nT}\tT{\nsecond block of prose\nT}\n");
        assert!(!out.contains("T{") && !out.contains("T}"), "{out}");
        assert!(out.contains("first"), "{out}");
        assert!(out.contains("second"), "{out}");
    }

    #[test]
    fn unterminated_text_block_does_not_lose_its_content() {
        // A page that opens `T{` and never closes it: the text still renders
        // rather than the region being discarded.
        let out = run("l l.\nKey\tT{\nprose that never closes\n");
        assert!(out.contains("prose"), "{out}");
        assert!(!out.contains("T{"), "{out}");
    }

    #[test]
    fn format_change_line_is_not_data() {
        // `.T&` replaces the format for the rows that follow. Unrecognized, the
        // format line itself was printed as a table row: systemctl(1) shows
        // `l l l` in its rendered output.
        let out = run("lB lB.\nName\tDesc\n.T&\nl l.\nfoo\tbar\n");
        assert!(!out.contains(".T&"), "{out}");
        assert!(
            !out.lines().any(|l| l.trim() == "l l"),
            "format line as data: {out}"
        );
        assert_eq!(out, "Name  Desc\nfoo   bar");
    }

    #[test]
    fn multi_line_format_section_describes_rows_not_columns() {
        // Each format line governs one row; the last repeats. Concatenating
        // them made systemctl(1)'s fifteen-line section a 45-column table, so
        // every text block was filled to L/46 — one word per line.
        let prose = "aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp";
        // The `.` terminates the whole section, so it appears only on the last
        // format line; the ones before it are additional per-row formats.
        let body = format!("l l\nl l\nl l.\nK\tT{{\n{prose}\nT}}\n");
        let out = run(&body);
        let widest = out
            .lines()
            .map(|l| l.trim_end().chars().count())
            .max()
            .unwrap_or(0);
        // Two columns, so blocks fill to about LL/3 = 24, not LL/7.
        assert!(widest > 20, "block filled to only {widest} columns:\n{out}");
        assert!(
            !out.lines().any(|l| l.trim() == "l l"),
            "format line as data:\n{out}"
        );
    }

    #[test]
    fn span_key_letters_occupy_a_column() {
        // `s` and `^` are span markers, not decoration: they hold a column
        // position. Dropping them undercounted the table's columns, which is
        // what sizes a text block — `l s s.` is a three-column table, so a
        // block fills to LL/4, not the LL/3 a two-column table would give.
        let prose = "aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr";
        let widest = run(&format!("l s s.\nK\tT{{\n{prose}\nT}}\n"))
            .lines()
            .map(|l| l.trim_end().chars().count())
            .max()
            .unwrap_or(0);
        // Three columns: LL/4 = 18, plus the `K` column and its separator.
        assert!(
            widest <= 24,
            "block filled to {widest}; span columns were not counted"
        );
    }

    #[test]
    fn tab_option_tolerates_whitespace_before_its_argument() {
        // man(1)'s own page writes `tab (@);`. Requiring `tab(` left the rows
        // unsplit, so every `T{` stayed embedded in a field and no text block
        // was recognized.
        let out = run("tab (@);\nl lx.\n1@T{\nExecutable programs\nT}\n");
        assert!(!out.contains("T{") && !out.contains("T}"), "{out}");
        assert!(out.starts_with("1  Executable programs"), "{out}");
    }
}
