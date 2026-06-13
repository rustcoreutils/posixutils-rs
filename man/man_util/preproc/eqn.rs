//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! An `eqn` preprocessor: linearize `.EQ … .EN` equations to ASCII.
//!
//! A pragmatic terminal rendering — `over` becomes `/`, `sup`/`sub` become
//! `^`/`_`, common functions and Greek letters map to Unicode — rather than a
//! full two-dimensional layout, which a terminal cannot reproduce.

/// Linearize the body of an `.EQ … .EN` region into text.
pub fn format(body: &[String]) -> Vec<String> {
    body.iter()
        .filter(|l| !is_directive(l))
        .map(|l| linearize(l))
        .filter(|l| !l.trim().is_empty())
        .collect()
}

/// `delim`, `define`, `gsize`, etc. are configuration, not content.
fn is_directive(line: &str) -> bool {
    matches!(
        line.split_whitespace().next(),
        Some("delim") | Some("define") | Some("gsize") | Some("gfont") | Some("ndefine")
    )
}

/// Linearize one equation line by mapping eqn keywords to ASCII/Unicode.
fn linearize(line: &str) -> String {
    let mut out: Vec<String> = Vec::new();
    let mut tokens = line.split_whitespace().peekable();
    while let Some(tok) = tokens.next() {
        match tok {
            // Binary operators that join the previous and next tokens.
            "over" => {
                if let Some(next) = tokens.next() {
                    let prev = out.pop().unwrap_or_default();
                    out.push(format!("{prev}/{}", symbol(next)));
                }
            }
            "sup" => {
                if let Some(next) = tokens.next() {
                    let prev = out.pop().unwrap_or_default();
                    out.push(format!("{prev}^{}", symbol(next)));
                }
            }
            "sub" => {
                if let Some(next) = tokens.next() {
                    let prev = out.pop().unwrap_or_default();
                    out.push(format!("{prev}_{}", symbol(next)));
                }
            }
            "sqrt" => {
                if let Some(next) = tokens.next() {
                    out.push(format!("\u{221a}{}", symbol(next)));
                }
            }
            // Grouping and spacing keywords are dropped.
            "left" | "right" | "{" | "}" | "~" | "^" => {}
            other => out.push(symbol(other)),
        }
    }
    out.join(" ")
}

/// Map an eqn name to its Unicode symbol, or return it unchanged.
fn symbol(tok: &str) -> String {
    let s = match tok {
        "alpha" => "α",
        "beta" => "β",
        "gamma" => "γ",
        "delta" => "δ",
        "epsilon" => "ε",
        "theta" => "θ",
        "lambda" => "λ",
        "mu" => "μ",
        "pi" => "π",
        "sigma" => "σ",
        "phi" => "φ",
        "omega" => "ω",
        "inf" | "infinity" => "∞",
        "sum" => "∑",
        "prod" => "∏",
        "int" => "∫",
        "partial" => "∂",
        "times" => "×",
        "cdot" => "·",
        "approx" => "≈",
        "!=" | "neq" => "≠",
        "<=" | "le" => "≤",
        ">=" | "ge" => "≥",
        "->" => "→",
        "<-" => "←",
        _ => tok,
    };
    s.to_string()
}

#[cfg(test)]
mod tests {
    use super::format;

    fn run(body: &str) -> String {
        let lines: Vec<String> = body.lines().map(|s| s.to_string()).collect();
        format(&lines).join("\n")
    }

    #[test]
    fn fraction_and_power() {
        assert_eq!(run("x over y"), "x/y");
        assert_eq!(run("x sup 2"), "x^2");
        assert_eq!(run("a sub i"), "a_i");
    }

    #[test]
    fn sqrt_and_symbols() {
        assert_eq!(run("sqrt x"), "√x");
        assert_eq!(run("alpha + beta"), "α + β");
        assert_eq!(run("sum x"), "∑ x");
    }

    #[test]
    fn directives_dropped() {
        assert_eq!(run("delim $$\nx over y"), "x/y");
    }
}
