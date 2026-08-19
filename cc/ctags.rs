//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// ctags - create a tags file
//
// POSIX ctags utility: creates a tags file for use with vi/ex editors.
// Recognizes function definitions, type definitions, and macros with arguments.
//

use clap::Parser;
use gettextrs::gettext;
use posixutils_cc::parse::ast::ExternalDecl;
use posixutils_cc::parse::Parser as CParser;
use posixutils_cc::strings::StringTable;
use posixutils_cc::symbol::SymbolTable;
use posixutils_cc::target::Target;
use posixutils_cc::token::{preprocess_with_defines, PreprocessConfig, StreamTable, Tokenizer};
use posixutils_cc::types::TypeTable;
use std::collections::BTreeMap;
use std::fs;
use std::io;
use std::path::Path;
use std::process::ExitCode;

// ============================================================================
// CLI
// ============================================================================

#[derive(Parser)]
#[command(version, about = gettext("ctags - create a tags file"))]
struct Args {
    /// Append to existing tags file
    #[arg(short = 'a', long, conflicts_with = "index", help = gettext("Append to tagsfile"))]
    append: bool,

    /// Write tags to specified file (default: tags)
    #[arg(short = 'f', long = "file", default_value = "tags", conflicts_with = "index",
          help = gettext("Write tags to specified file"))]
    tags_file: String,

    /// Print index to stdout instead of creating tags file
    #[arg(short = 'x', long, help = gettext("Print index to stdout"))]
    index: bool,

    #[arg(required = true, help = gettext("Input files"))]
    files: Vec<String>,
}

// ============================================================================
// Tag Entry
// ============================================================================

/// A tag entry representing a definition location
#[derive(Debug, Clone, PartialEq, Eq)]
struct TagEntry {
    /// The identifier name
    name: String,
    /// The source file
    file: String,
    /// Line number (1-based)
    line: u32,
    /// The line content (for search pattern)
    line_content: String,
}

impl TagEntry {
    /// Identity of a tag for duplicate-suppression purposes.
    ///
    /// Two definitions of the same name in different files (or on different
    /// lines of one file) are distinct tags and must both survive; only an
    /// exact re-discovery of the same location is a duplicate.
    fn key(&self) -> (&str, &str, u32) {
        (&self.name, &self.file, self.line)
    }

    /// Format as tags file line: identifier<TAB>filename<TAB>/^pattern$/
    fn format_tags(&self) -> String {
        // Escape slashes and backslashes in the pattern
        let pattern = self.line_content.replace('\\', "\\\\").replace('/', "\\/");
        format!("{}\t{}\t/^{}$/", self.name, self.file, pattern.trim_end())
    }

    /// Format as -x index line.
    ///
    /// POSIX STDOUT mandates exactly `"%s %d %s %s", <object-name>,
    /// <line-number>, <filename>, <text>` where `<text>` is "the text of line
    /// <line-number> of file <filename>" — single-space separated, and the
    /// line reproduced verbatim (leading indentation included).
    fn format_index(&self) -> String {
        format!(
            "{} {} {} {}",
            self.name,
            self.line,
            self.file,
            self.line_content.trim_end()
        )
    }
}

// ============================================================================
// Source File Processing
// ============================================================================

/// Get the line content for a given line number (1-based)
fn get_line_content(lines: &[String], line_num: u32) -> String {
    if line_num == 0 || line_num as usize > lines.len() {
        String::new()
    } else {
        lines[line_num as usize - 1].clone()
    }
}

/// Parse a C source file and extract tag entries
fn process_file(path: &str, streams: &mut StreamTable) -> io::Result<Vec<TagEntry>> {
    let mut tags = Vec::new();

    // Read file content once, one `char` per source byte.
    //
    // `String::from_utf8_lossy` was wrong here even though it kept the file's
    // tags: it replaces every invalid byte with U+FFFD, and those bytes end up
    // inside the emitted `/^...$/` search pattern — which then no longer
    // matches the line it points at, so the editor lands nowhere. Mapping each
    // byte to the char of the same value round-trips exactly (see
    // `bytes_from_latin1` at the write side), and it means the pattern is
    // reproduced byte-for-byte regardless of the source's encoding, which is
    // the behavior LC_CTYPE would otherwise have to select.
    let raw = fs::read(path)?;
    let content: String = raw.iter().map(|&b| b as char).collect();
    let lines: Vec<String> = content.lines().map(String::from).collect();

    // Extract macro tags first (before preprocessing removes them)
    extract_macro_tags(&lines, path, &mut tags);

    // Create stream
    let stream_id = streams.add(path.to_string());

    // Create string table for identifier interning
    let mut strings = StringTable::new();

    // Tokenize
    let tokens = {
        let mut tokenizer = Tokenizer::new(content.as_bytes(), stream_id, &mut strings);
        tokenizer.tokenize()
    };

    // Preprocess to handle #define, #include, etc.
    let target = Target::host();
    let preprocessed = preprocess_with_defines(
        tokens,
        &target,
        &mut strings,
        path,
        &PreprocessConfig::default(),
    );

    // Create symbol table and type table
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&target);

    // Parse
    // Strip the pragma markers the preprocessor leaves in the stream. This
    // tool computes no layout, but the markers are not C tokens and the
    // parser must not meet them.
    let mut preprocessed = preprocessed;
    let pack_directives =
        posixutils_cc::token::preprocess::extract_pragma_directives(&mut preprocessed);
    let mut parser = CParser::new(
        &preprocessed,
        &strings,
        &mut symbols,
        &mut types,
        pack_directives,
    );
    let ast = match parser.parse_translation_unit() {
        Ok(ast) => ast,
        Err(e) => {
            plib::diag::error(&format!("{}: {}: {}", path, gettext("parse error"), e));
            return Ok(tags);
        }
    };

    // Extract function definitions and declarations from AST
    for item in &ast.items {
        match item {
            ExternalDecl::FunctionDef(func) => {
                let name = strings.get(func.name).to_string();
                let line = func.pos.line;
                let line_content = get_line_content(&lines, line);

                // Special handling for main: prefix with M and strip .c suffix
                let tag_name = if name == "main" {
                    let stem = Path::new(path)
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("main");
                    format!("M{}", stem)
                } else {
                    name
                };

                tags.push(TagEntry {
                    name: tag_name,
                    file: path.to_string(),
                    line,
                    line_content,
                });
            }
            ExternalDecl::Declaration(decl) => {
                // Check symbol table for typedefs
                for declarator in &decl.declarators {
                    let sym = symbols.get(declarator.symbol);
                    let name = strings.get(sym.name).to_string();

                    // Check if it's a typedef
                    if sym.is_typedef() {
                        // Get line number from type or use default
                        // Since declarations don't have position, we scan for it
                        if let Some(line_num) = find_typedef_line(&lines, &name) {
                            let line_content = get_line_content(&lines, line_num);
                            tags.push(TagEntry {
                                name,
                                file: path.to_string(),
                                line: line_num,
                                line_content,
                            });
                        }
                    }
                }
            }
        }
    }

    Ok(tags)
}

/// Strip line comments and simple block-comment starts from a source line.
fn strip_comments(line: &str) -> &str {
    let mut code = line;
    if let Some(idx) = code.find("//") {
        code = &code[..idx];
    }
    if let Some(idx) = code.find("/*") {
        code = &code[..idx];
    }
    code
}

/// Split a line into identifier-like tokens.
fn identifiers(code: &str) -> impl Iterator<Item = &str> {
    code.split(|c: char| !(c.is_alphanumeric() || c == '_'))
        .filter(|t| !t.is_empty())
}

/// Whether `code` contains `typedef` as a standalone token.
fn has_typedef_keyword(code: &str) -> bool {
    identifiers(code).any(|t| t == "typedef")
}

/// Find the line number on which the typedef *name* `name` is declared.
///
/// A typedef declaration may span several lines — the extremely common
///
/// ```c
/// typedef struct {
///     int x;
/// } Foo;
/// ```
///
/// puts the `typedef` keyword and the declared name on different lines, so a
/// single-line "contains both" test never matches it. This walks the whole
/// typedef declaration (from the `typedef` keyword to the `;` that closes it at
/// brace depth zero) and reports the *last* line within that declaration on
/// which `name` occurs as an identifier — which is where the declarator sits,
/// and therefore the line whose text belongs in the tag's search pattern.
fn find_typedef_line(lines: &[String], name: &str) -> Option<u32> {
    let mut in_typedef = false;
    let mut depth: i32 = 0;
    let mut last_hit: Option<u32> = None;

    for (i, line) in lines.iter().enumerate() {
        let code = strip_comments(line);

        if !in_typedef {
            if !has_typedef_keyword(code) {
                continue;
            }
            in_typedef = true;
            depth = 0;
            last_hit = None;
        }

        if identifiers(code).any(|t| t == name) {
            last_hit = Some((i + 1) as u32);
        }

        for ch in code.chars() {
            match ch {
                '{' | '(' | '[' => depth += 1,
                '}' | ')' | ']' => depth -= 1,
                ';' if depth <= 0 => {
                    // Declaration complete.
                    if let Some(hit) = last_hit {
                        return Some(hit);
                    }
                    in_typedef = false;
                }
                _ => {}
            }
            if !in_typedef {
                break;
            }
        }
    }

    last_hit
}

/// Extract macro definitions with arguments from source
fn extract_macro_tags(lines: &[String], path: &str, tags: &mut Vec<TagEntry>) {
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        if trimmed.starts_with("#define") {
            // Parse #define NAME(args) or #define NAME value
            let rest = trimmed.strip_prefix("#define").unwrap().trim();
            if let Some(paren_pos) = rest.find('(') {
                // This is a function-like macro
                let name = rest[..paren_pos].trim();
                if !name.is_empty() && name.chars().all(|c| c.is_alphanumeric() || c == '_') {
                    tags.push(TagEntry {
                        name: name.to_string(),
                        file: path.to_string(),
                        line: (i + 1) as u32,
                        line_content: line.clone(),
                    });
                }
            }
        }
    }
}

// ============================================================================
// Main
// ============================================================================

fn main() -> ExitCode {
    plib::diag::init_locale("ctags");

    let args = Args::parse();

    // Collect all tags. Keyed by (name, file, line) rather than by name alone:
    // two files that each define `init` are two distinct tags, and dropping one
    // of them silently loses a definition the user asked for.
    let mut all_tags: BTreeMap<(String, String, u32), TagEntry> = BTreeMap::new();
    let mut streams = StreamTable::new();

    for file in &args.files {
        // Check file extension
        let ext = Path::new(file)
            .extension()
            .and_then(|s| s.to_str())
            .unwrap_or("");

        match ext {
            "c" | "h" => match process_file(file, &mut streams) {
                Ok(tags) => {
                    for tag in tags {
                        let (n, f, l) = tag.key();
                        all_tags.insert((n.to_string(), f.to_string(), l), tag);
                    }
                }
                Err(e) => {
                    plib::diag::error(&format!("{}: {}", file, plib::diag::io_error_text(&e)));
                }
            },
            "f" => {
                // FORTRAN is not mandated by POSIX.1-2024 (it appears only in
                // RATIONALE as something historical implementations understood).
                // Refusing it is a valid implementation-defined choice, but it
                // is still an error for this invocation.
                plib::diag::error(&format!(
                    "{}: {}",
                    file,
                    gettext("FORTRAN files not supported")
                ));
            }
            _ => {
                plib::diag::error(&format!("{}: {}", file, gettext("unknown file type")));
            }
        }
    }

    if args.index {
        // -x listing. POSIX ENVIRONMENT VARIABLES: LC_COLLATE determines "the
        // order in which output is sorted for the -x option" — unlike the tags
        // file below, which is fixed to the POSIX locale's collating sequence.
        let mut tags: Vec<&TagEntry> = all_tags.values().collect();
        tags.sort_by(|a, b| {
            plib::locale::strcoll(&a.name, &b.name)
                .then_with(|| a.file.cmp(&b.file))
                .then_with(|| a.line.cmp(&b.line))
        });
        for tag in tags {
            println!("{}", tag.format_index());
        }
    } else if args.append {
        // ctags.md 91292: "the file shall be sorted by identifier". A plain
        // append satisfies that only within the block it adds — the file as a
        // whole comes out as a run of separately-sorted blocks, which the
        // binary search in vi/ex relies on not happening. So merge with what
        // is already there and rewrite the whole file in order.
        let mut lines: Vec<String> = match fs::read(&args.tags_file) {
            Ok(existing) => existing
                .iter()
                .map(|&b| b as char)
                .collect::<String>()
                .lines()
                .map(String::from)
                .collect(),
            // No tags file yet is the ordinary case for the first -a.
            Err(e) if e.kind() == io::ErrorKind::NotFound => Vec::new(),
            Err(e) => {
                plib::diag::error(&format!(
                    "{}: {}: {}",
                    args.tags_file,
                    gettext("cannot open"),
                    e
                ));
                return ExitCode::from(1);
            }
        };
        lines.extend(all_tags.values().map(|t| t.format_tags()));
        // Byte order, as the tags file requires (the POSIX locale's collating
        // sequence), not LC_COLLATE — that governs -x only.
        lines.sort();
        lines.dedup();

        let mut body = String::new();
        for line in &lines {
            body.push_str(line);
            body.push('\n');
        }
        if let Err(e) =
            plib::io::write_atomic(Path::new(&args.tags_file), &bytes_from_latin1(&body))
        {
            plib::diag::error(&format!(
                "{}: {}: {}",
                args.tags_file,
                gettext("error writing"),
                e
            ));
            return ExitCode::from(1);
        }
    } else {
        // Fresh tags file: build it in memory and swap it into place, so an
        // interrupted or failed write cannot leave a truncated tags file behind
        // for a concurrently-running vi/ex to read.
        //
        // all_tags is keyed name-major, so iteration order is already the
        // POSIX-locale (byte) collating sequence the tags file requires.
        let mut body = String::new();
        for tag in all_tags.values() {
            body.push_str(&tag.format_tags());
            body.push('\n');
        }
        if let Err(e) =
            plib::io::write_atomic(Path::new(&args.tags_file), &bytes_from_latin1(&body))
        {
            plib::diag::error(&format!(
                "{}: {}: {}",
                args.tags_file,
                gettext("error writing"),
                e
            ));
            return ExitCode::from(1);
        }
    }

    posixutils_cc::tools::exit_code()
}

/// Undo the byte-per-`char` decoding used when the source was read.
///
/// Writing the `String` directly would UTF-8 encode any char above 0x7F,
/// turning one source byte into two and breaking the search pattern all over
/// again.
fn bytes_from_latin1(s: &str) -> Vec<u8> {
    s.chars().map(|c| c as u32 as u8).collect()
}
