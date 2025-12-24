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
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use posixutils_cc::parse::ast::ExternalDecl;
use posixutils_cc::parse::Parser as CParser;
use posixutils_cc::strings::StringTable;
use posixutils_cc::symbol::{Namespace, SymbolTable};
use posixutils_cc::target::Target;
use posixutils_cc::token::{preprocess_with_defines, StreamTable, Tokenizer};
use posixutils_cc::types::TypeTable;
use std::collections::BTreeMap;
use std::fs::{self, File, OpenOptions};
use std::io::{self, Write};
use std::path::Path;
use std::process::ExitCode;

// ============================================================================
// CLI
// ============================================================================

#[derive(Parser)]
#[command(version, about = gettext("ctags - create a tags file"))]
struct Args {
    /// Append to existing tags file
    #[arg(short = 'a', long, help = gettext("Append to tagsfile"))]
    append: bool,

    /// Write tags to specified file (default: tags)
    #[arg(short = 'f', long = "file", default_value = "tags", help = gettext("Write tags to specified file"))]
    tags_file: String,

    /// Print index to stdout instead of creating tags file
    #[arg(short = 'x', long, help = gettext("Print index to stdout"))]
    index: bool,

    /// Input files
    #[arg(required = true)]
    files: Vec<String>,
}

// ============================================================================
// Tag Entry
// ============================================================================

/// A tag entry representing a definition location
#[derive(Debug, Clone)]
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
    /// Format as tags file line: identifier<TAB>filename<TAB>/^pattern$/
    fn format_tags(&self) -> String {
        // Escape slashes and backslashes in the pattern
        let pattern = self.line_content.replace('\\', "\\\\").replace('/', "\\/");
        format!("{}\t{}\t/^{}$/", self.name, self.file, pattern.trim_end())
    }

    /// Format as -x index line: name line file text
    fn format_index(&self) -> String {
        format!(
            "{:<16} {:>6} {:<20} {}",
            self.name,
            self.line,
            self.file,
            self.line_content.trim()
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

    // Read file content once
    let content = fs::read_to_string(path)?;
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
        &[], // No extra defines
        &[], // No undefines
        &[], // No include paths
    );

    // Create symbol table and type table
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(target.pointer_width);

    // Parse
    let mut parser = CParser::new(&preprocessed, &strings, &mut symbols, &mut types);
    let ast = match parser.parse_translation_unit() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("ctags: {}: parse error: {}", path, e);
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
                    let name = strings.get(declarator.name).to_string();

                    // Look up in symbol table to determine if it's a typedef
                    if let Some(sym) = symbols.lookup(declarator.name, Namespace::Ordinary) {
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
    }

    Ok(tags)
}

/// Check whether a line contains a typedef definition for the given name.
///
/// This is a lightweight heuristic that:
/// - Ignores anything following `//` or `/*` on the line.
/// - Requires `typedef` to appear as a separate token.
/// - Requires the typedef name to appear as an identifier-like token
///   (allowing for trailing punctuation like `;` or `,`).
fn is_typedef_line(line: &str, name: &str) -> bool {
    // Strip off line comments and simple block comment starts.
    let mut code = line;
    if let Some(idx) = code.find("//") {
        code = &code[..idx];
    }
    if let Some(idx) = code.find("/*") {
        code = &code[..idx];
    }
    let code = code.trim();
    if code.is_empty() {
        return false;
    }
    let mut has_typedef = false;
    let mut has_name = false;
    for raw_token in code.split_whitespace() {
        if raw_token == "typedef" {
            has_typedef = true;
            continue;
        }
        // Strip trailing non-identifier characters (e.g., ';', ',', ')').
        let cleaned = raw_token.trim_end_matches(|c: char| !(c.is_alphanumeric() || c == '_'));
        if cleaned == name {
            has_name = true;
        }
    }
    has_typedef && has_name
}

/// Find the line number where a typedef is defined
fn find_typedef_line(lines: &[String], name: &str) -> Option<u32> {
    for (i, line) in lines.iter().enumerate() {
        if is_typedef_line(line, name) {
            return Some((i + 1) as u32);
        }
    }
    None
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
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").unwrap();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").unwrap();

    let args = Args::parse();

    // Collect all tags
    let mut all_tags: BTreeMap<String, TagEntry> = BTreeMap::new();
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
                        all_tags.insert(tag.name.clone(), tag);
                    }
                }
                Err(e) => {
                    eprintln!("ctags: {}: {}", file, e);
                }
            },
            "f" => {
                // FORTRAN support - not implemented
                eprintln!("ctags: {}: FORTRAN files not supported", file);
            }
            _ => {
                eprintln!("ctags: {}: unknown file type", file);
            }
        }
    }

    if args.index {
        // Print -x index to stdout
        for tag in all_tags.values() {
            println!("{}", tag.format_index());
        }
    } else {
        // Write tags file
        let open_result = if args.append {
            OpenOptions::new()
                .create(true)
                .append(true)
                .open(&args.tags_file)
        } else {
            File::create(&args.tags_file)
        };

        match open_result {
            Ok(mut file) => {
                for tag in all_tags.values() {
                    if let Err(e) = writeln!(file, "{}", tag.format_tags()) {
                        eprintln!("ctags: error writing to {}: {}", args.tags_file, e);
                        return ExitCode::from(1);
                    }
                }
            }
            Err(e) => {
                eprintln!("ctags: cannot open {}: {}", args.tags_file, e);
                return ExitCode::from(1);
            }
        }
    }

    ExitCode::SUCCESS
}
