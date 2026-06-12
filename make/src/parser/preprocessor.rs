use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::iter::Peekable;
use std::path::Path;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::Acquire;

#[derive(Debug)]
pub enum PreprocError {
    EmptyIdent,
    UnexpectedEOF,
    UnexpectedSymbol(char),
    TooManyColons,
    BadAssignmentOperator(char),
    CommandFailed,
    UndefinedMacro(String),
    BadMacroName,
    IncludeFailed { path: String, reason: String },
}

impl Display for PreprocError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            // A missing/unreadable include is a user-facing condition, so give
            // it a readable message rather than the debug representation.
            PreprocError::IncludeFailed { path, reason } => {
                writeln!(f, "cannot open include file '{path}': {reason}")
            }
            other => writeln!(f, "{:?}", other),
        }
    }
}

impl std::error::Error for PreprocError {}

type Result<T> = std::result::Result<T, PreprocError>;

fn skip_blank(letters: &mut Peekable<impl Iterator<Item = char>>) {
    while let Some(letter) = letters.peek() {
        if !letter.is_whitespace() {
            break;
        };
        letters.next();
    }
}

fn suitable_ident(c: &char) -> bool {
    c.is_alphanumeric() || matches!(c, '_' | '.')
}

/// Decide whether a whole makefile line is a macro definition, as opposed to a
/// recipe line, a rule header, or arbitrary text.
///
/// The preprocessor scans the file line-by-line for macro definitions and also
/// strips those lines before the rule parser runs. Both passes must agree, and
/// both must avoid misclassifying ordinary content: a recipe line is indented
/// with a `<tab>` and frequently contains `=` (shell assignments, `--opt=val`,
/// `test x = y`), and such a line is never a macro definition. A line is a
/// macro definition only when it is not a recipe line and the text preceding
/// the assignment operator is a single valid macro name (optionally prefixed
/// with `export`).
fn is_macro_definition(line: &str) -> bool {
    // Recipe lines are tab-indented and are never macro definitions.
    if line.starts_with('\t') {
        return false;
    }
    let Some(eq) = line.find('=') else {
        return false;
    };
    // Drop a trailing assignment-operator prefix (`:`, `?`, `+`, `!`) so that
    // `:=`, `::=`, `:::=`, `?=`, `+=`, and `!=` all reduce to the name.
    let mut name = line[..eq].trim_end_matches([':', '?', '+', '!']).trim();
    if let Some(rest) = name.strip_prefix("export") {
        // Require whitespace after `export` so `exported=1` is not mis-split.
        if rest.starts_with(char::is_whitespace) {
            name = rest.trim();
        }
    }
    !name.is_empty() && name.chars().all(|c| suitable_ident(&c))
}

fn get_ident(letters: &mut Peekable<impl Iterator<Item = char>>) -> Result<String> {
    let mut ident = String::new();

    while let Some(letter) = letters.peek() {
        if !suitable_ident(letter) {
            break;
        };
        ident.push(*letter);
        letters.next();
    }

    if ident.is_empty() {
        Err(PreprocError::EmptyIdent)
    } else {
        Ok(ident)
    }
}

fn take_till_eol(letters: &mut Peekable<impl Iterator<Item = char>>) -> String {
    let mut content = String::new();

    while let Some(letter) = letters.peek() {
        if matches!(letter, '\n' | '#') {
            break;
        };
        content.push(*letter);
        letters.next();
    }

    content
}

/// Searches for all the lines in makefile that resemble macro definition
/// and creates hashtable from macro names and bodies
fn generate_macro_table(
    source: &str,
) -> std::result::Result<HashMap<String, String>, PreprocError> {
    let macro_defs = source.lines().filter(|line| is_macro_definition(line));
    let mut macro_table = HashMap::<String, String>::new();

    for def in macro_defs {
        enum Operator {
            Equals,
            Colon,
            Colon2,
            Colon3,
            Bang,
            QuestionMark,
            Plus,
        }

        let mut text = def.chars().peekable();

        let mut macro_name = get_ident(&mut text)?;
        if macro_name == "export" {
            skip_blank(&mut text);
            macro_name = get_ident(&mut text)?;
        }
        skip_blank(&mut text);
        let Some(symbol) = text.next() else {
            Err(PreprocError::UnexpectedEOF)?
        };
        let operator = match symbol {
            '=' => Operator::Equals,
            ':' => {
                let mut count = 1;
                while let Some(':') = text.peek() {
                    count += 1;
                    text.next();
                }
                let Some('=') = text.next() else {
                    Err(PreprocError::BadAssignmentOperator(':'))?
                };

                match count {
                    1 => Operator::Colon,
                    2 => Operator::Colon2,
                    3 => Operator::Colon3,
                    _ => Err(PreprocError::TooManyColons)?,
                }
            }
            '!' => {
                let Some('=') = text.next() else {
                    Err(PreprocError::BadAssignmentOperator('!'))?
                };
                Operator::Bang
            }
            '?' => {
                let Some('=') = text.next() else {
                    Err(PreprocError::BadAssignmentOperator('?'))?
                };
                Operator::QuestionMark
            }
            '+' => {
                let Some('=') = text.next() else {
                    Err(PreprocError::BadAssignmentOperator('+'))?
                };
                Operator::Plus
            }
            c => Err(PreprocError::UnexpectedSymbol(c))?,
        };
        skip_blank(&mut text);
        let mut macro_body = take_till_eol(&mut text);

        match operator {
            Operator::Equals => {}
            Operator::Colon | Operator::Colon2 => loop {
                let (result, substitutions) = substitute(&macro_body, &macro_table)?;
                if substitutions == 0 {
                    break;
                } else {
                    macro_body = result
                }
            },
            Operator::Colon3 => {
                macro_body = substitute(&macro_body, &macro_table)?.0;
            }
            Operator::Bang => {
                macro_body = substitute(&macro_body, &macro_table)?.0;
                let Ok(result) = std::process::Command::new("sh")
                    .args(["-c", &macro_body])
                    .output()
                else {
                    Err(PreprocError::CommandFailed)?
                };
                macro_body = String::from_utf8_lossy(&result.stdout).to_string();
            }
            Operator::QuestionMark => {
                if let Some(body) = macro_table.remove(&macro_name) {
                    macro_body = body
                }
            }
            Operator::Plus => {
                if let Some(body) = macro_table.remove(&macro_name) {
                    macro_body = format!("{} {}", body, macro_body);
                }
            }
        }

        macro_table.insert(macro_name, macro_body);
    }

    Ok(macro_table)
}

pub static ENV_MACROS: AtomicBool = AtomicBool::new(false);

fn substitute(source: &str, table: &HashMap<String, String>) -> Result<(String, u32)> {
    let env_macros = ENV_MACROS.load(Acquire);

    let mut substitutions = 0;
    let mut result = String::with_capacity(source.len());

    let mut letters = source.chars().peekable();
    while let Some(letter) = letters.next() {
        if letter != '$' {
            result.push(letter);
            continue;
        }

        let Some(letter) = letters.next() else {
            Err(PreprocError::UnexpectedEOF)?
        };

        match letter {
            // Internal macros - we leave them "as is"
            // yet as they will be dealt with in the
            // parsing stage with more context available
            c @ ('$' | '@' | '%' | '?' | '<' | '*') => {
                result.push('$');
                result.push(c);
                continue;
            }
            c if suitable_ident(&c) => {
                let env_macro = if env_macros {
                    std::env::var(c.to_string()).ok()
                } else {
                    None
                };
                let table_macro = table.get(&c.to_string()).cloned();
                let Some(macro_body) = env_macro.or(table_macro) else {
                    Err(PreprocError::UndefinedMacro(c.to_string()))?
                };
                result.push_str(&macro_body);
                substitutions += 1;
                continue;
            }
            '(' | '{' => {
                skip_blank(&mut letters);
                let Ok(macro_name) = get_ident(&mut letters) else {
                    Err(PreprocError::BadMacroName)?
                };
                skip_blank(&mut letters);
                let Some(finilizer) = letters.next() else {
                    Err(PreprocError::UnexpectedEOF)?
                };
                if !matches!(finilizer, ')' | '}') {
                    Err(PreprocError::UnexpectedSymbol(finilizer))?
                }

                let env_macro = if env_macros {
                    std::env::var(&macro_name).ok()
                } else {
                    None
                };
                let table_macro = table.get(&macro_name).cloned();
                let Some(macro_body) = env_macro.or(table_macro) else {
                    Err(PreprocError::UndefinedMacro(macro_name.to_string()))?
                };
                result.push_str(&macro_body);
                substitutions += 1;

                continue;
            }
            c => Err(PreprocError::UnexpectedSymbol(c))?,
        }
    }

    Ok((result, substitutions))
}

/// Copy-pastes included makefiles into single one recursively.
/// Pretty much the same as C preprocessor and `#include` directive
fn process_include_lines(source: &str, table: &HashMap<String, String>) -> Result<(String, usize)> {
    let mut counter = 0;
    let mut result = String::new();
    for line in source.lines() {
        let expanded = if let Some(s) = line.strip_prefix("include") {
            counter += 1;
            let s = s.trim();
            let (path, _) = substitute(s, table).unwrap_or_default();
            match fs::read_to_string(Path::new(&path)) {
                Ok(contents) => contents,
                Err(err) => {
                    return Err(PreprocError::IncludeFailed {
                        path,
                        reason: err.to_string(),
                    })
                }
            }
        } else {
            line.to_string()
        };
        result.push_str(&expanded);
        result.push('\n');
    }
    Ok((result, counter))
}

fn remove_variables(source: &str) -> String {
    source
        .lines()
        .filter(|line| !is_macro_definition(line))
        .map(|x| {
            let mut x = x.to_string();
            x.push('\n');
            x
        })
        .collect::<String>()
}

/// Processes `include`s and macros
pub fn preprocess(source: &str) -> Result<String> {
    let mut source = source.to_string();
    let mut includes = 1;
    let mut table = generate_macro_table(&source)?;

    while includes > 0 {
        (source, includes) = process_include_lines(&source, &HashMap::new())?;
        table = generate_macro_table(&source)?;
    }

    source = remove_variables(&source);

    loop {
        let (result, substitutions) = substitute(&source, &table)?;
        if substitutions == 0 {
            break Ok(result);
        } else {
            source = result
        }
    }
}
