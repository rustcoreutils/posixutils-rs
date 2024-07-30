use std::{fs, path::PathBuf};

use crate::patch_utils::constants::context::{context_regex_cache, ContextRegexKind};

use super::constants::{
    normal::{get_normal_regex_list, NormalRegexKind},
    unified::{unified_regex_cache, UnifiedRegexKind},
    NO_NEW_LINE, NO_NEW_LINE_ED,
};

pub fn if_else<T>(condition: bool, if_true: T, if_false: T) -> T {
    if condition {
        if_true
    } else {
        if_false
    }
}

pub fn is_edit_script_range(line: &str) -> bool {
    "acd".chars().any(|ch| line.ends_with(ch))
        && line[..line.len().wrapping_sub(1)]
            .split(',')
            .all(|numeric_string| {
                numeric_string
                    .chars()
                    .all(|numeric_char| numeric_char.is_numeric())
            })
}

pub fn is_no_new_line(line: &str) -> bool {
    line.trim_end() == NO_NEW_LINE || line.trim_end() == NO_NEW_LINE_ED
}

pub fn is_normal_head(line: &str) -> bool {
    get_normal_regex_list(&[
        NormalRegexKind::RangeInsert,
        NormalRegexKind::RangeChange,
        NormalRegexKind::RangeDelete,
    ])
    .iter()
    .any(|regex| regex.is_match(line))
}

pub fn verify_patch_line(left: &str, right: &str) -> Result<(), ()> {
    if left.trim() != right.trim() {
        Err(())
    } else {
        Ok(())
    }
}

pub fn print_error(error: impl Into<String>) {
    eprintln!("patch: {}", error.into())
}

pub fn file_exists(path: impl Into<PathBuf>) -> bool {
    fs::metadata(path.into()).is_ok()
}

pub fn is_context_header(l0: &str, l1: &str) -> bool {
    let regex_cache = context_regex_cache();

    if regex_cache[&ContextRegexKind::FirstLine].is_match(l0)
        && regex_cache[&ContextRegexKind::SecondLine].is_match(l1)
    {
        true
    } else {
        false
    }
}

pub fn is_unfied_header(l0: &str, l1: &str) -> bool {
    let regex_cache = unified_regex_cache();

    if regex_cache[&UnifiedRegexKind::FirstLine].is_match(l0)
        && regex_cache[&UnifiedRegexKind::SecondLine].is_match(l1)
    {
        true
    } else {
        false
    }
}
