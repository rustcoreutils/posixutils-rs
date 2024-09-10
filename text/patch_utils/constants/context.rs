use regex::Regex;
use std::{collections::HashMap, sync::Once};

use crate::patch_utils::patch_unit::PatchUnitKind;

pub const ORIGINAL_SKIP: usize = 2; // 0 is Hunk separator; 1 is original range

// CONTEXT REGEX STRINGS
pub const CONTEXT_FORMAT_HUNK_SEPARATOR_REGEX: &str = r"^\s*\*{15}\s*$";
pub const CONTEXT_FORMAT_ORIGINAL_RANGE_REGEX: &str = r"^\s*^\*\*\*\s+(\d+|\d+,\d+)\s+\*{4}\s*$";
pub const CONTEXT_FORMAT_MODIFIED_RANGE_REGEX: &str = r"^\s*^-{3}\s+(\d+|\d+,\d+)\s+-{4}\s*$";
pub const CONTEXT_FORMAT_FIRST_LINE_REGEX: &str = r"^\s*\*{3}\s*.+\s+(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{9} [+-]\d{4}|\w{3} \w{3} \d{1,2} \d{2}:\d{2}:\d{2} \d{4})?\s*$";
pub const CONTEXT_FORMAT_SECOND_LINE_REGEX: &str = r"^\s*-{3}\s*.+\s+(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{9} [+-]\d{4}|\w{3} \w{3} \d{1,2} \d{2}:\d{2}:\d{2} \d{4})?\s*$";
pub const CONTEXT_FORMAT_INSERTED_LINE_REGEX: &str = r"^\s*\+\s.*\s*$";
pub const CONTEXT_FORMAT_DELETED_LINE_REGEX: &str = r"^\s*-\s.*\s*$";
pub const CONTEXT_FORMAT_CHANGED_LINE_REGEX: &str = r"^\s*\!\s.*\s*$";
pub const CONTEXT_FORMAT_UNCHANGED_LINE_REGEX: &str = r"^\s*\s\s.*\s*$";

const INITIALIZE_CONTEXT_REGEX_CACHE_ONCE: Once = Once::new();
static mut CONTEXT_REGEX_CACHE: Option<HashMap<ContextRegexKind, Regex>> = None;

const ORDERED_KINDS: &[ContextRegexKind] = &[
    ContextRegexKind::FirstLine,
    ContextRegexKind::SecondLine,
    ContextRegexKind::HunkSeparator,
    ContextRegexKind::OriginalRange,
    ContextRegexKind::ModifiedRange,
    ContextRegexKind::InsertedLine,
    ContextRegexKind::DeletedLine,
    ContextRegexKind::ChangedLine,
    ContextRegexKind::UnchangedLine,
];

pub fn context_match(line: &str) -> PatchUnitKind {
    let cache = context_regex_cache();

    for kind in ORDERED_KINDS {
        if cache[kind].is_match(line) {
            return PatchUnitKind::Context(*kind);
        }
    }

    // TODO: NewLine,
    // TODO: NoNewLine

    PatchUnitKind::Unkonw
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ContextRegexKind {
    HunkSeparator,
    OriginalRange,
    ModifiedRange,
    FirstLine,
    SecondLine,
    InsertedLine,
    DeletedLine,
    ChangedLine,
    UnchangedLine,
}

pub fn initialize_context_regex_cache() {
    INITIALIZE_CONTEXT_REGEX_CACHE_ONCE.call_once(|| unsafe {
        let mut regex_cache = HashMap::new();

        regex_cache.insert(
            ContextRegexKind::HunkSeparator,
            Regex::new(CONTEXT_FORMAT_HUNK_SEPARATOR_REGEX)
                .expect("CONTEXT_FORMAT_HUNK_SEPARATOR_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            ContextRegexKind::OriginalRange,
            Regex::new(CONTEXT_FORMAT_ORIGINAL_RANGE_REGEX)
                .expect("CONTEXT_FORMAT_ORIGINAL_RANGE_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            ContextRegexKind::ModifiedRange,
            Regex::new(CONTEXT_FORMAT_MODIFIED_RANGE_REGEX)
                .expect("CONTEXT_FORMAT_MODIFIED_RANGE_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            ContextRegexKind::FirstLine,
            Regex::new(CONTEXT_FORMAT_FIRST_LINE_REGEX)
                .expect("CONTEXT_FORMAT_FIRST_LINE_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            ContextRegexKind::SecondLine,
            Regex::new(CONTEXT_FORMAT_SECOND_LINE_REGEX)
                .expect("CONTEXT_FORMAT_SECOND_LINE_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            ContextRegexKind::InsertedLine,
            Regex::new(CONTEXT_FORMAT_INSERTED_LINE_REGEX)
                .expect("CONTEXT_FORMAT_INSERTED_LINE_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            ContextRegexKind::DeletedLine,
            Regex::new(CONTEXT_FORMAT_DELETED_LINE_REGEX)
                .expect("CONTEXT_FORMAT_DELETED_LINE_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            ContextRegexKind::ChangedLine,
            Regex::new(CONTEXT_FORMAT_CHANGED_LINE_REGEX)
                .expect("CONTEXT_FORMAT_CHANGED_LINE_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            ContextRegexKind::UnchangedLine,
            Regex::new(CONTEXT_FORMAT_UNCHANGED_LINE_REGEX)
                .expect("CONTEXT_FORMAT_UNCHANGED_LINE_REGEX regex is not correct!"),
        );

        CONTEXT_REGEX_CACHE = Some(regex_cache);
    });
}

pub fn context_regex_cache() -> &'static HashMap<ContextRegexKind, Regex> {
    #[allow(static_mut_refs)]
    if let Some(original_normal_regex_cache) = unsafe { &CONTEXT_REGEX_CACHE } {
        return original_normal_regex_cache;
    }

    panic!("NORMAL_REGEX_CACHE should not be empty!");
}
