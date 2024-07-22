use std::{collections::HashMap, sync::Once};

use regex::Regex;

use crate::patch_utils::{functions::if_else, patch_unit::PatchUnitKind};

pub const UNIFIED_FORMAT_RANGE_HEADER_REGEX: &str = r"^@@\s+-\d+(?:,\d+)?\s+\+\d+(?:,\d+)?\s+@@$";
pub const UNIFIED_FORMAT_FIRST_LINE_REGEX: &str = r"^\s*---\s*.+\s*(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{9} [+-]\d{4}|\w{3} \w{3} \d{1,2} \d{2}:\d{2}:\d{2} \d{4})?\s*$";
pub const UNIFIED_FORMAT_SECOND_LINE_REGEX: &str = r"^\s*\+{3}\s*.+\s*(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{9} [+-]\d{4}|\w{3} \w{3} \d{1,2} \d{2}:\d{2}:\d{2} \d{4})?\s*$";
pub const UNIFIED_FORMAT_REMOVED_LINE_REGEX: &str = r"^\s*-.*\s*$";
pub const UNIFIED_FORMAT_INSERTED_LINE_REGEX: &str = r"^\s*\+.*\s*$";
pub const UNIFIED_FORMAT_UNCHANGED_LINE_REGEX: &str = r"^\s+.*\s*$";

const INITIALIZE_UNIFIED_REGEX_CACHE_ONCE: Once = Once::new();
static mut UNIFIED_REGEX_CACHE: Option<HashMap<UnifiedRegexKind, Regex>> = None;

const ORDERED_KINDS: &[UnifiedRegexKind] = &[
    UnifiedRegexKind::FirstLine,
    UnifiedRegexKind::SecondLine,
    UnifiedRegexKind::RangeHeader,
    UnifiedRegexKind::InsertedLine,
    UnifiedRegexKind::UnchangedLine,
    UnifiedRegexKind::DeletedLine,
];

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnifiedRegexKind {
    FirstLine,
    SecondLine,
    RangeHeader,
    InsertedLine,
    DeletedLine,
    UnchangedLine,
}

pub fn initialize_unified_regex_cache() {
    INITIALIZE_UNIFIED_REGEX_CACHE_ONCE.call_once(|| unsafe {
        let mut regex_cache = HashMap::new();

        regex_cache.insert(
            UnifiedRegexKind::RangeHeader,
            Regex::new(UNIFIED_FORMAT_RANGE_HEADER_REGEX)
                .expect("UNIFIED_FORMAT_RANGE_HEADER_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            UnifiedRegexKind::FirstLine,
            Regex::new(UNIFIED_FORMAT_FIRST_LINE_REGEX)
                .expect("UNIFIED_FORMAT_FIRST_LINE_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            UnifiedRegexKind::SecondLine,
            Regex::new(UNIFIED_FORMAT_SECOND_LINE_REGEX)
                .expect("UNIFIED_FORMAT_SECOND_LINE_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            UnifiedRegexKind::DeletedLine,
            Regex::new(UNIFIED_FORMAT_REMOVED_LINE_REGEX)
                .expect("UNIFIED_FORMAT_REMOVED_LINE_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            UnifiedRegexKind::InsertedLine,
            Regex::new(UNIFIED_FORMAT_INSERTED_LINE_REGEX)
                .expect("UNIFIED_FORMAT_INSERTED_LINE_REGEX regex is not correct!"),
        );

        regex_cache.insert(
            UnifiedRegexKind::UnchangedLine,
            Regex::new(UNIFIED_FORMAT_UNCHANGED_LINE_REGEX)
                .expect("UNIFIED_FORMAT_UNCHANGED_LINE_REGEX regex is not correct!"),
        );

        UNIFIED_REGEX_CACHE = Some(regex_cache);
    });
}

pub fn unified_regex_cache() -> &'static HashMap<UnifiedRegexKind, Regex> {
    #[allow(static_mut_refs)]
    if let Some(regex_cache) = unsafe { &UNIFIED_REGEX_CACHE } {
        return regex_cache;
    }

    panic!("UNIFIED_REGEX_CACHE should not be empty!");
}

pub fn get_unified_regex_list(kind_list: &[UnifiedRegexKind]) -> Vec<&Regex> {
    let original_regex_list = unified_regex_cache();
    let mut regex_list = Vec::<&Regex>::new();

    for kind in kind_list {
        regex_list.push(&original_regex_list[kind]);
    }

    regex_list
}

pub fn unified_match(line: &str) -> PatchUnitKind {
    let cache = unified_regex_cache();

    for kind in ORDERED_KINDS {
        if cache[&kind].is_match(line) {
            return PatchUnitKind::Unified(*kind);
        }
    }

    // TODO: NewLine,
    // TODO: NoNewLine

    return PatchUnitKind::Unkonw;
}
