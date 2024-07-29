use regex::Regex;
use std::{collections::HashMap, sync::Once};

use crate::patch_utils::patch_unit::PatchUnitKind;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum EdRegexKind {
    LineDot,
    RangeAdd,
    RangeChange,
    RangeDelete,
    Line,
}

const INITIALIZE_ED_REGEX_CACHE_ONCE: Once = Once::new();
static mut ED_REGEX_CACHE: Option<HashMap<EdRegexKind, Regex>> = None;

const ORDERED_KINDS: &[EdRegexKind] = &[
    EdRegexKind::RangeAdd,
    EdRegexKind::RangeChange,
    EdRegexKind::RangeDelete,
    EdRegexKind::Line,
    EdRegexKind::LineDot,
];

pub const ED_FORMAT_DOT_REGEX: &str = r"^\s*\.\s*$";
pub const ED_FORMAT_RANGE_ADD_REGEX: &str = r"^\s*\d+a\s*$";
pub const ED_FORMAT_RANGE_CHANGE_REGEX: &str = r"^\s*\d+(,\d+)?c\s*$";
pub const ED_FORMAT_RANGE_DELETE_REGEX: &str = r"^\s*\d+(,\d+)?d\s*$";
pub const ED_FORMAT_LINE_REGEX: &str = r"^\s*.*\s*";

pub fn ed_match(line: &str) -> PatchUnitKind {
    let cache = ed_regex_cache();

    for kind in ORDERED_KINDS {
        if cache[&kind].is_match(line) {
            return PatchUnitKind::Ed(*kind);
        }
    }

    // TODO: NewLine,
    // TODO: NoNewLine

    return PatchUnitKind::Unkonw;
}

pub fn initialize_ed_regex_cache() {
    INITIALIZE_ED_REGEX_CACHE_ONCE.call_once(|| unsafe {
        let mut ed_regex_cache = HashMap::new();

        ed_regex_cache.insert(
            EdRegexKind::LineDot,
            Regex::new(ED_FORMAT_DOT_REGEX).expect("ED_FORMAT_DOT_REGEX regex is not correct!"),
        );

        ed_regex_cache.insert(
            EdRegexKind::RangeAdd,
            Regex::new(ED_FORMAT_RANGE_ADD_REGEX)
                .expect("ED_FORMAT_RANGE_ADD_REGEX regex is not correct!"),
        );

        ed_regex_cache.insert(
            EdRegexKind::RangeChange,
            Regex::new(ED_FORMAT_RANGE_CHANGE_REGEX)
                .expect("ED_FORMAT_RANGE_CHANGE_REGEX regex is not correct!"),
        );

        ed_regex_cache.insert(
            EdRegexKind::RangeDelete,
            Regex::new(ED_FORMAT_RANGE_DELETE_REGEX)
                .expect("ED_FORMAT_RANGE_DELETE_REGEX regex is not correct!"),
        );

        ed_regex_cache.insert(
            EdRegexKind::Line,
            Regex::new(ED_FORMAT_LINE_REGEX).expect("ED_FORMAT_LINE_REGEX regex is not correct!"),
        );

        ED_REGEX_CACHE = Some(ed_regex_cache);
    });
}

pub fn ed_regex_cache() -> &'static HashMap<EdRegexKind, Regex> {
    #[allow(static_mut_refs)]
    if let Some(original_ed_regex_cache) = unsafe { &ED_REGEX_CACHE } {
        return original_ed_regex_cache;
    }

    panic!("ED_REGEX_CACHE should not be empty!");
}
