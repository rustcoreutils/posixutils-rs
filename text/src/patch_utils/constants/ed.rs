use regex::Regex;
use std::{collections::HashMap, sync::Once};

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

pub const ED_FORMAT_DOT_REGEX: &str = r"^\s*\.\s*$";
pub const ED_FORMAT_RANGE_ADD_REGEX: &str = r"^\s*\d+a\s*$";
pub const ED_FORMAT_RANGE_CHANGE_REGEX: &str = r"^\s*\d+(,\d+)?c\s*$";
pub const ED_FORMAT_RANGE_DELETE_REGEX: &str = r"^\s*\d+(,\d+)?d\s*$";
pub const ED_FORMAT_LINE_REGEX: &str = r"^\s*.*\s*";

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

pub fn get_ed_regex_list(kind_list: &[EdRegexKind]) -> Vec<&Regex> {
    let original_regex_list = ed_regex_cache();
    let mut regex_list = Vec::<&Regex>::new();

    for kind in kind_list {
        regex_list.push(&original_regex_list[kind]);
    }

    regex_list
}
