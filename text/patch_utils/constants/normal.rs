use regex::Regex;
use std::{collections::HashMap, hash::Hash, sync::Once};

pub const NORMAL_FORMAT_RANGE_INSERT_REGEX: &str = r"^\s*\d+a(\d+(,\d+)?|\d+)\s*$";
pub const NORMAL_FORMAT_RANGE_CHANGE_REGEX: &str = r"^\s*(\d+(,\d+)?|\d+)c(\d+(,\d+)?|\d+)\s*$";
pub const NORMAL_FORMAT_RANGE_DELETE_REGEX: &str = r"^\s*(\d+(,\d+)?|\d+)d\d+\s*$";
pub const NORMAL_FORMAT_INSERTED_LINE_REGEX: &str = r"^\s*>\s.*\s*$";
pub const NORMAL_FORMAT_DELETED_LINE_REGEX: &str = r"^\s*<\s.*\s*$";
pub const NORMAL_FORMAT_CHANGE_SEPARATOR_REGEX: &str = r"^\s*---\s*$";

const INITIALIZE_NORMAL_REGEX_CACHE_ONCE: Once = Once::new();
static mut NORMAL_REGEX_CACHE: Option<HashMap<NormalRegexKind, Regex>> = None;

pub fn initialize_normal_regex_cache() {
    INITIALIZE_NORMAL_REGEX_CACHE_ONCE.call_once(|| unsafe {
        let mut regex_cache = HashMap::new();
        regex_cache.insert(
            NormalRegexKind::RangeInsert,
            Regex::new(NORMAL_FORMAT_RANGE_INSERT_REGEX)
                .expect("NORMAL_FORMAT_RANGE_INSERT_REGEX regex is not correct!"),
        );
        regex_cache.insert(
            NormalRegexKind::RangeChange,
            Regex::new(NORMAL_FORMAT_RANGE_CHANGE_REGEX)
                .expect("NORMAL_FORMAT_RANGE_CHANGE_REGEX regex is not correct!"),
        );
        regex_cache.insert(
            NormalRegexKind::RangeDelete,
            Regex::new(NORMAL_FORMAT_RANGE_DELETE_REGEX)
                .expect("NORMAL_FORMAT_RANGE_DELETE_REGEX regex is not correct!"),
        );
        regex_cache.insert(
            NormalRegexKind::LineInsert,
            Regex::new(NORMAL_FORMAT_INSERTED_LINE_REGEX)
                .expect("NORMAL_FORMAT_INSERTED_LINE regex is not correct!"),
        );
        regex_cache.insert(
            NormalRegexKind::LineDelete,
            Regex::new(NORMAL_FORMAT_DELETED_LINE_REGEX)
                .expect("NORMAL_FORMAT_DELETED_LINE regex is not correct!"),
        );
        regex_cache.insert(
            NormalRegexKind::LineChangeSeparator,
            Regex::new(NORMAL_FORMAT_CHANGE_SEPARATOR_REGEX)
                .expect("NORMAL_FORMAT_CHANGE_SEPARATOR regex is not correct!"),
        );

        NORMAL_REGEX_CACHE = Some(regex_cache);
    });
}

pub fn normal_regex_cache() -> &'static HashMap<NormalRegexKind, Regex> {
    #[allow(static_mut_refs)]
    if let Some(original_normal_regex_cache) = unsafe { &NORMAL_REGEX_CACHE } {
        return original_normal_regex_cache;
    }

    panic!("NORMAL_REGEX_CACHE should not be empty!");
}

pub fn get_normal_regex_list(kind_list: &[NormalRegexKind]) -> Vec<&Regex> {
    let original_regex_list = normal_regex_cache();
    let mut regex_list = Vec::<&Regex>::new();

    for kind in kind_list {
        regex_list.push(&original_regex_list[kind]);
    }

    regex_list
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum NormalRegexKind {
    RangeInsert,
    RangeChange,
    RangeDelete,
    LineInsert,
    LineDelete,
    LineChangeSeparator,
}
