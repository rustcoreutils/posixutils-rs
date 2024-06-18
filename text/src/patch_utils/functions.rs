use regex::Regex;

use super::constants::{
    regex::{
        NORMAL_FORMAT_RANGE_CHANGE_REGEX, NORMAL_FORMAT_RANGE_DELETE_REGEX,
        NORMAL_FORMAT_RANGE_INSERT_REGEX,
    },
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

pub fn is_normal_range(line: &str) -> bool {
    let patterns = [
        NORMAL_FORMAT_RANGE_INSERT_REGEX,
        NORMAL_FORMAT_RANGE_CHANGE_REGEX,
        NORMAL_FORMAT_RANGE_DELETE_REGEX,
    ];

    patterns.iter().any(|pattern| {
        Regex::new(pattern)
            .expect("NORMAL-FORMAT patterns are supposed to be correct!")
            .is_match(line)
    })
}
