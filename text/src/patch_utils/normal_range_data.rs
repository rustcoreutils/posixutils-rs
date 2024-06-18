use std::sync::mpsc::RecvError;

use super::{
    functions::{is_no_new_line, is_normal_range},
    patch_format::PatchFormat,
    range::{Range, RangeError},
};

#[derive(Debug, Clone, Copy)]
pub enum NormalRangeKind {
    Insert,
    Change,
    Remove,
}

#[derive(Debug, Clone)]
pub struct NormalRangeData<'a> {
    line: &'a str,
    line_in_patch: usize,
    range_left: Range,
    range_right: Range,
    kind: NormalRangeKind,
}

impl<'a> NormalRangeData<'a> {
    pub fn new(
        line: &'a str,
        line_in_patch: usize,
        range_left: Range,
        range_right: Range,
        kind: NormalRangeKind,
    ) -> Self {
        Self {
            line,
            line_in_patch,
            range_left,
            range_right,
            kind,
        }
    }

    pub fn kind(&self) -> NormalRangeKind {
        self.kind
    }

    pub fn line(&self) -> &str {
        self.line
    }

    pub fn range_left(&self) -> Range {
        self.range_left
    }

    pub fn range_right(&self) -> Range {
        self.range_right
    }

    pub fn try_from(line: &'a str, line_in_patch: usize) -> Result<Self, RangeError> {
        if !is_normal_range(line) {
            Err(RangeError::InvalidRange)
        } else {
            fn parse_side_range(comma_separated_numeric_string: &str) -> Range {
                let range_str_numbers = comma_separated_numeric_string
                    .split(',')
                    .collect::<Vec<&str>>();

                let range = range_str_numbers
                    .iter()
                    .map(|numeric| {
                        numeric
                            .parse::<usize>()
                            .expect("splitted comma_separated_numeric_strings are supposed to be valid numbers!")
                    })
                    .collect::<Vec<usize>>();

                match range.len() {
                    1 => Range::new(range[0], range[0], PatchFormat::Normal),
                    2 => Range::new(range[0], range[1], PatchFormat::Normal),
                    _ => panic!("length of range_left can only be 1 or 2."),
                }
            }

            let range_left = line
                .chars()
                .take_while(|ch| ch.is_numeric() || *ch == ',')
                .collect::<String>();

            let ident_index = range_left.len();

            let range_right = line
                .chars()
                .skip(ident_index + 1)
                .take_while(|ch| ch.is_numeric() || *ch == ',')
                .collect::<String>();

            let range_kind = match line.chars().nth(ident_index) {
                Some('a') => NormalRangeKind::Insert,
                Some('c') => NormalRangeKind::Change,
                Some('d') => NormalRangeKind::Remove,
                _ => {
                    return Err(RangeError::InvalidRangeWithError(
                        "NormalRange identifier should be one of `a` or `d` or `c`!".to_string(),
                    ))
                }
            };

            let range_left = parse_side_range(&range_left);
            let range_right = parse_side_range(&range_right);

            Ok(Self::new(
                line,
                line_in_patch,
                range_left,
                range_right,
                range_kind,
            ))
        }
    }
}
