use super::{
    patch_error::{PatchError, PatchResult},
    patch_format::PatchFormat,
    range::Range,
};

#[derive(Debug, Clone, Copy)]
pub enum NormalRangeKind {
    Insert,
    Change,
    Delete,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct NormalRangeData<'a> {
    line: &'a str,
    range_left: Range,
    range_right: Range,
    kind: NormalRangeKind,
}

impl<'a> NormalRangeData<'a> {
    pub fn new(
        line: &'a str,
        range_left: Range,
        range_right: Range,
        kind: NormalRangeKind,
    ) -> Self {
        Self {
            line,
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

    pub fn try_from(line: &'a str) -> PatchResult<Self> {
        fn parse_side_range(comma_separated_numeric_string: &str) -> PatchResult<Range> {
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
                1 => Ok(Range::new(range[0], range[0], PatchFormat::Normal)),
                2 => Ok(Range::new(range[0], range[1], PatchFormat::Normal)),
                _ => Err(PatchError::Error(
                    "Invalid Context range line splitted list length!",
                )),
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
            Some('d') => NormalRangeKind::Delete,
            _ => {
                return Err(PatchError::Error(
                    "NormalRange identifier should be one of `a` or `d` or `c`!",
                ))
            }
        };

        let range_left = parse_side_range(&range_left)?;
        let range_right = parse_side_range(&range_right)?;

        Ok(Self::new(line, range_left, range_right, range_kind))
    }
}
