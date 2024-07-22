use super::{
    edit_script_range_data::EditScriptHunkKind,
    patch_error::{PatchError, PatchResult},
    patch_format::PatchFormat,
};

type RangeResult = PatchResult<Range>;

#[derive(Clone, Copy, Debug)]
pub struct Range {
    start: usize,
    end: usize,
    kind: PatchFormat,
}

impl Range {
    pub fn new(start: usize, end: usize, kind: PatchFormat) -> Self {
        if !matches!(kind, PatchFormat::Unified) {
            assert!(
                start <= end,
                "Range creation failed! start should be less than or equal to end."
            );
        }

        Self { start, end, kind }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    /// panics when self.kind is [PatchFormat::None]
    pub fn end(&self) -> usize {
        match self.kind {
            PatchFormat::None => panic!("Range should belong to one of the four formats!"),
            PatchFormat::Normal => self.end,
            PatchFormat::Unified => self.start + self.end,
            PatchFormat::Context => self.end,
            PatchFormat::EditScript => self.end,
        }
    }

    pub fn try_from_unified(unified_raw_range: &str) -> RangeResult {
        let str_numbers = unified_raw_range[1..].split(',').collect::<Vec<&str>>();
        let mut numbers = Vec::<usize>::new();
        let radix = 10;

        for str_number in str_numbers {
            let number = usize::from_str_radix(str_number, radix)?;
            numbers.push(number);
        }

        match numbers.len() {
            1 => Ok(Range::new(numbers[0], 0, PatchFormat::Unified)),
            2 => Ok(Range::new(numbers[0], numbers[1], PatchFormat::Unified)),
            _ => Err(PatchError::Error(
                "Invalid Unified range numbers list length!",
            )),
        }
    }

    pub fn try_from_context(line: &str) -> RangeResult {
        let splitted = line.split(' ').collect::<Vec<&str>>();
        let range_numbers = splitted[1].split(',').collect::<Vec<&str>>();
        let radix = 10;

        let mut numbers = Vec::<usize>::new();

        for str_number in range_numbers {
            let number = usize::from_str_radix(str_number, radix)?;
            numbers.push(number);
        }

        match numbers.len() {
            1 => Ok(Range::new(numbers[0], numbers[0], PatchFormat::Context)),
            2 => Ok(Range::new(numbers[0], numbers[1], PatchFormat::Context)),
            _ => Err(PatchError::Error(
                "Invalid Context range numbers list length!",
            )),
        }
    }

    pub(crate) fn edit_script_range_kind(line: &str) -> EditScriptHunkKind {
        let last_char = line.trim().chars().last();

        if let Some(last_char) = last_char {
            return match last_char {
                'a' => EditScriptHunkKind::Insert,
                'c' => EditScriptHunkKind::Change,
                'd' => EditScriptHunkKind::Delete,
                _ => panic!("Invalid ed hunk range!"),
            };
        }

        panic!("Invalid ed hunk range!");
    }

    pub(crate) fn try_from_edit_script(line: &str) -> RangeResult {
        let comma_separated_numbers = &line.trim()[0..line.len().wrapping_sub(1)];
        let numeric_strings = comma_separated_numbers.split(',').collect::<Vec<&str>>();

        match numeric_strings.len() {
            1 => {
                let number = numeric_strings[0].parse::<usize>()?;
                return Ok(Range::new(number, number, PatchFormat::EditScript));
            }
            2 => {
                let number1 = numeric_strings[0].parse::<usize>()?;
                let number2 = numeric_strings[1].parse::<usize>()?;
                return Ok(Range::new(number1, number2, PatchFormat::EditScript));
            }
            _ => Err(PatchError::Error(
                "Invalid ED range numeric string list length!",
            )),
        }
    }
}
