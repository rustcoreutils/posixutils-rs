use super::patch_format::PatchFormat;

#[derive(Clone, Copy, Debug)]
pub struct Range {
    start: usize,
    end: usize,
    kind: PatchFormat,
}

impl Range {
    fn new(start: usize, end: usize, kind: PatchFormat) -> Self {
        Self { start, end, kind }
    }

    pub fn try_from_unified(unified_range: &str) -> Result<Self, RangeError> {
        let range_error = Err(RangeError::InvalidRange);
        let comma_separated_numbers = unified_range
            .chars()
            .filter(|ch| *ch != '+' && *ch != '-')
            .collect::<String>();
        let str_numbers = comma_separated_numbers.split(',');
        let mut numbers = Vec::<usize>::new();
        let radix = 10;

        for str_number in str_numbers {
            if let Ok(line_number) = usize::from_str_radix(str_number, radix) {
                numbers.push(line_number)
            } else {
                return range_error;
            }
        }

        match numbers.len() {
            1 => Ok(Range::new(numbers[0], 0, PatchFormat::Unified)),
            2 => Ok(Range::new(numbers[0], numbers[1], PatchFormat::Unified)),
            _ => range_error,
        }
    }

    pub fn kind(&self) -> &PatchFormat {
        &self.kind
    }

    pub fn start(&self) -> usize {
        self.start
    }

    /// panics when self.kind is [PatchFormat::None]
    pub fn end(&self) -> usize {
        match self.kind {
            PatchFormat::None => panic!("Range should belong to one of the four formats!"),
            PatchFormat::Default => todo!(),
            PatchFormat::Unified => self.start + self.end,
            PatchFormat::Context => self.end,
            PatchFormat::EditScript => todo!(),
        }
    }

    pub fn try_from_context(line: &str) -> Result<Range, RangeError> {
        let range_error = Err(RangeError::InvalidRange);
        let splitted = line.split(' ').collect::<Vec<&str>>();

        match splitted.len() {
            3 => {
                let range_numbers = splitted[1].split(',').collect::<Vec<&str>>();
                let radix = 10;
                let mut numbers = Vec::<usize>::new();

                if [1usize, 2usize].contains(&range_numbers.len()) {
                    for str_number in range_numbers {
                        if let Ok(number) = usize::from_str_radix(str_number, radix) {
                            numbers.push(number)
                        } else {
                            return range_error;
                        }
                    }

                    match numbers.len() {
                        1 => Ok(Range::new(numbers[0], numbers[0], PatchFormat::Context)),
                        2 => Ok(Range::new(numbers[0], numbers[1], PatchFormat::Context)),
                        _ => range_error,
                    }
                } else {
                    range_error
                }
            }
            _ => range_error,
        }
    }
}

#[derive(Debug)]
pub enum RangeError {
    InvalidRange,
}
