use super::{
    constants::*, context_hunk_range_data::ContextHunkRangeData, patch_line_data::PatchLineData,
    range::Range, unified_hunk_header_data::UnifiedHunkHeaderData,
};

#[derive(Debug, Clone)]
pub enum PatchLine<'a> {
    UnifiedHunkHeader(UnifiedHunkHeaderData),
    UnifiedUnchanged(PatchLineData<'a>),
    UnifiedInserted(PatchLineData<'a>),
    UnifiedDeleted(PatchLineData<'a>),
    NoNewLine(PatchLineData<'a>),
    ContextHunkSeparator(PatchLineData<'a>),
    ContextInserted(PatchLineData<'a>),
    ContextDeleted(PatchLineData<'a>),
    ContextUnchanged(PatchLineData<'a>),
    ContextSubstituted(PatchLineData<'a>),
    ContextHunkRange(ContextHunkRangeData),
}

impl<'a> PatchLine<'a> {
    pub fn original_line(&self) -> &str {
        match self {
            PatchLine::UnifiedHunkHeader(data) => data.line(),
            PatchLine::UnifiedUnchanged(data) => &data.line()[1..],
            PatchLine::UnifiedInserted(data) => &data.line()[1..],
            PatchLine::UnifiedDeleted(data) => &data.line()[1..],
            PatchLine::NoNewLine(data) => data.line(),
            PatchLine::ContextHunkSeparator(data) => data.line(),
            PatchLine::ContextInserted(data) => &data.line()[2..],
            PatchLine::ContextDeleted(data) => &data.line()[2..],
            PatchLine::ContextUnchanged(data) => &data.line()[2..],
            PatchLine::ContextSubstituted(data) => &data.line()[2..],
            PatchLine::ContextHunkRange(data) => data.line(),
        }
    }

    pub fn line(&self) -> &str {
        match self {
            PatchLine::UnifiedHunkHeader(data) => data.line(),
            PatchLine::UnifiedUnchanged(data) => data.line(),
            PatchLine::UnifiedInserted(data) => data.line(),
            PatchLine::UnifiedDeleted(data) => data.line(),
            PatchLine::NoNewLine(data) => data.line(),
            PatchLine::ContextHunkSeparator(data) => data.line(),
            PatchLine::ContextInserted(data) => data.line(),
            PatchLine::ContextDeleted(data) => data.line(),
            PatchLine::ContextUnchanged(data) => data.line(),
            PatchLine::ContextSubstituted(data) => data.line(),
            PatchLine::ContextHunkRange(data) => data.line(),
        }
    }

    pub fn unified_hunk_header_data(&self) -> Option<&UnifiedHunkHeaderData> {
        match self {
            PatchLine::UnifiedHunkHeader(hunk_header_data) => Some(hunk_header_data),
            _ => None,
        }
    }

    pub fn context_hunk_range_data(&self) -> Option<&ContextHunkRangeData> {
        match self {
            PatchLine::ContextHunkRange(data) => Some(data),
            _ => None,
        }
    }

    pub fn try_from_unified(line: &'a str, line_in_patch: usize) -> Result<Self, PatchError> {
        if !line.is_empty() {
            if Self::is_no_new_line(line) {
                return Ok(PatchLine::NoNewLine(PatchLineData::new(
                    line,
                    line_in_patch,
                )));
            }

            let unified_identifier = line.chars().nth(0).unwrap();

            let result = match unified_identifier {
                '+' => Ok(Self::UnifiedInserted(PatchLineData::new(
                    line,
                    line_in_patch,
                ))),
                '-' => Ok(Self::UnifiedDeleted(PatchLineData::new(
                    line,
                    line_in_patch,
                ))),
                '@' => {
                    let invalid_hunk_error = Err(PatchError::InvalidUnifiedHunkHeader);
                    let splited_hunk_header = line.trim().split(' ').collect::<Vec<&str>>();

                    if splited_hunk_header.len() != 4 {
                        return invalid_hunk_error;
                    }

                    let hunk_header = if splited_hunk_header[0] == UNIFIED_HUNK_HEADER_IDENTIFIER
                        && splited_hunk_header[3] == UNIFIED_HUNK_HEADER_IDENTIFIER
                    {
                        let f1_range = Range::try_from_unified(splited_hunk_header[1]);
                        let f2_range = Range::try_from_unified(splited_hunk_header[2]);

                        let hunk_header = match (f1_range, f2_range) {
                            (Ok(f1_range), Ok(f2_range)) => {
                                Ok(Self::UnifiedHunkHeader(UnifiedHunkHeaderData::new(
                                    line.to_string(),
                                    line_in_patch,
                                    f1_range,
                                    f2_range,
                                )))
                            }
                            _ => invalid_hunk_error,
                        };

                        hunk_header
                    } else {
                        invalid_hunk_error
                    };

                    hunk_header
                }
                ' ' => Ok(Self::UnifiedUnchanged(PatchLineData::new(
                    line,
                    line_in_patch,
                ))),
                _ => Err(PatchError::InvalidUnifiedPatchLine),
            };

            return result;
        }

        Err(PatchError::EmptyLineNotAllowed)
    }

    pub fn try_from_context(line: &'a str, line_in_patch: usize) -> Result<Self, PatchError> {
        if line.len() >= 2 {
            if line == CONTEXT_HUNK_SEPARATOR {
                return Ok(PatchLine::ContextHunkSeparator(PatchLineData::new(
                    line,
                    line_in_patch,
                )));
            }

            if Self::is_no_new_line(line) {
                return Ok(PatchLine::NoNewLine(PatchLineData::new(
                    line,
                    line_in_patch,
                )));
            }

            let is_context_original_range = line.starts_with(CONTEXT_ORIGINAL_RANGE_STARTER)
                && line.ends_with(CONTEXT_ORIGINAL_RANGE_FINISHER);

            let is_context_modified_range = line.starts_with(CONTEXT_MODIFIED_RANGE_STARTER)
                && line.ends_with(CONTEXT_MODIFIED_RANGE_FINISHER);

            let is_original = if is_context_original_range {
                Some(true)
            } else if is_context_modified_range {
                Some(false)
            } else {
                if line.starts_with(CONTEXT_ORIGINAL_RANGE_STARTER)
                    || line.starts_with(CONTEXT_MODIFIED_RANGE_STARTER)
                {
                    return Err(PatchError::InvalidContextRangeHeader);
                }

                None
            };

            if let Some(is_original) = is_original {
                let range_result = Range::try_from_context(line);

                if let Ok(range) = range_result {
                    return Ok(PatchLine::ContextHunkRange(ContextHunkRangeData::new(
                        line.to_string(),
                        line_in_patch,
                        range,
                        is_original,
                    )));
                } else if let Err(_error) = range_result {
                    return Err(PatchError::InvalidContextRangeHeader);
                };
            }

            let context_identifier = &line[0..2];

            let result = match context_identifier {
                "+ " => Ok(Self::ContextInserted(PatchLineData::new(
                    line,
                    line_in_patch,
                ))),
                "- " => Ok(Self::ContextDeleted(PatchLineData::new(
                    line,
                    line_in_patch,
                ))),
                "  " => Ok(Self::ContextUnchanged(PatchLineData::new(
                    line,
                    line_in_patch,
                ))),
                "! " => Ok(Self::ContextSubstituted(PatchLineData::new(
                    line,
                    line_in_patch,
                ))),
                _ => Err(PatchError::InvalidContextPatchLine),
            };

            return result;
        }

        Err(PatchError::InvalidContextPatchLine)
    }

    fn is_no_new_line(line: &str) -> bool {
        line.trim_end() == NO_NEW_LINE
    }

    pub fn is_unified_hunk_header(&self) -> bool {
        matches!(self, PatchLine::UnifiedHunkHeader(_))
    }

    pub fn is_context_hunk_separator(&self) -> bool {
        matches!(self, PatchLine::ContextHunkSeparator(_))
    }

    pub fn is_context_range(&self) -> bool {
        matches!(self, PatchLine::ContextHunkRange(_))
    }
}

#[derive(Debug)]
pub enum PatchError {
    EmptyLineNotAllowed,
    InvalidUnifiedHunkHeader,
    InvalidUnifiedPatchLine,
    InvalidPatchHeader,
    InvalidPatchFile,
    InvalidPatchLineWithData(String),
    CouldNotGuessPatchFormat,
    InvalidContextPatchLine,
    InvalidContextRangeHeader,
    PatchFormatUnavailable,
    InvalidContextHunkRange,
    ContextOrderedLinesUnavailable,
}
