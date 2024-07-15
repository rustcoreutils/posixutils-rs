use std::{fmt::Display, io};

use crate::patch_utils::{
    constants::normal::{normal_regex_cache, NormalRegexKind},
    functions::is_normal_head,
};

use super::{
    // constants::unified::UNIFIED_HUNK_HEADER_IDENTIFIER,
    context_hunk_range_data::ContextHunkRangeData,
    edit_script_range_data::{EditScriptHunkKind, EditScriptRangeData},
    functions::{is_edit_script_range, is_no_new_line},
    normal_range_data::NormalRangeData,
    patch_format::PatchFormat,
    patch_line_data::PatchLineData,
    range::Range,
    unified_hunk_header_data::UnifiedHunkHeaderData,
};

#[derive(Debug, Clone)]
pub enum PatchLine<'a> {
    UnifiedHunkHeader(UnifiedHunkHeaderData),
    UnifiedUnchanged(PatchLineData<'a>),
    UnifiedInserted(PatchLineData<'a>),
    UnifiedDeleted(PatchLineData<'a>),
    NoNewLine(PatchLineData<'a>),
    ContextHunkSeparator(PatchLineData<'a>),
    ContextInserted(
        PatchLineData<'a>,
        /// is Change
        bool,
    ),
    ContextDeleted(
        PatchLineData<'a>,
        /// is Change
        bool,
    ),
    ContextUnchanged(PatchLineData<'a>),
    ContextHunkRange(ContextHunkRangeData),
    EditScriptRange(EditScriptRangeData<'a>),
    EditScriptInsert(PatchLineData<'a>),
    EditScriptChange(PatchLineData<'a>),
    NormalRange(NormalRangeData<'a>),
    NormalChangeSeparator(PatchLineData<'a>),
    NormalNewLine(PatchLineData<'a>),
    NormalOldLine(PatchLineData<'a>),
}

impl<'a> PatchLine<'a> {
    pub fn kind(&self) -> PatchFormat {
        match self {
            PatchLine::UnifiedHunkHeader(_) => PatchFormat::Unified,
            PatchLine::UnifiedUnchanged(data) => data.kind(),
            PatchLine::UnifiedInserted(data) => data.kind(),
            PatchLine::UnifiedDeleted(data) => data.kind(),
            PatchLine::NoNewLine(data) => data.kind(),
            PatchLine::ContextHunkSeparator(data) => data.kind(),
            PatchLine::ContextInserted(data, _) => data.kind(),
            PatchLine::ContextDeleted(data, _) => data.kind(),
            PatchLine::ContextUnchanged(data) => data.kind(),
            PatchLine::ContextHunkRange(_) => PatchFormat::Context,
            PatchLine::EditScriptRange(_) => PatchFormat::EditScript,
            PatchLine::EditScriptInsert(data) => data.kind(),
            PatchLine::EditScriptChange(data) => data.kind(),
            PatchLine::NormalRange(_) => PatchFormat::Normal,
            PatchLine::NormalChangeSeparator(data) => data.kind(),
            PatchLine::NormalNewLine(data) => data.kind(),
            PatchLine::NormalOldLine(data) => data.kind(),
        }
    }

    pub fn original_line(&self) -> &str {
        match self {
            PatchLine::UnifiedHunkHeader(data) => data.line(),
            PatchLine::UnifiedUnchanged(data) => &data.line()[1..],
            PatchLine::UnifiedInserted(data) => &data.line()[1..],
            PatchLine::UnifiedDeleted(data) => &data.line()[1..],
            PatchLine::NoNewLine(data) => data.line(),
            PatchLine::ContextHunkSeparator(data) => data.line(),
            PatchLine::ContextInserted(data, _) => &data.line()[2..],
            PatchLine::ContextDeleted(data, _) => &data.line()[2..],
            PatchLine::ContextUnchanged(data) => &data.line()[2..],
            PatchLine::ContextHunkRange(data) => data.line(),
            PatchLine::EditScriptRange(data) => data.line(),
            PatchLine::EditScriptInsert(data) => data.line(),
            PatchLine::EditScriptChange(data) => data.line(),
            PatchLine::NormalRange(data) => data.line(),
            PatchLine::NormalChangeSeparator(data) => data.line(),
            PatchLine::NormalNewLine(data) => &data.line()[2..],
            PatchLine::NormalOldLine(data) => &data.line()[2..],
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
            PatchLine::ContextInserted(data, _) => data.line(),
            PatchLine::ContextDeleted(data, _) => data.line(),
            PatchLine::ContextUnchanged(data) => data.line(),
            PatchLine::ContextHunkRange(data) => data.line(),
            PatchLine::EditScriptRange(data) => data.line(),
            PatchLine::EditScriptInsert(data) => data.line(),
            PatchLine::EditScriptChange(data) => data.line(),
            PatchLine::NormalRange(data) => data.line(),
            PatchLine::NormalChangeSeparator(data) => data.line(),
            PatchLine::NormalNewLine(data) => data.line(),
            PatchLine::NormalOldLine(data) => data.line(),
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

    pub fn edit_script_range_data(&self) -> Option<&EditScriptRangeData> {
        match self {
            PatchLine::EditScriptRange(data) => Some(data),
            _ => None,
        }
    }

    pub(crate) fn normal_hunk_range_data(&self) -> Option<&NormalRangeData> {
        match self {
            PatchLine::NormalRange(data) => Some(data),
            _ => None,
        }
    }

    pub fn try_from_unified(line: &'a str, line_in_patch: usize) -> Result<Self, PatchError> {
        todo!();

        // if !line.is_empty() {
        //     if is_no_new_line(line) {
        //         return Ok(PatchLine::NoNewLine(PatchLineData::new(
        //             line,
        //             line_in_patch,
        //             PatchFormat::Unified,
        //         )));
        //     }

        //     let unified_identifier = line.chars().nth(0).unwrap();

        //     let result = match unified_identifier {
        //         '+' => Ok(Self::UnifiedInserted(PatchLineData::new(
        //             line,
        //             line_in_patch,
        //             PatchFormat::Unified,
        //         ))),
        //         '-' => Ok(Self::UnifiedDeleted(PatchLineData::new(
        //             line,
        //             line_in_patch,
        //             PatchFormat::Unified,
        //         ))),
        //         '@' => {
        //             let invalid_hunk_error = Err(PatchError::InvalidUnifiedHunkHeader);
        //             let splited_hunk_header = line.trim().split(' ').collect::<Vec<&str>>();

        //             if splited_hunk_header.len() != 4 {
        //                 return invalid_hunk_error;
        //             }

        //             let hunk_header = if splited_hunk_header[0] == UNIFIED_HUNK_HEADER_IDENTIFIER
        //                 && splited_hunk_header[3] == UNIFIED_HUNK_HEADER_IDENTIFIER
        //             {
        //                 let f1_range = Range::try_from_unified(splited_hunk_header[1]);
        //                 let f2_range = Range::try_from_unified(splited_hunk_header[2]);

        //                 let hunk_header = match (f1_range, f2_range) {
        //                     (Ok(f1_range), Ok(f2_range)) => {
        //                         Ok(Self::UnifiedHunkHeader(UnifiedHunkHeaderData::new(
        //                             line.to_string(),
        //                             line_in_patch,
        //                             f1_range,
        //                             f2_range,
        //                         )))
        //                     }
        //                     _ => invalid_hunk_error,
        //                 };

        //                 hunk_header
        //             } else {
        //                 invalid_hunk_error
        //             };

        //             hunk_header
        //         }
        //         ' ' => Ok(Self::UnifiedUnchanged(PatchLineData::new(
        //             line,
        //             line_in_patch,
        //             PatchFormat::Unified,
        //         ))),
        //         _ => Err(PatchError::InvalidUnifiedPatchLine),
        //     };

        //     return result;
        // }

        // Err(PatchError::EmptyLineNotAllowed)
    }

    pub fn try_from_normal(line: &'a str, line_in_patch: usize) -> Result<Self, PatchError> {
        let normal_regex = normal_regex_cache();

        if is_normal_head(line) {
            let possible_data = NormalRangeData::try_from(line, line_in_patch);

            if let Ok(data) = possible_data {
                return Ok(PatchLine::NormalRange(data));
            }

            return Err(PatchError::NormalPatch(
                NormalPatchError::InvalidPatchRange(line.to_owned()),
            ));
        }

        let patch_line_data = PatchLineData::new(line, line_in_patch, PatchFormat::Normal);

        if is_no_new_line(line) {
            return Ok(PatchLine::NoNewLine(patch_line_data));
        }

        if normal_regex[&NormalRegexKind::LineChangeSeparator].is_match(line) {
            return Ok(PatchLine::NormalChangeSeparator(patch_line_data));
        }

        if normal_regex[&NormalRegexKind::LineInsert].is_match(line) {
            return Ok(PatchLine::NormalNewLine(patch_line_data));
        }

        if normal_regex[&NormalRegexKind::LineDelete].is_match(line) {
            return Ok(PatchLine::NormalOldLine(patch_line_data));
        }

        dbg!(line);
        return Err(PatchError::NormalPatch(NormalPatchError::InvalidPatchLine(
            line.to_owned(),
        )));
    }

    pub fn try_from_context(
        line: &'a str,
        line_in_patch: usize,
        is_original_line: bool,
    ) -> Result<Self, PatchError> {
        todo!()
        // if line.len() >= 2 {
        //     if line == CONTEXT_HUNK_SEPARATOR {
        //         return Ok(PatchLine::ContextHunkSeparator(PatchLineData::new(
        //             line,
        //             line_in_patch,
        //             PatchFormat::Context,
        //         )));
        //     }

        //     if is_no_new_line(line) {
        //         return Ok(PatchLine::NoNewLine(PatchLineData::new(
        //             line,
        //             line_in_patch,
        //             PatchFormat::Context,
        //         )));
        //     }

        //     let is_context_original_range = line.starts_with(CONTEXT_ORIGINAL_RANGE_STARTER)
        //         && line.ends_with(CONTEXT_ORIGINAL_RANGE_FINISHER);

        //     let is_context_modified_range = line.starts_with(CONTEXT_MODIFIED_RANGE_STARTER)
        //         && line.ends_with(CONTEXT_MODIFIED_RANGE_FINISHER);

        //     let is_original = if is_context_original_range {
        //         Some(true)
        //     } else if is_context_modified_range {
        //         Some(false)
        //     } else {
        //         if line.starts_with(CONTEXT_ORIGINAL_RANGE_STARTER)
        //             || line.starts_with(CONTEXT_MODIFIED_RANGE_STARTER)
        //         {
        //             return Err(PatchError::InvalidContextRangeHeader);
        //         }

        //         None
        //     };

        //     if let Some(is_original) = is_original {
        //         let range_result = Range::try_from_context(line);

        //         if let Ok(range) = range_result {
        //             return Ok(PatchLine::ContextHunkRange(ContextHunkRangeData::new(
        //                 line.to_string(),
        //                 line_in_patch,
        //                 range,
        //                 is_original,
        //             )));
        //         } else if let Err(_error) = range_result {
        //             return Err(PatchError::InvalidContextRangeHeader);
        //         };
        //     }

        //     let context_identifier = &line[0..2];

        //     let result = match context_identifier {
        //         "+ " => Ok(Self::ContextInserted(
        //             PatchLineData::new(line, line_in_patch, PatchFormat::Context),
        //             false,
        //         )),
        //         "- " => Ok(Self::ContextDeleted(
        //             PatchLineData::new(line, line_in_patch, PatchFormat::Context),
        //             false,
        //         )),
        //         "  " => Ok(Self::ContextUnchanged(PatchLineData::new(
        //             line,
        //             line_in_patch,
        //             PatchFormat::Context,
        //         ))),
        //         "! " => if_else(
        //             is_original_line,
        //             Ok(Self::ContextDeleted(
        //                 PatchLineData::new(line, line_in_patch, PatchFormat::Context),
        //                 true,
        //             )),
        //             Ok(Self::ContextInserted(
        //                 PatchLineData::new(line, line_in_patch, PatchFormat::Context),
        //                 true,
        //             )),
        //         ),
        //         _ => Err(PatchError::InvalidContextPatchLine),
        //     };

        //     return result;
        // }

        // Err(PatchError::InvalidContextPatchLine)
    }

    pub(crate) fn try_from_edit_script(
        line: &'a str,
        line_in_patch: usize,
        current_ed_hunk_kind: EditScriptHunkKind,
    ) -> Result<Self, PatchError> {
        if is_no_new_line(line) {
            return Ok(PatchLine::NoNewLine(PatchLineData::new(
                line,
                line_in_patch,
                PatchFormat::EditScript,
            )));
        }

        if is_edit_script_range(line) {
            let range = Range::try_from_edit_script(line);
            if let Ok(range) = range {
                return Ok(PatchLine::EditScriptRange(EditScriptRangeData::new(
                    line,
                    line_in_patch,
                    range,
                    Range::edit_script_range_kind(line),
                )));
            } else {
                return Err(PatchError::InvalidEditScriptRange);
            }
        }

        match current_ed_hunk_kind {
            EditScriptHunkKind::Insert => Ok(PatchLine::EditScriptInsert(PatchLineData::new(
                line,
                line_in_patch,
                PatchFormat::EditScript,
            ))),
            EditScriptHunkKind::Delete => panic!("ed has not lines in delete hunks."),
            EditScriptHunkKind::Change => Ok(PatchLine::EditScriptChange(PatchLineData::new(
                line,
                line_in_patch,
                PatchFormat::EditScript,
            ))),
        }
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

    pub(crate) fn is_edit_script_range(&self) -> bool {
        matches!(self, PatchLine::EditScriptRange(_))
    }

    pub(crate) fn is_normal_range(&self) -> bool {
        matches!(self, PatchLine::NormalRange(_))
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum NormalPatchError {
    InvalidPatchLine(String),
    InvalidPatchRange(String),
}

#[derive(Debug)]
#[allow(dead_code)]
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
    InvalidEditScriptRange,
    InvalidEditScriptPatch,
    CouldNotOpenPatchDestinationFile,
    IOError(io::ErrorKind),
    NormalPatch(NormalPatchError),
}

impl Display for PatchError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            PatchError::EmptyLineNotAllowed => "EmptyLineNotAllowed",
            PatchError::InvalidUnifiedHunkHeader => "InvalidUnifiedHunkHeader",
            PatchError::InvalidUnifiedPatchLine => "InvalidUnifiedPatchLine",
            PatchError::InvalidPatchHeader => "InvalidPatchHeader",
            PatchError::InvalidPatchFile => "InvalidPatchFile",
            PatchError::InvalidPatchLineWithData(_) => "InvalidPatchLineWithData",
            PatchError::CouldNotGuessPatchFormat => "CouldNotGuessPatchFormat",
            PatchError::InvalidContextPatchLine => "InvalidContextPatchLine",
            PatchError::InvalidContextRangeHeader => "InvalidContextRangeHeader",
            PatchError::PatchFormatUnavailable => "PatchFormatUnavailable",
            PatchError::InvalidContextHunkRange => "InvalidContextHunkRange",
            PatchError::ContextOrderedLinesUnavailable => "ContextOrderedLinesUnavailable",
            PatchError::InvalidEditScriptRange => "InvalidEditScriptRange",
            PatchError::InvalidEditScriptPatch => "InvalidEditScriptPatch",
            PatchError::NormalPatch(normal_patch_error) => match normal_patch_error {
                NormalPatchError::InvalidPatchLine(line) => "Could not classify normal patch line.",
                NormalPatchError::InvalidPatchRange(line) => {
                    "This line supposed to be normal patch range."
                }
            },
            PatchError::CouldNotOpenPatchDestinationFile => "CouldNotOpenPatchDestinationFile",
            PatchError::IOError(_error_kind) => "IOError",
        };

        write!(formatter, "{}", message)
    }
}
