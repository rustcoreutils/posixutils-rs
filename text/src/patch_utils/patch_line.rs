use std::{fmt::Display, io};

use crate::patch_utils::{
    constants::normal::{normal_regex_cache, NormalRegexKind},
    functions::is_normal_head,
};

use super::{
    context_hunk_range_data::ContextHunkRangeData, edit_script_range_data::EditScriptRangeData,
    functions::is_no_new_line, normal_range_data::NormalRangeData, patch_format::PatchFormat,
    patch_line_data::PatchLineData, unified_hunk_header_data::UnifiedHunkHeaderData,
};

#[derive(Debug, Clone)]
pub enum PatchLine<'a> {
    UnifiedHunkHeader(UnifiedHunkHeaderData<'a>),
    UnifiedUnchanged(PatchLineData<'a>),
    UnifiedInserted(PatchLineData<'a>),
    UnifiedDeleted(PatchLineData<'a>),
    NoNewLine(PatchLineData<'a>),
    NewLine(PatchLineData<'a>),
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
    ContextHunkRange(ContextHunkRangeData<'a>),
    EditScriptRange(EditScriptRangeData<'a>),
    EditScriptInsert(PatchLineData<'a>),
    EditScriptChange(PatchLineData<'a>),
    EditScriptDelete(PatchLineData<'a>),
    EditScriptLineDot(PatchLineData<'a>),
    NormalRange(NormalRangeData<'a>),
    NormalChangeSeparator(PatchLineData<'a>),
    NormalLineInsert(PatchLineData<'a>),
    NormalLineDelete(PatchLineData<'a>),
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
            PatchLine::NormalLineInsert(data) => data.kind(),
            PatchLine::NormalLineDelete(data) => data.kind(),
            PatchLine::NewLine(data) => data.kind(),
            PatchLine::EditScriptLineDot(data) => data.kind(),
            PatchLine::EditScriptDelete(data) => data.kind(),
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
            PatchLine::NormalLineInsert(data) => &data.line()[2..],
            PatchLine::NormalLineDelete(data) => &data.line()[2..],
            PatchLine::NewLine(data) => data.line(),
            PatchLine::EditScriptLineDot(data) => data.line(),
            PatchLine::EditScriptDelete(data) => data.line(),
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
            PatchLine::NormalLineInsert(data) => data.line(),
            PatchLine::NormalLineDelete(data) => data.line(),
            PatchLine::NewLine(data) => data.line(),
            PatchLine::EditScriptLineDot(data) => data.line(),
            PatchLine::EditScriptDelete(data) => data.line(),
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

    // TODO: this may be deleted!
    pub fn try_from_normal(line: &'a str) -> Result<Self, PatchLineError> {
        let normal_regex = normal_regex_cache();

        if is_normal_head(line) {
            let range_data = NormalRangeData::try_from(line)
                .expect("NormalRange verified by regex is expected to be parsed, with no errors");

            return Ok(PatchLine::NormalRange(range_data));
        }

        let patch_line_data = PatchLineData::new(line, PatchFormat::Normal);

        if is_no_new_line(line) {
            return Ok(PatchLine::NoNewLine(patch_line_data));
        }

        if normal_regex[&NormalRegexKind::LineChangeSeparator].is_match(line) {
            return Ok(PatchLine::NormalChangeSeparator(patch_line_data));
        }

        if normal_regex[&NormalRegexKind::LineInsert].is_match(line) {
            return Ok(PatchLine::NormalLineInsert(patch_line_data));
        }

        if normal_regex[&NormalRegexKind::LineDelete].is_match(line) {
            return Ok(PatchLine::NormalLineDelete(patch_line_data));
        }

        return Err(PatchLineError::NormalPatch(
            NormalPatchError::InvalidPatchLine(line.to_owned()),
        ));
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
pub enum NormalPatchError {
    InvalidPatchLine(String),
    InvalidPatchRange(String),
}

#[derive(Debug)]
pub enum PatchLineError {
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

impl Display for PatchLineError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            PatchLineError::EmptyLineNotAllowed => "EmptyLineNotAllowed",
            PatchLineError::InvalidUnifiedHunkHeader => "InvalidUnifiedHunkHeader",
            PatchLineError::InvalidUnifiedPatchLine => "InvalidUnifiedPatchLine",
            PatchLineError::InvalidPatchHeader => "InvalidPatchHeader",
            PatchLineError::InvalidPatchFile => "InvalidPatchFile",
            PatchLineError::InvalidPatchLineWithData(_) => "InvalidPatchLineWithData",
            PatchLineError::CouldNotGuessPatchFormat => "CouldNotGuessPatchFormat",
            PatchLineError::InvalidContextPatchLine => "InvalidContextPatchLine",
            PatchLineError::InvalidContextRangeHeader => "InvalidContextRangeHeader",
            PatchLineError::PatchFormatUnavailable => "PatchFormatUnavailable",
            PatchLineError::InvalidContextHunkRange => "InvalidContextHunkRange",
            PatchLineError::ContextOrderedLinesUnavailable => "ContextOrderedLinesUnavailable",
            PatchLineError::InvalidEditScriptRange => "InvalidEditScriptRange",
            PatchLineError::InvalidEditScriptPatch => "InvalidEditScriptPatch",
            PatchLineError::NormalPatch(normal_patch_error) => match normal_patch_error {
                NormalPatchError::InvalidPatchLine(line) => "Could not classify normal patch line!",
                NormalPatchError::InvalidPatchRange(line) => "This line supposed to be normal patch range: "
            },
            PatchLineError::CouldNotOpenPatchDestinationFile => "CouldNotOpenPatchDestinationFile",
            PatchLineError::IOError(_error_kind) => "IOError",
        };

        write!(formatter, "{}", message)
    }
}
