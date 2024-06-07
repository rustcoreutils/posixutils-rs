use std::{borrow::BorrowMut, sync::Arc};

use crate::patch_utils::patch_line::PatchLine;

use super::{
    constants::{
        CONTEXT_FIRST_LINE_STARTER, CONTEXT_SECOND_LINE_STARTER, UNIFIED_FIRST_LINE_STARTER,
        UNIFIED_SECOND_LINE_STARTER,
    },
    context_hunk_data::ContextHunkData,
    context_hunk_range_data::ContextHunkRangeData,
    hunk::{self, Hunk},
    patch_file::PatchFile,
    patch_file_kind::PatchFileKind,
    patch_format::PatchFormat,
    patch_line::{self, PatchError},
    patch_options::PatchOptions,
    unified_hunk_data::UnifiedHunkData,
};

pub type PatchResult<T> = Result<T, PatchError>;

#[derive(Debug)]
pub struct Patch<'a> {
    patch: &'a PatchFile,
    file: &'a PatchFile,
    hunks: Vec<Hunk<'a>>,
    patch_f1_header: Option<&'a String>,
    patch_f2_header: Option<&'a String>,
    patch_options: PatchOptions,
}

impl<'a> Patch<'a> {
    pub fn try_new(
        patch_file: &'a PatchFile,
        main_file: &'a PatchFile,
        patch_options: PatchOptions,
    ) -> Result<Self, PatchError> {
        if patch_file.kind() == PatchFileKind::Patch && main_file.kind() != PatchFileKind::Patch {
            let patch_format = if let Some(patch_format) = patch_options.patch_format() {
                *patch_format
            } else {
                Self::guess_patch_format(patch_file.lines())
            };

            let hunks = match &patch_format {
                PatchFormat::None => return Err(PatchError::CouldNotGuessPatchFormat),
                PatchFormat::Default => Self::parse_default_patch(patch_file.lines())?,
                PatchFormat::Unified => Self::parse_unified_patch(patch_file.lines())?,
                PatchFormat::Context => Self::parse_context_patch(patch_file.lines())?,
                PatchFormat::EditScript => Self::parse_ed_patch(patch_file.lines())?,
            };

            let (patch_f1_header, patch_f2_header) = match patch_format {
                PatchFormat::None => (None, None),
                PatchFormat::Default => (None, None),
                PatchFormat::Unified => {
                    (Some(&patch_file.lines()[0]), Some(&patch_file.lines()[1]))
                }
                PatchFormat::Context => {
                    (Some(&patch_file.lines()[0]), Some(&patch_file.lines()[1]))
                }
                PatchFormat::EditScript => (None, None),
            };

            let mut patch_options = patch_options;
            patch_options.update_patch_format(patch_format);

            let possible_self = Self {
                patch: patch_file,
                file: main_file,
                hunks,
                patch_f1_header,
                patch_f2_header,
                patch_options,
            };

            possible_self.validate_hunks()?;

            return Ok(possible_self);
        }

        Err(PatchError::InvalidPatchFile)
    }

    /// panics if patch_format is NONE
    pub fn apply(&'a mut self) -> PatchResult<()> {
        if let Some(patch_format) = self.patch_options.patch_format() {
            match patch_format {
                PatchFormat::None => panic!("PatchFormat should be valid!"),
                PatchFormat::Default => self.apply_default(),
                PatchFormat::Unified => self.apply_unified(),
                PatchFormat::Context => self.apply_context(),
                PatchFormat::EditScript => self.apply_edit_script(),
            }
        } else {
            Err(PatchError::PatchFormatUnavailable)
        }
    }

    fn guess_patch_format(lines: &[String]) -> PatchFormat {
        if lines[0].starts_with(UNIFIED_FIRST_LINE_STARTER)
            && lines[1].starts_with(UNIFIED_SECOND_LINE_STARTER)
        {
            return PatchFormat::Unified;
        }

        if lines[0].starts_with(CONTEXT_FIRST_LINE_STARTER)
            && lines[1].starts_with(CONTEXT_SECOND_LINE_STARTER)
        {
            return PatchFormat::Context;
        }

        PatchFormat::None
    }

    fn parse_unified_patch(lines: &'a [String]) -> PatchResult<Vec<Hunk>> {
        let mut hunks = Vec::<Hunk>::new();

        for (i, line) in lines.iter().enumerate().skip(2) {
            let patch_line = PatchLine::try_from_unified(line, i + 1);

            if let Ok(patch_line) = patch_line {
                if patch_line.is_unified_hunk_header() {
                    let hunk_header_data = patch_line.unified_hunk_header_data().unwrap();
                    hunks.push(Hunk::new_unified_hunk(UnifiedHunkData::new(
                        hunk_header_data.f1_range(),
                        hunk_header_data.f2_range(),
                        vec![patch_line],
                    )));
                } else if let Some(last_hunk) = hunks.last_mut() {
                    last_hunk
                        .unified_hunk_data_mut()
                        .lines_mut()
                        .push(patch_line);
                } else {
                    return Err(PatchError::InvalidPatchLineWithData(
                        patch_line.line().to_string(),
                    ));
                }
            } else if let Err(error) = patch_line {
                return Err(error);
            }
        }

        Ok(hunks)
    }

    fn parse_context_patch(lines: &'a [String]) -> PatchResult<Vec<Hunk<'a>>> {
        let mut hunks = Vec::<Hunk<'a>>::new();

        for (i, line) in lines.iter().enumerate().skip(2) {
            let mut patch_line_to_add: Option<PatchLine> = None;
            let mut hunk_to_add: Option<Hunk> = None;
            let mut is_hunk_empty = true;
            let patch_line = PatchLine::try_from_context(line, i + 1);

            if let Ok(patch_line) = patch_line {
                if patch_line.is_context_hunk_separator() {
                    is_hunk_empty = false;

                    hunk_to_add = Some(Hunk::new_context_hunk(ContextHunkData::new(
                        None,
                        None,
                        vec![patch_line],
                        vec![],
                    )));
                } else if !is_hunk_empty {
                    if patch_line.is_context_range() {
                        let last_hunk = hunks.last_mut().unwrap();
                        let context_range_data = patch_line.context_hunk_range_data().unwrap();

                        patch_line_to_add =
                            Some(PatchLine::ContextHunkRange(ContextHunkRangeData::new(
                                line.to_string(),
                                context_range_data.line_in_patch(),
                                context_range_data.range(),
                                context_range_data.is_original(),
                            )));
                    } else {
                        patch_line_to_add = Some(patch_line);
                    }
                } else {
                    return Err(PatchError::InvalidPatchLineWithData(
                        patch_line.line().to_string(),
                    ));
                }
            } else if let Err(error) = patch_line {
                return Err(error);
            }

            if let Some(patch_line) = patch_line_to_add {
               if let Some(hunk) = &mut hunk_to_add {
                    hunk.context_hunk_data_mut().add_line(patch_line);
                }
            }

            // if let Some(hunk_to_add) = hunk_to_add {
            //     hunks.push(hunk_to_add);
            // }
        }

        Ok(hunks)
    }

    fn parse_ed_patch(lines: &[String]) -> PatchResult<Vec<Hunk>> {
        todo!()
    }

    fn parse_default_patch(lines: &[String]) -> PatchResult<Vec<Hunk>> {
        todo!()
    }

    pub fn apply_unified(&mut self) -> PatchResult<()> {
        self.hunks
            .sort_by_key(|hunk| hunk.unified_hunk_data().f1_range().start());
        let mut new_file_line = 1usize;
        let mut old_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        for i in 0..self.hunks.len() {
            let hunk = &mut self.hunks[i];
            let f1_range = hunk.unified_hunk_data().f1_range();
            let f2_range = hunk.unified_hunk_data().f2_range();

            if f1_range.start() > old_file_line {
                let start = old_file_line;
                for j in start..f1_range.start() {
                    println!("{}", self.file.lines()[j - 1]);
                    old_file_line += 1;
                    new_file_line += 1;
                }
            }

            for patch_line in hunk.unified_hunk_data().lines() {
                match patch_line {
                    PatchLine::UnifiedHunkHeader(_) => {}
                    PatchLine::UnifiedDeleted(data) => {
                        old_file_line += 1;
                    }
                    PatchLine::UnifiedUnchanged(data) => {
                        println!("{}", patch_line.original_line());
                        new_file_line += 1;
                        old_file_line += 1;
                    }
                    PatchLine::UnifiedInserted(data) => {
                        println!("{}", patch_line.original_line());
                        new_file_line += 1;
                    }
                    PatchLine::NoNewLine(_) => no_new_line_count += 1,
                    _ => return Err(PatchError::InvalidUnifiedPatchLine),
                }
            }
        }

        match no_new_line_count {
            0 => println!(),
            1 => {
                if self.file.kind() == PatchFileKind::Original && !self.file.ends_with_newline() {
                    println!();
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn apply_context(&mut self) -> PatchResult<()> {
        self.hunks
            .sort_by_key(|hunk| hunk.context_hunk_data().f1_range().unwrap().start());
        let mut new_file_line = 1usize;
        let mut old_file_line = 1usize;

        for i in 0..self.hunks.len() {
            let mut hunk = &mut self.hunks[i];
            let f2_range = hunk.context_hunk_data().f2_range();
            let f1_range = hunk.context_hunk_data().f1_range();

            if f1_range.is_none() || f2_range.is_none() {
                return Err(PatchError::InvalidContextHunkRange);
            }

            let (f1_range, f2_range) = (f1_range.unwrap(), f2_range.unwrap());

            if f1_range.start() > old_file_line {
                let start = old_file_line;
                for j in start..f1_range.start() {
                    println!("{}", self.file.lines()[j - 1]);
                    old_file_line += 1;
                    new_file_line += 1;
                }
            }

            hunk.order_context_lines();
            let ordered_lines = hunk.context_hunk_data().ordered_lines();

            if let Some(ordered_lines) = ordered_lines {
                for patch_line in ordered_lines {
                    match patch_line {
                        PatchLine::ContextHunkSeparator(_) => todo!(),
                        PatchLine::ContextInserted(_) => {
                            println!("{}", patch_line.original_line())
                        }
                        PatchLine::ContextDeleted(_) => todo!(),
                        PatchLine::ContextUnchanged(_) => todo!(),
                        PatchLine::ContextSubstituted(_) => todo!(),
                        PatchLine::ContextHunkRange(_) => todo!(),
                        PatchLine::NoNewLine(_) => todo!(),
                        _ => return Err(PatchError::InvalidContextPatchLine),
                    }
                }
            } else {
                return Err(PatchError::ContextOrderedLinesUnavailable);
            }
        }

        // match no_new_line_count {
        //     0 => println!(),
        //     1 => {
        //         if self.file.kind() == PatchFileKind::Original && !self.file.ends_with_newline() {
        //             println!();
        //         }
        //     }
        //     _ => {}
        // };

        Ok(())
    }

    fn apply_default(&self) -> PatchResult<()> {
        todo!()
    }

    fn apply_edit_script(&self) -> PatchResult<()> {
        todo!()
    }

    fn validate_hunks(&self) -> PatchResult<()> {
        // TODO
        Ok(())
    }
}
