use std::{borrow::BorrowMut, sync::Arc, vec};

use crate::patch_utils::{context_hunk_data::ContextHunkOrderIndex, patch_line::PatchLine};

use super::{
    constants::{
        CONTEXT_FIRST_LINE_STARTER, CONTEXT_SECOND_LINE_STARTER, UNIFIED_FIRST_LINE_STARTER,
        UNIFIED_SECOND_LINE_STARTER,
    },
    context_hunk_data::ContextHunkData,
    context_hunk_range_data::ContextHunkRangeData,
    edit_script_hunk_data::EditScriptHunkData,
    edit_script_range_data::EditScriptHunkKind,
    functions::{is_edit_script_range, is_normal_range},
    hunk::{self, Hunk},
    hunks::Hunks,
    normal_hunk_data::NormalHunkData,
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
    hunks: Hunks<'a>,
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
                PatchFormat::Unified => Self::parse_unified_patch(patch_file.lines())?,
                PatchFormat::Context => Self::parse_context_patch(patch_file.lines())?,
                PatchFormat::EditScript => Self::parse_ed_patch(patch_file.lines())?,
                PatchFormat::Normal => Self::parse_normal_patch(patch_file.lines())?,
            };

            let (patch_f1_header, patch_f2_header) = match patch_format {
                PatchFormat::None => (None, None),
                PatchFormat::Normal => (None, None),
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

            let mut possible_self = Self {
                patch: patch_file,
                file: main_file,
                hunks,
                patch_f1_header,
                patch_f2_header,
                patch_options,
            };

            possible_self.validate_hunks()?;

            match possible_self.hunks.kind() {
                PatchFormat::None => {}
                PatchFormat::Normal => {}
                PatchFormat::Unified => {}
                PatchFormat::Context => {
                    possible_self.hunks.modify_hunks(|hunks_mut| {
                        for hunk in hunks_mut {
                            hunk.order_context_lines();
                        }
                    });
                }
                PatchFormat::EditScript => {}
            }

            return Ok(possible_self);
        }

        Err(PatchError::InvalidPatchFile)
    }

    /// panics if patch_format is NONE
    pub fn apply(&'a mut self) -> PatchResult<()> {
        if let Some(patch_format) = self.patch_options.patch_format() {
            match patch_format {
                PatchFormat::None => panic!("PatchFormat should be valid!"),
                PatchFormat::Normal => self.apply_normal(),
                PatchFormat::Unified => self.apply_unified(),
                PatchFormat::Context => self.apply_context(),
                PatchFormat::EditScript => self.apply_edit_script(),
                _ => todo!(),
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

        if is_edit_script_range(&lines[0]) {
            return PatchFormat::EditScript;
        }

        if is_normal_range(&lines[0]) {
            return PatchFormat::Normal;
        }

        PatchFormat::None
    }

    fn parse_unified_patch(lines: &'a [String]) -> PatchResult<Hunks> {
        let mut hunks = Hunks::new(PatchFormat::Unified);

        for (i, line) in lines.iter().enumerate().skip(2) {
            let patch_line = PatchLine::try_from_unified(line, i + 1);

            if let Ok(patch_line) = patch_line {
                if patch_line.is_unified_hunk_header() {
                    let hunk_header_data = patch_line.unified_hunk_header_data().unwrap();
                    hunks.add_hunk(Hunk::new_unified_hunk(UnifiedHunkData::new(
                        hunk_header_data.f1_range(),
                        hunk_header_data.f2_range(),
                        vec![patch_line],
                    )));
                } else if hunks.has_no_hunks() {
                    return Err(PatchError::InvalidPatchLineWithData(
                        patch_line.line().to_string(),
                    ));
                } else {
                    hunks.add_patch_line(patch_line);
                }
            } else if let Err(error) = patch_line {
                return Err(error);
            }
        }

        Ok(hunks)
    }

    fn parse_context_patch(lines: &'a [String]) -> PatchResult<Hunks> {
        let mut hunks = Hunks::new(PatchFormat::Context);
        let mut is_original_line = true;

        for (i, line) in lines.iter().enumerate().skip(2) {
            let patch_line = PatchLine::try_from_context(line, i + 1, is_original_line);

            if let Ok(patch_line) = patch_line {
                if patch_line.is_context_hunk_separator() {
                    hunks.add_hunk(Hunk::new_context_hunk(ContextHunkData::new(
                        None,
                        None,
                        vec![],
                        vec![],
                    )));

                    hunks.add_patch_line(patch_line);
                } else if hunks.has_no_hunks() {
                    return Err(PatchError::InvalidPatchLineWithData(
                        patch_line.line().to_string(),
                    ));
                } else if patch_line.is_context_range() {
                    let context_range_data = patch_line.context_hunk_range_data().unwrap();
                    is_original_line = context_range_data.is_original();

                    hunks.add_patch_line(PatchLine::ContextHunkRange(ContextHunkRangeData::new(
                        line.to_string(),
                        context_range_data.line_in_patch(),
                        context_range_data.range(),
                        context_range_data.is_original(),
                    )));
                } else {
                    hunks.add_patch_line(patch_line);
                }
            } else if let Err(error) = patch_line {
                return Err(error);
            }
        }

        Ok(hunks)
    }

    fn parse_ed_patch(lines: &'a [String]) -> PatchResult<Hunks<'a>> {
        let mut hunks = Hunks::new(PatchFormat::EditScript);
        let mut last_ed_hunk_kind = EditScriptHunkKind::Change;

        for (i, line) in lines.iter().enumerate() {
            let patch_line = PatchLine::try_from_edit_script(line, i + 1, last_ed_hunk_kind);

            if let Ok(patch_line) = patch_line {
                if patch_line.is_edit_script_range() {
                    let edit_script_range_data = patch_line.edit_script_range_data().unwrap();
                    last_ed_hunk_kind = edit_script_range_data.kind();
                    let range = edit_script_range_data.range();

                    hunks.add_hunk(Hunk::new_edit_script_hunk(EditScriptHunkData::new(
                        vec![],
                        range,
                        edit_script_range_data.kind(),
                    )));
                }

                if hunks.has_no_hunks() {
                    return Err(PatchError::InvalidEditScriptPatch);
                }

                hunks.add_patch_line(patch_line);
            } else if let Err(error) = patch_line {
                return Err(error);
            }
        }

        Ok(hunks)
    }

    fn parse_normal_patch(lines: &'a [String]) -> PatchResult<Hunks<'a>> {
        let mut hunks = Hunks::new(PatchFormat::Normal);

        for (i, line) in lines.iter().enumerate() {
            let patch_line = PatchLine::try_from_normal(line, i + 1);

            if let Ok(patch_line) = patch_line {
                if patch_line.is_normal_range() {
                    let normal_range_data = patch_line
                        .normal_hunk_range_data()
                        .expect("NormalRangeData can not be None here!");

                    hunks.add_hunk(Hunk::new_normal_hunk(NormalHunkData::new(
                        normal_range_data.range_left(),
                        normal_range_data.range_right(),
                        vec![],
                        normal_range_data.kind(),
                    )));
                }

                if hunks.has_no_hunks() {
                    panic!("NormalHunk should not be empty at this point.");
                }

                hunks.add_patch_line(patch_line);
            } else if let Err(error) = patch_line {
                return Err(error);
            }
        }

        Ok(hunks)
    }

    pub fn apply_unified(&'a mut self) -> PatchResult<()> {
        self.hunks.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| hunk.unified_hunk_data().f1_range().start())
        });

        let mut new_file_line = 1usize;
        let mut old_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        let hunks = self.hunks.hunks();

        for (i, hunk) in hunks.iter().enumerate() {
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
        self.hunks.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| {
                hunk.context_hunk_data()
                    .f1_range()
                    .expect("Invalid f1_range for ContextHunkData!")
                    .start()
            })
        });

        let mut new_file_line = 1usize;
        let mut old_file_line = 1usize;

        for (i, hunk) in self.hunks.hunks().iter().enumerate() {
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

            let ordered_lines_indeces = hunk.context_hunk_ordered_lines_indeces();

            if let Some(ordered_lines_indeces) = ordered_lines_indeces {
                let x = ordered_lines_indeces.iter().map(|order_index| {});

                for order_line_index in ordered_lines_indeces {
                    let patch_line = hunk
                        .context_hunk_data()
                        .get_by_order_index(order_line_index);

                    match patch_line {
                        PatchLine::ContextInserted(_) => {
                            println!("{}", patch_line.original_line());
                            new_file_line += 1;
                        }
                        PatchLine::ContextDeleted(data) => {
                            old_file_line += 1;
                        }
                        PatchLine::ContextUnchanged(data) => {
                            println!("{}", patch_line.original_line());
                            old_file_line += 1;
                            new_file_line += 1;
                        }
                        PatchLine::NoNewLine(data) => {
                            if matches!(order_line_index, ContextHunkOrderIndex::Original(_)) {
                                old_file_line += 1;
                            }

                            new_file_line += 1;
                        }

                        _ => {
                            dbg!(&patch_line);
                            return Err(PatchError::InvalidContextPatchLine);
                        }
                    }
                }
            } else {
                panic!("ContextHunkData.ordered_lines should not be None!");
            }
        }

        Ok(())
    }

    fn apply_normal(&self) -> PatchResult<()> {
        println!("{:?}" , self);
        std::process::exit(0);
    }

    fn apply_edit_script(&mut self) -> PatchResult<()> {
        self.hunks.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| hunk.edit_script_hunk_data().range().end())
        });

        let mut new_file_line = 1usize;
        let mut old_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        let hunks = self.hunks.hunks();

        for (i, hunk) in hunks.iter().enumerate() {
            let range = hunk.edit_script_hunk_data().range();

            if (old_file_line - 1) <= range.start() {
                let old_file_boundry = if matches!(
                    hunk.edit_script_hunk_data().kind(),
                    EditScriptHunkKind::Insert
                ) {
                    range.start() + 1
                } else {
                    range.start()
                };
                while old_file_line < old_file_boundry {
                    println!("{}", self.file.lines()[old_file_line - 1]);
                    old_file_line += 1;
                    new_file_line += 1;
                }
            }

            let hunk_lines_len = hunk.edit_script_hunk_data().lines().len();

            for (j, patch_line) in hunk.edit_script_hunk_data().lines().iter().enumerate() {
                if j == hunk_lines_len.wrapping_sub(1) && patch_line.line() == "." {
                    break;
                }

                match patch_line {
                    PatchLine::EditScriptRange(data) => {
                        if matches!(data.kind(), EditScriptHunkKind::Delete) {
                            let range = data.range();
                            old_file_line += range.end() - range.start() + 1;
                        }

                        if matches!(data.kind(), EditScriptHunkKind::Change) {
                            old_file_line += range.end() - range.start() + 1;
                        }
                    }
                    PatchLine::EditScriptInsert(data) => {
                        println!("{}", data.line());
                        new_file_line += 1;
                    }
                    PatchLine::EditScriptChange(data) => {
                        println!("{}", data.line());
                        new_file_line += 1;
                    }
                    PatchLine::NoNewLine(_) => no_new_line_count += 1,
                    _ => panic!("Invalid PatchLine detected in EditScriptHunkData"),
                }
            }
        }

        if self.file.lines().len() > old_file_line {
            while old_file_line <= self.file.lines().len() {
                println!("{}", self.file.lines()[old_file_line - 1]);
                old_file_line += 1;
                new_file_line += 1;
            }
        }

        match no_new_line_count {
            0 => println!(""),
            1 => {
                if self.file.kind() == PatchFileKind::Original && !self.file.ends_with_newline() {
                    println!("");
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn validate_hunks(&self) -> PatchResult<()> {
        // TODO
        Ok(())
    }
}
