use std::io::Write;
use std::{
    fs::{self, File},
    io::{self, BufWriter},
};

use crate::patch_utils::patch_line::PatchLine;

use super::{
    constants::{
        context::ORIGINAL_SKIP, CONTEXT_FIRST_LINE_STARTER, CONTEXT_SECOND_LINE_STARTER,
        UNIFIED_FIRST_LINE_STARTER, UNIFIED_SECOND_LINE_STARTER,
    },
    context_hunk_data::ContextHunkData,
    context_hunk_range_data::ContextHunkRangeData,
    edit_script_hunk_data::EditScriptHunkData,
    edit_script_range_data::EditScriptHunkKind,
    functions::{is_edit_script_range, is_normal_range, print_error},
    hunk::Hunk,
    hunks::Hunks,
    normal_hunk_data::NormalHunkData,
    patch_file::PatchFile,
    patch_file_kind::PatchFileKind,
    patch_format::PatchFormat,
    patch_line::PatchError,
    patch_options::PatchOptions,
    unified_hunk_data::UnifiedHunkData,
};

pub type PatchResult<T> = Result<T, PatchError>;

#[derive(Debug)]
#[allow(dead_code)]
pub struct Patch<'a> {
    patch: &'a PatchFile,
    file: &'a PatchFile,
    hunks: Hunks<'a>,
    patch_f1_header: Option<&'a String>,
    patch_f2_header: Option<&'a String>,
    patch_options: PatchOptions,
    output: BufWriter<File>,
}

impl<'a> Patch<'a> {
    pub fn try_new(
        patch_file: &'a PatchFile,
        main_file: &'a PatchFile,
        patch_options: PatchOptions,
    ) -> Result<Self, PatchError> {
        if matches!(patch_file.kind(), PatchFileKind::Patch)
            && !matches!(main_file.kind(), PatchFileKind::Patch)
        {
            println!(
                "patching file {}",
                main_file.path().file_name().unwrap().to_str().unwrap()
            );

            let patch_format = if let Some(patch_format) = patch_options.patch_format() {
                *patch_format
            } else {
                Self::guess_patch_format(patch_file.lines())
            };

            let hunks = match &patch_format {
                PatchFormat::None => return Err(PatchError::CouldNotGuessPatchFormat),
                PatchFormat::Normal => Self::parse_normal_patch(patch_file.lines())?,
                PatchFormat::Unified => Self::parse_unified_patch(patch_file.lines())?,
                PatchFormat::Context => Self::parse_context_patch(patch_file.lines())?,
                PatchFormat::EditScript => Self::parse_ed_patch(patch_file.lines())?,
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

            let output_file = File::open(main_file.path());

            if output_file.is_err() {
                return Err(PatchError::CouldNotOpenPatchDestinationFile);
            }

            let output =
                BufWriter::new(output_file.expect("failed to open patch destination file!"));

            let possible_self = Self {
                patch: patch_file,
                file: main_file,
                hunks,
                patch_f1_header,
                patch_f2_header,
                patch_options,
                output: output,
            };

            possible_self.verify();

            return Ok(possible_self);
        }

        Err(PatchError::InvalidPatchFile)
    }

    /// panics if patch_format is NONE or reversed is enabled with ed script
    pub fn apply(&'a mut self) -> PatchResult<()> {
        if let Some(patch_format) = self.patch_options.patch_format() {
            match (patch_format, self.patch_options.reverse()) {
                (PatchFormat::None, _) => panic!("PatchFormat should be valid!"),
                (PatchFormat::Normal, false) => self.apply_normal(),
                (PatchFormat::Normal, true) => self.apply_normal_reverse(),
                (PatchFormat::Unified, false) => self.apply_unified(),
                (PatchFormat::Unified, true) => self.apply_unified_reverse(),
                (PatchFormat::Context, false) => self.apply_context(),
                (PatchFormat::Context, true) => self.apply_context_reverse(),
                (PatchFormat::EditScript, false) => self.apply_edit_script(),
                (PatchFormat::EditScript, true) => {
                    print_error("ed format + reverse option is not possible!");
                    std::process::exit(0);
                }
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

    fn apply_unified(&'a mut self) -> PatchResult<()> {
        self.hunks.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| hunk.unified_hunk_data().f1_range().start())
        });

        let mut _new_file_line = 1usize;
        let mut old_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        let hunks = self.hunks.hunks();

        for hunk in hunks.iter() {
            let f1_range = hunk.unified_hunk_data().f1_range();
            let _f2_range = hunk.unified_hunk_data().f2_range();

            if f1_range.start() > old_file_line {
                let start = old_file_line;
                for j in start..f1_range.start() {
                    println!("{}", self.file.lines()[j - 1]);
                    old_file_line += 1;
                    _new_file_line += 1;
                }
            }

            for patch_line in hunk.unified_hunk_data().lines() {
                match patch_line {
                    PatchLine::UnifiedHunkHeader(_) => {}
                    PatchLine::UnifiedDeleted(_) => {
                        old_file_line += 1;
                    }
                    PatchLine::UnifiedUnchanged(_) => {
                        println!("{}", patch_line.original_line());
                        _new_file_line += 1;
                        old_file_line += 1;
                    }
                    PatchLine::UnifiedInserted(_) => {
                        println!("{}", patch_line.original_line());
                        _new_file_line += 1;
                    }
                    PatchLine::NoNewLine(_) => no_new_line_count += 1,
                    _ => return Err(PatchError::InvalidUnifiedPatchLine),
                }
            }
        }

        match no_new_line_count {
            0 => println!(),
            1 => {
                if matches!(self.file.kind(), PatchFileKind::Original)
                    && !self.file.ends_with_newline()
                {
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

        let mut original_file_line = 1usize;
        let mut change_index = 0usize;
        let mut no_newline_count = 0usize;

        for hunk in self.hunks.hunks_mut().iter_mut() {
            let f2_range = hunk.context_hunk_data().f2_range();
            let f1_range = hunk.context_hunk_data().f1_range();

            if f1_range.is_none() || f2_range.is_none() {
                return Err(PatchError::InvalidContextHunkRange);
            }

            let range1 = f1_range.unwrap();

            if range1.start() > original_file_line {
                let start = original_file_line;
                for _ in start..range1.start() {
                    println!("{}", self.file.lines()[original_file_line - 1]);
                    original_file_line += 1;
                }
            }

            let hunk_data = hunk.context_hunk_data();
            let original_is_empty = hunk_data.is_original_empty(ORIGINAL_SKIP);

            let patch_lines: &Vec<PatchLine> = if original_is_empty {
                hunk.context_hunk_data().modified_lines()
            } else {
                hunk.context_hunk_data().original_lines()
            };

            for patch_line in patch_lines {
                match patch_line {
                    PatchLine::ContextInserted(_, is_change) => {
                        println!("{}", patch_line.original_line());

                        if *is_change {
                            original_file_line += 1;
                        }
                    }
                    PatchLine::ContextDeleted(_, is_change) => {
                        if *is_change {
                            println!(
                                "{}",
                                hunk_data.change_by_index(change_index).original_line()
                            );
                            change_index += 1;
                        }

                        original_file_line += 1;
                    }
                    PatchLine::ContextUnchanged(_) => {
                        println!("{}", patch_line.original_line());
                        original_file_line += 1;
                    }
                    PatchLine::ContextHunkRange(_) => {}
                    PatchLine::ContextHunkSeparator(_) => {}
                    PatchLine::NoNewLine(_) => {
                        no_newline_count += 1;
                    }
                    _ => {
                        return Err(PatchError::InvalidContextPatchLine);
                    }
                }
            }
        }

        match no_newline_count {
            0 => println!(),
            1 => {
                if matches!(self.file.kind(), PatchFileKind::Original)
                    && !self.file.ends_with_newline()
                {
                    println!();
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn apply_normal(&mut self) -> PatchResult<()> {
        self.hunks.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| hunk.normal_hunk_data().range_right().start())
        });

        let mut _new_file_line = 1usize;
        let mut old_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        for hunk in self.hunks.hunks().iter() {
            let range_left = hunk.normal_hunk_data().range_left();
            let _range_right = hunk.normal_hunk_data().range_right();

            if range_left.start() > old_file_line {
                let start = old_file_line;

                for j in start..range_left.start() {
                    writeln!(self.output, "{}", self.file.lines()[j - 1])?;
                    old_file_line += 1;
                    _new_file_line += 1;
                }
            }

            for patch_line in hunk.normal_hunk_data().lines() {
                match patch_line {
                    PatchLine::NormalRange(data) => {
                        let range_left = data.range_left();
                        let range_right = data.range_right();

                        let left_diff = range_left.end() - range_left.start();
                        let right_diff = range_right.end() - range_right.start();

                        match data.kind() {
                            super::normal_range_data::NormalRangeKind::Insert => {
                                if old_file_line <= range_left.start() {
                                    writeln!(
                                        self.output,
                                        "{}",
                                        self.file.lines()[old_file_line - 1]
                                    )?;
                                }

                                _new_file_line += left_diff + 1;
                            }
                            super::normal_range_data::NormalRangeKind::Change => {
                                old_file_line += left_diff + 1;
                                _new_file_line += right_diff + 1;
                            }
                            super::normal_range_data::NormalRangeKind::Delete => {
                                old_file_line += left_diff + 1;
                            }
                        }
                    }
                    PatchLine::NormalChangeSeparator(_) => {}
                    PatchLine::NormalNewLine(_) => {
                        writeln!(self.output, "{}", patch_line.original_line())?;
                    }
                    PatchLine::NormalOldLine(_) => {}
                    PatchLine::NoNewLine(_) => {
                        no_new_line_count += 1;
                    }
                    _ => panic!("Only PatchLines that start with Normal are allowed!"),
                }
            }
        }

        match no_new_line_count {
            0 => println!(),
            1 => {
                if matches!(self.file.kind(), PatchFileKind::Original)
                    && !self.file.ends_with_newline()
                {
                    writeln!(self.output, "")?;
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn apply_edit_script(&mut self) -> PatchResult<()> {
        self.hunks.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| hunk.edit_script_hunk_data().range().end())
        });

        let mut _new_file_line = 1usize;
        let mut old_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        let hunks = self.hunks.hunks();

        for hunk in hunks.iter() {
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
                    writeln!(self.output, "{}", self.file.lines()[old_file_line - 1])?;
                    old_file_line += 1;
                    _new_file_line += 1;
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
                        writeln!(self.output, "{}", data.line())?;
                        _new_file_line += 1;
                    }
                    PatchLine::EditScriptChange(data) => {
                        writeln!(self.output, "{}", data.line())?;
                        _new_file_line += 1;
                    }
                    PatchLine::NoNewLine(_) => no_new_line_count += 1,
                    _ => panic!("Invalid PatchLine detected in EditScriptHunkData"),
                }
            }
        }

        if self.file.lines().len() > old_file_line {
            while old_file_line <= self.file.lines().len() {
                writeln!(self.output, "{}", self.file.lines()[old_file_line - 1])?;
                old_file_line += 1;
                _new_file_line += 1;
            }
        }

        match no_new_line_count {
            0 => println!(),
            1 => {
                if matches!(self.file.kind(), PatchFileKind::Original)
                    && !self.file.ends_with_newline()
                {
                    println!();
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn verify(&self) {
        self.hunks.hunks().iter().for_each(|hunk| {
            hunk.verify_hunk();
        });

        let reversed = self.patch_options.reverse();
        self.verify_file(reversed)
    }

    fn apply_normal_reverse(&mut self) -> Result<(), PatchError> {
        self.hunks.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| hunk.normal_hunk_data().range_left().start())
        });

        let mut new_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        for hunk in self.hunks.hunks().iter() {
            let range_right = hunk.normal_hunk_data().range_right();

            if range_right.start() > new_file_line {
                let start = new_file_line;

                for j in start..range_right.start() {
                    writeln!(self.output, "{}", self.file.lines()[j - 1])?;
                    new_file_line += 1;
                }
            }

            for patch_line in hunk.normal_hunk_data().lines() {
                match patch_line {
                    PatchLine::NormalRange(data) => {
                        let range_right = data.range_right();
                        let right_diff = range_right.end() - range_right.start();

                        match data.kind() {
                            super::normal_range_data::NormalRangeKind::Insert => {
                                new_file_line += right_diff + 1;
                            }
                            super::normal_range_data::NormalRangeKind::Change => {
                                new_file_line += right_diff + 1;
                            }
                            super::normal_range_data::NormalRangeKind::Delete => {
                                if new_file_line <= range_right.start() {
                                    writeln!(
                                        self.output,
                                        "{}",
                                        self.file.lines()[new_file_line - 1]
                                    )?;
                                    new_file_line += 1;
                                }
                            }
                        }
                    }
                    PatchLine::NormalChangeSeparator(_) => {}
                    PatchLine::NormalNewLine(_) => {}
                    PatchLine::NormalOldLine(_) => {
                        writeln!(self.output, "{}", patch_line.original_line())?;
                    }
                    PatchLine::NoNewLine(_) => no_new_line_count += 1,
                    _ => panic!("Only PatchLines that start with Normal are allowed!"),
                }
            }
        }

        match no_new_line_count {
            0 => println!(),
            1 => {
                if matches!(self.file.kind(), PatchFileKind::Original)
                    && !self.file.ends_with_newline()
                {
                    println!();
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn apply_unified_reverse(&mut self) -> Result<(), PatchError> {
        self.hunks.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| hunk.unified_hunk_data().f2_range().end())
        });

        let mut new_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        let hunks = self.hunks.hunks();

        for hunk in hunks.iter() {
            let f2_range = hunk.unified_hunk_data().f2_range();

            if f2_range.start() > new_file_line {
                let start = new_file_line;
                for j in start..f2_range.start() {
                    writeln!(self.output, "{}", self.file.lines()[j - 1])?;
                    new_file_line += 1;
                }
            }

            for patch_line in hunk.unified_hunk_data().lines() {
                // println!("{:?}" , patch_line);
                match patch_line {
                    PatchLine::UnifiedHunkHeader(_) => {}
                    PatchLine::UnifiedDeleted(_) => {
                        writeln!(self.output, "{}", patch_line.original_line())?;
                    }
                    PatchLine::UnifiedUnchanged(_) => {
                        writeln!(self.output, "{}", patch_line.original_line())?;
                        new_file_line += 1;
                    }
                    PatchLine::UnifiedInserted(_) => {
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
                if matches!(self.file.kind(), PatchFileKind::Original)
                    && !self.file.ends_with_newline()
                {
                    writeln!(self.output, "")?;
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn apply_context_reverse(&mut self) -> Result<(), PatchError> {
        self.hunks.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| {
                hunk.context_hunk_data()
                    .f1_range()
                    .expect("Invalid f1_range for ContextHunkData!")
                    .start()
            })
        });

        let mut new_file_line = 1usize;
        let mut no_newline_count = 0usize;

        for hunk in self.hunks.hunks_mut().iter_mut() {
            let f2_range = hunk.context_hunk_data().f2_range();
            let f1_range = hunk.context_hunk_data().f1_range();

            if f1_range.is_none() || f2_range.is_none() {
                return Err(PatchError::InvalidContextHunkRange);
            }

            let f2_range = f2_range.unwrap();

            if f2_range.start() > new_file_line {
                let start = new_file_line;
                for _ in start..f2_range.start() {
                    writeln!(self.output, "{}", self.file.lines()[new_file_line - 1])?;
                    new_file_line += 1;
                }
            }

            let original_is_empty = hunk.context_hunk_data().is_original_empty(ORIGINAL_SKIP);

            let lines: &Vec<PatchLine> = if original_is_empty {
                hunk.context_hunk_data().modified_lines()
            } else {
                hunk.context_hunk_data().original_lines()
            };

            for patch_line in lines {
                match patch_line {
                    PatchLine::ContextInserted(_, _) => {
                        new_file_line += 1;
                    }
                    PatchLine::ContextDeleted(_, is_change) => {
                        writeln!(self.output, "{}", patch_line.original_line())?;

                        if *is_change {
                            new_file_line += 1;
                        }
                    }
                    PatchLine::ContextUnchanged(_) => {
                        writeln!(self.output, "{}", patch_line.original_line())?;
                        new_file_line += 1;
                    }
                    PatchLine::ContextHunkRange(_) => {}
                    PatchLine::ContextHunkSeparator(_) => {}
                    PatchLine::NoNewLine(_) => {
                        no_newline_count += 1;
                    }
                    _ => {
                        return Err(PatchError::InvalidContextPatchLine);
                    }
                }
            }
        }

        match no_newline_count {
            0 => println!(),
            1 => {
                if matches!(self.file.kind(), PatchFileKind::Modified)
                    && !self.file.ends_with_newline()
                {
                    writeln!(self.output, "")?;
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn verify_file(&self, reversed: bool) {
        for hunk in self.hunks.hunks() {
            if let Err(()) = hunk.verify_file(self.file, reversed) {
                self.reject();
                print_error("garbage patch detected!");
                std::process::exit(0);
            }
        }
    }

    fn reject(&self) {
        let reject_file_name = format!(
            "{}.rej",
            self.file.path().file_name().unwrap().to_str().unwrap()
        );
        if fs::copy(
            self.patch.path(),
            self.patch.path().with_file_name(reject_file_name),
        )
        .is_err()
        {
            print_error("failed to write reject file!");
        }
    }
}

impl From<io::Error> for PatchError {
    fn from(value: io::Error) -> Self {
        PatchError::IOError(value.kind())
    }
}
