use std::{fs::File, io::BufWriter, io::Write};

use chrono::{DateTime, Utc};

use super::{
    constants::context::ORIGINAL_SKIP,
    edit_script_range_data::EditScriptHunkKind,
    functions::print_error,
    hunk::Hunk,
    patch_error::{PatchError, PatchResult},
    patch_file::PatchFile,
    patch_file_kind::FileKind,
    patch_format::PatchFormat,
    patch_line::PatchLine,
    patch_options::PatchOptions,
};

pub trait Apply {
    fn apply(&mut self) -> PatchResult<()>;
}

#[derive(Debug)]
pub struct Hunks<'a> {
    kind: PatchFormat,
    data: Vec<Hunk<'a>>,
    output: Option<BufWriter<File>>,
    file: Option<PatchFile>,
    file1_header: Option<&'a str>,
    file1_date: Option<DateTime<Utc>>,
    file2_header: Option<&'a str>,
    file2_date: Option<DateTime<Utc>>,
    options: &'a PatchOptions,
}

impl<'a> Hunks<'a> {
    pub fn new(kind: PatchFormat, options: &'a PatchOptions) -> Self {
        assert!(
            !matches!(kind, PatchFormat::None),
            "Hunks:kind can not be PatchFormat::None"
        );

        Self {
            kind,
            data: Default::default(),
            output: None,
            file: None,
            file1_header: None,
            file2_header: None,
            options,
            file1_date: None,
            file2_date: None
        }
    }

    pub fn set_f1_header(&mut self, header: &'a str) {
        self.file1_header = Some(header)
    }

    pub fn set_f2_header(&mut self, header: &'a str) {
        self.file2_header = Some(header)
    }

    pub fn has_no_hunks(&self) -> bool {
        self.data.is_empty()
    }

    pub fn add_hunk(&mut self, hunk: Hunk<'a>) {
        let _hunk_kind = hunk.kind();
        assert!(
            matches!(self.kind, _hunk_kind),
            "Only hunks with the same kind are allowed!"
        );

        self.data.push(hunk);
    }

    pub fn add_patch_line(&mut self, patch_line: PatchLine<'a>) {
        let _patch_line_kind = patch_line.kind();

        assert!(
            matches!(self.kind, _patch_line_kind),
            "Adding PatchLine with different kind to Hunks is not allowed!"
        );

        assert!(
            !self.has_no_hunks(),
            "Can not add patch_line to an empty Hunks."
        );

        if let Some(last_hunk) = self.data.last_mut() {
            last_hunk.add_patch_line(patch_line);
        }
    }

    pub fn modify_hunks(&mut self, operator: fn(&mut Vec<Hunk>)) {
        operator(&mut self.data);
    }

    pub fn hunks(&self) -> &Vec<Hunk<'a>> {
        &self.data
    }

    pub fn hunks_mut(&mut self) -> &mut Vec<Hunk<'a>> {
        &mut self.data
    }

    fn apply_normal(&mut self) -> PatchResult<()> {
        self.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| hunk.normal_hunk_data().range_right().start())
        });

        let output: &mut BufWriter<File> = self.output.as_mut().unwrap();
        let file: &PatchFile = self.file.as_ref().unwrap();

        let mut _new_file_line = 1usize;
        let mut old_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        for hunk in self.data.iter() {
            let range_left = hunk.normal_hunk_data().range_left();
            let _range_right = hunk.normal_hunk_data().range_right();

            if range_left.start() > old_file_line {
                let start = old_file_line;

                for j in start..range_left.start() {
                    writeln!(output, "{}", file.lines()[j - 1])?;
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
                                    writeln!(output, "{}", file.lines()[old_file_line - 1])?;
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
                    PatchLine::NormalLineInsert(_) => {
                        writeln!(output, "{}", patch_line.original_line())?;
                    }
                    PatchLine::NormalLineDelete(_) => {}
                    PatchLine::NoNewLine(_) => {
                        no_new_line_count += 1;
                    }
                    _ => panic!("Invalid Normal PatchLine detected!"),
                }
            }
        }

        match no_new_line_count {
            0 => writeln!(output , "")?,
            1 => {
                if matches!(file.kind(), FileKind::Original) && !file.ends_with_newline() {
                    writeln!(output, "")?;
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn apply_normal_reverse(&mut self) -> PatchResult<()> {
        self.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| hunk.normal_hunk_data().range_left().start())
        });

        let output: &mut BufWriter<File> = self.output.as_mut().unwrap();
        let file: &PatchFile = self.file.as_ref().unwrap();

        let mut new_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        for hunk in self.data.iter() {
            let range_right = hunk.normal_hunk_data().range_right();

            if range_right.start() > new_file_line {
                let start = new_file_line;

                for j in start..range_right.start() {
                    writeln!(output, "{}", file.lines()[j - 1])?;
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
                                    writeln!(output, "{}", file.lines()[new_file_line - 1])?;
                                    new_file_line += 1;
                                }
                            }
                        }
                    }
                    PatchLine::NormalChangeSeparator(_) => {}
                    PatchLine::NormalLineInsert(_) => {}
                    PatchLine::NormalLineDelete(_) => {
                        writeln!(output, "{}", patch_line.original_line())?;
                    }
                    PatchLine::NoNewLine(_) => no_new_line_count += 1,
                    _ => panic!("Invalid Normal PatchLine detected!"),
                }
            }
        }

        match no_new_line_count {
            0 => writeln!(output)?,
            1 => {
                if matches!(file.kind(), FileKind::Original) && !file.ends_with_newline() {
                    writeln!(output)?
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn apply_unified(&mut self) -> PatchResult<()> {
        self.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| hunk.unified_hunk_data().f1_range().start())
        });

        let output: &mut BufWriter<File> = self.output.as_mut().unwrap();
        let file: &PatchFile = self.file.as_ref().unwrap();

        let mut _new_file_line = 1usize;
        let mut old_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        for hunk in self.data.iter() {
            let f1_range = hunk.unified_hunk_data().f1_range();
            let _f2_range = hunk.unified_hunk_data().f2_range();

            if f1_range.start() > old_file_line {
                let start = old_file_line;
                for j in start..f1_range.start() {
                    writeln!(output , "{}", file.lines()[j - 1])?;
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
                        writeln!(output,  "{}", patch_line.original_line())?;
                        _new_file_line += 1;
                        old_file_line += 1;
                    }
                    PatchLine::UnifiedInserted(_) => {
                        writeln!(output, "{}", patch_line.original_line())?;
                        _new_file_line += 1;
                    }
                    PatchLine::NoNewLine(_) => no_new_line_count += 1,
                    _ => panic!("Invalid Unified PatchLine detected!"),
                }
            }
        }

        match no_new_line_count {
            0 => writeln!(output)?,
            1 => {
                if matches!(file.kind(), FileKind::Original) && !file.ends_with_newline() {
                    writeln!(output)?;
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn apply_unified_reverse(&mut self) -> PatchResult<()> {
        self.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| hunk.unified_hunk_data().f2_range().end())
        });

        let output: &mut BufWriter<File> = self.output.as_mut().unwrap();
        let file: &PatchFile = self.file.as_ref().unwrap();

        let mut new_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        let hunks = &self.data;

        for hunk in hunks.iter() {
            let f2_range = hunk.unified_hunk_data().f2_range();

            if f2_range.start() > new_file_line {
                let start = new_file_line;
                for j in start..f2_range.start() {
                    writeln!(output, "{}", file.lines()[j - 1])?;
                    new_file_line += 1;
                }
            }

            for patch_line in hunk.unified_hunk_data().lines() {
                match patch_line {
                    PatchLine::UnifiedHunkHeader(_) => {}
                    PatchLine::UnifiedDeleted(_) => {
                        writeln!(output, "{}", patch_line.original_line())?;
                    }
                    PatchLine::UnifiedUnchanged(_) => {
                        writeln!(output, "{}", patch_line.original_line())?;
                        new_file_line += 1;
                    }
                    PatchLine::UnifiedInserted(_) => {
                        new_file_line += 1;
                    }
                    PatchLine::NoNewLine(_) => no_new_line_count += 1,
                    _ => panic!("Invalid Unified PatchLine detected!"),
                }
            }
        }

        match no_new_line_count {
            0 => writeln!(output, "")?,
            1 => {
                if matches!(file.kind(), FileKind::Original) && !file.ends_with_newline() {
                    writeln!(output, "")?;
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn apply_context(&mut self) -> PatchResult<()> {
        self.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| {
                hunk.context_hunk_data()
                    .f1_range()
                    .expect("Invalid f1_range for ContextHunkData!")
                    .start()
            })
        });

        let output: &mut BufWriter<File> = self.output.as_mut().unwrap();
        let file: &PatchFile = self.file.as_ref().unwrap();

        let mut original_file_line = 1usize;
        let mut change_index = 0usize;
        let mut no_newline_count = 0usize;

        for hunk in self.data.iter_mut() {
            let range1 = hunk
                .context_hunk_data()
                .f1_range()
                .expect("ContextRange is expected not to be None here!");
            let range2 = hunk
                .context_hunk_data()
                .f2_range()
                .expect("ContextRange is expected not to be None here!");

            if range1.start() > original_file_line {
                let start = original_file_line;
                for _ in start..range1.start() {
                    writeln!(output , "{}", file.lines()[original_file_line - 1])?;
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
                        writeln!(output,  "{}", patch_line.original_line())?;

                        if *is_change {
                            original_file_line += 1;
                        }
                    }
                    PatchLine::ContextDeleted(_, is_change) => {
                        if *is_change {
                            writeln!(
                                output,
                                "{}",
                                hunk_data.change_by_index(change_index).original_line()
                            )?;
                            change_index += 1;
                        }

                        original_file_line += 1;
                    }
                    PatchLine::ContextUnchanged(_) => {
                        writeln!(output,  "{}", patch_line.original_line())?;
                        original_file_line += 1;
                    }
                    PatchLine::ContextHunkRange(_) => {}
                    PatchLine::ContextHunkSeparator(_) => {}
                    PatchLine::NoNewLine(_) => {
                        no_newline_count += 1;
                    }
                    _ => panic!("Invalid Context PatchLine detected!"),
                }
            }
        }

        match no_newline_count {
            0 => writeln!(output, "")?,
            1 => {
                if matches!(file.kind(), FileKind::Original) && !file.ends_with_newline() {
                    writeln!(output, "")?;
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn apply_context_reverse(&mut self) -> Result<(), PatchError> {
        self.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| {
                hunk.context_hunk_data()
                    .f1_range()
                    .expect("Invalid f1_range for ContextHunkData!")
                    .start()
            })
        });

        let output: &mut BufWriter<File> = self.output.as_mut().unwrap();
        let file: &PatchFile = self.file.as_ref().unwrap();

        let mut new_file_line = 1usize;
        let mut no_newline_count = 0usize;

        for hunk in self.data.iter_mut() {
            let f2_range = hunk
                .context_hunk_data()
                .f2_range()
                .expect("ContextRange is expected not to be None here!");
            let f1_range = hunk
                .context_hunk_data()
                .f1_range()
                .expect("ContextRange is expected not to be None here!");

            if f2_range.start() > new_file_line {
                let start = new_file_line;
                for _ in start..f2_range.start() {
                    writeln!(output, "{}", file.lines()[new_file_line - 1])?;
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
                        writeln!(output, "{}", patch_line.original_line())?;

                        if *is_change {
                            new_file_line += 1;
                        }
                    }
                    PatchLine::ContextUnchanged(_) => {
                        writeln!(output, "{}", patch_line.original_line())?;
                        new_file_line += 1;
                    }
                    PatchLine::ContextHunkRange(_) => {}
                    PatchLine::ContextHunkSeparator(_) => {}
                    PatchLine::NoNewLine(_) => {
                        no_newline_count += 1;
                    }
                    _ => panic!("Invalid Context PatchLine detected!"),
                }
            }
        }

        match no_newline_count {
            0 => writeln!(output, "")?,
            1 => {
                if matches!(file.kind(), FileKind::Modified) && !file.ends_with_newline() {
                    writeln!(output, "")?;
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn apply_edit_script(&mut self) -> Result<(), PatchError> {
        self.modify_hunks(|hunks_mut| {
            hunks_mut.sort_by_key(|hunk| hunk.edit_script_hunk_data().range().end())
        });

        let output: &mut BufWriter<File> = self.output.as_mut().unwrap();
        let file: &PatchFile = self.file.as_ref().unwrap();

        let mut _new_file_line = 1usize;
        let mut old_file_line = 1usize;
        let mut no_new_line_count = 0usize;

        for hunk in self.data.iter() {
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
                    writeln!(output, "{}", file.lines()[old_file_line - 1])?;
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
                        writeln!(output, "{}", data.line())?;
                        _new_file_line += 1;
                    }
                    PatchLine::EditScriptChange(data) => {
                        writeln!(output, "{}", data.line())?;
                        _new_file_line += 1;
                    }
                    PatchLine::NoNewLine(_) => no_new_line_count += 1,
                    _ => panic!("Invalid PatchLine detected in EditScriptHunkData"),
                }
            }
        }

        if file.lines().len() > old_file_line {
            while old_file_line <= file.lines().len() {
                writeln!(output, "{}", file.lines()[old_file_line - 1])?;
                old_file_line += 1;
                _new_file_line += 1;
            }
        }

        match no_new_line_count {
            0 => writeln!(output)?,
            1 => {
                if matches!(file.kind(), FileKind::Original) && !file.ends_with_newline() {
                    writeln!(output)?;
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn prepare_normal_to_apply(&mut self) -> PatchResult<()> {
        let output_file = if let Some(output_file) = &self.options.output_patch {
            output_file
        } else if let Some(output_file) = &self.options.file_path {
            output_file   
        } else {
            return Err(PatchError::Error("Could not specify output file!"))
        };
        // determin output file

        // determin reject file
        // // it is reject_file or output + .rej
        // let output_file = if let Some(output_file) = self.options.output_patch {
        //     output_file
        // } else if let Some() {

        // }

        // if self.options.backup {

        // }

        todo!()
    }

    fn prepare_to_apply(&mut self) -> PatchResult<()> {
        // let (output , time) = if matches!(self.kind , PatchFormat::Unified) || matches!(self.kind , PatchFormat::Context) {

        // }

        match self.kind {
            PatchFormat::None => panic!("Preparing Hunks with unknown kind, is not possible!"),
            PatchFormat::Normal => self.prepare_normal_to_apply(),
            PatchFormat::Unified => todo!(),
            PatchFormat::Context => todo!(),
            PatchFormat::EditScript => todo!(),
        }
    }
}

impl Apply for Hunks<'_> {
    fn apply(&mut self) -> PatchResult<()> {
        self.prepare_to_apply()?;

        match (self.kind, self.options.reversed) {
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
                Ok(())
            }
            #[allow(unreachable_patterns)]
            _ => panic!("Unhandled patch format!"),
        }
    }
}
