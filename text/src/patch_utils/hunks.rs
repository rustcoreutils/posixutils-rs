use std::{fs::File, io::BufWriter, io::Write};

use super::{
    functions::print_error, hunk::Hunk, patch_error::PatchResult, patch_file::PatchFile,
    patch_file_kind::FileKind, patch_format::PatchFormat, patch_line::PatchLine,
    patch_options::PatchOptions,
};

pub trait Apply {
    fn apply(&mut self, reversed: bool) -> PatchResult<()>;
}

#[derive(Debug)]
pub struct Hunks<'a> {
    kind: PatchFormat,
    data: Vec<Hunk<'a>>,
    output: Option<BufWriter<File>>,
    file: Option<PatchFile>,
    patch_f1_header: Option<&'a str>,
    patch_f2_header: Option<&'a str>,
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
            patch_f1_header: None,
            patch_f2_header: None,
            options,
        }
    }

    pub fn set_f1_header(&mut self, header: &'a str) {
        self.patch_f1_header = Some(header)
    }

    pub fn set_f2_header(&mut self, header: &'a str) {
        self.patch_f2_header = Some(header)
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
                    _ => panic!("Only PatchLines that start with Normal are allowed!"),
                }
            }
        }

        match no_new_line_count {
            0 => println!(),
            1 => {
                if matches!(file.kind(), FileKind::Original) && !file.ends_with_newline() {
                    writeln!(output, "")?;
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn prepare_to_apply(&mut self) -> PatchResult<()> {
        todo!()
    }
}

impl Apply for Hunks<'_> {
    fn apply(&mut self, reversed: bool) -> PatchResult<()> {
        self.prepare_to_apply()?;
        match (self.kind, reversed) {
            (PatchFormat::None, _) => panic!("PatchFormat should be valid!"),
            (PatchFormat::Normal, false) => self.apply_normal(),
            // (PatchFormat::Normal, true) => self.apply_normal_reverse(),
            // (PatchFormat::Unified, false) => self.apply_unified(),
            // (PatchFormat::Unified, true) => self.apply_unified_reverse(),
            // (PatchFormat::Context, false) => self.apply_context(),
            // (PatchFormat::Context, true) => self.apply_context_reverse(),
            // (PatchFormat::EditScript, false) => self.apply_edit_script(),
            (PatchFormat::EditScript, true) => {
                print_error("ed format + reverse option is not possible!");
                std::process::exit(0);
            }
            _ => todo!(),
        }
    }
}
