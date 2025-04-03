use std::{collections::HashSet, ffi::OsString, io, path::Path};

use crate::diff_util::{
    constants::COULD_NOT_UNWRAP_FILENAME, diff_exit_status::DiffExitStatus, file_diff::FileDiff,
};

use super::{common::FormatOptions, dir_data::DirData};

pub struct DirDiff<'a> {
    dir1: &'a mut DirData<'a>,
    dir2: &'a mut DirData<'a>,
    format_options: &'a FormatOptions,
    recursive: bool,
}

impl<'a> DirDiff<'a> {
    fn new(
        dir1: &'a mut DirData<'a>,
        dir2: &'a mut DirData<'a>,
        format_options: &'a FormatOptions,
        recursive: bool,
    ) -> Self {
        Self {
            dir1,
            dir2,
            format_options,
            recursive,
        }
    }

    pub fn dir_diff(
        path1: &Path,
        path2: &Path,
        format_options: &FormatOptions,
        recursive: bool,
    ) -> io::Result<DiffExitStatus> {
        let mut dir1: DirData = DirData::load(path1)?;
        let mut dir2: DirData = DirData::load(path2)?;

        let mut dir_diff = DirDiff::new(&mut dir1, &mut dir2, format_options, recursive);
        dir_diff.analyze()
    }

    fn analyze(&mut self) -> io::Result<DiffExitStatus> {
        let mut exit_status = DiffExitStatus::NotDifferent;

        fn is_file(file_name: &OsString, dir_data: &DirData) -> io::Result<bool> {
            let is_file = dir_data
                .files()
                .get_key_value(file_name)
                .unwrap_or_else(|| panic!("Could not find file in {}", dir_data.path().display()))
                .1
                .file_type()?
                .is_file();

            Ok(is_file)
        }

        let mut dir1_files_name = self.dir1.files().keys().collect::<Vec<&OsString>>();
        let mut dir2_files_name = self.dir2.files().keys().collect::<Vec<&OsString>>();
        dir1_files_name.append(&mut dir2_files_name);

        let mut unique_files_name = HashSet::<&OsString>::from_iter(dir1_files_name)
            .iter()
            .cloned()
            .collect::<Vec<&OsString>>();
        unique_files_name.sort();

        for file_name in unique_files_name {
            let in_dir1 = self.dir1.files().contains_key(file_name);
            let in_dir2 = self.dir2.files().contains_key(file_name);

            match (in_dir1, in_dir2) {
                (true, true) => {
                    let in_dir1_is_file = is_file(file_name, self.dir1)?;
                    let in_dir2_is_file = is_file(file_name, self.dir2)?;

                    let path1 = self.dir1.path().join(file_name);
                    let path2 = self.dir2.path().join(file_name);

                    if in_dir1_is_file && in_dir2_is_file {
                        let mut show_if_different = String::from("diff ");

                        match self.format_options.output_format {
                            crate::diff_util::common::OutputFormat::Debug => {
                                show_if_different.push_str("--debug ")
                            }
                            crate::diff_util::common::OutputFormat::Default => {}
                            crate::diff_util::common::OutputFormat::Context(ctx) => {
                                show_if_different.push_str(format!("-C {} ", ctx).as_str())
                            }
                            crate::diff_util::common::OutputFormat::EditScript => {
                                show_if_different.push_str("-e ")
                            }
                            crate::diff_util::common::OutputFormat::ForwardEditScript => {
                                show_if_different.push_str("-f ")
                            }
                            crate::diff_util::common::OutputFormat::Unified(ufd) => {
                                show_if_different.push_str(format!("-U {} ", ufd).as_str())
                            }
                        }

                        if self.recursive {
                            show_if_different.push_str("-r ");
                        }

                        if self.format_options.ignore_trailing_white_spaces {
                            show_if_different.push_str("-b ");
                        }

                        if let Some(label1) = &self.format_options.label1() {
                            show_if_different.push_str(format!("--label {} ", label1).as_str())
                        }

                        if let Some(label2) = &self.format_options.label2() {
                            show_if_different.push_str(format!("--label2 {} ", label2).as_str())
                        }

                        if let Some(label1) = &self.format_options.label1() {
                            show_if_different.push_str(format!("{} ", label1).as_str())
                        } else {
                            show_if_different
                                .push_str(path1.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME));
                            show_if_different.push(' ');
                        }

                        if let Some(label2) = &self.format_options.label2() {
                            show_if_different.push_str(format!("{} ", label2).as_str())
                        } else {
                            show_if_different
                                .push_str(path2.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME));
                            show_if_different.push(' ');
                        }

                        let inner_exit_status = FileDiff::file_diff(
                            path1.as_path(),
                            path2.as_path(),
                            self.format_options,
                            Some(show_if_different),
                        )?;

                        if exit_status.status_code() < inner_exit_status.status_code() {
                            exit_status = inner_exit_status;
                        }
                    } else if !in_dir1_is_file && !in_dir2_is_file {
                        if self.recursive {
                            Self::dir_diff(
                                self.dir1.path().join(file_name).as_path(),
                                self.dir2.path().join(file_name).as_path(),
                                self.format_options,
                                self.recursive,
                            )?;
                        } else {
                            println!(
                                "Common subdirectories: \"{}\" and \"{}\"",
                                self.dir1.path().join(file_name).display(),
                                self.dir2.path().join(file_name).display()
                            );
                        }
                    } else {
                        let (file, dir) = if in_dir1_is_file && !in_dir2_is_file {
                            (path1.display(), path2.display())
                        } else {
                            (path2.display(), path1.display())
                        };

                        println!(
                            "File \"{}\" is a directory while file \"{}\" is a regular file",
                            dir, file
                        );
                    }
                }
                (true, false) => {
                    println!(
                        "Only in {}: {}",
                        self.dir1.path_str(),
                        file_name.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
                    )
                }
                (false, true) => {
                    println!(
                        "Only in {}: {}",
                        self.dir2.path_str(),
                        file_name.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
                    )
                }
                (false, false) => {
                    eprintln!(
                        "At least one of directories should contain file \"{}\"",
                        file_name.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
                    );
                    return Ok(DiffExitStatus::Trouble);
                }
            }
        }

        Ok(exit_status)
    }
}
