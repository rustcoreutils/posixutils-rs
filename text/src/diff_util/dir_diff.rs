use std::{collections::HashSet, ffi::OsString, io, path::PathBuf};

use crate::diff_util::{constants::COULD_NOT_UNWRAP_FILENAME, file_diff::FileDiff};

use super::{common::FormatOptions, dir_data::DirData};

pub struct DirDiff<'a> {
    dir1: &'a mut DirData,
    dir2: &'a mut DirData,
    format_options: &'a FormatOptions,
    recursive: bool,
}

impl<'a> DirDiff<'a> {
    fn new(
        dir1: &'a mut DirData,
        dir2: &'a mut DirData,
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
        path1: PathBuf,
        path2: PathBuf,
        format_options: &FormatOptions,
        recursive: bool,
    ) -> io::Result<()> {
        let mut dir1: DirData = DirData::load(PathBuf::from(path1))?;
        let mut dir2: DirData = DirData::load(PathBuf::from(path2))?;

        let mut dir_diff = DirDiff::new(&mut dir1, &mut dir2, &format_options, recursive);
        dir_diff.analyze()?;

        Ok(())
    }

    fn analyze(&mut self) -> io::Result<()> {
        fn is_file(file_name: &OsString, dir_data: &DirData) -> io::Result<bool> {
            let is_file = dir_data
                .files()
                .get_key_value(file_name)
                .expect(
                    format!(
                        "Could not find file in {}",
                        dir_data
                            .path()
                            .to_str()
                            .unwrap_or(COULD_NOT_UNWRAP_FILENAME)
                    )
                    .as_str(),
                )
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
                        // TODO: output format should be included

                        println!(
                            "diff \"{}\" \"{}\"",
                            path1.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME),
                            path2.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
                        );

                        FileDiff::file_diff(path1, path2, self.format_options)?;
                    } else if !in_dir1_is_file && !in_dir2_is_file {
                        if self.recursive {
                            Self::dir_diff(
                                self.dir1.path().join(file_name),
                                self.dir2.path().join(file_name),
                                self.format_options,
                                self.recursive,
                            )?;
                        } else {
                            println!(
                                "Common subdirectories: \"{}\" and \"{}\"",
                                self.dir1
                                    .path()
                                    .join(file_name)
                                    .to_str()
                                    .unwrap_or(COULD_NOT_UNWRAP_FILENAME),
                                self.dir2
                                    .path()
                                    .join(file_name)
                                    .to_str()
                                    .unwrap_or(COULD_NOT_UNWRAP_FILENAME)
                            );
                        }
                    } else {
                        let (file, dir) = if in_dir1_is_file && !in_dir2_is_file {
                            (
                                path1.to_str().unwrap_or(&COULD_NOT_UNWRAP_FILENAME),
                                path2.to_str().unwrap_or(&COULD_NOT_UNWRAP_FILENAME),
                            )
                        } else {
                            (
                                path2.to_str().unwrap_or(&COULD_NOT_UNWRAP_FILENAME),
                                path1.to_str().unwrap_or(&COULD_NOT_UNWRAP_FILENAME),
                            )
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
                (false, false) => panic!(
                    "At least one of directories should contain file \"{}\"",
                    file_name.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
                ),
            }
        }

        Ok(())
    }
}
