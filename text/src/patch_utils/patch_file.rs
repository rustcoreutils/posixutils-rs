use std::{
    fs,
    io::{self},
    path::PathBuf,
};

use super::patch_file_kind::PatchFileKind;

#[derive(Debug)]
#[allow(dead_code)]
pub struct PatchFile {
    lines: Vec<String>,
    kind: PatchFileKind,
    path: PathBuf,
    ends_with_newline: bool,
}

impl PatchFile {
    pub fn load_file(path: PathBuf, kind: PatchFileKind) -> io::Result<Self> {
        let content = fs::read_to_string(&path)?;
        let ends_with_newline = content.ends_with('\n');

        if matches!(kind, PatchFileKind::Patch) && content.is_empty() {
            std::process::exit(0);
        }

        let lines = content
            .lines()
            .map(|line| line.to_string())
            .collect::<Vec<String>>();

        let lines = if kind.is_patch() {
            let min_leading_whitespaces_count =
                lines.iter().map(|line| count_left_whitespaces(line)).min();

            if min_leading_whitespaces_count.is_some() && min_leading_whitespaces_count.unwrap() > 0
            {
                let val = min_leading_whitespaces_count.unwrap();

                lines
                    .iter()
                    .map(|line| String::from(&line[val..]))
                    .collect::<Vec<String>>()
            } else {
                lines
            }
        } else {
            lines
        };

        Ok(Self {
            lines,
            kind,
            path,
            ends_with_newline,
        })
    }

    pub fn lines(&self) -> &Vec<String> {
        &self.lines
    }

    pub fn kind(&self) -> PatchFileKind {
        self.kind
    }

    pub fn ends_with_newline(&self) -> bool {
        self.ends_with_newline
    }

    pub(crate) fn copy_to(&self, path: PathBuf) -> io::Result<()> {
        if let Err(error) = fs::copy(&self.path, path) {
            Err(error)
        } else {
            Ok(())
        }
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }
}

pub fn count_left_whitespaces(input: &str) -> usize {
    input
        .chars()
        .take_while(|ch| ch.is_whitespace() && *ch != '\n')
        .map(|ch| ch.len_utf8())
        .sum()
}
