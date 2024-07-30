use super::{
    functions::verify_patch_line, normal_range_data::NormalRangeKind, patch_line::PatchLine,
    range::Range,
};
use crate::patch_utils::patch_format::PatchFormat;

#[derive(Debug)]
pub struct NormalHunkData<'a> {
    range_left: Range,
    range_right: Range,
    lines: Vec<PatchLine<'a>>,
    kind: NormalRangeKind,
}

impl<'a> NormalHunkData<'a> {
    pub fn new(
        range_left: Range,
        range_right: Range,
        lines: Vec<PatchLine<'a>>,
        kind: NormalRangeKind,
    ) -> Self {
        Self {
            range_left,
            range_right,
            lines,
            kind,
        }
    }

    pub fn add_patch_line(&mut self, patch_line: PatchLine<'a>) {
        assert!(
            matches!(patch_line.kind(), PatchFormat::Normal),
            "Adding a patch line with different format to NormalHunkData is not allowed!"
        );

        self.lines.push(patch_line);
    }

    pub fn lines(&self) -> &Vec<PatchLine<'a>> {
        &self.lines
    }

    pub fn range_left(&self) -> Range {
        self.range_left
    }

    pub fn range_right(&self) -> Range {
        self.range_right
    }

    pub(crate) fn verify_hunk(&self) {
        // TODO
    }

    pub(crate) fn verify_file(
        &self,
        file: &super::patch_file::PatchFile,
        reversed: bool,
    ) -> Result<(), ()> {
        let mut original_file_line = usize::max(1, self.range_left.start());
        let mut modified_file_line = usize::max(1, self.range_right.start());

        match self.kind {
            NormalRangeKind::Insert => {
                for line in self
                    .lines()
                    .iter()
                    .filter(|&patch_line| matches!(patch_line, PatchLine::NormalLineInsert(_)))
                {
                    if reversed {
                        verify_patch_line(
                            line.original_line(),
                            &file.lines()[modified_file_line - 1],
                        )?;
                    }

                    modified_file_line += 1;
                }
            }
            NormalRangeKind::Change => {
                for line in self.lines() {
                    if matches!(line, PatchLine::NormalLineInsert(_)) && reversed {
                        verify_patch_line(
                            line.original_line(),
                            &file.lines()[modified_file_line - 1],
                        )?;
                        modified_file_line += 1;
                    }

                    if matches!(line, PatchLine::NormalLineDelete(_)) && !reversed {
                        verify_patch_line(
                            line.original_line(),
                            &file.lines()[original_file_line - 1],
                        )?;
                        original_file_line += 1;
                    }
                }
            }
            NormalRangeKind::Delete => {
                for line in self
                    .lines()
                    .iter()
                    .filter(|&patch_line| matches!(patch_line, PatchLine::NormalLineDelete(_)))
                {
                    if !reversed {
                        verify_patch_line(
                            line.original_line(),
                            &file.lines()[original_file_line - 1],
                        )?;
                    }

                    original_file_line += 1;
                }
            }
        }

        Ok(())
    }
}
