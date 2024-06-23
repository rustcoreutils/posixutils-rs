use crate::patch_utils::patch_format::PatchFormat;

use super::{patch_line::PatchLine, range::Range};

#[derive(Debug)]
pub struct UnifiedHunkData<'a> {
    f1_range: Range,
    f2_range: Range,
    lines: Vec<PatchLine<'a>>,
}

impl<'a> UnifiedHunkData<'a> {
    pub fn new(f1_range: Range, f2_range: Range, lines: Vec<PatchLine<'a>>) -> Self {
        Self {
            f1_range,
            f2_range,
            lines,
        }
    }

    pub fn lines(&self) -> &Vec<PatchLine<'a>> {
        &self.lines
    }

    pub fn add_patch_line(&mut self, patch_line: PatchLine<'a>) {
        let patch_line_kind = patch_line.kind();
        assert!(
            matches!(patch_line_kind, PatchFormat::Unified),
            "Only Unified patch lines are allowed in UnifiedHunkData!"
        );

        self.lines.push(patch_line);
    }

    pub fn f1_range(&self) -> &Range {
        &self.f1_range
    }

    pub fn f2_range(&self) -> &Range {
        &self.f2_range
    }

    pub(crate) fn verify_hunk(&self) {
        let mut file1_line_diff = usize::max(1, self.f1_range.end() - self.f1_range.start()) as i64;
        let mut file2_line_diff = usize::max(1, self.f2_range.end() - self.f2_range.start()) as i64;

        for patch_line in self.lines.iter() {
            match patch_line {
                PatchLine::UnifiedHunkHeader(_) => {}
                PatchLine::UnifiedUnchanged(_) => {
                    file1_line_diff -= 1;
                    file2_line_diff -= 1;
                }
                PatchLine::UnifiedInserted(_) => file2_line_diff -= 1,
                PatchLine::UnifiedDeleted(_) => file1_line_diff -= 1,
                PatchLine::NoNewLine(_) => {}
                _ => panic!("Invalid PatchLine detected in unified hunk!"),
            }
        }

        if file1_line_diff != 0 || file2_line_diff != 0 {
            panic!("Invalid unified hunk detected: at least one of {file1_line_diff} or {file2_line_diff} is not 0!")
        }
    }
}
