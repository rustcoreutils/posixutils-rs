use super::{normal_range_data::NormalRangeKind, patch_line::PatchLine, range::Range};
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
}
