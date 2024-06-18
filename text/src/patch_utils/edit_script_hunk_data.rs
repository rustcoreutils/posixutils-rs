use super::{edit_script_range_data::EditScriptHunkKind, patch_line::PatchLine, range::Range};

#[derive(Debug, Clone)]
pub struct EditScriptHunkData<'a> {
    lines: Vec<PatchLine<'a>>,
    range: Range,
    kind: EditScriptHunkKind,
}

impl<'a> EditScriptHunkData<'a> {
    pub fn new(lines: Vec<PatchLine<'a>>, range: Range, kind: EditScriptHunkKind) -> Self {
        Self { lines, range, kind }
    }

    pub fn add_patch_line(&mut self, patch_line: PatchLine<'a>) {
        self.lines.push(patch_line);
    }

    pub fn kind(&self) -> EditScriptHunkKind {
        self.kind
    }

    pub fn range(&self) -> Range {
        self.range
    }

    pub fn lines(&self) -> &Vec<PatchLine<'a>> {
        &self.lines
    }
}
