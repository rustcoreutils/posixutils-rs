use super::{
    edit_script_range_data::EditScriptHunkKind, functions::if_else, patch_line::PatchLine,
    range::Range,
};

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

    pub(crate) fn verify_hunk(&self) {
        // TODO
    }

    pub(crate) fn verify_file(&self, file: &super::patch_file::PatchFile) -> Result<(), ()> {
        if_else(file.lines().len() >= self.range.end(), Ok(()), Err(()))
    }
}
