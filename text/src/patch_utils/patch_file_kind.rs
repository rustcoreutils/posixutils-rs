#[derive(Clone, Copy, Debug)]
pub enum FileKind {
    Original,
    Modified,
    Patch,
}

impl FileKind {
    pub fn is_patch(&self) -> bool {
        matches!(self, Self::Patch)
    }
}