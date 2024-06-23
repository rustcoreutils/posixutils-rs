#[derive(Clone, Copy, Debug)]
#[allow(dead_code)]
pub enum PatchFileKind {
    Original,
    Modified,
    Patch,
}

impl PatchFileKind {
    pub fn is_patch(&self) -> bool {
        matches!(self, Self::Patch)
    }
}
