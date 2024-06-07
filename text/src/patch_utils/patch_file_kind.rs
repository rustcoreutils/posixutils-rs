#[derive(Clone, Copy, Debug)]
pub enum PatchFileKind {
    Original,
    Modified,
    Patch,
}

impl PartialEq<PatchFileKind> for PatchFileKind {
    fn eq(&self, other: &PatchFileKind) -> bool {
        core::mem::discriminant(self) == core::mem::discriminant(other)
    }
}

impl PatchFileKind {
    pub fn is_original(&self) -> bool {
        *self == Self::Original
    }

    pub fn is_modified(&self) -> bool {
        *self == Self::Modified
    }

    pub fn is_patch(&self) -> bool {
        *self == Self::Patch
    }
}
