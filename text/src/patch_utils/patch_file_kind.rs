#[derive(Clone, Copy, Debug)]
#[allow(dead_code)]
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

impl IsPatch for Option<FileKind> {
    fn is_patch(&self) -> bool {
        matches!(self, Some(FileKind::Patch))
    }
}

pub trait IsPatch {
    fn is_patch(&self) -> bool;
}
