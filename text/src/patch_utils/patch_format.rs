#[derive(Clone, Copy, Debug)]
pub enum PatchFormat {
    None,
    Normal,
    Unified,
    Context,
    EditScript,
}

impl PartialEq<PatchFormat> for PatchFormat {
    fn eq(&self, other: &PatchFormat) -> bool {
        core::mem::discriminant(self) == core::mem::discriminant(other)
    }
}
