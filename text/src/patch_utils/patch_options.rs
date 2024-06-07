use super::patch_format::PatchFormat;

#[derive(Debug)]
pub struct PatchOptions {
    patch_format: Option<PatchFormat>,
}

impl PatchOptions {
    pub fn new(patch_format: Option<PatchFormat>) -> Self {
        Self { patch_format }
    }

    pub fn patch_format(&self) -> &Option<PatchFormat> {
        &self.patch_format
    }

    pub fn update_patch_format(&mut self, patch_format: PatchFormat) {
        self.patch_format = Some(patch_format)
    }
}
