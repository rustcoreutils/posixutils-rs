use super::patch_format::PatchFormat;

// Check https://man7.org/linux/man-pages/man1/patch.1.html
// and https://www.gnu.org/software/diffutils/manual/html_node/patch-Options.html
// and https://pubs.opengroup.org/onlinepubs/009695399/utilities/patch.html
// for options

#[derive(Debug)]
pub struct PatchOptions {
    // -b | --backup
    backup: bool,
    // -R
    reverse: bool,
    patch_format: Option<PatchFormat>,
}

impl PatchOptions {
    pub fn new(backup: bool, reverse: bool, patch_format: Option<PatchFormat>) -> Self {
        Self {
            backup,
            reverse,
            patch_format,
        }
    }

    pub fn patch_format(&self) -> &Option<PatchFormat> {
        &self.patch_format
    }

    pub fn reverse(&self) -> bool {
        self.reverse
    }

    pub fn update_patch_format(&mut self, patch_format: PatchFormat) {
        self.patch_format = Some(patch_format)
    }

    pub fn backup(&self) -> bool {
        self.backup
    }
}
