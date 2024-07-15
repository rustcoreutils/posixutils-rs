use clap::Parser;

use super::patch_format::PatchFormat;

#[derive(Debug)]
pub struct PatchOptions {
    pub file_path: Option<String>,
    pub output_patch: Option<String>,
    pub patch_path: String,
    pub backup: bool,
    pub reversed: bool,
    pub patch_format: PatchFormat,
    pub strip: Option<usize>,
}
