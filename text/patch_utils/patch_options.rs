use std::path::PathBuf;

use super::patch_format::PatchFormat;

#[derive(Debug)]
pub struct PatchOptions {
    /// file to apply patch to, empty for copied context and unified formats
    pub file: Option<PathBuf>,
    /// output patched files to OUTPUT_FILE
    pub output_file: Option<PathBuf>,
    pub patch_file: PathBuf,
    /// backup original content of each file
    pub backup: bool,
    pub reverse: bool,
    pub patch_format: PatchFormat,
    pub strip: Option<usize>,
    pub reject_file: Option<PathBuf>,
}
