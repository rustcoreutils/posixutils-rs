use std::path::PathBuf;

use super::patch_format::PatchFormat;

#[derive(Debug)]
pub struct PatchOptions {
    /// file to apply patch to, empty for copied context and unified formats
    pub file_path: Option<PathBuf>,
    /// output patched files to OUTPUT_FILE
    pub output_patch: Option<PathBuf>,
    pub patch_path: PathBuf,
    /// backup original content of each file
    pub backup: bool,
    pub reversed: bool,
    pub patch_format: PatchFormat,
    pub strip: Option<usize>,
    pub reject_file: Option<PathBuf>,
}
