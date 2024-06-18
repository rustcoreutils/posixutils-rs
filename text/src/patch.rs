#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(dead_code)]

mod patch_utils;

use std::{
    io::{self},
    path::PathBuf,
};

use patch_utils::{
    constants::regex::NORMAL_FORMAT_RANGE_INSERT_REGEX, patch::Patch, patch_file::PatchFile,
    patch_file_kind::PatchFileKind, patch_options::PatchOptions,
};

fn main() -> io::Result<()> {
    let patch_file =
        PatchFile::load_file(PathBuf::from("patch_normal.diff"), PatchFileKind::Patch)?;
    let file = PatchFile::load_file(PathBuf::from("f1.txt"), PatchFileKind::Original)?;
    let patch = Patch::try_new(&patch_file, &file, PatchOptions::new(None));

    if let Ok(mut patch) = patch {
        let result = patch.apply();

        if let Ok(()) = result {
        } else if let Err(error) = result {
        }
    } else if let Err(error) = patch {
        println!("{:?}", error);
    }

    Ok(())
}
