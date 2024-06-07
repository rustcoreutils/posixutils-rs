#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(dead_code)]

mod patch_utils;

use std::{
    io::{self},
    path::PathBuf,
};

use patch_utils::{
    patch::Patch, patch_file::PatchFile, patch_file_kind::PatchFileKind,
    patch_options::PatchOptions,
};

fn main() -> io::Result<()> {
    let f1_path = "f1.txt";
    let f2_path = "f2.txt";
    let patch_path = "patch.diff";
    let patch_context_path = "patch_context.diff";
    let empty_file = "empty.diff";

    let patch_file = PatchFile::load_file(PathBuf::from(patch_context_path), PatchFileKind::Patch)?;
    let file = PatchFile::load_file(PathBuf::from(f1_path), PatchFileKind::Original)?;
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
