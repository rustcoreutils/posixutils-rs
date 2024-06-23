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
    let patch_file =
        PatchFile::load_file(PathBuf::from("patch_context.diff"), PatchFileKind::Patch)?;
    let file = PatchFile::load_file(PathBuf::from("f2.txt"), PatchFileKind::Modified)?;
    let patch = Patch::try_new(&patch_file, &file, PatchOptions::new(true, true, None));

    if let Ok(mut patch) = patch {
        let result = patch.apply();
        if let Err(error) = result {
            eprintln!("patch: {}", error);
        }
    } else if let Err(error) = patch {
        eprintln!("patch: {}", error);
    }

    Ok(())
}

// TODO : if hunks validation failed, write reject files.
// TODO : update modified_data of new file according to the patch
