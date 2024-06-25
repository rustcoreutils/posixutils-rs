mod patch_utils;

use std::{
    io::{self},
    path::PathBuf,
};

use patch_utils::{
    functions::if_else, patch::Patch, patch_file::PatchFile, patch_file_kind::PatchFileKind,
    patch_options::PatchOptions,
};

fn main() -> io::Result<()> {
    let file_path = PathBuf::from("f2.txt");
    let patch_path = PathBuf::from("patch_normal.diff");
    let patch_options = PatchOptions::new(true, true, None);

    let patch_file = PatchFile::load_file(patch_path, PatchFileKind::Patch)?;

    let file = PatchFile::load_file(
        file_path.clone(),
        if_else(
            patch_options.reverse(),
            PatchFileKind::Modified,
            PatchFileKind::Original,
        ),
    )?;

    if patch_options.backup() {
        let backup_file_path = file_path.with_file_name(format!(
            "{}.orig",
            file_path.file_name().unwrap().to_str().unwrap()
        ));

        file.copy_to(backup_file_path)?;
    }

    let patch = Patch::try_new(&patch_file, &file, patch_options);

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
