mod patch_utils;

extern crate clap;
extern crate plib;

use std::{
    io::{self},
    path::PathBuf,
};

use clap::{command, Arg, Parser};
use patch_utils::{
    functions::{file_exists, if_else, print_error},
    patch::Patch,
    patch_file::PatchFile,
    patch_file_kind::PatchFileKind,
    patch_options::PatchOptions,
};

/// patch - convert original file to modified file and vice versa using result of diff
#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about)]
struct Args {
    /// backup original content of each file
    #[arg(short = 'b', long = "backup")]
    backup: bool,

    #[clap(flatten)]
    format: FormatGroup,

    /// assume patches were created with old file and new file swapped
    #[arg(short = 'R', long = "reverse")]
    reverse: bool,

    /// change the working directory to dir first
    #[arg(short = 'd', long = "dir")]
    dir: Option<String>,

    /// make merged if-then-else output using NAME
    #[arg(short = 'D', long = "ifdef")]
    ifdef: Option<String>,

    /// strip NUM leading components from file names.
    #[arg(short = 'p', long = "strip")]
    num: Option<usize>,

    /// output rejects to REJECT_FILE
    #[arg(short = 'r', long = "reject-file")]
    reject_file: Option<String>,

    /// output patched files to OUTPUT_FILE
    #[arg(short = 'o', long = "output")]
    output_file: Option<String>,

    /// output patched files to OUTPUT_FILE
    #[arg(short , long )]
    file: String,

    /// read file from PATCHFILE instead of stdin
    #[arg(short = 'i', long = "input")]
    patchfile: String,
}

#[derive(Debug, Clone, clap::Args)]
#[group(required = false, multiple = false)]
pub struct FormatGroup {
    /// interpret the patch file as a copied context difference
    #[clap(short, long)]
    context: bool,
    /// interpret the patch file as an ed script
    #[clap(short, long)]
    ed: bool,
    /// interpret the patch file as a normal script
    #[clap(short, long)]
    normal: bool,
    /// interpret the patch file as a unified script
    #[clap(short, long)]
    unified: bool,
}

impl Into<PatchOptions> for Args {
    fn into(self) -> PatchOptions {
        PatchOptions::new(self.backup, false, None)
    }
}

impl Args {
    fn verify_patchfile_exists(self) -> Self {
        let if_true: fn(&str) = |_| {};

        let if_false: fn(&str) = |file_name: &str| {
            print_error(format!(
                " **** Can't open patch file {} : No such file or directory",
                file_name
            ));
        };

        if_else(file_exists(&self.patchfile), if_true, if_false)(&self.patchfile);

        self
    }

    fn verify_file_exists(self) -> Self {
        let if_true: fn(&str) = |_| {};

        let if_false: fn(&str) = |file_name: &str| {
            print_error(format!(
                " **** Can't open file {} : No such file or directory",
                file_name
            ));
        };

        if_else(file_exists(&self.patchfile), if_true, if_false)(&self.file);

        self
    }
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    args.verify_patchfile_exists()
        .verify_file_exists();
    
    return Ok(());
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
