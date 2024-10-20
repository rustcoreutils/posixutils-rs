// TODO: Handle no new lines
// TODO: detects loops!
// TODO: handle new ed PatchLine variants
// TODO: Check if patch is in ED or NORMAL format, if yes, output file should be Some(path)
// TODO: Determine patch kind and read to end of patch, do this again until all patches are extracted and pushed to patch list
// TODO: if number fo patches is more than 1, then they should be in either Context copied format or unified format, otherwise it is not possible to handle it
// TODO: If patch file is in context or unified format and file/output-file is not Some(path), extract path of original and modified files, and extract modification-date,
// TODO: Apply patch and if there is modification-date, apply it to the destination file.
// TODO: TaDa, done!
// TODO: Write tests!
//////////////////////////////////////////////////////////////////////////
// TODO : if hunks validation failed, write reject files.
// TODO : update modified_data of new file according to the patch
mod patch_utils;

extern crate clap;

use std::{
    io::{self},
    path::PathBuf,
};

use clap::Parser;
use patch_utils::{
    constants::init,
    functions::{context_unified_date_convert, file_exists, if_else, print_error},
    hunks::Apply,
    patch_file::PatchFile,
    patch_file_kind::FileKind,
    patch_format::PatchFormat,
    patch_options::PatchOptions,
    patch_units::PatchUnits,
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
    dir: Option<PathBuf>,

    /// make merged if-then-else output using NAME
    #[arg(short = 'D', long = "ifdef")]
    ifdef: Option<String>,

    /// strip NUM leading components from file names.
    #[arg(short = 'p', long = "strip")]
    num: Option<usize>,

    /// output rejects to REJECT_FILE
    #[arg(short = 'r', long = "reject-file")]
    reject_file: Option<PathBuf>,

    /// output patched files to OUTPUT_FILE
    #[arg(short = 'o', long = "output")]
    output_file: Option<PathBuf>,

    /// file to apply patch to, empty for copied context and unified formats
    #[arg(short, long)]
    file: Option<PathBuf>,

    /// read file from PATCHFILE instead of stdin
    #[arg(short = 'i', long = "input")]
    patchfile: PathBuf,
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

impl From<Args> for PatchOptions {
    fn from(value: Args) -> Self {
        let patch_format_options = [
            (value.format.ed, PatchFormat::EditScript),
            (value.format.normal, PatchFormat::Normal),
            (value.format.context, PatchFormat::Context),
            (value.format.unified, PatchFormat::Unified),
            (true, PatchFormat::None), // to ensure next unwrap works
        ];

        let patch_format = patch_format_options
            .iter()
            .filter(|option| option.0)
            .map(|option| option.1)
            .nth(0)
            .unwrap();

        PatchOptions {
            backup: value.backup,
            reverse: value.reverse,
            file: value.file,
            output_file: value.output_file,
            strip: value.num,
            patch_format,
            patch_file: value.patchfile,
            reject_file: value.reject_file,
        }
    }
}

impl Args {
    fn verify_patchfile_exists(self) -> io::Result<Self> {
        let if_true: fn(&PathBuf) = |_| {};

        let if_false: fn(&PathBuf) = |file_name: &PathBuf| {
            print_error(format!(
                " **** Can't open patch file {} : No such file or directory",
                file_name.to_str().expect("Could not unwrap file_name")
            ));
        };

        let file_exists = file_exists(&self.patchfile);

        if_else(file_exists, if_true, if_false)(&self.patchfile);

        match file_exists {
            true => Ok(self),
            false => Err(io::Error::new(
                io::ErrorKind::NotFound,
                "Could not find patch file!",
            )),
        }
    }

    fn change_dir(self) -> io::Result<Self> {
        if let Some(path) = &self.dir {
            std::env::set_current_dir(path)?;
        }

        Ok(self)
    }

    fn try_verify_format_and_output_file(self) -> io::Result<Self> {
        if self.output_file.is_none() && self.file.is_none() {
            if self.format.ed || self.format.normal {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "patch: applying patch for ed and normal format without specifying output-file is not possible!"));
            }
        }

        Ok(self)
    }
}

fn main() {
    patch_operation().expect("Failed!");
}

fn patch_operation() -> io::Result<()> {
    init();

    let args = Args::parse();

    let args = args
        .change_dir()?
        .verify_patchfile_exists()?
        .try_verify_format_and_output_file()?;

    let patch_options: PatchOptions = args.into();
    let patch_file =
        PatchFile::load_file(PathBuf::from(&patch_options.patch_file), FileKind::Patch)?;

    let patch = PatchUnits::try_build(&patch_options, &patch_file)?;

    if let Some(patch_units) = patch {
        let into_hunks = patch_units.into_hunks();

        match into_hunks {
            Ok(mut into_hunks) => {
                for hunks in into_hunks.hunks.iter_mut() {
                    let result = hunks.apply();
                    result.expect("Failed to apply hunk!");
                }

                for _failures in into_hunks.failures.iter_mut() {
                    // let reject_path = 
                }
            }
            Err(_) => todo!(),
        }
    }

    Ok(())
}
