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
#![allow(dead_code)]
mod patch_utils;

extern crate clap;

use std::io::{self};

use clap::Parser;
use patch_utils::{
    constants::{
        context::{context_regex_cache, initialize_context_regex_cache, ContextRegexKind},
        ed::{ed_regex_cache, initialize_ed_regex_cache, EdRegexKind},
        normal::{initialize_normal_regex_cache, normal_regex_cache, NormalRegexKind},
        unified::{initialize_unified_regex_cache, unified_regex_cache, UnifiedRegexKind},
    },
    functions::{file_exists, if_else, print_error},
    patch_builder::PatchBuilder,
    patch_format::PatchFormat,
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

    /// file to apply patch to, empty for copied context and unified formats
    #[arg(short, long)]
    file: Option<String>,

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
        let patch_format_options = [
            (self.format.ed, PatchFormat::EditScript),
            (self.format.normal, PatchFormat::Normal),
            (self.format.context, PatchFormat::Context),
            (self.format.unified, PatchFormat::Unified),
            (true, PatchFormat::None), // to ensure next unwrap works
        ];

        let patch_format = patch_format_options
            .iter()
            .filter(|option| option.0)
            .map(|option| option.1)
            .nth(0)
            .unwrap();

        PatchOptions {
            backup: self.backup,
            reversed: self.reverse,
            file_path: self.file,
            output_patch: self.output_file,
            strip: self.num,
            patch_format,
            patch_path: self.patchfile,
        }
    }
}

impl Args {
    fn verify_patchfile_exists(self) -> io::Result<Self> {
        let if_true: fn(&str) = |_| {};

        let if_false: fn(&str) = |file_name: &str| {
            print_error(format!(
                " **** Can't open patch file {} : No such file or directory",
                file_name
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
    // test_regex();
    main_operation().expect("Failed!");
}

fn test_regex() {
    initialize_normal_regex_cache();
    initialize_ed_regex_cache();
    initialize_context_regex_cache();
    initialize_unified_regex_cache();

    let context_regex_cache = context_regex_cache();
    let ed_regex_cache = ed_regex_cache();
    let normal_regex_cache = normal_regex_cache();
    let unified_regex_cache = unified_regex_cache();

    assert!(unified_regex_cache[&UnifiedRegexKind::RangeHeader].is_match("@@ -3,15 +3,11 @@"));
    assert!(unified_regex_cache[&UnifiedRegexKind::FirstLine]
        .is_match("--- file1.txt	2024-06-25 16:48:31.840752300 +0300"));
    assert!(unified_regex_cache[&UnifiedRegexKind::SecondLine]
        .is_match("+++ file2.txt	2024-06-25 16:48:46.889122200 +0300"));
    assert!(unified_regex_cache[&UnifiedRegexKind::RemovedLine].is_match("-line 6 will change"));
    assert!(unified_regex_cache[&UnifiedRegexKind::InsertedLine].is_match("+line 6 is updated"));
    assert!(unified_regex_cache[&UnifiedRegexKind::UnchangedLine].is_match(" line 4"));

    assert!(normal_regex_cache[&NormalRegexKind::RangeInsert].is_match("11a11,13"));
    assert!(normal_regex_cache[&NormalRegexKind::RangeChange].is_match("4c2,3"));
    assert!(normal_regex_cache[&NormalRegexKind::RangeDelete].is_match("1,2d0"));
    assert!(normal_regex_cache[&NormalRegexKind::LineInsert]
        .is_match("> The named is the mother of all things."));
    assert!(normal_regex_cache[&NormalRegexKind::LineDelete]
        .is_match("< The Way that can be told of is not the eternal Way;"));
    assert!(normal_regex_cache[&NormalRegexKind::LineChangeSeparator].is_match("---"));

    assert!(context_regex_cache[&ContextRegexKind::HunkSeparator].is_match("***************"));
    assert!(context_regex_cache[&ContextRegexKind::OriginalRange].is_match("*** 3,17 ****"));
    assert!(context_regex_cache[&ContextRegexKind::ModifiedRange].is_match("--- 3,13 ----"));
    assert!(context_regex_cache[&ContextRegexKind::FirstLine]
        .is_match("*** file1.txt	2024-06-25 16:48:31.840752300 +0300"));
    assert!(context_regex_cache[&ContextRegexKind::SecondLine]
        .is_match("--- file2.txt	2024-06-25 16:48:46.889122200 +0300"));
    assert!(context_regex_cache[&ContextRegexKind::InsertedLine].is_match("+ line 6 will change"));
    assert!(context_regex_cache[&ContextRegexKind::DeletedLine].is_match("- line 6 will change"));
    assert!(context_regex_cache[&ContextRegexKind::ChangedLine].is_match("! line 6 is updated"));
    assert!(context_regex_cache[&ContextRegexKind::UnchangedLine].is_match("  line 4"));

    assert!(ed_regex_cache[&EdRegexKind::LineDot].is_match("."));
    assert!(ed_regex_cache[&EdRegexKind::RangeAdd].is_match("11a"));
    assert!(ed_regex_cache[&EdRegexKind::RangeChange].is_match("12,13c"));
    assert!(ed_regex_cache[&EdRegexKind::RangeDelete].is_match("9,10d"));
}

fn main_operation() -> io::Result<()> {
    initialize_normal_regex_cache();
    initialize_ed_regex_cache();
    initialize_context_regex_cache();
    initialize_unified_regex_cache();

    let args = Args::parse();

    let args = args
        .change_dir()?
        .verify_patchfile_exists()?
        .try_verify_format_and_output_file()?;

    let patch_options: PatchOptions = args.into();
    let _ = PatchBuilder::from_patch_options(patch_options);

    return Ok(());
    // let patch_path = PathBuf::from("patch_normal.diff");

    // let patch_file = PatchFile::load_file(patch_path, Some(PatchFileKind::Patch))?;

    // let file = PatchFile::load_file(
    //     patch_options.file_path.clone(),
    //     if_else(
    //         patch_options.reverse(),
    //         PatchFileKind::Modified,
    //         PatchFileKind::Original,
    //     ),
    // )?;

    // if patch_options.backup() {
    //     let backup_file_path = file_path.with_file_name(format!(
    //         "{}.orig",
    //         file_path.file_name().unwrap().to_str().unwrap()
    //     ));

    //     file.copy_to(backup_file_path)?;
    // }

    // let patch = Patch::try_new(&patch_file, &file, patch_options);

    // if let Ok(mut patch) = patch {
    //     let result = patch.apply();
    //     if let Err(error) = result {
    //         eprintln!("patch: {}", error);
    //     }
    // } else if let Err(error) = patch {
    //     eprintln!("patch: {}", error);
    // }

    // Ok(())
}
