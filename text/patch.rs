//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! patch - apply changes to files
//!
//! POSIX-compliant implementation of the patch utility.

mod patch_util;

use clap::Parser;
use gettextrs::gettext;
use patch_util::{
    applier::PatchApplier,
    bytes,
    file_ops::{
        delete_target, determine_target_file, file_lines, open_target, write_output, write_rejects,
        Target,
    },
    parser::parse_patch,
    safe_fs::{Name, Refusal},
    types::{BackupName, FilePatch, PatchConfig, PatchError, RejectFile},
};
use std::{
    collections::HashSet,
    env,
    fs::File,
    io::{self, Read},
    path::PathBuf,
    process::ExitCode,
};

/// patch - apply changes to files
#[derive(Parser, Debug)]
#[command(
    version,
    disable_version_flag = true,
    about = gettext("patch - apply changes to files"),
    after_help = gettext("The patch utility reads a source (patch) file containing difference listings and applies those differences to a file.")
)]
struct Args {
    /// Save original file with .orig suffix
    #[arg(short = 'b', help = gettext("Save a copy of the original file with .orig suffix"))]
    backup: bool,

    /// Backup file name prefix (GNU; used by dpkg-source)
    #[arg(short = 'B', allow_hyphen_values = true, value_name = "PREFIX", help = gettext("Prefix PREFIX to a file's name to name its backup; implies -b"))]
    backup_prefix: Option<String>,

    /// Backup file name suffix (GNU; used by dpkg-source)
    #[arg(short = 'z', allow_hyphen_values = true, value_name = "SUFFIX", help = gettext("Name backups with SUFFIX instead of .orig; implies -b"))]
    backup_suffix: Option<String>,

    /// Backup method (GNU; used by dpkg-source). Only simple backups exist.
    #[arg(short = 'V', allow_hyphen_values = true, value_name = "METHOD", value_parser = ["never", "simple"], help = gettext("Backup method: only 'never' or 'simple' (FILE.orig) is supported"))]
    _version_control: Option<String>,

    /// Remove files left empty (GNU; used by dpkg-source)
    #[arg(short = 'E', help = gettext("Remove output files that are empty after patching"))]
    remove_empty: bool,

    /// Maximum fuzz (GNU; used by dpkg-source)
    #[arg(short = 'F', allow_hyphen_values = true, value_name = "NUM", help = gettext("Ignore at most NUM lines of context at each end of a hunk (default 2)"))]
    fuzz: Option<usize>,

    /// Batch mode (GNU; used by dpkg-source)
    #[arg(short = 't', help = gettext("Ask no questions: skip patches naming no file and assume reversed patches are reversed"))]
    batch: bool,

    /// Interpret patch as context diff
    #[arg(short = 'c', help = gettext("Interpret the patch file as a context difference"))]
    context: bool,

    /// Force; do not prompt
    #[arg(short = 'f', help = gettext("Force; do not ask any questions and assume answers"))]
    force: bool,

    /// Change to directory before processing
    #[arg(short = 'd', allow_hyphen_values = true, value_name = "DIR", help = gettext("Change to directory before processing"))]
    directory: Option<PathBuf>,

    /// Mark changes with #ifdef directive
    #[arg(short = 'D', allow_hyphen_values = true, value_name = "DEFINE", help = gettext("Mark changes with #ifdef/#endif using DEFINE"))]
    ifdef_define: Option<String>,

    /// Interpret patch as ed script
    #[arg(short = 'e', help = gettext("Interpret the patch file as an ed script"))]
    ed: bool,

    /// Read patch from file
    #[arg(short = 'i', allow_hyphen_values = true, value_name = "PATCHFILE", help = gettext("Read the patch from PATCHFILE"))]
    patchfile: Option<PathBuf>,

    /// Loose whitespace matching
    #[arg(short = 'l', help = gettext("Match any sequence of blanks in the diff to any sequence in the file"))]
    loose: bool,

    /// Interpret patch as normal diff
    #[arg(short = 'n', help = gettext("Interpret the patch file as a normal difference"))]
    normal: bool,

    /// Ignore already-applied patches
    #[arg(short = 'N', help = gettext("Ignore patches that appear to be already applied"))]
    forward: bool,

    /// Write output to file
    #[arg(short = 'o', allow_hyphen_values = true, value_name = "OUTFILE", help = gettext("Write output to OUTFILE instead of patching in place"))]
    output: Option<PathBuf>,

    /// Strip path components
    #[arg(short = 'p', allow_hyphen_values = true, value_name = "NUM", help = gettext("Strip NUM leading path components from file names"))]
    strip: Option<usize>,

    /// Override reject filename
    #[arg(short = 'r', long = "reject-file", allow_hyphen_values = true, value_name = "REJECTFILE", help = gettext("Write rejects to REJECTFILE instead of .rej; '-' discards them"))]
    reject: Option<PathBuf>,

    /// Reverse patch direction
    #[arg(short = 'R', help = gettext("Assume the patch was created with old and new files swapped"))]
    reverse: bool,

    /// Interpret patch as unified diff
    #[arg(short = 'u', help = gettext("Interpret the patch file as a unified difference"))]
    unified: bool,

    /// File to patch
    #[arg(name = "FILE", help = gettext("File to patch"))]
    file: Option<PathBuf>,

    #[arg(long, help = gettext("Print version"), action = clap::ArgAction::Version)]
    version: Option<bool>,
}

impl Args {
    /// Validate command-line arguments.
    fn validate(&self) -> Result<(), String> {
        // Check for mutually exclusive format options
        let format_count = [self.context, self.ed, self.normal, self.unified]
            .iter()
            .filter(|&&x| x)
            .count();

        if format_count > 1 {
            return Err(gettext("only one of -c, -e, -n, -u may be specified"));
        }

        // -R cannot be used with ed scripts
        if self.reverse && self.ed {
            return Err(gettext("-R cannot be used with ed scripts"));
        }

        Ok(())
    }

    /// How backups are named, if they are made at all: -B and -z each ask
    /// for one, as in GNU patch.
    fn backup_name(&self) -> Option<BackupName> {
        if !self.backup && self.backup_prefix.is_none() && self.backup_suffix.is_none() {
            return None;
        }
        Some(BackupName {
            prefix: self.backup_prefix.clone(),
            suffix: self.backup_suffix.clone(),
        })
    }

    /// Convert Args to PatchConfig.
    fn to_config(&self) -> PatchConfig {
        PatchConfig {
            backup: self.backup_name(),
            force: self.force,
            batch: self.batch,
            max_fuzz: self.fuzz,
            remove_empty: self.remove_empty,
            force_context: self.context,
            directory: self.directory.clone(),
            ifdef_define: self.ifdef_define.as_deref().map(bytes::from_arg),
            force_ed: self.ed,
            patchfile: self.patchfile.clone(),
            loose_whitespace: self.loose,
            force_normal: self.normal,
            ignore_applied: self.forward,
            output_file: self.output.clone(),
            strip_count: self.strip,
            reject_file: self.reject.as_ref().map(|r| {
                if r.as_os_str() == "-" {
                    RejectFile::Discard
                } else {
                    RejectFile::Path(r.clone())
                }
            }),
            reverse: self.reverse,
            force_unified: self.unified,
            target_file: self.file.clone(),
        }
    }
}

/// Read patch content from stdin or file, as patch text (see `bytes`).
fn read_patch_input(config: &PatchConfig) -> io::Result<String> {
    let mut content = Vec::new();
    match &config.patchfile {
        Some(path) => {
            File::open(path)?.read_to_end(&mut content)?;
        }
        None => {
            io::stdin().lock().read_to_end(&mut content)?;
        }
    }
    Ok(bytes::decode(&content))
}

/// Open the file a patch section applies to. None (with a message, in GNU
/// patch's words) when the file is refused: a name leading through a link is
/// skipped, and a file that is not a regular file is left alone with all of
/// the section's hunks put in its reject file. An I/O error ends the run.
fn open_section(
    file_patch: &FilePatch,
    name: Name,
    config: &PatchConfig,
    written_rejects: &mut HashSet<PathBuf>,
) -> Result<Option<Target>, PatchError> {
    let shown = name.path().display();
    match open_target(&name) {
        Ok(target) => Ok(Some(target)),
        Err(Refusal::InvalidName) => {
            eprintln!(
                "patch: {}",
                gettext!("Invalid file name {} -- skipping patch", shown)
            );
            Ok(None)
        }
        Err(Refusal::NotRegular) => {
            eprintln!(
                "patch: {}",
                gettext!("File {} is not a regular file -- refusing to patch", shown)
            );
            let rejects: Vec<_> = file_patch
                .hunks
                .iter()
                .enumerate()
                .map(|(i, hunk)| (i + 1, hunk.clone(), String::new()))
                .collect();
            if let Err(e) = write_rejects(&rejects, &name, config, written_rejects) {
                eprintln!("patch: {}: {}", shown, e);
            }
            Ok(None)
        }
        Err(Refusal::Io(e)) => Err(e.into()),
    }
}

/// Main entry point.
fn run(args: Args) -> Result<bool, PatchError> {
    let config = args.to_config();

    // Change directory if -d specified
    if let Some(ref dir) = config.directory {
        env::set_current_dir(dir)?;
    }

    // Read patch input
    let patch_content = read_patch_input(&config)?;

    // Parse patch
    let mut patch = parse_patch(&patch_content, &config)?;

    // Reverse if -R specified
    if config.reverse {
        patch.reverse();
    }

    let mut had_rejects = false;
    let mut exit_code = 0;

    // Track files already backed up (-b) and -o outputs already written, so
    // that backups capture the true original and successive -o versions of the
    // same file are concatenated rather than truncated.
    let mut backed_up: HashSet<PathBuf> = HashSet::new();
    let mut written_outputs: HashSet<PathBuf> = HashSet::new();
    // Reject files already opened this run, so a second section's rejects are
    // appended rather than truncating the first section's.
    let mut written_rejects: HashSet<PathBuf> = HashSet::new();

    // Process each file patch
    for file_patch in &mut patch.file_patches {
        // Determine target file
        let target = match determine_target_file(file_patch, &config) {
            Ok(t) => t,
            Err(e) => {
                eprintln!("patch: {}", e);
                exit_code = 2;
                continue;
            }
        };

        // Read target file content (or empty for new files). A read failure
        // ends the run, as GNU patch does: it usually means the invocation is
        // wrong rather than that this one file is special. A refused file is
        // a failed section, exit status 1, as in GNU patch.
        let target = match open_section(file_patch, target, &config, &mut written_rejects)? {
            Some(target) => target,
            None => {
                had_rejects = true;
                continue;
            }
        };
        let (lines, orig_trailing_newline) = match &target.original {
            Some(original) => file_lines(original),
            None if file_patch.creates_file() => (Vec::new(), true),
            None => {
                eprintln!(
                    "patch: {}: {}",
                    target.name.path().display(),
                    gettext("No such file or directory")
                );
                exit_code = 2;
                continue;
            }
        };
        let shown = target.name.path();

        // Apply patch
        let mut applier = PatchApplier::new(&config, lines, orig_trailing_newline);
        let result = applier.apply_patch(file_patch)?;

        // A deletion patch (new file is /dev/null) removes the target rather
        // than leaving an empty file behind, and so does -E for any file the
        // patch leaves empty.
        let removes = file_patch.is_delete_file
            || (config.remove_empty && result.content.is_empty() && result.applied_any);
        if removes && config.output_file.is_none() && result.rejected_hunks.is_empty() {
            if let Err(e) = delete_target(&target, &config, &mut backed_up) {
                eprintln!("patch: {}: {}", shown.display(), e);
                exit_code = 2;
            }
            continue;
        }

        // Nothing applied means the content is byte-identical to what was
        // read; writing it back would change the file's modification time, and
        // under -b leave a backup, for a patch that did nothing.
        if !result.applied_any && !result.rejected_hunks.is_empty() {
            had_rejects = true;
            if let Err(e) = write_rejects(
                &result.rejected_hunks,
                &target.name,
                &config,
                &mut written_rejects,
            ) {
                eprintln!("patch: {}: {}", shown.display(), e);
                exit_code = 2;
            }
            for (num, _, reason) in &result.rejected_hunks {
                eprintln!("patch: Hunk #{} FAILED -- {}", num, reason);
            }
            continue;
        }

        // A write failure is reported against the file it happened to, and the
        // remaining file patches are still attempted.
        if let Err(e) = write_output(
            &result.content,
            &target,
            &config,
            result.no_trailing_newline,
            &mut backed_up,
            &mut written_outputs,
        ) {
            eprintln!("patch: {}: {}", shown.display(), e);
            exit_code = 2;
            continue;
        }

        // Handle rejects
        if !result.rejected_hunks.is_empty() {
            had_rejects = true;
            if let Err(e) = write_rejects(
                &result.rejected_hunks,
                &target.name,
                &config,
                &mut written_rejects,
            ) {
                eprintln!("patch: {}: {}", shown.display(), e);
                exit_code = 2;
            }
            for (num, _, reason) in &result.rejected_hunks {
                eprintln!("patch: Hunk #{} FAILED -- {}", num, reason);
            }
        }
    }

    if exit_code > 0 {
        return Err(PatchError::Other(String::new()));
    }

    Ok(had_rejects)
}

fn main() -> ExitCode {
    plib::diag::init_locale("patch");

    let args = Args::parse();

    // Validate arguments
    if let Err(e) = args.validate() {
        eprintln!("patch: {}", e);
        return ExitCode::from(2);
    }

    match run(args) {
        Ok(had_rejects) => {
            if had_rejects {
                ExitCode::from(1)
            } else {
                ExitCode::SUCCESS
            }
        }
        Err(PatchError::Other(s)) if s.is_empty() => ExitCode::from(2),
        Err(e) => {
            eprintln!("patch: {}", e);
            ExitCode::from(2)
        }
    }
}
