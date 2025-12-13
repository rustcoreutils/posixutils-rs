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
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use patch_util::{
    applier::PatchApplier,
    file_ops::{determine_target_file, read_file_lines, write_output, write_rejects},
    parser::parse_patch,
    types::{PatchConfig, PatchError},
};
use std::{
    env,
    fs::File,
    io::{self, BufReader, Read},
    path::PathBuf,
    process::ExitCode,
};

/// patch - apply changes to files
#[derive(Parser, Debug)]
#[command(
    version,
    about = gettext("patch - apply changes to files"),
    after_help = gettext("The patch utility reads a source (patch) file containing difference listings and applies those differences to a file.")
)]
struct Args {
    /// Save original file with .orig suffix
    #[arg(short = 'b', help = gettext("Save a copy of the original file with .orig suffix"))]
    backup: bool,

    /// Interpret patch as context diff
    #[arg(short = 'c', help = gettext("Interpret the patch file as a context difference"))]
    context: bool,

    /// Change to directory before processing
    #[arg(short = 'd', value_name = "DIR", help = gettext("Change to directory before processing"))]
    directory: Option<PathBuf>,

    /// Mark changes with #ifdef directive
    #[arg(short = 'D', value_name = "DEFINE", help = gettext("Mark changes with #ifdef/#endif using DEFINE"))]
    ifdef_define: Option<String>,

    /// Interpret patch as ed script
    #[arg(short = 'e', help = gettext("Interpret the patch file as an ed script"))]
    ed: bool,

    /// Read patch from file
    #[arg(short = 'i', value_name = "PATCHFILE", help = gettext("Read the patch from PATCHFILE"))]
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
    #[arg(short = 'o', value_name = "OUTFILE", help = gettext("Write output to OUTFILE instead of patching in place"))]
    output: Option<PathBuf>,

    /// Strip path components
    #[arg(short = 'p', value_name = "NUM", help = gettext("Strip NUM leading path components from file names"))]
    strip: Option<usize>,

    /// Override reject filename
    #[arg(short = 'r', value_name = "REJECTFILE", help = gettext("Write rejects to REJECTFILE instead of .rej"))]
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

    /// Convert Args to PatchConfig.
    fn to_config(&self) -> PatchConfig {
        PatchConfig {
            backup: self.backup,
            force_context: self.context,
            directory: self.directory.clone(),
            ifdef_define: self.ifdef_define.clone(),
            force_ed: self.ed,
            patchfile: self.patchfile.clone(),
            loose_whitespace: self.loose,
            force_normal: self.normal,
            ignore_applied: self.forward,
            output_file: self.output.clone(),
            strip_count: self.strip,
            reject_file: self.reject.clone(),
            reverse: self.reverse,
            force_unified: self.unified,
            target_file: self.file.clone(),
        }
    }
}

/// Read patch content from stdin or file.
fn read_patch_input(config: &PatchConfig) -> io::Result<String> {
    match &config.patchfile {
        Some(path) => {
            let file = File::open(path)?;
            let mut reader = BufReader::new(file);
            let mut content = String::new();
            reader.read_to_string(&mut content)?;
            Ok(content)
        }
        None => {
            let stdin = io::stdin();
            let mut content = String::new();
            stdin.lock().read_to_string(&mut content)?;
            Ok(content)
        }
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

        // Read target file content (or empty for new files)
        let lines = if target.exists() {
            read_file_lines(&target)?
        } else if file_patch.is_new_file {
            Vec::new()
        } else {
            eprintln!(
                "patch: {}: {}",
                target.display(),
                gettext("No such file or directory")
            );
            exit_code = 2;
            continue;
        };

        // Apply patch
        let mut applier = PatchApplier::new(&config, lines);
        let result = applier.apply_patch(file_patch)?;

        // Report any offset/fuzz messages
        for (i, hunk) in file_patch.hunks.iter().enumerate() {
            if result.had_offset || result.had_fuzz {
                // Messages are printed by applier
            }
            let _ = (i, hunk); // Silence unused warning
        }

        // Write output
        write_output(&result.content, &target, &config)?;

        // Handle rejects
        if !result.rejected_hunks.is_empty() {
            had_rejects = true;
            write_rejects(&result.rejected_hunks, &target, file_patch.format, &config)?;
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
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").unwrap();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").unwrap();

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
