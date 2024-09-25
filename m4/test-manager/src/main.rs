use std::{io::Write, os::unix::ffi::OsStrExt, path::PathBuf, process::Stdio};

use clap::Parser;
use m4_test_manager::TestSnapshot;

#[derive(Parser)]
#[command(version, about)]
struct Args {
    #[command(subcommand)]
    command: Commands,
    #[arg(short, long, default_value = "fixtures/integration_tests")]
    fixtures_directory: PathBuf,
}

#[derive(Debug, clap::Subcommand)]
enum Commands {
    UpdateSnapshots(UpdateSnapshots),
}

/// Update the integration test snapshots.
#[derive(Debug, clap::Args)]
struct UpdateSnapshots {
    /// Optionally specify a secific test case name that you want to update, where name is
    /// {name}.m4 of the test case file.
    test_case_name: Option<String>,
    /// Which command to use to invoke m4 to generate reference (expected) output.
    #[arg(short, long, default_value = "m4")]
    reference_command: String,
    /// Whether to print debug output for stdout and stderr of the captured snapshot.
    /// Requires https://github.com/sharkdp/hexyl to be installed and on your path.
    #[arg(short, long)]
    debug_output: bool,
}

fn main() {
    let args = Args::parse();

    match &args.command {
        Commands::UpdateSnapshots(update) => update_snapshots(&args, update),
    }
}

fn debug(data: &[u8]) {
    let mut child = std::process::Command::new("hexyl")
        .stdin(Stdio::piped())
        .spawn()
        .expect("perhaps hexyl command isn't installed?");
    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(data).unwrap();
    drop(stdin);
    let status = child.wait().unwrap();
    if !status.success() {
        panic!("Error printing debug output");
    }
}

fn update_snapshots(args: &Args, update: &UpdateSnapshots) {
    let dir = std::fs::read_dir(&args.fixtures_directory).unwrap();
    dir.map(|result| result.unwrap())
        .filter(|entry| {
            if !(entry.path().is_file()
                && match entry.path().extension().map(|e| e.as_bytes()) {
                    Some(b"m4") => true,
                    Some(b"args") => true,
                    _ => false,
                })
            {
                return false;
            }

            if let Some(name) = update.test_case_name.as_deref() {
                if name != entry.path().file_stem().unwrap().to_str().unwrap() {
                    return false;
                }
            }

            true
        })
        .for_each(|input_file| {
            let test_name = input_file
                .path()
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_owned();

            let snapshot_file_name = format!("{test_name}.out");
            let snapshot_file = args.fixtures_directory.join(snapshot_file_name);
            let (expect_error, stdout_regex, skip_update) = if snapshot_file.exists() {
                let mut f = std::fs::OpenOptions::new()
                    .read(true)
                    .open(&snapshot_file)
                    .unwrap();
                let snapshot = TestSnapshot::deserialize(&mut f);
                if snapshot.ignore {
                    println!("SKIPPING ignored snapshot for {test_name}");
                    return;
                }
                if snapshot.skip_update && update.test_case_name.is_none() {
                    println!("SKIPPING snapshot with skip_update=true for {test_name}");
                    return;
                }
                (
                    snapshot.expect_error,
                    snapshot.stdout_regex,
                    snapshot.skip_update,
                )
            } else {
                (false, None, false)
            };

            println!("UPDATING snapshot for {test_name}");
            let output = match input_file
                .path()
                .extension()
                .expect("Input file should have extension")
                .as_bytes()
            {
                b"m4" => std::process::Command::new(&update.reference_command)
                    .arg(input_file.path())
                    .output()
                    .unwrap(),
                b"args" => {
                    let args = std::fs::read_to_string(input_file.path()).unwrap();
                    std::process::Command::new("sh")
                        .arg("-c")
                        .arg(format!("{} {args}", update.reference_command))
                        .output()
                        .unwrap()
                }
                _ => panic!("Unsupported input extension {input_file:?}"),
            };

            if update.debug_output {
                println!("stdout:");
                debug(&output.stdout);
                println!("stderr:");
                debug(&output.stderr);
                println!("status: {}", output.status);
            }

            let snapshot = TestSnapshot {
                stdout: String::from_utf8(output.stdout).unwrap(),
                stderr: String::from_utf8(output.stderr).unwrap(),
                status: output.status.code().unwrap(),
                ignore: false,
                expect_error,
                stdout_regex,
                skip_update,
            };

            if snapshot_file.exists() {
                std::fs::remove_file(&snapshot_file).unwrap();
            }
            let mut f = std::fs::OpenOptions::new()
                .write(true)
                .create(true)
                .open(snapshot_file)
                .unwrap();
            snapshot.serialize(&mut f);
        })
}
