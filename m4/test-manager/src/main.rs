use std::{ffi::OsStr, io::Write, os::unix::ffi::OsStrExt, path::PathBuf, process::Stdio};

use clap::Parser;
use m4_test_manager::TestSnapshot;

#[derive(Debug, clap::Parser)]
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
                && entry.path().extension() == Some(&OsStr::from_bytes(b"m4")))
            {
                return false;
            }

            if let Some(name) = update.test_case_name.as_ref().map(|s| s.as_str()) {
                if name != entry.path().file_stem().unwrap().to_str().unwrap() {
                    return false;
                }
            }

            true
        })
        .for_each(|m4_file| {
            let test_name = m4_file
                .path()
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_owned();

            let snapshot_file_name = format!("{test_name}.out");
            let snapshot_file = args.fixtures_directory.join(snapshot_file_name);
            if snapshot_file.exists() {
                let mut f = std::fs::OpenOptions::new()
                    .read(true)
                    .open(&snapshot_file)
                    .unwrap();
                let snapshot = TestSnapshot::deserialize(&mut f);
                if snapshot.ignore {
                    println!("SKIPPING ignored snapshot for {test_name}");
                    return;
                }
            }

            println!("UPDATING snapshot for {test_name}");
            let output = std::process::Command::new(&update.reference_command)
                .arg(m4_file.path())
                .output()
                .unwrap();

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
                expect_error: false,
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
