//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! PTY-based integration tests.
//!
//! The shell only enables job control, the vi-mode line editor and the other
//! interactive machinery when its standard input and standard error are
//! terminals, so none of that is reachable through the stdin-pipe harness in
//! `integration.rs`.
//!
//! Tests drive the real `sh` binary through a pseudo-terminal and assert on
//! files the shell was asked to write, rather than on the terminal stream: the
//! line editor redraws the line after every keystroke, so scraping the screen
//! would be both noisy and brittle.

use plib::tmp::{tempdir, TempDir};
use portable_pty::{native_pty_system, CommandBuilder, PtySize};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::thread;
use std::time::{Duration, Instant};

/// An interactive `sh` running in a pseudo-terminal.
struct ShPtySession {
    child: Box<dyn portable_pty::Child + Send + Sync>,
    writer: Box<dyn Write + Send>,
    dir: TempDir,
}

impl ShPtySession {
    fn new() -> Self {
        let dir = tempdir().unwrap();
        let pty_system = native_pty_system();
        let pair = pty_system
            .openpty(PtySize {
                rows: 24,
                cols: 80,
                pixel_width: 0,
                pixel_height: 0,
            })
            .unwrap();

        let mut cmd = CommandBuilder::new(plib::testing::get_binary_path("sh"));
        cmd.env("TERM", "vt100");
        // keep the tests independent of the user's history file
        cmd.env("HISTFILE", dir.path().join("history"));
        cmd.env("ENV", "");
        cmd.cwd(dir.path());

        let child = pair.slave.spawn_command(cmd).unwrap();
        drop(pair.slave);

        // The slave blocks once the terminal's output buffer fills, so the
        // master has to be drained continuously.
        let mut reader = pair.master.try_clone_reader().unwrap();
        thread::spawn(move || {
            let mut buf = [0u8; 4096];
            while matches!(reader.read(&mut buf), Ok(n) if n > 0) {}
        });
        let writer = pair.master.take_writer().unwrap();

        Self { child, writer, dir }
    }

    /// The directory the shell was started in; tests write their results here.
    fn path(&self, name: &str) -> PathBuf {
        self.dir.path().join(name)
    }

    /// Types `keys` as if at the keyboard, without a trailing newline.
    fn keys(&mut self, keys: &str) {
        self.writer.write_all(keys.as_bytes()).unwrap();
        self.writer.flush().unwrap();
        // the editor processes one keystroke at a time
        thread::sleep(Duration::from_millis(40));
    }

    /// Types a command line and submits it.
    fn line(&mut self, command: &str) {
        self.keys(command);
        self.keys("\r");
        // give the shell time to run it before the next line is typed
        thread::sleep(Duration::from_millis(120));
    }

    /// Submits `exit` and waits for the shell to terminate.
    fn exit(mut self) -> TempDir {
        self.line("exit");
        let deadline = Instant::now() + Duration::from_secs(5);
        while Instant::now() < deadline {
            if matches!(self.child.try_wait(), Ok(Some(_))) {
                break;
            }
            thread::sleep(Duration::from_millis(50));
        }
        let _ = self.child.kill();
        self.dir
    }
}

/// Reads a file the shell wrote, waiting briefly for it to appear.
fn read_result(path: &Path) -> String {
    let deadline = Instant::now() + Duration::from_secs(2);
    loop {
        if let Ok(contents) = std::fs::read_to_string(path) {
            return contents;
        }
        if Instant::now() >= deadline {
            panic!("{} was never written", path.display());
        }
        thread::sleep(Duration::from_millis(20));
    }
}

#[test]
fn dollar_dash_reports_an_interactive_shell() {
    let mut sh = ShPtySession::new();
    let out = sh.path("flags");
    sh.line(&format!("echo $- > '{}'", out.display()));
    let dir = sh.exit();
    let flags = read_result(&dir.path().join("flags"));
    assert!(
        flags.contains('i'),
        "expected $- to contain 'i', got {flags:?}"
    );
    // job control is on by default for an interactive shell
    assert!(
        flags.contains('m'),
        "expected $- to contain 'm', got {flags:?}"
    );
}

#[test]
fn jobs_lists_a_background_job_in_the_posix_format() {
    let mut sh = ShPtySession::new();
    let out = sh.path("jobs");
    sh.line("sleep 5 &");
    sh.line(&format!("jobs > '{}'", out.display()));
    sh.line("kill %1");
    let dir = sh.exit();
    let listing = read_result(&dir.path().join("jobs"));
    // "[%d] %c %s %s\n"
    assert!(
        listing.starts_with("[1] + Running sleep 5"),
        "got {listing:?}"
    );
}

#[test]
fn jobs_reports_a_signal_terminated_job_distinctly() {
    let mut sh = ShPtySession::new();
    let out = sh.path("jobs");
    sh.line("sleep 5 &");
    sh.line("kill -TERM %1");
    thread::sleep(Duration::from_millis(200));
    sh.line(&format!("jobs > '{}'", out.display()));
    let dir = sh.exit();
    let listing = read_result(&dir.path().join("jobs"));
    assert!(listing.contains("Terminated (SIGTERM)"), "got {listing:?}");
}

#[test]
fn fg_returns_the_jobs_exit_status() {
    let mut sh = ShPtySession::new();
    let out = sh.path("status");
    // Long enough that `fg` reliably attaches to a job that is still running,
    // even on a slow CI machine.
    sh.line("sh -c 'sleep 1; exit 7' &");
    sh.line("fg");
    thread::sleep(Duration::from_millis(1200));
    sh.line(&format!("echo $? > '{}'", out.display()));
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("status")).trim(), "7");
}

#[test]
fn bg_resumes_a_stopped_job() {
    // `$$` in a subshell is the interactive shell's pid, so the job stops
    // a shell of its own; `wait` returns once it has stopped.
    let mut sh = ShPtySession::new();
    sh.line("sh -c 'kill -STOP $$; echo resumed > resumed' &");
    sh.line("wait %1; jobs > stopped; bg %1 > bg");
    wait_for_file(&sh.path("resumed"));
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("resumed")).trim(), "resumed");
    assert!(read_result(&dir.path().join("stopped")).contains("Stopped (SIGSTOP)"));
    assert_eq!(
        read_result(&dir.path().join("bg")),
        "[1] sh -c 'kill -STOP $$; echo resumed > resumed' &\n"
    );
}

#[test]
fn vi_mode_recalls_the_previous_command_from_history() {
    // `k` in command mode steps back through the history; running the recalled
    // line again appends a second copy to the file.
    let mut sh = ShPtySession::new();
    let out = sh.path("history_test");
    sh.line("set -o vi");
    sh.line(&format!("echo ONE >> '{}'", out.display()));
    sh.keys("\x1b"); // ESC: leave insert mode
    sh.keys("k"); // recall the previous command
    sh.keys("\r");
    thread::sleep(Duration::from_millis(200));
    let dir = sh.exit();
    assert_eq!(
        read_result(&dir.path().join("history_test")),
        "ONE\nONE\n",
        "the recalled command should have run a second time"
    );
}

#[test]
fn vi_mode_edits_the_line_before_running_it() {
    // Type a line with a trailing typo, then delete it with `x` in command
    // mode before submitting.
    let mut sh = ShPtySession::new();
    let out = sh.path("edited");
    sh.line("set -o vi");
    sh.keys(&format!("echo TWO >> '{}'X", out.display()));
    sh.keys("\x1b"); // ESC: leaving insert mode steps back onto the typo
    sh.keys("x"); // delete it
    sh.keys("\r");
    thread::sleep(Duration::from_millis(200));
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("edited")), "TWO\n");
}

/// Runs `sh -c script` in `dir` with a fresh pseudo-terminal as its
/// controlling terminal, and hangs the terminal up once the script has
/// created the file `started`; the script goes on when the file `go`
/// appears. Returns whether the shell exited successfully.
fn run_through_a_hangup(script: &str, dir: &Path) -> bool {
    let pair = native_pty_system()
        .openpty(PtySize {
            rows: 24,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        })
        .unwrap();
    let mut cmd = CommandBuilder::new(plib::testing::get_binary_path("sh"));
    cmd.args(["-c", script]);
    cmd.env("ENV", "");
    cmd.cwd(dir);
    let mut child = pair.slave.spawn_command(cmd).unwrap();
    drop(pair.slave);
    read_result(&dir.join("started"));
    // Closing the only master descriptor hangs the terminal up: the session
    // loses its controlling terminal, as when expect's pty goes away.
    drop(pair.master);
    std::fs::write(dir.join("go"), "").unwrap();
    let deadline = Instant::now() + Duration::from_secs(10);
    loop {
        if let Ok(Some(status)) = child.try_wait() {
            return status.success();
        }
        if Instant::now() >= deadline {
            let _ = child.kill();
            panic!("the shell did not finish after the terminal hung up");
        }
        thread::sleep(Duration::from_millis(20));
    }
}

/// The script for `run_through_a_hangup`: a pipeline that is running when
/// the terminal goes away, then two more commands.
fn pipeline_across_a_hangup(set_option: &str) -> String {
    format!(
        "trap '' HUP; {set_option}
         {{ : > started; while [ ! -e go ]; do sleep 0.02; done; echo x; }} | cat > /dev/null
         echo survived $? > out
         echo a | cat > out2"
    )
}

// Without job control the shell never touches the terminal's foreground
// process group, so a terminal that goes away mid-pipeline changes nothing.
// The shell used to hand the pipeline the terminal and panicked when taking
// it back failed (ENOTTY), which failed ~40 of binutils' DejaGnu tests run
// under expect. dash and bash both carry on.
#[test]
fn pipeline_survives_the_terminal_going_away() {
    let dir = tempdir().unwrap();
    let ok = run_through_a_hangup(&pipeline_across_a_hangup(""), dir.path());
    assert_eq!(read_result(&dir.path().join("out")), "survived 0\n");
    assert_eq!(read_result(&dir.path().join("out2")), "a\n");
    assert!(ok, "the shell exited unsuccessfully");
}

// Under job control the pipeline is a job of its own and is handed the
// terminal; taking it back after the terminal has gone away fails (ENOTTY)
// and must change nothing, as in bash (dash stops with an error).
#[test]
fn job_control_pipeline_survives_the_terminal_going_away() {
    let dir = tempdir().unwrap();
    let ok = run_through_a_hangup(&pipeline_across_a_hangup("set -m"), dir.path());
    assert_eq!(read_result(&dir.path().join("out")), "survived 0\n");
    assert_eq!(read_result(&dir.path().join("out2")), "a\n");
    assert!(ok, "the shell exited unsuccessfully");
}

// Ctrl-C at an interactive prompt stops a running loop of built-ins and
// returns to the prompt, as in dash (the user's choice, 2026-10-14): the
// shell's SIGINT handling only recorded the signal, so `while :; do :; done`
// could never be interrupted.
#[test]
fn ctrl_c_interrupts_a_loop_of_builtins() {
    let mut sh = ShPtySession::new();
    let out = sh.path("back");
    sh.line("while :; do :; done");
    thread::sleep(Duration::from_millis(300));
    sh.keys("\x03");
    thread::sleep(Duration::from_millis(300));
    sh.line(&format!("echo back > '{}'", out.display()));
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("back")).trim(), "back");
}

/// Waits for the shell to create `path`, which a test command does once it
/// is running; generous, for a slow CI machine.
fn wait_for_file(path: &Path) {
    let deadline = Instant::now() + Duration::from_secs(10);
    while !path.exists() {
        if Instant::now() >= deadline {
            panic!("{} was never created", path.display());
        }
        thread::sleep(Duration::from_millis(20));
    }
}

/// Types ^C once the command running at the prompt has created `started`,
/// then has `$?` written to `status`.
fn interrupt_once_started(sh: &mut ShPtySession) {
    wait_for_file(&sh.path("started"));
    // let a foreground utility, if any, get going
    thread::sleep(Duration::from_millis(300));
    sh.keys("\x03");
    thread::sleep(Duration::from_millis(300));
    sh.line("echo $? > status");
}

/// Runs `command` at the prompt and interrupts it once started.
fn interrupt_at_the_prompt(command: &str) -> TempDir {
    let mut sh = ShPtySession::new();
    sh.line(command);
    interrupt_once_started(&mut sh);
    sh.exit()
}

// ^C abandons the whole command line, not just the loop, and leaves `$?`
// 130, as in dash.
#[test]
fn ctrl_c_abandons_the_command_line_with_status_130() {
    let dir = interrupt_at_the_prompt("while :; do : > started; done; echo no > after");
    assert_eq!(read_result(&dir.path().join("status")).trim(), "130");
    assert!(!dir.path().join("after").exists());
}

// A foreground utility has the terminal and gets the SIGINT itself; the
// shell then abandons the rest of the line, as dash does.
#[test]
fn ctrl_c_stops_a_foreground_utility_and_the_rest_of_the_line() {
    let dir = interrupt_at_the_prompt(": > started; sleep 30; echo no > after");
    assert_eq!(read_result(&dir.path().join("status")).trim(), "130");
    assert!(!dir.path().join("after").exists());
}

// The same inside a function, an `eval` and a loop's condition.
#[test]
fn ctrl_c_leaves_functions_eval_and_conditions() {
    let dir = interrupt_at_the_prompt(
        "f() { eval 'while : > started; do :; done'; echo no > f; }; f; echo no > after",
    );
    assert_eq!(read_result(&dir.path().join("status")).trim(), "130");
    assert!(!dir.path().join("f").exists());
    assert!(!dir.path().join("after").exists());
}

// `read` from the terminal is interrupted and assigns nothing.
#[test]
fn ctrl_c_interrupts_read() {
    let mut sh = ShPtySession::new();
    sh.line("x=old; : > started; if read x; then echo no > then; fi; echo no > after");
    interrupt_once_started(&mut sh);
    sh.line("echo \"$x\" > x");
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("status")).trim(), "130");
    assert_eq!(read_result(&dir.path().join("x")), "old\n");
    assert!(!dir.path().join("then").exists());
    assert!(!dir.path().join("after").exists());
}

// `wait` for a background job is interrupted; the job runs on.
#[test]
fn ctrl_c_interrupts_wait() {
    let mut sh = ShPtySession::new();
    sh.line("sleep 30 & : > started; wait; echo no > after");
    interrupt_once_started(&mut sh);
    sh.line("jobs > jobs; kill %1");
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("status")).trim(), "130");
    assert!(read_result(&dir.path().join("jobs")).contains("Running"));
    assert!(!dir.path().join("after").exists());
}

// ^C while a line is being typed discards it.
#[test]
fn ctrl_c_discards_the_line_being_typed() {
    let mut sh = ShPtySession::new();
    sh.keys("echo no > typed");
    sh.keys("\x03");
    thread::sleep(Duration::from_millis(300));
    sh.line("echo $? > status");
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("status")).trim(), "130");
    assert!(!dir.path().join("typed").exists());
}

// A trapped SIGINT runs its trap, and the command goes on.
#[test]
fn trapped_ctrl_c_runs_the_trap_and_goes_on() {
    let dir = interrupt_at_the_prompt(
        "trap 't=1' INT; t=0; while [ $t = 0 ]; do : > started; done; echo \"after $?\" > after",
    );
    assert_eq!(read_result(&dir.path().join("after")), "after 0\n");
    assert_eq!(read_result(&dir.path().join("status")).trim(), "0");
}

// An ignored SIGINT is ignored by the commands the shell runs too, though
// the interactive shell itself catches it, and a caught signal is reset to
// its default by exec.
#[test]
fn ignored_sigint_stays_ignored_for_commands() {
    let mut sh = ShPtySession::new();
    sh.line("trap '' INT; sh -c 'kill -INT $$; echo survived' > out");
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("out")), "survived\n");
}

/// The `sh` under test, quoted for a command line.
fn sh_binary() -> String {
    format!("'{}'", plib::testing::get_binary_path("sh").display())
}

/// Splits a line of numbers written by the shell.
fn numbers(text: &str) -> Vec<i64> {
    text.split_whitespace()
        .map(|field| field.parse().unwrap())
        .collect()
}

// XCU 2.11: an interactive shell leads a process group of its own and makes
// it the terminal's foreground group; when it exits, the terminal goes back
// to the group that had it. The session's shell is already a group leader,
// so a second interactive shell is started inside it.
#[test]
fn interactive_shell_takes_the_terminal_and_gives_it_back() {
    let mut sh = ShPtySession::new();
    sh.line(&format!("{} -i", sh_binary()));
    sh.line("echo $$ $(ps -o pgid= -p $$) $(ps -o tpgid= -p $$) > inner");
    wait_for_file(&sh.path("inner"));
    sh.line("exit");
    sh.line("echo $$ $(ps -o pgid= -p $$) $(ps -o tpgid= -p $$) > outer");
    let dir = sh.exit();
    let inner = numbers(&read_result(&dir.path().join("inner")));
    assert_eq!(inner.len(), 3, "got {inner:?}");
    assert!(
        inner[0] == inner[1] && inner[1] == inner[2],
        "pid, process group and terminal group differ: {inner:?}"
    );
    let outer = numbers(&read_result(&dir.path().join("outer")));
    assert_eq!(outer.len(), 3, "got {outer:?}");
    assert_eq!(outer[1], outer[2], "the terminal did not come back");
}

// A job-control shell ignores SIGTSTP, SIGTTIN and SIGTTOU for itself only:
// the commands it runs get the default actions back, but a signal ignored
// when the shell started stays ignored for them.
#[test]
fn job_control_stop_signals_are_the_shells_own() {
    let mut sh = ShPtySession::new();
    sh.line(&format!("{} -c trap > default", sh_binary()));
    sh.line(&format!("trap '' TSTP; {} -i", sh_binary()));
    sh.line(&format!("{} -c trap > inherited", sh_binary()));
    wait_for_file(&sh.path("inherited"));
    sh.line("exit");
    let dir = sh.exit();
    let default = read_result(&dir.path().join("default"));
    for signal in ["TSTP", "TTIN", "TTOU"] {
        assert!(!default.contains(signal), "{signal} not reset: {default:?}");
    }
    let inherited = read_result(&dir.path().join("inherited"));
    assert!(inherited.contains("TSTP"), "got {inherited:?}");
    assert!(!inherited.contains("TTIN"), "got {inherited:?}");
}

/// Types ^Z once the command running at the prompt has created `started`.
fn suspend_once_started(sh: &mut ShPtySession) {
    wait_for_file(&sh.path("started"));
    // let a foreground utility, if any, get going
    thread::sleep(Duration::from_millis(300));
    sh.keys("\x1a");
    thread::sleep(Duration::from_millis(300));
}

// XCU 2.11: a foreground command and every command of a foreground pipeline
// run in a process group of their own, which has the terminal while they
// run.
#[test]
fn foreground_jobs_get_a_process_group_and_the_terminal() {
    let mut sh = ShPtySession::new();
    sh.line("sh -c 'echo $$ $(ps -o pgid= -p $$) $(ps -o tpgid= -p $$)' > simple");
    sh.line("sh -c 'echo $(ps -o pgid= -p $$) $(ps -o tpgid= -p $$)' | cat > pipeline");
    sh.line("echo $$ > shell");
    let dir = sh.exit();
    let simple = numbers(&read_result(&dir.path().join("simple")));
    assert_eq!(simple.len(), 3, "got {simple:?}");
    assert!(
        simple[0] == simple[1] && simple[1] == simple[2],
        "pid, process group and terminal group differ: {simple:?}"
    );
    let pipeline = numbers(&read_result(&dir.path().join("pipeline")));
    let shell = numbers(&read_result(&dir.path().join("shell")));
    assert_eq!(pipeline.len(), 2, "got {pipeline:?}");
    assert_eq!(pipeline[0], pipeline[1], "the pipeline lacks the terminal");
    assert_ne!(
        pipeline[0], shell[0],
        "the pipeline is in the shell's group"
    );
}

// ^Z stops a foreground utility; it becomes a suspended job, and `$?` is
// that of a process stopped by SIGTSTP.
#[test]
fn ctrl_z_suspends_a_foreground_utility() {
    let mut sh = ShPtySession::new();
    sh.line(": > started; sleep 30");
    suspend_once_started(&mut sh);
    sh.line("kill -l $? > status; jobs > jobs; kill -KILL %1");
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("status")).trim(), "TSTP");
    assert_eq!(
        read_result(&dir.path().join("jobs")),
        "[1] + Stopped sleep 30\n"
    );
}

// ^Z stops every command of a foreground pipeline, as one job.
#[test]
fn ctrl_z_suspends_a_whole_pipeline() {
    let mut sh = ShPtySession::new();
    sh.line("sh -c ': > started; sleep 1; echo a' | cat > out");
    suspend_once_started(&mut sh);
    // longer than the pipeline would have taken had it gone on running
    thread::sleep(Duration::from_millis(1500));
    sh.line("jobs > jobs; cat out > early");
    sh.line("bg >/dev/null; wait; cat out > late");
    let dir = sh.exit();
    assert_eq!(
        read_result(&dir.path().join("jobs")),
        "[1] + Stopped sh -c ': > started; sleep 1; echo a' | cat >out\n"
    );
    assert_eq!(read_result(&dir.path().join("early")), "");
    assert_eq!(read_result(&dir.path().join("late")), "a\n");
}

// ^C reaches only the foreground job: a background job runs on, and the
// shell abandons the line and returns to the prompt.
#[test]
fn ctrl_c_reaches_only_the_foreground_job() {
    let mut sh = ShPtySession::new();
    sh.line("sleep 30 &");
    sh.line(": > started; sleep 30; echo no > after");
    interrupt_once_started(&mut sh);
    sh.line("jobs > jobs; kill %1");
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("status")).trim(), "130");
    assert_eq!(
        read_result(&dir.path().join("jobs")),
        "[1] + Running sleep 30 &\n"
    );
    assert!(!dir.path().join("after").exists());
}

// `fg` gives a suspended job the terminal again and resumes it; its status
// is the job's. A job that stops again under `fg` keeps its number.
#[test]
fn fg_resumes_a_suspended_job_in_the_foreground() {
    let mut sh = ShPtySession::new();
    sh.line("sh -c ': > started; sleep 1; exit 7'");
    suspend_once_started(&mut sh);
    sh.line("fg > fg; echo $? > status");
    wait_for_file(&sh.path("status"));
    // again, and stopped once more while in the foreground
    sh.line("rm started; : > started; sleep 30");
    suspend_once_started(&mut sh);
    sh.line("rm started; fg");
    thread::sleep(Duration::from_millis(300));
    sh.keys("\x1a");
    thread::sleep(Duration::from_millis(300));
    sh.line("jobs > jobs; kill -KILL %1");
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("status")).trim(), "7");
    assert_eq!(
        read_result(&dir.path().join("fg")),
        "sh -c ': > started; sleep 1; exit 7'\n"
    );
    assert_eq!(
        read_result(&dir.path().join("jobs")),
        "[1] + Stopped sleep 30\n"
    );
}

// `fg` resumes every process of a suspended pipeline: its output arrives.
#[test]
fn fg_resumes_a_whole_pipeline() {
    let mut sh = ShPtySession::new();
    sh.line("sh -c ': > started; sleep 1; echo a' | cat > out");
    suspend_once_started(&mut sh);
    sh.line("fg > /dev/null; echo $? > status");
    wait_for_file(&sh.path("status"));
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("status")).trim(), "0");
    assert_eq!(read_result(&dir.path().join("out")), "a\n");
}

// A background pipeline is one job whose processes the shell forked itself
// into the group of the first, as for a foreground pipeline: `jobs -l` shows
// that process, `$!` is the last one, `kill %1` reaches both and `wait %1`
// reports the last one's status.
#[test]
fn background_pipeline_is_a_job_of_every_command() {
    let mut sh = ShPtySession::new();
    sh.line("sh -c 'echo $$ > p1; sleep 30' | sh -c 'echo $$ > p2; cat' &");
    wait_for_file(&sh.path("p1"));
    wait_for_file(&sh.path("p2"));
    sh.line("echo $! > bang; jobs -l > jobs; kill %1; wait %1; echo $? > status");
    wait_for_file(&sh.path("status"));
    sh.line("kill -0 $(cat p1) $(cat p2) 2>/dev/null; echo $? > gone");
    let dir = sh.exit();
    let p1 = read_result(&dir.path().join("p1")).trim().to_string();
    let p2 = read_result(&dir.path().join("p2")).trim().to_string();
    assert_eq!(read_result(&dir.path().join("bang")).trim(), p2);
    assert_eq!(
        read_result(&dir.path().join("jobs")),
        format!(
            "[1] + {p1} Running sh -c 'echo $$ > p1; sleep 30' | sh -c 'echo $$ > p2; cat' &\n"
        )
    );
    assert_eq!(read_result(&dir.path().join("status")).trim(), "143");
    assert_ne!(read_result(&dir.path().join("gone")).trim(), "0");
}

// `bg` resumes a suspended job in the background: `jobs` shows it stopped,
// then running, and `wait` collects it.
#[test]
fn bg_resumes_a_suspended_job_in_the_background() {
    let mut sh = ShPtySession::new();
    sh.line("sh -c ': > started; sleep 1; echo done > out'");
    suspend_once_started(&mut sh);
    sh.line("jobs > before; bg > bg; jobs > after; wait; echo $? > status");
    wait_for_file(&sh.path("status"));
    let dir = sh.exit();
    let command = "sh -c ': > started; sleep 1; echo done > out'";
    assert_eq!(
        read_result(&dir.path().join("before")),
        format!("[1] + Stopped {command}\n")
    );
    assert_eq!(
        read_result(&dir.path().join("bg")),
        format!("[1] {command}\n")
    );
    assert_eq!(
        read_result(&dir.path().join("after")),
        format!("[1] + Running {command}\n")
    );
    assert_eq!(read_result(&dir.path().join("status")).trim(), "0");
    assert_eq!(read_result(&dir.path().join("out")), "done\n");
}

// The terminal's modes follow the foreground job, as in bash: one that
// stops or is killed leaves the terminal as the shell had it, and the
// shell keeps a stopped job's modes for `fg` to give back to it (as zsh
// and ksh do; bash does not). One that exits normally keeps what it set,
// as `stty` must. Modes are compared as `stty -g` writes them, which is
// portable where the flag names of `stty -a` are not.
#[test]
fn terminal_modes_follow_the_foreground_job() {
    let mut sh = ShPtySession::new();
    sh.line("stty -g > before");
    sh.line(
        "sh -c 'stty -echo; stty -g > noecho; kill -STOP $$; stty -g > job'; stty -g > stopped",
    );
    wait_for_file(&sh.path("stopped"));
    sh.line("fg > /dev/null; stty -g > after");
    wait_for_file(&sh.path("after"));
    sh.line("sh -c 'stty -echo; kill -KILL $$'; stty -g > killed");
    wait_for_file(&sh.path("killed"));
    sh.line("sh -c 'stty -echo'; stty -g > exited");
    wait_for_file(&sh.path("exited"));
    let dir = sh.exit();
    let modes = |name: &str| read_result(&dir.path().join(name));
    let (before, noecho) = (modes("before"), modes("noecho"));
    assert_ne!(before, noecho, "stty -echo changed nothing");
    assert_eq!(modes("stopped"), before, "a stopped job's modes stayed");
    assert_eq!(modes("job"), noecho, "fg did not give the job its modes");
    assert_eq!(modes("after"), noecho, "a job's exit undid its modes");
    assert_eq!(modes("killed"), before, "a killed job's modes stayed");
    assert_eq!(modes("exited"), noecho, "stty's change was undone");
}
