//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - OsStr, OsString
// - fix and test unary ops
//

extern crate libc;
extern crate plib;

use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::os::unix::fs::FileTypeExt;
use std::os::unix::fs::MetadataExt;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;

// unary operators
#[derive(PartialEq)]
enum UnaryOp {
    Block,
    Char,
    Directory,
    Exists,
    File,
    SGID,
    Symlink,
    StrNonZero,
    FIFO,
    Readable,
    Socket,
    SizeNonZero,
    Terminal,
    SUID,
    Writable,
    Executable,
    StrZero,
}

// binary operators
enum BinOp {
    PathEquals,
    PathNewer,
    PathOlder,
    StrEq,
    StrNE,
    StrLT,
    StrGT,
    IntEq,
    IntNE,
    IntLT,
    IntGT,
    IntGE,
    IntLE,
}

fn parse_unary_op(s: &str) -> Option<UnaryOp> {
    match s {
        "-b" => Some(UnaryOp::Block),
        "-c" => Some(UnaryOp::Char),
        "-d" => Some(UnaryOp::Directory),
        "-e" => Some(UnaryOp::Exists),
        "-f" => Some(UnaryOp::File),
        "-g" => Some(UnaryOp::SGID),
        "-h" => Some(UnaryOp::Symlink),
        "-L" => Some(UnaryOp::Symlink),
        "-n" => Some(UnaryOp::StrNonZero),
        "-p" => Some(UnaryOp::FIFO),
        "-r" => Some(UnaryOp::Readable),
        "-S" => Some(UnaryOp::Socket),
        "-s" => Some(UnaryOp::SizeNonZero),
        "-t" => Some(UnaryOp::Terminal),
        "-u" => Some(UnaryOp::SUID),
        "-w" => Some(UnaryOp::Writable),
        "-x" => Some(UnaryOp::Executable),
        "-z" => Some(UnaryOp::StrZero),
        _ => None,
    }
}

fn want_metadata(op: &UnaryOp) -> bool {
    match op {
        UnaryOp::Terminal | UnaryOp::StrNonZero | UnaryOp::StrZero => false,
        _ => true,
    }
}

fn eval_str(s: &str) -> bool {
    !s.is_empty()
}

fn eval_unary_str(op: &UnaryOp, s: &str) -> bool {
    match op {
        UnaryOp::StrNonZero => eval_str(s),
        UnaryOp::StrZero => !eval_str(s),

        _ => {
            unreachable!()
        }
    }
}

fn eval_unary_path(op: &UnaryOp, s: &str) -> bool {
    let path = Path::new(s);
    let metadata = match path.metadata() {
        Ok(m) => m,
        Err(_) => {
            return false;
        }
    };

    match op {
        UnaryOp::Block => metadata.file_type().is_block_device(),
        UnaryOp::Char => metadata.file_type().is_char_device(),
        UnaryOp::Directory => metadata.is_dir(),
        UnaryOp::Exists => true,
        UnaryOp::File => metadata.is_file(),
        UnaryOp::SGID => metadata.permissions().mode() & 0o2000 != 0,
        UnaryOp::Symlink => metadata.file_type().is_symlink(),
        UnaryOp::Readable => metadata.permissions().readonly(),
        UnaryOp::Socket => metadata.file_type().is_socket(),
        UnaryOp::SizeNonZero => metadata.len() > 0,
        UnaryOp::SUID => metadata.permissions().mode() & 0o4000 != 0,
        UnaryOp::Writable => metadata.permissions().mode() & 0o200 != 0,
        UnaryOp::Executable => metadata.permissions().mode() & 0o100 != 0,
        _ => {
            unreachable!()
        }
    }
}

fn eval_terminal(s: &str) -> bool {
    let fd = match s.parse::<u32>() {
        Ok(f) => f,
        Err(_) => {
            return false;
        }
    };

    // Normally, posixutils would use the atty crate.
    // Passing an arbitrary fd requires unsafe isatty in this case.

    unsafe { libc::isatty(fd as i32) == 1 }
}

fn eval_unary(op_str: &str, s: &str) -> bool {
    let op = match parse_unary_op(op_str) {
        Some(p) => p,
        None => {
            eprintln!("{}: {}", gettext("unknown operator"), op_str);
            return false;
        }
    };
    if want_metadata(&op) {
        eval_unary_path(&op, s)
    } else if op == UnaryOp::Terminal {
        eval_terminal(s)
    } else {
        eval_unary_str(&op, s)
    }
}

fn parse_binary_op(s: &str) -> Option<BinOp> {
    match s {
        "-ef" => Some(BinOp::PathEquals),
        "-nt" => Some(BinOp::PathNewer),
        "-ot" => Some(BinOp::PathOlder),
        "=" => Some(BinOp::StrEq),
        "!=" => Some(BinOp::StrNE),
        "<" => Some(BinOp::StrLT),
        ">" => Some(BinOp::StrGT),
        "-eq" => Some(BinOp::IntEq),
        "-ne" => Some(BinOp::IntNE),
        "-lt" => Some(BinOp::IntLT),
        "-gt" => Some(BinOp::IntGT),
        "-ge" => Some(BinOp::IntGE),
        "-le" => Some(BinOp::IntLE),
        _ => None,
    }
}

fn parse_int(s: &str) -> i64 {
    match s.parse::<i64>() {
        Ok(i) => i,
        Err(_) => 0,
    }
}

fn eval_binary_int(op: &BinOp, s1: &str, s2: &str) -> bool {
    let i1 = parse_int(s1);
    let i2 = parse_int(s2);

    match op {
        BinOp::IntEq => i1 == i2,
        BinOp::IntNE => i1 != i2,
        BinOp::IntLT => i1 < i2,
        BinOp::IntGT => i1 > i2,
        BinOp::IntGE => i1 >= i2,
        BinOp::IntLE => i1 <= i2,
        _ => {
            unreachable!()
        }
    }
}

fn eval_binary_str(op: &BinOp, s1: &str, s2: &str) -> bool {
    match op {
        BinOp::StrEq => s1 == s2,
        BinOp::StrNE => s1 != s2,
        BinOp::StrLT => s1 < s2,
        BinOp::StrGT => s1 > s2,
        _ => {
            unreachable!()
        }
    }
}

fn eval_binary_path(op: &BinOp, s1: &str, s2: &str) -> bool {
    let path1 = Path::new(s1);
    let path2 = Path::new(s2);
    let md1_res = path1.metadata();
    let md2_res = path2.metadata();

    match op {
        BinOp::PathEquals => {
            if md1_res.is_err() || md2_res.is_err() {
                return false;
            }
            let md1 = md1_res.unwrap();
            let md2 = md2_res.unwrap();

            (md1.dev() == md2.dev()) && (md1.ino() == md2.ino())
        }

        BinOp::PathNewer => {
            if md1_res.is_ok() && md2_res.is_err() {
                true
            } else if md1_res.is_ok() && md2_res.is_ok() {
                let l1 = md1_res.unwrap().modified().unwrap();
                let l2 = md2_res.unwrap().modified().unwrap();

                l1 > l2
            } else {
                false
            }
        }

        BinOp::PathOlder => {
            if md1_res.is_err() && md2_res.is_ok() {
                true
            } else if md1_res.is_ok() && md2_res.is_ok() {
                let l1 = md1_res.unwrap().modified().unwrap();
                let l2 = md2_res.unwrap().modified().unwrap();

                l1 < l2
            } else {
                false
            }
        }

        _ => {
            unreachable!()
        }
    }
}

fn eval_binary(s1: &str, op_str: &str, s2: &str) -> bool {
    let op = match parse_binary_op(op_str) {
        Some(p) => p,
        None => {
            eprintln!("{}: {}", gettext("unknown operator"), op_str);
            return false;
        }
    };

    match op {
        BinOp::PathEquals | BinOp::PathNewer | BinOp::PathOlder => eval_binary_path(&op, s1, s2),
        BinOp::StrEq | BinOp::StrNE | BinOp::StrLT | BinOp::StrGT => eval_binary_str(&op, s1, s2),
        BinOp::IntEq | BinOp::IntNE | BinOp::IntLT | BinOp::IntGT | BinOp::IntGE | BinOp::IntLE => {
            eval_binary_int(&op, s1, s2)
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut args: Vec<String> = std::env::args().collect();

    // if program name is "[", then final arg is "]".
    // Validate, then remove, final arg.
    if args[0] == "[" {
        if args.last() != Some(&"]".to_string()) {
            eprintln!("{}", gettext("missing closing bracket"));
            std::process::exit(1);
        }
        args.pop();
    }

    let mut op_result = false;

    match args.len() - 1 {
        0 => {}

        1 => {
            let arg1 = &args[1];
            op_result = eval_str(arg1);
        }

        2 => {
            let arg1 = &args[1];
            let arg2 = &args[2];

            if arg1 == "!" {
                op_result = !eval_str(arg2);
            } else {
                op_result = eval_unary(arg1, arg2);
            }
        }

        3 => {
            let arg1 = &args[1];
            let arg2 = &args[2];
            let arg3 = &args[3];

            if arg1 == "!" {
                op_result = !eval_unary(arg2, arg3);
            } else {
                op_result = eval_binary(arg1, arg2, arg3);
            }
        }

        4 => {
            let arg1 = &args[1];
            let arg2 = &args[2];
            let arg3 = &args[3];
            let arg4 = &args[4];

            if arg1 == "!" {
                op_result = !eval_binary(arg2, arg3, arg4);
            } else {
                eprintln!("{}", gettext("invalid number of arguments"));
            }
        }

        _ => {
            eprintln!("{}", gettext("invalid number of arguments"));
        }
    }

    let exit_code = if op_result { 0 } else { 1 };
    std::process::exit(exit_code)
}
