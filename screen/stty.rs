//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - stty get-short display
//

mod osdata;

use std::collections::HashMap;
use std::io::{self, Error};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use osdata::{ParamType, PARG, PNEG};
#[cfg(target_os = "linux")]
use termios::os::linux::{TAB0, TAB3, TABDLY};
#[cfg(target_os = "macos")]
use termios::os::macos::{TAB0, TAB3, TABDLY};
use termios::{
    cc_t, cfgetispeed, cfgetospeed, cfsetispeed, cfsetospeed, speed_t, tcflag_t, tcsetattr,
    Termios, BRKINT, CREAD, CS5, CS6, CS7, CS8, CSIZE, ECHO, ECHOE, ECHOK, HUPCL, ICANON, ICRNL,
    IEXTEN, IGNCR, IGNPAR, INLCR, INPCK, ISIG, IXON, OPOST, PARENB, PARODD, TCSANOW, VERASE, VKILL,
    VMIN, VTIME,
};

const HDR_SAVE: &str = "pfmt1";

#[derive(Parser)]
#[command(version, about = gettext("stty - set the options for a terminal"))]
struct Args {
    #[arg(
        short,
        long,
        group = "mode",
        help = gettext(
            "Write to standard output all the current settings, in human-readable form"
        )
    )]
    all: bool,

    #[arg(
        short = 'g',
        long,
        group = "mode",
        help = gettext(
            "Write to standard output all the current settings, in stty-readable form"
        )
    )]
    save: bool,

    #[arg(help = gettext("List of terminal configuration commands"))]
    operands: Vec<String>,
}

fn speed_to_str(revspeed: &HashMap<speed_t, &'static str>, speed: speed_t) -> String {
    match revspeed.get(&speed) {
        None => format!("B{}?", speed),
        Some(s) => String::from(*s),
    }
}

fn ti_baud_str(revspeed: &HashMap<speed_t, &'static str>, ti: &Termios) -> String {
    let ispeed = cfgetispeed(ti);
    let ispeed_str = speed_to_str(revspeed, ispeed);
    let ospeed = cfgetospeed(ti);
    let ospeed_str = speed_to_str(revspeed, ospeed);

    if ispeed == ospeed {
        format!("speed {} baud;", ispeed_str)
    } else {
        format!("ispeed {} baud; ospeed {} baud;", ispeed_str, ospeed_str)
    }
}

// display short-form stty values (abbreviated, non-defaults)
fn stty_show_short(ti: Termios) -> io::Result<()> {
    let speedmap = osdata::load_speeds();
    let revspeed = osdata::load_speeds_rev(&speedmap);
    let cchar_xlat = osdata::load_cchar_xlat();
    let cchar_rev = osdata::reverse_charmap(&cchar_xlat);

    // Always show baud rate
    print!("{} ", ti_baud_str(&revspeed, &ti));

    // Show key settings that may differ from typical defaults
    let mut settings: Vec<String> = Vec::new();

    // Character size (default is typically cs8)
    let csize = ti.c_cflag & CSIZE;
    if csize != CS8 {
        let csname = match csize {
            CS5 => "cs5",
            CS6 => "cs6",
            CS7 => "cs7",
            _ => "cs8",
        };
        settings.push(csname.to_string());
    }

    // Common local flags
    if (ti.c_lflag & ECHO) == 0 {
        settings.push("-echo".to_string());
    }
    if (ti.c_lflag & ICANON) == 0 {
        settings.push("-icanon".to_string());
    }
    if (ti.c_lflag & ISIG) == 0 {
        settings.push("-isig".to_string());
    }
    if (ti.c_lflag & IEXTEN) == 0 {
        settings.push("-iexten".to_string());
    }

    // Common input flags
    if (ti.c_iflag & ICRNL) == 0 {
        settings.push("-icrnl".to_string());
    }
    if (ti.c_iflag & IXON) == 0 {
        settings.push("-ixon".to_string());
    }

    // Common output flags
    if (ti.c_oflag & OPOST) == 0 {
        settings.push("-opost".to_string());
    }

    // Show min/time if non-canonical mode
    if (ti.c_lflag & ICANON) == 0 {
        settings.push(format!("min = {}", ti.c_cc[VMIN]));
        settings.push(format!("time = {}", ti.c_cc[VTIME]));
    }

    // Show key control characters with non-default values
    let show_cchar = |name: &str, idx: usize, default: u8| -> Option<String> {
        let ch = ti.c_cc[idx];
        if ch != default as cc_t {
            let ch_str = if ch == 0 {
                "<undef>".to_string()
            } else if let Some(xlat) = cchar_rev.get(&(ch as char)) {
                format!("^{}", xlat)
            } else if ch < 0x20 {
                format!("^{}", (ch + 0x40) as char)
            } else {
                format!("{}", ch)
            };
            Some(format!("{} = {}", name, ch_str))
        } else {
            None
        }
    };

    // Check key control characters against common defaults
    if let Some(s) = show_cchar("erase", VERASE, 0x7f) {
        settings.push(s);
    }
    if let Some(s) = show_cchar("kill", VKILL, 0x15) {
        settings.push(s);
    }
    if let Some(s) = show_cchar("intr", termios::VINTR, 0x03) {
        settings.push(s);
    }

    if settings.is_empty() {
        println!();
    } else {
        println!("{}", settings.join("; "));
    }

    Ok(())
}

fn build_flagstr(name: &str, flag: tcflag_t, pflg: u32, vset: tcflag_t, mask: tcflag_t) -> String {
    if (flag & mask) == vset {
        name.to_string()
    } else if (pflg & PNEG) != 0 {
        format!("-{}", name)
    } else {
        String::new()
    }
}

fn flagmap_push(
    ti: &Termios,
    flagmap: &mut HashMap<&'static str, Vec<String>>,
    name: &str,
    param: &ParamType,
) {
    match param {
        ParamType::Ifl(pflg, vset, vclear) => {
            let flag = ti.c_iflag;
            let flagstr = build_flagstr(name, flag, *pflg, *vset, *vclear);
            if !flagstr.is_empty() {
                let v = flagmap.get_mut("iflags").unwrap();
                v.push(flagstr);
            }
        }
        ParamType::Ofl(pflg, vset, vclear) => {
            let flag = ti.c_oflag;
            let flagstr = build_flagstr(name, flag, *pflg, *vset, *vclear);
            if !flagstr.is_empty() {
                let v = flagmap.get_mut("oflags").unwrap();
                v.push(flagstr);
            }
        }
        ParamType::Cfl(pflg, vset, vclear) => {
            let flag = ti.c_cflag;
            let flagstr = build_flagstr(name, flag, *pflg, *vset, *vclear);
            if !flagstr.is_empty() {
                let v = flagmap.get_mut("cflags").unwrap();
                v.push(flagstr);
            }
        }
        ParamType::Lfl(pflg, vset, vclear) => {
            let flag = ti.c_lflag;
            let flagstr = build_flagstr(name, flag, *pflg, *vset, *vclear);
            if !flagstr.is_empty() {
                let v = flagmap.get_mut("lflags").unwrap();
                v.push(flagstr);
            }
        }
        _ => {}
    };
}

fn show_flags(name: &str, flags: &[String]) {
    println!("{}: {}", name, flags.join(" "));
}

fn show_cchars(tty_params: &HashMap<&'static str, ParamType>, ti: &Termios) {
    let mut v = Vec::new();
    let cchar_xlat = osdata::load_cchar_xlat();
    let cchar_rev = osdata::reverse_charmap(&cchar_xlat);

    // minor inefficiency: 2nd iteration through param list

    for (name, param) in tty_params {
        if let ParamType::Cchar(_pflg, chidx) = param {
            let ch = ti.c_cc[*chidx] as char;
            let ch_rev = cchar_rev.get(&ch);
            let ch_str = {
                if ch == '\0' {
                    String::from("<undef>")
                } else if let Some(ch_xlat) = ch_rev {
                    format!("^{}", ch_xlat)
                } else {
                    format!("{}", ti.c_cc[*chidx])
                }
            };

            v.push(format!("{} = {}", name, ch_str));
        }
    }

    println!("cchars: {}", v.join("; "));
}

// display long-form stty values
fn stty_show_long(ti: Termios) -> io::Result<()> {
    let speedmap = osdata::load_speeds();
    let revspeed = osdata::load_speeds_rev(&speedmap);
    println!("{}", ti_baud_str(&revspeed, &ti));

    let tty_params = osdata::load_params();
    let flagnames = vec!["lflags", "iflags", "oflags", "cflags"];

    let mut flagmap: HashMap<&'static str, Vec<String>> = HashMap::new();
    for name in &flagnames {
        flagmap.insert(name, Vec::new());
    }

    for (name, param) in &tty_params {
        flagmap_push(&ti, &mut flagmap, name, param);
    }

    for flagname in &flagnames {
        show_flags(flagname, flagmap.get(flagname).unwrap());
    }

    show_cchars(&tty_params, &ti);

    Ok(())
}

// display compact, parse-able form stty values
#[allow(clippy::unnecessary_cast)] // tcflag_t is u64 on macOS, u32 on Linux
fn stty_show_compact(ti: Termios) -> io::Result<()> {
    // encode settings as pairs of (String,u64)
    let mut tiv = vec![
        (String::from("ifl"), ti.c_iflag as u64),
        (String::from("ofl"), ti.c_oflag as u64),
        (String::from("cfl"), ti.c_cflag as u64),
        (String::from("lfl"), ti.c_lflag as u64),
        (String::from("isp"), cfgetispeed(&ti) as u64),
        (String::from("osp"), cfgetospeed(&ti) as u64),
    ];

    // encode control chars as pairs of (String,u64)
    for (i, cc) in ti.c_cc.iter().enumerate() {
        tiv.push((format!("ch{}", i), *cc as u64));
    }

    // convert pairs to list of strings
    let mut sv = Vec::new();
    sv.push(String::from(HDR_SAVE));
    for te in tiv {
        sv.push(format!("{}={}", te.0, te.1));
    }

    // convert list to single compact string
    println!("{}", sv.join(":"));

    Ok(())
}

// set termio control chars based on pairmap values
fn set_ti_cchar(
    ti: &mut Termios,
    pairmap: &HashMap<String, u64>,
    mut dirty: bool,
    idx: usize,
) -> Result<bool, &'static str> {
    // lookup control char.  should only fail if data corruption.
    let mapkey = format!("ch{}", idx);
    let value_res = pairmap.get(&mapkey);
    if value_res.is_none() {
        return Err("Invalid ent: missing cchar");
    }
    let value = *value_res.unwrap() as termios::cc_t;

    // conditionally set the control char.  if changed, mark dirty.
    if ti.c_cc[idx] != value {
        ti.c_cc[idx] = value;
        dirty = true;
    }

    Ok(dirty)
}

// set termio flags based on pairmap values
fn set_ti_flags(
    ti: &mut Termios,
    pairmap: &HashMap<String, u64>,
    mut dirty: bool,
    setname: &str,
) -> Result<bool, &'static str> {
    // lookup flag set name.  should only fail if data corruption.
    let value_res = pairmap.get(setname);
    if value_res.is_none() {
        return Err("Invalid ent: missing flags");
    }
    let value = *value_res.unwrap() as termios::tcflag_t;

    // conditionally set the specified flag.  if changed, mark dirty.
    match setname {
        "ifl" => {
            if ti.c_iflag != value {
                ti.c_iflag = value;
                dirty = true;
            }
        }
        "ofl" => {
            if ti.c_oflag != value {
                ti.c_oflag = value;
                dirty = true;
            }
        }
        "cfl" => {
            if ti.c_cflag != value {
                ti.c_cflag = value;
                dirty = true;
            }
        }
        "lfl" => {
            if ti.c_lflag != value {
                ti.c_lflag = value;
                dirty = true;
            }
        }
        _ => {
            return Err("BUG - should never occur");
        }
    }

    Ok(dirty)
}

// mutate a termio flag based on the provided spec
fn set_ti_flag(
    flags: &mut tcflag_t,
    val_set: tcflag_t,
    val_clear: tcflag_t,
    negate: bool,
    mut dirty: bool,
) -> bool {
    // clear flag bits
    let mut newflags = *flags & !val_clear;

    // set flag bits (unless negation)
    if !negate {
        newflags |= val_set;
    }

    // commit flag bits to termio struct, if changed
    if newflags != *flags {
        dirty = true;
        *flags = newflags;
    }

    dirty
}

fn set_ti_cchar_oparg(
    xlat: &HashMap<char, char>,
    cc: &mut cc_t,
    op_arg: &str,
    _dirty: bool,
) -> Result<bool, &'static str> {
    if op_arg == "^-" || op_arg == "undef" {
        *cc = 0;
        return Ok(true);
    }

    if !op_arg.starts_with("^") || op_arg.len() != 2 {
        return Err("Invalid cchar specification");
    }
    let ch = op_arg.chars().nth(1).unwrap();
    let value_res = xlat.get(&ch);
    if value_res.is_none() {
        return Err("Unknown cchar specification");
    }

    let value = value_res.unwrap();
    *cc = *value as cc_t;
    Ok(true)
}

fn set_ti_speed(
    ti: &mut Termios,
    speedmap: &HashMap<&str, speed_t>,
    is_input: bool,
    op_arg: &str,
) -> io::Result<()> {
    let speed_res = speedmap.get(op_arg);
    if speed_res.is_none() {
        return Err(Error::other("Invalid speed"));
    }
    let speed = speed_res.unwrap();

    if is_input {
        cfsetispeed(ti, *speed)?;
    } else {
        cfsetospeed(ti, *speed)?;
    }

    Ok(())
}

// update termio settings based on pairmap values
fn merge_map(ti: &mut Termios, pairmap: &HashMap<String, u64>) -> Result<bool, &'static str> {
    // push flags into termio struct
    let mut dirty = false;
    dirty = set_ti_flags(ti, pairmap, dirty, "ifl")?;
    dirty = set_ti_flags(ti, pairmap, dirty, "ofl")?;
    dirty = set_ti_flags(ti, pairmap, dirty, "cfl")?;
    dirty = set_ti_flags(ti, pairmap, dirty, "lfl")?;

    // push control chars into termio struct
    let cclen = ti.c_cc.len();
    for idx in 0..cclen {
        dirty = set_ti_cchar(ti, pairmap, dirty, idx)?;
    }

    Ok(dirty)
}

// update termio settings based on compact-form input line
fn stty_set_compact(mut ti: Termios, compact: &str) -> io::Result<()> {
    // split by ':'
    let parts: Vec<&str> = compact.split(":").collect();

    // assert validates _prior_ user input check, guaranteeing this condition
    assert_eq!(parts[0], HDR_SAVE);

    // iterate through strings, assuming they are KEY=VALUE pair strings.
    // skip first entry, our header marker.
    let mut pairmap = HashMap::new();
    let rawpairs = &parts[1..];
    for pairstr in rawpairs {
        // split string into KEY=VALUE
        match pairstr.split_once('=') {
            // parse value into u64 integer
            Some((key, value)) => match value.parse::<u64>() {
                Ok(n) => {
                    // it's a valid entry.  insert into settings pairmap.
                    pairmap.insert(String::from(key), n);
                }
                Err(_e) => {
                    return Err(Error::other("Invalid ent"));
                }
            },
            None => {
                return Err(Error::other("Invalid ent"));
            }
        }
    }

    // merge parsed settings into termio struct
    let dirty = match merge_map(&mut ti, &pairmap) {
        Ok(d) => d,
        Err(e) => {
            return Err(Error::other(e));
        }
    };

    // finally, commit new termio settings (if any)
    if dirty {
        tcsetattr(libc::STDIN_FILENO, TCSANOW, &ti)?;
    }

    Ok(())
}

// Handle POSIX combination modes
// Returns Some(true) if mode was handled and changed settings
// Returns Some(false) if mode was handled but didn't change settings
// Returns None if not a combination mode
fn handle_combination_mode(ti: &mut Termios, operand: &str, negate: bool) -> Option<bool> {
    match operand {
        "tabs" => {
            // tabs = tab0, -tabs = tab3
            let newval = if negate { TAB3 } else { TAB0 };
            let oldval = ti.c_oflag & TABDLY;
            if oldval != newval {
                ti.c_oflag = (ti.c_oflag & !TABDLY) | newval;
                Some(true)
            } else {
                Some(false)
            }
        }
        "evenp" | "parity" => {
            // Enable parenb and cs7; disable parodd
            // -evenp/-parity: disable parenb, set cs8
            if negate {
                ti.c_cflag &= !PARENB;
                ti.c_cflag = (ti.c_cflag & !CSIZE) | CS8;
            } else {
                ti.c_cflag |= PARENB;
                ti.c_cflag &= !PARODD;
                ti.c_cflag = (ti.c_cflag & !CSIZE) | CS7;
            }
            Some(true)
        }
        "oddp" => {
            // Enable parenb, cs7, and parodd
            // -oddp: disable parenb, set cs8
            if negate {
                ti.c_cflag &= !PARENB;
                ti.c_cflag = (ti.c_cflag & !CSIZE) | CS8;
            } else {
                ti.c_cflag |= PARENB | PARODD;
                ti.c_cflag = (ti.c_cflag & !CSIZE) | CS7;
            }
            Some(true)
        }
        "raw" => {
            // raw mode: cs8, disable erase/kill/intr/quit/eof/eol, disable opost/inpck
            // POSIX: stty cs8 erase ^- kill ^- intr ^- quit ^- eof ^- eol ^- -post -inpck
            if negate {
                // -raw (cooked): restore normal processing
                ti.c_oflag |= OPOST;
                ti.c_iflag |= BRKINT | IGNPAR | ICRNL | IXON;
                ti.c_lflag |= ISIG | ICANON | ECHO | ECHOK | IEXTEN;
            } else {
                ti.c_cflag = (ti.c_cflag & !CSIZE) | CS8;
                ti.c_cc[VERASE] = 0;
                ti.c_cc[VKILL] = 0;
                ti.c_cc[termios::VINTR] = 0;
                ti.c_cc[termios::VQUIT] = 0;
                ti.c_cc[termios::VEOF] = 0;
                ti.c_cc[termios::VEOL] = 0;
                ti.c_oflag &= !OPOST;
                ti.c_iflag &= !INPCK;
            }
            Some(true)
        }
        "cooked" => {
            // Same as -raw
            ti.c_oflag |= OPOST;
            ti.c_iflag |= BRKINT | IGNPAR | ICRNL | IXON;
            ti.c_lflag |= ISIG | ICANON | ECHO | ECHOK | IEXTEN;
            Some(true)
        }
        "nl" => {
            // nl: disable icrnl
            // -nl: enable icrnl, unset inlcr and igncr
            if negate {
                ti.c_iflag |= ICRNL;
                ti.c_iflag &= !(INLCR | IGNCR);
            } else {
                ti.c_iflag &= !ICRNL;
            }
            Some(true)
        }
        "ek" => {
            // Reset ERASE and KILL to system defaults
            // Using common default values
            ti.c_cc[VERASE] = 0x7f; // DEL
            ti.c_cc[VKILL] = 0x15; // ^U
            Some(true)
        }
        "sane" => {
            // Reset to reasonable values
            ti.c_iflag = BRKINT | ICRNL | IXON;
            ti.c_oflag = OPOST;
            ti.c_cflag = (ti.c_cflag & !CSIZE) | CS8 | CREAD | HUPCL;
            ti.c_lflag = ISIG | ICANON | ECHO | ECHOE | ECHOK | IEXTEN;
            // Set default control characters
            ti.c_cc[VERASE] = 0x7f;
            ti.c_cc[VKILL] = 0x15;
            ti.c_cc[termios::VINTR] = 0x03; // ^C
            ti.c_cc[termios::VQUIT] = 0x1c; // ^\
            ti.c_cc[termios::VEOF] = 0x04; // ^D
            ti.c_cc[termios::VSTART] = 0x11; // ^Q
            ti.c_cc[termios::VSTOP] = 0x13; // ^S
            ti.c_cc[termios::VSUSP] = 0x1a; // ^Z
            ti.c_cc[VMIN] = 1;
            ti.c_cc[VTIME] = 0;
            Some(true)
        }
        _ => None,
    }
}

// update termio settings based on setting-per-arg parsed values
fn stty_set_long(mut ti: Termios, args: &Args) -> io::Result<()> {
    assert!(args.operands.len() > 1);

    // load static list of params
    let tty_params = osdata::load_params();
    let cchar_xlat = osdata::load_cchar_xlat();
    let speedmap = osdata::load_speeds();

    let mut dirty = false;

    // parse each operand
    let mut idx = 0;
    while idx < args.operands.len() {
        let operand_raw = &args.operands[idx];

        // if operand begins with "-", it is a negation
        let mut negate = false;
        let operand = {
            if let Some(st) = operand_raw.strip_prefix("-") {
                negate = true;
                st
            } else {
                operand_raw.as_str()
            }
        };

        // special case: set two speeds, if all-numeric operand
        if operand.parse::<u64>().is_ok() {
            set_ti_speed(&mut ti, &speedmap, true, operand)?;
            set_ti_speed(&mut ti, &speedmap, false, operand)?;
            idx += 1;
            continue;
        }

        // Handle combination modes (evenp, oddp, raw, cooked, nl, ek, sane, tabs)
        if let Some(changed) = handle_combination_mode(&mut ti, operand, negate) {
            if changed {
                dirty = true;
            }
            idx += 1;
            continue;
        }

        // lookup operand in param map
        let param_res = tty_params.get(operand);
        if param_res.is_none() {
            let errstr = format!("Unknown operand {}", operand);
            return Err(Error::other(errstr));
        }
        let param = param_res.unwrap();

        let flags = match param {
            ParamType::Cfl(pflg, _, _) => pflg,
            ParamType::Ifl(pflg, _, _) => pflg,
            ParamType::Ofl(pflg, _, _) => pflg,
            ParamType::Lfl(pflg, _, _) => pflg,
            ParamType::Cchar(pflg, _) => pflg,
            ParamType::CcNum(pflg, _) => pflg,
            ParamType::Ispeed(pflg) => pflg,
            ParamType::Ospeed(pflg) => pflg,
        };
        if negate && ((flags & PNEG) == 0) {
            let errstr = format!("Operand {} cannot be negated", operand);
            return Err(Error::other(errstr));
        }

        let mut op_arg = String::new();
        if (flags & PARG) != 0 {
            idx += 1;

            if idx == args.operands.len() {
                let errstr = format!("Missing operand for {}", operand);
                return Err(Error::other(errstr));
            }

            op_arg = String::from(&args.operands[idx]);
        }

        // handle operand
        match param {
            ParamType::Cfl(_pflg, bset, bclear) => {
                dirty = set_ti_flag(&mut ti.c_cflag, *bset, *bclear, negate, dirty);
            }
            ParamType::Ifl(_pflg, bset, bclear) => {
                dirty = set_ti_flag(&mut ti.c_iflag, *bset, *bclear, negate, dirty);
            }
            ParamType::Ofl(_pflg, bset, bclear) => {
                dirty = set_ti_flag(&mut ti.c_oflag, *bset, *bclear, negate, dirty);
            }
            ParamType::Lfl(_pflg, bset, bclear) => {
                dirty = set_ti_flag(&mut ti.c_lflag, *bset, *bclear, negate, dirty);
            }
            ParamType::Cchar(_pflg, chidx) => {
                let dirty_res =
                    set_ti_cchar_oparg(&cchar_xlat, &mut ti.c_cc[*chidx], &op_arg, dirty);
                if let Err(e) = dirty_res {
                    return Err(Error::other(e));
                }

                dirty = dirty_res.unwrap();
            }
            ParamType::CcNum(_pflg, ccidx) => {
                // min/time take numeric arguments
                match op_arg.parse::<u8>() {
                    Ok(n) => {
                        if ti.c_cc[*ccidx] != n as cc_t {
                            ti.c_cc[*ccidx] = n as cc_t;
                            dirty = true;
                        }
                    }
                    Err(_) => {
                        let errstr = format!("Invalid numeric value for {}", operand);
                        return Err(Error::other(errstr));
                    }
                }
            }
            ParamType::Ispeed(_pflg) => {
                set_ti_speed(&mut ti, &speedmap, true, &op_arg)?;
            }
            ParamType::Ospeed(_pflg) => {
                set_ti_speed(&mut ti, &speedmap, false, &op_arg)?;
            }
        }

        idx += 1;
    }

    // finally, commit new termio settings (if any)
    // note: speed is changed in set_ti_speed(), not here.
    if dirty {
        tcsetattr(libc::STDIN_FILENO, TCSANOW, &ti)?;
    }

    Ok(())
}

// set termio settings based on CLI operands supplied
fn stty_set(ti: Termios, args: &Args) -> io::Result<()> {
    if args.operands.len() == 1 && args.operands[0].starts_with(HDR_SAVE) {
        stty_set_compact(ti, &args.operands[0])
    } else {
        stty_set_long(ti, args)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    // load termio settings
    let ti = Termios::from_fd(libc::STDIN_FILENO)?;

    // display long form readable, if -a
    if args.all {
        stty_show_long(ti)?;

    // display computer-parseable, if -g
    } else if args.save {
        stty_show_compact(ti)?;

    // display short form readable, if no args
    } else if args.operands.is_empty() {
        stty_show_short(ti)?;

    // otherwise, a list of operands instructing termio updates
    } else {
        stty_set(ti, &args)?;
    }

    Ok(())
}
