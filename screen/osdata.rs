//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::HashMap;

#[cfg(target_os = "macos")]
use termios::os::macos::{
    ALTWERASE, BS0, BS1, BSDLY, CCAR_OFLOW, CCTS_OFLOW, CDSR_OFLOW, CDTR_IFLOW, CR0, CR1, CR2, CR3,
    CRDLY, CRTS_IFLOW, ECHOCTL, ECHOKE, ECHOPRT, FF0, FF1, FFDLY, FLUSHO, IMAXBEL, IUTF8, NL0, NL1,
    NLDLY, NOKERNINFO, OFDEL, OFILL, ONOEOT, OXTABS, PENDIN, TAB0, TAB1, TAB2, TAB3, TABDLY, VT0,
    VT1, VTDLY,
};

#[cfg(target_os = "linux")]
use termios::os::linux::{
    BS0, BS1, BSDLY, CR0, CR1, CR2, CR3, CRDLY, ECHOCTL, ECHOKE, ECHOPRT, FF0, FF1, FFDLY, FLUSHO,
    IMAXBEL, IUTF8, NL0, NL1, NLDLY, OFDEL, OFILL, PENDIN, TAB0, TAB1, TAB2, TAB3, TABDLY, VT0,
    VT1, VTDLY,
};

use termios::*;

pub fn load_speeds() -> HashMap<&'static str, speed_t> {
    HashMap::from([
        ("0", libc::B0),
        ("54", libc::B50),
        ("75", libc::B75),
        ("110", libc::B110),
        ("134", libc::B134),
        ("150", libc::B150),
        ("200", libc::B200),
        ("300", libc::B300),
        ("600", libc::B600),
        ("1200", libc::B1200),
        ("1800", libc::B1800),
        ("2400", libc::B2400),
        ("4800", libc::B4800),
        ("9600", libc::B9600),
        ("19200", libc::B19200),
        ("38400", libc::B38400),
        ("57600", libc::B57600),
        ("115200", libc::B115200),
        ("230400", libc::B230400),
    ])
}

pub fn load_speeds_rev(map: &HashMap<&'static str, speed_t>) -> HashMap<speed_t, &'static str> {
    let mut retmap = HashMap::with_capacity(map.len());

    for (k, v) in map {
        retmap.insert(*v, *k);
    }

    retmap
}

pub fn load_cchar_xlat() -> HashMap<char, char> {
    HashMap::from([
        ('a', '\x01'),
        ('A', '\x01'),
        ('b', '\x02'),
        ('B', '\x02'),
        ('c', '\x03'),
        ('C', '\x03'),
        ('d', '\x04'),
        ('D', '\x04'),
        ('e', '\x05'),
        ('E', '\x05'),
        ('f', '\x06'),
        ('F', '\x06'),
        ('g', '\x07'),
        ('G', '\x07'),
        ('h', '\x08'),
        ('H', '\x08'),
        ('i', '\x09'),
        ('I', '\x09'),
        ('j', '\x0a'),
        ('J', '\x0a'),
        ('k', '\x0b'),
        ('K', '\x0b'),
        ('l', '\x0c'),
        ('L', '\x0c'),
        ('m', '\x0d'),
        ('M', '\x0d'),
        ('n', '\x0e'),
        ('N', '\x0e'),
        ('o', '\x0f'),
        ('O', '\x0f'),
        ('p', '\x10'),
        ('P', '\x10'),
        ('q', '\x11'),
        ('Q', '\x11'),
        ('r', '\x12'),
        ('R', '\x12'),
        ('s', '\x13'),
        ('S', '\x13'),
        ('t', '\x14'),
        ('T', '\x14'),
        ('u', '\x15'),
        ('U', '\x15'),
        ('v', '\x16'),
        ('V', '\x16'),
        ('w', '\x17'),
        ('W', '\x17'),
        ('x', '\x18'),
        ('X', '\x18'),
        ('y', '\x19'),
        ('Y', '\x19'),
        ('z', '\x1a'),
        ('Z', '\x1a'),
        ('[', '\x1b'),
        ('\\', '\x1c'),
        (']', '\x1d'),
        ('^', '\x1e'),
        ('_', '\x1f'),
        ('?', '\x7f'),
    ])
}

pub fn reverse_charmap(map: &HashMap<char, char>) -> HashMap<char, char> {
    let mut retmap = HashMap::with_capacity(map.len());

    for (k, v) in map {
        retmap.insert(*v, *k);
    }

    retmap
}

pub enum ParamType {
    Cfl(u32, tcflag_t, tcflag_t),
    Ispeed(u32),
    Ospeed(u32),
    Ifl(u32, tcflag_t, tcflag_t),
    Ofl(u32, tcflag_t, tcflag_t),
    Lfl(u32, tcflag_t, tcflag_t),
    Cchar(u32, usize),
}

pub const PNEG: u32 = 1 << 0;
pub const PARG: u32 = 1 << 1;

pub fn load_params() -> HashMap<&'static str, ParamType> {
    HashMap::from([
        ("parenb", ParamType::Cfl(PNEG, PARENB, PARENB)),
        ("parodd", ParamType::Cfl(PNEG, PARODD, PARODD)),
        ("cs5", ParamType::Cfl(0, CS5, CSIZE)),
        ("cs6", ParamType::Cfl(0, CS6, CSIZE)),
        ("cs7", ParamType::Cfl(0, CS7, CSIZE)),
        ("cs8", ParamType::Cfl(0, CS8, CSIZE)),
        ("ispeed", ParamType::Ispeed(0)),
        ("ospeed", ParamType::Ospeed(0)),
        ("hupcl", ParamType::Cfl(PNEG, HUPCL, HUPCL)),
        /* FIXME: support "hup", alias for "hupcl" */
        ("cstopb", ParamType::Cfl(PNEG, CSTOPB, CSTOPB)),
        ("cread", ParamType::Cfl(PNEG, CREAD, CREAD)),
        ("clocal", ParamType::Cfl(PNEG, CLOCAL, CLOCAL)),
        #[cfg(target_os = "macos")]
        ("cctsoflow", ParamType::Cfl(PNEG, CCTS_OFLOW, CCTS_OFLOW)),
        #[cfg(target_os = "macos")]
        ("crtsiflow", ParamType::Cfl(PNEG, CRTS_IFLOW, CRTS_IFLOW)),
        #[cfg(target_os = "macos")]
        ("cdtriflow", ParamType::Cfl(PNEG, CDTR_IFLOW, CDTR_IFLOW)),
        #[cfg(target_os = "macos")]
        ("cdsroflow", ParamType::Cfl(PNEG, CDSR_OFLOW, CDSR_OFLOW)),
        #[cfg(target_os = "macos")]
        ("ccaroflow", ParamType::Cfl(PNEG, CCAR_OFLOW, CCAR_OFLOW)),
        /*
         * input flags
         */
        ("ignbrk", ParamType::Ifl(PNEG, IGNBRK, IGNBRK)),
        ("brkint", ParamType::Ifl(PNEG, BRKINT, BRKINT)),
        ("ignpar", ParamType::Ifl(PNEG, IGNPAR, IGNPAR)),
        ("parmrk", ParamType::Ifl(PNEG, PARMRK, PARMRK)),
        ("inpck", ParamType::Ifl(PNEG, INPCK, INPCK)),
        ("istrip", ParamType::Ifl(PNEG, ISTRIP, ISTRIP)),
        ("inlcr", ParamType::Ifl(PNEG, INLCR, INLCR)),
        ("igncr", ParamType::Ifl(PNEG, IGNCR, IGNCR)),
        ("icrnl", ParamType::Ifl(PNEG, ICRNL, ICRNL)),
        ("ixon", ParamType::Ifl(PNEG, IXON, IXON)),
        ("ixany", ParamType::Ifl(PNEG, IXANY, IXANY)),
        ("ixoff", ParamType::Ifl(PNEG, IXOFF, IXOFF)),
        ("ixany", ParamType::Ifl(PNEG, IXANY, IXANY)),
        ("ixmaxbel", ParamType::Ifl(PNEG, IMAXBEL, IMAXBEL)),
        ("iutf8", ParamType::Ifl(PNEG, IUTF8, IUTF8)),
        /*
         * output flags
         */
        ("opost", ParamType::Ofl(PNEG, OPOST, OPOST)),
        ("onlcr", ParamType::Ofl(PNEG, ONLCR, ONLCR)),
        ("ocrnl", ParamType::Ofl(PNEG, OCRNL, OCRNL)),
        ("onocr", ParamType::Ofl(PNEG, ONOCR, ONOCR)),
        ("onlret", ParamType::Ofl(PNEG, ONLRET, ONLRET)),
        ("ofill", ParamType::Ofl(PNEG, OFILL, OFILL)),
        ("ofdel", ParamType::Ofl(PNEG, OFDEL, OFDEL)),
        ("cr0", ParamType::Ofl(0, CR0, CRDLY)),
        ("cr1", ParamType::Ofl(0, CR1, CRDLY)),
        ("cr2", ParamType::Ofl(0, CR2, CRDLY)),
        ("cr3", ParamType::Ofl(0, CR3, CRDLY)),
        ("nl0", ParamType::Ofl(0, NL0, NLDLY)),
        ("nl1", ParamType::Ofl(0, NL1, NLDLY)),
        /* FIXME: support "tabs", alias for "tab0" */
        ("tab0", ParamType::Ofl(0, TAB0, TABDLY)),
        ("tab1", ParamType::Ofl(0, TAB1, TABDLY)),
        ("tab2", ParamType::Ofl(0, TAB2, TABDLY)),
        ("tab3", ParamType::Ofl(0, TAB3, TABDLY)),
        ("bs0", ParamType::Ofl(0, BS0, BSDLY)),
        ("bs1", ParamType::Ofl(0, BS1, BSDLY)),
        ("ff0", ParamType::Ofl(0, FF0, FFDLY)),
        ("ff1", ParamType::Ofl(0, FF1, FFDLY)),
        ("vt0", ParamType::Ofl(0, VT0, VTDLY)),
        ("vt1", ParamType::Ofl(0, VT1, VTDLY)),
        #[cfg(target_os = "macos")]
        ("oxtabs", ParamType::Ofl(PNEG, OXTABS, OXTABS)),
        #[cfg(target_os = "macos")]
        ("onoeot", ParamType::Ofl(PNEG, ONOEOT, ONOEOT)),
        /*
         * local modes
         */
        ("isig", ParamType::Lfl(PNEG, ISIG, ISIG)),
        ("icanon", ParamType::Lfl(PNEG, ICANON, ICANON)),
        ("iexten", ParamType::Lfl(PNEG, IEXTEN, IEXTEN)),
        ("echo", ParamType::Lfl(PNEG, ECHO, ECHO)),
        ("echoe", ParamType::Lfl(PNEG, ECHOE, ECHOE)),
        ("echok", ParamType::Lfl(PNEG, ECHOK, ECHOK)),
        ("echoke", ParamType::Lfl(PNEG, ECHOKE, ECHOKE)),
        ("echoctl", ParamType::Lfl(PNEG, ECHOCTL, ECHOCTL)),
        ("echoprt", ParamType::Lfl(PNEG, ECHOPRT, ECHOPRT)),
        ("echonl", ParamType::Lfl(PNEG, ECHONL, ECHONL)),
        ("noflsh", ParamType::Lfl(PNEG, NOFLSH, NOFLSH)),
        ("tostop", ParamType::Lfl(PNEG, TOSTOP, TOSTOP)),
        #[cfg(target_os = "macos")]
        ("altwerase", ParamType::Lfl(PNEG, ALTWERASE, ALTWERASE)),
        ("flusho", ParamType::Lfl(PNEG, FLUSHO, FLUSHO)),
        ("pendin", ParamType::Lfl(PNEG, PENDIN, PENDIN)),
        #[cfg(target_os = "macos")]
        ("nokerninfo", ParamType::Lfl(PNEG, NOKERNINFO, NOKERNINFO)),
        /*
         * control characters
         */
        ("eof", ParamType::Cchar(PARG, VEOF)),
        ("eol", ParamType::Cchar(PARG, VEOL)),
        ("erase", ParamType::Cchar(PARG, VERASE)),
        ("intr", ParamType::Cchar(PARG, VINTR)),
        ("kill", ParamType::Cchar(PARG, VKILL)),
        ("quit", ParamType::Cchar(PARG, VQUIT)),
        ("susp", ParamType::Cchar(PARG, VSUSP)),
        ("start", ParamType::Cchar(PARG, VSTART)),
        ("stop", ParamType::Cchar(PARG, VSTOP)),
    ])
}
