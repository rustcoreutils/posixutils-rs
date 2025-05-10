//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fmt::Display;

use pest::iterators::Pair;

use crate::man_util::parser::Rule;

/// Types of formatting AT&T UNIX version
#[derive(Debug, Clone, PartialEq)]
pub enum AtType {
    General,
    Version(String),
    V32,
    SystemIII,
    SystemV(Option<String>),
}

impl From<Pair<'_, Rule>> for AtType {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let at_type = pair.into_inner().next().unwrap();
        match at_type.as_rule() {
            Rule::at_version => Self::Version(at_type.as_str().chars().nth(1).unwrap().to_string()),
            Rule::at_32v => Self::V32,
            Rule::at_3 => Self::SystemIII,
            Rule::at_system_v => {
                Self::SystemV(at_type.as_str().chars().nth(2).map(|c| c.to_string()))
            }
            Rule::text_arg => Self::General,
            _ => unreachable!(),
        }
    }
}

impl Display for AtType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let at_n_t_unix = match self {
            AtType::General => "AT&T UNIX".to_string(),
            AtType::Version(v) => format!("Version {v} AT&T UNIX"),
            AtType::V32 => "AT&T UNIX v32".to_string(),
            AtType::SystemIII => "AT&T System III UNIX".to_string(),
            AtType::SystemV(None) => "AT&T System V UNIX".to_string(),
            AtType::SystemV(Some(v)) => format!("AT&T System V Release {v} UNIX"),
        };

        write!(f, "{at_n_t_unix}")
    }
}

impl Default for AtType {
    fn default() -> Self {
        Self::General
    }
}

/// Used for incapsulating formatting BSD/OS version logic
pub struct BsxType;

impl BsxType {
    pub fn format(version: &str) -> String {
        format!("BSD/OS {}", version)
    }

    pub fn format_default() -> String {
        "BSD/OS".to_string()
    }
}

/// Used for incapsulating formatting BSD version logic
pub struct BxType;

impl BxType {
    pub fn format(version: &str, variant: Option<&str>) -> String {
        match variant {
            Some(var) => format!("{}BSD-{}", version, var),
            None => format!("{}BSD", version),
        }
    }

    pub fn format_default() -> String {
        "BSD".to_string()
    }
}

/// Used for incapsulating formatting DragonFly version logic
pub struct DxType;

impl DxType {
    pub fn format(version: &str) -> String {
        format!("DragonFly {}", version)
    }

    pub fn format_default() -> String {
        "DragonFly".to_string()
    }
}

/// Used for incapsulating formatting FreeBSD version logic
pub struct FxType;

impl FxType {
    pub fn format(version: &str) -> String {
        format!("FreeBSD {}", version)
    }

    pub fn format_default() -> String {
        "FreeBSD".to_string()
    }
}

/// Used for incapsulating formatting NetBSD version logic
pub struct NxType;

impl NxType {
    pub fn format(version: &str) -> String {
        format!("NetBSD {}", version)
    }

    pub fn format_default() -> String {
        "NetBSD".to_string()
    }
}

/// Used for incapsulating formatting OpenBSD version logic
pub struct OxType;

impl OxType {
    pub fn format(version: &str) -> String {
        format!("OpenBSD {}", version)
    }

    pub fn format_default() -> String {
        "OpenBSD".to_string()
    }
}

/// Used for incapsulating formatting C language standards logic
#[derive(Debug, Clone, PartialEq)]
pub enum StType {
    // C Language Standards
    AnsiC,
    AnsiC89,
    IsoC,
    IsoC90,
    IsoCAmd1,
    IsoCTcor1,
    IsoCTcor2,
    IsoC99,
    IsoC2011,
    // POSIX.1 Standards before XPG4.2
    P1003188,
    P10031,
    P1003190,
    Iso9945190,
    P10031B93,
    P10031B,
    P10031C95,
    P10031I95,
    P1003196,
    Iso9945196,
    // X/Open Portability Guide before XPG4.2
    Xpg3,
    P10032,
    P1003292,
    Iso9945293,
    P10032A92,
    Xpg4,
    // X/Open Portability Guide Issue 4 Version 2 and Related Standards
    Susv1,
    Xpg42,
    XCurses42,
    P10031G2000,
    Svid4,
    // X/Open Portability Guide Issue 5 and Related Standards
    Susv2,
    Xbd5,
    Xsh5,
    Xcu5,
    Xns5,
    Xns52,
    // POSIX Issue 6 Standards
    P100312001,
    Susv3,
    P100312004,
    // POSIX Issues 7 and 8 Standards
    P100312008,
    Susv4,
    P100312024,
    // Other Standards
    Ieee754,
    Iso8601,
    Iso88023,
    Ieee127594,
}

impl From<Pair<'_, Rule>> for StType {
    fn from(pair: Pair<'_, Rule>) -> Self {
        match pair.into_inner().next().unwrap().as_rule() {
            // C Language Standards
            Rule::st_ansiC => Self::AnsiC,
            Rule::st_ansiC_89 => Self::AnsiC89,
            Rule::st_isoC => Self::IsoC,
            Rule::st_isoC_90 => Self::IsoC90,
            Rule::st_isoC_amd1 => Self::IsoCAmd1,
            Rule::st_isoC_tcor1 => Self::IsoCTcor1,
            Rule::st_isoC_tcor2 => Self::IsoCTcor2,
            Rule::st_isoC_99 => Self::IsoC99,
            Rule::st_isoC_2011 => Self::IsoC2011,
            // POSIX.1 Standards before XPG4.2
            Rule::st_p1003_1_88 => Self::P1003188,
            Rule::st_p1003_1 => Self::P10031,
            Rule::st_p1003_1_90 => Self::P1003190,
            Rule::st_iso9945_1_90 => Self::Iso9945190,
            Rule::st_p1003_1b_93 => Self::P10031B93,
            Rule::st_p1003_1b => Self::P10031B,
            Rule::st_p1003_1c_95 => Self::P10031C95,
            Rule::st_p1003_1i_95 => Self::P10031I95,
            Rule::st_p1003_1_96 => Self::P1003196,
            Rule::st_iso9945_1_96 => Self::Iso9945196,
            // X/Open Portability Guide before XPG4.2
            Rule::st_xpg3 => Self::Xpg3,
            Rule::st_p1003_2 => Self::P10032,
            Rule::st_p1003_2_92 => Self::P1003292,
            Rule::st_iso9945_2_93 => Self::Iso9945293,
            Rule::st_p1003_2a_92 => Self::P10032A92,
            Rule::st_xpg4 => Self::Xpg4,
            // X/Open Portability Guide Issue 4 Version 2 and Related Standards
            Rule::st_susv1 => Self::Susv1,
            Rule::st_xpg4_2 => Self::Xpg42,
            Rule::st_xcurses4_2 => Self::XCurses42,
            Rule::st_p1003_1g_2000 => Self::P10031G2000,
            Rule::st_svid4 => Self::Svid4,
            // X/Open Portability Guide Issue 5 and Related Standards
            Rule::st_susv2 => Self::Susv2,
            Rule::st_xbd5 => Self::Xbd5,
            Rule::st_xsh5 => Self::Xsh5,
            Rule::st_xcu5 => Self::Xcu5,
            Rule::st_xns5 => Self::Xns5,
            Rule::st_xns5_2 => Self::Xns52,
            // POSIX Issue 6 Standards
            Rule::st_p1003_1_2001 => Self::P100312001,
            Rule::st_susv3 => Self::Susv3,
            Rule::st_p1003_1_2004 => Self::P100312004,
            // POSIX Issues 7 and 8 Standards
            Rule::st_p1003_1_2008 => Self::P100312008,
            Rule::st_susv4 => Self::Susv4,
            Rule::st_p1003_1_2024 => Self::P100312024,
            // Other Standards
            Rule::st_ieee754 => Self::Ieee754,
            Rule::st_iso8601 => Self::Iso8601,
            Rule::st_iso8802_3 => Self::Iso88023,
            Rule::st_ieee1275_94 => Self::Ieee127594,
            _ => unreachable!(),
        }
    }
}

impl Display for StType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let standard = match self {
            // C Language Standards
            StType::AnsiC => "ANSI X3.159-1989 (“ANSI C89”)".to_string(),
            StType::AnsiC89 => "ANSI X3.159-1989 (“ANSI C89”)".to_string(),
            StType::IsoC => "ISO/IEC 9899:1990 (“ISO C90”)".to_string(),
            StType::IsoC90 => "ISO/IEC 9899:1990 (“ISO C90”)".to_string(),
            StType::IsoCAmd1 => "ISO/IEC 9899/AMD1:1995 (“ISO C90, Amendment 1”)".to_string(),
            StType::IsoCTcor1 => {
                "ISO/IEC 9899/TCOR1:1994 (“ISO C90, Technical Corrigendum 1”)".to_string()
            }
            StType::IsoCTcor2 => {
                "ISO/IEC 9899/TCOR2:1995 (“ISO C90, Technical Corrigendum 2”)".to_string()
            }
            StType::IsoC99 => "ISO/IEC 9899:1999 (“ISO C99”)".to_string(),
            StType::IsoC2011 => "ISO/IEC 9899:2011 (“ISO C11”)".to_string(),
            // POSIX.1 Standards before XPG4.2
            StType::P1003188 => "IEEE Std 1003.1-1988 (“POSIX.1”)".to_string(),
            StType::P10031 => "IEEE Std 1003.1 (“POSIX.1”)".to_string(),
            StType::P1003190 => "IEEE Std 1003.1-1990 (“POSIX.1”)".to_string(),
            StType::Iso9945190 => "ISO/IEC 9945-1:1990 (“POSIX.1”)".to_string(),
            StType::P10031B93 => "IEEE Std 1003.1b-1993 (“POSIX.1b”)".to_string(),
            StType::P10031B => "IEEE Std 1003.1b (“POSIX.1b”)".to_string(),
            StType::P10031C95 => "IEEE Std 1003.1c-1995 (“POSIX.1c”)".to_string(),
            StType::P10031I95 => "IEEE Std 1003.1i-1995 (“POSIX.1i”)".to_string(),
            StType::P1003196 => "ISO/IEC 9945-1:1996 (“POSIX.1”)".to_string(),
            StType::Iso9945196 => "ISO/IEC 9945-1:1996 (“POSIX.1”)".to_string(),
            // X/Open Portability Guide before XPG4.2
            StType::Xpg3 => "X/Open Portability Guide Issue 3 (“XPG3”)".to_string(),
            StType::P10032 => "IEEE Std 1003.2 (“POSIX.2”)".to_string(),
            StType::P1003292 => "IEEE Std 1003.2-1992 (“POSIX.2”)".to_string(),
            StType::Iso9945293 => "ISO/IEC 9945-2:1993 (“POSIX.2”)".to_string(),
            StType::P10032A92 => "IEEE Std 1003.2a-1992 (“POSIX.2”)".to_string(),
            StType::Xpg4 => "X/Open Portability Guide Issue 4 (“XPG4”)".to_string(),
            // X/Open Portability Guide Issue 4 Version 2 and Related Standards
            StType::Susv1 => "Version 1 of the Single UNIX Specification (“SUSv1”)".to_string(),
            StType::Xpg42 => "X/Open Portability Guide Issue 4, Version 2 (“XPG4.2”)".to_string(),
            StType::XCurses42 => "X/Open Curses Issue 4, Version 2 (“XCURSES4.2”)".to_string(),
            StType::P10031G2000 => "IEEE Std 1003.1g-2000 (“POSIX.1g”)".to_string(),
            StType::Svid4 => "System V Interface Definition, Fourth Edition (“SVID4”)".to_string(),
            // X/Open Portability Guide Issue 5 and Related Standards
            StType::Susv2 => "Version 2 of the Single UNIX Specification (“SUSv2”)".to_string(),
            StType::Xbd5 => "X/Open Base Definitions Issue 5 (“XBD5”)".to_string(),
            StType::Xsh5 => "X/Open System Interfaces and Headers Issue 5 (“XSH5”)".to_string(),
            StType::Xcu5 => "X/Open Commands and Utilities Issue 5 (“XCU5”)".to_string(),
            StType::Xns5 => "X/Open Networking Services Issue 5 (“XNS5”)".to_string(),
            StType::Xns52 => "X/Open Networking Services Issue 5.2 (“XNS5.2”)".to_string(),
            // POSIX Issue 6 Standards
            StType::P100312001 => "IEEE Std 1003.1-2001 (“POSIX.1”)".to_string(),
            StType::Susv3 => "Version 3 of the Single UNIX Specification (“SUSv3”)".to_string(),
            StType::P100312004 => "IEEE Std 1003.1-2004 (“POSIX.1”)".to_string(),
            // POSIX Issues 7 and 8 Standards
            StType::P100312008 => "IEEE Std 1003.1-2008 (“POSIX.1”)".to_string(),
            StType::Susv4 => "Version 4 of the Single UNIX Specification (“SUSv4”)".to_string(),
            // TODO: documentation doesn't containt needed text.
            StType::P100312024 => "".to_string(),
            // Other Standards
            StType::Ieee754 => "IEEE Std 754-1985".to_string(),
            StType::Iso8601 => "ISO 8601".to_string(),
            StType::Iso88023 => "ISO 8802-3: 1989".to_string(),
            StType::Ieee127594 => "IEEE Std 1275-1994 (“Open Firmware”)".to_string(),
        };

        write!(f, "{standard}")
    }
}
