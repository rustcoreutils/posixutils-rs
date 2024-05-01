//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO: Write proper docs for all the constants here
use std::{collections::HashMap, sync::OnceLock};

//
//---------------------- constants for storing "in" and validating compiled magic file --------------------
//
/// Maximum length of text description/MIME type
pub const MAX_DESC: usize = 64;

/// Maximum length of text MIME type
pub const MAX_MIME: usize = 80;

/// Maximum length of "string" types
pub const MAX_STRING: usize = 128;

/// The compiled magic number in the header
pub const COMPILED_MAGIC_FILE_VERSION_NO: u32 = 16;

/// The magic file version for which this program supports
pub const COMPILED_MAGIC_FILE_MAGIC_NUM: u32 = 0xf11e041c;

/// The size of "struct Magic"**(it must be)**
pub const MAGIC_STRUCT_SIZE: u16 = 376;

//
//------- constants(flags) for describing the type of addressing being done, test being done---------------
//
/// When we find '(....)' in the offset
pub const FLAG_INDIR: u8 = 0x01;

/// When we find either >* or '>...(&' in the offset
pub const FLAG_OFFADD: u8 = 0x02;

/// When we find '>&(' in the offset
pub const FLAG_INDIROFADD: u8 = 0x04;

/// It means the comparision is unsigned
pub const FLAG_UNSIGNED: u8 = 0x08;

/// Suppress the space characters before output
pub const FLAG_NOSPACE: u8 = 0x10;

/// Test is for binary type(set only for top-level tests)
pub const FLAG_BINTEST: u8 = 0x20; //

//
//---------------------- flags for describing the type of addressing being done --------------------
//
pub const FLAG_TEXTTEST: u8 = 0x40;

pub const FLAG_OFFNEGATIVE: u8 = 0x80;

pub const FILE_OPS: &str = "&|^+-*/%";

pub const FILE_OPAND: u8 = 0;

pub const FILE_OPOR: u8 = 1;

pub const FILE_OPXOR: u8 = 2;

pub const FILE_OPADD: u8 = 3;

pub const FILE_OPMINUS: u8 = 4;

pub const FILE_OPMULTIPLY: u8 = 5;

pub const FILE_OPDIVIDE: u8 = 6;

pub const FILE_OPMODULO: u8 = 7;

pub const FILE_OPS_MASK: u8 = 0x07; // mask for above ops
                                    //
pub const FILE_UNUSED_1: u8 = 0x08;

pub const FILE_UNUSED_2: u8 = 0x10;

pub const FILE_OPSIGNED: u8 = 0x20;

pub const FILE_OPINVERSE: u8 = 0x40;

pub const FILE_OPINDIRECT: u8 = 0x80;

//
//----------- Type of test being performed(see man of magic for file 5.41(freebsd)) --------------------
//

pub const FILE_INVALID: u8 = 0;

pub const FILE_BYTE: u8 = 1;

pub const FILE_SHORT: u8 = 2;

pub const FILE_DEFAULT: u8 = 3;

pub const FILE_LONG: u8 = 4;

pub const FILE_STRING: u8 = 5;

pub const FILE_DATE: u8 = 6;

pub const FILE_BESHORT: u8 = 7;

pub const FILE_BELONG: u8 = 8;

pub const FILE_BEDATE: u8 = 9;

pub const FILE_LESHORT: u8 = 10;

pub const FILE_LELONG: u8 = 11;

pub const FILE_LEDATE: u8 = 12;

pub const FILE_PSTRING: u8 = 13;

pub const FILE_LDATE: u8 = 14;

pub const FILE_BELDATE: u8 = 15;

pub const FILE_LELDATE: u8 = 16;

pub const FILE_REGEX: u8 = 17;

pub const FILE_BESTRING16: u8 = 18;

pub const FILE_LESTRING16: u8 = 19;

pub const FILE_SEARCH: u8 = 20;

pub const FILE_MEDATE: u8 = 21;

pub const FILE_MELDATE: u8 = 22;

pub const FILE_MELONG: u8 = 23;

pub const FILE_QUAD: u8 = 24;

pub const FILE_LEQUAD: u8 = 25;

pub const FILE_BEQUAD: u8 = 26;

pub const FILE_QDATE: u8 = 27;

pub const FILE_LEQDATE: u8 = 28;

pub const FILE_BEQDATE: u8 = 29;

pub const FILE_QLDATE: u8 = 30;

pub const FILE_LEQLDATE: u8 = 31;

pub const FILE_BEQLDATE: u8 = 32;

pub const FILE_FLOAT: u8 = 33;

pub const FILE_BEFLOAT: u8 = 34;

pub const FILE_LEFLOAT: u8 = 35;

pub const FILE_DOUBLE: u8 = 36;

pub const FILE_BEDOUBLE: u8 = 37;

pub const FILE_LEDOUBLE: u8 = 38;

pub const FILE_BEID3: u8 = 39;

pub const FILE_LEID3: u8 = 40;

pub const FILE_INDIRECT: u8 = 41;

pub const FILE_QWDATE: u8 = 42;

pub const FILE_LEQWDATE: u8 = 43;

pub const FILE_BEQWDATE: u8 = 44;

pub const FILE_NAME: u8 = 45;

pub const FILE_USE: u8 = 46;

pub const FILE_CLEAR: u8 = 47;

pub const FILE_DER: u8 = 48;

pub const FILE_GUID: u8 = 49;

pub const FILE_OFFSET: u8 = 50;

pub const FILE_BEVARINT: u8 = 51;

pub const FILE_LEVARINT: u8 = 52;

pub const FILE_NAMES_SIZE: u8 = 53;

pub const FILE_FMT_NONE: u8 = 0;

pub const FILE_FMT_NUM: u8 = 1;

pub const FILE_FMT_STR: u8 = 2;

pub const FILE_FMT_QUAD: u8 = 3;

pub const FILE_FMT_FLOAT: u8 = 4;

pub const FILE_FMT_DOUBLE: u8 = 5;

/// A static HashMap used for checking the "type" field
pub fn type_hashmap() -> &'static HashMap<&'static str, (u8, u8)> {
    static TYPE_HM: OnceLock<HashMap<&'static str, (u8, u8)>> = OnceLock::new();
    TYPE_HM.get_or_init(|| {
        let mut t_hm = HashMap::new();
        t_hm.extend(vec![
            (("invalid"), (FILE_INVALID, FILE_FMT_NONE)),
            (("byte"), (FILE_BYTE, FILE_FMT_NUM)),
            (("short"), (FILE_SHORT, FILE_FMT_NUM)),
            (("default"), (FILE_DEFAULT, FILE_FMT_NONE)),
            (("long"), (FILE_LONG, FILE_FMT_NUM)),
            (("string"), (FILE_STRING, FILE_FMT_STR)),
            (("date"), (FILE_DATE, FILE_FMT_STR)),
            (("beshort"), (FILE_BESHORT, FILE_FMT_NUM)),
            (("belong"), (FILE_BELONG, FILE_FMT_NUM)),
            (("bedate"), (FILE_BEDATE, FILE_FMT_STR)),
            (("leshort"), (FILE_LESHORT, FILE_FMT_NUM)),
            (("lelong"), (FILE_LELONG, FILE_FMT_NUM)),
            (("ledate"), (FILE_LEDATE, FILE_FMT_STR)),
            (("pstring"), (FILE_PSTRING, FILE_FMT_STR)),
            (("ldate"), (FILE_LDATE, FILE_FMT_STR)),
            (("beldate"), (FILE_BELDATE, FILE_FMT_STR)),
            (("leldate"), (FILE_LELDATE, FILE_FMT_STR)),
            (("regex"), (FILE_REGEX, FILE_FMT_STR)),
            (("bestring16"), (FILE_BESTRING16, FILE_FMT_STR)),
            (("lestring16"), (FILE_LESTRING16, FILE_FMT_STR)),
            (("search"), (FILE_SEARCH, FILE_FMT_STR)),
            (("medate"), (FILE_MEDATE, FILE_FMT_STR)),
            (("meldate"), (FILE_MELDATE, FILE_FMT_STR)),
            (("melong"), (FILE_MELONG, FILE_FMT_NUM)),
            (("quad"), (FILE_QUAD, FILE_FMT_QUAD)),
            (("lequad"), (FILE_LEQUAD, FILE_FMT_QUAD)),
            (("bequad"), (FILE_BEQUAD, FILE_FMT_QUAD)),
            (("qdate"), (FILE_QDATE, FILE_FMT_STR)),
            (("leqdate"), (FILE_LEQDATE, FILE_FMT_STR)),
            (("beqdate"), (FILE_BEQDATE, FILE_FMT_STR)),
            (("qldate"), (FILE_QLDATE, FILE_FMT_STR)),
            (("leqldate"), (FILE_LEQLDATE, FILE_FMT_STR)),
            (("beqldate"), (FILE_BEQLDATE, FILE_FMT_STR)),
            (("float"), (FILE_FLOAT, FILE_FMT_FLOAT)),
            (("befloat"), (FILE_BEFLOAT, FILE_FMT_FLOAT)),
            (("lefloat"), (FILE_LEFLOAT, FILE_FMT_FLOAT)),
            (("double"), (FILE_DOUBLE, FILE_FMT_DOUBLE)),
            (("bedouble"), (FILE_BEDOUBLE, FILE_FMT_DOUBLE)),
            (("ledouble"), (FILE_LEDOUBLE, FILE_FMT_DOUBLE)),
            (("leid3"), (FILE_LEID3, FILE_FMT_NUM)),
            (("beid3"), (FILE_BEID3, FILE_FMT_NUM)),
            (("indirect"), (FILE_INDIRECT, FILE_FMT_NUM)),
            (("qwdate"), (FILE_QWDATE, FILE_FMT_STR)),
            (("leqwdate"), (FILE_LEQWDATE, FILE_FMT_STR)),
            (("beqwdate"), (FILE_BEQWDATE, FILE_FMT_STR)),
            (("name"), (FILE_NAME, FILE_FMT_NONE)),
            (("use"), (FILE_USE, FILE_FMT_NONE)),
            (("clear"), (FILE_CLEAR, FILE_FMT_NONE)),
            (("der"), (FILE_DER, FILE_FMT_STR)),
            (("guid"), (FILE_GUID, FILE_FMT_STR)),
            (("offset"), (FILE_OFFSET, FILE_FMT_QUAD)),
            (("bevarint"), (FILE_BEVARINT, FILE_FMT_STR)),
            (("levarint"), (FILE_LEVARINT, FILE_FMT_STR)),
        ]);

        t_hm
    })
}

/// A static HashMap used for special purposes in checking the special "type" field.
///
/// "special" here means, the one that isn't unsigned or signed and not used to compare
/// a value against something but for some other purpose
pub fn special_hashmap() -> &'static HashMap<&'static str, (u8, u8)> {
    static SPECIAL_HM: OnceLock<HashMap<&'static str, (u8, u8)>> = OnceLock::new();
    SPECIAL_HM.get_or_init(|| {
        let mut s_hm = HashMap::new();
        s_hm.extend(vec![
            (("der"), (FILE_DER, FILE_FMT_STR)),
            (("name"), (FILE_BYTE, FILE_FMT_STR)),
            (("use"), (FILE_BYTE, FILE_FMT_STR)),
        ]);

        s_hm
    })
}
