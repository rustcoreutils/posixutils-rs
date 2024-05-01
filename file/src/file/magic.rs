//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use num_traits::Num;
use regex::Regex;
use std::{
    cell::RefCell,
    collections::HashMap,
    default,
    ffi::{c_char, CStr, CString},
    fs::{read, File, Metadata},
    io::{BufReader, ErrorKind, Read, Seek, SeekFrom},
    mem::size_of,
    path::Path,
    rc::Rc,
    str::FromStr,
    sync::OnceLock,
    u8,
};

use crate::{
    constants::{
        special_hashmap, type_hashmap, FILE_BEDATE, FILE_BEDOUBLE, FILE_BEFLOAT, FILE_BEID3,
        FILE_BELDATE, FILE_BELONG, FILE_BEQDATE, FILE_BEQLDATE, FILE_BEQUAD, FILE_BEQWDATE,
        FILE_BESHORT, FILE_BESTRING16, FILE_BEVARINT, FILE_BYTE, FILE_CLEAR, FILE_DATE,
        FILE_DEFAULT, FILE_DER, FILE_DOUBLE, FILE_FLOAT, FILE_FMT_DOUBLE, FILE_FMT_FLOAT,
        FILE_FMT_NONE, FILE_FMT_NUM, FILE_FMT_QUAD, FILE_FMT_STR, FILE_GUID, FILE_INDIRECT,
        FILE_INVALID, FILE_LDATE, FILE_LEDATE, FILE_LEDOUBLE, FILE_LEFLOAT, FILE_LEID3,
        FILE_LELDATE, FILE_LELONG, FILE_LEQDATE, FILE_LEQLDATE, FILE_LEQUAD, FILE_LEQWDATE,
        FILE_LESHORT, FILE_LESTRING16, FILE_LEVARINT, FILE_LONG, FILE_MEDATE, FILE_MELDATE,
        FILE_MELONG, FILE_NAME, FILE_OFFSET, FILE_OPAND, FILE_OPDIVIDE, FILE_OPINDIRECT,
        FILE_OPINVERSE, FILE_OPMINUS, FILE_OPMODULO, FILE_OPMULTIPLY, FILE_OPOR, FILE_OPSIGNED,
        FILE_OPXOR, FILE_PSTRING, FILE_QDATE, FILE_QLDATE, FILE_QUAD, FILE_QWDATE, FILE_REGEX,
        FILE_SEARCH, FILE_SHORT, FILE_STRING, FILE_USE, FLAG_INDIR, FLAG_INDIROFADD, FLAG_OFFADD,
        FLAG_OFFNEGATIVE, FLAG_UNSIGNED, MAGIC_STRUCT_SIZE, MAX_DESC, MAX_MIME, MAX_STRING,
    },
    error::CompiledMagicFileError,
};

/// Get the operator from the unsigned 8 bit value
fn get_op(c: char) -> u8 {
    match c {
        '&' => FILE_OPAND,
        '|' => FILE_OPOR,
        '^' => FILE_OPXOR,
        '+' => FILE_OPAND,
        '-' => FILE_OPMINUS,
        '*' => FILE_OPMULTIPLY,
        '/' => FILE_OPDIVIDE,
        '%' => FILE_OPMODULO,
        _ => u8::MAX,
    }
}

/// Get the type from one of the types mentionedin the "type_hashmap"
fn get_type_from_hashmap(line: &str) -> Option<(u8, &str)> {
    let th = type_hashmap();
    for key in type_hashmap().keys() {
        if line.starts_with(key) {
            let t = th[*key].0;
            return Some((t, *key));
        }
    }
    None
}
/// Parse the string containing C character escapes
///
/// We'll be accepting a line that starts with that string and we'll continue
/// until we get unescaped tab or space
fn parse_string(line: &str) {
    let mut chars = line.chars().enumerate();
    let mut value: Vec<u8> = vec![];

    while let Some((ind, c)) = chars.next() {
        if (c.is_ascii_whitespace()) {
            break;
        }
        if (ind >= (MAX_STRING)) {
            todo!("throw some error because we are iterating over a string that's more than required size");
        }

        //if c == '\\' {
        //    match chars.next().unwrap().1 {
        //        '\\' => '\\',
        //        'a' => '\x07', // alert
        //        'b' => '\x08', // backspace
        //        'f' => '\x0C', // form feed
        //        'n' => '\n',   // newline
        //        'r' => '\r',   // carriage return
        //        't' => '\t',   // horizontal tab
        //        'v' => '\x0B', // vertical tab
        //        ' ' => ' ',    // space
        //        '0'..='7' => {}
        //        'x' => {}
        //        _ => {
        //            ' '
        //            //result.push('\\');
        //            //escaped
        //        } // Treat any other character as itself
        //    }
        //};
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union ValueType {
    b: u8,

    h: u16,

    l: u32,

    q: u64,

    hs: [u8; 2],

    hl: [u8; 4],

    hq: [u8; 8],

    s: [u8; MAX_STRING],

    us: [u8; MAX_STRING],

    guid: [u64; 2],

    f: f32,

    d: f64,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union Union {
    _mask: u64,

    _s: StructS,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct StructS {
    _count: u32,

    _flags: u32,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Magic {
    // DONE
    pub cont_level: u16,

    // DONE
    pub flag: u8,

    // TODO: It comes from the strength annotation line
    pub factor: u8,

    // TODO
    pub reln: u8,

    pub vallen: u8,

    pub _type: u8,

    pub in_type: u8,

    pub in_op: u8,

    pub mask_op: u8,

    pub cond: u8,

    pub factor_op: u8,

    pub offset: i32,

    pub in_offset: i32,

    pub lineno: u32,

    pub union: Union,

    pub value: ValueType,

    pub desc: [u8; MAX_DESC],

    pub mimetype: [u8; MAX_MIME],

    pub apple: [u8; 8],

    pub ext: [u8; 64],
}

impl Magic {
    /// Create magic binary = 32
    pub fn from_binary(buf: &[u8]) -> Result<Magic, Box<dyn std::error::Error>> {
        let mut arr_buf = [0_u8; MAGIC_STRUCT_SIZE as usize];
        arr_buf.copy_from_slice(buf);

        let magic = unsafe { *(arr_buf.as_ptr() as *const Magic) };
        Ok(magic)
    }

    /// Create an empty Magic instance, which will be filled with values
    ///
    /// Btw, no values here are default values, they are just kept there to "fill up the struct"
    /// so that we can operate on it on "from_line" method
    fn empty() -> Self {
        let union = Union { _mask: 0 };
        let value_type = ValueType { b: 0 };
        Magic {
            cont_level: 0,
            flag: 0,
            factor: 0,
            reln: 0,
            vallen: 0,
            _type: 0,
            in_type: 0,
            offset: 0,
            in_offset: 0,
            in_op: 0,
            mask_op: 0,
            cond: 0,
            factor_op: 0,
            lineno: 0,
            desc: [0; MAX_DESC],
            mimetype: [0; MAX_MIME],
            apple: [0; 8],
            ext: [0; 64],
            union,
            value: value_type,
        }
    }

    pub fn from_line(mut line: String) -> Option<Magic> {
        let mut mg = Magic::empty();

        let trim_spaces_tabs = |s: &mut String| {
            *s = s.trim_start_matches(|c| c == ' ' || c == '\t').to_string();
        };

        mg.parse_cont_lvl(&mut line);
        mg.parse_offset(&mut line);

        trim_spaces_tabs(&mut line);
        mg.parse_type(&mut line);

        trim_spaces_tabs(&mut line);
        mg.parse_value(&mut line);

        trim_spaces_tabs(&mut line);
        mg.parse_desc(&mut line);

        None
    }

    /// Parses the cont_level in the line and consumes it too
    fn parse_cont_lvl(&mut self, line: &mut String) {
        let re = Regex::new(r"^>+").unwrap();
        self.cont_level = if let Some(mat) = re.find(line) {
            let cont_lvl = mat.end() as u16;
            line.replace_range(..mat.end(), "");
            cont_lvl
        } else {
            0
        };
    }

    /// Parse the offset value
    ///
    /// **Must do before**: Strip out the characters that define the `cont_lvl` i.e ">" in the
    /// given string and only put the data necessary to parse the offset(and the remaining
    /// data of the line), don't put the data to
    /// parse the type as well i.e respect the tab or spaces for separation
    ///
    fn parse_offset(&mut self, line: &mut String) -> Option<()> {
        match line.find(|c: char| c.is_whitespace()) {
            Some(ind) => {
                let offset_str = &line[0..ind];
                let rem = line[ind..].to_string();

                let mut chars: Vec<char> = offset_str.chars().collect();
                let mut ind = 0;

                if *chars.get(ind)? == '&' {
                    self.flag |= FLAG_OFFADD;
                    ind += 1;
                }

                if *chars.get(ind)? == '(' {
                    ind += 1;
                    self.flag |= FLAG_INDIR;
                    if (self.flag & FLAG_OFFADD) > 0 {
                        self.flag = (self.flag & !FLAG_OFFADD) | FLAG_INDIROFADD;
                    }

                    if *chars.get(ind)? == '&' {
                        ind += 1;
                        self.flag |= FLAG_OFFADD;
                    }
                }

                // we won't allow indirect offsets at cont_lvl 0
                if self.cont_level == 0 && (self.flag & (FLAG_OFFADD | FLAG_INDIROFADD)) > 0 {
                    todo!("Throw error for relative offset not allowed at cont lvl 0")
                }

                if *chars.get(ind)? == '-' {
                    ind += 1;
                    self.flag |= FLAG_OFFNEGATIVE;
                };
                chars.drain(..ind);

                self.offset = {
                    let mut v = chars.iter().collect::<String>();
                    let num = parse_and_consume_num(&mut v).unwrap();

                    chars = v.chars().collect();
                    ind = 0;
                    num
                };

                self.in_type = FILE_LONG;
                self.in_offset = 0;
                self.in_type = 0;
                if (self.flag & FLAG_INDIR) > 0 {
                    if *chars.get(ind)? == '.' || *chars.get(ind)? == '|' {
                        if *chars.get(ind)? == ',' {
                            self.in_op |= FILE_OPSIGNED;
                        }
                        ind += 1;

                        self.in_type = match *chars.get(ind)? {
                            'l' => FILE_LELONG,
                            'L' => FILE_BELONG,
                            'm' => FILE_MELONG,
                            'h' | 's' => FILE_LESHORT,
                            'H' | 'S' => FILE_BESHORT,
                            'c' | 'b' | 'C' | 'B' => FILE_BYTE,
                            'e' | 'f' | 'g' => FILE_LEDOUBLE,
                            'E' | 'F' | 'G' => FILE_BEDOUBLE,
                            'i' => FILE_LEID3,
                            'I' => FILE_BEID3,
                            'q' => FILE_LEQUAD,
                            'Q' => FILE_BEQUAD,
                            _ => {
                                todo!("Some sort of error");
                            }
                        };
                    }
                    ind += 1;

                    if *chars.get(ind)? == '~' {
                        self.in_op |= FILE_OPINVERSE;
                        ind += 1;
                    }

                    let op = match chars.get(ind)? {
                        '&' => FILE_OPAND,
                        '|' => FILE_OPOR,
                        '^' => FILE_OPXOR,
                        '+' => FILE_OPAND,
                        '-' => FILE_OPMINUS,
                        '*' => FILE_OPMULTIPLY,
                        '/' => FILE_OPDIVIDE,
                        '%' => FILE_OPMODULO,
                        _ => u8::MAX,
                    };

                    let op = get_op(*chars.get(ind)?);
                    if op != u8::MAX {
                        self.in_op |= op;
                        ind += 1;
                    }

                    if *chars.get(ind)? == '(' {
                        self.in_op |= FILE_OPINDIRECT;
                        ind += 1;
                    }

                    chars.drain(..ind);
                    ind = 0;
                    if (*chars.get(ind)?).is_numeric() || *chars.get(ind)? == '-' {
                        self.in_offset = {
                            let mut v = chars.iter().collect::<String>();
                            let num = parse_and_consume_num(&mut v).unwrap();

                            chars = v.chars().collect();
                            ind = 0;
                            num
                        };
                    }

                    match chars.get(ind) {
                        Some(c) => {
                            if *c != ')' {
                                todo!("throw error saying missing ) in indirect offset")
                            }
                        }
                        None => {
                            todo!("throw error saying missing ) in indirect offset")
                        }
                    }

                    if self.flag & FILE_OPINDIRECT > 0 {
                        ind += 1;
                        match chars.get(ind) {
                            Some(c) => {
                                //
                                if *c != ')' {
                                    todo!("throw error saying missing ) in indirect offset");
                                }
                            }
                            None => {
                                todo!("throw error saying missing ) in indirect offset");
                            }
                        }
                    }
                }
                *line = rem;
                Some(())
            }
            None => None,
        }
    }

    pub fn parse_type(&mut self, line: &mut String) -> Option<()> {
        let mut ind = 0;
        let chars: Vec<char> = line.chars().collect();

        if *chars.get(ind)? == 'u' {
            self._type = match get_type_from_hashmap(&line[(ind + 1)..]) {
                Some((_type, type_str)) => {
                    ind = ind + type_str.len() + 1;
                    line.drain(..ind);

                    _type
                }
                None => FILE_INVALID,
            };

            // it comes here when none of the type from the hashmap is matched
            //
            // it's usually because of the short forms being used
            // i.e dC uC etc
            if self._type == FILE_INVALID {
                self._type = match Self::get_type_from_standard_integer_type(&line[(ind + 1)..]) {
                    Some(_type) => _type,
                    None => {
                        todo!("Throw some error")
                    }
                }
            }

            if self._type != FILE_INVALID {
                self.flag |= FLAG_UNSIGNED;
            }
        } else {
            self._type = match get_type_from_hashmap(&line[(ind)..]) {
                Some((_type, type_str)) => {
                    ind = ind + type_str.len();
                    line.drain(..(type_str.len()));
                    _type
                }
                None => FILE_INVALID,
            };

            // TODO: Further FILE_INVALIDITY CHECK NEEDED
            //if self._type == FILE_INVALID {
            //todo!("Not implemented! these are types that need further write");
            //}
        }

        if self._type == FILE_INVALID {
            self._type = match Self::get_special_type_from_hashmap(&line) {
                Some((_type, type_str)) => {
                    ind = ind + type_str.len();
                    line.drain(..ind);
                    _type
                }
                None => FILE_INVALID,
            }
        }

        if self._type == FILE_INVALID {
            // in case of magic checking
            todo!("Throw an error stating that the type is simply invalid, because we can't proceed without a valid one");
        }

        if self._type == FILE_NAME && self.cont_level != 0 {
            // in case of magic checking
            todo!("Throw an error stating that name X can only be declared at the top level");
        }

        if (*chars.get(ind)? == '~') {
            if !self.is_type_string() {
                self.mask_op |= FILE_OPINVERSE;
            }
            // TODO: Some more validation may be needed
        }

        self.union._s._count = 0;
        self.union._s._flags = if self._type == FILE_PSTRING {
            1 << 7
        } else {
            0
        };

        let op = get_op(*chars.get(ind)?);

        if (op != u8::MAX) {
            if (self.is_type_string()) {
                if (op != FILE_OPDIVIDE) {
                    todo!("throw some error")
                }
                todo!("TODO: parse the modifier")
            } else {
                line.drain(0..1);
                ind += 1;
                self.union._mask = parse_and_consume_num(line).unwrap();
            }
        }
        Some(())
    }

    fn parse_value(&mut self, line: &mut String) -> Option<()> {
        let mut chars: Vec<char> = line.chars().collect();
        let mut ind = 0;

        let _char = *chars.get(ind)?;
        self.reln = match _char {
            '>' | '<' => {
                ind += 1;
                if *chars.get(ind)? == '=' {
                    // Not supported i.e >= or <=
                    todo!("propagate some error here");
                }
                _char as u8
            }
            '&' | '^' | '=' => {
                ind += 1;
                if *chars.get(ind)? == '=' {
                    //ignore it or explore more for what could be done
                }
                _char as u8
            }
            '!' => {
                ind += 1;
                _char as u8
            }
            _ => {
                ind += 1;
                if _char == 'x' && (*chars.get(ind)?).is_ascii_whitespace() {
                    _char as u8
                } else {
                    // we'll be using "=" as the default relation
                    b'='
                }
            }
        };

        line.drain(..ind);

        if (self.is_type_string()) {
            //*p = getstr(ms, m, *p, action == FILE_COMPILE);
            //if (*p == NULL) {
            //  if (ms->flags & MAGIC_CHECK)
            //    file_magwarn(ms, "cannot get string from `%s'", m->value.s);
            //  return -1;
            //}
            //if (m->type == FILE_REGEX) {
            //  file_regex_t rx;
            //  int rc = file_regcomp(&rx, m->value.s, REG_EXTENDED);
            //  if (rc) {
            //    if (ms->flags & MAGIC_CHECK)
            //      file_regerror(&rx, rc, ms);
            //  }
            //  file_regfree(&rx);
            //  return rc ? -1 : 0;
            //}
            //return 0;
        }
        //after we've got the relation we'll get the value part of that relation
        //chars = line.chars().collect();
        //for _char in chars {
        //if _char.is_ascii_whitespace() {
        //break;
        //}

        //if (self.is_type_string()) {}
        //}

        //line.drain(..ind);

        if (self.is_type_string()) {
        } else {
            if (self.reln == b'x') {
                return Some(());
            }
        }

        match self._type {
            FILE_FLOAT | FILE_BEFLOAT | FILE_LEFLOAT => {}
            FILE_DOUBLE | FILE_BEDOUBLE | FILE_LEDOUBLE => {}
            FILE_GUID => {}
            _ => {}
        }
        println!("{line}");

        //println!("{line}");
        //while let Some(c) = line.chars()

        //if (self.reln != 'x') {
        //if self.is_type_string() {}
        //}

        //if (self.reln != 'x' && get) {}
        //Some(())
        //println!("{line}");

        Some(())
    }

    fn parse_desc(&mut self, line: &mut String) -> Option<()> {
        None
    }

    fn is_type_string(&self) -> bool {
        matches!(
            self._type,
            FILE_STRING
                | FILE_PSTRING
                | FILE_BESTRING16
                | FILE_LESTRING16
                | FILE_REGEX
                | FILE_SEARCH
                | FILE_INDIRECT
                | FILE_NAME
                | FILE_USE
        )
    }

    /// Get the type from the standard integer types
    ///
    /// They are the types that are laid as combination of few alphabets to denote size
    ///
    /// Eg dC, uC, uS, dS etc
    fn get_type_from_standard_integer_type(line: &str) -> Option<u8> {
        let chars: Vec<char> = line.chars().collect();
        let _char = chars.get(0)?;

        let _type = if _char.is_alphabetic() {
            // the matched characters below are the characters of C types
            match _char {
                // It denotes either dC or uC
                'C' => FILE_BYTE,
                // It denotes either dS or uS
                'S' => FILE_SHORT,
                // It denotes either dI, dL, uL or dL
                'I' | 'L' => FILE_LONG,
                'Q' => FILE_QUAD,
                _ => FILE_INVALID,
            }
        } else if _char.is_digit(10) {
            // only the standard sizes are supported i.e 1 2 4 and 8
            match _char {
                '1' => FILE_BYTE,
                '2' => FILE_SHORT,
                '4' => FILE_LONG,
                '8' => FILE_QUAD,
                _ => FILE_INVALID,
            }
        } else {
            // it means that 'd' or 'u' that was before this check was by itself
            FILE_LONG
        };
        Some(_type)
    }

    fn get_special_type_from_hashmap(line: &str) -> Option<(u8, &str)> {
        let s_th = special_hashmap();

        for key in type_hashmap().keys() {
            if line.starts_with(key) {
                let t = s_th[*key].0;
                return Some((t, *key));
            }
        }
        None
    }

    pub fn test(&self, tf_reader: Rc<RefCell<BufReader<File>>>) -> () {

        //println!("{} and {}", self.offset, self.in_offset);
        //println!("Direct: {} and indirect : {}", self.offset, self.in_offset);
        //let _type = self.get_type();

        //match self.get_type() {
        //    Type::INVALID => {}
        //    //    Type::BYTE => self.test_byte(tf_reader),
        //    //    Type::SHORT => self.test_byte(tf_reader),
        //    //    Type::DEFAULT => self.test_byte(tf_reader),
        //    Type::STRING => self.test_string(tf_reader),
        //    Type::PSTRING => self.test_pstring(tf_reader),
        //    _ => {}
        //}

        //return String::from("abc");
        //let result = match _type {
        //Type::SHORT => self.test_short(),
        //_ => String::from("Hey"),
        //};
    }

    fn test_byte(&self, tf_rdr: Rc<RefCell<BufReader<File>>>) -> Option<CString> {
        let mut tf_rdr = tf_rdr.borrow_mut();
        let value = unsafe { self.value.b };
        tf_rdr.seek(SeekFrom::Start(self.offset as u64));

        let mut buf = [0; 1];
        if tf_rdr.read_exact(&mut buf).is_ok() && value == buf[0] {
            self.get_desc().ok()
        } else {
            None
        }
    }

    fn test_short(&self, tf_rdr: Rc<RefCell<BufReader<File>>>) -> Option<CString> {
        let mut tf_rdr = tf_rdr.borrow_mut();
        let value = unsafe { self.value.b };
        tf_rdr.seek(SeekFrom::Start(self.offset as u64));

        let mut buf = [0; 2];
        if tf_rdr.read_exact(&mut buf).is_ok() && value == buf[0] {
            self.get_desc().ok()
        } else {
            None
        }
    }

    fn test_default(&self, tf_rdr: Rc<RefCell<BufReader<File>>>) {}

    fn test_string(&self, tf_rdr: Rc<RefCell<BufReader<File>>>) {
        //let value = &self.value.s;
    }

    fn test_pstring(&self, tf_rdr: Rc<RefCell<BufReader<File>>>) {}

    fn test_date(&self, tf_rdr: Rc<RefCell<BufReader<File>>>) {}

    fn test_long(&self) {}

    fn test_name(&self) {}

    fn get_continuation_lvl(&self) -> u16 {
        self.cont_level
    }

    fn get_type(&self) -> u8 {
        self._type
    }

    pub fn get_flag(&self) -> u8 {
        self.flag
    }

    /// Offset to the magic number
    fn get_offset(&self) -> i32 {
        self.offset
    }

    fn get_desc(&self) -> Result<CString, Box<dyn std::error::Error>> {
        let desc = CStr::from_bytes_until_nul(self.desc.as_slice())?;
        Ok(desc.to_owned())
    }

    fn get_mime_type(&self) -> Result<CString, Box<dyn std::error::Error>> {
        let mime_type = CStr::from_bytes_until_nul(self.mimetype.as_slice())?;
        Ok(mime_type.to_owned())
    }

    pub fn set_mime(&mut self, mime: &str) {
        let mut buf = [0_u8; MAX_MIME];
        if mime.len() > MAX_MIME {
            todo!("throw some error")
        }
        buf.copy_from_slice(mime.as_bytes())
    }

    pub fn set_apple(&mut self, apple: &str) {
        let mut buf = [0_u8; 8];
        if apple.len() > 8 {
            todo!("throw some error")
        }
        buf.copy_from_slice(apple.as_bytes())
    }

    pub fn set_ext(&mut self, ext: &str) {
        let mut buf = [0_u8; 64];
        if ext.len() > 64 {
            todo!("throw some error")
        }
        buf.copy_from_slice(ext.as_bytes())
    }

    fn set_strength(&mut self) {}
}

/// It auto detects the radix and consumes the first bytes that it can parse
///
fn parse_and_consume_num<T: Num>(input: &mut String) -> Option<T> {
    if let Some(hex_num) = parse_hexadecimal(input) {
        Some(hex_num)
    } else if let Some(oct_num) = parse_octal(input) {
        Some(oct_num)
    } else if let Some(decimal) = parse_decimal(input) {
        Some(decimal)
    } else {
        None
    }
}

fn parse_hexadecimal<T: Num>(input: &mut String) -> Option<T> {
    let re = Regex::new(r"^0[xX]([0-9A-F]+)").ok()?;

    let _input = input.clone();
    let captures = re.captures(&_input)?;
    let expr_match = captures.get(0)?;
    let group_match = captures.get(1)?;

    *input = input.replacen(expr_match.as_str(), "", 1);
    T::from_str_radix(group_match.as_str(), 16).ok()
}

fn parse_octal<T: Num>(input: &mut String) -> Option<T> {
    let re = Regex::new(r"^(0[0-7]+)").ok()?;
    let _input = input.clone();
    let captures = re.captures(&_input)?;
    let expr_match = captures.get(0)?;
    let group_match = captures.get(1)?;

    *input = input.replacen(expr_match.as_str(), "", 1);
    T::from_str_radix(group_match.as_str(), 8).ok()
}

fn parse_decimal<T: Num>(input: &mut String) -> Option<T> {
    let re = Regex::new(r"^(-?\d+)").ok()?;

    let _input = input.clone();
    let captures = re.captures(&_input)?;
    let expr_match = captures.get(0)?;

    *input = input.replacen(expr_match.as_str(), "", 1);
    T::from_str_radix(expr_match.as_str(), 10).ok()
}

//fn parse_hexadecimal(input: &mut String) -> Option<u64> {

//}
pub struct CompiledMagicFile {
    cmf_metadata: Metadata,
    /// [BufReader<File>] for compiled magic file
    cmf_reader: Rc<RefCell<BufReader<File>>>,

    /// [BufReader<File>] for test file
    tf_reader: Rc<RefCell<BufReader<File>>>,
}

impl CompiledMagicFile {
    /// It parses and tests as it goes on
    pub fn parse_and_test<P: AsRef<Path>>(
        magic_file: P,
        test_file: P,
    ) -> Result<String, CompiledMagicFileError> {
        if !magic_file.as_ref().exists() {
            return Err(CompiledMagicFileError::MagicFileNotFound);
        }

        if !test_file.as_ref().exists() {
            return Err(CompiledMagicFileError::TestFileNotFound);
        }

        let compiled_magic_file = File::open(magic_file.as_ref()).unwrap();
        let cmf_metadata = compiled_magic_file.metadata().unwrap();
        let cmf_reader = Rc::new(RefCell::new(BufReader::new(compiled_magic_file)));

        let test_file = File::open(test_file.as_ref()).unwrap();
        let tf_reader = Rc::new(RefCell::new(BufReader::new(test_file)));

        let mut cmf = CompiledMagicFile {
            cmf_metadata,
            cmf_reader,
            tf_reader,
        };

        // the expected value is given in little endian order
        if !cmf.check_magic_number_header(COMPILED_MAGIC_FILE_MAGIC_NUM) {
            return Err(CompiledMagicFileError::InvalidMagicHeader);
        }

        if !cmf.check_version_number(COMPILED_MAGIC_FILE_VERSION_NO) {
            return Err(CompiledMagicFileError::InvalidVersionNumber);
        }

        if !cmf.check_if_size_of_struct_magic_is_same_as_MAGIC_STRUCT_SIZE()
            || !cmf.check_if_size_of_file_is_multiple_of_struct_magic()
        {
            return Err(CompiledMagicFileError::InvalidSize);
        }

        if !cmf.check_if_magic_set_count_and_entry_match() {
            return Err(CompiledMagicFileError::InvalidNumberOfEntries);
        }

        let mut count = 0;
        let mut cont_lvl = 0;
        let message: Option<String> = None;
        for magic in cmf.get_magic_structs() {
            let magic = magic.unwrap();

            let x = magic.get_flag();
            let y = magic.get_desc().unwrap();

            //let x = magic.get_mime_type().unwrap();
            //let y = magic.get_desc().unwrap();

            //if x.to_str().unwrap().len() != 0 {
            //println!("{x:?} and {y:?}");
            //}

            //magic.test(cmf.tf_reader.clone());

            ////// it's like we go on next line but in the next line
            ////if magic.get_continuation_lvl() == 0 && message.is_some() {
            ////    return Ok(message.unwrap());
            ////} else {
            ////}

            //if (magic.get_continuation_lvl() == 0) {
            //    cont_lvl = 0;
            //}

            //if magic.get_continuation_lvl() > cont_lvl && message.is_some() {}
            //println!("{}", magic.get_continuation_lvl());
            //count += 1;
            //if count == 30 {
            //panic!()
            //}

            // Bring back the cursor in test file to stating
            //let mut tf_reader = cmf.tf_reader.borrow_mut();
            //tf_reader.rewind();
        }

        if message.is_some() {
            return Ok(message.unwrap());
        }

        Ok(String::from("Hey"))
    }

    /// Check if the magic number on this compiled magic file matches
    ///
    /// It's the first 4 bytes
    fn check_magic_number_header(&self, expected: u32) -> bool {
        let mut reader = self.cmf_reader.borrow_mut();
        reader.seek(SeekFrom::Start(0)).unwrap();

        let mut buf = [0u8; 4];

        reader.read_exact(&mut buf).unwrap();
        let magic_n = u32::from_le_bytes(buf);

        expected == magic_n
    }

    /// Check the version number
    ///
    /// If the 4 bytes starting from offset position 4 on the file
    fn check_version_number(&self, expected: u32) -> bool {
        let mut reader = self.cmf_reader.borrow_mut();
        reader.seek(SeekFrom::Start(4)).unwrap();

        let mut buf = [0u8; 4];
        reader.read_exact(&mut buf).unwrap();

        let version_n = u32::from_le_bytes(buf);

        expected == version_n
    }

    /// Check if the size of "struct Magic" is same as the MAGIC_STRUCT_SIZE constant
    /// that we have
    ///
    /// As the compiled magic file is simply the contents of struct Magic being laid
    /// in a certain patter, this is very important
    #[allow(non_snake_case)]
    fn check_if_size_of_struct_magic_is_same_as_MAGIC_STRUCT_SIZE(&self) -> bool {
        size_of::<Magic>() as u16 == MAGIC_STRUCT_SIZE
    }

    /// Check if the size of the file  is multiple of the "struct Magic"
    ///
    /// We'll use both the [Magic] and [MAGIC_STRUCT_SIZE] considering the equality
    fn check_if_size_of_file_is_multiple_of_struct_magic(&self) -> bool {
        let mutiple = self.cmf_metadata.len() / size_of::<Magic>() as u64;
        mutiple * MAGIC_STRUCT_SIZE as u64 == self.cmf_metadata.len()
    }
    fn check_if_magic_set_count_and_entry_match(&self) -> bool {
        let ascii_magic_entry_count = self.get_ascii_magic_entry_counts();
        let binary_magic_entry_count = self.get_binary_magic_entry_counts();

        let nentries = self.get_magic_structs().count() as u32;

        nentries == (ascii_magic_entry_count + binary_magic_entry_count)
    }

    /// Check the ascii magic entry count(For now all i know is that means no of entries that were
    /// on the raw magic file)
    ///
    /// If the 4 bytes starting from offset position 8 on the file
    fn get_ascii_magic_entry_counts(&self) -> u32 {
        let mut reader = self.cmf_reader.borrow_mut();
        reader.seek(SeekFrom::Start(8)).unwrap();

        let mut buf = [0u8; 4];
        reader.read_exact(&mut buf).unwrap();

        u32::from_le_bytes(buf)
    }

    /// Check the binary magic entry count(I don't know what this is, yet to figure out)
    ///
    /// If the 4 bytes starting from offset position 12 on the file
    fn get_binary_magic_entry_counts(&self) -> u32 {
        let mut reader = self.cmf_reader.borrow_mut();
        reader.seek(SeekFrom::Start(12)).unwrap();

        let mut buf = [0u8; 4];
        reader.read_exact(&mut buf).unwrap();

        u32::from_le_bytes(buf)
    }

    fn get_magic_structs(&self) -> MagicStructIterator {
        MagicStructIterator::new(self.cmf_reader.clone())
    }
}

struct MagicStructIterator {
    offset: u64,
    reader: Rc<RefCell<BufReader<File>>>,
}

impl MagicStructIterator {
    fn new(reader: Rc<RefCell<BufReader<File>>>) -> Self {
        MagicStructIterator {
            reader,
            offset: 376,
        }
    }
}

impl Iterator for MagicStructIterator {
    type Item = Result<Magic, Box<dyn std::error::Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut reader = self.reader.borrow_mut();
        reader.seek(SeekFrom::Start(self.offset)).unwrap();

        let mut buf = vec![0; 376];

        match reader.read_exact(&mut buf) {
            Err(_) => None,
            _ => {
                self.offset = self.offset + 376;
                Some(Magic::from_binary(&buf))
            }
        }
    }
}
