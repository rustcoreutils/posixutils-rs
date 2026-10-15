//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU m4's `format`: C `printf` conversions applied to macro arguments.
//!
//! Unlike the `printf` utility, the format is used once and has no
//! backslash escapes; a missing argument is the empty string or zero.

use std::ffi::{c_char, c_int, CString};

/// The conversion flags C `printf` accepts.
const FLAGS: &[u8] = b"-+ #0'";

/// Format `args` by `format`. On a malformed conversion, the output so far
/// and the diagnostic, in GNU m4's words.
pub fn format(format: &[u8], args: &[Vec<u8>]) -> Result<Vec<u8>, (Vec<u8>, String)> {
    let mut out = Vec::new();
    let mut args = args.iter().map(Vec::as_slice);
    let mut i = 0;
    while i < format.len() {
        let c = format[i];
        i += 1;
        if c != b'%' {
            out.push(c);
            continue;
        }
        if format.get(i) == Some(&b'%') {
            out.push(b'%');
            i += 1;
            continue;
        }
        let start = i;
        while format.get(i).is_some_and(|c| FLAGS.contains(c)) {
            i += 1;
        }
        let flags = &format[start..i];
        let width = if format.get(i) == Some(&b'*') {
            i += 1;
            Some(int_arg(args.next()))
        } else {
            digits(format, &mut i).map(|n| n as i64)
        };
        let precision = if format.get(i) == Some(&b'.') {
            i += 1;
            if format.get(i) == Some(&b'*') {
                i += 1;
                // A negative precision is taken as if omitted.
                Some(int_arg(args.next())).filter(|p| *p >= 0)
            } else {
                Some(digits(format, &mut i).unwrap_or(0) as i64)
            }
        } else {
            None
        };
        // Length modifiers mean nothing here: every integer is a C long.
        while format.get(i).is_some_and(|c| b"hlLqjzt".contains(c)) {
            i += 1;
        }
        let Some(&conversion) = format.get(i) else {
            let text = String::from_utf8_lossy(format).into_owned();
            return Err((out, format!("unrecognized specifier in `{text}'")));
        };
        i += 1;
        let spec = Spec {
            flags,
            width,
            precision,
        };
        match conversion {
            b's' => spec.pad(&mut out, truncate(args.next().unwrap_or(b""), precision)),
            b'c' => spec.pad(&mut out, &[int_arg(args.next()) as u8]),
            b'd' | b'i' => {
                let value = int_arg(args.next()) as libc::c_long;
                spec.c_printf(&mut out, b"ld", |f, buf, n| unsafe {
                    libc::snprintf(buf, n, f, value)
                });
            }
            b'o' | b'u' | b'x' | b'X' => {
                let value = int_arg(args.next()) as libc::c_ulong;
                let conv = [b'l', conversion];
                spec.c_printf(&mut out, &conv, |f, buf, n| unsafe {
                    libc::snprintf(buf, n, f, value)
                });
            }
            b'e' | b'E' | b'f' | b'F' | b'g' | b'G' | b'a' | b'A' => {
                let value = float_arg(args.next());
                spec.c_printf(&mut out, &[conversion], |f, buf, n| unsafe {
                    libc::snprintf(buf, n, f, value)
                });
            }
            _ => {
                let text = String::from_utf8_lossy(format).into_owned();
                return Err((out, format!("unrecognized specifier in `{text}'")));
            }
        }
    }
    Ok(out)
}

/// One conversion's flags, field width and precision.
struct Spec<'a> {
    flags: &'a [u8],
    width: Option<i64>,
    precision: Option<i64>,
}

impl Spec<'_> {
    /// Write `text` padded to the field width with spaces, on the left
    /// unless the `-` flag (or a negative `*` width) says the right.
    fn pad(&self, out: &mut Vec<u8>, text: &[u8]) {
        let width = self.width.unwrap_or(0);
        let left = self.flags.contains(&b'-') || width < 0;
        let fill = (width.unsigned_abs() as usize).saturating_sub(text.len());
        if !left {
            out.resize(out.len() + fill, b' ');
        }
        out.extend_from_slice(text);
        if left {
            out.resize(out.len() + fill, b' ');
        }
    }

    /// Write a numeric conversion, done by the C library's `snprintf` so
    /// that every flag means what it means to C.
    fn c_printf(
        &self,
        out: &mut Vec<u8>,
        conversion: &[u8],
        print: impl Fn(*const c_char, *mut c_char, usize) -> c_int,
    ) {
        let mut spec = b"%".to_vec();
        spec.extend_from_slice(self.flags);
        if let Some(width) = self.width {
            if width < 0 {
                spec.push(b'-');
            }
            spec.extend_from_slice(width.unsigned_abs().to_string().as_bytes());
        }
        if let Some(precision) = self.precision {
            spec.push(b'.');
            spec.extend_from_slice(precision.to_string().as_bytes());
        }
        spec.extend_from_slice(conversion);
        let spec = CString::new(spec).expect("no NUL in a conversion");
        let needed = print(spec.as_ptr(), std::ptr::null_mut(), 0);
        let Ok(needed) = usize::try_from(needed) else {
            return;
        };
        let mut buf = vec![0u8; needed + 1];
        print(spec.as_ptr(), buf.as_mut_ptr().cast(), buf.len());
        buf.truncate(needed);
        out.extend_from_slice(&buf);
    }
}

/// `text` cut to `precision` bytes, as C's `%.Ns` does.
fn truncate(text: &[u8], precision: Option<i64>) -> &[u8] {
    match precision {
        Some(p) => &text[..text.len().min(p as usize)],
        None => text,
    }
}

/// Read a run of decimal digits at `format[*i]`, if there is one.
fn digits(format: &[u8], i: &mut usize) -> Option<usize> {
    let start = *i;
    while format.get(*i).is_some_and(u8::is_ascii_digit) {
        *i += 1;
    }
    std::str::from_utf8(&format[start..*i]).ok()?.parse().ok()
}

/// An argument as an integer, the way C's `strtol` reads it: leading
/// blanks, a sign, then decimal digits; anything else ends it. A missing
/// argument is 0.
fn int_arg(arg: Option<&[u8]>) -> i64 {
    let arg = arg.unwrap_or(b"");
    let text = arg.trim_ascii_start();
    let (negative, digits) = match text.split_first() {
        Some((b'-', rest)) => (true, rest),
        Some((b'+', rest)) => (false, rest),
        _ => (false, text),
    };
    let value = digits
        .iter()
        .take_while(|c| c.is_ascii_digit())
        .fold(0i64, |n, c| {
            n.wrapping_mul(10).wrapping_add(i64::from(c - b'0'))
        });
    if negative {
        value.wrapping_neg()
    } else {
        value
    }
}

/// An argument as a floating-point number, by C's `strtod`. A missing
/// argument is 0.
fn float_arg(arg: Option<&[u8]>) -> f64 {
    let Ok(text) = CString::new(arg.unwrap_or(b"")) else {
        return 0.0;
    };
    unsafe { libc::strtod(text.as_ptr(), std::ptr::null_mut()) }
}

#[cfg(test)]
mod tests {
    use super::format;

    fn f(format_text: &str, args: &[&str]) -> String {
        let args: Vec<Vec<u8>> = args.iter().map(|a| a.as_bytes().to_vec()).collect();
        String::from_utf8(format(format_text.as_bytes(), &args).unwrap()).unwrap()
    }

    #[test]
    fn strings() {
        assert_eq!(f("[%s]", &["abc"]), "[abc]");
        assert_eq!(f("[%*s]", &["5", "ab"]), "[   ab]");
        assert_eq!(f("[%-5s]", &["ab"]), "[ab   ]");
        assert_eq!(f("[%*s]", &["-4", "ab"]), "[ab  ]");
        assert_eq!(f("[%.*s]", &["2", "abcdef"]), "[ab]");
        assert_eq!(f("[%.1s]", &["xyz"]), "[x]");
        assert_eq!(f("[%*s]", &["3", ""]), "[   ]");
        assert_eq!(f("[%s]", &[]), "[]");
    }

    #[test]
    fn numbers() {
        assert_eq!(
            f("%d %i %5d|%-3d|", &["12", "-3", "42", "7"]),
            "12 -3    42|7  |"
        );
        assert_eq!(
            f("%x %X %o %05d", &["255", "255", "8", "42"]),
            "ff FF 10 00042"
        );
        assert_eq!(f("%c%c", &["65", "66"]), "AB");
        assert_eq!(f("%.2f %e", &["3.14159", "100"]), "3.14 1.000000e+02");
        assert_eq!(f("100%%", &[]), "100%");
    }
}
