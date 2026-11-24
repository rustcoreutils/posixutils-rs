//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! SCCS identification keywords.
//!
//! `get` scanned a `&str` and `admin` scanned a `&[u8]` for the same `%X%`
//! pattern, with bodies identical modulo the input type. Scanning bytes serves
//! both: a keyword is ASCII, and an s-file body need not be UTF-8.

use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::Path;

use plib::sccsfile::{paths, DeltaEntry, SccsDateTime, SccsFile};

/// Whether `data` contains an identification keyword of the form `%X%`, where
/// X is an uppercase ASCII letter.
pub fn contains(data: &[u8]) -> bool {
    data.windows(3)
        .any(|w| w[0] == b'%' && w[1].is_ascii_uppercase() && w[2] == b'%')
}

/// The value of each identification keyword `%X%` for one retrieval, indexed
/// by letter; `None` for a letter that is not a keyword. `%C%`, the line
/// number, is the one value that changes from line to line, so it is filled
/// in by [`expand`] itself.
///
/// Built once per file: the values do not depend on the line, and `%P%`
/// resolves the s-file's absolute path, which is not something to redo for
/// every line of the body.
pub struct KeywordValues([Option<Vec<u8>>; 26]);

impl KeywordValues {
    pub fn new(sccs_file: &SccsFile, delta: &DeltaEntry, sfile_path: &Path) -> Self {
        let module_name = module_name_of(sccs_file, sfile_path);
        let module_type = sccs_file.module_type().unwrap_or(b"");
        let q_value = sccs_file.q_text().unwrap_or(b"");
        let now = SccsDateTime::now();
        // Names are bytes, and go into the text as they are.
        let sccs_filename = sfile_path.file_name().unwrap_or_default().as_bytes();
        let abs_path = std::fs::canonicalize(sfile_path)
            .unwrap_or_else(|_| sfile_path.to_path_buf())
            .into_os_string()
            .into_vec();
        let mmddyy = |d: &SccsDateTime| format!("{:02}/{:02}/{:02}", d.month, d.day, d.year);

        let mut values: [Option<Vec<u8>>; 26] = Default::default();
        let mut set = |key: u8, value: Vec<u8>| values[(key - b'A') as usize] = Some(value);
        // "@(#)<a><sep><b>...", the shape of %W% and %A%: text and stored
        // bytes joined.
        let what = |parts: &[&[u8]]| -> Vec<u8> {
            let mut v = b"@(#)".to_vec();
            parts.iter().for_each(|p| v.extend_from_slice(p));
            v
        };
        let sid = delta.sid.to_string();
        set(b'Z', b"@(#)".to_vec());
        set(b'M', module_name.clone());
        set(b'I', sid.clone().into_bytes());
        set(b'R', delta.sid.rel.to_string().into_bytes());
        set(b'L', delta.sid.lev.to_string().into_bytes());
        set(b'B', delta.sid.br.to_string().into_bytes());
        set(b'S', delta.sid.seq.to_string().into_bytes());
        set(b'D', now.date_string().into_bytes());
        set(b'H', mmddyy(&now).into_bytes());
        set(b'T', now.time_string().into_bytes());
        set(b'E', delta.datetime.date_string().into_bytes());
        set(b'G', mmddyy(&delta.datetime).into_bytes());
        set(b'U', delta.datetime.time_string().into_bytes());
        set(b'Y', module_type.to_vec());
        set(b'F', sccs_filename.to_vec());
        set(b'P', abs_path);
        set(b'Q', q_value.to_vec());
        set(b'W', what(&[&module_name, b"\t", sid.as_bytes()]));
        set(
            b'A',
            what(&[
                module_type,
                b" ",
                &module_name,
                b" ",
                sid.as_bytes(),
                b"@(#)",
            ]),
        );
        KeywordValues(values)
    }
}

/// The module name: the `m` flag, else the s-file's name without its `s.`.
pub fn module_name_of(sccs: &SccsFile, sfile_path: &Path) -> Vec<u8> {
    match sccs.module_name() {
        Some(name) => name.to_vec(),
        None => paths::module_name(sfile_path).unwrap_or_default().to_vec(),
    }
}

/// Expand the identification keywords in one body line, `line_number` being
/// its 1-based position (`%C%`).
///
/// One pass, left to right: each `%X%` is replaced by its value and the scan
/// resumes after it, so a value is never itself expanded -- `%M%` with an `m`
/// flag of `mod%R%` yields `mod%R%`, as CSSC writes it. Expanding by a chain
/// of whole-line replacements re-expanded any keyword a value contained. A
/// `%X%` that is not a keyword is left as it is.
pub fn expand(line: &[u8], values: &KeywordValues, line_number: usize) -> Vec<u8> {
    let mut out = Vec::with_capacity(line.len());
    let mut i = 0;
    while i < line.len() {
        if let [b'%', key @ b'A'..=b'Z', b'%', ..] = line[i..] {
            let value = if key == b'C' {
                Some(line_number.to_string().into_bytes())
            } else {
                values.0[(key - b'A') as usize].clone()
            };
            if let Some(value) = value {
                out.extend_from_slice(&value);
                i += 3;
                continue;
            }
        }
        out.push(line[i]);
        i += 1;
    }
    out
}
