//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The s-file as a whole: reading, writing, the delta graph, the body.

use std::collections::HashSet;
use std::io::Read;
use std::path::Path;

use super::parse::SccsParser;
use super::{
    DeltaEntry, DeltaType, Kind, Line, Rec, ReleaseLock, SccsError, SccsFlag, SccsHeader,
    SccsResult, Sid, Walker,
};

/// What a command line adds to the choice of a version: the deltas `-i`
/// forces in and `-x` forces out, and the `-c` cutoff.
#[derive(Default, Clone, Copy)]
pub struct Selection<'a> {
    /// Serials named by `-i`.
    pub included: &'a [u16],
    /// Serials named by `-x`.
    pub excluded: &'a [u16],
    /// Whether a delta was created after the `-c` cutoff, if there is one.
    pub cutoff: Option<&'a dyn Fn(&DeltaEntry) -> bool>,
}

/// An s-file.
///
/// It is the file's bytes, kept exactly as they were read, an index of the
/// lines in them, and a parsed view of the header derived from those lines.
/// Nothing here regenerates the file from the view: an unchanged file is
/// written back byte for byte, and a change (the `edit` module holds them
/// all) replaces only the lines it names, as historical SCCS does when it
/// copies an s-file to its x-file. Regenerating it instead rewrote spacing,
/// reordered records and dropped every line the parser reads around.
///
/// The fields are private, so there is no way to change the view without
/// the bytes or the bytes without the view.
#[derive(Debug, Clone)]
pub struct SccsFile {
    /// The file, as read or as last edited.
    buf: Vec<u8>,
    /// Every line of `buf` from the checksum line on, in order.
    lines: Vec<Line>,
    /// The index in `lines` of the first line of the body.
    body_start: usize,
    /// The header those lines hold.
    header: SccsHeader,
}

/// The body of an s-file: its lines, for the weave to walk.
#[derive(Debug, Clone, Copy)]
pub struct Body<'a> {
    buf: &'a [u8],
    lines: &'a [Line],
    /// The highest serial in the delta table: a block marker can name no
    /// higher one.
    max_serial: u16,
}

impl<'a> Body<'a> {
    /// How many lines it has.
    pub fn len(&self) -> usize {
        self.lines.len()
    }

    /// Whether it has no lines.
    pub fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }

    /// The highest serial a marker may name.
    pub(super) fn max_serial(&self) -> u16 {
        self.max_serial
    }

    /// Line `index`: what it is, and its bytes.
    pub(super) fn line(&self, index: usize) -> Option<(Kind, &'a [u8])> {
        let line = self.lines.get(index)?;
        Some((line.kind, &self.buf[line.start..line.start + line.len]))
    }
}

impl SccsFile {
    /// The parser's result: the image it read and what it found there.
    pub(super) fn from_parts(
        buf: Vec<u8>,
        lines: Vec<Line>,
        body_start: usize,
        header: SccsHeader,
    ) -> Self {
        SccsFile {
            buf,
            lines,
            body_start,
            header,
        }
    }

    /// The header: the delta table, the user list, the flags and the
    /// descriptive text.
    pub fn header(&self) -> &SccsHeader {
        &self.header
    }

    /// The body, for the weave.
    pub fn body(&self) -> Body<'_> {
        Body {
            buf: &self.buf,
            lines: &self.lines[self.body_start..],
            max_serial: self
                .header
                .deltas
                .iter()
                .map(|d| d.serial)
                .max()
                .unwrap_or(0),
        }
    }

    /// Check that the weave holds together: every block marker usable, every
    /// block opened once and closed. A body that passes can be read for any
    /// version.
    pub fn check_weave(&self) -> SccsResult<()> {
        let nothing_applied = HashSet::new();
        Walker::new(self.body(), &nothing_applied).try_for_each(|step| step.map(|_| ()))
    }

    /// The lines of the header after the checksum line, each with its kind.
    pub(super) fn header_lines(&self) -> impl Iterator<Item = (Kind, &[u8])> {
        self.lines[1.min(self.body_start)..self.body_start]
            .iter()
            .map(|l| (l.kind, &self.buf[l.start..l.start + l.len]))
    }

    /// The lines of the body, each with its kind.
    pub(super) fn body_lines(&self) -> impl Iterator<Item = (Kind, &[u8])> {
        self.lines[self.body_start..]
            .iter()
            .map(|l| (l.kind, &self.buf[l.start..l.start + l.len]))
    }

    /// Parse SCCS file from bytes
    pub fn from_bytes(data: &[u8]) -> SccsResult<Self> {
        SccsParser::new(data, false).parse()
    }

    /// Parse SCCS file from bytes, refusing any line no record accounts for.
    ///
    /// [`from_bytes`](Self::from_bytes) reads around a stray line in the
    /// delta table, in a delta entry or among the flags, so that `get` and
    /// `prs` still work on a damaged file; this is for `admin -h` and `val`,
    /// which "check the structure of the SCCS file" (POSIX admin 84107) and
    /// must report it.
    pub fn from_bytes_strict(data: &[u8]) -> SccsResult<Self> {
        SccsParser::new(data, true).parse()
    }

    /// Parse SCCS file from a reader
    pub fn from_reader<R: Read>(mut reader: R) -> SccsResult<Self> {
        let mut data = Vec::new();
        reader.read_to_end(&mut data)?;
        Self::from_bytes(&data)
    }

    /// Parse SCCS file from a file path
    pub fn from_path<P: AsRef<Path>>(path: P) -> SccsResult<Self> {
        let data = std::fs::read(path)?;
        Self::from_bytes(&data)
    }

    /// The file's bytes: exactly those read, or those of the last edit.
    pub fn to_bytes(&self) -> Vec<u8> {
        self.buf.clone()
    }

    // =========================================================================
    // Delta Graph Operations
    // =========================================================================

    /// Find delta by serial number
    pub fn find_delta_by_serial(&self, serial: u16) -> Option<&DeltaEntry> {
        self.header.deltas.iter().find(|d| d.serial == serial)
    }

    /// Serials of every delta on the ancestor chain from `end` back to
    /// `start`, inclusive.
    ///
    /// This is what a `lo-hi` element of an SCCS list option-argument denotes
    /// (`get -i`/`-x`, `delta -g`). Returns `None` when either SID is unknown
    /// or `start` is not an ancestor of `end` — the case the spec singles out
    /// for a diagnostic.
    pub fn ancestor_chain(&self, start: &Sid, end: &Sid) -> Option<Vec<u16>> {
        let start = self.find_delta_by_sid(start)?;
        let end = self.find_delta_by_sid(end)?;

        let mut serials = Vec::new();
        let mut cur = end.serial;
        loop {
            serials.push(cur);
            if cur == start.serial {
                return Some(serials);
            }
            match self.find_delta_by_serial(cur) {
                Some(d) if d.pred_serial != 0 => cur = d.pred_serial,
                _ => return None,
            }
        }
    }

    /// Find delta by SID
    pub fn find_delta_by_sid(&self, sid: &Sid) -> Option<&DeltaEntry> {
        self.live_deltas().find(|d| &d.sid == sid)
    }

    /// The deltas the file has: those not removed by `rmdel`.
    ///
    /// rmdel "shall remove the delta" (POSIX). Its entry stays in the delta
    /// table, as type `R`, for the record only: it has no text, it is no
    /// version's successor, and its SID is free -- the next delta made from
    /// its predecessor takes it, as in historical SCCS and CSSC, so a file
    /// may hold a removed and a live entry with one SID. Anything that asks
    /// what versions exist asks this.
    pub fn live_deltas(&self) -> impl Iterator<Item = &DeltaEntry> {
        self.header
            .deltas
            .iter()
            .filter(|d| d.delta_type == DeltaType::Normal)
    }

    /// Get the highest (most recent) delta on trunk
    pub fn get_trunk_head(&self) -> Option<&DeltaEntry> {
        self.header
            .deltas
            .iter()
            .filter(|d| d.sid.is_trunk() && d.delta_type == DeltaType::Normal)
            .max_by(|a, b| a.sid.cmp(&b.sid))
    }

    /// Get the highest serial number in use
    pub fn max_serial(&self) -> u16 {
        self.header
            .deltas
            .iter()
            .map(|d| d.serial)
            .max()
            .unwrap_or(0)
    }

    /// The deltas applied for a version: the ones whose changes make it up.
    ///
    /// `target` is the delta retrieved; `selection` is what the command line
    /// adds (`-i`, `-x`, `-c`). Each delta is decided once, and the first
    /// decision stands, as in historical SCCS (`setup`):
    ///
    /// 1. The command line: a delta named by `-i` is applied, one named by
    ///    `-x` is not. Named by both, it is applied ("forced to be applied",
    ///    POSIX get 99084).
    /// 2. Then, from the newest delta to the oldest: a delta in the line of
    ///    predecessors from the target is applied, unless `-c` cuts it off;
    ///    any other is not. And each delta that is applied decides, for the
    ///    deltas its own lists name: its included ones are applied, its
    ///    excluded and ignored ones are not.
    ///
    /// Going from newest to oldest is what lets a later delta's lists
    /// override an earlier one's, and both override the line of
    /// predecessors. An included delta brings its own lists with it, but not
    /// its predecessors: including 1.3 into 1.1 applies 1.3, not 1.2.
    pub fn applied_set(&self, target: u16, selection: &Selection<'_>) -> SccsResult<HashSet<u16>> {
        if !self.live_deltas().any(|d| d.serial == target) {
            return Err(SccsError::InvalidSerial(format!(
                "Serial {} not found",
                target
            )));
        }
        // Decided deltas: applied or not.
        let mut decided: std::collections::HashMap<u16, bool> = std::collections::HashMap::new();
        for &serial in selection.included {
            decided.entry(serial).or_insert(true);
        }
        for &serial in selection.excluded {
            decided.entry(serial).or_insert(false);
        }

        let mut newest_first: Vec<&DeltaEntry> = self.live_deltas().collect();
        newest_first.sort_by_key(|d| std::cmp::Reverse(d.serial));
        let mut in_line: HashSet<u16> = HashSet::from([target]);
        for delta in newest_first {
            let on_the_line = in_line.contains(&delta.serial);
            if on_the_line {
                in_line.insert(delta.pred_serial);
            }
            let cut_off = selection.cutoff.is_some_and(|after| after(delta));
            let applied = *decided
                .entry(delta.serial)
                .or_insert(on_the_line && !cut_off);
            if applied {
                for &serial in &delta.included {
                    decided.entry(serial).or_insert(true);
                }
                for &serial in delta.excluded.iter().chain(&delta.ignored) {
                    decided.entry(serial).or_insert(false);
                }
            }
        }

        Ok(self
            .live_deltas()
            .map(|d| d.serial)
            .filter(|serial| decided.get(serial) == Some(&true))
            .collect())
    }

    /// [`applied_set`](Self::applied_set) for the version as recorded: no
    /// `-i`, `-x` or `-c`.
    pub fn compute_applied_set(&self, target: u16) -> SccsResult<HashSet<u16>> {
        self.applied_set(target, &Selection::default())
    }

    // =========================================================================
    // Body Evaluation
    // =========================================================================

    /// The text of the version made of the deltas `applied_set`, as its
    /// lines. A damaged weave is an error: there is no reading it.
    pub fn evaluate_body(&self, applied_set: &HashSet<u16>) -> SccsResult<Vec<Vec<u8>>> {
        Ok(self
            .evaluate_body_annotated(applied_set)?
            .into_iter()
            .map(|(_, line)| line)
            .collect())
    }

    /// [`evaluate_body`](Self::evaluate_body), each line with the serial of
    /// the delta that inserted it (`get -m`).
    pub fn evaluate_body_annotated(
        &self,
        applied_set: &HashSet<u16>,
    ) -> SccsResult<Vec<(u16, Vec<u8>)>> {
        let mut lines = Vec::new();
        for step in Walker::new(self.body(), applied_set) {
            if let (
                _,
                Rec::Text {
                    text,
                    kept_by: Some(serial),
                },
            ) = step?
            {
                lines.push((serial, text.to_vec()));
            }
        }
        Ok(lines)
    }

    // =========================================================================
    // Flag Access
    // =========================================================================

    /// Whether the body is uuencoded (the `e` flag is set non-zero).
    pub fn is_encoded(&self) -> bool {
        self.header
            .flags
            .iter()
            .any(|f| matches!(f, SccsFlag::Encoded(n) if *n != 0))
    }

    /// Check if branch flag is set
    pub fn branch_enabled(&self) -> bool {
        self.header
            .flags
            .iter()
            .any(|f| matches!(f, SccsFlag::BranchEnabled))
    }

    /// Get module name (from 'm' flag or derived from filename)
    pub fn module_name(&self) -> Option<&[u8]> {
        for flag in &self.header.flags {
            if let SccsFlag::ModuleName(name) = flag {
                return Some(name);
            }
        }
        None
    }

    /// Get module type (from 't' flag)
    pub fn module_type(&self) -> Option<&[u8]> {
        for flag in &self.header.flags {
            if let SccsFlag::ModuleType(t) = flag {
                return Some(t);
            }
        }
        None
    }

    /// Check if joint edit is allowed
    pub fn joint_edit(&self) -> bool {
        self.header
            .flags
            .iter()
            .any(|f| matches!(f, SccsFlag::JointEdit))
    }

    /// Ceiling release, the highest release `get -e` may retrieve (`c` flag,
    /// POSIX 84030-84032). "The default value for an unspecified c flag shall
    /// be 9 999."
    pub fn ceiling(&self) -> u16 {
        self.header
            .flags
            .iter()
            .find_map(|f| match f {
                SccsFlag::Ceiling(n) => Some(*n),
                _ => None,
            })
            .unwrap_or(9999)
    }

    /// Floor release, the lowest release `get -e` may retrieve (`f` flag,
    /// POSIX 84033-84035). "The default value for an unspecified f flag shall
    /// be 1."
    pub fn floor(&self) -> u16 {
        self.header
            .flags
            .iter()
            .find_map(|f| match f {
                SccsFlag::Floor(n) => Some(*n),
                _ => None,
            })
            .unwrap_or(1)
    }

    /// Default SID for a `get` with no `-r` (`d` flag, POSIX 84036).
    pub fn default_sid(&self) -> Option<Sid> {
        self.header.flags.iter().find_map(|f| match f {
            SccsFlag::DefaultSid(sid) => Some(*sid),
            _ => None,
        })
    }

    /// Releases closed to further deltas (`l` flag, POSIX 84047-84054).
    pub fn locked_releases(&self) -> Option<&ReleaseLock> {
        self.header.flags.iter().find_map(|f| match f {
            SccsFlag::LockedReleases(lock) => Some(lock),
            _ => None,
        })
    }

    /// MR-number validation program (`v` flag). The outer `Option` is "flag
    /// absent", the inner one "flag present with no program".
    pub fn mr_program(&self) -> Option<Option<&[u8]>> {
        self.header.flags.iter().find_map(|f| match f {
            SccsFlag::MrValidation(p) => Some(p.as_deref()),
            _ => None,
        })
    }

    /// User-defined `%Q%` text (`q` flag).
    pub fn q_text(&self) -> Option<&[u8]> {
        self.header.flags.iter().find_map(|f| match f {
            SccsFlag::QText(s) => Some(s.as_slice()),
            _ => None,
        })
    }

    /// ID-keyword check (`i` flag). The outer `Option` is "flag absent" (the
    /// missing-keyword message is only a warning); the inner one is the
    /// optional exact-match string. POSIX 84037-84043.
    pub fn id_keyword_check(&self) -> Option<Option<&[u8]>> {
        self.header.flags.iter().find_map(|f| match f {
            SccsFlag::IdKeywordError(s) => Some(s.as_deref()),
            _ => None,
        })
    }

    /// True if `serial` is `ancestor` itself, or has it in its predecessor
    /// chain. Used to cascade `-x` exclusions to descendant deltas for the
    /// `Excluded:` notation.
    pub fn is_descendant_of(&self, serial: u16, ancestor: u16) -> bool {
        let mut cur = serial;
        while cur != 0 {
            if cur == ancestor {
                return true;
            }
            match self.find_delta_by_serial(cur) {
                Some(d) => cur = d.pred_serial,
                None => break,
            }
        }
        false
    }

    /// Resolve a `-i`/`-x` list into delta serial numbers.
    ///
    /// The grammar is POSIX 99085-99087: `<list> ::= <range> | <list> , <range>`
    /// and `<range> ::= SID | SID - SID`. Separators are commas, spaces and
    /// tabs.
    ///
    /// A malformed range is a hard error: silently recording the wrong list
    /// would bake the mistake into the delta table. A token that merely names
    /// no such delta is returned as unresolved for the caller to report.
    pub fn resolve_sid_list(&self, list: &str) -> Result<(Vec<u16>, Vec<String>), String> {
        self.resolve_delta_list(list, false)
    }

    /// As [`Self::resolve_sid_list`], but a bare integer naming no SID is also
    /// accepted as a serial number.
    ///
    /// This is a `delta -g` convenience from historical SCCS, and it must not
    /// leak into `get -i`/`-x`: there, `-x2` means the SID `2`, and silently
    /// reading it as serial 2 hands the user a different version than they
    /// asked for with no diagnostic. CSSC treats `-x2` as naming no delta.
    pub fn resolve_ignore_list(&self, list: &str) -> Result<(Vec<u16>, Vec<String>), String> {
        self.resolve_delta_list(list, true)
    }

    fn resolve_delta_list(
        &self,
        list: &str,
        allow_serial: bool,
    ) -> Result<(Vec<u16>, Vec<String>), String> {
        let mut serials = Vec::new();
        let mut unresolved = Vec::new();

        for tok in list.split([',', ' ', '\t']).filter(|s| !s.is_empty()) {
            if let Some((lo, hi)) = tok.split_once('-') {
                let (lo_sid, hi_sid) = match (lo.parse::<Sid>(), hi.parse::<Sid>()) {
                    (Ok(l), Ok(h)) => (l, h),
                    _ => return Err(format!("{}: invalid SID range", tok)),
                };
                match self.ancestor_chain(&lo_sid, &hi_sid) {
                    Some(mut r) => serials.append(&mut r),
                    // "A diagnostic message shall be written if the first SID
                    // in the range is not an ancestor of the second SID."
                    None => {
                        return Err(format!(
                            "{}: first SID in range is not an ancestor of the second",
                            tok
                        ))
                    }
                }
                continue;
            }

            if let Ok(sid) = tok.parse::<Sid>() {
                if let Some(d) = self.find_delta_by_sid(&sid) {
                    serials.push(d.serial);
                    continue;
                }
            }

            match tok.parse::<u16>() {
                Ok(n) if allow_serial && self.find_delta_by_serial(n).is_some() => serials.push(n),
                _ => unresolved.push(tok.to_string()),
            }
        }

        Ok((serials, unresolved))
    }
}
