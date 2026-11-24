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
use std::io::{self, Read, Write};
use std::path::Path;

use super::parse::SccsParser;
use super::{
    compute_checksum, BlockKind, BodyRecord, DeltaEntry, DeltaType, Rec, ReleaseLock, SccsError,
    SccsFlag, SccsHeader, SccsResult, Sid, Walker,
};

/// Complete SCCS file representation
#[derive(Debug, Clone, Default)]
pub struct SccsFile {
    pub header: SccsHeader,
    pub body: Vec<BodyRecord>,
}

impl SccsFile {
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

    /// Serialize to bytes
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        self.write_to(&mut output).unwrap();
        output
    }

    /// Write to a writer
    pub fn write_to<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        // First, serialize content (everything after checksum line)
        let mut content = Vec::new();
        self.write_content(&mut content)?;

        // Compute checksum of content
        let checksum = compute_checksum(&content);

        // Write checksum line
        writeln!(writer, "\x01h{:05}", checksum)?;

        // Write content
        writer.write_all(&content)?;
        Ok(())
    }

    fn write_content<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        // Write delta table
        for delta in &self.header.deltas {
            // Stats line
            writeln!(writer, "\x01s {}", delta.stats)?;

            // Delta header line
            write!(
                writer,
                "\x01d {} {} {} {} ",
                delta.delta_type.to_char(),
                delta.sid,
                delta.datetime.date_string(),
                delta.datetime.time_string(),
            )?;
            writer.write_all(&delta.user)?;
            writeln!(writer, " {} {}", delta.serial, delta.pred_serial)?;

            // Included deltas
            if !delta.included.is_empty() {
                let serials: Vec<String> = delta.included.iter().map(|s| s.to_string()).collect();
                writeln!(writer, "\x01i {}", serials.join(" "))?;
            }

            // Excluded deltas
            if !delta.excluded.is_empty() {
                let serials: Vec<String> = delta.excluded.iter().map(|s| s.to_string()).collect();
                writeln!(writer, "\x01x {}", serials.join(" "))?;
            }

            // Ignored deltas
            if !delta.ignored.is_empty() {
                let serials: Vec<String> = delta.ignored.iter().map(|s| s.to_string()).collect();
                writeln!(writer, "\x01g {}", serials.join(" "))?;
            }

            // MR numbers
            for mr in &delta.mr_numbers {
                write_record(writer, b"\x01m ", mr)?;
            }

            // Comments
            for comment in &delta.comments {
                write_record(writer, b"\x01c ", comment)?;
            }

            // End of delta
            writeln!(writer, "\x01e")?;
        }

        // User list
        writeln!(writer, "\x01u")?;
        for user in &self.header.users {
            write_record(writer, b"", user)?;
        }
        writeln!(writer, "\x01U")?;

        // Flags
        for flag in &self.header.flags {
            let value = flag.value();
            if value.is_empty() {
                writeln!(writer, "\x01f {}", flag.flag_char())?;
            } else {
                write!(writer, "\x01f {} ", flag.flag_char())?;
                write_record(writer, b"", &value)?;
            }
        }

        // Descriptive text
        writeln!(writer, "\x01t")?;
        for line in &self.header.descriptive_text {
            write_record(writer, b"", line)?;
        }
        writeln!(writer, "\x01T")?;

        // Body
        for record in &self.body {
            match record {
                BodyRecord::Insert(serial) => writeln!(writer, "\x01I {}", serial)?,
                BodyRecord::Delete(serial) => writeln!(writer, "\x01D {}", serial)?,
                BodyRecord::End(serial) => writeln!(writer, "\x01E {}", serial)?,
                BodyRecord::Text(line) => write_record(writer, b"", line)?,
            }
        }

        Ok(())
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

    /// Compute the "applied set" - serial numbers of deltas needed to reconstruct a version
    pub fn compute_applied_set(&self, target_serial: u16) -> SccsResult<HashSet<u16>> {
        let mut applied = HashSet::new();
        let mut stack = vec![target_serial];
        let mut visited = HashSet::new();

        while let Some(serial) = stack.pop() {
            if visited.contains(&serial) || serial == 0 {
                continue;
            }
            visited.insert(serial);

            let delta = self
                .find_delta_by_serial(serial)
                .ok_or_else(|| SccsError::InvalidSerial(format!("Serial {} not found", serial)))?;

            // Skip removed deltas
            if delta.delta_type == DeltaType::Removed {
                continue;
            }

            // Add this delta
            applied.insert(serial);

            // Process predecessor
            if delta.pred_serial > 0 {
                stack.push(delta.pred_serial);
            }

            // Process included deltas
            for &inc in &delta.included {
                stack.push(inc);
            }
        }

        // Remove excluded deltas.
        //
        // Every delta reached contributes its exclusion list, not just the
        // target: an exclusion recorded on an ancestor is part of what that
        // ancestor's content *is*. Honoring it only at the target -- while
        // honoring `included` at every delta, since those are pushed during
        // the walk -- meant a delta made with `get -e -x` produced correct
        // content itself, and then its own successor resurrected the excluded
        // lines. That was latent for as long as delta recorded no exclusions.
        for serial in &visited {
            let Some(delta) = self.find_delta_by_serial(*serial) else {
                continue;
            };
            for &exc in &delta.excluded {
                applied.remove(&exc);
            }
        }

        // An inclusion outranks an exclusion, matching POSIX 99084 ("forced to
        // be applied") and `applied_set_with`, which layers the command-line
        // -i/-x on top of this set and must not resolve the overlap the other
        // way.
        for serial in &visited {
            let Some(delta) = self.find_delta_by_serial(*serial) else {
                continue;
            };
            for &inc in &delta.included {
                let mut cur = inc;
                while cur != 0 && applied.insert(cur) {
                    match self.find_delta_by_serial(cur) {
                        Some(d) => cur = d.pred_serial,
                        None => break,
                    }
                }
            }
        }

        Ok(applied)
    }

    // =========================================================================
    // Body Evaluation
    // =========================================================================

    /// Evaluate the body to produce file content for a given set of deltas
    pub fn evaluate_body(&self, applied_set: &HashSet<u16>) -> Vec<Vec<u8>> {
        Walker::new(&self.body, applied_set)
            .filter_map(|(_, rec)| match rec {
                Rec::Text {
                    text,
                    kept_by: Some(_),
                } => Some(text.to_vec()),
                _ => None,
            })
            .collect()
    }

    /// [`evaluate_body`](Self::evaluate_body), each line with the serial of
    /// the delta that inserted it (`get -m`).
    pub fn evaluate_body_annotated(&self, applied_set: &HashSet<u16>) -> Vec<(u16, Vec<u8>)> {
        Walker::new(&self.body, applied_set)
            .filter_map(|(_, rec)| match rec {
                Rec::Text {
                    text,
                    kept_by: Some(serial),
                } => Some((serial, text.to_vec())),
                _ => None,
            })
            .collect()
    }

    /// Remove a (leaf) delta: mark its delta-table entry as `Removed` and
    /// reweave the body so the delta's footprint is undone, matching historical
    /// SCCS / CSSC `rmdel`. The delta's own inserted block (`^AI s … ^AE s`) is
    /// dropped in full (control lines and text); each block it deleted
    /// (`^AD s … ^AE s`) has its control lines dropped but the wrapped text
    /// restored (un-deleted). The delta-table entry is retained (type `R`) so
    /// the SID cannot be reused. Callers must ensure the delta is a leaf.
    pub fn remove_delta(&mut self, serial: u16) {
        if let Some(d) = self.header.deltas.iter_mut().find(|d| d.serial == serial) {
            d.delta_type = DeltaType::Removed;
        }

        // Each of this delta's blocks runs to the `^AE` carrying its serial,
        // whatever opens or closes between. Inside its insert block
        // everything goes; of a delete block only the two markers go.
        let nothing_applied = HashSet::new();
        let mut walker = Walker::new(&self.body, &nothing_applied);
        let mut keep = Vec::with_capacity(self.body.len());
        let mut in_own_insert = false;
        while let Some((index, rec)) = walker.next() {
            let own_marker =
                matches!(rec, Rec::Insert(s) | Rec::Delete(s) | Rec::End(s) if s == serial);
            if !own_marker && !in_own_insert {
                keep.push(index);
            }
            in_own_insert = walker.blocks().is_open(BlockKind::Insert, serial);
        }
        let mut old = std::mem::take(&mut self.body)
            .into_iter()
            .map(Some)
            .collect::<Vec<_>>();
        self.body = keep.into_iter().filter_map(|i| old[i].take()).collect();
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

    /// The set of deltas applied when reconstructing `base_serial`, adjusted
    /// by forced `-i` inclusions and `-x` exclusions.
    ///
    /// `-i` is applied *after* `-x` because POSIX 99084 defines the included
    /// list as "forced to be applied": a delta named by both options ends up
    /// applied. CSSC resolves the overlap the same way. Including a delta
    /// pulls in its whole predecessor chain; excluding one drops only the
    /// named delta, since its descendants may still be wanted.
    pub fn applied_set_with(
        &self,
        base_serial: u16,
        included: &[u16],
        excluded: &[u16],
    ) -> SccsResult<HashSet<u16>> {
        let mut set = self.compute_applied_set(base_serial)?;

        for x in excluded {
            set.remove(x);
        }

        for &inc in included {
            let mut cur = inc;
            while cur != 0 {
                set.insert(cur);
                match self.find_delta_by_serial(cur) {
                    Some(d) => cur = d.pred_serial,
                    None => break,
                }
            }
        }

        Ok(set)
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

/// Write one s-file line: `prefix` (a control code and its blank, or nothing),
/// the stored bytes, and the newline that ends it.
fn write_record<W: Write>(writer: &mut W, prefix: &[u8], text: &[u8]) -> io::Result<()> {
    writer.write_all(prefix)?;
    writer.write_all(text)?;
    writer.write_all(b"\n")
}
