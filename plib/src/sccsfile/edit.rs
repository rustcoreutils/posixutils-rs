//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Changing an s-file.
//!
//! Every change the SCCS utilities make to an s-file is one of the methods
//! here: there is no other way to alter a [`SccsFile`]. `admin` sets and
//! removes flags, edits the user list and replaces the descriptive text;
//! `delta` adds a delta; `rmdel` removes one.

use std::collections::HashSet;

use super::{
    weave_delta, BlockKind, BodyRecord, DeltaEntry, DeltaType, Rec, SccsError, SccsFile, SccsFlag,
    SccsHeader, SccsResult, Walker,
};
use crate::linediff::LineOp;

/// What a new s-file starts with: its first delta and that delta's text, and
/// the header `admin` was asked to give it.
#[derive(Debug)]
pub struct NewFile<'a> {
    /// The first delta. Its serial is 1 and it has no predecessor.
    pub delta: DeltaEntry,
    pub users: &'a [Vec<u8>],
    pub flags: &'a [SccsFlag],
    pub descriptive_text: &'a [Vec<u8>],
    /// The lines of the body as stored: the text, or its encoding.
    pub lines: &'a [Vec<u8>],
}

impl SccsFile {
    /// A new s-file.
    pub fn create(new: NewFile<'_>) -> SccsResult<Self> {
        if new.delta.serial != 1 || new.delta.pred_serial != 0 {
            return Err(SccsError::InvalidDelta(
                "the first delta has serial 1 and no predecessor".into(),
            ));
        }
        let mut body = Vec::with_capacity(new.lines.len() + 2);
        body.push(BodyRecord::Insert(1));
        body.extend(new.lines.iter().cloned().map(BodyRecord::Text));
        body.push(BodyRecord::End(1));
        Ok(SccsFile {
            header: SccsHeader {
                checksum: 0, // computed when the file is written
                deltas: vec![new.delta],
                users: new.users.to_vec(),
                flags: new.flags.to_vec(),
                descriptive_text: new.descriptive_text.to_vec(),
            },
            body,
        })
    }

    /// Set a flag. One the file already has, by its letter, is replaced
    /// where it stands; a new one goes after the others.
    pub fn set_flag(&mut self, flag: SccsFlag) {
        let letter = flag.flag_char();
        let flags = &mut self.header.flags;
        match flags.iter().position(|f| f.flag_char() == letter) {
            Some(at) => {
                flags[at] = flag;
                // Any further line with the letter would contradict it.
                let mut seen = false;
                flags.retain(|f| f.flag_char() != letter || !std::mem::replace(&mut seen, true));
            }
            None => flags.push(flag),
        }
    }

    /// Remove the flag with this letter. Returns whether the file had it.
    pub fn remove_flag(&mut self, letter: char) -> bool {
        let flags = &mut self.header.flags;
        let before = flags.len();
        flags.retain(|f| f.flag_char() != letter);
        flags.len() != before
    }

    /// Add a login to the user list. Returns whether it was added: a login
    /// already on the list is not listed twice.
    pub fn add_user(&mut self, login: &[u8]) -> bool {
        let users = &mut self.header.users;
        if users.iter().any(|u| u == login) {
            return false;
        }
        users.push(login.to_vec());
        true
    }

    /// Remove a login from the user list. Returns whether it was there.
    pub fn remove_user(&mut self, login: &[u8]) -> bool {
        let users = &mut self.header.users;
        let before = users.len();
        users.retain(|u| u != login);
        users.len() != before
    }

    /// Replace the descriptive text; no lines removes it.
    pub fn set_descriptive_text(&mut self, lines: Vec<Vec<u8>>) {
        self.header.descriptive_text = lines;
    }

    /// Add a delta: its entry at the head of the delta table, and its
    /// changes woven into the body.
    ///
    /// `base` is the version it was made from (the deltas applied), `ops`
    /// the edit script from that version's lines to the new text, and
    /// `new_lines` the new text. The two parts cannot be added apart: a
    /// script that does not fit the base version is refused and the file is
    /// left as it was.
    pub fn add_delta(
        &mut self,
        entry: DeltaEntry,
        base: &HashSet<u16>,
        ops: &[LineOp],
        new_lines: &[Vec<u8>],
    ) -> SccsResult<()> {
        if entry.serial != self.max_serial() + 1 {
            return Err(SccsError::InvalidDelta(format!(
                "new delta has serial {}, not the next one, {}",
                entry.serial,
                self.max_serial() + 1
            )));
        }
        self.body = weave_delta(&self.body, base, entry.serial, ops, new_lines)?;
        // Deltas are stored newest first.
        self.header.deltas.insert(0, entry);
        Ok(())
    }

    /// Remove the delta with this serial, as `rmdel` does: its entry stays
    /// in the delta table, marked removed, and its changes leave the body.
    /// The lines it inserted go, markers and text; the lines it deleted lose
    /// only its markers, and are text again.
    ///
    /// The caller checks that the delta may be removed (that nothing was
    /// made from it).
    pub fn remove_delta(&mut self, serial: u16) -> SccsResult<()> {
        let entry = self
            .header
            .deltas
            .iter_mut()
            .find(|d| d.serial == serial && d.delta_type == DeltaType::Normal)
            .ok_or_else(|| SccsError::InvalidSerial(format!("no delta has serial {serial}")))?;
        entry.delta_type = DeltaType::Removed;

        // Each of the delta's blocks runs to the `^AE` carrying its serial,
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
        Ok(())
    }
}
