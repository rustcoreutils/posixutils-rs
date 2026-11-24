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
//!
//! Each is done the way historical SCCS does it: the lines of the file are
//! copied through as they are, the lines the change names are left out or
//! written anew, and the checksum line is set from the result. The new image
//! is then read back by the parser, so the parsed view is always exactly
//! what the bytes say, and a change that would write something unreadable
//! fails here, before anything reaches the disk.

use std::collections::HashSet;

use super::parse::SccsParser;
use super::{
    compute_checksum, weave_delta, with_recomputed_checksum, BlockKind, DeltaEntry, DeltaType,
    Kind, Rec, SccsError, SccsFile, SccsFlag, SccsResult, Walker, SOH,
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

/// The content of an s-file being written: every line after the checksum
/// line, each ended by its newline.
#[derive(Debug, Default)]
pub(super) struct Content(Vec<u8>);

impl Content {
    /// A line copied from the file, or one already known to be whole.
    pub(super) fn line(&mut self, line: &[u8]) {
        self.0.extend_from_slice(line);
        self.0.push(b'\n');
    }

    /// A line made of a control prefix and a value. The value cannot hold a
    /// newline: it would end the line and start one of its own.
    fn record(&mut self, prefix: &[u8], value: &[u8]) -> SccsResult<()> {
        if value.contains(&b'\n') {
            return Err(SccsError::InvalidFormat(format!(
                "a newline cannot be stored in {:?}",
                String::from_utf8_lossy(value)
            )));
        }
        self.0.extend_from_slice(prefix);
        self.line(value);
        Ok(())
    }

    /// A line that stands by itself, with no control prefix: a login, a
    /// line of descriptive text, a line of the body. Besides a newline it
    /// cannot begin with SOH, which would make a control line of it.
    pub(super) fn text(&mut self, line: &[u8]) -> SccsResult<()> {
        if line.first() == Some(&SOH) {
            return Err(SccsError::InvalidFormat(format!(
                "a line beginning with SOH cannot be stored: {:?}",
                String::from_utf8_lossy(line)
            )));
        }
        self.record(b"", line)
    }

    /// `^AI serial` or `^AD serial`.
    pub(super) fn marker(&mut self, kind: BlockKind, serial: u16) {
        let letter = match kind {
            BlockKind::Insert => 'I',
            BlockKind::Delete => 'D',
        };
        self.line(format!("\x01{letter} {serial}").as_bytes());
    }

    /// `^AE serial`.
    pub(super) fn end(&mut self, serial: u16) {
        self.line(format!("\x01E {serial}").as_bytes());
    }

    /// The lines of a delta-table entry.
    fn delta(&mut self, delta: &DeltaEntry) -> SccsResult<()> {
        self.line(format!("\x01s {}", delta.stats).as_bytes());

        // The login sits between blank-separated fields.
        if delta.user.is_empty() || delta.user.iter().any(u8::is_ascii_whitespace) {
            return Err(SccsError::InvalidDelta(format!(
                "the login {:?} cannot be stored",
                String::from_utf8_lossy(&delta.user)
            )));
        }
        let mut head = format!(
            "\x01d {} {} {} {} ",
            delta.delta_type.to_char(),
            delta.sid,
            delta.datetime.date_string(),
            delta.datetime.time_string(),
        )
        .into_bytes();
        head.extend_from_slice(&delta.user);
        head.extend_from_slice(format!(" {} {}", delta.serial, delta.pred_serial).as_bytes());
        self.line(&head);

        for (prefix, serials) in [
            ("\x01i ", &delta.included),
            ("\x01x ", &delta.excluded),
            ("\x01g ", &delta.ignored),
        ] {
            if !serials.is_empty() {
                let list: Vec<String> = serials.iter().map(u16::to_string).collect();
                self.line(format!("{prefix}{}", list.join(" ")).as_bytes());
            }
        }
        for mr in &delta.mr_numbers {
            self.record(b"\x01m ", mr)?;
        }
        for comment in &delta.comments {
            self.record(b"\x01c ", comment)?;
        }
        self.line(b"\x01e");
        Ok(())
    }

    /// A flag's line.
    fn flag(&mut self, flag: &SccsFlag) -> SccsResult<()> {
        let value = flag.value();
        let letter = flag.flag_char();
        if value.is_empty() {
            self.line(format!("\x01f {letter}").as_bytes());
            Ok(())
        } else {
            self.record(format!("\x01f {letter} ").as_bytes(), &value)
        }
    }

    /// The s-file this content makes: the checksum line for it, then it.
    fn into_image(self) -> Vec<u8> {
        let mut image = format!("\x01h{:05}\n", compute_checksum(&self.0)).into_bytes();
        image.extend_from_slice(&self.0);
        image
    }
}

/// One flag line, as bytes, for comparing with a line of the file.
fn flag_line(flag: &SccsFlag) -> SccsResult<Vec<u8>> {
    let mut content = Content::default();
    content.flag(flag)?;
    content.0.pop(); // its newline
    Ok(content.0)
}

impl SccsFile {
    /// Replace the file with `content` under a fresh checksum line, read
    /// back by the parser. On an error the file is left as it was.
    fn rewrite(&mut self, content: Content) -> SccsResult<()> {
        *self = SccsParser::new(&content.into_image(), false).parse()?;
        Ok(())
    }

    /// The header copied through, with `edit` deciding what becomes of each
    /// line: it is given the line and writes to the content what should
    /// stand in its place (the line itself, to keep it). The body follows
    /// unchanged.
    fn rewrite_header(
        &mut self,
        mut edit: impl FnMut(Kind, &[u8], &mut Content) -> SccsResult<()>,
    ) -> SccsResult<()> {
        let mut content = Content::default();
        for (kind, line) in self.header_lines() {
            edit(kind, line, &mut content)?;
        }
        for (_, line) in self.body_lines() {
            content.line(line);
        }
        self.rewrite(content)
    }

    /// A new s-file.
    pub fn create(new: NewFile<'_>) -> SccsResult<Self> {
        if new.delta.serial != 1 || new.delta.pred_serial != 0 {
            return Err(SccsError::InvalidDelta(
                "the first delta has serial 1 and no predecessor".into(),
            ));
        }
        let mut content = Content::default();
        content.delta(&new.delta)?;
        content.line(b"\x01u");
        for user in new.users {
            content.text(user)?;
        }
        content.line(b"\x01U");
        for flag in new.flags {
            content.flag(flag)?;
        }
        content.line(b"\x01t");
        for line in new.descriptive_text {
            content.text(line)?;
        }
        content.line(b"\x01T");
        content.marker(BlockKind::Insert, 1);
        for line in new.lines {
            content.text(line)?;
        }
        content.end(1);
        // A file made here has nothing to read around.
        SccsParser::new(&content.into_image(), true).parse()
    }

    /// Set a flag. One the file already has, by its letter, is replaced
    /// where it stands; a new one goes after the other flags. Setting a flag
    /// to what it already is changes nothing.
    pub fn set_flag(&mut self, flag: SccsFlag) -> SccsResult<()> {
        let letter = flag.flag_char();
        let wanted = flag_line(&flag)?;
        let is_it = |kind: Kind| kind == Kind::Flag(letter);
        let existing: Vec<&[u8]> = self
            .header_lines()
            .filter(|&(kind, _)| is_it(kind))
            .map(|(_, line)| line)
            .collect();
        if existing == [&wanted[..]] {
            return Ok(());
        }
        let replacing = !existing.is_empty();
        drop(existing);

        // A new flag goes after the last flag line, or where the flags
        // would be: ahead of the descriptive text.
        let last_flag = self
            .header_lines()
            .enumerate()
            .filter(|(_, (kind, _))| matches!(kind, Kind::Flag(_)))
            .map(|(i, _)| i)
            .last();
        let mut index = 0;
        let mut written = false;
        self.rewrite_header(|kind, line, out| {
            let here = index;
            index += 1;
            if is_it(kind) {
                // The first line with the letter becomes the flag; a later
                // one would contradict it.
                if !std::mem::replace(&mut written, true) {
                    out.line(&wanted);
                }
                return Ok(());
            }
            if !replacing && last_flag.is_none() && kind == Kind::DescBegin {
                out.line(&wanted);
            }
            out.line(line);
            if !replacing && last_flag == Some(here) {
                out.line(&wanted);
            }
            Ok(())
        })
    }

    /// Remove the flag with this letter. Returns whether the file had it.
    pub fn remove_flag(&mut self, letter: char) -> SccsResult<bool> {
        if !self.header().flags.iter().any(|f| f.flag_char() == letter) {
            return Ok(false);
        }
        self.rewrite_header(|kind, line, out| {
            if kind != Kind::Flag(letter) {
                out.line(line);
            }
            Ok(())
        })?;
        Ok(true)
    }

    /// Add a login to the user list, after those there. Returns whether it
    /// was added: a login already on the list is not listed twice.
    pub fn add_user(&mut self, login: &[u8]) -> SccsResult<bool> {
        if self.header().users.iter().any(|u| u == login) {
            return Ok(false);
        }
        self.rewrite_header(|kind, line, out| {
            if kind == Kind::UsersEnd {
                out.text(login)?;
            }
            out.line(line);
            Ok(())
        })?;
        Ok(true)
    }

    /// Remove a login from the user list. Returns whether it was there.
    pub fn remove_user(&mut self, login: &[u8]) -> SccsResult<bool> {
        if !self.header().users.iter().any(|u| u == login) {
            return Ok(false);
        }
        self.rewrite_header(|kind, line, out| {
            if kind != Kind::User || line.trim_ascii() != login {
                out.line(line);
            }
            Ok(())
        })?;
        Ok(true)
    }

    /// Replace the descriptive text; no lines removes it. The lines that
    /// begin and end it stay as they are.
    pub fn set_descriptive_text(&mut self, lines: &[Vec<u8>]) -> SccsResult<()> {
        if self.header().descriptive_text == lines {
            return Ok(());
        }
        self.rewrite_header(|kind, line, out| {
            match kind {
                Kind::Desc => {}
                Kind::DescEnd => {
                    for new in lines {
                        out.text(new)?;
                    }
                    out.line(line);
                }
                _ => out.line(line),
            }
            Ok(())
        })
    }

    /// Set the checksum line from the file as it stands, and change nothing
    /// else: what `admin -z` does (POSIX admin 84111). Blank lines ahead of
    /// the checksum line, which are not part of the file, go.
    pub fn recompute_checksum(&mut self) -> SccsResult<()> {
        let image = with_recomputed_checksum(&self.to_bytes());
        if image != self.to_bytes() {
            *self = SccsParser::new(&image, false).parse()?;
        }
        Ok(())
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
        // Deltas are stored newest first: the entry goes straight after
        // the checksum line.
        let mut content = Content::default();
        content.delta(&entry)?;
        for (_, line) in self.header_lines() {
            content.line(line);
        }
        weave_delta(
            self.body(),
            base,
            entry.serial,
            ops,
            new_lines,
            &mut content,
        )?;
        self.rewrite(content)
    }

    /// Remove the delta with this serial, as `rmdel` does: its entry stays
    /// in the delta table, its type changed from `D` to `R` and nothing else
    /// of it touched, and its changes leave the body. The lines it inserted
    /// go, markers and text; the lines it deleted lose only its markers, and
    /// are text again.
    ///
    /// The caller checks that the delta may be removed (that nothing was
    /// made from it).
    pub fn remove_delta(&mut self, serial: u16) -> SccsResult<()> {
        // Entries are in the delta table in the order of the view.
        let entry = self
            .header()
            .deltas
            .iter()
            .position(|d| d.serial == serial && d.delta_type == DeltaType::Normal)
            .ok_or_else(|| SccsError::InvalidSerial(format!("no delta has serial {serial}")))?;

        let mut content = Content::default();
        let mut entries_seen = 0;
        for (kind, line) in self.header_lines() {
            if kind == Kind::DeltaHead {
                entries_seen += 1;
                if entries_seen == entry + 1 {
                    content.line(&with_type_removed(line)?);
                    continue;
                }
            }
            content.line(line);
        }

        // Each of the delta's blocks runs to the `^AE` carrying its serial,
        // whatever opens or closes between. Inside its insert block
        // everything goes; of a delete block only the two markers go.
        let nothing_applied = HashSet::new();
        let body = self.body();
        let mut walker = Walker::new(body, &nothing_applied);
        let mut in_own_insert = false;
        while let Some((index, rec)) = walker.next() {
            let own_marker =
                matches!(rec, Rec::Insert(s) | Rec::Delete(s) | Rec::End(s) if s == serial);
            if !own_marker && !in_own_insert {
                if let Some((_, line)) = body.line(index) {
                    content.line(line);
                }
            }
            in_own_insert = walker.blocks().is_open(BlockKind::Insert, serial);
        }
        self.rewrite(content)
    }
}

/// A delta's `^Ad` line with its type, the first field, changed to `R`: one
/// byte, as rmdel has always done it.
fn with_type_removed(line: &[u8]) -> SccsResult<Vec<u8>> {
    let at = line
        .iter()
        .enumerate()
        .skip(2)
        .find(|(_, b)| !b.is_ascii_whitespace())
        .map(|(i, _)| i)
        .ok_or_else(|| SccsError::InvalidDelta("a delta line with no type".into()))?;
    let mut line = line.to_vec();
    line[at] = DeltaType::Removed.to_char() as u8;
    Ok(line)
}
