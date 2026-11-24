//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The weave: the body of an s-file, every version's text interleaved.
//!
//! Text lines sit inside blocks. `^AI n` opens a block of lines delta `n`
//! inserted, `^AD n` one of lines it deleted, and `^AE n` ends block `n`.
//! Blocks need not nest: a delete may open inside one delta's insert and end
//! inside another's, so the end marker names the block it ends.
//!
//! Which lines make up a version is decided by the blocks open around each
//! line and the set of deltas applied for that version. That decision, and
//! the walk over the records that feeds it, are here and nowhere else: `get`,
//! `get -m`, `delta` and `rmdel` each kept a copy of the walk, and the copies
//! shared one wrong assumption (that blocks nest) for as long as they were
//! copies.

use std::collections::HashSet;

use super::edit::Content;
use super::{Body, Kind, SccsError, SccsResult};
use crate::linediff::LineOp;

/// Damage to the weave: a body that cannot be read, because which lines
/// make up a version depends on blocks that do not hold together.
/// Historical SCCS calls each of these a format error, in every command that
/// reads the body.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WeaveError {
    /// `^AI n` or `^AD n` while block `n` is open.
    OpenedTwice(u16),
    /// `^AE n` with no block `n` open.
    NotOpen(u16),
    /// The body ends with block `n` open.
    NeverClosed(u16),
    /// A marker naming a serial no delta has.
    NoSuchDelta(u16),
    /// A control line that is not a block marker, or a marker with no
    /// serial.
    BadControl,
}

impl WeaveError {
    /// The error for line `index` of the body.
    fn at(self, index: usize) -> SccsError {
        let what = match self {
            WeaveError::OpenedTwice(n) => format!("block {n} is opened twice"),
            WeaveError::NotOpen(n) => format!("block {n} is ended but not open"),
            WeaveError::NeverClosed(n) => format!("block {n} is never closed"),
            WeaveError::NoSuchDelta(n) => format!("a block of delta {n}, which is not in the file"),
            WeaveError::BadControl => "a control line that is not a block marker".to_string(),
        };
        SccsError::InvalidBody(format!("line {} of the body: {what}", index + 1))
    }
}

/// What a block's delta did to the lines inside it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockKind {
    /// `^AI`: the delta inserted them.
    Insert,
    /// `^AD`: the delta deleted them.
    Delete,
}

/// The blocks open at a point in the weave, and what they make of a text
/// line there for one version (the set of deltas applied).
#[derive(Debug)]
pub struct OpenBlocks<'a> {
    applied: &'a HashSet<u16>,
    /// Highest serial first: the order in which they are asked.
    open: Vec<(u16, BlockKind)>,
}

impl<'a> OpenBlocks<'a> {
    /// No block open, for the version made of the deltas `applied`.
    pub fn new(applied: &'a HashSet<u16>) -> Self {
        OpenBlocks {
            applied,
            open: Vec::new(),
        }
    }

    /// `^AI serial` or `^AD serial`. A delta has at most one block open at
    /// a time; a second is an error.
    pub fn open(&mut self, kind: BlockKind, serial: u16) -> Result<(), WeaveError> {
        let at = self.open.partition_point(|&(s, _)| s > serial);
        if self.open.get(at).is_some_and(|&(s, _)| s == serial) {
            return Err(WeaveError::OpenedTwice(serial));
        }
        self.open.insert(at, (serial, kind));
        Ok(())
    }

    /// `^AE serial`: end block `serial`, which need not be the one opened
    /// last. An end with no block of its serial open is an error.
    pub fn close(&mut self, serial: u16) -> Result<(), WeaveError> {
        let at = self
            .open
            .iter()
            .position(|&(s, _)| s == serial)
            .ok_or(WeaveError::NotOpen(serial))?;
        self.open.remove(at);
        Ok(())
    }

    /// Whether a block of this kind and serial is open.
    pub fn is_open(&self, kind: BlockKind, serial: u16) -> bool {
        self.open.contains(&(serial, kind))
    }

    /// Whether a text line here belongs to the version, and if so the serial
    /// of the delta that inserted it.
    ///
    /// The open block of the newest delta that has a say decides. A block
    /// has a say unless it is a delete by a delta that is not applied:
    ///
    /// | block                  | says                         |
    /// |------------------------|------------------------------|
    /// | insert, delta applied  | keep; that delta inserted it |
    /// | insert, not applied    | drop: the line is not there  |
    /// | delete, delta applied  | drop: it was deleted         |
    /// | delete, not applied    | nothing                      |
    ///
    /// With no block that has a say, the line is dropped: text outside every
    /// insert belongs to no version. This is the rule of historical SCCS
    /// (`readmod`'s queue, ordered by serial) and of CSSC. Requiring every
    /// open insert to be applied and no open delete to be, as this did,
    /// agrees with it only while the open blocks nest in serial order; it
    /// lost a line a later delta inserted inside a block an earlier one had
    /// deleted.
    pub fn kept_by(&self) -> Option<u16> {
        for &(serial, kind) in &self.open {
            let applied = self.applied.contains(&serial);
            match (kind, applied) {
                (BlockKind::Insert, true) => return Some(serial),
                (BlockKind::Insert, false) | (BlockKind::Delete, true) => return None,
                (BlockKind::Delete, false) => {}
            }
        }
        None
    }
}

/// One record of the weave, as a [`Walker`] reports it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Rec<'a> {
    /// `^AI serial`
    Insert(u16),
    /// `^AD serial`
    Delete(u16),
    /// `^AE serial`
    End(u16),
    /// A text line, and the delta that inserted it if it belongs to the
    /// version walked.
    Text {
        text: &'a [u8],
        kept_by: Option<u16>,
    },
}

/// A walk over the weave for one version: every record in order, each text
/// line with the verdict of the blocks open around it. Damage to the weave
/// ends the walk with an error.
#[derive(Debug)]
pub struct Walker<'a> {
    body: Body<'a>,
    pos: usize,
    blocks: OpenBlocks<'a>,
    /// Set once the walk has ended, in an error or at the end of the body.
    done: bool,
}

impl<'a> Walker<'a> {
    /// Walk `body` for the version made of the deltas `applied`.
    pub fn new(body: Body<'a>, applied: &'a HashSet<u16>) -> Self {
        Walker {
            body,
            pos: 0,
            blocks: OpenBlocks::new(applied),
            done: false,
        }
    }

    /// The blocks open after the record last returned.
    pub fn blocks(&self) -> &OpenBlocks<'a> {
        &self.blocks
    }

    /// The record at `index`, with the blocks brought up to date.
    fn step(&mut self, index: usize, kind: Kind, text: &'a [u8]) -> Result<Rec<'a>, WeaveError> {
        let known = |serial: u16| {
            if (1..=self.body.max_serial()).contains(&serial) {
                Ok(serial)
            } else {
                Err(WeaveError::NoSuchDelta(serial))
            }
        };
        let _ = index;
        Ok(match kind {
            Kind::Insert(serial) => {
                self.blocks.open(BlockKind::Insert, known(serial)?)?;
                Rec::Insert(serial)
            }
            Kind::Delete(serial) => {
                self.blocks.open(BlockKind::Delete, known(serial)?)?;
                Rec::Delete(serial)
            }
            Kind::End(serial) => {
                self.blocks.close(serial)?;
                Rec::End(serial)
            }
            Kind::BadControl => return Err(WeaveError::BadControl),
            // Every other line of a body is text.
            _ => Rec::Text {
                text,
                kept_by: self.blocks.kept_by(),
            },
        })
    }
}

impl<'a> Iterator for Walker<'a> {
    /// The record's index in the body, and the record; or what is wrong
    /// with the weave there.
    type Item = SccsResult<(usize, Rec<'a>)>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let index = self.pos;
        let Some((kind, text)) = self.body.line(index) else {
            // The end of the body: no block may be left open.
            self.done = true;
            return self
                .blocks
                .open
                .first()
                .map(|&(serial, _)| Err(WeaveError::NeverClosed(serial).at(index)));
        };
        self.pos += 1;
        Some(match self.step(index, kind, text) {
            Ok(rec) => Ok((index, rec)),
            Err(e) => {
                self.done = true;
                Err(e.at(index))
            }
        })
    }
}

/// Weave a new delta into `body`, writing the new body to `out`.
///
/// `base` is the version the delta was made from (its applied deltas), `ops`
/// the edit script from that version's lines to the new text, and
/// `new_lines` the new text, which the script's `Insert` steps index. The
/// new body is the old one with the delta's changes recorded under `serial`,
/// and every line of the old one copied as it was, byte for byte.
///
/// The changes are laid out as SCCS and CSSC lay them out, so that the body
/// is the one they would write:
/// - A run of deleted lines is one `^AD serial` block. It opens right before
///   the first deleted line and stays open, over whatever records of other
///   deltas lie between, until the next line the version keeps. (That is how
///   blocks come to overlap.)
/// - Lines inserted go in one `^AI serial` block right before the next line
///   the version keeps, after the delete block, if any, has closed.
/// - At the end of the body an open delete block closes, and lines inserted
///   after the last line follow.
///
/// One pass over the body, with a cursor into the script.
///
/// The script must account for exactly the lines of the base version: one
/// `Keep` or `Delete` each, in order. Anything else is an error.
pub(super) fn weave_delta(
    body: Body<'_>,
    base: &HashSet<u16>,
    serial: u16,
    ops: &[LineOp],
    new_lines: &[Vec<u8>],
    out: &mut Content,
) -> SccsResult<()> {
    let mismatch =
        || SccsError::InvalidBody("the edit does not fit the version it was made from".into());
    let mut cursor = 0;
    // Inserted lines (as indices into `new_lines`) waiting for the next kept
    // line, and whether a delete block is open.
    let mut pending: Vec<usize> = Vec::new();
    let mut deleting = false;
    let take_inserts = |cursor: &mut usize, pending: &mut Vec<usize>| {
        while let Some(LineOp::Insert(j)) = ops.get(*cursor) {
            pending.push(*j);
            *cursor += 1;
        }
    };
    // What happens ahead of a kept line, and at the end of the body: the
    // delete block closes, and the waiting inserted lines go in.
    let settle =
        |out: &mut Content, deleting: &mut bool, pending: &mut Vec<usize>| -> SccsResult<()> {
            if std::mem::take(deleting) {
                out.end(serial);
            }
            if pending.is_empty() {
                return Ok(());
            }
            out.marker(BlockKind::Insert, serial);
            for j in pending.drain(..) {
                out.text(new_lines.get(j).ok_or_else(mismatch)?)?;
            }
            out.end(serial);
            Ok(())
        };

    for step in Walker::new(body, base) {
        let (index, rec) = step?;
        let (_, line) = body.line(index).ok_or_else(mismatch)?;
        let kept = matches!(
            rec,
            Rec::Text {
                kept_by: Some(_),
                ..
            }
        );
        if !kept {
            out.line(line);
            continue;
        }
        take_inserts(&mut cursor, &mut pending);
        match ops.get(cursor) {
            Some(LineOp::Keep) => settle(out, &mut deleting, &mut pending)?,
            Some(LineOp::Delete) => {
                if !std::mem::replace(&mut deleting, true) {
                    out.marker(BlockKind::Delete, serial);
                }
            }
            _ => return Err(mismatch()),
        }
        out.line(line);
        cursor += 1;
    }
    take_inserts(&mut cursor, &mut pending);
    if cursor != ops.len() {
        return Err(mismatch());
    }
    settle(out, &mut deleting, &mut pending)
}
