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
    /// In the order opened.
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

    /// `^AI serial` or `^AD serial`.
    pub fn open(&mut self, kind: BlockKind, serial: u16) {
        self.open.push((serial, kind));
    }

    /// `^AE serial`: end block `serial`, which need not be the one opened
    /// last. An end that matches no open block (a damaged file) ends the
    /// last one.
    pub fn close(&mut self, serial: u16) {
        match self.open.iter().rposition(|&(s, _)| s == serial) {
            Some(at) => {
                self.open.remove(at);
            }
            None => {
                self.open.pop();
            }
        }
    }

    /// Whether a block of this kind and serial is open.
    pub fn is_open(&self, kind: BlockKind, serial: u16) -> bool {
        self.open.contains(&(serial, kind))
    }

    /// Whether a text line here belongs to the version, and if so the serial
    /// of the delta that inserted it.
    ///
    /// It belongs when every open insert is applied and no open delete is;
    /// its inserter is the applied insert opened last (serial 1 where none
    /// is open).
    pub fn kept_by(&self) -> Option<u16> {
        let applied = |s: &u16| self.applied.contains(s);
        let kept = self.open.iter().all(|(serial, kind)| match kind {
            BlockKind::Insert => applied(serial),
            BlockKind::Delete => !applied(serial),
        });
        kept.then(|| {
            self.open
                .iter()
                .rev()
                .find(|(serial, kind)| *kind == BlockKind::Insert && applied(serial))
                .map_or(1, |&(serial, _)| serial)
        })
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
/// line with the verdict of the blocks open around it.
#[derive(Debug)]
pub struct Walker<'a> {
    body: Body<'a>,
    pos: usize,
    blocks: OpenBlocks<'a>,
}

impl<'a> Walker<'a> {
    /// Walk `body` for the version made of the deltas `applied`.
    pub fn new(body: Body<'a>, applied: &'a HashSet<u16>) -> Self {
        Walker {
            body,
            pos: 0,
            blocks: OpenBlocks::new(applied),
        }
    }

    /// The blocks open after the record last returned.
    pub fn blocks(&self) -> &OpenBlocks<'a> {
        &self.blocks
    }
}

impl<'a> Iterator for Walker<'a> {
    /// The record's index in the body, and the record.
    type Item = (usize, Rec<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.pos;
        let (kind, text) = self.body.line(index)?;
        let rec = match kind {
            Kind::Insert(serial) => {
                self.blocks.open(BlockKind::Insert, serial);
                Rec::Insert(serial)
            }
            Kind::Delete(serial) => {
                self.blocks.open(BlockKind::Delete, serial);
                Rec::Delete(serial)
            }
            Kind::End(serial) => {
                self.blocks.close(serial);
                Rec::End(serial)
            }
            // Every other line of a body is text.
            _ => Rec::Text {
                text,
                kept_by: self.blocks.kept_by(),
            },
        };
        self.pos += 1;
        Some((index, rec))
    }
}

/// Weave a new delta into `body`, writing the new body to `out`.
///
/// `base` is the version the delta was made from (its applied deltas), `ops`
/// the edit script from that version's lines to the new text, and
/// `new_lines` the new text, which the script's `Insert` steps index. The
/// new body is the old one with the delta's changes recorded under `serial`:
/// every line the script deletes inside a `^AD serial` block, every line it
/// inserts inside a `^AI serial` block, and every other line copied as it
/// was, byte for byte.
///
/// One pass over the body, with a cursor into the script. A run of inserted
/// lines goes in ahead of the next line the script keeps (or at the end of
/// the body); each deleted line gets a block of its own.
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
    // line.
    let mut pending: Vec<usize> = Vec::new();
    let take_inserts = |cursor: &mut usize, pending: &mut Vec<usize>| {
        while let Some(LineOp::Insert(j)) = ops.get(*cursor) {
            pending.push(*j);
            *cursor += 1;
        }
    };
    let flush = |out: &mut Content, pending: &mut Vec<usize>| -> SccsResult<()> {
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

    for (index, rec) in Walker::new(body, base) {
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
            Some(LineOp::Keep) => {
                flush(out, &mut pending)?;
                out.line(line);
            }
            Some(LineOp::Delete) => {
                out.marker(BlockKind::Delete, serial);
                out.line(line);
                out.end(serial);
            }
            _ => return Err(mismatch()),
        }
        cursor += 1;
    }
    take_inserts(&mut cursor, &mut pending);
    if cursor != ops.len() {
        return Err(mismatch());
    }
    flush(out, &mut pending)
}
