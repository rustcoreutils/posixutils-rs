//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The s-file parser.

use std::str::FromStr;

use super::{
    control, DeltaEntry, DeltaStats, DeltaType, Kind, Line, SccsDateTime, SccsError, SccsFile,
    SccsFlag, SccsHeader, SccsResult, SOH,
};

/// The argument of a control line `^A<c> <arg>`: everything after the
/// separating blank, or nothing for a line too short to have one.
///
/// A line ends where its newline is; a CR before that newline belongs to the
/// line, so callers that read a number or a name from the argument trim it,
/// and callers that keep text keep it.
fn control_arg(line: &[u8]) -> &[u8] {
    line.get(3..).unwrap_or(&[])
}

pub(super) struct SccsParser<'a> {
    data: &'a [u8],
    pos: usize,
    line_num: usize,
    /// Refuse lines no record accounts for, rather than skip them.
    strict: bool,
    /// Every line consumed, in order, with what it was taken for.
    lines: Vec<Line>,
}

impl<'a> SccsParser<'a> {
    pub(super) fn new(data: &'a [u8], strict: bool) -> Self {
        Self {
            data,
            pos: 0,
            line_num: 0,
            strict,
            lines: Vec::new(),
        }
    }

    /// Record what the line last consumed is. Until told, it is a stray.
    fn mark(&mut self, kind: Kind) {
        if let Some(line) = self.lines.last_mut() {
            line.kind = kind;
        }
    }

    /// A line that is not part of any record, at `self.line_num`: an error
    /// when parsing strictly, and otherwise skipped.
    fn stray_line(&self, line: &[u8]) -> SccsResult<()> {
        if self.strict {
            return Err(SccsError::InvalidFormat(format!(
                "line {}: unexpected line {:?}",
                self.line_num,
                String::from_utf8_lossy(line)
            )));
        }
        Ok(())
    }

    pub(super) fn parse(&mut self) -> SccsResult<SccsFile> {
        // Skip leading whitespace/empty lines
        self.skip_whitespace();

        // Parse checksum
        let checksum = self.parse_checksum()?;

        // Parse delta table
        let deltas = self.parse_delta_table()?;

        // Parse user list
        let users = self.parse_user_list()?;

        // Parse flags
        let flags = self.parse_flags()?;

        // Parse descriptive text
        let descriptive_text = self.parse_descriptive_text()?;

        // Parse body
        let body_start = self.lines.len();
        self.parse_body()?;

        Ok(SccsFile::from_parts(
            self.data.to_vec(),
            std::mem::take(&mut self.lines),
            body_start,
            SccsHeader {
                checksum,
                deltas,
                users,
                flags,
                descriptive_text,
            },
        ))
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.data.len()
            && (self.data[self.pos] == b'\n' || self.data[self.pos] == b'\r')
        {
            if self.data[self.pos] == b'\n' {
                self.line_num += 1;
            }
            self.pos += 1;
        }
    }

    fn next_line(&mut self) -> Option<&'a [u8]> {
        if self.pos >= self.data.len() {
            return None;
        }

        let start = self.pos;
        while self.pos < self.data.len() && self.data[self.pos] != b'\n' {
            self.pos += 1;
        }
        let end = self.pos;

        // Skip the newline
        if self.pos < self.data.len() {
            self.pos += 1;
        }
        self.line_num += 1;
        self.lines.push(Line {
            start,
            len: end - start,
            kind: Kind::Stray,
        });

        Some(&self.data[start..end])
    }

    fn peek_line(&self) -> Option<&'a [u8]> {
        if self.pos >= self.data.len() {
            return None;
        }

        let start = self.pos;
        let mut end = self.pos;
        while end < self.data.len() && self.data[end] != b'\n' {
            end += 1;
        }

        Some(&self.data[start..end])
    }

    fn parse_checksum(&mut self) -> SccsResult<u16> {
        let line = self
            .next_line()
            .ok_or(SccsError::MissingField("checksum".into()))?;

        // Must start with ^Ah
        if line.len() < 3 || line[0] != SOH || line[1] != control::CHECKSUM {
            return Err(SccsError::InvalidFormat(
                "File does not start with checksum line".into(),
            ));
        }
        self.mark(Kind::Checksum);

        let sum_str = std::str::from_utf8(&line[2..])
            .map_err(|_| SccsError::InvalidFormat("Invalid checksum encoding".into()))?;

        sum_str
            .trim()
            .parse()
            .map_err(|_| SccsError::InvalidFormat(format!("Invalid checksum: {}", sum_str)))
    }

    fn parse_delta_table(&mut self) -> SccsResult<Vec<DeltaEntry>> {
        let mut deltas = Vec::new();

        while let Some(line) = self.peek_line() {
            // Check for end of delta table (^Au starts user list)
            if line.len() >= 2 && line[0] == SOH && line[1] == control::USER_BEGIN {
                break;
            }

            // Parse delta entry starting with ^As
            if line.len() >= 2 && line[0] == SOH && line[1] == control::STATS {
                let delta = self.parse_delta_entry()?;
                deltas.push(delta);
            } else {
                self.next_line();
                self.stray_line(line)?;
            }
        }

        Ok(deltas)
    }

    fn parse_delta_entry(&mut self) -> SccsResult<DeltaEntry> {
        let mut entry = DeltaEntry::default();

        // Parse ^As line (stats)
        let stats_line = self
            .next_line()
            .ok_or(SccsError::MissingField("stats".into()))?;
        if stats_line.len() < 3 || stats_line[0] != SOH || stats_line[1] != control::STATS {
            return Err(SccsError::InvalidDelta("Expected stats line".into()));
        }
        self.mark(Kind::Stats);
        let stats_str = std::str::from_utf8(control_arg(stats_line))
            .map_err(|_| SccsError::InvalidDelta("Invalid stats encoding".into()))?;
        entry.stats = DeltaStats::from_str(stats_str.trim())?;

        // Parse ^Ad line (delta header)
        let delta_line = self
            .next_line()
            .ok_or(SccsError::MissingField("delta header".into()))?;
        if delta_line.len() < 3 || delta_line[0] != SOH || delta_line[1] != control::DELTA {
            return Err(SccsError::InvalidDelta("Expected delta line".into()));
        }
        self.mark(Kind::DeltaHead);
        // Parse: <type> <sid> <date> <time> <user> <serial> <pred>. The user
        // is a login and kept as its bytes; every other field is ASCII.
        let delta_arg = control_arg(delta_line);
        let parts: Vec<&[u8]> = delta_arg
            .split(u8::is_ascii_whitespace)
            .filter(|p| !p.is_empty())
            .collect();
        if parts.len() < 7 {
            return Err(SccsError::InvalidDelta(format!(
                "Invalid delta format: {}",
                String::from_utf8_lossy(delta_arg)
            )));
        }
        let field = |i: usize| {
            std::str::from_utf8(parts[i])
                .map_err(|_| SccsError::InvalidDelta("Invalid delta encoding".into()))
        };

        entry.delta_type = DeltaType::from_char(field(0)?.chars().next().unwrap_or('D'))?;
        entry.sid = field(1)?.parse()?;
        entry.datetime = SccsDateTime::parse(field(2)?, field(3)?)?;
        entry.user = parts[4].to_vec();
        entry.serial = field(5)?
            .parse()
            .map_err(|_| SccsError::InvalidDelta("Invalid serial".into()))?;
        entry.pred_serial = field(6)?
            .parse()
            .map_err(|_| SccsError::InvalidDelta("Invalid predecessor".into()))?;

        // Parse optional lines until ^Ae
        loop {
            let line = self
                .next_line()
                .ok_or(SccsError::MissingField("delta end".into()))?;

            if line.len() < 2 || line[0] != SOH {
                self.stray_line(line)?;
                continue;
            }

            match line[1] {
                control::END_DELTA => {
                    self.mark(Kind::DeltaEnd);
                    break;
                }
                control::INCLUDE => {
                    self.mark(Kind::Include);
                    let serials_str = std::str::from_utf8(control_arg(line)).unwrap_or("");
                    for s in serials_str.split_whitespace() {
                        if let Ok(serial) = s.parse() {
                            entry.included.push(serial);
                        }
                    }
                }
                control::EXCLUDE => {
                    self.mark(Kind::Exclude);
                    let serials_str = std::str::from_utf8(control_arg(line)).unwrap_or("");
                    for s in serials_str.split_whitespace() {
                        if let Ok(serial) = s.parse() {
                            entry.excluded.push(serial);
                        }
                    }
                }
                control::IGNORE => {
                    self.mark(Kind::Ignore);
                    let serials_str = std::str::from_utf8(control_arg(line)).unwrap_or("");
                    for s in serials_str.split_whitespace() {
                        if let Ok(serial) = s.parse() {
                            entry.ignored.push(serial);
                        }
                    }
                }
                control::MR => {
                    self.mark(Kind::Mr);
                    let mr = control_arg(line).trim_ascii();
                    if !mr.is_empty() {
                        entry.mr_numbers.push(mr.to_vec());
                    }
                }
                control::COMMENT => {
                    self.mark(Kind::Comment);
                    entry.comments.push(control_arg(line).to_vec());
                }
                _ => self.stray_line(line)?,
            }
        }

        Ok(entry)
    }

    fn parse_user_list(&mut self) -> SccsResult<Vec<Vec<u8>>> {
        let mut users = Vec::new();

        // Consume ^Au line
        let line = self
            .next_line()
            .ok_or(SccsError::MissingField("user list".into()))?;
        if line.len() < 2 || line[0] != SOH || line[1] != control::USER_BEGIN {
            return Err(SccsError::InvalidFormat("Expected user list start".into()));
        }
        self.mark(Kind::UsersBegin);

        // Read user names until ^AU
        loop {
            let line = self
                .next_line()
                .ok_or(SccsError::MissingField("user list end".into()))?;

            if line.len() >= 2 && line[0] == SOH && line[1] == control::USER_END {
                self.mark(Kind::UsersEnd);
                break;
            }
            self.mark(Kind::User);

            let user = line.trim_ascii();
            if !user.is_empty() {
                users.push(user.to_vec());
            }
        }

        Ok(users)
    }

    fn parse_flags(&mut self) -> SccsResult<Vec<SccsFlag>> {
        let mut flags = Vec::new();

        while let Some(line) = self.peek_line() {
            // Check for end of flags (^At starts descriptive text)
            if line.len() >= 2 && line[0] == SOH && line[1] == control::DESC_BEGIN {
                break;
            }

            // Parse ^Af lines
            if line.len() >= 4 && line[0] == SOH && line[1] == control::FLAG {
                self.next_line();
                let flag = control_arg(line).trim_ascii();
                if !flag.is_empty() {
                    // The flag letter is one character; refuse a line that
                    // does not start with one rather than guess at it.
                    let letter_len = (1..=flag.len().min(4))
                        .find(|&n| std::str::from_utf8(&flag[..n]).is_ok())
                        .ok_or_else(|| {
                            SccsError::InvalidFlag(String::from_utf8_lossy(flag).into_owned())
                        })?;
                    let letter = std::str::from_utf8(&flag[..letter_len])
                        .ok()
                        .and_then(|s| s.chars().next())
                        .unwrap_or('?');
                    flags.push(SccsFlag::parse(letter, &flag[letter_len..])?);
                    self.mark(Kind::Flag(letter));
                }
            } else {
                self.next_line();
                self.stray_line(line)?;
            }
        }

        Ok(flags)
    }

    fn parse_descriptive_text(&mut self) -> SccsResult<Vec<Vec<u8>>> {
        let mut text = Vec::new();

        // Consume ^At line
        let line = self
            .next_line()
            .ok_or(SccsError::MissingField("descriptive text".into()))?;
        if line.len() < 2 || line[0] != SOH || line[1] != control::DESC_BEGIN {
            return Err(SccsError::InvalidFormat(
                "Expected descriptive text start".into(),
            ));
        }
        self.mark(Kind::DescBegin);

        // Read text until ^AT
        loop {
            let line = self
                .next_line()
                .ok_or(SccsError::MissingField("descriptive text end".into()))?;

            if line.len() >= 2 && line[0] == SOH && line[1] == control::DESC_END {
                self.mark(Kind::DescEnd);
                break;
            }
            self.mark(Kind::Desc);

            text.push(line.to_vec());
        }

        Ok(text)
    }

    /// The body: every line is a block marker or text. An unknown control
    /// line is kept as text.
    fn parse_body(&mut self) -> SccsResult<()> {
        // Serials of the blocks open so far, for the strict check: an end
        // closes a block of its serial, and none is left open. Blocks need
        // not nest -- one may end inside another.
        let mut open: Vec<u16> = Vec::new();

        while let Some(line) = self.next_line() {
            self.mark(Kind::Text);
            if line.len() < 2 || line[0] != SOH {
                continue;
            }
            let serial_str = std::str::from_utf8(control_arg(line)).unwrap_or("").trim();
            let parsed = serial_str.parse::<u16>();
            let serial = parsed.clone().unwrap_or(0);
            let is_marker = matches!(
                line[1],
                control::BODY_INSERT | control::BODY_DELETE | control::BODY_END
            );
            if is_marker && parsed.is_err() {
                self.stray_line(line)?;
            }

            match line[1] {
                control::BODY_INSERT => {
                    open.push(serial);
                    self.mark(Kind::Insert(serial));
                }
                control::BODY_DELETE => {
                    open.push(serial);
                    self.mark(Kind::Delete(serial));
                }
                control::BODY_END => {
                    match open.iter().rposition(|&s| s == serial) {
                        Some(at) => {
                            open.remove(at);
                        }
                        None => self.stray_line(line)?,
                    }
                    self.mark(Kind::End(serial));
                }
                // An unknown control line in the body is kept as text.
                _ => self.stray_line(line)?,
            }
        }

        if self.strict && !open.is_empty() {
            return Err(SccsError::InvalidBody(format!(
                "block {} is never closed",
                open[open.len() - 1]
            )));
        }

        Ok(())
    }
}
