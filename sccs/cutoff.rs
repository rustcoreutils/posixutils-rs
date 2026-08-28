//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The `-c cutoff` option-argument, shared by `get` and `prs`.
//!
//! `get` and `prs` carried code-identical copies of the parser; `prs`'s copy
//! was patched once for a 4-digit-year bug that `get`'s copy happened not to
//! have, with nothing linking the two.
//!
//! The *comparisons* are deliberately not unified into one predicate. `get`
//! drops deltas "created after the specified cutoff date-time" (POSIX 99062),
//! so its boundary is exclusive; `prs -e`/`-l` select deltas "earlier than and
//! including" / "later than and including" (112230-112232), so theirs are
//! inclusive. Two functions named `delta_after_cutoff` with `>` and `>=` were
//! not a drift to be reconciled — they are different questions, and naming
//! them apart is what keeps them from being "fixed" into agreement.

use plib::sccsfile::{DeltaEntry, SccsDateTime};

/// A parsed `-c` cutoff, ordered as (year, month, day, hour, minute, second).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Cutoff(pub (u16, u8, u8, u8, u8, u8));

/// Full 4-digit year for a 2-digit s-file datetime year (POSIX pivot).
pub fn full_year(dt: &SccsDateTime) -> u16 {
    if dt.year < 100 {
        if dt.year < 69 {
            2000 + dt.year
        } else {
            1900 + dt.year
        }
    } else {
        dt.year
    }
}

/// A delta's creation time as a comparable tuple.
pub fn delta_key(delta: &DeltaEntry) -> (u16, u8, u8, u8, u8, u8) {
    let dt = &delta.datetime;
    (
        full_year(dt),
        dt.month,
        dt.day,
        dt.hour,
        dt.minute,
        dt.second,
    )
}

/// Parse a `-c cutoff` date-time string.
///
/// Digits are grouped YY[MM[DD[HH[MM[SS]]]]] with non-numeric separators
/// ignored. The 2-digit-year POSIX pivot applies: 69-99 => 1900+, 00-68 =>
/// 2000+. Units omitted from the cutoff default to their maximum possible
/// value (so `-c 7502` means up to the end of February 1975), per the spec.
pub fn parse(cutoff: &str) -> Option<Cutoff> {
    let digits: String = cutoff.chars().filter(|c| c.is_ascii_digit()).collect();

    if digits.len() < 2 {
        return None;
    }

    // POSIX -c uses a 2-digit year (YY) with the documented century pivot:
    // 69-99 => 1900+, 00-68 => 2000+.
    let yy: u16 = digits[0..2].parse().ok()?;
    let year = if yy < 69 { 2000 + yy } else { 1900 + yy };

    // Each omitted field defaults to its maximum, so a partial cutoff means
    // "up to the end of" the period it names.
    let mut rest = &digits[2..];
    let mut take = |default: u8| -> u8 {
        if rest.len() >= 2 {
            let v = rest[0..2].parse().unwrap_or(default);
            rest = &rest[2..];
            v
        } else {
            rest = "";
            default
        }
    };

    let month = take(12);
    let day = take(31);
    let hour = take(23);
    let min = take(59);
    let sec = take(59);

    // Reject out-of-range fields so an invalid -c cutoff is surfaced as an
    // error rather than silently filtering nonsensically.
    if !(1..=12).contains(&month) || !(1..=31).contains(&day) || hour > 23 || min > 59 || sec > 59 {
        return None;
    }

    Some(Cutoff((year, month, day, hour, min, sec)))
}

impl Cutoff {
    /// True if the delta was created strictly after the cutoff.
    ///
    /// This is the `get -c` exclusion test: "No changes (deltas) to the SCCS
    /// file that were created after the specified cutoff date-time shall be
    /// included."
    pub fn is_after(&self, delta: &DeltaEntry) -> bool {
        delta_key(delta) > self.0
    }

    /// True if the delta was created at or after the cutoff.
    ///
    /// This is `prs -l`, which asks for deltas "later than and including".
    pub fn is_at_or_after(&self, delta: &DeltaEntry) -> bool {
        delta_key(delta) >= self.0
    }

    /// True if the delta was created at or before the cutoff.
    ///
    /// This is `prs` without `-l`, which keeps everything not created after
    /// the cutoff.
    pub fn is_at_or_before(&self, delta: &DeltaEntry) -> bool {
        delta_key(delta) <= self.0
    }
}
