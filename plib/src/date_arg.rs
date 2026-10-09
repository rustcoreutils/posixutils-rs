//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The date-time operand of `touch -d` and `date -d`.
//!
//! One grammar serves both utilities: the POSIX `touch -d` form (extended
//! ISO 8601), that form without seconds or followed by ` UTC` / ` GMT`, the
//! RFC 5322 date `date -R` prints, and `@SECONDS`.  This is not GNU's
//! free-form date parser, and is not meant to grow into one.

use chrono::{Datelike, FixedOffset, Local, NaiveDate, NaiveDateTime, TimeZone, Utc};
use gettextrs::gettext;

/// An instant: seconds and nanoseconds since the epoch.
pub type Instant = (i64, u32);

/// How a date-time without a zone is read: in the local zone (`TZ`), or
/// in UTC (`date -u`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Zoneless {
    Local,
    Utc,
}

/// Parse a date-time operand: `@SECONDS` (see [`parse_epoch`]), the RFC 5322
/// date `date -R` prints (see [`parse_rfc5322`]), the POSIX extended ISO 8601
/// form followed by ` UTC` or ` GMT` (see [`strip_utc_word`]), or the ISO 8601
/// form itself (see [`parse_iso8601`]).
pub fn parse(input: &str, zoneless: Zoneless) -> Result<Instant, String> {
    let invalid = || gettext!("invalid date format: '{}'", input);
    if let Some(rest) = input.strip_prefix('@') {
        return parse_epoch(rest).map(|secs| (secs, 0)).ok_or_else(invalid);
    }
    if let Some(secs) = parse_rfc5322(input) {
        return Ok((secs, 0));
    }
    if let Some(datetime) = strip_utc_word(input) {
        return parse_iso8601(&format!("{datetime}Z"), zoneless).ok_or_else(invalid);
    }
    parse_iso8601(input, zoneless).ok_or_else(invalid)
}

/// `@SECONDS`: an optionally signed decimal count of seconds since the epoch.
/// A GNU form; perl's and guile's Debian builds pass `@$SOURCE_DATE_EPOCH`.
/// Whole seconds only, with no spaces.
fn parse_epoch(digits: &str) -> Option<i64> {
    let unsigned = digits.strip_prefix(['+', '-']).unwrap_or(digits);
    if unsigned.is_empty() || !unsigned.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    digits.parse().ok()
}

/// The POSIX date-time before a trailing ` UTC` or ` GMT`, a word that means exactly what a
/// trailing `Z` means. This is not POSIX: Debian's base-files passes
/// `touch -d "1999-08-26 12:06:20 UTC"`. One space and the upper-case word only; any other
/// zone word, spelling or spacing is left to fail as before.
fn strip_utc_word(input: &str) -> Option<&str> {
    let datetime = input
        .strip_suffix(" UTC")
        .or_else(|| input.strip_suffix(" GMT"))?;
    (!datetime.ends_with(char::is_whitespace)).then_some(datetime)
}

/// The English day and month abbreviations of RFC 5322, as `date -R` spells them.
const DAY_NAMES: [&str; 7] = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];
const MONTH_NAMES: [&str; 12] = [
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
];

/// Parse an RFC 5322 date-time, as `date -R` and Debian changelogs write it, to seconds since
/// the epoch: `[Day, ]D Mon YYYY HH:MM[:SS] +hhmm`, e.g. `Fri, 17 Jul 2026 19:05:00 +0200`.
/// This is not POSIX: Debian's base-files passes `dpkg-parsechangelog -SDate` to `touch -d`.
///
/// Strict, unlike GNU's free-form parser: fields are separated by single spaces; names are
/// spelled exactly as above; a day name must be the date's own; the day has one or two digits,
/// the year four, each time field two; the zone is a numeric offset (no `GMT`, `UT` or other
/// obsolete name, which `date -R` never prints). Every field is range-checked, and a leap second
/// (`:60`) is refused, as GNU does. The offset alone fixes the instant; `TZ` plays no part.
fn parse_rfc5322(input: &str) -> Option<i64> {
    let mut fields = input.split(' ');
    let mut field = fields.next()?;
    let weekday = match field.strip_suffix(',') {
        Some(name) => {
            field = fields.next()?;
            Some(DAY_NAMES.iter().position(|d| *d == name)?)
        }
        None => None,
    };
    let day = digits(field, 1..=2)?;
    let month_name = fields.next()?;
    let month = MONTH_NAMES.iter().position(|m| *m == month_name)? as u32 + 1;
    let year = digits(fields.next()?, 4..=4)?;
    let (hour, minute, second) = parse_rfc5322_time(fields.next()?)?;
    let offset = parse_rfc5322_zone(fields.next()?)?;
    if fields.next().is_some() {
        return None;
    }

    let date = NaiveDate::from_ymd_opt(year as i32, month, day)?;
    if weekday.is_some_and(|w| w != date.weekday().num_days_from_monday() as usize) {
        return None;
    }
    // Hour 24, minute 60 and second 60 are all out of range here.
    let naive = date.and_hms_opt(hour, minute, second)?;
    Some(offset.from_local_datetime(&naive).single()?.timestamp())
}

/// `HH:MM` or `HH:MM:SS`, each two digits; ranges are checked by the caller.
fn parse_rfc5322_time(field: &str) -> Option<(u32, u32, u32)> {
    let mut parts = field.split(':');
    let hour = digits(parts.next()?, 2..=2)?;
    let minute = digits(parts.next()?, 2..=2)?;
    let second = match parts.next() {
        Some(s) => digits(s, 2..=2)?,
        None => 0,
    };
    if parts.next().is_some() {
        return None;
    }
    Some((hour, minute, second))
}

/// A `+hhmm` or `-hhmm` offset east of UTC, hours below 24 and minutes below 60. `-0000` is UTC.
fn parse_rfc5322_zone(field: &str) -> Option<FixedOffset> {
    let (sign, hhmm) = match field.split_at_checked(1)? {
        ("+", rest) => (1, rest),
        ("-", rest) => (-1, rest),
        _ => return None,
    };
    let hhmm = digits(hhmm, 4..=4)?;
    let (hours, minutes) = (hhmm / 100, hhmm % 100);
    if hours > 23 || minutes > 59 {
        return None;
    }
    FixedOffset::east_opt(sign * (hours * 3600 + minutes * 60) as i32)
}

/// `field` as a number, if it is all ASCII digits and its length is in `len`.
fn digits(field: &str, len: std::ops::RangeInclusive<usize>) -> Option<u32> {
    if !len.contains(&field.len()) || !field.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    field.parse().ok()
}

/// Parse the extended ISO 8601 form `YYYY-MM-DDThh:mm[:SS[.frac]][zone]`: `T` or a space
/// separator, `.`/`,` fractional seconds, and a zone of `Z` or `+hh:mm` / `-hh:mm`. Without a
/// zone the value is read as `zoneless` says. POSIX requires the seconds; leaving them out is
/// an extension (`touch -d 1990-06-22T12:00Z`).
fn parse_iso8601(input: &str, zoneless: Zoneless) -> Option<Instant> {
    let norm = input.trim().replace(',', ".");
    let (body, offset) = split_zone(&norm)?;
    let body = body.replacen('T', " ", 1);

    let naive = [
        "%Y-%m-%d %H:%M:%S%.f",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
    ]
    .iter()
    .find_map(|fmt| NaiveDateTime::parse_from_str(&body, fmt).ok())?;

    let dt = match (offset, zoneless) {
        (Some(offset), _) => offset.from_local_datetime(&naive).single()?,
        (None, Zoneless::Utc) => Utc.from_utc_datetime(&naive).fixed_offset(),
        (None, Zoneless::Local) => Local.from_local_datetime(&naive).single()?.fixed_offset(),
    };
    Some((dt.timestamp(), dt.timestamp_subsec_nanos()))
}

/// Split a trailing zone off an ISO 8601 date-time: `Z`, or `+hh:mm` / `-hh:mm` after the
/// time. `None` for a zone that is malformed or out of range.
fn split_zone(s: &str) -> Option<(&str, Option<FixedOffset>)> {
    if let Some(body) = s.strip_suffix('Z') {
        return Some((body, FixedOffset::east_opt(0)));
    }
    // A sign after the date part (which has its own `-`s) starts the zone.
    let time_start = s.find(['T', ' ']).unwrap_or(s.len());
    let Some(sign_at) = s[time_start..].find(['+', '-']).map(|i| time_start + i) else {
        return Some((s, None));
    };
    let (body, zone) = s.split_at(sign_at);
    let sign = if zone.starts_with('-') { -1 } else { 1 };
    let (hh, mm) = zone[1..].split_once(':')?;
    let (hours, minutes) = (digits(hh, 2..=2)?, digits(mm, 2..=2)?);
    if hours > 23 || minutes > 59 {
        return None;
    }
    let offset = FixedOffset::east_opt(sign * (hours * 3600 + minutes * 60) as i32)?;
    Some((body, Some(offset)))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn utc(input: &str) -> Option<Instant> {
        parse(input, Zoneless::Utc).ok()
    }

    #[test]
    fn epoch_forms() {
        assert_eq!(utc("@0"), Some((0, 0)));
        assert_eq!(utc("@-1"), Some((-1, 0)));
        assert_eq!(utc("@+86400"), Some((86_400, 0)));
        for bad in ["@", "@+", "@x", "@1.5", "@ 1", "@1 ", "@--1"] {
            assert_eq!(utc(bad), None, "{bad:?}");
        }
    }

    #[test]
    fn iso8601_zones_and_precision() {
        let noon = 646_056_000; // 1990-06-22 12:00:00 UTC
        assert_eq!(utc("1990-06-22T12:00Z"), Some((noon, 0)));
        assert_eq!(utc("1990-06-22 12:00"), Some((noon, 0)));
        assert_eq!(utc("1990-06-22T12:00:00Z"), Some((noon, 0)));
        assert_eq!(utc("1990-06-22T12:00+02:00"), Some((noon - 7200, 0)));
        assert_eq!(utc("1990-06-22T12:00-01:30"), Some((noon + 5400, 0)));
        assert_eq!(utc("1990-06-22T12:00:00,5Z"), Some((noon, 500_000_000)));
        for bad in [
            "1990-06-22",
            "1990-06-22T12",
            "1990-06-22T12:00+2",
            "1990-06-22T12:00+0200",
            "1990-06-22T12:00+24:00",
            "1990-06-22T12:00ZZ",
        ] {
            assert_eq!(utc(bad), None, "{bad:?}");
        }
    }
}
