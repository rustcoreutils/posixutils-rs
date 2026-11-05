//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The header summary: one line per message, as `headers` and `from` print it.

use crate::mailbox::Mailbox;
use crate::message::{extract_address, Message};
use crate::variables::Variables;

/// Print the header summary of `msg_nums`, or of every message.
pub fn print_headers(mb: &Mailbox, msg_nums: Option<&[usize]>, vars: &Variables) {
    let screen = vars.screen_lines().get();
    let show_to = vars.is_set("showto");

    let nums: Vec<usize> = msg_nums
        .map(|n| n.to_vec())
        .unwrap_or_else(|| (1..=mb.messages.len()).collect());

    for &num in nums.iter().take(screen) {
        if let Some(msg) = mb.get(num) {
            let current_marker = if Some(num) == mb.current() { '>' } else { ' ' };
            let state_char = msg.status_char();

            // Decide whether to show To or From
            let address_field = if show_to && vars.is_me(msg.from()) {
                format!("To {}", crate::util::bare_addresses(msg.to()))
            } else {
                sender_of(msg)
            };

            let date = date_of(msg);
            let lines = msg.line_count();
            let size = msg.size();
            // Both fields are text the sender chose, written to the
            // terminal for every message in the mailbox without the user
            // asking to read it. A raw escape sequence there could
            // retitle or clear the window, or redraw the summary to hide
            // or misrepresent messages, so control characters are shown
            // as `?`.
            let address_field = printable(&address_field);
            let subject = truncate_display(&printable(msg.subject()), 25);

            // The traditional layout: sender cut to 20 columns, date
            // `Www Mmm dd hh:mm`, then lines/bytes.
            println!(
                "{}{}{:>3} {:<20.20}  {:>16.16} {:>3}/{:<5} {}",
                current_marker, state_char, num, address_field, date, lines, size, subject
            );
        }
    }
}

/// The date for the header summary, as `Www Mmm dd hh:mm`.
///
/// Traditional mailx takes it from the `From ` line, which records when
/// the message was delivered here. A missing or malformed one falls back
/// to the `Date:` header.
fn date_of(msg: &Message) -> String {
    let line = msg.envelope();
    let envelope = line
        .strip_prefix("From ")
        .map(|rest| rest.trim_start())
        .and_then(|rest| rest.split_once(char::is_whitespace))
        .and_then(|(_sender, date)| envelope_date(date));
    envelope
        .or_else(|| header_date(msg.date()))
        .unwrap_or_default()
}

/// The sender for the header summary: the `From:` address, or failing
/// that the `From ` line's, as traditional mailx shows it.
fn sender_of(msg: &Message) -> String {
    match msg.get_header("from").map(extract_address) {
        Some(addr) if !addr.is_empty() => addr,
        _ => msg
            .envelope()
            .strip_prefix("From ")
            .and_then(|rest| rest.split_whitespace().next())
            .unwrap_or("")
            .to_string(),
    }
}

const WEEKDAYS: [&str; 7] = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
const WEEKDAY_NAMES: [&str; 7] = [
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
];
const MONTHS: [&str; 12] = [
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
];
const MONTH_NAMES: [&str; 12] = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
];

/// A day of the month, `1` to `31`.
fn day_of_month(s: &str) -> Option<u32> {
    s.parse().ok().filter(|d| (1..=31).contains(d))
}

/// The `hh:mm` of an `h:mm[:ss]` or `hh:mm[:ss]` time, if it is one.
fn hours_minutes(s: &str) -> Option<String> {
    let mut fields = s.split(':');
    let (h, m) = (fields.next()?, fields.next()?);
    let in_range = |f: &str, max: u32| {
        (1..=2).contains(&f.len())
            && f.bytes().all(|b| b.is_ascii_digit())
            && f.parse::<u32>().is_ok_and(|n| n <= max)
    };
    let seconds_ok = fields.next().is_none_or(|sec| in_range(sec, 60));
    (in_range(h, 23) && m.len() == 2 && in_range(m, 59) && seconds_ok && fields.next().is_none())
        .then(|| format!("{:0>2}:{}", h, m))
}

/// Format the summary date from its parts.
fn summary_date(weekday: Option<&str>, month: &str, day: u32, time: Option<String>) -> String {
    let mut out = String::new();
    if let Some(w) = weekday {
        out.push_str(w);
        out.push(' ');
    }
    out.push_str(&format!("{} {:>2}", month, day));
    if let Some(t) = time {
        out.push(' ');
        out.push_str(&t);
    }
    out
}

/// The summary date of a `From ` line's `Www Mmm dd hh:mm:ss yyyy`.
fn envelope_date(s: &str) -> Option<String> {
    let mut parts = s.split_whitespace();
    let weekday = parts.next().filter(|w| WEEKDAYS.contains(w))?;
    let month = parts.next().filter(|m| MONTHS.contains(m))?;
    let day = day_of_month(parts.next()?)?;
    let time = hours_minutes(parts.next()?)?;
    Some(summary_date(Some(weekday), month, day, Some(time)))
}

/// The abbreviation `s` stands for, when it is one of `abbrevs` or the
/// matching full name in `names` (`Nov`, `November`), ignoring case.
///
/// Only whole words count: matching on the first three letters took any
/// word that began like one, so `marked` read as March.
fn abbreviation(s: &str, abbrevs: &[&'static str], names: &[&str]) -> Option<&'static str> {
    abbrevs
        .iter()
        .zip(names)
        .find(|(a, n)| s.eq_ignore_ascii_case(a) || s.eq_ignore_ascii_case(n))
        .map(|(a, _)| *a)
}

/// The summary date of a `Date:` value.
///
/// RFC 5322 writes `[Www,] dd Mmm yyyy hh:mm`, but mail in the wild also
/// carries `Www Mmm dd hh:mm:ss yyyy`, omits the time, or the space after the
/// comma, so the parts are found rather than expected in place. The day is
/// the number next to the month that can be one, preferring the one before
/// it: taking the number after it read `29 Jan 2007` as `Jan 2007`.
fn header_date(s: &str) -> Option<String> {
    let parts: Vec<&str> = s
        .split(|c: char| c.is_whitespace() || c == ',')
        .filter(|p| !p.is_empty())
        .collect();
    let weekday = parts
        .first()
        .and_then(|w| abbreviation(w, &WEEKDAYS, &WEEKDAY_NAMES));
    let (at, month) = parts
        .iter()
        .enumerate()
        .find_map(|(i, p)| abbreviation(p, &MONTHS, &MONTH_NAMES).map(|m| (i, m)))?;
    let before = at.checked_sub(1).and_then(|i| day_of_month(parts[i]));
    let day = before.or_else(|| parts.get(at + 1).and_then(|p| day_of_month(p)))?;
    let time = parts.iter().find_map(|p| hours_minutes(p));
    Some(summary_date(weekday, month, day, time))
}

/// Truncate `s` to at most `max_len` characters for header-summary display,
/// appending an ellipsis when shortened.
///
/// Counts and slices by characters (not bytes) so a multibyte `From:`/`Subject:`
/// value cannot land a slice on a non-char boundary and panic (audit #3).
fn truncate_display(s: &str, max_len: usize) -> String {
    if s.chars().count() <= max_len {
        s.to_string()
    } else {
        let keep = max_len.saturating_sub(3);
        let truncated: String = s.chars().take(keep).collect();
        format!("{}...", truncated)
    }
}

/// `s` with each control character replaced by `?`.
fn printable(s: &str) -> String {
    s.chars()
        .map(|c| if c.is_control() { '?' } else { c })
        .collect()
}
