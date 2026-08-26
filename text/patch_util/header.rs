//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared parsing for the file-name headers of unified and context diffs.

use regex::Regex;
use std::sync::LazyLock;

/// A trailing ISO-style timestamp, as GNU diff writes it:
/// "2024-01-01 10:00:00.000000000 +0000", with the fractional seconds and the
/// zone offset both optional.
static ISO_STAMP_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"\s+\d{4}-\d{2}-\d{2}\s+\d{1,2}:\d{2}:\d{2}(?:\.\d+)?(?:\s+[-+]\d{4})?$")
        .expect("invalid regex")
});

/// A trailing ctime-style timestamp: "Mon Jan  1 10:00:00 2024".
static CTIME_STAMP_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"(?x) \s+ (?:Mon|Tue|Wed|Thu|Fri|Sat|Sun) \s+
          (?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) \s+
          \d{1,2} \s+ \d{1,2}:\d{2}:\d{2} \s+ \d{4} $",
    )
    .expect("invalid regex")
});

/// Extract the file name from the text following a `--- `, `+++ ` or `*** `
/// header marker.
///
/// diff separates the name from the modification time with a tab, but that tab
/// does not survive every mail path, and some diff implementations use spaces
/// to begin with. Fall back to recognizing the timestamp itself so a patch that
/// reached us through a mailer still names its target. A file name may contain
/// spaces, so the timestamp patterns are anchored at the end and matched
/// greedily: only a trailing run that really looks like a date is removed.
pub fn parse_filename(s: &str) -> String {
    let s = s.trim_end_matches(['\r', '\n']);
    // A tab is unambiguous: everything after the first one is metadata.
    if let Some(tab_pos) = s.find('\t') {
        return s[..tab_pos].trim().to_string();
    }
    let s = s.trim();
    for re in [&*ISO_STAMP_RE, &*CTIME_STAMP_RE] {
        if let Some(m) = re.find(s) {
            return s[..m.start()].to_string();
        }
    }
    // Some diffs separate the name from unrecognized trailing text with two
    // spaces.
    if let Some(space_pos) = s.find("  ") {
        return s[..space_pos].to_string();
    }
    s.to_string()
}

#[cfg(test)]
mod tests {
    use super::parse_filename;

    #[test]
    fn tab_separated() {
        assert_eq!(
            parse_filename("t.c\t2024-01-01 10:00:00.000000000 +0000"),
            "t.c"
        );
    }

    #[test]
    fn space_separated_iso_stamp() {
        assert_eq!(
            parse_filename("t.c 2024-01-01 10:00:00.000000000 +0000"),
            "t.c"
        );
        assert_eq!(parse_filename("t.c 2024-01-01 10:00:00"), "t.c");
        assert_eq!(
            parse_filename("/dev/null 1970-01-01 00:00:00.000000000 +0000"),
            "/dev/null"
        );
    }

    #[test]
    fn space_separated_ctime_stamp() {
        assert_eq!(parse_filename("t.c Mon Jan  1 10:00:00 2024"), "t.c");
    }

    #[test]
    fn name_with_spaces_is_kept() {
        assert_eq!(parse_filename("my file.c"), "my file.c");
        assert_eq!(
            parse_filename("my file.c\t2024-01-01 10:00:00"),
            "my file.c"
        );
        assert_eq!(parse_filename("my file.c 2024-01-01 10:00:00"), "my file.c");
    }

    #[test]
    fn name_that_merely_contains_a_date_is_kept() {
        assert_eq!(
            parse_filename("log 2024-01-01 10:00:00.txt"),
            "log 2024-01-01 10:00:00.txt"
        );
    }

    #[test]
    fn bare_name() {
        assert_eq!(parse_filename("a/t.c"), "a/t.c");
    }
}
