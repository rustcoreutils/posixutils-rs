//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::HashMap;
use std::str::FromStr;

#[derive(Debug)]
pub struct SccsFile {
    pub header: SccsHeader,
    pub deltas: Vec<SccsDelta>,
    pub edits: Vec<SccsEdit>,
    pub user_info: SccsUserInfo,
    pub stats: SccsStats,
}

#[derive(Debug)]
pub struct SccsHeader {
    pub format_version: String,
    pub creation_date: String,
    pub author: String,
    pub description: String,
}

#[derive(Debug)]
pub struct SccsDelta {
    pub sid: String,
    pub date: String,
    pub time: String,
    pub author: String,
    pub added_lines: usize,
    pub deleted_lines: usize,
    pub comments: String,
}

#[derive(Debug)]
pub enum SccsEdit {
    Insert(String),
    Delete(usize),
}

#[derive(Debug)]
pub struct SccsUserInfo {
    pub users: HashMap<String, String>,
}

#[derive(Debug)]
pub struct SccsStats {
    pub total_deltas: usize,
    pub total_lines: usize,
}

impl SccsFile {
    pub fn from_string(s: &str) -> Result<Self, &'static str> {
        let lines: Vec<&str> = s.lines().collect();

        let header = parse_header(&lines)?;
        let deltas = parse_deltas(&lines)?;
        let edits = parse_edits(&lines)?;
        let user_info = parse_user_info(&lines)?;
        let stats = parse_stats(&deltas)?;

        Ok(SccsFile {
            header,
            deltas,
            edits,
            user_info,
            stats,
        })
    }

    pub fn serialize(&self) -> String {
        serialize_sccs_file(self)
    }
}

fn parse_header(lines: &[&str]) -> Result<SccsHeader, &'static str> {
    let mut format_version = String::new();
    let mut creation_date = String::new();
    let author = String::new();
    let description = String::new();

    for line in lines {
        if line.starts_with('h') {
            format_version = line.to_string();
        } else if line.starts_with('s') && creation_date.is_empty() {
            creation_date = line.to_string();
        } else if line.starts_with('d') {
            break; // End of header section
        }
    }

    if format_version.is_empty() || creation_date.is_empty() {
        return Err("Missing header");
    }

    Ok(SccsHeader {
        format_version,
        creation_date,
        author,
        description,
    })
}

fn parse_edits(lines: &[&str]) -> Result<Vec<SccsEdit>, &'static str> {
    // Simplified edit parsing
    let mut edits = Vec::new();
    for line in lines.iter().skip_while(|l| !l.starts_with("edits")).skip(1) {
        if line.starts_with("insert") {
            edits.push(SccsEdit::Insert(line.to_string()));
        } else if line.starts_with("delete") {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() < 2 {
                return Err("Invalid delete format");
            }
            edits.push(SccsEdit::Delete(
                usize::from_str(parts[1]).map_err(|_| "Invalid number format")?,
            ));
        }
    }
    Ok(edits)
}

fn parse_user_info(lines: &[&str]) -> Result<SccsUserInfo, &'static str> {
    // Simplified user info parsing
    let mut users = HashMap::new();
    for line in lines.iter().skip_while(|l| !l.starts_with("users")).skip(1) {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 2 {
            return Err("Invalid user info format");
        }
        users.insert(parts[0].to_string(), parts[1].to_string());
    }
    Ok(SccsUserInfo { users })
}

fn parse_deltas(lines: &[&str]) -> Result<Vec<SccsDelta>, &'static str> {
    let mut deltas = Vec::new();
    let mut current_comments = String::new();
    let mut in_delta_section = false;

    for line in lines.iter() {
        if line.starts_with("d D") {
            in_delta_section = true;
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() < 8 {
                return Err("Invalid delta format");
            }
            deltas.push(SccsDelta {
                sid: parts[2].to_string(),
                date: parts[3].to_string(),
                time: parts[4].to_string(),
                author: parts[5].to_string(),
                added_lines: usize::from_str(parts[6]).map_err(|_| "Invalid number format")?,
                deleted_lines: usize::from_str(parts[7]).map_err(|_| "Invalid number format")?,
                comments: current_comments.clone(),
            });
            current_comments.clear();
        } else if in_delta_section {
            if line.starts_with("c ") {
                current_comments.push_str(&line[2..]);
                current_comments.push('\n');
            } else if line.starts_with("e") {
                in_delta_section = false;
            }
        }
    }

    Ok(deltas)
}

fn parse_stats(deltas: &[SccsDelta]) -> Result<SccsStats, &'static str> {
    let total_deltas = deltas.len();
    let total_lines = deltas.iter().map(|d| d.added_lines + d.deleted_lines).sum();

    Ok(SccsStats {
        total_deltas,
        total_lines,
    })
}

fn serialize_sccs_file(sccs_file: &SccsFile) -> String {
    let mut result = String::new();
    result.push_str(&sccs_file.header.format_version);
    result.push('\n');
    result.push_str(&sccs_file.header.creation_date);
    result.push('\n');
    result.push_str(&sccs_file.header.author);
    result.push('\n');
    result.push_str(&sccs_file.header.description);
    result.push('\n');

    for delta in &sccs_file.deltas {
        result.push_str(&format!(
            "d D {} {} {} {} {} {}\n",
            delta.sid, delta.date, delta.time, delta.author, delta.added_lines, delta.deleted_lines
        ));
        if !delta.comments.is_empty() {
            result.push_str(&format!("c {}\n", delta.comments.trim_end()));
        }
    }

    result.push_str("edits\n");
    for edit in &sccs_file.edits {
        match edit {
            SccsEdit::Insert(line) => result.push_str(&format!("insert {}\n", line)),
            SccsEdit::Delete(line_no) => result.push_str(&format!("delete {}\n", line_no)),
        }
    }

    result.push_str("users\n");
    for (user, info) in &sccs_file.user_info.users {
        result.push_str(&format!("{} {}\n", user, info));
    }

    result.push_str(&format!(
        "stats total_deltas:{} total_lines:{}\n",
        sccs_file.stats.total_deltas, sccs_file.stats.total_lines
    ));

    result
}

#[cfg(test)]
mod sccstest {
    use super::*;

    #[test]
    fn basic_sccs_file_parse() {
        let simple = r#"
h23005
s 00003/00000/00013
d D 1.2 24/07/09 19:42:04 jgarzik 2 1
c added more data
e
s 00013/00000/00000
d D 1.1 24/07/09 19:38:28 jgarzik 1 0
c date and time created 24/07/09 19:38:28 by jgarzik
e
u
U
f e 0
t
T
I 1
apple
banana
charlie
delta
echo
foxtrot
golf
hotel
india
juliet
kilo
lima
mike
E 1
I 2
november
october
pauly
E 2
"#;

        let sccs_file = SccsFile::from_string(simple).expect("Failed to parse SCCS file");
	assert_eq!(sccs_file.header.format_version, "h23005");
	assert_eq!(sccs_file.header.creation_date, "s 00003/00000/00013");
	assert_eq!(sccs_file.header.author, "");
	assert_eq!(sccs_file.header.description, "");

	assert_eq!(sccs_file.deltas.len(), 2);

	assert_eq!(sccs_file.user_info.users.len(), 0);

	assert_eq!(sccs_file.stats.total_deltas, 2);
    }
}

