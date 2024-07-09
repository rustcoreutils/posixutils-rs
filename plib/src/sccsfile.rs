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
    pub author: String,
    pub date_time: String,
    pub added_lines: usize,
    pub deleted_lines: usize,
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
        let stats = parse_stats(&lines)?;

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
    // Simplified header parsing
    let format_version = lines.get(0).ok_or("Missing header")?.to_string();
    let creation_date = lines.get(1).ok_or("Missing header")?.to_string();
    let author = lines.get(2).ok_or("Missing header")?.to_string();
    let description = lines.get(3).ok_or("Missing header")?.to_string();

    Ok(SccsHeader {
        format_version,
        creation_date,
        author,
        description,
    })
}

fn parse_deltas(lines: &[&str]) -> Result<Vec<SccsDelta>, &'static str> {
    // Simplified delta parsing
    let mut deltas = Vec::new();
    for line in lines.iter().skip(4) {
        if line.starts_with("delta") {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() < 5 {
                return Err("Invalid delta format");
            }
            deltas.push(SccsDelta {
                sid: parts[1].to_string(),
                author: parts[2].to_string(),
                date_time: parts[3].to_string(),
                added_lines: usize::from_str(parts[4]).map_err(|_| "Invalid number format")?,
                deleted_lines: usize::from_str(parts[5]).map_err(|_| "Invalid number format")?,
            });
        }
    }
    Ok(deltas)
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

fn parse_stats(lines: &[&str]) -> Result<SccsStats, &'static str> {
    // Simplified stats parsing
    let total_deltas = lines.iter().filter(|&&l| l.starts_with("delta")).count();
    let total_lines = lines.len();

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
            "delta {} {} {} {} {}\n",
            delta.sid, delta.author, delta.date_time, delta.added_lines, delta.deleted_lines
        ));
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
