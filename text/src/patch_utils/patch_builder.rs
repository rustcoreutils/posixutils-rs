use std::{io, path::PathBuf};

use crate::patch_utils::{
    constants::{
        context::context_regex_cache, ed::ed_regex_cache, normal::normal_regex_cache,
        unified::unified_regex_cache,
    },
    functions::{is_context_header, is_edit_script_range, is_unfied_header},
    patch_unit::PatchUnitKind,
};

use super::{
    functions::is_normal_head, patch_file::PatchFile, patch_file_kind::FileKind,
    patch_format::PatchFormat, patch_options::PatchOptions, patch_unit::PatchUnit,
};

pub struct PatchBuilder {}

impl PatchBuilder {
    pub fn from_patch_options(options: PatchOptions) -> io::Result<Option<Self>> {
        let patch_file = PatchFile::load_file(PathBuf::from(options.patch_path), FileKind::Patch)?;

        if patch_file.content().is_empty() || patch_file.lines().is_empty() {
            return Ok(None);
        }

        let patches = match options.patch_format {
            PatchFormat::EditScript => Self::get_ed_patch_units(patch_file.lines(), 0)?,
            PatchFormat::Normal => Self::get_normal_patch_units(patch_file.lines(), 0)?,
            PatchFormat::Context => Self::get_context_patch_units(patch_file.lines(), 0)?,
            PatchFormat::Unified => Self::get_unified_patch_units(patch_file.lines(), 0)?,
            PatchFormat::None => Self::guess_get_patch_units(patch_file.lines(), 0)?,
        };

        dbg!(patches);

        Ok(Some(Self {}))
    }

    fn get_ed_patch_units(lines: &[String], skip: usize) -> io::Result<Vec<PatchUnit>> {
        println!("get_ed_patch_units visited!");
        let regex_cache = ed_regex_cache();
        let mut parsed_lines = Vec::<&str>::new();
        let mut kinds = Vec::<PatchUnitKind>::new();
        let mut last_item = skip;

        for line in lines.iter().skip(skip) {
            let kind = regex_cache
                .iter()
                .filter(|pair| pair.1.is_match(line))
                .map(|pair| pair.0)
                .nth(0);

            let kind = if let Some(kind) = kind {
                println!("Kind: {:?}", kind);
                PatchUnitKind::Ed(*kind)
            } else {
                println!("Kind: Unknown");
                PatchUnitKind::Unkonw
            };

            kinds.push(kind);
            parsed_lines.push(line);
            last_item += 1;

            if matches!(kind, PatchUnitKind::Unkonw) {
                last_item -= 1;
                parsed_lines.pop();
                kinds.pop();
                break;
            }
        }

        let visited_all_lines = last_item == lines.len();

        if !visited_all_lines {
            if lines[last_item].trim().is_empty() {
                last_item += 1;
                kinds.push(PatchUnitKind::NewLine);
                parsed_lines.push(&lines[last_item]);
            }
        }

        return Ok(vec![PatchUnit::new(
            parsed_lines,
            kinds,
            PatchFormat::EditScript,
        )]);
    }

    fn get_normal_patch_units(lines: &[String], skip: usize) -> io::Result<Vec<PatchUnit>> {
        println!("get_normal_patch_units visited!");
        let regex_cache = normal_regex_cache();
        let mut parsed_lines = Vec::<&str>::new();
        let mut kinds = Vec::<PatchUnitKind>::new();
        let mut last_item = skip;

        for line in lines.iter().skip(skip) {
            let kind = regex_cache
                .iter()
                .filter(|pair| pair.1.is_match(line))
                .map(|pair| pair.0)
                .nth(0);

            let kind = if let Some(kind) = kind {
                println!("Kind: {:?}", kind);
                PatchUnitKind::Normal(*kind)
            } else {
                println!("Kind: Unknown");
                PatchUnitKind::Unkonw
            };

            kinds.push(kind);
            parsed_lines.push(line);
            last_item += 1;

            if matches!(kind, PatchUnitKind::Unkonw) {
                last_item -= 1;
                parsed_lines.pop();
                kinds.pop();
                break;
            }
        }

        let visited_all_lines = last_item == lines.len();

        if !visited_all_lines {
            if lines[last_item].trim().is_empty() {
                last_item += 1;
                kinds.push(PatchUnitKind::NewLine);
                parsed_lines.push(&lines[last_item]);
            }
        }

        return Ok(vec![PatchUnit::new(
            parsed_lines,
            kinds,
            PatchFormat::Normal,
        )]);
    }

    fn get_context_patch_units(lines: &[String], skip: usize) -> io::Result<Vec<PatchUnit>> {
        println!("get_context_patch_units visited!");
        let regex_cache = context_regex_cache();
        let mut parsed_lines = Vec::<&str>::new();
        let mut kinds = Vec::<PatchUnitKind>::new();
        let mut last_item = skip;

        for line in lines.iter().skip(skip) {
            let kind = regex_cache
                .iter()
                .filter(|pair| pair.1.is_match(line))
                .map(|pair| pair.0)
                .nth(0);

            let kind = if let Some(kind) = kind {
                println!("Kind: {:?}", kind);
                PatchUnitKind::Context(*kind)
            } else {
                println!("Kind: Unknown");
                PatchUnitKind::Unkonw
            };

            kinds.push(kind);
            parsed_lines.push(line);
            last_item += 1;

            if matches!(kind, PatchUnitKind::Unkonw) {
                last_item -= 1;
                parsed_lines.pop();
                kinds.pop();
                break;
            }
        }

        let visited_all_lines = last_item == lines.len();

        if !visited_all_lines {
            if lines[last_item].trim().is_empty() {
                kinds.push(PatchUnitKind::NewLine);
                parsed_lines.push(&lines[last_item + 1]);
                last_item += 1;
            }
        }

        let visited_all_lines = last_item + 1 == lines.len();

        if visited_all_lines {
            return Ok(vec![PatchUnit::new(
                parsed_lines,
                kinds,
                PatchFormat::Context,
            )]);
        } else {
            return Self::guess_get_patch_units(lines, last_item);
        }
    }

    fn get_unified_patch_units(lines: &[String], skip: usize) -> io::Result<Vec<PatchUnit>> {
        println!("get_unified_patch_units visited!");
        let regex_cache = unified_regex_cache();
        let mut parsed_lines = Vec::<&str>::new();
        let mut kinds = Vec::<PatchUnitKind>::new();
        let mut last_item = skip;

        for line in lines.iter().skip(skip) {
            let kind = regex_cache
                .iter()
                .filter(|pair| pair.1.is_match(line))
                .map(|pair| pair.0)
                .nth(0);

            let kind = if let Some(kind) = kind {
                println!("Kind: {:?}", kind);
                PatchUnitKind::Unified(*kind)
            } else {
                println!("Kind: Unknown");
                PatchUnitKind::Unkonw
            };

            kinds.push(kind);
            parsed_lines.push(line);
            last_item += 1;

            if matches!(kind, PatchUnitKind::Unkonw) {
                last_item -= 1;
                parsed_lines.pop();
                kinds.pop();
                break;
            }
        }

        let visited_all_lines = last_item == lines.len();

        if !visited_all_lines {
            if lines[last_item].trim().is_empty() {
                last_item += 1;
                kinds.push(PatchUnitKind::NewLine);
                parsed_lines.push(&lines[last_item]);
            }
        }

        let visited_all_lines = last_item + 1 == lines.len();

        if visited_all_lines {
            return Ok(vec![PatchUnit::new(
                parsed_lines,
                kinds,
                PatchFormat::Unified,
            )]);
        } else {
            return Self::guess_get_patch_units(lines, last_item);
        }
    }

    fn guess_get_patch_units(lines: &[String], skip: usize) -> io::Result<Vec<PatchUnit>> {
        let ed_normal_allowed = skip == 0;

        for (index, line) in lines.iter().enumerate() {
            if ed_normal_allowed && is_normal_head(line) {
                return Self::get_normal_patch_units(lines, index);
            }

            if ed_normal_allowed && is_edit_script_range(line) {
                return Self::get_ed_patch_units(lines, index);
            }

            if index + 1 < lines.len() && is_context_header(line, &lines[index]) {
                return Self::get_context_patch_units(lines, index + 1);
            }

            if index + 1 < lines.len() && is_unfied_header(line, &lines[index]) {
                return Self::get_unified_patch_units(lines, index);
            }
        }

        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Could not find any patches!",
        ));
    }
}
