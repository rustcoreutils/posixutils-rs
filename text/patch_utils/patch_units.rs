use std::io;

use crate::patch_utils::{
    constants::{
        context::context_match, ed::ed_match, normal::normal_regex_cache, unified::unified_match,
    },
    functions::{if_else, is_context_header, is_edit_script_range, is_unfied_header},
    patch_unit::PatchUnitKind,
};

use super::{
    functions::is_normal_head, hunks::Hunks, patch_file::PatchFile,
    patch_format::PatchFormat, patch_options::PatchOptions, patch_unit::PatchUnit,
};

#[derive(Debug)]
enum PatchUnitStatus<'a> {
    Done,
    Left(&'a [String]),
}

#[derive(Debug)]
pub struct PatchUnits<'a> {
    patch_file: &'a PatchFile,
    patches: Vec<PatchUnit<'a>>,
    errors: Vec<io::Error>,
    options: &'a PatchOptions,
}

impl<'a> PatchUnits<'a> {
    pub fn patch_file(&self) -> &PatchFile {
        self.patch_file
    }

    pub fn patches(&self) -> &Vec<PatchUnit<'a>> {
        &self.patches
    }

    pub fn errors(&self) -> &Vec<io::Error> {
        &self.errors
    }

    pub fn options(&self) -> &PatchOptions {
        &self.options
    }

    pub fn try_build(
        options: &'a PatchOptions,
        patch_file: &'a PatchFile,
    ) -> io::Result<Option<Self>> {
        if patch_file.content().is_empty() || patch_file.lines().is_empty() {
            return Ok(None);
        }

        let mut errors = vec![];
        let mut patches = vec![];

        let lines = match options.patch_format {
            PatchFormat::None => patch_file.lines(),
            PatchFormat::Normal => Self::slice_for_normal_patch(patch_file.lines())?,
            PatchFormat::Unified => Self::slice_for_unified_patch(patch_file.lines())?,
            PatchFormat::Context => Self::slice_for_context_patch(patch_file.lines())?,
            PatchFormat::EditScript => Self::slice_for_ed_patch(patch_file.lines())?,
        };

        let mut status = PatchUnitStatus::Left(lines);

        while let PatchUnitStatus::Left(lines) = status {
            if lines.is_empty() {
                break;
            }

            let ed_normal_allowed = patch_file.lines().len() == lines.len();
            let result = match options.patch_format {
                PatchFormat::EditScript => Self::get_ed_patch_units(lines, &options),
                PatchFormat::Normal => Self::get_normal_patch_unit(lines, &options),
                PatchFormat::Unified => Self::get_unified_patch_unit(lines, &options),
                PatchFormat::Context => Self::get_context_patch_unit(lines, &options),
                PatchFormat::None => Self::guess_get_patch_unit(lines, ed_normal_allowed, &options),
            };

            match result {
                Ok((patch_unit, patch_status)) => {
                    status = patch_status;
                    patches.push(patch_unit);
                }
                Err(error) => {
                    status = PatchUnitStatus::Done;
                    errors.push(error);
                }
            }
        }

        Ok(Some(Self {
            patches,
            patch_file,
            options,
            errors,
        }))
    }

    fn get_ed_patch_units(
        lines: &'a [String],
        options: &'a PatchOptions,
    ) -> io::Result<(PatchUnit<'a>, PatchUnitStatus<'a>)> {
        let mut parsed_lines = Vec::<&str>::new();
        let mut kinds = Vec::<PatchUnitKind>::new();
        let mut last_index = 0;

        for (index, line) in lines.iter().enumerate() {
            let kind = ed_match(line);

            kinds.push(kind);
            parsed_lines.push(line);
            last_index = index;

            if matches!(kind, PatchUnitKind::Unkonw) {
                last_index -= 1;
                parsed_lines.pop();
                kinds.pop();
                break;
            }
        }

        let visited_all_lines = last_index + 1 == lines.len();

        if !visited_all_lines {
            if lines[last_index].trim().is_empty() {
                last_index = last_index.wrapping_sub(1);
                kinds.push(PatchUnitKind::NewLine);
                parsed_lines.push(&lines[last_index]);
            }
        }

        let visited_all_lines = last_index + 1 == lines.len();

        let status = if_else(
            visited_all_lines,
            PatchUnitStatus::Done,
            PatchUnitStatus::Left(&lines[last_index..]),
        );

        return Ok((
            PatchUnit::new(parsed_lines, kinds, PatchFormat::EditScript, options),
            status,
        ));
    }

    fn get_normal_patch_unit(
        lines: &'a [String],
        options: &'a PatchOptions,
    ) -> io::Result<(PatchUnit<'a>, PatchUnitStatus<'a>)> {
        let regex_cache = normal_regex_cache();
        let mut parsed_lines = Vec::<&str>::new();
        let mut kinds = Vec::<PatchUnitKind>::new();
        let mut last_index = 0;

        for (index, line) in lines.iter().enumerate() {
            let kind = regex_cache
                .iter()
                .filter(|pair| pair.1.is_match(line))
                .map(|pair| pair.0)
                .nth(0);

            let kind = if let Some(kind) = kind {
                PatchUnitKind::Normal(*kind)
            } else {
                PatchUnitKind::Unkonw
            };

            kinds.push(kind);
            parsed_lines.push(line);
            last_index = index;

            if matches!(kind, PatchUnitKind::Unkonw) {
                last_index = last_index.wrapping_sub(1);
                parsed_lines.pop();
                kinds.pop();
                break;
            }
        }

        let visited_all_lines = last_index + 1 == lines.len();

        if !visited_all_lines {
            if lines[last_index].trim().is_empty() {
                last_index += 1;
                kinds.push(PatchUnitKind::NewLine);
                parsed_lines.push(&lines[last_index]);
            }
        }

        let status = if_else(
            visited_all_lines,
            PatchUnitStatus::Done,
            PatchUnitStatus::Left(&lines[last_index..]),
        );

        return Ok((
            PatchUnit::new(parsed_lines, kinds, PatchFormat::Normal, options),
            status,
        ));
    }

    fn get_context_patch_unit(
        lines: &'a [String],
        options: &'a PatchOptions,
    ) -> io::Result<(PatchUnit<'a>, PatchUnitStatus<'a>)> {
        let mut parsed_lines = Vec::<&str>::new();
        let mut kinds = Vec::<PatchUnitKind>::new();
        let mut last_index = 0;

        for (index, line) in lines.iter().enumerate() {
            let kind = context_match(line);

            kinds.push(kind);
            parsed_lines.push(line);
            last_index = index;

            if matches!(kind, PatchUnitKind::Unkonw) {
                last_index = last_index.wrapping_sub(1);
                parsed_lines.pop();
                kinds.pop();
                break;
            }
        }

        let visited_all_lines = last_index + 1 == lines.len();

        if !visited_all_lines {
            if lines[last_index].trim().is_empty() {
                last_index += 1;
                kinds.push(PatchUnitKind::NewLine);
                parsed_lines.push(&lines[last_index]);
            }
        }

        let visited_all_lines = last_index + 1 == lines.len();

        let status = if_else(
            visited_all_lines,
            PatchUnitStatus::Done,
            PatchUnitStatus::Left(&lines[last_index..]),
        );

        return Ok((
            PatchUnit::new(parsed_lines, kinds, PatchFormat::Context, options),
            status,
        ));
    }

    fn get_unified_patch_unit(
        lines: &'a [String],
        options: &'a PatchOptions,
    ) -> io::Result<(PatchUnit<'a>, PatchUnitStatus<'a>)> {
        let mut parsed_lines = Vec::<&str>::new();
        let mut kinds = Vec::<PatchUnitKind>::new();
        let mut last_index = 0;

        for (index, line) in lines.iter().enumerate() {
            let kind = unified_match(line);

            kinds.push(kind);
            parsed_lines.push(line);
            last_index = index;

            if matches!(kind, PatchUnitKind::Unkonw) {
                last_index = last_index.wrapping_sub(1);
                parsed_lines.pop();
                kinds.pop();
                break;
            }
        }

        let visited_all_lines = last_index + 1 == lines.len();

        if !visited_all_lines {
            if lines[last_index].trim().is_empty() {
                last_index += 1;
                kinds.push(PatchUnitKind::NewLine);
                parsed_lines.push(&lines[last_index]);
            }
        }

        let visited_all_lines = last_index + 1 == lines.len();

        let status = if_else(
            visited_all_lines,
            PatchUnitStatus::Done,
            PatchUnitStatus::Left(&lines[last_index..]),
        );

        return Ok((
            PatchUnit::new(parsed_lines, kinds, PatchFormat::Unified, options),
            status,
        ));
    }

    fn guess_get_patch_unit(
        lines: &'a [String],
        ed_normal_allowed: bool,
        options: &'a PatchOptions,
    ) -> io::Result<(PatchUnit<'a>, PatchUnitStatus<'a>)> {
        for (index, line) in lines.iter().enumerate() {
            if ed_normal_allowed && is_normal_head(line) {
                return Self::get_normal_patch_unit(&lines[index..], options);
            }

            if ed_normal_allowed && is_edit_script_range(line) {
                return Self::get_ed_patch_units(&lines[index..], options);
            }

            if index + 1 < lines.len() && is_context_header(line, &lines[index + 1]) {
                return Self::get_context_patch_unit(&lines[index..], options);
            }

            if index + 1 < lines.len() && is_unfied_header(line, &lines[index + 1]) {
                return Self::get_unified_patch_unit(&lines[index..], options);
            }
        }

        Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Could not find more patches!",
        ))
    }

    fn slice_for_normal_patch(lines: &[String]) -> io::Result<&[String]> {
        for (index, line) in lines.iter().enumerate() {
            if is_normal_head(line) {
                return Ok(&lines[index..]);
            }
        }

        Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Could not find any normal patch in the provided data!",
        ))
    }

    fn slice_for_ed_patch(lines: &[String]) -> io::Result<&[String]> {
        for (index, line) in lines.iter().enumerate() {
            if is_edit_script_range(line) {
                return Ok(&lines[index..]);
            }
        }

        Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Could not find any normal patch in the provided data!",
        ))
    }

    fn slice_for_unified_patch(lines: &[String]) -> io::Result<&[String]> {
        for (index, line) in lines.iter().enumerate() {
            if index + 1 < lines.len() && is_unfied_header(line, &lines[index + 1]) {
                return Ok(&lines[index..]);
            }
        }

        Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Could not find any unified patch in the provided data!",
        ))
    }

    fn slice_for_context_patch(lines: &[String]) -> io::Result<&[String]> {
        for (index, line) in lines.iter().enumerate() {
            if index + 1 < lines.len() && is_context_header(line, &lines[index + 1]) {
                return Ok(&lines[index..]);
            }
        }

        Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Could not find any context patch in the provided data!",
        ))
    }

    pub fn into_hunks(&self) -> io::Result<Vec<Hunks<'a>>> {
        let mut hunks_collection: Vec<Hunks<'a>> = vec![];

        for unit in &self.patches {
            hunks_collection.push(unit.into_hunks());
        }

        Ok(hunks_collection)
    }
}
