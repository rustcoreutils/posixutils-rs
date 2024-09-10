use std::vec;

use crate::patch_utils::{
    context_hunk_data::ContextHunkData,
    context_hunk_range_data::ContextHunkRangeData,
    edit_script_hunk_data::EditScriptHunkData,
    edit_script_range_data::{EditScriptHunkKind, EditScriptRangeData},
    hunk::Hunk,
    normal_hunk_data::NormalHunkData,
    normal_range_data::{NormalRangeData, NormalRangeKind},
    patch_line::PatchLine,
    patch_line_data::PatchLineData,
    range::Range,
    unified_hunk_data::UnifiedHunkData,
    unified_hunk_header_data::UnifiedHunkHeaderData,
};

use super::{
    constants::{
        context::ContextRegexKind, ed::EdRegexKind, normal::NormalRegexKind,
        unified::UnifiedRegexKind,
    },
    hunks::Hunks,
    patch_error::PatchResult,
    patch_format::PatchFormat,
    patch_options::PatchOptions,
};

#[derive(Debug, Clone, Copy)]
pub enum PatchUnitKind {
    Unkonw,
    Ed(EdRegexKind),
    Normal(NormalRegexKind),
    Context(ContextRegexKind),
    Unified(UnifiedRegexKind),
    NewLine,
    NoNewLine,
}

#[derive(Debug)]
pub struct PatchUnit<'a> {
    lines: Vec<&'a str>,
    kinds: Vec<PatchUnitKind>,
    format: PatchFormat,
    options: &'a PatchOptions,
}

impl<'a> PatchUnit<'a> {
    pub fn new(
        lines: Vec<&'a str>,
        kinds: Vec<PatchUnitKind>,
        format: PatchFormat,
        options: &'a PatchOptions,
    ) -> Self {
        assert!(
            !matches!(format, PatchFormat::None),
            "Patch format can not be None in PatchUnit!"
        );

        Self {
            lines,
            kinds,
            format,
            options,
        }
    }

    pub fn lines(&self) -> &[&'a str] {
        &self.lines
    }

    pub fn format(&self) -> PatchFormat {
        self.format
    }

    pub fn kinds(&self) -> &[PatchUnitKind] {
        &self.kinds
    }

    fn into_hunks_from_normal_patch_unit(&self) -> Hunks<'a> {
        assert!(
            matches!(self.format(), PatchFormat::Normal),
            "Format should be Normal when converting PatchUnit to Hunks!"
        );

        let mut hunks = Hunks::new(PatchFormat::Normal, self.options);

        for pair in self.lines().iter().zip(self.kinds()) {
            match pair.1 {
                    crate::patch_utils::patch_unit::PatchUnitKind::Normal(normal_regex_kind) => {
                        match  normal_regex_kind {
                            crate::patch_utils::constants::normal::NormalRegexKind::RangeInsert => {
                                let range_data = NormalRangeData::try_from(pair.0).expect("NormalRange verified by regex is expected to be parsed, with no errors");
                                hunks.add_hunk(Hunk::new_normal_hunk(NormalHunkData::new(range_data.range_left(), range_data.range_right(), vec![PatchLine::NormalRange(range_data)], NormalRangeKind::Insert)))
                            },
                            crate::patch_utils::constants::normal::NormalRegexKind::RangeChange => {
                                let range_data = NormalRangeData::try_from(pair.0).expect("NormalRange verified by regex is expected to be parsed, with no errors");
                                hunks.add_hunk(Hunk::new_normal_hunk(NormalHunkData::new(range_data.range_left(), range_data.range_right(), vec![PatchLine::NormalRange(range_data)], NormalRangeKind::Change)))
                            },
                            crate::patch_utils::constants::normal::NormalRegexKind::RangeDelete => {
                                let range_data = NormalRangeData::try_from(pair.0).expect("NormalRange verified by regex is expected to be parsed, with no errors");
                                hunks.add_hunk(Hunk::new_normal_hunk(NormalHunkData::new(range_data.range_left(), range_data.range_right(), vec![PatchLine::NormalRange(range_data)], NormalRangeKind::Delete)))
                            },
                            crate::patch_utils::constants::normal::NormalRegexKind::LineInsert => hunks.add_patch_line(PatchLine::NormalLineInsert(PatchLineData::new(pair.0, PatchFormat::Normal))),
                            crate::patch_utils::constants::normal::NormalRegexKind::LineDelete => hunks.add_patch_line(PatchLine::NormalLineDelete(PatchLineData::new(pair.0, PatchFormat::Normal))),
                            crate::patch_utils::constants::normal::NormalRegexKind::LineChangeSeparator => hunks.add_patch_line(PatchLine::NormalChangeSeparator(PatchLineData::new(pair.0, PatchFormat::Normal))),
                        }
                    },
                    crate::patch_utils::patch_unit::PatchUnitKind::NewLine => hunks.add_patch_line(PatchLine::NewLine(PatchLineData::new(pair.0, PatchFormat::Normal))),
                    _ => panic!("Normal PatchUnit includes invalid kinds!")
                }
        }

        hunks
    }

    fn into_hunks_from_unified_patch_unit(&self) -> Hunks<'a> {
        assert!(
            matches!(self.format(), PatchFormat::Unified),
            "Format should be Unified when converting PatchUnit to Hunks!"
        );

        let mut hunks = Hunks::new(PatchFormat::Unified, self.options);

        for pair in self.lines().iter().zip(self.kinds()) {
            match pair.1 {
                PatchUnitKind::Unified(unified_regex_kind) => match unified_regex_kind {
                    UnifiedRegexKind::RangeHeader => {
                        let splitted_range_line = pair.0.split(' ').collect::<Vec<&str>>();
                        let range1 = Range::try_from_unified(splitted_range_line[1]).expect("UnifiedRange verified by regex is expected to be parsed, with no errors");
                        let range2 = Range::try_from_unified(splitted_range_line[2]).expect("UnifiedRange verified by regex is expected to be parsed, with no errors");

                        hunks.add_hunk(Hunk::new_unified_hunk(UnifiedHunkData::new(
                            range1,
                            range2,
                            vec![PatchLine::UnifiedHunkHeader(UnifiedHunkHeaderData::new(
                                pair.0, range1, range2,
                            ))],
                        )));
                    }
                    UnifiedRegexKind::FirstLine => hunks.set_f1_header(pair.0),
                    UnifiedRegexKind::SecondLine => hunks.set_f2_header(pair.0),
                    UnifiedRegexKind::DeletedLine => hunks.add_patch_line(
                        PatchLine::UnifiedDeleted(PatchLineData::new(pair.0, PatchFormat::Unified)),
                    ),
                    UnifiedRegexKind::InsertedLine => {
                        hunks.add_patch_line(PatchLine::UnifiedInserted(PatchLineData::new(
                            pair.0,
                            PatchFormat::Unified,
                        )))
                    }
                    UnifiedRegexKind::UnchangedLine => {
                        hunks.add_patch_line(PatchLine::UnifiedUnchanged(PatchLineData::new(
                            pair.0,
                            PatchFormat::Unified,
                        )))
                    }
                },
                PatchUnitKind::NewLine => hunks.add_patch_line(PatchLine::NewLine(
                    PatchLineData::new(pair.0, PatchFormat::Unified),
                )),
                _ => panic!("Normal PatchUnit includes invalid kinds!"),
            }
        }

        hunks
    }

    fn into_hunks_from_context_patch_unit(&self) -> Hunks<'a> {
        assert!(
            matches!(self.format(), PatchFormat::Context),
            "Format should be Context when converting PatchUnit to Hunks!"
        );

        let mut hunks = Hunks::new(PatchFormat::Context, self.options);

        let mut is_change = false;

        for pair in self.lines().iter().zip(self.kinds()) {
            match pair.1 {
                PatchUnitKind::Context(regex_kind) => match regex_kind {
                    ContextRegexKind::FirstLine => hunks.set_f1_header(pair.0),
                    ContextRegexKind::SecondLine => hunks.set_f2_header(pair.0),
                    ContextRegexKind::HunkSeparator => {
                        hunks.add_hunk(Hunk::new_context_hunk(ContextHunkData::new(
                            None,
                            None,
                            vec![],
                            vec![],
                        )));
                        hunks.add_patch_line(PatchLine::ContextHunkSeparator(PatchLineData::new(
                            pair.0,
                            PatchFormat::Context,
                        )))
                    }
                    ContextRegexKind::OriginalRange => {
                        is_change = false;
                        let range = Range::try_from_context(pair.0).expect("ContextOriginalRange verified by regex is expected to be parsed, with no errors");
                        hunks.add_patch_line(PatchLine::ContextHunkRange(
                            ContextHunkRangeData::new(pair.0, range, true),
                        ))
                    }
                    ContextRegexKind::ModifiedRange => {
                        is_change = true;
                        let range = Range::try_from_context(pair.0).expect("ContextModifiedRange verified by regex is expected to be parsed, with no errors");
                        hunks.add_patch_line(PatchLine::ContextHunkRange(
                            ContextHunkRangeData::new(pair.0, range, false),
                        ))
                    }
                    ContextRegexKind::InsertedLine => {
                        hunks.add_patch_line(PatchLine::ContextInserted(
                            PatchLineData::new(pair.0, PatchFormat::Context),
                            is_change,
                        ))
                    }
                    ContextRegexKind::DeletedLine => {
                        hunks.add_patch_line(PatchLine::ContextDeleted(
                            PatchLineData::new(pair.0, PatchFormat::Context),
                            is_change,
                        ))
                    }
                    ContextRegexKind::ChangedLine => {
                        if is_change {
                            hunks.add_patch_line(PatchLine::ContextInserted(
                                PatchLineData::new(pair.0, PatchFormat::Context),
                                is_change,
                            ))
                        } else {
                            hunks.add_patch_line(PatchLine::ContextDeleted(
                                PatchLineData::new(pair.0, PatchFormat::Context),
                                is_change,
                            ))
                        }
                    }
                    ContextRegexKind::UnchangedLine => {
                        hunks.add_patch_line(PatchLine::ContextUnchanged(PatchLineData::new(
                            pair.0,
                            PatchFormat::Context,
                        )))
                    }
                },
                PatchUnitKind::NewLine => hunks.add_patch_line(PatchLine::NewLine(
                    PatchLineData::new(pair.0, PatchFormat::Unified),
                )),
                _ => panic!("Context PatchUnit includes invalid kinds!"),
            }
        }

        hunks
    }

    fn into_hunks_from_ed_patch_unit(&self) -> Hunks<'a> {
        assert!(
            matches!(self.format(), PatchFormat::EditScript),
            "Format should be ED when converting PatchUnit to Hunks!"
        );

        let mut hunks = Hunks::new(PatchFormat::EditScript, self.options);
        let mut last_hunk_kind = EditScriptHunkKind::Insert;

        for pair in self.lines().iter().zip(self.kinds()) {
            match pair.1 {
                PatchUnitKind::Ed(regex_kind) => {
                    match regex_kind {
                        EdRegexKind::RangeAdd => {
                            last_hunk_kind = EditScriptHunkKind::Insert;
                            let range = Range::try_from_edit_script(pair.0).expect("EdRange verified by regex is expected to be parsed, with no errors");
                            hunks.add_hunk(Hunk::new_edit_script_hunk(EditScriptHunkData::new(
                                vec![],
                                range,
                                last_hunk_kind,
                            )));
                            hunks.add_patch_line(PatchLine::EditScriptRange(
                                EditScriptRangeData::new(pair.0, range, last_hunk_kind),
                            ));
                        }
                        EdRegexKind::RangeChange => {
                            last_hunk_kind = EditScriptHunkKind::Change;
                            let range = Range::try_from_edit_script(pair.0).expect("EdRange verified by regex is expected to be parsed, with no errors");
                            hunks.add_hunk(Hunk::new_edit_script_hunk(EditScriptHunkData::new(
                                vec![],
                                range,
                                last_hunk_kind,
                            )));
                            hunks.add_patch_line(PatchLine::EditScriptRange(
                                EditScriptRangeData::new(pair.0, range, last_hunk_kind),
                            ));
                        }
                        EdRegexKind::RangeDelete => {
                            last_hunk_kind = EditScriptHunkKind::Delete;
                            let range = Range::try_from_edit_script(pair.0).expect("EdRange verified by regex is expected to be parsed, with no errors");
                            hunks.add_hunk(Hunk::new_edit_script_hunk(EditScriptHunkData::new(
                                vec![],
                                range,
                                last_hunk_kind,
                            )));
                            hunks.add_patch_line(PatchLine::EditScriptRange(
                                EditScriptRangeData::new(pair.0, range, last_hunk_kind),
                            ));
                        }
                        EdRegexKind::Line => match last_hunk_kind {
                            EditScriptHunkKind::Insert => {
                                hunks.add_patch_line(PatchLine::EditScriptInsert(
                                    PatchLineData::new(pair.0, PatchFormat::EditScript),
                                ))
                            }
                            EditScriptHunkKind::Delete => {
                                hunks.add_patch_line(PatchLine::EditScriptDelete(
                                    PatchLineData::new(pair.0, PatchFormat::EditScript),
                                ))
                            }
                            EditScriptHunkKind::Change => {
                                hunks.add_patch_line(PatchLine::EditScriptChange(
                                    PatchLineData::new(pair.0, PatchFormat::EditScript),
                                ))
                            }
                        },
                        EdRegexKind::LineDot => hunks.add_patch_line(PatchLine::EditScriptLineDot(
                            PatchLineData::new(pair.0, PatchFormat::EditScript),
                        )),
                    }
                }
                PatchUnitKind::NewLine => hunks.add_patch_line(PatchLine::NewLine(
                    PatchLineData::new(pair.0, PatchFormat::Unified),
                )),
                _ => panic!("Context PatchUnit includes invalid kinds!"),
            }
        }

        hunks
    }

    pub fn verify_patch(&self) -> PatchResult<()> {
        match self.format() {
            PatchFormat::None => panic!("Trying to convert PatchUnit of format None into Hunks!"),
            PatchFormat::Normal => self.verify_normal_patch(),
            PatchFormat::Unified => self.verify_unified_patch(),
            PatchFormat::Context => self.verify_context_patch(),
            PatchFormat::EditScript => self.verify_ed_patch(),
        }
    }

    pub fn into_hunks(&self) -> Hunks<'a> {
        match self.format() {
            PatchFormat::None => panic!("Trying to convert PatchUnit of format None into Hunks!"),
            PatchFormat::Normal => self.into_hunks_from_normal_patch_unit(),
            PatchFormat::Unified => self.into_hunks_from_unified_patch_unit(),
            PatchFormat::Context => self.into_hunks_from_context_patch_unit(),
            PatchFormat::EditScript => self.into_hunks_from_ed_patch_unit(),
        }
    }

    fn verify_normal_patch(&self) -> PatchResult<()> {
        todo!()
    }

    fn verify_unified_patch(&self) -> Result<(), super::patch_error::PatchError> {
        todo!()
    }

    fn verify_context_patch(&self) -> Result<(), super::patch_error::PatchError> {
        todo!()
    }

    fn verify_ed_patch(&self) -> Result<(), super::patch_error::PatchError> {
        todo!()
    }
}
