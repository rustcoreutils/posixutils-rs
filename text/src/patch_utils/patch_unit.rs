use super::{
    constants::{
        context::ContextRegexKind, ed::EdRegexKind, normal::NormalRegexKind,
        unified::UnifiedRegexKind,
    },
    patch_format::PatchFormat,
};

#[derive(Debug, Clone, Copy)]
pub enum PatchUnitKind {
    Unkonw,
    Ed(EdRegexKind),
    Normal(NormalRegexKind),
    Context(ContextRegexKind),
    Unified(UnifiedRegexKind),
    NewLine,
}

#[derive(Debug)]
pub struct PatchUnit<'a> {
    lines: Vec<&'a str>,
    kinds: Vec<PatchUnitKind>,
    format: PatchFormat,
}

impl<'a> PatchUnit<'a> {
    pub fn new(lines: Vec<&'a str>, kinds: Vec<PatchUnitKind>, format: PatchFormat) -> Self {
        Self {
            lines,
            kinds,
            format,
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
}
