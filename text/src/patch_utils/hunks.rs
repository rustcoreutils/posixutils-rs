use super::{hunk::Hunk, patch_format::PatchFormat, patch_line::PatchLine};

#[derive(Debug)]
pub struct Hunks<'a> {
    kind: PatchFormat,
    data: Vec<Hunk<'a>>,
}

impl<'a> Hunks<'a> {
    pub fn kind(&self) -> PatchFormat {
        self.kind
    }

    pub fn new(kind: PatchFormat) -> Self {
        assert!(
            !matches!(kind, PatchFormat::None),
            "Hunks:kind can not be PatchFormat::None"
        );

        Self {
            kind,
            data: Default::default(),
        }
    }

    pub fn has_no_hunks(&self) -> bool {
        self.data.is_empty()
    }

    pub fn add_hunk(&mut self, hunk: Hunk<'a>) {
        let _hunk_kind = hunk.kind();
        assert!(
            matches!(self.kind, _hunk_kind),
            "Only hunks with the same kind are allowed!"
        );

        self.data.push(hunk);
    }

    pub fn add_patch_line(&mut self, patch_line: PatchLine<'a>) {
        let _patch_line_kind = patch_line.kind();

        assert!(
            matches!(self.kind, _patch_line_kind),
            "Adding PatchLine with different kind to Hunks is not allowed!"
        );

        assert!(
            !self.has_no_hunks(),
            "Can not add patch_line to an empty Hunks."
        );

        if let Some(last_hunk) = self.data.last_mut() {
            last_hunk.add_patch_line(patch_line);
        }
    }

    pub fn modify_hunks(&mut self, operator: fn(&mut Vec<Hunk>)) {
        operator(&mut self.data);
    }

    pub fn hunks(&self) -> &Vec<Hunk<'a>> {
        &self.data
    }

    pub fn hunks_mut(&mut self) -> &mut Vec<Hunk<'a>> {
        &mut self.data
    }
}
