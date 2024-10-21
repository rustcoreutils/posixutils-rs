use super::patch_format::PatchFormat;

#[derive(Debug, Clone, Copy)]
pub struct PatchLineData<'a> {
    line: &'a str,
    kind: PatchFormat,
}

impl<'a> PatchLineData<'a> {
    pub fn new(line: &'a str, kind: PatchFormat) -> Self {
        Self { line, kind }
    }

    pub fn line(&self) -> &str {
        self.line
    }

    pub fn kind(&self) -> PatchFormat {
        self.kind
    }
}
