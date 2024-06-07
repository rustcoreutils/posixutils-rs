#[derive(Debug, Clone, Copy)]
pub struct PatchLineData<'a> {
    line: &'a str,
    line_in_patch: usize,
}

impl<'a> PatchLineData<'a> {
    pub fn new(line: &'a str, line_in_patch: usize) -> Self {
        Self {
            line,
            line_in_patch,
        }
    }

    pub fn line(&self) -> &str {
        self.line
    }
}
