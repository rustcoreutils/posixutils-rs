use super::range::Range;

#[derive(Debug, Clone)]
pub struct ContextHunkRangeData {
    line: String,
    line_in_patch: usize,
    range: Range,
    is_original: bool,
}

impl ContextHunkRangeData {
    pub fn new(line: String, line_in_patch: usize, range: Range, is_original: bool) -> Self {
        Self {
            line,
            line_in_patch,
            range,
            is_original,
        }
    }

    pub fn line(&self) -> &str {
        &self.line
    }

    pub fn is_original(&self) -> bool {
        self.is_original
    }

    pub fn range(&self) -> Range {
        self.range
    }

    pub fn line_in_patch(&self) -> usize {
        self.line_in_patch
    }
}
