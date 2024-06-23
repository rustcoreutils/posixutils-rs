use super::range::Range;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct UnifiedHunkHeaderData {
    line: String,
    line_in_patch: usize,
    f1_range: Range,
    f2_range: Range,
}

impl UnifiedHunkHeaderData {
    pub fn new(line: String, line_in_patch: usize, f1_range: Range, f2_range: Range) -> Self {
        Self {
            line,
            line_in_patch,
            f1_range,
            f2_range,
        }
    }

    pub fn f1_range(&self) -> Range {
        self.f1_range
    }

    pub fn f2_range(&self) -> Range {
        self.f2_range
    }

    pub fn line(&self) -> &str {
        &self.line
    }
}
