use super::range::Range;

#[derive(Debug, Clone)]
pub struct ContextHunkRangeData<'a> {
    line: &'a str,
    range: Range,
    is_original: bool,
}

impl<'a> ContextHunkRangeData<'a> {
    pub fn new(line: &'a str, range: Range, is_original: bool) -> Self {
        Self {
            line,
            range,
            is_original,
        }
    }

    pub fn line(&self) -> &str {
        self.line
    }

    pub fn is_original(&self) -> bool {
        self.is_original
    }

    pub fn range(&self) -> Range {
        self.range
    }
}
