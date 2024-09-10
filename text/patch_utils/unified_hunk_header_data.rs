use super::range::Range;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct UnifiedHunkHeaderData<'a> {
    line: &'a str,
    f1_range: Range,
    f2_range: Range,
}

impl<'a> UnifiedHunkHeaderData<'a> {
    pub fn new(line: &'a str, f1_range: Range, f2_range: Range) -> Self {
        Self {
            line,
            f1_range,
            f2_range,
        }
    }

    pub fn line(&self) -> &str {
        self.line
    }
}
