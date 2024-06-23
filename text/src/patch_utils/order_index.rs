#[derive(Debug)]
#[allow(dead_code)]
pub struct OrderIndex {
    index: usize,
    original_line_number: usize,
    modified_line_number: usize,
}

impl OrderIndex {
    pub fn new(index: usize, original_line_number: usize, modified_line_number: usize) -> Self {
        Self {
            index,
            original_line_number,
            modified_line_number,
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }
}
