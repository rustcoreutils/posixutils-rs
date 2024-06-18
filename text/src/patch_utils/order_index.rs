#[derive(Debug)]
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

    pub fn original_line_number(&self) -> usize {
        self.original_line_number
    }

    pub fn modified_line_number(&self) -> usize {
        self.modified_line_number
    }
}
