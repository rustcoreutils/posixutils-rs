use super::{patch_line::PatchLine, range::Range};

#[derive(Debug, Default)]
pub struct CurrentContextHunkData {
    original_index: usize,
    modified_index: usize,
}

impl CurrentContextHunkData {
    pub fn original_index(&self) -> usize {
        self.original_index
    }

    pub fn modified_index(&self) -> usize {
        self.modified_index
    }
}

#[derive(Debug)]
pub struct ContextHunkData<'a> {
    f1_range: Option<Range>,
    f2_range: Option<Range>,
    original_lines: Vec<PatchLine<'a>>,
    modified_lines: Vec<PatchLine<'a>>,
    add_to_modified: bool,
    current: Option<CurrentContextHunkData>,
    ordered_lines: Option<Vec<&'a PatchLine<'a>>>,
}

impl<'a> ContextHunkData<'a> {
    pub fn new(
        f1_range: Option<Range>,
        f2_range: Option<Range>,
        original_lines: Vec<PatchLine<'a>>,
        modified_lines: Vec<PatchLine<'a>>,
    ) -> Self {
        Self {
            f1_range,
            f2_range,
            original_lines,
            modified_lines,
            add_to_modified: false,
            current: None,
            ordered_lines: None,
        }
    }

    pub fn enable_add_to_modified(&mut self) {
        self.add_to_modified = true
    }

    pub fn add_line(&'a mut self, patch_line: PatchLine<'a>) {
        if self.add_to_modified {
            self.modified_lines.push(patch_line);
        } else {
            self.original_lines.push(patch_line);
        }
    }

    pub fn update_f1_range(&'a mut self, f1_range: Range) {
        self.f1_range = Some(f1_range)
    }

    pub fn update_f2_range(&'a mut self, f2_range: Range) {
        self.f2_range = Some(f2_range)
    }

    pub fn f1_range(&'a self) -> &Option<Range> {
        &self.f1_range
    }

    pub fn f2_range(&'a self) -> &Option<Range> {
        &self.f2_range
    }

    fn is_original_empty(&self) -> bool {
        for patch_line in &self.original_lines {
            let result = match patch_line {
                PatchLine::NoNewLine(_) => true,
                PatchLine::ContextHunkSeparator(_) => true,
                PatchLine::ContextDeleted(_) => false,
                PatchLine::ContextUnchanged(_) => false,
                PatchLine::ContextSubstituted(_) => false,
                PatchLine::ContextHunkRange(_) => true,
                _ => panic!("Invalid PatchLine in copied context original file hunk!"),
            };

            if !result {
                return result;
            }
        }

        true
    }

    fn is_modified_empty(&self) -> bool {
        for patch_line in &self.original_lines {
            let result = match patch_line {
                PatchLine::NoNewLine(_) => true,
                PatchLine::ContextInserted(_) => false,
                PatchLine::ContextUnchanged(_) => false,
                PatchLine::ContextSubstituted(_) => false,
                PatchLine::ContextHunkRange(_) => true,
                _ => panic!("Invalid PatchLine in copied context modified file hunk!"),
            };

            if !result {
                return result;
            }
        }

        true
    }

    pub fn ordered_lines(&self) -> &Option<Vec<&'a PatchLine<'a>>> {
        &self.ordered_lines
    }

    pub fn order_lines(&mut self) {
        // if self.current.is_none() && self.is_original_empty() {
        //     self.current = Some(CurrentContextHunkData {
        //         original_index: self.original_lines.len(),
        //         modified_index: 0,
        //     });
        // }

        // if self.current.is_none() && self.is_modified_empty() {
        //     self.current = Some(CurrentContextHunkData {
        //         original_index: 0,
        //         modified_index: self.modified_lines.len(),
        //     });
        // }

        // if self.current.is_none() {
        //     self.current = Some(CurrentContextHunkData::default())
        // }

        // let current = &mut self.current;

        // if let Some(current) = current {
        //     let original_done = current.original_index >= self.original_lines.len();
        //     let modified_done = current.modified_index >= self.modified_lines.len();

        //     if original_done && modified_done {
        //         return None;
        //     }

        //     if original_done {
        //         current.modified_index += 1;
        //         let current_line = &self.modified_lines[current.modified_index - 1];
        //         return Some(current_line.clone());
        //     }

        //     if modified_done {
        //         current.original_index += 1;
        //         let current_line = &self.original_lines[current.original_index - 1];
        //         return Some(current_line.clone());
        //     }
        // }

        // None
    }
}
