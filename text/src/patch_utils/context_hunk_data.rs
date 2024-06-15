use super::{patch_line::PatchLine, range::Range};

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

#[derive(Debug)]
pub enum ContextHunkOrderIndex {
    Original(OrderIndex),
    Modified(OrderIndex),
}

#[derive(Debug)]
pub struct ContextHunkData<'a> {
    f1_range: Option<Range>,
    f2_range: Option<Range>,
    original_lines: Vec<PatchLine<'a>>,
    modified_lines: Vec<PatchLine<'a>>,
    add_to_modified: bool,
    ordered_lines_indeces: Option<Vec<ContextHunkOrderIndex>>,
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
            ordered_lines_indeces: None,
        }
    }

    pub fn enable_add_to_modified(&mut self) {
        self.add_to_modified = true
    }

    pub fn add_patch_line(&mut self, patch_line: PatchLine<'a>) {
        if patch_line.is_context_range() {
            let range_data = patch_line
                .context_hunk_range_data()
                .expect("context_hunk_range_data should not return None!");

            if range_data.is_original() {
                self.update_f1_range(range_data.range());
            } else {
                self.add_to_modified = true;
                self.update_f2_range(range_data.range());
            }
        }

        if self.add_to_modified {
            self.modified_lines.push(patch_line);
        } else {
            self.original_lines.push(patch_line);
        }
    }

    pub fn update_f1_range(&mut self, f1_range: Range) {
        self.f1_range = Some(f1_range)
    }

    pub fn update_f2_range(&mut self, f2_range: Range) {
        self.f2_range = Some(f2_range)
    }

    pub fn f1_range(&self) -> &Option<Range> {
        &self.f1_range
    }

    pub fn f2_range(&self) -> &Option<Range> {
        &self.f2_range
    }

    fn is_original_empty(&self, skip: usize) -> bool {
        self.original_lines
            .iter()
            .skip(skip)
            .map(|patch_line| {
                matches!(
                    patch_line,
                    PatchLine::NoNewLine(_)
                        | PatchLine::ContextDeleted(_)
                        | PatchLine::ContextUnchanged(_)
                )
            })
            .all(|mached| !mached)
    }

    fn is_modified_empty(&self, skip: usize) -> bool {
        self.modified_lines
            .iter()
            .skip(skip)
            .map(|patch_line| {
                matches!(
                    patch_line,
                    PatchLine::NoNewLine(_)
                        | PatchLine::ContextInserted(_)
                        | PatchLine::ContextUnchanged(_)
                )
            })
            .all(|mached| !mached)
    }

    pub fn ordered_lines_indeces(&self) -> &Option<Vec<ContextHunkOrderIndex>> {
        &self.ordered_lines_indeces
    }

    pub fn order_lines(&mut self) {
        if self.ordered_lines_indeces.is_some() {
            return;
        }

        let original_skip = 2;// 0 is Hunk separator; 1 is original range
        let modified_skip = 1;// 0 is original range

        let is_original_empty = self.is_original_empty(original_skip);
        let is_modified_empty = self.is_modified_empty(modified_skip);

        let mut original_index: usize = original_skip; 
        let mut modified_index: usize = modified_skip; 

        assert!(
            !is_original_empty || !is_modified_empty,
            "In a context hunk, both original and modified sections can not be empty!"
        );

        let mut ordered_lines_indices = Vec::<ContextHunkOrderIndex>::new();

        if is_original_empty {
            for i in modified_index..self.modified_lines.len() {
                ordered_lines_indices.push(ContextHunkOrderIndex::Modified(OrderIndex::new(
                    i,
                    original_index + self.f1_range().unwrap().start() - original_skip,
                    i + self.f2_range().unwrap().start() - modified_skip,
                )));
            }
        } else if is_modified_empty {
            for i in original_index..self.original_lines.len() {
                ordered_lines_indices.push(ContextHunkOrderIndex::Original(OrderIndex::new(
                    i,
                    i + self.f1_range().unwrap().start() - original_skip,
                    modified_index + self.f2_range().unwrap().start() - modified_skip,
                )));
            }
        } else {
            loop {
                let original_finished = original_index >= self.original_lines.len();
                let modified_finished = modified_index >= self.modified_lines.len();

                if original_finished && modified_finished {
                    break;
                }

                if original_finished {
                    if self.is_proper_modified_to_order(modified_index) {
                        ordered_lines_indices.push(ContextHunkOrderIndex::Modified(
                            OrderIndex::new(
                                modified_index,
                                original_index + self.f1_range().unwrap().start(),
                                modified_index + self.f2_range().unwrap().start(),
                            ),
                        ));
                    }

                    modified_index += 1;
                    continue;
                }

                if modified_finished {
                    if self.is_proper_original_to_order(original_index) {
                        ordered_lines_indices.push(ContextHunkOrderIndex::Original(
                            OrderIndex::new(
                                original_index,
                                original_index + self.f1_range().unwrap().start(),
                                modified_index + self.f2_range().unwrap().start(),
                            ),
                        ));
                    }

                    original_index += 1;
                    continue;
                }

                if self.is_proper_original_to_order(original_index) {
                    ordered_lines_indices.push(ContextHunkOrderIndex::Original(OrderIndex::new(
                        original_index,
                        original_index + self.f1_range().unwrap().start(),
                        modified_index + self.f2_range().unwrap().start(),
                    )));
                }

                original_index += 1;

                if self.is_proper_modified_to_order(modified_index) {
                    ordered_lines_indices.push(ContextHunkOrderIndex::Modified(OrderIndex::new(
                        modified_index,
                        original_index + self.f1_range().unwrap().start(),
                        modified_index + self.f2_range().unwrap().start(),
                    )));
                }

                modified_index += 1;
            }
        }

        assert!(
            !ordered_lines_indices.is_empty(),
            "At least 1 original or modified ContextHunkOrderIndex is required."
        );

        self.ordered_lines_indeces = Some(ordered_lines_indices);
    }

    pub fn get_by_order_index(&self, index: &ContextHunkOrderIndex) -> &PatchLine {
        match index {
            ContextHunkOrderIndex::Original(index) => &self.original_lines[index.index()],
            ContextHunkOrderIndex::Modified(index) => &self.modified_lines[index.index()],
        }
    }

    pub fn is_proper_modified_to_order(&self, modified_index: usize) -> bool {
        matches!(
            self.modified_lines[modified_index],
            PatchLine::NoNewLine(_)
                | PatchLine::ContextInserted(_)
                | PatchLine::ContextUnchanged(_)
        )
    }

    pub fn is_proper_original_to_order(&self, original_index: usize) -> bool {
        matches!(
            self.original_lines[original_index],
            PatchLine::NoNewLine(_) | PatchLine::ContextDeleted(_)
        )
    }
}
