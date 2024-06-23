use crate::patch_utils::{
    constants::context::{MODIFIED_SKIP, ORIGINAL_SKIP},
    order_index::OrderIndex,
};

use super::{patch_line::PatchLine, range::Range};

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
    pub fn original_lines(&self) -> &Vec<PatchLine<'a>> {
        &self.original_lines
    }

    pub fn modified_lines(&self) -> &Vec<PatchLine<'a>> {
        &self.modified_lines
    }

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

    pub fn is_original_empty(&self, skip: usize) -> bool {
        self.original_lines
            .iter()
            .skip(skip)
            .map(|patch_line| {
                matches!(
                    patch_line,
                    PatchLine::NoNewLine(_)
                        | PatchLine::ContextDeleted(_, _)
                        | PatchLine::ContextUnchanged(_)
                )
            })
            .all(|mached| !mached)
    }

    pub fn is_modified_empty(&self, skip: usize) -> bool {
        self.modified_lines
            .iter()
            .skip(skip)
            .map(|patch_line| {
                matches!(
                    patch_line,
                    PatchLine::NoNewLine(_)
                        | PatchLine::ContextInserted(_, _)
                        | PatchLine::ContextUnchanged(_)
                )
            })
            .all(|mached| !mached)
    }

    pub fn ordered_lines_indices(&self) -> &Option<Vec<ContextHunkOrderIndex>> {
        &self.ordered_lines_indeces
    }

    pub fn order_lines(&mut self) {
        if self.ordered_lines_indeces.is_some() {
            return;
        }

        let is_original_empty = self.is_original_empty(ORIGINAL_SKIP);
        let is_modified_empty = self.is_modified_empty(MODIFIED_SKIP);

        let mut original_index: usize = ORIGINAL_SKIP;
        let mut modified_index: usize = MODIFIED_SKIP;

        let f1_range_start = self.f1_range().unwrap().start();
        let f2_range_start = self.f2_range().unwrap().start();

        let original_line_number_by_index =
            |index: usize| -> usize { index - ORIGINAL_SKIP + f1_range_start };

        let modified_line_number_by_index =
            |index: usize| -> usize { index - MODIFIED_SKIP + f2_range_start };

        assert!(
            !is_original_empty || !is_modified_empty,
            "In a context hunk, both original and modified sections can not be empty!"
        );

        let mut ordered_lines_indices = Vec::<ContextHunkOrderIndex>::new();

        if is_original_empty {
            for i in modified_index..self.modified_lines.len() {
                ordered_lines_indices.push(ContextHunkOrderIndex::Modified(OrderIndex::new(
                    i,
                    original_line_number_by_index(original_index),
                    modified_line_number_by_index(i),
                )));
            }
        } else if is_modified_empty {
            for i in original_index..self.original_lines.len() {
                ordered_lines_indices.push(ContextHunkOrderIndex::Original(OrderIndex::new(
                    i,
                    original_line_number_by_index(i),
                    modified_line_number_by_index(modified_index),
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
                                original_line_number_by_index(original_index),
                                modified_line_number_by_index(modified_index),
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
                                original_line_number_by_index(original_index),
                                modified_line_number_by_index(modified_index),
                            ),
                        ));
                    }

                    original_index += 1;
                    continue;
                }

                if self.is_proper_original_to_order(original_index) {
                    ordered_lines_indices.push(ContextHunkOrderIndex::Original(OrderIndex::new(
                        original_index,
                        original_line_number_by_index(original_index),
                        modified_line_number_by_index(modified_index),
                    )));
                }

                original_index += 1;

                if self.is_proper_modified_to_order(modified_index) {
                    ordered_lines_indices.push(ContextHunkOrderIndex::Modified(OrderIndex::new(
                        modified_index,
                        original_line_number_by_index(original_index),
                        modified_line_number_by_index(modified_index),
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
                | PatchLine::ContextInserted(_, _)
                | PatchLine::ContextUnchanged(_)
        )
    }

    pub fn is_proper_original_to_order(&self, original_index: usize) -> bool {
        matches!(
            self.original_lines[original_index],
            PatchLine::NoNewLine(_) | PatchLine::ContextDeleted(_, _)
        )
    }

    pub(crate) fn verify_hunk(&self) {
        // TODO
    }
}
