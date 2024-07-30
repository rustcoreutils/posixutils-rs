use crate::patch_utils::constants::context::ORIGINAL_SKIP;

use super::{functions::verify_patch_line, patch_line::PatchLine, range::Range};

#[derive(Debug)]
pub struct ContextHunkData<'a> {
    f1_range: Option<Range>,
    f2_range: Option<Range>,
    original_lines: Vec<PatchLine<'a>>,
    modified_lines: Vec<PatchLine<'a>>,
    add_to_modified: bool,
    change_indices: Vec<usize>,
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
            change_indices: vec![],
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
            if matches!(patch_line, PatchLine::ContextInserted(_, true)) {
                self.change_indices.push(self.modified_lines.len());
            }

            self.modified_lines.push(patch_line);
        } else {
            self.original_lines.push(patch_line);
        }
    }

    pub fn change_by_index(&self, index: usize) -> &PatchLine<'a> {
        &self.modified_lines[self.change_indices[index]]
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

    pub(crate) fn verify_hunk(&self) {
        // TODO
    }

    pub(crate) fn verify_file(
        &self,
        file: &super::patch_file::PatchFile,
        reversed: bool,
    ) -> Result<(), ()> {
        let r1 = self.f1_range.unwrap();
        let r2 = self.f2_range.unwrap();

        let mut current_original_line = r1.start();
        let mut current_modified_line = r2.start();

        let original_is_empty = self.is_original_empty(ORIGINAL_SKIP);

        let lines: &Vec<PatchLine> = if original_is_empty {
            self.modified_lines()
        } else {
            self.original_lines()
        };

        for line in lines {
            match line {
                PatchLine::ContextHunkSeparator(_) => {}
                PatchLine::ContextInserted(_, is_change) => {
                    if reversed {
                        verify_patch_line(
                            line.original_line(),
                            &file.lines()[current_modified_line - 1],
                        )?;
                    } else if *is_change {
                        verify_patch_line(
                            line.original_line(),
                            &file.lines()[current_original_line - 1],
                        )?;
                    }

                    current_modified_line += 1;

                    if *is_change {
                        current_original_line += 1;
                    }
                }
                PatchLine::ContextDeleted(_, is_change) => {
                    if !reversed {
                        verify_patch_line(
                            line.original_line(),
                            &file.lines()[current_original_line - 1],
                        )?;
                    }

                    current_original_line += 1;

                    if *is_change {
                        current_modified_line += 1;
                    }
                }
                PatchLine::ContextUnchanged(_) => {
                    if reversed {
                        verify_patch_line(
                            line.original_line(),
                            &file.lines()[current_modified_line - 1],
                        )?;
                    } else {
                        verify_patch_line(
                            line.original_line(),
                            &file.lines()[current_original_line - 1],
                        )?;
                    }

                    current_modified_line += 1;
                    current_original_line += 1;
                }
                PatchLine::ContextHunkRange(_) => {}
                PatchLine::NoNewLine(_) => {}
                _ => panic!("Invalid PatchLine detected in context hunk!"),
            }
        }

        Ok(())
    }
}
