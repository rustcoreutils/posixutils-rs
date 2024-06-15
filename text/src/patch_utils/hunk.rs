use super::{
    context_hunk_data::{ContextHunkData, ContextHunkOrderIndex},
    context_hunk_range_data::ContextHunkRangeData,
    patch_format::PatchFormat,
    patch_line::PatchLine,
    unified_hunk_data::UnifiedHunkData,
};

#[derive(Debug)]
pub enum Hunk<'a> {
    UnifiedHunk(UnifiedHunkData<'a>),
    ContextHunk(ContextHunkData<'a>),
}

impl<'a> Hunk<'a> {
    pub fn new_context_hunk(data: ContextHunkData<'a>) -> Self {
        Self::ContextHunk(data)
    }

    pub fn new_unified_hunk(data: UnifiedHunkData<'a>) -> Self {
        Self::UnifiedHunk(data)
    }

    pub fn kind(&self) -> PatchFormat {
        match self {
            Hunk::UnifiedHunk(_) => PatchFormat::Unified,
            Hunk::ContextHunk(_) => PatchFormat::Context,
        }
    }

    pub fn context_hunk_data(&self) -> &ContextHunkData {
        match self {
            Hunk::ContextHunk(data) => data,
            _ => panic!("No context-hunk-data for variants other than ContextHunk"),
        }
    }

    pub fn context_patch_line_by_index(&self, index: &ContextHunkOrderIndex) -> &PatchLine<'_> {
        self.context_hunk_data().get_by_order_index(index)
    }

    pub fn context_hunk_ordered_lines_indeces(&self) -> &Option<Vec<ContextHunkOrderIndex>> {
        if let Hunk::ContextHunk(data) = self {
            data.ordered_lines_indeces()
        } else {
            panic!("No context-hunk-data for variants other than ContextHunk");
        }
    }

    pub fn context_hunk_data_mut(&mut self) -> &'a mut ContextHunkData {
        match self {
            Hunk::ContextHunk(data) => data,
            _ => panic!("No context-hunk-data for variants other than ContextHunk"),
        }
    }

    fn add_context_hunk_range_data(&'a mut self, range_data: &ContextHunkRangeData) {
        match self {
            Hunk::ContextHunk(data) => {
                if range_data.is_original() {
                    data.update_f1_range(range_data.range())
                } else {
                    data.enable_add_to_modified();
                    data.update_f2_range(range_data.range())
                }
            }
            _ => panic!("No context-hunk-data for variants other than ContextHunk"),
        };
    }

    pub fn unified_hunk_data(&self) -> &UnifiedHunkData<'a> {
        match self {
            Hunk::UnifiedHunk(data) => data,
            _ => panic!("No unified-hunk-data for variants other than UnifiedHunk"),
        }
    }

    pub fn unified_hunk_data_mut(&mut self) -> &mut UnifiedHunkData<'a> {
        match self {
            Hunk::UnifiedHunk(data) => data,
            _ => panic!("No unified-hunk-data for variants other than UnifiedHunk"),
        }
    }

    pub fn order_context_lines(&mut self) {
        if let Hunk::ContextHunk(data) = self {
            data.order_lines();
        }
    }

    pub fn add_patch_line(&mut self, patch_line: PatchLine<'a>) {
        match self {
            Hunk::UnifiedHunk(data) => data.add_patch_line(patch_line),
            Hunk::ContextHunk(data) => data.add_patch_line(patch_line),
        }
    }
}
