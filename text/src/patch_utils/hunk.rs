use super::{
    context_hunk_data::ContextHunkData, context_hunk_range_data::ContextHunkRangeData,
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

    pub fn context_hunk_data(&'a self) -> &'a ContextHunkData<'a> {
        match self {
            Hunk::ContextHunk(data) => data,
            _ => panic!("No context-hunk-data for variants other than ContextHunk"),
        }
    }

    pub fn context_hunk_data_mut(&'a mut self) -> &'a mut ContextHunkData {
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
        match self {
            Hunk::ContextHunk(data) => data.order_lines(),
            _ => {}
        }
    }
}
