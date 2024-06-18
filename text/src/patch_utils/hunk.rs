use super::{
    context_hunk_data::{ContextHunkData, ContextHunkOrderIndex},
    context_hunk_range_data::ContextHunkRangeData,
    edit_script_hunk_data::EditScriptHunkData,
    normal_hunk_data::NormalHunkData,
    patch_format::PatchFormat,
    patch_line::PatchLine,
    unified_hunk_data::UnifiedHunkData,
};

#[derive(Debug)]
pub enum Hunk<'a> {
    Unified(UnifiedHunkData<'a>),
    Context(ContextHunkData<'a>),
    EditScript(EditScriptHunkData<'a>),
    Normal(NormalHunkData<'a>),
}

impl<'a> Hunk<'a> {
    pub fn new_context_hunk(data: ContextHunkData<'a>) -> Self {
        Self::Context(data)
    }

    pub fn new_unified_hunk(data: UnifiedHunkData<'a>) -> Self {
        Self::Unified(data)
    }

    pub fn new_edit_script_hunk(data: EditScriptHunkData<'a>) -> Self {
        Self::EditScript(data)
    }

    pub(crate) fn new_normal_hunk(data: NormalHunkData<'a>) -> Hunk {
        Self::Normal(data)
    }

    pub fn kind(&self) -> PatchFormat {
        match self {
            Hunk::Unified(_) => PatchFormat::Unified,
            Hunk::Context(_) => PatchFormat::Context,
            Hunk::EditScript(_) => PatchFormat::EditScript,
            Hunk::Normal(_) => PatchFormat::Normal,
        }
    }

    pub fn context_hunk_data(&self) -> &ContextHunkData {
        match self {
            Hunk::Context(data) => data,
            _ => panic!("No context-hunk-data for variants other than ContextHunk"),
        }
    }

    pub fn context_patch_line_by_index(&self, index: &ContextHunkOrderIndex) -> &PatchLine<'_> {
        self.context_hunk_data().get_by_order_index(index)
    }

    pub fn context_hunk_ordered_lines_indeces(&self) -> &Option<Vec<ContextHunkOrderIndex>> {
        if let Hunk::Context(data) = self {
            data.ordered_lines_indeces()
        } else {
            panic!("No context-hunk-data for variants other than ContextHunk");
        }
    }

    pub fn context_hunk_data_mut(&mut self) -> &'a mut ContextHunkData {
        match self {
            Hunk::Context(data) => data,
            _ => panic!("No context-hunk-data for variants other than ContextHunk"),
        }
    }

    fn add_context_hunk_range_data(&'a mut self, range_data: &ContextHunkRangeData) {
        match self {
            Hunk::Context(data) => {
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
            Hunk::Unified(data) => data,
            _ => panic!("No unified-hunk-data for variants other than UnifiedHunk"),
        }
    }

    pub fn unified_hunk_data_mut(&mut self) -> &mut UnifiedHunkData<'a> {
        match self {
            Hunk::Unified(data) => data,
            _ => panic!("No unified-hunk-data for variants other than UnifiedHunk"),
        }
    }

    pub fn edit_script_hunk_data(&self) -> &EditScriptHunkData<'a> {
        match self {
            Hunk::EditScript(data) => data,
            _ => panic!("No edit-script-hunk-data for variants other than EditScript"),
        }
    }

    pub fn order_context_lines(&mut self) {
        if let Hunk::Context(data) = self {
            data.order_lines();
        }
    }

    pub fn add_patch_line(&mut self, patch_line: PatchLine<'a>) {
        match self {
            Hunk::Unified(data) => data.add_patch_line(patch_line),
            Hunk::Context(data) => data.add_patch_line(patch_line),
            Hunk::EditScript(data) => data.add_patch_line(patch_line),
            Hunk::Normal(data) => data.add_patch_line(patch_line),
        }
    }
}
