use super::{
    context_hunk_data::ContextHunkData, edit_script_hunk_data::EditScriptHunkData,
    normal_hunk_data::NormalHunkData, patch_format::PatchFormat, patch_line::PatchLine,
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

    pub fn unified_hunk_data(&self) -> &UnifiedHunkData<'a> {
        match self {
            Hunk::Unified(data) => data,
            _ => panic!("No unified-hunk-data for variants other than UnifiedHunk"),
        }
    }

    pub fn normal_hunk_data(&self) -> &NormalHunkData<'a> {
        match self {
            Hunk::Normal(data) => data,
            _ => panic!("No normal-hunk-data for variants other than Normal"),
        }
    }

    pub fn edit_script_hunk_data(&self) -> &EditScriptHunkData<'a> {
        match self {
            Hunk::EditScript(data) => data,
            _ => panic!("No edit-script-hunk-data for variants other than EditScript"),
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

    pub(crate) fn verify_hunk(&self) {
        match self {
            Hunk::Unified(data) => data.verify_hunk(),
            Hunk::Context(data) => data.verify_hunk(),
            Hunk::EditScript(data) => data.verify_hunk(),
            Hunk::Normal(data) => data.verify_hunk(),
        }
    }

    pub(crate) fn verify_file(
        &self,
        file: &super::patch_file::PatchFile,
        reversed: bool,
    ) -> Result<(), ()> {
        match self {
            Hunk::Unified(data) => data.verify_file(file, reversed),
            Hunk::Context(data) => data.verify_file(file, reversed),
            Hunk::EditScript(data) => data.verify_file(file),
            Hunk::Normal(data) => data.verify_file(file, reversed),
        }
    }
}
