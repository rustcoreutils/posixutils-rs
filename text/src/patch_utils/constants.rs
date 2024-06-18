pub const CONTEXT_HUNK_SEPARATOR: &str = "***************";
pub const CONTEXT_ORIGINAL_RANGE_STARTER: &str = "*** ";
pub const CONTEXT_ORIGINAL_RANGE_FINISHER: &str = " ****";
pub const CONTEXT_MODIFIED_RANGE_STARTER: &str = "--- ";
pub const CONTEXT_MODIFIED_RANGE_FINISHER: &str = " ----";
pub const UNIFIED_HUNK_HEADER_IDENTIFIER: &str = "@@";
pub const NO_NEW_LINE: &str = "\\ No newline at end of file";
pub const NO_NEW_LINE_ED: &str = "diff: f2.txt: No newline at end of file";
pub const UNIFIED_FIRST_LINE_STARTER: &str = "--- ";
pub const UNIFIED_SECOND_LINE_STARTER: &str = "+++ ";
pub const CONTEXT_FIRST_LINE_STARTER: &str = "*** ";
pub const CONTEXT_SECOND_LINE_STARTER: &str = "--- ";
pub const NORMAL_CHANGE_SEPARATOR: &str = "---";
pub const NORMAL_NEW_LINE_IDENT: &str = "> ";
pub const NORMAL_OLD_LINE_IDENT: &str = "< ";

pub mod regex {
    pub const NORMAL_FORMAT_RANGE_INSERT_REGEX: &str = r"^\d+a(\d+(,\d+)?|\d+)$";
    pub const NORMAL_FORMAT_RANGE_CHANGE_REGEX: &str = r"^(\d+(,\d+)?|\d+)c(\d+(,\d+)?|\d+)$";
    pub const NORMAL_FORMAT_RANGE_DELETE_REGEX: &str = r"^(\d+(,\d+)?|\d+)d\d+$";
}
