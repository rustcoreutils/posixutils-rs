
use std::time::SystemTime;
pub struct DirEntryData {
   pub  name: String,
   pub    path: String,
    pub    is_dir: bool,
    pub    is_symlink: Option<bool>,

    pub    size: u64,
    pub    modified_time: Option<SystemTime>,
    pub    created_time: Option<SystemTime>,
    pub    modified_time_str: Option<String>,
    pub    symlink_target_name: Option<String>,
    pub   size_in_blocks: Option<u64>,
    pub   permissions: Option<String>,
    pub   nlinks: Option<u64>,
    pub   uid: Option<u32>,
    pub   gid: Option<u32>,
    pub   inode_and_name:Option<String>,
    pub   blocks_and_name:Option<String>,
    pub   inode: Option<u64>,
    pub    user_name: Option<String>,
    pub    blocks: Option<u64>,
    pub    has_extended_attributes: Option<bool>,
    pub    group_name: Option<String>,
    pub    file_type: Option<String>,
}
