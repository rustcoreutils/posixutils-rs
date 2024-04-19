use std::path::Path;
use std::fs;
use std::io;
use chrono::{DateTime, Local};
use std::os::unix::fs::MetadataExt;

use std::os::unix::fs::PermissionsExt;
use crate::permissions::{*};
use std::path::PathBuf;

pub 
fn find_symlink_target(path: &Path) -> io::Result<Option<PathBuf>> {
    // Check if the path is a symlink
    let metadata = fs::symlink_metadata(path)?;
    if metadata.file_type().is_symlink() {
        // Read the symlink target
        fs::read_link(path).map(Some)
    } else {
        // Path is not a symlink
        Ok(None)
    }
}
pub fn get_symlink_size(path: &Path) -> io::Result<u64> {
    let metadata = fs::symlink_metadata(path)?;
    Ok(metadata.len()) // `len()` returns the size of the symlink
}
pub fn get_symlink_blocks(path: &Path) -> io::Result<u64> {
    let metadata = fs::symlink_metadata(path)?;
    Ok(metadata.blocks()) // `len()` returns the size of the symlink
}
pub fn get_symlink_inode(path: &Path) -> io::Result<u64> {
    let metadata = fs::symlink_metadata(path)?;
    Ok(metadata.ino()) // `len()` returns the size of the symlink
}
pub fn get_symlink_modified(path: &Path) -> io::Result<String> {
    let metadata = fs::symlink_metadata(path)?;
    let modified = DateTime::<Local>::from(metadata.modified().unwrap());
    let mut formatted_time = modified.format("%b %d %H:%M").to_string();
    Ok(formatted_time) // `len()` returns the size of the symlink
}
pub fn get_symlink_permissions(path: &Path) -> io::Result<String> {
    let metadata = fs::symlink_metadata(path)?;

    let permissions = metadata.permissions();
    let permissions = format_mode(permissions.mode());
    let forced_permissions = permissions; //format!("{}{}", 'l', &permissions[1..]);
                                          //println!("{}",forced_permissions);

    Ok(forced_permissions)
    // `len()` returns the size of the symlink
}