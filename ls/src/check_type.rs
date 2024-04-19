use std::path::Path;
use std::fs;
use std::os::unix::fs::PermissionsExt;

use std::os::unix::fs::FileTypeExt;

pub fn is_directory<P: AsRef<Path>>(path: P) -> bool {
    match fs::metadata(path) {
        Ok(metadata) => metadata.is_dir(),
        Err(e) => {
           // eprintln!("Failed to read metadata: {}", e);
            false
        }
    }
}

pub fn is_symlink2(path: &Path) -> bool {
  if let Ok(   metadata) = fs::symlink_metadata(path){
    return true
  }else{
    return false;
  }
}
pub fn is_symlink(path: &Path) -> bool {
    let metadata = fs::symlink_metadata(path).unwrap();
    return metadata.file_type().is_symlink();
}
pub fn is_executable<P: AsRef<Path>>(path: &P) -> bool {
    let metadata = match fs::metadata(path) {
        Ok(metadata) => metadata,
        Err(e) => {
           // eprintln!("Failed to read metadata: {}", e);
            return false;
        }
    };

    let permissions = metadata.permissions();
    (permissions.mode() & 0o111) != 0 // Check if any execute bits are set (owner, group, or others)
}

pub fn is_fifo<P: AsRef<Path>>(path: P) -> bool {
    match fs::metadata(path) {
        Ok(metadata) => metadata.file_type().is_fifo(),
        Err(e) => {
            //eprintln!("Failed to read metadata: {}", e);
            false
        }
    }
}