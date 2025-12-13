//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::error::PaxResult;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Type of archive entry
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum EntryType {
    /// Regular file
    #[default]
    Regular,
    /// Directory
    Directory,
    /// Symbolic link
    Symlink,
    /// Hard link to another file
    Hardlink,
    /// Block device
    BlockDevice,
    /// Character device
    CharDevice,
    /// FIFO (named pipe)
    Fifo,
    /// Socket (not typically stored in archives, but recognized)
    Socket,
}

/// Metadata for an archive entry
#[derive(Debug, Clone, Default)]
pub struct ArchiveEntry {
    /// Path of the file within the archive
    pub path: PathBuf,
    /// File mode (permissions)
    pub mode: u32,
    /// User ID
    pub uid: u32,
    /// Group ID
    pub gid: u32,
    /// File size in bytes
    pub size: u64,
    /// Modification time (seconds since epoch)
    pub mtime: u64,
    /// Modification time nanoseconds (for pax format)
    pub mtime_nsec: u32,
    /// Access time (seconds since epoch, for pax format)
    pub atime: Option<u64>,
    /// Access time nanoseconds (for pax format)
    pub atime_nsec: u32,
    /// Type of entry
    pub entry_type: EntryType,
    /// Link target for symlinks and hardlinks
    pub link_target: Option<PathBuf>,
    /// User name (optional)
    pub uname: Option<String>,
    /// Group name (optional)
    pub gname: Option<String>,
    /// Device ID (for hard link tracking)
    pub dev: u64,
    /// Inode number (for hard link tracking)
    pub ino: u64,
    /// Number of hard links
    pub nlink: u32,
    /// Device major number (for block/char devices)
    pub devmajor: u32,
    /// Device minor number (for block/char devices)
    pub devminor: u32,
}

impl ArchiveEntry {
    /// Create a new archive entry with default values
    pub fn new(path: PathBuf, entry_type: EntryType) -> Self {
        ArchiveEntry {
            path,
            mode: 0o644,
            uid: 0,
            gid: 0,
            size: 0,
            mtime: 0,
            mtime_nsec: 0,
            atime: None,
            atime_nsec: 0,
            entry_type,
            link_target: None,
            uname: None,
            gname: None,
            dev: 0,
            ino: 0,
            nlink: 1,
            devmajor: 0,
            devminor: 0,
        }
    }

    /// Check if this entry is a special device file
    pub fn is_device(&self) -> bool {
        matches!(
            self.entry_type,
            EntryType::BlockDevice | EntryType::CharDevice
        )
    }

    /// Check if this is a directory
    pub fn is_dir(&self) -> bool {
        self.entry_type == EntryType::Directory
    }
}

/// Trait for reading archives
pub trait ArchiveReader {
    /// Read the next entry from the archive
    /// Returns None when the archive is exhausted
    fn read_entry(&mut self) -> PaxResult<Option<ArchiveEntry>>;

    /// Read the data for the current entry
    fn read_data(&mut self, buf: &mut [u8]) -> PaxResult<usize>;

    /// Skip the data for the current entry
    fn skip_data(&mut self) -> PaxResult<()>;
}

/// Trait for writing archives
pub trait ArchiveWriter {
    /// Write an entry header to the archive
    fn write_entry(&mut self, entry: &ArchiveEntry) -> PaxResult<()>;

    /// Write data for the current entry
    fn write_data(&mut self, data: &[u8]) -> PaxResult<()>;

    /// Finish writing data for the current entry (handles padding)
    fn finish_entry(&mut self) -> PaxResult<()>;

    /// Write the archive trailer
    fn finish(&mut self) -> PaxResult<()>;
}

/// Tracks hard links during archive creation
#[derive(Debug, Default)]
pub struct HardLinkTracker {
    /// Maps (dev, ino) to the first path seen
    seen: HashMap<(u64, u64), PathBuf>,
}

impl HardLinkTracker {
    /// Create a new tracker
    pub fn new() -> Self {
        HardLinkTracker {
            seen: HashMap::new(),
        }
    }

    /// Check if we've seen this file before (by dev/ino)
    /// Returns the original path if this is a hard link
    pub fn check(&mut self, entry: &ArchiveEntry) -> Option<PathBuf> {
        if entry.nlink <= 1 {
            return None;
        }

        let key = (entry.dev, entry.ino);
        if let Some(original) = self.seen.get(&key) {
            Some(original.clone())
        } else {
            self.seen.insert(key, entry.path.clone());
            None
        }
    }
}

/// Tracks extracted files for hard link creation during extraction
#[derive(Debug, Default)]
pub struct ExtractedLinks {
    /// Maps (dev, ino) to the extracted path
    extracted: HashMap<(u64, u64), PathBuf>,
}

impl ExtractedLinks {
    /// Create a new tracker
    pub fn new() -> Self {
        ExtractedLinks {
            extracted: HashMap::new(),
        }
    }

    /// Record that we extracted a file
    pub fn record(&mut self, entry: &ArchiveEntry, path: &Path) {
        if entry.nlink > 1 {
            let key = (entry.dev, entry.ino);
            self.extracted
                .entry(key)
                .or_insert_with(|| path.to_path_buf());
        }
    }

    /// Get the path to link to, if this is a hard link
    pub fn get_link_target(&self, entry: &ArchiveEntry) -> Option<&PathBuf> {
        if entry.nlink <= 1 {
            return None;
        }
        let key = (entry.dev, entry.ino);
        self.extracted.get(&key)
    }
}

/// Archive format type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArchiveFormat {
    /// POSIX ustar tar format
    Ustar,
    /// POSIX cpio format
    Cpio,
    /// POSIX pax format (extended tar with extended headers)
    Pax,
}

impl std::fmt::Display for ArchiveFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArchiveFormat::Ustar => write!(f, "ustar"),
            ArchiveFormat::Cpio => write!(f, "cpio"),
            ArchiveFormat::Pax => write!(f, "pax"),
        }
    }
}
