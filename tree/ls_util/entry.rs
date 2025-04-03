//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::ls_from_utf8_lossy;
use crate::{
    ClassifyFiles, Config, FileTimeOption, LongFormatOptions, OutputFormat,
    DATE_TIME_FORMAT_OLD_OR_FUTURE, DATE_TIME_FORMAT_RECENT,
};
use chrono::{DateTime, Local};
use std::cmp::Ordering;
use std::ffi::{CStr, OsStr, OsString};
use std::io;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::time::{Duration, SystemTime};

enum FileInfo {
    Size(u64),
    DeviceInfo((u32, u32)),
}

// A file in the file system.
pub struct Entry {
    file_info: FileInfo,
    blocks: u64,
    time: SystemTime,
    time_string: String,

    file_name_raw: OsString,   // Actual file name, might not be valid UTF-8
    file_name_display: String, // File name to be displayed

    blocks_str: Option<String>,
    inode_str: Option<String>,
    suffix: Option<char>,
    target_path: Option<String>,

    multi_column_padding: MultiColumnPadding,

    terminal_width: usize,

    long_format_data: Option<LongFormatData>,
}

impl Entry {
    pub fn new(
        target_path: Option<String>,
        file_name_raw: OsString,
        metadata: &ftw::Metadata,
        config: &Config,
    ) -> io::Result<Self> {
        let file_info = get_file_info(metadata);

        // This `SystemTime` *is* affected by -c or -u
        let (time, time_string) = get_time_and_time_string(metadata, &config.file_time_option)?;

        let blocks = metadata.blocks();
        let blocks_str = if config.display_size {
            // The 2 is because `crate::BLOCK_SIZE_KIBIBYTES` is double that of
            // `crate::BLOCK_SIZE`
            let b = if config.kibibytes { blocks / 2 } else { blocks };
            Some(format!("{}", b))
        } else {
            None
        };

        let inode_str = if config.inode {
            Some(format!("{}", metadata.ino()))
        } else {
            None
        };

        let long_format_data =
            if let OutputFormat::Long(long_format_options) = &config.output_format {
                Some(LongFormatData::new(metadata, long_format_options)?)
            } else {
                None
            };

        let suffix = match config.classify_files {
            ClassifyFiles::Complete => {
                if metadata.is_dir() {
                    Some('/')
                } else if metadata.is_symlink() {
                    Some('@')
                } else {
                    let file_type = metadata.file_type();
                    if file_type.is_fifo() {
                        Some('|')
                    } else {
                        let mode = metadata.mode();
                        if mode
                            & (libc::S_IXUSR as u32 | libc::S_IXGRP as u32 | libc::S_IXOTH as u32)
                            != 0
                        {
                            Some('*')
                        } else {
                            None
                        }
                    }
                }
            }
            ClassifyFiles::DirectoryOrNotDirectory => {
                if metadata.is_dir() {
                    Some('/')
                } else {
                    None
                }
            }
            ClassifyFiles::None => None,
        };

        let file_name_display = {
            let tmp = ls_from_utf8_lossy(file_name_raw.as_bytes());

            // -q
            if config.hide_control_chars {
                tmp.chars()
                    .map(|c| if c.is_control() || c == '\t' { '?' } else { c })
                    .collect()
            } else {
                tmp
            }
        };

        let mut file_name_width = file_name_display.chars().count();
        if suffix.is_some() {
            file_name_width += 1;
        }

        let mut total_width = file_name_width;

        let inode_str_width = if let Some(inode_str) = &inode_str {
            let n = inode_str.chars().count();
            total_width += 1 + n; // Add 1 for the space
            n
        } else {
            0
        };

        let blocks_str_width = if let Some(blocks_str) = &blocks_str {
            let n = blocks_str.chars().count();
            total_width += 1 + n; // Also adding 1 for the space
            n
        } else {
            0
        };

        let multi_column_padding = MultiColumnPadding {
            total_width,
            inode_str_width,
            blocks_str_width,
            file_name_width,
        };

        Ok(Self {
            file_info,
            blocks,
            time,
            time_string,
            file_name_raw,
            file_name_display,
            blocks_str,
            inode_str,
            suffix,
            target_path,
            multi_column_padding,
            terminal_width: config.terminal_width,
            long_format_data,
        })
    }

    /// Return the number of 512-byte blocks allocated to this file.
    pub fn blocks(&self) -> u64 {
        self.blocks
    }

    pub fn file_name_str(&self) -> &str {
        &self.file_name_display
    }

    /// Sets the width of the inode and blocks to be equal to of the `padding`.
    ///
    /// This is for coreutils compatibility. coreutils sets the column widths
    /// for inode to be all equal, and the same for blocks.
    pub fn recalculate_widths(&mut self, padding: &MultiColumnPadding) {
        assert!(padding.inode_str_width >= self.multi_column_padding.inode_str_width);
        assert!(padding.blocks_str_width >= self.multi_column_padding.blocks_str_width);

        let mut delta = 0;
        delta += padding.inode_str_width - self.multi_column_padding.inode_str_width;
        delta += padding.blocks_str_width - self.multi_column_padding.blocks_str_width;

        self.multi_column_padding.inode_str_width = padding.inode_str_width;
        self.multi_column_padding.blocks_str_width = padding.blocks_str_width;

        self.multi_column_padding.total_width += delta;
    }

    /// Return the `[inode] [blocks] filename` string.
    pub fn build_stream_mode_string(&self) -> String {
        let mut output = String::new();

        if let Some(inode_str) = &self.inode_str {
            output.push_str(inode_str);
            output.push(' ');
        }

        if let Some(blocks_str) = &self.blocks_str {
            output.push_str(blocks_str);
            output.push(' ');
        }

        output.push_str(self.file_name_str());

        if let Some(suffix) = &self.suffix {
            output.push(*suffix);
        }

        output
    }

    /// Print a single grid cell in multi-column format.
    pub fn print_multi_column(&self, padding: &MultiColumnPadding) {
        let MultiColumnPadding {
            total_width,
            inode_str_width,
            blocks_str_width,
            file_name_width,
        } = padding;

        let inode_str = if let Some(inode_str) = &self.inode_str {
            format!("{:>inode_str_width$} ", inode_str)
        } else {
            String::from("")
        };

        let blocks_str = if let Some(blocks_str) = &self.blocks_str {
            format!("{:>blocks_str_width$} ", blocks_str)
        } else {
            String::from("")
        };

        let mut file_name = self.file_name_str().to_string();
        if let Some(suffix) = &self.suffix {
            file_name.push(*suffix);
        }

        let mut file_name_width = *file_name_width;

        // This implies that this will be printed in a single column. Don't
        // inherit the padding for the longest string to avoid unnecessary
        // whitespaces.
        if *total_width > self.terminal_width {
            file_name_width = 0;
        }

        print!("{}{}{:<file_name_width$}", inode_str, blocks_str, file_name,);
    }

    /// Print one row in long format (-l).
    pub fn println_long_format(&self, padding: &LongFormatPadding) {
        // Destructure to get the individual fields
        let LongFormatPadding {
            blocks_str_width,
            inode_str_width,
            num_links_width,
            owner_name_width,
            group_name_width,
            file_size_width,
            device_id_major_width,
            device_id_minor_width,
            time_width,
        } = padding;

        let Some(long_format_data) = &self.long_format_data else {
            return;
        };

        let inode_str = if let Some(inode_str) = &self.inode_str {
            format!("{:>inode_str_width$} ", inode_str)
        } else {
            String::from("")
        };

        let blocks_str = if let Some(blocks_str) = &self.blocks_str {
            format!("{:>blocks_str_width$} ", blocks_str)
        } else {
            String::from("")
        };

        // Owner and group names are optional and can be disabled
        let owner_name = if let Some(owner_name) = &long_format_data.owner_name {
            format!(" {:<owner_name_width$}", owner_name)
        } else {
            String::from("")
        };
        let group_name = if let Some(group_name) = &long_format_data.group_name {
            format!(" {:<group_name_width$}", group_name)
        } else {
            String::from("")
        };

        let file_info = match &self.file_info {
            FileInfo::Size(size) => size.to_string(),
            FileInfo::DeviceInfo((major, minor)) => {
                format!(
                    "{:>device_id_major_width$}, {:>device_id_minor_width$}",
                    major, minor
                )
            }
        };

        let mut file_name = self.file_name_str().to_string();
        if let Some(suffix) = &self.suffix {
            file_name.push(*suffix);
        }
        if let Some(target_path) = &self.target_path {
            file_name.push_str(" -> ");
            file_name.push_str(target_path);
        }

        // Alignment and padding is not mentioned in the specification however
        // the example given there does have alignment and padding.
        //
        // The padding/width is calculated by looping through the directory
        // contents and using the length of the longest string for each column.
        //
        // As for the alignment, <number of links>, <size> or <device info>,
        // <date and time> are right-aligned and the rest are left-aligned.
        println!(
            "{}{}{} {:>num_links_width$}{}{} {:>file_size_width$} {:>time_width$} {}",
            inode_str,
            blocks_str,
            long_format_data.file_mode,
            long_format_data.num_links,
            owner_name,
            group_name,
            file_info,
            self.time_string,
            file_name
        );
    }

    /// Comparison key for sorting based on just the file name.
    pub fn sorting_cmp_lexicographic(&self, other: &Self) -> Ordering {
        self.file_name_raw.cmp(&other.file_name_raw)
    }

    // Returns (is_device, size, file_name). The `bool` is to have devices
    // sorted after normal files.
    fn sorting_key_size(&self) -> (bool, u64, &OsStr) {
        match self.file_info {
            FileInfo::Size(size) => (false, size, &self.file_name_raw),
            FileInfo::DeviceInfo(_) => (true, 0, &self.file_name_raw),
        }
    }

    /// Comparison key for sorting where the primary key is file size and the
    /// secondary key is the file name.
    ///
    /// Character and block devices that have no file size are ordered last with
    /// only the file name being the sorting key.
    pub fn sorting_cmp_size(&self, other: &Self) -> Ordering {
        let self_sorting_key = self.sorting_key_size();
        let other_sorting_key = other.sorting_key_size();

        match self_sorting_key.0.cmp(&other_sorting_key.0) {
            Ordering::Equal => {
                match self_sorting_key.1.cmp(&other_sorting_key.1) {
                    Ordering::Equal => self_sorting_key.2.cmp(other_sorting_key.2),
                    r => r.reverse(), // Default is from largest file size to smallest
                }
            }
            r => r,
        }
    }

    /// Comparison key for sorting where the primary key is time and the
    /// secondary key is the file name.
    ///
    /// The kind of time is dependent on the flags -t, -c, -u.
    pub fn sorting_cmp_time(&self, other: &Self) -> Ordering {
        match self.time.cmp(&other.time) {
            Ordering::Equal => self.file_name_raw.cmp(&other.file_name_raw),
            r => r.reverse(), // Default is newest to oldest
        }
    }

    pub fn get_multi_column_padding(&self) -> &MultiColumnPadding {
        &self.multi_column_padding
    }

    /// Return the minimum required padding for each column of this `Entry` in
    /// long format mode.
    pub fn get_long_format_padding(&self) -> LongFormatPadding {
        // Calculate the length of the resulting string when `v` is converted.
        fn decimal_str_len(v: u64) -> usize {
            if v == 0 {
                1
            } else {
                v.ilog10() as usize + 1
            }
        }

        let blocks_str_width = self.multi_column_padding.blocks_str_width;

        let inode_str_width = self.multi_column_padding.inode_str_width;

        let (num_links_width, owner_name_width, group_name_width) = self
            .long_format_data
            .as_ref()
            .map(|d| {
                let num_links_width = d.num_links.chars().count();
                let owner_name_width = d
                    .owner_name
                    .as_ref()
                    .map(|s| s.chars().count())
                    .unwrap_or(0);
                let group_name_width = d
                    .group_name
                    .as_ref()
                    .map(|s| s.chars().count())
                    .unwrap_or(0);
                (num_links_width, owner_name_width, group_name_width)
            })
            .unwrap_or((0, 0, 0));

        let file_size_width = match &self.file_info {
            FileInfo::Size(s) => decimal_str_len(*s),
            FileInfo::DeviceInfo(_) => 0,
        };

        let (device_id_major_width, device_id_minor_width) = match &self.file_info {
            FileInfo::Size(_) => (0, 0),
            FileInfo::DeviceInfo((major, minor)) => (
                decimal_str_len(*major as u64),
                decimal_str_len(*minor as u64),
            ),
        };

        let time_width = self.time_string.chars().count();

        LongFormatPadding {
            blocks_str_width,
            inode_str_width,
            num_links_width,
            owner_name_width,
            group_name_width,
            file_size_width,
            device_id_major_width,
            device_id_minor_width,
            time_width,
        }
    }
}

/// Used for padding in long format
#[derive(Default)]
pub struct LongFormatPadding {
    pub blocks_str_width: usize,
    pub inode_str_width: usize,
    pub num_links_width: usize,
    pub owner_name_width: usize,
    pub group_name_width: usize,
    pub file_size_width: usize,
    pub device_id_major_width: usize,
    pub device_id_minor_width: usize,
    pub time_width: usize,
}

impl LongFormatPadding {
    /// Get the maximum padding for each field.
    pub fn update_maximum(&mut self, other: &LongFormatPadding) {
        self.blocks_str_width = usize::max(self.blocks_str_width, other.blocks_str_width);
        self.inode_str_width = usize::max(self.inode_str_width, other.inode_str_width);
        self.num_links_width = usize::max(self.num_links_width, other.num_links_width);
        self.owner_name_width = usize::max(self.owner_name_width, other.owner_name_width);
        self.group_name_width = usize::max(self.group_name_width, other.group_name_width);
        self.file_size_width = usize::max(self.file_size_width, other.file_size_width);
        self.device_id_major_width =
            usize::max(self.device_id_major_width, other.device_id_major_width);
        self.device_id_minor_width =
            usize::max(self.device_id_minor_width, other.device_id_minor_width);
        self.time_width = usize::max(self.time_width, other.time_width);
    }

    /// Compare the width of the file size string with the width of the device
    /// ID string and use the largest.
    ///
    /// This needs to be called after looping through all the entries to get the
    /// maximum width for both file size and device ID.
    pub fn update_file_info_padding(&mut self) {
        let mut device_id_width = self.device_id_major_width + self.device_id_minor_width;
        if device_id_width > 0 {
            // +2 for the ", " separator
            device_id_width += 2;
        }

        self.file_size_width = usize::max(self.file_size_width, device_id_width);
    }
}

/// Used for padding in multi-column format
#[derive(Default)]
pub struct MultiColumnPadding {
    pub total_width: usize,
    pub inode_str_width: usize,
    pub blocks_str_width: usize,
    pub file_name_width: usize,
}

impl MultiColumnPadding {
    pub fn update_maximum(&mut self, other: &MultiColumnPadding) {
        self.total_width = usize::max(self.total_width, other.total_width);
        self.inode_str_width = usize::max(self.inode_str_width, other.inode_str_width);
        self.blocks_str_width = usize::max(self.blocks_str_width, other.blocks_str_width);
        self.file_name_width = usize::max(self.file_name_width, other.file_name_width);
    }
}

// Data that is only needed in long format mode.
struct LongFormatData {
    file_mode: String,
    num_links: String,
    owner_name: Option<String>,
    group_name: Option<String>,
}

impl LongFormatData {
    pub fn new(
        metadata: &ftw::Metadata,
        long_format_options: &LongFormatOptions,
    ) -> io::Result<Self> {
        let file_mode = get_file_mode_string(metadata);

        let num_links = metadata.nlink().to_string();

        let owner_name = if long_format_options.without_owner {
            None
        } else {
            Some(get_owner_name(
                metadata,
                long_format_options.numeric_uid_gid,
            )?)
        };

        let group_name = if long_format_options.without_group {
            None
        } else {
            Some(get_group_name(
                metadata,
                long_format_options.numeric_uid_gid,
            )?)
        };

        Ok(Self {
            file_mode,
            num_links,
            owner_name,
            group_name,
        })
    }
}

fn get_file_mode_string(metadata: &ftw::Metadata) -> String {
    let mut file_mode = String::with_capacity(10);

    let file_type = metadata.file_type();

    // Entry type
    file_mode.push(match file_type {
        ftw::FileType::SymbolicLink => 'l',
        ftw::FileType::BlockDevice => 'b',
        ftw::FileType::Directory => 'd',
        ftw::FileType::CharacterDevice => 'c',
        ftw::FileType::Fifo => 'p',
        _ => '-',
    });

    let mode = metadata.mode();

    // Owner permissions
    file_mode.push(if mode & (libc::S_IRUSR as u32) != 0 {
        'r'
    } else {
        '-'
    });
    file_mode.push(if mode & (libc::S_IWUSR as u32) != 0 {
        'w'
    } else {
        '-'
    });
    file_mode.push({
        let executable = mode & (libc::S_IXUSR as u32) != 0;
        let set_user_id = mode & (libc::S_ISUID as u32) != 0;
        match (executable, set_user_id) {
            (true, true) => 's',
            (true, false) => 'x',
            (false, true) => 'S',
            (false, false) => '-',
        }
    });

    // Group permissions
    file_mode.push(if mode & (libc::S_IRGRP as u32) != 0 {
        'r'
    } else {
        '-'
    });
    file_mode.push(if mode & (libc::S_IWGRP as u32) != 0 {
        'w'
    } else {
        '-'
    });
    file_mode.push(if mode & (libc::S_IXGRP as u32) != 0 {
        'x'
    } else {
        '-'
    });

    // Other permissions
    file_mode.push(if mode & (libc::S_IROTH as u32) != 0 {
        'r'
    } else {
        '-'
    });
    file_mode.push(if mode & (libc::S_IWOTH as u32) != 0 {
        'w'
    } else {
        '-'
    });
    file_mode.push({
        if file_type.is_dir() {
            let searchable = mode & (libc::S_IXOTH as u32) != 0;
            let restricted_deletion = mode & (libc::S_ISVTX as u32) != 0;
            match (searchable, restricted_deletion) {
                (true, true) => 't',
                (true, false) => 'x',
                (false, true) => 'T',
                (false, false) => '-',
            }
        } else {
            if mode & (libc::S_IXOTH as u32) != 0 {
                'x'
            } else {
                '-'
            }
        }
    });

    file_mode
}

fn get_owner_name(metadata: &ftw::Metadata, numeric: bool) -> io::Result<String> {
    let uid = metadata.uid();
    if numeric {
        Ok(uid.to_string())
    } else {
        unsafe {
            let passwd = libc::getpwuid(uid);
            if passwd.is_null() {
                return Err(io::Error::last_os_error());
            }
            let passwd_ref = &*passwd;
            let name = CStr::from_ptr(passwd_ref.pw_name);
            Ok(ls_from_utf8_lossy(name.to_bytes()))
        }
    }
}

fn get_group_name(metadata: &ftw::Metadata, numeric: bool) -> io::Result<String> {
    let gid = metadata.gid();
    if numeric {
        Ok(gid.to_string())
    } else {
        unsafe {
            let group = libc::getgrgid(gid);
            if group.is_null() {
                return Err(io::Error::last_os_error());
            }
            let group_ref = &*group;
            let name = CStr::from_ptr(group_ref.gr_name);
            Ok(ls_from_utf8_lossy(name.to_bytes()))
        }
    }
}

fn get_file_info(metadata: &ftw::Metadata) -> FileInfo {
    match metadata.file_type() {
        ftw::FileType::CharacterDevice | ftw::FileType::BlockDevice => {
            let device_id = metadata.rdev();

            // major ID - class of the device
            // minor ID - specific instance of a device
            let (major, minor) = {
                (
                    libc::major(device_id as libc::dev_t),
                    libc::minor(device_id as libc::dev_t),
                )
            };

            FileInfo::DeviceInfo((major as u32, minor as u32))
        }
        _ => FileInfo::Size(metadata.size()),
    }
}

fn get_system_time(
    metadata: &ftw::Metadata,
    file_time_option: &FileTimeOption,
) -> io::Result<SystemTime> {
    let seconds_since_epoch = match file_time_option {
        FileTimeOption::LastModificationTime => {
            u64::try_from(metadata.mtime()).map_err(|_| io::Error::other("negative mtime"))?
        }
        FileTimeOption::LastAcessTime => {
            u64::try_from(metadata.atime()).map_err(|_| io::Error::other("negative atime"))?
        }
        FileTimeOption::LastStatusChangeTime => {
            u64::try_from(metadata.ctime()).map_err(|_| io::Error::other("negative ctime"))?
        }
    };

    let time = SystemTime::UNIX_EPOCH
        .checked_add(Duration::from_secs(seconds_since_epoch))
        .ok_or(io::Error::other("`SystemTime` overflow"))?;
    Ok(time)
}

fn get_time_and_time_string(
    metadata: &ftw::Metadata,
    file_time_option: &FileTimeOption,
) -> io::Result<(SystemTime, String)> {
    let time = get_system_time(metadata, file_time_option)?;

    let dt_format = {
        // The specification says to base it of "last modification time"
        // without any mention of -c or -u
        let last_modified_time = get_system_time(metadata, &FileTimeOption::LastModificationTime)?;
        let now = SystemTime::now();

        match now.duration_since(last_modified_time) {
            Ok(duration) => {
                const SIX_MONTHS: Duration = Duration::from_secs(3600 * 24 * 30 * 6);
                if duration > SIX_MONTHS {
                    DATE_TIME_FORMAT_OLD_OR_FUTURE // Old
                } else {
                    DATE_TIME_FORMAT_RECENT
                }
            }
            Err(_) => DATE_TIME_FORMAT_OLD_OR_FUTURE, // Future
        }
    };

    let time_string = {
        // chrono parses the TZ environment variable in this conversion
        let dt: DateTime<Local> = time.into();
        dt.format(dt_format).to_string()
    };
    Ok((time, time_string))
}
