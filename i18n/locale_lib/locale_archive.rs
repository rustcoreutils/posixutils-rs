use std::{
    error::Error,
    fmt::Display,
    fs::File,
    io::{Read, Seek, SeekFrom},
    path::Path,
};

use bytemuck::{Pod, Zeroable};

const LOCAL_ARCHIVE_MAGIC_HEADER: u32 = 0xde020109;
const LOCALE_ARCHIVE_PATH: &str = "/usr/lib/locale/locale-archive";

#[repr(C)]
#[derive(Debug, Copy, Clone, Pod, Zeroable)]
struct LocalArchiveHeader {
    magic_header: u32,

    serial_number: u32,

    name_hash_offset: u32,

    total_name_hash_used: u32,

    total_name_hash_size: u32,

    string_offset: u32,

    total_string_used: u32,

    total_string_size: u32,

    locrectab_offset: u32,

    total_locrectab_used: u32,

    total_locrectab_size: u32,

    sum_hash_offset: u32,

    total_sum_hash_used: u32,

    total_sum_hash_size: u32,
}

#[repr(C)]
#[derive(Debug, Copy, Clone, Pod, Zeroable)]
struct NameHashEntry {
    hash_value: u32,

    name_offset: u32,

    locrec_offset: u32,
}

#[derive(Debug)]
pub enum ReadLocaleArchiveError {
    InvalidMagicNumber,
}

impl Error for ReadLocaleArchiveError {}

impl Display for ReadLocaleArchiveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReadLocaleArchiveError::InvalidMagicNumber => {
                write!(f, "Invalid magic number in locale archive")
            }
        }
    }
}

pub fn read_locale_archive() -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let path = Path::new(LOCALE_ARCHIVE_PATH);
    let mut file = File::open(path)?;

    let mut buf = [0u8; size_of::<LocalArchiveHeader>()];
    file.read_exact(&mut buf).unwrap();
    let header =
        bytemuck::from_bytes::<LocalArchiveHeader>(&buf[..size_of::<LocalArchiveHeader>()]);

    if header.magic_header != LOCAL_ARCHIVE_MAGIC_HEADER {
        return Err(ReadLocaleArchiveError::InvalidMagicNumber.into());
    }

    let mut buf = vec![0u8; header.total_name_hash_size as usize * size_of::<NameHashEntry>()];
    file.read_exact(&mut buf).unwrap();
    let name_hash_entries = bytemuck::cast_slice::<u8, NameHashEntry>(&buf);

    let mut local_names = Vec::new();
    for entry in name_hash_entries {
        if entry.locrec_offset != 0 {
            let mut name = Vec::new();
            file.seek(SeekFrom::Start(entry.name_offset as u64))?;

            let mut buffer = [0u8; 1];
            loop {
                match file.read_exact(&mut buffer) {
                    Ok(_) => {
                        if buffer[0] == 0 {
                            break;
                        }
                        name.push(buffer[0]);
                    }
                    Err(_) => break,
                }
            }

            if let Ok(locale_name) = String::from_utf8(name) {
                local_names.push(locale_name);
            }
        }
    }

    Ok(local_names)
}
