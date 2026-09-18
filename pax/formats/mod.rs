//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Archive format implementations

pub mod cpio;
pub mod pax;
pub mod ustar;

use crate::error::{PaxError, PaxResult};
use std::io::Read;

/// Largest extended-header record set (pax `x`/`g`) this will accept.
pub const MAX_EXTENDED_HEADER: u64 = 64 * 1024 * 1024;

/// Largest pathname or symbolic-link target this will accept from a header.
///
/// `PATH_MAX` is 4096 on Linux and 1024 on macOS; this is far above both, so it
/// rejects only lengths no filesystem could name anyway.
pub const MAX_NAME: u64 = 64 * 1024;

/// Read exactly the `len` bytes a header claims, without trusting `len` enough to
/// allocate it up front.
///
/// Header length fields are attacker-controlled. Sizing a buffer from one lets a
/// 512-byte archive declare 4 GiB and abort the process on the allocation alone,
/// before a single byte of it has been read. `limit` is what the format could
/// legitimately need; past that the archive is rejected by name. Within it the
/// buffer grows only as bytes actually arrive, so a header that lies about a
/// length it cannot supply costs what it delivers and then hits end-of-file.
pub fn read_declared<R: Read>(
    reader: &mut R,
    len: u64,
    limit: u64,
    what: &str,
) -> PaxResult<Vec<u8>> {
    if len > limit {
        return Err(PaxError::InvalidHeader(format!(
            "{what} of {len} bytes exceeds the {limit} byte limit"
        )));
    }

    let mut buf = Vec::new();
    let mut chunk = [0u8; 8192];
    let mut remaining = len as usize;
    while remaining > 0 {
        let n = remaining.min(chunk.len());
        reader.read_exact(&mut chunk[..n])?;
        buf.extend_from_slice(&chunk[..n]);
        remaining -= n;
    }
    Ok(buf)
}

pub use cpio::{checksum_bytes, CpioFormat, CpioReader, CpioWriter};
pub use pax::{PaxReader, PaxWriter};
pub use ustar::{UstarReader, UstarWriter};
