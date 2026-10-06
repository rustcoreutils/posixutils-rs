//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Multi-volume archive support (GNU tar compatible)
//!
//! A multi-volume archive is one archive stored in pieces: `archive`,
//! `archive.2`, `archive.3`, ... Read back, the volumes are one byte stream,
//! as GNU tar treats them, and that stream is read by the same reader as any
//! single archive.
//!
//! ## How one volume leads to the next
//!
//! - The last volume ends with the end-of-archive indicator. No other volume
//!   has one: a volume that ends without it is followed by another. That is
//!   what makes a missing volume an error rather than the end of the
//!   archive, and what keeps a stale `archive.N` left by an earlier, longer
//!   run from being read as part of this one.
//! - A volume may start with a GNU volume label ('V'), which is not a member.
//! - GNU tar divides a member between volumes: the next volume then starts
//!   with an 'M' header for the rest of its data. The header is stepped
//!   over, so the data reads on as the one member's.
//!
//! ## Limitations
//!
//! - pax does not *write* split members: a volume holds whole members only,
//!   and a member too large for one volume is refused with a diagnostic.
//! - pax writes only the ustar format, and nothing compressed.
//! - GNU tar's POSIX-format continuation (`GNU.volume.*` records) is refused.

use crate::archive::{ArchiveEntry, ArchiveWriter};
use crate::blocked_io::{BlockedReader, BlockedWriter};
use crate::error::{PaxError, PaxResult};
use crate::formats::ustar::{
    build_header, is_zero_block, parse_numeric, stores_data, verify_checksum, SIZE_OFF,
    TYPEFLAG_OFF,
};
use crate::modes::write::ArchiveFiles;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

const BLOCK_SIZE: usize = 512;

/// GNU tar type flag for multi-volume continuation
const GNUTYPE_MULTIVOL: u8 = b'M';

/// GNU tar type flag for a volume label
const GNUTYPE_VOLHDR: u8 = b'V';

/// Options for multi-volume archive operations
#[derive(Clone, Default)]
pub struct MultiVolumeOptions {
    /// Maximum size per volume in bytes (None = unlimited)
    pub volume_size: Option<u64>,
    /// Script to run when changing volumes (None = prompt user)
    pub volume_script: Option<String>,
    /// Base path for archive files
    pub archive_path: PathBuf,
    /// Whether to run in verbose mode
    pub verbose: bool,
    /// Standard input is pax's own -- the list of names to archive -- so the
    /// script is not given it: whatever it read would be names lost.
    pub stdin_in_use: bool,
}

/// The path of volume `volume` (1-based): the archive's own for the first,
/// then the archive's bytes with `.N` after them.
fn volume_path(archive: &Path, volume: u32) -> PathBuf {
    let mut path = archive.as_os_str().to_owned();
    if volume > 1 {
        path.push(format!(".{volume}"));
    }
    PathBuf::from(path)
}

/// Have volume `volume` made ready at `path`: run the volume script, or ask
/// on the terminal.
fn request_volume(options: &MultiVolumeOptions, volume: u32, path: &Path) -> PaxResult<()> {
    let requested = match options.volume_script {
        Some(ref script) => run_volume_script(script, options.stdin_in_use, volume, path),
        None => prompt_volume_change(volume, path),
    };
    requested.map_err(|e| match e {
        PaxError::Io(e) => volume_error(volume, path, e),
        e => e,
    })
}

/// Run the volume script for volume `volume`, without standard input when
/// pax is reading that itself.
fn run_volume_script(script: &str, no_stdin: bool, volume: u32, path: &Path) -> PaxResult<()> {
    let mut command = Command::new("sh");
    command
        .arg("-c")
        .arg(script)
        .env("TAR_VOLUME", volume.to_string())
        .env("TAR_ARCHIVE", path);
    if no_stdin {
        command.stdin(Stdio::null());
    }
    let status = command.status()?;
    if !status.success() {
        return Err(PaxError::Io(io::Error::other(format!(
            "volume script failed ({status})"
        ))));
    }
    Ok(())
}

/// An I/O error about volume `volume`, saying which volume it is.
fn volume_error(volume: u32, path: &Path, e: io::Error) -> PaxError {
    let what = format!("volume {volume} ({}): {e}", path.display());
    PaxError::Io(io::Error::new(e.kind(), what))
}

/// Ask on `/dev/tty` for the next volume to be made ready, and wait for the
/// response. End of file there ends the run, as it does for `-i`.
fn prompt_volume_change(volume: u32, path: &Path) -> PaxResult<()> {
    use std::io::BufRead;

    let tty_error = |e: io::Error| io::Error::new(e.kind(), format!("/dev/tty: {e}"));
    let tty_read = File::open("/dev/tty").map_err(tty_error)?;
    let mut tty_write = std::fs::OpenOptions::new()
        .write(true)
        .open("/dev/tty")
        .map_err(tty_error)?;

    let mut msg = format!("\nPrepare volume #{} for '", volume).into_bytes();
    crate::escape::push_escaped(
        &mut msg,
        crate::rawpath::as_bytes(path),
        crate::escape::Style::TTY,
    );
    msg.extend_from_slice(b"' and press ENTER: ");
    tty_write.write_all(&msg)?;
    tty_write.flush()?;

    let mut line = Vec::new();
    if std::io::BufReader::new(tty_read).read_until(b'\n', &mut line)? == 0 {
        return Err(PaxError::TtyEof);
    }
    Ok(())
}

/// Say which volume is being opened, under -v.
fn announce(options: &MultiVolumeOptions, what: &str, volume: u32, path: &Path) {
    if options.verbose {
        let mut line = format!("pax: {what} volume {volume} (").into_bytes();
        line.extend_from_slice(crate::rawpath::as_bytes(path));
        line.push(b')');
        crate::escape::write_stderr_line(&line);
    }
}

/// Tracks state during multi-volume archive writing
pub struct MultiVolumeWriter {
    /// Current volume number (1-based)
    current_volume: u32,
    /// Bytes written to current volume
    bytes_written: u64,
    /// Maximum bytes per volume
    volume_size: u64,
    /// Options for volume handling
    options: MultiVolumeOptions,
    /// Bytes per write to a volume (-b)
    record_size: usize,
    /// Current output file; `None` only after a volume change failed
    writer: Option<BlockedWriter<File>>,
    /// The member being written stores no data (a link): drop what is offered.
    skip_data: bool,
    /// Where each volume is recorded as it is created, so the walk does not
    /// archive a volume into itself.
    archive_files: ArchiveFiles,
}

impl MultiVolumeWriter {
    /// Create a new multi-volume writer that writes `record_size` bytes at a
    /// time, adding each volume to `archive_files`.
    pub fn new(
        options: MultiVolumeOptions,
        record_size: usize,
        archive_files: ArchiveFiles,
    ) -> PaxResult<Self> {
        let volume_size = options.volume_size.unwrap_or(u64::MAX);

        let mut writer = MultiVolumeWriter {
            current_volume: 1,
            bytes_written: 0,
            volume_size,
            options,
            record_size,
            writer: None,
            skip_data: false,
            archive_files,
        };
        writer.open_volume()?;
        Ok(writer)
    }

    /// Create the file of the current volume.
    fn open_volume(&mut self) -> PaxResult<()> {
        let path = volume_path(&self.options.archive_path, self.current_volume);
        announce(&self.options, "opening", self.current_volume, &path);
        let file = File::create(&path)?;
        self.archive_files.add(&file.metadata()?);
        self.writer = Some(BlockedWriter::new(file, self.record_size));
        Ok(())
    }

    /// Close the current volume and open the next.
    ///
    /// The volume ends without the end-of-archive indicator, and without zero
    /// padding out to a whole record, which a reader would take for one: that
    /// another volume follows is said by there being none. It is closed
    /// before the script runs or the prompt is shown, since that is when it
    /// is taken away.
    fn next_volume(&mut self) -> PaxResult<()> {
        if let Some(writer) = self.writer.take() {
            writer.unpadded_last_record().close()?;
        }
        self.current_volume += 1;
        self.bytes_written = 0;
        let path = volume_path(&self.options.archive_path, self.current_volume);
        request_volume(&self.options, self.current_volume, &path)?;
        self.open_volume()
    }

    fn writer(&mut self) -> PaxResult<&mut BlockedWriter<File>> {
        self.writer
            .as_mut()
            .ok_or_else(|| PaxError::Io(io::Error::other("no volume open")))
    }

    /// The size of a volume holding `content` bytes of members: they are
    /// followed by the end-of-archive marker, and the last record is padded
    /// out whole. A volume is filled only as far as it could be the last.
    fn volume_bytes(&self, content: u64) -> u64 {
        let record = self.record_size as u64;
        (content + 2 * BLOCK_SIZE as u64).div_ceil(record) * record
    }
}

impl ArchiveWriter for MultiVolumeWriter {
    fn write_entry(&mut self, entry: &ArchiveEntry) -> PaxResult<()> {
        // A header the format refuses is refused before the volume is
        // changed for it, which would leave a volume with nothing on it.
        let header = build_header(entry)?;

        // A member is never divided between volumes, so it has to fit in one
        // whole. Refuse it up front: the alternative is streaming the payload
        // past the tape length and producing a volume that silently exceeds the
        // limit the user asked for.
        let data = if stores_data(entry.entry_type) {
            entry.size
        } else {
            0
        };
        let needed = BLOCK_SIZE as u64 + data.div_ceil(BLOCK_SIZE as u64) * BLOCK_SIZE as u64;
        if self.volume_bytes(needed) > self.volume_size {
            return Err(PaxError::InvalidFormat(format!(
                "{}: {} bytes does not fit in a {}-byte volume",
                entry.path.display(),
                entry.size,
                self.volume_size
            )));
        }

        if self.volume_bytes(self.bytes_written + needed) > self.volume_size {
            self.next_volume()?;
        }

        self.skip_data = !stores_data(entry.entry_type);
        self.writer()?.write_all(&header)?;
        self.bytes_written += BLOCK_SIZE as u64;
        Ok(())
    }

    fn write_data(&mut self, data: &[u8]) -> PaxResult<()> {
        // A link's target is in its header; nothing follows it.
        if self.skip_data {
            return Ok(());
        }
        self.writer()?.write_all(data)?;
        self.bytes_written += data.len() as u64;
        Ok(())
    }

    fn finish_entry(&mut self) -> PaxResult<()> {
        // Pad to block boundary
        let remainder = (self.bytes_written % BLOCK_SIZE as u64) as usize;
        if remainder != 0 {
            let padding = BLOCK_SIZE - remainder;
            self.writer()?.write_all(&[0u8; BLOCK_SIZE][..padding])?;
            self.bytes_written += padding as u64;
        }
        Ok(())
    }

    fn finish(&mut self) -> PaxResult<()> {
        // A volume change that failed has been reported, and left nothing
        // open to finish.
        let Some(mut writer) = self.writer.take() else {
            return Ok(());
        };
        writer.write_all(&[0u8; 2 * BLOCK_SIZE])?;
        writer.close()?;

        if self.options.verbose {
            eprintln!("pax: wrote {} volume(s)", self.current_volume);
        }
        Ok(())
    }
}

/// The volumes of a multi-volume archive, read as one byte stream.
///
/// A volume's end of file is not the end of the stream: the archive's own
/// end-of-archive indicator is, and whatever reads the stream stops there. So
/// the end of a volume is where the next one is opened -- after the script
/// has run or the user has been asked, when it is not there.
pub struct VolumeChain {
    options: MultiVolumeOptions,
    /// The volume being read (1-based)
    volume: u32,
    current: BlockedReader<File>,
    /// Largest record the reads must hold (-b)
    record_size: usize,
}

impl VolumeChain {
    /// The chain starting at the first volume, which must be there.
    pub fn open(options: MultiVolumeOptions, record_size: usize) -> PaxResult<Self> {
        let path = volume_path(&options.archive_path, 1);
        let file = File::open(&path)?;
        announce(&options, "reading", 1, &path);
        let mut chain = VolumeChain {
            options,
            volume: 1,
            current: BlockedReader::new(file).with_record_size(record_size),
            record_size,
        };
        chain.step_over_volume_headers()?;
        Ok(chain)
    }

    /// Open the volume after the current one.
    fn next_volume(&mut self) -> PaxResult<()> {
        self.volume += 1;
        let path = volume_path(&self.options.archive_path, self.volume);
        if !path.exists() {
            request_volume(&self.options, self.volume, &path)?;
        }
        let file = File::open(&path).map_err(|e| volume_error(self.volume, &path, e))?;
        announce(&self.options, "reading", self.volume, &path);
        self.current = BlockedReader::new(file).with_record_size(self.record_size);
        self.step_over_volume_headers()
    }

    /// Once the archive has ended, say so of a next volume that is there all
    /// the same: left by an earlier, longer run, or the rest of a set whose
    /// every volume ends with the indicator -- as pax used to write them.
    /// It is not read either way, but not silently.
    pub fn warn_of_next_volume(&self) {
        let path = volume_path(&self.options.archive_path, self.volume + 1);
        if path.exists() {
            crate::error::report_warning(&*path, "follows the end of the archive; not read");
        }
    }

    /// The header block the current volume continues with, if the next
    /// block is one.
    fn peek_header(&mut self) -> io::Result<Option<[u8; BLOCK_SIZE]>> {
        let peeked = self.current.peek(BLOCK_SIZE)?;
        let Some(block) = peeked.get(..BLOCK_SIZE) else {
            return Ok(None);
        };
        let block: [u8; BLOCK_SIZE] = block.try_into().expect("one block");
        Ok((!is_zero_block(&block) && verify_checksum(&block)).then_some(block))
    }

    /// Step over what starts a volume without being part of the archive: a
    /// volume label, and on a later volume the header of the member that
    /// carries on from the one before.
    fn step_over_volume_headers(&mut self) -> PaxResult<()> {
        let mut header = self.peek_header()?;
        if let Some(label) = header.filter(|h| h[TYPEFLAG_OFF] == GNUTYPE_VOLHDR) {
            let size = parse_numeric(&label[SIZE_OFF..SIZE_OFF + 12])?;
            let skip = BLOCK_SIZE as u64 + size.div_ceil(BLOCK_SIZE as u64) * BLOCK_SIZE as u64;
            let skipped = io::copy(&mut (&mut self.current).take(skip), &mut io::sink())?;
            if skipped < skip {
                return Err(crate::formats::truncated_header());
            }
            header = self.peek_header()?;
        }
        let Some(header) = header else {
            return Ok(());
        };
        match header[TYPEFLAG_OFF] {
            GNUTYPE_MULTIVOL if self.volume == 1 => Err(PaxError::InvalidFormat(
                "the first volume continues a member from an earlier one: \
                 the volumes are out of order"
                    .to_string(),
            )),
            GNUTYPE_MULTIVOL => {
                // The rest of the member's data follows it.
                self.current.read_exact(&mut [0u8; BLOCK_SIZE])?;
                Ok(())
            }
            b'x' if self.volume > 1 && self.is_posix_continuation(&header)? => {
                Err(PaxError::InvalidFormat(
                    "a GNU tar POSIX-format continuation volume is not supported".to_string(),
                ))
            }
            _ => Ok(()),
        }
    }

    /// Whether an `x` header at the start of a volume is GNU tar's
    /// continuation of a divided member -- `GNU.volume.*` records -- which
    /// would otherwise be read as a new member holding the rest of the data.
    fn is_posix_continuation(&mut self, header: &[u8; BLOCK_SIZE]) -> PaxResult<bool> {
        let size = parse_numeric(&header[SIZE_OFF..SIZE_OFF + 12])?;
        let want = BLOCK_SIZE + size.min(crate::formats::MAX_EXTENDED_HEADER) as usize;
        let peeked = self.current.peek(want)?;
        let records = &peeked[BLOCK_SIZE.min(peeked.len())..];
        Ok(records.windows(11).any(|w| w == b"GNU.volume."))
    }
}

impl Read for VolumeChain {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }
        loop {
            let n = self.current.read(buf)?;
            if n > 0 {
                return Ok(n);
            }
            self.next_volume().map_err(|e| match e {
                PaxError::Io(e) => e,
                e => io::Error::other(e.to_string()),
            })?;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_volume_path() {
        let base = Path::new("/tmp/test.tar");
        assert_eq!(volume_path(base, 1), PathBuf::from("/tmp/test.tar"));
        assert_eq!(volume_path(base, 2), PathBuf::from("/tmp/test.tar.2"));
        assert_eq!(volume_path(base, 3), PathBuf::from("/tmp/test.tar.3"));
    }

    #[test]
    fn test_volume_path_keeps_the_name_bytes() {
        use std::ffi::OsStr;
        use std::os::unix::ffi::OsStrExt;
        let base = Path::new(OsStr::from_bytes(b"v\xff"));
        assert_eq!(
            volume_path(base, 2).as_os_str().as_bytes(),
            b"v\xff.2".as_slice()
        );
    }

    #[test]
    fn test_volume_bytes_counts_trailer_and_record_padding() {
        let writer = MultiVolumeWriter {
            current_volume: 1,
            bytes_written: 0,
            volume_size: 20480,
            options: MultiVolumeOptions::default(),
            record_size: 10240,
            writer: None,
            skip_data: false,
            archive_files: ArchiveFiles::default(),
        };
        assert_eq!(writer.volume_bytes(0), 10240);
        assert_eq!(writer.volume_bytes(9216), 10240);
        // No room left for the end-of-archive marker in the first record.
        assert_eq!(writer.volume_bytes(9728), 20480);
    }
}
