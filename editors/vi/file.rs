//! File I/O operations for vi.
//!
//! POSIX vi supports:
//! - Reading files into buffer
//! - Writing buffer to files
//! - Reading/writing ranges
//! - Alternate file tracking

use crate::buffer::{Buffer, Line};
use crate::error::{Result, ViError};
use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};

/// File manager for the editor.
#[derive(Debug)]
pub struct FileManager {
    /// Current file name.
    current_file: Option<PathBuf>,
    /// Alternate file name (for Ctrl-^).
    alternate_file: Option<PathBuf>,
    /// Argument list of files.
    arg_list: Vec<PathBuf>,
    /// Current index in argument list.
    arg_index: usize,
    /// Whether current file is read-only.
    readonly: bool,
}

impl Default for FileManager {
    fn default() -> Self {
        Self::new()
    }
}

impl FileManager {
    /// Create a new file manager.
    pub fn new() -> Self {
        Self {
            current_file: None,
            alternate_file: None,
            arg_list: Vec::new(),
            arg_index: 0,
            readonly: false,
        }
    }

    /// Set the argument list of files.
    pub fn set_args(&mut self, args: Vec<PathBuf>) {
        self.arg_list = args;
        self.arg_index = 0;
    }

    /// Get the argument list.
    pub fn args(&self) -> &[PathBuf] {
        &self.arg_list
    }

    /// Get the current argument index.
    pub fn arg_index(&self) -> usize {
        self.arg_index
    }

    /// Get the current file name.
    pub fn current_file(&self) -> Option<&Path> {
        self.current_file.as_deref()
    }

    /// Get the alternate file name.
    pub fn alternate_file(&self) -> Option<&Path> {
        self.alternate_file.as_deref()
    }

    /// Set the current file name.
    pub fn set_current_file(&mut self, path: Option<PathBuf>) {
        // Move current to alternate
        if let Some(current) = self.current_file.take() {
            self.alternate_file = Some(current);
        }
        self.current_file = path;
    }

    /// Set read-only mode.
    pub fn set_readonly(&mut self, readonly: bool) {
        self.readonly = readonly;
    }

    /// Check if read-only mode.
    pub fn is_readonly(&self) -> bool {
        self.readonly
    }

    /// Switch to alternate file.
    pub fn switch_to_alternate(&mut self) -> Result<PathBuf> {
        let alt = self.alternate_file.take().ok_or(ViError::NoAlternateFile)?;

        // Swap current and alternate
        let current = self.current_file.take();
        self.current_file = Some(alt.clone());
        self.alternate_file = current;

        Ok(alt)
    }

    /// Go to next file in argument list.
    pub fn next_file(&mut self) -> Result<PathBuf> {
        if self.arg_list.is_empty() {
            return Err(ViError::NoFileName);
        }
        if self.arg_index + 1 >= self.arg_list.len() {
            return Err(ViError::InvalidCommand("No more files".to_string()));
        }
        self.arg_index += 1;
        let path = self.arg_list[self.arg_index].clone();
        self.set_current_file(Some(path.clone()));
        Ok(path)
    }

    /// Go to previous file in argument list.
    pub fn prev_file(&mut self) -> Result<PathBuf> {
        if self.arg_list.is_empty() {
            return Err(ViError::NoFileName);
        }
        if self.arg_index == 0 {
            return Err(ViError::InvalidCommand("Already at first file".to_string()));
        }
        self.arg_index -= 1;
        let path = self.arg_list[self.arg_index].clone();
        self.set_current_file(Some(path.clone()));
        Ok(path)
    }

    /// Rewind to first file in argument list.
    pub fn rewind(&mut self) -> Result<PathBuf> {
        if self.arg_list.is_empty() {
            return Err(ViError::NoFileName);
        }
        self.arg_index = 0;
        let path = self.arg_list[0].clone();
        self.set_current_file(Some(path.clone()));
        Ok(path)
    }

    /// Get count of remaining files.
    pub fn files_remaining(&self) -> usize {
        if self.arg_list.is_empty() {
            0
        } else {
            self.arg_list.len() - self.arg_index - 1
        }
    }

    /// Format args list for display.
    pub fn format_args(&self) -> String {
        let mut result = String::new();
        for (i, path) in self.arg_list.iter().enumerate() {
            if i == self.arg_index {
                result.push('[');
            }
            result.push_str(&path.display().to_string());
            if i == self.arg_index {
                result.push(']');
            }
            if i + 1 < self.arg_list.len() {
                result.push(' ');
            }
        }
        result
    }
}

/// Read a file into a buffer.
pub fn read_file(path: &Path) -> Result<Buffer> {
    if !path.exists() {
        // New file - return empty buffer
        let buffer = Buffer::new();
        return Ok(buffer);
    }

    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let mut content = String::new();

    for line_result in reader.lines() {
        let line = line_result?;
        if !content.is_empty() {
            content.push('\n');
        }
        content.push_str(&line);
    }

    // If file was empty, return buffer with one empty line
    if content.is_empty() {
        return Ok(Buffer::new());
    }

    Ok(Buffer::from_text(&content))
}

/// Read a file into buffer after a specific line.
pub fn read_file_after(buffer: &mut Buffer, line_num: usize, path: &Path) -> Result<usize> {
    if !path.exists() {
        return Err(ViError::FileNotFound(path.display().to_string()));
    }

    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let mut count = 0;

    for (i, line_result) in reader.lines().enumerate() {
        let line = line_result?;
        buffer.insert_line_after(line_num + i, Line::from(line.as_str()));
        count += 1;
    }

    Ok(count)
}

/// Write buffer to a file.
pub fn write_file(buffer: &Buffer, path: &Path, append: bool) -> Result<WriteStats> {
    let file = if append {
        OpenOptions::new().create(true).append(true).open(path)?
    } else {
        File::create(path)?
    };

    let mut writer = BufWriter::new(file);
    let mut bytes = 0;
    let lines = buffer.line_count();

    for line_num in 1..=lines {
        if let Some(line) = buffer.line(line_num) {
            let content = line.content();
            writer.write_all(content.as_bytes())?;
            writer.write_all(b"\n")?;
            bytes += content.len() + 1;
        }
    }

    writer.flush()?;

    Ok(WriteStats { lines, bytes })
}

/// Write a range of buffer lines to a file.
pub fn write_range(
    buffer: &Buffer,
    path: &Path,
    start: usize,
    end: usize,
    append: bool,
) -> Result<WriteStats> {
    let file = if append {
        OpenOptions::new().create(true).append(true).open(path)?
    } else {
        File::create(path)?
    };

    let mut writer = BufWriter::new(file);
    let mut bytes = 0;
    let mut lines = 0;

    for line_num in start..=end {
        if let Some(line) = buffer.line(line_num) {
            let content = line.content();
            writer.write_all(content.as_bytes())?;
            writer.write_all(b"\n")?;
            bytes += content.len() + 1;
            lines += 1;
        }
    }

    writer.flush()?;

    Ok(WriteStats { lines, bytes })
}

/// Statistics from a write operation.
#[derive(Debug, Clone, Copy)]
pub struct WriteStats {
    /// Number of lines written.
    pub lines: usize,
    /// Number of bytes written.
    pub bytes: usize,
}

impl WriteStats {
    /// Format as a message.
    pub fn message(&self, path: &Path) -> String {
        format!(
            "\"{}\" {} lines, {} bytes",
            path.display(),
            self.lines,
            self.bytes
        )
    }
}

/// Read output of a shell command into buffer.
pub fn read_shell_output(buffer: &mut Buffer, line_num: usize, command: &str) -> Result<usize> {
    use std::process::Command;

    let output = Command::new("sh").arg("-c").arg(command).output()?;

    if !output.status.success() {
        return Err(ViError::InvalidCommand(
            String::from_utf8_lossy(&output.stderr).to_string(),
        ));
    }

    let text = String::from_utf8_lossy(&output.stdout);
    let mut count = 0;

    for (i, line) in text.lines().enumerate() {
        buffer.insert_line_after(line_num + i, Line::from(line));
        count += 1;
    }

    Ok(count)
}

/// Write buffer range to a shell command.
pub fn write_shell_input(
    buffer: &Buffer,
    start: usize,
    end: usize,
    command: &str,
) -> Result<String> {
    use std::io::Write as IoWrite;
    use std::process::{Command, Stdio};

    let mut child = Command::new("sh")
        .arg("-c")
        .arg(command)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    // Write buffer content to stdin
    if let Some(stdin) = child.stdin.as_mut() {
        for line_num in start..=end {
            if let Some(line) = buffer.line(line_num) {
                writeln!(stdin, "{}", line.content())?;
            }
        }
    }

    let output = child.wait_with_output()?;

    if !output.status.success() {
        return Err(ViError::InvalidCommand(
            String::from_utf8_lossy(&output.stderr).to_string(),
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

/// Get file information.
pub fn file_info(path: &Path) -> Result<FileInfo> {
    if !path.exists() {
        return Ok(FileInfo {
            exists: false,
            size: 0,
            readonly: false,
        });
    }

    let metadata = fs::metadata(path)?;
    let readonly = metadata.permissions().readonly();

    Ok(FileInfo {
        exists: true,
        size: metadata.len(),
        readonly,
    })
}

/// Information about a file.
#[derive(Debug, Clone, Copy)]
pub struct FileInfo {
    /// Whether the file exists.
    pub exists: bool,
    /// File size in bytes.
    pub size: u64,
    /// Whether file is read-only.
    pub readonly: bool,
}

/// Create a backup of a file.
pub fn create_backup(path: &Path) -> Result<()> {
    if path.exists() {
        let mut backup_path = path.to_path_buf();
        let name = backup_path
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();
        backup_path.set_file_name(format!("{}.bak", name));
        fs::copy(path, backup_path)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write as IoWrite;
    use tempfile::NamedTempFile;

    #[test]
    fn test_read_file() {
        let mut tmp = NamedTempFile::new().unwrap();
        writeln!(tmp, "line one").unwrap();
        writeln!(tmp, "line two").unwrap();
        writeln!(tmp, "line three").unwrap();

        let buffer = read_file(tmp.path()).unwrap();
        assert_eq!(buffer.line_count(), 3);
        assert_eq!(buffer.line(1).unwrap().content(), "line one");
        assert_eq!(buffer.line(2).unwrap().content(), "line two");
        assert_eq!(buffer.line(3).unwrap().content(), "line three");
    }

    #[test]
    fn test_read_nonexistent_file() {
        // Reading a nonexistent file returns an empty buffer (new file case)
        let buffer = read_file(Path::new("/nonexistent/file/path")).unwrap();
        // Empty buffer has no lines yet (will get populated when editing)
        assert_eq!(buffer.line_count(), 0);
    }

    #[test]
    fn test_write_file() {
        let buffer = Buffer::from_text("hello\nworld");
        let tmp = NamedTempFile::new().unwrap();

        let stats = write_file(&buffer, tmp.path(), false).unwrap();
        assert_eq!(stats.lines, 2);

        // Read back and verify
        let read_buffer = read_file(tmp.path()).unwrap();
        assert_eq!(read_buffer.line_count(), 2);
        assert_eq!(read_buffer.line(1).unwrap().content(), "hello");
        assert_eq!(read_buffer.line(2).unwrap().content(), "world");
    }

    #[test]
    fn test_write_range() {
        let buffer = Buffer::from_text("one\ntwo\nthree\nfour");
        let tmp = NamedTempFile::new().unwrap();

        let stats = write_range(&buffer, tmp.path(), 2, 3, false).unwrap();
        assert_eq!(stats.lines, 2);

        let read_buffer = read_file(tmp.path()).unwrap();
        assert_eq!(read_buffer.line_count(), 2);
        assert_eq!(read_buffer.line(1).unwrap().content(), "two");
        assert_eq!(read_buffer.line(2).unwrap().content(), "three");
    }

    #[test]
    fn test_write_append() {
        let buffer = Buffer::from_text("appended");
        let mut tmp = NamedTempFile::new().unwrap();
        writeln!(tmp, "existing").unwrap();

        write_file(&buffer, tmp.path(), true).unwrap();

        let read_buffer = read_file(tmp.path()).unwrap();
        assert_eq!(read_buffer.line_count(), 2);
        assert_eq!(read_buffer.line(1).unwrap().content(), "existing");
        assert_eq!(read_buffer.line(2).unwrap().content(), "appended");
    }

    #[test]
    fn test_file_manager_args() {
        let mut fm = FileManager::new();
        fm.set_args(vec![
            PathBuf::from("file1.txt"),
            PathBuf::from("file2.txt"),
            PathBuf::from("file3.txt"),
        ]);

        assert_eq!(fm.arg_index(), 0);
        assert_eq!(fm.files_remaining(), 2);

        let next = fm.next_file().unwrap();
        assert_eq!(next, PathBuf::from("file2.txt"));
        assert_eq!(fm.arg_index(), 1);

        let prev = fm.prev_file().unwrap();
        assert_eq!(prev, PathBuf::from("file1.txt"));
        assert_eq!(fm.arg_index(), 0);
    }

    #[test]
    fn test_file_manager_alternate() {
        let mut fm = FileManager::new();
        fm.set_current_file(Some(PathBuf::from("file1.txt")));
        fm.set_current_file(Some(PathBuf::from("file2.txt")));

        assert_eq!(fm.current_file(), Some(Path::new("file2.txt")));
        assert_eq!(fm.alternate_file(), Some(Path::new("file1.txt")));

        let alt = fm.switch_to_alternate().unwrap();
        assert_eq!(alt, PathBuf::from("file1.txt"));
        assert_eq!(fm.current_file(), Some(Path::new("file1.txt")));
        assert_eq!(fm.alternate_file(), Some(Path::new("file2.txt")));
    }

    #[test]
    fn test_file_info_exists() {
        let tmp = NamedTempFile::new().unwrap();
        let info = file_info(tmp.path()).unwrap();
        assert!(info.exists);
    }

    #[test]
    fn test_file_info_not_exists() {
        let info = file_info(Path::new("/nonexistent")).unwrap();
        assert!(!info.exists);
    }

    #[test]
    fn test_format_args() {
        let mut fm = FileManager::new();
        fm.set_args(vec![
            PathBuf::from("a.txt"),
            PathBuf::from("b.txt"),
            PathBuf::from("c.txt"),
        ]);

        assert_eq!(fm.format_args(), "[a.txt] b.txt c.txt");

        fm.next_file().unwrap();
        assert_eq!(fm.format_args(), "a.txt [b.txt] c.txt");
    }
}
