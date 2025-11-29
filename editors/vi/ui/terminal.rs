//! Terminal abstraction for raw mode and control.

use crate::error::{Result, ViError};
use std::io::{self, Write};
use std::os::unix::io::AsRawFd;
use termios::{tcsetattr, Termios, ECHO, ICANON, ICRNL, ISIG, IXON, OPOST, TCSAFLUSH, VMIN, VTIME};

/// Terminal size.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TerminalSize {
    /// Number of rows.
    pub rows: u16,
    /// Number of columns.
    pub cols: u16,
}

impl Default for TerminalSize {
    fn default() -> Self {
        Self { rows: 24, cols: 80 }
    }
}

/// Terminal abstraction for raw mode and control sequences.
pub struct Terminal {
    /// Original terminal settings.
    original_termios: Option<Termios>,
    /// Current terminal size.
    size: TerminalSize,
    /// Whether raw mode is enabled.
    raw_mode: bool,
}

impl Terminal {
    /// Create a new terminal instance.
    pub fn new() -> Result<Self> {
        let size = Self::get_size_internal()?;
        Ok(Self {
            original_termios: None,
            size,
            raw_mode: false,
        })
    }

    /// Create a dummy terminal for headless/testing mode.
    /// This terminal does not interact with any real terminal.
    pub fn new_dummy() -> Self {
        Self {
            original_termios: None,
            size: TerminalSize::default(),
            raw_mode: false,
        }
    }

    /// Enable raw mode.
    pub fn enable_raw_mode(&mut self) -> Result<()> {
        if self.raw_mode {
            return Ok(());
        }

        let stdin_fd = io::stdin().as_raw_fd();
        let original = Termios::from_fd(stdin_fd).map_err(ViError::Io)?;

        self.original_termios = Some(original);

        let mut raw = original;

        // Input flags: disable ICRNL (translate CR to NL), IXON (XON/XOFF flow control)
        raw.c_iflag &= !(ICRNL | IXON);

        // Output flags: disable OPOST (post-processing)
        raw.c_oflag &= !(OPOST);

        // Local flags: disable ECHO, ICANON (canonical mode), ISIG (signals)
        raw.c_lflag &= !(ECHO | ICANON | ISIG);

        // Control characters: read returns after 1 byte, no timeout
        raw.c_cc[VMIN] = 1;
        raw.c_cc[VTIME] = 0;

        tcsetattr(stdin_fd, TCSAFLUSH, &raw).map_err(ViError::Io)?;

        self.raw_mode = true;
        Ok(())
    }

    /// Disable raw mode and restore original settings.
    pub fn disable_raw_mode(&mut self) -> Result<()> {
        if !self.raw_mode {
            return Ok(());
        }

        if let Some(original) = self.original_termios {
            let stdin_fd = io::stdin().as_raw_fd();
            tcsetattr(stdin_fd, TCSAFLUSH, &original).map_err(ViError::Io)?;
        }

        self.raw_mode = false;
        Ok(())
    }

    /// Get terminal size.
    pub fn size(&self) -> TerminalSize {
        self.size
    }

    /// Refresh terminal size.
    pub fn refresh_size(&mut self) -> Result<()> {
        self.size = Self::get_size_internal()?;
        Ok(())
    }

    /// Minimum terminal dimensions (safety bounds).
    const MIN_ROWS: u16 = 2;
    const MIN_COLS: u16 = 10;
    /// Maximum terminal dimensions (safety bounds against malformed values).
    const MAX_ROWS: u16 = 1000;
    const MAX_COLS: u16 = 1000;

    /// Get terminal size using ioctl.
    fn get_size_internal() -> Result<TerminalSize> {
        unsafe {
            let mut ws: libc::winsize = std::mem::zeroed();
            if libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &mut ws) == -1 {
                // Fallback to default
                return Ok(TerminalSize::default());
            }

            // Validate and clamp values to safe bounds to protect against
            // malformed terminal responses that could cause division by zero
            // or excessive memory allocation
            let rows = ws.ws_row.clamp(Self::MIN_ROWS, Self::MAX_ROWS);
            let cols = ws.ws_col.clamp(Self::MIN_COLS, Self::MAX_COLS);

            // If values were zero or invalid, use defaults
            if ws.ws_row == 0 || ws.ws_col == 0 {
                return Ok(TerminalSize::default());
            }

            Ok(TerminalSize { rows, cols })
        }
    }

    /// Clear the entire screen.
    pub fn clear_screen(&self) -> Result<()> {
        let mut stdout = io::stdout();
        stdout.write_all(b"\x1b[2J")?;
        stdout.flush()?;
        Ok(())
    }

    /// Clear from cursor to end of screen.
    pub fn clear_to_end(&self) -> Result<()> {
        let mut stdout = io::stdout();
        stdout.write_all(b"\x1b[J")?;
        stdout.flush()?;
        Ok(())
    }

    /// Clear from cursor to end of line.
    pub fn clear_line_to_end(&self) -> Result<()> {
        let mut stdout = io::stdout();
        stdout.write_all(b"\x1b[K")?;
        stdout.flush()?;
        Ok(())
    }

    /// Clear entire line.
    pub fn clear_line(&self) -> Result<()> {
        let mut stdout = io::stdout();
        stdout.write_all(b"\x1b[2K")?;
        stdout.flush()?;
        Ok(())
    }

    /// Move cursor to position (1-indexed row and column).
    pub fn move_cursor(&self, row: u16, col: u16) -> Result<()> {
        let mut stdout = io::stdout();
        write!(stdout, "\x1b[{};{}H", row, col)?;
        stdout.flush()?;
        Ok(())
    }

    /// Move cursor to home (top-left).
    pub fn move_cursor_home(&self) -> Result<()> {
        let mut stdout = io::stdout();
        stdout.write_all(b"\x1b[H")?;
        stdout.flush()?;
        Ok(())
    }

    /// Hide cursor.
    pub fn hide_cursor(&self) -> Result<()> {
        let mut stdout = io::stdout();
        stdout.write_all(b"\x1b[?25l")?;
        stdout.flush()?;
        Ok(())
    }

    /// Show cursor.
    pub fn show_cursor(&self) -> Result<()> {
        let mut stdout = io::stdout();
        stdout.write_all(b"\x1b[?25h")?;
        stdout.flush()?;
        Ok(())
    }

    /// Ring the terminal bell.
    pub fn bell(&self) -> Result<()> {
        let mut stdout = io::stdout();
        stdout.write_all(b"\x07")?;
        stdout.flush()?;
        Ok(())
    }

    /// Enter alternate screen buffer.
    pub fn enter_alternate_screen(&self) -> Result<()> {
        let mut stdout = io::stdout();
        stdout.write_all(b"\x1b[?1049h")?;
        stdout.flush()?;
        Ok(())
    }

    /// Leave alternate screen buffer.
    pub fn leave_alternate_screen(&self) -> Result<()> {
        let mut stdout = io::stdout();
        stdout.write_all(b"\x1b[?1049l")?;
        stdout.flush()?;
        Ok(())
    }

    /// Write a string at the current cursor position.
    pub fn write_str(&self, s: &str) -> Result<()> {
        let mut stdout = io::stdout();
        stdout.write_all(s.as_bytes())?;
        stdout.flush()?;
        Ok(())
    }

    /// Write bytes at the current cursor position.
    pub fn write_bytes(&self, bytes: &[u8]) -> Result<()> {
        let mut stdout = io::stdout();
        stdout.write_all(bytes)?;
        stdout.flush()?;
        Ok(())
    }

    /// Flush output.
    pub fn flush(&self) -> Result<()> {
        io::stdout().flush()?;
        Ok(())
    }
}

impl Default for Terminal {
    fn default() -> Self {
        Self::new().unwrap_or_else(|_| Self {
            original_termios: None,
            size: TerminalSize::default(),
            raw_mode: false,
        })
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        let _ = self.disable_raw_mode();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_terminal_size_default() {
        let size = TerminalSize::default();
        assert_eq!(size.rows, 24);
        assert_eq!(size.cols, 80);
    }
}
