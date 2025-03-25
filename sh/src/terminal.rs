use nix::sys::termios;
use nix::sys::termios::{InputFlags, LocalFlags};
use std::io;
use std::io::{Read, Write};
use std::os::fd::AsFd;

pub fn clear_line() {
    print!("\r\x1b[K");
}

pub fn set_cursor_pos(pos: usize) {
    print!("\r\x1b[{}G", pos + 1);
}

pub fn set_terminal_raw_mode() {
    let mut termios = termios::tcgetattr(io::stdin().as_fd()).unwrap();
    termios.local_flags &= !(LocalFlags::ECHO | LocalFlags::ICANON);
    termios.input_flags &= !(InputFlags::ICRNL | InputFlags::IXON);
    termios.control_chars[termios::SpecialCharacterIndices::VMIN as usize] = 0;
    termios.control_chars[termios::SpecialCharacterIndices::VTIME as usize] = 0;

    termios::tcsetattr(io::stdin().as_fd(), termios::SetArg::TCSANOW, &termios).unwrap();
}

pub fn read_nonblocking_char() -> Option<u8> {
    let mut buf = [0u8; 1];
    match io::stdin().read(&mut buf) {
        Ok(1) => Some(buf[0]),
        _ => None,
    }
}
