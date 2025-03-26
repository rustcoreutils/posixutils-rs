use nix::sys::termios;
use nix::sys::termios::{LocalFlags, Termios};
use std::io;
use std::io::Read;
use std::os::fd::AsFd;

#[derive(Clone)]
pub struct Terminal {
    base_settings: Termios,
}

impl Terminal {
    pub fn set_nonblocking_no_echo(&self) {
        let mut termios = self.base_settings.clone();
        termios.local_flags &= !(LocalFlags::ECHO | LocalFlags::ICANON);
        termios.control_chars[termios::SpecialCharacterIndices::VMIN as usize] = 0;
        termios.control_chars[termios::SpecialCharacterIndices::VTIME as usize] = 0;

        termios::tcsetattr(io::stdin().as_fd(), termios::SetArg::TCSANOW, &termios).unwrap();
    }

    pub fn reset(&self) -> Termios {
        let current = termios::tcgetattr(io::stdin().as_fd()).unwrap();
        termios::tcsetattr(
            io::stdin().as_fd(),
            termios::SetArg::TCSANOW,
            &self.base_settings,
        )
        .unwrap();
        current
    }

    pub fn set(&self, settings: Termios) {
        termios::tcsetattr(io::stdin().as_fd(), termios::SetArg::TCSANOW, &settings).unwrap();
    }
}

impl Default for Terminal {
    fn default() -> Self {
        let base_settings = termios::tcgetattr(io::stdin().as_fd()).unwrap();
        Terminal { base_settings }
    }
}

pub fn read_nonblocking_char() -> Option<u8> {
    let mut buf = [0u8; 1];
    match io::stdin().read(&mut buf) {
        Ok(1) => Some(buf[0]),
        _ => None,
    }
}
