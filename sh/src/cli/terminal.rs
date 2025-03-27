//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use atty::Stream;
use nix::sys::termios;
use nix::sys::termios::{LocalFlags, Termios};
use std::io;
use std::io::Read;
use std::os::fd::AsFd;

#[derive(Clone)]
pub struct Terminal {
    base_settings: Option<Termios>,
}

impl Terminal {
    /// # Panic
    /// Panics if the current process is not attached to a terminal.
    pub fn set_nonblocking_no_echo(&self) {
        let mut termios = self.base_settings.clone().unwrap();
        termios.local_flags &= !(LocalFlags::ECHO | LocalFlags::ICANON);
        termios.control_chars[termios::SpecialCharacterIndices::VMIN as usize] = 0;
        termios.control_chars[termios::SpecialCharacterIndices::VTIME as usize] = 0;

        termios::tcsetattr(io::stdin().as_fd(), termios::SetArg::TCSANOW, &termios).unwrap();
    }

    /// # Panic
    /// Panics if the current process is not attached to a terminal.
    pub fn set_nonblocking(&self) {
        let mut termios = self.base_settings.clone().unwrap();
        termios.local_flags &= !LocalFlags::ICANON;
        termios.control_chars[termios::SpecialCharacterIndices::VMIN as usize] = 0;
        termios.control_chars[termios::SpecialCharacterIndices::VTIME as usize] = 0;

        termios::tcsetattr(io::stdin().as_fd(), termios::SetArg::TCSANOW, &termios).unwrap();
    }

    /// Doesn't do anything if the current process is not attached to a terminal.
    pub fn reset(&self) -> Termios {
        let current = termios::tcgetattr(io::stdin().as_fd()).unwrap();
        if let Some(base_settings) = &self.base_settings {
            termios::tcsetattr(io::stdin().as_fd(), termios::SetArg::TCSANOW, base_settings)
                .unwrap();
        }
        current
    }

    pub fn set(&self, settings: Termios) {
        termios::tcsetattr(io::stdin().as_fd(), termios::SetArg::TCSANOW, &settings).unwrap();
    }
}

impl Default for Terminal {
    fn default() -> Self {
        if is_attached_to_terminal() {
            let base_settings = termios::tcgetattr(io::stdin().as_fd()).unwrap();
            Terminal {
                base_settings: Some(base_settings),
            }
        } else {
            Terminal {
                base_settings: None,
            }
        }
    }
}

pub fn read_nonblocking_char() -> Option<u8> {
    let mut buf = [0u8; 1];
    match io::stdin().read(&mut buf) {
        Ok(1) => Some(buf[0]),
        _ => None,
    }
}

pub fn is_attached_to_terminal() -> bool {
    atty::is(Stream::Stdin) && atty::is(Stream::Stdout)
}
