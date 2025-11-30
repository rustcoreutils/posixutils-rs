//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::os::errno::get_current_errno_value;
use std::io;
use std::io::{IsTerminal, Read};

fn get_current_settings() -> libc::termios {
    // using zeroed here because terminos has additional members on some systems
    let mut settings = unsafe { std::mem::zeroed::<libc::termios>() };
    let result = unsafe { libc::tcgetattr(libc::STDIN_FILENO, &mut settings) };
    if result < 0 {
        panic!(
            "failed to read terminal settings ({})",
            get_current_errno_value()
        );
    }
    settings
}

fn set_terminal_settings(settings: &libc::termios) {
    let result = unsafe { libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, settings) };
    if result < 0 {
        panic!(
            "failed to set terminal settings {}",
            get_current_errno_value()
        );
    }
}

#[derive(Clone)]
pub struct Terminal {
    base_settings: Option<libc::termios>,
}

impl Terminal {
    /// # Panic
    /// Panics if the current process is not attached to a terminal.
    pub fn set_nonblocking_no_echo(&self) {
        let mut termios = self.base_settings.unwrap();
        termios.c_lflag &= !(libc::ECHO | libc::ICANON);
        termios.c_cc[libc::VMIN] = 0;
        termios.c_cc[libc::VTIME] = 0;
        set_terminal_settings(&termios);
    }

    /// # Panic
    /// Panics if the current process is not attached to a terminal.
    pub fn set_nonblocking(&self) {
        let mut termios = self.base_settings.unwrap();
        termios.c_lflag &= !libc::ICANON;
        termios.c_cc[libc::VMIN] = 0;
        termios.c_cc[libc::VTIME] = 0;
        set_terminal_settings(&termios);
    }

    /// Doesn't do anything if the current process is not attached to a terminal.
    pub fn reset(&self) -> libc::termios {
        let current = get_current_settings();
        if let Some(base_settings) = &self.base_settings {
            set_terminal_settings(base_settings);
        }
        current
    }

    pub fn set(&self, settings: libc::termios) {
        set_terminal_settings(&settings)
    }
}

impl Default for Terminal {
    fn default() -> Self {
        if is_attached_to_terminal() {
            Terminal {
                base_settings: Some(get_current_settings()),
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
    std::io::stdin().is_terminal() && std::io::stdout().is_terminal()
}
