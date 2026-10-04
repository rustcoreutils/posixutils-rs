//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

// The PTY sessions `more`'s interactive tests run in. Unix-only: on the
// Windows runner, a program driven through the pseudo-console never has
// anything it writes after the first input come back -- cmd.exe included --
// so the sessions there test the harness, not `more`.
#[cfg(unix)]
mod common;
mod echo;
mod more;
mod printf;
