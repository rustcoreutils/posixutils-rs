//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

// stty/tabs PTY-backed tests use the shared `common` harness; tput and the
// argument-parsing paths of tabs use plain subprocess tests.

mod common;
mod stty;
mod tabs;
mod tput;
