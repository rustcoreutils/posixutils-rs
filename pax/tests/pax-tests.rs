//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod common;

mod acl;
mod append;
mod archive;
mod compression;
mod copy;
mod cpio;
mod list;
mod malformed;
mod multivolume;
mod options;
mod privileges;
mod security;
mod special;
mod subst;
mod tar;
mod update;
#[cfg(any(target_os = "linux", target_os = "macos"))]
mod xattr;
