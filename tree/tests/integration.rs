//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod cp;
mod ls;
mod mv;
mod rm;

use std::sync::Mutex;

static UMASK_SETTER: Mutex<UmaskSetter> = Mutex::new(UmaskSetter);

// Used to serialize changes to the process' umask
struct UmaskSetter;

impl UmaskSetter {
    fn umask(&self, mask: libc::mode_t) -> libc::mode_t {
        let original = unsafe { libc::umask(mask) };

        // Pessimistically makes sure that the umask is applied before
        // continuing execution
        unsafe { while libc::umask(mask) != mask {} }

        original
    }
}
