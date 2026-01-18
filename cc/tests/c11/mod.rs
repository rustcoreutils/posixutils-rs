//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C11 tests for pcc
//
// This module contains mega-tests for C11-era features:
// - core: _Static_assert, _Alignof, _Noreturn, _Thread_local
// - atomics: ALL atomic operations
//

mod atomics;
mod core;
