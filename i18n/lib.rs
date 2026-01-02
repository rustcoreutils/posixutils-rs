//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! posixutils-i18n library
//!
//! This library provides functionality for internationalization utilities:
//! - gettext/ngettext: message catalog lookup
//! - msgfmt: .po to .mo compilation
//! - locale/localedef: locale handling

pub mod gettext_lib;
pub mod locale_lib;
