//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Constructs the makefiles of real packages depend on: automake output,
//! gettext's po Makefiles and the upstream Makefiles a Debian build runs
//! `$(MAKE)` over. Each test writes its makefile into a directory of its own
//! and runs make there. The expected output was checked against GNU make
//! and against the POSIX.2024 text.

mod common;

mod automake;

mod cancel_rules;
mod colon_command;
mod computed_names;
mod curdir;
mod deep_chain;
mod dotted_suffixes;
mod double_colon;
mod empty_targets;
mod escaped_hash;
mod expansion;
mod files_commands_make;
mod header_dollars;
mod include_remaking;
mod makeflags;
mod no_rule;
mod not_parallel;
mod order_only;
mod recipe_continuation;
mod rule_globs;
mod shell_status;
mod static_pattern;
mod substitution;
mod target_vars;
mod up_to_date;
mod upstream_rules;
