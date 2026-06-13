//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

/// Bd block variants
#[derive(Debug, Clone, PartialEq)]
pub enum BdType {
    Centered,
    Filled,
    Literal,
    Ragged,
    Unfilled,
}

/// Bd, Bl blocks offset variants
#[derive(Debug, Clone, PartialEq)]
pub enum OffsetType {
    Indent,
    /// 2x [`OffsetType::Indent`]
    IndentTwo,
    Left,
    Right,
    Center,
}

/// Bf block font style variants
#[derive(Debug, Clone, PartialEq)]
pub enum BfType {
    /// Enables italic font mode
    Emphasis,
    /// Enables typewriter font mode
    Literal,
    /// Enables boldface font mode
    Symbolic,
}

/// Bl block variants
#[derive(Debug, Clone, PartialEq)]
pub enum BlType {
    /// No item heads can be specified, but a bullet will be printed at the head of each item
    Bullet,
    /// A columnated list
    Column,
    /// Like -bullet, except that dashes are used in place of bullets
    Dash,
    /// Like -inset, except that item heads are not parsed for macro invocations
    Diag,
    /// A numbered list
    Enum,
    /// Like -tag, except that the first lines of item bodies are not indented, but follow the item heads like in -inset lists
    Hang,
    /// Item bodies follow items heads on the same line, using normal inter-word spacing
    Inset,
    /// No item heads can be specified, and none are printed
    Item,
    /// Item bodies start on the line following item heads and are not indented
    Ohang,
    /// Item bodies are indented according to the -width argument
    Tag,
}

/// Defines how split authors names
#[derive(Debug, Clone, PartialEq)]
pub enum AnType {
    Split,
    NoSplit,
    Name,
}

/// Spacing mode for output generated from macros
#[derive(Debug, Clone, PartialEq)]
pub enum SmMode {
    /// Space is inserted between macro arguments and between the output generated from adjacent macros
    On,
    /// No white space is inserted between macro arguments and between the output generated from adjacent macros
    Off,
}
