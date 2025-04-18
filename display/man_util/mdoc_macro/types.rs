//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use pest::iterators::Pair;

use crate::man_util::parser::Rule;

/// Bd block variants
#[derive(Debug, Clone, PartialEq)]
pub enum BdType {
    Centered,
    Filled,
    Literal,
    Ragged,
    Unfilled,
}

impl From<Pair<'_, Rule>> for BdType {
    fn from(pair: Pair<'_, Rule>) -> Self {
        match pair.into_inner().next().unwrap().as_rule() {
            Rule::bd_centered => Self::Centered,
            Rule::bd_filled => Self::Filled,
            Rule::bd_literal => Self::Literal,
            Rule::bd_ragged => Self::Ragged,
            Rule::bd_unfilled => Self::Unfilled,
            _ => unreachable!(),
        }
    }
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

impl From<Pair<'_, Rule>> for OffsetType {
    fn from(pair: Pair<'_, Rule>) -> Self {
        match pair.into_inner().next().unwrap().as_rule() {
            Rule::off_indent => Self::Indent,
            Rule::off_indent_two => Self::IndentTwo,
            Rule::off_left => Self::Left,
            Rule::off_right => Self::Right,
            Rule::off_center => Self::Center,
            Rule::word => Self::Indent,
            _ => unreachable!(),
        }
    }
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

impl From<Pair<'_, Rule>> for BfType {
    fn from(pair: Pair<'_, Rule>) -> Self {
        match pair.into_inner().next().unwrap().as_rule() {
            Rule::bf_emphasis | Rule::bf_em => Self::Emphasis,
            Rule::bf_literal | Rule::bf_li => Self::Literal,
            Rule::bf_symbolic | Rule::bf_sy => Self::Symbolic,
            _ => unreachable!(),
        }
    }
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

impl From<Pair<'_, Rule>> for BlType {
    fn from(pair: Pair<'_, Rule>) -> Self {
        match pair.into_inner().next().unwrap().as_rule() {
            Rule::bl_bullet => Self::Bullet,
            Rule::bl_column => Self::Column,
            Rule::bl_dash | Rule::bl_hyphen => Self::Dash,
            Rule::bl_diag => Self::Diag,
            Rule::bl_enum => Self::Enum,
            Rule::bl_hang => Self::Hang,
            Rule::bl_inset => Self::Inset,
            Rule::bl_item => Self::Item,
            Rule::bl_ohang => Self::Ohang,
            Rule::bl_tag => Self::Tag,
            _ => unreachable!(),
        }
    }
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
