//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use text_production::{AtType, BsxType};
use thiserror::Error;
use types::{BdType, BfType, OffsetType, SmMode};

use crate::man_util::mdoc_macro::text_production::{
    BxType, DxType, FxType, NxType, OxType, StType,
};

use super::mdoc_macro::types::*;
use super::mdoc_macro::*;

use std::mem::discriminant;
use std::sync::LazyLock;

/// Rs submacros sorting order
static RS_SUBMACRO_ORDER: LazyLock<Vec<Macro>> = LazyLock::new(|| {
    vec![
        Macro::A,
        Macro::T,
        Macro::B,
        Macro::I,
        Macro::J,
        Macro::R,
        Macro::N,
        Macro::V,
        Macro::U,
        Macro::P,
        Macro::Q,
        Macro::C,
        Macro::D,
        Macro::O,
    ]
});

static BLOCK_PARTIAL_IMPLICIT: &[&str] = &[
    "Aq", "Bq", "Brq", "D1", "Dl", "Dq", "En", "Op", "Pq", "Ql", "Qq", "Sq", "Vt",
];

#[allow(unreachable_patterns)]
fn does_start_with_macro(word: &str) -> bool {
    matches!(
        word,
        "Bd" | "Bf"
            | "Bk"
            | "Bl"
            | "Ed"
            | "Ef"
            | "Ek"
            | "El"
            | "It"
            | "Nd"
            | "Nm"
            | "Sh"
            | "Ss"
            | "Ac"
            | "Ao"
            | "Bc"
            | "Bo"
            | "Brc"
            | "Bro"
            | "Dc"
            | "Do"
            | "Ec"
            | "Eo"
            | "Fc"
            | "Fo"
            | "Oc"
            | "Oo"
            | "Pc"
            | "Po"
            | "Qc"
            | "Qo"
            | "Re"
            | "Rs"
            | "Sc"
            | "So"
            | "Xc"
            | "Xo"
            | "Aq"
            | "Bq"
            | "Brq"
            | "D1"
            | "Dl"
            | "Dq"
            | "En"
            | "Op"
            | "Pq"
            | "Ql"
            | "Qq"
            | "Sq"
            | "Vt"
            | "Ta"
            | "%A"
            | "%B"
            | "%C"
            | "%D"
            | "%I"
            | "%J"
            | "%N"
            | "%O"
            | "%P"
            | "%Q"
            | "%R"
            | "%T"
            | "%U"
            | "%V"
            | "Ad"
            | "An"
            | "Ap"
            | "Ar"
            | "At"
            | "Bsx"
            | "Bt"
            | "Bx"
            | "Cd"
            | "Cm"
            | "Db"
            | "Dd"
            | "Dt"
            | "Dv"
            | "Dx"
            | "Em"
            | "Er"
            | "Es"
            | "Ev"
            | "Ex"
            | "Fa"
            | "Fd"
            | "Fl"
            | "Fn"
            | "Fr"
            | "Ft"
            | "Fx"
            | "Hf"
            | "Ic"
            | "In"
            | "Lb"
            | "Li"
            | "Lk"
            | "Lp"
            | "Ms"
            | "Mt"
            | "Nm"
            | "No"
            | "Ns"
            | "Nx"
            | "Os"
            | "Ot"
            | "Ox"
            | "Pa"
            | "Pf"
            | "Pp"
            | "Rv"
            | "Sm"
            | "St"
            | "Sx"
            | "Sy"
            | "Tg"
            | "Tn"
            | "Ud"
            | "Ux"
            | "Va"
            | "Vt"
            | "Xr"
    )
}

pub fn prepare_document(text: &str) -> String {
    let mut is_bd_literal_block = false;

    text.lines()
        .filter(|l| !l.trim_start().starts_with(".Tg"))
        .map(|l| {
            let line = if l.contains(".It") {
                l.replace('\t', " Ta ").replace("    ", " Ta ")
            } else {
                l.to_string()
            };

            if line.contains(".Bd") && (line.contains("-literal") || line.contains("-unfilled")) {
                is_bd_literal_block = true;
            }

            if is_bd_literal_block && line.contains(".Ed") {
                is_bd_literal_block = false;
            }

            let transformed_line = if is_bd_literal_block {
                let mut leading_spaces = if line.is_empty() { 1 } else { 0 };
                let mut index = 0;
                for (i, ch) in line.char_indices() {
                    if !ch.is_whitespace() {
                        break;
                    }
                    leading_spaces += if ch == '\t' { 4 } else { 1 };
                    index = i + ch.len_utf8();
                }

                format!("{}{}", "\\^".repeat(leading_spaces), &line[index..])
            } else {
                line.clone()
            };

            let mut processed_line = if let Some(first_word) = line.split_whitespace().next() {
                if does_start_with_macro(first_word) {
                    format!("\\&{}", transformed_line)
                } else {
                    transformed_line
                }
            } else {
                transformed_line
            };

            let count_partials = processed_line
                .split_whitespace()
                .filter(|word| BLOCK_PARTIAL_IMPLICIT.contains(word))
                .count();

            if count_partials > 0 {
                processed_line.push_str(&"\n".repeat(count_partials));
            }

            processed_line
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Mdoc files parser
#[derive(Parser)]
#[grammar = "./man_util/mdoc.pest"]
pub struct MdocParser;

/// Stores macro parameters and subnodes
#[derive(Debug, Clone, PartialEq)]
pub struct MacroNode {
    /// Macro type
    pub mdoc_macro: Macro,
    /// Sub nodes of current node
    pub nodes: Vec<Element>,
}

/// Mdoc language units
#[derive(Debug, Clone, PartialEq)]
pub enum Element {
    /// Text node
    Text(String),
    /// Macro node
    Macro(MacroNode),
    /// "End of input" marker
    Eoi,
}

impl From<Element> for String {
    fn from(element: Element) -> Self {
        match element {
            Element::Text(text) => text,
            Element::Macro(macro_node) => format!("{:?}", macro_node),
            Element::Eoi => "EOI".to_string(),
        }
    }
}

impl From<String> for Element {
    fn from(value: String) -> Self {
        Element::Text(value)
    }
}

/// Stores full mdoc AST
#[derive(Debug, Clone, PartialEq)]
pub struct MdocDocument {
    pub elements: Vec<Element>,
}

/// Mdoc parsing errors
#[derive(Error, Debug, PartialEq)]
pub enum MdocError {
    /// Pest rules violation
    #[error("mdoc: {0}")]
    Pest(#[from] Box<pest::error::Error<Rule>>),
}

impl MdocParser {
    fn parse_element(pair: Pair<Rule>) -> Element {
        match pair.as_rule() {
            Rule::element => Self::parse_element(pair.into_inner().next().unwrap()),
            Rule::block_full_explicit => Self::parse_block_full_explicit(pair),
            Rule::block_full_implicit => Self::parse_block_full_implicit(pair),
            Rule::block_partial_implicit => Self::parse_block_partial_implicit(pair),
            Rule::partial_implicit_element => {
                Self::parse_element(pair.into_inner().next().unwrap())
            }
            Rule::block_partial_explicit => Self::parse_block_partial_explicit(pair),
            Rule::inline => Self::parse_inline(pair),
            Rule::arg => Self::parse_arg(pair.into_inner().next().unwrap()),
            Rule::macro_arg => Self::parse_element(pair.into_inner().next().unwrap()),
            Rule::ta | Rule::ta_head => Self::parse_ta(pair),
            Rule::text_line | Rule::line => Element::Text(trim_quotes(
                pair.into_inner().next().unwrap().as_str().to_string(),
            )),
            Rule::EOI => Element::Eoi,
            _ => Element::Text(trim_quotes(pair.as_str().to_string())),
        }
    }

    fn parse_arg(pair: Pair<Rule>) -> Element {
        match pair.as_rule() {
            Rule::text_arg => Element::Text(pair.as_str().to_string()),
            Rule::macro_arg => Self::parse_element(pair.into_inner().next().unwrap()),
            _ => unreachable!(),
        }
    }

    fn parse_ta(_pair: Pair<Rule>) -> Element {
        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ta,
            nodes: vec![],
        })
    }

    /// Parses full mdoc file
    pub fn parse_mdoc(input: &str) -> Result<MdocDocument, MdocError> {
        let input = prepare_document(input);
        let pairs = MdocParser::parse(Rule::mdoc, input.as_ref())
            .map_err(|err| MdocError::Pest(Box::new(err)))?;

        // Iterate each pair (macro or text element)
        let mut elements: Vec<Element> = pairs
            .flat_map(|p| {
                let inner_rules = p.into_inner();
                inner_rules.map(Self::parse_element)
            })
            .collect();

        if let Some(Element::Eoi) = elements.last() {
            elements.pop(); // Remove `Element::Eoi` element
        }

        let mdoc = MdocDocument { elements };

        Ok(mdoc)
    }
}

// Block full-explicit macros parsing
impl MdocParser {
    /// Parses (`Bd`)[https://man.openbsd.org/mdoc#Bd]:
    /// `Bd -type [-offset width] [-compact]`
    fn parse_bd_block(pair: Pair<Rule>) -> Element {
        fn parse_bd_open(pair: Pair<Rule>) -> Macro {
            let mut inner = pair.into_inner();

            // -type
            let block_type = BdType::from(inner.next().unwrap());

            let mut offset: Option<OffsetType> = None;
            let mut compact = false;

            for arg_pair in inner {
                if !matches!(arg_pair.as_rule(), Rule::bd_offset | Rule::bd_compact) {
                    unreachable!()
                }
                for arg_pair in arg_pair.into_inner() {
                    match arg_pair.as_rule() {
                        Rule::offset => offset = Some(OffsetType::from(arg_pair)),
                        Rule::compact => compact = true,
                        _ => unreachable!(),
                    }
                }
            }

            Macro::Bd {
                block_type,
                offset,
                compact,
            }
        }

        let mut pairs = pair.into_inner();

        let bd_macro = parse_bd_open(pairs.next().unwrap());

        let nodes = pairs
            .take_while(|p| p.as_rule() != Rule::ed_close)
            .map(Self::parse_element)
            .collect();
        // .map(|p| parse_bd_body(bd_macro.clone(), p))

        Element::Macro(MacroNode {
            mdoc_macro: bd_macro,
            nodes,
        })
    }

    /// Parses (`Bf`)[https://man.openbsd.org/mdoc#Bf]:
    /// `Bf -emphasis | -literal | -symbolic | Em | Li | Sy`
    fn parse_bf_block(pair: Pair<Rule>) -> Element {
        fn parse_bf_open(pair: Pair<Rule>) -> Macro {
            let mut inner = pair.into_inner();

            // -type
            let block_type = BfType::from(inner.next().unwrap());

            Macro::Bf(block_type)
        }

        let mut pairs = pair.into_inner();

        let bf_macro = parse_bf_open(pairs.next().unwrap());

        let nodes = pairs
            .take_while(|p| p.as_rule() != Rule::ef_close)
            .map(Self::parse_element)
            .collect();

        Element::Macro(MacroNode {
            mdoc_macro: bf_macro,
            nodes,
        })
    }

    /// Parses (`Bk`)[https://man.openbsd.org/mdoc#Bk]:
    /// `Bk -words`
    fn parse_bk_block(pair: Pair<Rule>) -> Element {
        let mut pairs = pair.into_inner();

        // `bk_open`
        let _ = pairs.next().unwrap();

        let nodes = pairs
            .take_while(|p| p.as_rule() != Rule::ek_close)
            .map(Self::parse_element)
            .collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Bk,
            nodes,
        })
    }

    // Parses (`Bl`)[https://man.openbsd.org/mdoc#Bl]
    // `Bl -type [-width val] [-offset val] [-compact] [col ...]`
    fn parse_bl_block(pair: Pair<Rule>) -> Element {
        fn parse_bl_parameter(
            pair: Pair<Rule>,
            width: &mut Option<u8>,
            offset: &mut Option<OffsetType>,
            compact: &mut bool,
            columns: &mut Vec<String>,
            count: &mut (usize, usize, usize),
        ) -> bool {
            match pair.as_rule() {
                Rule::bl_width => {
                    if count.0 > 0 {
                        return true;
                    }
                    count.0 += 1;
                    let mut width_p = pair
                        .into_inner()
                        .find(|p| Rule::word == p.as_rule())
                        .map(|p| p.as_str().to_string())
                        .unwrap_or("".to_string());

                    if width_p.is_empty() {
                        *width = None;
                    } else if width_p.chars().next().unwrap().is_ascii_digit() {
                        width_p = width_p
                            .chars()
                            .take_while(|ch| ch.is_ascii_digit())
                            .collect::<String>();
                        if let Ok(w) = str::parse::<u8>(&width_p) {
                            *width = Some(w);
                        }
                    } else {
                        *width = match width_p.as_str() {
                            "Er" => Some(19),
                            "Ds" => Some(8),
                            "Ev" => Some(17),
                            "Fl" => Some(12),
                            _ => width_p.len().try_into().ok(),
                        }
                    }
                }
                Rule::bl_offset => {
                    if count.1 > 0 {
                        return true;
                    }
                    count.1 += 1;
                    let offset_p = pair
                        .into_inner()
                        .find(|p| Rule::offset == p.as_rule())
                        .unwrap();
                    *offset = Some(OffsetType::from(offset_p));
                }
                Rule::compact => {
                    if count.2 > 0 {
                        return true;
                    }
                    count.2 += 1;
                    *compact = true;
                }
                _ => columns.push(pair.as_str().to_string()),
            }
            false
        }

        fn parse_bl_open(pair: Pair<Rule>) -> Macro {
            let mut inner = pair.into_inner();

            // -type
            let bl_type_pair = inner.next().unwrap();
            let list_type = BlType::from(bl_type_pair);

            let mut offset: Option<OffsetType> = None;
            let mut width: Option<u8> = None;
            let mut compact = false;
            let mut columns = vec![];
            let mut count = (0, 0, 0);

            for opt_pair in inner {
                match opt_pair.as_rule() {
                    Rule::bl_param => {
                        for parameter in opt_pair.into_inner() {
                            let has_repeat = parse_bl_parameter(
                                parameter.clone(),
                                &mut width,
                                &mut offset,
                                &mut compact,
                                &mut columns,
                                &mut count,
                            );

                            if has_repeat {
                                columns.extend(
                                    parameter
                                        .as_str()
                                        .split(" ")
                                        .filter(|s| !s.is_empty())
                                        .map(|s| s.to_string())
                                        .collect::<Vec<_>>(),
                                );
                                continue;
                            }
                        }
                    }
                    _ => columns.push(opt_pair.as_str().to_string()),
                }
            }

            Macro::Bl {
                list_type,
                width,
                offset,
                compact,
                columns,
            }
        }

        let mut pairs = pair.into_inner();

        let bl_macro = parse_bl_open(pairs.next().unwrap());

        let nodes = pairs
            .take_while(|p| p.as_rule() != Rule::el_close)
            .filter(|p| p.as_rule() != Rule::bl_skip)
            .map(Self::parse_it_block)
            .collect();

        Element::Macro(MacroNode {
            mdoc_macro: bl_macro,
            nodes,
        })
    }

    fn parse_block_full_explicit(pair: Pair<Rule>) -> Element {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::bd_block => Self::parse_bd_block(pair),
            Rule::bf_block => Self::parse_bf_block(pair),
            Rule::bk_block => Self::parse_bk_block(pair),
            Rule::bl_block => Self::parse_bl_block(pair),
            _ => unreachable!(),
        }
    }
}

// Block full-implicit macros parsing
impl MdocParser {
    // Parses (`It`)[https://man.openbsd.org/mdoc#It]
    // `It [head]`
    fn parse_it_block(pair: Pair<Rule>) -> Element {
        fn string_to_elements(input: &str) -> Vec<Element> {
            if let Ok(pairs) = MdocParser::parse(Rule::args, input) {
                pairs
                    .flat_map(|p| {
                        let inner_rules = p.into_inner();
                        inner_rules.map(MdocParser::parse_element)
                    })
                    .filter(|el| !matches!(el, Element::Eoi))
                    .collect()
            } else {
                vec![]
            }
        }

        let mut inner_pairs = pair.into_inner();

        let mut head: Vec<_> = inner_pairs
            .next()
            .unwrap()
            .into_inner()
            .map(Self::parse_element)
            .collect();

        let mut parse_buffer = String::new();
        let mut new_head = vec![];
        for element in head {
            match element {
                Element::Text(text) => {
                    parse_buffer.push_str(&(text + " "));
                }
                _ => {
                    new_head.extend(string_to_elements(&parse_buffer));
                    parse_buffer.clear();
                    new_head.push(element);
                }
            }
        }

        new_head.extend(string_to_elements(&parse_buffer));
        head = new_head;

        let nodes = inner_pairs
            .next()
            .unwrap()
            .into_inner()
            .map(Self::parse_element)
            .collect::<Vec<_>>();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::It { head },
            nodes,
        })
    }

    // Parses (`Nd`)[https://man.openbsd.org/mdoc#Nd]
    // `Nd line`
    fn parse_nd(pair: Pair<Rule>) -> Element {
        let mut inner_nodes = pair.into_inner();

        let mut nodes: Vec<_> = inner_nodes
            .next()
            .unwrap()
            .into_inner()
            .map(Self::parse_element)
            .collect();

        for body in inner_nodes {
            let mut inner = body.into_inner();
            for pair in inner.by_ref() {
                nodes.push(Self::parse_element(pair));
            }
        }

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Nd,
            nodes,
        })
    }

    // Parses (`Nm`)[https://man.openbsd.org/mdoc#Nm]
    // `Nm [name]`
    fn parse_nm(pair: Pair<Rule>) -> Element {
        let mut inner_pairs = pair.into_inner();

        let mut name = None;
        let mut nodes = vec![];

        if let Some(val) = inner_pairs.next() {
            let val = val.as_str().to_string();
            if val.chars().all(|ch| ch.is_alphanumeric()) {
                name = Some(val);
            } else {
                nodes.push(Element::Text(val));
            }
        }

        nodes.extend(inner_pairs.map(Self::parse_element));

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Nm { name },
            nodes,
        })
    }

    // Parses (`Sh`)[https://man.openbsd.org/mdoc#Sh]
    // `Sh TITLE LINE`
    fn parse_sh_block(pair: Pair<Rule>) -> Element {
        let mut inner = pair.into_inner();

        let title = inner
            .next() // `sh_block` -> `sh_open`
            .unwrap()
            .into_inner()
            .next() // `sh_open` -> `sh_title_line`
            .expect("Expected title for 'Sh' block")
            .as_str()
            .trim_end()
            .to_string();

        // Parse `sh_block_element`
        let nodes = inner
            .filter_map(|p| p.into_inner().next().map(Self::parse_element))
            .collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Sh { title },
            nodes,
        })
    }

    /// Parses (`Ss`)[https://man.openbsd.org/mdoc#Ss]:
    /// `Ss Title line`
    fn parse_ss_block(pair: Pair<Rule>) -> Element {
        let mut inner = pair.into_inner();

        let title = inner
            .next() // `ss_block` -> `ss_open`
            .unwrap()
            .into_inner()
            .next() // `ss_open` -> `ss_title_line`
            .expect("Expected title for 'Ss' block")
            .as_str()
            .trim_end()
            .to_string();

        // Parse `ss_block_element`
        let nodes = inner
            .filter_map(|p| p.into_inner().next().map(Self::parse_element))
            .collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ss { title },
            nodes,
        })
    }

    fn parse_block_full_implicit(pair: Pair<Rule>) -> Element {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::it_block => Self::parse_it_block(pair),
            Rule::nd_block => Self::parse_nd(pair),
            Rule::nm_block => Self::parse_nm(pair),
            Rule::sh_block => Self::parse_sh_block(pair),
            Rule::ss_block => Self::parse_ss_block(pair),
            _ => unreachable!(),
        }
    }
}

// Block partial-implicit macros parsing
impl MdocParser {
    // Parses (`Aq`)[https://man.openbsd.org/mdoc#Aq]:
    // `Aq line`
    fn parse_aq_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Aq,
            nodes,
        })
    }

    // Parses (`Bq`)[https://man.openbsd.org/mdoc#Bq]:
    // `Bq line`
    fn parse_bq_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Bq,
            nodes,
        })
    }

    // Parses (`Brq`)[https://man.openbsd.org/mdoc#Brq]:
    // `Brq line`
    fn parse_brq_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Brq,
            nodes,
        })
    }

    // Parses (`D1`)[https://man.openbsd.org/mdoc#D1]:
    // `D1 line`
    fn parse_d1_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::D1,
            nodes,
        })
    }

    // Parses (`Dl`)[https://man.openbsd.org/mdoc#Dl]:
    // `Dl line`
    fn parse_dl_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Dl,
            nodes,
        })
    }

    // Parses (`Dq`)[https://man.openbsd.org/mdoc#Dq]:
    // `Dq line`
    fn parse_dq_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Dq,
            nodes,
        })
    }

    // Parses (`En`)[https://man.openbsd.org/mdoc#En]:
    // `En word ...`
    fn parse_en_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::En,
            nodes,
        })
    }

    // Parses (`Op`)[https://man.openbsd.org/mdoc#Op]:
    // `Op line`
    fn parse_op_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Op,
            nodes,
        })
    }

    // Parses (`Pq`)[https://man.openbsd.org/mdoc#Pq]:
    // `Pq line`
    fn parse_pq_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Pq,
            nodes,
        })
    }

    // Parses (`Ql`)[https://man.openbsd.org/mdoc#Ql]:
    // `Ql line`
    fn parse_ql_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ql,
            nodes,
        })
    }

    // Parses (`Qq`)[https://man.openbsd.org/mdoc#Qq]:
    // `Qq line`
    fn parse_qq_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Qq,
            nodes,
        })
    }

    // Parses (`Sq`)[https://man.openbsd.org/mdoc#Sq]:
    // `Sq line`
    fn parse_sq_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Sq,
            nodes,
        })
    }

    // Parses (`Vt`)[https://man.openbsd.org/mdoc#Vt]:
    // `Vt type [identifier] ...`
    fn parse_vt_block(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Vt,
            nodes,
        })
    }

    fn parse_block_partial_implicit(pair: Pair<Rule>) -> Element {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::aq_block => Self::parse_aq_block(pair),
            Rule::bq_block => Self::parse_bq_block(pair),
            Rule::brq_block => Self::parse_brq_block(pair),
            Rule::d1_block => Self::parse_d1_block(pair),
            Rule::dl_block => Self::parse_dl_block(pair),
            Rule::dq_block => Self::parse_dq_block(pair),
            Rule::en_block => Self::parse_en_block(pair),
            Rule::op_block => Self::parse_op_block(pair),
            Rule::pq_block => Self::parse_pq_block(pair),
            Rule::ql_block => Self::parse_ql_block(pair),
            Rule::qq_block => Self::parse_qq_block(pair),
            Rule::sq_block => Self::parse_sq_block(pair),
            Rule::vt_block => Self::parse_vt_block(pair),
            _ => unreachable!(),
        }
    }
}

// Block partial-explicit parsing
impl MdocParser {
    // Parses (`Ao`)[https://man.openbsd.org/mdoc#Ao]:
    // `Ao block`
    fn parse_ao_block(pair: Pair<Rule>) -> Element {
        let inner_pairs = pair.into_inner();
        let mut nodes: Vec<_> = inner_pairs
            .clone()
            .take_while(|p| p.as_rule() != Rule::ac)
            .flat_map(|p| p.into_inner().map(Self::parse_element).collect::<Vec<_>>())
            .collect();

        let ac = inner_pairs
            .skip_while(|p| p.as_rule() != Rule::ac)
            .map(Self::parse_ac);

        nodes.extend(ac);

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ao,
            nodes,
        })
    }

    // Parses (`Ac`)[https://man.openbsd.org/mdoc#Ac]:
    // `Ac`
    fn parse_ac(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ac,
            nodes,
        })
    }

    // Parses (`Bo`)[https://man.openbsd.org/mdoc#Bo]:
    // `Bo block`
    fn parse_bo_block(pair: Pair<Rule>) -> Element {
        let inner_pairs = pair.into_inner();
        let mut nodes: Vec<_> = inner_pairs
            .clone()
            .take_while(|p| p.as_rule() != Rule::bc)
            .flat_map(|p| p.into_inner().map(Self::parse_element).collect::<Vec<_>>())
            .collect();

        let bc = inner_pairs
            .skip_while(|p| p.as_rule() != Rule::bc)
            .map(Self::parse_bc);

        nodes.extend(bc);
        Element::Macro(MacroNode {
            mdoc_macro: Macro::Bo,
            nodes,
        })
    }

    // Parses (`Bc`)[https://man.openbsd.org/mdoc#Bc]:
    // `Bc`
    fn parse_bc(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Bc,
            nodes,
        })
    }

    // Parses (`Bro`)[https://man.openbsd.org/mdoc#Bro]:
    // `Bro`
    fn parse_bro_block(pair: Pair<Rule>) -> Element {
        let inner_pairs = pair.into_inner();
        let mut nodes: Vec<_> = inner_pairs
            .clone()
            .take_while(|p| p.as_rule() != Rule::brc)
            .flat_map(|p| p.into_inner().map(Self::parse_element).collect::<Vec<_>>())
            .collect();

        let brc = inner_pairs
            .skip_while(|p| p.as_rule() != Rule::brc)
            .map(Self::parse_brc);

        nodes.extend(brc);

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Bro,
            nodes,
        })
    }

    // Parses (`Brc`)[https://man.openbsd.org/mdoc#Brc]:
    // `Brc`
    fn parse_brc(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Brc,
            nodes,
        })
    }

    // Parses (`Do`)[https://man.openbsd.org/mdoc#Do]:
    // `Do`
    fn parse_do_block(pair: Pair<Rule>) -> Element {
        let inner_pairs = pair.into_inner();
        let mut nodes: Vec<_> = inner_pairs
            .clone()
            .take_while(|p| p.as_rule() != Rule::dc)
            .flat_map(|p| p.into_inner().map(Self::parse_element).collect::<Vec<_>>())
            .collect();

        let dc = inner_pairs
            .skip_while(|p| p.as_rule() != Rule::dc)
            .map(Self::parse_dc);

        nodes.extend(dc);

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Do,
            nodes,
        })
    }

    // Parses (`Dc`)[https://man.openbsd.org/mdoc#Dc]:
    // `Dc block`
    fn parse_dc(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Dc,
            nodes,
        })
    }

    // Parses (`Eo`)[https://man.openbsd.org/mdoc#Eo]:
    // `Eo block`
    fn parse_eo_block(pair: Pair<Rule>) -> Element {
        let mut inner_pairs = pair.into_inner();

        let head = inner_pairs.next().unwrap().into_inner();

        let mut nodes = Vec::new();
        let mut opening_delimiter = None;
        let mut closing_delimiter = None;

        for arg in head {
            if arg.as_rule() == Rule::opening_delimiter {
                opening_delimiter = Some(arg.as_str().parse::<char>().unwrap());
            } else {
                nodes.push(Self::parse_element(arg));
            }
        }

        let next_arg = inner_pairs.next().unwrap();
        match next_arg.as_rule() {
            Rule::ec => {
                if let Some(arg) = next_arg.into_inner().next() {
                    closing_delimiter = Some(arg.as_str().parse::<char>().unwrap());
                }
            }
            Rule::eo_body => {
                let iter = next_arg
                    .into_inner()
                    .take_while(|p| p.as_rule() != Rule::ec)
                    .map(Self::parse_element);

                nodes.extend(iter);

                if let Some(arg) = inner_pairs.next().unwrap().into_inner().next() {
                    closing_delimiter = Some(arg.as_str().parse::<char>().unwrap());
                }
            }
            _ => unreachable!(),
        }

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Eo {
                opening_delimiter,
                closing_delimiter,
            },
            nodes,
        })
    }

    // Parses (`Ec`)[https://man.openbsd.org/mdoc#Ec]:
    // `Ec`
    fn parse_ec(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ec,
            nodes,
        })
    }

    // Parses (`Fo`)[https://man.openbsd.org/mdoc#Fo]:
    // `Fo block`
    fn parse_fo_block(pair: Pair<Rule>) -> Element {
        let mut inner_pairs = pair.into_inner();
        let mut head = inner_pairs.next().unwrap().into_inner();

        let funcname = head.next().unwrap().as_str().to_string();
        let mut nodes: Vec<_> = head.map(Self::parse_element).collect();

        nodes.extend(inner_pairs.filter_map(|p| p.into_inner().next().map(Self::parse_element)));

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Fo { funcname },
            nodes,
        })
    }

    // Parses (`Fc`)[https://man.openbsd.org/mdoc#Fc]:
    // `Fc`
    fn parse_fc(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Fc,
            nodes,
        })
    }

    // Parses (`Oo`)[https://man.openbsd.org/mdoc#Oo]:
    // `Oo block`
    fn parse_oo_block(pair: Pair<Rule>) -> Element {
        let inner_pairs = pair.into_inner();
        let mut nodes: Vec<_> = inner_pairs
            .clone()
            .take_while(|p| p.as_rule() != Rule::oc)
            .flat_map(|p| p.into_inner().map(Self::parse_element).collect::<Vec<_>>())
            .collect();

        let oc = inner_pairs
            .skip_while(|p| p.as_rule() != Rule::oc)
            .map(Self::parse_oc);

        nodes.extend(oc);

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Oo,
            nodes,
        })
    }

    // Parses (`Oc`)[https://man.openbsd.org/mdoc#Oc]:
    // `Oc`
    fn parse_oc(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Oc,
            nodes,
        })
    }

    // Parses (`Po`)[https://man.openbsd.org/mdoc#Po]:
    // `Po block`
    fn parse_po_block(pair: Pair<Rule>) -> Element {
        let inner_pairs = pair.into_inner();
        let mut nodes: Vec<_> = inner_pairs
            .clone()
            .take_while(|p| p.as_rule() != Rule::pc)
            .flat_map(|p| p.into_inner().map(Self::parse_element).collect::<Vec<_>>())
            .collect();

        let pc = inner_pairs
            .skip_while(|p| p.as_rule() != Rule::pc)
            .map(Self::parse_pc);

        nodes.extend(pc);

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Po,
            nodes,
        })
    }

    // Parses (`Pc`)[https://man.openbsd.org/mdoc#Pc]:
    // `Pc`
    fn parse_pc(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Pc,
            nodes,
        })
    }

    // Parses (`Qo`)[https://man.openbsd.org/mdoc#Qo]:
    // `Qo block`
    fn parse_qo_block(pair: Pair<Rule>) -> Element {
        let inner_pairs = pair.into_inner();
        let mut nodes: Vec<_> = inner_pairs
            .clone()
            .take_while(|p| p.as_rule() != Rule::qc)
            .flat_map(|p| p.into_inner().map(Self::parse_element).collect::<Vec<_>>())
            .collect();

        let qc = inner_pairs
            .skip_while(|p| p.as_rule() != Rule::qc)
            .map(Self::parse_qc);

        nodes.extend(qc);

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Qo,
            nodes,
        })
    }

    // Parses (`Qc`)[https://man.openbsd.org/mdoc#Qc]:
    // `Qc`
    fn parse_qc(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Qc,
            nodes,
        })
    }

    // Parses (`Rs`)[https://man.openbsd.org/mdoc#Rs]:
    // `Rs`
    fn parse_rs_block(pair: Pair<Rule>) -> Element {
        fn rs_submacro_cmp(a: &Element, b: &Element) -> std::cmp::Ordering {
            let get_macro_order_position = |n| {
                RS_SUBMACRO_ORDER
                    .iter()
                    .position(|m| discriminant(m) == discriminant(n))
                    .unwrap_or(RS_SUBMACRO_ORDER.len())
            };

            let Element::Macro(MacroNode {
                mdoc_macro: macro_a,
                ..
            }) = a
            else {
                return std::cmp::Ordering::Greater;
            };

            let Element::Macro(MacroNode {
                mdoc_macro: macro_b,
                ..
            }) = b
            else {
                return std::cmp::Ordering::Greater;
            };

            let a_pos = get_macro_order_position(macro_a);
            let b_pos = get_macro_order_position(macro_b);

            a_pos.cmp(&b_pos)
        }

        let mut nodes: Vec<_> = pair
            .into_inner()
            .skip_while(|p| p.as_rule() == Rule::rs_head)
            .take_while(|p| p.as_rule() != Rule::re)
            .filter_map(|p| p.into_inner().next().map(Self::parse_rs_submacro))
            .collect();

        nodes.sort_by(rs_submacro_cmp);

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Rs,
            nodes,
        })
    }

    // Parses (`Re`)[https://man.openbsd.org/mdoc#Re]:
    // `Re`
    fn parse_re(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Re,
            nodes,
        })
    }

    // Parses (`So`)[https://man.openbsd.org/mdoc#So]:
    // `So block`
    fn parse_so_block(pair: Pair<Rule>) -> Element {
        let inner_pairs = pair.into_inner();
        let mut nodes: Vec<_> = inner_pairs
            .clone()
            .take_while(|p| p.as_rule() != Rule::sc)
            .flat_map(|p| p.into_inner().map(Self::parse_element).collect::<Vec<_>>())
            .collect();

        let sc = inner_pairs
            .skip_while(|p| p.as_rule() != Rule::sc)
            .map(Self::parse_sc);

        nodes.extend(sc);

        Element::Macro(MacroNode {
            mdoc_macro: Macro::So,
            nodes,
        })
    }

    // Parses (`Sc`)[https://man.openbsd.org/mdoc#Sc]:
    // `Sc`
    fn parse_sc(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Sc,
            nodes,
        })
    }

    // Parses (`Xo`)[https://man.openbsd.org/mdoc#Xo]:
    // `Xo block`
    fn parse_xo_block(pair: Pair<Rule>) -> Element {
        let inner_pairs = pair.into_inner();
        let mut nodes: Vec<_> = inner_pairs
            .clone()
            .take_while(|p| p.as_rule() != Rule::xc)
            .flat_map(|p| p.into_inner().map(Self::parse_element).collect::<Vec<_>>())
            .collect();

        let xc = inner_pairs
            .skip_while(|p| p.as_rule() != Rule::xc)
            .map(Self::parse_xc);

        nodes.extend(xc);

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Xo,
            nodes,
        })
    }

    // Parses (`Xc`)[https://man.openbsd.org/mdoc#Xc]:
    // `Xc`
    fn parse_xc(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Xc,
            nodes,
        })
    }

    fn parse_block_partial_explicit(pair: Pair<Rule>) -> Element {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::ao_block => Self::parse_ao_block(pair),
            Rule::bo_block => Self::parse_bo_block(pair),
            Rule::bro_block => Self::parse_bro_block(pair),
            Rule::do_block => Self::parse_do_block(pair),
            Rule::eo_block => Self::parse_eo_block(pair),
            Rule::fo_block => Self::parse_fo_block(pair),
            Rule::oo_block => Self::parse_oo_block(pair),
            Rule::po_block => Self::parse_po_block(pair),
            Rule::qo_block => Self::parse_qo_block(pair),
            Rule::rs_block => Self::parse_rs_block(pair),
            Rule::so_block => Self::parse_so_block(pair),
            Rule::xo_block => Self::parse_xo_block(pair),
            Rule::ac => Self::parse_ac(pair),
            Rule::bc => Self::parse_bc(pair),
            Rule::brc => Self::parse_brc(pair),
            Rule::dc => Self::parse_dc(pair),
            Rule::ec => Self::parse_ec(pair),
            Rule::fc => Self::parse_fc(pair),
            Rule::oc => Self::parse_oc(pair),
            Rule::pc => Self::parse_pc(pair),
            Rule::qc => Self::parse_qc(pair),
            Rule::re => Self::parse_re(pair),
            Rule::sc => Self::parse_sc(pair),
            Rule::xc => Self::parse_xc(pair),
            _ => unreachable!(),
        }
    }
}

/// Trim `"` quotes from [`String`]
pub fn trim_quotes(mut s: String) -> String {
    if !s.starts_with("\\&\"") {
        if let Some(stripped) = s.strip_prefix("\"") {
            s = stripped.to_string();
        }
    }
    if !s.ends_with("\\&\"") {
        if let Some(stripped) = s.strip_suffix("\"") {
            s = stripped.to_string();
        }
    }

    s
}

// In-line macros parsing
impl MdocParser {
    fn parse_rs_submacro(pair: Pair<Rule>) -> Element {
        // Parses (`%A`)[https://man.openbsd.org/mdoc#_A]:
        // `%A first_name ... last_name`
        fn parse_a(pair: Pair<Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::A,
                nodes,
            })
        }

        // Parses (`%B`)[https://man.openbsd.org/mdoc#_B]:
        // `%B title`
        fn parse_b(pair: Pair<Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::B,
                nodes,
            })
        }

        // Parses (`%C`)[https://man.openbsd.org/mdoc#_C]:
        // `%C location`
        fn parse_c(pair: Pair<'_, Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::C,
                nodes,
            })
        }

        // Parses (`%D`)[https://man.openbsd.org/mdoc#_D]:
        // `%D [month day,] year`
        fn parse_d(pair: Pair<'_, Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::D,
                nodes,
            })
        }

        // Parses (`%I`)[https://man.openbsd.org/mdoc#_I]:
        // `%I name`
        fn parse_i(pair: Pair<'_, Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::I,
                nodes,
            })
        }

        // Parses (`%J`)[https://man.openbsd.org/mdoc#_J]:
        // `%J name`
        fn parse_j(pair: Pair<'_, Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::J,
                nodes,
            })
        }

        // Parses (`%N`)[https://man.openbsd.org/mdoc#_N]:
        // `%N number`
        fn parse_n(pair: Pair<'_, Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::N,
                nodes,
            })
        }

        // Parses (`%O`)[https://man.openbsd.org/mdoc#_O]:
        // `%O line`
        fn parse_o(pair: Pair<'_, Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::O,
                nodes,
            })
        }

        // Parses (`%P`)[https://man.openbsd.org/mdoc#_P]:
        // `%P number`
        fn parse_p(pair: Pair<'_, Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::P,
                nodes,
            })
        }

        // Parses (`%Q`)[https://man.openbsd.org/mdoc#_Q]:
        // `%Q name`
        fn parse_q(pair: Pair<'_, Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::Q,
                nodes,
            })
        }

        // Parses (`%R`)[https://man.openbsd.org/mdoc#_R]:
        // `%R name`
        fn parse_r(pair: Pair<'_, Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::R,
                nodes,
            })
        }

        // Parses (`%T`)[https://man.openbsd.org/mdoc#_T]:
        // `%T title`
        fn parse_t(pair: Pair<'_, Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::T,
                nodes,
            })
        }

        // Parses (`%U`)[https://man.openbsd.org/mdoc#_U]:
        // `%U protocol://path`
        fn parse_u(pair: Pair<'_, Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::U,
                nodes,
            })
        }

        // Parses (`%V`)[https://man.openbsd.org/mdoc#_V]:
        // `%V number`
        fn parse_v(pair: Pair<'_, Rule>) -> Element {
            let nodes = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .map(MdocParser::parse_element)
                .collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::V,
                nodes,
            })
        }

        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::a => parse_a(pair),
            Rule::b => parse_b(pair),
            Rule::c => parse_c(pair),
            Rule::d => parse_d(pair),
            Rule::i => parse_i(pair),
            Rule::j => parse_j(pair),
            Rule::n => parse_n(pair),
            Rule::o => parse_o(pair),
            Rule::p => parse_p(pair),
            Rule::q => parse_q(pair),
            Rule::r => parse_r(pair),
            Rule::t => parse_t(pair),
            Rule::u => parse_u(pair),
            Rule::v => parse_v(pair),
            _ => unreachable!(),
        }
    }

    fn process_delimiters(inner: &[Pair<Rule>], mut i: usize, rule: Rule) -> (Vec<Element>, usize) {
        let mut nodes = Vec::new();
        while i < inner.len() && inner[i].as_rule() == rule {
            nodes.push(MdocParser::parse_element(inner[i].clone()));
            i += 1;
        }
        (nodes, i)
    }

    fn parse_text_production(pair: Pair<Rule>) -> Element {
        fn parse_x_args<F, D>(
            pair: Pair<Rule>,
            macro_value: Macro,
            format: F,
            format_default: D,
        ) -> Element
        where
            F: Fn(&str) -> String,
            D: Fn() -> String,
        {
            let inner: Vec<_> = pair.into_inner().collect();

            if inner.is_empty() {
                return Element::Macro(MacroNode {
                    mdoc_macro: macro_value,
                    nodes: vec![Element::Text(format_default())],
                });
            }

            let mut nodes = Vec::new();
            let mut i = 0;

            // Process opening delimiters.
            let (open_nodes, new_i) =
                MdocParser::process_delimiters(&inner, i, Rule::opening_delimiter);
            nodes.extend(open_nodes);
            i = new_i;

            // Process the middle argument if it exists.
            if i < inner.len() {
                match inner[i].as_rule() {
                    Rule::text_arg => {
                        nodes.push(Element::Text(format(inner[i].as_str())));
                        i += 1;
                    }
                    Rule::closing_delimiter => {
                        nodes.push(Element::Text(format_default()));
                        nodes.push(Element::Text(inner[i].as_str().to_string()));
                        i += 1;
                    }
                    _ => unreachable!(),
                }
            }

            // Process closing delimiters.
            let (close_nodes, new_i) =
                MdocParser::process_delimiters(&inner, i, Rule::closing_delimiter);
            nodes.extend(close_nodes);

            i = new_i;
            while i < inner.len() {
                nodes.push(MdocParser::parse_element(inner[i].clone()));
                i += 1;
            }

            Element::Macro(MacroNode {
                mdoc_macro: macro_value,
                nodes,
            })
        }

        // Parses (`At`)[https://man.openbsd.org/mdoc#At]:
        // `At [version]`
        fn parse_at(pair: Pair<Rule>) -> Element {
            let inner: Vec<_> = pair.into_inner().collect();

            if inner.is_empty() {
                return Element::Macro(MacroNode {
                    mdoc_macro: Macro::At,
                    nodes: vec![Element::Text(AtType::default().to_string())],
                });
            }

            let mut i = 0;
            let mut nodes = Vec::new();

            let (open_nodes, new_i) =
                MdocParser::process_delimiters(&inner, i, Rule::opening_delimiter);
            nodes.extend(open_nodes);
            i = new_i;

            if i < inner.len() {
                match inner[i].as_rule() {
                    Rule::text_arg => {
                        nodes.push(Element::Text(AtType::default().to_string()));
                        nodes.push(MdocParser::parse_element(inner[i].clone()));
                        i += 1;
                    }
                    Rule::at_type => {
                        nodes.push(Element::Text(AtType::from(inner[i].clone()).to_string()));
                        i += 1;
                    }
                    Rule::closing_delimiter => {
                        nodes.push(Element::Text(AtType::default().to_string()));
                    }
                    _ => unreachable!(),
                }
            }

            let (close_nodes, new_i) =
                MdocParser::process_delimiters(&inner, i, Rule::closing_delimiter);
            nodes.extend(close_nodes);

            i = new_i;
            while i < inner.len() {
                nodes.push(MdocParser::parse_element(inner[i].clone()));
                i += 1;
            }

            Element::Macro(MacroNode {
                mdoc_macro: Macro::At,
                nodes,
            })
        }

        // Parses (`Bsx`)[https://man.openbsd.org/mdoc#Bsx]:
        // `Bsx [version]`
        fn parse_bsx(pair: Pair<Rule>) -> Element {
            parse_x_args(pair, Macro::Bsx, BsxType::format, BsxType::format_default)
        }

        // Parses (`Bx`)[https://man.openbsd.org/mdoc#Bx]:
        // `Bx [version [variant]]`
        fn parse_bx(pair: Pair<Rule>) -> Element {
            let inner: Vec<_> = pair.into_inner().collect();

            if inner.is_empty() {
                return Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bx,
                    nodes: vec![Element::Text(BxType::format_default())],
                });
            }

            let mut nodes = Vec::new();
            let mut i = 0;

            let (open_nodes, new_i) =
                MdocParser::process_delimiters(&inner, i, Rule::opening_delimiter);
            nodes.extend(open_nodes);
            i = new_i;

            if i < inner.len() {
                match inner[i].as_rule() {
                    Rule::text_arg => {
                        let version = inner[i].as_str();

                        i += 1;

                        let variant = match i < inner.len() && inner[i].as_rule() == Rule::text_arg
                        {
                            true => {
                                let res = Some(inner[i].as_str());
                                i += 1;
                                res
                            }
                            false => None,
                        };

                        nodes.push(Element::Text(BxType::format(version, variant)));
                    }
                    Rule::closing_delimiter => nodes.push(Element::Text(BxType::format_default())),
                    _ => unreachable!(),
                }
            }

            let (close_nodes, new_i) =
                MdocParser::process_delimiters(&inner, i, Rule::closing_delimiter);
            nodes.extend(close_nodes);

            i = new_i;
            while i < inner.len() {
                nodes.push(MdocParser::parse_element(inner[i].clone()));
                i += 1;
            }

            Element::Macro(MacroNode {
                mdoc_macro: Macro::Bx,
                nodes,
            })
        }

        // Parses (`Dx`)[https://man.openbsd.org/mdoc#Dx]:
        // `Dx [version]`
        fn parse_dx(pair: Pair<Rule>) -> Element {
            parse_x_args(pair, Macro::Dx, DxType::format, DxType::format_default)
        }

        // Parses (`Fx`)[https://man.openbsd.org/mdoc#Fx]:
        // `Fx [version]`
        fn parse_fx(pair: Pair<Rule>) -> Element {
            parse_x_args(pair, Macro::Fx, FxType::format, FxType::format_default)
        }

        // Parses (`Ex`)[https://man.openbsd.org/mdoc#Ex]
        // .Ex VAR, ...
        fn parse_ex(pair: Pair<Rule>) -> Element {
            let nodes = pair.into_inner().map(MdocParser::parse_element).collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::Ex,
                nodes,
            })
        }

        // Parses (`Nx`)[http://man.openbsd.org/mdoc#Nx]:
        // `Nx [version]`
        fn parse_nx(pair: Pair<Rule>) -> Element {
            parse_x_args(pair, Macro::Nx, NxType::format, NxType::format_default)
        }

        // Parses (`Ox`)[https://man.openbsd.org/mdoc#Ox]:
        // `Ox [version]`
        fn parse_ox(pair: Pair<Rule>) -> Element {
            parse_x_args(pair, Macro::Ox, OxType::format, OxType::format_default)
        }

        // Parses (`St`)[https://man.openbsd.org/mdoc#St]:
        // `St -abbreviation`
        fn parse_st(pair: Pair<Rule>) -> Element {
            let mut inner = pair.into_inner();

            let st_type = StType::from(inner.next().unwrap());
            let nodes: Vec<_> = inner.map(MdocParser::parse_element).collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::St(st_type),
                nodes,
            })
        }

        // Parses (`Rv`)[https://man.openbsd.org/mdoc#Rv]:
        // `Rv -std [function ...]`
        fn parse_rv(pair: Pair<Rule>) -> Element {
            let nodes = pair.into_inner().map(MdocParser::parse_element).collect();

            Element::Macro(MacroNode {
                mdoc_macro: Macro::Rv,
                nodes,
            })
        }

        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::at => parse_at(pair),
            Rule::bsx => parse_bsx(pair),
            Rule::bx => parse_bx(pair),
            Rule::dx => parse_dx(pair),
            Rule::fx => parse_fx(pair),
            Rule::ex => parse_ex(pair),
            Rule::nx => parse_nx(pair),
            Rule::ox => parse_ox(pair),
            Rule::st => parse_st(pair),
            Rule::rv => parse_rv(pair),
            _ => unreachable!(),
        }
    }

    // Parses (`Ad`)[https://man.openbsd.org/mdoc#Ad]:
    // `Ad address`
    fn parse_ad(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ad,
            nodes,
        })
    }

    // Parses (`An`)[https://man.openbsd.org/mdoc#An]:
    // `An -split | -nosplit | first_name ... last_name`
    fn parse_an(pair: Pair<Rule>) -> Element {
        let an_arg = pair.into_inner().next().unwrap();
        let (author_name_type, nodes) = match an_arg.as_rule() {
            Rule::an_split => (AnType::Split, vec![]),
            Rule::an_no_split => (AnType::NoSplit, vec![]),
            Rule::an_name => (
                AnType::Name,
                an_arg.into_inner().map(Self::parse_element).collect(),
            ),
            _ => unreachable!(),
        };

        Element::Macro(MacroNode {
            mdoc_macro: Macro::An { author_name_type },
            nodes,
        })
    }

    // Parses (`Ap`)[https://man.openbsd.org/mdoc#Ap]:
    // `Ap`
    fn parse_ap(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ap,
            nodes,
        })
    }

    // Parses (`Ar`)[https://man.openbsd.org/mdoc#Ar]:
    // `Ar [placeholder ...]`
    fn parse_ar(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ar,
            nodes,
        })
    }

    // Parses (`Bt`)[https://man.openbsd.org/mdoc#Bt]:
    // `Bt`
    fn parse_bt(_pair: Pair<Rule>) -> Element {
        Element::Macro(MacroNode {
            mdoc_macro: Macro::Bt,
            nodes: vec![],
        })
    }

    // Parses (`Cd`)[https://man.openbsd.org/mdoc#Cd]:
    // `Cd line`
    fn parse_cd(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Cd,
            nodes,
        })
    }

    // Parses (`Cd`)[https://man.openbsd.org/mdoc#Cm]:
    // `Cm keyword ...`
    fn parse_cm(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Cm,
            nodes,
        })
    }

    // Parses (`Db`)[https://man.openbsd.org/mdoc#Db]
    // Obsolete
    fn parse_db(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Db,
            nodes,
        })
    }

    // Parses (`Dd`)[https://man.openbsd.org/mdoc#Dd]
    // `Dd [date]`
    fn parse_dd(pair: Pair<Rule>) -> Element {
        let mut inner = pair.into_inner();

        let nodes = match inner.next() {
            Some(line) => vec![Element::Text(line.as_str().to_string())],
            None => Vec::new(),
        };

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Dd,
            nodes,
        })
    }

    // Parses (`Dt`)[https://man.openbsd.org/mdoc#Dt]
    fn parse_dt(pair: Pair<Rule>) -> Element {
        let mut inner = pair.into_inner();

        let (title, section) = if let Some(arg) = inner.next() {
            if matches!(arg.as_rule(), Rule::title) {
                let title = Some(arg.as_str().to_string());
                let section = inner.next().unwrap().as_str().to_string();

                (title, section)
            } else {
                let section = arg.as_str().to_string();

                (None, section)
            }
        } else {
            unreachable!()
        };

        let arch = inner.next().map(|arch| arch.as_str().trim().to_string());

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Dt {
                title,
                section,
                arch,
            },
            nodes: vec![],
        })
    }

    // Parses (`Dv`)[https://man.openbsd.org/mdoc#Dv]
    fn parse_dv(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Dv,
            nodes,
        })
    }

    // Parses (`Em`)[https://man.openbsd.org/mdoc#Em]
    // .Em word ...
    fn parse_em(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Em,
            nodes,
        })
    }

    // Parses (`Er`)[https://man.openbsd.org/mdoc#Er]
    // .Er CONSTANT ...
    fn parse_er(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Er,
            nodes,
        })
    }

    // Parses (`Es`)[https://man.openbsd.org/mdoc#Es]
    // .Es opening_delimiter closing_delimiter
    fn parse_es(pair: Pair<Rule>) -> Element {
        let mut inner_pairs = pair.into_inner();

        let opening_delimiter = inner_pairs
            .next()
            .unwrap()
            .as_str()
            .parse::<char>()
            .unwrap();
        let closing_delimiter = inner_pairs
            .next()
            .unwrap()
            .as_str()
            .parse::<char>()
            .expect("Macro Es expected closing delimiter as the second argument");

        let nodes = inner_pairs.map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Es {
                opening_delimiter,
                closing_delimiter,
            },
            nodes,
        })
    }

    // Parses (`Ev`)[https://man.openbsd.org/mdoc#Ev]
    // .Ev VAR, ...
    fn parse_ev(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ev,
            nodes,
        })
    }

    // Parses (`Fa`)[https://man.openbsd.org/mdoc#Fa]
    // .Fa [args]
    fn parse_fa(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Fa,
            nodes,
        })
    }

    // Parses (`Fd`)[https://man.openbsd.org/mdoc#Fd]
    // .Fd directive [args]
    fn parse_fd(pair: Pair<Rule>) -> Element {
        let mut inner = pair.into_inner();

        let directive = inner.next().unwrap().as_str().to_string();

        let mut args = vec![];

        for arg in inner {
            args.push(arg.as_str().to_string());
        }

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Fd {
                directive,
                arguments: args,
            },
            nodes: vec![],
        })
    }

    // Parses (`Fl`)[https://man.openbsd.org/mdoc#Fl]
    fn parse_fl(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Fl,
            nodes,
        })
    }

    // Parses (`Fn`)[https://man.openbsd.org/mdoc#Fn]
    fn parse_fn(pair: Pair<Rule>) -> Element {
        let mut inner_nodes = pair.into_inner();
        let mut funcname = String::new();
        let arg = inner_nodes.next().unwrap();

        match arg.as_rule() {
            Rule::opening_delimiter => {
                funcname.push_str(arg.as_str());
                let name = inner_nodes.next().unwrap();
                funcname.push_str(name.as_str());
            }
            Rule::text_arg => funcname.push_str(arg.as_str()),
            _ => unreachable!(),
        };

        let nodes = inner_nodes
            .map(|n| {
                if n.as_rule() == Rule::text_arg {
                    return Element::Text(trim_quotes(n.as_str().to_string()));
                }
                Self::parse_element(n)
            })
            .collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Fn { funcname },
            nodes,
        })
    }

    // Parses (`Fr`)[https://man.openbsd.org/mdoc#Fr]
    // Obsolete
    // .Fr num
    fn parse_fr(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(MdocParser::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Fr,
            nodes,
        })
    }

    // Parses (`Ft`)[https://man.openbsd.org/mdoc#Ft]
    fn parse_ft(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ft,
            nodes,
        })
    }

    // Parses (`Hf`)[https://man.openbsd.org/mdoc#Hf]
    // .Hf filename
    fn parse_hf(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(MdocParser::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Hf,
            nodes,
        })
    }

    // Parses (`Ic`)[https://man.openbsd.org/mdoc#Ic]
    // .Ic keyword
    fn parse_ic(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(MdocParser::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ic,
            nodes,
        })
    }

    // Parses (`In`)[https://man.openbsd.org/mdoc#In]
    // .In filename
    fn parse_in(pair: Pair<Rule>) -> Element {
        let mut inner_pairs = pair.into_inner();
        // let mut filename =  String::new();
        // let mut nodes = Vec::new();
        let arg = inner_pairs.next().unwrap();

        let filename = match arg.as_rule() {
            Rule::opening_delimiter => {
                // nodes.push(Element::Text(arg.as_str().to_string()));
                let name = inner_pairs.next().unwrap().as_str();
                // filename.push_str(name);
                format!("{}{}", arg.as_str(), name)
            }
            Rule::word => arg.as_str().to_string(),
            _ => unreachable!(),
        };

        // let iter = inner_pairs.map(Self::parse_element);
        // nodes.extend(iter);
        let nodes = inner_pairs.map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::In { filename },
            nodes,
        })
    }

    // Parses (`Lb`)[https://man.openbsd.org/mdoc#Lb]
    // .Lb libname
    fn parse_lb(pair: Pair<Rule>) -> Element {
        let mut inner_pairs = pair.into_inner();
        let mut lib_name = String::new();
        let mut nodes = Vec::new();
        let arg = inner_pairs.next().unwrap();

        match arg.as_rule() {
            Rule::opening_delimiter => {
                nodes.push(Element::Text(arg.as_str().to_string()));
                let name = inner_pairs.next().unwrap().as_str();
                lib_name.push_str(name);
            }
            Rule::word => lib_name.push_str(arg.as_str()),
            _ => unreachable!(),
        }

        if let Some(del) = inner_pairs.next() {
            nodes.push(Element::Text(del.as_str().to_string()));
        }

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Lb { lib_name },
            nodes,
        })
    }

    // Parses (`Li`)[https://man.openbsd.org/mdoc#Li]
    // .Li word ...
    fn parse_li(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Li,
            nodes,
        })
    }

    // Parses (`Lk`)[https://man.openbsd.org/mdoc#Lk]
    // .Lk link [display_name]
    fn parse_lk(pair: Pair<Rule>) -> Element {
        let mut inner = pair.into_inner();

        let uri = inner.next().unwrap().as_str().to_string();
        let nodes = inner.map(MdocParser::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Lk { uri },
            nodes,
        })
    }

    // Parses (`Lp`)[https://man.openbsd.org/mdoc#Lp]
    // Deprecated
    fn parse_lp(_pair: Pair<Rule>) -> Element {
        Element::Macro(MacroNode {
            mdoc_macro: Macro::Lp,
            nodes: vec![],
        })
    }

    // ---------------------------------------------------------------------------

    // Parses (`Ms`)[https://man.openbsd.org/mdoc#Ms]:
    // `Ms name`
    fn parse_ms(pair: Pair<Rule>) -> Element {
        let nodes = pair
            .into_inner()
            .take_while(|p| p.as_rule() == Rule::text_arg)
            .map(Self::parse_element)
            .collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ms,
            nodes,
        })
    }

    // Parses (`Mt`)[https://man.openbsd.org/mdoc#Mt]:
    // `Mt localpart@domain`
    fn parse_mt(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Mt,
            nodes,
        })
    }

    // Parses (`No`)[https://man.openbsd.org/mdoc#No]:
    // `No word ...`

    fn parse_no(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::No,
            nodes,
        })
    }

    // Parses (`Ns`)[https://man.openbsd.org/mdoc#Ns]:
    // `Ns`
    fn parse_ns(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ns,
            nodes,
        })
    }

    // Parses (`Os`)[https://man.openbsd.org/mdoc#Os]:
    // `Os [footer text]`
    fn parse_os(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(MdocParser::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Os,
            nodes,
        })
    }

    // Parses (`Ot`)[https://man.openbsd.org/mdoc#Ot]:
    // `Ot functype`
    fn parse_ot(pair: Pair<Rule>) -> Element {
        let nodes = pair
            .into_inner()
            .take_while(|p| p.as_rule() == Rule::text_arg)
            .map(Self::parse_element)
            .collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ft,
            nodes,
        })
    }

    // Parses (`Pa`)[https://man.openbsd.org/mdoc#Pa]:
    // `Pa name ...`
    fn parse_pa(pair: Pair<Rule>) -> Element {
        let nodes = pair
            .into_inner()
            .take_while(|p| p.as_rule() == Rule::text_arg)
            .map(Self::parse_element)
            .collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Pa,
            nodes,
        })
    }

    // Parses (`Pf`)[https://man.openbsd.org/mdoc#Pf]:
    // `Pf prefix macro [argument ...]`
    fn parse_pf(pair: Pair<Rule>) -> Element {
        let mut inner_pairs = pair.into_inner();

        let prefix = inner_pairs.next().unwrap().as_str().to_string();

        let nodes = inner_pairs.map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Pf { prefix },
            nodes,
        })
    }

    // Parses (`Pp`)[https://man.openbsd.org/mdoc#Pp]:
    // `Pp`
    fn parse_pp(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(MdocParser::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Pp,
            nodes,
        })
    }

    // Parses (`Sm`)[https://man.openbsd.org/mdoc#Sm]:
    // `Sm [on | off]`
    fn parse_sm(pair: Pair<Rule>) -> Element {
        fn parse_spacing_mode(pair: Pair<Rule>) -> SmMode {
            match pair.as_rule() {
                Rule::sm_on => SmMode::On,
                Rule::sm_off => SmMode::Off,
                _ => unreachable!(),
            }
        }

        let mut inner = pair.into_inner();

        let spacing_mode = if let Some(sm_arg) = inner.next() {
            match sm_arg.as_rule() {
                Rule::spacing_mode => {
                    let sm_arg = sm_arg.into_inner().next().unwrap();
                    Some(parse_spacing_mode(sm_arg))
                }
                _ => None,
            }
        } else {
            None
        };

        let nodes = inner.map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Sm(spacing_mode),
            nodes,
        })
    }

    // Parses (`Sx`)[https://man.openbsd.org/mdoc#Sx]:
    // `Sx Title line`
    fn parse_sx(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Sx,
            nodes,
        })
    }

    // Parses (`Sy`)[https://man.openbsd.org/mdoc#Sy]:
    // `Sy word ...`
    fn parse_sy(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Sy,
            nodes,
        })
    }

    // Parses (`Tg`)[https://man.openbsd.org/mdoc#Tg]:
    // `Tg [term]`
    fn parse_tg(pair: Pair<Rule>) -> Element {
        let mut nodes = pair.into_inner().map(Self::parse_element);

        let term = match nodes.next() {
            Some(Element::Text(term)) => {
                if term.is_empty() {
                    None
                } else {
                    Some(term)
                }
            }
            None => None,
            _ => unreachable!(),
        };

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Tg { term },
            nodes: vec![],
        })
    }

    // Parses (`Tn`)[https://man.openbsd.org/mdoc#Tn]:
    // `Tn word ...`

    fn parse_tn(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Tn,
            nodes,
        })
    }

    // Parses (`Ud`)[https://man.openbsd.org/mdoc#Ud]:
    // `Ud`
    fn parse_ud(_pair: Pair<Rule>) -> Element {
        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ud,
            nodes: vec![],
        })
    }

    // Parses (`Ux`)[https://man.openbsd.org/mdoc#Ux]:
    // `Ux`
    fn parse_ux(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Ux,
            nodes,
        })
    }

    // Parses (`Va`)[https://man.openbsd.org/mdoc#Va]:
    // `Va [type] identifier ...`

    fn parse_va(pair: Pair<Rule>) -> Element {
        let nodes = pair.into_inner().map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Va,
            nodes,
        })
    }

    // Parses (`Xr`)[https://man.openbsd.org/mdoc#Xr]:
    // `Xr name section`
    fn parse_xr(pair: Pair<Rule>) -> Element {
        let mut inner = pair.into_inner();

        let name = inner.next().unwrap();
        let name = match name.as_rule() {
            Rule::text_arg => name.as_str().to_string(),
            _ => unreachable!(),
        };

        let section = inner.next().unwrap();
        let section = match section.as_rule() {
            Rule::text_arg => section.as_str().to_string(),
            _ => unreachable!(),
        };

        let nodes = inner.map(Self::parse_element).collect();

        Element::Macro(MacroNode {
            mdoc_macro: Macro::Xr { name, section },
            nodes,
        })
    }

    fn parse_inline(pair: Pair<Rule>) -> Element {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::rs_submacro => Self::parse_rs_submacro(pair),
            Rule::text_production => Self::parse_text_production(pair),
            Rule::ad => Self::parse_ad(pair),
            Rule::an => Self::parse_an(pair),
            Rule::ap => Self::parse_ap(pair),
            Rule::ar => Self::parse_ar(pair),
            Rule::bt => Self::parse_bt(pair),
            Rule::cd => Self::parse_cd(pair),
            Rule::cm => Self::parse_cm(pair),
            Rule::db => Self::parse_db(pair),
            Rule::dd => Self::parse_dd(pair),
            Rule::dt => Self::parse_dt(pair),
            Rule::dv => Self::parse_dv(pair),
            Rule::em => Self::parse_em(pair),
            Rule::er => Self::parse_er(pair),
            Rule::es => Self::parse_es(pair),
            Rule::ev => Self::parse_ev(pair),
            Rule::fa => Self::parse_fa(pair),
            Rule::fd => Self::parse_fd(pair),
            Rule::fl => Self::parse_fl(pair),
            Rule::Fn => Self::parse_fn(pair),
            Rule::fr => Self::parse_fr(pair),
            Rule::ft => Self::parse_ft(pair),
            Rule::hf => Self::parse_hf(pair),
            Rule::ic => Self::parse_ic(pair),
            Rule::In => Self::parse_in(pair),
            Rule::lb => Self::parse_lb(pair),
            Rule::li => Self::parse_li(pair),
            Rule::lk => Self::parse_lk(pair),
            Rule::lp => Self::parse_lp(pair),
            Rule::ms => Self::parse_ms(pair),
            Rule::mt => Self::parse_mt(pair),
            Rule::no => Self::parse_no(pair),
            Rule::ns => Self::parse_ns(pair),
            Rule::os => Self::parse_os(pair),
            Rule::ot => Self::parse_ot(pair),
            Rule::pa => Self::parse_pa(pair),
            Rule::pf => Self::parse_pf(pair),
            Rule::pp => Self::parse_pp(pair),
            Rule::sm => Self::parse_sm(pair),
            Rule::sx => Self::parse_sx(pair),
            Rule::sy => Self::parse_sy(pair),
            Rule::tg => Self::parse_tg(pair),
            Rule::tn => Self::parse_tn(pair),
            Rule::ud => Self::parse_ud(pair),
            Rule::ux => Self::parse_ux(pair),
            Rule::va => Self::parse_va(pair),
            Rule::xr => Self::parse_xr(pair),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::man_util::parser::*;

    #[test]
    fn text_line() {
        let content = "Line 1\nLine 2\nLine 3\n";
        let elements = vec![
            Element::Text("Line 1".to_string()),
            Element::Text("Line 2".to_string()),
            Element::Text("Line 3".to_string()),
        ];

        let mdoc = MdocParser::parse_mdoc(content).unwrap();
        assert_eq!(mdoc.elements, elements);
    }

    mod block_full_explicit {
        use std::collections::HashMap;

        use crate::man_util::parser::*;

        #[test]
        fn bd() {
            let content = ".Bd -literal -offset indent -compact\nLine 1\nLine 2\n.Ed";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bd {
                    block_type: BdType::Literal,
                    offset: Some(OffsetType::Indent),
                    compact: true,
                },
                nodes: vec![
                    Element::Text("Line 1".to_string()),
                    Element::Text("Line 2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bd_no_closing_macro() {
            let input = ".Bd -literal -offset indent -compact\nLine 1\nLine 2\n";
            assert_eq!(MdocParser::parse_mdoc(input).unwrap().elements, vec![]);
        }

        #[test]
        fn bd_foreign_closing_macros() {
            let closing_macros = vec![".Ef", ".Ek", ".El"];
            let content = ".Bd -literal -offset indent -compact\nLine 1\nLine 2\n";

            for closing_macro in closing_macros {
                let input = format!("{content}.{closing_macro}");
                assert_eq!(MdocParser::parse_mdoc(&input).unwrap().elements, vec![]);
            }
        }

        #[test]
        fn bd_no_body() {
            let content = ".Bd -literal\n.Ed";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bd {
                    block_type: BdType::Literal,
                    offset: None,
                    compact: false,
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bd_type() {
            let mut bd_types: HashMap<&str, BdType> = Default::default();
            bd_types.insert("-centered", BdType::Centered);
            bd_types.insert("-filled", BdType::Filled);
            bd_types.insert("-literal", BdType::Literal);
            bd_types.insert("-ragged", BdType::Ragged);
            bd_types.insert("-unfilled", BdType::Unfilled);

            for (str_type, enum_type) in bd_types {
                let content = format!(".Bd {str_type}\n.Ed");
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bd {
                        block_type: enum_type,
                        offset: None,
                        compact: false,
                    },
                    nodes: vec![],
                })];

                let mdoc = MdocParser::parse_mdoc(&content).unwrap();
                assert_eq!(mdoc.elements, elements, "Bd type: {str_type}");
            }
        }

        #[test]
        fn bd_offset() {
            let mut offset_types: HashMap<&str, OffsetType> = Default::default();
            offset_types.insert("indent", OffsetType::Indent);
            offset_types.insert("indent-two", OffsetType::IndentTwo);
            offset_types.insert("left", OffsetType::Left);
            offset_types.insert("right", OffsetType::Right);

            for (str_type, enum_type) in offset_types {
                let content = format!(".Bd -literal -offset {str_type}\n.Ed");
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bd {
                        block_type: BdType::Literal,
                        offset: Some(enum_type),
                        compact: false,
                    },
                    nodes: vec![],
                })];

                let mdoc = MdocParser::parse_mdoc(&content).unwrap();
                assert_eq!(mdoc.elements, elements, "Bd offset: {str_type}");
            }
        }

        #[test]
        fn bd_invalid_offset() {
            let input = ".Bd -literal -offset invalid_offset\n.Ed";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bd {
                    block_type: BdType::Literal,
                    offset: Some(OffsetType::Indent),
                    compact: false,
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bd_compact() {
            let content = ".Bd -literal -compact\n.Ed";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bd {
                    block_type: BdType::Literal,
                    offset: None,
                    compact: true,
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bd_not_parsed() {
            let input = ".Bd -literal -compact Ad addr1\n.Ed";
            assert_eq!(MdocParser::parse_mdoc(input).unwrap().elements, vec![]);
        }

        #[test]
        fn bd_not_callable() {
            let input = ".Ad addr1 Bd -literal\n.Ed";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Bd".to_string()),
                    Element::Text("-literal".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bf() {
            let content = ".Bf -emphasis\nLine 1\nLine 2\n.Ef";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bf(BfType::Emphasis),
                nodes: vec![
                    Element::Text("Line 1".to_string()),
                    Element::Text("Line 2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bf_no_closing_macro() {
            let input = ".Bf -emphasis\nLine 1\nLine 2\n";
            assert_eq!(MdocParser::parse_mdoc(input).unwrap().elements, vec![]);
        }

        #[test]
        fn bf_foreign_closing_macros() {
            let closing_macros = vec![".Ed", ".Ek", ".El"];
            let content = ".Bf -emphasis\nLine 1\nLine 2\n";

            for closing_macro in closing_macros {
                let input = format!("{content}.{closing_macro}");
                assert_eq!(MdocParser::parse_mdoc(&input).unwrap().elements, vec![]);
            }
        }

        #[test]
        fn bf_type() {
            let mut bf_types: HashMap<&str, BfType> = Default::default();
            bf_types.insert("-emphasis", BfType::Emphasis);
            bf_types.insert("Em", BfType::Emphasis);
            bf_types.insert("-literal", BfType::Literal);
            bf_types.insert("Li", BfType::Literal);
            bf_types.insert("-symbolic", BfType::Symbolic);
            bf_types.insert("Sy", BfType::Symbolic);

            for (str_type, enum_type) in bf_types {
                let content = format!(".Bf {str_type}\n.Ef");
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bf(enum_type),
                    nodes: vec![],
                })];

                let mdoc = MdocParser::parse_mdoc(&content).unwrap();
                assert_eq!(mdoc.elements, elements, "Bf type: {str_type}");
            }
        }

        #[test]
        fn bf_invalid_type() {
            let input = ".Bf -invalid\n.Ef";
            assert_eq!(MdocParser::parse_mdoc(input).unwrap().elements, vec![]);
        }

        #[test]
        fn bf_not_parsed() {
            let input = ".Bf Em Ad addr1\n.Ef";
            assert_eq!(MdocParser::parse_mdoc(input).unwrap().elements, vec![]);
        }

        #[test]
        fn bf_not_callable() {
            let input = ".Ad addr1 Bf Em\n.Ef";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Bf".to_string()),
                    Element::Text("Em".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bk() {
            let content = ".Bk -words\nLine 1\nLine 2\n.Ek";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bk,
                nodes: vec![
                    Element::Text("Line 1".to_string()),
                    Element::Text("Line 2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bk_no_body() {
            let content = ".Bk -words\n.Ek";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bk,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bk_no_words() {
            let input = ".Bk\n.Ek";
            assert_eq!(MdocParser::parse_mdoc(input).unwrap().elements, vec![]);
        }

        #[test]
        fn bk_not_parsed() {
            let content = ".Bk -words Ad\n.Ek";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bk,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bk_not_callable() {
            let input = ".Ad addr1 Bk -words\n.Ek";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Bk".to_string()),
                    Element::Text("-words".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bl() {
            let content = r#".Bl -bullet -width 15 -offset indent-two -compact col1 col2 col3
.It Line 1
.It Line 2
.El"#;
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bl {
                    list_type: BlType::Bullet,
                    width: Some(15),
                    offset: Some(OffsetType::IndentTwo),
                    compact: true,
                    columns: vec!["col1".to_string(), "col2".to_string(), "col3".to_string()],
                },
                nodes: vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::It {
                            head: vec![
                                Element::Text("Line".to_string()),
                                Element::Text("1".to_string()),
                            ],
                        },
                        nodes: vec![],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::It {
                            head: vec![
                                Element::Text("Line".to_string()),
                                Element::Text("2".to_string()),
                            ],
                        },
                        nodes: vec![],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bl_no_closing_macro() {
            let input = ".Bl -bullet\nLine 1\nLine 2\n";
            assert_eq!(MdocParser::parse_mdoc(input).unwrap().elements, vec![]);
        }

        #[test]
        fn bl_foreign_closing_macros() {
            let closing_macros = vec![".Ed", ".Ef", ".Ek"];
            let content = ".Bl -bullet\nLine 1\nLine 2\n";

            for closing_macro in closing_macros {
                let input = format!("{content}.{closing_macro}");
                assert_eq!(MdocParser::parse_mdoc(&input).unwrap().elements, vec![]);
            }
        }

        #[test]
        fn bl_no_body() {
            let content = ".Bl -bullet\n.El";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bl {
                    list_type: BlType::Bullet,
                    width: None,
                    offset: None,
                    compact: false,
                    columns: vec![],
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bl_types() {
            let mut macro_types: HashMap<&str, BlType> = Default::default();
            macro_types.insert("-bullet", BlType::Bullet);
            macro_types.insert("-column", BlType::Column);
            macro_types.insert("-dash", BlType::Dash);
            macro_types.insert("-hyphen", BlType::Dash);
            macro_types.insert("-diag", BlType::Diag);
            macro_types.insert("-enum", BlType::Enum);
            macro_types.insert("-hang", BlType::Hang);
            macro_types.insert("-inset", BlType::Inset);
            macro_types.insert("-item", BlType::Item);
            macro_types.insert("-ohang", BlType::Ohang);
            macro_types.insert("-tag", BlType::Tag);

            for (str_type, enum_type) in macro_types {
                let content = format!(".Bl {str_type}\n.El");
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bl {
                        list_type: enum_type,
                        width: None,
                        offset: None,
                        compact: false,
                        columns: vec![],
                    },
                    nodes: vec![],
                })];

                let mdoc = MdocParser::parse_mdoc(&content).unwrap();
                assert_eq!(mdoc.elements, elements, "Bl type: {str_type}");
            }
        }

        #[test]
        fn bl_width() {
            let mut width_types: HashMap<&str, Option<u8>> = Default::default();
            width_types.insert("15", Some(15));
            width_types.insert("300", None);
            width_types.insert("left", Some(4));

            for (str_type, width_result) in width_types {
                let content = format!(".Bl -bullet -width {str_type}\n.El");
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bl {
                        list_type: BlType::Bullet,
                        width: width_result,
                        offset: None,
                        compact: false,
                        columns: vec![],
                    },
                    nodes: vec![],
                })];

                let mdoc = MdocParser::parse_mdoc(&content).unwrap();
                assert_eq!(mdoc.elements, elements, "Bl width: {str_type}");
            }
        }

        #[test]
        fn bl_offset() {
            let mut offset_types: HashMap<&str, OffsetType> = Default::default();
            offset_types.insert("indent", OffsetType::Indent);
            offset_types.insert("indent-two", OffsetType::IndentTwo);
            offset_types.insert("left", OffsetType::Left);
            offset_types.insert("right", OffsetType::Right);

            for (str_type, enum_type) in offset_types {
                let content = format!(".Bl -bullet -offset {str_type}\n.El");
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bl {
                        list_type: BlType::Bullet,
                        width: None,
                        offset: Some(enum_type),
                        compact: false,
                        columns: vec![],
                    },
                    nodes: vec![],
                })];

                let mdoc = MdocParser::parse_mdoc(&content).unwrap();
                assert_eq!(mdoc.elements, elements, "Bl offset: {str_type}");
            }
        }

        #[test]
        fn bl_invalid_offset() {
            // Because of invalid offset, it is considered as column
            let content = ".Bl -bullet -offset invalid_offset\n.El";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bl {
                    list_type: BlType::Bullet,
                    width: None,
                    offset: Some(OffsetType::Indent),
                    compact: false,
                    columns: vec![],
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bl_compact() {
            let content = ".Bl -bullet -compact\n.El";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bl {
                    list_type: BlType::Bullet,
                    width: None,
                    offset: None,
                    compact: true,
                    columns: vec![],
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bl_columns() {
            let content = ".Bl -bullet col1 col2 col3\n.El";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bl {
                    list_type: BlType::Bullet,
                    width: None,
                    offset: None,
                    compact: false,
                    columns: vec!["col1".to_string(), "col2".to_string(), "col3".to_string()],
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        #[allow(clippy::type_complexity)]
        fn bl_parameters() {
            let mut parameters_cases: HashMap<
                &str,
                (Option<u8>, Option<OffsetType>, bool, Vec<String>),
            > = Default::default();
            parameters_cases.insert(
                "-width 15 -offset indent-two -compact col1 col2",
                (
                    Some(15),
                    Some(OffsetType::IndentTwo),
                    true,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-width 15 -compact -offset indent-two col1 col2",
                (
                    Some(15),
                    Some(OffsetType::IndentTwo),
                    true,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-offset indent-two -width 15 -compact col1 col2",
                (
                    Some(15),
                    Some(OffsetType::IndentTwo),
                    true,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-offset indent-two -compact -width 15 col1 col2",
                (
                    Some(15),
                    Some(OffsetType::IndentTwo),
                    true,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-compact -width 15 -offset indent-two col1 col2",
                (
                    Some(15),
                    Some(OffsetType::IndentTwo),
                    true,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-compact -offset indent-two -width 15 col1 col2",
                (
                    Some(15),
                    Some(OffsetType::IndentTwo),
                    true,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-width 15 -offset indent-two col1 col2",
                (
                    Some(15),
                    Some(OffsetType::IndentTwo),
                    false,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-width 15 -compact col1 col2",
                (
                    Some(15),
                    None,
                    true,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-offset indent-two -width 15 col1 col2",
                (
                    Some(15),
                    Some(OffsetType::IndentTwo),
                    false,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-offset indent-two -compact col1 col2",
                (
                    None,
                    Some(OffsetType::IndentTwo),
                    true,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-compact -offset indent-two col1 col2",
                (
                    None,
                    Some(OffsetType::IndentTwo),
                    true,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-compact -width 15 col1 col2",
                (
                    Some(15),
                    None,
                    true,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-width 15 col1 col2",
                (
                    Some(15),
                    None,
                    false,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-offset indent-two col1 col2",
                (
                    None,
                    Some(OffsetType::IndentTwo),
                    false,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert(
                "-compact col1 col2",
                (
                    None,
                    None,
                    true,
                    vec!["col1".to_string(), "col2".to_string()],
                ),
            );
            parameters_cases.insert("-width 8 -compact", (Some(8), None, true, vec![]));

            for (input, output) in parameters_cases {
                let (width, offset, compact, columns) = output;
                let content = format!(".Bl -bullet {input}\n.El");
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bl {
                        list_type: BlType::Bullet,
                        width,
                        offset,
                        compact,
                        columns,
                    },
                    nodes: vec![],
                })];

                let mdoc = MdocParser::parse_mdoc(&content).unwrap();
                assert_eq!(mdoc.elements, elements, "Bl parameters: {input}");
            }
        }

        #[test]
        #[allow(clippy::type_complexity)]
        fn bl_invalid_parameters() {
            let mut parameters_cases: HashMap<
                &str,
                (Option<u8>, Option<OffsetType>, bool, Vec<&str>),
            > = Default::default();
            parameters_cases.insert(
                "-width 15 -width 15 -offset indent",
                (
                    Some(15),
                    Some(OffsetType::Indent),
                    false,
                    "-width 15".split(" ").collect::<Vec<_>>(),
                ),
            );
            parameters_cases.insert(
                "-offset indent -offset indent -compact",
                (
                    None,
                    Some(OffsetType::Indent),
                    true,
                    "-offset indent".split(" ").collect::<Vec<_>>(),
                ),
            );
            parameters_cases.insert(
                "-width 15 word -width 15 -offset indent",
                (
                    Some(15),
                    Some(OffsetType::Indent),
                    false,
                    "word -width 15".split(" ").collect::<Vec<_>>(),
                ),
            );
            parameters_cases.insert(
                "-compact -width 15 -offset indent -width 15",
                (
                    Some(15),
                    Some(OffsetType::Indent),
                    true,
                    "-width 15".split(" ").collect::<Vec<_>>(),
                ),
            );
            parameters_cases.insert(
                "-compact -compact -width 15",
                (
                    Some(15),
                    None,
                    true,
                    "-compact".split(" ").collect::<Vec<_>>(),
                ),
            );
            parameters_cases.insert(
                "-compact word -width 15",
                (Some(15), None, true, "word".split(" ").collect::<Vec<_>>()),
            );

            for (input, output) in parameters_cases {
                let (width, offset, compact, columns) = output;
                let content = format!(".Bl -bullet {input}\n.El");
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bl {
                        list_type: BlType::Bullet,
                        width,
                        offset,
                        compact,
                        columns: columns.iter().map(|s| s.to_string()).collect::<Vec<_>>(),
                    },
                    nodes: vec![],
                })];

                let mdoc = MdocParser::parse_mdoc(&content).unwrap();
                assert_eq!(mdoc.elements, elements, "Bl parameters: {input}");
            }
        }

        #[test]
        fn bl_not_parsed() {
            // Callable macro as opaque text
            let content = ".Bl -bullet Ad\n.El";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bl {
                    list_type: BlType::Bullet,
                    width: None,
                    offset: None,
                    compact: false,
                    columns: vec!["Ad".to_string()],
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bl_not_callable() {
            let content = ".Ad addr1 Bl Em\n.El";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Bl".to_string()),
                    Element::Text("Em".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }
    }

    mod block_full_implicit {
        use crate::man_util::parser::*;

        #[test]
        fn it_first_variant() {
            let input = r#".Bl -hang
.It arg Ad addr1
Some text
.It arg1 arg2
.Ad addr
Some text
.El
"#;
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bl {
                    list_type: BlType::Hang,
                    width: None,
                    offset: None,
                    compact: false,
                    columns: vec![],
                },
                nodes: vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::It {
                            head: vec![
                                Element::Text("arg".to_string()),
                                Element::Macro(MacroNode {
                                    mdoc_macro: Macro::Ad,
                                    nodes: vec![Element::Text("addr1".to_string())],
                                }),
                            ],
                        },
                        nodes: vec![Element::Text("Some text".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::It {
                            head: vec![
                                Element::Text("arg1".to_string()),
                                Element::Text("arg2".to_string()),
                            ],
                        },
                        nodes: vec![
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Ad,
                                nodes: vec![Element::Text("addr".to_string())],
                            }),
                            Element::Text("Some text".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements)
        }

        #[test]
        fn it_second_variant() {
            let input = r#".Bl -bullet
.It
Line
.It
.Ad addr Ad addr
.El
"#;
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bl {
                    list_type: BlType::Bullet,
                    width: None,
                    offset: None,
                    compact: false,
                    columns: vec![],
                },
                nodes: vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::It { head: vec![] },
                        nodes: vec![Element::Text("Line".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::It { head: vec![] },
                        nodes: vec![
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Ad,
                                nodes: vec![Element::Text("addr".to_string())],
                            }),
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Ad,
                                nodes: vec![Element::Text("addr".to_string())],
                            }),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements)
        }

        #[test]
        fn it_column_variant() {
            let input = r#".Bl -column
.It Em Command Ta Em External Ta Ad addr
.El"#;

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bl {
                    list_type: BlType::Column,
                    width: None,
                    offset: None,
                    compact: false,
                    columns: vec![],
                },
                nodes: vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::It {
                        head: vec![
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Em,
                                nodes: vec![Element::Text("Command".to_string())],
                            }),
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Ta,
                                nodes: vec![],
                            }),
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Em,
                                nodes: vec![Element::Text("External".to_string())],
                            }),
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Ta,
                                nodes: vec![],
                            }),
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Ad,
                                nodes: vec![Element::Text("addr".to_string())],
                            }),
                        ],
                    },
                    nodes: vec![],
                })],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements)
        }

        #[test]
        fn nd() {
            let content = ".Nd short description";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Nd,
                nodes: vec![
                    Element::Text("short".to_string()),
                    Element::Text("description".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn nd_with_line_whitespaces_and_tabs() {
            let content = ".Nd short description\t    \t";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Nd,
                nodes: vec![
                    Element::Text("short".to_string()),
                    Element::Text("description".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn nd_surrounded_by_text() {
            let content = "Line 1\n.Nd short description\nLine 2\n";
            let elements = vec![
                Element::Text("Line 1".to_string()),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Nd,
                    nodes: vec![
                        Element::Text("short".to_string()),
                        Element::Text("description".to_string()),
                        Element::Text("Line 2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn nd_with_sh_closure() {
            let content = ".Nd short description\nLine 1\nLine 2\n.Sh SECTION";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Nd,
                    nodes: vec![
                        Element::Text("short".to_string()),
                        Element::Text("description".to_string()),
                        Element::Text("Line 1".to_string()),
                        Element::Text("Line 2".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Sh {
                        title: "SECTION".to_string(),
                    },
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn nd_not_parsed() {
            let content = ".Nd name Ad addr1";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Nd,
                    nodes: vec![Element::Text("name".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sh() {
            let content = ".Sh SECTION
This is the SECTION section.";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sh {
                    title: "SECTION".to_string(),
                },
                nodes: vec![Element::Text("This is the SECTION section.".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sh_with_multiple_lines() {
            let content = ".Sh SECTION\nLine 1\nLine 2\nLine 3\n";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sh {
                    title: "SECTION".to_string(),
                },
                nodes: vec![
                    Element::Text("Line 1".to_string()),
                    Element::Text("Line 2".to_string()),
                    Element::Text("Line 3".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sh_without_title() {
            assert_eq!(
                MdocParser::parse_mdoc(".Sh\nLine 1\n").unwrap().elements,
                vec![]
            );
        }

        #[test]
        fn sh_without_body() {
            let content = ".Sh SECTION";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sh {
                    title: "SECTION".to_string(),
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sh_title_line() {
            let content = ".Sh TITLE LINE\nLine 1\n";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sh {
                    title: "TITLE LINE".to_string(),
                },
                nodes: vec![Element::Text("Line 1".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sh_with_multiple_chapters() {
            let content = ".Sh SECTION 1\nLine 1\n.Sh SECTION 2\nLine 2\n";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Sh {
                        title: "SECTION 1".to_string(),
                    },
                    nodes: vec![Element::Text("Line 1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Sh {
                        title: "SECTION 2".to_string(),
                    },
                    nodes: vec![Element::Text("Line 2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sh_name_with_nd() {
            let content = ".Sh NAME\nLine 1\n.Nd short description";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sh {
                    title: "NAME".to_string(),
                },
                nodes: vec![
                    Element::Text("Line 1".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Nd,
                        nodes: vec![
                            Element::Text("short".to_string()),
                            Element::Text("description".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sh_parsed() {
            // Although this macro is parsed, it should not consist of child
            // node or it may not be linked with Sx.
            let content = ".Sh SECTION Ad addr1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sh {
                    title: "SECTION Ad addr1".to_string(),
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ss() {
            let content = ".Ss Options\nThese are the available options.";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ss {
                    title: "Options".to_string(),
                },
                nodes: vec![Element::Text(
                    "These are the available options.".to_string(),
                )],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ss_with_multiple_lines() {
            let content = ".Ss Options\nLine 1\nLine 2\nLine 3\n";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ss {
                    title: "Options".to_string(),
                },
                nodes: vec![
                    Element::Text("Line 1".to_string()),
                    Element::Text("Line 2".to_string()),
                    Element::Text("Line 3".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ss_without_title() {
            assert_eq!(
                MdocParser::parse_mdoc(".Ss\nLine 1").unwrap().elements,
                vec![]
            );
        }

        #[test]
        fn ss_without_body() {
            let content = ".Ss Options";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ss {
                    title: "Options".to_string(),
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ss_title_line() {
            let content = ".Ss TITLE LINE\nLine 1\n";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ss {
                    title: "TITLE LINE".to_string(),
                },
                nodes: vec![Element::Text("Line 1".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ss_nested_in_sh() {
            let content = ".Sh SECTION\n.Ss Subsection\nLine 1\n";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sh {
                    title: "SECTION".to_string(),
                },
                nodes: vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ss {
                        title: "Subsection".to_string(),
                    },
                    nodes: vec![Element::Text("Line 1".to_string())],
                })],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ss_with_multiple_subchapters() {
            let content = ".Ss Subchapter 1\nLine 1\n.Ss Subchapter 2\nLine 2\n";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ss {
                        title: "Subchapter 1".to_string(),
                    },
                    nodes: vec![Element::Text("Line 1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ss {
                        title: "Subchapter 2".to_string(),
                    },
                    nodes: vec![Element::Text("Line 2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ss_parsed() {
            // Although this macro is parsed, it should not consist of child
            // node or it may not be linked with Sx.
            let content = ".Ss Subchapter Ad addr1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ss {
                    title: "Subchapter Ad addr1".to_string(),
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }
    }

    mod block_partial_implicit {
        use crate::man_util::parser::*;

        #[test]
        fn aq_empty() {
            let content = ".Aq";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Aq,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn aq_text_line() {
            let content = ".Aq Line 1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Aq,
                nodes: vec![
                    Element::Text("Line".to_string()),
                    Element::Text("1".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn aq_parsed() {
            let content = ".Aq Text Ad addr1 addr2 Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Aq,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn aq_callable() {
            let content = ".Ad addr1 Aq addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Aq,
                    nodes: vec![Element::Text("addr2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bq_empty() {
            let content = ".Bq";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bq,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bq_text_line() {
            let content = ".Bq Line 1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bq,
                nodes: vec![
                    Element::Text("Line".to_string()),
                    Element::Text("1".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bq_parsed() {
            let content = ".Bq Text Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bq,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bq_callable() {
            let content = ".Ad addr1 Bq addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bq,
                    nodes: vec![Element::Text("addr2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn brq_empty() {
            let content = ".Brq";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Brq,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn brq_text_line() {
            let content = ".Brq Line 1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Brq,
                nodes: vec![
                    Element::Text("Line".to_string()),
                    Element::Text("1".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn brq_parsed() {
            let content = ".Brq Text Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Brq,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn brq_callable() {
            let content = ".Ad addr1 Brq addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Brq,
                    nodes: vec![Element::Text("addr2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn d1_empty() {
            let content = ".D1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::D1,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn d1_text_line() {
            let content = ".D1 Line 1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::D1,
                nodes: vec![
                    Element::Text("Line".to_string()),
                    Element::Text("1".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn d1_parsed() {
            let content = ".D1 Text Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::D1,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn d1_not_callable() {
            let content = ".Ad addr1 D1 addr2";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("D1".to_string()),
                    Element::Text("addr2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dl_empty() {
            let content = ".Dl";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Dl,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dl_text_line() {
            let content = ".Dl Line 1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Dl,
                nodes: vec![
                    Element::Text("Line".to_string()),
                    Element::Text("1".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dl_parsed() {
            let content = ".Dl Text Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Dl,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dl_not_callable() {
            let content = ".Ad addr1 Dl addr2";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Dl".to_string()),
                    Element::Text("addr2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dq_empty() {
            let content = ".Dq";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Dq,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dq_text_line() {
            let content = ".Dq Line 1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Dq,
                nodes: vec![
                    Element::Text("Line".to_string()),
                    Element::Text("1".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dq_parsed() {
            let content = ".Dq Text Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Dq,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dq_callable() {
            let content = ".Ad addr1 Dq addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Dq,
                    nodes: vec![Element::Text("addr2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn en() {
            let content = ".En word1 word2 word3";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::En,
                nodes: vec![
                    Element::Text("word1".to_string()),
                    Element::Text("word2".to_string()),
                    Element::Text("word3".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn en_no_words() {
            assert_eq!(MdocParser::parse_mdoc(".En").unwrap().elements, vec![]);
        }

        #[test]
        fn en_parsed() {
            let content = ".En Text Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::En,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn en_callable() {
            let content = ".Ad addr1 En addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::En,
                    nodes: vec![Element::Text("addr2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn op_empty() {
            let content = ".Op";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Op,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn op_text_line() {
            let content = ".Op Line 1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Op,
                nodes: vec![
                    Element::Text("Line".to_string()),
                    Element::Text("1".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn op_parsed() {
            let content = ".Op Text Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Op,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn op_callable() {
            let content = ".Ad addr1 Op addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Op,
                    nodes: vec![Element::Text("addr2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn pq_empty() {
            let content = ".Pq";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Pq,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn pq_text_line() {
            let content = ".Pq Line 1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Pq,
                nodes: vec![
                    Element::Text("Line".to_string()),
                    Element::Text("1".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn pq_parsed() {
            let content = ".Pq Text Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Pq,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn pq_callable() {
            let content = ".Ad addr1 Pq addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Pq,
                    nodes: vec![Element::Text("addr2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ql_empty() {
            let content = ".Ql";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ql,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ql_text_line() {
            let content = ".Ql Line 1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ql,
                nodes: vec![
                    Element::Text("Line".to_string()),
                    Element::Text("1".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ql_parsed() {
            let content = ".Ql Text Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ql,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ql_callable() {
            let content = ".Ad addr1 Ql addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ql,
                    nodes: vec![Element::Text("addr2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn qq_empty() {
            let content = ".Qq";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Qq,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn qq_text_line() {
            let content = ".Qq Line 1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Qq,
                nodes: vec![
                    Element::Text("Line".to_string()),
                    Element::Text("1".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn qq_parsed() {
            let content = ".Qq Text Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Qq,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn qq_callable() {
            let content = ".Ad addr1 Qq addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Qq,
                    nodes: vec![Element::Text("addr2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sq_empty() {
            let content = ".Sq";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sq,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sq_text_line() {
            let content = ".Sq Line 1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sq,
                nodes: vec![
                    Element::Text("Line".to_string()),
                    Element::Text("1".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sq_parsed() {
            let content = ".Sq Text Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sq,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sq_callable() {
            let content = ".Ad addr1 Sq addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Sq,
                    nodes: vec![Element::Text("addr2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn vt() {
            let content = ".Vt type some identifier";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Vt,
                nodes: vec![
                    Element::Text("type".to_string()),
                    Element::Text("some".to_string()),
                    Element::Text("identifier".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn vt_empty() {
            assert_eq!(MdocParser::parse_mdoc(".Vt").unwrap().elements, vec![]);
        }

        #[test]
        fn vt_only_type() {
            let content = ".Vt type";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Vt,
                nodes: vec![Element::Text("type".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn vt_parsed() {
            let content = ".Vt Text Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Vt,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![
                            Element::Text("addr1".to_string()),
                            Element::Text("addr2".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn vt_callable() {
            let content = ".Ad addr1 Vt addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Vt,
                    nodes: vec![Element::Text("addr2".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }
    }

    mod block_partial_explicit {
        use crate::man_util::parser::*;

        #[test]
        fn ao() {
            let input = r#".Ao
Line1
Line2
.Ac
.Ao El1 El2 El3 Ac
.Ao arg Ac
.Ao
.Dv ARG
.Ac
.Ao arg
.Dv ARG Ac
.Ao Dv ARG Ac
.Ao
Line
.Ac
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ao,
                    nodes: vec![
                        Element::Text("Line1".to_string()),
                        Element::Text("Line2".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Ac,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ao,
                    nodes: vec![
                        Element::Text("El1".to_string()),
                        Element::Text("El2".to_string()),
                        Element::Text("El3".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Ac,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ao,
                    nodes: vec![
                        Element::Text("arg".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Ac,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ao,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("ARG".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Ac,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ao,
                    nodes: vec![
                        Element::Text("arg".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("ARG".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Ac,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ao,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("ARG".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Ac,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ao,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Ac,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ao_not_args() {
            let input = r#".Ao Ac
.Ao
.Ac"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ao,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ac,
                        nodes: vec![],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ao,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ac,
                        nodes: vec![],
                    })],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }
        #[test]
        fn ao_callable() {
            let input = ".Ao Ao arg Ac\n.Ac";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ao,
                nodes: vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ao,
                        nodes: vec![
                            Element::Text("arg".to_string()),
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Ac,
                                nodes: vec![],
                            }),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ac,
                        nodes: vec![],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bo() {
            let input = r#".Bo
Line1
Line2
.Bc
.Bo El1 El2 El3 Bc
.Bo arg Bc
.Bo
.Dv ARG
.Bc
.Bo arg
.Dv ARG Bc
.Bo Dv ARG Bc
.Bo
Line
.Bc"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bo,
                    nodes: vec![
                        Element::Text("Line1".to_string()),
                        Element::Text("Line2".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Bc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bo,
                    nodes: vec![
                        Element::Text("El1".to_string()),
                        Element::Text("El2".to_string()),
                        Element::Text("El3".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Bc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bo,
                    nodes: vec![
                        Element::Text("arg".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Bc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bo,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("ARG".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Bc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bo,
                    nodes: vec![
                        Element::Text("arg".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("ARG".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Bc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bo,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("ARG".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Bc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bo,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Bc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bo_not_args() {
            let input = r#".Bo Bc
.Bo
.Bc"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bo,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bc,
                        nodes: vec![],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bo,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bc,
                        nodes: vec![],
                    })],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bo_callable() {
            let input = ".Bo Bo arg Bc\n.Bc";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bo,
                nodes: vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bo,
                        nodes: vec![
                            Element::Text("arg".to_string()),
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Bc,
                                nodes: vec![],
                            }),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bc,
                        nodes: vec![],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bro() {
            let input = r#".Bro
Line1
Line2
.Brc
.Bro El1 El2 El3 Brc
.Bro arg Brc
.Bro
.Dv ARG
.Brc
.Bro arg
.Dv ARG Brc
.Bro Dv ARG Brc
.Bro
Line
.Brc"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bro,
                    nodes: vec![
                        Element::Text("Line1".to_string()),
                        Element::Text("Line2".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Brc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bro,
                    nodes: vec![
                        Element::Text("El1".to_string()),
                        Element::Text("El2".to_string()),
                        Element::Text("El3".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Brc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bro,
                    nodes: vec![
                        Element::Text("arg".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Brc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bro,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("ARG".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Brc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bro,
                    nodes: vec![
                        Element::Text("arg".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("ARG".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Brc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bro,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("ARG".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Brc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bro,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Brc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bro_not_args() {
            let input = r#".Bro Brc
.Bro
.Brc"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bro,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Brc,
                        nodes: vec![],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bro,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Brc,
                        nodes: vec![],
                    })],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bro_callable() {
            let input = ".Bo Bro arg Brc\n.Bc";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bo,
                nodes: vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bro,
                        nodes: vec![
                            Element::Text("arg".to_string()),
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Brc,
                                nodes: vec![],
                            }),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bc,
                        nodes: vec![],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn r#do() {
            let input = r#".Do
Line1
Line2
.Dc
.Do El1 El2 El3 Dc
.Do arg Dc
.Do
.Dv ARG
.Dc
.Do arg
.Dv ARG Dc
.Do Dv ARG Dc
.Do
Line
.Dc"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Do,
                    nodes: vec![
                        Element::Text("Line1".to_string()),
                        Element::Text("Line2".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Do,
                    nodes: vec![
                        Element::Text("El1".to_string()),
                        Element::Text("El2".to_string()),
                        Element::Text("El3".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Do,
                    nodes: vec![
                        Element::Text("arg".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Do,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("ARG".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Do,
                    nodes: vec![
                        Element::Text("arg".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("ARG".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Do,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("ARG".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Do,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn do_not_args() {
            let input = r#".Do Dc
.Do
.Dc"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Do,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dc,
                        nodes: vec![],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Do,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dc,
                        nodes: vec![],
                    })],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn do_callable() {
            let input = ".Bo Do arg Dc\n.Bc";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bo,
                nodes: vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Do,
                        nodes: vec![
                            Element::Text("arg".to_string()),
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Dc,
                                nodes: vec![],
                            }),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bc,
                        nodes: vec![],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn eo() {
            let input = r#".Eo
Line
.Ec
.Eo [
Line
.Ec ]
.Eo
Line
.Ec .
.Eo [ arg1 arg2 Ec ]
.Eo arg1 arg2 Ec
.Eo arg1 arg2 Ec .
.Eo [ arg1
.Ec ]
.Eo
Line
.Ec
"#;

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: None,
                        closing_delimiter: None,
                    },
                    nodes: vec![Element::Text("Line".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: Some('['),
                        closing_delimiter: Some(']'),
                    },
                    nodes: vec![Element::Text("Line".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: None,
                        closing_delimiter: Some('.'),
                    },
                    nodes: vec![Element::Text("Line".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: Some('['),
                        closing_delimiter: Some(']'),
                    },
                    nodes: vec![
                        Element::Text("arg1".to_string()),
                        Element::Text("arg2".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: None,
                        closing_delimiter: None,
                    },
                    nodes: vec![
                        Element::Text("arg1".to_string()),
                        Element::Text("arg2".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: None,
                        closing_delimiter: Some('.'),
                    },
                    nodes: vec![
                        Element::Text("arg1".to_string()),
                        Element::Text("arg2".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: Some('['),
                        closing_delimiter: Some(']'),
                    },
                    nodes: vec![Element::Text("arg1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: None,
                        closing_delimiter: None,
                    },
                    nodes: vec![Element::Text("Line".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn eo_not_args() {
            let input = ".Eo Ec";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Eo {
                    opening_delimiter: None,
                    closing_delimiter: None,
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn eo_parsed() {
            let input = r#".Eo
.Dv ARG
.Ec
.Eo
.Dv ARG
.Ec .
.Eo [
.Dv ARG
.Ec ]
.Eo Dv ARG
.Ec
.Eo Dv ARG
.Ec .
.Eo [ Dv ARG
.Ec ]
.Eo Dv ARG Ec
.Eo Dv ARG Ec .
.Eo [ Dv ARG Ec ]
.Eo
Text
.Ec
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: None,
                        closing_delimiter: None,
                    },
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dv,
                        nodes: vec![Element::Text("ARG".to_string())],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: None,
                        closing_delimiter: Some('.'),
                    },
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dv,
                        nodes: vec![Element::Text("ARG".to_string())],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: Some('['),
                        closing_delimiter: Some(']'),
                    },
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dv,
                        nodes: vec![Element::Text("ARG".to_string())],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: None,
                        closing_delimiter: None,
                    },
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dv,
                        nodes: vec![Element::Text("ARG".to_string())],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: None,
                        closing_delimiter: Some('.'),
                    },
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dv,
                        nodes: vec![Element::Text("ARG".to_string())],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: Some('['),
                        closing_delimiter: Some(']'),
                    },
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dv,
                        nodes: vec![Element::Text("ARG".to_string())],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: None,
                        closing_delimiter: None,
                    },
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dv,
                        nodes: vec![Element::Text("ARG".to_string())],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: None,
                        closing_delimiter: Some('.'),
                    },
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dv,
                        nodes: vec![Element::Text("ARG".to_string())],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: Some('['),
                        closing_delimiter: Some(']'),
                    },
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dv,
                        nodes: vec![Element::Text("ARG".to_string())],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Eo {
                        opening_delimiter: None,
                        closing_delimiter: None,
                    },
                    nodes: vec![Element::Text("Text".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn eo_callable() {
            let input = ".Bo Eo [ arg Ec ]\n.Bc";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bo,
                nodes: vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Eo {
                            opening_delimiter: Some('['),
                            closing_delimiter: Some(']'),
                        },
                        nodes: vec![Element::Text("arg".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bc,
                        nodes: vec![],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fo() {
            let input = r#".Fo funcname
Line
.Fc
.Fo funcname Fc
.Fo funcname arg1
arg2 Fc
.Fc
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fo {
                        funcname: "funcname".to_string(),
                    },
                    nodes: vec![Element::Text("Line".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fo {
                        funcname: "funcname".to_string(),
                    },
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fo {
                        funcname: "funcname".to_string(),
                    },
                    nodes: vec![
                        Element::Text("arg1".to_string()),
                        Element::Text("arg2 Fc".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fo_not_args() {
            assert_eq!(MdocParser::parse_mdoc(".Fo.Fc").unwrap().elements, vec![]);
        }

        #[test]
        fn fo_not_parsed() {
            let input = r#".Fo funcname Dv ARG
.Fc            
"#;
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Fo {
                    funcname: "funcname".to_string(),
                },
                nodes: vec![Element::Text("Dv ARG".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // .Oo -----------------------------------------------------------

        #[test]
        fn oo() {
            let input = r#".Oo
Line1
Line2
.Oc
.Oo El1 El2 El3 Oc
.Oo
Line
.Oc
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Oo,
                    nodes: vec![
                        Element::Text("Line1".to_string()),
                        Element::Text("Line2".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Oc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Oo,
                    nodes: vec![
                        Element::Text("El1".to_string()),
                        Element::Text("El2".to_string()),
                        Element::Text("El3".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Oc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Oo,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Oc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn oo_no_args() {
            let input = r#".Oo Oc
.Oo
.Oc"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Oo,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Oc,
                        nodes: vec![],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Oo,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Oc,
                        nodes: vec![],
                    })],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn oo_parsed() {
            let input = r#".Oo
.Ad addr
.Oc
.Oo Dv CONSTANT
.Oc
.Oo
Line
.Oc
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Oo,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Ad,
                            nodes: vec![Element::Text("addr".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Oc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Oo,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("CONSTANT".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Oc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Oo,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Oc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn oo_called() {
            let input = r#".Ao
.Oo
.Ad addr
.Oc
.Oo Dv CONSTANT
.Oc
.Oo
Line
.Oc
.Ac
"#;
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ao,
                nodes: vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Oo,
                        nodes: vec![
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Ad,
                                nodes: vec![Element::Text("addr".to_string())],
                            }),
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Oc,
                                nodes: vec![],
                            }),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Oo,
                        nodes: vec![
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Dv,
                                nodes: vec![Element::Text("CONSTANT".to_string())],
                            }),
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Oc,
                                nodes: vec![],
                            }),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Oo,
                        nodes: vec![
                            Element::Text("Line".to_string()),
                            Element::Macro(MacroNode {
                                mdoc_macro: Macro::Oc,
                                nodes: vec![],
                            }),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ac,
                        nodes: vec![],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // .Po -----------------------------------------------------------

        #[test]
        fn po() {
            let input = r#".Po
Line1
Line2
.Pc
.Po El1 El2 El3 Pc
.Po
Line
.Pc
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Po,
                    nodes: vec![
                        Element::Text("Line1".to_string()),
                        Element::Text("Line2".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Pc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Po,
                    nodes: vec![
                        Element::Text("El1".to_string()),
                        Element::Text("El2".to_string()),
                        Element::Text("El3".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Pc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Po,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Pc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn po_no_args() {
            let input = r#".Po Pc
.Po
.Pc"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Po,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Pc,
                        nodes: vec![],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Po,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Pc,
                        nodes: vec![],
                    })],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn po_parsed() {
            let input = r#".Po
.Ad addr
.Pc
.Po Dv CONSTANT
.Pc
.Po
Line
.Pc
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Po,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Ad,
                            nodes: vec![Element::Text("addr".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Pc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Po,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("CONSTANT".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Pc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Po,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Pc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // .Qo -----------------------------------------------------------

        #[test]
        fn qo() {
            let input = r#".Qo
Line1
Line2
.Qc
.Qo El1 El2 El3 Qc
.Qo
Line
.Qc
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Qo,
                    nodes: vec![
                        Element::Text("Line1".to_string()),
                        Element::Text("Line2".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Qc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Qo,
                    nodes: vec![
                        Element::Text("El1".to_string()),
                        Element::Text("El2".to_string()),
                        Element::Text("El3".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Qc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Qo,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Qc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn qo_no_args() {
            let input = r#".Qo Qc
.Qo
.Qc"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Qo,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Qc,
                        nodes: vec![],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Qo,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Qc,
                        nodes: vec![],
                    })],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn qo_parsed() {
            let input = r#".Qo
.Ad addr
.Qc
.Qo Dv CONSTANT
.Qc
.Qo
Line
.Qc
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Qo,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Ad,
                            nodes: vec![Element::Text("addr".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Qc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Qo,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("CONSTANT".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Qc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Qo,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Qc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // .Rs -----------------------------------------------------------

        #[test]
        fn rs() {
            let input = r#".Rs
.%A John Doe
.%B Title Line Ad addr1
.%D January 1, 1970
.%U protocol://path
.Re
.Rs %A John Doe %B Title Line Ad addr1 %D January 1, 1970 %U protocol://path 
.Re
.Rs %A John Doe 
.%B Title Line Ad addr1
.%D January 1, 1970
.%U protocol://path
.Re"#;

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Rs,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::A,
                            nodes: vec![
                                Element::Text("John".to_string()),
                                Element::Text("Doe".to_string()),
                            ],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::B,
                            nodes: vec![
                                Element::Text("Title".to_string()),
                                Element::Text("Line".to_string()),
                                Element::Text("Ad".to_string()),
                                Element::Text("addr1".to_string()),
                            ],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::U,
                            nodes: vec![Element::Text("protocol://path".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::D,
                            nodes: vec![
                                Element::Text("January".to_string()),
                                Element::Text("1,".to_string()),
                                Element::Text("1970".to_string()),
                            ],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Rs,
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Rs,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::B,
                            nodes: vec![
                                Element::Text("Title".to_string()),
                                Element::Text("Line".to_string()),
                                Element::Text("Ad".to_string()),
                                Element::Text("addr1".to_string()),
                            ],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::U,
                            nodes: vec![Element::Text("protocol://path".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::D,
                            nodes: vec![
                                Element::Text("January".to_string()),
                                Element::Text("1,".to_string()),
                                Element::Text("1970".to_string()),
                            ],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]

        fn rs_wrong_args() {
            assert_eq!(
                MdocParser::parse_mdoc(
                    r#".Rs
Line1
Line2
.Re
"#
                )
                .unwrap()
                .elements,
                vec![]
            );
            assert_eq!(
                MdocParser::parse_mdoc(
                    r#".Rs El1 El2 El3 
Re"#
                )
                .unwrap()
                .elements,
                vec![]
            );
            assert_eq!(
                MdocParser::parse_mdoc(
                    r#".Rs
arg Re"#
                )
                .unwrap()
                .elements,
                vec![]
            );
            assert_eq!(
                MdocParser::parse_mdoc(
                    r#".Rs
Line
.Re
"#
                )
                .unwrap()
                .elements,
                vec![]
            );
        }

        #[test]
        fn rs_no_args() {
            assert_eq!(
                MdocParser::parse_mdoc(
                    r#".Rs
            .Re"#
                )
                .unwrap()
                .elements,
                vec![]
            );
        }

        #[test]
        fn rs_not_parsed() {
            assert_eq!(
                MdocParser::parse_mdoc(
                    r#".Rs
.%A John Doe
.%B Title Line Ad addr1
.%D January 1, 1970
.%U protocol://path
.Ad addr
.Re"#
                )
                .unwrap()
                .elements,
                vec![]
            );
            assert_eq!(
                MdocParser::parse_mdoc(
                    r#".Rs %A John Doe 
.%B Title Line Ad addr1
.%D January 1, 1970
.%U protocol://path
.Ad addr
.Re"#
                )
                .unwrap()
                .elements,
                vec![]
            );
        }

        #[test]
        fn rs_submacro_sorting() {
            let input = r#".Rs
.%O Optional information
.%D January 1, 1970
.%C Location line
.%Q John Doe
.%P pp. 1-100
.%U protocol://path
.%V Volume No. 1
.%N Issue No. 1
.%R Technical report No. 1
.%J Journal Name Line
.%I John Doe
.%B Title Line
.%T Article title line
.%A John Doe
.Re"#;

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Rs,
                nodes: vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::A,
                        nodes: vec![
                            Element::Text("John".to_string()),
                            Element::Text("Doe".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::T,
                        nodes: vec![
                            Element::Text("Article".to_string()),
                            Element::Text("title".to_string()),
                            Element::Text("line".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::B,
                        nodes: vec![
                            Element::Text("Title".to_string()),
                            Element::Text("Line".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::I,
                        nodes: vec![
                            Element::Text("John".to_string()),
                            Element::Text("Doe".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::J,
                        nodes: vec![
                            Element::Text("Journal".to_string()),
                            Element::Text("Name".to_string()),
                            Element::Text("Line".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::R,
                        nodes: vec![
                            Element::Text("Technical".to_string()),
                            Element::Text("report".to_string()),
                            Element::Text("No.".to_string()),
                            Element::Text("1".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::N,
                        nodes: vec![
                            Element::Text("Issue".to_string()),
                            Element::Text("No.".to_string()),
                            Element::Text("1".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::V,
                        nodes: vec![
                            Element::Text("Volume".to_string()),
                            Element::Text("No.".to_string()),
                            Element::Text("1".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::U,
                        nodes: vec![Element::Text("protocol://path".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::P,
                        nodes: vec![
                            Element::Text("pp.".to_string()),
                            Element::Text("1-100".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Q,
                        nodes: vec![
                            Element::Text("John".to_string()),
                            Element::Text("Doe".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::C,
                        nodes: vec![
                            Element::Text("Location".to_string()),
                            Element::Text("line".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::D,
                        nodes: vec![
                            Element::Text("January".to_string()),
                            Element::Text("1,".to_string()),
                            Element::Text("1970".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::O,
                        nodes: vec![
                            Element::Text("Optional".to_string()),
                            Element::Text("information".to_string()),
                        ],
                    }),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // .So -----------------------------------------------------------

        #[test]
        fn so() {
            let input = r#".So
Line1
Line2
.Sc
.So El1 El2 El3 Sc
.So
Line
.Sc
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::So,
                    nodes: vec![
                        Element::Text("Line1".to_string()),
                        Element::Text("Line2".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Sc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::So,
                    nodes: vec![
                        Element::Text("El1".to_string()),
                        Element::Text("El2".to_string()),
                        Element::Text("El3".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Sc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::So,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Sc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn so_no_args() {
            let input = r#".So Sc
.So
.Sc"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::So,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Sc,
                        nodes: vec![],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::So,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Sc,
                        nodes: vec![],
                    })],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn so_parsed() {
            let input = r#".So
.Ad addr
.Sc
.So Dv CONSTANT
.Sc
.So
Line
.Sc
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::So,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Ad,
                            nodes: vec![Element::Text("addr".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Sc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::So,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("CONSTANT".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Sc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::So,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Sc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // .Xo -----------------------------------------------------------

        #[test]
        fn xo() {
            let input = r#".Xo
Line1
Line2
.Xc
.Xo El1 El2 El3 Xc
.Xo
Line
.Xc
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Xo,
                    nodes: vec![
                        Element::Text("Line1".to_string()),
                        Element::Text("Line2".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Xc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Xo,
                    nodes: vec![
                        Element::Text("El1".to_string()),
                        Element::Text("El2".to_string()),
                        Element::Text("El3".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Xc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Xo,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Xc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn xo_no_args() {
            let input = r#".Xo Xc
.Xo
.Xc"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Xo,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Xc,
                        nodes: vec![],
                    })],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Xo,
                    nodes: vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Xc,
                        nodes: vec![],
                    })],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn xo_parsed() {
            let input = r#".Xo
.Ad addr
.Xc
.Xo Dv CONSTANT
.Xc
.Xo
Line
.Xc
"#;
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Xo,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Ad,
                            nodes: vec![Element::Text("addr".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Xc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Xo,
                    nodes: vec![
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Dv,
                            nodes: vec![Element::Text("CONSTANT".to_string())],
                        }),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Xc,
                            nodes: vec![],
                        }),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Xo,
                    nodes: vec![
                        Element::Text("Line".to_string()),
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::Xc,
                            nodes: vec![],
                        }),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }
    }

    mod inline {
        use crate::man_util::parser::*;

        mod rs_submacros {
            use crate::man_util::parser::*;

            #[test]
            fn a() {
                let content = ".%A John Doe";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::A,
                    nodes: vec![
                        Element::Text("John".to_string()),
                        Element::Text("Doe".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn a_with_whitespaces() {
                let content = ".%A John  \t  Doe";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::A,
                    nodes: vec![
                        Element::Text("John".to_string()),
                        Element::Text("Doe".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn a_no_args() {
                let content = ".%A";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn a_not_parsed() {
                let content = ".%A John Doe Ad addr1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::A,
                    nodes: vec![
                        Element::Text("John".to_string()),
                        Element::Text("Doe".to_string()),
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn a_not_callable() {
                let content = ".Ad addr1 %A John Doe";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%A".to_string()),
                        Element::Text("John".to_string()),
                        Element::Text("Doe".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn b() {
                let content = ".%B Title Line";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::B,
                    nodes: vec![
                        Element::Text("Title".to_string()),
                        Element::Text("Line".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn b_with_whitespaces() {
                let content = ".%B Title  \t  Line\n";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::B,
                    nodes: vec![
                        Element::Text("Title".to_string()),
                        Element::Text("Line".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn b_no_args() {
                let content = ".%B";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn b_not_parsed() {
                let content = ".%B Title Line Ad addr1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::B,
                    nodes: vec![
                        Element::Text("Title".to_string()),
                        Element::Text("Line".to_string()),
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn b_not_callable() {
                let content = ".Ad addr1 %B Title Line";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%B".to_string()),
                        Element::Text("Title".to_string()),
                        Element::Text("Line".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn c() {
                let content = ".%C Location line";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::C,
                    nodes: vec![
                        Element::Text("Location".to_string()),
                        Element::Text("line".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn c_with_whitespaces() {
                let content = ".%C Location  \t  Line\n";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::C,
                    nodes: vec![
                        Element::Text("Location".to_string()),
                        Element::Text("Line".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn c_no_args() {
                let content = ".%C";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn c_not_parsed() {
                let content = ".%C Location Line Ad addr1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::C,
                    nodes: vec![
                        Element::Text("Location".to_string()),
                        Element::Text("Line".to_string()),
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn c_not_callable() {
                let content = ".Ad addr1 %C Location Line";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%C".to_string()),
                        Element::Text("Location".to_string()),
                        Element::Text("Line".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn d() {
                let content = ".%D January 1, 1970";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::D,
                    nodes: vec![
                        Element::Text("January".to_string()),
                        Element::Text("1,".to_string()),
                        Element::Text("1970".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn d_with_whitespaces() {
                let content = ".%D January  \t  1,  \t  1970\n";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::D,
                    nodes: vec![
                        Element::Text("January".to_string()),
                        Element::Text("1,".to_string()),
                        Element::Text("1970".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn d_no_month_day() {
                let content = ".%D 1970";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::D,
                    nodes: vec![Element::Text("1970".to_string())],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn d_no_args() {
                let content = ".%D";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn d_not_parsed() {
                let input = ".%D Ad 1, 1970";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::D,
                    nodes: vec![
                        Element::Text("Ad".to_string()),
                        Element::Text("1,".to_string()),
                        Element::Text("1970".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(input).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn d_not_callable() {
                let content = ".Ad addr1 %D January 1, 1970";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%D".to_string()),
                        Element::Text("January".to_string()),
                        Element::Text("1,".to_string()),
                        Element::Text("1970".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn i() {
                let content = ".%I John Doe";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::I,
                    nodes: vec![
                        Element::Text("John".to_string()),
                        Element::Text("Doe".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn i_with_whitespaces() {
                let content = ".%I John  \t  Doe\n";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::I,
                    nodes: vec![
                        Element::Text("John".to_string()),
                        Element::Text("Doe".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn i_no_args() {
                let content = ".%I";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn i_not_parsed() {
                let content = ".%I John Doe Ad addr1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::I,
                    nodes: vec![
                        Element::Text("John".to_string()),
                        Element::Text("Doe".to_string()),
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn i_not_callable() {
                let content = ".Ad addr1 %I John Doe";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%I".to_string()),
                        Element::Text("John".to_string()),
                        Element::Text("Doe".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn j() {
                let content = ".%J Journal Name Line";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::J,
                    nodes: vec![
                        Element::Text("Journal".to_string()),
                        Element::Text("Name".to_string()),
                        Element::Text("Line".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn j_with_whitespaces() {
                let content = ".%J Journal  \t  Name  \t  Line\n";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::J,
                    nodes: vec![
                        Element::Text("Journal".to_string()),
                        Element::Text("Name".to_string()),
                        Element::Text("Line".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn j_no_args() {
                let content = ".%J";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn j_not_parsed() {
                let content = ".%J Journal Name Ad addr1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::J,
                    nodes: vec![
                        Element::Text("Journal".to_string()),
                        Element::Text("Name".to_string()),
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn j_not_callable() {
                let content = ".Ad addr1 %J Journal Name";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%J".to_string()),
                        Element::Text("Journal".to_string()),
                        Element::Text("Name".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn n() {
                let content = ".%N Issue No. 1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::N,
                    nodes: vec![
                        Element::Text("Issue".to_string()),
                        Element::Text("No.".to_string()),
                        Element::Text("1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn n_with_whitespaces() {
                let content = ".%N Issue  \t  No.  \t  1\n";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::N,
                    nodes: vec![
                        Element::Text("Issue".to_string()),
                        Element::Text("No.".to_string()),
                        Element::Text("1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn n_no_args() {
                let content = ".%N";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn n_not_parsed() {
                let content = ".%N Issue No. 1 Ad addr1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::N,
                    nodes: vec![
                        Element::Text("Issue".to_string()),
                        Element::Text("No.".to_string()),
                        Element::Text("1".to_string()),
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn n_not_callable() {
                let content = ".Ad addr1 %N Issue No. 1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%N".to_string()),
                        Element::Text("Issue".to_string()),
                        Element::Text("No.".to_string()),
                        Element::Text("1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn o() {
                let content = ".%O Optional information line";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::O,
                    nodes: vec![
                        Element::Text("Optional".to_string()),
                        Element::Text("information".to_string()),
                        Element::Text("line".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn o_with_whitespaces() {
                let content = ".%O Optional  \t  information  \t  line\n";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::O,
                    nodes: vec![
                        Element::Text("Optional".to_string()),
                        Element::Text("information".to_string()),
                        Element::Text("line".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn o_no_args() {
                let content = ".%O";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn o_not_parsed() {
                let content = ".%O Optional information Ad addr1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::O,
                    nodes: vec![
                        Element::Text("Optional".to_string()),
                        Element::Text("information".to_string()),
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn o_not_callable() {
                let content = ".Ad addr1 %O Optional information";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%O".to_string()),
                        Element::Text("Optional".to_string()),
                        Element::Text("information".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn p() {
                let content = ".%P pp. 1-100";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::P,
                    nodes: vec![
                        Element::Text("pp.".to_string()),
                        Element::Text("1-100".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn p_with_whitespaces() {
                let content = ".%P pp.  \t  1-100\n";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::P,
                    nodes: vec![
                        Element::Text("pp.".to_string()),
                        Element::Text("1-100".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn p_no_args() {
                let content = ".%P";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn p_not_parsed() {
                let content = ".%P pp. 1-100 Ad addr1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::P,
                    nodes: vec![
                        Element::Text("pp.".to_string()),
                        Element::Text("1-100".to_string()),
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn p_not_callable() {
                let content = ".Ad addr1 %P pp. 1-100";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%P".to_string()),
                        Element::Text("pp.".to_string()),
                        Element::Text("1-100".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn q() {
                let content = ".%Q John Doe";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Q,
                    nodes: vec![
                        Element::Text("John".to_string()),
                        Element::Text("Doe".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn q_with_whitespaces() {
                let content = ".%Q John  \t  Doe\n";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Q,
                    nodes: vec![
                        Element::Text("John".to_string()),
                        Element::Text("Doe".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn q_no_args() {
                let content = ".%Q";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn q_not_parsed() {
                let content = ".%Q John Doe Ad addr1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Q,
                    nodes: vec![
                        Element::Text("John".to_string()),
                        Element::Text("Doe".to_string()),
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn q_not_callable() {
                let content = ".Ad addr1 %Q John Doe";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%Q".to_string()),
                        Element::Text("John".to_string()),
                        Element::Text("Doe".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn r() {
                let content = ".%R Technical report No. 1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::R,
                    nodes: vec![
                        Element::Text("Technical".to_string()),
                        Element::Text("report".to_string()),
                        Element::Text("No.".to_string()),
                        Element::Text("1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn r_with_whitespaces() {
                let content = ".%R Technical  \t  report  \t  No.  \t  1\n";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::R,
                    nodes: vec![
                        Element::Text("Technical".to_string()),
                        Element::Text("report".to_string()),
                        Element::Text("No.".to_string()),
                        Element::Text("1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn r_no_args() {
                let content = ".%R";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn r_not_parsed() {
                let content = ".%R Technical report Ad addr1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::R,
                    nodes: vec![
                        Element::Text("Technical".to_string()),
                        Element::Text("report".to_string()),
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn r_not_callable() {
                let content = ".Ad addr1 %R Technical report";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%R".to_string()),
                        Element::Text("Technical".to_string()),
                        Element::Text("report".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn t() {
                let content = ".%T Article title line";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::T,
                    nodes: vec![
                        Element::Text("Article".to_string()),
                        Element::Text("title".to_string()),
                        Element::Text("line".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn t_with_whitespaces() {
                let content = ".%T Article  \t  title  \t  line\n";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::T,
                    nodes: vec![
                        Element::Text("Article".to_string()),
                        Element::Text("title".to_string()),
                        Element::Text("line".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn t_no_args() {
                let content = ".%T";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn t_not_parsed() {
                let content = ".%T Article title Ad addr1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::T,
                    nodes: vec![
                        Element::Text("Article".to_string()),
                        Element::Text("title".to_string()),
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn t_not_callable() {
                let content = ".Ad addr1 %T Article title";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%T".to_string()),
                        Element::Text("Article".to_string()),
                        Element::Text("title".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn u() {
                let content = ".%U protocol://path";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::U,
                    nodes: vec![Element::Text("protocol://path".to_string())],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn u_no_args() {
                let content = ".%U";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn u_not_parsed() {
                let content = ".%U Ad addr1";

                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::U,
                    nodes: vec![
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn u_not_callable() {
                let content = ".Ad addr1 %U protocol://path";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%U".to_string()),
                        Element::Text("protocol://path".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn v() {
                let content = ".%V Volume No. 1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::V,
                    nodes: vec![
                        Element::Text("Volume".to_string()),
                        Element::Text("No.".to_string()),
                        Element::Text("1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn v_with_whitespaces() {
                let content = ".%V Volume  \t  No.  \t  1\n";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::V,
                    nodes: vec![
                        Element::Text("Volume".to_string()),
                        Element::Text("No.".to_string()),
                        Element::Text("1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn v_no_args() {
                let content = ".%V";
                let elements = vec![];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn v_not_parsed() {
                let content = ".%V Volume No. 1 Ad addr1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::V,
                    nodes: vec![
                        Element::Text("Volume".to_string()),
                        Element::Text("No.".to_string()),
                        Element::Text("1".to_string()),
                        Element::Text("Ad".to_string()),
                        Element::Text("addr1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn v_not_callable() {
                let content = ".Ad addr1 %V Volume No. 1";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("%V".to_string()),
                        Element::Text("Volume".to_string()),
                        Element::Text("No.".to_string()),
                        Element::Text("1".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }
        }

        mod text_production {
            use std::collections::HashMap;

            use crate::man_util::parser::*;

            #[test]
            fn at() {
                let mut at_types: HashMap<&str, AtType> = Default::default();
                at_types.insert("", AtType::General);
                at_types.insert("v1", AtType::Version("1".to_string()));
                at_types.insert("v2", AtType::Version("2".to_string()));
                at_types.insert("v3", AtType::Version("3".to_string()));
                at_types.insert("v4", AtType::Version("4".to_string()));
                at_types.insert("v5", AtType::Version("5".to_string()));
                at_types.insert("v6", AtType::Version("6".to_string()));
                at_types.insert("v7", AtType::Version("7".to_string()));
                at_types.insert("32v", AtType::V32);
                at_types.insert("III", AtType::SystemIII);
                at_types.insert("V", AtType::SystemV(None));
                at_types.insert("V.1", AtType::SystemV(Some("1".to_string())));
                at_types.insert("V.2", AtType::SystemV(Some("2".to_string())));
                at_types.insert("V.3", AtType::SystemV(Some("3".to_string())));
                at_types.insert("V.4", AtType::SystemV(Some("4".to_string())));

                for (str_type, enum_type) in at_types {
                    let content = format!(".At {str_type}");
                    let elements = vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::At,
                        nodes: vec![Element::Text(enum_type.to_string())],
                    })];

                    let mdoc = MdocParser::parse_mdoc(&content).unwrap();
                    assert_eq!(mdoc.elements, elements, "At type: {str_type}");
                }
            }

            #[test]
            fn at_other_text_values() {
                let at_args = vec![
                    "v0".to_string(),
                    "v8".to_string(),
                    "V.0".to_string(),
                    "V.5".to_string(),
                    "word".to_string(),
                ];

                for arg in at_args {
                    let content = format!(".At {arg} word\n");
                    let elements = vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::At,
                        nodes: vec![
                            Element::Text(AtType::General.to_string()),
                            Element::Text(arg.clone()),
                            Element::Text("word".to_string()),
                        ],
                    })];

                    let mdoc = MdocParser::parse_mdoc(&content).unwrap();
                    assert_eq!(mdoc.elements, elements, "At type: {arg}");
                }
            }

            #[test]
            fn at_parsed() {
                let content = ".At v1 Ad addr1";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::At,
                        nodes: vec![Element::Text(AtType::Version("1".to_string()).to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn at_callable() {
                let content = ".Ad addr1 At v1 word\n";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::At,
                        nodes: vec![
                            Element::Text(AtType::Version(1.to_string()).to_string()),
                            Element::Text("word".to_string()),
                        ],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn at_with_delimiters() {
                let input = r#".At ( v1 )
.At ( v2
.At v3 )
.At , v1
"#;
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::At,
                        nodes: vec![
                            Element::Text("(".to_string()),
                            Element::Text(AtType::Version("1".to_string()).to_string()),
                            Element::Text(")".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::At,
                        nodes: vec![
                            Element::Text("(".to_string()),
                            Element::Text(AtType::Version("2".to_string()).to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::At,
                        nodes: vec![
                            Element::Text(AtType::Version("3".to_string()).to_string()),
                            Element::Text(")".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::At,
                        nodes: vec![
                            Element::Text(AtType::default().to_string()),
                            Element::Text(",".to_string()),
                            Element::Text("v1".to_string()),
                        ],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(input).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn bsx() {
                let content = ".Bsx 1.0";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bsx,
                    nodes: vec![Element::Text(BsxType::format("1.0"))],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn bsx_no_args() {
                let content = ".Bsx";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bsx,
                    nodes: vec![Element::Text(BsxType::format_default())],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn bsx_parsed() {
                let content = ".Bsx 1.0 Ad addr1";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bsx,
                        nodes: vec![Element::Text(BsxType::format("1.0"))],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn bsx_callable() {
                let content = ".Ad addr1 Bsx 1.0";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bsx,
                        nodes: vec![Element::Text(BsxType::format("1.0"))],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn bsx_with_delimiters() {
                let input = r#".Bsx ( v1 )
.Bsx ( v2
.Bsx v3 )
.Bsx , v1
"#;
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bsx,
                        nodes: vec![
                            Element::Text("(".to_string()),
                            Element::Text(BsxType::format("v1")),
                            Element::Text(")".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bsx,
                        nodes: vec![
                            Element::Text("(".to_string()),
                            Element::Text(BsxType::format("v2")),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bsx,
                        nodes: vec![
                            Element::Text(BsxType::format("v3")),
                            Element::Text(")".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bsx,
                        nodes: vec![
                            Element::Text(BsxType::format_default()),
                            Element::Text(",".to_string()),
                            Element::Text("v1".to_string()),
                        ],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(input).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn bx() {
                let mut bx_args: HashMap<&str, (&str, Option<&str>)> = Default::default();
                bx_args.insert("", ("", None));
                bx_args.insert("4.3", ("4.3", None));
                bx_args.insert("4.3 Tahoe", ("4.3", Some("Tahoe")));

                for (args, (version, variant)) in bx_args {
                    let content = format!(".Bx {args}");
                    let elements = vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bx,
                        nodes: vec![Element::Text(BxType::format(version, variant))],
                    })];

                    let mdoc = MdocParser::parse_mdoc(&content).unwrap();
                    assert_eq!(mdoc.elements, elements, "Bx args: {args}");
                }
            }

            #[test]
            fn bx_parsed() {
                let content = ".Bx 4.3 Tahoe Ad addr1";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bx,
                        nodes: vec![Element::Text(BxType::format("4.3", Some("Tahoe")))],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn bx_callable_with_arg() {
                let content = ".Ad addr1 Bx 4.3 Tahoe Example\n";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bx,
                        nodes: vec![
                            Element::Text(BxType::format("4.3", Some("Tahoe"))),
                            Element::Text("Example".to_string()),
                        ],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn bx_callable_without_arg() {
                let content = ".Ad addr1 Bx";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bx,
                        nodes: vec![Element::Text(BxType::format_default())],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn bx_with_delimiters() {
                let input = r#".Bx ( v1 )
.Bx ( v2
.Bx v3 )
.Bx , v1
"#;
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bx,
                        nodes: vec![
                            Element::Text("(".to_string()),
                            Element::Text(BxType::format("v1", None)),
                            Element::Text(")".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bx,
                        nodes: vec![
                            Element::Text("(".to_string()),
                            Element::Text(BxType::format("v2", None)),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bx,
                        nodes: vec![
                            Element::Text(BxType::format("v3", None)),
                            Element::Text(")".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Bx,
                        nodes: vec![
                            Element::Text(BxType::format_default()),
                            Element::Text(",".to_string()),
                            Element::Text("v1".to_string()),
                        ],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(input).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn dx() {
                let content = ".Dx 1.0";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Dx,
                    nodes: vec![Element::Text(DxType::format("1.0"))],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn dx_no_args() {
                let content = ".Dx";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Dx,
                    nodes: vec![Element::Text(DxType::format_default())],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn dx_parsed() {
                let content = ".Dx 1.0 Ad addr1";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dx,
                        nodes: vec![Element::Text(DxType::format("1.0"))],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn dx_callable_with_arg() {
                let content = ".Ad addr1 Dx 1.0 text";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dx,
                        nodes: vec![
                            Element::Text(DxType::format("1.0")),
                            Element::Text("text".to_string()),
                        ],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn dx_callable_without_arg() {
                let content = ".Ad addr1 Dx";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dx,
                        nodes: vec![Element::Text(DxType::format_default())],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn dx_with_delimiters() {
                let input = r#".Dx ( v1 )
.Dx ( v2
.Dx v3 )
.Dx , v1
"#;
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dx,
                        nodes: vec![
                            Element::Text("(".to_string()),
                            Element::Text(DxType::format("v1")),
                            Element::Text(")".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dx,
                        nodes: vec![
                            Element::Text("(".to_string()),
                            Element::Text(DxType::format("v2")),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dx,
                        nodes: vec![
                            Element::Text(DxType::format("v3")),
                            Element::Text(")".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Dx,
                        nodes: vec![
                            Element::Text(DxType::format_default()),
                            Element::Text(",".to_string()),
                            Element::Text("v1".to_string()),
                        ],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(input).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn nx() {
                let content = ".Nx 1.0";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Nx,
                    nodes: vec![Element::Text(NxType::format("1.0"))],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn nx_no_args() {
                let content = ".Nx";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Nx,
                    nodes: vec![Element::Text(NxType::format_default())],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn nx_parsed() {
                let content = ".Nx 1.0 Ad addr1";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Nx,
                        nodes: vec![Element::Text(NxType::format("1.0"))],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn nx_callable_with_arg() {
                let content = ".Ad addr1 Nx 1.0";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Nx,
                        nodes: vec![Element::Text(NxType::format("1.0"))],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn nx_callable_without_arg() {
                let content = ".Ad addr1 Nx";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Nx,
                        nodes: vec![Element::Text(NxType::format_default())],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn nx_with_delimiters() {
                let input = r#".Nx ( v1 )
.Nx ( v2
.Nx v3 )
.Nx , v1
"#;
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Nx,
                        nodes: vec![
                            Element::Text("(".to_string()),
                            Element::Text(NxType::format("v1")),
                            Element::Text(")".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Nx,
                        nodes: vec![
                            Element::Text("(".to_string()),
                            Element::Text(NxType::format("v2")),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Nx,
                        nodes: vec![
                            Element::Text(NxType::format("v3")),
                            Element::Text(")".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Nx,
                        nodes: vec![
                            Element::Text(NxType::format_default()),
                            Element::Text(",".to_string()),
                            Element::Text("v1".to_string()),
                        ],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(input).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn ox() {
                let content = ".Ox 1.0";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ox,
                    nodes: vec![Element::Text(OxType::format("1.0"))],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn ox_no_args() {
                let content = ".Ox";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ox,
                    nodes: vec![Element::Text(OxType::format_default())],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn ox_parsed() {
                let content = ".Ox 1.0 Ad addr1";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ox,
                        nodes: vec![Element::Text(OxType::format("1.0"))],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn ox_callable_with_arg() {
                let content = ".Ad addr1 Ox 1.0";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ox,
                        nodes: vec![Element::Text(OxType::format("1.0"))],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn ox_callable_without_arg() {
                let content = ".Ad addr1 Ox";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ox,
                        nodes: vec![Element::Text(OxType::format_default())],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn ox_with_delimiters() {
                let input = r#".Ox ( v1 )
.Ox ( v2
.Ox v3 )
.Ox , v1
"#;
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ox,
                        nodes: vec![
                            Element::Text("(".to_string()),
                            Element::Text(OxType::format("v1")),
                            Element::Text(")".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ox,
                        nodes: vec![
                            Element::Text("(".to_string()),
                            Element::Text(OxType::format("v2")),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ox,
                        nodes: vec![
                            Element::Text(OxType::format("v3")),
                            Element::Text(")".to_string()),
                        ],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ox,
                        nodes: vec![
                            Element::Text(OxType::format_default()),
                            Element::Text(",".to_string()),
                            Element::Text("v1".to_string()),
                        ],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(input).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn st() {
                let mut st_types: HashMap<&str, StType> = Default::default();
                // C Language Standards
                st_types.insert("-ansiC", StType::AnsiC);
                st_types.insert("-ansiC-89", StType::AnsiC89);
                st_types.insert("-isoC", StType::IsoC);
                st_types.insert("-isoC-90", StType::IsoC90);
                st_types.insert("-isoC-amd1", StType::IsoCAmd1);
                st_types.insert("-isoC-tcor1", StType::IsoCTcor1);
                st_types.insert("-isoC-tcor2", StType::IsoCTcor2);
                st_types.insert("-isoC-99", StType::IsoC99);
                st_types.insert("-isoC-2011", StType::IsoC2011);
                // POSIX.1 Standards before XPG4.2
                st_types.insert("-p1003.1-88", StType::P1003188);
                st_types.insert("-p1003.1", StType::P10031);
                st_types.insert("-p1003.1-90", StType::P1003190);
                st_types.insert("-iso9945-1-90", StType::Iso9945190);
                st_types.insert("-p1003.1b-93", StType::P10031B93);
                st_types.insert("-p1003.1b", StType::P10031B);
                st_types.insert("-p1003.1c-95", StType::P10031C95);
                st_types.insert("-p1003.1i-95", StType::P10031I95);
                st_types.insert("-p1003.1-96", StType::P1003196);
                st_types.insert("-iso9945-1-96", StType::Iso9945196);
                // X/Open Portability Guide before XPG4.2
                st_types.insert("-xpg3", StType::Xpg3);
                st_types.insert("-p1003.2", StType::P10032);
                st_types.insert("-p1003.2-92", StType::P1003292);
                st_types.insert("-iso9945-2-93", StType::Iso9945293);
                st_types.insert("-p1003.2a-92", StType::P10032A92);
                st_types.insert("-xpg4", StType::Xpg4);
                // X/Open Portability Guide Issue 4 Version 2 and Related Standards
                st_types.insert("-susv1", StType::Susv1);
                st_types.insert("-xpg4.2", StType::Xpg42);
                st_types.insert("-xcurses4.2", StType::XCurses42);
                st_types.insert("-p1003.1g-2000", StType::P10031G2000);
                st_types.insert("-svid4", StType::Svid4);
                // X/Open Portability Guide Issue 5 and Related Standards
                st_types.insert("-susv2", StType::Susv2);
                st_types.insert("-xbd5", StType::Xbd5);
                st_types.insert("-xsh5", StType::Xsh5);
                st_types.insert("-xcu5", StType::Xcu5);
                st_types.insert("-xns5", StType::Xns5);
                st_types.insert("-xns5.2", StType::Xns52);
                // POSIX Issue 6 Standards
                st_types.insert("-p1003.1-2001", StType::P100312001);
                st_types.insert("-susv3", StType::Susv3);
                st_types.insert("-p1003.1-2004", StType::P100312004);
                // POSIX Issues 7 and 8 Standards
                st_types.insert("-p1003.1-2008", StType::P100312008);
                st_types.insert("-susv4", StType::Susv4);
                st_types.insert("-p1003.1-2024", StType::P100312024);
                // Other Standards
                st_types.insert("-ieee754", StType::Ieee754);
                st_types.insert("-iso8601", StType::Iso8601);
                st_types.insert("-iso8802-3", StType::Iso88023);
                st_types.insert("-ieee1275-94", StType::Ieee127594);

                for (str_type, enum_type) in st_types {
                    let content = format!(".St {str_type} word");
                    let elements = vec![Element::Macro(MacroNode {
                        mdoc_macro: Macro::St(enum_type),
                        nodes: vec![Element::Text("word".to_string())],
                    })];

                    let mdoc = MdocParser::parse_mdoc(&content).unwrap();
                    assert_eq!(mdoc.elements, elements, "St type: {str_type}");
                }
            }

            #[test]
            fn st_no_abbreviation() {
                assert_eq!(MdocParser::parse_mdoc(".St word").unwrap().elements, vec![])
            }

            #[test]
            fn st_parsed() {
                let content = ".St -ansiC Ad addr1";
                let elements = vec![
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::St(StType::AnsiC),
                        nodes: vec![],
                    }),
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ad,
                        nodes: vec![Element::Text("addr1".to_string())],
                    }),
                ];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }

            #[test]
            fn st_not_callable() {
                let content = ".Ad addr1 St -ansiC word";
                let elements = vec![Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("St".to_string()),
                        Element::Text("-ansiC".to_string()),
                        Element::Text("word".to_string()),
                    ],
                })];

                let mdoc = MdocParser::parse_mdoc(content).unwrap();
                assert_eq!(mdoc.elements, elements);
            }
        }

        #[test]
        fn ad() {
            let content = ".Ad addr1 addr2 addr3";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("addr2".to_string()),
                    Element::Text("addr3".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]

        fn ad_no_args() {
            let content = ".Ad";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ad_parsed() {
            let content = ".Ad addr1 Ad arg1 arg2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("arg1".to_string()),
                        Element::Text("arg2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ad_callable() {
            let content = ".Ad word1 Ad addr1 addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("word1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("addr2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn an_split() {
            let content = ".An -split";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::An {
                    author_name_type: AnType::Split,
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn an_nosplit() {
            let content = ".An -nosplit";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::An {
                    author_name_type: AnType::NoSplit,
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn an_name() {
            let content = ".An John Doe";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::An {
                    author_name_type: AnType::Name,
                },
                nodes: vec![
                    Element::Text("John".to_string()),
                    Element::Text("Doe".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]

        fn an_no_args() {
            let content = ".An";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn an_parsed() {
            let content = ".An Name Ad addr1 addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::An {
                        author_name_type: AnType::Name,
                    },
                    nodes: vec![Element::Text("Name".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("addr2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn an_callable() {
            let content = ".Ad word1 An -nosplit";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("word1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::An {
                        author_name_type: AnType::NoSplit,
                    },
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ap() {
            let content = ".Ap Text Line";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ap,
                nodes: vec![
                    Element::Text("Text".to_string()),
                    Element::Text("Line".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ap_no_args() {
            let content = ".Ap";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ap,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ap_parsed() {
            let content = ".Ap some text Ad addr1 addr2";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ap,
                    nodes: vec![
                        Element::Text("some".to_string()),
                        Element::Text("text".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("addr2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ap_callable() {
            let content = ".Ad addr1 Ap word1 word2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ap,
                    nodes: vec![
                        Element::Text("word1".to_string()),
                        Element::Text("word2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ar() {
            let content = ".Ar arg1 arg2 arg3";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ar,
                nodes: vec![
                    Element::Text("arg1".to_string()),
                    Element::Text("arg2".to_string()),
                    Element::Text("arg3".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ar_no_args() {
            let content = ".Ar";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ar,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ar_parsed() {
            let content = ".Ar arg1 Ad addr1 addr2";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("arg1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("addr2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ar_callable() {
            let content = ".Ad addr1 Ar word1 word2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![
                        Element::Text("word1".to_string()),
                        Element::Text("word2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bt() {
            // "Text Line" will be ignored
            let content = ".Bt Text Line";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bt,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bt_no_args() {
            let content = ".Bt";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bt,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bt_not_parsed() {
            // "Ad" macro will be ignored
            let content = ".Bt Ad addr1 addr2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Bt,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn bt_not_callable() {
            let content = ".Ad addr1 Bt addr2";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Bt".to_string()),
                    Element::Text("addr2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn cd() {
            let content = ".Cd kernel configuration declaration";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Cd,
                nodes: vec![
                    Element::Text("kernel".to_string()),
                    Element::Text("configuration".to_string()),
                    Element::Text("declaration".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn cd_no_args() {
            let content = ".Cd";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn cd_parsed() {
            let content = ".Cd declaration Ad addr1 addr2";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Cd,
                    nodes: vec![Element::Text("declaration".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("addr2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn cd_callable() {
            let content = ".Ad addr1 Cd configuration declaration";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Cd,
                    nodes: vec![
                        Element::Text("configuration".to_string()),
                        Element::Text("declaration".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn cm() {
            let content = ".Cm mod1 mod2 mod3";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Cm,
                nodes: vec![
                    Element::Text("mod1".to_string()),
                    Element::Text("mod2".to_string()),
                    Element::Text("mod3".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn cm_no_args() {
            let content = ".Cm";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn cm_parsed() {
            let content = ".Cm cmdm1 cmdm2 Ad addr1 addr2";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Cm,
                    nodes: vec![
                        Element::Text("cmdm1".to_string()),
                        Element::Text("cmdm2".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("addr2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn cm_callable() {
            let content = ".Ad addr1 Cm mod1 mod2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Cm,
                    nodes: vec![
                        Element::Text("mod1".to_string()),
                        Element::Text("mod2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn db() {
            let content = ".Db text_argument";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Db,
                nodes: vec![Element::Text("text_argument".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn db_not_callable() {
            let content = ".Ad addr1 Db addr2";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Db".to_string()),
                    Element::Text("addr2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn db_not_parsed() {
            let content = ".Db Ad";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Db,
                nodes: vec![Element::Text("Ad".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn db_no_args() {
            let content = ".Db";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Db,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dd() {
            let content = ".Dd $Mdocdate: July 2 2018$";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Dd,
                nodes: vec![Element::Text("$Mdocdate: July 2 2018$".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dd_no_date() {
            let content = ".Dd $Mdocdate$";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Dd,
                nodes: vec![Element::Text("$Mdocdate$".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dd_no_args() {
            let content = ".Dd";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Dd,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dd_not_callable() {
            let content = ".Ad addr1 Dd addr2";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Dd".to_string()),
                    Element::Text("addr2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dd_not_parsed() {
            let content = ".Dd Ad 2, 2018";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Dd,
                nodes: vec![Element::Text("Ad 2, 2018".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dt() {
            let content = ".Dt PROGNAME 1 i386\n.Dt 1 i386 \n.Dt PROGNAME 1";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Dt {
                        title: Some("PROGNAME".to_string()),
                        section: "1".to_string(),
                        arch: Some("i386".to_string()),
                    },
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Dt {
                        title: None,
                        section: "1".to_string(),
                        arch: Some("i386".to_string()),
                    },
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Dt {
                        title: Some("PROGNAME".to_string()),
                        section: "1".to_string(),
                        arch: None,
                    },
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dt_not_callable() {
            let content = ".Ad addr1 Dt addr2";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Dt".to_string()),
                    Element::Text("addr2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dt_not_parsed() {
            let content = ".Dt Ad 1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Dt {
                    title: Some("Ad".to_string()),
                    section: "1".to_string(),
                    arch: None,
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dt_no_args() {
            let content = ".Dt";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dv() {
            let content = ".Dv CONSTANT1 CONSTANT2";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Dv,
                nodes: vec![
                    Element::Text("CONSTANT1".to_string()),
                    Element::Text("CONSTANT2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dv_no_args() {
            let content = ".Dv";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn dv_callable() {
            let content = ".Ad addr1 addr2 Dv CONST1";
            let elemenets = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("addr2".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Dv,
                    nodes: vec![Element::Text("CONST1".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elemenets)
        }

        #[test]
        fn dv_parsed() {
            let content = ".Dv CONST1 Ad addr1 addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Dv,
                    nodes: vec![Element::Text("CONST1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("addr2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn em() {
            let input = ".Em word1 word2";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Em,
                nodes: vec![
                    Element::Text("word1".to_string()),
                    Element::Text("word2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn em_no_args() {
            let input = ".Em";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn em_parsed() {
            let input = ".Em word1 Ad addr1 addr2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Em,
                    nodes: vec![Element::Text("word1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("addr2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn em_callable() {
            let input = ".Ad addr1 addr2 Em word1";
            let elemenets = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("addr2".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Em,
                    nodes: vec![Element::Text("word1".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elemenets)
        }

        #[test]
        fn er() {
            let input = ".Er ERROR";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Er,
                nodes: vec![Element::Text("ERROR".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn er_no_args() {
            let input = ".Er";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn er_parsed() {
            let input = ".Er ERROR Ad addr1";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Er,
                    nodes: vec![Element::Text("ERROR".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn er_callable() {
            let input = ".Ad addr1 addr2 Er ERROR ERROR2";
            let elemenets = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("addr2".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Er,
                    nodes: vec![
                        Element::Text("ERROR".to_string()),
                        Element::Text("ERROR2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elemenets)
        }

        #[test]
        fn es() {
            let input = ".Es ( )";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Es {
                    opening_delimiter: '(',
                    closing_delimiter: ')',
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn es_bad_args() {
            assert_eq!(MdocParser::parse_mdoc(".Es").unwrap().elements, vec![]);
            assert_eq!(MdocParser::parse_mdoc(".Es (").unwrap().elements, vec![]);
            assert_eq!(MdocParser::parse_mdoc(".Es { }").unwrap().elements, vec![]);
        }

        #[test]
        fn es_parsed() {
            let input = ".Es [ ] At 2.32";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Es {
                        opening_delimiter: '[',
                        closing_delimiter: ']',
                    },
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::At,
                    nodes: vec![
                        Element::Text(AtType::General.to_string()),
                        Element::Text("2.32".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn es_callable() {
            let input = ".Ad addr1 addr2 Es ( )";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![
                        Element::Text("addr1".to_string()),
                        Element::Text("addr2".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Es {
                        opening_delimiter: '(',
                        closing_delimiter: ')',
                    },
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // .Ev -----------------------------------------------------------

        #[test]
        fn ev() {
            let input = ".Ev DISPLAY";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ev,
                nodes: vec![Element::Text("DISPLAY".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ev_no_args() {
            let input = ".Ev";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ev_parsed() {
            let input = ".Ev DISPLAY Ad ADDRESS";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ev,
                    nodes: vec![Element::Text("DISPLAY".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("ADDRESS".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ev_callable() {
            let input = ".Ad addr1 Ev ADDRESS";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ev,
                    nodes: vec![Element::Text("ADDRESS".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // .Ex -----------------------------------------------------------

        #[test]
        fn ex() {
            let input = ".Ex -std grep";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ex,
                nodes: vec![Element::Text("grep".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ex_no_args() {
            let input = ".Ex";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ex_not_parsed() {
            let input = ".Ex -std grep Ad addr";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ex,
                nodes: vec![
                    Element::Text("grep".to_string()),
                    Element::Text("Ad".to_string()),
                    Element::Text("addr".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ex_not_callable() {
            let input = ".Ad addr Ex -std grep";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr".to_string()),
                    Element::Text("Ex".to_string()),
                    Element::Text("-std".to_string()),
                    Element::Text("grep".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // .Fa -----------------------------------------------------------

        #[test]
        fn fa() {
            let input = ".Fa size_t";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Fa,
                nodes: vec![Element::Text("size_t".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fa_no_args() {
            let input = ".Fa";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fa_parsed() {
            let input = ".Fa funcname Ft const char *";
            let elemets = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fa,
                    nodes: vec![Element::Text("funcname".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ft,
                    nodes: vec![
                        Element::Text("const".to_string()),
                        Element::Text("char".to_string()),
                        Element::Text("*".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elemets);
        }

        #[test]
        fn fa_callable() {
            let input = ".Ft functype Fa int";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ft,
                    nodes: vec![Element::Text("functype".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fa,
                    nodes: vec![Element::Text("int".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fd() {
            let input = ".Fd #define sa_handler __sigaction_u.__sa_handler\n.Fd #define SIO_MAXNFDS\n.Fd #endif";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fd {
                        directive: "#define".to_string(),
                        arguments: vec![
                            "sa_handler".to_string(),
                            "__sigaction_u.__sa_handler".to_string(),
                        ],
                    },
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fd {
                        directive: "#define".to_string(),
                        arguments: vec!["SIO_MAXNFDS".to_string()],
                    },
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fd {
                        directive: "#endif".to_string(),
                        arguments: vec![],
                    },
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fd_no_args() {
            let input = ".Fd";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fd_not_parsed() {
            let input = ".Fd #define Ad addr";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Fd {
                    directive: "#define".to_string(),
                    arguments: vec!["Ad".to_string(), "addr".to_string()],
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fd_not_callable() {
            let input = ".Ad Fd #define ADDRESS";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("Fd".to_string()),
                    Element::Text("#define".to_string()),
                    Element::Text("ADDRESS".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fl() {
            let input = ".Fl H | L | P\n.Fl inet";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fl,
                    nodes: vec![
                        Element::Text("H".to_string()),
                        Element::Text("|".to_string()),
                        Element::Text("L".to_string()),
                        Element::Text("|".to_string()),
                        Element::Text("P".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fl,
                    nodes: vec![Element::Text("inet".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fl_no_args() {
            let input = ".Fl";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Fl,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fl_parsed() {
            let input = ".Fl inet Ar destination gateway";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fl,
                    nodes: vec![Element::Text("inet".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![
                        Element::Text("destination".to_string()),
                        Element::Text("gateway".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fl_callable() {
            let input = ".Cm add Fl inet";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Cm,
                    nodes: vec![Element::Text("add".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fl,
                    nodes: vec![Element::Text("inet".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // .Fn -----------------------------------------------------------

        #[test]
        fn r#fn() {
            let input = ".Fn \"int funcname\" \"int arg0\" \"int arg1\"\n.Fn funcname \"int arg0\"\n.Fn funcname arg0\n.Fn ( funcname )";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fn {
                        funcname: "\"int funcname\"".to_string(),
                    },
                    nodes: vec![
                        Element::Text("int arg0".to_string()),
                        Element::Text("int arg1".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fn {
                        funcname: "funcname".to_string(),
                    },
                    nodes: vec![Element::Text("int arg0".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fn {
                        funcname: "funcname".to_string(),
                    },
                    nodes: vec![Element::Text("arg0".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fn {
                        funcname: "(funcname".to_string(),
                    },
                    nodes: vec![Element::Text(")".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fn_no_args() {
            let input = ".Fn";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fn_parsed() {
            let input = ".Fn funcname arg Ft int";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fn {
                        funcname: "funcname".to_string(),
                    },
                    nodes: vec![Element::Text("arg".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ft,
                    nodes: vec![Element::Text("int".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fn_callable() {
            let input = ".Ft functype Fn funcname";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ft,
                    nodes: vec![Element::Text("functype".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fn {
                        funcname: "funcname".to_string(),
                    },
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fr() {
            let input = ".Fr 32";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Fr,
                nodes: vec![Element::Text("32".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fr_no_args() {
            let input = ".Fr";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fr_parsed() {
            let input = ".Fr 32 Ad addr";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fr,
                    nodes: vec![Element::Text("32".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fr_callable() {
            let input = ".Ft functype Fr 12";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ft,
                    nodes: vec![Element::Text("functype".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fr,
                    nodes: vec![Element::Text("12".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // .Ft -----------------------------------------------------------

        #[test]
        fn ft() {
            let input = ".Ft int32 void";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ft,
                nodes: vec![
                    Element::Text("int32".to_string()),
                    Element::Text("void".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ft_no_args() {
            let input = ".Ft";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ft_parsed() {
            let input = ".Ft functype Fa arg";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ft,
                    nodes: vec![Element::Text("functype".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fa,
                    nodes: vec![Element::Text("arg".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ft_callable() {
            let input = ".Fa funcname Ft const char *";
            let elemets = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fa,
                    nodes: vec![Element::Text("funcname".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ft,
                    nodes: vec![
                        Element::Text("const".to_string()),
                        Element::Text("char".to_string()),
                        Element::Text("*".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elemets);
        }

        #[test]
        fn fx() {
            let input = ".Fx 1.0 arg\n";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Fx,
                nodes: vec![
                    Element::Text(FxType::format("1.0")),
                    Element::Text("arg".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fx_no_args() {
            let input = ".Fx";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Fx,
                nodes: vec![Element::Text(FxType::format_default())],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fx_parsed() {
            let input = ".Fx 1.0 Ad addr";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fx,
                    nodes: vec![Element::Text(FxType::format("1.0"))],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn fx_callable() {
            let input = ".Ad addr Fx 1.0";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fx,
                    nodes: vec![Element::Text(FxType::format("1.0"))],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn hf() {
            let input = ".Hf file/path file2/path";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Hf,
                nodes: vec![
                    Element::Text("file/path".to_string()),
                    Element::Text("file2/path".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn hf_no_args() {
            let input = ".Hf";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Hf,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn hf_not_parsed() {
            let input = ".Hf Ad addr";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Hf,
                nodes: vec![
                    Element::Text("Ad".to_string()),
                    Element::Text("addr".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn hf_not_callable() {
            let input = ".Ad Hf path/to/some/file";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("Hf".to_string()),
                    Element::Text("path/to/some/file".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ic() {
            let input = ".Ic :wq";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ic,
                nodes: vec![Element::Text(":wq".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ic_no_args() {
            let input = ".Ic";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ic_parsed() {
            let input = ".Ic lookup Cm file bind";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ic,
                    nodes: vec![Element::Text("lookup".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Cm,
                    nodes: vec![
                        Element::Text("file".to_string()),
                        Element::Text("bind".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ic_callable() {
            let input = ".Ad addr Ic :wq";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ic,
                    nodes: vec![Element::Text(":wq".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn r#in() {
            let input = ".In stdatomic.h";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::In {
                    filename: "stdatomic.h".to_string(),
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn in_no_args() {
            let input = ".In";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn in_parsed() {
            let input = ".In stdio.h Ad addr";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::In {
                        filename: "stdio.h".to_string(),
                    },
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn in_callable() {
            let input = ".Ad addr In stdatomic.c";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::In {
                        filename: "stdatomic.c".to_string(),
                    },
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn lb() {
            let input = ".Lb libname";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Lb {
                    lib_name: "libname".to_string(),
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn lb_wrong_args() {
            assert_eq!(MdocParser::parse_mdoc(".Lb").unwrap().elements, vec![]);
        }

        #[test]
        fn lb_not_parsed() {
            let input = ".Lb Ar";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Lb {
                    lib_name: "Ar".to_string(),
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn lb_not_callable() {
            let input = ".Ad Lb stdio.h";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("Lb".to_string()),
                    Element::Text("stdio.h".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn li() {
            let input = ".Li Book Antiqua";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Li,
                nodes: vec![
                    Element::Text("Book".to_string()),
                    Element::Text("Antiqua".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn li_no_args() {
            let input = ".Li";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn li_parsed() {
            let input = ".Li font Ev DEFAULT_FONT";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Li,
                    nodes: vec![Element::Text("font".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ev,
                    nodes: vec![Element::Text("DEFAULT_FONT".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn li_callable() {
            let input = ".Ad addr Li font";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Li,
                    nodes: vec![Element::Text("font".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn lk() {
            let input = ".Lk https://bsd.lv The BSD.lv Project";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Lk {
                    uri: "https://bsd.lv".to_string(),
                },
                nodes: vec![
                    Element::Text("The".to_string()),
                    Element::Text("BSD.lv".to_string()),
                    Element::Text("Project".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn lk_no_args() {
            let input = ".Lk";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn lk_parsed() {
            let input = ".Lk https://bsd.lv Ev NAME";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Lk {
                        uri: "https://bsd.lv".to_string(),
                    },
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ev,
                    nodes: vec![Element::Text("NAME".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn lk_callable() {
            let input = ".Ad addr Lk https://bsd.lv";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Lk {
                        uri: "https://bsd.lv".to_string(),
                    },
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn lp() {
            let input = ".Lp";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Lp,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn lp_not_parsed() {
            let input = ".Lp Ad addr";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Lp,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn lp_not_callable() {
            let input = ".Ad addr Lp";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr".to_string()),
                    Element::Text("Lp".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Ms --------------------------------------------------------------------------

        #[test]
        fn ms() {
            let content = ".Ms alpha beta";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ms,
                nodes: vec![
                    Element::Text("alpha".to_string()),
                    Element::Text("beta".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ms_no_args() {
            let content = ".Ms";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ms_parsed() {
            let content = ".Ms beta Ux";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ms,
                    nodes: vec![Element::Text("beta".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ux,
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ms_callable() {
            let content = ".No / Ms aleph";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::No,
                    nodes: vec![Element::Text("/".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ms,
                    nodes: vec![Element::Text("aleph".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Mt --------------------------------------------------------------------------

        #[test]
        fn mt() {
            let content = ".Mt abc@gmail.com abc@gmail.com";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Mt,
                nodes: vec![
                    Element::Text("abc@gmail.com".to_string()),
                    Element::Text("abc@gmail.com".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn mt_no_args() {
            let content = ".Mt";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn mt_parsed() {
            let content = ".Mt abc@gmail.com Ux";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Mt,
                    nodes: vec![Element::Text("abc@gmail.com".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ux,
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn mt_callable() {
            let content = ".Ad address1 Mt abc@gmail.com";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("address1".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Mt,
                    nodes: vec![Element::Text("abc@gmail.com".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // No --------------------------------------------------------------------------

        #[test]
        fn no() {
            let content = ".No a b c";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::No,
                nodes: vec![
                    Element::Text("a".to_string()),
                    Element::Text("b".to_string()),
                    Element::Text("c".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements)
        }

        #[test]
        fn no_no_args() {
            let content = ".No";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn no_parsed() {
            let content = ".No a Ar value";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::No,
                    nodes: vec![Element::Text("a".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn no_callable() {
            let content = ".Ar value No a";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::No,
                    nodes: vec![Element::Text("a".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Ns --------------------------------------------------------------------------

        #[test]
        fn ns() {
            let content = ".Ns";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ns,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ns_parsed() {
            let content = ".Ns Ar value";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ns,
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ns_callable() {
            let content = ".Ar value Ns";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ns,
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Os --------------------------------------------------------------------------

        #[test]
        fn os() {
            let content = ".Os footer text";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Os,
                nodes: vec![
                    Element::Text("footer".to_string()),
                    Element::Text("text".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn os_no_args() {
            let content = ".Os";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Os,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn os_not_parsed() {
            let content = ".Os Ar value";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Os,
                nodes: vec![
                    Element::Text("Ar".to_string()),
                    Element::Text("value".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn os_not_callable() {
            let content = ".Ad addr1 Os";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Os".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Ot --------------------------------------------------------------------------

        #[test]
        fn ot() {
            let content = ".Ot functype";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ft,
                nodes: vec![Element::Text("functype".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ot_no_args() {
            let content = ".Ot";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ot_parsed() {
            let content = ".Ot functype Ar value";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ft,
                    nodes: vec![Element::Text("functype".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ot_callable() {
            let content = ".Ar value Ot functype";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ft,
                    nodes: vec![Element::Text("functype".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Pa --------------------------------------------------------------------------

        #[test]
        fn pa() {
            let content = ".Pa name1 name2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Pa,
                nodes: vec![
                    Element::Text("name1".to_string()),
                    Element::Text("name2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn pa_no_args() {
            let content = ".Pa";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn pa_parsed() {
            let content = ".Pa name1 name2 Ar value";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Pa,
                    nodes: vec![
                        Element::Text("name1".to_string()),
                        Element::Text("name2".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn pa_callable() {
            let content = ".Ar value Pa name1 name2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Pa,
                    nodes: vec![
                        Element::Text("name1".to_string()),
                        Element::Text("name2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Pf --------------------------------------------------------------------------

        #[test]
        fn pf() {
            let content = ".Pf $ Ar variable_name";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Pf {
                        prefix: "$".to_string(),
                    },
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("variable_name".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn pf_no_args() {
            let content = ".Pf";
            let elements = vec![];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn pf_callable() {
            let content = ".Ar value Pf $ Ar variable_name";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Pf {
                        prefix: "$".to_string(),
                    },
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("variable_name".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Pp --------------------------------------------------------------------------

        #[test]
        fn pp() {
            let content = ".Pp";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Pp,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn pp_not_parsed() {
            // "Ar" macro will be ignored
            let content = ".Pp Ar value";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Pp,
                nodes: vec![
                    Element::Text("Ar".to_string()),
                    Element::Text("value".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn pp_not_callable() {
            let content = ".Ad addr1 Pp";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Pp".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Rv --------------------------------------------------------------------------

        #[test]
        fn rv() {
            let content = ".Rv -std f1 f2 Ar value";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Rv,
                nodes: vec![
                    Element::Text("f1".to_string()),
                    Element::Text("f2".to_string()),
                    Element::Text("Ar".to_string()),
                    Element::Text("value".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn rv_no_std() {
            assert_eq!(
                MdocParser::parse_mdoc(".Rv f1 f2").unwrap().elements,
                vec![]
            );
        }

        #[test]
        fn rv_no_args() {
            let content = ".Rv -std";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Rv,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn rv_no_std_and_args() {
            assert_eq!(MdocParser::parse_mdoc(".Rv").unwrap().elements, vec![]);
        }

        #[test]
        fn rv_not_parsed() {
            // "Ar" macro will be ignored
            let content = ".Rv -std f1 Ar value";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Rv,
                nodes: vec![
                    Element::Text("f1".to_string()),
                    Element::Text("Ar".to_string()),
                    Element::Text("value".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn rv_not_callable() {
            let content = ".Ad addr1 Rv -std f1";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Rv".to_string()),
                    Element::Text("-std".to_string()),
                    Element::Text("f1".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Sm --------------------------------------------------------------------------

        #[test]
        fn sm_on() {
            let content = ".Sm on";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sm(Some(SmMode::On)),
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sm_off() {
            let content = ".Sm off";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sm(Some(SmMode::Off)),
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sm_no_args() {
            let content = ".Sm";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sm(None),
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sm_not_parsed() {
            // "Ar" macro will be ignored
            let content = ".Sm Ar value";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Sm(None),
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sm_not_callable() {
            let content = ".Ad addr1 Sm";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Sm".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Sx --------------------------------------------------------------------------

        #[test]
        fn sx() {
            let content = ".Sx MANUAL STRUCTURE";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sx,
                nodes: vec![
                    Element::Text("MANUAL".to_string()),
                    Element::Text("STRUCTURE".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sx_no_args() {
            assert_eq!(MdocParser::parse_mdoc(".Sx").unwrap().elements, vec![]);
        }

        #[test]
        fn sx_wrong_args() {
            assert_eq!(
                MdocParser::parse_mdoc(".Sx Ar value").unwrap().elements,
                vec![]
            );
        }

        #[test]
        fn sx_parsed() {
            let content = ".Sx MANUAL STRUCTURE Ar value";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Sx,
                    nodes: vec![
                        Element::Text("MANUAL".to_string()),
                        Element::Text("STRUCTURE".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sx_callable() {
            let content = ".Ar value Sx MANUAL STRUCTURE";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Sx,
                    nodes: vec![
                        Element::Text("MANUAL".to_string()),
                        Element::Text("STRUCTURE".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Sy --------------------------------------------------------------------------

        #[test]
        fn sy() {
            let content = ".Sy word1 word2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Sy,
                nodes: vec![
                    Element::Text("word1".to_string()),
                    Element::Text("word2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sy_no_args() {
            assert_eq!(MdocParser::parse_mdoc(".Sy").unwrap().elements, vec![]);
        }

        #[test]
        fn sy_parsed() {
            let content = ".Sy word1 word2 Ar value";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Sy,
                    nodes: vec![
                        Element::Text("word1".to_string()),
                        Element::Text("word2".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn sy_callable() {
            let content = ".Ar value Sy word1 word2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Sy,
                    nodes: vec![
                        Element::Text("word1".to_string()),
                        Element::Text("word2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Tn --------------------------------------------------------------------------

        #[test]
        fn tn() {
            let content = ".Tn word1 word2";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Tn,
                nodes: vec![
                    Element::Text("word1".to_string()),
                    Element::Text("word2".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn tn_no_args() {
            assert_eq!(MdocParser::parse_mdoc(".Tn").unwrap().elements, vec![]);
        }

        #[test]
        fn tn_parsed() {
            let content = ".Tn word1 word2 Ar value";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Tn,
                    nodes: vec![
                        Element::Text("word1".to_string()),
                        Element::Text("word2".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn tn_callable() {
            let content = ".Ar value Tn word1 word2";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Tn,
                    nodes: vec![
                        Element::Text("word1".to_string()),
                        Element::Text("word2".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Ud --------------------------------------------------------------------------

        #[test]
        fn ud() {
            let content = ".Ud";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ud,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ud_not_parsed() {
            // "Ar" macro will be ignored
            let content = ".Ud Ar value";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ud,
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ud_not_callable() {
            let content = ".Ad addr1 Ud";
            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ad,
                nodes: vec![
                    Element::Text("addr1".to_string()),
                    Element::Text("Ud".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Ux --------------------------------------------------------------------------

        #[test]
        fn ux() {
            let content = ".Ux";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Ux,
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ux_parsed() {
            let content = ".Ux Ar value";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ux,
                    nodes: vec![],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn ux_callable() {
            let content = ".Ar value Ux";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ux,
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Va --------------------------------------------------------------------------

        #[test]
        fn va() {
            let content = ".Va const char *bar";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Va,
                nodes: vec![
                    Element::Text("const".to_string()),
                    Element::Text("char".to_string()),
                    Element::Text("*bar".to_string()),
                ],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn va_without_type() {
            let content = ".Va foo";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Va,
                nodes: vec![Element::Text("foo".to_string())],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn va_no_args() {
            assert_eq!(MdocParser::parse_mdoc(".Va").unwrap().elements, vec![]);
        }

        #[test]
        fn va_parsed() {
            let content = ".Va bool foo Ar value";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Va,
                    nodes: vec![
                        Element::Text("bool".to_string()),
                        Element::Text("foo".to_string()),
                    ],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn va_callable() {
            let content = ".Ar value Va char foo";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Va,
                    nodes: vec![
                        Element::Text("char".to_string()),
                        Element::Text("foo".to_string()),
                    ],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        // Xr --------------------------------------------------------------------------

        #[test]
        fn xr() {
            let content = ".Xr mandoc 1";

            let elements = vec![Element::Macro(MacroNode {
                mdoc_macro: Macro::Xr {
                    name: "mandoc".to_string(),
                    section: "1".to_string(),
                },
                nodes: vec![],
            })];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        #[should_panic]
        fn xr_no_args() {
            assert!(MdocParser::parse_mdoc(".Xr mandoc").is_err());
            assert!(MdocParser::parse_mdoc(".Xr 1").is_err());
            assert!(MdocParser::parse_mdoc(".Xr").is_err());
        }

        #[test]
        fn xr_parsed() {
            let content = ".Xr mandoc 1 test Ns";

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Xr {
                        name: "mandoc".to_string(),
                        section: "1".to_string(),
                    },
                    nodes: vec![Element::Text("test".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ns,
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn xr_callable() {
            let content = ".Ar value Xr mandoc 1";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ar,
                    nodes: vec![Element::Text("value".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Xr {
                        name: "mandoc".to_string(),
                        section: "1".to_string(),
                    },
                    nodes: vec![],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(content).unwrap();
            assert_eq!(mdoc.elements, elements);
        }
    }

    mod general {
        use crate::man_util::parser::*;

        #[test]
        fn comment_in_text_line() {
            let input = r#".\" comment
.\" Still comment1
.\" Still comment2
Line \" comment
.\" Still comment2
Line \" comment
"#;
            let elements = vec![
                Element::Text("Line ".to_string()),
                Element::Text("Line ".to_string()),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn comment_in_lines() {
            let input = r#".%A John \" Doe
.Fo funcname \" comment
Line
.Fc
.%B John \" Doe
.%C John \" Doe
.%I John \" Doe
.%J John \" Doe
.%N John \" Doe
.%O John \" Doe
.%Q John \" Doe
.%R John \" Doe
.%T John \" Doe
.%V John \" Doe
"#;

            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::A,
                    nodes: vec![Element::Text("John".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fo {
                        funcname: "funcname".to_string(),
                    },
                    nodes: vec![Element::Text("Line".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::B,
                    nodes: vec![Element::Text("John".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::C,
                    nodes: vec![Element::Text("John".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::I,
                    nodes: vec![Element::Text("John".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::J,
                    nodes: vec![Element::Text("John".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::N,
                    nodes: vec![Element::Text("John".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::O,
                    nodes: vec![Element::Text("John".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Q,
                    nodes: vec![Element::Text("John".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::R,
                    nodes: vec![Element::Text("John".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::T,
                    nodes: vec![Element::Text("John".to_string())],
                }),
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::V,
                    nodes: vec![Element::Text("John".to_string())],
                }),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }

        #[test]
        fn comment_in_macros() {
            let input = ".Ad addr \\\"comment";
            let elements = vec![
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Ad,
                    nodes: vec![Element::Text("addr".to_string())],
                }),
                Element::Text("".to_string()),
            ];

            let mdoc = MdocParser::parse_mdoc(input).unwrap();
            assert_eq!(mdoc.elements, elements);
        }
    }
}
