use std::collections::HashMap;

use crate::lexer::{MacroName, MacroParseConfig};

pub fn utf8(input: &[u8]) -> String {
    String::from_utf8(input.to_vec()).unwrap()
}

pub fn macro_name(input: &[u8]) -> MacroName {
    MacroName::try_from_slice(input).expect("Error parsing macro name")
}

pub fn macro_parse_config(input: &[u8], min_args: usize) -> MacroParseConfig {
    MacroParseConfig {
        name: macro_name(input),
        min_args,
    }
}

pub fn macro_parse_configs(
    items: impl IntoIterator<Item = MacroParseConfig>,
) -> HashMap<MacroName, MacroParseConfig> {
    items.into_iter().map(|c| (c.name.clone(), c)).collect()
}
