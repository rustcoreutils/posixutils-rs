use crate::lexer::{MacroName, MacroNameParseConfig};

pub fn utf8(input: &[u8]) -> String {
    String::from_utf8(input.to_vec()).unwrap()
}

pub fn macro_name_config(input: &[u8], min_args: usize) -> MacroNameParseConfig {
    MacroNameParseConfig {
        name: MacroName::try_from_slice(input).expect("Error parsing macro name"),
        min_args
    }
}
