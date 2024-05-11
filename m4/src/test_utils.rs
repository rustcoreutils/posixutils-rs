use crate::lexer::MacroName;

pub fn utf8(input: &[u8]) -> String {
    String::from_utf8(input.to_vec()).unwrap()
}

pub fn macro_name(input: &[u8]) -> MacroName {
    MacroName::try_from_slice(input).expect("Error parsing macro name")
}
