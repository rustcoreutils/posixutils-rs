//! Command processing for vi editor.
//!
//! This module handles command mode parsing and execution,
//! including motions, operators, and text objects.

pub mod motion;
pub mod operator;
pub mod parser;
pub mod text_object;

pub use motion::{
    MotionResult, find_char_backward, find_char_forward, find_matching_bracket,
    move_bigword_backward, move_bigword_end, move_bigword_forward, move_down, move_left,
    move_right, move_to_column, move_to_first_non_blank, move_to_line, move_to_line_end,
    move_to_line_start, move_up, move_word_backward, move_word_end, move_word_forward,
    till_char_backward, till_char_forward,
};
pub use operator::{
    OperatorResult, change, delete, put_after, put_before, shift_left, shift_right, yank,
};
pub use parser::{CommandParser, MotionCommand, ParsedCommand, ParserState};
pub use text_object::{
    is_blank, is_paragraph_boundary, is_punct, is_section_boundary, is_word_char, next_bigword_end,
    next_bigword_start, next_paragraph_start, next_word_end, next_word_start, prev_bigword_start,
    prev_paragraph_start, prev_word_start, word_end, word_start,
};
