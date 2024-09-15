#![allow(dead_code)]

pub const EXIT_STATUS_NO_DIFFERENCE: u8 = 0;
pub const EXIT_STATUS_DIFFERENCE: u8 = 1;
pub const EXIT_STATUS_TROUBLE: u8 = 2;
pub const NO_NEW_LINE_AT_END_OF_FILE: &'static str = "\\ No newline at end of file";
pub const COULD_NOT_UNWRAP_FILENAME: &'static str = "Could not unwrap filename!";
pub const UTF8_NOT_ALLOWED_BYTES: [u8; 26] = [
    0, 1, 2, 3, 4, 5, 6, 11, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28, 29, 30, 31,
    127,
];
