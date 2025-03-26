pub mod args;
pub mod terminal;
pub mod vi;

pub fn clear_line() {
    print!("\r\x1b[K");
}

pub fn set_cursor_pos(pos: usize) {
    print!("\r\x1b[{}G", pos + 1);
}
