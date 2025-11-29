//! vi-rs: A POSIX-compliant vi editor implementation in Rust.
//!
//! This library provides a complete vi editor implementation following
//! the POSIX specification.

pub mod buffer;
pub mod command;
pub mod editor;
pub mod error;
pub mod ex;
pub mod file;
pub mod input;
pub mod mode;
pub mod options;
pub mod register;
pub mod search;
pub mod shell;
pub mod ui;
pub mod undo;

pub use buffer::{Buffer, BufferMode, Line, Position, Range};
pub use command::{
    change, delete, put_after, put_before, shift_left, shift_right, yank, CommandParser,
    MotionCommand, MotionResult, OperatorResult, ParsedCommand, ParserState,
};
pub use editor::{Editor, ExInsertMode, FindCommand, LastCommand};
pub use error::{Result, ViError};
pub use ex::{parse_ex_command, Address, AddressRange, ExCommand, ExResult};
pub use file::{read_file, write_file, FileInfo, FileManager, WriteStats};
pub use input::{InputReader, Key};
pub use mode::{enter_insert_mode, process_insert_key, InsertKind, InsertState, Mode};
pub use options::Options;
pub use register::{RegisterContent, Registers};
pub use search::{SearchDirection, SearchState, Substitutor};
pub use shell::{ShellExecutor, ShellOutput};
pub use ui::{
    byte_offset_to_display_col, char_width, display_col_to_byte_offset, expand_for_display,
    string_width, truncate_to_width, Screen, StatusLine, Terminal, TerminalSize,
};
pub use undo::{Change, ChangeKind, UndoManager};
