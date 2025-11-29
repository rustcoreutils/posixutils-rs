//! Editor mode definitions for vi.
//!
//! This module defines the various modes the editor can be in:
//! - Command mode (normal mode)
//! - Insert mode (various sub-types)
//! - Ex mode (command line)
//! - Replace mode
//! - Open mode

pub mod insert;
#[allow(clippy::module_inception)]
pub mod mode;

pub use insert::{enter_insert_mode, process_insert_key, InsertState};
pub use mode::{InsertKind, Mode};
