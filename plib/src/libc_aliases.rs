#[cfg(target_env = "musl")]
pub mod musl;
#[cfg(target_env = "musl")]
pub use musl::*;

#[cfg(not(target_env = "musl"))]
pub use libc::{
    endutxent, getutxent, setutxent, BOOT_TIME, DEAD_PROCESS, EMPTY, INIT_PROCESS, LOGIN_PROCESS,
    NEW_TIME, OLD_TIME, RUN_LVL, USER_PROCESS,
};
