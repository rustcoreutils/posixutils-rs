use libc::{c_short, utmpx};

// https://git.musl-libc.org/cgit/musl/tree/include/utmpx.h?id=1e7f0fcd7ff2096904fd93a2ee6d12a2392be392
pub const EMPTY: c_short = 0_i16;
pub const RUN_LVL: c_short = 1_i16;
pub const BOOT_TIME: c_short = 2_i16;
pub const NEW_TIME: c_short = 3_i16;
pub const OLD_TIME: c_short = 4_i16;
pub const INIT_PROCESS: c_short = 5_i16;
pub const LOGIN_PROCESS: c_short = 6_i16;
pub const USER_PROCESS: c_short = 7_i16;
pub const DEAD_PROCESS: c_short = 8_i16;

// https://github.com/rust-lang/libc/commit/e3caaf6b0ea08ae294e25a861022c256a7535ec4#diff-5822a2981791fb0bb7689a921abdc2133cc73116ee125eabefad3a9374056b7a
extern "C" {
    pub fn getutxent() -> *mut utmpx;
    pub fn getutxid(ut: *const utmpx) -> *mut utmpx;
    pub fn getutxline(ut: *const utmpx) -> *mut utmpx;
    pub fn pututxline(ut: *const utmpx) -> *mut utmpx;
    pub fn setutxent();
    pub fn endutxent();
}
