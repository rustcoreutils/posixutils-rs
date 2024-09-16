// TODO
// Avoid restating local alias names
cfg_if::cfg_if! {
    if #[cfg(target_env = "musl")] {
        // https://git.musl-libc.org/cgit/musl/tree/include/utmpx.h?id=1e7f0fcd7ff2096904fd93a2ee6d12a2392be392
        pub const EMPTY: libc::c_short = 0_i16;
        pub const RUN_LVL: libc::c_short = 1_i16;
        pub const BOOT_TIME: libc::c_short = 2_i16;
        pub const NEW_TIME: libc::c_short = 3_i16;
        pub const OLD_TIME: libc::c_short = 4_i16;
        pub const INIT_PROCESS: libc::c_short = 5_i16;
        pub const LOGIN_PROCESS: libc::c_short = 6_i16;
        pub const USER_PROCESS: libc::c_short = 7_i16;
        pub const DEAD_PROCESS: libc::c_short = 8_i16;

        // Remove when https://github.com/rust-lang/libc/issues/3190 is resolved
        // https://github.com/rust-lang/libc/commit/e3caaf6b0ea08ae294e25a861022c256a7535ec4#diff-5822a2981791fb0bb7689a921abdc2133cc73116ee125eabefad3a9374056b7a
        extern "C" {
            pub fn getutxent() -> *mut libc::utmpx;
            pub fn getutxid(ut: *const libc::utmpx) -> *mut libc::utmpx;
            pub fn getutxline(ut: *const libc::utmpx) -> *mut libc::utmpx;
            pub fn pututxline(ut: *const libc::utmpx) -> *mut libc::utmpx;
            pub fn setutxent();
            pub fn endutxent();
        }

        type LocalPIoctlOp = libc::c_int;
        type LocalPPriorityWhichT = libc::c_int;
    } else {
        pub use libc::{
            endutxent,
            getutxent,
            setutxent,
            BOOT_TIME,
            DEAD_PROCESS,
            EMPTY,
            INIT_PROCESS,
            LOGIN_PROCESS,
            NEW_TIME,
            OLD_TIME,
            RUN_LVL,
            USER_PROCESS,
        };

        cfg_if::cfg_if! {
            if #[cfg(target_os = "macos")] {
                type LocalPPriorityWhichT = libc::c_int;
            } else {
                type LocalPPriorityWhichT = libc::__priority_which_t;
            }
        }

        type LocalPIoctlOp = libc::c_ulong;
    }
}

// Constants taken from:
// https://docs.rs/term_size/0.3.2/src/term_size/platform/unix.rs.html#5-19
pub(crate) const P_WINSIZE_REQUEST_CODE: LocalPIoctlOp = ({
    #[cfg(any(target_os = "linux", target_os = "android"))]
    {
        0x5413
    }

    #[cfg(any(
        target_os = "macos",
        target_os = "ios",
        target_os = "dragonfly",
        target_os = "freebsd",
        target_os = "netbsd",
        target_os = "openbsd"
    ))]
    {
        0x40087468
    }

    #[cfg(target_os = "solaris")]
    {
        0x5468
    }
}) as LocalPIoctlOp;

pub(crate) type PPriorityWhichT = LocalPPriorityWhichT;
