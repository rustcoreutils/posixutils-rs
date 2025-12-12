//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, PartialEq, Eq)]
pub struct Errno {
    // value is always a valid libc errno value
    value: libc::c_int,
}

impl Errno {
    pub const E2BIG: Self = Self { value: libc::E2BIG };
    pub const EACCES: Self = Self {
        value: libc::EACCES,
    };
    pub const EADDRINUSE: Self = Self {
        value: libc::EADDRINUSE,
    };
    pub const EADDRNOTAVAIL: Self = Self {
        value: libc::EADDRNOTAVAIL,
    };
    pub const EAGAIN: Self = Self {
        value: libc::EAGAIN,
    };
    pub const EALREADY: Self = Self {
        value: libc::EALREADY,
    };
    pub const EBADF: Self = Self { value: libc::EBADF };
    pub const EBADMSG: Self = Self {
        value: libc::EBADMSG,
    };
    pub const EBUSY: Self = Self { value: libc::EBUSY };
    pub const ECANCELED: Self = Self {
        value: libc::ECANCELED,
    };
    pub const ECHILD: Self = Self {
        value: libc::ECHILD,
    };
    pub const ECONNABORTED: Self = Self {
        value: libc::ECONNABORTED,
    };
    pub const ECONNREFUSED: Self = Self {
        value: libc::ECONNREFUSED,
    };
    pub const ECONNRESET: Self = Self {
        value: libc::ECONNRESET,
    };
    pub const EDEADLK: Self = Self {
        value: libc::EDEADLK,
    };
    pub const EDESTADDRREQ: Self = Self {
        value: libc::EDESTADDRREQ,
    };
    pub const EDOM: Self = Self { value: libc::EDOM };
    pub const EDQUOT: Self = Self {
        value: libc::EDQUOT,
    };
    pub const EEXIST: Self = Self {
        value: libc::EEXIST,
    };
    pub const EFAULT: Self = Self {
        value: libc::EFAULT,
    };
    pub const EFBIG: Self = Self { value: libc::EFBIG };
    pub const EHOSTUNREACH: Self = Self {
        value: libc::EHOSTUNREACH,
    };
    pub const EIDRM: Self = Self { value: libc::EIDRM };
    pub const EILSEQ: Self = Self {
        value: libc::EILSEQ,
    };
    pub const EINPROGRESS: Self = Self {
        value: libc::EINPROGRESS,
    };
    pub const EINTR: Self = Self { value: libc::EINTR };
    pub const EINVAL: Self = Self {
        value: libc::EINVAL,
    };
    pub const EIO: Self = Self { value: libc::EIO };
    pub const EISCONN: Self = Self {
        value: libc::EISCONN,
    };
    pub const EISDIR: Self = Self {
        value: libc::EISDIR,
    };
    pub const ELOOP: Self = Self { value: libc::ELOOP };
    pub const EMFILE: Self = Self {
        value: libc::EMFILE,
    };
    pub const EMLINK: Self = Self {
        value: libc::EMLINK,
    };
    pub const EMSGSIZE: Self = Self {
        value: libc::EMSGSIZE,
    };
    pub const EMULTIHOP: Self = Self {
        value: libc::EMULTIHOP,
    };
    pub const ENAMETOOLONG: Self = Self {
        value: libc::ENAMETOOLONG,
    };
    pub const ENETDOWN: Self = Self {
        value: libc::ENETDOWN,
    };
    pub const ENETRESET: Self = Self {
        value: libc::ENETRESET,
    };
    pub const ENETUNREACH: Self = Self {
        value: libc::ENETUNREACH,
    };
    pub const ENFILE: Self = Self {
        value: libc::ENFILE,
    };
    pub const ENOBUFS: Self = Self {
        value: libc::ENOBUFS,
    };
    pub const ENODEV: Self = Self {
        value: libc::ENODEV,
    };
    pub const ENOENT: Self = Self {
        value: libc::ENOENT,
    };
    pub const ENOEXEC: Self = Self {
        value: libc::ENOEXEC,
    };
    pub const ENOLCK: Self = Self {
        value: libc::ENOLCK,
    };
    pub const ENOLINK: Self = Self {
        value: libc::ENOLINK,
    };
    pub const ENOMEM: Self = Self {
        value: libc::ENOMEM,
    };
    pub const ENOMSG: Self = Self {
        value: libc::ENOMSG,
    };
    pub const ENOPROTOOPT: Self = Self {
        value: libc::ENOPROTOOPT,
    };
    pub const ENOSPC: Self = Self {
        value: libc::ENOSPC,
    };
    pub const ENOSYS: Self = Self {
        value: libc::ENOSYS,
    };
    pub const ENOTCONN: Self = Self {
        value: libc::ENOTCONN,
    };
    pub const ENOTDIR: Self = Self {
        value: libc::ENOTDIR,
    };
    pub const ENOTEMPTY: Self = Self {
        value: libc::ENOTEMPTY,
    };
    pub const ENOTRECOVERABLE: Self = Self {
        value: libc::ENOTRECOVERABLE,
    };
    pub const ENOTSOCK: Self = Self {
        value: libc::ENOTSOCK,
    };
    pub const ENOTSUP: Self = Self {
        value: libc::ENOTSUP,
    };
    pub const ENOTTY: Self = Self {
        value: libc::ENOTTY,
    };
    pub const ENXIO: Self = Self { value: libc::ENXIO };
    pub const EOPNOTSUPP: Self = Self {
        value: libc::EOPNOTSUPP,
    };
    pub const EOVERFLOW: Self = Self {
        value: libc::EOVERFLOW,
    };
    pub const EOWNERDEAD: Self = Self {
        value: libc::EOWNERDEAD,
    };
    pub const EPERM: Self = Self { value: libc::EPERM };
    pub const EPIPE: Self = Self { value: libc::EPIPE };
    pub const EPROTO: Self = Self {
        value: libc::EPROTO,
    };
    pub const EPROTONOSUPPORT: Self = Self {
        value: libc::EPROTONOSUPPORT,
    };
    pub const EPROTOTYPE: Self = Self {
        value: libc::EPROTOTYPE,
    };
    pub const ERANGE: Self = Self {
        value: libc::ERANGE,
    };
    pub const EROFS: Self = Self { value: libc::EROFS };
    pub const ESOCKTNOSUPPORT: Self = Self {
        value: libc::ESOCKTNOSUPPORT,
    };
    pub const ESPIPE: Self = Self {
        value: libc::ESPIPE,
    };
    pub const ESRCH: Self = Self { value: libc::ESRCH };
    pub const ESTALE: Self = Self {
        value: libc::ESTALE,
    };
    pub const ETIMEDOUT: Self = Self {
        value: libc::ETIMEDOUT,
    };
    pub const ETXTBSY: Self = Self {
        value: libc::ETXTBSY,
    };
    pub const EWOULDBLOCK: Self = Self {
        value: libc::EWOULDBLOCK,
    };
    pub const EXDEV: Self = Self { value: libc::EXDEV };
}

impl Debug for Errno {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // some variants have the same value, but it's not guaranteed by the
        // standard
        #[allow(unreachable_patterns, clippy::match_overlapping_arm)]
        match self.value {
            libc::E2BIG => write!(f, "E2BIG: argument list too long"),
            libc::EACCES => write!(f, "EACCESS: permission denied"),
            libc::EADDRINUSE => write!(f, "EADDRINUSE: address in use"),
            libc::EADDRNOTAVAIL => write!(f, "EADDRNOTAVAILABLE: address not available"),
            libc::EAGAIN => write!(f, "EAGAIN: resource unavailable, try again"),
            libc::EALREADY => write!(f, "EALREADY: connection already in progress"),
            libc::EBADF => write!(f, "EBADF: bad file descriptor"),
            libc::EBADMSG => write!(f, "EBADMSG: bad message"),
            libc::EBUSY => write!(f, "EBUSY: device or resource busy"),
            libc::ECANCELED => write!(f, "ECANCELED: operation canceled"),
            libc::ECHILD => write!(f, "ECHILD: no child processes"),
            libc::ECONNABORTED => write!(f, "ECONNABORTED: connection aborted"),
            libc::ECONNREFUSED => write!(f, "ECONNREFUSED: connection refused"),
            libc::ECONNRESET => write!(f, "ECONNRESET: connection reset"),
            libc::EDEADLK => write!(f, "EDEADLK: resource deadlock would occur"),
            libc::EDESTADDRREQ => write!(f, "EDESTADDRREQ: destination address required"),
            libc::EDOM => write!(f, "EDOM: mathematics argument out of domain of function"),
            libc::EDQUOT => write!(f, "EDQUOT: disk quota exceeded"),
            libc::EEXIST => write!(f, "EEXIST: file exists"),
            libc::EFAULT => write!(f, "EFAULT: bad address"),
            libc::EFBIG => write!(f, "EFBIG: file too large"),
            libc::EHOSTUNREACH => write!(f, "EHOSTUNREACH: host is unreachable"),
            libc::EIDRM => write!(f, "EIDRM: identifier removed"),
            libc::EILSEQ => write!(f, "EILSEQ: illegal byte sequence"),
            libc::EINPROGRESS => write!(f, "EINPROGRESS: operation in progress"),
            libc::EINTR => write!(f, "EINTR: interrupted function"),
            libc::EINVAL => write!(f, "EINVAL: invalid argument"),
            libc::EIO => write!(f, "EIO: I/O error"),
            libc::EISCONN => write!(f, "EISCONN: socket is connected"),
            libc::EISDIR => write!(f, "EISDIR: is a directory"),
            libc::ELOOP => write!(f, "ELOOP: too many levels of symbolic links"),
            libc::EMFILE => write!(f, "EMFILE: file descriptor value too large"),
            libc::EMLINK => write!(f, "EMLINK: too many hard links"),
            libc::EMSGSIZE => write!(f, "EMSGSIZE: message too large"),
            libc::EMULTIHOP => write!(f, "EMULTIHOP: multihop attempted"),
            libc::ENAMETOOLONG => write!(f, "ENAMETOOLONG: filename too long"),
            libc::ENETDOWN => write!(f, "ENETDOWN: network is down"),
            libc::ENETRESET => write!(f, "ENETRESET: connection aborted by network"),
            libc::ENETUNREACH => write!(f, "ENETUNREACH: network unreachable"),
            libc::ENFILE => write!(f, "ENFILE: too many files open in system"),
            libc::ENOBUFS => write!(f, "ENOBUFS: no buffer space available"),
            libc::ENODEV => write!(f, "ENODEV: no such device"),
            libc::ENOENT => write!(f, "ENOENT: no such file or directory"),
            libc::ENOEXEC => write!(f, "ENOEXEC: executable file format error"),
            libc::ENOLCK => write!(f, "ENOLCK: no locks available"),
            libc::ENOLINK => write!(f, "ENOLINK: link has been severed"),
            libc::ENOMEM => write!(f, "ENOMEM: not enough space"),
            libc::ENOMSG => write!(f, "ENOMSG: no message of the desired type"),
            libc::ENOPROTOOPT => write!(f, "ENOPROTOOPT: protocol not available"),
            libc::ENOSPC => write!(f, "ENOSPC: no space left on device"),
            libc::ENOSYS => write!(f, "ENOSYS: functionality not supported"),
            libc::ENOTCONN => write!(f, "ENOTCONN: the socket is not connected"),
            libc::ENOTDIR => write!(
                f,
                "ENOTDIR: not a directory or a symbolic link to a directory"
            ),
            libc::ENOTEMPTY => write!(f, "ENOTEMPTY: directory not empty"),
            libc::ENOTRECOVERABLE => write!(f, "ENOTRECOVERABLE: state not recoverable"),
            libc::ENOTSOCK => write!(f, "ENOTSOCK: not a socket"),
            libc::ENOTSUP => write!(f, "ENOTSUP: not supported"),
            libc::ENOTTY => write!(f, "ENOTTY: inappropriate I/O control operation"),
            libc::ENXIO => write!(f, "ENXIO: no such device or address"),
            libc::EOPNOTSUPP => write!(f, "EOPNOTSUPP: operation not supported on socket"),
            libc::EOVERFLOW => write!(f, "EOVERFLOW: value too large to be stored in data type"),
            libc::EOWNERDEAD => write!(f, "EOWNERDEAD: previous owner died"),
            libc::EPERM => write!(f, "EPERM: operation not permitted"),
            libc::EPIPE => write!(f, "EPIPE: broken pipe"),
            libc::EPROTO => write!(f, "EPROTO: protocol error"),
            libc::EPROTONOSUPPORT => write!(f, "EPROTONOSUPPORT: protocol not supported"),
            libc::EPROTOTYPE => write!(f, "EPROTOTYPE: protocol wrong type for socket"),
            libc::ERANGE => write!(f, "ERANGE: result too large"),
            libc::EROFS => write!(f, "EROFS: read-only file system"),
            libc::ESOCKTNOSUPPORT => write!(f, "ESOCKTNOSUPPORT: socket type not supported"),
            libc::ESPIPE => write!(f, "ESPIPE: invalid seek"),
            libc::ESRCH => write!(f, "ESRCH: no such process"),
            libc::ESTALE => write!(f, "ESTALE: stale file handle"),
            libc::ETIMEDOUT => write!(f, "ETIMEDOUT: connection timed out"),
            libc::ETXTBSY => write!(f, "ETXTBSY: text file busy"),
            libc::EWOULDBLOCK => write!(f, "EWOULDBLOCK: operation would block"),
            libc::EXDEV => write!(f, "EXDEV: improper hard link"),
            other => unreachable!("invalid errno value {}", other),
        }
    }
}

impl Display for Errno {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl TryFrom<libc::c_int> for Errno {
    type Error = ();
    fn try_from(value: libc::c_int) -> Result<Self, Self::Error> {
        // some variants have the same value, but its not guaranteed by the
        // standard
        #[allow(unreachable_patterns, clippy::match_overlapping_arm)]
        match value {
            libc::E2BIG => Ok(Self::E2BIG),
            libc::EACCES => Ok(Self::EACCES),
            libc::EADDRINUSE => Ok(Self::EADDRINUSE),
            libc::EADDRNOTAVAIL => Ok(Self::EADDRNOTAVAIL),
            libc::EAGAIN => Ok(Self::EAGAIN),
            libc::EALREADY => Ok(Self::EALREADY),
            libc::EBADF => Ok(Self::EBADF),
            libc::EBADMSG => Ok(Self::EBADMSG),
            libc::EBUSY => Ok(Self::EBUSY),
            libc::ECANCELED => Ok(Self::ECANCELED),
            libc::ECHILD => Ok(Self::ECHILD),
            libc::ECONNABORTED => Ok(Self::ECONNABORTED),
            libc::ECONNREFUSED => Ok(Self::ECONNREFUSED),
            libc::ECONNRESET => Ok(Self::ECONNRESET),
            libc::EDEADLK => Ok(Self::EDEADLK),
            libc::EDESTADDRREQ => Ok(Self::EDESTADDRREQ),
            libc::EDOM => Ok(Self::EDOM),
            libc::EDQUOT => Ok(Self::EDQUOT),
            libc::EEXIST => Ok(Self::EEXIST),
            libc::EFAULT => Ok(Self::EFAULT),
            libc::EFBIG => Ok(Self::EFBIG),
            libc::EHOSTUNREACH => Ok(Self::EHOSTUNREACH),
            libc::EIDRM => Ok(Self::EIDRM),
            libc::EILSEQ => Ok(Self::EILSEQ),
            libc::EINPROGRESS => Ok(Self::EINPROGRESS),
            libc::EINTR => Ok(Self::EINTR),
            libc::EINVAL => Ok(Self::EINVAL),
            libc::EIO => Ok(Self::EIO),
            libc::EISCONN => Ok(Self::EISCONN),
            libc::EISDIR => Ok(Self::EISDIR),
            libc::ELOOP => Ok(Self::ELOOP),
            libc::EMFILE => Ok(Self::EMFILE),
            libc::EMLINK => Ok(Self::EMLINK),
            libc::EMSGSIZE => Ok(Self::EMSGSIZE),
            libc::EMULTIHOP => Ok(Self::EMULTIHOP),
            libc::ENAMETOOLONG => Ok(Self::ENAMETOOLONG),
            libc::ENETDOWN => Ok(Self::ENETDOWN),
            libc::ENETRESET => Ok(Self::ENETRESET),
            libc::ENETUNREACH => Ok(Self::ENETUNREACH),
            libc::ENFILE => Ok(Self::ENFILE),
            libc::ENOBUFS => Ok(Self::ENOBUFS),
            libc::ENODEV => Ok(Self::ENODEV),
            libc::ENOENT => Ok(Self::ENOENT),
            libc::ENOEXEC => Ok(Self::ENOEXEC),
            libc::ENOLCK => Ok(Self::ENOLCK),
            libc::ENOLINK => Ok(Self::ENOLINK),
            libc::ENOMEM => Ok(Self::ENOMEM),
            libc::ENOMSG => Ok(Self::ENOMSG),
            libc::ENOPROTOOPT => Ok(Self::ENOPROTOOPT),
            libc::ENOSPC => Ok(Self::ENOSPC),
            libc::ENOSYS => Ok(Self::ENOSYS),
            libc::ENOTCONN => Ok(Self::ENOTCONN),
            libc::ENOTDIR => Ok(Self::ENOTDIR),
            libc::ENOTEMPTY => Ok(Self::ENOTEMPTY),
            libc::ENOTRECOVERABLE => Ok(Self::ENOTRECOVERABLE),
            libc::ENOTSOCK => Ok(Self::ENOTSOCK),
            libc::ENOTSUP => Ok(Self::ENOTSUP),
            libc::ENOTTY => Ok(Self::ENOTTY),
            libc::ENXIO => Ok(Self::ENXIO),
            libc::EOPNOTSUPP => Ok(Self::EOPNOTSUPP),
            libc::EOVERFLOW => Ok(Self::EOVERFLOW),
            libc::EOWNERDEAD => Ok(Self::EOWNERDEAD),
            libc::EPERM => Ok(Self::EPERM),
            libc::EPIPE => Ok(Self::EPIPE),
            libc::EPROTO => Ok(Self::EPROTO),
            libc::EPROTONOSUPPORT => Ok(Self::EPROTONOSUPPORT),
            libc::EPROTOTYPE => Ok(Self::EPROTOTYPE),
            libc::ERANGE => Ok(Self::ERANGE),
            libc::EROFS => Ok(Self::EROFS),
            libc::ESOCKTNOSUPPORT => Ok(Self::ESOCKTNOSUPPORT),
            libc::ESPIPE => Ok(Self::ESPIPE),
            libc::ESRCH => Ok(Self::ESRCH),
            libc::ESTALE => Ok(Self::ESTALE),
            libc::ETIMEDOUT => Ok(Self::ETIMEDOUT),
            libc::ETXTBSY => Ok(Self::ETXTBSY),
            libc::EWOULDBLOCK => Ok(Self::EWOULDBLOCK),
            libc::EXDEV => Ok(Self::EXDEV),
            _ => Err(()),
        }
    }
}

pub fn get_current_errno_value() -> Errno {
    // guaranteed to be some, unwrap is safe
    let errno = std::io::Error::last_os_error().raw_os_error().unwrap();
    errno.try_into().expect("invalid errno value")
}
