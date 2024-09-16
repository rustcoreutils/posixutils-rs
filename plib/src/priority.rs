use crate::platform::PPriorityWhichT;
use std::io;

pub fn getpriority(which: u32, id: u32) -> io::Result<i32> {
    errno::set_errno(errno::Errno(0));

    let res = unsafe { libc::getpriority(which as PPriorityWhichT, id) };

    let errno_res = errno::errno().0;
    if errno_res == 0 {
        Ok(res)
    } else {
        let e = io::Error::from_raw_os_error(errno_res);
        eprintln!("getpriority: {e}");
        Err(e)
    }
}

pub fn setpriority(which: u32, id: u32, prio: i32) -> io::Result<()> {
    let res = unsafe { libc::setpriority(which as PPriorityWhichT, id, prio) };

    if res < 0 {
        let e = io::Error::last_os_error();
        eprintln!("setpriority: {e}");
        Err(e)
    } else {
        Ok(())
    }
}
