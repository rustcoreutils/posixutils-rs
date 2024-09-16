use crate::platform::P_WINSIZE_REQUEST_CODE;
use std::mem::MaybeUninit;

pub fn get_terminal_width() -> usize {
    // COLUMNS is usually automatically set and it even changes when the
    // terminal window is resized.
    if let Ok(s) = std::env::var("COLUMNS") {
        if let Ok(num_columns) = s.parse() {
            return num_columns;
        }
    }

    // Fallback to manually querying via `ioctl`.
    unsafe {
        let mut winsize: MaybeUninit<libc::winsize> = MaybeUninit::zeroed();
        let ret = libc::ioctl(
            libc::STDOUT_FILENO,
            P_WINSIZE_REQUEST_CODE,
            winsize.as_mut_ptr(),
        );

        // We're only interested in stdout here unlike `term_size::dimensions`
        // so we won't query further if the first `ioctl` call fails.
        if ret == 0 {
            let winsize = winsize.assume_init();
            return winsize.ws_col as usize;
        }
    }

    // Historical default terminal width is 80
    80
}
