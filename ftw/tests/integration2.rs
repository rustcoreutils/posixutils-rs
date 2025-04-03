use std::ffi::CString;
use std::os::fd::AsRawFd;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::{fs, io};

use rand::distributions::Standard;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

static GLOBAL_MUTEX: Mutex<()> = Mutex::new(());

fn count_open_fds() -> usize {
    if cfg!(target_os = "linux") {
        let pid = unsafe { libc::getpid() };
        fs::read_dir(format!("/proc/{pid}/fd")).unwrap().count()
    } else if cfg!(target_os = "macos") {
        fs::read_dir("/dev/fd").unwrap().count()
    } else {
        unimplemented!()
    }
}

// Tests if the algorithm properly closes all file descriptors. Must run by itself to avoid counting
// file descriptors opened in other test threads.
#[test]
fn test_ftw_fd_raii() {
    let _guard = GLOBAL_MUTEX.lock();

    let test_dir = &format!("{}/test_ftw_fd_raii", env!("CARGO_TARGET_TMPDIR"));

    let starting_fd_count = count_open_fds();
    fs::create_dir(test_dir).unwrap();

    // Create a directory tree
    {
        let mut rng = StdRng::seed_from_u64(1234567890);

        let mut current_dir = PathBuf::new();
        current_dir.push(test_dir);
        let mut i = 0;
        for _ in 0..500 {
            let is_dir: bool = rng.sample(Standard);
            if is_dir {
                fs::create_dir(current_dir.join(format!("{i}"))).unwrap();
                let descend: bool = rng.sample(Standard);
                if descend {
                    current_dir.push(format!("{i}"));
                    i = 0;
                } else {
                    i += 1;
                }
            } else {
                fs::File::create(current_dir.join(format!("{i}"))).unwrap();
                i += 1;
            }
        }
    }

    ftw::traverse_directory(
        test_dir,
        |_| Ok(true),
        |_| Ok(()),
        |_, _| {},
        ftw::TraverseDirectoryOpts::default(),
    );

    let final_fd_count = count_open_fds();
    assert_eq!(starting_fd_count, final_fd_count);

    fs::remove_dir_all(test_dir).unwrap();
}

// Tests if the slow path that handles the "too many open files" error is not leaking file
// descriptors.
#[test]
fn test_ftw_too_many_open_files() {
    let _guard = GLOBAL_MUTEX.lock();

    let test_dir = &format!(
        "{}/test_ftw_too_many_open_files",
        env!("CARGO_TARGET_TMPDIR")
    );

    const DEFAULT_FD_LIMIT: usize = 1024;
    const DIR_HIERARCHY_DEPTH: usize = DEFAULT_FD_LIMIT + 50;

    let test_dir_name = CString::new(test_dir.as_bytes()).unwrap();
    let dir_name = CString::new(["x"; 200].join("").as_bytes()).unwrap();

    {
        let mut fd = ftw::FileDescriptor::cwd();

        for i in 0..DIR_HIERARCHY_DEPTH {
            let filename = if i == 0 { &test_dir_name } else { &dir_name };
            let ret = unsafe { libc::mkdirat(fd.as_raw_fd(), filename.as_ptr(), 0o755) };
            if ret != 0 {
                panic!("{}", io::Error::last_os_error());
            }

            fd = ftw::FileDescriptor::open_at(&fd, filename, libc::O_RDONLY).unwrap();
        }
    }

    let starting_fd_count = count_open_fds();

    let mut dir_count = 0;

    ftw::traverse_directory(
        test_dir,
        |_| {
            dir_count += 1;
            Ok(true)
        },
        |_| Ok(()),
        |_, e| {
            assert!(e.kind() == ftw::ErrorKind::Open);
            assert_eq!(e.inner().raw_os_error(), Some(libc::EMFILE));
        },
        ftw::TraverseDirectoryOpts::default(),
    );

    assert_eq!(dir_count, DIR_HIERARCHY_DEPTH);

    let final_fd_count = count_open_fds();
    assert_eq!(starting_fd_count, final_fd_count);

    // Manually remove the directory tree
    {
        let mut fd = ftw::FileDescriptor::cwd();

        // Open the penultimate directory
        for i in 0..DIR_HIERARCHY_DEPTH - 1 {
            let filename = if i == 0 { &test_dir_name } else { &dir_name };
            fd = ftw::FileDescriptor::open_at(&fd, filename, libc::O_RDONLY).unwrap();
        }

        // Remove the directories starting from the deepest
        for i in (0..DIR_HIERARCHY_DEPTH).rev() {
            let (filename, raw_fd) = if i == 0 {
                (&test_dir_name, libc::AT_FDCWD)
            } else {
                (&dir_name, fd.as_raw_fd())
            };

            let ret = unsafe { libc::unlinkat(raw_fd, filename.as_ptr(), libc::AT_REMOVEDIR) };
            if ret != 0 {
                panic!("{}", io::Error::last_os_error());
            }

            // Go up a level in the tree
            fd = ftw::FileDescriptor::open_at(&fd, c"..", libc::O_RDONLY).unwrap();
        }

        assert!(!Path::new(test_dir).exists());
    }
}
