use rand::{distributions::Standard, rngs::StdRng, Rng, SeedableRng};
use std::{ffi::CString, fs, io, os::fd::AsRawFd, path::Path, path::PathBuf, sync::Mutex};
use tempfile::TempDir;

static GLOBAL_MUTEX: Mutex<()> = Mutex::new(());

/// Helper struct that cleans up deep directory hierarchies on drop using unlinkat().
/// This is needed for the very deep test hierarchies that standard fs::remove_dir_all() can't handle.
struct DeepDirCleanup {
    test_dir: TempDir,
    dir_name: CString,
    depth: usize,
}

impl DeepDirCleanup {
    fn new(prefix: &str, dir_name_str: &str, depth: usize) -> io::Result<Self> {
        let test_dir = tempfile::Builder::new()
            .prefix(prefix)
            .tempdir_in(env!("CARGO_TARGET_TMPDIR"))?;
        let dir_name = CString::new(dir_name_str.as_bytes()).unwrap();
        Ok(Self {
            test_dir,
            dir_name,
            depth,
        })
    }

    fn path(&self) -> &Path {
        self.test_dir.path()
    }
}

impl Drop for DeepDirCleanup {
    fn drop(&mut self) {
        // Try standard removal first (works for shallow hierarchies)
        if fs::remove_dir_all(self.test_dir.path()).is_ok() {
            // Prevent TempDir from trying to remove again
            if let Ok(new_tmp) = tempfile::tempdir() {
                let _ = std::mem::replace(&mut self.test_dir, new_tmp);
            }
            return;
        }

        // Fall back to manual unlinkat() approach for deep hierarchies.
        // The structure is: test_dir/x/x/x/.../x (depth-1 nested 'x' directories inside test_dir)
        //
        // To remove, we navigate to the second-to-last directory (depth-2),
        // then remove from the deepest working back up.
        let test_dir_name =
            CString::new(self.test_dir.path().to_str().unwrap().as_bytes()).unwrap();

        let mut fd = ftw::FileDescriptor::cwd();

        // Navigate down to depth-2 (the parent of the deepest directory).
        // At i=0 we open test_dir (depth 1)
        // At i=1 we open first x (depth 2)
        // ...
        // At i=depth-3 we open x at depth-2
        // This way we end up at the parent of the deepest 'x'
        for i in 0..(self.depth.saturating_sub(2)) {
            let filename = if i == 0 {
                &test_dir_name
            } else {
                &self.dir_name
            };
            match ftw::FileDescriptor::open_at(&fd, filename, libc::O_RDONLY) {
                Ok(new_fd) => {
                    fd = new_fd;
                }
                _ => {
                    // Can't descend further - might already be cleaned up
                    return;
                }
            }
        }

        // Now fd is at depth-2. Remove directories from deepest to shallowest.
        // depth-1 is the deepest 'x', which we unlink from fd (at depth-2)
        // Then go up to depth-3 and unlink the 'x' at depth-2
        // Continue until we unlink test_dir from cwd
        for remaining in (0..self.depth).rev() {
            let (filename, raw_fd) = if remaining == 0 {
                // Unlinking test_dir itself from cwd
                (&test_dir_name, libc::AT_FDCWD)
            } else {
                // Unlinking an 'x' directory from fd
                (&self.dir_name, fd.as_raw_fd())
            };

            unsafe {
                libc::unlinkat(raw_fd, filename.as_ptr(), libc::AT_REMOVEDIR);
            }

            // Move fd up one level (except when we just unlinked test_dir)
            if remaining > 1 {
                match ftw::FileDescriptor::open_at(&fd, c"..", libc::O_RDONLY) {
                    Ok(parent_fd) => {
                        fd = parent_fd;
                    }
                    _ => {
                        break;
                    }
                }
            }
        }

        // Prevent TempDir from trying to remove (we already did, or at least tried)
        if let Ok(new_tmp) = tempfile::tempdir() {
            let _ = std::mem::replace(&mut self.test_dir, new_tmp);
        }
    }
}

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

    let tmp_dir = tempfile::Builder::new()
        .prefix("test_ftw_fd_raii")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let test_dir = tmp_dir.path().to_str().unwrap();

    let starting_fd_count = count_open_fds();

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
    // Cleanup happens automatically when tmp_dir is dropped
}

// Tests if the slow path that handles the "too many open files" error is not leaking file
// descriptors.
// Note: This test requires adequate stack size and file descriptor limits.
// Run with: RUST_MIN_STACK=16777216 cargo test -p ftw
// And ensure ulimit -n is at least 2048.
#[test]
fn test_ftw_too_many_open_files() {
    let _guard = GLOBAL_MUTEX.lock();

    const DEFAULT_FD_LIMIT: usize = 1024;
    const DIR_HIERARCHY_DEPTH: usize = DEFAULT_FD_LIMIT + 50;

    // Spawn a thread with a larger stack to avoid stack overflow
    let result = std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024) // 16MB stack
        .spawn(move || {
            let dir_name_str = ["x"; 200].join("");
            let cleanup = DeepDirCleanup::new(
                "test_ftw_too_many_open_files",
                &dir_name_str,
                DIR_HIERARCHY_DEPTH,
            )
            .unwrap();
            let test_dir = cleanup.path().to_str().unwrap().to_string();

            let dir_name = CString::new(dir_name_str.as_bytes()).unwrap();

            {
                let mut fd = ftw::FileDescriptor::open_at(
                    &ftw::FileDescriptor::cwd(),
                    &CString::new(test_dir.as_bytes()).unwrap(),
                    libc::O_RDONLY,
                )
                .unwrap();

                // Create nested directories inside the temp dir (starting at depth 1)
                for _ in 1..DIR_HIERARCHY_DEPTH {
                    let ret = unsafe { libc::mkdirat(fd.as_raw_fd(), dir_name.as_ptr(), 0o755) };
                    if ret != 0 {
                        panic!("{}", io::Error::last_os_error());
                    }

                    fd = ftw::FileDescriptor::open_at(&fd, &dir_name, libc::O_RDONLY).unwrap();
                }
            }

            let starting_fd_count = count_open_fds();

            let mut dir_count = 0;

            ftw::traverse_directory(
                &test_dir,
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
            // Cleanup happens automatically when cleanup is dropped via DeepDirCleanup
        })
        .unwrap()
        .join();

    result.unwrap();
}
