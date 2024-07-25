use rand::{distributions::Standard, rngs::StdRng, SeedableRng};
use std::{fs, path::PathBuf};

// Tests if the algorithm properly closes all file descriptors. Must run by itself to avoid counting
// file descriptors opened in other test threads.
#[cfg(target_os = "linux")]
#[test]
fn test_walkdir_fd_raii() {
    use rand::Rng;

    let test_dir = &format!("{}/test_walkdir_fd_raii", env!("CARGO_TARGET_TMPDIR"));

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

    plib::walkdir::traverse_directory(test_dir, |_| Ok(true), |_| Ok(()), |_, _| {}, false, false);

    let final_fd_count = count_open_fds();
    assert_eq!(starting_fd_count, final_fd_count);

    fs::remove_dir_all(test_dir).unwrap();
}

fn count_open_fds() -> usize {
    let pid = unsafe { libc::getpid() };
    fs::read_dir(format!("/proc/{pid}/fd")).unwrap().count()
}
