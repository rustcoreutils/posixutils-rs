//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! What make has learned about the file system since it last ran a command.
//!
//! Deciding that a tree is up to date asks the same questions many times:
//! whether a target exists, when it was modified, whether an inference rule's
//! source is there, and each through `VPATH`.  Without a cache a no-op run
//! of an automake directory made about twenty times as many `stat` calls as
//! GNU make.
//!
//! The answers stay true until a command runs, and nothing but a command
//! changes the tree under make, so every entry is dropped whenever one does
//! (`invalidate`): a recipe, an SCCS retrieval, or `-t` touching a target.
//! GNU make discards its directory cache on the same event.
//!
//! Under `-j` a command may finish while another thread is asking: each
//! answer is filed only if no command finished between the question and
//! the answer, so a result read from before a command ran can never outlive
//! it in the cache.

use crate::fast_hash::Map as HashMap;
use std::fs;
use std::sync::Mutex;
use std::time::SystemTime;

/// What a path named, as far as make cares.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Stat {
    Missing,
    /// It exists; with its modification time, when there is one.
    Present(Option<SystemTime>),
}

#[derive(Default)]
struct Inner {
    /// Bumped by every `invalidate`.
    generation: u64,
    entries: HashMap<String, Stat>,
}

/// Cached `stat` results, dropped whenever a command runs.
#[derive(Default)]
pub struct StatCache {
    inner: Mutex<Inner>,
}

impl StatCache {
    /// Whether `path` names an existing file, following symbolic links as
    /// `Path::exists` does.
    pub fn exists(&self, path: &str) -> bool {
        self.lookup(path) != Stat::Missing
    }

    /// The modification time of `path`, or `None` if it does not exist.
    ///
    /// An `archive(member)` name is the member's time inside the archive.
    pub fn modified(&self, path: &str) -> Option<SystemTime> {
        match self.lookup(path) {
            Stat::Present(mtime) => mtime,
            Stat::Missing => None,
        }
    }

    /// Forget everything: a command ran and may have changed any file.
    pub fn invalidate(&self) {
        let mut inner = self.inner.lock().unwrap();
        inner.generation += 1;
        inner.entries.clear();
    }

    fn lookup(&self, path: &str) -> Stat {
        let generation = {
            let inner = self.inner.lock().unwrap();
            if let Some(stat) = inner.entries.get(path) {
                return *stat;
            }
            inner.generation
        };
        // The lock is not held across the system call, so that `-j` threads
        // do not queue behind one another's `stat`.
        let stat = stat_path(path);
        let mut inner = self.inner.lock().unwrap();
        if inner.generation == generation {
            inner.entries.insert(path.to_string(), stat);
        }
        stat
    }
}

fn stat_path(path: &str) -> Stat {
    if let Some((archive, member)) = crate::parse_archive_target(path) {
        return match crate::archive_member_mtime(archive, member) {
            Some(mtime) => Stat::Present(Some(mtime)),
            None => Stat::Missing,
        };
    }
    match fs::metadata(path) {
        Ok(meta) => Stat::Present(meta.modified().ok()),
        Err(_) => Stat::Missing,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scratch(name: &str) -> String {
        let dir = std::env::temp_dir().join(format!(
            "posixutils-make-stat-cache-{}-{name}",
            std::process::id()
        ));
        let _ = fs::remove_file(&dir);
        dir.to_string_lossy().into_owned()
    }

    #[test]
    fn answers_are_kept_until_invalidated() {
        let path = scratch("kept");
        let cache = StatCache::default();
        assert!(!cache.exists(&path));
        fs::write(&path, "").unwrap();
        // Still the cached answer: no command was said to have run.
        assert!(!cache.exists(&path));
        cache.invalidate();
        assert!(cache.exists(&path));
        assert!(cache.modified(&path).is_some());
        fs::remove_file(&path).unwrap();
        cache.invalidate();
        assert!(!cache.exists(&path));
        assert_eq!(cache.modified(&path), None);
    }

    #[test]
    fn a_new_modification_time_is_seen_after_invalidate() {
        let path = scratch("mtime");
        fs::write(&path, "").unwrap();
        let cache = StatCache::default();
        let old = SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(1_000_000);
        fs::File::options()
            .write(true)
            .open(&path)
            .unwrap()
            .set_modified(old)
            .unwrap();
        assert_eq!(cache.modified(&path), Some(old));
        let new = old + std::time::Duration::from_secs(60);
        fs::File::options()
            .write(true)
            .open(&path)
            .unwrap()
            .set_modified(new)
            .unwrap();
        cache.invalidate();
        assert_eq!(cache.modified(&path), Some(new));
        fs::remove_file(&path).unwrap();
    }
}
