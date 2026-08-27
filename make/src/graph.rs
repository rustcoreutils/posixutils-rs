//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Build-graph state: which targets are done, which are being built, and
//! whether the graph has a cycle.
//!
//! One mechanism gives three properties. A target moves `Unvisited ->
//! InProgress -> Done` and only the thread that observes `Unvisited` performs
//! the transition, so:
//!
//! * a target's recipe runs exactly once, even under `-j` (audit #29);
//! * a second visit replays the recorded outcome instead of re-walking the
//!   subgraph (audit #31);
//! * and the cycle check is the same colouring, run once up front (audit #28).
//!
//! Running the cycle check before any worker exists is what makes the wait
//! below deadlock-free: a thread only ever blocks on a target that is an
//! ancestor-free descendant of its own, and a cycle is the only way that could
//! close back on itself.

use crate::error_code::ErrorCode;
use std::collections::{HashMap, HashSet};
use std::sync::{Condvar, Mutex};

/// What became of a target.
#[derive(Debug, Clone)]
pub enum Outcome {
    /// Its recipe ran.
    Built,
    /// It was already up to date; nothing ran.
    UpToDate,
    /// It could not be remade.
    Failed(ErrorCode),
}

impl Outcome {
    /// Render back into the `build_target` signature.
    pub fn into_result(self) -> Result<bool, ErrorCode> {
        match self {
            Outcome::Built => Ok(true),
            Outcome::UpToDate => Ok(false),
            Outcome::Failed(err) => Err(err),
        }
    }

    pub fn from_result(result: &Result<bool, ErrorCode>) -> Self {
        match result {
            Ok(true) => Outcome::Built,
            Ok(false) => Outcome::UpToDate,
            Err(err) => Outcome::Failed(err.clone()),
        }
    }
}

enum Entry {
    /// A thread is running this target's recipe.
    Running,
    /// It finished, with this outcome.
    Finished(Outcome),
}

/// What a caller should do after claiming a target.
pub enum Claim {
    /// This thread owns the build; it must call `finish` when done.
    Build,
    /// Another thread already produced this outcome.
    Done(Outcome),
}

/// Tracks the state of every target across the build, including under `-j`.
#[derive(Default)]
pub struct Ledger {
    entries: Mutex<HashMap<String, Entry>>,
    finished: Condvar,
}

impl Ledger {
    pub fn new() -> Self {
        Self::default()
    }

    /// Take ownership of a target, or wait for whoever already has it.
    ///
    /// Blocking happens only on `Running`, and the pre-build cycle check
    /// guarantees the waited-for target cannot transitively depend on the
    /// waiter, so this cannot deadlock.
    pub fn claim(&self, target: &str) -> Claim {
        let mut entries = self.entries.lock().unwrap();
        loop {
            match entries.get(target) {
                Some(Entry::Finished(outcome)) => return Claim::Done(outcome.clone()),
                Some(Entry::Running) => {
                    entries = self.finished.wait(entries).unwrap();
                }
                None => {
                    entries.insert(target.to_string(), Entry::Running);
                    return Claim::Build;
                }
            }
        }
    }

    /// Publish a target's outcome and wake anyone waiting on it.
    pub fn finish(&self, target: &str, result: &Result<bool, ErrorCode>) {
        let mut entries = self.entries.lock().unwrap();
        entries.insert(
            target.to_string(),
            Entry::Finished(Outcome::from_result(result)),
        );
        drop(entries);
        self.finished.notify_all();
    }
}

/// A target's direct prerequisites, for cycle detection.
pub trait Edges {
    fn prerequisites_of(&self, target: &str) -> Vec<String>;
}

/// Find a dependency cycle reachable from `root`, returning the target the
/// cycle closes on.
///
/// Iterative, so a deep or cyclic graph reports an error rather than
/// overflowing the stack (audit #28). The previous version seeded its visited
/// and stack sets with the root and never inserted during the walk, so only a
/// cycle passing back through the root was caught.
pub fn find_cycle(graph: &impl Edges, root: &str) -> Option<String> {
    // (target, next prerequisite index)
    let mut stack: Vec<(String, usize)> = vec![(root.to_string(), 0)];
    let mut on_stack: HashSet<String> = HashSet::from([root.to_string()]);
    let mut done: HashSet<String> = HashSet::new();
    let mut children: HashMap<String, Vec<String>> =
        HashMap::from([(root.to_string(), graph.prerequisites_of(root))]);

    while let Some((target, index)) = stack.pop() {
        let prerequisites = children.get(&target).cloned().unwrap_or_default();
        if index < prerequisites.len() {
            let child = prerequisites[index].clone();
            stack.push((target, index + 1));
            if on_stack.contains(&child) {
                return Some(child);
            }
            if done.contains(&child) {
                continue;
            }
            children
                .entry(child.clone())
                .or_insert_with(|| graph.prerequisites_of(&child));
            on_stack.insert(child.clone());
            stack.push((child, 0));
        } else {
            on_stack.remove(&target);
            done.insert(target);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Fixture(HashMap<&'static str, Vec<&'static str>>);

    impl Edges for Fixture {
        fn prerequisites_of(&self, target: &str) -> Vec<String> {
            self.0
                .get(target)
                .map(|v| v.iter().map(|s| s.to_string()).collect())
                .unwrap_or_default()
        }
    }

    fn graph(edges: &[(&'static str, Vec<&'static str>)]) -> Fixture {
        Fixture(edges.iter().cloned().collect())
    }

    #[test]
    fn acyclic_graph_has_no_cycle() {
        let g = graph(&[("a", vec!["b", "c"]), ("b", vec!["d"]), ("c", vec!["d"])]);
        assert_eq!(find_cycle(&g, "a"), None);
    }

    #[test]
    fn direct_cycle_is_found() {
        let g = graph(&[("a", vec!["b"]), ("b", vec!["a"])]);
        assert!(find_cycle(&g, "a").is_some());
    }

    // Audit #28: this is the shape that used to overflow the stack, because
    // the cycle does not pass back through the root.
    #[test]
    fn indirect_cycle_not_through_the_root_is_found() {
        let g = graph(&[("a", vec!["b"]), ("b", vec!["c"]), ("c", vec!["b"])]);
        assert_eq!(find_cycle(&g, "a"), Some("b".to_string()));
    }

    #[test]
    fn self_cycle_is_found() {
        let g = graph(&[("a", vec!["a"])]);
        assert_eq!(find_cycle(&g, "a"), Some("a".to_string()));
    }

    // A diamond visits `d` by two paths but is not a cycle. The old code's
    // never-populated visited set made this quadratic; here it is linear.
    #[test]
    fn diamond_is_not_a_cycle() {
        let g = graph(&[
            ("a", vec!["b", "c"]),
            ("b", vec!["d"]),
            ("c", vec!["d"]),
            ("d", vec![]),
        ]);
        assert_eq!(find_cycle(&g, "a"), None);
    }

    #[test]
    fn deep_chain_does_not_overflow() {
        let names: Vec<String> = (0..10_000).map(|i| format!("n{i}")).collect();
        struct Chain(usize);
        impl Edges for Chain {
            fn prerequisites_of(&self, target: &str) -> Vec<String> {
                let i: usize = target[1..].parse().unwrap_or(self.0);
                if i + 1 < self.0 {
                    vec![format!("n{}", i + 1)]
                } else {
                    vec![]
                }
            }
        }
        assert_eq!(find_cycle(&Chain(names.len()), "n0"), None);
    }

    #[test]
    fn ledger_replays_a_finished_outcome() {
        let ledger = Ledger::new();
        assert!(matches!(ledger.claim("t"), Claim::Build));
        ledger.finish("t", &Ok(true));
        match ledger.claim("t") {
            Claim::Done(Outcome::Built) => {}
            _ => panic!("second claim must replay the outcome"),
        }
    }

    #[test]
    fn ledger_replays_a_failure() {
        let ledger = Ledger::new();
        assert!(matches!(ledger.claim("t"), Claim::Build));
        ledger.finish("t", &Err(ErrorCode::NoMakefile));
        match ledger.claim("t") {
            Claim::Done(Outcome::Failed(_)) => {}
            _ => panic!("failures must be replayed too"),
        }
    }

    // Audit #29: two threads racing for the same target -- exactly one builds.
    #[test]
    fn only_one_thread_builds_a_target() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        let ledger = Ledger::new();
        let builds = AtomicUsize::new(0);
        std::thread::scope(|scope| {
            for _ in 0..8 {
                scope.spawn(|| match ledger.claim("shared") {
                    Claim::Build => {
                        builds.fetch_add(1, Ordering::SeqCst);
                        std::thread::sleep(std::time::Duration::from_millis(20));
                        ledger.finish("shared", &Ok(true));
                    }
                    Claim::Done(_) => {}
                });
            }
        });
        assert_eq!(builds.load(Ordering::SeqCst), 1);
    }
}
