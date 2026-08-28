//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A minimal line-level diff.
//!
//! This is Myers' O(ND) algorithm in the linear-space, divide-and-conquer
//! form: the cost is proportional to the size of the edit, not to the size of
//! the files, which suits version control where most deltas are small.
//!
//! `delta` previously used a greedy scan with a ten-line lookahead window.
//! That window is why inserting twelve lines between two unchanged ones was
//! recorded as thirteen insertions and one deletion instead of twelve
//! insertions: the anchor line sat just past where the scan could see. The
//! same scan indexed the new file unguarded when it had run off the end, so
//! deleting two or more trailing lines panicked outright.

/// One step of a diff, in order over the two inputs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineOp {
    /// The next line of the old text is unchanged.
    Keep,
    /// The next line of the old text is removed.
    Delete,
    /// The new text's line at this index is added.
    Insert(usize),
}

/// A shortest edit script turning `old` into `new`.
///
/// The result is minimal: no other script has fewer `Delete` plus `Insert`
/// steps.
pub fn diff<T: PartialEq>(old: &[T], new: &[T]) -> Vec<LineOp> {
    let mut ops = Vec::new();
    split(old, 0, old.len(), new, 0, new.len(), &mut ops);
    ops
}

/// Emit the edit script for `old[alo..ahi]` against `new[blo..bhi]`.
fn split<T: PartialEq>(
    old: &[T],
    mut alo: usize,
    mut ahi: usize,
    new: &[T],
    mut blo: usize,
    mut bhi: usize,
    ops: &mut Vec<LineOp>,
) {
    // Matching lines at either end are not part of any edit, and trimming them
    // is what guarantees the recursion below makes progress.
    while alo < ahi && blo < bhi && old[alo] == new[blo] {
        ops.push(LineOp::Keep);
        alo += 1;
        blo += 1;
    }
    let mut suffix = 0;
    while ahi > alo && bhi > blo && old[ahi - 1] == new[bhi - 1] {
        ahi -= 1;
        bhi -= 1;
        suffix += 1;
    }

    if alo == ahi {
        for j in blo..bhi {
            ops.push(LineOp::Insert(j));
        }
    } else if blo == bhi {
        for _ in alo..ahi {
            ops.push(LineOp::Delete);
        }
    } else {
        // Both sides still hold lines, and neither end matches, so an optimal
        // path crosses a snake somewhere in the middle. Recurse around it.
        let (x, y, u, v) = middle_snake(old, alo, ahi, new, blo, bhi);
        split(old, alo, x, new, blo, y, ops);
        for _ in x..u {
            ops.push(LineOp::Keep);
        }
        split(old, u, ahi, new, v, bhi, ops);
    }

    for _ in 0..suffix {
        ops.push(LineOp::Keep);
    }
}

/// Find the middle snake of an optimal path through the edit graph.
///
/// Returns `(x, y, u, v)`: the snake runs from `old[x..u]` / `new[y..v]`, a run
/// of equal lines (possibly empty) that an optimal edit script passes through.
/// Forward and reverse breadth-first searches are advanced in step until they
/// meet, which is what keeps the space linear.
fn middle_snake<T: PartialEq>(
    old: &[T],
    alo: usize,
    ahi: usize,
    new: &[T],
    blo: usize,
    bhi: usize,
) -> (usize, usize, usize, usize) {
    let n = (ahi - alo) as isize;
    let m = (bhi - blo) as isize;
    let delta = n - m;
    let odd = delta & 1 != 0;
    let max = (n + m + 1) / 2;

    // Diagonal k is indexed at `off + k`; the extra slot on each side lets the
    // k == -d and k == d edges read their neighbour without a bounds check.
    let off = (max + 1) as usize;
    let size = 2 * off + 1;
    let mut vf = vec![0isize; size];
    let mut vr = vec![0isize; size];

    for d in 0..=max {
        // Forward: furthest-reaching D-path on each diagonal.
        let mut k = -d;
        while k <= d {
            let i = (off as isize + k) as usize;
            let mut x = if k == -d || (k != d && vf[i - 1] < vf[i + 1]) {
                vf[i + 1]
            } else {
                vf[i - 1] + 1
            };
            let mut y = x - k;
            let (x0, y0) = (x, y);
            while x < n && y < m && old[alo + x as usize] == new[blo + y as usize] {
                x += 1;
                y += 1;
            }
            vf[i] = x;

            // The reverse search stores its progress on diagonal `delta - k`
            // measured from the far corner; the two overlap once their
            // furthest points have crossed.
            if odd {
                let kr = delta - k;
                if kr > -d && kr < d {
                    let ir = (off as isize + kr) as usize;
                    if vf[i] + vr[ir] >= n {
                        return (
                            alo + x0 as usize,
                            blo + y0 as usize,
                            alo + x as usize,
                            blo + y as usize,
                        );
                    }
                }
            }
            k += 2;
        }

        // Reverse: the same search from the far corner, in reversed
        // coordinates, so `x` counts lines consumed from the end of `old`.
        let mut k = -d;
        while k <= d {
            let i = (off as isize + k) as usize;
            let mut x = if k == -d || (k != d && vr[i - 1] < vr[i + 1]) {
                vr[i + 1]
            } else {
                vr[i - 1] + 1
            };
            let mut y = x - k;
            let (x0, y0) = (x, y);
            while x < n
                && y < m
                && old[alo + (n - x - 1) as usize] == new[blo + (m - y - 1) as usize]
            {
                x += 1;
                y += 1;
            }
            vr[i] = x;

            if !odd {
                let kf = delta - k;
                if kf >= -d && kf <= d {
                    let idx = (off as isize + kf) as usize;
                    if vr[i] + vf[idx] >= n {
                        return (
                            alo + (n - x) as usize,
                            blo + (m - y) as usize,
                            alo + (n - x0) as usize,
                            blo + (m - y0) as usize,
                        );
                    }
                }
            }
            k += 2;
        }
    }

    // Unreachable: the two searches must meet by d == ceil((n+m)/2), since
    // together they span every diagonal. Degrade to "replace everything"
    // rather than panicking in a utility that is rewriting a history file.
    (alo, blo, alo, blo)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Apply an edit script and return the text it produces.
    fn apply(ops: &[LineOp], old: &[u8], new: &[u8]) -> Vec<u8> {
        let mut out = Vec::new();
        let mut i = 0;
        for op in ops {
            match op {
                LineOp::Keep => {
                    out.push(old[i]);
                    i += 1;
                }
                LineOp::Delete => i += 1,
                LineOp::Insert(j) => out.push(new[*j]),
            }
        }
        assert_eq!(i, old.len(), "script must consume all of old");
        out
    }

    fn edit_count(ops: &[LineOp]) -> usize {
        ops.iter().filter(|o| !matches!(o, LineOp::Keep)).count()
    }

    /// Optimal edit distance by dynamic programming, as an independent oracle.
    fn optimal(old: &[u8], new: &[u8]) -> usize {
        let (n, m) = (old.len(), new.len());
        let mut d = vec![vec![0usize; m + 1]; n + 1];
        for (i, row) in d.iter_mut().enumerate().take(n + 1) {
            row[0] = i;
        }
        for (j, cell) in d[0].iter_mut().enumerate() {
            *cell = j;
        }
        for i in 1..=n {
            for j in 1..=m {
                d[i][j] = if old[i - 1] == new[j - 1] {
                    d[i - 1][j - 1]
                } else {
                    1 + d[i - 1][j].min(d[i][j - 1])
                };
            }
        }
        d[n][m]
    }

    /// Exhaustively check every pair of short strings over a two-letter
    /// alphabet: the script must rebuild `new`, and be no longer than the
    /// dynamic-programming optimum.
    #[test]
    fn diff_is_correct_and_minimal_over_all_short_inputs() {
        let mut cases = vec![Vec::new()];
        for _ in 0..6 {
            let mut next = Vec::new();
            for c in &cases {
                for ch in *b"ab" {
                    let mut v = c.clone();
                    v.push(ch);
                    next.push(v);
                }
            }
            cases.extend(next);
        }

        for old in &cases {
            for new in &cases {
                let ops = diff(old, new);
                assert_eq!(
                    apply(&ops, old, new),
                    *new,
                    "script did not rebuild new for {old:?} -> {new:?}"
                );
                assert_eq!(
                    edit_count(&ops),
                    optimal(old, new),
                    "script not minimal for {old:?} -> {new:?}: {ops:?}"
                );
            }
        }
    }

    /// The shapes that used to panic: deleting trailing lines, and deleting
    /// everything.
    #[test]
    fn trailing_deletions_are_ordinary_deletes() {
        let old = b"abcd";
        assert_eq!(edit_count(&diff(old, b"a")), 3);
        assert_eq!(edit_count(&diff(old, b"")), 4);
        assert_eq!(apply(&diff(old, b"a"), old, b"a"), b"a");
    }

    /// The shape the ten-line lookahead window could not see across.
    #[test]
    fn an_insertion_wider_than_a_lookahead_window_is_all_insertion() {
        let old = b"az";
        let new = b"a123456789012z";
        let ops = diff(old, new);
        assert_eq!(edit_count(&ops), new.len() - old.len());
        assert!(
            !ops.iter().any(|o| matches!(o, LineOp::Delete)),
            "a pure insertion must record no deletions: {ops:?}"
        );
        assert_eq!(apply(&ops, old, new), new);
    }

    #[test]
    fn identical_inputs_produce_no_edits() {
        let a = b"hello";
        assert!(diff(a, a).iter().all(|o| matches!(o, LineOp::Keep)));
    }
}
