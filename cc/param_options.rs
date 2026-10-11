//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! The `--param <name>=<value>` settings c17 accepts.
//!
//! gcc's parameters tune its own heuristics -- inlining budgets, unrolling
//! limits, cache sizes -- and none changes what a program means, so c17
//! takes each in silence. One changes what c17 emits, and the driver
//! honours it: `ssp-buffer-size`, plain `-fstack-protector`'s threshold
//! (`target::StackProtection`). A name gcc 13 does not know is refused as gcc
//! refuses it, so a configure probe learns what it would learn from gcc:
//! `--param=allow-store-data-races=0` is one, an `-f` option since gcc 10.
//!
//! The table is gcc 13's, from `gcc --help=params -Q`: every name, with the
//! range or the words it takes.

/// The value a parameter takes.
#[derive(Clone, Copy, Debug)]
enum Value {
    /// A non-negative integer, within the bounds if there are any.
    Int(Option<(u32, u32)>),
    /// One of these words.
    Choice(&'static [&'static str]),
}

use Value::{Choice, Int};

/// gcc 13's parameters, sorted by name.
const PARAMS: &[(&str, Value)] = &[
    ("align-loop-iterations", Int(None)),
    ("align-threshold", Int(Some((1, 65536)))),
    ("analyzer-bb-explosion-factor", Int(None)),
    ("analyzer-max-constraints", Int(None)),
    ("analyzer-max-enodes-for-full-dump", Int(None)),
    ("analyzer-max-enodes-per-program-point", Int(None)),
    ("analyzer-max-infeasible-edges", Int(None)),
    ("analyzer-max-recursion-depth", Int(None)),
    ("analyzer-max-svalue-depth", Int(None)),
    ("analyzer-min-snodes-for-call-summary", Int(None)),
    ("asan-globals", Int(Some((0, 1)))),
    ("asan-instrument-allocas", Int(Some((0, 1)))),
    ("asan-instrument-reads", Int(Some((0, 1)))),
    ("asan-instrument-writes", Int(Some((0, 1)))),
    ("asan-instrumentation-with-call-threshold", Int(None)),
    ("asan-kernel-mem-intrinsic-prefix", Int(Some((0, 1)))),
    ("asan-memintrin", Int(Some((0, 1)))),
    ("asan-stack", Int(Some((0, 1)))),
    ("asan-use-after-return", Int(Some((0, 1)))),
    ("avg-loop-niter", Int(Some((1, 65536)))),
    ("avoid-fma-max-bits", Int(Some((0, 512)))),
    ("builtin-expect-probability", Int(Some((0, 100)))),
    ("builtin-string-cmp-inline-length", Int(Some((0, 100)))),
    ("case-values-threshold", Int(None)),
    ("comdat-sharing-probability", Int(None)),
    ("constructive-interference-size", Int(None)),
    ("cxx-max-namespaces-for-diagnostic-help", Int(None)),
    ("destructive-interference-size", Int(None)),
    ("dse-max-alias-queries-per-store", Int(None)),
    ("dse-max-object-size", Int(None)),
    ("early-inlining-insns", Int(None)),
    ("evrp-sparse-threshold", Int(None)),
    ("evrp-switch-limit", Int(None)),
    ("fsm-scale-path-stmts", Int(Some((1, 10)))),
    ("gcse-after-reload-critical-fraction", Int(None)),
    ("gcse-after-reload-partial-fraction", Int(None)),
    ("gcse-cost-distance-ratio", Int(None)),
    ("gcse-unrestricted-cost", Int(None)),
    ("ggc-min-expand", Int(None)),
    ("ggc-min-heapsize", Int(None)),
    ("gimple-fe-computed-hot-bb-threshold", Int(None)),
    ("graphite-allow-codegen-errors", Int(Some((0, 1)))),
    ("graphite-max-arrays-per-scop", Int(None)),
    ("graphite-max-nb-scop-params", Int(None)),
    ("hash-table-verification-limit", Int(None)),
    ("hot-bb-count-fraction", Int(None)),
    ("hot-bb-count-ws-permille", Int(Some((0, 1000)))),
    ("hot-bb-frequency-fraction", Int(None)),
    ("hwasan-instrument-allocas", Int(Some((0, 1)))),
    ("hwasan-instrument-mem-intrinsics", Int(Some((0, 1)))),
    ("hwasan-instrument-reads", Int(Some((0, 1)))),
    ("hwasan-instrument-stack", Int(Some((0, 1)))),
    ("hwasan-instrument-writes", Int(Some((0, 1)))),
    ("hwasan-random-frame-tag", Int(Some((0, 1)))),
    ("inline-heuristics-hint-percent", Int(Some((100, 1000000)))),
    ("inline-min-speedup", Int(Some((0, 100)))),
    ("inline-unit-growth", Int(None)),
    ("integer-share-limit", Int(Some((2, 65536)))),
    ("ipa-cp-eval-threshold", Int(None)),
    ("ipa-cp-large-unit-insns", Int(None)),
    ("ipa-cp-loop-hint-bonus", Int(None)),
    ("ipa-cp-max-recursive-depth", Int(None)),
    ("ipa-cp-min-recursive-probability", Int(None)),
    ("ipa-cp-profile-count-base", Int(Some((0, 100)))),
    ("ipa-cp-recursion-penalty", Int(Some((0, 100)))),
    ("ipa-cp-recursive-freq-factor", Int(None)),
    ("ipa-cp-single-call-penalty", Int(Some((0, 100)))),
    ("ipa-cp-unit-growth", Int(None)),
    ("ipa-cp-value-list-size", Int(None)),
    ("ipa-jump-function-lookups", Int(None)),
    ("ipa-max-aa-steps", Int(None)),
    ("ipa-max-agg-items", Int(None)),
    ("ipa-max-loop-predicates", Int(None)),
    ("ipa-max-param-expr-ops", Int(None)),
    ("ipa-max-switch-predicate-bounds", Int(None)),
    ("ipa-sra-deref-prob-threshold", Int(Some((0, 100)))),
    ("ipa-sra-max-replacements", Int(Some((0, 16)))),
    ("ipa-sra-ptr-growth-factor", Int(None)),
    ("ipa-sra-ptrwrap-growth-factor", Int(Some((1, 8)))),
    ("ira-consider-dup-in-all-alts", Int(Some((0, 1)))),
    ("ira-loop-reserved-regs", Int(None)),
    ("ira-max-conflict-table-size", Int(None)),
    ("ira-max-loops-num", Int(None)),
    ("ira-simple-lra-insn-threshold", Int(None)),
    ("iv-always-prune-cand-set-bound", Int(None)),
    ("iv-consider-all-candidates-bound", Int(None)),
    ("iv-max-considered-uses", Int(None)),
    (
        "jump-table-max-growth-ratio-for-size",
        Int(Some((0, 10000))),
    ),
    (
        "jump-table-max-growth-ratio-for-speed",
        Int(Some((0, 10000))),
    ),
    ("l1-cache-line-size", Int(None)),
    ("l1-cache-size", Int(None)),
    ("l2-cache-size", Int(None)),
    ("large-function-growth", Int(None)),
    ("large-function-insns", Int(None)),
    ("large-stack-frame", Int(None)),
    ("large-stack-frame-growth", Int(None)),
    ("large-unit-insns", Int(None)),
    ("lazy-modules", Int(None)),
    ("lim-expensive", Int(None)),
    ("logical-op-non-short-circuit", Int(Some((0, 1)))),
    ("loop-block-tile-size", Int(None)),
    ("loop-interchange-max-num-stmts", Int(None)),
    ("loop-interchange-stride-ratio", Int(None)),
    ("loop-invariant-max-bbs-in-loop", Int(None)),
    ("loop-max-datarefs-for-datadeps", Int(None)),
    ("loop-versioning-max-inner-insns", Int(None)),
    ("loop-versioning-max-outer-insns", Int(None)),
    (
        "lra-inheritance-ebb-probability-cutoff",
        Int(Some((0, 100))),
    ),
    ("lra-max-considered-reload-pseudos", Int(None)),
    ("lto-max-partition", Int(None)),
    ("lto-max-streaming-parallelism", Int(Some((1, 65536)))),
    ("lto-min-partition", Int(None)),
    ("lto-partitions", Int(Some((1, 65536)))),
    ("max-average-unrolled-insns", Int(None)),
    ("max-combine-insns", Int(Some((2, 4)))),
    ("max-completely-peel-loop-nest-depth", Int(None)),
    ("max-completely-peel-times", Int(None)),
    ("max-completely-peeled-insns", Int(None)),
    ("max-crossjump-edges", Int(None)),
    ("max-cse-insns", Int(None)),
    ("max-cse-path-length", Int(Some((1, 65536)))),
    ("max-cselib-memory-locations", Int(None)),
    ("max-debug-marker-count", Int(None)),
    ("max-delay-slot-insn-search", Int(None)),
    ("max-delay-slot-live-search", Int(None)),
    ("max-dse-active-local-stores", Int(None)),
    ("max-early-inliner-iterations", Int(None)),
    ("max-fields-for-field-sensitive", Int(None)),
    ("max-find-base-term-values", Int(None)),
    ("max-fsm-thread-path-insns", Int(Some((1, 999999)))),
    ("max-gcse-insertion-ratio", Int(None)),
    ("max-gcse-memory", Int(None)),
    ("max-goto-duplication-insns", Int(None)),
    ("max-grow-copy-bb-insns", Int(None)),
    ("max-hoist-depth", Int(None)),
    ("max-inline-functions-called-once-insns", Int(None)),
    ("max-inline-functions-called-once-loop-depth", Int(None)),
    ("max-inline-insns-auto", Int(None)),
    ("max-inline-insns-recursive", Int(None)),
    ("max-inline-insns-recursive-auto", Int(None)),
    ("max-inline-insns-single", Int(None)),
    ("max-inline-insns-size", Int(None)),
    ("max-inline-insns-small", Int(None)),
    ("max-inline-recursive-depth", Int(None)),
    ("max-inline-recursive-depth-auto", Int(None)),
    ("max-isl-operations", Int(None)),
    ("max-iterations-computation-cost", Int(None)),
    ("max-iterations-to-track", Int(None)),
    ("max-jump-thread-duplication-stmts", Int(None)),
    ("max-jump-thread-paths", Int(Some((1, 65536)))),
    ("max-last-value-rtl", Int(None)),
    ("max-loop-header-insns", Int(None)),
    ("max-modulo-backtrack-attempts", Int(None)),
    ("max-partial-antic-length", Int(None)),
    ("max-peel-branches", Int(None)),
    ("max-peel-times", Int(None)),
    ("max-peeled-insns", Int(None)),
    ("max-pending-list-length", Int(None)),
    ("max-pipeline-region-blocks", Int(None)),
    ("max-pipeline-region-insns", Int(None)),
    ("max-pow-sqrt-depth", Int(Some((1, 32)))),
    ("max-predicted-iterations", Int(Some((0, 65536)))),
    ("max-reload-search-insns", Int(None)),
    ("max-rtl-if-conversion-insns", Int(Some((0, 99)))),
    (
        "max-rtl-if-conversion-predictable-cost",
        Int(Some((0, 200))),
    ),
    (
        "max-rtl-if-conversion-unpredictable-cost",
        Int(Some((0, 200))),
    ),
    ("max-sched-extend-regions-iters", Int(None)),
    ("max-sched-insn-conflict-delay", Int(Some((1, 10)))),
    ("max-sched-ready-insns", Int(Some((1, 65536)))),
    ("max-sched-region-blocks", Int(None)),
    ("max-sched-region-insns", Int(None)),
    ("max-slsr-cand-scan", Int(Some((1, 999999)))),
    ("max-speculative-devirt-maydefs", Int(None)),
    ("max-ssa-name-query-depth", Int(Some((1, 10)))),
    ("max-store-chains-to-track", Int(Some((1, 65536)))),
    ("max-stores-to-merge", Int(Some((2, 65536)))),
    ("max-stores-to-sink", Int(None)),
    ("max-stores-to-track", Int(Some((2, 1048576)))),
    ("max-tail-merge-comparisons", Int(None)),
    ("max-tail-merge-iterations", Int(None)),
    ("max-tracked-strlens", Int(None)),
    ("max-tree-if-conversion-phi-args", Int(Some((2, 65536)))),
    ("max-unroll-times", Int(None)),
    ("max-unrolled-insns", Int(None)),
    ("max-unswitch-depth", Int(Some((1, 50)))),
    ("max-unswitch-insns", Int(None)),
    ("max-variable-expansions-in-unroller", Int(None)),
    ("max-vartrack-expr-depth", Int(None)),
    ("max-vartrack-reverse-op-size", Int(None)),
    ("max-vartrack-size", Int(None)),
    ("min-crossjump-insns", Int(Some((1, 65536)))),
    ("min-inline-recursive-probability", Int(None)),
    ("min-insn-to-prefetch-ratio", Int(None)),
    ("min-loop-cond-split-prob", Int(Some((0, 100)))),
    ("min-nondebug-insn-uid", Int(None)),
    ("min-pagesize", Int(None)),
    ("min-size-for-stack-sharing", Int(None)),
    ("min-spec-prob", Int(None)),
    ("min-vect-loop-bound", Int(None)),
    ("modref-max-accesses", Int(None)),
    ("modref-max-adjustments", Int(Some((0, 254)))),
    ("modref-max-bases", Int(None)),
    ("modref-max-depth", Int(Some((1, 65536)))),
    ("modref-max-escape-points", Int(None)),
    ("modref-max-refs", Int(None)),
    ("modref-max-tests", Int(None)),
    ("openacc-kernels", Choice(&["decompose", "parloops"])),
    ("openacc-privatization", Choice(&["quiet", "noisy"])),
    ("parloops-chunk-size", Int(None)),
    ("parloops-min-per-thread", Int(Some((2, 65536)))),
    (
        "parloops-schedule",
        Choice(&["static", "dynamic", "guided", "auto", "runtime"]),
    ),
    ("partial-inlining-entry-probability", Int(Some((0, 100)))),
    ("predictable-branch-outcome", Int(Some((0, 50)))),
    ("prefetch-dynamic-strides", Int(Some((0, 1)))),
    ("prefetch-latency", Int(None)),
    ("prefetch-min-insn-to-mem-ratio", Int(None)),
    ("prefetch-minimum-stride", Int(None)),
    ("profile-func-internal-id", Int(Some((0, 1)))),
    ("ranger-debug", Int(None)),
    ("ranger-logical-depth", Int(Some((1, 999)))),
    ("ranger-recompute-depth", Int(Some((1, 100)))),
    ("relation-block-limit", Int(Some((0, 9999)))),
    ("rpo-vn-max-loop-depth", Int(Some((2, 65536)))),
    ("sccvn-max-alias-queries-per-access", Int(None)),
    ("scev-max-expr-complexity", Int(None)),
    ("scev-max-expr-size", Int(None)),
    ("sched-autopref-queue-depth", Int(None)),
    ("sched-mem-true-dep-cost", Int(None)),
    ("sched-pressure-algorithm", Int(Some((1, 2)))),
    ("sched-spec-prob-cutoff", Int(Some((0, 100)))),
    ("sched-state-edge-prob-cutoff", Int(Some((0, 100)))),
    ("selsched-insns-to-rename", Int(None)),
    ("selsched-max-lookahead", Int(None)),
    ("selsched-max-sched-times", Int(Some((1, 65536)))),
    ("simultaneous-prefetches", Int(None)),
    ("sink-frequency-threshold", Int(Some((0, 100)))),
    ("sms-dfa-history", Int(Some((0, 16)))),
    ("sms-loop-average-count-threshold", Int(None)),
    ("sms-max-ii-factor", Int(Some((1, 16)))),
    ("sms-min-sc", Int(Some((1, 2)))),
    ("sra-max-propagations", Int(None)),
    ("sra-max-scalarization-size-Osize", Int(None)),
    ("sra-max-scalarization-size-Ospeed", Int(None)),
    ("ssa-name-def-chain-limit", Int(None)),
    ("ssp-buffer-size", Int(Some((1, 65536)))),
    ("stack-clash-protection-guard-size", Int(Some((12, 30)))),
    ("stack-clash-protection-probe-interval", Int(Some((10, 16)))),
    ("store-merging-allow-unaligned", Int(Some((0, 1)))),
    ("store-merging-max-size", Int(Some((1, 65536)))),
    ("switch-conversion-max-branch-ratio", Int(Some((1, 65536)))),
    ("threader-debug", Int(None)),
    ("tm-max-aggregate-size", Int(None)),
    ("tracer-dynamic-coverage", Int(Some((0, 100)))),
    ("tracer-dynamic-coverage-feedback", Int(Some((0, 100)))),
    ("tracer-max-code-growth", Int(None)),
    ("tracer-min-branch-probability", Int(Some((0, 100)))),
    (
        "tracer-min-branch-probability-feedback",
        Int(Some((0, 100))),
    ),
    ("tracer-min-branch-ratio", Int(Some((0, 100)))),
    ("tree-reassoc-width", Int(None)),
    ("tsan-distinguish-volatile", Int(Some((0, 1)))),
    ("tsan-instrument-func-entry-exit", Int(Some((0, 1)))),
    ("uninit-control-dep-attempts", Int(Some((1, 65536)))),
    ("uninit-max-chain-len", Int(Some((1, 128)))),
    ("uninit-max-num-chains", Int(Some((1, 128)))),
    ("uninlined-function-insns", Int(Some((0, 1000000)))),
    ("uninlined-function-time", Int(Some((0, 1000000)))),
    ("uninlined-thunk-insns", Int(Some((0, 1000000)))),
    ("uninlined-thunk-time", Int(Some((0, 1000000)))),
    ("unlikely-bb-count-fraction", Int(None)),
    ("unroll-jam-max-unroll", Int(None)),
    ("unroll-jam-min-percent", Int(Some((0, 100)))),
    ("use-after-scope-direct-emission-threshold", Int(None)),
    ("use-canonical-types", Int(Some((0, 1)))),
    ("vect-epilogues-nomask", Int(Some((0, 1)))),
    ("vect-induction-float", Int(Some((0, 1)))),
    ("vect-inner-loop-cost-factor", Int(Some((1, 10000)))),
    ("vect-max-layout-candidates", Int(None)),
    ("vect-max-peeling-for-alignment", Int(Some((0, 64)))),
    ("vect-max-version-for-alias-checks", Int(None)),
    ("vect-max-version-for-alignment-checks", Int(None)),
    ("vect-partial-vector-usage", Int(Some((0, 2)))),
    ("x86-stlf-window-ninsns", Int(None)),
    ("x86-stv-max-visits", Int(Some((1, 1000000)))),
];

/// The largest value gcc stores in a parameter.
const INT_MAX: u64 = i32::MAX as u64;

/// Check the setting `<name>=<value>` given to `--param`: `Ok` for one gcc
/// 13 takes, or gcc's lines (an error, then any notes) refusing it.
pub fn check(setting: &str) -> Result<(), Vec<String>> {
    let option = format!("--param={setting}");
    let unrecognized = || {
        vec![format!(
            "error: unrecognized command-line option '{option}'"
        )]
    };
    let (name, value) = setting.split_once('=').ok_or_else(unrecognized)?;
    let kind = PARAMS
        .binary_search_by(|(n, _)| n.cmp(&name))
        .map(|i| PARAMS[i].1)
        .map_err(|_| unrecognized())?;
    if value.is_empty() {
        return Err(vec![format!(
            "error: missing argument to '--param={name}='"
        )]);
    }
    match kind {
        Choice(words) if words.contains(&value) => Ok(()),
        Choice(words) => Err(vec![
            format!("error: unrecognized argument in option '{option}'"),
            format!(
                "note: valid arguments to '--param={name}=' are: {}",
                words.join(" ")
            ),
        ]),
        Int(bounds) => {
            if !value.bytes().all(|b| b.is_ascii_digit()) {
                return Err(vec![format!(
                    "error: argument to '--param={name}=' should be a non-negative integer"
                )]);
            }
            // Too long for a u64 is past every bound.
            let n = value.parse::<u64>().unwrap_or(u64::MAX);
            match bounds {
                Some((lo, hi)) if n < u64::from(lo) || n > u64::from(hi) => Err(vec![format!(
                    "error: argument to '--param={name}=' is not between {lo} and {hi}"
                )]),
                _ if n > INT_MAX => Err(vec![format!(
                    "error: argument to '--param={name}=' is bigger than {INT_MAX}"
                )]),
                _ => Ok(()),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn error(setting: &str) -> String {
        check(setting).unwrap_err().join("\n")
    }

    #[test]
    fn table_is_sorted_and_unique() {
        assert!(PARAMS.windows(2).all(|w| w[0].0 < w[1].0));
    }

    #[test]
    fn known_settings() {
        for setting in [
            "max-inline-insns-single=10",
            "ssp-buffer-size=4",
            "align-threshold=1",
            "align-threshold=65536",
            "parloops-schedule=guided",
            "large-stack-frame=2147483647",
        ] {
            assert_eq!(check(setting), Ok(()), "{setting}");
        }
    }

    #[test]
    fn refusals_have_gcc_text() {
        for setting in [
            "allow-store-data-races=0",
            "foo=1",
            "max-inline-insns-single",
            "=1",
        ] {
            assert_eq!(
                error(setting),
                format!("error: unrecognized command-line option '--param={setting}'")
            );
        }
        assert_eq!(
            error("ssp-buffer-size="),
            "error: missing argument to '--param=ssp-buffer-size='"
        );
        for v in ["-1", "x", "1k"] {
            assert_eq!(
                error(&format!("ssp-buffer-size={v}")),
                "error: argument to '--param=ssp-buffer-size=' should be a non-negative integer"
            );
        }
        assert_eq!(
            error("large-stack-frame=2147483648"),
            "error: argument to '--param=large-stack-frame=' is bigger than 2147483647"
        );
        assert_eq!(
            error("max-combine-insns=5"),
            "error: argument to '--param=max-combine-insns=' is not between 2 and 4"
        );
        assert_eq!(
            error("parloops-schedule=x"),
            "error: unrecognized argument in option '--param=parloops-schedule=x'\n\
             note: valid arguments to '--param=parloops-schedule=' are: \
             static dynamic guided auto runtime"
        );
    }
}
