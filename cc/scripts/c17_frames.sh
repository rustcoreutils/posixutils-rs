#!/bin/bash
# Compare c17's stack frames with gcc's, function by function, through the
# .su records both write under -fstack-usage; or check that two c17 builds
# emit identical assembly.
#
# The corpus is CPython's interpreter and object sources plus every 25th
# gcc.c-torture execute test. Neither is vendored here (CPython's licence is
# not MIT, the torture suite is GPLv3):
#
#   CPYTHON_SRC     a configured CPython 3.12 tree (it needs pyconfig.h);
#                   default /tmp/claude-1000/cpy/src
#   TORTURE_SUITE   as for c17_torture.sh; default ~/tmp/repo/gcc-testsuite
#
# Usage:
#   c17_frames.sh [-c C17] [-O LEVEL]          frames: c17 against gcc
#   c17_frames.sh -i BASE_C17 [-c C17]          identical: every function's
#                                               assembly, both targets, -O0/-O2
#
# Frames mode prints the corpus totals, the ratio to gcc's, the functions
# where c17 is furthest above gcc, and _PyEval_EvalFrameDefault, the
# interpreter loop whose frame decides how deep CPython recurses. A function
# only one compiler emits (the other inlined it everywhere) shows "-" for the
# other, counts in that compiler's all-functions total only, and is left out
# of the ratio and the list, which compare functions both emitted.
set -u
export LC_ALL=C
REPO=$(cd "$(dirname "$0")/../.." && pwd)
C17=$REPO/target/release/c17
BASE=
OPT=-O2
while getopts "c:i:O:" o; do
    case $o in
        c) C17=$OPTARG ;;
        i) BASE=$OPTARG ;;
        O) OPT=$OPTARG ;;
        *) echo "usage: $0 [-c C17] [-O LEVEL] [-i BASE_C17]" >&2; exit 2 ;;
    esac
done
CPY=${CPYTHON_SRC:-/tmp/claude-1000/cpy/src}
SUITE=${TORTURE_SUITE:-$HOME/tmp/repo/gcc-testsuite}/gcc/testsuite/gcc.c-torture/execute
[ -f "$CPY/pyconfig.h" ] || { echo "no configured CPython at $CPY" >&2; exit 2; }
[ -d "$SUITE" ] || { echo "no torture suite at $SUITE" >&2; exit 2; }
WORK=$(mktemp -d "${TMPDIR:-/tmp}/c17_frames.XXXXXX")
trap 'rm -rf "$WORK"' EXIT

cpy_files=$(cd "$CPY" && ls Python/*.c Objects/*.c | grep -v -e dynload_ -e frozen)
torture_files=$(ls "$SUITE"/*.c | awk 'NR % 25 == 0')
cpy_flags="-I $CPY/Include -I $CPY/Include/internal -I $CPY -DPy_BUILD_CORE -w"

# compile CC OUTDIR EXTRA...: every corpus file to OUTDIR/<n>.s, -fstack-usage
# beside it, in parallel; a file a compiler refuses is left out, by name.
compile() {
    local cc=$1 out=$2; shift 2
    mkdir -p "$out"
    local n=0 f
    {
        for f in $cpy_files; do
            n=$((n + 1))
            printf '%s\0' "cd '$CPY' && $cc $OPT $cpy_flags $* -S -o '$out/c$n.s' '$f'"
        done
        n=0
        for f in $torture_files; do
            n=$((n + 1))
            printf '%s\0' "$cc $OPT -w $* -S -o '$out/t$n.s' '$f'"
        done
    } | xargs -0 -P "$(nproc)" -I{} sh -c '{} 2>/dev/null' || true
}

if [ -n "$BASE" ]; then
    diffs=0 compared=0
    for tgt in x86_64-unknown-linux-gnu aarch64-unknown-linux-gnu; do
        for OPT in -O0 -O2; do
            rm -rf "$WORK/a" "$WORK/b"
            compile "$BASE" "$WORK/a" --target=$tgt
            compile "$C17" "$WORK/b" --target=$tgt
            for s in "$WORK"/a/*.s; do
                b=$WORK/b/$(basename "$s")
                compared=$((compared + 1))
                if ! cmp -s "$s" "$b"; then
                    echo "DIFF $tgt $OPT $(basename "$s")"
                    diffs=$((diffs + 1))
                fi
            done
        done
    done
    echo "compared=$compared differ=$diffs"
    [ "$diffs" = 0 ]
    exit
fi

compile gcc "$WORK/gcc" -fstack-usage
compile "$C17" "$WORK/c17" -fstack-usage
# One "name bytes" line per function: the function name, qualified by the
# source file so that two statics of one name stay apart.
su() { cat "$1"/*.su 2>/dev/null | awk -F'\t' '{ split($1, p, ":"); n = split(p[1], d, "/"); print d[n] ":" p[4], $2 }' | sort; }
su "$WORK/gcc" > "$WORK/gcc.txt"
su "$WORK/c17" > "$WORK/c17.txt"
join -a1 -a2 -e - -o 0,1.2,2.2 "$WORK/c17.txt" "$WORK/gcc.txt" > "$WORK/all.txt"
awk '$2 != "-" && $3 != "-"' "$WORK/all.txt" > "$WORK/both.txt"
awk '$2 != "-" { c += $2; nc++ } $3 != "-" { g += $3; ng++ } END {
        printf "all functions: c17 %d in %d bytes, gcc %d in %d bytes\n", nc, c, ng, g }' "$WORK/all.txt"
awk '{ c += $2; g += $3; n++ } END {
        printf "emitted by both: %d  c17 %d bytes  gcc %d bytes  ratio %.2f\n", n, c, g, (g ? c / g : 0) }' "$WORK/both.txt"
echo "-- furthest above gcc, emitted by both (name c17 gcc) --"
awk '{ print $2 - $3, $0 }' "$WORK/both.txt" | sort -rn | head -15 | cut -d' ' -f2-
echo "-- interpreter loop --"
grep ':_PyEval_EvalFrameDefault ' "$WORK/all.txt"
