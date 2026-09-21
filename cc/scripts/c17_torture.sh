#!/bin/bash
# Run the GCC C torture suite (gcc.c-torture) against c17.
#
# The suite is NOT vendored here: it is GPLv3 and this repo is MIT. Point
# TORTURE_SUITE at an external checkout. To make one:
#
#   git clone --depth 1 --filter=blob:none --sparse \
#       https://github.com/gcc-mirror/gcc.git ~/tmp/repo/gcc-testsuite
#   cd ~/tmp/repo/gcc-testsuite
#   git sparse-checkout set --no-cone gcc/testsuite/gcc.c-torture gcc/testsuite/lib
#
# Usage:
#   c17_torture.sh                 run execute/ at the default levels, diff vs baseline
#   c17_torture.sh -b              record a new baseline instead of diffing
#   c17_torture.sh -O all          every torture level (slow)
#   c17_torture.sh -s compile      pick a sub-suite: execute|ieee|builtins|compile|all
#   c17_torture.sh -f 931004       only tests whose name matches this
#   c17_torture.sh -j 8            job count
#
# Exit status: 0 only if nothing regressed against the baseline. A missing
# compiler or suite is 2. Never exits 0 on a broken run.

set -u

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
C17="${C17:-$REPO_ROOT/target/release/c17}"
TORTURE_SUITE="${TORTURE_SUITE:-$HOME/tmp/repo/gcc-testsuite/gcc/testsuite/gcc.c-torture}"
BASELINE="${BASELINE:-$REPO_ROOT/cc/scripts/torture-baseline.txt}"
WORK="${WORK:-/tmp/c17-torture-$$}"

JOBS=$(nproc)
SUBSUITE=execute
LEVELS=fast
FILTER=""
RECORD=0

while [[ "${1:-}" == -* ]]; do
    case "$1" in
        -b) RECORD=1; shift;;
        -j) JOBS="$2"; shift 2;;
        -s) SUBSUITE="$2"; shift 2;;
        -O) LEVELS="$2"; shift 2;;
        -f) FILTER="$2"; shift 2;;
        -h|--help) sed -n '2,25p' "$0"; exit 0;;
        *) echo "unknown option: $1" >&2; exit 2;;
    esac
done

[ -x "$C17" ] || {
    echo "FATAL: no c17 at $C17" >&2
    echo "       build it with: cargo build --release" >&2
    exit 2
}
[ -d "$TORTURE_SUITE/execute" ] || {
    echo "FATAL: no torture suite at $TORTURE_SUITE" >&2
    echo "       set TORTURE_SUITE, or make a checkout (see header of $0)" >&2
    exit 2
}

# Torture levels, from gcc/testsuite/lib/c-torture.exp. GCC runs every test at
# each of these; "fast" is the inner-loop subset.
case "$LEVELS" in
    fast) OPT_LEVELS=("-O0" "-O2");;
    all)  OPT_LEVELS=(
            "-O0" "-O1" "-O2"
            "-O3 -fomit-frame-pointer -funroll-loops -fpeel-loops -ftracer -finline-functions"
            "-O3 -g" "-Os" "-Og -g"
          );;
    *)    OPT_LEVELS=("$LEVELS");;
esac

mkdir -p "$WORK/bin"
# KEEP=1 leaves $WORK/results.txt behind for triage: it is the per-test record
# the summary is computed from, and the phase work needs to read it.
if [ "${KEEP:-0}" = 1 ]; then
    trap 'rm -rf "$WORK/bin"; echo "kept: $WORK/results.txt"' EXIT
else
    trap 'rm -rf "$WORK"' EXIT
fi

# ---------------------------------------------------------------- directives
#
# Read the dg- directives a test carries and answer three questions: should we
# skip it, what extra flags does it want, and does it need longer to run.
#
# Prints: "<skip-reason>|<extra flags>|<timeout multiplier>"
# An empty skip-reason means run it.
dg_scan() {
    awk '
    BEGIN { skip=""; flags=""; mult=1; stack="" }
    # Only the leading comment block carries directives; stop at the first
    # line of real code so a string containing "dg-" cannot trip us.
    /^[ \t]*[A-Za-z_#]/ && !/^[ \t]*\/\*/ && !/^[ \t]*\*/ && NR>1 { exit }
    {
        line = $0
        while (match(line, /\{[ \t]*dg-[a-z-]+[^}]*\}/)) {
            d = substr(line, RSTART, RLENGTH)
            line = substr(line, RSTART+RLENGTH)

            if (d ~ /dg-(additional-)?options/) {
                if (match(d, /"[^"]*"/)) {
                    o = substr(d, RSTART+1, RLENGTH-2)
                    # A pre-C99 dialect request means exactly one thing to c17:
                    # relax implicit int and implicit function declarations.
                    # c17 has a single language mode; -std= is inert, so the
                    # translation happens here rather than in the driver.
                    if (o ~ /-std=(gnu89|c89|gnu90|c90|iso9899:1990)/)
                        o = o " -fpermissive"
                    gsub(/-std=[a-z0-9:]+/, "", o)
                    # A machine flag is a request about the target, and c17
                    # rejects the ones it does not implement rather than
                    # ignoring them. Forwarding one turns "this test tunes for
                    # i686" into a c17 failure, which it is not: gcc compiles
                    # these and so does c17 without the flag.
                    if (o ~ /(^| )-m[a-z]/) skip = "needs a machine flag c17 does not implement"
                    flags = flags " " o
                }
            }
            else if (d ~ /dg-skip-if/)   { skip = "dg-skip-if" }
            else if (d ~ /dg-require-stack-size/) {
                # The test says how much stack it needs. Honour it rather than
                # letting it die on the default 8 MB.
                if (match(d, /0x[0-9a-fA-F]+|[0-9]+/)) stack = substr(d, RSTART, RLENGTH)
            }
            else if (d ~ /dg-require-effective-target/) {
                # Only the targets we cannot satisfy matter.
                if (d ~ /(vect_|lto|profile|fpic|tls_|alias|weak|trampolines|indirect_jumps|nonlocal_goto|label_values)/)
                    skip = "unsupported target requirement"
            }
            else if (d ~ /dg-timeout-factor/) {
                if (match(d, /[0-9]+/)) mult = substr(d, RSTART, RLENGTH)
            }
        }
    }
    END { printf "%s|%s|%s|%s", skip, flags, mult, stack }
    ' "$1"
}

# Tests GCC itself excludes via a .x file. We honour the existence of the file
# rather than interpreting its Tcl.
has_x_file() { [ -f "${1%.c}.x" ]; }

# Features c17 does not implement and will not, per the plan. Skipping these
# with a named reason is honest; letting them count as failures is not.
UNSUPPORTED_RE='vector_size|__label__|__builtin_apply|__builtin_setjmp|__builtin_longjmp|attribute__ *\(\( *alias'

# Tests gcc on this machine fails exactly as c17 does, verified by running both
# at -O0 and -O2. Counting them as c17 failures overstates the gap, and they are
# the kind of thing that gets re-triaged every few months because nothing says
# otherwise.
#
# Each list is deliberately narrow. A test where gcc fails but c17 *passes* is
# not here -- `20101011-1` is one, and skipping it would have thrown away a
# case c17 gets right. Nor is one where gcc fails at only one level:
# `pr124358` passes under gcc at -O0 and fails under c17, so it stays a real
# failure.

# Calls an undefined function the optimizer is expected to delete. Neither
# compiler can link it at -O0; gcc's own harness runs these at -O1 and above.
# They pass at -O2 under both, so skipping them outright would lose that.
NEEDS_OPTIMIZATION=" 20001121-1 20020107-1 930526-1 961223-1 loop-2c p18298 restrict-1 unroll-1 "

# gcc rejects or fails these at every level here.
GCC_ALSO_FAILS=" 980608-1 bcp-1 eeprof-1 pr117432 pr123864 va-arg-7 va-arg-8 "

# ------------------------------------------------------------------ one test
run_one() {
    local src="$1" opt="$2" work="$3" cc="$4" mode="$5"
    local base; base=$(basename "$src" .c)

    # `execute/builtins/` is three files, not one: the test, a `-lib.c` giving
    # the library functions it checks, and a shared `lib/main.c`. Its own
    # builtins.exp also turns off five gcc passes so the optimizer cannot do
    # the work the library call is supposed to do. Driving it as a single file
    # links nothing and proves nothing.
    local companions="" extra=""
    if [ -f "${src%.c}-lib.c" ]; then
        companions="${src%.c}-lib.c $(dirname "$src")/lib/main.c"
        extra="-fno-tree-dse -fno-tree-loop-distribute-patterns -fno-tracer -fno-ipa-ra -fno-inline-functions"
    fi
    local tag="$base@${opt// /_}"
    local exe="$work/bin/$tag.$$"
    local log="$exe.log"

    if has_x_file "$src"; then
        echo "SKIP	$tag	.x file"; return
    fi
    if awk "/$UNSUPPORTED_RE/ {found=1} END {exit !found}" "$src" 2>/dev/null; then
        echo "SKIP	$tag	unsupported extension"; return
    fi
    case "$GCC_ALSO_FAILS" in
        *" $base "*) echo "SKIP	$tag	gcc fails this too"; return;;
    esac
    case "$NEEDS_OPTIMIZATION" in
        *" $base "*)
            # Only at -O0, where neither compiler can link it.
            case "$opt" in
                *-O0*) echo "SKIP	$tag	needs -O1+ to link; gcc cannot either"; return;;
            esac;;
    esac

    local scan skip flags mult stack
    scan=$(dg_scan "$src")
    skip=${scan%%|*}; scan=${scan#*|}
    flags=${scan%%|*}; scan=${scan#*|}
    mult=${scan%%|*}; stack=${scan##*|}
    if [ -n "$skip" ]; then
        echo "SKIP	$tag	$skip"; return
    fi

    local ctimeout=$((30 * mult)) rtimeout=$((20 * mult))

    # A compile that runs out of time is not a compile error, and reporting it
    # as one hides it: the log is empty, so the failure reads as a mystery.
    # `pr28982b` passes a 256 KB struct by value and takes c17 65 seconds
    # against gcc's 0.02, which is how this was found.
    # shellcheck disable=SC2086
    if [ "$mode" = compile ]; then
        timeout "$ctimeout" "$cc" $opt -w $extra $flags -c "$src" -o "$exe.o" >"$log" 2>&1
        local crc=$?
        rm -f "$exe.o"
        case $crc in
            0)   echo "PASS	$tag	";;
            124) echo "CTIMEOUT	$tag	compile exceeded ${ctimeout}s";;
            *)   echo "CFAIL	$tag	$(head -c 160 "$log" | tr '\n' ' ')";;
        esac
        rm -f "$log"
        return
    fi

    # shellcheck disable=SC2086
    timeout "$ctimeout" "$cc" $opt -w $extra $flags "$src" $companions -o "$exe" -lm >"$log" 2>&1
    local crc=$?
    if [ $crc -ne 0 ]; then
        if [ $crc -eq 124 ]; then
            echo "CTIMEOUT	$tag	compile exceeded ${ctimeout}s"
        else
            echo "CFAIL	$tag	$(head -c 160 "$log" | tr '\n' ' ')"
        fi
        rm -f "$exe" "$log"; return
    fi
    # `dg-require-stack-size` states what the test needs; give it that rather
    # than reporting a stack overflow as a wrong answer.
    if [ -n "$stack" ]; then
        local kb=$(( (stack + 1023) / 1024 * 2 ))
        [ "$kb" -lt 8192 ] && kb=8192
        ( ulimit -s "$kb" 2>/dev/null; timeout "$rtimeout" "$exe" >/dev/null 2>&1 )
    else
        timeout "$rtimeout" "$exe" >/dev/null 2>&1
    fi
    local rc=$?
    rm -f "$exe" "$log"
    case $rc in
        0)   echo "PASS	$tag	";;
        124) echo "TIMEOUT	$tag	";;
        *)   echo "RFAIL	$tag	exit=$rc";;
    esac
}
export -f run_one dg_scan has_x_file
export UNSUPPORTED_RE GCC_ALSO_FAILS NEEDS_OPTIMIZATION

# ------------------------------------------------------------- collect tests
collect() {
    case "$SUBSUITE" in
        execute)  find "$TORTURE_SUITE/execute" -maxdepth 1 -name '*.c';;
        ieee)     find "$TORTURE_SUITE/execute/ieee" -name '*.c';;
        builtins) find "$TORTURE_SUITE/execute/builtins" -name '*.c' \
                       -not -name '*-lib.c' -not -path '*/lib/*';;
        compile)  find "$TORTURE_SUITE/compile" -maxdepth 1 -name '*.c';;
        all)      find "$TORTURE_SUITE/execute" -maxdepth 1 -name '*.c'
                  find "$TORTURE_SUITE/execute/ieee" -name '*.c'
                  find "$TORTURE_SUITE/execute/builtins" -name '*.c' \
                       -not -name '*-lib.c' -not -path '*/lib/*';;
        *) echo "unknown sub-suite: $SUBSUITE" >&2; exit 2;;
    esac | { [ -n "$FILTER" ] && grep -- "$FILTER" || cat; } | sort
}

MODE=run
[ "$SUBSUITE" = compile ] && MODE=compile

TESTS=$(collect)
NTESTS=$(printf '%s\n' "$TESTS" | grep -c . )
[ "$NTESTS" -gt 0 ] || { echo "FATAL: no tests matched" >&2; exit 2; }

echo "compiler:  $C17"
echo "suite:     $TORTURE_SUITE"
echo "sub-suite: $SUBSUITE ($NTESTS tests)  mode=$MODE"
echo "levels:    ${OPT_LEVELS[*]}"
echo "jobs:      $JOBS"
echo

RES="$WORK/results.txt"
: > "$RES"
for opt in "${OPT_LEVELS[@]}"; do
    printf '%s\n' "$TESTS" \
      | xargs -P "$JOBS" -I{} bash -c 'run_one "$@"' _ {} "$opt" "$WORK" "$C17" "$MODE" \
      2>/dev/null >> "$RES"
done

TOTAL=$(wc -l < "$RES")
[ "$TOTAL" -gt 0 ] || { echo "FATAL: harness produced no results" >&2; exit 2; }

for k in PASS CFAIL RFAIL TIMEOUT SKIP; do
    n=$(awk -F'\t' -v k="$k" '$1==k' "$RES" | wc -l)
    printf "%-8s %5d  (%5.1f%%)\n" "$k" "$n" \
        "$(awk -v n="$n" -v t="$TOTAL" 'BEGIN{printf "%.1f", 100*n/t}')"
done
printf "%-8s %5d\n" TOTAL "$TOTAL"

# ------------------------------------------------------------ baseline diff
CUR="$WORK/passing.txt"
awk -F'\t' '$1=="PASS"{print $2}' "$RES" | sort > "$CUR"

if [ "$RECORD" = 1 ]; then
    cp "$CUR" "$BASELINE"
    echo
    echo "baseline recorded: $BASELINE ($(wc -l < "$BASELINE") passing)"
    exit 0
fi

if [ ! -f "$BASELINE" ]; then
    echo
    echo "NOTE: no baseline at $BASELINE -- record one with -b"
    echo
    echo "=== top compile errors ==="
    awk -F'\t' '$1=="CFAIL"{print $3}' "$RES" \
      | sed -E 's#[^ ]*/([^/ ]+\.c)#\1#g; s/[0-9]+/N/g' \
      | cut -c1-90 | sort | uniq -c | sort -rn | head -20
    exit 0
fi

# Restrict the comparison to what this run attempted. Without this, `-f 931004`
# reported every other baseline entry as a regression -- a filtered run could
# never be green, so the filter was unusable for checking a fix.
ATTEMPTED="$WORK/attempted.txt"
awk -F'\t' '{print $2}' "$RES" | sort > "$ATTEMPTED"
RELEVANT="$WORK/relevant.txt"
comm -12 "$BASELINE" "$ATTEMPTED" > "$RELEVANT"

REGRESSED=$(comm -23 "$RELEVANT" "$CUR")
FIXED=$(comm -13 "$BASELINE" "$CUR")

echo
if [ -n "$FIXED" ]; then
    echo "=== newly passing ($(printf '%s\n' "$FIXED" | grep -c .)) ==="
    printf '%s\n' "$FIXED" | sed 's/^/  + /'
fi
if [ -n "$REGRESSED" ]; then
    echo "=== REGRESSED ($(printf '%s\n' "$REGRESSED" | grep -c .)) ==="
    printf '%s\n' "$REGRESSED" | sed 's/^/  - /'
    echo
    echo "FAIL: tests that passed in the baseline no longer pass."
    exit 1
fi
echo "OK: no regressions ($(wc -l < "$RELEVANT") of $(wc -l < "$BASELINE") baseline tests attempted; $(wc -l < "$CUR") passing now)."
