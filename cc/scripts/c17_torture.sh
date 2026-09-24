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
#   c17_torture.sh                 run every sub-suite at the default levels, diff vs baseline
#   c17_torture.sh -b              record a new baseline instead of diffing
#   c17_torture.sh -O all          every torture level (slow)
#   c17_torture.sh -s compile      one sub-suite: execute|ieee|builtins|compile|all
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

# The target triple `dg-skip-if` selectors are matched against. gcc spells
# these `<arch>-<vendor>-<os>` and globs them, so `x86_64-*-*` has to match and
# `i?86-*-*` has to not.
TORTURE_TRIPLE="${TORTURE_TRIPLE:-$(uname -m)-pc-$(uname -s | tr 'A-Z' 'a-z')-gnu}"

JOBS=$(nproc)
SUBSUITE=all
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
# Read the dg- directives a test carries and answer four questions: should we
# skip it, what extra flags does it want, how should it be built, and does it
# need longer to run.
#
# Prints: "<skip-reason>|<extra flags>|<timeout multiplier>|<stack>|<dg-do>"
# An empty skip-reason means run it.
#
# Directives are read from **comment text only**, over the whole file. The
# earlier version stopped at the first line starting with a letter, so that a
# string containing "dg-" could not trip it -- but a continuation line of a
# multi-line comment starts with a letter too, so every directive below one was
# silently dropped. `20031220-2` and `20000804-1` lost their `-std=gnu89`, and
# with it the `-fpermissive` it translates to, and were reported as plain
# compile failures. Stripping comments instead excludes strings by
# construction and needs no guess about where the header ends.
dg_scan() {
    awk -v TRIPLE="$TORTURE_TRIPLE" -v OPTS="${2:-}" '
    BEGIN { skip=""; flags=""; mult=1; stack=""; dgdo=""; in_c=0 }

    # Reduce the line to the comment text it contains, tracking /* */ across
    # lines. A // comment runs to end of line. Anything outside a comment --
    # code, and every string literal in it -- is discarded.
    {
        line = $0; out = ""
        while (length(line) > 0) {
            if (in_c) {
                e = index(line, "*/")
                if (e == 0) { out = out " " line; line = "" }
                else { out = out " " substr(line, 1, e - 1); line = substr(line, e + 2); in_c = 0 }
            } else {
                b = index(line, "/*")
                l = index(line, "//")
                if (l > 0 && (b == 0 || l < b)) { out = out " " substr(line, l + 2); line = "" }
                else if (b > 0) { line = substr(line, b + 2); in_c = 1 }
                else { line = "" }
            }
        }
        scan(out)
    }

    function scan(text,   d, o, rest, q1, q2) {
        while (match(text, /\{[ \t]*dg-[a-z-]+/)) {
            d = substr(text, RSTART, RLENGTH)
            text = substr(text, RSTART + RLENGTH)

            if (d ~ /dg-(additional-)?options/) {
                # The option string is the first quoted run after the name.
                q1 = index(text, "\"")
                if (q1 == 0) continue
                rest = substr(text, q1 + 1)
                q2 = index(rest, "\"")
                if (q2 == 0) continue
                o = substr(rest, 1, q2 - 1)
                rest = substr(rest, q2 + 1)

                # A selector follows the string when the directive is meant for
                # one target family: `dg-options "..." { target powerpc*-*-* }`.
                # We cannot evaluate a selector, and applying one anyway is how
                # PowerPC-only `-G0` reached the driver and was rejected. Skip
                # the whole option line, which is what gcc does everywhere the
                # selector does not match -- almost everywhere.
                if (rest ~ /^[ \t]*\{/) continue

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
            else if (d ~ /dg-do/) {
                # What gcc builds this test as. `compile` stops at assembly,
                # which is why a test whose body is another target'"'"'s inline
                # assembly still passes there: gas never sees it.
                if (match(text, /^[ \t]*(compile|assemble|run|link|preprocess)/)) {
                    dgdo = substr(text, RSTART, RLENGTH)
                    gsub(/[ \t]/, "", dgdo)
                }
            }
            else if (d ~ /dg-skip-if/)   {
                # `dg-skip-if "reason" { targets } { flags } { flags }`.
                # Treating every one of these as "skip" threw away tests that
                # pass: nearly all of them name a target that is not ours --
                # bpf, avr, pdp11, m68k, hppa -- or `freestanding`, and we are
                # hosted. Read the selector instead.
                if (dg_skip_applies(text)) skip = "dg-skip-if"
            }
            else if (d ~ /dg-require-stack-size/) {
                # The test says how much stack it needs. Honour it rather than
                # letting it die on the default 8 MB.
                if (match(text, /0x[0-9a-fA-F]+|[0-9]+/)) stack = substr(text, RSTART, RLENGTH)
            }
            else if (d ~ /dg-require-effective-target/) {
                # Only the targets we cannot satisfy matter.
                if (match(text, /^[^}]*/) &&
                    substr(text, RSTART, RLENGTH) ~ /(vect_|lto|profile|fpic|tls_|alias|weak|trampolines|indirect_jumps|nonlocal_goto|label_values)/)
                    skip = "unsupported target requirement"
            }
            else if (d ~ /dg-timeout-factor/) {
                if (match(text, /[0-9]+/)) mult = substr(text, RSTART, RLENGTH)
            }
        }
    }

    # Does the selector that follows a dg-skip-if apply to this run?
    #
    # Two groups matter: the targets it names, and the option strings. A target
    # group naming only other architectures does not apply here; `*-*-*` names
    # every target, and then the option group decides -- `{ *-*-* } "-O1"`
    # skips only at -O1.
    function dg_skip_applies(text,   grp, rest, i, n, tok, toks, hit) {
        grp = first_group(text)
        if (grp == "") return 1           # no selector at all: unconditional
        rest = substr(text, GRP_END + 1)

        gsub(/[{}"]/, " ", grp)
        n = split(grp, toks, /[ \t]+/)
        hit = 0
        for (i = 1; i <= n; i++) {
            tok = toks[i]
            if (tok == "" || tok == "&&" || tok == "||" || tok == "!") continue
            # `freestanding` is the one effective target here that is plainly
            # false for us: this harness links against a hosted libc.
            if (tok == "freestanding") continue
            # An effective-target name we do not model. Assume it applies, so
            # an unread selector errs towards skipping rather than towards a
            # failure we would have to triage as a target question.
            if (index(tok, "-") == 0) return option_group_applies(rest)
            if (glob_match(tok, TRIPLE)) hit = 1
        }
        if (hit) return option_group_applies(rest)
        return 0
    }

    # The option group, when present, lists the command lines the skip applies
    # to. Empty or absent means every one.
    function option_group_applies(rest,   head, n, i, toks) {
        # Everything up to the directive'"'"'s own close. gcc writes the option
        # list either braced or as bare quoted strings, so take both.
        head = rest
        if (match(head, /\}/)) head = substr(head, 1, RSTART - 1)
        gsub(/[{}]/, " ", head)
        n = split(head, toks, /"/)
        # split on quotes: the odd fields are outside, the even ones inside.
        for (i = 2; i <= n; i += 2) {
            if (toks[i] ~ /^[ \t]*$/) continue
            if (index(OPTS, toks[i]) > 0) return 1
        }
        for (i = 2; i <= n; i += 2) if (toks[i] !~ /^[ \t]*$/) return 0
        return 1
    }

    # The first brace-balanced { ... } group in text, or "" if none precedes
    # the directive'"'"'s own close.
    function first_group(text,   i, c, depth, start) {
        depth = 0
        GRP_END = 0
        for (i = 1; i <= length(text); i++) {
            c = substr(text, i, 1)
            if (c == "{") { if (depth == 0) start = i; depth++ }
            else if (c == "}") {
                depth--
                if (depth < 0) return ""          # closed the directive first
                if (depth == 0) { GRP_END = i; return substr(text, start, i - start + 1) }
            }
        }
        return ""
    }

    # Tcl-style glob against the target triple.
    function glob_match(pat, str,   re) {
        re = pat
        gsub(/[.+^$()\[\]|\\]/, "\\&", re)
        gsub(/\*/, ".*", re)
        gsub(/\?/, ".", re)
        return str ~ ("^" re "$")
    }

    END { printf "%s|%s|%s|%s|%s", skip, flags, mult, stack, dgdo }
    ' "$1"
}

# Tests GCC itself excludes via a .x file. We honour the existence of the file
# rather than interpreting its Tcl.
has_x_file() { [ -f "${1%.c}.x" ]; }

# Features c17 does not implement and will not, per the plan. Skipping these
# with a named reason is honest; letting them count as failures is not.
UNSUPPORTED_RE='vector_size|__label__|__builtin_apply|__builtin_setjmp|__builtin_longjmp|attribute__ *\(\( *alias'

# Features c17 has decided not to implement: GNU-only language extensions, and
# anything newer than C17. A test that needs one is **out of scope**, not a
# failure -- counting it as one measures a decision rather than a defect, and
# invites the same triage every few months.
#
# Listed by name, never matched against the source. Scanning for the feature
# looked tidier and was wrong: `pr86659-1`, `pr86659-2` and `pr87623` all
# mention `scalar_storage_order` and **pass** anyway, so a content match threw
# away three cases c17 gets right -- the same trap the note above describes.
# A name list also means every skip is auditable, and a test added to the suite
# later shows up as a new failure and gets triaged then, which is the right
# moment to decide.
#
# Post-C17: `_Decimal32/64/128` is TR 24732, folded into C23; `[[...]]` is the
# C23 attribute syntax.
OUT_OF_SCOPE_POST_C17=" execute/pr80692 execute/pr123978 execute/pr124358 execute/pr125291 \
 compile/pr111059-7 compile/pr111059-8 compile/pr111059-9 \
 compile/pr111059-10 compile/pr111059-11 compile/pr111059-12 \
 compile/pr111911-2 "

# gcc's own GIMPLE front end (`-fgimple`), which parses its internal
# representation rather than C. gcc itself rejects these without the flag.
OUT_OF_SCOPE_GCC_INTERNAL=" compile/pr115143-2 compile/pr115143-3 "

# `-fgnu89-inline` selects pre-C99 `inline` semantics, where `extern inline`
# is a definition another may override. c17 honours the flag; these two also
# want gcc to *reject* a redefinition without it, which c17 does not diagnose.
OUT_OF_SCOPE_GNU89_INLINE=" execute/20021120-1 compile/20021120-1 \
 compile/20021120-2 "

# Written for a target c17 does not have a backend for.
OUT_OF_SCOPE_OTHER_TARGET=" compile/mipscop-1 compile/mipscop-2 compile/mipscop-3 compile/mipscop-4 "

# A GNU-only attribute: reverse-endian load/store lowering.
OUT_OF_SCOPE_GNU_ATTR=" execute/20230630-2 execute/20230630-4 "

# Two more GNU-only features, each named in cc/DECISIONS.md with what it would
# take.
#
# Nested function definitions: a static chain and executable trampolines.
OUT_OF_SCOPE_NESTED_FN=" execute/20010209-1 compile/20010209-1 execute/20010605-1 \
 compile/20010605-1 execute/20030501-1 execute/20040520-1 \
 execute/20090219-1 execute/nest-align-1 execute/nestfunc-7 \
 execute/nest-stdar-1 execute/pr103405 execute/pr22061-3 \
 execute/pr22061-4 compile/20010903-2 compile/20011023-1 \
 compile/20020309-1 compile/20021204-1 \
 compile/20030418-1 compile/20030716-1 \
 compile/20031011-1 compile/20040310-1 compile/20040317-3 \
 compile/20050119-1 compile/951116-1 compile/nested-2 \
 compile/nested-3 compile/pr35006 compile/pr99324 "

# A variable-length array as a struct or union member: struct layout computed
# at run time, and `offsetof` through it.
OUT_OF_SCOPE_VLA_MEMBER=" execute/20020412-1 execute/20040308-1 execute/20040423-1 \
 execute/20041218-2 execute/20070919-1 compile/20070919-1 \
 execute/align-nest execute/pr41935 execute/pr82210 compile/20020210-1 \
 compile/20030224-1 compile/20050801-2 compile/920428-4 compile/920501-16 \
 compile/pr42956 compile/pr77754-6 compile/pr82564 "

# gcc-specific *behaviour*, as opposed to a gcc-specific feature. Neither is
# required by C17 and c17 deliberately does something else; see the
# "Deliberate divergences" table in cc/DECISIONS.md.
#
#   20021127-1  gcc folds llabs() and never calls the program's own definition
#               of llabs. Matching it means a local definition is ignored.
#   20031003-1  (int)2147483648.0f is undefined behaviour; gcc's folder
#               saturates to INT_MAX. aarch64 agrees by hardware accident.
#   pr46309     a conditional with one `void` arm, which gcc takes as an
#               extension and C17 6.5.15p3 forbids.
OUT_OF_SCOPE_GCC_BEHAVIOUR=" execute/20021127-1 execute/20031003-1 execute/pr46309 "

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
NEEDS_OPTIMIZATION=" execute/20001121-1 compile/20001121-1 execute/20020107-1 \
 execute/930526-1 execute/961223-1 execute/loop-2c execute/p18298 \
 execute/restrict-1 execute/unroll-1 "

# Pre-C99 implicit `int` in a test that never asks for a pre-C99 dialect.
# C17 6.7.2p2 requires a type specifier, and gcc made it an error in 14 too,
# so the test is simply older than the rule it breaks. Tests that *do* ask --
# `dg-additional-options "-std=gnu89"` or `"-fpermissive"` -- are honoured and
# pass, which is why this list is two names rather than a hundred.
NEEDS_PRE_C99_DIALECT=" compile/pr29201 "

# `__builtin_issignaling`, which distinguishes a signalling NaN from a quiet
# one. No system header uses it -- `<math.h>` has `issignaling` as its own
# macro and does not reach for a builtin -- so nothing fails to build without
# it, and seven of the nine tests need a format c17 does not have at all
# (`_Float128`, `_Float64x`, `bfloat16`).
OUT_OF_SCOPE_ISSIGNALING=" ieee/builtin-issignaling-1 \
 ieee/bfloat16-builtin-issignaling-1 ieee/float128-builtin-issignaling-1 \
 ieee/float128x-builtin-issignaling-1 ieee/float16-builtin-issignaling-1 \
 ieee/float32-builtin-issignaling-1 ieee/float32x-builtin-issignaling-1 \
 ieee/float64-builtin-issignaling-1 ieee/float64x-builtin-issignaling-1 "

# gcc rejects or fails these at every level here.
GCC_ALSO_FAILS=" execute/980608-1 execute/bcp-1 execute/eeprof-1 execute/pr117432 \
 execute/pr123864 execute/va-arg-7 execute/va-arg-8 compile/dll "

# ------------------------------------------------------------------ one test
run_one() {
    local entry="$1" opt="$2" work="$3" cc="$4"
    # `<sub-suite>:<path>`; the sub-suite is part of the result tag because a
    # test name is not unique across them -- `20000403-1` is in both execute/
    # and compile/, `strlen-3` in both execute/ and execute/builtins/ -- so a
    # bare-name key made one sub-suite's baseline contradict another's.
    local suite="${entry%%:*}" src="${entry#*:}"
    local mode; mode=$(default_mode "$suite")
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
    local tag="$suite/$base@${opt// /_}"
    # The tag carries a `/`, so it is not a file name; scratch files get the
    # same token with the separator flattened.
    local exe="$work/bin/${tag//\//-}.$$"
    local log="$exe.log"

    if has_x_file "$src"; then
        echo "SKIP	$tag	.x file"; return
    fi
    # Scan the test *and* anything it includes by a relative path. Several
    # tests are a two-line wrapper around a file elsewhere in the tree --
    # `pr71626-2` includes `pr71626-1.c`, and `pr109938`/`pr109986` reach into
    # `gcc.dg/tree-ssa/` -- so the feature that blocks them is not in the file
    # named on the command line, and scanning only that file called three
    # `vector_size` tests plain compile failures.
    local scan_files="$src" inc
    while read -r inc; do
        [ -n "$inc" ] || continue
        [ -f "$(dirname "$src")/$inc" ] && scan_files="$scan_files $(dirname "$src")/$inc"
    done <<EOF
$(awk -F'"' '/^[[:space:]]*#[[:space:]]*include[[:space:]]*"/ {print $2}' "$src" 2>/dev/null)
EOF
    if awk "/$UNSUPPORTED_RE/ {found=1} END {exit !found}" $scan_files 2>/dev/null; then
        echo "SKIP	$tag	unsupported extension"; return
    fi
    local key="$suite/$base"
    case "$GCC_ALSO_FAILS" in
        *" $key "*) echo "SKIP	$tag	gcc fails this too"; return;;
    esac
    case "$NEEDS_OPTIMIZATION" in
        *" $key "*)
            # Only at -O0, where neither compiler can link it.
            case "$opt" in
                *-O0*) echo "SKIP	$tag	needs -O1+ to link; gcc cannot either"; return;;
            esac;;
    esac

    case "$OUT_OF_SCOPE_POST_C17" in
        *" $key "*) echo "SKIP	$tag	out of scope: post-C17 feature"; return;;
    esac
    case "$OUT_OF_SCOPE_GNU_ATTR" in
        *" $key "*) echo "SKIP	$tag	out of scope: GNU-only attribute"; return;;
    esac
    case "$OUT_OF_SCOPE_NESTED_FN" in
        *" $key "*) echo "SKIP	$tag	out of scope: nested functions"; return;;
    esac
    case "$OUT_OF_SCOPE_VLA_MEMBER" in
        *" $key "*) echo "SKIP	$tag	out of scope: VLA as a struct member"; return;;
    esac
    case "$OUT_OF_SCOPE_GCC_BEHAVIOUR" in
        *" $key "*) echo "SKIP	$tag	out of scope: gcc-specific behaviour"; return;;
    esac
    case "$OUT_OF_SCOPE_GCC_INTERNAL" in
        *" $key "*) echo "SKIP	$tag	out of scope: gcc's GIMPLE front end"; return;;
    esac
    case "$OUT_OF_SCOPE_GNU89_INLINE" in
        *" $key "*) echo "SKIP	$tag	out of scope: -fgnu89-inline semantics"; return;;
    esac
    case "$OUT_OF_SCOPE_OTHER_TARGET" in
        *" $key "*) echo "SKIP	$tag	out of scope: another target's backend"; return;;
    esac
    case "$NEEDS_PRE_C99_DIALECT" in
        *" $key "*) echo "SKIP	$tag	implicit int without a dialect request"; return;;
    esac
    case "$OUT_OF_SCOPE_ISSIGNALING" in
        *" $key "*) echo "SKIP	$tag	out of scope: __builtin_issignaling"; return;;
    esac
    local scan skip flags mult stack dgdo
    scan=$(dg_scan "$src" "$opt")
    skip=${scan%%|*}; scan=${scan#*|}
    flags=${scan%%|*}; scan=${scan#*|}
    mult=${scan%%|*}; scan=${scan#*|}
    stack=${scan%%|*}; dgdo=${scan##*|}

    # `dg-do` says what gcc builds this test as, and it is not decoration:
    # gcc drives gcc.c-torture/compile with `-S`, so a test whose body is some
    # other target's inline assembly compiles there and never reaches gas.
    # Assembling it anyway -- which `-c` does -- reported `mipscop-1..4` and
    # `920521-1` as c17 failures for gas diagnostics about MIPS and about an
    # instruction the test invented.
    case "$dgdo" in
        assemble)  mode=assemble;;
        compile|preprocess) mode=compile;;
        run|link)  [ "$mode" = compile ] && mode=run;;
    esac
    if [ -n "$skip" ]; then
        echo "SKIP	$tag	$skip"; return
    fi

    local ctimeout=$((30 * mult)) rtimeout=$((20 * mult))

    # A compile that runs out of time is not a compile error, and reporting it
    # as one hides it: the log is empty, so the failure reads as a mystery.
    # `pr28982b` passes a 256 KB struct by value and takes c17 65 seconds
    # against gcc's 0.02, which is how this was found.
    # shellcheck disable=SC2086
    if [ "$mode" = compile ] || [ "$mode" = assemble ]; then
        local stop_at=-S out="$exe.s"
        [ "$mode" = assemble ] && { stop_at=-c; out="$exe.o"; }
        timeout "$ctimeout" "$cc" $opt -w $extra $flags "$stop_at" "$src" -o "$out" >"$log" 2>&1
        local crc=$?
        rm -f "$out"
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
# How a sub-suite is built when the test says nothing. `dg-do` overrides it.
default_mode() {
    case "$1" in
        compile) echo compile;;
        *)       echo run;;
    esac
}

export -f run_one dg_scan has_x_file default_mode
export UNSUPPORTED_RE GCC_ALSO_FAILS NEEDS_OPTIMIZATION TORTURE_TRIPLE
export OUT_OF_SCOPE_POST_C17 OUT_OF_SCOPE_GNU_ATTR
export OUT_OF_SCOPE_NESTED_FN OUT_OF_SCOPE_VLA_MEMBER
export OUT_OF_SCOPE_GCC_BEHAVIOUR OUT_OF_SCOPE_GCC_INTERNAL
export OUT_OF_SCOPE_GNU89_INLINE OUT_OF_SCOPE_OTHER_TARGET
export NEEDS_PRE_C99_DIALECT OUT_OF_SCOPE_ISSIGNALING

# ------------------------------------------------------------- collect tests
# Each line is `<sub-suite>:<path>`, so a worker knows which sub-suite it is in
# without re-deriving it from the path.
collect_one() {
    case "$1" in
        execute)  find "$TORTURE_SUITE/execute" -maxdepth 1 -name '*.c';;
        ieee)     find "$TORTURE_SUITE/execute/ieee" -name '*.c';;
        builtins) find "$TORTURE_SUITE/execute/builtins" -name '*.c' \
                       -not -name '*-lib.c' -not -path '*/lib/*';;
        compile)  find "$TORTURE_SUITE/compile" -maxdepth 1 -name '*.c';;
        *) echo "unknown sub-suite: $1" >&2; exit 2;;
    esac | sed "s#^#$1:#"
}

collect() {
    local sub
    for sub in $SUBSUITES; do collect_one "$sub"; done \
      | { [ -n "$FILTER" ] && grep -- "$FILTER" || cat; } | sort
}

case "$SUBSUITE" in
    all) SUBSUITES="execute ieee builtins compile";;
    *)   SUBSUITES="$SUBSUITE";;
esac

TESTS=$(collect)
NTESTS=$(printf '%s\n' "$TESTS" | grep -c . )
[ "$NTESTS" -gt 0 ] || { echo "FATAL: no tests matched" >&2; exit 2; }

echo "compiler:  $C17"
echo "suite:     $TORTURE_SUITE"
echo "sub-suite: $SUBSUITE ($NTESTS tests)"
echo "levels:    ${OPT_LEVELS[*]}"
echo "jobs:      $JOBS"
echo

RES="$WORK/results.txt"
: > "$RES"
for opt in "${OPT_LEVELS[@]}"; do
    printf '%s\n' "$TESTS" \
      | xargs -P "$JOBS" -I{} bash -c 'run_one "$@"' _ {} "$opt" "$WORK" "$C17" \
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

# A baseline that shares no entry with the run is not a clean run: it is a
# baseline for something else. Renaming the result tag -- which adding the
# sub-suite to it did -- makes every comparison vacuous, and the gate would
# have reported "no regressions (0 of 3076 attempted)" and exited 0 for a
# compiler that failed everything.
if [ ! -s "$RELEVANT" ]; then
    echo
    echo "FATAL: the baseline and this run have no test in common." >&2
    echo "       $(wc -l < "$BASELINE") baseline entries, $(wc -l < "$ATTEMPTED") attempted." >&2
    echo "       Record a new baseline with -b if the tag format changed." >&2
    exit 2
fi

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
