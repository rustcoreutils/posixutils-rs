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
#   c17_torture.sh -t aarch64      build for linux-aarch64 and run under qemu,
#                                  against torture-baseline-aarch64.txt
#
# The aarch64 mode (`-t aarch64`, or TORTURE_TARGET=aarch64) is the only gate
# for aarch64 code generation: every `compile/` output is assembled with
# `aarch64-linux-gnu-as`, and every executable is linked with
# `aarch64-linux-gnu-gcc -static` and run under `qemu-aarch64-static`. The same
# directives and sub-suite handling apply; result tags carry an
# `aarch64/` prefix so they can never be mistaken for host results. It needs
# the cross toolchain and qemu-user, and refuses to run without them.
#
# A test carrying an applicable `dg-error` must be rejected: it passes only
# when c17 fails to compile it with an error on each marked line (see
# expect_reject), and is never run.
#
# Exit status: 0 only if nothing regressed against the baseline. A missing
# compiler, suite or cross toolchain is 2. Never exits 0 on a broken run.

set -u

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
C17="${C17:-$REPO_ROOT/target/release/c17}"
TORTURE_SUITE="${TORTURE_SUITE:-$HOME/tmp/repo/gcc-testsuite/gcc/testsuite/gcc.c-torture}"
WORK="${WORK:-/tmp/c17-torture-$$}"
TORTURE_TARGET="${TORTURE_TARGET:-host}"

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
        -t) TORTURE_TARGET="$2"; shift 2;;
        -h|--help) sed -n '2,35p' "$0"; exit 0;;
        *) echo "unknown option: $1" >&2; exit 2;;
    esac
done

# What each target mode builds with, and the triple `dg-skip-if` selectors are
# matched against. gcc spells a triple `<arch>-<vendor>-<os>` and globs it, so
# `x86_64-*-*` has to match on the host and `aarch64*-*-*` in aarch64 mode.
case "$TORTURE_TARGET" in
    host)
        TARGET_FLAGS=""
        TAG_PREFIX=""
        DEFAULT_BASELINE="$REPO_ROOT/cc/scripts/torture-baseline.txt"
        TORTURE_TRIPLE="${TORTURE_TRIPLE:-$(uname -m)-pc-$(uname -s | tr 'A-Z' 'a-z')-gnu}"
        ;;
    aarch64)
        # Debian's cross packages put the target's headers at
        # /usr/aarch64-linux-gnu/include rather than under a sysroot's
        # usr/include, so `--sysroot` cannot name them; `-isystem` puts them
        # ahead of the host directories, the order aarch64-linux-gnu-gcc
        # itself searches in.
        TARGET_FLAGS="--target aarch64-unknown-linux-gnu -isystem /usr/aarch64-linux-gnu/include"
        TAG_PREFIX="aarch64/"
        DEFAULT_BASELINE="$REPO_ROOT/cc/scripts/torture-baseline-aarch64.txt"
        TORTURE_TRIPLE="${TORTURE_TRIPLE:-aarch64-unknown-linux-gnu}"
        # A run that attempted nothing must not look like a clean one, so a
        # missing tool is fatal rather than a pile of skips.
        for tool in aarch64-linux-gnu-gcc aarch64-linux-gnu-as qemu-aarch64-static; do
            command -v "$tool" >/dev/null 2>&1 || {
                echo "FATAL: aarch64 mode needs $tool, which is not installed" >&2
                exit 2
            }
        done
        [ -d /usr/aarch64-linux-gnu ] || {
            echo "FATAL: aarch64 mode needs the target libc at /usr/aarch64-linux-gnu" >&2
            exit 2
        }
        ;;
    *) echo "unknown target mode: $TORTURE_TARGET (host|aarch64)" >&2; exit 2;;
esac
BASELINE="${BASELINE:-$DEFAULT_BASELINE}"

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
# Read the dg- directives a test carries and answer five questions: should we
# skip it, what extra flags does it want, how should it be built, does it need
# longer to run, and must it be rejected.
#
# Prints: "<skip-reason>|<extra flags>|<timeout multiplier>|<stack>|<dg-do>|<error lines>"
# An empty skip-reason means run it. <error lines> is the space-separated line
# numbers of every `dg-error` that applies to this run; empty means the test
# is valid code.
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
    BEGIN { skip=""; flags=""; mult=1; stack=""; dgdo=""; errs=""; in_c=0
            SEL_FALSE = 0; SEL_TRUE = 1; SEL_UNKNOWN = 2 }

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
                # A machine flag is forwarded like any other: c17
                # implements the SSE levels, -march= and -mtune=, and one it
                # does not implement is a compile failure worth seeing.
                flags = flags " " o
            }
            else if (d ~ /dg-do/) {
                # What gcc builds this test as. `compile` stops at assembly,
                # which is why a test whose body is another target'"'"'s inline
                # assembly still passes there: gas never sees it.
                if (match(text, /^[ \t]*(compile|assemble|run|link|preprocess)/)) {
                    dgdo = substr(text, RSTART, RLENGTH)
                    gsub(/[ \t]/, "", dgdo)
                    # `dg-do compile { target i?86-*-* x86_64-*-* }`: on any
                    # other target DejaGnu reports the test unsupported, so it
                    # is not built here either -- its body is another
                    # target'"'"'s inline assembly. The words are a list, as
                    # braces make them for `sel_eval`.
                    sel = substr(text, RSTART + RLENGTH)
                    if (sel ~ /^[ \t]*\{/) {
                        sel = first_group(sel)
                        sel = substr(sel, index(sel, "{") + 1)
                        sub(/\}[ \t]*$/, "", sel)
                        if (match(sel, /^[ \t]*target[ \t]/) &&
                            sel_eval("{ " substr(sel, RLENGTH + 1) " }") == SEL_FALSE)
                            skip = "dg-do target excludes this target"
                    }
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
                # letting it die on the default 8 MB. The size is often an
                # expression -- "8*100*100", "40000 * 4 + 256" -- and taking
                # its first number read those as 8 and 40000; run_one
                # evaluates it.
                if (match(text, /"[^"]*"/)) stack = substr(text, RSTART + 1, RLENGTH - 2)
            }
            # `dg-require-effective-target`, `dg-require-alias` and the rest
            # describe gcc'"'"'s runner, not the test, and are not read. A regex
            # over them once skipped `alias`, `weak`, `fpic` and
            # `label_values` tests long after c17 had all four, and hid three
            # bugs behind them; modelling each name instead skipped avx512,
            # `-fexceptions` and profiling tests that c17 compiles. Every test
            # is attempted unless SKIP_BY_NAME names it.
            else if (d ~ /dg-timeout-factor/) {
                if (match(text, /[0-9]+/)) mult = substr(text, RSTART, RLENGTH)
            }
            else if (d ~ /dg-error/) {
                # gcc runs a test carrying an applicable `dg-error` as one that
                # must be REJECTED, with an error on each marked line. Ignoring
                # the directive counted "compiled" as a pass, so c17 accepting
                # invalid code read as a pass and rejecting it read as a
                # regression -- both backwards.
                dg_error(text)
            }
        }
    }

    # `dg-error "regexp" ["comment"] [{ target <selector> }]`, on the line it
    # expects the error on. Those are the only shapes the torture suite uses.
    # gcc also allows an `xfail` selector and a trailing line number (`N`,
    # `.-1`); neither occurs, so neither is modelled, and a test that grows one
    # is skipped by reason rather than read wrongly -- the same rule as an
    # unrecognised `.x` file.
    function dg_error(text,   n, rest, grp, v) {
        rest = text
        # The regexp and the comment: up to two quoted strings. Consumed
        # before looking for a brace, since a regexp may contain one.
        for (n = 0; n < 2 && match(rest, /^[ \t]*"([^"\\]|\\.)*"/); n++)
            rest = substr(rest, RSTART + RLENGTH)
        if (n == 0) { skip = "unrecognised dg-error form"; return }
        if (rest ~ /^[ \t]*\{/) {
            grp = first_group(rest)
            rest = substr(rest, GRP_END + 1)
            # Strip the outer braces; what is left must be `target <selector>`.
            grp = substr(grp, index(grp, "{") + 1)
            sub(/\}[ \t]*$/, "", grp)
            if (!match(grp, /^[ \t]*target[ \t]/)) { skip = "unrecognised dg-error form"; return }
            v = sel_eval(substr(grp, RLENGTH + 1))
            if (v == SEL_UNKNOWN) { skip = "dg-error selector names a target not modelled"; return }
            if (v == SEL_FALSE) return
        }
        # Only the directive'"'"'s own close may follow. Anything else is a line
        # number or a shape not listed above.
        if (rest !~ /^[ \t]*\}/) { skip = "unrecognised dg-error form"; return }
        errs = errs " " FNR
    }

    # Does the selector that follows a dg-skip-if apply to this run?
    #
    # Two groups matter: the targets it names, and the option strings. A target
    # group naming only other architectures does not apply here; `*-*-*` names
    # every target, and then the option group decides -- `{ *-*-* } "-O1"`
    # skips only at -O1.
    #
    # The target group is an expression, not a word list: `{ ! { x86_64-*-* } }`
    # and `{ { x86_64-*-* } && { ia32 } }` both occur, and reading every word
    # as a target that might match skipped 990413-2 and 20000804-1 on the one
    # target each of them is meant to run on.
    function dg_skip_applies(text,   grp, rest, v) {
        grp = first_group(text)
        if (grp == "") return 1           # no selector at all: unconditional
        rest = substr(text, GRP_END + 1)
        v = sel_eval(grp)
        # An effective target we do not model leaves the answer unknown. Assume
        # the skip applies, so an unread selector errs towards skipping rather
        # than towards a failure we would have to triage as a target question.
        if (v == SEL_FALSE) return 0
        return option_group_applies(rest)
    }

    # Evaluate a target selector expression: SEL_FALSE, SEL_TRUE or
    # SEL_UNKNOWN. Shared by `dg-skip-if` and `dg-error { target ... }`.
    function sel_eval(grp,   i, n) {
        gsub(/"/, " ", grp)
        gsub(/\{/, " { ", grp)
        gsub(/\}/, " } ", grp)
        gsub(/!/, " ! ", grp)
        gsub(/&&/, " \\&\\& ", grp)
        gsub(/\|\|/, " || ", grp)
        NTK = split(grp, TK, /[ \t]+/)
        # split() leaves an empty first field for leading blanks; drop empties.
        n = 0
        for (i = 1; i <= NTK; i++) if (TK[i] != "") TK[++n] = TK[i]
        NTK = n
        PTK = 1
        return sel_or()
    }

    # A three-valued evaluator over the tokens in TK[PTK..NTK]: SEL_FALSE,
    # SEL_TRUE, or SEL_UNKNOWN for an effective target not modelled here.
    #
    #   or      := and { "||" and }
    #   and     := unary { "&&" unary }
    #   unary   := "!" unary | primary
    #   primary := "{" or { or } "}" | word
    #
    # Items side by side inside braces -- `{ avr-*-* pdp11-*-* }` -- are a
    # list, and a list matches when any of its items does.
    function sel_or(   v, w) {
        v = sel_and()
        while (PTK <= NTK && TK[PTK] == "||") { PTK++; w = sel_and(); v = sel_any(v, w) }
        return v
    }
    function sel_and(   v, w) {
        v = sel_unary()
        while (PTK <= NTK && TK[PTK] == "&&") { PTK++; w = sel_unary(); v = sel_all(v, w) }
        return v
    }
    function sel_unary(   v) {
        if (PTK <= NTK && TK[PTK] == "!") {
            PTK++
            v = sel_unary()
            if (v == SEL_UNKNOWN) return v
            return v == SEL_TRUE ? SEL_FALSE : SEL_TRUE
        }
        return sel_primary()
    }
    function sel_primary(   v, tok) {
        if (PTK > NTK) return SEL_UNKNOWN
        tok = TK[PTK++]
        if (tok == "{") {
            v = SEL_FALSE
            while (PTK <= NTK && TK[PTK] != "}") v = sel_any(v, sel_or())
            PTK++                         # the closing brace
            return v
        }
        return sel_word(tok)
    }
    function sel_any(a, b) {
        if (a == SEL_TRUE || b == SEL_TRUE) return SEL_TRUE
        if (a == SEL_UNKNOWN || b == SEL_UNKNOWN) return SEL_UNKNOWN
        return SEL_FALSE
    }
    function sel_all(a, b) {
        if (a == SEL_FALSE || b == SEL_FALSE) return SEL_FALSE
        if (a == SEL_UNKNOWN || b == SEL_UNKNOWN) return SEL_UNKNOWN
        return SEL_TRUE
    }

    # A target triple is globbed; anything else is an effective-target name.
    # These are the ones the torture selectors name whose answer is the same
    # on every target c17 builds for: both are hosted LP64 Linux, and each is
    # the check_effective_target_* of the same name in gcc lib/target-supports.exp.
    # `size32plus` (32-bit or wider size_t and pointers, no small address
    # space) and `int32plus` are true of LP64; pr46534'"'"'s dg-error is guarded
    # by `! size32plus`, so on our targets it does not apply.
    function sel_word(tok) {
        if (index(tok, "-") > 0) return glob_match(tok, TRIPLE) ? SEL_TRUE : SEL_FALSE
        if (tok == "freestanding" || tok == "ia32" || tok == "ilp32") return SEL_FALSE
        if (tok == "lp64" || tok == "untyped_assembly" || tok == "size20plus" ||
            tok == "size32plus" || tok == "int32plus") return SEL_TRUE
        return SEL_UNKNOWN
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

    END { sub(/^ /, "", errs); printf "%s|%s|%s|%s|%s|%s", skip, flags, mult, stack, dgdo, errs }
    ' "$1"
}

# A test's `.x` file is Tcl that gcc's harness runs before the test. Treating
# its mere existence as "skip" threw away 26 builtins tests, nearly all of
# whose `.x` files return 0 here: they add a flag, drop one torture level, or
# exclude a target that is not ours. The suite uses a handful of shapes, and
# this reads exactly those:
#
#   set additional_flags X                      -> extra flags
#   torture_eval_before_compile ... {*-Og*} ... -> skip that level
#   istarget "nvptx-*-*" ... return 1           -> skip on a matching target
#   check_effective_target_freestanding         -> false: we are hosted
#   check_effective_target_nonlocal_goto        -> true, as for gcc here
#
# Anything else is reported as an unrecognised `.x` file, by name, so a new
# shape is seen rather than guessed at.
#
# Prints "skip:<reason>", "flags:<flags>", or nothing.
x_file_verdict() {
    local x="${1%.c}.x" opt="$2"
    [ -f "$x" ] || return 0
    awk -v OPT="$opt" -v TRIPLE="$TORTURE_TRIPLE" '
        /^[ \t]*#/ { next }
        { body = body " " $0 }
        END {
            flags = ""
            if (match(body, /set additional_flags[ \t]+("[^"]*"|[^ \t]+)/)) {
                flags = substr(body, RSTART, RLENGTH)
                sub(/set additional_flags[ \t]+/, "", flags)
                gsub(/"/, "", flags)
            }
            rest = body
            while (match(rest, /string match \{[^}]*\}/)) {
                pat = substr(rest, RSTART + 14, RLENGTH - 15)
                rest = substr(rest, RSTART + RLENGTH)
                gsub(/\*/, "", pat)
                if (index(OPT, pat)) { print "skip:the test'"'"'s .x file skips " pat; exit }
            }
            rest = body
            while (match(rest, /istarget "[^"]*"/)) {
                pat = substr(rest, RSTART + 10, RLENGTH - 11)
                rest = substr(rest, RSTART + RLENGTH)
                re = pat; gsub(/[.+^$()|\\]/, "\\&", re); gsub(/\*/, ".*", re)
                if (TRIPLE ~ ("^" re "$")) { print "skip:the test'"'"'s .x file excludes this target"; exit }
            }
            # What is left must be the shapes above and nothing else.
            known = body
            gsub(/load_lib target-supports\.exp/, "", known)
            gsub(/set additional_flags[ \t]+("[^"]*"|[^ \t]+)/, "", known)
            gsub(/set torture_eval_before_compile \{[^{}]*\{[^{}]*\{[^{}]*\}[^{}]*\}[^{}]*\{[^{}]*\}[^{}]*\}/, "", known)
            gsub(/if \{ *!? *\[check_effective_target_(freestanding|nonlocal_goto)\] *\} *\{[^}]*\}/, "", known)
            gsub(/if \[istarget "[^"]*"\] *\{[^}]*\}/, "", known)
            gsub(/return 0;?/, "", known)
            if (known !~ /^[ \t;]*$/) { print "skip:unrecognised .x file"; exit }
            if (flags != "") print "flags:" flags
        }' "$x"
}


# Tests skipped by name: `<sub-suite>/<name>  <target>  <reason>`, target
# `all`, `host` or `aarch64`. Everything else is attempted. Only what the
# project has decided not to do belongs here -- nested functions, VLA struct
# members, post-C17 features, another compiler's internals -- and the odd
# test gcc itself rejects. A failure that is a c17 gap or bug is fixed, not
# listed.
SKIP_BY_NAME='
builtins/uabs-1        all      post-C17 feature
builtins/uabs-2        all      post-C17 feature
builtins/uabs-3        all      post-C17 feature
compile/20010226-1     all      nested functions
compile/20010605-1     all      nested functions
compile/20010903-2     all      nested functions
compile/20011023-1     all      nested functions
compile/20020210-1     all      VLA as a struct member
compile/20020309-1     all      nested functions
compile/20021204-1     all      nested functions
compile/20030224-1     all      VLA as a struct member
compile/20030418-1     all      nested functions
compile/20030716-1     all      nested functions
compile/20031011-1     all      nested functions
compile/20031023-1     all      needs 64-bit stack frames
compile/20031023-2     all      needs 64-bit stack frames
compile/20031023-3     all      needs 64-bit stack frames
compile/20031023-4     all      needs 64-bit stack frames
compile/20040310-1     all      nested functions
compile/20040317-3     all      nested functions
compile/20040323-1     all      nested functions
compile/20050119-1     all      nested functions
compile/20050122-2     all      nested functions (non-local goto to a __label__)
compile/20050801-2     all      VLA as a struct member
compile/920415-1       all      nested functions (non-local goto to a __label__)
compile/920428-4       all      VLA as a struct member
compile/920501-16      all      VLA as a struct member
compile/930506-2       all      nested functions
compile/950919-1       all      GNU preprocessor assertions (#cpu)
compile/951116-1       all      nested functions
compile/dll            all      gcc rejects it too
compile/mipscop-1      all      inline assembly for another target
compile/mipscop-2      all      inline assembly for another target
compile/mipscop-3      all      inline assembly for another target
compile/mipscop-4      all      inline assembly for another target
compile/nested-1       all      nested functions
compile/nested-2       all      nested functions
compile/nested-3       all      nested functions
compile/pr110386-2     all      -mavx: above the SSE4.2 ceiling
compile/pr111059-10    all      post-C17 feature
compile/pr111059-11    all      post-C17 feature
compile/pr111059-12    all      post-C17 feature
compile/pr111059-7     all      post-C17 feature
compile/pr111059-8     all      post-C17 feature
compile/pr111059-9     all      post-C17 feature
compile/pr111911-2     all      post-C17 feature
compile/pr115143-2     all      the GIMPLE front end of gcc (-fgimple)
compile/pr115143-3     all      the GIMPLE front end of gcc (-fgimple)
compile/pr21728        all      nested functions (non-local goto to a __label__)
compile/pr27528        aarch64  non-PIC "s" operands; gcc rejects them under PIC too
compile/pr27889        all      nested functions
compile/pr35006        all      nested functions
compile/pr39394        all      VLA as a struct member
compile/pr42956        all      VLA as a struct member
compile/pr77754-6      all      VLA as a struct member
compile/pr82564        all      VLA as a struct member
compile/pr99324        all      nested functions
compile/stack-check-1  all      needs 64-bit stack frames
execute/20000822-1     all      nested functions
execute/20010209-1     all      nested functions
execute/20010605-1     all      nested functions
execute/20020412-1     all      VLA as a struct member
execute/20030501-1     all      nested functions
execute/20040308-1     all      VLA as a struct member
execute/20040423-1     all      VLA as a struct member
execute/20040520-1     all      nested functions
execute/20041218-2     all      VLA as a struct member
execute/20061220-1     all      nested functions
execute/20070919-1     all      VLA as a struct member
execute/20090219-1     all      nested functions
execute/920415-1       all      nested functions (non-local goto to a __label__)
execute/920428-2       all      nested functions (non-local goto to a __label__)
execute/920501-7       all      nested functions (non-local goto to a __label__)
execute/920612-2       all      nested functions
execute/920721-4       all      nested functions (non-local goto to a __label__)
execute/921017-1       all      nested functions
execute/921215-1       all      nested functions
execute/931002-1       all      nested functions
execute/align-nest     all      VLA as a struct member
execute/comp-goto-2    all      nested functions (non-local goto to a __label__)
execute/eeprof-1       all      -finstrument-functions
execute/nest-align-1   all      nested functions
execute/nest-stdar-1   all      nested functions
execute/nestfunc-1     all      nested functions
execute/nestfunc-2     all      nested functions
execute/nestfunc-3     all      nested functions
execute/nestfunc-5     all      nested functions (non-local goto to a __label__)
execute/nestfunc-6     all      nested functions (non-local goto to a __label__)
execute/nestfunc-7     all      nested functions
execute/pr103405       all      nested functions
execute/pr117432       all      gcc rejects it too
execute/pr123864       all      implicit int; gcc 14 rejects it too
execute/pr123978       all      post-C17 feature
execute/pr124358       all      post-C17 feature
execute/pr125291       all      post-C17 feature
execute/pr22061-3      all      nested functions
execute/pr22061-4      all      nested functions
execute/pr24135        all      nested functions (non-local goto to a __label__)
execute/pr41935        all      VLA as a struct member
execute/pr47237        all      __builtin_apply
execute/pr51447        all      nested functions (non-local goto to a __label__)
execute/pr71494        all      nested functions
execute/pr80692        all      post-C17 feature
execute/pr82210        all      VLA as a struct member
ieee/bfloat16-builtin-issignaling-1  all  __bf16
ieee/float128x-builtin-issignaling-1 all  _Float128x: no target has it, gcc included
'

# `dg-do compile` tests whose asm template is deliberately not an instruction
# -- `asm("%0" :: "r"(1.5))`, `asm("f")` -- so they only mean something up to
# `-S`, which is where gcc stops. aarch64 mode assembles every other compile
# test, since rejected output is what it exists to catch; these stop at `-S`.
TEMPLATE_NOT_ASSEMBLED=" compile/920520-1 compile/920521-1 "

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
    local tag="$TAG_PREFIX$suite/$base@${opt// /_}"
    # The tag carries a `/`, so it is not a file name; scratch files get the
    # same token with the separator flattened.
    local exe="$work/bin/${tag//\//-}.$$"
    local log="$exe.log"

    local xv xflags=""
    xv=$(x_file_verdict "$src" "$opt")
    case "$xv" in
        skip:*)  echo "SKIP	$tag	${xv#skip:}"; return;;
        flags:*) xflags=${xv#flags:};;
    esac
    local key="$suite/$base"
    local why
    why=$(printf '%s\n' "$SKIP_BY_NAME" | awk -v k="$key" -v t="$TORTURE_TARGET" \
        '$1 == k && ($2 == "all" || $2 == t) { $1 = ""; $2 = ""; sub(/^ +/, ""); print; exit }')
    if [ -n "$why" ]; then
        echo "SKIP	$tag	$why"; return
    fi
    local scan skip flags mult stack dgdo errlines
    scan=$(dg_scan "$src" "$opt")
    skip=${scan%%|*}; scan=${scan#*|}
    flags=${scan%%|*}; scan=${scan#*|}
    mult=${scan%%|*}; scan=${scan#*|}
    stack=${scan%%|*}; scan=${scan#*|}
    dgdo=${scan%%|*}; errlines=${scan#*|}

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
    # A `.x` file's flags join the test's own, with the same -std= handling:
    # c17 has one language mode, and a dialect the test asks for is either
    # honoured by name above or out of scope.
    [ -n "$xflags" ] && flags="$flags $(printf '%s' "$xflags" | sed -E 's/-std=[a-z0-9:]+//g')"
    # `dg-require-stack-size` is an expression over integers.
    if [ -n "$stack" ]; then
        case "$stack" in
            *[!0-9xXa-fA-F\ +*\(\)-]*) stack="";;
            *) stack=$(( stack ));;
        esac
    fi

    local ctimeout=$((30 * mult)) rtimeout=$((20 * mult))

    # An applicable `dg-error` makes this an expect-reject test: it is never
    # built further or run, whatever its sub-suite or dg-do.
    if [ -n "$errlines" ]; then
        expect_reject "$ctimeout" "$cc" "$opt $extra $flags" "$src" "$exe" "$log" "$errlines" "$tag"
        rm -f "$log"
        return
    fi

    # A compile that runs out of time is not a compile error, and reporting it
    # as one hides it: the log is empty, so the failure reads as a mystery.
    # `pr28982b` passes a 256 KB struct by value and takes c17 65 seconds
    # against gcc's 0.02, which is how this was found.
    # shellcheck disable=SC2086
    if [ "$mode" = compile ] || [ "$mode" = assemble ]; then
        target_compile_only "$mode" "$ctimeout" "$cc" "$opt $extra $flags" "$src" "$exe" "$log" "$key"
        local crc=$?
        case $crc in
            0)   echo "PASS	$tag	";;
            124) echo "CTIMEOUT	$tag	compile exceeded ${ctimeout}s";;
            *)   echo "CFAIL	$tag	$(head -c 160 "$log" | tr '\n' ' ')";;
        esac
        rm -f "$log"
        return
    fi

    target_build "$ctimeout" "$cc" "$opt $extra $flags" "$exe" "$log" "$src" $companions
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
    target_run "$rtimeout" "$exe" "$stack"
    local rc=$?
    rm -f "$exe" "$log"
    case $rc in
        0)   echo "PASS	$tag	";;
        124) echo "TIMEOUT	$tag	";;
        *)   echo "RFAIL	$tag	exit=$rc";;
    esac
}
# A test with applicable `dg-error`s passes when c17 rejects it -- non-zero
# status -- with an error on every marked line and on no other line of the
# test file, which is gcc's rule too (an unmarked error is gcc's "excess
# errors" failure). Errors are matched by c17's `<file>:<LINE>:<COL>: error:`
# on the test's own file name; a header's line numbers mean nothing here.
#
# The message regexp is NOT matched. It is gcc's wording ("void value not
# ignored as it ought to be"), and holding c17 to another compiler's prose
# would turn a correct rejection into a failure over phrasing. The line is
# the part of the contract that says c17 found the right defect.
expect_reject() {
    local t="$1" cc="$2" flags="$3" src="$4" exe="$5" log="$6" want="$7" tag="$8"
    # shellcheck disable=SC2086
    timeout "$t" "$cc" $TARGET_FLAGS $flags -w -S "$src" -o "$exe.s" >"$log" 2>&1
    local crc=$?
    rm -f "$exe.s"
    if [ $crc -eq 124 ]; then
        echo "CTIMEOUT	$tag	compile exceeded ${t}s"; return
    fi
    if [ $crc -eq 0 ]; then
        echo "CFAIL	$tag	accepted invalid code (dg-error on line $want)"; return
    fi
    local got
    got=$(awk -v F="$(basename "$src")" '
        { i = index($0, ": error:"); if (i == 0) next
          head = substr($0, 1, i - 1)
          n = split(head, p, ":")
          if (n < 3) next
          f = p[1]; for (k = 2; k <= n - 2; k++) f = f ":" p[k]
          sub(/.*\//, "", f)
          if (f == F && p[n-1] ~ /^[0-9]+$/) print p[n-1] }' "$log" | sort -nu | tr '\n' ' ')
    local n missing="" excess=""
    for n in $want; do
        case " $got" in *" $n "*) ;; *) missing="$missing $n";; esac
    done
    for n in $got; do
        case " $want " in *" $n "*) ;; *) excess="$excess $n";; esac
    done
    if [ -n "$missing" ]; then
        echo "CFAIL	$tag	missing error on line${missing}"
    elif [ -n "$excess" ]; then
        echo "CFAIL	$tag	error on unmarked line${excess}"
    else
        echo "PASS	$tag	"
    fi
}

# ----------------------------------------------------------- target steps
#
# The three things a target mode changes; everything above is shared.

# Build `src` without linking. The host stops where gcc does: `-S` for
# `dg-do compile`, `-c` for `assemble`. In aarch64 mode the output is always
# assembled with the cross assembler: rejected assembly is the defect that
# mode exists to catch, and stopping at `-S` would hide it.
# Returns the compiler's status, 124 for a timeout.
target_compile_only() {
    local mode="$1" t="$2" cc="$3" flags="$4" src="$5" exe="$6" log="$7" key="$8"
    if [ "$TORTURE_TARGET" = aarch64 ]; then
        # shellcheck disable=SC2086
        timeout "$t" "$cc" $TARGET_FLAGS $flags -w -S "$src" -o "$exe.s" >"$log" 2>&1
        local crc=$?
        case "$TEMPLATE_NOT_ASSEMBLED" in
            *" $key "*) rm -f "$exe.s"; return $crc;;
        esac
        if [ $crc -eq 0 ]; then
            aarch64-linux-gnu-as "$exe.s" -o "$exe.o" >"$log" 2>&1
            crc=$?
        fi
        rm -f "$exe.s" "$exe.o"
        return $crc
    fi
    local stop_at=-S out="$exe.s"
    [ "$mode" = assemble ] && { stop_at=-c; out="$exe.o"; }
    # shellcheck disable=SC2086
    timeout "$t" "$cc" $flags -w "$stop_at" "$src" -o "$out" >"$log" 2>&1
    local crc=$?
    rm -f "$out"
    return $crc
}

# Build an executable from one or more sources.
target_build() {
    local t="$1" cc="$2" flags="$3" exe="$4" log="$5"
    shift 5
    if [ "$TORTURE_TARGET" = aarch64 ]; then
        local s objs="" i=0 crc
        for s in "$@"; do
            i=$((i + 1))
            # shellcheck disable=SC2086
            timeout "$t" "$cc" $TARGET_FLAGS $flags -w -S "$s" -o "$exe.$i.s" >>"$log" 2>&1
            crc=$?
            if [ $crc -ne 0 ]; then rm -f "$exe".*.s; return $crc; fi
            objs="$objs $exe.$i.s"
        done
        # shellcheck disable=SC2086
        aarch64-linux-gnu-gcc -static -w $objs -o "$exe" -lm >>"$log" 2>&1
        crc=$?
        rm -f "$exe".*.s
        return $crc
    fi
    # shellcheck disable=SC2086
    timeout "$t" "$cc" $flags -w "$@" -o "$exe" -lm >"$log" 2>&1
}

# Run a built test, honouring `dg-require-stack-size`. Under qemu-user the
# guest stack is QEMU_STACK_SIZE, not the host's ulimit, and emulation is
# slower, so the run gets three times the time.
target_run() {
    local t="$1" exe="$2" stack="$3" kb=""
    if [ -n "$stack" ]; then
        kb=$(( (stack + 1023) / 1024 * 2 ))
        [ "$kb" -lt 8192 ] && kb=8192
    fi
    if [ "$TORTURE_TARGET" = aarch64 ]; then
        if [ -n "$kb" ]; then
            QEMU_STACK_SIZE=$((kb * 1024)) QEMU_LD_PREFIX=/usr/aarch64-linux-gnu \
                timeout $((t * 3)) qemu-aarch64-static "$exe" >/dev/null 2>&1
        else
            QEMU_LD_PREFIX=/usr/aarch64-linux-gnu \
                timeout $((t * 3)) qemu-aarch64-static "$exe" >/dev/null 2>&1
        fi
        return
    fi
    if [ -n "$kb" ]; then
        ( ulimit -s "$kb" 2>/dev/null; timeout "$t" "$exe" >/dev/null 2>&1 )
    else
        timeout "$t" "$exe" >/dev/null 2>&1
    fi
}

# How a sub-suite is built when the test says nothing. `dg-do` overrides it.
default_mode() {
    case "$1" in
        compile) echo compile;;
        *)       echo run;;
    esac
}

export -f run_one dg_scan x_file_verdict default_mode expect_reject
export -f target_compile_only target_build target_run
export TORTURE_TARGET TARGET_FLAGS TAG_PREFIX TEMPLATE_NOT_ASSEMBLED SKIP_BY_NAME
export TORTURE_TRIPLE

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
echo "target:    $TORTURE_TARGET ($TORTURE_TRIPLE)"
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
#
# A filtered run (-f) may legitimately select only tests the baseline does not
# hold yet -- checking a fix for tests that never passed -- so it reports that
# and goes on; an unfiltered run still catches a changed tag format here.
if [ ! -s "$RELEVANT" ] && [ -n "$FILTER" ]; then
    echo
    echo "NOTE: no baseline test matches -f '$FILTER'; nothing can regress."
elif [ ! -s "$RELEVANT" ]; then
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
