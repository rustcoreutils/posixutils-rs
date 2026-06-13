#!/usr/bin/env python3
"""Regenerate the inline `test_formatting` snapshot expectations in
man/man_util/formatter.rs.

The chosen test strategy for the man renderer rewrite is self-blessed snapshots
(no external mandoc oracle). When the renderer's output legitimately changes,
run:

    rm -f /tmp/man_bless.txt
    MAN_BLESS=/tmp/man_bless.txt cargo test -p posixutils-man --lib
    python3 dev/bless_man_snapshots.py man/man_util/formatter.rs /tmp/man_bless.txt
    cargo test -p posixutils-man --lib   # now green

`test_formatting` (gated on MAN_BLESS) appends `<call_line> <hex(actual)>` for
each mismatch, using #[track_caller] for the exact call-site line. This script
rewrites each test's `let output = "..."` literal with the actual output.

ALWAYS eyeball `git diff` afterward — blessing trusts the new output as correct.
"""
import sys


def main():
    if len(sys.argv) != 3:
        sys.exit(f"usage: {sys.argv[0]} <formatter.rs> <bless-records>")
    src_path, rec_path = sys.argv[1], sys.argv[2]

    # Parse records: last write for a given call line wins.
    recs = {}
    with open(rec_path, encoding="utf-8") as f:
        for raw in f:
            raw = raw.strip()
            if not raw:
                continue
            line_s, hex_s = raw.split(" ", 1)
            recs[int(line_s)] = bytes.fromhex(hex_s).decode("utf-8")

    with open(src_path, encoding="utf-8") as f:
        lines = f.readlines()  # keep trailing newlines; index 0 == source line 1

    # Apply from the bottom up so earlier line numbers stay valid.
    for call_line in sorted(recs, reverse=True):
        actual = recs[call_line]
        call_idx = call_line - 1  # 0-based
        if "test_formatting(input, output)" not in lines[call_idx]:
            sys.exit(f"line {call_line}: not a test_formatting(input, output) call")

        # Scan upward for the matching `let output =`.
        out_idx = None
        for i in range(call_idx - 1, -1, -1):
            if "let output =" in lines[i]:
                out_idx = i
                break
        if out_idx is None:
            sys.exit(f"line {call_line}: no preceding `let output =` found")

        out_indent = lines[out_idx][: len(lines[out_idx]) - len(lines[out_idx].lstrip())]
        call_indent = lines[call_idx][: len(lines[call_idx]) - len(lines[call_idx].lstrip())]

        esc = actual.replace("\\", "\\\\").replace('"', '\\"')
        # Real newlines are kept: each rendered line (with its own leading spaces,
        # which are string content) sits at source column 0, matching house style.
        block = (
            f'{out_indent}let output = "{esc}";\n'
            f"{call_indent}test_formatting(input, output);\n"
        )
        lines[out_idx : call_idx + 1] = [block]

    with open(src_path, "w", encoding="utf-8") as f:
        f.writelines(lines)
    print(f"blessed {len(recs)} snapshot(s)")


if __name__ == "__main__":
    main()
