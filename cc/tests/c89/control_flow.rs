//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C89 Control Flow Mega-Test
//
// Consolidates: if/while/for/do-while/switch/goto/break/continue tests from c17/
//

use crate::common::{compile_and_run, compile_and_run_optimized};

// ============================================================================
// Mega-test: C89 control flow (loops, conditionals, jumps)
// ============================================================================

#[test]
fn c89_control_flow_mega() {
    let code = r#"
int main(void) {
    // ========== IF/ELSE SECTION (returns 1-19) ==========
    {
        int x = 10;

        // Basic if
        if (x > 5) {
            // pass
        } else {
            return 1;
        }

        // If-else branch
        if (x < 5) {
            return 2;
        } else {
            // pass
        }

        // Nested if
        int a = 10, b = 20;
        if (a < b) {
            if (b > 15) {
                // pass (correct path)
            } else {
                return 3;
            }
        } else {
            return 4;
        }

        // If-else-if chain
        x = 50;
        int result;
        if (x < 30) {
            result = 1;
        } else if (x < 40) {
            result = 2;
        } else if (x < 60) {
            result = 3;
        } else {
            result = 4;
        }
        if (result != 3) return 5;

        // Complex condition
        a = 5; b = 10;
        if (a < b && b < 20) {
            // pass
        } else {
            return 6;
        }

        if (a > b || b > 5) {
            // pass
        } else {
            return 7;
        }
    }

    // ========== WHILE LOOP SECTION (returns 20-39) ==========
    {
        int sum = 0, i = 1;

        // Basic while
        while (i <= 5) {
            sum = sum + i;
            i = i + 1;
        }
        if (sum != 15) return 20;  // 1+2+3+4+5

        // While with break
        i = 0;
        while (1) {
            i = i + 1;
            if (i == 5) break;
        }
        if (i != 5) return 21;

        // While with continue
        sum = 0;
        i = 0;
        while (i < 10) {
            i = i + 1;
            if ((i % 2) == 0) continue;  // Skip even
            sum = sum + i;
        }
        if (sum != 25) return 22;  // 1+3+5+7+9

        // Nested while
        sum = 0;
        i = 0;
        int j;
        while (i < 3) {
            j = 0;
            while (j < 3) {
                sum++;
                j++;
            }
            i++;
        }
        if (sum != 9) return 23;  // 3*3
    }

    // ========== DO-WHILE SECTION (returns 40-49) ==========
    {
        int sum = 0, i = 1;

        // Basic do-while
        do {
            sum = sum + i;
            i = i + 1;
        } while (i <= 5);
        if (sum != 15) return 40;

        // Do-while executes at least once
        i = 100;
        int count = 0;
        do {
            count++;
        } while (i < 10);  // False immediately
        if (count != 1) return 41;

        // Do-while with break
        i = 0;
        do {
            i = i + 1;
            if (i == 5) break;
        } while (1);
        if (i != 5) return 42;

        // Do-while with continue
        sum = 0;
        i = 0;
        do {
            i = i + 1;
            if ((i % 2) == 0) continue;
            sum = sum + i;
        } while (i < 10);
        if (sum != 25) return 43;

        // Nested do-while
        sum = 0;
        i = 0;
        int j;
        do {
            j = 0;
            do {
                sum++;
                j++;
            } while (j < 2);
            i++;
        } while (i < 3);
        if (sum != 6) return 44;  // 3*2
    }

    // ========== FOR LOOP SECTION (returns 50-69) ==========
    {
        int sum = 0, i;

        // Basic for
        for (i = 1; i <= 5; i++) {
            sum = sum + i;
        }
        if (sum != 15) return 50;

        // For with break
        for (i = 1; i <= 100; i++) {
            if ((i % 7) == 0) break;
        }
        if (i != 7) return 51;

        // For with continue
        sum = 0;
        for (i = 1; i <= 9; i++) {
            if ((i % 2) == 0) continue;
            sum = sum + i;
        }
        if (sum != 25) return 52;  // 1+3+5+7+9

        // For with early break
        sum = 0;
        for (i = 1; i <= 100; i++) {
            sum = sum + i;
            if (sum >= 10) break;
        }
        if (sum != 10) return 53;  // 1+2+3+4 = 10

        // Nested for (2 levels)
        sum = 0;
        int j;
        for (i = 0; i < 3; i++) {
            for (j = 0; j < 3; j++) {
                sum++;
            }
        }
        if (sum != 9) return 54;

        // Nested for (3 levels)
        sum = 0;
        int k;
        for (i = 0; i < 2; i++) {
            for (j = 0; j < 3; j++) {
                for (k = 0; k < 4; k++) {
                    sum++;
                }
            }
        }
        if (sum != 24) return 55;  // 2*3*4

        // For with empty parts
        i = 0;
        for (;;) {
            i++;
            if (i == 5) break;
        }
        if (i != 5) return 56;
    }

    // ========== SWITCH SECTION (returns 70-89) ==========
    {
        int x = 2, result = 0;

        // Basic switch
        switch (x) {
            case 1: result = 10; break;
            case 2: result = 20; break;
            case 3: result = 30; break;
            default: result = 99; break;
        }
        if (result != 20) return 70;

        // Switch fallthrough
        x = 1;
        result = 0;
        switch (x) {
            case 1:
                result = result + 1;
            case 2:
                result = result + 2;
                break;
            case 3:
                result = 30;
                break;
        }
        if (result != 3) return 71;  // 1 + 2 from fallthrough

        // Switch default
        x = 99;
        result = 0;
        switch (x) {
            case 1: result = 10; break;
            case 2: result = 20; break;
            default: result = 42; break;
        }
        if (result != 42) return 72;

        // Switch no default, no match
        x = 99;
        result = 5;
        switch (x) {
            case 1: result = 10; break;
            case 2: result = 20; break;
        }
        if (result != 5) return 73;  // unchanged

        // Switch negative case
        x = -1;
        result = 0;
        switch (x) {
            case -2: result = 1; break;
            case -1: result = 2; break;
            case 0: result = 3; break;
        }
        if (result != 2) return 74;

        // Switch zero case
        x = 0;
        result = 99;
        switch (x) {
            case 0: result = 0; break;
            case 1: result = 1; break;
        }
        if (result != 0) return 75;

        // Enum with switch
        enum Color { RED, GREEN, BLUE };
        enum Color c = GREEN;
        result = 0;
        switch (c) {
            case RED: result = 1; break;
            case GREEN: result = 2; break;
            case BLUE: result = 3; break;
        }
        if (result != 2) return 76;
    }

    // ========== GOTO SECTION (returns 90-99) ==========
    {
        int x = 1;

        // Forward goto
        goto skip;
        x = 99;  // Should be skipped
    skip:
        if (x != 1) return 90;

        // Backward goto (loop-like)
        int sum = 0, i = 1;
    loop:
        sum = sum + i;
        i = i + 1;
        if (i <= 5)
            goto loop;
        if (sum != 15) return 91;

        // Multiple labels
        x = 0;
        goto first;
    second:
        x = x + 10;
        goto done;
    first:
        x = x + 1;
        goto second;
    done:
        if (x != 11) return 92;
    }

    // ========== ENUM SECTION (returns 100-109) ==========
    {
        // Basic enum
        enum Color { RED, GREEN, BLUE };
        enum Color c = GREEN;
        if (c != 1) return 100;
        if (RED != 0) return 101;
        if (BLUE != 2) return 102;

        // Explicit values
        enum Status { OK = 0, ERROR = 10, PENDING = 100 };
        if (OK != 0) return 103;
        if (ERROR != 10) return 104;
        if (PENDING != 100) return 105;

        // Negative values
        enum Level { LOW = -1, MEDIUM = 0, HIGH = 1 };
        if (LOW != -1) return 106;
        if (MEDIUM != 0) return 107;
        if (HIGH != 1) return 108;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_control_flow_mega", code, &[]), 0);
}

// ============================================================================
// Mega-test: Statement edge cases (null stmt, empty blocks, dangling else,
// omitted for clauses, void return, expression statements)
// ============================================================================

#[test]
fn c89_statements_edge_cases_mega() {
    let code = r#"
// Helper: void return
void set_value(int *p, int v) {
    *p = v;
    return;
}

void set_if(int *p, int v, int cond) {
    if (!cond) return;
    *p = v;
}

int main(void) {
    // ========== NULL STATEMENT (returns 1-9) ==========
    {
        ;                          // standalone null statement

        int i = 0;
        for (i = 0; i < 5; i++) ; // null for body
        if (i != 5) return 1;

        while (0) ;                // null while body (never executes)

        if (1) ; else ;            // null in both if/else branches

        do ; while (0);            // null do-while body

        // Multiple null statements
        ; ; ;
    }

    // ========== EMPTY BLOCKS (returns 10-19) ==========
    {
        {}                         // standalone empty block

        if (1) {} else {}          // empty blocks in if/else

        int i;
        for (i = 0; i < 3; i++) {} // empty for body
        if (i != 3) return 10;

        while (0) {}               // empty while body

        do {} while (0);           // empty do-while body

        // Nested empty blocks
        { { { } } }
    }

    // ========== DANGLING ELSE (returns 20-29) ==========
    {
        // C99 6.8.4.1: else binds to nearest if
        int x = 0;
        if (0)
            if (1)
                x = 1;
            else
                x = 2;
        // outer if(0) is false => entire body skipped, x stays 0
        if (x != 0) return 20;

        int y = 0;
        if (1)
            if (0)
                y = 1;
            else
                y = 2;
        // outer if(1) true, inner if(0) false => else executes, y = 2
        if (y != 2) return 21;

        // Triple nesting
        int z = 0;
        if (1)
            if (1)
                if (0)
                    z = 1;
                else
                    z = 2;
        if (z != 2) return 22;
    }

    // ========== OMITTED FOR CLAUSES (returns 30-39) ==========
    {
        // for(;cond;) — omitted init and post
        int i = 0, sum = 0;
        for (; i < 5;) { sum += i; i++; }
        if (sum != 10) return 30;   // 0+1+2+3+4

        // for(init;;) — omitted cond and post (infinite, break out)
        for (i = 0;;) { if (i >= 3) break; i++; }
        if (i != 3) return 31;

        // for(init; cond;) — omitted post
        sum = 0;
        for (i = 0; i < 5;) { sum += i; i++; }
        if (sum != 10) return 32;

        // for(; ;post) — omitted init and cond
        i = 0;
        for (; ; i++) { if (i >= 4) break; }
        if (i != 4) return 33;

        // for(init; ;post) — omitted cond only
        sum = 0;
        for (i = 0; ; i++) {
            if (i >= 5) break;
            sum += i;
        }
        if (sum != 10) return 34;
    }

    // ========== VOID RETURN (returns 40-49) ==========
    {
        int val = 0;
        set_value(&val, 42);
        if (val != 42) return 40;

        // Early void return (cond = 0 => returns early, no write)
        val = 0;
        set_if(&val, 99, 0);
        if (val != 0) return 41;

        // Normal path (cond = 1 => writes)
        set_if(&val, 99, 1);
        if (val != 99) return 42;
    }

    // ========== EXPRESSION STATEMENTS (returns 50-59) ==========
    {
        int a = 0;
        a++;                       // postfix increment as expr stmt
        if (a != 1) return 50;

        a += 5;                    // compound assignment as expr stmt
        if (a != 6) return 51;

        ++a;                       // prefix increment as expr stmt
        if (a != 7) return 52;

        a--;                       // postfix decrement
        if (a != 6) return 53;

        (void)a;                   // cast-to-void expr stmt

        int b = 10;
        a = b = 20;               // chained assignment as expr stmt
        if (a != 20) return 54;
        if (b != 20) return 55;

        a, b;                      // comma operator as expr stmt (no side effect)
    }

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c89_statements_edge_cases_mega", code, &[]),
        0
    );
}

// ============================================================================
// Test: Duff's device (switch interleaved with do-while)
// ============================================================================

#[test]
fn c89_duffs_device() {
    let code = r#"
#include <string.h>

// Classic Duff's device: unrolled memory copy
void duff_copy(char *to, const char *from, int count) {
    int n = (count + 7) / 8;
    switch (count % 8) {
    case 0: do { *to++ = *from++;
    case 7:      *to++ = *from++;
    case 6:      *to++ = *from++;
    case 5:      *to++ = *from++;
    case 4:      *to++ = *from++;
    case 3:      *to++ = *from++;
    case 2:      *to++ = *from++;
    case 1:      *to++ = *from++;
            } while (--n > 0);
    }
}

int main(void) {
    // Test 1: copy "hello world" (11 chars)
    {
        const char *src = "hello world";
        char dst[12];
        memset(dst, 0, sizeof(dst));
        duff_copy(dst, src, 11);
        dst[11] = '\0';
        if (strcmp(dst, "hello world") != 0) return 1;
    }

    // Test 2: copy exactly 8 bytes (no partial first iteration)
    {
        const char *src = "abcdefgh";
        char dst[9];
        memset(dst, 0, sizeof(dst));
        duff_copy(dst, src, 8);
        dst[8] = '\0';
        if (strcmp(dst, "abcdefgh") != 0) return 2;
    }

    // Test 3: copy 1 byte
    {
        const char *src = "X";
        char dst[2];
        memset(dst, 0, sizeof(dst));
        duff_copy(dst, src, 1);
        dst[1] = '\0';
        if (dst[0] != 'X') return 3;
    }

    // Test 4: copy 16 bytes (two full iterations)
    {
        const char *src = "0123456789abcdef";
        char dst[17];
        memset(dst, 0, sizeof(dst));
        duff_copy(dst, src, 16);
        dst[16] = '\0';
        if (strcmp(dst, "0123456789abcdef") != 0) return 4;
    }

    // Test 5: copy 3 bytes (partial first iteration)
    {
        const char *src = "abc";
        char dst[4];
        memset(dst, 0, sizeof(dst));
        duff_copy(dst, src, 3);
        dst[3] = '\0';
        if (strcmp(dst, "abc") != 0) return 5;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_duffs_device", code, &[]), 0);
}

/// A `switch` whose body is not a compound statement dropped the test (#C99).
///
/// `case E : statement` is one labeled statement in the grammar, but the AST
/// models the label as a sibling marker that carries no statement. Inside a
/// block that is sound -- marker and statement stay adjacent items of one list.
/// A non-compound body has room for exactly one statement, so the marker took
/// the whole body and the labeled statement escaped to become the *next*
/// statement of the enclosing block, reached unconditionally: `f(2)` on
/// `switch (x) case 1: return 2;` returned **2**, where gcc returns 0.
///
/// Both label kinds are covered, and the block form is kept alongside as a
/// control, since it was always correct and must stay so.
#[test]
fn c89_switch_with_a_non_compound_body_still_tests_the_value() {
    let code = r#"
static int one_case(int x)      { switch (x) case 1: return 2; return 0; }
static int one_default(int x)   { switch (x) default: return 7; return 0; }
static int two_labels(int x)    { switch (x) case 1: case 2: return 5; return 0; }
static int empty_labeled(int x) { switch (x) case 1: ; return 9; }

/* The block form, which never had the defect. */
static int blocked(int x) { switch (x) { case 1: return 2; } return 0; }

/* A non-compound body with no label at all is legal and does nothing; it must
   keep compiling and must not run its body. */
static int unlabelled(int x) { int n = 0; switch (x) n = 5; return n; }

/* A loop, `if`, `do` and `for` as the non-compound body, each with the case
   inside it. These are the second half of the defect: the label is not a
   prefix here, so the parser leaves the body alone, and it was the linearizer
   that collected no cases from anything but a block -- emitting a switch with
   an empty table, so every value took the default edge and the body never
   ran. */
static int loop_body(int x) {
    int n = 0;
    switch (x) while (n < 3) { case 1: n++; }
    return n;
}
static int if_body(int x)   { int n = 0; switch (x) if (1) { case 1: n = 4; } return n; }

/* A statement standing before the first `case` has no edge from the switch and
   must not run. It was lowered into the very block the switch terminates, so
   it ran unconditionally -- in the block form too, which is why this is not
   merely a consequence of the non-compound fix. A declaration there is legal
   and its initializer is equally unreachable. */
static int prefix_stmt(int x) { int n = 0; switch (x) { n = 5; case 1: n += 1; } return n; }
static int prefix_only(int x) { int n = 0; switch (x) { n = 5; } return n; }
static int prefix_decl(int x) { int r = 0; switch (x) { int y = 7; case 1: r = 1; break; } return r; }
static int do_body(int x)   { int n = 0; switch (x) do { case 1: n++; } while (n < 2); return n; }
static int for_body(int x)  { int n = 0; switch (x) for (; n < 2;) { case 1: n++; } return n; }

int main(void) {
    if (one_case(1) != 2)  return 1;
    if (one_case(2) != 0)  return 2;   /* the regression */
    if (one_case(0) != 0)  return 3;

    if (one_default(5) != 7) return 4;
    if (one_default(1) != 7) return 5;

    if (two_labels(1) != 5) return 6;
    if (two_labels(2) != 5) return 7;
    if (two_labels(3) != 0) return 8;  /* neither label matches */

    if (empty_labeled(1) != 9) return 9;
    if (empty_labeled(2) != 9) return 10;

    if (blocked(1) != 2) return 11;
    if (blocked(2) != 0) return 12;

    if (unlabelled(1) != 0) return 13;
    if (unlabelled(9) != 0) return 14;

    if (loop_body(1) != 3) return 15;
    if (loop_body(9) != 0) return 16;
    if (if_body(1)   != 4) return 17;
    if (if_body(9)   != 0) return 18;
    if (do_body(1)   != 2) return 19;
    if (do_body(9)   != 0) return 20;
    if (for_body(1)  != 2) return 21;
    if (for_body(9)  != 0) return 22;

    if (prefix_stmt(9) != 0) return 23;
    if (prefix_stmt(1) != 1) return 24;
    if (prefix_only(1) != 0) return 25;
    if (prefix_decl(1) != 1) return 26;

    return 0;
}
"#;
    assert_eq!(compile_and_run("switch_non_compound_body", code, &[]), 0);
}

/// GNU case ranges: `case lo ... hi:`.
///
/// Measured as the most-used extension c17 rejected — 612 files in the Linux
/// tree, 18 in mesa, 4 in CPython. GCC requires whitespace around the `...`
/// (`case 1...9:` lexes as one pp-number and is rejected there too), so only
/// the spaced form is accepted.
///
/// A range is *not* expanded into individual labels: `case 0 ... 1000000:` is
/// legal, and every label costs a basic block and a comparison. It lowers to
/// `(x - lo) <=unsigned (hi - lo)`, one subtraction and one compare whatever
/// the width.
#[test]
fn c89_case_ranges() {
    let code = r#"
static int basic(int x) { switch (x) { case 1 ... 9: return 0; default: return 1; } }

static int edges(int x) { switch (x) { case 3 ... 7: return 1; default: return 0; } }

static int mixed(int x) {
    switch (x) {
    case 0:        return 10;
    case 1 ... 3:  return 20;
    case 9:        return 30;
    default:       return 40;
    }
}

/* A range falls through to the next label like any other. */
static int fallthrough(int x) {
    int n = 0;
    switch (x) {
    case 1 ... 3: n++;
    case 4:       n += 10; break;
    default:      n = 99;
    }
    return n;
}

/* Not expanded: a million values must still compile quickly. */
static int huge(int x) { switch (x) { case 0 ... 1000000: return 0; default: return 1; } }

static int negative(int x) { switch (x) { case -5 ... -1: return 0; default: return 1; } }

/* Wider than 32 bits, so the immediate does not fit a compare. */
static int wide(long x) {
    switch (x) { case 100000000000L ... 100000000010L: return 0; default: return 1; }
}

/* An empty range never matches; GCC warns and compiles. */
static int empty(int x) { switch (x) { case 9 ... 1: return 0; default: return 1; } }

int main(void)
{
    if (basic(5)) return 1;
    if (basic(0) != 1 || basic(10) != 1) return 2;

    /* Both endpoints are inclusive. */
    if (!edges(3) || !edges(7) || !edges(5)) return 3;
    if (edges(2) || edges(8)) return 4;

    if (mixed(0) != 10 || mixed(2) != 20 || mixed(9) != 30 || mixed(5) != 40) return 5;
    if (fallthrough(2) != 11) return 6;

    if (huge(0) || huge(999999) || huge(1000000)) return 7;
    if (huge(-1) != 1 || huge(1000001) != 1) return 8;

    if (negative(-3) || negative(-5) || negative(-1)) return 9;
    if (negative(0) != 1 || negative(-6) != 1) return 10;

    if (wide(100000000005L)) return 11;
    if (wide(99999999999L) != 1) return 12;

    if (empty(5) != 1 || empty(1) != 1 || empty(9) != 1) return 13;

    /* A single-value range is the ordinary label. */
    switch (3) { case 3 ... 3: break; default: return 14; }
    return 0;
}
"#;
    assert_eq!(compile_and_run("case_ranges", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("case_ranges_opt", code), 0);
}

/// GNU computed goto: the label address `&&label` and the indirect `goto *p`.
///
/// The reason the extension exists is interpreter dispatch, so that shape is
/// the centrepiece here. `&&label` needs no opcode of its own — every basic
/// block already emits an assembly label, and both backends already lower a
/// leading-`.` symbol to a pc-relative address — but the branch does: the
/// reachable blocks are not derivable from the instruction, so the CFG edges
/// to every address-taken label are recorded on the block, exactly as `asm
/// goto` does. Without them DCE deletes the targets.
#[test]
fn c89_computed_goto() {
    let code = r#"
/* A bytecode interpreter's dispatch loop: the shape the extension is for.
   The table is `static`, so the label addresses go through the data-image
   path rather than through runtime stores. */
static int run(const int *code, int n)
{
    static void *op[] = { &&ADD, &&MUL, &&END };
    int acc = 1, i = 0;
    goto *op[code[i]];
ADD: acc += 2; if (++i < n) goto *op[code[i]]; return acc;
MUL: acc *= 3; if (++i < n) goto *op[code[i]]; return acc;
END: return acc;
}

/* An automatic table: the runtime-store path. */
static int pick(int i)
{
    void *t[] = { &&A, &&B, &&C };
    goto *t[i];
A: return 10;
B: return 20;
C: return 30;
}

/* The address may be taken before the label is seen. */
static int forward(void)
{
    void *p = &&L;
    goto *p;
    return 1;
L:  return 0;
}

/* A label address outlives the block it was taken in. */
static int across_scope(void)
{
    void *p;
    { p = &&L; }
    goto *p;
L:  return 0;
}

/* And it is an ordinary value: it can live in a struct. */
static int in_struct(void)
{
    struct { void *p; int n; } s = { &&L, 7 };
    if (s.n != 7) return 1;
    goto *s.p;
L:  return 0;
}

int main(void)
{
    int prog[] = { 0, 1, 0, 2 };   /* acc = ((1+2)*3)+2 = 11 */
    if (run(prog, 4) != 11) return 1;

    int prog2[] = { 1, 1, 2 };     /* acc = ((1*3)*3) = 9 */
    if (run(prog2, 3) != 9) return 2;

    if (pick(0) != 10 || pick(1) != 20 || pick(2) != 30) return 3;
    if (forward()) return 4;
    if (across_scope()) return 5;
    if (in_struct()) return 6;
    return 0;
}
"#;
    assert_eq!(compile_and_run("computed_goto", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("computed_goto_opt", code), 0);
}

/// A value live across an indirect branch must survive it.
///
/// The companion to `codegen_inline_asm_x86_64_asm_goto_pseudo_survives_edge`,
/// and the same hazard: liveness is computed from the block's recorded
/// successors, so if the edges to the address-taken labels were missing, a
/// pseudo live only along one of them would look dead and its register could
/// be reused before the branch.
#[test]
fn c89_computed_goto_value_survives_the_edge() {
    let code = r#"
static int dispatch(int which, int a, int b)
{
    void *t[] = { &&X, &&Y };
    /* `a` and `b` are computed here and used only after the branch, so they
       are live across it along one edge each. */
    int sum = a + b;
    int prod = a * b;
    goto *t[which];
X:  return sum;
Y:  return prod;
}

int main(void)
{
    if (dispatch(0, 3, 4) != 7) return 1;
    if (dispatch(1, 3, 4) != 12) return 2;
    /* Enough live values to force spilling around the branch. */
    if (dispatch(0, 100, 200) != 300) return 3;
    if (dispatch(1, 100, 200) != 20000) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("computed_goto_liveness", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("computed_goto_liveness_opt", code),
        0
    );
}

/// An unsigned switch range whose bound exceeds `i64::MAX`.
///
/// Case endpoints are carried as `i64`, so `ULONG_MAX` read as -1 and the
/// range looked empty: it warned "empty range specified" and never matched.
/// The ordering has to follow the switch type's own signedness.
#[test]
fn c89_case_range_unsigned_bounds() {
    let code = r#"
static int whole(unsigned long x)
{
    switch (x) { case 0ul ... 18446744073709551615ul: return 1; default: return 0; }
}

static int upper(unsigned long x)
{
    switch (x) {
    case 9223372036854775808ul ... 18446744073709551615ul: return 1;
    default: return 0;
    }
}

int main(void)
{
    if (!whole(0) || !whole(1) || !whole(18446744073709551615ul)) return 1;
    if (!upper(9223372036854775808ul) || !upper(18446744073709551615ul)) return 2;
    if (upper(0) || upper(9223372036854775807ul)) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("case_range_unsigned", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("case_range_unsigned_opt", code),
        0
    );
}

/// A label whose address is taken but never branched on must still be emitted.
///
/// The blocks were linked to the dispatch block, and a function with no
/// computed goto has none — so DCE deleted the block and the link failed on an
/// undefined `.L` symbol. Storing a label address for later use is legal.
#[test]
fn c89_label_address_without_a_computed_goto() {
    let code = r#"
void *slot;

static void take(int x)
{
    slot = &&L;
    if (x) return;
    return;
L:  slot = 0;
}

int main(void)
{
    take(1);
    return slot != 0 ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("label_addr_no_goto", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("label_addr_no_goto_opt", code), 0);
}
