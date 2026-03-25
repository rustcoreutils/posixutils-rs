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
// Consolidates: if/while/for/do-while/switch/goto/break/continue tests from pcc/
//

use crate::common::compile_and_run;

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
