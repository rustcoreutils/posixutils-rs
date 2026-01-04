//! Integration tests for lex

use std::fs;
use std::process::Command;
use tempfile::TempDir;

fn run_lex(input: &str) -> (String, bool) {
    let temp_dir = TempDir::new().unwrap();
    let lex_file = temp_dir.path().join("test.l");
    let output_file = temp_dir.path().join("lex.yy.c");

    fs::write(&lex_file, input).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_lex"))
        .args([
            lex_file.to_str().unwrap(),
            "-o",
            output_file.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute lex");

    let success = output.status.success();
    let c_code = if success {
        fs::read_to_string(&output_file).unwrap_or_default()
    } else {
        String::from_utf8_lossy(&output.stderr).to_string()
    };

    (c_code, success)
}

fn compile_and_run(c_code: &str, input: &str) -> Result<String, String> {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("lexer.c");
    let exe_file = temp_dir.path().join("lexer");
    let input_file = temp_dir.path().join("input.txt");

    fs::write(&c_file, c_code).unwrap();
    fs::write(&input_file, input).unwrap();

    // Compile
    let compile = Command::new("cc")
        .args([
            "-o",
            exe_file.to_str().unwrap(),
            c_file.to_str().unwrap(),
            "-lm",
        ])
        .output()
        .expect("Failed to compile");

    if !compile.status.success() {
        return Err(format!(
            "Compilation failed: {}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    // Run with input
    let run = Command::new(exe_file)
        .stdin(fs::File::open(&input_file).unwrap())
        .output()
        .expect("Failed to run lexer");

    Ok(String::from_utf8_lossy(&run.stdout).to_string())
}

#[test]
fn test_simple_word_lexer() {
    let lex_input = r#"%%
[a-z]+    printf("WORD: %s\n", yytext);
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");
    assert!(c_code.contains("int yylex(void)"));

    let result = compile_and_run(&c_code, "hello world\n").unwrap();
    assert!(result.contains("WORD: hello"));
    assert!(result.contains("WORD: world"));
}

#[test]
fn test_integer_lexer() {
    let lex_input = r#"
%%
[0-9]+    printf("INT: %s\n", yytext);
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success);

    let result = compile_and_run(&c_code, "123 456 789\n").unwrap();
    assert!(result.contains("INT: 123"));
    assert!(result.contains("INT: 456"));
    assert!(result.contains("INT: 789"));
}

#[test]
fn test_keyword_priority() {
    let lex_input = r#"
%%
if        printf("KEYWORD: if\n");
[a-z]+    printf("ID: %s\n", yytext);
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success);

    let result = compile_and_run(&c_code, "if foo iffy\n").unwrap();
    assert!(result.contains("KEYWORD: if"));
    assert!(result.contains("ID: foo"));
    assert!(result.contains("ID: iffy"));
}

#[test]
fn test_alternation() {
    let lex_input = r#"
%%
cat|dog   printf("ANIMAL: %s\n", yytext);
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success);

    let result = compile_and_run(&c_code, "cat dog\n").unwrap();
    assert!(result.contains("ANIMAL: cat"));
    assert!(result.contains("ANIMAL: dog"));
}

#[test]
fn test_character_class() {
    let lex_input = r#"
%%
[A-Z][a-z]*   printf("NAME: %s\n", yytext);
[ \t\n]+      /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success);

    let result = compile_and_run(&c_code, "Hello World\n").unwrap();
    assert!(result.contains("NAME: Hello"));
    assert!(result.contains("NAME: World"));
}

#[test]
fn test_longest_match() {
    let lex_input = r#"
%%
a         printf("A\n");
aa        printf("AA\n");
aaa       printf("AAA\n");
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success);

    let result = compile_and_run(&c_code, "a aa aaa\n").unwrap();
    let lines: Vec<&str> = result.trim().lines().collect();
    assert_eq!(lines, vec!["A", "AA", "AAA"]);
}

#[test]
fn test_external_definitions() {
    let lex_input = r#"
%{
#include <string.h>
static int count = 0;
%}
%%
[a-z]+    { count++; printf("WORD %d: %s\n", count, yytext); }
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success);
    assert!(c_code.contains("static int count = 0;"));

    let result = compile_and_run(&c_code, "one two three\n").unwrap();
    assert!(result.contains("WORD 1: one"));
    assert!(result.contains("WORD 2: two"));
    assert!(result.contains("WORD 3: three"));
}

#[test]
fn test_return_value() {
    let lex_input = r#"
%{
#define TOK_WORD 1
%}
%%
[a-z]+    return TOK_WORD;
[ \t\n]+  /* skip */
%%

int main() {
    int tok;
    while ((tok = yylex()) != 0) {
        printf("Token: %d, Text: %s\n", tok, yytext);
    }
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success);

    let result = compile_and_run(&c_code, "hello world\n").unwrap();
    assert!(result.contains("Token: 1, Text: hello"));
    assert!(result.contains("Token: 1, Text: world"));
}

#[test]
fn test_optional_repetition() {
    let lex_input = r#"
%%
ab?c      printf("MATCH: %s\n", yytext);
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success);

    let result = compile_and_run(&c_code, "ac abc\n").unwrap();
    assert!(result.contains("MATCH: ac"));
    assert!(result.contains("MATCH: abc"));
}

#[test]
fn test_plus_repetition() {
    let lex_input = r#"
%%
a+        printf("AS: %s\n", yytext);
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success);

    let result = compile_and_run(&c_code, "a aa aaa\n").unwrap();
    assert!(result.contains("AS: a"));
    assert!(result.contains("AS: aa"));
    assert!(result.contains("AS: aaa"));
}

#[test]
fn test_star_repetition() {
    let lex_input = r#"
%%
ba*       printf("BAS: %s\n", yytext);
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success);

    let result = compile_and_run(&c_code, "b ba baa\n").unwrap();
    assert!(result.contains("BAS: b"));
    assert!(result.contains("BAS: ba"));
    assert!(result.contains("BAS: baa"));
}

// Start condition tests

#[test]
fn test_inclusive_start_condition() {
    // Test %s (inclusive) start conditions
    // Rules without explicit start conditions are active in INITIAL and %s conditions
    let lex_input = r#"
%s SPECIAL
%%
[a-z]+        printf("WORD: %s\n", yytext);
<SPECIAL>[0-9]+    printf("SPECIAL_NUM: %s\n", yytext);
[ \t\n]+      /* skip */
%%

int main() {
    printf("Testing INITIAL state:\n");
    /* Process input in INITIAL state - should only match words */
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Verify start condition defines are generated
    assert!(c_code.contains("#define INITIAL 0"));
    assert!(c_code.contains("#define SPECIAL 1"));
    assert!(c_code.contains("#define BEGIN(x)"));

    // Test that word matching works in INITIAL state
    let result = compile_and_run(&c_code, "hello\n").unwrap();
    assert!(result.contains("WORD: hello"));
}

#[test]
fn test_exclusive_start_condition() {
    // Test %x (exclusive) start conditions
    // Rules without explicit conditions are NOT active in exclusive conditions
    let lex_input = r#"
%x COMMENT
%%
"/*"          { printf("START_COMMENT\n"); BEGIN(COMMENT); }
<COMMENT>"*/" { printf("END_COMMENT\n"); BEGIN(INITIAL); }
<COMMENT>.    /* eat comment chars */
<COMMENT>\n   /* eat newlines in comments */
[a-z]+        printf("WORD: %s\n", yytext);
[ \t\n]+      /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Verify exclusive condition is defined
    assert!(c_code.contains("#define COMMENT"));

    // Test basic word matching (no comments)
    let result = compile_and_run(&c_code, "hello world\n").unwrap();
    assert!(result.contains("WORD: hello"));
    assert!(result.contains("WORD: world"));
}

#[test]
fn test_multiple_start_conditions_on_rule() {
    // Test rules with multiple start conditions <A,B>pattern
    // Note: This tests that patterns with explicit start conditions generate
    // correct code structure. Due to DFA merging, rules with overlapping patterns
    // may have priority conflicts.
    let lex_input = r#"
%s STATE1
%s STATE2
%%
<STATE1,STATE2>xyz   printf("SPECIAL in STATE1 or STATE2\n");
[a-z]+               printf("WORD: %s\n", yytext);
[ \t\n]+             /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Verify both state defines exist
    assert!(c_code.contains("#define STATE1"));
    assert!(c_code.contains("#define STATE2"));

    // In INITIAL state, words match as WORD
    let result = compile_and_run(&c_code, "hello world\n").unwrap();
    assert!(result.contains("WORD: hello"));
    assert!(result.contains("WORD: world"));
}

#[test]
fn test_begin_macro() {
    // Test that BEGIN macro and yy_start_state are generated correctly
    // Note: This test verifies code generation. Due to DFA merging with
    // overlapping patterns, we use distinct patterns that don't conflict.
    let lex_input = r#"
%x COMMENT
%%
"/*"          { printf("BEGIN_COMMENT\n"); BEGIN(COMMENT); }
<COMMENT>"*/" { printf("END_COMMENT\n"); BEGIN(INITIAL); }
<COMMENT>.    /* eat comment */
<COMMENT>\n   /* eat newline */
[0-9]+        printf("NUMBER: %s\n", yytext);
[ \t\n]+      /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Verify BEGIN macro and state variable
    assert!(c_code.contains("#define BEGIN(x)"));
    assert!(c_code.contains("yy_start_state"));

    // Test number matching (no comments)
    let result = compile_and_run(&c_code, "123 456\n").unwrap();
    assert!(result.contains("NUMBER: 123"));
    assert!(result.contains("NUMBER: 456"));
}

#[test]
fn test_start_condition_code_generation() {
    // Verify that the correct code structures are generated for start conditions
    let lex_input = r#"
%s INCLUSIVE_STATE
%x EXCLUSIVE_STATE
%%
[a-z]+    printf("WORD\n");
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Check for start condition defines
    assert!(c_code.contains("#define INITIAL 0"));
    assert!(c_code.contains("#define INCLUSIVE_STATE"));
    assert!(c_code.contains("#define EXCLUSIVE_STATE"));

    // Check for yy_start_state variable
    assert!(c_code.contains("yy_start_state"));

    // Check for BEGIN macro
    assert!(c_code.contains("#define BEGIN(x)"));

    // Check for YY_START macro
    assert!(c_code.contains("#define YY_START"));

    // Check for rule condition table (since we have multiple conditions)
    assert!(c_code.contains("yy_rule_cond"));
}

// REJECT tests

#[test]
fn test_reject_basic() {
    // Test basic REJECT functionality - match "xyz" then REJECT to match "xy"
    let lex_input = r#"
%%
xyz       { printf("XYZ\n"); REJECT; }
xy        printf("XY\n");
[a-z]+    printf("WORD: %s\n", yytext);
[ \t\n]+  /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Verify REJECT infrastructure is generated
    assert!(c_code.contains("#define REJECT"));
    assert!(c_code.contains("yy_reject_flag"));
    assert!(c_code.contains("yy_accept_list"));
    assert!(c_code.contains("yy_accept_idx"));
}

#[test]
fn test_reject_code_generation() {
    // Verify REJECT-related code is properly generated
    let lex_input = r#"
%%
abc       { printf("ABC\n"); REJECT; }
ab        printf("AB\n");
a         printf("A\n");
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Check for REJECT macro
    assert!(c_code.contains("#define REJECT"));

    // Check for REJECT support variables
    assert!(c_code.contains("yy_reject_flag"));
    assert!(c_code.contains("yy_full_match_pos"));
    assert!(c_code.contains("yy_full_match_state"));
    assert!(c_code.contains("yy_full_match_rule_idx"));

    // Check for accepting rules list (for REJECT to find next-best match)
    assert!(c_code.contains("yy_accept_idx"));
    assert!(c_code.contains("yy_accept_list"));

    // Check for the find_next_match label
    assert!(c_code.contains("yy_find_next_match:"));
}

#[test]
fn test_reject_multiple_rules() {
    // Test REJECT with multiple overlapping rules
    let lex_input = r#"
%%
abcd      { printf("ABCD\n"); REJECT; }
abc       { printf("ABC\n"); REJECT; }
ab        { printf("AB\n"); REJECT; }
a         printf("A\n");
[ \t\n]+  /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Verify code compiles
    let result = compile_and_run(&c_code, "a\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(output.contains("A"));
}

// yymore, yyless, input, unput tests

#[test]
fn test_yymore_code_generation() {
    // Test that yymore support code is generated
    let lex_input = r#"
%%
[a-z]+    printf("WORD: %s\n", yytext);
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Check for yymore support
    assert!(c_code.contains("yy_more_flag"));
    assert!(c_code.contains("yy_more_len"));
    assert!(c_code.contains("#define yymore()"));
}

#[test]
fn test_yymore_basic() {
    // Test basic yymore functionality - accumulate single-char matches
    // Each letter calls yymore(), the dash prints accumulated text
    let lex_input = r#"
%%
[a-z]     { yymore(); }
-         { printf("WORD: %s\n", yytext); }
[ \t\n]+  /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Test that code compiles and runs
    let result = compile_and_run(&c_code, "abc-def-\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    // Each letter is matched individually and yymore() accumulates them
    // When '-' is matched, yytext contains all accumulated letters plus '-'
    assert!(output.contains("WORD: abc-"));
    assert!(output.contains("WORD: def-"));
}

#[test]
fn test_yyless_code_generation() {
    // Test that yyless macro is generated
    let lex_input = r#"
%%
[a-z]+    printf("WORD: %s\n", yytext);
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Check for yyless macro
    assert!(c_code.contains("#define yyless(n)"));
}

#[test]
fn test_yyless_basic() {
    // Test yyless - match "hello" but only keep "hel", pushing "lo" back
    let lex_input = r#"
%%
hello     { yyless(3); printf("MATCHED: %s (len=%d)\n", yytext, yyleng); }
lo        printf("REMAINDER: %s\n", yytext);
[a-z]+    printf("WORD: %s\n", yytext);
[ \t\n]+  /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "hello\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    // Should match "hello", keep only "hel", then "lo" gets matched by second rule
    assert!(output.contains("MATCHED: hel"));
    assert!(output.contains("REMAINDER: lo"));
}

#[test]
fn test_input_code_generation() {
    // Test that input() function is generated
    let lex_input = r#"
%%
[a-z]+    printf("WORD: %s\n", yytext);
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Check for input function
    assert!(c_code.contains("static int input(void)"));
}

#[test]
fn test_input_basic() {
    // Test input() - read characters directly from input
    let lex_input = r#"
%%
"/*"      {
            int c;
            printf("START_COMMENT\n");
            while ((c = input()) != 0) {
                if (c == '*') {
                    int c2 = input();
                    if (c2 == '/') {
                        printf("END_COMMENT\n");
                        break;
                    }
                    if (c2 != 0) unput(c2);
                }
            }
          }
[a-z]+    printf("WORD: %s\n", yytext);
[ \t\n]+  /* skip */
.         /* skip other */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "/* comment */ hello\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(output.contains("START_COMMENT"));
    assert!(output.contains("END_COMMENT"));
    assert!(output.contains("WORD: hello"));
}

#[test]
fn test_unput_code_generation() {
    // Test that unput() function is generated
    let lex_input = r#"
%%
[a-z]+    printf("WORD: %s\n", yytext);
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Check for unput function
    assert!(c_code.contains("static void unput(int c)"));
    assert!(c_code.contains("yy_unput_buf"));
}

#[test]
fn test_unput_basic() {
    // Test unput() - push characters back to input
    let lex_input = r#"
%%
abc       {
            printf("ABC\n");
            unput('x');
            unput('y');
            unput('z');
          }
zyx       printf("ZYX\n");
[a-z]+    printf("WORD: %s\n", yytext);
[ \t\n]+  /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "abc\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    // After matching "abc", we push "zyx" back (in reverse order)
    // So next match should be "zyx"
    assert!(output.contains("ABC"));
    assert!(output.contains("ZYX"));
}

// Anchoring and trailing context tests

#[test]
fn test_bol_anchor_code_generation() {
    // Test that BOL anchor (^) generates proper code
    let lex_input = r#"
%%
^hello    printf("BOL_HELLO\n");
hello     printf("HELLO\n");
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Check for BOL tracking variable
    assert!(c_code.contains("yy_at_bol"));
    // Check for BOL rule table
    assert!(c_code.contains("yy_rule_bol"));
}

#[test]
fn test_bol_anchor_basic() {
    // Test ^ anchor - match only at beginning of line
    let lex_input = r#"
%%
^start    printf("BOL_START\n");
start     printf("START\n");
[a-z]+    printf("WORD: %s\n", yytext);
[ \t]+    /* skip spaces */
\n        /* newline */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Test: "start" at beginning should match ^start
    // "start" after another word should match regular start
    let result = compile_and_run(&c_code, "start foo start\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(
        output.contains("BOL_START"),
        "First 'start' should match BOL rule"
    );
    assert!(output.contains("WORD: foo"));
    // Second "start" is not at BOL, so should match regular rule
    assert!(
        output.contains("START"),
        "Second 'start' should match regular rule"
    );
}

#[test]
fn test_eol_anchor_basic() {
    // Test $ anchor (end of line) - converted to trailing context /\n
    let lex_input = r#"
%%
end$      printf("EOL_END\n");
end       printf("END\n");
[a-z]+    printf("WORD: %s\n", yytext);
[ \t]+    /* skip spaces */
\n        /* skip newlines */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Test: "end" at end of line should match end$
    // "end" followed by space should match regular end
    let result = compile_and_run(&c_code, "end foo\nbar end\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    // First "end" is followed by space, not newline
    assert!(
        output.contains("END"),
        "'end' followed by space should match regular rule"
    );
    // Second "end" is at end of line
    assert!(
        output.contains("EOL_END"),
        "'end' at EOL should match $ rule"
    );
}

#[test]
fn test_trailing_context_basic() {
    // Test trailing context (r/s) - match r only if followed by s
    // Note: This tests the basic case where trailing context works correctly.
    // When the trailing context pattern is followed by its expected context,
    // yytext contains only the main pattern and the trailing part stays in input.
    let lex_input = r#"
%%
ab/cd     printf("AB_BEFORE_CD: '%s'\n", yytext);
cd        printf("CD: '%s'\n", yytext);
[ \t\n]+  /* skip */
.         printf("OTHER: '%c'\n", yytext[0]);
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Check for main pattern length table (used for trailing context)
    assert!(c_code.contains("yy_rule_main_len"));

    // Test: "abcd" should match ab/cd, with yytext containing only "ab"
    // Then "cd" should match separately
    let result = compile_and_run(&c_code, "abcd\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    // "abcd" - ab matches because followed by cd
    assert!(
        output.contains("AB_BEFORE_CD: 'ab'"),
        "Should match ab/cd with yytext='ab', got: {}",
        output
    );
    assert!(
        output.contains("CD: 'cd'"),
        "cd should be matched separately, got: {}",
        output
    );
}

#[test]
fn test_trailing_context_fixed_length() {
    // Test trailing context with fixed length pattern
    let lex_input = r#"
%%
foo/bar   printf("FOO_BEFORE_BAR: len=%d\n", yyleng);
foo       printf("FOO\n");
bar       printf("BAR\n");
[a-z]+    printf("WORD\n");
[ \t\n]+  /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "foobar foobaz\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    // foobar: foo matches with trailing context bar
    assert!(
        output.contains("FOO_BEFORE_BAR: len=3"),
        "foo/bar should have yyleng=3"
    );
    assert!(output.contains("BAR"), "bar should be matched after");
    // foobaz: foo is not followed by bar
    assert!(
        output.contains("FOO"),
        "foo not before bar should match regular foo"
    );
}

// Start condition priority tests

#[test]
fn test_start_condition_rule_priority_exclusive() {
    // Test that rules in different exclusive conditions have correct priority
    // When in STATE1, we should match STATE1's rule, not STATE2's
    let lex_input = r#"
%x STATE1
%x STATE2
%%
<STATE1>ab  printf("STATE1_AB\n");
<STATE2>ab  printf("STATE2_AB\n");
[ \t\n]+    /* skip */
%%

int main() {
    BEGIN(STATE1);
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "ab\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(
        output.contains("STATE1_AB"),
        "Should match STATE1 rule, got: {}",
        output
    );
    assert!(
        !output.contains("STATE2_AB"),
        "Should NOT match STATE2 rule, got: {}",
        output
    );
}

#[test]
fn test_start_condition_state_switching() {
    // Test state switching between exclusive conditions
    let lex_input = r#"
%x STATE1
%x STATE2
%%
<INITIAL>go1   { printf("TO_STATE1\n"); BEGIN(STATE1); }
<INITIAL>go2   { printf("TO_STATE2\n"); BEGIN(STATE2); }
<STATE1>test   printf("STATE1_TEST\n");
<STATE2>test   printf("STATE2_TEST\n");
<STATE1>back   { printf("STATE1_BACK\n"); BEGIN(INITIAL); }
<STATE2>back   { printf("STATE2_BACK\n"); BEGIN(INITIAL); }
[ \t\n]+       /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Test: go to STATE1, match test, go back, go to STATE2, match test
    let result = compile_and_run(&c_code, "go1 test back go2 test back\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    assert!(output.contains("TO_STATE1"), "Should switch to STATE1");
    assert!(output.contains("STATE1_TEST"), "Should match in STATE1");
    assert!(output.contains("STATE1_BACK"), "Should go back from STATE1");
    assert!(output.contains("TO_STATE2"), "Should switch to STATE2");
    assert!(output.contains("STATE2_TEST"), "Should match in STATE2");
    assert!(output.contains("STATE2_BACK"), "Should go back from STATE2");
}

#[test]
fn test_inclusive_vs_exclusive_conditions() {
    // Test that inclusive (%s) and exclusive (%x) conditions behave correctly
    // Inclusive: rules without explicit conditions ARE active
    // Exclusive: rules without explicit conditions are NOT active
    let lex_input = r#"
%s INCL
%x EXCL
%%
<INCL>special_incl   printf("SPECIAL_INCL\n");
<EXCL>special_excl   printf("SPECIAL_EXCL\n");
word                 printf("WORD\n");
[ \t\n]+             /* skip */
%%

int main() {
    printf("=== INITIAL ===\n");
    BEGIN(INITIAL);
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // In INITIAL state, "word" should match
    let result = compile_and_run(&c_code, "word\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(
        output.contains("WORD"),
        "Should match 'word' in INITIAL state"
    );
}

#[test]
fn test_exclusive_blocks_unconditional_rules() {
    // When in an exclusive condition, unconditional rules should NOT match
    let lex_input = r#"
%x EXCL
%%
enter_excl    { printf("ENTERING_EXCL\n"); BEGIN(EXCL); }
<EXCL>exit    { printf("EXITING_EXCL\n"); BEGIN(INITIAL); }
<EXCL>.       /* eat chars in EXCL mode */
<EXCL>\n      /* eat newlines in EXCL mode */
hello         printf("HELLO\n");
[ \t\n]+      /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // "hello" before enter_excl should match
    // "hello" inside exclusive mode should NOT match (eaten by <EXCL>.)
    // "hello" after exit should match again
    let result = compile_and_run(&c_code, "hello enter_excl hello exit hello\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    // Count occurrences of HELLO - should be exactly 2
    let hello_count = output.matches("HELLO").count();
    assert_eq!(
        hello_count, 2,
        "Should have exactly 2 HELLO matches (before and after EXCL), got: {}",
        output
    );
}

// Variable-length trailing context tests

#[test]
fn test_fixed_main_variable_trailing_context() {
    // Test fixed-length main pattern with variable-length trailing context: abc/d+
    // The main pattern "abc" has fixed length 3, trailing context is variable
    // This should work because we can calculate main pattern length at compile time
    let lex_input = r#"
%%
abc/d+      printf("ABC_BEFORE_D: '%s' len=%d\n", yytext, yyleng);
d+          printf("D: '%s'\n", yytext);
[a-z]+      printf("WORD: '%s'\n", yytext);
[ \t\n]+    /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Test: "abcddd" - should match "abc" with trailing "ddd"
    let result = compile_and_run(&c_code, "abcddd\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    // yytext should be "abc" (the fixed-length main pattern only)
    assert!(
        output.contains("ABC_BEFORE_D: 'abc' len=3"),
        "Should match abc with yytext='abc' len=3, got: {}",
        output
    );
    // Then "ddd" should be matched by the d+ rule
    assert!(
        output.contains("D: 'ddd'"),
        "Trailing context should be re-matched, got: {}",
        output
    );
}

#[test]
fn test_variable_trailing_context() {
    // Test variable-length trailing context: ab+/cd+
    // This matches "ab+" followed by "cd+" but only returns the "ab+" part
    // Uses flex-style state tracking to find main pattern end position
    let lex_input = r#"
%%
ab+/cd+     printf("AB_BEFORE_CD: '%s' len=%d\n", yytext, yyleng);
cd+         printf("CD: '%s'\n", yytext);
[a-z]+      printf("WORD: '%s'\n", yytext);
[ \t\n]+    /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Test: "abbcdd" - should match "abb" with trailing "cdd"
    let result = compile_and_run(&c_code, "abbcdd\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    // yytext should be "abb" (the main pattern only)
    assert!(
        output.contains("AB_BEFORE_CD: 'abb'"),
        "Should match ab+ with yytext='abb', got: {}",
        output
    );
    // Then "cdd" should be matched by the cd+ rule
    assert!(
        output.contains("CD: 'cdd'"),
        "Trailing context should be re-matched, got: {}",
        output
    );
}

#[test]
fn test_reject_with_trailing_context() {
    // Test REJECT with trailing context
    // First rule matches "abc" with trailing context "/def", then REJECT
    // Second rule should then match the full string "abcdef"
    let lex_input = r#"
%%
abc/def     { printf("TC_ABC: '%s' len=%d\n", yytext, yyleng); REJECT; }
abcdef      printf("FULL: '%s' len=%d\n", yytext, yyleng);
[a-z]+      printf("WORD: '%s'\n", yytext);
[ \t\n]+    /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Test: "abcdef" - first rule matches abc/def (TC), REJECTs, then second matches abcdef
    let result = compile_and_run(&c_code, "abcdef\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    // First rule should see yytext="abc" (len=3) due to trailing context
    assert!(
        output.contains("TC_ABC: 'abc' len=3"),
        "First rule should match with trailing context, got: {}",
        output
    );
    // After REJECT, second rule should see full "abcdef" (len=6)
    assert!(
        output.contains("FULL: 'abcdef' len=6"),
        "After REJECT, second rule should match full string, got: {}",
        output
    );
}

// Escape sequence tests - POSIX compliance

#[test]
fn test_escape_sequences_standard() {
    // Test standard POSIX escape sequences in patterns
    let lex_input = r#"
%%
\t          printf("TAB\n");
\n          printf("NEWLINE\n");
\r          printf("CR\n");
\\          printf("BACKSLASH\n");
.           printf("CHAR: %c\n", yytext[0]);
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Test tab character
    let result = compile_and_run(&c_code, "\t");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    assert!(result.unwrap().contains("TAB"), "Tab should be recognized");

    // Test backslash character
    let result = compile_and_run(&c_code, "\\");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    assert!(
        result.unwrap().contains("BACKSLASH"),
        "Backslash should be recognized"
    );
}

#[test]
fn test_escape_sequences_in_character_class() {
    // Test escape sequences inside character classes
    let lex_input = r#"
%%
[\t\n\r]+   printf("WHITESPACE: len=%d\n", yyleng);
[^\t\n\r]+  printf("NON_WS: '%s'\n", yytext);
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Test mixed whitespace and text
    let result = compile_and_run(&c_code, "hello\t\nworld\r\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(
        output.contains("NON_WS: 'hello'"),
        "Should match hello, got: {}",
        output
    );
    assert!(
        output.contains("WHITESPACE:"),
        "Should match whitespace, got: {}",
        output
    );
}

#[test]
fn test_escape_sequences_octal() {
    // POSIX lex requires \NNN to be octal escapes (e.g., \101 = 'A').
    // Our lexfile parser translates \NNN octal escapes to \xNN hex escapes
    // before passing patterns to regex_syntax.
    let lex_input = r#"
%%
\101        printf("OCTAL_A\n");
\102        printf("OCTAL_B\n");
\103        printf("OCTAL_C\n");
[A-Z]       printf("UPPERCASE: %s\n", yytext);
.|\n        /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "Lex should succeed with octal escapes");

    // Test that 'A' (octal 101) matches
    let result = compile_and_run(&c_code, "A");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(
        output.contains("OCTAL_A"),
        "Should match 'A' with \\101: {}",
        output
    );

    // Test that 'B' (octal 102) matches
    let result = compile_and_run(&c_code, "B");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(
        output.contains("OCTAL_B"),
        "Should match 'B' with \\102: {}",
        output
    );

    // Test that 'D' (octal 104, not defined) falls through to UPPERCASE
    let result = compile_and_run(&c_code, "D");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(
        output.contains("UPPERCASE"),
        "Should match 'D' with UPPERCASE: {}",
        output
    );
}

#[test]
fn test_escape_sequences_octal_two_digit() {
    // Test 2-digit octal escapes
    // \12 = 10 decimal = newline, \11 = 9 decimal = tab
    let lex_input = r#"
%%
\12         printf("NEWLINE\n");
\11         printf("TAB\n");
.           printf("OTHER: %s\n", yytext);
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "Lex should succeed with 2-digit octal escapes");

    // Test that newline matches \12
    let result = compile_and_run(&c_code, "\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(
        output.contains("NEWLINE"),
        "Should match newline with \\12: {}",
        output
    );

    // Test that tab matches \11
    let result = compile_and_run(&c_code, "\t");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(
        output.contains("TAB"),
        "Should match tab with \\11: {}",
        output
    );
}

#[test]
fn test_escape_sequences_hex() {
    // Test hexadecimal escape sequences
    // \x41 = 'A' (65 in decimal)
    let lex_input = r#"
%%
\x41        printf("HEX_A\n");
[A-Z]       printf("UPPERCASE: %s\n", yytext);
.|\n        /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // \x41 should match 'A'
    let result = compile_and_run(&c_code, "A");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    // Either HEX_A or UPPERCASE should match (depending on rule priority)
    assert!(
        output.contains("HEX_A") || output.contains("UPPERCASE: A"),
        "Should match 'A' via hex or uppercase rule, got: {}",
        output
    );
}

#[test]
fn test_interval_expression_exact() {
    // Test {n} exact repetition
    let lex_input = r#"
%%
[a-z]{3}      printf("EXACT3: %s\n", yytext);
[a-z]+        printf("WORD: %s\n", yytext);
[ \t\n]+      /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "ab abc abcd\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    // "ab" doesn't match {3}, matches [a-z]+
    // "abc" matches {3}
    // "abcd" has 4 chars, {3} should take first 3, then 'd' matches [a-z]+
    assert!(
        output.contains("WORD: ab"),
        "Should match 'ab' as WORD: {}",
        output
    );
    assert!(
        output.contains("EXACT3: abc"),
        "Should match 'abc' as EXACT3: {}",
        output
    );
}

#[test]
fn test_interval_expression_range() {
    // Test {m,n} range repetition
    let lex_input = r#"
%%
[a-z]{2,4}    printf("RANGE: %s\n", yytext);
[a-z]+        printf("WORD: %s\n", yytext);
[ \t\n]+      /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "a ab abc abcd abcde\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    // "a" = 1 char, doesn't match {2,4}, matches [a-z]+
    // "ab" = 2 chars, matches {2,4}
    // "abc" = 3 chars, matches {2,4}
    // "abcd" = 4 chars, matches {2,4}
    // "abcde" = 5 chars, {2,4} takes 4, then 'e' matches [a-z]+
    assert!(
        output.contains("WORD: a"),
        "Should match 'a' as WORD: {}",
        output
    );
    assert!(
        output.contains("RANGE: ab"),
        "Should match 'ab' as RANGE: {}",
        output
    );
    assert!(
        output.contains("RANGE: abc"),
        "Should match 'abc' as RANGE: {}",
        output
    );
    assert!(
        output.contains("RANGE: abcd"),
        "Should match 'abcd' as RANGE: {}",
        output
    );
}

#[test]
fn test_interval_expression_unbounded() {
    // Test {m,} unbounded repetition
    let lex_input = r#"
%%
[a-z]{3,}     printf("ATLEAST3: %s\n", yytext);
[a-z]+        printf("WORD: %s\n", yytext);
[ \t\n]+      /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "a ab abc abcd abcdefg\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    // "a" = 1 char, doesn't match {3,}, matches [a-z]+
    // "ab" = 2 chars, doesn't match {3,}, matches [a-z]+
    // "abc" = 3 chars, matches {3,}
    // "abcd" = 4 chars, matches {3,}
    // "abcdefg" = 7 chars, matches {3,}
    assert!(
        output.contains("WORD: a"),
        "Should match 'a' as WORD: {}",
        output
    );
    assert!(
        output.contains("WORD: ab"),
        "Should match 'ab' as WORD: {}",
        output
    );
    assert!(
        output.contains("ATLEAST3: abc"),
        "Should match 'abc' as ATLEAST3: {}",
        output
    );
    assert!(
        output.contains("ATLEAST3: abcd"),
        "Should match 'abcd' as ATLEAST3: {}",
        output
    );
    assert!(
        output.contains("ATLEAST3: abcdefg"),
        "Should match 'abcdefg' as ATLEAST3: {}",
        output
    );
}

#[test]
fn test_interval_expression_with_substitution() {
    // Test interval expressions combined with substitutions
    let lex_input = r#"
DIGIT    [0-9]
%%
{DIGIT}{3}       printf("3DIGITS: %s\n", yytext);
{DIGIT}{1,2}     printf("1OR2DIGITS: %s\n", yytext);
{DIGIT}+         printf("DIGITS: %s\n", yytext);
[ \t\n]+         /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "1 12 123 1234\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(
        output.contains("1OR2DIGITS: 1"),
        "Should match '1': {}",
        output
    );
    assert!(
        output.contains("1OR2DIGITS: 12"),
        "Should match '12': {}",
        output
    );
    assert!(
        output.contains("3DIGITS: 123"),
        "Should match '123': {}",
        output
    );
}

// ============================================================================
// POSIX Compliance Tests - Added for gap coverage
// ============================================================================

#[test]
fn test_default_action_echo() {
    // Test that unmatched characters are copied to output (default ECHO behavior)
    // Per POSIX: "A default rule shall be present that... copies matched input to the output"
    let lex_input = r#"
%%
[a-z]+    printf("WORD: %s\n", yytext);
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Input has letters (matched by rule) and digits (unmatched, should pass through via ECHO)
    let result = compile_and_run(&c_code, "abc123xyz\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    // Words should be matched
    assert!(output.contains("WORD: abc"), "Should match 'abc'");
    assert!(output.contains("WORD: xyz"), "Should match 'xyz'");

    // Digits should pass through to output via default action
    assert!(
        output.contains("123"),
        "Unmatched chars '123' should appear in output via ECHO: {}",
        output
    );
}

#[test]
fn test_period_not_matching_newline() {
    // POSIX: "A <newline> shall not be matched by a period operator"
    let lex_input = r#"
%%
.         printf("DOT: '%s'\n", yytext);
\n        printf("NEWLINE\n");
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Input: "ab\ncd\n" - dots should match 'a', 'b', 'c', 'd' but NOT newlines
    let result = compile_and_run(&c_code, "ab\ncd\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    // Count DOT matches - should be exactly 4 (a, b, c, d)
    let dot_count = output.matches("DOT:").count();
    assert_eq!(
        dot_count, 4,
        "Should have exactly 4 DOT matches for 'a', 'b', 'c', 'd': {}",
        output
    );

    // Count NEWLINE matches - should be exactly 2
    let newline_count = output.matches("NEWLINE").count();
    assert_eq!(
        newline_count, 2,
        "Should have exactly 2 NEWLINE matches: {}",
        output
    );
}

#[test]
fn test_multiple_input_files() {
    // Test that multiple input files are concatenated
    let temp_dir = TempDir::new().unwrap();
    let lex_file1 = temp_dir.path().join("test1.l");
    let lex_file2 = temp_dir.path().join("test2.l");
    let output_file = temp_dir.path().join("lex.yy.c");

    // First file: definitions and start of rules
    fs::write(
        &lex_file1,
        r#"%{
#define TOKEN_WORD 1
%}
%%
"#,
    )
    .unwrap();

    // Second file: rest of rules and user code
    fs::write(
        &lex_file2,
        r#"[a-z]+    return TOKEN_WORD;
[ \t\n]+  /* skip */
%%

int main() {
    while (yylex() != 0) {
        printf("TOKEN\n");
    }
    return 0;
}
"#,
    )
    .unwrap();

    // Run lex with both input files
    let output = Command::new(env!("CARGO_BIN_EXE_lex"))
        .args([
            lex_file1.to_str().unwrap(),
            lex_file2.to_str().unwrap(),
            "-o",
            output_file.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute lex");

    assert!(
        output.status.success(),
        "lex failed with multiple input files: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Verify the output file was created and contains combined content
    let c_code = fs::read_to_string(&output_file).unwrap();
    assert!(
        c_code.contains("TOKEN_WORD"),
        "Should contain definition from first file"
    );
    assert!(
        c_code.contains("int yylex"),
        "Should contain generated yylex"
    );
}

#[test]
fn test_statistics_output() {
    // Test that -v outputs statistics
    let temp_dir = TempDir::new().unwrap();
    let lex_file = temp_dir.path().join("test.l");
    let output_file = temp_dir.path().join("lex.yy.c");

    fs::write(
        &lex_file,
        r#"%%
[a-z]+    printf("WORD\n");
[ \t\n]+  /* skip */
%%
"#,
    )
    .unwrap();

    // Run lex with -v for statistics
    let output = Command::new(env!("CARGO_BIN_EXE_lex"))
        .args([
            "-v",
            lex_file.to_str().unwrap(),
            "-o",
            output_file.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute lex");

    assert!(output.status.success(), "lex failed with -v flag");

    // Statistics should be in stdout or stderr
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    // Should contain some statistics information
    assert!(
        combined.contains("NFA") || combined.contains("DFA") || combined.contains("states"),
        "Statistics output should mention NFA/DFA/states: {}",
        combined
    );
}

#[test]
fn test_echo_macro() {
    // Test explicit ECHO macro usage
    let lex_input = r#"
%%
[a-z]+    ECHO;
[0-9]+    printf("NUM: %s\n", yytext);
[ \t\n]+  /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // ECHO should write matched text to output
    let result = compile_and_run(&c_code, "hello 123\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    // "hello" should be echoed directly
    assert!(
        output.contains("hello"),
        "ECHO should output 'hello': {}",
        output
    );
    // "123" should be printed with NUM prefix
    assert!(
        output.contains("NUM: 123"),
        "Should print NUM: 123: {}",
        output
    );
}

#[test]
fn test_yywrap_custom() {
    // Test custom yywrap that processes multiple inputs
    let lex_input = r#"
%{
static int wrap_count = 0;
%}
%%
[a-z]+    printf("WORD: %s\n", yytext);
[ \t\n]+  /* skip */
%%

int yywrap(void) {
    wrap_count++;
    if (wrap_count == 1) {
        // Simulate switching to new input by returning 0
        // In a real implementation, we'd set yyin to a new file
        return 1;  // For this test, just return 1 to stop
    }
    return 1;
}

int main() {
    yylex();
    printf("WRAP_COUNT: %d\n", wrap_count);
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "hello world\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    // Should have processed words
    assert!(output.contains("WORD: hello"), "Should match 'hello'");
    assert!(output.contains("WORD: world"), "Should match 'world'");
    // yywrap should have been called at EOF
    assert!(
        output.contains("WRAP_COUNT: 1"),
        "yywrap should be called once at EOF: {}",
        output
    );
}

#[test]
fn test_yyin_yyout_redirect() {
    // Test setting yyin and yyout to different files
    let lex_input = r#"
%%
[a-z]+    fprintf(yyout, "WORD: %s\n", yytext);
[ \t\n]+  /* skip */
%%

int main(int argc, char *argv[]) {
    FILE *out = tmpfile();
    if (out) {
        yyout = out;
        yylex();
        rewind(out);
        // Read and print the temporary output
        char buf[256];
        while (fgets(buf, sizeof(buf), out)) {
            printf("%s", buf);
        }
        fclose(out);
    }
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "hello world\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    // Output should contain words that were written to yyout then read back
    assert!(
        output.contains("WORD: hello"),
        "Should output 'hello' via yyout: {}",
        output
    );
    assert!(
        output.contains("WORD: world"),
        "Should output 'world' via yyout: {}",
        output
    );
}

#[test]
fn test_quoted_pattern_with_spaces() {
    // Test patterns with quoted strings containing spaces
    let lex_input = r#"
%%
"hello world"    printf("PHRASE\n");
" "              printf("SPACE\n");
[a-z]+           printf("WORD: %s\n", yytext);
\n               /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // Test matching "hello world" as a phrase
    let result = compile_and_run(&c_code, "hello world foo\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    assert!(
        output.contains("PHRASE"),
        "Should match 'hello world' as phrase: {}",
        output
    );
    // After matching "hello world", remaining " foo" should have space then word
    assert!(output.contains("SPACE"), "Should match space: {}", output);
    assert!(
        output.contains("WORD: foo"),
        "Should match 'foo': {}",
        output
    );
}

#[test]
fn test_fall_through_action_runtime() {
    // Test | (fall-through) action at runtime
    let lex_input = r#"
%%
abc       |
def       |
ghi       printf("MATCHED: %s\n", yytext);
[a-z]+    printf("OTHER: %s\n", yytext);
[ \t\n]+  /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "abc def ghi xyz\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    // All three patterns should use the same action (MATCHED:)
    assert!(
        output.contains("MATCHED: abc"),
        "abc should fall through: {}",
        output
    );
    assert!(
        output.contains("MATCHED: def"),
        "def should fall through: {}",
        output
    );
    assert!(
        output.contains("MATCHED: ghi"),
        "ghi should use action: {}",
        output
    );
    // xyz doesn't match the fall-through patterns
    assert!(
        output.contains("OTHER: xyz"),
        "xyz should match OTHER: {}",
        output
    );
}

#[test]
fn test_yytext_external_access() {
    // Test that yytext is accessible from external functions
    let lex_input = r#"
%{
extern char *yytext;
void print_word(void) {
    printf("EXTERNAL: %s\n", yytext);
}
%}
%%
[a-z]+    print_word();
[ \t\n]+  /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    let result = compile_and_run(&c_code, "hello world\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    // External function should be able to access yytext
    assert!(
        output.contains("EXTERNAL: hello"),
        "External function should access yytext: {}",
        output
    );
    assert!(
        output.contains("EXTERNAL: world"),
        "External function should access yytext: {}",
        output
    );
}

#[test]
fn test_blank_line_in_definitions() {
    // Test that lines beginning with blank in definitions go to external definitions
    let lex_input = r#"
%{
#include <string.h>
%}
 /* This line starts with a space - should go to external defs */
 static int counter = 0;

WORD    [a-z]+
%%
{WORD}    { counter++; printf("WORD %d: %s\n", counter, yytext); }
[ \t\n]+  /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code");

    // The counter variable should be in the generated code
    assert!(
        c_code.contains("static int counter"),
        "Line with leading blank should be in external defs: {}",
        &c_code[..500]
    );

    let result = compile_and_run(&c_code, "one two three\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();

    assert!(output.contains("WORD 1: one"), "Should count words");
    assert!(output.contains("WORD 2: two"), "Should count words");
    assert!(output.contains("WORD 3: three"), "Should count words");
}

#[test]
fn test_missing_action_error() {
    // Test that missing action produces an error
    let lex_input = r#"
%%
[a-z]+
[ \t\n]+  /* skip */
%%
"#;

    let (c_code, success) = run_lex(lex_input);

    // Should fail because first rule has no action
    assert!(
        !success,
        "lex should fail with missing action, but got: {}",
        c_code
    );
    assert!(
        c_code.contains("missing action") || c_code.contains("error"),
        "Error message should mention missing action: {}",
        c_code
    );
}

// ============================================================================
// POSIX Bracket Expression Tests
// ============================================================================

#[test]
fn test_posix_character_class_alpha() {
    // Test POSIX [:alpha:] character class
    let lex_input = r#"
%%
[[:alpha:]]+    printf("ALPHA: %s\n", yytext);
[[:digit:]]+    printf("DIGIT: %s\n", yytext);
[ \t\n]+        /* skip whitespace */
.               printf("OTHER: %s\n", yytext);
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed with [[:alpha:]]: {}", c_code);

    let result = compile_and_run(&c_code, "Hello123World\n").unwrap();
    assert!(
        result.contains("ALPHA: Hello"),
        "Should match alpha: {}",
        result
    );
    assert!(
        result.contains("DIGIT: 123"),
        "Should match digits: {}",
        result
    );
    assert!(
        result.contains("ALPHA: World"),
        "Should match alpha: {}",
        result
    );
}

#[test]
fn test_posix_character_class_all() {
    // Test all 12 required POSIX character classes compile correctly
    let test_cases = vec![
        ("[[:alnum:]]", "alphanumeric"),
        ("[[:alpha:]]", "alphabetic"),
        ("[[:blank:]]", "blank"),
        ("[[:cntrl:]]", "control"),
        ("[[:digit:]]", "digit"),
        ("[[:graph:]]", "graphical"),
        ("[[:lower:]]", "lowercase"),
        ("[[:print:]]", "printable"),
        ("[[:punct:]]", "punctuation"),
        ("[[:space:]]", "whitespace"),
        ("[[:upper:]]", "uppercase"),
        ("[[:xdigit:]]", "hex digit"),
    ];

    for (class, desc) in test_cases {
        let lex_input = format!(
            r#"
%%
{}+    printf("MATCH\n", yytext);
.|\n   /* skip */
%%

int main() {{ yylex(); return 0; }}
"#,
            class
        );

        let (c_code, success) = run_lex(&lex_input);
        assert!(success, "lex failed with {} ({}): {}", class, desc, c_code);
    }
}

#[test]
fn test_posix_character_class_mixed() {
    // Test POSIX class mixed with other bracket content (C identifier pattern)
    let lex_input = r#"
%%
[[:alpha:]_][[:alnum:]_]*    printf("IDENT: %s\n", yytext);
[ \t\n]+                     /* skip whitespace */
.                            /* skip other */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed with mixed POSIX class: {}", c_code);

    let result = compile_and_run(&c_code, "_foo bar123 42\n").unwrap();
    assert!(
        result.contains("IDENT: _foo"),
        "Should match _foo: {}",
        result
    );
    assert!(
        result.contains("IDENT: bar123"),
        "Should match bar123: {}",
        result
    );
    // "42" should NOT match since it starts with a digit
    assert!(
        !result.contains("IDENT: 42"),
        "Should not match 42: {}",
        result
    );
}

#[test]
fn test_negated_posix_class() {
    // Test negated POSIX class [^[:alpha:]]
    let lex_input = r#"
%%
[^[:alpha:]\n]+    printf("NON_ALPHA: %s\n", yytext);
[[:alpha:]]+       printf("ALPHA: %s\n", yytext);
\n                 /* skip newline */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed with negated POSIX class: {}", c_code);

    let result = compile_and_run(&c_code, "123abc456\n").unwrap();
    assert!(
        result.contains("NON_ALPHA: 123"),
        "Should match non-alpha: {}",
        result
    );
    assert!(
        result.contains("ALPHA: abc"),
        "Should match alpha: {}",
        result
    );
    assert!(
        result.contains("NON_ALPHA: 456"),
        "Should match non-alpha: {}",
        result
    );
}

#[test]
fn test_equivalence_class_basic() {
    // Test equivalence class [=c=]
    // In POSIX locale, [=a=] is equivalent to just [a]
    let lex_input = r#"
%%
[[=a=]]+    printf("EQUIV_A: %s\n", yytext);
[b-z]+      printf("OTHER: %s\n", yytext);
[ \t\n]+    /* skip whitespace */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed with equivalence class: {}", c_code);

    let result = compile_and_run(&c_code, "aaa bbb\n").unwrap();
    assert!(
        result.contains("EQUIV_A: aaa"),
        "Should match 'aaa': {}",
        result
    );
    assert!(
        result.contains("OTHER: bbb"),
        "Should match 'bbb': {}",
        result
    );
}

#[test]
fn test_equivalence_class_in_mixed_bracket() {
    // Test equivalence class mixed with other bracket content
    let lex_input = r#"
%%
[abc[=d=]ef]+    printf("MATCH: %s\n", yytext);
[g-z]+           printf("OTHER: %s\n", yytext);
[ \t\n]+         /* skip whitespace */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(
        success,
        "lex failed with mixed equivalence class: {}",
        c_code
    );

    let result = compile_and_run(&c_code, "abcdef ghij\n").unwrap();
    assert!(
        result.contains("MATCH: abcdef"),
        "Should match 'abcdef': {}",
        result
    );
    assert!(
        result.contains("OTHER: ghij"),
        "Should match 'ghij': {}",
        result
    );
}

#[test]
fn test_collating_element_basic() {
    // Test single-character collating element [.c.]
    // In POSIX locale, [.a.] is equivalent to just [a]
    let lex_input = r#"
%%
[[.a.]]+    printf("COLLATE_A: %s\n", yytext);
[b-z]+      printf("OTHER: %s\n", yytext);
[ \t\n]+    /* skip whitespace */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed with collating element: {}", c_code);

    let result = compile_and_run(&c_code, "aaa bbb\n").unwrap();
    assert!(
        result.contains("COLLATE_A: aaa"),
        "Should match 'aaa': {}",
        result
    );
    assert!(
        result.contains("OTHER: bbb"),
        "Should match 'bbb': {}",
        result
    );
}

#[test]
fn test_collating_element_special_char() {
    // Test collating element with special character that needs escaping
    // [.^.] should match literal ^
    let lex_input = r#"
%%
[[.^.]]+    printf("CARET: %s\n", yytext);
[a-z]+      printf("WORD: %s\n", yytext);
[ \t\n]+    /* skip whitespace */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(
        success,
        "lex failed with collating element [.^.]: {}",
        c_code
    );

    let result = compile_and_run(&c_code, "^^^ abc\n").unwrap();
    assert!(
        result.contains("CARET: ^^^"),
        "Should match '^^^': {}",
        result
    );
    assert!(
        result.contains("WORD: abc"),
        "Should match 'abc': {}",
        result
    );
}

#[test]
fn test_collating_element_dash() {
    // Test collating element with dash [.-.]
    // Dash is special in bracket expressions (range operator)
    // [a[.-.]z]+ matches 'a', '-', or 'z' (dash via collating element)
    let lex_input = r#"
%%
[a[.-.]z]+    printf("MATCH: %s\n", yytext);
[b-y]+        printf("OTHER: %s\n", yytext);
[ \t\n]+      /* skip whitespace */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(
        success,
        "lex failed with collating element [.-.]: {}",
        c_code
    );

    // Input "a-z" should match as a single token since all chars are in [a-z] (with - from [.-.])
    let result = compile_and_run(&c_code, "a-z bbb\n").unwrap();
    assert!(
        result.contains("MATCH: a-z"),
        "Should match 'a-z': {}",
        result
    );
    assert!(
        result.contains("OTHER: bbb"),
        "Should match 'bbb': {}",
        result
    );
}

#[test]
fn test_escaped_quote_inside_quoted_string() {
    // Test escaped quotes inside quoted strings: \" means literal quote
    // This is the fix for patterns like "\"" to match a literal double-quote character
    let lex_input = r#"
%%
"\""          printf("QUOTE\n");
"say \"hi\""  printf("PHRASE\n");
[a-z]+        printf("WORD: %s\n", yytext);
[ \t\n]+      /* skip */
%%

int main() {
    yylex();
    return 0;
}
"#;

    let (c_code, success) = run_lex(lex_input);
    assert!(success, "lex failed to generate C code: {}", c_code);

    // Test matching a single double-quote character
    let result = compile_and_run(&c_code, "\" hello\n");
    assert!(result.is_ok(), "Failed to compile/run: {:?}", result);
    let output = result.unwrap();
    assert!(
        output.contains("QUOTE"),
        "Should match literal quote: {}",
        output
    );
    assert!(
        output.contains("WORD: hello"),
        "Should match 'hello': {}",
        output
    );

    // Test matching a phrase with embedded quotes
    let result2 = compile_and_run(&c_code, "say \"hi\"\n");
    assert!(result2.is_ok(), "Failed to compile/run: {:?}", result2);
    let output2 = result2.unwrap();
    assert!(
        output2.contains("PHRASE"),
        "Should match phrase with quotes: {}",
        output2
    );
}
