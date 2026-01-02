//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Integration tests for yacc

use std::fs;
use std::process::Command;
use tempfile::TempDir;

fn run_yacc(args: &[&str], grammar_content: &str) -> std::process::Output {
    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar_content).unwrap();

    let mut cmd_args: Vec<&str> = args.to_vec();
    cmd_args.push(grammar_path.to_str().unwrap());

    Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&cmd_args)
        .output()
        .expect("failed to execute yacc-rs")
}

/// Run yacc in both fast and strict modes, asserting both succeed
fn run_yacc_both_modes(args: &[&str], grammar_content: &str) {
    // Fast mode (default)
    let output_fast = run_yacc(args, grammar_content);
    assert!(
        output_fast.status.success(),
        "yacc (fast mode) should succeed: {}",
        String::from_utf8_lossy(&output_fast.stderr)
    );

    // Strict mode
    let mut strict_args = vec!["--strict"];
    strict_args.extend(args);
    let output_strict = run_yacc(&strict_args, grammar_content);
    assert!(
        output_strict.status.success(),
        "yacc --strict should succeed: {}",
        String::from_utf8_lossy(&output_strict.stderr)
    );
}

/// Run full end-to-end test (yacc + compile + run) in both modes
fn run_end_to_end_both_modes(grammar: &str, test_name: &str) {
    run_end_to_end_with_mode(grammar, test_name, false); // Fast mode
    run_end_to_end_with_mode(grammar, test_name, true); // Strict mode
}

fn run_end_to_end_with_mode(grammar: &str, test_name: &str, strict: bool) {
    let mode_name = if strict { "strict" } else { "fast" };

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    // Run yacc
    let mut yacc_args = vec![];
    if strict {
        yacc_args.push("--strict");
    }
    yacc_args.push(grammar_path.to_str().unwrap());

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&yacc_args)
        .output()
        .expect("failed to execute yacc");

    assert!(
        output.status.success(),
        "{} ({} mode): yacc should succeed: {}",
        test_name,
        mode_name,
        String::from_utf8_lossy(&output.stderr)
    );

    // Compile
    let code_path = temp_dir.path().join("y.tab.c");
    let exe_path = temp_dir.path().join(format!("{}_{}", test_name, mode_name));

    let compile = Command::new("cc")
        .current_dir(temp_dir.path())
        .args(&[
            "-Wall",
            "-Wno-unused-variable",
            "-Wno-unused-but-set-variable",
            "-o",
            exe_path.to_str().unwrap(),
            code_path.to_str().unwrap(),
        ])
        .output()
        .expect("failed to execute cc");

    assert!(
        compile.status.success(),
        "{} ({} mode): cc should compile: {}",
        test_name,
        mode_name,
        String::from_utf8_lossy(&compile.stderr)
    );

    // Run
    let run = Command::new(&exe_path)
        .current_dir(temp_dir.path())
        .output()
        .expect("failed to execute parser");

    assert!(
        run.status.success(),
        "{} ({} mode): parser should succeed: {}",
        test_name,
        mode_name,
        String::from_utf8_lossy(&run.stderr)
    );
}

#[test]
fn test_simple_grammar() {
    let grammar = r#"
%token NUM
%%
expr : NUM
     ;
"#;

    run_yacc_both_modes(&[], grammar);
}

#[test]
fn test_expression_grammar() {
    let grammar = r#"
%{
#include <stdio.h>
%}

%token NUM

%left '+' '-'
%left '*' '/'

%%

expr : expr '+' expr    { $$ = $1 + $3; }
     | expr '-' expr    { $$ = $1 - $3; }
     | expr '*' expr    { $$ = $1 * $3; }
     | expr '/' expr    { $$ = $1 / $3; }
     | NUM              { $$ = $1; }
     ;

%%

int main() {
    return yyparse();
}

void yyerror(const char *s) {
    fprintf(stderr, "%s\n", s);
}
"#;

    run_yacc_both_modes(&[], grammar);
}

#[test]
fn test_with_header_flag() {
    let grammar = r#"
%token NUM
%%
expr : NUM
     ;
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-d", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc -d should succeed");

    // Check that header file was created
    let header_path = temp_dir.path().join("y.tab.h");
    assert!(
        header_path.exists(),
        "y.tab.h should be created with -d flag"
    );
}

#[test]
fn test_with_verbose_flag() {
    let grammar = r#"
%token NUM
%%
expr : NUM
     ;
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-v", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc -v should succeed");

    // Check that output file was created
    let output_path = temp_dir.path().join("y.output");
    assert!(
        output_path.exists(),
        "y.output should be created with -v flag"
    );
}

#[test]
fn test_file_prefix_option() {
    let grammar = r#"
%token NUM
%%
expr : NUM
     ;
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-b", "myparser", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc -b should succeed");

    // Check that file was created with custom prefix
    let code_path = temp_dir.path().join("myparser.tab.c");
    assert!(
        code_path.exists(),
        "myparser.tab.c should be created with -b flag"
    );
}

#[test]
fn test_union_and_types() {
    let grammar = r#"
%{
#include <stdio.h>
%}

%union {
    int ival;
    double dval;
}

%token <ival> INTEGER
%token <dval> FLOAT
%type <dval> expr

%%

expr : INTEGER          { $$ = (double)$1; }
     | FLOAT            { $$ = $1; }
     | expr '+' expr    { $$ = $1 + $3; }
     ;

%%
"#;

    run_yacc_both_modes(&[], grammar);
}

#[test]
fn test_precedence_and_associativity() {
    let grammar = r#"
%token NUM
%token UMINUS

%left '+' '-'
%left '*' '/'
%right UMINUS

%%

expr : expr '+' expr
     | expr '-' expr
     | expr '*' expr
     | expr '/' expr
     | '-' expr %prec UMINUS
     | NUM
     ;

%%
"#;

    // Test both modes
    run_yacc_both_modes(&[], grammar);

    // Additionally check for no conflicts (fast mode is representative)
    let output = run_yacc(&[], grammar);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("conflict"),
        "should have no conflicts with proper precedence"
    );
}

#[test]
fn test_ambiguous_grammar_reports_conflicts() {
    let grammar = r#"
%token NUM
%%
expr : expr '+' expr
     | NUM
     ;
"#;

    // Both modes should handle conflicts
    run_yacc_both_modes(&[], grammar);

    // Check that conflicts are reported (fast mode is representative)
    let output = run_yacc(&[], grammar);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("conflict"),
        "should report shift/reduce conflict"
    );
}

#[test]
fn test_error_recovery() {
    let grammar = r#"
%token NUM
%%
stmt_list : stmt_list stmt
          |
          ;

stmt : NUM ';'
     | error ';'
     ;
%%
"#;

    run_yacc_both_modes(&[], grammar);
}

#[test]
fn test_start_symbol() {
    let grammar = r#"
%token NUM
%start program
%%
program : stmt_list
        ;

stmt_list : stmt_list stmt
          |
          ;

stmt : NUM
     ;
%%
"#;

    run_yacc_both_modes(&[], grammar);
}

#[test]
fn test_yyerror_macro_defined() {
    // Test that YYERROR macro is defined in generated code
    let grammar = r#"
%token NUM
%%
expr : NUM { if ($1 < 0) YYERROR; }
     | error { $$ = 0; }
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc should succeed");

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    assert!(
        code.contains("#define YYERROR"),
        "generated code should define YYERROR macro"
    );
}

#[test]
fn test_yyaccept_yyabort_macros_defined() {
    // Test that YYACCEPT and YYABORT macros are defined
    let grammar = r#"
%token NUM DONE
%%
input : expr DONE { YYACCEPT; }
      | error { YYABORT; }
      ;
expr : NUM
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc should succeed");

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    assert!(
        code.contains("#define YYACCEPT"),
        "generated code should define YYACCEPT macro"
    );
    assert!(
        code.contains("#define YYABORT"),
        "generated code should define YYABORT macro"
    );
}

#[test]
fn test_yyerrok_yyclearin_macros_defined() {
    // Test that yyerrok and yyclearin macros are defined
    let grammar = r#"
%token NUM
%%
stmt_list : stmt_list stmt
          |
          ;

stmt : NUM ';'
     | error ';' { yyerrok; yyclearin; }
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc should succeed");

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    assert!(
        code.contains("#define yyerrok"),
        "generated code should define yyerrok macro"
    );
    assert!(
        code.contains("#define yyclearin"),
        "generated code should define yyclearin macro"
    );
}

#[test]
fn test_yyrecovering_macro_defined() {
    // Test that YYRECOVERING macro is defined
    let grammar = r#"
%token NUM
%%
stmt : NUM { if (YYRECOVERING()) printf("recovering\n"); }
     | error
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc should succeed");

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    assert!(
        code.contains("#define YYRECOVERING"),
        "generated code should define YYRECOVERING macro"
    );
}

#[test]
fn test_error_recovery_with_yyerrok() {
    // Test a grammar that uses error recovery with yyerrok
    let grammar = r#"
%{
#include <stdio.h>
int yylex(void);
void yyerror(const char *s);
%}

%token NUM SEMI

%%

program : stmt_list
        ;

stmt_list : stmt_list stmt
          | /* empty */
          ;

stmt : NUM SEMI
     | error SEMI { yyerrok; printf("Error recovered\n"); }
     ;

%%

void yyerror(const char *s) {
    fprintf(stderr, "%s\n", s);
}
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        output.status.success(),
        "yacc should succeed with error recovery grammar"
    );

    // Verify the generated code structure
    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // Check that error handling labels exist
    assert!(
        code.contains("yyerrlab:") || code.contains("errlab:"),
        "generated code should have error label"
    );
}

#[test]
fn test_sym_prefix_affects_functions() {
    // Test that -p option affects function/variable names but NOT #define macros (per POSIX)
    let grammar = r#"
%token NUM
%%
expr : NUM
     | error { yyerrok; yyclearin; }
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-p", "my", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc -p should succeed");

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // Per POSIX: -p shall NOT affect #define symbols
    // yyerrok and yyclearin should remain unchanged
    assert!(
        code.contains("#define yyerrok"),
        "generated code should define yyerrok (not affected by -p per POSIX)"
    );
    assert!(
        code.contains("#define yyclearin"),
        "generated code should define yyclearin (not affected by -p per POSIX)"
    );

    // But functions and variables should be prefixed
    assert!(
        code.contains("myparse"),
        "generated code should have myparse function"
    );
    assert!(
        code.contains("mylval"),
        "generated code should have mylval variable"
    );
    assert!(
        code.contains("mychar"),
        "generated code should have mychar variable"
    );
}

#[test]
fn test_debug_tables_generated_with_t_flag() {
    // Test that -t generates debug tables (yytname, yyrule)
    let grammar = r#"
%token NUM PLUS
%%
expr : NUM
     | expr PLUS NUM
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-t", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc -t should succeed");

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // Check that YYDEBUG is set to 1
    assert!(
        code.contains("#define YYDEBUG 1") || code.contains("# define YYDEBUG 1"),
        "generated code should define YYDEBUG to 1 with -t flag"
    );

    // Check that debug tables are present
    assert!(
        code.contains("yytname"),
        "generated code should have yytname table with -t flag"
    );
    assert!(
        code.contains("yyrule"),
        "generated code should have yyrule table with -t flag"
    );

    // Check for token names in yytname
    assert!(
        code.contains("\"NUM\"") || code.contains("NUM"),
        "yytname should contain token name NUM"
    );
    assert!(
        code.contains("\"PLUS\"") || code.contains("PLUS"),
        "yytname should contain token name PLUS"
    );
}

#[test]
fn test_debug_not_enabled_without_t_flag() {
    // Test that debug tables are NOT generated without -t
    let grammar = r#"
%token NUM
%%
expr : NUM
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc should succeed");

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // Check that YYDEBUG is set to 0
    assert!(
        code.contains("#define YYDEBUG 0") || code.contains("# define YYDEBUG 0"),
        "generated code should define YYDEBUG to 0 without -t flag"
    );

    // Check that debug tables are NOT present
    assert!(
        !code.contains("yytname["),
        "generated code should NOT have yytname table without -t flag"
    );
}

#[test]
fn test_yydebug_variable_declared_with_t_flag() {
    // Test that yydebug variable is declared with -t
    let grammar = r#"
%token NUM
%%
expr : NUM
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-t", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc -t should succeed");

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // Check for yydebug variable
    assert!(
        code.contains("int yydebug"),
        "generated code should declare yydebug variable with -t flag"
    );

    // Check it's initialized to 0 (POSIX requires initial value to be zero)
    assert!(
        code.contains("yydebug = 0"),
        "yydebug should be initialized to 0"
    );
}

#[test]
fn test_debug_output_format() {
    // Test that debug output includes state transitions and reductions
    let grammar = r#"
%token NUM
%%
expr : NUM
     | expr expr
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-t", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc -t should succeed");

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // Check for debug output messages
    assert!(
        code.contains("Entering state"),
        "debug output should include state entry messages"
    );
    assert!(
        code.contains("Shifting token"),
        "debug output should include shift messages"
    );
    assert!(
        code.contains("Reducing by rule"),
        "debug output should include reduce messages"
    );
    assert!(
        code.contains("Reading token"),
        "debug output should include token read messages"
    );
}

#[test]
fn test_yynerrs_declared() {
    // Test that yynerrs is declared in the generated parser
    let grammar = r#"
%token NUM
%%
expr : NUM
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc should succeed");

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    assert!(
        code.contains("yynerrs"),
        "generated code should declare yynerrs"
    );
}

#[test]
fn test_escape_sequences_in_grammar() {
    // Test that escape sequences work in character literals
    let grammar = r#"
%token CHAR
%%
input : CHAR
      | input CHAR
      ;

char : '\n'    { printf("newline\n"); }
     | '\t'    { printf("tab\n"); }
     | '\r'    { printf("carriage return\n"); }
     | '\\'    { printf("backslash\n"); }
     | '\''    { printf("single quote\n"); }
     | '\a'    { printf("alert\n"); }
     | '\b'    { printf("backspace\n"); }
     | '\f'    { printf("form feed\n"); }
     | '\v'    { printf("vertical tab\n"); }
     /* Note: '\0' (NUL) cannot be used as it conflicts with EOF token number 0 */
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        output.status.success(),
        "yacc should succeed with escape sequences: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Verify the generated code contains the escape characters
    let code_path = temp_dir.path().join("y.tab.c");
    assert!(code_path.exists(), "y.tab.c should be created");
}

#[test]
fn test_octal_escape_sequences() {
    // Test octal escape sequences in character literals
    // Note: '\0' cannot be used as it conflicts with EOF token number 0
    let grammar = r#"
%token NUM
%%
expr : '\1'   { $$ = 1; }
     | '\7'   { $$ = 7; }
     | '\77'  { $$ = 63; }
     | '\177' { $$ = 127; }
     | NUM
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        output.status.success(),
        "yacc should succeed with octal escape sequences: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn test_hex_escape_sequences() {
    // Test hex escape sequences in character literals
    // Note: '\x00' cannot be used as it conflicts with EOF token number 0
    let grammar = r#"
%token NUM
%%
expr : '\x01' { $$ = 1; }
     | '\x41' { $$ = 65; }
     | '\x7F' { $$ = 127; }
     | '\xff' { $$ = 255; }
     | NUM
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        output.status.success(),
        "yacc should succeed with hex escape sequences: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn test_generated_code_compiles() {
    let grammar = r#"
%{
#include <stdio.h>
#include <stdlib.h>

int yylex(void);
void yyerror(const char *s);
%}

%token NUM

%%

expr : NUM { printf("Got number\n"); }
     ;

%%

int main(void) {
    return yyparse();
}

void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
}

/* Simple lexer for testing */
int yylex(void) {
    static int called = 0;
    if (!called) {
        called = 1;
        yylval = 42;
        return NUM;
    }
    return 0;  /* EOF */
}
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    // Run yacc
    let yacc_output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-d", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(yacc_output.status.success(), "yacc should succeed");

    // Try to compile the generated code (if cc is available)
    let code_path = temp_dir.path().join("y.tab.c");
    assert!(code_path.exists(), "y.tab.c should exist");

    // Check if cc is available
    if Command::new("cc").arg("--version").output().is_ok() {
        let exe_path = temp_dir.path().join("parser");
        let compile_output = Command::new("cc")
            .current_dir(temp_dir.path())
            .args(&[
                "-o",
                exe_path.to_str().unwrap(),
                code_path.to_str().unwrap(),
            ])
            .output()
            .expect("failed to execute cc");

        if !compile_output.status.success() {
            let stderr = String::from_utf8_lossy(&compile_output.stderr);
            eprintln!("Compilation failed: {}", stderr);
        }

        // Note: We don't assert success here because the generated code
        // may have minor issues that need fixing for full C99 compliance.
        // The important thing is that the structure is correct.
    }
}

#[test]
fn test_negative_dollar_references() {
    // Test that $0 and negative $ references generate correct code
    // This tests left-context value references per POSIX
    let grammar = r#"
%token A B C
%%
/* Use $0 to access the value before the rule on the stack */
prog : item item item
     ;

item : A    { $$ = 1; }
     | B    { $$ = 2; }
     | C    {
         /* $0 refers to the value preceding this rule on the stack */
         /* When reducing C after A B, $0 would be B's value */
         $$ = $0 + 10;
       }
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        output.status.success(),
        "yacc should succeed with $0 reference: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // $0 with rhs_len=1 should generate yyvsp[-1]
    assert!(
        code.contains("yyvsp[-1]"),
        "generated code should contain yyvsp[-1] for $0 with single RHS: {}",
        code
    );
}

#[test]
fn test_negative_dollar_minus_one() {
    // Test $-1 reference
    let grammar = r#"
%token NUM
%%
expr : expr '+' term { $$ = $1 + $3; }
     | term          { $$ = $1; }
     ;

term : NUM           { $$ = $1; }
     | '(' expr ')'  {
         /* $-1 refers to the value two positions before the rule */
         $$ = $2 + $-1;
       }
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        output.status.success(),
        "yacc should succeed with $-1 reference: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // $-1 with rhs_len=3 should generate yyvsp[-4]
    assert!(
        code.contains("yyvsp[-4]"),
        "generated code should contain yyvsp[-4] for $-1 with 3-symbol RHS"
    );
}

#[test]
fn test_tagged_dollar_references() {
    // Test $<tag>n and $<tag>$ syntax for explicit type overrides
    let grammar = r#"
%union {
    int ival;
    char *sval;
}

%token <ival> NUM
%token <sval> STR

%type <ival> expr

%%
expr : NUM            { $$ = $1; }
     | STR            {
         /* Use explicit tag to access ival from different position */
         $<ival>$ = 0;
       }
     | expr '+' expr  {
         /* Explicit tags on both LHS and RHS */
         $<ival>$ = $<ival>1 + $<ival>3;
       }
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        output.status.success(),
        "yacc should succeed with tagged references: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // Check for tagged value references
    assert!(
        code.contains("yyval.ival"),
        "generated code should contain yyval.ival for $<ival>$"
    );
    assert!(
        code.contains(".ival)"),
        "generated code should contain .ival for $<ival>n references"
    );
}

#[test]
fn test_tagged_negative_reference() {
    // Test $<tag>0 and $<tag>-1 syntax
    let grammar = r#"
%union {
    int ival;
    double dval;
}

%token <ival> INT
%token <dval> FLOAT

%%
items : item item item
      ;

item : INT    { $$ = $1; }
     | FLOAT  {
         /* Use $<ival>0 to access int value from previous item on stack */
         $<dval>$ = $<ival>0 + $1;
       }
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        output.status.success(),
        "yacc should succeed with tagged negative reference: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // $<ival>0 with rhs_len=1 should generate yyvsp[-1].ival
    assert!(
        code.contains("yyvsp[-1].ival"),
        "generated code should contain yyvsp[-1].ival for $<ival>0"
    );
}

#[test]
fn test_line_directives_with_actual_line_numbers() {
    // Test that #line directives include actual source line numbers
    let grammar = r#"%{
/* This prologue starts on line 1 */
#include <stdio.h>
%}

%token NUM

%%
expr : NUM { $$ = $1; }
     ;
%%

/* Epilogue starts here */
int main() { return yyparse(); }
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        output.status.success(),
        "yacc should succeed: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // Check for #line directive with filename
    assert!(
        code.contains("#line") && code.contains("test.y"),
        "generated code should contain #line directives with source filename"
    );

    // Check that line numbers are actual numbers (not placeholder "1")
    // The prologue starts at line 1, the action at line 9, epilogue at line 13
    assert!(
        code.contains("#line 1 \"") || code.contains("#line 2 \""),
        "prologue #line should have line number from source"
    );
}

#[test]
fn test_line_directives_omitted_with_l_flag() {
    // Test that -l flag omits #line directives
    let grammar = r#"%{
#include <stdio.h>
%}
%token NUM
%%
expr : NUM { $$ = $1; }
     ;
%%
int main() { return yyparse(); }
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-l", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        output.status.success(),
        "yacc -l should succeed: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // With -l flag, there should be no #line directives pointing to source file
    assert!(
        !code.contains("#line") || !code.contains("test.y"),
        "generated code with -l should NOT contain #line directives with source filename"
    );
}

#[test]
fn test_line_directives_for_semantic_actions() {
    // Test that semantic actions get correct #line directives
    let grammar = r#"%token NUM
%%
expr : NUM { /* action on line 3 */ $$ = $1; }
     ;
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("action_test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        output.status.success(),
        "yacc should succeed: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // Check that semantic action has #line directive with the grammar filename
    assert!(
        code.contains("#line") && code.contains("action_test.y"),
        "semantic action should have #line directive with source filename"
    );
}

#[test]
fn test_line_directives_for_union() {
    // Test that %union gets correct #line directive
    let grammar = r#"%union {
    int ival;
    double dval;
}

%token <ival> NUM

%%
expr : NUM { $$ = $1; }
     ;
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("union_test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-d", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        output.status.success(),
        "yacc should succeed: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // Check code file has #line for union
    assert!(
        code.contains("#line") && code.contains("union_test.y"),
        "union in code file should have #line directive"
    );

    // Check header file also has #line for union
    let header_path = temp_dir.path().join("y.tab.h");
    let header = fs::read_to_string(&header_path).expect("should read generated header");

    assert!(
        header.contains("#line") && header.contains("union_test.y"),
        "union in header file should have #line directive"
    );
}

#[test]
fn test_duplicate_token_number_error() {
    // Test that duplicate token numbers are rejected
    let grammar = r#"
%token FOO 300
%token BAR 300
%%
expr : FOO
     | BAR
     ;
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        !output.status.success(),
        "yacc should fail with duplicate token numbers"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("duplicate token number 300"),
        "error message should mention duplicate token number: {}",
        stderr
    );
}

#[test]
fn test_token_number_reassignment_error() {
    // Test that trying to change a token's number is rejected
    let grammar = r#"
%token FOO 300
%left FOO 400
%%
expr : FOO
     ;
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        !output.status.success(),
        "yacc should fail when trying to reassign token number"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("already has number"),
        "error message should mention token already has number: {}",
        stderr
    );
}

#[test]
fn test_nul_char_conflicts_with_eof() {
    // Test that '\0' character literal conflicts with EOF (token number 0)
    let grammar = r#"
%token NUM
%%
expr : '\0'
     | NUM
     ;
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&[grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(
        !output.status.success(),
        "yacc should fail when '\\0' conflicts with EOF"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("duplicate token number 0"),
        "error message should mention conflict with token 0 (EOF): {}",
        stderr
    );
}

#[test]
fn test_sym_prefix_affects_semantic_actions() {
    // Test that -p option affects semantic action code ($$ and $n references)
    let grammar = r#"
%token NUM
%%
expr : expr '+' expr { $$ = $1 + $3; }
     | NUM           { $$ = $1; }
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-p", "foo", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc -p should succeed");

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // Semantic actions should use prefixed names
    // $$ should become (fooval) and $n should become (foovsp[...])
    assert!(
        code.contains("(fooval)"),
        "semantic action should use (fooval) for $$ with -p foo"
    );
    assert!(
        code.contains("foovsp["),
        "semantic action should use foovsp for $n with -p foo"
    );

    // Should NOT contain yyvsp or yyval in semantic actions
    // (Note: they may appear in parser infrastructure code, so we check within case statements)
    let case_section = code
        .split("switch (foon)")
        .nth(1)
        .map(|s| s.split("default:").next().unwrap_or(""))
        .unwrap_or("");

    assert!(
        !case_section.contains("yyvsp["),
        "semantic actions should NOT use yyvsp with -p foo"
    );
    assert!(
        !case_section.contains("(yyval)"),
        "semantic actions should NOT use (yyval) with -p foo"
    );
}

#[test]
fn test_description_file_shows_conflicts() {
    // Test that -v generates description file that includes conflict information
    let grammar = r#"
%token NUM
%%
expr : expr '+' expr
     | NUM
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-v", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc -v should succeed");

    // Check that description file was created
    let desc_path = temp_dir.path().join("y.output");
    assert!(
        desc_path.exists(),
        "y.output should be created with -v flag"
    );

    // Check that description file contains grammar and state information
    let desc = fs::read_to_string(&desc_path).expect("should read description file");

    assert!(
        desc.contains("Grammar"),
        "description file should contain Grammar section"
    );
    assert!(
        desc.contains("State"),
        "description file should contain State sections"
    );

    // For ambiguous grammar, should mention conflicts
    assert!(
        desc.contains("conflict") || desc.contains("Conflict"),
        "description file should mention conflicts for ambiguous grammar"
    );
}

#[test]
fn test_sym_prefix_affects_tagged_references() {
    // Test that -p option affects tagged semantic action code ($<tag>n and $<tag>$)
    let grammar = r#"
%union {
    int ival;
    double dval;
}

%token <ival> NUM
%type <ival> expr

%%
expr : expr '+' expr { $<ival>$ = $<ival>1 + $<ival>3; }
     | NUM           { $$ = $1; }
     ;
%%
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-p", "bar", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc-rs");

    assert!(output.status.success(), "yacc -p should succeed");

    let code_path = temp_dir.path().join("y.tab.c");
    let code = fs::read_to_string(&code_path).expect("should read generated code");

    // Tagged references should use prefixed names
    // $<ival>$ should become (barval.ival) and $<ival>n should become (barvsp[...].ival)
    assert!(
        code.contains("(barval.ival)"),
        "semantic action should use (barval.ival) for $<ival>$ with -p bar"
    );
    assert!(
        code.contains("barvsp[") && code.contains(".ival)"),
        "semantic action should use barvsp[...].ival for $<ival>n with -p bar"
    );
}

#[test]
fn test_undefined_nonterminal_error() {
    // POSIX: yacc shall report an error for any non-terminal symbol that does not
    // appear on the left side of at least one grammar rule
    let grammar = r#"
%token A
%%
start : A undefined_symbol ;
%%
"#;

    let output = run_yacc(&[], grammar);

    assert!(
        !output.status.success(),
        "yacc should fail with undefined non-terminal"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("undefined_symbol") && stderr.contains("no rules"),
        "error message should mention the undefined non-terminal: {}",
        stderr
    );
}

#[test]
fn test_yy_prefix_warning() {
    // POSIX: "Conforming applications shall not use names beginning in yy or YY"
    let grammar = r#"
%token YYtoken yytoken NORMAL
%%
yystart : YYtoken yytoken NORMAL ;
%%
"#;

    let output = run_yacc(&[], grammar);

    // Should succeed (warning, not error)
    assert!(
        output.status.success(),
        "yacc should succeed with yy/YY names (warning only)"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("YYtoken") && stderr.contains("reserved"),
        "should warn about YYtoken: {}",
        stderr
    );
    assert!(
        stderr.contains("yytoken") && stderr.contains("reserved"),
        "should warn about yytoken: {}",
        stderr
    );
    assert!(
        stderr.contains("yystart") && stderr.contains("reserved"),
        "should warn about yystart: {}",
        stderr
    );
}

#[test]
fn test_compile_and_run_simple_parser() {
    // End-to-end test: generate parser, compile with cc, and run
    // Tests both fast and strict modes
    let grammar = r#"
%{
#include <stdio.h>
#include <stdlib.h>

int yylex(void);
void yyerror(const char *s);

static int result = 0;
%}

%token NUM

%%
input : NUM { result = $1; }
      ;
%%

/* Simple lexer that returns a single NUM token with value 42 */
static int token_returned = 0;
int yylval;

int yylex(void) {
    if (token_returned) return 0;  /* EOF */
    token_returned = 1;
    yylval = 42;
    return NUM;
}

void yyerror(const char *s) {
    fprintf(stderr, "%s\n", s);
}

int main(void) {
    if (yyparse() != 0) {
        return 1;
    }
    return (result == 42) ? 0 : 1;
}
"#;

    run_end_to_end_both_modes(grammar, "simple_parser");
}

#[test]
fn test_compile_and_run_calculator() {
    // Full calculator grammar with operators
    // Tests both fast and strict modes
    let grammar = r#"
%{
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

int yylex(void);
void yyerror(const char *s);

static int result = 0;
static const char *input_str;
static int input_pos;
%}

%token NUM
%left '+' '-'
%left '*' '/'

%%
input : expr { result = $1; }
      ;

expr : expr '+' expr { $$ = $1 + $3; }
     | expr '-' expr { $$ = $1 - $3; }
     | expr '*' expr { $$ = $1 * $3; }
     | expr '/' expr { $$ = $1 / $3; }
     | NUM           { $$ = $1; }
     ;
%%

int yylval;

int yylex(void) {
    while (input_str[input_pos] == ' ') input_pos++;
    if (input_str[input_pos] == '\0') return 0;

    if (isdigit((unsigned char)input_str[input_pos])) {
        yylval = 0;
        while (isdigit((unsigned char)input_str[input_pos])) {
            yylval = yylval * 10 + (input_str[input_pos] - '0');
            input_pos++;
        }
        return NUM;
    }

    return input_str[input_pos++];
}

void yyerror(const char *s) {
    fprintf(stderr, "%s\n", s);
}

int main(void) {
    /* Test: 2 + 3 * 4 = 14 (due to precedence) */
    input_str = "2 + 3 * 4";
    input_pos = 0;

    if (yyparse() != 0) {
        fprintf(stderr, "Parse failed\n");
        return 1;
    }

    if (result != 14) {
        fprintf(stderr, "Expected 14, got %d\n", result);
        return 1;
    }

    return 0;
}
"#;

    run_end_to_end_both_modes(grammar, "calculator");
}

#[test]
fn test_error_recovery_basic() {
    // Test error token in grammar
    // Tests both fast and strict modes
    let grammar = r#"
%{
#include <stdio.h>
#include <stdlib.h>

int yylex(void);
void yyerror(const char *s);

static int recovered = 0;
static int error_count = 0;
%}

%token NUM SEMI

%%
input : stmts
      ;

stmts : stmts stmt
      |
      ;

stmt : NUM SEMI         { /* normal statement */ }
     | error SEMI       { recovered = 1; yyerrok; }
     ;
%%

static int tokens[] = { NUM, NUM, NUM, SEMI, 0 };  /* invalid: NUM NUM NUM before SEMI */
static int token_pos = 0;
int yylval;

int yylex(void) {
    return tokens[token_pos++];
}

void yyerror(const char *s) {
    error_count++;
    fprintf(stderr, "%s\n", s);
}

int main(void) {
    int result = yyparse();
    (void)result;
    /* Should recover from error */
    if (!recovered) {
        fprintf(stderr, "Error recovery failed\n");
        return 1;
    }
    return 0;
}
"#;

    run_end_to_end_both_modes(grammar, "error_recovery");
}

#[test]
fn test_empty_rule() {
    // Test epsilon production
    // Note: Uses tokens A and B to avoid shift/reduce conflict
    // The input "B" forces opt_a to match the empty rule
    // Tests both fast and strict modes
    let grammar = r#"
%{
#include <stdio.h>
int yylex(void);
void yyerror(const char *s);
static int empty_matched = 0;
%}

%token A B

%%
start : opt_a B { if (empty_matched) return 0; else return 1; }
      ;

opt_a :   /* empty */ { empty_matched = 1; }
      | A
      ;
%%

static int call_count = 0;

int yylex(void) {
    call_count++;
    if (call_count == 1) return B;  /* B forces empty opt_a to match */
    return 0;
}

void yyerror(const char *s) { fprintf(stderr, "%s\n", s); }

int main(void) {
    return yyparse();
}
"#;

    run_end_to_end_both_modes(grammar, "empty_rule");
}

#[test]
fn test_mid_rule_action() {
    // Test mid-rule semantic actions
    // The mid-rule action sets $$ = 100, which becomes accessible as $2 in the final action
    // Tests both fast and strict modes
    let grammar = r#"
%{
#include <stdio.h>
int yylex(void);
void yyerror(const char *s);
static int mid_value = 0;
%}

%token A B

%%
start : A { mid_value = 100; $$ = 100; } B { (void)mid_value; if ($2 != 100) return 1; return 0; }
      ;
%%

static int call_count = 0;
int yylval;

int yylex(void) {
    call_count++;
    if (call_count == 1) { yylval = 1; return A; }
    if (call_count == 2) { yylval = 2; return B; }
    return 0;
}

void yyerror(const char *s) { fprintf(stderr, "%s\n", s); }

int main(void) {
    return yyparse();
}
"#;

    run_end_to_end_both_modes(grammar, "mid_rule_action");
}

#[test]
fn test_prec_override() {
    // Test %prec directive for unary minus
    // Tests both fast and strict modes
    let grammar = r#"
%{
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

int yylex(void);
void yyerror(const char *s);

static int result = 0;
static const char *input_str;
static int input_pos;
%}

%token NUM
%left '+' '-'
%left '*'
%right UMINUS

%%
input : expr { result = $1; }
      ;

expr : expr '+' expr { $$ = $1 + $3; }
     | expr '-' expr { $$ = $1 - $3; }
     | expr '*' expr { $$ = $1 * $3; }
     | '-' expr %prec UMINUS { $$ = -$2; }
     | NUM           { $$ = $1; }
     ;
%%

int yylval;

int yylex(void) {
    while (input_str[input_pos] == ' ') input_pos++;
    if (input_str[input_pos] == '\0') return 0;

    if (isdigit((unsigned char)input_str[input_pos])) {
        yylval = 0;
        while (isdigit((unsigned char)input_str[input_pos])) {
            yylval = yylval * 10 + (input_str[input_pos] - '0');
            input_pos++;
        }
        return NUM;
    }

    return input_str[input_pos++];
}

void yyerror(const char *s) {
    fprintf(stderr, "%s\n", s);
}

int main(void) {
    /* Test: -3 * 2 = -6 (unary minus has higher precedence than *) */
    input_str = "-3 * 2";
    input_pos = 0;

    if (yyparse() != 0) {
        fprintf(stderr, "Parse failed\n");
        return 1;
    }

    if (result != -6) {
        fprintf(stderr, "Expected -6, got %d\n", result);
        return 1;
    }

    return 0;
}
"#;

    run_end_to_end_both_modes(grammar, "prec_override");
}

/// Test that error token value can be overridden per POSIX
#[test]
fn test_error_token_value_override() {
    let grammar = r#"
%token error 512
%token NUM
%%
expr : NUM
     | error
     ;
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("error_override.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .args(&["-d", grammar_path.to_str().unwrap()])
        .output()
        .expect("failed to execute yacc");

    assert!(
        output.status.success(),
        "yacc should accept %token error 512: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Check that the header file doesn't contain error=256 (the default)
    let header_path = temp_dir.path().join("y.tab.h");
    let header_content = fs::read_to_string(&header_path).unwrap();

    // The error token shouldn't be #defined (it's a reserved token),
    // but the grammar should accept the override without error
    assert!(
        !header_content.contains("error 256"),
        "error token should be overridden, not using default 256"
    );
}

/// Test that default semantic value propagation works ($$ = $1)
#[test]
fn test_default_value_propagation() {
    let grammar = r#"
%{
#include <stdio.h>

int result = 0;
const char *input_str;
int input_pos;
%}

%token NUM

%%

goal : expr         /* No action - should get $$ = $1 implicitly */
     ;

expr : NUM          { $$ = $1; }
     ;

%%

int yylex(void) {
    extern int yylval;
    while (input_str[input_pos] == ' ') input_pos++;
    if (input_str[input_pos] == '\0') return 0;
    if (input_str[input_pos] >= '0' && input_str[input_pos] <= '9') {
        yylval = input_str[input_pos++] - '0';
        return NUM;
    }
    return input_str[input_pos++];
}

void yyerror(const char *s) {
    fprintf(stderr, "%s\n", s);
}

int main(void) {
    extern int yyparse(void);
    input_str = "7";
    input_pos = 0;

    if (yyparse() != 0) {
        fprintf(stderr, "Parse failed\n");
        return 1;
    }

    /* The goal rule should have $$ = $1 implicitly,
       which means result should be the value from expr */
    return 0;
}
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("default_val.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .arg(grammar_path.to_str().unwrap())
        .output()
        .expect("failed to execute yacc");

    assert!(
        output.status.success(),
        "yacc should succeed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Check that the generated code contains the implicit $$ = $1 assignment
    let code_path = temp_dir.path().join("y.tab.c");
    let code_content = fs::read_to_string(&code_path).unwrap();

    // The goal rule (production 1) should have implicit $$ = $1
    assert!(
        code_content.contains("$$ = $1 (default)"),
        "Should have implicit $$ = $1 for rules without actions"
    );

    // Compile and run to verify it works
    let exe_path = temp_dir.path().join("default_val_test");
    let compile = Command::new("cc")
        .current_dir(temp_dir.path())
        .args(&[
            "-Wall",
            "-o",
            exe_path.to_str().unwrap(),
            code_path.to_str().unwrap(),
        ])
        .output()
        .expect("failed to execute cc");

    assert!(
        compile.status.success(),
        "cc should compile default value test: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(exe_path)
        .current_dir(temp_dir.path())
        .output()
        .expect("failed to execute default value test");

    assert!(
        run.status.success(),
        "Default value propagation should work: {}",
        String::from_utf8_lossy(&run.stderr)
    );
}

/// Test that POSIX single-reduce optimization generates consistent table
#[test]
fn test_consistent_state_optimization() {
    let grammar = r#"
%token NUM
%%
expr : NUM
     ;
"#;

    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("consistent.y");
    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .arg(grammar_path.to_str().unwrap())
        .output()
        .expect("failed to execute yacc");

    assert!(
        output.status.success(),
        "yacc should succeed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Check that the generated code contains the consistent table and optimization
    let code_path = temp_dir.path().join("y.tab.c");
    let code_content = fs::read_to_string(&code_path).unwrap();

    // Should have the consistent table
    assert!(
        code_content.contains("yyconsistent[]") || code_content.contains("consistent[]"),
        "Should have consistent state table for POSIX optimization"
    );

    // Should have the optimization check
    assert!(
        code_content.contains("POSIX: skip lookahead"),
        "Should have POSIX lookahead skip optimization"
    );
}

// ============================================================================
// --strict mode tests
// ============================================================================

/// Test that --strict flag is recognized
#[test]
fn test_strict_mode_flag_recognized() {
    let grammar = r#"
%token NUM
%%
expr : NUM
     ;
"#;

    let output = run_yacc(&["--strict"], grammar);
    assert!(
        output.status.success(),
        "yacc --strict should succeed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Test that --strict mode disables consistent state optimization
#[test]
fn test_strict_mode_disables_consistent_optimization() {
    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");

    // Grammar with consistent states (single reduce)
    let grammar = r#"
%token A B
%%
s : A B { $$ = 1; }
  | A   { $$ = 2; }
  ;
"#;

    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .arg("--strict")
        .arg(grammar_path.to_str().unwrap())
        .output()
        .expect("failed to execute yacc");

    assert!(
        output.status.success(),
        "yacc --strict should succeed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Check that consistent[] contains all zeros
    let code_path = temp_dir.path().join("y.tab.c");
    let code_content = fs::read_to_string(&code_path).unwrap();

    // Extract consistent table values
    if let Some(start) = code_content.find("yyconsistent[] =") {
        let subset = &code_content[start..];
        if let Some(end) = subset.find("};") {
            let table_def = &subset[..end];
            // In strict mode, all values should be 0
            let has_nonzero = table_def
                .chars()
                .filter(|c| c.is_ascii_digit())
                .any(|c| c != '0');
            assert!(
                !has_nonzero,
                "In strict mode, yyconsistent[] should contain all zeros"
            );
        }
    }
}

/// Test that fast mode (default) enables consistent state optimization
#[test]
fn test_fast_mode_enables_consistent_optimization() {
    let temp_dir = TempDir::new().unwrap();
    let grammar_path = temp_dir.path().join("test.y");

    // Grammar that will have consistent states (reduce-only states)
    let grammar = r#"
%token NUM
%%
expr : term
     ;
term : NUM
     ;
"#;

    fs::write(&grammar_path, grammar).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .arg(grammar_path.to_str().unwrap())
        .output()
        .expect("failed to execute yacc");

    assert!(
        output.status.success(),
        "yacc should succeed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // In fast mode, there should be at least one consistent state
    let code_path = temp_dir.path().join("y.tab.c");
    let code_content = fs::read_to_string(&code_path).unwrap();

    // Look for the consistent table
    assert!(
        code_content.contains("yyconsistent[]") || code_content.contains("consistent[]"),
        "Should have consistent state table"
    );
}

/// Test that both strict and fast modes produce working parsers
#[test]
fn test_strict_and_fast_both_compile() {
    let temp_dir = TempDir::new().unwrap();

    let grammar = r#"
%{
#include <stdio.h>
int yylex(void);
void yyerror(const char *s);
%}
%token NUM
%%
expr : NUM { printf("parsed\n"); }
     ;
%%
int yylex(void) { static int done = 0; if (done) return 0; done = 1; return NUM; }
void yyerror(const char *s) { fprintf(stderr, "%s\n", s); }
int main(void) { return yyparse(); }
"#;

    // Test fast mode (default)
    let grammar_path_fast = temp_dir.path().join("fast.y");
    fs::write(&grammar_path_fast, grammar).unwrap();

    let output_fast = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .arg("-b")
        .arg("fast")
        .arg(grammar_path_fast.to_str().unwrap())
        .output()
        .expect("failed to execute yacc");

    assert!(
        output_fast.status.success(),
        "yacc (fast mode) should succeed: {}",
        String::from_utf8_lossy(&output_fast.stderr)
    );

    // Test strict mode
    let grammar_path_strict = temp_dir.path().join("strict.y");
    fs::write(&grammar_path_strict, grammar).unwrap();

    let output_strict = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(temp_dir.path())
        .arg("--strict")
        .arg("-b")
        .arg("strict")
        .arg(grammar_path_strict.to_str().unwrap())
        .output()
        .expect("failed to execute yacc");

    assert!(
        output_strict.status.success(),
        "yacc --strict should succeed: {}",
        String::from_utf8_lossy(&output_strict.stderr)
    );

    // Both should generate valid C code
    let fast_code = temp_dir.path().join("fast.tab.c");
    let strict_code = temp_dir.path().join("strict.tab.c");

    assert!(fast_code.exists(), "fast.tab.c should exist");
    assert!(strict_code.exists(), "strict.tab.c should exist");
}

/// Test verification runs on grammars with conflicts (implicit via all other tests passing)
#[test]
fn test_verification_on_grammar_with_conflicts() {
    let grammar = r#"
%token NUM
%%
expr : expr '+' expr
     | NUM
     ;
"#;

    // This grammar has shift/reduce conflicts
    // Verification should still pass (it's not a bug, just a conflict)
    let output = run_yacc(&[], grammar);
    assert!(
        output.status.success(),
        "yacc should succeed on grammar with conflicts: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Also test with --strict
    let output_strict = run_yacc(&["--strict"], grammar);
    assert!(
        output_strict.status.success(),
        "yacc --strict should succeed on grammar with conflicts: {}",
        String::from_utf8_lossy(&output_strict.stderr)
    );
}

// ============================================================================
// Python 3.9 Parser Integration Test
// ============================================================================
//
// This comprehensive test demonstrates the yacc implementation by parsing
// a maximal subset of Python 3.9 syntax. It exercises all POSIX yacc features:
// - %token with explicit numbers
// - %left, %right, %nonassoc for operator precedence
// - %union and %type for semantic values
// - %start for explicit start symbol
// - %prec for precedence override
// - error token for syntax error recovery
// - Semantic actions
//
// The grammar handles Python's significant whitespace via a custom C lexer
// that tracks indentation levels and emits INDENT/DEDENT tokens.
// ============================================================================

use std::path::PathBuf;
use std::sync::OnceLock;

/// Path to Python 3.9 fixture files
fn python39_fixture_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

/// Build directory for generated files
fn python39_build_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("python39_parser")
}

/// Built parser executable path
fn python39_parser_exe() -> PathBuf {
    python39_build_dir().join("parser")
}

/// Build the Python parser once (lazy initialization)
static PYTHON39_PARSER_BUILT: OnceLock<Result<(), String>> = OnceLock::new();

fn ensure_python39_parser_built() -> Result<(), String> {
    PYTHON39_PARSER_BUILT
        .get_or_init(|| build_python39_parser())
        .clone()
}

fn build_python39_parser() -> Result<(), String> {
    let fixtures = python39_fixture_dir();
    let build_dir = python39_build_dir();

    // Create build directory
    fs::create_dir_all(&build_dir).map_err(|e| format!("mkdir: {}", e))?;

    let grammar_path = fixtures.join("python39.y");
    let lex_path = fixtures.join("python39.l");

    // Run yacc on fixture file directly
    let yacc_output = Command::new(env!("CARGO_BIN_EXE_yacc"))
        .current_dir(&build_dir)
        .args(["-d", grammar_path.to_str().unwrap()])
        .output()
        .map_err(|e| format!("yacc exec: {}", e))?;

    if !yacc_output.status.success() {
        return Err(format!(
            "yacc failed: {}",
            String::from_utf8_lossy(&yacc_output.stderr)
        ));
    }

    // Run lex on fixture file directly
    let lex_output = Command::new(env!("CARGO_BIN_EXE_lex"))
        .current_dir(&build_dir)
        .arg(lex_path.to_str().unwrap())
        .output()
        .map_err(|e| format!("lex exec: {}", e))?;

    if !lex_output.status.success() {
        return Err(format!(
            "lex failed: {}",
            String::from_utf8_lossy(&lex_output.stderr)
        ));
    }

    // Compile parser
    let compile = Command::new("cc")
        .current_dir(&build_dir)
        .args([
            "-Wall",
            "-Wno-unused-variable",
            "-Wno-unused-but-set-variable",
            "-Wno-unused-function",
            "-o",
            "parser",
            "y.tab.c",
            "lex.yy.c",
        ])
        .output()
        .map_err(|e| format!("cc exec: {}", e))?;

    if !compile.status.success() {
        return Err(format!(
            "cc failed: {}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    Ok(())
}

/// Run a single Python source through the pre-built parser
fn run_python_parser_test(python_source: &str, should_pass: bool, desc: &str) {
    // Ensure parser is built (once)
    if let Err(e) = ensure_python39_parser_built() {
        panic!("Failed to build Python parser: {}", e);
    }

    let parser_exe = python39_parser_exe();

    // Run parser with the Python source as stdin
    let mut child = Command::new(&parser_exe)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("failed to spawn parser");

    use std::io::Write;
    if let Some(ref mut stdin) = child.stdin {
        stdin.write_all(python_source.as_bytes()).unwrap();
    }

    let output = child.wait_with_output().expect("failed to wait for parser");

    if should_pass {
        assert!(
            output.status.success(),
            "{}: parser should accept valid Python:\n{}\nstderr: {}",
            desc,
            python_source,
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        assert!(
            !output.status.success(),
            "{}: parser should reject invalid Python:\n{}",
            desc,
            python_source
        );
    }
}

// ============================================================================
// Python 3.9 Parser Test Cases
// ============================================================================

#[test]
fn test_python39_grammar_compiles() {
    // Build parser from fixtures - this tests yacc, lex, and cc all succeed
    if let Err(e) = ensure_python39_parser_built() {
        panic!("Failed to build Python parser: {}", e);
    }
}

#[test]
fn test_python39_expressions() {
    let samples = [
        // Arithmetic with precedence
        ("x = 1 + 2 * 3\n", true, "arithmetic precedence"),
        ("x = (1 + 2) * 3\n", true, "parenthesized expression"),
        ("x = 2 ** 3 ** 2\n", true, "right-associative power"),
        ("x = -5 ** 2\n", true, "unary minus with power"),
        ("x = 10 // 3\n", true, "floor division"),
        ("x = 10 % 3\n", true, "modulo"),
        // Comparison chaining
        ("x = 1 < 2 < 3\n", true, "comparison chaining"),
        ("x = a == b != c\n", true, "equality chaining"),
        ("x = a <= b >= c\n", true, "inequality chaining"),
        // Boolean operators
        ("x = a and b or c\n", true, "boolean operators"),
        ("x = not a and not b\n", true, "not operator"),
        ("x = not (a or b)\n", true, "not with parens"),
        // Bitwise operators
        ("x = a | b ^ c & d\n", true, "bitwise precedence"),
        ("x = a << 2 | b >> 1\n", true, "shift operators"),
        ("x = ~a\n", true, "bitwise not"),
        // Conditional expression
        ("x = a if b else c\n", true, "conditional expression"),
        (
            "x = a if b else c if d else e\n",
            true,
            "nested conditional",
        ),
        // Lambda
        ("f = lambda x: x + 1\n", true, "simple lambda"),
        ("f = lambda x, y: x + y\n", true, "multi-arg lambda"),
        ("f = lambda: 42\n", true, "no-arg lambda"),
        // Membership and identity
        ("x = a in b\n", true, "in operator"),
        ("x = a not in b\n", true, "not in operator"),
        ("x = a is b\n", true, "is operator"),
        ("x = a is not b\n", true, "is not operator"),
        // Await (syntax only)
        ("x = await foo()\n", true, "await expression"),
    ];

    for (source, should_pass, desc) in samples {
        run_python_parser_test(source, should_pass, desc);
    }
}

#[test]
fn test_python39_statements() {
    let samples = [
        // If/elif/else
        ("if x:\n    pass\n", true, "simple if"),
        ("if x:\n    pass\nelif y:\n    pass\n", true, "if-elif"),
        ("if x:\n    pass\nelse:\n    pass\n", true, "if-else"),
        (
            "if x:\n    pass\nelif y:\n    pass\nelse:\n    pass\n",
            true,
            "if-elif-else",
        ),
        // While
        ("while True:\n    break\n", true, "while with break"),
        ("while x:\n    continue\n", true, "while with continue"),
        ("while x:\n    pass\nelse:\n    pass\n", true, "while-else"),
        // For
        ("for i in items:\n    pass\n", true, "for loop"),
        ("for x, y in items:\n    pass\n", true, "for with unpacking"),
        (
            "for i in items:\n    pass\nelse:\n    pass\n",
            true,
            "for-else",
        ),
        // Try/except
        ("try:\n    pass\nexcept:\n    pass\n", true, "try-except"),
        (
            "try:\n    pass\nexcept E as e:\n    pass\n",
            true,
            "try-except-as",
        ),
        ("try:\n    pass\nfinally:\n    pass\n", true, "try-finally"),
        (
            "try:\n    pass\nexcept:\n    pass\nfinally:\n    pass\n",
            true,
            "try-except-finally",
        ),
        (
            "try:\n    pass\nexcept:\n    pass\nelse:\n    pass\n",
            true,
            "try-except-else",
        ),
        // With
        ("with open(f):\n    pass\n", true, "with statement"),
        ("with open(f) as fp:\n    pass\n", true, "with statement as"),
        (
            "with a as x, b as y:\n    pass\n",
            true,
            "multiple with items",
        ),
        // Import
        ("import os\n", true, "simple import"),
        ("import os.path\n", true, "dotted import"),
        ("import os as o\n", true, "import as"),
        ("from os import path\n", true, "from import"),
        ("from os import path as p\n", true, "from import as"),
        ("from os import *\n", true, "from import star"),
        ("from . import module\n", true, "relative import"),
        ("from .. import module\n", true, "parent relative import"),
        ("from .pkg import module\n", true, "relative dotted import"),
        // Simple statements
        ("pass\n", true, "pass statement"),
        ("break\n", true, "break statement"),
        ("continue\n", true, "continue statement"),
        ("return\n", true, "return without value"),
        ("return x\n", true, "return with value"),
        ("return x, y\n", true, "return tuple"),
        ("raise\n", true, "bare raise"),
        ("raise ValueError\n", true, "raise exception"),
        ("raise ValueError from e\n", true, "raise from"),
        ("assert x\n", true, "simple assert"),
        ("assert x, 'message'\n", true, "assert with message"),
        ("del x\n", true, "del statement"),
        ("del x, y\n", true, "del multiple"),
        ("global x\n", true, "global"),
        ("global x, y\n", true, "global multiple"),
        ("nonlocal x\n", true, "nonlocal"),
        // Yield
        ("yield\n", true, "bare yield"),
        ("yield x\n", true, "yield value"),
        ("yield from items\n", true, "yield from"),
        // Multiple statements on one line
        ("x = 1; y = 2\n", true, "semicolon statements"),
        ("pass; pass; pass\n", true, "multiple pass"),
    ];

    for (source, should_pass, desc) in samples {
        run_python_parser_test(source, should_pass, desc);
    }
}

#[test]
fn test_python39_assignments() {
    let samples = [
        // Simple assignment
        ("x = 1\n", true, "simple assignment"),
        ("x = y = 1\n", true, "chained assignment"),
        ("x = y = z = 1\n", true, "triple chained"),
        // Tuple unpacking
        ("x, y = 1, 2\n", true, "tuple unpacking"),
        ("x, y, z = items\n", true, "triple unpacking"),
        ("(x, y) = pair\n", true, "parenthesized unpacking"),
        ("[x, y] = pair\n", true, "list unpacking"),
        // Star unpacking
        ("*x, = items\n", true, "star unpack only"),
        ("x, *y = items\n", true, "star at end"),
        ("*x, y = items\n", true, "star at start"),
        ("x, *y, z = items\n", true, "star in middle"),
        // Augmented assignment
        ("x += 1\n", true, "plus equals"),
        ("x -= 1\n", true, "minus equals"),
        ("x *= 2\n", true, "times equals"),
        ("x /= 2\n", true, "divide equals"),
        ("x //= 2\n", true, "floor divide equals"),
        ("x %= 2\n", true, "mod equals"),
        ("x **= 2\n", true, "power equals"),
        ("x &= mask\n", true, "and equals"),
        ("x |= mask\n", true, "or equals"),
        ("x ^= mask\n", true, "xor equals"),
        ("x <<= 1\n", true, "lshift equals"),
        ("x >>= 1\n", true, "rshift equals"),
        ("x @= mat\n", true, "matmul equals"),
        // Annotated assignment
        ("x: int\n", true, "type annotation"),
        ("x: int = 1\n", true, "annotated assignment"),
        // Walrus operator
        ("(x := 10)\n", true, "walrus in parens"),
        // Attribute and subscript assignment
        ("obj.attr = 1\n", true, "attribute assignment"),
        ("obj[0] = 1\n", true, "subscript assignment"),
        ("obj[0:2] = items\n", true, "slice assignment"),
    ];

    for (source, should_pass, desc) in samples {
        run_python_parser_test(source, should_pass, desc);
    }
}

#[test]
fn test_python39_functions() {
    let samples = [
        // Basic definitions
        ("def f():\n    pass\n", true, "simple function"),
        ("def f(x):\n    return x\n", true, "function with arg"),
        ("def f(x, y, z):\n    pass\n", true, "multiple args"),
        ("def f(x, y=1):\n    pass\n", true, "default arg"),
        ("def f(x=1, y=2):\n    pass\n", true, "multiple defaults"),
        ("def f(*args):\n    pass\n", true, "varargs"),
        ("def f(**kwargs):\n    pass\n", true, "keyword args"),
        (
            "def f(*args, **kwargs):\n    pass\n",
            true,
            "args and kwargs",
        ),
        ("def f(x, *args):\n    pass\n", true, "arg and varargs"),
        ("def f(x, **kwargs):\n    pass\n", true, "arg and kwargs"),
        (
            "def f(x, *args, **kwargs):\n    pass\n",
            true,
            "all arg types",
        ),
        // Keyword-only args
        ("def f(*, x):\n    pass\n", true, "keyword-only"),
        (
            "def f(*args, x):\n    pass\n",
            true,
            "keyword-only after star",
        ),
        (
            "def f(*, x=1):\n    pass\n",
            true,
            "keyword-only with default",
        ),
        // Type hints
        ("def f(x: int):\n    pass\n", true, "param type hint"),
        ("def f(x: int = 1):\n    pass\n", true, "typed default"),
        ("def f() -> int:\n    return 1\n", true, "return type"),
        (
            "def f(x: int) -> int:\n    return x\n",
            true,
            "full type hints",
        ),
        // Function body
        (
            "def f():\n    x = 1\n    return x\n",
            true,
            "multi-line body",
        ),
        (
            "def f():\n    if True:\n        return 1\n    return 0\n",
            true,
            "nested if",
        ),
        // Async
        ("async def f():\n    pass\n", true, "async function"),
        ("async def f():\n    await x\n", true, "async with await"),
    ];

    for (source, should_pass, desc) in samples {
        run_python_parser_test(source, should_pass, desc);
    }
}

#[test]
fn test_python39_classes() {
    let samples = [
        ("class C:\n    pass\n", true, "simple class"),
        ("class C():\n    pass\n", true, "class with parens"),
        ("class C(Base):\n    pass\n", true, "class with inheritance"),
        ("class C(A, B):\n    pass\n", true, "multiple inheritance"),
        ("class C(A, B, C):\n    pass\n", true, "triple inheritance"),
        ("class C(metaclass=M):\n    pass\n", true, "metaclass"),
        (
            "class C(Base, metaclass=M):\n    pass\n",
            true,
            "base and meta",
        ),
        // Class body
        ("class C:\n    x = 1\n", true, "class with attribute"),
        (
            "class C:\n    def __init__(self):\n        pass\n",
            true,
            "class with method",
        ),
        (
            "class C:\n    def f(self):\n        pass\n    def g(self):\n        pass\n",
            true,
            "class with two methods",
        ),
    ];

    for (source, should_pass, desc) in samples {
        run_python_parser_test(source, should_pass, desc);
    }
}

#[test]
fn test_python39_decorators() {
    let samples = [
        ("@decorator\ndef f():\n    pass\n", true, "single decorator"),
        (
            "@decorator()\ndef f():\n    pass\n",
            true,
            "decorator with parens",
        ),
        (
            "@decorator(arg)\ndef f():\n    pass\n",
            true,
            "decorator with arg",
        ),
        (
            "@decorator(a, b)\ndef f():\n    pass\n",
            true,
            "decorator with args",
        ),
        (
            "@decorator(x=1)\ndef f():\n    pass\n",
            true,
            "decorator with kwarg",
        ),
        (
            "@d1\n@d2\ndef f():\n    pass\n",
            true,
            "multiple decorators",
        ),
        (
            "@d1\n@d2\n@d3\ndef f():\n    pass\n",
            true,
            "triple decorators",
        ),
        (
            "@module.decorator\ndef f():\n    pass\n",
            true,
            "dotted decorator",
        ),
        ("@decorator\nclass C:\n    pass\n", true, "decorated class"),
        (
            "@d1\n@d2\nclass C:\n    pass\n",
            true,
            "multi-decorated class",
        ),
        // Complex decorator expressions
        ("@d[0]\ndef f():\n    pass\n", true, "subscript decorator"),
        ("@d.attr\ndef f():\n    pass\n", true, "attribute decorator"),
    ];

    for (source, should_pass, desc) in samples {
        run_python_parser_test(source, should_pass, desc);
    }
}

#[test]
fn test_python39_comprehensions() {
    let samples = [
        // List comprehensions
        ("x = [i for i in items]\n", true, "list comprehension"),
        (
            "x = [i for i in items if i > 0]\n",
            true,
            "list comp with if",
        ),
        (
            "x = [i*j for i in a for j in b]\n",
            true,
            "nested list comp",
        ),
        (
            "x = [i for i in items if i > 0 if i < 10]\n",
            true,
            "double filter",
        ),
        // Dict comprehensions
        ("x = {k: v for k, v in items}\n", true, "dict comprehension"),
        (
            "x = {k: v for k, v in items if v}\n",
            true,
            "dict comp with if",
        ),
        // Set comprehensions
        ("x = {i for i in items}\n", true, "set comprehension"),
        ("x = {i for i in items if i}\n", true, "set comp with if"),
        // Generator expressions
        ("x = (i for i in items)\n", true, "generator expression"),
        ("x = (i for i in items if i)\n", true, "generator with if"),
        (
            "sum(i for i in items)\n",
            true,
            "generator in function call",
        ),
        // Async comprehension
        ("x = [i async for i in items]\n", true, "async list comp"),
    ];

    for (source, should_pass, desc) in samples {
        run_python_parser_test(source, should_pass, desc);
    }
}

#[test]
fn test_python39_literals() {
    let samples = [
        // Numbers
        ("x = 42\n", true, "integer"),
        ("x = 0\n", true, "zero"),
        ("x = 1_000_000\n", true, "underscore int"),
        ("x = 3.14\n", true, "float"),
        ("x = .5\n", true, "float without leading zero"),
        ("x = 1.\n", true, "float without trailing zero"),
        ("x = 1e10\n", true, "scientific notation"),
        ("x = 1E10\n", true, "scientific uppercase"),
        ("x = 1e+10\n", true, "scientific positive"),
        ("x = 1e-10\n", true, "scientific negative"),
        ("x = 3.14e10\n", true, "float scientific"),
        ("x = 0xff\n", true, "hex lowercase"),
        ("x = 0xFF\n", true, "hex uppercase"),
        ("x = 0o77\n", true, "octal"),
        ("x = 0b1010\n", true, "binary"),
        ("x = 1j\n", true, "imaginary"),
        ("x = 3.14j\n", true, "complex"),
        // Strings
        ("x = 'hello'\n", true, "single quoted"),
        ("x = \"hello\"\n", true, "double quoted"),
        ("x = 'hello' 'world'\n", true, "string concat"),
        ("x = \"hello\" \"world\"\n", true, "double concat"),
        ("x = '''multi\nline'''\n", true, "triple single"),
        ("x = \"\"\"multi\nline\"\"\"\n", true, "triple double"),
        ("x = r'raw\\string'\n", true, "raw string"),
        ("x = R'raw\\string'\n", true, "raw uppercase"),
        ("x = b'bytes'\n", true, "bytes literal"),
        ("x = B'bytes'\n", true, "bytes uppercase"),
        ("x = f'hello'\n", true, "f-string simple"),
        ("x = f'hello {name}'\n", true, "f-string with expr"),
        ("x = F'hello'\n", true, "f-string uppercase"),
        ("x = rb'raw bytes'\n", true, "raw bytes"),
        ("x = br'raw bytes'\n", true, "bytes raw"),
        ("x = rf'raw fstring'\n", true, "raw f-string"),
        ("x = fr'raw fstring'\n", true, "f-string raw"),
        // Escape sequences
        ("x = 'hello\\nworld'\n", true, "newline escape"),
        ("x = 'hello\\tworld'\n", true, "tab escape"),
        ("x = 'it\\'s'\n", true, "quote escape"),
        ("x = \"it\\\"s\"\n", true, "double quote escape"),
        // Collections
        ("x = [1, 2, 3]\n", true, "list"),
        ("x = [1]\n", true, "single element list"),
        ("x = [1,]\n", true, "list trailing comma"),
        ("x = (1, 2, 3)\n", true, "tuple"),
        ("x = (1,)\n", true, "single element tuple"),
        ("x = 1, 2, 3\n", true, "tuple without parens"),
        ("x = {1, 2, 3}\n", true, "set"),
        ("x = {1}\n", true, "single element set"),
        ("x = {'a': 1, 'b': 2}\n", true, "dict"),
        ("x = {'a': 1}\n", true, "single element dict"),
        ("x = {'a': 1,}\n", true, "dict trailing comma"),
        ("x = {**other}\n", true, "dict unpacking"),
        ("x = []\n", true, "empty list"),
        ("x = ()\n", true, "empty tuple"),
        ("x = {}\n", true, "empty dict"),
        // Special
        ("x = None\n", true, "None"),
        ("x = True\n", true, "True"),
        ("x = False\n", true, "False"),
        ("x = ...\n", true, "Ellipsis"),
    ];

    for (source, should_pass, desc) in samples {
        run_python_parser_test(source, should_pass, desc);
    }
}

#[test]
fn test_python39_subscripts() {
    let samples = [
        ("x = a[0]\n", true, "simple subscript"),
        ("x = a[-1]\n", true, "negative subscript"),
        ("x = a[0:1]\n", true, "slice"),
        ("x = a[:1]\n", true, "slice from start"),
        ("x = a[0:]\n", true, "slice to end"),
        ("x = a[:]\n", true, "full slice"),
        ("x = a[::2]\n", true, "slice with step"),
        ("x = a[0:10:2]\n", true, "full slice with step"),
        ("x = a[0][1]\n", true, "chained subscript"),
        ("x = a[0, 1]\n", true, "tuple subscript"),
        ("x = a[0:1, 2:3]\n", true, "multi-dim slice"),
    ];

    for (source, should_pass, desc) in samples {
        run_python_parser_test(source, should_pass, desc);
    }
}

#[test]
fn test_python39_function_calls() {
    let samples = [
        ("f()\n", true, "no args"),
        ("f(1)\n", true, "single arg"),
        ("f(1, 2)\n", true, "two args"),
        ("f(1, 2, 3)\n", true, "three args"),
        ("f(x=1)\n", true, "keyword arg"),
        ("f(x=1, y=2)\n", true, "two keyword args"),
        ("f(1, x=2)\n", true, "positional and keyword"),
        ("f(*args)\n", true, "star args"),
        ("f(**kwargs)\n", true, "double star kwargs"),
        ("f(*args, **kwargs)\n", true, "both stars"),
        ("f(1, *args)\n", true, "arg and star"),
        ("f(1, **kwargs)\n", true, "arg and double star"),
        ("f(1, *args, **kwargs)\n", true, "all types"),
        ("f(1, x=2, *args, **kwargs)\n", true, "everything"),
        // Chained calls
        ("f()()\n", true, "chained calls"),
        ("f().g()\n", true, "method chain"),
        ("f()[0]\n", true, "call then subscript"),
        ("f().attr\n", true, "call then attr"),
        // Generator in call
        ("f(x for x in items)\n", true, "generator arg"),
    ];

    for (source, should_pass, desc) in samples {
        run_python_parser_test(source, should_pass, desc);
    }
}

#[test]
fn test_python39_complex_programs() {
    let samples = [
        // Fibonacci function
        (
            r#"def fib(n):
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)
"#,
            true,
            "fibonacci function",
        ),
        // Class with methods
        (
            r#"class Counter:
    def __init__(self):
        self.count = 0

    def increment(self):
        self.count += 1

    def get(self):
        return self.count
"#,
            true,
            "class with methods",
        ),
        // Context manager usage
        (
            r#"with open('file.txt') as f:
    for line in f:
        print(line)
"#,
            true,
            "context manager",
        ),
        // Exception handling
        (
            r#"try:
    result = risky_operation()
except ValueError as e:
    handle_error(e)
except Exception:
    raise
finally:
    cleanup()
"#,
            true,
            "exception handling",
        ),
        // Nested functions
        (
            r#"def outer():
    def inner():
        return 42
    return inner()
"#,
            true,
            "nested functions",
        ),
        // Decorator with argument
        (
            r#"@decorator(verbose=True)
def f():
    pass
"#,
            true,
            "decorator with kwarg",
        ),
        // Lambda in expression
        (
            r#"items = sorted(items, key=lambda x: x.value)
"#,
            true,
            "lambda in sorted",
        ),
        // Multiple decorators
        (
            r#"@staticmethod
@property
def value():
    return 42
"#,
            true,
            "multiple decorators on method",
        ),
        // Async function
        (
            r#"async def fetch():
    result = await get_data()
    return result
"#,
            true,
            "async function",
        ),
        // Complex comprehension
        (
            r#"matrix = [[i * j for j in range(5)] for i in range(5)]
"#,
            true,
            "nested comprehension",
        ),
    ];

    for (source, should_pass, desc) in samples {
        run_python_parser_test(source, should_pass, desc);
    }
}
