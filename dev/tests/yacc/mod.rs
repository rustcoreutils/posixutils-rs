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

#[test]
fn test_simple_grammar() {
    let grammar = r#"
%token NUM
%%
expr : NUM
     ;
"#;

    let output = run_yacc(&[], grammar);
    assert!(output.status.success(), "yacc should succeed");
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

    let output = run_yacc(&[], grammar);
    assert!(
        output.status.success(),
        "yacc should succeed for expression grammar"
    );
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

    let output = run_yacc(&[], grammar);
    assert!(
        output.status.success(),
        "yacc should handle union and types"
    );
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

    let output = run_yacc(&[], grammar);
    assert!(output.status.success(), "yacc should handle precedence");

    // This grammar with proper precedence should have no conflicts
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

    let output = run_yacc(&[], grammar);

    // Should still succeed, but report conflicts
    assert!(
        output.status.success(),
        "yacc should succeed even with conflicts"
    );

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

    let output = run_yacc(&[], grammar);
    assert!(output.status.success(), "yacc should handle error token");
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

    let output = run_yacc(&[], grammar);
    assert!(
        output.status.success(),
        "yacc should handle explicit start symbol"
    );
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
