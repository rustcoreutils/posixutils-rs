/*
 * python39.y - POSIX yacc grammar for Python 3.9 syntax
 *
 * This grammar demonstrates POSIX yacc features including:
 * - %{ %} C code in declarations section
 * - %union for semantic value types
 * - %token with explicit token numbers
 * - %left, %right, %nonassoc for precedence
 * - %type <tag> for non-terminal types
 * - %start to specify start symbol
 * - %prec for contextual precedence override
 * - error token for error recovery
 * - yyerrok for error state reset
 *
 * Usage: yacc -d python39.y && lex python39.l && cc y.tab.c lex.yy.c -o parser
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int yylex(void);
extern int yylineno;

void yyerror(const char *s);
%}

/* ========== Semantic Value Union ========== */
%union {
    int ival;
    double dval;
    char *sval;
}

/* ========== Token Declarations ========== */
/* Pseudo-tokens for precedence */
%token UMINUS 257
%token <sval> NAME 258
%token <sval> STRING 259
%token <ival> NUMBER 260
%token <dval> FLOAT_NUM 261
%token NEWLINE 262
%token INDENT 263
%token DEDENT 264
%token ENDMARKER 265
%token UPLUS 266
%token UNOT 267

/* Keywords - explicit token numbers 300-334 */
%token KW_FALSE 300
%token KW_NONE 301
%token KW_TRUE 302
%token KW_AND 303
%token KW_AS 304
%token KW_ASSERT 305
%token KW_ASYNC 306
%token KW_AWAIT 307
%token KW_BREAK 308
%token KW_CLASS 309
%token KW_CONTINUE 310
%token KW_DEF 311
%token KW_DEL 312
%token KW_ELIF 313
%token KW_ELSE 314
%token KW_EXCEPT 315
%token KW_FINALLY 316
%token KW_FOR 317
%token KW_FROM 318
%token KW_GLOBAL 319
%token KW_IF 320
%token KW_IMPORT 321
%token KW_IN 322
%token KW_IS 323
%token KW_LAMBDA 324
%token KW_NONLOCAL 325
%token KW_NOT 326
%token KW_OR 327
%token KW_PASS 328
%token KW_RAISE 329
%token KW_RETURN 330
%token KW_TRY 331
%token KW_WHILE 332
%token KW_WITH 333
%token KW_YIELD 334

/* Compound operators - explicit token numbers 400-423 */
%token COLONEQUAL 400
%token PLUSEQ 401
%token MINUSEQ 402
%token STAREQ 403
%token SLASHEQ 404
%token PERCENTEQ 405
%token AMPEQ 406
%token BAREQ 407
%token CARETEQ 408
%token LSHIFTEQ 409
%token RSHIFTEQ 410
%token STARSTAREQ 411
%token SLASHSLASHEQ 412
%token ATEQ 413
%token EQEQ 414
%token NOTEQ 415
%token LTEQ 416
%token GTEQ 417
%token LSHIFT 418
%token RSHIFT 419
%token STARSTAR 420
%token SLASHSLASH 421
%token ELLIPSIS 422
%token ARROW 423

/* ========== Operator Precedence (lowest to highest) ========== */
/* Walrus operator */
%right COLONEQUAL

/* Boolean operators */
%left KW_OR
%left KW_AND
%right KW_NOT UNOT

/* Comparison operators - non-associative for chained comparisons */
%nonassoc KW_IN KW_IS NOTEQ EQEQ '<' '>' LTEQ GTEQ

/* Bitwise operators */
%left '|'
%left '^'
%left '&'
%left LSHIFT RSHIFT

/* Arithmetic operators */
%left '+' '-'
%left '*' '@' '/' '%' SLASHSLASH

/* Unary operators */
%right UMINUS UPLUS '~'

/* Exponentiation - right associative */
%right STARSTAR

/* ========== Start Symbol ========== */
%start file_input

/* ========== Non-terminal Types ========== */
%type <ival> file_input statements statement simple_stmt compound_stmt
%type <ival> expr_stmt assignment_stmt augmented_assign_stmt annotated_stmt
%type <ival> if_stmt while_stmt for_stmt try_stmt with_stmt
%type <ival> funcdef classdef decorated async_stmt
%type <ival> expr or_expr and_expr not_expr comparison
%type <ival> bitor_expr bitxor_expr bitand_expr shift_expr
%type <ival> arith_expr term factor power await_expr primary
%type <ival> atom atom_expr trailer_list trailer
%type <ival> testlist exprlist arglist argument
%type <ival> comp_for comp_if comp_iter
%type <ival> list_maker dict_maker set_maker
%type <ival> parameters param_list param
%type <ival> suite elif_list except_clauses except_clause
%type <ival> with_items with_item
%type <ival> import_stmt import_name import_from
%type <ival> dotted_name dotted_as_names dotted_as_name
%type <ival> import_as_names import_as_name
%type <ival> decorators decorator
%type <ival> sliceop subscript subscriptlist
%type <ival> dictorsetmaker star_expr
%type <ival> yield_expr yield_arg
%type <ival> lambda_expr lambda_params
%type <ival> named_expr target targets target_list rhs_list for_targets for_target
%type <ival> del_stmt pass_stmt break_stmt continue_stmt return_stmt
%type <ival> raise_stmt global_stmt nonlocal_stmt assert_stmt
%type <ival> small_stmt small_stmts
%type <ival> strings

%%

/* ========== File Structure ========== */
file_input
    : statements ENDMARKER         { $$ = 0; }
    | ENDMARKER                    { $$ = 0; }
    ;

statements
    : statements statement         { $$ = 0; }
    | statement                    { $$ = 0; }
    ;

statement
    : simple_stmt                  { $$ = $1; }
    | compound_stmt                { $$ = $1; }
    | error NEWLINE                { yyerrok; $$ = 0; }
    ;

/* ========== Simple Statements ========== */
simple_stmt
    : small_stmts NEWLINE          { $$ = 0; }
    | small_stmts ';' NEWLINE      { $$ = 0; }
    ;

small_stmts
    : small_stmts ';' small_stmt   { $$ = 0; }
    | small_stmt                   { $$ = $1; }
    ;

small_stmt
    : expr_stmt                    { $$ = $1; }
    | del_stmt                     { $$ = $1; }
    | pass_stmt                    { $$ = $1; }
    | break_stmt                   { $$ = $1; }
    | continue_stmt                { $$ = $1; }
    | return_stmt                  { $$ = $1; }
    | raise_stmt                   { $$ = $1; }
    | import_stmt                  { $$ = $1; }
    | global_stmt                  { $$ = $1; }
    | nonlocal_stmt                { $$ = $1; }
    | assert_stmt                  { $$ = $1; }
    ;

/* ========== Expression Statement ========== */
expr_stmt
    : assignment_stmt              { $$ = $1; }
    | augmented_assign_stmt        { $$ = $1; }
    | annotated_stmt               { $$ = $1; }
    | named_expr                   { $$ = $1; }
    | testlist                     { $$ = $1; }
    | yield_expr                   { $$ = $1; }
    ;

/* Type annotation (PEP 526) */
annotated_stmt
    : NAME ':' expr                        { $$ = 0; }  /* x: int */
    | NAME ':' expr '=' expr               { $$ = 0; }  /* x: int = 5 */
    | atom_expr ':' expr                   { $$ = 0; }  /* obj.attr: int */
    | atom_expr ':' expr '=' expr          { $$ = 0; }  /* obj.attr: int = 5 */
    ;

assignment_stmt
    : testlist '=' rhs_list        { $$ = 0; }  /* x = 1, or x, y = 1, 2 */
    | testlist '=' yield_expr      { $$ = 0; }
    ;

rhs_list
    : rhs_list '=' testlist        { $$ = 0; }  /* chained: x = y = 1 */
    | rhs_list '=' yield_expr      { $$ = 0; }
    | testlist                     { $$ = $1; }
    | yield_expr                   { $$ = $1; }
    ;

targets
    : targets '=' target           { $$ = 0; }
    | target                       { $$ = $1; }
    ;

target
    : atom_expr                    { $$ = $1; }
    | '(' target_list ')'          { $$ = $2; }
    | '(' target ')'               { $$ = $2; }  /* parenthesized single target */
    /* Note: '[' target_list ']' removed to avoid conflict with list literals */
    ;

target_list
    : target_list ',' target       { $$ = 0; }
    | target                       { $$ = $1; }
    | target_list ','              { $$ = $1; }
    ;

augmented_assign_stmt
    : atom_expr PLUSEQ testlist         { $$ = 0; }
    | atom_expr MINUSEQ testlist        { $$ = 0; }
    | atom_expr STAREQ testlist         { $$ = 0; }
    | atom_expr SLASHEQ testlist        { $$ = 0; }
    | atom_expr PERCENTEQ testlist      { $$ = 0; }
    | atom_expr AMPEQ testlist          { $$ = 0; }
    | atom_expr BAREQ testlist          { $$ = 0; }
    | atom_expr CARETEQ testlist        { $$ = 0; }
    | atom_expr LSHIFTEQ testlist       { $$ = 0; }
    | atom_expr RSHIFTEQ testlist       { $$ = 0; }
    | atom_expr STARSTAREQ testlist     { $$ = 0; }
    | atom_expr SLASHSLASHEQ testlist   { $$ = 0; }
    | atom_expr ATEQ testlist           { $$ = 0; }
    ;

/* Named expression (walrus operator) */
named_expr
    : NAME COLONEQUAL expr         { $$ = 0; }
    ;

/* ========== Other Simple Statements ========== */
del_stmt
    : KW_DEL exprlist              { $$ = 0; }
    ;

pass_stmt
    : KW_PASS                      { $$ = 0; }
    ;

break_stmt
    : KW_BREAK                     { $$ = 0; }
    ;

continue_stmt
    : KW_CONTINUE                  { $$ = 0; }
    ;

return_stmt
    : KW_RETURN                    { $$ = 0; }
    | KW_RETURN testlist           { $$ = 0; }
    ;

raise_stmt
    : KW_RAISE                     { $$ = 0; }
    | KW_RAISE expr                { $$ = 0; }
    | KW_RAISE expr KW_FROM expr   { $$ = 0; }
    ;

global_stmt
    : KW_GLOBAL name_list          { $$ = 0; }
    ;

nonlocal_stmt
    : KW_NONLOCAL name_list        { $$ = 0; }
    ;

name_list
    : name_list ',' NAME           { }
    | NAME                         { }
    ;

assert_stmt
    : KW_ASSERT expr               { $$ = 0; }
    | KW_ASSERT expr ',' expr      { $$ = 0; }
    ;

/* ========== Import Statements ========== */
import_stmt
    : import_name                  { $$ = $1; }
    | import_from                  { $$ = $1; }
    ;

import_name
    : KW_IMPORT dotted_as_names    { $$ = 0; }
    ;

import_from
    : KW_FROM dotted_name KW_IMPORT '*'                { $$ = 0; }
    | KW_FROM dotted_name KW_IMPORT '(' import_as_names ')' { $$ = 0; }
    | KW_FROM dotted_name KW_IMPORT import_as_names    { $$ = 0; }
    | KW_FROM dots dotted_name KW_IMPORT '*'           { $$ = 0; }
    | KW_FROM dots dotted_name KW_IMPORT '(' import_as_names ')' { $$ = 0; }
    | KW_FROM dots dotted_name KW_IMPORT import_as_names { $$ = 0; }
    | KW_FROM dots KW_IMPORT '*'                       { $$ = 0; }
    | KW_FROM dots KW_IMPORT '(' import_as_names ')'   { $$ = 0; }
    | KW_FROM dots KW_IMPORT import_as_names           { $$ = 0; }
    ;

dots
    : dots '.'                     { }
    | '.'                          { }
    ;

dotted_name
    : dotted_name '.' NAME         { $$ = 0; }
    | NAME                         { $$ = 0; }
    ;

dotted_as_names
    : dotted_as_names ',' dotted_as_name { $$ = 0; }
    | dotted_as_name               { $$ = $1; }
    ;

dotted_as_name
    : dotted_name                  { $$ = $1; }
    | dotted_name KW_AS NAME       { $$ = 0; }
    ;

import_as_names
    : import_as_names ',' import_as_name { $$ = 0; }
    | import_as_name               { $$ = $1; }
    | import_as_names ','          { $$ = $1; }
    ;

import_as_name
    : NAME                         { $$ = 0; }
    | NAME KW_AS NAME              { $$ = 0; }
    ;

/* ========== Compound Statements ========== */
compound_stmt
    : if_stmt                      { $$ = $1; }
    | while_stmt                   { $$ = $1; }
    | for_stmt                     { $$ = $1; }
    | try_stmt                     { $$ = $1; }
    | with_stmt                    { $$ = $1; }
    | funcdef                      { $$ = $1; }
    | classdef                     { $$ = $1; }
    | decorated                    { $$ = $1; }
    | async_stmt                   { $$ = $1; }
    ;

/* ========== If Statement ========== */
if_stmt
    : KW_IF expr ':' suite elif_list KW_ELSE ':' suite { $$ = 0; }
    | KW_IF expr ':' suite elif_list                   { $$ = 0; }
    | KW_IF expr ':' suite KW_ELSE ':' suite           { $$ = 0; }
    | KW_IF expr ':' suite                             { $$ = 0; }
    ;

elif_list
    : elif_list KW_ELIF expr ':' suite { $$ = 0; }
    | KW_ELIF expr ':' suite       { $$ = 0; }
    ;

/* ========== While Statement ========== */
while_stmt
    : KW_WHILE expr ':' suite KW_ELSE ':' suite { $$ = 0; }
    | KW_WHILE expr ':' suite      { $$ = 0; }
    ;

/* ========== For Statement ========== */
/* Uses for_targets instead of exprlist to avoid KW_IN conflict with comparisons */
for_stmt
    : KW_FOR for_targets KW_IN testlist ':' suite KW_ELSE ':' suite { $$ = 0; }
    | KW_FOR for_targets KW_IN testlist ':' suite                   { $$ = 0; }
    ;

/* For loop targets - simpler than exprlist, avoids comparison conflict */
for_targets
    : for_targets ',' for_target   { $$ = 0; }
    | for_target                   { $$ = $1; }
    | for_targets ','              { $$ = $1; }
    ;

for_target
    : atom_expr                    { $$ = $1; }
    | '*' for_target               { $$ = 0; }
    ;

/* ========== Try Statement ========== */
try_stmt
    : KW_TRY ':' suite except_clauses KW_ELSE ':' suite KW_FINALLY ':' suite { $$ = 0; }
    | KW_TRY ':' suite except_clauses KW_FINALLY ':' suite                   { $$ = 0; }
    | KW_TRY ':' suite except_clauses KW_ELSE ':' suite                      { $$ = 0; }
    | KW_TRY ':' suite except_clauses                                         { $$ = 0; }
    | KW_TRY ':' suite KW_FINALLY ':' suite                                   { $$ = 0; }
    ;

except_clauses
    : except_clauses except_clause { $$ = 0; }
    | except_clause                { $$ = $1; }
    ;

except_clause
    : KW_EXCEPT ':' suite                    { $$ = 0; }
    | KW_EXCEPT expr ':' suite               { $$ = 0; }
    | KW_EXCEPT expr KW_AS NAME ':' suite    { $$ = 0; }
    ;

/* ========== With Statement ========== */
with_stmt
    : KW_WITH with_items ':' suite { $$ = 0; }
    ;

with_items
    : with_items ',' with_item     { $$ = 0; }
    | with_item                    { $$ = $1; }
    ;

with_item
    : expr                         { $$ = $1; }
    | expr KW_AS target            { $$ = 0; }
    ;

/* ========== Function Definition ========== */
funcdef
    : KW_DEF NAME parameters ':' suite                    { $$ = 0; }
    | KW_DEF NAME parameters ARROW expr ':' suite         { $$ = 0; }
    ;

parameters
    : '(' ')'                      { $$ = 0; }
    | '(' param_list ')'           { $$ = $2; }
    | '(' param_list ',' ')'       { $$ = $2; }
    ;

param_list
    : param_list ',' param         { $$ = 0; }
    | param                        { $$ = $1; }
    ;

param
    : NAME                         { $$ = 0; }
    | NAME ':' expr                { $$ = 0; }
    | NAME '=' expr                { $$ = 0; }
    | NAME ':' expr '=' expr       { $$ = 0; }
    | '*'                          { $$ = 0; }
    | '*' NAME                     { $$ = 0; }
    | '*' NAME ':' expr            { $$ = 0; }
    | STARSTAR NAME                { $$ = 0; }
    | STARSTAR NAME ':' expr       { $$ = 0; }
    ;

/* ========== Class Definition ========== */
classdef
    : KW_CLASS NAME ':' suite                       { $$ = 0; }
    | KW_CLASS NAME '(' ')' ':' suite               { $$ = 0; }
    | KW_CLASS NAME '(' arglist ')' ':' suite       { $$ = 0; }
    ;

/* ========== Decorators ========== */
decorated
    : decorators funcdef           { $$ = $2; }
    | decorators classdef          { $$ = $2; }
    | decorators async_stmt        { $$ = $2; }
    ;

decorators
    : decorators decorator         { $$ = 0; }
    | decorator                    { $$ = $1; }
    ;

decorator
    : '@' expr NEWLINE                          { $$ = 0; }
    ;

/* ========== Async Statement ========== */
async_stmt
    : KW_ASYNC funcdef             { $$ = $2; }
    | KW_ASYNC for_stmt            { $$ = $2; }
    | KW_ASYNC with_stmt           { $$ = $2; }
    ;

/* ========== Suite (Block) ========== */
suite
    : simple_stmt                  { $$ = $1; }
    | NEWLINE INDENT statements DEDENT { $$ = 0; }
    ;

/* ========== Expressions ========== */
expr
    : or_expr KW_IF or_expr KW_ELSE expr  { $$ = 0; }  /* Conditional expression */
    | or_expr                      { $$ = $1; }
    | lambda_expr                  { $$ = $1; }
    ;

lambda_expr
    : KW_LAMBDA ':' expr           { $$ = 0; }
    | KW_LAMBDA lambda_params ':' expr { $$ = 0; }
    ;

lambda_params
    : lambda_params ',' NAME       { $$ = 0; }
    | NAME                         { $$ = 0; }
    | lambda_params ',' NAME '=' expr { $$ = 0; }
    | NAME '=' expr                { $$ = 0; }
    | lambda_params ',' '*' NAME   { $$ = 0; }
    | '*' NAME                     { $$ = 0; }
    | lambda_params ',' STARSTAR NAME { $$ = 0; }
    | STARSTAR NAME                { $$ = 0; }
    ;

or_expr
    : or_expr KW_OR and_expr       { $$ = 0; }
    | and_expr                     { $$ = $1; }
    ;

and_expr
    : and_expr KW_AND not_expr     { $$ = 0; }
    | not_expr                     { $$ = $1; }
    ;

not_expr
    : KW_NOT not_expr %prec UNOT   { $$ = 0; }
    | comparison                   { $$ = $1; }
    ;

/* Comparison - allows chaining like a < b < c */
comparison
    : comparison comp_op bitor_expr { $$ = 0; }
    | bitor_expr                   { $$ = $1; }
    ;

comp_op
    : '<'                          { }
    | '>'                          { }
    | EQEQ                         { }
    | GTEQ                         { }
    | LTEQ                         { }
    | NOTEQ                        { }
    | KW_IN                        { }
    | KW_NOT KW_IN                 { }
    | KW_IS                        { }
    | KW_IS KW_NOT                 { }
    ;

bitor_expr
    : bitor_expr '|' bitxor_expr   { $$ = 0; }
    | bitxor_expr                  { $$ = $1; }
    ;

bitxor_expr
    : bitxor_expr '^' bitand_expr  { $$ = 0; }
    | bitand_expr                  { $$ = $1; }
    ;

bitand_expr
    : bitand_expr '&' shift_expr   { $$ = 0; }
    | shift_expr                   { $$ = $1; }
    ;

shift_expr
    : shift_expr LSHIFT arith_expr { $$ = 0; }
    | shift_expr RSHIFT arith_expr { $$ = 0; }
    | arith_expr                   { $$ = $1; }
    ;

arith_expr
    : arith_expr '+' term          { $$ = 0; }
    | arith_expr '-' term          { $$ = 0; }
    | term                         { $$ = $1; }
    ;

term
    : term '*' factor              { $$ = 0; }
    | term '@' factor              { $$ = 0; }
    | term '/' factor              { $$ = 0; }
    | term '%' factor              { $$ = 0; }
    | term SLASHSLASH factor       { $$ = 0; }
    | factor                       { $$ = $1; }
    ;

factor
    : '+' factor %prec UPLUS       { $$ = 0; }
    | '-' factor %prec UMINUS      { $$ = 0; }
    | '~' factor                   { $$ = 0; }
    | power                        { $$ = $1; }
    ;

power
    : await_expr STARSTAR factor   { $$ = 0; }
    | await_expr                   { $$ = $1; }
    ;

await_expr
    : KW_AWAIT primary             { $$ = 0; }
    | primary                      { $$ = $1; }
    ;

primary
    : atom_expr                    { $$ = $1; }
    ;

atom_expr
    : atom trailer_list            { $$ = 0; }
    | atom                         { $$ = $1; }
    ;

trailer_list
    : trailer_list trailer         { $$ = 0; }
    | trailer                      { $$ = $1; }
    ;

trailer
    : '(' ')'                      { $$ = 0; }
    | '(' arglist ')'              { $$ = $2; }
    | '[' subscriptlist ']'        { $$ = $2; }
    | '.' NAME                     { $$ = 0; }
    ;

/* ========== Atoms ========== */
atom
    : NAME                         { $$ = 0; }
    | NUMBER                       { $$ = 0; }
    | FLOAT_NUM                    { $$ = 0; }
    | STRING                       { $$ = 0; }
    | strings                      { $$ = 0; }
    | KW_TRUE                      { $$ = 0; }
    | KW_FALSE                     { $$ = 0; }
    | KW_NONE                      { $$ = 0; }
    | ELLIPSIS                     { $$ = 0; }
    | '(' ')'                      { $$ = 0; }
    | '(' testlist ')'             { $$ = $2; }
    | '(' named_expr ')'           { $$ = $2; }  /* Walrus operator */
    | '(' expr comp_for ')'        { $$ = 0; }  /* Generator expression */
    | '(' yield_expr ')'           { $$ = $2; }
    | '[' ']'                      { $$ = 0; }
    | '[' list_maker ']'           { $$ = $2; }
    | '{' '}'                      { $$ = 0; }
    | '{' dictorsetmaker '}'       { $$ = $2; }
    ;

/* Multiple consecutive strings are concatenated */
strings
    : strings STRING               { $$ = 0; }
    | STRING STRING                { $$ = 0; }
    ;

/* ========== Subscript ========== */
subscriptlist
    : subscriptlist ',' subscript  { $$ = 0; }
    | subscript                    { $$ = $1; }
    | subscriptlist ','            { $$ = $1; }
    ;

subscript
    : expr                         { $$ = $1; }
    | expr ':' expr                { $$ = 0; }
    | expr ':' expr ':' expr       { $$ = 0; }
    | ':' expr                     { $$ = 0; }
    | ':' expr ':' expr            { $$ = 0; }
    | expr ':'                     { $$ = 0; }
    | expr ':' ':' expr            { $$ = 0; }
    | ':'                          { $$ = 0; }
    | ':' ':'                      { $$ = 0; }
    | ':' ':' expr                 { $$ = 0; }
    ;

sliceop
    : ':' expr                     { $$ = 0; }
    | ':'                          { $$ = 0; }
    ;

/* ========== List, Dict, Set Makers ========== */
list_maker
    : testlist                     { $$ = $1; }
    | expr comp_for                { $$ = 0; }
    ;

dictorsetmaker
    : dict_maker                   { $$ = $1; }
    | set_maker                    { $$ = $1; }
    ;

dict_maker
    : dict_maker ',' expr ':' expr { $$ = 0; }
    | expr ':' expr                { $$ = 0; }
    | dict_maker ','               { $$ = $1; }
    | expr ':' expr comp_for       { $$ = 0; }
    | dict_maker ',' STARSTAR expr { $$ = 0; }
    | STARSTAR expr                { $$ = 0; }
    ;

set_maker
    : testlist                     { $$ = $1; }
    | expr comp_for                { $$ = 0; }
    ;

/* ========== Comprehensions ========== */
/* Uses for_targets instead of exprlist to avoid KW_IN conflict */
comp_for
    : KW_FOR for_targets KW_IN or_expr comp_iter { $$ = 0; }
    | KW_FOR for_targets KW_IN or_expr           { $$ = 0; }
    | KW_ASYNC KW_FOR for_targets KW_IN or_expr comp_iter { $$ = 0; }
    | KW_ASYNC KW_FOR for_targets KW_IN or_expr  { $$ = 0; }
    ;

comp_iter
    : comp_for                     { $$ = $1; }
    | comp_if                      { $$ = $1; }
    ;

comp_if
    : KW_IF or_expr comp_iter      { $$ = 0; }
    | KW_IF or_expr                { $$ = 0; }
    ;

/* ========== Yield Expression ========== */
yield_expr
    : KW_YIELD yield_arg           { $$ = 0; }
    | KW_YIELD                     { $$ = 0; }
    ;

yield_arg
    : KW_FROM expr                 { $$ = 0; }
    | testlist                     { $$ = $1; }
    ;

/* ========== Star Expression ========== */
star_expr
    : '*' expr                     { $$ = 0; }
    ;

/* ========== Argument Lists ========== */
arglist
    : arglist ',' argument         { $$ = 0; }
    | argument                     { $$ = $1; }
    | arglist ','                  { $$ = $1; }
    ;

argument
    : expr                         { $$ = $1; }
    | expr comp_for                { $$ = 0; }
    | NAME '=' expr                { $$ = 0; }
    | '*' expr                     { $$ = 0; }
    | STARSTAR expr                { $$ = 0; }
    ;

/* ========== Test and Expr Lists ========== */
testlist
    : testlist ',' expr            { $$ = 0; }
    | expr                         { $$ = $1; }
    | testlist ','                 { $$ = $1; }
    | testlist ',' star_expr       { $$ = 0; }
    | star_expr                    { $$ = $1; }
    ;

exprlist
    : exprlist ',' expr            { $$ = 0; }
    | expr                         { $$ = $1; }
    | exprlist ','                 { $$ = $1; }
    | exprlist ',' star_expr       { $$ = 0; }
    | star_expr                    { $$ = $1; }
    ;

%%

/* ========== Error Handler ========== */
void yyerror(const char *s) {
    fprintf(stderr, "Parse error at line %d: %s\n", yylineno, s);
}
