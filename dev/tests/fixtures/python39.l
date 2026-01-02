/*
 * python39.l - POSIX lex lexer for Python 3.9 syntax
 *
 * This lexer demonstrates POSIX lex features including:
 * - %{ %} C code in definitions section
 * - {name} substitution definitions
 * - Exclusive start conditions (%x)
 * - yymore() for string accumulation
 * - input() for manual character reading
 * - unput() for pushback
 * - Token queue for INDENT/DEDENT handling
 *
 * Usage: lex python39.l && yacc -d python39.y && cc lex.yy.c y.tab.c -o parser
 */

%{
#include "y.tab.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Forward declarations for lex functions used in check_indent */
static int input(void);
static void unput(int c);

/* Forward declaration for parser */
extern int yyparse(void);

/* ========== Indentation Tracking ========== */
#define MAX_INDENT 100
static int indent_stack[MAX_INDENT] = {0};
static int indent_top = 0;

/* ========== Token Queue for INDENT/DEDENT ========== */
static int pending_dedents = 0;
static int at_line_start = 1;
static int paren_depth = 0;
static int returned_endmarker = 0;

/* ========== Line Tracking ========== */
int yylineno = 1;

/* ========== Indentation Check Function ========== */
/* Returns: INDENT, DEDENT (sets pending_dedents), ENDMARKER, or 0 (continue) */
static int check_indent(void) {
    int current_indent = 0;
    int c;

    /* Count leading whitespace */
    while (1) {
        c = input();
        if (c == ' ') {
            current_indent++;
        } else if (c == '\t') {
            /* Tab = next multiple of 8 */
            current_indent = (current_indent + 8) & ~7;
        } else if (c == '\n' || c == '\r') {
            /* Blank line - skip and restart */
            if (c == '\r') {
                int next = input();
                if (next != '\n' && next != EOF) unput(next);
            }
            yylineno++;
            current_indent = 0;
            continue;
        } else if (c == '#') {
            /* Comment line - skip to end */
            while ((c = input()) != EOF && c != '\n');
            if (c == '\n') yylineno++;
            current_indent = 0;
            continue;
        } else if (c == '\\') {
            /* Check for line continuation */
            int next = input();
            if (next == '\n') {
                yylineno++;
                current_indent = 0;
                continue;
            }
            if (next != EOF) unput(next);
            unput(c);
            break;
        } else if (c == EOF) {
            /* End of file - emit remaining DEDENTs */
            while (indent_top > 0) {
                indent_top--;
                pending_dedents++;
            }
            if (pending_dedents > 0) {
                pending_dedents--;
                return DEDENT;
            }
            returned_endmarker = 1;
            return ENDMARKER;
        } else {
            /* Non-whitespace - push back and process */
            unput(c);
            break;
        }
    }

    /* Compare with current indentation level */
    if (current_indent > indent_stack[indent_top]) {
        /* Increased indentation - push and return INDENT */
        indent_top++;
        if (indent_top >= MAX_INDENT) {
            fprintf(stderr, "Error: too many indentation levels\n");
            returned_endmarker = 1;
            return ENDMARKER;
        }
        indent_stack[indent_top] = current_indent;
        return INDENT;
    } else if (current_indent < indent_stack[indent_top]) {
        /* Decreased indentation - pop stack and queue DEDENTs */
        while (indent_top > 0 && current_indent < indent_stack[indent_top]) {
            indent_top--;
            pending_dedents++;
        }
        if (current_indent != indent_stack[indent_top]) {
            fprintf(stderr, "Error: inconsistent indentation at line %d\n", yylineno);
        }
        if (pending_dedents > 0) {
            pending_dedents--;
            return DEDENT;
        }
    }

    /* Same indentation level - no token needed */
    return 0;
}

%}

/* ========== Substitution Definitions ========== */
DIGIT           [0-9]
NONZERODIGIT    [1-9]
HEXDIGIT        [0-9a-fA-F]
OCTDIGIT        [0-7]
BINDIGIT        [01]
LETTER          [a-zA-Z_]

/* Identifier */
ID              {LETTER}({LETTER}|{DIGIT})*

/* Integer literals (Python 3.6+ allows underscores) */
DECINTEGER      {NONZERODIGIT}("_"?{DIGIT})*|"0"+("_"?"0")*
HEXINTEGER      "0"[xX]("_"?{HEXDIGIT})+
OCTINTEGER      "0"[oO]("_"?{OCTDIGIT})+
BININTEGER      "0"[bB]("_"?{BINDIGIT})+
INTEGER         {DECINTEGER}|{HEXINTEGER}|{OCTINTEGER}|{BININTEGER}

/* Float literals (Python 3.6+ allows underscores) */
EXPONENT        [eE][+-]?("_"?{DIGIT})+
POINTFLOAT      ({DIGIT}("_"?{DIGIT})*)?"."{DIGIT}("_"?{DIGIT})*|{DIGIT}("_"?{DIGIT})*"."
FLOATNUMBER     {POINTFLOAT}{EXPONENT}?|{DIGIT}("_"?{DIGIT})*{EXPONENT}

/* Imaginary literals */
IMAGNUMBER      ({FLOATNUMBER}|{DIGIT}("_"?{DIGIT})*)[jJ]

/* ========== Start Conditions ========== */
%x STRING_SQ STRING_DQ STRING_TSQ STRING_TDQ

%%
    /* ========== Code at start of yylex - check token queue ========== */
    /* This code runs at the beginning of each yylex() call */
    {
        /* After ENDMARKER, always return 0 (EOF) */
        if (returned_endmarker) {
            return 0;
        }

        /* Return pending DEDENT tokens */
        if (pending_dedents > 0) {
            pending_dedents--;
            return DEDENT;
        }

        /* Check indentation at start of line (not inside brackets) */
        if (at_line_start && paren_depth == 0) {
            int tok;
            at_line_start = 0;
            tok = check_indent();
            if (tok != 0) {
                return tok;
            }
        }
    }

 /* ========== Keywords (must come before identifiers) ========== */
"False"         { return KW_FALSE; }
"None"          { return KW_NONE; }
"True"          { return KW_TRUE; }
"and"           { return KW_AND; }
"as"            { return KW_AS; }
"assert"        { return KW_ASSERT; }
"async"         { return KW_ASYNC; }
"await"         { return KW_AWAIT; }
"break"         { return KW_BREAK; }
"class"         { return KW_CLASS; }
"continue"      { return KW_CONTINUE; }
"def"           { return KW_DEF; }
"del"           { return KW_DEL; }
"elif"          { return KW_ELIF; }
"else"          { return KW_ELSE; }
"except"        { return KW_EXCEPT; }
"finally"       { return KW_FINALLY; }
"for"           { return KW_FOR; }
"from"          { return KW_FROM; }
"global"        { return KW_GLOBAL; }
"if"            { return KW_IF; }
"import"        { return KW_IMPORT; }
"in"            { return KW_IN; }
"is"            { return KW_IS; }
"lambda"        { return KW_LAMBDA; }
"nonlocal"      { return KW_NONLOCAL; }
"not"           { return KW_NOT; }
"or"            { return KW_OR; }
"pass"          { return KW_PASS; }
"raise"         { return KW_RAISE; }
"return"        { return KW_RETURN; }
"try"           { return KW_TRY; }
"while"         { return KW_WHILE; }
"with"          { return KW_WITH; }
"yield"         { return KW_YIELD; }

 /* ========== Identifiers ========== */
{ID}            { return NAME; }

 /* ========== Number Literals ========== */
{IMAGNUMBER}    { return FLOAT_NUM; }  /* Imaginary returns as float */
{FLOATNUMBER}   { return FLOAT_NUM; }
{INTEGER}       { return NUMBER; }

 /* ========== Multi-character Operators ========== */
"**="           { return STARSTAREQ; }
"//="           { return SLASHSLASHEQ; }
"<<="           { return LSHIFTEQ; }
">>="           { return RSHIFTEQ; }
"**"            { return STARSTAR; }
"//"            { return SLASHSLASH; }
"<<"            { return LSHIFT; }
">>"            { return RSHIFT; }
"<="            { return LTEQ; }
">="            { return GTEQ; }
"=="            { return EQEQ; }
"!="            { return NOTEQ; }
":="            { return COLONEQUAL; }
"->"            { return ARROW; }
"..."           { return ELLIPSIS; }
"+="            { return PLUSEQ; }
"-="            { return MINUSEQ; }
"*="            { return STAREQ; }
"/="            { return SLASHEQ; }
"%="            { return PERCENTEQ; }
"@="            { return ATEQ; }
"&="            { return AMPEQ; }
"|="            { return BAREQ; }
"^="            { return CARETEQ; }

 /* ========== Single-character Operators ========== */
"+"             { return '+'; }
"-"             { return '-'; }
"*"             { return '*'; }
"/"             { return '/'; }
"%"             { return '%'; }
"@"             { return '@'; }
"&"             { return '&'; }
"|"             { return '|'; }
"^"             { return '^'; }
"~"             { return '~'; }
"<"             { return '<'; }
">"             { return '>'; }
"="             { return '='; }

 /* ========== Brackets - Track Depth ========== */
"("             { paren_depth++; return '('; }
")"             { paren_depth--; return ')'; }
"["             { paren_depth++; return '['; }
"]"             { paren_depth--; return ']'; }
"{"             { paren_depth++; return '{'; }
"}"             { paren_depth--; return '}'; }

 /* ========== Punctuation ========== */
":"             { return ':'; }
","             { return ','; }
";"             { return ';'; }
"."             { return '.'; }

 /* ========== String Literals ========== */
 /* Triple-quoted strings first (longer match) */
"\"\"\""        { BEGIN(STRING_TDQ); yymore(); }
"'''"           { BEGIN(STRING_TSQ); yymore(); }

 /* Single-quoted strings */
\"              { BEGIN(STRING_DQ); yymore(); }
\'              { BEGIN(STRING_SQ); yymore(); }

 /* String prefix handling (f, r, b, u) - single prefix */
[fFrRbBuU]\"\"\" { BEGIN(STRING_TDQ); yymore(); }
[fFrRbBuU]"'''" { BEGIN(STRING_TSQ); yymore(); }
[fFrRbBuU]\"    { BEGIN(STRING_DQ); yymore(); }
[fFrRbBuU]\'    { BEGIN(STRING_SQ); yymore(); }

 /* Double prefix (fr, rf, br, rb) */
[fF][rR]\"\"\"  { BEGIN(STRING_TDQ); yymore(); }
[rR][fF]\"\"\"  { BEGIN(STRING_TDQ); yymore(); }
[fF][rR]"'''"   { BEGIN(STRING_TSQ); yymore(); }
[rR][fF]"'''"   { BEGIN(STRING_TSQ); yymore(); }
[fF][rR]\"      { BEGIN(STRING_DQ); yymore(); }
[rR][fF]\"      { BEGIN(STRING_DQ); yymore(); }
[fF][rR]\'      { BEGIN(STRING_SQ); yymore(); }
[rR][fF]\'      { BEGIN(STRING_SQ); yymore(); }
[bB][rR]\"\"\"  { BEGIN(STRING_TDQ); yymore(); }
[rR][bB]\"\"\"  { BEGIN(STRING_TDQ); yymore(); }
[bB][rR]"'''"   { BEGIN(STRING_TSQ); yymore(); }
[rR][bB]"'''"   { BEGIN(STRING_TSQ); yymore(); }
[bB][rR]\"      { BEGIN(STRING_DQ); yymore(); }
[rR][bB]\"      { BEGIN(STRING_DQ); yymore(); }
[bB][rR]\'      { BEGIN(STRING_SQ); yymore(); }
[rR][bB]\'      { BEGIN(STRING_SQ); yymore(); }

 /* String content - double quoted */
<STRING_DQ>\"   { BEGIN(INITIAL); return STRING; }
<STRING_DQ>\\.  { yymore(); }
<STRING_DQ>\n   { yylineno++; yymore(); }
<STRING_DQ>.    { yymore(); }

 /* String content - single quoted */
<STRING_SQ>\'   { BEGIN(INITIAL); return STRING; }
<STRING_SQ>\\.  { yymore(); }
<STRING_SQ>\n   { yylineno++; yymore(); }
<STRING_SQ>.    { yymore(); }

 /* String content - triple double quoted */
<STRING_TDQ>\"\"\" { BEGIN(INITIAL); return STRING; }
<STRING_TDQ>\\.  { yymore(); }
<STRING_TDQ>\n   { yylineno++; yymore(); }
<STRING_TDQ>.    { yymore(); }

 /* String content - triple single quoted */
<STRING_TSQ>"'''" { BEGIN(INITIAL); return STRING; }
<STRING_TSQ>\\.  { yymore(); }
<STRING_TSQ>\n   { yylineno++; yymore(); }
<STRING_TSQ>.    { yymore(); }

 /* ========== Comments ========== */
#.*             { /* Skip comment to end of line */ }

 /* ========== Newline ========== */
\n              {
                    yylineno++;
                    if (paren_depth == 0) {
                        at_line_start = 1;
                        return NEWLINE;
                    }
                    /* Inside brackets - ignore newline */
                }
\r\n            {
                    yylineno++;
                    if (paren_depth == 0) {
                        at_line_start = 1;
                        return NEWLINE;
                    }
                }
\r              {
                    yylineno++;
                    if (paren_depth == 0) {
                        at_line_start = 1;
                        return NEWLINE;
                    }
                }

 /* ========== Whitespace (not at line start - that's handled by check_indent) ========== */
[ \t]+          { /* Skip whitespace */ }

 /* ========== Line Continuation ========== */
\\\n            { yylineno++; /* Explicit line continuation */ }
\\\r\n          { yylineno++; }
\\\r            { yylineno++; }

 /* ========== End of File ========== */
<<EOF>>         {
                    /* Emit remaining DEDENTs */
                    while (indent_top > 0) {
                        indent_top--;
                        pending_dedents++;
                    }
                    if (pending_dedents > 0) {
                        pending_dedents--;
                        return DEDENT;
                    }
                    returned_endmarker = 1;
                    return ENDMARKER;
                }

 /* ========== Default - Unrecognized Character ========== */
.               { fprintf(stderr, "Unrecognized character: '%c' at line %d\n", yytext[0], yylineno); }

%%

/* Required by POSIX lex */
int yywrap(void) {
    return 1;
}

/* Main function - calls parser */
int main(void) {
    return yyparse();
}
