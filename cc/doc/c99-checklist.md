# C99 Compiler Compliance Audit Checklist

> **Standard Reference:** ISO/IEC 9899:1999 (C99)  
> **Purpose:** Exhaustive verification checklist for C99 compiler implementation  
> **Project:** POSIX Utils Rust C Compiler

---

## Table of Contents

1. [Keywords](#1-keywords-37-total)
2. [Types](#2-types)
3. [Type Qualifiers & Specifiers](#3-type-qualifiers--specifiers)
4. [Operators](#4-operators)
5. [Expressions](#5-expressions)
6. [Statements](#6-statements)
7. [Declarations](#7-declarations)
8. [Preprocessor](#8-preprocessor)
9. [Lexical Elements](#9-lexical-elements)
10. [C99-Specific Features](#10-c99-specific-features)
11. [Standard Library Headers](#11-standard-library-headers)
12. [Predefined Macros](#12-predefined-macros)
13. [Undefined & Implementation-Defined Behavior](#13-undefined--implementation-defined-behavior)
14. [Translation Limits](#14-translation-limits)

---

## 1. Keywords (37 Total)

### 1.1 Storage Class Specifiers
- [x] `auto`
- [x] `extern`
- [x] `register`
- [x] `static`

### 1.2 Type Specifiers
- [x] `void`
- [x] `char`
- [x] `short`
- [x] `int`
- [x] `long`
- [x] `float`
- [x] `double`
- [x] `signed`
- [x] `unsigned`
- [x] `_Bool` *(C99 new)*
- [x] `_Complex` *(C99 new)*
- N/A `_Imaginary` *(will not implement: removed in C11; no mainstream compiler implements it)*

### 1.3 Type Qualifiers
- [x] `const`
- [x] `volatile`
- [x] `restrict` *(C99 new)*

### 1.4 Struct/Union/Enum
- [x] `struct`
- [x] `union`
- [x] `enum`

### 1.5 Control Flow
- [x] `if`
- [x] `else`
- [x] `switch`
- [x] `case`
- [x] `default`
- [x] `while`
- [x] `do`
- [x] `for`
- [x] `break`
- [x] `continue`
- [x] `goto`
- [x] `return`

### 1.6 Other Keywords
- [x] `sizeof`
- [x] `typedef`
- [x] `inline` *(C99 new)*

---

## 2. Types

### 2.1 Basic Types
- [x] `void`
- [x] `char`
- [x] `signed char`
- [x] `unsigned char`
- [x] `short` / `short int` / `signed short` / `signed short int`
- [x] `unsigned short` / `unsigned short int`
- [x] `int` / `signed` / `signed int`
- [x] `unsigned` / `unsigned int`
- [x] `long` / `long int` / `signed long` / `signed long int`
- [x] `unsigned long` / `unsigned long int`
- [x] `long long` / `long long int` / `signed long long` / `signed long long int` *(C99 new)*
- [x] `unsigned long long` / `unsigned long long int` *(C99 new)*
- [x] `float`
- [x] `double`
- [x] `long double`

### 2.2 Boolean Type (C99)
- [x] `_Bool` type
- [x] Implicit conversion: any scalar → `_Bool` (0 or 1)
- [x] `<stdbool.h>` macro `bool` expands to `_Bool`
- [x] `<stdbool.h>` macro `true` expands to `1`
- [x] `<stdbool.h>` macro `false` expands to `0`

### 2.3 Complex Types (C99)
- [x] `float _Complex`
- [x] `double _Complex`
- [x] `long double _Complex`
- N/A `float _Imaginary` — *will not implement*
- N/A `double _Imaginary` — *will not implement*
- N/A `long double _Imaginary` — *will not implement*
- [x] `<complex.h>` macro `complex` expands to `_Complex`
- N/A `<complex.h>` macro `imaginary` expands to `_Imaginary` — *will not implement*
- [x] `<complex.h>` macro `I` (imaginary unit)
- [x] Complex arithmetic operations (+, -, *, /)
- [x] `creal()`, `cimag()`, `cabs()`, `carg()`, `conj()`

### 2.4 Derived Types
- [x] Arrays (fixed size)
- [x] Arrays (variable length / VLA) *(C99 new)*
- [x] Pointers
- [x] Pointers to functions
- [x] Structures
- [x] Unions
- [x] Functions

### 2.5 Enumeration Types
- [x] `enum` declaration
- [x] Enumeration constants as `int`
- [x] Explicit enumerator values
- [x] Implicit sequential values
- [x] Negative enumerator values

### 2.6 Type Compatibility
- [x] Compatible types determination
- [x] Composite type construction
- [x] Type equivalence across translation units

---

## 3. Type Qualifiers & Specifiers

### 3.1 Type Qualifiers
- [x] `const` — object not modifiable
- [x] `volatile` — side effects, no optimization
- [x] `restrict` — pointer aliasing hint *(C99 new)*
- [x] Multiple qualifiers on same type
- [x] Qualifier inheritance through pointers
- [x] `const` in array declarations `int f(int a[const])`

### 3.2 Storage Class Specifiers
- [x] `auto` — automatic storage duration (block scope default)
- [x] `extern` — external linkage
- [x] `static` at file scope — internal linkage
- [x] `static` at block scope — static storage duration
- [x] `register` — hint for register allocation
- [x] Only one storage class per declaration

### 3.3 Function Specifiers
- [x] `inline` *(C99 new)*
- [x] `inline` with `extern` (external definition)
- [x] `inline` without `extern` (inline definition only)
- [x] `static inline` functions

---

## 4. Operators

### 4.1 Arithmetic Operators
- [x] `+` (addition)
- [x] `-` (subtraction)
- [x] `*` (multiplication)
- [x] `/` (division)
- [x] `%` (modulo)
- [x] `+` (unary plus)
- [x] `-` (unary minus)
- [x] `++` (prefix increment)
- [x] `++` (postfix increment)
- [x] `--` (prefix decrement)
- [x] `--` (postfix decrement)

### 4.2 Relational Operators
- [x] `<` (less than)
- [x] `>` (greater than)
- [x] `<=` (less than or equal)
- [x] `>=` (greater than or equal)
- [x] `==` (equality)
- [x] `!=` (inequality)

### 4.3 Logical Operators
- [x] `&&` (logical AND)
- [x] `||` (logical OR)
- [x] `!` (logical NOT)
- [x] Short-circuit evaluation for `&&`
- [x] Short-circuit evaluation for `||`

### 4.4 Bitwise Operators
- [x] `&` (bitwise AND)
- [x] `|` (bitwise OR)
- [x] `^` (bitwise XOR)
- [x] `~` (bitwise NOT / complement)
- [x] `<<` (left shift)
- [x] `>>` (right shift)

### 4.5 Assignment Operators
- [x] `=` (simple assignment)
- [x] `+=` (addition assignment)
- [x] `-=` (subtraction assignment)
- [x] `*=` (multiplication assignment)
- [x] `/=` (division assignment)
- [x] `%=` (modulo assignment)
- [x] `&=` (bitwise AND assignment)
- [x] `|=` (bitwise OR assignment)
- [x] `^=` (bitwise XOR assignment)
- [x] `<<=` (left shift assignment)
- [x] `>>=` (right shift assignment)

### 4.6 Other Operators
- [x] `sizeof` (unary, type)
- [x] `sizeof` (unary, expression)
- [x] `sizeof` on VLA *(C99 new)*
- [x] `&` (address-of)
- [x] `*` (dereference)
- [x] `->` (member access through pointer)
- [x] `.` (direct member access)
- [x] `[]` (array subscript)
- [x] `()` (function call)
- [x] `(type)` (cast)
- [x] `?:` (conditional/ternary)
- [x] `,` (comma operator)

### 4.7 Operator Precedence (15 Levels)
- [x] Level 1: `()` `[]` `->` `.` (postfix `++` `--`)
- [x] Level 2: `!` `~` `++` `--` `+` `-` `*` `&` `sizeof` `(type)` (unary, right-to-left)
- [x] Level 3: `*` `/` `%`
- [x] Level 4: `+` `-`
- [x] Level 5: `<<` `>>`
- [x] Level 6: `<` `<=` `>` `>=`
- [x] Level 7: `==` `!=`
- [x] Level 8: `&`
- [x] Level 9: `^`
- [x] Level 10: `|`
- [x] Level 11: `&&`
- [x] Level 12: `||`
- [x] Level 13: `?:` (right-to-left)
- [x] Level 14: `=` `+=` `-=` etc. (right-to-left)
- [x] Level 15: `,`

### 4.8 Operator Associativity
- [x] Left-to-right for most binary operators
- [x] Right-to-left for unary, assignment, conditional

---

## 5. Expressions

### 5.1 Primary Expressions
- [x] Identifiers
- [x] Constants (integer, floating, character)
- [x] String literals
- [x] Parenthesized expressions `(expr)`

### 5.2 Postfix Expressions
- [x] Array subscripting `a[i]`
- [x] Function calls `f(args)`
- [x] Structure/union member access `.`
- [x] Structure/union member access through pointer `->`
- [x] Postfix increment `x++`
- [x] Postfix decrement `x--`
- [x] Compound literals `(type){initializer-list}` *(C99 new)*

### 5.3 Unary Expressions
- [x] Prefix increment `++x`
- [x] Prefix decrement `--x`
- [x] Address-of `&x`
- [x] Dereference `*p`
- [x] Unary plus `+x`
- [x] Unary minus `-x`
- [x] Bitwise NOT `~x`
- [x] Logical NOT `!x`
- [x] `sizeof expr`
- [x] `sizeof(type)`

### 5.4 Cast Expressions
- [x] Explicit type casts `(type)expr`
- [x] Cast to void
- [x] Cast between integer types
- [x] Cast between floating types
- [x] Cast between pointer types
- [x] Cast pointer ↔ integer (implementation-defined)

### 5.5 Constant Expressions
- [x] Integer constant expressions
- [x] Arithmetic constant expressions
- [x] Address constants
- [x] Null pointer constants
- [x] Compile-time evaluation

### 5.6 Implicit Conversions
- [x] Integer promotions
- [x] Usual arithmetic conversions
- [x] Default argument promotions
- [x] Array-to-pointer decay
- [x] Function-to-pointer decay
- [x] Lvalue conversion

---

## 6. Statements

### 6.1 Labeled Statements
- [x] Identifier labels `label:`
- [x] `case` constant-expression `:`
- [x] `default:`

### 6.2 Compound Statements (Blocks)
- [x] `{ declarations statements }`
- [x] Mixed declarations and statements *(C99 new)*
- [x] Empty blocks `{}`
- [x] Nested blocks with shadowing

### 6.3 Expression Statements
- [x] Expression followed by `;`
- [x] Null statement `;`

### 6.4 Selection Statements
- [x] `if (expr) statement`
- [x] `if (expr) statement else statement`
- [x] Dangling else resolution (bind to nearest `if`)
- [x] `switch (expr) statement`
- [x] `switch` with integer expression
- [x] `case` labels (integer constant expressions)
- [x] `default` label
- [x] Fall-through between cases
- [x] Duff's device pattern

### 6.5 Iteration Statements
- [x] `while (expr) statement`
- [x] `do statement while (expr);`
- [x] `for (expr; expr; expr) statement`
- [x] `for` with declaration `for (int i = 0; ...)` *(C99 new)*
- [x] Infinite loops `for(;;)`, `while(1)`
- [x] Omitted `for` clauses

### 6.6 Jump Statements
- [x] `goto identifier;`
- [x] `continue;`
- [x] `break;`
- [x] `return;`
- [x] `return expression;`
- [x] `goto` into block (jump past VLA — undefined)

---

## 7. Declarations

### 7.1 Declaration Syntax
- [x] Declaration specifiers + declarators
- [x] Multiple declarators per declaration
- [x] Abstract declarators (in casts, sizeof, prototypes)
- [x] Declarator with initializer

### 7.2 Variable Declarations
- [x] Simple variable `int x;`
- [x] With initializer `int x = 5;`
- [x] Multiple variables `int x, y, z;`
- [x] Pointer variable `int *p;`
- [x] Array variable `int a[10];`
- [x] Array without size (external/incomplete) `extern int a[];`
- [x] Pointer to array `int (*p)[10];`
- [x] Array of pointers `int *a[10];`

### 7.3 Function Declarations
- [x] Function prototype `int f(int, char);`
- [x] Function prototype with parameter names `int f(int x, char c);`
- [x] Function returning pointer `int *f(void);`
- [x] Pointer to function `int (*pf)(int);`
- [x] Function returning pointer to function
- [x] Empty parameter list `f()` (unspecified params, K&R style)
- [x] Void parameter list `f(void)` (no parameters)
- [x] Variadic function `f(int, ...)`

### 7.4 Function Definitions
- [x] Function body with compound statement
- [x] K&R style parameter declarations
- [x] Modern prototype-style parameters
- [x] `static` functions (internal linkage)
- [x] `inline` functions *(C99 new)*

### 7.5 Structure Declarations
- [x] `struct` with tag
- [x] Anonymous `struct`
- [x] `struct` with members
- [x] Nested `struct`
- [x] Forward declaration `struct tag;`
- [x] Self-referential `struct` (linked list)
- [x] Bit-fields `int x : 5;`
- [x] Bit-field with zero width (alignment)
- [x] Flexible array member `int data[];` *(C99 new)*

### 7.6 Union Declarations
- [x] `union` with tag
- [x] Anonymous `union`
- [x] `union` with members
- [x] `union` member access

### 7.7 Enumeration Declarations
- [x] `enum` with tag
- [x] Anonymous `enum`
- [x] Enumerators with explicit values
- [x] Enumerators with implicit values

### 7.8 Typedef
- [x] Simple typedef `typedef int INT;`
- [x] Pointer typedef `typedef int *PINT;`
- [x] Function pointer typedef `typedef int (*FP)(int);`
- [x] Array typedef `typedef int ARR[10];`
- [x] Struct typedef `typedef struct { } S;`
- [x] Typedef redeclaration (same type, allowed)

### 7.9 Initializers
- [x] Scalar initializer `int x = 5;`
- [x] Brace-enclosed initializer `int a[] = {1, 2, 3};`
- [x] Nested brace initializers for structs/arrays
- [x] String literal initializer for char arrays
- [x] Partial initialization (rest zero-initialized)
- [x] Designated initializers `{.field = val}` *(C99 new)*
- [x] Designated initializers `{[index] = val}` *(C99 new)*
- [x] Mixed designated/positional initializers *(C99 new)*
- [x] Out-of-order designated initializers *(C99 new)*
- [x] Compound literals `(int[]){1, 2, 3}` *(C99 new)*
- [x] Compound literals in initializers *(C99 new)*

---

## 8. Preprocessor

### 8.1 Directives
- [x] `#include <header>`
- [x] `#include "header"`
- [x] `#include` with macro expansion
- [x] `#define` object-like macro
- [x] `#define` function-like macro
- [x] `#define` variadic macro `__VA_ARGS__` *(C99 new)*
- [x] `#define` with empty replacement
- [x] `#undef`
- [x] `#if` constant-expression
- [x] `#ifdef`
- [x] `#ifndef`
- [x] `#else`
- [x] `#elif`
- [x] `#endif`
- [x] `#line`
- [x] `#error`
- [x] `#pragma`
- [x] `_Pragma` operator *(C99 new)*
- [x] `# ` (null directive)

### 8.2 Macro Features
- [x] Stringification `#param`
- [x] Token pasting `##`
- [x] Macro recursion prevention
- [x] Prescan of arguments
- [x] Empty macro arguments *(C99 new)*
- [x] Variadic macros `...` and `__VA_ARGS__` *(C99 new)*
- [x] Function-like macro with no arguments `M()`
- [x] Macro expansion order

### 8.3 Predefined Macros (Required)
- [x] `__DATE__` — compilation date
- [x] `__FILE__` — source filename
- [x] `__LINE__` — line number
- [x] `__TIME__` — compilation time
- [x] `__STDC__` — 1 if conforming
- [x] `__STDC_VERSION__` — `199901L` for C99 *(C99 new)*
- [x] `__STDC_HOSTED__` — 1 if hosted implementation *(C99 new)*
- [x] `__func__` — function name (not macro, identifier; implemented in parser) *(C99 new)*

### 8.4 Conditional Compilation
- [x] `defined` operator in `#if`
- [x] Logical operators in `#if`
- [x] Arithmetic in `#if`
- [x] Character constants in `#if`
- [x] Integer constants in `#if`
- [x] Undefined macros evaluate to 0

### 8.5 Pragmas
- [x] `#pragma STDC FP_CONTRACT` *(C99 new)* — recognized, no semantic effect
- [x] `#pragma STDC FENV_ACCESS` *(C99 new)* — recognized, no semantic effect
- [x] `#pragma STDC CX_LIMITED_RANGE` *(C99 new)* — recognized, no semantic effect
- [x] Unrecognized pragmas ignored

---

## 9. Lexical Elements

### 9.1 Character Set
- [x] Basic source character set (91 characters)
- [x] Basic execution character set
- [x] Extended characters
- [x] Universal character names `\uXXXX` *(C99 new)*
- [x] Universal character names `\UXXXXXXXX` *(C99 new)*
- N/A Trigraphs (??=, ??(, ??/, etc.) — *will not implement: deprecated in C99, removed in C11*
- [x] Digraphs (<:, :>, <%, %>, %:, %:%:) *(C99 new)*

### 9.2 Tokens
- [x] Keywords (identifiers reserved by language)
- [x] Identifiers
- [x] Constants
- [x] String literals
- [x] Punctuators
- [x] Header names (in `#include`)

### 9.3 Comments
- [x] Block comments `/* ... */`
- [x] Nested block comments (not allowed)
- [x] Line comments `// ...` *(C99 new)*
- [x] Comment at end of file
- [x] Comment spanning multiple lines

### 9.4 Identifiers
- [x] Start with letter or underscore
- [x] Contain letters, digits, underscores
- [x] Case sensitive
- [x] Reserved identifiers (`_X`, `__x`, etc.)
- [x] Minimum significant characters: 63 internal, 31 external *(C99 increased)*
- [x] Universal character names in identifiers *(C99 new)*

### 9.5 Integer Constants
- [x] Decimal constants
- [x] Octal constants (leading `0`)
- [x] Hexadecimal constants (`0x` or `0X`)
- [x] Integer suffixes: `u`, `U`
- [x] Integer suffixes: `l`, `L`
- [x] Integer suffixes: `ll`, `LL` *(C99 new)*
- [x] Combined suffixes: `ul`, `UL`, `ull`, `ULL`, etc.
- [x] Correct type selection per suffix

### 9.6 Floating Constants
- [x] Decimal floating constants
- [x] Hexadecimal floating constants *(C99 new)*
- [x] Exponent notation (`e`, `E`)
- [x] Binary exponent for hex floats (`p`, `P`) *(C99 new)*
- [x] Float suffix `f`, `F`
- [x] Long double suffix `l`, `L`
- [x] No suffix = `double`

### 9.7 Character Constants
- [x] Single character `'x'`
- [x] Escape sequences: `\a`, `\b`, `\f`, `\n`, `\r`, `\t`, `\v`
- [x] Escape sequences: `\\`, `\'`, `\"`, `\?`
- [x] Octal escape `\nnn`
- [x] Hexadecimal escape `\xhh`
- [x] Wide character constant `L'x'`
- [x] Multi-character constants (implementation-defined)

### 9.8 String Literals
- [x] Basic string literal `"hello"`
- [x] Adjacent string literal concatenation
- [x] Escape sequences in strings
- [x] Wide string literal `L"hello"`
- [x] Adjacent wide/narrow string concatenation rules
- [x] Null termination
- [x] Modifying string literal (undefined behavior)

---

## 10. C99-Specific Features

### 10.1 Variable Length Arrays (VLAs)
- [x] VLA declaration in block scope
- [x] VLA with runtime-determined size
- [x] `sizeof` on VLA (runtime evaluation)
- [x] VLA as function parameter `f(int n, int a[n])`
- [x] VLA in prototype `f(int n, int a[*])`
- [x] Multi-dimensional VLA
- [x] Pointer to VLA
- [x] Typedef of VLA type (variably modified type)
- [x] VLA in `for` loop scope
- [x] Goto/switch jumping into VLA scope (undefined)

### 10.2 Designated Initializers
- [x] Struct field designator `.field = value`
- [x] Array index designator `[index] = value`
- [x] Nested designators `.a.b = value`
- N/A Array range (GCC extension, NOT C99)
- [x] Mixing designated and positional
- [x] Out-of-order initialization
- [x] Overwriting previous initializer

### 10.3 Compound Literals
- [x] Array compound literal `(int[]){1, 2, 3}`
- [x] Struct compound literal `(struct S){.x = 1}`
- [x] Compound literal as lvalue
- [x] Compound literal storage duration (block scope = automatic)
- [x] Compound literal storage duration (file scope = static)
- [x] Compound literal as function argument
- [x] Modifiable compound literal

### 10.4 Flexible Array Members
- [x] Struct with flexible array member `struct { int n; int data[]; }`
- [x] `sizeof` struct (excludes flexible member)
- [x] Flexible member must be last
- [x] Single flexible member per struct
- [x] Allocation size calculation
- [x] Nested struct with flexible array (not allowed)

### 10.5 `restrict` Qualifier
- [x] Basic `restrict` pointer declaration
- [x] `restrict` in function parameters
- [x] `restrict` semantics for optimization
- [x] Multiple `restrict` pointers
- [x] `restrict` on array parameters

### 10.6 Inline Functions
- [x] `inline` function definition
- [x] `static inline` — always generates code
- [x] `extern inline` — external definition
- [x] `inline` only — inline definition (no standalone)
- [x] Inline in header files
- [x] Inline function with static variables

### 10.7 `_Bool` Type
- [x] `_Bool` declaration
- [x] Conversion to `_Bool` (0 if zero, 1 otherwise)
- [x] `_Bool` in expressions
- [x] `<stdbool.h>` macros

### 10.8 `long long` Type
- [x] `long long int` declaration
- [x] `unsigned long long int` declaration
- [x] `long long` constants with `LL` suffix
- [x] Arithmetic on `long long`
- [x] `LLONG_MIN`, `LLONG_MAX`, `ULLONG_MAX` in `<limits.h>`
- [x] `printf`/`scanf` with `%lld`, `%llu`

### 10.9 Complex Numbers
- [x] `_Complex` type declarations
- N/A `_Imaginary` type declarations — *will not implement*
- [x] Complex arithmetic
- [x] `<complex.h>` functions
- [x] `I` macro
- [x] `CMPLX`, `CMPLXF`, `CMPLXL` macros

### 10.10 Mixed Declarations and Statements
- [x] Declaration after statement in block
- [x] Declaration in `for` loop initializer
- [x] Declaration after `case` label

### 10.11 `__func__` Identifier
- [x] Predefined `__func__` in function body
- [x] Type: `static const char[]`
- [x] Value: function name

### 10.12 Hexadecimal Floating-Point Literals
- [x] Format: `0xh.hhhhpd`
- [x] Binary exponent (`p` or `P`)
- [x] Exact representation of floats
- [x] Suffix for float/long double

### 10.13 Variadic Macros
- [x] `#define M(...) use __VA_ARGS__`
- [x] `#define M(a, ...) use __VA_ARGS__`
- [x] `__VA_ARGS__` stringification
- [x] `__VA_ARGS__` token pasting
- [x] Empty `__VA_ARGS__`

### 10.14 `_Pragma` Operator
- [x] `_Pragma("directive")` syntax
- [x] Equivalent to `#pragma directive`
- [x] Use in macros
- [x] String literal destringification — consumed (pragmas are no-ops)

---

## 11. Standard Library Headers

### 11.1 C89 Headers (Required in C99)
- [ ] `<assert.h>` — diagnostics
- [ ] `<ctype.h>` — character handling
- [ ] `<errno.h>` — error numbers
- [ ] `<float.h>` — floating-point limits
- [ ] `<limits.h>` — integer limits
- [ ] `<locale.h>` — localization
- [ ] `<math.h>` — mathematics
- [ ] `<setjmp.h>` — non-local jumps
- [ ] `<signal.h>` — signal handling
- [ ] `<stdarg.h>` — variable arguments
- [ ] `<stddef.h>` — common definitions
- [ ] `<stdio.h>` — input/output
- [ ] `<stdlib.h>` — general utilities
- [ ] `<string.h>` — string handling
- [ ] `<time.h>` — date and time

### 11.2 C99 New Headers
- [ ] `<complex.h>` — complex arithmetic
- [ ] `<fenv.h>` — floating-point environment
- [ ] `<inttypes.h>` — integer format conversion
- [ ] `<iso646.h>` — alternative spellings
- [ ] `<stdbool.h>` — boolean type and values
- [ ] `<stdint.h>` — integer types
- [ ] `<tgmath.h>` — type-generic math
- [ ] `<wchar.h>` — wide character utilities
- [ ] `<wctype.h>` — wide character classification

### 11.3 Key Functions & Macros by Header

#### `<stdint.h>` Types *(C99)*
- [ ] `int8_t`, `int16_t`, `int32_t`, `int64_t`
- [ ] `uint8_t`, `uint16_t`, `uint32_t`, `uint64_t`
- [ ] `int_least8_t`, `int_least16_t`, `int_least32_t`, `int_least64_t`
- [ ] `uint_least8_t`, `uint_least16_t`, `uint_least32_t`, `uint_least64_t`
- [ ] `int_fast8_t`, `int_fast16_t`, `int_fast32_t`, `int_fast64_t`
- [ ] `uint_fast8_t`, `uint_fast16_t`, `uint_fast32_t`, `uint_fast64_t`
- [ ] `intmax_t`, `uintmax_t`
- [ ] `intptr_t`, `uintptr_t`
- [ ] Limit macros (`INT8_MIN`, `INT8_MAX`, `UINT8_MAX`, etc.)
- [ ] `INTMAX_C()`, `UINTMAX_C()` macros

#### `<inttypes.h>` *(C99)*
- [ ] `PRId8`, `PRId16`, `PRId32`, `PRId64` format specifiers
- [ ] `PRIu8`, `PRIu16`, `PRIu32`, `PRIu64` format specifiers
- [ ] `PRIx8`, `PRIx16`, `PRIx32`, `PRIx64` format specifiers
- [ ] `SCNd8`, `SCNd16`, `SCNd32`, `SCNd64` scan specifiers
- [ ] `imaxabs()`, `imaxdiv()`
- [ ] `strtoimax()`, `strtoumax()`
- [ ] `wcstoimax()`, `wcstoumax()`

#### `<fenv.h>` *(C99)*
- [ ] `fenv_t`, `fexcept_t` types
- [ ] `FE_DIVBYZERO`, `FE_INEXACT`, `FE_INVALID`, `FE_OVERFLOW`, `FE_UNDERFLOW`
- [ ] `FE_ALL_EXCEPT`
- [ ] `FE_DOWNWARD`, `FE_TONEAREST`, `FE_TOWARDZERO`, `FE_UPWARD`
- [ ] `feclearexcept()`, `fegetexceptflag()`, `feraiseexcept()`, `fesetexceptflag()`, `fetestexcept()`
- [ ] `fegetround()`, `fesetround()`
- [ ] `fegetenv()`, `feholdexcept()`, `fesetenv()`, `feupdateenv()`

#### `<math.h>` C99 Additions
- [ ] Classification macros: `fpclassify`, `isfinite`, `isinf`, `isnan`, `isnormal`, `signbit`
- [ ] Comparison macros: `isgreater`, `isgreaterequal`, `isless`, `islessequal`, `islessgreater`, `isunordered`
- [ ] `float` versions: `sinf`, `cosf`, `tanf`, `expf`, `logf`, `sqrtf`, etc.
- [ ] `long double` versions: `sinl`, `cosl`, `tanl`, `expl`, `logl`, `sqrtl`, etc.
- [ ] New functions: `cbrt`, `exp2`, `expm1`, `log2`, `log1p`
- [ ] `hypot`, `erf`, `erfc`, `lgamma`, `tgamma`
- [ ] `copysign`, `nan`, `nextafter`, `nexttoward`
- [ ] `fdim`, `fmax`, `fmin`, `fma`
- [ ] `round`, `trunc`, `lround`, `llround`, `lrint`, `llrint`
- [ ] `remainder`, `remquo`
- [ ] `scalbn`, `scalbln`, `ilogb`, `logb`
- [ ] `nearbyint`, `rint`
- [ ] `HUGE_VALF`, `HUGE_VALL`, `INFINITY`, `NAN`
- [ ] `FP_INFINITE`, `FP_NAN`, `FP_NORMAL`, `FP_SUBNORMAL`, `FP_ZERO`

#### `<stdio.h>` C99 Additions
- [ ] `snprintf()` — bounded sprintf
- [ ] `vsnprintf()` — bounded vsprintf
- [ ] `vscanf()`, `vfscanf()`, `vsscanf()` — variadic scanf
- [ ] `%a`, `%A` — hex float format specifier
- [ ] `%lld`, `%llu` — long long format specifier
- [ ] `%zu`, `%zd` — size_t format specifier
- [ ] `%td` — ptrdiff_t format specifier
- [ ] `%jd`, `%ju` — intmax_t format specifier
- [ ] `hh` length modifier (char)
- [ ] `ll` length modifier (long long)
- [ ] `z` length modifier (size_t)
- [ ] `t` length modifier (ptrdiff_t)
- [ ] `j` length modifier (intmax_t)

#### `<stdlib.h>` C99 Additions
- [ ] `atoll()` — string to long long
- [ ] `strtoll()`, `strtoull()` — string to long long
- [ ] `strtof()`, `strtold()` — string to float/long double
- [ ] `_Exit()` — immediate termination
- [ ] `llabs()` — long long absolute value
- [ ] `lldiv()`, `lldiv_t` — long long division

---

## 12. Predefined Macros

### 12.1 Required Macros
- [ ] `__DATE__` — "Mmm dd yyyy"
- [ ] `__FILE__` — current source file name
- [ ] `__LINE__` — current line number (decimal)
- [ ] `__TIME__` — "hh:mm:ss"
- [ ] `__STDC__` — 1 for conforming implementation
- [ ] `__STDC_VERSION__` — `199901L` *(C99)*
- [ ] `__STDC_HOSTED__` — 1 if hosted *(C99)*

### 12.2 Conditionally Defined Macros (C99)
- [ ] `__STDC_IEC_559__` — IEEE 754 floating-point
- [ ] `__STDC_IEC_559_COMPLEX__` — IEEE 754 complex
- [ ] `__STDC_ISO_10646__` — ISO 10646 yyyymmL

### 12.3 Predefined Identifier (Not Macro)
- [ ] `__func__` — current function name *(C99)*

---

## 13. Undefined & Implementation-Defined Behavior

### 13.1 Key Undefined Behaviors to Detect/Handle
- [ ] Signed integer overflow
- [ ] Division by zero
- [ ] Dereferencing null pointer
- [ ] Accessing array out of bounds
- [ ] Modifying string literal
- [ ] Use of uninitialized variable
- [ ] Sequence point violations (`i++ + i++`)
- [ ] Shifting by negative or >= width
- [ ] Accessing freed memory
- [ ] Multiple definitions with external linkage
- [ ] Function call without prototype (wrong argument type)
- [ ] Modifying `const` object
- [ ] Accessing union via wrong member type (strict aliasing)

### 13.2 Implementation-Defined Behaviors to Document
- [ ] `char` signedness
- [ ] Size of integer types
- [ ] Size of pointers
- [ ] Byte order (endianness)
- [ ] Behavior of right shift on negative integers
- [ ] Behavior of integer-to-pointer conversion
- [ ] Floating-point representation
- [ ] `sizeof` struct/union with padding
- [ ] Bitfield implementation details
- [ ] Multi-character constants value
- [ ] Sign of `%` result for negative operands
- [ ] Stack vs heap allocation

---

## 14. Translation Limits

### 14.1 C99 Minimum Translation Limits
- [ ] 127 nesting levels of blocks
- [ ] 63 nesting levels of conditional inclusion
- [ ] 12 pointer, array, function declarators
- [ ] 63 nesting levels of parenthesized declarators
- [ ] 63 nesting levels of parenthesized expressions
- [ ] 63 significant initial characters (internal identifier)
- [ ] 31 significant initial characters (external identifier)
- [ ] 4095 external identifiers in one translation unit
- [ ] 511 identifiers with block scope in one block
- [ ] 4095 macro identifiers simultaneously defined
- [ ] 127 parameters in one function definition
- [ ] 127 arguments in one function call
- [ ] 127 parameters in one macro definition
- [ ] 127 arguments in one macro invocation
- [ ] 4095 characters in logical source line
- [ ] 4095 characters in string literal (after concatenation)
- [ ] 65535 bytes in an object
- [ ] 15 nesting levels of `#include`
- [ ] 1023 case labels in switch
- [ ] 1023 members in struct or union
- [ ] 1023 enumeration constants in enum
- [ ] 63 levels of nested struct/union in declaration

---

## Appendix A: C99 Grammar Summary (Key Productions)

### Translation Unit
- [ ] external-declaration*
- [ ] function-definition | declaration

### Declarations
- [ ] declaration-specifiers init-declarator-list? `;`

### Function Definition
- [ ] declaration-specifiers declarator declaration-list? compound-statement

### Statements
- [ ] labeled-statement | compound-statement | expression-statement | selection-statement | iteration-statement | jump-statement

---

## Appendix B: ABI Considerations

- [ ] Calling conventions documented
- [ ] Structure layout and alignment
- [ ] Stack frame layout
- [ ] Register usage conventions
- [ ] Floating-point ABI
- [ ] Variadic argument passing
- [ ] Return value handling (small struct, large struct, floats)

---

## Appendix C: Testing Strategy

### Unit Tests
- [ ] Each keyword
- [ ] Each operator
- [ ] Each type
- [ ] Each preprocessor directive
- [ ] Each escape sequence
- [ ] Each conversion rule

### Integration Tests
- [ ] CPython compilation ✓
- [ ] Standard library compilation
- [ ] POSIX utility compilation
- [ ] Open source C99 projects

### Conformance Tests
- [ ] GCC torture tests (C99 subset)
- [ ] clang test suite (C99 subset)
- [ ] Custom C99 conformance suite

---

## Notes & Progress Tracking

| Section | Total Items | Completed | % |
|---------|-------------|-----------|---|
| 1. Keywords | 37 | 35+1 N/A | 97% |
| 2. Types | 45 | 41+4 N/A | 100% |
| 3. Qualifiers | 15 | 15 | 100% |
| 4. Operators | 63 | 63 | 100% |
| 5. Expressions | 34 | 34 | 100% |
| 6. Statements | 27 | 27 | 100% |
| 7. Declarations | 55 | 55 | 100% |
| 8. Preprocessor | 50 | 50 | 100% |
| 9. Lexical | 50 | 49+1 N/A | 100% |
| 10. C99 Features | 70 | 65+2 N/A | 96% |
| 11. Std Library | 120+ | 0 | 0% |
| 12. Macros | 15 | 0 | 0% |
| 13. UB/IDB | 30 | 0 | 0% |
| 14. Limits | 25 | 0 | 0% |

---

*Generated for C99 compliance audit. Reference: ISO/IEC 9899:1999*
