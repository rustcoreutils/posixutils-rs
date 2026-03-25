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
- [ ] `auto`
- [ ] `extern`
- [ ] `register`
- [ ] `static`

### 1.2 Type Specifiers
- [ ] `void`
- [ ] `char`
- [ ] `short`
- [ ] `int`
- [ ] `long`
- [ ] `float`
- [ ] `double`
- [ ] `signed`
- [ ] `unsigned`
- [ ] `_Bool` *(C99 new)*
- [ ] `_Complex` *(C99 new)*
- [ ] `_Imaginary` *(C99 new)*

### 1.3 Type Qualifiers
- [ ] `const`
- [ ] `volatile`
- [ ] `restrict` *(C99 new)*

### 1.4 Struct/Union/Enum
- [ ] `struct`
- [ ] `union`
- [ ] `enum`

### 1.5 Control Flow
- [ ] `if`
- [ ] `else`
- [ ] `switch`
- [ ] `case`
- [ ] `default`
- [ ] `while`
- [ ] `do`
- [ ] `for`
- [ ] `break`
- [ ] `continue`
- [ ] `goto`
- [ ] `return`

### 1.6 Other Keywords
- [ ] `sizeof`
- [ ] `typedef`
- [ ] `inline` *(C99 new)*

---

## 2. Types

### 2.1 Basic Types
- [ ] `void`
- [ ] `char`
- [ ] `signed char`
- [ ] `unsigned char`
- [ ] `short` / `short int` / `signed short` / `signed short int`
- [ ] `unsigned short` / `unsigned short int`
- [ ] `int` / `signed` / `signed int`
- [ ] `unsigned` / `unsigned int`
- [ ] `long` / `long int` / `signed long` / `signed long int`
- [ ] `unsigned long` / `unsigned long int`
- [ ] `long long` / `long long int` / `signed long long` / `signed long long int` *(C99 new)*
- [ ] `unsigned long long` / `unsigned long long int` *(C99 new)*
- [ ] `float`
- [ ] `double`
- [ ] `long double`

### 2.2 Boolean Type (C99)
- [ ] `_Bool` type
- [ ] Implicit conversion: any scalar → `_Bool` (0 or 1)
- [ ] `<stdbool.h>` macro `bool` expands to `_Bool`
- [ ] `<stdbool.h>` macro `true` expands to `1`
- [ ] `<stdbool.h>` macro `false` expands to `0`

### 2.3 Complex Types (C99)
- [ ] `float _Complex`
- [ ] `double _Complex`
- [ ] `long double _Complex`
- [ ] `float _Imaginary`
- [ ] `double _Imaginary`
- [ ] `long double _Imaginary`
- [ ] `<complex.h>` macro `complex` expands to `_Complex`
- [ ] `<complex.h>` macro `imaginary` expands to `_Imaginary`
- [ ] `<complex.h>` macro `I` (imaginary unit)
- [ ] Complex arithmetic operations (+, -, *, /)
- [ ] `creal()`, `cimag()`, `cabs()`, `carg()`, `conj()`

### 2.4 Derived Types
- [ ] Arrays (fixed size)
- [ ] Arrays (variable length / VLA) *(C99 new)*
- [ ] Pointers
- [ ] Pointers to functions
- [ ] Structures
- [ ] Unions
- [ ] Functions

### 2.5 Enumeration Types
- [ ] `enum` declaration
- [ ] Enumeration constants as `int`
- [ ] Explicit enumerator values
- [ ] Implicit sequential values
- [ ] Negative enumerator values

### 2.6 Type Compatibility
- [ ] Compatible types determination
- [ ] Composite type construction
- [ ] Type equivalence across translation units

---

## 3. Type Qualifiers & Specifiers

### 3.1 Type Qualifiers
- [ ] `const` — object not modifiable
- [ ] `volatile` — side effects, no optimization
- [ ] `restrict` — pointer aliasing hint *(C99 new)*
- [ ] Multiple qualifiers on same type
- [ ] Qualifier inheritance through pointers
- [ ] `const` in array declarations `int f(int a[const])`

### 3.2 Storage Class Specifiers
- [ ] `auto` — automatic storage duration (block scope default)
- [ ] `extern` — external linkage
- [ ] `static` at file scope — internal linkage
- [ ] `static` at block scope — static storage duration
- [ ] `register` — hint for register allocation
- [ ] Only one storage class per declaration

### 3.3 Function Specifiers
- [ ] `inline` *(C99 new)*
- [ ] `inline` with `extern` (external definition)
- [ ] `inline` without `extern` (inline definition only)
- [ ] `static inline` functions

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
- [ ] Declaration specifiers + declarators
- [ ] Multiple declarators per declaration
- [ ] Abstract declarators (in casts, sizeof, prototypes)
- [ ] Declarator with initializer

### 7.2 Variable Declarations
- [ ] Simple variable `int x;`
- [ ] With initializer `int x = 5;`
- [ ] Multiple variables `int x, y, z;`
- [ ] Pointer variable `int *p;`
- [ ] Array variable `int a[10];`
- [ ] Array without size (external/incomplete) `extern int a[];`
- [ ] Pointer to array `int (*p)[10];`
- [ ] Array of pointers `int *a[10];`

### 7.3 Function Declarations
- [ ] Function prototype `int f(int, char);`
- [ ] Function prototype with parameter names `int f(int x, char c);`
- [ ] Function returning pointer `int *f(void);`
- [ ] Pointer to function `int (*pf)(int);`
- [ ] Function returning pointer to function
- [ ] Empty parameter list `f()` (unspecified params, K&R style)
- [ ] Void parameter list `f(void)` (no parameters)
- [ ] Variadic function `f(int, ...)`

### 7.4 Function Definitions
- [ ] Function body with compound statement
- [ ] K&R style parameter declarations
- [ ] Modern prototype-style parameters
- [ ] `static` functions (internal linkage)
- [ ] `inline` functions *(C99 new)*

### 7.5 Structure Declarations
- [ ] `struct` with tag
- [ ] Anonymous `struct`
- [ ] `struct` with members
- [ ] Nested `struct`
- [ ] Forward declaration `struct tag;`
- [ ] Self-referential `struct` (linked list)
- [ ] Bit-fields `int x : 5;`
- [ ] Bit-field with zero width (alignment)
- [ ] Flexible array member `int data[];` *(C99 new)*

### 7.6 Union Declarations
- [ ] `union` with tag
- [ ] Anonymous `union`
- [ ] `union` with members
- [ ] `union` member access

### 7.7 Enumeration Declarations
- [ ] `enum` with tag
- [ ] Anonymous `enum`
- [ ] Enumerators with explicit values
- [ ] Enumerators with implicit values

### 7.8 Typedef
- [ ] Simple typedef `typedef int INT;`
- [ ] Pointer typedef `typedef int *PINT;`
- [ ] Function pointer typedef `typedef int (*FP)(int);`
- [ ] Array typedef `typedef int ARR[10];`
- [ ] Struct typedef `typedef struct { } S;`
- [ ] Typedef redeclaration (same type, allowed)

### 7.9 Initializers
- [ ] Scalar initializer `int x = 5;`
- [ ] Brace-enclosed initializer `int a[] = {1, 2, 3};`
- [ ] Nested brace initializers for structs/arrays
- [ ] String literal initializer for char arrays
- [ ] Partial initialization (rest zero-initialized)
- [ ] Designated initializers `{.field = val}` *(C99 new)*
- [ ] Designated initializers `{[index] = val}` *(C99 new)*
- [ ] Mixed designated/positional initializers *(C99 new)*
- [ ] Out-of-order designated initializers *(C99 new)*
- [ ] Compound literals `(int[]){1, 2, 3}` *(C99 new)*
- [ ] Compound literals in initializers *(C99 new)*

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
- [ ] VLA declaration in block scope
- [ ] VLA with runtime-determined size
- [ ] `sizeof` on VLA (runtime evaluation)
- [ ] VLA as function parameter `f(int n, int a[n])`
- [ ] VLA in prototype `f(int n, int a[*])`
- [ ] Multi-dimensional VLA
- [ ] Pointer to VLA
- [ ] Typedef of VLA type (variably modified type)
- [ ] VLA in `for` loop scope
- [ ] Goto/switch jumping into VLA scope (undefined)

### 10.2 Designated Initializers
- [ ] Struct field designator `.field = value`
- [ ] Array index designator `[index] = value`
- [ ] Nested designators `.a.b = value`
- [ ] Array range (GCC extension, NOT C99)
- [ ] Mixing designated and positional
- [ ] Out-of-order initialization
- [ ] Overwriting previous initializer

### 10.3 Compound Literals
- [ ] Array compound literal `(int[]){1, 2, 3}`
- [ ] Struct compound literal `(struct S){.x = 1}`
- [ ] Compound literal as lvalue
- [ ] Compound literal storage duration (block scope = automatic)
- [ ] Compound literal storage duration (file scope = static)
- [ ] Compound literal as function argument
- [ ] Modifiable compound literal

### 10.4 Flexible Array Members
- [ ] Struct with flexible array member `struct { int n; int data[]; }`
- [ ] `sizeof` struct (excludes flexible member)
- [ ] Flexible member must be last
- [ ] Single flexible member per struct
- [ ] Allocation size calculation
- [ ] Nested struct with flexible array (not allowed)

### 10.5 `restrict` Qualifier
- [ ] Basic `restrict` pointer declaration
- [ ] `restrict` in function parameters
- [ ] `restrict` semantics for optimization
- [ ] Multiple `restrict` pointers
- [ ] `restrict` on array parameters

### 10.6 Inline Functions
- [ ] `inline` function definition
- [ ] `static inline` — always generates code
- [ ] `extern inline` — external definition
- [ ] `inline` only — inline definition (no standalone)
- [ ] Inline in header files
- [ ] Inline function with static variables

### 10.7 `_Bool` Type
- [ ] `_Bool` declaration
- [ ] Conversion to `_Bool` (0 if zero, 1 otherwise)
- [ ] `_Bool` in expressions
- [ ] `<stdbool.h>` macros

### 10.8 `long long` Type
- [ ] `long long int` declaration
- [ ] `unsigned long long int` declaration
- [ ] `long long` constants with `LL` suffix
- [ ] Arithmetic on `long long`
- [ ] `LLONG_MIN`, `LLONG_MAX`, `ULLONG_MAX` in `<limits.h>`
- [ ] `printf`/`scanf` with `%lld`, `%llu`

### 10.9 Complex Numbers
- [ ] `_Complex` type declarations
- [ ] `_Imaginary` type declarations
- [ ] Complex arithmetic
- [ ] `<complex.h>` functions
- [ ] `I` macro
- [ ] `CMPLX`, `CMPLXF`, `CMPLXL` macros

### 10.10 Mixed Declarations and Statements
- [ ] Declaration after statement in block
- [ ] Declaration in `for` loop initializer
- [ ] Declaration after `case` label

### 10.11 `__func__` Identifier
- [ ] Predefined `__func__` in function body
- [ ] Type: `static const char[]`
- [ ] Value: function name

### 10.12 Hexadecimal Floating-Point Literals
- [ ] Format: `0xh.hhhhpd`
- [ ] Binary exponent (`p` or `P`)
- [ ] Exact representation of floats
- [ ] Suffix for float/long double

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
| 1. Keywords | 37 | 0 | 0% |
| 2. Types | 45+ | 0 | 0% |
| 3. Qualifiers | 15 | 0 | 0% |
| 4. Operators | 63 | 63 | 100% |
| 5. Expressions | 34 | 34 | 100% |
| 6. Statements | 27 | 27 | 100% |
| 7. Declarations | 55 | 0 | 0% |
| 8. Preprocessor | 50 | 50 | 100% |
| 9. Lexical | 50 | 49+1 N/A | 100% |
| 10. C99 Features | 70 | 0 | 0% |
| 11. Std Library | 120+ | 0 | 0% |
| 12. Macros | 15 | 0 | 0% |
| 13. UB/IDB | 30 | 0 | 0% |
| 14. Limits | 25 | 0 | 0% |

---

*Generated for C99 compliance audit. Reference: ISO/IEC 9899:1999*
