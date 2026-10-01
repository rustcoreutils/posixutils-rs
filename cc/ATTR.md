# Attributes

This document describes attribute support in c17: what each attribute does,
what `__has_attribute` answers for it, and how to add a new one.

## Table of Contents

- [Overview](#overview)
- [Implemented Attributes](#implemented-attributes)
- [Accepted and Ignored](#accepted-and-ignored)
- [Not Recognised](#not-recognised)
- [`__has_attribute`](#__has_attribute)
- [Adding an Attribute](#adding-an-attribute)
- [References](#references)

## Overview

c17 accepts GNU attributes only:

1. `__attribute__((name))` or `__attribute((name))`, with the name spelled
   `name` or `__name__`. Every recognised attribute has both spellings.
2. The C11 keyword `_Noreturn`, and `__noreturn__` written as a declaration
   specifier outside any `__attribute__`, both of which set the same
   `TypeModifiers::NORETURN` bit.

There is no `[[...]]` attribute syntax (that is C23); `[[noreturn]]` is a parse
error, and `__has_c_attribute` is not defined.

`const` is a keyword, but `__attribute__((const))` is accepted in either
spelling as gcc accepts it, because an attribute name is matched by text.

### Where an attribute is written

An attribute applies to a declarator or to a whole declaration depending on
where it appears, and c17 follows gcc:

```c
int p, q __attribute__((aligned(64))), r;   /* q alone */
int __attribute__((aligned(64))) u, v;      /* u and v */
_Alignas(64) int w, x;                      /* w and x */
```

This matters for the per-symbol attributes -- `weak`, `section`, `visibility`,
`used` and `alias` -- where naming the wrong symbol is an ABI change rather than
a missed optimization. `pure` and `const` are stricter than gcc: written among
the specifiers they apply to the first declarator only, since wrongly believing
a callee writes nothing is a miscompile at the call site.

### Arguments

An argument is read as gcc reads it: an optional leading bare name (`printf` in
`format(printf, 1, 2)`, `QI` in `mode(QI)`), then strings and integer constant
expressions, folded by the same evaluator as array bounds, so `aligned(0x40)`,
an enumerator and `sizeof` all work. Integer-valued attributes take no names.
A bad argument drops the attribute after a diagnostic:

| Attribute | Bad argument |
|-----------|--------------|
| `aligned` | error unless a power of two no larger than 2^28; `aligned(0)` is a warning and ignored |
| `vector_size` | error unless a positive constant |
| `constructor`, `destructor` | error unless 0 to 65535 |
| `nonnull`, `nonnull_if_nonzero`, `sentinel`, `alloc_size`, `alloc_align`, `regparm` | warning; the attribute is dropped |

The arguments of an unrecognised attribute are skipped without being parsed.

## Implemented Attributes

| Attribute | Applies to | Effect |
|-----------|-----------|--------|
| `noreturn` | Functions | Part of the function *type*. The linearizer emits an `Unreachable` after every call through such a type, which the backends lower to `ud2` (x86-64) or `brk #1` (AArch64), and which ends the block for the optimizer. A noreturn function's own body is compiled normally |
| `aligned` | Types, variables, struct members, functions | Raises alignment: layout and `_Alignof` on a type, `.align` on a variable, `.p2align` on a function. Bare `aligned` means 16. Several requests take the largest |
| `packed` | Structs, unions, struct and union members | On a member, drops the alignment its type demands to 1 (a typedef's `aligned` included) and packs a bit-field to the bit; the rest of the aggregate is unaffected, and the aggregate's alignment is the largest its members then demand. On a struct or union it is `packed` on every member. One rule, `TypeTable::member_alignment`, combines it with the rest: an `aligned`/`_Alignas` written on the member raises the result and never lowers it, and a `#pragma pack(n)` in force caps it at `n` last, written alignment included. Accepted anywhere in the struct-or-union specifier, and anywhere in a member declaration: among the specifiers it reaches every declarator, after a declarator only that one. Ignored silently on an anonymous member, as gcc does, and on anything that is not a member, where gcc warns |
| `transparent_union` | Unions | An argument matching **any** member's type may be passed to a parameter of this union, and the union is passed as its **first** member would be -- an unnamed zero-width bit-field is not a member for this purpose. glibc declares every socket call this way. Calls only: assignment and `return` stay strict, as in gcc. Ignored with a warning anywhere but a union |
| `mode(M)` | Types, declarators | Replaces the declared type with the one of that width in the same family, keeping signedness and qualifiers: `QI` `HI` `SI` `DI` `TI` `word` `pointer`, `HF` `SF` `DF` `XF` `TF`, `HC` `SC` `DC` `XC` `TC`. `TF`/`TC` only where `_Float128` exists. Binds to the declarator it is written on, including a member's and a parameter's. Any other mode (the vector modes) warns and leaves the type unchanged |
| `vector_size(N)` | Types | Gives the type a vector's *storage*: an array of `N / sizeof(element)` elements, aligned to `N` rounded up to a power of two and capped at 16 (gcc's default layout on both targets); an `aligned` written alongside overrides that. Element-wise arithmetic is not implemented: using a vector as a value is an error, never a silent one-element computation |
| `pure`, `const` | Functions | Recorded as a `MemEffect` on the declarator, so a prototype alone is enough. The optimizer (`ir/effects.rs`) treats a call as writing no memory the caller can see. Calls are never deleted or merged on the strength of it, so the two currently act alike |
| `noinline` | Functions | The inliner leaves the function alone, whatever its size |
| `always_inline` | Functions | Inlined at every call site regardless of size, and at `-O0` too. `noinline` outranks it, as in gcc |
| `gnu_inline` | Functions | GNU89 inline semantics for this definition: plain `inline` emits an external definition, `extern inline` does not (the reverse of C99). `-fgnu89-inline` makes it the default. glibc's `__fortify_function` relies on it |
| `constructor` | Functions | Runs before `main`, via `.init_array` (`__DATA,__mod_init_func` on Mach-O). An optional priority orders it: ELF encodes it in the section name; Mach-O has no equivalent, so it only orders the entries within one translation unit. An unprioritised entry runs after prioritised ones |
| `destructor` | Functions | Runs after `main` returns or on `exit`. ELF lists it in `.fini_array`. Mach-O does not run `__mod_term_func` for a main executable, so each destructor is rewritten (`ir/mach_o_dtors.rs`) into an `atexit` registration made from a synthesized constructor of the same priority |
| `weak` | Functions, variables | `.weak` rather than `.globl`: another definition wins, and an unresolved reference is null rather than a link error. Honoured on a *declaration* with no definition too. A weak *definition* is never inlined, since the body that runs may be some other one -- `static` is exempt, having internal linkage nothing can interpose, and `always_inline` outranks it as in gcc |
| `used` | Functions, variables | Kept even when nothing refers to it. Load-bearing for functions, since an unreferenced static function is pruned at `-O1` and above. Variables are not pruned, so it changes nothing there |
| `visibility` | Functions, variables | ELF `.hidden` / `.protected` / `.internal`; "default" is the *absence* of a directive, not a `.default` pseudo-op. Mach-O has only `.private_extern`, used for "hidden" and "internal". A zero-initialized variable leaves the `.comm` fast path rather than lose it |
| `alias("target")` | Functions, variables | The declaration becomes a second symbol for `target`, which this translation unit must define: `.set name, target`, with the alias's own binding -- `.globl`, `.weak` with `weak`, nothing when `static` -- and its own visibility. The target may itself be an alias, and a static function reached only through its alias is kept. An error, as in gcc, when the target is undefined here or only an inline definition, when one of the two is a function and the other a variable, and when the alias is also defined normally. Mach-O has no symbol aliases and the attribute is an error there, as in clang. The optimizer treats a store through either name as a store to the one object |
| `section("name")` | Functions, variables | Places the symbol in the named section, ahead of every other rule -- including the zero-initialized fast path, since `.comm` would let the linker choose. ELF flags follow the contents: `"ax"` for code, `"aw"` for mutable data, `"a"` for read-only data |

### `noreturn` spellings

```c
void exit(int status) __attribute__((noreturn));
void abort(void) __attribute__((__noreturn__));
_Noreturn void my_exit(int code);
```

The property is carried on the function type, gathered from `_Noreturn` among
the specifiers or the attribute on this or any earlier declaration of the name
(`Parser::function_declarator_attrs`). `setjmp`/`longjmp` need no attribute:
they are recognised by name and lowered to `Opcode::Setjmp` / `Opcode::Longjmp`.

## Accepted and Ignored

These are recognised: no warning, `__has_attribute` answers 1, and nothing
else happens. Each is either a pure hint or something ignoring cannot change
the result of a correct program. glibc puts most of them on every declaration.

`unused`, `deprecated`, `hot`, `cold`, `warn_unused_result`, `format`,
`nonstring`, `malloc`, `sentinel`, `nonnull`, `nonnull_if_nonzero`,
`returns_nonnull`, `nothrow`, `access`, `returns_twice`, `externally_visible`,
`abi_tag`, `weakref`, `simd`, `regparm`, `leaf`, `alloc_size`, `alloc_align`,
`noclone`, `no_instrument_function`, `copy`, `designated_init`, `may_alias`,
`artificial`, `no_sanitize_memory`, `no_sanitize_address`, `no_sanitize_thread`.

No diagnostic comes from `deprecated`, `warn_unused_result` or `format`.
`artificial` is recorded in `FunctionAttrs` but no debug annotation is emitted.

Also accepted:

- `sysv_abi`, `ms_abi`: recorded as `FunctionAttrs::calling_conv`. A function
  *definition* then classifies its own parameters, its return and the calls it
  makes with `abi::get_abi_for_conv`. `sysv_abi` is already the x86-64 default;
  `ms_abi` has no Win64 implementation and falls back to the target default. A
  call to such a function is classified by the caller's convention, not the
  callee's.
- `fallthrough`: the statement `__attribute__((fallthrough));` (gcc's
  spelling of C23 `[[fallthrough]];`) is a null statement. Diagnosed as gcc
  does, as far as the next token can tell: outside every `switch` it is an
  error ("invalid use of attribute 'fallthrough'"); followed by anything but a
  label or a brace it warns "attribute 'fallthrough' not preceding a case
  label or default label". c17 has no `-Wimplicit-fallthrough`, so it changes
  nothing else. Written on a non-null statement it is a malformed declaration,
  rejected as in gcc.

## Attribute Declarations

An attribute list standing alone before `;` -- `__attribute__((...));` --
declares nothing and is parsed by the same attribute parser as any other;
whatever it would have left pending for a declarator is discarded.

- In a block, with `fallthrough`, it is the fallthrough statement above. Any
  other recognised attribute beside it warns "'name' attribute ignored"
  (`-Wno-attributes`), as does a `fallthrough` given an argument.
- Without `fallthrough`, in a block or at file scope, it warns "empty
  declaration", which `-Wno-attributes` does not suppress (nor does gcc's).
- `fallthrough` at file scope warns "'fallthrough' attribute at top level"
  (`-Wno-attributes`).

## Not Recognised

Any other name -- including `cleanup` -- warns "'name' attribute directive
ignored", suppressible with `-Wno-attributes`, and its arguments are skipped.

## `__has_attribute`

Answers 1 exactly when the name carries the `SUPPORTED_ATTR` tag in `kw.rs`:
every attribute in the two sections above, in both spellings, plus `__const`.
Both evaluators read that one tag -- `eval_has_attribute` in
`token/preprocess.rs` (`#if`) and the `BuiltinMacro::HasAttribute` arm in
`token/preprocess_macro.rs` -- so there is no second list to keep in step.

`cleanup` answers 0.

```c
#if __has_attribute(noreturn)
#define NORETURN __attribute__((noreturn))
#else
#define NORETURN
#endif
```

## Adding an Attribute

In order:

1. **Name and tag.** Add both spellings, `(_, "name", SUPPORTED_ATTR)` and
   `(_, "__name__", SUPPORTED_ATTR)`, to the supported-attribute block of
   `define_keywords!` in `kw.rs`. This alone silences the "directive ignored"
   warning and makes `__has_attribute` answer 1. Tag a name only once the
   attribute does what it promises, or is harmless to ignore; an attribute that
   changes what a type *is* must never be accepted and ignored.

2. **Arguments.** In `parse/attribute.rs`, `AttrArgs::of` picks the grammar.
   An integer-valued attribute gets an `IntArgRole`: its range check goes in
   `Parser::check_integer_args` and its gcc-worded messages in
   `IntArgRole::report`. Anything else is `AttrArgs::General` and arrives as
   `AttributeArg::{Ident, String, Int}` in the `Attribute`.

3. **Query.** Add an `AttributeList` method that reads it through `has_attr`
   (which matches both spellings) or by scanning `attrs` with
   `name.trim_matches('_')`.

4. **Carry it.** `Parser::skip_extensions_inner` is where an attribute list
   between specifiers and declarators lands in the parser's `pending_*` slots
   (fields of `Parser` in `parse/parser.rs`). Pick the carrier by what the
   attribute describes:
   - *How a function is emitted or optimized*: a field on `ast::FunctionAttrs`,
     filled in `AttributeList::function_attrs` and combined in
     `FunctionAttrs::merge`. `Parser::accumulate_fn_attrs` merges it across
     every declaration of the name (`declared_fn_attrs`), it reaches
     `FunctionDef::attrs`, and the linearizer copies it onto `ir::Function`
     where it sets `is_noinline` and the rest (`ir/linearize.rs`).
   - *How any symbol is emitted*: a field on `ast::SymbolAttrs`, filled in
     `AttributeList::symbol_attrs` and combined in `SymbolAttrs::merge`. It is
     carried on `InitDeclarator::symbol_attrs` and `FunctionAttrs::symbol`, then
     `ir::GlobalDef::symbol_attrs` / `ir::Function::symbol_attrs`, and emitted
     by `CodeGenBase` in `arch/codegen.rs` (`emit_global`,
     `emit_declared_symbol_attrs`, `emit_symbol_aliases`).
   - *A property of the function type*, as `noreturn` is: set it on the type in
     `Parser::function_declarator_attrs` (`parse/bind.rs`).
   - *A property of a type*: a `pending_*` slot set in
     `Parser::parse_single_attribute` and applied in
     `Parser::apply_pending_type_attrs`, once the type is final. A new slot must
     also join `PendingDeclAttrs` (both `take_pending_decl_attrs` and
     `restore_pending_decl_attrs`, so a type-name inside an initializer cannot
     steal it) and be cleared in `Parser::reset_pending_declaration_state`.
   - *A struct or union attribute*: read it where `packed` is read, in
     `Parser::parse_struct_or_union_specifier` (`parse/aggregate.rs`).

   Respect the declarator scoping above: `SpecifierAttrs` snapshots the
   specifiers' share and `Parser::begin_declarator` restores it for each
   declarator. A value that must belong to one declarator only is *taken* when
   consumed, as `Parser::take_pending_fn_effect` takes the memory effect.

5. **Semantics.** Apply it in the IR pass or backend that owns the behaviour,
   on both targets and both object formats; where a format cannot express it,
   reject it as `alias` is rejected on Mach-O rather than drop it.

6. **Misuse.** An attribute in the wrong place, as gcc does, is a warning in the
   `ATTRIBUTE_WARNING` group (`-Wno-attributes`), issued only when
   `diag::warning_group_enabled(ATTRIBUTE_WARNING)` -- see
   `Parser::warn_transparent_union_ignored`. A value the attribute cannot mean
   is a `diag::error` / `diag::error_args`, and the attribute is dropped.

7. **Tests** (per `cc/CLAUDE.md`, unit and end-to-end):
   - `parse/test_parser.rs`: the parsed result (the `test_attribute_*` and
     `test_attr_*` tests are the model), including both spellings and
     declarator scoping. A new tag can join `test_tags_supported_attr` in
     `kw.rs`.
   - Unit tests in `cc/ir/` for any IR change.
   - `tests/builtins/has_feature.rs` and
     `diagnostics_recognised_attributes_are_silent` in
     `tests/diagnostics/mod.rs`: the `__has_attribute` answer and silence.
   - `tests/diagnostics/mod.rs`: every new warning and error, by message text.
   - End-to-end behaviour: `tests/codegen/asm_attributes.rs` for symbol and
     constructor attributes (assembly checks via `asm_for_with` on both
     targets), `tests/codegen/inlining.rs` for inlining, `tests/c99/types.rs`
     for type attributes, run with `compile_and_run` /
     `compile_and_run_everywhere`.

## References

- [GCC Function Attributes](https://gcc.gnu.org/onlinedocs/gcc/Function-Attributes.html)
- [GCC Common Variable Attributes](https://gcc.gnu.org/onlinedocs/gcc/Common-Variable-Attributes.html)
- [GCC Common Type Attributes](https://gcc.gnu.org/onlinedocs/gcc/Common-Type-Attributes.html)
- [C11 _Noreturn specifier](https://en.cppreference.com/w/c/language/_Noreturn)
