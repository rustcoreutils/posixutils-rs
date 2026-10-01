/*
 * plib: the environment musl's regex sources expect from the rest of musl,
 * for building them on their own against another C runtime (Windows). Not
 * part of musl; MIT, as the posixutils-rs project.
 *
 * Every vendored source reaches this header before it uses anything it
 * provides: the .c files through <regex.h> (the plib one beside this file)
 * or directly, tre.h through <regex.h>.
 */
#ifndef PLIB_MUSL_H
#define PLIB_MUSL_H

/* The C runtime's headers come first, so that their declarations keep the
   runtime's names; the renames below then apply only to the vendored code. */
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <wctype.h>

/* musl marks library-internal functions `hidden`; a static library has no
   visibility to restrict. */
#define hidden

/* musl's <limits.h> values; the Windows C runtime has neither. */
#ifndef CHARCLASS_NAME_MAX
#define CHARCLASS_NAME_MAX 14
#endif
#ifndef RE_DUP_MAX
#define RE_DUP_MAX 255
#endif

/* Every external symbol gets a plib_ name, so that none can collide with
   anything a platform library provides: these by macro, the two from
   iswctype.c (below) in its source. */
#define regcomp plib_regcomp
#define regexec plib_regexec
#define regfree plib_regfree
#define regerror plib_regerror
#define __tre_mem_new_impl plib_tre_mem_new_impl
#define __tre_mem_alloc_impl plib_tre_mem_alloc_impl
#define __tre_mem_destroy plib_tre_mem_destroy

/* Character classes by name come from musl's iswctype.c, not the runtime,
   whose wctype() maps names onto its own ctype bits rather than POSIX's
   classes: msvcrt's knows no "blank" at all, so [[:blank:]] would fail to
   compile. The per-class predicates (iswalpha, iswblank, ...) stay the
   runtime's. tre.h names these directly: a macro renaming iswctype itself
   would also capture the runtime's own isw* macros, which expand to
   iswctype(c, <runtime ctype bits>). */
int plib_iswctype(wint_t, wctype_t);
wctype_t plib_wctype(const char *);

#endif
