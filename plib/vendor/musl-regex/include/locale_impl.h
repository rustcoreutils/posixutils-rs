/*
 * plib: stands in for musl's src/internal/locale_impl.h, of which regerror.c
 * uses only LCTRANS_CUR, to translate a message through the current locale's
 * catalog. The messages are left untranslated, as they are in musl's C
 * locale.
 */
#ifndef PLIB_LOCALE_IMPL_H
#define PLIB_LOCALE_IMPL_H

#define LCTRANS_CUR(msg) (msg)

#endif
