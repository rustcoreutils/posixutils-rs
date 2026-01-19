//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// ISO C Standard:  5.2.4.2.2  Characteristics of floating types <float.h>
//

#ifndef _PCC_FLOAT_H
#define _PCC_FLOAT_H

// Radix of exponent representation
#define FLT_RADIX __FLT_RADIX__

// Number of base-FLT_RADIX digits in the significand
#define FLT_MANT_DIG __FLT_MANT_DIG__
#define DBL_MANT_DIG __DBL_MANT_DIG__
#define LDBL_MANT_DIG __LDBL_MANT_DIG__

// Number of decimal digits for precision
#define FLT_DIG __FLT_DIG__
#define DBL_DIG __DBL_DIG__
#define LDBL_DIG __LDBL_DIG__

// Minimum negative integer such that FLT_RADIX raised to that power minus 1
// is a normalized floating-point number
#define FLT_MIN_EXP __FLT_MIN_EXP__
#define DBL_MIN_EXP __DBL_MIN_EXP__
#define LDBL_MIN_EXP __LDBL_MIN_EXP__

// Minimum negative integer such that 10 raised to that power is in the range
// of normalized floating-point numbers
#define FLT_MIN_10_EXP __FLT_MIN_10_EXP__
#define DBL_MIN_10_EXP __DBL_MIN_10_EXP__
#define LDBL_MIN_10_EXP __LDBL_MIN_10_EXP__

// Maximum integer such that FLT_RADIX raised to that power minus 1 is a
// representable finite floating-point number
#define FLT_MAX_EXP __FLT_MAX_EXP__
#define DBL_MAX_EXP __DBL_MAX_EXP__
#define LDBL_MAX_EXP __LDBL_MAX_EXP__

// Maximum integer such that 10 raised to that power is in the range of
// representable finite floating-point numbers
#define FLT_MAX_10_EXP __FLT_MAX_10_EXP__
#define DBL_MAX_10_EXP __DBL_MAX_10_EXP__
#define LDBL_MAX_10_EXP __LDBL_MAX_10_EXP__

// Maximum representable finite floating-point number
#define FLT_MAX __FLT_MAX__
#define DBL_MAX __DBL_MAX__
#define LDBL_MAX __LDBL_MAX__

// The difference between 1 and the least value greater than 1 that is
// representable in the given floating point type
#define FLT_EPSILON __FLT_EPSILON__
#define DBL_EPSILON __DBL_EPSILON__
#define LDBL_EPSILON __LDBL_EPSILON__

// Minimum normalized positive floating-point number
#define FLT_MIN __FLT_MIN__
#define DBL_MIN __DBL_MIN__
#define LDBL_MIN __LDBL_MIN__

// Addition rounds to 0: zero, 1: nearest, 2: +inf, 3: -inf, -1: unknown
#define FLT_ROUNDS 1

// C99/C11 additions
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L

// Floating-point expression evaluation method
// 0 = evaluate to the range/precision of the type
// 1 = evaluate float/double to double range/precision
// 2 = evaluate all to long double range/precision
// -1 = indeterminate
#define FLT_EVAL_METHOD 0

// Number of decimal digits needed to round-trip the widest type
#define DECIMAL_DIG __LDBL_DECIMAL_DIG__

#endif // C99

// C11 additions
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L

// Decimal digits for each type
#define FLT_DECIMAL_DIG __FLT_DECIMAL_DIG__
#define DBL_DECIMAL_DIG __DBL_DECIMAL_DIG__
#define LDBL_DECIMAL_DIG __LDBL_DECIMAL_DIG__

// Whether types support subnormal numbers
#define FLT_HAS_SUBNORM __FLT_HAS_DENORM__
#define DBL_HAS_SUBNORM __DBL_HAS_DENORM__
#define LDBL_HAS_SUBNORM __LDBL_HAS_DENORM__

// Minimum positive values, including subnormals
#define FLT_TRUE_MIN __FLT_DENORM_MIN__
#define DBL_TRUE_MIN __DBL_DENORM_MIN__
#define LDBL_TRUE_MIN __LDBL_DENORM_MIN__

#endif // C11

// _Float16 support (IEEE 754-2008 binary16)
#ifdef __FLT16_MANT_DIG__
#define FLT16_MANT_DIG __FLT16_MANT_DIG__
#define FLT16_DIG __FLT16_DIG__
#define FLT16_MIN_EXP __FLT16_MIN_EXP__
#define FLT16_MIN_10_EXP __FLT16_MIN_10_EXP__
#define FLT16_MAX_EXP __FLT16_MAX_EXP__
#define FLT16_MAX_10_EXP __FLT16_MAX_10_EXP__
#define FLT16_MAX __FLT16_MAX__
#define FLT16_EPSILON __FLT16_EPSILON__
#define FLT16_MIN __FLT16_MIN__
#define FLT16_DECIMAL_DIG __FLT16_DECIMAL_DIG__
#define FLT16_TRUE_MIN __FLT16_DENORM_MIN__
#endif

#endif // _PCC_FLOAT_H
