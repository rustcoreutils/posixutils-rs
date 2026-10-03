/*
 * c17 builtin arm_neon.h - AArch64 Advanced SIMD (NEON) intrinsics
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 *
 * Written from the Arm C Language Extensions and the Arm architecture's
 * instruction pseudocode, over GNU vectors and plain C: no
 * `__builtin_aarch64_*`, and one inline-assembly statement (FSQRT, so that
 * vsqrt needs no libm). Every intrinsic is defined whatever -march is
 * given, including the dot products and vqrdmlah/vqrdmlsh.
 *
 * Intrinsics whose operands must be immediates (lane numbers, shift counts,
 * vext positions, fixed-point bit counts) are function-like macros that
 * forward to an inline function; the operand must still be in range.
 *
 * vrecpe, vrsqrte and the u32 forms compute the architecture's estimate
 * tables bit for bit (FPRecipEstimate, FPRSqrtEstimate, UnsignedRecipEstimate,
 * UnsignedRSqrtEstimate). Floating-point lane operations assume the default
 * FPCR: round to nearest, no flush to zero, no default-NaN mode.
 *
 * Not provided: float16, bfloat16, poly64/poly128 and the crypto, i8mm,
 * complex-arithmetic and scalar (vaddd_s64, vqaddb_s8, ...) intrinsics.
 */

#ifndef _ARM_NEON_H_INCLUDED
#define _ARM_NEON_H_INCLUDED

#ifndef __aarch64__
#error "arm_neon.h requires an AArch64 target"
#endif

#include <stdint.h>

/* The attributes every intrinsic carries: always inlined, and no symbol of
   its own. */
#ifndef __C17_INTRIN
#define __C17_INTRIN static __inline__ __attribute__((__always_inline__, __artificial__))
#endif

typedef float float32_t;
typedef double float64_t;
typedef uint8_t poly8_t;
typedef uint16_t poly16_t;

typedef int8_t int8x8_t __attribute__((__vector_size__(8)));
typedef int8_t int8x16_t __attribute__((__vector_size__(16)));
typedef int16_t int16x4_t __attribute__((__vector_size__(8)));
typedef int16_t int16x8_t __attribute__((__vector_size__(16)));
typedef int32_t int32x2_t __attribute__((__vector_size__(8)));
typedef int32_t int32x4_t __attribute__((__vector_size__(16)));
typedef int64_t int64x1_t __attribute__((__vector_size__(8)));
typedef int64_t int64x2_t __attribute__((__vector_size__(16)));
typedef uint8_t uint8x8_t __attribute__((__vector_size__(8)));
typedef uint8_t uint8x16_t __attribute__((__vector_size__(16)));
typedef uint16_t uint16x4_t __attribute__((__vector_size__(8)));
typedef uint16_t uint16x8_t __attribute__((__vector_size__(16)));
typedef uint32_t uint32x2_t __attribute__((__vector_size__(8)));
typedef uint32_t uint32x4_t __attribute__((__vector_size__(16)));
typedef uint64_t uint64x1_t __attribute__((__vector_size__(8)));
typedef uint64_t uint64x2_t __attribute__((__vector_size__(16)));
typedef float32_t float32x2_t __attribute__((__vector_size__(8)));
typedef float32_t float32x4_t __attribute__((__vector_size__(16)));
typedef float64_t float64x1_t __attribute__((__vector_size__(8)));
typedef float64_t float64x2_t __attribute__((__vector_size__(16)));
typedef poly8_t poly8x8_t __attribute__((__vector_size__(8)));
typedef poly8_t poly8x16_t __attribute__((__vector_size__(16)));
typedef poly16_t poly16x4_t __attribute__((__vector_size__(8)));
typedef poly16_t poly16x8_t __attribute__((__vector_size__(16)));

typedef struct int8x8x2_t { int8x8_t val[2]; } int8x8x2_t;
typedef struct int8x8x3_t { int8x8_t val[3]; } int8x8x3_t;
typedef struct int8x8x4_t { int8x8_t val[4]; } int8x8x4_t;
typedef struct int8x16x2_t { int8x16_t val[2]; } int8x16x2_t;
typedef struct int8x16x3_t { int8x16_t val[3]; } int8x16x3_t;
typedef struct int8x16x4_t { int8x16_t val[4]; } int8x16x4_t;
typedef struct int16x4x2_t { int16x4_t val[2]; } int16x4x2_t;
typedef struct int16x4x3_t { int16x4_t val[3]; } int16x4x3_t;
typedef struct int16x4x4_t { int16x4_t val[4]; } int16x4x4_t;
typedef struct int16x8x2_t { int16x8_t val[2]; } int16x8x2_t;
typedef struct int16x8x3_t { int16x8_t val[3]; } int16x8x3_t;
typedef struct int16x8x4_t { int16x8_t val[4]; } int16x8x4_t;
typedef struct int32x2x2_t { int32x2_t val[2]; } int32x2x2_t;
typedef struct int32x2x3_t { int32x2_t val[3]; } int32x2x3_t;
typedef struct int32x2x4_t { int32x2_t val[4]; } int32x2x4_t;
typedef struct int32x4x2_t { int32x4_t val[2]; } int32x4x2_t;
typedef struct int32x4x3_t { int32x4_t val[3]; } int32x4x3_t;
typedef struct int32x4x4_t { int32x4_t val[4]; } int32x4x4_t;
typedef struct int64x1x2_t { int64x1_t val[2]; } int64x1x2_t;
typedef struct int64x1x3_t { int64x1_t val[3]; } int64x1x3_t;
typedef struct int64x1x4_t { int64x1_t val[4]; } int64x1x4_t;
typedef struct int64x2x2_t { int64x2_t val[2]; } int64x2x2_t;
typedef struct int64x2x3_t { int64x2_t val[3]; } int64x2x3_t;
typedef struct int64x2x4_t { int64x2_t val[4]; } int64x2x4_t;
typedef struct uint8x8x2_t { uint8x8_t val[2]; } uint8x8x2_t;
typedef struct uint8x8x3_t { uint8x8_t val[3]; } uint8x8x3_t;
typedef struct uint8x8x4_t { uint8x8_t val[4]; } uint8x8x4_t;
typedef struct uint8x16x2_t { uint8x16_t val[2]; } uint8x16x2_t;
typedef struct uint8x16x3_t { uint8x16_t val[3]; } uint8x16x3_t;
typedef struct uint8x16x4_t { uint8x16_t val[4]; } uint8x16x4_t;
typedef struct uint16x4x2_t { uint16x4_t val[2]; } uint16x4x2_t;
typedef struct uint16x4x3_t { uint16x4_t val[3]; } uint16x4x3_t;
typedef struct uint16x4x4_t { uint16x4_t val[4]; } uint16x4x4_t;
typedef struct uint16x8x2_t { uint16x8_t val[2]; } uint16x8x2_t;
typedef struct uint16x8x3_t { uint16x8_t val[3]; } uint16x8x3_t;
typedef struct uint16x8x4_t { uint16x8_t val[4]; } uint16x8x4_t;
typedef struct uint32x2x2_t { uint32x2_t val[2]; } uint32x2x2_t;
typedef struct uint32x2x3_t { uint32x2_t val[3]; } uint32x2x3_t;
typedef struct uint32x2x4_t { uint32x2_t val[4]; } uint32x2x4_t;
typedef struct uint32x4x2_t { uint32x4_t val[2]; } uint32x4x2_t;
typedef struct uint32x4x3_t { uint32x4_t val[3]; } uint32x4x3_t;
typedef struct uint32x4x4_t { uint32x4_t val[4]; } uint32x4x4_t;
typedef struct uint64x1x2_t { uint64x1_t val[2]; } uint64x1x2_t;
typedef struct uint64x1x3_t { uint64x1_t val[3]; } uint64x1x3_t;
typedef struct uint64x1x4_t { uint64x1_t val[4]; } uint64x1x4_t;
typedef struct uint64x2x2_t { uint64x2_t val[2]; } uint64x2x2_t;
typedef struct uint64x2x3_t { uint64x2_t val[3]; } uint64x2x3_t;
typedef struct uint64x2x4_t { uint64x2_t val[4]; } uint64x2x4_t;
typedef struct float32x2x2_t { float32x2_t val[2]; } float32x2x2_t;
typedef struct float32x2x3_t { float32x2_t val[3]; } float32x2x3_t;
typedef struct float32x2x4_t { float32x2_t val[4]; } float32x2x4_t;
typedef struct float32x4x2_t { float32x4_t val[2]; } float32x4x2_t;
typedef struct float32x4x3_t { float32x4_t val[3]; } float32x4x3_t;
typedef struct float32x4x4_t { float32x4_t val[4]; } float32x4x4_t;
typedef struct float64x1x2_t { float64x1_t val[2]; } float64x1x2_t;
typedef struct float64x1x3_t { float64x1_t val[3]; } float64x1x3_t;
typedef struct float64x1x4_t { float64x1_t val[4]; } float64x1x4_t;
typedef struct float64x2x2_t { float64x2_t val[2]; } float64x2x2_t;
typedef struct float64x2x3_t { float64x2_t val[3]; } float64x2x3_t;
typedef struct float64x2x4_t { float64x2_t val[4]; } float64x2x4_t;
typedef struct poly8x8x2_t { poly8x8_t val[2]; } poly8x8x2_t;
typedef struct poly8x8x3_t { poly8x8_t val[3]; } poly8x8x3_t;
typedef struct poly8x8x4_t { poly8x8_t val[4]; } poly8x8x4_t;
typedef struct poly8x16x2_t { poly8x16_t val[2]; } poly8x16x2_t;
typedef struct poly8x16x3_t { poly8x16_t val[3]; } poly8x16x3_t;
typedef struct poly8x16x4_t { poly8x16_t val[4]; } poly8x16x4_t;
typedef struct poly16x4x2_t { poly16x4_t val[2]; } poly16x4x2_t;
typedef struct poly16x4x3_t { poly16x4_t val[3]; } poly16x4x3_t;
typedef struct poly16x4x4_t { poly16x4_t val[4]; } poly16x4x4_t;
typedef struct poly16x8x2_t { poly16x8_t val[2]; } poly16x8x2_t;
typedef struct poly16x8x3_t { poly16x8_t val[3]; } poly16x8x3_t;
typedef struct poly16x8x4_t { poly16x8_t val[4]; } poly16x8x4_t;

/* Internal helpers, named for the architecture operation they model. */

/* Clamp to the signed or unsigned range of a __w-bit lane (__w <= 64). */
__C17_INTRIN int64_t __c17_sat_s(__int128 __v, int __w)
{
  __int128 __hi = ((__int128)1 << (__w - 1)) - 1;
  if (__v > __hi)
    return (int64_t)__hi;
  if (__v < -__hi - 1)
    return (int64_t)(-__hi - 1);
  return (int64_t)__v;
}

__C17_INTRIN uint64_t __c17_sat_u(__int128 __v, int __w)
{
  __int128 __hi = ((__int128)1 << __w) - 1;
  if (__v > __hi)
    return (uint64_t)__hi;
  if (__v < 0)
    return 0;
  return (uint64_t)__v;
}

/* The low __w bits of __v, sign-extended. */
__C17_INTRIN int64_t __c17_wrap_s(uint64_t __v, int __w)
{
  return (int64_t)(__v << (64 - __w)) >> (64 - __w);
}

/* Shift a signed __w-bit lane left by __s, or right by -__s, as SSHL does;
   __rnd rounds a right shift (SRSHL) and __sat saturates a left shift
   (SQSHL, SQRSHL). */
__C17_INTRIN int64_t __c17_shl_s(int64_t __a, int __w, int __s, int __rnd, int __sat)
{
  int __n;
  if (__s >= 0) {
    __int128 __v;
    if (__s >= __w) {
      if (!__sat || __a == 0)
        return 0;
      return __c17_sat_s(__a < 0 ? -((__int128)1 << 64) : ((__int128)1 << 64), __w);
    }
    __v = (__int128)__a * ((__int128)1 << __s);
    if (__sat)
      return __c17_sat_s(__v, __w);
    return __c17_wrap_s((uint64_t)__v, __w);
  }
  __n = -__s;
  if (__rnd) {
    if (__n > __w)
      return 0;
    return (int64_t)(((__int128)__a + ((__int128)1 << (__n - 1))) >> __n);
  }
  if (__n >= __w)
    return __a < 0 ? -1 : 0;
  return __a >> __n;
}

/* The unsigned counterpart: USHL, URSHL, UQSHL, UQRSHL. */
__C17_INTRIN uint64_t __c17_shl_u(uint64_t __a, int __w, int __s, int __rnd, int __sat)
{
  uint64_t __max = __w == 64 ? ~(uint64_t)0 : ((uint64_t)1 << __w) - 1;
  int __n;
  if (__s >= 0) {
    unsigned __int128 __v;
    if (__s >= __w)
      return (__sat && __a) ? __max : 0;
    __v = (unsigned __int128)__a << __s;
    if (__sat && __v > __max)
      return __max;
    return (uint64_t)__v & __max;
  }
  __n = -__s;
  if (__rnd) {
    if (__n > __w)
      return 0;
    return (uint64_t)(((unsigned __int128)__a + ((unsigned __int128)1 << (__n - 1))) >> __n);
  }
  if (__n >= __w)
    return 0;
  return __a >> __n;
}

/* Carry-less (polynomial) product of two bytes. */
__C17_INTRIN uint16_t __c17_pmul8(uint8_t __a, uint8_t __b)
{
  uint16_t __r = 0;
  for (int __i = 0; __i < 8; __i++)
    if ((__b >> __i) & 1)
      __r ^= (uint16_t)(__a << __i);
  return __r;
}

__C17_INTRIN uint8_t __c17_rbit8(uint8_t __a)
{
  uint8_t __r = 0;
  for (int __i = 0; __i < 8; __i++)
    if ((__a >> __i) & 1)
      __r |= (uint8_t)(0x80 >> __i);
  return __r;
}

/* Leading zeros of a __w-bit value. */
__C17_INTRIN int __c17_clz(uint64_t __x, int __w)
{
  return __x ? __builtin_clzll(__x) - (64 - __w) : __w;
}

/* RecipEstimate: __a in [256, 512) is a fraction in [0.5, 1.0); the result,
   in [256, 512), approximates its reciprocal in [1.0, 2.0). */
__C17_INTRIN int __c17_recip_est(int __a)
{
  int __b;
  __a = __a * 2 + 1;
  __b = (1 << 19) / __a;
  return (__b + 1) / 2;
}

/* RecipSqrtEstimate: __a in [128, 512) is a fraction in [0.25, 1.0). */
__C17_INTRIN int __c17_rsqrt_est(int __a)
{
  int64_t __b = 512;
  if (__a < 256) {
    __a = __a * 2 + 1;
  } else {
    __a = (__a >> 1) << 1;
    __a = (__a + 1) * 2;
  }
  while (__a * (__b + 1) * (__b + 1) < ((int64_t)1 << 28))
    __b++;
  return (int)((__b + 1) / 2);
}

/* URECPE and URSQRTE on one lane. */
__C17_INTRIN uint32_t __c17_urecpe(uint32_t __a)
{
  if (!(__a >> 31))
    return 0xffffffffu;
  return (uint32_t)__c17_recip_est((int)(__a >> 23)) << 23;
}

__C17_INTRIN uint32_t __c17_ursqrte(uint32_t __a)
{
  if (!(__a >> 30))
    return 0xffffffffu;
  return (uint32_t)__c17_rsqrt_est((int)(__a >> 23)) << 23;
}

/* float lanes. */
__C17_INTRIN uint32_t __c17_bits_f32(float __x)
{
  union { float __f; uint32_t __u; } __v;
  __v.__f = __x;
  return __v.__u;
}

__C17_INTRIN float __c17_from_bits_f32(uint32_t __x)
{
  union { float __f; uint32_t __u; } __v;
  __v.__u = __x;
  return __v.__f;
}

/* FABS and FNEG touch only the sign bit, NaNs included. */
__C17_INTRIN float __c17_fabs_f32(float __x)
{
  return __c17_from_bits_f32(__c17_bits_f32(__x) & ~0x80000000u);
}

__C17_INTRIN float __c17_fneg_f32(float __x)
{
  return __c17_from_bits_f32(__c17_bits_f32(__x) ^ 0x80000000u);
}

__C17_INTRIN int __c17_isqnan_f32(float __x)
{
  return (__c17_bits_f32(__x) & 0x7fc00000u) == 0x7fc00000u;
}

__C17_INTRIN int __c17_isinf_f32(float __x)
{
  return (__c17_bits_f32(__x) & ~0x80000000u) == 0x7f800000u;
}

/* An unfused multiply, kept apart from the addition of vmla/vmls. */
__C17_INTRIN float __c17_fmul_f32(float __x, float __y)
{
  return __x * __y;
}

__C17_INTRIN float __c17_fma_f32(float __x, float __y, float __z)
{
  return __builtin_fmaf(__x, __y, __z);
}

/* FMAX/FMIN: a NaN operand propagates (an addition selects and quiets it
   exactly as FMAX does), and +0 is above -0. */
__C17_INTRIN float __c17_fmax_f32(float __a, float __b)
{
  if (__a != __a || __b != __b)
    return __a + __b;
  if (__a == __b)
    return __c17_from_bits_f32(__c17_bits_f32(__a) & __c17_bits_f32(__b));
  return __a > __b ? __a : __b;
}

__C17_INTRIN float __c17_fmin_f32(float __a, float __b)
{
  if (__a != __a || __b != __b)
    return __a + __b;
  if (__a == __b)
    return __c17_from_bits_f32(__c17_bits_f32(__a) | __c17_bits_f32(__b));
  return __a < __b ? __a : __b;
}

/* FMAXNM/FMINNM: one quiet NaN against a number gives the number. */
__C17_INTRIN float __c17_fmaxnm_f32(float __a, float __b)
{
  if (__c17_isqnan_f32(__a) && __b == __b)
    return __b;
  if (__c17_isqnan_f32(__b) && __a == __a)
    return __a;
  return __c17_fmax_f32(__a, __b);
}

__C17_INTRIN float __c17_fminnm_f32(float __a, float __b)
{
  if (__c17_isqnan_f32(__a) && __b == __b)
    return __b;
  if (__c17_isqnan_f32(__b) && __a == __a)
    return __a;
  return __c17_fmin_f32(__a, __b);
}

/* FMULX: 0 * Inf is 2 with the product's sign. */
__C17_INTRIN float __c17_fmulx_f32(float __a, float __b)
{
  if ((__c17_isinf_f32(__a) && __b == 0) || (__a == 0 && __c17_isinf_f32(__b)))
    return __c17_from_bits_f32(((__c17_bits_f32(__a) ^ __c17_bits_f32(__b)) & 0x80000000u) | __c17_bits_f32(2));
  return __a * __b;
}

/* FSQRT, as the instruction: no libm call, no errno. */
__C17_INTRIN float __c17_fsqrt_f32(float __x)
{
  float __r;
  __asm__("fsqrt %s0, %s1" : "=w"(__r) : "w"(__x));
  return __r;
}

/* The FRINT family. FRINTN rounds ties to even whatever the FPCR says. */
__C17_INTRIN float __c17_rndz_f32(float __x) { return __builtin_truncf(__x); }
__C17_INTRIN float __c17_rnda_f32(float __x) { return __builtin_roundf(__x); }
__C17_INTRIN float __c17_rndm_f32(float __x) { return __builtin_floorf(__x); }
__C17_INTRIN float __c17_rndp_f32(float __x) { return __builtin_ceilf(__x); }
__C17_INTRIN float __c17_rndx_f32(float __x) { return __builtin_rintf(__x); }
__C17_INTRIN float __c17_rndi_f32(float __x) { return __builtin_nearbyintf(__x); }

__C17_INTRIN float __c17_rndn_f32(float __x)
{
  float __r = __builtin_roundf(__x);
  if (__c17_fabs_f32(__r - __x) == (float)0.5)
    __r = 2 * __builtin_roundf(__x * (float)0.5);
  return __r;
}

/* FRECPE: FPRecipEstimate, bit for bit. */
__C17_INTRIN float __c17_frecpe_f32(float __x)
{
  uint32_t __u = __c17_bits_f32(__x);
  uint32_t __sign = __u & 0x80000000u;
  int __exp = (int)((__u >> 23) & 0xff);
  uint64_t __frac = (uint64_t)(__u & 0x7fffffu) << (52 - 23);
  int __est, __rexp;
  if (__x != __x)
    return __x + __x;
  if (__exp == 0xff)
    return __c17_from_bits_f32(__sign);
  if ((__u & ~0x80000000u) < 0x00200000u)
    return __c17_from_bits_f32(__sign | 0x7f800000u);
  if (__exp == 0) {
    if (!((__frac >> 51) & 1)) {
      __exp = -1;
      __frac = (__frac << 2) & 0xfffffffffffffULL;
    } else {
      __frac = (__frac << 1) & 0xfffffffffffffULL;
    }
  }
  __est = __c17_recip_est((int)(0x100 | ((__frac >> 44) & 0xff)));
  __rexp = 253 - __exp;
  __frac = (uint64_t)(__est & 0xff) << 44;
  if (__rexp == 0) {
    __frac = ((uint64_t)1 << 51) | (__frac >> 1);
  } else if (__rexp == -1) {
    __frac = ((uint64_t)1 << 50) | (__frac >> 2);
    __rexp = 0;
  }
  return __c17_from_bits_f32(__sign | ((uint32_t)__rexp << 23) | (uint32_t)(__frac >> (52 - 23)));
}

/* FRSQRTE: FPRSqrtEstimate, bit for bit. */
__C17_INTRIN float __c17_frsqrte_f32(float __x)
{
  uint32_t __u = __c17_bits_f32(__x);
  int __exp = (int)((__u >> 23) & 0xff);
  uint64_t __frac = (uint64_t)(__u & 0x7fffffu) << (52 - 23);
  int __scaled, __est, __rexp;
  if (__x != __x)
    return __x + __x;
  if ((__u & ~0x80000000u) == 0)
    return __c17_from_bits_f32((__u & 0x80000000u) | 0x7f800000u);
  if (__u & 0x80000000u)
    return __c17_from_bits_f32(0x7fc00000u);
  if (__exp == 0xff)
    return 0;
  if (__exp == 0) {
    while (!((__frac >> 51) & 1)) {
      __frac = (__frac << 1) & 0xfffffffffffffULL;
      __exp--;
    }
    __frac = (__frac << 1) & 0xfffffffffffffULL;
  }
  if ((__exp & 1) == 0)
    __scaled = (int)(0x100 | ((__frac >> 44) & 0xff));
  else
    __scaled = (int)(0x80 | ((__frac >> 45) & 0x7f));
  __rexp = (380 - __exp) / 2;
  __est = __c17_rsqrt_est(__scaled);
  return __c17_from_bits_f32(((uint32_t)__rexp << 23) | ((uint32_t)(__est & 0xff) << (23 - 8)));
}

/* FRECPS: 2 - a*b, fused; 0 * Inf gives 2. */
__C17_INTRIN float __c17_frecps_f32(float __a, float __b)
{
  if ((__c17_isinf_f32(__a) && __b == 0) || (__a == 0 && __c17_isinf_f32(__b)))
    return 2;
  return __c17_fma_f32(__c17_fneg_f32(__a), __b, 2);
}

/* FRSQRTS: (3 - a*b) / 2, fused and rounded once; 0 * Inf gives 1.5. The
   halving is folded into the larger operand, where it is exact, so that a
   product beyond the format's range still gives the finite result. */
__C17_INTRIN float __c17_frsqrts_f32(float __a, float __b)
{
  float __aa = __c17_fabs_f32(__a), __ab = __c17_fabs_f32(__b);
  if ((__c17_isinf_f32(__a) && __b == 0) || (__a == 0 && __c17_isinf_f32(__b)))
    return (float)1.5;
  if (__a != __a || __b != __b)
    return __c17_fma_f32(__c17_fneg_f32(__a), __b, 3);
  if (__aa < 0x1p-100f && __ab < 0x1p-100f)
    return __c17_fma_f32(__c17_fneg_f32(__a), __b, 3) * (float)0.5;
  if (__aa >= __ab)
    return __c17_fma_f32(__c17_fneg_f32(__a * (float)0.5), __b, (float)1.5);
  return __c17_fma_f32(__c17_fneg_f32(__a), __b * (float)0.5, (float)1.5);
}

/* double lanes. */
__C17_INTRIN uint64_t __c17_bits_f64(double __x)
{
  union { double __f; uint64_t __u; } __v;
  __v.__f = __x;
  return __v.__u;
}

__C17_INTRIN double __c17_from_bits_f64(uint64_t __x)
{
  union { double __f; uint64_t __u; } __v;
  __v.__u = __x;
  return __v.__f;
}

/* FABS and FNEG touch only the sign bit, NaNs included. */
__C17_INTRIN double __c17_fabs_f64(double __x)
{
  return __c17_from_bits_f64(__c17_bits_f64(__x) & ~0x8000000000000000ULL);
}

__C17_INTRIN double __c17_fneg_f64(double __x)
{
  return __c17_from_bits_f64(__c17_bits_f64(__x) ^ 0x8000000000000000ULL);
}

__C17_INTRIN int __c17_isqnan_f64(double __x)
{
  return (__c17_bits_f64(__x) & 0x7ff8000000000000ULL) == 0x7ff8000000000000ULL;
}

__C17_INTRIN int __c17_isinf_f64(double __x)
{
  return (__c17_bits_f64(__x) & ~0x8000000000000000ULL) == 0x7ff0000000000000ULL;
}

/* An unfused multiply, kept apart from the addition of vmla/vmls. */
__C17_INTRIN double __c17_fmul_f64(double __x, double __y)
{
  return __x * __y;
}

__C17_INTRIN double __c17_fma_f64(double __x, double __y, double __z)
{
  return __builtin_fma(__x, __y, __z);
}

/* FMAX/FMIN: a NaN operand propagates (an addition selects and quiets it
   exactly as FMAX does), and +0 is above -0. */
__C17_INTRIN double __c17_fmax_f64(double __a, double __b)
{
  if (__a != __a || __b != __b)
    return __a + __b;
  if (__a == __b)
    return __c17_from_bits_f64(__c17_bits_f64(__a) & __c17_bits_f64(__b));
  return __a > __b ? __a : __b;
}

__C17_INTRIN double __c17_fmin_f64(double __a, double __b)
{
  if (__a != __a || __b != __b)
    return __a + __b;
  if (__a == __b)
    return __c17_from_bits_f64(__c17_bits_f64(__a) | __c17_bits_f64(__b));
  return __a < __b ? __a : __b;
}

/* FMAXNM/FMINNM: one quiet NaN against a number gives the number. */
__C17_INTRIN double __c17_fmaxnm_f64(double __a, double __b)
{
  if (__c17_isqnan_f64(__a) && __b == __b)
    return __b;
  if (__c17_isqnan_f64(__b) && __a == __a)
    return __a;
  return __c17_fmax_f64(__a, __b);
}

__C17_INTRIN double __c17_fminnm_f64(double __a, double __b)
{
  if (__c17_isqnan_f64(__a) && __b == __b)
    return __b;
  if (__c17_isqnan_f64(__b) && __a == __a)
    return __a;
  return __c17_fmin_f64(__a, __b);
}

/* FMULX: 0 * Inf is 2 with the product's sign. */
__C17_INTRIN double __c17_fmulx_f64(double __a, double __b)
{
  if ((__c17_isinf_f64(__a) && __b == 0) || (__a == 0 && __c17_isinf_f64(__b)))
    return __c17_from_bits_f64(((__c17_bits_f64(__a) ^ __c17_bits_f64(__b)) & 0x8000000000000000ULL) | __c17_bits_f64(2));
  return __a * __b;
}

/* FSQRT, as the instruction: no libm call, no errno. */
__C17_INTRIN double __c17_fsqrt_f64(double __x)
{
  double __r;
  __asm__("fsqrt %d0, %d1" : "=w"(__r) : "w"(__x));
  return __r;
}

/* The FRINT family. FRINTN rounds ties to even whatever the FPCR says. */
__C17_INTRIN double __c17_rndz_f64(double __x) { return __builtin_trunc(__x); }
__C17_INTRIN double __c17_rnda_f64(double __x) { return __builtin_round(__x); }
__C17_INTRIN double __c17_rndm_f64(double __x) { return __builtin_floor(__x); }
__C17_INTRIN double __c17_rndp_f64(double __x) { return __builtin_ceil(__x); }
__C17_INTRIN double __c17_rndx_f64(double __x) { return __builtin_rint(__x); }
__C17_INTRIN double __c17_rndi_f64(double __x) { return __builtin_nearbyint(__x); }

__C17_INTRIN double __c17_rndn_f64(double __x)
{
  double __r = __builtin_round(__x);
  if (__c17_fabs_f64(__r - __x) == (double)0.5)
    __r = 2 * __builtin_round(__x * (double)0.5);
  return __r;
}

/* FRECPE: FPRecipEstimate, bit for bit. */
__C17_INTRIN double __c17_frecpe_f64(double __x)
{
  uint64_t __u = __c17_bits_f64(__x);
  uint64_t __sign = __u & 0x8000000000000000ULL;
  int __exp = (int)((__u >> 52) & 0x7ff);
  uint64_t __frac = (uint64_t)(__u & 0xfffffffffffffULL) << (52 - 52);
  int __est, __rexp;
  if (__x != __x)
    return __x + __x;
  if (__exp == 0x7ff)
    return __c17_from_bits_f64(__sign);
  if ((__u & ~0x8000000000000000ULL) < 0x0004000000000000ULL)
    return __c17_from_bits_f64(__sign | 0x7ff0000000000000ULL);
  if (__exp == 0) {
    if (!((__frac >> 51) & 1)) {
      __exp = -1;
      __frac = (__frac << 2) & 0xfffffffffffffULL;
    } else {
      __frac = (__frac << 1) & 0xfffffffffffffULL;
    }
  }
  __est = __c17_recip_est((int)(0x100 | ((__frac >> 44) & 0xff)));
  __rexp = 2045 - __exp;
  __frac = (uint64_t)(__est & 0xff) << 44;
  if (__rexp == 0) {
    __frac = ((uint64_t)1 << 51) | (__frac >> 1);
  } else if (__rexp == -1) {
    __frac = ((uint64_t)1 << 50) | (__frac >> 2);
    __rexp = 0;
  }
  return __c17_from_bits_f64(__sign | ((uint64_t)__rexp << 52) | (uint64_t)(__frac >> (52 - 52)));
}

/* FRSQRTE: FPRSqrtEstimate, bit for bit. */
__C17_INTRIN double __c17_frsqrte_f64(double __x)
{
  uint64_t __u = __c17_bits_f64(__x);
  int __exp = (int)((__u >> 52) & 0x7ff);
  uint64_t __frac = (uint64_t)(__u & 0xfffffffffffffULL) << (52 - 52);
  int __scaled, __est, __rexp;
  if (__x != __x)
    return __x + __x;
  if ((__u & ~0x8000000000000000ULL) == 0)
    return __c17_from_bits_f64((__u & 0x8000000000000000ULL) | 0x7ff0000000000000ULL);
  if (__u & 0x8000000000000000ULL)
    return __c17_from_bits_f64(0x7ff8000000000000ULL);
  if (__exp == 0x7ff)
    return 0;
  if (__exp == 0) {
    while (!((__frac >> 51) & 1)) {
      __frac = (__frac << 1) & 0xfffffffffffffULL;
      __exp--;
    }
    __frac = (__frac << 1) & 0xfffffffffffffULL;
  }
  if ((__exp & 1) == 0)
    __scaled = (int)(0x100 | ((__frac >> 44) & 0xff));
  else
    __scaled = (int)(0x80 | ((__frac >> 45) & 0x7f));
  __rexp = (3068 - __exp) / 2;
  __est = __c17_rsqrt_est(__scaled);
  return __c17_from_bits_f64(((uint64_t)__rexp << 52) | ((uint64_t)(__est & 0xff) << (52 - 8)));
}

/* FRECPS: 2 - a*b, fused; 0 * Inf gives 2. */
__C17_INTRIN double __c17_frecps_f64(double __a, double __b)
{
  if ((__c17_isinf_f64(__a) && __b == 0) || (__a == 0 && __c17_isinf_f64(__b)))
    return 2;
  return __c17_fma_f64(__c17_fneg_f64(__a), __b, 2);
}

/* FRSQRTS: (3 - a*b) / 2, fused and rounded once; 0 * Inf gives 1.5. The
   halving is folded into the larger operand, where it is exact, so that a
   product beyond the format's range still gives the finite result. */
__C17_INTRIN double __c17_frsqrts_f64(double __a, double __b)
{
  double __aa = __c17_fabs_f64(__a), __ab = __c17_fabs_f64(__b);
  if ((__c17_isinf_f64(__a) && __b == 0) || (__a == 0 && __c17_isinf_f64(__b)))
    return (double)1.5;
  if (__a != __a || __b != __b)
    return __c17_fma_f64(__c17_fneg_f64(__a), __b, 3);
  if (__aa < 0x1p-1000 && __ab < 0x1p-1000)
    return __c17_fma_f64(__c17_fneg_f64(__a), __b, 3) * (double)0.5;
  if (__aa >= __ab)
    return __c17_fma_f64(__c17_fneg_f64(__a * (double)0.5), __b, (double)1.5);
  return __c17_fma_f64(__c17_fneg_f64(__a), __b * (double)0.5, (double)1.5);
}


/* 2**__n, for __n in the normal exponent range. */
__C17_INTRIN float __c17_exp2_f32(int __n)
{
  return __c17_from_bits_f32((uint32_t)(127 + __n) << 23);
}

__C17_INTRIN double __c17_exp2_f64(int __n)
{
  return __c17_from_bits_f64((uint64_t)(1023 + __n) << 52);
}

/* FCVTZS/FCVTZU of an (already rounded) value to a __w-bit lane: truncate,
   saturate, and NaN gives 0. */
__C17_INTRIN int64_t __c17_cvt_s(double __x, int __w)
{
  double __lim = __c17_exp2_f64(__w - 1);
  if (__x != __x)
    return 0;
  if (__x >= __lim)
    return (int64_t)(((uint64_t)1 << (__w - 1)) - 1);
  if (__x < -__lim)
    return -(int64_t)(((uint64_t)1 << (__w - 1)) - 1) - 1;
  return (int64_t)__x;
}

__C17_INTRIN uint64_t __c17_cvt_u(double __x, int __w)
{
  if (!(__x > 0))
    return 0;
  if (__x >= __c17_exp2_f64(__w))
    return __w == 64 ? ~(uint64_t)0 : ((uint64_t)1 << __w) - 1;
  return (uint64_t)__x;
}

/* FCVTXN: narrow with round-to-odd. */
__C17_INTRIN float __c17_cvtx_f32(double __x)
{
  float __f = (float)__x;
  uint32_t __u;
  if (__x != __x || (double)__f == __x)
    return __f;
  __u = __c17_bits_f32(__f);
  if (__c17_fabs_f64((double)__f) > __c17_fabs_f64(__x))
    __u--;
  return __c17_from_bits_f32(__u | 1);
}

/* Loads and stores. */

__C17_INTRIN int8x8_t vld1_s8(const int8_t *__p)
{
  int8x8_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s8(int8_t *__p, int8x8_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int8x8_t vld1_dup_s8(const int8_t *__p)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN int8x8_t __c17_vld1_lane_s8(const int8_t *__p, int8x8_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1_lane_s8(...) __c17_vld1_lane_s8(__VA_ARGS__)

__C17_INTRIN void __c17_vst1_lane_s8(int8_t *__p, int8x8_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1_lane_s8(...) __c17_vst1_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x8x2_t vld1_s8_x2(const int8_t *__p)
{
  int8x8x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s8_x2(int8_t *__p, int8x8x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int8x8x2_t vld2_s8(const int8_t *__p)
{
  int8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2_s8(int8_t *__p, int8x8x2_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN int8x8x2_t vld2_dup_s8(const int8_t *__p)
{
  int8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN int8x8x2_t __c17_vld2_lane_s8(const int8_t *__p, int8x8x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2_lane_s8(...) __c17_vld2_lane_s8(__VA_ARGS__)

__C17_INTRIN void __c17_vst2_lane_s8(int8_t *__p, int8x8x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2_lane_s8(...) __c17_vst2_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x8x3_t vld1_s8_x3(const int8_t *__p)
{
  int8x8x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s8_x3(int8_t *__p, int8x8x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int8x8x3_t vld3_s8(const int8_t *__p)
{
  int8x8x3_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3_s8(int8_t *__p, int8x8x3_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN int8x8x3_t vld3_dup_s8(const int8_t *__p)
{
  int8x8x3_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN int8x8x3_t __c17_vld3_lane_s8(const int8_t *__p, int8x8x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3_lane_s8(...) __c17_vld3_lane_s8(__VA_ARGS__)

__C17_INTRIN void __c17_vst3_lane_s8(int8_t *__p, int8x8x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3_lane_s8(...) __c17_vst3_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x8x4_t vld1_s8_x4(const int8_t *__p)
{
  int8x8x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s8_x4(int8_t *__p, int8x8x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int8x8x4_t vld4_s8(const int8_t *__p)
{
  int8x8x4_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4_s8(int8_t *__p, int8x8x4_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN int8x8x4_t vld4_dup_s8(const int8_t *__p)
{
  int8x8x4_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN int8x8x4_t __c17_vld4_lane_s8(const int8_t *__p, int8x8x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4_lane_s8(...) __c17_vld4_lane_s8(__VA_ARGS__)

__C17_INTRIN void __c17_vst4_lane_s8(int8_t *__p, int8x8x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4_lane_s8(...) __c17_vst4_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t vld1q_s8(const int8_t *__p)
{
  int8x16_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s8(int8_t *__p, int8x16_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int8x16_t vld1q_dup_s8(const int8_t *__p)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN int8x16_t __c17_vld1q_lane_s8(const int8_t *__p, int8x16_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1q_lane_s8(...) __c17_vld1q_lane_s8(__VA_ARGS__)

__C17_INTRIN void __c17_vst1q_lane_s8(int8_t *__p, int8x16_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1q_lane_s8(...) __c17_vst1q_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x16x2_t vld1q_s8_x2(const int8_t *__p)
{
  int8x16x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s8_x2(int8_t *__p, int8x16x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int8x16x2_t vld2q_s8(const int8_t *__p)
{
  int8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2q_s8(int8_t *__p, int8x16x2_t __a)
{
  for (int __i = 0; __i < 16; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN int8x16x2_t vld2q_dup_s8(const int8_t *__p)
{
  int8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN int8x16x2_t __c17_vld2q_lane_s8(const int8_t *__p, int8x16x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2q_lane_s8(...) __c17_vld2q_lane_s8(__VA_ARGS__)

__C17_INTRIN void __c17_vst2q_lane_s8(int8_t *__p, int8x16x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2q_lane_s8(...) __c17_vst2q_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x16x3_t vld1q_s8_x3(const int8_t *__p)
{
  int8x16x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s8_x3(int8_t *__p, int8x16x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int8x16x3_t vld3q_s8(const int8_t *__p)
{
  int8x16x3_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3q_s8(int8_t *__p, int8x16x3_t __a)
{
  for (int __i = 0; __i < 16; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN int8x16x3_t vld3q_dup_s8(const int8_t *__p)
{
  int8x16x3_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN int8x16x3_t __c17_vld3q_lane_s8(const int8_t *__p, int8x16x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3q_lane_s8(...) __c17_vld3q_lane_s8(__VA_ARGS__)

__C17_INTRIN void __c17_vst3q_lane_s8(int8_t *__p, int8x16x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3q_lane_s8(...) __c17_vst3q_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x16x4_t vld1q_s8_x4(const int8_t *__p)
{
  int8x16x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s8_x4(int8_t *__p, int8x16x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int8x16x4_t vld4q_s8(const int8_t *__p)
{
  int8x16x4_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4q_s8(int8_t *__p, int8x16x4_t __a)
{
  for (int __i = 0; __i < 16; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN int8x16x4_t vld4q_dup_s8(const int8_t *__p)
{
  int8x16x4_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN int8x16x4_t __c17_vld4q_lane_s8(const int8_t *__p, int8x16x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4q_lane_s8(...) __c17_vld4q_lane_s8(__VA_ARGS__)

__C17_INTRIN void __c17_vst4q_lane_s8(int8_t *__p, int8x16x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4q_lane_s8(...) __c17_vst4q_lane_s8(__VA_ARGS__)

__C17_INTRIN int16x4_t vld1_s16(const int16_t *__p)
{
  int16x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s16(int16_t *__p, int16x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int16x4_t vld1_dup_s16(const int16_t *__p)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN int16x4_t __c17_vld1_lane_s16(const int16_t *__p, int16x4_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1_lane_s16(...) __c17_vld1_lane_s16(__VA_ARGS__)

__C17_INTRIN void __c17_vst1_lane_s16(int16_t *__p, int16x4_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1_lane_s16(...) __c17_vst1_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4x2_t vld1_s16_x2(const int16_t *__p)
{
  int16x4x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s16_x2(int16_t *__p, int16x4x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int16x4x2_t vld2_s16(const int16_t *__p)
{
  int16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2_s16(int16_t *__p, int16x4x2_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN int16x4x2_t vld2_dup_s16(const int16_t *__p)
{
  int16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN int16x4x2_t __c17_vld2_lane_s16(const int16_t *__p, int16x4x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2_lane_s16(...) __c17_vld2_lane_s16(__VA_ARGS__)

__C17_INTRIN void __c17_vst2_lane_s16(int16_t *__p, int16x4x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2_lane_s16(...) __c17_vst2_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4x3_t vld1_s16_x3(const int16_t *__p)
{
  int16x4x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s16_x3(int16_t *__p, int16x4x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int16x4x3_t vld3_s16(const int16_t *__p)
{
  int16x4x3_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3_s16(int16_t *__p, int16x4x3_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN int16x4x3_t vld3_dup_s16(const int16_t *__p)
{
  int16x4x3_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN int16x4x3_t __c17_vld3_lane_s16(const int16_t *__p, int16x4x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3_lane_s16(...) __c17_vld3_lane_s16(__VA_ARGS__)

__C17_INTRIN void __c17_vst3_lane_s16(int16_t *__p, int16x4x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3_lane_s16(...) __c17_vst3_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4x4_t vld1_s16_x4(const int16_t *__p)
{
  int16x4x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s16_x4(int16_t *__p, int16x4x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int16x4x4_t vld4_s16(const int16_t *__p)
{
  int16x4x4_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4_s16(int16_t *__p, int16x4x4_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN int16x4x4_t vld4_dup_s16(const int16_t *__p)
{
  int16x4x4_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN int16x4x4_t __c17_vld4_lane_s16(const int16_t *__p, int16x4x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4_lane_s16(...) __c17_vld4_lane_s16(__VA_ARGS__)

__C17_INTRIN void __c17_vst4_lane_s16(int16_t *__p, int16x4x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4_lane_s16(...) __c17_vst4_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t vld1q_s16(const int16_t *__p)
{
  int16x8_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s16(int16_t *__p, int16x8_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int16x8_t vld1q_dup_s16(const int16_t *__p)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN int16x8_t __c17_vld1q_lane_s16(const int16_t *__p, int16x8_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1q_lane_s16(...) __c17_vld1q_lane_s16(__VA_ARGS__)

__C17_INTRIN void __c17_vst1q_lane_s16(int16_t *__p, int16x8_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1q_lane_s16(...) __c17_vst1q_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8x2_t vld1q_s16_x2(const int16_t *__p)
{
  int16x8x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s16_x2(int16_t *__p, int16x8x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int16x8x2_t vld2q_s16(const int16_t *__p)
{
  int16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2q_s16(int16_t *__p, int16x8x2_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN int16x8x2_t vld2q_dup_s16(const int16_t *__p)
{
  int16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN int16x8x2_t __c17_vld2q_lane_s16(const int16_t *__p, int16x8x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2q_lane_s16(...) __c17_vld2q_lane_s16(__VA_ARGS__)

__C17_INTRIN void __c17_vst2q_lane_s16(int16_t *__p, int16x8x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2q_lane_s16(...) __c17_vst2q_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8x3_t vld1q_s16_x3(const int16_t *__p)
{
  int16x8x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s16_x3(int16_t *__p, int16x8x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int16x8x3_t vld3q_s16(const int16_t *__p)
{
  int16x8x3_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3q_s16(int16_t *__p, int16x8x3_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN int16x8x3_t vld3q_dup_s16(const int16_t *__p)
{
  int16x8x3_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN int16x8x3_t __c17_vld3q_lane_s16(const int16_t *__p, int16x8x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3q_lane_s16(...) __c17_vld3q_lane_s16(__VA_ARGS__)

__C17_INTRIN void __c17_vst3q_lane_s16(int16_t *__p, int16x8x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3q_lane_s16(...) __c17_vst3q_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8x4_t vld1q_s16_x4(const int16_t *__p)
{
  int16x8x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s16_x4(int16_t *__p, int16x8x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int16x8x4_t vld4q_s16(const int16_t *__p)
{
  int16x8x4_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4q_s16(int16_t *__p, int16x8x4_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN int16x8x4_t vld4q_dup_s16(const int16_t *__p)
{
  int16x8x4_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN int16x8x4_t __c17_vld4q_lane_s16(const int16_t *__p, int16x8x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4q_lane_s16(...) __c17_vld4q_lane_s16(__VA_ARGS__)

__C17_INTRIN void __c17_vst4q_lane_s16(int16_t *__p, int16x8x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4q_lane_s16(...) __c17_vst4q_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x2_t vld1_s32(const int32_t *__p)
{
  int32x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s32(int32_t *__p, int32x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int32x2_t vld1_dup_s32(const int32_t *__p)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN int32x2_t __c17_vld1_lane_s32(const int32_t *__p, int32x2_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1_lane_s32(...) __c17_vld1_lane_s32(__VA_ARGS__)

__C17_INTRIN void __c17_vst1_lane_s32(int32_t *__p, int32x2_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1_lane_s32(...) __c17_vst1_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2x2_t vld1_s32_x2(const int32_t *__p)
{
  int32x2x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s32_x2(int32_t *__p, int32x2x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int32x2x2_t vld2_s32(const int32_t *__p)
{
  int32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2_s32(int32_t *__p, int32x2x2_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN int32x2x2_t vld2_dup_s32(const int32_t *__p)
{
  int32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN int32x2x2_t __c17_vld2_lane_s32(const int32_t *__p, int32x2x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2_lane_s32(...) __c17_vld2_lane_s32(__VA_ARGS__)

__C17_INTRIN void __c17_vst2_lane_s32(int32_t *__p, int32x2x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2_lane_s32(...) __c17_vst2_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2x3_t vld1_s32_x3(const int32_t *__p)
{
  int32x2x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s32_x3(int32_t *__p, int32x2x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int32x2x3_t vld3_s32(const int32_t *__p)
{
  int32x2x3_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3_s32(int32_t *__p, int32x2x3_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN int32x2x3_t vld3_dup_s32(const int32_t *__p)
{
  int32x2x3_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN int32x2x3_t __c17_vld3_lane_s32(const int32_t *__p, int32x2x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3_lane_s32(...) __c17_vld3_lane_s32(__VA_ARGS__)

__C17_INTRIN void __c17_vst3_lane_s32(int32_t *__p, int32x2x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3_lane_s32(...) __c17_vst3_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2x4_t vld1_s32_x4(const int32_t *__p)
{
  int32x2x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s32_x4(int32_t *__p, int32x2x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int32x2x4_t vld4_s32(const int32_t *__p)
{
  int32x2x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4_s32(int32_t *__p, int32x2x4_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN int32x2x4_t vld4_dup_s32(const int32_t *__p)
{
  int32x2x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN int32x2x4_t __c17_vld4_lane_s32(const int32_t *__p, int32x2x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4_lane_s32(...) __c17_vld4_lane_s32(__VA_ARGS__)

__C17_INTRIN void __c17_vst4_lane_s32(int32_t *__p, int32x2x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4_lane_s32(...) __c17_vst4_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t vld1q_s32(const int32_t *__p)
{
  int32x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s32(int32_t *__p, int32x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int32x4_t vld1q_dup_s32(const int32_t *__p)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN int32x4_t __c17_vld1q_lane_s32(const int32_t *__p, int32x4_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1q_lane_s32(...) __c17_vld1q_lane_s32(__VA_ARGS__)

__C17_INTRIN void __c17_vst1q_lane_s32(int32_t *__p, int32x4_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1q_lane_s32(...) __c17_vst1q_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4x2_t vld1q_s32_x2(const int32_t *__p)
{
  int32x4x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s32_x2(int32_t *__p, int32x4x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int32x4x2_t vld2q_s32(const int32_t *__p)
{
  int32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2q_s32(int32_t *__p, int32x4x2_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN int32x4x2_t vld2q_dup_s32(const int32_t *__p)
{
  int32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN int32x4x2_t __c17_vld2q_lane_s32(const int32_t *__p, int32x4x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2q_lane_s32(...) __c17_vld2q_lane_s32(__VA_ARGS__)

__C17_INTRIN void __c17_vst2q_lane_s32(int32_t *__p, int32x4x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2q_lane_s32(...) __c17_vst2q_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4x3_t vld1q_s32_x3(const int32_t *__p)
{
  int32x4x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s32_x3(int32_t *__p, int32x4x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int32x4x3_t vld3q_s32(const int32_t *__p)
{
  int32x4x3_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3q_s32(int32_t *__p, int32x4x3_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN int32x4x3_t vld3q_dup_s32(const int32_t *__p)
{
  int32x4x3_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN int32x4x3_t __c17_vld3q_lane_s32(const int32_t *__p, int32x4x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3q_lane_s32(...) __c17_vld3q_lane_s32(__VA_ARGS__)

__C17_INTRIN void __c17_vst3q_lane_s32(int32_t *__p, int32x4x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3q_lane_s32(...) __c17_vst3q_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4x4_t vld1q_s32_x4(const int32_t *__p)
{
  int32x4x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s32_x4(int32_t *__p, int32x4x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int32x4x4_t vld4q_s32(const int32_t *__p)
{
  int32x4x4_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4q_s32(int32_t *__p, int32x4x4_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN int32x4x4_t vld4q_dup_s32(const int32_t *__p)
{
  int32x4x4_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN int32x4x4_t __c17_vld4q_lane_s32(const int32_t *__p, int32x4x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4q_lane_s32(...) __c17_vld4q_lane_s32(__VA_ARGS__)

__C17_INTRIN void __c17_vst4q_lane_s32(int32_t *__p, int32x4x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4q_lane_s32(...) __c17_vst4q_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x1_t vld1_s64(const int64_t *__p)
{
  int64x1_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s64(int64_t *__p, int64x1_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int64x1_t vld1_dup_s64(const int64_t *__p)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN int64x1_t __c17_vld1_lane_s64(const int64_t *__p, int64x1_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1_lane_s64(...) __c17_vld1_lane_s64(__VA_ARGS__)

__C17_INTRIN void __c17_vst1_lane_s64(int64_t *__p, int64x1_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1_lane_s64(...) __c17_vst1_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x1x2_t vld1_s64_x2(const int64_t *__p)
{
  int64x1x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s64_x2(int64_t *__p, int64x1x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int64x1x2_t vld2_s64(const int64_t *__p)
{
  int64x1x2_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2_s64(int64_t *__p, int64x1x2_t __a)
{
  for (int __i = 0; __i < 1; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN int64x1x2_t vld2_dup_s64(const int64_t *__p)
{
  int64x1x2_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN int64x1x2_t __c17_vld2_lane_s64(const int64_t *__p, int64x1x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2_lane_s64(...) __c17_vld2_lane_s64(__VA_ARGS__)

__C17_INTRIN void __c17_vst2_lane_s64(int64_t *__p, int64x1x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2_lane_s64(...) __c17_vst2_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x1x3_t vld1_s64_x3(const int64_t *__p)
{
  int64x1x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s64_x3(int64_t *__p, int64x1x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int64x1x3_t vld3_s64(const int64_t *__p)
{
  int64x1x3_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3_s64(int64_t *__p, int64x1x3_t __a)
{
  for (int __i = 0; __i < 1; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN int64x1x3_t vld3_dup_s64(const int64_t *__p)
{
  int64x1x3_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN int64x1x3_t __c17_vld3_lane_s64(const int64_t *__p, int64x1x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3_lane_s64(...) __c17_vld3_lane_s64(__VA_ARGS__)

__C17_INTRIN void __c17_vst3_lane_s64(int64_t *__p, int64x1x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3_lane_s64(...) __c17_vst3_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x1x4_t vld1_s64_x4(const int64_t *__p)
{
  int64x1x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_s64_x4(int64_t *__p, int64x1x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int64x1x4_t vld4_s64(const int64_t *__p)
{
  int64x1x4_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4_s64(int64_t *__p, int64x1x4_t __a)
{
  for (int __i = 0; __i < 1; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN int64x1x4_t vld4_dup_s64(const int64_t *__p)
{
  int64x1x4_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN int64x1x4_t __c17_vld4_lane_s64(const int64_t *__p, int64x1x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4_lane_s64(...) __c17_vld4_lane_s64(__VA_ARGS__)

__C17_INTRIN void __c17_vst4_lane_s64(int64_t *__p, int64x1x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4_lane_s64(...) __c17_vst4_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t vld1q_s64(const int64_t *__p)
{
  int64x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s64(int64_t *__p, int64x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int64x2_t vld1q_dup_s64(const int64_t *__p)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN int64x2_t __c17_vld1q_lane_s64(const int64_t *__p, int64x2_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1q_lane_s64(...) __c17_vld1q_lane_s64(__VA_ARGS__)

__C17_INTRIN void __c17_vst1q_lane_s64(int64_t *__p, int64x2_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1q_lane_s64(...) __c17_vst1q_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x2x2_t vld1q_s64_x2(const int64_t *__p)
{
  int64x2x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s64_x2(int64_t *__p, int64x2x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int64x2x2_t vld2q_s64(const int64_t *__p)
{
  int64x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2q_s64(int64_t *__p, int64x2x2_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN int64x2x2_t vld2q_dup_s64(const int64_t *__p)
{
  int64x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN int64x2x2_t __c17_vld2q_lane_s64(const int64_t *__p, int64x2x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2q_lane_s64(...) __c17_vld2q_lane_s64(__VA_ARGS__)

__C17_INTRIN void __c17_vst2q_lane_s64(int64_t *__p, int64x2x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2q_lane_s64(...) __c17_vst2q_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x2x3_t vld1q_s64_x3(const int64_t *__p)
{
  int64x2x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s64_x3(int64_t *__p, int64x2x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int64x2x3_t vld3q_s64(const int64_t *__p)
{
  int64x2x3_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3q_s64(int64_t *__p, int64x2x3_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN int64x2x3_t vld3q_dup_s64(const int64_t *__p)
{
  int64x2x3_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN int64x2x3_t __c17_vld3q_lane_s64(const int64_t *__p, int64x2x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3q_lane_s64(...) __c17_vld3q_lane_s64(__VA_ARGS__)

__C17_INTRIN void __c17_vst3q_lane_s64(int64_t *__p, int64x2x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3q_lane_s64(...) __c17_vst3q_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x2x4_t vld1q_s64_x4(const int64_t *__p)
{
  int64x2x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_s64_x4(int64_t *__p, int64x2x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN int64x2x4_t vld4q_s64(const int64_t *__p)
{
  int64x2x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4q_s64(int64_t *__p, int64x2x4_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN int64x2x4_t vld4q_dup_s64(const int64_t *__p)
{
  int64x2x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN int64x2x4_t __c17_vld4q_lane_s64(const int64_t *__p, int64x2x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4q_lane_s64(...) __c17_vld4q_lane_s64(__VA_ARGS__)

__C17_INTRIN void __c17_vst4q_lane_s64(int64_t *__p, int64x2x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4q_lane_s64(...) __c17_vst4q_lane_s64(__VA_ARGS__)

__C17_INTRIN uint8x8_t vld1_u8(const uint8_t *__p)
{
  uint8x8_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u8(uint8_t *__p, uint8x8_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint8x8_t vld1_dup_u8(const uint8_t *__p)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN uint8x8_t __c17_vld1_lane_u8(const uint8_t *__p, uint8x8_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1_lane_u8(...) __c17_vld1_lane_u8(__VA_ARGS__)

__C17_INTRIN void __c17_vst1_lane_u8(uint8_t *__p, uint8x8_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1_lane_u8(...) __c17_vst1_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x8x2_t vld1_u8_x2(const uint8_t *__p)
{
  uint8x8x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u8_x2(uint8_t *__p, uint8x8x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint8x8x2_t vld2_u8(const uint8_t *__p)
{
  uint8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2_u8(uint8_t *__p, uint8x8x2_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN uint8x8x2_t vld2_dup_u8(const uint8_t *__p)
{
  uint8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN uint8x8x2_t __c17_vld2_lane_u8(const uint8_t *__p, uint8x8x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2_lane_u8(...) __c17_vld2_lane_u8(__VA_ARGS__)

__C17_INTRIN void __c17_vst2_lane_u8(uint8_t *__p, uint8x8x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2_lane_u8(...) __c17_vst2_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x8x3_t vld1_u8_x3(const uint8_t *__p)
{
  uint8x8x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u8_x3(uint8_t *__p, uint8x8x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint8x8x3_t vld3_u8(const uint8_t *__p)
{
  uint8x8x3_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3_u8(uint8_t *__p, uint8x8x3_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN uint8x8x3_t vld3_dup_u8(const uint8_t *__p)
{
  uint8x8x3_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN uint8x8x3_t __c17_vld3_lane_u8(const uint8_t *__p, uint8x8x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3_lane_u8(...) __c17_vld3_lane_u8(__VA_ARGS__)

__C17_INTRIN void __c17_vst3_lane_u8(uint8_t *__p, uint8x8x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3_lane_u8(...) __c17_vst3_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x8x4_t vld1_u8_x4(const uint8_t *__p)
{
  uint8x8x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u8_x4(uint8_t *__p, uint8x8x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint8x8x4_t vld4_u8(const uint8_t *__p)
{
  uint8x8x4_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4_u8(uint8_t *__p, uint8x8x4_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN uint8x8x4_t vld4_dup_u8(const uint8_t *__p)
{
  uint8x8x4_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN uint8x8x4_t __c17_vld4_lane_u8(const uint8_t *__p, uint8x8x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4_lane_u8(...) __c17_vld4_lane_u8(__VA_ARGS__)

__C17_INTRIN void __c17_vst4_lane_u8(uint8_t *__p, uint8x8x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4_lane_u8(...) __c17_vst4_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t vld1q_u8(const uint8_t *__p)
{
  uint8x16_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u8(uint8_t *__p, uint8x16_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint8x16_t vld1q_dup_u8(const uint8_t *__p)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN uint8x16_t __c17_vld1q_lane_u8(const uint8_t *__p, uint8x16_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1q_lane_u8(...) __c17_vld1q_lane_u8(__VA_ARGS__)

__C17_INTRIN void __c17_vst1q_lane_u8(uint8_t *__p, uint8x16_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1q_lane_u8(...) __c17_vst1q_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x16x2_t vld1q_u8_x2(const uint8_t *__p)
{
  uint8x16x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u8_x2(uint8_t *__p, uint8x16x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint8x16x2_t vld2q_u8(const uint8_t *__p)
{
  uint8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2q_u8(uint8_t *__p, uint8x16x2_t __a)
{
  for (int __i = 0; __i < 16; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN uint8x16x2_t vld2q_dup_u8(const uint8_t *__p)
{
  uint8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN uint8x16x2_t __c17_vld2q_lane_u8(const uint8_t *__p, uint8x16x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2q_lane_u8(...) __c17_vld2q_lane_u8(__VA_ARGS__)

__C17_INTRIN void __c17_vst2q_lane_u8(uint8_t *__p, uint8x16x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2q_lane_u8(...) __c17_vst2q_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x16x3_t vld1q_u8_x3(const uint8_t *__p)
{
  uint8x16x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u8_x3(uint8_t *__p, uint8x16x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint8x16x3_t vld3q_u8(const uint8_t *__p)
{
  uint8x16x3_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3q_u8(uint8_t *__p, uint8x16x3_t __a)
{
  for (int __i = 0; __i < 16; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN uint8x16x3_t vld3q_dup_u8(const uint8_t *__p)
{
  uint8x16x3_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN uint8x16x3_t __c17_vld3q_lane_u8(const uint8_t *__p, uint8x16x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3q_lane_u8(...) __c17_vld3q_lane_u8(__VA_ARGS__)

__C17_INTRIN void __c17_vst3q_lane_u8(uint8_t *__p, uint8x16x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3q_lane_u8(...) __c17_vst3q_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x16x4_t vld1q_u8_x4(const uint8_t *__p)
{
  uint8x16x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u8_x4(uint8_t *__p, uint8x16x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint8x16x4_t vld4q_u8(const uint8_t *__p)
{
  uint8x16x4_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4q_u8(uint8_t *__p, uint8x16x4_t __a)
{
  for (int __i = 0; __i < 16; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN uint8x16x4_t vld4q_dup_u8(const uint8_t *__p)
{
  uint8x16x4_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN uint8x16x4_t __c17_vld4q_lane_u8(const uint8_t *__p, uint8x16x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4q_lane_u8(...) __c17_vld4q_lane_u8(__VA_ARGS__)

__C17_INTRIN void __c17_vst4q_lane_u8(uint8_t *__p, uint8x16x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4q_lane_u8(...) __c17_vst4q_lane_u8(__VA_ARGS__)

__C17_INTRIN uint16x4_t vld1_u16(const uint16_t *__p)
{
  uint16x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u16(uint16_t *__p, uint16x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint16x4_t vld1_dup_u16(const uint16_t *__p)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN uint16x4_t __c17_vld1_lane_u16(const uint16_t *__p, uint16x4_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1_lane_u16(...) __c17_vld1_lane_u16(__VA_ARGS__)

__C17_INTRIN void __c17_vst1_lane_u16(uint16_t *__p, uint16x4_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1_lane_u16(...) __c17_vst1_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x4x2_t vld1_u16_x2(const uint16_t *__p)
{
  uint16x4x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u16_x2(uint16_t *__p, uint16x4x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint16x4x2_t vld2_u16(const uint16_t *__p)
{
  uint16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2_u16(uint16_t *__p, uint16x4x2_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN uint16x4x2_t vld2_dup_u16(const uint16_t *__p)
{
  uint16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN uint16x4x2_t __c17_vld2_lane_u16(const uint16_t *__p, uint16x4x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2_lane_u16(...) __c17_vld2_lane_u16(__VA_ARGS__)

__C17_INTRIN void __c17_vst2_lane_u16(uint16_t *__p, uint16x4x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2_lane_u16(...) __c17_vst2_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x4x3_t vld1_u16_x3(const uint16_t *__p)
{
  uint16x4x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u16_x3(uint16_t *__p, uint16x4x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint16x4x3_t vld3_u16(const uint16_t *__p)
{
  uint16x4x3_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3_u16(uint16_t *__p, uint16x4x3_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN uint16x4x3_t vld3_dup_u16(const uint16_t *__p)
{
  uint16x4x3_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN uint16x4x3_t __c17_vld3_lane_u16(const uint16_t *__p, uint16x4x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3_lane_u16(...) __c17_vld3_lane_u16(__VA_ARGS__)

__C17_INTRIN void __c17_vst3_lane_u16(uint16_t *__p, uint16x4x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3_lane_u16(...) __c17_vst3_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x4x4_t vld1_u16_x4(const uint16_t *__p)
{
  uint16x4x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u16_x4(uint16_t *__p, uint16x4x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint16x4x4_t vld4_u16(const uint16_t *__p)
{
  uint16x4x4_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4_u16(uint16_t *__p, uint16x4x4_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN uint16x4x4_t vld4_dup_u16(const uint16_t *__p)
{
  uint16x4x4_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN uint16x4x4_t __c17_vld4_lane_u16(const uint16_t *__p, uint16x4x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4_lane_u16(...) __c17_vld4_lane_u16(__VA_ARGS__)

__C17_INTRIN void __c17_vst4_lane_u16(uint16_t *__p, uint16x4x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4_lane_u16(...) __c17_vst4_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t vld1q_u16(const uint16_t *__p)
{
  uint16x8_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u16(uint16_t *__p, uint16x8_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint16x8_t vld1q_dup_u16(const uint16_t *__p)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN uint16x8_t __c17_vld1q_lane_u16(const uint16_t *__p, uint16x8_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1q_lane_u16(...) __c17_vld1q_lane_u16(__VA_ARGS__)

__C17_INTRIN void __c17_vst1q_lane_u16(uint16_t *__p, uint16x8_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1q_lane_u16(...) __c17_vst1q_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x8x2_t vld1q_u16_x2(const uint16_t *__p)
{
  uint16x8x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u16_x2(uint16_t *__p, uint16x8x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint16x8x2_t vld2q_u16(const uint16_t *__p)
{
  uint16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2q_u16(uint16_t *__p, uint16x8x2_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN uint16x8x2_t vld2q_dup_u16(const uint16_t *__p)
{
  uint16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN uint16x8x2_t __c17_vld2q_lane_u16(const uint16_t *__p, uint16x8x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2q_lane_u16(...) __c17_vld2q_lane_u16(__VA_ARGS__)

__C17_INTRIN void __c17_vst2q_lane_u16(uint16_t *__p, uint16x8x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2q_lane_u16(...) __c17_vst2q_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x8x3_t vld1q_u16_x3(const uint16_t *__p)
{
  uint16x8x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u16_x3(uint16_t *__p, uint16x8x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint16x8x3_t vld3q_u16(const uint16_t *__p)
{
  uint16x8x3_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3q_u16(uint16_t *__p, uint16x8x3_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN uint16x8x3_t vld3q_dup_u16(const uint16_t *__p)
{
  uint16x8x3_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN uint16x8x3_t __c17_vld3q_lane_u16(const uint16_t *__p, uint16x8x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3q_lane_u16(...) __c17_vld3q_lane_u16(__VA_ARGS__)

__C17_INTRIN void __c17_vst3q_lane_u16(uint16_t *__p, uint16x8x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3q_lane_u16(...) __c17_vst3q_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x8x4_t vld1q_u16_x4(const uint16_t *__p)
{
  uint16x8x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u16_x4(uint16_t *__p, uint16x8x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint16x8x4_t vld4q_u16(const uint16_t *__p)
{
  uint16x8x4_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4q_u16(uint16_t *__p, uint16x8x4_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN uint16x8x4_t vld4q_dup_u16(const uint16_t *__p)
{
  uint16x8x4_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN uint16x8x4_t __c17_vld4q_lane_u16(const uint16_t *__p, uint16x8x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4q_lane_u16(...) __c17_vld4q_lane_u16(__VA_ARGS__)

__C17_INTRIN void __c17_vst4q_lane_u16(uint16_t *__p, uint16x8x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4q_lane_u16(...) __c17_vst4q_lane_u16(__VA_ARGS__)

__C17_INTRIN uint32x2_t vld1_u32(const uint32_t *__p)
{
  uint32x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u32(uint32_t *__p, uint32x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint32x2_t vld1_dup_u32(const uint32_t *__p)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN uint32x2_t __c17_vld1_lane_u32(const uint32_t *__p, uint32x2_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1_lane_u32(...) __c17_vld1_lane_u32(__VA_ARGS__)

__C17_INTRIN void __c17_vst1_lane_u32(uint32_t *__p, uint32x2_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1_lane_u32(...) __c17_vst1_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x2x2_t vld1_u32_x2(const uint32_t *__p)
{
  uint32x2x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u32_x2(uint32_t *__p, uint32x2x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint32x2x2_t vld2_u32(const uint32_t *__p)
{
  uint32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2_u32(uint32_t *__p, uint32x2x2_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN uint32x2x2_t vld2_dup_u32(const uint32_t *__p)
{
  uint32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN uint32x2x2_t __c17_vld2_lane_u32(const uint32_t *__p, uint32x2x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2_lane_u32(...) __c17_vld2_lane_u32(__VA_ARGS__)

__C17_INTRIN void __c17_vst2_lane_u32(uint32_t *__p, uint32x2x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2_lane_u32(...) __c17_vst2_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x2x3_t vld1_u32_x3(const uint32_t *__p)
{
  uint32x2x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u32_x3(uint32_t *__p, uint32x2x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint32x2x3_t vld3_u32(const uint32_t *__p)
{
  uint32x2x3_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3_u32(uint32_t *__p, uint32x2x3_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN uint32x2x3_t vld3_dup_u32(const uint32_t *__p)
{
  uint32x2x3_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN uint32x2x3_t __c17_vld3_lane_u32(const uint32_t *__p, uint32x2x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3_lane_u32(...) __c17_vld3_lane_u32(__VA_ARGS__)

__C17_INTRIN void __c17_vst3_lane_u32(uint32_t *__p, uint32x2x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3_lane_u32(...) __c17_vst3_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x2x4_t vld1_u32_x4(const uint32_t *__p)
{
  uint32x2x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u32_x4(uint32_t *__p, uint32x2x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint32x2x4_t vld4_u32(const uint32_t *__p)
{
  uint32x2x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4_u32(uint32_t *__p, uint32x2x4_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN uint32x2x4_t vld4_dup_u32(const uint32_t *__p)
{
  uint32x2x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN uint32x2x4_t __c17_vld4_lane_u32(const uint32_t *__p, uint32x2x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4_lane_u32(...) __c17_vld4_lane_u32(__VA_ARGS__)

__C17_INTRIN void __c17_vst4_lane_u32(uint32_t *__p, uint32x2x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4_lane_u32(...) __c17_vst4_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t vld1q_u32(const uint32_t *__p)
{
  uint32x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u32(uint32_t *__p, uint32x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint32x4_t vld1q_dup_u32(const uint32_t *__p)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN uint32x4_t __c17_vld1q_lane_u32(const uint32_t *__p, uint32x4_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1q_lane_u32(...) __c17_vld1q_lane_u32(__VA_ARGS__)

__C17_INTRIN void __c17_vst1q_lane_u32(uint32_t *__p, uint32x4_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1q_lane_u32(...) __c17_vst1q_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x4x2_t vld1q_u32_x2(const uint32_t *__p)
{
  uint32x4x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u32_x2(uint32_t *__p, uint32x4x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint32x4x2_t vld2q_u32(const uint32_t *__p)
{
  uint32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2q_u32(uint32_t *__p, uint32x4x2_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN uint32x4x2_t vld2q_dup_u32(const uint32_t *__p)
{
  uint32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN uint32x4x2_t __c17_vld2q_lane_u32(const uint32_t *__p, uint32x4x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2q_lane_u32(...) __c17_vld2q_lane_u32(__VA_ARGS__)

__C17_INTRIN void __c17_vst2q_lane_u32(uint32_t *__p, uint32x4x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2q_lane_u32(...) __c17_vst2q_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x4x3_t vld1q_u32_x3(const uint32_t *__p)
{
  uint32x4x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u32_x3(uint32_t *__p, uint32x4x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint32x4x3_t vld3q_u32(const uint32_t *__p)
{
  uint32x4x3_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3q_u32(uint32_t *__p, uint32x4x3_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN uint32x4x3_t vld3q_dup_u32(const uint32_t *__p)
{
  uint32x4x3_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN uint32x4x3_t __c17_vld3q_lane_u32(const uint32_t *__p, uint32x4x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3q_lane_u32(...) __c17_vld3q_lane_u32(__VA_ARGS__)

__C17_INTRIN void __c17_vst3q_lane_u32(uint32_t *__p, uint32x4x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3q_lane_u32(...) __c17_vst3q_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x4x4_t vld1q_u32_x4(const uint32_t *__p)
{
  uint32x4x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u32_x4(uint32_t *__p, uint32x4x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint32x4x4_t vld4q_u32(const uint32_t *__p)
{
  uint32x4x4_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4q_u32(uint32_t *__p, uint32x4x4_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN uint32x4x4_t vld4q_dup_u32(const uint32_t *__p)
{
  uint32x4x4_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN uint32x4x4_t __c17_vld4q_lane_u32(const uint32_t *__p, uint32x4x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4q_lane_u32(...) __c17_vld4q_lane_u32(__VA_ARGS__)

__C17_INTRIN void __c17_vst4q_lane_u32(uint32_t *__p, uint32x4x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4q_lane_u32(...) __c17_vst4q_lane_u32(__VA_ARGS__)

__C17_INTRIN uint64x1_t vld1_u64(const uint64_t *__p)
{
  uint64x1_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u64(uint64_t *__p, uint64x1_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint64x1_t vld1_dup_u64(const uint64_t *__p)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN uint64x1_t __c17_vld1_lane_u64(const uint64_t *__p, uint64x1_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1_lane_u64(...) __c17_vld1_lane_u64(__VA_ARGS__)

__C17_INTRIN void __c17_vst1_lane_u64(uint64_t *__p, uint64x1_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1_lane_u64(...) __c17_vst1_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x1x2_t vld1_u64_x2(const uint64_t *__p)
{
  uint64x1x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u64_x2(uint64_t *__p, uint64x1x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint64x1x2_t vld2_u64(const uint64_t *__p)
{
  uint64x1x2_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2_u64(uint64_t *__p, uint64x1x2_t __a)
{
  for (int __i = 0; __i < 1; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN uint64x1x2_t vld2_dup_u64(const uint64_t *__p)
{
  uint64x1x2_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN uint64x1x2_t __c17_vld2_lane_u64(const uint64_t *__p, uint64x1x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2_lane_u64(...) __c17_vld2_lane_u64(__VA_ARGS__)

__C17_INTRIN void __c17_vst2_lane_u64(uint64_t *__p, uint64x1x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2_lane_u64(...) __c17_vst2_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x1x3_t vld1_u64_x3(const uint64_t *__p)
{
  uint64x1x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u64_x3(uint64_t *__p, uint64x1x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint64x1x3_t vld3_u64(const uint64_t *__p)
{
  uint64x1x3_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3_u64(uint64_t *__p, uint64x1x3_t __a)
{
  for (int __i = 0; __i < 1; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN uint64x1x3_t vld3_dup_u64(const uint64_t *__p)
{
  uint64x1x3_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN uint64x1x3_t __c17_vld3_lane_u64(const uint64_t *__p, uint64x1x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3_lane_u64(...) __c17_vld3_lane_u64(__VA_ARGS__)

__C17_INTRIN void __c17_vst3_lane_u64(uint64_t *__p, uint64x1x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3_lane_u64(...) __c17_vst3_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x1x4_t vld1_u64_x4(const uint64_t *__p)
{
  uint64x1x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_u64_x4(uint64_t *__p, uint64x1x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint64x1x4_t vld4_u64(const uint64_t *__p)
{
  uint64x1x4_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4_u64(uint64_t *__p, uint64x1x4_t __a)
{
  for (int __i = 0; __i < 1; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN uint64x1x4_t vld4_dup_u64(const uint64_t *__p)
{
  uint64x1x4_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN uint64x1x4_t __c17_vld4_lane_u64(const uint64_t *__p, uint64x1x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4_lane_u64(...) __c17_vld4_lane_u64(__VA_ARGS__)

__C17_INTRIN void __c17_vst4_lane_u64(uint64_t *__p, uint64x1x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4_lane_u64(...) __c17_vst4_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t vld1q_u64(const uint64_t *__p)
{
  uint64x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u64(uint64_t *__p, uint64x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint64x2_t vld1q_dup_u64(const uint64_t *__p)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN uint64x2_t __c17_vld1q_lane_u64(const uint64_t *__p, uint64x2_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1q_lane_u64(...) __c17_vld1q_lane_u64(__VA_ARGS__)

__C17_INTRIN void __c17_vst1q_lane_u64(uint64_t *__p, uint64x2_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1q_lane_u64(...) __c17_vst1q_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x2x2_t vld1q_u64_x2(const uint64_t *__p)
{
  uint64x2x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u64_x2(uint64_t *__p, uint64x2x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint64x2x2_t vld2q_u64(const uint64_t *__p)
{
  uint64x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2q_u64(uint64_t *__p, uint64x2x2_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN uint64x2x2_t vld2q_dup_u64(const uint64_t *__p)
{
  uint64x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN uint64x2x2_t __c17_vld2q_lane_u64(const uint64_t *__p, uint64x2x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2q_lane_u64(...) __c17_vld2q_lane_u64(__VA_ARGS__)

__C17_INTRIN void __c17_vst2q_lane_u64(uint64_t *__p, uint64x2x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2q_lane_u64(...) __c17_vst2q_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x2x3_t vld1q_u64_x3(const uint64_t *__p)
{
  uint64x2x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u64_x3(uint64_t *__p, uint64x2x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint64x2x3_t vld3q_u64(const uint64_t *__p)
{
  uint64x2x3_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3q_u64(uint64_t *__p, uint64x2x3_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN uint64x2x3_t vld3q_dup_u64(const uint64_t *__p)
{
  uint64x2x3_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN uint64x2x3_t __c17_vld3q_lane_u64(const uint64_t *__p, uint64x2x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3q_lane_u64(...) __c17_vld3q_lane_u64(__VA_ARGS__)

__C17_INTRIN void __c17_vst3q_lane_u64(uint64_t *__p, uint64x2x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3q_lane_u64(...) __c17_vst3q_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x2x4_t vld1q_u64_x4(const uint64_t *__p)
{
  uint64x2x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_u64_x4(uint64_t *__p, uint64x2x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN uint64x2x4_t vld4q_u64(const uint64_t *__p)
{
  uint64x2x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4q_u64(uint64_t *__p, uint64x2x4_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN uint64x2x4_t vld4q_dup_u64(const uint64_t *__p)
{
  uint64x2x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN uint64x2x4_t __c17_vld4q_lane_u64(const uint64_t *__p, uint64x2x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4q_lane_u64(...) __c17_vld4q_lane_u64(__VA_ARGS__)

__C17_INTRIN void __c17_vst4q_lane_u64(uint64_t *__p, uint64x2x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4q_lane_u64(...) __c17_vst4q_lane_u64(__VA_ARGS__)

__C17_INTRIN float32x2_t vld1_f32(const float32_t *__p)
{
  float32x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_f32(float32_t *__p, float32x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float32x2_t vld1_dup_f32(const float32_t *__p)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN float32x2_t __c17_vld1_lane_f32(const float32_t *__p, float32x2_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1_lane_f32(...) __c17_vld1_lane_f32(__VA_ARGS__)

__C17_INTRIN void __c17_vst1_lane_f32(float32_t *__p, float32x2_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1_lane_f32(...) __c17_vst1_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2x2_t vld1_f32_x2(const float32_t *__p)
{
  float32x2x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_f32_x2(float32_t *__p, float32x2x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float32x2x2_t vld2_f32(const float32_t *__p)
{
  float32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2_f32(float32_t *__p, float32x2x2_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN float32x2x2_t vld2_dup_f32(const float32_t *__p)
{
  float32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN float32x2x2_t __c17_vld2_lane_f32(const float32_t *__p, float32x2x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2_lane_f32(...) __c17_vld2_lane_f32(__VA_ARGS__)

__C17_INTRIN void __c17_vst2_lane_f32(float32_t *__p, float32x2x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2_lane_f32(...) __c17_vst2_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2x3_t vld1_f32_x3(const float32_t *__p)
{
  float32x2x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_f32_x3(float32_t *__p, float32x2x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float32x2x3_t vld3_f32(const float32_t *__p)
{
  float32x2x3_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3_f32(float32_t *__p, float32x2x3_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN float32x2x3_t vld3_dup_f32(const float32_t *__p)
{
  float32x2x3_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN float32x2x3_t __c17_vld3_lane_f32(const float32_t *__p, float32x2x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3_lane_f32(...) __c17_vld3_lane_f32(__VA_ARGS__)

__C17_INTRIN void __c17_vst3_lane_f32(float32_t *__p, float32x2x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3_lane_f32(...) __c17_vst3_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2x4_t vld1_f32_x4(const float32_t *__p)
{
  float32x2x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_f32_x4(float32_t *__p, float32x2x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float32x2x4_t vld4_f32(const float32_t *__p)
{
  float32x2x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4_f32(float32_t *__p, float32x2x4_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN float32x2x4_t vld4_dup_f32(const float32_t *__p)
{
  float32x2x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN float32x2x4_t __c17_vld4_lane_f32(const float32_t *__p, float32x2x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4_lane_f32(...) __c17_vld4_lane_f32(__VA_ARGS__)

__C17_INTRIN void __c17_vst4_lane_f32(float32_t *__p, float32x2x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4_lane_f32(...) __c17_vst4_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t vld1q_f32(const float32_t *__p)
{
  float32x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_f32(float32_t *__p, float32x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float32x4_t vld1q_dup_f32(const float32_t *__p)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN float32x4_t __c17_vld1q_lane_f32(const float32_t *__p, float32x4_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1q_lane_f32(...) __c17_vld1q_lane_f32(__VA_ARGS__)

__C17_INTRIN void __c17_vst1q_lane_f32(float32_t *__p, float32x4_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1q_lane_f32(...) __c17_vst1q_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4x2_t vld1q_f32_x2(const float32_t *__p)
{
  float32x4x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_f32_x2(float32_t *__p, float32x4x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float32x4x2_t vld2q_f32(const float32_t *__p)
{
  float32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2q_f32(float32_t *__p, float32x4x2_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN float32x4x2_t vld2q_dup_f32(const float32_t *__p)
{
  float32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN float32x4x2_t __c17_vld2q_lane_f32(const float32_t *__p, float32x4x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2q_lane_f32(...) __c17_vld2q_lane_f32(__VA_ARGS__)

__C17_INTRIN void __c17_vst2q_lane_f32(float32_t *__p, float32x4x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2q_lane_f32(...) __c17_vst2q_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4x3_t vld1q_f32_x3(const float32_t *__p)
{
  float32x4x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_f32_x3(float32_t *__p, float32x4x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float32x4x3_t vld3q_f32(const float32_t *__p)
{
  float32x4x3_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3q_f32(float32_t *__p, float32x4x3_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN float32x4x3_t vld3q_dup_f32(const float32_t *__p)
{
  float32x4x3_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN float32x4x3_t __c17_vld3q_lane_f32(const float32_t *__p, float32x4x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3q_lane_f32(...) __c17_vld3q_lane_f32(__VA_ARGS__)

__C17_INTRIN void __c17_vst3q_lane_f32(float32_t *__p, float32x4x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3q_lane_f32(...) __c17_vst3q_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4x4_t vld1q_f32_x4(const float32_t *__p)
{
  float32x4x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_f32_x4(float32_t *__p, float32x4x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float32x4x4_t vld4q_f32(const float32_t *__p)
{
  float32x4x4_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4q_f32(float32_t *__p, float32x4x4_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN float32x4x4_t vld4q_dup_f32(const float32_t *__p)
{
  float32x4x4_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN float32x4x4_t __c17_vld4q_lane_f32(const float32_t *__p, float32x4x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4q_lane_f32(...) __c17_vld4q_lane_f32(__VA_ARGS__)

__C17_INTRIN void __c17_vst4q_lane_f32(float32_t *__p, float32x4x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4q_lane_f32(...) __c17_vst4q_lane_f32(__VA_ARGS__)

__C17_INTRIN float64x1_t vld1_f64(const float64_t *__p)
{
  float64x1_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_f64(float64_t *__p, float64x1_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float64x1_t vld1_dup_f64(const float64_t *__p)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN float64x1_t __c17_vld1_lane_f64(const float64_t *__p, float64x1_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1_lane_f64(...) __c17_vld1_lane_f64(__VA_ARGS__)

__C17_INTRIN void __c17_vst1_lane_f64(float64_t *__p, float64x1_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1_lane_f64(...) __c17_vst1_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x1x2_t vld1_f64_x2(const float64_t *__p)
{
  float64x1x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_f64_x2(float64_t *__p, float64x1x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float64x1x2_t vld2_f64(const float64_t *__p)
{
  float64x1x2_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2_f64(float64_t *__p, float64x1x2_t __a)
{
  for (int __i = 0; __i < 1; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN float64x1x2_t vld2_dup_f64(const float64_t *__p)
{
  float64x1x2_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN float64x1x2_t __c17_vld2_lane_f64(const float64_t *__p, float64x1x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2_lane_f64(...) __c17_vld2_lane_f64(__VA_ARGS__)

__C17_INTRIN void __c17_vst2_lane_f64(float64_t *__p, float64x1x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2_lane_f64(...) __c17_vst2_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x1x3_t vld1_f64_x3(const float64_t *__p)
{
  float64x1x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_f64_x3(float64_t *__p, float64x1x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float64x1x3_t vld3_f64(const float64_t *__p)
{
  float64x1x3_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3_f64(float64_t *__p, float64x1x3_t __a)
{
  for (int __i = 0; __i < 1; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN float64x1x3_t vld3_dup_f64(const float64_t *__p)
{
  float64x1x3_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN float64x1x3_t __c17_vld3_lane_f64(const float64_t *__p, float64x1x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3_lane_f64(...) __c17_vld3_lane_f64(__VA_ARGS__)

__C17_INTRIN void __c17_vst3_lane_f64(float64_t *__p, float64x1x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3_lane_f64(...) __c17_vst3_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x1x4_t vld1_f64_x4(const float64_t *__p)
{
  float64x1x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_f64_x4(float64_t *__p, float64x1x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float64x1x4_t vld4_f64(const float64_t *__p)
{
  float64x1x4_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4_f64(float64_t *__p, float64x1x4_t __a)
{
  for (int __i = 0; __i < 1; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN float64x1x4_t vld4_dup_f64(const float64_t *__p)
{
  float64x1x4_t __r;
  for (int __i = 0; __i < 1; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN float64x1x4_t __c17_vld4_lane_f64(const float64_t *__p, float64x1x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4_lane_f64(...) __c17_vld4_lane_f64(__VA_ARGS__)

__C17_INTRIN void __c17_vst4_lane_f64(float64_t *__p, float64x1x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4_lane_f64(...) __c17_vst4_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t vld1q_f64(const float64_t *__p)
{
  float64x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_f64(float64_t *__p, float64x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float64x2_t vld1q_dup_f64(const float64_t *__p)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN float64x2_t __c17_vld1q_lane_f64(const float64_t *__p, float64x2_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1q_lane_f64(...) __c17_vld1q_lane_f64(__VA_ARGS__)

__C17_INTRIN void __c17_vst1q_lane_f64(float64_t *__p, float64x2_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1q_lane_f64(...) __c17_vst1q_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x2x2_t vld1q_f64_x2(const float64_t *__p)
{
  float64x2x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_f64_x2(float64_t *__p, float64x2x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float64x2x2_t vld2q_f64(const float64_t *__p)
{
  float64x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2q_f64(float64_t *__p, float64x2x2_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN float64x2x2_t vld2q_dup_f64(const float64_t *__p)
{
  float64x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN float64x2x2_t __c17_vld2q_lane_f64(const float64_t *__p, float64x2x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2q_lane_f64(...) __c17_vld2q_lane_f64(__VA_ARGS__)

__C17_INTRIN void __c17_vst2q_lane_f64(float64_t *__p, float64x2x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2q_lane_f64(...) __c17_vst2q_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x2x3_t vld1q_f64_x3(const float64_t *__p)
{
  float64x2x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_f64_x3(float64_t *__p, float64x2x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float64x2x3_t vld3q_f64(const float64_t *__p)
{
  float64x2x3_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3q_f64(float64_t *__p, float64x2x3_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN float64x2x3_t vld3q_dup_f64(const float64_t *__p)
{
  float64x2x3_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN float64x2x3_t __c17_vld3q_lane_f64(const float64_t *__p, float64x2x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3q_lane_f64(...) __c17_vld3q_lane_f64(__VA_ARGS__)

__C17_INTRIN void __c17_vst3q_lane_f64(float64_t *__p, float64x2x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3q_lane_f64(...) __c17_vst3q_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x2x4_t vld1q_f64_x4(const float64_t *__p)
{
  float64x2x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_f64_x4(float64_t *__p, float64x2x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN float64x2x4_t vld4q_f64(const float64_t *__p)
{
  float64x2x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4q_f64(float64_t *__p, float64x2x4_t __a)
{
  for (int __i = 0; __i < 2; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN float64x2x4_t vld4q_dup_f64(const float64_t *__p)
{
  float64x2x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN float64x2x4_t __c17_vld4q_lane_f64(const float64_t *__p, float64x2x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4q_lane_f64(...) __c17_vld4q_lane_f64(__VA_ARGS__)

__C17_INTRIN void __c17_vst4q_lane_f64(float64_t *__p, float64x2x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4q_lane_f64(...) __c17_vst4q_lane_f64(__VA_ARGS__)

__C17_INTRIN poly8x8_t vld1_p8(const poly8_t *__p)
{
  poly8x8_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_p8(poly8_t *__p, poly8x8_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly8x8_t vld1_dup_p8(const poly8_t *__p)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN poly8x8_t __c17_vld1_lane_p8(const poly8_t *__p, poly8x8_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1_lane_p8(...) __c17_vld1_lane_p8(__VA_ARGS__)

__C17_INTRIN void __c17_vst1_lane_p8(poly8_t *__p, poly8x8_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1_lane_p8(...) __c17_vst1_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x8x2_t vld1_p8_x2(const poly8_t *__p)
{
  poly8x8x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_p8_x2(poly8_t *__p, poly8x8x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly8x8x2_t vld2_p8(const poly8_t *__p)
{
  poly8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2_p8(poly8_t *__p, poly8x8x2_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN poly8x8x2_t vld2_dup_p8(const poly8_t *__p)
{
  poly8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN poly8x8x2_t __c17_vld2_lane_p8(const poly8_t *__p, poly8x8x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2_lane_p8(...) __c17_vld2_lane_p8(__VA_ARGS__)

__C17_INTRIN void __c17_vst2_lane_p8(poly8_t *__p, poly8x8x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2_lane_p8(...) __c17_vst2_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x8x3_t vld1_p8_x3(const poly8_t *__p)
{
  poly8x8x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_p8_x3(poly8_t *__p, poly8x8x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly8x8x3_t vld3_p8(const poly8_t *__p)
{
  poly8x8x3_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3_p8(poly8_t *__p, poly8x8x3_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN poly8x8x3_t vld3_dup_p8(const poly8_t *__p)
{
  poly8x8x3_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN poly8x8x3_t __c17_vld3_lane_p8(const poly8_t *__p, poly8x8x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3_lane_p8(...) __c17_vld3_lane_p8(__VA_ARGS__)

__C17_INTRIN void __c17_vst3_lane_p8(poly8_t *__p, poly8x8x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3_lane_p8(...) __c17_vst3_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x8x4_t vld1_p8_x4(const poly8_t *__p)
{
  poly8x8x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_p8_x4(poly8_t *__p, poly8x8x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly8x8x4_t vld4_p8(const poly8_t *__p)
{
  poly8x8x4_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4_p8(poly8_t *__p, poly8x8x4_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN poly8x8x4_t vld4_dup_p8(const poly8_t *__p)
{
  poly8x8x4_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN poly8x8x4_t __c17_vld4_lane_p8(const poly8_t *__p, poly8x8x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4_lane_p8(...) __c17_vld4_lane_p8(__VA_ARGS__)

__C17_INTRIN void __c17_vst4_lane_p8(poly8_t *__p, poly8x8x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4_lane_p8(...) __c17_vst4_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x16_t vld1q_p8(const poly8_t *__p)
{
  poly8x16_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_p8(poly8_t *__p, poly8x16_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly8x16_t vld1q_dup_p8(const poly8_t *__p)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN poly8x16_t __c17_vld1q_lane_p8(const poly8_t *__p, poly8x16_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1q_lane_p8(...) __c17_vld1q_lane_p8(__VA_ARGS__)

__C17_INTRIN void __c17_vst1q_lane_p8(poly8_t *__p, poly8x16_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1q_lane_p8(...) __c17_vst1q_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x16x2_t vld1q_p8_x2(const poly8_t *__p)
{
  poly8x16x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_p8_x2(poly8_t *__p, poly8x16x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly8x16x2_t vld2q_p8(const poly8_t *__p)
{
  poly8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2q_p8(poly8_t *__p, poly8x16x2_t __a)
{
  for (int __i = 0; __i < 16; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN poly8x16x2_t vld2q_dup_p8(const poly8_t *__p)
{
  poly8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN poly8x16x2_t __c17_vld2q_lane_p8(const poly8_t *__p, poly8x16x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2q_lane_p8(...) __c17_vld2q_lane_p8(__VA_ARGS__)

__C17_INTRIN void __c17_vst2q_lane_p8(poly8_t *__p, poly8x16x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2q_lane_p8(...) __c17_vst2q_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x16x3_t vld1q_p8_x3(const poly8_t *__p)
{
  poly8x16x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_p8_x3(poly8_t *__p, poly8x16x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly8x16x3_t vld3q_p8(const poly8_t *__p)
{
  poly8x16x3_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3q_p8(poly8_t *__p, poly8x16x3_t __a)
{
  for (int __i = 0; __i < 16; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN poly8x16x3_t vld3q_dup_p8(const poly8_t *__p)
{
  poly8x16x3_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN poly8x16x3_t __c17_vld3q_lane_p8(const poly8_t *__p, poly8x16x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3q_lane_p8(...) __c17_vld3q_lane_p8(__VA_ARGS__)

__C17_INTRIN void __c17_vst3q_lane_p8(poly8_t *__p, poly8x16x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3q_lane_p8(...) __c17_vst3q_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x16x4_t vld1q_p8_x4(const poly8_t *__p)
{
  poly8x16x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_p8_x4(poly8_t *__p, poly8x16x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly8x16x4_t vld4q_p8(const poly8_t *__p)
{
  poly8x16x4_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4q_p8(poly8_t *__p, poly8x16x4_t __a)
{
  for (int __i = 0; __i < 16; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN poly8x16x4_t vld4q_dup_p8(const poly8_t *__p)
{
  poly8x16x4_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN poly8x16x4_t __c17_vld4q_lane_p8(const poly8_t *__p, poly8x16x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4q_lane_p8(...) __c17_vld4q_lane_p8(__VA_ARGS__)

__C17_INTRIN void __c17_vst4q_lane_p8(poly8_t *__p, poly8x16x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4q_lane_p8(...) __c17_vst4q_lane_p8(__VA_ARGS__)

__C17_INTRIN poly16x4_t vld1_p16(const poly16_t *__p)
{
  poly16x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_p16(poly16_t *__p, poly16x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly16x4_t vld1_dup_p16(const poly16_t *__p)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN poly16x4_t __c17_vld1_lane_p16(const poly16_t *__p, poly16x4_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1_lane_p16(...) __c17_vld1_lane_p16(__VA_ARGS__)

__C17_INTRIN void __c17_vst1_lane_p16(poly16_t *__p, poly16x4_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1_lane_p16(...) __c17_vst1_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x4x2_t vld1_p16_x2(const poly16_t *__p)
{
  poly16x4x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_p16_x2(poly16_t *__p, poly16x4x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly16x4x2_t vld2_p16(const poly16_t *__p)
{
  poly16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2_p16(poly16_t *__p, poly16x4x2_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN poly16x4x2_t vld2_dup_p16(const poly16_t *__p)
{
  poly16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN poly16x4x2_t __c17_vld2_lane_p16(const poly16_t *__p, poly16x4x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2_lane_p16(...) __c17_vld2_lane_p16(__VA_ARGS__)

__C17_INTRIN void __c17_vst2_lane_p16(poly16_t *__p, poly16x4x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2_lane_p16(...) __c17_vst2_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x4x3_t vld1_p16_x3(const poly16_t *__p)
{
  poly16x4x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_p16_x3(poly16_t *__p, poly16x4x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly16x4x3_t vld3_p16(const poly16_t *__p)
{
  poly16x4x3_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3_p16(poly16_t *__p, poly16x4x3_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN poly16x4x3_t vld3_dup_p16(const poly16_t *__p)
{
  poly16x4x3_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN poly16x4x3_t __c17_vld3_lane_p16(const poly16_t *__p, poly16x4x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3_lane_p16(...) __c17_vld3_lane_p16(__VA_ARGS__)

__C17_INTRIN void __c17_vst3_lane_p16(poly16_t *__p, poly16x4x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3_lane_p16(...) __c17_vst3_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x4x4_t vld1_p16_x4(const poly16_t *__p)
{
  poly16x4x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1_p16_x4(poly16_t *__p, poly16x4x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly16x4x4_t vld4_p16(const poly16_t *__p)
{
  poly16x4x4_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4_p16(poly16_t *__p, poly16x4x4_t __a)
{
  for (int __i = 0; __i < 4; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN poly16x4x4_t vld4_dup_p16(const poly16_t *__p)
{
  poly16x4x4_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN poly16x4x4_t __c17_vld4_lane_p16(const poly16_t *__p, poly16x4x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4_lane_p16(...) __c17_vld4_lane_p16(__VA_ARGS__)

__C17_INTRIN void __c17_vst4_lane_p16(poly16_t *__p, poly16x4x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4_lane_p16(...) __c17_vst4_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x8_t vld1q_p16(const poly16_t *__p)
{
  poly16x8_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_p16(poly16_t *__p, poly16x8_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly16x8_t vld1q_dup_p16(const poly16_t *__p)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = *__p;
  return __r;
}

__C17_INTRIN poly16x8_t __c17_vld1q_lane_p16(const poly16_t *__p, poly16x8_t __a, const int __lane)
{
  __a[__lane] = *__p;
  return __a;
}
#define vld1q_lane_p16(...) __c17_vld1q_lane_p16(__VA_ARGS__)

__C17_INTRIN void __c17_vst1q_lane_p16(poly16_t *__p, poly16x8_t __a, const int __lane)
{
  *__p = __a[__lane];
}
#define vst1q_lane_p16(...) __c17_vst1q_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x8x2_t vld1q_p16_x2(const poly16_t *__p)
{
  poly16x8x2_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_p16_x2(poly16_t *__p, poly16x8x2_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly16x8x2_t vld2q_p16(const poly16_t *__p)
{
  poly16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[2 * __i + 0];
    __r.val[1][__i] = __p[2 * __i + 1];
  }
  return __r;
}

__C17_INTRIN void vst2q_p16(poly16_t *__p, poly16x8x2_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[2 * __i + 0] = __a.val[0][__i];
    __p[2 * __i + 1] = __a.val[1][__i];
  }
}

__C17_INTRIN poly16x8x2_t vld2q_dup_p16(const poly16_t *__p)
{
  poly16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
  }
  return __r;
}

__C17_INTRIN poly16x8x2_t __c17_vld2q_lane_p16(const poly16_t *__p, poly16x8x2_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  return __a;
}
#define vld2q_lane_p16(...) __c17_vld2q_lane_p16(__VA_ARGS__)

__C17_INTRIN void __c17_vst2q_lane_p16(poly16_t *__p, poly16x8x2_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
}
#define vst2q_lane_p16(...) __c17_vst2q_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x8x3_t vld1q_p16_x3(const poly16_t *__p)
{
  poly16x8x3_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_p16_x3(poly16_t *__p, poly16x8x3_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly16x8x3_t vld3q_p16(const poly16_t *__p)
{
  poly16x8x3_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[3 * __i + 0];
    __r.val[1][__i] = __p[3 * __i + 1];
    __r.val[2][__i] = __p[3 * __i + 2];
  }
  return __r;
}

__C17_INTRIN void vst3q_p16(poly16_t *__p, poly16x8x3_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[3 * __i + 0] = __a.val[0][__i];
    __p[3 * __i + 1] = __a.val[1][__i];
    __p[3 * __i + 2] = __a.val[2][__i];
  }
}

__C17_INTRIN poly16x8x3_t vld3q_dup_p16(const poly16_t *__p)
{
  poly16x8x3_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
  }
  return __r;
}

__C17_INTRIN poly16x8x3_t __c17_vld3q_lane_p16(const poly16_t *__p, poly16x8x3_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  return __a;
}
#define vld3q_lane_p16(...) __c17_vld3q_lane_p16(__VA_ARGS__)

__C17_INTRIN void __c17_vst3q_lane_p16(poly16_t *__p, poly16x8x3_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
}
#define vst3q_lane_p16(...) __c17_vst3q_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x8x4_t vld1q_p16_x4(const poly16_t *__p)
{
  poly16x8x4_t __r;
  __builtin_memcpy(&__r, __p, sizeof(__r));
  return __r;
}

__C17_INTRIN void vst1q_p16_x4(poly16_t *__p, poly16x8x4_t __a)
{
  __builtin_memcpy(__p, &__a, sizeof(__a));
}

__C17_INTRIN poly16x8x4_t vld4q_p16(const poly16_t *__p)
{
  poly16x8x4_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[4 * __i + 0];
    __r.val[1][__i] = __p[4 * __i + 1];
    __r.val[2][__i] = __p[4 * __i + 2];
    __r.val[3][__i] = __p[4 * __i + 3];
  }
  return __r;
}

__C17_INTRIN void vst4q_p16(poly16_t *__p, poly16x8x4_t __a)
{
  for (int __i = 0; __i < 8; __i++) {
    __p[4 * __i + 0] = __a.val[0][__i];
    __p[4 * __i + 1] = __a.val[1][__i];
    __p[4 * __i + 2] = __a.val[2][__i];
    __p[4 * __i + 3] = __a.val[3][__i];
  }
}

__C17_INTRIN poly16x8x4_t vld4q_dup_p16(const poly16_t *__p)
{
  poly16x8x4_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __p[0];
    __r.val[1][__i] = __p[1];
    __r.val[2][__i] = __p[2];
    __r.val[3][__i] = __p[3];
  }
  return __r;
}

__C17_INTRIN poly16x8x4_t __c17_vld4q_lane_p16(const poly16_t *__p, poly16x8x4_t __a, const int __lane)
{
  __a.val[0][__lane] = __p[0];
  __a.val[1][__lane] = __p[1];
  __a.val[2][__lane] = __p[2];
  __a.val[3][__lane] = __p[3];
  return __a;
}
#define vld4q_lane_p16(...) __c17_vld4q_lane_p16(__VA_ARGS__)

__C17_INTRIN void __c17_vst4q_lane_p16(poly16_t *__p, poly16x8x4_t __a, const int __lane)
{
  __p[0] = __a.val[0][__lane];
  __p[1] = __a.val[1][__lane];
  __p[2] = __a.val[2][__lane];
  __p[3] = __a.val[3][__lane];
}
#define vst4q_lane_p16(...) __c17_vst4q_lane_p16(__VA_ARGS__)


/* Duplication, lanes, combination and reinterpretation. */

__C17_INTRIN int8x8_t vdup_n_s8(int8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int8x8_t vmov_n_s8(int8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int8x8_t __c17_vdup_lane_s8(int8x8_t __a, const int __lane)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_lane_s8(...) __c17_vdup_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vdup_laneq_s8(int8x16_t __a, const int __lane)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_laneq_s8(...) __c17_vdup_laneq_s8(__VA_ARGS__)

__C17_INTRIN int8_t __c17_vget_lane_s8(int8x8_t __a, const int __lane)
{
  return __a[__lane];
}
#define vget_lane_s8(...) __c17_vget_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vset_lane_s8(int8_t __a, int8x8_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vset_lane_s8(...) __c17_vset_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vcopy_lane_s8(int8x8_t __a, const int __la, int8x8_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_lane_s8(...) __c17_vcopy_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vcopy_laneq_s8(int8x8_t __a, const int __la, int8x16_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_laneq_s8(...) __c17_vcopy_laneq_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t vdupq_n_s8(int8_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int8x16_t vmovq_n_s8(int8_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int8x16_t __c17_vdupq_lane_s8(int8x8_t __a, const int __lane)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_lane_s8(...) __c17_vdupq_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vdupq_laneq_s8(int8x16_t __a, const int __lane)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_laneq_s8(...) __c17_vdupq_laneq_s8(__VA_ARGS__)

__C17_INTRIN int8_t __c17_vgetq_lane_s8(int8x16_t __a, const int __lane)
{
  return __a[__lane];
}
#define vgetq_lane_s8(...) __c17_vgetq_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vsetq_lane_s8(int8_t __a, int8x16_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vsetq_lane_s8(...) __c17_vsetq_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vcopyq_lane_s8(int8x16_t __a, const int __la, int8x8_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_lane_s8(...) __c17_vcopyq_lane_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vcopyq_laneq_s8(int8x16_t __a, const int __la, int8x16_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_laneq_s8(...) __c17_vcopyq_laneq_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t vget_low_s8(int8x16_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN int8x8_t vget_high_s8(int8x16_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i + 8];
  return __r;
}

__C17_INTRIN int8x16_t vcombine_s8(int8x8_t __a, int8x8_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? __a[__i] : __b[__i - 8];
  return __r;
}

__C17_INTRIN int8x8_t vcreate_s8(uint64_t __a)
{
  return (int8x8_t)(uint64x1_t){__a};
}

__C17_INTRIN int16x4_t vdup_n_s16(int16_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int16x4_t vmov_n_s16(int16_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int16x4_t __c17_vdup_lane_s16(int16x4_t __a, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_lane_s16(...) __c17_vdup_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vdup_laneq_s16(int16x8_t __a, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_laneq_s16(...) __c17_vdup_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16_t __c17_vget_lane_s16(int16x4_t __a, const int __lane)
{
  return __a[__lane];
}
#define vget_lane_s16(...) __c17_vget_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vset_lane_s16(int16_t __a, int16x4_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vset_lane_s16(...) __c17_vset_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vcopy_lane_s16(int16x4_t __a, const int __la, int16x4_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_lane_s16(...) __c17_vcopy_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vcopy_laneq_s16(int16x4_t __a, const int __la, int16x8_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_laneq_s16(...) __c17_vcopy_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t vdupq_n_s16(int16_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int16x8_t vmovq_n_s16(int16_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int16x8_t __c17_vdupq_lane_s16(int16x4_t __a, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_lane_s16(...) __c17_vdupq_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vdupq_laneq_s16(int16x8_t __a, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_laneq_s16(...) __c17_vdupq_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16_t __c17_vgetq_lane_s16(int16x8_t __a, const int __lane)
{
  return __a[__lane];
}
#define vgetq_lane_s16(...) __c17_vgetq_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vsetq_lane_s16(int16_t __a, int16x8_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vsetq_lane_s16(...) __c17_vsetq_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vcopyq_lane_s16(int16x8_t __a, const int __la, int16x4_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_lane_s16(...) __c17_vcopyq_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vcopyq_laneq_s16(int16x8_t __a, const int __la, int16x8_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_laneq_s16(...) __c17_vcopyq_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t vget_low_s16(int16x8_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN int16x4_t vget_high_s16(int16x8_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i + 4];
  return __r;
}

__C17_INTRIN int16x8_t vcombine_s16(int16x4_t __a, int16x4_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[__i] : __b[__i - 4];
  return __r;
}

__C17_INTRIN int16x4_t vcreate_s16(uint64_t __a)
{
  return (int16x4_t)(uint64x1_t){__a};
}

__C17_INTRIN int32x2_t vdup_n_s32(int32_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int32x2_t vmov_n_s32(int32_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int32x2_t __c17_vdup_lane_s32(int32x2_t __a, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_lane_s32(...) __c17_vdup_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vdup_laneq_s32(int32x4_t __a, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_laneq_s32(...) __c17_vdup_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32_t __c17_vget_lane_s32(int32x2_t __a, const int __lane)
{
  return __a[__lane];
}
#define vget_lane_s32(...) __c17_vget_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vset_lane_s32(int32_t __a, int32x2_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vset_lane_s32(...) __c17_vset_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vcopy_lane_s32(int32x2_t __a, const int __la, int32x2_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_lane_s32(...) __c17_vcopy_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vcopy_laneq_s32(int32x2_t __a, const int __la, int32x4_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_laneq_s32(...) __c17_vcopy_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t vdupq_n_s32(int32_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int32x4_t vmovq_n_s32(int32_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int32x4_t __c17_vdupq_lane_s32(int32x2_t __a, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_lane_s32(...) __c17_vdupq_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vdupq_laneq_s32(int32x4_t __a, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_laneq_s32(...) __c17_vdupq_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32_t __c17_vgetq_lane_s32(int32x4_t __a, const int __lane)
{
  return __a[__lane];
}
#define vgetq_lane_s32(...) __c17_vgetq_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vsetq_lane_s32(int32_t __a, int32x4_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vsetq_lane_s32(...) __c17_vsetq_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vcopyq_lane_s32(int32x4_t __a, const int __la, int32x2_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_lane_s32(...) __c17_vcopyq_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vcopyq_laneq_s32(int32x4_t __a, const int __la, int32x4_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_laneq_s32(...) __c17_vcopyq_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t vget_low_s32(int32x4_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN int32x2_t vget_high_s32(int32x4_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i + 2];
  return __r;
}

__C17_INTRIN int32x4_t vcombine_s32(int32x2_t __a, int32x2_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[__i] : __b[__i - 2];
  return __r;
}

__C17_INTRIN int32x2_t vcreate_s32(uint64_t __a)
{
  return (int32x2_t)(uint64x1_t){__a};
}

__C17_INTRIN int64x1_t vdup_n_s64(int64_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int64x1_t vmov_n_s64(int64_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int64x1_t __c17_vdup_lane_s64(int64x1_t __a, const int __lane)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_lane_s64(...) __c17_vdup_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t __c17_vdup_laneq_s64(int64x2_t __a, const int __lane)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_laneq_s64(...) __c17_vdup_laneq_s64(__VA_ARGS__)

__C17_INTRIN int64_t __c17_vget_lane_s64(int64x1_t __a, const int __lane)
{
  return __a[__lane];
}
#define vget_lane_s64(...) __c17_vget_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t __c17_vset_lane_s64(int64_t __a, int64x1_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vset_lane_s64(...) __c17_vset_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t __c17_vcopy_lane_s64(int64x1_t __a, const int __la, int64x1_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_lane_s64(...) __c17_vcopy_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t __c17_vcopy_laneq_s64(int64x1_t __a, const int __la, int64x2_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_laneq_s64(...) __c17_vcopy_laneq_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t vdupq_n_s64(int64_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int64x2_t vmovq_n_s64(int64_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN int64x2_t __c17_vdupq_lane_s64(int64x1_t __a, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_lane_s64(...) __c17_vdupq_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vdupq_laneq_s64(int64x2_t __a, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_laneq_s64(...) __c17_vdupq_laneq_s64(__VA_ARGS__)

__C17_INTRIN int64_t __c17_vgetq_lane_s64(int64x2_t __a, const int __lane)
{
  return __a[__lane];
}
#define vgetq_lane_s64(...) __c17_vgetq_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vsetq_lane_s64(int64_t __a, int64x2_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vsetq_lane_s64(...) __c17_vsetq_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vcopyq_lane_s64(int64x2_t __a, const int __la, int64x1_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_lane_s64(...) __c17_vcopyq_lane_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vcopyq_laneq_s64(int64x2_t __a, const int __la, int64x2_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_laneq_s64(...) __c17_vcopyq_laneq_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t vget_low_s64(int64x2_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN int64x1_t vget_high_s64(int64x2_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i + 1];
  return __r;
}

__C17_INTRIN int64x2_t vcombine_s64(int64x1_t __a, int64x1_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[__i] : __b[__i - 1];
  return __r;
}

__C17_INTRIN int64x1_t vcreate_s64(uint64_t __a)
{
  return (int64x1_t)(uint64x1_t){__a};
}

__C17_INTRIN uint8x8_t vdup_n_u8(uint8_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint8x8_t vmov_n_u8(uint8_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint8x8_t __c17_vdup_lane_u8(uint8x8_t __a, const int __lane)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_lane_u8(...) __c17_vdup_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vdup_laneq_u8(uint8x16_t __a, const int __lane)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_laneq_u8(...) __c17_vdup_laneq_u8(__VA_ARGS__)

__C17_INTRIN uint8_t __c17_vget_lane_u8(uint8x8_t __a, const int __lane)
{
  return __a[__lane];
}
#define vget_lane_u8(...) __c17_vget_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vset_lane_u8(uint8_t __a, uint8x8_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vset_lane_u8(...) __c17_vset_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vcopy_lane_u8(uint8x8_t __a, const int __la, uint8x8_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_lane_u8(...) __c17_vcopy_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vcopy_laneq_u8(uint8x8_t __a, const int __la, uint8x16_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_laneq_u8(...) __c17_vcopy_laneq_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t vdupq_n_u8(uint8_t __a)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint8x16_t vmovq_n_u8(uint8_t __a)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint8x16_t __c17_vdupq_lane_u8(uint8x8_t __a, const int __lane)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_lane_u8(...) __c17_vdupq_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vdupq_laneq_u8(uint8x16_t __a, const int __lane)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_laneq_u8(...) __c17_vdupq_laneq_u8(__VA_ARGS__)

__C17_INTRIN uint8_t __c17_vgetq_lane_u8(uint8x16_t __a, const int __lane)
{
  return __a[__lane];
}
#define vgetq_lane_u8(...) __c17_vgetq_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vsetq_lane_u8(uint8_t __a, uint8x16_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vsetq_lane_u8(...) __c17_vsetq_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vcopyq_lane_u8(uint8x16_t __a, const int __la, uint8x8_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_lane_u8(...) __c17_vcopyq_lane_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vcopyq_laneq_u8(uint8x16_t __a, const int __la, uint8x16_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_laneq_u8(...) __c17_vcopyq_laneq_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t vget_low_u8(uint8x16_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vget_high_u8(uint8x16_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i + 8];
  return __r;
}

__C17_INTRIN uint8x16_t vcombine_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? __a[__i] : __b[__i - 8];
  return __r;
}

__C17_INTRIN uint8x8_t vcreate_u8(uint64_t __a)
{
  return (uint8x8_t)(uint64x1_t){__a};
}

__C17_INTRIN uint16x4_t vdup_n_u16(uint16_t __a)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint16x4_t vmov_n_u16(uint16_t __a)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint16x4_t __c17_vdup_lane_u16(uint16x4_t __a, const int __lane)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_lane_u16(...) __c17_vdup_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vdup_laneq_u16(uint16x8_t __a, const int __lane)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_laneq_u16(...) __c17_vdup_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint16_t __c17_vget_lane_u16(uint16x4_t __a, const int __lane)
{
  return __a[__lane];
}
#define vget_lane_u16(...) __c17_vget_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vset_lane_u16(uint16_t __a, uint16x4_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vset_lane_u16(...) __c17_vset_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vcopy_lane_u16(uint16x4_t __a, const int __la, uint16x4_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_lane_u16(...) __c17_vcopy_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vcopy_laneq_u16(uint16x4_t __a, const int __la, uint16x8_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_laneq_u16(...) __c17_vcopy_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t vdupq_n_u16(uint16_t __a)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint16x8_t vmovq_n_u16(uint16_t __a)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint16x8_t __c17_vdupq_lane_u16(uint16x4_t __a, const int __lane)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_lane_u16(...) __c17_vdupq_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vdupq_laneq_u16(uint16x8_t __a, const int __lane)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_laneq_u16(...) __c17_vdupq_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint16_t __c17_vgetq_lane_u16(uint16x8_t __a, const int __lane)
{
  return __a[__lane];
}
#define vgetq_lane_u16(...) __c17_vgetq_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vsetq_lane_u16(uint16_t __a, uint16x8_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vsetq_lane_u16(...) __c17_vsetq_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vcopyq_lane_u16(uint16x8_t __a, const int __la, uint16x4_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_lane_u16(...) __c17_vcopyq_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vcopyq_laneq_u16(uint16x8_t __a, const int __la, uint16x8_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_laneq_u16(...) __c17_vcopyq_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t vget_low_u16(uint16x8_t __a)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN uint16x4_t vget_high_u16(uint16x8_t __a)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i + 4];
  return __r;
}

__C17_INTRIN uint16x8_t vcombine_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[__i] : __b[__i - 4];
  return __r;
}

__C17_INTRIN uint16x4_t vcreate_u16(uint64_t __a)
{
  return (uint16x4_t)(uint64x1_t){__a};
}

__C17_INTRIN uint32x2_t vdup_n_u32(uint32_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint32x2_t vmov_n_u32(uint32_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint32x2_t __c17_vdup_lane_u32(uint32x2_t __a, const int __lane)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_lane_u32(...) __c17_vdup_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vdup_laneq_u32(uint32x4_t __a, const int __lane)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_laneq_u32(...) __c17_vdup_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint32_t __c17_vget_lane_u32(uint32x2_t __a, const int __lane)
{
  return __a[__lane];
}
#define vget_lane_u32(...) __c17_vget_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vset_lane_u32(uint32_t __a, uint32x2_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vset_lane_u32(...) __c17_vset_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vcopy_lane_u32(uint32x2_t __a, const int __la, uint32x2_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_lane_u32(...) __c17_vcopy_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vcopy_laneq_u32(uint32x2_t __a, const int __la, uint32x4_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_laneq_u32(...) __c17_vcopy_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t vdupq_n_u32(uint32_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint32x4_t vmovq_n_u32(uint32_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint32x4_t __c17_vdupq_lane_u32(uint32x2_t __a, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_lane_u32(...) __c17_vdupq_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vdupq_laneq_u32(uint32x4_t __a, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_laneq_u32(...) __c17_vdupq_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint32_t __c17_vgetq_lane_u32(uint32x4_t __a, const int __lane)
{
  return __a[__lane];
}
#define vgetq_lane_u32(...) __c17_vgetq_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vsetq_lane_u32(uint32_t __a, uint32x4_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vsetq_lane_u32(...) __c17_vsetq_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vcopyq_lane_u32(uint32x4_t __a, const int __la, uint32x2_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_lane_u32(...) __c17_vcopyq_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vcopyq_laneq_u32(uint32x4_t __a, const int __la, uint32x4_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_laneq_u32(...) __c17_vcopyq_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t vget_low_u32(uint32x4_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN uint32x2_t vget_high_u32(uint32x4_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i + 2];
  return __r;
}

__C17_INTRIN uint32x4_t vcombine_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[__i] : __b[__i - 2];
  return __r;
}

__C17_INTRIN uint32x2_t vcreate_u32(uint64_t __a)
{
  return (uint32x2_t)(uint64x1_t){__a};
}

__C17_INTRIN uint64x1_t vdup_n_u64(uint64_t __a)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint64x1_t vmov_n_u64(uint64_t __a)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint64x1_t __c17_vdup_lane_u64(uint64x1_t __a, const int __lane)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_lane_u64(...) __c17_vdup_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t __c17_vdup_laneq_u64(uint64x2_t __a, const int __lane)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_laneq_u64(...) __c17_vdup_laneq_u64(__VA_ARGS__)

__C17_INTRIN uint64_t __c17_vget_lane_u64(uint64x1_t __a, const int __lane)
{
  return __a[__lane];
}
#define vget_lane_u64(...) __c17_vget_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t __c17_vset_lane_u64(uint64_t __a, uint64x1_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vset_lane_u64(...) __c17_vset_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t __c17_vcopy_lane_u64(uint64x1_t __a, const int __la, uint64x1_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_lane_u64(...) __c17_vcopy_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t __c17_vcopy_laneq_u64(uint64x1_t __a, const int __la, uint64x2_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_laneq_u64(...) __c17_vcopy_laneq_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t vdupq_n_u64(uint64_t __a)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint64x2_t vmovq_n_u64(uint64_t __a)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN uint64x2_t __c17_vdupq_lane_u64(uint64x1_t __a, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_lane_u64(...) __c17_vdupq_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vdupq_laneq_u64(uint64x2_t __a, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_laneq_u64(...) __c17_vdupq_laneq_u64(__VA_ARGS__)

__C17_INTRIN uint64_t __c17_vgetq_lane_u64(uint64x2_t __a, const int __lane)
{
  return __a[__lane];
}
#define vgetq_lane_u64(...) __c17_vgetq_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vsetq_lane_u64(uint64_t __a, uint64x2_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vsetq_lane_u64(...) __c17_vsetq_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vcopyq_lane_u64(uint64x2_t __a, const int __la, uint64x1_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_lane_u64(...) __c17_vcopyq_lane_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vcopyq_laneq_u64(uint64x2_t __a, const int __la, uint64x2_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_laneq_u64(...) __c17_vcopyq_laneq_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t vget_low_u64(uint64x2_t __a)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN uint64x1_t vget_high_u64(uint64x2_t __a)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i + 1];
  return __r;
}

__C17_INTRIN uint64x2_t vcombine_u64(uint64x1_t __a, uint64x1_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[__i] : __b[__i - 1];
  return __r;
}

__C17_INTRIN uint64x1_t vcreate_u64(uint64_t __a)
{
  return (uint64x1_t)(uint64x1_t){__a};
}

__C17_INTRIN float32x2_t vdup_n_f32(float32_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN float32x2_t vmov_n_f32(float32_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN float32x2_t __c17_vdup_lane_f32(float32x2_t __a, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_lane_f32(...) __c17_vdup_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vdup_laneq_f32(float32x4_t __a, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_laneq_f32(...) __c17_vdup_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32_t __c17_vget_lane_f32(float32x2_t __a, const int __lane)
{
  return __a[__lane];
}
#define vget_lane_f32(...) __c17_vget_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vset_lane_f32(float32_t __a, float32x2_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vset_lane_f32(...) __c17_vset_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vcopy_lane_f32(float32x2_t __a, const int __la, float32x2_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_lane_f32(...) __c17_vcopy_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vcopy_laneq_f32(float32x2_t __a, const int __la, float32x4_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_laneq_f32(...) __c17_vcopy_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t vdupq_n_f32(float32_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN float32x4_t vmovq_n_f32(float32_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN float32x4_t __c17_vdupq_lane_f32(float32x2_t __a, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_lane_f32(...) __c17_vdupq_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vdupq_laneq_f32(float32x4_t __a, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_laneq_f32(...) __c17_vdupq_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32_t __c17_vgetq_lane_f32(float32x4_t __a, const int __lane)
{
  return __a[__lane];
}
#define vgetq_lane_f32(...) __c17_vgetq_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vsetq_lane_f32(float32_t __a, float32x4_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vsetq_lane_f32(...) __c17_vsetq_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vcopyq_lane_f32(float32x4_t __a, const int __la, float32x2_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_lane_f32(...) __c17_vcopyq_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vcopyq_laneq_f32(float32x4_t __a, const int __la, float32x4_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_laneq_f32(...) __c17_vcopyq_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t vget_low_f32(float32x4_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN float32x2_t vget_high_f32(float32x4_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i + 2];
  return __r;
}

__C17_INTRIN float32x4_t vcombine_f32(float32x2_t __a, float32x2_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[__i] : __b[__i - 2];
  return __r;
}

__C17_INTRIN float32x2_t vcreate_f32(uint64_t __a)
{
  return (float32x2_t)(uint64x1_t){__a};
}

__C17_INTRIN float64x1_t vdup_n_f64(float64_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN float64x1_t vmov_n_f64(float64_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN float64x1_t __c17_vdup_lane_f64(float64x1_t __a, const int __lane)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_lane_f64(...) __c17_vdup_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t __c17_vdup_laneq_f64(float64x2_t __a, const int __lane)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_laneq_f64(...) __c17_vdup_laneq_f64(__VA_ARGS__)

__C17_INTRIN float64_t __c17_vget_lane_f64(float64x1_t __a, const int __lane)
{
  return __a[__lane];
}
#define vget_lane_f64(...) __c17_vget_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t __c17_vset_lane_f64(float64_t __a, float64x1_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vset_lane_f64(...) __c17_vset_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t __c17_vcopy_lane_f64(float64x1_t __a, const int __la, float64x1_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_lane_f64(...) __c17_vcopy_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t __c17_vcopy_laneq_f64(float64x1_t __a, const int __la, float64x2_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_laneq_f64(...) __c17_vcopy_laneq_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t vdupq_n_f64(float64_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN float64x2_t vmovq_n_f64(float64_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN float64x2_t __c17_vdupq_lane_f64(float64x1_t __a, const int __lane)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_lane_f64(...) __c17_vdupq_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t __c17_vdupq_laneq_f64(float64x2_t __a, const int __lane)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_laneq_f64(...) __c17_vdupq_laneq_f64(__VA_ARGS__)

__C17_INTRIN float64_t __c17_vgetq_lane_f64(float64x2_t __a, const int __lane)
{
  return __a[__lane];
}
#define vgetq_lane_f64(...) __c17_vgetq_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t __c17_vsetq_lane_f64(float64_t __a, float64x2_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vsetq_lane_f64(...) __c17_vsetq_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t __c17_vcopyq_lane_f64(float64x2_t __a, const int __la, float64x1_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_lane_f64(...) __c17_vcopyq_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t __c17_vcopyq_laneq_f64(float64x2_t __a, const int __la, float64x2_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_laneq_f64(...) __c17_vcopyq_laneq_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t vget_low_f64(float64x2_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN float64x1_t vget_high_f64(float64x2_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i + 1];
  return __r;
}

__C17_INTRIN float64x2_t vcombine_f64(float64x1_t __a, float64x1_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[__i] : __b[__i - 1];
  return __r;
}

__C17_INTRIN float64x1_t vcreate_f64(uint64_t __a)
{
  return (float64x1_t)(uint64x1_t){__a};
}

__C17_INTRIN poly8x8_t vdup_n_p8(poly8_t __a)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN poly8x8_t vmov_n_p8(poly8_t __a)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN poly8x8_t __c17_vdup_lane_p8(poly8x8_t __a, const int __lane)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_lane_p8(...) __c17_vdup_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x8_t __c17_vdup_laneq_p8(poly8x16_t __a, const int __lane)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_laneq_p8(...) __c17_vdup_laneq_p8(__VA_ARGS__)

__C17_INTRIN poly8_t __c17_vget_lane_p8(poly8x8_t __a, const int __lane)
{
  return __a[__lane];
}
#define vget_lane_p8(...) __c17_vget_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x8_t __c17_vset_lane_p8(poly8_t __a, poly8x8_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vset_lane_p8(...) __c17_vset_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x8_t __c17_vcopy_lane_p8(poly8x8_t __a, const int __la, poly8x8_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_lane_p8(...) __c17_vcopy_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x8_t __c17_vcopy_laneq_p8(poly8x8_t __a, const int __la, poly8x16_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_laneq_p8(...) __c17_vcopy_laneq_p8(__VA_ARGS__)

__C17_INTRIN poly8x16_t vdupq_n_p8(poly8_t __a)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN poly8x16_t vmovq_n_p8(poly8_t __a)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN poly8x16_t __c17_vdupq_lane_p8(poly8x8_t __a, const int __lane)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_lane_p8(...) __c17_vdupq_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x16_t __c17_vdupq_laneq_p8(poly8x16_t __a, const int __lane)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_laneq_p8(...) __c17_vdupq_laneq_p8(__VA_ARGS__)

__C17_INTRIN poly8_t __c17_vgetq_lane_p8(poly8x16_t __a, const int __lane)
{
  return __a[__lane];
}
#define vgetq_lane_p8(...) __c17_vgetq_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x16_t __c17_vsetq_lane_p8(poly8_t __a, poly8x16_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vsetq_lane_p8(...) __c17_vsetq_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x16_t __c17_vcopyq_lane_p8(poly8x16_t __a, const int __la, poly8x8_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_lane_p8(...) __c17_vcopyq_lane_p8(__VA_ARGS__)

__C17_INTRIN poly8x16_t __c17_vcopyq_laneq_p8(poly8x16_t __a, const int __la, poly8x16_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_laneq_p8(...) __c17_vcopyq_laneq_p8(__VA_ARGS__)

__C17_INTRIN poly8x8_t vget_low_p8(poly8x16_t __a)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN poly8x8_t vget_high_p8(poly8x16_t __a)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i + 8];
  return __r;
}

__C17_INTRIN poly8x16_t vcombine_p8(poly8x8_t __a, poly8x8_t __b)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? __a[__i] : __b[__i - 8];
  return __r;
}

__C17_INTRIN poly8x8_t vcreate_p8(uint64_t __a)
{
  return (poly8x8_t)(uint64x1_t){__a};
}

__C17_INTRIN poly16x4_t vdup_n_p16(poly16_t __a)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN poly16x4_t vmov_n_p16(poly16_t __a)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN poly16x4_t __c17_vdup_lane_p16(poly16x4_t __a, const int __lane)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_lane_p16(...) __c17_vdup_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x4_t __c17_vdup_laneq_p16(poly16x8_t __a, const int __lane)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdup_laneq_p16(...) __c17_vdup_laneq_p16(__VA_ARGS__)

__C17_INTRIN poly16_t __c17_vget_lane_p16(poly16x4_t __a, const int __lane)
{
  return __a[__lane];
}
#define vget_lane_p16(...) __c17_vget_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x4_t __c17_vset_lane_p16(poly16_t __a, poly16x4_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vset_lane_p16(...) __c17_vset_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x4_t __c17_vcopy_lane_p16(poly16x4_t __a, const int __la, poly16x4_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_lane_p16(...) __c17_vcopy_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x4_t __c17_vcopy_laneq_p16(poly16x4_t __a, const int __la, poly16x8_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopy_laneq_p16(...) __c17_vcopy_laneq_p16(__VA_ARGS__)

__C17_INTRIN poly16x8_t vdupq_n_p16(poly16_t __a)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN poly16x8_t vmovq_n_p16(poly16_t __a)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a;
  return __r;
}

__C17_INTRIN poly16x8_t __c17_vdupq_lane_p16(poly16x4_t __a, const int __lane)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_lane_p16(...) __c17_vdupq_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x8_t __c17_vdupq_laneq_p16(poly16x8_t __a, const int __lane)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__lane];
  return __r;
}
#define vdupq_laneq_p16(...) __c17_vdupq_laneq_p16(__VA_ARGS__)

__C17_INTRIN poly16_t __c17_vgetq_lane_p16(poly16x8_t __a, const int __lane)
{
  return __a[__lane];
}
#define vgetq_lane_p16(...) __c17_vgetq_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x8_t __c17_vsetq_lane_p16(poly16_t __a, poly16x8_t __b, const int __lane)
{
  __b[__lane] = __a;
  return __b;
}
#define vsetq_lane_p16(...) __c17_vsetq_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x8_t __c17_vcopyq_lane_p16(poly16x8_t __a, const int __la, poly16x4_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_lane_p16(...) __c17_vcopyq_lane_p16(__VA_ARGS__)

__C17_INTRIN poly16x8_t __c17_vcopyq_laneq_p16(poly16x8_t __a, const int __la, poly16x8_t __b, const int __lb)
{
  __a[__la] = __b[__lb];
  return __a;
}
#define vcopyq_laneq_p16(...) __c17_vcopyq_laneq_p16(__VA_ARGS__)

__C17_INTRIN poly16x4_t vget_low_p16(poly16x8_t __a)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN poly16x4_t vget_high_p16(poly16x8_t __a)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i + 4];
  return __r;
}

__C17_INTRIN poly16x8_t vcombine_p16(poly16x4_t __a, poly16x4_t __b)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[__i] : __b[__i - 4];
  return __r;
}

__C17_INTRIN poly16x4_t vcreate_p16(uint64_t __a)
{
  return (poly16x4_t)(uint64x1_t){__a};
}

__C17_INTRIN int8x8_t vreinterpret_s8_s16(int16x4_t __a)
{
  return (int8x8_t)__a;
}

__C17_INTRIN int8x8_t vreinterpret_s8_s32(int32x2_t __a)
{
  return (int8x8_t)__a;
}

__C17_INTRIN int8x8_t vreinterpret_s8_s64(int64x1_t __a)
{
  return (int8x8_t)__a;
}

__C17_INTRIN int8x8_t vreinterpret_s8_u8(uint8x8_t __a)
{
  return (int8x8_t)__a;
}

__C17_INTRIN int8x8_t vreinterpret_s8_u16(uint16x4_t __a)
{
  return (int8x8_t)__a;
}

__C17_INTRIN int8x8_t vreinterpret_s8_u32(uint32x2_t __a)
{
  return (int8x8_t)__a;
}

__C17_INTRIN int8x8_t vreinterpret_s8_u64(uint64x1_t __a)
{
  return (int8x8_t)__a;
}

__C17_INTRIN int8x8_t vreinterpret_s8_f32(float32x2_t __a)
{
  return (int8x8_t)__a;
}

__C17_INTRIN int8x8_t vreinterpret_s8_f64(float64x1_t __a)
{
  return (int8x8_t)__a;
}

__C17_INTRIN int8x8_t vreinterpret_s8_p8(poly8x8_t __a)
{
  return (int8x8_t)__a;
}

__C17_INTRIN int8x8_t vreinterpret_s8_p16(poly16x4_t __a)
{
  return (int8x8_t)__a;
}

__C17_INTRIN int16x4_t vreinterpret_s16_s8(int8x8_t __a)
{
  return (int16x4_t)__a;
}

__C17_INTRIN int16x4_t vreinterpret_s16_s32(int32x2_t __a)
{
  return (int16x4_t)__a;
}

__C17_INTRIN int16x4_t vreinterpret_s16_s64(int64x1_t __a)
{
  return (int16x4_t)__a;
}

__C17_INTRIN int16x4_t vreinterpret_s16_u8(uint8x8_t __a)
{
  return (int16x4_t)__a;
}

__C17_INTRIN int16x4_t vreinterpret_s16_u16(uint16x4_t __a)
{
  return (int16x4_t)__a;
}

__C17_INTRIN int16x4_t vreinterpret_s16_u32(uint32x2_t __a)
{
  return (int16x4_t)__a;
}

__C17_INTRIN int16x4_t vreinterpret_s16_u64(uint64x1_t __a)
{
  return (int16x4_t)__a;
}

__C17_INTRIN int16x4_t vreinterpret_s16_f32(float32x2_t __a)
{
  return (int16x4_t)__a;
}

__C17_INTRIN int16x4_t vreinterpret_s16_f64(float64x1_t __a)
{
  return (int16x4_t)__a;
}

__C17_INTRIN int16x4_t vreinterpret_s16_p8(poly8x8_t __a)
{
  return (int16x4_t)__a;
}

__C17_INTRIN int16x4_t vreinterpret_s16_p16(poly16x4_t __a)
{
  return (int16x4_t)__a;
}

__C17_INTRIN int32x2_t vreinterpret_s32_s8(int8x8_t __a)
{
  return (int32x2_t)__a;
}

__C17_INTRIN int32x2_t vreinterpret_s32_s16(int16x4_t __a)
{
  return (int32x2_t)__a;
}

__C17_INTRIN int32x2_t vreinterpret_s32_s64(int64x1_t __a)
{
  return (int32x2_t)__a;
}

__C17_INTRIN int32x2_t vreinterpret_s32_u8(uint8x8_t __a)
{
  return (int32x2_t)__a;
}

__C17_INTRIN int32x2_t vreinterpret_s32_u16(uint16x4_t __a)
{
  return (int32x2_t)__a;
}

__C17_INTRIN int32x2_t vreinterpret_s32_u32(uint32x2_t __a)
{
  return (int32x2_t)__a;
}

__C17_INTRIN int32x2_t vreinterpret_s32_u64(uint64x1_t __a)
{
  return (int32x2_t)__a;
}

__C17_INTRIN int32x2_t vreinterpret_s32_f32(float32x2_t __a)
{
  return (int32x2_t)__a;
}

__C17_INTRIN int32x2_t vreinterpret_s32_f64(float64x1_t __a)
{
  return (int32x2_t)__a;
}

__C17_INTRIN int32x2_t vreinterpret_s32_p8(poly8x8_t __a)
{
  return (int32x2_t)__a;
}

__C17_INTRIN int32x2_t vreinterpret_s32_p16(poly16x4_t __a)
{
  return (int32x2_t)__a;
}

__C17_INTRIN int64x1_t vreinterpret_s64_s8(int8x8_t __a)
{
  return (int64x1_t)__a;
}

__C17_INTRIN int64x1_t vreinterpret_s64_s16(int16x4_t __a)
{
  return (int64x1_t)__a;
}

__C17_INTRIN int64x1_t vreinterpret_s64_s32(int32x2_t __a)
{
  return (int64x1_t)__a;
}

__C17_INTRIN int64x1_t vreinterpret_s64_u8(uint8x8_t __a)
{
  return (int64x1_t)__a;
}

__C17_INTRIN int64x1_t vreinterpret_s64_u16(uint16x4_t __a)
{
  return (int64x1_t)__a;
}

__C17_INTRIN int64x1_t vreinterpret_s64_u32(uint32x2_t __a)
{
  return (int64x1_t)__a;
}

__C17_INTRIN int64x1_t vreinterpret_s64_u64(uint64x1_t __a)
{
  return (int64x1_t)__a;
}

__C17_INTRIN int64x1_t vreinterpret_s64_f32(float32x2_t __a)
{
  return (int64x1_t)__a;
}

__C17_INTRIN int64x1_t vreinterpret_s64_f64(float64x1_t __a)
{
  return (int64x1_t)__a;
}

__C17_INTRIN int64x1_t vreinterpret_s64_p8(poly8x8_t __a)
{
  return (int64x1_t)__a;
}

__C17_INTRIN int64x1_t vreinterpret_s64_p16(poly16x4_t __a)
{
  return (int64x1_t)__a;
}

__C17_INTRIN uint8x8_t vreinterpret_u8_s8(int8x8_t __a)
{
  return (uint8x8_t)__a;
}

__C17_INTRIN uint8x8_t vreinterpret_u8_s16(int16x4_t __a)
{
  return (uint8x8_t)__a;
}

__C17_INTRIN uint8x8_t vreinterpret_u8_s32(int32x2_t __a)
{
  return (uint8x8_t)__a;
}

__C17_INTRIN uint8x8_t vreinterpret_u8_s64(int64x1_t __a)
{
  return (uint8x8_t)__a;
}

__C17_INTRIN uint8x8_t vreinterpret_u8_u16(uint16x4_t __a)
{
  return (uint8x8_t)__a;
}

__C17_INTRIN uint8x8_t vreinterpret_u8_u32(uint32x2_t __a)
{
  return (uint8x8_t)__a;
}

__C17_INTRIN uint8x8_t vreinterpret_u8_u64(uint64x1_t __a)
{
  return (uint8x8_t)__a;
}

__C17_INTRIN uint8x8_t vreinterpret_u8_f32(float32x2_t __a)
{
  return (uint8x8_t)__a;
}

__C17_INTRIN uint8x8_t vreinterpret_u8_f64(float64x1_t __a)
{
  return (uint8x8_t)__a;
}

__C17_INTRIN uint8x8_t vreinterpret_u8_p8(poly8x8_t __a)
{
  return (uint8x8_t)__a;
}

__C17_INTRIN uint8x8_t vreinterpret_u8_p16(poly16x4_t __a)
{
  return (uint8x8_t)__a;
}

__C17_INTRIN uint16x4_t vreinterpret_u16_s8(int8x8_t __a)
{
  return (uint16x4_t)__a;
}

__C17_INTRIN uint16x4_t vreinterpret_u16_s16(int16x4_t __a)
{
  return (uint16x4_t)__a;
}

__C17_INTRIN uint16x4_t vreinterpret_u16_s32(int32x2_t __a)
{
  return (uint16x4_t)__a;
}

__C17_INTRIN uint16x4_t vreinterpret_u16_s64(int64x1_t __a)
{
  return (uint16x4_t)__a;
}

__C17_INTRIN uint16x4_t vreinterpret_u16_u8(uint8x8_t __a)
{
  return (uint16x4_t)__a;
}

__C17_INTRIN uint16x4_t vreinterpret_u16_u32(uint32x2_t __a)
{
  return (uint16x4_t)__a;
}

__C17_INTRIN uint16x4_t vreinterpret_u16_u64(uint64x1_t __a)
{
  return (uint16x4_t)__a;
}

__C17_INTRIN uint16x4_t vreinterpret_u16_f32(float32x2_t __a)
{
  return (uint16x4_t)__a;
}

__C17_INTRIN uint16x4_t vreinterpret_u16_f64(float64x1_t __a)
{
  return (uint16x4_t)__a;
}

__C17_INTRIN uint16x4_t vreinterpret_u16_p8(poly8x8_t __a)
{
  return (uint16x4_t)__a;
}

__C17_INTRIN uint16x4_t vreinterpret_u16_p16(poly16x4_t __a)
{
  return (uint16x4_t)__a;
}

__C17_INTRIN uint32x2_t vreinterpret_u32_s8(int8x8_t __a)
{
  return (uint32x2_t)__a;
}

__C17_INTRIN uint32x2_t vreinterpret_u32_s16(int16x4_t __a)
{
  return (uint32x2_t)__a;
}

__C17_INTRIN uint32x2_t vreinterpret_u32_s32(int32x2_t __a)
{
  return (uint32x2_t)__a;
}

__C17_INTRIN uint32x2_t vreinterpret_u32_s64(int64x1_t __a)
{
  return (uint32x2_t)__a;
}

__C17_INTRIN uint32x2_t vreinterpret_u32_u8(uint8x8_t __a)
{
  return (uint32x2_t)__a;
}

__C17_INTRIN uint32x2_t vreinterpret_u32_u16(uint16x4_t __a)
{
  return (uint32x2_t)__a;
}

__C17_INTRIN uint32x2_t vreinterpret_u32_u64(uint64x1_t __a)
{
  return (uint32x2_t)__a;
}

__C17_INTRIN uint32x2_t vreinterpret_u32_f32(float32x2_t __a)
{
  return (uint32x2_t)__a;
}

__C17_INTRIN uint32x2_t vreinterpret_u32_f64(float64x1_t __a)
{
  return (uint32x2_t)__a;
}

__C17_INTRIN uint32x2_t vreinterpret_u32_p8(poly8x8_t __a)
{
  return (uint32x2_t)__a;
}

__C17_INTRIN uint32x2_t vreinterpret_u32_p16(poly16x4_t __a)
{
  return (uint32x2_t)__a;
}

__C17_INTRIN uint64x1_t vreinterpret_u64_s8(int8x8_t __a)
{
  return (uint64x1_t)__a;
}

__C17_INTRIN uint64x1_t vreinterpret_u64_s16(int16x4_t __a)
{
  return (uint64x1_t)__a;
}

__C17_INTRIN uint64x1_t vreinterpret_u64_s32(int32x2_t __a)
{
  return (uint64x1_t)__a;
}

__C17_INTRIN uint64x1_t vreinterpret_u64_s64(int64x1_t __a)
{
  return (uint64x1_t)__a;
}

__C17_INTRIN uint64x1_t vreinterpret_u64_u8(uint8x8_t __a)
{
  return (uint64x1_t)__a;
}

__C17_INTRIN uint64x1_t vreinterpret_u64_u16(uint16x4_t __a)
{
  return (uint64x1_t)__a;
}

__C17_INTRIN uint64x1_t vreinterpret_u64_u32(uint32x2_t __a)
{
  return (uint64x1_t)__a;
}

__C17_INTRIN uint64x1_t vreinterpret_u64_f32(float32x2_t __a)
{
  return (uint64x1_t)__a;
}

__C17_INTRIN uint64x1_t vreinterpret_u64_f64(float64x1_t __a)
{
  return (uint64x1_t)__a;
}

__C17_INTRIN uint64x1_t vreinterpret_u64_p8(poly8x8_t __a)
{
  return (uint64x1_t)__a;
}

__C17_INTRIN uint64x1_t vreinterpret_u64_p16(poly16x4_t __a)
{
  return (uint64x1_t)__a;
}

__C17_INTRIN float32x2_t vreinterpret_f32_s8(int8x8_t __a)
{
  return (float32x2_t)__a;
}

__C17_INTRIN float32x2_t vreinterpret_f32_s16(int16x4_t __a)
{
  return (float32x2_t)__a;
}

__C17_INTRIN float32x2_t vreinterpret_f32_s32(int32x2_t __a)
{
  return (float32x2_t)__a;
}

__C17_INTRIN float32x2_t vreinterpret_f32_s64(int64x1_t __a)
{
  return (float32x2_t)__a;
}

__C17_INTRIN float32x2_t vreinterpret_f32_u8(uint8x8_t __a)
{
  return (float32x2_t)__a;
}

__C17_INTRIN float32x2_t vreinterpret_f32_u16(uint16x4_t __a)
{
  return (float32x2_t)__a;
}

__C17_INTRIN float32x2_t vreinterpret_f32_u32(uint32x2_t __a)
{
  return (float32x2_t)__a;
}

__C17_INTRIN float32x2_t vreinterpret_f32_u64(uint64x1_t __a)
{
  return (float32x2_t)__a;
}

__C17_INTRIN float32x2_t vreinterpret_f32_f64(float64x1_t __a)
{
  return (float32x2_t)__a;
}

__C17_INTRIN float32x2_t vreinterpret_f32_p8(poly8x8_t __a)
{
  return (float32x2_t)__a;
}

__C17_INTRIN float32x2_t vreinterpret_f32_p16(poly16x4_t __a)
{
  return (float32x2_t)__a;
}

__C17_INTRIN float64x1_t vreinterpret_f64_s8(int8x8_t __a)
{
  return (float64x1_t)__a;
}

__C17_INTRIN float64x1_t vreinterpret_f64_s16(int16x4_t __a)
{
  return (float64x1_t)__a;
}

__C17_INTRIN float64x1_t vreinterpret_f64_s32(int32x2_t __a)
{
  return (float64x1_t)__a;
}

__C17_INTRIN float64x1_t vreinterpret_f64_s64(int64x1_t __a)
{
  return (float64x1_t)__a;
}

__C17_INTRIN float64x1_t vreinterpret_f64_u8(uint8x8_t __a)
{
  return (float64x1_t)__a;
}

__C17_INTRIN float64x1_t vreinterpret_f64_u16(uint16x4_t __a)
{
  return (float64x1_t)__a;
}

__C17_INTRIN float64x1_t vreinterpret_f64_u32(uint32x2_t __a)
{
  return (float64x1_t)__a;
}

__C17_INTRIN float64x1_t vreinterpret_f64_u64(uint64x1_t __a)
{
  return (float64x1_t)__a;
}

__C17_INTRIN float64x1_t vreinterpret_f64_f32(float32x2_t __a)
{
  return (float64x1_t)__a;
}

__C17_INTRIN float64x1_t vreinterpret_f64_p8(poly8x8_t __a)
{
  return (float64x1_t)__a;
}

__C17_INTRIN float64x1_t vreinterpret_f64_p16(poly16x4_t __a)
{
  return (float64x1_t)__a;
}

__C17_INTRIN poly8x8_t vreinterpret_p8_s8(int8x8_t __a)
{
  return (poly8x8_t)__a;
}

__C17_INTRIN poly8x8_t vreinterpret_p8_s16(int16x4_t __a)
{
  return (poly8x8_t)__a;
}

__C17_INTRIN poly8x8_t vreinterpret_p8_s32(int32x2_t __a)
{
  return (poly8x8_t)__a;
}

__C17_INTRIN poly8x8_t vreinterpret_p8_s64(int64x1_t __a)
{
  return (poly8x8_t)__a;
}

__C17_INTRIN poly8x8_t vreinterpret_p8_u8(uint8x8_t __a)
{
  return (poly8x8_t)__a;
}

__C17_INTRIN poly8x8_t vreinterpret_p8_u16(uint16x4_t __a)
{
  return (poly8x8_t)__a;
}

__C17_INTRIN poly8x8_t vreinterpret_p8_u32(uint32x2_t __a)
{
  return (poly8x8_t)__a;
}

__C17_INTRIN poly8x8_t vreinterpret_p8_u64(uint64x1_t __a)
{
  return (poly8x8_t)__a;
}

__C17_INTRIN poly8x8_t vreinterpret_p8_f32(float32x2_t __a)
{
  return (poly8x8_t)__a;
}

__C17_INTRIN poly8x8_t vreinterpret_p8_f64(float64x1_t __a)
{
  return (poly8x8_t)__a;
}

__C17_INTRIN poly8x8_t vreinterpret_p8_p16(poly16x4_t __a)
{
  return (poly8x8_t)__a;
}

__C17_INTRIN poly16x4_t vreinterpret_p16_s8(int8x8_t __a)
{
  return (poly16x4_t)__a;
}

__C17_INTRIN poly16x4_t vreinterpret_p16_s16(int16x4_t __a)
{
  return (poly16x4_t)__a;
}

__C17_INTRIN poly16x4_t vreinterpret_p16_s32(int32x2_t __a)
{
  return (poly16x4_t)__a;
}

__C17_INTRIN poly16x4_t vreinterpret_p16_s64(int64x1_t __a)
{
  return (poly16x4_t)__a;
}

__C17_INTRIN poly16x4_t vreinterpret_p16_u8(uint8x8_t __a)
{
  return (poly16x4_t)__a;
}

__C17_INTRIN poly16x4_t vreinterpret_p16_u16(uint16x4_t __a)
{
  return (poly16x4_t)__a;
}

__C17_INTRIN poly16x4_t vreinterpret_p16_u32(uint32x2_t __a)
{
  return (poly16x4_t)__a;
}

__C17_INTRIN poly16x4_t vreinterpret_p16_u64(uint64x1_t __a)
{
  return (poly16x4_t)__a;
}

__C17_INTRIN poly16x4_t vreinterpret_p16_f32(float32x2_t __a)
{
  return (poly16x4_t)__a;
}

__C17_INTRIN poly16x4_t vreinterpret_p16_f64(float64x1_t __a)
{
  return (poly16x4_t)__a;
}

__C17_INTRIN poly16x4_t vreinterpret_p16_p8(poly8x8_t __a)
{
  return (poly16x4_t)__a;
}

__C17_INTRIN int8x16_t vreinterpretq_s8_s16(int16x8_t __a)
{
  return (int8x16_t)__a;
}

__C17_INTRIN int8x16_t vreinterpretq_s8_s32(int32x4_t __a)
{
  return (int8x16_t)__a;
}

__C17_INTRIN int8x16_t vreinterpretq_s8_s64(int64x2_t __a)
{
  return (int8x16_t)__a;
}

__C17_INTRIN int8x16_t vreinterpretq_s8_u8(uint8x16_t __a)
{
  return (int8x16_t)__a;
}

__C17_INTRIN int8x16_t vreinterpretq_s8_u16(uint16x8_t __a)
{
  return (int8x16_t)__a;
}

__C17_INTRIN int8x16_t vreinterpretq_s8_u32(uint32x4_t __a)
{
  return (int8x16_t)__a;
}

__C17_INTRIN int8x16_t vreinterpretq_s8_u64(uint64x2_t __a)
{
  return (int8x16_t)__a;
}

__C17_INTRIN int8x16_t vreinterpretq_s8_f32(float32x4_t __a)
{
  return (int8x16_t)__a;
}

__C17_INTRIN int8x16_t vreinterpretq_s8_f64(float64x2_t __a)
{
  return (int8x16_t)__a;
}

__C17_INTRIN int8x16_t vreinterpretq_s8_p8(poly8x16_t __a)
{
  return (int8x16_t)__a;
}

__C17_INTRIN int8x16_t vreinterpretq_s8_p16(poly16x8_t __a)
{
  return (int8x16_t)__a;
}

__C17_INTRIN int16x8_t vreinterpretq_s16_s8(int8x16_t __a)
{
  return (int16x8_t)__a;
}

__C17_INTRIN int16x8_t vreinterpretq_s16_s32(int32x4_t __a)
{
  return (int16x8_t)__a;
}

__C17_INTRIN int16x8_t vreinterpretq_s16_s64(int64x2_t __a)
{
  return (int16x8_t)__a;
}

__C17_INTRIN int16x8_t vreinterpretq_s16_u8(uint8x16_t __a)
{
  return (int16x8_t)__a;
}

__C17_INTRIN int16x8_t vreinterpretq_s16_u16(uint16x8_t __a)
{
  return (int16x8_t)__a;
}

__C17_INTRIN int16x8_t vreinterpretq_s16_u32(uint32x4_t __a)
{
  return (int16x8_t)__a;
}

__C17_INTRIN int16x8_t vreinterpretq_s16_u64(uint64x2_t __a)
{
  return (int16x8_t)__a;
}

__C17_INTRIN int16x8_t vreinterpretq_s16_f32(float32x4_t __a)
{
  return (int16x8_t)__a;
}

__C17_INTRIN int16x8_t vreinterpretq_s16_f64(float64x2_t __a)
{
  return (int16x8_t)__a;
}

__C17_INTRIN int16x8_t vreinterpretq_s16_p8(poly8x16_t __a)
{
  return (int16x8_t)__a;
}

__C17_INTRIN int16x8_t vreinterpretq_s16_p16(poly16x8_t __a)
{
  return (int16x8_t)__a;
}

__C17_INTRIN int32x4_t vreinterpretq_s32_s8(int8x16_t __a)
{
  return (int32x4_t)__a;
}

__C17_INTRIN int32x4_t vreinterpretq_s32_s16(int16x8_t __a)
{
  return (int32x4_t)__a;
}

__C17_INTRIN int32x4_t vreinterpretq_s32_s64(int64x2_t __a)
{
  return (int32x4_t)__a;
}

__C17_INTRIN int32x4_t vreinterpretq_s32_u8(uint8x16_t __a)
{
  return (int32x4_t)__a;
}

__C17_INTRIN int32x4_t vreinterpretq_s32_u16(uint16x8_t __a)
{
  return (int32x4_t)__a;
}

__C17_INTRIN int32x4_t vreinterpretq_s32_u32(uint32x4_t __a)
{
  return (int32x4_t)__a;
}

__C17_INTRIN int32x4_t vreinterpretq_s32_u64(uint64x2_t __a)
{
  return (int32x4_t)__a;
}

__C17_INTRIN int32x4_t vreinterpretq_s32_f32(float32x4_t __a)
{
  return (int32x4_t)__a;
}

__C17_INTRIN int32x4_t vreinterpretq_s32_f64(float64x2_t __a)
{
  return (int32x4_t)__a;
}

__C17_INTRIN int32x4_t vreinterpretq_s32_p8(poly8x16_t __a)
{
  return (int32x4_t)__a;
}

__C17_INTRIN int32x4_t vreinterpretq_s32_p16(poly16x8_t __a)
{
  return (int32x4_t)__a;
}

__C17_INTRIN int64x2_t vreinterpretq_s64_s8(int8x16_t __a)
{
  return (int64x2_t)__a;
}

__C17_INTRIN int64x2_t vreinterpretq_s64_s16(int16x8_t __a)
{
  return (int64x2_t)__a;
}

__C17_INTRIN int64x2_t vreinterpretq_s64_s32(int32x4_t __a)
{
  return (int64x2_t)__a;
}

__C17_INTRIN int64x2_t vreinterpretq_s64_u8(uint8x16_t __a)
{
  return (int64x2_t)__a;
}

__C17_INTRIN int64x2_t vreinterpretq_s64_u16(uint16x8_t __a)
{
  return (int64x2_t)__a;
}

__C17_INTRIN int64x2_t vreinterpretq_s64_u32(uint32x4_t __a)
{
  return (int64x2_t)__a;
}

__C17_INTRIN int64x2_t vreinterpretq_s64_u64(uint64x2_t __a)
{
  return (int64x2_t)__a;
}

__C17_INTRIN int64x2_t vreinterpretq_s64_f32(float32x4_t __a)
{
  return (int64x2_t)__a;
}

__C17_INTRIN int64x2_t vreinterpretq_s64_f64(float64x2_t __a)
{
  return (int64x2_t)__a;
}

__C17_INTRIN int64x2_t vreinterpretq_s64_p8(poly8x16_t __a)
{
  return (int64x2_t)__a;
}

__C17_INTRIN int64x2_t vreinterpretq_s64_p16(poly16x8_t __a)
{
  return (int64x2_t)__a;
}

__C17_INTRIN uint8x16_t vreinterpretq_u8_s8(int8x16_t __a)
{
  return (uint8x16_t)__a;
}

__C17_INTRIN uint8x16_t vreinterpretq_u8_s16(int16x8_t __a)
{
  return (uint8x16_t)__a;
}

__C17_INTRIN uint8x16_t vreinterpretq_u8_s32(int32x4_t __a)
{
  return (uint8x16_t)__a;
}

__C17_INTRIN uint8x16_t vreinterpretq_u8_s64(int64x2_t __a)
{
  return (uint8x16_t)__a;
}

__C17_INTRIN uint8x16_t vreinterpretq_u8_u16(uint16x8_t __a)
{
  return (uint8x16_t)__a;
}

__C17_INTRIN uint8x16_t vreinterpretq_u8_u32(uint32x4_t __a)
{
  return (uint8x16_t)__a;
}

__C17_INTRIN uint8x16_t vreinterpretq_u8_u64(uint64x2_t __a)
{
  return (uint8x16_t)__a;
}

__C17_INTRIN uint8x16_t vreinterpretq_u8_f32(float32x4_t __a)
{
  return (uint8x16_t)__a;
}

__C17_INTRIN uint8x16_t vreinterpretq_u8_f64(float64x2_t __a)
{
  return (uint8x16_t)__a;
}

__C17_INTRIN uint8x16_t vreinterpretq_u8_p8(poly8x16_t __a)
{
  return (uint8x16_t)__a;
}

__C17_INTRIN uint8x16_t vreinterpretq_u8_p16(poly16x8_t __a)
{
  return (uint8x16_t)__a;
}

__C17_INTRIN uint16x8_t vreinterpretq_u16_s8(int8x16_t __a)
{
  return (uint16x8_t)__a;
}

__C17_INTRIN uint16x8_t vreinterpretq_u16_s16(int16x8_t __a)
{
  return (uint16x8_t)__a;
}

__C17_INTRIN uint16x8_t vreinterpretq_u16_s32(int32x4_t __a)
{
  return (uint16x8_t)__a;
}

__C17_INTRIN uint16x8_t vreinterpretq_u16_s64(int64x2_t __a)
{
  return (uint16x8_t)__a;
}

__C17_INTRIN uint16x8_t vreinterpretq_u16_u8(uint8x16_t __a)
{
  return (uint16x8_t)__a;
}

__C17_INTRIN uint16x8_t vreinterpretq_u16_u32(uint32x4_t __a)
{
  return (uint16x8_t)__a;
}

__C17_INTRIN uint16x8_t vreinterpretq_u16_u64(uint64x2_t __a)
{
  return (uint16x8_t)__a;
}

__C17_INTRIN uint16x8_t vreinterpretq_u16_f32(float32x4_t __a)
{
  return (uint16x8_t)__a;
}

__C17_INTRIN uint16x8_t vreinterpretq_u16_f64(float64x2_t __a)
{
  return (uint16x8_t)__a;
}

__C17_INTRIN uint16x8_t vreinterpretq_u16_p8(poly8x16_t __a)
{
  return (uint16x8_t)__a;
}

__C17_INTRIN uint16x8_t vreinterpretq_u16_p16(poly16x8_t __a)
{
  return (uint16x8_t)__a;
}

__C17_INTRIN uint32x4_t vreinterpretq_u32_s8(int8x16_t __a)
{
  return (uint32x4_t)__a;
}

__C17_INTRIN uint32x4_t vreinterpretq_u32_s16(int16x8_t __a)
{
  return (uint32x4_t)__a;
}

__C17_INTRIN uint32x4_t vreinterpretq_u32_s32(int32x4_t __a)
{
  return (uint32x4_t)__a;
}

__C17_INTRIN uint32x4_t vreinterpretq_u32_s64(int64x2_t __a)
{
  return (uint32x4_t)__a;
}

__C17_INTRIN uint32x4_t vreinterpretq_u32_u8(uint8x16_t __a)
{
  return (uint32x4_t)__a;
}

__C17_INTRIN uint32x4_t vreinterpretq_u32_u16(uint16x8_t __a)
{
  return (uint32x4_t)__a;
}

__C17_INTRIN uint32x4_t vreinterpretq_u32_u64(uint64x2_t __a)
{
  return (uint32x4_t)__a;
}

__C17_INTRIN uint32x4_t vreinterpretq_u32_f32(float32x4_t __a)
{
  return (uint32x4_t)__a;
}

__C17_INTRIN uint32x4_t vreinterpretq_u32_f64(float64x2_t __a)
{
  return (uint32x4_t)__a;
}

__C17_INTRIN uint32x4_t vreinterpretq_u32_p8(poly8x16_t __a)
{
  return (uint32x4_t)__a;
}

__C17_INTRIN uint32x4_t vreinterpretq_u32_p16(poly16x8_t __a)
{
  return (uint32x4_t)__a;
}

__C17_INTRIN uint64x2_t vreinterpretq_u64_s8(int8x16_t __a)
{
  return (uint64x2_t)__a;
}

__C17_INTRIN uint64x2_t vreinterpretq_u64_s16(int16x8_t __a)
{
  return (uint64x2_t)__a;
}

__C17_INTRIN uint64x2_t vreinterpretq_u64_s32(int32x4_t __a)
{
  return (uint64x2_t)__a;
}

__C17_INTRIN uint64x2_t vreinterpretq_u64_s64(int64x2_t __a)
{
  return (uint64x2_t)__a;
}

__C17_INTRIN uint64x2_t vreinterpretq_u64_u8(uint8x16_t __a)
{
  return (uint64x2_t)__a;
}

__C17_INTRIN uint64x2_t vreinterpretq_u64_u16(uint16x8_t __a)
{
  return (uint64x2_t)__a;
}

__C17_INTRIN uint64x2_t vreinterpretq_u64_u32(uint32x4_t __a)
{
  return (uint64x2_t)__a;
}

__C17_INTRIN uint64x2_t vreinterpretq_u64_f32(float32x4_t __a)
{
  return (uint64x2_t)__a;
}

__C17_INTRIN uint64x2_t vreinterpretq_u64_f64(float64x2_t __a)
{
  return (uint64x2_t)__a;
}

__C17_INTRIN uint64x2_t vreinterpretq_u64_p8(poly8x16_t __a)
{
  return (uint64x2_t)__a;
}

__C17_INTRIN uint64x2_t vreinterpretq_u64_p16(poly16x8_t __a)
{
  return (uint64x2_t)__a;
}

__C17_INTRIN float32x4_t vreinterpretq_f32_s8(int8x16_t __a)
{
  return (float32x4_t)__a;
}

__C17_INTRIN float32x4_t vreinterpretq_f32_s16(int16x8_t __a)
{
  return (float32x4_t)__a;
}

__C17_INTRIN float32x4_t vreinterpretq_f32_s32(int32x4_t __a)
{
  return (float32x4_t)__a;
}

__C17_INTRIN float32x4_t vreinterpretq_f32_s64(int64x2_t __a)
{
  return (float32x4_t)__a;
}

__C17_INTRIN float32x4_t vreinterpretq_f32_u8(uint8x16_t __a)
{
  return (float32x4_t)__a;
}

__C17_INTRIN float32x4_t vreinterpretq_f32_u16(uint16x8_t __a)
{
  return (float32x4_t)__a;
}

__C17_INTRIN float32x4_t vreinterpretq_f32_u32(uint32x4_t __a)
{
  return (float32x4_t)__a;
}

__C17_INTRIN float32x4_t vreinterpretq_f32_u64(uint64x2_t __a)
{
  return (float32x4_t)__a;
}

__C17_INTRIN float32x4_t vreinterpretq_f32_f64(float64x2_t __a)
{
  return (float32x4_t)__a;
}

__C17_INTRIN float32x4_t vreinterpretq_f32_p8(poly8x16_t __a)
{
  return (float32x4_t)__a;
}

__C17_INTRIN float32x4_t vreinterpretq_f32_p16(poly16x8_t __a)
{
  return (float32x4_t)__a;
}

__C17_INTRIN float64x2_t vreinterpretq_f64_s8(int8x16_t __a)
{
  return (float64x2_t)__a;
}

__C17_INTRIN float64x2_t vreinterpretq_f64_s16(int16x8_t __a)
{
  return (float64x2_t)__a;
}

__C17_INTRIN float64x2_t vreinterpretq_f64_s32(int32x4_t __a)
{
  return (float64x2_t)__a;
}

__C17_INTRIN float64x2_t vreinterpretq_f64_s64(int64x2_t __a)
{
  return (float64x2_t)__a;
}

__C17_INTRIN float64x2_t vreinterpretq_f64_u8(uint8x16_t __a)
{
  return (float64x2_t)__a;
}

__C17_INTRIN float64x2_t vreinterpretq_f64_u16(uint16x8_t __a)
{
  return (float64x2_t)__a;
}

__C17_INTRIN float64x2_t vreinterpretq_f64_u32(uint32x4_t __a)
{
  return (float64x2_t)__a;
}

__C17_INTRIN float64x2_t vreinterpretq_f64_u64(uint64x2_t __a)
{
  return (float64x2_t)__a;
}

__C17_INTRIN float64x2_t vreinterpretq_f64_f32(float32x4_t __a)
{
  return (float64x2_t)__a;
}

__C17_INTRIN float64x2_t vreinterpretq_f64_p8(poly8x16_t __a)
{
  return (float64x2_t)__a;
}

__C17_INTRIN float64x2_t vreinterpretq_f64_p16(poly16x8_t __a)
{
  return (float64x2_t)__a;
}

__C17_INTRIN poly8x16_t vreinterpretq_p8_s8(int8x16_t __a)
{
  return (poly8x16_t)__a;
}

__C17_INTRIN poly8x16_t vreinterpretq_p8_s16(int16x8_t __a)
{
  return (poly8x16_t)__a;
}

__C17_INTRIN poly8x16_t vreinterpretq_p8_s32(int32x4_t __a)
{
  return (poly8x16_t)__a;
}

__C17_INTRIN poly8x16_t vreinterpretq_p8_s64(int64x2_t __a)
{
  return (poly8x16_t)__a;
}

__C17_INTRIN poly8x16_t vreinterpretq_p8_u8(uint8x16_t __a)
{
  return (poly8x16_t)__a;
}

__C17_INTRIN poly8x16_t vreinterpretq_p8_u16(uint16x8_t __a)
{
  return (poly8x16_t)__a;
}

__C17_INTRIN poly8x16_t vreinterpretq_p8_u32(uint32x4_t __a)
{
  return (poly8x16_t)__a;
}

__C17_INTRIN poly8x16_t vreinterpretq_p8_u64(uint64x2_t __a)
{
  return (poly8x16_t)__a;
}

__C17_INTRIN poly8x16_t vreinterpretq_p8_f32(float32x4_t __a)
{
  return (poly8x16_t)__a;
}

__C17_INTRIN poly8x16_t vreinterpretq_p8_f64(float64x2_t __a)
{
  return (poly8x16_t)__a;
}

__C17_INTRIN poly8x16_t vreinterpretq_p8_p16(poly16x8_t __a)
{
  return (poly8x16_t)__a;
}

__C17_INTRIN poly16x8_t vreinterpretq_p16_s8(int8x16_t __a)
{
  return (poly16x8_t)__a;
}

__C17_INTRIN poly16x8_t vreinterpretq_p16_s16(int16x8_t __a)
{
  return (poly16x8_t)__a;
}

__C17_INTRIN poly16x8_t vreinterpretq_p16_s32(int32x4_t __a)
{
  return (poly16x8_t)__a;
}

__C17_INTRIN poly16x8_t vreinterpretq_p16_s64(int64x2_t __a)
{
  return (poly16x8_t)__a;
}

__C17_INTRIN poly16x8_t vreinterpretq_p16_u8(uint8x16_t __a)
{
  return (poly16x8_t)__a;
}

__C17_INTRIN poly16x8_t vreinterpretq_p16_u16(uint16x8_t __a)
{
  return (poly16x8_t)__a;
}

__C17_INTRIN poly16x8_t vreinterpretq_p16_u32(uint32x4_t __a)
{
  return (poly16x8_t)__a;
}

__C17_INTRIN poly16x8_t vreinterpretq_p16_u64(uint64x2_t __a)
{
  return (poly16x8_t)__a;
}

__C17_INTRIN poly16x8_t vreinterpretq_p16_f32(float32x4_t __a)
{
  return (poly16x8_t)__a;
}

__C17_INTRIN poly16x8_t vreinterpretq_p16_f64(float64x2_t __a)
{
  return (poly16x8_t)__a;
}

__C17_INTRIN poly16x8_t vreinterpretq_p16_p8(poly8x16_t __a)
{
  return (poly16x8_t)__a;
}


/* Arithmetic. */

__C17_INTRIN int8x8_t vadd_s8(int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)((uint8x8_t)__a + (uint8x8_t)__b);
}

__C17_INTRIN int8x8_t vsub_s8(int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)((uint8x8_t)__a - (uint8x8_t)__b);
}

__C17_INTRIN int8x8_t vmul_s8(int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)((uint8x8_t)__a * (uint8x8_t)__b);
}

__C17_INTRIN int8x8_t vmla_s8(int8x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return (int8x8_t)((uint8x8_t)__a + (uint8x8_t)__b * (uint8x8_t)__c);
}

__C17_INTRIN int8x8_t vmls_s8(int8x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return (int8x8_t)((uint8x8_t)__a - (uint8x8_t)__b * (uint8x8_t)__c);
}

__C17_INTRIN int8x8_t vabs_s8(int8x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] < 0 ? 0 - (uint64_t)__a[__i] : (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int8x8_t vneg_s8(int8x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = 0 - (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int8x8_t vqabs_s8(int8x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(__a[__i] < 0 ? -(__int128)__a[__i] : (__int128)__a[__i], 8);
  return __r;
}

__C17_INTRIN int8x8_t vqneg_s8(int8x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(-(__int128)__a[__i], 8);
  return __r;
}

__C17_INTRIN int8x8_t vabd_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i];
  return __r;
}

__C17_INTRIN int8x8_t vaba_s8(int8x8_t __a, int8x8_t __b, int8x8_t __c)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN int8x8_t vmax_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] > __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN int8x8_t vmin_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] < __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN int8x8_t vhadd_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN int8x8_t vrhadd_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i] + 1) >> 1;
  return __r;
}

__C17_INTRIN int8x8_t vhsub_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((int64_t)__a[__i] - __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN int8x8_t vqadd_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 8);
  return __r;
}

__C17_INTRIN int8x8_t vqsub_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __b[__i], 8);
  return __r;
}

__C17_INTRIN int8x8_t vuqadd_s8(int8x8_t __a, uint8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 8);
  return __r;
}

__C17_INTRIN int8x16_t vaddq_s8(int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)((uint8x16_t)__a + (uint8x16_t)__b);
}

__C17_INTRIN int8x16_t vsubq_s8(int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)((uint8x16_t)__a - (uint8x16_t)__b);
}

__C17_INTRIN int8x16_t vmulq_s8(int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)((uint8x16_t)__a * (uint8x16_t)__b);
}

__C17_INTRIN int8x16_t vmlaq_s8(int8x16_t __a, int8x16_t __b, int8x16_t __c)
{
  return (int8x16_t)((uint8x16_t)__a + (uint8x16_t)__b * (uint8x16_t)__c);
}

__C17_INTRIN int8x16_t vmlsq_s8(int8x16_t __a, int8x16_t __b, int8x16_t __c)
{
  return (int8x16_t)((uint8x16_t)__a - (uint8x16_t)__b * (uint8x16_t)__c);
}

__C17_INTRIN int8x16_t vabsq_s8(int8x16_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i] < 0 ? 0 - (uint64_t)__a[__i] : (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int8x16_t vnegq_s8(int8x16_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = 0 - (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int8x16_t vqabsq_s8(int8x16_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_sat_s(__a[__i] < 0 ? -(__int128)__a[__i] : (__int128)__a[__i], 8);
  return __r;
}

__C17_INTRIN int8x16_t vqnegq_s8(int8x16_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_sat_s(-(__int128)__a[__i], 8);
  return __r;
}

__C17_INTRIN int8x16_t vabdq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i];
  return __r;
}

__C17_INTRIN int8x16_t vabaq_s8(int8x16_t __a, int8x16_t __b, int8x16_t __c)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN int8x16_t vmaxq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i] > __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN int8x16_t vminq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i] < __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN int8x16_t vhaddq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN int8x16_t vrhaddq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i] + 1) >> 1;
  return __r;
}

__C17_INTRIN int8x16_t vhsubq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = ((int64_t)__a[__i] - __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN int8x16_t vqaddq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 8);
  return __r;
}

__C17_INTRIN int8x16_t vqsubq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __b[__i], 8);
  return __r;
}

__C17_INTRIN int8x16_t vuqaddq_s8(int8x16_t __a, uint8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 8);
  return __r;
}

__C17_INTRIN int16x4_t vadd_s16(int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)((uint16x4_t)__a + (uint16x4_t)__b);
}

__C17_INTRIN int16x4_t vsub_s16(int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)((uint16x4_t)__a - (uint16x4_t)__b);
}

__C17_INTRIN int16x4_t vmul_s16(int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)((uint16x4_t)__a * (uint16x4_t)__b);
}

__C17_INTRIN int16x4_t vmla_s16(int16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int16x4_t)((uint16x4_t)__a + (uint16x4_t)__b * (uint16x4_t)__c);
}

__C17_INTRIN int16x4_t vmls_s16(int16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int16x4_t)((uint16x4_t)__a - (uint16x4_t)__b * (uint16x4_t)__c);
}

__C17_INTRIN int16x4_t vmul_n_s16(int16x4_t __a, int16_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b;
  return __r;
}

__C17_INTRIN int16x4_t vmla_n_s16(int16x4_t __a, int16x4_t __b, int16_t __c)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN int16x4_t vmls_n_s16(int16x4_t __a, int16x4_t __b, int16_t __c)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN int16x4_t __c17_vmul_lane_s16(int16x4_t __a, int16x4_t __b, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmul_lane_s16(...) __c17_vmul_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vmla_lane_s16(int16x4_t __a, int16x4_t __b, int16x4_t __c, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmla_lane_s16(...) __c17_vmla_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vmls_lane_s16(int16x4_t __a, int16x4_t __b, int16x4_t __c, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmls_lane_s16(...) __c17_vmls_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vmul_laneq_s16(int16x4_t __a, int16x8_t __b, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmul_laneq_s16(...) __c17_vmul_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vmla_laneq_s16(int16x4_t __a, int16x4_t __b, int16x8_t __c, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmla_laneq_s16(...) __c17_vmla_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vmls_laneq_s16(int16x4_t __a, int16x4_t __b, int16x8_t __c, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmls_laneq_s16(...) __c17_vmls_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t vabs_s16(int16x4_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] < 0 ? 0 - (uint64_t)__a[__i] : (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int16x4_t vneg_s16(int16x4_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = 0 - (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int16x4_t vqabs_s16(int16x4_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(__a[__i] < 0 ? -(__int128)__a[__i] : (__int128)__a[__i], 16);
  return __r;
}

__C17_INTRIN int16x4_t vqneg_s16(int16x4_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(-(__int128)__a[__i], 16);
  return __r;
}

__C17_INTRIN int16x4_t vabd_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i];
  return __r;
}

__C17_INTRIN int16x4_t vaba_s16(int16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN int16x4_t vmax_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] > __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN int16x4_t vmin_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] < __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN int16x4_t vhadd_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN int16x4_t vrhadd_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i] + 1) >> 1;
  return __r;
}

__C17_INTRIN int16x4_t vhsub_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((int64_t)__a[__i] - __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN int16x4_t vqadd_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 16);
  return __r;
}

__C17_INTRIN int16x4_t vqsub_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __b[__i], 16);
  return __r;
}

__C17_INTRIN int16x4_t vuqadd_s16(int16x4_t __a, uint16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 16);
  return __r;
}

__C17_INTRIN int16x4_t vqdmulh_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__i]) >> 16, 16);
  return __r;
}

__C17_INTRIN int16x4_t vqdmulh_n_s16(int16x4_t __a, int16_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b) >> 16, 16);
  return __r;
}

__C17_INTRIN int16x4_t __c17_vqdmulh_lane_s16(int16x4_t __a, int16x4_t __b, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane]) >> 16, 16);
  return __r;
}
#define vqdmulh_lane_s16(...) __c17_vqdmulh_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vqdmulh_laneq_s16(int16x4_t __a, int16x8_t __b, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane]) >> 16, 16);
  return __r;
}
#define vqdmulh_laneq_s16(...) __c17_vqdmulh_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t vqrdmulh_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__i] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}

__C17_INTRIN int16x4_t vqrdmulh_n_s16(int16x4_t __a, int16_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}

__C17_INTRIN int16x4_t __c17_vqrdmulh_lane_s16(int16x4_t __a, int16x4_t __b, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}
#define vqrdmulh_lane_s16(...) __c17_vqrdmulh_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vqrdmulh_laneq_s16(int16x4_t __a, int16x8_t __b, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}
#define vqrdmulh_laneq_s16(...) __c17_vqrdmulh_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t vqrdmlah_s16(int16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 16) + (__int128)2 * __b[__i] * __c[__i] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}

__C17_INTRIN int16x4_t __c17_vqrdmlah_lane_s16(int16x4_t __a, int16x4_t __b, int16x4_t __c, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 16) + (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}
#define vqrdmlah_lane_s16(...) __c17_vqrdmlah_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vqrdmlah_laneq_s16(int16x4_t __a, int16x4_t __b, int16x8_t __c, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 16) + (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}
#define vqrdmlah_laneq_s16(...) __c17_vqrdmlah_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t vqrdmlsh_s16(int16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 16) - (__int128)2 * __b[__i] * __c[__i] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}

__C17_INTRIN int16x4_t __c17_vqrdmlsh_lane_s16(int16x4_t __a, int16x4_t __b, int16x4_t __c, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 16) - (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}
#define vqrdmlsh_lane_s16(...) __c17_vqrdmlsh_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vqrdmlsh_laneq_s16(int16x4_t __a, int16x4_t __b, int16x8_t __c, const int __lane)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 16) - (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}
#define vqrdmlsh_laneq_s16(...) __c17_vqrdmlsh_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t vaddq_s16(int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)((uint16x8_t)__a + (uint16x8_t)__b);
}

__C17_INTRIN int16x8_t vsubq_s16(int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)((uint16x8_t)__a - (uint16x8_t)__b);
}

__C17_INTRIN int16x8_t vmulq_s16(int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)((uint16x8_t)__a * (uint16x8_t)__b);
}

__C17_INTRIN int16x8_t vmlaq_s16(int16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return (int16x8_t)((uint16x8_t)__a + (uint16x8_t)__b * (uint16x8_t)__c);
}

__C17_INTRIN int16x8_t vmlsq_s16(int16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return (int16x8_t)((uint16x8_t)__a - (uint16x8_t)__b * (uint16x8_t)__c);
}

__C17_INTRIN int16x8_t vmulq_n_s16(int16x8_t __a, int16_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b;
  return __r;
}

__C17_INTRIN int16x8_t vmlaq_n_s16(int16x8_t __a, int16x8_t __b, int16_t __c)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN int16x8_t vmlsq_n_s16(int16x8_t __a, int16x8_t __b, int16_t __c)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN int16x8_t __c17_vmulq_lane_s16(int16x8_t __a, int16x4_t __b, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmulq_lane_s16(...) __c17_vmulq_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vmlaq_lane_s16(int16x8_t __a, int16x8_t __b, int16x4_t __c, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlaq_lane_s16(...) __c17_vmlaq_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vmlsq_lane_s16(int16x8_t __a, int16x8_t __b, int16x4_t __c, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlsq_lane_s16(...) __c17_vmlsq_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vmulq_laneq_s16(int16x8_t __a, int16x8_t __b, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmulq_laneq_s16(...) __c17_vmulq_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vmlaq_laneq_s16(int16x8_t __a, int16x8_t __b, int16x8_t __c, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlaq_laneq_s16(...) __c17_vmlaq_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vmlsq_laneq_s16(int16x8_t __a, int16x8_t __b, int16x8_t __c, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlsq_laneq_s16(...) __c17_vmlsq_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t vabsq_s16(int16x8_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] < 0 ? 0 - (uint64_t)__a[__i] : (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int16x8_t vnegq_s16(int16x8_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = 0 - (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int16x8_t vqabsq_s16(int16x8_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(__a[__i] < 0 ? -(__int128)__a[__i] : (__int128)__a[__i], 16);
  return __r;
}

__C17_INTRIN int16x8_t vqnegq_s16(int16x8_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(-(__int128)__a[__i], 16);
  return __r;
}

__C17_INTRIN int16x8_t vabdq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i];
  return __r;
}

__C17_INTRIN int16x8_t vabaq_s16(int16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN int16x8_t vmaxq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] > __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN int16x8_t vminq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] < __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN int16x8_t vhaddq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN int16x8_t vrhaddq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i] + 1) >> 1;
  return __r;
}

__C17_INTRIN int16x8_t vhsubq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((int64_t)__a[__i] - __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN int16x8_t vqaddq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 16);
  return __r;
}

__C17_INTRIN int16x8_t vqsubq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __b[__i], 16);
  return __r;
}

__C17_INTRIN int16x8_t vuqaddq_s16(int16x8_t __a, uint16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 16);
  return __r;
}

__C17_INTRIN int16x8_t vqdmulhq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__i]) >> 16, 16);
  return __r;
}

__C17_INTRIN int16x8_t vqdmulhq_n_s16(int16x8_t __a, int16_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b) >> 16, 16);
  return __r;
}

__C17_INTRIN int16x8_t __c17_vqdmulhq_lane_s16(int16x8_t __a, int16x4_t __b, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane]) >> 16, 16);
  return __r;
}
#define vqdmulhq_lane_s16(...) __c17_vqdmulhq_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vqdmulhq_laneq_s16(int16x8_t __a, int16x8_t __b, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane]) >> 16, 16);
  return __r;
}
#define vqdmulhq_laneq_s16(...) __c17_vqdmulhq_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t vqrdmulhq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__i] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}

__C17_INTRIN int16x8_t vqrdmulhq_n_s16(int16x8_t __a, int16_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}

__C17_INTRIN int16x8_t __c17_vqrdmulhq_lane_s16(int16x8_t __a, int16x4_t __b, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}
#define vqrdmulhq_lane_s16(...) __c17_vqrdmulhq_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vqrdmulhq_laneq_s16(int16x8_t __a, int16x8_t __b, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}
#define vqrdmulhq_laneq_s16(...) __c17_vqrdmulhq_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t vqrdmlahq_s16(int16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 16) + (__int128)2 * __b[__i] * __c[__i] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}

__C17_INTRIN int16x8_t __c17_vqrdmlahq_lane_s16(int16x8_t __a, int16x8_t __b, int16x4_t __c, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 16) + (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}
#define vqrdmlahq_lane_s16(...) __c17_vqrdmlahq_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vqrdmlahq_laneq_s16(int16x8_t __a, int16x8_t __b, int16x8_t __c, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 16) + (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}
#define vqrdmlahq_laneq_s16(...) __c17_vqrdmlahq_laneq_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t vqrdmlshq_s16(int16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 16) - (__int128)2 * __b[__i] * __c[__i] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}

__C17_INTRIN int16x8_t __c17_vqrdmlshq_lane_s16(int16x8_t __a, int16x8_t __b, int16x4_t __c, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 16) - (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}
#define vqrdmlshq_lane_s16(...) __c17_vqrdmlshq_lane_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vqrdmlshq_laneq_s16(int16x8_t __a, int16x8_t __b, int16x8_t __c, const int __lane)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 16) - (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 15)) >> 16, 16);
  return __r;
}
#define vqrdmlshq_laneq_s16(...) __c17_vqrdmlshq_laneq_s16(__VA_ARGS__)

__C17_INTRIN int32x2_t vadd_s32(int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)((uint32x2_t)__a + (uint32x2_t)__b);
}

__C17_INTRIN int32x2_t vsub_s32(int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)((uint32x2_t)__a - (uint32x2_t)__b);
}

__C17_INTRIN int32x2_t vmul_s32(int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)((uint32x2_t)__a * (uint32x2_t)__b);
}

__C17_INTRIN int32x2_t vmla_s32(int32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int32x2_t)((uint32x2_t)__a + (uint32x2_t)__b * (uint32x2_t)__c);
}

__C17_INTRIN int32x2_t vmls_s32(int32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int32x2_t)((uint32x2_t)__a - (uint32x2_t)__b * (uint32x2_t)__c);
}

__C17_INTRIN int32x2_t vmul_n_s32(int32x2_t __a, int32_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b;
  return __r;
}

__C17_INTRIN int32x2_t vmla_n_s32(int32x2_t __a, int32x2_t __b, int32_t __c)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN int32x2_t vmls_n_s32(int32x2_t __a, int32x2_t __b, int32_t __c)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN int32x2_t __c17_vmul_lane_s32(int32x2_t __a, int32x2_t __b, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmul_lane_s32(...) __c17_vmul_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vmla_lane_s32(int32x2_t __a, int32x2_t __b, int32x2_t __c, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmla_lane_s32(...) __c17_vmla_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vmls_lane_s32(int32x2_t __a, int32x2_t __b, int32x2_t __c, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmls_lane_s32(...) __c17_vmls_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vmul_laneq_s32(int32x2_t __a, int32x4_t __b, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmul_laneq_s32(...) __c17_vmul_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vmla_laneq_s32(int32x2_t __a, int32x2_t __b, int32x4_t __c, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmla_laneq_s32(...) __c17_vmla_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vmls_laneq_s32(int32x2_t __a, int32x2_t __b, int32x4_t __c, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmls_laneq_s32(...) __c17_vmls_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t vabs_s32(int32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] < 0 ? 0 - (uint64_t)__a[__i] : (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int32x2_t vneg_s32(int32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = 0 - (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int32x2_t vqabs_s32(int32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(__a[__i] < 0 ? -(__int128)__a[__i] : (__int128)__a[__i], 32);
  return __r;
}

__C17_INTRIN int32x2_t vqneg_s32(int32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(-(__int128)__a[__i], 32);
  return __r;
}

__C17_INTRIN int32x2_t vabd_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i];
  return __r;
}

__C17_INTRIN int32x2_t vaba_s32(int32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN int32x2_t vmax_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] > __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN int32x2_t vmin_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] < __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN int32x2_t vhadd_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN int32x2_t vrhadd_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i] + 1) >> 1;
  return __r;
}

__C17_INTRIN int32x2_t vhsub_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((int64_t)__a[__i] - __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN int32x2_t vqadd_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 32);
  return __r;
}

__C17_INTRIN int32x2_t vqsub_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __b[__i], 32);
  return __r;
}

__C17_INTRIN int32x2_t vuqadd_s32(int32x2_t __a, uint32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 32);
  return __r;
}

__C17_INTRIN int32x2_t vqdmulh_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__i]) >> 32, 32);
  return __r;
}

__C17_INTRIN int32x2_t vqdmulh_n_s32(int32x2_t __a, int32_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b) >> 32, 32);
  return __r;
}

__C17_INTRIN int32x2_t __c17_vqdmulh_lane_s32(int32x2_t __a, int32x2_t __b, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane]) >> 32, 32);
  return __r;
}
#define vqdmulh_lane_s32(...) __c17_vqdmulh_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vqdmulh_laneq_s32(int32x2_t __a, int32x4_t __b, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane]) >> 32, 32);
  return __r;
}
#define vqdmulh_laneq_s32(...) __c17_vqdmulh_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t vqrdmulh_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__i] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}

__C17_INTRIN int32x2_t vqrdmulh_n_s32(int32x2_t __a, int32_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}

__C17_INTRIN int32x2_t __c17_vqrdmulh_lane_s32(int32x2_t __a, int32x2_t __b, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}
#define vqrdmulh_lane_s32(...) __c17_vqrdmulh_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vqrdmulh_laneq_s32(int32x2_t __a, int32x4_t __b, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}
#define vqrdmulh_laneq_s32(...) __c17_vqrdmulh_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t vqrdmlah_s32(int32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 32) + (__int128)2 * __b[__i] * __c[__i] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}

__C17_INTRIN int32x2_t __c17_vqrdmlah_lane_s32(int32x2_t __a, int32x2_t __b, int32x2_t __c, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 32) + (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}
#define vqrdmlah_lane_s32(...) __c17_vqrdmlah_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vqrdmlah_laneq_s32(int32x2_t __a, int32x2_t __b, int32x4_t __c, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 32) + (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}
#define vqrdmlah_laneq_s32(...) __c17_vqrdmlah_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t vqrdmlsh_s32(int32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 32) - (__int128)2 * __b[__i] * __c[__i] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}

__C17_INTRIN int32x2_t __c17_vqrdmlsh_lane_s32(int32x2_t __a, int32x2_t __b, int32x2_t __c, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 32) - (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}
#define vqrdmlsh_lane_s32(...) __c17_vqrdmlsh_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vqrdmlsh_laneq_s32(int32x2_t __a, int32x2_t __b, int32x4_t __c, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 32) - (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}
#define vqrdmlsh_laneq_s32(...) __c17_vqrdmlsh_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t vaddq_s32(int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)((uint32x4_t)__a + (uint32x4_t)__b);
}

__C17_INTRIN int32x4_t vsubq_s32(int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)((uint32x4_t)__a - (uint32x4_t)__b);
}

__C17_INTRIN int32x4_t vmulq_s32(int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)((uint32x4_t)__a * (uint32x4_t)__b);
}

__C17_INTRIN int32x4_t vmlaq_s32(int32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return (int32x4_t)((uint32x4_t)__a + (uint32x4_t)__b * (uint32x4_t)__c);
}

__C17_INTRIN int32x4_t vmlsq_s32(int32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return (int32x4_t)((uint32x4_t)__a - (uint32x4_t)__b * (uint32x4_t)__c);
}

__C17_INTRIN int32x4_t vmulq_n_s32(int32x4_t __a, int32_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b;
  return __r;
}

__C17_INTRIN int32x4_t vmlaq_n_s32(int32x4_t __a, int32x4_t __b, int32_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN int32x4_t vmlsq_n_s32(int32x4_t __a, int32x4_t __b, int32_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN int32x4_t __c17_vmulq_lane_s32(int32x4_t __a, int32x2_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmulq_lane_s32(...) __c17_vmulq_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmlaq_lane_s32(int32x4_t __a, int32x4_t __b, int32x2_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlaq_lane_s32(...) __c17_vmlaq_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmlsq_lane_s32(int32x4_t __a, int32x4_t __b, int32x2_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlsq_lane_s32(...) __c17_vmlsq_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmulq_laneq_s32(int32x4_t __a, int32x4_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmulq_laneq_s32(...) __c17_vmulq_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmlaq_laneq_s32(int32x4_t __a, int32x4_t __b, int32x4_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlaq_laneq_s32(...) __c17_vmlaq_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmlsq_laneq_s32(int32x4_t __a, int32x4_t __b, int32x4_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlsq_laneq_s32(...) __c17_vmlsq_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t vabsq_s32(int32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] < 0 ? 0 - (uint64_t)__a[__i] : (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int32x4_t vnegq_s32(int32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = 0 - (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int32x4_t vqabsq_s32(int32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(__a[__i] < 0 ? -(__int128)__a[__i] : (__int128)__a[__i], 32);
  return __r;
}

__C17_INTRIN int32x4_t vqnegq_s32(int32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(-(__int128)__a[__i], 32);
  return __r;
}

__C17_INTRIN int32x4_t vabdq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i];
  return __r;
}

__C17_INTRIN int32x4_t vabaq_s32(int32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN int32x4_t vmaxq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] > __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN int32x4_t vminq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] < __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN int32x4_t vhaddq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN int32x4_t vrhaddq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i] + 1) >> 1;
  return __r;
}

__C17_INTRIN int32x4_t vhsubq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((int64_t)__a[__i] - __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN int32x4_t vqaddq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 32);
  return __r;
}

__C17_INTRIN int32x4_t vqsubq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __b[__i], 32);
  return __r;
}

__C17_INTRIN int32x4_t vuqaddq_s32(int32x4_t __a, uint32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 32);
  return __r;
}

__C17_INTRIN int32x4_t vqdmulhq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__i]) >> 32, 32);
  return __r;
}

__C17_INTRIN int32x4_t vqdmulhq_n_s32(int32x4_t __a, int32_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b) >> 32, 32);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vqdmulhq_lane_s32(int32x4_t __a, int32x2_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane]) >> 32, 32);
  return __r;
}
#define vqdmulhq_lane_s32(...) __c17_vqdmulhq_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqdmulhq_laneq_s32(int32x4_t __a, int32x4_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane]) >> 32, 32);
  return __r;
}
#define vqdmulhq_laneq_s32(...) __c17_vqdmulhq_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t vqrdmulhq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__i] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}

__C17_INTRIN int32x4_t vqrdmulhq_n_s32(int32x4_t __a, int32_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vqrdmulhq_lane_s32(int32x4_t __a, int32x2_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}
#define vqrdmulhq_lane_s32(...) __c17_vqrdmulhq_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqrdmulhq_laneq_s32(int32x4_t __a, int32x4_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)2 * __a[__i] * __b[__lane] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}
#define vqrdmulhq_laneq_s32(...) __c17_vqrdmulhq_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t vqrdmlahq_s32(int32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 32) + (__int128)2 * __b[__i] * __c[__i] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vqrdmlahq_lane_s32(int32x4_t __a, int32x4_t __b, int32x2_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 32) + (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}
#define vqrdmlahq_lane_s32(...) __c17_vqrdmlahq_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqrdmlahq_laneq_s32(int32x4_t __a, int32x4_t __b, int32x4_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 32) + (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}
#define vqrdmlahq_laneq_s32(...) __c17_vqrdmlahq_laneq_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t vqrdmlshq_s32(int32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 32) - (__int128)2 * __b[__i] * __c[__i] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vqrdmlshq_lane_s32(int32x4_t __a, int32x4_t __b, int32x2_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 32) - (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}
#define vqrdmlshq_lane_s32(...) __c17_vqrdmlshq_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqrdmlshq_laneq_s32(int32x4_t __a, int32x4_t __b, int32x4_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(((__int128)__a[__i] * ((__int128)1 << 32) - (__int128)2 * __b[__i] * __c[__lane] + ((__int128)1 << 31)) >> 32, 32);
  return __r;
}
#define vqrdmlshq_laneq_s32(...) __c17_vqrdmlshq_laneq_s32(__VA_ARGS__)

__C17_INTRIN int64x1_t vadd_s64(int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t)((uint64x1_t)__a + (uint64x1_t)__b);
}

__C17_INTRIN int64x1_t vsub_s64(int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t)((uint64x1_t)__a - (uint64x1_t)__b);
}

__C17_INTRIN int64x1_t vabs_s64(int64x1_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i] < 0 ? 0 - (uint64_t)__a[__i] : (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int64x1_t vneg_s64(int64x1_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = 0 - (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int64x1_t vqabs_s64(int64x1_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_sat_s(__a[__i] < 0 ? -(__int128)__a[__i] : (__int128)__a[__i], 64);
  return __r;
}

__C17_INTRIN int64x1_t vqneg_s64(int64x1_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_sat_s(-(__int128)__a[__i], 64);
  return __r;
}

__C17_INTRIN int64x1_t vqadd_s64(int64x1_t __a, int64x1_t __b)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 64);
  return __r;
}

__C17_INTRIN int64x1_t vqsub_s64(int64x1_t __a, int64x1_t __b)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __b[__i], 64);
  return __r;
}

__C17_INTRIN int64x1_t vuqadd_s64(int64x1_t __a, uint64x1_t __b)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 64);
  return __r;
}

__C17_INTRIN int64x2_t vaddq_s64(int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t)((uint64x2_t)__a + (uint64x2_t)__b);
}

__C17_INTRIN int64x2_t vsubq_s64(int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t)((uint64x2_t)__a - (uint64x2_t)__b);
}

__C17_INTRIN int64x2_t vabsq_s64(int64x2_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] < 0 ? 0 - (uint64_t)__a[__i] : (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int64x2_t vnegq_s64(int64x2_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = 0 - (uint64_t)__a[__i];
  return __r;
}

__C17_INTRIN int64x2_t vqabsq_s64(int64x2_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(__a[__i] < 0 ? -(__int128)__a[__i] : (__int128)__a[__i], 64);
  return __r;
}

__C17_INTRIN int64x2_t vqnegq_s64(int64x2_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(-(__int128)__a[__i], 64);
  return __r;
}

__C17_INTRIN int64x2_t vqaddq_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 64);
  return __r;
}

__C17_INTRIN int64x2_t vqsubq_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __b[__i], 64);
  return __r;
}

__C17_INTRIN int64x2_t vuqaddq_s64(int64x2_t __a, uint64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __b[__i], 64);
  return __r;
}

__C17_INTRIN uint8x8_t vadd_u8(uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)((uint8x8_t)__a + (uint8x8_t)__b);
}

__C17_INTRIN uint8x8_t vsub_u8(uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)((uint8x8_t)__a - (uint8x8_t)__b);
}

__C17_INTRIN uint8x8_t vmul_u8(uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)((uint8x8_t)__a * (uint8x8_t)__b);
}

__C17_INTRIN uint8x8_t vmla_u8(uint8x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return (uint8x8_t)((uint8x8_t)__a + (uint8x8_t)__b * (uint8x8_t)__c);
}

__C17_INTRIN uint8x8_t vmls_u8(uint8x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return (uint8x8_t)((uint8x8_t)__a - (uint8x8_t)__b * (uint8x8_t)__c);
}

__C17_INTRIN uint8x8_t vabd_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vaba_u8(uint8x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN uint8x8_t vmax_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] > __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vmin_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] < __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vhadd_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN uint8x8_t vrhadd_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i] + 1) >> 1;
  return __r;
}

__C17_INTRIN uint8x8_t vhsub_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((int64_t)__a[__i] - __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN uint8x8_t vqadd_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 8);
  return __r;
}

__C17_INTRIN uint8x8_t vqsub_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] - __b[__i], 8);
  return __r;
}

__C17_INTRIN uint8x8_t vsqadd_u8(uint8x8_t __a, int8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 8);
  return __r;
}

__C17_INTRIN uint8x16_t vaddq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)((uint8x16_t)__a + (uint8x16_t)__b);
}

__C17_INTRIN uint8x16_t vsubq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)((uint8x16_t)__a - (uint8x16_t)__b);
}

__C17_INTRIN uint8x16_t vmulq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)((uint8x16_t)__a * (uint8x16_t)__b);
}

__C17_INTRIN uint8x16_t vmlaq_u8(uint8x16_t __a, uint8x16_t __b, uint8x16_t __c)
{
  return (uint8x16_t)((uint8x16_t)__a + (uint8x16_t)__b * (uint8x16_t)__c);
}

__C17_INTRIN uint8x16_t vmlsq_u8(uint8x16_t __a, uint8x16_t __b, uint8x16_t __c)
{
  return (uint8x16_t)((uint8x16_t)__a - (uint8x16_t)__b * (uint8x16_t)__c);
}

__C17_INTRIN uint8x16_t vabdq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i];
  return __r;
}

__C17_INTRIN uint8x16_t vabaq_u8(uint8x16_t __a, uint8x16_t __b, uint8x16_t __c)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN uint8x16_t vmaxq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i] > __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN uint8x16_t vminq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i] < __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN uint8x16_t vhaddq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN uint8x16_t vrhaddq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i] + 1) >> 1;
  return __r;
}

__C17_INTRIN uint8x16_t vhsubq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = ((int64_t)__a[__i] - __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN uint8x16_t vqaddq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 8);
  return __r;
}

__C17_INTRIN uint8x16_t vqsubq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] - __b[__i], 8);
  return __r;
}

__C17_INTRIN uint8x16_t vsqaddq_u8(uint8x16_t __a, int8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 8);
  return __r;
}

__C17_INTRIN uint16x4_t vadd_u16(uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)((uint16x4_t)__a + (uint16x4_t)__b);
}

__C17_INTRIN uint16x4_t vsub_u16(uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)((uint16x4_t)__a - (uint16x4_t)__b);
}

__C17_INTRIN uint16x4_t vmul_u16(uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)((uint16x4_t)__a * (uint16x4_t)__b);
}

__C17_INTRIN uint16x4_t vmla_u16(uint16x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return (uint16x4_t)((uint16x4_t)__a + (uint16x4_t)__b * (uint16x4_t)__c);
}

__C17_INTRIN uint16x4_t vmls_u16(uint16x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return (uint16x4_t)((uint16x4_t)__a - (uint16x4_t)__b * (uint16x4_t)__c);
}

__C17_INTRIN uint16x4_t vmul_n_u16(uint16x4_t __a, uint16_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b;
  return __r;
}

__C17_INTRIN uint16x4_t vmla_n_u16(uint16x4_t __a, uint16x4_t __b, uint16_t __c)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN uint16x4_t vmls_n_u16(uint16x4_t __a, uint16x4_t __b, uint16_t __c)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN uint16x4_t __c17_vmul_lane_u16(uint16x4_t __a, uint16x4_t __b, const int __lane)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmul_lane_u16(...) __c17_vmul_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vmla_lane_u16(uint16x4_t __a, uint16x4_t __b, uint16x4_t __c, const int __lane)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmla_lane_u16(...) __c17_vmla_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vmls_lane_u16(uint16x4_t __a, uint16x4_t __b, uint16x4_t __c, const int __lane)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmls_lane_u16(...) __c17_vmls_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vmul_laneq_u16(uint16x4_t __a, uint16x8_t __b, const int __lane)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmul_laneq_u16(...) __c17_vmul_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vmla_laneq_u16(uint16x4_t __a, uint16x4_t __b, uint16x8_t __c, const int __lane)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmla_laneq_u16(...) __c17_vmla_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vmls_laneq_u16(uint16x4_t __a, uint16x4_t __b, uint16x8_t __c, const int __lane)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmls_laneq_u16(...) __c17_vmls_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t vabd_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i];
  return __r;
}

__C17_INTRIN uint16x4_t vaba_u16(uint16x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN uint16x4_t vmax_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] > __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN uint16x4_t vmin_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] < __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN uint16x4_t vhadd_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN uint16x4_t vrhadd_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i] + 1) >> 1;
  return __r;
}

__C17_INTRIN uint16x4_t vhsub_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((int64_t)__a[__i] - __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN uint16x4_t vqadd_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 16);
  return __r;
}

__C17_INTRIN uint16x4_t vqsub_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] - __b[__i], 16);
  return __r;
}

__C17_INTRIN uint16x4_t vsqadd_u16(uint16x4_t __a, int16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 16);
  return __r;
}

__C17_INTRIN uint16x8_t vaddq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)((uint16x8_t)__a + (uint16x8_t)__b);
}

__C17_INTRIN uint16x8_t vsubq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)((uint16x8_t)__a - (uint16x8_t)__b);
}

__C17_INTRIN uint16x8_t vmulq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)((uint16x8_t)__a * (uint16x8_t)__b);
}

__C17_INTRIN uint16x8_t vmlaq_u16(uint16x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return (uint16x8_t)((uint16x8_t)__a + (uint16x8_t)__b * (uint16x8_t)__c);
}

__C17_INTRIN uint16x8_t vmlsq_u16(uint16x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return (uint16x8_t)((uint16x8_t)__a - (uint16x8_t)__b * (uint16x8_t)__c);
}

__C17_INTRIN uint16x8_t vmulq_n_u16(uint16x8_t __a, uint16_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b;
  return __r;
}

__C17_INTRIN uint16x8_t vmlaq_n_u16(uint16x8_t __a, uint16x8_t __b, uint16_t __c)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN uint16x8_t vmlsq_n_u16(uint16x8_t __a, uint16x8_t __b, uint16_t __c)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN uint16x8_t __c17_vmulq_lane_u16(uint16x8_t __a, uint16x4_t __b, const int __lane)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmulq_lane_u16(...) __c17_vmulq_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vmlaq_lane_u16(uint16x8_t __a, uint16x8_t __b, uint16x4_t __c, const int __lane)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlaq_lane_u16(...) __c17_vmlaq_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vmlsq_lane_u16(uint16x8_t __a, uint16x8_t __b, uint16x4_t __c, const int __lane)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlsq_lane_u16(...) __c17_vmlsq_lane_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vmulq_laneq_u16(uint16x8_t __a, uint16x8_t __b, const int __lane)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmulq_laneq_u16(...) __c17_vmulq_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vmlaq_laneq_u16(uint16x8_t __a, uint16x8_t __b, uint16x8_t __c, const int __lane)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlaq_laneq_u16(...) __c17_vmlaq_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vmlsq_laneq_u16(uint16x8_t __a, uint16x8_t __b, uint16x8_t __c, const int __lane)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlsq_laneq_u16(...) __c17_vmlsq_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t vabdq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i];
  return __r;
}

__C17_INTRIN uint16x8_t vabaq_u16(uint16x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN uint16x8_t vmaxq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] > __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN uint16x8_t vminq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] < __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN uint16x8_t vhaddq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN uint16x8_t vrhaddq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i] + 1) >> 1;
  return __r;
}

__C17_INTRIN uint16x8_t vhsubq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((int64_t)__a[__i] - __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN uint16x8_t vqaddq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 16);
  return __r;
}

__C17_INTRIN uint16x8_t vqsubq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] - __b[__i], 16);
  return __r;
}

__C17_INTRIN uint16x8_t vsqaddq_u16(uint16x8_t __a, int16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 16);
  return __r;
}

__C17_INTRIN uint32x2_t vadd_u32(uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)((uint32x2_t)__a + (uint32x2_t)__b);
}

__C17_INTRIN uint32x2_t vsub_u32(uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)((uint32x2_t)__a - (uint32x2_t)__b);
}

__C17_INTRIN uint32x2_t vmul_u32(uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)((uint32x2_t)__a * (uint32x2_t)__b);
}

__C17_INTRIN uint32x2_t vmla_u32(uint32x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return (uint32x2_t)((uint32x2_t)__a + (uint32x2_t)__b * (uint32x2_t)__c);
}

__C17_INTRIN uint32x2_t vmls_u32(uint32x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return (uint32x2_t)((uint32x2_t)__a - (uint32x2_t)__b * (uint32x2_t)__c);
}

__C17_INTRIN uint32x2_t vmul_n_u32(uint32x2_t __a, uint32_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b;
  return __r;
}

__C17_INTRIN uint32x2_t vmla_n_u32(uint32x2_t __a, uint32x2_t __b, uint32_t __c)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN uint32x2_t vmls_n_u32(uint32x2_t __a, uint32x2_t __b, uint32_t __c)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN uint32x2_t __c17_vmul_lane_u32(uint32x2_t __a, uint32x2_t __b, const int __lane)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmul_lane_u32(...) __c17_vmul_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vmla_lane_u32(uint32x2_t __a, uint32x2_t __b, uint32x2_t __c, const int __lane)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmla_lane_u32(...) __c17_vmla_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vmls_lane_u32(uint32x2_t __a, uint32x2_t __b, uint32x2_t __c, const int __lane)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmls_lane_u32(...) __c17_vmls_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vmul_laneq_u32(uint32x2_t __a, uint32x4_t __b, const int __lane)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmul_laneq_u32(...) __c17_vmul_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vmla_laneq_u32(uint32x2_t __a, uint32x2_t __b, uint32x4_t __c, const int __lane)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmla_laneq_u32(...) __c17_vmla_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vmls_laneq_u32(uint32x2_t __a, uint32x2_t __b, uint32x4_t __c, const int __lane)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmls_laneq_u32(...) __c17_vmls_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t vabd_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i];
  return __r;
}

__C17_INTRIN uint32x2_t vaba_u32(uint32x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN uint32x2_t vmax_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] > __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN uint32x2_t vmin_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] < __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN uint32x2_t vhadd_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN uint32x2_t vrhadd_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i] + 1) >> 1;
  return __r;
}

__C17_INTRIN uint32x2_t vhsub_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((int64_t)__a[__i] - __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN uint32x2_t vqadd_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 32);
  return __r;
}

__C17_INTRIN uint32x2_t vqsub_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] - __b[__i], 32);
  return __r;
}

__C17_INTRIN uint32x2_t vsqadd_u32(uint32x2_t __a, int32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 32);
  return __r;
}

__C17_INTRIN uint32x4_t vaddq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)((uint32x4_t)__a + (uint32x4_t)__b);
}

__C17_INTRIN uint32x4_t vsubq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)((uint32x4_t)__a - (uint32x4_t)__b);
}

__C17_INTRIN uint32x4_t vmulq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)((uint32x4_t)__a * (uint32x4_t)__b);
}

__C17_INTRIN uint32x4_t vmlaq_u32(uint32x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return (uint32x4_t)((uint32x4_t)__a + (uint32x4_t)__b * (uint32x4_t)__c);
}

__C17_INTRIN uint32x4_t vmlsq_u32(uint32x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return (uint32x4_t)((uint32x4_t)__a - (uint32x4_t)__b * (uint32x4_t)__c);
}

__C17_INTRIN uint32x4_t vmulq_n_u32(uint32x4_t __a, uint32_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b;
  return __r;
}

__C17_INTRIN uint32x4_t vmlaq_n_u32(uint32x4_t __a, uint32x4_t __b, uint32_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN uint32x4_t vmlsq_n_u32(uint32x4_t __a, uint32x4_t __b, uint32_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c;
  return __r;
}

__C17_INTRIN uint32x4_t __c17_vmulq_lane_u32(uint32x4_t __a, uint32x2_t __b, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmulq_lane_u32(...) __c17_vmulq_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmlaq_lane_u32(uint32x4_t __a, uint32x4_t __b, uint32x2_t __c, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlaq_lane_u32(...) __c17_vmlaq_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmlsq_lane_u32(uint32x4_t __a, uint32x4_t __b, uint32x2_t __c, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlsq_lane_u32(...) __c17_vmlsq_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmulq_laneq_u32(uint32x4_t __a, uint32x4_t __b, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] * (uint64_t)__b[__lane];
  return __r;
}
#define vmulq_laneq_u32(...) __c17_vmulq_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmlaq_laneq_u32(uint32x4_t __a, uint32x4_t __b, uint32x4_t __c, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlaq_laneq_u32(...) __c17_vmlaq_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmlsq_laneq_u32(uint32x4_t __a, uint32x4_t __b, uint32x4_t __c, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i] * (uint64_t)__c[__lane];
  return __r;
}
#define vmlsq_laneq_u32(...) __c17_vmlsq_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t vabdq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i];
  return __r;
}

__C17_INTRIN uint32x4_t vabaq_u32(uint32x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN uint32x4_t vmaxq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] > __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN uint32x4_t vminq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] < __b[__i] ? __a[__i] : __b[__i];
  return __r;
}

__C17_INTRIN uint32x4_t vhaddq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN uint32x4_t vrhaddq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((int64_t)__a[__i] + __b[__i] + 1) >> 1;
  return __r;
}

__C17_INTRIN uint32x4_t vhsubq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((int64_t)__a[__i] - __b[__i]) >> 1;
  return __r;
}

__C17_INTRIN uint32x4_t vqaddq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 32);
  return __r;
}

__C17_INTRIN uint32x4_t vqsubq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] - __b[__i], 32);
  return __r;
}

__C17_INTRIN uint32x4_t vsqaddq_u32(uint32x4_t __a, int32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 32);
  return __r;
}

__C17_INTRIN uint64x1_t vadd_u64(uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t)((uint64x1_t)__a + (uint64x1_t)__b);
}

__C17_INTRIN uint64x1_t vsub_u64(uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t)((uint64x1_t)__a - (uint64x1_t)__b);
}

__C17_INTRIN uint64x1_t vqadd_u64(uint64x1_t __a, uint64x1_t __b)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 64);
  return __r;
}

__C17_INTRIN uint64x1_t vqsub_u64(uint64x1_t __a, uint64x1_t __b)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] - __b[__i], 64);
  return __r;
}

__C17_INTRIN uint64x1_t vsqadd_u64(uint64x1_t __a, int64x1_t __b)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 64);
  return __r;
}

__C17_INTRIN uint64x2_t vaddq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t)((uint64x2_t)__a + (uint64x2_t)__b);
}

__C17_INTRIN uint64x2_t vsubq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t)((uint64x2_t)__a - (uint64x2_t)__b);
}

__C17_INTRIN uint64x2_t vqaddq_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 64);
  return __r;
}

__C17_INTRIN uint64x2_t vqsubq_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] - __b[__i], 64);
  return __r;
}

__C17_INTRIN uint64x2_t vsqaddq_u64(uint64x2_t __a, int64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_u((__int128)__a[__i] + __b[__i], 64);
  return __r;
}

__C17_INTRIN float32x2_t vadd_f32(float32x2_t __a, float32x2_t __b)
{
  return __a + __b;
}

__C17_INTRIN float32x2_t vsub_f32(float32x2_t __a, float32x2_t __b)
{
  return __a - __b;
}

__C17_INTRIN float32x2_t vmul_f32(float32x2_t __a, float32x2_t __b)
{
  return __a * __b;
}

__C17_INTRIN float32x2_t vdiv_f32(float32x2_t __a, float32x2_t __b)
{
  return __a / __b;
}

__C17_INTRIN float32x2_t vmla_f32(float32x2_t __a, float32x2_t __b, float32x2_t __c)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] + __c17_fmul_f32(__b[__i], __c[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vmls_f32(float32x2_t __a, float32x2_t __b, float32x2_t __c)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] - __c17_fmul_f32(__b[__i], __c[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vfma_f32(float32x2_t __a, float32x2_t __b, float32x2_t __c)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f32(__b[__i], __c[__i], __a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vfms_f32(float32x2_t __a, float32x2_t __b, float32x2_t __c)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f32(__c17_fneg_f32(__b[__i]), __c[__i], __a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vmulx_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fmulx_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vmul_n_f32(float32x2_t __a, float32_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] * __b;
  return __r;
}

__C17_INTRIN float32x2_t vmla_n_f32(float32x2_t __a, float32x2_t __b, float32_t __c)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] + __c17_fmul_f32(__b[__i], __c);
  return __r;
}

__C17_INTRIN float32x2_t vmls_n_f32(float32x2_t __a, float32x2_t __b, float32_t __c)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] - __c17_fmul_f32(__b[__i], __c);
  return __r;
}

__C17_INTRIN float32x2_t vfma_n_f32(float32x2_t __a, float32x2_t __b, float32_t __c)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f32(__b[__i], __c, __a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vfms_n_f32(float32x2_t __a, float32x2_t __b, float32_t __c)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f32(__c17_fneg_f32(__b[__i]), __c, __a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t __c17_vmul_lane_f32(float32x2_t __a, float32x2_t __b, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] * __b[__lane];
  return __r;
}
#define vmul_lane_f32(...) __c17_vmul_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vmla_lane_f32(float32x2_t __a, float32x2_t __b, float32x2_t __c, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] + __c17_fmul_f32(__b[__i], __c[__lane]);
  return __r;
}
#define vmla_lane_f32(...) __c17_vmla_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vmls_lane_f32(float32x2_t __a, float32x2_t __b, float32x2_t __c, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] - __c17_fmul_f32(__b[__i], __c[__lane]);
  return __r;
}
#define vmls_lane_f32(...) __c17_vmls_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vfma_lane_f32(float32x2_t __a, float32x2_t __b, float32x2_t __c, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f32(__b[__i], __c[__lane], __a[__i]);
  return __r;
}
#define vfma_lane_f32(...) __c17_vfma_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vfms_lane_f32(float32x2_t __a, float32x2_t __b, float32x2_t __c, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f32(__c17_fneg_f32(__b[__i]), __c[__lane], __a[__i]);
  return __r;
}
#define vfms_lane_f32(...) __c17_vfms_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vmulx_lane_f32(float32x2_t __a, float32x2_t __b, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fmulx_f32(__a[__i], __b[__lane]);
  return __r;
}
#define vmulx_lane_f32(...) __c17_vmulx_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vmul_laneq_f32(float32x2_t __a, float32x4_t __b, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] * __b[__lane];
  return __r;
}
#define vmul_laneq_f32(...) __c17_vmul_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vmla_laneq_f32(float32x2_t __a, float32x2_t __b, float32x4_t __c, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] + __c17_fmul_f32(__b[__i], __c[__lane]);
  return __r;
}
#define vmla_laneq_f32(...) __c17_vmla_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vmls_laneq_f32(float32x2_t __a, float32x2_t __b, float32x4_t __c, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] - __c17_fmul_f32(__b[__i], __c[__lane]);
  return __r;
}
#define vmls_laneq_f32(...) __c17_vmls_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vfma_laneq_f32(float32x2_t __a, float32x2_t __b, float32x4_t __c, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f32(__b[__i], __c[__lane], __a[__i]);
  return __r;
}
#define vfma_laneq_f32(...) __c17_vfma_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vfms_laneq_f32(float32x2_t __a, float32x2_t __b, float32x4_t __c, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f32(__c17_fneg_f32(__b[__i]), __c[__lane], __a[__i]);
  return __r;
}
#define vfms_laneq_f32(...) __c17_vfms_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t __c17_vmulx_laneq_f32(float32x2_t __a, float32x4_t __b, const int __lane)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fmulx_f32(__a[__i], __b[__lane]);
  return __r;
}
#define vmulx_laneq_f32(...) __c17_vmulx_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t vabs_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fabs_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vneg_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fneg_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vabd_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fabs_f32(__a[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vmax_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fmax_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vmin_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fmin_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vmaxnm_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fmaxnm_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vminnm_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fminnm_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vsqrt_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fsqrt_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vrecpe_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_frecpe_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vrsqrte_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_frsqrte_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vrecps_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_frecps_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vrsqrts_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_frsqrts_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vrnd_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rndz_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vrndn_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rndn_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vrnda_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rnda_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vrndm_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rndm_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vrndp_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rndp_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vrndx_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rndx_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x2_t vrndi_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rndi_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vaddq_f32(float32x4_t __a, float32x4_t __b)
{
  return __a + __b;
}

__C17_INTRIN float32x4_t vsubq_f32(float32x4_t __a, float32x4_t __b)
{
  return __a - __b;
}

__C17_INTRIN float32x4_t vmulq_f32(float32x4_t __a, float32x4_t __b)
{
  return __a * __b;
}

__C17_INTRIN float32x4_t vdivq_f32(float32x4_t __a, float32x4_t __b)
{
  return __a / __b;
}

__C17_INTRIN float32x4_t vmlaq_f32(float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] + __c17_fmul_f32(__b[__i], __c[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vmlsq_f32(float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] - __c17_fmul_f32(__b[__i], __c[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vfmaq_f32(float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fma_f32(__b[__i], __c[__i], __a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vfmsq_f32(float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fma_f32(__c17_fneg_f32(__b[__i]), __c[__i], __a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vmulxq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fmulx_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vmulq_n_f32(float32x4_t __a, float32_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] * __b;
  return __r;
}

__C17_INTRIN float32x4_t vmlaq_n_f32(float32x4_t __a, float32x4_t __b, float32_t __c)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] + __c17_fmul_f32(__b[__i], __c);
  return __r;
}

__C17_INTRIN float32x4_t vmlsq_n_f32(float32x4_t __a, float32x4_t __b, float32_t __c)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] - __c17_fmul_f32(__b[__i], __c);
  return __r;
}

__C17_INTRIN float32x4_t vfmaq_n_f32(float32x4_t __a, float32x4_t __b, float32_t __c)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fma_f32(__b[__i], __c, __a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vfmsq_n_f32(float32x4_t __a, float32x4_t __b, float32_t __c)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fma_f32(__c17_fneg_f32(__b[__i]), __c, __a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t __c17_vmulq_lane_f32(float32x4_t __a, float32x2_t __b, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] * __b[__lane];
  return __r;
}
#define vmulq_lane_f32(...) __c17_vmulq_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vmlaq_lane_f32(float32x4_t __a, float32x4_t __b, float32x2_t __c, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] + __c17_fmul_f32(__b[__i], __c[__lane]);
  return __r;
}
#define vmlaq_lane_f32(...) __c17_vmlaq_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vmlsq_lane_f32(float32x4_t __a, float32x4_t __b, float32x2_t __c, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] - __c17_fmul_f32(__b[__i], __c[__lane]);
  return __r;
}
#define vmlsq_lane_f32(...) __c17_vmlsq_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vfmaq_lane_f32(float32x4_t __a, float32x4_t __b, float32x2_t __c, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fma_f32(__b[__i], __c[__lane], __a[__i]);
  return __r;
}
#define vfmaq_lane_f32(...) __c17_vfmaq_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vfmsq_lane_f32(float32x4_t __a, float32x4_t __b, float32x2_t __c, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fma_f32(__c17_fneg_f32(__b[__i]), __c[__lane], __a[__i]);
  return __r;
}
#define vfmsq_lane_f32(...) __c17_vfmsq_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vmulxq_lane_f32(float32x4_t __a, float32x2_t __b, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fmulx_f32(__a[__i], __b[__lane]);
  return __r;
}
#define vmulxq_lane_f32(...) __c17_vmulxq_lane_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vmulq_laneq_f32(float32x4_t __a, float32x4_t __b, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] * __b[__lane];
  return __r;
}
#define vmulq_laneq_f32(...) __c17_vmulq_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vmlaq_laneq_f32(float32x4_t __a, float32x4_t __b, float32x4_t __c, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] + __c17_fmul_f32(__b[__i], __c[__lane]);
  return __r;
}
#define vmlaq_laneq_f32(...) __c17_vmlaq_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vmlsq_laneq_f32(float32x4_t __a, float32x4_t __b, float32x4_t __c, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] - __c17_fmul_f32(__b[__i], __c[__lane]);
  return __r;
}
#define vmlsq_laneq_f32(...) __c17_vmlsq_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vfmaq_laneq_f32(float32x4_t __a, float32x4_t __b, float32x4_t __c, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fma_f32(__b[__i], __c[__lane], __a[__i]);
  return __r;
}
#define vfmaq_laneq_f32(...) __c17_vfmaq_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vfmsq_laneq_f32(float32x4_t __a, float32x4_t __b, float32x4_t __c, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fma_f32(__c17_fneg_f32(__b[__i]), __c[__lane], __a[__i]);
  return __r;
}
#define vfmsq_laneq_f32(...) __c17_vfmsq_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t __c17_vmulxq_laneq_f32(float32x4_t __a, float32x4_t __b, const int __lane)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fmulx_f32(__a[__i], __b[__lane]);
  return __r;
}
#define vmulxq_laneq_f32(...) __c17_vmulxq_laneq_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t vabsq_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fabs_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vnegq_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fneg_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vabdq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fabs_f32(__a[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vmaxq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fmax_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vminq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fmin_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vmaxnmq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fmaxnm_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vminnmq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fminnm_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vsqrtq_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_fsqrt_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vrecpeq_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_frecpe_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vrsqrteq_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_frsqrte_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vrecpsq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_frecps_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vrsqrtsq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_frsqrts_f32(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vrndq_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_rndz_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vrndnq_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_rndn_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vrndaq_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_rnda_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vrndmq_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_rndm_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vrndpq_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_rndp_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vrndxq_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_rndx_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vrndiq_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_rndi_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vadd_f64(float64x1_t __a, float64x1_t __b)
{
  return __a + __b;
}

__C17_INTRIN float64x1_t vsub_f64(float64x1_t __a, float64x1_t __b)
{
  return __a - __b;
}

__C17_INTRIN float64x1_t vmul_f64(float64x1_t __a, float64x1_t __b)
{
  return __a * __b;
}

__C17_INTRIN float64x1_t vdiv_f64(float64x1_t __a, float64x1_t __b)
{
  return __a / __b;
}

__C17_INTRIN float64x1_t vmla_f64(float64x1_t __a, float64x1_t __b, float64x1_t __c)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i] + __c17_fmul_f64(__b[__i], __c[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vmls_f64(float64x1_t __a, float64x1_t __b, float64x1_t __c)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i] - __c17_fmul_f64(__b[__i], __c[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vfma_f64(float64x1_t __a, float64x1_t __b, float64x1_t __c)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fma_f64(__b[__i], __c[__i], __a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vfms_f64(float64x1_t __a, float64x1_t __b, float64x1_t __c)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fma_f64(__c17_fneg_f64(__b[__i]), __c[__i], __a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vmulx_f64(float64x1_t __a, float64x1_t __b)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fmulx_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vmul_n_f64(float64x1_t __a, float64_t __b)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i] * __b;
  return __r;
}

__C17_INTRIN float64x1_t vfma_n_f64(float64x1_t __a, float64x1_t __b, float64_t __c)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fma_f64(__b[__i], __c, __a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vfms_n_f64(float64x1_t __a, float64x1_t __b, float64_t __c)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fma_f64(__c17_fneg_f64(__b[__i]), __c, __a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t __c17_vmul_lane_f64(float64x1_t __a, float64x1_t __b, const int __lane)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i] * __b[__lane];
  return __r;
}
#define vmul_lane_f64(...) __c17_vmul_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t __c17_vfma_lane_f64(float64x1_t __a, float64x1_t __b, float64x1_t __c, const int __lane)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fma_f64(__b[__i], __c[__lane], __a[__i]);
  return __r;
}
#define vfma_lane_f64(...) __c17_vfma_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t __c17_vfms_lane_f64(float64x1_t __a, float64x1_t __b, float64x1_t __c, const int __lane)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fma_f64(__c17_fneg_f64(__b[__i]), __c[__lane], __a[__i]);
  return __r;
}
#define vfms_lane_f64(...) __c17_vfms_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t __c17_vmulx_lane_f64(float64x1_t __a, float64x1_t __b, const int __lane)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fmulx_f64(__a[__i], __b[__lane]);
  return __r;
}
#define vmulx_lane_f64(...) __c17_vmulx_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t __c17_vmul_laneq_f64(float64x1_t __a, float64x2_t __b, const int __lane)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i] * __b[__lane];
  return __r;
}
#define vmul_laneq_f64(...) __c17_vmul_laneq_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t __c17_vfma_laneq_f64(float64x1_t __a, float64x1_t __b, float64x2_t __c, const int __lane)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fma_f64(__b[__i], __c[__lane], __a[__i]);
  return __r;
}
#define vfma_laneq_f64(...) __c17_vfma_laneq_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t __c17_vfms_laneq_f64(float64x1_t __a, float64x1_t __b, float64x2_t __c, const int __lane)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fma_f64(__c17_fneg_f64(__b[__i]), __c[__lane], __a[__i]);
  return __r;
}
#define vfms_laneq_f64(...) __c17_vfms_laneq_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t __c17_vmulx_laneq_f64(float64x1_t __a, float64x2_t __b, const int __lane)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fmulx_f64(__a[__i], __b[__lane]);
  return __r;
}
#define vmulx_laneq_f64(...) __c17_vmulx_laneq_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t vabs_f64(float64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fabs_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vneg_f64(float64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fneg_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vabd_f64(float64x1_t __a, float64x1_t __b)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fabs_f64(__a[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vmax_f64(float64x1_t __a, float64x1_t __b)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fmax_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vmin_f64(float64x1_t __a, float64x1_t __b)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fmin_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vmaxnm_f64(float64x1_t __a, float64x1_t __b)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fmaxnm_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vminnm_f64(float64x1_t __a, float64x1_t __b)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fminnm_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vsqrt_f64(float64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_fsqrt_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vrecpe_f64(float64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_frecpe_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vrsqrte_f64(float64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_frsqrte_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vrecps_f64(float64x1_t __a, float64x1_t __b)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_frecps_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vrsqrts_f64(float64x1_t __a, float64x1_t __b)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_frsqrts_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vrnd_f64(float64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_rndz_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vrndn_f64(float64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_rndn_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vrnda_f64(float64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_rnda_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vrndm_f64(float64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_rndm_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vrndp_f64(float64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_rndp_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vrndx_f64(float64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_rndx_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x1_t vrndi_f64(float64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_rndi_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vaddq_f64(float64x2_t __a, float64x2_t __b)
{
  return __a + __b;
}

__C17_INTRIN float64x2_t vsubq_f64(float64x2_t __a, float64x2_t __b)
{
  return __a - __b;
}

__C17_INTRIN float64x2_t vmulq_f64(float64x2_t __a, float64x2_t __b)
{
  return __a * __b;
}

__C17_INTRIN float64x2_t vdivq_f64(float64x2_t __a, float64x2_t __b)
{
  return __a / __b;
}

__C17_INTRIN float64x2_t vmlaq_f64(float64x2_t __a, float64x2_t __b, float64x2_t __c)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] + __c17_fmul_f64(__b[__i], __c[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vmlsq_f64(float64x2_t __a, float64x2_t __b, float64x2_t __c)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] - __c17_fmul_f64(__b[__i], __c[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vfmaq_f64(float64x2_t __a, float64x2_t __b, float64x2_t __c)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f64(__b[__i], __c[__i], __a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vfmsq_f64(float64x2_t __a, float64x2_t __b, float64x2_t __c)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f64(__c17_fneg_f64(__b[__i]), __c[__i], __a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vmulxq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fmulx_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vmulq_n_f64(float64x2_t __a, float64_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] * __b;
  return __r;
}

__C17_INTRIN float64x2_t vfmaq_n_f64(float64x2_t __a, float64x2_t __b, float64_t __c)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f64(__b[__i], __c, __a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vfmsq_n_f64(float64x2_t __a, float64x2_t __b, float64_t __c)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f64(__c17_fneg_f64(__b[__i]), __c, __a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t __c17_vmulq_lane_f64(float64x2_t __a, float64x1_t __b, const int __lane)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] * __b[__lane];
  return __r;
}
#define vmulq_lane_f64(...) __c17_vmulq_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t __c17_vfmaq_lane_f64(float64x2_t __a, float64x2_t __b, float64x1_t __c, const int __lane)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f64(__b[__i], __c[__lane], __a[__i]);
  return __r;
}
#define vfmaq_lane_f64(...) __c17_vfmaq_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t __c17_vfmsq_lane_f64(float64x2_t __a, float64x2_t __b, float64x1_t __c, const int __lane)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f64(__c17_fneg_f64(__b[__i]), __c[__lane], __a[__i]);
  return __r;
}
#define vfmsq_lane_f64(...) __c17_vfmsq_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t __c17_vmulxq_lane_f64(float64x2_t __a, float64x1_t __b, const int __lane)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fmulx_f64(__a[__i], __b[__lane]);
  return __r;
}
#define vmulxq_lane_f64(...) __c17_vmulxq_lane_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t __c17_vmulq_laneq_f64(float64x2_t __a, float64x2_t __b, const int __lane)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] * __b[__lane];
  return __r;
}
#define vmulq_laneq_f64(...) __c17_vmulq_laneq_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t __c17_vfmaq_laneq_f64(float64x2_t __a, float64x2_t __b, float64x2_t __c, const int __lane)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f64(__b[__i], __c[__lane], __a[__i]);
  return __r;
}
#define vfmaq_laneq_f64(...) __c17_vfmaq_laneq_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t __c17_vfmsq_laneq_f64(float64x2_t __a, float64x2_t __b, float64x2_t __c, const int __lane)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fma_f64(__c17_fneg_f64(__b[__i]), __c[__lane], __a[__i]);
  return __r;
}
#define vfmsq_laneq_f64(...) __c17_vfmsq_laneq_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t __c17_vmulxq_laneq_f64(float64x2_t __a, float64x2_t __b, const int __lane)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fmulx_f64(__a[__i], __b[__lane]);
  return __r;
}
#define vmulxq_laneq_f64(...) __c17_vmulxq_laneq_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t vabsq_f64(float64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fabs_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vnegq_f64(float64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fneg_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vabdq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fabs_f64(__a[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vmaxq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fmax_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vminq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fmin_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vmaxnmq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fmaxnm_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vminnmq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fminnm_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vsqrtq_f64(float64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_fsqrt_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vrecpeq_f64(float64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_frecpe_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vrsqrteq_f64(float64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_frsqrte_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vrecpsq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_frecps_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vrsqrtsq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_frsqrts_f64(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vrndq_f64(float64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rndz_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vrndnq_f64(float64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rndn_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vrndaq_f64(float64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rnda_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vrndmq_f64(float64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rndm_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vrndpq_f64(float64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rndp_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vrndxq_f64(float64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rndx_f64(__a[__i]);
  return __r;
}

__C17_INTRIN float64x2_t vrndiq_f64(float64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_rndi_f64(__a[__i]);
  return __r;
}

__C17_INTRIN poly8x8_t vadd_p8(poly8x8_t __a, poly8x8_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN poly8x8_t vmul_p8(poly8x8_t __a, poly8x8_t __b)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (poly8_t)__c17_pmul8(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN poly8x16_t vaddq_p8(poly8x16_t __a, poly8x16_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN poly8x16_t vmulq_p8(poly8x16_t __a, poly8x16_t __b)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (poly8_t)__c17_pmul8(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN poly16x4_t vadd_p16(poly16x4_t __a, poly16x4_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN poly16x8_t vaddq_p16(poly16x8_t __a, poly16x8_t __b)
{
  return __a ^ __b;
}


/* Widening and narrowing. */

__C17_INTRIN int16x8_t vaddl_s8(int8x8_t __a, int8x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (int16_t)__a[__i] + (int16_t)__b[__i];
  return __r;
}

__C17_INTRIN int16x8_t vsubl_s8(int8x8_t __a, int8x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (int16_t)__a[__i] - (int16_t)__b[__i];
  return __r;
}

__C17_INTRIN int16x8_t vaddw_s8(int16x8_t __a, int8x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN int16x8_t vsubw_s8(int16x8_t __a, int8x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN int16x8_t vmull_s8(int8x8_t __a, int8x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (int16_t)((int16_t)__a[__i] * (int16_t)__b[__i]);
  return __r;
}

__C17_INTRIN int16x8_t vmlal_s8(int16x8_t __a, int8x8_t __b, int8x8_t __c)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int16_t)((int16_t)__b[__i] * (int16_t)__c[__i]);
  return __r;
}

__C17_INTRIN int16x8_t vmlsl_s8(int16x8_t __a, int8x8_t __b, int8x8_t __c)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int16_t)((int16_t)__b[__i] * (int16_t)__c[__i]);
  return __r;
}

__C17_INTRIN int16x8_t vabdl_s8(int8x8_t __a, int8x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i]);
  return __r;
}

__C17_INTRIN int16x8_t vabal_s8(int16x8_t __a, int8x8_t __b, int8x8_t __c)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN int16x8_t vmovl_s8(int8x8_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN int16x8_t __c17_vshll_n_s8(int8x8_t __a, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] << __n;
  return __r;
}
#define vshll_n_s8(...) __c17_vshll_n_s8(__VA_ARGS__)

__C17_INTRIN int16x8_t vaddl_high_s8(int8x16_t __a, int8x16_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (int16_t)__a[__i + 8] + (int16_t)__b[__i + 8];
  return __r;
}

__C17_INTRIN int16x8_t vsubl_high_s8(int8x16_t __a, int8x16_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (int16_t)__a[__i + 8] - (int16_t)__b[__i + 8];
  return __r;
}

__C17_INTRIN int16x8_t vaddw_high_s8(int16x8_t __a, int8x16_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i + 8];
  return __r;
}

__C17_INTRIN int16x8_t vsubw_high_s8(int16x8_t __a, int8x16_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i + 8];
  return __r;
}

__C17_INTRIN int16x8_t vmull_high_s8(int8x16_t __a, int8x16_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (int16_t)((int16_t)__a[__i + 8] * (int16_t)__b[__i + 8]);
  return __r;
}

__C17_INTRIN int16x8_t vmlal_high_s8(int16x8_t __a, int8x16_t __b, int8x16_t __c)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int16_t)((int16_t)__b[__i + 8] * (int16_t)__c[__i + 8]);
  return __r;
}

__C17_INTRIN int16x8_t vmlsl_high_s8(int16x8_t __a, int8x16_t __b, int8x16_t __c)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int16_t)((int16_t)__b[__i + 8] * (int16_t)__c[__i + 8]);
  return __r;
}

__C17_INTRIN int16x8_t vabdl_high_s8(int8x16_t __a, int8x16_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__a[__i + 8] > __b[__i + 8] ? (int64_t)__a[__i + 8] - __b[__i + 8] : (int64_t)__b[__i + 8] - __a[__i + 8]);
  return __r;
}

__C17_INTRIN int16x8_t vabal_high_s8(int16x8_t __a, int8x16_t __b, int8x16_t __c)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i + 8] > __c[__i + 8] ? (int64_t)__b[__i + 8] - __c[__i + 8] : (int64_t)__c[__i + 8] - __b[__i + 8]);
  return __r;
}

__C17_INTRIN int16x8_t vmovl_high_s8(int8x16_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i + 8];
  return __r;
}

__C17_INTRIN int16x8_t __c17_vshll_high_n_s8(int8x16_t __a, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i + 8] << __n;
  return __r;
}
#define vshll_high_n_s8(...) __c17_vshll_high_n_s8(__VA_ARGS__)

__C17_INTRIN int32x4_t vaddl_s16(int16x4_t __a, int16x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)__a[__i] + (int32_t)__b[__i];
  return __r;
}

__C17_INTRIN int32x4_t vsubl_s16(int16x4_t __a, int16x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)__a[__i] - (int32_t)__b[__i];
  return __r;
}

__C17_INTRIN int32x4_t vaddw_s16(int32x4_t __a, int16x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN int32x4_t vsubw_s16(int32x4_t __a, int16x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN int32x4_t vmull_s16(int16x4_t __a, int16x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)((int32_t)__a[__i] * (int32_t)__b[__i]);
  return __r;
}

__C17_INTRIN int32x4_t vmlal_s16(int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int32_t)((int32_t)__b[__i] * (int32_t)__c[__i]);
  return __r;
}

__C17_INTRIN int32x4_t vmlsl_s16(int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int32_t)((int32_t)__b[__i] * (int32_t)__c[__i]);
  return __r;
}

__C17_INTRIN int32x4_t vabdl_s16(int16x4_t __a, int16x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i]);
  return __r;
}

__C17_INTRIN int32x4_t vabal_s16(int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN int32x4_t vmovl_s16(int16x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN int32x4_t __c17_vshll_n_s16(int16x4_t __a, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] << __n;
  return __r;
}
#define vshll_n_s16(...) __c17_vshll_n_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t vmull_n_s16(int16x4_t __a, int16_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)((int32_t)__a[__i] * (int32_t)__b);
  return __r;
}

__C17_INTRIN int32x4_t vmlal_n_s16(int32x4_t __a, int16x4_t __b, int16_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int32_t)((int32_t)__b[__i] * (int32_t)__c);
  return __r;
}

__C17_INTRIN int32x4_t vmlsl_n_s16(int32x4_t __a, int16x4_t __b, int16_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int32_t)((int32_t)__b[__i] * (int32_t)__c);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vmull_lane_s16(int16x4_t __a, int16x4_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)((int32_t)__a[__i] * (int32_t)__b[__lane]);
  return __r;
}
#define vmull_lane_s16(...) __c17_vmull_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmlal_lane_s16(int32x4_t __a, int16x4_t __b, int16x4_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int32_t)((int32_t)__b[__i] * (int32_t)__c[__lane]);
  return __r;
}
#define vmlal_lane_s16(...) __c17_vmlal_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmlsl_lane_s16(int32x4_t __a, int16x4_t __b, int16x4_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int32_t)((int32_t)__b[__i] * (int32_t)__c[__lane]);
  return __r;
}
#define vmlsl_lane_s16(...) __c17_vmlsl_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmull_laneq_s16(int16x4_t __a, int16x8_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)((int32_t)__a[__i] * (int32_t)__b[__lane]);
  return __r;
}
#define vmull_laneq_s16(...) __c17_vmull_laneq_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmlal_laneq_s16(int32x4_t __a, int16x4_t __b, int16x8_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int32_t)((int32_t)__b[__i] * (int32_t)__c[__lane]);
  return __r;
}
#define vmlal_laneq_s16(...) __c17_vmlal_laneq_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmlsl_laneq_s16(int32x4_t __a, int16x4_t __b, int16x8_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int32_t)((int32_t)__b[__i] * (int32_t)__c[__lane]);
  return __r;
}
#define vmlsl_laneq_s16(...) __c17_vmlsl_laneq_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t vqdmull_s16(int16x4_t __a, int16x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i] * __b[__i], 32);
  return __r;
}

__C17_INTRIN int32x4_t vqdmull_n_s16(int16x4_t __a, int16_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i] * __b, 32);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vqdmull_lane_s16(int16x4_t __a, int16x4_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i] * __b[__lane], 32);
  return __r;
}
#define vqdmull_lane_s16(...) __c17_vqdmull_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqdmull_laneq_s16(int16x4_t __a, int16x8_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i] * __b[__lane], 32);
  return __r;
}
#define vqdmull_laneq_s16(...) __c17_vqdmull_laneq_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t vqdmlal_s16(int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i] * __c[__i], 32), 32);
  return __r;
}

__C17_INTRIN int32x4_t vqdmlal_n_s16(int32x4_t __a, int16x4_t __b, int16_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i] * __c, 32), 32);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vqdmlal_lane_s16(int32x4_t __a, int16x4_t __b, int16x4_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i] * __c[__lane], 32), 32);
  return __r;
}
#define vqdmlal_lane_s16(...) __c17_vqdmlal_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqdmlal_laneq_s16(int32x4_t __a, int16x4_t __b, int16x8_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i] * __c[__lane], 32), 32);
  return __r;
}
#define vqdmlal_laneq_s16(...) __c17_vqdmlal_laneq_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t vqdmlsl_s16(int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i] * __c[__i], 32), 32);
  return __r;
}

__C17_INTRIN int32x4_t vqdmlsl_n_s16(int32x4_t __a, int16x4_t __b, int16_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i] * __c, 32), 32);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vqdmlsl_lane_s16(int32x4_t __a, int16x4_t __b, int16x4_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i] * __c[__lane], 32), 32);
  return __r;
}
#define vqdmlsl_lane_s16(...) __c17_vqdmlsl_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqdmlsl_laneq_s16(int32x4_t __a, int16x4_t __b, int16x8_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i] * __c[__lane], 32), 32);
  return __r;
}
#define vqdmlsl_laneq_s16(...) __c17_vqdmlsl_laneq_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t vaddl_high_s16(int16x8_t __a, int16x8_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)__a[__i + 4] + (int32_t)__b[__i + 4];
  return __r;
}

__C17_INTRIN int32x4_t vsubl_high_s16(int16x8_t __a, int16x8_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)__a[__i + 4] - (int32_t)__b[__i + 4];
  return __r;
}

__C17_INTRIN int32x4_t vaddw_high_s16(int32x4_t __a, int16x8_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i + 4];
  return __r;
}

__C17_INTRIN int32x4_t vsubw_high_s16(int32x4_t __a, int16x8_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i + 4];
  return __r;
}

__C17_INTRIN int32x4_t vmull_high_s16(int16x8_t __a, int16x8_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)((int32_t)__a[__i + 4] * (int32_t)__b[__i + 4]);
  return __r;
}

__C17_INTRIN int32x4_t vmlal_high_s16(int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int32_t)((int32_t)__b[__i + 4] * (int32_t)__c[__i + 4]);
  return __r;
}

__C17_INTRIN int32x4_t vmlsl_high_s16(int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int32_t)((int32_t)__b[__i + 4] * (int32_t)__c[__i + 4]);
  return __r;
}

__C17_INTRIN int32x4_t vabdl_high_s16(int16x8_t __a, int16x8_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__a[__i + 4] > __b[__i + 4] ? (int64_t)__a[__i + 4] - __b[__i + 4] : (int64_t)__b[__i + 4] - __a[__i + 4]);
  return __r;
}

__C17_INTRIN int32x4_t vabal_high_s16(int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i + 4] > __c[__i + 4] ? (int64_t)__b[__i + 4] - __c[__i + 4] : (int64_t)__c[__i + 4] - __b[__i + 4]);
  return __r;
}

__C17_INTRIN int32x4_t vmovl_high_s16(int16x8_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i + 4];
  return __r;
}

__C17_INTRIN int32x4_t __c17_vshll_high_n_s16(int16x8_t __a, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i + 4] << __n;
  return __r;
}
#define vshll_high_n_s16(...) __c17_vshll_high_n_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t vmull_high_n_s16(int16x8_t __a, int16_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)((int32_t)__a[__i + 4] * (int32_t)__b);
  return __r;
}

__C17_INTRIN int32x4_t vmlal_high_n_s16(int32x4_t __a, int16x8_t __b, int16_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int32_t)((int32_t)__b[__i + 4] * (int32_t)__c);
  return __r;
}

__C17_INTRIN int32x4_t vmlsl_high_n_s16(int32x4_t __a, int16x8_t __b, int16_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int32_t)((int32_t)__b[__i + 4] * (int32_t)__c);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vmull_high_lane_s16(int16x8_t __a, int16x4_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)((int32_t)__a[__i + 4] * (int32_t)__b[__lane]);
  return __r;
}
#define vmull_high_lane_s16(...) __c17_vmull_high_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmlal_high_lane_s16(int32x4_t __a, int16x8_t __b, int16x4_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int32_t)((int32_t)__b[__i + 4] * (int32_t)__c[__lane]);
  return __r;
}
#define vmlal_high_lane_s16(...) __c17_vmlal_high_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmlsl_high_lane_s16(int32x4_t __a, int16x8_t __b, int16x4_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int32_t)((int32_t)__b[__i + 4] * (int32_t)__c[__lane]);
  return __r;
}
#define vmlsl_high_lane_s16(...) __c17_vmlsl_high_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmull_high_laneq_s16(int16x8_t __a, int16x8_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)((int32_t)__a[__i + 4] * (int32_t)__b[__lane]);
  return __r;
}
#define vmull_high_laneq_s16(...) __c17_vmull_high_laneq_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmlal_high_laneq_s16(int32x4_t __a, int16x8_t __b, int16x8_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int32_t)((int32_t)__b[__i + 4] * (int32_t)__c[__lane]);
  return __r;
}
#define vmlal_high_laneq_s16(...) __c17_vmlal_high_laneq_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vmlsl_high_laneq_s16(int32x4_t __a, int16x8_t __b, int16x8_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int32_t)((int32_t)__b[__i + 4] * (int32_t)__c[__lane]);
  return __r;
}
#define vmlsl_high_laneq_s16(...) __c17_vmlsl_high_laneq_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t vqdmull_high_s16(int16x8_t __a, int16x8_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i + 4] * __b[__i + 4], 32);
  return __r;
}

__C17_INTRIN int32x4_t vqdmull_high_n_s16(int16x8_t __a, int16_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i + 4] * __b, 32);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vqdmull_high_lane_s16(int16x8_t __a, int16x4_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i + 4] * __b[__lane], 32);
  return __r;
}
#define vqdmull_high_lane_s16(...) __c17_vqdmull_high_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqdmull_high_laneq_s16(int16x8_t __a, int16x8_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i + 4] * __b[__lane], 32);
  return __r;
}
#define vqdmull_high_laneq_s16(...) __c17_vqdmull_high_laneq_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t vqdmlal_high_s16(int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i + 4] * __c[__i + 4], 32), 32);
  return __r;
}

__C17_INTRIN int32x4_t vqdmlal_high_n_s16(int32x4_t __a, int16x8_t __b, int16_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i + 4] * __c, 32), 32);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vqdmlal_high_lane_s16(int32x4_t __a, int16x8_t __b, int16x4_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i + 4] * __c[__lane], 32), 32);
  return __r;
}
#define vqdmlal_high_lane_s16(...) __c17_vqdmlal_high_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqdmlal_high_laneq_s16(int32x4_t __a, int16x8_t __b, int16x8_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i + 4] * __c[__lane], 32), 32);
  return __r;
}
#define vqdmlal_high_laneq_s16(...) __c17_vqdmlal_high_laneq_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t vqdmlsl_high_s16(int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i + 4] * __c[__i + 4], 32), 32);
  return __r;
}

__C17_INTRIN int32x4_t vqdmlsl_high_n_s16(int32x4_t __a, int16x8_t __b, int16_t __c)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i + 4] * __c, 32), 32);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vqdmlsl_high_lane_s16(int32x4_t __a, int16x8_t __b, int16x4_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i + 4] * __c[__lane], 32), 32);
  return __r;
}
#define vqdmlsl_high_lane_s16(...) __c17_vqdmlsl_high_lane_s16(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqdmlsl_high_laneq_s16(int32x4_t __a, int16x8_t __b, int16x8_t __c, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i + 4] * __c[__lane], 32), 32);
  return __r;
}
#define vqdmlsl_high_laneq_s16(...) __c17_vqdmlsl_high_laneq_s16(__VA_ARGS__)

__C17_INTRIN int64x2_t vaddl_s32(int32x2_t __a, int32x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)__a[__i] + (int64_t)__b[__i];
  return __r;
}

__C17_INTRIN int64x2_t vsubl_s32(int32x2_t __a, int32x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)__a[__i] - (int64_t)__b[__i];
  return __r;
}

__C17_INTRIN int64x2_t vaddw_s32(int64x2_t __a, int32x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN int64x2_t vsubw_s32(int64x2_t __a, int32x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN int64x2_t vmull_s32(int32x2_t __a, int32x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)((int64_t)__a[__i] * (int64_t)__b[__i]);
  return __r;
}

__C17_INTRIN int64x2_t vmlal_s32(int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int64_t)((int64_t)__b[__i] * (int64_t)__c[__i]);
  return __r;
}

__C17_INTRIN int64x2_t vmlsl_s32(int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int64_t)((int64_t)__b[__i] * (int64_t)__c[__i]);
  return __r;
}

__C17_INTRIN int64x2_t vabdl_s32(int32x2_t __a, int32x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i]);
  return __r;
}

__C17_INTRIN int64x2_t vabal_s32(int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN int64x2_t vmovl_s32(int32x2_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN int64x2_t __c17_vshll_n_s32(int32x2_t __a, const int __n)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] << __n;
  return __r;
}
#define vshll_n_s32(...) __c17_vshll_n_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t vmull_n_s32(int32x2_t __a, int32_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)((int64_t)__a[__i] * (int64_t)__b);
  return __r;
}

__C17_INTRIN int64x2_t vmlal_n_s32(int64x2_t __a, int32x2_t __b, int32_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int64_t)((int64_t)__b[__i] * (int64_t)__c);
  return __r;
}

__C17_INTRIN int64x2_t vmlsl_n_s32(int64x2_t __a, int32x2_t __b, int32_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int64_t)((int64_t)__b[__i] * (int64_t)__c);
  return __r;
}

__C17_INTRIN int64x2_t __c17_vmull_lane_s32(int32x2_t __a, int32x2_t __b, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)((int64_t)__a[__i] * (int64_t)__b[__lane]);
  return __r;
}
#define vmull_lane_s32(...) __c17_vmull_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vmlal_lane_s32(int64x2_t __a, int32x2_t __b, int32x2_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int64_t)((int64_t)__b[__i] * (int64_t)__c[__lane]);
  return __r;
}
#define vmlal_lane_s32(...) __c17_vmlal_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vmlsl_lane_s32(int64x2_t __a, int32x2_t __b, int32x2_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int64_t)((int64_t)__b[__i] * (int64_t)__c[__lane]);
  return __r;
}
#define vmlsl_lane_s32(...) __c17_vmlsl_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vmull_laneq_s32(int32x2_t __a, int32x4_t __b, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)((int64_t)__a[__i] * (int64_t)__b[__lane]);
  return __r;
}
#define vmull_laneq_s32(...) __c17_vmull_laneq_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vmlal_laneq_s32(int64x2_t __a, int32x2_t __b, int32x4_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int64_t)((int64_t)__b[__i] * (int64_t)__c[__lane]);
  return __r;
}
#define vmlal_laneq_s32(...) __c17_vmlal_laneq_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vmlsl_laneq_s32(int64x2_t __a, int32x2_t __b, int32x4_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int64_t)((int64_t)__b[__i] * (int64_t)__c[__lane]);
  return __r;
}
#define vmlsl_laneq_s32(...) __c17_vmlsl_laneq_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t vqdmull_s32(int32x2_t __a, int32x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i] * __b[__i], 64);
  return __r;
}

__C17_INTRIN int64x2_t vqdmull_n_s32(int32x2_t __a, int32_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i] * __b, 64);
  return __r;
}

__C17_INTRIN int64x2_t __c17_vqdmull_lane_s32(int32x2_t __a, int32x2_t __b, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i] * __b[__lane], 64);
  return __r;
}
#define vqdmull_lane_s32(...) __c17_vqdmull_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vqdmull_laneq_s32(int32x2_t __a, int32x4_t __b, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i] * __b[__lane], 64);
  return __r;
}
#define vqdmull_laneq_s32(...) __c17_vqdmull_laneq_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t vqdmlal_s32(int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i] * __c[__i], 64), 64);
  return __r;
}

__C17_INTRIN int64x2_t vqdmlal_n_s32(int64x2_t __a, int32x2_t __b, int32_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i] * __c, 64), 64);
  return __r;
}

__C17_INTRIN int64x2_t __c17_vqdmlal_lane_s32(int64x2_t __a, int32x2_t __b, int32x2_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i] * __c[__lane], 64), 64);
  return __r;
}
#define vqdmlal_lane_s32(...) __c17_vqdmlal_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vqdmlal_laneq_s32(int64x2_t __a, int32x2_t __b, int32x4_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i] * __c[__lane], 64), 64);
  return __r;
}
#define vqdmlal_laneq_s32(...) __c17_vqdmlal_laneq_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t vqdmlsl_s32(int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i] * __c[__i], 64), 64);
  return __r;
}

__C17_INTRIN int64x2_t vqdmlsl_n_s32(int64x2_t __a, int32x2_t __b, int32_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i] * __c, 64), 64);
  return __r;
}

__C17_INTRIN int64x2_t __c17_vqdmlsl_lane_s32(int64x2_t __a, int32x2_t __b, int32x2_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i] * __c[__lane], 64), 64);
  return __r;
}
#define vqdmlsl_lane_s32(...) __c17_vqdmlsl_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vqdmlsl_laneq_s32(int64x2_t __a, int32x2_t __b, int32x4_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i] * __c[__lane], 64), 64);
  return __r;
}
#define vqdmlsl_laneq_s32(...) __c17_vqdmlsl_laneq_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t vaddl_high_s32(int32x4_t __a, int32x4_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)__a[__i + 2] + (int64_t)__b[__i + 2];
  return __r;
}

__C17_INTRIN int64x2_t vsubl_high_s32(int32x4_t __a, int32x4_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)__a[__i + 2] - (int64_t)__b[__i + 2];
  return __r;
}

__C17_INTRIN int64x2_t vaddw_high_s32(int64x2_t __a, int32x4_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i + 2];
  return __r;
}

__C17_INTRIN int64x2_t vsubw_high_s32(int64x2_t __a, int32x4_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i + 2];
  return __r;
}

__C17_INTRIN int64x2_t vmull_high_s32(int32x4_t __a, int32x4_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)((int64_t)__a[__i + 2] * (int64_t)__b[__i + 2]);
  return __r;
}

__C17_INTRIN int64x2_t vmlal_high_s32(int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int64_t)((int64_t)__b[__i + 2] * (int64_t)__c[__i + 2]);
  return __r;
}

__C17_INTRIN int64x2_t vmlsl_high_s32(int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int64_t)((int64_t)__b[__i + 2] * (int64_t)__c[__i + 2]);
  return __r;
}

__C17_INTRIN int64x2_t vabdl_high_s32(int32x4_t __a, int32x4_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__a[__i + 2] > __b[__i + 2] ? (int64_t)__a[__i + 2] - __b[__i + 2] : (int64_t)__b[__i + 2] - __a[__i + 2]);
  return __r;
}

__C17_INTRIN int64x2_t vabal_high_s32(int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i + 2] > __c[__i + 2] ? (int64_t)__b[__i + 2] - __c[__i + 2] : (int64_t)__c[__i + 2] - __b[__i + 2]);
  return __r;
}

__C17_INTRIN int64x2_t vmovl_high_s32(int32x4_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i + 2];
  return __r;
}

__C17_INTRIN int64x2_t __c17_vshll_high_n_s32(int32x4_t __a, const int __n)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i + 2] << __n;
  return __r;
}
#define vshll_high_n_s32(...) __c17_vshll_high_n_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t vmull_high_n_s32(int32x4_t __a, int32_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)((int64_t)__a[__i + 2] * (int64_t)__b);
  return __r;
}

__C17_INTRIN int64x2_t vmlal_high_n_s32(int64x2_t __a, int32x4_t __b, int32_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int64_t)((int64_t)__b[__i + 2] * (int64_t)__c);
  return __r;
}

__C17_INTRIN int64x2_t vmlsl_high_n_s32(int64x2_t __a, int32x4_t __b, int32_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int64_t)((int64_t)__b[__i + 2] * (int64_t)__c);
  return __r;
}

__C17_INTRIN int64x2_t __c17_vmull_high_lane_s32(int32x4_t __a, int32x2_t __b, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)((int64_t)__a[__i + 2] * (int64_t)__b[__lane]);
  return __r;
}
#define vmull_high_lane_s32(...) __c17_vmull_high_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vmlal_high_lane_s32(int64x2_t __a, int32x4_t __b, int32x2_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int64_t)((int64_t)__b[__i + 2] * (int64_t)__c[__lane]);
  return __r;
}
#define vmlal_high_lane_s32(...) __c17_vmlal_high_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vmlsl_high_lane_s32(int64x2_t __a, int32x4_t __b, int32x2_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int64_t)((int64_t)__b[__i + 2] * (int64_t)__c[__lane]);
  return __r;
}
#define vmlsl_high_lane_s32(...) __c17_vmlsl_high_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vmull_high_laneq_s32(int32x4_t __a, int32x4_t __b, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)((int64_t)__a[__i + 2] * (int64_t)__b[__lane]);
  return __r;
}
#define vmull_high_laneq_s32(...) __c17_vmull_high_laneq_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vmlal_high_laneq_s32(int64x2_t __a, int32x4_t __b, int32x4_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(int64_t)((int64_t)__b[__i + 2] * (int64_t)__c[__lane]);
  return __r;
}
#define vmlal_high_laneq_s32(...) __c17_vmlal_high_laneq_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vmlsl_high_laneq_s32(int64x2_t __a, int32x4_t __b, int32x4_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(int64_t)((int64_t)__b[__i + 2] * (int64_t)__c[__lane]);
  return __r;
}
#define vmlsl_high_laneq_s32(...) __c17_vmlsl_high_laneq_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t vqdmull_high_s32(int32x4_t __a, int32x4_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i + 2] * __b[__i + 2], 64);
  return __r;
}

__C17_INTRIN int64x2_t vqdmull_high_n_s32(int32x4_t __a, int32_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i + 2] * __b, 64);
  return __r;
}

__C17_INTRIN int64x2_t __c17_vqdmull_high_lane_s32(int32x4_t __a, int32x2_t __b, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i + 2] * __b[__lane], 64);
  return __r;
}
#define vqdmull_high_lane_s32(...) __c17_vqdmull_high_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vqdmull_high_laneq_s32(int32x4_t __a, int32x4_t __b, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)2 * __a[__i + 2] * __b[__lane], 64);
  return __r;
}
#define vqdmull_high_laneq_s32(...) __c17_vqdmull_high_laneq_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t vqdmlal_high_s32(int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i + 2] * __c[__i + 2], 64), 64);
  return __r;
}

__C17_INTRIN int64x2_t vqdmlal_high_n_s32(int64x2_t __a, int32x4_t __b, int32_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i + 2] * __c, 64), 64);
  return __r;
}

__C17_INTRIN int64x2_t __c17_vqdmlal_high_lane_s32(int64x2_t __a, int32x4_t __b, int32x2_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i + 2] * __c[__lane], 64), 64);
  return __r;
}
#define vqdmlal_high_lane_s32(...) __c17_vqdmlal_high_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vqdmlal_high_laneq_s32(int64x2_t __a, int32x4_t __b, int32x4_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] + __c17_sat_s((__int128)2 * __b[__i + 2] * __c[__lane], 64), 64);
  return __r;
}
#define vqdmlal_high_laneq_s32(...) __c17_vqdmlal_high_laneq_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t vqdmlsl_high_s32(int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i + 2] * __c[__i + 2], 64), 64);
  return __r;
}

__C17_INTRIN int64x2_t vqdmlsl_high_n_s32(int64x2_t __a, int32x4_t __b, int32_t __c)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i + 2] * __c, 64), 64);
  return __r;
}

__C17_INTRIN int64x2_t __c17_vqdmlsl_high_lane_s32(int64x2_t __a, int32x4_t __b, int32x2_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i + 2] * __c[__lane], 64), 64);
  return __r;
}
#define vqdmlsl_high_lane_s32(...) __c17_vqdmlsl_high_lane_s32(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vqdmlsl_high_laneq_s32(int64x2_t __a, int32x4_t __b, int32x4_t __c, const int __lane)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s((__int128)__a[__i] - __c17_sat_s((__int128)2 * __b[__i + 2] * __c[__lane], 64), 64);
  return __r;
}
#define vqdmlsl_high_laneq_s32(...) __c17_vqdmlsl_high_laneq_s32(__VA_ARGS__)

__C17_INTRIN uint16x8_t vaddl_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint16_t)__a[__i] + (uint16_t)__b[__i];
  return __r;
}

__C17_INTRIN uint16x8_t vsubl_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint16_t)__a[__i] - (uint16_t)__b[__i];
  return __r;
}

__C17_INTRIN uint16x8_t vaddw_u8(uint16x8_t __a, uint8x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN uint16x8_t vsubw_u8(uint16x8_t __a, uint8x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN uint16x8_t vmull_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint16_t)((uint16_t)__a[__i] * (uint16_t)__b[__i]);
  return __r;
}

__C17_INTRIN uint16x8_t vmlal_u8(uint16x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint16_t)((uint16_t)__b[__i] * (uint16_t)__c[__i]);
  return __r;
}

__C17_INTRIN uint16x8_t vmlsl_u8(uint16x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint16_t)((uint16_t)__b[__i] * (uint16_t)__c[__i]);
  return __r;
}

__C17_INTRIN uint16x8_t vabdl_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i]);
  return __r;
}

__C17_INTRIN uint16x8_t vabal_u8(uint16x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN uint16x8_t vmovl_u8(uint8x8_t __a)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN uint16x8_t __c17_vshll_n_u8(uint8x8_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] << __n;
  return __r;
}
#define vshll_n_u8(...) __c17_vshll_n_u8(__VA_ARGS__)

__C17_INTRIN uint16x8_t vaddl_high_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint16_t)__a[__i + 8] + (uint16_t)__b[__i + 8];
  return __r;
}

__C17_INTRIN uint16x8_t vsubl_high_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint16_t)__a[__i + 8] - (uint16_t)__b[__i + 8];
  return __r;
}

__C17_INTRIN uint16x8_t vaddw_high_u8(uint16x8_t __a, uint8x16_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i + 8];
  return __r;
}

__C17_INTRIN uint16x8_t vsubw_high_u8(uint16x8_t __a, uint8x16_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i + 8];
  return __r;
}

__C17_INTRIN uint16x8_t vmull_high_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint16_t)((uint16_t)__a[__i + 8] * (uint16_t)__b[__i + 8]);
  return __r;
}

__C17_INTRIN uint16x8_t vmlal_high_u8(uint16x8_t __a, uint8x16_t __b, uint8x16_t __c)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint16_t)((uint16_t)__b[__i + 8] * (uint16_t)__c[__i + 8]);
  return __r;
}

__C17_INTRIN uint16x8_t vmlsl_high_u8(uint16x8_t __a, uint8x16_t __b, uint8x16_t __c)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint16_t)((uint16_t)__b[__i + 8] * (uint16_t)__c[__i + 8]);
  return __r;
}

__C17_INTRIN uint16x8_t vabdl_high_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__a[__i + 8] > __b[__i + 8] ? (int64_t)__a[__i + 8] - __b[__i + 8] : (int64_t)__b[__i + 8] - __a[__i + 8]);
  return __r;
}

__C17_INTRIN uint16x8_t vabal_high_u8(uint16x8_t __a, uint8x16_t __b, uint8x16_t __c)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i + 8] > __c[__i + 8] ? (int64_t)__b[__i + 8] - __c[__i + 8] : (int64_t)__c[__i + 8] - __b[__i + 8]);
  return __r;
}

__C17_INTRIN uint16x8_t vmovl_high_u8(uint8x16_t __a)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i + 8];
  return __r;
}

__C17_INTRIN uint16x8_t __c17_vshll_high_n_u8(uint8x16_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i + 8] << __n;
  return __r;
}
#define vshll_high_n_u8(...) __c17_vshll_high_n_u8(__VA_ARGS__)

__C17_INTRIN uint32x4_t vaddl_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)__a[__i] + (uint32_t)__b[__i];
  return __r;
}

__C17_INTRIN uint32x4_t vsubl_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)__a[__i] - (uint32_t)__b[__i];
  return __r;
}

__C17_INTRIN uint32x4_t vaddw_u16(uint32x4_t __a, uint16x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN uint32x4_t vsubw_u16(uint32x4_t __a, uint16x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN uint32x4_t vmull_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)((uint32_t)__a[__i] * (uint32_t)__b[__i]);
  return __r;
}

__C17_INTRIN uint32x4_t vmlal_u16(uint32x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint32_t)((uint32_t)__b[__i] * (uint32_t)__c[__i]);
  return __r;
}

__C17_INTRIN uint32x4_t vmlsl_u16(uint32x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint32_t)((uint32_t)__b[__i] * (uint32_t)__c[__i]);
  return __r;
}

__C17_INTRIN uint32x4_t vabdl_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i]);
  return __r;
}

__C17_INTRIN uint32x4_t vabal_u16(uint32x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN uint32x4_t vmovl_u16(uint16x4_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN uint32x4_t __c17_vshll_n_u16(uint16x4_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] << __n;
  return __r;
}
#define vshll_n_u16(...) __c17_vshll_n_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t vmull_n_u16(uint16x4_t __a, uint16_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)((uint32_t)__a[__i] * (uint32_t)__b);
  return __r;
}

__C17_INTRIN uint32x4_t vmlal_n_u16(uint32x4_t __a, uint16x4_t __b, uint16_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint32_t)((uint32_t)__b[__i] * (uint32_t)__c);
  return __r;
}

__C17_INTRIN uint32x4_t vmlsl_n_u16(uint32x4_t __a, uint16x4_t __b, uint16_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint32_t)((uint32_t)__b[__i] * (uint32_t)__c);
  return __r;
}

__C17_INTRIN uint32x4_t __c17_vmull_lane_u16(uint16x4_t __a, uint16x4_t __b, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)((uint32_t)__a[__i] * (uint32_t)__b[__lane]);
  return __r;
}
#define vmull_lane_u16(...) __c17_vmull_lane_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmlal_lane_u16(uint32x4_t __a, uint16x4_t __b, uint16x4_t __c, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint32_t)((uint32_t)__b[__i] * (uint32_t)__c[__lane]);
  return __r;
}
#define vmlal_lane_u16(...) __c17_vmlal_lane_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmlsl_lane_u16(uint32x4_t __a, uint16x4_t __b, uint16x4_t __c, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint32_t)((uint32_t)__b[__i] * (uint32_t)__c[__lane]);
  return __r;
}
#define vmlsl_lane_u16(...) __c17_vmlsl_lane_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmull_laneq_u16(uint16x4_t __a, uint16x8_t __b, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)((uint32_t)__a[__i] * (uint32_t)__b[__lane]);
  return __r;
}
#define vmull_laneq_u16(...) __c17_vmull_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmlal_laneq_u16(uint32x4_t __a, uint16x4_t __b, uint16x8_t __c, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint32_t)((uint32_t)__b[__i] * (uint32_t)__c[__lane]);
  return __r;
}
#define vmlal_laneq_u16(...) __c17_vmlal_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmlsl_laneq_u16(uint32x4_t __a, uint16x4_t __b, uint16x8_t __c, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint32_t)((uint32_t)__b[__i] * (uint32_t)__c[__lane]);
  return __r;
}
#define vmlsl_laneq_u16(...) __c17_vmlsl_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t vaddl_high_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)__a[__i + 4] + (uint32_t)__b[__i + 4];
  return __r;
}

__C17_INTRIN uint32x4_t vsubl_high_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)__a[__i + 4] - (uint32_t)__b[__i + 4];
  return __r;
}

__C17_INTRIN uint32x4_t vaddw_high_u16(uint32x4_t __a, uint16x8_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i + 4];
  return __r;
}

__C17_INTRIN uint32x4_t vsubw_high_u16(uint32x4_t __a, uint16x8_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i + 4];
  return __r;
}

__C17_INTRIN uint32x4_t vmull_high_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)((uint32_t)__a[__i + 4] * (uint32_t)__b[__i + 4]);
  return __r;
}

__C17_INTRIN uint32x4_t vmlal_high_u16(uint32x4_t __a, uint16x8_t __b, uint16x8_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint32_t)((uint32_t)__b[__i + 4] * (uint32_t)__c[__i + 4]);
  return __r;
}

__C17_INTRIN uint32x4_t vmlsl_high_u16(uint32x4_t __a, uint16x8_t __b, uint16x8_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint32_t)((uint32_t)__b[__i + 4] * (uint32_t)__c[__i + 4]);
  return __r;
}

__C17_INTRIN uint32x4_t vabdl_high_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__a[__i + 4] > __b[__i + 4] ? (int64_t)__a[__i + 4] - __b[__i + 4] : (int64_t)__b[__i + 4] - __a[__i + 4]);
  return __r;
}

__C17_INTRIN uint32x4_t vabal_high_u16(uint32x4_t __a, uint16x8_t __b, uint16x8_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i + 4] > __c[__i + 4] ? (int64_t)__b[__i + 4] - __c[__i + 4] : (int64_t)__c[__i + 4] - __b[__i + 4]);
  return __r;
}

__C17_INTRIN uint32x4_t vmovl_high_u16(uint16x8_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i + 4];
  return __r;
}

__C17_INTRIN uint32x4_t __c17_vshll_high_n_u16(uint16x8_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i + 4] << __n;
  return __r;
}
#define vshll_high_n_u16(...) __c17_vshll_high_n_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t vmull_high_n_u16(uint16x8_t __a, uint16_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)((uint32_t)__a[__i + 4] * (uint32_t)__b);
  return __r;
}

__C17_INTRIN uint32x4_t vmlal_high_n_u16(uint32x4_t __a, uint16x8_t __b, uint16_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint32_t)((uint32_t)__b[__i + 4] * (uint32_t)__c);
  return __r;
}

__C17_INTRIN uint32x4_t vmlsl_high_n_u16(uint32x4_t __a, uint16x8_t __b, uint16_t __c)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint32_t)((uint32_t)__b[__i + 4] * (uint32_t)__c);
  return __r;
}

__C17_INTRIN uint32x4_t __c17_vmull_high_lane_u16(uint16x8_t __a, uint16x4_t __b, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)((uint32_t)__a[__i + 4] * (uint32_t)__b[__lane]);
  return __r;
}
#define vmull_high_lane_u16(...) __c17_vmull_high_lane_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmlal_high_lane_u16(uint32x4_t __a, uint16x8_t __b, uint16x4_t __c, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint32_t)((uint32_t)__b[__i + 4] * (uint32_t)__c[__lane]);
  return __r;
}
#define vmlal_high_lane_u16(...) __c17_vmlal_high_lane_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmlsl_high_lane_u16(uint32x4_t __a, uint16x8_t __b, uint16x4_t __c, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint32_t)((uint32_t)__b[__i + 4] * (uint32_t)__c[__lane]);
  return __r;
}
#define vmlsl_high_lane_u16(...) __c17_vmlsl_high_lane_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmull_high_laneq_u16(uint16x8_t __a, uint16x8_t __b, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)((uint32_t)__a[__i + 4] * (uint32_t)__b[__lane]);
  return __r;
}
#define vmull_high_laneq_u16(...) __c17_vmull_high_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmlal_high_laneq_u16(uint32x4_t __a, uint16x8_t __b, uint16x8_t __c, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint32_t)((uint32_t)__b[__i + 4] * (uint32_t)__c[__lane]);
  return __r;
}
#define vmlal_high_laneq_u16(...) __c17_vmlal_high_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vmlsl_high_laneq_u16(uint32x4_t __a, uint16x8_t __b, uint16x8_t __c, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint32_t)((uint32_t)__b[__i + 4] * (uint32_t)__c[__lane]);
  return __r;
}
#define vmlsl_high_laneq_u16(...) __c17_vmlsl_high_laneq_u16(__VA_ARGS__)

__C17_INTRIN uint64x2_t vaddl_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN uint64x2_t vsubl_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN uint64x2_t vaddw_u32(uint64x2_t __a, uint32x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN uint64x2_t vsubw_u32(uint64x2_t __a, uint32x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i];
  return __r;
}

__C17_INTRIN uint64x2_t vmull_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)((uint64_t)__a[__i] * (uint64_t)__b[__i]);
  return __r;
}

__C17_INTRIN uint64x2_t vmlal_u32(uint64x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint64_t)((uint64_t)__b[__i] * (uint64_t)__c[__i]);
  return __r;
}

__C17_INTRIN uint64x2_t vmlsl_u32(uint64x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint64_t)((uint64_t)__b[__i] * (uint64_t)__c[__i]);
  return __r;
}

__C17_INTRIN uint64x2_t vabdl_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__a[__i] > __b[__i] ? (int64_t)__a[__i] - __b[__i] : (int64_t)__b[__i] - __a[__i]);
  return __r;
}

__C17_INTRIN uint64x2_t vabal_u32(uint64x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i] > __c[__i] ? (int64_t)__b[__i] - __c[__i] : (int64_t)__c[__i] - __b[__i]);
  return __r;
}

__C17_INTRIN uint64x2_t vmovl_u32(uint32x2_t __a)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN uint64x2_t __c17_vshll_n_u32(uint32x2_t __a, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] << __n;
  return __r;
}
#define vshll_n_u32(...) __c17_vshll_n_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t vmull_n_u32(uint32x2_t __a, uint32_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)((uint64_t)__a[__i] * (uint64_t)__b);
  return __r;
}

__C17_INTRIN uint64x2_t vmlal_n_u32(uint64x2_t __a, uint32x2_t __b, uint32_t __c)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint64_t)((uint64_t)__b[__i] * (uint64_t)__c);
  return __r;
}

__C17_INTRIN uint64x2_t vmlsl_n_u32(uint64x2_t __a, uint32x2_t __b, uint32_t __c)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint64_t)((uint64_t)__b[__i] * (uint64_t)__c);
  return __r;
}

__C17_INTRIN uint64x2_t __c17_vmull_lane_u32(uint32x2_t __a, uint32x2_t __b, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)((uint64_t)__a[__i] * (uint64_t)__b[__lane]);
  return __r;
}
#define vmull_lane_u32(...) __c17_vmull_lane_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vmlal_lane_u32(uint64x2_t __a, uint32x2_t __b, uint32x2_t __c, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint64_t)((uint64_t)__b[__i] * (uint64_t)__c[__lane]);
  return __r;
}
#define vmlal_lane_u32(...) __c17_vmlal_lane_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vmlsl_lane_u32(uint64x2_t __a, uint32x2_t __b, uint32x2_t __c, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint64_t)((uint64_t)__b[__i] * (uint64_t)__c[__lane]);
  return __r;
}
#define vmlsl_lane_u32(...) __c17_vmlsl_lane_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vmull_laneq_u32(uint32x2_t __a, uint32x4_t __b, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)((uint64_t)__a[__i] * (uint64_t)__b[__lane]);
  return __r;
}
#define vmull_laneq_u32(...) __c17_vmull_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vmlal_laneq_u32(uint64x2_t __a, uint32x2_t __b, uint32x4_t __c, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint64_t)((uint64_t)__b[__i] * (uint64_t)__c[__lane]);
  return __r;
}
#define vmlal_laneq_u32(...) __c17_vmlal_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vmlsl_laneq_u32(uint64x2_t __a, uint32x2_t __b, uint32x4_t __c, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint64_t)((uint64_t)__b[__i] * (uint64_t)__c[__lane]);
  return __r;
}
#define vmlsl_laneq_u32(...) __c17_vmlsl_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t vaddl_high_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i + 2] + (uint64_t)__b[__i + 2];
  return __r;
}

__C17_INTRIN uint64x2_t vsubl_high_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i + 2] - (uint64_t)__b[__i + 2];
  return __r;
}

__C17_INTRIN uint64x2_t vaddw_high_u32(uint64x2_t __a, uint32x4_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[__i + 2];
  return __r;
}

__C17_INTRIN uint64x2_t vsubw_high_u32(uint64x2_t __a, uint32x4_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)__b[__i + 2];
  return __r;
}

__C17_INTRIN uint64x2_t vmull_high_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)((uint64_t)__a[__i + 2] * (uint64_t)__b[__i + 2]);
  return __r;
}

__C17_INTRIN uint64x2_t vmlal_high_u32(uint64x2_t __a, uint32x4_t __b, uint32x4_t __c)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint64_t)((uint64_t)__b[__i + 2] * (uint64_t)__c[__i + 2]);
  return __r;
}

__C17_INTRIN uint64x2_t vmlsl_high_u32(uint64x2_t __a, uint32x4_t __b, uint32x4_t __c)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint64_t)((uint64_t)__b[__i + 2] * (uint64_t)__c[__i + 2]);
  return __r;
}

__C17_INTRIN uint64x2_t vabdl_high_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__a[__i + 2] > __b[__i + 2] ? (int64_t)__a[__i + 2] - __b[__i + 2] : (int64_t)__b[__i + 2] - __a[__i + 2]);
  return __r;
}

__C17_INTRIN uint64x2_t vabal_high_u32(uint64x2_t __a, uint32x4_t __b, uint32x4_t __c)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(__b[__i + 2] > __c[__i + 2] ? (int64_t)__b[__i + 2] - __c[__i + 2] : (int64_t)__c[__i + 2] - __b[__i + 2]);
  return __r;
}

__C17_INTRIN uint64x2_t vmovl_high_u32(uint32x4_t __a)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i + 2];
  return __r;
}

__C17_INTRIN uint64x2_t __c17_vshll_high_n_u32(uint32x4_t __a, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i + 2] << __n;
  return __r;
}
#define vshll_high_n_u32(...) __c17_vshll_high_n_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t vmull_high_n_u32(uint32x4_t __a, uint32_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)((uint64_t)__a[__i + 2] * (uint64_t)__b);
  return __r;
}

__C17_INTRIN uint64x2_t vmlal_high_n_u32(uint64x2_t __a, uint32x4_t __b, uint32_t __c)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint64_t)((uint64_t)__b[__i + 2] * (uint64_t)__c);
  return __r;
}

__C17_INTRIN uint64x2_t vmlsl_high_n_u32(uint64x2_t __a, uint32x4_t __b, uint32_t __c)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint64_t)((uint64_t)__b[__i + 2] * (uint64_t)__c);
  return __r;
}

__C17_INTRIN uint64x2_t __c17_vmull_high_lane_u32(uint32x4_t __a, uint32x2_t __b, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)((uint64_t)__a[__i + 2] * (uint64_t)__b[__lane]);
  return __r;
}
#define vmull_high_lane_u32(...) __c17_vmull_high_lane_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vmlal_high_lane_u32(uint64x2_t __a, uint32x4_t __b, uint32x2_t __c, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint64_t)((uint64_t)__b[__i + 2] * (uint64_t)__c[__lane]);
  return __r;
}
#define vmlal_high_lane_u32(...) __c17_vmlal_high_lane_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vmlsl_high_lane_u32(uint64x2_t __a, uint32x4_t __b, uint32x2_t __c, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint64_t)((uint64_t)__b[__i + 2] * (uint64_t)__c[__lane]);
  return __r;
}
#define vmlsl_high_lane_u32(...) __c17_vmlsl_high_lane_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vmull_high_laneq_u32(uint32x4_t __a, uint32x4_t __b, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)((uint64_t)__a[__i + 2] * (uint64_t)__b[__lane]);
  return __r;
}
#define vmull_high_laneq_u32(...) __c17_vmull_high_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vmlal_high_laneq_u32(uint64x2_t __a, uint32x4_t __b, uint32x4_t __c, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)(uint64_t)((uint64_t)__b[__i + 2] * (uint64_t)__c[__lane]);
  return __r;
}
#define vmlal_high_laneq_u32(...) __c17_vmlal_high_laneq_u32(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vmlsl_high_laneq_u32(uint64x2_t __a, uint32x4_t __b, uint32x4_t __c, const int __lane)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] - (uint64_t)(uint64_t)((uint64_t)__b[__i + 2] * (uint64_t)__c[__lane]);
  return __r;
}
#define vmlsl_high_laneq_u32(...) __c17_vmlsl_high_laneq_u32(__VA_ARGS__)

__C17_INTRIN poly16x8_t vmull_p8(poly8x8_t __a, poly8x8_t __b)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_pmul8(__a[__i], __b[__i]);
  return __r;
}

__C17_INTRIN poly16x8_t vmull_high_p8(poly8x16_t __a, poly8x16_t __b)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_pmul8(__a[__i + 8], __b[__i + 8]);
  return __r;
}

__C17_INTRIN int8x8_t vmovn_s16(int16x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN int8x16_t vmovn_high_s16(int8x8_t __r0, int16x8_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __a[__i];
  }
  return __r;
}

__C17_INTRIN int8x8_t vqmovn_s16(int16x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(__a[__i], 8);
  return __r;
}

__C17_INTRIN int8x16_t vqmovn_high_s16(int8x8_t __r0, int16x8_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_sat_s(__a[__i], 8);
  }
  return __r;
}

__C17_INTRIN int8x8_t vaddhn_s16(int16x8_t __a, int16x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)__a[__i] + (uint64_t)__b[__i]) >> 8;
  return __r;
}

__C17_INTRIN int8x16_t vaddhn_high_s16(int8x8_t __r0, int16x8_t __a, int16x8_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = ((uint64_t)__a[__i] + (uint64_t)__b[__i]) >> 8;
  }
  return __r;
}

__C17_INTRIN int8x8_t vsubhn_s16(int16x8_t __a, int16x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)__a[__i] - (uint64_t)__b[__i]) >> 8;
  return __r;
}

__C17_INTRIN int8x16_t vsubhn_high_s16(int8x8_t __r0, int16x8_t __a, int16x8_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = ((uint64_t)__a[__i] - (uint64_t)__b[__i]) >> 8;
  }
  return __r;
}

__C17_INTRIN int8x8_t vraddhn_s16(int16x8_t __a, int16x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)__a[__i] + (uint64_t)__b[__i] + ((uint64_t)1 << 7)) >> 8;
  return __r;
}

__C17_INTRIN int8x16_t vraddhn_high_s16(int8x8_t __r0, int16x8_t __a, int16x8_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = ((uint64_t)__a[__i] + (uint64_t)__b[__i] + ((uint64_t)1 << 7)) >> 8;
  }
  return __r;
}

__C17_INTRIN int8x8_t vrsubhn_s16(int16x8_t __a, int16x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)__a[__i] - (uint64_t)__b[__i] + ((uint64_t)1 << 7)) >> 8;
  return __r;
}

__C17_INTRIN int8x16_t vrsubhn_high_s16(int8x8_t __r0, int16x8_t __a, int16x8_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = ((uint64_t)__a[__i] - (uint64_t)__b[__i] + ((uint64_t)1 << 7)) >> 8;
  }
  return __r;
}

__C17_INTRIN uint8x8_t vqmovun_s16(int16x8_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_u(__a[__i], 8);
  return __r;
}

__C17_INTRIN uint8x16_t vqmovun_high_s16(uint8x8_t __r0, int16x8_t __a)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_sat_u(__a[__i], 8);
  }
  return __r;
}

__C17_INTRIN int8x8_t __c17_vshrn_n_s16(int16x8_t __a, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, -__n, 0, 0);
  return __r;
}
#define vshrn_n_s16(...) __c17_vshrn_n_s16(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vshrn_high_n_s16(int8x8_t __r0, int16x8_t __a, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_shl_s(__a[__i], 16, -__n, 0, 0);
  }
  return __r;
}
#define vshrn_high_n_s16(...) __c17_vshrn_high_n_s16(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vrshrn_n_s16(int16x8_t __a, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, -__n, 1, 0);
  return __r;
}
#define vrshrn_n_s16(...) __c17_vrshrn_n_s16(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vrshrn_high_n_s16(int8x8_t __r0, int16x8_t __a, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_shl_s(__a[__i], 16, -__n, 1, 0);
  }
  return __r;
}
#define vrshrn_high_n_s16(...) __c17_vrshrn_high_n_s16(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vqshrn_n_s16(int16x8_t __a, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(__c17_shl_s(__a[__i], 16, -__n, 0, 0), 8);
  return __r;
}
#define vqshrn_n_s16(...) __c17_vqshrn_n_s16(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vqshrn_high_n_s16(int8x8_t __r0, int16x8_t __a, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_sat_s(__c17_shl_s(__a[__i], 16, -__n, 0, 0), 8);
  }
  return __r;
}
#define vqshrn_high_n_s16(...) __c17_vqshrn_high_n_s16(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vqrshrn_n_s16(int16x8_t __a, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_s(__c17_shl_s(__a[__i], 16, -__n, 1, 0), 8);
  return __r;
}
#define vqrshrn_n_s16(...) __c17_vqrshrn_n_s16(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vqrshrn_high_n_s16(int8x8_t __r0, int16x8_t __a, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_sat_s(__c17_shl_s(__a[__i], 16, -__n, 1, 0), 8);
  }
  return __r;
}
#define vqrshrn_high_n_s16(...) __c17_vqrshrn_high_n_s16(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vqshrun_n_s16(int16x8_t __a, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_u(__c17_shl_s(__a[__i], 16, -__n, 0, 0), 8);
  return __r;
}
#define vqshrun_n_s16(...) __c17_vqshrun_n_s16(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vqshrun_high_n_s16(uint8x8_t __r0, int16x8_t __a, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_sat_u(__c17_shl_s(__a[__i], 16, -__n, 0, 0), 8);
  }
  return __r;
}
#define vqshrun_high_n_s16(...) __c17_vqshrun_high_n_s16(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vqrshrun_n_s16(int16x8_t __a, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_u(__c17_shl_s(__a[__i], 16, -__n, 1, 0), 8);
  return __r;
}
#define vqrshrun_n_s16(...) __c17_vqrshrun_n_s16(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vqrshrun_high_n_s16(uint8x8_t __r0, int16x8_t __a, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_sat_u(__c17_shl_s(__a[__i], 16, -__n, 1, 0), 8);
  }
  return __r;
}
#define vqrshrun_high_n_s16(...) __c17_vqrshrun_high_n_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t vmovn_s32(int32x4_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN int16x8_t vmovn_high_s32(int16x4_t __r0, int32x4_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __a[__i];
  }
  return __r;
}

__C17_INTRIN int16x4_t vqmovn_s32(int32x4_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(__a[__i], 16);
  return __r;
}

__C17_INTRIN int16x8_t vqmovn_high_s32(int16x4_t __r0, int32x4_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_sat_s(__a[__i], 16);
  }
  return __r;
}

__C17_INTRIN int16x4_t vaddhn_s32(int32x4_t __a, int32x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)__a[__i] + (uint64_t)__b[__i]) >> 16;
  return __r;
}

__C17_INTRIN int16x8_t vaddhn_high_s32(int16x4_t __r0, int32x4_t __a, int32x4_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = ((uint64_t)__a[__i] + (uint64_t)__b[__i]) >> 16;
  }
  return __r;
}

__C17_INTRIN int16x4_t vsubhn_s32(int32x4_t __a, int32x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)__a[__i] - (uint64_t)__b[__i]) >> 16;
  return __r;
}

__C17_INTRIN int16x8_t vsubhn_high_s32(int16x4_t __r0, int32x4_t __a, int32x4_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = ((uint64_t)__a[__i] - (uint64_t)__b[__i]) >> 16;
  }
  return __r;
}

__C17_INTRIN int16x4_t vraddhn_s32(int32x4_t __a, int32x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)__a[__i] + (uint64_t)__b[__i] + ((uint64_t)1 << 15)) >> 16;
  return __r;
}

__C17_INTRIN int16x8_t vraddhn_high_s32(int16x4_t __r0, int32x4_t __a, int32x4_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = ((uint64_t)__a[__i] + (uint64_t)__b[__i] + ((uint64_t)1 << 15)) >> 16;
  }
  return __r;
}

__C17_INTRIN int16x4_t vrsubhn_s32(int32x4_t __a, int32x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)__a[__i] - (uint64_t)__b[__i] + ((uint64_t)1 << 15)) >> 16;
  return __r;
}

__C17_INTRIN int16x8_t vrsubhn_high_s32(int16x4_t __r0, int32x4_t __a, int32x4_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = ((uint64_t)__a[__i] - (uint64_t)__b[__i] + ((uint64_t)1 << 15)) >> 16;
  }
  return __r;
}

__C17_INTRIN uint16x4_t vqmovun_s32(int32x4_t __a)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_u(__a[__i], 16);
  return __r;
}

__C17_INTRIN uint16x8_t vqmovun_high_s32(uint16x4_t __r0, int32x4_t __a)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_sat_u(__a[__i], 16);
  }
  return __r;
}

__C17_INTRIN int16x4_t __c17_vshrn_n_s32(int32x4_t __a, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, -__n, 0, 0);
  return __r;
}
#define vshrn_n_s32(...) __c17_vshrn_n_s32(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vshrn_high_n_s32(int16x4_t __r0, int32x4_t __a, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_shl_s(__a[__i], 32, -__n, 0, 0);
  }
  return __r;
}
#define vshrn_high_n_s32(...) __c17_vshrn_high_n_s32(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vrshrn_n_s32(int32x4_t __a, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, -__n, 1, 0);
  return __r;
}
#define vrshrn_n_s32(...) __c17_vrshrn_n_s32(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vrshrn_high_n_s32(int16x4_t __r0, int32x4_t __a, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_shl_s(__a[__i], 32, -__n, 1, 0);
  }
  return __r;
}
#define vrshrn_high_n_s32(...) __c17_vrshrn_high_n_s32(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vqshrn_n_s32(int32x4_t __a, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(__c17_shl_s(__a[__i], 32, -__n, 0, 0), 16);
  return __r;
}
#define vqshrn_n_s32(...) __c17_vqshrn_n_s32(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vqshrn_high_n_s32(int16x4_t __r0, int32x4_t __a, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_sat_s(__c17_shl_s(__a[__i], 32, -__n, 0, 0), 16);
  }
  return __r;
}
#define vqshrn_high_n_s32(...) __c17_vqshrn_high_n_s32(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vqrshrn_n_s32(int32x4_t __a, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_s(__c17_shl_s(__a[__i], 32, -__n, 1, 0), 16);
  return __r;
}
#define vqrshrn_n_s32(...) __c17_vqrshrn_n_s32(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vqrshrn_high_n_s32(int16x4_t __r0, int32x4_t __a, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_sat_s(__c17_shl_s(__a[__i], 32, -__n, 1, 0), 16);
  }
  return __r;
}
#define vqrshrn_high_n_s32(...) __c17_vqrshrn_high_n_s32(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vqshrun_n_s32(int32x4_t __a, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_u(__c17_shl_s(__a[__i], 32, -__n, 0, 0), 16);
  return __r;
}
#define vqshrun_n_s32(...) __c17_vqshrun_n_s32(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vqshrun_high_n_s32(uint16x4_t __r0, int32x4_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_sat_u(__c17_shl_s(__a[__i], 32, -__n, 0, 0), 16);
  }
  return __r;
}
#define vqshrun_high_n_s32(...) __c17_vqshrun_high_n_s32(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vqrshrun_n_s32(int32x4_t __a, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_u(__c17_shl_s(__a[__i], 32, -__n, 1, 0), 16);
  return __r;
}
#define vqrshrun_n_s32(...) __c17_vqrshrun_n_s32(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vqrshrun_high_n_s32(uint16x4_t __r0, int32x4_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_sat_u(__c17_shl_s(__a[__i], 32, -__n, 1, 0), 16);
  }
  return __r;
}
#define vqrshrun_high_n_s32(...) __c17_vqrshrun_high_n_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t vmovn_s64(int64x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN int32x4_t vmovn_high_s64(int32x2_t __r0, int64x2_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __a[__i];
  }
  return __r;
}

__C17_INTRIN int32x2_t vqmovn_s64(int64x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(__a[__i], 32);
  return __r;
}

__C17_INTRIN int32x4_t vqmovn_high_s64(int32x2_t __r0, int64x2_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_sat_s(__a[__i], 32);
  }
  return __r;
}

__C17_INTRIN int32x2_t vaddhn_s64(int64x2_t __a, int64x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((uint64_t)__a[__i] + (uint64_t)__b[__i]) >> 32;
  return __r;
}

__C17_INTRIN int32x4_t vaddhn_high_s64(int32x2_t __r0, int64x2_t __a, int64x2_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = ((uint64_t)__a[__i] + (uint64_t)__b[__i]) >> 32;
  }
  return __r;
}

__C17_INTRIN int32x2_t vsubhn_s64(int64x2_t __a, int64x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((uint64_t)__a[__i] - (uint64_t)__b[__i]) >> 32;
  return __r;
}

__C17_INTRIN int32x4_t vsubhn_high_s64(int32x2_t __r0, int64x2_t __a, int64x2_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = ((uint64_t)__a[__i] - (uint64_t)__b[__i]) >> 32;
  }
  return __r;
}

__C17_INTRIN int32x2_t vraddhn_s64(int64x2_t __a, int64x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((uint64_t)__a[__i] + (uint64_t)__b[__i] + ((uint64_t)1 << 31)) >> 32;
  return __r;
}

__C17_INTRIN int32x4_t vraddhn_high_s64(int32x2_t __r0, int64x2_t __a, int64x2_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = ((uint64_t)__a[__i] + (uint64_t)__b[__i] + ((uint64_t)1 << 31)) >> 32;
  }
  return __r;
}

__C17_INTRIN int32x2_t vrsubhn_s64(int64x2_t __a, int64x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((uint64_t)__a[__i] - (uint64_t)__b[__i] + ((uint64_t)1 << 31)) >> 32;
  return __r;
}

__C17_INTRIN int32x4_t vrsubhn_high_s64(int32x2_t __r0, int64x2_t __a, int64x2_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = ((uint64_t)__a[__i] - (uint64_t)__b[__i] + ((uint64_t)1 << 31)) >> 32;
  }
  return __r;
}

__C17_INTRIN uint32x2_t vqmovun_s64(int64x2_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_u(__a[__i], 32);
  return __r;
}

__C17_INTRIN uint32x4_t vqmovun_high_s64(uint32x2_t __r0, int64x2_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_sat_u(__a[__i], 32);
  }
  return __r;
}

__C17_INTRIN int32x2_t __c17_vshrn_n_s64(int64x2_t __a, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, -__n, 0, 0);
  return __r;
}
#define vshrn_n_s64(...) __c17_vshrn_n_s64(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vshrn_high_n_s64(int32x2_t __r0, int64x2_t __a, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_shl_s(__a[__i], 64, -__n, 0, 0);
  }
  return __r;
}
#define vshrn_high_n_s64(...) __c17_vshrn_high_n_s64(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vrshrn_n_s64(int64x2_t __a, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, -__n, 1, 0);
  return __r;
}
#define vrshrn_n_s64(...) __c17_vrshrn_n_s64(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vrshrn_high_n_s64(int32x2_t __r0, int64x2_t __a, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_shl_s(__a[__i], 64, -__n, 1, 0);
  }
  return __r;
}
#define vrshrn_high_n_s64(...) __c17_vrshrn_high_n_s64(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vqshrn_n_s64(int64x2_t __a, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(__c17_shl_s(__a[__i], 64, -__n, 0, 0), 32);
  return __r;
}
#define vqshrn_n_s64(...) __c17_vqshrn_n_s64(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqshrn_high_n_s64(int32x2_t __r0, int64x2_t __a, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_sat_s(__c17_shl_s(__a[__i], 64, -__n, 0, 0), 32);
  }
  return __r;
}
#define vqshrn_high_n_s64(...) __c17_vqshrn_high_n_s64(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vqrshrn_n_s64(int64x2_t __a, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_s(__c17_shl_s(__a[__i], 64, -__n, 1, 0), 32);
  return __r;
}
#define vqrshrn_n_s64(...) __c17_vqrshrn_n_s64(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqrshrn_high_n_s64(int32x2_t __r0, int64x2_t __a, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_sat_s(__c17_shl_s(__a[__i], 64, -__n, 1, 0), 32);
  }
  return __r;
}
#define vqrshrn_high_n_s64(...) __c17_vqrshrn_high_n_s64(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vqshrun_n_s64(int64x2_t __a, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_u(__c17_shl_s(__a[__i], 64, -__n, 0, 0), 32);
  return __r;
}
#define vqshrun_n_s64(...) __c17_vqshrun_n_s64(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vqshrun_high_n_s64(uint32x2_t __r0, int64x2_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_sat_u(__c17_shl_s(__a[__i], 64, -__n, 0, 0), 32);
  }
  return __r;
}
#define vqshrun_high_n_s64(...) __c17_vqshrun_high_n_s64(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vqrshrun_n_s64(int64x2_t __a, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_u(__c17_shl_s(__a[__i], 64, -__n, 1, 0), 32);
  return __r;
}
#define vqrshrun_n_s64(...) __c17_vqrshrun_n_s64(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vqrshrun_high_n_s64(uint32x2_t __r0, int64x2_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_sat_u(__c17_shl_s(__a[__i], 64, -__n, 1, 0), 32);
  }
  return __r;
}
#define vqrshrun_high_n_s64(...) __c17_vqrshrun_high_n_s64(__VA_ARGS__)

__C17_INTRIN uint8x8_t vmovn_u16(uint16x8_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN uint8x16_t vmovn_high_u16(uint8x8_t __r0, uint16x8_t __a)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __a[__i];
  }
  return __r;
}

__C17_INTRIN uint8x8_t vqmovn_u16(uint16x8_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_u(__a[__i], 8);
  return __r;
}

__C17_INTRIN uint8x16_t vqmovn_high_u16(uint8x8_t __r0, uint16x8_t __a)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_sat_u(__a[__i], 8);
  }
  return __r;
}

__C17_INTRIN uint8x8_t vaddhn_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)__a[__i] + (uint64_t)__b[__i]) >> 8;
  return __r;
}

__C17_INTRIN uint8x16_t vaddhn_high_u16(uint8x8_t __r0, uint16x8_t __a, uint16x8_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = ((uint64_t)__a[__i] + (uint64_t)__b[__i]) >> 8;
  }
  return __r;
}

__C17_INTRIN uint8x8_t vsubhn_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)__a[__i] - (uint64_t)__b[__i]) >> 8;
  return __r;
}

__C17_INTRIN uint8x16_t vsubhn_high_u16(uint8x8_t __r0, uint16x8_t __a, uint16x8_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = ((uint64_t)__a[__i] - (uint64_t)__b[__i]) >> 8;
  }
  return __r;
}

__C17_INTRIN uint8x8_t vraddhn_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)__a[__i] + (uint64_t)__b[__i] + ((uint64_t)1 << 7)) >> 8;
  return __r;
}

__C17_INTRIN uint8x16_t vraddhn_high_u16(uint8x8_t __r0, uint16x8_t __a, uint16x8_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = ((uint64_t)__a[__i] + (uint64_t)__b[__i] + ((uint64_t)1 << 7)) >> 8;
  }
  return __r;
}

__C17_INTRIN uint8x8_t vrsubhn_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)__a[__i] - (uint64_t)__b[__i] + ((uint64_t)1 << 7)) >> 8;
  return __r;
}

__C17_INTRIN uint8x16_t vrsubhn_high_u16(uint8x8_t __r0, uint16x8_t __a, uint16x8_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = ((uint64_t)__a[__i] - (uint64_t)__b[__i] + ((uint64_t)1 << 7)) >> 8;
  }
  return __r;
}

__C17_INTRIN uint8x8_t __c17_vshrn_n_u16(uint16x8_t __a, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, -__n, 0, 0);
  return __r;
}
#define vshrn_n_u16(...) __c17_vshrn_n_u16(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vshrn_high_n_u16(uint8x8_t __r0, uint16x8_t __a, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_shl_u(__a[__i], 16, -__n, 0, 0);
  }
  return __r;
}
#define vshrn_high_n_u16(...) __c17_vshrn_high_n_u16(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vrshrn_n_u16(uint16x8_t __a, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, -__n, 1, 0);
  return __r;
}
#define vrshrn_n_u16(...) __c17_vrshrn_n_u16(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vrshrn_high_n_u16(uint8x8_t __r0, uint16x8_t __a, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_shl_u(__a[__i], 16, -__n, 1, 0);
  }
  return __r;
}
#define vrshrn_high_n_u16(...) __c17_vrshrn_high_n_u16(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vqshrn_n_u16(uint16x8_t __a, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_u(__c17_shl_u(__a[__i], 16, -__n, 0, 0), 8);
  return __r;
}
#define vqshrn_n_u16(...) __c17_vqshrn_n_u16(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vqshrn_high_n_u16(uint8x8_t __r0, uint16x8_t __a, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_sat_u(__c17_shl_u(__a[__i], 16, -__n, 0, 0), 8);
  }
  return __r;
}
#define vqshrn_high_n_u16(...) __c17_vqshrn_high_n_u16(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vqrshrn_n_u16(uint16x8_t __a, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_sat_u(__c17_shl_u(__a[__i], 16, -__n, 1, 0), 8);
  return __r;
}
#define vqrshrn_n_u16(...) __c17_vqrshrn_n_u16(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vqrshrn_high_n_u16(uint8x8_t __r0, uint16x8_t __a, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 8] = __c17_sat_u(__c17_shl_u(__a[__i], 16, -__n, 1, 0), 8);
  }
  return __r;
}
#define vqrshrn_high_n_u16(...) __c17_vqrshrn_high_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t vmovn_u32(uint32x4_t __a)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN uint16x8_t vmovn_high_u32(uint16x4_t __r0, uint32x4_t __a)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __a[__i];
  }
  return __r;
}

__C17_INTRIN uint16x4_t vqmovn_u32(uint32x4_t __a)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_u(__a[__i], 16);
  return __r;
}

__C17_INTRIN uint16x8_t vqmovn_high_u32(uint16x4_t __r0, uint32x4_t __a)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_sat_u(__a[__i], 16);
  }
  return __r;
}

__C17_INTRIN uint16x4_t vaddhn_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)__a[__i] + (uint64_t)__b[__i]) >> 16;
  return __r;
}

__C17_INTRIN uint16x8_t vaddhn_high_u32(uint16x4_t __r0, uint32x4_t __a, uint32x4_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = ((uint64_t)__a[__i] + (uint64_t)__b[__i]) >> 16;
  }
  return __r;
}

__C17_INTRIN uint16x4_t vsubhn_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)__a[__i] - (uint64_t)__b[__i]) >> 16;
  return __r;
}

__C17_INTRIN uint16x8_t vsubhn_high_u32(uint16x4_t __r0, uint32x4_t __a, uint32x4_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = ((uint64_t)__a[__i] - (uint64_t)__b[__i]) >> 16;
  }
  return __r;
}

__C17_INTRIN uint16x4_t vraddhn_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)__a[__i] + (uint64_t)__b[__i] + ((uint64_t)1 << 15)) >> 16;
  return __r;
}

__C17_INTRIN uint16x8_t vraddhn_high_u32(uint16x4_t __r0, uint32x4_t __a, uint32x4_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = ((uint64_t)__a[__i] + (uint64_t)__b[__i] + ((uint64_t)1 << 15)) >> 16;
  }
  return __r;
}

__C17_INTRIN uint16x4_t vrsubhn_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)__a[__i] - (uint64_t)__b[__i] + ((uint64_t)1 << 15)) >> 16;
  return __r;
}

__C17_INTRIN uint16x8_t vrsubhn_high_u32(uint16x4_t __r0, uint32x4_t __a, uint32x4_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = ((uint64_t)__a[__i] - (uint64_t)__b[__i] + ((uint64_t)1 << 15)) >> 16;
  }
  return __r;
}

__C17_INTRIN uint16x4_t __c17_vshrn_n_u32(uint32x4_t __a, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, -__n, 0, 0);
  return __r;
}
#define vshrn_n_u32(...) __c17_vshrn_n_u32(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vshrn_high_n_u32(uint16x4_t __r0, uint32x4_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_shl_u(__a[__i], 32, -__n, 0, 0);
  }
  return __r;
}
#define vshrn_high_n_u32(...) __c17_vshrn_high_n_u32(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vrshrn_n_u32(uint32x4_t __a, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, -__n, 1, 0);
  return __r;
}
#define vrshrn_n_u32(...) __c17_vrshrn_n_u32(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vrshrn_high_n_u32(uint16x4_t __r0, uint32x4_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_shl_u(__a[__i], 32, -__n, 1, 0);
  }
  return __r;
}
#define vrshrn_high_n_u32(...) __c17_vrshrn_high_n_u32(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vqshrn_n_u32(uint32x4_t __a, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_u(__c17_shl_u(__a[__i], 32, -__n, 0, 0), 16);
  return __r;
}
#define vqshrn_n_u32(...) __c17_vqshrn_n_u32(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vqshrn_high_n_u32(uint16x4_t __r0, uint32x4_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_sat_u(__c17_shl_u(__a[__i], 32, -__n, 0, 0), 16);
  }
  return __r;
}
#define vqshrn_high_n_u32(...) __c17_vqshrn_high_n_u32(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vqrshrn_n_u32(uint32x4_t __a, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_sat_u(__c17_shl_u(__a[__i], 32, -__n, 1, 0), 16);
  return __r;
}
#define vqrshrn_n_u32(...) __c17_vqrshrn_n_u32(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vqrshrn_high_n_u32(uint16x4_t __r0, uint32x4_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 4] = __c17_sat_u(__c17_shl_u(__a[__i], 32, -__n, 1, 0), 16);
  }
  return __r;
}
#define vqrshrn_high_n_u32(...) __c17_vqrshrn_high_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t vmovn_u64(uint64x2_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i];
  return __r;
}

__C17_INTRIN uint32x4_t vmovn_high_u64(uint32x2_t __r0, uint64x2_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __a[__i];
  }
  return __r;
}

__C17_INTRIN uint32x2_t vqmovn_u64(uint64x2_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_u(__a[__i], 32);
  return __r;
}

__C17_INTRIN uint32x4_t vqmovn_high_u64(uint32x2_t __r0, uint64x2_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_sat_u(__a[__i], 32);
  }
  return __r;
}

__C17_INTRIN uint32x2_t vaddhn_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((uint64_t)__a[__i] + (uint64_t)__b[__i]) >> 32;
  return __r;
}

__C17_INTRIN uint32x4_t vaddhn_high_u64(uint32x2_t __r0, uint64x2_t __a, uint64x2_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = ((uint64_t)__a[__i] + (uint64_t)__b[__i]) >> 32;
  }
  return __r;
}

__C17_INTRIN uint32x2_t vsubhn_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((uint64_t)__a[__i] - (uint64_t)__b[__i]) >> 32;
  return __r;
}

__C17_INTRIN uint32x4_t vsubhn_high_u64(uint32x2_t __r0, uint64x2_t __a, uint64x2_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = ((uint64_t)__a[__i] - (uint64_t)__b[__i]) >> 32;
  }
  return __r;
}

__C17_INTRIN uint32x2_t vraddhn_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((uint64_t)__a[__i] + (uint64_t)__b[__i] + ((uint64_t)1 << 31)) >> 32;
  return __r;
}

__C17_INTRIN uint32x4_t vraddhn_high_u64(uint32x2_t __r0, uint64x2_t __a, uint64x2_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = ((uint64_t)__a[__i] + (uint64_t)__b[__i] + ((uint64_t)1 << 31)) >> 32;
  }
  return __r;
}

__C17_INTRIN uint32x2_t vrsubhn_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((uint64_t)__a[__i] - (uint64_t)__b[__i] + ((uint64_t)1 << 31)) >> 32;
  return __r;
}

__C17_INTRIN uint32x4_t vrsubhn_high_u64(uint32x2_t __r0, uint64x2_t __a, uint64x2_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = ((uint64_t)__a[__i] - (uint64_t)__b[__i] + ((uint64_t)1 << 31)) >> 32;
  }
  return __r;
}

__C17_INTRIN uint32x2_t __c17_vshrn_n_u64(uint64x2_t __a, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, -__n, 0, 0);
  return __r;
}
#define vshrn_n_u64(...) __c17_vshrn_n_u64(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vshrn_high_n_u64(uint32x2_t __r0, uint64x2_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_shl_u(__a[__i], 64, -__n, 0, 0);
  }
  return __r;
}
#define vshrn_high_n_u64(...) __c17_vshrn_high_n_u64(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vrshrn_n_u64(uint64x2_t __a, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, -__n, 1, 0);
  return __r;
}
#define vrshrn_n_u64(...) __c17_vrshrn_n_u64(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vrshrn_high_n_u64(uint32x2_t __r0, uint64x2_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_shl_u(__a[__i], 64, -__n, 1, 0);
  }
  return __r;
}
#define vrshrn_high_n_u64(...) __c17_vrshrn_high_n_u64(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vqshrn_n_u64(uint64x2_t __a, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_u(__c17_shl_u(__a[__i], 64, -__n, 0, 0), 32);
  return __r;
}
#define vqshrn_n_u64(...) __c17_vqshrn_n_u64(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vqshrn_high_n_u64(uint32x2_t __r0, uint64x2_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_sat_u(__c17_shl_u(__a[__i], 64, -__n, 0, 0), 32);
  }
  return __r;
}
#define vqshrn_high_n_u64(...) __c17_vqshrn_high_n_u64(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vqrshrn_n_u64(uint64x2_t __a, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_sat_u(__c17_shl_u(__a[__i], 64, -__n, 1, 0), 32);
  return __r;
}
#define vqrshrn_n_u64(...) __c17_vqrshrn_n_u64(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vqrshrn_high_n_u64(uint32x2_t __r0, uint64x2_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r[__i] = __r0[__i];
    __r[__i + 2] = __c17_sat_u(__c17_shl_u(__a[__i], 64, -__n, 1, 0), 32);
  }
  return __r;
}
#define vqrshrn_high_n_u64(...) __c17_vqrshrn_high_n_u64(__VA_ARGS__)


/* Pairwise and across-vector operations. */

__C17_INTRIN int8x8_t vpadd_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 8] + (uint64_t)__b[2 * __i - 8 + 1];
  return __r;
}

__C17_INTRIN int8_t vaddv_s8(int8x8_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 8; __i++)
    __s += (uint64_t)__a[__i];
  return (int8_t)__s;
}

__C17_INTRIN int8x8_t vpmax_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? (__a[2 * __i] > __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 8] > __b[2 * __i - 8 + 1] ? __b[2 * __i - 8] : __b[2 * __i - 8 + 1]);
  return __r;
}

__C17_INTRIN int8x8_t vpmin_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? (__a[2 * __i] < __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 8] < __b[2 * __i - 8 + 1] ? __b[2 * __i - 8] : __b[2 * __i - 8 + 1]);
  return __r;
}

__C17_INTRIN int8_t vmaxv_s8(int8x8_t __a)
{
  int8_t __m = __a[0];
  for (int __i = 1; __i < 8; __i++)
    if (__a[__i] > __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN int8_t vminv_s8(int8x8_t __a)
{
  int8_t __m = __a[0];
  for (int __i = 1; __i < 8; __i++)
    if (__a[__i] < __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN int16_t vaddlv_s8(int8x8_t __a)
{
  int16_t __s = 0;
  for (int __i = 0; __i < 8; __i++)
    __s += __a[__i];
  return __s;
}

__C17_INTRIN int16x4_t vpaddl_s8(int8x8_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int16_t)__a[2 * __i] + __a[2 * __i + 1];
  return __r;
}

__C17_INTRIN int16x4_t vpadal_s8(int16x4_t __a, int8x8_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[2 * __i] + (uint64_t)__b[2 * __i + 1];
  return __r;
}

__C17_INTRIN int8x16_t vpaddq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 16] + (uint64_t)__b[2 * __i - 16 + 1];
  return __r;
}

__C17_INTRIN int8_t vaddvq_s8(int8x16_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 16; __i++)
    __s += (uint64_t)__a[__i];
  return (int8_t)__s;
}

__C17_INTRIN int8x16_t vpmaxq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? (__a[2 * __i] > __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 16] > __b[2 * __i - 16 + 1] ? __b[2 * __i - 16] : __b[2 * __i - 16 + 1]);
  return __r;
}

__C17_INTRIN int8x16_t vpminq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? (__a[2 * __i] < __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 16] < __b[2 * __i - 16 + 1] ? __b[2 * __i - 16] : __b[2 * __i - 16 + 1]);
  return __r;
}

__C17_INTRIN int8_t vmaxvq_s8(int8x16_t __a)
{
  int8_t __m = __a[0];
  for (int __i = 1; __i < 16; __i++)
    if (__a[__i] > __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN int8_t vminvq_s8(int8x16_t __a)
{
  int8_t __m = __a[0];
  for (int __i = 1; __i < 16; __i++)
    if (__a[__i] < __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN int16_t vaddlvq_s8(int8x16_t __a)
{
  int16_t __s = 0;
  for (int __i = 0; __i < 16; __i++)
    __s += __a[__i];
  return __s;
}

__C17_INTRIN int16x8_t vpaddlq_s8(int8x16_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (int16_t)__a[2 * __i] + __a[2 * __i + 1];
  return __r;
}

__C17_INTRIN int16x8_t vpadalq_s8(int16x8_t __a, int8x16_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[2 * __i] + (uint64_t)__b[2 * __i + 1];
  return __r;
}

__C17_INTRIN int16x4_t vpadd_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 4] + (uint64_t)__b[2 * __i - 4 + 1];
  return __r;
}

__C17_INTRIN int16_t vaddv_s16(int16x4_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 4; __i++)
    __s += (uint64_t)__a[__i];
  return (int16_t)__s;
}

__C17_INTRIN int16x4_t vpmax_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? (__a[2 * __i] > __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 4] > __b[2 * __i - 4 + 1] ? __b[2 * __i - 4] : __b[2 * __i - 4 + 1]);
  return __r;
}

__C17_INTRIN int16x4_t vpmin_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? (__a[2 * __i] < __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 4] < __b[2 * __i - 4 + 1] ? __b[2 * __i - 4] : __b[2 * __i - 4 + 1]);
  return __r;
}

__C17_INTRIN int16_t vmaxv_s16(int16x4_t __a)
{
  int16_t __m = __a[0];
  for (int __i = 1; __i < 4; __i++)
    if (__a[__i] > __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN int16_t vminv_s16(int16x4_t __a)
{
  int16_t __m = __a[0];
  for (int __i = 1; __i < 4; __i++)
    if (__a[__i] < __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN int32_t vaddlv_s16(int16x4_t __a)
{
  int32_t __s = 0;
  for (int __i = 0; __i < 4; __i++)
    __s += __a[__i];
  return __s;
}

__C17_INTRIN int32x2_t vpaddl_s16(int16x4_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int32_t)__a[2 * __i] + __a[2 * __i + 1];
  return __r;
}

__C17_INTRIN int32x2_t vpadal_s16(int32x2_t __a, int16x4_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[2 * __i] + (uint64_t)__b[2 * __i + 1];
  return __r;
}

__C17_INTRIN int16x8_t vpaddq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 8] + (uint64_t)__b[2 * __i - 8 + 1];
  return __r;
}

__C17_INTRIN int16_t vaddvq_s16(int16x8_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 8; __i++)
    __s += (uint64_t)__a[__i];
  return (int16_t)__s;
}

__C17_INTRIN int16x8_t vpmaxq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? (__a[2 * __i] > __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 8] > __b[2 * __i - 8 + 1] ? __b[2 * __i - 8] : __b[2 * __i - 8 + 1]);
  return __r;
}

__C17_INTRIN int16x8_t vpminq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? (__a[2 * __i] < __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 8] < __b[2 * __i - 8 + 1] ? __b[2 * __i - 8] : __b[2 * __i - 8 + 1]);
  return __r;
}

__C17_INTRIN int16_t vmaxvq_s16(int16x8_t __a)
{
  int16_t __m = __a[0];
  for (int __i = 1; __i < 8; __i++)
    if (__a[__i] > __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN int16_t vminvq_s16(int16x8_t __a)
{
  int16_t __m = __a[0];
  for (int __i = 1; __i < 8; __i++)
    if (__a[__i] < __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN int32_t vaddlvq_s16(int16x8_t __a)
{
  int32_t __s = 0;
  for (int __i = 0; __i < 8; __i++)
    __s += __a[__i];
  return __s;
}

__C17_INTRIN int32x4_t vpaddlq_s16(int16x8_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (int32_t)__a[2 * __i] + __a[2 * __i + 1];
  return __r;
}

__C17_INTRIN int32x4_t vpadalq_s16(int32x4_t __a, int16x8_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[2 * __i] + (uint64_t)__b[2 * __i + 1];
  return __r;
}

__C17_INTRIN int32x2_t vpadd_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 2] + (uint64_t)__b[2 * __i - 2 + 1];
  return __r;
}

__C17_INTRIN int32_t vaddv_s32(int32x2_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 2; __i++)
    __s += (uint64_t)__a[__i];
  return (int32_t)__s;
}

__C17_INTRIN int32x2_t vpmax_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? (__a[2 * __i] > __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 2] > __b[2 * __i - 2 + 1] ? __b[2 * __i - 2] : __b[2 * __i - 2 + 1]);
  return __r;
}

__C17_INTRIN int32x2_t vpmin_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? (__a[2 * __i] < __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 2] < __b[2 * __i - 2 + 1] ? __b[2 * __i - 2] : __b[2 * __i - 2 + 1]);
  return __r;
}

__C17_INTRIN int32_t vmaxv_s32(int32x2_t __a)
{
  int32_t __m = __a[0];
  for (int __i = 1; __i < 2; __i++)
    if (__a[__i] > __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN int32_t vminv_s32(int32x2_t __a)
{
  int32_t __m = __a[0];
  for (int __i = 1; __i < 2; __i++)
    if (__a[__i] < __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN int64_t vaddlv_s32(int32x2_t __a)
{
  int64_t __s = 0;
  for (int __i = 0; __i < 2; __i++)
    __s += __a[__i];
  return __s;
}

__C17_INTRIN int64x1_t vpaddl_s32(int32x2_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = (int64_t)__a[2 * __i] + __a[2 * __i + 1];
  return __r;
}

__C17_INTRIN int64x1_t vpadal_s32(int64x1_t __a, int32x2_t __b)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[2 * __i] + (uint64_t)__b[2 * __i + 1];
  return __r;
}

__C17_INTRIN int32x4_t vpaddq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 4] + (uint64_t)__b[2 * __i - 4 + 1];
  return __r;
}

__C17_INTRIN int32_t vaddvq_s32(int32x4_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 4; __i++)
    __s += (uint64_t)__a[__i];
  return (int32_t)__s;
}

__C17_INTRIN int32x4_t vpmaxq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? (__a[2 * __i] > __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 4] > __b[2 * __i - 4 + 1] ? __b[2 * __i - 4] : __b[2 * __i - 4 + 1]);
  return __r;
}

__C17_INTRIN int32x4_t vpminq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? (__a[2 * __i] < __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 4] < __b[2 * __i - 4 + 1] ? __b[2 * __i - 4] : __b[2 * __i - 4 + 1]);
  return __r;
}

__C17_INTRIN int32_t vmaxvq_s32(int32x4_t __a)
{
  int32_t __m = __a[0];
  for (int __i = 1; __i < 4; __i++)
    if (__a[__i] > __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN int32_t vminvq_s32(int32x4_t __a)
{
  int32_t __m = __a[0];
  for (int __i = 1; __i < 4; __i++)
    if (__a[__i] < __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN int64_t vaddlvq_s32(int32x4_t __a)
{
  int64_t __s = 0;
  for (int __i = 0; __i < 4; __i++)
    __s += __a[__i];
  return __s;
}

__C17_INTRIN int64x2_t vpaddlq_s32(int32x4_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (int64_t)__a[2 * __i] + __a[2 * __i + 1];
  return __r;
}

__C17_INTRIN int64x2_t vpadalq_s32(int64x2_t __a, int32x4_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[2 * __i] + (uint64_t)__b[2 * __i + 1];
  return __r;
}

__C17_INTRIN int64x2_t vpaddq_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 2] + (uint64_t)__b[2 * __i - 2 + 1];
  return __r;
}

__C17_INTRIN int64_t vaddvq_s64(int64x2_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 2; __i++)
    __s += (uint64_t)__a[__i];
  return (int64_t)__s;
}

__C17_INTRIN uint8x8_t vpadd_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 8] + (uint64_t)__b[2 * __i - 8 + 1];
  return __r;
}

__C17_INTRIN uint8_t vaddv_u8(uint8x8_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 8; __i++)
    __s += (uint64_t)__a[__i];
  return (uint8_t)__s;
}

__C17_INTRIN uint8x8_t vpmax_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? (__a[2 * __i] > __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 8] > __b[2 * __i - 8 + 1] ? __b[2 * __i - 8] : __b[2 * __i - 8 + 1]);
  return __r;
}

__C17_INTRIN uint8x8_t vpmin_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? (__a[2 * __i] < __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 8] < __b[2 * __i - 8 + 1] ? __b[2 * __i - 8] : __b[2 * __i - 8 + 1]);
  return __r;
}

__C17_INTRIN uint8_t vmaxv_u8(uint8x8_t __a)
{
  uint8_t __m = __a[0];
  for (int __i = 1; __i < 8; __i++)
    if (__a[__i] > __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN uint8_t vminv_u8(uint8x8_t __a)
{
  uint8_t __m = __a[0];
  for (int __i = 1; __i < 8; __i++)
    if (__a[__i] < __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN uint16_t vaddlv_u8(uint8x8_t __a)
{
  uint16_t __s = 0;
  for (int __i = 0; __i < 8; __i++)
    __s += __a[__i];
  return __s;
}

__C17_INTRIN uint16x4_t vpaddl_u8(uint8x8_t __a)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint16_t)__a[2 * __i] + __a[2 * __i + 1];
  return __r;
}

__C17_INTRIN uint16x4_t vpadal_u8(uint16x4_t __a, uint8x8_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[2 * __i] + (uint64_t)__b[2 * __i + 1];
  return __r;
}

__C17_INTRIN uint8x16_t vpaddq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 16] + (uint64_t)__b[2 * __i - 16 + 1];
  return __r;
}

__C17_INTRIN uint8_t vaddvq_u8(uint8x16_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 16; __i++)
    __s += (uint64_t)__a[__i];
  return (uint8_t)__s;
}

__C17_INTRIN uint8x16_t vpmaxq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? (__a[2 * __i] > __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 16] > __b[2 * __i - 16 + 1] ? __b[2 * __i - 16] : __b[2 * __i - 16 + 1]);
  return __r;
}

__C17_INTRIN uint8x16_t vpminq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? (__a[2 * __i] < __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 16] < __b[2 * __i - 16 + 1] ? __b[2 * __i - 16] : __b[2 * __i - 16 + 1]);
  return __r;
}

__C17_INTRIN uint8_t vmaxvq_u8(uint8x16_t __a)
{
  uint8_t __m = __a[0];
  for (int __i = 1; __i < 16; __i++)
    if (__a[__i] > __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN uint8_t vminvq_u8(uint8x16_t __a)
{
  uint8_t __m = __a[0];
  for (int __i = 1; __i < 16; __i++)
    if (__a[__i] < __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN uint16_t vaddlvq_u8(uint8x16_t __a)
{
  uint16_t __s = 0;
  for (int __i = 0; __i < 16; __i++)
    __s += __a[__i];
  return __s;
}

__C17_INTRIN uint16x8_t vpaddlq_u8(uint8x16_t __a)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint16_t)__a[2 * __i] + __a[2 * __i + 1];
  return __r;
}

__C17_INTRIN uint16x8_t vpadalq_u8(uint16x8_t __a, uint8x16_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[2 * __i] + (uint64_t)__b[2 * __i + 1];
  return __r;
}

__C17_INTRIN uint16x4_t vpadd_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 4] + (uint64_t)__b[2 * __i - 4 + 1];
  return __r;
}

__C17_INTRIN uint16_t vaddv_u16(uint16x4_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 4; __i++)
    __s += (uint64_t)__a[__i];
  return (uint16_t)__s;
}

__C17_INTRIN uint16x4_t vpmax_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? (__a[2 * __i] > __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 4] > __b[2 * __i - 4 + 1] ? __b[2 * __i - 4] : __b[2 * __i - 4 + 1]);
  return __r;
}

__C17_INTRIN uint16x4_t vpmin_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? (__a[2 * __i] < __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 4] < __b[2 * __i - 4 + 1] ? __b[2 * __i - 4] : __b[2 * __i - 4 + 1]);
  return __r;
}

__C17_INTRIN uint16_t vmaxv_u16(uint16x4_t __a)
{
  uint16_t __m = __a[0];
  for (int __i = 1; __i < 4; __i++)
    if (__a[__i] > __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN uint16_t vminv_u16(uint16x4_t __a)
{
  uint16_t __m = __a[0];
  for (int __i = 1; __i < 4; __i++)
    if (__a[__i] < __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN uint32_t vaddlv_u16(uint16x4_t __a)
{
  uint32_t __s = 0;
  for (int __i = 0; __i < 4; __i++)
    __s += __a[__i];
  return __s;
}

__C17_INTRIN uint32x2_t vpaddl_u16(uint16x4_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint32_t)__a[2 * __i] + __a[2 * __i + 1];
  return __r;
}

__C17_INTRIN uint32x2_t vpadal_u16(uint32x2_t __a, uint16x4_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[2 * __i] + (uint64_t)__b[2 * __i + 1];
  return __r;
}

__C17_INTRIN uint16x8_t vpaddq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 8] + (uint64_t)__b[2 * __i - 8 + 1];
  return __r;
}

__C17_INTRIN uint16_t vaddvq_u16(uint16x8_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 8; __i++)
    __s += (uint64_t)__a[__i];
  return (uint16_t)__s;
}

__C17_INTRIN uint16x8_t vpmaxq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? (__a[2 * __i] > __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 8] > __b[2 * __i - 8 + 1] ? __b[2 * __i - 8] : __b[2 * __i - 8 + 1]);
  return __r;
}

__C17_INTRIN uint16x8_t vpminq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? (__a[2 * __i] < __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 8] < __b[2 * __i - 8 + 1] ? __b[2 * __i - 8] : __b[2 * __i - 8 + 1]);
  return __r;
}

__C17_INTRIN uint16_t vmaxvq_u16(uint16x8_t __a)
{
  uint16_t __m = __a[0];
  for (int __i = 1; __i < 8; __i++)
    if (__a[__i] > __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN uint16_t vminvq_u16(uint16x8_t __a)
{
  uint16_t __m = __a[0];
  for (int __i = 1; __i < 8; __i++)
    if (__a[__i] < __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN uint32_t vaddlvq_u16(uint16x8_t __a)
{
  uint32_t __s = 0;
  for (int __i = 0; __i < 8; __i++)
    __s += __a[__i];
  return __s;
}

__C17_INTRIN uint32x4_t vpaddlq_u16(uint16x8_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint32_t)__a[2 * __i] + __a[2 * __i + 1];
  return __r;
}

__C17_INTRIN uint32x4_t vpadalq_u16(uint32x4_t __a, uint16x8_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[2 * __i] + (uint64_t)__b[2 * __i + 1];
  return __r;
}

__C17_INTRIN uint32x2_t vpadd_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 2] + (uint64_t)__b[2 * __i - 2 + 1];
  return __r;
}

__C17_INTRIN uint32_t vaddv_u32(uint32x2_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 2; __i++)
    __s += (uint64_t)__a[__i];
  return (uint32_t)__s;
}

__C17_INTRIN uint32x2_t vpmax_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? (__a[2 * __i] > __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 2] > __b[2 * __i - 2 + 1] ? __b[2 * __i - 2] : __b[2 * __i - 2 + 1]);
  return __r;
}

__C17_INTRIN uint32x2_t vpmin_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? (__a[2 * __i] < __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 2] < __b[2 * __i - 2 + 1] ? __b[2 * __i - 2] : __b[2 * __i - 2 + 1]);
  return __r;
}

__C17_INTRIN uint32_t vmaxv_u32(uint32x2_t __a)
{
  uint32_t __m = __a[0];
  for (int __i = 1; __i < 2; __i++)
    if (__a[__i] > __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN uint32_t vminv_u32(uint32x2_t __a)
{
  uint32_t __m = __a[0];
  for (int __i = 1; __i < 2; __i++)
    if (__a[__i] < __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN uint64_t vaddlv_u32(uint32x2_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 2; __i++)
    __s += __a[__i];
  return __s;
}

__C17_INTRIN uint64x1_t vpaddl_u32(uint32x2_t __a)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = (uint64_t)__a[2 * __i] + __a[2 * __i + 1];
  return __r;
}

__C17_INTRIN uint64x1_t vpadal_u32(uint64x1_t __a, uint32x2_t __b)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[2 * __i] + (uint64_t)__b[2 * __i + 1];
  return __r;
}

__C17_INTRIN uint32x4_t vpaddq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 4] + (uint64_t)__b[2 * __i - 4 + 1];
  return __r;
}

__C17_INTRIN uint32_t vaddvq_u32(uint32x4_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 4; __i++)
    __s += (uint64_t)__a[__i];
  return (uint32_t)__s;
}

__C17_INTRIN uint32x4_t vpmaxq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? (__a[2 * __i] > __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 4] > __b[2 * __i - 4 + 1] ? __b[2 * __i - 4] : __b[2 * __i - 4 + 1]);
  return __r;
}

__C17_INTRIN uint32x4_t vpminq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? (__a[2 * __i] < __a[2 * __i + 1] ? __a[2 * __i] : __a[2 * __i + 1]) : (__b[2 * __i - 4] < __b[2 * __i - 4 + 1] ? __b[2 * __i - 4] : __b[2 * __i - 4 + 1]);
  return __r;
}

__C17_INTRIN uint32_t vmaxvq_u32(uint32x4_t __a)
{
  uint32_t __m = __a[0];
  for (int __i = 1; __i < 4; __i++)
    if (__a[__i] > __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN uint32_t vminvq_u32(uint32x4_t __a)
{
  uint32_t __m = __a[0];
  for (int __i = 1; __i < 4; __i++)
    if (__a[__i] < __m)
      __m = __a[__i];
  return __m;
}

__C17_INTRIN uint64_t vaddlvq_u32(uint32x4_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 4; __i++)
    __s += __a[__i];
  return __s;
}

__C17_INTRIN uint64x2_t vpaddlq_u32(uint32x4_t __a)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[2 * __i] + __a[2 * __i + 1];
  return __r;
}

__C17_INTRIN uint64x2_t vpadalq_u32(uint64x2_t __a, uint32x4_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__b[2 * __i] + (uint64_t)__b[2 * __i + 1];
  return __r;
}

__C17_INTRIN uint64x2_t vpaddq_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? (uint64_t)__a[2 * __i] + (uint64_t)__a[2 * __i + 1] : (uint64_t)__b[2 * __i - 2] + (uint64_t)__b[2 * __i - 2 + 1];
  return __r;
}

__C17_INTRIN uint64_t vaddvq_u64(uint64x2_t __a)
{
  uint64_t __s = 0;
  for (int __i = 0; __i < 2; __i++)
    __s += (uint64_t)__a[__i];
  return (uint64_t)__s;
}

__C17_INTRIN float32x2_t vpadd_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i] + __a[2 * __i + 1] : __b[2 * __i - 2] + __b[2 * __i - 2 + 1];
  return __r;
}

__C17_INTRIN float32x2_t vpmax_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __c17_fmax_f32(__a[2 * __i], __a[2 * __i + 1]) : __c17_fmax_f32(__b[2 * __i - 2], __b[2 * __i - 2 + 1]);
  return __r;
}

__C17_INTRIN float32x2_t vpmin_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __c17_fmin_f32(__a[2 * __i], __a[2 * __i + 1]) : __c17_fmin_f32(__b[2 * __i - 2], __b[2 * __i - 2 + 1]);
  return __r;
}

__C17_INTRIN float32x2_t vpmaxnm_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __c17_fmaxnm_f32(__a[2 * __i], __a[2 * __i + 1]) : __c17_fmaxnm_f32(__b[2 * __i - 2], __b[2 * __i - 2 + 1]);
  return __r;
}

__C17_INTRIN float32x2_t vpminnm_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __c17_fminnm_f32(__a[2 * __i], __a[2 * __i + 1]) : __c17_fminnm_f32(__b[2 * __i - 2], __b[2 * __i - 2 + 1]);
  return __r;
}

__C17_INTRIN float32_t vaddv_f32(float32x2_t __a)
{
  return __a[0] + __a[1];
}

__C17_INTRIN float32_t vmaxv_f32(float32x2_t __a)
{
  return __c17_fmax_f32(__a[0], __a[1]);
}

__C17_INTRIN float32_t vminv_f32(float32x2_t __a)
{
  return __c17_fmin_f32(__a[0], __a[1]);
}

__C17_INTRIN float32_t vmaxnmv_f32(float32x2_t __a)
{
  return __c17_fmaxnm_f32(__a[0], __a[1]);
}

__C17_INTRIN float32_t vminnmv_f32(float32x2_t __a)
{
  return __c17_fminnm_f32(__a[0], __a[1]);
}

__C17_INTRIN float32x4_t vpaddq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i] + __a[2 * __i + 1] : __b[2 * __i - 4] + __b[2 * __i - 4 + 1];
  return __r;
}

__C17_INTRIN float32x4_t vpmaxq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __c17_fmax_f32(__a[2 * __i], __a[2 * __i + 1]) : __c17_fmax_f32(__b[2 * __i - 4], __b[2 * __i - 4 + 1]);
  return __r;
}

__C17_INTRIN float32x4_t vpminq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __c17_fmin_f32(__a[2 * __i], __a[2 * __i + 1]) : __c17_fmin_f32(__b[2 * __i - 4], __b[2 * __i - 4 + 1]);
  return __r;
}

__C17_INTRIN float32x4_t vpmaxnmq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __c17_fmaxnm_f32(__a[2 * __i], __a[2 * __i + 1]) : __c17_fmaxnm_f32(__b[2 * __i - 4], __b[2 * __i - 4 + 1]);
  return __r;
}

__C17_INTRIN float32x4_t vpminnmq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __c17_fminnm_f32(__a[2 * __i], __a[2 * __i + 1]) : __c17_fminnm_f32(__b[2 * __i - 4], __b[2 * __i - 4 + 1]);
  return __r;
}

__C17_INTRIN float32_t vaddvq_f32(float32x4_t __a)
{
  return (__a[0] + __a[1]) + (__a[2] + __a[3]);
}

__C17_INTRIN float32_t vmaxvq_f32(float32x4_t __a)
{
  return __c17_fmax_f32(__c17_fmax_f32(__a[0], __a[1]), __c17_fmax_f32(__a[2], __a[3]));
}

__C17_INTRIN float32_t vminvq_f32(float32x4_t __a)
{
  return __c17_fmin_f32(__c17_fmin_f32(__a[0], __a[1]), __c17_fmin_f32(__a[2], __a[3]));
}

__C17_INTRIN float32_t vmaxnmvq_f32(float32x4_t __a)
{
  return __c17_fmaxnm_f32(__c17_fmaxnm_f32(__a[0], __a[1]), __c17_fmaxnm_f32(__a[2], __a[3]));
}

__C17_INTRIN float32_t vminnmvq_f32(float32x4_t __a)
{
  return __c17_fminnm_f32(__c17_fminnm_f32(__a[0], __a[1]), __c17_fminnm_f32(__a[2], __a[3]));
}

__C17_INTRIN float64x2_t vpaddq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i] + __a[2 * __i + 1] : __b[2 * __i - 2] + __b[2 * __i - 2 + 1];
  return __r;
}

__C17_INTRIN float64x2_t vpmaxq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __c17_fmax_f64(__a[2 * __i], __a[2 * __i + 1]) : __c17_fmax_f64(__b[2 * __i - 2], __b[2 * __i - 2 + 1]);
  return __r;
}

__C17_INTRIN float64x2_t vpminq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __c17_fmin_f64(__a[2 * __i], __a[2 * __i + 1]) : __c17_fmin_f64(__b[2 * __i - 2], __b[2 * __i - 2 + 1]);
  return __r;
}

__C17_INTRIN float64x2_t vpmaxnmq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __c17_fmaxnm_f64(__a[2 * __i], __a[2 * __i + 1]) : __c17_fmaxnm_f64(__b[2 * __i - 2], __b[2 * __i - 2 + 1]);
  return __r;
}

__C17_INTRIN float64x2_t vpminnmq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __c17_fminnm_f64(__a[2 * __i], __a[2 * __i + 1]) : __c17_fminnm_f64(__b[2 * __i - 2], __b[2 * __i - 2 + 1]);
  return __r;
}

__C17_INTRIN float64_t vaddvq_f64(float64x2_t __a)
{
  return __a[0] + __a[1];
}

__C17_INTRIN float64_t vmaxvq_f64(float64x2_t __a)
{
  return __c17_fmax_f64(__a[0], __a[1]);
}

__C17_INTRIN float64_t vminvq_f64(float64x2_t __a)
{
  return __c17_fmin_f64(__a[0], __a[1]);
}

__C17_INTRIN float64_t vmaxnmvq_f64(float64x2_t __a)
{
  return __c17_fmaxnm_f64(__a[0], __a[1]);
}

__C17_INTRIN float64_t vminnmvq_f64(float64x2_t __a)
{
  return __c17_fminnm_f64(__a[0], __a[1]);
}


/* Comparisons and bitwise operations. */

__C17_INTRIN uint8x8_t vceq_s8(int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)(__a == __b);
}

__C17_INTRIN uint8x8_t vceqz_s8(int8x8_t __a)
{
  return (uint8x8_t)(__a == (int8x8_t){0});
}

__C17_INTRIN uint8x8_t vcge_s8(int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)(__a >= __b);
}

__C17_INTRIN uint8x8_t vcgez_s8(int8x8_t __a)
{
  return (uint8x8_t)(__a >= (int8x8_t){0});
}

__C17_INTRIN uint8x8_t vcgt_s8(int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)(__a > __b);
}

__C17_INTRIN uint8x8_t vcgtz_s8(int8x8_t __a)
{
  return (uint8x8_t)(__a > (int8x8_t){0});
}

__C17_INTRIN uint8x8_t vcle_s8(int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)(__a <= __b);
}

__C17_INTRIN uint8x8_t vclez_s8(int8x8_t __a)
{
  return (uint8x8_t)(__a <= (int8x8_t){0});
}

__C17_INTRIN uint8x8_t vclt_s8(int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)(__a < __b);
}

__C17_INTRIN uint8x8_t vcltz_s8(int8x8_t __a)
{
  return (uint8x8_t)(__a < (int8x8_t){0});
}

__C17_INTRIN uint8x8_t vtst_s8(int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)((__a & __b) != (int8x8_t){0});
}

__C17_INTRIN int8x8_t vand_s8(int8x8_t __a, int8x8_t __b)
{
  return __a & __b;
}

__C17_INTRIN int8x8_t vorr_s8(int8x8_t __a, int8x8_t __b)
{
  return __a | __b;
}

__C17_INTRIN int8x8_t veor_s8(int8x8_t __a, int8x8_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN int8x8_t vbic_s8(int8x8_t __a, int8x8_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN int8x8_t vorn_s8(int8x8_t __a, int8x8_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN int8x8_t vmvn_s8(int8x8_t __a)
{
  return ~__a;
}

__C17_INTRIN int8x8_t vbsl_s8(uint8x8_t __m, int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)((__m & (uint8x8_t)__a) | (~__m & (uint8x8_t)__b));
}

__C17_INTRIN uint8x16_t vceqq_s8(int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)(__a == __b);
}

__C17_INTRIN uint8x16_t vceqzq_s8(int8x16_t __a)
{
  return (uint8x16_t)(__a == (int8x16_t){0});
}

__C17_INTRIN uint8x16_t vcgeq_s8(int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)(__a >= __b);
}

__C17_INTRIN uint8x16_t vcgezq_s8(int8x16_t __a)
{
  return (uint8x16_t)(__a >= (int8x16_t){0});
}

__C17_INTRIN uint8x16_t vcgtq_s8(int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)(__a > __b);
}

__C17_INTRIN uint8x16_t vcgtzq_s8(int8x16_t __a)
{
  return (uint8x16_t)(__a > (int8x16_t){0});
}

__C17_INTRIN uint8x16_t vcleq_s8(int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)(__a <= __b);
}

__C17_INTRIN uint8x16_t vclezq_s8(int8x16_t __a)
{
  return (uint8x16_t)(__a <= (int8x16_t){0});
}

__C17_INTRIN uint8x16_t vcltq_s8(int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)(__a < __b);
}

__C17_INTRIN uint8x16_t vcltzq_s8(int8x16_t __a)
{
  return (uint8x16_t)(__a < (int8x16_t){0});
}

__C17_INTRIN uint8x16_t vtstq_s8(int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)((__a & __b) != (int8x16_t){0});
}

__C17_INTRIN int8x16_t vandq_s8(int8x16_t __a, int8x16_t __b)
{
  return __a & __b;
}

__C17_INTRIN int8x16_t vorrq_s8(int8x16_t __a, int8x16_t __b)
{
  return __a | __b;
}

__C17_INTRIN int8x16_t veorq_s8(int8x16_t __a, int8x16_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN int8x16_t vbicq_s8(int8x16_t __a, int8x16_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN int8x16_t vornq_s8(int8x16_t __a, int8x16_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN int8x16_t vmvnq_s8(int8x16_t __a)
{
  return ~__a;
}

__C17_INTRIN int8x16_t vbslq_s8(uint8x16_t __m, int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)((__m & (uint8x16_t)__a) | (~__m & (uint8x16_t)__b));
}

__C17_INTRIN uint16x4_t vceq_s16(int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)(__a == __b);
}

__C17_INTRIN uint16x4_t vceqz_s16(int16x4_t __a)
{
  return (uint16x4_t)(__a == (int16x4_t){0});
}

__C17_INTRIN uint16x4_t vcge_s16(int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)(__a >= __b);
}

__C17_INTRIN uint16x4_t vcgez_s16(int16x4_t __a)
{
  return (uint16x4_t)(__a >= (int16x4_t){0});
}

__C17_INTRIN uint16x4_t vcgt_s16(int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)(__a > __b);
}

__C17_INTRIN uint16x4_t vcgtz_s16(int16x4_t __a)
{
  return (uint16x4_t)(__a > (int16x4_t){0});
}

__C17_INTRIN uint16x4_t vcle_s16(int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)(__a <= __b);
}

__C17_INTRIN uint16x4_t vclez_s16(int16x4_t __a)
{
  return (uint16x4_t)(__a <= (int16x4_t){0});
}

__C17_INTRIN uint16x4_t vclt_s16(int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)(__a < __b);
}

__C17_INTRIN uint16x4_t vcltz_s16(int16x4_t __a)
{
  return (uint16x4_t)(__a < (int16x4_t){0});
}

__C17_INTRIN uint16x4_t vtst_s16(int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)((__a & __b) != (int16x4_t){0});
}

__C17_INTRIN int16x4_t vand_s16(int16x4_t __a, int16x4_t __b)
{
  return __a & __b;
}

__C17_INTRIN int16x4_t vorr_s16(int16x4_t __a, int16x4_t __b)
{
  return __a | __b;
}

__C17_INTRIN int16x4_t veor_s16(int16x4_t __a, int16x4_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN int16x4_t vbic_s16(int16x4_t __a, int16x4_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN int16x4_t vorn_s16(int16x4_t __a, int16x4_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN int16x4_t vmvn_s16(int16x4_t __a)
{
  return ~__a;
}

__C17_INTRIN int16x4_t vbsl_s16(uint16x4_t __m, int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)((__m & (uint16x4_t)__a) | (~__m & (uint16x4_t)__b));
}

__C17_INTRIN uint16x8_t vceqq_s16(int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)(__a == __b);
}

__C17_INTRIN uint16x8_t vceqzq_s16(int16x8_t __a)
{
  return (uint16x8_t)(__a == (int16x8_t){0});
}

__C17_INTRIN uint16x8_t vcgeq_s16(int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)(__a >= __b);
}

__C17_INTRIN uint16x8_t vcgezq_s16(int16x8_t __a)
{
  return (uint16x8_t)(__a >= (int16x8_t){0});
}

__C17_INTRIN uint16x8_t vcgtq_s16(int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)(__a > __b);
}

__C17_INTRIN uint16x8_t vcgtzq_s16(int16x8_t __a)
{
  return (uint16x8_t)(__a > (int16x8_t){0});
}

__C17_INTRIN uint16x8_t vcleq_s16(int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)(__a <= __b);
}

__C17_INTRIN uint16x8_t vclezq_s16(int16x8_t __a)
{
  return (uint16x8_t)(__a <= (int16x8_t){0});
}

__C17_INTRIN uint16x8_t vcltq_s16(int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)(__a < __b);
}

__C17_INTRIN uint16x8_t vcltzq_s16(int16x8_t __a)
{
  return (uint16x8_t)(__a < (int16x8_t){0});
}

__C17_INTRIN uint16x8_t vtstq_s16(int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)((__a & __b) != (int16x8_t){0});
}

__C17_INTRIN int16x8_t vandq_s16(int16x8_t __a, int16x8_t __b)
{
  return __a & __b;
}

__C17_INTRIN int16x8_t vorrq_s16(int16x8_t __a, int16x8_t __b)
{
  return __a | __b;
}

__C17_INTRIN int16x8_t veorq_s16(int16x8_t __a, int16x8_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN int16x8_t vbicq_s16(int16x8_t __a, int16x8_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN int16x8_t vornq_s16(int16x8_t __a, int16x8_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN int16x8_t vmvnq_s16(int16x8_t __a)
{
  return ~__a;
}

__C17_INTRIN int16x8_t vbslq_s16(uint16x8_t __m, int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)((__m & (uint16x8_t)__a) | (~__m & (uint16x8_t)__b));
}

__C17_INTRIN uint32x2_t vceq_s32(int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)(__a == __b);
}

__C17_INTRIN uint32x2_t vceqz_s32(int32x2_t __a)
{
  return (uint32x2_t)(__a == (int32x2_t){0});
}

__C17_INTRIN uint32x2_t vcge_s32(int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)(__a >= __b);
}

__C17_INTRIN uint32x2_t vcgez_s32(int32x2_t __a)
{
  return (uint32x2_t)(__a >= (int32x2_t){0});
}

__C17_INTRIN uint32x2_t vcgt_s32(int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)(__a > __b);
}

__C17_INTRIN uint32x2_t vcgtz_s32(int32x2_t __a)
{
  return (uint32x2_t)(__a > (int32x2_t){0});
}

__C17_INTRIN uint32x2_t vcle_s32(int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)(__a <= __b);
}

__C17_INTRIN uint32x2_t vclez_s32(int32x2_t __a)
{
  return (uint32x2_t)(__a <= (int32x2_t){0});
}

__C17_INTRIN uint32x2_t vclt_s32(int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)(__a < __b);
}

__C17_INTRIN uint32x2_t vcltz_s32(int32x2_t __a)
{
  return (uint32x2_t)(__a < (int32x2_t){0});
}

__C17_INTRIN uint32x2_t vtst_s32(int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)((__a & __b) != (int32x2_t){0});
}

__C17_INTRIN int32x2_t vand_s32(int32x2_t __a, int32x2_t __b)
{
  return __a & __b;
}

__C17_INTRIN int32x2_t vorr_s32(int32x2_t __a, int32x2_t __b)
{
  return __a | __b;
}

__C17_INTRIN int32x2_t veor_s32(int32x2_t __a, int32x2_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN int32x2_t vbic_s32(int32x2_t __a, int32x2_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN int32x2_t vorn_s32(int32x2_t __a, int32x2_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN int32x2_t vmvn_s32(int32x2_t __a)
{
  return ~__a;
}

__C17_INTRIN int32x2_t vbsl_s32(uint32x2_t __m, int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)((__m & (uint32x2_t)__a) | (~__m & (uint32x2_t)__b));
}

__C17_INTRIN uint32x4_t vceqq_s32(int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)(__a == __b);
}

__C17_INTRIN uint32x4_t vceqzq_s32(int32x4_t __a)
{
  return (uint32x4_t)(__a == (int32x4_t){0});
}

__C17_INTRIN uint32x4_t vcgeq_s32(int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)(__a >= __b);
}

__C17_INTRIN uint32x4_t vcgezq_s32(int32x4_t __a)
{
  return (uint32x4_t)(__a >= (int32x4_t){0});
}

__C17_INTRIN uint32x4_t vcgtq_s32(int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)(__a > __b);
}

__C17_INTRIN uint32x4_t vcgtzq_s32(int32x4_t __a)
{
  return (uint32x4_t)(__a > (int32x4_t){0});
}

__C17_INTRIN uint32x4_t vcleq_s32(int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)(__a <= __b);
}

__C17_INTRIN uint32x4_t vclezq_s32(int32x4_t __a)
{
  return (uint32x4_t)(__a <= (int32x4_t){0});
}

__C17_INTRIN uint32x4_t vcltq_s32(int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)(__a < __b);
}

__C17_INTRIN uint32x4_t vcltzq_s32(int32x4_t __a)
{
  return (uint32x4_t)(__a < (int32x4_t){0});
}

__C17_INTRIN uint32x4_t vtstq_s32(int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)((__a & __b) != (int32x4_t){0});
}

__C17_INTRIN int32x4_t vandq_s32(int32x4_t __a, int32x4_t __b)
{
  return __a & __b;
}

__C17_INTRIN int32x4_t vorrq_s32(int32x4_t __a, int32x4_t __b)
{
  return __a | __b;
}

__C17_INTRIN int32x4_t veorq_s32(int32x4_t __a, int32x4_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN int32x4_t vbicq_s32(int32x4_t __a, int32x4_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN int32x4_t vornq_s32(int32x4_t __a, int32x4_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN int32x4_t vmvnq_s32(int32x4_t __a)
{
  return ~__a;
}

__C17_INTRIN int32x4_t vbslq_s32(uint32x4_t __m, int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)((__m & (uint32x4_t)__a) | (~__m & (uint32x4_t)__b));
}

__C17_INTRIN uint64x1_t vceq_s64(int64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t)(__a == __b);
}

__C17_INTRIN uint64x1_t vceqz_s64(int64x1_t __a)
{
  return (uint64x1_t)(__a == (int64x1_t){0});
}

__C17_INTRIN uint64x1_t vcge_s64(int64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t)(__a >= __b);
}

__C17_INTRIN uint64x1_t vcgez_s64(int64x1_t __a)
{
  return (uint64x1_t)(__a >= (int64x1_t){0});
}

__C17_INTRIN uint64x1_t vcgt_s64(int64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t)(__a > __b);
}

__C17_INTRIN uint64x1_t vcgtz_s64(int64x1_t __a)
{
  return (uint64x1_t)(__a > (int64x1_t){0});
}

__C17_INTRIN uint64x1_t vcle_s64(int64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t)(__a <= __b);
}

__C17_INTRIN uint64x1_t vclez_s64(int64x1_t __a)
{
  return (uint64x1_t)(__a <= (int64x1_t){0});
}

__C17_INTRIN uint64x1_t vclt_s64(int64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t)(__a < __b);
}

__C17_INTRIN uint64x1_t vcltz_s64(int64x1_t __a)
{
  return (uint64x1_t)(__a < (int64x1_t){0});
}

__C17_INTRIN uint64x1_t vtst_s64(int64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t)((__a & __b) != (int64x1_t){0});
}

__C17_INTRIN int64x1_t vand_s64(int64x1_t __a, int64x1_t __b)
{
  return __a & __b;
}

__C17_INTRIN int64x1_t vorr_s64(int64x1_t __a, int64x1_t __b)
{
  return __a | __b;
}

__C17_INTRIN int64x1_t veor_s64(int64x1_t __a, int64x1_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN int64x1_t vbic_s64(int64x1_t __a, int64x1_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN int64x1_t vorn_s64(int64x1_t __a, int64x1_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN int64x1_t vbsl_s64(uint64x1_t __m, int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t)((__m & (uint64x1_t)__a) | (~__m & (uint64x1_t)__b));
}

__C17_INTRIN uint64x2_t vceqq_s64(int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t)(__a == __b);
}

__C17_INTRIN uint64x2_t vceqzq_s64(int64x2_t __a)
{
  return (uint64x2_t)(__a == (int64x2_t){0});
}

__C17_INTRIN uint64x2_t vcgeq_s64(int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t)(__a >= __b);
}

__C17_INTRIN uint64x2_t vcgezq_s64(int64x2_t __a)
{
  return (uint64x2_t)(__a >= (int64x2_t){0});
}

__C17_INTRIN uint64x2_t vcgtq_s64(int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t)(__a > __b);
}

__C17_INTRIN uint64x2_t vcgtzq_s64(int64x2_t __a)
{
  return (uint64x2_t)(__a > (int64x2_t){0});
}

__C17_INTRIN uint64x2_t vcleq_s64(int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t)(__a <= __b);
}

__C17_INTRIN uint64x2_t vclezq_s64(int64x2_t __a)
{
  return (uint64x2_t)(__a <= (int64x2_t){0});
}

__C17_INTRIN uint64x2_t vcltq_s64(int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t)(__a < __b);
}

__C17_INTRIN uint64x2_t vcltzq_s64(int64x2_t __a)
{
  return (uint64x2_t)(__a < (int64x2_t){0});
}

__C17_INTRIN uint64x2_t vtstq_s64(int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t)((__a & __b) != (int64x2_t){0});
}

__C17_INTRIN int64x2_t vandq_s64(int64x2_t __a, int64x2_t __b)
{
  return __a & __b;
}

__C17_INTRIN int64x2_t vorrq_s64(int64x2_t __a, int64x2_t __b)
{
  return __a | __b;
}

__C17_INTRIN int64x2_t veorq_s64(int64x2_t __a, int64x2_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN int64x2_t vbicq_s64(int64x2_t __a, int64x2_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN int64x2_t vornq_s64(int64x2_t __a, int64x2_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN int64x2_t vbslq_s64(uint64x2_t __m, int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t)((__m & (uint64x2_t)__a) | (~__m & (uint64x2_t)__b));
}

__C17_INTRIN uint8x8_t vceq_u8(uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)(__a == __b);
}

__C17_INTRIN uint8x8_t vceqz_u8(uint8x8_t __a)
{
  return (uint8x8_t)(__a == (uint8x8_t){0});
}

__C17_INTRIN uint8x8_t vcge_u8(uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)(__a >= __b);
}

__C17_INTRIN uint8x8_t vcgt_u8(uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)(__a > __b);
}

__C17_INTRIN uint8x8_t vcle_u8(uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)(__a <= __b);
}

__C17_INTRIN uint8x8_t vclt_u8(uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)(__a < __b);
}

__C17_INTRIN uint8x8_t vtst_u8(uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)((__a & __b) != (uint8x8_t){0});
}

__C17_INTRIN uint8x8_t vand_u8(uint8x8_t __a, uint8x8_t __b)
{
  return __a & __b;
}

__C17_INTRIN uint8x8_t vorr_u8(uint8x8_t __a, uint8x8_t __b)
{
  return __a | __b;
}

__C17_INTRIN uint8x8_t veor_u8(uint8x8_t __a, uint8x8_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN uint8x8_t vbic_u8(uint8x8_t __a, uint8x8_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN uint8x8_t vorn_u8(uint8x8_t __a, uint8x8_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN uint8x8_t vmvn_u8(uint8x8_t __a)
{
  return ~__a;
}

__C17_INTRIN uint8x8_t vbsl_u8(uint8x8_t __m, uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)((__m & (uint8x8_t)__a) | (~__m & (uint8x8_t)__b));
}

__C17_INTRIN uint8x16_t vceqq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)(__a == __b);
}

__C17_INTRIN uint8x16_t vceqzq_u8(uint8x16_t __a)
{
  return (uint8x16_t)(__a == (uint8x16_t){0});
}

__C17_INTRIN uint8x16_t vcgeq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)(__a >= __b);
}

__C17_INTRIN uint8x16_t vcgtq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)(__a > __b);
}

__C17_INTRIN uint8x16_t vcleq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)(__a <= __b);
}

__C17_INTRIN uint8x16_t vcltq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)(__a < __b);
}

__C17_INTRIN uint8x16_t vtstq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)((__a & __b) != (uint8x16_t){0});
}

__C17_INTRIN uint8x16_t vandq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return __a & __b;
}

__C17_INTRIN uint8x16_t vorrq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return __a | __b;
}

__C17_INTRIN uint8x16_t veorq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN uint8x16_t vbicq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN uint8x16_t vornq_u8(uint8x16_t __a, uint8x16_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN uint8x16_t vmvnq_u8(uint8x16_t __a)
{
  return ~__a;
}

__C17_INTRIN uint8x16_t vbslq_u8(uint8x16_t __m, uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)((__m & (uint8x16_t)__a) | (~__m & (uint8x16_t)__b));
}

__C17_INTRIN uint16x4_t vceq_u16(uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)(__a == __b);
}

__C17_INTRIN uint16x4_t vceqz_u16(uint16x4_t __a)
{
  return (uint16x4_t)(__a == (uint16x4_t){0});
}

__C17_INTRIN uint16x4_t vcge_u16(uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)(__a >= __b);
}

__C17_INTRIN uint16x4_t vcgt_u16(uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)(__a > __b);
}

__C17_INTRIN uint16x4_t vcle_u16(uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)(__a <= __b);
}

__C17_INTRIN uint16x4_t vclt_u16(uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)(__a < __b);
}

__C17_INTRIN uint16x4_t vtst_u16(uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)((__a & __b) != (uint16x4_t){0});
}

__C17_INTRIN uint16x4_t vand_u16(uint16x4_t __a, uint16x4_t __b)
{
  return __a & __b;
}

__C17_INTRIN uint16x4_t vorr_u16(uint16x4_t __a, uint16x4_t __b)
{
  return __a | __b;
}

__C17_INTRIN uint16x4_t veor_u16(uint16x4_t __a, uint16x4_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN uint16x4_t vbic_u16(uint16x4_t __a, uint16x4_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN uint16x4_t vorn_u16(uint16x4_t __a, uint16x4_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN uint16x4_t vmvn_u16(uint16x4_t __a)
{
  return ~__a;
}

__C17_INTRIN uint16x4_t vbsl_u16(uint16x4_t __m, uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)((__m & (uint16x4_t)__a) | (~__m & (uint16x4_t)__b));
}

__C17_INTRIN uint16x8_t vceqq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)(__a == __b);
}

__C17_INTRIN uint16x8_t vceqzq_u16(uint16x8_t __a)
{
  return (uint16x8_t)(__a == (uint16x8_t){0});
}

__C17_INTRIN uint16x8_t vcgeq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)(__a >= __b);
}

__C17_INTRIN uint16x8_t vcgtq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)(__a > __b);
}

__C17_INTRIN uint16x8_t vcleq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)(__a <= __b);
}

__C17_INTRIN uint16x8_t vcltq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)(__a < __b);
}

__C17_INTRIN uint16x8_t vtstq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)((__a & __b) != (uint16x8_t){0});
}

__C17_INTRIN uint16x8_t vandq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return __a & __b;
}

__C17_INTRIN uint16x8_t vorrq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return __a | __b;
}

__C17_INTRIN uint16x8_t veorq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN uint16x8_t vbicq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN uint16x8_t vornq_u16(uint16x8_t __a, uint16x8_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN uint16x8_t vmvnq_u16(uint16x8_t __a)
{
  return ~__a;
}

__C17_INTRIN uint16x8_t vbslq_u16(uint16x8_t __m, uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)((__m & (uint16x8_t)__a) | (~__m & (uint16x8_t)__b));
}

__C17_INTRIN uint32x2_t vceq_u32(uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)(__a == __b);
}

__C17_INTRIN uint32x2_t vceqz_u32(uint32x2_t __a)
{
  return (uint32x2_t)(__a == (uint32x2_t){0});
}

__C17_INTRIN uint32x2_t vcge_u32(uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)(__a >= __b);
}

__C17_INTRIN uint32x2_t vcgt_u32(uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)(__a > __b);
}

__C17_INTRIN uint32x2_t vcle_u32(uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)(__a <= __b);
}

__C17_INTRIN uint32x2_t vclt_u32(uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)(__a < __b);
}

__C17_INTRIN uint32x2_t vtst_u32(uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)((__a & __b) != (uint32x2_t){0});
}

__C17_INTRIN uint32x2_t vand_u32(uint32x2_t __a, uint32x2_t __b)
{
  return __a & __b;
}

__C17_INTRIN uint32x2_t vorr_u32(uint32x2_t __a, uint32x2_t __b)
{
  return __a | __b;
}

__C17_INTRIN uint32x2_t veor_u32(uint32x2_t __a, uint32x2_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN uint32x2_t vbic_u32(uint32x2_t __a, uint32x2_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN uint32x2_t vorn_u32(uint32x2_t __a, uint32x2_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN uint32x2_t vmvn_u32(uint32x2_t __a)
{
  return ~__a;
}

__C17_INTRIN uint32x2_t vbsl_u32(uint32x2_t __m, uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)((__m & (uint32x2_t)__a) | (~__m & (uint32x2_t)__b));
}

__C17_INTRIN uint32x4_t vceqq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)(__a == __b);
}

__C17_INTRIN uint32x4_t vceqzq_u32(uint32x4_t __a)
{
  return (uint32x4_t)(__a == (uint32x4_t){0});
}

__C17_INTRIN uint32x4_t vcgeq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)(__a >= __b);
}

__C17_INTRIN uint32x4_t vcgtq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)(__a > __b);
}

__C17_INTRIN uint32x4_t vcleq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)(__a <= __b);
}

__C17_INTRIN uint32x4_t vcltq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)(__a < __b);
}

__C17_INTRIN uint32x4_t vtstq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)((__a & __b) != (uint32x4_t){0});
}

__C17_INTRIN uint32x4_t vandq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return __a & __b;
}

__C17_INTRIN uint32x4_t vorrq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return __a | __b;
}

__C17_INTRIN uint32x4_t veorq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN uint32x4_t vbicq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN uint32x4_t vornq_u32(uint32x4_t __a, uint32x4_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN uint32x4_t vmvnq_u32(uint32x4_t __a)
{
  return ~__a;
}

__C17_INTRIN uint32x4_t vbslq_u32(uint32x4_t __m, uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)((__m & (uint32x4_t)__a) | (~__m & (uint32x4_t)__b));
}

__C17_INTRIN uint64x1_t vceq_u64(uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t)(__a == __b);
}

__C17_INTRIN uint64x1_t vceqz_u64(uint64x1_t __a)
{
  return (uint64x1_t)(__a == (uint64x1_t){0});
}

__C17_INTRIN uint64x1_t vcge_u64(uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t)(__a >= __b);
}

__C17_INTRIN uint64x1_t vcgt_u64(uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t)(__a > __b);
}

__C17_INTRIN uint64x1_t vcle_u64(uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t)(__a <= __b);
}

__C17_INTRIN uint64x1_t vclt_u64(uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t)(__a < __b);
}

__C17_INTRIN uint64x1_t vtst_u64(uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t)((__a & __b) != (uint64x1_t){0});
}

__C17_INTRIN uint64x1_t vand_u64(uint64x1_t __a, uint64x1_t __b)
{
  return __a & __b;
}

__C17_INTRIN uint64x1_t vorr_u64(uint64x1_t __a, uint64x1_t __b)
{
  return __a | __b;
}

__C17_INTRIN uint64x1_t veor_u64(uint64x1_t __a, uint64x1_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN uint64x1_t vbic_u64(uint64x1_t __a, uint64x1_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN uint64x1_t vorn_u64(uint64x1_t __a, uint64x1_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN uint64x1_t vbsl_u64(uint64x1_t __m, uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t)((__m & (uint64x1_t)__a) | (~__m & (uint64x1_t)__b));
}

__C17_INTRIN uint64x2_t vceqq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t)(__a == __b);
}

__C17_INTRIN uint64x2_t vceqzq_u64(uint64x2_t __a)
{
  return (uint64x2_t)(__a == (uint64x2_t){0});
}

__C17_INTRIN uint64x2_t vcgeq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t)(__a >= __b);
}

__C17_INTRIN uint64x2_t vcgtq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t)(__a > __b);
}

__C17_INTRIN uint64x2_t vcleq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t)(__a <= __b);
}

__C17_INTRIN uint64x2_t vcltq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t)(__a < __b);
}

__C17_INTRIN uint64x2_t vtstq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t)((__a & __b) != (uint64x2_t){0});
}

__C17_INTRIN uint64x2_t vandq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return __a & __b;
}

__C17_INTRIN uint64x2_t vorrq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return __a | __b;
}

__C17_INTRIN uint64x2_t veorq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return __a ^ __b;
}

__C17_INTRIN uint64x2_t vbicq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return __a & ~__b;
}

__C17_INTRIN uint64x2_t vornq_u64(uint64x2_t __a, uint64x2_t __b)
{
  return __a | ~__b;
}

__C17_INTRIN uint64x2_t vbslq_u64(uint64x2_t __m, uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t)((__m & (uint64x2_t)__a) | (~__m & (uint64x2_t)__b));
}

__C17_INTRIN uint32x2_t vceq_f32(float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)(__a == __b);
}

__C17_INTRIN uint32x2_t vceqz_f32(float32x2_t __a)
{
  return (uint32x2_t)(__a == (float32x2_t){0});
}

__C17_INTRIN uint32x2_t vcge_f32(float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)(__a >= __b);
}

__C17_INTRIN uint32x2_t vcgez_f32(float32x2_t __a)
{
  return (uint32x2_t)(__a >= (float32x2_t){0});
}

__C17_INTRIN uint32x2_t vcgt_f32(float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)(__a > __b);
}

__C17_INTRIN uint32x2_t vcgtz_f32(float32x2_t __a)
{
  return (uint32x2_t)(__a > (float32x2_t){0});
}

__C17_INTRIN uint32x2_t vcle_f32(float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)(__a <= __b);
}

__C17_INTRIN uint32x2_t vclez_f32(float32x2_t __a)
{
  return (uint32x2_t)(__a <= (float32x2_t){0});
}

__C17_INTRIN uint32x2_t vclt_f32(float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)(__a < __b);
}

__C17_INTRIN uint32x2_t vcltz_f32(float32x2_t __a)
{
  return (uint32x2_t)(__a < (float32x2_t){0});
}

__C17_INTRIN uint32x2_t vcage_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __x, __y;
  for (int __i = 0; __i < 2; __i++) {
    __x[__i] = __c17_fabs_f32(__a[__i]);
    __y[__i] = __c17_fabs_f32(__b[__i]);
  }
  return (uint32x2_t)(__x >= __y);
}

__C17_INTRIN uint32x2_t vcagt_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __x, __y;
  for (int __i = 0; __i < 2; __i++) {
    __x[__i] = __c17_fabs_f32(__a[__i]);
    __y[__i] = __c17_fabs_f32(__b[__i]);
  }
  return (uint32x2_t)(__x > __y);
}

__C17_INTRIN uint32x2_t vcale_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __x, __y;
  for (int __i = 0; __i < 2; __i++) {
    __x[__i] = __c17_fabs_f32(__a[__i]);
    __y[__i] = __c17_fabs_f32(__b[__i]);
  }
  return (uint32x2_t)(__x <= __y);
}

__C17_INTRIN uint32x2_t vcalt_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __x, __y;
  for (int __i = 0; __i < 2; __i++) {
    __x[__i] = __c17_fabs_f32(__a[__i]);
    __y[__i] = __c17_fabs_f32(__b[__i]);
  }
  return (uint32x2_t)(__x < __y);
}

__C17_INTRIN float32x2_t vbsl_f32(uint32x2_t __m, float32x2_t __a, float32x2_t __b)
{
  return (float32x2_t)((__m & (uint32x2_t)__a) | (~__m & (uint32x2_t)__b));
}

__C17_INTRIN uint32x4_t vceqq_f32(float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)(__a == __b);
}

__C17_INTRIN uint32x4_t vceqzq_f32(float32x4_t __a)
{
  return (uint32x4_t)(__a == (float32x4_t){0});
}

__C17_INTRIN uint32x4_t vcgeq_f32(float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)(__a >= __b);
}

__C17_INTRIN uint32x4_t vcgezq_f32(float32x4_t __a)
{
  return (uint32x4_t)(__a >= (float32x4_t){0});
}

__C17_INTRIN uint32x4_t vcgtq_f32(float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)(__a > __b);
}

__C17_INTRIN uint32x4_t vcgtzq_f32(float32x4_t __a)
{
  return (uint32x4_t)(__a > (float32x4_t){0});
}

__C17_INTRIN uint32x4_t vcleq_f32(float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)(__a <= __b);
}

__C17_INTRIN uint32x4_t vclezq_f32(float32x4_t __a)
{
  return (uint32x4_t)(__a <= (float32x4_t){0});
}

__C17_INTRIN uint32x4_t vcltq_f32(float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)(__a < __b);
}

__C17_INTRIN uint32x4_t vcltzq_f32(float32x4_t __a)
{
  return (uint32x4_t)(__a < (float32x4_t){0});
}

__C17_INTRIN uint32x4_t vcageq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __x, __y;
  for (int __i = 0; __i < 4; __i++) {
    __x[__i] = __c17_fabs_f32(__a[__i]);
    __y[__i] = __c17_fabs_f32(__b[__i]);
  }
  return (uint32x4_t)(__x >= __y);
}

__C17_INTRIN uint32x4_t vcagtq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __x, __y;
  for (int __i = 0; __i < 4; __i++) {
    __x[__i] = __c17_fabs_f32(__a[__i]);
    __y[__i] = __c17_fabs_f32(__b[__i]);
  }
  return (uint32x4_t)(__x > __y);
}

__C17_INTRIN uint32x4_t vcaleq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __x, __y;
  for (int __i = 0; __i < 4; __i++) {
    __x[__i] = __c17_fabs_f32(__a[__i]);
    __y[__i] = __c17_fabs_f32(__b[__i]);
  }
  return (uint32x4_t)(__x <= __y);
}

__C17_INTRIN uint32x4_t vcaltq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __x, __y;
  for (int __i = 0; __i < 4; __i++) {
    __x[__i] = __c17_fabs_f32(__a[__i]);
    __y[__i] = __c17_fabs_f32(__b[__i]);
  }
  return (uint32x4_t)(__x < __y);
}

__C17_INTRIN float32x4_t vbslq_f32(uint32x4_t __m, float32x4_t __a, float32x4_t __b)
{
  return (float32x4_t)((__m & (uint32x4_t)__a) | (~__m & (uint32x4_t)__b));
}

__C17_INTRIN uint64x1_t vceq_f64(float64x1_t __a, float64x1_t __b)
{
  return (uint64x1_t)(__a == __b);
}

__C17_INTRIN uint64x1_t vceqz_f64(float64x1_t __a)
{
  return (uint64x1_t)(__a == (float64x1_t){0});
}

__C17_INTRIN uint64x1_t vcge_f64(float64x1_t __a, float64x1_t __b)
{
  return (uint64x1_t)(__a >= __b);
}

__C17_INTRIN uint64x1_t vcgez_f64(float64x1_t __a)
{
  return (uint64x1_t)(__a >= (float64x1_t){0});
}

__C17_INTRIN uint64x1_t vcgt_f64(float64x1_t __a, float64x1_t __b)
{
  return (uint64x1_t)(__a > __b);
}

__C17_INTRIN uint64x1_t vcgtz_f64(float64x1_t __a)
{
  return (uint64x1_t)(__a > (float64x1_t){0});
}

__C17_INTRIN uint64x1_t vcle_f64(float64x1_t __a, float64x1_t __b)
{
  return (uint64x1_t)(__a <= __b);
}

__C17_INTRIN uint64x1_t vclez_f64(float64x1_t __a)
{
  return (uint64x1_t)(__a <= (float64x1_t){0});
}

__C17_INTRIN uint64x1_t vclt_f64(float64x1_t __a, float64x1_t __b)
{
  return (uint64x1_t)(__a < __b);
}

__C17_INTRIN uint64x1_t vcltz_f64(float64x1_t __a)
{
  return (uint64x1_t)(__a < (float64x1_t){0});
}

__C17_INTRIN uint64x1_t vcage_f64(float64x1_t __a, float64x1_t __b)
{
  float64x1_t __x, __y;
  for (int __i = 0; __i < 1; __i++) {
    __x[__i] = __c17_fabs_f64(__a[__i]);
    __y[__i] = __c17_fabs_f64(__b[__i]);
  }
  return (uint64x1_t)(__x >= __y);
}

__C17_INTRIN uint64x1_t vcagt_f64(float64x1_t __a, float64x1_t __b)
{
  float64x1_t __x, __y;
  for (int __i = 0; __i < 1; __i++) {
    __x[__i] = __c17_fabs_f64(__a[__i]);
    __y[__i] = __c17_fabs_f64(__b[__i]);
  }
  return (uint64x1_t)(__x > __y);
}

__C17_INTRIN uint64x1_t vcale_f64(float64x1_t __a, float64x1_t __b)
{
  float64x1_t __x, __y;
  for (int __i = 0; __i < 1; __i++) {
    __x[__i] = __c17_fabs_f64(__a[__i]);
    __y[__i] = __c17_fabs_f64(__b[__i]);
  }
  return (uint64x1_t)(__x <= __y);
}

__C17_INTRIN uint64x1_t vcalt_f64(float64x1_t __a, float64x1_t __b)
{
  float64x1_t __x, __y;
  for (int __i = 0; __i < 1; __i++) {
    __x[__i] = __c17_fabs_f64(__a[__i]);
    __y[__i] = __c17_fabs_f64(__b[__i]);
  }
  return (uint64x1_t)(__x < __y);
}

__C17_INTRIN float64x1_t vbsl_f64(uint64x1_t __m, float64x1_t __a, float64x1_t __b)
{
  return (float64x1_t)((__m & (uint64x1_t)__a) | (~__m & (uint64x1_t)__b));
}

__C17_INTRIN uint64x2_t vceqq_f64(float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t)(__a == __b);
}

__C17_INTRIN uint64x2_t vceqzq_f64(float64x2_t __a)
{
  return (uint64x2_t)(__a == (float64x2_t){0});
}

__C17_INTRIN uint64x2_t vcgeq_f64(float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t)(__a >= __b);
}

__C17_INTRIN uint64x2_t vcgezq_f64(float64x2_t __a)
{
  return (uint64x2_t)(__a >= (float64x2_t){0});
}

__C17_INTRIN uint64x2_t vcgtq_f64(float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t)(__a > __b);
}

__C17_INTRIN uint64x2_t vcgtzq_f64(float64x2_t __a)
{
  return (uint64x2_t)(__a > (float64x2_t){0});
}

__C17_INTRIN uint64x2_t vcleq_f64(float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t)(__a <= __b);
}

__C17_INTRIN uint64x2_t vclezq_f64(float64x2_t __a)
{
  return (uint64x2_t)(__a <= (float64x2_t){0});
}

__C17_INTRIN uint64x2_t vcltq_f64(float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t)(__a < __b);
}

__C17_INTRIN uint64x2_t vcltzq_f64(float64x2_t __a)
{
  return (uint64x2_t)(__a < (float64x2_t){0});
}

__C17_INTRIN uint64x2_t vcageq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __x, __y;
  for (int __i = 0; __i < 2; __i++) {
    __x[__i] = __c17_fabs_f64(__a[__i]);
    __y[__i] = __c17_fabs_f64(__b[__i]);
  }
  return (uint64x2_t)(__x >= __y);
}

__C17_INTRIN uint64x2_t vcagtq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __x, __y;
  for (int __i = 0; __i < 2; __i++) {
    __x[__i] = __c17_fabs_f64(__a[__i]);
    __y[__i] = __c17_fabs_f64(__b[__i]);
  }
  return (uint64x2_t)(__x > __y);
}

__C17_INTRIN uint64x2_t vcaleq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __x, __y;
  for (int __i = 0; __i < 2; __i++) {
    __x[__i] = __c17_fabs_f64(__a[__i]);
    __y[__i] = __c17_fabs_f64(__b[__i]);
  }
  return (uint64x2_t)(__x <= __y);
}

__C17_INTRIN uint64x2_t vcaltq_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __x, __y;
  for (int __i = 0; __i < 2; __i++) {
    __x[__i] = __c17_fabs_f64(__a[__i]);
    __y[__i] = __c17_fabs_f64(__b[__i]);
  }
  return (uint64x2_t)(__x < __y);
}

__C17_INTRIN float64x2_t vbslq_f64(uint64x2_t __m, float64x2_t __a, float64x2_t __b)
{
  return (float64x2_t)((__m & (uint64x2_t)__a) | (~__m & (uint64x2_t)__b));
}

__C17_INTRIN uint8x8_t vceq_p8(poly8x8_t __a, poly8x8_t __b)
{
  return (uint8x8_t)(__a == __b);
}

__C17_INTRIN uint8x8_t vceqz_p8(poly8x8_t __a)
{
  return (uint8x8_t)(__a == (poly8x8_t){0});
}

__C17_INTRIN uint8x8_t vtst_p8(poly8x8_t __a, poly8x8_t __b)
{
  return (uint8x8_t)((__a & __b) != (poly8x8_t){0});
}

__C17_INTRIN poly8x8_t vmvn_p8(poly8x8_t __a)
{
  return ~__a;
}

__C17_INTRIN poly8x8_t vbsl_p8(uint8x8_t __m, poly8x8_t __a, poly8x8_t __b)
{
  return (poly8x8_t)((__m & (uint8x8_t)__a) | (~__m & (uint8x8_t)__b));
}

__C17_INTRIN uint8x16_t vceqq_p8(poly8x16_t __a, poly8x16_t __b)
{
  return (uint8x16_t)(__a == __b);
}

__C17_INTRIN uint8x16_t vceqzq_p8(poly8x16_t __a)
{
  return (uint8x16_t)(__a == (poly8x16_t){0});
}

__C17_INTRIN uint8x16_t vtstq_p8(poly8x16_t __a, poly8x16_t __b)
{
  return (uint8x16_t)((__a & __b) != (poly8x16_t){0});
}

__C17_INTRIN poly8x16_t vmvnq_p8(poly8x16_t __a)
{
  return ~__a;
}

__C17_INTRIN poly8x16_t vbslq_p8(uint8x16_t __m, poly8x16_t __a, poly8x16_t __b)
{
  return (poly8x16_t)((__m & (uint8x16_t)__a) | (~__m & (uint8x16_t)__b));
}

__C17_INTRIN uint16x4_t vtst_p16(poly16x4_t __a, poly16x4_t __b)
{
  return (uint16x4_t)((__a & __b) != (poly16x4_t){0});
}

__C17_INTRIN poly16x4_t vbsl_p16(uint16x4_t __m, poly16x4_t __a, poly16x4_t __b)
{
  return (poly16x4_t)((__m & (uint16x4_t)__a) | (~__m & (uint16x4_t)__b));
}

__C17_INTRIN uint16x8_t vtstq_p16(poly16x8_t __a, poly16x8_t __b)
{
  return (uint16x8_t)((__a & __b) != (poly16x8_t){0});
}

__C17_INTRIN poly16x8_t vbslq_p16(uint16x8_t __m, poly16x8_t __a, poly16x8_t __b)
{
  return (poly16x8_t)((__m & (uint16x8_t)__a) | (~__m & (uint16x8_t)__b));
}


/* Shifts. */

__C17_INTRIN int8x8_t vshl_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN int8x8_t vrshl_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN int8x8_t vqshl_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN int8x8_t vqrshl_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN int8x8_t __c17_vshl_n_s8(int8x8_t __a, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, __n, 0, 0);
  return __r;
}
#define vshl_n_s8(...) __c17_vshl_n_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vqshl_n_s8(int8x8_t __a, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, __n, 0, 1);
  return __r;
}
#define vqshl_n_s8(...) __c17_vqshl_n_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vshr_n_s8(int8x8_t __a, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, -__n, 0, 0);
  return __r;
}
#define vshr_n_s8(...) __c17_vshr_n_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vrshr_n_s8(int8x8_t __a, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, -__n, 1, 0);
  return __r;
}
#define vrshr_n_s8(...) __c17_vrshr_n_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vsra_n_s8(int8x8_t __a, int8x8_t __b, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 8, -__n, 0, 0);
  return __r;
}
#define vsra_n_s8(...) __c17_vsra_n_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vrsra_n_s8(int8x8_t __a, int8x8_t __b, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 8, -__n, 1, 0);
  return __r;
}
#define vrsra_n_s8(...) __c17_vrsra_n_s8(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vqshlu_n_s8(int8x8_t __a, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] < 0 ? 0 : __c17_shl_u((uint64_t)__a[__i], 8, __n, 0, 1);
  return __r;
}
#define vqshlu_n_s8(...) __c17_vqshlu_n_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vsli_n_s8(int8x8_t __a, int8x8_t __b, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)(uint8_t)(__b[__i]) << __n) | ((uint64_t)(uint8_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsli_n_s8(...) __c17_vsli_n_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t __c17_vsri_n_s8(int8x8_t __a, int8x8_t __b, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __n >= 8 ? (uint64_t)(uint8_t)(__a[__i]) : ((uint64_t)(uint8_t)(__b[__i]) >> __n) | ((uint64_t)(uint8_t)(__a[__i]) & ~((((uint64_t)1 << 8) - 1) >> __n));
  return __r;
}
#define vsri_n_s8(...) __c17_vsri_n_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t vshlq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN int8x16_t vrshlq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN int8x16_t vqshlq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN int8x16_t vqrshlq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN int8x16_t __c17_vshlq_n_s8(int8x16_t __a, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, __n, 0, 0);
  return __r;
}
#define vshlq_n_s8(...) __c17_vshlq_n_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vqshlq_n_s8(int8x16_t __a, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, __n, 0, 1);
  return __r;
}
#define vqshlq_n_s8(...) __c17_vqshlq_n_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vshrq_n_s8(int8x16_t __a, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, -__n, 0, 0);
  return __r;
}
#define vshrq_n_s8(...) __c17_vshrq_n_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vrshrq_n_s8(int8x16_t __a, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 8, -__n, 1, 0);
  return __r;
}
#define vrshrq_n_s8(...) __c17_vrshrq_n_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vsraq_n_s8(int8x16_t __a, int8x16_t __b, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 8, -__n, 0, 0);
  return __r;
}
#define vsraq_n_s8(...) __c17_vsraq_n_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vrsraq_n_s8(int8x16_t __a, int8x16_t __b, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 8, -__n, 1, 0);
  return __r;
}
#define vrsraq_n_s8(...) __c17_vrsraq_n_s8(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vqshluq_n_s8(int8x16_t __a, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i] < 0 ? 0 : __c17_shl_u((uint64_t)__a[__i], 8, __n, 0, 1);
  return __r;
}
#define vqshluq_n_s8(...) __c17_vqshluq_n_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vsliq_n_s8(int8x16_t __a, int8x16_t __b, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = ((uint64_t)(uint8_t)(__b[__i]) << __n) | ((uint64_t)(uint8_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsliq_n_s8(...) __c17_vsliq_n_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t __c17_vsriq_n_s8(int8x16_t __a, int8x16_t __b, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __n >= 8 ? (uint64_t)(uint8_t)(__a[__i]) : ((uint64_t)(uint8_t)(__b[__i]) >> __n) | ((uint64_t)(uint8_t)(__a[__i]) & ~((((uint64_t)1 << 8) - 1) >> __n));
  return __r;
}
#define vsriq_n_s8(...) __c17_vsriq_n_s8(__VA_ARGS__)

__C17_INTRIN int16x4_t vshl_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN int16x4_t vrshl_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN int16x4_t vqshl_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN int16x4_t vqrshl_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN int16x4_t __c17_vshl_n_s16(int16x4_t __a, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, __n, 0, 0);
  return __r;
}
#define vshl_n_s16(...) __c17_vshl_n_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vqshl_n_s16(int16x4_t __a, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, __n, 0, 1);
  return __r;
}
#define vqshl_n_s16(...) __c17_vqshl_n_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vshr_n_s16(int16x4_t __a, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, -__n, 0, 0);
  return __r;
}
#define vshr_n_s16(...) __c17_vshr_n_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vrshr_n_s16(int16x4_t __a, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, -__n, 1, 0);
  return __r;
}
#define vrshr_n_s16(...) __c17_vrshr_n_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vsra_n_s16(int16x4_t __a, int16x4_t __b, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 16, -__n, 0, 0);
  return __r;
}
#define vsra_n_s16(...) __c17_vsra_n_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vrsra_n_s16(int16x4_t __a, int16x4_t __b, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 16, -__n, 1, 0);
  return __r;
}
#define vrsra_n_s16(...) __c17_vrsra_n_s16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vqshlu_n_s16(int16x4_t __a, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] < 0 ? 0 : __c17_shl_u((uint64_t)__a[__i], 16, __n, 0, 1);
  return __r;
}
#define vqshlu_n_s16(...) __c17_vqshlu_n_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vsli_n_s16(int16x4_t __a, int16x4_t __b, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)(uint16_t)(__b[__i]) << __n) | ((uint64_t)(uint16_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsli_n_s16(...) __c17_vsli_n_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t __c17_vsri_n_s16(int16x4_t __a, int16x4_t __b, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __n >= 16 ? (uint64_t)(uint16_t)(__a[__i]) : ((uint64_t)(uint16_t)(__b[__i]) >> __n) | ((uint64_t)(uint16_t)(__a[__i]) & ~((((uint64_t)1 << 16) - 1) >> __n));
  return __r;
}
#define vsri_n_s16(...) __c17_vsri_n_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t vshlq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN int16x8_t vrshlq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN int16x8_t vqshlq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN int16x8_t vqrshlq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN int16x8_t __c17_vshlq_n_s16(int16x8_t __a, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, __n, 0, 0);
  return __r;
}
#define vshlq_n_s16(...) __c17_vshlq_n_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vqshlq_n_s16(int16x8_t __a, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, __n, 0, 1);
  return __r;
}
#define vqshlq_n_s16(...) __c17_vqshlq_n_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vshrq_n_s16(int16x8_t __a, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, -__n, 0, 0);
  return __r;
}
#define vshrq_n_s16(...) __c17_vshrq_n_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vrshrq_n_s16(int16x8_t __a, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 16, -__n, 1, 0);
  return __r;
}
#define vrshrq_n_s16(...) __c17_vrshrq_n_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vsraq_n_s16(int16x8_t __a, int16x8_t __b, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 16, -__n, 0, 0);
  return __r;
}
#define vsraq_n_s16(...) __c17_vsraq_n_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vrsraq_n_s16(int16x8_t __a, int16x8_t __b, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 16, -__n, 1, 0);
  return __r;
}
#define vrsraq_n_s16(...) __c17_vrsraq_n_s16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vqshluq_n_s16(int16x8_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i] < 0 ? 0 : __c17_shl_u((uint64_t)__a[__i], 16, __n, 0, 1);
  return __r;
}
#define vqshluq_n_s16(...) __c17_vqshluq_n_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vsliq_n_s16(int16x8_t __a, int16x8_t __b, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)(uint16_t)(__b[__i]) << __n) | ((uint64_t)(uint16_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsliq_n_s16(...) __c17_vsliq_n_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t __c17_vsriq_n_s16(int16x8_t __a, int16x8_t __b, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __n >= 16 ? (uint64_t)(uint16_t)(__a[__i]) : ((uint64_t)(uint16_t)(__b[__i]) >> __n) | ((uint64_t)(uint16_t)(__a[__i]) & ~((((uint64_t)1 << 16) - 1) >> __n));
  return __r;
}
#define vsriq_n_s16(...) __c17_vsriq_n_s16(__VA_ARGS__)

__C17_INTRIN int32x2_t vshl_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN int32x2_t vrshl_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN int32x2_t vqshl_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN int32x2_t vqrshl_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN int32x2_t __c17_vshl_n_s32(int32x2_t __a, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, __n, 0, 0);
  return __r;
}
#define vshl_n_s32(...) __c17_vshl_n_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vqshl_n_s32(int32x2_t __a, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, __n, 0, 1);
  return __r;
}
#define vqshl_n_s32(...) __c17_vqshl_n_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vshr_n_s32(int32x2_t __a, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, -__n, 0, 0);
  return __r;
}
#define vshr_n_s32(...) __c17_vshr_n_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vrshr_n_s32(int32x2_t __a, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, -__n, 1, 0);
  return __r;
}
#define vrshr_n_s32(...) __c17_vrshr_n_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vsra_n_s32(int32x2_t __a, int32x2_t __b, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 32, -__n, 0, 0);
  return __r;
}
#define vsra_n_s32(...) __c17_vsra_n_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vrsra_n_s32(int32x2_t __a, int32x2_t __b, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 32, -__n, 1, 0);
  return __r;
}
#define vrsra_n_s32(...) __c17_vrsra_n_s32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vqshlu_n_s32(int32x2_t __a, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] < 0 ? 0 : __c17_shl_u((uint64_t)__a[__i], 32, __n, 0, 1);
  return __r;
}
#define vqshlu_n_s32(...) __c17_vqshlu_n_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vsli_n_s32(int32x2_t __a, int32x2_t __b, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((uint64_t)(uint32_t)(__b[__i]) << __n) | ((uint64_t)(uint32_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsli_n_s32(...) __c17_vsli_n_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vsri_n_s32(int32x2_t __a, int32x2_t __b, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __n >= 32 ? (uint64_t)(uint32_t)(__a[__i]) : ((uint64_t)(uint32_t)(__b[__i]) >> __n) | ((uint64_t)(uint32_t)(__a[__i]) & ~((((uint64_t)1 << 32) - 1) >> __n));
  return __r;
}
#define vsri_n_s32(...) __c17_vsri_n_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t vshlq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN int32x4_t vrshlq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN int32x4_t vqshlq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN int32x4_t vqrshlq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vshlq_n_s32(int32x4_t __a, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, __n, 0, 0);
  return __r;
}
#define vshlq_n_s32(...) __c17_vshlq_n_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vqshlq_n_s32(int32x4_t __a, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, __n, 0, 1);
  return __r;
}
#define vqshlq_n_s32(...) __c17_vqshlq_n_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vshrq_n_s32(int32x4_t __a, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, -__n, 0, 0);
  return __r;
}
#define vshrq_n_s32(...) __c17_vshrq_n_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vrshrq_n_s32(int32x4_t __a, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 32, -__n, 1, 0);
  return __r;
}
#define vrshrq_n_s32(...) __c17_vrshrq_n_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vsraq_n_s32(int32x4_t __a, int32x4_t __b, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 32, -__n, 0, 0);
  return __r;
}
#define vsraq_n_s32(...) __c17_vsraq_n_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vrsraq_n_s32(int32x4_t __a, int32x4_t __b, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 32, -__n, 1, 0);
  return __r;
}
#define vrsraq_n_s32(...) __c17_vrsraq_n_s32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vqshluq_n_s32(int32x4_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i] < 0 ? 0 : __c17_shl_u((uint64_t)__a[__i], 32, __n, 0, 1);
  return __r;
}
#define vqshluq_n_s32(...) __c17_vqshluq_n_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vsliq_n_s32(int32x4_t __a, int32x4_t __b, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)(uint32_t)(__b[__i]) << __n) | ((uint64_t)(uint32_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsliq_n_s32(...) __c17_vsliq_n_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vsriq_n_s32(int32x4_t __a, int32x4_t __b, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __n >= 32 ? (uint64_t)(uint32_t)(__a[__i]) : ((uint64_t)(uint32_t)(__b[__i]) >> __n) | ((uint64_t)(uint32_t)(__a[__i]) & ~((((uint64_t)1 << 32) - 1) >> __n));
  return __r;
}
#define vsriq_n_s32(...) __c17_vsriq_n_s32(__VA_ARGS__)

__C17_INTRIN int64x1_t vshl_s64(int64x1_t __a, int64x1_t __b)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN int64x1_t vrshl_s64(int64x1_t __a, int64x1_t __b)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN int64x1_t vqshl_s64(int64x1_t __a, int64x1_t __b)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN int64x1_t vqrshl_s64(int64x1_t __a, int64x1_t __b)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN int64x1_t __c17_vshl_n_s64(int64x1_t __a, const int __n)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, __n, 0, 0);
  return __r;
}
#define vshl_n_s64(...) __c17_vshl_n_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t __c17_vqshl_n_s64(int64x1_t __a, const int __n)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, __n, 0, 1);
  return __r;
}
#define vqshl_n_s64(...) __c17_vqshl_n_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t __c17_vshr_n_s64(int64x1_t __a, const int __n)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, -__n, 0, 0);
  return __r;
}
#define vshr_n_s64(...) __c17_vshr_n_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t __c17_vrshr_n_s64(int64x1_t __a, const int __n)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, -__n, 1, 0);
  return __r;
}
#define vrshr_n_s64(...) __c17_vrshr_n_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t __c17_vsra_n_s64(int64x1_t __a, int64x1_t __b, const int __n)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 64, -__n, 0, 0);
  return __r;
}
#define vsra_n_s64(...) __c17_vsra_n_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t __c17_vrsra_n_s64(int64x1_t __a, int64x1_t __b, const int __n)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 64, -__n, 1, 0);
  return __r;
}
#define vrsra_n_s64(...) __c17_vrsra_n_s64(__VA_ARGS__)

__C17_INTRIN uint64x1_t __c17_vqshlu_n_s64(int64x1_t __a, const int __n)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __a[__i] < 0 ? 0 : __c17_shl_u((uint64_t)__a[__i], 64, __n, 0, 1);
  return __r;
}
#define vqshlu_n_s64(...) __c17_vqshlu_n_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t __c17_vsli_n_s64(int64x1_t __a, int64x1_t __b, const int __n)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = ((uint64_t)(uint64_t)(__b[__i]) << __n) | ((uint64_t)(uint64_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsli_n_s64(...) __c17_vsli_n_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t __c17_vsri_n_s64(int64x1_t __a, int64x1_t __b, const int __n)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __n >= 64 ? (uint64_t)(uint64_t)(__a[__i]) : ((uint64_t)(uint64_t)(__b[__i]) >> __n) | ((uint64_t)(uint64_t)(__a[__i]) & ~(~(uint64_t)0 >> __n));
  return __r;
}
#define vsri_n_s64(...) __c17_vsri_n_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t vshlq_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN int64x2_t vrshlq_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN int64x2_t vqshlq_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN int64x2_t vqrshlq_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN int64x2_t __c17_vshlq_n_s64(int64x2_t __a, const int __n)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, __n, 0, 0);
  return __r;
}
#define vshlq_n_s64(...) __c17_vshlq_n_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vqshlq_n_s64(int64x2_t __a, const int __n)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, __n, 0, 1);
  return __r;
}
#define vqshlq_n_s64(...) __c17_vqshlq_n_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vshrq_n_s64(int64x2_t __a, const int __n)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, -__n, 0, 0);
  return __r;
}
#define vshrq_n_s64(...) __c17_vshrq_n_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vrshrq_n_s64(int64x2_t __a, const int __n)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_s(__a[__i], 64, -__n, 1, 0);
  return __r;
}
#define vrshrq_n_s64(...) __c17_vrshrq_n_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vsraq_n_s64(int64x2_t __a, int64x2_t __b, const int __n)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 64, -__n, 0, 0);
  return __r;
}
#define vsraq_n_s64(...) __c17_vsraq_n_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vrsraq_n_s64(int64x2_t __a, int64x2_t __b, const int __n)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_s(__b[__i], 64, -__n, 1, 0);
  return __r;
}
#define vrsraq_n_s64(...) __c17_vrsraq_n_s64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vqshluq_n_s64(int64x2_t __a, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i] < 0 ? 0 : __c17_shl_u((uint64_t)__a[__i], 64, __n, 0, 1);
  return __r;
}
#define vqshluq_n_s64(...) __c17_vqshluq_n_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vsliq_n_s64(int64x2_t __a, int64x2_t __b, const int __n)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((uint64_t)(uint64_t)(__b[__i]) << __n) | ((uint64_t)(uint64_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsliq_n_s64(...) __c17_vsliq_n_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vsriq_n_s64(int64x2_t __a, int64x2_t __b, const int __n)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __n >= 64 ? (uint64_t)(uint64_t)(__a[__i]) : ((uint64_t)(uint64_t)(__b[__i]) >> __n) | ((uint64_t)(uint64_t)(__a[__i]) & ~(~(uint64_t)0 >> __n));
  return __r;
}
#define vsriq_n_s64(...) __c17_vsriq_n_s64(__VA_ARGS__)

__C17_INTRIN uint8x8_t vshl_u8(uint8x8_t __a, int8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN uint8x8_t vrshl_u8(uint8x8_t __a, int8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN uint8x8_t vqshl_u8(uint8x8_t __a, int8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN uint8x8_t vqrshl_u8(uint8x8_t __a, int8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN uint8x8_t __c17_vshl_n_u8(uint8x8_t __a, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, __n, 0, 0);
  return __r;
}
#define vshl_n_u8(...) __c17_vshl_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vqshl_n_u8(uint8x8_t __a, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, __n, 0, 1);
  return __r;
}
#define vqshl_n_u8(...) __c17_vqshl_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vshr_n_u8(uint8x8_t __a, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, -__n, 0, 0);
  return __r;
}
#define vshr_n_u8(...) __c17_vshr_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vrshr_n_u8(uint8x8_t __a, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, -__n, 1, 0);
  return __r;
}
#define vrshr_n_u8(...) __c17_vrshr_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vsra_n_u8(uint8x8_t __a, uint8x8_t __b, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 8, -__n, 0, 0);
  return __r;
}
#define vsra_n_u8(...) __c17_vsra_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vrsra_n_u8(uint8x8_t __a, uint8x8_t __b, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 8, -__n, 1, 0);
  return __r;
}
#define vrsra_n_u8(...) __c17_vrsra_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vsli_n_u8(uint8x8_t __a, uint8x8_t __b, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)(uint8_t)(__b[__i]) << __n) | ((uint64_t)(uint8_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsli_n_u8(...) __c17_vsli_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t __c17_vsri_n_u8(uint8x8_t __a, uint8x8_t __b, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __n >= 8 ? (uint64_t)(uint8_t)(__a[__i]) : ((uint64_t)(uint8_t)(__b[__i]) >> __n) | ((uint64_t)(uint8_t)(__a[__i]) & ~((((uint64_t)1 << 8) - 1) >> __n));
  return __r;
}
#define vsri_n_u8(...) __c17_vsri_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t vshlq_u8(uint8x16_t __a, int8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN uint8x16_t vrshlq_u8(uint8x16_t __a, int8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN uint8x16_t vqshlq_u8(uint8x16_t __a, int8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN uint8x16_t vqrshlq_u8(uint8x16_t __a, int8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN uint8x16_t __c17_vshlq_n_u8(uint8x16_t __a, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, __n, 0, 0);
  return __r;
}
#define vshlq_n_u8(...) __c17_vshlq_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vqshlq_n_u8(uint8x16_t __a, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, __n, 0, 1);
  return __r;
}
#define vqshlq_n_u8(...) __c17_vqshlq_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vshrq_n_u8(uint8x16_t __a, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, -__n, 0, 0);
  return __r;
}
#define vshrq_n_u8(...) __c17_vshrq_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vrshrq_n_u8(uint8x16_t __a, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 8, -__n, 1, 0);
  return __r;
}
#define vrshrq_n_u8(...) __c17_vrshrq_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vsraq_n_u8(uint8x16_t __a, uint8x16_t __b, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 8, -__n, 0, 0);
  return __r;
}
#define vsraq_n_u8(...) __c17_vsraq_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vrsraq_n_u8(uint8x16_t __a, uint8x16_t __b, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 8, -__n, 1, 0);
  return __r;
}
#define vrsraq_n_u8(...) __c17_vrsraq_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vsliq_n_u8(uint8x16_t __a, uint8x16_t __b, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = ((uint64_t)(uint8_t)(__b[__i]) << __n) | ((uint64_t)(uint8_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsliq_n_u8(...) __c17_vsliq_n_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t __c17_vsriq_n_u8(uint8x16_t __a, uint8x16_t __b, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __n >= 8 ? (uint64_t)(uint8_t)(__a[__i]) : ((uint64_t)(uint8_t)(__b[__i]) >> __n) | ((uint64_t)(uint8_t)(__a[__i]) & ~((((uint64_t)1 << 8) - 1) >> __n));
  return __r;
}
#define vsriq_n_u8(...) __c17_vsriq_n_u8(__VA_ARGS__)

__C17_INTRIN uint16x4_t vshl_u16(uint16x4_t __a, int16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN uint16x4_t vrshl_u16(uint16x4_t __a, int16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN uint16x4_t vqshl_u16(uint16x4_t __a, int16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN uint16x4_t vqrshl_u16(uint16x4_t __a, int16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN uint16x4_t __c17_vshl_n_u16(uint16x4_t __a, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, __n, 0, 0);
  return __r;
}
#define vshl_n_u16(...) __c17_vshl_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vqshl_n_u16(uint16x4_t __a, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, __n, 0, 1);
  return __r;
}
#define vqshl_n_u16(...) __c17_vqshl_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vshr_n_u16(uint16x4_t __a, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, -__n, 0, 0);
  return __r;
}
#define vshr_n_u16(...) __c17_vshr_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vrshr_n_u16(uint16x4_t __a, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, -__n, 1, 0);
  return __r;
}
#define vrshr_n_u16(...) __c17_vrshr_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vsra_n_u16(uint16x4_t __a, uint16x4_t __b, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 16, -__n, 0, 0);
  return __r;
}
#define vsra_n_u16(...) __c17_vsra_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vrsra_n_u16(uint16x4_t __a, uint16x4_t __b, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 16, -__n, 1, 0);
  return __r;
}
#define vrsra_n_u16(...) __c17_vrsra_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vsli_n_u16(uint16x4_t __a, uint16x4_t __b, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)(uint16_t)(__b[__i]) << __n) | ((uint64_t)(uint16_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsli_n_u16(...) __c17_vsli_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t __c17_vsri_n_u16(uint16x4_t __a, uint16x4_t __b, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __n >= 16 ? (uint64_t)(uint16_t)(__a[__i]) : ((uint64_t)(uint16_t)(__b[__i]) >> __n) | ((uint64_t)(uint16_t)(__a[__i]) & ~((((uint64_t)1 << 16) - 1) >> __n));
  return __r;
}
#define vsri_n_u16(...) __c17_vsri_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t vshlq_u16(uint16x8_t __a, int16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN uint16x8_t vrshlq_u16(uint16x8_t __a, int16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN uint16x8_t vqshlq_u16(uint16x8_t __a, int16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN uint16x8_t vqrshlq_u16(uint16x8_t __a, int16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN uint16x8_t __c17_vshlq_n_u16(uint16x8_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, __n, 0, 0);
  return __r;
}
#define vshlq_n_u16(...) __c17_vshlq_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vqshlq_n_u16(uint16x8_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, __n, 0, 1);
  return __r;
}
#define vqshlq_n_u16(...) __c17_vqshlq_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vshrq_n_u16(uint16x8_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, -__n, 0, 0);
  return __r;
}
#define vshrq_n_u16(...) __c17_vshrq_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vrshrq_n_u16(uint16x8_t __a, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 16, -__n, 1, 0);
  return __r;
}
#define vrshrq_n_u16(...) __c17_vrshrq_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vsraq_n_u16(uint16x8_t __a, uint16x8_t __b, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 16, -__n, 0, 0);
  return __r;
}
#define vsraq_n_u16(...) __c17_vsraq_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vrsraq_n_u16(uint16x8_t __a, uint16x8_t __b, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 16, -__n, 1, 0);
  return __r;
}
#define vrsraq_n_u16(...) __c17_vrsraq_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vsliq_n_u16(uint16x8_t __a, uint16x8_t __b, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)(uint16_t)(__b[__i]) << __n) | ((uint64_t)(uint16_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsliq_n_u16(...) __c17_vsliq_n_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t __c17_vsriq_n_u16(uint16x8_t __a, uint16x8_t __b, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __n >= 16 ? (uint64_t)(uint16_t)(__a[__i]) : ((uint64_t)(uint16_t)(__b[__i]) >> __n) | ((uint64_t)(uint16_t)(__a[__i]) & ~((((uint64_t)1 << 16) - 1) >> __n));
  return __r;
}
#define vsriq_n_u16(...) __c17_vsriq_n_u16(__VA_ARGS__)

__C17_INTRIN uint32x2_t vshl_u32(uint32x2_t __a, int32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN uint32x2_t vrshl_u32(uint32x2_t __a, int32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN uint32x2_t vqshl_u32(uint32x2_t __a, int32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN uint32x2_t vqrshl_u32(uint32x2_t __a, int32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN uint32x2_t __c17_vshl_n_u32(uint32x2_t __a, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, __n, 0, 0);
  return __r;
}
#define vshl_n_u32(...) __c17_vshl_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vqshl_n_u32(uint32x2_t __a, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, __n, 0, 1);
  return __r;
}
#define vqshl_n_u32(...) __c17_vqshl_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vshr_n_u32(uint32x2_t __a, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, -__n, 0, 0);
  return __r;
}
#define vshr_n_u32(...) __c17_vshr_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vrshr_n_u32(uint32x2_t __a, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, -__n, 1, 0);
  return __r;
}
#define vrshr_n_u32(...) __c17_vrshr_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vsra_n_u32(uint32x2_t __a, uint32x2_t __b, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 32, -__n, 0, 0);
  return __r;
}
#define vsra_n_u32(...) __c17_vsra_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vrsra_n_u32(uint32x2_t __a, uint32x2_t __b, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 32, -__n, 1, 0);
  return __r;
}
#define vrsra_n_u32(...) __c17_vrsra_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vsli_n_u32(uint32x2_t __a, uint32x2_t __b, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((uint64_t)(uint32_t)(__b[__i]) << __n) | ((uint64_t)(uint32_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsli_n_u32(...) __c17_vsli_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vsri_n_u32(uint32x2_t __a, uint32x2_t __b, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __n >= 32 ? (uint64_t)(uint32_t)(__a[__i]) : ((uint64_t)(uint32_t)(__b[__i]) >> __n) | ((uint64_t)(uint32_t)(__a[__i]) & ~((((uint64_t)1 << 32) - 1) >> __n));
  return __r;
}
#define vsri_n_u32(...) __c17_vsri_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t vshlq_u32(uint32x4_t __a, int32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN uint32x4_t vrshlq_u32(uint32x4_t __a, int32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN uint32x4_t vqshlq_u32(uint32x4_t __a, int32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN uint32x4_t vqrshlq_u32(uint32x4_t __a, int32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN uint32x4_t __c17_vshlq_n_u32(uint32x4_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, __n, 0, 0);
  return __r;
}
#define vshlq_n_u32(...) __c17_vshlq_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vqshlq_n_u32(uint32x4_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, __n, 0, 1);
  return __r;
}
#define vqshlq_n_u32(...) __c17_vqshlq_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vshrq_n_u32(uint32x4_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, -__n, 0, 0);
  return __r;
}
#define vshrq_n_u32(...) __c17_vshrq_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vrshrq_n_u32(uint32x4_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 32, -__n, 1, 0);
  return __r;
}
#define vrshrq_n_u32(...) __c17_vrshrq_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vsraq_n_u32(uint32x4_t __a, uint32x4_t __b, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 32, -__n, 0, 0);
  return __r;
}
#define vsraq_n_u32(...) __c17_vsraq_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vrsraq_n_u32(uint32x4_t __a, uint32x4_t __b, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 32, -__n, 1, 0);
  return __r;
}
#define vrsraq_n_u32(...) __c17_vrsraq_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vsliq_n_u32(uint32x4_t __a, uint32x4_t __b, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)(uint32_t)(__b[__i]) << __n) | ((uint64_t)(uint32_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsliq_n_u32(...) __c17_vsliq_n_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vsriq_n_u32(uint32x4_t __a, uint32x4_t __b, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __n >= 32 ? (uint64_t)(uint32_t)(__a[__i]) : ((uint64_t)(uint32_t)(__b[__i]) >> __n) | ((uint64_t)(uint32_t)(__a[__i]) & ~((((uint64_t)1 << 32) - 1) >> __n));
  return __r;
}
#define vsriq_n_u32(...) __c17_vsriq_n_u32(__VA_ARGS__)

__C17_INTRIN uint64x1_t vshl_u64(uint64x1_t __a, int64x1_t __b)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN uint64x1_t vrshl_u64(uint64x1_t __a, int64x1_t __b)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN uint64x1_t vqshl_u64(uint64x1_t __a, int64x1_t __b)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN uint64x1_t vqrshl_u64(uint64x1_t __a, int64x1_t __b)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN uint64x1_t __c17_vshl_n_u64(uint64x1_t __a, const int __n)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, __n, 0, 0);
  return __r;
}
#define vshl_n_u64(...) __c17_vshl_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t __c17_vqshl_n_u64(uint64x1_t __a, const int __n)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, __n, 0, 1);
  return __r;
}
#define vqshl_n_u64(...) __c17_vqshl_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t __c17_vshr_n_u64(uint64x1_t __a, const int __n)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, -__n, 0, 0);
  return __r;
}
#define vshr_n_u64(...) __c17_vshr_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t __c17_vrshr_n_u64(uint64x1_t __a, const int __n)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, -__n, 1, 0);
  return __r;
}
#define vrshr_n_u64(...) __c17_vrshr_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t __c17_vsra_n_u64(uint64x1_t __a, uint64x1_t __b, const int __n)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 64, -__n, 0, 0);
  return __r;
}
#define vsra_n_u64(...) __c17_vsra_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t __c17_vrsra_n_u64(uint64x1_t __a, uint64x1_t __b, const int __n)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 64, -__n, 1, 0);
  return __r;
}
#define vrsra_n_u64(...) __c17_vrsra_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t __c17_vsli_n_u64(uint64x1_t __a, uint64x1_t __b, const int __n)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = ((uint64_t)(uint64_t)(__b[__i]) << __n) | ((uint64_t)(uint64_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsli_n_u64(...) __c17_vsli_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t __c17_vsri_n_u64(uint64x1_t __a, uint64x1_t __b, const int __n)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __n >= 64 ? (uint64_t)(uint64_t)(__a[__i]) : ((uint64_t)(uint64_t)(__b[__i]) >> __n) | ((uint64_t)(uint64_t)(__a[__i]) & ~(~(uint64_t)0 >> __n));
  return __r;
}
#define vsri_n_u64(...) __c17_vsri_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t vshlq_u64(uint64x2_t __a, int64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, (int)(int8_t)__b[__i], 0, 0);
  return __r;
}

__C17_INTRIN uint64x2_t vrshlq_u64(uint64x2_t __a, int64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, (int)(int8_t)__b[__i], 1, 0);
  return __r;
}

__C17_INTRIN uint64x2_t vqshlq_u64(uint64x2_t __a, int64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, (int)(int8_t)__b[__i], 0, 1);
  return __r;
}

__C17_INTRIN uint64x2_t vqrshlq_u64(uint64x2_t __a, int64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, (int)(int8_t)__b[__i], 1, 1);
  return __r;
}

__C17_INTRIN uint64x2_t __c17_vshlq_n_u64(uint64x2_t __a, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, __n, 0, 0);
  return __r;
}
#define vshlq_n_u64(...) __c17_vshlq_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vqshlq_n_u64(uint64x2_t __a, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, __n, 0, 1);
  return __r;
}
#define vqshlq_n_u64(...) __c17_vqshlq_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vshrq_n_u64(uint64x2_t __a, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, -__n, 0, 0);
  return __r;
}
#define vshrq_n_u64(...) __c17_vshrq_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vrshrq_n_u64(uint64x2_t __a, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_shl_u(__a[__i], 64, -__n, 1, 0);
  return __r;
}
#define vrshrq_n_u64(...) __c17_vrshrq_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vsraq_n_u64(uint64x2_t __a, uint64x2_t __b, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 64, -__n, 0, 0);
  return __r;
}
#define vsraq_n_u64(...) __c17_vsraq_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vrsraq_n_u64(uint64x2_t __a, uint64x2_t __b, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__a[__i] + (uint64_t)__c17_shl_u(__b[__i], 64, -__n, 1, 0);
  return __r;
}
#define vrsraq_n_u64(...) __c17_vrsraq_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vsliq_n_u64(uint64x2_t __a, uint64x2_t __b, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = ((uint64_t)(uint64_t)(__b[__i]) << __n) | ((uint64_t)(uint64_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsliq_n_u64(...) __c17_vsliq_n_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vsriq_n_u64(uint64x2_t __a, uint64x2_t __b, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __n >= 64 ? (uint64_t)(uint64_t)(__a[__i]) : ((uint64_t)(uint64_t)(__b[__i]) >> __n) | ((uint64_t)(uint64_t)(__a[__i]) & ~(~(uint64_t)0 >> __n));
  return __r;
}
#define vsriq_n_u64(...) __c17_vsriq_n_u64(__VA_ARGS__)

__C17_INTRIN poly8x8_t __c17_vsli_n_p8(poly8x8_t __a, poly8x8_t __b, const int __n)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)(uint8_t)(__b[__i]) << __n) | ((uint64_t)(uint8_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsli_n_p8(...) __c17_vsli_n_p8(__VA_ARGS__)

__C17_INTRIN poly8x8_t __c17_vsri_n_p8(poly8x8_t __a, poly8x8_t __b, const int __n)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __n >= 8 ? (uint64_t)(uint8_t)(__a[__i]) : ((uint64_t)(uint8_t)(__b[__i]) >> __n) | ((uint64_t)(uint8_t)(__a[__i]) & ~((((uint64_t)1 << 8) - 1) >> __n));
  return __r;
}
#define vsri_n_p8(...) __c17_vsri_n_p8(__VA_ARGS__)

__C17_INTRIN poly8x16_t __c17_vsliq_n_p8(poly8x16_t __a, poly8x16_t __b, const int __n)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = ((uint64_t)(uint8_t)(__b[__i]) << __n) | ((uint64_t)(uint8_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsliq_n_p8(...) __c17_vsliq_n_p8(__VA_ARGS__)

__C17_INTRIN poly8x16_t __c17_vsriq_n_p8(poly8x16_t __a, poly8x16_t __b, const int __n)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __n >= 8 ? (uint64_t)(uint8_t)(__a[__i]) : ((uint64_t)(uint8_t)(__b[__i]) >> __n) | ((uint64_t)(uint8_t)(__a[__i]) & ~((((uint64_t)1 << 8) - 1) >> __n));
  return __r;
}
#define vsriq_n_p8(...) __c17_vsriq_n_p8(__VA_ARGS__)

__C17_INTRIN poly16x4_t __c17_vsli_n_p16(poly16x4_t __a, poly16x4_t __b, const int __n)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = ((uint64_t)(uint16_t)(__b[__i]) << __n) | ((uint64_t)(uint16_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsli_n_p16(...) __c17_vsli_n_p16(__VA_ARGS__)

__C17_INTRIN poly16x4_t __c17_vsri_n_p16(poly16x4_t __a, poly16x4_t __b, const int __n)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __n >= 16 ? (uint64_t)(uint16_t)(__a[__i]) : ((uint64_t)(uint16_t)(__b[__i]) >> __n) | ((uint64_t)(uint16_t)(__a[__i]) & ~((((uint64_t)1 << 16) - 1) >> __n));
  return __r;
}
#define vsri_n_p16(...) __c17_vsri_n_p16(__VA_ARGS__)

__C17_INTRIN poly16x8_t __c17_vsliq_n_p16(poly16x8_t __a, poly16x8_t __b, const int __n)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = ((uint64_t)(uint16_t)(__b[__i]) << __n) | ((uint64_t)(uint16_t)(__a[__i]) & (((uint64_t)1 << __n) - 1));
  return __r;
}
#define vsliq_n_p16(...) __c17_vsliq_n_p16(__VA_ARGS__)

__C17_INTRIN poly16x8_t __c17_vsriq_n_p16(poly16x8_t __a, poly16x8_t __b, const int __n)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __n >= 16 ? (uint64_t)(uint16_t)(__a[__i]) : ((uint64_t)(uint16_t)(__b[__i]) >> __n) | ((uint64_t)(uint16_t)(__a[__i]) & ~((((uint64_t)1 << 16) - 1) >> __n));
  return __r;
}
#define vsriq_n_p16(...) __c17_vsriq_n_p16(__VA_ARGS__)


/* Conversions and estimates. */

__C17_INTRIN float32x2_t vcvt_f32_s32(int32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (float32_t)__a[__i];
  return __r;
}

__C17_INTRIN float32x2_t __c17_vcvt_n_f32_s32(int32x2_t __a, const int __n)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (float32_t)__a[__i] * __c17_exp2_f32(-__n);
  return __r;
}
#define vcvt_n_f32_s32(...) __c17_vcvt_n_f32_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t vcvt_s32_f32(float32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_s(__a[__i], 32);
  return __r;
}

__C17_INTRIN int32x2_t vcvtn_s32_f32(float32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_s(__c17_rndn_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN int32x2_t vcvta_s32_f32(float32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_s(__c17_rnda_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN int32x2_t vcvtm_s32_f32(float32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_s(__c17_rndm_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN int32x2_t vcvtp_s32_f32(float32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_s(__c17_rndp_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN int32x2_t __c17_vcvt_n_s32_f32(float32x2_t __a, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_s((double)__a[__i] * __c17_exp2_f64(__n), 32);
  return __r;
}
#define vcvt_n_s32_f32(...) __c17_vcvt_n_s32_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t vcvt_f32_u32(uint32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (float32_t)__a[__i];
  return __r;
}

__C17_INTRIN float32x2_t __c17_vcvt_n_f32_u32(uint32x2_t __a, const int __n)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (float32_t)__a[__i] * __c17_exp2_f32(-__n);
  return __r;
}
#define vcvt_n_f32_u32(...) __c17_vcvt_n_f32_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t vcvt_u32_f32(float32x2_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_u(__a[__i], 32);
  return __r;
}

__C17_INTRIN uint32x2_t vcvtn_u32_f32(float32x2_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_u(__c17_rndn_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN uint32x2_t vcvta_u32_f32(float32x2_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_u(__c17_rnda_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN uint32x2_t vcvtm_u32_f32(float32x2_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_u(__c17_rndm_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN uint32x2_t vcvtp_u32_f32(float32x2_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_u(__c17_rndp_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN uint32x2_t __c17_vcvt_n_u32_f32(float32x2_t __a, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_u((double)__a[__i] * __c17_exp2_f64(__n), 32);
  return __r;
}
#define vcvt_n_u32_f32(...) __c17_vcvt_n_u32_f32(__VA_ARGS__)

__C17_INTRIN float64x1_t vcvt_f64_s64(int64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = (float64_t)__a[__i];
  return __r;
}

__C17_INTRIN float64x1_t __c17_vcvt_n_f64_s64(int64x1_t __a, const int __n)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = (float64_t)__a[__i] * __c17_exp2_f64(-__n);
  return __r;
}
#define vcvt_n_f64_s64(...) __c17_vcvt_n_f64_s64(__VA_ARGS__)

__C17_INTRIN int64x1_t vcvt_s64_f64(float64x1_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_cvt_s(__a[__i], 64);
  return __r;
}

__C17_INTRIN int64x1_t vcvtn_s64_f64(float64x1_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_cvt_s(__c17_rndn_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN int64x1_t vcvta_s64_f64(float64x1_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_cvt_s(__c17_rnda_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN int64x1_t vcvtm_s64_f64(float64x1_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_cvt_s(__c17_rndm_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN int64x1_t vcvtp_s64_f64(float64x1_t __a)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_cvt_s(__c17_rndp_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN int64x1_t __c17_vcvt_n_s64_f64(float64x1_t __a, const int __n)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_cvt_s((double)__a[__i] * __c17_exp2_f64(__n), 64);
  return __r;
}
#define vcvt_n_s64_f64(...) __c17_vcvt_n_s64_f64(__VA_ARGS__)

__C17_INTRIN float64x1_t vcvt_f64_u64(uint64x1_t __a)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = (float64_t)__a[__i];
  return __r;
}

__C17_INTRIN float64x1_t __c17_vcvt_n_f64_u64(uint64x1_t __a, const int __n)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = (float64_t)__a[__i] * __c17_exp2_f64(-__n);
  return __r;
}
#define vcvt_n_f64_u64(...) __c17_vcvt_n_f64_u64(__VA_ARGS__)

__C17_INTRIN uint64x1_t vcvt_u64_f64(float64x1_t __a)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_cvt_u(__a[__i], 64);
  return __r;
}

__C17_INTRIN uint64x1_t vcvtn_u64_f64(float64x1_t __a)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_cvt_u(__c17_rndn_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN uint64x1_t vcvta_u64_f64(float64x1_t __a)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_cvt_u(__c17_rnda_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN uint64x1_t vcvtm_u64_f64(float64x1_t __a)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_cvt_u(__c17_rndm_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN uint64x1_t vcvtp_u64_f64(float64x1_t __a)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_cvt_u(__c17_rndp_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN uint64x1_t __c17_vcvt_n_u64_f64(float64x1_t __a, const int __n)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __c17_cvt_u((double)__a[__i] * __c17_exp2_f64(__n), 64);
  return __r;
}
#define vcvt_n_u64_f64(...) __c17_vcvt_n_u64_f64(__VA_ARGS__)

__C17_INTRIN float32x4_t vcvtq_f32_s32(int32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (float32_t)__a[__i];
  return __r;
}

__C17_INTRIN float32x4_t __c17_vcvtq_n_f32_s32(int32x4_t __a, const int __n)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (float32_t)__a[__i] * __c17_exp2_f32(-__n);
  return __r;
}
#define vcvtq_n_f32_s32(...) __c17_vcvtq_n_f32_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t vcvtq_s32_f32(float32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_cvt_s(__a[__i], 32);
  return __r;
}

__C17_INTRIN int32x4_t vcvtnq_s32_f32(float32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_cvt_s(__c17_rndn_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN int32x4_t vcvtaq_s32_f32(float32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_cvt_s(__c17_rnda_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN int32x4_t vcvtmq_s32_f32(float32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_cvt_s(__c17_rndm_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN int32x4_t vcvtpq_s32_f32(float32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_cvt_s(__c17_rndp_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vcvtq_n_s32_f32(float32x4_t __a, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_cvt_s((double)__a[__i] * __c17_exp2_f64(__n), 32);
  return __r;
}
#define vcvtq_n_s32_f32(...) __c17_vcvtq_n_s32_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t vcvtq_f32_u32(uint32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (float32_t)__a[__i];
  return __r;
}

__C17_INTRIN float32x4_t __c17_vcvtq_n_f32_u32(uint32x4_t __a, const int __n)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (float32_t)__a[__i] * __c17_exp2_f32(-__n);
  return __r;
}
#define vcvtq_n_f32_u32(...) __c17_vcvtq_n_f32_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t vcvtq_u32_f32(float32x4_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_cvt_u(__a[__i], 32);
  return __r;
}

__C17_INTRIN uint32x4_t vcvtnq_u32_f32(float32x4_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_cvt_u(__c17_rndn_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN uint32x4_t vcvtaq_u32_f32(float32x4_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_cvt_u(__c17_rnda_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN uint32x4_t vcvtmq_u32_f32(float32x4_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_cvt_u(__c17_rndm_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN uint32x4_t vcvtpq_u32_f32(float32x4_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_cvt_u(__c17_rndp_f32(__a[__i]), 32);
  return __r;
}

__C17_INTRIN uint32x4_t __c17_vcvtq_n_u32_f32(float32x4_t __a, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_cvt_u((double)__a[__i] * __c17_exp2_f64(__n), 32);
  return __r;
}
#define vcvtq_n_u32_f32(...) __c17_vcvtq_n_u32_f32(__VA_ARGS__)

__C17_INTRIN float64x2_t vcvtq_f64_s64(int64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (float64_t)__a[__i];
  return __r;
}

__C17_INTRIN float64x2_t __c17_vcvtq_n_f64_s64(int64x2_t __a, const int __n)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (float64_t)__a[__i] * __c17_exp2_f64(-__n);
  return __r;
}
#define vcvtq_n_f64_s64(...) __c17_vcvtq_n_f64_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t vcvtq_s64_f64(float64x2_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_s(__a[__i], 64);
  return __r;
}

__C17_INTRIN int64x2_t vcvtnq_s64_f64(float64x2_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_s(__c17_rndn_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN int64x2_t vcvtaq_s64_f64(float64x2_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_s(__c17_rnda_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN int64x2_t vcvtmq_s64_f64(float64x2_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_s(__c17_rndm_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN int64x2_t vcvtpq_s64_f64(float64x2_t __a)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_s(__c17_rndp_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN int64x2_t __c17_vcvtq_n_s64_f64(float64x2_t __a, const int __n)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_s((double)__a[__i] * __c17_exp2_f64(__n), 64);
  return __r;
}
#define vcvtq_n_s64_f64(...) __c17_vcvtq_n_s64_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t vcvtq_f64_u64(uint64x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (float64_t)__a[__i];
  return __r;
}

__C17_INTRIN float64x2_t __c17_vcvtq_n_f64_u64(uint64x2_t __a, const int __n)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (float64_t)__a[__i] * __c17_exp2_f64(-__n);
  return __r;
}
#define vcvtq_n_f64_u64(...) __c17_vcvtq_n_f64_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t vcvtq_u64_f64(float64x2_t __a)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_u(__a[__i], 64);
  return __r;
}

__C17_INTRIN uint64x2_t vcvtnq_u64_f64(float64x2_t __a)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_u(__c17_rndn_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN uint64x2_t vcvtaq_u64_f64(float64x2_t __a)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_u(__c17_rnda_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN uint64x2_t vcvtmq_u64_f64(float64x2_t __a)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_u(__c17_rndm_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN uint64x2_t vcvtpq_u64_f64(float64x2_t __a)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_u(__c17_rndp_f64(__a[__i]), 64);
  return __r;
}

__C17_INTRIN uint64x2_t __c17_vcvtq_n_u64_f64(float64x2_t __a, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvt_u((double)__a[__i] * __c17_exp2_f64(__n), 64);
  return __r;
}
#define vcvtq_n_u64_f64(...) __c17_vcvtq_n_u64_f64(__VA_ARGS__)

__C17_INTRIN float32x2_t vcvt_f32_f64(float64x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (float)__a[__i];
  return __r;
}

__C17_INTRIN float32x4_t vcvt_high_f32_f64(float32x2_t __r0, float64x2_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __r0[__i] : (float)__a[__i - 2];
  return __r;
}

__C17_INTRIN float64x2_t vcvt_f64_f32(float32x2_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (double)__a[__i];
  return __r;
}

__C17_INTRIN float64x2_t vcvt_high_f64_f32(float32x4_t __a)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (double)__a[__i + 2];
  return __r;
}

__C17_INTRIN float32x2_t vcvtx_f32_f64(float64x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_cvtx_f32(__a[__i]);
  return __r;
}

__C17_INTRIN float32x4_t vcvtx_high_f32_f64(float32x2_t __r0, float64x2_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __r0[__i] : __c17_cvtx_f32(__a[__i - 2]);
  return __r;
}

__C17_INTRIN uint32x2_t vrecpe_u32(uint32x2_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_urecpe(__a[__i]);
  return __r;
}

__C17_INTRIN uint32x2_t vrsqrte_u32(uint32x2_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_ursqrte(__a[__i]);
  return __r;
}

__C17_INTRIN uint32x4_t vrecpeq_u32(uint32x4_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_urecpe(__a[__i]);
  return __r;
}

__C17_INTRIN uint32x4_t vrsqrteq_u32(uint32x4_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_ursqrte(__a[__i]);
  return __r;
}


/* Permutations and table lookups. */

__C17_INTRIN int8x8_t __c17_vext_s8(int8x8_t __a, int8x8_t __b, const int __n)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i + __n < 8 ? __a[__i + __n] : __b[__i + __n - 8];
  return __r;
}
#define vext_s8(...) __c17_vext_s8(__VA_ARGS__)

__C17_INTRIN int8x8_t vrev16_s8(int8x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN int8x8_t vrev32_s8(int8x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 3];
  return __r;
}

__C17_INTRIN int8x8_t vrev64_s8(int8x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 7];
  return __r;
}

__C17_INTRIN int8x8_t vzip1_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN int8x8_t vzip2_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 4] : __a[(__i >> 1) + 4];
  return __r;
}

__C17_INTRIN int8x8_t vuzp1_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[2 * __i] : __b[2 * __i - 8];
  return __r;
}

__C17_INTRIN int8x8_t vuzp2_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 8];
  return __r;
}

__C17_INTRIN int8x8_t vtrn1_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN int8x8_t vtrn2_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN int8x8x2_t vzip_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 4] : __a[(__i >> 1) + 4];
  }
  return __r;
}

__C17_INTRIN int8x8x2_t vuzp_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __i < 4 ? __a[2 * __i] : __b[2 * __i - 8];
    __r.val[1][__i] = __i < 4 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 8];
  }
  return __r;
}

__C17_INTRIN int8x8x2_t vtrn_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN int8x16_t __c17_vextq_s8(int8x16_t __a, int8x16_t __b, const int __n)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i + __n < 16 ? __a[__i + __n] : __b[__i + __n - 16];
  return __r;
}
#define vextq_s8(...) __c17_vextq_s8(__VA_ARGS__)

__C17_INTRIN int8x16_t vrev16q_s8(int8x16_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN int8x16_t vrev32q_s8(int8x16_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i ^ 3];
  return __r;
}

__C17_INTRIN int8x16_t vrev64q_s8(int8x16_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i ^ 7];
  return __r;
}

__C17_INTRIN int8x16_t vzip1q_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN int8x16_t vzip2q_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 8] : __a[(__i >> 1) + 8];
  return __r;
}

__C17_INTRIN int8x16_t vuzp1q_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? __a[2 * __i] : __b[2 * __i - 16];
  return __r;
}

__C17_INTRIN int8x16_t vuzp2q_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 16];
  return __r;
}

__C17_INTRIN int8x16_t vtrn1q_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN int8x16_t vtrn2q_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN int8x16x2_t vzipq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 8] : __a[(__i >> 1) + 8];
  }
  return __r;
}

__C17_INTRIN int8x16x2_t vuzpq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __i < 8 ? __a[2 * __i] : __b[2 * __i - 16];
    __r.val[1][__i] = __i < 8 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 16];
  }
  return __r;
}

__C17_INTRIN int8x16x2_t vtrnq_s8(int8x16_t __a, int8x16_t __b)
{
  int8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN int16x4_t __c17_vext_s16(int16x4_t __a, int16x4_t __b, const int __n)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i + __n < 4 ? __a[__i + __n] : __b[__i + __n - 4];
  return __r;
}
#define vext_s16(...) __c17_vext_s16(__VA_ARGS__)

__C17_INTRIN int16x4_t vrev32_s16(int16x4_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN int16x4_t vrev64_s16(int16x4_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i ^ 3];
  return __r;
}

__C17_INTRIN int16x4_t vzip1_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN int16x4_t vzip2_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 2] : __a[(__i >> 1) + 2];
  return __r;
}

__C17_INTRIN int16x4_t vuzp1_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i] : __b[2 * __i - 4];
  return __r;
}

__C17_INTRIN int16x4_t vuzp2_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 4];
  return __r;
}

__C17_INTRIN int16x4_t vtrn1_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN int16x4_t vtrn2_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN int16x4x2_t vzip_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 2] : __a[(__i >> 1) + 2];
  }
  return __r;
}

__C17_INTRIN int16x4x2_t vuzp_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __i < 2 ? __a[2 * __i] : __b[2 * __i - 4];
    __r.val[1][__i] = __i < 2 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 4];
  }
  return __r;
}

__C17_INTRIN int16x4x2_t vtrn_s16(int16x4_t __a, int16x4_t __b)
{
  int16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN int16x8_t __c17_vextq_s16(int16x8_t __a, int16x8_t __b, const int __n)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i + __n < 8 ? __a[__i + __n] : __b[__i + __n - 8];
  return __r;
}
#define vextq_s16(...) __c17_vextq_s16(__VA_ARGS__)

__C17_INTRIN int16x8_t vrev32q_s16(int16x8_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN int16x8_t vrev64q_s16(int16x8_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 3];
  return __r;
}

__C17_INTRIN int16x8_t vzip1q_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN int16x8_t vzip2q_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 4] : __a[(__i >> 1) + 4];
  return __r;
}

__C17_INTRIN int16x8_t vuzp1q_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[2 * __i] : __b[2 * __i - 8];
  return __r;
}

__C17_INTRIN int16x8_t vuzp2q_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 8];
  return __r;
}

__C17_INTRIN int16x8_t vtrn1q_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN int16x8_t vtrn2q_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN int16x8x2_t vzipq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 4] : __a[(__i >> 1) + 4];
  }
  return __r;
}

__C17_INTRIN int16x8x2_t vuzpq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __i < 4 ? __a[2 * __i] : __b[2 * __i - 8];
    __r.val[1][__i] = __i < 4 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 8];
  }
  return __r;
}

__C17_INTRIN int16x8x2_t vtrnq_s16(int16x8_t __a, int16x8_t __b)
{
  int16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN int32x2_t __c17_vext_s32(int32x2_t __a, int32x2_t __b, const int __n)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i + __n < 2 ? __a[__i + __n] : __b[__i + __n - 2];
  return __r;
}
#define vext_s32(...) __c17_vext_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t vrev64_s32(int32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN int32x2_t vzip1_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN int32x2_t vzip2_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 1] : __a[(__i >> 1) + 1];
  return __r;
}

__C17_INTRIN int32x2_t vuzp1_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i] : __b[2 * __i - 2];
  return __r;
}

__C17_INTRIN int32x2_t vuzp2_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 2];
  return __r;
}

__C17_INTRIN int32x2_t vtrn1_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN int32x2_t vtrn2_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN int32x2x2_t vzip_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 1] : __a[(__i >> 1) + 1];
  }
  return __r;
}

__C17_INTRIN int32x2x2_t vuzp_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __i < 1 ? __a[2 * __i] : __b[2 * __i - 2];
    __r.val[1][__i] = __i < 1 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 2];
  }
  return __r;
}

__C17_INTRIN int32x2x2_t vtrn_s32(int32x2_t __a, int32x2_t __b)
{
  int32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN int32x4_t __c17_vextq_s32(int32x4_t __a, int32x4_t __b, const int __n)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i + __n < 4 ? __a[__i + __n] : __b[__i + __n - 4];
  return __r;
}
#define vextq_s32(...) __c17_vextq_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t vrev64q_s32(int32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN int32x4_t vzip1q_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN int32x4_t vzip2q_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 2] : __a[(__i >> 1) + 2];
  return __r;
}

__C17_INTRIN int32x4_t vuzp1q_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i] : __b[2 * __i - 4];
  return __r;
}

__C17_INTRIN int32x4_t vuzp2q_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 4];
  return __r;
}

__C17_INTRIN int32x4_t vtrn1q_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN int32x4_t vtrn2q_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN int32x4x2_t vzipq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 2] : __a[(__i >> 1) + 2];
  }
  return __r;
}

__C17_INTRIN int32x4x2_t vuzpq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __i < 2 ? __a[2 * __i] : __b[2 * __i - 4];
    __r.val[1][__i] = __i < 2 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 4];
  }
  return __r;
}

__C17_INTRIN int32x4x2_t vtrnq_s32(int32x4_t __a, int32x4_t __b)
{
  int32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN int64x1_t __c17_vext_s64(int64x1_t __a, int64x1_t __b, const int __n)
{
  int64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __i + __n < 1 ? __a[__i + __n] : __b[__i + __n - 1];
  return __r;
}
#define vext_s64(...) __c17_vext_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t __c17_vextq_s64(int64x2_t __a, int64x2_t __b, const int __n)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i + __n < 2 ? __a[__i + __n] : __b[__i + __n - 2];
  return __r;
}
#define vextq_s64(...) __c17_vextq_s64(__VA_ARGS__)

__C17_INTRIN int64x2_t vzip1q_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN int64x2_t vzip2q_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 1] : __a[(__i >> 1) + 1];
  return __r;
}

__C17_INTRIN int64x2_t vuzp1q_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i] : __b[2 * __i - 2];
  return __r;
}

__C17_INTRIN int64x2_t vuzp2q_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 2];
  return __r;
}

__C17_INTRIN int64x2_t vtrn1q_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN int64x2_t vtrn2q_s64(int64x2_t __a, int64x2_t __b)
{
  int64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN uint8x8_t __c17_vext_u8(uint8x8_t __a, uint8x8_t __b, const int __n)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i + __n < 8 ? __a[__i + __n] : __b[__i + __n - 8];
  return __r;
}
#define vext_u8(...) __c17_vext_u8(__VA_ARGS__)

__C17_INTRIN uint8x8_t vrev16_u8(uint8x8_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN uint8x8_t vrev32_u8(uint8x8_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 3];
  return __r;
}

__C17_INTRIN uint8x8_t vrev64_u8(uint8x8_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 7];
  return __r;
}

__C17_INTRIN uint8x8_t vzip1_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN uint8x8_t vzip2_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 4] : __a[(__i >> 1) + 4];
  return __r;
}

__C17_INTRIN uint8x8_t vuzp1_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[2 * __i] : __b[2 * __i - 8];
  return __r;
}

__C17_INTRIN uint8x8_t vuzp2_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 8];
  return __r;
}

__C17_INTRIN uint8x8_t vtrn1_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vtrn2_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN uint8x8x2_t vzip_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 4] : __a[(__i >> 1) + 4];
  }
  return __r;
}

__C17_INTRIN uint8x8x2_t vuzp_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __i < 4 ? __a[2 * __i] : __b[2 * __i - 8];
    __r.val[1][__i] = __i < 4 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 8];
  }
  return __r;
}

__C17_INTRIN uint8x8x2_t vtrn_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN uint8x16_t __c17_vextq_u8(uint8x16_t __a, uint8x16_t __b, const int __n)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i + __n < 16 ? __a[__i + __n] : __b[__i + __n - 16];
  return __r;
}
#define vextq_u8(...) __c17_vextq_u8(__VA_ARGS__)

__C17_INTRIN uint8x16_t vrev16q_u8(uint8x16_t __a)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN uint8x16_t vrev32q_u8(uint8x16_t __a)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i ^ 3];
  return __r;
}

__C17_INTRIN uint8x16_t vrev64q_u8(uint8x16_t __a)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i ^ 7];
  return __r;
}

__C17_INTRIN uint8x16_t vzip1q_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN uint8x16_t vzip2q_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 8] : __a[(__i >> 1) + 8];
  return __r;
}

__C17_INTRIN uint8x16_t vuzp1q_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? __a[2 * __i] : __b[2 * __i - 16];
  return __r;
}

__C17_INTRIN uint8x16_t vuzp2q_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 16];
  return __r;
}

__C17_INTRIN uint8x16_t vtrn1q_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN uint8x16_t vtrn2q_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN uint8x16x2_t vzipq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 8] : __a[(__i >> 1) + 8];
  }
  return __r;
}

__C17_INTRIN uint8x16x2_t vuzpq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __i < 8 ? __a[2 * __i] : __b[2 * __i - 16];
    __r.val[1][__i] = __i < 8 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 16];
  }
  return __r;
}

__C17_INTRIN uint8x16x2_t vtrnq_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN uint16x4_t __c17_vext_u16(uint16x4_t __a, uint16x4_t __b, const int __n)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i + __n < 4 ? __a[__i + __n] : __b[__i + __n - 4];
  return __r;
}
#define vext_u16(...) __c17_vext_u16(__VA_ARGS__)

__C17_INTRIN uint16x4_t vrev32_u16(uint16x4_t __a)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN uint16x4_t vrev64_u16(uint16x4_t __a)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i ^ 3];
  return __r;
}

__C17_INTRIN uint16x4_t vzip1_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN uint16x4_t vzip2_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 2] : __a[(__i >> 1) + 2];
  return __r;
}

__C17_INTRIN uint16x4_t vuzp1_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i] : __b[2 * __i - 4];
  return __r;
}

__C17_INTRIN uint16x4_t vuzp2_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 4];
  return __r;
}

__C17_INTRIN uint16x4_t vtrn1_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN uint16x4_t vtrn2_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN uint16x4x2_t vzip_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 2] : __a[(__i >> 1) + 2];
  }
  return __r;
}

__C17_INTRIN uint16x4x2_t vuzp_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __i < 2 ? __a[2 * __i] : __b[2 * __i - 4];
    __r.val[1][__i] = __i < 2 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 4];
  }
  return __r;
}

__C17_INTRIN uint16x4x2_t vtrn_u16(uint16x4_t __a, uint16x4_t __b)
{
  uint16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN uint16x8_t __c17_vextq_u16(uint16x8_t __a, uint16x8_t __b, const int __n)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i + __n < 8 ? __a[__i + __n] : __b[__i + __n - 8];
  return __r;
}
#define vextq_u16(...) __c17_vextq_u16(__VA_ARGS__)

__C17_INTRIN uint16x8_t vrev32q_u16(uint16x8_t __a)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN uint16x8_t vrev64q_u16(uint16x8_t __a)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 3];
  return __r;
}

__C17_INTRIN uint16x8_t vzip1q_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN uint16x8_t vzip2q_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 4] : __a[(__i >> 1) + 4];
  return __r;
}

__C17_INTRIN uint16x8_t vuzp1q_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[2 * __i] : __b[2 * __i - 8];
  return __r;
}

__C17_INTRIN uint16x8_t vuzp2q_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 8];
  return __r;
}

__C17_INTRIN uint16x8_t vtrn1q_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN uint16x8_t vtrn2q_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN uint16x8x2_t vzipq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 4] : __a[(__i >> 1) + 4];
  }
  return __r;
}

__C17_INTRIN uint16x8x2_t vuzpq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __i < 4 ? __a[2 * __i] : __b[2 * __i - 8];
    __r.val[1][__i] = __i < 4 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 8];
  }
  return __r;
}

__C17_INTRIN uint16x8x2_t vtrnq_u16(uint16x8_t __a, uint16x8_t __b)
{
  uint16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN uint32x2_t __c17_vext_u32(uint32x2_t __a, uint32x2_t __b, const int __n)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i + __n < 2 ? __a[__i + __n] : __b[__i + __n - 2];
  return __r;
}
#define vext_u32(...) __c17_vext_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t vrev64_u32(uint32x2_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN uint32x2_t vzip1_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN uint32x2_t vzip2_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 1] : __a[(__i >> 1) + 1];
  return __r;
}

__C17_INTRIN uint32x2_t vuzp1_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i] : __b[2 * __i - 2];
  return __r;
}

__C17_INTRIN uint32x2_t vuzp2_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 2];
  return __r;
}

__C17_INTRIN uint32x2_t vtrn1_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN uint32x2_t vtrn2_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN uint32x2x2_t vzip_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 1] : __a[(__i >> 1) + 1];
  }
  return __r;
}

__C17_INTRIN uint32x2x2_t vuzp_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __i < 1 ? __a[2 * __i] : __b[2 * __i - 2];
    __r.val[1][__i] = __i < 1 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 2];
  }
  return __r;
}

__C17_INTRIN uint32x2x2_t vtrn_u32(uint32x2_t __a, uint32x2_t __b)
{
  uint32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN uint32x4_t __c17_vextq_u32(uint32x4_t __a, uint32x4_t __b, const int __n)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i + __n < 4 ? __a[__i + __n] : __b[__i + __n - 4];
  return __r;
}
#define vextq_u32(...) __c17_vextq_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t vrev64q_u32(uint32x4_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN uint32x4_t vzip1q_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN uint32x4_t vzip2q_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 2] : __a[(__i >> 1) + 2];
  return __r;
}

__C17_INTRIN uint32x4_t vuzp1q_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i] : __b[2 * __i - 4];
  return __r;
}

__C17_INTRIN uint32x4_t vuzp2q_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 4];
  return __r;
}

__C17_INTRIN uint32x4_t vtrn1q_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN uint32x4_t vtrn2q_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN uint32x4x2_t vzipq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 2] : __a[(__i >> 1) + 2];
  }
  return __r;
}

__C17_INTRIN uint32x4x2_t vuzpq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __i < 2 ? __a[2 * __i] : __b[2 * __i - 4];
    __r.val[1][__i] = __i < 2 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 4];
  }
  return __r;
}

__C17_INTRIN uint32x4x2_t vtrnq_u32(uint32x4_t __a, uint32x4_t __b)
{
  uint32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN uint64x1_t __c17_vext_u64(uint64x1_t __a, uint64x1_t __b, const int __n)
{
  uint64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __i + __n < 1 ? __a[__i + __n] : __b[__i + __n - 1];
  return __r;
}
#define vext_u64(...) __c17_vext_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t __c17_vextq_u64(uint64x2_t __a, uint64x2_t __b, const int __n)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i + __n < 2 ? __a[__i + __n] : __b[__i + __n - 2];
  return __r;
}
#define vextq_u64(...) __c17_vextq_u64(__VA_ARGS__)

__C17_INTRIN uint64x2_t vzip1q_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN uint64x2_t vzip2q_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 1] : __a[(__i >> 1) + 1];
  return __r;
}

__C17_INTRIN uint64x2_t vuzp1q_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i] : __b[2 * __i - 2];
  return __r;
}

__C17_INTRIN uint64x2_t vuzp2q_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 2];
  return __r;
}

__C17_INTRIN uint64x2_t vtrn1q_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN uint64x2_t vtrn2q_u64(uint64x2_t __a, uint64x2_t __b)
{
  uint64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN float32x2_t __c17_vext_f32(float32x2_t __a, float32x2_t __b, const int __n)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i + __n < 2 ? __a[__i + __n] : __b[__i + __n - 2];
  return __r;
}
#define vext_f32(...) __c17_vext_f32(__VA_ARGS__)

__C17_INTRIN float32x2_t vrev64_f32(float32x2_t __a)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN float32x2_t vzip1_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN float32x2_t vzip2_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 1] : __a[(__i >> 1) + 1];
  return __r;
}

__C17_INTRIN float32x2_t vuzp1_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i] : __b[2 * __i - 2];
  return __r;
}

__C17_INTRIN float32x2_t vuzp2_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 2];
  return __r;
}

__C17_INTRIN float32x2_t vtrn1_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN float32x2_t vtrn2_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN float32x2x2_t vzip_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 1] : __a[(__i >> 1) + 1];
  }
  return __r;
}

__C17_INTRIN float32x2x2_t vuzp_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = __i < 1 ? __a[2 * __i] : __b[2 * __i - 2];
    __r.val[1][__i] = __i < 1 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 2];
  }
  return __r;
}

__C17_INTRIN float32x2x2_t vtrn_f32(float32x2_t __a, float32x2_t __b)
{
  float32x2x2_t __r;
  for (int __i = 0; __i < 2; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN float32x4_t __c17_vextq_f32(float32x4_t __a, float32x4_t __b, const int __n)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i + __n < 4 ? __a[__i + __n] : __b[__i + __n - 4];
  return __r;
}
#define vextq_f32(...) __c17_vextq_f32(__VA_ARGS__)

__C17_INTRIN float32x4_t vrev64q_f32(float32x4_t __a)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN float32x4_t vzip1q_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN float32x4_t vzip2q_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 2] : __a[(__i >> 1) + 2];
  return __r;
}

__C17_INTRIN float32x4_t vuzp1q_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i] : __b[2 * __i - 4];
  return __r;
}

__C17_INTRIN float32x4_t vuzp2q_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 4];
  return __r;
}

__C17_INTRIN float32x4_t vtrn1q_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN float32x4_t vtrn2q_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN float32x4x2_t vzipq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 2] : __a[(__i >> 1) + 2];
  }
  return __r;
}

__C17_INTRIN float32x4x2_t vuzpq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __i < 2 ? __a[2 * __i] : __b[2 * __i - 4];
    __r.val[1][__i] = __i < 2 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 4];
  }
  return __r;
}

__C17_INTRIN float32x4x2_t vtrnq_f32(float32x4_t __a, float32x4_t __b)
{
  float32x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN float64x1_t __c17_vext_f64(float64x1_t __a, float64x1_t __b, const int __n)
{
  float64x1_t __r;
  for (int __i = 0; __i < 1; __i++)
    __r[__i] = __i + __n < 1 ? __a[__i + __n] : __b[__i + __n - 1];
  return __r;
}
#define vext_f64(...) __c17_vext_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t __c17_vextq_f64(float64x2_t __a, float64x2_t __b, const int __n)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i + __n < 2 ? __a[__i + __n] : __b[__i + __n - 2];
  return __r;
}
#define vextq_f64(...) __c17_vextq_f64(__VA_ARGS__)

__C17_INTRIN float64x2_t vzip1q_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN float64x2_t vzip2q_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 1] : __a[(__i >> 1) + 1];
  return __r;
}

__C17_INTRIN float64x2_t vuzp1q_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i] : __b[2 * __i - 2];
  return __r;
}

__C17_INTRIN float64x2_t vuzp2q_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __i < 1 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 2];
  return __r;
}

__C17_INTRIN float64x2_t vtrn1q_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN float64x2_t vtrn2q_f64(float64x2_t __a, float64x2_t __b)
{
  float64x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN poly8x8_t __c17_vext_p8(poly8x8_t __a, poly8x8_t __b, const int __n)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i + __n < 8 ? __a[__i + __n] : __b[__i + __n - 8];
  return __r;
}
#define vext_p8(...) __c17_vext_p8(__VA_ARGS__)

__C17_INTRIN poly8x8_t vrev16_p8(poly8x8_t __a)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN poly8x8_t vrev32_p8(poly8x8_t __a)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 3];
  return __r;
}

__C17_INTRIN poly8x8_t vrev64_p8(poly8x8_t __a)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 7];
  return __r;
}

__C17_INTRIN poly8x8_t vzip1_p8(poly8x8_t __a, poly8x8_t __b)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN poly8x8_t vzip2_p8(poly8x8_t __a, poly8x8_t __b)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 4] : __a[(__i >> 1) + 4];
  return __r;
}

__C17_INTRIN poly8x8_t vuzp1_p8(poly8x8_t __a, poly8x8_t __b)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[2 * __i] : __b[2 * __i - 8];
  return __r;
}

__C17_INTRIN poly8x8_t vuzp2_p8(poly8x8_t __a, poly8x8_t __b)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 8];
  return __r;
}

__C17_INTRIN poly8x8_t vtrn1_p8(poly8x8_t __a, poly8x8_t __b)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN poly8x8_t vtrn2_p8(poly8x8_t __a, poly8x8_t __b)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN poly8x8x2_t vzip_p8(poly8x8_t __a, poly8x8_t __b)
{
  poly8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 4] : __a[(__i >> 1) + 4];
  }
  return __r;
}

__C17_INTRIN poly8x8x2_t vuzp_p8(poly8x8_t __a, poly8x8_t __b)
{
  poly8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __i < 4 ? __a[2 * __i] : __b[2 * __i - 8];
    __r.val[1][__i] = __i < 4 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 8];
  }
  return __r;
}

__C17_INTRIN poly8x8x2_t vtrn_p8(poly8x8_t __a, poly8x8_t __b)
{
  poly8x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN poly8x16_t __c17_vextq_p8(poly8x16_t __a, poly8x16_t __b, const int __n)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i + __n < 16 ? __a[__i + __n] : __b[__i + __n - 16];
  return __r;
}
#define vextq_p8(...) __c17_vextq_p8(__VA_ARGS__)

__C17_INTRIN poly8x16_t vrev16q_p8(poly8x16_t __a)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN poly8x16_t vrev32q_p8(poly8x16_t __a)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i ^ 3];
  return __r;
}

__C17_INTRIN poly8x16_t vrev64q_p8(poly8x16_t __a)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __a[__i ^ 7];
  return __r;
}

__C17_INTRIN poly8x16_t vzip1q_p8(poly8x16_t __a, poly8x16_t __b)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN poly8x16_t vzip2q_p8(poly8x16_t __a, poly8x16_t __b)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 8] : __a[(__i >> 1) + 8];
  return __r;
}

__C17_INTRIN poly8x16_t vuzp1q_p8(poly8x16_t __a, poly8x16_t __b)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? __a[2 * __i] : __b[2 * __i - 16];
  return __r;
}

__C17_INTRIN poly8x16_t vuzp2q_p8(poly8x16_t __a, poly8x16_t __b)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __i < 8 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 16];
  return __r;
}

__C17_INTRIN poly8x16_t vtrn1q_p8(poly8x16_t __a, poly8x16_t __b)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN poly8x16_t vtrn2q_p8(poly8x16_t __a, poly8x16_t __b)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN poly8x16x2_t vzipq_p8(poly8x16_t __a, poly8x16_t __b)
{
  poly8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 8] : __a[(__i >> 1) + 8];
  }
  return __r;
}

__C17_INTRIN poly8x16x2_t vuzpq_p8(poly8x16_t __a, poly8x16_t __b)
{
  poly8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = __i < 8 ? __a[2 * __i] : __b[2 * __i - 16];
    __r.val[1][__i] = __i < 8 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 16];
  }
  return __r;
}

__C17_INTRIN poly8x16x2_t vtrnq_p8(poly8x16_t __a, poly8x16_t __b)
{
  poly8x16x2_t __r;
  for (int __i = 0; __i < 16; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN poly16x4_t __c17_vext_p16(poly16x4_t __a, poly16x4_t __b, const int __n)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i + __n < 4 ? __a[__i + __n] : __b[__i + __n - 4];
  return __r;
}
#define vext_p16(...) __c17_vext_p16(__VA_ARGS__)

__C17_INTRIN poly16x4_t vrev32_p16(poly16x4_t __a)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN poly16x4_t vrev64_p16(poly16x4_t __a)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __a[__i ^ 3];
  return __r;
}

__C17_INTRIN poly16x4_t vzip1_p16(poly16x4_t __a, poly16x4_t __b)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN poly16x4_t vzip2_p16(poly16x4_t __a, poly16x4_t __b)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 2] : __a[(__i >> 1) + 2];
  return __r;
}

__C17_INTRIN poly16x4_t vuzp1_p16(poly16x4_t __a, poly16x4_t __b)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i] : __b[2 * __i - 4];
  return __r;
}

__C17_INTRIN poly16x4_t vuzp2_p16(poly16x4_t __a, poly16x4_t __b)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __i < 2 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 4];
  return __r;
}

__C17_INTRIN poly16x4_t vtrn1_p16(poly16x4_t __a, poly16x4_t __b)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN poly16x4_t vtrn2_p16(poly16x4_t __a, poly16x4_t __b)
{
  poly16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN poly16x4x2_t vzip_p16(poly16x4_t __a, poly16x4_t __b)
{
  poly16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 2] : __a[(__i >> 1) + 2];
  }
  return __r;
}

__C17_INTRIN poly16x4x2_t vuzp_p16(poly16x4_t __a, poly16x4_t __b)
{
  poly16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = __i < 2 ? __a[2 * __i] : __b[2 * __i - 4];
    __r.val[1][__i] = __i < 2 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 4];
  }
  return __r;
}

__C17_INTRIN poly16x4x2_t vtrn_p16(poly16x4_t __a, poly16x4_t __b)
{
  poly16x4x2_t __r;
  for (int __i = 0; __i < 4; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN poly16x8_t __c17_vextq_p16(poly16x8_t __a, poly16x8_t __b, const int __n)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i + __n < 8 ? __a[__i + __n] : __b[__i + __n - 8];
  return __r;
}
#define vextq_p16(...) __c17_vextq_p16(__VA_ARGS__)

__C17_INTRIN poly16x8_t vrev32q_p16(poly16x8_t __a)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 1];
  return __r;
}

__C17_INTRIN poly16x8_t vrev64q_p16(poly16x8_t __a)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __a[__i ^ 3];
  return __r;
}

__C17_INTRIN poly16x8_t vzip1q_p16(poly16x8_t __a, poly16x8_t __b)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
  return __r;
}

__C17_INTRIN poly16x8_t vzip2q_p16(poly16x8_t __a, poly16x8_t __b)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[(__i >> 1) + 4] : __a[(__i >> 1) + 4];
  return __r;
}

__C17_INTRIN poly16x8_t vuzp1q_p16(poly16x8_t __a, poly16x8_t __b)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[2 * __i] : __b[2 * __i - 8];
  return __r;
}

__C17_INTRIN poly16x8_t vuzp2q_p16(poly16x8_t __a, poly16x8_t __b)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __i < 4 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 8];
  return __r;
}

__C17_INTRIN poly16x8_t vtrn1q_p16(poly16x8_t __a, poly16x8_t __b)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
  return __r;
}

__C17_INTRIN poly16x8_t vtrn2q_p16(poly16x8_t __a, poly16x8_t __b)
{
  poly16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  return __r;
}

__C17_INTRIN poly16x8x2_t vzipq_p16(poly16x8_t __a, poly16x8_t __b)
{
  poly16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i >> 1] : __a[__i >> 1];
    __r.val[1][__i] = (__i & 1) ? __b[(__i >> 1) + 4] : __a[(__i >> 1) + 4];
  }
  return __r;
}

__C17_INTRIN poly16x8x2_t vuzpq_p16(poly16x8_t __a, poly16x8_t __b)
{
  poly16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = __i < 4 ? __a[2 * __i] : __b[2 * __i - 8];
    __r.val[1][__i] = __i < 4 ? __a[2 * __i + 1] : __b[2 * __i + 1 - 8];
  }
  return __r;
}

__C17_INTRIN poly16x8x2_t vtrnq_p16(poly16x8_t __a, poly16x8_t __b)
{
  poly16x8x2_t __r;
  for (int __i = 0; __i < 8; __i++) {
    __r.val[0][__i] = (__i & 1) ? __b[__i - 1] : __a[__i];
    __r.val[1][__i] = (__i & 1) ? __b[__i] : __a[__i + 1];
  }
  return __r;
}

__C17_INTRIN int8x8_t vtbl1_s8(int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[8];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 8 ? __t[(uint8_t)__b[__i]] : 0;
  return __r;
}

__C17_INTRIN int8x8_t vtbx1_s8(int8x8_t __r0, int8x8_t __a, int8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[8];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 8 ? __t[(uint8_t)__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN int8x8_t vqtbl1_s8(int8x16_t __a, uint8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 16 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN int8x8_t vqtbx1_s8(int8x8_t __r0, int8x16_t __a, uint8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 16 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN int8x16_t vqtbl1q_s8(int8x16_t __a, uint8x16_t __b)
{
  int8x16_t __r;
  int8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 16 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN int8x16_t vqtbx1q_s8(int8x16_t __r0, int8x16_t __a, uint8x16_t __b)
{
  int8x16_t __r;
  int8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 16 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN int8x8_t vtbl2_s8(int8x8x2_t __a, int8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 16 ? __t[(uint8_t)__b[__i]] : 0;
  return __r;
}

__C17_INTRIN int8x8_t vtbx2_s8(int8x8_t __r0, int8x8x2_t __a, int8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 16 ? __t[(uint8_t)__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN int8x8_t vqtbl2_s8(int8x16x2_t __a, uint8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 32 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN int8x8_t vqtbx2_s8(int8x8_t __r0, int8x16x2_t __a, uint8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 32 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN int8x16_t vqtbl2q_s8(int8x16x2_t __a, uint8x16_t __b)
{
  int8x16_t __r;
  int8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 32 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN int8x16_t vqtbx2q_s8(int8x16_t __r0, int8x16x2_t __a, uint8x16_t __b)
{
  int8x16_t __r;
  int8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 32 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN int8x8_t vtbl3_s8(int8x8x3_t __a, int8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[24];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 24 ? __t[(uint8_t)__b[__i]] : 0;
  return __r;
}

__C17_INTRIN int8x8_t vtbx3_s8(int8x8_t __r0, int8x8x3_t __a, int8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[24];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 24 ? __t[(uint8_t)__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN int8x8_t vqtbl3_s8(int8x16x3_t __a, uint8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[48];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 48 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN int8x8_t vqtbx3_s8(int8x8_t __r0, int8x16x3_t __a, uint8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[48];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 48 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN int8x16_t vqtbl3q_s8(int8x16x3_t __a, uint8x16_t __b)
{
  int8x16_t __r;
  int8_t __t[48];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 48 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN int8x16_t vqtbx3q_s8(int8x16_t __r0, int8x16x3_t __a, uint8x16_t __b)
{
  int8x16_t __r;
  int8_t __t[48];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 48 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN int8x8_t vtbl4_s8(int8x8x4_t __a, int8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 32 ? __t[(uint8_t)__b[__i]] : 0;
  return __r;
}

__C17_INTRIN int8x8_t vtbx4_s8(int8x8_t __r0, int8x8x4_t __a, int8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 32 ? __t[(uint8_t)__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN int8x8_t vqtbl4_s8(int8x16x4_t __a, uint8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[64];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 64 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN int8x8_t vqtbx4_s8(int8x8_t __r0, int8x16x4_t __a, uint8x8_t __b)
{
  int8x8_t __r;
  int8_t __t[64];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 64 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN int8x16_t vqtbl4q_s8(int8x16x4_t __a, uint8x16_t __b)
{
  int8x16_t __r;
  int8_t __t[64];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 64 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN int8x16_t vqtbx4q_s8(int8x16_t __r0, int8x16x4_t __a, uint8x16_t __b)
{
  int8x16_t __r;
  int8_t __t[64];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 64 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vtbl1_u8(uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[8];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 8 ? __t[(uint8_t)__b[__i]] : 0;
  return __r;
}

__C17_INTRIN uint8x8_t vtbx1_u8(uint8x8_t __r0, uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[8];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 8 ? __t[(uint8_t)__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vqtbl1_u8(uint8x16_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 16 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN uint8x8_t vqtbx1_u8(uint8x8_t __r0, uint8x16_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 16 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN uint8x16_t vqtbl1q_u8(uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  uint8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 16 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN uint8x16_t vqtbx1q_u8(uint8x16_t __r0, uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  uint8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 16 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vtbl2_u8(uint8x8x2_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 16 ? __t[(uint8_t)__b[__i]] : 0;
  return __r;
}

__C17_INTRIN uint8x8_t vtbx2_u8(uint8x8_t __r0, uint8x8x2_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 16 ? __t[(uint8_t)__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vqtbl2_u8(uint8x16x2_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 32 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN uint8x8_t vqtbx2_u8(uint8x8_t __r0, uint8x16x2_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 32 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN uint8x16_t vqtbl2q_u8(uint8x16x2_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  uint8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 32 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN uint8x16_t vqtbx2q_u8(uint8x16_t __r0, uint8x16x2_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  uint8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 32 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vtbl3_u8(uint8x8x3_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[24];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 24 ? __t[(uint8_t)__b[__i]] : 0;
  return __r;
}

__C17_INTRIN uint8x8_t vtbx3_u8(uint8x8_t __r0, uint8x8x3_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[24];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 24 ? __t[(uint8_t)__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vqtbl3_u8(uint8x16x3_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[48];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 48 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN uint8x8_t vqtbx3_u8(uint8x8_t __r0, uint8x16x3_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[48];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 48 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN uint8x16_t vqtbl3q_u8(uint8x16x3_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  uint8_t __t[48];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 48 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN uint8x16_t vqtbx3q_u8(uint8x16_t __r0, uint8x16x3_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  uint8_t __t[48];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 48 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vtbl4_u8(uint8x8x4_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 32 ? __t[(uint8_t)__b[__i]] : 0;
  return __r;
}

__C17_INTRIN uint8x8_t vtbx4_u8(uint8x8_t __r0, uint8x8x4_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 32 ? __t[(uint8_t)__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN uint8x8_t vqtbl4_u8(uint8x16x4_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[64];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 64 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN uint8x8_t vqtbx4_u8(uint8x8_t __r0, uint8x16x4_t __a, uint8x8_t __b)
{
  uint8x8_t __r;
  uint8_t __t[64];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 64 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN uint8x16_t vqtbl4q_u8(uint8x16x4_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  uint8_t __t[64];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 64 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN uint8x16_t vqtbx4q_u8(uint8x16_t __r0, uint8x16x4_t __a, uint8x16_t __b)
{
  uint8x16_t __r;
  uint8_t __t[64];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 64 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN poly8x8_t vtbl1_p8(poly8x8_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[8];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 8 ? __t[(uint8_t)__b[__i]] : 0;
  return __r;
}

__C17_INTRIN poly8x8_t vtbx1_p8(poly8x8_t __r0, poly8x8_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[8];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 8 ? __t[(uint8_t)__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN poly8x8_t vqtbl1_p8(poly8x16_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 16 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN poly8x8_t vqtbx1_p8(poly8x8_t __r0, poly8x16_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 16 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN poly8x16_t vqtbl1q_p8(poly8x16_t __a, uint8x16_t __b)
{
  poly8x16_t __r;
  poly8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 16 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN poly8x16_t vqtbx1q_p8(poly8x16_t __r0, poly8x16_t __a, uint8x16_t __b)
{
  poly8x16_t __r;
  poly8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 16 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN poly8x8_t vtbl2_p8(poly8x8x2_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 16 ? __t[(uint8_t)__b[__i]] : 0;
  return __r;
}

__C17_INTRIN poly8x8_t vtbx2_p8(poly8x8_t __r0, poly8x8x2_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[16];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 16 ? __t[(uint8_t)__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN poly8x8_t vqtbl2_p8(poly8x16x2_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 32 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN poly8x8_t vqtbx2_p8(poly8x8_t __r0, poly8x16x2_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 32 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN poly8x16_t vqtbl2q_p8(poly8x16x2_t __a, uint8x16_t __b)
{
  poly8x16_t __r;
  poly8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 32 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN poly8x16_t vqtbx2q_p8(poly8x16_t __r0, poly8x16x2_t __a, uint8x16_t __b)
{
  poly8x16_t __r;
  poly8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 32 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN poly8x8_t vtbl3_p8(poly8x8x3_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[24];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 24 ? __t[(uint8_t)__b[__i]] : 0;
  return __r;
}

__C17_INTRIN poly8x8_t vtbx3_p8(poly8x8_t __r0, poly8x8x3_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[24];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 24 ? __t[(uint8_t)__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN poly8x8_t vqtbl3_p8(poly8x16x3_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[48];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 48 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN poly8x8_t vqtbx3_p8(poly8x8_t __r0, poly8x16x3_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[48];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 48 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN poly8x16_t vqtbl3q_p8(poly8x16x3_t __a, uint8x16_t __b)
{
  poly8x16_t __r;
  poly8_t __t[48];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 48 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN poly8x16_t vqtbx3q_p8(poly8x16_t __r0, poly8x16x3_t __a, uint8x16_t __b)
{
  poly8x16_t __r;
  poly8_t __t[48];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 48 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN poly8x8_t vtbl4_p8(poly8x8x4_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 32 ? __t[(uint8_t)__b[__i]] : 0;
  return __r;
}

__C17_INTRIN poly8x8_t vtbx4_p8(poly8x8_t __r0, poly8x8x4_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[32];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = (uint8_t)__b[__i] < 32 ? __t[(uint8_t)__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN poly8x8_t vqtbl4_p8(poly8x16x4_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[64];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 64 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN poly8x8_t vqtbx4_p8(poly8x8_t __r0, poly8x16x4_t __a, uint8x8_t __b)
{
  poly8x8_t __r;
  poly8_t __t[64];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __b[__i] < 64 ? __t[__b[__i]] : __r0[__i];
  return __r;
}

__C17_INTRIN poly8x16_t vqtbl4q_p8(poly8x16x4_t __a, uint8x16_t __b)
{
  poly8x16_t __r;
  poly8_t __t[64];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 64 ? __t[__b[__i]] : 0;
  return __r;
}

__C17_INTRIN poly8x16_t vqtbx4q_p8(poly8x16_t __r0, poly8x16x4_t __a, uint8x16_t __b)
{
  poly8x16_t __r;
  poly8_t __t[64];
  __builtin_memcpy(__t, &__a, sizeof(__t));
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __b[__i] < 64 ? __t[__b[__i]] : __r0[__i];
  return __r;
}


/* Bit counting and dot products. */

__C17_INTRIN int8x8_t vcnt_s8(int8x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __builtin_popcount((uint8_t)__a[__i]);
  return __r;
}

__C17_INTRIN int8x8_t vrbit_s8(int8x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_rbit8((uint8_t)__a[__i]);
  return __r;
}

__C17_INTRIN uint8x8_t vcnt_u8(uint8x8_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __builtin_popcount((uint8_t)__a[__i]);
  return __r;
}

__C17_INTRIN uint8x8_t vrbit_u8(uint8x8_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_rbit8((uint8_t)__a[__i]);
  return __r;
}

__C17_INTRIN poly8x8_t vcnt_p8(poly8x8_t __a)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __builtin_popcount((uint8_t)__a[__i]);
  return __r;
}

__C17_INTRIN poly8x8_t vrbit_p8(poly8x8_t __a)
{
  poly8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_rbit8((uint8_t)__a[__i]);
  return __r;
}

__C17_INTRIN int8x8_t vclz_s8(int8x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_clz((uint64_t)(uint8_t)(__a[__i]), 8);
  return __r;
}

__C17_INTRIN int8x8_t vcls_s8(int8x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_clz(((uint64_t)(uint8_t)(__a[__i]) ^ ((uint64_t)(uint8_t)(__a[__i]) >> 1)) & 127u, 8) - 1;
  return __r;
}

__C17_INTRIN int16x4_t vclz_s16(int16x4_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_clz((uint64_t)(uint16_t)(__a[__i]), 16);
  return __r;
}

__C17_INTRIN int16x4_t vcls_s16(int16x4_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_clz(((uint64_t)(uint16_t)(__a[__i]) ^ ((uint64_t)(uint16_t)(__a[__i]) >> 1)) & 32767u, 16) - 1;
  return __r;
}

__C17_INTRIN int32x2_t vclz_s32(int32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_clz((uint64_t)(uint32_t)(__a[__i]), 32);
  return __r;
}

__C17_INTRIN int32x2_t vcls_s32(int32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_clz(((uint64_t)(uint32_t)(__a[__i]) ^ ((uint64_t)(uint32_t)(__a[__i]) >> 1)) & 2147483647u, 32) - 1;
  return __r;
}

__C17_INTRIN uint8x8_t vclz_u8(uint8x8_t __a)
{
  uint8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_clz((uint64_t)(uint8_t)(__a[__i]), 8);
  return __r;
}

__C17_INTRIN int8x8_t vcls_u8(uint8x8_t __a)
{
  int8x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_clz(((uint64_t)(uint8_t)(__a[__i]) ^ ((uint64_t)(uint8_t)(__a[__i]) >> 1)) & 127u, 8) - 1;
  return __r;
}

__C17_INTRIN uint16x4_t vclz_u16(uint16x4_t __a)
{
  uint16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_clz((uint64_t)(uint16_t)(__a[__i]), 16);
  return __r;
}

__C17_INTRIN int16x4_t vcls_u16(uint16x4_t __a)
{
  int16x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_clz(((uint64_t)(uint16_t)(__a[__i]) ^ ((uint64_t)(uint16_t)(__a[__i]) >> 1)) & 32767u, 16) - 1;
  return __r;
}

__C17_INTRIN uint32x2_t vclz_u32(uint32x2_t __a)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_clz((uint64_t)(uint32_t)(__a[__i]), 32);
  return __r;
}

__C17_INTRIN int32x2_t vcls_u32(uint32x2_t __a)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = __c17_clz(((uint64_t)(uint32_t)(__a[__i]) ^ ((uint64_t)(uint32_t)(__a[__i]) >> 1)) & 2147483647u, 32) - 1;
  return __r;
}

__C17_INTRIN int32x2_t vdot_s32(int32x2_t __r0, int8x8_t __a, int8x8_t __b)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__r0[__i] + (uint64_t)((int64_t)__a[4 * __i + 0] * __b[4 * __i + 0]) + (uint64_t)((int64_t)__a[4 * __i + 1] * __b[4 * __i + 1]) + (uint64_t)((int64_t)__a[4 * __i + 2] * __b[4 * __i + 2]) + (uint64_t)((int64_t)__a[4 * __i + 3] * __b[4 * __i + 3]);
  return __r;
}

__C17_INTRIN int32x2_t __c17_vdot_lane_s32(int32x2_t __r0, int8x8_t __a, int8x8_t __b, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__r0[__i] + (uint64_t)((int64_t)__a[4 * __i + 0] * __b[4 * __lane + 0]) + (uint64_t)((int64_t)__a[4 * __i + 1] * __b[4 * __lane + 1]) + (uint64_t)((int64_t)__a[4 * __i + 2] * __b[4 * __lane + 2]) + (uint64_t)((int64_t)__a[4 * __i + 3] * __b[4 * __lane + 3]);
  return __r;
}
#define vdot_lane_s32(...) __c17_vdot_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x2_t __c17_vdot_laneq_s32(int32x2_t __r0, int8x8_t __a, int8x16_t __b, const int __lane)
{
  int32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__r0[__i] + (uint64_t)((int64_t)__a[4 * __i + 0] * __b[4 * __lane + 0]) + (uint64_t)((int64_t)__a[4 * __i + 1] * __b[4 * __lane + 1]) + (uint64_t)((int64_t)__a[4 * __i + 2] * __b[4 * __lane + 2]) + (uint64_t)((int64_t)__a[4 * __i + 3] * __b[4 * __lane + 3]);
  return __r;
}
#define vdot_laneq_s32(...) __c17_vdot_laneq_s32(__VA_ARGS__)

__C17_INTRIN uint32x2_t vdot_u32(uint32x2_t __r0, uint8x8_t __a, uint8x8_t __b)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__r0[__i] + (uint64_t)((int64_t)__a[4 * __i + 0] * __b[4 * __i + 0]) + (uint64_t)((int64_t)__a[4 * __i + 1] * __b[4 * __i + 1]) + (uint64_t)((int64_t)__a[4 * __i + 2] * __b[4 * __i + 2]) + (uint64_t)((int64_t)__a[4 * __i + 3] * __b[4 * __i + 3]);
  return __r;
}

__C17_INTRIN uint32x2_t __c17_vdot_lane_u32(uint32x2_t __r0, uint8x8_t __a, uint8x8_t __b, const int __lane)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__r0[__i] + (uint64_t)((int64_t)__a[4 * __i + 0] * __b[4 * __lane + 0]) + (uint64_t)((int64_t)__a[4 * __i + 1] * __b[4 * __lane + 1]) + (uint64_t)((int64_t)__a[4 * __i + 2] * __b[4 * __lane + 2]) + (uint64_t)((int64_t)__a[4 * __i + 3] * __b[4 * __lane + 3]);
  return __r;
}
#define vdot_lane_u32(...) __c17_vdot_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x2_t __c17_vdot_laneq_u32(uint32x2_t __r0, uint8x8_t __a, uint8x16_t __b, const int __lane)
{
  uint32x2_t __r;
  for (int __i = 0; __i < 2; __i++)
    __r[__i] = (uint64_t)__r0[__i] + (uint64_t)((int64_t)__a[4 * __i + 0] * __b[4 * __lane + 0]) + (uint64_t)((int64_t)__a[4 * __i + 1] * __b[4 * __lane + 1]) + (uint64_t)((int64_t)__a[4 * __i + 2] * __b[4 * __lane + 2]) + (uint64_t)((int64_t)__a[4 * __i + 3] * __b[4 * __lane + 3]);
  return __r;
}
#define vdot_laneq_u32(...) __c17_vdot_laneq_u32(__VA_ARGS__)

__C17_INTRIN int8x16_t vcntq_s8(int8x16_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __builtin_popcount((uint8_t)__a[__i]);
  return __r;
}

__C17_INTRIN int8x16_t vrbitq_s8(int8x16_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_rbit8((uint8_t)__a[__i]);
  return __r;
}

__C17_INTRIN uint8x16_t vcntq_u8(uint8x16_t __a)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __builtin_popcount((uint8_t)__a[__i]);
  return __r;
}

__C17_INTRIN uint8x16_t vrbitq_u8(uint8x16_t __a)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_rbit8((uint8_t)__a[__i]);
  return __r;
}

__C17_INTRIN poly8x16_t vcntq_p8(poly8x16_t __a)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __builtin_popcount((uint8_t)__a[__i]);
  return __r;
}

__C17_INTRIN poly8x16_t vrbitq_p8(poly8x16_t __a)
{
  poly8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_rbit8((uint8_t)__a[__i]);
  return __r;
}

__C17_INTRIN int8x16_t vclzq_s8(int8x16_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_clz((uint64_t)(uint8_t)(__a[__i]), 8);
  return __r;
}

__C17_INTRIN int8x16_t vclsq_s8(int8x16_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_clz(((uint64_t)(uint8_t)(__a[__i]) ^ ((uint64_t)(uint8_t)(__a[__i]) >> 1)) & 127u, 8) - 1;
  return __r;
}

__C17_INTRIN int16x8_t vclzq_s16(int16x8_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_clz((uint64_t)(uint16_t)(__a[__i]), 16);
  return __r;
}

__C17_INTRIN int16x8_t vclsq_s16(int16x8_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_clz(((uint64_t)(uint16_t)(__a[__i]) ^ ((uint64_t)(uint16_t)(__a[__i]) >> 1)) & 32767u, 16) - 1;
  return __r;
}

__C17_INTRIN int32x4_t vclzq_s32(int32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_clz((uint64_t)(uint32_t)(__a[__i]), 32);
  return __r;
}

__C17_INTRIN int32x4_t vclsq_s32(int32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_clz(((uint64_t)(uint32_t)(__a[__i]) ^ ((uint64_t)(uint32_t)(__a[__i]) >> 1)) & 2147483647u, 32) - 1;
  return __r;
}

__C17_INTRIN uint8x16_t vclzq_u8(uint8x16_t __a)
{
  uint8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_clz((uint64_t)(uint8_t)(__a[__i]), 8);
  return __r;
}

__C17_INTRIN int8x16_t vclsq_u8(uint8x16_t __a)
{
  int8x16_t __r;
  for (int __i = 0; __i < 16; __i++)
    __r[__i] = __c17_clz(((uint64_t)(uint8_t)(__a[__i]) ^ ((uint64_t)(uint8_t)(__a[__i]) >> 1)) & 127u, 8) - 1;
  return __r;
}

__C17_INTRIN uint16x8_t vclzq_u16(uint16x8_t __a)
{
  uint16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_clz((uint64_t)(uint16_t)(__a[__i]), 16);
  return __r;
}

__C17_INTRIN int16x8_t vclsq_u16(uint16x8_t __a)
{
  int16x8_t __r;
  for (int __i = 0; __i < 8; __i++)
    __r[__i] = __c17_clz(((uint64_t)(uint16_t)(__a[__i]) ^ ((uint64_t)(uint16_t)(__a[__i]) >> 1)) & 32767u, 16) - 1;
  return __r;
}

__C17_INTRIN uint32x4_t vclzq_u32(uint32x4_t __a)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_clz((uint64_t)(uint32_t)(__a[__i]), 32);
  return __r;
}

__C17_INTRIN int32x4_t vclsq_u32(uint32x4_t __a)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = __c17_clz(((uint64_t)(uint32_t)(__a[__i]) ^ ((uint64_t)(uint32_t)(__a[__i]) >> 1)) & 2147483647u, 32) - 1;
  return __r;
}

__C17_INTRIN int32x4_t vdotq_s32(int32x4_t __r0, int8x16_t __a, int8x16_t __b)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__r0[__i] + (uint64_t)((int64_t)__a[4 * __i + 0] * __b[4 * __i + 0]) + (uint64_t)((int64_t)__a[4 * __i + 1] * __b[4 * __i + 1]) + (uint64_t)((int64_t)__a[4 * __i + 2] * __b[4 * __i + 2]) + (uint64_t)((int64_t)__a[4 * __i + 3] * __b[4 * __i + 3]);
  return __r;
}

__C17_INTRIN int32x4_t __c17_vdotq_lane_s32(int32x4_t __r0, int8x16_t __a, int8x8_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__r0[__i] + (uint64_t)((int64_t)__a[4 * __i + 0] * __b[4 * __lane + 0]) + (uint64_t)((int64_t)__a[4 * __i + 1] * __b[4 * __lane + 1]) + (uint64_t)((int64_t)__a[4 * __i + 2] * __b[4 * __lane + 2]) + (uint64_t)((int64_t)__a[4 * __i + 3] * __b[4 * __lane + 3]);
  return __r;
}
#define vdotq_lane_s32(...) __c17_vdotq_lane_s32(__VA_ARGS__)

__C17_INTRIN int32x4_t __c17_vdotq_laneq_s32(int32x4_t __r0, int8x16_t __a, int8x16_t __b, const int __lane)
{
  int32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__r0[__i] + (uint64_t)((int64_t)__a[4 * __i + 0] * __b[4 * __lane + 0]) + (uint64_t)((int64_t)__a[4 * __i + 1] * __b[4 * __lane + 1]) + (uint64_t)((int64_t)__a[4 * __i + 2] * __b[4 * __lane + 2]) + (uint64_t)((int64_t)__a[4 * __i + 3] * __b[4 * __lane + 3]);
  return __r;
}
#define vdotq_laneq_s32(...) __c17_vdotq_laneq_s32(__VA_ARGS__)

__C17_INTRIN uint32x4_t vdotq_u32(uint32x4_t __r0, uint8x16_t __a, uint8x16_t __b)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__r0[__i] + (uint64_t)((int64_t)__a[4 * __i + 0] * __b[4 * __i + 0]) + (uint64_t)((int64_t)__a[4 * __i + 1] * __b[4 * __i + 1]) + (uint64_t)((int64_t)__a[4 * __i + 2] * __b[4 * __i + 2]) + (uint64_t)((int64_t)__a[4 * __i + 3] * __b[4 * __i + 3]);
  return __r;
}

__C17_INTRIN uint32x4_t __c17_vdotq_lane_u32(uint32x4_t __r0, uint8x16_t __a, uint8x8_t __b, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__r0[__i] + (uint64_t)((int64_t)__a[4 * __i + 0] * __b[4 * __lane + 0]) + (uint64_t)((int64_t)__a[4 * __i + 1] * __b[4 * __lane + 1]) + (uint64_t)((int64_t)__a[4 * __i + 2] * __b[4 * __lane + 2]) + (uint64_t)((int64_t)__a[4 * __i + 3] * __b[4 * __lane + 3]);
  return __r;
}
#define vdotq_lane_u32(...) __c17_vdotq_lane_u32(__VA_ARGS__)

__C17_INTRIN uint32x4_t __c17_vdotq_laneq_u32(uint32x4_t __r0, uint8x16_t __a, uint8x16_t __b, const int __lane)
{
  uint32x4_t __r;
  for (int __i = 0; __i < 4; __i++)
    __r[__i] = (uint64_t)__r0[__i] + (uint64_t)((int64_t)__a[4 * __i + 0] * __b[4 * __lane + 0]) + (uint64_t)((int64_t)__a[4 * __i + 1] * __b[4 * __lane + 1]) + (uint64_t)((int64_t)__a[4 * __i + 2] * __b[4 * __lane + 2]) + (uint64_t)((int64_t)__a[4 * __i + 3] * __b[4 * __lane + 3]);
  return __r;
}
#define vdotq_laneq_u32(...) __c17_vdotq_laneq_u32(__VA_ARGS__)


#endif /* _ARM_NEON_H_INCLUDED */
