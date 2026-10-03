/*
 * Self-checking test of c17's bundled intrinsic headers: expected values
 * were captured from gcc running on the hardware (or under qemu).
 * Exits 0 on success, else the number of the first failing check.
 * SPDX-License-Identifier: MIT
 */
/* Self-checking test for c17's arm_neon.h: each check compares an intrinsic's
 * result bytes with gcc's arm_neon.h on the same input (captured under qemu)
 * and returns the check number on a mismatch, 0 when all pass. */
#include <stdint.h>
#include <string.h>
#include <arm_neon.h>

static const int8_t S8A[16] = {-128, -127, -100, -64, -9, -2, -1, 0, 1, 2, 9, 63, 64, 100, 126, 127};
static const int8_t S8B[16] = {127, -128, 100, 64, 3, -2, 1, -1, -1, 7, -9, 65, 64, 27, 2, 127};
static const uint8_t U8A[16] = {0, 1, 2, 7, 8, 15, 16, 100, 127, 128, 129, 200, 250, 254, 255, 3};
static const uint8_t U8B[16] = {255, 1, 254, 9, 8, 0, 31, 200, 1, 128, 127, 100, 6, 2, 255, 17};
static const int8_t BIG8[64] = {
  -128, -127, -100, -64, -9, -2, -1, 0, 1, 2, 9, 63, 64, 100, 126, 127,
  5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  -5, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20,
  40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55};
static const int16_t BIG16[16] = {-32768, -32767, -300, -1, 0, 1, 300, 32767,
  1000, -1000, 2000, -2000, 3000, -3000, 4000, -4000};
static const int16_t S16A[8] = {-32768, -32767, -300, -1, 0, 1, 300, 32767};
static const int16_t S16B[8] = {-32768, 2, 300, -1, 17, -16, 32767, 32767};
static const uint16_t U16A[8] = {0, 1, 255, 256, 32767, 32768, 65534, 65535};
static const uint16_t U16B[8] = {65535, 1, 3, 256, 1, 32768, 2, 65535};
static const int32_t S32A[4] = {INT32_MIN, -5, 7, INT32_MAX};
static const int32_t S32B[4] = {INT32_MIN, 3, -70000, 2};
static const uint32_t U32A[4] = {0, 1, 0x80000000u, 0xffffffffu};
static const uint32_t U32B[4] = {0xffffffffu, 5, 0x80000001u, 0x12345678u};
static const int64_t S64A[2] = {INT64_MIN, 12345};
static const int64_t S64B[2] = {-1, INT64_MAX};
static const uint64_t U64A[2] = {~0ull, 3};
static const uint64_t U64B[2] = {2, 0x8000000000000000ull};
/* 1.5, -2.5, quiet NaN with payload, -0.0 / +Inf, -Inf, 0.5, 3.5 */
static const uint32_t F32A[4] = {0x3fc00000u, 0xc0200000u, 0x7fc12345u, 0x80000000u};
static const uint32_t F32B[4] = {0x7f800000u, 0xff800000u, 0x3f000000u, 0x40600000u};
/* 2.5e9, -3e9, 0.49999997, -1.5 */
static const uint32_t F32C[4] = {0x4f1502f9u, 0xcf32d05eu, 0x3effffffu, 0xbfc00000u};
/* 2.5, -0.0 / quiet NaN, 1e300 */
static const uint64_t F64A[2] = {0x4004000000000000ull, 0x8000000000000000ull};
static const uint64_t F64B[2] = {0x7ff8000000000001ull, 0x7e37e43c8800759cull};

static int8x16_t s8a, s8b;
static uint8x16_t u8a, u8b;
static int16x8_t s16a, s16b;
static uint16x8_t u16a, u16b;
static int32x4_t s32a, s32b;
static uint32x4_t u32a, u32b;
static int64x2_t s64a, s64b;
static uint64x2_t u64a, u64b;
static float32x4_t f32a, f32b, f32c;
static float64x2_t f64a, f64b;

static void setup(void)
{
  s8a = vld1q_s8(S8A); s8b = vld1q_s8(S8B);
  u8a = vld1q_u8(U8A); u8b = vld1q_u8(U8B);
  s16a = vld1q_s16(S16A); s16b = vld1q_s16(S16B);
  u16a = vld1q_u16(U16A); u16b = vld1q_u16(U16B);
  s32a = vld1q_s32(S32A); s32b = vld1q_s32(S32B);
  u32a = vld1q_u32(U32A); u32b = vld1q_u32(U32B);
  s64a = vld1q_s64(S64A); s64b = vld1q_s64(S64B);
  u64a = vld1q_u64(U64A); u64b = vld1q_u64(U64B);
  f32a = vreinterpretq_f32_u32(vld1q_u32(F32A));
  f32b = vreinterpretq_f32_u32(vld1q_u32(F32B));
  f32c = vreinterpretq_f32_u32(vld1q_u32(F32C));
  f64a = vreinterpretq_f64_u64(vld1q_u64(F64A));
  f64b = vreinterpretq_f64_u64(vld1q_u64(F64B));
}

static uint8x16_t st3_ld3(void)
{
  uint8_t __buf[48];
  uint8x16x3_t v = {{u8a, u8b, veorq_u8(u8a, u8b)}};
  uint8x16x3_t w;
  vst3q_u8(__buf, v);
  w = vld3q_u8(__buf);
  return vaddq_u8(vld1q_u8(__buf + 16), vsubq_u8(w.val[2], v.val[2]));
}

static int16x4x4_t ld4_s16(void)
{
  return vld4_s16(BIG16);
}

static uint32x4_t st1_lane(void)
{
  uint32_t __buf[4] = {1, 2, 3, 4};
  vst1q_lane_u32(__buf + 2, u32b, 3);
  return vld1q_u32(__buf);
}

static uint16x8x2_t ld2_lane(void)
{
  uint16x8x2_t v = {{u16a, u16b}};
  uint16_t __src[2] = {0xabcd, 0x1234};
  return vld2q_lane_u16(__src, v, 5);
}

static float32x4x2_t ld1_x2(void)
{
  float __f[8] = {1, 2, 3, 4, 5, 6, 7, 8};
  return vld1q_f32_x2(__f);
}
/* Compare a result's bytes with the expected hex string. */
static int differs(const void *p, size_t n, const char *hex)
{
  const unsigned char *b = p;
  for (size_t i = 0; i < n; i++) {
    unsigned v = 0;
    for (int j = 0; j < 2; j++) {
      char c = hex[2 * i + j];
      v = v * 16 + (unsigned)(c <= '9' ? c - '0' : c - 'a' + 10);
    }
    if (b[i] != v)
      return 1;
  }
  return hex[2 * n] != 0;
}
int main(void)
{
  setup();
  { /* 1 */
    int8x8_t r = vld1_s8(S8A + 3);
    if (differs(&r, sizeof r, "c0f7feff00010209"))
      return 1;
  }
  { /* 2 */
    int8x16x4_t r = vld4q_s8(BIG8);
    if (differs(&r, sizeof r, "80f7014005090d11fbf7f3ef282c303481fe0264060a0e12faf6f2ee292d31359cff097e070b0f13f9f5f1ed2a2e3236c0003f7f080c1014f8f4f0ec2b2f3337"))
      return 2;
  }
  { /* 3 */
    int8x16x3_t r = vld1q_s8_x3(BIG8 + 8);
    if (differs(&r, sizeof r, "0102093f40647e7f05060708090a0b0c0d0e0f1011121314fbfaf9f8f7f6f5f4f3f2f1f0efeeedec28292a2b2c2d2e2f"))
      return 3;
  }
  { /* 4 */
    int16x8_t r = vld1q_dup_s16(S16A + 6);
    if (differs(&r, sizeof r, "2c012c012c012c012c012c012c012c01"))
      return 4;
  }
  { /* 5 */
    int8x16x2_t r = vld2q_s8(BIG8);
    if (differs(&r, sizeof r, "809cf7ff0109407e0507090b0d0f111381c0fe00023f647f06080a0c0e101214"))
      return 5;
  }
  { /* 6 */
    int16x4x4_t r = ld4_s16();
    if (differs(&r, sizeof r, "00800000e803b80b0180010018fc48f4d4fe2c01d007a00fffffff7f30f860f0"))
      return 6;
  }
  { /* 7 */
    uint8x16_t r = st3_ld3();
    if (differs(&r, sizeof r, "000f101f0f64c8ac7f017e808000817f"))
      return 7;
  }
  { /* 8 */
    uint32x4_t r = st1_lane();
    if (differs(&r, sizeof r, "01000000020000007856341204000000"))
      return 8;
  }
  { /* 9 */
    uint16x8x2_t r = ld2_lane();
    if (differs(&r, sizeof r, "00000100ff000001ff7fcdabfeffffffffff010003000001010034120200ffff"))
      return 9;
  }
  { /* 10 */
    float32x4x2_t r = ld1_x2();
    if (differs(&r, sizeof r, "0000803f0000004000004040000080400000a0400000c0400000e04000000041"))
      return 10;
  }
  { /* 11 */
    int32x4_t r = vld1q_lane_s32(S32B + 2, s32a, 1);
    if (differs(&r, sizeof r, "0000008090eefeff07000000ffffff7f"))
      return 11;
  }
  { /* 12 */
    uint8x16_t r = vdupq_n_u8(0xa5);
    if (differs(&r, sizeof r, "a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5"))
      return 12;
  }
  { /* 13 */
    float64x2_t r = vdupq_n_f64(-0.0);
    if (differs(&r, sizeof r, "00000000000000800000000000000080"))
      return 13;
  }
  { /* 14 */
    int16x4_t r = vdup_lane_s16(vget_low_s16(s16a), 2);
    if (differs(&r, sizeof r, "d4fed4fed4fed4fe"))
      return 14;
  }
  { /* 15 */
    uint32x4_t r = vdupq_laneq_u32(u32b, 3);
    if (differs(&r, sizeof r, "78563412785634127856341278563412"))
      return 15;
  }
  { /* 16 */
    int8_t r = vgetq_lane_s8(s8a, 15);
    if (differs(&r, sizeof r, "7f"))
      return 16;
  }
  { /* 17 */
    float32_t r = vgetq_lane_f32(f32a, 1);
    if (differs(&r, sizeof r, "000020c0"))
      return 17;
  }
  { /* 18 */
    int64x2_t r = vsetq_lane_s64(-7, s64a, 1);
    if (differs(&r, sizeof r, "0000000000000080f9ffffffffffffff"))
      return 18;
  }
  { /* 19 */
    uint8x8_t r = vget_high_u8(u8a);
    if (differs(&r, sizeof r, "7f8081c8fafeff03"))
      return 19;
  }
  { /* 20 */
    int16x8_t r = vcombine_s16(vget_high_s16(s16b), vget_low_s16(s16a));
    if (differs(&r, sizeof r, "1100f0ffff7fff7f00800180d4feffff"))
      return 20;
  }
  { /* 21 */
    uint16x4_t r = vcreate_u16(0x0123456789abcdefull);
    if (differs(&r, sizeof r, "efcdab8967452301"))
      return 21;
  }
  { /* 22 */
    int32x4_t r = vcopyq_lane_s32(s32a, 0, vget_low_s32(s32b), 1);
    if (differs(&r, sizeof r, "03000000fbffffff07000000ffffff7f"))
      return 22;
  }
  { /* 23 */
    float32x4_t r = vreinterpretq_f32_s8(s8b);
    if (differs(&r, sizeof r, "7f80644003fe01ffff07f741401b027f"))
      return 23;
  }
  { /* 24 */
    uint64x2_t r = vreinterpretq_u64_f64(f64a);
    if (differs(&r, sizeof r, "00000000000004400000000000000080"))
      return 24;
  }
  { /* 25 */
    poly8x16_t r = vreinterpretq_p8_u16(u16a);
    if (differs(&r, sizeof r, "00000100ff000001ff7f0080feffffff"))
      return 25;
  }
  { /* 26 */
    int8x16_t r = vaddq_s8(s8a, s8b);
    if (differs(&r, sizeof r, "ff010000fafc00ff00090080807f80fe"))
      return 26;
  }
  { /* 27 */
    uint16x8_t r = vsubq_u16(u16a, u16b);
    if (differs(&r, sizeof r, "01000000fc000000fe7f0000fcff0000"))
      return 27;
  }
  { /* 28 */
    int32x4_t r = vmulq_s32(s32a, s32b);
    if (differs(&r, sizeof r, "00000000f1fffffff085f8fffeffffff"))
      return 28;
  }
  { /* 29 */
    uint8x16_t r = vmlaq_u8(u8a, u8b, u8b);
    if (differs(&r, sizeof r, "01020658480fd1a4808082d81e020024"))
      return 29;
  }
  { /* 30 */
    int16x8_t r = vmlsq_s16(s16a, s16b, s16a);
    if (differs(&r, sizeof r, "0080ff7f645efeff000011005802fe7f"))
      return 30;
  }
  { /* 31 */
    int16x8_t r = vmulq_n_s16(s16a, -3);
    if (differs(&r, sizeof r, "0080fd7f840303000000fdff7cfc0380"))
      return 31;
  }
  { /* 32 */
    uint32x4_t r = vmulq_laneq_u32(u32a, u32b, 3);
    if (differs(&r, sizeof r, "00000000785634120000000088a9cbed"))
      return 32;
  }
  { /* 33 */
    int32x4_t r = vmlaq_lane_s32(s32a, s32b, vget_low_s32(s32b), 1);
    if (differs(&r, sizeof r, "0000000004000000b7cbfcff05000080"))
      return 33;
  }
  { /* 34 */
    float32x4_t r = vaddq_f32(f32a, f32b);
    if (differs(&r, sizeof r, "0000807f000080ff4523c17f00006040"))
      return 34;
  }
  { /* 35 */
    float32x4_t r = vmulq_f32(f32a, f32c);
    if (differs(&r, sizeof r, "76845f4f7684df4f4523c17f00000000"))
      return 35;
  }
  { /* 36 */
    float32x4_t r = vdivq_f32(f32c, f32a);
    if (differs(&r, sizeof r, "a1aec64e180d8f4e4523c17f0000807f"))
      return 36;
  }
  { /* 37 */
    float64x2_t r = vdivq_f64(f64a, f64b);
    if (differs(&r, sizeof r, "010000000000f87f0000000000000080"))
      return 37;
  }
  { /* 38 */
    float32x4_t r = vmlaq_f32(f32c, f32c, f32c);
    if (differs(&r, sizeof r, "ec78ad5ed9ccf95effff3f3f0000403f"))
      return 38;
  }
  { /* 39 */
    float32x4_t r = vfmaq_f32(f32c, f32c, f32c);
    if (differs(&r, sizeof r, "ec78ad5ed9ccf95effff3f3f0000403f"))
      return 39;
  }
  { /* 40 */
    float32x4_t r = vfmsq_laneq_f32(f32c, f32c, f32c, 2);
    if (differs(&r, sizeof r, "fa02954e5fd0b2ce0000803e010040bf"))
      return 40;
  }
  { /* 41 */
    float64x2_t r = vfmaq_n_f64(f64a, f64a, 3.0);
    if (differs(&r, sizeof r, "00000000000024400000000000000080"))
      return 41;
  }
  { /* 42 */
    float32x4_t r = vmulxq_f32(vcombine_f32(vget_low_f32(f32b), vdup_n_f32(0.0f)), vcombine_f32(vdup_n_f32(-0.0f), vget_low_f32(f32b)));
    if (differs(&r, sizeof r, "000000c00000004000000040000000c0"))
      return 42;
  }
  { /* 43 */
    int8x16_t r = vabsq_s8(s8a);
    if (differs(&r, sizeof r, "807f6440090201000102093f40647e7f"))
      return 43;
  }
  { /* 44 */
    int8x16_t r = vqabsq_s8(s8a);
    if (differs(&r, sizeof r, "7f7f6440090201000102093f40647e7f"))
      return 44;
  }
  { /* 45 */
    int64x2_t r = vqnegq_s64(s64a);
    if (differs(&r, sizeof r, "ffffffffffffff7fc7cfffffffffffff"))
      return 45;
  }
  { /* 46 */
    float32x4_t r = vnegq_f32(f32a);
    if (differs(&r, sizeof r, "0000c0bf000020404523c1ff00000000"))
      return 46;
  }
  { /* 47 */
    float32x4_t r = vabsq_f32(f32a);
    if (differs(&r, sizeof r, "0000c03f000020404523c17f00000000"))
      return 47;
  }
  { /* 48 */
    uint8x16_t r = vabdq_u8(u8a, u8b);
    if (differs(&r, sizeof r, "ff00fc02000f0f647e000264f4fc000e"))
      return 48;
  }
  { /* 49 */
    int16x8_t r = vabdq_s16(s16a, s16b);
    if (differs(&r, sizeof r, "000001805802000011001100d37e0000"))
      return 49;
  }
  { /* 50 */
    float32x4_t r = vabdq_f32(f32a, f32c);
    if (differs(&r, sizeof r, "f902154f5ed0324f4523c17f0000c03f"))
      return 50;
  }
  { /* 51 */
    uint32x4_t r = vabdl_u16(vget_low_u16(u16a), vget_low_u16(u16b));
    if (differs(&r, sizeof r, "ffff000000000000fc00000000000000"))
      return 51;
  }
  { /* 52 */
    int8x16_t r = vabaq_s8(s8a, s8b, s8a);
    if (differs(&r, sizeof r, "7f82644003fe010103071b4140adfa7f"))
      return 52;
  }
  { /* 53 */
    int16x8_t r = vaddl_s8(vget_low_s8(s8a), vget_low_s8(s8b));
    if (differs(&r, sizeof r, "ffff01ff00000000fafffcff0000ffff"))
      return 53;
  }
  { /* 54 */
    uint32x4_t r = vsubl_high_u16(u16a, u16b);
    if (differs(&r, sizeof r, "fe7f000000000000fcff000000000000"))
      return 54;
  }
  { /* 55 */
    int32x4_t r = vaddw_s16(s32a, vget_high_s16(s16a));
    if (differs(&r, sizeof r, "00000080fcffffff33010000fe7f0080"))
      return 55;
  }
  { /* 56 */
    int64x2_t r = vmull_s32(vget_low_s32(s32a), vget_low_s32(s32b));
    if (differs(&r, sizeof r, "0000000000000040f1ffffffffffffff"))
      return 56;
  }
  { /* 57 */
    uint16x8_t r = vmull_high_u8(u8a, u8b);
    if (differs(&r, sizeof r, "7f000040ff3f204edc05fc0101fe3300"))
      return 57;
  }
  { /* 58 */
    poly16x8_t r = vmull_p8(vreinterpret_p8_u8(vget_low_u8(u8a)), vreinterpret_p8_u8(vget_low_u8(u8b)));
    if (differs(&r, sizeof r, "00000100fc013f0040000000f0012028"))
      return 58;
  }
  { /* 59 */
    int32x4_t r = vmlal_s16(s32a, vget_low_s16(s16a), vget_low_s16(s16b));
    if (differs(&r, sizeof r, "000000c0fdfffeff77a0feff00000080"))
      return 59;
  }
  { /* 60 */
    uint64x2_t r = vmlsl_high_u32(u64a, u32a, u32b);
    if (differs(&r, sizeof r, "ffffff7fffffffbf7b56341288a9cbed"))
      return 60;
  }
  { /* 61 */
    int64x2_t r = vqdmull_s32(vget_low_s32(s32a), vget_low_s32(s32b));
    if (differs(&r, sizeof r, "ffffffffffffff7fe2ffffffffffffff"))
      return 61;
  }
  { /* 62 */
    int32x4_t r = vqdmlal_s16(s32a, vget_low_s16(s16a), vget_low_s16(s16b));
    if (differs(&r, sizeof r, "fffffffffffffdffe740fdffffffff7f"))
      return 62;
  }
  { /* 63 */
    int8x8_t r = vaddhn_s16(s16a, s16b);
    if (differs(&r, sizeof r, "008000ff00ff81ff"))
      return 63;
  }
  { /* 64 */
    uint16x8_t r = vraddhn_high_u32(vget_low_u16(u16a), u32a, u32b);
    if (differs(&r, sizeof r, "00000100ff0000010000000000003412"))
      return 64;
  }
  { /* 65 */
    int16x8_t r = vmovl_s8(vget_low_s8(s8a));
    if (differs(&r, sizeof r, "80ff81ff9cffc0fff7fffeffffff0000"))
      return 65;
  }
  { /* 66 */
    uint8x8_t r = vmovn_u16(u16a);
    if (differs(&r, sizeof r, "0001ff00ff00feff"))
      return 66;
  }
  { /* 67 */
    int8x8_t r = vqmovn_s16(s16a);
    if (differs(&r, sizeof r, "808080ff00017f7f"))
      return 67;
  }
  { /* 68 */
    uint16x4_t r = vqmovun_s32(s32b);
    if (differs(&r, sizeof r, "0000030000000200"))
      return 68;
  }
  { /* 69 */
    uint32x4_t r = vqmovn_high_u64(vget_low_u32(u32a), u64a);
    if (differs(&r, sizeof r, "0000000001000000ffffffff03000000"))
      return 69;
  }
  { /* 70 */
    int8x16_t r = vpaddq_s8(s8a, s8b);
    if (differs(&r, sizeof r, "015cf5ff0348a4fdffa4010006385b81"))
      return 70;
  }
  { /* 71 */
    uint16x4_t r = vpmax_u16(vget_low_u16(u16a), vget_high_u16(u16b));
    if (differs(&r, sizeof r, "010000010080ffff"))
      return 71;
  }
  { /* 72 */
    float32x4_t r = vpminq_f32(f32a, f32c);
    if (differs(&r, sizeof r, "000020c04523c17f5ed032cf0000c0bf"))
      return 72;
  }
  { /* 73 */
    int16x8_t r = vpaddlq_s8(s8a);
    if (differs(&r, sizeof r, "01ff5cfff5ffffff03004800a400fd00"))
      return 73;
  }
  { /* 74 */
    uint32x4_t r = vpadalq_u16(u32a, u16a);
    if (differs(&r, sizeof r, "0100000000020000ffff0080fcff0100"))
      return 74;
  }
  { /* 75 */
    int32_t r = vaddvq_s32(s32a);
    if (differs(&r, sizeof r, "01000000"))
      return 75;
  }
  { /* 76 */
    uint16_t r = vaddlvq_u8(u8a);
    if (differs(&r, sizeof r, "d705"))
      return 76;
  }
  { /* 77 */
    int64_t r = vaddlvq_s32(s32a);
    if (differs(&r, sizeof r, "0100000000000000"))
      return 77;
  }
  { /* 78 */
    float32_t r = vaddvq_f32(f32c);
    if (differs(&r, sizeof r, "286beecd"))
      return 78;
  }
  { /* 79 */
    int8_t r = vmaxvq_s8(s8b);
    if (differs(&r, sizeof r, "7f"))
      return 79;
  }
  { /* 80 */
    uint16_t r = vminvq_u16(u16b);
    if (differs(&r, sizeof r, "0100"))
      return 80;
  }
  { /* 81 */
    float32_t r = vmaxvq_f32(f32a);
    if (differs(&r, sizeof r, "4523c17f"))
      return 81;
  }
  { /* 82 */
    float32_t r = vmaxnmvq_f32(f32a);
    if (differs(&r, sizeof r, "0000c03f"))
      return 82;
  }
  { /* 83 */
    float32_t r = vminnmvq_f32(vcombine_f32(vget_low_f32(f32c), vget_low_f32(vreinterpretq_f32_u32(vdupq_n_u32(0x7fc00000u)))));
    if (differs(&r, sizeof r, "5ed032cf"))
      return 83;
  }
  { /* 84 */
    int8x16_t r = vmaxq_s8(s8a, s8b);
    if (differs(&r, sizeof r, "7f81644003fe01000107094140647e7f"))
      return 84;
  }
  { /* 85 */
    uint32x4_t r = vminq_u32(u32a, u32b);
    if (differs(&r, sizeof r, "00000000010000000000008078563412"))
      return 85;
  }
  { /* 86 */
    float32x4_t r = vmaxq_f32(f32a, f32b);
    if (differs(&r, sizeof r, "0000807f000020c04523c17f00006040"))
      return 86;
  }
  { /* 87 */
    float32x4_t r = vminq_f32(f32a, vnegq_f32(f32a));
    if (differs(&r, sizeof r, "0000c0bf000020c04523c17f00000080"))
      return 87;
  }
  { /* 88 */
    float32x4_t r = vmaxnmq_f32(f32a, f32b);
    if (differs(&r, sizeof r, "0000807f000020c00000003f00006040"))
      return 88;
  }
  { /* 89 */
    float64x2_t r = vminnmq_f64(f64a, f64b);
    if (differs(&r, sizeof r, "00000000000004400000000000000080"))
      return 89;
  }
  { /* 90 */
    uint8x16_t r = vceqq_s8(s8a, s8b);
    if (differs(&r, sizeof r, "0000000000ff000000000000ff0000ff"))
      return 90;
  }
  { /* 91 */
    uint16x8_t r = vcgeq_u16(u16a, u16b);
    if (differs(&r, sizeof r, "0000ffffffffffffffffffffffffffff"))
      return 91;
  }
  { /* 92 */
    uint32x4_t r = vcltq_f32(f32a, f32b);
    if (differs(&r, sizeof r, "ffffffff0000000000000000ffffffff"))
      return 92;
  }
  { /* 93 */
    uint32x4_t r = vceqq_f32(f32a, f32a);
    if (differs(&r, sizeof r, "ffffffffffffffff00000000ffffffff"))
      return 93;
  }
  { /* 94 */
    uint64x2_t r = vcgtq_s64(s64a, s64b);
    if (differs(&r, sizeof r, "00000000000000000000000000000000"))
      return 94;
  }
  { /* 95 */
    uint8x16_t r = vcltzq_s8(s8a);
    if (differs(&r, sizeof r, "ffffffffffffff000000000000000000"))
      return 95;
  }
  { /* 96 */
    uint32x4_t r = vcgezq_f32(f32a);
    if (differs(&r, sizeof r, "ffffffff0000000000000000ffffffff"))
      return 96;
  }
  { /* 97 */
    uint16x8_t r = vtstq_s16(s16a, s16b);
    if (differs(&r, sizeof r, "ffff0000ffffffff00000000ffffffff"))
      return 97;
  }
  { /* 98 */
    uint32x4_t r = vcagtq_f32(f32a, f32c);
    if (differs(&r, sizeof r, "00000000000000000000000000000000"))
      return 98;
  }
  { /* 99 */
    uint8x16_t r = vbicq_u8(u8a, u8b);
    if (differs(&r, sizeof r, "00000006000f00247e008088f8fc0002"))
      return 99;
  }
  { /* 100 */
    int16x8_t r = vornq_s16(s16a, s16b);
    if (differs(&r, sizeof r, "fffffdffd7feffffeeff0f002c81ffff"))
      return 100;
  }
  { /* 101 */
    uint32x4_t r = veorq_u32(u32a, u32b);
    if (differs(&r, sizeof r, "ffffffff040000000100000087a9cbed"))
      return 101;
  }
  { /* 102 */
    int8x16_t r = vmvnq_s8(s8a);
    if (differs(&r, sizeof r, "7f7e633f080100fffefdf6c0bf9b8180"))
      return 102;
  }
  { /* 103 */
    float32x4_t r = vbslq_f32(vcgtq_f32(f32c, vdupq_n_f32(0)), f32a, f32b);
    if (differs(&r, sizeof r, "0000c03f000080ff4523c17f00006040"))
      return 103;
  }
  { /* 104 */
    int8x16_t r = vshlq_s8(s8a, s8b);
    if (differs(&r, sizeof r, "00ff0000b8fffe00000000000000f800"))
      return 104;
  }
  { /* 105 */
    uint16x8_t r = vshlq_u16(u16a, vdupq_n_s16(-3));
    if (differs(&r, sizeof r, "000000001f002000ff0f0010ff1fff1f"))
      return 105;
  }
  { /* 106 */
    int32x4_t r = vrshlq_s32(s32a, vdupq_n_s32(-31));
    if (differs(&r, sizeof r, "ffffffff000000000000000001000000"))
      return 106;
  }
  { /* 107 */
    int8x16_t r = vqshlq_s8(s8a, vdupq_n_s8(2));
    if (differs(&r, sizeof r, "80808080dcf8fc000408247f7f7f7f7f"))
      return 107;
  }
  { /* 108 */
    uint64x2_t r = vqrshlq_u64(u64a, vdupq_n_s64(-1));
    if (differs(&r, sizeof r, "00000000000000800200000000000000"))
      return 108;
  }
  { /* 109 */
    int16x8_t r = vshlq_n_s16(s16a, 15);
    if (differs(&r, sizeof r, "00000080000000800000008000000080"))
      return 109;
  }
  { /* 110 */
    int16x8_t r = vshrq_n_s16(s16a, 16);
    if (differs(&r, sizeof r, "ffffffffffffffff0000000000000000"))
      return 110;
  }
  { /* 111 */
    uint64x2_t r = vshrq_n_u64(u64a, 64);
    if (differs(&r, sizeof r, "00000000000000000000000000000000"))
      return 111;
  }
  { /* 112 */
    int32x4_t r = vrshrq_n_s32(s32a, 32);
    if (differs(&r, sizeof r, "00000000000000000000000000000000"))
      return 112;
  }
  { /* 113 */
    int8x16_t r = vsraq_n_s8(s8a, s8b, 3);
    if (differs(&r, sizeof r, "8f71a8c8f7fdffff0002074748677e8e"))
      return 113;
  }
  { /* 114 */
    uint8x16_t r = vrsraq_n_u8(u8a, u8b, 8);
    if (differs(&r, sizeof r, "01010307080f10657f8181c8fafe0003"))
      return 114;
  }
  { /* 115 */
    int8x16_t r = vqshlq_n_s8(s8a, 7);
    if (differs(&r, sizeof r, "80808080808080007f7f7f7f7f7f7f7f"))
      return 115;
  }
  { /* 116 */
    uint16x8_t r = vqshluq_n_s16(s16a, 3);
    if (differs(&r, sizeof r, "0000000000000000000008006009ffff"))
      return 116;
  }
  { /* 117 */
    int8x8_t r = vshrn_n_s16(s16a, 8);
    if (differs(&r, sizeof r, "8080feff0000017f"))
      return 117;
  }
  { /* 118 */
    uint8x8_t r = vrshrn_n_u16(u16a, 1);
    if (differs(&r, sizeof r, "000180800000ff00"))
      return 118;
  }
  { /* 119 */
    int16x4_t r = vqshrn_n_s32(s32a, 3);
    if (differs(&r, sizeof r, "0080ffff0000ff7f"))
      return 119;
  }
  { /* 120 */
    int32x2_t r = vqrshrn_n_s64(s64b, 32);
    if (differs(&r, sizeof r, "00000000ffffff7f"))
      return 120;
  }
  { /* 121 */
    uint8x8_t r = vqshrun_n_s16(s16a, 2);
    if (differs(&r, sizeof r, "0000000000004bff"))
      return 121;
  }
  { /* 122 */
    uint16x8_t r = vqrshrun_high_n_s32(vget_low_u16(u16a), s32b, 7);
    if (differs(&r, sizeof r, "00000100ff0000010000000000000000"))
      return 122;
  }
  { /* 123 */
    int16x8_t r = vshll_n_s8(vget_low_s8(s8a), 8);
    if (differs(&r, sizeof r, "00800081009c00c000f700fe00ff0000"))
      return 123;
  }
  { /* 124 */
    uint32x4_t r = vshll_high_n_u16(u16a, 5);
    if (differs(&r, sizeof r, "e0ff0f0000001000c0ff1f00e0ff1f00"))
      return 124;
  }
  { /* 125 */
    uint8x16_t r = vsliq_n_u8(u8a, u8b, 3);
    if (differs(&r, sizeof r, "f809f24f4007f8440f00f9203216ff8b"))
      return 125;
  }
  { /* 126 */
    int64x2_t r = vsriq_n_s64(s64a, s64b, 64);
    if (differs(&r, sizeof r, "00000000000000803930000000000000"))
      return 126;
  }
  { /* 127 */
    uint16x8_t r = vsriq_n_u16(u16a, u16b, 4);
    if (differs(&r, sizeof r, "ff0f0000000010000070008800f0ffff"))
      return 127;
  }
  { /* 128 */
    int8x16_t r = vqaddq_s8(s8a, s8b);
    if (differs(&r, sizeof r, "ff800000fafc00ff0009007f7f7f7f7f"))
      return 128;
  }
  { /* 129 */
    uint8x16_t r = vqsubq_u8(u8a, u8b);
    if (differs(&r, sizeof r, "00000000000f00007e000264f4fc0000"))
      return 129;
  }
  { /* 130 */
    int64x2_t r = vqaddq_s64(s64a, s64b);
    if (differs(&r, sizeof r, "0000000000000080ffffffffffffff7f"))
      return 130;
  }
  { /* 131 */
    uint64x2_t r = vqaddq_u64(u64a, u64b);
    if (differs(&r, sizeof r, "ffffffffffffffff0300000000000080"))
      return 131;
  }
  { /* 132 */
    int8x16_t r = vuqaddq_s8(s8a, u8a);
    if (differs(&r, sizeof r, "80829ec7ff0d0f647f7f7f7f7f7f7f7f"))
      return 132;
  }
  { /* 133 */
    int16x8_t r = vqdmulhq_s16(s16a, s16b);
    if (differs(&r, sizeof r, "ff7ffefffdff00000000ffff2b01fe7f"))
      return 133;
  }
  { /* 134 */
    int32x4_t r = vqrdmulhq_s32(s32a, s32b);
    if (differs(&r, sizeof r, "ffffff7f000000000000000002000000"))
      return 134;
  }
  { /* 135 */
    int16x8_t r = vqrdmulhq_lane_s16(s16a, vget_high_s16(s16b), 3);
    if (differs(&r, sizeof r, "01800280d4feffff000001002c01fe7f"))
      return 135;
  }
  { /* 136 */
    int16x8_t r = vqrdmlahq_s16(s16a, s16b, s16b);
    if (differs(&r, sizeof r, "00000180d7feffff00000100ff7fff7f"))
      return 136;
  }
  { /* 137 */
    int32x4_t r = vqrdmlshq_laneq_s32(s32a, s32b, s32b, 0);
    if (differs(&r, sizeof r, "00000080feffffff97eefeffffffff7f"))
      return 137;
  }
  { /* 138 */
    uint8x16_t r = vhaddq_u8(u8a, u8b);
    if (differs(&r, sizeof r, "7f01800808071796408080968080ff0a"))
      return 138;
  }
  { /* 139 */
    int16x8_t r = vrhaddq_s16(s16a, s16b);
    if (differs(&r, sizeof r, "008002c00000ffff0900f9ff9640ff7f"))
      return 139;
  }
  { /* 140 */
    int32x4_t r = vhsubq_s32(s32a, s32b);
    if (differs(&r, sizeof r, "00000000fcffffffbb880000feffff3f"))
      return 140;
  }
  { /* 141 */
    int32x4_t r = vcvtq_s32_f32(f32a);
    if (differs(&r, sizeof r, "01000000feffffff0000000000000000"))
      return 141;
  }
  { /* 142 */
    int32x4_t r = vcvtq_s32_f32(f32c);
    if (differs(&r, sizeof r, "ffffff7f0000008000000000ffffffff"))
      return 142;
  }
  { /* 143 */
    uint32x4_t r = vcvtq_u32_f32(f32c);
    if (differs(&r, sizeof r, "00f90295000000000000000000000000"))
      return 143;
  }
  { /* 144 */
    int32x4_t r = vcvtnq_s32_f32(vcombine_f32(vdup_n_f32(2.5f), vdup_n_f32(-3.5f)));
    if (differs(&r, sizeof r, "0200000002000000fcfffffffcffffff"))
      return 144;
  }
  { /* 145 */
    int32x4_t r = vcvtaq_s32_f32(vcombine_f32(vdup_n_f32(2.5f), vdup_n_f32(-3.5f)));
    if (differs(&r, sizeof r, "0300000003000000fcfffffffcffffff"))
      return 145;
  }
  { /* 146 */
    int32x4_t r = vcvtmq_s32_f32(f32a);
    if (differs(&r, sizeof r, "01000000fdffffff0000000000000000"))
      return 146;
  }
  { /* 147 */
    int32x4_t r = vcvtpq_s32_f32(f32a);
    if (differs(&r, sizeof r, "02000000feffffff0000000000000000"))
      return 147;
  }
  { /* 148 */
    int64x2_t r = vcvtq_s64_f64(f64b);
    if (differs(&r, sizeof r, "0000000000000000ffffffffffffff7f"))
      return 148;
  }
  { /* 149 */
    float32x4_t r = vcvtq_f32_s32(s32a);
    if (differs(&r, sizeof r, "000000cf0000a0c00000e0400000004f"))
      return 149;
  }
  { /* 150 */
    float32x4_t r = vcvtq_f32_u32(u32b);
    if (differs(&r, sizeof r, "0000804f0000a0400000004fb4a2914d"))
      return 150;
  }
  { /* 151 */
    float64x2_t r = vcvtq_f64_u64(u64a);
    if (differs(&r, sizeof r, "000000000000f0430000000000000840"))
      return 151;
  }
  { /* 152 */
    float32x4_t r = vcvtq_n_f32_s32(s32a, 16);
    if (differs(&r, sizeof r, "000000c70000a0b80000e03800000047"))
      return 152;
  }
  { /* 153 */
    int32x4_t r = vcvtq_n_s32_f32(f32a, 3);
    if (differs(&r, sizeof r, "0c000000ecffffff0000000000000000"))
      return 153;
  }
  { /* 154 */
    float32x2_t r = vcvt_f32_f64(f64b);
    if (differs(&r, sizeof r, "0000c07f0000807f"))
      return 154;
  }
  { /* 155 */
    float64x2_t r = vcvt_high_f64_f32(f32a);
    if (differs(&r, sizeof r, "000000a06824f87f0000000000000080"))
      return 155;
  }
  { /* 156 */
    float32x2_t r = vcvtx_f32_f64(vcombine_f64(vdup_n_f64(1.0 / 3.0), vdup_n_f64(1e300)));
    if (differs(&r, sizeof r, "abaaaa3effff7f7f"))
      return 156;
  }
  { /* 157 */
    float32x4_t r = vrndnq_f32(vcombine_f32(vdup_n_f32(2.5f), vdup_n_f32(-0.5f)));
    if (differs(&r, sizeof r, "00000040000000400000008000000080"))
      return 157;
  }
  { /* 158 */
    float32x4_t r = vrndaq_f32(vcombine_f32(vdup_n_f32(2.5f), vdup_n_f32(-0.5f)));
    if (differs(&r, sizeof r, "0000404000004040000080bf000080bf"))
      return 158;
  }
  { /* 159 */
    float32x4_t r = vrndq_f32(f32c);
    if (differs(&r, sizeof r, "f902154f5ed032cf00000000000080bf"))
      return 159;
  }
  { /* 160 */
    float32x4_t r = vrndmq_f32(f32a);
    if (differs(&r, sizeof r, "0000803f000040c04523c17f00000080"))
      return 160;
  }
  { /* 161 */
    float64x2_t r = vrndpq_f64(f64a);
    if (differs(&r, sizeof r, "00000000000008400000000000000080"))
      return 161;
  }
  { /* 162 */
    float32x4_t r = vrndxq_f32(f32c);
    if (differs(&r, sizeof r, "f902154f5ed032cf00000000000000c0"))
      return 162;
  }
  { /* 163 */
    float32x4_t r = vsqrtq_f32(vabsq_f32(f32c));
    if (differs(&r, sizeof r, "0050434741f45547f304353f71c49c3f"))
      return 163;
  }
  { /* 164 */
    float64x2_t r = vsqrtq_f64(f64b);
    if (differs(&r, sizeof r, "010000000000f87faf96502e358d135f"))
      return 164;
  }
  { /* 165 */
    float32x4_t r = vrecpeq_f32(f32c);
    if (differs(&r, sizeof r, "0080db2f0080b7af0000004000802abf"))
      return 165;
  }
  { /* 166 */
    float32x4_t r = vrsqrteq_f32(vabsq_f32(f32c));
    if (differs(&r, sizeof r, "0080a737008099370000b53f0000513f"))
      return 166;
  }
  { /* 167 */
    float64x2_t r = vrecpeq_f64(f64b);
    if (differs(&r, sizeof r, "010000000000f87f000000000070a501"))
      return 167;
  }
  { /* 168 */
    uint32x4_t r = vrecpeq_u32(u32b);
    if (differs(&r, sizeof r, "00000080ffffffff000080ffffffffff"))
      return 168;
  }
  { /* 169 */
    uint32x4_t r = vrsqrteq_u32(u32b);
    if (differs(&r, sizeof r, "00000080ffffffff000080b4ffffffff"))
      return 169;
  }
  { /* 170 */
    float32x4_t r = vrecpsq_f32(f32c, f32b);
    if (differs(&r, sizeof r, "000080ff000080ff0000e03f0000e840"))
      return 170;
  }
  { /* 171 */
    float32x4_t r = vrsqrtsq_f32(f32c, f32c);
    if (differs(&r, sizeof r, "ec782dded9cc79de0000b03f0000c03e"))
      return 171;
  }
  { /* 172 */
    int8x16_t r = vextq_s8(s8a, s8b, 13);
    if (differs(&r, sizeof r, "647e7f7f80644003fe01ffff07f74140"))
      return 172;
  }
  { /* 173 */
    uint16x8_t r = vrev64q_u16(u16a);
    if (differs(&r, sizeof r, "0001ff0001000000fffffeff0080ff7f"))
      return 173;
  }
  { /* 174 */
    int8x16_t r = vrev16q_s8(s8a);
    if (differs(&r, sizeof r, "8180c09cfef700ff02013f0964407f7e"))
      return 174;
  }
  { /* 175 */
    int8x16_t r = vzip1q_s8(s8a, s8b);
    if (differs(&r, sizeof r, "807f81809c64c040f703fefeff0100ff"))
      return 175;
  }
  { /* 176 */
    uint16x8_t r = vzip2q_u16(u16a, u16b);
    if (differs(&r, sizeof r, "ff7f010000800080feff0200ffffffff"))
      return 176;
  }
  { /* 177 */
    int32x4_t r = vuzp1q_s32(s32a, s32b);
    if (differs(&r, sizeof r, "00000080070000000000008090eefeff"))
      return 177;
  }
  { /* 178 */
    float32x4_t r = vuzp2q_f32(f32a, f32b);
    if (differs(&r, sizeof r, "000020c000000080000080ff00006040"))
      return 178;
  }
  { /* 179 */
    uint8x16_t r = vtrn1q_u8(u8a, u8b);
    if (differs(&r, sizeof r, "00ff02fe0808101f7f01817ffa06ffff"))
      return 179;
  }
  { /* 180 */
    int64x2_t r = vtrn2q_s64(s64a, s64b);
    if (differs(&r, sizeof r, "3930000000000000ffffffffffffff7f"))
      return 180;
  }
  { /* 181 */
    int16x4x2_t r = vzip_s16(vget_low_s16(s16a), vget_high_s16(s16b));
    if (differs(&r, sizeof r, "008011000180f0ffd4feff7fffffff7f"))
      return 181;
  }
  { /* 182 */
    uint8x16x2_t r = vuzpq_u8(u8a, u8b);
    if (differs(&r, sizeof r, "000208107f81fafffffe081f017f06ff01070f6480c8fe03010900c880640211"))
      return 182;
  }
  { /* 183 */
    float32x4x2_t r = vtrnq_f32(f32a, f32b);
    if (differs(&r, sizeof r, "0000c03f0000807f4523c17f0000003f000020c0000080ff0000008000006040"))
      return 183;
  }
  { /* 184 */
    uint8x8_t r = vtbl1_u8(vget_low_u8(u8a), vget_high_u8(u8b));
    if (differs(&r, sizeof r, "0100000010020000"))
      return 184;
  }
  { /* 185 */
    int8x8_t r = vtbx2_s8(vget_low_s8(s8b), (int8x8x2_t){{vget_low_s8(s8a), vget_high_s8(s8a)}}, vget_high_s8(s8b));
    if (differs(&r, sizeof r, "7f00644003fe9cff"))
      return 185;
  }
  { /* 186 */
    uint8x16_t r = vqtbl1q_u8(u8a, vandq_u8(u8b, vdupq_n_u8(31)));
    if (differs(&r, sizeof r, "000100807f00007f0100000810020000"))
      return 186;
  }
  { /* 187 */
    int8x16_t r = vqtbx2q_s8(s8b, (int8x16x2_t){{s8a, s8b}}, vandq_u8(u8b, vdupq_n_u8(63)));
    if (differs(&r, sizeof r, "7f81640201807f018180f741ff9c0280"))
      return 187;
  }
  { /* 188 */
    uint8x16_t r = vcntq_u8(u8a);
    if (differs(&r, sizeof r, "00010103010401030701020306070802"))
      return 188;
  }
  { /* 189 */
    int16x8_t r = vclzq_s16(s16a);
    if (differs(&r, sizeof r, "000000000000000010000f0007000100"))
      return 189;
  }
  { /* 190 */
    int32x4_t r = vclsq_s32(s32a);
    if (differs(&r, sizeof r, "000000001c0000001c00000000000000"))
      return 190;
  }
  { /* 191 */
    int8x16_t r = vclsq_s8(s8a);
    if (differs(&r, sizeof r, "00000001030607070605030100000000"))
      return 191;
  }
  { /* 192 */
    uint8x16_t r = vrbitq_u8(u8a);
    if (differs(&r, sizeof r, "008040e010f00826fe0181135f7fffc0"))
      return 192;
  }
  { /* 193 */
    int32x4_t r = vdotq_s32(s32a, s8a, s8b);
    if (differs(&r, sizeof r, "f0c8ff7fe3ffffffc20f0000885a0080"))
      return 193;
  }
  { /* 194 */
    uint32x4_t r = vdotq_u32(u32a, u8a, u8b);
    if (differs(&r, sizeof r, "3c020000515000009ece00800b060100"))
      return 194;
  }
  { /* 195 */
    int32x4_t r = vdotq_laneq_s32(s32b, s8a, s8b, 3);
    if (differs(&r, sizeof r, "13b2ff7f8bfdffff590effff8b5a0000"))
      return 195;
  }
  return 0;
}
