/*
 * Self-checking test of c17's bundled intrinsic headers: expected values
 * were captured from gcc running on the hardware (or under qemu).
 * Exits 0 on success, else the number of the first failing check.
 * SPDX-License-Identifier: MIT
 */
/*
 * Self-checking end-to-end test for the SSE4.1/SSE4.2 intrinsics
 * (smmintrin.h / nmmintrin.h). Each intrinsic -- and, for the string
 * compares, each (intrinsic, immediate) pair -- folds its results over a
 * fixed input set into an FNV-1a hash; the expected hashes were produced
 * by gcc 13 -msse4.2 on hardware. Exit status 0 on success, otherwise the
 * 1-based index (capped at 255) of the first failing group.
 */
#include <stdio.h>
#include <string.h>
#include <smmintrin.h>
#include <nmmintrin.h>

typedef union { __m128i i; __m128 f; __m128d d; unsigned char b[16]; unsigned int u[4]; unsigned long long q[2]; } V;

#define NG 86
static unsigned long long hash[NG];
static const char *const names[NG] = {"cvtepi8_epi16","cvtepi8_epi32","cvtepi8_epi64","cvtepi16_epi32","cvtepi16_epi64","cvtepi32_epi64","cvtepu8_epi16","cvtepu8_epi32","cvtepu8_epi64","cvtepu16_epi32","cvtepu16_epi64","cvtepu32_epi64","minpos_epu16","stream_load_si128","test_all_ones","extract_epi8","insert_epi8","extract_epi32","extract_ps","insert_epi32","_MM_EXTRACT_FLOAT","_MM_PICK_OUT_PS","extract_epi64","insert_epi64","cmpeq_epi64","cmpgt_epi64","min_epi8","max_epi8","min_epu16","max_epu16","min_epi32","max_epi32","min_epu32","max_epu32","mullo_epi32","mul_epi32","packus_epi32","blendv_epi8","blendv_ps","blendv_pd","testz_si128","testc_si128","testnzc_si128","test_all_zeros","test_mix_ones_zeros","mpsadbw_epu8","blend_epi16","blend_ps","blend_pd","crc32_u8","crc32_u16","crc32_u32","crc32_u64","round_ps","round_ss","floor_ps","ceil_ps","floor_ss","ceil_ss","round_ps_cur","round_ss_cur","dp_ps","insert_ps","round_pd","round_sd","floor_pd","ceil_pd","floor_sd","ceil_sd","round_pd_cur","round_sd_cur","dp_pd","cmpistrm","cmpistri","cmpistra","cmpistrc","cmpistro","cmpistrs","cmpistrz","cmpestrm","cmpestri","cmpestra","cmpestrc","cmpestro","cmpestrs","cmpestrz"};

static void hbytes(int g, const void *p, int n)
{
    const unsigned char *c = p;
    int k;
    if (!hash[g])
        hash[g] = 0xcbf29ce484222325ull;
    for (k = 0; k < n; k++)
        hash[g] = (hash[g] ^ c[k]) * 0x100000001b3ull;
}
static void H(int g, V v) { hbytes(g, v.b, 16); }
static void HS(int g, long long x) { hbytes(g, &x, 8); }
static void setrc(int rc)
{
    unsigned int m;
    __asm__ volatile("stmxcsr %0" : "=m"(m));
    m = (m & ~0x6000u) | ((unsigned int)rc << 13);
    __asm__ volatile("ldmxcsr %0" : : "m"(m));
}
static V IV[] = {{.b={0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00}},
{.b={0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff}},
{.b={0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f}},
{.b={0x80,0x7f,0x00,0xff,0x01,0xfe,0x80,0x00,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x80}},
{.b={0x00,0x00,0x01,0x00,0xff,0xff,0x00,0x80,0x34,0x12,0xcd,0xab,0x00,0x00,0xff,0x7f}},
{.b={0x75,0xe4,0xe4,0xbc,0x8a,0xac,0x14,0xe4,0xc7,0x93,0x3d,0x3a,0x27,0xaa,0x35,0x00}},
{.b={0xad,0x93,0x88,0x36,0x6b,0x9f,0x40,0xd4,0x11,0xb2,0x4b,0x27,0x8c,0xfe,0x30,0xe6}},
{.b={0x56,0x05,0xe0,0x83,0x6f,0xc4,0x2d,0xc0,0x5b,0xf5,0x20,0xc9,0xa3,0x9c,0x60,0x30}}};
#define NIV 8
static V FV[] = {{.u={0x00000000u,0x80000000u,0x3f000000u,0xbf000000u}},
{.u={0x40200000u,0xc0200000u,0x7f800000u,0xff800000u}},
{.u={0x7fc00001u,0x7f800001u,0xffc00003u,0x00000001u}},
{.u={0x4b000001u,0x3effffffu,0xbfc00000u,0x40490fdbu}},
{.u={0x5f000000u,0xdf000000u,0x3f800001u,0x4a800001u}}};
#define NFV 5
static V DV[] = {{.q={0x0000000000000000ull,0x8000000000000000ull}},
{.q={0x3fe0000000000000ull,0xbff8000000000000ull}},
{.q={0x4004000000000000ull,0x7ff0000000000000ull}},
{.q={0x7ff8000000000001ull,0x7ff0000000000001ull}},
{.q={0xfff8000000000002ull,0x0000000000000001ull}},
{.q={0x4330000000000001ull,0x3fdfffffffffffffull}},
{.q={0x43e0000000000000ull,0xc00921fb54442d18ull}}};
#define NDV 7
static V SV[] = {{.b={0x68,0x65,0x6c,0x6c,0x6f,0x20,0x77,0x6f,0x72,0x6c,0x64,0x21,0x61,0x62,0x63,0x64}},
{.b={0x77,0x6f,0x72,0x6c,0x64,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00}},
{.b={0x61,0x7a,0x41,0x5a,0x30,0x39,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00}},
{.b={0x48,0x65,0x6c,0x6c,0x6f,0x2c,0x20,0x57,0x6f,0x72,0x6c,0x64,0x20,0x31,0x32,0x33}},
{.b={0x61,0x62,0x63,0x00,0x64,0x65,0x66,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69}},
{.b={0x63,0x61,0x62,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00}},
{.b={0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00}},
{.b={0x80,0xff,0x7f,0x01,0x81,0xfe,0x00,0x05,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00}},
{.b={0x81,0x90,0xf0,0xff,0x01,0x7f,0x80,0x81,0x90,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b}},
{.b={0x01,0x00,0x02,0x00,0x03,0x00,0x00,0x00,0x04,0x00,0x05,0x00,0x06,0x00,0x07,0x00}},
{.b={0x02,0x00,0x03,0x00,0x01,0x00,0x09,0x00,0x00,0x01,0x00,0x00,0x00,0x00,0x00,0x00}},
{.b={0x00,0x80,0xff,0x7f,0x01,0x00,0xff,0xff,0x10,0x00,0x20,0x00,0x00,0x00,0x00,0x00}},
{.b={0x78,0x79,0x7a,0x78,0x79,0x7a,0x78,0x79,0x7a,0x78,0x79,0x7a,0x78,0x79,0x7a,0x78}},
{.b={0x7a,0x78,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00}}};
#define NSV 14
static const int LENS[] = {0, 3, 8, 16, 17, -5, -16, -2147483647 - 1};
#define NL 8
static void t_int(void) {
 int i, j; V r;
 for (i = 0; i < NIV; i++) {
  r.i = (__m128i)(_mm_cvtepi8_epi16(IV[i].i)); H(0, r);
  r.i = (__m128i)(_mm_cvtepi8_epi32(IV[i].i)); H(1, r);
  r.i = (__m128i)(_mm_cvtepi8_epi64(IV[i].i)); H(2, r);
  r.i = (__m128i)(_mm_cvtepi16_epi32(IV[i].i)); H(3, r);
  r.i = (__m128i)(_mm_cvtepi16_epi64(IV[i].i)); H(4, r);
  r.i = (__m128i)(_mm_cvtepi32_epi64(IV[i].i)); H(5, r);
  r.i = (__m128i)(_mm_cvtepu8_epi16(IV[i].i)); H(6, r);
  r.i = (__m128i)(_mm_cvtepu8_epi32(IV[i].i)); H(7, r);
  r.i = (__m128i)(_mm_cvtepu8_epi64(IV[i].i)); H(8, r);
  r.i = (__m128i)(_mm_cvtepu16_epi32(IV[i].i)); H(9, r);
  r.i = (__m128i)(_mm_cvtepu16_epi64(IV[i].i)); H(10, r);
  r.i = (__m128i)(_mm_cvtepu32_epi64(IV[i].i)); H(11, r);
  r.i = (__m128i)(_mm_minpos_epu16(IV[i].i)); H(12, r);
  r.i = (__m128i)(_mm_stream_load_si128(&IV[i].i)); H(13, r);
  HS(14, (long long)(_mm_test_all_ones(IV[i].i)));
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 0))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, 511, 0)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 1))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, -1, 1)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 2))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, 128, 2)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 3))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, 127, 3)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 4))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, 511, 4)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 5))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, -1, 5)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 6))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, 128, 6)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 7))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, 127, 7)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 8))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, 511, 8)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 9))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, -1, 9)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 10))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, 128, 10)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 11))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, 127, 11)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 12))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, 511, 12)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 13))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, -1, 13)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 14))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, 128, 14)); H(16, r);
  HS(15, (long long)(_mm_extract_epi8(IV[i].i, 15))); r.i = (__m128i)(_mm_insert_epi8(IV[i].i, 127, 15)); H(16, r);
  HS(17, (long long)(_mm_extract_epi32(IV[i].i, 0))); HS(18, (long long)(_mm_extract_ps(IV[i].f, 0))); r.i = (__m128i)(_mm_insert_epi32(IV[i].i, -5, 0)); H(19, r);
  { float fx; _MM_EXTRACT_FLOAT(fx, IV[i].f, 0); r.u[0] = 0; memcpy(&r.u[0], &fx, 4); HS(20, r.u[0]); }
  r.i = (__m128i)(_MM_PICK_OUT_PS(IV[i].f, 0)); H(21, r);
  HS(17, (long long)(_mm_extract_epi32(IV[i].i, 1))); HS(18, (long long)(_mm_extract_ps(IV[i].f, 1))); r.i = (__m128i)(_mm_insert_epi32(IV[i].i, 2147483647, 1)); H(19, r);
  { float fx; _MM_EXTRACT_FLOAT(fx, IV[i].f, 1); r.u[0] = 0; memcpy(&r.u[0], &fx, 4); HS(20, r.u[0]); }
  r.i = (__m128i)(_MM_PICK_OUT_PS(IV[i].f, 1)); H(21, r);
  HS(17, (long long)(_mm_extract_epi32(IV[i].i, 2))); HS(18, (long long)(_mm_extract_ps(IV[i].f, 2))); r.i = (__m128i)(_mm_insert_epi32(IV[i].i, -2147483647, 2)); H(19, r);
  { float fx; _MM_EXTRACT_FLOAT(fx, IV[i].f, 2); r.u[0] = 0; memcpy(&r.u[0], &fx, 4); HS(20, r.u[0]); }
  r.i = (__m128i)(_MM_PICK_OUT_PS(IV[i].f, 2)); H(21, r);
  HS(17, (long long)(_mm_extract_epi32(IV[i].i, 3))); HS(18, (long long)(_mm_extract_ps(IV[i].f, 3))); r.i = (__m128i)(_mm_insert_epi32(IV[i].i, 12345, 3)); H(19, r);
  { float fx; _MM_EXTRACT_FLOAT(fx, IV[i].f, 3); r.u[0] = 0; memcpy(&r.u[0], &fx, 4); HS(20, r.u[0]); }
  r.i = (__m128i)(_MM_PICK_OUT_PS(IV[i].f, 3)); H(21, r);
  HS(22, (long long)(_mm_extract_epi64(IV[i].i, 0))); r.i = (__m128i)(_mm_insert_epi64(IV[i].i, -77LL, 0)); H(23, r);
  HS(22, (long long)(_mm_extract_epi64(IV[i].i, 1))); r.i = (__m128i)(_mm_insert_epi64(IV[i].i, 81985529216486895LL, 1)); H(23, r);
  for (j = 0; j < NIV; j++) {
   r.i = (__m128i)(_mm_cmpeq_epi64(IV[i].i, IV[j].i)); H(24, r);
   r.i = (__m128i)(_mm_cmpgt_epi64(IV[i].i, IV[j].i)); H(25, r);
   r.i = (__m128i)(_mm_min_epi8(IV[i].i, IV[j].i)); H(26, r);
   r.i = (__m128i)(_mm_max_epi8(IV[i].i, IV[j].i)); H(27, r);
   r.i = (__m128i)(_mm_min_epu16(IV[i].i, IV[j].i)); H(28, r);
   r.i = (__m128i)(_mm_max_epu16(IV[i].i, IV[j].i)); H(29, r);
   r.i = (__m128i)(_mm_min_epi32(IV[i].i, IV[j].i)); H(30, r);
   r.i = (__m128i)(_mm_max_epi32(IV[i].i, IV[j].i)); H(31, r);
   r.i = (__m128i)(_mm_min_epu32(IV[i].i, IV[j].i)); H(32, r);
   r.i = (__m128i)(_mm_max_epu32(IV[i].i, IV[j].i)); H(33, r);
   r.i = (__m128i)(_mm_mullo_epi32(IV[i].i, IV[j].i)); H(34, r);
   r.i = (__m128i)(_mm_mul_epi32(IV[i].i, IV[j].i)); H(35, r);
   r.i = (__m128i)(_mm_packus_epi32(IV[i].i, IV[j].i)); H(36, r);
   r.i = (__m128i)(_mm_blendv_epi8(IV[i].i, IV[j].i, IV[(i+j)%NIV].i)); H(37, r);
   r.i = (__m128i)(_mm_blendv_ps(IV[i].f, IV[j].f, IV[(i+j+1)%NIV].f)); H(38, r);
   r.i = (__m128i)(_mm_blendv_pd(IV[i].d, IV[j].d, IV[(i+j+1)%NIV].d)); H(39, r);
   HS(40, (long long)(_mm_testz_si128(IV[i].i, IV[j].i)));
   HS(41, (long long)(_mm_testc_si128(IV[i].i, IV[j].i)));
   HS(42, (long long)(_mm_testnzc_si128(IV[i].i, IV[j].i)));
   HS(43, (long long)(_mm_test_all_zeros(IV[i].i, IV[j].i)));
   HS(44, (long long)(_mm_test_mix_ones_zeros(IV[i].i, IV[j].i)));
   r.i = (__m128i)(_mm_mpsadbw_epu8(IV[i].i, IV[j].i, 0)); H(45, r);
   r.i = (__m128i)(_mm_mpsadbw_epu8(IV[i].i, IV[j].i, 1)); H(45, r);
   r.i = (__m128i)(_mm_mpsadbw_epu8(IV[i].i, IV[j].i, 2)); H(45, r);
   r.i = (__m128i)(_mm_mpsadbw_epu8(IV[i].i, IV[j].i, 3)); H(45, r);
   r.i = (__m128i)(_mm_mpsadbw_epu8(IV[i].i, IV[j].i, 4)); H(45, r);
   r.i = (__m128i)(_mm_mpsadbw_epu8(IV[i].i, IV[j].i, 5)); H(45, r);
   r.i = (__m128i)(_mm_mpsadbw_epu8(IV[i].i, IV[j].i, 6)); H(45, r);
   r.i = (__m128i)(_mm_mpsadbw_epu8(IV[i].i, IV[j].i, 7)); H(45, r);
   r.i = (__m128i)(_mm_blend_epi16(IV[i].i, IV[j].i, 0)); H(46, r);
   r.i = (__m128i)(_mm_blend_epi16(IV[i].i, IV[j].i, 1)); H(46, r);
   r.i = (__m128i)(_mm_blend_epi16(IV[i].i, IV[j].i, 128)); H(46, r);
   r.i = (__m128i)(_mm_blend_epi16(IV[i].i, IV[j].i, 170)); H(46, r);
   r.i = (__m128i)(_mm_blend_epi16(IV[i].i, IV[j].i, 85)); H(46, r);
   r.i = (__m128i)(_mm_blend_epi16(IV[i].i, IV[j].i, 255)); H(46, r);
   r.i = (__m128i)(_mm_blend_epi16(IV[i].i, IV[j].i, 60)); H(46, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 0)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 1)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 2)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 3)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 4)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 5)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 6)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 7)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 8)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 9)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 10)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 11)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 12)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 13)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 14)); H(47, r);
   r.i = (__m128i)(_mm_blend_ps(IV[i].f, IV[j].f, 15)); H(47, r);
   r.i = (__m128i)(_mm_blend_pd(IV[i].d, IV[j].d, 0)); H(48, r);
   r.i = (__m128i)(_mm_blend_pd(IV[i].d, IV[j].d, 1)); H(48, r);
   r.i = (__m128i)(_mm_blend_pd(IV[i].d, IV[j].d, 2)); H(48, r);
   r.i = (__m128i)(_mm_blend_pd(IV[i].d, IV[j].d, 3)); H(48, r);
   HS(49, (long long)(_mm_crc32_u8(IV[i].u[0], IV[j].b[3])));
   HS(50, (long long)(_mm_crc32_u16(IV[i].u[1], (unsigned short)IV[j].u[2])));
   HS(51, (long long)(_mm_crc32_u32(IV[i].u[2], IV[j].u[3])));
   HS(52, (long long)(_mm_crc32_u64(IV[i].q[0], IV[j].q[1])));
  }
 }
}
static void t_flt(void) {
 int i, j, rc; V r;
 for (i = 0; i < NFV; i++) {
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 0)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 0)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 1)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 1)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 2)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 2)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 3)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 3)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 4)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 4)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 5)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 5)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 6)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 6)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 7)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 7)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 8)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 8)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 9)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 9)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 10)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 10)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 11)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 11)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 12)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 12)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 13)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 13)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 14)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 14)); H(54, r);
  r.i = (__m128i)(_mm_round_ps(FV[i].f, 15)); H(53, r); r.i = (__m128i)(_mm_round_ss(FV[(i+1)%NFV].f, FV[i].f, 15)); H(54, r);
  r.i = (__m128i)(_mm_floor_ps(FV[i].f)); H(55, r); r.i = (__m128i)(_mm_ceil_ps(FV[i].f)); H(56, r);
  r.i = (__m128i)(_mm_floor_ss(FV[0].f, FV[i].f)); H(57, r); r.i = (__m128i)(_mm_ceil_ss(FV[0].f, FV[i].f)); H(58, r);
  for (rc = 0; rc < 4; rc++) { setrc(rc); r.f = _mm_round_ps(FV[i].f, _MM_FROUND_CUR_DIRECTION); setrc(0); H(59, r);
   setrc(rc); r.f = _mm_round_ss(FV[i].f, FV[(i+3)%NFV].f, _MM_FROUND_NEARBYINT); setrc(0); H(60, r); }
  for (j = 0; j < NFV; j++) {
   for (rc = 0; rc < 4; rc++) {
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 0)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 1)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 2)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 3)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 4)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 5)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 6)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 7)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 8)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 9)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 10)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 11)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 12)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 13)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 14)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 15)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 17)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 18)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 20)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 21)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 24)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 26)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 31)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 33)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 34)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 36)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 37)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 40)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 42)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 47)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 49)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 50)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 52)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 53)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 56)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 58)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 63)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 65)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 66)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 68)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 69)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 72)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 74)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 79)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 81)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 82)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 84)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 85)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 88)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 90)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 95)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 97)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 98)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 100)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 101)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 104)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 106)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 111)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 113)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 114)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 116)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 117)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 120)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 122)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 127)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 129)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 130)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 132)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 133)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 136)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 138)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 143)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 145)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 146)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 148)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 149)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 152)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 154)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 159)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 161)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 162)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 164)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 165)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 168)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 170)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 175)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 177)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 178)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 180)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 181)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 184)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 186)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 191)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 193)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 194)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 196)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 197)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 200)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 202)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 207)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 209)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 210)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 212)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 213)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 216)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 218)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 223)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 225)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 226)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 228)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 229)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 232)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 234)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 239)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 241)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 242)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 244)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 245)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 248)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 250)); H(61, r);
    r.i = (__m128i)(_mm_dp_ps(FV[(i+rc)%NFV].f, FV[j].f, 255)); H(61, r);
   }
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 0)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 5)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 10)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 15)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 20)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 25)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 30)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 35)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 40)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 45)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 50)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 55)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 60)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 65)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 70)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 75)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 80)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 85)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 90)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 95)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 100)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 105)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 110)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 115)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 120)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 125)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 130)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 135)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 140)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 145)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 150)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 155)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 160)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 165)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 170)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 175)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 180)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 185)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 190)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 195)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 200)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 205)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 210)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 215)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 220)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 225)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 230)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 235)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 240)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 245)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 250)); H(62, r);
   r.i = (__m128i)(_mm_insert_ps(FV[i].f, FV[j].f, 255)); H(62, r);
  }
 }
 for (i = 0; i < NDV; i++) {
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 0)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 0)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 1)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 1)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 2)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 2)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 3)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 3)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 4)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 4)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 5)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 5)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 6)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 6)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 7)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 7)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 8)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 8)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 9)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 9)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 10)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 10)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 11)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 11)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 12)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 12)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 13)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 13)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 14)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 14)); H(64, r);
  r.i = (__m128i)(_mm_round_pd(DV[i].d, 15)); H(63, r); r.i = (__m128i)(_mm_round_sd(DV[(i+1)%NDV].d, DV[i].d, 15)); H(64, r);
  r.i = (__m128i)(_mm_floor_pd(DV[i].d)); H(65, r); r.i = (__m128i)(_mm_ceil_pd(DV[i].d)); H(66, r);
  r.i = (__m128i)(_mm_floor_sd(DV[0].d, DV[i].d)); H(67, r); r.i = (__m128i)(_mm_ceil_sd(DV[0].d, DV[i].d)); H(68, r);
  for (rc = 0; rc < 4; rc++) { setrc(rc); r.d = _mm_round_pd(DV[i].d, _MM_FROUND_RINT); setrc(0); H(69, r);
   setrc(rc); r.d = _mm_round_sd(DV[i].d, DV[(i+3)%NDV].d, 12); setrc(0); H(70, r); }
  for (j = 0; j < NDV; j++) {
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 0)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 1)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 2)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 3)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 16)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 17)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 18)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 19)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 32)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 33)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 34)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 35)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 48)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 49)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 50)); H(71, r);
   r.i = (__m128i)(_mm_dp_pd(DV[i].d, DV[j].d, 51)); H(71, r);
  }
 }
}
#define TS(M) static void ts##M(void) { int i, j, k; V r; \
 for (i = 0; i < NSV; i++) for (j = 0; j < NSV; j++) { \
  r.i = _mm_cmpistrm(SV[i].i, SV[j].i, M); H(72, r); \
  HS(73, _mm_cmpistri(SV[i].i, SV[j].i, M)); \
  HS(74, _mm_cmpistra(SV[i].i, SV[j].i, M)); \
  HS(75, _mm_cmpistrc(SV[i].i, SV[j].i, M)); \
  HS(76, _mm_cmpistro(SV[i].i, SV[j].i, M)); \
  HS(77, _mm_cmpistrs(SV[i].i, SV[j].i, M)); \
  HS(78, _mm_cmpistrz(SV[i].i, SV[j].i, M)); \
  for (k = 0; k < NL; k++) { int la = LENS[k], lb = LENS[(k * 3 + i + j) % NL]; \
   r.i = _mm_cmpestrm(SV[i].i, la, SV[j].i, lb, M); H(79, r); \
   HS(80, _mm_cmpestri(SV[i].i, la, SV[j].i, lb, M)); \
   HS(81, _mm_cmpestra(SV[i].i, la, SV[j].i, lb, M)); \
   HS(82, _mm_cmpestrc(SV[i].i, la, SV[j].i, lb, M)); \
   HS(83, _mm_cmpestro(SV[i].i, la, SV[j].i, lb, M)); \
   HS(84, _mm_cmpestrs(SV[i].i, la, SV[j].i, lb, M)); \
   HS(85, _mm_cmpestrz(SV[i].i, la, SV[j].i, lb, M)); \
  } } }
TS(0) TS(1) TS(2) TS(3) TS(4) TS(5) TS(6) TS(7)
TS(8) TS(9) TS(10) TS(11) TS(12) TS(13) TS(14) TS(15)
TS(16) TS(17) TS(18) TS(19) TS(20) TS(21) TS(22) TS(23)
TS(24) TS(25) TS(26) TS(27) TS(28) TS(29) TS(30) TS(31)
TS(32) TS(33) TS(34) TS(35) TS(36) TS(37) TS(38) TS(39)
TS(40) TS(41) TS(42) TS(43) TS(44) TS(45) TS(46) TS(47)
TS(48) TS(49) TS(50) TS(51) TS(52) TS(53) TS(54) TS(55)
TS(56) TS(57) TS(58) TS(59) TS(60) TS(61) TS(62) TS(63)
TS(64) TS(65) TS(66) TS(67) TS(68) TS(69) TS(70) TS(71)
TS(72) TS(73) TS(74) TS(75) TS(76) TS(77) TS(78) TS(79)
TS(80) TS(81) TS(82) TS(83) TS(84) TS(85) TS(86) TS(87)
TS(88) TS(89) TS(90) TS(91) TS(92) TS(93) TS(94) TS(95)
TS(96) TS(97) TS(98) TS(99) TS(100) TS(101) TS(102) TS(103)
TS(104) TS(105) TS(106) TS(107) TS(108) TS(109) TS(110) TS(111)
TS(112) TS(113) TS(114) TS(115) TS(116) TS(117) TS(118) TS(119)
TS(120) TS(121) TS(122) TS(123) TS(124) TS(125) TS(126) TS(127)
#ifndef GEN
static const unsigned long long expect[NG] = {
0xa83c0e9274e6acf2ull,
0xdc70c590e1c9d20dull,
0x500d01a71b069607ull,
0x0f29d79c6b4576b0ull,
0xc324eb6d471dc141ull,
0xba496837f9561560ull,
0x982f3a786a4be38aull,
0x351b16dd6c6493edull,
0x1946246cb7114ac7ull,
0x554cf00aa32450a4ull,
0xf5e34abae9f95c7full,
0x6f3705677b447504ull,
0xb31d7d82757257d6ull,
0xe9b5e97f99d042f4ull,
0x9f29f986becfad44ull,
0x0a2930cda552c51aull,
0x4e2dafedc08fc854ull,
0xbf27f6ddc6586c38ull,
0xbf27f6ddc6586c38ull,
0xbcd6d082c40aebbcull,
0x94b4cd68fa7240b4ull,
0x39a88eafe1b01a74ull,
0xe9b5e97f99d042f4ull,
0x244b5c42f6bfc6b8ull,
0xaf0b42942c6640a5ull,
0x46a9b88294ebc765ull,
0xb436cba0e55925b8ull,
0x4e9fed107bd41d10ull,
0xc750e340ef2f6aa8ull,
0xf392946e47049810ull,
0xcf55b783532c7aa4ull,
0x4b44db7cad59a374ull,
0xa90084bcd9a17f08ull,
0x8d0505a801c31580ull,
0xf1a612ac6491cc2eull,
0xe072cc8429afb992ull,
0x068653c55b1c3e05ull,
0x648de9c651aeede5ull,
0xa686fd2b0847cde1ull,
0x6ee3d17ab05e21d9ull,
0xcdf2eb4ad3445ea4ull,
0x1f853af69bd4d104ull,
0xd81ada1dc93d4a85ull,
0xcdf2eb4ad3445ea4ull,
0xd81ada1dc93d4a85ull,
0x70bc659891f3afa9ull,
0x0816dac505b42a81ull,
0x5a261cef64189155ull,
0x72b8c588bde32545ull,
0x4cda89df07b7b739ull,
0x96970c4b069ba73dull,
0xb832d081cde66dd5ull,
0x9842425e59242d65ull,
0x1de8325709e4b9c9ull,
0x2dbc634be0c7fc25ull,
0x147b020401f5a76dull,
0xc96f5f14a1b34c9full,
0xc73cede9784871c4ull,
0xa1ca8d2f09398904ull,
0xadcbc450d0868fc2ull,
0x196a21e1cb764f25ull,
0x3f09d88f7dcb3f05ull,
0x136083d75a438010ull,
0x0b994ac56ca959e5ull,
0x83292770dbb9f665ull,
0x19fcc5af6e36d6eaull,
0x99c02bc2a0fb822aull,
0x63242dab1a6bdec3ull,
0xf03f1d0b46a05beeull,
0x1f0c29534244b13cull,
0xd8c8f326efcfee70ull,
0x52768c48eccfc70dull,
0xdac0b6e9548a0e65ull,
0x75638ddc15ada424ull,
0xa4743b7bd4094325ull,
0xddd9c5eba2a14325ull,
0xa1d41acf960a3aa5ull,
0xcfc658c22389bb25ull,
0xbde34953ec7bb325ull,
0x6fb8f0b5e1f477c1ull,
0x30a46f44d77056a4ull,
0x6551064e2c1b2325ull,
0xfcc96de6fec1a025ull,
0xd769fbc04428d2a5ull,
0x4e390403aa072325ull,
0x36948bce7b6e4325ull,
};
#endif
int main(void)
{
    int g;

    t_int();
    t_flt();
    ts0();
    ts1();
    ts2();
    ts3();
    ts4();
    ts5();
    ts6();
    ts7();
    ts8();
    ts9();
    ts10();
    ts11();
    ts12();
    ts13();
    ts14();
    ts15();
    ts16();
    ts17();
    ts18();
    ts19();
    ts20();
    ts21();
    ts22();
    ts23();
    ts24();
    ts25();
    ts26();
    ts27();
    ts28();
    ts29();
    ts30();
    ts31();
    ts32();
    ts33();
    ts34();
    ts35();
    ts36();
    ts37();
    ts38();
    ts39();
    ts40();
    ts41();
    ts42();
    ts43();
    ts44();
    ts45();
    ts46();
    ts47();
    ts48();
    ts49();
    ts50();
    ts51();
    ts52();
    ts53();
    ts54();
    ts55();
    ts56();
    ts57();
    ts58();
    ts59();
    ts60();
    ts61();
    ts62();
    ts63();
    ts64();
    ts65();
    ts66();
    ts67();
    ts68();
    ts69();
    ts70();
    ts71();
    ts72();
    ts73();
    ts74();
    ts75();
    ts76();
    ts77();
    ts78();
    ts79();
    ts80();
    ts81();
    ts82();
    ts83();
    ts84();
    ts85();
    ts86();
    ts87();
    ts88();
    ts89();
    ts90();
    ts91();
    ts92();
    ts93();
    ts94();
    ts95();
    ts96();
    ts97();
    ts98();
    ts99();
    ts100();
    ts101();
    ts102();
    ts103();
    ts104();
    ts105();
    ts106();
    ts107();
    ts108();
    ts109();
    ts110();
    ts111();
    ts112();
    ts113();
    ts114();
    ts115();
    ts116();
    ts117();
    ts118();
    ts119();
    ts120();
    ts121();
    ts122();
    ts123();
    ts124();
    ts125();
    ts126();
    ts127();
#ifdef GEN
    for (g = 0; g < NG; g++)
        printf("0x%016llxull,\n", hash[g]);
    return 0;
#else
    for (g = 0; g < NG; g++)
        if (hash[g] != expect[g]) {
            printf("FAIL %s: got %016llx want %016llx\n", names[g], hash[g], expect[g]);
            return g + 1 > 255 ? 255 : g + 1;
        }
    printf("ok %d groups\n", NG);
    return 0;
#endif
}
