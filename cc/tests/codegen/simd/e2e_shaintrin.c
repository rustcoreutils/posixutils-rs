/*
 * SHA-NI intrinsics (<shaintrin.h>, through <immintrin.h>), checked against
 * the values gcc 13 -msha computed on the hardware. The instructions run
 * only where the CPU has them; elsewhere the fixture still compiles them.
 */
#include <immintrin.h>

static int same(__m128i v, unsigned int w0, unsigned int w1, unsigned int w2,
                unsigned int w3) {
    unsigned int w[4];
    _mm_storeu_si128((__m128i *)w, v);
    return w[0] == w0 && w[1] == w1 && w[2] == w2 && w[3] == w3;
}

/* Kept out of line, so that -O2 has registers to choose for each. */
__attribute__((noinline)) static int check(__m128i a, __m128i b, __m128i c) {
    if (!same(_mm_sha1msg1_epu32(a, b), 0x3d0e5b68, 0xf1c297a4, 0xffffffff, 0xffffffff))
        return 1;
    if (!same(_mm_sha1msg2_epu32(a, b), 0xc54cd45d, 0x7a1cb6d0, 0x1c7ad0b6, 0x94f2583e))
        return 2;
    if (!same(_mm_sha1nexte_epu32(a, b), 0xc3d2e1f0, 0x8796a5b4, 0x4b5a6978, 0xcf66fe95))
        return 3;
    if (!same(_mm_sha1rnds4_epu32(a, b, 0), 0x63398393, 0x6dfc76b3, 0xc17a2516, 0x933e756d))
        return 4;
    if (!same(_mm_sha1rnds4_epu32(a, b, 3), 0x7fc33856, 0x62d0a548, 0x965789ce, 0x705b4d41))
        return 5;
    if (!same(_mm_sha256msg1_epu32(a, b), 0x76d443a1, 0x3c3a86e4, 0xa92bbc5d, 0x42a50e70))
        return 6;
    if (!same(_mm_sha256msg2_epu32(a, b), 0xefd62c6c, 0xd216fc7f, 0x5d2bed89, 0xa2a7dc5d))
        return 7;
    if (!same(_mm_sha256rnds2_epu32(a, b, c), 0xb7651dc5, 0xc32a3eb1, 0x4b66c0d4, 0x9cde1cf4))
        return 8;
    /* The round constant can be any value, the first operand's included. */
    __m128i r = _mm_sha256rnds2_epu32(c, b, a);
    __m128i s = _mm_sha256rnds2_epu32(c, b, a);
    if (!same(r, (unsigned)_mm_cvtsi128_si32(s), (unsigned)_mm_extract_epi32(s, 1),
              (unsigned)_mm_extract_epi32(s, 2), (unsigned)_mm_extract_epi32(s, 3)))
        return 9;
    return 0;
}

int main(void) {
    __builtin_cpu_init();
    if (!__builtin_cpu_supports("sha"))
        return 0;
    __m128i a = _mm_set_epi32(0x01234567, (int)0x89abcdef, (int)0xfedcba98, 0x76543210);
    __m128i b = _mm_set_epi32(0x0f1e2d3c, 0x4b5a6978, (int)0x8796a5b4, (int)0xc3d2e1f0);
    __m128i c = _mm_set_epi32(0x13579bdf, 0x2468ace0, (int)0xdeadbeef, 0x0badf00d);
    return check(a, b, c);
}
