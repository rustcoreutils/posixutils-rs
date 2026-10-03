/*
 * Self-checking test of c17's bundled intrinsic headers: expected values
 * were captured from gcc running on the hardware (or under qemu).
 * Exits 0 on success, else the number of the first failing check.
 * SPDX-License-Identifier: MIT
 */
/*
 * Self-checking end-to-end test of every SSE3 (pmmintrin.h) and SSSE3
 * (tmmintrin.h) intrinsic. Expected values were produced by gcc 13 with
 * -mssse3 and its own headers (build with -DGEN to print them again).
 * Returns 0 on success, or the 1-based number of the first failing check.
 */
#include <tmmintrin.h>
#include <stdio.h>
#include <string.h>

typedef union { __m128i i; __m128 f; __m128d d; unsigned long long q[2]; } U;
typedef union { __m64 m; unsigned long long q; } M;

static const unsigned long long E[][2] = {
    {0xffc000007fc00001ULL, 0x00000000bf000000ULL},
    {0xff8000007fe00000ULL, 0xffc000023f800000ULL},
    {0x7fe0000040000000ULL, 0x7fc000013fc00000ULL},
    {0x7fc000013fc00000ULL, 0x80000000ffc00002ULL},
    {0x7fe0000040000000ULL, 0x7fc000013fc00000ULL},
    {0x00000000ffc00002ULL, 0x7fe0000040000000ULL},
    {0x7f8000007f800000ULL, 0x8000000080000000ULL},
    {0x7fc000017fc00001ULL, 0x3fc000003fc00000ULL},
    {0x7ff0000000000000ULL, 0xfff8000000000001ULL},
    {0xfff0000000000000ULL, 0x0000000000000000ULL},
    {0xfff0000000000000ULL, 0x3ff0000000000000ULL},
    {0xfff0000000000000ULL, 0xfff8000000000001ULL},
    {0x7ff0000000000000ULL, 0x3ff0000000000000ULL},
    {0xfff8000000000001ULL, 0x7ff0000000000000ULL},
    {0xfff8000000000001ULL, 0xfff8000000000001ULL},
    {0xc004000000000000ULL, 0xc004000000000000ULL},
    {0x817a736c655e5750ULL, 0x49423b342d261f18ULL},
    {0x0000000000000000ULL, 0x0000004000000000ULL},
    {0x0000000000000000ULL, 0x0000004000000040ULL},
    {0x0005ff9cfffe7fffULL, 0x000080007fff8000ULL},
    {0x875bcd15fffffffbULL, 0x7fffffff80000000ULL},
    {0x0005ff9c7fff8000ULL, 0x80007fff80007fffULL},
    {0x80007fff80007fffULL, 0x80007fffd4998001ULL},
    {0xfffb012c00008001ULL, 0x0000000080017ffeULL},
    {0x875bcd15fffffffbULL, 0x800000017ffffffeULL},
    {0xfffb012c00008001ULL, 0x0000000080017ffeULL},
    {0x7ffd00007fff8000ULL, 0xfffb012c00008001ULL},
    {0x0000000000000000ULL, 0x0005ff9c7fff8000ULL},
    {0x0000000000000000ULL, 0x875bcd1580000000ULL},
    {0x0000000000000000ULL, 0x80007fff80007fffULL},
    {0x0000000000000000ULL, 0x7ffd000000008001ULL},
    {0x0000000000000000ULL, 0xfffffffb80000001ULL},
    {0x0000000000000000ULL, 0x0000800180017ffeULL},
    {0xff8100008000bf81ULL, 0xbf01bf013e023e02ULL},
    {0x6f12ff66074ee754ULL, 0xef0002fa7e81ff01ULL},
    {0x0000000000000000ULL, 0xbf01bf013e023e02ULL},
    {0x0000000000000000ULL, 0x6f12ff66074ee754ULL},
    {0xfffb0000ff9c0032ULL, 0xffff800100008001ULL},
    {0x8000800020002000ULL, 0x0000800000007ffeULL},
    {0xfffb0000ff380064ULL, 0xa461303900008000ULL},
    {0x0000000000000000ULL, 0x8000800020002000ULL},
    {0x0000000000000000ULL, 0xa461303900008000ULL},
    {0x7ff080009afe0080ULL, 0xf000008000f00000ULL},
    {0x00000000108f007fULL, 0x00ff00ffff000100ULL},
    {0x0000000000000000ULL, 0xc000008000c00000ULL},
    {0x0000000000000000ULL, 0x34f012009abc0012ULL},
    {0xf0de00667856cc12ULL, 0xc0c0fe0000ff8180ULL},
    {0x80000000c0004000ULL, 0xffff8000ffff8001ULL},
    {0xfffb0000ff380064ULL, 0x80017fffffff8000ULL},
    {0x0000000180000000ULL, 0x0000000080000001ULL},
    {0x80000000f8a432ebULL, 0x00000000fffffffbULL},
    {0x0000000000000000ULL, 0xf0de00667856cc12ULL},
    {0x0000000000000000ULL, 0xffff8000ffff8001ULL},
    {0x0000000000000000ULL, 0x0000000180000000ULL},
    {0x017f00ff0c058110ULL, 0x0f8f0300807fffffULL},
    {0x80017f00ff0c0581ULL, 0x100f8f0300807fffULL},
    {0x40fe0100ff7f8001ULL, 0x7f00ff0c0581100fULL},
    {0xdebc9a78563412c0ULL, 0x40fe0100ff7f8001ULL},
    {0xf0debc9a78563412ULL, 0xc040fe0100ff7f80ULL},
    {0x00f0debc9a785634ULL, 0x12c040fe0100ff7fULL},
    {0x0000000000000000ULL, 0x00000000000000f0ULL},
    {0x0000000000000000ULL, 0x0000000000000000ULL},
    {0x0000000000000000ULL, 0x0000000000000000ULL},
    {0x0000000000000000ULL, 0x0f8f0300807fffffULL},
    {0x0000000000000000ULL, 0xff7f800f8f030080ULL},
    {0x0000000000000000ULL, 0xc040fe0100ff7f80ULL},
    {0x0000000000000000ULL, 0x0000000000c040feULL},
    {0x0000000000000000ULL, 0x0000000000000000ULL},
    {0x0000000000000000ULL, 0x0000000000000000ULL},
    {0x1022446678563412ULL, 0x4040020100017f80ULL},
    {0x017f00010c057f10ULL, 0x0f710300807f0101ULL},
    {0x8000800040004000ULL, 0x0001800000017fffULL},
    {0x7fff00027fff7fffULL, 0x5ba0303900018000ULL},
    {0x0000000180000000ULL, 0x000000017fffffffULL},
    {0x80000000075bcd15ULL, 0x0000000000000005ULL},
    {0x0000000000000000ULL, 0x4040020100017f80ULL},
    {0x0000000000000000ULL, 0x0001800000017fffULL},
    {0x0000000000000000ULL, 0x0000000180000000ULL},
};

static int n;

static int ck(__m128i v)
{
    U u;
    u.i = v;
#ifdef GEN
    printf("    {0x%016llxULL, 0x%016llxULL},\n", u.q[1], u.q[0]);
    n++;
    return 1;
#else
    int ok = u.q[1] == E[n][0] && u.q[0] == E[n][1];
    n++;
    return ok;
#endif
}

static __m128i w64(__m64 v)
{
    M m;
    m.m = v;
    return (__m128i)(__v2di){(long long)m.q, 0};
}

#define C(...) do { if (!ck(__VA_ARGS__)) return n; } while (0)
#define CF(v) C((__m128i)(v))
#define CD(v) C((__m128i)(v))
#define C64(v) C(w64(v))

static __m64 lo64(__m128i x) { M m; m.q = (unsigned long long)((__v2di)x)[0]; return m.m; }
static __m64 hi64(__m128i x) { M m; m.q = (unsigned long long)((__v2di)x)[1]; return m.m; }

int main(int argc, char **argv)
{
    /* float: 1.5, -0.0, NaN(0x7fc00001), +Inf / 2.0, +0.0, sNaN(0x7fa00000), -Inf */
    __m128 fa = (__m128)(__v4su){0x3fc00000u, 0x80000000u, 0x7fc00001u, 0x7f800000u};
    __m128 fb = (__m128)(__v4su){0x40000000u, 0x00000000u, 0x7fa00000u, 0xff800000u};
    __m128 fc = (__m128)(__v4su){0x3f800000u, 0xffc00002u, 0x80000000u, 0x80000000u};
    /* double: 1.0, -0.0 / NaN(-, payload 1), +Inf / 3.0, -Inf */
    __m128d da = (__m128d)(__v2du){0x3ff0000000000000ull, 0x8000000000000000ull};
    __m128d db = (__m128d)(__v2du){0xfff8000000000001ull, 0x7ff0000000000000ull};
    __m128d dc = (__m128d)(__v2du){0x4008000000000000ull, 0xfff0000000000000ull};
    __m128i w1 = (__m128i)(__v8hi){0x7fff, 1, -32768, -1, 0x4000, 0x4000, -32768, -32768};
    __m128i w2 = (__m128i)(__v8hi){-32768, -1, 0x7fff, 0x7fff, 100, -200, 0, 5};
    __m128i w3 = (__m128i)(__v8hi){-32768, 1, 12345, -23456, 0x7fff, 0x7fff, -2, -32767};
    __m128i d1 = (__m128i)(__v4si){0x7fffffff, 1, (int)0x80000000, -1};
    __m128i d2 = (__m128i)(__v4si){-5, 0, 123456789, (int)0x80000000};
    __m128i b1 = (__m128i)(__v16qi){(char)0x80, 0x7f, (char)0xff, 0, 1, (char)0xfe, 0x40, (char)0xc0,
                                    0x12, 0x34, 0x56, 0x78, (char)0x9a, (char)0xbc, (char)0xde, (char)0xf0};
    __m128i b2 = (__m128i)(__v16qi){(char)0xff, (char)0xff, 0x7f, (char)0x80, 0, 3, (char)0x8f, 0x0f,
                                    0x10, (char)0x81, 0x05, 0x0c, (char)0xff, 0x00, 0x7f, 0x01};
    __m128i b3 = (__m128i)(__v16qi){(char)0xff, 0x7f, (char)0xff, 0x7f, (char)0xff, (char)0x80, (char)0xff, (char)0x80,
                                    (char)0xff, 0x7f, (char)0xff, (char)0x80, 0, 0, 0x7f, 0x7f};
    __m128i b4 = (__m128i)(__v16qi){(char)0xff, 0x7f, (char)0xff, 0x7f, (char)0xff, (char)0x80, (char)0xff, (char)0x80,
                                    (char)0x80, 0x7f, (char)0x80, (char)0x80, 0x7f, 0x7f, 0x7f, (char)0x80};
    unsigned char buf[40];
    double dd = -2.5;
    int k;

    for (k = 0; k < 40; k++)
        buf[k] = (unsigned char)(k * 7 + 3);
    if (argc > 1000) { /* MONITOR/MWAIT fault in user mode: compile only */
        _mm_monitor(argv, 0, 0);
        _mm_mwait(0, 0);
    }

    /* SSE3 */
    CF(_mm_addsub_ps(fa, fb));
    CF(_mm_addsub_ps(fb, fc));
    CF(_mm_hadd_ps(fa, fb));
    CF(_mm_hadd_ps(fc, fa));
    CF(_mm_hsub_ps(fa, fb));
    CF(_mm_hsub_ps(fb, fc));
    CF(_mm_movehdup_ps(fa));
    CF(_mm_moveldup_ps(fa));
    CD(_mm_addsub_pd(da, db));
    CD(_mm_addsub_pd(dc, dc));
    CD(_mm_hadd_pd(da, dc));
    CD(_mm_hadd_pd(db, dc));
    CD(_mm_hsub_pd(da, dc));
    CD(_mm_hsub_pd(dc, db));
    CD(_mm_movedup_pd(db));
    CD(_mm_loaddup_pd(&dd));
    C(_mm_lddqu_si128((const __m128i *)(buf + 3)));
    {
        unsigned int m0 = _MM_GET_DENORMALS_ZERO_MODE();
        _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_ON);
        unsigned int m1 = _MM_GET_DENORMALS_ZERO_MODE();
        volatile float tiny = 1e-40f;
        volatile float r = tiny * 2.0f;
        float rf = r;
        unsigned int rb;
        memcpy(&rb, &rf, 4);
        _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_OFF);
        unsigned int m2 = _MM_GET_DENORMALS_ZERO_MODE();
        C((__m128i)(__v4su){m0, m1, m2, rb});
        C((__m128i)(__v4su){_MM_DENORMALS_ZERO_MASK, _MM_DENORMALS_ZERO_ON, _MM_DENORMALS_ZERO_OFF, 0});
    }

    /* SSSE3 horizontal */
    C(_mm_hadd_epi16(w1, w2));
    C(_mm_hadd_epi32(d1, d2));
    C(_mm_hadds_epi16(w1, w2));
    C(_mm_hadds_epi16(w3, w1));
    C(_mm_hsub_epi16(w1, w2));
    C(_mm_hsub_epi32(d1, d2));
    C(_mm_hsubs_epi16(w1, w2));
    C(_mm_hsubs_epi16(w2, w3));
    C64(_mm_hadd_pi16(lo64(w1), hi64(w2)));
    C64(_mm_hadd_pi32(lo64(d1), hi64(d2)));
    C64(_mm_hadds_pi16(lo64(w1), hi64(w1)));
    C64(_mm_hsub_pi16(lo64(w2), hi64(w3)));
    C64(_mm_hsub_pi32(hi64(d1), lo64(d2)));
    C64(_mm_hsubs_pi16(lo64(w1), lo64(w2)));

    /* multiply */
    C(_mm_maddubs_epi16(b3, b4));
    C(_mm_maddubs_epi16(b1, b2));
    C64(_mm_maddubs_pi16(lo64(b3), lo64(b4)));
    C64(_mm_maddubs_pi16(hi64(b1), hi64(b2)));
    C(_mm_mulhrs_epi16(w1, w2));
    C(_mm_mulhrs_epi16(w1, w1));
    C(_mm_mulhrs_epi16(w3, w2));
    C64(_mm_mulhrs_pi16(hi64(w1), hi64(w1)));
    C64(_mm_mulhrs_pi16(lo64(w3), lo64(w2)));

    /* shuffle */
    C(_mm_shuffle_epi8(b1, b2));
    C(_mm_shuffle_epi8(b2, b1));
    C64(_mm_shuffle_pi8(lo64(b1), lo64(b2)));
    C64(_mm_shuffle_pi8(hi64(b1), hi64(b2)));

    /* sign */
    C(_mm_sign_epi8(b1, b2));
    C(_mm_sign_epi16(w1, w2));
    C(_mm_sign_epi16(w2, w3));
    C(_mm_sign_epi32(d1, d2));
    C(_mm_sign_epi32(d2, d1));
    C64(_mm_sign_pi8(hi64(b1), hi64(b2)));
    C64(_mm_sign_pi16(lo64(w1), lo64(w2)));
    C64(_mm_sign_pi32(hi64(d1), hi64(d2)));

    /* alignr */
    C(_mm_alignr_epi8(b1, b2, 0));
    C(_mm_alignr_epi8(b1, b2, 1));
    C(_mm_alignr_epi8(b1, b2, 7));
    C(_mm_alignr_epi8(b1, b2, 15));
    C(_mm_alignr_epi8(b1, b2, 16));
    C(_mm_alignr_epi8(b1, b2, 17));
    C(_mm_alignr_epi8(b1, b2, 31));
    C(_mm_alignr_epi8(b1, b2, 32));
    C(_mm_alignr_epi8(b1, b2, 200));
    C64(_mm_alignr_pi8(lo64(b1), lo64(b2), 0));
    C64(_mm_alignr_pi8(lo64(b1), lo64(b2), 3));
    C64(_mm_alignr_pi8(lo64(b1), lo64(b2), 8));
    C64(_mm_alignr_pi8(lo64(b1), lo64(b2), 13));
    C64(_mm_alignr_pi8(lo64(b1), lo64(b2), 16));
    C64(_mm_alignr_pi8(lo64(b1), lo64(b2), 255));

    /* abs */
    C(_mm_abs_epi8(b1));
    C(_mm_abs_epi8(b2));
    C(_mm_abs_epi16(w1));
    C(_mm_abs_epi16(w3));
    C(_mm_abs_epi32(d1));
    C(_mm_abs_epi32(d2));
    C64(_mm_abs_pi8(lo64(b1)));
    C64(_mm_abs_pi16(lo64(w1)));
    C64(_mm_abs_pi32(hi64(d1)));

#ifdef GEN
    fprintf(stderr, "%d checks\n", n);
#endif
    return 0;
}
