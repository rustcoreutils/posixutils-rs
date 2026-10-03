/*
 * Self-checking test of c17's bundled intrinsic headers: expected values
 * were captured from gcc running on the hardware (or under qemu).
 * Exits 0 on success, else the number of the first failing check.
 * SPDX-License-Identifier: MIT
 */
/* Self-checking end-to-end test for c17's mmintrin.h and xmmintrin.h.
   Expected values were recorded from gcc 13 with its own headers on x86-64
   hardware. Exit status 0 = pass; otherwise the number of the failed check.
   rcp/rsqrt are approximations on hardware, so they are checked against a
   relative tolerance of 1.5 * 2^-12 instead of exact bits. */
#include <xmmintrin.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

static float fb(uint32_t u) { float f; memcpy(&f, &u, 4); return f; }
static __m64 m64(unsigned long long u) { __m64 m; memcpy(&m, &u, 8); return m; }

/* Inputs are globals hidden behind a compiler barrier, so that no
   compiler can fold the intrinsics at compile time. */
__m128 A, B, C, D, N;
__m64 P, Q, R, S;
float G[16];
long long L[4];
static float buf[12] __attribute__((aligned(16)));

static void setup(void)
{
    A = _mm_setr_ps(1.5f, -2.5f, fb(0x7fc00000), -0.0f);
    B = _mm_setr_ps(-0.0f, -3.0f, 4.0f, fb(0x7f800000));
    C = _mm_setr_ps(3.0e9f, -2.5f, 2.5f, 65535.5f);
    D = _mm_setr_ps(-2147483648.0f, 2147483520.0f, -1e20f, 0.5f);
    N = _mm_setr_ps(fb(0x7fc00000), 0.0f, 0.0f, 0.0f);
    P = m64(0x7fff8000007f0080ULL);
    Q = m64(0x80007fff80017ffeULL);
    R = m64(0x7f80ff017f80ff01ULL);
    S = m64(0x0123456789abcdefULL);
    for (int i = 0; i < 12; i++)
        buf[i] = (float)i * 1.25f - 3.0f;
    static const float g[16] = {2147483648.0f, 2147483520.0f, -2.9f, -2.5f, -1e20f,
                                -9.2233715e18f, 0.0f, -0.0f, -200.0f, 127.5f, 3.5f,
                                -0.5f, -1.0f, 7.0f, 3.0f, 0.1f};
    for (int i = 0; i < 16; i++)
        G[i] = g[i];
    L[0] = -16777217;
    L[1] = 0x7fffffffffffffffLL;
    L[2] = (1LL << 53) + 1;
    L[3] = -1;
    __asm__ __volatile__("" : : : "memory");
}

/* Relative error of an approximate reciprocal (sq = 0) or reciprocal
   square root (sq = 1) of x, within 1.5 * 2^-12 (doubled for the square). */
static int near(float got, float x, int sq)
{
    double e = sq ? (double)got * got * x - 1.0 : (double)got * x - 1.0;
    double lim = (sq ? 3.0 : 1.5) / 4096.0 * 1.0001;
    return e <= lim && e >= -lim;
}

#define C128(id, e, x0, x1, x2, x3) do { __m128 r_ = (e); uint32_t u_[4]; \
    memcpy(u_, &r_, 16); \
    if (u_[0] != (x0) || u_[1] != (x1) || u_[2] != (x2) || u_[3] != (x3)) { \
        fprintf(stderr, "check %d failed: %s\n", id, #e); return id; } } while (0)
#define C64(id, e, x) do { __m64 r_ = (e); unsigned long long u_; memcpy(&u_, &r_, 8); \
    if (u_ != (x)) { fprintf(stderr, "check %d failed: %s\n", id, #e); return id; } } while (0)
#define CI(id, e, x) do { if ((unsigned long long)(long long)(e) != (x)) { \
    fprintf(stderr, "check %d failed: %s\n", id, #e); return id; } } while (0)
#define CT(id, e) do { if (!(e)) { fprintf(stderr, "check %d failed: %s\n", id, #e); \
    return id; } } while (0)

static __m128 via_storer(__m128 v)
{
    float o[4] __attribute__((aligned(16)));
    _mm_storer_ps(o, v);
    return _mm_loadu_ps(o);
}

static __m128 via_storeu(__m128 v)
{
    float o[6] = {0};
    _mm_storeu_ps(o + 1, v);
    return _mm_loadu_ps(o);
}

static __m128 via_store_ss_h_l(__m128 v)
{
    float o[8] = {0};
    _mm_store_ss(o, v);
    _mm_storeh_pi((__m64 *)(o + 1), v);
    _mm_storel_pi((__m64 *)(o + 4), v);
    return _mm_loadu_ps(o + 1);
}

static __m128 via_store1(__m128 v)
{
    float o[4] __attribute__((aligned(16)));
    _mm_store1_ps(o, v);
    return _mm_load_ps(o);
}

static __m64 via_maskmove(__m64 d, __m64 n)
{
    char o[8];
    memset(o, 0x5a, 8);
    _mm_maskmove_si64(d, n, o);
    __m64 r;
    memcpy(&r, o, 8);
    return r;
}

static __m128 transposed_row(int k)
{
    __m128 r0 = _mm_setr_ps(0, 1, 2, 3), r1 = _mm_setr_ps(4, 5, 6, 7);
    __m128 r2 = _mm_setr_ps(8, 9, 10, 11), r3 = _mm_setr_ps(12, 13, 14, 15);
    _MM_TRANSPOSE4_PS(r0, r1, r2, r3);
    return k == 0 ? r0 : k == 1 ? r1 : k == 2 ? r2 : r3;
}

/* cvtss_si32 of 2.5, -2.5, 3.5 and -0.5 under a rounding mode, packed into
   one word, restoring MXCSR afterwards. */
static unsigned int rounded(unsigned int mode)
{
    unsigned int saved = _mm_getcsr();
    _MM_SET_ROUNDING_MODE(mode);
    volatile float in[4] = {2.5f, -2.5f, 3.5f, -0.5f};
    unsigned int r = 0;
    for (int i = 0; i < 4; i++)
        r = r << 8 | (_mm_cvtss_si32(_mm_set_ss(in[i])) & 0xff);
    _mm_setcsr(saved);
    return r;
}

static int rcp_ok(__m128 x)
{
    __m128 r = _mm_rcp_ps(x), s = _mm_rsqrt_ps(x);
    __m128 rs = _mm_rcp_ss(x), ss = _mm_rsqrt_ss(x);
    for (int i = 0; i < 4; i++)
        if (!near(r[i], x[i], 0) || !near(s[i], x[i], 1))
            return 0;
    return near(rs[0], x[0], 0) && near(ss[0], x[0], 1) && rs[1] == x[1] && ss[3] == x[3];
}

int main(void)
{
    setup();
    C128(1, _mm_add_ps(A, B), 0x3fc00000u, 0xc0b00000u, 0x7fc00000u, 0x7f800000u);
    C128(2, _mm_sub_ps(A, C), 0xcf32d05eu, 0x00000000u, 0x7fc00000u, 0xc77fff80u);
    C128(3, _mm_mul_ps(B, C), 0x80000000u, 0x40f00000u, 0x41200000u, 0x7f800000u);
    C128(4, _mm_div_ps(C, B), 0xff800000u, 0x3f555555u, 0x3f200000u, 0x00000000u);
    C128(5, _mm_add_ss(C, D), 0x4e4b4178u, 0xc0200000u, 0x40200000u, 0x477fff80u);
    C128(6, _mm_sub_ss(D, C), 0xcf99682fu, 0x4effffffu, 0xe0ad78ecu, 0x3f000000u);
    C128(7, _mm_mul_ss(A, C), 0x4f861c46u, 0xc0200000u, 0x7fc00000u, 0x80000000u);
    C128(8, _mm_div_ss(A, B), 0xff800000u, 0xc0200000u, 0x7fc00000u, 0x80000000u);
    C128(9, _mm_sqrt_ps(B), 0x80000000u, 0xffc00000u, 0x40000000u, 0x7f800000u);
    C128(10, _mm_sqrt_ss(C), 0x4755f441u, 0xc0200000u, 0x40200000u, 0x477fff80u);
    C128(11, _mm_min_ps(A, B), 0x80000000u, 0xc0400000u, 0x40800000u, 0x80000000u);
    C128(12, _mm_min_ps(B, A), 0x80000000u, 0xc0400000u, 0x7fc00000u, 0x80000000u);
    C128(13, _mm_max_ps(A, B), 0x3fc00000u, 0xc0200000u, 0x40800000u, 0x7f800000u);
    C128(14, _mm_max_ps(B, A), 0x3fc00000u, 0xc0200000u, 0x7fc00000u, 0x7f800000u);
    C128(15, _mm_min_ss(N, A), 0x3fc00000u, 0x00000000u, 0x00000000u, 0x00000000u);
    C128(16, _mm_max_ss(A, N), 0x7fc00000u, 0xc0200000u, 0x7fc00000u, 0x80000000u);
    C128(17, _mm_max_ss(_mm_set_ss(G[6]), _mm_set_ss(G[7])), 0x80000000u, 0x00000000u, 0x00000000u, 0x00000000u);
    C128(18, _mm_and_ps(A, C), 0x0f000000u, 0xc0200000u, 0x40000000u, 0x00000000u);
    C128(19, _mm_andnot_ps(A, C), 0x4032d05eu, 0x00000000u, 0x00200000u, 0x477fff80u);
    C128(20, _mm_or_ps(B, D), 0xcf000000u, 0xceffffffu, 0xe0ad78ecu, 0x7f800000u);
    C128(21, _mm_xor_ps(A, D), 0xf0c00000u, 0x8edfffffu, 0x9f6d78ecu, 0xbf000000u);
    C128(22, _mm_cmpeq_ps(A, A), 0xffffffffu, 0xffffffffu, 0x00000000u, 0xffffffffu);
    C128(23, _mm_cmplt_ps(A, B), 0x00000000u, 0x00000000u, 0x00000000u, 0xffffffffu);
    C128(24, _mm_cmple_ps(B, A), 0xffffffffu, 0xffffffffu, 0x00000000u, 0x00000000u);
    C128(25, _mm_cmpgt_ps(C, A), 0xffffffffu, 0x00000000u, 0x00000000u, 0xffffffffu);
    C128(26, _mm_cmpge_ps(A, B), 0xffffffffu, 0xffffffffu, 0x00000000u, 0x00000000u);
    C128(27, _mm_cmpneq_ps(A, A), 0x00000000u, 0x00000000u, 0xffffffffu, 0x00000000u);
    C128(28, _mm_cmpnlt_ps(A, B), 0xffffffffu, 0xffffffffu, 0xffffffffu, 0x00000000u);
    C128(29, _mm_cmpnle_ps(A, B), 0xffffffffu, 0xffffffffu, 0xffffffffu, 0x00000000u);
    C128(30, _mm_cmpngt_ps(A, B), 0x00000000u, 0x00000000u, 0xffffffffu, 0xffffffffu);
    C128(31, _mm_cmpnge_ps(A, B), 0x00000000u, 0x00000000u, 0xffffffffu, 0xffffffffu);
    C128(32, _mm_cmpord_ps(A, B), 0xffffffffu, 0xffffffffu, 0x00000000u, 0xffffffffu);
    C128(33, _mm_cmpunord_ps(A, B), 0x00000000u, 0x00000000u, 0xffffffffu, 0x00000000u);
    C128(34, _mm_cmpeq_ss(N, N), 0x00000000u, 0x00000000u, 0x00000000u, 0x00000000u);
    C128(35, _mm_cmplt_ss(A, C), 0xffffffffu, 0xc0200000u, 0x7fc00000u, 0x80000000u);
    C128(36, _mm_cmpneq_ss(N, A), 0xffffffffu, 0x00000000u, 0x00000000u, 0x00000000u);
    C128(37, _mm_cmpnge_ss(A, D), 0x00000000u, 0xc0200000u, 0x7fc00000u, 0x80000000u);
    C128(38, _mm_cmpunord_ss(N, A), 0xffffffffu, 0x00000000u, 0x00000000u, 0x00000000u);
    CI(39, _mm_comieq_ss(A, A), 0x1ull);
    CI(40, _mm_comieq_ss(N, N), 0x0ull);
    CI(41, _mm_comineq_ss(N, N), 0x1ull);
    CI(42, _mm_comilt_ss(A, C), 0x1ull);
    CI(43, _mm_comige_ss(N, A), 0x0ull);
    CI(44, _mm_ucomigt_ss(C, A), 0x1ull);
    CI(45, _mm_ucomile_ss(N, A), 0x0ull);
    CI(46, _mm_ucomineq_ss(B, _mm_set_ss(G[6])), 0x0ull);
    CI(47, _mm_cvtss_si32(C), 0xffffffff80000000ull);
    CI(48, _mm_cvtss_si32(D), 0xffffffff80000000ull);
    CI(49, _mm_cvtss_si32(N), 0xffffffff80000000ull);
    CI(50, _mm_cvt_ss2si(_mm_set_ss(G[3])), 0xfffffffffffffffeull);
    CI(51, _mm_cvttss_si32(_mm_set_ss(G[2])), 0xfffffffffffffffeull);
    CI(52, _mm_cvttss_si32(_mm_set_ss(G[0])), 0xffffffff80000000ull);
    CI(53, _mm_cvtt_ss2si(_mm_set_ss(G[1])), 0x7fffff80ull);
    CI(54, _mm_cvtss_si64(C), 0xb2d05e00ull);
    CI(55, _mm_cvtss_si64(_mm_set_ss(G[4])), 0x8000000000000000ull);
    CI(56, _mm_cvttss_si64(_mm_set_ss(G[5])), 0x8000008000000000ull);
    CI(57, _mm_cvttss_si64(N), 0x8000000000000000ull);
    C64(58, _mm_cvtps_pi32(C), 0xfffffffe80000000ull);
    C64(59, _mm_cvttps_pi32(D), 0x7fffff8080000000ull);
    C64(60, _mm_cvt_ps2pi(A), 0xfffffffe00000002ull);
    C64(61, _mm_cvtps_pi16(C), 0x7fff0002fffe8000ull);
    C64(62, _mm_cvtps_pi16(D), 0x000080007fff8000ull);
    C64(63, _mm_cvtps_pi8(_mm_setr_ps(G[8], G[9], G[10], G[11])), 0x0000000000047f80ull);
    C128(64, _mm_cvtsi32_ss(A, (int)L[0]), 0xcb800000u, 0xc0200000u, 0x7fc00000u, 0x80000000u);
    C128(65, _mm_cvtsi64_ss(A, L[1]), 0x5f000000u, 0xc0200000u, 0x7fc00000u, 0x80000000u);
    C128(66, _mm_cvtsi64_ss(A, L[2]), 0x5a000000u, 0xc0200000u, 0x7fc00000u, 0x80000000u);
    C128(67, _mm_cvtpi32_ps(A, P), 0x4afe0100u, 0x4effff00u, 0x7fc00000u, 0x80000000u);
    C128(68, _mm_cvtpi16_ps(Q), 0x46fffc00u, 0xc6fffe00u, 0x46fffe00u, 0xc7000000u);
    C128(69, _mm_cvtpu16_ps(Q), 0x46fffc00u, 0x47000100u, 0x46fffe00u, 0x47000000u);
    C128(70, _mm_cvtpi8_ps(R), 0x3f800000u, 0xbf800000u, 0xc3000000u, 0x42fe0000u);
    C128(71, _mm_cvtpu8_ps(R), 0x3f800000u, 0x437f0000u, 0x43000000u, 0x42fe0000u);
    C128(72, _mm_cvtpi32x2_ps(P, Q), 0x4afe0100u, 0x4effff00u, 0xcefffd00u, 0xceffff00u);
    CI(73, _mm_cvtss_f32(D) == -2147483648.0f, 0x1ull);
    C128(74, _mm_set_ps(1.0f, 2.0f, 3.0f, 4.0f), 0x40800000u, 0x40400000u, 0x40000000u, 0x3f800000u);
    C128(75, _mm_setr_ps(1.0f, 2.0f, 3.0f, 4.0f), 0x3f800000u, 0x40000000u, 0x40400000u, 0x40800000u);
    C128(76, _mm_set_ss(7.0f), 0x40e00000u, 0x00000000u, 0x00000000u, 0x00000000u);
    C128(77, _mm_set1_ps(-0.0f), 0x80000000u, 0x80000000u, 0x80000000u, 0x80000000u);
    C128(78, _mm_setzero_ps(), 0x00000000u, 0x00000000u, 0x00000000u, 0x00000000u);
    C128(79, _mm_load_ss(buf + 3), 0x3f400000u, 0x00000000u, 0x00000000u, 0x00000000u);
    C128(80, _mm_load1_ps(buf + 5), 0x40500000u, 0x40500000u, 0x40500000u, 0x40500000u);
    C128(81, _mm_load_ps(buf + 4), 0x40000000u, 0x40500000u, 0x40900000u, 0x40b80000u);
    C128(82, _mm_loadu_ps(buf + 1), 0xbfe00000u, 0xbf000000u, 0x3f400000u, 0x40000000u);
    C128(83, _mm_loadr_ps(buf + 8), 0x412c0000u, 0x41180000u, 0x41040000u, 0x40e00000u);
    C128(84, _mm_loadh_pi(A, (__m64 const *)(buf + 3)), 0x3fc00000u, 0xc0200000u, 0x3f400000u, 0x40000000u);
    C128(85, _mm_loadl_pi(A, (__m64 const *)(buf + 6)), 0x40900000u, 0x40b80000u, 0x7fc00000u, 0x80000000u);
    C128(86, via_storer(C), 0x477fff80u, 0x40200000u, 0xc0200000u, 0x4f32d05eu);
    C128(87, via_storeu(D), 0x00000000u, 0xcf000000u, 0x4effffffu, 0xe0ad78ecu);
    C128(88, via_store_ss_h_l(C), 0x40200000u, 0x477fff80u, 0x00000000u, 0x4f32d05eu);
    C128(89, via_store1(D), 0xcf000000u, 0xcf000000u, 0xcf000000u, 0xcf000000u);
    C128(90, _mm_shuffle_ps(A, C, _MM_SHUFFLE(0, 3, 1, 2)), 0x7fc00000u, 0xc0200000u, 0x477fff80u, 0x4f32d05eu);
    C128(91, _mm_shuffle_ps(C, D, 0x1b), 0x477fff80u, 0x40200000u, 0x4effffffu, 0xcf000000u);
    C128(92, _mm_unpackhi_ps(A, C), 0x7fc00000u, 0x40200000u, 0x80000000u, 0x477fff80u);
    C128(93, _mm_unpacklo_ps(A, C), 0x3fc00000u, 0x4f32d05eu, 0xc0200000u, 0xc0200000u);
    C128(94, _mm_move_ss(A, C), 0x4f32d05eu, 0xc0200000u, 0x7fc00000u, 0x80000000u);
    C128(95, _mm_movehl_ps(A, C), 0x40200000u, 0x477fff80u, 0x7fc00000u, 0x80000000u);
    C128(96, _mm_movelh_ps(A, C), 0x3fc00000u, 0xc0200000u, 0x4f32d05eu, 0xc0200000u);
    CI(97, _mm_movemask_ps(A), 0xaull);
    CI(98, _mm_movemask_ps(D), 0x5ull);
    C128(99, transposed_row(0), 0x00000000u, 0x40800000u, 0x41000000u, 0x41400000u);
    C128(100, transposed_row(3), 0x40400000u, 0x40e00000u, 0x41300000u, 0x41700000u);
    CI(101, _mm_getcsr() & 0xffc0, 0x1f80ull);
    CI(102, rounded(_MM_ROUND_NEAREST), 0x2fe0400ull);
    CI(103, rounded(_MM_ROUND_DOWN), 0x2fd03ffull);
    CI(104, rounded(_MM_ROUND_UP), 0x3fe0400ull);
    CI(105, rounded(_MM_ROUND_TOWARD_ZERO), 0x2fe0300ull);
    CT(106, rcp_ok(_mm_setr_ps(G[14], G[15], -G[4], G[13])));
    CT(107, rcp_ok(_mm_setr_ps(0.001f, 2.0f, 12345.0f, 1e-30f)));
    CT(108, _mm_rcp_ps(B)[0] == -__builtin_inff());
    CT(109, _mm_rsqrt_ss(_mm_set_ss(G[12]))[0] != _mm_rsqrt_ss(_mm_set_ss(G[12]))[0]);
    C64(110, _mm_add_pi8(P, Q), 0xffffffff80807f7eull);
    C64(111, _mm_add_pi16(P, Q), 0xffffffff8080807eull);
    C64(112, _mm_add_pi32(P, Q), 0xffffffff8080807eull);
    C64(113, _mm_add_si64(P, Q), 0xffffffff8080807eull);
    C64(114, _mm_sub_pi8(P, Q), 0xffff0101807e8182ull);
    C64(115, _mm_sub_si64(Q, P), 0x0000ffff7f827f7eull);
    C64(116, _mm_adds_pi8(P, R), 0x7f8080017fffff81ull);
    C64(117, _mm_adds_pi16(P, Q), 0xffffffff80807fffull);
    C64(118, _mm_adds_pu8(R, S), 0x80a3ff68fffffff0ull);
    C64(119, _mm_adds_pu16(Q, S), 0x8123c566ffffffffull);
    C64(120, _mm_subs_pi8(P, R), 0x007f81ff817f0180ull);
    C64(121, _mm_subs_pi16(Q, P), 0x80007fff80007f7eull);
    C64(122, _mm_subs_pu8(S, R), 0x000000660a2b00eeull);
    C64(123, _mm_subs_pu16(P, Q), 0x0000000100000000ull);
    C64(124, _mm_madd_pi16(Q, Q), 0x7fff00017ffd0005ull);
    C64(125, _mm_madd_pi16(m64(0x8000800080008000ULL), m64(0x8000800080008000ULL)), 0x8000000080000000ull);
    C64(126, _mm_mulhi_pi16(P, Q), 0xc000c000ffc0003full);
    C64(127, _mm_mullo_pi16(P, S), 0x7edd80004bd5f780ull);
    C64(128, _mm_packs_pi16(P, S), 0x7f7f80807f807f7full);
    C64(129, _mm_packs_pi32(P, Q), 0x800080007fff7fffull);
    C64(130, _mm_packs_pu16(Q, R), 0xff00ff0000ff00ffull);
    C64(131, _mm_unpackhi_pi8(P, S), 0x017f23ff45806700ull);
    C64(132, _mm_unpacklo_pi16(P, S), 0x89ab007fcdef0080ull);
    C64(133, _mm_unpackhi_pi32(P, S), 0x012345677fff8000ull);
    C64(134, _mm_cmpeq_pi8(R, R), 0xffffffffffffffffull);
    C64(135, _mm_cmpgt_pi8(P, R), 0x00ff000000ffff00ull);
    C64(136, _mm_cmpgt_pi16(P, Q), 0xffff0000ffff0000ull);
    C64(137, _mm_cmpgt_pi32(P, Q), 0xffffffffffffffffull);
    C64(138, _mm_and_si64(P, S), 0x01230000002b0080ull);
    C64(139, _mm_andnot_si64(P, S), 0x000045678980cd6full);
    C64(140, _mm_or_si64(Q, S), 0x81237fff89abffffull);
    C64(141, _mm_xor_si64(R, S), 0x7ea3ba66f62b32eeull);
    C64(142, _mm_slli_pi16(S, 4), 0x123056709ab0def0ull);
    C64(143, _mm_slli_pi32(S, 32), 0x0000000000000000ull);
    C64(144, _mm_srli_pi16(Q, 15), 0x0001000000010000ull);
    C64(145, _mm_srli_si64(S, 4), 0x00123456789abcdeull);
    C64(146, _mm_srai_pi16(Q, 3), 0xf0000ffff0000fffull);
    C64(147, _mm_srai_pi16(Q, 99), 0xffff0000ffff0000ull);
    C64(148, _mm_srai_pi32(Q, 31), 0xffffffffffffffffull);
    C64(149, _mm_sll_pi32(S, m64(8)), 0x23456700abcdef00ull);
    C64(150, _mm_sll_si64(S, m64(64)), 0x0000000000000000ull);
    C64(151, _mm_srl_pi32(S, m64(0x100000000ULL)), 0x0000000000000000ull);
    C64(152, _mm_sra_pi16(Q, m64(200)), 0xffff0000ffff0000ull);
    C64(153, _mm_sra_pi32(Q, m64(4)), 0xf80007fff80017ffull);
    C64(154, _mm_set_pi8(1, 2, 3, 4, 5, 6, 7, -8), 0x01020304050607f8ull);
    C64(155, _mm_setr_pi16(1, -2, 3, -4), 0xfffc0003fffe0001ull);
    C64(156, _mm_set1_pi32(-5), 0xfffffffbfffffffbull);
    C64(157, _mm_cvtsi32_si64((int)L[3]), 0x00000000ffffffffull);
    CI(158, _mm_cvtsi64_si32(Q), 0xffffffff80017ffeull);
    CI(159, _mm_cvtm64_si64(S), 0x123456789abcdefull);
    C64(160, _m_paddusb(R, S), 0x80a3ff68fffffff0ull);
    C64(161, _m_psubsw(Q, P), 0x80007fff80007f7eull);
    C64(162, _m_pmaddwd(P, Q), 0x8001000000007f7full);
    C64(163, _m_punpcklbw(P, S), 0x8900ab7fcd00ef80ull);
    C64(164, _mm_avg_pu8(R, S), 0x4052a2348496e678ull);
    C64(165, _mm_avg_pu16(Q, S), 0x409262b384d6a6f7ull);
    C64(166, _mm_max_pi16(P, Q), 0x7fff7fff007f7ffeull);
    C64(167, _mm_min_pi16(P, Q), 0x8000800080010080ull);
    C64(168, _mm_max_pu8(R, S), 0x7f80ff6789abffefull);
    C64(169, _mm_min_pu8(R, S), 0x012345017f80cd01ull);
    C64(170, _mm_mulhi_pu16(Q, S), 0x009122b344d666f5ull);
    C64(171, _mm_sad_pu8(R, S), 0x0000000000000350ull);
    C64(172, _mm_sad_pu8(m64(0), m64(0xffffffffffffffffULL)), 0x00000000000007f8ull);
    CI(173, _mm_movemask_pi8(R), 0x66ull);
    CI(174, _mm_extract_pi16(Q, 0), 0x7ffeull);
    CI(175, _mm_extract_pi16(Q, 3), 0x8000ull);
    C64(176, _mm_insert_pi16(S, -1, 2), 0x0123ffff89abcdefull);
    C64(177, _mm_shuffle_pi16(S, _MM_SHUFFLE(0, 1, 2, 3)), 0xcdef89ab45670123ull);
    C64(178, _m_pshufw(S, 0xe4), 0x0123456789abcdefull);
    C64(179, via_maskmove(S, R), 0x5a23455a5aabcd5aull);
    return 0;
}
