/*
 * Self-checking test of c17's bundled intrinsic headers: expected values
 * were captured from gcc running on the hardware (or under qemu).
 * Exits 0 on success, else the number of the first failing check.
 * SPDX-License-Identifier: MIT
 */
#include <emmintrin.h>
#include <string.h>

/* Self-checking SSE2 intrinsics test; expected values were produced by gcc
   (-msse2, its own headers). Returns 0, or the number of the first failing
   check. */
static int chk128(__m128i v, unsigned long long hi, unsigned long long lo)
{ unsigned long long q[2]; memcpy(q, &v, 16); return q[0] == lo && q[1] == hi; }
static int chk64(__m64 v, unsigned long long e)
{ unsigned long long q; memcpy(&q, &v, 8); return q == e; }
static __m128i hide_si(__m128i x) { volatile __m128i t = x; return t; }
static __m128d hide_pd(__m128d x) { volatile __m128d t = x; return t; }
#define CV(n, x, hi, lo) if (!chk128((__m128i)(x), hi, lo)) return n;
#define CM(n, x, e) if (!chk64((x), e)) return n;
#define CI(n, x, e) if ((long long)(x) != (long long)(e)) return n;
int main(void)
{

    __m128d dA = {1.5, -2.25}, dB = {-0.0, 3.0};
    __m128d dN = {__builtin_nan(""), 4.0}, dZ = {0.0, -0.0}, dI = {__builtin_inf(), -1e300};
    __m128d dR = {2.5, -3.5}, dBig = {3e9, -2147483648.6}, dH = {2147483647.5, 9.3e18};
    __m128 fA = {1.5f, -2.5f, 3e9f, __builtin_nanf("")};
    __m128i iA = {0x7f80ff0001fe8001LL, (long long)0x80007fff12345678ULL};
    __m128i iB = {0x0102037f80ff7e81LL, (long long)0xffff0001fedcba98ULL};
    __m128i iC = {(long long)0x8000000080000000ULL, 0x7fffffff00000001LL};
    __m128i cnt = {5, 0}, cbig = {64, 0};
    volatile int sh = 3;
    double mem[4] __attribute__((aligned(16))) = {11.0, -12.0, 13.5, 0.125};
    unsigned char bytes[32] __attribute__((aligned(16)));
    for (int i = 0; i < 32; i++) bytes[i] = (unsigned char)(i * 29 + 3);
    /* Launder the inputs so no compiler folds the checks at compile time. */
    dA = hide_pd(dA); dB = hide_pd(dB); dN = hide_pd(dN); dZ = hide_pd(dZ); dI = hide_pd(dI);
    dR = hide_pd(dR); dBig = hide_pd(dBig); dH = hide_pd(dH); fA = (__m128)hide_si((__m128i)fA);
    iA = hide_si(iA); iB = hide_si(iB); iC = hide_si(iC); cnt = hide_si(cnt); cbig = hide_si(cbig);
    CV(1, _mm_add_pd(dA, dB), 0x3fe8000000000000ULL, 0x3ff8000000000000ULL)
    CV(2, _mm_add_sd(dA, dB), 0xc002000000000000ULL, 0x3ff8000000000000ULL)
    CV(3, _mm_sub_pd(dA, dB), 0xc015000000000000ULL, 0x3ff8000000000000ULL)
    CV(4, _mm_mul_pd(dA, dB), 0xc01b000000000000ULL, 0x8000000000000000ULL)
    CV(5, _mm_div_pd(dA, dB), 0xbfe8000000000000ULL, 0xfff0000000000000ULL)
    CV(6, _mm_div_sd(dB, dA), 0x4008000000000000ULL, 0x8000000000000000ULL)
    CV(7, _mm_sqrt_pd(dB), 0x3ffbb67ae8584caaULL, 0x8000000000000000ULL)
    CV(8, _mm_sqrt_sd(dA, dA), 0xc002000000000000ULL, 0x3ff3988e1409212eULL)
    CV(9, _mm_min_pd(dN, dA), 0xc002000000000000ULL, 0x3ff8000000000000ULL)
    CV(10, _mm_min_pd(dA, dN), 0xc002000000000000ULL, 0x7ff8000000000000ULL)
    CV(11, _mm_max_pd(dZ, _mm_set_pd(0.0, -0.0)), 0x0000000000000000ULL, 0x8000000000000000ULL)
    CV(12, _mm_max_sd(dN, dB), 0x4010000000000000ULL, 0x8000000000000000ULL)
    CV(13, _mm_min_sd(dA, dB), 0xc002000000000000ULL, 0x8000000000000000ULL)
    CV(14, _mm_and_pd(dA, dB), 0x4000000000000000ULL, 0x0000000000000000ULL)
    CV(15, _mm_andnot_pd(dZ, dA), 0x4002000000000000ULL, 0x3ff8000000000000ULL)
    CV(16, _mm_or_pd(dA, dZ), 0xc002000000000000ULL, 0x3ff8000000000000ULL)
    CV(17, _mm_xor_pd(dA, dB), 0x800a000000000000ULL, 0xbff8000000000000ULL)
    CV(18, _mm_cmpeq_pd(dN, dN), 0xffffffffffffffffULL, 0x0000000000000000ULL)
    CV(19, _mm_cmpneq_pd(dN, dN), 0x0000000000000000ULL, 0xffffffffffffffffULL)
    CV(20, _mm_cmplt_pd(dA, dB), 0xffffffffffffffffULL, 0x0000000000000000ULL)
    CV(21, _mm_cmpnlt_pd(dN, dA), 0xffffffffffffffffULL, 0xffffffffffffffffULL)
    CV(22, _mm_cmpord_pd(dN, dA), 0xffffffffffffffffULL, 0x0000000000000000ULL)
    CV(23, _mm_cmpunord_pd(dN, dA), 0x0000000000000000ULL, 0xffffffffffffffffULL)
    CV(24, _mm_cmpge_sd(dA, dB), 0xc002000000000000ULL, 0xffffffffffffffffULL)
    CV(25, _mm_cmpngt_sd(dN, dA), 0x4010000000000000ULL, 0xffffffffffffffffULL)
    CV(26, _mm_cmple_pd(dZ, dB), 0xffffffffffffffffULL, 0xffffffffffffffffULL)
    CV(27, _mm_cmpgt_sd(dB, dA), 0x4008000000000000ULL, 0x0000000000000000ULL)
    CI(28, _mm_comieq_sd(dN, dN), 0x0ULL)
    CI(29, _mm_comineq_sd(dN, dN), 0x1ULL)
    CI(30, _mm_comilt_sd(dA, dB), 0x0ULL)
    CI(31, _mm_ucomige_sd(dB, dA), 0x0ULL)
    CI(32, _mm_ucomieq_sd(dZ, _mm_set_sd(-0.0)), 0x1ULL)
    CI(33, _mm_comigt_sd(dN, dA), 0x0ULL)
    CV(34, _mm_cvtpd_epi32(dR), 0x0000000000000000ULL, 0xfffffffc00000002ULL)
    CV(35, _mm_cvttpd_epi32(dR), 0x0000000000000000ULL, 0xfffffffd00000002ULL)
    CV(36, _mm_cvtpd_epi32(dBig), 0x0000000000000000ULL, 0x8000000080000000ULL)
    CV(37, _mm_cvtpd_epi32(dH), 0x0000000000000000ULL, 0x8000000080000000ULL)
    CV(38, _mm_cvttpd_epi32(dN), 0x0000000000000000ULL, 0x0000000480000000ULL)
    CV(39, _mm_cvtps_epi32(fA), 0x8000000080000000ULL, 0xfffffffe00000002ULL)
    CV(40, _mm_cvttps_epi32(fA), 0x8000000080000000ULL, 0xfffffffe00000001ULL)
    CV(41, _mm_cvtepi32_pd(iC), 0xc1e0000000000000ULL, 0xc1e0000000000000ULL)
    CV(42, _mm_cvtepi32_ps(iC), 0x4f0000003f800000ULL, 0xcf000000cf000000ULL)
    CV(43, _mm_cvtpd_ps(dI), 0x0000000000000000ULL, 0xff8000007f800000ULL)
    CV(44, _mm_cvtps_pd(fA), 0xc004000000000000ULL, 0x3ff8000000000000ULL)
    CV(45, _mm_cvtsd_ss(fA, dA), 0x7fc000004f32d05eULL, 0xc02000003fc00000ULL)
    CV(46, _mm_cvtss_sd(dA, fA), 0xc002000000000000ULL, 0x3ff8000000000000ULL)
    CV(47, _mm_cvtsi32_sd(dA, -7), 0xc002000000000000ULL, 0xc01c000000000000ULL)
    CV(48, _mm_cvtsi64_sd(dA, 0x7fffffffffffffffLL), 0xc002000000000000ULL, 0x43e0000000000000ULL)
    CI(49, _mm_cvtsd_si32(dR), 0x2ULL)
    CI(50, _mm_cvttsd_si32(dBig), 0xffffffff80000000ULL)
    CI(51, _mm_cvtsd_si64(dH), 0x80000000ULL)
    CI(52, _mm_cvttsd_si64(_mm_unpackhi_pd(dH, dH)), 0x8000000000000000ULL)
    CI(53, _mm_cvtsd_si64x(_mm_set_sd(-3.5)), 0xfffffffffffffffcULL)
    CM(54, _mm_cvtpd_pi32(dR), 0xfffffffc00000002ULL)
    CM(55, _mm_cvttpd_pi32(dR), 0xfffffffd00000002ULL)
    CV(56, _mm_cvtpi32_pd(_mm_movepi64_pi64(iC)), 0xc1e0000000000000ULL, 0xc1e0000000000000ULL)
    CV(57, _mm_unpackhi_pd(dA, dB), 0x4008000000000000ULL, 0xc002000000000000ULL)
    CV(58, _mm_unpacklo_pd(dA, dB), 0x8000000000000000ULL, 0x3ff8000000000000ULL)
    CV(59, _mm_shuffle_pd(dA, dB, 1), 0x8000000000000000ULL, 0xc002000000000000ULL)
    CV(60, _mm_shuffle_pd(dA, dB, _MM_SHUFFLE2(1, 0)), 0x4008000000000000ULL, 0x3ff8000000000000ULL)
    CI(61, _mm_movemask_pd(dA), 0x2ULL)
    CV(62, _mm_move_sd(dA, dB), 0xc002000000000000ULL, 0x8000000000000000ULL)
    CV(63, _mm_add_epi8(iA, iB), 0x7fff7f0010101010ULL, 0x8082027f81fdfe82ULL)
    CV(64, _mm_add_epi16(iA, iB), 0x7fff800011101110ULL, 0x8082027f82fdfe82ULL)
    CV(65, _mm_add_epi32(iA, iB), 0x7fff800011111110ULL, 0x8083027f82fdfe82ULL)
    CV(66, _mm_add_epi64(iA, iC), 0x00007ffe12345679ULL, 0xff80ff0081fe8001ULL)
    CV(67, _mm_sub_epi8(iA, iB), 0x81017ffe14589ce0ULL, 0x7e7efc8181ff0280ULL)
    CV(68, _mm_sub_epi16(iA, iB), 0x80017ffe13589be0ULL, 0x7e7efb8180ff0180ULL)
    CV(69, _mm_sub_epi32(iA, iC), 0x0000800012345677ULL, 0xff80ff0081fe8001ULL)
    CV(70, _mm_sub_epi64(iC, iA), 0xffff7fffedcba989ULL, 0x007f01007e017fffULL)
    CV(71, _mm_adds_epi8(iA, iB), 0x80ff7f0010101010ULL, 0x7f82027f81fdfe82ULL)
    CV(72, _mm_adds_epi16(iA, iB), 0x80007fff11101110ULL, 0x7fff027f82fdfe82ULL)
    CV(73, _mm_adds_epu8(iA, iB), 0xffff7fffffffffffULL, 0x8082ff7f81fffe82ULL)
    CV(74, _mm_adds_epu16(iA, iB), 0xffff8000ffffffffULL, 0x8082ffff82fdfe82ULL)
    CV(75, _mm_subs_epi8(iA, iB), 0x81017ffe14587f7fULL, 0x7e80fc817fff807fULL)
    CV(76, _mm_subs_epi16(iA, iB), 0x80017ffe13587fffULL, 0x7e7efb817fff8000ULL)
    CV(77, _mm_subs_epu8(iA, iB), 0x00007ffe00000000ULL, 0x7e7efc0000000200ULL)
    CV(78, _mm_subs_epu16(iA, iB), 0x00007ffe00000000ULL, 0x7e7efb8100000180ULL)
    CV(79, _mm_madd_epi16(iA, iB), 0x0000ffffe879c3f0ULL, 0x007d0000bfc2fa83ULL)
    CV(80, _mm_madd_epi16(_mm_set1_epi16(-32768), _mm_set1_epi16(-32768)), 0x8000000080000000ULL, 0x8000000080000000ULL)
    CV(81, _mm_mulhi_epi16(iA, iB), 0x00000000ffebe88eULL, 0x0080fffcff02c0bfULL)
    CV(82, _mm_mulhi_epu16(iA, iB), 0x7fff0000121f3f06ULL, 0x0080037b01003f40ULL)
    CV(83, _mm_mullo_epi16(iA, iB), 0x80007fff3cb08740ULL, 0x7f008100fc02fe81ULL)
    CV(84, _mm_mul_epu32(iA, iB), 0x121fa00a35068740ULL, 0x01013d7e453dfe81ULL)
    CM(85, _mm_mul_su32(_mm_movepi64_pi64(iA), _mm_movepi64_pi64(iB)), 0x01013d7e453dfe81ULL)
    CV(86, _mm_max_epi16(iA, iB), 0xffff7fff12345678ULL, 0x7f80037f01fe7e81ULL)
    CV(87, _mm_min_epi16(iA, iB), 0x80000001fedcba98ULL, 0x0102ff0080ff8001ULL)
    CV(88, _mm_max_epu8(iA, iB), 0xffff7ffffedcba98ULL, 0x7f80ff7f80ff8081ULL)
    CV(89, _mm_min_epu8(iA, iB), 0x8000000112345678ULL, 0x0102030001fe7e01ULL)
    CV(90, _mm_avg_epu8(iA, iB), 0xc080408088888888ULL, 0x4041814041ff7f41ULL)
    CV(91, _mm_avg_epu16(iA, iB), 0xc000400088888888ULL, 0x40418140417f7f41ULL)
    CV(92, _mm_sad_epu8(iA, iB), 0x0000000000000513ULL, 0x0000000000000379ULL)
    CV(93, _mm_and_si128(iA, iB), 0x8000000112141218ULL, 0x0100030000fe0001ULL)
    CV(94, _mm_andnot_si128(iA, iB), 0x7fff0000ecc8a880ULL, 0x0002007f80017e80ULL)
    CV(95, _mm_or_si128(iA, iB), 0xffff7ffffefcfef8ULL, 0x7f82ff7f81fffe81ULL)
    CV(96, _mm_xor_si128(iA, iB), 0x7fff7ffeece8ece0ULL, 0x7e82fc7f8101fe80ULL)
    CV(97, _mm_cmpeq_epi8(iA, iA), 0xffffffffffffffffULL, 0xffffffffffffffffULL)
    CV(98, _mm_cmpeq_epi16(iA, iB), 0x0000000000000000ULL, 0x0000000000000000ULL)
    CV(99, _mm_cmpeq_epi32(iC, iC), 0xffffffffffffffffULL, 0xffffffffffffffffULL)
    CV(100, _mm_cmpgt_epi8(iA, iB), 0x00ffff00ffffffffULL, 0xff000000ff0000ffULL)
    CV(101, _mm_cmpgt_epi16(iA, iB), 0x0000ffffffffffffULL, 0xffff0000ffff0000ULL)
    CV(102, _mm_cmpgt_epi32(iA, iC), 0x00000000ffffffffULL, 0xffffffffffffffffULL)
    CV(103, _mm_cmplt_epi8(iA, iB), 0xff0000ff00000000ULL, 0x00ffffff00ffff00ULL)
    CV(104, _mm_cmplt_epi16(iA, iB), 0xffff000000000000ULL, 0x0000ffff0000ffffULL)
    CV(105, _mm_cmplt_epi32(iA, iC), 0xffffffff00000000ULL, 0x0000000000000000ULL)
    CV(106, _mm_slli_epi16(iA, 4), 0x0000fff023406780ULL, 0xf800f0001fe00010ULL)
    CV(107, _mm_slli_epi32(iA, sh), 0x0003fff891a2b3c0ULL, 0xfc07f8000ff40008ULL)
    CV(108, _mm_slli_epi64(iA, 64), 0x0000000000000000ULL, 0x0000000000000000ULL)
    CV(109, _mm_srli_epi16(iA, 15), 0x0001000000000000ULL, 0x0000000100000001ULL)
    CV(110, _mm_srli_epi32(iA, 16), 0x0000800000001234ULL, 0x00007f80000001feULL)
    CV(111, _mm_srli_epi64(iA, 1), 0x40003fff891a2b3cULL, 0x3fc07f8000ff4000ULL)
    CV(112, _mm_srai_epi16(iA, 3), 0xf0000fff02460acfULL, 0x0ff0ffe0003ff000ULL)
    CV(113, _mm_srai_epi32(iC, 40), 0x0000000000000000ULL, 0xffffffffffffffffULL)
    CV(114, _mm_sll_epi16(iA, cnt), 0x0000ffe04680cf00ULL, 0xf000e0003fc00020ULL)
    CV(115, _mm_sll_epi32(iA, cbig), 0x0000000000000000ULL, 0x0000000000000000ULL)
    CV(116, _mm_sll_epi64(iA, cnt), 0x000fffe2468acf00ULL, 0xf01fe0003fd00020ULL)
    CV(117, _mm_srl_epi16(iA, cnt), 0x040003ff009102b3ULL, 0x03fc07f8000f0400ULL)
    CV(118, _mm_srl_epi32(iA, cnt), 0x040003ff0091a2b3ULL, 0x03fc07f8000ff400ULL)
    CV(119, _mm_srl_epi64(iA, cbig), 0x0000000000000000ULL, 0x0000000000000000ULL)
    CV(120, _mm_sra_epi16(iA, cbig), 0xffff000000000000ULL, 0x0000ffff0000ffffULL)
    CV(121, _mm_sra_epi32(iA, cnt), 0xfc0003ff0091a2b3ULL, 0x03fc07f8000ff400ULL)
    CV(122, _mm_slli_si128(iA, 3), 0xff123456787f80ffULL, 0x0001fe8001000000ULL)
    CV(123, _mm_srli_si128(iA, 5), 0x000000000080007fULL, 0xff123456787f80ffULL)
    CV(124, _mm_bslli_si128(iA, 16), 0x0000000000000000ULL, 0x0000000000000000ULL)
    CV(125, _mm_bsrli_si128(iA, 15), 0x0000000000000000ULL, 0x0000000000000080ULL)
    CV(126, _mm_packs_epi16(iA, iB), 0xff0180807f7f807fULL, 0x807f7f7f7f807f80ULL)
    CV(127, _mm_packs_epi32(iA, iC), 0x7fff000180008000ULL, 0x80007fff7fff7fffULL)
    CV(128, _mm_packus_epi16(iA, iB), 0x00010000ffff00ffULL, 0x00ffffffff00ff00ULL)
    CV(129, _mm_unpackhi_epi8(iA, iB), 0xff80ff00007f01ffULL, 0xfe12dc34ba569878ULL)
    CV(130, _mm_unpackhi_epi16(iA, iB), 0xffff800000017fffULL, 0xfedc1234ba985678ULL)
    CV(131, _mm_unpackhi_epi32(iA, iB), 0xffff000180007fffULL, 0xfedcba9812345678ULL)
    CV(132, _mm_unpackhi_epi64(iA, iB), 0xffff0001fedcba98ULL, 0x80007fff12345678ULL)
    CV(133, _mm_unpacklo_epi8(iA, iB), 0x017f028003ff7f00ULL, 0x8001fffe7e808101ULL)
    CV(134, _mm_unpacklo_epi16(iA, iB), 0x01027f80037fff00ULL, 0x80ff01fe7e818001ULL)
    CV(135, _mm_unpacklo_epi32(iA, iB), 0x0102037f7f80ff00ULL, 0x80ff7e8101fe8001ULL)
    CV(136, _mm_unpacklo_epi64(iA, iB), 0x0102037f80ff7e81ULL, 0x7f80ff0001fe8001ULL)
    CI(137, _mm_movemask_epi8(iA), 0x9066ULL)
    CI(138, _mm_extract_epi16(iA, 6), 0x7fffULL)
    CV(139, _mm_insert_epi16(iA, 0x12345, 2), 0x80007fff12345678ULL, 0x7f80234501fe8001ULL)
    CV(140, _mm_shuffle_epi32(iA, 0x1b), 0x01fe80017f80ff00ULL, 0x1234567880007fffULL)
    CV(141, _mm_shufflelo_epi16(iA, _MM_SHUFFLE(0, 1, 2, 3)), 0x80007fff12345678ULL, 0x800101feff007f80ULL)
    CV(142, _mm_shufflehi_epi16(iA, 0xe1), 0x80007fff56781234ULL, 0x7f80ff0001fe8001ULL)
    CV(143, _mm_set_epi8(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,-16), 0x0102030405060708ULL, 0x090a0b0c0d0e0ff0ULL)
    CV(144, _mm_setr_epi8(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,-16), 0xf00f0e0d0c0b0a09ULL, 0x0807060504030201ULL)
    CV(145, _mm_set_epi16(1,2,3,4,5,6,7,-8), 0x0001000200030004ULL, 0x000500060007fff8ULL)
    CV(146, _mm_setr_epi16(1,2,3,4,5,6,7,-8), 0xfff8000700060005ULL, 0x0004000300020001ULL)
    CV(147, _mm_set_epi32(1,2,3,-4), 0x0000000100000002ULL, 0x00000003fffffffcULL)
    CV(148, _mm_setr_epi32(1,2,3,-4), 0xfffffffc00000003ULL, 0x0000000200000001ULL)
    CV(149, _mm_set_epi64x(1, -2), 0x0000000000000001ULL, 0xfffffffffffffffeULL)
    CV(150, _mm_set1_epi8(-3), 0xfdfdfdfdfdfdfdfdULL, 0xfdfdfdfdfdfdfdfdULL)
    CV(151, _mm_set1_epi16(-4), 0xfffcfffcfffcfffcULL, 0xfffcfffcfffcfffcULL)
    CV(152, _mm_set1_epi32(-5), 0xfffffffbfffffffbULL, 0xfffffffbfffffffbULL)
    CV(153, _mm_set1_epi64x(-6), 0xfffffffffffffffaULL, 0xfffffffffffffffaULL)
    CV(154, _mm_set_epi64(_mm_movepi64_pi64(iA), _mm_movepi64_pi64(iB)), 0x7f80ff0001fe8001ULL, 0x0102037f80ff7e81ULL)
    CV(155, _mm_set_pd(1.0, 2.0), 0x3ff0000000000000ULL, 0x4000000000000000ULL)
    CV(156, _mm_setr_pd(1.0, 2.0), 0x4000000000000000ULL, 0x3ff0000000000000ULL)
    CV(157, _mm_set1_pd(-1.0), 0xbff0000000000000ULL, 0xbff0000000000000ULL)
    CV(158, _mm_set_sd(5.0), 0x0000000000000000ULL, 0x4014000000000000ULL)
    CV(159, _mm_setzero_pd(), 0x0000000000000000ULL, 0x0000000000000000ULL)
    CV(160, _mm_setzero_si128(), 0x0000000000000000ULL, 0x0000000000000000ULL)
    CV(161, _mm_load_pd(mem), 0xc028000000000000ULL, 0x4026000000000000ULL)
    CV(162, _mm_loadu_pd(mem + 1), 0x402b000000000000ULL, 0xc028000000000000ULL)
    CV(163, _mm_loadr_pd(mem + 2), 0x402b000000000000ULL, 0x3fc0000000000000ULL)
    CV(164, _mm_load1_pd(mem + 3), 0x3fc0000000000000ULL, 0x3fc0000000000000ULL)
    CV(165, _mm_load_sd(mem + 1), 0x0000000000000000ULL, 0xc028000000000000ULL)
    CV(166, _mm_loadh_pd(dA, mem), 0x4026000000000000ULL, 0x3ff8000000000000ULL)
    CV(167, _mm_loadl_pd(dA, mem + 3), 0xc002000000000000ULL, 0x3fc0000000000000ULL)
    CV(168, _mm_load_si128((__m128i *)bytes), 0xb6997c5f422508ebULL, 0xceb194775a3d2003ULL)
    CV(169, _mm_loadu_si128((__m128i_u *)(bytes + 1)), 0xd3b6997c5f422508ULL, 0xebceb194775a3d20ULL)
    CV(170, _mm_loadl_epi64((__m128i_u *)(bytes + 2)), 0x0000000000000000ULL, 0x08ebceb194775a3dULL)
    CV(171, _mm_loadu_si32(bytes + 3), 0x0000000000000000ULL, 0x00000000b194775aULL)
    CV(172, _mm_loadu_si16(bytes + 5), 0x0000000000000000ULL, 0x000000000000b194ULL)
    CI(173, _mm_cvtsi128_si32(iA), 0x1fe8001ULL)
    CI(174, _mm_cvtsi128_si64(iB), 0x102037f80ff7e81ULL)
    CV(175, _mm_cvtsi32_si128(-9), 0x0000000000000000ULL, 0x00000000fffffff7ULL)
    CV(176, _mm_cvtsi64_si128(-10), 0x0000000000000000ULL, 0xfffffffffffffff6ULL)
    CV(177, _mm_move_epi64(iA), 0x0000000000000000ULL, 0x7f80ff0001fe8001ULL)
    CM(178, _mm_movepi64_pi64(iB), 0x0102037f80ff7e81ULL)
    CV(179, _mm_movpi64_epi64(_mm_movepi64_pi64(iA)), 0x0000000000000000ULL, 0x7f80ff0001fe8001ULL)
    CV(180, _mm_castpd_si128(dA), 0xc002000000000000ULL, 0x3ff8000000000000ULL)
    CV(181, _mm_castps_si128(fA), 0x7fc000004f32d05eULL, 0xc02000003fc00000ULL)
    CV(182, _mm_castsi128_pd(iA), 0x80007fff12345678ULL, 0x7f80ff0001fe8001ULL)
    /* Stores, streaming stores and fences. */
    {
        double d[2] __attribute__((aligned(16)));
        unsigned char b[16] __attribute__((aligned(16)));
        _mm_store_pd(d, dA); if (d[0] != 1.5 || d[1] != -2.25) return 900;
        _mm_storer_pd(d, dA); if (d[0] != -2.25 || d[1] != 1.5) return 901;
        _mm_storeh_pd(d, dB); if (d[0] != 3.0) return 902;
        _mm_store1_pd(d, dB); if (d[1] != 0.0 || !__builtin_signbit(d[1])) return 903;
        memset(b, 0, 16);
        _mm_maskmoveu_si128(_mm_set1_epi8(7), _mm_setr_epi8(-1,0,-128,1,0,0,0,0,0,0,0,0,0,0,0,(char)0x80), (char *)b);
        if (b[0] != 7 || b[1] != 0 || b[2] != 7 || b[3] != 0 || b[15] != 7) return 904;
        _mm_storeu_si32(b + 1, _mm_cvtsi32_si128(0x04030201)); if (b[1] != 1 || b[4] != 4 || b[5] != 0) return 905;
        _mm_stream_si128((__m128i *)b, iA); _mm_mfence(); _mm_lfence(); _mm_clflush(b);
        if (memcmp(b, &iA, 16) != 0) return 906;
        int s32; _mm_stream_si32(&s32, -42); if (s32 != -42) return 907;
    }
    return 0;
}
