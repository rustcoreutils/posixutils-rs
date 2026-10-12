/* Floating-point values in registers: across blocks, across calls -- in
   V8-V15 on aarch64, which a callee preserves only the low 64 bits of --
   across setjmp, and under more pressure than there are registers. Each
   callee overwrites every register it may, so a value kept where a call
   destroys it comes back wrong. Exit status 0, or the check that failed. */
#include <setjmp.h>

typedef double v2d __attribute__((vector_size(16)));
typedef float v4f __attribute__((vector_size(16)));

/* Overwrites every caller-saved floating-point and vector register. */
__attribute__((noinline)) double trash_caller(double v) {
#if defined(__aarch64__)
    __asm__ volatile(
        "movi v0.16b, #0x55\n movi v1.16b, #0x55\n movi v2.16b, #0x55\n"
        "movi v3.16b, #0x55\n movi v4.16b, #0x55\n movi v5.16b, #0x55\n"
        "movi v6.16b, #0x55\n movi v7.16b, #0x55\n movi v16.16b, #0x55\n"
        "movi v17.16b, #0x55\n movi v18.16b, #0x55\n movi v19.16b, #0x55\n"
        "movi v20.16b, #0x55\n movi v21.16b, #0x55\n movi v22.16b, #0x55\n"
        "movi v23.16b, #0x55\n movi v24.16b, #0x55\n movi v25.16b, #0x55\n"
        "movi v26.16b, #0x55\n movi v27.16b, #0x55\n movi v28.16b, #0x55\n"
        "movi v29.16b, #0x55\n movi v30.16b, #0x55\n movi v31.16b, #0x55\n"
        "ins v8.d[1], v0.d[0]\n ins v9.d[1], v0.d[0]\n"
        "ins v10.d[1], v0.d[0]\n ins v11.d[1], v0.d[0]\n"
        "ins v12.d[1], v0.d[0]\n ins v13.d[1], v0.d[0]\n"
        "ins v14.d[1], v0.d[0]\n ins v15.d[1], v0.d[0]\n"
        ::: "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v16", "v17",
            "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25", "v26",
            "v27", "v28", "v29", "v30", "v31");
#elif defined(__x86_64__)
    __asm__ volatile(
        "pcmpeqd %%xmm0, %%xmm0\n pcmpeqd %%xmm1, %%xmm1\n"
        "pcmpeqd %%xmm2, %%xmm2\n pcmpeqd %%xmm3, %%xmm3\n"
        "pcmpeqd %%xmm4, %%xmm4\n pcmpeqd %%xmm5, %%xmm5\n"
        "pcmpeqd %%xmm6, %%xmm6\n pcmpeqd %%xmm7, %%xmm7\n"
        "pcmpeqd %%xmm8, %%xmm8\n pcmpeqd %%xmm9, %%xmm9\n"
        "pcmpeqd %%xmm10, %%xmm10\n pcmpeqd %%xmm11, %%xmm11\n"
        "pcmpeqd %%xmm12, %%xmm12\n pcmpeqd %%xmm13, %%xmm13\n"
        "pcmpeqd %%xmm14, %%xmm14\n pcmpeqd %%xmm15, %%xmm15\n"
        ::: "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
            "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14",
            "xmm15");
#endif
    return v + 1.0;
}

/* Writes d8-d15, which it must save and restore, and the high halves of
   v8-v15, which it need not; and keeps doubles of its own across a call. */
__attribute__((noinline)) double trash_callee(double v) {
#if defined(__aarch64__)
    __asm__ volatile(
        "movi v8.16b, #0x77\n movi v9.16b, #0x77\n movi v10.16b, #0x77\n"
        "movi v11.16b, #0x77\n movi v12.16b, #0x77\n movi v13.16b, #0x77\n"
        "movi v14.16b, #0x77\n movi v15.16b, #0x77\n"
        ::: "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15");
#endif
    double a = v * 0.25, b = v * 0.125;
    double c = trash_caller(a);
    return c - 1.0 + a + b + v * 0.125;
}

/* Doubles and floats live across calls and blocks, in a loop. */
__attribute__((noinline)) double work(const double *p, int n, float scale) {
    double s = 0.0, t = 1.0, q = p[0];
    float fs = scale, fh = 0.5f;
    for (int i = 0; i < n; i++) {
        double v = p[i];
        if (v > s)
            s = trash_caller(v) + q;
        else
            t = t * trash_callee(v) + 1.0;
        fs = fs * 1.5f;
        if (i & 1)
            q = q + trash_caller((double)fs);
        else
            fh = fh + (float)trash_callee(q);
    }
    return s * 1024.0 + t + fs + q + fh;
}

/* The same, every value in memory. */
__attribute__((noinline)) double work_ref(const double *p, int n, float scale) {
    volatile double s = 0.0, t = 1.0, q = p[0];
    volatile float fs = scale, fh = 0.5f;
    for (volatile int i = 0; i < n; i++) {
        volatile double v = p[i];
        if (v > s)
            s = trash_caller(v) + q;
        else
            t = t * trash_callee(v) + 1.0;
        fs = fs * 1.5f;
        if (i & 1)
            q = q + trash_caller((double)fs);
        else
            fh = fh + (float)trash_callee(q);
    }
    return s * 1024.0 + t + fs + q + fh;
}

/* More values across a call than V8-V15 hold, and every argument too. */
__attribute__((noinline)) double pressure(double a, double b, double c,
                                          double d, float e, double f,
                                          float g, double h) {
    double v0 = a + 1, v1 = b + 2, v2 = c + 3, v3 = d + 4, v4 = a * 5,
           v5 = b * 6, v6 = c * 7, v7 = d * 8, v8 = a - 9, v9 = b - 10;
    double r = trash_caller(a) + trash_callee(h);
    r += trash_caller(r);
    return v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + r + a + b + c +
           d + e + f + g + h;
}

__attribute__((noinline)) double pressure_ref(double a, double b, double c,
                                              double d, float e, double f,
                                              float g, double h) {
    volatile double v0 = a + 1, v1 = b + 2, v2 = c + 3, v3 = d + 4,
                    v4 = a * 5, v5 = b * 6, v6 = c * 7, v7 = d * 8,
                    v8 = a - 9, v9 = b - 10;
    volatile double r = trash_caller(a) + trash_callee(h);
    r += trash_caller(r);
    return v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + r + a + b + c +
           d + e + f + g + h;
}

/* Values wider than 64 bits, and a _Float16, live across calls: a long
   double (binary128 on aarch64 Linux, whose high half V8-V15 lose) and
   128-bit vectors. */
__attribute__((noinline)) int wide_across(double a) {
    v2d vec = {a, a * 2.0};
    v4f vf = {1.0f, 2.0f, 3.0f, (float)a};
    long double ld = (long double)a / 3.0L;
    _Float16 hf = (_Float16)a * (_Float16)0.5;
    double x = trash_callee(a) + trash_caller(a);
    x += trash_caller(x);
    vec = vec + vec;
    vf = vf * vf;
    volatile long double want_ld = (long double)a;
    want_ld = want_ld / 3.0L;
    if (vec[0] != a * 2.0 || vec[1] != a * 4.0)
        return 1;
    if (vf[0] != 1.0f || vf[2] != 9.0f || vf[3] != (float)a * (float)a)
        return 2;
    if (ld != want_ld)
        return 3;
    if (hf != (_Float16)(a * 0.5))
        return 4;
    return x > 0 ? 0 : 5;
}

static jmp_buf jb;

__attribute__((noinline)) void jump_trashing(void) {
    trash_callee(1.0);
    trash_caller(2.0);
    longjmp(jb, 1);
}

/* Values computed before setjmp and not changed after it survive the
   second return. */
__attribute__((noinline)) int across_setjmp(double a, float f) {
    double x = a * 3.0;
    float y = f * 2.0f;
    double z = trash_caller(a);
    if (setjmp(jb))
        return x == a * 3.0 && y == f * 2.0f && z == a + 1.0 ? 0 : 1;
    jump_trashing();
    return 2;
}

/* The case 8bdb25c1 fixed in the old allocator: `int_max` lives across the
   block boundary of the `&&`, and its register must not be reused for the
   reload of `dval` in the first comparison. */
__attribute__((noinline)) int in_range(double dval, double int_max) {
    if (-int_max < dval && dval < int_max)
        return 1;
    return 0;
}

__attribute__((noinline)) int in_long_range(double dval) {
    double int_max = (double)(1UL << 63);
    double m = dval * 0.5;
    if (-int_max < dval && dval < int_max && -int_max < m)
        return 1;
    return 0;
}

int main(void) {
    static const double p[] = {3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8};
    if (work(p, 12, 1.0f) != work_ref(p, 12, 1.0f))
        return 10;
    if (pressure(1, 2, 3, 4, 5, 6, 7, 8) != pressure_ref(1, 2, 3, 4, 5, 6, 7, 8))
        return 11;
    int w = wide_across(3.0);
    if (w)
        return 20 + w;
    if (across_setjmp(1.5, 2.5f))
        return 30;
    if (in_range(0.0, 2147483648.0) != 1 || in_range(3e9, 2147483648.0) != 0 ||
        in_range(-3e9, 2147483648.0) != 0 || in_range(-5.0, 2147483648.0) != 1)
        return 40;
    if (in_long_range(1e18) != 1 || in_long_range(1e19) != 0 ||
        in_long_range(-1e19) != 0)
        return 41;
    return 0;
}
