volatile int sink;
int depth(void);

__attribute__((noinline, noclone)) int pressure(int a, double x) {
    int b = a * 3, c = a * 5, d = a * 7, e = a * 11, f = a * 13;
    double y = x * 2, z = x * 3, w = x * 5;
    int r = depth();
    sink = b + c + d + e + f + (int)(y + z + w);
    return r;
}

__attribute__((noinline, noclone)) int dyn(int n) {
    char *p = __builtin_alloca(n);
    char vla[n + 1];
    __builtin_memset(p, 1, n);
    __builtin_memset(vla, 2, n + 1);
    sink = p[0] + vla[n];
    return pressure(n, 1.5) + sink - sink;
}

__attribute__((noinline, noclone)) int big(int n) {
    volatile char buf[8200];
    buf[n] = 1;
    return dyn(n) + buf[n] - 1;
}

__attribute__((noinline, noclone)) int mid(int n) {
    volatile char buf[1000];
    buf[n] = 1;
    return big(n) + buf[n] - 1;
}

struct ov { _Alignas(64) char c[64]; };

__attribute__((noinline, noclone)) int aligned(int n) {
    struct ov o;
    __builtin_memset(&o, n, sizeof o);
    sink = o.c[3];
    return mid(n) + sink - sink;
}

__attribute__((noinline, noclone)) int varargs(int n, ...) {
    __builtin_va_list ap;
    __builtin_va_start(ap, n);
    int m = __builtin_va_arg(ap, int);
    __builtin_va_end(ap);
    if (m == 99)
        return -1;
    return aligned(m) + sink - sink;
}
