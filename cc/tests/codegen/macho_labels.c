int g(int);
static const char *names[] = { "zero", "one", "two" };
const char *greeting = "hello";
int *file_cl = (int[]){ 4, 5, 6 };
const int *wide = (const int *)L"wide";
_Thread_local int tls_counter;
double scale(double x) { return x * 1.25 + 0.5; }
int epilogue(int x) { if (x) return g(x) + 1; return 3; }
int many_returns(int x) {
    for (int i = 0; i < x; i++) {
        if (g(i) == 7) return i;
        if (g(i) < 0) return -i;
    }
    return x > 3 ? 1 : 2;
}
int dispatch(int op) {
    static void *table[] = { &&add, &&sub, &&done };
    int acc = 0;
    goto *table[op % 3];
add: acc += 2; goto done;
sub: acc -= 2;
done: return acc;
}
int müller(int x) {
    void *targets[] = { &&lab0, &&lab1 };
    goto *targets[x & 1];
lab0: return 1;
lab1: return 2;
}
int sw(int v) {
    switch (v) {
    case 0: return g(10); case 1: return g(11); case 2: return g(12);
    case 3: return g(13); case 4: return g(14); case 5: return g(15);
    case 6: return g(16); case 7: return g(17); default: return -1;
    }
}
int jumped(int x) {
#if defined(__aarch64__)
    __asm__ goto ("cbz %w0, %l[out]" : : "r"(x) : : out);
#else
    __asm__ goto ("testl %0, %0; jz %l[out]" : : "r"(x) : : out);
#endif
    return 1;
out:
    return 0;
}
int big_frame(int i) {
    volatile char buf[70000];
    buf[i] = (char)i;
    return buf[69999 - i] + g(i);
}
int vla(int n) {
    int a[n];
    for (int i = 0; i < n; i++) a[i] = g(i);
    return n ? a[n - 1] : 0;
}
int bump(void) { return ++tls_counter; }
const char *name_of(int i) { return names[i % 3]; }
__attribute__((constructor)) static void init(void) { tls_counter = 1; }
__attribute__((destructor)) static void fini(void) { tls_counter = 0; }
long double ld(long double x) { return x * 3.0L; }
/* gcc's builtin setjmp stores the address of a label of its own. */
static void *resume_buf[5];
void nonlocal_jump(void) { __builtin_longjmp(resume_buf, 1); }
int nonlocal_target(void) { return __builtin_setjmp(resume_buf) ? g(1) : g(0); }
/* GNU label differences: a jump table of offsets from a base label, at
   every entry width, as glibc's vfprintf builds one. */
int offsets(int op) {
    static const signed char b[] = { &&o1 - &&o0, &&o2 - &&o0 };
    static const short s[] = { &&o2 - &&o0 + 1 };
    static const int w[] = { &&o0 - &&o0, &&o1 - &&o0, &&o2 - &&o0 };
    static const long q[] = { &&o2 - &&o1 - 1 };
    goto *(&&o0 + w[op % 3] + b[0] - b[0] + s[0] - s[0] + q[0] - q[0]);
o0: return g(0);
o1: return g(1);
o2: return g(2);
}
