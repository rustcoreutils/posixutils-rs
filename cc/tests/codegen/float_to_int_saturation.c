/* A floating constant converted to an integer type it does not fit: C17
   6.3.1.4p1 leaves the result undefined, and gcc's constant folder
   saturates -- to the type's maximum above it, its minimum below it (0 for
   an unsigned type), and 0 for a NaN. It does so wherever it folds: a static
   initializer, an integer constant expression, a cast or an implicit
   conversion of a constant in code at every level, and from -O1 a constant
   that reaches the conversion through a variable. Every answer here is gcc
   13's on x86-64 and on aarch64, at -O0, -O1 and -O2. A conversion of a value
   not known until run time is the hardware's -- x86-64 gives the minimum,
   aarch64 saturates -- so the variable rows are checked only when
   optimizing. */
#include <limits.h>

typedef __int128 i128;
typedef unsigned __int128 u128;
#define I128_MAX ((i128)(~(u128)0 >> 1))
#define I128_MIN (-I128_MAX - 1)
#define U128_MAX (~(u128)0)
#define INF __builtin_inf()
#define QNAN __builtin_nan("")
#define NI __attribute__((noinline))

/* X(n, target, source, value, gcc's answer). Each value is converted to the
   source type first, then to the target. */
#define ROWS(X) \
    X(1, _Bool, double, QNAN, 1) \
    X(2, _Bool, float, 0.5, 1) \
    X(3, _Bool, long double, INF, 1) \
    X(4, signed char, double, 1e10, SCHAR_MAX) \
    X(5, signed char, float, -129.0, SCHAR_MIN) \
    X(6, signed char, double, 128.0, SCHAR_MAX) \
    X(7, signed char, long double, QNAN, 0) \
    X(8, unsigned char, double, 300.0, UCHAR_MAX) \
    X(9, unsigned char, float, -1.0, 0) \
    X(10, unsigned char, _Float128, 256.0, UCHAR_MAX) \
    X(11, short, double, 32768.0, SHRT_MAX) \
    X(12, short, double, -32769.0, SHRT_MIN) \
    X(13, short, float, -INF, SHRT_MIN) \
    X(14, unsigned short, float, 65536.0, USHRT_MAX) \
    X(15, unsigned short, double, -1.0, 0) \
    X(16, int, float, 2147483648.0, INT_MAX) \
    X(17, int, float, 2147483647.0, INT_MAX) \
    X(18, int, double, -2147483649.0, INT_MIN) \
    X(19, int, double, 2147483647.9, INT_MAX) \
    X(20, int, double, -2147483648.9, INT_MIN) \
    X(21, int, double, 1e300, INT_MAX) \
    X(22, int, double, -1e10, INT_MIN) \
    X(23, int, double, QNAN, 0) \
    X(24, int, double, -QNAN, 0) \
    X(25, int, long double, INF, INT_MAX) \
    X(26, int, long double, 1e4000L, INT_MAX) \
    X(27, int, _Float128, -INF, INT_MIN) \
    X(28, unsigned int, double, -1.0, 0) \
    X(29, unsigned int, double, -0.5, 0) \
    X(30, unsigned int, double, 4294967296.0, UINT_MAX) \
    X(31, unsigned int, float, QNAN, 0) \
    X(32, unsigned int, float, INF, UINT_MAX) \
    X(33, unsigned int, double, -INF, 0) \
    X(34, long, double, 0x1p63, LONG_MAX) \
    X(35, long, double, -0x1p64, LONG_MIN) \
    X(36, long, float, 1e30, LONG_MAX) \
    X(37, long long, double, 1e300, LLONG_MAX) \
    X(38, long long, double, -0x1p63, LLONG_MIN) \
    X(39, long long, long double, -0x1p63L - 1.0L, LLONG_MIN) \
    X(40, long long, _Float128, QNAN, 0) \
    X(41, unsigned long, double, 0x1p64, ULONG_MAX) \
    X(42, unsigned long, float, -1.0, 0) \
    X(43, unsigned long long, double, 1e19, 10000000000000000000ULL) \
    X(44, unsigned long long, long double, 1e300, ULLONG_MAX) \
    X(45, unsigned long long, _Float128, -INF, 0) \
    X(46, __int128, double, 1e300, I128_MAX) \
    X(47, __int128, double, -1e300, I128_MIN) \
    X(48, __int128, long double, 0x1p127L, I128_MAX) \
    X(49, __int128, float, INF, I128_MAX) \
    X(50, __int128, double, QNAN, 0) \
    X(51, __int128, _Float128, -0x1p127L, I128_MIN) \
    X(52, unsigned __int128, long double, 0x1p128L, U128_MAX) \
    X(53, unsigned __int128, double, 1e300, U128_MAX) \
    X(54, unsigned __int128, float, -1.0, 0) \
    X(55, unsigned __int128, _Float128, INF, U128_MAX) \
    X(56, unsigned __int128, double, QNAN, 0)

/* A static initializer, a cast in code, an implicit conversion in code, and
   a constant that reaches the conversion only through a variable -- which
   only the optimizer folds; -O0 converts it at run time. */
#define DEFINE(n, T, S, V, W) \
    static T s##n = (T)(S)(V); \
    NI static T cast##n(void) { return (T)(S)(V); } \
    NI static T implicit##n(void) { T r = (S)(V); return r; } \
    NI static T var##n(void) { S x = (S)(V); return (T)x; }
ROWS(DEFINE)

static int check(int n, u128 got, u128 want)
{
    return got == want ? 0 : n;
}

/* Not for a 128-bit target: c17 turns that conversion into a libgcc call
   (__fixdfti and its siblings) before its optimizer runs, so a constant that
   reaches one through a variable is converted at run time at every level,
   where gcc folds it from -O1. */
#ifdef __OPTIMIZE__
#define CHECK_VAR(n, T, W) \
    if (sizeof(T) < 16 && check(n, (u128)var##n(), (u128)(T)(W))) return 180 + n;
#else
#define CHECK_VAR(n, T, W)
#endif

#define CHECK(n, T, S, V, W) \
    { \
        static T b = (T)(S)(V); \
        if (check(n, (u128)s##n, (u128)(T)(W))) return n; \
        if (check(n, (u128)b, (u128)(T)(W))) return n; \
        if (check(n, (u128)cast##n(), (u128)(T)(W))) return 60 + n; \
        if (check(n, (u128)implicit##n(), (u128)(T)(W))) return 120 + n; \
        CHECK_VAR(n, T, W) \
    }

static int rows(void)
{
    ROWS(CHECK)
    return 0;
}

/* Integer constant expressions: gcc saturates in each, with a warning. */
enum {
    E1 = (int)1e10,
    E2 = (int)-1e10,
    E3 = (int)__builtin_nan(""),
    E4 = (unsigned char)300.0,
    E5 = (short)-1e10,
    E6 = (unsigned)-1.0
};
_Static_assert((int)2147483648.0f == INT_MAX, "float just past INT_MAX");
_Static_assert((int)(float)2147483647 == INT_MAX, "rounds up past INT_MAX");
_Static_assert((unsigned)-1.0 == 0, "negative to unsigned");
_Static_assert((int)__builtin_nan("") == 0, "NaN");
_Static_assert((long long)1e300 == LLONG_MAX, "huge");
_Static_assert((long long)-__builtin_inf() == LLONG_MIN, "-inf");
_Static_assert(__builtin_constant_p((int)1e10), "a constant");
struct Width { unsigned f : (unsigned char)300.0 / 32; };
struct Align { _Alignas((unsigned char)1e10 / 16 + 1) char c; };
_Static_assert(_Alignof(struct Align) == 16, "alignment");

NI static int label(int x)
{
    switch (x) {
    case (int)1e10: return 1;
    case (int)-1e10: return 2;
    case (short)1e10: return 3;
    case (unsigned char)-1.0: return 4;
    default: return 0;
    }
}

NI static int take(int x) { return x; }
NI static int ret(void) { return 1e10; }
NI static unsigned assign(void) { unsigned u; u = -5.0; return u; }

static int contexts(void)
{
    if (E1 != INT_MAX || E2 != INT_MIN || E3 != 0 || E4 != 255 || E5 != SHRT_MIN || E6 != 0)
        return 1;
    struct Width w = {0};
    w.f = 0xff;
    if (w.f != 127) return 2;
    if (label(INT_MAX) != 1 || label(INT_MIN) != 2 || label(SHRT_MAX) != 3 || label(0) != 4)
        return 3;
    if (take(1e10) != INT_MAX || ret() != INT_MAX || assign() != 0) return 4;
    /* A VLA in gcc, sized at run time by the same fold. */
    char vla[(unsigned char)1e10];
    if (sizeof vla != 255) return 5;
    return 0;
}

int main(void)
{
    int r = rows();
    if (r) return r;
    r = contexts();
    if (r) return 240 + r;
    return 0;
}
