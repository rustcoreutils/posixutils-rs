// Test: Internal function calls + __func__ predefined identifier
// Expected exit code: 14

int sum(int a, int b) {
    // Verify __func__ is "sum" (first char 's' = 115)
    if (__func__[0] != 115) return 100;
    return a + b;
}

int double_it(int x) {
    // Verify __func__ is "double_it" (first char 'd' = 100)
    if (__func__[0] != 100) return 101;
    return x + x;
}

int main(void) {
    // Verify __func__ is "main" (first char 'm' = 109)
    if (__func__[0] != 109) return 102;

    int x = sum(3, 4);      // 7
    int y = double_it(x);   // 14
    return y;
}
