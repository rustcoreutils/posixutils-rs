// Test: Combined internal and external function calls
// Expected exit code: 14
// Expected stdout: Function calls work!

int puts(char *s);

int sum(int a, int b) {
    return a + b;
}

int double_it(int x) {
    return x + x;
}

int main(void) {
    int x = sum(3, 4);      // 7
    int y = double_it(x);   // 14
    puts("Function calls work!");
    return y;
}
