// Test: External libc function call (puts)
// Expected exit code: 0
// Expected stdout: Hello from libc!

int puts(char *s);

int main(void) {
    puts("Hello from libc!");
    return 0;
}
