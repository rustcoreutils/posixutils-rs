/* Test file for ctags */
typedef int myint;

#define MAX(a,b) ((a)>(b)?(a):(b))

int global_var;

int foo(int x) {
    return x + 1;
}

int bar(int a, int b) {
    return a + b;
}

int main() {
    int result = foo(1);
    result = bar(result, 2);
    return result;
}
