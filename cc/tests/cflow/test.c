/* Test file for cflow */
int h();
int global_var;

int f() {
    global_var = h();
    return global_var;
}

int g() {
    return 0;
}

int main() {
    f();
    g();
    f();
    return 0;
}
