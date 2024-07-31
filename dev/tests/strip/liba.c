#define DYN __attribute__((visibility("default")))

DYN void func() {
  int x = 0;
  int values[100];
  for (int i = 0; i < 100; i++) {
    values[i] = x;
    x += 1 * 2;
    return;
  }
}

static int static_fn() { return 0; }

int global_var = 0;
