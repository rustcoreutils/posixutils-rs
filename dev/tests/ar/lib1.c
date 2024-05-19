int lib1_init(char* arg) {
	if (arg == 0) {
		return 0;
	} else {
		return 1 + 345;
	}
	return 2;
}

long long factorial(int n) {
    if (n == 0)
        return 1;
    else
        return n * factorial(n - 1);
}

int print_factorial(int n) {
    long long fact = factorial(n);
    return fact;
}