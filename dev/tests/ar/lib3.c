
int l3_sum(int a, int b) {
	return a + b;
}

int l3_difference(int a, int b) {
	return a - b;
}

int l3_product(int a, int b) {
	return a * b;
}

double l3_division(int a, int b) {
	if (b != 0) {
		return (double) a / b;
	} else {
		return 0.0;
	}
}

int l3_square(int a) {
	return a * a;
}

int l3_factorial(int n) {
	if (n == 0) {
		return 1;
	} else {
		return n * l3_factorial(n - 1);
	}
}