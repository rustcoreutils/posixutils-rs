/* Test file for cxref */
int counter;

int increment() {
    counter++;
    return counter;
}

int decrement() {
    counter--;
    return counter;
}

int main() {
    counter = 0;
    increment();
    increment();
    decrement();
    return counter;
}
