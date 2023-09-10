void maxArray(int* x, int* y) {
    int i;

    for (i = 0; i < 3; i = i + 1) {
        if (y[i] > x[i]) {
            x[i] = y[i];
        }
    }
    return;
}

int main() {
    int a[3];
    a[0] = 1;
    a[1] = 3;
    a[2] = 4;
    int b[3];
    b[0] = 3;
    b[1] = 1;
    b[2] = 1;

    maxArray(a, b);

    return a[0] + a[1] + a[2];
}