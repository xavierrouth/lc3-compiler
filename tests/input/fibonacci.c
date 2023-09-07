int fib(int n) {
    if (2 > n) {
        return n;
    }
    return fib(n-1) + fib(n-2);
}

int main() {
    return fib(11);
}