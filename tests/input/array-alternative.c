int main() {
    int a[15];
    *(a+10) = 5;
    return a[10];
}