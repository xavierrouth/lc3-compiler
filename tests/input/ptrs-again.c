int main() {
    int a = 15;
    int d = 10;

    int* aptr = &a;
    int* dptr = &d;

    *aptr = *dptr;
    return a;
}