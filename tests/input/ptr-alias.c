int main() {
    int a = 15;
    int* b = &a;
    int* c = b;
    *b = 10;
    *c = 3;
    return a;
}