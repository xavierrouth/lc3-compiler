struct node {
    int padding2;
    int data;
    int padding1;
};

int main() {
    struct node hi;
    struct node hi2;
    hi.data = 10;
    hi2.data = hi.data;
    int a = hi2.data;
    return a;
}