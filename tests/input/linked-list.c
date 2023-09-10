struct Node {
    int data;
    struct Node* next;
};

int main() {
    struct Node node1;
    struct Node node2;
    struct Node node3;

    node1.data = 10;
    node2.data = 8;
    node3.data = 5;

    node1.next = &node2;
    node2.next = &node3;
    node3.next = 0; // NULL.

    struct Node* current = &node1;

    int sum = 0;
    
    sum = sum + current->data;
    current = current->next;

    sum = sum + current->data;
    current = current->next;

    sum = sum + current->data;
    // Now, next is NULL, so we shouldn't try 
    return sum;
}