struct Node {
    int data;
    struct Node* next;
};

int test() {
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

#define API_VER 2
#include "framework.h"

void Test(lc3::sim& sim, Tester& tester, double points) {
    sim.writePC(0x3000);
    sim.setRunInstLimit(50000);
    sim.run();

    tester.verify("correct?", sim.readMem(0xFDFF) == test(), 1);
}

void testBringup(lc3::sim & sim) { }

void testTeardown(lc3::sim & sim) { }

void setup(Tester & tester) { 
    tester.registerTest("linked-list", Test, 1, false);
}

void shutdown(void) { }