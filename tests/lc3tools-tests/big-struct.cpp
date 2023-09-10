struct node {
    int padding2;
    int data;
    int padding1;
};

int test() {
    struct node hi;
    struct node hi2;
    hi.data = 10;
    hi2.data = hi.data;
    int a = hi2.data;
    return a;
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
    tester.registerTest("big-struct", Test, 1, false);
}

void shutdown(void) { }
