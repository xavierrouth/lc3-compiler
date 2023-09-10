struct node {
    int data;
};

int test() {
    struct node hi;
    hi.data = 10;
    int a = hi.data;
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
    tester.registerTest("simple-struct", Test, 1, false);
}

void shutdown(void) { }
