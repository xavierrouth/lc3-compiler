#define API_VER 2
#include "framework.h"

int test() {
    int a = 15;
    int d = 10;

    int* aptr = &a;
    int* dptr = &d;

    *aptr = *dptr;
    return a;
}

void Test(lc3::sim& sim, Tester& tester, double points) {
    sim.writePC(0x3000);
    sim.setRunInstLimit(50000);
    sim.run();

    tester.verify("correct?", sim.readMem(0xFDFF) == test(), 1);
}

void testBringup(lc3::sim & sim) { }

void testTeardown(lc3::sim & sim) { }

void setup(Tester & tester) { 
    tester.registerTest("random-ptr", Test, 1, false);
}

void shutdown(void) { }
