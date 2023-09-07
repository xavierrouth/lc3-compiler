int test() {
    int a = 0;
    for (int i = 0; i < 10; i = i + 1) {
        a = a + i;
    }
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
    tester.registerTest("for-loop", Test, 1, false);
}

void shutdown(void) { }
