#define API_VER 2
#include "framework.h"

int test() {
    int a = 15;
    int* b = &a;
    int* c = b;
    *b = 10;
    *c = 3;
    return a;
}

void PtrAliasTest(lc3::sim& sim, Tester& tester, double points) {
    sim.writePC(0x3000);
    sim.setRunInstLimit(50000);
    sim.run();

    tester.verify("correct?", sim.readMem(0xFDFF) == test(), 1);
}

void testBringup(lc3::sim & sim) { }

void testTeardown(lc3::sim & sim) { }

void setup(Tester & tester) { 
    tester.registerTest("ptr-alias", PtrAliasTest, 1, false);
}

void shutdown(void) { }
