int one() {
  return 1;
}

int test() {
  int a = one() + one() + one() + one() + one();
  return a;
}

#define RET_LOCATION 0xFDFF
#define API_VER 2
#include "framework.h"

void Test(lc3::sim& sim, Tester& tester, double points) {

    sim.writePC(0x3000);
    sim.setRunInstLimit(50000);
    sim.run();

    tester.verify("Returned correct value?", sim.readMem(0xFDFF) == test(), 1);
}

void testBringup(lc3::sim & sim) { }

void testTeardown(lc3::sim & sim) { }

void setup(Tester & tester) { 
    tester.registerTest("function-call-clobbering", Test, 1, false);
}

void shutdown(void) { }