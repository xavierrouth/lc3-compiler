#define API_VER 2
#include "framework.h"

int fib(int n) {
    if (2 > n) {
        return n;
    }
    return fib(n-1) + fib(n-2);
}

void FibonacciTest(lc3::sim& sim, Tester& tester, double points) {
    sim.writePC(0x3000);
    sim.setRunInstLimit(50000);
    sim.run();

    tester.verify("Correct Value", sim.readMem(0xFDFF) == fib(11), 1);
}

void testBringup(lc3::sim & sim) { }

void testTeardown(lc3::sim & sim) { }

void setup(Tester & tester) { 
    tester.registerTest("fibonacci", FibonacciTest, 1, false);
}

void shutdown(void) { }
