void maxArray(int* x, int* y) {
    int i;

    for (i = 0; i < 3; i = i + 1) {
        if (y[i] > x[i]) {
            x[i] = y[i];
        }
    }
    return;
}

int test() {
    int a[3];
    a[0] = 1;
    a[1] = 3;
    a[2] = 4;
    int b[3];
    b[0] = 3;
    b[1] = 1;
    b[2] = 1;

    maxArray(a, b);

    return a[0] + a[1] + a[2];
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
    tester.registerTest("array-max", Test, 1, false);
}

void shutdown(void) { }