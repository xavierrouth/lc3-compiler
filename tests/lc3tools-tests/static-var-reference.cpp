int* printx(){
	static int x = 0;
	x = x + 1;
	//printf("value of x is %d \n",x);
	return (&x);
}

int test(){
	int *x_ptr;
	x_ptr = printx();
	x_ptr = printx();
	*x_ptr = (*x_ptr) + 1;
	printx();
	return *x_ptr;
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
    tester.registerTest("static-var-reference", Test, 1, false);
}

void shutdown(void) { }