/*
 * This file is linked with each test and executed to determine if the test
 * passed
 */

#include <stdbool.h>

bool passed_test(void);

int main(void)
{
	return !passed_test();
}
