#!/bin/sh
set -uex
gcc -c tests/run_test.c -o tests/run_test.o
for test in tests/*.qf; do
	if ! ./quoftc "$test"; then
		exit 1
	fi
	gcc a.out tests/run_test.o -o tests/run_test
	if ! tests/run_test; then
		exit 1
	fi
done
