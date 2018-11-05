#!/bin/sh
set -ue
gcc -c tests/run_test.c -o tests/run_test.o
for test in tests/*.qf; do
	echo "$test" 1>&2
	if ! ./quoftc "$test"; then
		echo "Error compiling" 1>&2
		exit 1
	fi
	gcc a.out tests/run_test.o -o tests/run_test
	if ! tests/run_test; then
		echo "Error running" 1>&2
		exit 1
	fi
done
