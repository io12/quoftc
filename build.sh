#!/bin/sh
cflags="-g -O0 -std=c99 -pedantic -Wall -Wextra -Werror -Wfatal-errors \
	-Werror=missing-prototypes `llvm-config --cflags`"
ldflags="`llvm-config --ldflags --libs`"
args="$cflags $ldflags *.c -o quoftc"
gcc $args && clang $args && ./run_tests.sh
