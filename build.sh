#!/bin/sh
warning_flags="-Wall -Wextra -Werror -Wfatal-errors -Werror=missing-prototypes \
	-Wno-missing-braces"
cflags="-g -O0 -std=c99 -pedantic $warning_flags `llvm-config --cflags`"
ldflags="`llvm-config --ldflags --libs`"
args="$cflags $ldflags *.c -o quoftc"
gcc $args && clang $args && ./run_tests.sh
