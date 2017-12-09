#!/bin/sh
CFLAGS="-g -O0 -std=c99 -pedantic -Wall -Wextra -Werror -Wfatal-errors \
	-Werror=missing-prototypes `llvm-config --cflags`"
LDFLAGS="`llvm-config --ldflags --libs`"
cc $CFLAGS $LDFLAGS *.c -o quoftc
