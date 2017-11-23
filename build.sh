#!/bin/sh
CFLAGS="-g -O0 -std=c99 -pedantic -Wall -Wextra -Wfatal-errors -Werror=missing-prototypes"
cc $CFLAGS *.c -o quoftc
