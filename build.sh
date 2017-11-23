#!/bin/sh
CFLAGS="-g -O0 -std=c99 -pedantic -Wall -Wextra -Werror -Wfatal-errors \
	-Werror=missing-prototypes"
cc $CFLAGS *.c -o quoftc
