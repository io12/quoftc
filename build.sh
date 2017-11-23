#!/bin/sh
CFLAGS="-g -O0 -std=c99 -pedantic -Wall -Wextra -Wfatal-errors"
cc $CFLAGS *.c -o quoftc
