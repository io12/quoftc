#!/bin/sh
CFLAGS="-g -O0 -std=c99 -pedantic -Wall -Wextra"
cc $CFLAGS *.c -o langc
