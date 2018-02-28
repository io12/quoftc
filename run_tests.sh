#!/bin/sh
for test in tests/*.qf; do
	if ! ./quoftc "$test"; then
		exit 1
	fi
done
