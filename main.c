#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "langc.h"

const char *argv0;

static void *ptr_sanitize(void *p)
{
	if (p == NULL) {
		fprintf(stderr, "%s: error: %s\n", argv0, strerror(errno));
		exit(EXIT_FAILURE);
	}
	return p;
}

MALLOC_LIKE void *emalloc(size_t size)
{
	return ptr_sanitize(malloc(size));
}

MALLOC_LIKE void *ecalloc(size_t size)
{
	return ptr_sanitize(calloc(1, size));
}

void *erealloc(void *p, size_t size)
{
	return ptr_sanitize(realloc(p, size));
}

char *estrdup(const char *s)
{
	return strcpy(emalloc(strlen(s) + 1), s);
}

int main(int argc, char *argv[])
{
	int i;

	argv0 = argv[0];
	if (argc < 2) {
		fprintf(stderr, "Usage: %s file...\n", argv0);
		exit(EXIT_FAILURE);
	}
	for (i = 1; i < argc; i++) {
		// TODO: Compile file
	}
}
