#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include "langc.h"

char *argv0, *filename, *inp;
uint16_t lineno;

NORETURN PRINTF_LIKE void fatal_error(char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%hu: error: ", filename, lineno);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fputc('\n', stderr);
	exit(EXIT_FAILURE);
}

NORETURN void internal_error(void)
{
	fatal_error("Internal error");
}

MALLOC_LIKE void *emalloc(size_t size)
{
	void *p;

	p = malloc(size);
	if (p == NULL) {
		fprintf(stderr, "%s: error: %s\n", argv0, strerror(errno));
		exit(EXIT_FAILURE);
	}
	return p;
}

MALLOC_LIKE void *ecalloc(size_t size)
{
	void *p;

	p = calloc(1, size);
	if (p == NULL) {
		fprintf(stderr, "%s: error: %s\n", argv0, strerror(errno));
		exit(EXIT_FAILURE);
	}
	return p;
}

void *erealloc(void *p, size_t size)
{
	p = realloc(p, size);
	if (p == NULL) {
		fprintf(stderr, "%s: error: %s\n", argv0, strerror(errno));
		exit(EXIT_FAILURE);
	}
	return p;
}

char *estrdup(const char *s)
{
	return strcpy(emalloc(strlen(s) + 1), s);
}

int main(int argc, char *argv[])
{
	int i, fd;
	struct stat stat;

	argv0 = argv[0];
	if (argc < 2) {
		fprintf(stderr, "Usage: %s file...\n", argv0);
		exit(EXIT_FAILURE);
	}
	for (i = 1; i < argc; i++) {
		filename = argv[1];
		fd = open(filename, O_RDONLY);
		if (fd == -1 || fstat(fd, &stat) == -1) {
			goto file_error;
		}
		inp = mmap(NULL, stat.st_size + 1, PROT_READ | PROT_WRITE,
				MAP_PRIVATE, fd, 0);
		if (inp == MAP_FAILED) {
			goto file_error;
		}
		inp[stat.st_size] = '\0';
		lineno = 1;
	}
file_error:
	fprintf(stderr, "%s: error: %s: %s\n", argv0, filename,
			strerror(errno));
	exit(EXIT_FAILURE);
}
