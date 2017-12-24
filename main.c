#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ds.h"
#include "ast.h"
#include "check_semantics.h"
#include "code_gen.h"
#include "lex.h"
#include "parse.h"
#include "quoftc.h"

const char *argv0;

NORETURN PRINTF(2, 3) void fatal_error(unsigned lineno, const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%u: error: ", get_filename(), lineno);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fputc('\n', stderr);
	exit(EXIT_FAILURE);
}

NORETURN void internal_error(void)
{
	fprintf(stderr, "%s: Internal error\n", argv0);
	exit(EXIT_FAILURE);
}

static void *ptr_sanitize(void *p)
{
	if (p == NULL) {
		fprintf(stderr, "%s: error: %s\n", argv0, strerror(errno));
		exit(EXIT_FAILURE);
	}
	return p;
}

MALLOC void *xmalloc(size_t size)
{
	return ptr_sanitize(malloc(size));
}

MALLOC void *xcalloc(size_t size)
{
	return ptr_sanitize(calloc(1, size));
}

void *xrealloc(void *p, size_t size)
{
	return ptr_sanitize(realloc(p, size));
}

char *xstrdup(const char *s)
{
	return strcpy(xmalloc(strlen(s) + 1), s);
}

static void compile_file(const char *filename)
{
	struct ast ast;

	ast = parse_file(filename);
	check_ast(ast);
	compile_ast(ast);
	free_ast(ast);
}

int main(int argc, const char *argv[])
{
	argv0 = argv[0];
	if (argc != 2) {
		fprintf(stderr, "Usage: %s filename\n", argv0);
		exit(EXIT_FAILURE);
	}
	compile_file(argv[1]);
}
