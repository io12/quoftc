#ifdef __GNUC__
	#define NORETURN __attribute__((noreturn))
	#define PRINTF(fmt, args) __attribute__((format(printf, fmt, args)))
	#define MALLOC __attribute__((malloc))

	#define UNLIKELY(x) __builtin_expect((x), false)
#else
	#define NORETURN
	#define PRINTF(fmt, args)
	#define MALLOC

	#define UNLIKELY(x) (x)
#endif

#define UNIMPLEMENTED()                             \
	no_lineno_error("Unimplemented: %s:%s:%d",  \
			__FILE__, __func__, __LINE__)

#define INTERNAL_ERROR()                             \
	no_lineno_error("Internal error: %s:%s:%d",  \
			__FILE__, __func__, __LINE__)

#define IN_RANGE(x, min, max) ((x) >= min && (x) <= max)
#define EITHER_EQ(x, y, z) ((x) == (z) || (y) == (z))
#define NEW(type) ((type *) xmalloc(sizeof(type)))
#define NEWC(type) ((type *) xcalloc(sizeof(type)))

#define MAX_LINENO 60000 // MAX_LINENO < UINT_MAX on all platforms
#define MAX_IDENT_SIZE 512
#define MAX_STRING_SIZE 1024 // TODO: Make this unlimited

PRINTF(2, 3) void warn(unsigned lineno, const char *fmt, ...);
NORETURN PRINTF(2, 3) void fatal_error(unsigned, const char *, ...);
NORETURN PRINTF(1, 2) void no_lineno_error(const char *, ...);
MALLOC void *xmalloc(size_t);
MALLOC void *xcalloc(size_t);
void *xrealloc(void *, size_t);
char *xstrdup(const char *);

extern const char *argv0;
