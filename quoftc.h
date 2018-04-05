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

#define UNIMPLEMENTED() assert(false)
#define IN_RANGE(x, min, max) ((x) >= min && (x) <= max)
#define EITHER_EQ(x, y, z) ((x) == (z) || (y) == (z))
#define NEW(type) ((type *) xmalloc(sizeof(type)))
#define NEWC(type) ((type *) xcalloc(sizeof(type)))

PRINTF(2, 3) void warn(unsigned lineno, const char *fmt, ...);
NORETURN PRINTF(2, 3) void fatal_error(unsigned, const char *, ...);
NORETURN void internal_error(void);
MALLOC void *xmalloc(size_t);
MALLOC void *xcalloc(size_t);
void *xrealloc(void *, size_t);
char *xstrdup(const char *);

extern const char *argv0;
