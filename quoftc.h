#ifdef __GNUC__
	#define NORETURN __attribute__((noreturn))
	#define PRINTF_LIKE(fmt, args) \
		__attribute__((format(printf, fmt, args)))
	#define MALLOC_LIKE __attribute__((malloc))

	#define UNLIKELY(x) __builtin_expect((x), false)
#else
	#define NORETURN
	#define PRINTF_LIKE(fmt, args)
	#define MALLOC_LIKE

	#define UNLIKELY(x) (x)
#endif

#define XSTR(x) STR__(x)
#define STR__(x) #x
#define IN_RANGE(x, min, max) ((x) >= min && (x) <= max)
#define NEW(type) ((type *) emalloc(sizeof(type)))
#define NEWC(type) ((type *) ecalloc(sizeof(type)))

NORETURN PRINTF_LIKE(2, 3) void fatal_error(uint16_t, const char *, ...);
NORETURN void internal_error(void);
MALLOC_LIKE void *emalloc(size_t);
MALLOC_LIKE void *ecalloc(size_t);
void *erealloc(void *, size_t);
char *estrdup(const char *);

const char *argv0;
