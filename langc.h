#ifdef __GNUC__
	#define NORETURN __attribute__((noreturn))
	#define PRINTF_LIKE __attribute__((format(printf, 1, 2)))
	#define MALLOC_LIKE __attribute__((malloc))

	#define UNLIKELY(x) __builtin_expect((x), false)
#else
	#define NORETURN
	#define PRINTF_LIKE
	#define MALLOC_LIKE

	#define UNLIKELY(x) x
#endif

#define MAX_IDENT_SIZE 512
#define MAX_STRING_SIZE 1024 // TODO: Make this unlimited

#define xstr(x) str__(x)
#define str__(x) #x
#define LEN(x) (sizeof(x) / sizeof((x)[0]))
#define IN_RANGE(x, min, max) ((x) >= min && (x) <= max)
#define NEW(type) ((type *) emalloc(sizeof(type)))
#define NEWC(type) ((type *) ecalloc(sizeof(type)))

enum tok {
	INVALID_TOK,

	MUT,
	IMPURE,
	IDENT,
	TYPEDEF,

	TRUE, FALSE,
	NUM_LIT, CHAR_LIT, STRING_LIT,

	PLUS_PLUS, MINUS_MINUS,
	PLUS, MINUS, STAR, SLASH, PERCENT,
	LT, GT, LT_EQ, GT_EQ, EQ_EQ, BANG_EQ,
	AMP, PIPE, CARET, TILDE, LT_LT, GT_GT,
	AMP_AMP, PIPE_PIPE, CARET_CARET, BANG,
	EQ,
	PLUS_EQ, MINUS_EQ, STAR_EQ, SLASH_EQ, PERCENT_EQ,
	AMP_EQ, PIPE_EQ, CARET_EQ, LT_LT_EQ, GT_GT_EQ,

	IF, THEN, ELSE, DO, WHILE, FOR, MATCH, COND,
	BREAK, CONTINUE, DEFER, RETURN,

	// Changing the order of these may break something
	U8, U16, U32, U64,
	I8, I16, I32, I64,
	F32, F64,
	BOOL, VOID, CHAR,

	DOT, COLON, SEMICOLON, COMMA, ARROW, BACKSLASH,

	OPEN_BRACKET, CLOSE_BRACKET,
	OPEN_PAREN, CLOSE_PAREN,
	OPEN_BRACE, CLOSE_BRACE,

	TEOF
};
union yystype {
	uint32_t char_lit;
	char string_lit[MAX_STRING_SIZE + 1];
	long num_lit;
};

char *tok_to_str(enum tok);
enum tok next_tok(void);
enum tok peek_tok(void);
bool accept_tok(enum tok);
void expect_tok(enum tok);

NORETURN PRINTF_LIKE void fatal_error(char *, ...);
NORETURN void internal_error(void);
MALLOC_LIKE void *emalloc(size_t);
MALLOC_LIKE void *ecalloc(size_t);
void *erealloc(void *, size_t);
char *estrdup(const char *);

bool is_valid_code_point(uint32_t);
int str_to_code_point(uint32_t *, const char *);
bool is_valid_utf8(const char *);

extern char yytext[MAX_IDENT_SIZE + 1];
extern union yystype yylval;

extern char *inp, *filename;
extern uint16_t lineno;
