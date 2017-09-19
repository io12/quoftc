#ifdef __GNUC__
#define NORETURN __attribute__((noreturn))
#define PRINTF_LIKE __attribute__((format(printf, 1, 2)))
#define MALLOC_LIKE __attribute__((malloc))
#else
#define NORETURN
#define PRINTF_LIKE
#define MALLOC_LIKE
#endif

#define MAX_IDENT_SIZE 512

#define NEW(type) = ((type *) emalloc(sizeof(type)))

enum tok {
	N, A, // N/A (Not applicable) == 0

	MUT,
	IMPURE,
	IDENT,
	TYPEDEF,

	TRUE, FALSE,
	NUM_LITERAL, CHAR_LITERAL, STRING_LITERAL,

	PLUS_PLUS, MINUS_MINUS,
	PLUS, MINUS, STAR, SLASH, PERCENT,
	LT, GT, LT_EQ, GT_EQ, EQ_EQ, BANG_EQ,
	AMP, PIPE, CARET, TILDE, LT_LT, GT_GT,
	AMP_AMP, PIPE_PIPE, CARET_CARET, BANG,
	EQ,
	PLUS_EQ, MINUS_EQ, STAR_EQ, SLASH_EQ, PERCENT_EQ,
	AMP_EQ, PIPE_EQ, CARET_EQ,

	IF, THEN, ELSE, DO, WHILE, FOR, MATCH, COND,
	BREAK, CONTINUE, DEFER, RETURN,

	// Changing the order of these may break something
	U8, U16, U32, U64,
	I8, I16, I32, I64,
	F32, F64,
	BOOL, VOID, CHAR,

	DOT, COLON, SEMICOLON, COMMA,

	OPEN_BRACKET, CLOSE_BRACKET,
	OPEN_PAREN, CLOSE_PAREN,
	OPEN_BRACE, CLOSE_BRACE,

	TEOF
};

enum tok next_tok(void);
enum tok peek_tok(void);

MALLOC_LIKE void *emalloc(size_t);

int str_to_code_point(uint32_t *, const char *);
bool is_valid_utf8(const char *);

extern char *inp, *filename;
