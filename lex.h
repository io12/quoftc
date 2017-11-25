#define MAX_IDENT_SIZE 512
#define MAX_STRING_SIZE 1024 // TODO: Make this unlimited
#define MAX_NUM_CHARS 128 // TODO: Maybe change this?

enum tok {
	INVALID_TOK,

	CONST, VAR,
	IMPURE,
	IDENT,
	TYPEDEF,

	TRUE, FALSE,
	INT_LIT, FLOAT_LIT, CHAR_LIT, STRING_LIT,

	PLUS_PLUS, MINUS_MINUS,
	PLUS, MINUS, STAR, SLASH, PERCENT,
	LT, GT, LT_EQ, GT_EQ, EQ_EQ, BANG_EQ,
	AMP, PIPE, CARET, TILDE, LT_LT, GT_GT,
	AMP_AMP, PIPE_PIPE, CARET_CARET, BANG,
	EQ,
	PLUS_EQ, MINUS_EQ, STAR_EQ, SLASH_EQ, PERCENT_EQ,
	AMP_EQ, PIPE_EQ, CARET_EQ, LT_LT_EQ, GT_GT_EQ,

	IF, THEN, ELSE, DO, WHILE, FOR, SWITCH,
	BREAK, CONTINUE, DEFER, RETURN,

	U8, U16, U32, U64,
	I8, I16, I32, I64,
	F32, F64,
	BOOL, VOID, CHAR,

	DOT, COLON, SEMICOLON, COMMA, ARROW, BACK_ARROW, BIG_ARROW,
	BACKSLASH, UNDERSCORE,

	OPEN_BRACKET, CLOSE_BRACKET,
	OPEN_PAREN, CLOSE_PAREN,
	OPEN_BRACE, CLOSE_BRACE,

	TEOF
};
union yystype {
	uint32_t char_lit;
	struct {
		char val[MAX_STRING_SIZE + 1];
		uint64_t len;
	} string_lit;
	uint64_t int_lit;
	double float_lit;
};

const char *get_filename(void);
uint16_t get_lineno(void);
char *tok_to_str(enum tok);
enum tok next_tok(void);
enum tok peek_tok(void);
bool accept_tok(enum tok);
void expect_tok(enum tok);
void init_lex(const char *);
void cleanup_lex(void);

extern char yytext[MAX_IDENT_SIZE + 1];
extern union yystype yylval;
