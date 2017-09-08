#ifdef __GNUC__
#define NORETURN __attribute__((noreturn))
#else
#define NORETURN
#endif

#define LEN(x) (sizeof(x) / sizeof((x)[0]))
#define MAX_IDENT_SIZE 512

// N/A (Not applicable) == -1
#define N -1
#define A 1

enum tok {
	IMPURE,
	IDENT,
	TYPEDEF,

	TRUE, FALSE,
	NUM_LITERAL, CHAR_LITERAL, STRING_LITERAL,

	PLUS_PLUS, MINUS_MINUS,
	PLUS, MINUS, STAR, SLASH, PERCENT,
	LT_EQ, GT_EQ, EQ_EQ, BANG_EQ,
	AMP, PIPE, CARET, TILDE, LT_LT, GT_GT,
	AMP_AMP, PIPE_PIPE, CARET_CARET, BANG,
	EQ,
	PLUS_EQ, MINUS_EQ, STAR_EQ, SLASH_EQ, PERCENT_EQ,
	AMP_EQ, PIPE_EQ, CARET_EQ, LT_LT_EQ, GT_GT_EQ,

	IF, THEN, ELSE, DO, WHILE, FOR, MATCH, COND,
	BREAK, CONTINUE, DEFER, RETURN,

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

struct tok {
	enum tok_type type;
	union {
		char *string_val;
		bool bool_val;
		BigNum num_val; // TODO
		uint32_t char_val;
	} u;
};

static uint16_t lineno = 1;

static NORETURN void fatal_error(char *s)
{
	// TODO: filename
	fprintf(stderr, "filename:%hu: error: %s\n", lineno, s);
	exit(EXIT_FAILURE);
}

static void inc_lineno(void)
{
	if (++lineno == 0) {
		lineno--;
		fatal_error("Source file longer than "#UINT16_MAX" lines");
	}
}

static void skipspaces(void)
{
	do {
		if (*inp++ == '\n') {
			inc_lineno();
		}
	} while (isspace(*inp));
}

// TODO: This should probably use a hash table
static enum tok lookup_keyword(char *keyword)
{
	static struct {
		char *keyword;
		enum tok tok;
	} keywords[] = {
		{ "impure", IMPURE },
		{ "typedef", TYPEDEF },
		{ "True", TRUE },
		{ "False", FALSE },
		{ "if", IF },
		{ "then", THEN },
		{ "else", ELSE },
		{ "do", DO },
		{ "while", WHILE },
		{ "for", FOR },
		{ "match", MATCH },
		{ "cond", COND },
		{ "break", BREAK },
		{ "continue", CONTINUE },
		{ "defer", DEFER },
		{ "return", RETURN },
		{ "U8", U8 },
		{ "U16", U16 },
		{ "U32", U32 },
		{ "U64", U64 },
		{ "I8", I8 },
		{ "I16", I16 },
		{ "I32", I32 },
		{ "I64", I64 },
		{ "F32", F32 },
		{ "F64", F64 },
		{ "bool", BOOL },
		{ "void", VOID },
		{ "char", CHAR }
	};
	int i;

	for (i = 0; i < LEN(keywords); i++) {
		if (strcmp(keyword, keywords[i].keyword) == 0) {
			return keywords[i].tok;
		}
	}
	return IDENT;
}

static bool is_ident_head(int c)
{
	return c == '_' || isalpha(c);
}

static bool is_ident_tail(int c)
{
	return c == '_' || isalnum(c);
}

static enum tok ident(void)
{
	char *p, yytext[MAX_IDENT_SIZE + 1];
	int i;

	if (!is_ident_head(*inp)) {
		fatal_error("Identifiers must begin with an underscore, "
		            "a letter, or a digit");
	}
	for (i = 0; is_ident_tail(inp[i]); i++) {
		if (i == MAX_IDENT_SIZE) {
			fatal_error("Identifier longer than the "
			            "maximum allowed size "
			            "("#MAX_IDENT_SIZE")");
		}
		yytext[i] = *inp++;
	}
	yytext[i] = '\0';
	return lookup_keyword(yytext);
}

static enum tok op(int c, enum tok single, enum tok twice, enum tok eq_postfix)
{
	if (*inp != c) {
		fatal_error("Internal error");
	}
	inp++;
	if (twice != N/A && *inp == c) {
		inp++;
		return twice;
	}
	if (eq_postfix != N/A && *inp == '=') {
		inp++;
		return eq_postfix;
	}
	return single;
}

// TODO: <<= and >>=
enum tok next_tok(void)
{
	skipspaces();
	switch (*inp) {
	case ''':
	case 'U':
		return char_(); // TODO: make sure this isn't an IDENT
	case '"': return string();
	case '+': return op('+', PLUS, PLUS_PLUS, PLUS_EQ);
	case '-': return op('-', MINUS, MINUS_MINUS, MINUS_EQ);
	case '*': return op('*', STAR, N/A, STAR_EQ);
	case '/': return op('/', SLASH, N/A, SLASH_EQ);
	case '%': return op('%', PERCENT, N/A, PERCENT_EQ);
	case '<': return op('<', LT, LT_LT, LT_EQ);
	case '>': return op('>', GT, GT_GT, GT_EQ);
	case '=': return op('=', EQ, EQ_EQ, EQ_EQ);
	case '!': return op('!', BANG, N/A, BANG_EQ);
	case '&': return op('&', AMP, AMP_AMP, AMP_EQ);
	case '|': return op('|', PIPE, PIPE_PIPE, PIPE_EQ);
	case '^': return op('^', CARET, CARET_CARET, CARET_EQ);
	case '~': inp++; return TILDE;
	case '.': inp++; return DOT;
	case ':': inp++; return COLON;
	case ';': inp++; return SEMICOLON;
	case ',': inp++; return COMMA;
	case '[': inp++; return OPEN_BRACKET;
	case ']': inp++; return CLOSE_BRACKET;
	case '(': inp++; return OPEN_PAREN;
	case ')': inp++; return CLOSE_PAREN;
	case '{': inp++; return OPEN_BRACE;
	case '}': inp++; return CLOSE_BRACE;
	case '\0': return TEOF;
	}
	if (is_ident_head(*inp)) {
		return ident();
	}
	if (isdigit(*inp)) {
		return num();
	}
	fatal_error("Invalid token");
}
