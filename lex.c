#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "langc.h"

#ifdef __GNUC__
#define NORETURN __attribute__((noreturn))
#else
#define NORETURN
#endif

#define xstr(x) str__(x)
#define str__(x) #x
#define LEN(x) (sizeof(x) / sizeof((x)[0]))
#define MAX_STRING_SIZE 1024 // TODO: Make this unlimited
#define MAX_IDENT_SIZE 512

enum tok {
	N, A, // N/A (Not applicable) == 0

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

static uint16_t lineno = 1;
static union {
	uint32_t char_literal;
	char string_literal[MAX_STRING_SIZE + 1];
} yylval;

static NORETURN void fatal_error(char *s)
{
	fprintf(stderr, "%s:%hu: error: %s\n", filename, lineno, s);
	exit(EXIT_FAILURE);
}

static NORETURN void internal_error(void)
{
	fatal_error("Internal error");
}

static void inc_lineno(void)
{
	if (++lineno == 0) {
		lineno--;
		fatal_error("Source file longer than "xstr(UINT16_MAX)" lines");
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

static enum tok char_literal(void)
{
	switch (*inp) {
	case '\'':
		inp++;
		inp += str_to_code_point(&yylval.char_literal, inp);
		if (*inp++ != '\'') {
			fatal_error("Invalid char literal");
		}
		return CHAR_LITERAL;
	case 'U':
		if (*++inp != '+') {
			internal_error();
		}
	}
	internal_error();
}

static enum tok string_literal(void)
{
	char *p;

	if (*inp++ != '"') {
		internal_error();
	}
	p = yylval.string_literal;
	do {
		// TODO: Fix this
		if (p - yylval.string_literal == MAX_STRING_SIZE) {
			fatal_error("String literal is longer than the maximum "
			            "allowed length ("xstr(MAX_STRING_SIZE)
			            " bytes)");
		}
		*p++ = *inp++;
	} while (*inp != '"');
	*p = '\0';
	if (!is_valid_utf8(yylval.string_literal)) {
		fatal_error("Invalid string literal");
	}
	inp++;
	return STRING_LITERAL;
}

// TODO: This should probably use a hash table
static enum tok lookup_keyword(char *keyword)
{
	static const struct {
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
	size_t i;

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
	char yytext[MAX_IDENT_SIZE + 1];
	int i;

	if (!is_ident_head(*inp)) {
		internal_error();
	}
	for (i = 0; is_ident_tail(inp[i]); i++) {
		if (i == MAX_IDENT_SIZE) {
			fatal_error("Identifier longer than the "
			            "maximum allowed size "
			            "("xstr(MAX_IDENT_SIZE)")");
		}
		yytext[i] = *inp++;
	}
	yytext[i] = '\0';
	return lookup_keyword(yytext);
}

static enum tok op(int c, enum tok single, enum tok twice, enum tok eq_postfix)
{
	if (*inp != c) {
		internal_error();
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

enum tok next_tok(void)
{
	skipspaces();
	switch (*inp) {
	case '\'': return char_literal();
	case '"': return string_literal();
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
	if (inp[0] == 'U' && inp[1] == '+') {
		return char_literal();
	}
	if (is_ident_head(*inp)) {
		return ident();
	}
	if (isdigit(*inp)) {
		return num_literal();
	}
	fatal_error("Invalid token");
}
