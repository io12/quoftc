#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "langc.h"

#define xstr(x) str__(x)
#define str__(x) #x
#define LEN(x) (sizeof(x) / sizeof((x)[0]))
#define IN_RANGE(x, min, max) (x >= min && x <= max)
#define MAX_STRING_SIZE 1024 // TODO: Make this unlimited

static uint16_t lineno = 1;
static union {
	uint32_t char_literal;
	char string_literal[MAX_STRING_SIZE + 1];
	long num_literal;
} yylval;

static NORETURN PRINTF_LIKE void fatal_error(char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%hu: error: ", filename, lineno);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fputc('\n', stderr);
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
	while (isspace(*inp)) {
		if (*inp++ == '\n') {
			inc_lineno();
		}
	}
}

static enum tok char_literal(void)
{
	if (*inp++ != '\'') {
		internal_error();
	}
	if (inp[0] == 'U' && inp[1] == '+') {
		// TODO:
	}
	inp += str_to_code_point(&yylval.char_literal, inp);
	if (*inp++ != '\'') {
		fatal_error("Invalid char literal");
	}
	return CHAR_LITERAL;
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
		{ "fn", FN },
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

static bool is_bin_digit(int c)
{
	return c == '0' || c == '1';
}

static bool is_oct_digit(int c)
{
	return IN_RANGE(c, '0', '7');
}

static bool is_dec_digit(int c)
{
	return isdigit(c);
}

static bool is_hex_digit(int c)
{
	return is_dec_digit(c) || IN_RANGE(c, 'A', 'F');
}

static int value_of_digit(int c)
{
	return is_dec_digit(c) ? c - '0' : c - 'A' + 10;
}

static enum tok num_literal(void)
{
	bool (*is_valid_digit)(int c) = is_dec_digit;
	int base = 10;

	if (*inp == '0') {
		inp++;
		switch (*inp++) {
		case 'b':
			is_valid_digit = is_bin_digit;
			base = 2;
			break;
		case 'o':
			is_valid_digit = is_oct_digit;
			base = 8;
			break;
		case 'x':
			is_valid_digit = is_hex_digit;
			base = 16;
			break;
		default:
			fatal_error("Numerical literal has a leading zero");
		}
	}
	yylval.num_literal = 0;
	while (is_valid_digit(*inp)) {
		yylval.num_literal *= base;
		yylval.num_literal += value_of_digit(*inp++);
	}
	return NUM_LITERAL;
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
	if (is_ident_head(*inp)) {
		return ident();
	}
	if (isdigit(*inp)) {
		return num_literal();
	}
	fatal_error("Invalid token");
}

enum tok peek_tok(void)
{
	char *inp_save;
	enum tok tok;

	inp_save = inp;
	tok = next_tok();
	inp = inp_save;
	return tok;
}

static char *tok_to_str(enum tok tok)
{
	static char *tok_names[] = {
		[IMPURE] = "`impure`",
		[IDENT] = "an identifier",
		[TYPEDEF] = "`typedef`",
		[TRUE] = "`True`",
		[FALSE] = "`False`",
		[NUM_LITERAL] = "a numerical literal",
		[CHAR_LITERAL] = "a character literal",
		[STRING_LITERAL] = "a string literal",
		[PLUS_PLUS] = "`++`",
		[MINUS_MINUS] = "`--`",
		[PLUS] = "`+`",
		[MINUS] = "`-`",
		[STAR] = "`*`",
		[SLASH] = "`/`",
		[PERCENT] = "`%`",
		[LT] = "`<`",
		[GT] = "`>`",
		[LT_EQ] = "`<=`",
		[GT_EQ] = "`>=`",
		[EQ_EQ] = "`==`",
		[BANG_EQ] = "`!=`",
		[AMP] = "`&`",
		[PIPE] = "`|`",
		[CARET] = "`^`",
		[TILDE] = "`~`",
		[LT_LT] = "`<<`",
		[GT_GT] = "`>>`",
		[AMP_AMP] = "`&&`",
		[PIPE_PIPE] = "`||`",
		[CARET_CARET] = "`^^`",
		[BANG] = "`!`",
		[EQ] = "`=`",
		[PLUS_EQ] = "`+=`",
		[MINUS_EQ] = "`-=`",
		[STAR_EQ] = "`*=`",
		[SLASH_EQ] = "`/=`",
		[PERCENT_EQ] = "`%=`",
		[AMP_EQ] = "`&=`",
		[PIPE_EQ] = "`|=`",
		[CARET_EQ] = "`^=`",
		[IF] = "`if`",
		[THEN] = "`then`",
		[ELSE] = "`else`",
		[DO] = "`do`",
		[WHILE] = "`while`",
		[FOR] = "`for`",
		[MATCH] = "`match`",
		[BREAK] = "`break`",
		[CONTINUE] = "`continue`",
		[DEFER] = "`defer`",
		[RETURN] = "`return`",
		[U8] = "`U8`",
		[U16] = "`U16`",
		[U32] = "`U32`",
		[U64] = "`U64`",
		[I8] = "`I8`",
		[I16] = "`I16`",
		[I32] = "`I32`",
		[I64] = "`I64`",
		[F32] = "`F32`",
		[F64] = "`F64`",
		[BOOL] = "`bool`",
		[VOID] = "`void`",
		[CHAR] = "`char`",
		[DOT] = "`.`",
		[COLON] = "`:`",
		[SEMICOLON] = "`;`",
		[COMMA] = "`,`",
		[OPEN_BRACKET] = "`[`",
		[CLOSE_BRACKET] = "`]`",
		[OPEN_PAREN] = "`(`",
		[CLOSE_PAREN] = "`)`",
		[OPEN_BRACE] = "`{`",
		[CLOSE_BRACE] = "`}`",
		[TEOF] = "end of file"
	};

	if (tok_names[tok] == NULL) {
		internal_error();
	}
	return tok_names[tok];
}

void expect_tok(enum tok expected_tok)
{
	enum tok tok;

	tok = next_tok();
	if (tok != expected_tok) {
		fatal_error("Expected %s, instead got %s",
				tok_to_str(expected_tok), tok_to_str(tok));
	}
}
