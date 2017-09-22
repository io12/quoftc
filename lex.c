#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include "langc.h"
#include "ds.h"

char yytext[MAX_IDENT_SIZE + 1];
union yystype yylval;

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

static enum tok num_lit__(bool (*)(int), int);
static bool is_hex_digit(int);

static enum tok char_lit(void)
{
	long c;

	if (*inp++ != '\'') {
		internal_error();
	}
	if (inp[0] == 'U' && inp[1] == '+') {
		inp += 2;
		num_lit__(is_hex_digit, 16);
		c = yylval.num_lit;
		if (c < 0 || !is_valid_code_point(c)) {
			goto invalid;
		}
		yylval.char_lit = c;
		return CHAR_LIT;
	}
	inp += str_to_code_point(&yylval.char_lit, inp);
	if (*inp++ != '\'') {
		goto invalid;
	}
	return CHAR_LIT;
invalid:
	fatal_error("Invalid char literal");
}

static enum tok string_lit(void)
{
	char *p;

	if (*inp++ != '"') {
		internal_error();
	}
	p = yylval.string_lit;
	do {
		// TODO: Fix this
		if (p - yylval.string_lit == MAX_STRING_SIZE) {
			fatal_error("String literal is longer than the maximum "
			            "allowed length ("xstr(MAX_STRING_SIZE)
			            " bytes)");
		}
		*p++ = *inp++;
	} while (*inp != '"');
	*p = '\0';
	if (!is_valid_utf8(yylval.string_lit)) {
		fatal_error("Invalid string literal");
	}
	inp++;
	return STRING_LIT;
}

static enum tok lookup_keyword(char *keyword)
{
	static HashTable *keywords = NULL;

	if (keywords == NULL) {
		keywords = alloc_hash_table();
#define K(keyword, tok) hash_table_set(keywords, keyword, (void *) tok)
		K("mut", MUT);
		K("impure", IMPURE);
		K("typedef", TYPEDEF);
		K("True", TRUE);
		K("False", FALSE);
		K("++", PLUS_PLUS);
		K("--", MINUS_MINUS);
		K("+", PLUS);
		K("-", MINUS);
		K("*", STAR);
		K("/", SLASH);
		K("%", PERCENT);
		K("<", LT);
		K(">", GT);
		K("<=", LT_EQ);
		K(">=", GT_EQ);
		K("==", EQ_EQ);
		K("!=", BANG_EQ);
		K("&", AMP);
		K("|", PIPE);
		K("^", CARET);
		K("~", TILDE);
		K("<<", LT_LT);
		K(">>", GT_GT);
		K("&&", AMP_AMP);
		K("||", PIPE_PIPE);
		K("^^", CARET_CARET);
		K("!", BANG);
		K("=", EQ);
		K("+=", PLUS_EQ);
		K("-=", MINUS_EQ);
		K("*=", STAR_EQ);
		K("/=", SLASH_EQ);
		K("%=", PERCENT_EQ);
		K("&=", AMP_EQ);
		K("|=", PIPE_EQ);
		K("^=", CARET_EQ);
		K("<<=", LT_LT_EQ);
		K(">>=", GT_GT_EQ);
		K("if", IF);
		K("then", THEN);
		K("else", ELSE);
		K("do", DO);
		K("while", WHILE);
		K("for", FOR);
		K("match", MATCH);
		K("cond", COND);
		K("break", BREAK);
		K("continue", CONTINUE);
		K("defer", DEFER);
		K("return", RETURN);
		K("U8", U8);
		K("U16", U16);
		K("U32", U32);
		K("U64", U64);
		K("I8", I8);
		K("I16", I16);
		K("I32", I32);
		K("I64", I64);
		K("F32", F32);
		K("F64", F64);
		K("bool", BOOL);
		K("void", VOID);
		K("char", CHAR);
		K(".", DOT);
		K(":", COLON);
		K(";", SEMICOLON);
		K(",", COMMA);
		K("->", ARROW);
		K("\\", BACKSLASH);
		K("[", OPEN_BRACKET);
		K("]", CLOSE_BRACKET);
		K("(", OPEN_PAREN);
		K(")", CLOSE_PAREN);
		K("{", OPEN_BRACE);
		K("}", CLOSE_BRACE);
#undef K
	}
	// Returns INVALID_TOK if not found
	return (enum tok) hash_table_get(keywords, keyword);
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
	int i;
	enum tok tok;

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
	tok = lookup_keyword(yytext);
	return tok == INVALID_TOK ? IDENT : tok;
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

static enum tok num_lit__(bool (*is_valid_digit)(int c), int base)
{
	yylval.num_lit = 0;
	while (is_valid_digit(*inp)) {
		yylval.num_lit *= base;
		yylval.num_lit += value_of_digit(*inp++);
	}
	return NUM_LIT;
}

static enum tok num_lit(void)
{
	if (*inp == '0') {
		inp++;
		switch (*inp++) {
		case 'b':
			return num_lit__(is_bin_digit, 2);
		case 'o':
			return num_lit__(is_oct_digit, 8);
		case 'x':
			return num_lit__(is_hex_digit, 16);
		default:
			fatal_error("Numerical literal has a leading zero");
		}
	}
	return num_lit__(is_dec_digit, 10);
}

static bool is_op_char(int c)
{
	return strchr("+-*/%<>=!&|^~.:;,[](){}", c) != NULL;
}

static enum tok op(void)
{
	int i = 0;
	enum tok tok;

	if (!is_op_char(*inp)) {
		internal_error();
	}
	do {
		if (i == MAX_IDENT_SIZE) {
			fatal_error("Operator longer than the "
			            "maximum allowed size "
			            "("xstr(MAX_IDENT_SIZE)")");
		}
		yytext[i++] = *inp++;
	} while (is_op_char(*inp));
	yytext[i] = '\0';
	tok = lookup_keyword(yytext);
	if (tok == INVALID_TOK) {
		fatal_error("`%s` is not a valid operator", yytext);
	}
	return tok;
}

enum tok next_tok(void)
{
	skipspaces();
	switch (*inp) {
	case '\'': return char_lit();
	case '"': return string_lit();
	case '\0': return TEOF;
	}
	if (is_op_char(*inp)) {
		return op();
	}
	if (is_ident_head(*inp)) {
		return ident();
	}
	if (isdigit(*inp)) {
		return num_lit();
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

bool accept_tok(enum tok tok)
{
	if (peek_tok() == tok) {
		next_tok();
		return true;
	}
	return false;
}

static char *tok_to_str(enum tok tok)
{
	static char *tok_names[] = {
		[MUT] = "`mut`",
		[IMPURE] = "`impure`",
		[IDENT] = "an identifier",
		[TYPEDEF] = "`typedef`",
		[TRUE] = "`True`",
		[FALSE] = "`False`",
		[NUM_LIT] = "a numerical literal",
		[CHAR_LIT] = "a character literal",
		[STRING_LIT] = "a string literal",
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
		[LT_LT_EQ] = "`<<=`",
		[GT_GT_EQ] = "`>>=`",
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
		[ARROW] = "`->`",
		[BACKSLASH] = "`\\`",
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
