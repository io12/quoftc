#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include "ds.h"
#include "quoftc.h"
#include "utf8.h"
#include "lex.h"

char yytext[MAX_IDENT_SIZE + 1];
union yystype yylval;
static const char *filename;
static uint16_t lineno;
static char *inp_origin, *inp;

const char *get_filename(void)
{
	return filename;
}

uint16_t get_lineno(void)
{
	return lineno;
}

static void inc_lineno(void)
{
	if (++lineno == 0) {
		fatal_error(lineno - 1, "Source file longer than "
				XSTR(UINT16_MAX)" lines");
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

static enum tok num_lit_with_base(int);
static bool is_hex_digit(int);

static enum tok char_lit(void)
{
	long c;

	if (*inp++ != '\'') {
		internal_error();
	}
	if (inp[0] == 'U' && inp[1] == '+') {
		inp += 2;
		if (num_lit_with_base(16) == FLOAT_LIT) {
			goto invalid;
		}
		c = yylval.int_lit;
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
	fatal_error(lineno, "Invalid char literal");
}

static enum tok string_lit(void)
{
	char *p;
	uint64_t len;

	if (*inp++ != '"') {
		internal_error();
	}
	p = yylval.string_lit.val;
	len = 0;
	do {
		// TODO: Fix this
		if (len == MAX_STRING_SIZE) {
			fatal_error(lineno, "String literal is longer than the "
			                    "maximum allowed length "
					    "("XSTR(MAX_STRING_SIZE)" bytes)");
		}
		*p++ = *inp++;
		len++;
	} while (*inp != '"');
	inp++;
	*p = '\0';
	yylval.string_lit.len = len;
	if (!is_valid_utf8(yylval.string_lit.val)) {
		fatal_error(lineno, "Invalid string literal");
	}
	return STRING_LIT;
}

static enum tok lookup_keyword(const char *keyword)
{
	static HashTable *keywords = NULL;

	if (UNLIKELY(keywords == NULL)) {
		keywords = alloc_hash_table();
#define K(keyword, tok) hash_table_set(keywords, keyword, (void *) tok)
		K("const", CONST);
		K("var", VAR);
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
		K("switch", SWITCH);
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
		K("<-", BACK_ARROW);
		K("=>", BIG_ARROW);
		K("\\", BACKSLASH);
		K("_", UNDERSCORE);
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
	for (i = 0; is_ident_tail(*inp); i++) {
		if (i == MAX_IDENT_SIZE) {
			fatal_error(lineno, "Identifier longer than the "
			                    "maximum allowed size "
			                    "("XSTR(MAX_IDENT_SIZE)")");
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

typedef bool IsValidDigitFunc(int);

static IsValidDigitFunc *get_is_valid_digit_func(int base)
{
	switch (base) {
	case 2:
		return is_bin_digit;
	case 8:
		return is_oct_digit;
	case 10:
		return is_dec_digit;
	case 16:
		return is_hex_digit;
	default:
		internal_error();
	}
}

// TODO: Split this into multiple functions
static enum tok num_lit_with_base(int base)
{
	char num_text[MAX_NUM_CHARS + 1];
	IsValidDigitFunc *is_valid_digit;
	int i;
	bool found_radix_point;
	unsigned long long inum;
	double dnum;

	is_valid_digit = get_is_valid_digit_func(base);
	i = 0;
	found_radix_point = false;
	while (is_valid_digit(*inp) || *inp == '.') {
		if (*inp == '.') {
			if (found_radix_point) {
				fatal_error(lineno, "Floating point literal "
						"has multiple radix points");
			}
			found_radix_point = true;
		}
		num_text[i++] = *inp++;
		if (i == MAX_NUM_CHARS + 1) {
			fatal_error(lineno, "Numerical literal has more than "
					XSTR(MAX_NUM_CHARS)" characters");
		}
	}
	if (i == 0) {
		fatal_error(lineno, "Numerical literal has no digits");
	}
	num_text[i] = '\0';
	if (found_radix_point) {
		// TODO: Floats
		if (num_text[0] == '.') {
			fatal_error(lineno, "Radix point at beginning of "
					"floating point literal");
		}
		if (num_text[i - 1] == '.') {
			fatal_error(lineno, "Radix point at end of floating "
					"point literal");
		}
		if (base != 10) {
			fatal_error(lineno, "Floating point literal is not "
					"base 10");
		}
		dnum = strtod(num_text, NULL);
		if (errno == ERANGE) {
			fatal_error(lineno, "Floating point literal too large");
		}
		yylval.float_lit = dnum;
		return FLOAT_LIT;
	} else {
		inum = strtoull(num_text, NULL, base);
		/*
		 * Compare inum with UINT64_MAX for the rare case when this
		 * compiler is used on systems where ULLONG_MAX > UINT64_MAX
		 */
		if (errno == ERANGE || inum > UINT64_MAX) {
			fatal_error(lineno, "Integer literal greater than "
					XSTR(UINT64_MAX));
		}
		yylval.int_lit = inum;
		return INT_LIT;
	}
}

static enum tok num_lit(void)
{
	if (*inp == '0') {
		inp++;
		switch (*inp++) {
		case 'b':
			return num_lit_with_base(2);
		case 'o':
			return num_lit_with_base(8);
		case 'x':
			return num_lit_with_base(16);
		case '.':
			inp -= 2;
			goto decimal;
		default:
			fatal_error(lineno,
					"Numerical literal has a leading zero");
		}
	}
decimal:
	return num_lit_with_base(10);
}

static bool is_op_char(int c)
{
	return strchr("+-*/%<>=!&|^~.:;,", c) != NULL;
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
			fatal_error(lineno, "Operator longer than the "
			                    "maximum allowed size "
			                    "("XSTR(MAX_IDENT_SIZE)")");
		}
		yytext[i++] = *inp++;
	} while (is_op_char(*inp));
	yytext[i] = '\0';
	tok = lookup_keyword(yytext);
	if (tok == INVALID_TOK) {
		fatal_error(lineno, "`%s` is not a valid operator", yytext);
	}
	return tok;
}

const char *tok_to_str(enum tok tok)
{
	static const char *tok_names[] = {
		[CONST] = "`const`",
		[VAR] = "`var`",
		[IMPURE] = "`impure`",
		[IDENT] = "an identifier",
		[TYPEDEF] = "`typedef`",
		[TRUE] = "`True`",
		[FALSE] = "`False`",
		[INT_LIT] = "an integer literal",
		[FLOAT_LIT] = "a float literal",
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
		[SWITCH] = "`switch`",
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
		[BACK_ARROW] = "`<-`",
		[BIG_ARROW] = "`=>`",
		[BACKSLASH] = "`\\`",
		[UNDERSCORE] = "`_`",
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

enum tok next_tok(void)
{
	skipspaces();
	switch (*inp) {
	case '\'':
		return char_lit();
	case '"':
		return string_lit();
	case '[':
		inp++;
		return OPEN_BRACKET;
	case ']':
		inp++;
		return CLOSE_BRACKET;
	case '(':
		inp++;
		return OPEN_PAREN;
	case ')':
		inp++;
		return CLOSE_PAREN;
	case '{':
		inp++;
		return OPEN_BRACE;
	case '}':
		inp++;
		return CLOSE_BRACE;
	case '\0':
		return TEOF;
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
	fatal_error(lineno, "Invalid token `%c`", *inp);
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
	char *inp_save;

	inp_save = inp;
	if (next_tok() == tok) {
		return true;
	}
	inp = inp_save;
	return false;
}

void expect_tok(enum tok expected_tok)
{
	enum tok tok;

	tok = next_tok();
	if (tok != expected_tok) {
		fatal_error(lineno, "Expected %s, instead got %s",
				tok_to_str(expected_tok), tok_to_str(tok));
	}
}

static NORETURN void file_error(void)
{
	fprintf(stderr, "%s: error: %s: %s\n", argv0, filename,
			strerror(errno));
	exit(EXIT_FAILURE);
}

static size_t file_len;

void init_lex(const char *filename_)
{
	int fd;
	struct stat stat;

	filename = filename_;
	fd = open(filename, O_RDONLY);
	if (fd == -1 || fstat(fd, &stat) == -1) {
		file_error();
	}
	file_len = stat.st_size;
	inp = mmap(NULL, file_len + 1, PROT_READ | PROT_WRITE,
			MAP_PRIVATE, fd, 0);
	if (inp == MAP_FAILED || close(fd) == -1) {
		file_error();
	}
	inp[file_len] = '\0';
	inp_origin = inp;
	lineno = 1;
}

void cleanup_lex(void)
{
	if (munmap(inp_origin, file_len + 1) == -1) {
		file_error();
	}
}
