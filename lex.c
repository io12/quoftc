#include <assert.h>
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

#define MAX_LINENO UINT16_MAX

static const char *filename;
static char *inp_origin, *inp;
static unsigned lineno;

const char *get_filename(void)
{
	return filename;
}

static void inc_lineno(void)
{
	if (lineno == MAX_LINENO) {
		fatal_error(lineno, "Source file longer than "XSTR(MAX_LINENO)
		                    " lines");
	}
	lineno++;
}

static void skip_line_comment(void)
{
	assert(inp[0] == '/' && inp[1] == '/');
	inp += 2;
	for (;;) {
		if (*inp == '\n') {
			inp++;
			inc_lineno();
			return;
		}
		if (*inp == '\0') {
			fatal_error(lineno, "End of file in line comment");
		}
		inp++;
	}
}

static void skip_block_comment(void)
{
	assert(inp[0] == '/' && inp[1] == '*');
	inp += 2;
	for (;;) {
		if (inp[0] == '*' && inp[1] == '/') {
			inp += 2;
			return;
		}
		if (*inp == '\n') {
			inc_lineno();
		}
		if (*inp == '\0') {
			fatal_error(lineno, "End of file in block comment");
		}
		inp++;
	}
}

static void skip_spaces(void)
{
	for (;;) {
		if (isspace(*inp)) {
			if (*inp == '\n') {
				inc_lineno();
			}
			inp++;
		} else if (inp[0] == '/' && inp[1] == '/') {
			skip_line_comment();
		} else if (inp[0] == '/' && inp[1] == '*') {
			skip_block_comment();
		} else {
			return;
		}
	}
}

static void init_char_lit_tok(struct tok *tok, uint32_t c)
{
	tok->kind = CHAR_LIT;
	tok->lineno = lineno;
	tok->u.char_lit = c;
}

static void lex_num_lit_with_base(struct tok *, int);

static void lex_char_lit(struct tok *tok)
{
	struct tok num_tok;
	uint64_t num;
	uint32_t c;

	assert(*inp == '\'');
	inp++;
	if (inp[0] == 'U' && inp[1] == '+') {
		inp += 2;
		lex_num_lit_with_base(&num_tok, 16);
		if (num_tok.kind == FLOAT_LIT) {
			goto invalid;
		}
		num = num_tok.u.int_lit;
		if (num > UINT32_MAX || !is_valid_code_point(num)) {
			goto invalid;
		}
		c = num;
	} else {
		inp += str_to_code_point(&c, inp);
	}
	if (*inp++ != '\'') {
		goto invalid;
	}
	init_char_lit_tok(tok, c);
	return;
invalid:
	fatal_error(lineno, "Invalid char literal");
}

static void init_string_lit_tok(struct tok *tok, char val[MAX_STRING_SIZE + 1],
		unsigned len)
{
	tok->kind = STRING_LIT;
	tok->lineno = lineno;
	strcpy(tok->u.string_lit.val, val);
	tok->u.string_lit.len = len;
}

static void lex_string_lit(struct tok *tok)
{
	char text[MAX_STRING_SIZE + 1], *p;
	unsigned len;

	assert(*inp == '"');
	inp++;
	p = text;
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
	if (!is_valid_utf8(text)) {
		fatal_error(lineno, "Invalid string literal");
	}
	init_string_lit_tok(tok, text, len);
}

static enum tok_kind lookup_keyword(const char *keyword)
{
	static HashTable *keywords = NULL;

	if (UNLIKELY(keywords == NULL)) {
		keywords = alloc_hash_table();
#define K(keyword, tok) hash_table_set(keywords, keyword, (void *) tok)
		K("const", CONST);
		K("var", VAR);
		K("impure", IMPURE);
		K("typedef", TYPEDEF);
		K("true", TRUE);
		K("false", FALSE);
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
		K("_", UNDERSCORE);
#undef K
	}
	// Returns INVALID_TOK if not found
	return (enum tok_kind) hash_table_get(keywords, keyword);
}

static enum tok_kind lookup_op(const char *op)
{
	static HashTable *ops = NULL;

	if (UNLIKELY(ops == NULL)) {
		ops = alloc_hash_table();
#define K(op, tok) hash_table_set(ops, op, (void *) tok)
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
		K(".", DOT);
		K(":", COLON);
		K(";", SEMICOLON);
		K(",", COMMA);
		K("->", ARROW);
		K("<-", BACK_ARROW);
		K("=>", BIG_ARROW);
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
	return (enum tok_kind) hash_table_get(ops, op);
}

static bool is_ident_head(int c)
{
	return c == '_' || isalpha(c);
}

static bool is_ident_tail(int c)
{
	return c == '_' || isalnum(c);
}

static void init_ident_tok(struct tok *tok, char ident[MAX_IDENT_SIZE + 1])
{
	tok->kind = IDENT;
	tok->lineno = lineno;
	strcpy(tok->u.ident, ident);
}

static void init_basic_tok(struct tok *tok, enum tok_kind kind)
{
	tok->kind = kind;
	tok->lineno = lineno;
}

static void lex_ident(struct tok *tok)
{
	int i;
	char ident[MAX_IDENT_SIZE + 1];
	enum tok_kind tok_kind;

	assert(is_ident_head(*inp));
	for (i = 0; is_ident_tail(*inp); i++) {
		if (i == MAX_IDENT_SIZE) {
			fatal_error(lineno, "Identifier longer than the "
			                    "maximum allowed size "
			                    "("XSTR(MAX_IDENT_SIZE)")");
		}
		ident[i] = *inp++;
	}
	ident[i] = '\0';
	tok_kind = lookup_keyword(ident);
	if (tok_kind == INVALID_TOK) {
		init_ident_tok(tok, ident);
	} else {
		init_basic_tok(tok, tok_kind);
	}
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

static void init_float_lit_tok(struct tok *tok, double val)
{
	tok->kind = FLOAT_LIT;
	tok->lineno = lineno;
	tok->u.float_lit = val;
}

static void init_int_lit_tok(struct tok *tok, uint64_t val)
{
	tok->kind = INT_LIT;
	tok->lineno = lineno;
	tok->u.int_lit = val;
}

// TODO: Split this into multiple functions
static void lex_num_lit_with_base(struct tok *tok, int base)
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
		init_float_lit_tok(tok, dnum);
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
		init_int_lit_tok(tok, inum);
	}
}

static void lex_num_lit(struct tok *tok)
{
	if (*inp == '0') {
		inp++;
		switch (*inp++) {
		case 'b':
			lex_num_lit_with_base(tok, 2);
			break;
		case 'o':
			lex_num_lit_with_base(tok, 8);
			break;
		case 'x':
			lex_num_lit_with_base(tok, 16);
			break;
		case '.':
			inp -= 2;
			lex_num_lit_with_base(tok, 10);
			break;
		default:
			fatal_error(lineno, "Numerical literal has a leading "
			                    "zero");
		}
	} else {
		lex_num_lit_with_base(tok, 10);
	}
}

static bool is_op_char(int c)
{
	return strchr("+-*/%<>=!&|^~.:;,[](){}", c) != NULL;
}

static void lex_op(struct tok *tok)
{
	int i = 0;
	char op_text[MAX_OP_SIZE + 1];
	enum tok_kind tok_kind;

	assert(is_op_char(*inp));
	do {
		if (i == MAX_IDENT_SIZE) {
			fatal_error(lineno, "Operator longer than the "
			                    "maximum allowed size "
			                    "("XSTR(MAX_IDENT_SIZE)")");
		}
		op_text[i++] = *inp++;
	} while (is_op_char(*inp));
	op_text[i] = '\0';
	tok_kind = lookup_op(op_text);
	if (tok_kind == INVALID_TOK) {
		fatal_error(lineno, "`%s` is not a valid operator", op_text);
	}
	init_basic_tok(tok, tok_kind);
}

const char *tok_to_str(enum tok_kind kind)
{
	static const char *tok_names[] = {
		[CONST] = "`const`",
		[VAR] = "`var`",
		[IMPURE] = "`impure`",
		[IDENT] = "an identifier",
		[TYPEDEF] = "`typedef`",
		[TRUE] = "`true`",
		[FALSE] = "`false`",
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

	return tok_names[kind];
}

void lex(struct tok *tok)
{
	skip_spaces();
	switch (*inp) {
	case '\'':
		lex_char_lit(tok);
		return;
	case '"':
		lex_string_lit(tok);
		return;
	case '\0':
		init_basic_tok(tok, TEOF);
		return;
	}
	if (is_op_char(*inp)) {
		lex_op(tok);
	} else if (is_ident_head(*inp)) {
		lex_ident(tok);
	} else if (isdigit(*inp)) {
		lex_num_lit(tok);
	} else {
		fatal_error(lineno, "Invalid token `%c`", *inp);
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
