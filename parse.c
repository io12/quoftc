#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include "langc.h"
#include "ds.h"

#define ALLOC_STRUCT(struct_tag, ...) \
	((struct struct_tag *) \
		memcpy(NEW(struct struct_tag), &(struct struct_tag){ \
			__VA_ARGS__ \
		}, sizeof(struct struct_tag)))

#define ALLOC_UNION(struct_tag, enum_tag, sub_struct_name, ...) \
	((struct struct_tag *) \
		memcpy(NEW(struct struct_tag), &(struct struct_tag){ \
			.type = enum_tag, \
			.u.sub_struct_name = { __VA_ARGS__ }, \
		}, sizeof(struct struct_tag)))

struct type {
	enum { PRIM_TYPE, ALIAS_TYPE, PARAM_TYPE, TUPLE_TYPE, FUNC_TYPE } type;
	union {
		struct {
			enum tok tok;
		} prim;
		struct {
			char *name;
		} alias;
		struct {
			char *name;
			Vec *params;
		} param;
		struct {
			struct type *l, *r;
		} tuple, func;
	} u;
};

#define ALLOC_PRIM_TYPE(...) \
	ALLOC_UNION(type, PRIM_TYPE, prim, __VA_ARGS__)
#define ALLOC_ALIAS_TYPE(...) \
	ALLOC_UNION(type, ALIAS_TYPE, alias, __VA_ARGS__)
#define ALLOC_PARAM_TYPE(...) \
	ALLOC_UNION(type, PARAM_TYPE, param, __VA_ARGS__)
#define ALLOC_TUPLE_TYPE(...) \
	ALLOC_UNION(type, TUPLE_TYPE, tuple, __VA_ARGS__)
#define ALLOC_FUNC_TYPE(...) \
	ALLOC_UNION(type, FUNC_TYPE, func, __VA_ARGS__)

struct expr {
	enum {
		BOOL_LIT_EXPR, CHAR_LIT_EXPR, STRING_LIT_EXPR,
		UNARY_OP_EXPR, BIN_OP_EXPR, LAMBDA_EXPR, ARRAY_LIT_EXPR,
		IDENT_EXPR, BLOCK_EXPR
	} type;
	union {
		struct {
			bool val;
		} bool_lit;
		struct {
			uint32_t val;
		} char_lit;
		struct {
			char *val;
		} string_lit;
		struct {
			enum tok op;
			struct expr *subexpr;
		} unary_op;
		struct {
			enum tok op;
			struct expr *l, *r;
		} bin_op;
		struct {
			Vec *params;
			struct expr *body;
		} lambda;
		struct {
			Vec *val;
		} array_lit;
		struct {
			char *name;
		} ident;
		struct {
			Vec *stmts;
		} block;
	} u;
};

#define ALLOC_BOOL_LIT_EXPR(...) \
	ALLOC_UNION(expr, BOOL_LIT_EXPR, bool_lit, __VA_ARGS__)
#define ALLOC_CHAR_LIT_EXPR(...) \
	ALLOC_UNION(expr, CHAR_LIT_EXPR, char_lit, __VA_ARGS__)
#define ALLOC_STRING_LIT_EXPR(...) \
	ALLOC_UNION(expr, STRING_LIT_EXPR, string_lit, __VA_ARGS__)
#define ALLOC_UNARY_OP_EXPR(...) \
	ALLOC_UNION(expr, UNARY_OP_EXPR, unary_op, __VA_ARGS__)
#define ALLOC_BIN_OP_EXPR(...) \
	ALLOC_UNION(expr, BIN_OP_EXPR, bin_op, __VA_ARGS__)
#define ALLOC_LAMBDA_EXPR(...) \
	ALLOC_UNION(expr, LAMBDA_EXPR, lambda, __VA_ARGS__)
#define ALLOC_ARRAY_LIT_EXPR(...) \
	ALLOC_UNION(expr, ARRAY_LIT_EXPR, array_lit, __VA_ARGS__)
#define ALLOC_IDENT_EXPR(...) \
	ALLOC_UNION(expr, IDENT_EXPR, ident, __VA_ARGS__)
#define ALLOC_BLOCK_EXPR(...) \
	ALLOC_UNION(expr, BLOCK_EXPR, block, __VA_ARGS__)

struct decl {
	bool is_mut;
	struct type *type;
	char *name;
	struct expr *val;
};

#define ALLOC_DECL(...) \
	ALLOC_STRUCT(decl, __VA_ARGS__)

struct stmt {
	enum {
		DECL_STMT, EXPR_STMT, IF_STMT, DO_STMT,
		WHILE_STMT, FOR_STMT, MATCH_STMT
	} type;
	union {
		struct {
			struct decl *decl;
		} decl;
		struct {
			struct expr *expr;
		} expr;
		struct {
			struct expr *cond;
			Vec *then_stmts, *else_stmts;
		} if_;
		struct {
			Vec *stmts;
			struct expr *cond;
		} do_, while_;
		struct {
			struct expr *init, *cond, *post;
			Vec *stmts;
		} for_;
		struct {
			// TODO
			int x;
		} match;
	} u;
};

#define ALLOC_DECL_STMT(...) \
	ALLOC_UNION(stmt, DECL_STMT, decl, __VA_ARGS__)
#define ALLOC_EXPR_STMT(...) \
	ALLOC_UNION(stmt, EXPR_STMT, expr, __VA_ARGS__)
#define ALLOC_IF_STMT(...) \
	ALLOC_UNION(stmt, IF_STMT, if_, __VA_ARGS__)
#define ALLOC_DO_STMT(...) \
	ALLOC_UNION(stmt, DO_STMT, do_, __VA_ARGS__)
#define ALLOC_WHILE_STMT(...) \
	ALLOC_UNION(stmt, WHILE_STMT, while_, __VA_ARGS__)
#define ALLOC_FOR_STMT(...) \
	ALLOC_UNION(stmt, FOR_STMT, for_, __VA_ARGS__)
#define ALLOC_MATCH_STMT(...) \
	ALLOC_UNION(stmt, MATCH_STMT, match, __VA_ARGS__)

static struct type *parse_type(void);
static struct expr *parse_expr(void);
static struct stmt *parse_stmt(void);

static bool is_prim_type(enum tok tok)
{
	return IN_RANGE(tok, U8, CHAR);
}

static bool is_primary_type_head(enum tok tok)
{
	return tok == OPEN_PAREN || tok == IDENT || tok == IMPURE
		|| is_prim_type(tok);
}

static struct type *parse_primary_type(void)
{
	enum tok tok;
	struct type *type;
	Vec *params;

	tok = next_tok();
	switch (tok) {
	case OPEN_PAREN:
		type = parse_type();
		expect_tok(CLOSE_PAREN);
		return type;
	case IDENT:
	case IMPURE:
		if (is_primary_type_head(peek_tok())) {
			params = alloc_vec();
			do {
				vec_push(params, parse_primary_type());
			} while (is_primary_type_head(peek_tok()));
			return ALLOC_PARAM_TYPE(estrdup(yytext), params);
		}
		return ALLOC_ALIAS_TYPE(estrdup(yytext));
	default:
		break;
	}
	if (is_prim_type(tok)) {
		return ALLOC_PRIM_TYPE(tok);
	}
	fatal_error("Expected a primary type, instead got %s", tok_to_str(tok));
}

static struct type *parse_type(void)
{
	struct type *l;

	l = parse_primary_type();
	switch (peek_tok()) {
	case COMMA:
		next_tok();
		return ALLOC_TUPLE_TYPE(l, parse_type());
	case ARROW:
		next_tok();
		return ALLOC_FUNC_TYPE(l, parse_type());
	default:
		return l;
	}
}

static struct expr *parse_lambda_expr(void)
{
	Vec *params;

	expect_tok(BACKSLASH);
	params = alloc_vec();
	while (accept_tok(IDENT)) {
		vec_push(params, estrdup(yytext));
	}
	expect_tok(ARROW);
	return ALLOC_LAMBDA_EXPR(params, parse_expr());
}

static struct expr *parse_array_lit_expr(void)
{
	Vec *items;

	expect_tok(OPEN_BRACKET);
	items = alloc_vec();
	do {
		// TODO: Tuples break this
		vec_push(items, parse_expr());
	} while (accept_tok(COMMA));
	expect_tok(CLOSE_BRACKET);
	return ALLOC_ARRAY_LIT_EXPR(items);
}

static struct expr *parse_block_expr(void)
{
	expect_tok(OPEN_BRACE);
	return NULL; // TODO
}

static struct expr *parse_paren_expr(void)
{
	struct expr *expr;

	expect_tok(OPEN_PAREN);
	expr = parse_expr();
	expect_tok(CLOSE_PAREN);
	return expr;
}

static struct expr *parse_primary_expr(void)
{
	enum tok peek;

	peek = peek_tok();
	switch (peek) {
	case IDENT:
		return ALLOC_IDENT_EXPR(estrdup(yytext));
	case TRUE:
		next_tok();
		return ALLOC_BOOL_LIT_EXPR(true);
	case FALSE:
		next_tok();
		return ALLOC_BOOL_LIT_EXPR(false);
	case NUM_LIT:
		// TODO
		break;
	case CHAR_LIT:
		next_tok();
		return ALLOC_CHAR_LIT_EXPR(yylval.char_lit);
	case STRING_LIT:
		next_tok();
		return ALLOC_STRING_LIT_EXPR(estrdup(yylval.string_lit));
	case PLUS_PLUS:
	case MINUS_MINUS:
	case STAR:
	case AMP:
	case TILDE:
	case BANG:
		next_tok();
		return ALLOC_UNARY_OP_EXPR(peek, parse_primary_expr());
	case BACKSLASH:
		return parse_lambda_expr();
	case OPEN_BRACKET:
		return parse_array_lit_expr();
	case OPEN_PAREN:
		return parse_paren_expr();
	case OPEN_BRACE:
		return parse_block_expr();
	default:
		break;
	}
	fatal_error("Expected a primary expression, instead got %s",
			tok_to_str(peek));
}

static bool is_bin_op(enum tok tok)
{
	switch (tok) {
	case PLUS: case MINUS:
	case STAR: case SLASH: case PERCENT:
	case LT: case GT: case LT_EQ: case GT_EQ: case EQ_EQ: case BANG_EQ:
	case AMP: case PIPE: case CARET: case LT_LT: case GT_GT:
	case AMP_AMP: case PIPE_PIPE: case CARET_CARET:
	case EQ:
	case PLUS_EQ: case MINUS_EQ:
	case STAR_EQ: case SLASH_EQ: case PERCENT_EQ:
	case AMP_EQ: case PIPE_EQ: case CARET_EQ: case LT_LT_EQ: case GT_GT_EQ:
	case DOT: case COMMA:
		return true;
	default:
		return false;
	}
}

static int get_bin_op_prec(enum tok op)
{
	switch (op) {
	case DOT:
		return 13;
	case STAR: case SLASH: case PERCENT:
		return 12;
	case PLUS: case MINUS:
		return 11;
	case LT_LT: case GT_GT:
		return 10;
	case AMP:
		return 9;
	case CARET:
		return 8;
	case PIPE:
		return 7;
	case LT: case GT: case LT_EQ: case GT_EQ:
		return 6;
	case EQ_EQ: case BANG_EQ:
		return 5;
	case AMP_AMP:
		return 4;
	case CARET_CARET:
		return 3;
	case PIPE_PIPE:
		return 2;
	case COMMA:
		return 1;
	case EQ:
	case STAR_EQ: case SLASH_EQ: case PERCENT_EQ:
	case PLUS_EQ: case MINUS_EQ:
	case LT_LT_EQ: case GT_GT_EQ:
	case AMP_EQ: case CARET_EQ: case PIPE_EQ:
		return 0;
	default:
		internal_error();
	}
}

enum assoc {
	L_ASSOC, R_ASSOC, NON_ASSOC
};

static enum assoc get_bin_op_assoc(enum tok op)
{
	switch (op) {
	case DOT:
	case STAR: case SLASH: case PERCENT:
	case PLUS: case MINUS:
	case LT_LT: case GT_GT:
	case AMP:
	case CARET:
	case PIPE:
	case EQ_EQ: case BANG_EQ:
	case AMP_AMP:
	case CARET_CARET:
	case PIPE_PIPE:
		return L_ASSOC;
	case LT: case GT: case LT_EQ: case GT_EQ:
		return NON_ASSOC;
	case COMMA:
	case EQ:
	case STAR_EQ: case SLASH_EQ: case PERCENT_EQ:
	case PLUS_EQ: case MINUS_EQ:
	case LT_LT_EQ: case GT_GT_EQ:
	case AMP_EQ: case CARET_EQ: case PIPE_EQ:
		return R_ASSOC;
	default:
		internal_error();
	}
}

// Precedence climbing
static struct expr *parse_expr__(struct expr *l, int min_prec)
{
	enum tok peek, op;
	struct expr *r;

	peek = peek_tok();
	while (is_bin_op(peek) && get_bin_op_prec(peek) >= min_prec) {
		op = peek;
		next_tok();
		r = parse_primary_expr();
		peek = peek_tok();
		while (is_bin_op(peek) &&
				(get_bin_op_prec(peek) > get_bin_op_prec(op)
				|| (get_bin_op_assoc(peek) == R_ASSOC
				&& get_bin_op_prec(peek)
				== get_bin_op_prec(op)))) {
			r = parse_expr__(r, get_bin_op_prec(peek));
			peek = peek_tok();
		}
		l = ALLOC_BIN_OP_EXPR(op, l, r);
	}
	return l;
}

static struct expr *parse_expr(void)
{
	return parse_expr__(parse_primary_expr(), 0);
}

static struct decl *parse_decl(void)
{
	bool is_mut;
	struct type *type;
	char *name;
	struct expr *val;

	if (accept_tok(MUT)) {
		is_mut = true;
	} else {
		is_mut = false;
	}
	type = parse_type();
	expect_tok(IDENT);
	name = estrdup(yytext);
	if (accept_tok(SEMICOLON)) {
		val = NULL;
	} else {
		val = parse_expr();
		expect_tok(SEMICOLON);
	}
	return ALLOC_DECL(is_mut, type, name, val);
}

static Vec *parse_compound_stmt(void)
{
	Vec *stmts;

	expect_tok(OPEN_BRACE);
	stmts = alloc_vec();
	while (!accept_tok(CLOSE_BRACE)) {
		vec_push(stmts, parse_stmt());
	}
	return stmts;
}

static struct stmt *parse_if_stmt(void)
{
	struct expr *cond;
	Vec *then_stmts, *else_stmts;
	enum tok peek;

	expect_tok(IF);
	cond = parse_paren_expr();
	then_stmts = parse_compound_stmt();
	if (accept_tok(ELSE)) {
		peek = peek_tok();
		switch (peek) {
		case IF:
			else_stmts = alloc_vec();
			vec_push(else_stmts, parse_if_stmt());
			break;
		case OPEN_BRACE:
			else_stmts = parse_compound_stmt();
			break;
		default:
			fatal_error("Expected %s or %s, instead got %s",
					tok_to_str(IF),
					tok_to_str(OPEN_BRACE),
					tok_to_str(peek));
		}
	} else {
		else_stmts = NULL;
	}
	return ALLOC_IF_STMT(cond, then_stmts, else_stmts);
}

static struct stmt *parse_do_stmt(void)
{
	Vec *stmts;
	struct expr *cond;

	expect_tok(DO);
	stmts = parse_compound_stmt();
	expect_tok(WHILE);
	cond = parse_paren_expr();
	expect_tok(SEMICOLON);
	return ALLOC_DO_STMT(stmts, cond);
}

static struct stmt *parse_while_stmt(void)
{
	Vec *stmts;
	struct expr *cond;

	expect_tok(WHILE);
	cond = parse_paren_expr();
	stmts = parse_compound_stmt();
	return ALLOC_WHILE_STMT(stmts, cond);
}

static struct stmt *parse_for_stmt(void)
{
	struct expr *init, *cond, *post;
	Vec *stmts;

	expect_tok(FOR);
	expect_tok(OPEN_PAREN);
	if (accept_tok(SEMICOLON)) {
		init = NULL;
	} else {
		init = parse_expr();
		expect_tok(SEMICOLON);
	}
	if (accept_tok(SEMICOLON)) {
		cond = NULL;
	} else {
		cond = parse_expr();
		expect_tok(SEMICOLON);
	}
	if (accept_tok(CLOSE_PAREN)) {
		post = NULL;
	} else {
		post = parse_expr();
		expect_tok(CLOSE_PAREN);
	}
	stmts = parse_compound_stmt();
	return ALLOC_FOR_STMT(init, cond, post, stmts);
}

static struct stmt *parse_match_stmt(void)
{
	return NULL; // TODO
}

static struct stmt *parse_stmt(void)
{
	switch (peek_tok()) {
	case IF:
		return parse_if_stmt();
	case DO:
		return parse_do_stmt();
	case WHILE:
		return parse_while_stmt();
	case FOR:
		return parse_for_stmt();
	case MATCH:
		return parse_match_stmt();
	default:
		break;
	}
	// TODO: Decl and expr stmts
}
