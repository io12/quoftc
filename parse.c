#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include "langc.h"
#include "ds.h"

struct type {
	enum { PRIM_TYPE, ALIAS_TYPE, PARAM_TYPE, TUPLE_TYPE, FUNC_TYPE } type;
	union {
		struct {
			enum tok tok;
		} prim;
		struct {
			char name[MAX_IDENT_SIZE + 1];
		} alias;
		struct {
			char name[MAX_IDENT_SIZE + 1];
			Vec *params;
		} param;
		struct {
			struct type *l, *r;
		} tuple, func;
	} u;
};

static struct type *alloc_prim_type(enum tok tok)
{
	struct type *type;
	
	type = NEW(struct type);
	type->type = PRIM_TYPE;
	type->u.prim.tok = tok;
	return type;
}

static struct type *alloc_alias_type(char name[MAX_IDENT_SIZE + 1])
{
	struct type *type;
	
	type = NEW(struct type);
	type->type = ALIAS_TYPE;
	strcpy(type->u.alias.name, name);
	return type;
}

static struct type *alloc_param_type(char name[MAX_IDENT_SIZE + 1], Vec *params)
{
	struct type *type;
	
	type = NEW(struct type);
	type->type = PARAM_TYPE;
	strcpy(type->u.param.name, name);
	type->u.param.params = params;
	return type;
}

static struct type *alloc_tuple_type(struct type *l, struct type *r)
{
	struct type *type;
	
	type = NEW(struct type);
	type->type = TUPLE_TYPE;
	type->u.tuple.l = l;
	type->u.tuple.r = r;
	return type;
}

static struct type *alloc_func_type(struct type *l, struct type *r)
{
	struct type *type;
	
	type = NEW(struct type);
	type->type = FUNC_TYPE;
	type->u.func.l = l;
	type->u.func.r = r;
	return type;
}

struct expr {
	enum {
		BOOL_LIT_EXPR, CHAR_LIT_EXPR, STRING_LIT_EXPR,
		UNARY_OP_EXPR, LAMBDA_EXPR, ARRAY_LIT_EXPR
	} type;
	union {
		bool bool_lit;
		uint32_t char_lit;
		char string_lit[MAX_STRING_SIZE + 1];
		struct {
			enum tok op;
			struct expr *subexpr;
		} unary_op;
		struct {
			char param_name[MAX_STRING_SIZE + 1];
			struct expr *body;
		} lambda;
		Vec *array_lit;
	} u;
};

static struct expr *alloc_bool_lit_expr(bool val)
{
	struct expr *expr;

	expr = NEW(struct expr);
	expr->type = BOOL_LIT_EXPR;
	expr->u.bool_lit = val;
	return expr;
}

static struct expr *alloc_char_lit_expr(uint32_t char_lit)
{
	struct expr *expr;

	expr = NEW(struct expr);
	expr->type = CHAR_LIT_EXPR;
	expr->u.char_lit = char_lit;
	return expr;
}

static struct expr *alloc_string_lit_expr(char string_lit[MAX_STRING_SIZE + 1])
{
	struct expr *expr;

	expr = NEW(struct expr);
	expr->type = STRING_LIT_EXPR;
	strcpy(expr->u.string_lit, string_lit);
	return expr;
}

static struct expr *alloc_unary_op_expr(enum tok op, struct expr *subexpr)
{
	struct expr *expr;

	expr = NEW(struct expr);
	expr->type = UNARY_OP_EXPR;
	expr->u.unary_op.op = op;
	expr->u.unary_op.subexpr = subexpr;
	return expr;
}

static struct expr *alloc_lambda_expr(char param_name[MAX_STRING_SIZE + 1],
		struct expr *body)
{
	struct expr *expr;

	expr = NEW(struct expr);
	expr->type = LAMBDA_EXPR;
	strcpy(expr->u.lambda.param_name, param_name);
	expr->u.lambda.body = body;
	return expr;
}

static struct expr *alloc_array_lit_expr(Vec *array_lit)
{
	struct expr *expr;

	expr = NEW(struct expr);
	expr->type = ARRAY_LIT_EXPR;
	expr->u.array_lit = array_lit;
	return expr;
}

struct decl {
	bool is_mut;
	struct type *type;
	char name[MAX_IDENT_SIZE + 1];
	struct expr *val;
};

static struct decl *alloc_decl(bool is_mut, struct type *type,
		char name[MAX_IDENT_SIZE + 1], struct expr *val)
{
	struct decl *decl;

	decl = NEW(struct decl);
	decl->is_mut = is_mut;
	decl->type = type;
	strcpy(decl->name, name);
	decl->val = val;
	return decl;
}

static bool is_primary_type_head(enum tok tok)
{
	return is_prim_type(tok) || tok == OPEN_PAREN || tok == IDENT;
}

static bool is_prim_type(enum tok tok)
{
	return IN_RANGE(tok, U8, CHAR);
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
			return alloc_param_type(yytext, params);
		}
		return alloc_alias_type(yytext);
	default:
		break;
	}
	if (is_prim_type(tok)) {
		return alloc_prim_type(tok);
	}
	// TODO: error
	internal_error();
}

static struct type *parse_type(void)
{
	struct type *l;

	l = parse_primary_type();
	switch (peek_tok()) {
	case COMMA:
		next_tok();
		return alloc_tuple_type(l, parse_type());
	case ARROW:
		next_tok();
		return alloc_func_type(l, parse_type());
	default:
		break;
	}
	return l;
}

static struct expr *parse_lambda_expr(void)
{
}

static struct expr *parse_array_lit_expr(void)
{
	Vec *items;

	expect_tok(OPEN_BRACKET);
	items = alloc_vec();
	do {
		// TODO: Tuples break this
		vec_push(items, parse_expr());
	} while ();
}

static struct expr *parse_primary_expr(void)
{
	enum tok peek;
	struct expr *expr;

	peek = peek_tok();
	switch (peek) {
	case TRUE:
		next_tok();
		return alloc_bool_lit_expr(true);
	case FALSE:
		next_tok();
		return alloc_bool_lit_expr(false);
	case NUM_LIT:
		// TODO
		break;
	case CHAR_LIT:
		next_tok();
		return alloc_char_lit_expr(yylval.char_lit);
	case STRING_LIT:
		next_tok();
		return alloc_char_lit_expr(yylval.string_lit);
	case PLUS_PLUS:
	case MINUS_MINUS:
	case STAR:
	case AMP:
	case TILDE:
	case BANG:
		next_tok();
		return alloc_unary_op_expr(peek, parse_primary_expr());
	case BACKSLASH:
		return parse_lambda_expr();
	case OPEN_BRACKET:
		return parse_array_lit_expr();
	case OPEN_PAREN:
		next_tok();
		expr = parse_expr();
		expect_tok(CLOSE_PAREN);
		return expr;
	}
	// TODO: IDENT
}

static struct expr *parse_expr__(struct expr *l, int min_prec)
{
}

static struct expr *parse_expr(void)
{
	return parse_expr__(parse_primary_expr(), 0);
}

static struct decl *parse_decl(void)
{
	bool is_mut;
	struct type *type;
	char name[MAX_IDENT_SIZE + 1];
	struct expr *val;

	if (accept_tok(MUT)) {
		is_mut = true;
	} else {
		is_mut = false;
	}
	type = parse_type();
	expect_tok(IDENT);
	strcpy(name, yytext);
	if (accept_tok(SEMICOLON)) {
		val = NULL;
	} else {
		val = parse_expr();
		expect_tok(SEMICOLON);
	}
	return alloc_decl(is_mut, type, name, val);
}
