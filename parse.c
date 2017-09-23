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
		UNARY_OP_EXPR, BIN_OP_EXPR, LAMBDA_EXPR, ARRAY_LIT_EXPR,
		IDENT_EXPR, BLOCK_EXPR
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
			enum tok op;
			struct expr *l, *r;
		} bin_op;
		struct {
			Vec *params;
			struct expr *body;
		} lambda;
		Vec *array_lit;
		char *ident;
		struct {
			Vec *stmts;
		} block;
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

static struct expr *alloc_bin_op_expr(enum tok op, struct expr *l,
		struct expr *r)
{
	struct expr *expr;

	expr = NEW(struct expr);
	expr->type = BIN_OP_EXPR;
	expr->u.bin_op.op = op;
	expr->u.bin_op.l = l;
	expr->u.bin_op.r = r;
	return expr;
}

static struct expr *alloc_lambda_expr(Vec *params, struct expr *body)
{
	struct expr *expr;

	expr = NEW(struct expr);
	expr->type = LAMBDA_EXPR;
	expr->u.lambda.params = params;
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

static struct expr *alloc_ident_expr(char ident[MAX_IDENT_SIZE + 1])
{
	struct expr *expr;

	expr = NEW(struct expr);
	expr->type = IDENT_EXPR;
	strcpy(expr->u.ident, ident);
	return expr;
}

static struct expr *alloc_block_expr(Vec *stmts)
{
	struct expr *expr;

	expr = NEW(struct expr);
	expr->type = BLOCK_EXPR;
	expr->u.block.stmts = stmts;
	return expr;
}

struct decl {
	bool is_mut;
	struct type *type;
	char *name;
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

struct stmt {
	enum {
		DECL_STMT, EXPR_STMT, IF_STMT, DO_STMT,
		WHILE_STMT, FOR_STMT, MATCH_STMT
	} type;
	union {
		struct decl *decl;
		struct expr *expr;
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
		} match;
	} u;
};

/*
static struct stmt *alloc_decl_stmt(struct decl *decl)
{
	struct stmt *stmt;

	stmt = NEW(struct stmt);
	stmt->type = DECL_STMT;
	stmt->u.decl = decl;
	return stmt;
}

static struct stmt *alloc_expr_stmt(struct expr *expr)
{
	struct stmt *stmt;

	stmt = NEW(struct stmt);
	stmt->type = EXPR_STMT;
	stmt->u.expr = expr;
	return stmt;
}

static struct stmt *alloc_if_stmt(struct expr *cond, Vec *then_stmts,
		Vec *else_stmts)
{
	struct stmt *stmt;

	stmt = NEW(struct stmt);
	stmt->type = IF_STMT;
	stmt->u.if_.cond = cond;
	stmt->u.if_.then_stmts = then_stmts;
	stmt->u.if_.else_stmts = else_stmts;
	return stmt;
}

static struct stmt *alloc_do_stmt(Vec *stmts, struct expr *cond)
{
	struct stmt *stmt;

	stmt = NEW(struct stmt);
	stmt->type = DO_STMT;
	stmt->u.do_.stmts = stmts;
	stmt->u.do_.cond = cond;
	return stmt;
}

static struct stmt *alloc_while_stmt(Vec *stmts, struct expr *cond)
{
	struct stmt *stmt;

	stmt = NEW(struct stmt);
	stmt->type = WHILE_STMT;
	stmt->u.while_.stmts = stmts;
	stmt->u.while_.cond = cond;
	return stmt;
}

static struct stmt *alloc_for_stmt(struct expr *init, struct expr *cond,
		struct expr *post, Vec *stmts)
{
	struct stmt *stmt;

	stmt = NEW(struct stmt);
	stmt->type = FOR_STMT;
	stmt->u.for_.init = init;
	stmt->u.for_.cond = cond;
	stmt->u.for_.post = post;
	stmt->u.for_.stmts = stmts;
	return stmt;
}

static struct stmt *alloc_match_stmt(/* TODO */)
{
	struct stmt *stmt;

	stmt = NEW(struct stmt);
	// TODO
}
*/

static bool is_prim_type(enum tok);
static bool is_primary_type_head(enum tok);
static struct type *parse_primary_type(void);
static struct type *parse_type(void);
static struct expr *parse_lambda_expr(void);
static struct expr *parse_array_lit_expr(void);
static struct expr *parse_primary_expr(void);
static struct expr *parse_expr__(struct expr *, int);
static struct expr *parse_expr(void);
static struct decl *parse_decl(void);

static bool is_prim_type(enum tok tok)
{
	return IN_RANGE(tok, U8, CHAR);
}

static bool is_primary_type_head(enum tok tok)
{
	return is_prim_type(tok) || tok == OPEN_PAREN || tok == IDENT;
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
	fatal_error("Expected a primary type, instead got %s", tok_to_str(tok));
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
	return alloc_lambda_expr(params, parse_expr());
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
	return alloc_array_lit_expr(items);
}

static struct expr *parse_block_expr(void)
{
	expect_tok(OPEN_BRACE);
}

static struct expr *parse_primary_expr(void)
{
	enum tok tok;
	struct expr *expr;

	tok = peek_tok();
	switch (tok) {
	case IDENT:
		return alloc_ident_expr(yytext);
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
		return alloc_string_lit_expr(yylval.string_lit);
	case PLUS_PLUS:
	case MINUS_MINUS:
	case STAR:
	case AMP:
	case TILDE:
	case BANG:
		next_tok();
		return alloc_unary_op_expr(tok, parse_primary_expr());
	case BACKSLASH:
		return parse_lambda_expr();
	case OPEN_BRACKET:
		return parse_array_lit_expr();
	case OPEN_PAREN:
		next_tok();
		expr = parse_expr();
		expect_tok(CLOSE_PAREN);
		return expr;
	case OPEN_BRACE:
		return parse_block_expr();
	default:
		break;
	}
	fatal_error("Expected a primary expression, instead got %s",
			tok_to_str(tok));
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
		l = alloc_bin_op_expr(op, l, r);
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
