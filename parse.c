#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include "quoftc.h"
#include "ds.h"

#define ALLOC_STRUCT(struct_tag, lineno, ...) \
	((struct struct_tag *) \
		memcpy(NEW(struct struct_tag), &(struct struct_tag){ \
			.lineno = lineno, \
			__VA_ARGS__ \
		}, sizeof(struct struct_tag)))

#define ALLOC_UNION(struct_tag, kind_, sub_struct_name, lineno, ...) \
	((struct struct_tag *) \
		memcpy(NEW(struct struct_tag), &(struct struct_tag){ \
			.lineno = lineno, \
			.kind = kind_, \
			.u.sub_struct_name = { __VA_ARGS__ }, \
		}, sizeof(struct struct_tag)))

#define ALLOC_UNION_KIND_ONLY(struct_tag, kind_, lineno) \
	((struct struct_tag *) \
		memcpy(NEW(struct struct_tag), &(struct struct_tag){ \
			.lineno = lineno, \
			.kind = kind_ \
		}, sizeof(struct struct_tag)))

struct type {
	int lineno;
	enum {
		U8_TYPE, U16_TYPE, U32_TYPE, U64_TYPE,
		I8_TYPE, I16_TYPE, I32_TYPE, I64_TYPE,
		F32_TYPE, F64_TYPE, BOOL_TYPE, VOID_TYPE, CHAR_TYPE,
		ALIAS_TYPE, PARAM_TYPE, ARRAY_TYPE, POINTER_TYPE,
		TUPLE_TYPE, FUNC_TYPE
	} kind;
	union {
		struct {
			char *name;
		} alias;
		struct {
			char *name;
			Vec *params;
		} param;
		struct {
			struct type *l;
			struct expr *len;
		} array;
		struct {
			struct type *l;
		} pointer;
		struct {
			Vec *types;
		} tuple;
		struct {
			struct type *ret;
			Vec *params;
		} func;
	} u;
};

#define ALLOC_U8_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, U8_TYPE, lineno)
#define ALLOC_U16_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, U16_TYPE, lineno)
#define ALLOC_U32_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, U32_TYPE, lineno)
#define ALLOC_U64_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, U64_TYPE, lineno)
#define ALLOC_I8_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, I8_TYPE, lineno)
#define ALLOC_I16_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, I16_TYPE, lineno)
#define ALLOC_I32_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, I32_TYPE, lineno)
#define ALLOC_I64_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, I64_TYPE, lineno)
#define ALLOC_F32_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, F32_TYPE, lineno)
#define ALLOC_F64_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, F64_TYPE, lineno)
#define ALLOC_BOOL_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, BOOL_TYPE, lineno)
#define ALLOC_VOID_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, VOID_TYPE, lineno)
#define ALLOC_CHAR_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, CHAR_TYPE, lineno)
#define ALLOC_ALIAS_TYPE(...) \
	ALLOC_UNION(type, ALIAS_TYPE, alias, __VA_ARGS__)
#define ALLOC_PARAM_TYPE(...) \
	ALLOC_UNION(type, PARAM_TYPE, param, __VA_ARGS__)
#define ALLOC_ARRAY_TYPE(...) \
	ALLOC_UNION(type, ARRAY_TYPE, array, __VA_ARGS__)
#define ALLOC_POINTER_TYPE(...) \
	ALLOC_UNION(type, POINTER_TYPE, pointer, __VA_ARGS__)
#define ALLOC_TUPLE_TYPE(...) \
	ALLOC_UNION(type, TUPLE_TYPE, tuple, __VA_ARGS__)
#define ALLOC_FUNC_TYPE(...) \
	ALLOC_UNION(type, FUNC_TYPE, func, __VA_ARGS__)

struct expr {
	int lineno;
	enum {
		BOOL_LIT_EXPR, CHAR_LIT_EXPR, STRING_LIT_EXPR,
		UNARY_OP_EXPR, BIN_OP_EXPR, LAMBDA_EXPR, ARRAY_LIT_EXPR,
		IDENT_EXPR, BLOCK_EXPR, IF_EXPR, SWITCH_EXPR, TUPLE_EXPR
	} kind;
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
		struct {
			struct expr *cond, *then, *else_;
		} if_;
		struct {
			struct expr *ctrl;
			Vec *cases;
		} switch_;
		struct {
			Vec *items;
		} tuple;
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
#define ALLOC_IF_EXPR(...) \
	ALLOC_UNION(expr, IF_EXPR, if_, __VA_ARGS__)
#define ALLOC_SWITCH_EXPR(...) \
	ALLOC_UNION(expr, SWITCH_EXPR, switch_, __VA_ARGS__)
#define ALLOC_TUPLE_EXPR(...) \
	ALLOC_UNION(expr, TUPLE_EXPR, tuple, __VA_ARGS__)

struct switch_pattern {
	int lineno;
	enum {
		UNDERSCORE_SWITCH_PATTERN, OR_SWITCH_PATTERN,
		ARRAY_SWITCH_PATTERN, TUPLE_SWITCH_PATTERN, EXPR_SWITCH_PATTERN
	} kind;
	union {
		struct {
			Vec *patterns;
		} or, array, tuple;
		struct {
			struct expr *expr;
		} expr;
	} u;
};

#define ALLOC_UNDERSCORE_SWITCH_PATTERN(lineno) \
	ALLOC_UNION_KIND_ONLY(switch_pattern, UNDERSCORE_SWITCH_PATTERN, lineno)
#define ALLOC_OR_SWITCH_PATTERN(...) \
	ALLOC_UNION(switch_pattern, OR_SWITCH_PATTERN, or, __VA_ARGS__)
#define ALLOC_ARRAY_SWITCH_PATTERN(...) \
	ALLOC_UNION(switch_pattern, ARRAY_SWITCH_PATTERN, array, __VA_ARGS__)
#define ALLOC_TUPLE_SWITCH_PATTERN(...) \
	ALLOC_UNION(switch_pattern, TUPLE_SWITCH_PATTERN, tuple, __VA_ARGS__)
#define ALLOC_EXPR_SWITCH_PATTERN(...) \
	ALLOC_UNION(switch_pattern, EXPR_SWITCH_PATTERN, expr, __VA_ARGS__)

struct switch_case {
	int lineno;
	struct switch_pattern *l;
	struct expr *r;
};

#define ALLOC_SWITCH_CASE(...) \
	ALLOC_STRUCT(switch_case, __VA_ARGS__)

struct decl {
	int lineno;
	bool is_mut;
	struct type *type;
	char *name;
	struct expr *val;
};

#define ALLOC_DECL(...) \
	ALLOC_STRUCT(decl, __VA_ARGS__)

struct ast {
	Vec *decls;
};

struct stmt {
	int lineno;
	enum {
		DECL_STMT, EXPR_STMT, IF_STMT, DO_STMT,
		WHILE_STMT, FOR_STMT
	} kind;
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

static struct type *parse_type(void);
static struct switch_pattern *parse_switch_pattern(void);
static struct expr *parse_expr(void);
static struct stmt *parse_stmt(void);

static struct type *parse_tuple_or_func_type(void)
{
	uint16_t lineno;
	struct type *first_type;
	Vec *types;
	enum tok tok;

	lineno = get_lineno();
	expect_tok(OPEN_PAREN);
	first_type = parse_type();
	types = alloc_vec();
	tok = next_tok();
	switch (tok) {
	case COMMA:
		vec_push(types, first_type);
		do {
			vec_push(types, parse_type());
		} while (accept_tok(COMMA));
		expect_tok(CLOSE_PAREN);
		return ALLOC_TUPLE_TYPE(lineno, types);
	case BACK_ARROW:
		do {
			vec_push(types, parse_type());
		} while (accept_tok(COMMA));
		expect_tok(CLOSE_PAREN);
		return ALLOC_FUNC_TYPE(lineno, first_type, types);
	default:
		fatal_error(lineno, "Expected %s or %s, instead got %s",
				tok_to_str(COMMA),
				tok_to_str(BACK_ARROW),
				tok_to_str(tok));
	}
}

static struct type *parse_type(void)
{
	uint16_t lineno;
	enum tok peek;
	struct type *type;
	char *name;
	Vec *params;
	struct expr *len;

	lineno = get_lineno();
	peek = peek_tok();
	switch (peek) {
	case OPEN_PAREN:
		type = parse_tuple_or_func_type();
		break;
	case IDENT:
	case IMPURE:
		next_tok();
		name = estrdup(yytext);
		if (accept_tok(LT)) {
			params = alloc_vec();
			do {
				vec_push(params, parse_type());
			} while (accept_tok(COMMA));
			expect_tok(GT);
			type = ALLOC_PARAM_TYPE(lineno, name, params);
		} else {
			type = ALLOC_ALIAS_TYPE(lineno, name);
		}
		break;
	case U8:
		return ALLOC_U8_TYPE(lineno);
	case U16:
		return ALLOC_U16_TYPE(lineno);
	case U32:
		return ALLOC_U32_TYPE(lineno);
	case U64:
		return ALLOC_U64_TYPE(lineno);
	case I8:
		return ALLOC_I8_TYPE(lineno);
	case I16:
		return ALLOC_I16_TYPE(lineno);
	case I32:
		return ALLOC_I32_TYPE(lineno);
	case I64:
		return ALLOC_I64_TYPE(lineno);
	case F32:
		return ALLOC_F32_TYPE(lineno);
	case F64:
		return ALLOC_F64_TYPE(lineno);
	case BOOL:
		return ALLOC_BOOL_TYPE(lineno);
	case VOID:
		return ALLOC_VOID_TYPE(lineno);
	case CHAR:
		return ALLOC_CHAR_TYPE(lineno);
	default:
		fatal_error(lineno, "Expected a primary type, instead got %s",
				tok_to_str(peek));
	}
	for (;;) {
		if (accept_tok(OPEN_BRACKET)) {
			if (accept_tok(CLOSE_BRACKET)) {
				len = NULL;
			} else {
				len = parse_expr();
				expect_tok(CLOSE_BRACKET);
			}
			type = ALLOC_ARRAY_TYPE(lineno, type, len);
		} else if (accept_tok(STAR)) {
			type = ALLOC_POINTER_TYPE(lineno, type);
		} else {
			return type;
		}
	}
}

static struct expr *parse_lambda_expr(void)
{
	uint16_t lineno;
	Vec *params;

	lineno = get_lineno();
	expect_tok(BACKSLASH);
	params = alloc_vec();
	while (accept_tok(IDENT)) {
		vec_push(params, estrdup(yytext));
	}
	expect_tok(ARROW);
	return ALLOC_LAMBDA_EXPR(lineno, params, parse_expr());
}

static struct expr *parse_array_lit_expr(void)
{
	uint16_t lineno;
	Vec *items;

	lineno = get_lineno();
	expect_tok(OPEN_BRACKET);
	items = alloc_vec();
	do {
		vec_push(items, parse_expr());
	} while (accept_tok(COMMA));
	expect_tok(CLOSE_BRACKET);
	return ALLOC_ARRAY_LIT_EXPR(lineno, items);
}

static struct expr *parse_tuple_or_paren_expr(void)
{
	uint16_t lineno;
	struct expr *expr;
	Vec *items;

	lineno = get_lineno();
	expect_tok(OPEN_PAREN);
	expr = parse_expr();
	if (accept_tok(COMMA)) {
		items = alloc_vec();
		vec_push(items, expr);
		do {
			vec_push(items, parse_expr());
		} while (accept_tok(COMMA));
		expect_tok(CLOSE_PAREN);
		return ALLOC_TUPLE_EXPR(lineno, items);
	}
	expect_tok(CLOSE_PAREN);
	return expr;
}

static struct expr *parse_block_expr(void)
{
	uint16_t lineno;
	Vec *stmts;

	lineno = get_lineno();
	expect_tok(OPEN_BRACE);
	stmts = alloc_vec();
	while (!accept_tok(CLOSE_BRACE)) {
		vec_push(stmts, parse_stmt());
	}
	return ALLOC_BLOCK_EXPR(lineno, stmts);
}

static struct expr *parse_paren_expr(void)
{
	struct expr *expr;

	expect_tok(OPEN_PAREN);
	expr = parse_expr();
	expect_tok(CLOSE_PAREN);
	return expr;
}

static struct expr *parse_if_expr(void)
{
	uint16_t lineno;
	struct expr *cond, *then, *else_;

	lineno = get_lineno();
	expect_tok(IF);
	cond = parse_paren_expr();
	expect_tok(THEN);
	then = parse_expr();
	expect_tok(ELSE);
	else_ = parse_expr();
	return ALLOC_IF_EXPR(lineno, cond, then, else_);
}

static struct switch_pattern *parse_array_switch_pattern(void)
{
	uint16_t lineno;
	Vec *patterns;

	lineno = get_lineno();
	expect_tok(OPEN_BRACKET);
	patterns = alloc_vec();
	do {
		vec_push(patterns, parse_switch_pattern());
	} while (accept_tok(COMMA));
	expect_tok(CLOSE_BRACKET);
	return ALLOC_ARRAY_SWITCH_PATTERN(lineno, patterns);
}

static struct switch_pattern *parse_tuple_switch_pattern(void)
{
	uint16_t lineno;
	Vec *patterns;

	lineno = get_lineno();
	expect_tok(OPEN_PAREN);
	patterns = alloc_vec();
	do {
		vec_push(patterns, parse_switch_pattern());
	} while (accept_tok(COMMA));
	expect_tok(CLOSE_PAREN);
	return ALLOC_TUPLE_SWITCH_PATTERN(lineno, patterns);
}

static struct switch_pattern *parse_primary_switch_pattern(void)
{
	uint16_t lineno;

	lineno = get_lineno();
	switch(peek_tok()) {
	case UNDERSCORE:
		next_tok();
		return ALLOC_UNDERSCORE_SWITCH_PATTERN(lineno);
	case OPEN_BRACKET:
		return parse_array_switch_pattern();
	case OPEN_PAREN:
		return parse_tuple_switch_pattern();
	default:
		return ALLOC_EXPR_SWITCH_PATTERN(lineno, parse_expr());
	}
}

static struct switch_pattern *parse_switch_pattern(void)
{
	uint16_t lineno;
	struct switch_pattern *l;
	Vec *patterns;

	lineno = get_lineno();
	l = parse_primary_switch_pattern();
	if (peek_tok() != PIPE) {
		return l;
	}
	patterns = vec_push(alloc_vec(), l);
	while (accept_tok(PIPE)) {
		vec_push(patterns, parse_primary_switch_pattern());
	}
	return ALLOC_OR_SWITCH_PATTERN(lineno, patterns);
}

static struct switch_case *parse_switch_case(void)
{
	uint16_t lineno;
	struct switch_pattern *l;
	struct expr *r;

	lineno = get_lineno();
	l = parse_switch_pattern();
	expect_tok(BIG_ARROW); // TODO: What if the programmer overloads this?
	r = parse_expr();
	return ALLOC_SWITCH_CASE(lineno, l, r);
}

static struct expr *parse_switch_expr(void)
{
	uint16_t lineno;
	struct expr *ctrl;
	Vec *cases;

	lineno = get_lineno();
	expect_tok(SWITCH);
	ctrl = peek_tok() == OPEN_PAREN ? parse_paren_expr() : NULL;
	expect_tok(OPEN_BRACE);
	cases = alloc_vec();
	while (!accept_tok(CLOSE_BRACE)) {
		// TODO: The case list may need a delimiter
		vec_push(cases, parse_switch_case());
	}
	return ALLOC_SWITCH_EXPR(lineno, ctrl, cases);
}

static struct expr *parse_primary_expr(void)
{
	uint16_t lineno;
	enum tok peek;

	lineno = get_lineno();
	peek = peek_tok();
	switch (peek) {
	case IDENT:
		return ALLOC_IDENT_EXPR(lineno, estrdup(yytext));
	case TRUE:
		next_tok();
		return ALLOC_BOOL_LIT_EXPR(lineno, true);
	case FALSE:
		next_tok();
		return ALLOC_BOOL_LIT_EXPR(lineno, false);
	case INT_LIT:
		// TODO
		break;
	case FLOAT_LIT:
		// TODO
		break;
	case CHAR_LIT:
		next_tok();
		return ALLOC_CHAR_LIT_EXPR(lineno, yylval.char_lit);
	case STRING_LIT:
		next_tok();
		return ALLOC_STRING_LIT_EXPR(lineno,
				estrdup(yylval.string_lit));
	case PLUS_PLUS:
	case MINUS_MINUS:
	case STAR:
	case AMP:
	case TILDE:
	case BANG:
		next_tok();
		return ALLOC_UNARY_OP_EXPR(lineno, peek, parse_primary_expr());
	case BACKSLASH:
		return parse_lambda_expr();
	case OPEN_BRACKET:
		return parse_array_lit_expr();
	case OPEN_PAREN:
		return parse_tuple_or_paren_expr();
	case OPEN_BRACE:
		return parse_block_expr();
	case IF:
		return parse_if_expr();
	case SWITCH:
		return parse_switch_expr();
	default:
		break;
	}
	fatal_error(lineno, "Expected a primary expression, instead got %s",
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
	case DOT:
		return true;
	default:
		return false;
	}
}

static int get_bin_op_prec(enum tok op)
{
	switch (op) {
	case DOT:
		return 12;
	case STAR: case SLASH: case PERCENT:
		return 11;
	case PLUS: case MINUS:
		return 10;
	case LT_LT: case GT_GT:
		return 9;
	case AMP:
		return 8;
	case CARET:
		return 7;
	case PIPE:
		return 6;
	case LT: case GT: case LT_EQ: case GT_EQ:
		return 5;
	case EQ_EQ: case BANG_EQ:
		return 4;
	case AMP_AMP:
		return 3;
	case CARET_CARET:
		return 2;
	case PIPE_PIPE:
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
	uint16_t lineno;
	enum tok peek, op;
	struct expr *r;

	lineno = get_lineno();
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
		l = ALLOC_BIN_OP_EXPR(lineno, op, l, r);
	}
	return l;
}

static struct expr *parse_expr(void)
{
	return parse_expr__(parse_primary_expr(), 0);
}

static struct decl *parse_decl(void)
{
	uint16_t lineno;
	enum tok tok;
	bool is_mut;
	struct type *type;
	char *name;
	struct expr *val;

	lineno = get_lineno();
	tok = next_tok();
	switch (tok) {
	case CONST:
		is_mut = false;
		break;
	case VAR:
		is_mut = true;
		break;
	default:
		fatal_error(lineno, "Expected %s or %s, instead got %s",
				tok_to_str(CONST),
				tok_to_str(VAR),
				tok_to_str(tok));
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
	return ALLOC_DECL(lineno, is_mut, type, name, val);
}

static struct stmt *parse_decl_stmt(void)
{
	uint16_t lineno;

	lineno = get_lineno();
	return ALLOC_DECL_STMT(lineno, parse_decl());
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
	uint16_t lineno;
	struct expr *cond;
	Vec *then_stmts, *else_stmts;
	enum tok peek;

	lineno = get_lineno();
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
			fatal_error(lineno, "Expected %s or %s, instead got %s",
					tok_to_str(IF),
					tok_to_str(OPEN_BRACE),
					tok_to_str(peek));
		}
	} else {
		else_stmts = NULL;
	}
	return ALLOC_IF_STMT(lineno, cond, then_stmts, else_stmts);
}

static struct stmt *parse_do_stmt(void)
{
	uint16_t lineno;
	Vec *stmts;
	struct expr *cond;

	lineno = get_lineno();
	expect_tok(DO);
	stmts = parse_compound_stmt();
	expect_tok(WHILE);
	cond = parse_paren_expr();
	expect_tok(SEMICOLON);
	return ALLOC_DO_STMT(lineno, stmts, cond);
}

static struct stmt *parse_while_stmt(void)
{
	uint16_t lineno;
	Vec *stmts;
	struct expr *cond;

	lineno = get_lineno();
	expect_tok(WHILE);
	cond = parse_paren_expr();
	stmts = parse_compound_stmt();
	return ALLOC_WHILE_STMT(lineno, stmts, cond);
}

static struct stmt *parse_for_stmt(void)
{
	uint16_t lineno;
	struct expr *init, *cond, *post;
	Vec *stmts;

	lineno = get_lineno();
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
	return ALLOC_FOR_STMT(lineno, init, cond, post, stmts);
}

static struct stmt *parse_expr_stmt(void)
{
	uint16_t lineno;

	lineno = get_lineno();
	return ALLOC_EXPR_STMT(lineno, parse_expr());
}

static struct stmt *parse_stmt(void)
{
	switch (peek_tok()) {
	case CONST:
	case VAR:
		return parse_decl_stmt();
	case IF:
		return parse_if_stmt();
	case DO:
		return parse_do_stmt();
	case WHILE:
		return parse_while_stmt();
	case FOR:
		return parse_for_stmt();
	default:
		return parse_expr_stmt();
	}
}

struct ast parse(void)
{
	Vec *decls;

	decls = alloc_vec();
	do {
		vec_push(decls, parse_decl());
	} while (peek_tok() != TEOF);
	return (struct ast){decls};
}
