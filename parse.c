#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "ds.h"
#include "quoftc.h"
#include "lex.h"
#include "ast.h"
#include "eval.h"
#include "parse.h"

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
	types = alloc_vec(free_type);
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
	struct expr *array_len_expr;
	uint64_t array_len;

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
			params = alloc_vec(free_type);
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
				array_len = 0;
			} else {
				array_len_expr = parse_expr();
				array_len = eval_const_expr(array_len_expr);
				free(array_len_expr);
				expect_tok(CLOSE_BRACKET);
			}
			type = ALLOC_ARRAY_TYPE(lineno, type, array_len);
		} else if (accept_tok(STAR)) {
			type = ALLOC_POINTER_TYPE(lineno, type);
		} else {
			return type;
		}
	}
}

static enum unary_op tok_to_unary_op(enum tok tok)
{
	switch (tok) {
	case PLUS_PLUS:
		return INC_OP;
	case MINUS_MINUS:
		return DEC_OP;
	case STAR:
		return DEREF_OP;
	case AMP:
		return REF_OP;
	case TILDE:
		return BIT_NOT_OP;
	case BANG:
		return LOG_NOT_OP;
	default:
		internal_error();
	}
}

static struct expr *parse_lambda_expr(void)
{
	uint16_t lineno;
	Vec *params;

	lineno = get_lineno();
	expect_tok(BACKSLASH);
	params = alloc_vec(free);
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
	items = alloc_vec(free_expr);
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
		items = alloc_vec(free_expr);
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
	stmts = alloc_vec(free_stmt);
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
	patterns = alloc_vec(free_switch_pattern);
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
	patterns = alloc_vec(free_switch_pattern);
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
	patterns = alloc_vec(free_switch_pattern);
	vec_push(patterns, l);
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
	cases = alloc_vec(free_switch_case);
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
		next_tok();
		return ALLOC_IDENT_EXPR(lineno, estrdup(yytext));
	case TRUE:
		next_tok();
		return ALLOC_BOOL_LIT_EXPR(lineno, true);
	case FALSE:
		next_tok();
		return ALLOC_BOOL_LIT_EXPR(lineno, false);
	case INT_LIT:
		next_tok();
		return ALLOC_INT_LIT_EXPR(lineno, yylval.int_lit);
	case FLOAT_LIT:
		next_tok();
		return ALLOC_FLOAT_LIT_EXPR(lineno, yylval.float_lit);
	case CHAR_LIT:
		next_tok();
		return ALLOC_CHAR_LIT_EXPR(lineno, yylval.char_lit);
	case STRING_LIT: {
		char *str = yylval.string_lit.val;
		uint64_t len = yylval.string_lit.len;

		next_tok();
		return ALLOC_STRING_LIT_EXPR(lineno, estrdup(str), len);
	}
	case PLUS_PLUS:
	case MINUS_MINUS:
	case STAR:
	case AMP:
	case TILDE:
	case BANG:
		next_tok();
		return ALLOC_UNARY_OP_EXPR(lineno, tok_to_unary_op(peek),
				parse_primary_expr());
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

static enum bin_op tok_to_bin_op(enum tok tok)
{
	switch (tok) {
	case PLUS:
		return ADD_OP;
	case MINUS:
		return SUB_OP;
	case STAR:
		return MULT_OP;
	case SLASH:
		return DIV_OP;
	case PERCENT:
		return MOD_OP;
	case LT:
		return LT_OP;
	case GT:
		return GT_OP;
	case LT_EQ:
		return LT_EQ_OP;
	case GT_EQ:
		return GT_EQ_OP;
	case EQ_EQ:
		return LOG_EQ_OP;
	case BANG_EQ:
		return NOT_EQ_OP;
	case AMP:
		return BIT_AND_OP;
	case PIPE:
		return BIT_OR_OP;
	case CARET:
		return BIT_XOR_OP;
	case LT_LT:
		return BIT_SHIFT_L_OP;
	case GT_GT:
		return BIT_SHIFT_R_OP;
	case AMP_AMP:
		return LOG_AND_OP;
	case PIPE_PIPE:
		return LOG_OR_OP;
	case CARET_CARET:
		return LOG_XOR_OP;
	case EQ:
		return ASSIGN_OP;
	case PLUS_EQ:
		return ADD_ASSIGN_OP;
	case MINUS_EQ:
		return SUB_ASSIGN_OP;
	case STAR_EQ:
		return MULT_ASSIGN_OP;
	case SLASH_EQ:
		return DIV_ASSIGN_OP;
	case PERCENT_EQ:
		return MOD_ASSIGN_OP;
	case AMP_EQ:
		return BIT_AND_ASSIGN_OP;
	case PIPE_EQ:
		return BIT_OR_ASSIGN_OP;
	case CARET_EQ:
		return BIT_XOR_ASSIGN_OP;
	case LT_LT_EQ:
		return BIT_SHIFT_L_ASSIGN_OP;
	case GT_GT_EQ:
		return BIT_SHIFT_R_ASSIGN_OP;
	case DOT:
		return FIELD_OP;
	default:
		internal_error();
	}
}

enum assoc {
	L_ASSOC, R_ASSOC, NON_ASSOC
};

static struct {
	int prec;
	enum assoc assoc;
} bin_op_info[] = {
	[ADD_OP] = { .prec = 10, .assoc = L_ASSOC },
	[SUB_OP] = { .prec = 10, .assoc = L_ASSOC },
	[MULT_OP] = { .prec = 11, .assoc = L_ASSOC },
	[DIV_OP] = { .prec = 11, .assoc = L_ASSOC },
	[MOD_OP] = { .prec = 11, .assoc = L_ASSOC },
	[LT_OP] = { .prec = 5, .assoc = NON_ASSOC },
	[GT_OP] = { .prec = 5, .assoc = NON_ASSOC },
	[LT_EQ_OP] = { .prec = 5, .assoc = NON_ASSOC },
	[GT_EQ_OP] = { .prec = 5, .assoc = NON_ASSOC },
	[LOG_EQ_OP] = { .prec = 4, .assoc = L_ASSOC },
	[NOT_EQ_OP] = { .prec = 4, .assoc = L_ASSOC },
	[BIT_AND_OP] = { .prec = 8, .assoc = L_ASSOC },
	[BIT_OR_OP] = { .prec = 6, .assoc = L_ASSOC },
	[BIT_XOR_OP] = { .prec = 7, .assoc = L_ASSOC },
	[BIT_SHIFT_L_OP] = { .prec = 9, .assoc = L_ASSOC },
	[BIT_SHIFT_R_OP] = { .prec = 9, .assoc = L_ASSOC },
	[LOG_AND_OP] = { .prec = 3, .assoc = L_ASSOC },
	[LOG_OR_OP] = { .prec = 1, .assoc = L_ASSOC },
	[LOG_XOR_OP] = { .prec = 2, .assoc = L_ASSOC },
	[ASSIGN_OP] = { .prec = 0, .assoc = NON_ASSOC },
	[ADD_ASSIGN_OP] = { .prec = 0, .assoc = NON_ASSOC },
	[SUB_ASSIGN_OP] = { .prec = 0, .assoc = NON_ASSOC },
	[MULT_ASSIGN_OP] = { .prec = 0, .assoc = NON_ASSOC },
	[DIV_ASSIGN_OP] = { .prec = 0, .assoc = NON_ASSOC },
	[MOD_ASSIGN_OP] = { .prec = 0, .assoc = NON_ASSOC },
	[BIT_AND_ASSIGN_OP] = { .prec = 0, .assoc = NON_ASSOC },
	[BIT_OR_ASSIGN_OP] = { .prec = 0, .assoc = NON_ASSOC },
	[BIT_XOR_ASSIGN_OP] = { .prec = 0, .assoc = NON_ASSOC },
	[BIT_SHIFT_L_ASSIGN_OP] = { .prec = 0, .assoc = NON_ASSOC },
	[BIT_SHIFT_R_ASSIGN_OP] = { .prec = 0, .assoc = NON_ASSOC },
	[FIELD_OP] = { .prec = 12, .assoc = L_ASSOC }
};

static int get_bin_op_prec(enum bin_op op)
{
	if (!IN_RANGE(op, 0, ARRAY_LEN(bin_op_info))) {
		internal_error();
	}
	return bin_op_info[op].prec;
}

static enum assoc get_bin_op_assoc(enum bin_op op)
{
	if (!IN_RANGE(op, 0, ARRAY_LEN(bin_op_info))) {
		internal_error();
	}
	return bin_op_info[op].assoc;
}

// Precedence climbing
static struct expr *parse_expr__(struct expr *l, int min_prec)
{
	uint16_t lineno;
	enum tok peek;
	enum bin_op op, op2;
	struct expr *r;

	lineno = get_lineno();
	peek = peek_tok();
	for (;;) {
		if (!is_bin_op(peek)) {
			break;
		}
		op = tok_to_bin_op(peek);
		if (get_bin_op_prec(op) < min_prec) {
			break;
		}
		next_tok();
		r = parse_primary_expr();
		peek = peek_tok();
		for (;;) {
			if (!is_bin_op(peek)) {
				break;
			}
			op2 = tok_to_bin_op(peek);
			if (get_bin_op_prec(op2) <= get_bin_op_prec(op) ||
					(get_bin_op_assoc(op2) == R_ASSOC &&
					 get_bin_op_prec(op2) ==
					 get_bin_op_prec(op))) {
				break;
			}
			r = parse_expr__(r, get_bin_op_prec(op2));
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
	stmts = alloc_vec(free_stmt);
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
			else_stmts = alloc_vec(free_stmt);
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

	decls = alloc_vec(free_decl);
	do {
		vec_push(decls, parse_decl());
	} while (peek_tok() != TEOF);
	return (struct ast){decls};
}
