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

#define MAX_FUNC_ARGS 127
#define MAX_ARRAY_LEN UINT16_MAX

static struct tok cur_tok, lookahead_tok;

static void consume_tok(void)
{
	cur_tok = lookahead_tok;
	lex(&lookahead_tok);
}

static bool accept_tok(enum tok_kind kind)
{
	if (cur_tok.kind == kind) {
		consume_tok();
		return true;
	}
	return false;
}

static void expect_tok_no_consume(enum tok_kind expected)
{
	if (cur_tok.kind != expected) {
		fatal_error(cur_tok.lineno, "Expected %s, instead got %s",
				tok_to_str(expected),
				tok_to_str(cur_tok.kind));
	}
}

static void expect_tok(enum tok_kind expected)
{
	expect_tok_no_consume(expected);
	consume_tok();
}

static NORETURN void expected_either_error(enum tok_kind expected1,
		enum tok_kind expected2)
{
	fatal_error(cur_tok.lineno, "Expected %s or %s, instead got %s",
			tok_to_str(expected1),
			tok_to_str(expected2),
			tok_to_str(cur_tok.kind));
}

static struct type *parse_type(void);
static struct switch_pattern *parse_switch_pattern(void);
static struct expr *parse_expr(void);
static struct stmt *parse_stmt(void);

static struct type *parse_tuple_or_func_type(void)
{
	unsigned lineno;
	struct type *first_type;
	Vec *types;

	expect_tok(OPEN_PAREN);
	first_type = parse_type();
	types = alloc_vec(free_type);
	switch (cur_tok.kind) {
	case COMMA:
		lineno = cur_tok.lineno;
		consume_tok();
		vec_push(types, first_type);
		do {
			vec_push(types, parse_type());
		} while (accept_tok(COMMA));
		expect_tok(CLOSE_PAREN);
		return ALLOC_TUPLE_TYPE(lineno, types);
	case BACK_ARROW:
		lineno = cur_tok.lineno;
		consume_tok();
		do {
			vec_push(types, parse_type());
			if (vec_len(types) > MAX_FUNC_ARGS) {
				fatal_error(lineno, "Function type has more "
				                    "than "XSTR(MAX_FUNC_ARGS)
				                    " parameters");
			}
		} while (accept_tok(COMMA));
		expect_tok(CLOSE_PAREN);
		return ALLOC_FUNC_TYPE(lineno, first_type, types);
	default:
		expected_either_error(COMMA, BACK_ARROW);
	}
}

static struct type *parse_array_type_suffix(struct type *type)
{
	unsigned lineno;
	uint64_t array_len;
	struct expr *array_len_expr;

	lineno = cur_tok.lineno;
	expect_tok(OPEN_BRACKET);
	if (accept_tok(CLOSE_BRACKET)) {
		array_len = 0;
	} else {
		array_len_expr = parse_expr();
		array_len = eval_const_expr(array_len_expr);
		if (array_len > MAX_ARRAY_LEN) {
			fatal_error(array_len_expr->lineno,
					"Specified array length is greater "
					"than "XSTR(MAX_ARRAY_LEN));
		}
		free(array_len_expr);
		expect_tok(CLOSE_BRACKET);
	}
	return ALLOC_ARRAY_TYPE(lineno, type, array_len);
}

static struct type *parse_type_suffix(struct type *type)
{
	unsigned lineno;

	for (;;) {
		lineno = cur_tok.lineno;
		if (cur_tok.kind == OPEN_BRACKET) {
			type = parse_array_type_suffix(type);
		} else if (accept_tok(STAR)) {
			type = ALLOC_POINTER_TYPE(lineno, type);
		} else {
			return type;
		}
	}
}

static struct type *parse_type(void)
{
	unsigned lineno;
	struct type *type;
	char *name;
	Vec *params;

	lineno = cur_tok.lineno;
	switch (cur_tok.kind) {
	case OPEN_PAREN:
		type = parse_tuple_or_func_type();
		break;
	case IDENT:
	case IMPURE:
		name = xstrdup(cur_tok.u.ident);
		consume_tok();
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
		consume_tok();
		type = ALLOC_U8_TYPE(lineno);
		break;
	case U16:
		consume_tok();
		type = ALLOC_U16_TYPE(lineno);
		break;
	case U32:
		consume_tok();
		type = ALLOC_U32_TYPE(lineno);
		break;
	case U64:
		consume_tok();
		type = ALLOC_U64_TYPE(lineno);
		break;
	case I8:
		consume_tok();
		type = ALLOC_I8_TYPE(lineno);
		break;
	case I16:
		consume_tok();
		type = ALLOC_I16_TYPE(lineno);
		break;
	case I32:
		consume_tok();
		type = ALLOC_I32_TYPE(lineno);
		break;
	case I64:
		consume_tok();
		type = ALLOC_I64_TYPE(lineno);
		break;
	case F32:
		consume_tok();
		type = ALLOC_F32_TYPE(lineno);
		break;
	case F64:
		consume_tok();
		type = ALLOC_F64_TYPE(lineno);
		break;
	case BOOL:
		consume_tok();
		type = ALLOC_BOOL_TYPE(lineno);
		break;
	case VOID:
		consume_tok();
		type = ALLOC_VOID_TYPE(lineno);
		break;
	case CHAR:
		consume_tok();
		type = ALLOC_CHAR_TYPE(lineno);
		break;
	default:
		fatal_error(lineno, "Expected a primary type, instead got %s",
				tok_to_str(cur_tok.kind));
	}
	return parse_type_suffix(type);
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

static struct expr *parse_lambda_expr(void)
{
	unsigned lineno;
	Vec *params;

	lineno = cur_tok.lineno;
	expect_tok(BACKSLASH);
	params = alloc_vec(free);
	while (accept_tok(IDENT)) {
		vec_push(params, xstrdup(cur_tok.u.ident));
		if (vec_len(params) > MAX_FUNC_ARGS) {
			fatal_error(lineno, "Lambda expression has more than "
			                     XSTR(MAX_FUNC_ARGS)" parameters");
		}
	}
	expect_tok(ARROW);
	return ALLOC_LAMBDA_EXPR(lineno, params, parse_expr());
}

static struct expr *parse_array_lit_expr(void)
{
	unsigned lineno;
	Vec *items;

	lineno = cur_tok.lineno;
	expect_tok(OPEN_BRACKET);
	items = alloc_vec(free_expr);
	do {
		vec_push(items, parse_expr());
		if (vec_len(items) > MAX_ARRAY_LEN) {
			fatal_error(lineno, "Array literal has more than "
			                     XSTR(MAX_ARRAY_LEN)" items");
		}
	} while (accept_tok(COMMA));
	expect_tok(CLOSE_BRACKET);
	return ALLOC_ARRAY_LIT_EXPR(lineno, items);
}

static struct expr *parse_tuple_or_paren_expr(void)
{
	unsigned lineno;
	struct expr *expr;
	Vec *items;

	lineno = cur_tok.lineno;
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
	unsigned lineno;

	lineno = cur_tok.lineno;
	return ALLOC_BLOCK_EXPR(lineno, parse_compound_stmt());
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
	unsigned lineno;
	struct expr *cond, *then, *else_;

	lineno = cur_tok.lineno;
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
	unsigned lineno;
	Vec *patterns;

	lineno = cur_tok.lineno;
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
	unsigned lineno;
	Vec *patterns;

	lineno = cur_tok.lineno;
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
	unsigned lineno;

	lineno = cur_tok.lineno;
	switch(cur_tok.kind) {
	case UNDERSCORE:
		consume_tok();
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
	unsigned lineno;
	struct switch_pattern *first_pattern;
	Vec *patterns;

	lineno = cur_tok.lineno;
	first_pattern = parse_primary_switch_pattern();
	if (cur_tok.kind != PIPE) {
		return first_pattern;
	}
	patterns = alloc_vec(free_switch_pattern);
	vec_push(patterns, first_pattern);
	while (accept_tok(PIPE)) {
		vec_push(patterns, parse_primary_switch_pattern());
	}
	return ALLOC_OR_SWITCH_PATTERN(lineno, patterns);
}

static struct switch_case *parse_switch_case(void)
{
	unsigned lineno;
	struct switch_pattern *l;
	struct expr *r;

	lineno = cur_tok.lineno;
	l = parse_switch_pattern();
	expect_tok(BIG_ARROW);
	r = parse_expr();
	return ALLOC_SWITCH_CASE(lineno, l, r);
}

static struct expr *parse_switch_expr(void)
{
	unsigned lineno;
	struct expr *ctrl;
	Vec *cases;

	lineno = cur_tok.lineno;
	expect_tok(SWITCH);
	ctrl = cur_tok.kind == OPEN_PAREN ? parse_paren_expr() : NULL;
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
	unsigned lineno;

	lineno = cur_tok.lineno;
	switch (cur_tok.kind) {
	case IDENT: {
		char *ident = xstrdup(cur_tok.u.ident);

		consume_tok();
		return ALLOC_IDENT_EXPR(lineno, ident);
	}
	case TRUE:
		consume_tok();
		return ALLOC_BOOL_LIT_EXPR(lineno, true);
	case FALSE:
		consume_tok();
		return ALLOC_BOOL_LIT_EXPR(lineno, false);
	case INT_LIT: {
		uint64_t val = cur_tok.u.int_lit;

		consume_tok();
		return ALLOC_INT_LIT_EXPR(lineno, val);
	}
	case FLOAT_LIT: {
		double val = cur_tok.u.float_lit;

		consume_tok();
		return ALLOC_FLOAT_LIT_EXPR(lineno, val);
	}
	case CHAR_LIT: {
		uint32_t val = cur_tok.u.char_lit;

		consume_tok();
		return ALLOC_CHAR_LIT_EXPR(lineno, val);
	}
	case STRING_LIT: {
		char *str = xstrdup(cur_tok.u.string_lit.val);
		unsigned len = cur_tok.u.string_lit.len;

		consume_tok();
		return ALLOC_STRING_LIT_EXPR(lineno, str, len);
	}
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
	fatal_error(cur_tok.lineno, "Expected a primary expression, instead "
	                            "got %s", tok_to_str(cur_tok.kind));
}

static Vec *parse_func_call_args(void)
{
	Vec *args;

	expect_tok(OPEN_PAREN);
	args = alloc_vec(free_expr);
	if (accept_tok(CLOSE_PAREN)) {
		return args;
	}
	for (;;) {
		vec_push(args, parse_expr());
		if (accept_tok(CLOSE_PAREN)) {
			break;
		}
		expect_tok(COMMA);
	}
	return args;
}

static struct expr *parse_postfix_unary_expr(void)
{
	unsigned lineno;
	struct expr *operand;

	operand = parse_primary_expr();
	lineno = cur_tok.lineno;
	switch (cur_tok.kind) {
	case PLUS_PLUS:
		return ALLOC_UNARY_OP_EXPR(lineno, POST_INC_OP, operand);
	case MINUS_MINUS:
		return ALLOC_UNARY_OP_EXPR(lineno, POST_DEC_OP, operand);
	case OPEN_PAREN:
		return ALLOC_FUNC_CALL_EXPR(lineno, operand,
				parse_func_call_args());
	default:
		return operand;
	}
}

static struct expr *parse_unary_expr(void)
{
	unsigned lineno;
	enum unary_op op;

	lineno = cur_tok.lineno;
	switch (cur_tok.kind) {
	case MINUS:
		op = NEG_OP;
		break;
	case PLUS_PLUS:
		op = PRE_INC_OP;
		break;
	case MINUS_MINUS:
		op = PRE_DEC_OP;
		break;
	case STAR:
		op = DEREF_OP;
		break;
	case AMP:
		op = REF_OP;
		break;
	case TILDE:
		op = BIT_NOT_OP;
		break;
	case BANG:
		op = LOG_NOT_OP;
		break;
	default:
		return parse_postfix_unary_expr();
	}
	consume_tok();
	return ALLOC_UNARY_OP_EXPR(lineno, op, parse_unary_expr());
}

static bool is_bin_op(enum tok_kind tok_kind)
{
	switch (tok_kind) {
	case PLUS: case MINUS:
	case STAR: case SLASH: case PERCENT:
	case LT: case GT: case LT_EQ: case GT_EQ: case EQ_EQ: case BANG_EQ:
	case AMP: case PIPE: case CARET: case LT_LT: case GT_GT:
	case AMP_AMP: case PIPE_PIPE:
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

static enum bin_op tok_to_bin_op(enum tok_kind tok_kind)
{
	switch (tok_kind) {
	case PLUS:
		return ADD_OP;
	case MINUS:
		return SUB_OP;
	case STAR:
		return MUL_OP;
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
		return EQ_OP;
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
	case EQ:
		return ASSIGN_OP;
	case PLUS_EQ:
		return ADD_ASSIGN_OP;
	case MINUS_EQ:
		return SUB_ASSIGN_OP;
	case STAR_EQ:
		return MUL_ASSIGN_OP;
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
	[ADD_OP] = {
		.prec = 10, .assoc = L_ASSOC
	},
	[SUB_OP] = {
		.prec = 10, .assoc = L_ASSOC
	},
	[MUL_OP] = {
		.prec = 11, .assoc = L_ASSOC
	},
	[DIV_OP] = {
		.prec = 11, .assoc = L_ASSOC
	},
	[MOD_OP] = {
		.prec = 11, .assoc = L_ASSOC
	},
	[LT_OP] = {
		.prec = 5, .assoc = NON_ASSOC
	},
	[GT_OP] = {
		.prec = 5, .assoc = NON_ASSOC
	},
	[LT_EQ_OP] = {
		.prec = 5, .assoc = NON_ASSOC
	},
	[GT_EQ_OP] = {
		.prec = 5, .assoc = NON_ASSOC
	},
	[EQ_OP] = {
		.prec = 4, .assoc = L_ASSOC
	},
	[NOT_EQ_OP] = {
		.prec = 4, .assoc = L_ASSOC
	},
	[BIT_AND_OP] = {
		.prec = 8, .assoc = L_ASSOC
	},
	[BIT_OR_OP] = {
		.prec = 6, .assoc = L_ASSOC
	},
	[BIT_XOR_OP] = {
		.prec = 7, .assoc = L_ASSOC
	},
	[BIT_SHIFT_L_OP] = {
		.prec = 9, .assoc = L_ASSOC
	},
	[BIT_SHIFT_R_OP] = {
		.prec = 9, .assoc = L_ASSOC
	},
	[LOG_AND_OP] = {
		.prec = 3, .assoc = L_ASSOC
	},
	[LOG_OR_OP] = {
		.prec = 1, .assoc = L_ASSOC
	},
	[ASSIGN_OP] = {
		.prec = 0, .assoc = NON_ASSOC
	},
	[ADD_ASSIGN_OP] = {
		.prec = 0, .assoc = NON_ASSOC
	},
	[SUB_ASSIGN_OP] = {
		.prec = 0, .assoc = NON_ASSOC
	},
	[MUL_ASSIGN_OP] = {
		.prec = 0, .assoc = NON_ASSOC
	},
	[DIV_ASSIGN_OP] = {
		.prec = 0, .assoc = NON_ASSOC
	},
	[MOD_ASSIGN_OP] = {
		.prec = 0, .assoc = NON_ASSOC
	},
	[BIT_AND_ASSIGN_OP] = {
		.prec = 0, .assoc = NON_ASSOC
	},
	[BIT_OR_ASSIGN_OP] = {
		.prec = 0, .assoc = NON_ASSOC
	},
	[BIT_XOR_ASSIGN_OP] = {
		.prec = 0, .assoc = NON_ASSOC
	},
	[BIT_SHIFT_L_ASSIGN_OP] = {
		.prec = 0, .assoc = NON_ASSOC
	},
	[BIT_SHIFT_R_ASSIGN_OP] = {
		.prec = 0, .assoc = NON_ASSOC
	},
	[FIELD_OP] = {
		.prec = 12, .assoc = L_ASSOC
	}
};

static int get_bin_op_prec(enum bin_op op)
{
	return bin_op_info[op].prec;
}

static enum assoc get_bin_op_assoc(enum bin_op op)
{
	return bin_op_info[op].assoc;
}

// Precedence climbing
static struct expr *parse_expr__(struct expr *l, int min_prec)
{
	unsigned lineno;
	enum bin_op op, op2;
	struct expr *r;

	for (;;) {
		if (!is_bin_op(cur_tok.kind)) {
			break;
		}
		lineno = cur_tok.lineno;
		op = tok_to_bin_op(cur_tok.kind);
		if (get_bin_op_prec(op) < min_prec) {
			break;
		}
		consume_tok();
		r = parse_unary_expr();
		for (;;) {
			if (!is_bin_op(cur_tok.kind)) {
				break;
			}
			lineno = cur_tok.lineno;
			op2 = tok_to_bin_op(cur_tok.kind);
			if (get_bin_op_prec(op2) <= get_bin_op_prec(op) ||
					(get_bin_op_assoc(op2) == R_ASSOC &&
					 get_bin_op_prec(op2) ==
					 get_bin_op_prec(op))) {
				break;
			}
			r = parse_expr__(r, get_bin_op_prec(op2));
		}
		l = ALLOC_BIN_OP_EXPR(lineno, op, l, r);
	}
	return l;
}

static struct expr *parse_expr(void)
{
	return parse_expr__(parse_unary_expr(), 0);
}

static struct decl *parse_decl(void);

static struct stmt *parse_decl_stmt(void)
{
	unsigned lineno;

	lineno = cur_tok.lineno;
	return ALLOC_DECL_STMT(lineno, parse_decl());
}

static struct stmt *parse_if_stmt(void)
{
	unsigned lineno;
	struct expr *cond;
	Vec *then_stmts, *else_stmts;

	lineno = cur_tok.lineno;
	expect_tok(IF);
	cond = parse_paren_expr();
	then_stmts = parse_compound_stmt();
	if (accept_tok(ELSE)) {
		switch (cur_tok.kind) {
		case IF:
			else_stmts = alloc_vec(free_stmt);
			vec_push(else_stmts, parse_if_stmt());
			break;
		case OPEN_BRACE:
			else_stmts = parse_compound_stmt();
			break;
		default:
			expected_either_error(IF, OPEN_BRACE);
		}
	} else {
		else_stmts = NULL;
	}
	return ALLOC_IF_STMT(lineno, cond, then_stmts, else_stmts);
}

static struct stmt *parse_do_stmt(void)
{
	unsigned lineno;
	Vec *stmts;
	struct expr *cond;

	lineno = cur_tok.lineno;
	expect_tok(DO);
	stmts = parse_compound_stmt();
	expect_tok(WHILE);
	cond = parse_paren_expr();
	expect_tok(SEMICOLON);
	return ALLOC_DO_STMT(lineno, stmts, cond);
}

static struct stmt *parse_while_stmt(void)
{
	unsigned lineno;
	Vec *stmts;
	struct expr *cond;

	lineno = cur_tok.lineno;
	expect_tok(WHILE);
	cond = parse_paren_expr();
	stmts = parse_compound_stmt();
	return ALLOC_WHILE_STMT(lineno, stmts, cond);
}

static struct stmt *parse_for_stmt(void)
{
	unsigned lineno;
	struct expr *init, *cond, *post;
	Vec *stmts;

	lineno = cur_tok.lineno;
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

static struct stmt *parse_return_stmt(void)
{
	unsigned lineno;
	struct expr *expr;

	lineno = cur_tok.lineno;
	expect_tok(RETURN);
	if (accept_tok(SEMICOLON)) {
		expr = NULL;
	} else {
		expr = parse_expr();
		expect_tok(SEMICOLON);
	}
	return ALLOC_RETURN_STMT(lineno, expr);
}

static struct stmt *parse_expr_stmt(void)
{
	unsigned lineno;

	lineno = cur_tok.lineno;
	return ALLOC_EXPR_STMT(lineno, parse_expr());
}

static struct stmt *parse_stmt(void)
{
	switch (cur_tok.kind) {
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
	case RETURN:
		return parse_return_stmt();
	default:
		return parse_expr_stmt();
	}
}

static struct decl *parse_data_decl(void)
{
	unsigned lineno;
	bool is_const;
	struct type *type;
	char *name;
	struct expr *init;

	lineno = cur_tok.lineno;
	switch (cur_tok.kind) {
	case CONST:
		is_const = true;
		break;
	case VAR:
		is_const = false;
		break;
	default:
		expected_either_error(CONST, VAR);
	}
	consume_tok();
	type = parse_type();
	expect_tok_no_consume(IDENT);
	name = xstrdup(cur_tok.u.ident);
	consume_tok();
	if (accept_tok(SEMICOLON)) {
		init = NULL;
	} else {
		expect_tok(EQ);
		init = parse_expr();
		expect_tok(SEMICOLON);
	}
	return ALLOC_DATA_DECL(lineno, is_const, type, name, init);
}

static struct decl *parse_typedef(void)
{
	unsigned lineno;
	char *name;
	Vec *params;
	struct type *type;

	expect_tok(TYPEDEF);
	expect_tok_no_consume(IDENT);
	lineno = cur_tok.lineno;
	name = xstrdup(cur_tok.u.ident);
	consume_tok();
	params = alloc_vec(free);
	if (accept_tok(LT)) {
		do {
			expect_tok_no_consume(IDENT);
			vec_push(params, xstrdup(cur_tok.u.ident));
			consume_tok();
		} while (accept_tok(COMMA));
		expect_tok(GT);
	}
	type = parse_type();
	expect_tok(SEMICOLON);
	return ALLOC_TYPEDEF_DECL(lineno, name, params, type);
}

static struct decl *parse_func_decl(void)
{
	unsigned lineno;
	struct type *type, *return_type;
	char *name;
	Vec *param_types, *param_names;
	Vec *body_stmts;

	return_type = parse_type();
	expect_tok_no_consume(IDENT);
	lineno = cur_tok.lineno;
	name = xstrdup(cur_tok.u.ident);
	consume_tok();
	expect_tok(OPEN_PAREN);
	param_types = alloc_vec(free_type);
	param_names = alloc_vec(free);
	if (cur_tok.kind == VOID) {
		consume_tok();
	} else {
		do {
			vec_push(param_types, parse_type());
			expect_tok_no_consume(IDENT);
			vec_push(param_names, xstrdup(cur_tok.u.ident));
			consume_tok();
		} while (accept_tok(COMMA));
	}
	expect_tok(CLOSE_PAREN);
	body_stmts = parse_compound_stmt();
	type = ALLOC_FUNC_TYPE(lineno, return_type, param_types);
	return ALLOC_FUNC_DECL(lineno, type, name, param_names, body_stmts);
}

static struct decl *parse_decl(void)
{
	switch (cur_tok.kind) {
	case CONST:
	case VAR:
		return parse_data_decl();
	case TYPEDEF:
		return parse_typedef();
	default:
		return parse_func_decl();
	}
}

static struct ast parse_file__(void)
{
	Vec *decls;
	struct ast ast;

	decls = alloc_vec(free_decl);
	do {
		vec_push(decls, parse_decl());
	} while (cur_tok.kind != TEOF);
	ast.decls = decls;
	return ast;
}

struct ast parse_file(const char *filename)
{
	struct ast ast;

	init_lex(filename);
	lex(&cur_tok);
	lex(&lookahead_tok);
	ast = parse_file__();
	cleanup_lex();
	return ast;
}
