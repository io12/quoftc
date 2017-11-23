#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include "ds.h"
#include "quoftc.h"
#include "lex.h"
#include "ast.h"
#include "parse.h"
#include "eval.h"

static NORETURN void eval_error(struct expr *expr)
{
	fatal_error(expr->lineno, "Expected an unsigned integer constant "
	                          "expression");
}

static uint64_t eval_unary_op_const_expr(struct expr *expr)
{
	enum tok op = expr->u.unary_op.op;
	struct expr *subexpr = expr->u.unary_op.subexpr;

	switch (op) {
	case PLUS_PLUS:
	case MINUS_MINUS:
	case STAR:
	case AMP:
	case BANG:
		eval_error(expr);
	case TILDE:
		return ~eval_const_expr(subexpr);
	default:
		internal_error();
	}
}

static uint64_t eval_bin_op_const_expr(struct expr *expr)
{
	enum tok op = expr->u.bin_op.op;
	struct expr *l = expr->u.bin_op.l,
	            *r = expr->u.bin_op.r;
	uint64_t l_val = eval_const_expr(l),
	         r_val = eval_const_expr(r);

	switch (op) {
	case PLUS:
		return l_val + r_val;
	case MINUS:
		return l_val - r_val;
	case STAR:
		return l_val * r_val;
	case SLASH:
		return l_val / r_val;
	case PERCENT:
		return l_val % r_val;
	case AMP:
		return l_val & r_val;
	case PIPE:
		return l_val | r_val;
	case CARET:
		return l_val ^ r_val;
	case LT_LT:
		return l_val << r_val;
	case GT_GT:
		return l_val >> r_val;
	case AMP_AMP:
	case PIPE_PIPE:
	case CARET_CARET:
	case LT:
	case GT:
	case LT_EQ:
	case GT_EQ:
	case EQ_EQ:
	case BANG_EQ:
	case EQ:
	case PLUS_EQ:
	case MINUS_EQ:
	case STAR_EQ:
	case SLASH_EQ:
	case PERCENT_EQ:
	case AMP_EQ:
	case PIPE_EQ:
	case CARET_EQ:
	case LT_LT_EQ:
	case GT_GT_EQ:
	case DOT:
		eval_error(expr);
	default:
		internal_error();
	}
}

uint64_t eval_const_expr(struct expr *expr)
{
	switch (expr->kind) {
	case BOOL_LIT_EXPR:
	case CHAR_LIT_EXPR:
	case STRING_LIT_EXPR:
	case LAMBDA_EXPR:
	case ARRAY_LIT_EXPR:
	case IDENT_EXPR:
	case BLOCK_EXPR:
	case IF_EXPR:
	case SWITCH_EXPR:
	case TUPLE_EXPR:
		eval_error(expr);
	case UNARY_OP_EXPR:
		return eval_unary_op_const_expr(expr);
	case BIN_OP_EXPR:
		return eval_bin_op_const_expr(expr);
	}
	// NOTREACHED
	internal_error();
}
