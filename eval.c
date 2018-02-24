#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include "ds.h"
#include "quoftc.h"
#include "ast.h"
#include "eval.h"

static NORETURN void eval_error(struct expr *expr)
{
	fatal_error(expr->lineno, "Expected an unsigned integer constant "
	                          "expression");
}

static uint64_t eval_unary_op_const_expr(struct expr *expr)
{
	enum unary_op op = expr->u.unary_op.op;
	struct expr *operand = expr->u.unary_op.operand;

	switch (op) {
	case BIT_NOT_OP:
		return ~eval_const_expr(operand);
	default:
		eval_error(expr);
	}
}

static uint64_t eval_bin_op_const_expr(struct expr *expr)
{
	enum bin_op op = expr->u.bin_op.op;
	struct expr *l_expr = expr->u.bin_op.l,
	            *r_expr = expr->u.bin_op.r;
	uint64_t l = eval_const_expr(l_expr),
	         r = eval_const_expr(r_expr);

	switch (op) {
	case ADD_OP:
		return l + r;
	case SUB_OP:
		return l - r;
	case MUL_OP:
		return l * r;
	case DIV_OP:
		return l / r;
	case MOD_OP:
		return l % r;
	case BIT_AND_OP:
		return l & r;
	case BIT_OR_OP:
		return l | r;
	case BIT_XOR_OP:
		return l ^ r;
	case BIT_SHIFT_L_OP:
		return l << r;
	case BIT_SHIFT_R_OP:
		return l >> r;
	default:
		eval_error(expr);
	}
}

uint64_t eval_const_expr(struct expr *expr)
{
	switch (expr->kind) {
	case BOOL_LIT_EXPR:
	case FLOAT_LIT_EXPR:
	case CHAR_LIT_EXPR:
	case STRING_LIT_EXPR:
	case LAMBDA_EXPR:
	case ARRAY_LIT_EXPR:
	case IDENT_EXPR:
	case BLOCK_EXPR:
	case IF_EXPR:
	case SWITCH_EXPR:
	case TUPLE_EXPR:
	case FUNC_CALL_EXPR:
	case FIELD_ACCESS_EXPR:
		eval_error(expr);
	case INT_LIT_EXPR:
		return expr->u.int_lit.val;
	case UNARY_OP_EXPR:
		return eval_unary_op_const_expr(expr);
	case BIN_OP_EXPR:
		return eval_bin_op_const_expr(expr);
	}
	// NOTREACHED
	internal_error();
}
