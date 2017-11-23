#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "ds.h"
#include "quoftc.h"
#include "lex.h"
#include "parse.h"
#include "ast.h"
#include "eval.h"
#include "symbol_table.h"
#include "check_semantics.h"

static struct symbol_table sym_tbl;

static NORETURN void compat_error(struct expr *expr)
{
	fatal_error(expr->lineno, "Expression not compatible with type");
}

static NORETURN void lvalue_error(struct expr *expr)
{
	fatal_error(expr->lineno, "Value mutated that is not an lvalue");
}

static bool is_pure_expr(struct expr *expr);

static bool vec_has_pure_items(Vec *vec)
{
	size_t i;
	for (i = 0; i < vec_len(vec); i++) {
		if (!is_pure_expr(vec_get(vec, i))) {
			return false;
		}
	}
	return true;
}

static bool is_pure_block_expr(struct expr *expr)
{
	Vec *stmts;

	if (expr->kind != BLOCK_EXPR) {
		internal_error();
	}
	stmts = expr->u.block.stmts;
	(void) stmts;
	return false; // TODO
}

static bool is_pure_expr(struct expr *expr)
{
	switch (expr->kind) {
	case BOOL_LIT_EXPR:
	case CHAR_LIT_EXPR:
	case STRING_LIT_EXPR:
	case LAMBDA_EXPR:
	case IDENT_EXPR:
		return true;
	case UNARY_OP_EXPR:
		return is_pure_expr(expr->u.unary_op.subexpr);
	case BIN_OP_EXPR:
		return is_pure_expr(expr->u.bin_op.l)
			&& is_pure_expr(expr->u.bin_op.r);
	case ARRAY_LIT_EXPR:
		return vec_has_pure_items(expr->u.array_lit.val);
	case BLOCK_EXPR:
		return is_pure_block_expr(expr);
	case IF_EXPR:
		return is_pure_expr(expr->u.if_.cond)
			&& is_pure_expr(expr->u.if_.then)
			&& is_pure_expr(expr->u.if_.else_);
	case SWITCH_EXPR:
		return NULL; // TODO
	case TUPLE_EXPR:
		return vec_has_pure_items(expr->u.tuple.items);
	default:
		internal_error();
	}
}

static bool is_lvalue(struct expr *expr)
{
	(void) expr;
	return false;
}

static bool is_num_type(struct type *type)
{
	switch (type->kind) {
	case U8_TYPE:
	case U16_TYPE:
	case U32_TYPE:
	case U64_TYPE:
	case I8_TYPE:
	case I16_TYPE:
	case I32_TYPE:
	case I64_TYPE:
	case F32_TYPE:
	case F64_TYPE:
		return true;
	default:
		return false;
	}
}

static bool is_unsigned_type(struct type *type)
{
	switch (type->kind) {
	case U8_TYPE:
	case U16_TYPE:
	case U32_TYPE:
	case U64_TYPE:
		return true;
	default:
		return false;
	}
}

static void check_expr_with_type(struct type *type, struct expr *expr);

static void check_unary_op_expr_with_type(struct type *type, struct expr *expr)
{
	enum tok op = expr->u.unary_op.op;
	struct expr *subexpr = expr->u.unary_op.subexpr;

	switch (op) {
	case PLUS_PLUS:
	case MINUS_MINUS:
		if (!is_num_type(type)) {
			compat_error(expr);
		}
		if (!is_lvalue(subexpr)) {
			lvalue_error(expr);
		}
		check_expr_with_type(type, subexpr);
		return;
	case STAR: {
		struct type *subtype;

		subtype = ALLOC_POINTER_TYPE(type->lineno, type);
		check_expr_with_type(subtype, subexpr);
		free(subtype);
		return;
	}
	case AMP:
		if (type->kind != POINTER_TYPE) {
			compat_error(expr);
		}
		if (!is_lvalue(subexpr)) {
			lvalue_error(expr);
		}
		check_expr_with_type(type->u.pointer.l, subexpr);
		return;
	case TILDE:
		if (!is_unsigned_type(type)) {
			compat_error(expr);
		}
		check_expr_with_type(type, subexpr);
		return;
	case BANG:
		if (type->kind != BOOL_TYPE) {
			compat_error(expr);
		}
		check_expr_with_type(type, subexpr);
		return;
	default:
		internal_error();
	}
}

static void check_bin_op_expr_with_type(struct type *type, struct expr *expr)
{
	enum tok op = expr->u.bin_op.op;
	struct expr *l = expr->u.bin_op.l,
	            *r = expr->u.bin_op.r;

	switch (op) {
	case PLUS:
	case MINUS:
	case STAR:
	case SLASH:
	case PERCENT:
		if (!is_num_type(type)) {
			compat_error(expr);
		}
		check_expr_with_type(type, l);
		check_expr_with_type(type, r);
		return;
	case LT:
	case GT:
	case LT_EQ:
	case GT_EQ:
		if (type->kind != BOOL_TYPE) {
			compat_error(expr);
		}
		// TODO: Num type checking
		return;
	case EQ_EQ:
	case BANG_EQ:
		if (type->kind != BOOL_TYPE) {
			compat_error(expr);
		}
		// TODO: Check l and r
		return;
	case AMP:
	case PIPE:
	case CARET:
	case LT_LT:
	case GT_GT:
		if (!is_unsigned_type(type)) {
			compat_error(expr);
		}
		check_expr_with_type(type, l);
		check_expr_with_type(type, r);
		return;
	case AMP_AMP:
	case PIPE_PIPE:
	case CARET_CARET:
		if (type->kind != BOOL_TYPE) {
			compat_error(expr);
		}
		check_expr_with_type(type, l);
		check_expr_with_type(type, r);
		return;
	case EQ:
		if (!is_lvalue(l)) {
			lvalue_error(expr);
		}
		check_expr_with_type(type, l);
		check_expr_with_type(type, r);
		return;
	case PLUS_EQ:
	case MINUS_EQ:
	case STAR_EQ:
	case SLASH_EQ:
	case PERCENT_EQ:
		if (!is_num_type(type)) {
			lvalue_error(expr);
		}
		check_expr_with_type(type, l);
		check_expr_with_type(type, r);
		return;
	case AMP_EQ:
	case PIPE_EQ:
	case CARET_EQ:
	case LT_LT_EQ:
	case GT_GT_EQ:
		if (!is_unsigned_type(type)) {
			lvalue_error(expr);
		}
		check_expr_with_type(type, l);
		check_expr_with_type(type, r);
		return;
	case DOT:
		return; // TODO: Dot operator
	default:
		internal_error();
	}
}

static void check_lambda_expr_with_type(struct type *type, struct expr *expr)
{
	if (type->kind != FUNC_TYPE) {
		compat_error(expr);
	}
	// TODO: Lambda expr checking
}

static void check_array_lit_expr_with_type(struct type *type, struct expr *expr)
{
	struct type *subtype;
	struct expr *len;
	Vec *items;
	size_t i;

	if (type->kind != ARRAY_TYPE) {
		compat_error(expr);
	}
	subtype = type->u.array.l;
	len = type->u.array.len;
	items = expr->u.array_lit.val;
	if (eval_const_expr(len) != vec_len(items)) {
		compat_error(expr);
	}
	for (i = 0; i < vec_len(items); i++) {
		check_expr_with_type(subtype, vec_get(items, i));
	}
}

static void check_expr_with_type(struct type *type, struct expr *expr)
{
	switch (expr->kind) {
	case BOOL_LIT_EXPR:
		if (type->kind != BOOL_TYPE) {
			compat_error(expr);
		}
		return;
	case CHAR_LIT_EXPR:
		if (type->kind != CHAR_TYPE) {
			compat_error(expr);
		}
		return;
	case STRING_LIT_EXPR: // TODO: Length agreement
		if (type->kind != ARRAY_TYPE) {
			compat_error(expr);
		}
		return;
	case UNARY_OP_EXPR:
		check_unary_op_expr_with_type(type, expr);
		return;
	case BIN_OP_EXPR:
		check_bin_op_expr_with_type(type, expr);
		return;
	case LAMBDA_EXPR:
		check_lambda_expr_with_type(type, expr);
		return;
	case ARRAY_LIT_EXPR:
		check_array_lit_expr_with_type(type, expr);
		return;
	case IDENT_EXPR:
	case BLOCK_EXPR:
	case IF_EXPR:
	case SWITCH_EXPR:
	case TUPLE_EXPR:
		break;
	}
}

static void check_decl(struct decl *decl)
// TODO: Add a maximum nest level
{
	if (lookup_symbol(sym_tbl, decl->name) != NULL) {
		fatal_error(decl->lineno, "Name `%s` already declared in scope",
				decl->name);
	}
	if (is_global_scope(sym_tbl) && !is_pure_expr(decl->val)) {
		fatal_error(decl->lineno, "Top level declaration of `%s` is "
		                          "assigned to an impure expression",
					  decl->name);
	}
	check_expr_with_type(decl->type, decl->val);
	add_symbol(sym_tbl, decl->name, decl->type);
}

void check_ast(struct ast ast)
// TODO: Scan all top level decls first to remove the need for prototypes
{
	Vec *decls = ast.decls;
	size_t i;

	sym_tbl = alloc_symbol_table();
	enter_new_scope(sym_tbl); // Global scope
	for (i = 0; i < vec_len(decls); i++) {
		check_decl(vec_get(decls, i));
	}
	free_symbol_table(sym_tbl);
}
