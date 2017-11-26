#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "ds.h"
#include "quoftc.h"
#include "ast.h"
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

static bool is_pure_expr(struct expr *);

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

	stmts = expr->u.block.stmts;
	(void) stmts;
	return false; // TODO
}

static bool is_pure_expr(struct expr *expr)
{
	switch (expr->kind) {
	case BOOL_LIT_EXPR:
	case INT_LIT_EXPR:
	case FLOAT_LIT_EXPR:
	case CHAR_LIT_EXPR:
	case STRING_LIT_EXPR:
	case LAMBDA_EXPR:
	case IDENT_EXPR:
		return true;
	case UNARY_OP_EXPR:
		return is_pure_expr(expr->u.unary_op.operand);
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
	}
	internal_error();
}

static bool is_lvalue_unary_op(struct expr *expr)
{
	enum unary_op op = expr->u.unary_op.op;

	switch (op) {
	case DEREF_OP:
		return true; // TODO: Check for mutability
	default:
		return false;
	}
}

static bool is_lvalue_bin_op(struct expr *expr)
{
	enum bin_op op = expr->u.bin_op.op;

	switch (op) {
	case FIELD_OP:
		return true; // TODO: Check for mutability
	default:
		return false;
	}
}

static bool is_lvalue(struct expr *expr)
{
	switch (expr->kind) {
	case BOOL_LIT_EXPR:
	case INT_LIT_EXPR:
	case FLOAT_LIT_EXPR:
	case CHAR_LIT_EXPR:
	case STRING_LIT_EXPR:
	case ARRAY_LIT_EXPR:
	case LAMBDA_EXPR:
		return false;
	case UNARY_OP_EXPR:
		return is_lvalue_unary_op(expr);
	case BIN_OP_EXPR:
		return is_lvalue_bin_op(expr);
	case IDENT_EXPR:
		return true; // TODO: Check for mutability
	case BLOCK_EXPR:
		// TODO: Stub
	case IF_EXPR:
	case SWITCH_EXPR:
	case TUPLE_EXPR:
		return false;
	}
	internal_error();
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

static void type_check_int_lit(struct type *type, struct expr *expr)
{
	uint64_t val = expr->u.int_lit.val;

	switch (type->kind) {
	case U8_TYPE:
		if (val > UINT8_MAX) {
			compat_error(expr);
		}
		expr->type = ALLOC_U8_TYPE(expr->lineno);
		return;
	case U16_TYPE:
		if (val > UINT16_MAX) {
			compat_error(expr);
		}
		expr->type = ALLOC_U16_TYPE(expr->lineno);
		return;
	case U32_TYPE:
		if (val > UINT32_MAX) {
			compat_error(expr);
		}
		expr->type = ALLOC_U32_TYPE(expr->lineno);
		return;
	case U64_TYPE:
		expr->type = ALLOC_U64_TYPE(expr->lineno);
		return;
	case I8_TYPE:
		if (val > INT8_MAX) {
			compat_error(expr);
		}
		expr->type = ALLOC_I8_TYPE(expr->lineno);
		return;
	case I16_TYPE:
		if (val > INT16_MAX) {
			compat_error(expr);
		}
		expr->type = ALLOC_I16_TYPE(expr->lineno);
		return;
	case I32_TYPE:
		if (val > INT32_MAX) {
			compat_error(expr);
		}
		expr->type = ALLOC_I32_TYPE(expr->lineno);
		return;
	case I64_TYPE:
		if (val > INT64_MAX) {
			compat_error(expr);
		}
		expr->type = ALLOC_I64_TYPE(expr->lineno);
		return;
	default:
		compat_error(expr);
	}
}

static void type_check_string_lit(struct type *type, struct expr *expr)
{
	struct type *array_item_type;
	uint64_t type_len;
	uint64_t str_len = expr->u.string_lit.len;

	if (type->kind != ARRAY_TYPE) {
		compat_error(expr);
	}
	array_item_type = type->u.array.l;
	type_len = type->u.array.len; // Zero if unspecified
	if (array_item_type->kind != CHAR_TYPE) {
		compat_error(expr);
	}
	if (type_len != 0 && type_len != str_len) {
		compat_error(expr);
	}
	expr->type = ALLOC_ARRAY_TYPE(expr->lineno,
			ALLOC_CHAR_TYPE(expr->lineno), str_len);
}

static void type_check(struct type *, struct expr *);

static void type_check_unary_op(struct type *type, struct expr *expr)
{
	enum unary_op op = expr->u.unary_op.op;
	struct expr *operand = expr->u.unary_op.operand;

	switch (op) {
	case INC_OP:
	case DEC_OP:
		if (!is_num_type(type)) {
			compat_error(expr);
		}
		if (!is_lvalue(operand)) {
			lvalue_error(expr);
		}
		type_check(type, operand);
		break;
	case DEREF_OP: {
		struct type *operand_type;

		operand_type = ALLOC_POINTER_TYPE(type->lineno, type);
		type_check(operand_type, operand);
		free(operand_type);
		break;
	}
	case REF_OP: {
		struct type *operand_type;

		if (type->kind != POINTER_TYPE) {
			compat_error(expr);
		}
		operand_type = type->u.pointer.l;
		if (!is_lvalue(operand)) {
			lvalue_error(expr);
		}
		type_check(operand_type, operand);
		break;
	}
	case BIT_NOT_OP:
		if (!is_unsigned_type(type)) {
			compat_error(expr);
		}
		type_check(type, operand);
		break;
	case LOG_NOT_OP:
		if (type->kind != BOOL_TYPE) {
			compat_error(expr);
		}
		type_check(type, operand);
		break;
	}
	expr->type = dup_type(type);
}

static void type_check_bin_op(struct type *type, struct expr *expr)
{
	enum bin_op op = expr->u.bin_op.op;
	struct expr *l = expr->u.bin_op.l,
	            *r = expr->u.bin_op.r;

	switch (op) {
	case ADD_OP:
	case SUB_OP:
	case MULT_OP:
	case DIV_OP:
	case MOD_OP:
		if (!is_num_type(type)) {
			compat_error(expr);
		}
		type_check(type, l);
		type_check(type, r);
		break;
	case LT_OP:
	case GT_OP:
	case LT_EQ_OP:
	case GT_EQ_OP:
		if (type->kind != BOOL_TYPE) {
			compat_error(expr);
		}
		// TODO: Num type checking
		break;
	case LOG_EQ_OP:
	case NOT_EQ_OP:
		if (type->kind != BOOL_TYPE) {
			compat_error(expr);
		}
		// TODO: Check l and r
		break;
	case BIT_AND_OP:
	case BIT_OR_OP:
	case BIT_XOR_OP:
	case BIT_SHIFT_L_OP:
	case BIT_SHIFT_R_OP:
		if (!is_unsigned_type(type)) {
			compat_error(expr);
		}
		type_check(type, l);
		type_check(type, r);
		break;
	case LOG_AND_OP:
	case LOG_OR_OP:
	case LOG_XOR_OP:
		if (type->kind != BOOL_TYPE) {
			compat_error(expr);
		}
		type_check(type, l);
		type_check(type, r);
		break;
	case ASSIGN_OP:
	case ADD_ASSIGN_OP:
	case SUB_ASSIGN_OP:
	case MULT_ASSIGN_OP:
	case DIV_ASSIGN_OP:
	case MOD_ASSIGN_OP:
	case BIT_AND_ASSIGN_OP:
	case BIT_OR_ASSIGN_OP:
	case BIT_XOR_ASSIGN_OP:
	case BIT_SHIFT_L_ASSIGN_OP:
	case BIT_SHIFT_R_ASSIGN_OP:
		if (type->kind != VOID_TYPE) {
			compat_error(expr);
		}
		if (!is_lvalue(l)) {
			lvalue_error(expr);
		}
		// TODO: Check l and r
		break;
	case FIELD_OP:
		break; // TODO: Dot operator
	}
	expr->type = dup_type(type);
}

static void type_check_lambda(struct type *type, struct expr *expr)
{
	size_t nparams, i;
	Vec *param_types;
	Vec *param_names;

	if (type->kind != FUNC_TYPE) {
		compat_error(expr);
	}
	param_types = type->u.func.params;
	param_names = expr->u.lambda.params;
	if (vec_len(param_types) != vec_len(param_names)) {
		compat_error(expr);
	}
	nparams = vec_len(param_types);
	enter_new_scope(sym_tbl);
	for (i = 0; i < nparams; i++) {
		insert_symbol(sym_tbl, vec_get(param_names, i),
				vec_get(param_types, i));
	}
	// TODO: Finish checking
	leave_scope(sym_tbl);
	expr->type = dup_type(type);
}

static void type_check_array_lit(struct type *type, struct expr *expr)
{
	struct type *subtype;
	uint64_t type_len;
	Vec *items;
	size_t i;

	if (type->kind != ARRAY_TYPE) {
		compat_error(expr);
	}
	subtype = type->u.array.l;
	type_len = type->u.array.len;
	items = expr->u.array_lit.val;
	if (type_len != vec_len(items)) {
		compat_error(expr);
	}
	for (i = 0; i < vec_len(items); i++) {
		type_check(subtype, vec_get(items, i));
	}
	// TODO: Finish checking
}

static void type_check(struct type *type, struct expr *expr)
{
	switch (expr->kind) {
	case BOOL_LIT_EXPR:
		if (type->kind != BOOL_TYPE) {
			compat_error(expr);
		}
		expr->type = dup_type(type);
		break;
	case INT_LIT_EXPR:
		type_check_int_lit(type, expr);
		break;
	case FLOAT_LIT_EXPR:
		// TODO: Fix this
		if (type->kind != F64_TYPE) {
			compat_error(expr);
		}
		expr->type = dup_type(type);
		break;
	case CHAR_LIT_EXPR:
		if (type->kind != CHAR_TYPE) {
			compat_error(expr);
		}
		expr->type = dup_type(type);
		break;
	case STRING_LIT_EXPR:
		type_check_string_lit(type, expr);
		break;
	case UNARY_OP_EXPR:
		type_check_unary_op(type, expr);
		break;
	case BIN_OP_EXPR:
		type_check_bin_op(type, expr);
		break;
	case LAMBDA_EXPR:
		type_check_lambda(type, expr);
		break;
	case ARRAY_LIT_EXPR:
		type_check_array_lit(type, expr);
		break;
	case IDENT_EXPR:
	case BLOCK_EXPR:
	case IF_EXPR:
	case SWITCH_EXPR:
	case TUPLE_EXPR:
		break; // TODO: Stub
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
	type_check(decl->type, decl->val);
	insert_symbol(sym_tbl, decl->name, decl->type);
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
