/*
 * This file verifies the correctness of the parsed AST and adds type
 * information to each expr node.
 */

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "ds.h"
#include "quoftc.h"
#include "ast.h"
#include "symbol_table.h"
#include "check_semantics.h"

struct symbol_info {
	enum { VALUE_SYM, TYPE_SYM } kind;
	union {
		struct {
			bool is_let;
			struct type *type;
		} value;
		struct type *type;
	} u;
};

static struct symbol_table sym_tbl;
static struct type *cur_func_type;

static struct symbol_info *alloc_val_sym_info(bool is_let, struct type *type)
{
	struct symbol_info *sym_info;

	sym_info = NEW(struct symbol_info);
	sym_info->kind = VALUE_SYM;
	sym_info->u.value.is_let = is_let;
	sym_info->u.value.type = type;
	return sym_info;
}

#if 0
static struct symbol_info *alloc_type_sym_info(struct type *type)
{
	struct symbol_info *sym_info;

	sym_info = NEW(struct symbol_info);
	sym_info->kind = TYPE_SYM;
	sym_info->u.type = type;
	return sym_info;
}
#endif

static NORETURN void compat_error(unsigned lineno)
{
	fatal_error(lineno, "Expression not compatible with type");
}

static NORETURN void lvalue_error(unsigned lineno)
{
	fatal_error(lineno, "Value mutated that is not an lvalue");
}

static bool is_pure_expr(struct expr *);

static bool is_pure_unary_op_expr(struct expr *expr)
{
	struct expr *operand;
	enum unary_op op;

	assert(expr->kind == UNARY_OP_EXPR);
	operand = expr->u.unary_op.operand;
	op = expr->u.unary_op.op;
	switch (op) {
	case NEG_OP:
	case DEREF_OP:
	case REF_OP:
	case BIT_NOT_OP:
	case LOG_NOT_OP:
		return is_pure_expr(operand);
	case PRE_INC_OP:
	case POST_INC_OP:
	case PRE_DEC_OP:
	case POST_DEC_OP:
		return false;
	}
	internal_error();
}

static bool is_pure_bin_op_expr(struct expr *expr)
{
	struct expr *l, *r;
	enum bin_op op;

	assert(expr->kind == BIN_OP_EXPR);
	l = expr->u.bin_op.l;
	r = expr->u.bin_op.r;
	op = expr->u.bin_op.op;
	switch (op) {
	case ADD_OP:
	case SUB_OP:
	case MUL_OP:
	case DIV_OP:
	case MOD_OP:
	case LT_OP:
	case GT_OP:
	case LT_EQ_OP:
	case GT_EQ_OP:
	case EQ_OP:
	case NOT_EQ_OP:
	case BIT_AND_OP:
	case BIT_OR_OP:
	case BIT_XOR_OP:
	case BIT_SHIFT_L_OP:
	case BIT_SHIFT_R_OP:
	case LOG_AND_OP:
	case LOG_OR_OP:
		return is_pure_expr(l) && is_pure_expr(r);
	case ASSIGN_OP:
	case ADD_ASSIGN_OP:
	case SUB_ASSIGN_OP:
	case MUL_ASSIGN_OP:
	case DIV_ASSIGN_OP:
	case MOD_ASSIGN_OP:
	case BIT_AND_ASSIGN_OP:
	case BIT_OR_ASSIGN_OP:
	case BIT_XOR_ASSIGN_OP:
	case BIT_SHIFT_L_ASSIGN_OP:
	case BIT_SHIFT_R_ASSIGN_OP:
		return false;
	}
	internal_error();
}

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
		return true;
	case IDENT_EXPR:
		return false;
	case UNARY_OP_EXPR:
		return is_pure_unary_op_expr(expr);
	case BIN_OP_EXPR:
		return is_pure_bin_op_expr(expr);
	case ARRAY_LIT_EXPR:
		return vec_has_pure_items(expr->u.array_lit.val);
	case BLOCK_EXPR:
		return is_pure_block_expr(expr);
	case IF_EXPR:
		return is_pure_expr(expr->u.if_.cond)
			&& is_pure_expr(expr->u.if_.then)
			&& is_pure_expr(expr->u.if_.else_);
	case SWITCH_EXPR:
		return false; // TODO
	case TUPLE_EXPR:
		return vec_has_pure_items(expr->u.tuple.items);
	case FUNC_CALL_EXPR:
		return false; // TODO
	case FIELD_ACCESS_EXPR:
		internal_error(); // TODO: Stub
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
		return false;
	case IDENT_EXPR: {
		struct symbol_info *sym_info;
		char *name;

		name = expr->u.ident.name;
		sym_info = lookup_symbol(sym_tbl, name);
		assert(sym_info != NULL);
		assert(sym_info->kind == VALUE_SYM);
		return !sym_info->u.value.is_let;
	}
	case BLOCK_EXPR:
		// TODO: Stub
	case IF_EXPR:
	case SWITCH_EXPR:
	case TUPLE_EXPR:
	case FUNC_CALL_EXPR:
	case FIELD_ACCESS_EXPR:
		return false;
	}
	internal_error();
}

bool is_unsigned_int_type(struct type *type)
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

bool is_signed_int_type(struct type *type)
{
	switch (type->kind) {
	case I8_TYPE:
	case I16_TYPE:
	case I32_TYPE:
	case I64_TYPE:
		return true;
	default:
		return false;
	}
}

static bool is_int_type(struct type *type)
{
	return type->kind == UNSIZED_INT_TYPE || is_unsigned_int_type(type) ||
		is_signed_int_type(type);
}

bool is_float_type(struct type *type)
{
	switch (type->kind) {
	case F32_TYPE:
	case F64_TYPE:
		return true;
	default:
		return false;
	}
}

static bool is_num_type(struct type *type)
{
	return is_int_type(type) || is_float_type(type);
}

static void type_check(struct expr *);

static void type_check_unary_op(struct expr *expr)
{
	enum unary_op op = expr->u.unary_op.op;
	struct expr *operand = expr->u.unary_op.operand;

	type_check(operand);
	switch (op) {
	case NEG_OP:
		if (!is_num_type(operand->type)) {
			compat_error(expr->lineno);
		}
		if (is_unsigned_int_type(operand->type)) {
			compat_error(expr->lineno);
		}
		expr->type = dup_type(operand->type);
		break;
	case PRE_INC_OP:
	case POST_INC_OP:
	case PRE_DEC_OP:
	case POST_DEC_OP:
		if (!is_lvalue(operand)) {
			lvalue_error(expr->lineno);
		}
		if (!is_num_type(operand->type)) {
			compat_error(expr->lineno);
		}
		expr->type = dup_type(operand->type);
		break;
	case DEREF_OP: {
		type_check(operand);
		if (operand->type->kind != POINTER_TYPE) {
			compat_error(expr->lineno);
		}
		expr->type = dup_type(operand->type->u.pointer.l);
		break;
	}
	case REF_OP: {
		if (!is_lvalue(operand)) {
			lvalue_error(expr->lineno);
		}
		expr->type = ALLOC_POINTER_TYPE(expr->lineno, operand->type);
		break;
	}
	case BIT_NOT_OP:
		if (!is_unsigned_int_type(operand->type)) {
			compat_error(expr->lineno);
		}
		expr->type = dup_type(operand->type);
		break;
	case LOG_NOT_OP:
		if (operand->type->kind != BOOL_TYPE) {
			compat_error(expr->lineno);
		}
		expr->type = ALLOC_BOOL_TYPE(expr->lineno);
		break;
	}
}

static struct type *remove_const_and_volatile(struct type *type)
{
	switch (type->kind) {
	case CONST_TYPE:
		return remove_const_and_volatile(type->u.const_.type);
	case VOLATILE_TYPE:
		return remove_const_and_volatile(type->u.volatile_.type);
	default:
		return type;
	}
}

static bool are_types_compat(struct type *, struct type *);

static bool vecs_have_compat_types(Vec *types1, Vec *types2)
{
	size_t len, i;

	if (vec_len(types1) != vec_len(types2)) {
		return false;
	}
	len = vec_len(types1);
	for (i = 0; i < len; i++) {
		if (!are_types_compat(vec_get(types1, i), vec_get(types2, i))) {
			return false;
		}
	}
	return true;
}

static bool are_types_compat(struct type *type1, struct type *type2)
{
	// TODO: Const and volatile checking
	type1 = remove_const_and_volatile(type1);
	type2 = remove_const_and_volatile(type2);
	switch (type1->kind) {
	case UNSIZED_INT_TYPE:
		return is_int_type(type2);
	case U8_TYPE:
	case U16_TYPE:
	case U32_TYPE:
	case U64_TYPE:
	case I8_TYPE:
	case I16_TYPE:
	case I32_TYPE:
	case I64_TYPE:
		return type1->kind == type2->kind ||
			type2->kind == UNSIZED_INT_TYPE;
	case F32_TYPE:
	case F64_TYPE:
	case BOOL_TYPE:
	case VOID_TYPE:
	case CHAR_TYPE:
		return type1->kind == type2->kind;
	case ALIAS_TYPE: {
		const char *name1, *name2;

		if (type2->kind != ALIAS_TYPE) {
			return false;
		}
		name1 = type1->u.alias.name;
		name2 = type2->u.alias.name;
		return strcmp(name1, name2) == 0;
	}
	case PARAM_TYPE: {
		Vec *params1, *params2;
		const char *name1, *name2;

		if (type2->kind != PARAM_TYPE) {
			return false;
		}
		name1 = type1->u.param.name;
		name2 = type2->u.param.name;
		params1 = type1->u.param.params;
		params2 = type2->u.param.params;
		return strcmp(name1, name2) == 0 &&
			vecs_have_compat_types(params1, params2);
	}
	case ARRAY_TYPE: {
		struct type *subtype1, *subtype2;
		unsigned len1, len2;

		if (type2->kind != ARRAY_TYPE) {
			return false;
		}
		subtype1 = type1->u.array.l;
		subtype2 = type2->u.array.l;
		len1 = type1->u.array.len;
		len2 = type2->u.array.len;
		return are_types_compat(subtype1, subtype2) &&
			(EITHER_EQ(len1, len2, 0) || len1 == len2);
	}
	case POINTER_TYPE: {
		struct type *subtype1, *subtype2;

		if (type2->kind != POINTER_TYPE) {
			return false;
		}
		subtype1 = type1->u.pointer.l;
		subtype2 = type2->u.pointer.l;
		return are_types_compat(subtype1, subtype2);
	}
	case TUPLE_TYPE: {
		Vec *types1, *types2;

		if (type2->kind != TUPLE_TYPE) {
			return false;
		}
		types1 = type1->u.tuple.types;
		types2 = type2->u.tuple.types;
		return vecs_have_compat_types(types1, types2);
	}
	case STRUCT_TYPE:
		internal_error(); // TODO: Stub
	case FUNC_TYPE: {
		struct type *ret1, *ret2;
		Vec *params1, *params2;

		if (type2->kind != FUNC_TYPE) {
			return false;
		}
		ret1 = type1->u.func.ret;
		ret2 = type2->u.func.ret;
		params1 = type1->u.func.params;
		params2 = type2->u.func.params;
		return are_types_compat(ret1, ret2) &&
			vecs_have_compat_types(params1, params2);
	}
	case CONST_TYPE:
	case VOLATILE_TYPE:
		// NOTREACHED
		internal_error();
	}
	internal_error();
}

static struct type *dup_stricter_type(struct type *type1, struct type *type2)
{
	if (!are_types_compat(type1, type2)) {
		compat_error(type1->lineno);
	}

	switch (type1->kind) {
	case UNSIZED_INT_TYPE:
		return dup_type(type2);
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
	case BOOL_TYPE:
	case VOID_TYPE:
	case CHAR_TYPE:
		return dup_type(type1);
	case ALIAS_TYPE:
	case PARAM_TYPE:
		internal_error(); // TODO: Stub
	case ARRAY_TYPE: {
		struct type *subtype1, *subtype2, *stricter_subtype;
		unsigned len;

		subtype1 = type1->u.array.l;
		subtype2 = type2->u.array.l;
		stricter_subtype = dup_stricter_type(subtype1, subtype2);
		if (type1->u.array.len == 0) {
			len = type2->u.array.len;
		} else {
			len = type1->u.array.len;
		}
		return ALLOC_ARRAY_TYPE(type1->lineno, stricter_subtype, len);
	}
	case POINTER_TYPE: {
		struct type *subtype1, *subtype2, *stricter_subtype;

		subtype1 = type1->u.pointer.l;
		subtype2 = type2->u.pointer.l;
		stricter_subtype = dup_stricter_type(subtype1, subtype2);
		return ALLOC_POINTER_TYPE(type1->lineno, stricter_subtype);
	}
	case TUPLE_TYPE: {
		Vec *types1, *types2, *strictest_types;
		size_t i;
		struct type *elem1, *elem2, *stricter_elem;

		types1 = type1->u.tuple.types;
		types2 = type2->u.tuple.types;
		strictest_types = alloc_vec(free_type);
		for (i = 0; i < vec_len(types1); i++) {
			elem1 = vec_get(types1, i);
			elem2 = vec_get(types2, i);
			stricter_elem = dup_stricter_type(elem1, elem2);
			vec_push(strictest_types, stricter_elem);
		}
		return ALLOC_TUPLE_TYPE(type1->lineno, strictest_types);
	}
	case STRUCT_TYPE:
		internal_error(); // TODO: Stub
	case FUNC_TYPE:
		internal_error(); // TODO: Stub
	case CONST_TYPE: {
		struct type *subtype1, *subtype2;

		subtype1 = type1->u.const_.type;
		if (type2->kind == CONST_TYPE) {
			subtype2 = type2->u.const_.type;
			return ALLOC_CONST_TYPE(type1->lineno,
					dup_stricter_type(subtype1, subtype2));
		} else {
			return ALLOC_CONST_TYPE(type1->lineno,
					dup_stricter_type(subtype1, type2));
		}
	}
	case VOLATILE_TYPE: {
		struct type *subtype1, *subtype2;

		subtype1 = type1->u.volatile_.type;
		if (type2->kind == VOLATILE_TYPE) {
			subtype2 = type2->u.volatile_.type;
			return ALLOC_VOLATILE_TYPE(type1->lineno,
					dup_stricter_type(subtype1, subtype2));
		} else {
			return ALLOC_VOLATILE_TYPE(type1->lineno,
					dup_stricter_type(subtype1, type2));
		}
	}
	}
	internal_error();
}

static bool type_is_convertible(struct type *, struct type *);

static bool types_are_convertible(Vec *from_types, Vec *to_types)
{
	struct type *from_type, *to_type;
	size_t len, i;

	if (vec_len(from_types) != vec_len(to_types)) {
		return false;
	}
	len = vec_len(from_types);
	for (i = 0; i < len; i++) {
		from_type = vec_get(from_types, i);
		to_type = vec_get(to_types, i);
		if (!type_is_convertible(from_type, to_type)) {
			return false;
		}
	}
	return true;
}

static bool type_is_convertible(struct type *from_type, struct type *to_type)
{
	switch (to_type->kind) {
	case UNSIZED_INT_TYPE:
		// Only `from_type` should ever be an `UNSIZED_INT_TYPE`
		internal_error();
	case U8_TYPE:
	case U16_TYPE:
	case U32_TYPE:
	case U64_TYPE:
	case I8_TYPE:
	case I16_TYPE:
	case I32_TYPE:
	case I64_TYPE:
		return from_type->kind == UNSIZED_INT_TYPE ||
				from_type->kind == to_type->kind;
	case F32_TYPE:
	case F64_TYPE:
	case BOOL_TYPE:
	case VOID_TYPE:
	case CHAR_TYPE:
		return from_type->kind == to_type->kind;
	case ALIAS_TYPE:
	case PARAM_TYPE:
		internal_error(); // TODO: Stub
	case ARRAY_TYPE: {
		struct type *from_subtype, *to_subtype;
		unsigned from_len, to_len;

		if (from_type->kind != ARRAY_TYPE) {
			return false;
		}
		from_subtype = from_type->u.array.l;
		to_subtype = to_type->u.array.l;
		from_len = from_type->u.array.len;
		to_len = to_type->u.array.len;
		return type_is_convertible(from_subtype, to_subtype) &&
			(to_len == 0 || from_len == to_len);
	}
	case POINTER_TYPE: {
		struct type *from_subtype, *to_subtype;

		if (from_type->kind != POINTER_TYPE) {
			return false;
		}
		from_subtype = from_type->u.pointer.l;
		to_subtype = to_type->u.pointer.l;
		return type_is_convertible(from_subtype, to_subtype);
	}
	case TUPLE_TYPE: {
		Vec *from_types, *to_types;

		if (from_type->kind != TUPLE_TYPE) {
			return false;
		}
		from_types = from_type->u.tuple.types;
		to_types = to_type->u.tuple.types;
		return types_are_convertible(from_types, to_types);
	}
	case STRUCT_TYPE:
		internal_error(); // TODO: Stub
	// TODO: Make sure this isn't problematic
	case FUNC_TYPE: {
		struct type *from_return_type, *to_return_type;
		Vec *from_param_types, *to_param_types;

		if (from_type->kind != FUNC_TYPE) {
			return false;
		}
		from_return_type = from_type->u.func.ret;
		to_return_type = to_type->u.func.ret;
		from_param_types = from_type->u.func.params;
		to_param_types = to_type->u.func.params;
		return type_is_convertible(from_return_type, to_return_type) &&
			types_are_convertible(from_param_types, to_param_types);
	}
	case CONST_TYPE: {
		struct type *from_subtype, *to_subtype;

		to_subtype = to_type->u.const_.type;
		if (from_type->kind == CONST_TYPE) {
			from_subtype = from_type->u.const_.type;
			return type_is_convertible(from_subtype, to_subtype);
		} else {
			return type_is_convertible(from_type, to_subtype);
		}
	}
	case VOLATILE_TYPE: {
		struct type *from_subtype, *to_subtype;

		to_subtype = to_type->u.volatile_.type;
		if (from_type->kind == VOLATILE_TYPE) {
			from_subtype = from_type->u.volatile_.type;
			return type_is_convertible(from_subtype, to_subtype);
		} else {
			return type_is_convertible(from_type, to_subtype);
		}
	}
	}
	internal_error();
}

static void ensure_bool_expr(struct expr *expr)
{
	if (expr->type->kind != BOOL_TYPE) {
		fatal_error(expr->lineno, "Expected a boolean expression");
	}
}

static void type_check_bin_op(struct expr *expr)
{
	enum bin_op op = expr->u.bin_op.op;
	struct expr *l = expr->u.bin_op.l,
	            *r = expr->u.bin_op.r;

	type_check(l);
	type_check(r);
	switch (op) {
	case ADD_OP:
	case SUB_OP:
	case MUL_OP:
	case DIV_OP:
	case MOD_OP:
		if (!is_num_type(l->type)) {
			compat_error(expr->lineno);
		}
		if (!are_types_compat(l->type, r->type)) {
			compat_error(expr->lineno);
		}
		expr->type = dup_stricter_type(l->type, r->type);
		break;
	case LT_OP:
	case GT_OP:
	case LT_EQ_OP:
	case GT_EQ_OP:
		if (!is_num_type(l->type)) {
			compat_error(expr->lineno);
		}
		if (!are_types_compat(l->type, r->type)) {
			compat_error(expr->lineno);
		}
		expr->type = ALLOC_BOOL_TYPE(expr->lineno);
		break;
	case EQ_OP:
	case NOT_EQ_OP:
		if (!are_types_compat(l->type, r->type)) {
			compat_error(expr->lineno);
		}
		expr->type = ALLOC_BOOL_TYPE(expr->lineno);
		break;
	case BIT_AND_OP:
	case BIT_OR_OP:
	case BIT_XOR_OP:
	case BIT_SHIFT_L_OP:
	case BIT_SHIFT_R_OP:
		if (!is_unsigned_int_type(l->type)) {
			compat_error(expr->lineno);
		}
		expr->type = dup_stricter_type(l->type, r->type);
		break;
	case LOG_AND_OP:
	case LOG_OR_OP:
		if (l->type->kind != BOOL_TYPE) {
			compat_error(expr->lineno);
		}
		if (!are_types_compat(l->type, r->type)) {
			compat_error(expr->lineno);
		}
		expr->type = ALLOC_BOOL_TYPE(expr->lineno);
		break;
	case ADD_ASSIGN_OP:
	case SUB_ASSIGN_OP:
	case MUL_ASSIGN_OP:
	case DIV_ASSIGN_OP:
	case MOD_ASSIGN_OP:
		if (!is_num_type(l->type)) {
			compat_error(expr->lineno);
		}
		goto assign_op_common;
	case BIT_AND_ASSIGN_OP:
	case BIT_OR_ASSIGN_OP:
	case BIT_XOR_ASSIGN_OP:
	case BIT_SHIFT_L_ASSIGN_OP:
	case BIT_SHIFT_R_ASSIGN_OP:
		if (!is_unsigned_int_type(l->type)) {
			compat_error(expr->lineno);
		}
		goto assign_op_common;
	case ASSIGN_OP:
	assign_op_common:
		if (!is_lvalue(l)) {
			lvalue_error(expr->lineno);
		}
		if (!are_types_compat(l->type, r->type)) {
			compat_error(expr->lineno);
		}
		expr->type = ALLOC_VOID_TYPE(expr->lineno);
		break;
	}
}

static void type_check_lambda(struct expr *expr)
{
	(void) expr;
#if 0
	// TODO: This is broken
	Vec *params;
	size_t i;

	params = expr->u.lambda.params;
	enter_new_scope(sym_tbl);
	for (i = 0; i < vec_len(param_names); i++) {
		insert_symbol(val_sym_tbl, vec_get(param_names, i),
				vec_get(param_types, i));
	}
	// TODO: Finish checking
	leave_scope(sym_tbl);
	expr->type = dup_type(type);
#endif
}

static void type_check_exprs(Vec *exprs)
{
	size_t i;

	for (i = 0; i < vec_len(exprs); i++) {
		type_check(vec_get(exprs, i));
	}
}

static void type_check_array_lit(struct expr *expr)
{
	Vec *items;
	struct expr *item, *first_item;
	struct type *strictest_type, *tmp;
	size_t len, i;

	items = expr->u.array_lit.val;
	type_check_exprs(items);
	len = vec_len(items);
	first_item = vec_get(items, 0);
	strictest_type = dup_type(first_item->type);
	for (i = 1; i < len; i++) {
		item = vec_get(items, i);
		tmp = strictest_type;
		strictest_type = dup_stricter_type(strictest_type, item->type);
		free(tmp);
	}
	expr->type = ALLOC_ARRAY_TYPE(expr->lineno, strictest_type, len);
}

static void type_check_ident(struct expr *expr)
{
	struct symbol_info *sym_info;
	char *name;

	assert(expr->kind == IDENT_EXPR);
	name = expr->u.ident.name;
	sym_info = lookup_symbol(sym_tbl, name);
	if (sym_info == NULL) {
		fatal_error(expr->lineno, "Name `%s` does not exist in scope; "
		                          "did you spell it wrong?", name);
	}
	if (sym_info->kind != VALUE_SYM) {
		fatal_error(expr->lineno, "Name `%s` is the name of a type, "
		                          "not a value", name);
	}
	expr->type = dup_type(sym_info->u.value.type);
}

static void check_compound_stmt(Vec *);

static void type_check_block(struct expr *expr)
{
	Vec *stmts;

	assert(expr->kind == BLOCK_EXPR);
	stmts = expr->u.block.stmts;

	enter_new_scope(sym_tbl);
	check_compound_stmt(stmts);
	leave_scope(sym_tbl);
	expr->type = ALLOC_VOID_TYPE(expr->lineno);
}

static void type_check_if(struct expr *expr)
{
	struct expr *cond, *then, *else_;

	assert(expr->kind == IF_EXPR);
	cond = expr->u.if_.cond;
	then = expr->u.if_.then;
	else_ = expr->u.if_.else_;

	type_check(cond);
	ensure_bool_expr(cond);
	type_check(then);
	type_check(else_);
	if (!are_types_compat(then->type, else_->type)) {
		fatal_error(then->lineno, "Types of `then` and `else` "
		                          "expressions are not compatible");
	}
	expr->type = dup_stricter_type(then->type, else_->type);
}

static void type_check_tuple(struct expr *expr)
{
	struct expr *item;
	Vec *items, *types;
	size_t i;

	assert(expr->kind == TUPLE_EXPR);
	items = expr->u.tuple.items;

	types = alloc_vec(free_type);
	for (i = 0; i < vec_len(items); i++) {
		item = vec_get(items, i);
		type_check(item);
		vec_push(types, item->type);
	}
	expr->type = ALLOC_TUPLE_TYPE(expr->lineno, types);
}

static void type_check_func_call(struct expr *expr)
{
	struct type *param_type, *return_type;
	struct expr *func, *arg;
	Vec *args, *param_types;
	size_t i;

	assert(expr->kind == FUNC_CALL_EXPR);
	func = expr->u.func_call.func;
	args = expr->u.func_call.args;
	type_check(func);
	type_check_exprs(args);
	if (func->type->kind != FUNC_TYPE) {
		fatal_error(expr->lineno,
				"Expression is called but is not a function");
	}
	param_types = func->type->u.func.params;
	for (i = 0; i < vec_len(args); i++) {
		arg = vec_get(args, i);
		param_type = vec_get(param_types, i);
		// TODO: are_types_compat() may be the wrong check (const)
		if (!are_types_compat(arg->type, param_type)) {
			fatal_error(arg->lineno, "Type of passed argument is "
			                         "an unexpected type");
		}
	}
	return_type = func->type->u.func.ret;
	expr->type = dup_type(return_type);
}

static void type_check_field_access_expr(struct expr *expr)
{
	(void) expr;
	internal_error(); // TODO: Stub
}

static void type_check(struct expr *expr)
{
	assert(expr->type == NULL);
	switch (expr->kind) {
	case BOOL_LIT_EXPR:
		expr->type = ALLOC_BOOL_TYPE(expr->lineno);
		break;
	case INT_LIT_EXPR:
		expr->type = ALLOC_UNSIZED_INT_TYPE(expr->lineno);
		break;
	case FLOAT_LIT_EXPR:
		// TODO: Fix this
		expr->type = ALLOC_F64_TYPE(expr->lineno);
		break;
	case CHAR_LIT_EXPR:
		expr->type = ALLOC_CHAR_TYPE(expr->lineno);
		break;
	case STRING_LIT_EXPR: {
		unsigned len = expr->u.string_lit.len;

		expr->type = ALLOC_ARRAY_TYPE(expr->lineno,
				ALLOC_CHAR_TYPE(expr->lineno), len);
		break;
	}
	case UNARY_OP_EXPR:
		type_check_unary_op(expr);
		break;
	case BIN_OP_EXPR:
		type_check_bin_op(expr);
		break;
	case LAMBDA_EXPR:
		type_check_lambda(expr);
		break;
	case ARRAY_LIT_EXPR:
		type_check_array_lit(expr);
		break;
	case IDENT_EXPR:
		type_check_ident(expr);
		break;
	case BLOCK_EXPR:
		type_check_block(expr);
		break;
	case IF_EXPR:
		type_check_if(expr);
		break;
	case SWITCH_EXPR:
		internal_error(); // TODO: Stub
	case TUPLE_EXPR:
		type_check_tuple(expr);
		break;
	case FUNC_CALL_EXPR:
		type_check_func_call(expr);
		break;
	case FIELD_ACCESS_EXPR:
		type_check_field_access_expr(expr);
		break;
	}
}

static void ensure_declarable_type(struct type *type)
{
	switch (type->kind) {
	case UNSIZED_INT_TYPE:
		internal_error(); // The parser shouldn't set this
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
	case BOOL_TYPE:
	case CHAR_TYPE:
		break;
	case VOID_TYPE:
		fatal_error(type->lineno, "Void is not a declarable type");
	case ALIAS_TYPE:
	case PARAM_TYPE:
		internal_error(); // TODO: Stub
	case ARRAY_TYPE:
		ensure_declarable_type(type->u.array.l);
		break;
	case POINTER_TYPE:
		ensure_declarable_type(type->u.pointer.l);
		break;
	case TUPLE_TYPE: {
		Vec *types;
		size_t i;

		types = type->u.tuple.types;
		for (i = 0; i < vec_len(types); i++) {
			ensure_declarable_type(vec_get(types, i));
		}
		break;
	}
	case STRUCT_TYPE:
	case FUNC_TYPE:
	case CONST_TYPE:
	case VOLATILE_TYPE:
		internal_error(); // TODO: Stub
	}
}

static void ensure_not_declared(char *name, unsigned lineno)
{
	if (lookup_symbol(sym_tbl, name) != NULL) {
		fatal_error(lineno, "Name `%s` already declared in scope",
				name);
	}
}

static void check_data_decl(struct decl *decl)
{
	unsigned lineno;
	bool is_let;
	struct type *type;
	char *name;
	struct expr *init;

	lineno = decl->lineno;
	is_let = decl->u.data.is_let;
	type = decl->u.data.type;
	name = decl->u.data.name;
	init = decl->u.data.init;

	ensure_declarable_type(type);
	ensure_not_declared(name, decl->lineno);
	if (is_global_scope(sym_tbl)) {
		if (init == NULL) {
			fatal_error(lineno, "Top level declaration of `%s` "
			                    "lacks an initializer", name);
		}
		if (!is_pure_expr(init)) {
			fatal_error(lineno, "Top level declaration of "
			                    "`%s` is assigned to an impure "
			                    "expression", name);
		}
	}
	if (is_let && init == NULL) {
		fatal_error(lineno, "Constant declaration of `%s` lacks an "
		                    "initializer", name);
	}
	if (init != NULL) {
		type_check(init);
		/*
		 * TODO: are_types_compat() is problematic here; type must
		 * always be stricter than init->type.
		 */
		if (!are_types_compat(type, init->type)) {
			compat_error(lineno);
		}
	}
	insert_symbol(sym_tbl, name, alloc_val_sym_info(is_let, type));
}

static void check_typedef_decl(struct decl *decl)
{
	struct type *type;
	Vec *params;
	char *name;
	size_t i;

	assert(decl->kind == TYPEDEF_DECL);
	name = decl->u.typedef_.name;
	params = decl->u.typedef_.params;
	type = decl->u.typedef_.type;
	ensure_not_declared(name, decl->lineno);
	for (i = 0; i < vec_len(params); i++) {
		// TODO: Handle type parameters
	}
	ensure_declarable_type(type);
	internal_error(); // TODO: Stub
}

static void check_if_stmt(struct stmt *stmt)
{
	struct expr *cond;
	Vec *then_stmts, *else_stmts;

	cond = stmt->u.if_.cond;
	then_stmts = stmt->u.if_.then_stmts;
	else_stmts = stmt->u.if_.else_stmts;

	type_check(cond);
	ensure_bool_expr(cond);
	enter_new_scope(sym_tbl);
	check_compound_stmt(then_stmts);
	leave_scope(sym_tbl);
	if (else_stmts != NULL) {
		enter_new_scope(sym_tbl);
		check_compound_stmt(else_stmts);
		leave_scope(sym_tbl);
	}
}

static void check_do_stmt(struct stmt *stmt)
{
	Vec *stmts;
	struct expr *cond;

	stmts = stmt->u.do_.stmts;
	cond = stmt->u.do_.cond;

	enter_new_scope(sym_tbl);
	check_compound_stmt(stmts);
	type_check(cond);
	ensure_bool_expr(cond);
	leave_scope(sym_tbl);
}

static void check_while_stmt(struct stmt *stmt)
{
	struct expr *cond;
	Vec *stmts;

	cond = stmt->u.while_.cond;
	stmts = stmt->u.while_.stmts;

	type_check(cond);
	ensure_bool_expr(cond);
	enter_new_scope(sym_tbl);
	check_compound_stmt(stmts);
	leave_scope(sym_tbl);
}

static void check_for_stmt(struct stmt *stmt)
{
	struct expr *init, *cond, *post;
	Vec *stmts;

	init = stmt->u.for_.init;
	cond = stmt->u.for_.cond;
	post = stmt->u.for_.post;
	stmts = stmt->u.for_.stmts;

	type_check(init);
	// TODO: Ensure init has side effects
	type_check(cond);
	ensure_bool_expr(cond);
	type_check(post);
	// TODO: Ensure post has side effects
	enter_new_scope(sym_tbl);
	check_compound_stmt(stmts);
	leave_scope(sym_tbl);
}

static void check_return_stmt(struct stmt *stmt)
{
	struct type *return_type;
	struct expr *expr;

	assert(stmt->kind == RETURN_STMT);
	expr = stmt->u.return_.expr;
	assert(cur_func_type->kind == FUNC_TYPE);
	return_type = cur_func_type->u.func.ret;
	if (expr == NULL) {
		if (return_type->kind != VOID_TYPE) {
			fatal_error(stmt->lineno,
					"Returning void in a non-void fuction");
		}
	} else {
		type_check(expr);
		/*
		 * TODO: are_types_compat() does not recognize that return_type
		 * must be at least as strict as expr->type.
		 */
		if (!are_types_compat(return_type, expr->type)) {
			fatal_error(stmt->lineno,
					"Type of value returned is not "
					"compatible with the function's return "
					"type");
		}
	}
}

static void check_decl(struct decl *);

static void check_stmt(struct stmt *stmt)
{
	switch (stmt->kind) {
	case DECL_STMT:
		check_decl(stmt->u.decl.decl);
		break;
	case EXPR_STMT:
		type_check(stmt->u.expr.expr);
		break;
	case IF_STMT:
		check_if_stmt(stmt);
		break;
	case DO_STMT:
		check_do_stmt(stmt);
		break;
	case WHILE_STMT:
		check_while_stmt(stmt);
		break;
	case FOR_STMT:
		check_for_stmt(stmt);
		break;
	case RETURN_STMT:
		check_return_stmt(stmt);
		break;
	}
}

static void check_compound_stmt(Vec *stmts)
{
	size_t i;

	for (i = 0; i < vec_len(stmts); i++) {
		check_stmt(vec_get(stmts, i));
	}
}

static void check_func_decl(struct decl *decl)
{
	struct type *func_type, *param_type;
	char *func_name, *param_name;
	Vec *param_types, *param_names;
	Vec *body_stmts;
	size_t i, nparams;

	assert(decl->kind == FUNC_DECL);
	func_type = decl->u.func.type;
	func_name = decl->u.func.name;
	param_names = decl->u.func.param_names;
	body_stmts = decl->u.func.body_stmts;
	assert(func_type->kind == FUNC_TYPE);
	param_types = func_type->u.func.params;

	ensure_not_declared(func_name, decl->lineno);
	if (!is_global_scope(sym_tbl)) {
		fatal_error(decl->lineno, "Function defined with local scope");
	}
	insert_symbol(sym_tbl, func_name, alloc_val_sym_info(true, func_type));
	cur_func_type = func_type;
	enter_new_scope(sym_tbl);
	nparams = vec_len(param_types);
	for (i = 0; i < nparams; i++) {
		param_type = vec_get(param_types, i);
		param_name = vec_get(param_names, i);
		insert_symbol(sym_tbl, param_name,
				alloc_val_sym_info(true, param_type));
	}
	check_compound_stmt(body_stmts);
	leave_scope(sym_tbl);
}

// TODO: Add a maximum nest level
static void check_decl(struct decl *decl)
{
	switch (decl->kind) {
	case DATA_DECL:
		check_data_decl(decl);
		break;
	case TYPEDEF_DECL:
		check_typedef_decl(decl);
		break;
	case FUNC_DECL:
		check_func_decl(decl);
		break;
	}
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
