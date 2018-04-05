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
			Type *type;
		} value;
		Type *type;
	} u;
};

static struct symbol_table sym_tbl;
static Type *cur_func_type;

static struct symbol_info *alloc_val_sym_info(bool is_let, Type *type)
{
	struct symbol_info *sym_info;

	sym_info = NEW(struct symbol_info);
	sym_info->kind = VALUE_SYM;
	sym_info->u.value.is_let = is_let;
	sym_info->u.value.type = type;
	return sym_info;
}

#if 0
static struct symbol_info *alloc_type_sym_info(Type *type)
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

static bool is_pure_expr(Expr *);

static bool is_pure_unary_op_expr(UnaryOpExpr *expr)
{
	switch (expr->op) {
	case NUM_NEG_OP:
	case DEREF_OP:
	case REF_OP:
	case BIT_NEG_OP:
	case LOG_NEG_OP:
		return is_pure_expr(expr->operand);
	case PRE_INC_OP:
	case POST_INC_OP:
	case PRE_DEC_OP:
	case POST_DEC_OP:
		return false;
	}
	internal_error();
}

static bool is_pure_bin_op_expr(BinOpExpr *expr)
{
	switch (expr->op) {
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
		return is_pure_expr(expr->l) && is_pure_expr(expr->r);
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

static bool are_exprs_pure(Vec *exprs)
{
	size_t i;

	for (i = 0; i < vec_len(exprs); i++) {
		if (!is_pure_expr(vec_get(exprs, i))) {
			return false;
		}
	}
	return true;
}

static bool is_pure_array_lit_expr(ArrayLitExpr *expr)
{
	return are_exprs_pure(expr->subexprs);
}

static bool is_pure_block_expr(BlockExpr *expr)
{
	UNIMPLEMENTED();
}

static bool is_pure_if_expr(IfExpr *expr)
{
	return is_pure_expr(expr->cond)
		&& is_pure_expr(expr->then)
		&& is_pure_expr(expr->else_);
}

static bool is_pure_tuple_expr(TupleExpr *expr)
{
	return are_exprs_pure(expr->subexprs);
}

static bool is_pure_expr(Expr *expr)
{
	switch (expr->h.kind) {
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
		return is_pure_unary_op_expr((UnaryOpExpr *) expr);
	case BIN_OP_EXPR:
		return is_pure_bin_op_expr((BinOpExpr *) expr);
	case ARRAY_LIT_EXPR:
		return is_pure_array_lit_expr((ArrayLitExpr *) expr);
	case BLOCK_EXPR:
		return is_pure_block_expr((BlockExpr *) expr);
	case IF_EXPR:
		return is_pure_if_expr((IfExpr *) expr);
	case SWITCH_EXPR:
		UNIMPLEMENTED();
	case TUPLE_EXPR:
		return is_pure_tuple_expr((TupleExpr *) expr);
	case FUNC_CALL_EXPR:
		UNIMPLEMENTED();
	case FIELD_ACCESS_EXPR:
		UNIMPLEMENTED();
	}
	internal_error();
}

static bool unary_op_expr_is_lvalue(UnaryOpExpr *expr)
{
	switch (expr->op) {
	case DEREF_OP:
		UNIMPLEMENTED(); // TODO: Check for mutability
	default:
		return false;
	}
}

static bool ident_expr_is_lvalue(IdentExpr *expr)
{
	struct symbol_info *sym_info;

	sym_info = lookup_symbol(sym_tbl, expr->name);
	assert(sym_info != NULL);
	assert(sym_info->kind == VALUE_SYM);
	return !sym_info->u.value.is_let;
}

static bool expr_is_lvalue(Expr *expr)
{
	switch (expr->h.kind) {
	case BOOL_LIT_EXPR:
	case INT_LIT_EXPR:
	case FLOAT_LIT_EXPR:
	case CHAR_LIT_EXPR:
	case STRING_LIT_EXPR:
	case ARRAY_LIT_EXPR:
	case LAMBDA_EXPR:
		return false;
	case UNARY_OP_EXPR:
		return unary_op_expr_is_lvalue((UnaryOpExpr *) expr);
	case BIN_OP_EXPR:
		return false;
	case IDENT_EXPR:
		return ident_expr_is_lvalue((IdentExpr *) expr);
	case BLOCK_EXPR:
	case IF_EXPR:
	case SWITCH_EXPR:
	case TUPLE_EXPR:
	case FUNC_CALL_EXPR:
	case FIELD_ACCESS_EXPR:
		return false;
	}
	internal_error();
}

bool type_is_unsigned_int(Type *type)
{
	switch (type->h.kind) {
	case U8_TYPE:
	case U16_TYPE:
	case U32_TYPE:
	case U64_TYPE:
		return true;
	default:
		return false;
	}
}

bool type_is_signed_int(Type *type)
{
	switch (type->h.kind) {
	case I8_TYPE:
	case I16_TYPE:
	case I32_TYPE:
	case I64_TYPE:
		return true;
	default:
		return false;
	}
}

static bool type_is_int(Type *type)
{
	return type->h.kind == UNSIZED_INT_TYPE
		|| type_is_unsigned_int(type)
		|| type_is_signed_int(type);
}

bool type_is_float(Type *type)
{
	switch (type->h.kind) {
	case F32_TYPE:
	case F64_TYPE:
		return true;
	default:
		return false;
	}
}

static bool type_is_num(Type *type)
{
	return type_is_int(type) || type_is_float(type);
}

static void type_check(Expr *);

static void type_check_num_neg_expr(UnaryOpExpr *expr)
{
	Expr *operand = expr->operand;

	type_check(operand);
	if (!type_is_num(operand->h.type)) {
		compat_error(expr->h.lineno);
	}
	if (type_is_unsigned_int(operand->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = dup_type(operand->h.type);
}

static void type_check_inc_or_dec_expr(UnaryOpExpr *expr)
{
	Expr *operand = expr->operand;

	type_check(operand);
	if (!expr_is_lvalue(operand)) {
		lvalue_error(expr->h.lineno);
	}
	if (!type_is_num(operand->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = dup_type(operand->h.type);
}

static void type_check_deref_expr(UnaryOpExpr *expr)
{
	PointerType *pointer_type;
	Expr *operand = expr->operand;

	type_check(operand);
	if (operand->h.type->h.kind != POINTER_TYPE) {
		compat_error(expr->h.lineno);
	}
	pointer_type = (PointerType *) operand->h.type;
	expr->h.type = dup_type(pointer_type->pointee_type);
}

static void type_check_ref_expr(UnaryOpExpr *expr)
{
	Expr *operand = expr->operand;

	type_check(operand);
	if (!expr_is_lvalue(operand)) {
		lvalue_error(expr->h.lineno);
	}
	expr->h.type = (Type *) ALLOC_POINTER_TYPE(NO_LINENO,
			dup_type(operand->h.type));
}

static void type_check_bit_neg_expr(UnaryOpExpr *expr)
{
	Expr *operand = expr->operand;

	type_check(operand);
	if (!type_is_unsigned_int(operand->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = dup_type(operand->h.type);
}

static void type_check_log_neg_expr(UnaryOpExpr *expr)
{
	Expr *operand = expr->operand;

	type_check(operand);
	if (operand->h.type->h.kind != BOOL_TYPE) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = (Type *) ALLOC_BOOL_TYPE(NO_LINENO);
}

static void type_check_unary_op_expr(UnaryOpExpr *expr)
{
	switch (expr->op) {
	case NUM_NEG_OP:
		type_check_num_neg_expr(expr);
		break;
	case PRE_INC_OP:
	case POST_INC_OP:
	case PRE_DEC_OP:
	case POST_DEC_OP:
		type_check_inc_or_dec_expr(expr);
		break;
	case DEREF_OP:
		type_check_deref_expr(expr);
		break;
	case REF_OP:
		type_check_ref_expr(expr);
		break;
	case BIT_NEG_OP:
		type_check_bit_neg_expr(expr);
		break;
	case LOG_NEG_OP:
		type_check_log_neg_expr(expr);
		break;
	}
}

static Type *remove_const_and_volatile(Type *type)
{
	switch (type->h.kind) {
	case CONST_TYPE: {
		ConstType *const_type = (ConstType *) type;
		return remove_const_and_volatile(const_type->subtype);
	}
	case VOLATILE_TYPE: {
		VolatileType *volatile_type = (VolatileType *) type;
		return remove_const_and_volatile(volatile_type->subtype);
	}
	default:
		return type;
	}
}

static bool types_are_compat(Type *, Type *);

static bool type_vecs_are_compat(Vec *types1, Vec *types2)
{
	Type *type1, *type2;
	size_t len, i;

	if (vec_len(types1) != vec_len(types2)) {
		return false;
	}
	len = vec_len(types1);
	for (i = 0; i < len; i++) {
		type1 = vec_get(types1, i);
		type2 = vec_get(types2, i);
		if (!types_are_compat(type1, type2)) {
			return false;
		}
	}
	return true;
}

static bool alias_types_are_compat(AliasType *type1, AliasType *type2)
{
	return strcmp(type1->name, type2->name) == 0;
}

static bool param_types_are_compat(ParamType *type1, ParamType *type2)
{
	return strcmp(type1->name, type2->name) == 0
		&& type_vecs_are_compat(type1->params, type2->params);
}

static bool array_types_are_compat(ArrayType *type1, ArrayType *type2)
{
	return types_are_compat(type1->item_type, type2->item_type)
		&& (EITHER_EQ(type1->len, type2->len, 0)
				|| type1->len == type2->len);
}

static bool pointer_types_are_compat(PointerType *type1, PointerType *type2)
{
	return types_are_compat(type1->pointee_type, type2->pointee_type);
}

static bool tuple_types_are_compat(TupleType *type1, TupleType *type2)
{
	return type_vecs_are_compat(type1->member_types, type2->member_types);
}

static bool func_types_are_compat(FuncType *type1, FuncType *type2)
{
	return types_are_compat(type1->return_type, type2->return_type)
		&& type_vecs_are_compat(type1->param_types, type2->param_types);
}

static bool types_are_compat(Type *type1, Type *type2)
{
	// TODO: Const and volatile checking
	type1 = remove_const_and_volatile(type1);
	type2 = remove_const_and_volatile(type2);
	switch (type1->h.kind) {
	case UNSIZED_INT_TYPE:
		return type_is_int(type2);
	case U8_TYPE:
	case U16_TYPE:
	case U32_TYPE:
	case U64_TYPE:
	case I8_TYPE:
	case I16_TYPE:
	case I32_TYPE:
	case I64_TYPE:
		return type2->h.kind == type1->h.kind
			|| type2->h.kind == UNSIZED_INT_TYPE;
	case F32_TYPE:
	case F64_TYPE:
	case BOOL_TYPE:
	case VOID_TYPE:
	case CHAR_TYPE:
		return type2->h.kind == type1->h.kind;
	case ALIAS_TYPE:
		return type2->h.kind == type1->h.kind
			&& alias_types_are_compat((AliasType *) type1,
					(AliasType *) type2);
	case PARAM_TYPE:
		return type2->h.kind == type1->h.kind
			&& param_types_are_compat((ParamType *) type1,
					(ParamType *) type2);
	case ARRAY_TYPE:
		return type2->h.kind == type1->h.kind
			&& array_types_are_compat((ArrayType *) type1,
					(ArrayType *) type2);
	case POINTER_TYPE:
		return type2->h.kind == type1->h.kind
			&& pointer_types_are_compat((PointerType *) type1,
					(PointerType *) type2);
	case TUPLE_TYPE:
		return type2->h.kind == type1->h.kind
			&& tuple_types_are_compat((TupleType *) type1,
					(TupleType *) type2);
	case STRUCT_TYPE:
		UNIMPLEMENTED();
	case FUNC_TYPE:
		return type2->h.kind == type1->h.kind
			&& func_types_are_compat((FuncType *) type1,
					(FuncType *) type2);
	case CONST_TYPE:
	case VOLATILE_TYPE:
		// NOTREACHED
		internal_error();
	}
	internal_error();
}

static Type *dup_stricter_type(Type *, Type *);

static ArrayType *dup_stricter_array_type(ArrayType *type1, ArrayType *type2)
{
	Type *stricter_subtype;
	unsigned len;

	stricter_subtype =
		dup_stricter_type(type1->item_type, type2->item_type);
	if (type1->len == 0) {
		len = type2->len;
	} else {
		len = type1->len;
	}
	return ALLOC_ARRAY_TYPE(NO_LINENO, stricter_subtype, len);
}

static PointerType *dup_stricter_pointer_type(PointerType *type1,
		PointerType *type2)
{
	Type *stricter_subtype;

	stricter_subtype =
		dup_stricter_type(type1->pointee_type, type2->pointee_type);
	return ALLOC_POINTER_TYPE(NO_LINENO, stricter_subtype);
}

static TupleType *dup_stricter_tuple_type(TupleType *type1, TupleType *type2)
{
	Type *member_type_1, *member_type_2, *stricter_member_type;
	Vec *strictest_types;
	size_t i, len;

	strictest_types = alloc_vec(free_type);
	len = vec_len(type1->member_types);
	for (i = 0; i < len; i++) {
		member_type_1 = vec_get(type1->member_types, i);
		member_type_2 = vec_get(type2->member_types, i);
		stricter_member_type =
			dup_stricter_type(member_type_1, member_type_2);
		vec_push(strictest_types, stricter_member_type);
	}
	return ALLOC_TUPLE_TYPE(NO_LINENO, strictest_types);
}

static Type *dup_stricter_type(Type *type1, Type *type2)
{
	if (!types_are_compat(type1, type2)) {
		// TODO: Is this lineno ever NO_LINENO
		compat_error(type1->h.lineno);
	}

	switch (type1->h.kind) {
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
		UNIMPLEMENTED();
	case ARRAY_TYPE:
		return (Type *) dup_stricter_array_type((ArrayType *) type1,
				(ArrayType *) type2);
	case POINTER_TYPE:
		return (Type *) dup_stricter_pointer_type((PointerType *) type1,
				(PointerType *) type2);
	case TUPLE_TYPE:
		return (Type *) dup_stricter_tuple_type((TupleType *) type1,
				(TupleType *) type2);
	case STRUCT_TYPE:
		UNIMPLEMENTED();
	case FUNC_TYPE:
		UNIMPLEMENTED();
	case CONST_TYPE: {
#if 0
		Type *subtype1, *subtype2;

		subtype1 = type1->u.const_.type;
		if (type2->kind == CONST_TYPE) {
			subtype2 = type2->u.const_.type;
			return ALLOC_CONST_TYPE(NO_LINENO,
					dup_stricter_type(subtype1, subtype2));
		} else {
			return ALLOC_CONST_TYPE(NO_LINENO,
					dup_stricter_type(subtype1, type2));
		}
#endif
		UNIMPLEMENTED();
	}
	case VOLATILE_TYPE: {
#if 0
		Type *subtype1, *subtype2;

		subtype1 = type1->u.volatile_.type;
		if (type2->kind == VOLATILE_TYPE) {
			subtype2 = type2->u.volatile_.type;
			return ALLOC_VOLATILE_TYPE(NO_LINENO,
					dup_stricter_type(subtype1, subtype2));
		} else {
			return ALLOC_VOLATILE_TYPE(NO_LINENO,
					dup_stricter_type(subtype1, type2));
		}
#endif
		UNIMPLEMENTED();
	}
	}
	internal_error();
}

static bool type_is_convertible(Type *, Type *);

static bool types_are_convertible(Vec *from_types, Vec *to_types)
{
	Type *from_type, *to_type;
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

static bool type_is_convertible(Type *from_type, Type *to_type)
{
	switch (to_type->h.kind) {
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
		UNIMPLEMENTED();
	case ARRAY_TYPE: {
#if 0
		Type *from_subtype, *to_subtype;
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
#endif
		UNIMPLEMENTED();
	}
	case POINTER_TYPE: {
#if 0
		Type *from_subtype, *to_subtype;

		if (from_type->kind != POINTER_TYPE) {
			return false;
		}
		from_subtype = from_type->u.pointer.l;
		to_subtype = to_type->u.pointer.l;
		return type_is_convertible(from_subtype, to_subtype);
#endif
		UNIMPLEMENTED();
	}
	case TUPLE_TYPE: {
#if 0
		Vec *from_types, *to_types;

		if (from_type->kind != TUPLE_TYPE) {
			return false;
		}
		from_types = from_type->u.tuple.types;
		to_types = to_type->u.tuple.types;
		return types_are_convertible(from_types, to_types);
#endif
		UNIMPLEMENTED();
	}
	case STRUCT_TYPE:
		UNIMPLEMENTED();
	// TODO: Make sure this isn't problematic
	case FUNC_TYPE: {
#if 0
		Type *from_return_type, *to_return_type;
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
#endif
		UNIMPLEMENTED();
	}
	case CONST_TYPE: {
#if 0
		Type *from_subtype, *to_subtype;

		to_subtype = to_type->u.const_.type;
		if (from_type->kind == CONST_TYPE) {
			from_subtype = from_type->u.const_.type;
			return type_is_convertible(from_subtype, to_subtype);
		} else {
			return type_is_convertible(from_type, to_subtype);
		}
#endif
		UNIMPLEMENTED();
	}
	case VOLATILE_TYPE: {
#if 0
		Type *from_subtype, *to_subtype;

		to_subtype = to_type->u.volatile_.type;
		if (from_type->kind == VOLATILE_TYPE) {
			from_subtype = from_type->u.volatile_.type;
			return type_is_convertible(from_subtype, to_subtype);
		} else {
			return type_is_convertible(from_type, to_subtype);
		}
#endif
		UNIMPLEMENTED();
	}
	}
	internal_error();
}

static void ensure_bool_expr(Expr *expr)
{
	if (expr->h.type->h.kind != BOOL_TYPE) {
		fatal_error(expr->lineno, "Expected a boolean expression");
	}
}

static void type_check_bin_math_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check(l);
	type_check(r);
	if (!type_is_num(l->h.type)) {
		compat_error(expr->h.lineno);
	}
	if (!are_types_compat(l->h.type, r->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = dup_stricter_type(l->h.type, r->h.type);
}

static void type_check_relational_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check(l);
	type_check(r);
	if (!type_is_num(l->h.type)) {
		compat_error(expr->h.lineno);
	}
	if (!are_types_compat(l->h.type, r->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = ALLOC_BOOL_TYPE(NO_LINENO);
}

static void type_check_equality_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check(l);
	type_check(r);
	if (!are_types_compat(l->h.type, r->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = ALLOC_BOOL_TYPE(NO_LINENO);
}

static void type_check_bin_bitwise_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check(l);
	type_check(r);
	if (!type_is_unsigned_int(l->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = dup_stricter_type(l->h.type, r->h.type);
}

static void type_check_bin_logical_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check(l);
	type_check(r);
	if (l->h.type->h.kind != BOOL_TYPE) {
		compat_error(expr->h.lineno);
	}
	if (!are_types_compat(l->h.type, r->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = ALLOC_BOOL_TYPE(NO_LINENO);
}

static void type_check_assign_expr_common(BinOpExpr *expr)
{
	if (!expr_is_lvalue(l)) {
		lvalue_error(expr->h.lineno);
	}
	if (!are_types_compat(l->h.type, r->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = ALLOC_VOID_TYPE(NO_LINENO);
}

static void type_check_assign_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check(l);
	type_check(r);
	type_check_assign_expr_common(expr);
}

static void type_check_math_assign_expr(BinOpExpr *expr)
{
	type_check(l);
	type_check(r);
	if (!type_is_num(l->h.type)) {
		compat_error(expr->h.lineno);
	}
	type_check_assign_expr_common(expr);
}

static void type_check_bitwise_assign_expr(BinOpExpr *expr)
{
	type_check(l);
	type_check(r);
	if (!type_is_unsigned_int(l->h.type)) {
		compat_error(expr->h.lineno);
	}
	type_check_assign_expr_common(expr);
}

static void type_check_bin_op_expr(BinOpExpr *expr)
{
	switch (expr->op) {
	case ADD_OP:
	case SUB_OP:
	case MUL_OP:
	case DIV_OP:
	case MOD_OP:
		type_check_bin_math_expr(expr);
		break;
	case LT_OP:
	case GT_OP:
	case LT_EQ_OP:
	case GT_EQ_OP:
		type_check_relational_expr(expr);
		break;
	case EQ_OP:
	case NOT_EQ_OP:
		type_check_equality_expr(expr);
		break;
	case BIT_AND_OP:
	case BIT_OR_OP:
	case BIT_XOR_OP:
	case BIT_SHIFT_L_OP:
	case BIT_SHIFT_R_OP:
		type_check_bin_bitwise_expr(expr);
		break;
	case LOG_AND_OP:
	case LOG_OR_OP:
		type_check_bin_logical_expr(expr);
		break;
	case ASSIGN_OP:
		type_check_assign_expr(expr);
		break;
	case ADD_ASSIGN_OP:
	case SUB_ASSIGN_OP:
	case MUL_ASSIGN_OP:
	case DIV_ASSIGN_OP:
	case MOD_ASSIGN_OP:
		type_check_math_assign_expr(expr);
		break;
	case BIT_AND_ASSIGN_OP:
	case BIT_OR_ASSIGN_OP:
	case BIT_XOR_ASSIGN_OP:
	case BIT_SHIFT_L_ASSIGN_OP:
	case BIT_SHIFT_R_ASSIGN_OP:
		type_check_bitwise_assign_expr(expr);
		break;
	}
}

static void type_check_lambda(LambdaExpr *expr)
{
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
	UNIMPLEMENTED();
}

static void type_check_exprs(Vec *exprs)
{
	size_t i;

	for (i = 0; i < vec_len(exprs); i++) {
		type_check(vec_get(exprs, i));
	}
}

static void type_check_array_lit(ArrayLitExpr *expr)
{
	Expr *subexpr, *first_subexpr;
	Type *strictest_type, *tmp;
	size_t len, i;

	type_check_exprs(expr->subexprs);
	len = vec_len(expr->subexprs);
	first_subexpr = vec_get(expr->subexprs, 0);
	strictest_type = dup_type(first_subexpr->type);
	for (i = 1; i < len; i++) {
		subexpr = vec_get(expr->subexprs, i);
		tmp = strictest_type;
		strictest_type =
			dup_stricter_type(strictest_type, subexpr->h.type);
		free(tmp);
	}
	expr->type = ALLOC_ARRAY_TYPE(NO_LINENO, strictest_type, len);
}

static void type_check_ident(IdentExpr *expr)
{
	struct symbol_info *sym_info;

	sym_info = lookup_symbol(sym_tbl, expr->name);
	if (sym_info == NULL) {
		fatal_error(expr->h.lineno,
				"Name `%s` does not exist in scope; did you "
				"spell it wrong?", expr->name);
	}
	if (sym_info->kind != VALUE_SYM) {
		fatal_error(expr->h.lineno,
				"Name `%s` is the name of a type, not a value",
				expr->name);
	}
	expr->type = dup_type(sym_info->u.value.type);
}

static void check_compound_stmt(Vec *);

static void type_check_block(Expr *expr)
{
	Vec *stmts;

	stmts = expr->u.block.stmts;

	enter_new_scope(sym_tbl);
	check_compound_stmt(stmts);
	leave_scope(sym_tbl);
	expr->type = ALLOC_VOID_TYPE(NO_LINENO);
}

static void type_check_if(Expr *expr)
{
	Expr *cond, *then, *else_;

	assert(expr->kind == IF_EXPR);
	cond = expr->u.if_.cond;
	then = expr->u.if_.then;
	else_ = expr->u.if_.else_;

	type_check(cond);
	ensure_bool_expr(cond);
	type_check(then);
	type_check(else_);
	if (!are_types_compat(then->h.type, else_->h.type)) {
		fatal_error(then->lineno, "Types of `then` and `else` "
		                          "expressions are not compatible");
	}
	expr->h.type = dup_stricter_type(then->h.type, else_->h.type);
}

static void type_check_tuple(TupleExpr *expr)
{
	Expr *subexpr;
	Vec *subexpr_types;
	size_t i;

	subexpr_types = alloc_vec(free_type);
	for (i = 0; i < vec_len(expr->subexprs); i++) {
		subexpr = vec_get(expr->subexprs, i);
		type_check(subexpr);
		vec_push(subexpr_types, subexpr->h.type);
	}
	expr->h.type = (Type *) ALLOC_TUPLE_TYPE(NO_LINENO, subexpr_types);
}

static void type_check_func_call(FuncCallExpr *expr)
{
	Type *param_type, *return_type;
	Expr *func, *arg;
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

static void type_check_field_access_expr(Expr *expr)
{
	(void) expr;
	internal_error(); // TODO: Stub
}

static void type_check(Expr *expr)
{
	assert(expr->type == NULL);
	switch (expr->kind) {
	case BOOL_LIT_EXPR:
		expr->type = ALLOC_BOOL_TYPE(NO_LINENO);
		break;
	case INT_LIT_EXPR:
		expr->type = ALLOC_UNSIZED_INT_TYPE(NO_LINENO);
		break;
	case FLOAT_LIT_EXPR:
		// TODO: Fix this
		expr->type = ALLOC_F64_TYPE(NO_LINENO);
		break;
	case CHAR_LIT_EXPR:
		expr->type = ALLOC_CHAR_TYPE(NO_LINENO);
		break;
	case STRING_LIT_EXPR: {
		unsigned len = expr->u.string_lit.len;

		expr->type = ALLOC_ARRAY_TYPE(NO_LINENO,
				ALLOC_CHAR_TYPE(NO_LINENO), len);
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

static void ensure_declarable_type(Type *type)
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

static void check_data_decl(Decl *decl)
{
	unsigned lineno;
	bool is_let;
	Type *type;
	char *name;
	Expr *init;

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

static void check_typedef_decl(Decl *decl)
{
	Type *type;
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

static void check_if_stmt(Stmt *stmt)
{
	Expr *cond;
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

static void check_do_stmt(Stmt *stmt)
{
	Vec *stmts;
	Expr *cond;

	stmts = stmt->u.do_.stmts;
	cond = stmt->u.do_.cond;

	enter_new_scope(sym_tbl);
	check_compound_stmt(stmts);
	type_check(cond);
	ensure_bool_expr(cond);
	leave_scope(sym_tbl);
}

static void check_while_stmt(Stmt *stmt)
{
	Expr *cond;
	Vec *stmts;

	cond = stmt->u.while_.cond;
	stmts = stmt->u.while_.stmts;

	type_check(cond);
	ensure_bool_expr(cond);
	enter_new_scope(sym_tbl);
	check_compound_stmt(stmts);
	leave_scope(sym_tbl);
}

static void check_for_stmt(Stmt *stmt)
{
	Expr *init, *cond, *post;
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

static void check_return_stmt(Stmt *stmt)
{
	Type *return_type;
	Expr *expr;

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

static void check_decl(Decl *);

static void check_stmt(Stmt *stmt)
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

static void check_func_decl(Decl *decl)
{
	Type *func_type, *param_type;
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
static void check_decl(Decl *decl)
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

void check_ast(Ast ast)
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
