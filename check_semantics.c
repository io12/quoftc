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
static FuncType *cur_func_type;

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
	INTERNAL_ERROR();
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
	INTERNAL_ERROR();
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
	INTERNAL_ERROR();
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
	INTERNAL_ERROR();
}

bool is_unsigned_int_type(Type *type)
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
		|| is_unsigned_int_type(type)
		|| type_is_signed_int(type);
}

bool is_float_type(Type *type)
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
	return type_is_int(type) || is_float_type(type);
}

static void type_check_string_lit_expr(StringLitExpr *expr)
{
	Type *char_type;
	
	char_type = (Type *) ALLOC_CHAR_TYPE(NO_LINENO);
	expr->h.type = (Type *) ALLOC_ARRAY_TYPE(NO_LINENO, char_type,
			expr->len);
}

static void type_check_expr(Expr *);

static void type_check_num_neg_expr(UnaryOpExpr *expr)
{
	Expr *operand = expr->operand;

	type_check_expr(operand);
	if (!type_is_num(operand->h.type)) {
		compat_error(expr->h.lineno);
	}
	if (is_unsigned_int_type(operand->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = dup_type(operand->h.type);
}

static void type_check_inc_or_dec_expr(UnaryOpExpr *expr)
{
	Expr *operand = expr->operand;

	type_check_expr(operand);
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

	type_check_expr(operand);
	if (operand->h.type->h.kind != POINTER_TYPE) {
		compat_error(expr->h.lineno);
	}
	pointer_type = (PointerType *) operand->h.type;
	expr->h.type = dup_type(pointer_type->pointee_type);
}

static void type_check_ref_expr(UnaryOpExpr *expr)
{
	Expr *operand = expr->operand;

	type_check_expr(operand);
	if (!expr_is_lvalue(operand)) {
		lvalue_error(expr->h.lineno);
	}
	expr->h.type = (Type *) ALLOC_POINTER_TYPE(NO_LINENO,
			dup_type(operand->h.type));
}

static void type_check_bit_neg_expr(UnaryOpExpr *expr)
{
	Expr *operand = expr->operand;

	type_check_expr(operand);
	if (!is_unsigned_int_type(operand->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = dup_type(operand->h.type);
}

static void type_check_log_neg_expr(UnaryOpExpr *expr)
{
	Expr *operand = expr->operand;

	type_check_expr(operand);
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
	return type_vecs_are_compat(type1->members, type2->members);
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
		INTERNAL_ERROR();
	}
	INTERNAL_ERROR();
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
	len = vec_len(type1->members);
	for (i = 0; i < len; i++) {
		member_type_1 = vec_get(type1->members, i);
		member_type_2 = vec_get(type2->members, i);
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
	INTERNAL_ERROR();
}

#if 0
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
		INTERNAL_ERROR();
	case U8_TYPE:
	case U16_TYPE:
	case U32_TYPE:
	case U64_TYPE:
	case I8_TYPE:
	case I16_TYPE:
	case I32_TYPE:
	case I64_TYPE:
		return from_type->h.kind == UNSIZED_INT_TYPE
			|| from_type->h.kind == to_type->h.kind;
	case F32_TYPE:
	case F64_TYPE:
	case BOOL_TYPE:
	case VOID_TYPE:
	case CHAR_TYPE:
		return from_type->h.kind == to_type->h.kind;
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
	INTERNAL_ERROR();
}
#endif

static void ensure_bool_expr(Expr *expr)
{
	if (expr->h.type->h.kind != BOOL_TYPE) {
		fatal_error(expr->h.lineno, "Expected a boolean expression");
	}
}

static void type_check_bin_math_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check_expr(l);
	type_check_expr(r);
	if (!type_is_num(l->h.type)) {
		compat_error(expr->h.lineno);
	}
	if (!types_are_compat(l->h.type, r->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = dup_stricter_type(l->h.type, r->h.type);
}

static void type_check_relational_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check_expr(l);
	type_check_expr(r);
	if (!type_is_num(l->h.type)) {
		compat_error(expr->h.lineno);
	}
	if (!types_are_compat(l->h.type, r->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = ALLOC_BOOL_TYPE(NO_LINENO);
}

static void type_check_equality_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check_expr(l);
	type_check_expr(r);
	if (!types_are_compat(l->h.type, r->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = ALLOC_BOOL_TYPE(NO_LINENO);
}

static void type_check_bin_bitwise_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check_expr(l);
	type_check_expr(r);
	if (!is_unsigned_int_type(l->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = dup_stricter_type(l->h.type, r->h.type);
}

static void type_check_bin_logical_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check_expr(l);
	type_check_expr(r);
	if (l->h.type->h.kind != BOOL_TYPE) {
		compat_error(expr->h.lineno);
	}
	if (!types_are_compat(l->h.type, r->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = ALLOC_BOOL_TYPE(NO_LINENO);
}

static void type_check_assign_expr_common(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	if (!expr_is_lvalue(l)) {
		lvalue_error(expr->h.lineno);
	}
	if (!types_are_compat(l->h.type, r->h.type)) {
		compat_error(expr->h.lineno);
	}
	expr->h.type = ALLOC_VOID_TYPE(NO_LINENO);
}

static void type_check_assign_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check_expr(l);
	type_check_expr(r);
	type_check_assign_expr_common(expr);
}

static void type_check_math_assign_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check_expr(l);
	type_check_expr(r);
	if (!type_is_num(l->h.type)) {
		compat_error(expr->h.lineno);
	}
	type_check_assign_expr_common(expr);
}

static void type_check_bitwise_assign_expr(BinOpExpr *expr)
{
	Expr *l = expr->l;
	Expr *r = expr->r;

	type_check_expr(l);
	type_check_expr(r);
	if (!is_unsigned_int_type(l->h.type)) {
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

static void type_check_lambda_expr(LambdaExpr *expr)
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
	expr->h.type = dup_type(type);
#endif
	UNIMPLEMENTED();
}

static void type_check_exprs(Vec *exprs)
{
	size_t i;

	for (i = 0; i < vec_len(exprs); i++) {
		type_check_expr(vec_get(exprs, i));
	}
}

static void type_check_array_lit_expr(ArrayLitExpr *expr)
{
	Expr *subexpr, *first_subexpr;
	Type *strictest_type, *tmp;
	size_t len, i;

	type_check_exprs(expr->subexprs);
	len = vec_len(expr->subexprs);
	first_subexpr = vec_get(expr->subexprs, 0);
	strictest_type = dup_type(first_subexpr->h.type);
	for (i = 1; i < len; i++) {
		subexpr = vec_get(expr->subexprs, i);
		tmp = strictest_type;
		strictest_type =
			dup_stricter_type(strictest_type, subexpr->h.type);
		free(tmp);
	}
	expr->h.type =
		(Type *) ALLOC_ARRAY_TYPE(NO_LINENO, strictest_type, len);
}

static void type_check_ident_expr(IdentExpr *expr)
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
	expr->h.type = dup_type(sym_info->u.value.type);
}

static void check_stmt_block(StmtBlock *);

static void type_check_block_expr(BlockExpr *expr)
{
	check_stmt_block(expr->block);
	expr->h.type = (Type *) ALLOC_VOID_TYPE(NO_LINENO);
}

static void type_check_if_expr(IfExpr *expr)
{
	Type *then_type, *else_type;

	type_check_expr(expr->cond);
	ensure_bool_expr(expr->cond);
	type_check_expr(expr->then);
	type_check_expr(expr->else_);
	then_type = expr->then->h.type;
	else_type = expr->else_->h.type;
	if (!types_are_compat(then_type, else_type)) {
		fatal_error(expr->then->h.lineno,
				"Types of `then` and `else` expressions are "
				"not compatible");
	}
	expr->h.type = dup_stricter_type(then_type, else_type);
}

static void type_check_tuple_expr(TupleExpr *expr)
{
	Expr *subexpr;
	Vec *subexpr_types;
	size_t i;

	subexpr_types = alloc_vec(free_type);
	for (i = 0; i < vec_len(expr->subexprs); i++) {
		subexpr = vec_get(expr->subexprs, i);
		type_check_expr(subexpr);
		vec_push(subexpr_types, subexpr->h.type);
	}
	expr->h.type = (Type *) ALLOC_TUPLE_TYPE(NO_LINENO, subexpr_types);
}

static void type_check_func_call_expr(FuncCallExpr *expr)
{
	FuncType *func_type;
	Type *param_type;
	Expr *arg;
	size_t i;

	type_check_expr(expr->func);
	type_check_exprs(expr->args);
	if (expr->func->h.type->h.kind != FUNC_TYPE) {
		fatal_error(expr->h.lineno,
				"Expression is called but is not a function");
	}
	func_type = (FuncType *) expr->func->h.type;
	if (vec_len(expr->args) != vec_len(func_type->param_types)) {
		fatal_error(expr->h.lineno,
				"Function called with incorrect amount of "
				"arguments");
	}
	for (i = 0; i < vec_len(expr->args); i++) {
		arg = vec_get(expr->args, i);
		param_type = vec_get(func_type->param_types, i);
		// TODO: types_are_compat() may be the wrong check (const)
		if (!types_are_compat(arg->h.type, param_type)) {
			fatal_error(arg->h.lineno,
					"Type of passed argument is an "
					"unexpected type");
		}
	}
	expr->h.type = dup_type(func_type->return_type);
}

static void type_check_field_access_expr(FieldAccessExpr *expr)
{
	UNIMPLEMENTED();
}

static void type_check_expr(Expr *expr)
{
	assert(expr->h.type == NULL); // The type should not already be checked
	switch (expr->h.kind) {
	case BOOL_LIT_EXPR:
		expr->h.type = (Type *) ALLOC_BOOL_TYPE(NO_LINENO);
		break;
	case INT_LIT_EXPR:
		expr->h.type = (Type *) ALLOC_UNSIZED_INT_TYPE(NO_LINENO);
		break;
	case FLOAT_LIT_EXPR:
		// TODO: Fix this
		expr->h.type = (Type *) ALLOC_F64_TYPE(NO_LINENO);
		break;
	case CHAR_LIT_EXPR:
		expr->h.type = (Type *) ALLOC_CHAR_TYPE(NO_LINENO);
		break;
	case STRING_LIT_EXPR:
		type_check_string_lit_expr((StringLitExpr *) expr);
		break;
	case UNARY_OP_EXPR:
		type_check_unary_op_expr((UnaryOpExpr *) expr);
		break;
	case BIN_OP_EXPR:
		type_check_bin_op_expr((BinOpExpr *) expr);
		break;
	case LAMBDA_EXPR:
		type_check_lambda_expr((LambdaExpr *) expr);
		break;
	case ARRAY_LIT_EXPR:
		type_check_array_lit_expr((ArrayLitExpr *) expr);
		break;
	case IDENT_EXPR:
		type_check_ident_expr((IdentExpr *) expr);
		break;
	case BLOCK_EXPR:
		type_check_block_expr((BlockExpr *) expr);
		break;
	case IF_EXPR:
		type_check_if_expr((IfExpr *) expr);
		break;
	case SWITCH_EXPR:
		UNIMPLEMENTED();
	case TUPLE_EXPR:
		type_check_tuple_expr((TupleExpr *) expr);
		break;
	case FUNC_CALL_EXPR:
		type_check_func_call_expr((FuncCallExpr *) expr);
		break;
	case FIELD_ACCESS_EXPR:
		type_check_field_access_expr((FieldAccessExpr *) expr);
		break;
	}
}

static void ensure_declarable_type(Type *type)
{
	switch (type->h.kind) {
	case UNSIZED_INT_TYPE:
		INTERNAL_ERROR(); // The parser shouldn't set this
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
		fatal_error(type->h.lineno, "Void is not a declarable type");
	case ALIAS_TYPE:
	case PARAM_TYPE:
		UNIMPLEMENTED();
	case ARRAY_TYPE:
		ensure_declarable_type(((ArrayType *) type)->item_type);
		break;
	case POINTER_TYPE:
		ensure_declarable_type(((PointerType *) type)->pointee_type);
		break;
	case TUPLE_TYPE: {
		Vec *types;
		size_t i;

		types = ((TupleType *) type)->members;
		for (i = 0; i < vec_len(types); i++) {
			ensure_declarable_type(vec_get(types, i));
		}
		break;
	}
	case STRUCT_TYPE:
	case FUNC_TYPE:
	case CONST_TYPE:
	case VOLATILE_TYPE:
		UNIMPLEMENTED();
	}
}

static void ensure_not_declared(char *name, unsigned lineno)
{
	if (lookup_symbol(sym_tbl, name) != NULL) {
		fatal_error(lineno, "Name `%s` already declared in scope",
				name);
	}
}

static void check_data_decl(DataDecl *decl)
{
	ensure_declarable_type(decl->type);
	ensure_not_declared(decl->name, decl->h.lineno);
	if (is_global_scope(sym_tbl)) {
		if (decl->init == NULL) {
			fatal_error(decl->h.lineno, "Top level declaration of "
					"`%s` lacks an initializer",
					decl->name);
		}
		if (!is_pure_expr(decl->init)) {
			fatal_error(decl->h.lineno, "Top level declaration of "
			                    "`%s` is assigned to an impure "
			                    "expression", decl->name);
		}
	}
	if (decl->is_let && decl->init == NULL) {
		fatal_error(decl->h.lineno, "Constant declaration of `%s` "
				"lacks an initializer", decl->name);
	}
	if (decl->init != NULL) {
		type_check_expr(decl->init);
		/*
		 * TODO: types_are_compat() is problematic here; decl->type must
		 * always be stricter than decl->init->h.type.
		 */
		if (!types_are_compat(decl->type, decl->init->h.type)) {
			compat_error(decl->h.lineno);
		}
	}
	insert_symbol(sym_tbl, decl->name,
			alloc_val_sym_info(decl->is_let, decl->type));
}

static void check_typedef_decl(TypedefDecl *decl)
{
#if 0
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
#endif
	UNIMPLEMENTED();
}

static void check_decl_stmt(DeclStmt *stmt)
{
	UNIMPLEMENTED();
}

static void check_expr_stmt(ExprStmt *stmt)
{
	UNIMPLEMENTED();
}

static void check_if_stmt(IfStmt *stmt)
{
	type_check_expr(stmt->cond);
	ensure_bool_expr(stmt->cond);
	check_stmt_block(stmt->then_block);
	if (stmt->else_block != NULL) {
		check_stmt_block(stmt->else_block);
	}
}

static void check_stmts(Vec *stmts);

static void check_do_stmt(DoStmt *stmt)
{
	enter_new_scope(sym_tbl);
	check_stmts(stmt->block->stmts);
	type_check_expr(stmt->cond);
	ensure_bool_expr(stmt->cond);
	leave_scope(sym_tbl);
}

static void check_while_stmt(WhileStmt *stmt)
{
	type_check_expr(stmt->cond);
	ensure_bool_expr(stmt->cond);
	check_stmt_block(stmt->block);
}

// TODO: Ensure `init` and `post` have side effects
static void check_for_stmt(ForStmt *stmt)
{
	type_check_expr(stmt->init);
	type_check_expr(stmt->cond);
	ensure_bool_expr(stmt->cond);
	type_check_expr(stmt->post);
	check_stmt_block(stmt->block);
}

static void check_return_stmt(ReturnStmt *stmt)
{
	Type *func_return_type;

	func_return_type = cur_func_type->return_type;
	if (stmt->expr == NULL) {
		if (func_return_type->h.kind != VOID_TYPE) {
			fatal_error(stmt->h.lineno,
					"Returning void in a non-void fuction");
		}
	} else {
		type_check_expr(stmt->expr);
		/*
		 * TODO: types_are_compat() does not recognize that
		 * func_return_type must be at least as strict as
		 * stmt->expr->h.type.
		 */
		if (!types_are_compat(func_return_type, stmt->expr->h.type)) {
			fatal_error(stmt->h.lineno,
					"Type of value returned is not "
					"compatible with the function's return "
					"type");
		}
	}
}

static void check_decl(Decl *);

static void check_stmt(Stmt *stmt)
{
	switch (stmt->h.kind) {
	case DECL_STMT:
		check_decl_stmt((DeclStmt *) stmt);
		break;
	case EXPR_STMT:
		check_expr_stmt((ExprStmt *) stmt);
		break;
	case IF_STMT:
		check_if_stmt((IfStmt *) stmt);
		break;
	case DO_STMT:
		check_do_stmt((DoStmt *) stmt);
		break;
	case WHILE_STMT:
		check_while_stmt((WhileStmt *) stmt);
		break;
	case FOR_STMT:
		check_for_stmt((ForStmt *) stmt);
		break;
	case RETURN_STMT:
		check_return_stmt((ReturnStmt *) stmt);
		break;
	}
}

static void check_stmts(Vec *stmts)
{
	size_t i;

	for (i = 0; i < vec_len(stmts); i++) {
		check_stmt(vec_get(stmts, i));
	}
}

/*
 * This function checks a scoped statement block. Functions that need more
 * control over scoping can call `check_stmts()` directly.
 */
static void check_stmt_block(StmtBlock *block)
{
	enter_new_scope(sym_tbl);
	check_stmts(block->stmts);
	leave_scope(sym_tbl);
}

static void check_func_decl(FuncDecl *func)
{
	Type *param_type;
	char *param_name;
	Vec *param_types, *param_names;
	size_t i, nparams;

	ensure_not_declared(func->name, func->h.lineno);
	if (!is_global_scope(sym_tbl)) {
		fatal_error(func->h.lineno,
				"Function defined with local scope");
	}
	insert_symbol(sym_tbl, func->name,
			alloc_val_sym_info(true, (Type *) func->type));
	cur_func_type = func->type;
	param_types = func->type->param_types;
	param_names = func->param_names;
	assert(vec_len(param_names) == vec_len(param_types));
	nparams = vec_len(param_types);
	enter_new_scope(sym_tbl);
	for (i = 0; i < nparams; i++) {
		param_type = vec_get(param_types, i);
		param_name = vec_get(param_names, i);
		insert_symbol(sym_tbl, param_name,
				alloc_val_sym_info(true, param_type));
	}
	check_stmts(func->body->stmts);
	leave_scope(sym_tbl);
}

// TODO: Add a maximum nest level
static void check_decl(Decl *decl)
{
	switch (decl->h.kind) {
	case DATA_DECL:
		check_data_decl((DataDecl *) decl);
		break;
	case TYPEDEF_DECL:
		check_typedef_decl((TypedefDecl *) decl);
		break;
	case FUNC_DECL:
		check_func_decl((FuncDecl *) decl);
		break;
	}
}

// TODO: Scan all top level decls first to remove the need for prototypes
void check_ast(Ast ast)
{
	size_t i;

	sym_tbl = alloc_symbol_table();
	enter_new_scope(sym_tbl); // Global scope
	for (i = 0; i < vec_len(ast.decls); i++) {
		check_decl(vec_get(ast.decls, i));
	}
	free_symbol_table(sym_tbl);
}
