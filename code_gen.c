// Compile an AST with LLVM
// TODO: Fix scoping

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>
#include <llvm-c/TargetMachine.h>
#include "ds.h"
#include "ast.h"
#include "check_semantics.h"
#include "lex.h"
#include "quoftc.h"
#include "symbol_table.h"
#include "code_gen.h"

struct symbol_info {
	bool is_ptr;
	LLVMValueRef val;
};

static struct symbol_table sym_tbl;
static LLVMBasicBlockRef cur_func_return_block;
static LLVMValueRef cur_func_return_val_ptr;

static struct symbol_info *alloc_sym_info(bool is_ptr, LLVMValueRef val)
{
	struct symbol_info *sym_info;

	sym_info = NEW(struct symbol_info);
	sym_info->is_ptr = is_ptr;
	sym_info->val = val;
	return sym_info;
}

static LLVMTypeRef get_fat_ptr_type(LLVMTypeRef item_type)
{
	LLVMTypeRef struct_item_types[2];

	struct_item_types[0] = LLVMInt16Type();
	struct_item_types[1] = LLVMPointerType(item_type, 0);
	return LLVMStructType(struct_item_types, 2, false);
}

static LLVMTypeRef get_llvm_type(struct type *);

/*
 * The pointer returned from this function must be freed. It is okay to free
 * it after passing to an LLVM function.
 */
static LLVMTypeRef *get_llvm_types(Vec *types)
{
	LLVMTypeRef *llvm_types;
	size_t i, ntypes;

	ntypes = vec_len(types);
	llvm_types = xmalloc(sizeof(LLVMTypeRef) * ntypes);
	for (i = 0; i < ntypes; i++) {
		llvm_types[i] = get_llvm_type(vec_get(types, i));
	}
	return llvm_types;
}

static LLVMTypeRef get_llvm_type(struct type *type)
{
	switch (type->kind) {
	case UNSIZED_INT_TYPE:
		// TODO: Base this on the compilation target
		return LLVMInt32Type();
	case U8_TYPE:
	case I8_TYPE:
		return LLVMInt8Type();
	case U16_TYPE:
	case I16_TYPE:
		return LLVMInt16Type();
	case U32_TYPE:
	case I32_TYPE:
	case CHAR_TYPE:
		return LLVMInt32Type();
	case U64_TYPE:
	case I64_TYPE:
		return LLVMInt64Type();
	case F32_TYPE:
		return LLVMFloatType();
	case F64_TYPE:
		return LLVMDoubleType();
	case BOOL_TYPE:
		return LLVMInt1Type();
	case VOID_TYPE:
		return LLVMVoidType();
	case ALIAS_TYPE:
		// TODO: Resolve type
	case PARAM_TYPE:
		// TODO: Resolve type
	case ARRAY_TYPE: {
		LLVMTypeRef item_type;
		unsigned len;

		item_type = get_llvm_type(type->u.array.l);
		len = type->u.array.len;
		if (len == 0) {
			return get_fat_ptr_type(item_type);
		} else {
			return LLVMArrayType(item_type, len);
		}
	}
	case POINTER_TYPE:
		return LLVMPointerType(get_llvm_type(type->u.pointer.l), 0);
	case TUPLE_TYPE: {
		LLVMTypeRef tuple_type, *types;
		size_t len;

		types = get_llvm_types(type->u.tuple.types);
		len = vec_len(type->u.tuple.types);
		tuple_type = LLVMStructType(types, len, false);
		free(types);
		return tuple_type;
	}
	case STRUCT_TYPE: {
		LLVMTypeRef struct_type, *types;
		size_t len;

		types = get_llvm_types(type->u.struct_.types);
		len = vec_len(type->u.struct_.types);
		struct_type = LLVMStructType(types, len, false);
		free(types);
		return struct_type;
	}
	case FUNC_TYPE: {
		LLVMTypeRef func_type, ret, *params;
		size_t nparams;

		ret = get_llvm_type(type->u.func.ret);
		params = get_llvm_types(type->u.func.params);
		nparams = vec_len(type->u.func.params);
		func_type = LLVMFunctionType(ret, params, nparams, false);
		free(params);
		return func_type;
	}
	case CONST_TYPE:
		return get_llvm_type(type->u.const_.type);
	case VOLATILE_TYPE: // TODO: Volatile code gen
		return get_llvm_type(type->u.volatile_.type);
	}
	internal_error();
}

static LLVMValueRef emit_expr(LLVMBuilderRef, struct expr *);

static LLVMValueRef emit_lval(LLVMBuilderRef builder, struct expr *expr)
{
	switch (expr->kind) {
	case UNARY_OP_EXPR: {
		struct expr *operand;

		assert(expr->u.unary_op.op == DEREF_OP);
		operand = expr->u.unary_op.operand;
		return emit_expr(builder, operand);
	}
	case IDENT_EXPR: {
		struct symbol_info *sym_info;
		char *name;

		name = expr->u.ident.name;
		sym_info = lookup_symbol(sym_tbl, name);
		assert(sym_info != NULL);
		assert(sym_info->is_ptr);
		assert(sym_info->val != NULL);
		return sym_info->val;
	}
	case FIELD_ACCESS_EXPR:
		internal_error(); // TODO: Stub
	default:
		internal_error();
	}
}

static LLVMValueRef emit_inc_or_dec_expr(LLVMBuilderRef builder,
		struct expr *expr)
{
	enum unary_op op = expr->u.unary_op.op;
	LLVMValueRef ptr_val = emit_lval(builder, expr->u.unary_op.operand);
	LLVMTypeRef type = get_llvm_type(expr->type);
	LLVMValueRef old_val, one_val, new_val;
	bool is_signed, is_inc, is_prefix;

	is_signed = !is_unsigned_int_type(expr->type);
	is_inc = (op == PRE_INC_OP || op == POST_INC_OP);
	is_prefix = (op == PRE_INC_OP || op == PRE_DEC_OP);
	old_val = LLVMBuildLoad(builder, ptr_val, "old_val");
	one_val = LLVMConstInt(type, 1, is_signed);
	if (is_inc) {
		new_val = LLVMBuildAdd(builder, old_val, one_val, "inc_val");
	} else {
		new_val = LLVMBuildSub(builder, old_val, one_val, "dec_val");
	}
	LLVMBuildStore(builder, new_val, ptr_val);
	if (is_prefix) {
		return new_val;
	} else {
		return old_val;
	}
}

static LLVMValueRef emit_unary_op_expr(LLVMBuilderRef builder,
		struct expr *expr)
{
	enum unary_op op = expr->u.unary_op.op;
	LLVMValueRef operand = emit_expr(builder, expr->u.unary_op.operand);
	LLVMTypeRef type = get_llvm_type(expr->type);
	bool is_const_expr = (builder == NULL);

	switch (op) {
	case NEG_OP:
		if (is_const_expr) {
			return LLVMConstNeg(operand);
		} else {
			return LLVMBuildNeg(builder, operand, "neg");
		}
	case PRE_INC_OP:
	case POST_INC_OP:
	case PRE_DEC_OP:
	case POST_DEC_OP:
		return emit_inc_or_dec_expr(builder, expr);
	case DEREF_OP:
		return LLVMBuildLoad(builder, operand, "loaded_val");
	case REF_OP:
		return operand;
	case BIT_NOT_OP:
		if (is_const_expr) {
			return LLVMConstNot(operand);
		} else {
			return LLVMBuildNot(builder, operand, "bit_not");
		}
	case LOG_NOT_OP: {
		LLVMValueRef zero_val;

		zero_val = LLVMConstInt(type, 0, false);
		if (is_const_expr) {
			return LLVMConstICmp(LLVMIntEQ, operand, zero_val);
		} else {
			return LLVMBuildICmp(builder, LLVMIntEQ, operand,
					zero_val, "not");
		}
	}
	}
	internal_error();
}

static LLVMValueRef emit_add(LLVMBuilderRef builder, LLVMValueRef l,
		LLVMValueRef r, struct type *type)
{
	bool is_const_expr = (builder == NULL);

	if (is_const_expr) {
		if (is_float_type(type)) {
			return LLVMConstFAdd(l, r);
		} else {
			return LLVMConstAdd(l, r);
		}
	} else {
		if (is_float_type(type)) {
			return LLVMBuildFAdd(builder, l, r, "sum");
		} else {
			return LLVMBuildAdd(builder, l, r, "sum");
		}
	}
}

static LLVMValueRef emit_sub(LLVMBuilderRef builder, LLVMValueRef l,
		LLVMValueRef r, struct type *type)
{
	bool is_const_expr = (builder == NULL);

	if (is_const_expr) {
		if (is_float_type(type)) {
			return LLVMConstFSub(l, r);
		} else {
			return LLVMConstSub(l, r);
		}
	} else {
		if (is_float_type(type)) {
			return LLVMBuildFSub(builder, l, r, "diff");
		} else {
			return LLVMBuildSub(builder, l, r, "diff");
		}
	}
}

static LLVMValueRef emit_mul(LLVMBuilderRef builder, LLVMValueRef l,
		LLVMValueRef r, struct type *type)
{
	bool is_const_expr = (builder == NULL);

	if (is_const_expr) {
		if (is_float_type(type)) {
			return LLVMConstFMul(l, r);
		} else {
			return LLVMConstMul(l, r);
		}
	} else {
		if (is_float_type(type)) {
			return LLVMBuildFMul(builder, l, r, "prod");
		} else {
			return LLVMBuildMul(builder, l, r, "prod");
		}
	}
}

static LLVMValueRef emit_div(LLVMBuilderRef builder, LLVMValueRef l,
		LLVMValueRef r, struct type *type)
{
	bool is_const_expr = (builder == NULL);

	if (is_const_expr) {
		if (is_float_type(type)) {
			return LLVMConstFDiv(l, r);
		} else if (is_unsigned_int_type(type)) {
			return LLVMConstUDiv(l, r);
		} else {
			return LLVMConstSDiv(l, r);
		}
	} else {
		if (is_float_type(type)) {
			return LLVMBuildFDiv(builder, l, r, "quot");
		} else if (is_unsigned_int_type(type)) {
			return LLVMBuildUDiv(builder, l, r, "quot");
		} else {
			return LLVMBuildSDiv(builder, l, r, "quot");
		}
	}
}

static LLVMValueRef emit_mod(LLVMBuilderRef builder, LLVMValueRef l,
		LLVMValueRef r, struct type *type)
{
	bool is_const_expr = (builder == NULL);

	if (is_const_expr) {
		if (is_float_type(type)) {
			return LLVMConstFRem(l, r);
		} else if (is_unsigned_int_type(type)) {
			return LLVMConstURem(l, r);
		} else {
			return LLVMConstSRem(l, r);
		}
	} else {
		if (is_float_type(type)) {
			return LLVMBuildFRem(builder, l, r, "rem");
		} else if (is_unsigned_int_type(type)) {
			return LLVMBuildURem(builder, l, r, "rem");
		} else {
			return LLVMBuildSRem(builder, l, r, "rem");
		}
	}
}

static LLVMValueRef emit_lt(LLVMBuilderRef builder, LLVMValueRef l,
		LLVMValueRef r, struct type *type)
{
	bool is_const_expr = (builder == NULL);

	if (is_const_expr) {
		if (is_float_type(type)) {
			return LLVMConstFCmp(LLVMRealOLT, l, r);
		} else if (is_unsigned_int_type(type)) {
			return LLVMConstICmp(LLVMIntULT, l, r);
		} else {
			return LLVMConstICmp(LLVMIntSLT, l, r);
		}
	} else {
		if (is_float_type(type)) {
			return LLVMBuildFCmp(builder, LLVMRealOLT, l, r, "lt");
		} else if (is_unsigned_int_type(type)) {
			return LLVMBuildICmp(builder, LLVMIntULT, l, r, "lt");
		} else {
			return LLVMBuildICmp(builder, LLVMIntSLT, l, r, "lt");
		}
	}
}

static LLVMValueRef emit_gt(LLVMBuilderRef builder, LLVMValueRef l,
		LLVMValueRef r, struct type *type)
{
	bool is_const_expr = (builder == NULL);

	if (is_const_expr) {
		if (is_float_type(type)) {
			return LLVMConstFCmp(LLVMRealOGT, l, r);
		} else if (is_unsigned_int_type(type)) {
			return LLVMConstICmp(LLVMIntUGT, l, r);
		} else {
			return LLVMConstICmp(LLVMIntSGT, l, r);
		}
	} else {
		if (is_float_type(type)) {
			return LLVMBuildFCmp(builder, LLVMRealOGT, l, r, "gt");
		} else if (is_unsigned_int_type(type)) {
			return LLVMBuildICmp(builder, LLVMIntUGT, l, r, "gt");
		} else {
			return LLVMBuildICmp(builder, LLVMIntSGT, l, r, "gt");
		}
	}
}

static LLVMValueRef emit_le(LLVMBuilderRef builder, LLVMValueRef l,
		LLVMValueRef r, struct type *type)
{
	bool is_const_expr = (builder == NULL);

	if (is_const_expr) {
		if (is_float_type(type)) {
			return LLVMConstFCmp(LLVMRealOLE, l, r);
		} else if (is_unsigned_int_type(type)) {
			return LLVMConstICmp(LLVMIntULE, l, r);
		} else {
			return LLVMConstICmp(LLVMIntSLE, l, r);
		}
	} else {
		if (is_float_type(type)) {
			return LLVMBuildFCmp(builder, LLVMRealOLE, l, r, "le");
		} else if (is_unsigned_int_type(type)) {
			return LLVMBuildICmp(builder, LLVMIntULE, l, r, "le");
		} else {
			return LLVMBuildICmp(builder, LLVMIntSLE, l, r, "le");
		}
	}
}

static LLVMValueRef emit_ge(LLVMBuilderRef builder, LLVMValueRef l,
		LLVMValueRef r, struct type *type)
{
	bool is_const_expr = (builder == NULL);

	if (is_const_expr) {
		if (is_float_type(type)) {
			return LLVMConstFCmp(LLVMRealOGE, l, r);
		} else if (is_unsigned_int_type(type)) {
			return LLVMConstICmp(LLVMIntUGE, l, r);
		} else {
			return LLVMConstICmp(LLVMIntSGE, l, r);
		}
	} else {
		if (is_float_type(type)) {
			return LLVMBuildFCmp(builder, LLVMRealOGE, l, r, "ge");
		} else if (is_unsigned_int_type(type)) {
			return LLVMBuildICmp(builder, LLVMIntUGE, l, r, "ge");
		} else {
			return LLVMBuildICmp(builder, LLVMIntSGE, l, r, "ge");
		}
	}
}

static LLVMValueRef emit_eq(LLVMBuilderRef builder, LLVMValueRef l,
		LLVMValueRef r, struct type *type)
{
	bool is_const_expr = (builder == NULL);

	if (is_const_expr) {
		if (is_float_type(type)) {
			return LLVMConstFCmp(LLVMRealOEQ, l, r);
		} else {
			return LLVMConstICmp(LLVMIntEQ, l, r);
		}
	} else {
		if (is_float_type(type)) {
			return LLVMBuildFCmp(builder, LLVMRealOEQ, l, r, "eq");
		} else {
			return LLVMBuildICmp(builder, LLVMIntEQ, l, r, "eq");
		}
	}
}

static LLVMValueRef emit_ne(LLVMBuilderRef builder, LLVMValueRef l,
		LLVMValueRef r, struct type *type)
{
	bool is_const_expr = (builder == NULL);

	if (is_const_expr) {
		if (is_float_type(type)) {
			return LLVMConstFCmp(LLVMRealONE, l, r);
		} else {
			return LLVMConstICmp(LLVMIntNE, l, r);
		}
	} else {
		if (is_float_type(type)) {
			return LLVMBuildFCmp(builder, LLVMRealONE, l, r, "ne");
		} else {
			return LLVMBuildICmp(builder, LLVMIntNE, l, r, "ne");
		}
	}
}

static bool is_assignment(enum bin_op op)
{
	switch (op) {
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
		return true;
	default:
		return false;
	}
}

/*
 * Promote an unsized integer to a sized integer of a target type. If the value
 * is not an unsized integer, emit nothing.
 */
static LLVMValueRef maybe_emit_int_promotion(LLVMBuilderRef builder,
		LLVMValueRef val, struct type *target_type,
		struct type *source_type)
{
	if (source_type->kind != UNSIZED_INT_TYPE) {
		return val;
	}
	assert(is_int_type(target_type));
	return LLVMBuildIntCast(builder, val, get_llvm_type(target_type),
			"promoted_int");
}

static LLVMValueRef emit_bin_op_expr(LLVMBuilderRef builder, struct expr *expr)
{
	LLVMValueRef l, r, old_val, new_val;
	struct expr *l_expr, *r_expr;
	struct type *type, *l_type, *r_type;
	enum bin_op op;
	bool is_const_expr;

	op = expr->u.bin_op.op;
	l_expr = expr->u.bin_op.l;
	r_expr = expr->u.bin_op.r;
	l_type = l_expr->type;
	r_type = r_expr->type;
	if (is_assignment(op)) {
		l = emit_lval(builder, l_expr);
	} else {
		l = emit_expr(builder, l_expr);
	}
	r = emit_expr(builder, r_expr);
	type = expr->type;
	is_const_expr = (builder == NULL);
	assert(is_assignment(op) ? !is_const_expr : true);

	l = maybe_emit_int_promotion(builder, l, r_type, l_type);
	r = maybe_emit_int_promotion(builder, r, l_type, r_type);
	switch (op) {
	case ADD_OP:
		return emit_add(builder, l, r, type);
	case SUB_OP:
		return emit_sub(builder, l, r, type);
	case MUL_OP:
		return emit_mul(builder, l, r, type);
	case DIV_OP:
		return emit_div(builder, l, r, type);
	case MOD_OP:
		return emit_mod(builder, l, r, type);
	case LT_OP:
		return emit_lt(builder, l, r, type);
	case GT_OP:
		return emit_gt(builder, l, r, type);
	case LT_EQ_OP:
		return emit_le(builder, l, r, type);
	case GT_EQ_OP:
		return emit_ge(builder, l, r, type);
	case EQ_OP:
		return emit_eq(builder, l, r, type);
	case NOT_EQ_OP:
		return emit_ne(builder, l, r, type);
	case BIT_AND_OP:
	case LOG_AND_OP:
		if (is_const_expr) {
			return LLVMConstAnd(l, r);
		} else {
			return LLVMBuildAnd(builder, l, r, "and");
		}
	case BIT_OR_OP:
	case LOG_OR_OP:
		if (is_const_expr) {
			return LLVMConstOr(l, r);
		} else {
			return LLVMBuildOr(builder, l, r, "or");
		}
	case BIT_XOR_OP:
		if (is_const_expr) {
			return LLVMConstXor(l, r);
		} else {
			return LLVMBuildXor(builder, l, r, "xor");
		}
	case BIT_SHIFT_L_OP:
		if (is_const_expr) {
			return LLVMConstShl(l, r);
		} else {
			return LLVMBuildShl(builder, l, r, "shl");
		}
	case BIT_SHIFT_R_OP:
		if (is_const_expr) {
			return LLVMConstLShr(l, r);
		} else {
			return LLVMBuildLShr(builder, l, r, "lshr");
		}
	case ASSIGN_OP:
		return LLVMBuildStore(builder, r, l);
	case ADD_ASSIGN_OP:
		old_val = LLVMBuildLoad(builder, l, "old_val");
		new_val = emit_add(builder, old_val, r, type);
		return LLVMBuildStore(builder, new_val, l);
	case SUB_ASSIGN_OP:
		old_val = LLVMBuildLoad(builder, l, "old_val");
		new_val = emit_sub(builder, old_val, r, type);
		return LLVMBuildStore(builder, new_val, l);
	case MUL_ASSIGN_OP:
		old_val = LLVMBuildLoad(builder, l, "old_val");
		new_val = emit_mul(builder, old_val, r, type);
		return LLVMBuildStore(builder, new_val, l);
	case DIV_ASSIGN_OP:
		old_val = LLVMBuildLoad(builder, l, "old_val");
		new_val = emit_div(builder, old_val, r, type);
		return LLVMBuildStore(builder, new_val, l);
	case MOD_ASSIGN_OP:
		old_val = LLVMBuildLoad(builder, l, "old_val");
		new_val = emit_mod(builder, old_val, r, type);
		return LLVMBuildStore(builder, new_val, l);
	case BIT_AND_ASSIGN_OP:
		old_val = LLVMBuildLoad(builder, l, "old_val");
		new_val = LLVMBuildAnd(builder, old_val, r, "and");
		return LLVMBuildStore(builder, new_val, l);
	case BIT_OR_ASSIGN_OP:
		old_val = LLVMBuildLoad(builder, l, "old_val");
		new_val = LLVMBuildOr(builder, old_val, r, "or");
		return LLVMBuildStore(builder, new_val, l);
	case BIT_XOR_ASSIGN_OP:
		old_val = LLVMBuildLoad(builder, l, "old_val");
		new_val = LLVMBuildXor(builder, old_val, r, "xor");
		return LLVMBuildStore(builder, new_val, l);
	case BIT_SHIFT_L_ASSIGN_OP:
		old_val = LLVMBuildLoad(builder, l, "old_val");
		new_val = LLVMBuildShl(builder, old_val, r, "shl");
		return LLVMBuildStore(builder, new_val, l);
	case BIT_SHIFT_R_ASSIGN_OP:
		old_val = LLVMBuildLoad(builder, l, "old_val");
		new_val = LLVMBuildLShr(builder, old_val, r, "lshr");
		return LLVMBuildStore(builder, new_val, l);
	}
	internal_error();
}

static LLVMValueRef emit_ident_expr(LLVMBuilderRef builder, struct expr *expr)
{
	struct symbol_info *sym_info;
	char *name;

	assert(expr->kind == IDENT_EXPR);
	name = expr->u.ident.name;
	sym_info = lookup_symbol(sym_tbl, name);
	assert(sym_info != NULL);
	assert(sym_info->val != NULL);
	assert(builder != NULL);
	if (sym_info->is_ptr) {
		return LLVMBuildLoad(builder, sym_info->val, "var_val");
	} else {
		return sym_info->val;
	}
}

static void emit_compound_stmt(LLVMBuilderRef, Vec *);

static LLVMValueRef emit_block_expr(LLVMBuilderRef builder, struct expr *expr)
{
	Vec *stmts;

	assert(expr->kind == BLOCK_EXPR);
	stmts = expr->u.block.stmts;

	enter_new_scope(sym_tbl);
	emit_compound_stmt(builder, stmts);
	leave_scope(sym_tbl);
	return NULL;
}

/*
 * The pointer returned from this function must be freed. It is okay to free
 * it after passing to an LLVM function.
 */
static LLVMValueRef *emit_exprs(LLVMBuilderRef builder, Vec *exprs)
{
	LLVMValueRef *llvm_exprs;
	size_t nexprs, i;

	nexprs = vec_len(exprs);
	llvm_exprs = xmalloc(sizeof(LLVMValueRef) * nexprs);
	for (i = 0; i < nexprs; i++) {
		llvm_exprs[i] = emit_expr(builder, vec_get(exprs, i));
	}
	return llvm_exprs;
}

static LLVMValueRef emit_func_call_expr(LLVMBuilderRef builder,
		struct expr *expr)
{
	LLVMValueRef call_val, func_val, *arg_vals;
	struct expr *arg, *func;
	struct type *param_type;
	Vec *args, *params;
	unsigned nargs, i;

	assert(expr->kind == FUNC_CALL_EXPR);
	args = expr->u.func_call.args;
	func = expr->u.func_call.func;
	assert(func->type->kind == FUNC_TYPE);
	params = func->type->u.func.params;
	nargs = vec_len(args);
	arg_vals = emit_exprs(builder, args);
	for (i = 0; i < nargs; i++) {
		arg = vec_get(args, i);
		param_type = vec_get(params, i);
		arg_vals[i] = maybe_emit_int_promotion(builder, arg_vals[i],
				param_type, arg->type);
	}
	func_val = emit_expr(builder, func);
	call_val = LLVMBuildCall(builder, func_val, arg_vals, nargs,
			"call_ret");
	free(arg_vals);
	return call_val;
}

static LLVMValueRef emit_expr(LLVMBuilderRef builder, struct expr *expr)
{
	LLVMTypeRef llvm_type = get_llvm_type(expr->type);

	switch (expr->kind) {
	case BOOL_LIT_EXPR: {
		bool val = expr->u.bool_lit.val;

		return LLVMConstInt(llvm_type, val, false);
	}
	case INT_LIT_EXPR: {
		uint64_t val = expr->u.int_lit.val;

		return LLVMConstInt(llvm_type, val, false);
	}
	case FLOAT_LIT_EXPR: {
		double val = expr->u.float_lit.val;

		return LLVMConstReal(llvm_type, val);
	}
	case CHAR_LIT_EXPR: {
		uint32_t val = expr->u.char_lit.val;

		return LLVMConstInt(llvm_type, val, false);
	}
	case STRING_LIT_EXPR: {
		char *val = expr->u.string_lit.val;
		unsigned len = expr->u.string_lit.len;

		return LLVMConstString(val, len, true);
	}
	case UNARY_OP_EXPR:
		return emit_unary_op_expr(builder, expr);
	case BIN_OP_EXPR:
		return emit_bin_op_expr(builder, expr);
	case LAMBDA_EXPR:
	case ARRAY_LIT_EXPR:
		internal_error(); // TODO: Stub
	case IDENT_EXPR:
		return emit_ident_expr(builder, expr);
	case BLOCK_EXPR:
		return emit_block_expr(builder, expr);
	case IF_EXPR:
	case SWITCH_EXPR:
	case TUPLE_EXPR:
		internal_error(); // TODO: Stub
	case FUNC_CALL_EXPR:
		return emit_func_call_expr(builder, expr);
	case FIELD_ACCESS_EXPR:
		internal_error(); // TODO: Stub
	}
	internal_error();
}

static LLVMValueRef emit_const_expr(struct expr *expr)
{
	return emit_expr(NULL, expr);
}

static void emit_global_data_decl(LLVMModuleRef module, struct decl *decl)
{
	bool is_let;
	LLVMTypeRef type;
	char *name;
	struct expr *init_expr;
	LLVMValueRef global, init;
	bool is_signed_int;

	assert(decl->kind == DATA_DECL);
	is_let = decl->u.data.is_let;
	type = get_llvm_type(decl->u.data.type);
	name = decl->u.data.name;
	init_expr = decl->u.data.init;
	is_signed_int = is_signed_int_type(decl->u.data.type);

	global = LLVMAddGlobal(module, type, name);
	init = emit_const_expr(init_expr);
	if (init_expr->type->kind == UNSIZED_INT_TYPE) {
		init = LLVMConstIntCast(init, type, is_signed_int);
	}
	LLVMSetInitializer(global, init);
	LLVMSetGlobalConstant(global, is_let);
	insert_symbol(sym_tbl, name, alloc_sym_info(true, global));
}

static void emit_local_data_decl(LLVMBuilderRef builder, struct decl *decl)
{
	LLVMTypeRef type;
	LLVMValueRef local_ptr;
	struct expr *init;
	char *name;

	assert(decl->kind == DATA_DECL);
	type = get_llvm_type(decl->u.data.type);
	name = decl->u.data.name;
	init = decl->u.data.init;
	local_ptr = LLVMBuildAlloca(builder, type, name);
	if (init != NULL) {
		LLVMBuildStore(builder, emit_expr(builder, init), local_ptr);
	}
	insert_symbol(sym_tbl, name, alloc_sym_info(true, local_ptr));
}

static LLVMValueRef get_cur_func(LLVMBuilderRef builder)
{
	return LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
}

static LLVMBasicBlockRef append_basic_block(LLVMBuilderRef builder,
		const char *name)
{
	return LLVMAppendBasicBlock(get_cur_func(builder), name);
}

static bool block_has_terminator(LLVMBasicBlockRef block)
{
	return LLVMGetBasicBlockTerminator(block) != NULL;
}

static bool cur_block_has_terminator(LLVMBuilderRef builder)
{
	LLVMBasicBlockRef cur_block;

	cur_block = LLVMGetInsertBlock(builder);
	return block_has_terminator(cur_block);
}

// Emits a branch unless it would be unreachable
static void maybe_emit_branch(LLVMBuilderRef builder,
		LLVMBasicBlockRef target_block)
{
	if (!cur_block_has_terminator(builder)) {
		LLVMBuildBr(builder, target_block);
	}
}

// Emits a conditional branch unless it would be unreachable
static void maybe_emit_cond_branch(LLVMBuilderRef builder,
		LLVMValueRef cond_val, LLVMBasicBlockRef then_block,
		LLVMBasicBlockRef else_block)
{
	if (!cur_block_has_terminator(builder)) {
		LLVMBuildCondBr(builder, cond_val, then_block, else_block);
	}
}

static void emit_if_stmt(LLVMBuilderRef builder, struct stmt *stmt)
{
	struct expr *cond = stmt->u.if_.cond;
	Vec *then_stmts = stmt->u.if_.then_stmts,
	    *else_stmts = stmt->u.if_.else_stmts;
	LLVMValueRef cond_val;
	LLVMBasicBlockRef then_block, else_block, merge_block;

	cond_val = emit_expr(builder, cond);
	then_block = append_basic_block(builder, "then");
	merge_block = append_basic_block(builder, "if.end");
	if (else_stmts == NULL) {
		else_block = merge_block;
	} else {
		else_block = append_basic_block(builder, "else");
	}
	maybe_emit_cond_branch(builder, cond_val, then_block, else_block);
	LLVMPositionBuilderAtEnd(builder, then_block);
	emit_compound_stmt(builder, then_stmts);
	maybe_emit_branch(builder, merge_block);
	if (else_stmts != NULL) {
		LLVMPositionBuilderAtEnd(builder, else_block);
		emit_compound_stmt(builder, else_stmts);
		maybe_emit_branch(builder, merge_block);
	}
	LLVMPositionBuilderAtEnd(builder, merge_block);
}

static void emit_do_stmt(LLVMBuilderRef builder, struct stmt *stmt)
{
	LLVMBasicBlockRef do_block, cont_block;
	LLVMValueRef cond_val;
	struct expr *cond;
	Vec *stmts;

	assert(stmt->kind == DO_STMT);
	cond = stmt->u.do_.cond;
	stmts = stmt->u.do_.stmts;
	do_block = append_basic_block(builder, "do.start");
	cont_block = append_basic_block(builder, "do.end");
	maybe_emit_branch(builder, do_block);
	LLVMPositionBuilderAtEnd(builder, do_block);
	enter_new_scope(sym_tbl);
	emit_compound_stmt(builder, stmts);
	cond_val = emit_expr(builder, cond);
	maybe_emit_cond_branch(builder, cond_val, do_block, cont_block);
	leave_scope(sym_tbl);
	LLVMPositionBuilderAtEnd(builder, cont_block);
}

static void emit_while_stmt(LLVMBuilderRef builder, struct stmt *stmt)
{
	LLVMBasicBlockRef cond_block, while_block, cont_block;
	LLVMValueRef cond_val;
	struct expr *cond;
	Vec *stmts;

	assert(stmt->kind == WHILE_STMT);
	cond = stmt->u.while_.cond;
	stmts = stmt->u.while_.stmts;
	cond_block = append_basic_block(builder, "while.cond");
	while_block = append_basic_block(builder, "while.start");
	cont_block = append_basic_block(builder, "while.end");
	maybe_emit_branch(builder, cond_block);
	LLVMPositionBuilderAtEnd(builder, cond_block);
	cond_val = emit_expr(builder, cond);
	maybe_emit_cond_branch(builder, cond_val, while_block, cont_block);
	LLVMPositionBuilderAtEnd(builder, while_block);
	enter_new_scope(sym_tbl);
	emit_compound_stmt(builder, stmts);
	leave_scope(sym_tbl);
	maybe_emit_branch(builder, cond_block);
	LLVMPositionBuilderAtEnd(builder, cont_block);
}

static void emit_for_stmt(LLVMBuilderRef builder, struct stmt *stmt)
{
	LLVMBasicBlockRef init_block, cond_block, post_block, for_block,
			  cont_block;
	LLVMValueRef cond_val;
	struct expr *init, *cond, *post;
	Vec *stmts;

	assert(stmt->kind == FOR_STMT);
	init = stmt->u.for_.init;
	cond = stmt->u.for_.cond;
	post = stmt->u.for_.post;
	stmts = stmt->u.for_.stmts;
	init_block = append_basic_block(builder, "for.init");
	cond_block = append_basic_block(builder, "for.cond");
	post_block = append_basic_block(builder, "for.post");
	for_block = append_basic_block(builder, "for.start");
	cont_block = append_basic_block(builder, "for.end");
	maybe_emit_branch(builder, init_block);
	LLVMPositionBuilderAtEnd(builder, init_block);
	emit_expr(builder, init);
	maybe_emit_branch(builder, cond_block);
	LLVMPositionBuilderAtEnd(builder, cond_block);
	cond_val = emit_expr(builder, cond);
	maybe_emit_cond_branch(builder, cond_val, for_block, cont_block);
	LLVMPositionBuilderAtEnd(builder, for_block);
	enter_new_scope(sym_tbl);
	emit_compound_stmt(builder, stmts);
	leave_scope(sym_tbl);
	maybe_emit_branch(builder, post_block);
	LLVMPositionBuilderAtEnd(builder, post_block);
	emit_expr(builder, post);
	maybe_emit_branch(builder, cond_block);
	LLVMPositionBuilderAtEnd(builder, cont_block);
}

static void emit_return_stmt(LLVMBuilderRef builder, struct stmt *stmt)
{
	struct expr *expr;

	expr = stmt->u.return_.expr;
	if (expr != NULL) {
		/*
		 * The value in `cur_func_return_val_ptr` will be returned in a
		 * block at the end of the function.  LLVM complains if the
		 * LLVMBuildRet is done here.
		 *
		 * TODO: Check this again
		 */
		LLVMBuildStore(builder, emit_expr(builder, expr),
				cur_func_return_val_ptr);
	}
	maybe_emit_branch(builder, cur_func_return_block);
}

static void emit_stmt(LLVMBuilderRef builder, struct stmt *stmt)
{
	switch (stmt->kind) {
	case DECL_STMT:
		emit_local_data_decl(builder, stmt->u.decl.decl);
		break;
	case EXPR_STMT:
		emit_expr(builder, stmt->u.expr.expr);
		break;
	case IF_STMT:
		emit_if_stmt(builder, stmt);
		break;
	case DO_STMT:
		emit_do_stmt(builder, stmt);
		break;
	case WHILE_STMT:
		emit_while_stmt(builder, stmt);
		break;
	case FOR_STMT:
		emit_for_stmt(builder, stmt);
		break;
	case RETURN_STMT:
		emit_return_stmt(builder, stmt);
		break;
	}
}

static void emit_compound_stmt(LLVMBuilderRef builder, Vec *stmts)
{
	size_t i;

	for (i = 0; i < vec_len(stmts); i++) {
		emit_stmt(builder, vec_get(stmts, i));
	}
}

// TODO: Add comments and maybe split this
static void emit_func_decl(LLVMModuleRef module, struct decl *decl)
{
	LLVMTypeRef func_type, llvm_param_type;
	LLVMValueRef func_val, param_val, param_ptr_val, return_val;
	LLVMBasicBlockRef entry_block, last_block;
	LLVMBuilderRef builder;
	struct type *return_type, *param_type;
	Vec *param_types, *param_names, *body_stmts;
	char *func_name, *param_name;
	size_t i;

	assert(decl->kind == FUNC_DECL);
	func_type = get_llvm_type(decl->u.func.type);
	func_name = decl->u.func.name;
	param_names = decl->u.func.param_names;
	body_stmts = decl->u.func.body_stmts;
	assert(decl->u.func.type->kind == FUNC_TYPE);
	return_type = decl->u.func.type->u.func.ret;
	param_types = decl->u.func.type->u.func.params;

	func_val = LLVMAddFunction(module, func_name, func_type);
	insert_symbol(sym_tbl, func_name, alloc_sym_info(false, func_val));
	cur_func_return_block = LLVMAppendBasicBlock(func_val, "return");
	builder = LLVMCreateBuilder();
	entry_block = LLVMAppendBasicBlock(func_val, "entry");
	LLVMPositionBuilderAtEnd(builder, entry_block);
	enter_new_scope(sym_tbl);
	for (i = 0; i < vec_len(param_names); i++) {
		param_type = vec_get(param_types, i);
		llvm_param_type = get_llvm_type(param_type);
		param_name = vec_get(param_names, i);
		param_ptr_val = LLVMBuildAlloca(builder, llvm_param_type,
				"param_ptr");
		param_val = LLVMGetParam(func_val, i);
		LLVMBuildStore(builder, param_val, param_ptr_val);
		insert_symbol(sym_tbl, param_name,
				alloc_sym_info(true, param_ptr_val));
	}
	if (return_type->kind != VOID_TYPE) {
		cur_func_return_val_ptr = LLVMBuildAlloca(builder,
				get_llvm_type(return_type), "return_val_ptr");
	}
	emit_compound_stmt(builder, body_stmts);
	last_block = LLVMGetLastBasicBlock(func_val);
	maybe_emit_branch(builder, cur_func_return_block);
	LLVMMoveBasicBlockAfter(cur_func_return_block, last_block);
	LLVMPositionBuilderAtEnd(builder, cur_func_return_block);
	if (return_type->kind == VOID_TYPE) {
		LLVMBuildRetVoid(builder);
	} else {
		return_val = LLVMBuildLoad(builder, cur_func_return_val_ptr,
				"return_val");
		LLVMBuildRet(builder, return_val);
	}
	LLVMDisposeBuilder(builder);
	leave_scope(sym_tbl);
}

static void emit_global_decl(LLVMModuleRef module, struct decl *decl)
{
	switch (decl->kind) {
	case DATA_DECL:
		emit_global_data_decl(module, decl);
		break;
	case TYPEDEF_DECL:
		internal_error(); // TODO: Stub
	case FUNC_DECL:
		emit_func_decl(module, decl);
		break;
	}
}

static LLVMModuleRef emit_ast(struct ast ast)
{
	LLVMModuleRef module;
	Vec *decls = ast.decls;
	size_t i;

	sym_tbl = alloc_symbol_table();
	enter_new_scope(sym_tbl); // Global scope
	module = LLVMModuleCreateWithName(get_filename());
	for (i = 0; i < vec_len(decls); i++) {
		emit_global_decl(module, vec_get(decls, i));
	}
	free_symbol_table(sym_tbl);
	return module;
}

static NORETURN void llvm_error(const char *errmsg)
{
	fprintf(stderr, "%s: LLVM error:\n%s\n", argv0, errmsg);
	exit(EXIT_FAILURE);
}

static void compile_module(char *target_file, LLVMModuleRef module)
{
	char *target_triplet;
	const char *cpu, *features;
	LLVMTargetRef target;
	bool failed;
	char *errmsg;
	LLVMTargetMachineRef target_machine;
#if 0
	LLVMTargetDataRef data_layout;
#endif
#if 0
	LLVMDumpModule(module);
#endif
	LLVMVerifyModule(module, LLVMAbortProcessAction, NULL);
	LLVMInitializeAllTargetInfos();
	LLVMInitializeAllTargets();
	LLVMInitializeAllTargetMCs();
	LLVMInitializeAllAsmParsers();
	LLVMInitializeAllAsmPrinters();
	target_triplet = LLVMGetDefaultTargetTriple();
	failed = LLVMGetTargetFromTriple(target_triplet, &target, &errmsg);
	if (failed) {
		llvm_error(errmsg);
	}
	cpu = "generic";
	features = "";
	target_machine = LLVMCreateTargetMachine(target, target_triplet, cpu,
			features, LLVMCodeGenLevelDefault, LLVMRelocDefault,
			LLVMCodeModelDefault);
#if 0
TODO: Add data layout to module
	data_layout = LLVMCreateDataLayout(target_machine);
	LLVMSetDataLayout(module, data_layout);
#endif
	LLVMSetTarget(module, target_triplet);
	failed = LLVMTargetMachineEmitToFile(target_machine, module,
			target_file, LLVMObjectFile, &errmsg);
	if (failed) {
		llvm_error(errmsg);
	}
	LLVMDisposeTargetMachine(target_machine);
	LLVMDisposeMessage(target_triplet);
}

void compile_ast(char *target_file, struct ast ast)
{
	LLVMModuleRef module;

	module = emit_ast(ast);
	compile_module(target_file, module);
	LLVMDisposeModule(module);
}
