#include <stdbool.h>
#include <stddef.h>
#include <llvm-c/Core.h>
#include "ds.h"
#include "ast.h"
#include "check_semantics.h"
#include "lex.h"
#include "quoftc.h"
#include "symbol_table.h"
#include "emit.h"

static LLVMModuleRef module;
static struct symbol_table sym_tbl;

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
	case ARRAY_TYPE:
		//return LLVMArrayType(get_llvm_type(type->u.array.l),
		///* TODO: eval len */);
	case POINTER_TYPE:
		return LLVMPointerType(get_llvm_type(type->u.pointer.l), 0);
	case TUPLE_TYPE: {
		Vec *types = type->u.tuple.types;
		size_t len = vec_len(types), i;
		LLVMTypeRef *llvm_types = xmalloc(sizeof(LLVMTypeRef) * len);

		for (i = 0; i < len; i++) {
			llvm_types[i] = get_llvm_type(vec_get(types, i));
		}
		return LLVMStructType(llvm_types, len, false);
	}
	case FUNC_TYPE: {
		Vec *params = type->u.func.params;
		size_t len = vec_len(params);
		LLVMTypeRef *llvm_params = xmalloc(sizeof(LLVMTypeRef) * len);

		return LLVMFunctionType(get_llvm_type(type->u.func.ret),
				llvm_params, len, false);
	}
	}
	internal_error();
}

static LLVMValueRef emit_expr(LLVMBuilderRef, struct expr *);

static LLVMValueRef emit_inc_or_dec_expr(LLVMBuilderRef builder,
		struct expr *expr)
{
	enum unary_op op = expr->u.unary_op.op;
	LLVMValueRef ptr_val = emit_expr(builder, expr->u.unary_op.operand);
	LLVMTypeRef type = get_llvm_type(expr->type);
	LLVMValueRef old_val, one_val, new_val;
	bool is_signed, is_inc, is_prefix;

	is_signed = !is_unsigned_int_type(expr->type);
	is_inc = (op == PRE_INC_OP || op == POST_INC_OP);
	is_prefix = (op == PRE_INC_OP || op == PRE_DEC_OP);
	old_val = LLVMBuildLoad(builder, ptr_val, "inc_or_dec_load");
	one_val = LLVMConstInt(type, 1, is_signed);
	if (is_inc) {
		new_val = LLVMBuildAdd(builder, old_val, one_val, "inc_add");
	} else {
		new_val = LLVMBuildSub(builder, old_val, one_val, "dec_add");
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
	LLVMValueRef operand =
		emit_expr(builder, expr->u.unary_op.operand);
	LLVMTypeRef type = get_llvm_type(expr->type);

	switch (op) {
	case PRE_INC_OP:
	case POST_INC_OP:
	case PRE_DEC_OP:
	case POST_DEC_OP:
		return emit_inc_or_dec_expr(builder, expr);
	case DEREF_OP:
		return LLVMBuildLoad(builder, operand, "deref_load");
	case REF_OP:
		return operand;
	case BIT_NOT_OP:
		return LLVMBuildNot(builder, operand, "bitwise_not");
	case LOG_NOT_OP: {
		LLVMValueRef zero_val;

		zero_val = LLVMConstInt(type, 0, false);
		return LLVMBuildICmp(builder, LLVMIntEQ, operand, zero_val,
				"logical_not");
	}
	}
	internal_error();
}

static LLVMValueRef emit_bin_op_expr(LLVMBuilderRef builder, struct expr *expr)
{
	enum bin_op op = expr->u.bin_op.op;
	LLVMValueRef l = emit_expr(builder, expr->u.bin_op.l),
	             r = emit_expr(builder, expr->u.bin_op.r);

	switch (op) {
	case ADD_OP:
		return LLVMBuildAdd(builder, l, r, "add");
	case SUB_OP:
		return LLVMBuildSub(builder, l, r, "sub");
	case MUL_OP:
		return LLVMBuildMul(builder, l, r, "mul");
	case DIV_OP:
		if (is_unsigned_int_type(expr->type)) {
			return LLVMBuildUDiv(builder, l, r, "div");
		} else {
			return LLVMBuildSDiv(builder, l, r, "div");
		}
	case MOD_OP:
		if (is_unsigned_int_type(expr->type)) {
			return LLVMBuildURem(builder, l, r, "mod");
		} else {
			return LLVMBuildSRem(builder, l, r, "mod");
		}
	default:
		return NULL; // TODO: More ops
	}
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
		uint64_t len = expr->u.string_lit.len;

		// TODO: This function accepts `unsigned`
		return LLVMConstString(val, len, true);
	}
	case UNARY_OP_EXPR:
		return emit_unary_op_expr(builder, expr);
	case BIN_OP_EXPR:
		return emit_bin_op_expr(builder, expr);
	case LAMBDA_EXPR:
	case ARRAY_LIT_EXPR:
	case IDENT_EXPR:
	case BLOCK_EXPR:
	case IF_EXPR:
	case SWITCH_EXPR:
	case TUPLE_EXPR:
	default:
		return NULL; // TODO: Stub
	}
}

static void emit_global_val(bool is_const, struct type *type, const char *name,
		struct expr *init)
{
	LLVMValueRef global;
	LLVMBuilderRef builder;
	
	global = LLVMAddGlobal(module, get_llvm_type(type), name);
	builder = LLVMCreateBuilder();
	LLVMSetInitializer(global, emit_expr(builder, init));
	LLVMSetGlobalConstant(global, is_const);
}

static void emit_local_val(LLVMBuilderRef builder, struct type *type,
		char *name, struct expr *init)
{
	LLVMValueRef local_ptr;

	local_ptr = LLVMBuildAlloca(builder, get_llvm_type(type), name);
	if (init != NULL) {
		LLVMBuildStore(builder, emit_expr(builder, init), local_ptr);
	}
	insert_symbol(sym_tbl, name, local_ptr);
}

static LLVMValueRef get_current_func(LLVMBuilderRef builder)
{
	return LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
}

static void emit_stmts(LLVMBuilderRef, Vec *);

static void emit_if_stmt(LLVMBuilderRef builder, struct stmt *stmt)
{
	struct expr *cond = stmt->u.if_.cond;
	Vec *then_stmts = stmt->u.if_.then_stmts,
	    *else_stmts = stmt->u.if_.else_stmts;
	LLVMValueRef func_val, cond_val;
	LLVMBasicBlockRef then_block, else_block, merge_block;

	func_val = get_current_func(builder);
	cond_val = emit_expr(builder, cond);
	then_block = LLVMAppendBasicBlock(func_val, "then");
	else_block = LLVMAppendBasicBlock(func_val, "else");
	merge_block = LLVMAppendBasicBlock(func_val, "merge");
	LLVMBuildCondBr(builder, cond_val, then_block, else_block);
	LLVMPositionBuilderAtEnd(builder, then_block);
	emit_stmts(builder, then_stmts);
	LLVMBuildBr(builder, merge_block);
	LLVMPositionBuilderAtEnd(builder, else_block);
	emit_stmts(builder, else_stmts);
	LLVMBuildBr(builder, merge_block);
	LLVMPositionBuilderAtEnd(builder, merge_block);
}

static void emit_do_stmt(LLVMBuilderRef builder, struct stmt *stmt)
{
	Vec *stmts = stmt->u.do_.stmts;
	struct expr *cond = stmt->u.do_.cond;
	LLVMValueRef func_val, cond_val;
	LLVMBasicBlockRef do_block, after_do_block;

	func_val = get_current_func(builder);
	cond_val = emit_expr(builder, cond);
	do_block = LLVMAppendBasicBlock(func_val, "do");
	after_do_block = LLVMAppendBasicBlock(func_val, "after_do");
	LLVMPositionBuilderAtEnd(builder, do_block);
	emit_stmts(builder, stmts);
	LLVMBuildCondBr(builder, cond_val, do_block, after_do_block);
	LLVMPositionBuilderAtEnd(builder, after_do_block);
}

static void emit_stmt(LLVMBuilderRef, struct stmt *);

static void emit_stmts(LLVMBuilderRef builder, Vec *stmts)
{
	size_t i;

	for (i = 0; i < vec_len(stmts); i++) {
		emit_stmt(builder, vec_get(stmts, i));
	}
}

static void emit_stmt(LLVMBuilderRef builder, struct stmt *stmt)
{
	switch (stmt->kind) {
	case DECL_STMT: {
		struct decl *decl = stmt->u.decl.decl;

		emit_local_val(builder, decl->type, decl->name, decl->init);
		break;
	}
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
		// TODO: emit_while_stmt(builder, stmt);
		break;
	case FOR_STMT:
		// TODO: emit_for_stmt(builder, stmt);
		break;
	}
}

static void emit_func(struct type *type, const char *name, struct expr *expr)
{
	Vec *param_names = expr->u.lambda.params,
	    *body_stmts = expr->u.lambda.body_stmts;
	LLVMValueRef func_val;
	LLVMBasicBlockRef block;
	LLVMBuilderRef builder;
	size_t i;
	char *param_name;
	LLVMValueRef param_val;

	func_val = LLVMAddFunction(module, name, get_llvm_type(type));
	block = LLVMAppendBasicBlock(func_val, name);
	builder = LLVMCreateBuilder();
	LLVMPositionBuilderAtEnd(builder, block);
	enter_new_scope(sym_tbl);
	for (i = 0; i < vec_len(param_names); i++) {
		param_name = vec_get(param_names, i);
		param_val = LLVMGetParam(func_val, i);
		insert_symbol(sym_tbl, param_name, param_val);
	}
	for (i = 0; i < vec_len(body_stmts); i++) {
		emit_stmt(builder, vec_get(body_stmts, i));
	}
	leave_scope(sym_tbl);
}

static void emit_global_decl(struct decl *decl)
{
	switch (decl->type->kind) {
	case UNSIZED_INT_TYPE:
	case VOID_TYPE:
		internal_error();
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
	case ALIAS_TYPE:
	case PARAM_TYPE:
	case ARRAY_TYPE:
	case POINTER_TYPE:
	case TUPLE_TYPE:
		emit_global_val(decl->is_const, decl->type, decl->name,
				decl->init);
		break;
	case FUNC_TYPE:
		emit_func(decl->type, decl->name, decl->init);
		break;
	}
}

void emit(struct ast ast)
{
	Vec *decls = ast.decls;
	size_t i;

	sym_tbl = alloc_symbol_table();
	enter_new_scope(sym_tbl); // Global scope
	module = LLVMModuleCreateWithName(get_filename());
	for (i = 0; i < vec_len(decls); i++) {
		emit_global_decl(vec_get(decls, i));
	}
	free_symbol_table(sym_tbl);
}
