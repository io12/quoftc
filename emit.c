#include <stdbool.h>
#include <stddef.h>
#include <llvm-c/Core.h>
#include "ds.h"
#include "ast.h"
#include "lex.h"
#include "quoftc.h"
#include "emit.h"

static LLVMModuleRef module;

static LLVMValueRef emit_expr(struct expr *expr)
{
#if 0
	switch (expr->kind) {
	case BOOL_LIT_EXPR:
	case INT_LIT_EXPR:
	case FLOAT_LIT_EXPR:
	case CHAR_LIT_EXPR:
	case STRING_LIT_EXPR:
	case UNARY_OP_EXPR:
	case BIN_OP_EXPR: {
		LLVMValueRef l = emit_expr(expr->u.bin_op.l),
		             r = emit_expr(expr->u.bin_op.r);

		switch (expr->u.bin_op.op) {
		case '+':
			return LLVMBuildAdd(llvm_builder, l, r, "add");
		case '-':
			return LLVMBuildSub(llvm_builder, l, r, "sub");
		case '*':
			return LLVMBuildMul(llvm_builder, l, r, "mul");
		case '/':
			return LLVMBuildDiv(llvm_builder, l, r, "div");
		case '%':
			return LLVMBuildMod(llvm_builder, l, r, "mod");
		case '<':
			return LLVMBuildMod(llvm_builder, LLVMRealULT, l, r, "mod");
		}
	}
	case LAMBDA_EXPR:
	case ARRAY_LIT_EXPR:
	case IDENT_EXPR:
	case BLOCK_EXPR:
	case IF_EXPR:
	case SWITCH_EXPR:
	case TUPLE_EXPR:
	}
#endif
	(void) expr;
	return NULL;
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
	case ARRAY_TYPE:
		//return LLVMArrayType(get_llvm_type(type->u.array.l),
		///* TODO: eval len */);
	case POINTER_TYPE:
		return LLVMPointerType(get_llvm_type(type->u.pointer.l), 0);
	case TUPLE_TYPE: {
		Vec *types = type->u.tuple.types;
		size_t len = vec_len(types), i;
		LLVMTypeRef *llvm_types = emalloc(sizeof(LLVMTypeRef) * len);

		for (i = 0; i < len; i++) {
			llvm_types[i] = get_llvm_type(vec_get(types, i));
		}
		return LLVMStructType(llvm_types, len, false);
	}
	case FUNC_TYPE: {
		Vec *params = type->u.func.params;
		size_t len = vec_len(params);
		LLVMTypeRef *llvm_params = emalloc(sizeof(LLVMTypeRef) * len);

		return LLVMFunctionType(get_llvm_type(type->u.func.ret),
				llvm_params, len, false);
	}
	}
	internal_error();
}

static void emit_global_val(bool is_const, struct type *type, const char *name,
		struct expr *init)
{
	LLVMValueRef global;
	
	global = LLVMAddGlobal(module, get_llvm_type(type), name);
	LLVMSetInitializer(global, emit_expr(init));
	LLVMSetGlobalConstant(global, is_const);
}

#if 0
static void emit_local_val(LLVMBuilderRef builder, struct type *type,
		const char *name, struct expr *init)
{
	LLVMValueRef local_ptr;

	local_ptr = LLVMBuildAlloca(builder, get_llvm_type(type), name);
	if (init != NULL) {
		LLVMBuildStore(builder, emit_expr(init), local_ptr);
	}
}
#endif

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
	cond_val = emit_expr(cond);
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
	cond_val = emit_expr(cond);
	do_block = LLVMAppendBasicBlock(func_val, "do");
	after_do_block = LLVMAppendBasicBlock(func_val, "after_do");
	LLVMPositionBuilderAtEnd(builder, do_block);
	emit_stmts(builder, stmts);
	LLVMBuildCondBr(builder, cond_val, do_block, after_do_block);
	LLVMPositionBuilderAtEnd(builder, after_do_block);
}

static void emit_stmt(LLVMBuilderRef builder, struct stmt *stmt)
{
	switch (stmt->kind) {
	case DECL_STMT:
		// TODO: emit_decl(LOCAL_SCOPE, stmt->u.decl.decl);
		break;
	case EXPR_STMT:
		emit_expr(stmt->u.expr.expr);
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

static void emit_stmts(LLVMBuilderRef builder, Vec *stmts)
{
	size_t i;

	for (i = 0; i < vec_len(stmts); i++) {
		emit_stmt(builder, vec_get(stmts, i));
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

	(void) param_names; // TODO: Use this
	func_val = LLVMAddFunction(module, name, get_llvm_type(type));
	block = LLVMAppendBasicBlock(func_val, name);
	builder = LLVMCreateBuilder();
	LLVMPositionBuilderAtEnd(builder, block);
	for (i = 0; i < vec_len(body_stmts); i++) {
		emit_stmt(builder, vec_get(body_stmts, i));
	}
}

enum scope { LOCAL_SCOPE, GLOBAL_SCOPE };

static void emit_decl(enum scope scope, struct decl *decl)
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
		switch (scope) {
		case LOCAL_SCOPE:
			// TODO: emit_local_val();
			break;
		case GLOBAL_SCOPE:
			emit_global_val(decl->is_const, decl->type, decl->name,
					decl->init);
			break;
		}
		break;
	case FUNC_TYPE:
		if (scope == LOCAL_SCOPE) {
			internal_error();
		}
		emit_func(decl->type, decl->name, decl->init);
		break;
	}
}

void emit(struct ast ast)
{
	Vec *decls = ast.decls;
	size_t i;

	module = LLVMModuleCreateWithName(get_filename());
	for (i = 0; i < vec_len(decls); i++) {
		emit_decl(GLOBAL_SCOPE, vec_get(decls, i));
	}
}
