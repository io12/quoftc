// Functions for freeing and duplicating AST nodes
/*
 * TODO: Change vectors to not store function pointers, which would allow more
 * of these functions to be `static`
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "ds.h"
#include "quoftc.h"
#include "ast.h"

/*
 * These functions take and return void pointers because they are passed to
 * `alloc_vec()` and `dup_vec()`.
 */

#if 0
// TODO: Unneeded?
static void *void_strdup(void *p)
{
	return xstrdup(p);
}
#endif

static Vec *dup_types(Vec *types)
{
	return dup_vec(types, dup_type);
}

static AliasType *dup_alias_type(AliasType *alias_type)
{
	return ALLOC_ALIAS_TYPE(alias_type->h.lineno,
			xstrdup(alias_type->name));
}

static ParamType *dup_param_type(ParamType *param_type)
{
	return ALLOC_PARAM_TYPE(param_type->h.lineno,
			xstrdup(param_type->name),
			dup_types(param_type->params));
}

static ArrayType *dup_array_type(ArrayType *array_type)
{
	return ALLOC_ARRAY_TYPE(array_type->h.lineno,
			dup_type(array_type->item_type), array_type->len);
}

static PointerType *dup_pointer_type(PointerType *pointer_type)
{
	return ALLOC_POINTER_TYPE(pointer_type->h.lineno,
			dup_type(pointer_type->pointee_type));
}

static TupleType *dup_tuple_type(TupleType *tuple_type)
{
	return ALLOC_TUPLE_TYPE(tuple_type->h.lineno,
			dup_types(tuple_type->members));
}

static void *dup_struct_member(void *p)
{
	StructMember *struct_member = p;

	return ALLOC_STRUCT_MEMBER(dup_type(struct_member->type),
			xstrdup(struct_member->name));
}

static StructType *dup_struct_type(StructType *struct_type)
{
	return ALLOC_STRUCT_TYPE(struct_type->h.lineno,
			dup_vec(struct_type->members, dup_struct_member));
}

static FuncType *dup_func_type(FuncType *func_type)
{
	return ALLOC_FUNC_TYPE(func_type->h.lineno,
			dup_type(func_type->return_type),
			dup_types(func_type->param_types));
}

static ConstType *dup_const_type(ConstType *const_type)
{
	return ALLOC_CONST_TYPE(const_type->h.lineno,
			dup_type(const_type->subtype));
}

static VolatileType *dup_volatile_type(VolatileType *volatile_type)
{
	return ALLOC_VOLATILE_TYPE(volatile_type->h.lineno,
			dup_type(volatile_type->subtype));
}

void *dup_type(void *p)
{
	Type *type = p;

	switch(type->h.kind) {
	case UNSIZED_INT_TYPE:
		return ALLOC_UNSIZED_INT_TYPE(type->h.lineno);
	case U8_TYPE:
		return ALLOC_U8_TYPE(type->h.lineno);
	case U16_TYPE:
		return ALLOC_U16_TYPE(type->h.lineno);
	case U32_TYPE:
		return ALLOC_U32_TYPE(type->h.lineno);
	case U64_TYPE:
		return ALLOC_U64_TYPE(type->h.lineno);
	case I8_TYPE:
		return ALLOC_I8_TYPE(type->h.lineno);
	case I16_TYPE:
		return ALLOC_I16_TYPE(type->h.lineno);
	case I32_TYPE:
		return ALLOC_I32_TYPE(type->h.lineno);
	case I64_TYPE:
		return ALLOC_I64_TYPE(type->h.lineno);
	case F32_TYPE:
		return ALLOC_F32_TYPE(type->h.lineno);
	case F64_TYPE:
		return ALLOC_F64_TYPE(type->h.lineno);
	case BOOL_TYPE:
		return ALLOC_BOOL_TYPE(type->h.lineno);
	case VOID_TYPE:
		return ALLOC_VOID_TYPE(type->h.lineno);
	case CHAR_TYPE:
		return ALLOC_CHAR_TYPE(type->h.lineno);
	case ALIAS_TYPE:
		return dup_alias_type((AliasType *) type);
	case PARAM_TYPE:
		return dup_param_type((ParamType *) type);
	case ARRAY_TYPE:
		return dup_array_type((ArrayType *) type);
	case POINTER_TYPE:
		return dup_pointer_type((PointerType *) type);
	case TUPLE_TYPE:
		return dup_tuple_type((TupleType *) type);
	case STRUCT_TYPE:
		return dup_struct_type((StructType *) type);
	case FUNC_TYPE:
		return dup_func_type((FuncType *) type);
	case CONST_TYPE:
		return dup_const_type((ConstType *) type);
	case VOLATILE_TYPE:
		return dup_volatile_type((VolatileType *) type);
	}
	INTERNAL_ERROR();
}

void free_type(void *p)
{
	Type *type = p;

	if (type == NULL) {
		return;
	}
	switch (type->h.kind) {
	case UNSIZED_INT_TYPE:
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
		break;
	case ALIAS_TYPE: {
		AliasType *alias_type = (AliasType *) type;
		free(alias_type->name);
		break;
	}
	case PARAM_TYPE: {
		ParamType *param_type = (ParamType *) type;
		free(param_type->name);
		free_vec(param_type->params);
		break;
	}
	case ARRAY_TYPE: {
		ArrayType *array_type = (ArrayType *) type;
		free_type(array_type->item_type);
		break;
	}
	case POINTER_TYPE: {
		PointerType *pointer_type = (PointerType *) type;
		free_type(pointer_type->pointee_type);
		break;
	}
	case TUPLE_TYPE: {
		TupleType *tuple_type = (TupleType *) type;
		free_vec(tuple_type->members);
		break;
	}
	case STRUCT_TYPE: {
		StructType *struct_type = (StructType *) type;
		free_vec(struct_type->members);
		break;
	}
	case FUNC_TYPE: {
		FuncType *func_type = (FuncType *) type;
		free_type(func_type->return_type);
		free_vec(func_type->param_types);
		break;
	}
	case CONST_TYPE: {
		ConstType *const_type = (ConstType *) type;
		free_type(const_type->subtype);
		break;
	}
	case VOLATILE_TYPE: {
		VolatileType *volatile_type = (VolatileType *) type;
		free_type(volatile_type->subtype);
		break;
	}
	}
	free(type);
}

static void free_stmt_block(StmtBlock *block)
{
	if (block == NULL) {
		return;
	}
	free_vec(block->stmts);
	free(block);
}

void free_expr(void *p)
{
	Expr *expr = p;

	if (expr == NULL) {
		return;
	}
	switch (expr->h.kind) {
	case BOOL_LIT_EXPR:
	case INT_LIT_EXPR:
	case FLOAT_LIT_EXPR:
	case CHAR_LIT_EXPR:
		break;
	case STRING_LIT_EXPR: {
		StringLitExpr *string_lit_expr = (StringLitExpr *) expr;
		free(string_lit_expr->val);
		break;
	}
	case UNARY_OP_EXPR: {
		UnaryOpExpr *unary_op_expr = (UnaryOpExpr *) expr;
		free_expr(unary_op_expr->operand);
		break;
	}
	case BIN_OP_EXPR: {
		BinOpExpr *bin_op_expr = (BinOpExpr *) expr;
		free_expr(bin_op_expr->l);
		free_expr(bin_op_expr->r);
		break;
	}
	case LAMBDA_EXPR: {
		LambdaExpr *lambda_expr = (LambdaExpr *) expr;
		free_vec(lambda_expr->param_names);
		free_expr(lambda_expr->body);
		break;
	}
	case ARRAY_LIT_EXPR: {
		ArrayLitExpr *array_lit_expr = (ArrayLitExpr *) expr;
		free_vec(array_lit_expr->subexprs);
		break;
	}
	case IDENT_EXPR: {
		IdentExpr *ident_expr = (IdentExpr *) expr;
		free(ident_expr->name);
		break;
	}
	case BLOCK_EXPR: {
		BlockExpr *block_expr = (BlockExpr *) expr;
		free_stmt_block(block_expr->block);
		break;
	}
	case IF_EXPR: {
		IfExpr *if_expr = (IfExpr *) expr;
		free_expr(if_expr->cond);
		free_expr(if_expr->then);
		free_expr(if_expr->else_);
		break;
	}
	case SWITCH_EXPR: {
		SwitchExpr *switch_expr = (SwitchExpr *) expr;
		free_expr(switch_expr->ctrl);
		free_vec(switch_expr->cases);
		break;
	}
	case TUPLE_EXPR: {
		TupleExpr *tuple_expr = (TupleExpr *) expr;
		free_vec(tuple_expr->subexprs);
		break;
	}
	case FUNC_CALL_EXPR: {
		FuncCallExpr *func_call_expr = (FuncCallExpr *) expr;
		free_expr(func_call_expr->func);
		free_vec(func_call_expr->args);
		break;
	}
	case FIELD_ACCESS_EXPR: {
		FieldAccessExpr *field_access_expr = (FieldAccessExpr *) expr;
		free_expr(field_access_expr->parent);
		free(field_access_expr->field);
		break;
	}
	}
	free_type(expr->h.type); // TODO: Is this safe?
	free(expr);
}

void free_switch_pattern(void *p)
{
	SwitchPattern *sp = p;

	if (sp == NULL) {
		return;
	}
	switch (sp->h.kind) {
	case UNDERSCORE_SWITCH_PATTERN:
		break;
	case OR_SWITCH_PATTERN: {
		OrSwitchPattern *or_switch_pattern = (OrSwitchPattern *) sp;
		free_vec(or_switch_pattern->subpatterns);
		break;
	}
	case ARRAY_SWITCH_PATTERN: {
		ArraySwitchPattern *array_switch_pattern =
			(ArraySwitchPattern *) sp;
		free_vec(array_switch_pattern->subpatterns);
		break;
	}
	case TUPLE_SWITCH_PATTERN: {
		TupleSwitchPattern *tuple_switch_pattern =
			(TupleSwitchPattern *) sp;
		free_vec(tuple_switch_pattern->subpatterns);
		break;
	}
	case EXPR_SWITCH_PATTERN: {
		ExprSwitchPattern *expr_switch_pattern =
			(ExprSwitchPattern *) sp;
		free_expr(expr_switch_pattern->expr);
		break;
	}
	}
	free(sp);
}

void free_switch_case(void *p)
{
	SwitchCase *sc = p;

	if (sc == NULL) {
		return;
	}
	free_switch_pattern(sc->l);
	free_expr(sc->r);
	free(sc);
}

void free_decl(void *p)
{
	Decl *decl = p;

	if (decl == NULL) {
		return;
	}
	switch (decl->h.kind) {
	case DATA_DECL: {
		DataDecl *data_decl = (DataDecl *) decl;
		free_type(data_decl->type);
		free(data_decl->name);
		free_expr(data_decl->init);
		break;
	}
	case TYPEDEF_DECL: {
		TypedefDecl *typedef_decl = (TypedefDecl *) decl;
		free(typedef_decl->name);
		free_vec(typedef_decl->params);
		free_type(typedef_decl->type);
		break;
	}
	case FUNC_DECL: {
		FuncDecl *func_decl = (FuncDecl *) decl;
		free_type(func_decl->type);
		free(func_decl->name);
		free_vec(func_decl->param_names);
		free_stmt_block(func_decl->body);
		break;
	}
	}
	free(decl);
}

void free_stmt(void *p)
{
	Stmt *stmt = p;

	if (stmt == NULL) {
		return;
	}
	switch (stmt->h.kind) {
	case DECL_STMT: {
		DeclStmt *decl_stmt = (DeclStmt *) stmt;
		free_decl(decl_stmt->decl);
		break;
	}
	case EXPR_STMT: {
		ExprStmt *expr_stmt = (ExprStmt *) stmt;
		free_expr(expr_stmt->expr);
		break;
	}
	case IF_STMT: {
		IfStmt *if_stmt = (IfStmt *) stmt;
		free_expr(if_stmt->cond);
		free_stmt_block(if_stmt->then_block);
		free_stmt_block(if_stmt->else_block);
		break;
	}
	case DO_STMT: {
		DoStmt *do_stmt = (DoStmt *) stmt;
		free_stmt_block(do_stmt->block);
		free_expr(do_stmt->cond);
		break;
	}
	case WHILE_STMT: {
		WhileStmt *while_stmt = (WhileStmt *) stmt;
		free_stmt_block(while_stmt->block);
		free_expr(while_stmt->cond);
		break;
	}
	case FOR_STMT: {
		ForStmt *for_stmt = (ForStmt *) stmt;
		free_expr(for_stmt->init);
		free_expr(for_stmt->cond);
		free_expr(for_stmt->post);
		free_stmt_block(for_stmt->block);
		break;
	}
	case RETURN_STMT: {
		ReturnStmt *return_stmt = (ReturnStmt *) stmt;
		free_expr(return_stmt->expr);
		break;
	}
	}
	free(stmt);
}

void free_ast(Ast ast)
{
	free_vec(ast.decls);
}
