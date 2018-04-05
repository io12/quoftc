// Functions for freeing and duplicating AST nodes

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

static void *void_strdup(void *p)
{
	return xstrdup(p);
}

static AliasType *dup_alias_type(AliasType *alias_type)
{
	return ALLOC_ALIAS_TYPE(xstrdup(alias_type->name));
}

static ParamType *dup_param_type(ParamType *param_type)
{
	return ALLOC_ALIAS_TYPE(xstrdup(param_type->name), dup_vec(param_type->params));
}

static ArrayType *dup_array_type(ArrayType *array_type)
{
	return ALLOC_ARRAY_TYPE(dup_type(array_type->item_type), array_type->params);
}

void *dup_type(void *p)
{
	Type *type = p;

	switch(type->header.kind) {
	case UNSIZED_INT_TYPE:
		return ALLOC_UNSIZED_INT_TYPE(type->lineno);
	case U8_TYPE:
		return ALLOC_U8_TYPE(type->lineno);
	case U16_TYPE:
		return ALLOC_U16_TYPE(type->lineno);
	case U32_TYPE:
		return ALLOC_U32_TYPE(type->lineno);
	case U64_TYPE:
		return ALLOC_U64_TYPE(type->lineno);
	case I8_TYPE:
		return ALLOC_I8_TYPE(type->lineno);
	case I16_TYPE:
		return ALLOC_I16_TYPE(type->lineno);
	case I32_TYPE:
		return ALLOC_I32_TYPE(type->lineno);
	case I64_TYPE:
		return ALLOC_I64_TYPE(type->lineno);
	case F32_TYPE:
		return ALLOC_F32_TYPE(type->lineno);
	case F64_TYPE:
		return ALLOC_F64_TYPE(type->lineno);
	case BOOL_TYPE:
		return ALLOC_BOOL_TYPE(type->lineno);
	case VOID_TYPE:
		return ALLOC_VOID_TYPE(type->lineno);
	case CHAR_TYPE:
		return ALLOC_CHAR_TYPE(type->lineno);
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
	internal_error();
}

void free_type(void *p)
{
	struct type *type = p;

	if (type == NULL) {
		return;
	}
	switch (type->kind) {
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
	case ALIAS_TYPE:
		free(type->u.alias.name);
		break;
	case PARAM_TYPE:
		free(type->u.param.name);
		free_vec(type->u.param.params);
		break;
	case ARRAY_TYPE:
		free_type(type->u.array.l);
		break;
	case POINTER_TYPE:
		free_type(type->u.pointer.l);
		break;
	case TUPLE_TYPE:
		free_vec(type->u.tuple.types);
		break;
	case STRUCT_TYPE:
		free_vec(type->u.struct_.types);
		free_vec(type->u.struct_.names);
		break;
	case FUNC_TYPE:
		free_type(type->u.func.ret);
		free_vec(type->u.func.params);
		break;
	case CONST_TYPE:
		free_type(type->u.const_.type);
		break;
	case VOLATILE_TYPE:
		free_type(type->u.volatile_.type);
		break;
	}
	free(type);
}

void free_expr(void *p)
{
	struct expr *expr = p;

	if (expr == NULL) {
		return;
	}
	switch (expr->kind) {
	case BOOL_LIT_EXPR:
	case INT_LIT_EXPR:
	case FLOAT_LIT_EXPR:
	case CHAR_LIT_EXPR:
		break;
	case STRING_LIT_EXPR:
		free(expr->u.string_lit.val);
		break;
	case UNARY_OP_EXPR:
		free_expr(expr->u.unary_op.operand);
		break;
	case BIN_OP_EXPR:
		free_expr(expr->u.bin_op.l);
		free_expr(expr->u.bin_op.r);
		break;
	case LAMBDA_EXPR:
		free_vec(expr->u.lambda.params);
		free_expr(expr->u.lambda.body);
		break;
	case ARRAY_LIT_EXPR:
		free_vec(expr->u.array_lit.val);
		break;
	case IDENT_EXPR:
		free(expr->u.ident.name);
		break;
	case BLOCK_EXPR:
		free_vec(expr->u.block.stmts);
		break;
	case IF_EXPR:
		free_expr(expr->u.if_.cond);
		free_expr(expr->u.if_.then);
		free_expr(expr->u.if_.else_);
		break;
	case SWITCH_EXPR:
		free_expr(expr->u.switch_.ctrl);
		free_vec(expr->u.switch_.cases);
		break;
	case TUPLE_EXPR:
		free_vec(expr->u.tuple.items);
		break;
	case FUNC_CALL_EXPR:
		free_expr(expr->u.func_call.func);
		free_vec(expr->u.func_call.args);
		break;
	case FIELD_ACCESS_EXPR:
		free_expr(expr->u.field_access.expr);
		free(expr->u.field_access.field);
		break;
	}
	free_type(expr->type); // TODO: Is this safe?
	free(expr);
}

void free_switch_pattern(void *p)
{
	struct switch_pattern *sp = p;

	if (sp == NULL) {
		return;
	}
	switch (sp->kind) {
	case UNDERSCORE_SWITCH_PATTERN:
		break;
	case OR_SWITCH_PATTERN:
		free_vec(sp->u.or.patterns);
		break;
	case ARRAY_SWITCH_PATTERN:
		free_vec(sp->u.array.patterns);
		break;
	case TUPLE_SWITCH_PATTERN:
		free_vec(sp->u.tuple.patterns);
		break;
	case EXPR_SWITCH_PATTERN:
		free_expr(sp->u.expr.expr);
		break;
	}
	free(sp);
}

void free_switch_case(void *p)
{
	struct switch_case *sc = p;

	if (sc == NULL) {
		return;
	}
	free_switch_pattern(sc->l);
	free_expr(sc->r);
	free(sc);
}

void free_decl(void *p)
{
	struct decl *decl = p;

	if (decl == NULL) {
		return;
	}
	switch (decl->kind) {
	case DATA_DECL:
		free_type(decl->u.data.type);
		free(decl->u.data.name);
		free_expr(decl->u.data.init);
		break;
	case TYPEDEF_DECL:
		free(decl->u.typedef_.name);
		free_vec(decl->u.typedef_.params);
		free_type(decl->u.typedef_.type);
		break;
	case FUNC_DECL:
		free_type(decl->u.func.type);
		free(decl->u.func.name);
		free_vec(decl->u.func.param_names);
		free_vec(decl->u.func.body_stmts);
		break;
	}
	free(decl);
}

void free_stmt(void *p)
{
	struct stmt *stmt = p;

	if (stmt == NULL) {
		return;
	}
	switch (stmt->kind) {
	case DECL_STMT:
		free_decl(stmt->u.decl.decl);
		break;
	case EXPR_STMT:
		free_expr(stmt->u.expr.expr);
		break;
	case IF_STMT:
		free_expr(stmt->u.if_.cond);
		free_vec(stmt->u.if_.then_stmts);
		free_vec(stmt->u.if_.else_stmts);
		break;
	case DO_STMT:
		free_vec(stmt->u.do_.stmts);
		free_expr(stmt->u.do_.cond);
		break;
	case WHILE_STMT:
		free_vec(stmt->u.while_.stmts);
		free_expr(stmt->u.while_.cond);
		break;
	case FOR_STMT:
		free_expr(stmt->u.for_.init);
		free_expr(stmt->u.for_.cond);
		free_expr(stmt->u.for_.post);
		free_vec(stmt->u.for_.stmts);
		break;
	case RETURN_STMT:
		free_expr(stmt->u.return_.expr);
		break;
	}
	free(stmt);
}

void free_ast(struct ast ast)
{
	free_vec(ast.decls);
}
