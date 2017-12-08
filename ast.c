#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "ds.h"
#include "quoftc.h"
#include "ast.h"

void *dup_type(void *p)
{
	struct type *src = p;

	switch(src->kind) {
	case UNSIZED_INT_TYPE:
		return ALLOC_UNSIZED_INT_TYPE(src->lineno);
	case U8_TYPE:
		return ALLOC_U8_TYPE(src->lineno);
	case U16_TYPE:
		return ALLOC_U16_TYPE(src->lineno);
	case U32_TYPE:
		return ALLOC_U32_TYPE(src->lineno);
	case U64_TYPE:
		return ALLOC_U64_TYPE(src->lineno);
	case I8_TYPE:
		return ALLOC_I8_TYPE(src->lineno);
	case I16_TYPE:
		return ALLOC_I16_TYPE(src->lineno);
	case I32_TYPE:
		return ALLOC_I32_TYPE(src->lineno);
	case I64_TYPE:
		return ALLOC_I64_TYPE(src->lineno);
	case F32_TYPE:
		return ALLOC_F32_TYPE(src->lineno);
	case F64_TYPE:
		return ALLOC_F64_TYPE(src->lineno);
	case BOOL_TYPE:
		return ALLOC_BOOL_TYPE(src->lineno);
	case VOID_TYPE:
		return ALLOC_VOID_TYPE(src->lineno);
	case CHAR_TYPE:
		return ALLOC_CHAR_TYPE(src->lineno);
	case ALIAS_TYPE:
		return ALLOC_ALIAS_TYPE(src->lineno,
				estrdup(src->u.alias.name));
	case PARAM_TYPE:
		return ALLOC_PARAM_TYPE(src->lineno,
				estrdup(src->u.param.name),
				dup_vec(src->u.param.params, dup_type));
	case ARRAY_TYPE:
		return ALLOC_ARRAY_TYPE(src->lineno, dup_type(src->u.array.l),
				src->u.array.len);
	case POINTER_TYPE:
		return ALLOC_POINTER_TYPE(src->lineno,
				dup_type(src->u.pointer.l));
	case TUPLE_TYPE:
		return ALLOC_TUPLE_TYPE(src->lineno,
				dup_vec(src->u.tuple.types, dup_type));
	case FUNC_TYPE:
		return ALLOC_FUNC_TYPE(src->lineno, dup_type(src->u.func.ret),
				dup_vec(src->u.func.params, dup_type));
	}
	internal_error();
}

void free_type(void *p)
{
	struct type *type = p;

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
	case FUNC_TYPE:
		free_type(type->u.func.ret);
		free_vec(type->u.func.params);
		break;
	}
	free(type);
}

void free_expr(void *p)
{
	struct expr *expr = p;

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
	}
	free_type(expr->type); // TODO: Is this safe?
	free(expr);
}

void free_switch_pattern(void *p)
{
	struct switch_pattern *sp = p;

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

	free_switch_pattern(sc->l);
	free_expr(sc->r);
	free(sc);
}

void free_decl(void *p)
{
	struct decl *decl = p;

	free_type(decl->type);
	free(decl->name);
	free_expr(decl->val);
	free(decl);
}

void free_stmt(void *p)
{
	struct stmt *stmt = p;

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
	}
	free(stmt);
}

void free_ast(struct ast ast)
{
	free_vec(ast.decls);
}
