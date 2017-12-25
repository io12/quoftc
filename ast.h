#define ALLOC_STRUCT(struct_tag, lineno_, ...)                        \
	((struct struct_tag *)                                        \
		memcpy(NEW(struct struct_tag), &(struct struct_tag){  \
			.lineno = lineno_,                            \
			__VA_ARGS__                                   \
		}, sizeof(struct struct_tag)))

#define ALLOC_UNION(struct_tag, kind_, sub_struct_name, lineno_, ...)  \
	((struct struct_tag *)                                         \
		memcpy(NEW(struct struct_tag), &(struct struct_tag){   \
			.lineno = lineno_,                             \
			.kind = kind_,                                 \
			.u.sub_struct_name = { __VA_ARGS__ },          \
		}, sizeof(struct struct_tag)))

#define ALLOC_UNION_KIND_ONLY(struct_tag, kind_, lineno_)             \
	((struct struct_tag *)                                        \
		memcpy(NEW(struct struct_tag), &(struct struct_tag){  \
			.lineno = lineno_,                            \
			.kind = kind_                                 \
		}, sizeof(struct struct_tag)))

struct type {
	unsigned lineno;
	enum {
		UNSIZED_INT_TYPE, // Unused until semantic analysis
		U8_TYPE, U16_TYPE, U32_TYPE, U64_TYPE,
		I8_TYPE, I16_TYPE, I32_TYPE, I64_TYPE,
		F32_TYPE, F64_TYPE, BOOL_TYPE, VOID_TYPE, CHAR_TYPE,
		ALIAS_TYPE, PARAM_TYPE, ARRAY_TYPE, POINTER_TYPE,
		TUPLE_TYPE, FUNC_TYPE
	} kind;
	union {
		struct {
			char *name;
		} alias;
		struct {
			char *name;
			Vec *params;
		} param;
		struct {
			struct type *l;
			unsigned len; // Zero if unspecified
		} array;
		struct {
			struct type *l;
		} pointer;
		struct {
			Vec *types;
		} tuple;
		struct {
			struct type *ret;
			Vec *params;
		} func;
	} u;
};

#define ALLOC_UNSIZED_INT_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, UNSIZED_INT_TYPE, lineno)
#define ALLOC_U8_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, U8_TYPE, lineno)
#define ALLOC_U16_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, U16_TYPE, lineno)
#define ALLOC_U32_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, U32_TYPE, lineno)
#define ALLOC_U64_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, U64_TYPE, lineno)
#define ALLOC_I8_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, I8_TYPE, lineno)
#define ALLOC_I16_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, I16_TYPE, lineno)
#define ALLOC_I32_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, I32_TYPE, lineno)
#define ALLOC_I64_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, I64_TYPE, lineno)
#define ALLOC_F32_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, F32_TYPE, lineno)
#define ALLOC_F64_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, F64_TYPE, lineno)
#define ALLOC_BOOL_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, BOOL_TYPE, lineno)
#define ALLOC_VOID_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, VOID_TYPE, lineno)
#define ALLOC_CHAR_TYPE(lineno) \
	ALLOC_UNION_KIND_ONLY(type, CHAR_TYPE, lineno)
#define ALLOC_ALIAS_TYPE(...) \
	ALLOC_UNION(type, ALIAS_TYPE, alias, __VA_ARGS__)
#define ALLOC_PARAM_TYPE(...) \
	ALLOC_UNION(type, PARAM_TYPE, param, __VA_ARGS__)
#define ALLOC_ARRAY_TYPE(...) \
	ALLOC_UNION(type, ARRAY_TYPE, array, __VA_ARGS__)
#define ALLOC_POINTER_TYPE(...) \
	ALLOC_UNION(type, POINTER_TYPE, pointer, __VA_ARGS__)
#define ALLOC_TUPLE_TYPE(...) \
	ALLOC_UNION(type, TUPLE_TYPE, tuple, __VA_ARGS__)
#define ALLOC_FUNC_TYPE(...) \
	ALLOC_UNION(type, FUNC_TYPE, func, __VA_ARGS__)

void *dup_type(void *);

void free_type(void *);

enum unary_op {
	NEG_OP,
	PRE_INC_OP, POST_INC_OP,
	PRE_DEC_OP, POST_DEC_OP,
	DEREF_OP, REF_OP,
	BIT_NOT_OP, LOG_NOT_OP
};

enum bin_op {
	ADD_OP, SUB_OP, MUL_OP, DIV_OP, MOD_OP, LT_OP, GT_OP, LT_EQ_OP,
	GT_EQ_OP, EQ_OP, NOT_EQ_OP, BIT_AND_OP, BIT_OR_OP, BIT_XOR_OP,
	BIT_SHIFT_L_OP, BIT_SHIFT_R_OP, LOG_AND_OP, LOG_OR_OP,
	ASSIGN_OP, ADD_ASSIGN_OP, SUB_ASSIGN_OP, MUL_ASSIGN_OP, DIV_ASSIGN_OP,
	MOD_ASSIGN_OP, BIT_AND_ASSIGN_OP, BIT_OR_ASSIGN_OP, BIT_XOR_ASSIGN_OP,
	BIT_SHIFT_L_ASSIGN_OP, BIT_SHIFT_R_ASSIGN_OP, FIELD_OP
};

struct expr {
	struct type *type; // Uninitialized until semantic analysis
	unsigned lineno;
	enum {
		BOOL_LIT_EXPR, INT_LIT_EXPR, FLOAT_LIT_EXPR, CHAR_LIT_EXPR,
		STRING_LIT_EXPR, UNARY_OP_EXPR, BIN_OP_EXPR, LAMBDA_EXPR,
		ARRAY_LIT_EXPR, IDENT_EXPR, BLOCK_EXPR, IF_EXPR, SWITCH_EXPR,
		TUPLE_EXPR
	} kind;
	union {
		struct {
			bool val;
		} bool_lit;
		struct {
			uint64_t val;
		} int_lit;
		struct {
			double val;
		} float_lit;
		struct {
			uint32_t val;
		} char_lit;
		struct {
			char *val;
			unsigned len;
		} string_lit;
		struct {
			enum unary_op op;
			struct expr *operand;
		} unary_op;
		struct {
			enum bin_op op;
			struct expr *l, *r;
		} bin_op;
		struct {
			Vec *params;
			struct expr *body;
		} lambda;
		struct {
			Vec *val;
		} array_lit;
		struct {
			char *name;
		} ident;
		struct {
			Vec *stmts;
		} block;
		struct {
			struct expr *cond, *then, *else_;
		} if_;
		struct {
			struct expr *ctrl;
			Vec *cases;
		} switch_;
		struct {
			Vec *items;
		} tuple;
	} u;
};

#define ALLOC_BOOL_LIT_EXPR(...) \
	ALLOC_UNION(expr, BOOL_LIT_EXPR, bool_lit, __VA_ARGS__)
#define ALLOC_INT_LIT_EXPR(...) \
	ALLOC_UNION(expr, INT_LIT_EXPR, int_lit, __VA_ARGS__)
#define ALLOC_FLOAT_LIT_EXPR(...) \
	ALLOC_UNION(expr, FLOAT_LIT_EXPR, float_lit, __VA_ARGS__)
#define ALLOC_CHAR_LIT_EXPR(...) \
	ALLOC_UNION(expr, CHAR_LIT_EXPR, char_lit, __VA_ARGS__)
#define ALLOC_STRING_LIT_EXPR(...) \
	ALLOC_UNION(expr, STRING_LIT_EXPR, string_lit, __VA_ARGS__)
#define ALLOC_UNARY_OP_EXPR(...) \
	ALLOC_UNION(expr, UNARY_OP_EXPR, unary_op, __VA_ARGS__)
#define ALLOC_BIN_OP_EXPR(...) \
	ALLOC_UNION(expr, BIN_OP_EXPR, bin_op, __VA_ARGS__)
#define ALLOC_LAMBDA_EXPR(...) \
	ALLOC_UNION(expr, LAMBDA_EXPR, lambda, __VA_ARGS__)
#define ALLOC_ARRAY_LIT_EXPR(...) \
	ALLOC_UNION(expr, ARRAY_LIT_EXPR, array_lit, __VA_ARGS__)
#define ALLOC_IDENT_EXPR(...) \
	ALLOC_UNION(expr, IDENT_EXPR, ident, __VA_ARGS__)
#define ALLOC_BLOCK_EXPR(...) \
	ALLOC_UNION(expr, BLOCK_EXPR, block, __VA_ARGS__)
#define ALLOC_IF_EXPR(...) \
	ALLOC_UNION(expr, IF_EXPR, if_, __VA_ARGS__)
#define ALLOC_SWITCH_EXPR(...) \
	ALLOC_UNION(expr, SWITCH_EXPR, switch_, __VA_ARGS__)
#define ALLOC_TUPLE_EXPR(...) \
	ALLOC_UNION(expr, TUPLE_EXPR, tuple, __VA_ARGS__)

void free_expr(void *);

struct switch_pattern {
	unsigned lineno;
	enum {
		UNDERSCORE_SWITCH_PATTERN, OR_SWITCH_PATTERN,
		ARRAY_SWITCH_PATTERN, TUPLE_SWITCH_PATTERN, EXPR_SWITCH_PATTERN
	} kind;
	union {
		struct {
			Vec *patterns;
		} or, array, tuple;
		struct {
			struct expr *expr;
		} expr;
	} u;
};

#define ALLOC_UNDERSCORE_SWITCH_PATTERN(lineno) \
	ALLOC_UNION_KIND_ONLY(switch_pattern, UNDERSCORE_SWITCH_PATTERN, lineno)
#define ALLOC_OR_SWITCH_PATTERN(...) \
	ALLOC_UNION(switch_pattern, OR_SWITCH_PATTERN, or, __VA_ARGS__)
#define ALLOC_ARRAY_SWITCH_PATTERN(...) \
	ALLOC_UNION(switch_pattern, ARRAY_SWITCH_PATTERN, array, __VA_ARGS__)
#define ALLOC_TUPLE_SWITCH_PATTERN(...) \
	ALLOC_UNION(switch_pattern, TUPLE_SWITCH_PATTERN, tuple, __VA_ARGS__)
#define ALLOC_EXPR_SWITCH_PATTERN(...) \
	ALLOC_UNION(switch_pattern, EXPR_SWITCH_PATTERN, expr, __VA_ARGS__)

void free_switch_pattern(void *);

struct switch_case {
	unsigned lineno;
	struct switch_pattern *l;
	struct expr *r;
};

#define ALLOC_SWITCH_CASE(...) \
	ALLOC_STRUCT(switch_case, __VA_ARGS__)

void free_switch_case(void *);

struct decl {
	unsigned lineno;
	enum {
		DATA_DECL, FUNC_DECL
	} kind;
	union {
		struct {
			bool is_const;
			struct type *type;
			char *name;
			struct expr *init;
		} data;
		struct {
			struct type *return_type;
			char *name;
			Vec *param_types, *param_names;
			Vec *body_stmts;
		} func;
	} u;
};

#define ALLOC_DATA_DECL(...) \
	ALLOC_UNION(decl, DATA_DECL, data, __VA_ARGS__)
#define ALLOC_FUNC_DECL(...) \
	ALLOC_UNION(decl, FUNC_DECL, func, __VA_ARGS__)

void free_decl(void *);

struct stmt {
	unsigned lineno;
	enum {
		DECL_STMT, EXPR_STMT, IF_STMT, DO_STMT,
		WHILE_STMT, FOR_STMT
	} kind;
	union {
		struct {
			struct decl *decl;
		} decl;
		struct {
			struct expr *expr;
		} expr;
		struct {
			struct expr *cond;
			Vec *then_stmts, *else_stmts;
		} if_;
		struct {
			Vec *stmts;
			struct expr *cond;
		} do_, while_;
		struct {
			struct expr *init, *cond, *post;
			Vec *stmts;
		} for_;
	} u;
};

#define ALLOC_DECL_STMT(...) \
	ALLOC_UNION(stmt, DECL_STMT, decl, __VA_ARGS__)
#define ALLOC_EXPR_STMT(...) \
	ALLOC_UNION(stmt, EXPR_STMT, expr, __VA_ARGS__)
#define ALLOC_IF_STMT(...) \
	ALLOC_UNION(stmt, IF_STMT, if_, __VA_ARGS__)
#define ALLOC_DO_STMT(...) \
	ALLOC_UNION(stmt, DO_STMT, do_, __VA_ARGS__)
#define ALLOC_WHILE_STMT(...) \
	ALLOC_UNION(stmt, WHILE_STMT, while_, __VA_ARGS__)
#define ALLOC_FOR_STMT(...) \
	ALLOC_UNION(stmt, FOR_STMT, for_, __VA_ARGS__)

void free_stmt(void *);

struct ast {
	Vec *decls;
};

void free_ast(struct ast);
