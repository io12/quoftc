// Structs representing AST nodes and constructor macros

// Generic constructor for C structs
#define ALLOC_STRUCT(type, ...)             \
	((type *)                           \
	 	memcpy(NEW(type), &(type){  \
			__VA_ARGS__         \
		}, sizeof(type)))

enum type_kind {
	UNSIZED_INT_TYPE, // Unused until semantic analysis
	U8_TYPE,
	U16_TYPE,
	U32_TYPE,
	U64_TYPE,
	I8_TYPE,
	I16_TYPE,
	I32_TYPE,
	I64_TYPE,
	F32_TYPE,
	F64_TYPE,
	BOOL_TYPE,
	VOID_TYPE,
	CHAR_TYPE,
	ALIAS_TYPE,
	PARAM_TYPE,
	ARRAY_TYPE,
	POINTER_TYPE,
	TUPLE_TYPE,
	STRUCT_TYPE,
	FUNC_TYPE,
	CONST_TYPE,
	VOLATILE_TYPE
};

typedef struct {
	enum type_kind kind;
	unsigned lineno;
} TypeHeader;

typedef struct {
	TypeHeader header;
} Type;

#define ALLOC_UNSIZED_INT_TYPE(...) \
	ALLOC_STRUCT(Type, UNSIZED_INT_TYPE, __VA_ARGS__)
#define ALLOC_U8_TYPE(...) \
	ALLOC_STRUCT(Type, U8_TYPE, __VA_ARGS__)
#define ALLOC_U16_TYPE(...) \
	ALLOC_STRUCT(Type, U16_TYPE, __VA_ARGS__)
#define ALLOC_U32_TYPE(...) \
	ALLOC_STRUCT(Type, U32_TYPE, __VA_ARGS__)
#define ALLOC_U64_TYPE(...) \
	ALLOC_STRUCT(Type, U64_TYPE, __VA_ARGS__)
#define ALLOC_I8_TYPE(...) \
	ALLOC_STRUCT(Type, I8_TYPE, __VA_ARGS__)
#define ALLOC_I16_TYPE(...) \
	ALLOC_STRUCT(Type, I16_TYPE, __VA_ARGS__)
#define ALLOC_I32_TYPE(...) \
	ALLOC_STRUCT(Type, I32_TYPE, __VA_ARGS__)
#define ALLOC_I64_TYPE(...) \
	ALLOC_STRUCT(Type, I64_TYPE, __VA_ARGS__)
#define ALLOC_F32_TYPE(...) \
	ALLOC_STRUCT(Type, F32_TYPE, __VA_ARGS__)
#define ALLOC_F64_TYPE(...) \
	ALLOC_STRUCT(Type, F64_TYPE, __VA_ARGS__)
#define ALLOC_BOOL_TYPE(...) \
	ALLOC_STRUCT(Type, BOOL_TYPE, __VA_ARGS__)
#define ALLOC_VOID_TYPE(...) \
	ALLOC_STRUCT(Type, VOID_TYPE, __VA_ARGS__)
#define ALLOC_CHAR_TYPE(...) \
	ALLOC_STRUCT(Type, CHAR_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader header;
	char *name;
} AliasType;

#define ALLOC_ALIAS_TYPE(...) \
	ALLOC_STRUCT(AliasType, ALIAS_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader header;
	char *name;
	Vec *params;
} ParamType;

#define ALLOC_PARAM_TYPE(...) \
	ALLOC_STRUCT(ParamType, PARAM_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader header;
	Type *item_type;
	unsigned len; // Zero if unspecified
} ArrayType;

#define ALLOC_ARRAY_TYPE(...) \
	ALLOC_STRUCT(ArrayType, ARRAY_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader header;
	Type *pointee_type;
} PointerType;

#define ALLOC_POINTER_TYPE(...) \
	ALLOC_STRUCT(PointerType, POINTER_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader header;
	Vec *member_types;
} TupleType;

#define ALLOC_TUPLE_TYPE(...) \
	ALLOC_STRUCT(TupleType, TUPLE_TYPE, __VA_ARGS__)

typedef struct {
	Type *type;
	char *name;
} StructMemberType;

#define ALLOC_STRUCT_MEMBER_TYPE(...) \
	ALLOC_STRUCT(StructMemberType, __VA_ARGS__)

typedef struct {
	TypeHeader header;
	Vec *member_types;
} StructType;

#define ALLOC_STRUCT_TYPE(...) \
	ALLOC_STRUCT(StructType, STRUCT_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader header;
	Type *return_type;
	Vec *param_types;
} FuncType;

#define ALLOC_FUNC_TYPE(...) \
	ALLOC_STRUCT(FuncType, FUNC_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader header;
	Type *subtype;
} ConstType, VolatileType;

#define ALLOC_CONST_TYPE(...) \
	ALLOC_STRUCT(ConstType, CONST_TYPE, __VA_ARGS__)
#define ALLOC_VOLATILE_TYPE(...) \
	ALLOC_STRUCT(VolatileType, VOLATILE_TYPE, __VA_ARGS__)

void *dup_type(void *);
void free_type(void *);

enum expr_kind {
	BOOL_LIT_EXPR,
	INT_LIT_EXPR,
	FLOAT_LIT_EXPR,
	CHAR_LIT_EXPR,
	STRING_LIT_EXPR,
	UNARY_OP_EXPR,
	BIN_OP_EXPR,
	LAMBDA_EXPR,
	ARRAY_LIT_EXPR,
	IDENT_EXPR,
	BLOCK_EXPR,
	IF_EXPR,
	SWITCH_EXPR,
	TUPLE_EXPR,
	FUNC_CALL_EXPR,
	FIELD_ACCESS_EXPR
};

typedef struct {
	Type *type; // Uninitialized until semantic analysis
	enum expr_kind kind;
	unsigned lineno;
} ExprHeader;

typedef struct {
	ExprHeader header;
} Expr;

typedef struct {
	ExprHeader header;
	bool val;
} BoolLitExpr;

#define ALLOC_BOOL_LIT_EXPR(...) \
	ALLOC_STRUCT(BoolLitExpr, BOOL_LIT_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	uint64_t val;
} IntLitExpr;

#define ALLOC_INT_LIT_EXPR(...) \
	ALLOC_STRUCT(IntLitExpr, INT_LIT_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	double val;
} FloatLitExpr;

#define ALLOC_FLOAT_LIT_EXPR(...) \
	ALLOC_STRUCT(FloatLitExpr, FLOAT_LIT_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	uint32_t val;
} CharLitExpr;

#define ALLOC_CHAR_LIT_EXPR(...) \
	ALLOC_STRUCT(CharLitExpr, CHAR_LIT_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	char *val;
	unsigned len;
} StringLitExpr;

#define ALLOC_STRING_LIT_EXPR(...) \
	ALLOC_STRUCT(StringLitExpr, STRING_LIT_EXPR, __VA_ARGS__)

enum unary_op {
	NEG_OP,
	PRE_INC_OP,
	POST_INC_OP,
	PRE_DEC_OP,
	POST_DEC_OP,
	DEREF_OP,
	REF_OP,
	BIT_NOT_OP,
	LOG_NOT_OP
};

typedef struct {
	ExprHeader header;
	enum unary_op op;
	Expr *operand;
} UnaryOpExpr;

#define ALLOC_UNARY_OP_EXPR(...) \
	ALLOC_STRUCT(UnaryOpExpr, UNARY_OP_EXPR, __VA_ARGS__)

enum bin_op {
	ADD_OP,
	SUB_OP,
	MUL_OP,
	DIV_OP,
	MOD_OP,
	LT_OP,
	GT_OP,
	LT_EQ_OP,
	GT_EQ_OP,
	EQ_OP,
	NOT_EQ_OP,
	BIT_AND_OP,
	BIT_OR_OP,
	BIT_XOR_OP,
	BIT_SHIFT_L_OP,
	BIT_SHIFT_R_OP,
	LOG_AND_OP,
	LOG_OR_OP,
	ASSIGN_OP,
	ADD_ASSIGN_OP,
	SUB_ASSIGN_OP,
	MUL_ASSIGN_OP,
	DIV_ASSIGN_OP,
	MOD_ASSIGN_OP,
	BIT_AND_ASSIGN_OP,
	BIT_OR_ASSIGN_OP,
	BIT_XOR_ASSIGN_OP,
	BIT_SHIFT_L_ASSIGN_OP,
	BIT_SHIFT_R_ASSIGN_OP
};

typedef struct {
	ExprHeader header;
	enum bin_op op;
	Expr *l, *r;
} BinOpExpr;

#define ALLOC_BIN_OP_EXPR(...) \
	ALLOC_STRUCT(BinOpExpr, BIN_OP_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	Vec *param_names;
	Expr *body;
} LambdaExpr;

#define ALLOC_LAMBDA_EXPR(...) \
	ALLOC_STRUCT(LambdaExpr, LAMBDA_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	Vec *subexprs;
} ArrayLitExpr;

#define ALLOC_ARRAY_LIT_EXPR(...) \
	ALLOC_STRUCT(ArrayLitExpr, ARRAY_LIT_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	char *name;
} IdentExpr;

#define ALLOC_IDENT_EXPR(...) \
	ALLOC_STRUCT(IdentExpr, IDENT_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	Vec *stmts;
} BlockExpr;

#define ALLOC_BLOCK_EXPR(...) \
	ALLOC_STRUCT(BlockExpr, BLOCK_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	Expr *cond, *then, *else_;
} IfExpr;

#define ALLOC_IF_EXPR(...) \
	ALLOC_STRUCT(IfExpr, IF_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	Expr *ctrl;
	Vec *cases;
} SwitchExpr;

#define ALLOC_SWITCH_EXPR(...) \
	ALLOC_STRUCT(SwitchExpr, SWITCH_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	Vec *items;
} TupleExpr;

#define ALLOC_TUPLE_EXPR(...) \
	ALLOC_STRUCT(TupleExpr, TUPLE_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	Expr *func;
	Vec *args;
} FuncCallExpr;

#define ALLOC_FUNC_CALL_EXPR(...) \
	ALLOC_STRUCT(FuncCallExpr, FUNC_CALL_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader header;
	Expr *parent;
	char *field;
} FieldAccessExpr;

#define ALLOC_FIELD_ACCESS_EXPR(...) \
	ALLOC_STRUCT(FieldAccessExpr, FIELD_ACCESS_EXPR, __VA_ARGS__)

void free_expr(void *);

enum switch_pattern_kind {
	UNDERSCORE_SWITCH_PATTERN,
	OR_SWITCH_PATTERN,
	ARRAY_SWITCH_PATTERN,
	TUPLE_SWITCH_PATTERN,
	EXPR_SWITCH_PATTERN
};

typedef struct {
	enum switch_pattern_kind kind;
	unsigned lineno;
} SwitchPatternHeader;

typedef struct {
	SwitchPatternHeader header;
} SwitchPattern;

#define UNDERSCORE_SWITCH_PATTERN(...) \
	ALLOC_STRUCT(SwitchPattern, UNDERSCORE_SWITCH_PATTERN, __VA_ARGS__)

typedef struct {
	SwitchPatternHeader header;
	Vec *subpatterns;
} OrSwitchPattern, ArraySwitchPattern, TupleSwitchPattern;

#define ALLOC_OR_SWITCH_PATTERN(...) \
	ALLOC_STRUCT(OrSwitchPattern, OR_SWITCH_PATTERN, __VA_ARGS__)
#define ALLOC_ARRAY_SWITCH_PATTERN(...) \
	ALLOC_STRUCT(ArraySwitchPattern, ARRAY_SWITCH_PATTERN, __VA_ARGS__)
#define ALLOC_TUPLE_SWITCH_PATTERN(...) \
	ALLOC_STRUCT(TupleSwitchPattern, TUPLE_SWITCH_PATTERN, __VA_ARGS__)

typedef struct {
	SwitchPatternHeader header;
	Expr *expr;
} ExprSwitchPattern;

#define ALLOC_EXPR_SWITCH_PATTERN(...) \
	ALLOC_STRUCT(ExprSwitchPattern, EXPR_SWITCH_PATTERN, __VA_ARGS__)

void free_switch_pattern(void *);

typedef struct {
	unsigned lineno;
	struct switch_pattern *l;
	Expr *r;
} SwitchCase;

#define ALLOC_SWITCH_CASE(...) \
	ALLOC_STRUCT(switch_case, __VA_ARGS__)

void free_switch_case(void *);

enum decl_kind {
	DATA_DECL,
	TYPEDEF_DECL,
	FUNC_DECL
};

typedef struct {
	enum decl_kind kind;
} DeclHeader;

typedef struct {
	DeclHeader header;
} Decl;

typedef struct {
	DeclHeader header;
	bool is_let;
	struct type *type;
	char *name;
	Expr *init;
} DataDecl;

#define ALLOC_DATA_DECL(...) \
	ALLOC_STRUCT(DataDecl, DATA_DECL, __VA_ARGS__)

typedef struct {
	DeclHeader header;
	char *name;
	Vec *params;
	struct type *type;
} TypedefDecl;

#define ALLOC_TYPEDEF_DECL(...) \
	ALLOC_STRUCT(TypedefDecl, TYPEDEF_DECL, __VA_ARGS__)

typedef struct {
	DeclHeader header;
	struct type *type;
	char *name;
	Vec *param_names;
	Vec *body_stmts;
} FuncDecl;

#define ALLOC_FUNC_DECL(...) \
	ALLOC_STRUCT(FuncDecl, FUNC_DECL, __VA_ARGS__)

void free_decl(void *);

enum stmt_kind {
	DECL_STMT,
	EXPR_STMT,
	IF_STMT,
	DO_STMT,
	WHILE_STMT,
	FOR_STMT,
	RETURN_STMT
};

typedef struct {
	enum stmt_kind kind;
} StmtHeader;

typedef struct {
	StmtHeader header;
} Stmt;

typedef struct {
	StmtHeader header;
	struct decl *decl;
} DeclStmt;

#define ALLOC_DECL_STMT(...) \
	ALLOC_STRUCT(DeclStmt, DECL_STMT, __VA_ARGS__)

typedef struct {
	StmtHeader header;
	Expr *expr;
} ExprStmt;

#define ALLOC_EXPR_STMT(...) \
	ALLOC_STRUCT(ExprStmt, EXPR_STMT, __VA_ARGS__)

typedef struct {
	StmtHeader header;
	Expr *cond;
	Vec *then_stmts, *else_stmts;
} IfStmt;

#define ALLOC_IF_STMT(...) \
	ALLOC_STRUCT(IfStmt, IF_STMT, __VA_ARGS__)

typedef struct {
	StmtHeader header;
	Vec *stmts;
	Expr *cond;
} DoStmt;

#define ALLOC_DO_STMT(...) \
	ALLOC_STRUCT(DoStmt, DO_STMT, __VA_ARGS__)

typedef struct {
	StmtHeader header;
	Expr *cond;
	Vec *stmts;
} WhileStmt;

#define ALLOC_WHILE_STMT(...) \
	ALLOC_STRUCT(WhileStmt, WHILE_STMT, __VA_ARGS__)

typedef struct {
	StmtHeader header;
	Expr *init, *cond, *post;
	Vec *stmts;
} ForStmt;

#define ALLOC_FOR_STMT(...) \
	ALLOC_STRUCT(ForStmt, FOR_STMT, __VA_ARGS__)

typedef struct {
	StmtHeader header;
	Expr *expr; // NULL if returning void
} ReturnStmt;

#define ALLOC_RETURN_STMT(...) \
	ALLOC_STRUCT(ReturnStmt, RETURN_STMT, __VA_ARGS__)

void free_stmt(void *);

typedef struct {
	Vec *decls;
} Ast;

void free_ast(Ast);
