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

/*
 * The `lineno` field in `TypeHeader` can be `NO_LINENO` if the `Type` node
 * does not represent a part of the source file's syntax.
 */

#define NO_LINENO (MAX_LINENO + 1)

typedef struct {
	enum type_kind kind;
	unsigned lineno;
} TypeHeader;

typedef struct {
	TypeHeader h;
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
	TypeHeader h;
	char *name;
} AliasType;

#define ALLOC_ALIAS_TYPE(...) \
	ALLOC_STRUCT(AliasType, ALIAS_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader h;
	char *name;
	Vec *params;
} ParamType;

#define ALLOC_PARAM_TYPE(...) \
	ALLOC_STRUCT(ParamType, PARAM_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader h;
	Type *item_type;
	unsigned len; // Zero if unspecified
} ArrayType;

#define ALLOC_ARRAY_TYPE(...) \
	ALLOC_STRUCT(ArrayType, ARRAY_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader h;
	Type *pointee_type;
} PointerType;

#define ALLOC_POINTER_TYPE(...) \
	ALLOC_STRUCT(PointerType, POINTER_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader h;
	Vec *members;
} TupleType;

#define ALLOC_TUPLE_TYPE(...) \
	ALLOC_STRUCT(TupleType, TUPLE_TYPE, __VA_ARGS__)

typedef struct {
	Type *type;
	char *name;
} StructMember;

#define ALLOC_STRUCT_MEMBER(...) \
	ALLOC_STRUCT(StructMember, __VA_ARGS__)

typedef struct {
	TypeHeader h;
	Vec *members;
} StructType;

#define ALLOC_STRUCT_TYPE(...) \
	ALLOC_STRUCT(StructType, STRUCT_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader h;
	Type *return_type;
	Vec *param_types;
} FuncType;

#define ALLOC_FUNC_TYPE(...) \
	ALLOC_STRUCT(FuncType, FUNC_TYPE, __VA_ARGS__)

typedef struct {
	TypeHeader h;
	Type *subtype;
} ConstType, VolatileType;

#define ALLOC_CONST_TYPE(...) \
	ALLOC_STRUCT(ConstType, CONST_TYPE, __VA_ARGS__)
#define ALLOC_VOLATILE_TYPE(...) \
	ALLOC_STRUCT(VolatileType, VOLATILE_TYPE, __VA_ARGS__)

void *dup_type(void *);
void free_type(void *);

typedef struct {
	Vec *stmts;
} StmtBlock;

#define ALLOC_STMT_BLOCK(...) \
	ALLOC_STRUCT(StmtBlock, __VA_ARGS__)

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
	ExprHeader h;
} Expr;

typedef struct {
	ExprHeader h;
	bool val;
} BoolLitExpr;

#define ALLOC_BOOL_LIT_EXPR(...) \
	ALLOC_STRUCT(BoolLitExpr, BOOL_LIT_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader h;
	uint64_t val;
} IntLitExpr;

#define ALLOC_INT_LIT_EXPR(...) \
	ALLOC_STRUCT(IntLitExpr, INT_LIT_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader h;
	double val;
} FloatLitExpr;

#define ALLOC_FLOAT_LIT_EXPR(...) \
	ALLOC_STRUCT(FloatLitExpr, FLOAT_LIT_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader h;
	uint32_t val;
} CharLitExpr;

#define ALLOC_CHAR_LIT_EXPR(...) \
	ALLOC_STRUCT(CharLitExpr, CHAR_LIT_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader h;
	char *val;
	unsigned len;
} StringLitExpr;

#define ALLOC_STRING_LIT_EXPR(...) \
	ALLOC_STRUCT(StringLitExpr, STRING_LIT_EXPR, __VA_ARGS__)

enum unary_op {
	NUM_NEG_OP,
	PRE_INC_OP,
	POST_INC_OP,
	PRE_DEC_OP,
	POST_DEC_OP,
	DEREF_OP,
	REF_OP,
	BIT_NEG_OP,
	LOG_NEG_OP
};

typedef struct {
	ExprHeader h;
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
	ExprHeader h;
	enum bin_op op;
	Expr *l, *r;
} BinOpExpr;

#define ALLOC_BIN_OP_EXPR(...) \
	ALLOC_STRUCT(BinOpExpr, BIN_OP_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader h;
	Vec *param_names;
	Expr *body;
} LambdaExpr;

#define ALLOC_LAMBDA_EXPR(...) \
	ALLOC_STRUCT(LambdaExpr, LAMBDA_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader h;
	Vec *subexprs;
} ArrayLitExpr;

#define ALLOC_ARRAY_LIT_EXPR(...) \
	ALLOC_STRUCT(ArrayLitExpr, ARRAY_LIT_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader h;
	char *name;
} IdentExpr;

#define ALLOC_IDENT_EXPR(...) \
	ALLOC_STRUCT(IdentExpr, IDENT_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader h;
	StmtBlock *block;
} BlockExpr;

#define ALLOC_BLOCK_EXPR(...) \
	ALLOC_STRUCT(BlockExpr, BLOCK_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader h;
	Expr *cond, *then, *else_;
} IfExpr;

#define ALLOC_IF_EXPR(...) \
	ALLOC_STRUCT(IfExpr, IF_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader h;
	Expr *ctrl;
	Vec *cases;
} SwitchExpr;

#define ALLOC_SWITCH_EXPR(...) \
	ALLOC_STRUCT(SwitchExpr, SWITCH_EXPR, __VA_ARGS__)

// TODO: Merge this with ArrayLitExpr?
typedef struct {
	ExprHeader h;
	Vec *subexprs;
} TupleExpr;

#define ALLOC_TUPLE_EXPR(...) \
	ALLOC_STRUCT(TupleExpr, TUPLE_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader h;
	Expr *func;
	Vec *args;
} FuncCallExpr;

#define ALLOC_FUNC_CALL_EXPR(...) \
	ALLOC_STRUCT(FuncCallExpr, FUNC_CALL_EXPR, __VA_ARGS__)

typedef struct {
	ExprHeader h;
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
	SwitchPatternHeader h;
} SwitchPattern;

#define UNDERSCORE_SWITCH_PATTERN(...) \
	ALLOC_STRUCT(SwitchPattern, UNDERSCORE_SWITCH_PATTERN, __VA_ARGS__)

typedef struct {
	SwitchPatternHeader h;
	Vec *subpatterns;
} OrSwitchPattern, ArraySwitchPattern, TupleSwitchPattern;

#define ALLOC_OR_SWITCH_PATTERN(...) \
	ALLOC_STRUCT(OrSwitchPattern, OR_SWITCH_PATTERN, __VA_ARGS__)
#define ALLOC_ARRAY_SWITCH_PATTERN(...) \
	ALLOC_STRUCT(ArraySwitchPattern, ARRAY_SWITCH_PATTERN, __VA_ARGS__)
#define ALLOC_TUPLE_SWITCH_PATTERN(...) \
	ALLOC_STRUCT(TupleSwitchPattern, TUPLE_SWITCH_PATTERN, __VA_ARGS__)

typedef struct {
	SwitchPatternHeader h;
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
	unsigned lineno;
} DeclHeader;

typedef struct {
	DeclHeader h;
} Decl;

typedef struct {
	DeclHeader h;
	bool is_let;
	Type *type;
	char *name;
	Expr *init;
} DataDecl;

#define ALLOC_DATA_DECL(...) \
	ALLOC_STRUCT(DataDecl, DATA_DECL, __VA_ARGS__)

typedef struct {
	DeclHeader h;
	char *name;
	Vec *params;
	Type *type;
} TypedefDecl;

#define ALLOC_TYPEDEF_DECL(...) \
	ALLOC_STRUCT(TypedefDecl, TYPEDEF_DECL, __VA_ARGS__)

typedef struct {
	DeclHeader h;
	FuncType *type;
	char *name;
	Vec *param_names;
	StmtBlock *body;
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
	unsigned lineno;
} StmtHeader;

typedef struct {
	StmtHeader h;
} Stmt;

typedef struct {
	StmtHeader h;
	struct decl *decl;
} DeclStmt;

#define ALLOC_DECL_STMT(...) \
	ALLOC_STRUCT(DeclStmt, DECL_STMT, __VA_ARGS__)

typedef struct {
	StmtHeader h;
	Expr *expr;
} ExprStmt;

#define ALLOC_EXPR_STMT(...) \
	ALLOC_STRUCT(ExprStmt, EXPR_STMT, __VA_ARGS__)

typedef struct {
	StmtHeader h;
	Expr *cond;
	StmtBlock *then_block, *else_block;
} IfStmt;

#define ALLOC_IF_STMT(...) \
	ALLOC_STRUCT(IfStmt, IF_STMT, __VA_ARGS__)

typedef struct {
	StmtHeader h;
	StmtBlock *block;
	Expr *cond;
} DoStmt, WhileStmt;

#define ALLOC_DO_STMT(...) \
	ALLOC_STRUCT(DoStmt, DO_STMT, __VA_ARGS__)
#define ALLOC_WHILE_STMT(...) \
	ALLOC_STRUCT(WhileStmt, WHILE_STMT, __VA_ARGS__)

typedef struct {
	StmtHeader h;
	Expr *init, *cond, *post;
	StmtBlock *block;
} ForStmt;

#define ALLOC_FOR_STMT(...) \
	ALLOC_STRUCT(ForStmt, FOR_STMT, __VA_ARGS__)

typedef struct {
	StmtHeader h;
	Expr *expr; // NULL if returning void
} ReturnStmt;

#define ALLOC_RETURN_STMT(...) \
	ALLOC_STRUCT(ReturnStmt, RETURN_STMT, __VA_ARGS__)

void free_stmt(void *);

typedef struct {
	Vec *decls;
} Ast;

void free_ast(Ast);
