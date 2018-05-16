enum tok_kind {
#define TOK(tok_enum, tok_str) tok_enum,
#include "tokens.inc"
#undef TOK
};

struct tok {
	enum tok_kind kind;
	unsigned lineno;
	union {
		uint32_t char_lit;
		struct {
			char val[MAX_STRING_SIZE + 1];
			unsigned len;
		} string_lit;
		uint64_t int_lit;
		double float_lit;
		char ident[MAX_IDENT_SIZE + 1];
	} u;
};

const char *get_filename(void);
const char *tok_to_str(enum tok_kind);
void lex(struct tok *);
void init_lex(const char *);
void cleanup_lex(void);
