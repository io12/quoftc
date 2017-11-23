#include <stdlib.h>
#include "ds.h"
#include "symbol_table.h"

struct symbol_table {
	Vec *scopes; // Each scope is a hash table of types
};

static void free_scope(void *scope)
{
	free_hash_table(scope);
}

struct symbol_table alloc_symbol_table(void)
{
	struct symbol_table sym_tbl;

	sym_tbl.scopes = alloc_vec(free_scope);
	return sym_tbl;
}

void free_symbol_table(struct symbol_table sym_tbl)
{
	free_vec(sym_tbl.scopes);
}

void enter_new_scope(struct symbol_table sym_tbl)
{
	vec_push(sym_tbl.scopes, alloc_hash_table());
}

void leave_scope(struct symbol_table sym_tbl)
{
	vec_pop(sym_tbl.scopes);
}
