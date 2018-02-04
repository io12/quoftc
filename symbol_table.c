#include <stdbool.h>
#include <stdlib.h>
#include "ds.h"
#include "symbol_table.h"

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

bool is_global_scope(struct symbol_table sym_tbl)
{
	return vec_len(sym_tbl.scopes) == 1;
}

void insert_symbol(struct symbol_table sym_tbl, char *name, void *info)
{
	hash_table_set(vec_top(sym_tbl.scopes), name, info);
}

void *lookup_symbol(struct symbol_table sym_tbl, const char *name)
{
	Vec *scopes = sym_tbl.scopes;
	HashTable *scope;
	void *info;
	size_t i;

	for (i = vec_len(scopes); i > 0; i--) {
		scope = vec_get(scopes, i - 1);
		info = hash_table_get(scope, name);
		if (info != NULL) {
			return info;
		}
	}
	return NULL;
}
