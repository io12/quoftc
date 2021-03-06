struct symbol_table {
	Vec *scopes; // Each scope is a hash table of symbol info
};

struct symbol_table alloc_symbol_table(void);
void free_symbol_table(struct symbol_table);
void enter_new_scope(struct symbol_table);
void leave_scope(struct symbol_table);
bool is_global_scope(struct symbol_table);
void insert_symbol(struct symbol_table, char *, void *);
void *lookup_symbol(struct symbol_table, const char *);
