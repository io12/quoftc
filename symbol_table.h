struct symbol_table;

struct symbol_table alloc_symbol_table(void);
void free_symbol_table(struct symbol_table);
void enter_new_scope(struct symbol_table);
void leave_scope(struct symbol_table);
