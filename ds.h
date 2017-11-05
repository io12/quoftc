struct vec;
typedef struct vec Vec;

Vec *alloc_vec(void);
void free_vec(Vec *, void (*)(void *));
size_t vec_len(Vec *);
void *vec_get(Vec *, size_t);
Vec *vec_push(Vec *, void *);

typedef struct hash_table HashTable;
struct hash_table {
	Vec *data[UINT8_MAX + 1];
};

HashTable *alloc_hash_table(void);
void hash_table_set(HashTable *, const char *, void *);
void *hash_table_get(HashTable *, const char *);
