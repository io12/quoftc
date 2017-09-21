typedef struct vec Vec;
struct vec {
	size_t len, nalloc;
	void **data;
};

Vec *alloc_vec(void);
void *vec_get(Vec *, size_t);
Vec *vec_push(Vec *, void *);

typedef struct hash_table HashTable;
struct hash_table {
	Vec *data[UINT8_MAX + 1];
};

HashTable *alloc_hash_table(void);
void hash_table_set(HashTable *ht, const char *key, void *val);
void *hash_table_get(HashTable *ht, const char *key);
