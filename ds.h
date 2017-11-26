#define ARRAY_LEN(arr) (sizeof(arr) / sizeof((arr)[0]))

struct vec;
typedef struct vec Vec;

Vec *alloc_vec(void (*)(void *));
Vec *dup_vec(Vec *, void *(*)(void *));
void free_vec(Vec *);
size_t vec_len(Vec *);
void *vec_get(Vec *, size_t);
Vec *vec_push(Vec *, void *);
void vec_pop(Vec *);
void *vec_top(Vec *);

struct hash_table;
typedef struct hash_table HashTable;

HashTable *alloc_hash_table(void);
void free_hash_table(HashTable *);
void hash_table_set(HashTable *, const char *, void *);
void *hash_table_get(HashTable *, const char *);
