#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "ds.h"
#include "quoftc.h"

typedef struct key_val_pair KeyValPair;
struct key_val_pair {
	const char *key;
	void *val;
};

struct hash_table {
	Vec *data[UINT8_MAX + 1];
};

HashTable *alloc_hash_table(void)
{
	return NEWC(HashTable);
}

void free_hash_table(HashTable *ht)
{
	int i;

	for (i = 0; i < ARRAY_LEN(ht->data); i++) {
		free(ht->data[i]); // TODO: Free key/value?
	}
	free(ht);
}

static KeyValPair *alloc_key_val_pair(const char *key, void *val)
{
	KeyValPair *pair;

	pair = NEW(KeyValPair);
	pair->key = key;
	pair->val = val;
	return pair;
}

static uint8_t hash(const char *s)
{
	uint8_t hash = 0;

	while (*s != '\0') {
		hash += *s++;
	}
	return hash;
}

void hash_table_set(HashTable *ht, const char *key, void *val)
{
	Vec **pairs;
	KeyValPair *pair;

	pair = alloc_key_val_pair(key, val);
	pairs = &ht->data[hash(key)];
	if (*pairs == NULL) {
		*pairs = alloc_vec();
	}
	vec_push(*pairs, pair);
}

void *hash_table_get(HashTable *ht, const char *key)
{
	Vec *pairs;
	size_t i;
	KeyValPair *pair;

	pairs = ht->data[hash(key)];
	for (i = 0; i < vec_len(pairs); i++) {
		pair = vec_get(pairs, i);
		if (strcmp(key, pair->key) == 0) {
			return pair->val;
		}
	}
	return NULL;
}
