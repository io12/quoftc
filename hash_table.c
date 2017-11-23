#include <stdint.h>
#include <string.h>
#include "ds.h"
#include "quoftc.h"

typedef struct hash_table_pair HashTablePair;
struct hash_table_pair {
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

static HashTablePair *alloc_hash_table_pair(const char *key, void *val)
{
	HashTablePair *pair;

	pair = NEW(HashTablePair);
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
	HashTablePair *pair;

	pair = alloc_hash_table_pair(key, val);
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
	HashTablePair *pair;

	pairs = ht->data[hash(key)];
	for (i = 0; i < vec_len(pairs); i++) {
		pair = vec_get(pairs, i);
		if (strcmp(key, pair->key) == 0) {
			return pair->val;
		}
	}
	return NULL;
}
