#include <stdint.h>
#include <stdlib.h>
#include "ds.h"
#include "quoftc.h"

#define VEC_SIZE 8

struct vec {
	void (*free_item)(void *);
	size_t len, nalloc;
	void **data;
};

Vec *alloc_vec(void (*free_item)(void *))
{
	Vec *vec;

	vec = NEW(Vec);
	vec->free_item = free_item;
	vec->len = 0;
	vec->nalloc = 8;
	vec->data = emalloc(VEC_SIZE * sizeof(void *));
	return vec;
}

void free_vec(Vec *vec)
{
	size_t i;

	for (i = 0; i < vec->len; i++) {
		vec->free_item(vec_get(vec, i));
	}
	free(vec->data);
	free(vec);
}

size_t vec_len(Vec *vec)
{
	return vec->len;
}

void *vec_get(Vec *vec, size_t n)
{
	if (n >= vec->len) {
		internal_error();
	}
	return vec->data[n];
}

Vec *vec_push(Vec *vec, void *val)
{
	vec->len++;
	if (vec->len > vec->nalloc) {
		vec->nalloc *= 2;
		vec->data = erealloc(vec->data, vec->nalloc * sizeof(void *));
	}
	vec->data[vec->len - 1] = val;
	return vec;
}

void *vec_pop(Vec *vec)
{
	vec->free_item(vec->data[--vec->len]);
	return vec->data[vec->len - 1];
}
