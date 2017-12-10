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
	vec->data = xmalloc(VEC_SIZE * sizeof(void *));
	return vec;
}

Vec *dup_vec(Vec *src, void *(*dup_item)(void *))
{
	Vec *dest;
	size_t i;

	dest = alloc_vec(src->free_item);
	for (i = 0; i < src->len; i++) {
		vec_push(dest, dup_item(vec_get(src, i)));
	}
	return dest;
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
		vec->data = xrealloc(vec->data, vec->nalloc * sizeof(void *));
	}
	vec->data[vec->len - 1] = val;
	return vec;
}

void vec_pop(Vec *vec)
{
	vec->free_item(vec->data[--vec->len]);
}

void *vec_top(Vec *vec)
{
	return vec->data[vec->len - 1];
}
