#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include "langc.h"
#include "vec.h"

#define VEC_SIZE 8

Vec *alloc_vec(void)
{
	Vec *vec;

	vec = NEW(Vec);
	vec->len = 0;
	vec->nalloc = 8;
	vec->data = emalloc(VEC_SIZE * sizeof(void *));
	return vec;
}

void *vec_get(Vec *vec, size_t n)
{
	if (n > vec->len) {
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
