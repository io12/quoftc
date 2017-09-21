typedef struct vec Vec;
struct vec {
	size_t len, nalloc;
	void **data;
};

Vec *vec(void);
void *vec_get(Vec *, size_t);
void vec_push(Vec *, void *);
