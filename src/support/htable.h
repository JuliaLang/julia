// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef HTABLE_H
#define HTABLE_H

#define HT_N_INLINE 32

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    size_t size;
    void **table;
    void *_space[HT_N_INLINE];
} htable_t;

// define this to be an invalid key/value
#define HT_NOTFOUND ((void*)1)

// initialize and free
htable_t *htable_new(htable_t *h, size_t size);
void htable_free(htable_t *h);

// clear and (possibly) change size
void htable_reset(htable_t *h, size_t sz);

#define HTPROT(HTNAME)                                          \
void *HTNAME##_get(htable_t *h, void *key);                     \
void HTNAME##_put(htable_t *h, void *key, void *val);           \
void HTNAME##_adjoin(htable_t *h, void *key, void *val);        \
int HTNAME##_has(htable_t *h, void *key);                       \
int HTNAME##_remove(htable_t *h, void *key);                    \
void **HTNAME##_bp(htable_t *h, void *key);

#ifdef __cplusplus
}
#endif

#endif
