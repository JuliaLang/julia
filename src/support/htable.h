// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_HTABLE_H
#define JL_HTABLE_H

#define HT_N_INLINE 32

#include "analyzer_annotations.h"

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

#define HTPROT(HTNAME)                                                  \
void *HTNAME##_get(htable_t *h, void *key) JL_NOTSAFEPOINT;             \
void HTNAME##_put(htable_t *h, void *key, void *val) JL_NOTSAFEPOINT;   \
void HTNAME##_adjoin(htable_t *h, void *key, void *val) JL_NOTSAFEPOINT;\
int HTNAME##_has(htable_t *h, void *key) JL_NOTSAFEPOINT;               \
int HTNAME##_remove(htable_t *h, void *key) JL_NOTSAFEPOINT;            \
void **HTNAME##_bp(htable_t *h, void *key) JL_NOTSAFEPOINT;

#define HTPROT_R(HTNAME)                                                \
void *HTNAME##_get_r(htable_t *h, void *key, void *ctx);                \
void HTNAME##_put_r(htable_t *h, void *key, void *val, void *ctx);      \
void HTNAME##_adjoin_r(htable_t *h, void *key, void *val, void *ctx);   \
int HTNAME##_has_r(htable_t *h, void *key, void *ctx);                  \
int HTNAME##_remove_r(htable_t *h, void *key, void *ctx);               \
void **HTNAME##_bp_r(htable_t *h, void *key, void *ctx);

#ifdef __cplusplus
}
#endif

#endif
