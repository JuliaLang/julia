//-*- mode:c -*-

#include "htable.h"

#define HTPROT(HTNAME)                                          \
void *HTNAME##_get(htable_t *h, void *key);                     \
void HTNAME##_put(htable_t *h, void *key, void *val);           \
void HTNAME##_adjoin(htable_t *h, void *key, void *val);        \
int HTNAME##_has(htable_t *h, void *key);                       \
int HTNAME##_remove(htable_t *h, void *key);                    \
void **HTNAME##_bp(htable_t *h, void *key);

// return value, or HT_NOTFOUND if key not found

// add key/value binding

// add binding iff key is unbound

// does key exist?

// logically remove key

// get a pointer to the location of the value for the given key.
// creates the location if it doesn't exist. only returns NULL
// if memory allocation fails.
// this should be used for updates, for example:
//     void **bp = ptrhash_bp(h, key);
//     *bp = f(*bp);
// do not reuse bp if there might be intervening calls to ptrhash_put,
// ptrhash_bp, ptrhash_reset, or ptrhash_free.
