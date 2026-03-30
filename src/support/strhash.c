// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  string-keyed hash table
  keys are NUL-terminated strings, compared by contents
  keys are internalized (strdup'd) on first insertion
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#include "dtypes.h"
#include "hashing.h"
#include "strhash.h"

static uint_t strhash_hash(uintptr_t key, void *ctx)
{
    (void)ctx;
    const char *s = (const char *)key;
    return memhash32(s, strlen(s));
}

static uint_t strhash_eq(void *key1, void *key2, void *ctx)
{
    (void)ctx;
    return strcmp((const char *)key1, (const char *)key2) == 0;
}

#include "htable.inc"

_HTIMPL_EX(strhash, strhash_hash, strhash_eq, )

// Wrapper that internalizes the key on first insertion.
void strhash_put(htable_t *h, void *key, void *val)
{
    // Check if the key already exists
    void **bp = strhash_peek_bp_r(h, key, NULL);
    if (bp != NULL) {
        *bp = val;
        return;
    }
    // Key not found — internalize it and insert
    char *ikey = strdup((const char *)key);
    strhash_put_r(h, ikey, val, NULL);
}

void **strhash_bp(htable_t *h, void *key)
{
    // Try to find existing entry first
    void **bp = strhash_peek_bp_r(h, key, NULL);
    if (bp != NULL)
        return bp;
    // Not found — internalize key and create entry
    char *ikey = strdup((const char *)key);
    return strhash_bp_r(h, ikey, NULL);
}

void *strhash_get(htable_t *h, void *key)
{
    return strhash_get_r(h, key, NULL);
}

int strhash_has(htable_t *h, void *key)
{
    return strhash_has_r(h, key, NULL);
}

int strhash_remove(htable_t *h, void *key)
{
    void **bp = strhash_peek_bp_r(h, key, NULL);
    if (bp != NULL) {
        // Free the internalized key (stored one slot before the value)
        free(*(bp - 1));
        *(bp - 1) = HT_NOTFOUND;
        *bp = HT_NOTFOUND;
        return 1;
    }
    return 0;
}

void strhash_adjoin(htable_t *h, void *key, void *val)
{
    void **bp = strhash_bp(h, key);
    if (*bp == HT_NOTFOUND)
        *bp = val;
}

htable_t *strhash_new(htable_t *h, size_t size)
{
    return htable_new(h, size);
}

void strhash_free(htable_t *h)
{
    size_t sz = h->size;
    void **tab = h->table;
    // Free all internalized key strings
    for (size_t i = 0; i < sz; i += 2) {
        if (tab[i] != HT_NOTFOUND && tab[i+1] != HT_NOTFOUND)
            free(tab[i]);
    }
    htable_free(h);
}
