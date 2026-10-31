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

static void *strhash_keyalloc(void *key, void *ctx) JL_NOTSAFEPOINT { (void)ctx; return strdup((const char *)key); }
static void strhash_keyfree(void *key, void *ctx) JL_NOTSAFEPOINT { (void)ctx; free(key); }
_HTIMPL_EX(strhash, strhash_hash, strhash_eq, strhash_keyalloc, strhash_keyfree, )

void  strhash_put(htable_t *h, void *key, void *val) { strhash_put_r(h, key, val, NULL); }
void *strhash_get(htable_t *h, void *key) { return strhash_get_r(h, key, NULL); }
void **strhash_bp(htable_t *h, void *key) { return strhash_bp_r(h, key, NULL); }
int   strhash_has(htable_t *h, void *key) { return strhash_has_r(h, key, NULL); }
int   strhash_remove(htable_t *h, void *key) { return strhash_remove_r(h, key, NULL); }
void  strhash_adjoin(htable_t *h, void *key, void *val) { strhash_adjoin_r(h, key, val, NULL); }

htable_t *strhash_new(htable_t *h, size_t size)
{
    return htable_new(h, size);
}

void strhash_free(htable_t *h)
{
    size_t sz = h->size;
    void **tab = h->table;
    // Free all internalized key strings (live and tombstone entries)
    for (size_t i = 0; i < sz; i += 2) {
        if (tab[i] != HT_NOTFOUND)
            free(tab[i]);
    }
    htable_free(h);
}
