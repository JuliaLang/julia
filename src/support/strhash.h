// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_STRHASH_H
#define JL_STRHASH_H

#include "htable.h"

#ifdef __cplusplus
extern "C" {
#endif

// String-keyed hash table.
// Keys are NUL-terminated C strings, compared by contents (not pointer identity).
// Keys are internalized (strdup'd) on first insertion into the table.
// Call strhash_free to free all internalized keys and the table.
HTPROT(strhash)

// Allocate the table with the given initial size.
htable_t *strhash_new(htable_t *h, size_t size) JL_NOTSAFEPOINT;

// Free the table and all internalized key strings.
void strhash_free(htable_t *h) JL_NOTSAFEPOINT;

#ifdef __cplusplus
}
#endif

#endif
