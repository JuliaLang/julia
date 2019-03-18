// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#include "dtypes.h"
#include "arraylist.h"

#ifdef __cplusplus
extern "C" {
#endif

arraylist_t *arraylist_new(arraylist_t *a, size_t size)
{
    a->len = 0;
    if (size <= AL_N_INLINE) {
        a->items = &a->_space[0];
        a->max = AL_N_INLINE;
    }
    else {
        a->items = (void**)LLT_ALLOC(size*sizeof(void*));
        if (a->items == NULL) return NULL;
        a->max = size;
    }
    return a;
}

void arraylist_free(arraylist_t *a)
{
    if (a->items != &a->_space[0])
        LLT_FREE(a->items);
    a->len = 0;
    a->max = AL_N_INLINE;
    a->items = &a->_space[0];
}

void arraylist_grow(arraylist_t *a, size_t n)
{
    size_t len = a->len;
    size_t newlen = len + n;
    if (newlen > a->max) {
        if (a->items == &a->_space[0]) {
            void **p = (void**)LLT_ALLOC((a->len+n)*sizeof(void*));
            if (p == NULL) return;
            memcpy(p, a->items, len * sizeof(void*));
            a->items = p;
            a->max = newlen;
        }
        else {
            size_t nm = a->max * 2;
            if (nm == 0)
                nm = 1;
            while (newlen > nm)
                nm *= 2;
            void **p = (void**)LLT_REALLOC(a->items, nm * sizeof(void*));
            if (p == NULL) return;
            a->items = p;
            a->max = nm;
        }
    }
    a->len = newlen;
}

void arraylist_push(arraylist_t *a, void *elt)
{
    arraylist_grow(a, 1);
    a->items[a->len - 1] = elt;
}

void *arraylist_pop(arraylist_t *a)
{
    if (a->len == 0) return NULL;
    void *p = a->items[--a->len];
    a->items[a->len] = NULL;
    return p;
}

#ifdef __cplusplus
}
#endif
