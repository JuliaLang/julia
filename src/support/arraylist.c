// This file is a part of Julia. License is MIT: http://julialang.org/license

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

arraylist_t *arraylist_str(arraylist_t *arr, size_t size, size_t elesz)
{
    arr->elesz = elesz;
    if (size*elesz <= sizeof(arr->_space)) {
        arr->items = arr->_space;
        arr->max = sizeof(arr->_space)/elesz;
    }
    else {
        // This should probably be rounded up
        arr->items = (void **)LLT_ALLOC(size*elesz);
        if (arr->items == NULL) return NULL;
        arr->max = size;
    }
    return arr;
}

void arraylist_free(arraylist_t *arr)
{
    if (arr->items != (void **)arr->_space && arr->items)
        LLT_FREE(arr->items);
    arr->len = 0;
    arr->max = sizeof(arr->_space)/arr->elesz;
    arr->items = (void **)arr->_space;
}

void arraylist_grow(arraylist_t *arr, size_t num)
{
    size_t newlen = arr->len + num;
    if (newlen > arr->max) {
        if (arr->items == (void **)arr->_space) {
            void *pnt = (void *)LLT_ALLOC(newlen * arr->elesz);
            if (pnt == NULL) return;
            memcpy(pnt, (void *)arr->items, arr->len * arr->elesz);
            arr->max = newlen;
            arr->items = (void **)pnt;
        }
        else {
            size_t nm = arr->max * 2;
            while (newlen > nm) nm *= 2;
            void *pnt = (void *)LLT_REALLOC(arr->items, nm * arr->elesz);
            if (pnt == NULL) return;
            arr->max = nm;
            arr->items = (void **)pnt;
        }
    }
    arr->len = newlen;
}

void arraylist_push_str(arraylist_t *arr, void *elt)
{
    arraylist_grow(arr, 1);
    memcpy((void *)arr->items + (arr->len-1) * arr->elesz, elt, arr->elesz);
}

void arraylist_push(arraylist_t *arr, void *elt)
{
    arraylist_grow(arr, 1);
    ((void **)arr->items)[arr->len-1] = elt;
}

void arraylist_pop_str(arraylist_t *arr, void *elt)
{
    if (arr->len != 0) {
        arr->len--;
        memcpy(elt, (void *)arr->items + (arr->len * arr->elesz), arr->elesz);
    }
}

// remove from this list
void arraylist_remove(arraylist_t *arr, size_t pos)
{
    size_t len = --(arr->len);
    if (pos < len) {
        memcpy(arr->items + (pos*arr->elesz), arr->items + (len*arr->elesz), arr->elesz);
    }
}
#ifdef __cplusplus
}
#endif
