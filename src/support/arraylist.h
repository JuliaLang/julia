// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_ARRAYLIST_H
#define JL_ARRAYLIST_H

#define AL_N_INLINE 29

#ifdef __cplusplus
extern "C" {
#endif

#include "analyzer_annotations.h"

typedef struct {
    size_t len;
    size_t max;
    void **items;
    void *_space[AL_N_INLINE];
} arraylist_t;

arraylist_t *arraylist_new(arraylist_t *a, size_t size) JL_NOTSAFEPOINT;
void arraylist_free(arraylist_t *a) JL_NOTSAFEPOINT;

void arraylist_push(arraylist_t *a, void *elt) JL_NOTSAFEPOINT;
void *arraylist_pop(arraylist_t *a) JL_NOTSAFEPOINT;
void arraylist_grow(arraylist_t *a, size_t n) JL_NOTSAFEPOINT;

#ifdef __cplusplus
}
#endif

#endif
