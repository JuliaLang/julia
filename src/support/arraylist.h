// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_ARRAYLIST_H
#define JL_ARRAYLIST_H

#define AL_N_INLINE 29

#define SMALL_AL_N_INLINE 5

#ifdef __cplusplus
extern "C" {
#endif

#include "analyzer_annotations.h"

typedef struct { // 32 words
    size_t len;
    size_t max;
    void **items;
    void *_space[AL_N_INLINE];
} arraylist_t;

JL_DLLEXPORT arraylist_t *arraylist_new(arraylist_t *a, size_t size) JL_NOTSAFEPOINT;
JL_DLLEXPORT void arraylist_free(arraylist_t *a) JL_NOTSAFEPOINT;

JL_DLLEXPORT void arraylist_push(arraylist_t *a, void *elt) JL_NOTSAFEPOINT;
JL_DLLEXPORT void *arraylist_pop(arraylist_t *a) JL_NOTSAFEPOINT;
JL_DLLEXPORT void arraylist_grow(arraylist_t *a, size_t n) JL_NOTSAFEPOINT;

typedef struct { // 8 words
    size_t len;
    size_t max;
    void **items;
    void *_space[SMALL_AL_N_INLINE];
} small_arraylist_t;


JL_DLLEXPORT small_arraylist_t *small_arraylist_new(small_arraylist_t *a, uint32_t size) JL_NOTSAFEPOINT;
JL_DLLEXPORT void small_arraylist_free(small_arraylist_t *a) JL_NOTSAFEPOINT;

JL_DLLEXPORT void small_arraylist_push(small_arraylist_t *a, void *elt) JL_NOTSAFEPOINT;
JL_DLLEXPORT void *small_arraylist_pop(small_arraylist_t *a) JL_NOTSAFEPOINT;
JL_DLLEXPORT void small_arraylist_grow(small_arraylist_t *a, uint32_t n) JL_NOTSAFEPOINT;

#ifdef __cplusplus
}
#endif

#endif
