// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef ARRAYLIST_H
#define ARRAYLIST_H

#ifdef __cplusplus
extern "C" {
#endif

#define AL_N_INLINE     28

typedef struct {
    void *items;
    size_t len;
    size_t max;
    size_t elesz;
    unsigned char _space[AL_N_INLINE*sizeof(void *)];
} arraylist_t;

void arraylist_free(arraylist_t *arr);
void arraylist_grow(arraylist_t *arr, size_t num);

arraylist_t *arraylist_str(arraylist_t *arr, size_t size, size_t elesz);
void arraylist_push_str(arraylist_t *arr, void *elt);
void arraylist_pop_str(arraylist_t *arr, void *elt);
void arraylist_remove(arraylist_t *arr, size_t pos);

#define arraylist_new(arr, size) arraylist_str(arr, size, sizeof(void *))
void *arraylist_pop(arraylist_t *arr);
void arraylist_push(arraylist_t *arr, void *elt);

#ifdef __cplusplus
}
#endif

#endif
