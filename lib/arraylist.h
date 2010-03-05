#ifndef __ARRAYLIST_H_
#define __ARRAYLIST_H_

#define AL_N_INLINE 29

typedef struct {
    size_t len;
    size_t max;
    void **items;
    void *_space[AL_N_INLINE];
} arraylist_t;

arraylist_t *arraylist_new(arraylist_t *a, size_t size);
void arraylist_free(arraylist_t *a);

void arraylist_push(arraylist_t *a, void *elt);
void *arraylist_pop(arraylist_t *a);

#endif
