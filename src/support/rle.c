// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "rle.h"

#ifdef __cplusplus
extern "C" {
#endif

/* iteration */

rle_iter_state rle_iter_init(uint64_t key0)
{
    rle_iter_state state = {-1, 0, key0};
    return state;
}

int rle_iter_increment(rle_iter_state *state, size_t len, uint64_t *rletable, size_t npairs)
{
    state->i += 1;
    size_t i = state->i, j = state->j;
    if (i >= len)
        return 0;
    if (rletable) {
        while (j < npairs && i >= rletable[j+1]) {
            state->key = rletable[j];
            j += 2;
        }
        state->j = j;
    }
    return 1;
}

/* indexing */

void rle_index_to_reference(rle_reference *rr, size_t i, uint64_t *rletable, size_t npairs, uint64_t key0)
{
    if (!rletable) {
        rr->key = key0;
        rr->index = i;
        return;
    }
    // Determine the active key
    uint64_t key = key0;
    size_t jj = 0;
    while (jj < npairs && i >= rletable[jj+1]) {
        key = rletable[jj];
        jj += 2;
    }
    // Subtract the number of preceding items with different keys
    uint64_t ckey = key0;
    size_t j, start = 0, index = i;
    for (j = 0; j < jj; j+=2) {
        if (key != ckey)
            index -= rletable[j+1] - start;
        ckey = rletable[j];
        start = rletable[j+1];
    }
    // Return the result
    rr->key = key;
    rr->index = index;
    return;
}

size_t rle_reference_to_index(rle_reference *rr, uint64_t *rletable, size_t npairs, uint64_t key0)
{
    uint64_t key = rr->key;
    size_t index = rr->index, i = index;
    if (!rletable) {
        assert(key == key0);
        return i;
    }
    uint64_t ckey = key0;
    size_t j, start = 0, n;
    for (j = 0; j < npairs; j+=2) {
        n = rletable[j+1] - start;
        if (key != ckey)
            i += n;
        else {
            if (index < n)
                break;
            index -= n;
        }
        ckey = rletable[j];
        start = rletable[j+1];
    }
    return i;
}


#ifdef __cplusplus
}
#endif
