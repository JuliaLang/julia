// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "eytzinger.h"
#include <assert.h>
#include <stdlib.h>

static int ptr_cmp(const void *l, const void *r) JL_NOTSAFEPOINT
{
    uintptr_t left = *(const uintptr_t*)l;
    uintptr_t right = *(const uintptr_t*)r;
    return (left > right) - (left < right);
}

// Build an eytzinger tree from a sorted array
static int eytzinger(uintptr_t *src, uintptr_t *dest,
                     size_t i, size_t k, size_t n) JL_NOTSAFEPOINT
{
    if (k <= n) {
        i = eytzinger(src, dest, i, 2 * k, n);
        dest[k - 1] = src[i];
        i++;
        i = eytzinger(src, dest, i, 2 * k + 1, n);
    }
    return i;
}

// Rebuild the tree from the current ranges. Caller must hold wrlock.
static void rebuild_tree(eyt_tree_t *t) JL_NOTSAFEPOINT
{
    size_t nranges = t->nranges;
    size_t end = 2 * nranges;
    size_t needed = end + 1; // + 1 for sentinel

    arraylist_grow(&t->tree, needed - t->tree.len);
    arraylist_grow(&t->idxs, needed - t->idxs.len);

    uintptr_t *tree = (uintptr_t*)t->tree.items;
    uintptr_t *idxs = (uintptr_t*)t->idxs.items;

    // Sentinel: outside any range
    idxs[end] = (uintptr_t)EYT_NOTFOUND;
    tree[end] = 1;

    for (size_t i = 0; i < end; i++) {
        eyt_range_t *r = &t->ranges[i / 2];
        uintptr_t val = (i & 1) ? r->end : r->start;
        assert(val % 4 == 0 && "Range boundary not 4-byte aligned!");
        // Encode each boundary so that the strictly-less-than predecessor search
        // in _eyt_obj_idx answers half-open [start, end) containment queries
        // correctly, including for abutting ranges. All boundaries are 4-byte
        // aligned (asserted above), so the low two bits are free; we offset a
        // start boundary by +2 and an end boundary by +1:
        //   start(s) -> s + 2   (s % 4 == 0, so this is even; low bit 0)
        //   end(e)   -> e + 1   (odd; low bit 1)
        // This gives three useful properties:
        // 1. A start and an end never encode to the same value, so all boundaries
        //    are distinct (required to build the Eytzinger tree).
        // 2. When a range ends exactly where the next one starts (abutting), the
        //    start boundary (e + 2) sorts *after* the coincident end boundary
        //    (e + 1), so a lookup at that address lands in the later range.
        // 3. The low bit distinguishes starts (even, in-range) from ends (odd,
        //    out-of-range), which eyt_tree_is_in_range tests directly.
        // We assume there are no 0-size ranges, which is safe since nothing could
        // be stored there anyway. See _eyt_obj_idx for how the query value for a
        // given address is chosen to match this encoding.
        idxs[i] = val + ((i & 1) ? 1 : 2);
    }
    qsort(idxs, end, sizeof(uintptr_t), ptr_cmp);
    t->min_addr = idxs[0];
    t->max_addr = idxs[end - 1] + 1;
    eytzinger(idxs, tree, 0, 1, end);

    // Reuse the scratch memory to store the userdata pointers
    // Still O(n log n): n iterations each doing an O(log n) binary search
    for (size_t i = 0; i < end; i++) {
        eyt_range_t *r = &t->ranges[i / 2];
        uintptr_t val = (i & 1) ? r->end : r->start;
        // This is the same computation as in the prior for loop
        uintptr_t eyt_val = val + ((i & 1) ? 1 : 2);
        size_t eyt_idx = _eyt_obj_idx(eyt_val + 1, tree, end, t->min_addr, t->max_addr);
        assert(eyt_idx < end);
        assert(tree[eyt_idx] == eyt_val &&
               "Eytzinger tree failed to find boundary!");
        if (i & 1)
            idxs[eyt_idx] = (uintptr_t)EYT_NOTFOUND;
        else
            idxs[eyt_idx] = (uintptr_t)r->data;
    }

    t->n = end;
}

JL_DLLEXPORT void eyt_tree_init(eyt_tree_t *t) JL_NOTSAFEPOINT
{
    memset(t, 0, sizeof(*t));
    arraylist_new(&t->tree, 0);
    arraylist_new(&t->idxs, 0);
    uv_rwlock_init(&t->rwlock);

    t->nranges = 0;
    t->ranges = NULL;

    // Initialize sentinel
    arraylist_push(&t->tree, (void*)1);
    arraylist_push(&t->idxs, EYT_NOTFOUND);
}

JL_DLLEXPORT void eyt_tree_add_range(eyt_tree_t *t, uintptr_t start, uintptr_t end, void *data) JL_NOTSAFEPOINT
{
    assert(start % 4 == 0 && "Range start not 4-byte aligned");
    assert(end % 4 == 0 && "Range end not 4-byte aligned");
    assert(start < end && "Empty range");

    uv_rwlock_wrlock(&t->rwlock);

    t->nranges++;
    t->ranges = (eyt_range_t*)realloc(t->ranges, t->nranges * sizeof(eyt_range_t));
    t->ranges[t->nranges - 1] = (eyt_range_t){start, end, data};
    rebuild_tree(t);

    uv_rwlock_wrunlock(&t->rwlock);
}
