// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "eytzinger.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

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

// The snapshot of a tree with no ranges
static uintptr_t eyt_empty_tree[1] = {1};
static uintptr_t eyt_empty_idxs[1] = {(uintptr_t)EYT_NOTFOUND};
static eyt_snapshot_t eyt_empty_snapshot = {0, 0, 0, eyt_empty_tree, eyt_empty_idxs, 0, NULL};

// Allocate a snapshot for `nranges` ranges, with its arrays in the same block
static eyt_snapshot_t *snapshot_alloc(size_t nranges) JL_NOTSAFEPOINT
{
    size_t n = 2 * nranges;
    size_t sz = sizeof(eyt_snapshot_t) + 2 * (n + 1) * sizeof(uintptr_t) + nranges * sizeof(eyt_range_t);
    eyt_snapshot_t *s = (eyt_snapshot_t*)malloc_s(sz);
    s->n = n;
    s->nranges = nranges;
    s->tree = (uintptr_t*)(s + 1);
    s->idxs = s->tree + n + 1;
    s->ranges = (eyt_range_t*)(s->idxs + n + 1);
    return s;
}

// Rebuild the tree of snapshot `s` from its ranges.
static void rebuild_tree(eyt_snapshot_t *s) JL_NOTSAFEPOINT
{
    size_t end = s->n;
    uintptr_t *tree = s->tree;
    uintptr_t *idxs = s->idxs;

    // Sentinel: outside any range
    idxs[end] = (uintptr_t)EYT_NOTFOUND;
    tree[end] = 1;

    for (size_t i = 0; i < end; i++) {
        eyt_range_t *r = &s->ranges[i / 2];
        uintptr_t val = (i & 1) ? r->end : r->start;
        assert(val % 4 == 0 && "Range boundary not 4-byte aligned!");
        // Encode each boundary so that the strictly-less-than predecessor search
        // in _eyt_obj_idx answers (start, end] containment queries (see
        // _eyt_addr_query for the convention), including for abutting ranges.
        // All boundaries are 4-byte aligned (asserted above), so the low two
        // bits are free; we offset a start boundary by +2 and an end boundary
        // by +1:
        //   start(s) -> s + 2   (s % 4 == 0, so this is even; low bit 0)
        //   end(e)   -> e + 1   (odd; low bit 1)
        // This gives three useful properties:
        // 1. A start and an end never encode to the same value, so all boundaries
        //    are distinct (required to build the Eytzinger tree).
        // 2. When a range ends exactly where the next one starts (abutting),
        //    the shared address resolves to the *earlier* range — where a
        //    trailing zero-size singleton's value pointer lands.
        // 3. The low bit distinguishes starts (even, in-range) from ends (odd,
        //    out-of-range), which eyt_tree_is_in_range tests directly.
        // We assume there are no 0-size ranges, which is safe since nothing could
        // be stored there anyway. See _eyt_obj_idx for how the query value for a
        // given address is chosen to match this encoding.
        idxs[i] = val + ((i & 1) ? 1 : 2);
    }
    qsort(idxs, end, sizeof(uintptr_t), ptr_cmp);
    s->min_addr = idxs[0];
    s->max_addr = idxs[end - 1] + 1;
    eytzinger(idxs, tree, 0, 1, end);

    // Reuse the scratch memory to store the userdata pointers
    // Still O(n log n): n iterations each doing an O(log n) binary search
    for (size_t i = 0; i < end; i++) {
        eyt_range_t *r = &s->ranges[i / 2];
        uintptr_t val = (i & 1) ? r->end : r->start;
        // This is the same computation as in the prior for loop
        uintptr_t eyt_val = val + ((i & 1) ? 1 : 2);
        size_t eyt_idx = _eyt_obj_idx(eyt_val + 1, tree, end, s->min_addr, s->max_addr);
        assert(eyt_idx < end);
        assert(tree[eyt_idx] == eyt_val &&
               "Eytzinger tree failed to find boundary!");
        if (i & 1)
            idxs[eyt_idx] = (uintptr_t)EYT_NOTFOUND;
        else
            idxs[eyt_idx] = (uintptr_t)r->data;
    }
}


JL_DLLEXPORT void eyt_tree_init(eyt_tree_t *t, void (*retire)(void *snapshot)) JL_NOTSAFEPOINT
{
    memset(t, 0, sizeof(*t));
    jl_atomic_store_relaxed(&t->snapshot, &eyt_empty_snapshot);
    uv_mutex_init(&t->writelock);
    t->retire = retire;
}

JL_DLLEXPORT size_t eyt_tree_add_range(eyt_tree_t *t, uintptr_t start, uintptr_t end, void *data) JL_NOTSAFEPOINT
{
    assert(start % 4 == 0 && "Range start not 4-byte aligned");
    assert(end % 4 == 0 && "Range end not 4-byte aligned");
    assert(start < end && "Empty range");

    uv_mutex_lock(&t->writelock);

    // build the new version beside the current one, which readers keep using
    eyt_snapshot_t *old = jl_atomic_load_relaxed(&t->snapshot);
    size_t i = old->nranges;
    eyt_snapshot_t *s = snapshot_alloc(i + 1);
    if (i > 0)
        memcpy(s->ranges, old->ranges, i * sizeof(eyt_range_t));
    s->ranges[i] = (eyt_range_t){start, end, data};
    rebuild_tree(s);

    jl_atomic_store_release(&t->snapshot, s);
    if (old != &eyt_empty_snapshot && t->retire)
        t->retire(old);

    uv_mutex_unlock(&t->writelock);
    return i;
}
