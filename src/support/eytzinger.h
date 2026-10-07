// This file is a part of Julia. License is MIT: https://julialang.org/license

// Eytzinger tree for fast address range lookups. Readers take no lock: a
// writer publishes a new immutable snapshot of the whole tree, and a reader
// uses the snapshot it loaded. The tree's owner retires a replaced snapshot
// once no reader can still hold it, so a reader must not pass a safepoint
// while using one: see _eyt_tree_snapshot.
// See https://algorithmica.org/en/eytzinger

#ifndef JL_EYTZINGER_H
#define JL_EYTZINGER_H

#include <uv.h>
#include "dtypes.h"
#include "../julia_atomics.h"

#ifdef __cplusplus
extern "C" {
#endif

// Search an Eytzinger tree for the largest boundary strictly less than the
// query value `q`. Returns the tree index (0-based) of that boundary, or `n`
// (the sentinel) if there is none.
// `tree` has `n + 1` entries (n boundaries + 1 sentinel).
//
// The boundaries are encoded by rebuild_tree (see there): a start boundary `s`
// is stored as `s + 2` and an end boundary `e` as `e + 1`. Querying with
// `q = addr + 1` (see _eyt_addr_query) makes the strictly-less-than
// predecessor the range's start boundary exactly when `start < addr <= end`.
//
// Containment is start-exclusive, end-inclusive because queries are object
// pointers into heap-image blobs (the convention staticdata.c asserts as
// `base < obj && obj <= base + size`): a blob's base is its first object's
// *tag* word, never an object — but it can equal the one-past-tag value
// pointer of a zero-size singleton bump-allocated just before the blob
// (#62521). Symmetrically, a trailing zero-size singleton's value pointer
// equals the blob's end.
static inline size_t _eyt_obj_idx(uintptr_t q, uintptr_t *tree, size_t n,
                                  uintptr_t min_addr, uintptr_t max_addr) JL_NOTSAFEPOINT
{
    if (n == 0)
        return n;
    assert(n % 2 == 0 && "Eytzinger tree not even length!");
    if (q <= min_addr || q > max_addr)
        return n;
    size_t k = 1;
    while (k <= n) {
        int greater = (q > tree[k - 1]);
        k <<= 1;
        k |= greater;
    }
    k >>= (__builtin_ctzll(k) + 1);
    assert(k != 0);
    assert(k <= n && "Eytzinger tree index out of bounds!");
    assert(tree[k - 1] < q && "Failed to find lower bound for object!");
    return k - 1;
}

// Map a 4-byte-aligned address to the query value used to test containment in
// the encoded tree. See _eyt_obj_idx and rebuild_tree for the encoding.
static inline uintptr_t _eyt_addr_query(uintptr_t addr) JL_NOTSAFEPOINT
{
    return addr + 1;
}

typedef struct {
    uintptr_t start;
    uintptr_t end;
    void *data; // caller-defined per-range metadata
} eyt_range_t;

// One version of the tree, immutable once published. Its arrays live in the
// same allocation as the header.
typedef struct {
    size_t n; // 2 * nranges (tree/idxs have n+1 entries)
    uintptr_t min_addr;
    uintptr_t max_addr;
    uintptr_t *tree; // Eytzinger-ordered encoded boundaries, then a sentinel
    uintptr_t *idxs; // per tree entry: the range's data for a start boundary, EYT_NOTFOUND for an end or the sentinel
    size_t nranges;
    eyt_range_t *ranges; // one entry per registered range, in insertion order
} eyt_snapshot_t;

typedef struct eyt_tree_t {
    _Atomic(eyt_snapshot_t*) snapshot; // the current version
    uv_mutex_t writelock; // serializes writers
    // Called under the write lock with the snapshot that a new one replaced.
    // Readers may still hold it, so the callback must not free it until every
    // thread has passed a safepoint (jl_gc_add_quiescent does), unless there
    // are no concurrent readers. NULL leaks it.
    void (*retire)(void *snapshot);
} eyt_tree_t;

// Sentinel value stored in `idxs` for end-boundaries and out-of-range positions.
#define EYT_NOTFOUND ((void*)1)

JL_DLLEXPORT void eyt_tree_init(eyt_tree_t *t, void (*retire)(void *snapshot)) JL_NOTSAFEPOINT;

// Add a range covering the object pointers in (start, end] with caller-defined
// data, publishing a new snapshot, and return the range's index: ranges are
// numbered in insertion order. Thread-safe against readers and other writers.
JL_DLLEXPORT size_t eyt_tree_add_range(eyt_tree_t *t, uintptr_t start, uintptr_t end, void *data) JL_NOTSAFEPOINT;

// The current snapshot, for the readers below: it is valid only until the
// thread next passes a safepoint, so a reader uses it within one
// JL_NOTSAFEPOINT body and never stores or returns it (range data has the
// caller's lifetime). A relaxed load suffices: every read of the snapshot
// depends on the loaded address, which orders it after the writer's release
// store on every target we support.
static inline eyt_snapshot_t *_eyt_tree_snapshot(eyt_tree_t *t) JL_NOTSAFEPOINT
{
    return jl_atomic_load_relaxed(&t->snapshot);
}

// Returns whether `addr` is inside any registered range (start-exclusive,
// end-inclusive; see _eyt_addr_query).
static inline int eyt_tree_is_in_range(eyt_tree_t *t, uintptr_t addr) JL_NOTSAFEPOINT
{
    eyt_snapshot_t *s = _eyt_tree_snapshot(t);
    size_t idx = _eyt_obj_idx(_eyt_addr_query(addr), s->tree, s->n, s->min_addr, s->max_addr);
    return (s->tree[idx] & 1) == 0;
}

// Returns the caller-defined data for the range containing `addr`, or EYT_NOTFOUND.
static inline void *eyt_tree_find_data(eyt_tree_t *t, uintptr_t addr) JL_NOTSAFEPOINT
{
    eyt_snapshot_t *s = _eyt_tree_snapshot(t);
    size_t idx = _eyt_obj_idx(_eyt_addr_query(addr), s->tree, s->n, s->min_addr, s->max_addr);
    return (void*)s->idxs[idx];
}

// The number of registered ranges.
static inline size_t eyt_tree_nranges(eyt_tree_t *t) JL_NOTSAFEPOINT
{
    return _eyt_tree_snapshot(t)->nranges;
}

// Returns the caller-defined data of the range with index `i`, or EYT_NOTFOUND.
static inline void *eyt_tree_range_data(eyt_tree_t *t, size_t i) JL_NOTSAFEPOINT
{
    eyt_snapshot_t *s = _eyt_tree_snapshot(t);
    return i < s->nranges ? s->ranges[i].data : EYT_NOTFOUND;
}

#ifdef __cplusplus
}
#endif

#endif
