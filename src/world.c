// This file is a part of Julia. License is MIT: https://julialang.org/license

// World age segment bookkeeping. See the overview comment next to
// jl_world_reaches in julia_internal.h: a world is a packed
// (segment, index) pair, segments form an append-only DAG whose edges are
// fixed at segment creation, and segment-level ancestry is answered by an
// immutable per-segment bitset so that readers require no synchronization
// beyond an acquire load of the segment pointer.

#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    uint32_t id;
    uint32_t anc_nbits;         // number of valid bits in anc_row (== id)
    _Atomic(size_t) end_idx;    // terminal index once closed; JL_WORLD_IDX_MASK while open
    uint64_t anc_row[];         // ancestor segment bitset, immutable after publication
} jl_world_segment_t;

// Append-only table of materialized segments. Segment 0 (boot) is implicit
// and never materialized; its entry stays NULL and the chain fallback in
// jl_world_seg_reaches gives the correct answers for it.
static _Atomic(jl_world_segment_t*) world_segments[JL_WORLD_MAX_SEGMENTS];
static uint32_t world_nsegments = 1; // protected by world_counter_lock

JL_DLLEXPORT int jl_world_seg_reaches(size_t sa, size_t sb) JL_NOTSAFEPOINT
{
    if (sa == sb)
        return 1;
    jl_world_segment_t *seg = sb < JL_WORLD_MAX_SEGMENTS ?
        jl_atomic_load_acquire(&world_segments[sb]) : NULL;
    if (seg == NULL)
        return sa < sb; // unmaterialized segment: pure chain order
    return sa < seg->anc_nbits && ((seg->anc_row[sa >> 6] >> (sa & 63)) & 1);
}

// Requires world_counter_lock. `chain_parent == ~(size_t)0` means no
// implicit parent.
static jl_world_segment_t *world_new_segment_locked(size_t chain_parent, size_t *parent_worlds, size_t nparents)
{
    uint32_t id = world_nsegments;
    assert(id < JL_WORLD_MAX_SEGMENTS);
    size_t nwords = (id + 63) / 64;
    jl_world_segment_t *seg = (jl_world_segment_t*)calloc_s(sizeof(jl_world_segment_t) + nwords * sizeof(uint64_t));
    seg->id = id;
    seg->anc_nbits = id; // ancestors always have smaller ids
    jl_atomic_store_relaxed(&seg->end_idx, JL_WORLD_IDX_MASK);
    for (size_t i = 0; i <= nparents; i++) {
        size_t pw = i == 0 ? chain_parent : parent_worlds[i - 1];
        if (pw == ~(size_t)0)
            continue;
        size_t psid = jl_world_seg(pw);
        assert(psid < id && "parent segment must precede the new segment");
        seg->anc_row[psid >> 6] |= (uint64_t)1 << (psid & 63);
        jl_world_segment_t *pseg = jl_atomic_load_relaxed(&world_segments[psid]);
        assert((pseg || psid == 0) && "non-boot parent segment must be materialized");
        if (pseg) {
            size_t pwords = (pseg->anc_nbits + 63) / 64;
            for (size_t w = 0; w < pwords; w++)
                seg->anc_row[w] |= pseg->anc_row[w];
        }
    }
    world_nsegments = id + 1;
    jl_atomic_store_release(&world_segments[id], seg);
    return seg;
}

// Create a new segment, without moving the world counter into it, whose
// parents are the segments of `parent_worlds` (referenced at full extent).
// Returns the segment's first world. Exposed for grafting serialized world
// histories and for testing.
JL_DLLEXPORT size_t jl_world_new_segment(size_t *parent_worlds, size_t nparents)
{
    JL_LOCK(&world_counter_lock);
    if (world_nsegments >= JL_WORLD_MAX_SEGMENTS) {
        JL_UNLOCK(&world_counter_lock);
        jl_error("world age segment limit exceeded");
    }
    jl_world_segment_t *seg = world_new_segment_locked(~(size_t)0, parent_worlds, nparents);
    JL_UNLOCK(&world_counter_lock);
    return (size_t)seg->id << JL_WORLD_IDX_BITS;
}

// Close the currently open segment at the current world counter and continue
// counting worlds in a fresh child segment. `extra_parent_worlds` (possibly
// none) name additional history -- e.g. a grafted image's final world --
// merged into the new segment. Returns the new segment's first world, which
// becomes the current world counter value.
JL_DLLEXPORT size_t jl_world_advance_into_segment(size_t *extra_parent_worlds, size_t nextra)
{
    JL_LOCK(&world_counter_lock);
    if (world_nsegments >= JL_WORLD_MAX_SEGMENTS) {
        JL_UNLOCK(&world_counter_lock);
        jl_error("world age segment limit exceeded");
    }
    size_t cur = jl_atomic_load_relaxed(&jl_world_counter);
    jl_world_segment_t *seg = world_new_segment_locked(cur, extra_parent_worlds, nextra);
    jl_world_segment_t *oldseg = jl_atomic_load_relaxed(&world_segments[jl_world_seg(cur)]);
    if (oldseg)
        jl_atomic_store_relaxed(&oldseg->end_idx, jl_world_idx(cur));
    size_t w = (size_t)seg->id << JL_WORLD_IDX_BITS;
    jl_atomic_store_release(&jl_world_counter, w);
    JL_UNLOCK(&world_counter_lock);
    return w;
}

// Close the currently open segment and continue the counter in a fresh
// segment, additionally materializing a side segment G (a child of the
// closed spine head) that the new spine segment also merges. A serialized
// image's world history is grafted into G: entries and world tokens from the
// image reference (G, offset) worlds, which are part of the new spine's
// history but isolated from invalidation events that happen after the graft.
// Returns G's first world.
JL_DLLEXPORT size_t jl_world_graft_segment(void)
{
    JL_LOCK(&world_counter_lock);
    if (world_nsegments + 1 >= JL_WORLD_MAX_SEGMENTS) {
        JL_UNLOCK(&world_counter_lock);
        jl_error("world age segment limit exceeded");
    }
    size_t cur = jl_atomic_load_relaxed(&jl_world_counter);
    jl_world_segment_t *gseg = world_new_segment_locked(cur, NULL, 0);
    size_t gbase = (size_t)gseg->id << JL_WORLD_IDX_BITS;
    jl_world_segment_t *sseg = world_new_segment_locked(cur, &gbase, 1);
    jl_world_segment_t *oldseg = jl_atomic_load_relaxed(&world_segments[jl_world_seg(cur)]);
    if (oldseg)
        jl_atomic_store_relaxed(&oldseg->end_idx, jl_world_idx(cur));
    jl_atomic_store_release(&jl_world_counter, (size_t)sseg->id << JL_WORLD_IDX_BITS);
    JL_UNLOCK(&world_counter_lock);
    return gbase;
}

#ifdef __cplusplus
}
#endif
