// This file is a part of Julia. License is MIT: https://julialang.org/license

// World age segment bookkeeping. See the overview comment next to
// jl_world_reaches in julia_internal.h: a world is a packed
// (segment, index) pair, segments form an append-only DAG whose edges are
// fixed at segment creation, and segment-level ancestry is answered by an
// immutable per-segment bitset so that readers require no synchronization
// beyond an acquire load of the segment pointer.
//
// Segments additionally carry a serializable identity (see
// enum jl_world_seg_kind): the unmaterialized boot prefix is shared verbatim
// between every process built on the same system image; this process's own
// spine runs are numbered consecutively; and runs grafted from a loaded
// package image are keyed by (image id, run ordinal), which is stable across
// processes because a given image file grafts identically everywhere.

#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

// Append-only table of materialized segments. Boot-prefix segments are
// implicit and never materialized; their entries stay NULL and the chain
// fallback in jl_world_seg_reaches gives the correct answers for them.
static _Atomic(jl_world_segment_t*) world_segments[JL_WORLD_MAX_SEGMENTS];
static uint32_t world_nsegments_ = 1; // protected by world_counter_lock
static uint32_t world_nruns = 0;      // spine run ordinals handed out; protected by world_counter_lock

JL_DLLEXPORT jl_world_segment_t *jl_world_get_segment(size_t seg) JL_NOTSAFEPOINT
{
    if (seg >= JL_WORLD_MAX_SEGMENTS)
        return NULL;
    return jl_atomic_load_acquire(&world_segments[seg]);
}

JL_DLLEXPORT uint32_t jl_world_nsegments(void) JL_NOTSAFEPOINT
{
    return world_nsegments_;
}

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
// implicit parent; otherwise it is the trunk (main-branch) parent, and the
// remaining parents are side branches merged at this segment's base world.
// Joins are asymmetric: a side branch's events order after every main-branch
// event before the join point and before every one after it, which is
// recorded in join_pos (the segment's total order over its visible cone).
static jl_world_segment_t *world_new_segment_locked(size_t chain_parent, size_t *parent_worlds, size_t nparents)
{
    uint32_t id = world_nsegments_;
    assert(id < JL_WORLD_MAX_SEGMENTS);
    size_t nwords = (id + 63) / 64;
    jl_world_segment_t *seg = (jl_world_segment_t*)calloc_s(sizeof(jl_world_segment_t) + nwords * sizeof(uint64_t));
    seg->id = id;
    seg->anc_nbits = id; // ancestors always have smaller ids
    seg->kind = JL_WORLD_SEG_OTHER;
    jl_atomic_store_relaxed(&seg->end_idx, JL_WORLD_IDX_MASK);
    seg->nparents = (uint32_t)nparents + (chain_parent != ~(size_t)0);
    seg->parents = seg->nparents ? (size_t*)malloc_s(seg->nparents * sizeof(size_t)) : NULL;
    seg->join_pos = id ? (size_t*)calloc_s(id * sizeof(size_t)) : NULL;
    size_t base = (size_t)id << JL_WORLD_IDX_BITS;
    uint32_t pi = 0;
    for (size_t i = 0; i <= nparents; i++) {
        size_t pw = i == 0 ? chain_parent : parent_worlds[i - 1];
        if (pw == ~(size_t)0)
            continue;
        int is_trunk = pi == 0; // the first parent continues the trunk
        seg->parents[pi++] = pw;
        size_t psid = jl_world_seg(pw);
        assert(psid < id && "parent segment must precede the new segment");
        jl_world_segment_t *pseg = jl_atomic_load_relaxed(&world_segments[psid]);
        if (pseg) {
            seg->anc_row[psid >> 6] |= (uint64_t)1 << (psid & 63);
            size_t pwords = (pseg->anc_nbits + 63) / 64;
            for (size_t w = 0; w < pwords; w++)
                seg->anc_row[w] |= pseg->anc_row[w];
            if (is_trunk) {
                // inherit the trunk parent's total order and extend the trunk
                memcpy(seg->join_pos, pseg->join_pos, pseg->anc_nbits * sizeof(size_t));
                seg->join_pos[psid] = JL_WORLD_POS_TRUNK;
            }
            else {
                // a side branch: everything newly visible through it joins
                // the trunk at this segment's base world
                for (uint32_t b = 0; b <= psid; b++) {
                    if (b == psid || ((pseg->anc_row[b >> 6] >> (b & 63)) & 1)) {
                        if (seg->join_pos[b] == 0)
                            seg->join_pos[b] = base;
                    }
                }
            }
        }
        else {
            // an unmaterialized (boot prefix) parent: its history is the
            // whole linear chain of segments up to and including it
            for (size_t b = 0; b <= psid; b++) {
                seg->anc_row[b >> 6] |= (uint64_t)1 << (b & 63);
                if (seg->join_pos[b] == 0)
                    seg->join_pos[b] = is_trunk ? JL_WORLD_POS_TRUNK : base;
            }
        }
    }
    world_nsegments_ = id + 1;
    jl_atomic_store_release(&world_segments[id], seg);
    return seg;
}

// Create a new segment, without moving the world counter into it, whose
// parents are the segments of `parent_worlds` (referenced at full extent).
// Returns the segment's first world. Exposed for testing; segments created
// this way carry no serializable identity.
JL_DLLEXPORT size_t jl_world_new_segment(size_t *parent_worlds, size_t nparents)
{
    JL_LOCK(&world_counter_lock);
    if (world_nsegments_ >= JL_WORLD_MAX_SEGMENTS) {
        JL_UNLOCK(&world_counter_lock);
        jl_error("world age segment limit exceeded");
    }
    jl_world_segment_t *seg = world_new_segment_locked(~(size_t)0, parent_worlds, nparents);
    JL_UNLOCK(&world_counter_lock);
    return (size_t)seg->id << JL_WORLD_IDX_BITS;
}

// Requires world_counter_lock: close the currently open segment at the
// current counter and continue counting worlds in a fresh spine run whose
// extra parents (beyond the closed segment) are `extra_parent_worlds`.
static jl_world_segment_t *world_advance_locked(size_t *extra_parent_worlds, size_t nextra)
{
    size_t cur = jl_atomic_load_relaxed(&jl_world_counter);
    jl_world_segment_t *seg = world_new_segment_locked(cur, extra_parent_worlds, nextra);
    seg->kind = JL_WORLD_SEG_RUN;
    seg->run = world_nruns++;
    jl_world_segment_t *oldseg = jl_atomic_load_relaxed(&world_segments[jl_world_seg(cur)]);
    if (oldseg)
        jl_atomic_store_relaxed(&oldseg->end_idx, jl_world_idx(cur));
    jl_atomic_store_release(&jl_world_counter, (size_t)seg->id << JL_WORLD_IDX_BITS);
    return seg;
}

// Position of `w` in the total order that the observer segment imposes on
// its visible cone: `w` itself for worlds on the observer's trunk, the merge
// point for worlds of merged side branches. Positions are trunk worlds and
// compare exactly as integers.
static size_t world_pos(size_t w, jl_world_segment_t *obs) JL_NOTSAFEPOINT
{
    size_t ws = jl_world_seg(w);
    if (obs == NULL || ws == obs->id)
        return w; // the observer's own segment, or a fully linear history
    size_t p = ws < obs->anc_nbits ? obs->join_pos[ws] : 0;
    if (p == JL_WORLD_POS_TRUNK || p == 0)
        return w; // on the trunk (0: not visible; fall back to chain order)
    return p;
}

// The earliest world in the observer's total order whose history includes
// both `a` and `b`. For comparable worlds this is simply the later of the
// two; for worlds on different branches it is the later of their merge
// points on the observer's trunk.
JL_DLLEXPORT size_t jl_world_join(size_t a, size_t b, size_t observer) JL_NOTSAFEPOINT
{
    if (jl_world_reaches(a, b))
        return b;
    if (jl_world_reaches(b, a))
        return a;
    jl_world_segment_t *obs = jl_world_get_segment(jl_world_seg(observer));
    size_t pa = world_pos(a, obs);
    size_t pb = world_pos(b, obs);
    return pa > pb ? pa : pb;
}

// The join with the current spine head as the observer.
JL_DLLEXPORT size_t jl_world_spine_join(size_t a, size_t b) JL_NOTSAFEPOINT
{
    return jl_world_join(a, b, jl_atomic_load_acquire(&jl_world_counter));
}

// The three-point predicate `a preceq_observer b`: does `a` come at or
// before `b` in the total order that the observer's segment imposes on its
// visible history? Trunk worlds order as themselves; a merged side branch's
// worlds order at its merge point (after the main-branch events preceding
// the join, before those following it), and within one merge point, by the
// side branch's own total order, recursively.
JL_DLLEXPORT int jl_world_ordered_before(size_t a, size_t b, size_t observer) JL_NOTSAFEPOINT
{
    if (a == b)
        return 1;
    if (jl_world_reaches(a, b))
        return 1;
    if (jl_world_reaches(b, a))
        return 0;
    jl_world_segment_t *obs = jl_world_get_segment(jl_world_seg(observer));
    size_t pa = world_pos(a, obs);
    size_t pb = world_pos(b, obs);
    if (pa != pb)
        return pa < pb;
    // both worlds joined the observer's trunk at the same merge point:
    // order them by the total order of the side branch that merged there
    jl_world_segment_t *pseg = jl_world_get_segment(jl_world_seg(pa));
    if (pseg != NULL) {
        for (uint32_t i = 1; i < pseg->nparents; i++) {
            size_t pw = pseg->parents[i];
            int ra = jl_world_reaches(a, pw);
            int rb = jl_world_reaches(b, pw);
            if (ra && rb) {
                assert(jl_world_seg(pw) != jl_world_seg(observer) && "side branch cannot be the observer's segment");
                return jl_world_ordered_before(a, b, pw);
            }
            if (ra || rb)
                return ra; // side branches merged together order by parent position
        }
    }
    // not visible through materialized structure: fall back to chain order
    return a < b;
}

// Close the currently open segment at the current world counter and continue
// counting worlds in a fresh spine run. `extra_parent_worlds` (possibly
// none) name additional history -- e.g. a grafted image's final world --
// merged into the new run. Returns the new run's first world, which becomes
// the current world counter value.
JL_DLLEXPORT size_t jl_world_advance_into_segment(size_t *extra_parent_worlds, size_t nextra)
{
    JL_LOCK(&world_counter_lock);
    if (world_nsegments_ >= JL_WORLD_MAX_SEGMENTS) {
        JL_UNLOCK(&world_counter_lock);
        jl_error("world age segment limit exceeded");
    }
    jl_world_segment_t *seg = world_advance_locked(extra_parent_worlds, nextra);
    size_t w = (size_t)seg->id << JL_WORLD_IDX_BITS;
    JL_UNLOCK(&world_counter_lock);
    return w;
}

// Called once after a system image restore has set jl_world_counter: the
// worlds the image was built at become the closed, shared boot prefix
// (reserving segment ids for them if the image's counter was itself packed),
// and this process's first spine run starts in a fresh segment. This keeps
// every parent reference to the prefix exact: prefix segments are referenced
// only at full extent, never mid-segment.
JL_DLLEXPORT void jl_world_init_runs(void)
{
    JL_LOCK(&world_counter_lock);
    size_t cur = jl_atomic_load_relaxed(&jl_world_counter);
    uint32_t reserve = (uint32_t)jl_world_seg(cur) + 1;
    if (world_nsegments_ < reserve)
        world_nsegments_ = reserve;
    assert(world_nruns == 0 && "spine runs already initialized");
    world_advance_locked(NULL, 0);
    JL_UNLOCK(&world_counter_lock);
}

// The materialized segment for run `run` of the image identified by
// `image_id`, or ~(size_t)0 if that run has not been grafted.
JL_DLLEXPORT size_t jl_world_image_run(uint64_t image_id, uint32_t run) JL_NOTSAFEPOINT
{
    uint32_t n = world_nsegments_;
    for (uint32_t i = 0; i < n; i++) {
        jl_world_segment_t *seg = jl_atomic_load_acquire(&world_segments[i]);
        if (seg && seg->kind == JL_WORLD_SEG_IMAGE && seg->image_id == image_id && seg->run == run)
            return (size_t)i << JL_WORLD_IDX_BITS;
    }
    return ~(size_t)0;
}

// Graft run `run` of the image identified by `image_id`, whose parents are
// the segments of `parent_worlds` at full extent. Returns the run's first
// world. The counter does not move; the caller is expected to eventually
// merge the image's final run into the spine with
// jl_world_advance_into_segment.
JL_DLLEXPORT size_t jl_world_new_image_run(uint64_t image_id, uint32_t run, size_t *parent_worlds, size_t nparents)
{
    JL_LOCK(&world_counter_lock);
    if (world_nsegments_ >= JL_WORLD_MAX_SEGMENTS) {
        JL_UNLOCK(&world_counter_lock);
        jl_error("world age segment limit exceeded");
    }
    assert(jl_world_image_run(image_id, run) == ~(size_t)0 && "image run already grafted");
    jl_world_segment_t *seg = world_new_segment_locked(~(size_t)0, parent_worlds, nparents);
    seg->kind = JL_WORLD_SEG_IMAGE;
    seg->image_id = image_id;
    seg->run = run;
    size_t w = (size_t)seg->id << JL_WORLD_IDX_BITS;
    JL_UNLOCK(&world_counter_lock);
    return w;
}

#ifdef __cplusplus
}
#endif
