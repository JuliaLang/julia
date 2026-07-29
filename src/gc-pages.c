// This file is a part of Julia. License is MIT: https://julialang.org/license

// Slab-based page allocator: pool pages, big pages for objects above the pool
// limit, and malloc'd Memory buffers. See the overview comment in gc-stock.h.

#include "gc-common.h"
#include "gc-stock.h"

#ifdef __cplusplus
extern "C" {
#endif

uv_mutex_t gc_big_lock;

static size_t gc_big_class_sz[GC_N_BIG_CLASSES];

// Map an allocation size (<= GC_BIG_CLASS_MAX_SZ) to its size class.
STATIC_INLINE int gc_big_szclass(size_t sz) JL_NOTSAFEPOINT
{
    if (sz <= GC_BIG_CLASS_MIN_SZ)
        return 0;
    // 4 classes per power of two: for sz in (2^lg, 2^(lg+1)], class sizes are
    // 2^lg + (r+1) * 2^(lg-2) for r in 0:3
    int lg = 63 - __builtin_clzll((uint64_t)(sz - 1));
    size_t base = (size_t)1 << lg;
    int sub = (int)((sz - base - 1) >> (lg - 2));
    int c = 4 * (lg - 11) + sub + 1;
    assert(c > 0 && c < GC_N_BIG_CLASSES);
    assert(gc_big_class_sz[c] >= sz && (c == 0 || gc_big_class_sz[c - 1] < sz));
    return c;
}

// log2 of the page size used for a size class (mimalloc's tiering rule:
// a page must hold several objects of its class, see gc-stock.h)
STATIC_INLINE int gc_big_page_lg2(size_t class_sz) JL_NOTSAFEPOINT
{
    if (class_sz <= GC_BIG_SMALL_PAGE_MAX_SZ)
        return 16; // 64 KiB
    if (class_sz <= GC_BIG_MEDIUM_PAGE_MAX_SZ)
        return 19; // 512 KiB
    return GC_SLAB_LG2; // a whole slab
}

// per-class stacks of pages with free slots; popped lock-free by mutators,
// (re)filled only during sweep while the world is stopped. Refill prefers the
// fullest bucket (see gc_occupancy_bucket).
typedef struct {
    _Atomic(jl_gc_bigpagemeta_t *) bottom;
} gc_big_page_stack_t;

static gc_big_page_stack_t gc_big_partial_pages[GC_N_BIG_CLASSES][GC_OCCUPANCY_BUCKETS];

STATIC_INLINE int gc_big_page_bucket(jl_gc_bigpagemeta_t *pg) JL_NOTSAFEPOINT
{
    return gc_occupancy_bucket((size_t)pg->nfree * pg->osize,
                               (size_t)1 << pg->page_lg2);
}

STATIC_INLINE void gc_big_stack_push(gc_big_page_stack_t *st, jl_gc_bigpagemeta_t *pg) JL_NOTSAFEPOINT
{
    // only called during sweep with the world stopped, no CAS needed
    pg->next = jl_atomic_load_relaxed(&st->bottom);
    jl_atomic_store_relaxed(&st->bottom, pg);
}

STATIC_INLINE jl_gc_bigpagemeta_t *gc_big_stack_pop(gc_big_page_stack_t *st) JL_NOTSAFEPOINT
{
    while (1) {
        jl_gc_bigpagemeta_t *pg = jl_atomic_load_relaxed(&st->bottom);
        if (pg == NULL)
            return NULL;
        if (jl_atomic_cmpswap(&st->bottom, &pg, pg->next))
            return pg;
        jl_cpu_pause();
    }
}

// ======================================================================== //
// slab map: two-level radix tree from `addr >> GC_SLAB_LG2` to slab metadata.
// Every address handed out by this allocator has its containing (2 MiB-
// aligned) slab registered here; huge mappings are 2 MiB-aligned so their
// base address identifies them uniquely.
// ======================================================================== //

#ifdef _P64
#define GC_SLAB_MAP0_BITS 16 // bits 21..36
#define GC_SLAB_MAP1_BITS 9  // bits 37..45
#define GC_SLAB_MAP2_BITS 18 // bits 46..63
#else
// Only bits 21..31 exist above the slab shift, so a single leaf level covers the
// whole address space and the upper levels collapse to one entry each.
#define GC_SLAB_MAP0_BITS 11 // bits 21..31
#define GC_SLAB_MAP1_BITS 0
#define GC_SLAB_MAP2_BITS 0
#endif

typedef struct {
    jl_gc_slabmeta_t *meta[1 << GC_SLAB_MAP0_BITS];
} gc_slab_map0_t;

typedef struct {
    gc_slab_map0_t *map0[1 << GC_SLAB_MAP1_BITS];
} gc_slab_map1_t;

static gc_slab_map1_t *gc_slab_map[1 << GC_SLAB_MAP2_BITS];

// Requires `gc_big_lock` when `create` is set. Lookups without `create` may
// run without the lock: interior nodes are never freed, and leaf slots are
// only read for addresses this allocator handed out (whose entries were
// published before the allocation was visible to anyone).
static jl_gc_slabmeta_t **gc_slab_map_slot(void *p, int create) JL_NOTSAFEPOINT
{
    // 64-bit even on 32-bit targets: the shift counts below exceed the width of
    // a 32-bit pointer, which would be undefined behaviour on `uintptr_t`.
    uint64_t a = (uint64_t)(uintptr_t)p;
    unsigned i2 = (unsigned)((a >> (GC_SLAB_LG2 + GC_SLAB_MAP0_BITS + GC_SLAB_MAP1_BITS)) & ((1 << GC_SLAB_MAP2_BITS) - 1));
    gc_slab_map1_t *m1 = gc_slab_map[i2];
    if (m1 == NULL) {
        if (!create)
            return NULL;
        m1 = (gc_slab_map1_t*)calloc_s(sizeof(gc_slab_map1_t));
        gc_slab_map[i2] = m1;
    }
    unsigned i1 = (unsigned)((a >> (GC_SLAB_LG2 + GC_SLAB_MAP0_BITS)) & ((1 << GC_SLAB_MAP1_BITS) - 1));
    gc_slab_map0_t *m0 = m1->map0[i1];
    if (m0 == NULL) {
        if (!create)
            return NULL;
        m0 = (gc_slab_map0_t*)calloc_s(sizeof(gc_slab_map0_t));
        m1->map0[i1] = m0;
    }
    return &m0->meta[(size_t)((a >> GC_SLAB_LG2) & ((1 << GC_SLAB_MAP0_BITS) - 1))];
}

STATIC_INLINE jl_gc_slabmeta_t *gc_slab_map_lookup(void *p) JL_NOTSAFEPOINT
{
    jl_gc_slabmeta_t **slot = gc_slab_map_slot(p, 0);
    return slot == NULL ? NULL : *slot;
}

// ======================================================================== //
// OS memory
// ======================================================================== //

static int gc_use_hugepages = 1;

#ifndef MAP_NORESERVE // not defined in POSIX, FreeBSD, etc.
#define MAP_NORESERVE (0)
#endif

// Reserve a GC_SLAB_SZ-aligned region of `sz` bytes (a multiple of
// GC_SLAB_SZ). On POSIX the memory is immediately usable; on Windows it is
// only reserved and each slab must be committed before use.
// Updates bytes_mapped; the caller is responsible for bytes_resident.
// `raw_base_out`, if non-NULL, receives the base of the underlying OS mapping
// (needed to release it on Windows; equal to the returned pointer on POSIX).
static char *gc_reserve_aligned(size_t sz, void **raw_base_out) JL_NOTSAFEPOINT
{
#ifdef _OS_WINDOWS_
    // over-reserve to align; the unused head/tail cannot be released
    // separately on Windows, so it is simply never committed
    char *mem = (char*)VirtualAlloc(NULL, sz + GC_SLAB_SZ, MEM_RESERVE, PAGE_READWRITE);
    if (mem == NULL)
        return NULL;
    char *base = (char*)LLT_ALIGN((uintptr_t)mem, GC_SLAB_SZ);
    if (raw_base_out != NULL)
        *raw_base_out = mem;
#else
    char *mem = (char*)mmap(0, sz + GC_SLAB_SZ, PROT_READ | PROT_WRITE,
                            MAP_NORESERVE | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mem == MAP_FAILED)
        return NULL;
    char *base = (char*)LLT_ALIGN((uintptr_t)mem, GC_SLAB_SZ);
    // trim the unaligned head and tail
    if (base != mem)
        munmap(mem, base - mem);
    munmap(base + sz, mem + GC_SLAB_SZ - base);
    if (raw_base_out != NULL)
        *raw_base_out = base;
#ifdef MADV_HUGEPAGE
    if (gc_use_hugepages)
        madvise(base, sz, MADV_HUGEPAGE);
#endif
#endif
    // On Windows the head/tail of the over-reservation cannot be released
    // separately, so the whole reservation stays charged to bytes_mapped;
    // on POSIX it has been trimmed away and only `sz` remains mapped.
#ifdef _OS_WINDOWS_
    jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_mapped, sz + GC_SLAB_SZ);
#else
    jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_mapped, sz);
#endif
    return base;
}

// Commit a decommitted (or never-committed) slab. Returns 0 on failure.
static int gc_slab_commit(char *data) JL_NOTSAFEPOINT
{
#ifdef _OS_WINDOWS_
    if (VirtualAlloc(data, GC_SLAB_SZ, MEM_COMMIT, PAGE_READWRITE) == NULL)
        return 0;
#endif
    // POSIX: MADV_FREE/DONTNEED memory recommits transparently on touch
    jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_resident, GC_SLAB_SZ);
    return 1;
}

static void gc_slab_decommit(char *data) JL_NOTSAFEPOINT
{
#ifdef _OS_WINDOWS_
    VirtualFree(data, GC_SLAB_SZ, MEM_DECOMMIT);
#elif defined(MADV_FREE)
    static _Atomic(int) supports_madv_free = 1;
    if (jl_atomic_load_relaxed(&supports_madv_free)) {
        if (madvise(data, GC_SLAB_SZ, MADV_FREE) == -1) {
            assert(errno == EINVAL);
            jl_atomic_store_relaxed(&supports_madv_free, 0);
        }
    }
    if (!jl_atomic_load_relaxed(&supports_madv_free))
        madvise(data, GC_SLAB_SZ, MADV_DONTNEED);
#else
    madvise(data, GC_SLAB_SZ, MADV_DONTNEED);
#endif
    msan_unpoison(data, GC_SLAB_SZ);
    jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_resident, -(int64_t)GC_SLAB_SZ);
}

// ======================================================================== //
// slab management (all under gc_big_lock)
// ======================================================================== //

#define GC_SLAB_BLOCK_NSLABS 32 // 64 MB blocks

// Slabs with some (not all) units free, bucketed by occupancy so that carving
// prefers the fullest slab and the emptiest ones drain to wholly free (and so
// become decommittable). Without this, a slab lands at the head of the list
// exactly when something in it is freed, and would be the first refilled.
static jl_gc_slabmeta_t *gc_partial_slabs[GC_OCCUPANCY_BUCKETS];
static jl_gc_slabmeta_t *gc_free_slabs; // fully free slabs

// Requires `gc_big_lock`. The slab must have some, but not all, units free.
static void gc_partial_slab_link(jl_gc_slabmeta_t *s) JL_NOTSAFEPOINT
{
    assert(s->partial_bucket < 0 && "slab is already on a partial list");
    assert(s->free_unit_map != 0 && s->free_unit_map != UINT32_MAX);
    int b = gc_occupancy_bucket((size_t)__builtin_popcount(s->free_unit_map) << GC_UNIT_LG2,
                                GC_SLAB_SZ);
    s->partial_bucket = (int8_t)b;
    s->prev = NULL;
    s->next = gc_partial_slabs[b];
    if (s->next != NULL)
        s->next->prev = s;
    gc_partial_slabs[b] = s;
}

// Requires `gc_big_lock`. No-op if the slab is not on a partial list. The
// bucket is remembered rather than recomputed, so this stays correct however
// `free_unit_map` has changed since the slab was linked.
static void gc_partial_slab_unlink(jl_gc_slabmeta_t *s) JL_NOTSAFEPOINT
{
    int b = s->partial_bucket;
    if (b < 0)
        return;
    if (s->prev != NULL)
        s->prev->next = s->next;
    else
        gc_partial_slabs[b] = s->next;
    if (s->next != NULL)
        s->next->prev = s->prev;
    s->next = s->prev = NULL;
    s->partial_bucket = -1;
}

// Requires `gc_big_lock`. The slab must be fully free and not on a
// partial-slab list.
static void gc_release_slab(jl_gc_slabmeta_t *s) JL_NOTSAFEPOINT
{
    assert(s->free_unit_map == UINT32_MAX);
    assert(s->partial_bucket < 0);
    s->next = gc_free_slabs;
    gc_free_slabs = s;
    s->free_sweeps = 0;
}

// Map a new block of slabs, register them, and return one of them; the rest
// go onto the free-slab list. Returns NULL if the OS is out of memory.
static jl_gc_slabmeta_t *gc_alloc_slab_block(void) JL_NOTSAFEPOINT
{
    int nslabs = GC_SLAB_BLOCK_NSLABS;
    char *base;
    void *raw_base;
    while (1) {
        base = gc_reserve_aligned((size_t)nslabs << GC_SLAB_LG2, &raw_base);
        if (base != NULL)
            break;
        if (nslabs == 1)
            return NULL;
        nslabs = nslabs / 4 > 0 ? nslabs / 4 : 1;
    }
#ifndef _OS_WINDOWS_
    jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_resident, (size_t)nslabs << GC_SLAB_LG2);
#endif
    jl_gc_slabmeta_t *metas = (jl_gc_slabmeta_t*)calloc_s(nslabs * sizeof(jl_gc_slabmeta_t));
    for (int i = 0; i < nslabs; i++) {
        jl_gc_slabmeta_t *s = &metas[i];
        s->data = base + ((size_t)i << GC_SLAB_LG2);
        s->free_unit_map = UINT32_MAX;
        s->partial_bucket = -1; // wholly free: on the free-slab list, not a partial one
#ifdef _OS_WINDOWS_
        s->resident = 0;
#else
        s->resident = 1;
#endif
        *gc_slab_map_slot(s->data, 1) = s;
        if (i != 0) {
            s->next = gc_free_slabs;
            gc_free_slabs = s;
        }
    }
    // Remember the base of the OS reservation on the block's first slab. Slab
    // blocks are never unmapped (only decommitted slab by slab), but on Windows
    // this is the only pointer VirtualFree would accept, so dropping it would
    // make the reservation permanently unreleasable.
    metas[0].map_base = raw_base;
    metas[0].map_sz = (size_t)nslabs << GC_SLAB_LG2;
    return &metas[0];
}

// Take a fully-free slab. Returns NULL if the OS is out of memory.
static jl_gc_slabmeta_t *gc_take_slab(void) JL_NOTSAFEPOINT
{
    jl_gc_slabmeta_t *s = gc_free_slabs;
    if (s != NULL) {
        gc_free_slabs = s->next;
        s->next = NULL;
        s->free_sweeps = 0;
    }
    else {
        s = gc_alloc_slab_block();
        if (s == NULL)
            return NULL;
    }
    if (!s->resident) {
        if (!gc_slab_commit(s->data)) {
            // undo and report OOM
            s->next = gc_free_slabs;
            gc_free_slabs = s;
            return NULL;
        }
        s->resident = 1;
    }
    return s;
}

// A page is always 1 unit (64 KiB), 8 units (512 KiB), or a whole slab.
// `free_unit_map` is the only record of which units of a slab are free: runs
// are found by masking it, and freed runs coalesce implicitly when their bits
// go back. A free run carries no size class, so any class of the matching page
// size can reuse it: only the three page sizes, not the size classes, can
// fragment.
#define GC_UNITS_PER_MEDIUM 8

// free_unit_map bits for the run of `nunits` units starting at unit `u`
STATIC_INLINE uint32_t gc_unit_run_mask(int u, int nunits) JL_NOTSAFEPOINT
{
    return (nunits >= GC_UNITS_PER_SLAB ? UINT32_MAX : ((uint32_t)1 << nunits) - 1) << u;
}

// mask of the units belonging to the 8-unit group containing unit `u`
STATIC_INLINE uint32_t gc_medium_group_mask(int u) JL_NOTSAFEPOINT
{
    return (uint32_t)0xff << (u & ~(GC_UNITS_PER_MEDIUM - 1));
}

// Units of `free` that lie in an entirely free 8-unit group. Handing one of
// these out to a 64 KiB page destroys a 512 KiB page's worth of contiguity.
STATIC_INLINE uint32_t gc_units_in_full_groups(uint32_t free) JL_NOTSAFEPOINT
{
    uint32_t full = 0;
    for (int u = 0; u < GC_UNITS_PER_SLAB; u += GC_UNITS_PER_MEDIUM) {
        uint32_t m = gc_medium_group_mask(u);
        if ((free & m) == m)
            full |= m;
    }
    return full;
}

// Requires `gc_big_lock`. First unit of an aligned free run of `nunits` units
// in some partial slab, or -1 if no partial slab has one. Under
// `avoid_breakup`, a single unit is taken only from outside an entirely free
// 8-unit group; the caller retries without it, so a group is broken up only
// once no loose unit is left in *any* slab -- keeping 512 KiB pages
// allocatable for as long as possible.
static int gc_find_in_partials(int nunits, int avoid_breakup,
                               jl_gc_slabmeta_t **s_out) JL_NOTSAFEPOINT
{
    // Fullest bucket first. Note this is the inner preference: not breaking up
    // a free 8-unit group loses a whole page size and outranks draining a slab,
    // so the caller sweeps every bucket with `avoid_breakup` before retrying.
    for (int b = 0; b < GC_OCCUPANCY_BUCKETS; b++) {
        for (jl_gc_slabmeta_t *s = gc_partial_slabs[b]; s != NULL; s = s->next) {
            uint32_t free = s->free_unit_map;
            int u;
            if (nunits == 1) {
                if (avoid_breakup)
                    free &= ~gc_units_in_full_groups(free);
                if (free == 0)
                    continue;
                u = __builtin_ctz(free);
            }
            else {
                assert(nunits == GC_UNITS_PER_MEDIUM);
                // a whole free slab would be on the free-slab list, not here, so
                // there is no larger run for this to break up
                uint32_t groups = gc_units_in_full_groups(free);
                if (groups == 0)
                    continue;
                u = __builtin_ctz(groups);
            }
            *s_out = s;
            return u;
        }
    }
    return -1;
}

// Requires `gc_big_lock`. Claim an aligned run of `nunits` (1, 8, or 32) units.
// Returns NULL if the OS is out of memory.
static jl_gc_slabmeta_t *gc_carve_units(int nunits, int *unit_out) JL_NOTSAFEPOINT
{
    jl_gc_slabmeta_t *s = NULL;
    int u = -1;
    // Reuse space in a slab already in use before committing a fresh one. A
    // whole-slab page can never fit in a partial slab, so don't bother looking.
    if (nunits != GC_UNITS_PER_SLAB) {
        u = gc_find_in_partials(nunits, 1, &s);
        if (u < 0 && nunits == 1)
            u = gc_find_in_partials(nunits, 0, &s);
    }
    if (u < 0) {
        s = gc_take_slab(); // arrives on no list
        if (s == NULL)
            return NULL;
        u = 0;
    }
    assert(u % nunits == 0 && "carved run is misaligned for its size");
    uint32_t run = gc_unit_run_mask(u, nunits);
    assert((s->free_unit_map & run) == run);
    // relink rather than mutate in place: the slab's occupancy, and hence its
    // bucket, changes here
    gc_partial_slab_unlink(s);
    s->free_unit_map &= ~run;
    if (s->free_unit_map != 0)
        gc_partial_slab_link(s);
    *unit_out = u;
    return s;
}

// Requires `gc_big_lock`. Return an aligned run of units to its slab, where it
// coalesces with any adjacent free run; a fully-free slab moves to the
// free-slab list, from where it can be decommitted.
static void gc_free_units(jl_gc_slabmeta_t *s, int u, int nunits) JL_NOTSAFEPOINT
{
    assert(u % nunits == 0 && "freed run is misaligned for its size");
    uint32_t run = gc_unit_run_mask(u, nunits);
    assert((s->free_unit_map & run) == 0);
    gc_partial_slab_unlink(s); // no-op for a slab with no free units left
    s->free_unit_map |= run;
    if (s->free_unit_map == UINT32_MAX)
        gc_release_slab(s);
    else
        gc_partial_slab_link(s);
}

// ======================================================================== //
// big pages
// ======================================================================== //

// Take a fresh page for `szclass` from the slab layer and make it the
// caller's current page. Returns NULL if the OS is out of memory.
static NOINLINE jl_gc_bigpagemeta_t *gc_big_fresh_page(int szclass) JL_NOTSAFEPOINT
{
    size_t osize = gc_big_class_sz[szclass];
    int page_lg2 = gc_big_page_lg2(osize);
    int nunits = 1 << (page_lg2 - GC_UNIT_LG2);
    uv_mutex_lock(&gc_big_lock);
    int u;
    jl_gc_slabmeta_t *s = gc_carve_units(nunits, &u);
    if (s == NULL) {
        uv_mutex_unlock(&gc_big_lock);
        return NULL;
    }
    if (s->pages == NULL)
        s->pages = (jl_gc_bigpagemeta_t*)calloc_s(GC_UNITS_PER_SLAB * sizeof(jl_gc_bigpagemeta_t));
    for (int j = 0; j < nunits; j++)
        s->unit_page_start[u + j] = (uint8_t)u;
    uv_mutex_unlock(&gc_big_lock);

    jl_gc_bigpagemeta_t *pg = &s->pages[u];
    pg->data = s->data + ((size_t)u << GC_UNIT_LG2);
    pg->page_lg2 = (uint8_t)page_lg2;
    pg->szclass = szclass;
    pg->osize = (uint32_t)osize;
    pg->nobjs = (uint32_t)(((size_t)1 << page_lg2) / osize);
    pg->nfree = pg->nobjs;
    pg->freelist = NULL;
    pg->bump = pg->data;
    pg->sweep_next = NULL;
    pg->on_sweep_list = 0;
    return pg;
}

// ======================================================================== //
// huge allocations
// ======================================================================== //

static void *gc_huge_alloc(size_t allocsz, size_t *usable_sz)
{
    size_t sz = LLT_ALIGN(allocsz, jl_page_size);
    size_t map_sz = LLT_ALIGN(sz, GC_SLAB_SZ);
    // reserve at slab granularity so the base address owns its slab-map slots
    void *raw_base;
    char *base = gc_reserve_aligned(map_sz, &raw_base);
    if (base == NULL)
        return NULL;
    jl_gc_slabmeta_t *s = (jl_gc_slabmeta_t*)calloc_s(sizeof(jl_gc_slabmeta_t));
    s->is_huge = 1;
    s->partial_bucket = -1; // huge slabs are never carved into units
    s->data = base;
    s->map_base = raw_base;
    s->map_sz = map_sz;
    s->usable_sz = sz;
    s->resident = 1;
#ifdef _OS_WINDOWS_
    if (VirtualAlloc(base, sz, MEM_COMMIT, PAGE_READWRITE) == NULL) {
        VirtualFree(raw_base, 0, MEM_RELEASE);
        jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_mapped,
                                    -(int64_t)(map_sz + GC_SLAB_SZ));
        free(s);
        return NULL;
    }
#endif
    jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_resident, sz);
    uv_mutex_lock(&gc_big_lock);
    *gc_slab_map_slot(base, 1) = s;
    uv_mutex_unlock(&gc_big_lock);
    *usable_sz = sz;
    return base;
}

static size_t gc_huge_free(jl_gc_slabmeta_t *s) JL_NOTSAFEPOINT
{
    size_t usable = s->usable_sz;
    // Take the lock: the slab map is also read without it (gc_slab_map_lookup)
    // and grown under it (gc_slab_map_slot(.., 1)) by other threads, including
    // the concurrent page sweeper.
    uv_mutex_lock(&gc_big_lock);
    jl_gc_slabmeta_t **slot = gc_slab_map_slot(s->data, 0);
    assert(slot != NULL && *slot == s && "huge allocation missing from the slab map");
    if (slot != NULL)
        *slot = NULL;
    uv_mutex_unlock(&gc_big_lock);
#ifdef _OS_WINDOWS_
    // releases the whole over-reservation, head and tail included
    VirtualFree(s->map_base, 0, MEM_RELEASE);
    jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_mapped,
                                -(int64_t)(s->map_sz + GC_SLAB_SZ));
#else
    munmap(s->data, s->map_sz);
    jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_mapped, -(int64_t)s->map_sz);
#endif
    jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_resident, -(int64_t)usable);
    free(s);
    return usable;
}

void *jl_gc_big_mem_alloc(jl_ptls_t ptls, size_t allocsz, size_t *usable_sz)
{
    if (allocsz > GC_BIG_CLASS_MAX_SZ)
        return gc_huge_alloc(allocsz, usable_sz);
    int c = gc_big_szclass(allocsz);
    size_t osize = gc_big_class_sz[c];
    *usable_sz = osize;
    jl_gc_bigpagemeta_t *pg = ptls->gc_tls.big_pages[c];
    while (1) {
        if (pg != NULL) {
            void *p = pg->freelist;
            if (p != NULL) {
                pg->freelist = *(void**)p;
                pg->nfree--;
                return p;
            }
            if (pg->bump + osize <= pg->data + (size_t)pg->nobjs * osize) {
                p = pg->bump;
                pg->bump += osize;
                pg->nfree--;
                return p;
            }
            assert(pg->nfree == 0);
            pg->state = GC_BIG_PAGE_FULL; // retire; sweep picks it back up
        }
        // take the fullest available partial page so emptier pages drain
        pg = NULL;
        for (int b = 0; b < GC_OCCUPANCY_BUCKETS && pg == NULL; b++)
            pg = gc_big_stack_pop(&gc_big_partial_pages[c][b]);
        if (pg == NULL) {
            pg = gc_big_fresh_page(c);
            if (pg == NULL) {
                ptls->gc_tls.big_pages[c] = NULL;
                return NULL;
            }
        }
        pg->state = GC_BIG_PAGE_CURRENT;
        ptls->gc_tls.big_pages[c] = pg;
    }
}

// ======================================================================== //
// sweep integration
// ======================================================================== //

// pages that received at least one free slot during the current sweep
static jl_gc_bigpagemeta_t *gc_sweep_touched_pages;

// pages given up by exiting threads while they still had free slots, linked
// through `next`. They are on no per-class stack, so nothing but the next
// sweep will ever hand their remaining slots out again. Requires `gc_big_lock`.
static jl_gc_bigpagemeta_t *gc_orphaned_pages;

size_t jl_gc_big_mem_free(void *p) JL_NOTSAFEPOINT
{
    jl_gc_slabmeta_t *s = gc_slab_map_lookup(p);
    assert(s != NULL && "freeing a pointer not allocated by jl_gc_big_mem_alloc");
    if (s->is_huge) {
        assert(p == s->data);
        return gc_huge_free(s);
    }
    int u = (int)(((uintptr_t)p >> GC_UNIT_LG2) & (GC_UNITS_PER_SLAB - 1));
    jl_gc_bigpagemeta_t *pg = &s->pages[s->unit_page_start[u]];
    assert(pg->state != GC_BIG_PAGE_FREE);
    *(void**)p = pg->freelist;
    pg->freelist = p;
    pg->nfree++;
    if (!pg->on_sweep_list) {
        pg->on_sweep_list = 1;
        pg->sweep_next = gc_sweep_touched_pages;
        gc_sweep_touched_pages = pg;
    }
    return pg->osize;
}

// Return a fully-free page's units to its slab. Requires `gc_big_lock`.
static void gc_big_release_page(jl_gc_bigpagemeta_t *pg) JL_NOTSAFEPOINT
{
    jl_gc_slabmeta_t *s = gc_slab_map_lookup(pg->data);
    int u = (int)((pg->data - s->data) >> GC_UNIT_LG2);
    assert(&s->pages[u] == pg);
    pg->state = GC_BIG_PAGE_FREE;
    pg->freelist = NULL;
    gc_free_units(s, u, 1 << (pg->page_lg2 - GC_UNIT_LG2));
}

// Requires `gc_big_lock`. Not a current page and not on the sweep list.
static void gc_big_dispose_page(jl_gc_bigpagemeta_t *pg) JL_NOTSAFEPOINT
{
    assert(pg->state != GC_BIG_PAGE_CURRENT);
    if (pg->nfree == pg->nobjs) {
        gc_big_release_page(pg);
    }
    else if (pg->nfree > 0) {
        pg->state = GC_BIG_PAGE_PARTIAL;
        gc_big_stack_push(&gc_big_partial_pages[pg->szclass][gc_big_page_bucket(pg)], pg);
    }
    else {
        pg->state = GC_BIG_PAGE_FULL;
    }
}

// Give up the current allocation pages of a thread that is going away. Sweep
// only ever re-sorts pages that are not GC_BIG_PAGE_CURRENT, so pages left
// pointing at a dead thread would keep their units forever.
void jl_gc_big_mem_thread_exit(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&gc_big_lock);
    for (int c = 0; c < GC_N_BIG_CLASSES; c++) {
        jl_gc_bigpagemeta_t *pg = ptls->gc_tls.big_pages[c];
        if (pg == NULL)
            continue;
        ptls->gc_tls.big_pages[c] = NULL;
        assert(pg->state == GC_BIG_PAGE_CURRENT);
        if (pg->nfree == pg->nobjs && !pg->on_sweep_list) {
            // nothing live in it: hand the units straight back
            gc_big_release_page(pg);
        }
        else {
            // Can't push to a per-class stack outside stop-the-world; queue it
            // for the next sweep to re-sort.
            pg->state = GC_BIG_PAGE_FULL;
            if (!pg->on_sweep_list) {
                pg->next = gc_orphaned_pages;
                gc_orphaned_pages = pg;
            }
        }
    }
    uv_mutex_unlock(&gc_big_lock);
}

void jl_gc_big_mem_finish_sweep(void) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&gc_big_lock);
    // The per-class partial stacks and the sweep-touched list may overlap, and
    // pages in the stacks may have become fully free (which requires removing
    // them, impossible in-place). Drain everything and re-sort.
    for (int c = 0; c < GC_N_BIG_CLASSES; c++) {
        jl_gc_bigpagemeta_t *pg;
        jl_gc_bigpagemeta_t *untouched = NULL;
        for (int b = 0; b < GC_OCCUPANCY_BUCKETS; b++) {
            while ((pg = gc_big_stack_pop(&gc_big_partial_pages[c][b])) != NULL) {
                if (pg->on_sweep_list)
                    continue; // will be re-sorted from the sweep-touched list
                pg->next = untouched;
                untouched = pg;
            }
        }
        while (untouched != NULL) {
            pg = untouched;
            untouched = pg->next;
            gc_big_dispose_page(pg);
        }
    }
    // Pages orphaned by exiting threads, before the sweep-touched list for the
    // same reason the per-class stacks are drained first: a page can be on both,
    // and disposing it twice would push it onto two bucket stacks.
    jl_gc_bigpagemeta_t *orphan = gc_orphaned_pages;
    gc_orphaned_pages = NULL;
    while (orphan != NULL) {
        jl_gc_bigpagemeta_t *nxt = orphan->next;
        orphan->next = NULL;
        if (!orphan->on_sweep_list)
            gc_big_dispose_page(orphan); // else re-sorted from the list below
        orphan = nxt;
    }
    jl_gc_bigpagemeta_t *pg = gc_sweep_touched_pages;
    gc_sweep_touched_pages = NULL;
    while (pg != NULL) {
        jl_gc_bigpagemeta_t *nxt = pg->sweep_next;
        pg->sweep_next = NULL;
        pg->on_sweep_list = 0;
        if (pg->state != GC_BIG_PAGE_CURRENT)
            gc_big_dispose_page(pg);
        pg = nxt;
    }
    // Return cold slabs to the OS. Every sweep end is a heap low point, so a
    // slab that was already free at the end of the previous sweep and is still
    // free now was not needed for a whole GC cycle; decommit it.
    for (jl_gc_slabmeta_t *s = gc_free_slabs; s != NULL; s = s->next) {
        if (!s->resident)
            continue;
        if (s->free_sweeps == 0) {
            s->free_sweeps = 1;
        }
        else {
            gc_slab_decommit(s->data);
            s->resident = 0;
        }
    }
    uv_mutex_unlock(&gc_big_lock);
}

// ======================================================================== //
// pool pages: one slab unit each. Freed units return to their slab's bitmap;
// memory is returned to the OS only in whole slabs (see
// jl_gc_big_mem_finish_sweep), which also keeps transparent huge pages intact.
// ======================================================================== //

JL_DLLEXPORT uint64_t jl_get_pg_size(void)
{
    return GC_PAGE_SZ;
}

// get a new page, either from a slab with free units
// or from the kernel if none are available
NOINLINE jl_gc_pagemeta_t *jl_gc_alloc_page(void) JL_NOTSAFEPOINT
{
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    // try to get a page that was lazily freed during the last sweep
    jl_gc_pagemeta_t *meta = pop_lf_back(&global_page_pool_lazily_freed);
    if (meta != NULL) {
        gc_alloc_map_set(meta->data, GC_PAGE_ALLOCATED);
        goto exit;
    }

    uv_mutex_lock(&gc_big_lock);
    int u;
    jl_gc_slabmeta_t *s = gc_carve_units(1, &u);
    if (s == NULL) {
        uv_mutex_unlock(&gc_big_lock);
#ifdef _OS_WINDOWS_
        SetLastError(last_error);
#endif
        errno = last_errno;
        jl_throw(jl_memory_exception);
    }
    if (s->pool_pages == NULL)
        s->pool_pages = (jl_gc_pagemeta_t*)calloc_s(GC_UNITS_PER_SLAB * sizeof(jl_gc_pagemeta_t));
    meta = &s->pool_pages[u];
    if (meta->data == NULL) {
        meta->data = s->data + ((size_t)u << GC_UNIT_LG2);
        gc_alloc_map_maybe_create(meta->data);
    }
    uv_mutex_unlock(&gc_big_lock);
    gc_alloc_map_set(meta->data, GC_PAGE_ALLOCATED);
exit:
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;
    return meta;
}

// return a page's unit to its slab; whole-slab decommit happens at the end of sweep
NOINLINE void jl_gc_free_page(jl_gc_pagemeta_t *pg) JL_NOTSAFEPOINT
{
    gc_alloc_map_set(pg->data, GC_PAGE_FREED);
    msan_unpoison(pg->data, GC_PAGE_SZ);
    uv_mutex_lock(&gc_big_lock);
    jl_gc_slabmeta_t *s = gc_slab_map_lookup(pg->data);
    assert(s != NULL && !s->is_huge);
    int u = (int)(((uintptr_t)pg->data >> GC_UNIT_LG2) & (GC_UNITS_PER_SLAB - 1));
    gc_free_units(s, u, 1);
    uv_mutex_unlock(&gc_big_lock);
}

void jl_gc_init_big_pages(void)
{
    uv_mutex_init(&gc_big_lock);
    gc_big_class_sz[0] = GC_BIG_CLASS_MIN_SZ;
    for (int c = 1; c < GC_N_BIG_CLASSES; c++) {
        int q = (c - 1) / 4, r = (c - 1) % 4;
        gc_big_class_sz[c] = ((size_t)GC_BIG_CLASS_MIN_SZ << q) +
            (size_t)(r + 1) * ((size_t)(GC_BIG_CLASS_MIN_SZ / 4) << q);
    }
    assert(gc_big_class_sz[GC_N_BIG_CLASSES - 1] == GC_BIG_CLASS_MAX_SZ);
    char *env = getenv("JULIA_GC_HUGEPAGES");
    if (env != NULL && env[0] == '0')
        gc_use_hugepages = 0;
}

#ifdef __cplusplus
}
#endif
