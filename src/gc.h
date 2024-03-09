// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  allocation and garbage collection
  . non-moving, precise mark and sweep collector
  . pool-allocates small objects, keeps big objects on a simple list
*/

#ifndef JL_GC_H
#define JL_GC_H

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <inttypes.h>
#include "julia.h"
#include "julia_threads.h"
#include "julia_internal.h"
#include "threading.h"
#ifndef _OS_WINDOWS_
#include <sys/mman.h>
#if defined(_OS_DARWIN_) && !defined(MAP_ANONYMOUS)
#define MAP_ANONYMOUS MAP_ANON
#endif
#endif
#include "julia_assert.h"
#include "gc-heap-snapshot.h"
#include "gc-alloc-profiler.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef GC_SMALL_PAGE
#define GC_PAGE_LG2 12 // log2(size of a page)
#else
#define GC_PAGE_LG2 14 // log2(size of a page)
#endif
#define GC_PAGE_SZ (1 << GC_PAGE_LG2)
#define GC_PAGE_OFFSET (JL_HEAP_ALIGNMENT - (sizeof(jl_taggedvalue_t) % JL_HEAP_ALIGNMENT))

#define jl_malloc_tag ((void*)0xdeadaa01)
#define jl_singleton_tag ((void*)0xdeadaa02)

// Used by GC_DEBUG_ENV
typedef struct {
    uint64_t num;
    uint64_t next;
    uint64_t min;
    uint64_t interv;
    uint64_t max;
    unsigned short random[3];
} jl_alloc_num_t;

typedef struct {
    int always_full;
    int wait_for_debugger;
    jl_alloc_num_t pool;
    jl_alloc_num_t other;
    jl_alloc_num_t print;
} jl_gc_debug_env_t;

// This struct must be kept in sync with the Julia type of the same name in base/timing.jl
typedef struct {
    int64_t     allocd;
    int64_t     deferred_alloc;
    int64_t     freed;
    uint64_t    malloc;
    uint64_t    realloc;
    uint64_t    poolalloc;
    uint64_t    bigalloc;
    uint64_t    freecall;
    uint64_t    total_time;
    uint64_t    total_allocd;
    size_t      interval;
    int         pause;
    int         full_sweep;
    uint64_t    max_pause;
    uint64_t    max_memory;
    uint64_t    time_to_safepoint;
    uint64_t    max_time_to_safepoint;
    uint64_t    total_time_to_safepoint;
    uint64_t    sweep_time;
    uint64_t    mark_time;
    uint64_t    total_sweep_time;
    uint64_t    total_mark_time;
    uint64_t    last_full_sweep;
    uint64_t    last_incremental_sweep;
} jl_gc_num_t;

// Array chunks (work items representing suffixes of
// large arrays of pointers left to be marked)

typedef enum {
    GC_empty_chunk = 0, // for sentinel representing no items left in chunk queue
    GC_objary_chunk,    // for chunk of object array
    GC_ary8_chunk,      // for chunk of array with 8 bit field descriptors
    GC_ary16_chunk,     // for chunk of array with 16 bit field descriptors
    GC_finlist_chunk,   // for chunk of finalizer list
} gc_chunk_id_t;

typedef struct _jl_gc_chunk_t {
    gc_chunk_id_t cid;
    struct _jl_value_t *parent; // array owner
    struct _jl_value_t **begin; // pointer to first element that needs scanning
    struct _jl_value_t **end;   // pointer to last element that needs scanning
    void *elem_begin;           // used to scan pointers within objects when marking `ary8` or `ary16`
    void *elem_end;             // used to scan pointers within objects when marking `ary8` or `ary16`
    uint32_t step;              // step-size used when marking objarray
    uintptr_t nptr;             // (`nptr` & 0x1) if array has young element and (`nptr` & 0x2) if array owner is old
} jl_gc_chunk_t;

#define GC_CHUNK_BATCH_SIZE (1 << 16)       // maximum number of references that can be processed
                                            // without creating a chunk

#define GC_PTR_QUEUE_INIT_SIZE (1 << 18)    // initial size of queue of `jl_value_t *`
#define GC_CHUNK_QUEUE_INIT_SIZE (1 << 14)  // initial size of chunk-queue

#define GC_REMSET_PTR_TAG (0x1)             // lowest bit of `jl_value_t *` is tagged if it's in the remset

// layout for big (>2k) objects

JL_EXTENSION typedef struct _bigval_t {
    struct _bigval_t *next;
    struct _bigval_t **prev; // pointer to the next field of the prev entry
    size_t sz;
#ifdef _P64 // Add padding so that the value is 64-byte aligned
    // (8 pointers of 8 bytes each) - (4 other pointers in struct)
    void *_padding[8 - 4];
#else
    // (16 pointers of 4 bytes each) - (4 other pointers in struct)
    void *_padding[16 - 4];
#endif
    //struct jl_taggedvalue_t <>;
    union {
        uintptr_t header;
        struct {
            uintptr_t gc:2;
        } bits;
    };
    // must be 64-byte aligned here, in 32 & 64 bit modes
} bigval_t;

// data structure for tracking malloc'd arrays and genericmemory.

typedef struct _mallocarray_t {
    jl_value_t *a;
    struct _mallocarray_t *next;
} mallocarray_t;

// pool page metadata
typedef struct _jl_gc_pagemeta_t {
    // next metadata structure in per-thread list
    // or in one of the `jl_gc_global_page_pool_t`
    struct _jl_gc_pagemeta_t *next;
    // index of pool that owns this page
    uint8_t pool_n;
    // Whether any cell in the page is marked
    // This bit is set before sweeping iff there are live cells in the page.
    // Note that before marking or after sweeping there can be live
    // (and young) cells in the page for `!has_marked`.
    uint8_t has_marked;
    // Whether any cell was live and young **before sweeping**.
    // For a normal sweep (quick sweep that is NOT preceded by a
    // full sweep) this bit is set iff there are young or newly dead
    // objects in the page and the page needs to be swept.
    //
    // For a full sweep, this bit should be ignored.
    //
    // For a quick sweep preceded by a full sweep. If this bit is set,
    // the page needs to be swept. If this bit is not set, there could
    // still be old dead objects in the page and `nold` and `prev_nold`
    // should be used to determine if the page needs to be swept.
    uint8_t has_young;
    // number of old objects in this page
    uint16_t nold;
    // number of old objects in this page during the previous full sweep
    uint16_t prev_nold;
    // number of free objects in this page.
    // invalid if pool that owns this page is allocating objects from this page.
    uint16_t nfree;
    uint16_t osize;           // size of each object in this page
    uint16_t fl_begin_offset; // offset of first free object in this page
    uint16_t fl_end_offset;   // offset of last free object in this page
    uint16_t thread_n;        // thread id of the heap that owns this page
    char *data;
} jl_gc_pagemeta_t;

extern jl_gc_page_stack_t global_page_pool_lazily_freed;
extern jl_gc_page_stack_t global_page_pool_clean;
extern jl_gc_page_stack_t global_page_pool_freed;

// Lock-free stack implementation taken
// from Herlihy's "The Art of Multiprocessor Programming"
// XXX: this is not a general-purpose lock-free stack. We can
// get away with just using a CAS and not implementing some ABA
// prevention mechanism since once a node is popped from the
// `jl_gc_page_stack_t`, it may only be pushed back to them
// in the sweeping phase, which also doesn't push a node into the
// same stack after it's popped

STATIC_INLINE void push_lf_back_nosync(jl_gc_page_stack_t *pool, jl_gc_pagemeta_t *elt) JL_NOTSAFEPOINT
{
    jl_gc_pagemeta_t *old_back = jl_atomic_load_relaxed(&pool->bottom);
    elt->next = old_back;
    jl_atomic_store_relaxed(&pool->bottom, elt);
}

STATIC_INLINE jl_gc_pagemeta_t *pop_lf_back_nosync(jl_gc_page_stack_t *pool) JL_NOTSAFEPOINT
{
    jl_gc_pagemeta_t *old_back = jl_atomic_load_relaxed(&pool->bottom);
    if (old_back == NULL) {
        return NULL;
    }
    jl_atomic_store_relaxed(&pool->bottom, old_back->next);
    return old_back;
}

STATIC_INLINE void push_lf_back(jl_gc_page_stack_t *pool, jl_gc_pagemeta_t *elt) JL_NOTSAFEPOINT
{
    while (1) {
        jl_gc_pagemeta_t *old_back = jl_atomic_load_relaxed(&pool->bottom);
        elt->next = old_back;
        if (jl_atomic_cmpswap(&pool->bottom, &old_back, elt)) {
            break;
        }
        jl_cpu_pause();
    }
}

#define MAX_POP_ATTEMPTS (1 << 10)

STATIC_INLINE jl_gc_pagemeta_t *try_pop_lf_back(jl_gc_page_stack_t *pool) JL_NOTSAFEPOINT
{
    for (int i = 0; i < MAX_POP_ATTEMPTS; i++) {
        jl_gc_pagemeta_t *old_back = jl_atomic_load_relaxed(&pool->bottom);
        if (old_back == NULL) {
            return NULL;
        }
        if (jl_atomic_cmpswap(&pool->bottom, &old_back, old_back->next)) {
            return old_back;
        }
        jl_cpu_pause();
    }
    return NULL;
}

STATIC_INLINE jl_gc_pagemeta_t *pop_lf_back(jl_gc_page_stack_t *pool) JL_NOTSAFEPOINT
{
    while (1) {
        jl_gc_pagemeta_t *old_back = jl_atomic_load_relaxed(&pool->bottom);
        if (old_back == NULL) {
            return NULL;
        }
        if (jl_atomic_cmpswap(&pool->bottom, &old_back, old_back->next)) {
            return old_back;
        }
        jl_cpu_pause();
    }
}
typedef struct {
    jl_gc_page_stack_t stack;
    // pad to 128 bytes to avoid false-sharing
#ifdef _P64
    void *_pad[15];
#else
    void *_pad[31];
#endif
} jl_gc_padded_page_stack_t;
static_assert(sizeof(jl_gc_padded_page_stack_t) == 128, "jl_gc_padded_page_stack_t is not 128 bytes");

typedef struct {
    _Atomic(size_t) n_freed_objs;
    _Atomic(size_t) n_pages_allocd;
} gc_fragmentation_stat_t;

#ifdef GC_SMALL_PAGE
#ifdef _P64
#define REGION0_PG_COUNT (1 << 16)
#define REGION1_PG_COUNT (1 << 18)
#define REGION2_PG_COUNT (1 << 18)
#define REGION0_INDEX(p) (((uintptr_t)(p) >> 12) & 0xFFFF) // shift by GC_PAGE_LG2
#define REGION1_INDEX(p) (((uintptr_t)(p) >> 28) & 0x3FFFF)
#define REGION_INDEX(p)  (((uintptr_t)(p) >> 46) & 0x3FFFF)
#else
#define REGION0_PG_COUNT (1 << 10)
#define REGION1_PG_COUNT (1 << 10)
#define REGION2_PG_COUNT (1 << 0)
#define REGION0_INDEX(p) (((uintptr_t)(p) >> 12) & 0x3FF) // shift by GC_PAGE_LG2
#define REGION1_INDEX(p) (((uintptr_t)(p) >> 22) & 0x3FF)
#define REGION_INDEX(p)  (0)
#endif
#else
#ifdef _P64
#define REGION0_PG_COUNT (1 << 16)
#define REGION1_PG_COUNT (1 << 16)
#define REGION2_PG_COUNT (1 << 18)
#define REGION0_INDEX(p) (((uintptr_t)(p) >> 14) & 0xFFFF) // shift by GC_PAGE_LG2
#define REGION1_INDEX(p) (((uintptr_t)(p) >> 30) & 0xFFFF)
#define REGION_INDEX(p)  (((uintptr_t)(p) >> 46) & 0x3FFFF)
#else
#define REGION0_PG_COUNT (1 << 8)
#define REGION1_PG_COUNT (1 << 10)
#define REGION2_PG_COUNT (1 << 0)
#define REGION0_INDEX(p) (((uintptr_t)(p) >> 14) & 0xFF) // shift by GC_PAGE_LG2
#define REGION1_INDEX(p) (((uintptr_t)(p) >> 22) & 0x3FF)
#define REGION_INDEX(p)  (0)
#endif
#endif

// define the representation of the levels of the page-table (0 to 2)
typedef struct {
    uint8_t meta[REGION0_PG_COUNT];
} pagetable0_t;

typedef struct {
    pagetable0_t *meta0[REGION1_PG_COUNT];
} pagetable1_t;

typedef struct {
    pagetable1_t *meta1[REGION2_PG_COUNT];
} pagetable_t;

typedef struct {
    _Atomic(size_t) bytes_mapped;
    _Atomic(size_t) bytes_resident;
    _Atomic(size_t) heap_size;
    _Atomic(size_t) heap_target;
} gc_heapstatus_t;

#define GC_PAGE_UNMAPPED        0
#define GC_PAGE_ALLOCATED       1
#define GC_PAGE_LAZILY_FREED    2
#define GC_PAGE_FREED           3

extern pagetable_t alloc_map;

STATIC_INLINE uint8_t gc_alloc_map_is_set(char *_data) JL_NOTSAFEPOINT
{
    uintptr_t data = ((uintptr_t)_data);
    unsigned i;
    i = REGION_INDEX(data);
    pagetable1_t *r1 = alloc_map.meta1[i];
    if (r1 == NULL)
        return 0;
    i = REGION1_INDEX(data);
    pagetable0_t *r0 = r1->meta0[i];
    if (r0 == NULL)
        return 0;
    i = REGION0_INDEX(data);
    return (r0->meta[i] == GC_PAGE_ALLOCATED);
}

STATIC_INLINE void gc_alloc_map_set(char *_data, uint8_t v) JL_NOTSAFEPOINT
{
    uintptr_t data = ((uintptr_t)_data);
    unsigned i;
    i = REGION_INDEX(data);
    pagetable1_t *r1 = alloc_map.meta1[i];
    assert(r1 != NULL);
    i = REGION1_INDEX(data);
    pagetable0_t *r0 = r1->meta0[i];
    assert(r0 != NULL);
    i = REGION0_INDEX(data);
    r0->meta[i] = v;
}

STATIC_INLINE void gc_alloc_map_maybe_create(char *_data) JL_NOTSAFEPOINT
{
    uintptr_t data = ((uintptr_t)_data);
    unsigned i;
    i = REGION_INDEX(data);
    pagetable1_t *r1 = alloc_map.meta1[i];
    if (r1 == NULL) {
        r1 = (pagetable1_t*)calloc_s(sizeof(pagetable1_t));
        alloc_map.meta1[i] = r1;
    }
    i = REGION1_INDEX(data);
    pagetable0_t *r0 = r1->meta0[i];
    if (r0 == NULL) {
        r0 = (pagetable0_t*)calloc_s(sizeof(pagetable0_t));
        r1->meta0[i] = r0;
    }
}

// Page layout:
//  Metadata pointer: sizeof(jl_gc_pagemeta_t*)
//  Padding: GC_PAGE_OFFSET - sizeof(jl_gc_pagemeta_t*)
//  Blocks: osize * n
//    Tag: sizeof(jl_taggedvalue_t)
//    Data: <= osize - sizeof(jl_taggedvalue_t)

STATIC_INLINE char *gc_page_data(void *x) JL_NOTSAFEPOINT
{
    return (char*)(((uintptr_t)x >> GC_PAGE_LG2) << GC_PAGE_LG2);
}

STATIC_INLINE jl_gc_pagemeta_t *page_metadata_unsafe(void *_data) JL_NOTSAFEPOINT
{
    return *(jl_gc_pagemeta_t**)(gc_page_data(_data));
}

STATIC_INLINE jl_gc_pagemeta_t *page_metadata(void *_data) JL_NOTSAFEPOINT
{
    if (!gc_alloc_map_is_set((char*)_data)) {
        return NULL;
    }
    return page_metadata_unsafe(_data);
}

STATIC_INLINE void set_page_metadata(jl_gc_pagemeta_t *pg) JL_NOTSAFEPOINT
{
    *(jl_gc_pagemeta_t**)(pg->data) = pg;
}

STATIC_INLINE void push_page_metadata_back(jl_gc_pagemeta_t **ppg, jl_gc_pagemeta_t *elt) JL_NOTSAFEPOINT
{
    elt->next = *ppg;
    *ppg = elt;
}

STATIC_INLINE jl_gc_pagemeta_t *pop_page_metadata_back(jl_gc_pagemeta_t **ppg) JL_NOTSAFEPOINT
{
    jl_gc_pagemeta_t *v = *ppg;
    if (*ppg != NULL) {
        *ppg = (*ppg)->next;
    }
    return v;
}

#ifdef __clang_gcanalyzer__ /* clang may not have __builtin_ffs */
unsigned ffs_u32(uint32_t bitvec) JL_NOTSAFEPOINT;
#else
STATIC_INLINE unsigned ffs_u32(uint32_t bitvec)
{
    return __builtin_ffs(bitvec) - 1;
}
#endif

extern jl_gc_num_t gc_num;
extern bigval_t *big_objects_marked;
extern arraylist_t finalizer_list_marked;
extern arraylist_t to_finalize;
extern int64_t buffered_pages;
extern int gc_first_tid;
extern int gc_n_threads;
extern jl_ptls_t* gc_all_tls_states;
extern gc_heapstatus_t gc_heap_stats;

STATIC_INLINE bigval_t *bigval_header(jl_taggedvalue_t *o) JL_NOTSAFEPOINT
{
    return container_of(o, bigval_t, header);
}

STATIC_INLINE jl_taggedvalue_t *page_pfl_beg(jl_gc_pagemeta_t *p) JL_NOTSAFEPOINT
{
    return (jl_taggedvalue_t*)(p->data + p->fl_begin_offset);
}

STATIC_INLINE jl_taggedvalue_t *page_pfl_end(jl_gc_pagemeta_t *p) JL_NOTSAFEPOINT
{
    return (jl_taggedvalue_t*)(p->data + p->fl_end_offset);
}

STATIC_INLINE int gc_marked(uintptr_t bits) JL_NOTSAFEPOINT
{
    return (bits & GC_MARKED) != 0;
}

STATIC_INLINE int gc_old(uintptr_t bits) JL_NOTSAFEPOINT
{
    return (bits & GC_OLD) != 0;
}

STATIC_INLINE uintptr_t gc_set_bits(uintptr_t tag, int bits) JL_NOTSAFEPOINT
{
    return (tag & ~(uintptr_t)3) | bits;
}

STATIC_INLINE uintptr_t gc_ptr_tag(void *v, uintptr_t mask) JL_NOTSAFEPOINT
{
    return ((uintptr_t)v) & mask;
}

STATIC_INLINE void *gc_ptr_clear_tag(void *v, uintptr_t mask) JL_NOTSAFEPOINT
{
    return (void*)(((uintptr_t)v) & ~mask);
}

NOINLINE uintptr_t gc_get_stack_ptr(void);

STATIC_INLINE void gc_big_object_unlink(const bigval_t *hdr) JL_NOTSAFEPOINT
{
    *hdr->prev = hdr->next;
    if (hdr->next) {
        hdr->next->prev = hdr->prev;
    }
}

STATIC_INLINE void gc_big_object_link(bigval_t *hdr, bigval_t **list) JL_NOTSAFEPOINT
{
    hdr->next = *list;
    hdr->prev = list;
    if (*list)
        (*list)->prev = &hdr->next;
    *list = hdr;
}

extern uv_mutex_t gc_threads_lock;
extern uv_cond_t gc_threads_cond;
extern uv_sem_t gc_sweep_assists_needed;
extern _Atomic(int) gc_n_threads_marking;
extern _Atomic(int) gc_n_threads_sweeping;
void gc_mark_queue_all_roots(jl_ptls_t ptls, jl_gc_markqueue_t *mq);
void gc_mark_finlist_(jl_gc_markqueue_t *mq, jl_value_t *fl_parent, jl_value_t **fl_begin, jl_value_t **fl_end) JL_NOTSAFEPOINT;
void gc_mark_finlist(jl_gc_markqueue_t *mq, arraylist_t *list, size_t start) JL_NOTSAFEPOINT;
void gc_mark_loop_serial_(jl_ptls_t ptls, jl_gc_markqueue_t *mq);
void gc_mark_loop_serial(jl_ptls_t ptls);
void gc_mark_loop_parallel(jl_ptls_t ptls, int master);
void gc_sweep_pool_parallel(jl_ptls_t ptls);
void gc_free_pages(void);
void sweep_stack_pools(void);
void jl_gc_debug_init(void);

// GC pages

void jl_gc_init_page(void) JL_NOTSAFEPOINT;
NOINLINE jl_gc_pagemeta_t *jl_gc_alloc_page(void) JL_NOTSAFEPOINT;
void jl_gc_free_page(jl_gc_pagemeta_t *p) JL_NOTSAFEPOINT;

// GC debug

#if defined(GC_TIME) || defined(GC_FINAL_STATS)
void gc_settime_premark_end(void);
void gc_settime_postmark_end(void);
#else
#define gc_settime_premark_end()
#define gc_settime_postmark_end()
#endif

#ifdef GC_FINAL_STATS
void gc_final_count_page(size_t pg_cnt);
void gc_final_pause_end(int64_t t0, int64_t tend);
#else
#define gc_final_count_page(pg_cnt)
#define gc_final_pause_end(t0, tend)
#endif

#ifdef GC_TIME
void gc_time_pool_start(void) JL_NOTSAFEPOINT;
void gc_time_count_page(int freedall, int pg_skpd) JL_NOTSAFEPOINT;
void gc_time_pool_end(int sweep_full) JL_NOTSAFEPOINT;
void gc_time_sysimg_end(uint64_t t0) JL_NOTSAFEPOINT;

void gc_time_big_start(void) JL_NOTSAFEPOINT;
void gc_time_count_big(int old_bits, int bits) JL_NOTSAFEPOINT;
void gc_time_big_end(void) JL_NOTSAFEPOINT;

void gc_time_mallocd_memory_start(void) JL_NOTSAFEPOINT;
void gc_time_count_mallocd_memory(int bits) JL_NOTSAFEPOINT;
void gc_time_mallocd_memory_end(void) JL_NOTSAFEPOINT;

void gc_time_mark_pause(int64_t t0, int64_t scanned_bytes,
                        int64_t perm_scanned_bytes);
void gc_time_sweep_pause(uint64_t gc_end_t, int64_t actual_allocd,
                         int64_t live_bytes, int64_t estimate_freed,
                         int sweep_full);
void gc_time_summary(int sweep_full, uint64_t start, uint64_t end,
                     uint64_t freed, uint64_t live, uint64_t interval,
                     uint64_t pause, uint64_t ttsp, uint64_t mark,
                     uint64_t sweep);
void gc_heuristics_summary(
        uint64_t old_alloc_diff, uint64_t alloc_mem,
        uint64_t old_mut_time, uint64_t alloc_time,
        uint64_t old_freed_diff, uint64_t gc_mem,
        uint64_t old_pause_time, uint64_t gc_time,
        int thrash_counter, const char *reason,
        uint64_t current_heap, uint64_t target_heap);
#else
#define gc_time_pool_start()
STATIC_INLINE void gc_time_count_page(int freedall, int pg_skpd) JL_NOTSAFEPOINT
{
    (void)freedall;
    (void)pg_skpd;
}
#define gc_time_pool_end(sweep_full) (void)(sweep_full)
#define gc_time_sysimg_end(t0) (void)(t0)
#define gc_time_big_start()
STATIC_INLINE void gc_time_count_big(int old_bits, int bits) JL_NOTSAFEPOINT
{
    (void)old_bits;
    (void)bits;
}
#define gc_time_big_end()
#define gc_time_mallocd_memory_start()
STATIC_INLINE void gc_time_count_mallocd_memory(int bits) JL_NOTSAFEPOINT
{
    (void)bits;
}
#define gc_time_mallocd_memory_end()
#define gc_time_mark_pause(t0, scanned_bytes, perm_scanned_bytes)
#define gc_time_sweep_pause(gc_end_t, actual_allocd, live_bytes,        \
                            estimate_freed, sweep_full)
#define  gc_time_summary(sweep_full, start, end, freed, live,           \
                         interval, pause, ttsp, mark, sweep)
#define gc_heuristics_summary( \
        old_alloc_diff, alloc_mem, \
        old_mut_time, alloc_time, \
        old_freed_diff, gc_mem, \
        old_pause_time, gc_time, \
        thrash_counter, reason, \
        current_heap, target_heap)
#endif

#ifdef MEMFENCE
void gc_verify_tags(void);
#else
static inline void gc_verify_tags(void)
{
}
#endif

#ifdef GC_VERIFY
extern jl_value_t *lostval;
void gc_verify(jl_ptls_t ptls);
void add_lostval_parent(jl_value_t *parent);
#define verify_val(v) do {                                              \
        if (lostval == (jl_value_t*)(v) && (v) != 0) {                  \
            jl_printf(JL_STDOUT,                                        \
                      "Found lostval %p at %s:%d oftype: ",             \
                      (void*)(lostval), __FILE__, __LINE__);            \
            jl_static_show(JL_STDOUT, jl_typeof(v));                    \
            jl_printf(JL_STDOUT, "\n");                                 \
        }                                                               \
    } while(0);

#define verify_parent(ty, obj, slot, args...) do {                      \
        if (gc_ptr_clear_tag(*(void**)(slot), 3) == (void*)lostval &&   \
            (jl_value_t*)(obj) != lostval) {                            \
            jl_printf(JL_STDOUT, "Found parent %p %p at %s:%d\n",       \
                      (void*)(ty), (void*)(obj), __FILE__, __LINE__);   \
            jl_printf(JL_STDOUT, "\tloc %p : ", (void*)(slot));         \
            jl_printf(JL_STDOUT, args);                                 \
            jl_printf(JL_STDOUT, "\n");                                 \
            jl_printf(JL_STDOUT, "\ttype: ");                           \
            jl_static_show(JL_STDOUT, jl_typeof(obj));                  \
            jl_printf(JL_STDOUT, "\n");                                 \
            add_lostval_parent((jl_value_t*)(obj));                     \
        }                                                               \
    } while(0);

#define verify_parent1(ty,obj,slot,arg1) verify_parent(ty,obj,slot,arg1)
#define verify_parent2(ty,obj,slot,arg1,arg2) verify_parent(ty,obj,slot,arg1,arg2)
extern int gc_verifying;
#else
#define gc_verify(ptls)
#define verify_val(v)
#define verify_parent1(ty,obj,slot,arg1) do {} while (0)
#define verify_parent2(ty,obj,slot,arg1,arg2) do {} while (0)
#define gc_verifying (0)
#endif

int gc_slot_to_fieldidx(void *_obj, void *slot, jl_datatype_t *vt) JL_NOTSAFEPOINT;
int gc_slot_to_arrayidx(void *_obj, void *begin) JL_NOTSAFEPOINT;
NOINLINE void gc_mark_loop_unwind(jl_ptls_t ptls, jl_gc_markqueue_t *mq, int offset) JL_NOTSAFEPOINT;

#ifdef GC_DEBUG_ENV
JL_DLLEXPORT extern jl_gc_debug_env_t jl_gc_debug_env;
#define gc_sweep_always_full jl_gc_debug_env.always_full
int jl_gc_debug_check_other(void);
int gc_debug_check_pool(void);
void jl_gc_debug_print(void);
void gc_scrub_record_task(jl_task_t *ta) JL_NOTSAFEPOINT;
void gc_scrub(void);
#else
#define gc_sweep_always_full 0
static inline int jl_gc_debug_check_other(void)
{
    return 0;
}
static inline int gc_debug_check_pool(void)
{
    return 0;
}
static inline void jl_gc_debug_print(void)
{
}
static inline void gc_scrub_record_task(jl_task_t *ta) JL_NOTSAFEPOINT
{
    (void)ta;
}
static inline void gc_scrub(void)
{
}
#endif

#ifdef OBJPROFILE
void objprofile_count(void *ty, int old, int sz) JL_NOTSAFEPOINT;
void objprofile_printall(void);
void objprofile_reset(void);
#else
static inline void objprofile_count(void *ty, int old, int sz) JL_NOTSAFEPOINT
{
}

static inline void objprofile_printall(void)
{
}

static inline void objprofile_reset(void)
{
}
#endif

#ifdef MEMPROFILE
void gc_stats_all_pool(void);
void gc_stats_big_obj(void);
#else
#define gc_stats_all_pool()
#define gc_stats_big_obj()
#endif

// For debugging
void gc_count_pool(void);

size_t jl_genericmemory_nbytes(jl_genericmemory_t *a) JL_NOTSAFEPOINT;

JL_DLLEXPORT void jl_enable_gc_logging(int enable);
JL_DLLEXPORT int jl_is_gc_logging_enabled(void);
JL_DLLEXPORT uint32_t jl_get_num_stack_mappings(void);
void _report_gc_finished(uint64_t pause, uint64_t freed, int full, int recollect, int64_t live_bytes) JL_NOTSAFEPOINT;

#ifdef __cplusplus
}
#endif

#endif
