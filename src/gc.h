// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  allocation and garbage collection
  . non-moving, precise mark and sweep collector
  . pool-allocates small objects, keeps big objects on a simple list
*/

#ifndef JULIA_GC_H
#define JULIA_GC_H

#include <stdlib.h>
#include <string.h>
#ifndef _MSC_VER
#include <strings.h>
#endif
#include <assert.h>
#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
#include "threading.h"
#ifndef _OS_WINDOWS_
#include <sys/mman.h>
#if defined(_OS_DARWIN_) && !defined(MAP_ANONYMOUS)
#define MAP_ANONYMOUS MAP_ANON
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

// manipulating mark bits

#define GC_CLEAN 0 // freshly allocated
#define GC_MARKED 1 // reachable and old
#define GC_QUEUED 2 // if it is reachable it will be marked as old
#define GC_MARKED_NOESC (GC_MARKED | GC_QUEUED) // reachable and young

#define GC_PAGE_LG2 14 // log2(size of a page)
#define GC_PAGE_SZ (1 << GC_PAGE_LG2) // 16k
#define GC_PAGE_OFFSET (JL_SMALL_BYTE_ALIGNMENT - (sizeof_jl_taggedvalue_t % JL_SMALL_BYTE_ALIGNMENT))

// 8G * 32768 = 2^48
// It's really unlikely that we'll actually allocate that much though...
#define REGION_COUNT 32768

#define jl_buff_tag ((uintptr_t)0x4eade800)
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
    int sweep_mask;
    int wait_for_debugger;
    jl_alloc_num_t pool;
    jl_alloc_num_t other;
    jl_alloc_num_t print;
} jl_gc_debug_env_t;

// This struct must be kept in sync with the Julia type of the same name in base/util.jl
typedef struct {
    int64_t     allocd;
    int64_t     freed;
    uint64_t    malloc;
    uint64_t    realloc;
    uint64_t    poolalloc;
    uint64_t    bigalloc;
    uint64_t    freecall;
    uint64_t    total_time;
    uint64_t    total_allocd;
    uint64_t    since_sweep;
    size_t      interval;
    int         pause;
    int         full_sweep;
} jl_gc_num_t;

// layout for small (<2k) objects

typedef struct _buff_t {
    union {
        uintptr_t header;
        struct _buff_t *next;
        uintptr_t flags;
        jl_value_t *type; // 16-bytes aligned
        struct {
            uintptr_t gc_bits:2;
            uintptr_t pooled:1;
        };
    };
    char data[];
} buff_t;
typedef buff_t gcval_t;

// layout for big (>2k) objects

typedef struct _bigval_t {
    struct _bigval_t *next;
    struct _bigval_t **prev; // pointer to the next field of the prev entry
    union {
        size_t sz;
        uintptr_t age : 2;
    };
    #ifdef _P64 // Add padding so that char data[] below is 64-byte aligned
        // (8 pointers of 8 bytes each) - (4 other pointers in struct)
        void *_padding[8 - 4];
    #else
        // (16 pointers of 4 bytes each) - (4 other pointers in struct)
        void *_padding[16 - 4];
    #endif
    //struct buff_t <>;
    union {
        uintptr_t header;
        uintptr_t flags;
        uintptr_t gc_bits:2;
    };
    // must be 64-byte aligned here, in 32 & 64 bit modes
    char data[];
} bigval_t;

// data structure for tracking malloc'd arrays.

typedef struct _mallocarray_t {
    jl_array_t *a;
    struct _mallocarray_t *next;
} mallocarray_t;

// pool page metadata
typedef struct {
    struct {
        // index of pool that owns this page
        uint16_t pool_n : 8;
        // Whether any cell in the page is marked
        // This bit is set before sweeping iff there's live cells in the page.
        // Note that before marking or after sweeping there can be live
        // (and young) cells in the page for `!has_marked`.
        uint16_t has_marked: 1;
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
        uint16_t has_young: 1;
    };
    // number of old objects in this page
    uint16_t nold;
    // number of old objects in this page during the previous full sweep
    uint16_t prev_nold;
    // number of free objects in this page.
    // invalid if pool that owns this page is allocating objects from this page.
    uint16_t nfree;
    uint16_t osize; // size of each object in this page
    uint16_t fl_begin_offset; // offset of first free object in this page
    uint16_t fl_end_offset;   // offset of last free object in this page
    uint16_t thread_n;        // index (into jl_thread_heap) of heap that owns this page
    char *data;
    uint8_t *ages;
} jl_gc_pagemeta_t;

typedef struct {
    char data[GC_PAGE_SZ];
} jl_gc_page_t
#if !defined(_COMPILER_MICROSOFT_) && !(defined(_COMPILER_MINGW_) && defined(_COMPILER_CLANG_))
__attribute__((aligned(GC_PAGE_SZ)))
#endif
;

typedef struct {
    // Page layout:
    //  Padding: GC_PAGE_OFFSET
    //  Blocks: osize * n
    //    Tag: sizeof_jl_taggedvalue_t
    //    Data: <= osize - sizeof_jl_taggedvalue_t
    jl_gc_page_t *pages; // [pg_cnt]; must be first, to preserve page alignment
    uint32_t *allocmap; // [pg_cnt / 32]
    jl_gc_pagemeta_t *meta; // [pg_cnt]
    int pg_cnt;
    // store a lower bound of the first free page in each region
    int lb;
    // an upper bound of the last non-free page
    int ub;
} region_t
;

extern jl_gc_num_t gc_num;
extern region_t regions[REGION_COUNT];
extern bigval_t *big_objects_marked;
extern arraylist_t finalizer_list;
extern arraylist_t finalizer_list_marked;
extern arraylist_t to_finalize;

// Counters
// GC_FINAL_STATS only
extern size_t max_pg_count;

#define bigval_header(data) container_of((data), bigval_t, header)

// round an address inside a gcpage's data to its beginning
STATIC_INLINE char *gc_page_data(void *x)
{
    return (char*)(((uintptr_t)x >> GC_PAGE_LG2) << GC_PAGE_LG2);
}

STATIC_INLINE gcval_t *page_pfl_beg(jl_gc_pagemeta_t *p)
{
    return (gcval_t*)(p->data + p->fl_begin_offset);
}

STATIC_INLINE gcval_t *page_pfl_end(jl_gc_pagemeta_t *p)
{
    return (gcval_t*)(p->data + p->fl_end_offset);
}

STATIC_INLINE int page_index(region_t *region, void *data)
{
    return (gc_page_data(data) - region->pages->data) / GC_PAGE_SZ;
}

#define gc_bits(o) (((gcval_t*)(o))->gc_bits)
#define gc_marked(o)  (((gcval_t*)(o))->gc_bits & GC_MARKED)
#define _gc_setmark(o, mark_mode) (((gcval_t*)(o))->gc_bits = mark_mode)

NOINLINE uintptr_t gc_get_stack_ptr(void);

STATIC_INLINE region_t *find_region(void *ptr, int maybe)
{
    // on 64bit systems we could probably use a single region and remove this loop
    for (int i = 0; i < REGION_COUNT && regions[i].pages; i++) {
        region_t *region = &regions[i];
        char *begin = region->pages->data;
        char *end = begin + region->pg_cnt * sizeof(jl_gc_page_t);
        if ((char*)ptr >= begin && (char*)ptr <= end) {
            return region;
        }
    }
    (void)maybe;
    assert(maybe && "find_region failed");
    return NULL;
}

STATIC_INLINE jl_gc_pagemeta_t *page_metadata(void *data)
{
    region_t *r = find_region(data, 0);
    int pg_idx = page_index(r, (char*)data - GC_PAGE_OFFSET);
    return &r->meta[pg_idx];
}

void pre_mark(void);
void post_mark(arraylist_t *list, int dryrun);
void gc_debug_init(void);

#define jl_thread_heap (jl_get_ptls_states()->heap)

// GC pages

void jl_gc_init_page(void);
NOINLINE void *jl_gc_alloc_page(void);
void jl_gc_free_page(void *p);

// GC debug

#ifdef GC_VERIFY
extern jl_value_t *lostval;
void gc_verify(void);
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
        if (*(jl_value_t**)(slot) == lostval &&                         \
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
#define gc_verify()
#define verify_val(v)
#define verify_parent1(ty,obj,slot,arg1)
#define verify_parent2(ty,obj,slot,arg1,arg2)
#define gc_verifying (0)
#endif

#ifdef GC_DEBUG_ENV
JL_DLLEXPORT extern jl_gc_debug_env_t jl_gc_debug_env;
#define gc_quick_sweep_mask jl_gc_debug_env.sweep_mask
int gc_debug_check_other(void);
int gc_debug_check_pool(void);
void gc_debug_print(void);
void gc_scrub(char *stack_hi);
#else
#define gc_quick_sweep_mask GC_MARKED_NOESC
static inline int gc_debug_check_other(void)
{
    return 0;
}
static inline int gc_debug_check_pool(void)
{
    return 0;
}
static inline void gc_debug_print(void)
{
}
static inline void gc_scrub(char *stack_hi)
{
    (void)stack_hi;
}
#endif

#ifdef OBJPROFILE
void objprofile_count(void *ty, int old, int sz);
void objprofile_printall(void);
void objprofile_reset(void);
#else
static inline void objprofile_count(void *ty, int old, int sz)
{
}

static inline void objprofile_printall(void)
{
}

static inline void objprofile_reset(void)
{
}
#endif

// For debugging
void gc_count_pool(void);

#ifdef __cplusplus
}
#endif

#endif
