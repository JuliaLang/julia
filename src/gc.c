// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  allocation and garbage collection
  . non-moving, precise mark and sweep collector
  . pool-allocates small objects, keeps big objects on a simple list
*/
// use mmap instead of malloc to allocate pages. default = off.
//#define USE_MMAP

// free pages as soon as they are empty. if not defined, then we
// will wait for the next GC, to allow the space to be reused more
// efficiently. default = on.
#define FREE_PAGES_EAGER
#include <stdlib.h>
#include <string.h>
#ifndef _MSC_VER
#include <strings.h>
#endif
#include <assert.h>
#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
#ifndef _OS_WINDOWS_
#include <sys/mman.h>
#ifdef _OS_DARWIN_
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

#define jl_valueof(v) (&((jl_taggedvalue_t*)(v))->value)

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
    size_t      collect;
    int         pause;
    int         full_sweep;
} GC_Num;

static GC_Num gc_num = {0,0,0,0,0,0,0,0,0,0,0,0,0};

#define collect_interval gc_num.collect
#define n_pause         gc_num.pause
#define n_full_sweep    gc_num.full_sweep
#define allocd_bytes    gc_num.allocd
#define freed_bytes     gc_num.freed
#define total_gc_time   gc_num.total_time
#define total_allocd_bytes gc_num.total_allocd
#define allocd_bytes_since_sweep gc_num.since_sweep
static size_t last_long_collect_interval;

typedef struct _buff_t {
    union {
        uintptr_t header;
        struct _buff_t *next;
        uptrint_t flags;
        jl_value_t *type; // 16-bytes aligned
        struct {
            uintptr_t gc_bits:2;
            uintptr_t pooled:1;
        };
    };
    // Work around a bug affecting gcc up to (at least) version 4.4.7
    // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=36839
#if !defined(_COMPILER_MICROSOFT_)
    int _dummy[0];
#endif
    char data[];
} buff_t;
typedef buff_t gcval_t;


// layout for big (>2k) objects

typedef struct _bigval_t {
    struct _bigval_t *next;
    struct _bigval_t **prev; // pointer to the next field of the prev entry
    union {
        size_t sz;
        uptrint_t age : 2;
    };
    //struct buff_t <>;
    union {
        uptrint_t header;
        uptrint_t flags;
        uptrint_t gc_bits:2;
    };
    // Work around a bug affecting gcc up to (at least) version 4.4.7
    // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=36839
#if !defined(_COMPILER_MICROSOFT_)
    int _dummy[0];
#endif
    // must be 16-aligned here, in 32 & 64b
    char data[];
} bigval_t;

#define bigval_header(data) container_of((data), bigval_t, header)

// data structure for tracking malloc'd arrays.

typedef struct _mallocarray_t {
    jl_array_t *a;
    struct _mallocarray_t *next;
} mallocarray_t;

typedef struct _pool_t {
    gcval_t *freelist;   // root of list of free objects
    gcval_t *newpages;   // root of list of chunks of free objects
    uint16_t end_offset; // stored to avoid computing it at each allocation
    uint16_t osize;      // size of objects in this pool
    uint16_t nfree;      // number of free objects in page pointed into by free_list
} pool_t;

// layout for small (<2k) objects

#define GC_PAGE_LG2 14 // log2(size of a page)
#define GC_PAGE_SZ (1 << GC_PAGE_LG2) // 16k
#define GC_PAGE_OFFSET (16 - (sizeof_jl_taggedvalue_t % 16))

// pool page metadata
typedef struct _gcpage_t {
    struct {
        uint16_t pool_n : 8; // index (into norm_pool) of pool that owns this page
        uint16_t allocd : 1; // true if an allocation happened in this page since last sweep
        uint16_t gc_bits : 2; // this is a bitwise | of all gc_bits in this page
    };
    uint16_t nfree; // number of free objects in this page.
                    // invalid if pool that owns this page is allocating objects from this page.
    uint16_t osize; // size of each object in this page
    uint16_t fl_begin_offset; // offset of first free object in this page
    uint16_t fl_end_offset;   // offset of last free object in this page
    char *data;
    uint8_t *ages;
} gcpage_t;

#define PAGE_PFL_BEG(p) ((gcval_t**)((p->data) + (p)->fl_begin_offset))
#define PAGE_PFL_END(p) ((gcval_t**)((p->data) + (p)->fl_end_offset))
// round an address inside a gcpage's data to its beginning
#define GC_PAGE_DATA(x) ((char*)((uintptr_t)(x) >> GC_PAGE_LG2 << GC_PAGE_LG2))

// A region is contiguous storage for up to REGION_PG_COUNT naturally aligned GC_PAGE_SZ pages
// It uses a very naive allocator (see malloc_page & free_page)
#if defined(_P64) && !defined(_COMPILER_MICROSOFT_)
#define REGION_PG_COUNT 16*8*4096 // 8G because virtual memory is cheap
#else
#define REGION_PG_COUNT 8*4096 // 512M
#endif
#define REGION_COUNT 8

typedef struct {
    // Page layout:
    //  Padding: GC_PAGE_OFFSET
    //  Blocks: osize * n
    //    Tag: sizeof_jl_taggedvalue_t
    //    Data: <= osize - sizeof_jl_taggedvalue_t
    char pages[REGION_PG_COUNT][GC_PAGE_SZ]; // must be first, to preserve page alignment
    uint32_t freemap[REGION_PG_COUNT/32];
    gcpage_t meta[REGION_PG_COUNT];
} region_t
#ifndef _COMPILER_MICROSOFT_
__attribute__((aligned(GC_PAGE_SZ)))
#endif
;

static region_t *regions[REGION_COUNT] = {NULL};
// store a lower bound of the first free page in each region
static int regions_lb[REGION_COUNT] = {0};
// an upper bound of the last non-free page
static int regions_ub[REGION_COUNT] = {REGION_PG_COUNT/32-1};

// Variables that become fields of a thread-local struct in the thread-safe
// version.

#define HEAP_DECL static

// variable for tracking preserved values.
HEAP_DECL arraylist_t preserved_values;

// variable for tracking weak references
HEAP_DECL arraylist_t weak_refs;

// variables for tracking malloc'd arrays
HEAP_DECL mallocarray_t *mallocarrays;
HEAP_DECL mallocarray_t *mafreelist;

// variables for tracking big objects
HEAP_DECL bigval_t *big_objects;

// variables for tracking "remembered set"
HEAP_DECL arraylist_t rem_bindings;
HEAP_DECL arraylist_t _remset[2]; // contains jl_value_t*
// lower bound of the number of pointers inside remembered values
static int remset_nptr;
HEAP_DECL arraylist_t *remset;
HEAP_DECL arraylist_t *last_remset;

// variables for allocating objects from pools
#ifdef _P64
#define N_POOLS 41
#else
#define N_POOLS 43
#endif
HEAP_DECL pool_t norm_pools[N_POOLS];

// End of Variables that become fields of a thread-local struct in the thread-safe version.

// The following macros are used for accessing these variables.
// In the future multi-threaded version, they establish the desired thread context.
// In the single-threaded version, they are essentially noops, but nonetheless
// serve to check that the thread context macros are being used.
#define FOR_CURRENT_HEAP {void *current_heap=NULL;
#define END }
#define FOR_EACH_HEAP {void *current_heap=NULL;
/*}*/
#define HEAP(x) (*((void)current_heap,&(x)))
#define preserved_values HEAP(preserved_values)
#define weak_refs HEAP(weak_refs)
#define big_objects HEAP(big_objects)
#define mallocarrays HEAP(mallocarrays)
#define mafreelist HEAP(mafreelist)
#define remset HEAP(remset)
#define last_remset HEAP(last_remset)
#define rem_bindings HEAP(rem_bindings)
#define pools norm_pools

// List of marked big objects.  Not per-thread.  Accessed only by master thread.
static bigval_t *big_objects_marked = NULL;

// finalization
static arraylist_t finalizer_list;
static arraylist_t finalizer_list_marked;
static arraylist_t to_finalize;

static int check_timeout = 0;
#define should_timeout() 0

#define gc_bits(o) (((gcval_t*)(o))->gc_bits)
#define gc_marked(o)  (((gcval_t*)(o))->gc_bits & GC_MARKED)
#define _gc_setmark(o, mark_mode) (((gcval_t*)(o))->gc_bits = mark_mode)

static gcpage_t *page_metadata(void *data);
static void pre_mark(void);
static void post_mark(arraylist_t *list, int dryrun);
static region_t *find_region(void *ptr, int maybe);

#define PAGE_INDEX(region, data)              \
    ((GC_PAGE_DATA((data) - GC_PAGE_OFFSET) - \
      &(region)->pages[0][0])/GC_PAGE_SZ)

NOINLINE static uintptr_t gc_get_stack_ptr()
{
    void *dummy = NULL;
    // The mask is to suppress the compiler warning about returning
    // address of local variable
    return (uintptr_t)&dummy & ~(uintptr_t)15;
}

#include "gc-debug.c"

int jl_in_gc; // referenced from switchto task.c
static int jl_gc_finalizers_inhibited; // don't run finalizers during codegen #11956

// malloc wrappers, aligned allocation

#if defined(_P64) || defined(__APPLE__)
#define malloc_a16(sz) malloc(sz)
#define realloc_a16(p, sz, oldsz) realloc((p), (sz))
#define free_a16(p) free(p)

#elif defined(_OS_WINDOWS_) /* 32-bit OS is implicit here. */
#define malloc_a16(sz) _aligned_malloc((sz)?(sz):1, 16)
#define realloc_a16(p, sz, oldsz) _aligned_realloc((p), (sz)?(sz):1, 16)
#define free_a16(p) _aligned_free(p)

#else
static inline void *malloc_a16(size_t sz)
{
    void *ptr;
    if (posix_memalign(&ptr, 16, sz))
        return NULL;
    return ptr;
}
static inline void *realloc_a16(void *d, size_t sz, size_t oldsz)
{
    void *b = malloc_a16(sz);
    if (b != NULL) {
        memcpy(b, d, oldsz);
        free(d);
    }
    return b;
}
#define free_a16(p) free(p)
#endif

static void schedule_finalization(void *o, void *f)
{
    arraylist_push(&to_finalize, o);
    arraylist_push(&to_finalize, f);
}

static void run_finalizer(jl_value_t *o, jl_value_t *ff)
{
    jl_function_t *f = (jl_function_t*)ff;
    assert(jl_is_function(f));
    JL_TRY {
        jl_apply(f, (jl_value_t**)&o, 1);
    }
    JL_CATCH {
        jl_printf(JL_STDERR, "error in running finalizer: ");
        jl_static_show(JL_STDERR, jl_exception_in_transit);
        jl_printf(JL_STDERR, "\n");
    }
}

static int finalize_object(jl_value_t *o)
{
    int success = 0;
    jl_value_t *f = NULL;
    JL_GC_PUSH1(&f);
    for(int i = 0; i < finalizer_list.len; i+=2) {
        if (o == (jl_value_t*)finalizer_list.items[i]) {
            f = (jl_value_t*)finalizer_list.items[i+1];
            if (i < finalizer_list.len - 2) {
                finalizer_list.items[i] = finalizer_list.items[finalizer_list.len-2];
                finalizer_list.items[i+1] = finalizer_list.items[finalizer_list.len-1];
                i -= 2;
            }
            finalizer_list.len -= 2;
            run_finalizer(o, f);
            success = 1;
        }
    }
    JL_GC_POP();
    return success;
}

static void run_finalizers(void)
{
    void *o = NULL, *f = NULL;
    JL_GC_PUSH2(&o, &f);
    while (to_finalize.len > 0) {
        f = arraylist_pop(&to_finalize);
        o = arraylist_pop(&to_finalize);
        run_finalizer((jl_value_t*)o, (jl_value_t*)f);
    }
    JL_GC_POP();
}

void jl_gc_inhibit_finalizers(int state)
{
    if (jl_gc_finalizers_inhibited && !state && !jl_in_gc) {
        jl_in_gc = 1;
        run_finalizers();
        jl_in_gc = 0;
    }
    jl_gc_finalizers_inhibited = state;
}

static void schedule_all_finalizers(arraylist_t* flist)
{
    // Multi-thread version should steal the entire list while holding a lock.
    for(size_t i=0; i < flist->len; i+=2) {
        jl_value_t *f = (jl_value_t*)flist->items[i+1];
        if (f != HT_NOTFOUND && !jl_is_cpointer(f)) {
            schedule_finalization(flist->items[i], flist->items[i+1]);
        }
    }
    flist->len = 0;
}

void jl_gc_run_all_finalizers(void)
{
    schedule_all_finalizers(&finalizer_list);
    schedule_all_finalizers(&finalizer_list_marked);
    run_finalizers();
}

DLLEXPORT void jl_gc_add_finalizer(jl_value_t *v, jl_function_t *f)
{
    arraylist_push(&finalizer_list, (void*)v);
    arraylist_push(&finalizer_list, (void*)f);
}

void jl_finalize(jl_value_t *o)
{
    (void)finalize_object(o);
}

static region_t *find_region(void *ptr, int maybe)
{
    // on 64bit systems we could probably use a single region and remove this loop
    for (int i = 0; i < REGION_COUNT && regions[i]; i++) {
        char *begin = &regions[i]->pages[0][0];
        char *end = begin + sizeof(regions[i]->pages);
        if ((char*)ptr >= begin && (char*)ptr <= end)
            return regions[i];
    }
    (void)maybe;
    assert(maybe && "find_region failed");
    return NULL;
}

static gcpage_t *page_metadata(void *data)
{
    region_t *r = find_region(data, 0);
    int pg_idx = PAGE_INDEX(r, (char*)data);
    return &r->meta[pg_idx];
}

static uint8_t *page_age(gcpage_t *pg)
{
    return pg->ages;
}

#define GC_POOL_END_OFS(osize) ((((GC_PAGE_SZ - GC_PAGE_OFFSET)/(osize)) - 1)*(osize) + GC_PAGE_OFFSET)


// GC knobs and self-measurement variables
static int64_t last_gc_total_bytes = 0;

static int gc_inc_steps = 1;
#ifdef _P64
#define default_collect_interval (5600*1024*sizeof(void*))
static size_t max_collect_interval = 1250000000UL;
#else
#define default_collect_interval (3200*1024*sizeof(void*))
static size_t max_collect_interval =  500000000UL;
#endif

// global variables for GC stats

#define NS_TO_S(t) ((double)(t/1000)/(1000*1000))
#define NS2MS(t) ((double)(t/1000)/1000)
static int64_t live_bytes = 0;
static int64_t promoted_bytes = 0;
static size_t current_pg_count = 0;
static size_t max_pg_count = 0;

#ifdef OBJPROFILE
static htable_t obj_counts[3];
static htable_t obj_sizes[3];
#endif

DLLEXPORT size_t jl_gc_total_freed_bytes=0;
#ifdef GC_FINAL_STATS
static uint64_t max_pause = 0;
static uint64_t total_sweep_time = 0;
static uint64_t total_mark_time = 0;
static uint64_t total_fin_time = 0;
#endif
int sweeping = 0;

/*
 * The state transition looks like :
 *
 * ([(quick)sweep] means either a sweep or a quicksweep)
 *
 * <-[(quick)sweep]-
 *                 |
 *     ---> GC_QUEUED <--[(quick)sweep && age>promotion]--
 *     |     |     ^                                     |
 *     |   [mark]  |                                     |
 *  [sweep]  |  [write barrier]                          |
 *     |     v     |                                     |
 *     ----- GC_MARKED <--------                         |
 *              |              |                         |
 *              --[quicksweep]--                         |
 *                                                       |
 *  ========= above this line objects are old =========  |
 *                                                       |
 *  ----[new]------> GC_CLEAN ------[mark]--------> GC_MARKED_NOESC
 *                    |    ^                                   |
 *  <-[(quick)sweep]---    |                                   |
 *                         --[(quick)sweep && age<=promotion]---
 */

// A quick sweep is a sweep where sweep_mask == GC_MARKED_NOESC.
// It means we won't touch GC_MARKED objects (old gen).

// When a reachable object has survived more than PROMOTE_AGE+1 collections
// it is tagged with GC_QUEUED during sweep and will be promoted on next mark
// because at that point we can know easily if it references young objects.
// Marked old objects that reference young ones are kept in the remset.

// When a write barrier triggers, the offending marked object is both queued,
// so as not to trigger the barrier again, and put in the remset.


#define PROMOTE_AGE 1
// this cannot be increased as is without changing :
// - sweep_page which is specialized for 1bit age
// - the size of the age storage in region_t


static int64_t scanned_bytes; // young bytes scanned while marking
static int64_t perm_scanned_bytes; // old bytes scanned while marking
static int prev_sweep_mask = GC_MARKED;
static size_t scanned_bytes_goal;

#ifdef OBJPROFILE
static void *BUFFTY = (void*)0xdeadb00f;
#endif
static void *MATY = (void*)0xdeadaa01;
static size_t array_nbytes(jl_array_t*);
static inline void objprofile_count(void* ty, int old, int sz)
{
#ifdef OBJPROFILE
#ifdef GC_VERIFY
    if (verifying) return;
#endif
    if ((intptr_t)ty <= 0x10)
        ty = BUFFTY;
    void **bp = ptrhash_bp(&obj_counts[old], ty);
    if (*bp == HT_NOTFOUND)
        *bp = (void*)2;
    else
        (*((ptrint_t*)bp))++;
    bp = ptrhash_bp(&obj_sizes[old], ty);
    if (*bp == HT_NOTFOUND)
        *bp = (void*)(1 + sz);
    else
        *((ptrint_t*)bp) += sz;
#endif
}

//static inline void gc_setmark_other(jl_value_t *v, int mark_mode) // unused function
//{
//    jl_taggedvalue_t *o = jl_astaggedvalue(v);
//    _gc_setmark(o, mark_mode);
//    verify_val(o);
//}

#define inc_sat(v,s) v = (v) >= s ? s : (v)+1

static inline int gc_setmark_big(void *o, int mark_mode)
{
#ifdef GC_VERIFY
    if (verifying) {
        _gc_setmark(o, mark_mode);
        return 0;
    }
#endif
    bigval_t* hdr = bigval_header(o);
    int bits = gc_bits(o);
    if (bits == GC_QUEUED || bits == GC_MARKED)
        mark_mode = GC_MARKED;
    if ((mark_mode == GC_MARKED) & (bits != GC_MARKED)) {
        // Move hdr from big_objects list to big_objects_marked list
        *hdr->prev = hdr->next;
        if (hdr->next)
            hdr->next->prev = hdr->prev;
        hdr->next = big_objects_marked;
        hdr->prev = &big_objects_marked;
        if (big_objects_marked)
            big_objects_marked->prev = &hdr->next;
        big_objects_marked = hdr;
    }
    if (!(bits & GC_MARKED)) {
        if (mark_mode == GC_MARKED)
            perm_scanned_bytes += hdr->sz&~3;
        else
            scanned_bytes += hdr->sz&~3;
#ifdef OBJPROFILE
        objprofile_count(jl_typeof(o), mark_mode == GC_MARKED, hdr->sz&~3);
#endif
    }
    _gc_setmark(o, mark_mode);
    verify_val(jl_valueof(o));
    return mark_mode;
}

static inline int gc_setmark_pool(void *o, int mark_mode)
{
#ifdef GC_VERIFY
    if (verifying) {
        _gc_setmark(o, mark_mode);
        return mark_mode;
    }
#endif
    gcpage_t* page = page_metadata(o);
    int bits = gc_bits(o);
    if (bits == GC_QUEUED || bits == GC_MARKED) {
        mark_mode = GC_MARKED;
    }
    if (!(bits & GC_MARKED)) {
        if (mark_mode == GC_MARKED)
            perm_scanned_bytes += page->osize;
        else
            scanned_bytes += page->osize;
#ifdef OBJPROFILE
        objprofile_count(jl_typeof(o), mark_mode == GC_MARKED, page->osize);
#endif
    }
    _gc_setmark(o, mark_mode);
    page->gc_bits |= mark_mode;
    verify_val(jl_valueof(o));
    return mark_mode;
}


static inline int gc_setmark(jl_value_t *v, int sz, int mark_mode)
{
    jl_taggedvalue_t *o = jl_astaggedvalue(v);
    sz += sizeof_jl_taggedvalue_t;
#ifdef MEMDEBUG
    return gc_setmark_big(o, mark_mode);
#endif
    if (sz <= GC_MAX_SZCLASS + sizeof(buff_t))
        return gc_setmark_pool(o, mark_mode);
    else
        return gc_setmark_big(o, mark_mode);
}

#define gc_typeof(v) jl_typeof(v)
#define gc_val_buf(o) ((buff_t*)(((void**)(o))-1))

inline void gc_setmark_buf(void *o, int mark_mode)
{
    buff_t *buf = gc_val_buf(o);
#ifdef MEMDEBUG
    gc_setmark_big(buf, mark_mode);
    return;
#endif
    if (buf->pooled)
        gc_setmark_pool(buf, mark_mode);
    else
        gc_setmark_big(buf, mark_mode);
}

static NOINLINE void *malloc_page(void)
{
    void *ptr = (void*)0;
    int i;
    region_t* region;
    int region_i = 0;
    while(region_i < REGION_COUNT) {
        region = regions[region_i];
        if (region == NULL) {
            size_t alloc_size = sizeof(region_t);
#ifdef _OS_WINDOWS_
            char* mem = (char*)VirtualAlloc(NULL, sizeof(region_t) + GC_PAGE_SZ, MEM_RESERVE, PAGE_READWRITE);
#else
            if (GC_PAGE_SZ > jl_page_size)
                alloc_size += GC_PAGE_SZ;
            char* mem = (char*)mmap(0, alloc_size, PROT_READ | PROT_WRITE, MAP_NORESERVE | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
            mem = mem == MAP_FAILED ? NULL : mem;
#endif
            if (mem == NULL) {
                jl_printf(JL_STDERR, "could not allocate pools\n");
                abort();
            }
            if (GC_PAGE_SZ > jl_page_size) {
                // round data pointer up to the nearest GC_PAGE_DATA-aligned boundary
                // if mmap didn't already do so
                alloc_size += GC_PAGE_SZ;
                region = (region_t*)((char*)GC_PAGE_DATA(mem + GC_PAGE_SZ - 1));
            }
            else {
                region = (region_t*)mem;
            }
#ifdef _OS_WINDOWS_
            VirtualAlloc(region->freemap, REGION_PG_COUNT/8, MEM_COMMIT, PAGE_READWRITE);
            VirtualAlloc(region->meta, REGION_PG_COUNT*sizeof(gcpage_t), MEM_COMMIT, PAGE_READWRITE);
#endif
            memset(region->freemap, 0xff, REGION_PG_COUNT/8);
            regions[region_i] = region;
        }
        for(i = regions_lb[region_i]; i < REGION_PG_COUNT/32; i++) {
            if (region->freemap[i]) break;
        }
        if (i == REGION_PG_COUNT/32) {
            // region full
            region_i++;
            continue;
        }
        break;
    }
    if (region_i >= REGION_COUNT) {
        jl_printf(JL_STDERR, "increase REGION_COUNT or allocate less memory\n");
        abort();
    }
    if (regions_lb[region_i] < i)
        regions_lb[region_i] = i;
    if (regions_ub[region_i] < i)
        regions_ub[region_i] = i;

#if defined(_COMPILER_MINGW_)
    int j = __builtin_ffs(region->freemap[i]) - 1;
#elif defined(_COMPILER_MICROSOFT_)
    unsigned long j;
    _BitScanForward(&j, region->freemap[i]);
#else
    int j = ffs(region->freemap[i]) - 1;
#endif

    region->freemap[i] &= ~(uint32_t)(1 << j);
    ptr = region->pages[i*32 + j];
#ifdef _OS_WINDOWS_
    VirtualAlloc(ptr, GC_PAGE_SZ, MEM_COMMIT, PAGE_READWRITE);
#endif
    current_pg_count++;
    max_pg_count = max_pg_count < current_pg_count ? current_pg_count : max_pg_count;
    return ptr;
}

static void free_page(void *p)
{
    int pg_idx = -1;
    int i;
    for(i = 0; i < REGION_COUNT && regions[i] != NULL; i++) {
        pg_idx = PAGE_INDEX(regions[i], (char*)p+GC_PAGE_OFFSET);
        if (pg_idx >= 0 && pg_idx < REGION_PG_COUNT) break;
    }
    assert(i < REGION_COUNT && regions[i] != NULL);
    region_t *region = regions[i];
    uint32_t msk = (uint32_t)(1 << (pg_idx % 32));
    assert(!(region->freemap[pg_idx/32] & msk));
    region->freemap[pg_idx/32] ^= msk;
    free(region->meta[pg_idx].ages);
    // tell the OS we don't need these pages right now
    size_t decommit_size = GC_PAGE_SZ;
    if (GC_PAGE_SZ < jl_page_size) {
        // ensure so we don't release more memory than intended
        size_t n_pages = (GC_PAGE_SZ + jl_page_size - 1) / GC_PAGE_SZ;
        decommit_size = jl_page_size;
        p = (void*)((uintptr_t)&region->pages[pg_idx][0] & ~(jl_page_size - 1)); // round down to the nearest page
        pg_idx = PAGE_INDEX(region, (char*)p+GC_PAGE_OFFSET);
        if (pg_idx + n_pages > REGION_PG_COUNT) goto no_decommit;
        for (; n_pages--; pg_idx++) {
            msk = (uint32_t)(1 << ((pg_idx % 32)));
            if (!(region->freemap[pg_idx/32] & msk)) goto no_decommit;
        }
    }
#ifdef _OS_WINDOWS_
    VirtualFree(p, decommit_size, MEM_DECOMMIT);
#else
    madvise(p, decommit_size, MADV_DONTNEED);
#endif
no_decommit:
    if (regions_lb[i] > pg_idx/32) regions_lb[i] = pg_idx/32;
    current_pg_count--;
}

#define should_collect() (__unlikely(allocd_bytes>0))

static inline int maybe_collect(void)
{
    if (should_collect() || gc_debug_check_other()) {
        jl_gc_collect(0);
        return 1;
    }
    return 0;
}


// preserved values

DLLEXPORT int jl_gc_n_preserved_values(void)
{
    FOR_CURRENT_HEAP
        return preserved_values.len;
    END
}

DLLEXPORT void jl_gc_preserve(jl_value_t *v)
{
    FOR_CURRENT_HEAP
        arraylist_push(&preserved_values, (void*)v);
    END
}

DLLEXPORT void jl_gc_unpreserve(void)
{
    FOR_CURRENT_HEAP
        (void)arraylist_pop(&preserved_values);
    END
}

// weak references

DLLEXPORT jl_weakref_t *jl_gc_new_weakref(jl_value_t *value)
{
    jl_weakref_t *wr = (jl_weakref_t*)jl_gc_alloc_1w();
    jl_set_typeof(wr, jl_weakref_type);
    wr->value = value;
    FOR_CURRENT_HEAP
        arraylist_push(&weak_refs, wr);
    END
    return wr;
}

static void sweep_weak_refs(void)
{
    FOR_EACH_HEAP
        size_t n=0, ndel=0, l=weak_refs.len;
        jl_weakref_t *wr;
        void **lst = weak_refs.items;
        void *tmp;
#define SWAP_wr(a,b) (tmp=a,a=b,b=tmp,1)
        if (l == 0)
            return;
        do {
            wr = (jl_weakref_t*)lst[n];
            if (gc_marked(jl_astaggedvalue(wr))) {
                // weakref itself is alive
                if (!gc_marked(jl_astaggedvalue(wr->value)))
                    wr->value = (jl_value_t*)jl_nothing;
                n++;
            }
            else {
                ndel++;
            }
        } while ((n < l-ndel) && SWAP_wr(lst[n],lst[n+ndel]));

        weak_refs.len -= ndel;
    END
}

// big value list

static NOINLINE void *alloc_big(size_t sz)
{
    maybe_collect();
    size_t offs = offsetof(bigval_t, header);
    size_t allocsz = LLT_ALIGN(sz + offs, 16);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    bigval_t *v = (bigval_t*)malloc_a16(allocsz);
    if (v == NULL)
        jl_throw(jl_memory_exception);
    allocd_bytes += allocsz;
    gc_num.bigalloc++;
#ifdef MEMDEBUG
    memset(v, 0xee, allocsz);
#endif
    v->sz = allocsz;
    v->flags = 0;
    v->age = 0;
    FOR_CURRENT_HEAP
        v->next = big_objects;
        v->prev = &big_objects;
        if (v->next)
            v->next->prev = &v->next;
        big_objects = v;
    END
    return (void*)&v->header;
}

static int big_total;
static int big_freed;
static int big_reset;

// Sweep list rooted at *pv, removing and freeing any unmarked objects.
// Return pointer to last `next` field in the culled list.
static bigval_t** sweep_big_list(int sweep_mask, bigval_t** pv)
{
    bigval_t *v = *pv;
    while (v != NULL) {
        bigval_t *nxt = v->next;
        if (gc_marked(&v->header)) {
            pv = &v->next;
            int age = v->age;
            int bits = gc_bits(&v->header);
            if (age >= PROMOTE_AGE) {
                if (sweep_mask == GC_MARKED || bits == GC_MARKED_NOESC) {
                    bits = GC_QUEUED;
                }
            }
            else {
                inc_sat(age, PROMOTE_AGE);
                v->age = age;
                if ((sweep_mask & bits) == sweep_mask) {
                    bits = GC_CLEAN;
                    big_reset++;
                }
            }
            gc_bits(&v->header) = bits;
        }
        else {
            // Remove v from list and free it
            *pv = nxt;
            if (nxt)
                nxt->prev = pv;
            freed_bytes += v->sz&~3;
#ifdef MEMDEBUG
            memset(v, 0xbb, v->sz&~3);
#endif
            free_a16(v);
            big_freed++;
        }
        big_total++;
        v = nxt;
    }
    return pv;
}

static void sweep_big(int sweep_mask)
{
    FOR_EACH_HEAP
        sweep_big_list(sweep_mask, &big_objects);
    END
    if (sweep_mask == GC_MARKED) {
        bigval_t** last_next = sweep_big_list(sweep_mask, &big_objects_marked);
        // Move all survivors from big_objects_marked list to big_objects list.
        FOR_CURRENT_HEAP
            if (big_objects)
                big_objects->prev = last_next;
            *last_next = big_objects;
            big_objects = big_objects_marked;
            if (big_objects)
                big_objects->prev = &big_objects;
        END
        big_objects_marked = NULL;
    }
}

// tracking Arrays with malloc'd storage

void jl_gc_track_malloced_array(jl_array_t *a)
{
    FOR_CURRENT_HEAP
        mallocarray_t *ma;
        if (mafreelist == NULL) {
            ma = (mallocarray_t*)malloc(sizeof(mallocarray_t));
        }
        else {
            ma = mafreelist;
            mafreelist = ma->next;
        }
        ma->a = a;
        ma->next = mallocarrays;
        mallocarrays = ma;
    END
}

void jl_gc_count_allocd(size_t sz)
{
    allocd_bytes += sz;
}

static size_t array_nbytes(jl_array_t *a)
{
    size_t sz = 0;
    if (jl_array_ndims(a)==1)
        sz = a->elsize * a->maxsize + (a->elsize == 1 ? 1 : 0);
    else
        sz = a->elsize * jl_array_len(a);
    return sz;
}

void jl_gc_free_array(jl_array_t *a)
{
    if (a->how == 2) {
        char *d = (char*)a->data - a->offset*a->elsize;
        if (a->isaligned)
            free_a16(d);
        else
            free(d);
        freed_bytes += array_nbytes(a);
    }
}

static int mallocd_array_total;
static int mallocd_array_freed;


static void sweep_malloced_arrays(void)
{
    FOR_EACH_HEAP
        mallocarray_t *ma = mallocarrays;
        mallocarray_t **pma = &mallocarrays;
        while (ma != NULL) {
            mallocarray_t *nxt = ma->next;
            if (gc_marked(jl_astaggedvalue(ma->a))) {
                pma = &ma->next;
            }
            else {
                *pma = nxt;
                assert(ma->a->how == 2);
                jl_gc_free_array(ma->a);
                ma->next = mafreelist;
                mafreelist = ma;
                mallocd_array_freed++;
            }
            mallocd_array_total++;
            ma = nxt;
        }
    END
}

// pool allocation
static inline gcval_t *reset_page(pool_t *p, gcpage_t *pg, gcval_t *fl)
{
    pg->gc_bits = 0;
    pg->nfree = (GC_PAGE_SZ - GC_PAGE_OFFSET) / p->osize;
    pg->pool_n = p - norm_pools;
    memset(page_age(pg), 0, LLT_ALIGN(GC_PAGE_SZ / p->osize, 8));
    gcval_t *beg = (gcval_t*)(pg->data + GC_PAGE_OFFSET);
    gcval_t *end = (gcval_t*)((char*)beg + (pg->nfree - 1)*p->osize);
    end->next = fl;
    pg->allocd = 0;
    pg->fl_begin_offset = GC_PAGE_OFFSET;
    pg->fl_end_offset = (char*)end - (char*)beg + GC_PAGE_OFFSET;
    return beg;
}

static NOINLINE void add_page(pool_t *p)
{
    char *data = (char*)malloc_page();
    if (data == NULL)
        jl_throw(jl_memory_exception);
    gcpage_t *pg = page_metadata(data + GC_PAGE_OFFSET);
    pg->data = data;
    pg->osize = p->osize;
    pg->ages = (uint8_t*)malloc(LLT_ALIGN(GC_PAGE_SZ / p->osize, 8));
    gcval_t *fl = reset_page(p, pg, p->newpages);
    p->newpages = fl;
}

static inline void *__pool_alloc(pool_t* p, int osize, int end_offset)
{
    gcval_t *v, *end;
    if (__unlikely((allocd_bytes += osize) >= 0) || gc_debug_check_pool()) {
        //allocd_bytes -= osize;
        jl_gc_collect(0);
        //allocd_bytes += osize;
    }
    gc_num.poolalloc++;
    // first try to use the freelist
    v = p->freelist;
    if (v) {
        gcval_t* next = v->next;
        v->flags = 0;
        p->nfree--;
        p->freelist = next;
        if (__unlikely(GC_PAGE_DATA(v) != GC_PAGE_DATA(next))) {
            // we only update pg's fields when the freelist changes page
            // since pg's metadata is likely not in cache
            gcpage_t* pg = page_metadata(v);
            assert(pg->osize == p->osize);
            pg->nfree = 0;
            pg->allocd = 1;
            if (next)
                p->nfree = page_metadata(next)->nfree;
        }
        return v;
    }
    // if the freelist is empty we reuse empty but not freed pages
    v = p->newpages;
    if (__unlikely(!v)) {
        add_page(p);
        v = p->newpages;
    }
    end = (gcval_t*)&(GC_PAGE_DATA(v)[end_offset]);
    if (__likely(v != end)) {
        p->newpages = (gcval_t*)((char*)v + osize);
    }
    else {
        // like the freelist case, but only update the page metadata when it is full
        gcpage_t* pg = page_metadata(v);
        assert(pg->osize == p->osize);
        pg->nfree = 0;
        pg->allocd = 1;
        p->newpages = v->next;
    }
    v->flags = 0;
    return v;
}

// use this variant when osize is statically known
// and is definitely in sizeclasses
// GC_POOL_END_OFS uses an integer division
static inline void *_pool_alloc(pool_t *p, int osize)
{
    return __pool_alloc(p, osize, GC_POOL_END_OFS(osize));
}

static inline void *pool_alloc(pool_t *p)
{
    return __pool_alloc(p, p->osize, p->end_offset);
}

// pools are 16376 bytes large (GC_POOL_SZ - GC_PAGE_OFFSET)
static const int sizeclasses[N_POOLS] = {
#ifdef _P64
    8,
#else
    4, 8, 12,
#endif

    // 16 pools at 16-byte spacing
    16, 32, 48, 64, 80, 96, 112, 128,
    144, 160, 176, 192, 208, 224, 240, 256,

    // the following tables are computed for maximum packing efficiency via the formula:
    // sz=(div(2^14-8,rng)÷16)*16; hcat(sz, (2^14-8)÷sz, 2^14-(2^14-8)÷sz.*sz)'

    // rng = 60:-4:32 (8 pools)
    272, 288, 304, 336, 368, 400, 448, 496,
//   60,  56,  53,  48,  44,  40,  36,  33, /pool
//   64, 256, 272, 256, 192, 384, 256,  16, bytes lost

    // rng = 30:-2:16 (8 pools)
    544, 576, 624, 672, 736, 816, 896, 1008,
//   30,  28,  26,  24,  22,  20,  18,  16, /pool
//   64, 256, 160, 256, 192,  64, 256, 256, bytes lost

    // rng = 15:-1:8 (8 pools)
    1088, 1168, 1248, 1360, 1488, 1632, 1808, 2032
//    15,   14,   13,   12,   11,   10,    9,    8, /pool
//    64,   32,  160,   64,   16,   64,  112,  128, bytes lost
};


static inline int szclass(size_t sz)
{
#ifdef _P64
    if (sz <=    8)
        return 0;
    const int N = 0;
#else
    if (sz <=   12)
        return (sz + 3) / 4 - 1;
    const int N = 2;
#endif
    if (sz <=  256)
        return (sz + 15) / 16 + N;
    if (sz <=  496)
        return 16 - 16376 / 4 / LLT_ALIGN(sz, 16 * 4) + 16 + N;
    if (sz <= 1008)
        return 16 - 16376 / 2 / LLT_ALIGN(sz, 16 * 2) + 24 + N;
    assert(sz <= GC_MAX_SZCLASS + sizeof(buff_t) && sizeclasses[N_POOLS-1] == GC_MAX_SZCLASS + sizeof(buff_t));
    return     16 - 16376 / 1 / LLT_ALIGN(sz, 16 * 1) + 32 + N;
}

// sweep phase

static int skipped_pages = 0;
static int total_pages = 0;
static int freed_pages = 0;
static int lazy_freed_pages = 0;
static int page_done = 0;
static gcval_t** sweep_page(pool_t* p, gcpage_t* pg, gcval_t **pfl,int,int);
static void sweep_pool_region(gcval_t **pfl[N_POOLS], int region_i, int sweep_mask)
{
    region_t* region = regions[region_i];

    // the actual sweeping
    int ub = 0;
    int lb = regions_lb[region_i];
    for (int pg_i = 0; pg_i <= regions_ub[region_i]; pg_i++) {
        uint32_t line = region->freemap[pg_i];
        if (!!~line) {
            ub = pg_i;
            for (int j = 0; j < 32; j++) {
                if (!((line >> j) & 1)) {
                    gcpage_t *pg = &region->meta[pg_i*32 + j];
                    int p_n = pg->pool_n;
                    pool_t *p = &norm_pools[p_n];
                    int osize = pg->osize;
                    pfl[p_n] = sweep_page(p, pg, pfl[p_n], sweep_mask, osize);
                }
            }
        }
        else if (pg_i < lb) {
            lb = pg_i;
        }
    }
    regions_ub[region_i] = ub;
    regions_lb[region_i] = lb;
}

// Returns pointer to terminal pointer of list rooted at *pfl.
static gcval_t** sweep_page(pool_t* p, gcpage_t* pg, gcval_t **pfl, int sweep_mask, int osize)
{
#ifdef FREE_PAGES_EAGER
    int freedall;
#else
    int empty;
#endif
    gcval_t **prev_pfl = pfl;
    gcval_t *v;
    size_t old_nfree = 0, nfree = 0;
    int pg_freedall = 0, pg_total = 0, pg_skpd = 0;
    int obj_per_page = (GC_PAGE_SZ - GC_PAGE_OFFSET)/osize;
    char *data = pg->data;
    uint8_t *ages = page_age(pg);
    v = (gcval_t*)(data + GC_PAGE_OFFSET);
    char *lim = (char*)v + GC_PAGE_SZ - GC_PAGE_OFFSET - osize;
    freedall = 1;
    old_nfree += pg->nfree;

    if (pg->gc_bits == GC_MARKED) {
        // this page only contains GC_MARKED and free cells
        // if we are doing a quick sweep and nothing has been allocated inside since last sweep
        // we can skip it
        if (sweep_mask == GC_MARKED_NOESC && !pg->allocd) {
            // the position of the freelist begin/end in this page is stored in its metadata
            if (pg->fl_begin_offset != (uint16_t)-1) {
                *pfl = (gcval_t*)PAGE_PFL_BEG(pg);
                pfl = prev_pfl = PAGE_PFL_END(pg);
            }
            pg_skpd++;
            freedall = 0;
            goto free_page;
        }
    }
    else if (pg->gc_bits == GC_CLEAN) {
        goto free_page;
    }

    {  // scope to avoid clang goto errors
        int pg_nfree = 0;
        gcval_t **pfl_begin = NULL;
        uint8_t msk = 1; // mask for the age bit in the current age byte
        while ((char*)v <= lim) {
            int bits = gc_bits(v);
            if (!(bits & GC_MARKED)) {
                *pfl = v;
                pfl = &v->next;
                pfl_begin = pfl_begin ? pfl_begin : pfl;
                pg_nfree++;
                *ages &= ~msk;
            }
            else { // marked young or old
                if (*ages & msk) { // old enough
                    if (sweep_mask == GC_MARKED || bits == GC_MARKED_NOESC) {
                        gc_bits(v) = GC_QUEUED; // promote
                    }
                }
                else if ((sweep_mask & bits) == sweep_mask) {
                    gc_bits(v) = GC_CLEAN; // unmark
                }
                *ages |= msk;
                freedall = 0;
            }
            v = (gcval_t*)((char*)v + osize);
            msk <<= 1;
            if (!msk) {
                msk = 1;
                ages++;
            }
        }

        pg->fl_begin_offset = pfl_begin ? (char*)pfl_begin - data : (uint16_t)-1;
        pg->fl_end_offset = pfl_begin ? (char*)pfl - data : (uint16_t)-1;

        pg->nfree = pg_nfree;
        page_done++;
        pg->allocd = 0;
    }
 free_page:
    pg_freedall += freedall;

    // lazy version: (empty) if the whole page was already unused, free it
    // eager version: (freedall) free page as soon as possible
    // the eager one uses less memory.
    pg_total++;
    if (freedall) {
        // on quick sweeps, keep a few pages empty but allocated for performance
        if (sweep_mask == GC_MARKED_NOESC && lazy_freed_pages <= default_collect_interval/GC_PAGE_SZ) {
            gcval_t *begin = reset_page(p, pg, 0);
            gcval_t** pend = (gcval_t**)((char*)begin + ((int)pg->nfree - 1)*osize);
            gcval_t* npg = p->newpages;
            *pend = npg;
            p->newpages = begin;
            begin->next = (gcval_t*)0;
            lazy_freed_pages++;
            pfl = prev_pfl;
        }
        else {
            pfl = prev_pfl;
#ifdef MEMDEBUG
            memset(pg->data, 0xbb, GC_PAGE_SZ);
#endif
            free_page(data);
#ifdef MEMDEBUG
            memset(pg, 0xbb, sizeof(gcpage_t));
#endif
        }
        freed_pages++;
        nfree += obj_per_page;
    }
    else {
        if (sweep_mask == GC_MARKED)
            pg->gc_bits = GC_CLEAN;
        if (sweep_mask == GC_MARKED_NOESC)
            pg->gc_bits = GC_MARKED;
        nfree += pg->nfree;
    }

    skipped_pages += pg_skpd;
    total_pages += pg_total;
    freed_bytes += (nfree - old_nfree)*osize;
    return pfl;
}

//extern void jl_unmark_symbols(void);

static void gc_sweep_once(int sweep_mask)
{
#ifdef GC_TIME
    double t0 = clock_now();
    mallocd_array_total = 0;
    mallocd_array_freed = 0;
#endif
    sweep_malloced_arrays();
#ifdef GC_TIME
    jl_printf(JL_STDOUT, "GC sweep arrays %.2f (freed %d/%d)\n", (clock_now() - t0)*1000, mallocd_array_freed, mallocd_array_total);
    t0 = clock_now();
    big_total = 0;
    big_freed = 0;
    big_reset = 0;
#endif
    sweep_big(sweep_mask);
#ifdef GC_TIME
    jl_printf(JL_STDOUT, "GC sweep big %.2f (freed %d/%d with %d rst)\n", (clock_now() - t0)*1000, big_freed, big_total, big_reset);
    t0 = clock_now();
#endif
    //if (sweep_mask == GC_MARKED)
    //    jl_unmark_symbols();
#ifdef GC_TIME
    jl_printf(JL_STDOUT, "GC sweep symbols %.2f\n", (clock_now() - t0)*1000);
#endif
}

// returns 0 if not finished
static int gc_sweep_inc(int sweep_mask)
{
#ifdef GC_TIME
    double t0 = clock_now();
#endif
    skipped_pages = 0;
    total_pages = 0;
    freed_pages = 0;
    lazy_freed_pages = 0;
    page_done = 0;
    int finished = 1;

    gcval_t **pfl[N_POOLS];

    // update metadata of pages that were pointed to by freelist or newpages from a pool
    // i.e. pages being the current allocation target
    FOR_EACH_HEAP
        for (int i = 0; i < N_POOLS; i++) {
            pool_t* p = &HEAP(norm_pools)[i];
            gcval_t* last = p->freelist;
            if (last) {
                gcpage_t* pg = page_metadata(last);
                pg->allocd = 1;
                pg->nfree = p->nfree;
            }
            p->freelist =  NULL;
            pfl[i] = &p->freelist;

            last = p->newpages;
            if (last) {
                gcpage_t* pg = page_metadata(last);
                pg->nfree = (GC_PAGE_SZ - ((char*)last - GC_PAGE_DATA(last))) / p->osize;
                pg->allocd = 1;
            }
            p->newpages = NULL;
        }
    END

    for (int i = 0; i < REGION_COUNT; i++) {
        if (regions[i])
            /*finished &= */sweep_pool_region(pfl, i, sweep_mask);
    }


    // null out terminal pointers of free lists and cache back pg->nfree in the pool_t
    FOR_EACH_HEAP
        for (int i = 0; i < N_POOLS; i++) {
            pool_t* p = &HEAP(norm_pools)[i];
            *pfl[i] = NULL;
            if (p->freelist) {
                p->nfree = page_metadata(p->freelist)->nfree;
            }
        }
    END

#ifdef GC_TIME
    double sweep_pool_sec = clock_now() - t0;
    double sweep_speed = ((((double)total_pages)*GC_PAGE_SZ)/(1024*1024*1024))/sweep_pool_sec;
    jl_printf(JL_STDOUT, "GC sweep pools %s %.2f at %.1f GB/s (skipped %d%% of %d, done %d pgs, %d freed with %d lazily) mask %d\n", finished ? "end" : "inc", sweep_pool_sec*1000, sweep_speed, total_pages ? (skipped_pages*100)/total_pages : 0, total_pages, page_done, freed_pages, lazy_freed_pages,  sweep_mask);
#endif
    return finished;
}

// mark phase

static jl_value_t **mark_stack = NULL;
static jl_value_t **mark_stack_base = NULL;
static size_t mark_stack_size = 0;
static size_t mark_sp = 0;


static void grow_mark_stack(void)
{
    size_t newsz = mark_stack_size>0 ? mark_stack_size*2 : 32000;
    size_t offset = mark_stack - mark_stack_base;
    mark_stack_base = (jl_value_t**)realloc(mark_stack_base, newsz*sizeof(void*));
    if (mark_stack_base == NULL) {
        jl_printf(JL_STDERR, "Couldn't grow mark stack to : %" PRIuPTR "\n",
                  (uintptr_t)newsz);
        exit(1);
    }
    mark_stack = mark_stack_base + offset;
    mark_stack_size = newsz;
}

static int max_msp = 0;

static void reset_remset(void)
{
    FOR_EACH_HEAP
        arraylist_t *tmp = remset;
        remset = last_remset;
        last_remset = tmp;
        remset->len = 0;
        remset_nptr = 0;
    END
}

DLLEXPORT void jl_gc_queue_root(jl_value_t *ptr)
{
    FOR_CURRENT_HEAP
        jl_taggedvalue_t *o = jl_astaggedvalue(ptr);
        assert(gc_bits(o) != GC_QUEUED);
        gc_bits(o) = GC_QUEUED;
        arraylist_push(remset, ptr);
        remset_nptr++; // conservative
    END
}

void gc_queue_binding(jl_binding_t *bnd)
{
    FOR_CURRENT_HEAP
        buff_t *buf = gc_val_buf(bnd);
        assert(gc_bits(buf) != GC_QUEUED);
        gc_bits(buf) = GC_QUEUED;
        arraylist_push(&rem_bindings, bnd);
    END
}

static int push_root(jl_value_t *v, int d, int);
#ifdef JL_DEBUG_BUILD
static void *volatile gc_findval; // for usage from gdb, for finding the gc-root for a value
#endif
static inline int gc_push_root(void *v, int d) // v isa jl_value_t*
{
#ifdef JL_DEBUG_BUILD
    if (v == gc_findval)
        jl_raise_debugger();
#endif
    assert(v != NULL);
    jl_taggedvalue_t* o = jl_astaggedvalue(v);
    verify_val(v);
    int bits = gc_bits(o);
    if (!gc_marked(o)) {
        return push_root((jl_value_t*)v, d, bits);
    }
    return bits;
}

void jl_gc_setmark(jl_value_t *v) // TODO rename this as it is misleading now
{
    //    int64_t s = perm_scanned_bytes;
    jl_taggedvalue_t *o = jl_astaggedvalue(v);
    if (!gc_marked(o)) {
        //        objprofile_count(jl_typeof(v), 1, 16);
#ifdef MEMDEBUG
        gc_setmark_big(o, GC_MARKED_NOESC);
#else
        gc_setmark_pool(o, GC_MARKED_NOESC);
#endif
    }
    //    perm_scanned_bytes = s;
}

static void gc_mark_stack(jl_value_t* ta, jl_gcframe_t *s, ptrint_t offset, int d)
{
    while (s != NULL) {
        s = (jl_gcframe_t*)((char*)s + offset);
        jl_value_t ***rts = (jl_value_t***)(((void**)s)+2);
        size_t nr = s->nroots>>1;
        if (s->nroots & 1) {
            for(size_t i=0; i < nr; i++) {
                jl_value_t **ptr = (jl_value_t**)((char*)rts[i] + offset);
                if (*ptr != NULL)
                    gc_push_root(*ptr, d);
            }
        }
        else {
            for(size_t i=0; i < nr; i++) {
                if (rts[i] != NULL) {
                    verify_parent2("task", ta, &rts[i], "stack(%d)", (int)i);
                    gc_push_root(rts[i], d);
                }
            }
        }
        s = s->prev;
    }
}

NOINLINE static int gc_mark_module(jl_module_t *m, int d)
{
    size_t i;
    int refyoung = 0;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            gc_setmark_buf(b, gc_bits(jl_astaggedvalue(m)));
#ifdef GC_VERIFY
            void* vb = gc_val_buf(b);
            verify_parent1("module", m, &vb, "binding_buff");
#endif
            if (b->value != NULL) {
                verify_parent2("module", m, &b->value, "binding(%s)", b->name->name);
                refyoung |= gc_push_root(b->value, d);
            }
            if (b->globalref != NULL)
                refyoung |= gc_push_root(b->globalref, d);
        }
    }
    // this is only necessary because bindings for "using" modules
    // are added only when accessed. therefore if a module is replaced
    // after "using" it but before accessing it, this array might
    // contain the only reference.
    for(i=0; i < m->usings.len; i++) {
        refyoung |= gc_push_root(m->usings.items[i], d);
    }
    if (m->constant_table) {
        verify_parent1("module", m, &m->constant_table, "constant_table");
        refyoung |= gc_push_root(m->constant_table, d);
    }

    if (m->parent) {
        refyoung |= gc_push_root(m->parent, d);
    }

    return refyoung;
}

static void gc_mark_task_stack(jl_task_t *ta, int d)
{
    if (ta->stkbuf != NULL || ta == jl_current_task) {
        if (ta->stkbuf != NULL) {
            gc_setmark_buf(ta->stkbuf, gc_bits(jl_astaggedvalue(ta)));
        }
#ifdef COPY_STACKS
        ptrint_t offset;
        if (ta == jl_current_task) {
            offset = 0;
            gc_mark_stack((jl_value_t*)ta, jl_pgcstack, offset, d);
        }
        else {
            offset = (char *)ta->stkbuf - ((char *)jl_stackbase - ta->ssize);
            gc_mark_stack((jl_value_t*)ta, ta->gcstack, offset, d);
        }
#else
        gc_mark_stack((jl_value_t*)ta, ta->gcstack, 0, d);
#endif
    }
}

NOINLINE static void gc_mark_task(jl_task_t *ta, int d)
{
    if (ta->parent) gc_push_root(ta->parent, d);
    if (ta->last) gc_push_root(ta->last, d);
    gc_push_root(ta->tls, d);
    gc_push_root(ta->consumers, d);
    gc_push_root(ta->donenotify, d);
    gc_push_root(ta->exception, d);
    if (ta->backtrace) gc_push_root(ta->backtrace, d);
    if (ta->start)  gc_push_root(ta->start, d);
    if (ta->result) gc_push_root(ta->result, d);
    gc_mark_task_stack(ta, d);
}


// for chasing down unwanted references
/*
static jl_value_t *lookforme = NULL;
DLLEXPORT void jl_gc_lookfor(jl_value_t *v) { lookforme = v; }
*/

#define MAX_MARK_DEPTH 400
// mark v and recurse on its children (or store them on the mark stack when recursion depth becomes too high)
// it does so assuming the gc bits of v are "bits" and returns the new bits of v
// if v becomes GC_MARKED (old) and some of its children are GC_MARKED_NOESC (young), v is added to the remset
static int push_root(jl_value_t *v, int d, int bits)
{
    assert(v != NULL);
    jl_value_t *vt = (jl_value_t*)gc_typeof(v);
    int refyoung = 0, nptr = 0;

    if (vt == (jl_value_t*)jl_weakref_type) {
        bits = gc_setmark(v, sizeof(jl_weakref_t), GC_MARKED_NOESC);
        goto ret;
    }
    if ((jl_is_datatype(vt) && ((jl_datatype_t*)vt)->pointerfree)) {
        int sz = jl_datatype_size(vt);
        bits = gc_setmark(v, sz, GC_MARKED_NOESC);
        goto ret;
    }
#define MARK(v, s) do {                         \
            s;                                  \
            if (d >= MAX_MARK_DEPTH)            \
                goto queue_the_root;            \
            if (should_timeout())               \
                goto queue_the_root;            \
    } while (0)

    d++;

    // some values have special representations
    if (vt == (jl_value_t*)jl_simplevector_type) {
        size_t l = jl_svec_len(v);
        MARK(v, bits = gc_setmark(v, l*sizeof(void*) + sizeof(jl_svec_t), GC_MARKED_NOESC));
        jl_value_t **data = ((jl_svec_t*)v)->data;
        nptr += l;
        for(size_t i=0; i < l; i++) {
            jl_value_t *elt = data[i];
            if (elt != NULL) {
                verify_parent2("svec", v, &data[i], "elem(%d)", (int)i);
                refyoung |= gc_push_root(elt, d);
            }
        }
    }
    else if (((jl_datatype_t*)(vt))->name == jl_array_typename) {
        jl_array_t *a = (jl_array_t*)v;
        jl_taggedvalue_t *o = jl_astaggedvalue(v);
        int todo = !(bits & GC_MARKED);
        if (a->pooled)
#ifdef MEMDEBUG
#define _gc_setmark_pool gc_setmark_big
#else
#define _gc_setmark_pool gc_setmark_pool
#endif
            MARK(a,
                 bits = _gc_setmark_pool(o, GC_MARKED_NOESC);
                 if (a->how == 2 && todo) {
                     objprofile_count(MATY, gc_bits(o) == GC_MARKED, array_nbytes(a));
                     if (gc_bits(o) == GC_MARKED)
                         perm_scanned_bytes += array_nbytes(a);
                     else
                         scanned_bytes += array_nbytes(a);
                 });
        else
            MARK(a,
                 bits = gc_setmark_big(o, GC_MARKED_NOESC);
                 if (a->how == 2 && todo) {
                     objprofile_count(MATY, gc_bits(o) == GC_MARKED, array_nbytes(a));
                     if (gc_bits(o) == GC_MARKED)
                         perm_scanned_bytes += array_nbytes(a);
                     else
                         scanned_bytes += array_nbytes(a);
                 });
        if (a->how == 3) {
            jl_value_t *owner = jl_array_data_owner(a);
            refyoung |= gc_push_root(owner, d);
            goto ret;
        }
        else if (a->how == 1) {
#ifdef GC_VERIFY
            void* val_buf = gc_val_buf((char*)a->data - a->offset*a->elsize);
            verify_parent1("array", v, &val_buf, "buffer ('loc' addr is meaningless)");
#endif
            gc_setmark_buf((char*)a->data - a->offset*a->elsize, gc_bits(o));
        }
        if (a->ptrarray && a->data!=NULL) {
            size_t l = jl_array_len(a);
            if (l > 100000 && d > MAX_MARK_DEPTH-10) {
                // don't mark long arrays at high depth, to try to avoid
                // copying the whole array into the mark queue
                goto queue_the_root;
            }
            else {
                nptr += l;
                void *data = a->data;
                for(size_t i=0; i < l; i++) {
                    jl_value_t *elt = ((jl_value_t**)data)[i];
                    if (elt != NULL) {
                        verify_parent2("array", v, &((jl_value_t**)data)[i], "elem(%d)", (int)i);
                        refyoung |= gc_push_root(elt, d);
                    }
                    // try to split large array marking (incremental mark TODO)
                    // if (should_timeout() && l > 1000) goto queue_the_root;
                }
            }
        }
    }
    else if (vt == (jl_value_t*)jl_module_type) {
        // should increase nptr here
        MARK(v, bits = gc_setmark(v, sizeof(jl_module_t), GC_MARKED_NOESC));
        refyoung |= gc_mark_module((jl_module_t*)v, d);
    }
    else if (vt == (jl_value_t*)jl_task_type) {
        // ditto nptr
        MARK(v, bits = gc_setmark(v, sizeof(jl_task_t), GC_MARKED_NOESC));
        gc_mark_task((jl_task_t*)v, d);
        // tasks should always be remarked since we do not trigger the write barrier
        // for stores to stack slots
        refyoung = GC_MARKED_NOESC;
    }
    else if (vt == (jl_value_t*)jl_symbol_type) {
        //gc_setmark_other(v, GC_MARKED); // symbols have their own allocator and are never freed
    }
    // this check should not be needed but it helps catching corruptions early
    else if (gc_typeof(vt) == (jl_value_t*)jl_datatype_type) {
        jl_datatype_t *dt = (jl_datatype_t*)vt;
        size_t dtsz;
        if (dt == jl_datatype_type)
            dtsz = NWORDS(sizeof(jl_datatype_t) + jl_datatype_nfields(v)*sizeof(jl_fielddesc_t))*sizeof(void*);
        else
            dtsz = jl_datatype_size(dt);
        MARK(v, bits = gc_setmark(v, dtsz, GC_MARKED_NOESC));
        int nf = (int)jl_datatype_nfields(dt);
        // TODO check if there is a perf improvement for objects with a lot of fields
        // int fdsz = sizeof(void*)*nf;
        // void** children = alloca(fdsz);
        // int ci = 0;
        for(int i=0; i < nf; i++) {
            if (jl_field_isptr(dt, i)) {
                nptr++;
                jl_value_t **slot = (jl_value_t**)((char*)v +
                                                   jl_field_offset(dt, i));
                jl_value_t *fld = *slot;
                if (fld) {
                    verify_parent2("object", v, slot, "field(%d)", i);
                    //children[ci++] = fld;
                    refyoung |= gc_push_root(fld, d);
                }
            }
        }
        //while(ci)
        //  refyoung |= gc_push_root(children[--ci], d);
    }
    else {
        jl_printf(JL_STDOUT, "GC error (probable corruption) :\n");
        jl_(vt);
        abort();
    }

 ret:
#ifdef GC_VERIFY
    if (verifying) return bits;
#endif
    if ((bits == GC_MARKED) && (refyoung == GC_MARKED_NOESC)) {
        remset_nptr += nptr;
        FOR_CURRENT_HEAP
            // v is an old object referencing young objects
            arraylist_push(remset, v);
        END
    }
    return bits;

#undef MARK

 queue_the_root:
    if (mark_sp >= mark_stack_size) grow_mark_stack();
    mark_stack[mark_sp++] = (jl_value_t*)v;
    max_msp = max_msp > mark_sp ? max_msp : mark_sp;
    return bits;
}

static void visit_mark_stack_inc(int mark_mode)
{
    while(mark_sp > 0 && !should_timeout()) {
        jl_value_t* v = mark_stack[--mark_sp];
        assert(gc_bits(jl_astaggedvalue(v)) == GC_QUEUED ||
               gc_bits(jl_astaggedvalue(v)) == GC_MARKED ||
               gc_bits(jl_astaggedvalue(v)) == GC_MARKED_NOESC);
        push_root(v, 0, gc_bits(jl_astaggedvalue(v)));
    }
}

static void visit_mark_stack(int mark_mode)
{
    int ct = check_timeout;
    check_timeout = 0;
    visit_mark_stack_inc(mark_mode);
    assert(!mark_sp);
    check_timeout = ct;
}

void jl_mark_box_caches(void);

extern JL_THREAD jl_value_t * volatile jl_task_arg_in_transit;
#if defined(GCTIME) || defined(GC_FINAL_STATS)
double clock_now(void);
#endif

extern jl_module_t *jl_old_base_module;
extern jl_array_t *typeToTypeId;
extern jl_array_t *jl_module_init_order;

static int inc_count = 0;
static int quick_count = 0;

// mark the initial root set
static void pre_mark(void)
{
    // modules
    gc_push_root(jl_main_module, 0);
    gc_push_root(jl_current_module, 0);
    if (jl_old_base_module) gc_push_root(jl_old_base_module, 0);
    gc_push_root(jl_internal_main_module, 0);
    gc_push_root(jl_root_task, 0);
    gc_push_root(jl_current_task, 0);

    // invisible builtin values
    if (jl_an_empty_cell) gc_push_root(jl_an_empty_cell, 0);
    gc_push_root(jl_exception_in_transit, 0);
    gc_push_root(jl_task_arg_in_transit, 0);
    gc_push_root(typeToTypeId, 0);
    if (jl_module_init_order != NULL)
        gc_push_root(jl_module_init_order, 0);

    size_t i;

    // stuff randomly preserved
    FOR_EACH_HEAP
        for(i=0; i < preserved_values.len; i++) {
            gc_push_root((jl_value_t*)preserved_values.items[i], 0);
        }
    END

    // objects currently being finalized
    for(i=0; i < to_finalize.len; i++) {
        gc_push_root(to_finalize.items[i], 0);
    }

    jl_mark_box_caches();
    gc_push_root(jl_unprotect_stack_func, 0);
    gc_push_root(jl_bottom_func, 0);
    gc_push_root(jl_typetype_type, 0);

    // constants
    gc_push_root(jl_emptysvec, 0);
    gc_push_root(jl_emptytuple, 0);
    gc_push_root(jl_typeof(jl_emptytuple), 0);
    gc_push_root(jl_true, 0);
    gc_push_root(jl_false, 0);
}

static int n_finalized;

// find unmarked objects that need to be finalized from the finalizer list "list".
// this must happen last in the mark phase.
// if dryrun == 1, it does not schedule any actual finalization and only marks finalizers
static void post_mark(arraylist_t *list, int dryrun)
{
    n_finalized = 0;
    for(size_t i=0; i < list->len; i+=2) {
        jl_value_t *v = (jl_value_t*)list->items[i];
        jl_value_t *fin = (jl_value_t*)list->items[i+1];
        int isfreed = !gc_marked(jl_astaggedvalue(v));
        gc_push_root(fin, 0);
        int isold = list == &finalizer_list && gc_bits(jl_astaggedvalue(v)) == GC_MARKED && gc_bits(jl_astaggedvalue(fin)) == GC_MARKED;
        if (!dryrun && (isfreed || isold)) {
            // remove from this list
            if (i < list->len - 2) {
                list->items[i] = list->items[list->len-2];
                list->items[i+1] = list->items[list->len-1];
                i -= 2;
            }
            list->len -= 2;
        }
        if (isfreed) {
            // schedule finalizer or execute right away if it is not julia code
            if (gc_typeof(fin) == (jl_value_t*)jl_voidpointer_type) {
                void *p = jl_unbox_voidpointer(fin);
                if (!dryrun && p)
                    ((void (*)(void*))p)(jl_data_ptr(v));
                continue;
            }
            gc_push_root(v, 0);
            if (!dryrun) schedule_finalization(v, fin);
            n_finalized++;
        }
        if (!dryrun && isold) {
            arraylist_push(&finalizer_list_marked, v);
            arraylist_push(&finalizer_list_marked, fin);
        }
    }
    visit_mark_stack(GC_MARKED_NOESC);
}

// collector entry point and control

static int is_gc_enabled = 1;
DLLEXPORT int jl_gc_enable(int on)
{
    int prev = is_gc_enabled;
    is_gc_enabled = (on!=0);
    return prev;
}
DLLEXPORT int jl_gc_is_enabled(void) { return is_gc_enabled; }

DLLEXPORT int64_t jl_gc_total_bytes(void) { return total_allocd_bytes + allocd_bytes + collect_interval; }
DLLEXPORT uint64_t jl_gc_total_hrtime(void) { return total_gc_time; }
DLLEXPORT GC_Num jl_gc_num(void) { return gc_num; }

DLLEXPORT int64_t jl_gc_diff_total_bytes(void)
{
    int64_t oldtb = last_gc_total_bytes;
    int64_t newtb = jl_gc_total_bytes();
    last_gc_total_bytes = newtb;
    return newtb - oldtb;
}
void jl_gc_sync_total_bytes(void) {last_gc_total_bytes = jl_gc_total_bytes();}

#if defined(MEMPROFILE)
static void all_pool_stats(void);
static void big_obj_stats(void);
#endif

#ifdef OBJPROFILE
static void reset_obj_profile()
{
    for(int g=0; g < 3; g++) {
        htable_reset(&obj_counts[g], 0);
        htable_reset(&obj_sizes[g], 0);
    }
}

static void print_obj_profile(htable_t nums, htable_t sizes)
{
    for(int i=0; i < nums.size; i+=2) {
        if (nums.table[i+1] != HT_NOTFOUND) {
            void* ty = nums.table[i];
            int num = (int)nums.table[i+1] - 1;
            size_t sz = (int)ptrhash_get(&sizes, ty) - 1;
            jl_printf(JL_STDERR, "   %6d : %4d kB of ", num, sz/1024);
            if (ty == BUFFTY)
                jl_printf(JL_STDERR, "buffer");
            else if (ty == MATY)
                jl_printf(JL_STDERR, "malloc");
            else
                jl_static_show(JL_STDERR, (jl_value_t*)ty);
            jl_printf(JL_STDERR, "\n");
        }
    }
}

void print_obj_profiles(void)
{
    jl_printf(JL_STDERR, "Transient mark :\n");
    print_obj_profile(obj_counts[0], obj_sizes[0]);
    jl_printf(JL_STDERR, "Perm mark :\n");
    print_obj_profile(obj_counts[1], obj_sizes[1]);
    jl_printf(JL_STDERR, "Remset :\n");
    print_obj_profile(obj_counts[2], obj_sizes[2]);
}
#endif

#if defined(GC_TIME)
static int saved_mark_sp = 0;
#endif
static int sweep_mask = GC_MARKED;
#define MIN_SCAN_BYTES 1024*1024

static void gc_mark_task_stack(jl_task_t*,int);

void prepare_sweep(void)
{
}

void jl_gc_collect(int full)
{
    if (!is_gc_enabled) return;
    if (jl_in_gc) return;
    char *stack_hi = (char*)gc_get_stack_ptr();
    gc_debug_print();
    JL_SIGATOMIC_BEGIN();
    jl_in_gc = 1;
    uint64_t t0 = jl_hrtime();
    int recollect = 0;
#if defined(GC_TIME)
    int wb_activations = mark_sp - saved_mark_sp;
#endif
    int64_t last_perm_scanned_bytes = perm_scanned_bytes;
    if (!sweeping) {

        inc_count++;
        quick_count++;

        scanned_bytes_goal = inc_count*(live_bytes/gc_inc_steps + mark_sp*sizeof(void*));
        scanned_bytes_goal = scanned_bytes_goal < MIN_SCAN_BYTES ? MIN_SCAN_BYTES : scanned_bytes_goal;
        if (gc_inc_steps > 1)
            check_timeout = 1;
        assert(mark_sp == 0);

        // 1. mark every object in the remset
        reset_remset();
        FOR_EACH_HEAP
            // avoid counting remembered objects & bindings twice in perm_scanned_bytes
            for(int i = 0; i < last_remset->len; i++) {
                jl_value_t *item = (jl_value_t*)last_remset->items[i];
                objprofile_count(jl_typeof(item), 2, 0);
                gc_bits(jl_astaggedvalue(item)) = GC_MARKED;
            }
            for (int i = 0; i < rem_bindings.len; i++) {
                void *ptr = rem_bindings.items[i];
                gc_bits(gc_val_buf(ptr)) = GC_MARKED;
            }

            for (int i = 0; i < last_remset->len; i++) {
                jl_value_t *item = (jl_value_t*)last_remset->items[i];
                push_root(item, 0, GC_MARKED);
            }
        END

        // 2. mark every object in a remembered binding
        int n_bnd_refyoung = 0;
        FOR_EACH_HEAP
            for (int i = 0; i < rem_bindings.len; i++) {
                jl_binding_t *ptr = (jl_binding_t*)rem_bindings.items[i];
                // A null pointer can happen here when the binding is cleaned up
                // as an exception is thrown after it was already queued (#10221)
                if (!ptr->value) continue;
                if (gc_push_root(ptr->value, 0) == GC_MARKED_NOESC) {
                    rem_bindings.items[n_bnd_refyoung] = ptr;
                    n_bnd_refyoung++;
                }
            }
            rem_bindings.len = n_bnd_refyoung;
        END

        // 3. walk roots
        pre_mark();
        visit_mark_stack(GC_MARKED_NOESC);

        allocd_bytes_since_sweep += allocd_bytes + (int64_t)collect_interval;

#if defined(GC_TIME) || defined(GC_FINAL_STATS)
        uint64_t mark_pause = jl_hrtime() - t0;
#endif
#ifdef GC_TIME
        FOR_EACH_HEAP
            jl_printf(JL_STDOUT, "GC mark pause %.2f ms | scanned %ld kB = %ld + %ld | stack %d -> %d (wb %d) | remset %d %d\n", NS2MS(mark_pause), (scanned_bytes + perm_scanned_bytes)/1024, scanned_bytes/1024, perm_scanned_bytes/1024, saved_mark_sp, mark_sp, wb_activations, last_remset->len, remset_nptr);
        END
        saved_mark_sp = mark_sp;
#endif
#ifdef GC_FINAL_STATS
        total_mark_time += mark_pause;
#endif
    }
    #ifdef GC_TIME
    int64_t bonus = -1, SAVE = -1, SAVE2 = -1, SAVE3 = -1, pct = -1;
    #endif
    int64_t estimate_freed = -1;

#if defined(GC_TIME) || defined(GC_FINAL_STATS)
    uint64_t post_time = 0, finalize_time = 0;
#endif
    if (mark_sp == 0 || sweeping) {
#if defined(GC_TIME) || defined(GC_FINAL_STATS)
        uint64_t sweep_t0 = jl_hrtime();
#endif
        int64_t actual_allocd = allocd_bytes_since_sweep;
        if (!sweeping) {
            // marking is over
#if defined(GC_TIME) || defined(GC_FINAL_STATS)
            post_time = jl_hrtime();
#endif
            // 4. check for objects to finalize
            post_mark(&finalizer_list, 0);
            if (prev_sweep_mask == GC_MARKED) {
                post_mark(&finalizer_list_marked, 0);
            }
#if defined(GC_TIME) || defined(GC_FINAL_STATS)
            post_time = jl_hrtime() - post_time;
#endif
            estimate_freed = live_bytes - scanned_bytes - perm_scanned_bytes + actual_allocd;

            gc_verify();

#if defined(MEMPROFILE)
            all_pool_stats();
            big_obj_stats();
#endif
#ifdef OBJPROFILE
            print_obj_profiles();
            reset_obj_profile();
#endif
            total_allocd_bytes += allocd_bytes_since_sweep;
            if (prev_sweep_mask == GC_MARKED_NOESC)
                promoted_bytes += perm_scanned_bytes - last_perm_scanned_bytes;
            // 5. next collection decision
            int not_freed_enough = estimate_freed < (7*(actual_allocd/10));
            int large_frontier = remset_nptr*sizeof(void*) >= default_collect_interval; // many pointers in the intergen frontier => "quick" mark is not quick
            if ((full || large_frontier || ((not_freed_enough || promoted_bytes >= collect_interval) && (promoted_bytes >= default_collect_interval || prev_sweep_mask == GC_MARKED))) && n_pause > 1) {
                if (prev_sweep_mask != GC_MARKED || full) {
                    if (full) recollect = 1; // TODO enable this?
                }
                if (large_frontier)
                    collect_interval = last_long_collect_interval;
                if (not_freed_enough || large_frontier) {
                    if (collect_interval < default_collect_interval)
                        collect_interval = default_collect_interval;
                    else if (collect_interval <= 2*(max_collect_interval/5)) {
                        collect_interval = 5*(collect_interval/2);
                    }
                }
                last_long_collect_interval = collect_interval;
                sweep_mask = GC_MARKED;
                promoted_bytes = 0;
                quick_count = 0;
            }
            else {
                collect_interval = default_collect_interval/2;
#ifdef GC_DEBUG_ENV
                sweep_mask = gc_debug_env.sweep_mask;
#else
                sweep_mask = GC_MARKED_NOESC;
#endif
            }
            if (sweep_mask == GC_MARKED)
                perm_scanned_bytes = 0;
            scanned_bytes = 0;
            // 5. start sweeping
            sweep_weak_refs();
            gc_sweep_once(sweep_mask);
            sweeping = 1;
            gc_scrub(stack_hi);
        }
        if (gc_sweep_inc(sweep_mask)) {
            // sweeping is over
            // 6. if it is a quick sweep, put back the remembered objects in queued state
            // so that we don't trigger the barrier again on them.
            FOR_EACH_HEAP
                if (sweep_mask == GC_MARKED_NOESC) {
                    for (int i = 0; i < remset->len; i++) {
                        gc_bits(jl_astaggedvalue(remset->items[i])) = GC_QUEUED;
                    }
                    for (int i = 0; i < rem_bindings.len; i++) {
                        void *ptr = rem_bindings.items[i];
                        gc_bits(gc_val_buf(ptr)) = GC_QUEUED;
                    }
                }
                else {
                    remset->len = 0;
                    rem_bindings.len = 0;
                    n_full_sweep++;
                }
            END

            sweeping = 0;
#ifdef GC_TIME
            SAVE2 = freed_bytes;
            SAVE3 = allocd_bytes_since_sweep;
            pct = actual_allocd ? (freed_bytes*100)/actual_allocd : -1;
#endif
            prev_sweep_mask = sweep_mask;


            allocd_bytes = -(int64_t)collect_interval;
            inc_count = 0;
            live_bytes += -freed_bytes + allocd_bytes_since_sweep;
            allocd_bytes_since_sweep = 0;
            jl_gc_total_freed_bytes += freed_bytes;
            freed_bytes = 0;

#if defined(GC_FINAL_STATS) || defined(GC_TIME)
            finalize_time = jl_hrtime();
#endif
            if (!jl_gc_finalizers_inhibited) {
                run_finalizers();
            }
#if defined(GC_FINAL_STATS) || defined(GC_TIME)
            finalize_time = jl_hrtime() - finalize_time;
#endif
        }
#if defined(GC_FINAL_STATS) || defined(GC_TIME)
        uint64_t sweep_pause = jl_hrtime() - sweep_t0;
#endif
#ifdef GC_FINAL_STATS
        total_sweep_time += sweep_pause - finalize_time - post_time;
        total_fin_time += finalize_time + post_time;
#endif
#ifdef GC_TIME
        jl_printf(JL_STDOUT, "GC sweep pause %.2f ms live %ld kB (freed %d kB EST %d kB [error %d] = %d%% of allocd %d kB b/r %ld/%ld) (%.2f ms in post_mark, %.2f ms in %d fin) (marked in %d inc) mask %d | next in %d kB\n", NS2MS(sweep_pause), live_bytes/1024, SAVE2/1024, estimate_freed/1024, (SAVE2 - estimate_freed), pct, SAVE3/1024, bonus/1024, SAVE/1024, NS2MS(post_time), NS2MS(finalize_time), n_finalized, inc_count, sweep_mask, -allocd_bytes/1024);
#endif
    }
    n_pause++;
    uint64_t pause = jl_hrtime() - t0;
    total_gc_time += pause;
#ifdef GC_FINAL_STATS
    max_pause = max_pause < pause ? pause : max_pause;
#endif
    jl_in_gc = 0;
    JL_SIGATOMIC_END();
#ifdef GC_TIME
    if (estimate_freed != SAVE2) {
        // this should not happen but it does
        // mostly because of gc_counted_* allocations
    }
#endif
    if (recollect) {
        n_pause--;
        jl_gc_collect(0);
    }
}

// allocator entry points

void *allocb(size_t sz)
{
    buff_t *b;
    size_t allocsz = sz + sizeof(buff_t);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
#ifdef MEMDEBUG
    b = (buff_t*)alloc_big(allocsz);
    b->header = 0x4EADE800;
    b->pooled = 0;
#else
    if (allocsz > GC_MAX_SZCLASS + sizeof(buff_t)) {
        b = (buff_t*)alloc_big(allocsz);
        b->header = 0x4EADE800;
        b->pooled = 0;
    }
    else {
        b = (buff_t*)pool_alloc(&pools[szclass(allocsz)]);
        b->header = 0x4EADE800;
        b->pooled = 1;
    }
#endif
    return &b->data[0];
}

/* this function is horribly broken in that it is unable to fix the bigval_t pointer chain after the realloc
 * so it is basically just completely invalid in the bigval_t case
void *reallocb(void *b, size_t sz)
{
    buff_t *buff = gc_val_buf(b);
    if (buff->pooled) {
        void* b2 = allocb(sz);
        memcpy(b2, b, page_metadata(buff)->osize);
        return b2;
    }
    else {
        size_t allocsz = LLT_ALIGN(sz + sizeof(bigval_t), 16);
        if (allocsz < sz)  // overflow in adding offs, size was "negative"
            jl_throw(jl_memory_exception);
        bigval_t *bv = bigval_header(buff);
        bv = (bigval_t*)realloc_a16(bv, allocsz, bv->sz&~3);
        if (bv == NULL)
            jl_throw(jl_memory_exception);
        return &bv->data[0];
    }
}
*/

DLLEXPORT jl_value_t *jl_gc_allocobj(size_t sz)
{
    size_t allocsz = sz + sizeof_jl_taggedvalue_t;
    if (allocsz < sz) // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
#ifdef MEMDEBUG
    return jl_valueof(alloc_big(allocsz));
#endif
    if (allocsz <= GC_MAX_SZCLASS + sizeof(buff_t))
        return jl_valueof(pool_alloc(&pools[szclass(allocsz)]));
    else
        return jl_valueof(alloc_big(allocsz));
}

DLLEXPORT jl_value_t *jl_gc_alloc_0w(void)
{
    const int sz = sizeof_jl_taggedvalue_t;
#ifdef MEMDEBUG
    return jl_valueof(alloc_big(sz));
#endif
    return jl_valueof(_pool_alloc(&pools[szclass(sz)], sz));
}

DLLEXPORT jl_value_t *jl_gc_alloc_1w(void)
{
    const int sz = LLT_ALIGN(sizeof_jl_taggedvalue_t + sizeof(void*), 16);
#ifdef MEMDEBUG
    return jl_valueof(alloc_big(sz));
#endif
    return jl_valueof(_pool_alloc(&pools[szclass(sz)], sz));
}

DLLEXPORT jl_value_t *jl_gc_alloc_2w(void)
{
    const int sz = LLT_ALIGN(sizeof_jl_taggedvalue_t + sizeof(void*) * 2, 16);
#ifdef MEMDEBUG
    return jl_valueof(alloc_big(sz));
#endif
    return jl_valueof(_pool_alloc(&pools[szclass(sz)], sz));
}

DLLEXPORT jl_value_t *jl_gc_alloc_3w(void)
{
    const int sz = LLT_ALIGN(sizeof_jl_taggedvalue_t + sizeof(void*) * 3, 16);
#ifdef MEMDEBUG
    return jl_valueof(alloc_big(sz));
#endif
    return jl_valueof(_pool_alloc(&pools[szclass(sz)], sz));
}

#ifdef GC_FINAL_STATS
static double process_t0;
#include <malloc.h>
void jl_print_gc_stats(JL_STREAM *s)
{
    double gct = total_gc_time/1e9;
    malloc_stats();
    double ptime = clock_now()-process_t0;
    jl_printf(s, "exec time\t%.5f sec\n", ptime);
    if (n_pause > 0) {
        jl_printf(s, "gc time  \t%.5f sec (%2.1f%%) in %d (%d full) collections\n",
                  NS_TO_S(total_gc_time), (NS_TO_S(total_gc_time)/ptime)*100, n_pause, n_full_sweep);
        jl_printf(s, "gc pause \t%.2f ms avg\n\t\t%2.0f ms max\n",
                  NS2MS(total_gc_time)/n_pause, NS2MS(max_pause));
        jl_printf(s, "\t\t(%2d%% mark, %2d%% sweep, %2d%% finalizers)\n",
                  (int)(total_mark_time * 100 / total_gc_time),
                  (int)(total_sweep_time * 100 / total_gc_time),
                  (int)(total_fin_time * 100 / total_gc_time));
    }
    int i = 0;
    while (i < REGION_COUNT && regions[i]) i++;
    jl_printf(s, "max allocated regions : %d\n", i);
    struct mallinfo mi = mallinfo();
    jl_printf(s, "malloc size\t%d MB\n", mi.uordblks/1024/1024);
    jl_printf(s, "max page alloc\t%ld MB\n", max_pg_count*GC_PAGE_SZ/1024/1024);
    jl_printf(s, "total freed\t%" PRIuPTR " b\n", jl_gc_total_freed_bytes);
    jl_printf(s, "free rate\t%.1f MB/sec\n", (jl_gc_total_freed_bytes/gct)/1024/1024);
}
#endif

// Per-thread initialization (when threading is fully implemented)
static void jl_mk_thread_heap(void) {
    FOR_CURRENT_HEAP
        const int* szc = sizeclasses;
        pool_t *p = HEAP(norm_pools);
        for(int i=0; i < N_POOLS; i++) {
            assert((szc[i] < 16 && szc[i] % sizeof(void*) == 0) ||
                   (szc[i] % 16 == 0));
            p[i].osize = szc[i];
            p[i].freelist = NULL;
            p[i].newpages = NULL;
            p[i].end_offset = GC_POOL_END_OFS(szc[i]);
        }
        arraylist_new(&preserved_values, 0);
        arraylist_new(&weak_refs, 0);
        mallocarrays = NULL;
        mafreelist = NULL;
        big_objects = NULL;
        arraylist_new(&rem_bindings, 0);
        remset = &HEAP(_remset)[0];
        last_remset = &HEAP(_remset)[1];
        arraylist_new(remset, 0);
        arraylist_new(last_remset, 0);
    END
}

// System-wide initializations
void jl_gc_init(void)
{
    gc_debug_init();
    jl_mk_thread_heap();

    arraylist_new(&finalizer_list, 0);
    arraylist_new(&finalizer_list_marked, 0);
    arraylist_new(&to_finalize, 0);

    collect_interval = default_collect_interval;
    last_long_collect_interval = default_collect_interval;
    allocd_bytes = -default_collect_interval;

#ifdef GC_VERIFY
    for(int i = 0; i < 4; i++)
        arraylist_new(&bits_save[i], 0);
    arraylist_new(&lostval_parents, 0);
    arraylist_new(&lostval_parents_done, 0);
#endif

#ifdef OBJPROFILE
    for(int g=0; g<3; g++) {
        htable_new(&obj_counts[g], 0);
        htable_new(&obj_sizes[g], 0);
    }
#endif
#ifdef GC_FINAL_STATS
    process_t0 = clock_now();
#endif

#ifdef _P64
    // on a big memory machine, set max_collect_interval to totalmem/ncores/2
    size_t maxmem = (uv_get_total_memory()/jl_cpu_cores())/2;
    if (maxmem > max_collect_interval)
        max_collect_interval = maxmem;
#endif
}

// GC summary stats

#if defined(MEMPROFILE)
// TODO repair this
static size_t pool_stats(pool_t *p, size_t *pwaste, size_t *np, size_t *pnold)
{
    gcval_t *v;
    gcpage_t *pg = p->pages;
    size_t osize = p->osize;
    size_t nused=0, nfree=0, npgs=0, nold = 0;

    while (pg != NULL) {
        npgs++;
        v = (gcval_t*)(pg->data + GC_PAGE_OFFSET);
        char *lim = (char*)v + GC_PAGE_SZ - GC_PAGE_OFFSET - osize;
        int i = 0;
        while ((char*)v <= lim) {
            if (!gc_marked(v)) {
                nfree++;
            }
            else {
                nused++;
                if (gc_bits(v) == GC_MARKED) {
                    nold++;
                }
            }
            v = (gcval_t*)((char*)v + osize);
            i++;
        }
        gcpage_t *nextpg = NULL;
        pg = nextpg;
    }
    *pwaste = npgs * GC_PAGE_SZ - (nused * p->osize);
    *np = npgs;
    *pnold = nold;
    if (npgs != 0) {
        jl_printf(JL_STDOUT,
                  "%4d : %7d/%7d objects (%3d%% old), %5d pages, %5d kB, %5d kB waste\n",
                  p->osize,
                  nused,
                  nused+nfree,
                  nused ? (nold*100)/nused : 0,
                  npgs,
                  (nused*p->osize)/1024,
                  *pwaste/1024);
    }
    return nused*p->osize;
}

static void all_pool_stats(void)
{
    int i;
    size_t nb=0, w, tw=0, no=0,tp=0, nold=0,noldbytes=0, b, np, nol;
    for(i=0; i < N_POOLS; i++) {
        b = pool_stats(&norm_pools[i], &w, &np, &nol);
        nb += b;
        no += (b/norm_pools[i].osize);
        tw += w;
        tp += np;
        nold += nol;
        noldbytes += nol*norm_pools[i].osize;
    }
    jl_printf(JL_STDOUT,
              "%d objects (%d%% old), %d kB (%d%% old) total allocated, %d total fragments (%d%% overhead), in %d pages\n",
              no, (nold*100)/no, nb/1024, (noldbytes*100)/nb, tw, (tw*100)/nb, tp);
}

static void big_obj_stats(void)
{
    bigval_t *v = big_objects;
    size_t nused=0, nbytes=0;
    while (v != NULL) {
        if (gc_marked(&v->_data)) {
            nused++;
            nbytes += v->sz&~3;
        }
        v = v->next;
    }
    v = big_objects_marked;
    size_t nused_old=0, nbytes_old=0;
    while (v != NULL) {
        if (gc_marked(&v->_data)) {
            nused_old++;
            nbytes_old += v->sz&~3;
        }
        v = v->next;
    }

    mallocarray_t *ma = mallocarrays;
    while (ma != NULL) {
        if (gc_marked(jl_astaggedvalue(ma->a))) {
            nused++;
            nbytes += array_nbytes(ma->a);
        }
        ma = ma->next;
    }

    jl_printf(JL_STDOUT, "%d kB (%d%% old) in %d large objects (%d%% old)\n", (nbytes + nbytes_old)/1024, nbytes + nbytes_old ? (nbytes_old*100)/(nbytes + nbytes_old) : 0, nused + nused_old, nused+nused_old ? (nused_old*100)/(nused + nused_old) : 0);
}
#endif //MEMPROFILE

DLLEXPORT void *jl_gc_counted_malloc(size_t sz)
{
    maybe_collect();
    allocd_bytes += sz;
    gc_num.malloc++;
    void *b = malloc(sz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    return b;
}

DLLEXPORT void *jl_gc_counted_calloc(size_t nm, size_t sz)
{
    maybe_collect();
    allocd_bytes += nm*sz;
    gc_num.malloc++;
    void *b = calloc(nm, sz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    return b;
}

DLLEXPORT void jl_gc_counted_free(void *p, size_t sz)
{
    free(p);
    freed_bytes += sz;
    gc_num.freecall++;
}

DLLEXPORT void *jl_gc_counted_realloc_with_old_size(void *p, size_t old, size_t sz)
{
    maybe_collect();

    if (sz < old)
       freed_bytes += (old - sz);
    else
       allocd_bytes += (sz - old);
    gc_num.realloc++;
    void *b = realloc(p, sz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    return b;
}

DLLEXPORT void *jl_malloc(size_t sz)
{
    int64_t *p = (int64_t *)jl_gc_counted_malloc(sz + 16);
    p[0] = sz;
    return (void *)(p + 2);
}

DLLEXPORT void *jl_calloc(size_t nm, size_t sz)
{
    int64_t *p;
    size_t nmsz = nm*sz;
    p = (int64_t *)jl_gc_counted_calloc(nmsz + 16, 1);
    p[0] = nmsz;
    return (void *)(p + 2);
}

DLLEXPORT void jl_free(void *p)
{
    int64_t *pp = (int64_t *)p - 2;
    size_t sz = pp[0];
    jl_gc_counted_free(pp, sz + 16);
}

DLLEXPORT void *jl_realloc(void *p, size_t sz)
{
    int64_t *pp = (int64_t *)p - 2;
    size_t szold = pp[0];
    int64_t *pnew = (int64_t *)jl_gc_counted_realloc_with_old_size(pp, szold + 16, sz + 16);
    pnew[0] = sz;
    return (void *)(pnew + 2);
}

DLLEXPORT void *jl_gc_managed_malloc(size_t sz)
{
    maybe_collect();
    size_t allocsz = LLT_ALIGN(sz, 16);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    allocd_bytes += allocsz;
    gc_num.malloc++;
    void *b = malloc_a16(allocsz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    return b;
}

DLLEXPORT void *jl_gc_managed_realloc(void *d, size_t sz, size_t oldsz, int isaligned, jl_value_t* owner)
{
    maybe_collect();

    size_t allocsz = LLT_ALIGN(sz, 16);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);

    if (gc_bits(jl_astaggedvalue(owner)) == GC_MARKED) {
        perm_scanned_bytes += allocsz - oldsz;
        live_bytes += allocsz - oldsz;
    }
    else if (allocsz < oldsz)
        freed_bytes += (oldsz - allocsz);
    else
        allocd_bytes += (allocsz - oldsz);
    gc_num.realloc++;

    void *b;
    if (isaligned)
        b = realloc_a16(d, allocsz, oldsz);
    else
        b = realloc(d, allocsz);
    if (b == NULL)
        jl_throw(jl_memory_exception);

    return b;
}

#ifdef __cplusplus
}
#endif
