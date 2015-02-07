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
#include <strings.h>
#include <assert.h>
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

#pragma pack(push, 1)

typedef struct {
    union {
        uintptr_t header;
        struct {
            uintptr_t gc_bits:2;
            uintptr_t pooled:1;
        };
    };
    // Work around a bug affecting gcc up to (at least) version 4.4.7
    // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=36839
    int _dummy[0];
    char data[];
} buff_t;

typedef struct _gcval_t {
    union {
        struct _gcval_t *next;
        uptrint_t flags;
        uptrint_t gc_bits:2;
    };
} gcval_t;

// layout for small (<2k) objects

#define GC_PAGE_LG2 14 // log2(size of a page)
#define GC_PAGE_SZ (1 << GC_PAGE_LG2) // 16k

// pool page metadata
typedef struct _gcpage_t {
    struct {
        uint16_t pool_n : 8;
        uint16_t allocd : 1; // true if an allocation happened in this page since last sweep
        uint16_t gc_bits : 2; // this is a bitwise | of all gc_bits in this page
    };
    uint16_t nfree;
    uint16_t osize;
    uint16_t fl_begin_offset;
    uint16_t fl_end_offset;
    char *data;
    char *ages;
} gcpage_t;

#define PAGE_PFL_BEG(p) ((gcval_t**)((p->data) + (p)->fl_begin_offset))
#define PAGE_PFL_END(p) ((gcval_t**)((p->data) + (p)->fl_end_offset))
// round an address inside a gcpage's data to its begining
#define GC_PAGE_DATA(x) ((char*)((uintptr_t)(x) >> GC_PAGE_LG2 << GC_PAGE_LG2))

// contiguous storage for up to REGION_PG_COUNT naturally aligned GC_PAGE_SZ blocks
// uses a very naive allocator (see malloc_page & free_page)
#ifdef _P64
#define REGION_PG_COUNT 16*8*4096 // 8G because virtual memory is cheap
#else
#define REGION_PG_COUNT 8*4096 // 512M
#endif
#define HEAP_COUNT 8

typedef struct {
    uint32_t freemap[REGION_PG_COUNT/32];
    char pages[REGION_PG_COUNT][GC_PAGE_SZ];
    gcpage_t meta[REGION_PG_COUNT];
} region_t;
static region_t *heaps[HEAP_COUNT] = {NULL};
// store a lower bound of the first free block in each region
static int heaps_lb[HEAP_COUNT] = {0};
// an upper bound of the last non-free block
static int heaps_ub[HEAP_COUNT] = {REGION_PG_COUNT/32-1};

typedef struct _pool_t {
    gcval_t *freelist;
    gcval_t *newpages;
    uint16_t end_offset; // avoid to compute this at each allocation
    uint16_t osize;
    uint16_t nfree;
} pool_t;

static region_t *find_region(void *ptr)
{
    // on 64bit systems we could probably use a single region and get rid of this loop
    for (int i = 0; i < HEAP_COUNT && heaps[i]; i++) {
        if ((char*)ptr >= (char*)heaps[i] && (char*)ptr <= (char*)heaps[i] + sizeof(region_t))
            return heaps[i];
    }
    return NULL;
}
gcpage_t *page_metadata(void *data)
{
    region_t *r = find_region(data);
    int pg_idx = (GC_PAGE_DATA(data) - &r->pages[0][0])/GC_PAGE_SZ;
    return &r->meta[pg_idx];
}

char *page_age(gcpage_t *pg)
{
    return pg->ages;
}

#define GC_POOL_END_OFS(osize) (((GC_PAGE_SZ/osize) - 1)*osize)


// layout for big (>2k) objects

typedef struct _bigval_t {
    struct _bigval_t *next;
    struct _bigval_t **prev; // pointer to the next field of the prev entry
    size_t sz;
    union {
        uptrint_t _pad0;
        uptrint_t age : 2;
    };
    // must be 16-aligned here, in 32 & 64b
    union {
        uptrint_t flags;
        uptrint_t gc_bits:2;
        char _data[1];
    };
} bigval_t;

#define BVOFFS (offsetof(bigval_t, _data)/sizeof(void*))
#define bigval_header(data) ((bigval_t*)((char*)(data) - BVOFFS*sizeof(void*)))

#pragma pack(pop)

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
static size_t collect_interval;
static int64_t allocd_bytes;

#define N_POOLS 42
static __attribute__((aligned (64))) pool_t norm_pools[N_POOLS];
#define pools norm_pools

static bigval_t *big_objects = NULL;
static bigval_t *big_objects_marked = NULL;

static int64_t total_allocd_bytes = 0;
static int64_t allocd_bytes_since_sweep = 0;
static int64_t freed_bytes = 0;
static uint64_t total_gc_time = 0;
#define NS_TO_S(t) ((double)(t/1000)/(1000*1000))
#define NS2MS(t) ((double)(t/1000)/1000)
static int64_t live_bytes = 0;
static int64_t promoted_bytes = 0;
static size_t current_pg_count = 0;
static size_t max_pg_count = 0;

int jl_in_gc; // referenced from switchto task.c

#ifdef OBJPROFILE
static htable_t obj_counts[3];
static htable_t obj_sizes[3];
#endif

#ifdef GC_FINAL_STATS
static size_t total_freed_bytes=0;
static uint64_t max_pause = 0;
static uint64_t total_sweep_time=0;
static uint64_t total_mark_time=0;
static uint64_t total_fin_time=0;
#endif
static int n_pause = 0;
static int n_full_sweep = 0;
int sweeping = 0;

// manipulating mark bits

#define GC_CLEAN 0 // freshly allocated
#define GC_MARKED 1 // reachable and old
#define GC_QUEUED 2 // if it is reachable it will be marked as old
#define GC_MARKED_NOESC (GC_MARKED | GC_QUEUED) // reachable and young

/*
  The state transition looks like :

 <-[quicksweep]--
 <-[sweep]---   |
            |   |
    ---> GC_QUEUED <---[sweep && age>promotion]--------
    |     |     ^                                     |
    |   [mark]  |                                     |
 [sweep]  |  [write barrier]                          |
    |     v     |                                     |
    ----- GC_MARKED <--------                         |
             |              |                         |
             --[quicksweep]--                         |
                                                      |          === above this line objects are old
 ----[new]------> GC_CLEAN ------[mark]--------> GC_MARKED_NOESC
                   | ^ ^                                | |
                   | | |                                | |
 <---[sweep]-------- | ------[sweep && age<=promotion]--- |
                     |                                    |
                     --[quicksweep && age<=promotion]------
 */

// A quick sweep is a sweep where sweep_mask == GC_MARKED_NOESC. It means we won't touch GC_MARKED objects.

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

#define gc_bits(o) (((gcval_t*)(o))->gc_bits)
#define gc_marked(o)  (((gcval_t*)(o))->gc_bits & GC_MARKED)
#define _gc_setmark(o, mark_mode) (((gcval_t*)(o))->gc_bits = mark_mode)

// mark verification
#ifdef GC_VERIFY
static jl_value_t* lostval = 0;
static arraylist_t lostval_parents;
static arraylist_t lostval_parents_done;
static int verifying;

static void add_lostval_parent(jl_value_t* parent)
{
    for(int i = 0; i < lostval_parents_done.len; i++) {
        if((jl_value_t*)lostval_parents_done.items[i] == parent)
            return;
    }
    for(int i = 0; i < lostval_parents.len; i++) {
        if((jl_value_t*)lostval_parents.items[i] == parent)
            return;
    }
    arraylist_push(&lostval_parents, parent);
}

#define verify_val(v) do {                                              \
        if(lostval == (jl_value_t*)(v) && (v) != 0) {                   \
            JL_PRINTF(JL_STDOUT,                                        \
                      "Found lostval 0x%lx at %s:%d oftype: ",          \
                      (uintptr_t)(lostval), __FILE__, __LINE__);        \
            jl_static_show(JL_STDOUT, jl_typeof(v));                    \
            JL_PRINTF(JL_STDOUT, "\n");                                 \
        }                                                               \
    } while(0);


#define verify_parent(ty, obj, slot, args...) do {                      \
        if(*(jl_value_t**)(slot) == lostval && (obj) != lostval) {      \
            JL_PRINTF(JL_STDOUT, "Found parent %s 0x%lx at %s:%d\n",    \
                      ty, (uintptr_t)(obj), __FILE__, __LINE__);        \
            JL_PRINTF(JL_STDOUT, "\tloc 0x%lx : ", (uintptr_t)(slot));  \
            JL_PRINTF(JL_STDOUT, args);                                 \
            JL_PRINTF(JL_STDOUT, "\n");                                 \
            JL_PRINTF(JL_STDOUT, "\ttype: ");                           \
            jl_static_show(JL_STDOUT, jl_typeof(obj));                  \
            JL_PRINTF(JL_STDOUT, "\n");                                 \
            add_lostval_parent((jl_value_t*)(obj));                     \
        }                                                               \
    } while(0);

#else
#define verify_val(v)
#define verify_parent(ty,obj,slot,args...)
#endif

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

static inline void gc_setmark_other(void *o, int mark_mode)
{
    _gc_setmark(o, mark_mode);
    verify_val(o);
}

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
            perm_scanned_bytes += hdr->sz;
        else
            scanned_bytes += hdr->sz;
#ifdef OBJPROFILE
        objprofile_count(jl_typeof(o), mark_mode == GC_MARKED, hdr->sz);
#endif
    }
    _gc_setmark(o, mark_mode);
    verify_val(o);
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
    verify_val(o);
    return mark_mode;
}


static inline int gc_setmark(void *o, int sz, int mark_mode)
{
#ifdef MEMDEBUG
    return gc_setmark_big(o, mark_mode);
#endif
    if (sz <= 2048)
        return gc_setmark_pool(o, mark_mode);
    else
        return gc_setmark_big(o, mark_mode);
}

#define gc_typeof(v) ((jl_value_t*)(((uptrint_t)jl_typeof(v))&(~(uintptr_t)3)))
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

// malloc wrappers, aligned allocation

#ifdef _P64
#define malloc_a16(sz) malloc(((sz)+15)&-16)
#define free_a16(p) free(p)

#elif defined(_OS_WINDOWS_) /* 32-bit OS is implicit here. */
#define malloc_a16(sz) _aligned_malloc(sz?((sz)+15)&-16:1, 16)
#define free_a16(p) _aligned_free(p)

#elif defined(__APPLE__)
#define malloc_a16(sz) malloc(((sz)+15)&-16)
#define free_a16(p) free(p)

#else
static inline void *malloc_a16(size_t sz)
{
    void *ptr;
    if (posix_memalign(&ptr, 16, (sz+15)&-16))
        return NULL;
    return ptr;
}
#define free_a16(p) free(p)

#endif

static __attribute__((noinline)) void *malloc_page(void)
{
    void *ptr = (void*)0;
    int i;
    region_t* heap;
    int heap_i = 0;
    while(heap_i < HEAP_COUNT) {
        heap = heaps[heap_i];
        if (heap == NULL) {
#ifdef _OS_WINDOWS_
            char* mem = VirtualAlloc(NULL, sizeof(region_t) + GC_PAGE_SZ, MEM_RESERVE, PAGE_READWRITE);
#else
            char* mem = mmap(0, sizeof(region_t) + GC_PAGE_SZ, PROT_READ | PROT_WRITE, MAP_NORESERVE | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
            mem = mem == MAP_FAILED ? NULL : mem;
#endif
            if (mem == NULL) {
                jl_printf(JL_STDERR, "could not allocate pools\n");
                abort();
            }
            heap = (region_t*)((char*)GC_PAGE_DATA(mem + REGION_PG_COUNT/8 + GC_PAGE_SZ - 1) - REGION_PG_COUNT/8);
            heaps[heap_i] = heap;
#ifdef _OS_WINDOWS_
            VirtualAlloc(heap->freemap, REGION_PG_COUNT/8, MEM_COMMIT, PAGE_READWRITE);
            VirtualAlloc(heap->meta, REGION_PG_COUNT*sizeof(gcpage_t), MEM_COMMIT, PAGE_READWRITE);
#endif
            memset(heap->freemap, 0xff, REGION_PG_COUNT/8);
        }
        for(i = heaps_lb[heap_i]; i < REGION_PG_COUNT/32; i++) {
            if (heap->freemap[i]) break;
        }
        if (i == REGION_PG_COUNT/32) {
            // heap full
            heap_i++;
            continue;
        }
        break;
    }
    if (heap_i >= HEAP_COUNT) {
        jl_printf(JL_STDERR, "increase HEAP_COUNT or allocate less memory\n");
        abort();
    }
    if (heaps_lb[heap_i] < i)
        heaps_lb[heap_i] = i;
    if (heaps_ub[heap_i] < i)
        heaps_ub[heap_i] = i;

#ifdef __MINGW32__
    int j = __builtin_ffs(heap->freemap[i]) - 1;
#elif _MSC_VER
    int j;
    _BitScanForward(&j, heap->freemap[i]);
#else
    int j = ffs(heap->freemap[i]) - 1;
#endif

    heap->freemap[i] &= ~(uint32_t)(1 << j);
    ptr = heap->pages[i*32 + j];
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
    for(i = 0; i < HEAP_COUNT && heaps[i] != NULL; i++) {
        pg_idx = ((char*)p - (char*)&heaps[i]->pages[0])/GC_PAGE_SZ;
        if (pg_idx >= 0 && pg_idx < REGION_PG_COUNT) break;
    }
    assert(i < HEAP_COUNT && heaps[i] != NULL);
    region_t *heap = heaps[i];
    uint32_t msk = (uint32_t)(1 << ((pg_idx % 32)));
    assert(!(heap->freemap[pg_idx/32] & msk));
    heap->freemap[pg_idx/32] ^= msk;
    free(heap->meta[pg_idx].ages);
#ifdef _OS_WINDOWS_
    VirtualFree(p, GC_PAGE_SZ, MEM_DECOMMIT);
#else
    madvise(p, GC_PAGE_SZ, MADV_DONTNEED);
#endif
    if (heaps_lb[i] > pg_idx/32) heaps_lb[i] = pg_idx/32;
    current_pg_count--;
}

#define should_collect() (__unlikely(allocd_bytes>0))
static inline int maybe_collect(void)
{
    if (should_collect()) {
        jl_gc_collect(0);
        return 1;
    }
    return 0;
}

DLLEXPORT void *jl_gc_counted_malloc(size_t sz)
{
    maybe_collect();
    allocd_bytes += sz;
    void *b = malloc(sz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    return b;
}

DLLEXPORT void jl_gc_counted_free(void *p, size_t sz)
{
    free(p);
    freed_bytes += sz;
}

DLLEXPORT void *jl_gc_counted_realloc_with_old_size(void *p, size_t old, size_t sz)
{
    maybe_collect();
    allocd_bytes += (sz-old);
    void *b = realloc(p, sz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    return b;
}

void *jl_gc_managed_malloc(size_t sz)
{
    maybe_collect();
    allocd_bytes += sz;
    sz = (sz+15) & -16;
    void *b = malloc_a16(sz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    return b;
}

void *jl_gc_managed_realloc(void *d, size_t sz, size_t oldsz, int isaligned, jl_value_t* owner)
{
    maybe_collect();

    if (gc_bits(owner) == GC_MARKED) {
        perm_scanned_bytes += sz - oldsz;
        live_bytes += sz - oldsz;
    }
    else {
        allocd_bytes += sz - oldsz;
    }

    sz = (sz+15) & -16;
    void *b;
#ifdef _P64
    b = realloc(d, sz);
#elif defined(_OS_WINDOWS_)
    if (isaligned)
        b = _aligned_realloc(d, sz, 16);
    else
        b = realloc(d, sz);
#elif defined(__APPLE__)
    b = realloc(d, sz);
#else
    // TODO better aligned realloc here
    b = malloc_a16(sz);
    if (b != NULL) {
        memcpy(b, d, oldsz);
        if (isaligned) free_a16(d); else free(d);
    }
#endif
    if (b == NULL)
        jl_throw(jl_memory_exception);

    return b;
}

// preserved values

static arraylist_t preserved_values;

DLLEXPORT int jl_gc_n_preserved_values(void)
{
    return preserved_values.len;
}

DLLEXPORT void jl_gc_preserve(jl_value_t *v)
{
    arraylist_push(&preserved_values, (void*)v);
}

DLLEXPORT void jl_gc_unpreserve(void)
{
    (void)arraylist_pop(&preserved_values);
}

// weak references

static arraylist_t weak_refs;

DLLEXPORT jl_weakref_t *jl_gc_new_weakref(jl_value_t *value)
{
    jl_weakref_t *wr = (jl_weakref_t*)alloc_2w();
    wr->type = (jl_value_t*)jl_weakref_type;
    wr->value = value;
    arraylist_push(&weak_refs, wr);
    return wr;
}

static void sweep_weak_refs(void)
{
    size_t n=0, ndel=0, l=weak_refs.len;
    jl_weakref_t *wr;
    void **lst = weak_refs.items;
    void *tmp;
#define SWAP_wr(a,b) (tmp=a,a=b,b=tmp,1)
    if (l == 0)
        return;
    do {
        wr = (jl_weakref_t*)lst[n];
        if (gc_marked(wr)) {
            // weakref itself is alive
            if (!gc_marked(wr->value))
                wr->value = (jl_value_t*)jl_nothing;
            n++;
        }
        else {
            ndel++;
        }
    } while ((n < l-ndel) && SWAP_wr(lst[n],lst[n+ndel]));

    weak_refs.len -= ndel;
}

// finalization
static arraylist_t finalizer_list;
static arraylist_t finalizer_list_marked;
static arraylist_t to_finalize;

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
        JL_PRINTF(JL_STDERR, "error in running finalizer: ");
        jl_static_show(JL_STDERR, jl_exception_in_transit);
        JL_PUTC('\n',JL_STDERR);
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
        int ok = 1;run_finalizer((jl_value_t*)o, (jl_value_t*)f);
        assert(ok); (void)ok;
    }
    JL_GC_POP();
}

void schedule_all_finalizers(arraylist_t* flist)
{
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

void jl_gc_add_finalizer(jl_value_t *v, jl_function_t *f)
{
    arraylist_push(&finalizer_list, (void*)v);
    arraylist_push(&finalizer_list, (void*)f);
}

void jl_finalize(jl_value_t *o)
{
    (void)finalize_object(o);
}

// big value list

static __attribute__((noinline)) void *alloc_big(size_t sz)
{
    maybe_collect();
    size_t offs = BVOFFS*sizeof(void*);
    if (sz+offs+15 < offs+15)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    size_t allocsz = (sz+offs+15) & -16;
    bigval_t *v = (bigval_t*)malloc_a16(allocsz);
    allocd_bytes += allocsz;
    if (v == NULL)
        jl_throw(jl_memory_exception);
#ifdef MEMDEBUG
    memset(v, 0xee, allocsz);
#endif
    v->sz = allocsz;
    v->flags = 0;
    v->age = 0;
    v->next = big_objects;
    v->prev = &big_objects;
    if (v->next)
        v->next->prev = &v->next;
    big_objects = v;
    void* ptr = &v->_data[0];
    return ptr;
}

static int big_total;
static int big_freed;
static int big_reset;

static bigval_t** sweep_big_list(int sweep_mask, bigval_t** pv)
{
    bigval_t *v = *pv;
    while (v != NULL) {
        bigval_t *nxt = v->next;
        if (gc_marked(&v->_data)) {
            pv = &v->next;
            int age = v->age;
            int bits = gc_bits(&v->_data);
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
            gc_bits(&v->_data) = bits;
        }
        else {
            *pv = nxt;
            if (nxt)
                nxt->prev = pv;
            freed_bytes += v->sz;
#ifdef MEMDEBUG
            memset(v, 0xbb, v->sz);
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
    sweep_big_list(sweep_mask, &big_objects);
    if (sweep_mask == GC_MARKED) {
        bigval_t** last_next = sweep_big_list(sweep_mask, &big_objects_marked);
        if (big_objects)
            big_objects->prev = last_next;
        *last_next = big_objects;
        big_objects = big_objects_marked;
        if (big_objects)
            big_objects->prev = &big_objects;
        big_objects_marked = NULL;
    }
}

// tracking Arrays with malloc'd storage

typedef struct _mallocarray_t {
    jl_array_t *a;
    struct _mallocarray_t *next;
} mallocarray_t;

static mallocarray_t *mallocarrays = NULL;
static mallocarray_t *mafreelist = NULL;

void jl_gc_track_malloced_array(jl_array_t *a)
{
    mallocarray_t *ma;
    if (mafreelist == NULL) {
        ma = (mallocarray_t*)malloc(sizeof(mallocarray_t));
    }
    else {
        ma = mafreelist;
        mafreelist = mafreelist->next;
    }
    ma->a = a;
    ma->next = mallocarrays;
    mallocarrays = ma;
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
    mallocarray_t *ma = mallocarrays;
    mallocarray_t **pma = &mallocarrays;
    while (ma != NULL) {
        mallocarray_t *nxt = ma->next;
        if (gc_marked(ma->a)) {
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
}

// pool allocation
static inline gcval_t *reset_page(pool_t *p, gcpage_t *pg, gcval_t *fl)
{
    pg->gc_bits = 0;
    pg->nfree = GC_PAGE_SZ/p->osize;
    pg->pool_n = p - norm_pools;
    memset(page_age(pg), 0, (GC_PAGE_SZ/p->osize + 7)/8);
    gcval_t *beg = (gcval_t*)pg->data;
    gcval_t *end = (gcval_t*)((char*)beg + (pg->nfree - 1)*p->osize);
    end->next = fl;
    pg->allocd = 0;
    pg->fl_begin_offset = 0;
    pg->fl_end_offset = (char*)end - (char*)beg;
    return beg;
}

static __attribute__((noinline)) void  add_page(pool_t *p)
{
    char *data = malloc_page();
    if (data == NULL)
        jl_throw(jl_memory_exception);
    gcpage_t *pg = page_metadata(data);
    pg->data = data;
    pg->osize = p->osize;
    pg->ages = malloc((GC_PAGE_SZ/p->osize + 7)/8);
    gcval_t *fl = reset_page(p, pg, p->newpages);
    p->newpages = fl;
}

static inline  void *__pool_alloc(pool_t* p, int osize, int end_offset)
{
    gcval_t *v, *end;
    if (__unlikely((allocd_bytes += osize) >= 0)) {
        //allocd_bytes -= osize;
        jl_gc_collect(0);
        //allocd_bytes += osize;
    }
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
    } else {
        // like in the freelist case, only update the page metadata when it is full
        gcpage_t* pg = page_metadata(v);
        pg->nfree = 0;
        pg->allocd = 1;
        p->newpages = v->next;
    }
    v->flags = 0;
    return v;
}

// use this variant when osize is statically known
// GC_POOL_END_OFS uses an integer division
static inline void *_pool_alloc(pool_t *p, int osize)
{
    return __pool_alloc(p, osize, GC_POOL_END_OFS(osize));
}

static inline void *pool_alloc(pool_t *p)
{
    return __pool_alloc(p, p->osize, p->end_offset);
}

static const int sizeclasses[N_POOLS] = {
    8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56,
    64, 72, 80, 88, 96, //#=18

    112, 128, 144, 160, 176, 192, 208, 224, 240, 256,

    288, 320, 352, 384, 416, 448, 480, 512,

    640, 768, 896, 1024,

    1536, 2048 };

static int szclass(size_t sz)
{
#ifndef _P64
    if     (sz <=    8) return 0;
#endif
    if     (sz <=   56) return ((sz+3)/4) - 2;
    if     (sz <=   96) return ((sz+7)/8) + 5;
    if     (sz <=  512) {
        if (sz <=  256) return ((sz+15)-112)/16 + 18;
        else            return ((sz+31)-288)/32 + 28;
    }
    if     (sz <= 1024) return ((sz+127)-640)/128 + 36;
    if     (sz <= 1536) return 40;
    return 41;
}

int check_timeout = 0;
#define should_timeout() 0

// sweep phase

static int skipped_pages = 0;
static int total_pages = 0;
static int freed_pages = 0;
static int lazy_freed_pages = 0;
static int page_done = 0;
static gcval_t** sweep_page(pool_t* p, gcpage_t* pg, gcval_t **pfl,int,int);
static void sweep_pool_region(int heap_i, int sweep_mask)
{
    region_t* heap = heaps[heap_i];
    gcval_t **pfl[N_POOLS];

    // update metadata of pages that were pointed to by freelist or newpages from a pool
    // i.e. pages being the current allocation target
    for (int i = 0; i < N_POOLS; i++) {
        gcval_t* last = norm_pools[i].freelist;
        if (last) {
            gcpage_t* pg = page_metadata(last);
            pg->allocd = 1;
            pg->nfree = norm_pools[i].nfree;
        }
        norm_pools[i].freelist =  NULL;
        pfl[i] = &norm_pools[i].freelist;

        last = norm_pools[i].newpages;
        if (last) {
            gcpage_t* pg = page_metadata(last);
            pg->nfree = (GC_PAGE_SZ - ((char*)last - GC_PAGE_DATA(last)))/norm_pools[i].osize;
            pg->allocd = 1;
        }
        norm_pools[i].newpages = NULL;
    }

    // the actual sweeping
    int ub = 0;
    int lb = heaps_lb[heap_i];
    for (int pg_i = 0; pg_i <= heaps_ub[heap_i]; pg_i++) {
        uint32_t line = heap->freemap[pg_i];
        if (!!~line) {
            ub = pg_i;
            for (int j = 0; j < 32; j++) {
                if (!((line >> j) & 1)) {
                    gcpage_t *pg = &heap->meta[pg_i*32 + j];
                    int p_n = pg->pool_n;
                    pool_t *p = &norm_pools[p_n];
                    int osize = pg->osize;
                    pfl[p_n] = sweep_page(p, pg, pfl[p_n], sweep_mask, osize);
                }
            }
        } else if (pg_i < lb) lb = pg_i;
    }
    heaps_ub[heap_i] = ub;
    heaps_lb[heap_i] = lb;

    // cache back pg->nfree in the pool_t
    int i = 0;
    for (pool_t* p = norm_pools; p < norm_pools + N_POOLS; p++) {
        *pfl[i++] = NULL;
        if (p->freelist) {
            p->nfree = page_metadata(p->freelist)->nfree;
        }
    }
}

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
    int obj_per_page = GC_PAGE_SZ/osize;
    char *data = pg->data;
    char *ages = page_age(pg);
    v = (gcval_t*)data;
    char *lim = (char*)v + GC_PAGE_SZ - osize;
    freedall = 1;
    old_nfree += pg->nfree;

    if (pg->gc_bits == GC_MARKED) {
        // this page only contains GC_MARKED and free cells
        // if we are doing a quick sweep and nothing has been allocated inside since last sweep
        // we can skip it
        if (sweep_mask == GC_MARKED_NOESC && !pg->allocd) {
            // the position of the freelist begin/end in this page is stored in it's metadata
            if (pg->fl_begin_offset != (uint16_t)-1) {
                *pfl = (gcval_t*)PAGE_PFL_BEG(pg);
                pfl = prev_pfl = PAGE_PFL_END(pg);
            }
            pg_skpd++;
            freedall = 0;
            goto free_page;
        }
    }
    else if(pg->gc_bits == GC_CLEAN) {
        goto free_page;
    }

    int pg_nfree = 0;
    gcval_t **pfl_begin = NULL;
    unsigned char msk = 1; // mask for the age bit in the current age byte
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
        msk *= 2;
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

extern void jl_unmark_symbols(void);

static void gc_sweep_once(int sweep_mask)
{
#ifdef GC_TIME
    double t0 = clock_now();
    mallocd_array_total = 0;
    mallocd_array_freed = 0;
#endif
    sweep_malloced_arrays();
#ifdef GC_TIME
    JL_PRINTF(JL_STDOUT, "GC sweep arrays %.2f (freed %d/%d)\n", (clock_now() - t0)*1000, mallocd_array_freed, mallocd_array_total);
    t0 = clock_now();
    big_total = 0;
    big_freed = 0;
    big_reset = 0;
#endif
    sweep_big(sweep_mask);
#ifdef GC_TIME
    JL_PRINTF(JL_STDOUT, "GC sweep big %.2f (freed %d/%d with %d rst)\n", (clock_now() - t0)*1000, big_freed, big_total, big_reset);
    t0 = clock_now();
#endif
    if (sweep_mask == GC_MARKED)
        jl_unmark_symbols();
#ifdef GC_TIME
    JL_PRINTF(JL_STDOUT, "GC sweep symbols %.2f\n", (clock_now() - t0)*1000);
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

    for (int i = 0; i < HEAP_COUNT; i++) {
        if (heaps[i])
            /*finished &= */sweep_pool_region(i, sweep_mask);
    }

#ifdef GC_TIME
    double sweep_pool_sec = clock_now() - t0;
    double sweep_speed = ((((double)total_pages)*GC_PAGE_SZ)/(1024*1024*1024))/sweep_pool_sec;
    JL_PRINTF(JL_STDOUT, "GC sweep pools %s %.2f at %.1f GB/s (skipped %d%% of %d, done %d pgs, %d freed with %d lazily) mask %d\n", finished ? "end" : "inc", sweep_pool_sec*1000, sweep_speed, total_pages ? (skipped_pages*100)/total_pages : 0, total_pages, page_done, freed_pages, lazy_freed_pages,  sweep_mask);
#endif
    return finished;
}

// mark phase

jl_value_t **mark_stack = NULL;
jl_value_t **mark_stack_base = NULL;
size_t mark_stack_size = 0;
size_t mark_sp = 0;
size_t perm_marked = 0;


void grow_mark_stack(void)
{
    size_t newsz = mark_stack_size>0 ? mark_stack_size*2 : 32000;
    size_t offset = mark_stack - mark_stack_base;
    mark_stack_base = (jl_value_t**)realloc(mark_stack_base, newsz*sizeof(void*));
    if (mark_stack_base == NULL) {
        JL_PRINTF(JL_STDERR, "Could'nt grow mark stack to : %d\n", newsz);
        exit(1);
    }
    mark_stack = mark_stack_base + offset;
    mark_stack_size = newsz;
}

int max_msp = 0;

static arraylist_t tasks;
static arraylist_t rem_bindings;
static arraylist_t _remset[2];
static arraylist_t *remset = &_remset[0];
static arraylist_t *last_remset = &_remset[1];
void reset_remset(void)
{
    arraylist_t *tmp = remset;
    remset = last_remset;
    last_remset = tmp;
    remset->len = 0;
}

DLLEXPORT void gc_queue_root(void *ptr)
{
    assert(gc_bits(ptr) != GC_QUEUED);
    gc_bits(ptr) = GC_QUEUED;
    arraylist_push(remset, ptr);
}
void gc_queue_binding(void *bnd)
{
    assert(gc_bits(bnd) != GC_QUEUED);
    gc_bits(bnd) = GC_QUEUED;
    arraylist_push(&rem_bindings, (void*)((void**)bnd + 1));
}

static int push_root(jl_value_t *v, int d, int);
static inline int gc_push_root(void *v, int d)
{
    assert((v) != NULL);
    verify_val(v);
    int bits = gc_bits(v);
    if (!gc_marked(v)) {
        return push_root((jl_value_t*)(v),d, bits);
    }
    return bits;
}

void jl_gc_setmark(jl_value_t *v) // TODO rename this as it is misleading now
{
    //    int64_t s = perm_scanned_bytes;
    if (!gc_marked(v)) {
        //        objprofile_count(jl_typeof(v), 1, 16);
#ifdef MEMDEBUG
        gc_setmark_big(v, GC_MARKED_NOESC);
#else
        gc_setmark_pool(v, GC_MARKED_NOESC);
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
                    verify_parent("task", ta, &rts[i], "stack(%d)", i);
                    gc_push_root(rts[i], d);
                }
            }
        }
        s = s->prev;
    }
}

__attribute__((noinline)) static int gc_mark_module(jl_module_t *m, int d)
{
    size_t i;
    int refyoung = 0;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            gc_setmark_buf(b, gc_bits(m));
#ifdef GC_VERIFY
            void* vb = gc_val_buf(b);
            verify_parent("module", m, &vb, "binding_buff");
#endif
            if (b->value != NULL) {
                verify_parent("module", m, &b->value, "binding(%s)", b->name->name);
                refyoung |= gc_push_root(b->value, d);
            }
            if (b->type != (jl_value_t*)jl_any_type) {
                refyoung |= gc_push_root(b->type, d);
            }
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
        verify_parent("module", m, &m->constant_table, "constant_table");
        refyoung |= gc_push_root(m->constant_table, d);
    }
    return refyoung;
}

static void gc_mark_task_stack(jl_task_t *ta, int d)
{
    if (ta->stkbuf != NULL || ta == jl_current_task) {
        if (ta->stkbuf != NULL) {
            gc_setmark_buf(ta->stkbuf, gc_bits(ta));
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

#if 0
static void mark_task_stacks(void) {
    for (int i = 0; i < tasks.len; i++) {
        gc_mark_task_stack(tasks.items[i], 0);
    }
}
#endif

__attribute__((noinline)) static void gc_mark_task(jl_task_t *ta, int d)
{
    if (ta->parent) gc_push_root(ta->parent, d);
    if (ta->last) gc_push_root(ta->last, d);
    gc_push_root(ta->tls, d);
    gc_push_root(ta->consumers, d);
    gc_push_root(ta->donenotify, d);
    gc_push_root(ta->exception, d);
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
    int refyoung = 0;

    if (vt == (jl_value_t*)jl_weakref_type) {
        bits = gc_setmark(v, jl_datatype_size(jl_weakref_type), GC_MARKED_NOESC);
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
    if (vt == (jl_value_t*)jl_tuple_type) {
        size_t l = jl_tuple_len(v);
        MARK(v, bits = gc_setmark(v, l*sizeof(void*) + sizeof(jl_tuple_t), GC_MARKED_NOESC));
        jl_value_t **data = ((jl_tuple_t*)v)->data;
        for(size_t i=0; i < l; i++) {
            jl_value_t *elt = data[i];
            if (elt != NULL) {
                verify_parent("tuple", v, &data[i], "elem(%d)", i);
                refyoung |= gc_push_root(elt, d);
            }
        }
    }
    else if (((jl_datatype_t*)(vt))->name == jl_array_typename) {
        jl_array_t *a = (jl_array_t*)v;
        int todo = !(bits & GC_MARKED);
        if (a->pooled)
            MARK(a,
#ifdef MEMDEBUG
                 bits = gc_setmark_big(a, GC_MARKED_NOESC);
#else
                 bits = gc_setmark_pool(a, GC_MARKED_NOESC);
#endif
                 if (a->how == 2 && todo) {
                     objprofile_count(MATY, gc_bits(a) == GC_MARKED, array_nbytes(a));
                     if (gc_bits(a) == GC_MARKED)
                         perm_scanned_bytes += array_nbytes(a);
                     else
                         scanned_bytes += array_nbytes(a);
                 });
        else
            MARK(a,
                 bits = gc_setmark_big(a, GC_MARKED_NOESC);
                 if (a->how == 2 && todo) {
                     objprofile_count(MATY, gc_bits(a) == GC_MARKED, array_nbytes(a));
                     if (gc_bits(a) == GC_MARKED)
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
            verify_parent("array", v, &val_buf, "buffer ('loc' addr is meaningless)");
#endif
            gc_setmark_buf((char*)a->data - a->offset*a->elsize, gc_bits(v));
        }
        if (a->ptrarray && a->data!=NULL) {
            size_t l = jl_array_len(a);
            if (l > 100000 && d > MAX_MARK_DEPTH-10) {
                // don't mark long arrays at high depth, to try to avoid
                // copying the whole array into the mark queue
                goto queue_the_root;
            }
            else {
                void *data = a->data;
                for(size_t i=0; i < l; i++) {
                    jl_value_t *elt = ((jl_value_t**)data)[i];
                    if (elt != NULL) {
                        verify_parent("array", v, &((jl_value_t**)data)[i], "elem(%d)", i);
                        refyoung |= gc_push_root(elt, d);
                    }
                    // try to split large array marking (incremental mark TODO)
                    // if (should_timeout() && l > 1000) goto queue_the_root;
                }
            }
        }
    }
    else if (vt == (jl_value_t*)jl_module_type) {
        MARK(v, bits = gc_setmark(v, sizeof(jl_module_t), GC_MARKED_NOESC));
        refyoung |= gc_mark_module((jl_module_t*)v, d);
    }
    else if (vt == (jl_value_t*)jl_task_type) {
        MARK(v, bits = gc_setmark(v, sizeof(jl_task_t), GC_MARKED_NOESC));
        gc_mark_task((jl_task_t*)v, d);
        // tasks should always be remarked since we do not trigger the write barrier
        // for stores to stack slots
        refyoung = GC_MARKED_NOESC;
    }
    else if(vt == (jl_value_t*)jl_symbol_type) {
        gc_setmark_other(v, GC_MARKED); // symbols have their own allocator
    }
    else if(
#ifdef GC_VERIFY
            // this check should not be needed but it helps catching corruptions early
            gc_typeof(vt) == (jl_value_t*)jl_datatype_type
#else
            1
#endif
            ) {
        jl_datatype_t *dt = (jl_datatype_t*)vt;
        MARK(v, bits = gc_setmark(v, jl_datatype_size(dt), GC_MARKED_NOESC));
        int nf = (int)jl_tuple_len(dt->names);
        // TODO check if there is a perf improvement for objects with a lot of fields
        // int fdsz = sizeof(void*)*nf;
        // void** children = alloca(fdsz);
        // int ci = 0;
        jl_fielddesc_t* fields = dt->fields;
        for(int i=0; i < nf; i++) {
            if (fields[i].isptr) {
                jl_value_t **slot = (jl_value_t**)((char*)v + fields[i].offset + sizeof(void*));
                jl_value_t *fld = *slot;
                if (fld) {
                    verify_parent("object", v, slot, "field(%d)", i);
                    //children[ci++] = fld;
                    refyoung |= gc_push_root(fld, d);
                }
            }
        }
        //while(ci)
        //  refyoung |= gc_push_root(children[--ci], d);
    }
#ifdef GC_VERIFY
    else {
        JL_PRINTF(JL_STDOUT, "GC error (probable corruption) :\n");
        jl_(vt);
        abort();
    }
#endif

 ret:
#ifdef GC_VERIFY
    if (verifying) return bits;
#endif
    if ((bits == GC_MARKED) && (refyoung == GC_MARKED_NOESC)) {
        // v is an old object referencing young objects
        arraylist_push(remset, v);
    }
    return bits;

#undef MARK

 queue_the_root:
    if(mark_sp >= mark_stack_size) grow_mark_stack();
    mark_stack[mark_sp++] = (jl_value_t*)v;
    max_msp = max_msp > mark_sp ? max_msp : mark_sp;
    return bits;
}

static void visit_mark_stack_inc(int mark_mode)
{
    while(mark_sp > 0 && !should_timeout()) {
        jl_value_t* v = mark_stack[--mark_sp];
        assert(gc_bits(v) == GC_QUEUED || gc_bits(v) == GC_MARKED || gc_bits(v) == GC_MARKED_NOESC);
        push_root(v, 0, gc_bits(v));
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

extern jl_value_t * volatile jl_task_arg_in_transit;
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
    for(i=0; i < preserved_values.len; i++) {
        gc_push_root((jl_value_t*)preserved_values.items[i], 0);
    }

    // objects currently being finalized
    for(i=0; i < to_finalize.len; i++) {
        gc_push_root(to_finalize.items[i], 0);
    }

    jl_mark_box_caches();
    gc_push_root(jl_unprotect_stack_func, 0);
    gc_push_root(jl_bottom_func, 0);
    gc_push_root(jl_typetype_type, 0);
    gc_push_root(jl_tupletype_type, 0);

    // constants
    gc_push_root(jl_null, 0);
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
        int isfreed = !gc_marked(v);
        gc_push_root(fin, 0);
        int isold = list == &finalizer_list && gc_bits(v) == GC_MARKED && gc_bits(fin) == GC_MARKED;
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

/*
 How to debug a missing write barrier :
 (or rather how I do it, if you know of a better way update this)
 First, reproduce it with GC_VERIFY. It does change the allocation profile so if the error
 is rare enough this may not be straightforward. If the backtracking goes well you should know
 which object and which of its slots was written to without being caught by the write
 barrier. Most times this allows you to take a guess. If this type of object is modified
 by C code directly, look for missing gc_wb() on pointer updates. Be aware that there are
 innocent looking functions which allocate (and thus trigger marking) only on special cases.

 If you cant find it, you can try the following :
 - Ensure that should_timeout() is deterministic instead of clock based.
 - Once you have a completly deterministic program which crashes on gc_verify, the addresses
   should stay constant between different runs (with same binary, same environment ...).
   Do not forget to turn off ASLR (linux: echo 0 > /proc/sys/kernel/randomize_va_space).
   At this point you should be able to run under gdb and use a hw watch to look for writes
   at the exact addr of the slot (use something like watch *slot_addr if *slot_addr == val).
 - If it went well you are now stopped at the exact point the problem is happening.
   Backtraces in JIT'd code wont work for me (but I'm not sure they should) so in that
   case you can try to jl_throw(something) from gdb.
 */
// this does not yet detect missing writes from marked to marked_noesc
// the error is caught at the first long collection
#ifdef GC_VERIFY
static arraylist_t bits_save[4];

// set all mark bits to bits
// record the state of the heap and can replay it in restore()
// restore _must_ be called as this will overwrite parts of the
// freelist in pools
static void clear_mark(int bits)
{
    size_t i;
    pool_t* pool;
    gcval_t* pv;
    if (!verifying) {
    for(int i = 0; i < 4; i++)
        bits_save[i].len = 0;
    }

    bigval_t *bigs[] = { big_objects, big_objects_marked };
    for (int i = 0; i < 2; i++) {
        bigval_t *v = bigs[i];
        while (v != NULL) {
            void* gcv = &v->_data;
            if (!verifying) arraylist_push(&bits_save[gc_bits(gcv)], gcv);
            gc_bits(gcv) = bits;
            v = v->next;
        }
    }
    for (int h = 0; h < HEAP_COUNT; h++) {
        region_t* heap = heaps[h];
        if (!heap) break;
        for (int pg_i = 0; pg_i < REGION_PG_COUNT/32; pg_i++) {
            uint32_t line = heap->freemap[pg_i];
            if (!!~line) {
                for (int j = 0; j < 32; j++) {
                    if (!((line >> j) & 1)) {
                        gcpage_t *pg = page_metadata(heap->pages[pg_i*32 + j]);
                        pool_t *pool = &norm_pools[pg->pool_n];
                        pv = (gcval_t*)pg->data;
                        char *lim = (char*)pv + GC_PAGE_SZ - pool->osize;
                        while ((char*)pv <= lim) {
                            if (!verifying) arraylist_push(&bits_save[gc_bits(pv)], pv);
                            gc_bits(pv) = bits;
                            pv = (gcval_t*)((char*)pv + pool->osize);
                        }
                    }
                }
            }
        }
    }
}

static void restore(void)
{
    for(int b = 0; b < 4; b++) {
        for(int i = 0; i < bits_save[b].len; i++) {
            gc_bits(bits_save[b].items[i]) = b;
        }
    }
}

static void gc_verify_track(void)
{
    do {
        arraylist_push(&lostval_parents_done, lostval);
        jl_printf(JL_STDERR, "Now looking for 0x%lx =======\n", lostval);
        clear_mark(GC_CLEAN);
        pre_mark();
        post_mark(&finalizer_list, 1);
        post_mark(&finalizer_list_marked, 1);
        if (lostval_parents.len == 0) {
            jl_printf(JL_STDERR, "Could not find the missing link. We missed a toplevel root. This is odd.\n");
            break;
        }
        jl_value_t* lostval_parent = NULL;
        for(int i = 0; i < lostval_parents.len; i++) {
            lostval_parent = (jl_value_t*)lostval_parents.items[i];
            int clean_len = bits_save[GC_CLEAN].len;
            for(int j = 0; j < clean_len + bits_save[GC_QUEUED].len; j++) {
                if (bits_save[j >= clean_len ? GC_QUEUED : GC_CLEAN].items[j >= clean_len ? j - clean_len : j] == lostval_parent) {
                    lostval = lostval_parent;
                    lostval_parent = NULL;
                    break;
                }
            }
            if (lostval_parent != NULL) break;
        }
        if (lostval_parent == NULL) { // all parents of lostval were also scheduled for deletion
            lostval = arraylist_pop(&lostval_parents);
        }
        else {
            jl_printf(JL_STDERR, "Missing write barrier found !\n");
            jl_printf(JL_STDERR, "0x%lx was written a reference to 0x%lx that was not recorded\n", lostval_parent, lostval);
            jl_printf(JL_STDERR, "(details above)\n");
            lostval = NULL;
        }
        restore();
    } while(lostval != NULL);
}

static void gc_verify(void)
{
    lostval = NULL;
    lostval_parents.len = 0;
    lostval_parents_done.len = 0;
    check_timeout = 0;
    clear_mark(GC_CLEAN);
    verifying = 1;
    pre_mark();
    post_mark(&finalizer_list, 1);
    post_mark(&finalizer_list_marked, 1);
    int clean_len = bits_save[GC_CLEAN].len;
    for(int i = 0; i < clean_len + bits_save[GC_QUEUED].len; i++) {
        gcval_t* v = (gcval_t*)bits_save[i >= clean_len ? GC_QUEUED : GC_CLEAN].items[i >= clean_len ? i - clean_len : i];
        if (gc_marked(v)) {
            jl_printf(JL_STDERR, "Error. Early free of 0x%lx type :", (uptrint_t)v);
            jl_(jl_typeof(v));
            jl_printf(JL_STDERR, "val : ");
            jl_(v);
            jl_printf(JL_STDERR, "Let's try to backtrack the missing write barrier :\n");
            lostval = v;
            break;
        }
    }
    if (lostval == NULL) {
        verifying = 0;
        restore();  // we did not miss anything
        return;
    }
    restore();
    gc_verify_track();
    abort();
}
#endif


// collector entry point and control

static int is_gc_enabled = 1;
DLLEXPORT void jl_gc_enable(void)    { is_gc_enabled = 1; }
DLLEXPORT void jl_gc_disable(void)   { is_gc_enabled = 0; }
DLLEXPORT int jl_gc_is_enabled(void) { return is_gc_enabled; }

DLLEXPORT int64_t jl_gc_total_bytes(void) { return total_allocd_bytes + allocd_bytes + collect_interval; }
DLLEXPORT uint64_t jl_gc_total_hrtime(void) { return total_gc_time; }
DLLEXPORT int64_t jl_gc_num_pause(void) { return n_pause; }
DLLEXPORT int64_t jl_gc_num_full_sweep(void) { return n_full_sweep; }

int64_t diff_gc_total_bytes(void)
{
    int64_t oldtb = last_gc_total_bytes;
    int64_t newtb = jl_gc_total_bytes();
    last_gc_total_bytes = newtb;
    return newtb - oldtb;
}
void sync_gc_total_bytes(void) {last_gc_total_bytes = jl_gc_total_bytes();}

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

int saved_mark_sp = 0;
int sweep_mask = GC_MARKED;
#define MIN_SCAN_BYTES 1024*1024

static void gc_mark_task_stack(jl_task_t*,int);

void prepare_sweep(void)
{
}

#ifdef GC_VERIFY
static void clear_mark(int);
#endif


void jl_gc_collect(int full)
{
    if (!is_gc_enabled) return;
    if (jl_in_gc) return;
    jl_in_gc = 1;
    JL_SIGATOMIC_BEGIN();
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
        // avoid counting remembered objects & bindings twice in perm_scanned_bytes
        for(int i = 0; i < last_remset->len; i++) {
            uintptr_t item = (uintptr_t)last_remset->items[i];
            void* ptr = (void*)(item & ~(uintptr_t)1);
            objprofile_count(jl_typeof(ptr), 2, 0);
            gc_bits(ptr) = GC_MARKED;
        }
        for (int i = 0; i < rem_bindings.len; i++) {
            void *ptr = rem_bindings.items[i];
            gc_bits(gc_val_buf(ptr)) = GC_MARKED;
        }

        for (int i = 0; i < last_remset->len; i++) {
            uintptr_t item = (uintptr_t)last_remset->items[i];
            void* ptr = (void*)(item & ~(uintptr_t)1);
            push_root(ptr, 0, gc_bits(ptr));
        }

        // 2. mark every object in a remembered binding
        int n_bnd_refyoung = 0;
        for (int i = 0; i < rem_bindings.len; i++) {
            void *ptr = rem_bindings.items[i];
            if (gc_push_root(((jl_binding_t*)ptr)->value, 0) == GC_MARKED_NOESC) {
                rem_bindings.items[n_bnd_refyoung] = ptr;
                n_bnd_refyoung++;
            }
        }
        rem_bindings.len = n_bnd_refyoung;

        // 3. walk roots
        pre_mark();
        visit_mark_stack(GC_MARKED_NOESC);

        allocd_bytes_since_sweep += allocd_bytes + (int64_t)collect_interval;

#if defined(GC_TIME) || defined(GC_FINAL_STATS)
        uint64_t mark_pause = jl_hrtime() - t0;
#endif
#ifdef GC_TIME
        JL_PRINTF(JL_STDOUT, "GC mark pause %.2f ms | scanned %ld kB = %ld + %ld | stack %d -> %d (wb %d) | remset %d %d\n", NS2MS(mark_pause), (scanned_bytes + perm_scanned_bytes)/1024, scanned_bytes/1024, perm_scanned_bytes/1024, saved_mark_sp, mark_sp, wb_activations, last_remset->len, allocd_bytes/1024);
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
    if(mark_sp == 0 || sweeping) {
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

#ifdef GC_VERIFY
            gc_verify();
#endif

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
            if ((full || ((not_freed_enough || promoted_bytes >= collect_interval) && (promoted_bytes >= default_collect_interval || prev_sweep_mask == GC_MARKED))) && n_pause > 1) {
                if (prev_sweep_mask != GC_MARKED || full) {
                    if (full) recollect = 1; // TODO enable this?
                }
                if (not_freed_enough) {
                    if (collect_interval < default_collect_interval)
                        collect_interval = default_collect_interval;
                    else if (collect_interval <= 2*(max_collect_interval/5)) {
                        collect_interval = 5*(collect_interval/2);
                    }
                }
                sweep_mask = GC_MARKED;
                promoted_bytes = 0;
                quick_count = 0;
            } else {
                collect_interval = default_collect_interval/2;
                sweep_mask = GC_MARKED_NOESC;
            }
            if (sweep_mask == GC_MARKED)
                perm_scanned_bytes = 0;
            scanned_bytes = 0;
            // 5. start sweeping
            sweep_weak_refs();
            gc_sweep_once(sweep_mask);
            sweeping = 1;
        }
        if (gc_sweep_inc(sweep_mask)) {
            // sweeping is over
            // 6. if it is a quick sweep, put back the remembered objects in queued state
            // so that we don't trigger the barrier again on them.
            if (sweep_mask == GC_MARKED_NOESC) {
                for (int i = 0; i < remset->len; i++) {
                    gc_bits(((uintptr_t)remset->items[i] & ~(uintptr_t)1)) = GC_QUEUED;
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

            sweeping = 0;
            if (sweep_mask == GC_MARKED) {
                tasks.len = 0;
            }
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
            freed_bytes = 0;

#if defined(GC_FINAL_STATS) || defined(GC_TIME)
            finalize_time = jl_hrtime();
#endif
            run_finalizers();
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
        JL_PRINTF(JL_STDOUT, "GC sweep pause %.2f ms live %ld kB (freed %d kB EST %d kB [error %d] = %d%% of allocd %d kB b/r %ld/%ld) (%.2f ms in post_mark, %.2f ms in %d fin) (marked in %d inc) mask %d | next in %d kB\n", NS2MS(sweep_pause), live_bytes/1024, SAVE2/1024, estimate_freed/1024, (SAVE2 - estimate_freed), pct, SAVE3/1024, bonus/1024, SAVE/1024, NS2MS(post_time), NS2MS(finalize_time), n_finalized, inc_count, sweep_mask, -allocd_bytes/1024);
#endif
    }
    n_pause++;
    uint64_t pause = jl_hrtime() - t0;
    total_gc_time += pause;
#ifdef GC_FINAL_STATS
    max_pause = max_pause < pause ? pause : max_pause;
#endif
    JL_SIGATOMIC_END();
    jl_in_gc = 0;
#ifdef GC_TIME
    if (estimate_freed != SAVE2) {
        // this should not happen but it does
        // mostly because of gc_counted_* allocations
    }
#endif
    if (recollect)
        jl_gc_collect(0);
}

// allocator entry points

void *allocb(size_t sz)
{
    buff_t *b;
    sz += sizeof(void*);
#ifdef MEMDEBUG
    b = (buff_t*)alloc_big(sz);
    b->pooled = 0;
#else
    if (sz > 2048) {
        b = (buff_t*)alloc_big(sz);
        b->pooled = 0;
    }
    else {
        b = (buff_t*)pool_alloc(&pools[szclass(sz)]);
        b->pooled = 1;
    }
#endif
    return &b->data;
}

void *reallocb(void *b, size_t sz)
{
    buff_t *buff = gc_val_buf(b);
    if (buff->pooled) {
        void* b2 = allocb(sz);
        memcpy(b2, b, page_metadata(buff)->osize);
        return b2;
    } else {
        bigval_t* bv = (bigval_t*)realloc(bigval_header(buff), sz + (BVOFFS + 1)*sizeof(void*));
        return (char*)bv + (BVOFFS + 1)*sizeof(void*);
    }
}

DLLEXPORT void *allocobj(size_t sz)
{
#ifdef MEMDEBUG
    return alloc_big(sz);
#endif
    if (sz <= 2048)
        return pool_alloc(&pools[szclass(sz)]);
    else
        return alloc_big(sz);
}

DLLEXPORT void *alloc_2w(void)
{
#ifdef MEMDEBUG
    return alloc_big(2*sizeof(void*));
#endif
#ifdef _P64
    return _pool_alloc(&pools[2], 2*sizeof(void*));
#else
    return _pool_alloc(&pools[0], 2*sizeof(void*));
#endif
}

DLLEXPORT void *alloc_3w(void)
{
#ifdef MEMDEBUG
    return alloc_big(3*sizeof(void*));
#endif
#ifdef _P64
    return _pool_alloc(&pools[4], 3*sizeof(void*));
#else
    return _pool_alloc(&pools[1], 3*sizeof(void*));
#endif

}

DLLEXPORT void *alloc_4w(void)
{
#ifdef MEMDEBUG
    return alloc_big(4*sizeof(void*));
#endif
#ifdef _P64
    return _pool_alloc(&pools[6], 4*sizeof(void*));
#else
    return pool_alloc(&pools[2]);
#endif
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
                  (total_mark_time*100)/total_gc_time, (total_sweep_time*100)/total_gc_time,
                  (total_fin_time*100)/total_gc_time);
    }
    int i = 0;
    while (i < HEAP_COUNT && heaps[i]) i++;
    jl_printf(s, "max allocated regions : %d\n", i);
    struct mallinfo mi = mallinfo();
    jl_printf(s, "malloc size\t%d MB\n", mi.uordblks/1024/1024);
    jl_printf(s, "max page alloc\t%ld MB\n", max_pg_count*GC_PAGE_SZ/1024/1024);
    jl_printf(s, "total freed\t%llu b\n", total_freed_bytes);
    jl_printf(s, "free rate\t%.1f MB/sec\n", (total_freed_bytes/gct)/1024/1024);
}
#endif

// initialization

void jl_gc_init(void)
{
    const int* szc = sizeclasses;
    int i;

    for(i=0; i < N_POOLS; i++) {
        assert(szc[i] % 4 == 0);
        norm_pools[i].osize = szc[i];
        norm_pools[i].freelist = NULL;
        norm_pools[i].newpages = NULL;
        norm_pools[i].end_offset = ((GC_PAGE_SZ/szc[i]) - 1)*szc[i];
    }

    collect_interval = default_collect_interval;
    allocd_bytes = -default_collect_interval;

    arraylist_new(&finalizer_list, 0);
    arraylist_new(&to_finalize, 0);
    arraylist_new(&preserved_values, 0);
    arraylist_new(&weak_refs, 0);
#ifdef GC_VERIFY
    for(int i = 0; i < 4; i++)
        arraylist_new(&bits_save[i], 0);
    arraylist_new(&lostval_parents, 0);
    arraylist_new(&lostval_parents_done, 0);
#endif
    arraylist_new(&tasks, 0);
    arraylist_new(&rem_bindings, 0);
    arraylist_new(remset, 0);
    arraylist_new(last_remset, 0);

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
        v = (gcval_t*)pg->data;
        char *lim = (char*)v + GC_PAGE_SZ - osize;
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
    *pwaste = npgs*GC_PAGE_SZ - (nused*p->osize);
    *np = npgs;
    *pnold = nold;
    if (npgs != 0) {
        JL_PRINTF(JL_STDOUT,
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
    JL_PRINTF(JL_STDOUT,
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
            nbytes += v->sz;
        }
        v = v->next;
    }
    v = big_objects_marked;
    size_t nused_old=0, nbytes_old=0;
    while (v != NULL) {
        if (gc_marked(&v->_data)) {
            nused_old++;
            nbytes_old += v->sz;
        }
        v = v->next;
    }

    mallocarray_t *ma = mallocarrays;
    while (ma != NULL) {
        if (gc_marked(ma->a)) {
            nused++;
            nbytes += array_nbytes(ma->a);
        }
        ma = ma->next;
    }

    JL_PRINTF(JL_STDOUT, "%d kB (%d%% old) in %d large objects (%d%% old)\n", (nbytes + nbytes_old)/1024, nbytes + nbytes_old ? (nbytes_old*100)/(nbytes + nbytes_old) : 0, nused + nused_old, nused+nused_old ? (nused_old*100)/(nused + nused_old) : 0);
}
#endif //MEMPROFILE

#ifdef __cplusplus
}
#endif
