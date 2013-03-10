/*
  allocation and garbage collection
  . non-moving, precise mark and sweep collector
  . pool-allocates small objects, keeps big objects on a simple list
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "julia.h"

// with MEMDEBUG, every object is allocated explicitly with malloc, and
// filled with 0xbb before being freed.
//#define MEMDEBUG

// MEMPROFILE prints pool summary statistics after every GC
//#define MEMPROFILE

// GCTIME prints time taken by each phase of GC
//#define GCTIME

// GC_FINAL_STATS prints total GC stats at exit
//#define GC_FINAL_STATS

// OBJPROFILE counts objects by type
//#define OBJPROFILE

#ifdef _P64
# define BVOFFS 2
#else
# define BVOFFS 4
#endif

#ifdef _P64
#define GC_PAGE_SZ (1536*sizeof(void*))//bytes
#else
#define GC_PAGE_SZ (2048*sizeof(void*))//bytes
#endif

typedef struct _gcpage_t {
    char data[GC_PAGE_SZ];
    union {
        struct _gcpage_t *next;
        char _pad[8];
    };
} gcpage_t;

typedef struct _gcval_t {
    union {
        struct _gcval_t *next;
        uptrint_t flags;
        uptrint_t data0;  // overlapped
        uptrint_t marked:1;
    };
} gcval_t;

typedef struct _pool_t {
    size_t osize;
    gcpage_t *pages;
    gcval_t *freelist;
} pool_t;

typedef struct _bigval_t {
    struct _bigval_t *next;
    size_t sz;
#ifndef _P64
    uptrint_t _pad0;
    uptrint_t _pad1;
#endif
    union {
        uptrint_t flags;
        uptrint_t marked:1;
        char _data[1];
    };
} bigval_t;

#define gc_marked(o)  (((gcval_t*)(o))->marked)
#define gc_setmark(o) (((gcval_t*)(o))->marked=1)
#define gc_val_buf(o) ((gcval_t*)(((void**)(o))-1))
#define gc_setmark_buf(o) gc_setmark(gc_val_buf(o))

static bigval_t *big_objects = NULL;

static jl_mallocptr_t *malloc_ptrs = NULL;
static jl_mallocptr_t *malloc_ptrs_freelist = NULL;

#define N_POOLS 42
static pool_t norm_pools[N_POOLS];
static pool_t ephe_pools[N_POOLS];
static pool_t *pools = &norm_pools[0];

static size_t allocd_bytes = 0;
static size_t freed_bytes = 0;
#define default_collect_interval (3200*1024*sizeof(void*))
static size_t collect_interval = default_collect_interval;
#ifdef _P64
# define max_collect_interval 1250000000UL
#else
# define max_collect_interval 500000000UL
#endif

static htable_t finalizer_table;
static arraylist_t to_finalize;

static arraylist_t preserved_values;

static arraylist_t weak_refs;

#ifdef OBJPROFILE
static htable_t obj_counts;
#endif

#ifdef GC_FINAL_STATS
static double total_gc_time=0;
static size_t total_freed_bytes=0;
#endif

int jl_gc_n_preserved_values(void)
{
    return preserved_values.len;
}

void jl_gc_preserve(jl_value_t *v)
{
    arraylist_push(&preserved_values, (void*)v);
}

void jl_gc_unpreserve(void)
{
    (void)arraylist_pop(&preserved_values);
}

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

static void schedule_finalization(void *o)
{
    arraylist_push(&to_finalize, o);
}

static void run_finalizers(void)
{
    void *o = NULL;
    jl_function_t *f=NULL;
    jl_value_t *ff=NULL;
    JL_GC_PUSH(&o, &f, &ff);
    while (to_finalize.len > 0) {
        o = arraylist_pop(&to_finalize);
        ff = (jl_value_t*)ptrhash_get(&finalizer_table, o);
        assert(ff != HT_NOTFOUND);
        ptrhash_remove(&finalizer_table, o);
        while (jl_is_tuple(ff)) {
            f = (jl_function_t*)jl_t0(ff);
            assert(jl_is_function(f));
            JL_TRY {
                jl_apply(f, (jl_value_t**)&o, 1);
            }
            JL_CATCH {
            }
            ff = jl_t1(ff);
        }
        f = (jl_function_t*)ff;
        assert(jl_is_function(f));
        jl_apply(f, (jl_value_t**)&o, 1);
    }
    JL_GC_POP();
}

void jl_gc_add_finalizer(jl_value_t *v, jl_function_t *f)
{
    jl_value_t **bp = (jl_value_t**)ptrhash_bp(&finalizer_table, v);
    if (*bp == HT_NOTFOUND) {
        *bp = (jl_value_t*)f;
    }
    else {
        *bp = (jl_value_t*)jl_tuple2((jl_value_t*)f, *bp);
    }
}

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

#ifdef __LP64__
#define malloc_a16(sz) malloc(((sz)+15)&-16)
#else
#if defined(WIN32)
// TODO - use _aligned_malloc, which requires _aligned_free
#define malloc_a16(sz) malloc(((sz)+15)&-16)

#elif defined(__APPLE__)
#define malloc_a16(sz) malloc(((sz)+15)&-16)

#else
static inline void *malloc_a16(size_t sz)
{
    void *ptr;
    if (posix_memalign(&ptr, 16, (sz+15)&-16))
        return NULL;
    return ptr;
}
#endif
#endif

static void *alloc_big(size_t sz)
{
    if (allocd_bytes > collect_interval) {
        jl_gc_collect();
    }
    size_t offs = BVOFFS*sizeof(void*);
    if (sz+offs+15 < offs+15)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    size_t allocsz = (sz+offs+15) & -16;
    bigval_t *v = (bigval_t*)malloc_a16(allocsz);
    allocd_bytes += allocsz;
    if (v == NULL)
        jl_throw(jl_memory_exception);
    v->sz = sz;
    v->flags = 0;
    v->next = big_objects;
    big_objects = v;
    return &v->_data[0];
}

static void sweep_big(void)
{
    bigval_t *v = big_objects;
    bigval_t **pv = &big_objects;
    while (v != NULL) {
        bigval_t *nxt = v->next;
        if (v->marked) {
            pv = &v->next;
            v->marked = 0;
        }
        else {
            *pv = nxt;
            freed_bytes += v->sz;
#ifdef MEMDEBUG
            memset(v, 0xbb, v->sz+BVOFFS*sizeof(void*));
#endif
            free(v);
        }
        v = nxt;
    }
}

jl_mallocptr_t *jl_gc_acquire_buffer(void *b, size_t sz)
{
    jl_mallocptr_t *mp;
    if (malloc_ptrs_freelist == NULL) {
        mp = malloc(sizeof(jl_mallocptr_t));
    }
    else {
        mp = malloc_ptrs_freelist;
        malloc_ptrs_freelist = malloc_ptrs_freelist->next;
    }
    mp->sz = sz;
    mp->ptr = b;
    mp->next = malloc_ptrs;
    malloc_ptrs = mp;
    return mp;
}

jl_mallocptr_t *jl_gc_managed_malloc(size_t sz)
{
    if (allocd_bytes > collect_interval) {
        jl_gc_collect();
    }
    sz = (sz+15) & -16;
    void *b = malloc_a16(sz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    allocd_bytes += sz;
    return jl_gc_acquire_buffer(b, sz);
}

static void sweep_malloc_ptrs(void)
{
    jl_mallocptr_t *mp = malloc_ptrs;
    jl_mallocptr_t **pmp = &malloc_ptrs;
    while (mp != NULL) {
        jl_mallocptr_t *nxt = (jl_mallocptr_t*)((uptrint_t)mp->next & ~1UL);
        if (((gcval_t*)mp)->marked) {
            pmp = &mp->next;
            ((gcval_t*)mp)->marked = 0;
        }
        else {
            *pmp = nxt;
            if (mp->ptr) {
                freed_bytes += mp->sz;
                free(mp->ptr);
            }
            mp->next = malloc_ptrs_freelist;
            malloc_ptrs_freelist = mp;
        }
        mp = nxt;
    }
}

static void add_page(pool_t *p)
{
    gcpage_t *pg = malloc_a16(sizeof(gcpage_t));
    if (pg == NULL)
        jl_throw(jl_memory_exception);
    gcval_t *v = (gcval_t*)&pg->data[0];
    char *lim = (char*)v + GC_PAGE_SZ - p->osize;
    gcval_t *fl;
    gcval_t **pfl = &fl;
    while ((char*)v <= lim) {
        *pfl = v;
        pfl = &v->next;
        v = (gcval_t*)((char*)v + p->osize);
    }
    // these statements are ordered so that interrupting after any of them
    // leaves the system in a valid state
    *pfl = p->freelist;
    pg->next = p->pages;
    p->pages = pg;
    p->freelist = fl;
}

static inline void *pool_alloc(pool_t *p)
{
    if (allocd_bytes > collect_interval) {
        jl_gc_collect();
    }
    allocd_bytes += p->osize;
    if (p->freelist == NULL) {
        add_page(p);
    }
    assert(p->freelist != NULL);
    gcval_t *v = p->freelist;
    p->freelist = p->freelist->next;
    v->flags = 0;
    return v;
}

static void sweep_pool(pool_t *p)
{
    //int empty;
    int freedall;
    gcval_t **prev_pfl;
    gcval_t *v;
    gcpage_t *pg = p->pages;
    gcpage_t **ppg = &p->pages;
    gcval_t **pfl = &p->freelist;
    size_t osize = p->osize;
    size_t nfreed = 0;
    size_t nfree = 0;
    gcval_t *old_fl = p->freelist;

    while (pg != NULL) {
        v = (gcval_t*)&pg->data[0];
        char *lim = (char*)v + GC_PAGE_SZ - osize;
        //empty = 1;
        freedall = 1;
        prev_pfl = pfl;
        while ((char*)v <= lim) {
            if (old_fl != NULL) {
                // keep track of difference between new and old freelist
                // in order to count newly-freed objects
                nfree++;
                old_fl = old_fl->next;
            }
            if (!v->marked) {
                *pfl = v;
                pfl = &v->next;
                nfreed++;
            }
            else {
                v->marked = 0;
                freedall = 0;
            }
            v = (gcval_t*)((char*)v + osize);
        }
        gcpage_t *nextpg = pg->next;
        // lazy version: (empty) if the whole page was already unused, free it
        // eager version: (freedall) free page as soon as possible
        // the eager one uses less memory.
        if (freedall) {
            pfl = prev_pfl;
            *ppg = nextpg;
#ifdef MEMDEBUG
            memset(pg, 0xbb, sizeof(gcpage_t));
#endif
            free(pg);
            //freed_bytes += GC_PAGE_SZ;
        }
        else {
            ppg = &pg->next;
        }
        pg = nextpg;
    }
    *pfl = NULL;
    freed_bytes += (nfreed-nfree)*osize;
}

extern void jl_unmark_symbols(void);

static void gc_sweep(void)
{
    sweep_big();
    sweep_malloc_ptrs();
    int i;
    for(i=0; i < N_POOLS; i++) {
        sweep_pool(&norm_pools[i]);
        sweep_pool(&ephe_pools[i]);
    }
    jl_unmark_symbols();
}

static jl_value_t **mark_stack = NULL;
static size_t mark_stack_size = 0;
static size_t mark_sp = 0;

#define gc_typeof(v) ((jl_value_t*)(((uptrint_t)jl_typeof(v))&~1UL))

static void push_root(jl_value_t *v)
{
    assert(v != NULL);
    if (gc_marked(v)) return;
    jl_value_t *vt = (jl_value_t*)jl_typeof(v);
#ifdef OBJPROFILE
    void **bp = ptrhash_bp(&obj_counts, vt);
    if (*bp == HT_NOTFOUND)
        *bp = (void*)2;
    else
        (*((ptrint_t*)bp))++;
#endif
    gc_setmark(v);
    if (vt == (jl_value_t*)jl_weakref_type ||
        (jl_is_datatype(vt) && ((jl_datatype_t*)vt)->pointerfree)) {
        return;
    }
    if (mark_sp >= mark_stack_size) {
        size_t newsz = mark_stack_size>0 ? mark_stack_size*2 : 32000;
        mark_stack = (jl_value_t**)realloc(mark_stack,newsz*sizeof(void*));
        if (mark_stack == NULL) exit(1);
        mark_stack_size = newsz;
    }
    mark_stack[mark_sp++] = v;
}

#define gc_push_root(v) push_root((jl_value_t*)(v))

void jl_gc_setmark(jl_value_t *v)
{
    gc_setmark(v);
}

static void gc_mark_stack(jl_gcframe_t *s, ptrint_t offset)
{
    while (s != NULL) {
        s = (jl_gcframe_t*)((char*)s + offset);
        jl_value_t ***rts = (jl_value_t***)(((void**)s)+2);
        size_t nr = s->nroots>>1;
        if (s->nroots & 1) {
            for(size_t i=0; i < nr; i++) {
                jl_value_t **ptr = (jl_value_t**)((char*)rts[i] + offset);
                if (*ptr != NULL)
                    gc_push_root(*ptr);
            }
        }
        else {
            for(size_t i=0; i < nr; i++) {
                if (rts[i] != NULL)
                    gc_push_root(rts[i]);
            }
        }
        s = s->prev;
    }
}

static void gc_mark_module(jl_module_t *m)
{
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            gc_setmark_buf(b);
            if (b->value != NULL)
                gc_push_root(b->value);
            gc_push_root(b->type);
        }
    }
}

// for chasing down unwanted references
/*
static jl_value_t *lookforme = NULL;
DLLEXPORT void jl_gc_lookfor(jl_value_t *v) { lookforme = v; }
*/

static void gc_mark_all()
{
    while (mark_sp > 0) {
    jl_value_t *v = mark_stack[--mark_sp];
    jl_value_t *vt = (jl_value_t*)gc_typeof(v);

    // some values have special representations
    if (vt == (jl_value_t*)jl_tuple_type) {
        size_t l = jl_tuple_len(v);
        jl_value_t **data = ((jl_tuple_t*)v)->data;
        for(size_t i=0; i < l; i++) {
            jl_value_t *elt = data[i];
            if (elt != NULL)
                gc_push_root(elt);
        }
    }
    else if (((jl_datatype_t*)(vt))->name == jl_array_typename) {
        jl_array_t *a = (jl_array_t*)v;
        char *data = a->data;
        if (data == NULL) continue;
        int ndims = jl_array_ndims(a);
        void *data_area = jl_array_inline_data_area(a);
        char *data0 = data;
        if (ndims == 1) data0 -= a->offset*a->elsize;
        if (data0 != data_area) {
            jl_value_t *owner = *(jl_value_t**)data_area;
            if (a->ismalloc) {
                // jl_mallocptr_t
                if (gc_marked(owner))
                    continue;
                gc_setmark(owner);
            }
            else {
                // an array
                v = owner;
                if (v != (jl_value_t*)a) {
                    gc_push_root(v);
                    continue;
                }
            }
        }
        if (a->ptrarray) {
            size_t l = jl_array_len(a);
            for(size_t i=0; i < l; i++) {
                jl_value_t *elt = ((jl_value_t**)data)[i];
                if (elt != NULL) gc_push_root(elt);
            }
        }
    }
    else if (vt == (jl_value_t*)jl_module_type) {
        gc_mark_module((jl_module_t*)v);
    }
    else if (vt == (jl_value_t*)jl_task_type) {
        jl_task_t *ta = (jl_task_t*)v;
        if (ta->on_exit) gc_push_root(ta->on_exit);
        gc_push_root(ta->last);
        gc_push_root(ta->tls);
        gc_push_root(ta->consumers);
        if (ta->start)  gc_push_root(ta->start);
        if (ta->result) gc_push_root(ta->result);
        if (ta->stkbuf != NULL || ta == jl_current_task) {
            if (ta->stkbuf != NULL)
                gc_setmark_buf(ta->stkbuf);
#ifdef COPY_STACKS
            ptrint_t offset;
            if (ta == jl_current_task) {
                offset = 0;
                gc_mark_stack(jl_pgcstack, offset);
            }
            else {
                offset = ta->stkbuf - (ta->stackbase-ta->ssize);
                gc_mark_stack(ta->gcstack, offset);
            }
#else
            gc_mark_stack(ta->gcstack, 0);
#endif
        }
    }
    else {
        jl_datatype_t *dt = (jl_datatype_t*)vt;
        int nf = (int)jl_tuple_len(dt->names);
        for(int i=0; i < nf; i++) {
            if (dt->fields[i].isptr) {
                jl_value_t *fld = *(jl_value_t**)((char*)v + dt->fields[i].offset + sizeof(void*));
                if (fld)
                    gc_push_root(fld);
            }
        }
    }
    }
}

void jl_mark_box_caches(void);

extern jl_value_t * volatile jl_task_arg_in_transit;
#if defined(GCTIME) || defined(GC_FINAL_STATS)
double clock_now(void);
#endif

static void gc_mark_uv_handle(uv_handle_t *handle, void *arg)
{
    if(handle->data) {
        gc_push_root((jl_value_t*)(handle->data));
    }
}

static void gc_mark_uv_state(uv_loop_t *loop)
{
    uv_walk(loop,gc_mark_uv_handle,0);
}

static void gc_mark(void)
{
    // mark all roots

    // active tasks
    gc_push_root(jl_root_task);
    gc_push_root(jl_current_task);

    // modules
    gc_push_root(jl_main_module);
    gc_push_root(jl_current_module);

    // invisible builtin values
    if (jl_an_empty_cell) gc_push_root(jl_an_empty_cell);
    gc_push_root(jl_exception_in_transit);
    gc_push_root(jl_task_arg_in_transit);
    gc_push_root(jl_unprotect_stack_func);
    gc_push_root(jl_bottom_func);
    gc_push_root(jl_typetype_type);

    // constants
    gc_push_root(jl_null);
    gc_push_root(jl_true);
    gc_push_root(jl_false);

    // libuv loops
    gc_mark_uv_state(jl_global_event_loop());

    jl_mark_box_caches();

    size_t i;

    // stuff randomly preserved
    for(i=0; i < preserved_values.len; i++) {
        gc_push_root((jl_value_t*)preserved_values.items[i]);
    }

    // objects currently being finalized
    for(i=0; i < to_finalize.len; i++) {
        gc_push_root(to_finalize.items[i]);
    }

    gc_mark_all();
    
    // find unmarked objects that need to be finalized.
    // this must happen last.
    for(i=0; i < finalizer_table.size; i+=2) {
        if (finalizer_table.table[i+1] != HT_NOTFOUND) {
            jl_value_t *v = finalizer_table.table[i];
            if (!gc_marked(v)) {
                gc_push_root(v);
                schedule_finalization(v);
            }
            gc_push_root(finalizer_table.table[i+1]);
        }
    }

    gc_mark_all();
}

static int is_gc_enabled = 1;
DLLEXPORT void jl_gc_enable(void)    { is_gc_enabled = 1; }
DLLEXPORT void jl_gc_disable(void)   { is_gc_enabled = 0; }
DLLEXPORT int jl_gc_is_enabled(void) { return is_gc_enabled; }

void jl_gc_ephemeral_on(void)  { pools = &ephe_pools[0]; }
void jl_gc_ephemeral_off(void) { pools = &norm_pools[0]; }

#if defined(MEMPROFILE)
static void all_pool_stats(void);
static void big_obj_stats(void);
#endif

#ifdef OBJPROFILE
static void print_obj_profile(void)
{
    jl_value_t *errstream = jl_stderr_obj();
    for(int i=0; i < obj_counts.size; i+=2) {
        if (obj_counts.table[i+1] != HT_NOTFOUND) {
            jl_printf(jl_stderr, "%d ", obj_counts.table[i+1]-1);
            jl_show(errstream, obj_counts.table[i]);
            jl_printf(jl_stderr, "\n");
        }
    }
}
#endif

void jl_gc_collect(void)
{
    allocd_bytes = 0;
    if (is_gc_enabled) {
        freed_bytes = 0;
        JL_SIGATOMIC_BEGIN();
#if defined(GCTIME) || defined(GC_FINAL_STATS)
        double t0 = clock_now();
#endif
        gc_mark();
#ifdef GCTIME
        JL_PRINTF(JL_STDERR, "mark time %.3f ms\n", (clock_now()-t0)*1000);
#endif
#if defined(MEMPROFILE)
        all_pool_stats();
        big_obj_stats();
#endif
#ifdef GCTIME
        t0 = clock_now();
#endif
        sweep_weak_refs();
        gc_sweep();
#ifdef GCTIME
        JL_PRINTF(JL_STDERR, "sweep time %.3f ms\n", (clock_now()-t0)*1000);
#endif
        run_finalizers();
        JL_SIGATOMIC_END();
#if defined(GC_FINAL_STATS)
        total_gc_time += (clock_now()-t0);
        total_freed_bytes += freed_bytes;
#endif
#ifdef OBJPROFILE
        print_obj_profile();
        htable_reset(&obj_counts, 0);
#endif

        // tune collect interval based on current live ratio
        if (freed_bytes < ((2*collect_interval)/5)) {
            if (collect_interval <= (2*max_collect_interval)/5)
                collect_interval = (5*collect_interval)/2;
        }
        else {
            collect_interval = default_collect_interval;
        }
    }
}

void *allocb(size_t sz)
{
    void *b;
    sz += sizeof(void*);
#ifdef MEMDEBUG
    b = alloc_big(sz);
#else
    if (sz > 2048) {
        b = alloc_big(sz);
    }
    else {
        b = pool_alloc(&pools[szclass(sz)]);
    }
#endif
    return (void*)((void**)b + 1);
}

void *allocobj(size_t sz)
{
#ifdef MEMDEBUG
    return alloc_big(sz);
#endif
    if (sz > 2048)
        return alloc_big(sz);
    return pool_alloc(&pools[szclass(sz)]);
}

void *alloc_2w(void)
{
#ifdef MEMDEBUG
    return alloc_big(2*sizeof(void*));
#endif
#ifdef _P64
    return pool_alloc(&pools[2]);
#else
    return pool_alloc(&pools[0]);
#endif
}

void *alloc_3w(void)
{
#ifdef MEMDEBUG
    return alloc_big(3*sizeof(void*));
#endif
#ifdef _P64
    return pool_alloc(&pools[4]);
#else
    return pool_alloc(&pools[1]);
#endif
}

void *alloc_4w(void)
{
#ifdef MEMDEBUG
    return alloc_big(4*sizeof(void*));
#endif
#ifdef _P64
    return pool_alloc(&pools[6]);
#else
    return pool_alloc(&pools[2]);
#endif
}

#ifdef GC_FINAL_STATS
static double process_t0;
#include <malloc.h>
void print_gc_stats(void)
{
    malloc_stats();
    double ptime = clock_now()-process_t0;
    jl_printf(JL_STDERR, "exec time\t%.5f sec\n", ptime);
    jl_printf(JL_STDOUT, "gc time  \t%.5f sec (%2.1f%%)\n", total_gc_time,
               (total_gc_time/ptime)*100);
    struct mallinfo mi = mallinfo();
    jl_printf(JL_STDOUT, "malloc size\t%d MB\n", mi.uordblks/1024/1024);
    jl_printf(JL_STDOUT, "total freed\t%llu b\n", total_freed_bytes);
    jl_printf(JL_STDOUT, "free rate\t%.1f MB/sec\n",
               (total_freed_bytes/total_gc_time)/1024/1024);
}
#endif

void jl_gc_init(void)
{
    int szc[N_POOLS] = { 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56,
                         64, 72, 80, 88, 96, //#=18

                         112, 128, 144, 160, 176, 192, 208, 224, 240, 256,

                         288, 320, 352, 384, 416, 448, 480, 512,

                         640, 768, 896, 1024, 

                         1536, 2048 };
    int i;
    for(i=0; i < N_POOLS; i++) {
        norm_pools[i].osize = szc[i];
        norm_pools[i].pages = NULL;
        norm_pools[i].freelist = NULL;

        ephe_pools[i].osize = szc[i];
        ephe_pools[i].pages = NULL;
        ephe_pools[i].freelist = NULL;
    }

    htable_new(&finalizer_table, 0);
    arraylist_new(&to_finalize, 0);
    arraylist_new(&preserved_values, 0);
    arraylist_new(&weak_refs, 0);

#ifdef OBJPROFILE
    htable_new(&obj_counts, 0);
#endif
#ifdef GC_FINAL_STATS
    process_t0 = clock_now();
    atexit(print_gc_stats);
#endif
}

#if defined(MEMPROFILE)
static size_t pool_stats(pool_t *p, size_t *pwaste)
{
    gcval_t *v;
    gcpage_t *pg = p->pages;
    size_t osize = p->osize;
    size_t nused=0, nfree=0, npgs=0;

    while (pg != NULL) {
        npgs++;
        v = (gcval_t*)&pg->data[0];
        char *lim = (char*)v + GC_PAGE_SZ - osize;
        while ((char*)v <= lim) {
            if (!v->marked) {
                nfree++;
            }
            else {
                nused++;
            }
            v = (gcval_t*)((char*)v + osize);
        }
        gcpage_t *nextpg = pg->next;
        pg = nextpg;
    }
    *pwaste = npgs*GC_PAGE_SZ - (nused*p->osize);
    JL_PRINTF(JL_STDOUT,
               "%4d : %7d/%7d objects, %5d pages, %8d bytes, %8d waste\n",
               p->osize,
               nused,
               nused+nfree,
               npgs,
               nused*p->osize,
               *pwaste);
    return nused*p->osize;
}

static void all_pool_stats(void)
{
    int i;
    size_t nb=0, w, tw=0, no=0, b;
    for(i=0; i < N_POOLS; i++) {
        b = pool_stats(&norm_pools[i], &w);
        nb += b;
        no += (b/norm_pools[i].osize);
        tw += w;

        b = pool_stats(&ephe_pools[i], &w);
        nb += b;
        no += (b/ephe_pools[i].osize);
        tw += w;
    }
    JL_PRINTF(JL_STDOUT,
               "%d objects, %d total allocated, %d total fragments\n",
               no, nb, tw);
}

static void big_obj_stats(void)
{
    bigval_t *v = big_objects;
    size_t nused=0, nbytes=0;
    while (v != NULL) {
        if (v->marked) {
            nused++;
            nbytes += v->sz;
        }
        v = v->next;
    }
    jl_mallocptr_t *mp = malloc_ptrs;
    while (mp != NULL) {
        jl_mallocptr_t *nxt = (jl_mallocptr_t*)((uptrint_t)mp->next & ~1UL);
        if (((gcval_t*)mp)->marked) {
            nused++;
            nbytes += mp->sz;
        }
        mp = nxt;
    }

    JL_PRINTF(JL_STDOUT, "%d bytes in %d large objects\n", nbytes, nused);
}
#endif //MEMPROFILE
