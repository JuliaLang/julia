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
#include <assert.h>
#ifdef USE_MMAP
# include <sys/mman.h>
# include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"

#ifdef _P64
# ifdef USE_MMAP
#  define GC_PAGE_SZ 16384//bytes
# else
#  define GC_PAGE_SZ 12288//bytes
# endif
#else
#define GC_PAGE_SZ  8192//bytes
#endif

#ifdef __cplusplus
extern "C" {
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

#ifdef _P64
# define BVOFFS 2
#else
# define BVOFFS 4
#endif
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

// GC knobs and self-measurement variables
static size_t allocd_bytes = 0;
static int64_t total_allocd_bytes = 0;
static int64_t last_gc_total_bytes = 0;
static size_t freed_bytes = 0;
static uint64_t total_gc_time=0;
#ifdef _P64
#define default_collect_interval (5600*1024*sizeof(void*))
static size_t max_collect_interval = 1250000000UL;
#else
#define default_collect_interval (3200*1024*sizeof(void*))
static size_t max_collect_interval =  500000000UL;
#endif
static size_t collect_interval = default_collect_interval;
int jl_in_gc; // referenced from switchto task.c

#ifdef OBJPROFILE
static htable_t obj_counts;
#endif

#ifdef GC_FINAL_STATS
static size_t total_freed_bytes=0;
#endif

// manipulating mark bits
#define gc_marked(o)  (((gcval_t*)(o))->marked)
#define gc_setmark(o) (((gcval_t*)(o))->marked=1)
#define gc_val_buf(o) ((gcval_t*)(((void**)(o))-1))
#define gc_setmark_buf(o) gc_setmark(gc_val_buf(o))
#define gc_typeof(v) ((jl_value_t*)(((uptrint_t)jl_typeof(v))&~1UL))

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

DLLEXPORT void *jl_gc_counted_malloc(size_t sz)
{
    if (allocd_bytes > collect_interval)
        jl_gc_collect();
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
    if (allocd_bytes > collect_interval)
        jl_gc_collect();
    if (sz > old)
        allocd_bytes += (sz-old);
    void *b = realloc(p, sz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    return b;
}

void *jl_gc_managed_malloc(size_t sz)
{
    if (allocd_bytes > collect_interval)
        jl_gc_collect();
    sz = (sz+15) & -16;
    void *b = malloc_a16(sz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    allocd_bytes += sz;
    return b;
}

void *jl_gc_managed_realloc(void *d, size_t sz, size_t oldsz, int isaligned)
{
    if (allocd_bytes > collect_interval)
        jl_gc_collect();
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
    allocd_bytes += sz;
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

static htable_t finalizer_table;
static arraylist_t to_finalize;

static void schedule_finalization(void *o)
{
    arraylist_push(&to_finalize, o);
}

static void run_finalizer(jl_value_t *o, jl_value_t *ff)
{
    jl_function_t *f;
    while (1) {
        if (jl_is_tuple(ff))
            f = (jl_function_t*)jl_t0(ff);
        else
            f = (jl_function_t*)ff;
        assert(jl_is_function(f));
        JL_TRY {
            jl_apply(f, (jl_value_t**)&o, 1);
        }
        JL_CATCH {
            JL_PRINTF(JL_STDERR, "error in running finalizer: ");
            jl_static_show(JL_STDERR, jl_exception_in_transit);
            JL_PUTC('\n',JL_STDERR);
        }
        if (jl_is_tuple(ff))
            ff = jl_t1(ff);
        else
            break;
    }
}

static int finalize_object(jl_value_t *o)
{
    jl_value_t *ff = NULL;
    int success = 0;
    JL_GC_PUSH1(&ff);
    ff = (jl_value_t*)ptrhash_get(&finalizer_table, o);
    if (ff != HT_NOTFOUND) {
        ptrhash_remove(&finalizer_table, o);
        run_finalizer((jl_value_t*)o, ff);
        success = 1;
    }
    JL_GC_POP();
    return success;
}

static void run_finalizers(void)
{
    void *o = NULL;
    JL_GC_PUSH1(&o);
    while (to_finalize.len > 0) {
        o = arraylist_pop(&to_finalize);
        int ok = finalize_object((jl_value_t*)o);
        assert(ok); (void)ok;
    }
    JL_GC_POP();
}

void jl_gc_run_all_finalizers(void)
{
    for(size_t i=0; i < finalizer_table.size; i+=2) {
        jl_value_t *f = (jl_value_t*)finalizer_table.table[i+1];
        if (f != HT_NOTFOUND && !jl_is_cpointer(f)) {
            schedule_finalization(finalizer_table.table[i]);
        }
    }
    run_finalizers();
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

void jl_finalize(jl_value_t *o)
{
    (void)finalize_object(o);
}

// big value list

static bigval_t *big_objects = NULL;

static void *alloc_big(size_t sz)
{
    if (allocd_bytes > collect_interval)
        jl_gc_collect();
    size_t offs = BVOFFS*sizeof(void*);
    if (sz+offs+15 < offs+15)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    size_t allocsz = (sz+offs+15) & -16;
    bigval_t *v = (bigval_t*)malloc_a16(allocsz);
    allocd_bytes += allocsz;
    if (v == NULL)
        jl_throw(jl_memory_exception);
#ifdef MEMDEBUG
    //memset(v, 0xee, allocsz);
#endif
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
            free_a16(v);
        }
        v = nxt;
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

static size_t array_nbytes(jl_array_t *a)
{
    if (jl_array_ndims(a)==1)
        return a->elsize * a->maxsize;
    else
        return a->elsize * jl_array_len(a);
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

static void sweep_malloced_arrays()
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
        }
        ma = nxt;
    }
}

// pool allocation

#define N_POOLS 42
static pool_t norm_pools[N_POOLS];
static pool_t ephe_pools[N_POOLS];
static pool_t *pools = &norm_pools[0];

static void add_page(pool_t *p)
{
#ifdef USE_MMAP
    gcpage_t *pg = (gcpage_t*)mmap(NULL, sizeof(gcpage_t), PROT_READ|PROT_WRITE,
                                   MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
#else
    gcpage_t *pg = (gcpage_t*)malloc_a16(sizeof(gcpage_t));
#endif
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
    if (allocd_bytes > collect_interval)
        jl_gc_collect();
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

static void sweep_pool(pool_t *p)
{
#ifdef FREE_PAGES_EAGER
    int freedall;
#else
    int empty;
#endif
    gcval_t **prev_pfl;
    gcval_t *v;
    gcpage_t *pg = p->pages;
    gcpage_t **ppg = &p->pages;
    gcval_t **pfl = &p->freelist;
    size_t osize = p->osize;
    size_t nfreed = 0;

    size_t old_nfree = 0;
    gcval_t *ofl = p->freelist;
    while (ofl != NULL) {
        old_nfree++;
        ofl = ofl->next;
    }

    while (pg != NULL) {
        v = (gcval_t*)&pg->data[0];
        char *lim = (char*)v + GC_PAGE_SZ - osize;
#ifdef FREE_PAGES_EAGER
        freedall = 1;
#else
        empty = 1;
#endif
        prev_pfl = pfl;
        while ((char*)v <= lim) {
            if (!v->marked) {
#ifndef FREE_PAGES_EAGER
                // check that all but last object points to its next object,
                // which is a heuristic check for being on the freelist.
                if ((char*)v->next != (char*)v + osize && v->next != NULL &&
                    (char*)v+osize <= lim)
                    empty = 0;
#endif
                *pfl = v;
                pfl = &v->next;
                nfreed++;
            }
            else {
                v->marked = 0;
#ifdef FREE_PAGES_EAGER
                freedall = 0;
#else
                empty = 0;
#endif
            }
            v = (gcval_t*)((char*)v + osize);
        }
        gcpage_t *nextpg = pg->next;
        // lazy version: (empty) if the whole page was already unused, free it
        // eager version: (freedall) free page as soon as possible
        // the eager one uses less memory.
        if (
#ifdef FREE_PAGES_EAGER
            freedall
#else
            empty
#endif
            ) {
            pfl = prev_pfl;
            *ppg = nextpg;
#ifdef MEMDEBUG
            memset(pg, 0xbb, sizeof(gcpage_t));
#endif
#ifdef USE_MMAP
            munmap(pg, sizeof(gcpage_t));
#else
            free_a16(pg);
#endif
            //freed_bytes += GC_PAGE_SZ;
        }
        else {
            ppg = &pg->next;
        }
        pg = nextpg;
    }
    *pfl = NULL;
    freed_bytes += (nfreed - old_nfree)*osize;
}

// sweep phase

extern void jl_unmark_symbols(void);

static void gc_sweep(void)
{
    sweep_malloced_arrays();
    sweep_big();
    int i;
    for(i=0; i < N_POOLS; i++) {
        sweep_pool(&norm_pools[i]);
        sweep_pool(&ephe_pools[i]);
    }
    jl_unmark_symbols();
}

// mark phase

static jl_value_t **mark_stack = NULL;
static size_t mark_stack_size = 0;
static size_t mark_sp = 0;

static void push_root(jl_value_t *v, int d);

#define gc_push_root(v,d) do {  assert(v != NULL); if (!gc_marked(v)) { push_root((jl_value_t*)(v),d); } } while (0)

void jl_gc_setmark(jl_value_t *v)
{
    gc_setmark(v);
}

static void gc_mark_stack(jl_gcframe_t *s, ptrint_t offset, int d)
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
                if (rts[i] != NULL)
                    gc_push_root(rts[i], d);
            }
        }
        s = s->prev;
    }
}

static void gc_mark_module(jl_module_t *m, int d)
{
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            gc_setmark_buf(b);
            if (b->value != NULL)
                gc_push_root(b->value, d);
            if (b->type != (jl_value_t*)jl_any_type)
                gc_push_root(b->type, d);
        }
    }
    // this is only necessary because bindings for "using" modules
    // are added only when accessed. therefore if a module is replaced
    // after "using" it but before accessing it, this array might
    // contain the only reference.
    for(i=0; i < m->usings.len; i++) {
        gc_push_root(m->usings.items[i], d);
    }
    if (m->constant_table)
        gc_push_root(m->constant_table, d);
}

static void gc_mark_task(jl_task_t *ta, int d)
{
    if (ta->parent) gc_push_root(ta->parent, d);
    if (ta->last) gc_push_root(ta->last, d);
    gc_push_root(ta->tls, d);
    gc_push_root(ta->consumers, d);
    gc_push_root(ta->donenotify, d);
    gc_push_root(ta->exception, d);
    if (ta->start)  gc_push_root(ta->start, d);
    if (ta->result) gc_push_root(ta->result, d);
    if (ta->stkbuf != NULL || ta == jl_current_task) {
        if (ta->stkbuf != NULL)
            gc_setmark_buf(ta->stkbuf);
#ifdef COPY_STACKS
        ptrint_t offset;
        if (ta == jl_current_task) {
            offset = 0;
            gc_mark_stack(jl_pgcstack, offset, d);
        }
        else {
            offset = (char *)ta->stkbuf - ((char *)jl_stackbase - ta->ssize);
            gc_mark_stack(ta->gcstack, offset, d);
        }
#else
        gc_mark_stack(ta->gcstack, 0, d);
#endif
    }
}

// for chasing down unwanted references
/*
static jl_value_t *lookforme = NULL;
DLLEXPORT void jl_gc_lookfor(jl_value_t *v) { lookforme = v; }
*/

#define MAX_MARK_DEPTH 400

static void push_root(jl_value_t *v, int d)
{
    assert(v != NULL);
    jl_value_t *vt = (jl_value_t*)gc_typeof(v);

#ifdef OBJPROFILE
    if (!gc_marked(v)) {
        void **bp = ptrhash_bp(&obj_counts, vt);
        if (*bp == HT_NOTFOUND)
            *bp = (void*)2;
        else
            (*((ptrint_t*)bp))++;
    }
#endif

    gc_setmark(v);

    if (vt == (jl_value_t*)jl_weakref_type ||
        (jl_is_datatype(vt) && ((jl_datatype_t*)vt)->pointerfree)) {
        return;
    }

    if (d >= MAX_MARK_DEPTH)
        goto queue_the_root;

    d++;

    // some values have special representations
    if (vt == (jl_value_t*)jl_tuple_type) {
        size_t l = jl_tuple_len(v);
        jl_value_t **data = ((jl_tuple_t*)v)->data;
        for(size_t i=0; i < l; i++) {
            jl_value_t *elt = data[i];
            if (elt != NULL)
                gc_push_root(elt, d);
        }
    }
    else if (((jl_datatype_t*)(vt))->name == jl_array_typename) {
        jl_array_t *a = (jl_array_t*)v;
        if (a->how == 3) {
            jl_value_t *owner = jl_array_data_owner(a);
            gc_push_root(owner, d);
            return;
        }
        else if (a->how == 1) {
            gc_setmark_buf((char*)a->data - a->offset*a->elsize);
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
                    if (elt != NULL) gc_push_root(elt, d);
                }
            }
        }
    }
    else if (vt == (jl_value_t*)jl_module_type) {
        gc_mark_module((jl_module_t*)v, d);
    }
    else if (vt == (jl_value_t*)jl_task_type) {
        gc_mark_task((jl_task_t*)v, d);
    }
    else {
        jl_datatype_t *dt = (jl_datatype_t*)vt;
        int nf = (int)jl_tuple_len(dt->names);
        for(int i=0; i < nf; i++) {
            if (dt->fields[i].isptr) {
                jl_value_t *fld = *(jl_value_t**)((char*)v + dt->fields[i].offset + sizeof(void*));
                if (fld)
                    gc_push_root(fld, d);
            }
        }
    }
    return;

 queue_the_root:
    if (mark_sp >= mark_stack_size) {
        size_t newsz = mark_stack_size>0 ? mark_stack_size*2 : 32000;
        mark_stack = (jl_value_t**)realloc(mark_stack,newsz*sizeof(void*));
        if (mark_stack == NULL) exit(1);
        mark_stack_size = newsz;
    }
    mark_stack[mark_sp++] = v;
}

static void visit_mark_stack()
{
    while (mark_sp > 0) {
        push_root(mark_stack[--mark_sp], 0);
    }
}

void jl_mark_box_caches(void);

extern jl_value_t * volatile jl_task_arg_in_transit;
#if defined(GCTIME) || defined(GC_FINAL_STATS)
double clock_now(void);
#endif

extern jl_module_t *jl_old_base_module;
extern jl_array_t *typeToTypeId;
extern jl_array_t *jl_module_init_order;

static void gc_mark(void)
{
    // mark all roots

    // active tasks
    gc_push_root(jl_root_task, 0);
    gc_push_root(jl_current_task, 0);

    // modules
    gc_push_root(jl_main_module, 0);
    gc_push_root(jl_internal_main_module, 0);
    gc_push_root(jl_current_module, 0);
    if (jl_old_base_module) gc_push_root(jl_old_base_module, 0);

    // invisible builtin values
    if (jl_an_empty_cell) gc_push_root(jl_an_empty_cell, 0);
    gc_push_root(jl_exception_in_transit, 0);
    gc_push_root(jl_task_arg_in_transit, 0);
    gc_push_root(jl_unprotect_stack_func, 0);
    gc_push_root(jl_bottom_func, 0);
    gc_push_root(jl_typetype_type, 0);
    gc_push_root(jl_tupletype_type, 0);
    gc_push_root(typeToTypeId, 0);
    if (jl_module_init_order != NULL)
        gc_push_root(jl_module_init_order, 0);

    // constants
    gc_push_root(jl_null, 0);
    gc_push_root(jl_true, 0);
    gc_push_root(jl_false, 0);

    jl_mark_box_caches();

    size_t i;

    // stuff randomly preserved
    for(i=0; i < preserved_values.len; i++) {
        gc_push_root((jl_value_t*)preserved_values.items[i], 0);
    }

    // objects currently being finalized
    for(i=0; i < to_finalize.len; i++) {
        gc_push_root(to_finalize.items[i], 0);
    }

    visit_mark_stack();

    // find unmarked objects that need to be finalized.
    // this must happen last.
    for(i=0; i < finalizer_table.size; i+=2) {
        if (finalizer_table.table[i+1] != HT_NOTFOUND) {
            jl_value_t *v = (jl_value_t*)finalizer_table.table[i];
            if (!gc_marked(v)) {
                jl_value_t *fin = (jl_value_t*)finalizer_table.table[i+1];
                if (gc_typeof(fin) == (jl_value_t*)jl_voidpointer_type) {
                    void *p = ((void**)fin)[1];
                    if (p)
                        ((void (*)(void*))p)(jl_data_ptr(v));
                    finalizer_table.table[i+1] = HT_NOTFOUND;
                    continue;
                }
                gc_push_root(v, 0);
                schedule_finalization(v);
            }
            gc_push_root(finalizer_table.table[i+1], 0);
        }
    }

    visit_mark_stack();
}

// collector entry point and control

static int is_gc_enabled = 1;
DLLEXPORT void jl_gc_enable(void)    { is_gc_enabled = 1; }
DLLEXPORT void jl_gc_disable(void)   { is_gc_enabled = 0; }
DLLEXPORT int jl_gc_is_enabled(void) { return is_gc_enabled; }

DLLEXPORT int64_t jl_gc_total_bytes(void) { return total_allocd_bytes + allocd_bytes; }
DLLEXPORT uint64_t jl_gc_total_hrtime(void) { return total_gc_time; }

int64_t diff_gc_total_bytes(void)
{
    int64_t oldtb = last_gc_total_bytes;
    int64_t newtb = jl_gc_total_bytes();
    last_gc_total_bytes = newtb;
    return newtb - oldtb;
}
void sync_gc_total_bytes(void) {last_gc_total_bytes = jl_gc_total_bytes();}

void jl_gc_ephemeral_on(void)  { pools = &ephe_pools[0]; }
void jl_gc_ephemeral_off(void) { pools = &norm_pools[0]; }

#if defined(MEMPROFILE)
static void all_pool_stats(void);
static void big_obj_stats(void);
#endif

#ifdef OBJPROFILE
static void print_obj_profile(void)
{
    for(int i=0; i < obj_counts.size; i+=2) {
        if (obj_counts.table[i+1] != HT_NOTFOUND) {
            jl_printf(JL_STDERR, "%d ", obj_counts.table[i+1]-1);
            jl_static_show(JL_STDERR, (jl_value_t*)obj_counts.table[i]);
            jl_printf(JL_STDERR, "\n");
        }
    }
}
#endif

void jl_gc_collect(void)
{
    size_t actual_allocd = allocd_bytes;
    total_allocd_bytes += allocd_bytes;
    allocd_bytes = 0;
    if (is_gc_enabled) {
        JL_SIGATOMIC_BEGIN();
        jl_in_gc = 1;
        uint64_t t0 = jl_hrtime();
        gc_mark();
#ifdef GCTIME
        JL_PRINTF(JL_STDERR, "mark time %.3f ms\n", (jl_hrtime()-t0)*1.0e6);
#endif
#if defined(MEMPROFILE)
        all_pool_stats();
        big_obj_stats();
#endif
#ifdef GCTIME
        uint64_t t1 = jl_hrtime();
#endif
        sweep_weak_refs();
        gc_sweep();
#ifdef GCTIME
        JL_PRINTF(JL_STDERR, "sweep time %.3f ms\n", (jl_hrtime()-t1)*1.0e6);
#endif
        int nfinal = to_finalize.len;
        run_finalizers();
        jl_in_gc = 0;
        JL_SIGATOMIC_END();
        total_gc_time += (jl_hrtime()-t0);
#if defined(GC_FINAL_STATS)
        total_freed_bytes += freed_bytes;
#endif
#ifdef OBJPROFILE
        print_obj_profile();
        htable_reset(&obj_counts, 0);
#endif

        // tune collect interval based on current live ratio
#if defined(MEMPROFILE)
        jl_printf(JL_STDERR, "allocd %ld, freed %ld, interval %ld, ratio %.2f\n",
                  actual_allocd, freed_bytes, collect_interval,
                  (double)freed_bytes/(double)actual_allocd);
#endif
        if (freed_bytes < (7*(actual_allocd/10))) {
            if (collect_interval <= 2*(max_collect_interval/5))
                collect_interval = 5*(collect_interval/2);
        }
        else {
            collect_interval = default_collect_interval;
        }
        freed_bytes = 0;
        // if a lot of objects were finalized, re-run GC to finish freeing
        // their storage if possible.
        if (nfinal > 100000)
            jl_gc_collect();
    }
}

// allocator entry points

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

DLLEXPORT void *allocobj(size_t sz)
{
#ifdef MEMDEBUG
    return alloc_big(sz);
#endif
    if (sz > 2048)
        return alloc_big(sz);
    return pool_alloc(&pools[szclass(sz)]);
}

DLLEXPORT void *alloc_2w(void)
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

DLLEXPORT void *alloc_3w(void)
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

DLLEXPORT void *alloc_4w(void)
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
void jl_print_gc_stats(JL_STREAM *s)
{
    double gct = total_gc_time/1e9;
    malloc_stats();
    double ptime = clock_now()-process_t0;
    jl_printf(s, "exec time\t%.5f sec\n", ptime);
    jl_printf(s, "gc time  \t%.5f sec (%2.1f%%)\n", gct, (gct/ptime)*100);
    struct mallinfo mi = mallinfo();
    jl_printf(s, "malloc size\t%d MB\n", mi.uordblks/1024/1024);
    jl_printf(s, "total freed\t%llu b\n", total_freed_bytes);
    jl_printf(s, "free rate\t%.1f MB/sec\n", (total_freed_bytes/gct)/1024/1024);
}
#endif

// initialization

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
    mallocarray_t *ma = mallocarrays;
    while (ma != NULL) {
        if (gc_marked(ma->a)) {
            nused++;
            nbytes += array_nbytes(ma->a);
        }
        ma = ma->next;
    }

    JL_PRINTF(JL_STDOUT, "%d bytes in %d large objects\n", nbytes, nused);
}
#endif //MEMPROFILE

#ifdef __cplusplus
}
#endif
