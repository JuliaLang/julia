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
// NOTE: needs to be defined in ios.c too, due to GC/IObuffer interaction
//#define MEMDEBUG

// MEMPROFILE prints pool summary statistics after every GC
// NOTE: define in ios.c too
//#define MEMPROFILE

// GCTIME prints time taken by each phase of GC
//#define GCTIME

// OBJPROFILE counts objects by type
//#define OBJPROFILE

#define GC_PAGE_SZ (1536*sizeof(void*)+8)//bytes

typedef struct _gcpage_t {
    union {
        struct _gcpage_t *next;
        char _pad[8];
    };
    char data[GC_PAGE_SZ - 8];
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
#if defined(MEMDEBUG) || defined(MEMPROFILE)
    union {
        size_t sz;
        char _pad[8];
    };
#endif
    union {
        uptrint_t flags;
        struct {
            uptrint_t marked:1;
            uptrint_t isobj:1;
        };
    };
    char _data[1];
} bigval_t;

#if defined(MEMDEBUG) || defined(MEMPROFILE)
# ifdef __LP64__
#  define BVOFFS 3
# else
#  define BVOFFS 4
# endif
#else
#define BVOFFS 2
#endif

#define gc_val(o)     ((gcval_t*)(((void**)(o))-1))
#define gc_marked(o)  (gc_val(o)->marked)
#define gc_setmark(o) (gc_val(o)->marked=1)
#define gc_marked_obj(o)  (((gcval_t*)(o))->marked)
#define gc_setmark_obj(o) (((gcval_t*)(o))->marked=1)

static bigval_t *big_objects = NULL;

#define N_POOLS 42
static pool_t norm_pools[N_POOLS];
static pool_t ephe_pools[N_POOLS];
static pool_t *pools = &norm_pools[0];

static size_t allocd_bytes = 0;
static const size_t collect_interval = 3200*1024*sizeof(void*);

static htable_t finalizer_table;
static arraylist_t to_finalize;

static arraylist_t preserved_values;

static arraylist_t weak_refs;

#ifdef OBJPROFILE
static htable_t obj_counts;
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
    wr->type = (jl_type_t*)jl_weakref_type;
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
        if (gc_marked_obj(wr)) {
            // weakref itself is alive
            if (!gc_marked_obj(wr->value))
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

htable_t *jl_gc_get_finalizer_table(void)
{
    return &finalizer_table;
}

static int szclass(size_t sz)
{
#ifndef __LP64__
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

static void *alloc_big(size_t sz, int isobj)
{
    if (allocd_bytes > collect_interval) {
        jl_gc_collect();
    }
    sz = (sz+3) & -4;
    allocd_bytes += sz;
    size_t offs = BVOFFS*sizeof(void*);
    if (sz + offs < offs)  // overflow in adding offs, size was "negative"
        jl_raise(jl_memory_exception);
    bigval_t *v = (bigval_t*)malloc(sz + offs);
    if (v == NULL)
        jl_raise(jl_memory_exception);
#if defined(MEMDEBUG) || defined(MEMPROFILE)
    v->sz = sz;
#endif
    v->next = big_objects;
    v->flags = 0;
    v->isobj = isobj;
    big_objects = v;
    return &v->_data[0];
}

void jl_gc_acquire_buffer(void *b)
{
    bigval_t *v = (bigval_t*)(((void**)b)-BVOFFS);
#if defined(MEMDEBUG) || defined(MEMPROFILE)
    v->sz = 0;  // ???
#endif
    v->next = big_objects;
    v->flags = 0;
    v->isobj = 0;
    big_objects = v;
}

#define bigval_word0(v) (((uptrint_t*)(&((bigval_t*)(v))->_data[0]))[0])

static void sweep_big(void)
{
    bigval_t *v = big_objects;
    bigval_t **pv = &big_objects;
    while (v != NULL) {
        bigval_t *nxt = v->next;
        if (v->isobj && (bigval_word0(v)&1)) {
            pv = &v->next;
            bigval_word0(v) &= ~1UL;
        }
        else if (!v->isobj && v->marked) {
            pv = &v->next;
            v->marked = 0;
        }
        else {
            *pv = nxt;
#ifdef MEMDEBUG
            memset(v, 0xbb, v->sz+BVOFFS*sizeof(void*));
#endif
            free(v);
        }
        v = nxt;
    }
}

static void add_page(pool_t *p)
{
    gcpage_t *pg = malloc(sizeof(gcpage_t));
    if (pg == NULL)
        jl_raise(jl_memory_exception);
    gcval_t *v = (gcval_t*)&pg->data[0];
    char *lim = (char*)pg + GC_PAGE_SZ - p->osize;
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

    while (pg != NULL) {
        char *lim = (char*)pg + GC_PAGE_SZ - osize;
        v = (gcval_t*)&pg->data[0];
        //empty = 1;
        freedall = 1;
        prev_pfl = pfl;
        while ((char*)v <= lim) {
            if (!v->marked) {
                *pfl = v;
                pfl = &v->next;
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
        }
        else {
            ppg = &pg->next;
        }
        pg = nextpg;
    }
    *pfl = NULL;
}

extern void jl_unmark_symbols(void);

static void gc_sweep(void)
{
    sweep_big();
    int i;
    for(i=0; i < N_POOLS; i++) {
        sweep_pool(&norm_pools[i]);
        sweep_pool(&ephe_pools[i]);
    }
    jl_unmark_symbols();
}

#define GC_Markval(v) gc_markval_((jl_value_t*)(v))
static void gc_markval_(jl_value_t *v);

void jl_gc_markval(jl_value_t *v)
{
    gc_markval_(v);
}

static void gc_mark_stack(jl_gcframe_t *s, ptrint_t offset)
{
    while (s != NULL) {
        s = (jl_gcframe_t*)((char*)s + offset);
        size_t i;
        jl_value_t ***rts = (jl_value_t***)((char*)s->roots + offset);
        if (s->indirect) {
            size_t nr = s->nroots;
            for(i=0; i < nr; i++) {
                jl_value_t **ptr = (jl_value_t**)((char*)rts[i] + offset);
                if (*ptr != NULL)
                    GC_Markval(*ptr);
            }
        }
        else {
            size_t nr = s->nroots;
            for(i=0; i < nr; i++) {
                if (rts[i] != NULL)
                    GC_Markval(rts[i]);
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
            gc_setmark(b);
            if (b->value != NULL)
                GC_Markval(b->value);
            GC_Markval(b->type);
        }
    }
    table = m->macros.table;
    for(i=1; i < m->macros.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            GC_Markval((jl_value_t*)table[i]);
        }
    }
}

#define gc_typeof(v) ((jl_value_t*)(((uptrint_t)jl_typeof(v))&~1UL))

// for chasing down unwanted references
/*
static jl_value_t *lookforme = NULL;
DLLEXPORT void jl_gc_lookfor(jl_value_t *v) { lookforme = v; }
*/

static void gc_markval_(jl_value_t *v)
{
 gc_markval_top:
    assert(v != NULL);
    //assert(v != lookforme);
    if (gc_marked_obj(v)) return;
    jl_value_t *vt = (jl_value_t*)jl_typeof(v);
#ifdef OBJPROFILE
    void **bp = ptrhash_bp(&obj_counts, vt);
    if (*bp == HT_NOTFOUND)
        *bp = (void*)2;
    else
        (*((ptrint_t*)bp))++;
#endif
    gc_setmark_obj(v);

    if (gc_typeof(vt) == (jl_value_t*)jl_bits_kind) return;

    // some values have special representations
    if (vt == (jl_value_t*)jl_tuple_type) {
        size_t l = jl_tuple_len(v);
        for(size_t i=0; i < l; i++) {
            jl_value_t *elt = ((jl_tuple_t*)v)->data[i];
            if (elt != NULL)
                GC_Markval(elt);
        }
    }
    else if (((jl_struct_type_t*)(vt))->name == jl_array_typename) {
        jl_array_t *a = (jl_array_t*)v;
        int ndims = jl_array_ndims(a);
        int ndimwords = (ndims > 2 ? (ndims-2) : 0);
#ifndef __LP64__
        // on 32-bit, ndimwords must be odd to preserve 8-byte alignment
        ndimwords += (~ndimwords)&1;
#endif
        void *data_area = &a->_space[0] + ndimwords*sizeof(size_t);
        if (a->reshaped) {
            GC_Markval(*((jl_value_t**)data_area));
        }
        else if (a->data) {
            char *data = a->data;
            if (ndims == 1) data -= a->offset*a->elsize;
            if (data != data_area) {
                gc_setmark(data);
            }
        }
        if (a->ptrarray) {
            size_t l = a->length;
            if (l > 0) {
                for(size_t i=0; i < l-1; i++) {
                    jl_value_t *elt = ((jl_value_t**)a->data)[i];
                    if (elt != NULL) GC_Markval(elt);
                }
                v = ((jl_value_t**)a->data)[l-1];
                if (v != NULL) goto gc_markval_top;
            }
        }
    }
    else if (vt == (jl_value_t*)jl_module_type) {
        gc_mark_module((jl_module_t*)v);
    }
    else if (vt == (jl_value_t*)jl_task_type) {
        jl_task_t *ta = (jl_task_t*)v;
        GC_Markval(ta->on_exit);
        GC_Markval(ta->tls);
        if (ta->start)
            GC_Markval(ta->start);
        if (ta->result)
            GC_Markval(ta->result);
        GC_Markval(ta->state.eh_task);
        if (ta->stkbuf != NULL)
            gc_setmark(ta->stkbuf);
#ifdef COPY_STACKS
        ptrint_t offset;
        if (ta == jl_current_task) {
            offset = 0;
            gc_mark_stack(jl_pgcstack, offset);
        }
        else {
            offset = ta->stkbuf - (ta->stackbase-ta->ssize);
            gc_mark_stack(ta->state.gcstack, offset);
        }
        jl_savestate_t *ss = &ta->state;
        while (ss != NULL) {
            GC_Markval(ss->ostream_obj);
            ss = ss->prev;
            if (ss != NULL)
                ss = (jl_savestate_t*)((char*)ss + offset);
        }
#else
        gc_mark_stack(ta->state.gcstack, 0);
        jl_savestate_t *ss = &ta->state;
        while (ss != NULL) {
            GC_Markval(ss->ostream_obj);
            ss = ss->prev;
        }
#endif
    }
    else if (vt == (jl_value_t*)jl_weakref_type) {
        // don't mark contents
    }
    else {
        int nf = (int)jl_tuple_len(((jl_struct_type_t*)vt)->names);
        if (nf > 0) {
            int i = 0;
            if (vt == (jl_value_t*)jl_struct_kind ||
                vt == (jl_value_t*)jl_function_type) {
                i++;  // skip fptr field
            }
            for(; i < nf-1; i++) {
                jl_value_t *fld = ((jl_value_t**)v)[i+1];
                if (fld)
                    GC_Markval(fld);
            }
            v = ((jl_value_t**)v)[i+1];
            if (v)
                goto gc_markval_top;
        }
    }
}

void jl_mark_box_caches(void);

extern jl_value_t * volatile jl_task_arg_in_transit;
#ifdef GCTIME
double clock_now(void);
#endif

static void gc_mark(void)
{
    // mark all roots

    // active tasks
    GC_Markval(jl_root_task);
    GC_Markval(jl_current_task);

    // modules
    GC_Markval(jl_core_module);
    GC_Markval(jl_current_module);

    // invisible builtin values
    if (jl_an_empty_cell) GC_Markval(jl_an_empty_cell);
    GC_Markval(jl_exception_in_transit);
    GC_Markval(jl_task_arg_in_transit);
    GC_Markval(jl_unprotect_stack_func);
    GC_Markval(jl_bottom_func);
    GC_Markval(jl_typetype_type);

    // constants
    GC_Markval(jl_null);
    GC_Markval(jl_true);
    GC_Markval(jl_false);

    jl_mark_box_caches();

    size_t i;

    // stuff randomly preserved
    for(i=0; i < preserved_values.len; i++) {
        GC_Markval((jl_value_t*)preserved_values.items[i]);
    }

    // objects currently being finalized
    for(i=0; i < to_finalize.len; i++) {
        GC_Markval(to_finalize.items[i]);
    }
    // find unmarked objects that need to be finalized.
    // this must happen last.
    for(i=0; i < finalizer_table.size; i+=2) {
        if (finalizer_table.table[i+1] != HT_NOTFOUND) {
            jl_value_t *v = finalizer_table.table[i];
            if (!gc_marked_obj(v)) {
                GC_Markval(v);
                schedule_finalization(v);
            }
            GC_Markval(finalizer_table.table[i+1]);
        }
    }
}

static int is_gc_enabled = 0;
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
            ios_printf(ios_stderr, "%d ", obj_counts.table[i+1]-1);
            jl_show(errstream, obj_counts.table[i]);
            ios_printf(ios_stderr, "\n");
        }
    }
}
#endif

void jl_gc_collect(void)
{
    allocd_bytes = 0;
    if (is_gc_enabled) {
        JL_SIGATOMIC_BEGIN();
#ifdef GCTIME
        double t0 = clock_now();
#endif
        gc_mark();
#ifdef GCTIME
        ios_printf(ios_stderr, "mark time %.3f ms\n", (clock_now()-t0)*1000);
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
        ios_printf(ios_stderr, "sweep time %.3f ms\n", (clock_now()-t0)*1000);
#endif
        run_finalizers();
        JL_SIGATOMIC_END();
#ifdef OBJPROFILE
        print_obj_profile();
        htable_reset(&obj_counts, 0);
#endif
    }
}

void *allocb(size_t sz)
{
#ifdef MEMDEBUG
    return alloc_big(sz, 0);
#endif
    if (sz > 2048-sizeof(void*))
        return alloc_big(sz, 0);
    sz += sizeof(void*);
    allocd_bytes += sz;
    void *b = pool_alloc(&pools[szclass(sz)]);
    return (void*)((void**)b + 1);
}

void *allocobj(size_t sz)
{
#ifdef MEMDEBUG
    return alloc_big(sz, 1);
#endif
    if (sz > 2048)
        return alloc_big(sz, 1);
    allocd_bytes += sz;
    return pool_alloc(&pools[szclass(sz)]);
}

void *alloc_2w(void)
{
#ifdef MEMDEBUG
    return alloc_big(2*sizeof(void*), 1);
#endif
    allocd_bytes += (2*sizeof(void*));
#ifdef __LP64__
    return pool_alloc(&pools[2]);
#else
    return pool_alloc(&pools[0]);
#endif
}

void *alloc_3w(void)
{
#ifdef MEMDEBUG
    return alloc_big(3*sizeof(void*), 1);
#endif
    allocd_bytes += (3*sizeof(void*));
#ifdef __LP64__
    return pool_alloc(&pools[4]);
#else
    return pool_alloc(&pools[1]);
#endif
}

void *alloc_4w(void)
{
#ifdef MEMDEBUG
    return alloc_big(4*sizeof(void*), 1);
#endif
    allocd_bytes += (4*sizeof(void*));
#ifdef __LP64__
    return pool_alloc(&pools[6]);
#else
    return pool_alloc(&pools[2]);
#endif
}

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
        char *lim = (char*)pg + GC_PAGE_SZ - osize;
        v = (gcval_t*)&pg->data[0];
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
    ios_printf(ios_stdout,
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
    ios_printf(ios_stdout,
               "%d objects, %d total allocated, %d total fragments\n",
               no, nb, tw);
}

static void big_obj_stats(void)
{
    bigval_t *v = big_objects;
    size_t nused=0, nbytes=0;
    while (v != NULL) {
        if (v->isobj && (bigval_word0(v)&1)) {
            nused++;
            nbytes += v->sz;
        }
        else if (!v->isobj && v->marked) {
            nused++;
            nbytes += v->sz;
        }
        v = v->next;
    }
    ios_printf(ios_stdout, "%d bytes in %d large objects\n", nbytes, nused);
}
#endif //MEMPROFILE
