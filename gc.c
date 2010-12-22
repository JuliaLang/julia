/*
  allocation and garbage collection
  . non-moving, precise mark and sweep collector
  . pool-allocates small objects, keeps big objects on a simple list
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <sys/types.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#ifdef BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

// with MEMDEBUG, every object is allocated explicitly with malloc, and
// filled with 0xbb before being freed.
//#define MEMDEBUG

#define GC_PAGE_SZ (4096*sizeof(void*))//bytes

typedef struct _gcpage_t {
    struct _gcpage_t *next;
    char data[GC_PAGE_SZ - sizeof(void*)];
} gcpage_t;

typedef struct _gcval_t {
    union {
        struct _gcval_t *next;
        uptrint_t flags;
        struct {
            uptrint_t marked:1;
            //uptrint_t finalize:1;
            //uptrint_t typed:1;
#ifdef BITS64
            uptrint_t otherbits:62;
#else
            uptrint_t otherbits:30;
#endif
        };
    };
    char _data[1];
} gcval_t;

typedef struct _pool_t {
    size_t osize;
    gcpage_t *pages;
    gcval_t *freelist;
} pool_t;

typedef struct _bigval_t {
    struct _bigval_t *next;
    size_t sz;
    union {
        uptrint_t flags;
        struct {
            uptrint_t marked:1;
            //uptrint_t finalize:1;
            //uptrint_t typed:1;
#ifdef BITS64
            uptrint_t otherbits:62;
#else
            uptrint_t otherbits:30;
#endif
        };
    };
    char _data[1];
} bigval_t;

#define gc_val(o)     ((gcval_t*)(((void**)(o))-1))
#define gc_marked(o)  (gc_val(o)->marked)
#define gc_setmark(o) (gc_val(o)->marked=1)
#define gc_unmark(o)  (gc_val(o)->marked=0)

#define gcv_isfree(v)  ((v)->otherbits!=0)

static bigval_t *big_objects = NULL;

#define N_POOLS 19
static pool_t pools[N_POOLS];

static size_t allocd_bytes = 0;
static size_t collect_interval = 8192*1024;

static htable_t finalizer_table;
static arraylist_t to_finalize;

static void schedule_finalization(void *o)
{
    arraylist_push(&to_finalize, o);
}

static void run_finalizers()
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
            jl_apply(f, (jl_value_t**)&o, 1);
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

// size classes:
// <=8, 12, 16, 20, 24, 28, 32, 48, 64, 96, 128, 192, 256, 384, 512, 768, 1024, 1536, 2048
//   0   1   2   3   4   5   6   7   8   9   10   11   12   13   14   15,   16,   17,   18

static int szclass(size_t sz)
{
    if (sz <= 8) return 0;
    if (sz <= 32) return ((sz+3)>>2) - 2;
    if (sz <= 128) {
        if (sz <= 64) {
            if (sz <= 48) return 7;
            return 8;
        }
        if (sz <= 96) return 9;
        return 10;
    }
    if (sz <= 512) {
        if (sz <= 256) {
            if (sz <= 192) return 11;
            return 12;
        }
        if (sz <= 384) return 13;
        return 14;
    }
    if (sz <= 1024) {
        if (sz <= 768) return 15;
        return 16;
    }
    if (sz <= 1536) return 17;
    return 18;
}

static void *alloc_big(size_t sz)
{
    sz = (sz+3) & -4;
    bigval_t *v = (bigval_t*)malloc(sz + 3*sizeof(void*));
    v->sz = sz;
    v->next = big_objects;
    v->flags = 0;
    big_objects = v;
    return &v->_data[0];
}

static void sweep_big()
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
#ifdef MEMDEBUG
            size_t thesz = v->sz;
            memset(v, 0xbb, v->sz+3*sizeof(void*));
#endif
            free(v);
        }
        v = nxt;
    }
}

static void add_page(pool_t *p)
{
    gcpage_t *pg = malloc(sizeof(gcpage_t));
    gcval_t *v = (gcval_t*)&pg->data[0];
    char *lim = (char*)pg + GC_PAGE_SZ - p->osize;
    gcval_t *oldfl = p->freelist;
    gcval_t **pfl = &p->freelist;
    while ((char*)v <= lim) {
        *pfl = v;
        pfl = &v->next;
        v = (gcval_t*)((char*)v + p->osize);
    }
    *pfl = oldfl;
    pg->next = p->pages;
    p->pages = pg;
}

static void *pool_alloc(pool_t *p)
{
    if (p->freelist == NULL)
        add_page(p);
    assert(p->freelist != NULL);
    gcval_t *v = p->freelist;
    p->freelist = p->freelist->next;
    v->flags = 0;
    return &v->_data[0];
}

static void sweep_pool(pool_t *p)
{
    int empty, freedall;
    gcval_t **prev_pfl;
    gcval_t *v;
    gcpage_t *pg = p->pages;
    gcpage_t **ppg = &p->pages;
    gcval_t **pfl = &p->freelist;

    while (pg != NULL) {
        char *lim = (char*)pg + GC_PAGE_SZ - p->osize;
        v = (gcval_t*)&pg->data[0];
        empty = 1;
        freedall = 1;
        prev_pfl = pfl;
        while ((char*)v <= lim) {
            //if (!gcv_isfree(v))
            //    empty = 0;
            if (gcv_isfree(v)) {
                *pfl = v;
                pfl = &v->next;
            }
            else if (!v->marked) {
                *pfl = v;
                pfl = &v->next;
            }
            else {
                v->marked = 0;
                freedall = 0;
            }
            v = (gcval_t*)((char*)v + p->osize);
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

static void gc_sweep()
{
    sweep_big();
    int i;
    for(i=0; i < N_POOLS; i++)
        sweep_pool(&pools[i]);
}

#define GC_Markval(v) gc_markval_((jl_value_t*)(v))
static void gc_markval_(jl_value_t *v);

void jl_gc_markval(jl_value_t *v)
{
    gc_markval_(v);
}

static void gc_mark_stack(jl_gcframe_t *s)
{
    while (s != NULL) {
        size_t i;
        if (s->indirect) {
            for(i=0; i < s->nroots; i++) {
                if (*s->roots[i] != NULL)
                    GC_Markval(*s->roots[i]);
            }
        }
        else {
            for(i=0; i < s->nroots; i++) {
                if (s->roots[i] != NULL)
                    GC_Markval(s->roots[i]);
            }
        }
        s = s->prev;
    }
}

static void gc_mark_methlist(jl_methlist_t *ml)
{
    while (ml != NULL) {
        gc_setmark(ml);
        GC_Markval(ml->sig);
        GC_Markval(ml->tvars);
        GC_Markval(ml->func);
        ml = ml->next;
    }
}

void jl_mark_type_cache(void *c);

static void gc_markval_(jl_value_t *v)
{
    assert(v != NULL);
    if (gc_marked(v)) return;
    gc_setmark(v);

    if (jl_is_bits_type(jl_typeof(v))) return;

    // some values have special representations
    if (jl_is_array(v)) {
        jl_array_t *a = (jl_array_t*)v;
        GC_Markval(a->dims);
        if (a->data && a->data != &a->_space[0])
            gc_setmark(a->data);
        jl_value_t *elty = jl_tparam0(jl_typeof(v));
        if (!jl_is_bits_type(elty)) {
            size_t i;
            for(i=0; i < a->length; i++) {
                jl_value_t *elt = ((jl_value_t**)a->data)[i];
                if (elt != NULL) GC_Markval(elt);
            }
        }
    }
    else if (jl_is_tuple(v)) {
        size_t i;
        for(i=0; i < ((jl_tuple_t*)v)->length; i++) {
            jl_value_t *elt = ((jl_tuple_t*)v)->data[i];
            if (elt != NULL)
                GC_Markval(elt);
        }
    }
    else if (jl_is_lambda_info(v)) {
        jl_lambda_info_t *li = (jl_lambda_info_t*)v;
        if (li->ast)
            GC_Markval(li->ast);
        GC_Markval(li->sparams);
        GC_Markval(li->tfunc);
        GC_Markval(li->roots);
        if (li->specTypes)
            GC_Markval(li->specTypes);
        if (li->unspecialized != NULL)
            GC_Markval(li->unspecialized);
    }
    else if (jl_is_typename(v)) {
        jl_typename_t *tn = (jl_typename_t*)v;
        if (tn->primary != NULL)
            GC_Markval(tn->primary);
        jl_mark_type_cache(tn->cache);
    }
    else if (jl_is_tag_type(v)) {
        jl_tag_type_t *tt = (jl_tag_type_t*)v;
        assert(tt->env == NULL);
        assert(tt->linfo == NULL);
        //if (tt->env  !=NULL) GC_Markval(tt->env);
        //if (tt->linfo!=NULL) GC_Markval(tt->linfo);
        GC_Markval(tt->name);
        GC_Markval(tt->super);
        GC_Markval(tt->parameters);
    }
    else if (jl_is_struct_type(v)) {
        jl_struct_type_t *st = (jl_struct_type_t*)v;
        if (st->env  !=NULL) GC_Markval(st->env);
        if (st->linfo!=NULL) GC_Markval(st->linfo);
        GC_Markval(st->name);
        GC_Markval(st->super);
        GC_Markval(st->parameters);
        GC_Markval(st->names);
        GC_Markval(st->types);
        if (st->ctor_factory != NULL)
            GC_Markval(st->ctor_factory);
        if (st->instance != NULL)
            GC_Markval(st->instance);
    }
    else if (jl_is_bits_type(v)) {
        jl_bits_type_t *bt = (jl_bits_type_t*)v;
        assert(bt->env == NULL);
        assert(bt->linfo == NULL);
        //if (bt->env  !=NULL) GC_Markval(bt->env);
        //if (bt->linfo!=NULL) GC_Markval(bt->linfo);
        GC_Markval(bt->name);
        GC_Markval(bt->super);
        GC_Markval(bt->parameters);
        GC_Markval(bt->bnbits);
    }
    else if (jl_is_func(v)) {
        jl_function_t *f = (jl_function_t*)v;
        if (f->env  !=NULL) GC_Markval(f->env);
        if (f->linfo!=NULL) GC_Markval(f->linfo);
    }
    else if (jl_is_mtable(v)) {
        jl_methtable_t *mt = (jl_methtable_t*)v;
        size_t i;
        gc_mark_methlist(mt->defs);
        gc_mark_methlist(mt->cache);
        for(i=0; i < mt->n_1arg; i++) {
            if (mt->cache_1arg[i] != NULL)
                GC_Markval(mt->cache_1arg[i]);
        }
    }
    else if (jl_is_task(v)) {
        jl_task_t *ta = (jl_task_t*)v;
        GC_Markval(ta->on_exit);
        if (ta->start)
            GC_Markval(ta->start);
        if (ta->result)
            GC_Markval(ta->result);
        gc_mark_stack(ta->state.gcstack);
        GC_Markval(ta->state.eh_task);
        if (ta->_stkbase != NULL)
            gc_setmark(ta->_stkbase);
        jl_savestate_t *ss = &ta->state;
        while (ss != NULL) {
            GC_Markval(ss->ostream_obj);
            ss = ss->prev;
        }
    }
    else {
        assert(jl_is_struct_type(jl_typeof(v)));
        size_t nf = ((jl_struct_type_t*)jl_typeof(v))->names->length;
        size_t i;
        for(i=0; i < nf; i++) {
            jl_value_t *fld = ((jl_value_t**)v)[i+1];
            if (fld)
                GC_Markval(fld);
        }
    }
}

static void gc_mark_module(jl_module_t *m)
{
    size_t i;
    void **table = m->bindings.table;
    gc_setmark(m);
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

void jl_mark_box_caches();

static void gc_mark()
{
    // mark all roots

    // active tasks
    GC_Markval(jl_root_task);
    GC_Markval(jl_current_task);

    // modules
    gc_mark_module(jl_system_module);
    gc_mark_module(jl_user_module);

    // invisible builtin values
    GC_Markval(jl_methtable_type);
    GC_Markval(jl_bottom_func);
    GC_Markval(jl_any_func);
    GC_Markval(jl_an_empty_string);
    GC_Markval(jl_an_empty_cell);
    GC_Markval(jl_exception_in_transit);

    // constants
    GC_Markval(jl_null);
    GC_Markval(jl_true);
    GC_Markval(jl_false);

    jl_mark_box_caches();

    size_t i;
    for(i=0; i < to_finalize.len; i++) {
        GC_Markval(to_finalize.items[i]);
    }
    for(i=0; i < finalizer_table.size; i+=2) {
        if (finalizer_table.table[i+1] != HT_NOTFOUND) {
            GC_Markval(finalizer_table.table[i+1]);
            jl_value_t *v = finalizer_table.table[i];
            if (!gc_marked(v)) {
                GC_Markval(v);
                schedule_finalization(v);
            }
        }
    }
}

static int is_gc_enabled = 0;

void jl_gc_enable()
{
    is_gc_enabled = 1;
}

void jl_gc_disable()
{
    is_gc_enabled = 0;
}

int jl_gc_is_enabled() { return is_gc_enabled; }

void jl_gc_collect()
{
    if (is_gc_enabled) {
        gc_mark();
        gc_sweep();
        run_finalizers(); 
    }
    allocd_bytes = 0;
}

void *allocb(size_t sz)
{
    if (allocd_bytes > collect_interval)
        jl_gc_collect();
    allocd_bytes += sz;
#ifdef MEMDEBUG
    return alloc_big(sz);
#endif
    if (sz > 2048)
        return alloc_big(sz);
    return pool_alloc(&pools[szclass(sz)]);
}

void *alloc_permanent(size_t sz)
{
    // we need 1 word before to allow marking
    char *ptr = (char*)malloc(sz+sizeof(void*));
    *((uptrint_t*)ptr) = 0;
    return ptr+sizeof(void*);
}

void *alloc_2w()
{
    if (allocd_bytes > collect_interval)
        jl_gc_collect();
    allocd_bytes += (2*sizeof(void*));
#ifdef MEMDEBUG
    return alloc_big(2*sizeof(void*));
#endif
#ifdef BITS64
    return pool_alloc(&pools[2]);
#else
    return pool_alloc(&pools[0]);
#endif
}

void *alloc_3w()
{
    if (allocd_bytes > collect_interval)
        jl_gc_collect();
    allocd_bytes += (3*sizeof(void*));
#ifdef MEMDEBUG
    return alloc_big(3*sizeof(void*));
#endif
#ifdef BITS64
    return pool_alloc(&pools[4]);
#else
    return pool_alloc(&pools[1]);
#endif
}

void *alloc_4w()
{
    if (allocd_bytes > collect_interval)
        jl_gc_collect();
    allocd_bytes += (4*sizeof(void*));
#ifdef MEMDEBUG
    return alloc_big(4*sizeof(void*));
#endif
#ifdef BITS64
    return pool_alloc(&pools[6]);
#else
    return pool_alloc(&pools[2]);
#endif
}

void jl_gc_init()
{
    int szc[N_POOLS] = { 8, 12, 16, 20, 24, 28, 32, 48, 64, 96, 128, 192, 256,
                         384, 512, 768, 1024, 1536, 2048 };
    int i;
    for(i=0; i < N_POOLS; i++) {
        pools[i].osize = szc[i]+sizeof(void*);
        pools[i].pages = NULL;
        pools[i].freelist = NULL;
    }

    htable_new(&finalizer_table, 0);
    arraylist_new(&to_finalize, 0);
}
