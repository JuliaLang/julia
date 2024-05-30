// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc.h"
#include "julia.h"
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

// re-include assert.h without NDEBUG,
// so that we can always use the assert macro in this file
// for use under their respective enable flags
#undef NDEBUG
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// Useful function in debugger to find page metadata
jl_gc_pagemeta_t *jl_gc_page_metadata(void *data)
{
    return page_metadata(data);
}

// Find the memory block in the pool that owns the byte pointed to by p.
// For end of object pointer (which is always the case for pointer to a
// singleton object), this usually returns the same pointer which points to
// the next object but it can also return NULL if the pointer is pointing to
// the end of the page.
JL_DLLEXPORT jl_taggedvalue_t *jl_gc_find_taggedvalue_pool(char *p, size_t *osize_p)
{
    if (!gc_alloc_map_is_set(p))
        // Not in the pool
        return NULL;
    jl_gc_pagemeta_t *meta = page_metadata(p);
    char *page_begin = gc_page_data(p) + GC_PAGE_OFFSET;
    // In the page header
    if (p < page_begin)
        return NULL;
    size_t ofs = p - page_begin;
    int osize = meta->osize;
    // Shouldn't be needed, just in case
    if (osize == 0)
        return NULL;
    char *tag = (char*)p - ofs % osize;
    // Points to an "object" that gets into the next page
    if (tag + osize > gc_page_data(p) + GC_PAGE_SZ)
        return NULL;
    if (osize_p)
        *osize_p = osize;
    return (jl_taggedvalue_t*)tag;
}

// mark verification
#ifdef GC_VERIFY
jl_value_t *lostval = NULL;
static arraylist_t lostval_parents;
static arraylist_t lostval_parents_done;
int gc_verifying;

void add_lostval_parent(jl_value_t *parent)
{
    for(int i = 0; i < lostval_parents_done.len; i++) {
        if ((jl_value_t*)lostval_parents_done.items[i] == parent)
            return;
    }
    for(int i = 0; i < lostval_parents.len; i++) {
        if ((jl_value_t*)lostval_parents.items[i] == parent)
            return;
    }
    arraylist_push(&lostval_parents, parent);
}

/*
 How to debug a missing write barrier :
 (or rather how I do it, if you know of a better way update this)
 First, reproduce it with GC_VERIFY. It does change the allocation profile so if the error
 is rare enough this may not be straightforward. If the backtracking goes well you should know
 which object and which of its slots was written to without being caught by the write
 barrier. Most times this allows you to take a guess. If this type of object is modified
 by C code directly, look for missing jl_gc_wb() on pointer updates. Be aware that there are
 innocent looking functions which allocate (and thus trigger marking) only on special cases.

 If you can't find it, you can try the following :
 - Once you have a completely deterministic program which crashes on gc_verify, the addresses
   should stay constant between different runs (with same binary, same environment ...).
   Do not forget to turn off ASLR (linux: echo 0 > /proc/sys/kernel/randomize_va_space).
   At this point you should be able to run under gdb and use a hw watch to look for writes
   at the exact addr of the slot (use something like watch *slot_addr if *slot_addr == val).
 - If it went well you are now stopped at the exact point the problem is happening.
   Backtraces in JIT'd code won't work for me (but I'm not sure they should) so in that
   case you can try to jl_throw(something) from gdb.
 */
// this does not yet detect missing writes from marked to marked_noesc
// the error is caught at the first long collection
static arraylist_t bits_save[4];

static void gc_clear_mark_page(jl_gc_pagemeta_t *pg, int bits)
{
    jl_ptls_t ptls2 = gc_all_tls_states[pg->thread_n];
    jl_gc_pool_t *pool = &ptls2->heap.norm_pools[pg->pool_n];
    jl_taggedvalue_t *pv = (jl_taggedvalue_t*)(pg->data + GC_PAGE_OFFSET);
    char *lim = (char*)pv + GC_PAGE_SZ - GC_PAGE_OFFSET - pool->osize;
    while ((char*)pv <= lim) {
        if (!gc_verifying)
            arraylist_push(&bits_save[pv->bits.gc], pv);
        pv->bits.gc = bits;
        pv = (jl_taggedvalue_t*)((char*)pv + pool->osize);
    }
}

static void gc_clear_mark_outer(int bits)
{
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        jl_gc_pagemeta_t *pg = jl_atomic_load_relaxed(&ptls2->page_metadata_allocd.bottom);
        while (pg != NULL) {
            gc_clear_mark_page(pg, bits);
            pg = pg->next;
        }
    }
}
// set all mark bits to bits
// record the state of the region and can replay it in restore()
// restore _must_ be called as this will overwrite parts of the
// freelist in pools
static void clear_mark(int bits)
{
    if (!gc_verifying) {
        for (int i = 0; i < 4; i++) {
            bits_save[i].len = 0;
        }
    }
    bigval_t *v;
    for (int i = 0; i < gc_n_threads; i++) {
        v = gc_all_tls_states[i]->heap.big_objects;
        while (v != NULL) {
            void *gcv = &v->header;
            if (!gc_verifying)
                arraylist_push(&bits_save[v->bits.gc], gcv);
            v->bits.gc = bits;
            v = v->next;
        }
    }

    v = big_objects_marked;
    while (v != NULL) {
        void *gcv = &v->header;
        if (!gc_verifying)
            arraylist_push(&bits_save[v->bits.gc], gcv);
        v->bits.gc = bits;
        v = v->next;
    }

    gc_clear_mark_outer(bits);
}

static void restore(void)
{
    for (int b = 0; b < 4; b++) {
        for (int i = 0; i < bits_save[b].len; i++) {
            ((jl_taggedvalue_t*)bits_save[b].items[i])->bits.gc = b;
        }
    }
}

static void gc_verify_track(jl_ptls_t ptls)
{
    // `gc_verify_track` is limited to single-threaded GC
    if (jl_n_gcthreads != 0)
        return;
    do {
        jl_gc_markqueue_t mq;
        jl_gc_markqueue_t *mq2 = &ptls->mark_queue;
        ws_queue_t *cq = &mq.chunk_queue;
        ws_queue_t *q = &mq.ptr_queue;
        jl_atomic_store_relaxed(&cq->top, 0);
        jl_atomic_store_relaxed(&cq->bottom, 0);
        jl_atomic_store_relaxed(&cq->array, jl_atomic_load_relaxed(&mq2->chunk_queue.array));
        jl_atomic_store_relaxed(&q->top, 0);
        jl_atomic_store_relaxed(&q->bottom, 0);
        jl_atomic_store_relaxed(&q->array, jl_atomic_load_relaxed(&mq2->ptr_queue.array));
        arraylist_new(&mq.reclaim_set, 32);
        arraylist_push(&lostval_parents_done, lostval);
        jl_safe_printf("Now looking for %p =======\n", lostval);
        clear_mark(GC_CLEAN);
        gc_mark_queue_all_roots(ptls, &mq);
        gc_mark_finlist(&mq, &to_finalize, 0);
        for (int i = 0; i < gc_n_threads;i++) {
            jl_ptls_t ptls2 = gc_all_tls_states[i];
            gc_mark_finlist(&mq, &ptls2->finalizers, 0);
        }
        gc_mark_finlist(&mq, &finalizer_list_marked, 0);
        gc_mark_loop_serial_(ptls, &mq);
        if (lostval_parents.len == 0) {
            jl_safe_printf("Could not find the missing link. We missed a toplevel root. This is odd.\n");
            break;
        }
        jl_value_t *lostval_parent = NULL;
        for(int i = 0; i < lostval_parents.len; i++) {
            lostval_parent = (jl_value_t*)lostval_parents.items[i];
            int clean_len = bits_save[GC_CLEAN].len;
            for(int j = 0; j < clean_len + bits_save[GC_OLD].len; j++) {
                void *p = bits_save[j >= clean_len ? GC_OLD : GC_CLEAN].items[j >= clean_len ? j - clean_len : j];
                if (jl_valueof(p) == lostval_parent) {
                    lostval = lostval_parent;
                    lostval_parent = NULL;
                    break;
                }
            }
            if (lostval_parent != NULL) break;
        }
        if (lostval_parent == NULL) { // all parents of lostval were also scheduled for deletion
            lostval = (jl_value_t*)arraylist_pop(&lostval_parents);
        }
        else {
            jl_safe_printf("Missing write barrier found !\n");
            jl_safe_printf("%p was written a reference to %p that was not recorded\n", lostval_parent, lostval);
            jl_safe_printf("(details above)\n");
            lostval = NULL;
        }
        restore();
    } while(lostval != NULL);
}

void gc_verify(jl_ptls_t ptls)
{
    // `gc_verify` is limited to single-threaded GC
    if (jl_n_gcthreads != 0) {
        jl_safe_printf("Warn. GC verify disabled in multi-threaded GC\n");
        return;
    }
    jl_gc_markqueue_t mq;
    jl_gc_markqueue_t *mq2 = &ptls->mark_queue;
    ws_queue_t *cq = &mq.chunk_queue;
    ws_queue_t *q = &mq.ptr_queue;
    jl_atomic_store_relaxed(&cq->top, 0);
    jl_atomic_store_relaxed(&cq->bottom, 0);
    jl_atomic_store_relaxed(&cq->array, jl_atomic_load_relaxed(&mq2->chunk_queue.array));
    jl_atomic_store_relaxed(&q->top, 0);
    jl_atomic_store_relaxed(&q->bottom, 0);
    jl_atomic_store_relaxed(&q->array, jl_atomic_load_relaxed(&mq2->ptr_queue.array));
    arraylist_new(&mq.reclaim_set, 32);
    lostval = NULL;
    lostval_parents.len = 0;
    lostval_parents_done.len = 0;
    clear_mark(GC_CLEAN);
    gc_verifying = 1;
    gc_mark_queue_all_roots(ptls, &mq);
    gc_mark_finlist(&mq, &to_finalize, 0);
    for (int i = 0; i < gc_n_threads;i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        gc_mark_finlist(&mq, &ptls2->finalizers, 0);
    }
    gc_mark_finlist(&mq, &finalizer_list_marked, 0);
    gc_mark_loop_serial_(ptls, &mq);
    int clean_len = bits_save[GC_CLEAN].len;
    for(int i = 0; i < clean_len + bits_save[GC_OLD].len; i++) {
        jl_taggedvalue_t *v = (jl_taggedvalue_t*)bits_save[i >= clean_len ? GC_OLD : GC_CLEAN].items[i >= clean_len ? i - clean_len : i];
        if (gc_marked(v->bits.gc)) {
            jl_safe_printf("Error. Early free of %p type :", v);
            jl_(jl_typeof(jl_valueof(v)));
            jl_safe_printf("val : ");
            jl_(jl_valueof(v));
            jl_safe_printf("Let's try to backtrack the missing write barrier :\n");
            lostval = jl_valueof(v);
            break;
        }
    }
    if (lostval == NULL) {
        gc_verifying = 0;
        restore();  // we did not miss anything
        return;
    }
    restore();
    gc_verify_track(ptls);
    jl_gc_debug_print_status();
    jl_gc_debug_critical_error();
    abort();
}
#endif

#ifdef MEMFENCE
static uint8_t freelist_map[GC_PAGE_SZ / sizeof(void*) / 8];
static int freelist_zerod;

static void gc_verify_tags_page(jl_gc_pagemeta_t *pg)
{
    // for all pages in use
    int p_n = pg->pool_n;
    int t_n = pg->thread_n;
    jl_ptls_t ptls2 = gc_all_tls_states[t_n];
    jl_gc_pool_t *p = &ptls2->heap.norm_pools[p_n];
    int osize = pg->osize;
    char *data = pg->data;
    char *page_begin = data + GC_PAGE_OFFSET;
    jl_taggedvalue_t *v = (jl_taggedvalue_t*)page_begin;
    char *lim = data + GC_PAGE_SZ - osize;
    // reset the freelist map to zero
    if (!freelist_zerod) {
        memset(freelist_map, 0, sizeof(freelist_map));
        freelist_zerod = 1;
    }
    // check for p in new newpages list
    jl_taggedvalue_t *halfpages = p->newpages;
    if (halfpages) {
        char *cur_page = gc_page_data((char*)halfpages - 1);
        if (cur_page == data) {
            lim = (char*)halfpages - 1;
        }
    }
    // compute the freelist_map
    if (pg->nfree) {
        jl_taggedvalue_t *next = NULL;
        if (gc_page_data(p->freelist) == data) {
            // currently allocating on this page
            next = p->freelist;
            assert(page_metadata(next)->osize == osize);
            freelist_zerod = 0;
        }
        else if (pg->fl_begin_offset != (uint16_t)-1) {
            // part of free list exists on this page
            next = page_pfl_beg(pg);
            freelist_zerod = 0;
        }
        assert(halfpages || next);
        while (gc_page_data(next) == data) {
            int obj_idx = (((char*)next) - page_begin) / sizeof(void*);
            freelist_map[obj_idx / 8] |= 1 << (obj_idx % 7);
            next = next->next;
        }
    }
    // validate all of the tags on the page
    while ((char*)v <= lim) {
        int obj_idx = (((char*)v) - page_begin) / sizeof(void*);
        int in_freelist = freelist_map[obj_idx / 8] & (1 << (obj_idx % 7));
        if (!in_freelist) {
            jl_value_t *dt = jl_typeof(jl_valueof(v));
            if (dt != (jl_value_t*)jl_buff_tag &&
                    // the following may be use (by the deserializer) to invalidate objects
                    v->header != 0xf10 && v->header != 0xf20 &&
                    v->header != 0xf30 && v->header != 0xf40 &&
                    v->header != 0xf50 && v->header != 0xf60) {
                assert(jl_typeof(dt) == (jl_value_t*)jl_datatype_type);
            }
        }
        v = (jl_taggedvalue_t*)((char*)v + osize);
    }
}

static void gc_verify_tags_pagestack(void)
{
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        jl_gc_page_stack_t *pgstk = &ptls2->page_metadata_allocd;
        jl_gc_pagemeta_t *pg = jl_atomic_load_relaxed(&pgstk->bottom);
        while (pg != NULL) {
            gc_verify_tags_page(pg);
            pg = pg->next;
        }
    }
}

void gc_verify_tags(void)
{
    // verify the freelist chains look valid
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        for (int i = 0; i < JL_GC_N_POOLS; i++) {
            // for all pools, iterate its freelist
            jl_gc_pool_t *p = &ptls2->heap.norm_pools[i];
            jl_taggedvalue_t *next = p->freelist;
            jl_taggedvalue_t *last = NULL;
            char *allocating = gc_page_data(next);
            while (next) {
                // and assert that the freelist values aren't gc-marked
                assert(next->bits.gc == 0);
                // TODO: verify they are ordered and on the right byte boundaries
                if (gc_page_data(next) != gc_page_data(last)) {
                    // and verify that the chain looks valid
                    jl_gc_pagemeta_t *pg = page_metadata(next);
                    assert(pg->osize == p->osize);
                    if (gc_page_data(next) != allocating) {
                        // when not currently allocating on this page, fl_begin_offset should be correct
                        assert(next == page_pfl_beg(pg));
                    }
                }
                last = next;
                next = next->next;
            }
        }
    }

    // verify that all the objects on every page are either valid julia objects
    // or are part of the freelist or are on the allocated half of a page
    gc_verify_tags_pagestack();
}
#endif

#ifdef GC_DEBUG_ENV
JL_DLLEXPORT jl_gc_debug_env_t jl_gc_debug_env = {
    0, 0,
    {0, UINT64_MAX, 0, 0, 0, {0, 0, 0}},
    {0, UINT64_MAX, 0, 0, 0, {0, 0, 0}},
    {0, UINT64_MAX, 0, 0, 0, {0, 0, 0}}
};

static void gc_debug_alloc_setnext(jl_alloc_num_t *num)
{
    uint64_t interv = num->interv;
    if (num->random[0] && num->interv != 1) {
        // Randomly trigger GC with the same average frequency
        double scale = log(1.0 + 1.0 / (double)(num->interv - 1));
        double randinterv = floor(fabs(log(erand48(num->random))) / scale) + 1;
        interv = randinterv >= UINT64_MAX ? UINT64_MAX : (uint64_t)randinterv;
    }
    uint64_t next = num->num + interv;
    if (!num->interv || next > num->max || interv > next)
        next = UINT64_MAX;
    num->next = next;
}

static void gc_debug_alloc_init(jl_alloc_num_t *num, const char *name)
{
    static const char *fmt = "JULIA_GC_ALLOC_%s";
    char *buff = (char*)alloca(strlen(fmt) + strlen(name) + 1);
    sprintf(buff, fmt, name);
    char *env = getenv(buff);
    if (!env || !*env)
        return;
    if (*env == 'r') {
        env++;
        for (int i = 0; i < 3; i++) {
            while (num->random[i] == 0) {
                num->random[i] = jl_rand();
            }
        }
    }
    num->interv = 1;
    num->max = UINT64_MAX;
    sscanf(env, "%" SCNd64 ":%" SCNd64 ":%" SCNd64,
           (int64_t*)&num->min, (int64_t*)&num->interv, (int64_t*)&num->max);
    if (num->interv == 0)
        num->interv = 1;
    num->next = num->min;
}

static int gc_debug_alloc_check(jl_alloc_num_t *num)
{
    if (++num->num < num->next)
        return 0;
    gc_debug_alloc_setnext(num);
    return 1;
}

int gc_debug_check_pool(void)
{
    return gc_debug_alloc_check(&jl_gc_debug_env.pool);
}

int jl_gc_debug_check_other(void)
{
    return gc_debug_alloc_check(&jl_gc_debug_env.other);
}

void jl_gc_debug_print_status(void) JL_NOTSAFEPOINT
{
    uint64_t pool_count = jl_gc_debug_env.pool.num;
    uint64_t other_count = jl_gc_debug_env.other.num;
    jl_safe_printf("Allocations: %" PRIu64 " "
                   "(Pool: %" PRIu64 "; Other: %" PRIu64 "); GC: %d\n",
                   pool_count + other_count, pool_count, other_count, gc_num.pause);
}

void jl_gc_debug_critical_error(void) JL_NOTSAFEPOINT
{
    jl_gc_debug_print_status();
    if (!jl_gc_debug_env.wait_for_debugger)
        return;
    jl_safe_printf("Waiting for debugger to attach\n");
    while (1) {
        sleep(1000);
    }
}

void jl_gc_debug_print(void)
{
    if (!gc_debug_alloc_check(&jl_gc_debug_env.print))
        return;
    jl_gc_debug_print_status();
}

// a list of tasks for conservative stack scan during gc_scrub
static arraylist_t jl_gc_debug_tasks;

void gc_scrub_record_task(jl_task_t *t)
{
    arraylist_push(&jl_gc_debug_tasks, t);
}

JL_NO_ASAN static void gc_scrub_range(char *low, char *high)
{
    jl_jmp_buf *old_buf = jl_get_safe_restore();
    jl_jmp_buf buf;
    if (jl_setjmp(buf, 0)) {
        jl_set_safe_restore(old_buf);
        return;
    }
    jl_set_safe_restore(&buf);
    low = (char*)((uintptr_t)low & ~(uintptr_t)15);
    for (char **stack_p = ((char**)high) - 1; stack_p > (char**)low; stack_p--) {
        char *p = *stack_p;
        size_t osize;
        jl_taggedvalue_t *tag = jl_gc_find_taggedvalue_pool(p, &osize);
        if (osize <= sizeof(jl_taggedvalue_t) || !tag || gc_marked(tag->bits.gc))
            continue;
        jl_gc_pagemeta_t *pg = page_metadata(tag);
        // Make sure the sweep rebuild the freelist
        pg->has_marked = 1;
        pg->has_young = 1;
        memset(tag, 0xff, osize);
        // set mark to GC_MARKED (young and marked)
        tag->bits.gc = GC_MARKED;
    }
    jl_set_safe_restore(old_buf);
}

static void gc_scrub_task(jl_task_t *ta)
{
    int16_t tid = ta->tid;
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_ptls_t ptls2 = NULL;
    if (tid != -1)
        ptls2 = gc_all_tls_states[tid];

    char *low;
    char *high;
    if (ta->copy_stack && ptls2 && ta == jl_atomic_load_relaxed(&ptls2->current_task)) {
        low  = (char*)ptls2->stackbase - ptls2->stacksize;
        high = (char*)ptls2->stackbase;
    }
    else if (ta->stkbuf) {
        low  = (char*)ta->stkbuf;
        high = (char*)ta->stkbuf + ta->bufsz;
    }
    else
        return;

    if (ptls == ptls2 && ptls2 && ta == jl_atomic_load_relaxed(&ptls2->current_task)) {
        // scan up to current `sp` for current thread and task
        low = (char*)jl_get_frame_addr();
    }
    gc_scrub_range(low, high);
}

void gc_scrub(void)
{
    for (size_t i = 0; i < jl_gc_debug_tasks.len; i++)
        gc_scrub_task((jl_task_t*)jl_gc_debug_tasks.items[i]);
    jl_gc_debug_tasks.len = 0;
}
#else
void jl_gc_debug_critical_error(void)
{
}

void jl_gc_debug_print_status(void)
{
    // May not be accurate but should be helpful enough
    uint64_t pool_count = gc_num.poolalloc;
    uint64_t big_count = gc_num.bigalloc;
    jl_safe_printf("Allocations: %" PRIu64 " "
                   "(Pool: %" PRIu64 "; Big: %" PRIu64 "); GC: %d\n",
                   pool_count + big_count, pool_count, big_count, gc_num.pause);
}
#endif

#ifdef OBJPROFILE
static htable_t obj_counts[3];
static htable_t obj_sizes[3];
void objprofile_count(void *ty, int old, int sz)
{
    if (gc_verifying) return;
    if ((intptr_t)ty <= 0x10) {
        ty = (void*)jl_buff_tag;
    }
    else if (ty != (void*)jl_buff_tag && ty != jl_malloc_tag &&
             jl_is_datatype(ty) && jl_is_datatype_singleton((jl_datatype_t*)ty)) {
        ty = jl_singleton_tag;
    }
    void **bp = ptrhash_bp(&obj_counts[old], ty);
    if (*bp == HT_NOTFOUND)
        *bp = (void*)2;
    else
        (*((intptr_t*)bp))++;
    bp = ptrhash_bp(&obj_sizes[old], ty);
    if (*bp == HT_NOTFOUND)
        *bp = (void*)(intptr_t)(1 + sz);
    else
        *((intptr_t*)bp) += sz;
}

void objprofile_reset(void)
{
    for (int g = 0; g < 3; g++) {
        htable_reset(&obj_counts[g], 0);
        htable_reset(&obj_sizes[g], 0);
    }
}

static void objprofile_print(htable_t nums, htable_t sizes)
{
    for(int i=0; i < nums.size; i+=2) {
        if (nums.table[i+1] != HT_NOTFOUND) {
            void *ty = nums.table[i];
            int num = (intptr_t)nums.table[i + 1] - 1;
            size_t sz = (uintptr_t)ptrhash_get(&sizes, ty) - 1;
            static const int ptr_hex_width = 2 * sizeof(void*);
            if (sz > 2e9) {
                jl_safe_printf(" %6d : %*.1f GB of (%*p) ",
                               num, 6, ((double)sz) / 1024 / 1024 / 1024,
                               ptr_hex_width, ty);
            }
            else if (sz > 2e6) {
                jl_safe_printf(" %6d : %*.1f MB of (%*p) ",
                               num, 6, ((double)sz) / 1024 / 1024,
                               ptr_hex_width, ty);
            }
            else if (sz > 2e3) {
                jl_safe_printf(" %6d : %*.1f kB of (%*p) ",
                               num, 6, ((double)sz) / 1024,
                               ptr_hex_width, ty);
            }
            else {
                jl_safe_printf(" %6d : %*d  B of (%*p) ",
                          num, 6, (int)sz, ptr_hex_width, ty);
            }
            if (ty == (void*)jl_buff_tag)
                jl_safe_printf("#<buffer>");
            else if (ty == jl_malloc_tag)
                jl_safe_printf("#<malloc>");
            else if (ty == jl_singleton_tag)
                jl_safe_printf("#<singletons>");
            else
                jl_static_show(JL_STDERR, (jl_value_t*)ty);
            jl_safe_printf("\n");
        }
    }
}

void objprofile_printall(void)
{
    jl_safe_printf("Transient mark :\n");
    objprofile_print(obj_counts[0], obj_sizes[0]);
    jl_safe_printf("Perm mark :\n");
    objprofile_print(obj_counts[1], obj_sizes[1]);
    jl_safe_printf("Remset :\n");
    objprofile_print(obj_counts[2], obj_sizes[2]);
}
#endif

#if defined(GC_TIME) || defined(GC_FINAL_STATS)
STATIC_INLINE double jl_ns2ms(int64_t t)
{
    return t / (double)1e6;
}

STATIC_INLINE double jl_ns2s(int64_t t)
{
    return t / (double)1e9;
}

static uint64_t gc_premark_end;
static uint64_t gc_postmark_end;
void gc_settime_premark_end(void)
{
    gc_premark_end = jl_hrtime();
}
void gc_settime_postmark_end(void)
{
    gc_postmark_end = jl_hrtime();
}
#endif

#ifdef GC_FINAL_STATS
#ifdef _OS_LINUX_
#include <malloc.h> // for mallinfo
#endif
static double process_t0;
static size_t max_pg_count = 0;
static size_t total_freed_bytes = 0;
static uint64_t max_pause = 0;
static uint64_t total_sweep_time = 0;
static uint64_t total_mark_time = 0;
static uint64_t total_fin_time = 0;

void gc_final_count_page(size_t pg_cnt)
{
    if (pg_cnt > max_pg_count) {
        max_pg_count = pg_cnt;
    }
}

void gc_final_pause_end(int64_t t0, int64_t tend)
{
    uint64_t post_time = gc_postmark_end - gc_premark_end;
    uint64_t sweep_pause = tend - gc_premark_end;
    uint64_t pause = tend - t0;
    total_freed_bytes += gc_num.freed;
    total_sweep_time += sweep_pause - post_time;
    total_fin_time += post_time;
    max_pause = max_pause < pause ? pause : max_pause;
    total_mark_time += gc_premark_end - t0;
}

static void gc_stats_pagetable0(pagetable0_t *pagetable0, unsigned *p0)
{
    for (int pg_i = 0; pg_i < REGION0_PG_COUNT; pg_i++) {
        uint8_t meta = pagetable0->meta[pg_i];
        assert(meta == GC_PAGE_UNMAPPED || meta == GC_PAGE_ALLOCATED ||
               meta == GC_PAGE_LAZILY_FREED || meta == GC_PAGE_FREED);
        if (meta != GC_PAGE_UNMAPPED) {
            (*p0)++;
        }
    }
}

static void gc_stats_pagetable1(pagetable1_t *pagetable1, unsigned *p1, unsigned *p0)
{
    for (int pg_i = 0; pg_i < REGION1_PG_COUNT; pg_i++) {
        pagetable0_t *pagetable0 = pagetable1->meta0[pg_i];
        if (pagetable0 == NULL) {
            continue;
        }
        (*p1)++;
        gc_stats_pagetable0(pagetable0, p0);
    }
}

static void gc_stats_pagetable(unsigned *p2, unsigned *p1, unsigned *p0)
{
    for (int pg_i = 0; pg_i < REGION2_PG_COUNT; pg_i++) {
        pagetable1_t *pagetable1 = alloc_map.meta1[pg_i];
        if (pagetable1 == NULL) {
            continue;
        }
        (*p2)++;
        gc_stats_pagetable1(pagetable1, p1, p0);
    }
}

void jl_print_gc_stats(JL_STREAM *s)
{
#ifdef _OS_LINUX_
    malloc_stats();
#endif
    double ptime = jl_hrtime() - process_t0;
    double exec_time = jl_ns2s(ptime);
    jl_safe_printf("exec time\t%.5f sec\n", exec_time);
    if (gc_num.pause > 0) {
        jl_safe_printf("gc time  \t%.5f sec (%2.1f%%) in %d (%d full) collections\n",
                       jl_ns2s(gc_num.total_time),
                       jl_ns2s(gc_num.total_time) / exec_time * 100,
                       gc_num.pause, gc_num.full_sweep);
        jl_safe_printf("gc pause \t%.2f ms avg\n\t\t%2.0f ms max\n",
                       jl_ns2ms(gc_num.total_time) / gc_num.pause,
                       jl_ns2ms(max_pause));
        jl_safe_printf("\t\t(%2d%% mark, %2d%% sweep, %2d%% finalizers)\n",
                       (int)(total_mark_time * 100 / gc_num.total_time),
                       (int)(total_sweep_time * 100 / gc_num.total_time),
                       (int)(total_fin_time * 100 / gc_num.total_time));
    }
    unsigned p2 = 0, p1 = 0, p0 = 0;
    gc_stats_pagetable(&p2, &p1, &p0);
    jl_safe_printf("page table max utilization : %u (%.1f%%) - %u (%.1f%%) - %u (%.1f%%)\n",
                   p2, p2 * 100.0 / REGION2_PG_COUNT,
                   p1, p1 * 100.0 / REGION1_PG_COUNT / p2,
                   p0, p0 * 100.0 / REGION0_PG_COUNT / p1);
#ifdef _OS_LINUX_
    double gct = gc_num.total_time / 1e9;
    struct mallinfo mi = mallinfo();
    jl_safe_printf("malloc size\t%d MB\n", mi.uordblks / 1024 / 1024);
    jl_safe_printf("max page alloc\t%ld MB\n", max_pg_count * GC_PAGE_SZ / 1024 / 1024);
    jl_safe_printf("total freed\t%" PRIuPTR " b\n", total_freed_bytes);
    jl_safe_printf("free rate\t%.1f MB/sec\n", (total_freed_bytes / gct) / 1024 / 1024);
#endif
}
#else
void jl_print_gc_stats(JL_STREAM *s)
{
}
#endif

#ifdef GC_TIME
static int64_t skipped_pages = 0;
static int64_t total_pages = 0;
static int64_t freed_pages = 0;
static int64_t pool_sweep_start;

void gc_time_pool_start(void)
{
    skipped_pages = 0;
    total_pages = 0;
    freed_pages = 0;
    pool_sweep_start = jl_hrtime();
}

void gc_time_count_page(int freedall, int pg_skpd)
{
    freed_pages += freedall;
    skipped_pages += pg_skpd;
    total_pages++;
}

void gc_time_pool_end(int sweep_full)
{
    double sweep_pool_sec = (jl_hrtime() - pool_sweep_start) / 1e9;
    double sweep_gb = total_pages * GC_PAGE_SZ / (double)(1024 * 1024 * 1024);
    double sweep_speed = sweep_gb / sweep_pool_sec;
    jl_safe_printf("GC sweep pools end %.2f ms at %.1f GB/s "
                   "(skipped %.2f %% of %" PRId64 ", swept %" PRId64 " pgs, "
                   "%" PRId64 " freed) %s\n",
                   sweep_pool_sec * 1000, sweep_speed,
                   (total_pages ? ((double)skipped_pages * 100) / total_pages : 0),
                   total_pages, total_pages - skipped_pages,
                   freed_pages,
                   sweep_full ? "full" : "quick");
}

void gc_time_sysimg_end(uint64_t t0)
{
    double sweep_pool_sec = (jl_hrtime() - t0) / 1e9;
    jl_safe_printf("GC sweep sysimg end %.2f ms\n",
                   sweep_pool_sec * 1000);
}

static int64_t big_total;
static int64_t big_freed;
static int64_t big_reset;
static int64_t big_sweep_start;

void gc_time_big_start(void)
{
    big_total = 0;
    big_freed = 0;
    big_reset = 0;
    big_sweep_start = jl_hrtime();
}

void gc_time_count_big(int old_bits, int bits)
{
    big_total++;
    big_reset += bits == GC_CLEAN;
    big_freed += !gc_marked(old_bits);
}

void gc_time_big_end(void)
{
    double t_ms = jl_ns2ms(jl_hrtime() - big_sweep_start);
    jl_safe_printf("GC sweep big %.2f ms "
                   "(freed %" PRId64 " / %" PRId64 " with %" PRId64 " rst)\n",
                   t_ms, big_freed, big_total, big_reset);
}

static int64_t mallocd_memory_total;
static int64_t mallocd_memory_freed;
static int64_t mallocd_memory_sweep_start;

void gc_time_mallocd_memory_start(void)
{
    mallocd_memory_total = 0;
    mallocd_memory_freed = 0;
    mallocd_memory_sweep_start = jl_hrtime();
}

void gc_time_count_mallocd_memory(int bits)
{
    mallocd_memory_total++;
    mallocd_memory_freed += !gc_marked(bits);
}

void gc_time_mallocd_memory_end(void)
{
    double t_ms = jl_ns2ms(jl_hrtime() - mallocd_memory_sweep_start);
    jl_safe_printf("GC sweep arrays %.2f ms "
                   "(freed %" PRId64 " / %" PRId64 ")\n",
                   t_ms, mallocd_memory_freed, mallocd_memory_total);
}

void gc_time_mark_pause(int64_t t0, int64_t scanned_bytes,
                        int64_t perm_scanned_bytes)
{
    int64_t last_remset_len = 0;
    int64_t remset_nptr = 0;
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        last_remset_len += ptls2->heap.last_remset->len;
        remset_nptr = ptls2->heap.remset_nptr;
    }
    jl_safe_printf("GC mark pause %.2f ms | "
                   "scanned %" PRId64 " kB = %" PRId64 " + %" PRId64 " | "
                   "remset %" PRId64 " %" PRId64 "\n",
                   jl_ns2ms(gc_premark_end - t0),
                   (scanned_bytes + perm_scanned_bytes) / 1024,
                   scanned_bytes / 1024, perm_scanned_bytes / 1024,
                   last_remset_len, remset_nptr);
}

void gc_time_sweep_pause(uint64_t gc_end_t, int64_t actual_allocd,
                         int64_t live_bytes, int64_t estimate_freed,
                         int sweep_full)
{
    uint64_t sweep_pause = gc_end_t - gc_premark_end;
    int pct = actual_allocd ? (gc_num.freed * 100) / actual_allocd : -1;
    jl_safe_printf("GC sweep pause %.2f ms live %" PRId64 " kB "
                   "(freed %" PRId64 " kB EST %" PRId64 " kB "
                   "[error %" PRId64 "] = %d%% of allocd b %" PRIu64 ") "
                   "(%.2f ms in post_mark) %s\n",
                   jl_ns2ms(sweep_pause), live_bytes / 1024,
                   gc_num.freed / 1024, estimate_freed / 1024,
                   gc_num.freed - estimate_freed, pct, gc_num.allocd / 1024,
                   jl_ns2ms(gc_postmark_end - gc_premark_end),
                   sweep_full ? "full" : "quick");
}

void gc_time_summary(int sweep_full, uint64_t start, uint64_t end,
                     uint64_t freed, uint64_t live, uint64_t interval,
                     uint64_t pause, uint64_t ttsp, uint64_t mark,
                     uint64_t sweep)
{
    if (sweep_full > 0)
        jl_safe_printf("TS: %" PRIu64 " Major collection: estimate freed = %" PRIu64
                       " live = %" PRIu64 "m new interval = %" PRIu64
                       "m time = %" PRIu64 "ms ttsp = %" PRIu64 "us mark time = %"
                       PRIu64 "ms sweep time = %" PRIu64 "ms \n",
                       end, freed, live/1024/1024,
                       interval/1024/1024, pause/1000000, ttsp,
                       mark/1000000,sweep/1000000);
    else
        jl_safe_printf("TS: %" PRIu64 " Minor collection: estimate freed = %" PRIu64
                       " live = %" PRIu64 "m new interval = %" PRIu64 "m pause time = %"
                       PRIu64 "ms ttsp = %" PRIu64 "us mark time = %" PRIu64
                       "ms sweep time = %" PRIu64 "ms\n",
                       end, freed, live/1024/1024,
                       interval/1024/1024, pause/1000000, ttsp,
                       mark/1000000,sweep/1000000);
}

void gc_heuristics_summary(
        uint64_t old_alloc_diff, uint64_t alloc_mem,
        uint64_t old_mut_time, uint64_t alloc_time,
        uint64_t old_freed_diff, uint64_t gc_mem,
        uint64_t old_pause_time, uint64_t gc_time,
        int thrash_counter, const char *reason,
        uint64_t current_heap, uint64_t target_heap)
{
    jl_safe_printf("Estimates: alloc_diff=%" PRIu64 "kB (%" PRIu64 ")"
                            //"  nongc_time=%" PRIu64 "ns (%" PRIu64 ")"
                            "  mut_time=%" PRIu64 "ns (%" PRIu64 ")"
                            "  freed_diff=%" PRIu64 "kB (%" PRIu64 ")"
                            "  pause_time=%" PRIu64 "ns (%" PRIu64 ")"
                            "  thrash_counter=%d%s"
                            "  current_heap=%" PRIu64 " MB"
                            "  target_heap=%" PRIu64 " MB\n",
                   old_alloc_diff/1024, alloc_mem/1024,
                   old_mut_time/1000, alloc_time/1000,
                   old_freed_diff/1024, gc_mem/1024,
                   old_pause_time/1000, gc_time/1000,
                   thrash_counter, reason,
                   current_heap/1024/1024, target_heap/1024/1024);
}
#endif

void jl_gc_debug_init(void)
{
#ifdef GC_DEBUG_ENV
    char *env = getenv("JULIA_GC_NO_GENERATIONAL");
    if (env && strcmp(env, "0") != 0)
        jl_gc_debug_env.always_full = 1;
    env = getenv("JULIA_GC_WAIT_FOR_DEBUGGER");
    jl_gc_debug_env.wait_for_debugger = env && strcmp(env, "0") != 0;
    gc_debug_alloc_init(&jl_gc_debug_env.pool, "POOL");
    gc_debug_alloc_init(&jl_gc_debug_env.other, "OTHER");
    gc_debug_alloc_init(&jl_gc_debug_env.print, "PRINT");
    arraylist_new(&jl_gc_debug_tasks, 0);
#endif

#ifdef GC_VERIFY
    for (int i = 0; i < 4; i++)
        arraylist_new(&bits_save[i], 0);
    arraylist_new(&lostval_parents, 0);
    arraylist_new(&lostval_parents_done, 0);
#endif

#ifdef OBJPROFILE
    for (int g = 0; g < 3; g++) {
        htable_new(&obj_counts[g], 0);
        htable_new(&obj_sizes[g], 0);
    }
#endif

#ifdef GC_FINAL_STATS
    process_t0 = jl_hrtime();
#endif
}

// GC summary stats

#ifdef MEMPROFILE
// TODO repair this and possibly merge with `gc_count_pool`
static size_t pool_stats(jl_gc_pool_t *p, size_t *pwaste, size_t *np,
                         size_t *pnold)
{
    jl_taggedvalue_t *halfpages = p->newpages;
    size_t osize = p->osize;
    size_t nused=0, nfree=0, npgs=0, nold=0;

    if (halfpages != NULL) {
        npgs++;
        char *v = gc_page_data(halfpages) + GC_PAGE_OFFSET;
        char *lim = (char*)halfpages - 1;
        int i = 0;
        while (v <= lim) {
            if (!gc_marked(((jl_taggedvalue_t*)v)->bits.gc)) {
                nfree++;
            }
            else {
                nused++;
                if (((jl_taggedvalue_t*)v)->bits.gc == GC_OLD_MARKED) {
                    nold++;
                }
            }
            v = v + osize;
            i++;
        }
        // only the first page is allocated on
    }
    *pwaste = npgs * GC_PAGE_SZ - (nused * p->osize);
    *np = npgs;
    *pnold = nold;
    if (npgs != 0) {
        jl_safe_printf("%4d : %7lld/%7lld objects (%3lld%% old), %5lld pages, %5lld kB, %5lld kB waste\n",
                       p->osize,
                       (long long)nused,
                       (long long)(nused + nfree),
                       (long long)(nused ? (nold * 100) / nused : 0),
                       (long long)npgs,
                       (long long)((nused * p->osize) / 1024),
                       (long long)(*pwaste / 1024));
    }
    return nused*p->osize;
}

void gc_stats_all_pool(void)
{
    size_t nb=0, w, tw=0, no=0, tp=0, nold=0, noldbytes=0, np, nol;
    for (int i = 0; i < JL_GC_N_POOLS; i++) {
        for (int t_i = 0; t_i < gc_n_threads; t_i++) {
            jl_ptls_t ptls2 = gc_all_tls_states[t_i];
            size_t b = pool_stats(&ptls2->heap.norm_pools[i], &w, &np, &nol);
            nb += b;
            no += (b / ptls2->heap.norm_pools[i].osize);
            tw += w;
            tp += np;
            nold += nol;
            noldbytes += nol * ptls2->heap.norm_pools[i].osize;
        }
    }
    jl_safe_printf("%lld objects (%lld%% old), %lld kB (%lld%% old) total allocated, "
                   "%lld total fragments (%lld%% overhead), in %lld pages\n",
                   (long long)no,
                   (long long)(no ? (nold * 100) / no : 0),
                   (long long)(nb / 1024),
                   (long long)(nb ? (noldbytes * 100) / nb : 0),
                   (long long)tw,
                   (long long)(nb ? (tw * 100) / nb : 0),
                   (long long)tp);
}

void gc_stats_big_obj(void)
{
    size_t nused=0, nbytes=0, nused_old=0, nbytes_old=0;
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        bigval_t *v = ptls2->heap.big_objects;
        while (v != NULL) {
            if (gc_marked(v->bits.gc)) {
                nused++;
                nbytes += v->sz & ~3;
            }
            v = v->next;
        }
        v = big_objects_marked;
        while (v != NULL) {
            if (gc_marked(v->bits.gc)) {
                nused_old++;
                nbytes_old += v->sz & ~3;
            }
            v = v->next;
        }

        mallocarray_t *ma = ptls2->heap.mallocarrays;
        while (ma != NULL) {
            if (gc_marked(jl_astaggedvalue(ma->a)->bits.gc)) {
                nused++;
                nbytes += jl_genericmemory_nbytes((jl_genericmemory_t*)ma->a);
            }
            ma = ma->next;
        }
    }

    jl_safe_printf("%lld kB (%lld%% old) in %lld large objects (%lld%% old)\n",
                   (long long)((nbytes + nbytes_old) / 1024),
                   (long long)(nbytes + nbytes_old ? (nbytes_old * 100) / (nbytes + nbytes_old) : 0),
                   (long long)(nused + nused_old),
                   (long long)(nused + nused_old ? (nused_old * 100) / (nused + nused_old) : 0));
}
#endif //MEMPROFILE

// Simple and dumb way to count cells with different gc bits in allocated pages
// Use as ground truth for debugging memory-leak-like issues.
static int64_t poolobj_sizes[4];
static int64_t empty_pages;

static void gc_count_pool_page(jl_gc_pagemeta_t *pg) JL_NOTSAFEPOINT
{
    int osize = pg->osize;
    char *data = pg->data;
    jl_taggedvalue_t *v = (jl_taggedvalue_t*)(data + GC_PAGE_OFFSET);
    char *lim = (char*)v + GC_PAGE_SZ - GC_PAGE_OFFSET - osize;
    int has_live = 0;
    while ((char*)v <= lim) {
        int bits = v->bits.gc;
        if (gc_marked(bits))
            has_live = 1;
        poolobj_sizes[bits] += osize;
        v = (jl_taggedvalue_t*)((char*)v + osize);
    }
    if (!has_live) {
        empty_pages++;
    }
}

static void gc_count_pool_pagetable(void)
{
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        jl_gc_pagemeta_t *pg = jl_atomic_load_relaxed(&ptls2->page_metadata_allocd.bottom);
        while (pg != NULL) {
            if (gc_alloc_map_is_set(pg->data)) {
                gc_count_pool_page(pg);
            }
            pg = pg->next;
        }
    }
}

void gc_count_pool(void)
{
    memset(&poolobj_sizes, 0, sizeof(poolobj_sizes));
    empty_pages = 0;
    gc_count_pool_pagetable();
    jl_safe_printf("****** Pool stat: ******\n");
    for (int i = 0; i < 4; i++)
        jl_safe_printf("bits(%d): %"  PRId64 "\n", i, poolobj_sizes[i]);
    // empty_pages is inaccurate after the sweep since young objects are
    // also GC_CLEAN
    jl_safe_printf("free pages: % "  PRId64 "\n", empty_pages);
    jl_safe_printf("************************\n");
}

int gc_slot_to_fieldidx(void *obj, void *slot, jl_datatype_t *vt) JL_NOTSAFEPOINT
{
    int nf = (int)jl_datatype_nfields(vt);
    for (int i = 1; i < nf; i++) {
        if (slot < (void*)((char*)obj + jl_field_offset(vt, i)))
            return i - 1;
    }
    return nf - 1;
}

int gc_slot_to_arrayidx(void *obj, void *_slot) JL_NOTSAFEPOINT
{
    char *slot = (char*)_slot;
    jl_datatype_t *vt = (jl_datatype_t*)jl_typeof(obj);
    char *start = NULL;
    size_t len = 0;
    size_t elsize = sizeof(void*);
    if (vt == jl_module_type) {
        jl_module_t *m = (jl_module_t*)obj;
        start = (char*)m->usings.items;
        len = m->usings.len;
    }
    else if (vt == jl_simplevector_type) {
        start = (char*)jl_svec_data(obj);
        len = jl_svec_len(obj);
    }
    if (slot < start || slot >= start + elsize * len)
        return -1;
    return (slot - start) / elsize;
}

static int gc_logging_enabled = 0;

JL_DLLEXPORT void jl_enable_gc_logging(int enable) {
    gc_logging_enabled = enable;
}

JL_DLLEXPORT int jl_is_gc_logging_enabled(void) {
    return gc_logging_enabled;
}

void _report_gc_finished(uint64_t pause, uint64_t freed, int full, int recollect, int64_t live_bytes) JL_NOTSAFEPOINT {
    if (!gc_logging_enabled) {
        return;
    }
    jl_safe_printf("\nGC: pause %.2fms. collected %fMB. %s %s\n",
        pause/1e6, freed/(double)(1<<20),
        full ? "full" : "incr",
        recollect ? "recollect" : ""
    );

    jl_safe_printf("Heap stats: bytes_mapped %.2f MB, bytes_resident %.2f MB,\nheap_size %.2f MB, heap_target %.2f MB, Fragmentation %.3f\n",
        jl_atomic_load_relaxed(&gc_heap_stats.bytes_mapped)/(double)(1<<20),
        jl_atomic_load_relaxed(&gc_heap_stats.bytes_resident)/(double)(1<<20),
        // live_bytes/(double)(1<<20), live byes tracking is not accurate.
        jl_atomic_load_relaxed(&gc_heap_stats.heap_size)/(double)(1<<20),
        jl_atomic_load_relaxed(&gc_heap_stats.heap_target)/(double)(1<<20),
        (double)live_bytes/(double)jl_atomic_load_relaxed(&gc_heap_stats.heap_size)
    );
    // Should fragmentation use bytes_resident instead of heap_size?
}

#ifdef __cplusplus
}
#endif
