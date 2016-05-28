// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "gc.h"
#include <inttypes.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

// Useful function in debugger to find page/region metadata
jl_gc_pagemeta_t *jl_gc_page_metadata(void *data)
{
    return page_metadata(data);
}

region_t *jl_gc_find_region(void *ptr, int maybe)
{
    return find_region(ptr, maybe);
}

// Find the memory block in the pool that owns the byte pointed to by p.
// For end of object pointer (which is always the case for pointer to a
// singleton object), this usually returns the same pointer which points to
// the next object but it can also return NULL if the pointer is pointing to
// the end of the page.
JL_DLLEXPORT jl_taggedvalue_t *jl_gc_find_taggedvalue_pool(char *p, size_t *osize_p)
{
    region_t *r = find_region(p, 1);
    // Not in the pool
    if (!r)
        return NULL;
    char *page_begin = gc_page_data(p) + GC_PAGE_OFFSET;
    // In the page header
    if (p < page_begin)
        return NULL;
    size_t ofs = p - page_begin;
    int pg_idx = page_index(r, page_begin);
    // Check if this is a free page
    if (!(r->allocmap[pg_idx / 32] & (uint32_t)(1 << (pg_idx % 32))))
        return NULL;
    jl_gc_pagemeta_t *pagemeta = &r->meta[pg_idx];
    int osize = pagemeta->osize;
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
static arraylist_t bits_save[4];

// set all mark bits to bits
// record the state of the region and can replay it in restore()
// restore _must_ be called as this will overwrite parts of the
// freelist in pools
static void clear_mark(int bits)
{
    gcval_t *pv;
    if (!gc_verifying) {
        for (int i = 0; i < 4; i++) {
            bits_save[i].len = 0;
        }
    }
    bigval_t *v;
    for (int i = 0;i < jl_n_threads;i++) {
        v = jl_all_tls_states[i]->heap.big_objects;
        while (v != NULL) {
            void *gcv = &v->header;
            if (!gc_verifying) arraylist_push(&bits_save[gc_bits(gcv)], gcv);
            gc_bits(gcv) = bits;
            v = v->next;
        }
    }

    v = big_objects_marked;
    while (v != NULL) {
        void *gcv = &v->header;
        if (!gc_verifying) arraylist_push(&bits_save[gc_bits(gcv)], gcv);
        gc_bits(gcv) = bits;
        v = v->next;
    }

    for (int h = 0; h < REGION_COUNT; h++) {
        region_t *region = &regions[h];
        if (!region->pages)
            break;
        for (int pg_i = 0; pg_i < region->pg_cnt / 32; pg_i++) {
            uint32_t line = region->allocmap[pg_i];
            if (line) {
                for (int j = 0; j < 32; j++) {
                    if ((line >> j) & 1) {
                        jl_gc_pagemeta_t *pg = page_metadata(region->pages[pg_i*32 + j].data + GC_PAGE_OFFSET);
                        jl_tls_states_t *ptls = jl_all_tls_states[pg->thread_n];
                        jl_gc_pool_t *pool = &ptls->heap.norm_pools[pg->pool_n];
                        pv = (gcval_t*)(pg->data + GC_PAGE_OFFSET);
                        char *lim = (char*)pv + GC_PAGE_SZ - GC_PAGE_OFFSET - pool->osize;
                        while ((char*)pv <= lim) {
                            if (!gc_verifying) arraylist_push(&bits_save[gc_bits(pv)], pv);
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
        jl_printf(JL_STDERR, "Now looking for %p =======\n", lostval);
        clear_mark(GC_CLEAN);
        pre_mark();
        post_mark(&finalizer_list, 1);
        post_mark(&finalizer_list_marked, 1);
        if (lostval_parents.len == 0) {
            jl_printf(JL_STDERR, "Could not find the missing link. We missed a toplevel root. This is odd.\n");
            break;
        }
        jl_value_t *lostval_parent = NULL;
        for(int i = 0; i < lostval_parents.len; i++) {
            lostval_parent = (jl_value_t*)lostval_parents.items[i];
            int clean_len = bits_save[GC_CLEAN].len;
            for(int j = 0; j < clean_len + bits_save[GC_QUEUED].len; j++) {
                void *p = bits_save[j >= clean_len ? GC_QUEUED : GC_CLEAN].items[j >= clean_len ? j - clean_len : j];
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
            jl_printf(JL_STDERR, "Missing write barrier found !\n");
            jl_printf(JL_STDERR, "%p was written a reference to %p that was not recorded\n", lostval_parent, lostval);
            jl_printf(JL_STDERR, "(details above)\n");
            lostval = NULL;
        }
        restore();
    } while(lostval != NULL);
}

void gc_verify(void)
{
    lostval = NULL;
    lostval_parents.len = 0;
    lostval_parents_done.len = 0;
    clear_mark(GC_CLEAN);
    gc_verifying = 1;
    pre_mark();
    post_mark(&finalizer_list, 1);
    post_mark(&finalizer_list_marked, 1);
    int clean_len = bits_save[GC_CLEAN].len;
    for(int i = 0; i < clean_len + bits_save[GC_QUEUED].len; i++) {
        gcval_t *v = (gcval_t*)bits_save[i >= clean_len ? GC_QUEUED : GC_CLEAN].items[i >= clean_len ? i - clean_len : i];
        if (gc_marked(v)) {
            jl_printf(JL_STDERR, "Error. Early free of %p type :", v);
            jl_(jl_typeof(jl_valueof(v)));
            jl_printf(JL_STDERR, "val : ");
            jl_(jl_valueof(v));
            jl_printf(JL_STDERR, "Let's try to backtrack the missing write barrier :\n");
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
    gc_verify_track();
    gc_debug_print_status();
    gc_debug_critical_error();
    abort();
}
#endif

#ifdef GC_DEBUG_ENV
JL_DLLEXPORT jl_gc_debug_env_t jl_gc_debug_env = {
    GC_MARKED_NOESC,
    0,
    {0, UINT64_MAX, 0, 0, 0, {0, 0, 0}},
    {0, UINT64_MAX, 0, 0, 0, {0, 0, 0}},
    {0, UINT64_MAX, 0, 0, 0, {0, 0, 0}}
};
static char *gc_stack_lo;

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
        srand((unsigned)uv_hrtime());
        for (int i = 0;i < 3;i++) {
            while (num->random[i] == 0) {
                num->random[i] = rand();
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

int gc_debug_check_other(void)
{
    return gc_debug_alloc_check(&jl_gc_debug_env.other);
}

void gc_debug_print_status(void)
{
    uint64_t pool_count = jl_gc_debug_env.pool.num;
    uint64_t other_count = jl_gc_debug_env.other.num;
    jl_safe_printf("Allocations: %" PRIu64 " "
                   "(Pool: %" PRIu64 "; Other: %" PRIu64 "); GC: %d\n",
                   pool_count + other_count, pool_count, other_count, gc_num.pause);
}

void gc_debug_critical_error(void)
{
    gc_debug_print_status();
    if (!jl_gc_debug_env.wait_for_debugger)
        return;
    jl_safe_printf("Waiting for debugger to attach\n");
    while (1) {
        sleep(1000);
    }
}

void gc_debug_print(void)
{
    if (!gc_debug_alloc_check(&jl_gc_debug_env.print))
        return;
    gc_debug_print_status();
}

static void gc_scrub_range(char *stack_lo, char *stack_hi)
{
    stack_lo = (char*)((uintptr_t)stack_lo & ~(uintptr_t)15);
    for (char **stack_p = (char**)stack_lo;
         stack_p > (char**)stack_hi;stack_p--) {
        char *p = *stack_p;
        size_t osize;
        jl_taggedvalue_t *tag = jl_gc_find_taggedvalue_pool(p, &osize);
        if (osize <= sizeof_jl_taggedvalue_t || !tag || gc_marked(tag))
            continue;
        jl_gc_pagemeta_t *pg = page_metadata(tag);
        // Make sure the sweep rebuild the freelist
        pg->allocd = 1;
        pg->gc_bits = 0x3;
        // Find the age bit
        char *page_begin = gc_page_data(tag) + GC_PAGE_OFFSET;
        int obj_id = (((char*)tag) - page_begin) / osize;
        uint8_t *ages = pg->ages + obj_id / 8;
        // Force this to be a young object to save some memory
        // (especially on 32bit where it's more likely to have pointer-like
        //  bit patterns)
        *ages &= ~(1 << (obj_id % 8));
        memset(tag, 0xff, osize);
    }
}

void gc_scrub(char *stack_hi)
{
    gc_scrub_range(gc_stack_lo, stack_hi);
}
#else
void gc_debug_critical_error(void)
{
}

void gc_debug_print_status(void)
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
             jl_typeof(ty) == (jl_value_t*)jl_datatype_type &&
             ((jl_datatype_t*)ty)->instance) {
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
    for(int g=0; g < 3; g++) {
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
                jl_printf(JL_STDERR, " %6d : %*.1f GB of (%*p) ",
                          num, 6, ((double)sz) / 1024 / 1024 / 1024,
                          ptr_hex_width, ty);
            }
            else if (sz > 2e6) {
                jl_printf(JL_STDERR, " %6d : %*.1f MB of (%*p) ",
                          num, 6, ((double)sz) / 1024 / 1024,
                          ptr_hex_width, ty);
            }
            else if (sz > 2e3) {
                jl_printf(JL_STDERR, " %6d : %*.1f kB of (%*p) ",
                          num, 6, ((double)sz) / 1024,
                          ptr_hex_width, ty);
            }
            else {
                jl_printf(JL_STDERR, " %6d : %*d  B of (%*p) ",
                          num, 6, (int)sz, ptr_hex_width, ty);
            }
            if (ty == (void*)jl_buff_tag)
                jl_printf(JL_STDERR, "#<buffer>");
            else if (ty == jl_malloc_tag)
                jl_printf(JL_STDERR, "#<malloc>");
            else if (ty == jl_singleton_tag)
                jl_printf(JL_STDERR, "#<singletons>");
            else
                jl_static_show(JL_STDERR, (jl_value_t*)ty);
            jl_printf(JL_STDERR, "\n");
        }
    }
}

void objprofile_printall(void)
{
    jl_printf(JL_STDERR, "Transient mark :\n");
    objprofile_print(obj_counts[0], obj_sizes[0]);
    jl_printf(JL_STDERR, "Perm mark :\n");
    objprofile_print(obj_counts[1], obj_sizes[1]);
    jl_printf(JL_STDERR, "Remset :\n");
    objprofile_print(obj_counts[2], obj_sizes[2]);
}
#endif

void gc_debug_init(void)
{
#ifdef GC_DEBUG_ENV
    gc_stack_lo = (char*)gc_get_stack_ptr();
    char *env = getenv("JULIA_GC_NO_GENERATIONAL");
    if (env && strcmp(env, "0") != 0)
        jl_gc_debug_env.sweep_mask = GC_MARKED;
    env = getenv("JULIA_GC_WAIT_FOR_DEBUGGER");
    jl_gc_debug_env.wait_for_debugger = env && strcmp(env, "0") != 0;
    gc_debug_alloc_init(&jl_gc_debug_env.pool, "POOL");
    gc_debug_alloc_init(&jl_gc_debug_env.other, "OTHER");
    gc_debug_alloc_init(&jl_gc_debug_env.print, "PRINT");
#endif

#ifdef GC_VERIFY
    for (int i = 0; i < 4; i++)
        arraylist_new(&bits_save[i], 0);
    arraylist_new(&lostval_parents, 0);
    arraylist_new(&lostval_parents_done, 0);
#endif

#ifdef OBJPROFILE
    for (int g = 0;g < 3;g++) {
        htable_new(&obj_counts[g], 0);
        htable_new(&obj_sizes[g], 0);
    }
#endif
}

// Simple and dumb way to count cells with different gc bits in allocated pages
// Use as ground truth for debugging memory-leak-like issues.
static int64_t poolobj_sizes[4];
static int64_t empty_pages;

static void gc_count_pool_page(jl_gc_pagemeta_t *pg)
{
    int osize = pg->osize;
    char *data = pg->data;
    gcval_t *v = (gcval_t*)(data + GC_PAGE_OFFSET);
    char *lim = (char*)v + GC_PAGE_SZ - GC_PAGE_OFFSET - osize;
    int has_live = 0;
    while ((char*)v <= lim) {
        int bits = gc_bits(v);
        if (bits & GC_MARKED)
            has_live = 1;
        poolobj_sizes[bits] += osize;
        v = (gcval_t*)((char*)v + osize);
    }
    if (!has_live) {
        empty_pages++;
    }
}

static void gc_count_pool_region(region_t *region)
{
    for (int pg_i = 0; pg_i < region->pg_cnt / 32; pg_i++) {
        uint32_t line = region->allocmap[pg_i];
        if (line) {
            for (int j = 0; j < 32; j++) {
                if ((line >> j) & 1) {
                    gc_count_pool_page(&region->meta[pg_i*32 + j]);
                }
            }
        }
    }
}

void gc_count_pool(void)
{
    memset(&poolobj_sizes, 0, sizeof(poolobj_sizes));
    empty_pages = 0;
    for (int i = 0; i < REGION_COUNT; i++) {
        if (regions[i].pages) {
            gc_count_pool_region(&regions[i]);
        }
    }
    jl_safe_printf("****** Pool stat: ******\n");
    for (int i = 0;i < 4;i++)
        jl_safe_printf("bits(%d): %"  PRId64 "\n", i, poolobj_sizes[i]);
    // empty_pages is inaccurate after the sweep since young objects are
    // also GC_CLEAN
    jl_safe_printf("free pages: % "  PRId64 "\n", empty_pages);
    jl_safe_printf("************************\n");
}

#ifdef __cplusplus
}
#endif
