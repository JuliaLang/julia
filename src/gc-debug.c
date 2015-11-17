// This file is a part of Julia. License is MIT: http://julialang.org/license

// Useful function in debugger to find page/region metadata
gcpage_t *jl_gc_page_metadata(void *data)
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
DLLEXPORT jl_taggedvalue_t *jl_gc_find_taggedvalue_pool(char *p,
                                                        size_t *osize_p)
{
    region_t *r = find_region(p, 1);
    // Not in the pool
    if (!r)
        return NULL;
    char *page_begin = GC_PAGE_DATA(p) + GC_PAGE_OFFSET;
    // In the page header
    if (p < page_begin)
        return NULL;
    size_t ofs = p - page_begin;
    int pg_idx = PAGE_INDEX(r, p);
    // Check if this is a free page
    if (r->freemap[pg_idx / 32] & (uint32_t)(1 << (pg_idx % 32)))
        return NULL;
    gcpage_t *pagemeta = &r->meta[pg_idx];
    int osize = pagemeta->osize;
    // Shouldn't be needed, just in case
    if (osize == 0)
        return NULL;
    char *tag = (char*)p - ofs % osize;
    // Points to an "object" that gets into the next page
    if (tag + osize > GC_PAGE_DATA(p) + GC_PAGE_SZ)
        return NULL;
    if (osize_p)
        *osize_p = osize;
    return (jl_taggedvalue_t*)tag;
}

#ifdef GC_DEBUG_ENV
#include <inttypes.h>
#include <stdio.h>
#endif

void jl_(void *jl_value);

// mark verification
#ifdef GC_VERIFY
static jl_value_t* lostval = 0;
static arraylist_t lostval_parents;
static arraylist_t lostval_parents_done;
static int verifying;

static void add_lostval_parent(jl_value_t* parent)
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
    if (!verifying) {
        for (int i = 0; i < 4; i++) {
            bits_save[i].len = 0;
        }
    }
    bigval_t *v;
    FOR_EACH_HEAP
        v = big_objects;
        while (v != NULL) {
            void* gcv = &v->header;
            if (!verifying) arraylist_push(&bits_save[gc_bits(gcv)], gcv);
            gc_bits(gcv) = bits;
            v = v->next;
        }
    END

    v = big_objects_marked;
    while (v != NULL) {
        void* gcv = &v->header;
        if (!verifying) arraylist_push(&bits_save[gc_bits(gcv)], gcv);
        gc_bits(gcv) = bits;
        v = v->next;
    }

    for (int h = 0; h < REGION_COUNT; h++) {
        region_t* region = regions[h];
        if (!region) break;
        for (int pg_i = 0; pg_i < REGION_PG_COUNT/32; pg_i++) {
            uint32_t line = region->freemap[pg_i];
            if (!!~line) {
                for (int j = 0; j < 32; j++) {
                    if (!((line >> j) & 1)) {
                        gcpage_t *pg = page_metadata(&region->pages[pg_i*32 + j][0] + GC_PAGE_OFFSET);
                        pool_t *pool;
                        FOR_HEAP(pg->thread_n)
                            pool = &pools[pg->pool_n];
                        END
                        pv = (gcval_t*)(pg->data + GC_PAGE_OFFSET);
                        char *lim = (char*)pv + GC_PAGE_SZ - GC_PAGE_OFFSET - pool->osize;
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
        jl_printf(JL_STDERR, "Now looking for %p =======\n", lostval);
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
                void* p = bits_save[j >= clean_len ? GC_QUEUED : GC_CLEAN].items[j >= clean_len ? j - clean_len : j];
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
        verifying = 0;
        restore();  // we did not miss anything
        return;
    }
    restore();
    gc_verify_track();
    abort();
}

#else
#define gc_verify()
#define verify_val(v)
#define verify_parent1(ty,obj,slot,arg1)
#define verify_parent2(ty,obj,slot,arg1,arg2)
#endif

#ifdef GC_DEBUG_ENV

typedef struct {
    uint64_t num;

    uint64_t min;
    uint64_t interv;
    uint64_t max;
} jl_alloc_num_t;

typedef struct {
    int sweep_mask;
    jl_alloc_num_t pool;
    jl_alloc_num_t other;
    jl_alloc_num_t print;
} jl_gc_debug_env_t;

DLLEXPORT jl_gc_debug_env_t jl_gc_debug_env = {
    GC_MARKED_NOESC,
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0}
};

static void gc_debug_alloc_init(jl_alloc_num_t *num, const char *name)
{
    // Not very generic and robust but good enough for a debug option
    char buff[128];
    sprintf(buff, "JL_GC_ALLOC_%s", name);
    char *env = getenv(buff);
    if (!env)
        return;
    num->interv = 1;
    num->max = (uint64_t)-1ll;
    sscanf(env, "%" SCNd64 ":%" SCNd64 ":%" SCNd64,
           (int64_t*)&num->min, (int64_t*)&num->interv, (int64_t*)&num->max);
}

static int gc_debug_alloc_check(jl_alloc_num_t *num)
{
    num->num++;
    if (num->interv == 0 || num->num < num->min || num->num > num->max)
        return 0;
    return ((num->num - num->min) % num->interv) == 0;
}

static char *gc_stack_lo;
static void gc_debug_init(void)
{
    gc_stack_lo = (char*)gc_get_stack_ptr();
    char *env = getenv("JL_GC_NO_GENERATIONAL");
    if (env && strcmp(env, "0") != 0) {
        jl_gc_debug_env.sweep_mask = GC_MARKED;
    }
    gc_debug_alloc_init(&jl_gc_debug_env.pool, "POOL");
    gc_debug_alloc_init(&jl_gc_debug_env.other, "OTHER");
    gc_debug_alloc_init(&jl_gc_debug_env.print, "PRINT");
}

static inline int gc_debug_check_pool(void)
{
    return gc_debug_alloc_check(&jl_gc_debug_env.pool);
}

static inline int gc_debug_check_other(void)
{
    return gc_debug_alloc_check(&jl_gc_debug_env.other);
}

void gc_debug_print_status(void)
{
    uint64_t pool_count = jl_gc_debug_env.pool.num;
    uint64_t other_count = jl_gc_debug_env.other.num;
    jl_printf(JL_STDOUT,
              "Allocations: %" PRIu64 " "
              "(Pool: %" PRIu64 "; Other: %" PRIu64 "); GC: %d\n",
              pool_count + other_count, pool_count, other_count,
              n_pause);
}

static inline void gc_debug_print(void)
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
        gcpage_t *pg = page_metadata(tag);
        // Make sure the sweep rebuild the freelist
        pg->allocd = 1;
        pg->gc_bits = 0x3;
        // Find the age bit
        char *page_begin = GC_PAGE_DATA(tag) + GC_PAGE_OFFSET;
        int obj_id = (((char*)tag) - page_begin) / osize;
        uint8_t *ages = pg->ages + obj_id / 8;
        // Force this to be a young object to save some memory
        // (especially on 32bit where it's more likely to have pointer-like
        //  bit patterns)
        *ages &= ~(1 << (obj_id % 8));
        memset(tag, 0xff, osize);
    }
}

static void gc_scrub(char *stack_hi)
{
    gc_scrub_range(gc_stack_lo, stack_hi);
}

#else

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

static inline void gc_debug_init(void)
{
}

static void gc_scrub(char *stack_hi)
{
    (void)stack_hi;
}

#endif
