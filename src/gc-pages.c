// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-common.h"
#include "gc-stock.h"
#ifndef _OS_WINDOWS_
#  include <sys/resource.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

uv_mutex_t gc_pages_lock;

JL_DLLEXPORT uint64_t jl_get_pg_size(void)
{
    return GC_PAGE_SZ;
}

// Try to allocate memory in chunks to permit faster allocation
// and improve memory locality of the pools
#ifdef _P64
#define DEFAULT_BLOCK_PG_ALLOC (4096) // 64 MB
#else
#define DEFAULT_BLOCK_PG_ALLOC (1024) // 16 MB
#endif
#define MIN_BLOCK_PG_ALLOC (1) // 16 KB

static int block_pg_cnt = DEFAULT_BLOCK_PG_ALLOC;

// Prefault. When set, the kernel populates a block at the claim
// (MAP_POPULATE), so no later first touch of a page takes a page fault -
// and none takes what a first touch pays on top of the fault once per few
// MB of new pages: the refill of the core's per-CPU free list from the
// zone, under the zone lock with interrupts off, 300-500 us. Set by
// jl_gc_heap_reserve: a hard real-time loop claims its heap before it
// starts and never faults inside.
static int gc_prefault_blocks = 0;
// The blocks mapped so far, so that a reserve can populate what the runtime
// already holds - the startup block above all, whose untouched pages a loop
// reaches long after the pools claimed them. A block is never unmapped. The
// table holds the first GC_MAX_BLOCKS blocks (64 GB at the default block
// size); a block past the table is mapped but not populated by a reserve.
#define GC_MAX_BLOCKS 4096
static char *gc_block_start[GC_MAX_BLOCKS];
static size_t gc_block_size[GC_MAX_BLOCKS];
static int gc_block_count = 0;

void jl_gc_init_page(void)
{
    if (GC_PAGE_SZ * block_pg_cnt < jl_page_size)
        block_pg_cnt = jl_page_size / GC_PAGE_SZ; // exact division
}

#ifndef MAP_NORESERVE // not defined in POSIX, FreeBSD, etc.
#define MAP_NORESERVE (0)
#endif

// Try to allocate a memory block for multiple pages
// Return `NULL` if allocation failed. Result is aligned to `GC_PAGE_SZ`.
char *jl_gc_try_alloc_pages_(int pg_cnt) JL_NOTSAFEPOINT
{
    size_t pages_sz = GC_PAGE_SZ * pg_cnt;
#ifdef _OS_WINDOWS_
    char *mem = (char*)VirtualAlloc(NULL, pages_sz + GC_PAGE_SZ,
                                    MEM_RESERVE, PAGE_READWRITE);
    if (mem == NULL)
        return NULL;
#else
    if (GC_PAGE_SZ > jl_page_size)
        pages_sz += GC_PAGE_SZ;
    int flags = MAP_NORESERVE | MAP_PRIVATE | MAP_ANONYMOUS;
#ifdef MAP_POPULATE
    if (gc_prefault_blocks)
        flags |= MAP_POPULATE;
#endif
    char *mem = (char*)mmap(0, pages_sz, PROT_READ | PROT_WRITE, flags, -1, 0);
    if (mem == MAP_FAILED)
        return NULL;
#endif
    if (GC_PAGE_SZ > jl_page_size)
        // round data pointer up to the nearest gc_page_data-aligned
        // boundary if mmap didn't already do so.
        mem = (char*)gc_page_data(mem + GC_PAGE_SZ - 1);
    jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_mapped, pages_sz);
    jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_resident, pages_sz);
    if (gc_block_count < GC_MAX_BLOCKS) {
        gc_block_start[gc_block_count] = mem;
        gc_block_size[gc_block_count] = GC_PAGE_SZ * pg_cnt;
        gc_block_count++;
    }
    return mem;
}

// Allocate the memory for a new page. Starts with `block_pg_cnt` number
// of pages. Decrease 4x every time so that there are enough space for a few.
// more chunks (or other allocations). The final page count is recorded
// and will be used as the starting count next time. If the page count is
// smaller `MIN_BLOCK_PG_ALLOC` a `jl_memory_exception` is thrown.
// Assumes `gc_pages_lock` is acquired, the lock is released before the
// exception is thrown.
char *jl_gc_try_alloc_pages(void) JL_NOTSAFEPOINT
{
    unsigned pg_cnt = block_pg_cnt;
    char *mem = NULL;
    while (1) {
        if (__likely((mem = jl_gc_try_alloc_pages_(pg_cnt))))
            break;
        size_t min_block_pg_alloc = MIN_BLOCK_PG_ALLOC;
        if (GC_PAGE_SZ * min_block_pg_alloc < jl_page_size)
            min_block_pg_alloc = jl_page_size / GC_PAGE_SZ; // exact division
        if (pg_cnt >= 4 * min_block_pg_alloc) {
            pg_cnt /= 4;
            block_pg_cnt = pg_cnt;
        }
        else if (pg_cnt > min_block_pg_alloc) {
            block_pg_cnt = pg_cnt = min_block_pg_alloc;
        }
        else {
            uv_mutex_unlock(&gc_pages_lock);
            jl_throw(jl_memory_exception);
        }
    }
    return mem;
}

// get a new page, either from the freemap
// or from the kernel if none are available
NOINLINE jl_gc_pagemeta_t *jl_gc_alloc_page(void) JL_NOTSAFEPOINT
{
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    jl_gc_pagemeta_t *meta = NULL;

    // try to get page from `pool_lazily_freed`
    meta = pop_lf_back(&global_page_pool_lazily_freed);
    if (meta != NULL) {
        gc_alloc_map_set(meta->data, GC_PAGE_ALLOCATED);
        // page is already mapped
        return meta;
    }

    // try to get page from `pool_clean`
    meta = pop_lf_back(&global_page_pool_clean);
    if (meta != NULL) {
        gc_alloc_map_set(meta->data, GC_PAGE_ALLOCATED);
        goto exit;
    }

    // try to get page from `pool_freed`
    meta = pop_lf_back(&global_page_pool_freed);
    if (meta != NULL) {
        jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_resident, GC_PAGE_SZ);
        gc_alloc_map_set(meta->data, GC_PAGE_ALLOCATED);
        goto exit;
    }

    uv_mutex_lock(&gc_pages_lock);
    // another thread may have allocated a large block while we were waiting...
    meta = pop_lf_back(&global_page_pool_clean);
    if (meta != NULL) {
        uv_mutex_unlock(&gc_pages_lock);
        gc_alloc_map_set(meta->data, GC_PAGE_ALLOCATED);
        goto exit;
    }
    {
        // must map a new set of pages
        char *data = jl_gc_try_alloc_pages();
        meta = (jl_gc_pagemeta_t*)malloc_s(block_pg_cnt * sizeof(jl_gc_pagemeta_t));
        for (int i = 0; i < block_pg_cnt; i++) {
            jl_gc_pagemeta_t *pg = &meta[i];
            pg->data = data + GC_PAGE_SZ * i;
            gc_alloc_map_maybe_create(pg->data);
            if (i == 0) {
                gc_alloc_map_set(pg->data, GC_PAGE_ALLOCATED);
            }
            else {
                push_lf_back(&global_page_pool_clean, pg);
            }
        }
        uv_mutex_unlock(&gc_pages_lock);
    }
exit:
#ifdef _OS_WINDOWS_
    VirtualAlloc(meta->data, GC_PAGE_SZ, MEM_COMMIT, PAGE_READWRITE);
    SetLastError(last_error);
#endif
    errno = last_errno;
    return meta;
}

// Claim `bytes` of page blocks now, populated, into the clean pool - and
// prefault every block claimed from here on. jl_gc_alloc_page serves the
// clean pool before it maps anything, so a loop whose heap fits the
// reserve maps nothing and faults nothing while it runs. The region tag of
// a page is set at its pool claim, so a page can wait here untagged.
// Returns the bytes mapped, rounded up to whole blocks.
JL_DLLEXPORT uint64_t jl_gc_heap_reserve(uint64_t bytes) JL_NOTSAFEPOINT
{
    gc_prefault_blocks = 1;
    // First what is already mapped: populate every block the runtime holds,
    // writable, so no page of them faults later. MADV_POPULATE_WRITE needs
    // Linux 5.14; where it is missing the blocks mapped before the reserve
    // keep their lazy pages, and only the blocks from here on are populated.
#ifdef MADV_POPULATE_WRITE
    uv_mutex_lock(&gc_pages_lock);
    for (int i = 0; i < gc_block_count; i++)
        madvise(gc_block_start[i], gc_block_size[i], MADV_POPULATE_WRITE);
    uv_mutex_unlock(&gc_pages_lock);
#endif
    uint64_t mapped = 0;
    while (mapped < bytes) {
        uv_mutex_lock(&gc_pages_lock);
        char *data = jl_gc_try_alloc_pages(); // unlocks and throws when the OS refuses
        int n = block_pg_cnt;
        jl_gc_pagemeta_t *meta = (jl_gc_pagemeta_t*)malloc_s(n * sizeof(jl_gc_pagemeta_t));
        for (int i = 0; i < n; i++) {
            jl_gc_pagemeta_t *pg = &meta[i];
            pg->data = data + GC_PAGE_SZ * i;
            gc_alloc_map_maybe_create(pg->data);
            push_lf_back(&global_page_pool_clean, pg);
        }
        uv_mutex_unlock(&gc_pages_lock);
        mapped += (uint64_t)n * GC_PAGE_SZ;
    }
    return mapped;
}

// return a page to the freemap allocator
void jl_gc_free_page(jl_gc_pagemeta_t *pg) JL_NOTSAFEPOINT
{
    void *p = pg->data;
    gc_alloc_map_set((char*)p, GC_PAGE_FREED);
    // tell the OS we don't need these pages right now
    size_t decommit_size = GC_PAGE_SZ;
    if (GC_PAGE_SZ < jl_page_size) {
        // ensure so we don't release more memory than intended
        size_t n_pages = jl_page_size / GC_PAGE_SZ; // exact division
        decommit_size = jl_page_size;
        void *otherp = (void*)((uintptr_t)p & ~(jl_page_size - 1)); // round down to the nearest physical page
        p = otherp;
        while (n_pages--) {
            if (gc_alloc_map_is_set((char*)otherp)) {
                return;
            }
            otherp = (void*)((char*)otherp + GC_PAGE_SZ);
        }
    }
#ifdef _OS_WINDOWS_
    VirtualFree(p, decommit_size, MEM_DECOMMIT);
#elif defined(MADV_FREE)
    static int supports_madv_free = 1;
    if (supports_madv_free) {
        if (madvise(p, decommit_size, MADV_FREE) == -1) {
            assert(errno == EINVAL);
            supports_madv_free = 0;
        }
    }
    if (!supports_madv_free) {
        madvise(p, decommit_size, MADV_DONTNEED);
    }
#else
    madvise(p, decommit_size, MADV_DONTNEED);
#endif
    msan_unpoison(p, decommit_size);
    jl_atomic_fetch_add_relaxed(&gc_heap_stats.bytes_resident, -decommit_size);
}

#ifdef __cplusplus
}
#endif
