// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc.h"
#ifndef _OS_WINDOWS_
#  include <sys/resource.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

// Try to allocate memory in chunks to permit faster allocation
// and improve memory locality of the pools
#ifdef _P64
#define DEFAULT_BLOCK_PG_ALLOC (4096) // 64 MB
#else
#define DEFAULT_BLOCK_PG_ALLOC (1024) // 16 MB
#endif
#define MIN_BLOCK_PG_ALLOC (1) // 16 KB

static int block_pg_cnt = DEFAULT_BLOCK_PG_ALLOC;
static size_t current_pg_count = 0;

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
static char *jl_gc_try_alloc_pages(int pg_cnt) JL_NOTSAFEPOINT
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
    char *mem = (char*)mmap(0, pages_sz, PROT_READ | PROT_WRITE,
                            MAP_NORESERVE | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mem == MAP_FAILED)
        return NULL;
#endif
    if (GC_PAGE_SZ > jl_page_size)
        // round data pointer up to the nearest gc_page_data-aligned
        // boundary if mmap didn't already do so.
        mem = (char*)gc_page_data(mem + GC_PAGE_SZ - 1);
    return mem;
}

// Allocate the memory for a new page. Starts with `block_pg_cnt` number
// of pages. Decrease 4x every time so that there are enough space for a few.
// more chunks (or other allocations). The final page count is recorded
// and will be used as the starting count next time. If the page count is
// smaller `MIN_BLOCK_PG_ALLOC` a `jl_memory_exception` is thrown.
// Assumes `gc_perm_lock` is acquired, the lock is released before the
// exception is thrown.
static jl_gc_pagemeta_t *jl_gc_alloc_new_page(void) JL_NOTSAFEPOINT
{
    // try to allocate a large block of memory (or a small one)
    unsigned pg, pg_cnt = block_pg_cnt;
    char *mem = NULL;
    while (1) {
        if (__likely((mem = jl_gc_try_alloc_pages(pg_cnt))))
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
            JL_UNLOCK_NOGC(&gc_perm_lock);
            jl_throw(jl_memory_exception);
        }
    }

    // now need to insert these pages into the pagetable metadata
    // if any allocation fails, this just stops recording more pages from that point
    // and will free (munmap) the remainder
    jl_gc_pagemeta_t *page_meta =
        (jl_gc_pagemeta_t*)jl_gc_perm_alloc_nolock(pg_cnt * sizeof(jl_gc_pagemeta_t), 1,
                                                   sizeof(void*), 0);
    pg = 0;
    if (page_meta) {
        for (; pg < pg_cnt; pg++) {
            struct jl_gc_metadata_ext info;
            uint32_t msk;
            unsigned i;
            pagetable1_t **ppagetable1;
            pagetable0_t **ppagetable0;
            jl_gc_pagemeta_t **pmeta;

            char *ptr = mem + (GC_PAGE_SZ * pg);
            page_meta[pg].data = ptr;

            // create & store the level 2 / outermost info
            i = REGION_INDEX(ptr);
            info.pagetable_i = i % 32;
            info.pagetable_i32 = i / 32;
            msk = (1 << info.pagetable_i);
            if ((memory_map.freemap1[info.pagetable_i32] & msk) == 0)
                memory_map.freemap1[info.pagetable_i32] |= msk; // has free
            info.pagetable1 = *(ppagetable1 = &memory_map.meta1[i]);
            if (!info.pagetable1) {
                info.pagetable1 = (pagetable1_t*)jl_gc_perm_alloc_nolock(sizeof(pagetable1_t), 1,
                                                                         sizeof(void*), 0);
                *ppagetable1 = info.pagetable1;
                if (!info.pagetable1)
                    break;
            }

            // create & store the level 1 info
            i = REGION1_INDEX(ptr);
            info.pagetable1_i = i % 32;
            info.pagetable1_i32 = i / 32;
            msk = (1 << info.pagetable1_i);
            if ((info.pagetable1->freemap0[info.pagetable1_i32] & msk) == 0)
                info.pagetable1->freemap0[info.pagetable1_i32] |= msk; // has free
            info.pagetable0 = *(ppagetable0 = &info.pagetable1->meta0[i]);
            if (!info.pagetable0) {
                info.pagetable0 = (pagetable0_t*)jl_gc_perm_alloc_nolock(sizeof(pagetable0_t), 1,
                                                                         sizeof(void*), 0);
                *ppagetable0 = info.pagetable0;
                if (!info.pagetable0)
                    break;
            }

            // create & store the level 0 / page info
            i = REGION0_INDEX(ptr);
            info.pagetable0_i = i % 32;
            info.pagetable0_i32 = i / 32;
            msk = (1 << info.pagetable0_i);
            info.pagetable0->freemap[info.pagetable0_i32] |= msk; // is free
            pmeta = &info.pagetable0->meta[i];
            info.meta = (*pmeta = &page_meta[pg]);
        }
    }

    if (pg < pg_cnt) {
#ifndef _OS_WINDOWS_
        // Trim the allocation to only cover the region
        // that we successfully created the metadata for.
        // This is not supported by the Windows kernel,
        // so we have to just skip it there and just lose these virtual addresses.
        munmap(mem + LLT_ALIGN(GC_PAGE_SZ * pg, jl_page_size),
               GC_PAGE_SZ * pg_cnt - LLT_ALIGN(GC_PAGE_SZ * pg, jl_page_size));
#endif
        if (pg == 0) {
            JL_UNLOCK_NOGC(&gc_perm_lock);
            jl_throw(jl_memory_exception);
        }
    }
    return page_meta;
}

// get a new page, either from the freemap
// or from the kernel if none are available
NOINLINE jl_gc_pagemeta_t *jl_gc_alloc_page(void) JL_NOTSAFEPOINT
{
    struct jl_gc_metadata_ext info;
    JL_LOCK_NOGC(&gc_perm_lock);

    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    // scan over memory_map page-table for existing allocated but unused pages
    for (info.pagetable_i32 = memory_map.lb; info.pagetable_i32 < (REGION2_PG_COUNT + 31) / 32; info.pagetable_i32++) {
        uint32_t freemap1 = memory_map.freemap1[info.pagetable_i32];
        for (info.pagetable_i = 0; freemap1; info.pagetable_i++, freemap1 >>= 1) {
            unsigned next = ffs_u32(freemap1);
            info.pagetable_i += next;
            freemap1 >>= next;
            info.pagetable1 = memory_map.meta1[info.pagetable_i + info.pagetable_i32 * 32];
            // repeat over page-table level 1
            for (info.pagetable1_i32 = info.pagetable1->lb; info.pagetable1_i32 < REGION1_PG_COUNT / 32; info.pagetable1_i32++) {
                uint32_t freemap0 = info.pagetable1->freemap0[info.pagetable1_i32];
                for (info.pagetable1_i = 0; freemap0; info.pagetable1_i++, freemap0 >>= 1) {
                    unsigned next = ffs_u32(freemap0);
                    info.pagetable1_i += next;
                    freemap0 >>= next;
                    info.pagetable0 = info.pagetable1->meta0[info.pagetable1_i + info.pagetable1_i32 * 32];
                    // repeat over page-table level 0
                    for (info.pagetable0_i32 = info.pagetable0->lb; info.pagetable0_i32 < REGION0_PG_COUNT / 32; info.pagetable0_i32++) {
                        uint32_t freemap = info.pagetable0->freemap[info.pagetable0_i32];
                        if (freemap) {
                            info.pagetable0_i = ffs_u32(freemap);
                            info.meta = info.pagetable0->meta[info.pagetable0_i + info.pagetable0_i32 * 32];
                            assert(info.meta->data);
                            // new pages available starting at min of lb and pagetable_i32
                            if (memory_map.lb < info.pagetable_i32)
                                memory_map.lb = info.pagetable_i32;
                            if (info.pagetable1->lb < info.pagetable1_i32)
                                info.pagetable1->lb = info.pagetable1_i32;
                            if (info.pagetable0->lb < info.pagetable0_i32)
                                info.pagetable0->lb = info.pagetable0_i32;
                            goto have_free_page; // break out of all of these loops
                        }
                    }
                    info.pagetable1->freemap0[info.pagetable1_i32] &= ~(uint32_t)(1 << info.pagetable1_i); // record that this was full
                }
            }
            memory_map.freemap1[info.pagetable_i32] &= ~(uint32_t)(1 << info.pagetable_i); // record that this was full
        }
    }

    // no existing pages found, allocate a new one
    {
        jl_gc_pagemeta_t *meta = jl_gc_alloc_new_page();
        info = page_metadata_ext(meta->data);
        assert(meta == info.meta);
        // new pages are now available starting at max of lb and pagetable_i32
        if (memory_map.lb > info.pagetable_i32)
            memory_map.lb = info.pagetable_i32;
        if (info.pagetable1->lb > info.pagetable1_i32)
            info.pagetable1->lb = info.pagetable1_i32;
        if (info.pagetable0->lb > info.pagetable0_i32)
            info.pagetable0->lb = info.pagetable0_i32;
    }

have_free_page:
    // in-use pages are now ending at min of ub and pagetable_i32
    if (memory_map.ub < info.pagetable_i32)
        memory_map.ub = info.pagetable_i32;
    if (info.pagetable1->ub < info.pagetable1_i32)
        info.pagetable1->ub = info.pagetable1_i32;
    if (info.pagetable0->ub < info.pagetable0_i32)
        info.pagetable0->ub = info.pagetable0_i32;

    // mark this entry as in-use and not free
    info.pagetable0->freemap[info.pagetable0_i32] &= ~(uint32_t)(1 << info.pagetable0_i);
    info.pagetable0->allocmap[info.pagetable0_i32] |= (uint32_t)(1 << info.pagetable0_i);
    info.pagetable1->allocmap0[info.pagetable1_i32] |= (uint32_t)(1 << info.pagetable1_i);
    memory_map.allocmap1[info.pagetable_i32] |= (uint32_t)(1 << info.pagetable_i);

#ifdef _OS_WINDOWS_
    VirtualAlloc(info.meta->data, GC_PAGE_SZ, MEM_COMMIT, PAGE_READWRITE);
#endif
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;
    current_pg_count++;
    gc_final_count_page(current_pg_count);
    JL_UNLOCK_NOGC(&gc_perm_lock);
    return info.meta;
}

// return a page to the freemap allocator
void jl_gc_free_page(void *p) JL_NOTSAFEPOINT
{
    // update the allocmap and freemap to indicate this contains a free entry
    struct jl_gc_metadata_ext info = page_metadata_ext(p);
    uint32_t msk;
    msk = (uint32_t)(1 << info.pagetable0_i);
    assert(!(info.pagetable0->freemap[info.pagetable0_i32] & msk));
    assert(info.pagetable0->allocmap[info.pagetable0_i32] & msk);
    info.pagetable0->allocmap[info.pagetable0_i32] &= ~msk;
    info.pagetable0->freemap[info.pagetable0_i32] |= msk;

    msk = (uint32_t)(1 << info.pagetable1_i);
    assert(info.pagetable1->allocmap0[info.pagetable1_i32] & msk);
    if ((info.pagetable1->freemap0[info.pagetable1_i32] & msk) == 0)
        info.pagetable1->freemap0[info.pagetable1_i32] |= msk;

    msk = (uint32_t)(1 << info.pagetable_i);
    assert(memory_map.allocmap1[info.pagetable_i32] & msk);
    if ((memory_map.freemap1[info.pagetable_i32] & msk) == 0)
        memory_map.freemap1[info.pagetable_i32] |= msk;

    free(info.meta->ages);
    info.meta->ages = NULL;

    // tell the OS we don't need these pages right now
    size_t decommit_size = GC_PAGE_SZ;
    if (GC_PAGE_SZ < jl_page_size) {
        // ensure so we don't release more memory than intended
        size_t n_pages = jl_page_size / GC_PAGE_SZ; // exact division
        decommit_size = jl_page_size;
        void *otherp = (void*)((uintptr_t)p & ~(jl_page_size - 1)); // round down to the nearest physical page
        p = otherp;
        while (n_pages--) {
            struct jl_gc_metadata_ext info = page_metadata_ext(otherp);
            msk = (uint32_t)(1 << info.pagetable0_i);
            if (info.pagetable0->allocmap[info.pagetable0_i32] & msk)
                goto no_decommit;
            otherp = (void*)((char*)otherp + GC_PAGE_SZ);
        }
    }
#ifdef _OS_WINDOWS_
    VirtualFree(p, decommit_size, MEM_DECOMMIT);
#else
    madvise(p, decommit_size, MADV_DONTNEED);
#endif

no_decommit:
    // new pages are now available starting at max of lb and pagetable_i32
    if (memory_map.lb > info.pagetable_i32)
        memory_map.lb = info.pagetable_i32;
    if (info.pagetable1->lb > info.pagetable1_i32)
        info.pagetable1->lb = info.pagetable1_i32;
    if (info.pagetable0->lb > info.pagetable0_i32)
        info.pagetable0->lb = info.pagetable0_i32;
    current_pg_count--;
}

#ifdef __cplusplus
}
#endif
