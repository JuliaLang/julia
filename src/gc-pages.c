// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "gc.h"

#ifdef __cplusplus
extern "C" {
#endif

// A region is contiguous storage for up to REGION_PG_COUNT naturally aligned GC_PAGE_SZ pages
// It uses a very naive allocator (see jl_gc_alloc_page & jl_gc_free_page)
#if defined(_P64)
#define REGION_PG_COUNT 16*8*4096 // 8G because virtual memory is cheap
#else
#define REGION_PG_COUNT 8*4096 // 512M
#endif

static jl_mutex_t pagealloc_lock;
static size_t current_pg_count = 0;

NOINLINE void *jl_gc_alloc_page(void)
{
    void *ptr = NULL;
    int i;
    region_t *region;
    int region_i = 0;
    JL_LOCK_NOGC(&pagealloc_lock);
    while(region_i < REGION_COUNT) {
        region = &regions[region_i];
        if (region->pages == NULL) {
            int pg_cnt = REGION_PG_COUNT;
            const size_t pages_sz = sizeof(jl_gc_page_t) * pg_cnt;
            const size_t freemap_sz = sizeof(uint32_t) * pg_cnt / 32;
            const size_t meta_sz = sizeof(jl_gc_pagemeta_t) * pg_cnt;
            size_t alloc_size = pages_sz + freemap_sz + meta_sz;
#ifdef _OS_WINDOWS_
            char *mem = (char*)VirtualAlloc(NULL, alloc_size + GC_PAGE_SZ,
                                            MEM_RESERVE, PAGE_READWRITE);
#else
            if (GC_PAGE_SZ > jl_page_size)
                alloc_size += GC_PAGE_SZ;
            char *mem = (char*)mmap(0, alloc_size, PROT_READ | PROT_WRITE, MAP_NORESERVE | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
            mem = mem == MAP_FAILED ? NULL : mem;
#endif
            if (mem == NULL) {
                jl_printf(JL_STDERR, "could not allocate pools\n");
                gc_debug_critical_error();
                abort();
            }
            if (GC_PAGE_SZ > jl_page_size) {
                // round data pointer up to the nearest gc_page_data-aligned
                // boundary if mmap didn't already do so.
                mem = (char*)gc_page_data(mem + GC_PAGE_SZ - 1);
            }
            region->pages = (jl_gc_page_t*)mem;
            region->freemap = (uint32_t*)(mem + pages_sz);
            region->meta = (jl_gc_pagemeta_t*)(mem + pages_sz +freemap_sz);
            region->lb = 0;
            region->ub = 0;
            region->pg_cnt = pg_cnt;
#ifdef _OS_WINDOWS_
            VirtualAlloc(region->freemap, region->pg_cnt / 8,
                         MEM_COMMIT, PAGE_READWRITE);
            VirtualAlloc(region->meta, region->pg_cnt * sizeof(jl_gc_pagemeta_t),
                         MEM_COMMIT, PAGE_READWRITE);
#endif
            memset(region->freemap, 0xff, region->pg_cnt / 8);
        }
        for (i = region->lb; i < region->pg_cnt / 32; i++) {
            if (region->freemap[i])
                break;
        }
        if (i == region->pg_cnt / 32) {
            // region full
            region_i++;
            continue;
        }
        break;
    }
    if (region_i >= REGION_COUNT) {
        jl_printf(JL_STDERR, "increase REGION_COUNT or allocate less memory\n");
        gc_debug_critical_error();
        abort();
    }
    if (region->lb < i)
        region->lb = i;
    if (region->ub < i)
        region->ub = i;

#if defined(_COMPILER_MINGW_)
    int j = __builtin_ffs(region->freemap[i]) - 1;
#elif defined(_COMPILER_MICROSOFT_)
    unsigned long j;
    _BitScanForward(&j, region->freemap[i]);
#else
    int j = ffs(region->freemap[i]) - 1;
#endif

    region->freemap[i] &= ~(uint32_t)(1 << j);
    ptr = region->pages[i*32 + j].data;
#ifdef _OS_WINDOWS_
    VirtualAlloc(ptr, GC_PAGE_SZ, MEM_COMMIT, PAGE_READWRITE);
#endif
    current_pg_count++;
#ifdef GC_FINAL_STATS
    max_pg_count = max_pg_count < current_pg_count ? current_pg_count : max_pg_count;
#endif
    JL_UNLOCK_NOGC(&pagealloc_lock);
    return ptr;
}

void jl_gc_free_page(void *p)
{
    int pg_idx = -1;
    int i;
    region_t *region = regions;
    for (i = 0; i < REGION_COUNT && regions[i].pages != NULL; i++) {
        region = &regions[i];
        pg_idx = page_index(region, p);
        if (pg_idx >= 0 && pg_idx < region->pg_cnt) {
            break;
        }
    }
    assert(i < REGION_COUNT && region->pages != NULL);
    uint32_t msk = (uint32_t)(1 << (pg_idx % 32));
    assert(!(region->freemap[pg_idx/32] & msk));
    region->freemap[pg_idx/32] ^= msk;
    free(region->meta[pg_idx].ages);
    // tell the OS we don't need these pages right now
    size_t decommit_size = GC_PAGE_SZ;
    if (GC_PAGE_SZ < jl_page_size) {
        // ensure so we don't release more memory than intended
        size_t n_pages = (GC_PAGE_SZ + jl_page_size - 1) / GC_PAGE_SZ;
        decommit_size = jl_page_size;
        p = (void*)((uintptr_t)region->pages[pg_idx].data & ~(jl_page_size - 1)); // round down to the nearest page
        pg_idx = page_index(region, p);
        if (pg_idx + n_pages > region->pg_cnt) goto no_decommit;
        for (; n_pages--; pg_idx++) {
            msk = (uint32_t)(1 << ((pg_idx % 32)));
            if (!(region->freemap[pg_idx/32] & msk)) goto no_decommit;
        }
    }
#ifdef _OS_WINDOWS_
    VirtualFree(p, decommit_size, MEM_DECOMMIT);
#else
    madvise(p, decommit_size, MADV_DONTNEED);
#endif
no_decommit:
    if (region->lb > pg_idx / 32)
        region->lb = pg_idx / 32;
    current_pg_count--;
}

#ifdef __cplusplus
}
#endif
