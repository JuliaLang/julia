// This file is a part of Julia. License is MIT: https://julialang.org/license

// Mappings for Memory that can grow in place: a private mapping holding a header page, then the
// data. Only claimed bytes count towards the heap; the rest is reserved address space, committed
// as it is claimed.

#include "gc-common.h"
#include "gc-stock.h"
#include "gc-alloc-profiler.h"

#ifdef __cplusplus
extern "C" {
#endif

// The header takes the first page, so that the data starts on a page of its own
#define JL_MEMORY_MAPPING_HEADER jl_page_size

#ifdef _P64
#ifdef _OS_WINDOWS_
#define SAVE_ERRNO DWORD last_error = GetLastError(); int last_errno = errno
#define RESTORE_ERRNO (SetLastError(last_error), errno = last_errno)
#else
#include <sys/resource.h>
#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0
#endif
#define SAVE_ERRNO int last_errno = errno
#define RESTORE_ERRNO (errno = last_errno)
#endif

typedef struct {
    _Atomic(size_t) mapped;  // bytes of address space, header included
    _Atomic(size_t) claimed; // data bytes handed out to Memory objects
    _Atomic(size_t) live;    // data bytes reached by the Memory objects marked since the last trim
} jl_memory_mapping_t;

#define JL_MEMORY_MAPPING_HEADROOM_MAX ((size_t)2 << 30)

// Budgets that leave mappings and address space to malloc, thread stacks and libraries; zero
// disables mapping
static _Atomic(size_t) memory_mappings, memory_mapping_bytes;
static size_t memory_mappings_max, memory_mapping_bytes_max;
// Room to grow reserved in each mapping
static size_t memory_mapping_headroom;

static void *os_reserve(size_t sz) JL_NOTSAFEPOINT
{
#ifdef _OS_WINDOWS_
    return VirtualAlloc(NULL, sz, MEM_RESERVE, PAGE_NOACCESS);
#else
    void *p = mmap(NULL, sz, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
    if (p == MAP_FAILED)
        return NULL;
    return p;
#endif
}

static void os_decommit(void *p, size_t sz) JL_NOTSAFEPOINT
{
#ifdef _OS_WINDOWS_
    VirtualFree(p, sz, MEM_DECOMMIT);
#elif defined(_OS_LINUX_) || !defined(MADV_FREE)
    madvise(p, sz, MADV_DONTNEED);
#else
    madvise(p, sz, MADV_FREE); // elsewhere MADV_DONTNEED need not release the pages
#endif
}

static void os_release(void *p, size_t sz) JL_NOTSAFEPOINT
{
#ifdef _OS_WINDOWS_
    VirtualFree(p, 0, MEM_RELEASE);
#else
    munmap(p, sz);
#endif
}

#ifdef _OS_LINUX_
static size_t read_sysctl(const char *path, size_t fallback) JL_NOTSAFEPOINT
{
    unsigned long long value = fallback;
    FILE *f = fopen(path, "r");
    if (f == NULL)
        return fallback;
    if (fscanf(f, "%llu", &value) != 1)
        value = fallback;
    fclose(f);
    return (size_t)value;
}
#endif

void memory_mapping_init(void) JL_NOTSAFEPOINT
{
    // unless the system says otherwise: Linux's default vm.max_map_count, x86-64's user space
    size_t mappings = 65530, space = (size_t)1 << 47;
#ifdef _OS_LINUX_
    // Strict overcommit would charge reserved address space as committed memory
    if (read_sysctl("/proc/sys/vm/overcommit_memory", 0) == 2)
        return;
    mappings = read_sysctl("/proc/sys/vm/max_map_count", mappings);
#endif
#ifndef _OS_WINDOWS_
    struct rlimit limit;
    if (getrlimit(RLIMIT_AS, &limit) == 0 && limit.rlim_cur < space)
        space = limit.rlim_cur;
#endif
    memory_mappings_max = mappings / 8;
    memory_mapping_bytes_max = space / 4;
    memory_mapping_headroom = memory_mapping_bytes_max / 64;
    if (memory_mapping_headroom > JL_MEMORY_MAPPING_HEADROOM_MAX)
        memory_mapping_headroom = JL_MEMORY_MAPPING_HEADROOM_MAX;
}

static jl_memory_mapping_t *memory_mapping(void *data) JL_NOTSAFEPOINT
{
    return (jl_memory_mapping_t*)((char*)data - JL_MEMORY_MAPPING_HEADER);
}

// Bytes of address space to reserve for `sz` data bytes, or 0 when over budget
static size_t memory_mapping_size(size_t sz) JL_NOTSAFEPOINT
{
    if (sz > memory_mapping_bytes_max)
        return 0;
    size_t reserve = 4 * sz;
    if (sz + memory_mapping_headroom > reserve)
        reserve = sz + memory_mapping_headroom;
    return LLT_ALIGN(JL_MEMORY_MAPPING_HEADER + reserve, jl_page_size);
}

static void memory_mapping_give(size_t bytes) JL_NOTSAFEPOINT
{
    jl_atomic_fetch_add_relaxed(&memory_mappings, -1);
    jl_atomic_fetch_add_relaxed(&memory_mapping_bytes, -bytes);
}

static int memory_mapping_take(size_t bytes) JL_NOTSAFEPOINT
{
    size_t mappings = jl_atomic_fetch_add_relaxed(&memory_mappings, 1) + 1;
    size_t total = jl_atomic_fetch_add_relaxed(&memory_mapping_bytes, bytes) + bytes;
    if (mappings <= memory_mappings_max && total <= memory_mapping_bytes_max)
        return 1;
    memory_mapping_give(bytes);
    return 0;
}

// Make the bytes of a mapping from `from` to `to` ready for use. Windows has to commit them;
// elsewhere they are committed as they are first written.
static int memory_mapping_commit(jl_memory_mapping_t *mapping, size_t from, size_t to) JL_NOTSAFEPOINT
{
#ifdef _OS_WINDOWS_
    size_t start = from & ~(jl_page_size - 1), len = LLT_ALIGN(to, jl_page_size) - start;
    return VirtualAlloc((char*)mapping + start, len, MEM_COMMIT, PAGE_READWRITE) != NULL;
#else
    return 1;
#endif
}

void memory_mapping_mark(void *data, size_t sz) JL_NOTSAFEPOINT
{
    jl_memory_mapping_t *mapping = memory_mapping(data);
    size_t live = jl_atomic_load_relaxed(&mapping->live);
    while (live < sz && !jl_atomic_cmpswap_relaxed(&mapping->live, &live, sz))
        ;
}

// Release the claimed bytes past the longest live Memory, and return how many. Only valid once
// a collection has marked every live Memory.
size_t memory_mapping_trim(void *data) JL_NOTSAFEPOINT
{
    jl_memory_mapping_t *mapping = memory_mapping(data);
    size_t claimed = jl_atomic_load_relaxed(&mapping->claimed);
    size_t live = jl_atomic_exchange_relaxed(&mapping->live, 0);
    if (live >= claimed)
        return 0;
    jl_atomic_store_relaxed(&mapping->claimed, live);
    size_t from = LLT_ALIGN(JL_MEMORY_MAPPING_HEADER + live, jl_page_size);
    size_t to = LLT_ALIGN(JL_MEMORY_MAPPING_HEADER + claimed, jl_page_size);
    if (to > from)
        os_decommit((char*)mapping + from, to - from);
    return claimed - live;
}

size_t memory_mapping_free(void *data) JL_NOTSAFEPOINT
{
    jl_memory_mapping_t *mapping = memory_mapping(data);
    size_t mapped = jl_atomic_load_relaxed(&mapping->mapped), claimed = jl_atomic_load_relaxed(&mapping->claimed);
    os_release(mapping, mapped);
    memory_mapping_give(mapped);
    return JL_MEMORY_MAPPING_HEADER + claimed;
}

// Map `sz` claimed bytes and return their address, or NULL when over budget
static void *memory_mapping_new(size_t sz) JL_NOTSAFEPOINT
{
    size_t mapped = memory_mapping_size(sz);
    if (mapped == 0)
        return NULL;
    if (!memory_mapping_take(mapped))
        return NULL;
    SAVE_ERRNO;
    jl_memory_mapping_t *mapping = (jl_memory_mapping_t*)os_reserve(mapped);
    if (mapping != NULL && !memory_mapping_commit(mapping, 0, JL_MEMORY_MAPPING_HEADER + sz)) {
        os_release(mapping, mapped);
        mapping = NULL;
    }
    RESTORE_ERRNO;
    if (mapping == NULL) {
        memory_mapping_give(mapped);
        return NULL;
    }
    jl_atomic_store_relaxed(&mapping->mapped, mapped);
    jl_atomic_store_relaxed(&mapping->claimed, sz);
    jl_atomic_store_relaxed(&mapping->live, 0);
    return (char*)mapping + JL_MEMORY_MAPPING_HEADER;
}

// Claim the bytes of the mapped `data` from `oldsz` to `newsz`. Only the Memory ending at the
// claimed bytes may grow, so no two can share new bytes.
static int memory_mapping_expand(void *data, size_t oldsz, size_t newsz) JL_NOTSAFEPOINT
{
    jl_memory_mapping_t *mapping = memory_mapping(data);
    size_t claimed = oldsz;
    if (JL_MEMORY_MAPPING_HEADER + newsz > jl_atomic_load_relaxed(&mapping->mapped) ||
        !jl_atomic_cmpswap(&mapping->claimed, &claimed, newsz))
        return 0;
    SAVE_ERRNO;
    int usable = memory_mapping_commit(mapping, JL_MEMORY_MAPPING_HEADER + oldsz, JL_MEMORY_MAPPING_HEADER + newsz);
    RESTORE_ERRNO;
    if (!usable) {
        jl_atomic_store_relaxed(&mapping->claimed, oldsz);
        return 0;
    }
    return 1;
}
#else
void memory_mapping_init(void) JL_NOTSAFEPOINT {}
void memory_mapping_mark(void *data, size_t sz) JL_NOTSAFEPOINT {}
size_t memory_mapping_trim(void *data) JL_NOTSAFEPOINT { return 0; }
static void *memory_mapping_new(size_t sz) JL_NOTSAFEPOINT { return NULL; }
static int memory_mapping_expand(void *data, size_t oldsz, size_t newsz) JL_NOTSAFEPOINT { return 0; }

size_t memory_mapping_free(void *data) JL_NOTSAFEPOINT
{
    abort(); // unreachable: nothing is mapped
}
#endif

static void gc_count_managed_bytes(jl_ptls_t ptls, size_t sz) JL_NOTSAFEPOINT
{
    jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.allocd,
        jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.allocd) + sz);
    jl_batch_accum_heap_size(&ptls->gc_tls_common.gc_num, sz);
}

JL_DLLEXPORT void *jl_gc_managed_map(size_t sz) JL_CANSAFEPOINT
{
    jl_ptls_t ptls = jl_current_task->ptls;
    maybe_collect(ptls);
    void *data = memory_mapping_new(sz);
    if (data == NULL)
        return NULL;
    gc_count_managed_bytes(ptls, JL_MEMORY_MAPPING_HEADER + sz);
    jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.malloc,
        jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.malloc) + 1);
    maybe_record_alloc_to_profile((jl_value_t*)data, sz, (jl_datatype_t*)jl_buff_tag);
    return data;
}

JL_DLLEXPORT int jl_gc_managed_expand(void *data, size_t oldsz, size_t newsz) JL_CANSAFEPOINT
{
    jl_ptls_t ptls = jl_current_task->ptls;
    maybe_collect(ptls);
    if (!memory_mapping_expand(data, oldsz, newsz))
        return 0;
    gc_count_managed_bytes(ptls, newsz - oldsz);
    maybe_record_alloc_to_profile((jl_value_t*)((char*)data + oldsz), newsz - oldsz, (jl_datatype_t*)jl_buff_tag);
    return 1;
}

#ifdef __cplusplus
}
#endif
