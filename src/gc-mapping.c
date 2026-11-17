// This file is a part of Julia. License is MIT: https://julialang.org/license

// Mappings for Memory that can grow in place: a private mapping holding a header page, then the
// data. Only claimed bytes count towards the heap; the rest is reserved address space, committed
// as it is claimed.

#include "gc-common.h"
#include "gc-stock.h"
#include "gc-alloc-profiler.h"
#include "julia_gcext.h"

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

typedef struct _jl_memory_mapping_t {
    _Atomic(size_t) mapped;  // bytes of address space, header included
    _Atomic(size_t) claimed; // data bytes handed out to Memory objects, and whether sealed
    _Atomic(size_t) live;    // data bytes reached by the Memory objects marked since the last trim
    _Atomic(size_t) used;    // data bytes ever claimed; those past them still read as zero
    struct _jl_memory_mapping_t *next; // while cached, the mapping freed before it
    uint64_t freed;          // while cached, when it was freed (jl_hrtime)
    int discarded;           // while cached, whether its pages were handed to the system
} jl_memory_mapping_t;

// Set in `claimed` once the bytes must never grow. Expanding compares `claimed` to the size it
// grows from, so this stops it in the one step that sealing takes.
#define JL_MEMORY_MAPPING_SEALED ((size_t)1 << (8 * sizeof(size_t) - 1))
#define JL_MEMORY_MAPPING_HEADROOM_MAX ((size_t)2 << 30)

// Budgets that leave mappings and address space to malloc, thread stacks and libraries; zero
// disables mapping
static _Atomic(size_t) memory_mappings, memory_mapping_bytes;
static size_t memory_mappings_max, memory_mapping_bytes_max;
// Room to grow reserved in each mapping
static size_t memory_mapping_headroom;

// Freed mappings are kept for reuse, as malloc keeps freed memory, so that their pages need
// not be faulted in again. A mapping still unused at the next sweep hands its pages to the
// system, which may then take them; discarding at once would make a prompt reuse refault them.
#define JL_MEMORY_MAPPING_CACHE_NS ((uint64_t)1000000000)
#define JL_MEMORY_MAPPING_CACHE_BYTES_MAX ((size_t)1 << 30)
static jl_memory_mapping_t *memory_mapping_cache; // the most recently freed first
static size_t memory_mapping_cache_bytes_max; // of claimed bytes, which may be resident
static uint64_t memory_mapping_cache_swept; // when the cache was last swept (jl_hrtime)
static uv_mutex_t memory_mapping_cache_lock;

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

// Let the system take the pages when it needs memory; until then they stay usable
static void os_discard(void *p, size_t sz) JL_NOTSAFEPOINT
{
#if defined(_OS_WINDOWS_)
    VirtualAlloc(p, sz, MEM_RESET, PAGE_READWRITE);
#elif defined(MADV_FREE)
    madvise(p, sz, MADV_FREE);
#else
    madvise(p, sz, MADV_DONTNEED);
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
    uv_mutex_init(&memory_mapping_cache_lock);
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
    uint64_t memory = uv_get_total_memory(), constrained = uv_get_constrained_memory();
    if (constrained > 0 && constrained < memory)
        memory = constrained;
    memory_mapping_cache_bytes_max = memory / 16;
    if (memory_mapping_cache_bytes_max > JL_MEMORY_MAPPING_CACHE_BYTES_MAX)
        memory_mapping_cache_bytes_max = JL_MEMORY_MAPPING_CACHE_BYTES_MAX;
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

static void memory_mapping_decommit(jl_memory_mapping_t *mapping, size_t from, size_t to) JL_NOTSAFEPOINT
{
    size_t start = LLT_ALIGN(JL_MEMORY_MAPPING_HEADER + from, jl_page_size);
    size_t end = LLT_ALIGN(JL_MEMORY_MAPPING_HEADER + to, jl_page_size);
    if (end > start)
        os_decommit((char*)mapping + start, end - start);
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
    memory_mapping_decommit(mapping, live, claimed);
    return claimed - live;
}

static void memory_mapping_release(jl_memory_mapping_t *mapping) JL_NOTSAFEPOINT
{
    size_t mapped = jl_atomic_load_relaxed(&mapping->mapped);
    os_release(mapping, mapped);
    memory_mapping_give(mapped);
}

// Release the cached mappings freed at least `age` ago, and the oldest past the size cap. The
// cache must be locked.
static void memory_mapping_cache_trim(uint64_t age) JL_NOTSAFEPOINT
{
    uint64_t now = jl_hrtime();
    size_t bytes = 0;
    jl_memory_mapping_t **link = &memory_mapping_cache;
    while (*link != NULL) {
        jl_memory_mapping_t *mapping = *link;
        size_t claimed = jl_atomic_load_relaxed(&mapping->claimed);
        if (now - mapping->freed < age && bytes + claimed <= memory_mapping_cache_bytes_max) {
            bytes += claimed;
            link = &mapping->next;
        }
        else {
            *link = mapping->next;
            memory_mapping_release(mapping);
        }
    }
}

size_t memory_mapping_free(void *data) JL_NOTSAFEPOINT
{
    jl_memory_mapping_t *mapping = memory_mapping(data);
    size_t claimed = jl_atomic_load_relaxed(&mapping->claimed) & ~JL_MEMORY_MAPPING_SEALED;
    jl_atomic_store_relaxed(&mapping->claimed, claimed); // unsealed, for the cache to count
    if (claimed > memory_mapping_cache_bytes_max) {
        memory_mapping_release(mapping);
        return JL_MEMORY_MAPPING_HEADER + claimed;
    }
    mapping->freed = jl_hrtime();
    mapping->discarded = 0;
    uv_mutex_lock(&memory_mapping_cache_lock);
    mapping->next = memory_mapping_cache;
    memory_mapping_cache = mapping;
    memory_mapping_cache_trim(UINT64_MAX);
    uv_mutex_unlock(&memory_mapping_cache_lock);
    return JL_MEMORY_MAPPING_HEADER + claimed;
}

// Take the smallest cached mapping of at least `mapped` bytes
static jl_memory_mapping_t *memory_mapping_reuse(size_t mapped) JL_NOTSAFEPOINT
{
    jl_memory_mapping_t **best = NULL;
    uv_mutex_lock(&memory_mapping_cache_lock);
    for (jl_memory_mapping_t **link = &memory_mapping_cache; *link != NULL; link = &(*link)->next) {
        size_t size = jl_atomic_load_relaxed(&(*link)->mapped);
        if (size >= mapped && (best == NULL || size < jl_atomic_load_relaxed(&(*best)->mapped)))
            best = link;
    }
    jl_memory_mapping_t *mapping = NULL;
    if (best != NULL) {
        mapping = *best;
        *best = mapping->next;
    }
    uv_mutex_unlock(&memory_mapping_cache_lock);
    return mapping;
}

// Release the mappings cached for too long, or all of them when `forced`, as by GC.gc()
void memory_mapping_cache_sweep(int forced) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&memory_mapping_cache_lock);
    if (forced)
        memory_mapping_cache_trim(0);
    else
        memory_mapping_cache_trim(JL_MEMORY_MAPPING_CACHE_NS);
    for (jl_memory_mapping_t *mapping = memory_mapping_cache; mapping != NULL; mapping = mapping->next) {
        if (mapping->freed < memory_mapping_cache_swept && !mapping->discarded) {
            size_t claimed = jl_atomic_load_relaxed(&mapping->claimed);
            os_discard((char*)mapping + JL_MEMORY_MAPPING_HEADER, LLT_ALIGN(claimed, jl_page_size));
            mapping->discarded = 1;
        }
    }
    memory_mapping_cache_swept = jl_hrtime();
    uv_mutex_unlock(&memory_mapping_cache_lock);
}

// Claim the data bytes up to `to`. If `zeroed`, zero those from `from` that were claimed
// before; the rest were never written.
static void memory_mapping_use(jl_memory_mapping_t *mapping, size_t from, size_t to, int zeroed) JL_NOTSAFEPOINT
{
    size_t used = jl_atomic_load_relaxed(&mapping->used);
    size_t written = to; // bytes before this may have been written
    if (to > used) {
        written = used;
        jl_atomic_store_relaxed(&mapping->used, to);
    }
    if (zeroed && written > from)
        memset((char*)mapping + JL_MEMORY_MAPPING_HEADER + from, 0, written - from);
}

// Map `sz` claimed bytes and return their address, or NULL when over budget
static void *memory_mapping_new(size_t sz) JL_NOTSAFEPOINT
{
    size_t mapped = memory_mapping_size(sz);
    if (mapped == 0)
        return NULL;
    SAVE_ERRNO;
    jl_memory_mapping_t *mapping = memory_mapping_reuse(mapped);
    int reused = mapping != NULL;
    if (reused) {
        mapped = jl_atomic_load_relaxed(&mapping->mapped);
        // else its last owner's pages past `sz` stay resident, uncounted
        memory_mapping_decommit(mapping, sz, jl_atomic_load_relaxed(&mapping->claimed));
    }
    else if (memory_mapping_take(mapped)) {
        mapping = (jl_memory_mapping_t*)os_reserve(mapped);
        if (mapping == NULL)
            memory_mapping_give(mapped);
    }
    if (mapping != NULL && !memory_mapping_commit(mapping, 0, JL_MEMORY_MAPPING_HEADER + sz)) {
        os_release(mapping, mapped);
        memory_mapping_give(mapped);
        mapping = NULL;
    }
    RESTORE_ERRNO;
    if (mapping == NULL)
        return NULL;
    jl_atomic_store_relaxed(&mapping->mapped, mapped);
    jl_atomic_store_relaxed(&mapping->claimed, sz);
    jl_atomic_store_relaxed(&mapping->live, 0);
    if (!reused)
        jl_atomic_store_relaxed(&mapping->used, 0);
    memory_mapping_use(mapping, 0, sz, 0);
    return (char*)mapping + JL_MEMORY_MAPPING_HEADER;
}

// Claim the bytes of the mapped `data` from `oldsz` to `newsz`, which read as zero if `zeroed`.
// Only the Memory ending at the claimed bytes may grow, so no two can share new bytes.
static int memory_mapping_expand(void *data, size_t oldsz, size_t newsz, int zeroed) JL_NOTSAFEPOINT
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
    memory_mapping_use(mapping, oldsz, newsz, zeroed);
    return 1;
}

// Stop the mapped `data` of `sz` claimed bytes from growing. Returns 0 if it has grown already,
// or has been sealed.
static int memory_mapping_seal(void *data, size_t sz) JL_NOTSAFEPOINT
{
    size_t claimed = sz;
    return jl_atomic_cmpswap(&memory_mapping(data)->claimed, &claimed, sz | JL_MEMORY_MAPPING_SEALED);
}
#else
void memory_mapping_init(void) JL_NOTSAFEPOINT {}
void memory_mapping_cache_sweep(int forced) JL_NOTSAFEPOINT {}
void memory_mapping_mark(void *data, size_t sz) JL_NOTSAFEPOINT {}
size_t memory_mapping_trim(void *data) JL_NOTSAFEPOINT { return 0; }
static void *memory_mapping_new(size_t sz) JL_NOTSAFEPOINT { return NULL; }
static int memory_mapping_expand(void *data, size_t oldsz, size_t newsz, int zeroed) JL_NOTSAFEPOINT { return 0; }
static int memory_mapping_seal(void *data, size_t sz) JL_NOTSAFEPOINT { return 1; }

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

// An expandable object is a big object at the start of a mapping, which claims exactly its
// header and the object's bytes; the GC frees it as it does any other big object.
JL_DLLEXPORT jl_value_t *jl_gc_alloc_expandable(jl_ptls_t ptls, size_t sz, void *ty) JL_CANSAFEPOINT
{
    maybe_collect(ptls);
    size_t allocsz = offsetof(bigval_t, header) + sz;
    bigval_t *v = (bigval_t*)memory_mapping_new(allocsz);
    if (v == NULL)
        return NULL;
    gc_invoke_callbacks(jl_gc_cb_notify_external_alloc_t,
        gc_cblist_notify_external_alloc, (v, allocsz));
    gc_count_managed_bytes(ptls, allocsz);
    jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.bigalloc,
        jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.bigalloc) + 1);
    v->sz = allocsz;
    v->mapped = 1;
#ifndef NDEBUG
    v->header = 0;
#endif
    gc_big_object_link(ptls->gc_tls.heap.young_generation_of_bigvals, v);
    jl_value_t *val = jl_valueof(&v->header);
    jl_set_typeof(val, ty);
    maybe_record_alloc_to_profile(val, sz, (jl_datatype_t*)ty);
    return val;
}

// Only big objects outside images have a bigval_t header to consult
static bigval_t *expandable_header(jl_value_t *val, size_t sz) JL_NOTSAFEPOINT
{
    if (sz <= GC_MAX_SZCLASS || jl_object_in_image(val))
        return NULL;
    bigval_t *v = bigval_header(jl_astaggedvalue(val));
    if (!v->mapped)
        return NULL;
    return v;
}

JL_DLLEXPORT int jl_gc_expand(jl_value_t *val, size_t oldsz, size_t newsz) JL_CANSAFEPOINT
{
    bigval_t *v = expandable_header(val, oldsz);
    size_t offs = offsetof(bigval_t, header);
    if (v == NULL || !jl_gc_managed_expand(v, offs + oldsz, offs + newsz, 0))
        return 0;
    v->sz = offs + newsz;
    return 1;
}

JL_DLLEXPORT int jl_gc_seal(jl_value_t *val, size_t sz) JL_NOTSAFEPOINT
{
    bigval_t *v = expandable_header(val, sz);
    return v == NULL || memory_mapping_seal(v, offsetof(bigval_t, header) + sz);
}

JL_DLLEXPORT int jl_gc_managed_expand(void *data, size_t oldsz, size_t newsz, int zeroed) JL_CANSAFEPOINT
{
    jl_ptls_t ptls = jl_current_task->ptls;
    maybe_collect(ptls);
    if (!memory_mapping_expand(data, oldsz, newsz, zeroed))
        return 0;
    gc_count_managed_bytes(ptls, newsz - oldsz);
    maybe_record_alloc_to_profile((jl_value_t*)((char*)data + oldsz), newsz - oldsz, (jl_datatype_t*)jl_buff_tag);
    return 1;
}

#ifdef __cplusplus
}
#endif
