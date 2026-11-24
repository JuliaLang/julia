// This file is a part of Julia. License is MIT: https://julialang.org/license

// Loads the native code section of a .ji file (see jinative.h) without the
// dynamic loader of the system: map three segments from the file, add the load
// address to the rebase slots, store the addresses of the imports in the bind
// slots, and tell the unwinder where the unwind tables are.

#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"
#include "jinative.h"

#ifndef _OS_WINDOWS_
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <dlfcn.h>
#include <pthread.h>
#include <signal.h>
#endif
#ifdef _OS_DARWIN_
#include <libkern/OSCacheControl.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

// Read from signal handlers and by the unwinder of a suspended thread, so entries are
// never moved or freed.
#define NATIVE_CHUNK 256
#define NATIVE_NCHUNKS 1024
static _Atomic(jl_native_image_t*) native_chunks[NATIVE_NCHUNKS];
static _Atomic(size_t) native_nimages;

static jl_native_image_t *native_find(uintptr_t addr) JL_NOTSAFEPOINT
{
    size_t n = jl_atomic_load_acquire(&native_nimages);
    for (size_t i = 0; i < n; i++) {
        jl_native_image_t *img = &jl_atomic_load_relaxed(&native_chunks[i / NATIVE_CHUNK])[i % NATIVE_CHUNK];
        if (addr - (uintptr_t)img->base < img->hdr->vm_size)
            return img;
    }
    return NULL;
}

static const jl_native_object_t *native_find_object(jl_native_image_t *img, uintptr_t addr) JL_NOTSAFEPOINT
{
    const jl_native_header_t *h = img->hdr;
    const jl_native_object_t *objs = (const jl_native_object_t*)(img->meta + h->objects_off);
    uint64_t off = addr - (uintptr_t)img->base;
    for (uint32_t i = 0; i < h->nobjects; i++) {
        if (off >= objs[i].text_start && off < objs[i].text_end)
            return &objs[i];
    }
    return NULL;
}

// The image and the linked object that `addr` is in, for its debug info
JL_DLLEXPORT const jl_native_object_t *jl_native_object_for_addr(uintptr_t addr, jl_native_image_t **pimg) JL_NOTSAFEPOINT
{
    jl_native_image_t *img = native_find(addr);
    if (img == NULL)
        return NULL;
    *pimg = img;
    return native_find_object(img, addr);
}

// `dladdr` for the images loaded here
JL_DLLEXPORT int jl_native_image_for_addr(uintptr_t addr, uint64_t *base, const char **path,
                                          void **saddr, const char **sname) JL_NOTSAFEPOINT
{
    jl_native_image_t *img = native_find(addr);
    if (img == NULL)
        return 0;
    if (base)
        *base = (uintptr_t)img->base;
    if (path)
        *path = img->path;
    if (saddr || sname) {
        const jl_native_header_t *h = img->hdr;
        const jl_native_sym_t *syms = (const jl_native_sym_t*)(img->meta + h->syms_off);
        uint64_t off = addr - (uintptr_t)img->base;
        size_t lo = 0, hi = h->nsyms;
        while (lo < hi) { // last symbol that starts at or before `off`
            size_t mid = (lo + hi) / 2;
            if (syms[mid].addr <= off)
                lo = mid + 1;
            else
                hi = mid;
        }
        int found = lo > 0 && off - syms[lo - 1].addr < syms[lo - 1].size;
        if (saddr)
            *saddr = found ? img->base + syms[lo - 1].addr : NULL;
        if (sname)
            *sname = found ? img->meta + h->strtab_off + syms[lo - 1].name : NULL;
    }
    return 1;
}

#ifndef _OS_WINDOWS_

#ifdef _OS_DARWIN_
struct unw_dynamic_unwind_sections {
    uintptr_t dso_base;
    uintptr_t dwarf_section;
    size_t dwarf_section_length;
    uintptr_t compact_unwind_section;
    size_t compact_unwind_section_length;
};
typedef int (*unw_find_dynamic_unwind_sections)(uintptr_t addr, struct unw_dynamic_unwind_sections *info);
typedef int (*unw_add_find_dynamic_unwind_sections)(unw_find_dynamic_unwind_sections find) JL_NOTSAFEPOINT;
extern int __unw_add_find_dynamic_unwind_sections(unw_find_dynamic_unwind_sections find) JL_NOTSAFEPOINT;

static int native_find_unwind_sections(uintptr_t addr, struct unw_dynamic_unwind_sections *info) JL_NOTSAFEPOINT
{
    jl_native_image_t *img = native_find(addr);
    if (img == NULL)
        return 0;
    const jl_native_object_t *obj = native_find_object(img, addr);
    if (obj == NULL)
        return 0;
    info->dso_base = (uintptr_t)img->base;
    info->dwarf_section = obj->eh_frame_size ? (uintptr_t)img->base + obj->eh_frame : 0;
    info->dwarf_section_length = obj->eh_frame_size;
    info->compact_unwind_section = obj->unwind_info_size ? (uintptr_t)img->base + obj->unwind_info : 0;
    info->compact_unwind_section_length = obj->unwind_info_size;
    return 1;
}

static void native_register_unwinder(void) JL_NOTSAFEPOINT
{
    // C++ exceptions unwind with the copy in libSystem
    void *sys = dlopen("/usr/lib/system/libunwind.dylib", RTLD_NOLOAD | RTLD_LAZY);
    unw_add_find_dynamic_unwind_sections add = NULL;
    if (sys)
        add = (unw_add_find_dynamic_unwind_sections)dlsym(sys, "__unw_add_find_dynamic_unwind_sections");
    // libunwind registers under a lock that unwinding takes too, so keep the profiler
    // and signal handlers from unwinding while this thread holds it
    int havelock = jl_lock_profile_wr();
    sigset_t sset, oset;
    sigfillset(&sset);
    pthread_sigmask(SIG_BLOCK, &sset, &oset);
    __unw_add_find_dynamic_unwind_sections(native_find_unwind_sections);
    if (add && add != &__unw_add_find_dynamic_unwind_sections)
        add(native_find_unwind_sections);
    pthread_sigmask(SIG_SETMASK, &oset, NULL);
    if (havelock)
        jl_unlock_profile_wr();
}
#endif

// Addresses of the symbols that images import, which are the same few in every image
#define IMPORT_CACHE_SIZE 4096 // power of 2
static struct { const char *name; void *addr; } import_cache[IMPORT_CACHE_SIZE];
static size_t import_cache_count;

static void *native_resolve_import(const char *name) JL_NOTSAFEPOINT
{
    size_t i = memhash(name, strlen(name)) & (IMPORT_CACHE_SIZE - 1);
    while (import_cache[i].name != NULL) {
        if (strcmp(import_cache[i].name, name) == 0)
            return import_cache[i].addr;
        i = (i + 1) & (IMPORT_CACHE_SIZE - 1);
    }
    void *addr = NULL;
    if (!jl_dlsym(jl_libjulia_internal_handle, name, &addr, 0, 0) &&
        !jl_dlsym(jl_libjulia_handle, name, &addr, 0, 0) &&
        !jl_dlsym(jl_RTLD_DEFAULT_handle, name, &addr, 0, 0))
        return NULL;
    if (import_cache_count < IMPORT_CACHE_SIZE / 2) {
        import_cache[i].name = strdup(name);
        import_cache[i].addr = addr;
        import_cache_count++;
    }
    return addr;
}

static pthread_mutex_t native_lock = PTHREAD_MUTEX_INITIALIZER;

static uint64_t native_time[4]; // map, imports, fixups, registration (ns)
static uint64_t native_count[4]; // images, rebases, binds, bytes of text

JL_DLLEXPORT void jl_native_image_stats(uint64_t *time, uint64_t *count) JL_NOTSAFEPOINT
{
    memcpy(time, native_time, sizeof(native_time));
    memcpy(count, native_count, sizeof(native_count));
}

// Load the native code section at offset `off` of the file `path`. Returns NULL, with
// the reason in `err`, if it cannot be loaded.
jl_native_image_t *jl_native_image_load(const char *path, uint64_t off, const char **err) JL_NOTSAFEPOINT
{
    jl_native_header_t hdr;
    jl_native_image_t *img = NULL;
    char *base = (char*)MAP_FAILED, *meta = (char*)MAP_FAILED;
    void **imports = NULL;
    size_t pagesz = jl_getpagesize();
    uint64_t t0 = jl_hrtime();
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd == -1) {
        *err = "cannot open the file";
        return NULL;
    }
    *err = "not a native code section";
    if (pread(fd, &hdr, sizeof(hdr), off) != sizeof(hdr) ||
        memcmp(hdr.magic, JL_NATIVE_MAGIC, sizeof(hdr.magic)) != 0 || hdr.version != JL_NATIVE_VERSION)
        goto fail;
    *err = "native code section is laid out for another page size";
    if (hdr.page_size % pagesz != 0 || off % pagesz != 0)
        goto fail;
    *err = "cannot map the native code section";
    size_t text_end = LLT_ALIGN(hdr.text_off + hdr.text_size, hdr.page_size);
    assert(text_end == hdr.rodata_off);
    base = (char*)mmap(NULL, hdr.vm_size, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (base == MAP_FAILED)
        goto fail;
    // The header and the code, shared with every process that has the file mapped
    int mapped = 0;
#ifdef _OS_DARWIN_
    // The kernel only maps signed files executable; the signature is the last part of the section
    fsignatures_t sig;
    memset(&sig, 0, sizeof(sig));
    sig.fs_file_start = off;
    sig.fs_blob_start = (void*)(uintptr_t)hdr.codesig_off;
    sig.fs_blob_size = hdr.codesig_size;
    mapped = hdr.codesig_size != 0 && getenv("JULIA_NATIVE_IMAGE_COPY") == NULL &&
             fcntl(fd, F_ADDFILESIGS_RETURN, &sig) != -1;
#else
    mapped = 1;
#endif
    if (mapped)
        mapped = mmap(base, text_end, PROT_READ | PROT_EXEC, MAP_PRIVATE | MAP_FIXED, fd, off) != MAP_FAILED;
    if (!mapped) {
        // e.g. a file system that is mounted noexec: use a private copy
        if (mprotect(base, text_end, PROT_READ | PROT_WRITE) != 0 ||
            pread(fd, base, text_end, off) != (ssize_t)text_end ||
            mprotect(base, text_end, PROT_READ | PROT_EXEC) != 0)
            goto fail;
#ifdef _OS_DARWIN_
        sys_icache_invalidate(base, text_end);
#endif
    }
    if (hdr.data_off > hdr.rodata_off &&
        mmap(base + hdr.rodata_off, hdr.data_off - hdr.rodata_off, PROT_READ, MAP_PRIVATE | MAP_FIXED,
             fd, off + hdr.rodata_off) == MAP_FAILED)
        goto fail;
    if (hdr.vm_size > hdr.data_off &&
        mmap(base + hdr.data_off, hdr.vm_size - hdr.data_off, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_FIXED,
             fd, off + hdr.data_off) == MAP_FAILED)
        goto fail;
    // The tables follow the segments. `meta` is such that their offsets can be used as they are.
    size_t meta_size = hdr.codesig_off - hdr.vm_size;
    if (meta_size != 0) {
        meta = (char*)mmap(NULL, meta_size, PROT_READ, MAP_PRIVATE, fd, off + hdr.vm_size);
        if (meta == MAP_FAILED)
            goto fail;
        meta -= hdr.vm_size;
    }
    uint64_t t1 = jl_hrtime();

    const uint32_t *import_names = (const uint32_t*)(meta + hdr.imports_off);
    imports = (void**)malloc(sizeof(void*) * (hdr.nimports + 1));
    pthread_mutex_lock(&native_lock);
    for (uint32_t i = 0; i < hdr.nimports; i++) {
        imports[i] = native_resolve_import(meta + hdr.strtab_off + import_names[i]);
        if (imports[i] == NULL) {
            pthread_mutex_unlock(&native_lock);
            *err = "native code refers to a symbol that this process does not have";
            goto fail;
        }
    }
    pthread_mutex_unlock(&native_lock);
    uint64_t t2 = jl_hrtime();

    const uint8_t *p = (const uint8_t*)(meta + hdr.rebase_off);
    uint64_t slot = 0;
    for (uint32_t i = 0; i < hdr.nrebase; i++) {
        uint64_t delta = 0;
        unsigned shift = 0;
        uint8_t b;
        do {
            b = *p++;
            delta |= (uint64_t)(b & 0x7f) << shift;
            shift += 7;
        } while (b & 0x80);
        slot += delta;
        uint64_t v;
        memcpy(&v, base + slot, sizeof(v));
        v += (uintptr_t)base;
        memcpy(base + slot, &v, sizeof(v));
    }
    const jl_native_bind_t *binds = (const jl_native_bind_t*)(meta + hdr.bind_off);
    for (uint32_t i = 0; i < hdr.nbind; i++) {
        uint64_t v = (uintptr_t)imports[binds[i].import] + binds[i].addend;
        memcpy(base + binds[i].slot, &v, sizeof(v));
    }
    free(imports);
    imports = NULL;
    uint64_t t3 = jl_hrtime();

    pthread_mutex_lock(&native_lock);
    size_t n = jl_atomic_load_relaxed(&native_nimages);
    if (n == NATIVE_CHUNK * NATIVE_NCHUNKS) {
        pthread_mutex_unlock(&native_lock);
        *err = "too many native images";
        goto fail;
    }
    jl_native_image_t *chunk = jl_atomic_load_relaxed(&native_chunks[n / NATIVE_CHUNK]);
    if (chunk == NULL) {
        chunk = (jl_native_image_t*)calloc(NATIVE_CHUNK, sizeof(jl_native_image_t));
        jl_atomic_store_release(&native_chunks[n / NATIVE_CHUNK], chunk);
    }
    img = &chunk[n % NATIVE_CHUNK];
    img->base = base;
    img->hdr = (const jl_native_header_t*)base;
    img->meta = meta;
    img->path = strdup(path);
    jl_atomic_store_release(&native_nimages, n + 1);
#ifdef _OS_DARWIN_
    static int unwinder_registered = 0;
    if (!unwinder_registered) {
        unwinder_registered = 1;
        native_register_unwinder();
    }
#else
    // TODO: the prelinker can write the lookup table that this builds
    const jl_native_object_t *objs = (const jl_native_object_t*)(meta + hdr.objects_off);
    for (uint32_t i = 0; i < hdr.nobjects; i++) {
        if (objs[i].eh_frame_size != 0)
            jl_register_eh_frames((uint8_t*)base + objs[i].eh_frame, objs[i].eh_frame_size);
    }
#endif
    uint64_t t4 = jl_hrtime();
    native_time[0] += t1 - t0;
    native_time[1] += t2 - t1;
    native_time[2] += t3 - t2;
    native_time[3] += t4 - t3;
    native_count[0] += 1;
    native_count[1] += hdr.nrebase;
    native_count[2] += hdr.nbind;
    native_count[3] += hdr.text_size;
    pthread_mutex_unlock(&native_lock);
    close(fd);
    *err = NULL;
    return img;

fail:
    free(imports);
    if (meta != MAP_FAILED)
        munmap(meta + hdr.vm_size, hdr.codesig_off - hdr.vm_size);
    if (base != MAP_FAILED)
        munmap(base, hdr.vm_size);
    close(fd);
    return NULL;
}

#else // _OS_WINDOWS_

JL_DLLEXPORT void jl_native_image_stats(uint64_t *time, uint64_t *count) JL_NOTSAFEPOINT
{
    memset(time, 0, 4 * sizeof(uint64_t));
    memset(count, 0, 4 * sizeof(uint64_t));
}

jl_native_image_t *jl_native_image_load(const char *path, uint64_t off, const char **err) JL_NOTSAFEPOINT
{
    // TODO: CreateFileMapping(PAGE_EXECUTE_READ) + MapViewOfFileEx, RtlAddFunctionTable
    *err = "native code sections are not implemented on this platform";
    return NULL;
}

#endif

#ifdef __cplusplus
}
#endif
