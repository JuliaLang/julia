// This file is a part of Julia. License is MIT: https://julialang.org/license

// Loads the native code section of a .ji file (see jinative.h) without the
// dynamic loader of the system: map the segments from the file, add the load
// address to the rebase slots, store the addresses of the imports in the bind
// slots, and tell the unwinder where the unwind tables are.

#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"
#include "jinative.h"
#include <stdio.h>
#include <zstd.h>

#ifdef _OS_WINDOWS_
#include <windows.h>
#else
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
            *sname = found && syms[lo - 1].name < h->strtab_size ? img->meta + h->strtab_off + syms[lo - 1].name : NULL;
    }
    return 1;
}

// The number of bytes from `addr` to the end of the function it is in, or 0
JL_DLLEXPORT size_t jl_native_image_symbol_size(uintptr_t addr) JL_NOTSAFEPOINT
{
    jl_native_image_t *img = native_find(addr);
    if (img == NULL)
        return 0;
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
    if (lo == 0 || off - syms[lo - 1].addr >= syms[lo - 1].size)
        return 0;
    return syms[lo - 1].size - (off - syms[lo - 1].addr);
}

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

#ifdef _OS_WINDOWS_
#if defined(_CPU_X86_64_)
extern void ___chkstk_ms(void);
#endif
static void *native_libgcc_handle(void) JL_NOTSAFEPOINT
{
    static void *handle = NULL;
    if (handle == NULL)
        handle = jl_dlopen(JL_LIBGCC_NAME, JL_RTLD_NOLOAD);
    return handle;
}
// The linker is given openlibm for the images: code for a CPU without SSE4.1
// calls e.g. `floor` and `rint`, which the JIT never needs for the CPU it runs on.
static void *native_libm_handle;
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
#ifdef _OS_WINDOWS_
    // where the JIT finds the symbols that code refers to (see JuliaOJIT::JuliaOJIT)
    void *handles[] = {jl_libjulia_internal_handle, jl_libjulia_handle, jl_exe_handle, native_libgcc_handle(),
                       native_libm_handle, jl_ntdll_handle, jl_kernel32_handle, jl_crtdll_handle, jl_winsock_handle};
#if defined(_CPU_X86_64_)
    // part of the static libgcc, which a linker puts into every image that needs it
    if (strcmp(name, "___chkstk_ms") == 0)
        addr = (void*)&___chkstk_ms;
#endif
#else
    // RTLD_DEFAULT is a null pointer on some systems, which the loop skips; the
    // handle of the program searches the same libraries there
    void *handles[] = {jl_libjulia_internal_handle, jl_libjulia_handle, jl_exe_handle, jl_RTLD_DEFAULT_handle};
#endif
    for (size_t h = 0; addr == NULL && h < sizeof(handles) / sizeof(handles[0]); h++) {
        if (handles[h] != NULL)
            jl_dlsym(handles[h], name, &addr, 0, /* search_deps */ 1);
    }
    if (addr == NULL)
        return NULL;
    if (import_cache_count < IMPORT_CACHE_SIZE / 2) {
        import_cache[i].name = strdup(name);
        import_cache[i].addr = addr;
        import_cache_count++;
    }
    return addr;
}

static jl_mutex_t native_lock;

static int native_env_flag(const char *name, int dflt) JL_NOTSAFEPOINT
{
    const char *v = getenv(name);
    return v ? atoi(v) != 0 : dflt;
}

// Debuggers learn about code from the dynamic loader, which does not know these
// images. With ENABLE_GDBLISTENER=1, as for code from the JIT, describe each
// linked object to them through the GDB JIT interface: a relocatable ELF file in
// memory that has the address of the code, the function symbols, the unwind
// tables and the debug info with its addresses moved to where the image is.
typedef struct {
    unsigned char e_ident[16];
    uint16_t e_type, e_machine;
    uint32_t e_version;
    uint64_t e_entry, e_phoff, e_shoff;
    uint32_t e_flags;
    uint16_t e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx;
} native_elf_ehdr_t;
typedef struct {
    uint32_t sh_name, sh_type;
    uint64_t sh_flags, sh_addr, sh_offset, sh_size;
    uint32_t sh_link, sh_info;
    uint64_t sh_addralign, sh_entsize;
} native_elf_shdr_t;
typedef struct {
    uint32_t st_name;
    unsigned char st_info, st_other;
    uint16_t st_shndx;
    uint64_t st_value, st_size;
} native_elf_sym_t;

static arraylist_t native_gdb_objects;

static void native_register_debugger(jl_native_image_t *img) JL_CANSAFEPOINT
{
    const jl_native_header_t *h = img->hdr;
    const jl_native_object_t *objs = (const jl_native_object_t*)(img->meta + h->objects_off);
    const jl_native_sym_t *syms = (const jl_native_sym_t*)(img->meta + h->syms_off);
    const jl_native_debug_section_t *dsecs = (const jl_native_debug_section_t*)(img->meta + h->debug_off);
    const char *strtab = img->meta + h->strtab_off;
    uint64_t slide = (uintptr_t)img->base - h->link_base;
    for (uint32_t o = 0; o < h->nobjects; o++) {
        const jl_native_object_t *obj = &objs[o];
        size_t lo = 0, hi = h->nsyms;
        while (lo < hi) { // first symbol of the object
            size_t mid = (lo + hi) / 2;
            if (syms[mid].addr < obj->text_start)
                lo = mid + 1;
            else
                hi = mid;
        }
        size_t first = lo, nsym = 0, strsz = 1;
        for (size_t i = first; i < h->nsyms && syms[i].addr < obj->text_end; i++) {
            if (syms[i].name >= h->strtab_size)
                break; // damaged: leave the rest out
            nsym++;
            strsz += strlen(strtab + syms[i].name) + 1;
        }
        int has_eh = obj->eh_frame_size != 0;
        uint32_t nsec = 2 + has_eh + obj->ndebug + 3;
        size_t shstrsz = 1 + sizeof(".text") + (has_eh ? sizeof(".eh_frame") : 0) + sizeof(".symtab") +
                         sizeof(".strtab") + sizeof(".shstrtab");
        size_t size = sizeof(native_elf_ehdr_t) + nsec * sizeof(native_elf_shdr_t) + obj->eh_frame_size;
        for (uint32_t i = 0; i < obj->ndebug; i++) {
            shstrsz += strlen(strtab + dsecs[obj->debug + i].name) + 2;
            const jl_native_debug_section_t *ds = &dsecs[obj->debug + i];
            size += LLT_ALIGN(ds->usize ? ds->usize : ds->size, 8);
        }
        size = LLT_ALIGN(size, 8) + (nsym + 1) * sizeof(native_elf_sym_t) + strsz + shstrsz;
        char *buf = (char*)calloc(1, size);
        native_elf_ehdr_t *eh = (native_elf_ehdr_t*)buf;
        native_elf_shdr_t *sh = (native_elf_shdr_t*)(buf + sizeof(*eh));
        memcpy(eh->e_ident, "\177ELF\2\1\1", 7);
        eh->e_type = 1; // ET_REL
#ifdef _CPU_AARCH64_
        eh->e_machine = 183;
#else
        eh->e_machine = 62;
#endif
        eh->e_version = 1;
        eh->e_shoff = sizeof(*eh);
        eh->e_ehsize = sizeof(*eh);
        eh->e_shentsize = sizeof(*sh);
        eh->e_shnum = nsec;
        eh->e_shstrndx = nsec - 1;
        size_t pos = sizeof(*eh) + nsec * sizeof(*sh);
        char *shstr = buf + size - shstrsz;
        size_t shstrpos = 1;
        uint32_t si = 1;
#define NATIVE_SHNAME(name) (strcpy(shstr + shstrpos, name), shstrpos += strlen(name) + 1, shstrpos - strlen(name) - 1)
        sh[si].sh_name = NATIVE_SHNAME(".text");
        sh[si].sh_type = 8; // SHT_NOBITS: the code is where sh_addr says
        sh[si].sh_flags = 2 | 4; // SHF_ALLOC | SHF_EXECINSTR
        sh[si].sh_addr = (uintptr_t)img->base + obj->text_start;
        sh[si].sh_offset = pos;
        sh[si].sh_size = obj->text_end - obj->text_start;
        sh[si].sh_addralign = 4;
        si++;
        if (has_eh) {
            sh[si].sh_name = NATIVE_SHNAME(".eh_frame");
            sh[si].sh_type = 1; // SHT_PROGBITS
            sh[si].sh_flags = 2;
            sh[si].sh_addr = (uintptr_t)img->base + obj->eh_frame;
            sh[si].sh_offset = pos;
            sh[si].sh_size = obj->eh_frame_size;
            sh[si].sh_addralign = 8;
            memcpy(buf + pos, img->base + obj->eh_frame, obj->eh_frame_size);
            pos += obj->eh_frame_size;
            si++;
        }
        for (uint32_t i = 0; i < obj->ndebug; i++) {
            const jl_native_debug_section_t *ds = &dsecs[obj->debug + i];
            pos = LLT_ALIGN(pos, 8);
            shstr[shstrpos] = '.';
            strcpy(shstr + shstrpos + 1, strtab + ds->name);
            sh[si].sh_name = shstrpos;
            shstrpos += strlen(strtab + ds->name) + 2;
            sh[si].sh_type = 1;
            sh[si].sh_offset = pos;
            size_t dsize = ds->usize ? ds->usize : ds->size;
            sh[si].sh_size = dsize;
            sh[si].sh_addralign = 1;
            if (ds->usize)
                ZSTD_decompress(buf + pos, dsize, img->meta + ds->off, ds->size);
            else
                memcpy(buf + pos, img->meta + ds->off, dsize);
            const uint32_t *slides = (const uint32_t*)(img->meta + ds->slides_off);
            for (uint32_t j = 0; j < ds->nslides; j++) {
                if (slides[j] > dsize - sizeof(uint64_t))
                    continue;
                uint64_t v;
                memcpy(&v, buf + pos + slides[j], sizeof(v));
                v += slide;
                memcpy(buf + pos + slides[j], &v, sizeof(v));
            }
            pos += dsize;
            si++;
        }
        pos = LLT_ALIGN(pos, 8);
        native_elf_sym_t *es = (native_elf_sym_t*)(buf + pos);
        char *str = buf + pos + (nsym + 1) * sizeof(*es);
        size_t strpos = 1;
        for (size_t i = 0; i < nsym; i++) {
            const char *name = strtab + syms[first + i].name;
            es[i + 1].st_name = strpos;
            strcpy(str + strpos, name);
            strpos += strlen(name) + 1;
            es[i + 1].st_info = (1 << 4) | 2; // STB_GLOBAL, STT_FUNC
            es[i + 1].st_shndx = 1;
            es[i + 1].st_value = syms[first + i].addr - obj->text_start; // relative to the section
            es[i + 1].st_size = syms[first + i].size;
        }
        sh[si].sh_name = NATIVE_SHNAME(".symtab");
        sh[si].sh_type = 2; // SHT_SYMTAB
        sh[si].sh_offset = pos;
        sh[si].sh_size = (nsym + 1) * sizeof(*es);
        sh[si].sh_link = si + 1;
        sh[si].sh_info = 1;
        sh[si].sh_addralign = 8;
        sh[si].sh_entsize = sizeof(*es);
        si++;
        sh[si].sh_name = NATIVE_SHNAME(".strtab");
        sh[si].sh_type = 3; // SHT_STRTAB
        sh[si].sh_offset = (str - buf);
        sh[si].sh_size = strsz;
        sh[si].sh_addralign = 1;
        si++;
        sh[si].sh_name = NATIVE_SHNAME(".shstrtab");
        sh[si].sh_type = 3;
        sh[si].sh_offset = (shstr - buf);
        sh[si].sh_size = shstrsz;
        sh[si].sh_addralign = 1;
#undef NATIVE_SHNAME
        assert(si == nsec - 1 && strpos == strsz && shstrpos == shstrsz);
        // the debugger keeps reading from the buffer, so it stays for good
        JL_LOCK_NOGC(&native_lock);
        if (native_gdb_objects.items == NULL)
            arraylist_new(&native_gdb_objects, 0);
        arraylist_push(&native_gdb_objects, buf);
        JL_UNLOCK_NOGC(&native_lock);
        jl_register_gdb_object(buf, size);
    }
}

// With ENABLE_JITPROFILING=1, as for code from the JIT, tell perf the names of the functions
static void native_write_perf_map(jl_native_image_t *img) JL_NOTSAFEPOINT
{
#ifndef _OS_WINDOWS_
    static FILE *map = NULL;
    const jl_native_header_t *h = img->hdr;
    const jl_native_sym_t *syms = (const jl_native_sym_t*)(img->meta + h->syms_off);
    JL_LOCK_NOGC(&native_lock);
    if (map == NULL) {
        char path[64];
        snprintf(path, sizeof(path), "/tmp/perf-%d.map", (int)getpid());
        map = fopen(path, "a");
    }
    if (map != NULL) {
        for (uint32_t i = 0; i < h->nsyms && syms[i].name < h->strtab_size; i++)
            fprintf(map, "%lx %x %s\n", (unsigned long)((uintptr_t)img->base + syms[i].addr),
                    (unsigned)syms[i].size, img->meta + h->strtab_off + syms[i].name);
        fflush(map);
    }
    JL_UNLOCK_NOGC(&native_lock);
#endif
}

static uint64_t native_time[4]; // map, imports, fixups, registration (ns)
static uint64_t native_count[4]; // images, rebases, binds, bytes of text

JL_DLLEXPORT void jl_native_image_stats(uint64_t *time, uint64_t *count) JL_NOTSAFEPOINT
{
    memcpy(time, native_time, sizeof(native_time));
    memcpy(count, native_count, sizeof(native_count));
}

// The file is as trusted as a shared library would be, but a truncated or damaged
// one should fail to load instead of making the fixups write somewhere else.
static int native_header_ok(const jl_native_header_t *h, uint64_t avail) JL_NOTSAFEPOINT
{
    uint64_t ps = h->page_size;
    if (ps == 0 || (ps & (ps - 1)) != 0 || h->file_size > avail)
        return 0;
    // the segments are in this order and each starts on a page
    if (h->text_off != ps || h->rodata_off % ps || h->relro_off % ps || h->data_off % ps || h->vm_size % ps)
        return 0;
    if (h->text_off > h->rodata_off || h->text_size > h->rodata_off - h->text_off ||
        h->rodata_off > h->relro_off || h->rodata_size > h->relro_off - h->rodata_off ||
        h->relro_off > h->data_off || h->relro_size > h->data_off - h->relro_off ||
        h->data_off > h->vm_size || h->data_size > h->vm_size - h->data_off)
        return 0;
    // the tables are between the segments and the signature
    if (h->vm_size > h->codesig_off || h->codesig_off > h->file_size ||
        h->codesig_size > h->file_size - h->codesig_off)
        return 0;
#define NATIVE_TABLE_OK(off, n, size) \
    ((off) >= h->vm_size && (off) <= h->codesig_off && (uint64_t)(n) <= (h->codesig_off - (off)) / (size))
    if (!NATIVE_TABLE_OK(h->rebase_off, h->nrebase, 1) ||
        !NATIVE_TABLE_OK(h->bind_off, h->nbind, sizeof(jl_native_bind_t)) ||
        !NATIVE_TABLE_OK(h->imports_off, h->nimports, sizeof(uint32_t)) ||
        !NATIVE_TABLE_OK(h->syms_off, h->nsyms, sizeof(jl_native_sym_t)) ||
        !NATIVE_TABLE_OK(h->objects_off, h->nobjects, sizeof(jl_native_object_t)) ||
        !NATIVE_TABLE_OK(h->debug_off, h->ndebug, sizeof(jl_native_debug_section_t)) ||
        !NATIVE_TABLE_OK(h->strtab_off, h->strtab_size, 1) || h->strtab_size == 0)
        return 0;
#undef NATIVE_TABLE_OK
    return h->image_pointers >= h->text_off && h->image_pointers < h->vm_size;
}

static int native_read(ios_t *f, void *buf, size_t n, uint64_t off) JL_NOTSAFEPOINT
{
    return ios_seek(f, off) == 0 && ios_readall(f, (char*)buf, n) == n;
}

// What unwinding and backtraces index with: the objects and their debug sections
static int native_tables_ok(const jl_native_header_t *h, const char *meta) JL_NOTSAFEPOINT
{
    const jl_native_object_t *objs = (const jl_native_object_t*)(meta + h->objects_off);
    const jl_native_debug_section_t *dsecs = (const jl_native_debug_section_t*)(meta + h->debug_off);
    for (uint32_t i = 0; i < h->nobjects; i++) {
        const jl_native_object_t *o = &objs[i];
        if (o->text_start > o->text_end || o->text_end > h->vm_size ||
            o->eh_frame > h->vm_size || o->eh_frame_size > h->vm_size - o->eh_frame ||
            o->unwind_info > h->vm_size || o->unwind_info_size > h->vm_size - o->unwind_info ||
            o->debug > h->ndebug || o->ndebug > h->ndebug - o->debug)
            return 0;
    }
    for (uint32_t i = 0; i < h->ndebug; i++) {
        const jl_native_debug_section_t *d = &dsecs[i];
        if (d->off < h->vm_size || d->off > h->codesig_off || d->size > h->codesig_off - d->off ||
            d->slides_off < h->vm_size || d->slides_off > h->codesig_off ||
            d->nslides > (h->codesig_off - d->slides_off) / sizeof(uint32_t) || d->name >= h->strtab_size)
            return 0;
    }
    return 1;
}

// Load the native code section at offset `off` of the open file `fd`, which this
// takes over; `path` is what backtraces show. Returns NULL, with the reason in
// `err`, if it cannot be loaded.
jl_native_image_t *jl_native_image_load(const char *path, int fd, uint64_t off, const char **err) JL_CANSAFEPOINT
{
    jl_native_header_t hdr;
    jl_native_image_t *img = NULL;
    // `meta` is such that the offsets of the tables, which follow the segments in
    // the file, can be used as they are; `meta_mem` is where they really are
    char *base = NULL, *meta = NULL, *meta_mem = NULL;
    void **imports = NULL;
    size_t pagesz = jl_getpagesize(), text_end = 0, meta_size = 0;
    int perf = 0, copy = 0, mapped = 0, unresolved = 0;
    const uint32_t *import_names;
    const uint8_t *p, *pend;
    const jl_native_bind_t *binds;
    const jl_native_object_t *objs;
    const char *strtab;
    uint64_t t0 = jl_hrtime(), t1, t2, t3, t4, slot = 0;
    int64_t fsize;
    size_t n;
    jl_native_image_t *chunk;
#ifdef _OS_DARWIN_
    fsignatures_t sig;
#endif
#ifdef JL_DEBUG_BUILD
    int gdb_default = 1;
#else
    int gdb_default = 0;
#endif
    ios_t f;
    if (ios_fd(&f, fd, 1, 1) == NULL) {
        *err = "cannot open the file";
        return NULL;
    }
    ios_bufmode(&f, bm_none);
    hdr.vm_size = hdr.codesig_off = 0;
    *err = "not a native code section";
    fsize = ios_filesize(&f);
    if (fsize < 0 || off > (uint64_t)fsize || !native_read(&f, &hdr, sizeof(hdr), off) ||
        memcmp(hdr.magic, JL_NATIVE_MAGIC, sizeof(hdr.magic)) != 0 || hdr.version != JL_NATIVE_VERSION)
        goto fail;
    *err = "native code section is damaged";
    if (!native_header_ok(&hdr, (uint64_t)fsize - off))
        goto fail;
    *err = "native code section is laid out for another page size";
    if (hdr.page_size % pagesz != 0 || off % pagesz != 0)
        goto fail;
    *err = "cannot map the native code section";
    text_end = hdr.rodata_off;
    meta_size = hdr.codesig_off - hdr.vm_size;
    // perf only looks symbols up in its map file for code that is not in a file
    perf = native_env_flag("ENABLE_JITPROFILING", 0);
    copy = perf || getenv("JULIA_NATIVE_IMAGE_COPY") != NULL;
#ifdef _OS_WINDOWS_
    // TODO: map the code from the file (MapViewOfFile3 onto a placeholder), so
    // that it is shared between processes. For now everything is a private copy.
    (void)copy; (void)mapped;
    base = (char*)VirtualAlloc(NULL, hdr.vm_size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    if (base == NULL || !native_read(&f, base, hdr.vm_size, off))
        goto fail;
    meta_mem = (char*)malloc(meta_size);
    if (meta_mem == NULL || !native_read(&f, meta_mem, meta_size, off + hdr.vm_size))
        goto fail;
#else
    base = (char*)mmap(NULL, hdr.vm_size, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (base == MAP_FAILED) {
        base = NULL;
        goto fail;
    }
    // The header and the code, shared with every process that has the file mapped
#ifdef _OS_DARWIN_
    // The kernel only maps signed files executable; the signature is the last part of the section
    memset(&sig, 0, sizeof(sig));
    sig.fs_file_start = off;
    sig.fs_blob_start = (void*)(uintptr_t)hdr.codesig_off;
    sig.fs_blob_size = hdr.codesig_size;
    mapped = hdr.codesig_size != 0 && !copy && fcntl(f.fd, F_ADDFILESIGS_RETURN, &sig) != -1;
#else
    mapped = !copy;
#endif
    if (mapped)
        mapped = mmap(base, text_end, PROT_READ | PROT_EXEC, MAP_PRIVATE | MAP_FIXED, f.fd, off) != MAP_FAILED;
    if (!mapped) {
        // e.g. a file system that is mounted noexec: use a private copy
        if (mprotect(base, text_end, PROT_READ | PROT_WRITE) != 0 ||
            !native_read(&f, base, text_end, off) ||
            mprotect(base, text_end, PROT_READ | PROT_EXEC) != 0)
            goto fail;
#ifdef _OS_DARWIN_
        sys_icache_invalidate(base, text_end);
#endif
    }
    if (hdr.relro_off > hdr.rodata_off &&
        mmap(base + hdr.rodata_off, hdr.relro_off - hdr.rodata_off, PROT_READ, MAP_PRIVATE | MAP_FIXED,
             f.fd, off + hdr.rodata_off) == MAP_FAILED)
        goto fail;
    // Everything that is fixed up; the first part of it is made read-only afterwards
    if (hdr.vm_size > hdr.relro_off &&
        mmap(base + hdr.relro_off, hdr.vm_size - hdr.relro_off, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_FIXED,
             f.fd, off + hdr.relro_off) == MAP_FAILED)
        goto fail;
    meta_mem = (char*)mmap(NULL, meta_size, PROT_READ, MAP_PRIVATE, f.fd, off + hdr.vm_size);
    if (meta_mem == MAP_FAILED) {
        meta_mem = NULL;
        goto fail;
    }
#endif
    meta = meta_mem - hdr.vm_size;
    t1 = jl_hrtime();

    *err = "native code section is damaged";
    strtab = meta + hdr.strtab_off;
    if (strtab[hdr.strtab_size - 1] != 0 || !native_tables_ok(&hdr, meta))
        goto fail;
    import_names = (const uint32_t*)(meta + hdr.imports_off);
    imports = (void**)malloc(sizeof(void*) * (hdr.nimports + 1));
#ifdef _OS_WINDOWS_
    // loading a library can be a safepoint, so not while the imports are resolved
    if (native_libm_handle == NULL)
        native_libm_handle = jl_load_dynamic_library("libopenlibm", JL_RTLD_DEFAULT, /* throw_err */ 0);
#endif
    JL_LOCK_NOGC(&native_lock);
    for (uint32_t i = 0; i < hdr.nimports; i++) {
        imports[i] = import_names[i] < hdr.strtab_size ? native_resolve_import(strtab + import_names[i]) : NULL;
        if (imports[i] == NULL) {
            // TODO: unconditional while this is brought up on CI
            if (import_names[i] < hdr.strtab_size)
                jl_safe_printf("native image %s: cannot resolve %s\n", path, strtab + import_names[i]);
            unresolved = 1;
        }
    }
    JL_UNLOCK_NOGC(&native_lock);
    if (unresolved) {
        *err = "native code refers to a symbol that this process does not have";
        goto fail;
    }
    t2 = jl_hrtime();

    // every slot is in the two segments that are still writable
    *err = "native code section is damaged";
    p = (const uint8_t*)(meta + hdr.rebase_off);
    pend = (const uint8_t*)(meta + hdr.codesig_off);
    for (uint32_t i = 0; i < hdr.nrebase; i++) {
        uint64_t delta = 0;
        unsigned shift = 0;
        uint8_t b;
        do {
            if (p == pend || shift > 63)
                goto fail;
            b = *p++;
            delta |= (uint64_t)(b & 0x7f) << shift;
            shift += 7;
        } while (b & 0x80);
        slot += delta;
        if (slot < hdr.relro_off || slot > hdr.vm_size - sizeof(uint64_t))
            goto fail;
        uint64_t v;
        memcpy(&v, base + slot, sizeof(v));
        v += (uintptr_t)base;
        memcpy(base + slot, &v, sizeof(v));
    }
    binds = (const jl_native_bind_t*)(meta + hdr.bind_off);
    for (uint32_t i = 0; i < hdr.nbind; i++) {
        if (binds[i].import >= hdr.nimports || binds[i].slot < hdr.relro_off ||
            binds[i].slot > hdr.vm_size - sizeof(uint64_t))
            goto fail;
        uint64_t v = (uintptr_t)imports[binds[i].import] + binds[i].addend;
        memcpy(base + binds[i].slot, &v, sizeof(v));
    }
    free(imports);
    imports = NULL;
    *err = "cannot map the native code section";
#ifdef _OS_WINDOWS_
    {
        DWORD old;
        if (!VirtualProtect(base, text_end, PAGE_EXECUTE_READ, &old) ||
            (hdr.data_off > text_end && !VirtualProtect(base + text_end, hdr.data_off - text_end, PAGE_READONLY, &old)))
            goto fail;
        FlushInstructionCache(GetCurrentProcess(), base, text_end);
    }
#else
    if (hdr.data_off > hdr.relro_off)
        mprotect(base + hdr.relro_off, hdr.data_off - hdr.relro_off, PROT_READ);
#endif
    t3 = jl_hrtime();

    JL_LOCK_NOGC(&native_lock);
    n = jl_atomic_load_relaxed(&native_nimages);
    if (n == NATIVE_CHUNK * NATIVE_NCHUNKS) {
        JL_UNLOCK_NOGC(&native_lock);
        *err = "too many native images";
        goto fail;
    }
    chunk = jl_atomic_load_relaxed(&native_chunks[n / NATIVE_CHUNK]);
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
    objs = (const jl_native_object_t*)(meta + hdr.objects_off);
    (void)objs;
#if defined(_OS_DARWIN_)
    static int unwinder_registered = 0;
    if (!unwinder_registered) {
        unwinder_registered = 1;
        native_register_unwinder();
    }
#elif defined(_OS_WINDOWS_)
#if defined(_CPU_X86_64_)
    // the function tables of each object, sorted by the prelinker
    for (uint32_t i = 0; i < hdr.nobjects; i++) {
        if (objs[i].unwind_info_size != 0)
            RtlAddFunctionTable((PRUNTIME_FUNCTION)(base + objs[i].unwind_info),
                                (DWORD)(objs[i].unwind_info_size / sizeof(RUNTIME_FUNCTION)), (DWORD64)(uintptr_t)base);
    }
#endif
#else
    // TODO: the prelinker can write the lookup table that this builds
    for (uint32_t i = 0; i < hdr.nobjects; i++) {
        if (objs[i].eh_frame_size != 0)
            jl_register_eh_frames((uint8_t*)base + objs[i].eh_frame, objs[i].eh_frame_size);
    }
#endif
    t4 = jl_hrtime();
    native_time[0] += t1 - t0;
    native_time[1] += t2 - t1;
    native_time[2] += t3 - t2;
    native_time[3] += t4 - t3;
    native_count[0] += 1;
    native_count[1] += hdr.nrebase;
    native_count[2] += hdr.nbind;
    native_count[3] += hdr.text_size;
    JL_UNLOCK_NOGC(&native_lock);
    ios_close(&f);
    if (native_env_flag("ENABLE_GDBLISTENER", gdb_default))
        native_register_debugger(img);
    if (perf)
        native_write_perf_map(img);
    *err = NULL;
    return img;

fail:
    // TODO: unconditional while this is brought up on CI
    jl_safe_printf("native image %s: %s\n", path, *err);
    free(imports);
#ifdef _OS_WINDOWS_
    free(meta_mem);
    if (base != NULL)
        VirtualFree(base, 0, MEM_RELEASE);
#else
    if (meta_mem != NULL)
        munmap(meta_mem, meta_size);
    if (base != NULL)
        munmap(base, hdr.vm_size);
#endif
    ios_close(&f);
    return NULL;
}

#ifdef __cplusplus
}
#endif
