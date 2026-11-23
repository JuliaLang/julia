// This file is a part of Julia. License is MIT: https://julialang.org/license

// Maps the dylib of a package image without dyld.
//
// dyld rebuilds its description of every loaded image on each `dlopen`, so the cost of
// loading a library grows with the number of libraries already loaded. A package image
// needs little from dyld: its segments mapped, its chained fixups applied and its unwind
// tables findable.

#include <fcntl.h>
#include <dlfcn.h>
#include <pthread.h>
#include <sys/mman.h>
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/fixup-chains.h>

#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _CPU_AARCH64_
#define JL_MACHO_CPUTYPE CPU_TYPE_ARM64
#else
#define JL_MACHO_CPUTYPE CPU_TYPE_X86_64
#endif

typedef struct {
    _Atomic(uintptr_t) base; // 0 once unmapped
    size_t size;
    const char *path;
    uintptr_t unwind_info, eh_frame;
    size_t unwind_info_size, eh_frame_size;
    const struct nlist_64 *syms;
    uint32_t nsyms;
    const char *strtab;
    uint32_t strsize;
} macho_image_t;

// Read from signal handlers and by the unwinder of a suspended thread, so entries are
// never moved or freed.
#define MACHO_CHUNK 256
#define MACHO_NCHUNKS 256
static _Atomic(macho_image_t*) macho_chunks[MACHO_NCHUNKS];
static _Atomic(size_t) macho_nimages;
static pthread_mutex_t macho_lock = PTHREAD_MUTEX_INITIALIZER;

static macho_image_t *macho_find(uintptr_t addr) JL_NOTSAFEPOINT
{
    size_t n = jl_atomic_load_acquire(&macho_nimages);
    for (size_t i = 0; i < n; i++) {
        macho_image_t *img = &jl_atomic_load_relaxed(&macho_chunks[i / MACHO_CHUNK])[i % MACHO_CHUNK];
        uintptr_t base = jl_atomic_load_relaxed(&img->base);
        if (base && addr - base < img->size)
            return img;
    }
    return NULL;
}

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

static int macho_find_unwind_sections(uintptr_t addr, struct unw_dynamic_unwind_sections *info) JL_NOTSAFEPOINT
{
    macho_image_t *img = macho_find(addr);
    if (img == NULL)
        return 0;
    info->dso_base = jl_atomic_load_relaxed(&img->base);
    info->dwarf_section = img->eh_frame;
    info->dwarf_section_length = img->eh_frame_size;
    info->compact_unwind_section = img->unwind_info;
    info->compact_unwind_section_length = img->unwind_info_size;
    return 1;
}

static void macho_register_unwinder(void) JL_NOTSAFEPOINT
{
    __unw_add_find_dynamic_unwind_sections(macho_find_unwind_sections);
    // C++ exceptions unwind with the copy in libSystem
    void *sys = dlopen("/usr/lib/system/libunwind.dylib", RTLD_NOLOAD | RTLD_LAZY);
    unw_add_find_dynamic_unwind_sections add = NULL;
    if (sys)
        add = (unw_add_find_dynamic_unwind_sections)dlsym(sys, "__unw_add_find_dynamic_unwind_sections");
    if (add && add != &__unw_add_find_dynamic_unwind_sections)
        add(macho_find_unwind_sections);
}

JL_DLLEXPORT int jl_macho_image_for_addr(uintptr_t addr, uint64_t *base, const char **path,
                                         void **saddr, const char **sname) JL_NOTSAFEPOINT
{
    macho_image_t *img = macho_find(addr);
    if (img == NULL)
        return 0;
    uintptr_t ibase = jl_atomic_load_relaxed(&img->base);
    if (base)
        *base = ibase;
    if (path)
        *path = img->path;
    if (saddr || sname) {
        uintptr_t best = 0;
        const char *name = NULL;
        for (uint32_t i = 0; i < img->nsyms; i++) {
            const struct nlist_64 *nl = &img->syms[i];
            if ((nl->n_type & N_STAB) || (nl->n_type & N_TYPE) != N_SECT || nl->n_un.n_strx >= img->strsize)
                continue;
            uintptr_t a = ibase + nl->n_value;
            if (a <= addr && a >= best) {
                best = a;
                name = img->strtab + nl->n_un.n_strx;
            }
        }
        if (name && name[0] == '_')
            name++;
        if (saddr)
            *saddr = (void*)best;
        if (sname)
            *sname = name;
    }
    return 1;
}

void jl_macho_unmap_image(uint64_t base) JL_NOTSAFEPOINT
{
    macho_image_t *img = macho_find((uintptr_t)base);
    if (img == NULL)
        return;
    jl_atomic_store_release(&img->base, 0);
    munmap((void*)(uintptr_t)base, img->size);
}

static void *macho_lib_handle(const char *install_name) JL_NOTSAFEPOINT
{
    static void *libsystem = NULL;
    const char *name = strrchr(install_name, '/');
    name = name ? name + 1 : install_name;
    if (strncmp(name, "libjulia-internal", 17) == 0)
        return jl_libjulia_internal_handle;
    if (strncmp(name, "libjulia", 8) == 0)
        return jl_libjulia_handle;
    if (strncmp(name, "libSystem.", 10) == 0) {
        if (libsystem == NULL)
            libsystem = dlopen("/usr/lib/libSystem.B.dylib", RTLD_NOLOAD | RTLD_LAZY);
        return libsystem;
    }
    return NULL;
}

#define MACHO_MAX_LIBS 16
#define MACHO_MAX_SEGS 8

// Bind and rebase the pointers of the image mapped at `base`.
static int macho_apply_fixups(char *base, size_t size, const struct dyld_chained_fixups_header *fh,
                              size_t fhsize, void **libs, int nlibs) JL_NOTSAFEPOINT
{
    if (fhsize < sizeof(*fh) || fh->fixups_version != 0 || fh->symbols_format != 0 ||
        (fh->imports_count && fh->imports_format != DYLD_CHAINED_IMPORT) ||
        fh->starts_offset > fhsize - sizeof(struct dyld_chained_starts_in_image) ||
        fh->imports_offset > fhsize || fh->symbols_offset >= fhsize ||
        fh->imports_count > (fhsize - fh->imports_offset) / sizeof(struct dyld_chained_import))
        return 0;
    const char *symbols = (const char*)fh + fh->symbols_offset;
    size_t symbols_size = fhsize - fh->symbols_offset;
    const struct dyld_chained_import *imports = (const struct dyld_chained_import*)((const char*)fh + fh->imports_offset);
    void **bound = (void**)malloc_s((fh->imports_count + 1) * sizeof(void*));
    int ok = 1;
    for (uint32_t i = 0; ok && i < fh->imports_count; i++) {
        const char *name = symbols + imports[i].name_offset;
        if (imports[i].name_offset >= symbols_size || memchr(name, 0, symbols_size - imports[i].name_offset) == NULL) {
            ok = 0;
            break;
        }
        if (name[0] == '_')
            name++;
        int ordinal = (int8_t)imports[i].lib_ordinal;
        void *lib = ordinal >= 1 && ordinal <= nlibs ? libs[ordinal - 1] : NULL;
        bound[i] = dlsym(lib ? lib : RTLD_DEFAULT, name);
        if (bound[i] == NULL && !imports[i].weak_import)
            ok = 0;
    }
    const struct dyld_chained_starts_in_image *starts = (const struct dyld_chained_starts_in_image*)((const char*)fh + fh->starts_offset);
    for (uint32_t s = 0; ok && s < starts->seg_count; s++) {
        if ((const char*)&starts->seg_info_offset[s + 1] > (const char*)fh + fhsize) {
            ok = 0;
            break;
        }
        uint32_t off = starts->seg_info_offset[s];
        if (off == 0)
            continue;
        const struct dyld_chained_starts_in_segment *seg = (const struct dyld_chained_starts_in_segment*)((const char*)starts + off);
        if (off > fhsize - fh->starts_offset - sizeof(*seg) ||
            (seg->pointer_format != DYLD_CHAINED_PTR_64 && seg->pointer_format != DYLD_CHAINED_PTR_64_OFFSET) ||
            (const char*)&seg->page_start[seg->page_count] > (const char*)fh + fhsize) {
            ok = 0;
            break;
        }
        for (uint32_t p = 0; ok && p < seg->page_count; p++) {
            if (seg->page_start[p] == DYLD_CHAINED_PTR_START_NONE)
                continue;
            uint64_t offset = seg->segment_offset + (uint64_t)p * seg->page_size + seg->page_start[p];
            while (1) {
                if (offset > size - sizeof(uint64_t)) {
                    ok = 0;
                    break;
                }
                uint64_t raw;
                memcpy(&raw, base + offset, sizeof(raw));
                uint64_t next = (raw >> 51) & 0xfff;
                uint64_t value;
                if (raw >> 63) { // dyld_chained_ptr_64_bind
                    uint32_t ordinal = raw & 0xffffff;
                    if (ordinal >= fh->imports_count) {
                        ok = 0;
                        break;
                    }
                    value = (uint64_t)bound[ordinal] + ((raw >> 24) & 0xff);
                }
                else { // dyld_chained_ptr_64_rebase; the image is linked at address 0
                    value = (uint64_t)base + (raw & 0xfffffffffULL) + (((raw >> 36) & 0xff) << 56);
                }
                memcpy(base + offset, &value, sizeof(value));
                if (next == 0)
                    break;
                offset += next * 4;
            }
        }
    }
    free(bound);
    return ok;
}

// Returns 0 if the file is not a dylib this loader handles; the caller then uses `dlopen`.
int jl_macho_map_image(const char *path, jl_macho_image_info_t *info) JL_NOTSAFEPOINT
{
    static int unwinder_registered = 0;
    char *cmds = NULL, *base = (char*)MAP_FAILED;
    const char *le, *strtab;
    size_t size = 0, off = 0, n;
    size_t pagesz = jl_getpagesize();
    struct mach_header_64 mh;
    const struct segment_command_64 *segs[MACHO_MAX_SEGS];
    const struct segment_command_64 *linkedit = NULL;
    const struct linkedit_data_command *fixups = NULL, *signature = NULL;
    const struct symtab_command *symtab = NULL;
    const struct dysymtab_command *dysymtab = NULL;
    const struct nlist_64 *syms;
    void *libs[MACHO_MAX_LIBS];
    int nsegs = 0, nlibs = 0;
    uintptr_t unwind_info = 0, eh_frame = 0;
    size_t unwind_info_size = 0, eh_frame_size = 0;
    macho_image_t *chunk, *img;
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd == -1)
        return 0;
    if (pread(fd, &mh, sizeof(mh), 0) != sizeof(mh) || mh.magic != MH_MAGIC_64 ||
        mh.cputype != JL_MACHO_CPUTYPE || mh.filetype != MH_DYLIB || mh.sizeofcmds > (1 << 20))
        goto unsupported;
    cmds = (char*)malloc_s(mh.sizeofcmds);
    if (pread(fd, cmds, mh.sizeofcmds, sizeof(mh)) != (ssize_t)mh.sizeofcmds)
        goto unsupported;

    for (uint32_t i = 0; i < mh.ncmds; i++) {
        const struct load_command *lc = (const struct load_command*)(cmds + off);
        if (off + sizeof(*lc) > mh.sizeofcmds || lc->cmdsize < sizeof(*lc) || lc->cmdsize > mh.sizeofcmds - off)
            goto unsupported;
        off += lc->cmdsize;
        switch (lc->cmd) {
        case LC_SEGMENT_64: {
            const struct segment_command_64 *seg = (const struct segment_command_64*)lc;
            if (lc->cmdsize < sizeof(*seg) || seg->nsects > (lc->cmdsize - sizeof(*seg)) / sizeof(struct section_64) ||
                nsegs == MACHO_MAX_SEGS || (seg->vmaddr | seg->fileoff) % pagesz != 0 ||
                seg->filesize > seg->vmsize || seg->vmaddr != size)
                goto unsupported;
            int is_linkedit = strncmp(seg->segname, "__LINKEDIT", 16) == 0;
            if (is_linkedit)
                linkedit = seg;
            else if (seg->filesize % pagesz != 0 || seg->vmsize % pagesz != 0)
                goto unsupported;
            const struct section_64 *sects = (const struct section_64*)(seg + 1);
            for (uint32_t j = 0; j < seg->nsects; j++) {
                switch (sects[j].flags & SECTION_TYPE) {
                case S_MOD_INIT_FUNC_POINTERS: case S_MOD_TERM_FUNC_POINTERS: case S_INIT_FUNC_OFFSETS:
                case S_THREAD_LOCAL_REGULAR: case S_THREAD_LOCAL_ZEROFILL: case S_THREAD_LOCAL_VARIABLES:
                case S_THREAD_LOCAL_VARIABLE_POINTERS: case S_THREAD_LOCAL_INIT_FUNCTION_POINTERS:
                case S_LAZY_SYMBOL_POINTERS: case S_LAZY_DYLIB_SYMBOL_POINTERS:
                    goto unsupported;
                }
                if (strncmp(sects[j].segname, "__TEXT", 16) != 0 || sects[j].addr < seg->vmaddr ||
                    sects[j].size > seg->vmsize || sects[j].addr - seg->vmaddr > seg->vmsize - sects[j].size)
                    continue;
                if (strncmp(sects[j].sectname, "__unwind_info", 16) == 0) {
                    unwind_info = sects[j].addr;
                    unwind_info_size = sects[j].size;
                }
                else if (strncmp(sects[j].sectname, "__eh_frame", 16) == 0) {
                    eh_frame = sects[j].addr;
                    eh_frame_size = sects[j].size;
                }
            }
            segs[nsegs++] = seg;
            size += LLT_ALIGN(seg->vmsize, pagesz);
            break;
        }
        case LC_LOAD_DYLIB:
        case LC_LOAD_WEAK_DYLIB: {
            const struct dylib_command *dl = (const struct dylib_command*)lc;
            if (lc->cmdsize < sizeof(*dl) || dl->dylib.name.offset >= lc->cmdsize ||
                memchr((const char*)dl + dl->dylib.name.offset, 0, lc->cmdsize - dl->dylib.name.offset) == NULL ||
                nlibs == MACHO_MAX_LIBS)
                goto unsupported;
            libs[nlibs++] = macho_lib_handle((const char*)dl + dl->dylib.name.offset);
            break;
        }
        case LC_DYLD_CHAINED_FIXUPS:
            fixups = (const struct linkedit_data_command*)lc;
            break;
        case LC_CODE_SIGNATURE:
            signature = (const struct linkedit_data_command*)lc;
            break;
        case LC_SYMTAB:
            symtab = (const struct symtab_command*)lc;
            break;
        case LC_DYSYMTAB:
            dysymtab = (const struct dysymtab_command*)lc;
            break;
        case LC_DYLD_INFO: case LC_DYLD_INFO_ONLY: case LC_REEXPORT_DYLIB: case LC_LOAD_UPWARD_DYLIB:
        case LC_ROUTINES_64: case LC_ENCRYPTION_INFO_64:
            goto unsupported;
        }
    }
    if (nsegs == 0 || linkedit == NULL || linkedit != segs[nsegs - 1] || segs[0]->fileoff != 0 ||
        fixups == NULL || symtab == NULL || dysymtab == NULL || size == 0)
        goto unsupported;
#define IN_LINKEDIT(o, n) ((uint64_t)(o) >= linkedit->fileoff && (uint64_t)(n) <= linkedit->filesize && \
                           (uint64_t)(o) - linkedit->fileoff <= linkedit->filesize - (uint64_t)(n))
    if (!IN_LINKEDIT(fixups->dataoff, fixups->datasize) ||
        !IN_LINKEDIT(symtab->symoff, (uint64_t)symtab->nsyms * sizeof(struct nlist_64)) ||
        !IN_LINKEDIT(symtab->stroff, symtab->strsize) || symtab->strsize == 0 ||
        (uint64_t)dysymtab->iextdefsym + dysymtab->nextdefsym > symtab->nsyms)
        goto unsupported;

    if (signature) {
        // attach the code signature, without which the kernel refuses to run the pages
        fsignatures_t sig = {0, (void*)(uintptr_t)signature->dataoff, signature->datasize};
        if (fcntl(fd, F_ADDFILESIGS_RETURN, &sig) == -1)
            goto unsupported;
    }
#ifdef _CPU_AARCH64_
    else {
        goto unsupported;
    }
#endif

    base = (char*)mmap(NULL, size, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (base == (char*)MAP_FAILED)
        goto unsupported;
    for (int i = 0; i < nsegs; i++) {
        const struct segment_command_64 *seg = segs[i];
        int prot = seg->initprot & (PROT_READ | PROT_WRITE | PROT_EXEC);
        if ((prot & PROT_WRITE) && (prot & PROT_EXEC))
            goto unsupported;
        if (seg->filesize &&
            mmap(base + seg->vmaddr, seg->filesize, prot, MAP_PRIVATE | MAP_FIXED, fd, seg->fileoff) == MAP_FAILED)
            goto unsupported;
        size_t filepages = LLT_ALIGN(seg->filesize, pagesz);
        if (seg->vmsize > filepages && mprotect(base + seg->vmaddr + filepages, seg->vmsize - filepages, prot) == -1)
            goto unsupported;
    }
    le = base + linkedit->vmaddr - linkedit->fileoff;
    if (!macho_apply_fixups(base, linkedit->vmaddr, (const struct dyld_chained_fixups_header*)(le + fixups->dataoff),
                            fixups->datasize, libs, nlibs))
        goto unsupported;
    for (int i = 0; i < nsegs; i++) {
        if (segs[i]->flags & SG_READ_ONLY)
            mprotect(base + segs[i]->vmaddr, segs[i]->vmsize, PROT_READ);
    }

    memset(info, 0, sizeof(*info));
    info->base = (uint64_t)(uintptr_t)base;
    syms = (const struct nlist_64*)(le + symtab->symoff);
    strtab = le + symtab->stroff;
    if (strtab[symtab->strsize - 1] != 0)
        goto unsupported;
    for (uint32_t i = dysymtab->iextdefsym; i < dysymtab->iextdefsym + dysymtab->nextdefsym; i++) {
        if (syms[i].n_un.n_strx >= symtab->strsize || syms[i].n_value >= linkedit->vmaddr)
            continue;
        const char *name = strtab + syms[i].n_un.n_strx;
        void *addr = base + syms[i].n_value;
        if (strcmp(name, "_jl_image_pointers") == 0)
            info->pointers = addr;
        else if (strcmp(name, "_jl_system_image_checksum") == 0)
            info->checksum = (const uint32_t*)addr;
        else if (strcmp(name, "_jl_image_unpack") == 0)
            info->unpack = *(void**)addr;
        else if (strcmp(name, "_jl_image_coverage") == 0)
            info->coverage = addr;
    }
    if (info->pointers == NULL || info->checksum == NULL || info->unpack == NULL)
        goto unsupported;

    pthread_mutex_lock(&macho_lock);
    n = jl_atomic_load_relaxed(&macho_nimages);
    if (n == MACHO_CHUNK * MACHO_NCHUNKS) {
        pthread_mutex_unlock(&macho_lock);
        goto unsupported;
    }
    chunk = jl_atomic_load_relaxed(&macho_chunks[n / MACHO_CHUNK]);
    if (chunk == NULL) {
        chunk = (macho_image_t*)calloc_s(MACHO_CHUNK * sizeof(macho_image_t));
        jl_atomic_store_release(&macho_chunks[n / MACHO_CHUNK], chunk);
    }
    img = &chunk[n % MACHO_CHUNK];
    img->size = size;
    img->path = strdup(path);
    img->unwind_info = unwind_info_size ? (uintptr_t)base + unwind_info : 0;
    img->unwind_info_size = unwind_info_size;
    img->eh_frame = eh_frame_size ? (uintptr_t)base + eh_frame : 0;
    img->eh_frame_size = eh_frame_size;
    img->syms = syms;
    img->nsyms = symtab->nsyms;
    img->strtab = strtab;
    img->strsize = symtab->strsize;
    jl_atomic_store_relaxed(&img->base, (uintptr_t)base);
    jl_atomic_store_release(&macho_nimages, n + 1);
    if (!unwinder_registered) {
        unwinder_registered = 1;
        macho_register_unwinder();
    }
    pthread_mutex_unlock(&macho_lock);
    close(fd);
    free(cmds);
    return 1;

unsupported:
    if (base != (char*)MAP_FAILED)
        munmap(base, size);
    close(fd);
    free(cmds);
    return 0;
}

#ifdef __cplusplus
}
#endif
