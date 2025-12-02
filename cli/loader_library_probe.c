#include "../src/support/platform.h"
#include "loader.h"

#ifdef _OS_LINUX_

#include <stdio.h>
#include <assert.h>

#include <stddef.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>

#include <sys/stat.h>
#include <sys/mman.h>
#include <linux/limits.h>

#include "dl-cache.h"

void *jl_loader_open_via_mmap(const char *filepath, size_t *size)
{
    int fd;
    while (1) {
        fd = open(filepath, O_CLOEXEC | O_RDONLY);
        if (fd >= 0) {
            break;
        } else if (errno != EINTR) {
            return NULL;
        }
    }

    struct stat info;
    while (1) {
        int err = fstat(fd, &info);
        if (err >= 0) {
            break;
        } else if (errno != EINTR) {
            close(fd);
            return NULL;
        }
    }

    void *buffer = mmap(
        NULL, info.st_size, PROT_READ, MAP_PRIVATE | MAP_NORESERVE, fd, /* offset */ 0
    );
    close(fd);

    if (MAP_FAILED == buffer)
        return NULL;

    *size = info.st_size;
    return buffer;
}

static const char *search_ldcache_new(struct cache_file_new *cache, const char *libname, size_t *index)
{
    if (strncmp(cache->magic, CACHEMAGIC_NEW, sizeof(CACHEMAGIC_NEW) - 1) != 0)
        return NULL;

    for (; *index < cache->nlibs; (*index)++) {
        struct file_entry_new *lib = &cache->libs[*index];

        const char *strtab = (const char *)cache;
        const char *key = &strtab[lib->key];
        const char *value = &strtab[lib->value];

        if (!_dl_cache_check_flags(lib->flags))
            continue;
        if (strcmp(key, libname) != 0)
            continue;
        (*index)++;
        return value;
    }

    return NULL;
}

static const char *search_ldcache(struct cache_file *cache, size_t cachesize, const char *libname, size_t *index)
{
    if (strncmp(cache->magic, CACHEMAGIC, sizeof(CACHEMAGIC) - 1) != 0)
        return search_ldcache_new((struct cache_file_new *)cache, libname, index);

    // check for an embedded / hybrid 'new'-style cache
    size_t offset = ALIGN_CACHE(sizeof(struct cache_file) + cache->nlibs * sizeof(struct file_entry));
    if (cachesize >= offset + sizeof(struct cache_file_new)) {
        struct cache_file_new *new_cache = (struct cache_file_new *)((void *)cache + offset);
        if (strncmp(new_cache->magic, CACHEMAGIC_NEW, sizeof(CACHEMAGIC_NEW) - 1) == 0)
            return search_ldcache_new(new_cache, libname, index);
    }

    for (; *index < cache->nlibs; (*index)++) {
        struct file_entry *lib = &cache->libs[*index];

        const char *strtab = (const char *)&cache->libs[cache->nlibs];
        const char *key = &strtab[lib->key];
        const char *value = &strtab[lib->value];

        if (!_dl_cache_check_flags(lib->flags))
            continue;
        if (strcmp(key, libname) != 0)
            continue;
        (*index)++;
        return value;
    }

    return NULL;
}

const char *ldcache_dirs[] = {
    "/etc/ld.so.cache",
};

const char *default_libdirs[] = {
    "/lib/",
    "/usr/lib/",
#ifdef _P64
    "/lib64/",
    "/usr/lib64/",
#endif
};

/**
 * Search for a system library with the filename `libname` containing `symbol`.
 * Return NULL if no matching library could be found.
 *
 * To emulate the Linux dynamic linker search behavior, this function scans for
 * system libraries in:
 *   1. LD_LIBRARY_PATH
 *   2. `/etc/ld.so.cache`
 *   3. "default" system libdirs (/lib, /usr/lib, etc.)
 *
 * This function does not consider any DT_RPATH or DT_RUNPATH entries.
 * (see `ld.so(8)` manpage)
 **/
const char *jl_loader_probe_system_library(const char *libname, const char *symbol)
{
    char buf[PATH_MAX];

    // Make a best-effort attempt to emulate the linker's use of LD_LIBRARY_PATH
    char *LD_LIBRARY_PATH = getenv("LD_LIBRARY_PATH");
    if (LD_LIBRARY_PATH != NULL) {
        LD_LIBRARY_PATH = strdup(LD_LIBRARY_PATH);
        char *path = LD_LIBRARY_PATH;

        int last = 0;
        while (!last) {
            // walk to next ':' or '\0'
            char *ch = path;
            while (1) {
                if (*ch == '\0')
                    last = 1;
                if (*ch == ':' || *ch == '\0')
                    break;
                ch += 1;
            }
            *ch = '\0';

            if (ch == path) {
                path += 1;
                continue;
            }

            int bytes = snprintf(buf, sizeof(buf), (ch[-1] == '/' ? "%s%s" : "%s/%s"), path, libname);
            path = ch + 1;
            if (bytes < 0 || sizeof(buf) < (size_t) bytes)
                continue;

            if (jl_loader_locate_symbol(buf, symbol)) {
                free(LD_LIBRARY_PATH);
                return strdup(buf);
            }
        }
        free(LD_LIBRARY_PATH);
    }

    // Check the ld.so.cache for the library. Assuming we can find the cache,
    // this is by far our best chance to locate the lib successfully.
    size_t npaths = sizeof(ldcache_dirs) / sizeof(const char *);
    for (size_t i = 0; i < npaths; i++) {
        size_t sz;
        struct cache_file *cache =
            (struct cache_file *)jl_loader_open_via_mmap(ldcache_dirs[i], &sz);

        if (cache == NULL)
            continue; // ld.so.cache was not found (could be NixOS)

        size_t iter = 0;
        const char *library;
        while ((library = search_ldcache(cache, sz, libname, &iter)) != NULL) {
            if (jl_loader_locate_symbol(library, symbol)) {
                library = strdup(library);
                munmap((void *)cache, sz);
                return library;
            }
        }

        munmap((void *)cache, sz);
    }

    // As a last-ditch effort, try to emulate / search the "default" libdirs used
    // by the GLIBC dynamic linker.
    size_t ndirs = sizeof(default_libdirs) / sizeof(const char *);
    for (size_t i = 0; i < ndirs; i++) {
        int bytes = snprintf(buf, sizeof(buf), "%s%s", default_libdirs[i], libname);
        if (bytes < 0 || sizeof(buf) < (size_t) bytes)
            continue;
        if (jl_loader_locate_symbol(buf, symbol))
            return strdup(buf);
    }

    return NULL;
}

#endif
