// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

#include "julia.h"
#include "julia_internal.h"
#include "support/strhash.h"

static int codegen_imaging_mode(void) JL_NOTSAFEPOINT
{
    return jl_options.image_codegen || (jl_generating_output() && jl_options.use_pkgimages);
}

// Logging for code coverage and memory allocation

#define logdata_blocksize 32 // target getting nearby lines in the same general cache area and reducing calls to malloc by chunking
typedef _Atomic(uint64_t) logdata_counter_t;
typedef logdata_counter_t logdata_block[logdata_blocksize];

// Per-file line data: a growable array of logdata_block pointers, indexed by block number.
typedef struct {
    logdata_block **blocks;
    size_t len;
    size_t cap;
} logdata_vec_t;

// A logdata_t is a string-keyed hash table mapping filenames to logdata_vec_t.
// We store the logdata_vec_t pointer as the htable value.
typedef htable_t logdata_t;

static void logdata_vec_resize(logdata_vec_t *v, size_t newlen) JL_NOTSAFEPOINT
{
    if (newlen > v->cap) {
        size_t newcap = v->cap ? v->cap * 2 : 8;
        if (newcap < newlen) newcap = newlen;
        v->blocks = (logdata_block **)realloc_s(v->blocks, newcap * sizeof(logdata_block *));
        memset(v->blocks + v->cap, 0, (newcap - v->cap) * sizeof(logdata_block *));
        v->cap = newcap;
    }
    v->len = newlen;
}

static logdata_vec_t *logdata_get_or_create(logdata_t *ld, const char *filename) JL_NOTSAFEPOINT
{
    void **bp = strhash_bp(ld, (void *)filename);
    if (*bp == HT_NOTFOUND) {
        logdata_vec_t *v = (logdata_vec_t *)calloc_s(sizeof(logdata_vec_t));
        *bp = v;
    }
    return (logdata_vec_t *)*bp;
}

static uv_mutex_t coverage_lock;

static logdata_counter_t *allocLine(logdata_vec_t *vec, int line) JL_NOTSAFEPOINT
{
    unsigned block = line / logdata_blocksize;
    line = line % logdata_blocksize;
    if (vec->len <= block)
        logdata_vec_resize(vec, block + 1);
    jl_assume(vec->blocks != NULL);
    if (vec->blocks[block] == NULL) {
        vec->blocks[block] = (logdata_block *)calloc_s(sizeof(logdata_block));
    }
    logdata_block *data = vec->blocks[block];
    if (jl_atomic_load_relaxed(&(*data)[line]) == 0)
        jl_atomic_store_relaxed(&(*data)[line], 1);
    return &(*data)[line];
}

// Code coverage

static logdata_t coverageData;

// The JIT registers (runtime slot, module counter) pairs after linking. Module
// counters are folded into the per-line runtime slots before writing a report.
// JIT code is not unloaded, so the counter addresses remain valid.
static arraylist_t registered_counters;

// Fold the registered per-module counters into the canonical slots.
// The caller must hold coverage_lock.
static void fold_registered_counters(void) JL_NOTSAFEPOINT
{
    for (size_t i = 0; i < registered_counters.len; i += 2) {
        logdata_counter_t *slot = (logdata_counter_t*)registered_counters.items[i];
        logdata_counter_t *counter = (logdata_counter_t*)registered_counters.items[i + 1];
        uint64_t value = jl_atomic_load_relaxed(counter);
        if (value == 0)
            continue;
        if (jl_options.code_coverage_mode == JL_COVERAGE_MODE_HIT) {
            // Folding is idempotent, so the module counter need not be reset.
            jl_atomic_store_relaxed(slot, 2);
        }
        else {
            // Reset the delta after folding it. Concurrent updates can make
            // count mode approximate.
            jl_atomic_store_relaxed(counter, 0);
            jl_atomic_store_relaxed(slot, jl_atomic_load_relaxed(slot) + value);
        }
    }
}

static int is_skip_filename(const char *filename) JL_NOTSAFEPOINT
{
    if (!filename || filename[0] == '\0') return 1;
    if (strcmp(filename, "none") == 0) return 1;
    if (strcmp(filename, "no file") == 0) return 1;
    if (strcmp(filename, "<missing>") == 0) return 1;
    return 0;
}

JL_DLLEXPORT int jl_path_is_tracked(const char *path) JL_NOTSAFEPOINT
{
    const char *tracked = jl_options.tracked_path;
    if (tracked == NULL || path == NULL)
        return 0;
    size_t tlen = strlen(tracked);
    if (tlen == 0)
        return 1; // no path given: everything is tracked
    while (tlen > 0 && (tracked[tlen - 1] == '/' || tracked[tlen - 1] == PATHSEPSTRING[0]))
        tlen--;
    if (tlen == 0)
        return jl_isabspath(path); // the filesystem root: every absolute path
    if (strncmp(path, tracked, tlen) != 0)
        return 0;
    char next = path[tlen];
    return next == '\0' || next == '/' || next == PATHSEPSTRING[0];
}

JL_DLLEXPORT int jl_coverage_enabled_for(jl_module_t *m, const char *filename) JL_NOTSAFEPOINT
{
    if (codegen_imaging_mode() || jl_generating_output() || is_skip_filename(filename))
        return 0;
    switch (jl_options.code_coverage) {
    case JL_LOG_ALL:
        return 1;
    case JL_LOG_USER:
        return m != NULL && jl_base_module != NULL && jl_core_module != NULL &&
               !jl_is_submodule(m, jl_base_module) && !jl_is_submodule(m, jl_core_module);
    case JL_LOG_PATH:
        return jl_path_is_tracked(filename);
    default:
        return 0;
    }
}

JL_DLLEXPORT void jl_coverage_alloc_line(const char *filename, int line)
{
    assert(!codegen_imaging_mode());
    if (is_skip_filename(filename) || line < 0)
        return;
    uv_mutex_lock(&coverage_lock);
    allocLine(logdata_get_or_create(&coverageData, filename), line);
    uv_mutex_unlock(&coverage_lock);
}

JL_DLLEXPORT logdata_counter_t *jl_coverage_data_pointer(const char *filename, int line)
{
    uv_mutex_lock(&coverage_lock);
    logdata_counter_t *ret = allocLine(logdata_get_or_create(&coverageData, filename), line);
    uv_mutex_unlock(&coverage_lock);
    return ret;
}

JL_DLLEXPORT void jl_coverage_register_counter(logdata_counter_t *slot, logdata_counter_t *counter)
{
    uv_mutex_lock(&coverage_lock);
    arraylist_push(&registered_counters, slot);
    arraylist_push(&registered_counters, counter);
    uv_mutex_unlock(&coverage_lock);
}

// Whether the sysimage carries coverage counters matching the current options.
static int sysimg_coverage_matched = 0;
// Whether any other image was loaded without matching coverage counters.
static int unmatched_image_loaded = 0;

// Whether image code can be trusted to already collect the requested coverage,
// making the usual invalidation of image code (Compiler.reinfer) unnecessary.
JL_DLLEXPORT int jl_image_coverage_trusted(void) JL_NOTSAFEPOINT
{
    // image code carries no allocation counters, so allocation tracking
    // always needs freshly instrumented code
    if (jl_options.malloc_log != JL_LOG_NONE)
        return 0;
    return sysimg_coverage_matched && !unmatched_image_loaded;
}

// Adopt the counters compiled into a just-loaded image. Registering a counter
// allocates the canonical per-line slot, so instrumented-but-unreached lines
// are still reported (with a zero count), matching JIT instrumentation.
void jl_register_image_coverage(const void *table, int is_sysimg)
{
    const jl_image_coverage_t *cov = (const jl_image_coverage_t*)table;
    int matched = cov != NULL &&
                  jl_options.code_coverage == JL_LOG_ALL &&
                  cov->scope == JL_LOG_ALL &&
                  cov->mode == (uint32_t)jl_options.code_coverage_mode;
    if (is_sysimg)
        sysimg_coverage_matched = matched;
    else if (!matched && jl_options.code_coverage != JL_LOG_NONE)
        unmatched_image_loaded = 1;
    if (!matched)
        return;
    for (uint64_t i = 0; i < cov->nentries; i++) {
        const jl_image_coverage_entry_t *e = &cov->entries[i];
        jl_coverage_register_counter(jl_coverage_data_pointer(e->file, e->line), e->counter);
    }
}

JL_DLLEXPORT void jl_coverage_visit_line(const char *filename, size_t len, int line) JL_CANSAFEPOINT
{
    // TODO: remove `len` and use C-style strings exclusively
    //       (kept for backwards-compatibility with JuliaInterpreter)
    assert(filename[len] == '\0');
    if (codegen_imaging_mode() || is_skip_filename(filename) || line < 0)
        return;
    uv_mutex_lock(&coverage_lock);
    logdata_vec_t *vec = logdata_get_or_create(&coverageData, filename);
    logdata_counter_t *ptr = allocLine(vec, line);
    if (jl_options.code_coverage_mode == JL_COVERAGE_MODE_HIT) {
        // Match codegen's hit encoding: 2 means reached and is reported as 1.
        jl_atomic_store_relaxed(ptr, 2);
    }
    else {
        uint64_t value = jl_atomic_load_relaxed(ptr);
        jl_atomic_store_relaxed(ptr, value + 1);
    }
    uv_mutex_unlock(&coverage_lock);
}

// Memory allocation log (malloc_log)

static logdata_t mallocData;

JL_DLLEXPORT logdata_counter_t *jl_malloc_data_pointer(const char *filename, int line) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&coverage_lock);
    logdata_counter_t *ret = allocLine(logdata_get_or_create(&mallocData, filename), line);
    uv_mutex_unlock(&coverage_lock);
    return ret;
}

static void clear_log_data(logdata_t *logData) JL_NOTSAFEPOINT
{
    size_t sz = logData->size;
    void **tab = logData->table;
    for (size_t i = 0; i < sz; i += 2) {
        if (tab[i] == HT_NOTFOUND || tab[i+1] == HT_NOTFOUND)
            continue;
        logdata_vec_t *vec = (logdata_vec_t *)tab[i+1];
        for (size_t j = 0; j < vec->len; j++) {
            if (vec->blocks[j]) {
                logdata_block *data = vec->blocks[j];
                for (int k = 0; k < logdata_blocksize; k++) {
                    if (jl_atomic_load_relaxed(&(*data)[k]) > 0)
                        jl_atomic_store_relaxed(&(*data)[k], 1);
                }
            }
        }
    }
    jl_gc_sync_total_bytes(0);
}

// Resets the malloc counts.
JL_DLLEXPORT void jl_clear_malloc_data(void) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&coverage_lock);
    clear_log_data(&mallocData);
    uv_mutex_unlock(&coverage_lock);
}

// Resets the code coverage
JL_DLLEXPORT void jl_clear_coverage_data(void) JL_NOTSAFEPOINT
{
    uv_mutex_lock(&coverage_lock);
    for (size_t i = 0; i < registered_counters.len; i += 2) {
        logdata_counter_t *counter = (logdata_counter_t*)registered_counters.items[i + 1];
        jl_atomic_store_relaxed(counter, 0);
    }
    clear_log_data(&coverageData);
    uv_mutex_unlock(&coverage_lock);
}

static void write_log_data(logdata_t *logData, const char *extension) JL_NOTSAFEPOINT
{
    char base[4096];
    snprintf(base, sizeof(base), "%s/../share/julia/base/", jl_options.julia_bindir);
    size_t sz = logData->size;
    void **tab = logData->table;
    for (size_t i = 0; i < sz; i += 2) {
        if (tab[i] == HT_NOTFOUND || tab[i+1] == HT_NOTFOUND)
            continue;
        const char *filename = (const char *)tab[i];
        logdata_vec_t *values = (logdata_vec_t *)tab[i+1];
        if (values->len == 0) continue;

        char fullpath[4096];
        if (!jl_isabspath(filename))
            snprintf(fullpath, sizeof(fullpath), "%s%s", base, filename);
        else
            snprintf(fullpath, sizeof(fullpath), "%s", filename);

        FILE *inf = fopen(fullpath, "r");
        if (!inf)
            continue;

        char outpath[4096];
        snprintf(outpath, sizeof(outpath), "%s%s", fullpath, extension);
        FILE *outf = fopen(outpath, "wb");
        if (outf) {
            int l = 1;
            unsigned block = 0;
            int c = getc(inf);
            while (c != EOF) {
                logdata_block *data = NULL;
                if (block < values->len) {
                    data = values->blocks[block];
                }
                uint64_t value = data ? jl_atomic_load_relaxed(&(*data)[l]) : 0;
                if (++l >= logdata_blocksize) {
                    l = 0;
                    block++;
                }
                if (value == 0)
                    fprintf(outf, "        -");
                else
                    fprintf(outf, "%9" PRIu64, value - 1);
                putc(' ', outf);
                while (c != EOF && c != '\n') {
                    putc(c, outf);
                    c = getc(inf);
                }
                putc('\n', outf);
                if (c == '\n')
                    c = getc(inf);
            }
            fclose(outf);
        }
        fclose(inf);
    }
}

static void write_lcov_data(logdata_t *logData, const char *outfile) JL_NOTSAFEPOINT
{
    FILE *outf = fopen(outfile, "ab");
    if (!outf) return;
    size_t sz = logData->size;
    void **tab = logData->table;
    for (size_t i = 0; i < sz; i += 2) {
        if (tab[i] == HT_NOTFOUND || tab[i+1] == HT_NOTFOUND)
            continue;
        const char *filename = (const char *)tab[i];
        logdata_vec_t *values = (logdata_vec_t *)tab[i+1];
        if (values->len == 0) continue;

        fprintf(outf, "SF:%s\n", filename);
        size_t n_covered = 0;
        size_t n_instrumented = 0;
        size_t lno = 0;
        for (size_t j = 0; j < values->len; j++) {
            if (values->blocks[j]) {
                logdata_block *data = values->blocks[j];
                for (int k = 0; k < logdata_blocksize; k++) {
                    uint64_t cov = jl_atomic_load_relaxed(&(*data)[k]);
                    if (cov > 0) {
                        n_instrumented++;
                        if (cov > 1)
                            n_covered++;
                        fprintf(outf, "DA:%zu,%" PRIu64 "\n", lno, cov - 1);
                    }
                    lno++;
                }
            }
            else {
                lno += logdata_blocksize;
            }
        }
        fprintf(outf, "LH:%zu\n", n_covered);
        fprintf(outf, "LF:%zu\n", n_instrumented);
        fprintf(outf, "end_of_record\n");
    }
    fclose(outf);
}

JL_DLLEXPORT void jl_write_coverage_data(const char *output)
{
    uv_mutex_lock(&coverage_lock);
    fold_registered_counters();
    if (output) {
        size_t len = strlen(output);
        if (len >= 5 && strcmp(output + len - 5, ".info") == 0) {
            char *formatted = jl_format_filename(output);
            write_lcov_data(&coverageData, formatted);
            free(formatted);
        }
    }
    else {
        char stm[32];
        snprintf(stm, sizeof(stm), ".%d.cov", uv_os_getpid());
        write_log_data(&coverageData, stm);
    }
    uv_mutex_unlock(&coverage_lock);
}

void jl_write_malloc_log(void)
{
    uv_mutex_lock(&coverage_lock);
    char stm[32];
    snprintf(stm, sizeof(stm), ".%d.mem", uv_os_getpid());
    write_log_data(&mallocData, stm);
    uv_mutex_unlock(&coverage_lock);
}

void jl_init_coverage(void)
{
    uv_mutex_init(&coverage_lock);
    strhash_new(&coverageData, 0);
    strhash_new(&mallocData, 0);
    arraylist_new(&registered_counters, 0);
}
