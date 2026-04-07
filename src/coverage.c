// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdint.h>
#include <pthread.h>
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
typedef uint64_t logdata_block[logdata_blocksize];

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

pthread_mutex_t coverage_lock = PTHREAD_MUTEX_INITIALIZER;

static uint64_t *allocLine(logdata_vec_t *vec, int line) JL_NOTSAFEPOINT
{
    unsigned block = line / logdata_blocksize;
    line = line % logdata_blocksize;
    if (vec->len <= block)
        logdata_vec_resize(vec, block + 1);
    jl_assume(vec->blocks != NULL);
    if (vec->blocks[block] == NULL) {
        vec->blocks[block] = (logdata_block *)calloc(1, sizeof(logdata_block));
    }
    logdata_block *data = vec->blocks[block];
    if ((*data)[line] == 0)
        (*data)[line] = 1;
    return &(*data)[line];
}

// Code coverage

static logdata_t coverageData;

static int is_skip_filename(const char *filename) JL_NOTSAFEPOINT
{
    if (!filename || filename[0] == '\0') return 1;
    if (strcmp(filename, "none") == 0) return 1;
    if (strcmp(filename, "no file") == 0) return 1;
    if (strcmp(filename, "<missing>") == 0) return 1;
    return 0;
}

JL_DLLEXPORT void jl_coverage_alloc_line(const char *filename, int line) JL_NOTSAFEPOINT
{
    assert(!codegen_imaging_mode());
    if (is_skip_filename(filename) || line < 0)
        return;
    pthread_mutex_lock(&coverage_lock);
    allocLine(logdata_get_or_create(&coverageData, filename), line);
    pthread_mutex_unlock(&coverage_lock);
}

JL_DLLEXPORT uint64_t *jl_coverage_data_pointer(const char *filename, int line) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&coverage_lock);
    uint64_t *ret = allocLine(logdata_get_or_create(&coverageData, filename), line);
    pthread_mutex_unlock(&coverage_lock);
    return ret;
}

JL_DLLEXPORT void jl_coverage_visit_line(const char *filename, size_t len, int line) JL_NOTSAFEPOINT
{
    // TODO: remove `len` and use C-style strings exclusively
    //       (kept for backwards-compatibility with JuliaInterpreter)
    assert(filename[len] == '\0');
    if (codegen_imaging_mode() || is_skip_filename(filename) || line < 0)
        return;
    pthread_mutex_lock(&coverage_lock);
    logdata_vec_t *vec = logdata_get_or_create(&coverageData, filename);
    uint64_t *ptr = allocLine(vec, line);
    (*ptr)++;
    pthread_mutex_unlock(&coverage_lock);
}

// Memory allocation log (malloc_log)

static logdata_t mallocData;

JL_DLLEXPORT uint64_t *jl_malloc_data_pointer(const char *filename, int line) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&coverage_lock);
    uint64_t *ret = allocLine(logdata_get_or_create(&mallocData, filename), line);
    pthread_mutex_unlock(&coverage_lock);
    return ret;
}

static void clear_log_data(logdata_t *logData, int resetValue) JL_NOTSAFEPOINT
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
                    if ((*data)[k] > 0)
                        (*data)[k] = resetValue;
                }
            }
        }
    }
    jl_gc_sync_total_bytes(0);
}

// Resets the malloc counts.
JL_DLLEXPORT void jl_clear_malloc_data(void) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&coverage_lock);
    clear_log_data(&mallocData, 1);
    pthread_mutex_unlock(&coverage_lock);
}

// Resets the code coverage
JL_DLLEXPORT void jl_clear_coverage_data(void) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&coverage_lock);
    clear_log_data(&coverageData, 0);
    pthread_mutex_unlock(&coverage_lock);
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
            char line[1024];
            int l = 1;
            unsigned block = 0;
            int ret = 0;
            while (ret != EOF && (ret = fscanf(inf, "%1023[^\n]", line)) != EOF) {
                // Skip n non-newline chars and a single trailing newline
                if ((ret = fscanf(inf, "%*[^\n]")) != EOF)
                    ret = fscanf(inf, "%*1[\n]");
                logdata_block *data = NULL;
                if (block < values->len) {
                    data = values->blocks[block];
                }
                uint64_t value = data ? (*data)[l] : 0;
                if (++l >= logdata_blocksize) {
                    l = 0;
                    block++;
                }
                if (value == 0)
                    fprintf(outf, "        -");
                else
                    fprintf(outf, "%9" PRIu64, value - 1);
                fprintf(outf, " %s\n", line);
                line[0] = 0;
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
                    uint64_t cov = (*data)[k];
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

JL_DLLEXPORT void jl_write_coverage_data(const char *output) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&coverage_lock);
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
    pthread_mutex_unlock(&coverage_lock);
}

void jl_write_malloc_log(void) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&coverage_lock);
    char stm[32];
    snprintf(stm, sizeof(stm), ".%d.mem", uv_os_getpid());
    write_log_data(&mallocData, stm);
    pthread_mutex_unlock(&coverage_lock);
}

void jl_init_coverage(void) JL_NOTSAFEPOINT
{
    strhash_new(&coverageData, 0);
    strhash_new(&mallocData, 0);
}
