// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <atomic>
#include <cstdint>
#include <memory>
#include <pthread.h>
#include <string>
#include <vector>

#include "llvm-version.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/Support/raw_ostream.h>

#include "julia.h"
#include "julia_internal.h"

using namespace llvm;

static int codegen_imaging_mode(void) JL_NOTSAFEPOINT
{
    return jl_options.image_codegen || (jl_generating_output() && jl_options.use_pkgimages);
}

// Logging for code coverage and memory allocation
//
// Optimized for minimal contention with multiple threads:
// 1. Per-file fine-grained locking - each file has its own mutex
// 2. Lock-free counter updates using atomic operations
// 3. Global map uses rwlock - parallel reads, exclusive writes only for new files

const int logdata_blocksize = 32; // target getting nearby lines in the same general cache area and reducing calls to malloc by chunking
typedef uint64_t logdata_block[logdata_blocksize];

// Per-file coverage data with fine-grained locking
struct FileLogData {
    pthread_mutex_t file_lock;
    std::vector<logdata_block*> blocks;

    FileLogData() JL_NOTSAFEPOINT {
        pthread_mutex_init(&file_lock, nullptr);
    }

    ~FileLogData() JL_NOTSAFEPOINT {
        pthread_mutex_destroy(&file_lock);
        for (auto *block : blocks) {
            if (block)
                free(block);
        }
    }

    FileLogData(const FileLogData&) = delete;
    FileLogData& operator=(const FileLogData&) = delete;
};

typedef StringMap<std::unique_ptr<FileLogData>> logdata_t;

// Global rwlock for map access - reads (lookups) can proceed in parallel
static pthread_rwlock_t coverage_map_lock = PTHREAD_RWLOCK_INITIALIZER;

// Get or create file entry - uses read lock for lookup, write lock only for insertion
static FileLogData *getOrCreateFileData(logdata_t &logData, StringRef filename) JL_NOTSAFEPOINT
{
    // Fast path: read lock for lookup (parallel)
    pthread_rwlock_rdlock(&coverage_map_lock);
    auto it = logData.find(filename);
    if (it != logData.end()) {
        FileLogData *data = it->second.get();
        pthread_rwlock_unlock(&coverage_map_lock);
        return data;
    }
    pthread_rwlock_unlock(&coverage_map_lock);

    // Slow path: write lock for insertion (exclusive)
    pthread_rwlock_wrlock(&coverage_map_lock);
    // Re-check after acquiring write lock
    it = logData.find(filename);
    if (it == logData.end()) {
        auto result = logData.try_emplace(filename, std::make_unique<FileLogData>());
        it = result.first;
    }
    FileLogData *data = it->second.get();
    pthread_rwlock_unlock(&coverage_map_lock);
    return data;
}

// Allocate and return pointer to line counter
// Per-file lock is held during resize/allocation, but counter updates are lock-free
static uint64_t *allocLine(FileLogData *fileData, int line) JL_NOTSAFEPOINT
{
    unsigned blockIdx = line / logdata_blocksize;
    int lineIdx = line % logdata_blocksize;

    pthread_mutex_lock(&fileData->file_lock);

    // Resize if needed
    if (blockIdx >= fileData->blocks.size()) {
        fileData->blocks.resize(blockIdx + 1, nullptr);
    }

    // Allocate block if needed
    if (fileData->blocks[blockIdx] == nullptr) {
        fileData->blocks[blockIdx] = (logdata_block*)calloc(1, sizeof(logdata_block));
    }

    logdata_block *block = fileData->blocks[blockIdx];
    pthread_mutex_unlock(&fileData->file_lock);

    uint64_t *counter = &(*block)[lineIdx];

    // Mark line as instrumented (value 0 -> 1) using atomic CAS
    uint64_t expected = 0;
    __atomic_compare_exchange_n(counter, &expected, 1, false,
        __ATOMIC_RELAXED, __ATOMIC_RELAXED);

    return counter;
}

// Code coverage

static logdata_t coverageData;

JL_DLLEXPORT void jl_coverage_alloc_line(StringRef filename, int line) JL_NOTSAFEPOINT
{
    assert(!codegen_imaging_mode());
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    FileLogData *fileData = getOrCreateFileData(coverageData, filename);
    allocLine(fileData, line);
}

JL_DLLEXPORT uint64_t *jl_coverage_data_pointer(StringRef filename, int line) JL_NOTSAFEPOINT
{
    FileLogData *fileData = getOrCreateFileData(coverageData, filename);
    return allocLine(fileData, line);
}

extern "C" JL_DLLEXPORT void jl_coverage_visit_line(const char *filename_, size_t len_filename, int line) JL_NOTSAFEPOINT
{
    StringRef filename = StringRef(filename_, len_filename);
    if (codegen_imaging_mode() || filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    FileLogData *fileData = getOrCreateFileData(coverageData, filename);
    uint64_t *ptr = allocLine(fileData, line);
    // On x86_64, use plain increment to avoid expensive lock xadd instruction.
    // On ARM64, use atomic since ldadd is cheap.
    // Races may cause undercounting, which is acceptable for coverage.
#if defined(__x86_64__) || defined(_M_X64)
    (*ptr)++;
#else
    __atomic_fetch_add(ptr, 1, __ATOMIC_RELAXED);
#endif
}

// Memory allocation log (malloc_log)

static logdata_t mallocData;

JL_DLLEXPORT uint64_t *jl_malloc_data_pointer(StringRef filename, int line) JL_NOTSAFEPOINT
{
    FileLogData *fileData = getOrCreateFileData(mallocData, filename);
    return allocLine(fileData, line);
}

static void clear_log_data(logdata_t &logData, int resetValue) JL_NOTSAFEPOINT
{
    for (auto &entry : logData) {
        FileLogData *fileData = entry.second.get();
        pthread_mutex_lock(&fileData->file_lock);
        for (auto *block : fileData->blocks) {
            if (block) {
                for (int i = 0; i < logdata_blocksize; i++) {
                    if ((*block)[i] > 0)
                        (*block)[i] = resetValue;
                }
            }
        }
        pthread_mutex_unlock(&fileData->file_lock);
    }
    jl_gc_sync_total_bytes(0);
}

// Resets the malloc counts.
extern "C" JL_DLLEXPORT void jl_clear_malloc_data(void) JL_NOTSAFEPOINT
{
    pthread_rwlock_rdlock(&coverage_map_lock);
    clear_log_data(mallocData, 1);
    pthread_rwlock_unlock(&coverage_map_lock);
}

// Resets the code coverage
extern "C" JL_DLLEXPORT void jl_clear_coverage_data(void) JL_NOTSAFEPOINT
{
    pthread_rwlock_rdlock(&coverage_map_lock);
    clear_log_data(coverageData, 0);
    pthread_rwlock_unlock(&coverage_map_lock);
}

static void write_log_data(logdata_t &logData, const char *extension) JL_NOTSAFEPOINT
{
    std::string base = std::string(jl_options.julia_bindir);
    base = base + "/../share/julia/base/";
    for (auto &entry : logData) {
        std::string filename(entry.first());
        FileLogData *fileData = entry.second.get();
        if (!fileData->blocks.empty()) {
            if (!jl_isabspath(filename.c_str()))
                filename = base + filename;
            FILE *inf = fopen(filename.c_str(), "r");
            if (!inf)
                continue;
            std::string outfile = filename + extension;
            FILE *outf = fopen(outfile.c_str(), "wb");
            if (outf) {
                char line[1024];
                int l = 1;
                unsigned block = 0;
                int ret = 0;
                while (ret != EOF && (ret = fscanf(inf, "%1023[^\n]", line)) != EOF) {
                    // Skip n non-newline chars and a single trailing newline
                    if ((ret = fscanf(inf, "%*[^\n]")) != EOF)
                        ret = fscanf(inf, "%*1[\n]");
                    logdata_block *data = nullptr;
                    if (block < fileData->blocks.size()) {
                        data = fileData->blocks[block];
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
}

static void write_lcov_data(logdata_t &logData, const std::string &outfile) JL_NOTSAFEPOINT
{
    FILE *outf = fopen(outfile.c_str(), "ab");
    if (!outf) return;
    for (auto &entry : logData) {
        StringRef filename = entry.first();
        FileLogData *fileData = entry.second.get();
        if (!fileData->blocks.empty()) {
            fprintf(outf, "SF:%.*s\n", (int)filename.size(), filename.data());
            size_t n_covered = 0;
            size_t n_instrumented = 0;
            size_t lno = 0;
            for (auto *block : fileData->blocks) {
                if (block) {
                    for (int i = 0; i < logdata_blocksize; i++) {
                        auto cov = (*block)[i];
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
    }
    fclose(outf);
}

extern "C" JL_DLLEXPORT void jl_write_coverage_data(const char *output) JL_NOTSAFEPOINT
{
    pthread_rwlock_rdlock(&coverage_map_lock);
    if (output) {
        StringRef output_pattern(output);
        if (output_pattern.ends_with(".info"))
            write_lcov_data(coverageData, jl_format_filename(output_pattern.str().c_str()));
    }
    else {
        std::string stm;
        raw_string_ostream(stm) << "." << uv_os_getpid() << ".cov";
        write_log_data(coverageData, stm.c_str());
    }
    pthread_rwlock_unlock(&coverage_map_lock);
}

extern "C" void jl_write_malloc_log(void) JL_NOTSAFEPOINT
{
    pthread_rwlock_rdlock(&coverage_map_lock);
    std::string stm;
    raw_string_ostream(stm) << "." << uv_os_getpid() << ".mem";
    write_log_data(mallocData, stm.c_str());
    pthread_rwlock_unlock(&coverage_map_lock);
}
