// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <cstdint>
#include <pthread.h>
#include <string>
#include <fstream>
#include <map>
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

const int logdata_blocksize = 32; // target getting nearby lines in the same general cache area and reducing calls to malloc by chunking
typedef uint64_t logdata_block[logdata_blocksize];
typedef StringMap< SmallVector<logdata_block*, 0> > logdata_t;

pthread_mutex_t coverage_lock = PTHREAD_MUTEX_INITIALIZER;

static uint64_t *allocLine(SmallVector<logdata_block*, 0> &vec, int line) JL_NOTSAFEPOINT
{
    unsigned block = line / logdata_blocksize;
    line = line % logdata_blocksize;
    if (vec.size() <= block)
        vec.resize(block + 1);
    if (vec[block] == NULL) {
        vec[block] = (logdata_block*)calloc(1, sizeof(logdata_block));
    }
    logdata_block &data = *vec[block];
    if (data[line] == 0)
        data[line] = 1;
    return &data[line];
}

// Code coverage

static logdata_t coverageData;

JL_DLLEXPORT void jl_coverage_alloc_line(StringRef filename, int line) JL_NOTSAFEPOINT
{
    assert(!codegen_imaging_mode());
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    pthread_mutex_lock(&coverage_lock);
    allocLine(coverageData[filename], line);
    pthread_mutex_unlock(&coverage_lock);
}

JL_DLLEXPORT uint64_t *jl_coverage_data_pointer(StringRef filename, int line) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&coverage_lock);
    uint64_t* ret = allocLine(coverageData[filename], line);
    pthread_mutex_unlock(&coverage_lock);
    return ret;
}

extern "C" JL_DLLEXPORT void jl_coverage_visit_line(const char *filename_, size_t len_filename, int line) JL_NOTSAFEPOINT
{
    StringRef filename = StringRef(filename_, len_filename);
    if (codegen_imaging_mode() || filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    pthread_mutex_lock(&coverage_lock);
    SmallVector<logdata_block*, 0> &vec = coverageData[filename];
    uint64_t *ptr = allocLine(vec, line);
    (*ptr)++;
    pthread_mutex_unlock(&coverage_lock);
}

// Memory allocation log (malloc_log)

static logdata_t mallocData;

JL_DLLEXPORT uint64_t *jl_malloc_data_pointer(StringRef filename, int line) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&coverage_lock);
    uint64_t* ret = allocLine(mallocData[filename], line);
    pthread_mutex_unlock(&coverage_lock);
    return ret;
}

static void clear_log_data(logdata_t &logData, int resetValue) JL_NOTSAFEPOINT
{
    logdata_t::iterator it = logData.begin();
    for (; it != logData.end(); it++) {
        SmallVector<logdata_block*, 0> &bytes = (*it).second;
        SmallVector<logdata_block*, 0>::iterator itb;
        for (itb = bytes.begin(); itb != bytes.end(); itb++) {
            if (*itb) {
                logdata_block &data = **itb;
                for (int i = 0; i < logdata_blocksize; i++) {
                    if (data[i] > 0)
                        data[i] = resetValue;
                }
            }
        }
    }
    jl_gc_sync_total_bytes(0);
}

// Resets the malloc counts.
extern "C" JL_DLLEXPORT void jl_clear_malloc_data(void) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&coverage_lock);
    clear_log_data(mallocData, 1);
    pthread_mutex_unlock(&coverage_lock);
}

// Resets the code coverage
extern "C" JL_DLLEXPORT void jl_clear_coverage_data(void) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&coverage_lock);
    clear_log_data(coverageData, 0);
    pthread_mutex_unlock(&coverage_lock);
}

static void write_log_data(logdata_t &logData, const char *extension) JL_NOTSAFEPOINT
{
    std::string base = std::string(jl_options.julia_bindir);
    base = base + "/../share/julia/base/";
    logdata_t::iterator it = logData.begin();
    for (; it != logData.end(); it++) {
        std::string filename(it->first());
        SmallVector<logdata_block*, 0> &values = it->second;
        if (!values.empty()) {
            if (!jl_isabspath(filename.c_str()))
                filename = base + filename;
            std::ifstream inf(filename.c_str());
            if (!inf.is_open())
                continue;
            std::string outfile = filename + extension;
            std::ofstream outf(outfile.c_str(), std::ofstream::trunc | std::ofstream::out | std::ofstream::binary);
            if (outf.is_open()) {
                inf.exceptions(std::ifstream::badbit);
                outf.exceptions(std::ifstream::failbit | std::ifstream::badbit);
                char line[1024];
                int l = 1;
                unsigned block = 0;
                while (!inf.eof()) {
                    inf.getline(line, sizeof(line));
                    if (inf.fail()) {
                        if (inf.eof())
                            break; // no content on trailing line
                        // Read through lines longer than sizeof(line)
                        inf.clear();
                        inf.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
                    }
                    logdata_block *data = NULL;
                    if (block < values.size()) {
                        data = values[block];
                    }
                    uint64_t value = data ? (*data)[l] : 0;
                    if (++l >= logdata_blocksize) {
                        l = 0;
                        block++;
                    }
                    outf.width(9);
                    if (value == 0)
                        outf << '-';
                    else
                        outf << (value - 1);
                    outf.width(0);
                    outf << " " << line << '\n';
                }
                outf.close();
            }
            inf.close();
        }
    }
}

static void write_lcov_data(logdata_t &logData, const std::string &outfile) JL_NOTSAFEPOINT
{
    std::ofstream outf(outfile.c_str(), std::ofstream::ate | std::ofstream::out | std::ofstream::binary);
    //std::string base = std::string(jl_options.julia_bindir);
    //base = base + "/../share/julia/base/";
    logdata_t::iterator it = logData.begin();
    for (; it != logData.end(); it++) {
        StringRef filename = it->first();
        const SmallVector<logdata_block*, 0> &values = it->second;
        if (!values.empty()) {
            outf << "SF:" << filename.str() << '\n';
            size_t n_covered = 0;
            size_t n_instrumented = 0;
            size_t lno = 0;
            for (auto &itv : values) {
                if (itv) {
                    logdata_block &data = *itv;
                    for (int i = 0; i < logdata_blocksize; i++) {
                        auto cov = data[i];
                        if (cov > 0) {
                            n_instrumented++;
                            if (cov > 1)
                                n_covered++;
                            outf << "DA:" << lno << ',' << (cov - 1) << '\n';
                        }
                        lno++;
                    }
                }
                else {
                    lno += logdata_blocksize;
                }
            }
            outf << "LH:" << n_covered << '\n';
            outf << "LF:" << n_instrumented << '\n';
            outf << "end_of_record\n";
        }
    }
    outf.close();
}

extern "C" JL_DLLEXPORT void jl_write_coverage_data(const char *output) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&coverage_lock);
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
    pthread_mutex_unlock(&coverage_lock);
}

extern "C" void jl_write_malloc_log(void) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&coverage_lock);
    std::string stm;
    raw_string_ostream(stm) << "." << uv_os_getpid() << ".mem";
    write_log_data(mallocData, stm.c_str());
    pthread_mutex_unlock(&coverage_lock);
}
