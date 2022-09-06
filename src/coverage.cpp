// This file is a part of Julia. License is MIT: https://julialang.org/license

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

static int codegen_imaging_mode(void)
{
    return jl_options.image_codegen || (jl_generating_output() && !jl_options.incremental);
}

// Logging for code coverage and memory allocation

const int logdata_blocksize = 32; // target getting nearby lines in the same general cache area and reducing calls to malloc by chunking
typedef uint64_t logdata_block[logdata_blocksize];
typedef StringMap< std::vector<logdata_block*> > logdata_t;

static uint64_t *allocLine(std::vector<logdata_block*> &vec, int line)
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

JL_DLLEXPORT void jl_coverage_alloc_line(StringRef filename, int line)
{
    assert(!codegen_imaging_mode());
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    allocLine(coverageData[filename], line);
}

JL_DLLEXPORT uint64_t *jl_coverage_data_pointer(StringRef filename, int line)
{
    return allocLine(coverageData[filename], line);
}

extern "C" JL_DLLEXPORT void jl_coverage_visit_line(const char *filename_, size_t len_filename, int line)
{
    StringRef filename = StringRef(filename_, len_filename);
    if (codegen_imaging_mode() || filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    std::vector<logdata_block*> &vec = coverageData[filename];
    uint64_t *ptr = allocLine(vec, line);
    (*ptr)++;
}

// Memory allocation log (malloc_log)

static logdata_t mallocData;

JL_DLLEXPORT uint64_t *jl_malloc_data_pointer(StringRef filename, int line)
{
    return allocLine(mallocData[filename], line);
}

// Resets the malloc counts.
extern "C" JL_DLLEXPORT void jl_clear_malloc_data(void)
{
    logdata_t::iterator it = mallocData.begin();
    for (; it != mallocData.end(); it++) {
        std::vector<logdata_block*> &bytes = (*it).second;
        std::vector<logdata_block*>::iterator itb;
        for (itb = bytes.begin(); itb != bytes.end(); itb++) {
            if (*itb) {
                logdata_block &data = **itb;
                for (int i = 0; i < logdata_blocksize; i++) {
                    if (data[i] > 0)
                        data[i] = 1;
                }
            }
        }
    }
    jl_gc_sync_total_bytes(0);
}

static void write_log_data(logdata_t &logData, const char *extension)
{
    std::string base = std::string(jl_options.julia_bindir);
    base = base + "/../share/julia/base/";
    logdata_t::iterator it = logData.begin();
    for (; it != logData.end(); it++) {
        std::string filename(it->first());
        std::vector<logdata_block*> &values = it->second;
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

static void write_lcov_data(logdata_t &logData, const std::string &outfile)
{
    std::ofstream outf(outfile.c_str(), std::ofstream::ate | std::ofstream::out | std::ofstream::binary);
    //std::string base = std::string(jl_options.julia_bindir);
    //base = base + "/../share/julia/base/";
    logdata_t::iterator it = logData.begin();
    for (; it != logData.end(); it++) {
        StringRef filename = it->first();
        const std::vector<logdata_block*> &values = it->second;
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

extern "C" JL_DLLEXPORT void jl_write_coverage_data(const char *output)
{
    if (output) {
        StringRef output_pattern(output);
        if (output_pattern.endswith(".info"))
            write_lcov_data(coverageData, jl_format_filename(output_pattern.str().c_str()));
    }
    else {
        std::string stm;
        raw_string_ostream(stm) << "." << uv_os_getpid() << ".cov";
        write_log_data(coverageData, stm.c_str());
    }
}

extern "C" JL_DLLEXPORT void jl_write_malloc_log(void)
{
    std::string stm;
    raw_string_ostream(stm) << "." << uv_os_getpid() << ".mem";
    write_log_data(mallocData, stm.c_str());
}
