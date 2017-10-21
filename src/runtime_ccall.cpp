// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include <map>
#include <string>
#include <cstdio>
#include <llvm/Support/Host.h>
#include "julia.h"
#include "julia_internal.h"
#include "processor.h"
#include "julia_assert.h"

using namespace llvm;

// --- library symbol lookup ---

// map from "libX" to full soname "libX.so.ver"
#if defined(__linux__) || defined(__FreeBSD__)
static uv_rwlock_t soname_lock;
static std::map<std::string, std::string> sonameMap;
static bool got_sonames = false;

extern "C" void jl_init_runtime_ccall(void)
{
    uv_rwlock_init(&soname_lock);
}

// This reloads the sonames, necessary after system upgrade.
// Keep this DLLEXPORTed, this is used by `BinDeps.jl` to make sure
// newly installed libraries can be found.
extern "C" JL_DLLEXPORT void jl_read_sonames(void)
{
    char *line=NULL;
    size_t sz=0;
#if defined(__linux__)
    FILE *ldc = popen("/sbin/ldconfig -p", "r");
#else
    FILE *ldc = popen("/sbin/ldconfig -r", "r");
#endif
    if (ldc == NULL) return; // ignore errors in running ldconfig (other than whatever might have been printed to stderr)

    // This loop is not allowed to call julia GC while holding the lock
    uv_rwlock_wrlock(&soname_lock);
    sonameMap.clear();
    while (!feof(ldc)) {
        ssize_t n = getline(&line, &sz, ldc);
        if (n == -1)
            break;
        if (n > 2 && isspace((unsigned char)line[0])) {
#ifdef __linux__
            int i = 0;
            while (isspace((unsigned char)line[++i])) ;
            char *name = &line[i];
            char *dot = strstr(name, ".so");
            i = 0;
#else
            char *name = strstr(line, ":-l");
            if (name == NULL) continue;
            strncpy(name, "lib", 3);
            char *dot = strchr(name, '.');
#endif

            if (NULL == dot)
                continue;

#ifdef __linux__
            // Detect if this entry is for the current architecture
            while (!isspace((unsigned char)dot[++i])) ;
            while (isspace((unsigned char)dot[++i])) ;
            int j = i;
            while (!isspace((unsigned char)dot[++j])) ;
            char *arch = strstr(dot+i,"x86-64");
            if (arch != NULL && arch < dot + j) {
#ifdef _P32
                continue;
#endif
            }
            else {
#ifdef _P64
                continue;
#endif
            }
#endif // __linux__

            char *abslibpath = strrchr(line, ' ');
            if (dot != NULL && abslibpath != NULL) {
                std::string pfx(name, dot - name);
                // Do not include ' ' in front and '\n' at the end
                std::string soname(abslibpath+1, line+n-(abslibpath+1)-1);
                sonameMap[pfx] = soname;
            }
        }
    }

    free(line);
    pclose(ldc);
    uv_rwlock_wrunlock(&soname_lock);
}

// This API is not thread safe. The return value can be free'd if
// `jl_read_sonames()` is called on another thread.
extern "C" JL_DLLEXPORT const char *jl_lookup_soname(const char *pfx, size_t n)
{
    if (!got_sonames) {
        jl_read_sonames();
        got_sonames = true;
    }
    const char *res = nullptr;
    uv_rwlock_rdlock(&soname_lock);
    auto search = sonameMap.find(std::string(pfx, n));
    if (search != sonameMap.end())
        res = search->second.c_str();
    uv_rwlock_rdunlock(&soname_lock);
    return res;
}

extern "C" void *jl_dlopen_soname(const char *pfx, size_t n, unsigned flags)
{
    if (!got_sonames) {
        jl_read_sonames();
        got_sonames = true;
    }
    void *res = nullptr;
    uv_rwlock_rdlock(&soname_lock);
    auto search = sonameMap.find(std::string(pfx, n));
    if (search != sonameMap.end())
        res = jl_dlopen(search->second.c_str(), flags);
    uv_rwlock_rdunlock(&soname_lock);
    return res;
}
#else
extern "C" void jl_init_runtime_ccall(void)
{
}
#endif

// map from user-specified lib names to handles
static std::map<std::string, void*> libMap;
static jl_mutex_t libmap_lock;
extern "C"
void *jl_get_library(const char *f_lib)
{
    void *hnd;
#ifdef _OS_WINDOWS_
    if (f_lib == JL_EXE_LIBNAME)
        return jl_exe_handle;
    if (f_lib == JL_DL_LIBNAME)
        return jl_dl_handle;
#endif
    if (f_lib == NULL)
        return jl_RTLD_DEFAULT_handle;
    JL_LOCK_NOGC(&libmap_lock);
    // This is the only operation we do on the map, which doesn't invalidate
    // any references or iterators.
    void **map_slot = &libMap[f_lib];
    JL_UNLOCK_NOGC(&libmap_lock);
    hnd = jl_atomic_load_acquire(map_slot);
    if (hnd != NULL)
        return hnd;
    // We might run this concurrently on two threads but it doesn't matter.
    hnd = jl_load_dynamic_library(f_lib, JL_RTLD_DEFAULT);
    if (hnd != NULL)
        jl_atomic_store_release(map_slot, hnd);
    return hnd;
}

extern "C" JL_DLLEXPORT
void *jl_load_and_lookup(const char *f_lib, const char *f_name, void **hnd)
{
    void *handle = jl_atomic_load_acquire(hnd);
    if (!handle)
        jl_atomic_store_release(hnd, (handle = jl_get_library(f_lib)));
    return jl_dlsym(handle, f_name);
}

// miscellany
std::string jl_get_cpu_name_llvm(void)
{
    return llvm::sys::getHostCPUName().str();
}

std::string jl_get_cpu_features_llvm(void)
{
    StringMap<bool> HostFeatures;
    llvm::sys::getHostCPUFeatures(HostFeatures);
    std::string attr;
    for (auto &ele: HostFeatures) {
        if (ele.getValue()) {
            if (!attr.empty()) {
                attr.append(",+");
            }
            else {
                attr.append("+");
            }
            attr.append(ele.getKey().str());
        }
    }
    // Explicitly disabled features need to be added at the end so that
    // they are not reenabled by other features that implies them by default.
    for (auto &ele: HostFeatures) {
        if (!ele.getValue()) {
            if (!attr.empty()) {
                attr.append(",-");
            }
            else {
                attr.append("-");
            }
            attr.append(ele.getKey().str());
        }
    }
    return attr;
}

extern "C" JL_DLLEXPORT
jl_value_t *jl_get_JIT(void)
{
    const std::string& HostJITName = "ORCJIT";
    return jl_pchar_to_string(HostJITName.data(), HostJITName.size());
}
