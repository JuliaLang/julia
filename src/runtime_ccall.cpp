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
