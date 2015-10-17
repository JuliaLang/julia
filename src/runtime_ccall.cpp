// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <map>
#include <string>
#include <cstdio>
#include "julia.h"
#include "julia_internal.h"

// --- library symbol lookup ---

// map from "libX" to full soname "libX.so.ver"
#if defined(__linux__) || defined(__FreeBSD__)
static std::map<std::string, std::string> sonameMap;
static bool got_sonames = false;

static void jl_read_sonames(void)
{
    char *line=NULL;
    size_t sz=0;
#if defined(__linux__)
    FILE *ldc = popen("/sbin/ldconfig -p", "r");
#else
    FILE *ldc = popen("/sbin/ldconfig -r", "r");
#endif

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
}

extern "C" DLLEXPORT const char *jl_lookup_soname(const char *pfx, size_t n)
{
    if (!got_sonames) {
        jl_read_sonames();
        got_sonames = true;
    }
    std::string str(pfx, n);
    if (sonameMap.find(str) != sonameMap.end()) {
        return sonameMap[str].c_str();
    }
    return NULL;
}
#endif

// map from user-specified lib names to handles
static std::map<std::string, uv_lib_t*> libMap;

extern "C"
uv_lib_t *jl_get_library(char *f_lib)
{
    uv_lib_t *hnd;
#ifdef _OS_WINDOWS_
    if ((intptr_t)f_lib == 1)
        return jl_exe_handle;
    if ((intptr_t)f_lib == 2)
        return jl_dl_handle;
#endif
    if (f_lib == NULL)
        return jl_RTLD_DEFAULT_handle;
    hnd = libMap[f_lib];
    if (hnd != NULL)
        return hnd;
    hnd = (uv_lib_t *) jl_load_dynamic_library(f_lib, JL_RTLD_DEFAULT);
    if (hnd != NULL)
        libMap[f_lib] = hnd;
    return hnd;
}

extern "C" DLLEXPORT
void *jl_load_and_lookup(char *f_lib, char *f_name, uv_lib_t **hnd)
{
    uv_lib_t *handle = *hnd;
    if (!handle)
        *hnd = handle = jl_get_library(f_lib);
    void *ptr = jl_dlsym_e(handle, f_name);
    if (!ptr)
        jl_errorf("symbol \"%s\" could not be found: %s", f_name, uv_dlerror(handle));
    return ptr;
}

// miscellany
#include <llvm/Support/Host.h>
extern "C" DLLEXPORT
jl_value_t *jl_get_cpu_name(void)
{
#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR < 5
    std::string HostCPUName = llvm::sys::getHostCPUName();
#else
    StringRef HostCPUName = llvm::sys::getHostCPUName();
#endif
    return jl_pchar_to_string(HostCPUName.data(), HostCPUName.size());
}
