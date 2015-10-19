// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/stat.h>

#include "platform.h"
#include "julia.h"
#include "julia_internal.h"
#ifdef _OS_WINDOWS_
#include <windows.h>
#include <direct.h>
#else
#include <unistd.h>
#include <dlfcn.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__APPLE__)
static char *extensions[] = { "", ".dylib" };
#define N_EXTENSIONS 2
#elif defined(_OS_WINDOWS_)
static char *extensions[] = { "", ".dll" };
#define N_EXTENSIONS 2
extern int needsSymRefreshModuleList;
#else
static char *extensions[] = { ".so", "" };
#define N_EXTENSIONS 2
#endif


#define PATHBUF 512

extern char *julia_home;

#define JL_RTLD(flags, FLAG) (flags & JL_RTLD_ ## FLAG ? RTLD_ ## FLAG : 0)
#ifdef __has_feature
#   if __has_feature(address_sanitizer)
#      define SANITIZE_ADDRESS
#   endif
#endif

DLLEXPORT int jl_uv_dlopen(const char *filename, jl_uv_libhandle lib_, unsigned flags)
{
    uv_lib_t *lib = (uv_lib_t *) lib_;
#if defined(_OS_WINDOWS_)
    needsSymRefreshModuleList = 1;
#endif
#if defined(RTLD_GLOBAL) && defined(RTLD_LAZY) /* POSIX flags available */
    dlerror(); /* Reset error status. */
    lib->handle = dlopen(filename,
                         (flags & JL_RTLD_NOW ? RTLD_NOW : RTLD_LAZY)
                         | JL_RTLD(flags, LOCAL)
                         | JL_RTLD(flags, GLOBAL)
#ifdef RTLD_NODELETE
                         | JL_RTLD(flags, NODELETE)
#endif
#ifdef RTLD_NOLOAD
                         | JL_RTLD(flags, NOLOAD)
#endif
#if defined(RTLD_DEEPBIND) && !defined(SANITIZE_ADDRESS)
                         | JL_RTLD(flags, DEEPBIND)
#endif
#ifdef RTLD_FIRST
                         | JL_RTLD(flags, FIRST)
#endif
                         );
    if (lib->handle) {
        lib->errmsg = NULL;
        return 0;
    }
    else {
        lib->errmsg = strdup(dlerror());
        return -1;
    }
#else
    return uv_dlopen(filename, lib);
#endif
}

static uv_lib_t *jl_load_dynamic_library_(const char *modname, unsigned flags, int throw_err)
{
    int error;
    char *ext;
    char path[PATHBUF];
    int i;
    uv_stat_t stbuf;
    uv_lib_t *handle = (uv_lib_t*)malloc(sizeof(uv_lib_t));
    handle->errmsg = NULL;

/*
    this branch returns handle of libjulia
*/
    if (modname == NULL) {
#ifdef _OS_WINDOWS_
        if (!GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                                (LPCWSTR)(&jl_load_dynamic_library),
                                &handle->handle)) {
            free(handle);
            jl_error("could not load base module");
        }
#else
        handle->handle = dlopen(NULL,RTLD_NOW);
#endif
        goto done;
    }
/*
    this branch shortcuts absolute paths
*/
#ifdef _OS_WINDOWS_
    else if (modname[1] == ':') {
#else
    else if (modname[0] == '/') {
#endif
        error = jl_uv_dlopen(modname,handle,flags);
        if (!error)
            goto done;
        // bail out and show the error if file actually exists
        if (jl_stat(modname, (char*)&stbuf) == 0)
            goto notfound;
        if (handle->errmsg)
            uv_dlclose(handle);
    }
/*
    this branch permutes all base paths in DL_LOAD_PATH with all extensions
    note: skip when !jl_base_module to avoid UndefVarError(:DL_LOAD_PATH)
*/
    else if (jl_base_module != NULL) {
        jl_array_t *DL_LOAD_PATH = (jl_array_t*)jl_get_global(jl_base_module, jl_symbol("DL_LOAD_PATH"));
        if (DL_LOAD_PATH != NULL) {
            size_t j;
            for (j = 0; j < jl_array_len(DL_LOAD_PATH); j++) {
                char *dl_path = jl_string_data(jl_cell_data(DL_LOAD_PATH)[j]);
                size_t len = strlen(dl_path);
                if (len == 0)
                    continue;
                for(i=0; i < N_EXTENSIONS; i++) {
                    ext = extensions[i];
                    path[0] = '\0';
                    handle->handle = NULL;
                    if (dl_path[len-1] == PATHSEPSTRING[0])
                        snprintf(path, PATHBUF, "%s%s%s", dl_path, modname, ext);
                    else
                        snprintf(path, PATHBUF, "%s" PATHSEPSTRING "%s%s", dl_path, modname, ext);
                    // free handle->errmsg, else it will leak on next uv_dlopen
                    if (handle->errmsg)
                        uv_dlclose(handle);
                    error = jl_uv_dlopen(path, handle, flags);
                    if (!error)
                        goto done;
                    // bail out and show the error if file actually exists
                    if (jl_stat(path, (char*)&stbuf) == 0)
                        goto notfound;
                }
            }
        }
    }

    // now fall back and look in default library paths, for all extensions
    for(i=0; i < N_EXTENSIONS; i++) {
        ext = extensions[i];
        path[0] = '\0';
        handle->handle = NULL;
        snprintf(path, PATHBUF, "%s%s", modname, ext);
        if (handle->errmsg)
            uv_dlclose(handle);
        error = jl_uv_dlopen(path, handle, flags);
        if (!error)
            goto done;
    }

#if defined(__linux__) || defined(__FreeBSD__)
// check map of versioned libs from "libX" to full soname "libX.so.ver"
    {
        const char *soname = jl_lookup_soname(modname, strlen(modname));
        error = (soname==NULL) || jl_uv_dlopen(soname, handle, flags);
        if (!error)
            goto done;
    }
#endif

notfound:
    // copy the error message into the path buffer so we can free the lib handle
    path[0] = '\0';
    snprintf(path, PATHBUF, "%s", uv_dlerror(handle));
    uv_dlclose(handle);
    free(handle);
    if (throw_err)
        jl_errorf("could not load library \"%s\"\n%s", modname, path);
    return NULL;

done:
    return handle;
}

jl_uv_libhandle jl_load_dynamic_library_e(const char *modname, unsigned flags)
{
    return (jl_uv_libhandle) jl_load_dynamic_library_(modname, flags, 0);
}

jl_uv_libhandle jl_load_dynamic_library(const char *modname, unsigned flags)
{
    return (jl_uv_libhandle) jl_load_dynamic_library_(modname, flags, 1);
}

void *jl_dlsym_e(jl_uv_libhandle handle, const char *symbol)
{
    void *ptr;
    int error = uv_dlsym((uv_lib_t *) handle, symbol, &ptr);
    if (error) ptr=NULL;
    return ptr;
}

void *jl_dlsym(jl_uv_libhandle handle, const char *symbol)
{
    void *ptr;
    int error = uv_dlsym((uv_lib_t *) handle, symbol, &ptr);
    if (error) {
        jl_errorf("could not load symbol \"%s\"\n%s", symbol, uv_dlerror((uv_lib_t *) handle));
    }
    return ptr;
}

#ifdef _OS_WINDOWS_
//Look for symbols in win32 libraries
char *jl_dlfind_win32(const char *f_name)
{
    if (jl_dlsym_e(jl_exe_handle, f_name))
        return (char*)1;
    if (jl_dlsym_e(jl_dl_handle, f_name))
        return (char*)2;
    if (jl_dlsym_e(jl_kernel32_handle, f_name))
        return "kernel32";
    if (jl_dlsym_e(jl_ntdll_handle, f_name))
        return "ntdll";
    if (jl_dlsym_e(jl_crtdll_handle, f_name))
#if _MSC_VER == 1800
        return "msvcr120";
#elif defined(_MSC_VER)
#error This version of MSVC has not been tested.
#else
        return "msvcrt";
#endif
    if (jl_dlsym(jl_winsock_handle, f_name))
        return "ws2_32";
    // additional common libraries (libc?) could be added here, but in general,
    // it is better to specify the library explicitly in the code. This exists
    // mainly to ease compatibility with linux, and for libraries that don't
    // have a name (julia.exe and libjulia.dll)
    // We could also loop over all libraries that have been used so far, but, again,
    // explicit is preferred over implicit
    return NULL;
    // oops, we didn't find it. NULL defaults to searching jl_RTLD_DEFAULT_handle,
    // which defaults to jl_dl_handle, where we won't find it, and will throw the
    // appropriate error.
}
#endif

#ifdef __cplusplus
}
#endif
