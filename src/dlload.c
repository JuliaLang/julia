#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/stat.h>

#include "platform.h"
#include "julia.h"
#include "uv.h"
#ifdef _OS_WINDOWS_
#include <windows.h>
#include <direct.h>
#else
#include <unistd.h>
#include <dlfcn.h>
#endif

#if defined(__APPLE__)
static char *extensions[] = { "", ".dylib" };
#define N_EXTENSIONS 2
#elif defined(_OS_WINDOWS_)
static char *extensions[] = { "", ".dll" };
#define N_EXTENSIONS 2
#if defined(_CPU_X86_64_)
int needsSymRefreshModuleList = 0;
#endif
#else
static char *extensions[] = { ".so", "" };
#define N_EXTENSIONS 2
#endif


#define PATHBUF 512

extern char *julia_home;

#if defined(__linux__)
char *jl_lookup_soname(char *pfx, size_t n);
#endif

#define JL_RTLD(flags, FLAG) (flags & JL_RTLD_ ## FLAG ? RTLD_ ## FLAG : 0)

static int jl_uv_dlopen(const char* filename, uv_lib_t* lib, unsigned flags)
{
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
    needsSymRefreshModuleList = 1;
#endif
#if defined(RTLD_GLOBAL) && defined(RTLD_LAZY) /* POSIX flags available */
    dlerror(); /* Reset error status. */
    lib->handle = dlopen(filename, 
                         (flags & JL_RTLD_NOW ? RTLD_NOW : RTLD_LAZY)
                         | JL_RTLD(flags, GLOBAL) | JL_RTLD(flags, LOCAL)
#ifdef RTLD_NODELETE
                         | JL_RTLD(flags, NODELETE)
#endif
#ifdef RTLD_NOLOAD
                         | JL_RTLD(flags, NOLOAD)
#endif
#ifdef RTLD_DEEPBIND
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

static uv_lib_t *jl_load_dynamic_library_(char *modname, unsigned flags, int throw_err)
{
    int error;
    char *ext;
    char path[PATHBUF];
    int i;
    uv_lib_t *handle=malloc(sizeof(uv_lib_t));
    handle->errmsg=NULL;

    if (modname == NULL) {
#ifdef _OS_WINDOWS_
        if (!GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                                (LPCSTR)(&jl_load_dynamic_library),
                                &handle->handle))
            jl_errorf("could not load base module", modname);
#else
        handle->handle = dlopen(NULL,RTLD_NOW);
#endif
        goto done;
    }
#ifdef _OS_WINDOWS_
    else if (modname[1] == ':') {
#else
    else if (modname[0] == '/') {
#endif
        error = jl_uv_dlopen(modname,handle,flags);
        if (!error) goto done;
    } else {
        jl_array_t* DL_LOAD_PATH = (jl_array_t*)jl_get_global(jl_main_module, jl_symbol("DL_LOAD_PATH"));
        if (DL_LOAD_PATH != NULL) {
            size_t i;
            for (i = 0; i < jl_array_len(DL_LOAD_PATH); i++) {
                char *dl_path = jl_string_data(jl_cell_data(DL_LOAD_PATH)[i]);
                size_t len = strlen(dl_path);
                if (len == 0)
                    continue;
                for(i=0; i < N_EXTENSIONS; i++) {
                    ext = extensions[i];
                    path[0] = '\0';
                    handle->handle = NULL;
                    if (dl_path[len-1] == PATHSEP)
                        snprintf(path, PATHBUF, "%s%s%s", dl_path, modname, ext);
                    else
                        snprintf(path, PATHBUF, "%s" PATHSEPSTRING "%s%s", dl_path, modname, ext);
                    error = jl_uv_dlopen(path, handle, flags);
                    if (!error) goto done;
                }
            }
        }
    }
    for(i=0; i < N_EXTENSIONS; i++) {
        ext = extensions[i];
        path[0] = '\0';
        handle->handle = NULL;
        /* try loading from standard library path */
        snprintf(path, PATHBUF, "%s%s", modname, ext);
        error = jl_uv_dlopen(path, handle, flags);
        if (!error) goto done;
    }
#if defined(__linux__)
    char *soname = jl_lookup_soname(modname, strlen(modname));
    error = (soname==NULL) || jl_uv_dlopen(soname, handle, flags);
    if (!error) goto done;
#endif

    if (throw_err) {
        //JL_PRINTF(JL_STDERR, "could not load module %s (%d): %s\n", modname, error, uv_dlerror(handle));
        jl_errorf("could not load module %s: %s", modname, uv_dlerror(handle));
    }
    uv_dlclose(handle);
    free(handle);
    return NULL;
done:
    return handle;
}

uv_lib_t *jl_load_dynamic_library_e(char *modname, unsigned flags)
{
    return jl_load_dynamic_library_(modname, flags, 0);
}

uv_lib_t *jl_load_dynamic_library(char *modname, unsigned flags)
{
    return jl_load_dynamic_library_(modname, flags, 1);
}

DLLEXPORT void *jl_dlsym_e(uv_lib_t *handle, char *symbol)
{
    void *ptr;
    int  error=uv_dlsym(handle, symbol, &ptr);
    if (error) ptr=NULL;
    return ptr;
}

DLLEXPORT void *jl_dlsym(uv_lib_t *handle, char *symbol)
{
    void *ptr;
    int  error = uv_dlsym(handle, symbol, &ptr);
    if (error != 0) {
        jl_printf(JL_STDERR, "symbol could not be found %s (%d): %s\n", symbol, error, uv_dlerror(handle));
    }
    return ptr;
}

#ifdef _OS_WINDOWS_
//Look for symbols in win32 libraries
void *jl_dlsym_win32(char *f_name)
{
    void *fptr = jl_dlsym_e(jl_exe_handle, f_name);
    if (!fptr) {
        fptr = jl_dlsym_e(jl_dl_handle, f_name);
        if (!fptr) {
            fptr = jl_dlsym_e(jl_kernel32_handle, f_name);
            if (!fptr) {
                fptr = jl_dlsym_e(jl_ntdll_handle, f_name);
                if (!fptr) {
                    fptr = jl_dlsym_e(jl_crtdll_handle, f_name);
                    if (!fptr) {
                        fptr = jl_dlsym(jl_winsock_handle, f_name);
                    }
                }
            }
        }
    }
    return fptr;
}

#endif
