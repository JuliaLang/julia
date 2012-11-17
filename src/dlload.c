#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/stat.h>

#ifdef _WIN32
#  define _WIN32_WINNT 0x0501
#  include <windows.h>
#  include <direct.h>
#else
#  include <unistd.h>
#  include <dlfcn.h>
#endif

#if defined(__APPLE__)
static char *extensions[] = { "", ".dylib" };
#define N_EXTENSIONS 2
#elif defined(_WIN32)
static char *extensions[] = { ".dll" };
#define N_EXTENSIONS 1
#else
static char *extensions[] = { ".so", "" };
#define N_EXTENSIONS 2
#endif

#include "julia.h"
#include "uv.h"

#define PATHBUF 512

extern char *julia_home;

int jl_uv_dlopen(const char* filename, uv_lib_t* lib)
{
#ifdef RTLD_DEEPBIND
    dlerror(); /* Reset error status. */
    lib->handle = dlopen(filename, RTLD_LAZY|RTLD_DEEPBIND);
    if (lib->handle) {
        lib->errmsg = NULL;
        return 0;
    } else {
        lib->errmsg = strdup(dlerror());
        return -1;
    }
#else
    return uv_dlopen(filename, lib);
#endif
}

uv_lib_t *jl_load_dynamic_library(char *modname)
{
    int error;
    char *ext;
    char path[PATHBUF];
    int i;
    uv_lib_t *handle=malloc(sizeof(uv_lib_t));
    handle->errmsg=NULL;

    if (modname == NULL) {
#ifdef _WIN32
        if(!GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
            (LPCSTR)(&jl_load_dynamic_library),
            &handle->handle))
            jl_errorf("could not load base module", modname);
#else
        handle->handle = dlopen(NULL,RTLD_NOW);
#endif
        goto done;
    }
#ifdef _WIN32
    else if (modname[1] == ':') {
#else
    else if (modname[0] == '/') {
#endif
        error = jl_uv_dlopen(modname,handle);
        if (!error) goto done;
    }

    for(i=0; i < N_EXTENSIONS; i++) {
        ext = extensions[i];
        path[0] = '\0';
        handle->handle = NULL;
        if (modname[0] != '/') {
            if (julia_home) {
                /* try julia_home/../lib */
                snprintf(path, PATHBUF, "%s/../lib/%s%s", julia_home, modname, ext);
                error = jl_uv_dlopen(path, handle);
                if (!error) goto done;
                // if file exists but didn't load, show error details
                struct stat sbuf;
                if (stat(path, &sbuf) != -1) {
                    //JL_PRINTF(JL_STDERR, "could not load module %s (%d): %s\n", modname, error, uv_dlerror(handle));
                    jl_errorf("could not load module %s: %s", modname, uv_dlerror(handle));
                }
            }
        }
        /* try loading from standard library path */
        snprintf(path, PATHBUF, "%s%s", modname, ext);
        error = jl_uv_dlopen(path, handle);
        if (!error) goto done;
    }

    //JL_PRINTF(JL_STDERR, "could not load module %s (%d): %s\n", modname, error, uv_dlerror(handle));
    jl_errorf("could not load module %s: %s", modname, uv_dlerror(handle));
    uv_dlclose(handle);
    free(handle);
    return NULL;
done:
    return handle;
}

void *jl_dlsym_e(uv_lib_t *handle, char *symbol)
{
    void *ptr;
    int  error=uv_dlsym(handle, symbol, &ptr);
    if(error) ptr=NULL;
    return ptr;
}

void *jl_dlsym(uv_lib_t *handle, char *symbol)
{
    void *ptr;
    int  error = uv_dlsym(handle, symbol, &ptr);
    if (error != 0) {
        JL_PRINTF(JL_STDERR, "symbol could not be found %s (%d): %s\n", symbol, error, uv_dlerror(handle));
    }
    return ptr;
}
