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

DLLEXPORT size_t jl_sizeof_uv_lib_t() { return sizeof(uv_lib_t); }

DLLEXPORT int jl_uv_dlopen(const char* filename, uv_lib_t* lib) {
    lib->errmsg=NULL;
    if (filename == NULL) {
#ifdef _WIN32
        if(!GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
            (LPCSTR)(&jl_uv_dlopen),
            &lib->handle))
            jl_errorf("could not load base module", modname);
#else
        lib->handle = dlopen(NULL,RTLD_NOW);
        assert(lib->handle != NULL);
#endif
        return 0;
    } else {
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
}

#define PATHBUF 512
extern char *julia_home;
// minimal boot-strapping dlopen() function, until Julia has enough functions to implement
// the necessary string and array processing for path and extension searching :O
// (this is only actually used for loading libfdm)
DLLEXPORT uv_lib_t *c_dlopen(char *modname) {
    int error;
    char *ext;
    char path[PATHBUF];
    int i;
    uv_lib_t *handle=malloc(sizeof(uv_lib_t));
    handle->errmsg=NULL;

    for(i=0; i < N_EXTENSIONS; i++) {
        ext = extensions[i];
        path[0] = '\0';
        handle->handle = NULL;
        if (modname[0] != '/') {
            if (julia_home) {
                /* try julia_home/../lib */
                snprintf(path, PATHBUF, "%s/../lib/%s%s", julia_home, modname, ext);
                error = jl_uv_dlopen(path, handle);
                if (!error) return handle;
                // if file exists but didn't load, show error details
                struct stat sbuf;
                if (stat(path, &sbuf) != -1) {
                    //warning: this leaks memory:
                    jl_errorf("could not load module %s: %s", modname, uv_dlerror(handle));
                }
            }
        }
        /* try loading from standard library path */
        snprintf(path, PATHBUF, "%s%s", modname, ext);
        error = jl_uv_dlopen(path, handle);
        if (!error) return handle;
    }

    //warning: this leaks memory:
    jl_errorf("could not load module %s: %s", modname, uv_dlerror(handle));
    return NULL;
}


void *jl_dlsym_e(uv_lib_t *handle, char *symbol) {
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
