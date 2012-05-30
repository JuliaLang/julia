#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/stat.h>

#if defined(__linux__) || defined(__FreeBSD__)
#include <unistd.h>
#include <dlfcn.h>
#define GET_FUNCTION_FROM_MODULE dlsym
#define CLOSE_MODULE dlclose
typedef void * module_handle_t;
static char *extensions[] = { ".so", "" };
#define N_EXTENSIONS 2

#elif defined(__APPLE__)
#include <unistd.h>
#include <dlfcn.h>
#define GET_FUNCTION_FROM_MODULE dlsym
#define CLOSE_MODULE dlclose
typedef void * module_handle_t;
static char *extensions[] = { "", ".dylib", ".bundle" };
#define N_EXTENSIONS 3
#elif defined(__WIN32__)
#define _WIN32_WINNT 0x0501
#include <windows.h>
#include <direct.h>
#define GET_FUNCTION_FROM_MODULE dlsym
#define CLOSE_MODULE dlclose
typedef void * module_handle_t;
static char *extensions[] = { ".dll" };
#define N_EXTENSIONS 1
#endif

#include "julia.h"
#include "uv.h"

#define PATHBUF 512

extern char *julia_home;

uv_lib_t *jl_load_dynamic_library(char *fname)
{
    int error;
    char *modname, *ext;
    char path[PATHBUF];
    int i;
    uv_lib_t *handle=malloc(sizeof(uv_lib_t));
    handle->errmsg=NULL;

    modname = fname;
    if (modname == NULL) {
#if defined(__WIN32__)
		if(!GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
          (LPCSTR)(&jl_load_dynamic_library),
          &handle->handle))
			    jl_errorf("could not load base module", fname);
#else
        handle->handle = dlopen(NULL,RTLD_NOW);
#endif
        goto done;
    }
#if defined(__WIN32__)
	else if (modname[1] == ':') {
#else
    else if (modname[0] == '/') {
#endif
        error = uv_dlopen(modname,handle);
        if (!error) goto done;
    }
    char *cwd;

    for(i=0; i < N_EXTENSIONS; i++) {
        ext = extensions[i];
        path[0] = '\0';
        handle->handle = NULL;
        if (modname[0] != '/') {
            if (julia_home) {
                /* try julia_home/../lib */
                strncpy(path, julia_home, PATHBUF-1);
                strncat(path, "/../lib/", PATHBUF-1-strlen(path));
                strncat(path, modname, PATHBUF-1-strlen(path));
                strncat(path, ext, PATHBUF-1-strlen(path));
                error = uv_dlopen(path, handle);
                if (!error) goto done;
                // if file exists but didn't load, show error details
                struct stat sbuf;
                if (stat(path, &sbuf) != -1) {
					JL_PRINTF(JL_STDERR, "could not load module %s (%d): %s\n", fname, error, uv_dlerror(handle));
                    jl_errorf("could not load module %s", fname);
                }
            }
            cwd = getcwd(path, PATHBUF);
            if (cwd != NULL) {
                /* next try load from current directory */
                strncat(path, "/", PATHBUF-1-strlen(path));
                strncat(path, modname, PATHBUF-1-strlen(path));
                strncat(path, ext, PATHBUF-1-strlen(path));
                error = uv_dlopen(path, handle);
                if (!error) goto done;
            }
        }
        /* try loading from standard library path */
        strncpy(path, modname, PATHBUF-1);
        strncat(path, ext, PATHBUF-1-strlen(path));
        error = uv_dlopen(path, handle);
        if (!error) goto done;
    }

    JL_PRINTF(JL_STDERR, "could not load module %s (%d): %s\n", fname, error, uv_dlerror(handle));
    jl_errorf("could not load module %s", fname);
    free(handle);
    return NULL;
done:
    return handle;
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
