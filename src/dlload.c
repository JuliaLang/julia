#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/stat.h>

#if defined(__linux) || defined(__FreeBSD__)
#include <unistd.h>
#include <dlfcn.h>
#define GET_FUNCTION_FROM_MODULE dlsym
#define CLOSE_MODULE dlclose
typedef void * module_handle_t;
static char *extensions[] = { ".so" };
#define N_EXTENSIONS 1

#elif defined(__APPLE__)
#include <unistd.h>
#include <dlfcn.h>
#define GET_FUNCTION_FROM_MODULE dlsym
#define CLOSE_MODULE dlclose
typedef void * module_handle_t;
static char *extensions[] = { ".dylib", ".bundle" };
#define N_EXTENSIONS 2
#elif defined(__WIN32__)
#define GET_FUNCTION_FROM_MODULE dlsym
#define CLOSE_MODULE dlclose
typedef void * module_handle_t;
static char *extensions[] = { "dll" };
#define N_EXTENSIONS 1
#endif

#include "julia.h"
#include "uv.h"

#include <windef.h>
#include <windows.h>

#define PATHBUF 512

extern char *julia_home;

uv_lib_t jl_load_dynamic_library(char *fname)
{
    uv_lib_t handle;
    uv_err_t error;
    char *modname, *ext;
    char path[PATHBUF];
    int i;

    modname = fname;
    if (modname == NULL) {
#ifdef __WIN32__
		HMODULE this_process;
		this_process=GetModuleHandle(NULL);
                handle=this_process;
#else
        uv_dlopen(NULL, handle);
#endif
        return handle;
    }
    char *cwd;

    for(i=0; i < N_EXTENSIONS; i++) {
        ext = extensions[i];
        path[0] = '\0';
        handle = NULL;
        if (modname[0] != '/') {
            if (julia_home) {
                /* try julia_home/lib */
                strncpy(path, julia_home, PATHBUF-1);
                strncat(path, "/lib/", PATHBUF-1-strlen(path));
                strncat(path, modname, PATHBUF-1-strlen(path));
                strncat(path, ext, PATHBUF-1-strlen(path));
                error = uv_dlopen(path, handle);
                if (!error.code) return handle;
                // if file exists but didn't load, show error details
                struct stat sbuf;
                if (stat(path, &sbuf) != -1) {
                    ios_printf(ios_stderr, "%d\n", error.code);
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
                if (!error.code) return handle;
            }
        }
        /* try loading from standard library path */
        strncpy(path, modname, PATHBUF-1);
        strncat(path, ext, PATHBUF-1-strlen(path));
        error = uv_dlopen(path, handle);
        if (!error.code) return handle;
    }
    assert(handle == NULL);
    ios_printf(ios_stderr, "Error: %d\n", error.code);
    jl_errorf("could not load module %s", fname);

    return NULL;
}

void *jl_dlsym(uv_lib_t handle, char *symbol)
{
    void *ptr;
    uv_err_t error = uv_dlsym(handle, symbol, &ptr);
    if (error.code != 0) {
        if(jl_errorexception_type==NULL) {
            ios_printf(ios_stderr, "Error: %d\n", error.code);
            exit(1);
        }
        else
            jl_errorf("dlsym: %d", error.code);
    }
    return ptr;
}
