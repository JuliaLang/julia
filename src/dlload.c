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
#endif

#include "julia.h"

#define PATHBUF 512

extern char *julia_home;

void *jl_load_dynamic_library(char *fname)
{
    module_handle_t handle;
    char *modname, *ext;
    char path[PATHBUF];
    int i;

    modname = fname;
    if (modname == NULL) {
        return (void*)dlopen(NULL, RTLD_NOW);
    }
    else if (modname[0] == '/') {
        handle = dlopen(modname, RTLD_NOW);
        if (handle != NULL) return handle;
    }
    char *cwd;

    for(i=0; i < N_EXTENSIONS; i++) {
        ext = extensions[i];
        path[0] = '\0';
        handle = NULL;
        if (modname[0] != '/') {
            if (julia_home) {
                /* try julia_home/usr/lib */
                strncpy(path, julia_home, PATHBUF-1);
                strncat(path, "/usr/lib/", PATHBUF-1-strlen(path));
                strncat(path, modname, PATHBUF-1-strlen(path));
                strncat(path, ext, PATHBUF-1-strlen(path));
                handle = dlopen(path, RTLD_NOW);
                if (handle != NULL) return handle;
                // if file exists but didn't load, show error details
                struct stat sbuf;
                if (stat(path, &sbuf) != -1) {
                    ios_printf(ios_stderr, "%s\n", dlerror());
                    jl_errorf("could not load module %s", fname);
                }
            }
            cwd = getcwd(path, PATHBUF);
            if (cwd != NULL) {
                /* next try load from current directory */
                strncat(path, "/", PATHBUF-1-strlen(path));
                strncat(path, modname, PATHBUF-1-strlen(path));
                strncat(path, ext, PATHBUF-1-strlen(path));
                handle = dlopen(path, RTLD_NOW);
                if (handle != NULL) return handle;
            }
        }
        /* try loading from standard library path */
        strncpy(path, modname, PATHBUF-1);
        strncat(path, ext, PATHBUF-1-strlen(path));
        handle = dlopen(path, RTLD_NOW);
        if (handle != NULL) return handle;
    }
    assert(handle == NULL);
    ios_printf(ios_stderr, "%s\n", dlerror());
    jl_errorf("could not load module %s", fname);

    return NULL;
}

void *jl_dlsym(void *handle, char *symbol)
{
    (void)dlerror();
    void *ptr = dlsym(handle, symbol);
    char *msg = dlerror();
    if (msg != NULL) {
        jl_errorf("dlsym: %s", msg);
    }
    return ptr;
}
