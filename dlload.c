#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <malloc.h>
#include <windows.h>
#undef TRUE
#undef FALSE
#undef VOID
#define GET_FUNCTION_FROM_MODULE GetProcAddress
#define CLOSE_MODULE FreeLibrary
typedef HINSTANCE module_handle_t;
#define EXTENSION ".dll"

#elif defined(LINUX)
#include <unistd.h>
#include <dlfcn.h>
#define GET_FUNCTION_FROM_MODULE dlsym
#define CLOSE_MODULE dlclose
typedef void * module_handle_t;
#define EXTENSION ".so"

#elif defined(MACOSX) || defined(MACINTEL)
#include <unistd.h>
#include <dlfcn.h>
#define GET_FUNCTION_FROM_MODULE dlsym
#define CLOSE_MODULE dlclose
typedef void * module_handle_t;
#define EXTENSION ".bundle"

#endif

#ifdef BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

#define PATHBUF 512

#ifdef WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern char *julia_home;

void *jl_load_dynamic_library(char *fname)
{
    module_handle_t handle;
    char *modname;
    char path[PATHBUF];
    path[0] = '\0';
#ifdef WIN32
    LPVOID lpMsgBuf;
    DWORD dw;
#endif

    modname = fname;
#ifndef WIN32
    if (modname == NULL) {
        return (void*)dlopen(NULL, RTLD_NOW);
    }
    char *cwd;

    /* first, try load from current directory */
    if (modname[0] != '/') {
        cwd = getcwd(path, PATHBUF);
        if (cwd == NULL)
            return NULL;
        strncat(path, "/", PATHBUF-1-strlen(path));
        strncat(path, modname, PATHBUF-1-strlen(path));

        handle = dlopen(path, RTLD_NOW);
        if (handle == NULL) {
            /* try julia home */
            if (julia_home) {
                strncpy(path, julia_home, PATHBUF-1);
                strncat(path, "/", PATHBUF-1-strlen(path));
                strncat(path, modname, PATHBUF-1-strlen(path));
                handle = dlopen(path, RTLD_NOW);
            }
            if (handle == NULL) {
                //ios_printf(ios_stderr, "%s\n", dlerror());
                /* try loading from standard library path */
                handle = dlopen(modname, RTLD_NOW);
                if (handle == NULL) {
                    ios_printf(ios_stderr, "%s\n", dlerror());
                    jl_errorf("could not load module %s", fname);
                }
            }
        }
    }
    else {
        handle = dlopen(modname, RTLD_NOW);
        if (handle == NULL) {
            ios_printf(ios_stderr, "%s\n", dlerror());
            jl_errorf("could not load module %s", fname);
        }
    }
#else
    if (modname == NULL) {
        return (void*)GetModuleHandle(NULL);
    }
    handle = LoadLibrary(modname);
    if (handle == NULL) {
        if (julia_home) {
            strncpy(path, julia_home, PATHBUF-1);
            strncat(path, "\\", PATHBUF-1-strlen(path));
            strncat(path, modname, PATHBUF-1-strlen(path));
            handle = LoadLibrary(path);
        }
        if (handle == NULL) {
            dw = GetLastError();
            FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                          FORMAT_MESSAGE_FROM_SYSTEM,
                          NULL,
                          dw,
                          MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                          (LPTSTR) &lpMsgBuf,
                          0, NULL );
            jl_errorf("could not load module %s: (%d) %s", fname, dw,
                      lpMsgBuf);
        }
    }
#endif

    return (void*)handle;
}

void *jl_dlsym(void *handle, char *symbol)
{
#ifndef WIN32
    (void)dlerror();
    void *ptr = dlsym(handle, symbol);
    char *msg = dlerror();
    if (msg != NULL) {
        jl_errorf("dlsym: %s", msg);
    }
    return ptr;
#else
    void *ptr = GET_FUNCTION_FROM_MODULE(handle, symbol);
#endif
    return ptr;
}
