// This file is a part of Julia. License is MIT: https://julialang.org/license

/**
 * Public data symbols for a static build of libjulia-internal.
 *
 * In the normal (shared) build these are defined by the libjulia loader
 * (cli/jl_exports.h) and filled in by `export_jl_sysimg_globals` and
 * `export_jl_small_typeof` through `jl_libjulia_handle`. A static
 * libjulia-internal (JL_LIBRARY_STATIC) has no separate libjulia, so it has to
 * provide the public copies itself. Nothing is defined here otherwise.
 **/
#ifdef JL_LIBRARY_STATIC

#include "libsupport.h"
#include "jloptions.h"

// Define the public data symbols (see the comments in the .inc). These must
// come before julia.h is included, since inside libjulia-internal it redefines
// the pointer names as macros for the internal copies (jl_data_globals_defs.inc).
#include "jl_exported_data_defs.inc"

// Addresses of the above in the order of JL_EXPORTED_DATA_POINTERS followed by
// JL_CONST_GLOBAL_VARS, so that export_jl_sysimg_globals (jltypes.c) can fill
// them in without naming them.
#define XX(name, type) &jl_##name,
JL_HIDDEN const void **const jl_static_exported_data_ptrs[] = {
    JL_EXPORTED_DATA_POINTERS(XX)
    JL_CONST_GLOBAL_VARS(XX)
};
#undef XX

#include "julia.h"
#include "julia_internal.h"
#ifdef _OS_WINDOWS_
#include <windows.h>
#else
#include <dlfcn.h>
#endif
#include "jl_exported_funcs.inc"

// n.b. `jl_small_typeof` is not defined here: the system image linked into the
// binary defines it (aotcompile.cpp), and export_jl_small_typeof fills it in.

// The runtime's exported functions are defined under their `ijl_` names
// (jl_internal_funcs.inc); libjulia normally re-exports them as `jl_`. Provide
// the `jl_` names here as tail-jump thunks. Each thunk lives in its own section
// so that --gc-sections can drop the unused ones, and is weak so that functions
// which the runtime also defines under their `jl_` name (e.g. jl_egal) win.
#if defined(_OS_WINDOWS_)
// TODO: COFF needs a different mechanism (e.g. a .def file with aliases)
#else
#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
#define JL_THUNK_BRANCH "jmp "
#elif defined(_CPU_AARCH64_) || defined(_CPU_ARM_)
#define JL_THUNK_BRANCH "b "
#elif defined(_CPU_RISCV64_)
#define JL_THUNK_BRANCH "tail "
#elif defined(_CPU_PPC64_)
#define JL_THUNK_BRANCH "b "
#else
#error "unsupported architecture for static jl_ function thunks"
#endif
#if defined(_OS_DARWIN_)
#define XX(name) __asm__( \
    ".section __TEXT,__text,regular,pure_instructions\n" \
    "\t.globl _" #name "\n" \
    "\t.weak_definition _" #name "\n" \
    "\t.p2align 2\n" \
    "_" #name ":\n" \
    "\t" JL_THUNK_BRANCH "_i" #name "\n");
#else
#define XX(name) __asm__( \
    ".section .text." #name ",\"ax\",@progbits\n" \
    "\t.weak " #name "\n" \
    "\t.type " #name ",@function\n" \
    "\t.p2align 2\n" \
    #name ":\n" \
    "\t" JL_THUNK_BRANCH "i" #name "\n" \
    "\t.size " #name ", .-" #name "\n");
#endif
JL_RUNTIME_EXPORTED_FUNCS(XX)
#undef XX
#endif // !_OS_WINDOWS_

// Normally provided by the libjulia loader: the directory of the object
// containing libjulia. Since the static runtime is linked into whatever the
// consumer built, that is the directory containing this executable or shared
// library, found the same way the loader finds itself (by the address of this
// function). Falls back to the executable's path when the containing object
// has no name (the main program on some platforms).
JL_DLLEXPORT const char *jl_get_libdir(void)
{
    static char *libdir = NULL;
    if (libdir != NULL)
        return libdir;
    char *path = (char*)malloc_s(JL_PATH_MAX);
    size_t size = JL_PATH_MAX;
    const char *modpath = NULL;
    void *handle = jl_find_dynamic_library_by_addr((void*)&jl_get_libdir, /* throw_err */ 0, /* close */ 1);
    if (handle != NULL)
        modpath = jl_pathname_for_handle(handle);
    if (modpath != NULL && modpath[0] != '\0') {
        size = strlen(modpath);
        if (size >= JL_PATH_MAX)
            size = JL_PATH_MAX - 1;
        memcpy(path, modpath, size);
    }
    else if (uv_exepath(path, &size) != 0) {
        free(path);
        return NULL;
    }
    path[size] = '\0';
    char *sep = NULL;
    for (char *p = path; *p; p++) {
#ifdef _OS_WINDOWS_
        if (*p == '/' || *p == '\\')
#else
        if (*p == '/')
#endif
            sep = p;
    }
    if (sep == NULL) {
        path[0] = '.';
        path[1] = '\0';
    }
    else if (sep == path) {
        sep[1] = '\0';
    }
    else {
        *sep = '\0';
    }
    libdir = path;
    return libdir;
}

// Convenience initializer for a program that has the runtime and the system
// image linked in statically: initializes the options and boots the runtime
// from the executable itself. Equivalent to what the libjulia loader plus
// jl_init do in the shared build.
JL_DLLEXPORT void jl_init_static(void)
{
    jl_init_options();
#ifdef _OS_WINDOWS_
    void *handle = GetModuleHandleW(NULL);
#else
    void *handle = dlopen(NULL, RTLD_NOW | RTLD_NOLOAD | RTLD_LOCAL);
#endif
    jl_init_with_image_handle(handle);
}

#endif // JL_LIBRARY_STATIC
