// This file is a part of Julia. License is MIT: https://julialang.org/license

/**
 * Definitions for a static build of libjulia-internal (JL_LIBRARY_STATIC).
 *
 * In the shared build, libjulia (cli/) defines the public data symbols, binds
 * the `jl_*` trampolines to the runtime's `ijl_*` functions, and locates the
 * installation. A static libjulia-internal has no libjulia, so those pieces are
 * provided here. Nothing is defined otherwise.
 **/
#ifdef JL_LIBRARY_STATIC

#include "libsupport.h"
#include "jloptions.h"

// Public data symbols, shared with the loader (cli/jl_exports.h). These must
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
#include "jl_exported_funcs.inc"
#include <libgen.h> // dirname

// n.b. `jl_small_typeof` is not defined here: the system image linked into the
// binary defines it (aotcompile.cpp), and export_jl_small_typeof fills it in.

// The public `jl_*` function names are provided by the loader's trampolines
// (cli/trampolines/*.S, compiled into the archive), each of which jumps through
// its `jl_<name>_addr` slot. The loader fills the slots with dlsym at load time;
// here they are bound at link time to the runtime's `ijl_` implementations
// (jl_internal_funcs.inc). The runtime must not define any of these public names
// itself in the static build (see `jl_egal` in builtins.c), since PE-COFF weak
// symbols are not usable across object files with GNU ld. The `__asm__` label
// references the `ijl_` symbol directly: not every exported function is
// declared in a header visible here, and spelling `ijl_<name>` in C would clash
// with the prototypes julia.h does declare.
#if defined(_OS_DARWIN_) || (defined(_OS_WINDOWS_) && defined(_CPU_X86_))
#define JL_TRAMPOLINE_TARGET(name) "_i" name // C ABI symbols have an underscore prefix
#else
#define JL_TRAMPOLINE_TARGET(name) "i" name
#endif
typedef void (anonfunc)(void);
#define XX(name) \
    extern anonfunc jl_static_impl_##name __asm__(JL_TRAMPOLINE_TARGET(#name)); \
    JL_HIDDEN anonfunc *const name##_addr = &jl_static_impl_##name;
JL_RUNTIME_EXPORTED_FUNCS(XX)
#ifdef _OS_WINDOWS_
JL_RUNTIME_EXPORTED_FUNCS_WIN(XX)
#endif
#undef XX

// Normally provided by the libjulia loader as the directory containing
// libjulia. The static runtime is linked into whatever the consumer built, so
// this is the directory of the object containing this function: the executable
// or a shared library. Where the main executable has no pathname in its link
// map (glibc, musl) the executable's path is used instead.
// n.b. jl_resolve_sysimg_location derives julia_bindir from this as
// `<libdir>/../bin` (except on Windows), i.e. it assumes the layout of an
// installation with the executable in `bin/`.
const char *jl_get_libdir(void) // declared in julia.h
{
    static char *libdir = NULL;
    if (libdir != NULL)
        return libdir;
    char *path = (char*)malloc_s(JL_PATH_MAX);
    size_t size = JL_PATH_MAX;
    const char *modpath = jl_pathname_for_symbol((void*)&jl_get_libdir);
    if (modpath != NULL && modpath[0] != '\0') {
        size = strlen(modpath);
        if (size >= JL_PATH_MAX) {
            jl_safe_printf("ERROR: path of the julia runtime is too long\n");
            abort();
        }
        memcpy(path, modpath, size + 1);
    }
    else if (uv_exepath(path, &size) != 0) {
        jl_safe_printf("ERROR: unable to determine the path of the executable\n");
        abort();
    }
    // dirname may either modify its argument in place or return a static buffer
    const char *dir = dirname(path);
    if (dir != path)
        memcpy(path, dir, strlen(dir) + 1); // never longer than the input
    libdir = path;
    return libdir;
}

// Convenience initializer for a program that has the runtime and the system
// image linked in statically: initializes the options and boots the runtime
// from the object containing this function, like the libjulia loader plus
// jl_init do in the shared build.
JL_DLLEXPORT void jl_init_static(void)
{
    jl_init_options();
    void *handle = jl_find_dynamic_library_by_addr((void*)&jl_init_static, /* throw_err */ 1, /* close */ 0);
    jl_init_with_image_handle(handle);
}

#endif // JL_LIBRARY_STATIC
