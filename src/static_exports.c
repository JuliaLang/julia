// This file is a part of Julia. License is MIT: https://julialang.org/license

// Static build of libjulia-internal (JL_LIBRARY_STATIC): what libjulia (cli/)
// provides in the shared build. Empty otherwise.
#ifdef JL_LIBRARY_STATIC

#include "libsupport.h"
#include "jloptions.h"

// Public data symbols (shared with cli/jl_exports.h). Must precede julia.h,
// which redefines these names as macros for the internal copies.
#include "jl_exported_data_defs.inc"

// Their addresses, in list order, for export_jl_sysimg_globals (jltypes.c).
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

// jl_small_typeof is defined by the linked system image (aotcompile.cpp).

// The public `jl_*` names are the loader's trampolines (cli/trampolines/*.S),
// which jump through `jl_<name>_addr`. The loader fills the slots with dlsym;
// here they are bound at link time to the `ijl_*` implementations. Only the
// trampolines may define these names (see jl_egal in builtins.c): PE-COFF weak
// symbols do not work across objects with GNU ld. The `__asm__` label avoids
// needing a prototype for every `ijl_*` function.
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

// In the loader, the directory of libjulia; here, the directory of the object
// containing the static runtime (the executable or a shared library).
// jl_resolve_sysimg_location takes julia_bindir to be `<libdir>/../bin`
// (except on Windows), i.e. an installation layout.
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
    // dirname may modify its argument or return a static buffer
    const char *dir = dirname(path);
    if (dir != path)
        memcpy(path, dir, strlen(dir) + 1); // never longer than the input
    libdir = path;
    return libdir;
}

// Initialize from the statically linked runtime and system image: what the
// loader plus jl_init do in the shared build.
JL_DLLEXPORT void jl_init_static(void)
{
    jl_init_options();
    void *handle = jl_find_dynamic_library_by_addr((void*)&jl_init_static, /* throw_err */ 1, /* close */ 0);
    jl_init_with_image_handle(handle);
}

#endif // JL_LIBRARY_STATIC
