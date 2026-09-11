// This file is a part of Julia. License is MIT: https://julialang.org/license

// Static build of libjulia-internal (JL_LIBRARY_STATIC): the public data
// symbols and installation lookup that libjulia (cli/) provides in the shared
// build.
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
#include <libgen.h> // dirname

#ifdef _OS_DARWIN_
// the public `jl_*` names, as indirect symbols (see julia.h)
#include "jl_exported_funcs.inc"
JL_RUNTIME_EXPORTED_FUNCS(JL_STATIC_ALIAS)
#endif

// jl_resolve_sysimg_location derives julia_bindir from this as `<libdir>/../bin`
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
