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
#include "jl_exported_data.inc"

// Definitions of the exported pointers `jl_<name>`. These must come before
// julia.h is included, since inside libjulia-internal it redefines each of
// these names as a macro for the internal copy (jl_data_globals_defs.inc).
#define XX(name, type) JL_DLLEXPORT const void *jl_##name;
JL_EXPORTED_DATA_POINTERS(XX)
JL_CONST_GLOBAL_VARS(XX)
#undef XX

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

// The public copy of ijl_small_typeof, filled in by export_jl_small_typeof
JL_DLLEXPORT jl_datatype_t *jl_small_typeof[(jl_max_tags << 4) / sizeof(jl_datatype_t*)];

// Data symbols that live in libjulia in the shared build (jl_options, jl_n_threads, ...)
#define XX(name, type) JL_DLLEXPORT type name;
JL_EXPORTED_DATA_SYMBOLS(XX)
#undef XX

#endif // JL_LIBRARY_STATIC
