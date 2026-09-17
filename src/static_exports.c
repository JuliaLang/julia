// This file is a part of Julia. License is MIT: https://julialang.org/license

// Static build of libjulia-internal (JL_LIBRARY_STATIC): the public symbols
// that libjulia (cli/) provides in the shared build.
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

#ifdef _OS_DARWIN_
// the public `jl_*` names, as indirect symbols (see julia.h)
#include "julia.h"
#include "jl_exported_funcs.inc"
JL_RUNTIME_EXPORTED_FUNCS(JL_STATIC_ALIAS)
#endif

#endif // JL_LIBRARY_STATIC
