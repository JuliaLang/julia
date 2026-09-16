// This file is a part of Julia. License is MIT: https://julialang.org/license

// Definitions of libjulia's public data symbols: the exported data pointers
// (filled in by export_jl_sysimg_globals) and jl_options, jl_n_threads, ....
// Included by the loader (cli/jl_exports.h) and by a static libjulia-internal
// (static_exports.c). Requires jloptions.h; in the internal build it must
// precede julia.h, which redefines the pointer names as macros.

#include "jl_exported_data.inc"

// Define pointer data as `const void * jl_$(name);`
#define XX(name, type)    JL_DLLEXPORT const void * jl_##name;
JL_EXPORTED_DATA_POINTERS(XX)
JL_CONST_GLOBAL_VARS(XX)
#undef XX

// Define symbol data as `$(type) $(name);`
#define XX(name, type)    JL_DLLEXPORT type name;
JL_EXPORTED_DATA_SYMBOLS(XX)
#undef XX
