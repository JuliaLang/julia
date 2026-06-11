// This file is a part of Julia. License is MIT: https://julialang.org/license

// Local stand-ins for the global data pointers that the CLI loader library
// normally exports (JL_EXPORTED_DATA_POINTERS / JL_CONST_GLOBAL_VARS).
// The embedded image's native code references these by symbol; defining
// them here makes those references bind to this library's own runtime
// values rather than the host's (the library is loaded with RTLD_DEEPBIND
// and linked with -Bsymbolic). The runtime's export_jl_sysimg_globals
// fills the slots after image restore through jl_libjulia_handle, which
// resolves to this library: it is located by the address of this library's
// own jl_options copy.
//
// This translation unit is compiled WITHOUT -DJL_LIBRARY_EXPORTS_INTERNAL:
// in internal translation units the global names below are macros over the
// runtime's hidden struct copies, so the symbol definitions can only be
// spelled here.

#include "julia.h"

#define XX(name, type) JL_DLLEXPORT type jl_##name;
JL_EXPORTED_DATA_POINTERS(XX)
JL_CONST_GLOBAL_VARS(XX)
#undef XX

