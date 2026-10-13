// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "jl_exported_funcs.inc"
#include "../src/support/platform.h"
// Export definitions use undecorated names, including on Windows i686.
#define XX(x) x
JL_RUNTIME_EXPORTED_FUNCS(XX)
#ifdef _OS_WINDOWS_
JL_RUNTIME_EXPORTED_FUNCS_WIN(XX)
#endif
#undef XX
