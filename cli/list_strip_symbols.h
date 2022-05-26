// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "jl_exported_funcs.inc"
#include "trampolines/common.h"
#define XX(x) --strip-symbol=CNAME(x)
JL_RUNTIME_EXPORTED_FUNCS(XX)
#ifdef _OS_WINDOWS_
JL_RUNTIME_EXPORTED_FUNCS_WIN(XX)
#endif
#undef XX
