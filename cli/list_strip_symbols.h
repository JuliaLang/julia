// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "jl_exported_funcs.inc"
#include "trampolines/common.h"
#define XX(x) --strip-symbol=CNAME(x)
JL_RUNTIME_EXPORTED_FUNCS(XX)
JL_RUNTIME_EXPORTED_FUNCS_WIN(XX)
JL_CODEGEN_EXPORTED_FUNCS(XX)
#undef XX
