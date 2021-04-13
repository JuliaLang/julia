// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "jl_exported_funcs.inc"
#include "trampolines/common.h"
#define XX(x) --strip-symbol=CNAME(x)
JL_EXPORTED_FUNCS(XX)
#undef XX
