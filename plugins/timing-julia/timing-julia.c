// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <inttypes.h>
#include "julia.h"

JL_DLLEXPORT int jl_timing_available_impl(void) {
    return 1;
}
