// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stddef.h>
#include "processor.h"

/**
 * These symbols support statically linking the sysimage with libjulia-internal.
 *
 * Here we provide dummy definitions that are used when these are not linked
 * together (the default build configuration). The 0 value of jl_image_unpack
 * is used as a sentinel to indicate that the sysimage should be loaded externally.
 *
 * In a static build of libjulia-internal (JL_LIBRARY_STATIC) the sysimage must
 * be linked in and provides the real definition, so nothing is defined here.
 **/
#ifndef JL_LIBRARY_STATIC
jl_image_unpack_func_t jl_image_unpack = NULL;
#endif
