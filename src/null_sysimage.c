// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stddef.h>
#include "processor.h"

/**
 * These symbols support statically linking the sysimage with libjulia-internal.
 *
 * Here we provide dummy definitions that are used when these are not linked
 * together (the default build configuration). The 0 value of jl_system_image_size
 * is used as a sentinel to indicate that the sysimage should be loaded externally.
 **/
char jl_system_image_data = 0;
size_t jl_system_image_size = 0;
jl_image_pointers_t jl_image_pointers = { 0 };
