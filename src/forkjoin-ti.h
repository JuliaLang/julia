// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef FORKJOINTI_H
#define FORKJOINTI_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// interface provided by this threading infrastructure
JL_DLLEXPORT jl_value_t *jl_threading_run(jl_value_t *_args);


#ifdef __cplusplus
}
#endif

#endif  /* FORKJOINTI_H */

