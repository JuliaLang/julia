// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifdef MMTK_GC

#include <assert.h>
#include "mmtkMutator.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    MMTkMutatorContext mmtk_mutator;
    size_t malloc_sz_since_last_poll;
} jl_gc_tls_states_t;

#ifdef __cplusplus
}
#endif

#endif // MMTK_GC
