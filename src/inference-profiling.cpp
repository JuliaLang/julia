// Implementation for type inference profiling

#include "julia.h"
#include "julia_internal.h"

jl_mutex_t typeinf_profiling_lock;

// == exported interface ==

extern "C" {

JL_DLLEXPORT jl_array_t* jl_typeinf_profiling_clear_and_fetch(
    jl_array_t *inference_profiling_results_array,
    jl_value_t *array_timing_type
)
{
    JL_LOCK(&typeinf_profiling_lock);

    size_t len = jl_array_len(inference_profiling_results_array);

    jl_array_t *out = jl_alloc_array_1d(array_timing_type, len);
    JL_GC_PUSH1(&out);

    memcpy(out->data, inference_profiling_results_array->data, len * sizeof(void*));

    jl_array_del_end(inference_profiling_results_array, len);

    JL_UNLOCK(&typeinf_profiling_lock);

    JL_GC_POP();
    return out;
}

JL_DLLEXPORT void jl_typeinf_profiling_push_timing(
    jl_array_t *inference_profiling_results_array,
    jl_value_t *timing
)
{
    JL_LOCK(&typeinf_profiling_lock);

    jl_array_ptr_1d_push(inference_profiling_results_array, timing);

    JL_UNLOCK(&typeinf_profiling_lock);
}

}  // extern "C"
