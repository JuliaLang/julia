// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef ABI_X86_VEC_H
#define ABI_X86_VEC_H

// Determine if object of bitstype ty maps to a __m128, __m256, or __m512 type in C.
static bool is_native_simd_type(jl_datatype_t *dt) {
    size_t size = jl_datatype_size(dt);
    if (size != 16 && size != 32 && size != 64)
        // Wrong size for xmm, ymm, or zmm register.
        return false;
    uint32_t n = jl_datatype_nfields(dt);
    if (n<2)
        // Not mapped to SIMD register.
        return false;
    jl_value_t *ft0 = jl_field_type(dt, 0);
    for (uint32_t i = 1; i < n; ++i)
        if (jl_field_type(dt, i) != ft0)
            // Not homogeneous
            return false;
    // Type is homogeneous.  Check if it maps to LLVM vector.
    return jl_special_vector_alignment(n, ft0) != 0;
}

#endif
