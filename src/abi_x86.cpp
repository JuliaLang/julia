//===-- abi_x86.cpp - x86 ABI description -----------------------*- C++ -*-===//
//
//                         LDC – the LLVM D compiler
//
// This file is distributed under the BSD-style LDC license. See the LICENSE
// file for details.
//
//===----------------------------------------------------------------------===//
//
// The ABI implementation used for 32 bit x86 targets.
//
//===----------------------------------------------------------------------===//


typedef bool AbiState;
AbiState default_abi_state = 0;

inline bool is_complex64(jl_value_t *ty)
{
    return jl_complex_type != NULL && jl_is_datatype(ty) &&
        ((jl_datatype_t*)ty)->name == jl_complex_type->name &&
        jl_tparam0(ty) == (jl_value_t*)jl_float32_type;
}

inline bool is_complex128(jl_value_t *ty)
{
    return jl_complex_type != NULL && jl_is_datatype(ty) &&
        ((jl_datatype_t*)ty)->name == jl_complex_type->name &&
        jl_tparam0(ty) == (jl_value_t*)jl_float64_type;
}

bool use_sret(AbiState *state, jl_value_t *ty)
{
    if (!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return false;
    int size = jl_datatype_size(ty);
    if (size == 0)
        return false;
    if (is_complex64(ty) || (jl_is_bitstype(ty) && size <= 8))
        return false;
    return true;
}

void needPassByRef(AbiState *state, jl_value_t *ty, bool *byRef, bool *inReg, bool *byRefAttr)
{
    if (!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return;
    int size = jl_datatype_size(ty);
    if (is_complex64(ty) || is_complex128(ty) || (jl_is_bitstype(ty) && size <= 8))
        return;
    *byRefAttr = *byRef = true;
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    if (!isret)
        return NULL;
    if (!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return NULL;
    // special case Complex{Float32} as a return type
    if (is_complex64(ty))
        return T_int64;
    return NULL;
}

bool need_private_copy(jl_value_t *ty, bool byRef)
{
    return false;
}
