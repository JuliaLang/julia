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
    return jl_subtype(ty,(jl_value_t*)jl_complex_type,0) && jl_tparam0(ty) == (jl_value_t*)jl_float32_type;
}

inline bool is_complex128(jl_value_t *ty)
{
    return jl_subtype(ty,(jl_value_t*)jl_complex_type,0) && jl_tparam0(ty) == (jl_value_t*)jl_float64_type;
}

bool use_sret(AbiState *state,jl_value_t *ty)
{
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_bitstype(ty) ||  jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return false;
    if(is_complex64(ty))
        return false;
    return jl_is_structtype(ty);
}

void needPassByRef(AbiState *state,jl_value_t *ty, bool *byRef, bool *inReg, bool *byRefAttr)
{
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_bitstype(ty) ||  jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return;
    if(jl_is_structtype(ty) && !need_destructure_argument(ty))
        *byRef = true;
    *byRefAttr = *byRef;
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    if(!isret)
        return NULL;
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_bitstype(ty) ||  jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return NULL;
    // special case Complex{Float32} as a return type
    if(jl_subtype(ty,(jl_value_t*)jl_complex_type,0) && jl_tparam0(ty) == (jl_value_t*)jl_float32_type)
        return T_int64;
    return NULL;
}

bool need_private_copy(jl_value_t *ty, bool byRef)
{
    return false;
}
