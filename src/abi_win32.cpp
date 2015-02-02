//===-- abi_win32.cpp - x86 ABI description ---------------------*- C++ -*-===//
//
//                         LDC – the LLVM D compiler
//
// This file is distributed under the BSD-style LDC license. See the LICENSE
// file for details.
//
//===----------------------------------------------------------------------===//
//
// The ABI implementation used for 32 bit x86 targets on Windows.
//
//===----------------------------------------------------------------------===//


typedef bool AbiState;
AbiState default_abi_state = 0;

bool use_sret(AbiState *state, jl_value_t *ty)
{
    if (!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return false;
    size_t size = jl_datatype_size(ty);
    if (size <= 8)
        return false;
    return true;
}

void needPassByRef(AbiState *state, jl_value_t *ty, bool *byRef, bool *inReg, bool *byRefAttr)
{
    if (!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return;
    size_t size = jl_datatype_size(ty);
    if (size <= 8)
        return;
    *byRefAttr = *byRef = true;
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    if (!isret)
        return NULL;
    if (!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return NULL;
    size_t size = jl_datatype_size(ty);
    if (size > 0 && size <= 8 && !jl_is_bitstype(ty))
        return Type::getIntNTy(getGlobalContext(), size*8);
    return NULL;
}

bool need_private_copy(jl_value_t *ty, bool byRef)
{
    return false;
}
