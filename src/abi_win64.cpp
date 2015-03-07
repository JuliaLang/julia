//===-- abi_win64.cpp - Windows x86_64 ABI description ----------*- C++ -*-===//
//
//                         LDC – the LLVM D compiler
//
// This file is distributed under the BSD-style LDC license. See the LICENSE
// file for details.
//
//===----------------------------------------------------------------------===//
//
// The ABI implementation used for 64 bit x86 (i.e. x86_64/AMD64/x64) targets
// on Windows.
//
//===----------------------------------------------------------------------===//


// Windows only uses the first four registers of either
struct AbiState {
    unsigned char int_regs, sse_regs;
};

const AbiState default_abi_state = {4,4};


bool use_sret(AbiState *state,jl_value_t *ty)
{
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return false;
    size_t size = jl_datatype_size(ty);
    bool sret =  !(size == 1 || size == 2 || size == 4 || size == 8); // || jl_is_sse(ty) if every implemented
    if(sret)
        state->int_regs--;
    return sret;
}

void needPassByRef(AbiState *state,jl_value_t *ty, bool *byRef, bool *inReg, bool *byRefAttr)
{
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return;
    if ((jl_datatype_t*)ty == jl_float32_type || (jl_datatype_t*)ty == jl_float64_type) {
        state->sse_regs--;
        return;
    }
    size_t size = jl_datatype_size(ty);
    *byRef = !(size == 1 || size == 2 || size == 4 || size == 8); // but not sse types
    *byRefAttr = *byRef;
    if(state->int_regs > 0) {
        state->int_regs--; //Windows passes these by pointer
        //*inReg = true;
    }
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return NULL;
    if(jl_is_bitstype(ty))
        return NULL;
    size_t size = jl_datatype_size(ty);
    if (size == 1 || size == 2 || size == 4 || size == 8)
        return T_int64;
    return NULL;
}

// Windows needs all types pased byRef to be passed in caller allocated memory
bool need_private_copy(jl_value_t *ty, bool byRef)
{
    return byRef;
}
