//===-- abi_llvm.cpp - LLVM Target ABI description --------------*- C++ -*-===//
//
//                         LDC – the LLVM D compiler
//
// This file is distributed under the BSD-style LDC license. See the LICENSE
// file for details.
//
//===----------------------------------------------------------------------===//
//
// This ABI implementation does whatever LLVM decides is fine.
// It can be useful when first porting Julia to a new platform
// (until a platform-specific implementation can be developed).
//
//===----------------------------------------------------------------------===//


typedef bool AbiState;
AbiState default_abi_state = 0;

bool use_sret(AbiState *state,jl_value_t *ty)
{
    return false;
}

void needPassByRef(AbiState *state,jl_value_t *ty, bool *byRef, bool *inReg)
{
    return;
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    return NULL;
}

bool need_private_copy(jl_value_t *ty, bool byRef)
{
    return false;
}
