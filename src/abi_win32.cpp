//===-- abi_win32.cpp - x86 ABI description ---------------------*- C++ -*-===//
//
//                         LDC – the LLVM D compiler
//
// This file is distributed under the BSD-style LDC license:
//
// Copyright (c) 2007-2012 LDC Team.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
//     * Redistributions of source code must retain the above copyright notice,
//       this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright notice,
//       this list of conditions and the following disclaimer in the documentation
//       and/or other materials provided with the distribution.
//     * Neither the name of the LDC Team nor the names of its contributors may be
//       used to endorse or promote products derived from this software without
//       specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
// ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
    // Assume jl_is_datatype(ty) && !jl_is_abstracttype(ty)
    jl_datatype_t *dt = (jl_datatype_t*)ty;
    // Use sret if the size of the argument is not one of 1, 2, 4, 8 bytes
    // This covers the special case of Complex64
    size_t size = dt->size;
    if (size == 1 || size == 2 || size == 4 || size == 8)
        return false;
    return true;
}

void needPassByRef(AbiState *state, jl_value_t *ty, bool *byRef, bool *inReg)
{
    // Assume jl_is_datatype(ty) && !jl_is_abstracttype(ty)
    jl_datatype_t *dt = (jl_datatype_t*)ty;
    // Use pass by reference for all structs
    *byRef = dt->nfields > 0;
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    // Arguments are either scalar or passed by value
    if (!isret || !jl_is_datatype(ty) || jl_is_abstracttype(ty))
        return NULL;
    jl_datatype_t *dt = (jl_datatype_t*)ty;
    // rewrite integer sized (non-sret) struct to the corresponding integer
    if (!dt->nfields)
        return NULL;
    return Type::getIntNTy(jl_LLVMContext, dt->size * 8);
}

bool need_private_copy(jl_value_t *ty, bool byRef)
{
    return false;
}
