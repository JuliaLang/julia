//===-- abi_win64.cpp - Windows x86_64 ABI description ----------*- C++ -*-===//
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
// The ABI implementation used for 64 bit x86 (i.e. x86_64/AMD64/x64) targets
// on Windows.
//
//===----------------------------------------------------------------------===//


struct AbiState {
};

const AbiState default_abi_state = {};


bool use_sret(AbiState *state, jl_value_t *ty)
{
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return false;
    size_t size = jl_datatype_size(ty);
    if (size <= 8)
        return false;
    return true;
}

void needPassByRef(AbiState *state, jl_value_t *ty, bool *byRef, bool *inReg)
{
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return;
    size_t size = jl_datatype_size(ty);
    if (size > 8)
        *byRef = true;
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    if (!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return NULL;
    size_t size = jl_datatype_size(ty);
    if (size > 0 && size <= 8 && !jl_is_bitstype(ty))
        return Type::getIntNTy(getGlobalContext(), size*8);
    return NULL;
}

// Windows needs all types pased byRef to be passed in caller allocated memory
bool need_private_copy(jl_value_t *ty, bool byRef)
{
    return byRef;
}
