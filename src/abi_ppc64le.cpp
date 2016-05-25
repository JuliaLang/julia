//===-- abi_ppc64le.cpp - Power v2 ABI description ---------------------*- C++ -*-===//
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
// The ABI implementation used for 64 bit little-endian PowerPC targets.
//
// The PowerOpen 64bit ELF v2 ABI can be found here:
// https://members.openpowerfoundation.org/document/dl/576
//===----------------------------------------------------------------------===//


typedef bool AbiState;
AbiState default_abi_state = 0;

// count the homogeneous floating agregate size (saturating at max count of 8)
static unsigned isHFA(jl_datatype_t *ty, jl_datatype_t **ty0)
{
    size_t i, l = ty->nfields;
    if (l == 0) {
        if (*ty0 == NULL) {
            if (ty == jl_float64_type || ty == jl_float32_type)
                *ty0 = ty;
        }
        return ty == *ty0 ? 1 : 9;
    }
    int n = 0;
    for (i = 0; i < l; i++) {
        jl_value_t *fld = jl_field_type(ty, i);
        if (!jl_is_datatype(fld))
            return 9;
        n += isHFA((jl_datatype_t*)fld, ty0);
        if (n > 8)
            return 9;
    }
    return n;
}

bool use_sret(AbiState *state, jl_value_t *ty)
{
    // Assume jl_is_datatype(ty) && !jl_is_abstracttype(ty)
    jl_datatype_t *dt = (jl_datatype_t*)ty;
    jl_datatype_t *ty0 = NULL;
    if (dt->size > 16 && isHFA(dt, &ty0) > 8)
        return true;
    return false;
}

void needPassByRef(AbiState *state, jl_value_t *ty, bool *byRef, bool *inReg)
{
    if (!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return;
    size_t size = jl_datatype_size(ty);
    if (size > 64)
        *byRef = true;
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    // Arguments are either scalar or passed by value
    if (!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return NULL;
    jl_datatype_t *dt = (jl_datatype_t*)ty;
    size_t size = dt->size;
    // don't need to change bitstypes
    if (!dt->nfields)
        return NULL;
    // legalize this into [n x f32/f64]
    jl_datatype_t *ty0 = NULL;
    int hfa = isHFA(dt, &ty0);
    if (hfa <= 8)
        return ArrayType::get(ty0 == jl_float32_type ? T_float32 : T_float64, hfa);
    // rewrite integer-sized (non-HFA) struct to an array of i64
    if (size > 8)
        return ArrayType::get(T_int64, (size + 7) / 8);
    return Type::getIntNTy(jl_LLVMContext, size * 8);
}

bool need_private_copy(jl_value_t *ty, bool byRef)
{
    return false;
}
