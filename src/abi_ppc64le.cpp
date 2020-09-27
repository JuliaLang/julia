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


struct ABI_PPC64leLayout : AbiLayout {

// count the homogeneous floating aggregate size (saturating at max count of 8)
unsigned isHFA(jl_datatype_t *ty, jl_datatype_t **ty0, bool *hva) const
{
    size_t i, l = ty->layout->nfields;
    // handle homogeneous float aggregates
    if (l == 0) {
        if (ty != jl_float64_type && ty != jl_float32_type)
            return 9;
        *hva = false;
        if (*ty0 == NULL)
            *ty0 = ty;
        else if (*hva || ty->size != (*ty0)->size)
            return 9;
        return 1;
    }

    // handle homogeneous vector aggregates
    jl_datatype_t *fld0 = (jl_datatype_t*)jl_field_type(ty, 0);
    if (!jl_is_datatype(fld0) || ty->name == jl_vecelement_typename)
        return 9;
    if (fld0->name == jl_vecelement_typename) {
        if (!jl_is_primitivetype(jl_tparam0(fld0)) || jl_datatype_size(ty) > 16)
            return 9;
        if (l != 1 && l != 2 && l != 4 && l != 8 && l != 16)
            return 9;
        *hva = true;
        if (*ty0 == NULL)
            *ty0 = ty;
        else if (!*hva || ty->size != (*ty0)->size)
            return 9;
        for (i = 1; i < l; i++) {
            jl_datatype_t *fld = (jl_datatype_t*)jl_field_type(ty, i);
            if (fld != fld0)
                return 9;
        }
        return 1;
    }

    // recurse through other struct types
    int n = 0;
    for (i = 0; i < l; i++) {
        jl_datatype_t *fld = (jl_datatype_t*)jl_field_type(ty, i);
        if (!jl_is_datatype(fld) || ((jl_datatype_t*)fld)->layout == NULL)
            return 9;
        n += isHFA((jl_datatype_t*)fld, ty0, hva);
        if (n > 8)
            return 9;
    }
    return n;
}

bool use_sret(jl_datatype_t *dt) override
{
    jl_datatype_t *ty0 = NULL;
    bool hva = false;
    if (jl_datatype_size(dt) > 16 && isHFA(dt, &ty0, &hva) > 8)
        return true;
    return false;
}

bool needPassByRef(jl_datatype_t *dt, AttrBuilder &ab) override
{
    jl_datatype_t *ty0 = NULL;
    bool hva = false;
    if (jl_datatype_size(dt) > 64 && isHFA(dt, &ty0, &hva) > 8) {
        ab.addAttribute(Attribute::ByVal);
        return true;
    }
    return false;
}

Type *preferred_llvm_type(jl_datatype_t *dt, bool isret) const override
{
    // Arguments are either scalar or passed by value
    size_t size = jl_datatype_size(dt);
    // don't need to change bitstypes
    if (!jl_datatype_nfields(dt))
        return NULL;
    // legalize this into [n x f32/f64]
    jl_datatype_t *ty0 = NULL;
    bool hva = false;
    int hfa = isHFA(dt, &ty0, &hva);
    if (hfa <= 8) {
        if (ty0 == jl_float32_type) {
            return ArrayType::get(T_float32, hfa);
        }
        else if (ty0 == jl_float64_type) {
            return ArrayType::get(T_float64, hfa);
        }
        else {
            jl_datatype_t *vecty = (jl_datatype_t*)jl_field_type(ty0, 0);
            assert(jl_is_datatype(vecty) && vecty->name == jl_vecelement_typename);
            Type *ety = bitstype_to_llvm(jl_tparam0(vecty));
            Type *vty = VectorType::get(ety, jl_datatype_nfields(ty0));
            return ArrayType::get(vty, hfa);
        }
    }
    // rewrite integer-sized (non-HFA) struct to an array
    // the bitsize of the integer gives the desired alignment
    if (size > 8) {
        if (jl_datatype_align(dt) <= 8) {
            return ArrayType::get(T_int64, (size + 7) / 8);
        }
        else {
            Type *T_int128 = Type::getIntNTy(jl_LLVMContext, 128);
            return ArrayType::get(T_int128, (size + 15) / 16);
        }
    }
    return Type::getIntNTy(jl_LLVMContext, size * 8);
}

};
