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

// whether the argument can be passed in register
static bool win64_reg_size(size_t size)
{
    return size <= 2 || size == 4 || size == 8;
}

struct ABI_Win64Layout : AbiLayout {
int nargs;
ABI_Win64Layout() : nargs(0) { }

bool use_sret(jl_datatype_t *dt) override
{
    size_t size = jl_datatype_size(dt);
    if (win64_reg_size(size) || is_native_simd_type(dt))
        return false;
    nargs++;
    return true;
}

bool needPassByRef(jl_datatype_t *dt, AttrBuilder &ab) override
{
    nargs++;
    size_t size = jl_datatype_size(dt);
    if (win64_reg_size(size))
        return false;
    if (nargs <= 4)
        ab.addAttribute(Attribute::ByVal);
    return true;
}

Type *preferred_llvm_type(jl_datatype_t *dt, bool isret) const override
{
    size_t size = jl_datatype_size(dt);
    if (size > 0 && win64_reg_size(size) && !jl_is_primitivetype(dt))
        return Type::getIntNTy(jl_LLVMContext, jl_datatype_nbits(dt));
    return NULL;
}

};
