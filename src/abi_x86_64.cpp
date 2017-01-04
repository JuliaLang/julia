//===-- abi_x86_64.cpp - x86_64 ABI description -----------------*- C++ -*-===//
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
// The ABI implementation used for 64 bit x86 (i.e. x86_64/AMD64/x64) targets.
//
//===----------------------------------------------------------------------===//

enum ArgClass { Integer, Sse, SseUp, X87, X87Up, ComplexX87, NoClass, Memory };

struct ABI_x86_64Layout : AbiLayout {

// used to track the state of the ABI generator during
// code generation
uint8_t int_regs, sse_regs;

ABI_x86_64Layout()
    : int_regs(6),
      sse_regs(8)
{
}

ABI_x86_64Layout(uint8_t int_regs, uint8_t sse_regs)
    : int_regs(int_regs),
      sse_regs(sse_regs)
{
}

struct Classification {
    bool isMemory;
    ArgClass classes[2];

    Classification() : isMemory(false)
    {
        classes[0] = NoClass;
        classes[1] = NoClass;
    }

    void addField(unsigned offset, ArgClass cl)
    {
        if (isMemory)
            return;

        // Note that we don't need to bother checking if it crosses 8 bytes.
        // We don't get here with unaligned fields, and anything that can be
        // big enough to cross 8 bytes (cdoubles, reals, structs and arrays)
        // is special-cased in classifyType()
        int idx = (offset < 8 ? 0 : 1);

        ArgClass nw = merge(classes[idx], cl);
        if (nw != classes[idx]) {
            classes[idx] = nw;

            if (nw == Memory) {
                classes[1-idx] = Memory;
                isMemory = true;
            }
        }
    }

    static ArgClass merge(ArgClass accum, ArgClass cl)
    {
        if (accum == cl)
            return accum;
        if (accum == NoClass)
            return cl;
        if (cl == NoClass)
            return accum;
        if (accum == Memory || cl == Memory)
            return Memory;
        if (accum == Integer || cl == Integer)
            return Integer;
        if (accum == X87 || accum == X87Up || accum == ComplexX87 ||
            cl == X87 || cl == X87Up || cl == ComplexX87)
            return Memory;
        return Sse;
    }
};

/*else if (ty == jl_float80_type) { //if this is ever added
        accum.addField(offset, X87);
        accum.addField(offset+8, X87Up);
    } else if (ty->ty == jl_complex80_type) {
        accum.addField(offset, ComplexX87);
        // make sure other half knows about it too:
        accum.addField(offset+16, ComplexX87);
    } */
void classifyType(Classification& accum, jl_datatype_t *dt, uint64_t offset) const
{
    // Floating point types
    if (dt == jl_float64_type || dt == jl_float32_type) {
        accum.addField(offset, Sse);
    }
    // Misc types
    else if (jl_is_cpointer_type((jl_value_t*)dt)) {
        accum.addField(offset, Integer); // passed as a pointer
    }
    // Ghost
    else if (jl_datatype_size(dt) == 0) {
    }
    // BitsTypes and not float, write as Integers
    else if (jl_is_bitstype(dt)) {
        if (jl_datatype_size(dt) <= 8) {
            accum.addField(offset, Integer);
        }
        else if (jl_datatype_size(dt) <= 16) {
            // Int128 or other 128bit wide INTEGER types
            accum.addField(offset, Integer);
            accum.addField(offset+8, Integer);
        }
        else {
            accum.addField(offset, Memory);
        }
    }
    // struct types that map to SIMD registers
    else if (is_native_simd_type(dt)) {
        accum.addField(offset, Sse);
    }
    // Other struct types
    else if (jl_datatype_size(dt) <= 16) {
        size_t i;
        for (i = 0; i < jl_datatype_nfields(dt); ++i) {
            jl_value_t *ty = jl_field_type(dt, i);
            if (!jl_is_datatype(ty) || ((jl_datatype_t*)ty)->layout == NULL || jl_is_array_type(ty))
                ty = (jl_value_t*)jl_voidpointer_type;
            classifyType(accum, (jl_datatype_t*)ty, offset + jl_field_offset(dt, i));
        }
    }
    else {
        accum.addField(offset, Memory);
    }
}

Classification classify(jl_datatype_t *dt) const
{
    Classification cl;
    classifyType(cl, dt, 0);
    return cl;
}

bool use_sret(jl_datatype_t *dt) override
{
    int sret = classify(dt).isMemory;
    if (sret) {
        assert(this->int_regs > 0 && "No int regs available when determining sret-ness?");
        this->int_regs--;
    }
    return sret;
}

bool needPassByRef(jl_datatype_t *dt, AttrBuilder &ab) override
{
    Classification cl = classify(dt);
    if (cl.isMemory) {
        ab.addAttribute(Attribute::ByVal);
        return true;
    }

    // Figure out how many registers we want for this arg:
    ABI_x86_64Layout wanted(0, 0);
    for (int i = 0 ; i < 2; i++) {
        if (cl.classes[i] == Integer)
            wanted.int_regs++;
        else if (cl.classes[i] == Sse)
            wanted.sse_regs++;
    }

    if (wanted.int_regs <= this->int_regs && wanted.sse_regs <= this->sse_regs) {
        this->int_regs -= wanted.int_regs;
        this->sse_regs -= wanted.sse_regs;
    }
    else if (jl_is_structtype(dt)) {
        // spill to memory even though we would ordinarily pass
        // it in registers
        ab.addAttribute(Attribute::ByVal);
        return true;
    }
    return false;
}

// Called on behalf of ccall to determine preferred LLVM representation
// for an argument or return value.
Type *preferred_llvm_type(jl_datatype_t *dt, bool isret) const override
{
    (void) isret;
    // no need to rewrite these types (they are returned as pointers anyways)
    if (is_native_simd_type(dt))
        return NULL;

    size_t size = jl_datatype_size(dt);
    size_t nbits = jl_datatype_nbits(dt);
    if (size > 16 || size == 0)
        return NULL;

    Classification cl = classify(dt);
    if (cl.isMemory)
        return NULL;

    Type *types[2];
    switch (cl.classes[0]) {
        case Integer:
            if (size >= 8)
                types[0] = T_int64;
            else
                types[0] = Type::getIntNTy(jl_LLVMContext, nbits);
            break;
        case Sse:
            if (size <= 4)
                types[0] = T_float32;
            else
                types[0] = T_float64;
            break;
        default:
            assert(0 && "Unexpected cl.classes[0]");
    }
    switch (cl.classes[1]) {
        case NoClass:
            return types[0];
        case Integer:
            assert(size > 8);
            types[1] = Type::getIntNTy(jl_LLVMContext, (nbits-64));
            return StructType::get(jl_LLVMContext,ArrayRef<Type*>(&types[0],2));
        case Sse:
            if (size <= 12)
                types[1] = T_float32;
            else
                types[1] = T_float64;
            return StructType::get(jl_LLVMContext,ArrayRef<Type*>(&types[0],2));
        default:
            assert(0 && "Unexpected cl.classes[0]");
    }
    // Silence GCC
    assert(0);
    return NULL;
}

};
