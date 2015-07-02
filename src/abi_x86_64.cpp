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


// used to track the state of the ABI generator during
// code generation
struct AbiState {
    unsigned char int_regs, sse_regs;
};

const AbiState default_abi_state = {6,8};

enum ArgClass { Integer, Sse, SseUp, X87, X87Up, ComplexX87, NoClass, Memory };

struct Classification {
    bool isMemory;
    ArgClass classes[2];

    Classification() : isMemory(false) {
        classes[0] = NoClass;
        classes[1] = NoClass;
    }

    void addField(unsigned offset, ArgClass cl) {
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

    static ArgClass merge(ArgClass accum, ArgClass cl) {
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
void classifyType(Classification& accum, jl_value_t* ty, uint64_t offset) {
    // Floating point types
    if (ty == (jl_value_t*)jl_float64_type || ty == (jl_value_t*)jl_float32_type) {
        accum.addField(offset, Sse);
    }
    // Misc types
    else if (!jl_is_datatype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty) || jl_is_abstracttype(ty)) {
        accum.addField(offset, Integer); // passed as a pointer
    }
    // Ghost
    else if (jl_datatype_size(ty) == 0) {
    }
    // BitsTypes and not float, write as Integers
    else if (jl_is_bitstype(ty)) {
        if (jl_datatype_size(ty) <= 8) {
            accum.addField(offset,Integer);
        }
        else if (jl_datatype_size(ty) <= 16) {
            // Int128 or other 128bit wide INTEGER types
            accum.addField(offset, Integer);
            accum.addField(offset+8, Integer);
        }
        else {
            accum.addField(offset, Memory);
        }
    }
    // Other struct types
    else if (jl_datatype_size(ty) <= 16) {
        size_t i;
        for (i = 0; i < jl_datatype_nfields(ty); ++i) {
            classifyType(accum, jl_field_type(ty,i), offset + jl_field_offset(ty,i));
        }
    }
    else {
        accum.addField(offset, Memory);
    }
}

Classification classify(jl_value_t* ty) {
    Classification cl;
    classifyType(cl, ty, 0);
    return cl;
}

bool use_sret(AbiState *state,jl_value_t *ty)
{
    int sret = classify(ty).isMemory;
    if (sret) {
        assert(state->int_regs>0 && "No int regs available when determining sret-ness?");
        state->int_regs--;
    }
    return sret;
}

void needPassByRef(AbiState *state, jl_value_t *ty, bool *byRef, bool *inReg)
{
    Classification cl = classify(ty);
    if (cl.isMemory) {
        *byRef = true;
        return;
    }

    // Figure out how many registers we want for this arg:
    AbiState wanted = { 0, 0 };
    for (int i = 0 ; i < 2; i++) {
        if (cl.classes[i] == Integer)
            wanted.int_regs++;
        else if (cl.classes[i] == Sse)
            wanted.sse_regs++;
    }

    if (wanted.int_regs <= state->int_regs && wanted.sse_regs <= state->sse_regs) {
        state->int_regs -= wanted.int_regs;
        state->sse_regs -= wanted.sse_regs;
        *inReg = true;
    }
    else if (jl_is_structtype(ty)) {
        // spill to memory even though we would ordinarily pass
        // it in registers
        *byRef = true;
    }
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    (void) isret;
    // no need to rewrite these types (they are returned as pointers anyways)
    if (!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return NULL;

    int size = jl_datatype_size(ty);
    if (size > 16 || size == 0)
        return NULL;

    Classification cl = classify(ty);
    if (cl.isMemory)
        return NULL;

    Type *types[2];
    switch (cl.classes[0]) {
        case Integer:
            if (size >= 8)
                types[0] = T_int64;
            else
                types[0] = Type::getIntNTy(getGlobalContext(), size*8);
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
            types[1] = Type::getIntNTy(getGlobalContext(), (size-8)*8);
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

bool need_private_copy(jl_value_t *ty, bool isRef)
{
    return false;
}
