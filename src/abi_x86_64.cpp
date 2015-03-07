//===-- abi_x86_64.cpp - x86_64 ABI description -----------------*- C++ -*-===//
//
//                         LDC – the LLVM D compiler
//
// This file is distributed under the BSD-style LDC license. See the LICENSE
// file for details.
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
    } else if (ty->ty == Tcomplex80) {
        accum.addField(offset, ComplexX87);
        // make sure other half knows about it too:
        accum.addField(offset+16, ComplexX87);
    } */
void classifyType(Classification& accum, jl_value_t* ty, uint64_t offset) {
    if (jl_is_cpointer_type(ty) || jl_is_array_type(ty)) {
        accum.addField(offset, Integer);
    } else if (jl_is_bitstype(ty) && jl_datatype_size(ty) == 16) {
        // Int128 or other 128bit wide INTEGER types
        accum.addField(offset, Integer);
        accum.addField(offset+8, Integer);
    }
    // Floating point types
    else if (ty == (jl_value_t*)jl_float64_type || ty == (jl_value_t*)jl_float32_type) {
        accum.addField(offset, Sse);
    }
    // Other integer types
    else if (jl_is_bitstype(ty))
    {
        if(jl_datatype_size(ty) > 8)
            jl_error("Bitstype of this size not supported in the C ABI");
        accum.addField(offset,Integer);
    } else if (jl_datatype_size(ty) > 16) {
        // This isn't creal, yet is > 16 bytes, so pass in memory.
        // Must be after creal case but before arrays and structs,
        // the other types that can get bigger than 16 bytes
        accum.addField(offset, Memory);
    } else if (jl_is_structtype(ty)) {
        for (int i = 0; i < jl_tuple_len(((jl_datatype_t*)ty)->types); ++i) {
            classifyType(accum, jl_tupleref(((jl_datatype_t*)ty)->types,i), offset + jl_field_offset(ty,i));
        }
    } else {
        jl_error("Unsupported type in C ABI");
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
    if(sret) {
        assert(state->int_regs>0 && "WTF? No int regs available?");
        state->int_regs--;
    }
    return sret;
}

void needPassByRef(AbiState *state,jl_value_t *ty, bool *byRef, bool *inReg, bool *byRefAttr)
{
    Classification cl = classify(ty);
    if (cl.isMemory) {
        *byRefAttr = *byRef = true;
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
    else if (jl_is_structtype(ty))
    {
        // spill to memory even though we would ordinarily pass
        // it in registers
        *byRef = true;
    }
    *byRefAttr = *byRef;
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    (void) isret;
    // no need to rewrite bitstypes or pointers (really only agregates are the problem)
    if (!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_bitstype(ty) ||  jl_is_cpointer_type(ty))
        return NULL;

    int size = jl_datatype_size(ty);
    if(!(size == 1 || size == 2 || size == 4 || size == 8))
        return NULL;

    Classification cl = classify(ty);
    if (cl.isMemory)
        return NULL;
    ArgClass c = Classification::merge(cl.classes[0],cl.classes[1]);
    Type *target_type = NULL;

    // Make into an aggregate of
    if (c == Sse)
        target_type = Type::getDoubleTy(jl_LLVMContext);
    else if (c == Integer)
        target_type = T_int64;
    else
        assert("Don't know how to rewrite type");

    return target_type;
}

bool need_private_copy(jl_value_t *ty, bool isRef)
{
    return false;
}
