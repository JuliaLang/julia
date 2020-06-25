// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APFloat.h>
#include <llvm/Support/MathExtras.h>

#include "APInt-C.h"
#include "julia.h"
#include "julia_assert.h"

using namespace llvm;

inline uint64_t RoundUpToAlignment(uint64_t Value, uint64_t Align, uint64_t Skew = 0) {
    return alignTo(Value, Align, Skew);
}

const unsigned int integerPartWidth = llvm::APInt::APINT_BITS_PER_WORD;
const unsigned int host_char_bit = 8;

/* create "APInt s" from "integerPart *ps" */
#define CREATE(s) \
    APInt s; \
    if ((numbits % integerPartWidth) != 0) { \
        /* use LLT_ALIGN to round the memory area up to the nearest integerPart-sized chunk */ \
        unsigned nbytes = RoundUpToAlignment(numbits, integerPartWidth) / host_char_bit; \
        integerPart *data_a64 = (integerPart*)alloca(nbytes); \
        /* TODO: this memcpy assumes little-endian,
         * for big-endian, need to align the copy to the other end */ \
        memcpy(data_a64, p##s, RoundUpToAlignment(numbits, host_char_bit) / host_char_bit); \
        s = APInt(numbits, makeArrayRef(data_a64, nbytes / sizeof(integerPart))); \
    } \
    else { \
        s = APInt(numbits, makeArrayRef(p##s, numbits / integerPartWidth)); \
    }

/* assign to "integerPart *pr" from "APInt a" */
#define ASSIGN(r, a) \
    if (numbits <= 8) \
        *(uint8_t*)p##r = a.getZExtValue(); \
    else if (numbits <= 16) \
        *(uint16_t*)p##r = a.getZExtValue(); \
    else if (numbits <= 32) \
        *(uint32_t*)p##r = a.getZExtValue(); \
    else if (numbits <= 64) \
        *(uint64_t*)p##r = a.getZExtValue(); \
    else \
        memcpy(p##r, a.getRawData(), RoundUpToAlignment(numbits, host_char_bit) / host_char_bit); \

extern "C" JL_DLLEXPORT
void LLVMNeg(unsigned numbits, integerPart *pa, integerPart *pr) {
    APInt z(numbits, 0);
    CREATE(a)
    z -= a;
    ASSIGN(r, z)
}

extern "C" JL_DLLEXPORT
void LLVMAdd(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    a += b;
    ASSIGN(r, a)
}

extern "C" JL_DLLEXPORT
void LLVMSub(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    a -= b;
    ASSIGN(r, a)
}

extern "C" JL_DLLEXPORT
void LLVMMul(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    a *= b;
    ASSIGN(r, a)
}

extern "C" JL_DLLEXPORT
void LLVMSDiv(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    if (LLVMDiv_sov(numbits, pa, pb, pr))
        jl_throw(jl_diverror_exception);
}

extern "C" JL_DLLEXPORT
void LLVMUDiv(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    if (LLVMDiv_uov(numbits, pa, pb, pr))
        jl_throw(jl_diverror_exception);
}

extern "C" JL_DLLEXPORT
void LLVMSRem(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    if (LLVMRem_sov(numbits, pa, pb, pr))
        jl_throw(jl_diverror_exception);
}

extern "C" JL_DLLEXPORT
void LLVMURem(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    if (LLVMRem_uov(numbits, pa, pb, pr))
        jl_throw(jl_diverror_exception);
}

extern "C" JL_DLLEXPORT
int LLVMICmpEQ(unsigned numbits, integerPart *pa, integerPart *pb) {
    CREATE(a)
    CREATE(b)
    return a.eq(b);
}

extern "C" JL_DLLEXPORT
int LLVMICmpNE(unsigned numbits, integerPart *pa, integerPart *pb) {
    CREATE(a)
    CREATE(b)
    return a.ne(b);
}

extern "C" JL_DLLEXPORT
int LLVMICmpSLT(unsigned numbits, integerPart *pa, integerPart *pb) {
    CREATE(a)
    CREATE(b)
    return a.slt(b);                  
}

extern "C" JL_DLLEXPORT
int LLVMICmpULT(unsigned numbits, integerPart *pa, integerPart *pb) {
    CREATE(a)
    CREATE(b)
    return a.ult(b);
}

extern "C" JL_DLLEXPORT
int LLVMICmpSLE(unsigned numbits, integerPart *pa, integerPart *pb) {
    CREATE(a)
    CREATE(b)
    return a.sle(b);
}

extern "C" JL_DLLEXPORT
int LLVMICmpULE(unsigned numbits, integerPart *pa, integerPart *pb) {
    CREATE(a)
    CREATE(b)
    return a.ule(b);
}

extern "C" JL_DLLEXPORT
void LLVMAnd(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    a &= b;
    ASSIGN(r, a)
}

extern "C" JL_DLLEXPORT
void LLVMOr(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    a |= b;
    ASSIGN(r, a)
}

extern "C" JL_DLLEXPORT
void LLVMXor(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    a ^= b;
    ASSIGN(r, a)
}

extern "C" JL_DLLEXPORT
void LLVMShl(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    a = a.shl(b);
    ASSIGN(r, a)
}

extern "C" JL_DLLEXPORT
void LLVMLShr(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    a = a.lshr(b);
    ASSIGN(r, a)
}
extern "C" JL_DLLEXPORT
void LLVMAShr(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    a = a.ashr(b);
    ASSIGN(r, a)
}

extern "C" JL_DLLEXPORT
void LLVMFlipAllBits(unsigned numbits, integerPart *pa, integerPart *pr) {
    CREATE(a)
    a.flipAllBits();
    ASSIGN(r, a)
}

extern "C" JL_DLLEXPORT
int LLVMAdd_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    bool Overflow;
    a = a.uadd_ov(b, Overflow);
    ASSIGN(r, a)
    return Overflow;
}

extern "C" JL_DLLEXPORT
int LLVMAdd_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    bool Overflow;
    a = a.sadd_ov(b, Overflow);
    ASSIGN(r, a)
    return Overflow;
}

extern "C" JL_DLLEXPORT
int LLVMSub_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    bool Overflow;
    a = a.usub_ov(b, Overflow);
    ASSIGN(r, a)
    return Overflow;
}

extern "C" JL_DLLEXPORT
int LLVMSub_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    bool Overflow;
    a = a.ssub_ov(b, Overflow);
    ASSIGN(r, a)
    return Overflow;
}

extern "C" JL_DLLEXPORT
int LLVMMul_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    bool Overflow;
    a = a.smul_ov(b, Overflow);
    ASSIGN(r, a)
    return Overflow;
}

extern "C" JL_DLLEXPORT
int LLVMMul_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    bool Overflow;
    a = a.umul_ov(b, Overflow);
    ASSIGN(r, a)
    return Overflow;
}

extern "C" JL_DLLEXPORT
int LLVMDiv_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    if (!b)
        return true;
    bool Overflow;
    a = a.sdiv_ov(b, Overflow);
    ASSIGN(r, a)
    return Overflow;
}

extern "C" JL_DLLEXPORT
int LLVMDiv_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    if (!b)
        return true;
    a = a.udiv(b);
    ASSIGN(r, a)
    return false;
}

extern "C" JL_DLLEXPORT
int LLVMRem_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    if (!b)
        return true;
    a = a.srem(b);
    ASSIGN(r, a)
    return false;
}

extern "C" JL_DLLEXPORT
int LLVMRem_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    CREATE(a)
    CREATE(b)
    if (!b)
        return true;
    a = a.urem(b);
    ASSIGN(r, a)
    return false;
}

extern "C" JL_DLLEXPORT
void LLVMByteSwap(unsigned numbits, integerPart *pa, integerPart *pr) {
    CREATE(a)
    a = a.byteSwap();
    ASSIGN(r, a)
}

void LLVMFPtoInt(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr, bool isSigned, bool *isExact) {
    double Val;
    if (numbits == 32)
        Val = *(float*)pa;
    else if (numbits == 64)
        Val = *(double*)pa;
    else
        jl_error("FPtoSI: runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64");
    unsigned onumbytes = RoundUpToAlignment(onumbits, host_char_bit) / host_char_bit;
    if (onumbits <= 64) { // fast-path, if possible
        if (isSigned) {
            int64_t ia = Val;
            memcpy(pr, &ia, onumbytes); // TODO: assumes little-endian
            if (isExact) {
                // check whether the conversion was lossless
                int64_t ia2 = ia < 0 ? -1 : 0;
                memcpy(&ia2, pr, onumbytes);
                *isExact = (Val == (double)ia2 && ia == ia2);
            }
        }
        else {
            uint64_t ia = Val;
            memcpy(pr, &ia, onumbytes); // TODO: assumes little-endian
            if (isExact) {
                // check whether the conversion was lossless
                uint64_t ia2 = 0;
                memcpy(&ia2, pr, onumbytes);
                *isExact = (Val == (double)ia2 && ia == ia2);
            }
        }
    }
    else {
        APFloat a(Val);
        bool isVeryExact;
        APFloat::roundingMode rounding_mode = APFloat::rmNearestTiesToEven;
        unsigned nbytes = RoundUpToAlignment(onumbits, integerPartWidth) / host_char_bit;
        integerPart *parts = (integerPart*)alloca(nbytes);
        APFloat::opStatus status = a.convertToInteger(MutableArrayRef<integerPart>(parts, nbytes), onumbits, isSigned, rounding_mode, &isVeryExact);
        memcpy(pr, parts, onumbytes);
        if (isExact)
            *isExact = (status == APFloat::opOK);
    }
}

extern "C" JL_DLLEXPORT
void LLVMFPtoSI(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr) {
    LLVMFPtoInt(numbits, pa, onumbits, pr, true, NULL);
}

extern "C" JL_DLLEXPORT
void LLVMFPtoUI(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr) {
    LLVMFPtoInt(numbits, pa, onumbits, pr, false, NULL);
}

extern "C" JL_DLLEXPORT
int LLVMFPtoSI_exact(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr) {
    bool isExact;
    LLVMFPtoInt(numbits, pa, onumbits, pr, true, &isExact);
    return isExact;
}

extern "C" JL_DLLEXPORT
int LLVMFPtoUI_exact(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr) {
    bool isExact;
    LLVMFPtoInt(numbits, pa, onumbits, pr, false, &isExact);
    return isExact;
}

extern "C" JL_DLLEXPORT
void LLVMSItoFP(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr) {
    double val;
    { // end scope before jl_error call
        CREATE(a)
        val = a.roundToDouble(true);
    }
    if (onumbits == 32)
        *(float*)pr = val;
    else if (onumbits == 64)
        *(double*)pr = val;
    else
        jl_error("SItoFP: runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64");
}

extern "C" JL_DLLEXPORT
void LLVMUItoFP(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr) {
    double val;
    { // end scope before jl_error call
        CREATE(a)
        val = a.roundToDouble(false);
    }
    if (onumbits == 32)
        *(float*)pr = val;
    else if (onumbits == 64)
        *(double*)pr = val;
    else
        jl_error("UItoFP: runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64");
}

extern "C" JL_DLLEXPORT
void LLVMSExt(unsigned inumbits, integerPart *pa, unsigned onumbits, integerPart *pr) {
    if (!(onumbits > inumbits))
        jl_error("SExt: output bitsize must be > input bitsize");
    unsigned inumbytes = RoundUpToAlignment(inumbits, host_char_bit) / host_char_bit;
    unsigned onumbytes = RoundUpToAlignment(onumbits, host_char_bit) / host_char_bit;
    int bits = (0 - inumbits) % host_char_bit;
    int signbit = (inumbits - 1) % host_char_bit;
    int sign = ((unsigned char*)pa)[inumbytes - 1] & (1 << signbit) ? -1 : 0;
    // copy over the input bytes
    memcpy(pr, pa, inumbytes);
    if (bits) {
        // sign-extend the partial byte
        ((signed char*)pr)[inumbytes - 1] = ((signed char*)pa)[inumbytes - 1] << bits >> bits;
    }
    // sign-extend the rest of the bytes
    memset((char*)pr + inumbytes, sign, onumbytes - inumbytes);
}

extern "C" JL_DLLEXPORT
void LLVMZExt(unsigned inumbits, integerPart *pa, unsigned onumbits, integerPart *pr) {
    if (!(onumbits > inumbits))
        jl_error("ZExt: output bitsize must be > input bitsize");
    unsigned inumbytes = RoundUpToAlignment(inumbits, host_char_bit) / host_char_bit;
    unsigned onumbytes = RoundUpToAlignment(onumbits, host_char_bit) / host_char_bit;
    int bits = (0 - inumbits) % host_char_bit;
    // copy over the input bytes
    memcpy(pr, pa, inumbytes);
    if (bits) {
        // zero the remaining bits of the partial byte
        ((unsigned char*)pr)[inumbytes - 1] = ((unsigned char*)pa)[inumbytes - 1] << bits >> bits;
    }
    // zero-extend the rest of the bytes
    memset((char*)pr + inumbytes, 0, onumbytes - inumbytes);
}

extern "C" JL_DLLEXPORT
void LLVMTrunc(unsigned inumbits, integerPart *pa, unsigned onumbits, integerPart *pr) {
    if (!(onumbits < inumbits))
        jl_error("Trunc: output bitsize must be < input bitsize");
    unsigned onumbytes = RoundUpToAlignment(onumbits, host_char_bit) / host_char_bit;
    memcpy(pr, pa, onumbytes);
}

extern "C" JL_DLLEXPORT
unsigned countTrailingZeros_8(uint8_t Val) {
    return countTrailingZeros(Val);
}

extern "C" JL_DLLEXPORT
unsigned countTrailingZeros_16(uint16_t Val) {
    return countTrailingZeros(Val);
}

extern "C" JL_DLLEXPORT
unsigned countTrailingZeros_32(uint32_t Val) {
    return countTrailingZeros(Val);
}

extern "C" JL_DLLEXPORT
unsigned countTrailingZeros_64(uint64_t Val) {
    return countTrailingZeros(Val);
}

extern "C" JL_DLLEXPORT
void jl_LLVMSMod(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    { // end scope before jl_error call
        CREATE(a)
        CREATE(b)
        if (!!b) {
            APInt r = a.srem(b);
            if (a.isNegative() != b.isNegative()) {
                r = (b + r).srem(b);
            }
            ASSIGN(r, r)
            return;
        }
    }
    jl_throw(jl_diverror_exception);
}

extern "C" JL_DLLEXPORT
void jl_LLVMFlipSign(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr) {
    unsigned numbytes = (numbits + host_char_bit - 1) / host_char_bit;
    int signbit = (numbits - 1) % host_char_bit;
    int sign = ((unsigned char*)pb)[numbytes - 1] & (1 << signbit);
    if (sign)
        LLVMNeg(numbits, pa, pr);
    else
        memcpy(pr, pa, numbytes);
}

extern "C" JL_DLLEXPORT
unsigned LLVMCountPopulation(unsigned numbits, integerPart *pa) {
    CREATE(a)
    return a.countPopulation();
}

extern "C" JL_DLLEXPORT
unsigned LLVMCountTrailingOnes(unsigned numbits, integerPart *pa) {
    CREATE(a)
    return a.countTrailingOnes();
}

extern "C" JL_DLLEXPORT
unsigned LLVMCountTrailingZeros(unsigned numbits, integerPart *pa) {
    CREATE(a)
    return a.countTrailingZeros();
}

extern "C" JL_DLLEXPORT
unsigned LLVMCountLeadingOnes(unsigned numbits, integerPart *pa) {
    CREATE(a)
    return a.countLeadingOnes();
}

extern "C" JL_DLLEXPORT
unsigned LLVMCountLeadingZeros(unsigned numbits, integerPart *pa) {
    CREATE(a)
    return a.countLeadingZeros();
}
