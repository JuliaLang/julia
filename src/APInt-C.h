// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef APINT_C_H
#define APINT_C_H

#ifdef __cplusplus
extern "C" {
#endif
#include "dtypes.h"

#ifdef LLVM_VERSION_MAJOR
using llvm::integerPart;
#else
typedef void integerPart;
#endif

DLLEXPORT void LLVMNeg(unsigned numbits, integerPart *pa, integerPart *pr);
DLLEXPORT void LLVMByteSwap(unsigned numbits, integerPart *pa, integerPart *pr);

DLLEXPORT void LLVMAdd(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void LLVMSub(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void LLVMMul(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void LLVMSDiv(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void LLVMUDiv(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void LLVMSRem(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void LLVMURem(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);

DLLEXPORT void LLVMAnd(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void LLVMOr(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void LLVMXor(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void LLVMShl(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void LLVMLShr(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void LLVMAShr(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void LLVMFlipAllBits(unsigned numbits, integerPart *pa, integerPart *pr);

DLLEXPORT int LLVMICmpEQ(unsigned numbits, integerPart *pa, integerPart *pr);
DLLEXPORT int LLVMICmpNE(unsigned numbits, integerPart *pa, integerPart *pb);
DLLEXPORT int LLVMICmpSLT(unsigned numbits, integerPart *pa, integerPart *pb);
DLLEXPORT int LLVMICmpULT(unsigned numbits, integerPart *pa, integerPart *pb);
DLLEXPORT int LLVMICmpSLE(unsigned numbits, integerPart *pa, integerPart *pb);
DLLEXPORT int LLVMICmpULE(unsigned numbits, integerPart *pa, integerPart *pb);

DLLEXPORT int LLVMAdd_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT int LLVMAdd_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT int LLVMSub_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT int LLVMSub_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT int LLVMMul_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT int LLVMMul_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);

DLLEXPORT unsigned LLVMCountPopulation(unsigned numbits, integerPart *pa);
DLLEXPORT unsigned LLVMCountTrailingOnes(unsigned numbits, integerPart *pa);
DLLEXPORT unsigned LLVMCountTrailingZeros(unsigned numbits, integerPart *pa);
DLLEXPORT unsigned LLVMCountLeadingOnes(unsigned numbits, integerPart *pa);
DLLEXPORT unsigned LLVMCountLeadingZeros(unsigned numbits, integerPart *pa);

DLLEXPORT void LLVMFPtoSI(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
DLLEXPORT void LLVMFPtoUI(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
DLLEXPORT void LLVMSItoFP(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
DLLEXPORT void LLVMUItoFP(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
DLLEXPORT void LLVMSExt(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
DLLEXPORT void LLVMZExt(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
DLLEXPORT void LLVMTrunc(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);

DLLEXPORT int LLVMFPtoSI_exact(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
DLLEXPORT int LLVMFPtoUI_exact(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);

DLLEXPORT void jl_LLVMSMod(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
DLLEXPORT void jl_LLVMFlipSign(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);

DLLEXPORT unsigned countTrailingZeros_8(uint8_t Val);
DLLEXPORT unsigned countTrailingZeros_16(uint16_t Val);
DLLEXPORT unsigned countTrailingZeros_32(uint32_t Val);
DLLEXPORT unsigned countTrailingZeros_64(uint64_t Val);

//uint8_t getSwappedBytes_8(uint8_t Value); // no-op
//uint16_t getSwappedBytes_16(uint16_t Value);
//uint32_t getSwappedBytes_32(uint32_t Value);
//uint64_t getSwappedBytes_64(uint64_t Value);


#ifdef __cplusplus
}
#endif

#endif
