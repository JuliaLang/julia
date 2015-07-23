
#ifndef APINT_C_H
#define APINT_C_H

#ifdef __cplusplus
extern "C" {
#endif
typedef void integerPart;

void LLVMNeg(unsigned numbits, integerPart *pa, integerPart *pr);
void LLVMByteSwap(unsigned numbits, integerPart *pa, integerPart *pr);

void LLVMAdd(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void LLVMSub(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void LLVMMul(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void LLVMSDiv(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void LLVMUDiv(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void LLVMSRem(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void LLVMURem(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);

void LLVMAnd(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void LLVMOr(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void LLVMXor(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void LLVMShl(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void LLVMLShr(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void LLVMAShr(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void LLVMFlipAllBits(unsigned numbits, integerPart *pa, integerPart *pr);

int LLVMICmpEQ(unsigned numbits, integerPart *pa, integerPart *pr);
int LLVMICmpNE(unsigned numbits, integerPart *pa, integerPart *pb);
int LLVMICmpSLT(unsigned numbits, integerPart *pa, integerPart *pb);
int LLVMICmpULT(unsigned numbits, integerPart *pa, integerPart *pb);
int LLVMICmpSLE(unsigned numbits, integerPart *pa, integerPart *pb);
int LLVMICmpULE(unsigned numbits, integerPart *pa, integerPart *pb);

int LLVMAdd_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
int LLVMAdd_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
int LLVMSub_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
int LLVMSub_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
int LLVMMul_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
int LLVMMul_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);

unsigned LLVMCountPopulation(unsigned numbits, integerPart *pa);
unsigned LLVMCountTrailingOnes(unsigned numbits, integerPart *pa);
unsigned LLVMCountTrailingZeros(unsigned numbits, integerPart *pa);
unsigned LLVMCountLeadingOnes(unsigned numbits, integerPart *pa);
unsigned LLVMCountLeadingZeros(unsigned numbits, integerPart *pa);

void LLVMFPtoSI(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
void LLVMFPtoUI(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
void LLVMSItoFP(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
void LLVMUItoFP(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
void LLVMSExt(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
void LLVMZExt(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
void LLVMTrunc(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);

int LLVMFPtoSI_exact(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);
int LLVMFPtoUI_exact(unsigned numbits, integerPart *pa, unsigned onumbits, integerPart *pr);

void jl_LLVMSMod(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
void jl_LLVMFlipSign(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);

unsigned countTrailingZeros_8(uint8_t Val);
unsigned countTrailingZeros_16(uint16_t Val);
unsigned countTrailingZeros_32(uint32_t Val);
unsigned countTrailingZeros_64(uint64_t Val);

uint8_t getSwappedBytes_8(uint8_t Value); // no-op
uint16_t getSwappedBytes_16(uint16_t Value);
uint32_t getSwappedBytes_32(uint32_t Value);
uint64_t getSwappedBytes_64(uint64_t Value);


#ifdef __cplusplus
}
#endif

#endif
