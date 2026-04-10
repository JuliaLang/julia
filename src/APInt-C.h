// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_APINT_C_H
#define JL_APINT_C_H

#include "julia.h"
#include "dtypes.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef void integerPart;

JL_DLLEXPORT void APInt_neg(unsigned numbits, integerPart *pa, integerPart *pr);
JL_DLLEXPORT void APInt_bswap(unsigned numbits, integerPart *pa, integerPart *pr);

JL_DLLEXPORT void APInt_add(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT void APInt_sub(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT void APInt_mul(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT void APInt_sdiv(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT void APInt_udiv(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT void APInt_srem(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT void APInt_urem(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);

JL_DLLEXPORT void APInt_and(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT void APInt_or(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT void APInt_xor(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT void APInt_shl(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT void APInt_lshr(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT void APInt_ashr(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT void APInt_not(unsigned numbits, integerPart *pa, integerPart *pr);

JL_DLLEXPORT int APInt_eq(unsigned numbits, integerPart *pa, integerPart *pr);
JL_DLLEXPORT int APInt_ne(unsigned numbits, integerPart *pa, integerPart *pb);
JL_DLLEXPORT int APInt_slt(unsigned numbits, integerPart *pa, integerPart *pb);
JL_DLLEXPORT int APInt_ult(unsigned numbits, integerPart *pa, integerPart *pb);
JL_DLLEXPORT int APInt_sle(unsigned numbits, integerPart *pa, integerPart *pb);
JL_DLLEXPORT int APInt_ule(unsigned numbits, integerPart *pa, integerPart *pb);

JL_DLLEXPORT int APInt_add_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT int APInt_add_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT int APInt_sub_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT int APInt_sub_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT int APInt_mul_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT int APInt_mul_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT int APInt_div_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT int APInt_div_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT int APInt_rem_sov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);
JL_DLLEXPORT int APInt_rem_uov(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);

JL_DLLEXPORT unsigned APInt_popcount(unsigned numbits, integerPart *pa);
JL_DLLEXPORT unsigned APInt_countr_one(unsigned numbits, integerPart *pa);
JL_DLLEXPORT unsigned APInt_countr_zero(unsigned numbits, integerPart *pa);
JL_DLLEXPORT unsigned APInt_countl_one(unsigned numbits, integerPart *pa);
JL_DLLEXPORT unsigned APInt_countl_zero(unsigned numbits, integerPart *pa);

JL_DLLEXPORT void APInt_fptosi(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr);
JL_DLLEXPORT void APInt_fptoui(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr);
JL_DLLEXPORT void APInt_sitofp(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr);
JL_DLLEXPORT void APInt_uitofp(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr);
JL_DLLEXPORT void APInt_sext(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr);
JL_DLLEXPORT void APInt_zext(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr);
JL_DLLEXPORT void APInt_trunc(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr);

JL_DLLEXPORT int APInt_fptosi_exact(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr);
JL_DLLEXPORT int APInt_fptoui_exact(jl_datatype_t *ty, integerPart *pa, jl_datatype_t *oty, integerPart *pr);

JL_DLLEXPORT void APInt_flipsign(unsigned numbits, integerPart *pa, integerPart *pb, integerPart *pr);

//uint8_t getSwappedBytes_8(uint8_t Value); // no-op
//uint16_t getSwappedBytes_16(uint16_t Value);
//uint32_t getSwappedBytes_32(uint32_t Value);
//uint64_t getSwappedBytes_64(uint64_t Value);


#ifdef __cplusplus
}
#endif

#endif
