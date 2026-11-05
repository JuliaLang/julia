// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_BITVECTOR_H
#define JL_BITVECTOR_H

#include "dtypes.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT uint32_t *bitvector_new(uint64_t n, int initzero) JL_NOTSAFEPOINT;
JL_DLLEXPORT
uint32_t *bitvector_resize(uint32_t *b, uint64_t oldsz, uint64_t newsz,
                           int initzero) JL_NOTSAFEPOINT;
size_t bitvector_nwords(uint64_t nbits) JL_NOTSAFEPOINT;
JL_DLLEXPORT void bitvector_set(uint32_t *b, uint64_t n, uint32_t c) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint32_t bitvector_get(uint32_t *b, uint64_t n) JL_NOTSAFEPOINT;

#ifdef __cplusplus
}
#endif

#endif
