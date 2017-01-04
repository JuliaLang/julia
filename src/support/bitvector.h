// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef BITVECTOR_H
#define BITVECTOR_H

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT uint32_t *bitvector_new(uint64_t n, int initzero);
JL_DLLEXPORT
uint32_t *bitvector_resize(uint32_t *b, uint64_t oldsz, uint64_t newsz,
                           int initzero);
size_t bitvector_nwords(uint64_t nbits);
JL_DLLEXPORT void bitvector_set(uint32_t *b, uint64_t n, uint32_t c);
JL_DLLEXPORT uint32_t bitvector_get(uint32_t *b, uint64_t n);

JL_DLLEXPORT uint64_t bitvector_next(uint32_t *b, uint64_t n0, uint64_t n);

JL_DLLEXPORT
uint64_t bitvector_count(uint32_t *b, uint64_t offs, uint64_t nbits);
JL_DLLEXPORT
uint32_t bitvector_any1(uint32_t *b, uint64_t offs, uint64_t nbits);

#ifdef __cplusplus
}
#endif

#endif
