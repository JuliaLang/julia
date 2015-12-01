// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef BITVECTOR_H
#define BITVECTOR_H

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT u_int32_t *bitvector_new(u_int64_t n, int initzero);
JL_DLLEXPORT
u_int32_t *bitvector_resize(u_int32_t *b, uint64_t oldsz, uint64_t newsz,
                            int initzero);
size_t bitvector_nwords(u_int64_t nbits);
JL_DLLEXPORT void bitvector_set(u_int32_t *b, u_int64_t n, u_int32_t c);
JL_DLLEXPORT u_int32_t bitvector_get(u_int32_t *b, u_int64_t n);

JL_DLLEXPORT uint64_t bitvector_next(uint32_t *b, uint64_t n0, uint64_t n);

JL_DLLEXPORT
u_int64_t bitvector_count(u_int32_t *b, u_int64_t offs, u_int64_t nbits);
JL_DLLEXPORT
u_int32_t bitvector_any1(u_int32_t *b, u_int64_t offs, u_int64_t nbits);

#ifdef __cplusplus
}
#endif

#endif
