#ifndef JULIALIB_LIBSIMPLE_H
#define JULIALIB_LIBSIMPLE_H
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

struct CTree_Float64;
typedef struct CVector_CTree_Float64 {
    int32_t length;
    struct CTree_Float64* data;
} CVector_CTree_Float64;
typedef struct CTree_Float64 {
    CVector_CTree_Float64 children;
} CTree_Float64;
typedef struct MyTwoVec {
    int32_t x;
    int32_t y;
} MyTwoVec;
typedef struct CVector_Float32 {
    int32_t length;
    float* data;
} CVector_Float32;
typedef struct CVectorPair_Float32 {
    CVector_Float32 from;
    CVector_Float32 to;
} CVectorPair_Float32;

float copyto_and_sum(CVectorPair_Float32 fromto);
int64_t tree_size(CTree_Float64 tree);
int32_t countsame(MyTwoVec* list, int32_t n);
#endif // JULIALIB_LIBSIMPLE_H
