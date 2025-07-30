#ifndef SIMPLELIB_H
#define SIMPLELIB_H

#include <stdint.h>
#include <stddef.h>

struct _MyTwoVec_ {
    int32_t x;
    int32_t y;
};
typedef struct _MyTwoVec_ MyTwoVec;

struct _CVector_Float32_ {
    int32_t length;
    float *data;
};
typedef struct _CVector_Float32_ CVector_Float32;

struct _CVectorPair_Float32_ {
    CVector_Float32 from;
    CVector_Float32 to;
};
typedef struct _CVectorPair_Float32_ CVectorPair_Float32;

float copyto_and_sum(CVectorPair_Float32 fromto);
int32_t countsame(MyTwoVec *list, int32_t length);

#endif // SIMPLELIB_H
