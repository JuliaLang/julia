#ifndef JULIALIB_LIBSIMPLE_H
#define JULIALIB_LIBSIMPLE_H
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct {
    int32_t length;
    float * data;
} CVector_float_;
typedef struct {
    CVector_float_ from;
    CVector_float_ to;
} CVectorPair_float_;
typedef struct {
    int32_t x;
    int32_t y;
} MyTwoVec;

int32_t countsame(MyTwoVec * list, int32_t n);
float copyto_and_sum(CVectorPair_float_ fromto);
#endif // JULIALIB_LIBSIMPLE_H
