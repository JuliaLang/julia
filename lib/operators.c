#include <limits.h>
#include <assert.h>
#include "dtypes.h"
#include "utils.h"
#include "ieee754.h"

// given a number, determine an appropriate type for storing it
#if 0
numerictype_t effective_numerictype(double r)
{
    double fp;

    fp = fpart(r);
    if (fp != 0 || r > U64_MAX || r < S64_MIN) {
        if (r > FLT_MAX || r < -FLT_MAX || (fabs(r) < FLT_MIN)) {
            return T_DOUBLE;
        }
        else {
            return T_FLOAT;
        }
    }
    else if (r >= SCHAR_MIN && r <= SCHAR_MAX) {
        return T_INT8;
    }
    else if (r >= SHRT_MIN && r <= SHRT_MAX) {
        return T_INT16;
    }
    else if (r >= INT_MIN && r <= INT_MAX) {
        return T_INT32;
    }
    else if (r <= S64_MAX) {
        return T_INT64;
    }
    return T_UINT64;
}
#else
// simpler version implementing a smaller preferred type repertoire
numerictype_t effective_numerictype(double r)
{
    double fp;

    fp = fpart(r);
    if (fp != 0 || r > U64_MAX || r < S64_MIN) {
        return T_DOUBLE;
    }
    else if (r >= INT_MIN && r <= INT_MAX) {
        return T_INT32;
    }
    else if (r <= S64_MAX) {
        return T_INT64;
    }
    return T_UINT64;
}
#endif

double conv_to_double(void *data, numerictype_t tag)
{
    double d=0;
    switch (tag) {
    case T_INT8:   d = (double)*(int8_t*)data; break;
    case T_UINT8:  d = (double)*(uint8_t*)data; break;
    case T_INT16:  d = (double)*(int16_t*)data; break;
    case T_UINT16: d = (double)*(uint16_t*)data; break;
    case T_INT32:  d = (double)*(int32_t*)data; break;
    case T_UINT32: d = (double)*(uint32_t*)data; break;
    case T_INT64:
        d = (double)*(int64_t*)data;
        if (d > 0 && *(int64_t*)data < 0)  // can happen!
            d = -d;
        break;
    case T_UINT64: d = (double)*(uint64_t*)data; break;
    case T_FLOAT:  d = (double)*(float*)data; break;
    case T_DOUBLE: return *(double*)data;
    }
    return d;
}

void conv_from_double(void *dest, double d, numerictype_t tag)
{
    switch (tag) {
    case T_INT8:   *(int8_t*)dest = d; break;
    case T_UINT8:  *(uint8_t*)dest = d; break;
    case T_INT16:  *(int16_t*)dest = d; break;
    case T_UINT16: *(uint16_t*)dest = d; break;
    case T_INT32:  *(int32_t*)dest = d; break;
    case T_UINT32: *(uint32_t*)dest = d; break;
    case T_INT64:
        *(int64_t*)dest = d;
        if (d > 0 && *(int64_t*)dest < 0)  // 0x8000000000000000 is a bitch
            *(int64_t*)dest = S64_MAX;
        break;
    case T_UINT64: *(uint64_t*)dest = (int64_t)d; break;
    case T_FLOAT:  *(float*)dest = d; break;
    case T_DOUBLE: *(double*)dest = d; break;
    }
}

#define CONV_TO_INTTYPE(type)                               \
type##_t conv_to_##type(void *data, numerictype_t tag)      \
{                                                           \
    type##_t i=0;                                           \
    switch (tag) {                                          \
    case T_INT8:   i = (type##_t)*(int8_t*)data; break;     \
    case T_UINT8:  i = (type##_t)*(uint8_t*)data; break;    \
    case T_INT16:  i = (type##_t)*(int16_t*)data; break;    \
    case T_UINT16: i = (type##_t)*(uint16_t*)data; break;   \
    case T_INT32:  i = (type##_t)*(int32_t*)data; break;    \
    case T_UINT32: i = (type##_t)*(uint32_t*)data; break;   \
    case T_INT64:  i = (type##_t)*(int64_t*)data; break;    \
    case T_UINT64: i = (type##_t)*(uint64_t*)data; break;   \
    case T_FLOAT:  i = (type##_t)*(float*)data; break;      \
    case T_DOUBLE: i = (type##_t)*(double*)data; break;     \
    }                                                       \
    return i;                                               \
}

CONV_TO_INTTYPE(int64)
CONV_TO_INTTYPE(int32)
CONV_TO_INTTYPE(uint32)

// this is needed to work around a possible compiler bug
// casting negative floats and doubles to uint64. you need
// to cast to int64 first.
uint64_t conv_to_uint64(void *data, numerictype_t tag)
{
    uint64_t i=0;
    switch (tag) {
    case T_INT8:   i = (uint64_t)*(int8_t*)data; break;
    case T_UINT8:  i = (uint64_t)*(uint8_t*)data; break;
    case T_INT16:  i = (uint64_t)*(int16_t*)data; break;
    case T_UINT16: i = (uint64_t)*(uint16_t*)data; break;
    case T_INT32:  i = (uint64_t)*(int32_t*)data; break;
    case T_UINT32: i = (uint64_t)*(uint32_t*)data; break;
    case T_INT64:  i = (uint64_t)*(int64_t*)data; break;
    case T_UINT64: i = (uint64_t)*(uint64_t*)data; break;
    case T_FLOAT:
        if (*(float*)data >= 0)
            i = (uint64_t)*(float*)data;
        else
            i = (uint64_t)(int64_t)*(float*)data;
        break;
    case T_DOUBLE:
        if (*(double*)data >= 0)
            i = (uint64_t)*(double*)data;
        else
            i = (uint64_t)(int64_t)*(double*)data;
        break;
    }
    return i;
}

int cmp_same_lt(void *a, void *b, numerictype_t tag)
{
    switch (tag) {
    case T_INT8:   return *(int8_t*)a < *(int8_t*)b;
    case T_UINT8:  return *(uint8_t*)a < *(uint8_t*)b;
    case T_INT16:  return *(int16_t*)a < *(int16_t*)b;
    case T_UINT16: return *(uint16_t*)a < *(uint16_t*)b;
    case T_INT32:  return *(int32_t*)a < *(int32_t*)b;
    case T_UINT32: return *(uint32_t*)a < *(uint32_t*)b;
    case T_INT64:  return *(int64_t*)a < *(int64_t*)b;
    case T_UINT64: return *(uint64_t*)a < *(uint64_t*)b;
    case T_FLOAT:  return *(float*)a < *(float*)b;
    case T_DOUBLE: return *(double*)a < *(double*)b;
    }
    return 0;
}

int cmp_same_eq(void *a, void *b, numerictype_t tag)
{
    switch (tag) {
    case T_INT8:   return *(int8_t*)a == *(int8_t*)b;
    case T_UINT8:  return *(uint8_t*)a == *(uint8_t*)b;
    case T_INT16:  return *(int16_t*)a == *(int16_t*)b;
    case T_UINT16: return *(uint16_t*)a == *(uint16_t*)b;
    case T_INT32:  return *(int32_t*)a == *(int32_t*)b;
    case T_UINT32: return *(uint32_t*)a == *(uint32_t*)b;
    case T_INT64:  return *(int64_t*)a == *(int64_t*)b;
    case T_UINT64: return *(uint64_t*)a == *(uint64_t*)b;
    case T_FLOAT:  return *(float*)a == *(float*)b;
    case T_DOUBLE: return *(double*)a == *(double*)b;
    }
    return 0;
}

int cmp_lt(void *a, numerictype_t atag, void *b, numerictype_t btag)
{
    if (atag==btag)
        return cmp_same_lt(a, b, atag);

    double da = conv_to_double(a, atag);
    double db = conv_to_double(b, btag);

    // casting to double will only get the wrong answer for big int64s
    // that differ in low bits
    if (da < db)
        return 1;
    if (db < da)
        return 0;

    if (atag == T_UINT64) {
        // this is safe because if a had been bigger than S64_MAX,
        // we would already have concluded that it's bigger than b.
        if (btag == T_INT64) {
            return ((int64_t)*(uint64_t*)a < *(int64_t*)b);
        }
        else if (btag == T_DOUBLE) {
            return (*(uint64_t*)a < (uint64_t)*(double*)b);
        }
    }
    else if (atag == T_INT64) {
        if (btag == T_UINT64) {
            return (*(int64_t*)a < (int64_t)*(uint64_t*)b);
        }
        else if (btag == T_DOUBLE) {
            return (*(int64_t*)a < (int64_t)*(double*)b);
        }
    }
    else if (btag == T_UINT64) {
        if (atag == T_INT64) {
            return ((int64_t)*(uint64_t*)b > *(int64_t*)a);
        }
        else if (atag == T_DOUBLE) {
            return (*(uint64_t*)b > (uint64_t)*(double*)a);
        }
    }
    else if (btag == T_INT64) {
        if (atag == T_UINT64) {
            return (*(int64_t*)b > (int64_t)*(uint64_t*)a);
        }
        else if (atag == T_DOUBLE) {
            return (*(int64_t*)b > (int64_t)*(double*)a);
        }
    }
    return 0;
}

int cmp_eq(void *a, numerictype_t atag, void *b, numerictype_t btag,
           int equalnans)
{
    if (atag==btag && (!equalnans || atag < T_FLOAT))
        return cmp_same_eq(a, b, atag);

    double da = conv_to_double(a, atag);
    double db = conv_to_double(b, btag);

    if ((int)atag >= T_FLOAT && (int)btag >= T_FLOAT) {
        if (equalnans) {
            return *(uint64_t*)&da == *(uint64_t*)&db;
        }
        return (da == db);
    }

    if (da != db)
        return 0;

    if (atag == T_UINT64) {
        // this is safe because if a had been bigger than S64_MAX,
        // we would already have concluded that it's bigger than b.
        if (btag == T_INT64) {
            return ((int64_t)*(uint64_t*)a == *(int64_t*)b);
        }
        else if (btag == T_DOUBLE) {
            return (*(uint64_t*)a == (uint64_t)(int64_t)*(double*)b);
        }
    }
    else if (atag == T_INT64) {
        if (btag == T_UINT64) {
            return (*(int64_t*)a == (int64_t)*(uint64_t*)b);
        }
        else if (btag == T_DOUBLE) {
            return (*(int64_t*)a == (int64_t)*(double*)b);
        }
    }
    else if (btag == T_UINT64) {
        if (atag == T_INT64) {
            return ((int64_t)*(uint64_t*)b == *(int64_t*)a);
        }
        else if (atag == T_DOUBLE) {
            return (*(uint64_t*)b == (uint64_t)(int64_t)*(double*)a);
        }
    }
    else if (btag == T_INT64) {
        if (atag == T_UINT64) {
            return (*(int64_t*)b == (int64_t)*(uint64_t*)a);
        }
        else if (atag == T_DOUBLE) {
            return (*(int64_t*)b == (int64_t)*(double*)a);
        }
    }
    return 1;
}

#ifdef ENABLE_LLT_TEST
void test_operators()
{
    int8_t i8, i8b;
    uint8_t ui8, ui8b;
    int16_t i16, i16b;
    uint16_t ui16, ui16b;
    int32_t i32, i32b;
    uint32_t ui32, ui32b;
    int64_t i64, i64b;
    uint64_t ui64, ui64b;
    float f, fb;
    double d, db;

    ui64 = U64_MAX;
    ui64b = U64_MAX-1;
    i64 = S64_MIN;
    i64b = i64+1;
    d = (double)ui64;
    db = (double)i64b;

    assert(cmp_lt(&i64, T_INT64, &ui64, T_UINT64));
    assert(!cmp_lt(&ui64, T_UINT64, &i64, T_INT64));
    assert(cmp_lt(&i64, T_INT64, &ui64b, T_UINT64));
    assert(!cmp_lt(&ui64b, T_UINT64, &i64, T_INT64));
    assert(cmp_lt(&i64, T_INT64, &i64b, T_INT64));
    assert(!cmp_lt(&i64b, T_INT64, &i64, T_INT64));

    // try to compare a double too big to fit in an int64 with an
    // int64 requiring too much precision to fit in a double...
    // this case fails but it's very difficult/expensive to support
    //assert(cmp_lt(&ui64b, T_UINT64, &d, T_DOUBLE));

    i64 = S64_MAX;
    ui64 = S64_MAX-1;
    assert(cmp_lt(&ui64, T_UINT64, &i64, T_INT64));
    assert(!cmp_lt(&i64, T_INT64, &ui64, T_UINT64));
    i64 = S64_MAX-1;
    ui64 = S64_MAX;
    assert(cmp_lt(&i64, T_INT64, &ui64, T_UINT64));
    assert(!cmp_lt(&ui64, T_UINT64, &i64, T_INT64));

    d = DBL_MAXINT;
    i64 = DBL_MAXINT+100;
    assert(cmp_lt(&d, T_DOUBLE, &i64, T_INT64));
    assert(!cmp_lt(&i64, T_INT64, &d, T_DOUBLE));
    i64 = DBL_MAXINT+10;
    assert(cmp_lt(&d, T_DOUBLE, &i64, T_INT64));
    assert(!cmp_lt(&i64, T_INT64, &d, T_DOUBLE));
    i64 = DBL_MAXINT+1;
    assert(cmp_lt(&d, T_DOUBLE, &i64, T_INT64));
    assert(!cmp_lt(&i64, T_INT64, &d, T_DOUBLE));

    assert(!cmp_eq(&d, T_DOUBLE, &i64, T_INT64, 0));
    i64 = DBL_MAXINT;
    assert(cmp_eq(&d, T_DOUBLE, &i64, T_INT64, 0));
}
#endif
