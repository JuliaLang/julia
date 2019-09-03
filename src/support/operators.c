// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <limits.h>
#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include "dtypes.h"
#include "utils.h"

#ifdef __cplusplus
extern "C" {
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
    case T_INT64: {
        int64_t l = (int64_t)jl_load_ptraligned_i64(data);
        d = (double)l;
        if (d > 0 && l < 0)  // can happen!
            d = -d;
        break;
    }
    case T_UINT64: d = (double)jl_load_ptraligned_i64(data); break;
    case T_FLOAT:  d = (double)*(float*)data; break;
    case T_DOUBLE: return jl_load_ptraligned_f64(data);
    }
    return d;
}

#define CONV_TO_INTTYPE(type)                                                  \
type##_t conv_to_##type(void *data, numerictype_t tag)                         \
{                                                                              \
    type##_t i=0;                                                              \
    switch (tag) {                                                             \
    case T_INT8:   i = (type##_t)*(int8_t*)data; break;                        \
    case T_UINT8:  i = (type##_t)*(uint8_t*)data; break;                       \
    case T_INT16:  i = (type##_t)*(int16_t*)(data); break;                     \
    case T_UINT16: i = (type##_t)*(uint16_t*)(data); break;                    \
    case T_INT32:  i = (type##_t)*(int32_t*)(data); break;                     \
    case T_UINT32: i = (type##_t)*(uint32_t*)(data); break;                    \
    case T_INT64:  i = (type##_t)(int64_t)jl_load_ptraligned_i64(data); break; \
    case T_UINT64: i = (type##_t)jl_load_ptraligned_i64(data); break;          \
    case T_FLOAT:  i = (type##_t)*(float*)data; break;                         \
    case T_DOUBLE: i = (type##_t)jl_load_ptraligned_f64(data); break;          \
    }                                                                          \
    return i;                                                                  \
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
    case T_INT64:  i = (uint64_t)jl_load_ptraligned_i64(data); break;
    case T_UINT64: i = (uint64_t)jl_load_ptraligned_i64(data); break;
    case T_FLOAT:
        if (*(float*)data >= 0)
            i = (uint64_t)*(float*)data;
        else
            i = (uint64_t)(int64_t)*(float*)data;
        break;
    case T_DOUBLE:
        if (jl_load_ptraligned_f64(data) >= 0)
            i = (uint64_t)jl_load_ptraligned_f64(data);
        else
            i = (uint64_t)(int64_t)jl_load_ptraligned_f64(data);
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
    case T_INT64:  return (int64_t)jl_load_ptraligned_i64(a) < (int64_t)jl_load_ptraligned_i64(b);
    case T_UINT64: return jl_load_ptraligned_i64(a) < jl_load_ptraligned_i64(b);
    case T_FLOAT:  return *(float*)a < *(float*)b;
    case T_DOUBLE: return jl_load_ptraligned_f64(a) < jl_load_ptraligned_f64(b);
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
    case T_INT64:  return jl_load_ptraligned_i64(a) == jl_load_ptraligned_i64(b);
    case T_UINT64: return jl_load_ptraligned_i64(a) == jl_load_ptraligned_i64(b);
    case T_FLOAT:  return *(float*)a == *(float*)b;
    case T_DOUBLE: return jl_load_ptraligned_f64(a) == jl_load_ptraligned_f64(b);
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
        if (btag == T_INT64) {
            if ((int64_t)jl_load_ptraligned_i64(b) >= 0) {
                return (jl_load_ptraligned_i64(a) < jl_load_ptraligned_i64(b));
            }
            return ((int64_t)jl_load_ptraligned_i64(a) <
                    (int64_t)jl_load_ptraligned_i64(b));
        }
        else if (btag == T_DOUBLE) {
            if (db != db) return 0;
            return (jl_load_ptraligned_i64(a) < (uint64_t)jl_load_ptraligned_f64(b));
        }
    }
    else if (atag == T_INT64) {
        if (btag == T_UINT64) {
            if ((int64_t)jl_load_ptraligned_i64(a) >= 0) {
                return (jl_load_ptraligned_i64(a) < jl_load_ptraligned_i64(b));
            }
            return ((int64_t)jl_load_ptraligned_i64(a) <
                    (int64_t)jl_load_ptraligned_i64(b));
        }
        else if (btag == T_DOUBLE) {
            if (db != db) return 0;
            return ((int64_t)jl_load_ptraligned_i64(a) < (int64_t)jl_load_ptraligned_f64(b));
        }
    }
    if (btag == T_UINT64) {
        if (atag == T_DOUBLE) {
            if (da != da) return 0;
            return (jl_load_ptraligned_i64(b) > (uint64_t)jl_load_ptraligned_f64(a));
        }
    }
    else if (btag == T_INT64) {
        if (atag == T_DOUBLE) {
            if (da != da) return 0;
            return ((int64_t)jl_load_ptraligned_i64(b) > (int64_t)jl_load_ptraligned_f64(a));
        }
    }
    return 0;
}

int cmp_eq(void *a, numerictype_t atag, void *b, numerictype_t btag,
           int equalnans)
{
    union { double d; int64_t i64; } u, v;
    if (atag==btag && (!equalnans || atag < T_FLOAT))
        return cmp_same_eq(a, b, atag);

    double da = conv_to_double(a, atag);
    double db = conv_to_double(b, btag);

    if ((int)atag >= T_FLOAT && (int)btag >= T_FLOAT) {
        if (equalnans) {
            u.d = da; v.d = db;
            return u.i64 == v.i64;
        }
        return (da == db);
    }

    if (da != db)
        return 0;

    if (atag == T_UINT64) {
        // this is safe because if a had been bigger than S64_MAX,
        // we would already have concluded that it's bigger than b.
        if (btag == T_INT64) {
            return (jl_load_ptraligned_i64(a) == jl_load_ptraligned_i64(b));
        }
        else if (btag == T_DOUBLE) {
            return (jl_load_ptraligned_i64(a) == (uint64_t)(int64_t)jl_load_ptraligned_f64(b));
        }
    }
    else if (atag == T_INT64) {
        if (btag == T_UINT64) {
            return (jl_load_ptraligned_i64(a) == jl_load_ptraligned_i64(b));
        }
        else if (btag == T_DOUBLE) {
            return ((int64_t)jl_load_ptraligned_i64(a) == (int64_t)jl_load_ptraligned_f64(b));
        }
    }
    else if (btag == T_UINT64) {
        assert(atag != T_INT64); // Taken care of above
        if (atag == T_DOUBLE) {
            return (jl_load_ptraligned_i64(b) == (uint64_t)(int64_t)jl_load_ptraligned_f64(a));
        }
    }
    else if (btag == T_INT64) {
        assert(atag != T_UINT64); // Taken care of above
        if (atag == T_DOUBLE) {
            return ((int64_t)jl_load_ptraligned_i64(b) == (int64_t)jl_load_ptraligned_f64(a));
        }
    }
    return 1;
}

#ifdef __cplusplus
}
#endif
