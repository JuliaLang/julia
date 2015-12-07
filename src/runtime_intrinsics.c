// This file is a part of Julia. License is MIT: http://julialang.org/license

// This is in implementation of the Julia intrinsic functions against boxed types
// excluding the c interface (ccall, cglobal, llvmcall)
//
// this file assumes a little-endian processor, although that isn't too hard to fix
// it also assumes two's complement negative numbers, which might be a bit harder to fix
//
// TODO: add half-float support

#include "julia.h"
#include "julia_internal.h"
#include "APInt-C.h"
const unsigned int host_char_bit = 8;

// run time version of box/unbox intrinsic
JL_DLLEXPORT jl_value_t *jl_reinterpret(jl_value_t *ty, jl_value_t *v)
{
    JL_TYPECHK(reinterpret, datatype, ty);
    if (!jl_is_leaf_type(ty) || !jl_is_bitstype(ty))
        jl_error("reinterpret: target type not a leaf bitstype");
    if (!jl_is_bitstype(jl_typeof(v)))
        jl_error("reinterpret: value not a bitstype");
    if (jl_datatype_size(jl_typeof(v)) != jl_datatype_size(ty))
        jl_error("reinterpret: argument size does not match size of target type");
    if (ty == jl_typeof(v))
        return v;
    if (ty == (jl_value_t*)jl_bool_type)
        return *(uint8_t*)jl_data_ptr(v) & 1 ? jl_true : jl_false;
    return jl_new_bits(ty, jl_data_ptr(v));
}

// run time version of pointerref intrinsic (warning: i is not rooted)
JL_DLLEXPORT jl_value_t *jl_pointerref(jl_value_t *p, jl_value_t *i)
{
    JL_TYPECHK(pointerref, pointer, p);
    JL_TYPECHK(pointerref, long, i);
    jl_value_t *ety = jl_tparam0(jl_typeof(p));
    if (ety == (jl_value_t*)jl_any_type) {
        jl_value_t **pp = (jl_value_t**)(jl_unbox_long(p) + (jl_unbox_long(i)-1)*sizeof(void*));
        return *pp;
    }
    else {
        if (!jl_is_datatype(ety))
            jl_error("pointerref: invalid pointer");
        size_t nb = LLT_ALIGN(jl_datatype_size(ety), ((jl_datatype_t*)ety)->alignment);
        char *pp = (char*)jl_unbox_long(p) + (jl_unbox_long(i)-1)*nb;
        return jl_new_bits(ety, pp);
    }
}

// run time version of pointerset intrinsic (warning: x is not gc-rooted)
JL_DLLEXPORT jl_value_t *jl_pointerset(jl_value_t *p, jl_value_t *x, jl_value_t *i)
{
    JL_TYPECHK(pointerset, pointer, p);
    JL_TYPECHK(pointerset, long, i);
    jl_value_t *ety = jl_tparam0(jl_typeof(p));
    if (ety == (jl_value_t*)jl_any_type) {
        jl_value_t **pp = (jl_value_t**)(jl_unbox_long(p) + (jl_unbox_long(i)-1)*sizeof(void*));
        *pp = x;
    }
    else {
        if (!jl_is_datatype(ety))
            jl_error("pointerset: invalid pointer");
        size_t nb = LLT_ALIGN(jl_datatype_size(ety), ((jl_datatype_t*)ety)->alignment);
        char *pp = (char*)jl_unbox_long(p) + (jl_unbox_long(i)-1)*nb;
        if (jl_typeof(x) != ety)
            jl_error("pointerset: type mismatch in assign");
        jl_assign_bits(pp, x);
    }
    return p;
}


static inline unsigned int next_power_of_two(unsigned int val) {
  /* this function taken from libuv src/unix/core.c */
  val -= 1;
  val |= val >> 1;
  val |= val >> 2;
  val |= val >> 4;
  val |= val >> 8;
  val |= val >> 16;
  val += 1;
  return val;
}

static inline char signbitbyte(void *a, unsigned bytes) {
    // sign bit of an signed number of n bytes, as a byte
    return (((signed char*)a)[bytes - 1] < 0) ? ~0 : 0;
}

static inline char usignbitbyte(void *a, unsigned bytes) {
    // sign bit of an unsigned number
    return 0;
}

static inline unsigned select_by_size(unsigned sz)
{
    /* choose the right sized function specialization */
    switch (sz) {
    default: return 0;
    case  1: return 1;
    case  2: return 2;
    case  4: return 3;
    case  8: return 4;
    case 16: return 5;
    }
}

#define SELECTOR_FUNC(intrinsic) \
    typedef intrinsic##_t select_##intrinsic##_t[6]; \
    static inline intrinsic##_t select_##intrinsic(unsigned sz, select_##intrinsic##_t list) \
    { \
        intrinsic##_t thunk = list[select_by_size(sz)]; \
        if (!thunk) thunk = list[0]; \
        return thunk; \
    }

#define fp_select(a, func) \
    sizeof(a) == sizeof(float) ? func##f((float)a) : func(a)
#define fp_select2(a, b, func) \
    sizeof(a) == sizeof(float) ? func##f(a, b) : func(a, b)

// fast-function generators //

// integer input
// OP::Function macro(input)
// name::unique string
// nbits::number of bits
// c_type::c_type corresponding to nbits
#define un_iintrinsic_ctype(OP, name, nbits, c_type) \
static inline void jl_##name##nbits(unsigned runtime_nbits, void *pa, void *pr) \
{ \
    c_type a = *(c_type*)pa; \
    *(c_type*)pr = OP(a); \
}

// integer input, unsigned output
// OP::Function macro(input)
// name::unique string
// nbits::number of bits
// c_type::c_type corresponding to nbits
#define uu_iintrinsic_ctype(OP, name, nbits, c_type) \
static inline unsigned jl_##name##nbits(unsigned runtime_nbits, void *pa) \
{ \
    c_type a = *(c_type*)pa; \
    return OP(a); \
}

// floating point
// OP::Function macro(output pointer, input)
// name::unique string
// nbits::number of bits in the *input*
// c_type::c_type corresponding to nbits
#define un_fintrinsic_ctype(OP, name, c_type) \
static inline void name(unsigned osize, void *pa, void *pr) \
{ \
    c_type a = *(c_type*)pa; \
    OP((c_type*)pr, a); \
}

// float or integer inputs
// OP::Function macro(inputa, inputb)
// name::unique string
// nbits::number of bits
// c_type::c_type corresponding to nbits
#define bi_intrinsic_ctype(OP, name, nbits, c_type) \
static void jl_##name##nbits(unsigned runtime_nbits, void *pa, void *pb, void *pr) \
{ \
    c_type a = *(c_type*)pa; \
    c_type b = *(c_type*)pb; \
    *(c_type*)pr = (c_type)OP(a, b); \
}

// float or integer inputs, bool output
// OP::Function macro(inputa, inputb)
// name::unique string
// nbits::number of bits
// c_type::c_type corresponding to nbits
#define bool_intrinsic_ctype(OP, name, nbits, c_type) \
static int jl_##name##nbits(unsigned runtime_nbits, void *pa, void *pb) \
{ \
    c_type a = *(c_type*)pa; \
    c_type b = *(c_type*)pb; \
    return OP(a, b); \
}

// integer inputs, with precondition test
// OP::Function macro(inputa, inputb)
// name::unique string
// nbits::number of bits
// c_type::c_type corresponding to nbits
#define checked_intrinsic_ctype(CHECK_OP, OP, name, nbits, c_type) \
static int jl_##name##nbits(unsigned runtime_nbits, void *pa, void *pb, void *pr) \
{ \
    c_type a = *(c_type*)pa; \
    c_type b = *(c_type*)pb; \
    if (CHECK_OP(a, b)) \
        return 1; \
    *(c_type*)pr = (c_type)OP(a, b); \
    return 0; \
}

// float inputs
// OP::Function macro(inputa, inputb, inputc)
// name::unique string
// nbits::number of bits
// c_type::c_type corresponding to nbits
#define ter_intrinsic_ctype(OP, name, nbits, c_type) \
static void jl_##name##nbits(unsigned runtime_nbits, void *pa, void *pb, void *pc, void *pr) \
{ \
    c_type a = *(c_type*)pa; \
    c_type b = *(c_type*)pb; \
    c_type c = *(c_type*)pc; \
    *(c_type*)pr = (c_type)OP(a, b, c); \
}


// unary operator generator //

typedef void (*intrinsic_1_t)(unsigned, void*, void*);
SELECTOR_FUNC(intrinsic_1)
#define un_iintrinsic(name, u) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *a) \
{ \
    return jl_iintrinsic_1(jl_typeof(a), a, #name, u##signbitbyte, jl_intrinsiclambda_ty1, name##_list); \
}
#define un_iintrinsic_fast(LLVMOP, OP, name, u) \
un_iintrinsic_ctype(OP, name, 8, u##int##8_t) \
un_iintrinsic_ctype(OP, name, 16, u##int##16_t) \
un_iintrinsic_ctype(OP, name, 32, u##int##32_t) \
un_iintrinsic_ctype(OP, name, 64, u##int##64_t) \
static select_intrinsic_1_t name##_list = { \
    LLVMOP, \
    jl_##name##8, \
    jl_##name##16, \
    jl_##name##32, \
    jl_##name##64, \
}; \
un_iintrinsic(name, u)
#define un_iintrinsic_slow(LLVMOP, name, u) \
static select_intrinsic_1_t name##_list = { \
    LLVMOP \
}; \
un_iintrinsic(name, u)

typedef unsigned (*intrinsic_u1_t)(unsigned, void*);
SELECTOR_FUNC(intrinsic_u1)
#define uu_iintrinsic(name, u) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *a) \
{ \
    return jl_iintrinsic_1(jl_typeof(a), a, #name, u##signbitbyte, jl_intrinsiclambda_u1, name##_list); \
}
#define uu_iintrinsic_fast(LLVMOP, OP, name, u) \
uu_iintrinsic_ctype(OP, name, 8, u##int##8_t) \
uu_iintrinsic_ctype(OP, name, 16, u##int##16_t) \
uu_iintrinsic_ctype(OP, name, 32, u##int##32_t) \
uu_iintrinsic_ctype(OP, name, 64, u##int##64_t) \
static select_intrinsic_u1_t name##_list = { \
    LLVMOP, \
    jl_##name##8, \
    jl_##name##16, \
    jl_##name##32, \
    jl_##name##64, \
}; \
uu_iintrinsic(name, u)
#define uu_iintrinsic_slow(LLVMOP, name, u) \
static select_intrinsic_u1_t name##_list = { \
    LLVMOP \
}; \
uu_iintrinsic(name, u)

static inline jl_value_t *jl_iintrinsic_1(jl_value_t *ty, jl_value_t *a, const char *name, char (*getsign)(void*, unsigned),
        jl_value_t* (*lambda1)(jl_value_t*, void*, unsigned, unsigned, void*), void *list)
{
    if (!jl_is_bitstype(jl_typeof(a)))
        jl_errorf("%s: value is not a bitstype", name);
    if (!jl_is_bitstype(ty))
        jl_errorf("%s: type is not a bitstype", name);
    void *pa = jl_data_ptr(a);
    unsigned isize = jl_datatype_size(jl_typeof(a));
    unsigned isize2 = next_power_of_two(isize);
    unsigned osize = jl_datatype_size(ty);
    unsigned osize2 = next_power_of_two(osize);
    if (isize2 > osize2)
        osize2 = isize2;
    if (osize2 > isize || isize2 > isize) {
        /* if needed, round type up to a real c-type and set/clear the unused bits */
        void *pa2;
        pa2 = alloca(osize2);
        /* TODO: this memcpy assumes little-endian,
         * for big-endian, need to align the copy to the other end */ \
        memcpy(pa2, pa, isize);
        memset((char*)pa2 + isize, getsign(pa, isize), osize2 - isize);
        pa = pa2;
    }
    jl_value_t *newv = lambda1(ty, pa, osize, osize2, list);
    if (ty == (jl_value_t*)jl_bool_type)
        return *(uint8_t*)jl_data_ptr(newv) & 1 ? jl_true : jl_false;
    return newv;
}

static inline jl_value_t *jl_intrinsiclambda_ty1(jl_value_t *ty, void *pa, unsigned osize, unsigned osize2, void *voidlist)
{
    jl_value_t *newv = newstruct((jl_datatype_t*)ty);
    intrinsic_1_t op = select_intrinsic_1(osize2, (intrinsic_1_t*)voidlist);
    op(osize * host_char_bit, pa, jl_data_ptr(newv));
    return newv;
}

static inline jl_value_t *jl_intrinsiclambda_u1(jl_value_t *ty, void *pa, unsigned osize, unsigned osize2, void *voidlist)
{
    jl_value_t *newv = newstruct((jl_datatype_t*)ty);
    intrinsic_u1_t op = select_intrinsic_u1(osize2, (intrinsic_u1_t*)voidlist);
    unsigned cnt = op(osize * host_char_bit, pa);
    // TODO: the following memset/memcpy assumes little-endian
    // for big-endian, need to copy from the other end of cnt
    if (osize > sizeof(unsigned)) {
        // perform zext, if needed
        memset((char*)jl_data_ptr(newv) + sizeof(unsigned), 0, osize - sizeof(unsigned));
        osize = sizeof(unsigned);
    }
    memcpy(jl_data_ptr(newv), &cnt, osize);
    return newv;
}

// conversion operator

typedef void (*intrinsic_cvt_t)(unsigned, void*, unsigned, void*);
typedef unsigned (*intrinsic_cvt_check_t)(unsigned, unsigned, void*);
#define cvt_iintrinsic_checked(LLVMOP, check_op, name) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *ty, jl_value_t *a) \
{ \
    return jl_intrinsic_cvt(ty, a, #name, LLVMOP, check_op); \
}
#define cvt_iintrinsic(LLVMOP, name) \
    cvt_iintrinsic_checked(LLVMOP, NULL, name) \

static inline jl_value_t *jl_intrinsic_cvt(jl_value_t *ty, jl_value_t *a, const char *name, intrinsic_cvt_t op, intrinsic_cvt_check_t check_op)
{
    jl_value_t *aty = jl_typeof(a);
    if (!jl_is_bitstype(aty))
        jl_errorf("%s: value is not a bitstype", name);
    if (!jl_is_bitstype(ty))
        jl_errorf("%s: type is not a bitstype", name);
    void *pa = jl_data_ptr(a);
    unsigned isize = jl_datatype_size(aty);
    unsigned osize = jl_datatype_size(ty);
    if (check_op && check_op(isize, osize, pa))
        jl_throw(jl_inexact_exception);
    jl_value_t *newv = newstruct((jl_datatype_t*)ty);
    op(aty == (jl_value_t*)jl_bool_type ? 1 : isize * host_char_bit, pa,
            osize * host_char_bit, jl_data_ptr(newv));
    if (ty == (jl_value_t*)jl_bool_type)
        return *(uint8_t*)jl_data_ptr(newv) & 1 ? jl_true : jl_false;
    return newv;
}

// floating point

#define un_fintrinsic_withtype(OP, name) \
un_fintrinsic_ctype(OP, jl_##name##32, float) \
un_fintrinsic_ctype(OP, jl_##name##64, double) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *ty, jl_value_t *a) \
{ \
    return jl_fintrinsic_1(ty, a, #name, jl_##name##32, jl_##name##64); \
}

#define un_fintrinsic(OP, name) \
un_fintrinsic_withtype(OP, name##_withtype) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *a) \
{ \
    return jl_##name##_withtype(jl_typeof(a), a); \
}

typedef void (fintrinsic_op1)(unsigned, void*, void*);

static inline jl_value_t *jl_fintrinsic_1(jl_value_t *ty, jl_value_t *a, const char *name, fintrinsic_op1 *floatop, fintrinsic_op1 *doubleop)
{
    if (!jl_is_bitstype(jl_typeof(a)))
        jl_errorf("%s: value is not a bitstype", name);
    if (!jl_is_bitstype(ty))
        jl_errorf("%s: type is not a bitstype", name);
    jl_value_t *newv = newstruct((jl_datatype_t*)ty);
    void *pa = jl_data_ptr(a), *pr = jl_data_ptr(newv);
    unsigned sz = jl_datatype_size(jl_typeof(a));
    unsigned sz2 = jl_datatype_size(ty);
    switch (sz) {
    /* choose the right size c-type operation based on the input */
    case 4:
        floatop(sz2 * host_char_bit, pa, pr);
        break;
    case 8:
        doubleop(sz2 * host_char_bit, pa, pr);
        break;
    default:
        jl_errorf("%s: runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64", name);
    }
    return newv;
}

// binary operator generator //

// integer

typedef void (*intrinsic_2_t)(unsigned, void*, void*, void*);
SELECTOR_FUNC(intrinsic_2)
#define bi_iintrinsic(name, u, cvtb) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *a, jl_value_t *b) \
{ \
    return jl_iintrinsic_2(a, b, #name, u##signbitbyte, jl_intrinsiclambda_2, name##_list, cvtb); \
}
#define bi_iintrinsic_cnvtb_fast(LLVMOP, OP, name, u, cvtb) \
bi_intrinsic_ctype(OP, name, 8, u##int##8_t) \
bi_intrinsic_ctype(OP, name, 16, u##int##16_t) \
bi_intrinsic_ctype(OP, name, 32, u##int##32_t) \
bi_intrinsic_ctype(OP, name, 64, u##int##64_t) \
static select_intrinsic_2_t name##_list = { \
    LLVMOP, \
    jl_##name##8, \
    jl_##name##16, \
    jl_##name##32, \
    jl_##name##64, \
}; \
bi_iintrinsic(name, u, cvtb)
#define bi_iintrinsic_fast(LLVMOP, OP, name, u) \
    bi_iintrinsic_cnvtb_fast(LLVMOP, OP, name, u, 0)

typedef int (*intrinsic_cmp_t)(unsigned, void*, void*);
SELECTOR_FUNC(intrinsic_cmp)
#define cmp_iintrinsic(name, u) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *a, jl_value_t *b) \
{ \
    return jl_iintrinsic_2(a, b, #name, u##signbitbyte, jl_intrinsiclambda_cmp, name##_list, 0); \
}
#define bool_iintrinsic_fast(LLVMOP, OP, name, u) \
bool_intrinsic_ctype(OP, name, 8, u##int##8_t) \
bool_intrinsic_ctype(OP, name, 16, u##int##16_t) \
bool_intrinsic_ctype(OP, name, 32, u##int##32_t) \
bool_intrinsic_ctype(OP, name, 64, u##int##64_t) \
static select_intrinsic_cmp_t name##_list = { \
    LLVMOP, \
    jl_##name##8, \
    jl_##name##16, \
    jl_##name##32, \
    jl_##name##64, \
}; \
cmp_iintrinsic(name, u)

typedef int (*intrinsic_checked_t)(unsigned, void*, void*, void*);
SELECTOR_FUNC(intrinsic_checked)
#define checked_iintrinsic(name, u) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *a, jl_value_t *b) \
{ \
    return jl_iintrinsic_2(a, b, #name, u##signbitbyte, jl_intrinsiclambda_checked, name##_list, 0); \
}
#define checked_iintrinsic_fast(LLVMOP, CHECK_OP, OP, name, u) \
checked_intrinsic_ctype(CHECK_OP, OP, name, 8, u##int##8_t) \
checked_intrinsic_ctype(CHECK_OP, OP, name, 16, u##int##16_t) \
checked_intrinsic_ctype(CHECK_OP, OP, name, 32, u##int##32_t) \
checked_intrinsic_ctype(CHECK_OP, OP, name, 64, u##int##64_t) \
static select_intrinsic_checked_t name##_list = { \
    LLVMOP, \
    jl_##name##8, \
    jl_##name##16, \
    jl_##name##32, \
    jl_##name##64, \
}; \
checked_iintrinsic(name, u)
#define checked_iintrinsic_slow(LLVMOP, name, u) \
static select_intrinsic_checked_t name##_list = { \
    LLVMOP \
}; \
checked_iintrinsic(name, u)

static inline jl_value_t *jl_iintrinsic_2(jl_value_t *a, jl_value_t *b, const char *name, char (*getsign)(void*, unsigned),
        jl_value_t* (*lambda2)(jl_value_t*, void*, void*, unsigned, unsigned, void*),
        void *list, int cvtb)
{
    jl_value_t *ty = jl_typeof(a);
    jl_value_t *tyb = jl_typeof(b);
    if (tyb != ty) {
        if (!cvtb)
            jl_errorf("%s: types of a and b must match", name);
        if (!jl_is_bitstype(tyb))
            jl_errorf("%s: b is not a bitstypes", name);
    }
    if (!jl_is_bitstype(ty))
        jl_errorf("%s: a is not a bitstypes", name);
    void *pa = jl_data_ptr(a), *pb = jl_data_ptr(b);
    unsigned sz = jl_datatype_size(ty);
    unsigned sz2 = next_power_of_two(sz);
    unsigned szb = jl_datatype_size(tyb);
    if (sz2 > sz) {
        /* round type up to the appropriate c-type and set/clear the unused bits */
        void *pa2 = alloca(sz2);
        memcpy(pa2, pa, sz);
        memset((char*)pa2 + sz, getsign(pa, sz), sz2 - sz);
        pa = pa2;
    }
    if (sz2 > szb) {
        /* round type up to the appropriate c-type and set/clear/truncate the unused bits */
        void *pb2 = alloca(sz2);
        memcpy(pb2, pb, szb);
        memset((char*)pb2 + szb, getsign(pb, sz), sz2 - szb);
        pb = pb2;
    }
    jl_value_t *newv = lambda2(ty, pa, pb, sz, sz2, list);
    return newv;
}

static inline jl_value_t *jl_intrinsiclambda_2(jl_value_t *ty, void *pa, void *pb, unsigned sz, unsigned sz2, void *voidlist)
{
    jl_value_t *newv = newstruct((jl_datatype_t*)ty);
    intrinsic_2_t op = select_intrinsic_2(sz2, (intrinsic_2_t*)voidlist);
    op(sz * host_char_bit, pa, pb, jl_data_ptr(newv));
    if (ty == (jl_value_t*)jl_bool_type)
        return *(uint8_t*)jl_data_ptr(newv) & 1 ? jl_true : jl_false;
    return newv;
}

static inline jl_value_t *jl_intrinsiclambda_cmp(jl_value_t *ty, void *pa, void *pb, unsigned sz, unsigned sz2, void *voidlist)
{
    intrinsic_cmp_t op = select_intrinsic_cmp(sz2, (intrinsic_cmp_t*)voidlist);
    int cmp = op(sz * host_char_bit, pa, pb);
    return cmp ? jl_true : jl_false;
}

static inline jl_value_t *jl_intrinsiclambda_checked(jl_value_t *ty, void *pa, void *pb, unsigned sz, unsigned sz2, void *voidlist)
{
    jl_value_t *newv = newstruct((jl_datatype_t*)ty);
    intrinsic_checked_t op = select_intrinsic_checked(sz2, (intrinsic_checked_t*)voidlist);
    int ovflw = op(sz * host_char_bit, pa, pb, jl_data_ptr(newv));
    if (ovflw)
        jl_throw(jl_overflow_exception);
    if (ty == (jl_value_t*)jl_bool_type)
        return *(uint8_t*)jl_data_ptr(newv) & 1 ? jl_true : jl_false;
    return newv;
}

// floating point

#define bi_fintrinsic(OP, name) \
    bi_intrinsic_ctype(OP, name, 32, float) \
    bi_intrinsic_ctype(OP, name, 64, double) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *a, jl_value_t *b) \
{ \
    jl_value_t *ty = jl_typeof(a); \
    if (jl_typeof(b) != ty) \
        jl_error(#name ": types of a and b must match"); \
    if (!jl_is_bitstype(ty)) \
        jl_error(#name ": values are not bitstypes"); \
    jl_value_t *newv = newstruct((jl_datatype_t*)ty); \
    void *pa = jl_data_ptr(a), *pb = jl_data_ptr(b), *pr = jl_data_ptr(newv); \
    int sz = jl_datatype_size(ty); \
    switch (sz) { \
    /* choose the right size c-type operation */ \
    case 4: \
        jl_##name##32(32, pa, pb, pr); \
        break; \
    case 8: \
        jl_##name##64(64, pa, pb, pr); \
        break; \
    default: \
        jl_error(#name ": runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64"); \
    } \
    return newv; \
}

#define bool_fintrinsic(OP, name) \
    bool_intrinsic_ctype(OP, name, 32, float) \
    bool_intrinsic_ctype(OP, name, 64, double) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *a, jl_value_t *b) \
{ \
    jl_value_t *ty = jl_typeof(a); \
    if (jl_typeof(b) != ty) \
        jl_error(#name ": types of a and b must match"); \
    if (!jl_is_bitstype(ty)) \
        jl_error(#name ": values are not bitstypes"); \
    void *pa = jl_data_ptr(a), *pb = jl_data_ptr(b); \
    int sz = jl_datatype_size(ty); \
    int cmp; \
    switch (sz) { \
    /* choose the right size c-type operation */ \
    case 4: \
        cmp = jl_##name##32(32, pa, pb); \
        break; \
    case 8: \
        cmp = jl_##name##64(64, pa, pb); \
        break; \
    default: \
        jl_error(#name ": runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64"); \
    } \
    return cmp ? jl_true : jl_false; \
}

#define ter_fintrinsic(OP, name) \
    ter_intrinsic_ctype(OP, name, 32, float) \
    ter_intrinsic_ctype(OP, name, 64, double) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *a, jl_value_t *b, jl_value_t *c) \
{ \
    jl_value_t *ty = jl_typeof(a); \
    if (jl_typeof(b) != ty || jl_typeof(c) != ty) \
        jl_error(#name ": types of a, b, and c must match"); \
    if (!jl_is_bitstype(ty)) \
        jl_error(#name ": values are not bitstypes"); \
    jl_value_t *newv = newstruct((jl_datatype_t*)ty); \
    void *pa = jl_data_ptr(a), *pb = jl_data_ptr(b), *pc = jl_data_ptr(c), *pr = jl_data_ptr(newv); \
    int sz = jl_datatype_size(ty); \
    switch (sz) { \
    /* choose the right size c-type operation */ \
    case 4: \
        jl_##name##32(32, pa, pb, pc, pr); \
        break; \
    case 8: \
        jl_##name##64(64, pa, pb, pc, pr); \
        break; \
    default: \
        jl_error(#name ": runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64"); \
    } \
    return newv; \
}

// arithmetic
#define neg(a) -a
#define neg_float(pr, a) *pr = -a
un_iintrinsic_fast(LLVMNeg, neg, neg_int, u)
#define add(a,b) a + b
bi_iintrinsic_fast(LLVMAdd, add, add_int, u)
#define sub(a,b) a - b
bi_iintrinsic_fast(LLVMSub, sub, sub_int, u)
#define mul(a,b) a * b
bi_iintrinsic_fast(LLVMMul, mul, mul_int, u)
#define div(a,b) a / b
bi_iintrinsic_fast(LLVMSDiv, div, sdiv_int,  )
bi_iintrinsic_fast(LLVMUDiv, div, udiv_int, u)
#define rem(a,b) a % b
bi_iintrinsic_fast(LLVMSRem, rem, srem_int,  )
bi_iintrinsic_fast(LLVMURem, rem, urem_int, u)
#define smod(a,b) ((a < 0) == (b < 0)) ? a % b : (b + (a % b)) % b
bi_iintrinsic_fast(jl_LLVMSMod, smod, smod_int,  )
#define frem(a, b) \
    fp_select2(a, b, fmod)

un_fintrinsic(neg_float,neg_float)
bi_fintrinsic(add,add_float)
bi_fintrinsic(sub,sub_float)
bi_fintrinsic(mul,mul_float)
bi_fintrinsic(div,div_float)
bi_fintrinsic(frem,rem_float)

// ternary operators //
#define fma(a, b, c) \
    sizeof(a) == sizeof(float) ? fmaf(a, b, c) : fma(a, b, c)
#define muladd(a, b, c) a * b + c
ter_fintrinsic(fma,fma_float)
ter_fintrinsic(muladd,muladd_float)

// same-type comparisons
#define eq(a,b) a == b
bool_iintrinsic_fast(LLVMICmpEQ, eq, eq_int, u)
#define ne(a,b) a != b
bool_iintrinsic_fast(LLVMICmpNE, ne, ne_int, u)
#define lt(a,b) a < b
bool_iintrinsic_fast(LLVMICmpSLT, lt, slt_int,  )
bool_iintrinsic_fast(LLVMICmpULT, lt, ult_int, u)
#define le(a,b) a <= b
bool_iintrinsic_fast(LLVMICmpSLE, le, sle_int,  )
bool_iintrinsic_fast(LLVMICmpULE, le, ule_int, u)

typedef union {
    float f;
    int32_t d;
    uint32_t ud;
} bits32;
typedef union {
    double f;
    int64_t d;
    uint64_t ud;
} bits64;

#define fpiseq_n(c_type, nbits) \
static inline int fpiseq##nbits(c_type a, c_type b) { \
    bits##nbits ua, ub; \
    ua.f = a; \
    ub.f = b; \
    return (isnan(a) && isnan(b)) || ua.d == ub.d; \
}
fpiseq_n(float, 32)
fpiseq_n(double, 64)
#define fpiseq(a,b) \
    sizeof(a) == sizeof(float) ? fpiseq32(a, b) : fpiseq64(a, b)

#define fpislt_n(c_type, nbits) \
static inline int fpislt##nbits(c_type a, c_type b) { \
    bits##nbits ua, ub; \
    ua.f = a; \
    ub.f = b; \
    if (!isnan(a) && isnan(b)) \
        return 1; \
    if (isnan(a) || isnan(b)) \
        return 0; \
    if (ua.d >= 0 && ua.d < ub.d) \
        return 1; \
    if (ua.d < 0 && ua.ud > ub.ud) \
        return 1; \
    return 0; \
}
fpislt_n(float, 32)
fpislt_n(double, 64)
#define fpislt(a, b) \
    sizeof(a) == sizeof(float) ? fpislt32(a, b) : fpislt64(a, b)

bool_fintrinsic(eq,eq_float)
bool_fintrinsic(ne,ne_float)
bool_fintrinsic(lt,lt_float)
bool_fintrinsic(le,le_float)
bool_fintrinsic(fpiseq,fpiseq)
bool_fintrinsic(fpislt,fpislt)

// bitwise operators
#define and_op(a,b) a & b
bi_iintrinsic_fast(LLVMAnd, and_op, and_int, u)
#define or_op(a,b) a | b
bi_iintrinsic_fast(LLVMOr, or_op, or_int, u)
#define xor_op(a,b) a ^ b
bi_iintrinsic_fast(LLVMXor, xor_op, xor_int, u)
#define shl_op(a,b) b >= 8 * sizeof(a) ? 0 : a << b
bi_iintrinsic_cnvtb_fast(LLVMShl, shl_op, shl_int, u, 1)
#define lshr_op(a,b) (b >= 8 * sizeof(a)) ? 0 : a >> b
bi_iintrinsic_cnvtb_fast(LLVMLShr, lshr_op, lshr_int, u, 1)
#define ashr_op(a,b) \
        /* if ((signed)a > 0) [in two's complement] ? ... : ...) */ \
        (a >> (host_char_bit * sizeof(a) - 1)) ? ~(b >= 8 * sizeof(a) ? 0 : (~a) >> b) : (b >= 8 * sizeof(a) ? 0 : a >> b)
bi_iintrinsic_cnvtb_fast(LLVMAShr, ashr_op, ashr_int, u, 1)
//#define bswap_op(a) __builtin_bswap(a)
//un_iintrinsic_fast(LLVMByteSwap, bswap_op, bswap_int, u)
un_iintrinsic_slow(LLVMByteSwap, bswap_int, u)
//#define ctpop_op(a) __builtin_ctpop(a)
//uu_iintrinsic_fast(LLVMCountPopulation, ctpop_op, ctpop_int, u)
uu_iintrinsic_slow(LLVMCountPopulation, ctpop_int, u)
//#define ctlz_op(a) __builtin_ctlz(a)
//uu_iintrinsic_fast(LLVMCountLeadingZeros, ctlz_op, ctlz_int, u)
uu_iintrinsic_slow(LLVMCountLeadingZeros, ctlz_int, u)
//#define cttz_op(a) __builtin_cttz(a)
//uu_iintrinsic_fast(LLVMCountTrailingZeros, cttz_op, cttz_int, u)
uu_iintrinsic_slow(LLVMCountTrailingZeros, cttz_int, u)
#define not_op(a) ~a
un_iintrinsic_fast(LLVMFlipAllBits, not_op, not_int, u)

// conversions
cvt_iintrinsic(LLVMTrunc, trunc_int)
cvt_iintrinsic(LLVMSExt, sext_int)
cvt_iintrinsic(LLVMZExt, zext_int)
cvt_iintrinsic(LLVMSItoFP, sitofp)
cvt_iintrinsic(LLVMUItoFP, uitofp)
cvt_iintrinsic(LLVMFPtoSI, fptosi)
cvt_iintrinsic(LLVMFPtoUI, fptoui)

#define fpcvt(pr, a) \
        if (osize == 32) \
            *(float*)pr = a; \
        else if (osize == 64) \
            *(double*)pr = a; \
        else \
            jl_error("fptrunc/fpext: runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64");
un_fintrinsic_withtype(fpcvt,fptrunc)
un_fintrinsic_withtype(fpcvt,fpext)

JL_DLLEXPORT jl_value_t *jl_fptoui_auto(jl_value_t *a)
{
    jl_datatype_t *ty;
    switch (jl_datatype_size(jl_typeof(a))) {
        case 4:
            ty = jl_uint32_type;
            break;
        case 8:
            ty = jl_uint64_type;
            break;
        default:
            jl_error("fptoui: runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64");
    }
    return jl_fptoui((jl_value_t*)ty, a);
}
JL_DLLEXPORT jl_value_t *jl_fptosi_auto(jl_value_t *a)
{
    jl_datatype_t *ty;
    switch (jl_datatype_size(jl_typeof(a))) {
        case 4:
            ty = jl_int32_type;
            break;
        case 8:
            ty = jl_int64_type;
            break;
        default:
            jl_error("fptoui: runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64");
    }
    return jl_fptosi((jl_value_t*)ty, a);
}

// checked conversion
static inline int all_eq(char *p, char n, char v)
{
    // computes p[0:n] == v
    while (n--)
        if (*p++ != v)
            return 0;
    return 1;
}
static unsigned check_trunc_sint(unsigned isize, unsigned osize, void *pa)
{
    return !all_eq((char*)pa + osize, isize - osize, signbitbyte(pa, isize)); // TODO: assumes little-endian
}
cvt_iintrinsic_checked(LLVMTrunc, check_trunc_sint, checked_trunc_sint)
static unsigned check_trunc_uint(unsigned isize, unsigned osize, void *pa)
{
    return !all_eq((char*)pa + osize, isize - osize, 0); // TODO: assumes little-endian
}
cvt_iintrinsic_checked(LLVMTrunc, check_trunc_uint, checked_trunc_uint)

#define checked_fptosi(pr, a) \
        if (!LLVMFPtoSI_exact(sizeof(a) * host_char_bit, pa, osize, pr)) \
            jl_throw(jl_inexact_exception);
un_fintrinsic_withtype(checked_fptosi, checked_fptosi)
#define checked_fptoui(pr, a) \
        if (!LLVMFPtoUI_exact(sizeof(a) * host_char_bit, pa, osize, pr)) \
            jl_throw(jl_inexact_exception);
un_fintrinsic_withtype(checked_fptoui, checked_fptoui)

JL_DLLEXPORT jl_value_t *jl_check_top_bit(jl_value_t *a)
{
    jl_value_t *ty = jl_typeof(a);
    if (!jl_is_bitstype(ty))
        jl_error("check_top_bit: value is not a bitstype");
    if (signbitbyte(jl_data_ptr(a), jl_datatype_size(ty)))
        jl_throw(jl_inexact_exception);
    return a;
}

// checked arithmetic
#define check_sadd(a,b) \
        /* this test is a reduction of (b > 0) ? (a + b > typemax(a)) : (a + b < typemin(a)) ==> overflow \
         * where (a - a) == (typeof(a))0 */ \
        (b > 0) ? (a > ~((a - a + 1) << (8 * sizeof(a) - 1)) - b) : (a < ((a - a + 1) << (8 * sizeof(a) - 1)) - b)
checked_iintrinsic_fast(LLVMAdd_sov, check_sadd, add, checked_sadd,  )
#define check_uadd(a,b) \
        /* this test checks for (a + b) > typemax(a) ==> overflow */ \
        a >= -b
checked_iintrinsic_fast(LLVMAdd_uov, check_uadd, add, checked_uadd, u)
#define check_ssub(a,b) check_sadd(a,-b)
checked_iintrinsic_fast(LLVMSub_sov, check_ssub, sub, checked_ssub,  )
#define check_usub(a,b) \
        /* this test checks for (a - b) < 0 ==> overflow */ \
        a < b
checked_iintrinsic_fast(LLVMSub_uov, check_usub, sub, checked_usub, u)
checked_iintrinsic_slow(LLVMMul_sov, checked_smul,  )
checked_iintrinsic_slow(LLVMMul_uov, checked_umul, u)
checked_iintrinsic_slow(LLVMDiv_sov, checked_sdiv,  )
checked_iintrinsic_slow(LLVMDiv_uov, checked_udiv, u)
checked_iintrinsic_slow(LLVMRem_sov, checked_srem,  )
checked_iintrinsic_slow(LLVMRem_uov, checked_urem, u)

JL_DLLEXPORT jl_value_t *jl_nan_dom_err(jl_value_t *a, jl_value_t *b)
{
    jl_value_t *ty = jl_typeof(a);
    if (jl_typeof(b) != ty)
        jl_error("nan_dom_err: types of a and b must match");
    if (!jl_is_bitstype(ty))
        jl_error("nan_dom_err: values are not bitstypes");
    switch (jl_datatype_size(ty)) {
        case 4:
            if (isnan(*(float*)a) && !isnan(*(float*)b))
                jl_throw(jl_domain_exception);
            break;
        case 8:
            if (isnan(*(double*)a) && !isnan(*(double*)b))
                jl_throw(jl_domain_exception);
            break;
        default:
            jl_error("nan_dom_err: runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64");
    }
    return a;
}

// functions
#define flipsign(a, b) \
        (b >= 0) ? a : -a
bi_iintrinsic_fast(jl_LLVMFlipSign, flipsign, flipsign_int,  )
#define abs_float(pr, a) *pr = fp_select(a, fabs)
#define ceil_float(pr, a) *pr = fp_select(a, ceil)
#define floor_float(pr, a) *pr = fp_select(a, floor)
#define trunc_float(pr, a) *pr = fp_select(a, trunc)
#define rint_float(pr, a) *pr = fp_select(a, rint)
#define sqrt_float(pr, a) \
        if (a < 0) \
            jl_throw(jl_domain_exception); \
        *pr = fp_select(a, sqrt)
#define copysign_float(a, b) \
        fp_select2(a, b, copysign)

un_fintrinsic(abs_float,abs_float)
bi_fintrinsic(copysign_float,copysign_float)
un_fintrinsic(ceil_float,ceil_llvm)
un_fintrinsic(floor_float,floor_llvm)
un_fintrinsic(trunc_float,trunc_llvm)
un_fintrinsic(rint_float,rint_llvm)
un_fintrinsic(sqrt_float,sqrt_llvm)

JL_DLLEXPORT jl_value_t *jl_powi_llvm(jl_value_t *a, jl_value_t *b)
{
    jl_value_t *ty = jl_typeof(a);
    if (!jl_is_bitstype(ty))
        jl_error("powi_llvm: a is not a bitstype");
    if (!jl_is_bitstype(jl_typeof(b)) || jl_datatype_size(jl_typeof(b)) != 4)
        jl_error("powi_llvm: b is not a 32-bit bitstype");
    jl_value_t *newv = newstruct((jl_datatype_t*)ty);
    void *pa = jl_data_ptr(a), *pr = jl_data_ptr(newv);
    int sz = jl_datatype_size(ty);
    switch (sz) {
    /* choose the right size c-type operation */
    case 4:
        *(float*)pr = powf(*(float*)pa, (float)jl_unbox_int32(b));
        break;
    case 8:
        *(double*)pr = pow(*(double*)pa, (double)jl_unbox_int32(b));
        break;
    default:
        jl_error("powi_llvm: runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64");
    }
    return newv;
}

JL_DLLEXPORT jl_value_t *jl_select_value(jl_value_t *isfalse, jl_value_t *a, jl_value_t *b)
{
    JL_TYPECHK(isfalse, bool, isfalse);
    return (isfalse == jl_false ? b : a);
}

JL_DLLEXPORT jl_value_t *jl_arraylen(jl_value_t *a)
{
    return jl_box_long(jl_array_len((jl_array_t*)a));
}
