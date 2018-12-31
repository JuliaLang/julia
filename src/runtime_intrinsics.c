// This file is a part of Julia. License is MIT: https://julialang.org/license

// This is in implementation of the Julia intrinsic functions against boxed types
// excluding the native function call interface (ccall, llvmcall)
//
// this file assumes a little-endian processor, although that isn't too hard to fix
// it also assumes two's complement negative numbers, which might be a bit harder to fix
//
// TODO: add half-float support

#include "julia.h"
#include "julia_internal.h"
#include "APInt-C.h"

const unsigned int host_char_bit = 8;

// run time version of bitcast intrinsic
JL_DLLEXPORT jl_value_t *jl_bitcast(jl_value_t *ty, jl_value_t *v)
{
    JL_TYPECHK(bitcast, datatype, ty);
    if (!jl_is_concrete_type(ty) || !jl_is_primitivetype(ty))
        jl_error("bitcast: target type not a leaf primitive type");
    if (!jl_is_primitivetype(jl_typeof(v)))
        jl_error("bitcast: value not a primitive type");
    if (jl_datatype_size(jl_typeof(v)) != jl_datatype_size(ty))
        jl_error("bitcast: argument size does not match size of target type");
    if (ty == jl_typeof(v))
        return v;
    if (ty == (jl_value_t*)jl_bool_type)
        return *(uint8_t*)jl_data_ptr(v) & 1 ? jl_true : jl_false;
    return jl_new_bits(ty, jl_data_ptr(v));
}

// run time version of pointerref intrinsic (warning: i is not rooted)
JL_DLLEXPORT jl_value_t *jl_pointerref(jl_value_t *p, jl_value_t *i, jl_value_t *align)
{
    JL_TYPECHK(pointerref, pointer, p);
    JL_TYPECHK(pointerref, long, i)
    JL_TYPECHK(pointerref, long, align);
    jl_value_t *ety = jl_tparam0(jl_typeof(p));
    if (ety == (jl_value_t*)jl_any_type) {
        jl_value_t **pp = (jl_value_t**)(jl_unbox_long(p) + (jl_unbox_long(i)-1)*sizeof(void*));
        return *pp;
    }
    else {
        if (!jl_is_datatype(ety))
            jl_error("pointerref: invalid pointer");
        size_t nb = LLT_ALIGN(jl_datatype_size(ety), jl_datatype_align(ety));
        char *pp = (char*)jl_unbox_long(p) + (jl_unbox_long(i)-1)*nb;
        return jl_new_bits(ety, pp);
    }
}

// run time version of pointerset intrinsic (warning: x is not gc-rooted)
JL_DLLEXPORT jl_value_t *jl_pointerset(jl_value_t *p, jl_value_t *x, jl_value_t *i, jl_value_t *align)
{
    JL_TYPECHK(pointerset, pointer, p);
    JL_TYPECHK(pointerset, long, i);
    JL_TYPECHK(pointerref, long, align);
    jl_value_t *ety = jl_tparam0(jl_typeof(p));
    if (ety == (jl_value_t*)jl_any_type) {
        jl_value_t **pp = (jl_value_t**)(jl_unbox_long(p) + (jl_unbox_long(i)-1)*sizeof(void*));
        *pp = x;
    }
    else {
        if (!jl_is_datatype(ety))
            jl_error("pointerset: invalid pointer");
        size_t elsz = jl_datatype_size(ety);
        size_t nb = LLT_ALIGN(elsz, jl_datatype_align(ety));
        char *pp = (char*)jl_unbox_long(p) + (jl_unbox_long(i)-1)*nb;
        if (jl_typeof(x) != ety)
            jl_type_error("pointerset", ety, x);
        memcpy(pp, x, elsz);
    }
    return p;
}

extern uint32_t __gmp_version;
extern int32_t __gmp_bits_per_limb;
extern void *jl_gc_counted_malloc(size_t sz);
extern void *jl_gc_counted_realloc_with_old_size(void *p, size_t old, size_t sz);
extern void jl_gc_counted_free_with_size(void *p, size_t sz);
JL_DLLEXPORT jl_value_t *jl_cglobal(jl_value_t *v, jl_value_t *ty)
{
    jl_(v); jl_(ty);
    JL_TYPECHK(cglobal, type, ty);
    JL_GC_PUSH1(&v);
    jl_value_t *rt =
        ty == (jl_value_t*)jl_void_type ? (jl_value_t*)jl_voidpointer_type : // a common case
            (jl_value_t*)jl_apply_type1((jl_value_t*)jl_pointer_type, ty);
    JL_GC_PROMISE_ROOTED(rt); // (JL_ALWAYS_LEAFTYPE)

    if (jl_is_symbol(v) || jl_is_tuple(v)) {
        jl_value_t *sym = v;
        if (jl_is_tuple(sym))
            sym = jl_fieldref(sym, 0);
        const char *name = jl_symbol_name(sym);
        
        # define WRAP(symname) \
        else if (strcmp(name, #symname) == 0) {                     \
            jl_ptls_t ptls = jl_get_ptls_states();                  \
            jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);   \
            *(void**)jl_data_ptr(v) = &symname;                     \
            return v;                                               \
        }
        
        if (strcmp(name, "jl_options") == 0) {
            jl_ptls_t ptls = jl_get_ptls_states();
            jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
            *(void**)jl_data_ptr(v) = &jl_options;
            return v;
        } else if (strcmp(name, "jl_uv_stdout") == 0) {
            jl_ptls_t ptls = jl_get_ptls_states();
            jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
            *(void**)jl_data_ptr(v) = &JL_STDOUT;
            return v;
        } else if (strcmp(name, "jl_uv_stderr") == 0) {
            jl_ptls_t ptls = jl_get_ptls_states();
            jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
            *(void**)jl_data_ptr(v) = &JL_STDERR;
            return v;
        } 
        WRAP(__gmp_version)
        WRAP(__gmp_bits_per_limb)
        WRAP(jl_gc_counted_malloc)        
        WRAP(jl_gc_counted_realloc_with_old_size)
        WRAP(jl_gc_counted_free_with_size)
    }

    if (!jl_is_concrete_type(rt))
        jl_error("cglobal: type argument not concrete");

    if (jl_is_tuple(v) && jl_nfields(v) == 1)
        v = jl_fieldref(v, 0);

    if (jl_is_pointer(v)) {
        v = jl_bitcast(rt, v);
        JL_GC_POP();
        return v;
    }

    char *f_lib = NULL;
    if (jl_is_tuple(v) && jl_nfields(v) > 1) {
        jl_value_t *t1 = jl_fieldref_noalloc(v, 1);
        v = jl_fieldref(v, 0);
        if (jl_is_symbol(t1))
            f_lib = jl_symbol_name((jl_sym_t*)t1);
        else if (jl_is_string(t1))
            f_lib = jl_string_data(t1);
        else
            JL_TYPECHK(cglobal, symbol, t1)
    }

    char *f_name = NULL;
    if (jl_is_symbol(v))
        f_name = jl_symbol_name((jl_sym_t*)v);
    else if (jl_is_string(v))
        f_name = jl_string_data(v);
    else
        JL_TYPECHK(cglobal, symbol, v)

#ifdef _OS_WINDOWS_
    if (!f_lib)
        f_lib = (char*)jl_dlfind_win32(f_name);
#endif

    void *ptr;
    jl_dlsym(jl_get_library(f_lib), f_name, &ptr, 1);
    jl_value_t *jv = jl_gc_alloc_1w();
    jl_set_typeof(jv, rt);
    *(void**)jl_data_ptr(jv) = ptr;
    JL_GC_POP();
    return jv;
}

JL_DLLEXPORT jl_value_t *jl_cglobal_auto(jl_value_t *v) {
    return jl_cglobal(v, (jl_value_t*)jl_void_type);
}

static inline char signbitbyte(void *a, unsigned bytes) JL_NOTSAFEPOINT
{
    // sign bit of an signed number of n bytes, as a byte
    return (((signed char*)a)[bytes - 1] < 0) ? ~0 : 0;
}

static inline char usignbitbyte(void *a, unsigned bytes) JL_NOTSAFEPOINT
{
    // sign bit of an unsigned number
    return 0;
}

static inline unsigned select_by_size(unsigned sz) JL_NOTSAFEPOINT
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
    static inline intrinsic##_t select_##intrinsic(unsigned sz, const select_##intrinsic##_t list) JL_NOTSAFEPOINT \
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
static inline void jl_##name##nbits(unsigned runtime_nbits, void *pa, void *pr) JL_NOTSAFEPOINT \
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
static inline unsigned jl_##name##nbits(unsigned runtime_nbits, void *pa) JL_NOTSAFEPOINT \
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
static inline void name(unsigned osize, void *pa, void *pr) JL_NOTSAFEPOINT \
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
static void jl_##name##nbits(unsigned runtime_nbits, void *pa, void *pb, void *pr) JL_NOTSAFEPOINT \
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
static int jl_##name##nbits(unsigned runtime_nbits, void *pa, void *pb) JL_NOTSAFEPOINT \
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
static int jl_##name##nbits(unsigned runtime_nbits, void *pa, void *pb, void *pr) JL_NOTSAFEPOINT \
{ \
    c_type a = *(c_type*)pa; \
    c_type b = *(c_type*)pb; \
    *(c_type*)pr = (c_type)OP(a, b); \
    return CHECK_OP(a, b); \
}

// float inputs
// OP::Function macro(inputa, inputb, inputc)
// name::unique string
// nbits::number of bits
// c_type::c_type corresponding to nbits
#define ter_intrinsic_ctype(OP, name, nbits, c_type) \
static void jl_##name##nbits(unsigned runtime_nbits, void *pa, void *pb, void *pc, void *pr) JL_NOTSAFEPOINT \
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
static const select_intrinsic_1_t name##_list = { \
    LLVMOP, \
    jl_##name##8, \
    jl_##name##16, \
    jl_##name##32, \
    jl_##name##64, \
}; \
un_iintrinsic(name, u)
#define un_iintrinsic_slow(LLVMOP, name, u) \
static const select_intrinsic_1_t name##_list = { \
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
static const select_intrinsic_u1_t name##_list = { \
    LLVMOP, \
    jl_##name##8, \
    jl_##name##16, \
    jl_##name##32, \
    jl_##name##64, \
}; \
uu_iintrinsic(name, u)
#define uu_iintrinsic_slow(LLVMOP, name, u) \
static const select_intrinsic_u1_t name##_list = { \
    LLVMOP \
}; \
uu_iintrinsic(name, u)

static inline
jl_value_t *jl_iintrinsic_1(jl_value_t *ty, jl_value_t *a, const char *name,
                            char (*getsign)(void*, unsigned),
                            jl_value_t *(*lambda1)(jl_value_t*, void*, unsigned, unsigned, const void*), const void *list)
{
    if (!jl_is_primitivetype(jl_typeof(a)))
        jl_errorf("%s: value is not a primitive type", name);
    if (!jl_is_primitivetype(ty))
        jl_errorf("%s: type is not a primitive type", name);
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

static inline jl_value_t *jl_intrinsiclambda_ty1(jl_value_t *ty, void *pa, unsigned osize, unsigned osize2, const void *voidlist)
{
    intrinsic_1_t op = select_intrinsic_1(osize2, (const intrinsic_1_t*)voidlist);
    void *pr = alloca(osize2);
    op(osize * host_char_bit, pa, pr);
    return jl_new_bits(ty, pr);
}

static inline jl_value_t *jl_intrinsiclambda_u1(jl_value_t *ty, void *pa, unsigned osize, unsigned osize2, const void *voidlist)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    intrinsic_u1_t op = select_intrinsic_u1(osize2, (const intrinsic_u1_t*)voidlist);
    uint64_t cnt = op(osize * host_char_bit, pa);
    // TODO: the following assume little-endian
    // for big-endian, need to copy from the other end of cnt
    if (osize <= sizeof(cnt)) {
        return jl_new_bits(ty, &cnt);
    }
    jl_value_t *newv = jl_gc_alloc(ptls, osize, ty);
    // perform zext, if needed
    memset((char*)jl_data_ptr(newv) + sizeof(cnt), 0, osize - sizeof(cnt));
    memcpy(jl_data_ptr(newv), &cnt, sizeof(cnt));
    return newv;
}

// conversion operator

typedef void (*intrinsic_cvt_t)(unsigned, void*, unsigned, void*);
typedef unsigned (*intrinsic_cvt_check_t)(unsigned, unsigned, void*);
#define cvt_iintrinsic(LLVMOP, name) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *ty, jl_value_t *a) \
{ \
    return jl_intrinsic_cvt(ty, a, #name, LLVMOP); \
}

static inline jl_value_t *jl_intrinsic_cvt(jl_value_t *ty, jl_value_t *a, const char *name, intrinsic_cvt_t op)
{
    jl_value_t *aty = jl_typeof(a);
    if (!jl_is_primitivetype(aty))
        jl_errorf("%s: value is not a primitive type", name);
    if (!jl_is_primitivetype(ty))
        jl_errorf("%s: type is not a primitive type", name);
    void *pa = jl_data_ptr(a);
    unsigned isize = jl_datatype_size(aty);
    unsigned osize = jl_datatype_size(ty);
    void *pr = alloca(osize);
    unsigned isize_bits = isize * host_char_bit;
    unsigned osize_bits = osize * host_char_bit;
    op(isize_bits, pa, osize_bits, pr);
    return jl_new_bits(ty, pr);
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
    jl_ptls_t ptls = jl_get_ptls_states();
    if (!jl_is_primitivetype(jl_typeof(a)))
        jl_errorf("%s: value is not a primitive type", name);
    if (!jl_is_primitivetype(ty))
        jl_errorf("%s: type is not a primitive type", name);
    unsigned sz2 = jl_datatype_size(ty);
    jl_value_t *newv = jl_gc_alloc(ptls, sz2, ty);
    void *pa = jl_data_ptr(a), *pr = jl_data_ptr(newv);
    unsigned sz = jl_datatype_size(jl_typeof(a));
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
static const select_intrinsic_2_t name##_list = { \
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
static const select_intrinsic_cmp_t name##_list = { \
    LLVMOP, \
    jl_##name##8, \
    jl_##name##16, \
    jl_##name##32, \
    jl_##name##64, \
}; \
cmp_iintrinsic(name, u)

typedef int (*intrinsic_checked_t)(unsigned, void*, void*, void*) JL_NOTSAFEPOINT;
SELECTOR_FUNC(intrinsic_checked)
#define checked_iintrinsic(name, u, lambda_checked) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *a, jl_value_t *b) \
{ \
    return jl_iintrinsic_2(a, b, #name, u##signbitbyte, lambda_checked, name##_list, 0); \
}
#define checked_iintrinsic_fast(LLVMOP, CHECK_OP, OP, name, u) \
checked_intrinsic_ctype(CHECK_OP, OP, name, 8, u##int##8_t) \
checked_intrinsic_ctype(CHECK_OP, OP, name, 16, u##int##16_t) \
checked_intrinsic_ctype(CHECK_OP, OP, name, 32, u##int##32_t) \
checked_intrinsic_ctype(CHECK_OP, OP, name, 64, u##int##64_t) \
static const select_intrinsic_checked_t name##_list = { \
    LLVMOP, \
    jl_##name##8, \
    jl_##name##16, \
    jl_##name##32, \
    jl_##name##64, \
}; \
checked_iintrinsic(name, u, jl_intrinsiclambda_checked)
#define checked_iintrinsic_slow(LLVMOP, name, u) \
static const select_intrinsic_checked_t name##_list = { \
    LLVMOP \
}; \
checked_iintrinsic(name, u, jl_intrinsiclambda_checked)
#define checked_iintrinsic_div(LLVMOP, name, u) \
static const select_intrinsic_checked_t name##_list = { \
    LLVMOP \
}; \
checked_iintrinsic(name, u, jl_intrinsiclambda_checkeddiv)

static inline
jl_value_t *jl_iintrinsic_2(jl_value_t *a, jl_value_t *b, const char *name,
                            char (*getsign)(void*, unsigned),
                            jl_value_t *(*lambda2)(jl_value_t*, void*, void*, unsigned, unsigned, const void*),
                            const void *list, int cvtb)
{
    jl_value_t *ty = jl_typeof(a);
    jl_value_t *tyb = jl_typeof(b);
    if (tyb != ty) {
        if (!cvtb)
            jl_errorf("%s: types of a and b must match", name);
        if (!jl_is_primitivetype(tyb))
            jl_errorf("%s: b is not a primitive type", name);
    }
    if (!jl_is_primitivetype(ty))
        jl_errorf("%s: a is not a primitive type", name);
    void *pa = jl_data_ptr(a), *pb = jl_data_ptr(b);
    unsigned sz = jl_datatype_size(ty);
    unsigned sz2 = next_power_of_two(sz);
    unsigned szb = cvtb ? jl_datatype_size(tyb) : sz;
    if (sz2 > sz) {
        /* round type up to the appropriate c-type and set/clear the unused bits */
        void *pa2 = alloca(sz2);
        memcpy(pa2, pa, sz);
        memset((char*)pa2 + sz, getsign(pa, sz), sz2 - sz);
        pa = pa2;
    }
    if (sz2 > szb) {
        /* round type up to the appropriate c-type and set/clear/truncate the unused bits
         * (zero-extend if cvtb is set, since in that case b is unsigned while the sign of a comes from the op)
         */
        void *pb2 = alloca(sz2);
        memcpy(pb2, pb, szb);
        memset((char*)pb2 + szb, cvtb ? 0 : getsign(pb, szb), sz2 - szb);
        pb = pb2;
    }
    jl_value_t *newv = lambda2(ty, pa, pb, sz, sz2, list);
    return newv;
}

static inline jl_value_t *jl_intrinsiclambda_2(jl_value_t *ty, void *pa, void *pb, unsigned sz, unsigned sz2, const void *voidlist)
{
    void *pr = alloca(sz2);
    intrinsic_2_t op = select_intrinsic_2(sz2, (const intrinsic_2_t*)voidlist);
    op(sz * host_char_bit, pa, pb, pr);
    return jl_new_bits(ty, pr);
}

static inline jl_value_t *jl_intrinsiclambda_cmp(jl_value_t *ty, void *pa, void *pb, unsigned sz, unsigned sz2, const void *voidlist)
{
    intrinsic_cmp_t op = select_intrinsic_cmp(sz2, (const intrinsic_cmp_t*)voidlist);
    int cmp = op(sz * host_char_bit, pa, pb);
    return cmp ? jl_true : jl_false;
}

static inline jl_value_t *jl_intrinsiclambda_checked(jl_value_t *ty, void *pa, void *pb, unsigned sz, unsigned sz2, const void *voidlist)
{
    jl_value_t *params[2];
    params[0] = ty;
    params[1] = (jl_value_t*)jl_bool_type;
    jl_datatype_t *tuptyp = jl_apply_tuple_type_v(params, 2);
    JL_GC_PROMISE_ROOTED(tuptyp); // (JL_ALAWYS_LEAFTYPE)
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_value_t *newv = jl_gc_alloc(ptls, ((jl_datatype_t*)tuptyp)->size, tuptyp);

    intrinsic_checked_t op = select_intrinsic_checked(sz2, (const intrinsic_checked_t*)voidlist);
    int ovflw = op(sz * host_char_bit, pa, pb, jl_data_ptr(newv));

    char *ao = (char*)jl_data_ptr(newv) + sz;
    *ao = (char)ovflw;
    return newv;
}
static inline jl_value_t *jl_intrinsiclambda_checkeddiv(jl_value_t *ty, void *pa, void *pb, unsigned sz, unsigned sz2, const void *voidlist)
{
    void *pr = alloca(sz2);
    intrinsic_checked_t op = select_intrinsic_checked(sz2, (const intrinsic_checked_t*)voidlist);
    int ovflw = op(sz * host_char_bit, pa, pb, pr);
    if (ovflw)
        jl_throw(jl_diverror_exception);
    return jl_new_bits(ty, pr);
}

// floating point

#define bi_fintrinsic(OP, name) \
    bi_intrinsic_ctype(OP, name, 32, float) \
    bi_intrinsic_ctype(OP, name, 64, double) \
JL_DLLEXPORT jl_value_t *jl_##name(jl_value_t *a, jl_value_t *b) \
{ \
    jl_ptls_t ptls = jl_get_ptls_states();\
    jl_value_t *ty = jl_typeof(a); \
    if (jl_typeof(b) != ty) \
        jl_error(#name ": types of a and b must match"); \
    if (!jl_is_primitivetype(ty)) \
        jl_error(#name ": values are not primitive types"); \
    int sz = jl_datatype_size(ty); \
    jl_value_t *newv = jl_gc_alloc(ptls, sz, ty);          \
    void *pa = jl_data_ptr(a), *pb = jl_data_ptr(b), *pr = jl_data_ptr(newv); \
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
    if (!jl_is_primitivetype(ty)) \
        jl_error(#name ": values are not primitive types"); \
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
    jl_ptls_t ptls = jl_get_ptls_states();\
    jl_value_t *ty = jl_typeof(a); \
    if (jl_typeof(b) != ty || jl_typeof(c) != ty) \
        jl_error(#name ": types of a, b, and c must match"); \
    if (!jl_is_primitivetype(ty)) \
        jl_error(#name ": values are not primitive types"); \
    int sz = jl_datatype_size(ty);                                      \
    jl_value_t *newv = jl_gc_alloc(ptls, sz, ty);                       \
    void *pa = jl_data_ptr(a), *pb = jl_data_ptr(b), *pc = jl_data_ptr(c), *pr = jl_data_ptr(newv); \
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
bi_iintrinsic_fast(LLVMAdd, add, add_ptr, u)
#define sub(a,b) a - b
bi_iintrinsic_fast(LLVMSub, sub, sub_int, u)
bi_iintrinsic_fast(LLVMSub, sub, sub_ptr, u)
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
static inline int fpiseq##nbits(c_type a, c_type b) JL_NOTSAFEPOINT { \
    bits##nbits ua, ub; \
    ua.f = a; \
    ub.f = b; \
    return (isnan(a) && isnan(b)) || ua.d == ub.d; \
}
fpiseq_n(float, 32)
fpiseq_n(double, 64)
#define fpiseq(a,b) \
    sizeof(a) == sizeof(float) ? fpiseq32(a, b) : fpiseq64(a, b)

#define fpislt_n(c_type, nbits)                                         \
    static inline int fpislt##nbits(c_type a, c_type b) JL_NOTSAFEPOINT \
    {                                                                   \
        bits##nbits ua, ub;                                             \
        ua.f = a;                                                       \
        ub.f = b;                                                       \
        if (!isnan(a) && isnan(b))                                      \
            return 1;                                                   \
        if (isnan(a) || isnan(b))                                       \
            return 0;                                                   \
        if (ua.d >= 0 && ua.d < ub.d)                                   \
            return 1;                                                   \
        if (ua.d < 0 && ua.ud > ub.ud)                                  \
            return 1;                                                   \
        return 0;                                                       \
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
#define ashr_op(a,b) ((b < 0 || b >= 8 * sizeof(a)) ? a >> (8 * sizeof(a) - 1) : a >> b)
bi_iintrinsic_cnvtb_fast(LLVMAShr, ashr_op, ashr_int, , 1)
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

#define fptrunc(pr, a) \
        if (!(osize < 8 * sizeof(a))) \
            jl_error("fptrunc: output bitsize must be < input bitsize"); \
        if (osize == 32) \
            *(float*)pr = a; \
        else if (osize == 64) \
            *(double*)pr = a; \
        else \
            jl_error("fptrunc: runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64");
#define fpext(pr, a) \
        if (!(osize > 8 * sizeof(a))) \
            jl_error("fpext: output bitsize must be > input bitsize"); \
        if (osize == 32) \
            *(float*)pr = a; \
        else if (osize == 64) \
            *(double*)pr = a; \
        else \
            jl_error("fpext: runtime floating point intrinsics are not implemented for bit sizes other than 32 and 64");
un_fintrinsic_withtype(fptrunc,fptrunc)
un_fintrinsic_withtype(fpext,fpext)

// checked arithmetic
/**
 * s_typemin = - s_typemax - 1
 * s_typemax = ((typeof a)1 << (runtime_nbits - 1)) - 1
 * u_typemin = 0
 * u_typemax = ((typeof a)1 << runtime_nbits) - 1
 * where (a - a) == (typeof(a)0
 **/
#define sTYPEMIN(a) -sTYPEMAX(a) - 1
#define sTYPEMAX(a)                                                \
    (8 * sizeof(a) == runtime_nbits                                \
         ? (((((a - a + 1) << (8 * sizeof(a) - 2)) - 1) << 1) + 1) \
         : (  ((a - a + 1) << (runtime_nbits - 1)) - 1))

#define uTYPEMIN(a) (0)
#define uTYPEMAX(a) \
    (8 * sizeof(~a) == runtime_nbits \
     ? (~(a - a)) \
     : (~((~(a - a)) << runtime_nbits)))
#define check_sadd_int(a,b) \
        /* this test checks for (b >= 0) ? (a + b > typemax) : (a + b < typemin) ==> overflow */ \
        (b >= 0) ? (a > sTYPEMAX(a) - b) : (a < sTYPEMIN(a) - b)
checked_iintrinsic_fast(LLVMAdd_sov, check_sadd_int, add, checked_sadd_int,  )
#define check_uadd_int(a,b) \
        /* this test checks for (a + b) > typemax(a) ==> overflow */ \
        a > uTYPEMAX(a) - b
checked_iintrinsic_fast(LLVMAdd_uov, check_uadd_int, add, checked_uadd_int, u)
#define check_ssub_int(a,b) \
        /* this test checks for (b >= 0) ? (a - b < typemin) : (a - b > typemax) ==> overflow */ \
        (b >= 0) ? (a < sTYPEMIN(a) + b) : (a > sTYPEMAX(a) + b)
checked_iintrinsic_fast(LLVMSub_sov, check_ssub_int, sub, checked_ssub_int,  )
#define check_usub_int(a,b) \
        /* this test checks for (a - b) < typemin ==> overflow */ \
        a < uTYPEMIN(a) + b
checked_iintrinsic_fast(LLVMSub_uov, check_usub_int, sub, checked_usub_int, u)
checked_iintrinsic_slow(LLVMMul_sov, checked_smul_int,  )
checked_iintrinsic_slow(LLVMMul_uov, checked_umul_int, u)

checked_iintrinsic_div(LLVMDiv_sov, checked_sdiv_int,  )
checked_iintrinsic_div(LLVMDiv_uov, checked_udiv_int, u)
checked_iintrinsic_div(LLVMRem_sov, checked_srem_int,  )
checked_iintrinsic_div(LLVMRem_uov, checked_urem_int, u)

// functions
#define flipsign(a, b) \
        (b >= 0) ? a : -a
bi_iintrinsic_fast(jl_LLVMFlipSign, flipsign, flipsign_int,  )
#define abs_float(pr, a)      *pr = fp_select(a, fabs)
#define ceil_float(pr, a)     *pr = fp_select(a, ceil)
#define floor_float(pr, a)    *pr = fp_select(a, floor)
#define trunc_float(pr, a)    *pr = fp_select(a, trunc)
#define rint_float(pr, a)     *pr = fp_select(a, rint)
#define sqrt_float(pr, a)     *pr = fp_select(a, sqrt)
#define copysign_float(a, b)  fp_select2(a, b, copysign)

un_fintrinsic(abs_float,abs_float)
bi_fintrinsic(copysign_float,copysign_float)
un_fintrinsic(ceil_float,ceil_llvm)
un_fintrinsic(floor_float,floor_llvm)
un_fintrinsic(trunc_float,trunc_llvm)
un_fintrinsic(rint_float,rint_llvm)
un_fintrinsic(sqrt_float,sqrt_llvm)

JL_DLLEXPORT jl_value_t *jl_arraylen(jl_value_t *a)
{
    JL_TYPECHK(arraylen, array, a);
    return jl_box_long(jl_array_len((jl_array_t*)a));
}
