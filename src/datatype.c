// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  defining DataTypes
  basic operations on struct and bits values
*/

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

// allocating TypeNames -----------------------------------------------------------

jl_sym_t *jl_demangle_typename(jl_sym_t *s)
{
    char *n = jl_symbol_name(s);
    if (n[0] != '#')
        return s;
    char *end = strrchr(n, '#');
    int32_t len;
    if (end == n || end == n+1)
        len = strlen(n) - 1;
    else
        len = (end-n) - 1;
    return jl_symbol_n(&n[1], len);
}

JL_DLLEXPORT jl_methtable_t *jl_new_method_table(jl_sym_t *name, jl_module_t *module)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_methtable_t *mt =
        (jl_methtable_t*)jl_gc_alloc(ptls, sizeof(jl_methtable_t),
                                     jl_methtable_type);
    mt->name = jl_demangle_typename(name);
    mt->module = module;
    mt->defs.unknown = jl_nothing;
    mt->cache.unknown = jl_nothing;
    mt->max_args = 0;
    mt->kwsorter = NULL;
    mt->backedges = NULL;
    JL_MUTEX_INIT(&mt->writelock);
    return mt;
}

JL_DLLEXPORT jl_typename_t *jl_new_typename_in(jl_sym_t *name, jl_module_t *module)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_typename_t *tn =
        (jl_typename_t*)jl_gc_alloc(ptls, sizeof(jl_typename_t),
                                    jl_typename_type);
    tn->name = name;
    tn->module = module;
    tn->wrapper = NULL;
    tn->cache = jl_emptysvec;
    tn->linearcache = jl_emptysvec;
    tn->names = NULL;
    tn->hash = bitmix(bitmix(module ? module->uuid : 0, name->hash), 0xa1ada1da);
    tn->mt = NULL;
    return tn;
}

JL_DLLEXPORT jl_typename_t *jl_new_typename(jl_sym_t *name)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return jl_new_typename_in(name, ptls->current_module);
}

// allocating DataTypes -----------------------------------------------------------

jl_datatype_t *jl_new_abstracttype(jl_value_t *name, jl_datatype_t *super, jl_svec_t *parameters)
{
    return jl_new_datatype((jl_sym_t*)name, super, parameters, jl_emptysvec, jl_emptysvec, 1, 0, 0);
}

jl_datatype_t *jl_new_uninitialized_datatype(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_datatype_t *t = (jl_datatype_t*)jl_gc_alloc(ptls, sizeof(jl_datatype_t), jl_datatype_type);
    t->depth = 0;
    t->hasfreetypevars = 0;
    t->isleaftype = 1;
    t->layout = NULL;
    return t;
}

static jl_datatype_layout_t *jl_get_layout(uint32_t nfields,
                                           uint32_t alignment,
                                           int haspadding,
                                           jl_fielddesc32_t desc[])
{
    // compute the smallest fielddesc type that can hold the layout description
    int fielddesc_type = 0;
    if (nfields > 0) {
        uint32_t max_size = 0;
        uint32_t max_offset = desc[nfields - 1].offset;
        for (size_t i = 0; i < nfields; i++) {
            if (desc[i].size > max_size)
                max_size = desc[i].size;
        }
        jl_fielddesc8_t maxdesc8 = { 0, max_size, max_offset };
        jl_fielddesc16_t maxdesc16 = { 0, max_size, max_offset };
        jl_fielddesc32_t maxdesc32 = { 0, max_size, max_offset };
        if (maxdesc8.size != max_size || maxdesc8.offset != max_offset) {
            fielddesc_type = 1;
            if (maxdesc16.size != max_size || maxdesc16.offset != max_offset) {
                fielddesc_type = 2;
                if (maxdesc32.size != max_size || maxdesc32.offset != max_offset) {
                    assert(0); // should have been verified by caller
                }
            }
        }
    }

    // allocate a new descriptor
    uint32_t fielddesc_size = jl_fielddesc_size(fielddesc_type);
    jl_datatype_layout_t *flddesc =
        (jl_datatype_layout_t*)jl_gc_perm_alloc(sizeof(jl_datatype_layout_t) + nfields * fielddesc_size);
    flddesc->nfields = nfields;
    flddesc->alignment = alignment;
    flddesc->haspadding = haspadding;
    flddesc->fielddesc_type = fielddesc_type;

    // fill out the fields of the new descriptor
    jl_fielddesc8_t* desc8 = (jl_fielddesc8_t*)jl_dt_layout_fields(flddesc);
    jl_fielddesc16_t* desc16 = (jl_fielddesc16_t*)jl_dt_layout_fields(flddesc);
    jl_fielddesc32_t* desc32 = (jl_fielddesc32_t*)jl_dt_layout_fields(flddesc);
    int ptrfree = 1;
    for (size_t i = 0; i < nfields; i++) {
        if (fielddesc_type == 0) {
            desc8[i].offset = desc[i].offset;
            desc8[i].size = desc[i].size;
            desc8[i].isptr = desc[i].isptr;
        }
        else if (fielddesc_type == 1) {
            desc16[i].offset = desc[i].offset;
            desc16[i].size = desc[i].size;
            desc16[i].isptr = desc[i].isptr;
        }
        else {
            desc32[i].offset = desc[i].offset;
            desc32[i].size = desc[i].size;
            desc32[i].isptr = desc[i].isptr;
        }
        if (desc[i].isptr)
            ptrfree = 0;
    }
    flddesc->pointerfree = ptrfree;
    return flddesc;
}

// Determine if homogeneous tuple with fields of type t will have
// a special alignment beyond normal Julia rules.
// Return special alignment if one exists, 0 if normal alignment rules hold.
// A non-zero result *must* match the LLVM rules for a vector type <nfields x t>.
// For sake of Ahead-Of-Time (AOT) compilation, this routine has to work
// without LLVM being available.
unsigned jl_special_vector_alignment(size_t nfields, jl_value_t *t)
{
    if (!jl_is_vecelement_type(t))
        return 0;
    // LLVM 3.7 and 3.8 either crash or generate wrong code for many
    // SIMD vector sizes N. It seems the rule is that N can have at
    // most 2 non-zero bits. (This is true at least for N<=100.) See
    // also <https://llvm.org/bugs/show_bug.cgi?id=27708>.
    size_t mask = nfields;
    // See e.g.
    // <https://graphics.stanford.edu/%7Eseander/bithacks.html> for an
    // explanation of this bit-counting algorithm.
    mask &= mask-1;             // clear least-significant 1 if present
    mask &= mask-1;             // clear another 1
    if (mask)
        return 0;               // nfields has more than two 1s
    assert(jl_datatype_nfields(t)==1);
    jl_value_t *ty = jl_field_type(t, 0);
    if (!jl_is_bitstype(ty))
        // LLVM requires that a vector element be a primitive type.
        // LLVM allows pointer types as vector elements, but until a
        // motivating use case comes up for Julia, we reject pointers.
        return 0;
    size_t elsz = jl_datatype_size(ty);
    if (elsz>8 || (1<<elsz & 0x116) == 0)
        // Element size is not 1, 2, 4, or 8.
        return 0;
    size_t size = nfields*elsz;
    // LLVM's alignment rule for vectors seems to be to round up to
    // a power of two, even if that's overkill for the target hardware.
    size_t alignment=1;
    for( ; size>alignment; alignment*=2 )
        continue;
    return alignment;
}

void jl_compute_field_offsets(jl_datatype_t *st)
{
    size_t sz = 0, alignm = 1;
    int homogeneous = 1;
    jl_value_t *lastty = NULL;

    uint64_t max_offset = (((uint64_t)1) << 32) - 1;
    uint64_t max_size = max_offset >> 1;

    uint32_t nfields = jl_svec_len(st->types);
    size_t descsz = nfields * sizeof(jl_fielddesc32_t);
    jl_fielddesc32_t *desc;
    if (descsz < jl_page_size)
        desc = (jl_fielddesc32_t*)alloca(descsz);
    else
        desc = (jl_fielddesc32_t*)malloc(descsz);
    int haspadding = 0;
    assert(st->name == jl_tuple_typename ||
           st == jl_sym_type ||
           st == jl_simplevector_type ||
           nfields != 0);

    for (size_t i = 0; i < nfields; i++) {
        jl_value_t *ty = jl_field_type(st, i);
        size_t fsz, al;
        if (jl_isbits(ty) && jl_is_leaf_type(ty) && ((jl_datatype_t*)ty)->layout) {
            fsz = jl_datatype_size(ty);
            // Should never happen
            if (__unlikely(fsz > max_size))
                goto throw_ovf;
            al = ((jl_datatype_t*)ty)->layout->alignment;
            desc[i].isptr = 0;
            if (((jl_datatype_t*)ty)->layout->haspadding)
                haspadding = 1;
        }
        else {
            fsz = sizeof(void*);
            if (fsz > MAX_ALIGN)
                fsz = MAX_ALIGN;
            al = fsz;
            desc[i].isptr = 1;
        }
        if (al != 0) {
            size_t alsz = LLT_ALIGN(sz, al);
            if (sz & (al - 1))
                haspadding = 1;
            sz = alsz;
            if (al > alignm)
                alignm = al;
        }
        homogeneous &= lastty==NULL || lastty==ty;
        lastty = ty;
        desc[i].offset = sz;
        desc[i].size = fsz;
        if (__unlikely(max_offset - sz < fsz))
            goto throw_ovf;
        sz += fsz;
    }
    if (homogeneous && lastty!=NULL && jl_is_tuple_type(st)) {
        // Some tuples become LLVM vectors with stronger alignment than what was calculated above.
        unsigned al = jl_special_vector_alignment(nfields, lastty);
        assert(al % alignm == 0);
        if (al)
            alignm = al;
    }
    st->size = LLT_ALIGN(sz, alignm);
    if (st->size > sz)
        haspadding = 1;
    st->layout = jl_get_layout(nfields, alignm, haspadding, desc);
    if (descsz >= jl_page_size) free(desc);
    return;
 throw_ovf:
    if (descsz >= jl_page_size) free(desc);
    jl_throw(jl_overflow_exception);
}

extern int jl_boot_file_loaded;

JL_DLLEXPORT jl_datatype_t *jl_new_datatype(jl_sym_t *name, jl_datatype_t *super,
                                            jl_svec_t *parameters,
                                            jl_svec_t *fnames, jl_svec_t *ftypes,
                                            int abstract, int mutabl,
                                            int ninitialized)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_datatype_t *t=NULL;
    jl_typename_t *tn=NULL;
    JL_GC_PUSH2(&t, &tn);

    if (!jl_boot_file_loaded && jl_is_symbol(name)) {
        // hack to avoid making two versions of basic types needed
        // during bootstrapping
        if (!strcmp(jl_symbol_name((jl_sym_t*)name), "Int32"))
            t = jl_int32_type;
        else if (!strcmp(jl_symbol_name((jl_sym_t*)name), "Int64"))
            t = jl_int64_type;
        else if (!strcmp(jl_symbol_name((jl_sym_t*)name), "Bool"))
            t = jl_bool_type;
        else if (!strcmp(jl_symbol_name((jl_sym_t*)name), "UInt8"))
            t = jl_uint8_type;
    }
    if (t == NULL)
        t = jl_new_uninitialized_datatype();
    else
        tn = t->name;
    // init before possibly calling jl_new_typename
    t->super = super;
    if (super != NULL) jl_gc_wb(t, t->super);
    t->parameters = parameters;
    jl_gc_wb(t, t->parameters);
    t->types = ftypes;
    if (ftypes != NULL) jl_gc_wb(t, t->types);
    t->abstract = abstract;
    t->mutabl = mutabl;
    t->ninitialized = ninitialized;
    t->instance = NULL;
    t->struct_decl = NULL;
    t->ditype = NULL;
    t->size = 0;

    if (tn == NULL) {
        t->name = NULL;
        if (jl_is_typename(name)) {
            tn = (jl_typename_t*)name;
        }
        else {
            tn = jl_new_typename((jl_sym_t*)name);
            if (!abstract) {
                tn->mt = jl_new_method_table(name, ptls->current_module);
                jl_gc_wb(tn, tn->mt);
            }
        }
        t->name = tn;
        jl_gc_wb(t, t->name);
    }
    t->name->names = fnames;
    jl_gc_wb(t->name, t->name->names);

    if (t->name->wrapper == NULL) {
        t->name->wrapper = (jl_value_t*)t;
        jl_gc_wb(t->name, t);
        int i;
        int np = jl_svec_len(parameters);
        for (i=np-1; i >= 0; i--) {
            t->name->wrapper = jl_new_struct(jl_unionall_type, jl_svecref(parameters,i), t->name->wrapper);
            jl_gc_wb(t->name, t->name->wrapper);
        }
    }
    jl_precompute_memoized_dt(t);

    if (abstract || jl_svec_len(parameters) > 0) {
        t->uid = 0;
    }
    else {
        t->uid = jl_assign_type_uid();
        if (t->types != NULL && t->isleaftype) {
            static const jl_datatype_layout_t singleton_layout = {0, 1, 0, 1, 0};
            if (fnames == jl_emptysvec)
                t->layout = &singleton_layout;
            else
                jl_compute_field_offsets(t);
        }
    }
    JL_GC_POP();
    return t;
}

JL_DLLEXPORT jl_datatype_t *jl_new_bitstype(jl_value_t *name, jl_datatype_t *super,
                                            jl_svec_t *parameters, size_t nbits)
{
    jl_datatype_t *bt = jl_new_datatype((jl_sym_t*)name, super, parameters,
                                        jl_emptysvec, jl_emptysvec, 0, 0, 0);
    uint32_t nbytes = (nbits + 7) / 8;
    uint32_t alignm = next_power_of_two(nbytes);
    if (alignm > MAX_ALIGN)
        alignm = MAX_ALIGN;
    bt->size = nbytes;
    bt->layout = jl_get_layout(0, alignm, 0, NULL);
    return bt;
}

// bits constructors ----------------------------------------------------------

typedef struct {
    int64_t a;
    int64_t b;
} bits128_t;

// Note that this function updates len
static jl_value_t *jl_new_bits_internal(jl_value_t *dt, void *data, size_t *len)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(jl_is_datatype(dt));
    jl_datatype_t *bt = (jl_datatype_t*)dt;
    size_t nb = jl_datatype_size(bt);
    if (nb == 0)
        return jl_new_struct_uninit(bt);
    *len = LLT_ALIGN(*len, bt->layout->alignment);
    data = (char*)data + (*len);
    *len += nb;
    if (bt == jl_uint8_type)   return jl_box_uint8(*(uint8_t*)data);
    if (bt == jl_int64_type)   return jl_box_int64(*(int64_t*)data);
    if (bt == jl_bool_type)    return (*(int8_t*)data) ? jl_true:jl_false;
    if (bt == jl_int32_type)   return jl_box_int32(*(int32_t*)data);
    if (bt == jl_float64_type) return jl_box_float64(*(double*)data);

    jl_value_t *v = jl_gc_alloc(ptls, nb, bt);
    switch (nb) {
    case  1: *(int8_t*)   jl_data_ptr(v) = *(int8_t*)data;    break;
    case  2: *(int16_t*)  jl_data_ptr(v) = *(int16_t*)data;   break;
    case  4: *(int32_t*)  jl_data_ptr(v) = *(int32_t*)data;   break;
    case  8: *(int64_t*)  jl_data_ptr(v) = *(int64_t*)data;   break;
    case 16: *(bits128_t*)jl_data_ptr(v) = *(bits128_t*)data; break;
    default: memcpy(jl_data_ptr(v), data, nb);
    }
    return v;
}

JL_DLLEXPORT jl_value_t *jl_new_bits(jl_value_t *bt, void *data)
{
    size_t len = 0;
    return jl_new_bits_internal(bt, data, &len);
}

void jl_assign_bits(void *dest, jl_value_t *bits)
{
    size_t nb = jl_datatype_size(jl_typeof(bits));
    if (nb == 0) return;
    switch (nb) {
    case  1: *(int8_t*)dest    = *(int8_t*)jl_data_ptr(bits);    break;
    case  2: *(int16_t*)dest   = *(int16_t*)jl_data_ptr(bits);   break;
    case  4: *(int32_t*)dest   = *(int32_t*)jl_data_ptr(bits);   break;
    case  8: *(int64_t*)dest   = *(int64_t*)jl_data_ptr(bits);   break;
    case 16: *(bits128_t*)dest = *(bits128_t*)jl_data_ptr(bits); break;
    default: memcpy(dest, jl_data_ptr(bits), nb);
    }
}

#define BOXN_FUNC(nb,nw)                                                \
    JL_DLLEXPORT jl_value_t *jl_box##nb(jl_datatype_t *t, int##nb##_t x) \
    {                                                                   \
        jl_ptls_t ptls = jl_get_ptls_states();                          \
        assert(jl_isbits(t));                                           \
        assert(jl_datatype_size(t) == sizeof(x));                       \
        jl_value_t *v = jl_gc_alloc(ptls, nw * sizeof(void*), t);       \
        *(int##nb##_t*)jl_data_ptr(v) = x;                              \
        return v;                                                       \
    }
BOXN_FUNC(8,  1)
BOXN_FUNC(16, 1)
BOXN_FUNC(32, 1)
#ifdef _P64
BOXN_FUNC(64, 1)
#else
BOXN_FUNC(64, 2)
#endif

#define UNBOX_FUNC(j_type,c_type)                                       \
    JL_DLLEXPORT c_type jl_unbox_##j_type(jl_value_t *v)                \
    {                                                                   \
        assert(jl_is_bitstype(jl_typeof(v)));                           \
        assert(jl_datatype_size(jl_typeof(v)) == sizeof(c_type));       \
        return *(c_type*)jl_data_ptr(v);                                \
    }
UNBOX_FUNC(int8,   int8_t)
UNBOX_FUNC(uint8,  uint8_t)
UNBOX_FUNC(int16,  int16_t)
UNBOX_FUNC(uint16, uint16_t)
UNBOX_FUNC(int32,  int32_t)
UNBOX_FUNC(uint32, uint32_t)
UNBOX_FUNC(int64,  int64_t)
UNBOX_FUNC(uint64, uint64_t)
UNBOX_FUNC(bool,   int8_t)
UNBOX_FUNC(float32, float)
UNBOX_FUNC(float64, double)
UNBOX_FUNC(voidpointer, void*)

#define BOX_FUNC(typ,c_type,pfx,nw)                             \
    JL_DLLEXPORT jl_value_t *pfx##_##typ(c_type x)              \
    {                                                           \
        jl_ptls_t ptls = jl_get_ptls_states();                  \
        jl_value_t *v = jl_gc_alloc(ptls, nw * sizeof(void*),   \
                                    jl_##typ##_type);           \
        *(c_type*)jl_data_ptr(v) = x;                           \
        return v;                                               \
    }
BOX_FUNC(float32, float,  jl_box, 1)
BOX_FUNC(voidpointer, void*,  jl_box, 1)
#ifdef _P64
BOX_FUNC(float64, double, jl_box, 1)
#else
BOX_FUNC(float64, double, jl_box, 2)
#endif

#define NBOX_C 1024

#define SIBOX_FUNC(typ,c_type,nw)\
    static jl_value_t *boxed_##typ##_cache[NBOX_C];             \
    JL_DLLEXPORT jl_value_t *jl_box_##typ(c_type x)             \
    {                                                           \
        jl_ptls_t ptls = jl_get_ptls_states();                  \
        c_type idx = x+NBOX_C/2;                                \
        if ((u##c_type)idx < (u##c_type)NBOX_C)                 \
            return boxed_##typ##_cache[idx];                    \
        jl_value_t *v = jl_gc_alloc(ptls, nw * sizeof(void*),   \
                                    jl_##typ##_type);           \
        *(c_type*)jl_data_ptr(v) = x;                           \
        return v;                                               \
    }
#define UIBOX_FUNC(typ,c_type,nw)                               \
    static jl_value_t *boxed_##typ##_cache[NBOX_C];             \
    JL_DLLEXPORT jl_value_t *jl_box_##typ(c_type x)             \
    {                                                           \
        jl_ptls_t ptls = jl_get_ptls_states();                  \
        if (x < NBOX_C)                                         \
            return boxed_##typ##_cache[x];                      \
        jl_value_t *v = jl_gc_alloc(ptls, nw * sizeof(void*),   \
                                    jl_##typ##_type);           \
        *(c_type*)jl_data_ptr(v) = x;                           \
        return v;                                               \
    }
SIBOX_FUNC(int16,  int16_t, 1)
SIBOX_FUNC(int32,  int32_t, 1)
UIBOX_FUNC(uint16, uint16_t, 1)
UIBOX_FUNC(uint32, uint32_t, 1)
UIBOX_FUNC(char,   uint32_t, 1)
UIBOX_FUNC(ssavalue, size_t, 1)
UIBOX_FUNC(slotnumber, size_t, 1)
#ifdef _P64
SIBOX_FUNC(int64,  int64_t, 1)
UIBOX_FUNC(uint64, uint64_t, 1)
#else
SIBOX_FUNC(int64,  int64_t, 2)
UIBOX_FUNC(uint64, uint64_t, 2)
#endif

static jl_value_t *boxed_int8_cache[256];
JL_DLLEXPORT jl_value_t *jl_box_int8(int8_t x)
{
    return boxed_int8_cache[(uint8_t)x];
}
static jl_value_t *boxed_uint8_cache[256];
JL_DLLEXPORT jl_value_t *jl_box_uint8(uint8_t x)
{
    return boxed_uint8_cache[x];
}

void jl_init_int32_int64_cache(void)
{
    int64_t i;
    for(i=0; i < NBOX_C; i++) {
        boxed_int32_cache[i]  = jl_box32(jl_int32_type, i-NBOX_C/2);
        boxed_int64_cache[i]  = jl_box64(jl_int64_type, i-NBOX_C/2);
#ifdef _P64
        boxed_ssavalue_cache[i] = jl_box64(jl_ssavalue_type, i);
        boxed_slotnumber_cache[i] = jl_box64(jl_slotnumber_type, i);
#else
        boxed_ssavalue_cache[i] = jl_box32(jl_ssavalue_type, i);
        boxed_slotnumber_cache[i] = jl_box32(jl_slotnumber_type, i);
#endif
    }
    for(i=0; i < 256; i++) {
        boxed_uint8_cache[i] = jl_box8(jl_uint8_type, i);
    }
}

void jl_init_box_caches(void)
{
    int64_t i;
    for(i=0; i < 256; i++) {
        boxed_int8_cache[i]  = jl_box8(jl_int8_type, i);
    }
    for(i=0; i < NBOX_C; i++) {
        boxed_int16_cache[i]  = jl_box16(jl_int16_type, i-NBOX_C/2);
        boxed_uint16_cache[i] = jl_box16(jl_uint16_type, i);
        boxed_uint32_cache[i] = jl_box32(jl_uint32_type, i);
        boxed_char_cache[i]   = jl_box32(jl_char_type, i);
        boxed_uint64_cache[i] = jl_box64(jl_uint64_type, i);
    }
}

void jl_mark_box_caches(jl_ptls_t ptls)
{
    int64_t i;
    for(i=0; i < 256; i++) {
        jl_gc_setmark(ptls, boxed_int8_cache[i]);
        jl_gc_setmark(ptls, boxed_uint8_cache[i]);
    }
    for(i=0; i < NBOX_C; i++) {
        jl_gc_setmark(ptls, boxed_int16_cache[i]);
        jl_gc_setmark(ptls, boxed_int32_cache[i]);
        jl_gc_setmark(ptls, boxed_int64_cache[i]);
        jl_gc_setmark(ptls, boxed_uint16_cache[i]);
        jl_gc_setmark(ptls, boxed_uint32_cache[i]);
        jl_gc_setmark(ptls, boxed_char_cache[i]);
        jl_gc_setmark(ptls, boxed_uint64_cache[i]);
        jl_gc_setmark(ptls, boxed_ssavalue_cache[i]);
        jl_gc_setmark(ptls, boxed_slotnumber_cache[i]);
    }
}

JL_DLLEXPORT jl_value_t *jl_box_bool(int8_t x)
{
    if (x)
        return jl_true;
    return jl_false;
}

// struct constructors --------------------------------------------------------

JL_DLLEXPORT jl_value_t *jl_new_struct(jl_datatype_t *type, ...)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (type->instance != NULL) return type->instance;
    va_list args;
    size_t nf = jl_datatype_nfields(type);
    va_start(args, type);
    jl_value_t *jv = jl_gc_alloc(ptls, jl_datatype_size(type), type);
    for(size_t i=0; i < nf; i++) {
        jl_set_nth_field(jv, i, va_arg(args, jl_value_t*));
    }
    va_end(args);
    return jv;
}

JL_DLLEXPORT jl_value_t *jl_new_structv(jl_datatype_t *type, jl_value_t **args,
                                        uint32_t na)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (type->instance != NULL) return type->instance;
    size_t nf = jl_datatype_nfields(type);
    jl_value_t *jv = jl_gc_alloc(ptls, jl_datatype_size(type), type);
    for(size_t i=0; i < na; i++) {
        jl_set_nth_field(jv, i, args[i]);
    }
    for(size_t i=na; i < nf; i++) {
        if (jl_field_isptr(type, i)) {
            *(jl_value_t**)((char*)jl_data_ptr(jv)+jl_field_offset(type,i)) = NULL;
        }
    }
    return jv;
}

JL_DLLEXPORT jl_value_t *jl_new_struct_uninit(jl_datatype_t *type)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (type->instance != NULL) return type->instance;
    size_t size = jl_datatype_size(type);
    jl_value_t *jv = jl_gc_alloc(ptls, size, type);
    if (size > 0)
        memset(jl_data_ptr(jv), 0, size);
    return jv;
}

// field access ---------------------------------------------------------------

JL_DLLEXPORT int jl_field_index(jl_datatype_t *t, jl_sym_t *fld, int err)
{
    jl_svec_t *fn = t->name->names;
    for(size_t i=0; i < jl_svec_len(fn); i++) {
        if (jl_svecref(fn,i) == (jl_value_t*)fld) {
            return (int)i;
        }
    }
    if (err)
        jl_errorf("type %s has no field %s", jl_symbol_name(t->name->name),
                  jl_symbol_name(fld));
    return -1;
}

JL_DLLEXPORT jl_value_t *jl_get_nth_field(jl_value_t *v, size_t i)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    assert(i < jl_datatype_nfields(st));
    size_t offs = jl_field_offset(st,i);
    if (jl_field_isptr(st,i)) {
        return *(jl_value_t**)((char*)v + offs);
    }
    return jl_new_bits(jl_field_type(st,i), (char*)v + offs);
}

JL_DLLEXPORT jl_value_t *jl_get_nth_field_checked(jl_value_t *v, size_t i)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    if (i >= jl_datatype_nfields(st))
        jl_bounds_error_int(v, i+1);
    size_t offs = jl_field_offset(st,i);
    if (jl_field_isptr(st,i)) {
        jl_value_t *fval = *(jl_value_t**)((char*)v + offs);
        if (fval == NULL)
            jl_throw(jl_undefref_exception);
        return fval;
    }
    return jl_new_bits(jl_field_type(st,i), (char*)v + offs);
}

JL_DLLEXPORT void jl_set_nth_field(jl_value_t *v, size_t i, jl_value_t *rhs)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t offs = jl_field_offset(st,i);
    if (jl_field_isptr(st,i)) {
        *(jl_value_t**)((char*)v + offs) = rhs;
        if (rhs != NULL) jl_gc_wb(v, rhs);
    }
    else {
        jl_assign_bits((char*)v + offs, rhs);
    }
}

JL_DLLEXPORT int jl_field_isdefined(jl_value_t *v, size_t i)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t offs = jl_field_offset(st,i);
    if (jl_field_isptr(st,i)) {
        return *(jl_value_t**)((char*)v + offs) != NULL;
    }
    return 1;
}

JL_DLLEXPORT size_t jl_get_field_offset(jl_datatype_t *ty, int field)
{
    if (ty->layout == NULL || field > jl_datatype_nfields(ty) || field < 1)
        jl_bounds_error_int((jl_value_t*)ty, field);
    return jl_field_offset(ty, field - 1);
}

JL_DLLEXPORT size_t jl_get_alignment(jl_datatype_t *ty)
{
    if (ty->layout == NULL)
        jl_error("non-leaf type doesn't have an alignment");
    return ty->layout->alignment;
}

#ifdef __cplusplus
}
#endif
