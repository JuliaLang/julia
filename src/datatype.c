// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  defining DataTypes
  basic operations on struct and bits values
*/

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

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
    tn->hash = bitmix(bitmix(module ? module->build_id : 0, name->hash), 0xa1ada1da);
    tn->mt = NULL;
    return tn;
}

// allocating DataTypes -----------------------------------------------------------

jl_datatype_t *jl_new_abstracttype(jl_value_t *name, jl_module_t *module, jl_datatype_t *super, jl_svec_t *parameters)
{
    return jl_new_datatype((jl_sym_t*)name, module, super, parameters, jl_emptysvec, jl_emptysvec, 1, 0, 0);
}

jl_datatype_t *jl_new_uninitialized_datatype(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_datatype_t *t = (jl_datatype_t*)jl_gc_alloc(ptls, sizeof(jl_datatype_t), jl_datatype_type);
    t->hasfreetypevars = 0;
    t->isdispatchtuple = 0;
    t->isbitstype = 0;
    t->zeroinit = 0;
    t->isinlinealloc = 0;
    t->layout = NULL;
    t->names = NULL;
    return t;
}

static jl_datatype_layout_t *jl_get_layout(uint32_t nfields,
                                           uint32_t alignment,
                                           int haspadding,
                                           jl_fielddesc32_t desc[])
{
    // compute the smallest fielddesc type that can hold the layout description
    int fielddesc_type = 0;
    uint32_t npointers = 0;
    // First pointer field
    uint32_t first_ptr = (uint32_t)-1;
    // Last pointer field
    uint32_t last_ptr = 0;
    if (nfields > 0) {
        uint32_t max_size = 0;
        uint32_t max_offset = desc[nfields - 1].offset;
        for (size_t i = 0; i < nfields; i++) {
            if (desc[i].size > max_size)
                max_size = desc[i].size;
            if (desc[i].isptr) {
                npointers++;
                if (first_ptr == (uint32_t)-1)
                    first_ptr = i;
                last_ptr = i;
            }
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
    int has_padding = nfields && npointers;
    jl_datatype_layout_t *flddesc =
        (jl_datatype_layout_t*)jl_gc_perm_alloc(sizeof(jl_datatype_layout_t) +
                                                nfields * fielddesc_size +
                                                (has_padding ? sizeof(uint32_t) : 0), 0, 4, 0);
    if (has_padding) {
        if (first_ptr > UINT16_MAX)
            first_ptr = UINT16_MAX;
        last_ptr = nfields - last_ptr - 1;
        if (last_ptr > UINT16_MAX)
            last_ptr = UINT16_MAX;
        flddesc = (jl_datatype_layout_t*)(((char*)flddesc) + sizeof(uint32_t));
        jl_datatype_layout_n_nonptr(flddesc) = (first_ptr << 16) | last_ptr;
    }
    flddesc->nfields = nfields;
    flddesc->alignment = alignment;
    flddesc->haspadding = haspadding;
    flddesc->fielddesc_type = fielddesc_type;

    // fill out the fields of the new descriptor
    jl_fielddesc8_t* desc8 = (jl_fielddesc8_t*)jl_dt_layout_fields(flddesc);
    jl_fielddesc16_t* desc16 = (jl_fielddesc16_t*)jl_dt_layout_fields(flddesc);
    jl_fielddesc32_t* desc32 = (jl_fielddesc32_t*)jl_dt_layout_fields(flddesc);
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
    }
    uint32_t nexp = 0;
    while (npointers >= 0x10000) {
        nexp++;
        npointers = npointers >> 1;
    }
    flddesc->npointers = npointers | (nexp << 16);
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
    if (!jl_is_primitivetype(ty))
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

STATIC_INLINE int jl_is_datatype_make_singleton(jl_datatype_t *d)
{
    return (!d->abstract && jl_datatype_size(d) == 0 && d != jl_sym_type && d->name != jl_array_typename &&
            d->uid != 0 && !d->mutabl);
}

STATIC_INLINE void jl_allocate_singleton_instance(jl_datatype_t *st)
{
    if (jl_is_datatype_make_singleton(st)) {
        st->instance = jl_gc_alloc(jl_get_ptls_states(), 0, st);
        jl_gc_wb(st, st->instance);
    }
}

static unsigned union_isbits(jl_value_t *ty, size_t *nbytes, size_t *align)
{
    if (jl_is_uniontype(ty)) {
        unsigned na = union_isbits(((jl_uniontype_t*)ty)->a, nbytes, align);
        if (na == 0)
            return 0;
        unsigned nb = union_isbits(((jl_uniontype_t*)ty)->b, nbytes, align);
        if (nb == 0)
            return 0;
        return na + nb;
    }
    if (jl_isbits(ty)) {
        size_t sz = jl_datatype_size(ty);
        size_t al = jl_datatype_align(ty);
        if (*nbytes < sz)
            *nbytes = sz;
        if (*align < al)
            *align = al;
        return 1;
    }
    return 0;
}

JL_DLLEXPORT int jl_islayout_inline(jl_value_t *eltype, size_t *fsz, size_t *al)
{
    unsigned countbits = union_isbits(eltype, fsz, al);
    return (countbits > 0 && countbits < 127) ? countbits : 0;
}

static int references_name(jl_value_t *p, jl_typename_t *name)
{
    if (jl_is_uniontype(p))
        return references_name(((jl_uniontype_t*)p)->a, name) ||
               references_name(((jl_uniontype_t*)p)->b, name);
    if (jl_is_unionall(p))
        return references_name((jl_value_t*)((jl_unionall_t*)p)->var, name) ||
               references_name(((jl_unionall_t*)p)->body, name);
    if (jl_is_typevar(p))
        return references_name(((jl_tvar_t*)p)->ub, name) ||
               references_name(((jl_tvar_t*)p)->lb, name);
    if (jl_is_datatype(p)) {
        if (((jl_datatype_t*)p)->name == name)
            return 1;
        size_t i, l = jl_nparams(p);
        for (i = 0; i < l; i++) {
            if (references_name(jl_tparam(p, i), name))
                return 1;
        }
    }
    return 0;
}

void jl_compute_field_offsets(jl_datatype_t *st)
{
    size_t sz = 0, alignm = 1;
    int homogeneous = 1;
    jl_value_t *lastty = NULL;
    uint64_t max_offset = (((uint64_t)1) << 32) - 1;
    uint64_t max_size = max_offset >> 1;

    if (st->name->wrapper) {
        jl_datatype_t *w = (jl_datatype_t*)jl_unwrap_unionall(st->name->wrapper);
        // compute whether this type can be inlined
        // based on whether its definition is self-referential
        if (w->types != NULL) {
            st->isbitstype = st->isconcretetype && !st->mutabl;
            size_t i, nf = jl_field_count(st);
            for (i = 0; i < nf; i++) {
                jl_value_t *fld = jl_field_type(st, i);
                if (st->isbitstype)
                    st->isbitstype = jl_is_datatype(fld) && ((jl_datatype_t*)fld)->isbitstype;
                if (!st->zeroinit)
                    st->zeroinit = (jl_is_datatype(fld) && ((jl_datatype_t*)fld)->isinlinealloc) ? ((jl_datatype_t*)fld)->zeroinit : 1;
            }
            if (st->isbitstype) {
                st->isinlinealloc = 1;
                size_t i, nf = jl_field_count(w);
                for (i = 0; i < nf; i++) {
                    jl_value_t *fld = jl_field_type(w, i);
                    if (references_name(fld, w->name)) {
                        st->isinlinealloc = 0;
                        st->isbitstype = 0;
                        st->zeroinit = 1;
                        break;
                    }
                }
            }
        }
        // If layout doesn't depend on type parameters, it's stored in st->name->wrapper
        // and reused by all subtypes.
        if (st != w && // this check allows us to re-compute layout for some types during init
                w->layout) {
            st->layout = w->layout;
            st->size = w->size;
            jl_allocate_singleton_instance(st);
            return;
        }
    }
    if (st->types == NULL || (jl_is_namedtuple_type(st) && !jl_is_concrete_type((jl_value_t*)st)))
        return;
    uint32_t nfields = jl_svec_len(st->types);
    if (nfields == 0) {
        if (st == jl_sym_type || st == jl_string_type) {
            // opaque layout - heap-allocated blob
            static const jl_datatype_layout_t opaque_byte_layout = {0, 1, 0, 1, 0};
            st->layout = &opaque_byte_layout;
        }
        else if (st == jl_simplevector_type || st->name == jl_array_typename) {
            static const jl_datatype_layout_t opaque_ptr_layout = {0, sizeof(void*), 0, 1, 0};
            st->layout = &opaque_ptr_layout;
        }
        else {
            // reuse the same layout for all singletons
            static const jl_datatype_layout_t singleton_layout = {0, 1, 0, 0, 0};
            st->layout = &singleton_layout;
            jl_allocate_singleton_instance(st);
        }
        return;
    }
    if (!jl_is_concrete_type((jl_value_t*)st)) {
        // compute layout whenever field types have no free variables
        for (size_t i = 0; i < nfields; i++) {
            if (jl_has_free_typevars(jl_field_type(st, i)))
                return;
        }
    }

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
        size_t fsz = 0, al = 0;
        if (jl_islayout_inline(ty, &fsz, &al)) {
            if (__unlikely(fsz > max_size))
                // Should never happen
                goto throw_ovf;
            desc[i].isptr = 0;
            if (jl_is_uniontype(ty)) {
                haspadding = 1;
                fsz += 1; // selector byte
            }
            else { // isbits struct
                if (((jl_datatype_t*)ty)->layout->haspadding)
                    haspadding = 1;
            }
        }
        else {
            fsz = sizeof(void*);
            if (fsz > MAX_ALIGN)
                fsz = MAX_ALIGN;
            al = fsz;
            desc[i].isptr = 1;
        }
        assert(al <= JL_HEAP_ALIGNMENT && (JL_HEAP_ALIGNMENT % al) == 0);
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
    if (homogeneous && lastty != NULL && jl_is_tuple_type(st)) {
        // Some tuples become LLVM vectors with stronger alignment than what was calculated above.
        unsigned al = jl_special_vector_alignment(nfields, lastty);
        assert(al % alignm == 0);
        // JL_HEAP_ALIGNMENT is the biggest alignment we can guarantee on the heap.
        if (al > JL_HEAP_ALIGNMENT)
            alignm = JL_HEAP_ALIGNMENT;
        else if (al)
            alignm = al;
    }
    st->size = LLT_ALIGN(sz, alignm);
    if (st->size > sz)
        haspadding = 1;
    st->layout = jl_get_layout(nfields, alignm, haspadding, desc);
    if (descsz >= jl_page_size) free(desc);
    jl_allocate_singleton_instance(st);
    return;
 throw_ovf:
    if (descsz >= jl_page_size) free(desc);
    jl_errorf("type %s has field offset %d that exceeds the page size", jl_symbol_name(st->name->name), descsz);
}

JL_DLLEXPORT jl_datatype_t *jl_new_datatype(
        jl_sym_t *name,
        jl_module_t *module,
        jl_datatype_t *super,
        jl_svec_t *parameters,
        jl_svec_t *fnames,
        jl_svec_t *ftypes,
        int abstract, int mutabl,
        int ninitialized)
{
    jl_datatype_t *t = NULL;
    jl_typename_t *tn = NULL;
    JL_GC_PUSH2(&t, &tn);

    if (t == NULL)
        t = jl_new_uninitialized_datatype();
    else
        tn = t->name;
    // init before possibly calling jl_new_typename_in
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
            tn = jl_new_typename_in((jl_sym_t*)name, module);
            if (!abstract) {
                tn->mt = jl_new_method_table(name, module);
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

    t->uid = 0;
    if (!abstract) {
        if (jl_svec_len(parameters) == 0)
            t->uid = jl_assign_type_uid();
        jl_compute_field_offsets(t);
    }
    JL_GC_POP();
    return t;
}

JL_DLLEXPORT jl_datatype_t *jl_new_primitivetype(jl_value_t *name, jl_module_t *module,
                                                 jl_datatype_t *super,
                                                 jl_svec_t *parameters, size_t nbits)
{
    jl_datatype_t *bt = jl_new_datatype((jl_sym_t*)name, module, super, parameters,
                                        jl_emptysvec, jl_emptysvec, 0, 0, 0);
    uint32_t nbytes = (nbits + 7) / 8;
    uint32_t alignm = next_power_of_two(nbytes);
    if (alignm > MAX_ALIGN)
        alignm = MAX_ALIGN;
    bt->isbitstype = bt->isinlinealloc = (parameters == jl_emptysvec);
    bt->size = nbytes;
    bt->layout = jl_get_layout(0, alignm, 0, NULL);
    bt->instance = NULL;
    return bt;
}

// bits constructors ----------------------------------------------------------

JL_DLLEXPORT jl_value_t *jl_new_bits(jl_value_t *dt, void *data)
{
    // data may not have the alignment required by the size
    // but will always have the alignment required by the datatype
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(jl_is_datatype(dt));
    jl_datatype_t *bt = (jl_datatype_t*)dt;
    size_t nb = jl_datatype_size(bt);
    // some types have special pools to minimize allocations
    if (nb == 0)               return jl_new_struct_uninit(bt); // returns bt->instance
    if (bt == jl_bool_type)    return (1 & *(int8_t*)data) ? jl_true : jl_false;
    if (bt == jl_uint8_type)   return jl_box_uint8(*(uint8_t*)data);
    if (bt == jl_int64_type)   return jl_box_int64(*(int64_t*)data);
    if (bt == jl_int32_type)   return jl_box_int32(*(int32_t*)data);
    if (bt == jl_int8_type)    return jl_box_int8(*(int8_t*)data);
    if (bt == jl_int16_type)   return jl_box_int16(*(int16_t*)data);
    if (bt == jl_uint64_type)  return jl_box_uint64(*(uint64_t*)data);
    if (bt == jl_uint32_type)  return jl_box_uint32(*(uint32_t*)data);
    if (bt == jl_uint16_type)  return jl_box_uint16(*(uint16_t*)data);
    if (bt == jl_char_type)    return jl_box_char(*(uint32_t*)data);

    jl_value_t *v = jl_gc_alloc(ptls, nb, bt);
    switch (nb) {
    case  1: *(uint8_t*) v = *(uint8_t*)data;    break;
    case  2: *(uint16_t*)v = jl_load_unaligned_i16(data);   break;
    case  4: *(uint32_t*)v = jl_load_unaligned_i32(data);   break;
    case  8: *(uint64_t*)v = jl_load_unaligned_i64(data);   break;
    case 16:
        memcpy(jl_assume_aligned(v, 16), data, 16);
        break;
    default: memcpy(v, data, nb);
    }
    return v;
}

// used by boot.jl
JL_DLLEXPORT jl_value_t *jl_typemax_uint(jl_value_t *bt)
{
    uint64_t data = 0xffffffffffffffffULL;
    jl_value_t *v = jl_gc_alloc(jl_get_ptls_states(), sizeof(size_t), bt);
    memcpy(v, &data, sizeof(size_t));
    return v;
}

void jl_assign_bits(void *dest, jl_value_t *bits)
{
    // bits must be a heap box.
    size_t nb = jl_datatype_size(jl_typeof(bits));
    if (nb == 0) return;
    switch (nb) {
    case  1: *(uint8_t*)dest    = *(uint8_t*)bits;    break;
    case  2: jl_store_unaligned_i16(dest, *(uint16_t*)bits); break;
    case  4: jl_store_unaligned_i32(dest, *(uint32_t*)bits); break;
    case  8: jl_store_unaligned_i64(dest, *(uint64_t*)bits); break;
    case 16:
        memcpy(dest, jl_assume_aligned(bits, 16), 16);
        break;
    default: memcpy(dest, bits, nb);
    }
}

#define PERMBOXN_FUNC(nb,nw)                                            \
    jl_value_t *jl_permbox##nb(jl_datatype_t *t, int##nb##_t x)         \
    {                                                                   \
        assert(jl_isbits(t));                                           \
        assert(jl_datatype_size(t) == sizeof(x));                       \
        jl_value_t *v = jl_gc_permobj(nw * sizeof(void*), t);           \
        *(int##nb##_t*)jl_data_ptr(v) = x;                              \
        return v;                                                       \
    }
PERMBOXN_FUNC(8,  1)
PERMBOXN_FUNC(16, 1)
PERMBOXN_FUNC(32, 1)
#ifdef _P64
PERMBOXN_FUNC(64, 1)
#else
PERMBOXN_FUNC(64, 2)
#endif

#define UNBOX_FUNC(j_type,c_type)                                       \
    JL_DLLEXPORT c_type jl_unbox_##j_type(jl_value_t *v)                \
    {                                                                   \
        assert(jl_is_primitivetype(jl_typeof(v)));                           \
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
UIBOX_FUNC(ssavalue, size_t, 1)
UIBOX_FUNC(slotnumber, size_t, 1)
#ifdef _P64
SIBOX_FUNC(int64,  int64_t, 1)
UIBOX_FUNC(uint64, uint64_t, 1)
#else
SIBOX_FUNC(int64,  int64_t, 2)
UIBOX_FUNC(uint64, uint64_t, 2)
#endif

static jl_value_t *boxed_char_cache[128];
JL_DLLEXPORT jl_value_t *jl_box_char(uint32_t x)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (0 < (int32_t)x)
        return boxed_char_cache[x >> 24];
    jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), jl_char_type);
    *(uint32_t*)jl_data_ptr(v) = x;
    return v;
}

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
        boxed_int32_cache[i]  = jl_permbox32(jl_int32_type, i-NBOX_C/2);
        boxed_int64_cache[i]  = jl_permbox64(jl_int64_type, i-NBOX_C/2);
#ifdef _P64
        boxed_ssavalue_cache[i] = jl_permbox64(jl_ssavalue_type, i);
        boxed_slotnumber_cache[i] = jl_permbox64(jl_slotnumber_type, i);
#else
        boxed_ssavalue_cache[i] = jl_permbox32(jl_ssavalue_type, i);
        boxed_slotnumber_cache[i] = jl_permbox32(jl_slotnumber_type, i);
#endif
    }
    for(i=0; i < 256; i++) {
        boxed_uint8_cache[i] = jl_permbox8(jl_uint8_type, i);
    }
}

void jl_init_box_caches(void)
{
    int64_t i;
    for(i=0; i < 128; i++) {
        boxed_char_cache[i] = jl_permbox32(jl_char_type, i << 24);
    }
    for(i=0; i < 256; i++) {
        boxed_int8_cache[i] = jl_permbox8(jl_int8_type, i);
    }
    for(i=0; i < NBOX_C; i++) {
        boxed_int16_cache[i]  = jl_permbox16(jl_int16_type, i-NBOX_C/2);
        boxed_uint16_cache[i] = jl_permbox16(jl_uint16_type, i);
        boxed_uint32_cache[i] = jl_permbox32(jl_uint32_type, i);
        boxed_uint64_cache[i] = jl_permbox64(jl_uint64_type, i);
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
    for (size_t i = 0; i < nf; i++) {
        jl_set_nth_field(jv, i, va_arg(args, jl_value_t*));
    }
    va_end(args);
    return jv;
}

JL_DLLEXPORT jl_value_t *jl_new_structv(jl_datatype_t *type, jl_value_t **args,
                                        uint32_t na)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (type->instance != NULL) {
        for (size_t i = 0; i < na; i++) {
            jl_value_t *ft = jl_field_type(type, i);
            if (!jl_isa(args[i], ft))
                jl_type_error("new", ft, args[i]);
        }
        return type->instance;
    }
    if (type->layout == NULL)
        jl_type_error("new", (jl_value_t*)jl_datatype_type, (jl_value_t*)type);
    size_t nf = jl_datatype_nfields(type);
    jl_value_t *jv = jl_gc_alloc(ptls, jl_datatype_size(type), type);
    JL_GC_PUSH1(&jv);
    for (size_t i = 0; i < na; i++) {
        jl_value_t *ft = jl_field_type(type, i);
        if (!jl_isa(args[i], ft))
            jl_type_error("new", ft, args[i]);
        jl_set_nth_field(jv, i, args[i]);
    }
    for(size_t i=na; i < nf; i++) {
        if (jl_field_isptr(type, i)) {
            *(jl_value_t**)((char*)jl_data_ptr(jv)+jl_field_offset(type,i)) = NULL;
        }
        else {
            jl_value_t *ft = jl_field_type(type, i);
            if (jl_is_uniontype(ft)) {
                uint8_t *psel = &((uint8_t *)jv)[jl_field_offset(type, i) + jl_field_size(type, i) - 1];
                *psel = 0;
            }
        }
    }
    JL_GC_POP();
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
    jl_svec_t *fn = jl_field_names(t);
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
    size_t offs = jl_field_offset(st, i);
    if (jl_field_isptr(st, i)) {
        return *(jl_value_t**)((char*)v + offs);
    }
    jl_value_t *ty = jl_field_type(st, i);
    if (jl_is_uniontype(ty)) {
        uint8_t sel = ((uint8_t*)v)[offs + jl_field_size(st, i) - 1];
        ty = jl_nth_union_component(ty, sel);
        if (jl_is_datatype_singleton((jl_datatype_t*)ty))
            return ((jl_datatype_t*)ty)->instance;
    }
    return jl_new_bits(ty, (char*)v + offs);
}

JL_DLLEXPORT jl_value_t *jl_get_nth_field_noalloc(jl_value_t *v JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    assert(i < jl_datatype_nfields(st));
    size_t offs = jl_field_offset(st,i);
    assert(jl_field_isptr(st,i));
    return *(jl_value_t**)((char*)v + offs);
}

JL_DLLEXPORT jl_value_t *jl_get_nth_field_checked(jl_value_t *v, size_t i)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    if (i >= jl_datatype_nfields(st))
        jl_bounds_error_int(v, i + 1);
    size_t offs = jl_field_offset(st, i);
    if (jl_field_isptr(st, i)) {
        jl_value_t *fval = *(jl_value_t**)((char*)v + offs);
        if (fval == NULL)
            jl_throw(jl_undefref_exception);
        return fval;
    }
    jl_value_t *ty = jl_field_type(st, i);
    if (jl_is_uniontype(ty)) {
        size_t fsz = jl_field_size(st, i);
        uint8_t sel = ((uint8_t*)v)[offs + fsz - 1];
        ty = jl_nth_union_component(ty, sel);
        if (jl_is_datatype_singleton((jl_datatype_t*)ty))
            return ((jl_datatype_t*)ty)->instance;
    }
    return jl_new_bits(ty, (char*)v + offs);
}

JL_DLLEXPORT void jl_set_nth_field(jl_value_t *v, size_t i, jl_value_t *rhs)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t offs = jl_field_offset(st, i);
    if (jl_field_isptr(st, i)) {
        *(jl_value_t**)((char*)v + offs) = rhs;
        if (rhs != NULL) jl_gc_wb(v, rhs);
    }
    else {
        jl_value_t *ty = jl_field_type(st, i);
        if (jl_is_uniontype(ty)) {
            uint8_t *psel = &((uint8_t*)v)[offs + jl_field_size(st, i) - 1];
            unsigned nth = 0;
            if (!jl_find_union_component(ty, jl_typeof(rhs), &nth))
                assert(0 && "invalid field assignment to isbits union");
            *psel = nth;
            if (jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(rhs)))
                return;
        }
        jl_assign_bits((char*)v + offs, rhs);
    }
}

JL_DLLEXPORT int jl_field_isdefined(jl_value_t *v, size_t i)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t offs = jl_field_offset(st, i);
    if (jl_field_isptr(st, i)) {
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

#ifdef __cplusplus
}
#endif
