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
#include "julia_gcext.h"

#ifdef __cplusplus
extern "C" {
#endif

// allocating TypeNames -----------------------------------------------------------

static int is10digit(char c) JL_NOTSAFEPOINT
{
    return (c >= '0' && c <= '9');
}

jl_sym_t *jl_demangle_typename(jl_sym_t *s) JL_NOTSAFEPOINT
{
    char *n = jl_symbol_name(s);
    if (n[0] != '#')
        return s;
    char *end = strrchr(n, '#');
    int32_t len;
    if (end == n || end == n+1)
        len = strlen(n) - 1;
    else
        len = (end-n) - 1;  // extract `f` from `#f#...`
    if (is10digit(n[1]))
        return jl_symbol_n(n, len+1);
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
    mt->defs = jl_nothing;
    mt->leafcache = (jl_array_t*)jl_an_empty_vec_any;
    mt->cache = jl_nothing;
    mt->max_args = 0;
    mt->kwsorter = NULL;
    mt->backedges = NULL;
    JL_MUTEX_INIT(&mt->writelock);
    mt->offs = 1;
    mt->frozen = 0;
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
    tn->partial = NULL;
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
    t->hash = 0;
    t->hasfreetypevars = 0;
    t->isdispatchtuple = 0;
    t->isbitstype = 0;
    t->zeroinit = 0;
    t->isinlinealloc = 0;
    t->has_concrete_subtype = 1;
    t->layout = NULL;
    t->names = NULL;
    return t;
}

static jl_datatype_layout_t *jl_get_layout(uint32_t nfields,
                                           uint32_t npointers,
                                           uint32_t alignment,
                                           int haspadding,
                                           jl_fielddesc32_t desc[],
                                           uint32_t pointers[]) JL_NOTSAFEPOINT
{
    assert(alignment); // should have been verified by caller

    // compute the smallest fielddesc type that can hold the layout description
    int fielddesc_type = 0;
    if (nfields > 0) {
        uint32_t max_size = 0;
        uint32_t max_offset = desc[nfields - 1].offset;
        if (npointers > 0 && pointers[npointers - 1] > max_offset)
            max_offset = pointers[npointers - 1];
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
    // TODO: lots of these are the same--take advantage of the fact these are immutable to combine them
    uint32_t fielddesc_size = jl_fielddesc_size(fielddesc_type);
    jl_datatype_layout_t *flddesc = (jl_datatype_layout_t*)jl_gc_perm_alloc(
                sizeof(jl_datatype_layout_t) + nfields * fielddesc_size + (npointers << fielddesc_type),
                0, 4, 0);
    flddesc->nfields = nfields;
    flddesc->alignment = alignment;
    flddesc->haspadding = haspadding;
    flddesc->fielddesc_type = fielddesc_type;
    flddesc->npointers = npointers;
    flddesc->first_ptr = (npointers > 0 ? pointers[0] : -1);

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
    uint8_t* ptrs8 = (uint8_t*)jl_dt_layout_ptrs(flddesc);
    uint16_t* ptrs16 = (uint16_t*)jl_dt_layout_ptrs(flddesc);
    uint32_t* ptrs32 = (uint32_t*)jl_dt_layout_ptrs(flddesc);
    for (size_t i = 0; i < npointers; i++) {
        if (fielddesc_type == 0) {
            ptrs8[i] = pointers[i];
        }
        else if (fielddesc_type == 1) {
            ptrs16[i] = pointers[i];
        }
        else {
            ptrs32[i] = pointers[i];
        }
    }
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
    assert(jl_datatype_nfields(t) == 1);
    jl_value_t *ty = jl_field_type((jl_datatype_t*)t, 0);
    if (!jl_is_primitivetype(ty))
        // LLVM requires that a vector element be a primitive type.
        // LLVM allows pointer types as vector elements, but until a
        // motivating use case comes up for Julia, we reject pointers.
        return 0;
    size_t elsz = jl_datatype_size(ty);
    if (elsz != 1 && elsz != 2 && elsz != 4 && elsz != 8)
        // Only handle power-of-two-sized elements (for now)
        return 0;
    size_t size = nfields * elsz;
    // Use natural alignment for this vector: this matches LLVM and clang.
    return next_power_of_two(size);
}

STATIC_INLINE int jl_is_datatype_make_singleton(jl_datatype_t *d)
{
    return (!d->abstract && jl_datatype_size(d) == 0 && d != jl_symbol_type && d->name != jl_array_typename &&
            d->isconcretetype && !d->mutabl);
}

STATIC_INLINE void jl_maybe_allocate_singleton_instance(jl_datatype_t *st)
{
    if (jl_is_datatype_make_singleton(st)) {
        st->instance = jl_gc_alloc(jl_get_ptls_states(), 0, st);
        jl_gc_wb(st, st->instance);
    }
}

static unsigned union_isinlinable(jl_value_t *ty, int pointerfree, size_t *nbytes, size_t *align) JL_NOTSAFEPOINT
{
    if (jl_is_uniontype(ty)) {
        unsigned na = union_isinlinable(((jl_uniontype_t*)ty)->a, 1, nbytes, align);
        if (na == 0)
            return 0;
        unsigned nb = union_isinlinable(((jl_uniontype_t*)ty)->b, 1, nbytes, align);
        if (nb == 0)
            return 0;
        return na + nb;
    }
    if (jl_is_datatype(ty) && jl_datatype_isinlinealloc(ty) && (!pointerfree || ((jl_datatype_t*)ty)->layout->npointers == 0)) {
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

JL_DLLEXPORT int jl_islayout_inline(jl_value_t *eltype, size_t *fsz, size_t *al) JL_NOTSAFEPOINT
{
    unsigned countbits = union_isinlinable(eltype, 0, fsz, al);
    return (countbits > 0 && countbits < 127) ? countbits : 0;
}

JL_DLLEXPORT int jl_stored_inline(jl_value_t *eltype) JL_NOTSAFEPOINT
{
    size_t fsz = 0, al = 0;
    return jl_islayout_inline(eltype, &fsz, &al);
}

// whether instances of this type can use pointer comparison for `===`
int jl_pointer_egal(jl_value_t *t)
{
    if (t == (jl_value_t*)jl_any_type)
        return 0; // when setting up the initial types, jl_is_type_type gets confused about this
    if (t == (jl_value_t*)jl_symbol_type)
        return 1;
    if (jl_is_mutable_datatype(t) && // excludes abstract types
        t != (jl_value_t*)jl_string_type && // technically mutable, but compared by contents
        t != (jl_value_t*)jl_simplevector_type &&
        !jl_is_kind(t))
        return 1;
    if (jl_is_type_type(t) && jl_is_concrete_type(jl_tparam0(t))) {
        // need to use typeseq for most types
        // but can compare some types by pointer
        return 1;
    }
    return 0;
}

static int references_name(jl_value_t *p, jl_typename_t *name, int affects_layout) JL_NOTSAFEPOINT
{
    if (jl_is_uniontype(p))
        return references_name(((jl_uniontype_t*)p)->a, name, affects_layout) ||
               references_name(((jl_uniontype_t*)p)->b, name, affects_layout);
    if (jl_is_unionall(p))
        return references_name((jl_value_t*)((jl_unionall_t*)p)->var, name, 0) ||
               references_name(((jl_unionall_t*)p)->body, name, affects_layout);
    if (jl_is_typevar(p))
        return references_name(((jl_tvar_t*)p)->ub, name, 0) ||
               references_name(((jl_tvar_t*)p)->lb, name, 0);
    if (jl_is_datatype(p)) {
        jl_datatype_t *dp = (jl_datatype_t*)p;
        if (affects_layout && dp->name == name)
            return 1;
        affects_layout = dp->types == NULL || jl_svec_len(dp->types) != 0;
        size_t i, l = jl_nparams(p);
        for (i = 0; i < l; i++) {
            if (references_name(jl_tparam(p, i), name, affects_layout))
                return 1;
        }
    }
    return 0;
}

static void throw_ovf(int should_malloc, void *desc, jl_datatype_t* st, int offset)
{
    if (should_malloc)
        free(desc);
    jl_errorf("type %s has field offset %d that exceeds the page size", jl_symbol_name(st->name->name), offset);
}

void jl_compute_field_offsets(jl_datatype_t *st)
{
    const uint64_t max_offset = (((uint64_t)1) << 32) - 1;
    const uint64_t max_size = max_offset >> 1;

    if (st->types == NULL || st->name->wrapper == NULL)
        return;
    if ((jl_is_tuple_type(st) || jl_is_namedtuple_type(st)) && !jl_is_concrete_type((jl_value_t*)st))
        return;
    jl_datatype_t *w = (jl_datatype_t*)jl_unwrap_unionall(st->name->wrapper);
    if (w->types == NULL) // we got called too early--we'll be back
        return;
    size_t i, nfields = jl_svec_len(st->types);
    int isinlinealloc = st->isconcretetype && !st->mutabl;
    int isbitstype = isinlinealloc;
    assert(st->ninitialized <= nfields);
    if (st == w && st->layout) {
        // this check allows us to force re-computation of the layout for some types during init
        st->layout = NULL;
        st->size = 0;
        st->zeroinit = 0;
        st->has_concrete_subtype = 1;
    }
    // If layout doesn't depend on type parameters, it's stored in st->name->wrapper
    // and reused by all subtypes.
    if (w->layout) {
        st->layout = w->layout;
        st->size = w->size;
        st->zeroinit = w->zeroinit;
        st->has_concrete_subtype = w->has_concrete_subtype;
        if (jl_is_layout_opaque(st->layout)) { // e.g. jl_array_typename
            return;
        }
    }
    else if (nfields == 0) {
        // if we have no fields, we can trivially skip the rest
        if (st == jl_symbol_type || st == jl_string_type) {
            // opaque layout - heap-allocated blob
            static const jl_datatype_layout_t opaque_byte_layout = {0, 1, -1, 1, 0, 0};
            st->layout = &opaque_byte_layout;
            return;
        }
        else if (st == jl_simplevector_type || st->name == jl_array_typename) {
            static const jl_datatype_layout_t opaque_ptr_layout = {0, 1, -1, sizeof(void*), 0, 0};
            st->layout = &opaque_ptr_layout;
            return;
        }
        else {
            // reuse the same layout for all singletons
            static const jl_datatype_layout_t singleton_layout = {0, 0, -1, 1, 0, 0};
            st->layout = &singleton_layout;
        }
    }
    else {
        // compute a conservative estimate of whether there could exist an instance of a subtype of this
        for (i = 0; st->has_concrete_subtype && i < st->ninitialized; i++) {
            jl_value_t *fld = jl_svecref(st->types, i);
            if (fld == jl_bottom_type)
                st->has_concrete_subtype = 0;
            else
                st->has_concrete_subtype = !jl_is_datatype(fld) || ((jl_datatype_t *)fld)->has_concrete_subtype;
        }
        // compute layout for the wrapper object if the field types have no free variables
        if (!st->isconcretetype) {
            if (st != w)
                return; // otherwise we would leak memory
            for (i = 0; i < nfields; i++) {
                if (jl_has_free_typevars(jl_field_type(st, i)))
                    return; // not worthwhile computing the rest
            }
        }
    }

    // compute whether this type may ever be inlined
    // based solely on whether its definition is self-referential
    if (isinlinealloc) {
        size_t i, nf = jl_svec_len(w->types);
        for (i = 0; i < nf; i++) {
            jl_value_t *fld = jl_svecref(w->types, i);
            if (references_name(fld, w->name, 1)) {
                isinlinealloc = 0;
                isbitstype = 0;
                break;
            }
        }
        for (i = 0; isbitstype && i < nfields; i++) {
            jl_value_t *fld = jl_field_type(st, i);
            isbitstype = jl_isbits(fld);
        }
    }

    // if we didn't reuse the layout above, compute it now
    if (st->layout == NULL) {
        size_t descsz = nfields * sizeof(jl_fielddesc32_t);
        jl_fielddesc32_t *desc;
        uint32_t *pointers;
        int should_malloc = descsz >= jl_page_size;
        if (should_malloc)
            desc = (jl_fielddesc32_t*)malloc_s(descsz);
        else
            desc = (jl_fielddesc32_t*)alloca(descsz);
        size_t sz = 0;
        size_t alignm = 1;
        int zeroinit = 0;
        int haspadding = 0;
        int homogeneous = 1;
        uint32_t npointers = 0;
        jl_value_t *firstty = jl_field_type(st, 0);
        for (i = 0; i < nfields; i++) {
            jl_value_t *fld = jl_field_type(st, i);
            size_t fsz = 0, al = 1;
            if (jl_islayout_inline(fld, &fsz, &al)) { // aka jl_datatype_isinlinealloc
                if (__unlikely(fsz > max_size))
                    // Should never happen
                    throw_ovf(should_malloc, desc, st, fsz);
                desc[i].isptr = 0;
                if (jl_is_uniontype(fld)) {
                    haspadding = 1;
                    fsz += 1; // selector byte
                    zeroinit = 1;
                }
                else {
                    if (((jl_datatype_t*)fld)->layout->haspadding)
                        haspadding = 1;
                    if (!zeroinit)
                        zeroinit = ((jl_datatype_t*)fld)->zeroinit;
                    npointers += ((jl_datatype_t*)fld)->layout->npointers;
                }
            }
            else {
                fsz = sizeof(void*);
                if (fsz > MAX_ALIGN)
                    fsz = MAX_ALIGN;
                al = fsz;
                desc[i].isptr = 1;
                zeroinit = 1;
                npointers++;
                if (!jl_pointer_egal(fld)) {
                    // this somewhat poorly named flag says whether some of the bits can be non-unique
                    haspadding = 1;
                }
            }
            if (al != 0) {
                size_t alsz = LLT_ALIGN(sz, al);
                if (sz & (al - 1))
                    haspadding = 1;
                sz = alsz;
                if (al > alignm)
                    alignm = al;
            }
            homogeneous &= firstty == fld;
            desc[i].offset = sz;
            desc[i].size = fsz;
            if (__unlikely(max_offset - sz < fsz))
                throw_ovf(should_malloc, desc, st, sz);
            sz += fsz;
        }
        if (homogeneous && jl_is_tuple_type(st)) {
            // Some tuples become LLVM vectors with stronger alignment than what was calculated above.
            unsigned al = jl_special_vector_alignment(nfields, firstty);
            assert(al % alignm == 0);
            if (al > alignm)
                alignm = al;
        }
        st->size = LLT_ALIGN(sz, alignm);
        if (st->size > sz)
            haspadding = 1;
        if (should_malloc && npointers)
            pointers = (uint32_t*)malloc_s(npointers * sizeof(uint32_t));
        else
            pointers = (uint32_t*)alloca(npointers * sizeof(uint32_t));
        size_t ptr_i = 0;
        for (i = 0; i < nfields; i++) {
            jl_value_t *fld = jl_field_type(st, i);
            uint32_t offset = desc[i].offset / sizeof(jl_value_t**);
            if (desc[i].isptr)
                pointers[ptr_i++] = offset;
            else if (jl_is_datatype(fld)) {
                int j, npointers = ((jl_datatype_t*)fld)->layout->npointers;
                for (j = 0; j < npointers; j++) {
                    pointers[ptr_i++] = offset + jl_ptr_offset((jl_datatype_t*)fld, j);
                }
            }
        }
        assert(ptr_i == npointers);
        st->layout = jl_get_layout(nfields, npointers, alignm, haspadding, desc, pointers);
        if (should_malloc) {
            free(desc);
            if (npointers)
                free(pointers);
        }
    }
    // now finish deciding if this instantiation qualifies for special properties
    assert(!isbitstype || st->layout->npointers == 0); // the definition of isbits
    if (isinlinealloc && st->layout->npointers > 0) {
        if (st->ninitialized != nfields)
            isinlinealloc = 0;
        else if (st->layout->fielddesc_type != 0) // GC only implements support for this
            isinlinealloc = 0;
    }
    st->isbitstype = isbitstype;
    st->isinlinealloc = isinlinealloc;
    jl_maybe_allocate_singleton_instance(st);
    return;
}

static int is_anonfn_typename(char *name)
{
    if (name[0] != '#')
        return 0;
    char *other = strrchr(name, '#');
    return (name[1] != '#' && other > &name[1] && is10digit(other[1]));
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

    assert(parameters);

    // init enough before possibly calling jl_new_typename_in
    t = jl_new_uninitialized_datatype();
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
    t->size = 0;

    t->name = NULL;
    if (jl_is_typename(name)) {
        // This code-path is used by the Serialization module to by-pass normal expectations
        tn = (jl_typename_t*)name;
    }
    else {
        tn = jl_new_typename_in((jl_sym_t*)name, module);
        if (super == jl_function_type || super == jl_builtin_type || is_anonfn_typename(jl_symbol_name(name))) {
            // Callable objects (including compiler-generated closures) get independent method tables
            // as an optimization
            tn->mt = jl_new_method_table(name, module);
            jl_gc_wb(tn, tn->mt);
            if (jl_svec_len(parameters) > 0)
                tn->mt->offs = 0;
        }
        else {
            // Everything else, gets to use the unified table
            tn->mt = jl_nonfunction_mt;
        }
    }
    t->name = tn;
    jl_gc_wb(t, t->name);
    t->name->names = fnames;
    jl_gc_wb(t->name, t->name->names);

    if (t->name->wrapper == NULL) {
        t->name->wrapper = (jl_value_t*)t;
        jl_gc_wb(t->name, t);
        int i, np = jl_svec_len(parameters);
        for (i = np - 1; i >= 0; i--) {
            t->name->wrapper = jl_new_struct(jl_unionall_type, jl_svecref(parameters, i), t->name->wrapper);
            jl_gc_wb(t->name, t->name->wrapper);
        }
    }
    jl_precompute_memoized_dt(t, 0);

    if (!abstract)
        jl_compute_field_offsets(t);

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
    bt->layout = jl_get_layout(0, 0, alignm, 0, NULL, NULL);
    bt->instance = NULL;
    return bt;
}

JL_DLLEXPORT jl_datatype_t * jl_new_foreign_type(jl_sym_t *name,
                                                 jl_module_t *module,
                                                 jl_datatype_t *super,
                                                 jl_markfunc_t markfunc,
                                                 jl_sweepfunc_t sweepfunc,
                                                 int haspointers,
                                                 int large)
{
    jl_datatype_t *bt = jl_new_datatype(name, module, super,
      jl_emptysvec, jl_emptysvec, jl_emptysvec, 0, 1, 0);
    bt->size = large ? GC_MAX_SZCLASS+1 : 0;
    jl_datatype_layout_t *layout = (jl_datatype_layout_t *)
      jl_gc_perm_alloc(sizeof(jl_datatype_layout_t) + sizeof(jl_fielddescdyn_t),
        0, 4, 0);
    layout->nfields = 0;
    layout->alignment = sizeof(void *);
    layout->haspadding = 1;
    layout->npointers = haspointers;
    layout->fielddesc_type = 3;
    jl_fielddescdyn_t * desc =
      (jl_fielddescdyn_t *) ((char *)layout + sizeof(*layout));
    desc->markfunc = markfunc;
    desc->sweepfunc = sweepfunc;
    bt->layout = layout;
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
    {   /* NOTE: t must be a concrete isbits datatype */                \
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
UNBOX_FUNC(uint8pointer, uint8_t*)

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
BOX_FUNC(uint8pointer, uint8_t*,  jl_box, 1)
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
    uint32_t u = bswap_32(x);
    if (u < 128)
        return boxed_char_cache[(uint8_t)u];
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
        set_nth_field(type, (void*)jv, i, va_arg(args, jl_value_t*));
    }
    va_end(args);
    return jv;
}

static void init_struct_tail(jl_datatype_t *type, jl_value_t *jv, size_t na)
{
    if (na < jl_datatype_nfields(type)) {
        char *data = (char*)jl_data_ptr(jv);
        size_t offs = jl_field_offset(type, na);
        memset(data + offs, 0, jl_datatype_size(type) - offs);
    }
}

JL_DLLEXPORT jl_value_t *jl_new_structv(jl_datatype_t *type, jl_value_t **args, uint32_t na)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (!jl_is_datatype(type) || type->layout == NULL)
        jl_type_error("new", (jl_value_t*)jl_datatype_type, (jl_value_t*)type);
    if (type->ninitialized > na || na > jl_datatype_nfields(type))
        jl_error("invalid struct allocation");
    for (size_t i = 0; i < na; i++) {
        jl_value_t *ft = jl_field_type(type, i);
        if (!jl_isa(args[i], ft))
            jl_type_error("new", ft, args[i]);
    }
    if (type->instance != NULL)
        return type->instance;
    jl_value_t *jv = jl_gc_alloc(ptls, jl_datatype_size(type), type);
    JL_GC_PUSH1(&jv);
    for (size_t i = 0; i < na; i++) {
        set_nth_field(type, (void*)jv, i, args[i]);
    }
    init_struct_tail(type, jv, na);
    JL_GC_POP();
    return jv;
}

JL_DLLEXPORT jl_value_t *jl_new_structt(jl_datatype_t *type, jl_value_t *tup)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (!jl_is_tuple(tup))
        jl_type_error("new", (jl_value_t*)jl_tuple_type, tup);
    if (!jl_is_datatype(type) || type->layout == NULL)
        jl_type_error("new", (jl_value_t *)jl_datatype_type, (jl_value_t *)type);
    size_t nargs = jl_nfields(tup);
    size_t nf = jl_datatype_nfields(type);
    JL_NARGS(new, nf, nf);
    if (type->instance != NULL) {
        jl_datatype_t *tupt = (jl_datatype_t*)jl_typeof(tup);
        for (size_t i = 0; i < nargs; i++) {
            jl_value_t *ft = jl_field_type(type, i);
            jl_value_t *et = jl_field_type(tupt, i);
            assert(jl_is_concrete_type(ft) && jl_is_concrete_type(et));
            if (et != ft)
                jl_type_error("new", ft, jl_get_nth_field(tup, i));
        }
        return type->instance;
    }
    jl_value_t *jv = jl_gc_alloc(ptls, jl_datatype_size(type), type);
    jl_value_t *fi = NULL;
    JL_GC_PUSH2(&jv, &fi);
    if (type->layout->npointers > 0) {
        // if there are references, zero the space first to prevent the GC
        // from seeing uninitialized references during jl_get_nth_field and jl_isa,
        // which can allocate.
        memset(jl_data_ptr(jv), 0, jl_datatype_size(type));
    }
    for (size_t i = 0; i < nargs; i++) {
        jl_value_t *ft = jl_field_type(type, i);
        fi = jl_get_nth_field(tup, i);
        if (!jl_isa(fi, ft))
            jl_type_error("new", ft, fi);
        set_nth_field(type, (void*)jv, i, fi);
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
    size_t n = jl_svec_len(fn);
    if (n == 0) {
        if (jl_is_namedtuple_type(t)) {
            jl_value_t *ns = jl_tparam0(t);
            if (jl_is_tuple(ns)) {
                n = jl_nfields(ns);
                for(size_t i=0; i < n; i++) {
                    if (jl_get_nth_field(ns, i) == (jl_value_t*)fld) {
                        return (int)i;
                    }
                }
            }
        }
    }
    else {
        for(size_t i=0; i < n; i++) {
            if (jl_svecref(fn,i) == (jl_value_t*)fld) {
                return (int)i;
            }
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
        if (__unlikely(fval == NULL))
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
    return undefref_check((jl_datatype_t*)ty, jl_new_bits(ty, (char*)v + offs));
}

void set_nth_field(jl_datatype_t *st, void *v, size_t i, jl_value_t *rhs) JL_NOTSAFEPOINT
{
    size_t offs = jl_field_offset(st, i);
    if (jl_field_isptr(st, i)) {
        *(jl_value_t**)((char*)v + offs) = rhs;
        if (rhs != NULL)
            jl_gc_wb(v, rhs);
    }
    else {
        jl_value_t *ty = jl_field_type_concrete(st, i);
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
        jl_gc_multi_wb(v, rhs);
    }
}

JL_DLLEXPORT int jl_field_isdefined(jl_value_t *v, size_t i)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t offs = jl_field_offset(st, i);
    char *fld = (char*)v + offs;
    if (jl_field_isptr(st, i)) {
        return *(jl_value_t**)fld != NULL;
    }
    jl_datatype_t *ft = (jl_datatype_t*)jl_field_type(st, i);
    if (jl_is_datatype(ft) && ft->layout->first_ptr >= 0) {
         return ((jl_value_t**)fld)[ft->layout->first_ptr] != NULL;
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
