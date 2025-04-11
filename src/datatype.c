// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  defining DataTypes
  basic operations on struct and bits values
*/

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdalign.h>
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"
#include "julia_gcext.h"

#ifdef __cplusplus
extern "C" {
#endif

// allocating TypeNames -----------------------------------------------------------

static jl_sym_t *jl_demangle_typename(jl_sym_t *s) JL_NOTSAFEPOINT
{
    char *n = jl_symbol_name(s);
    if (n[0] != '#')
        return s;
    char *end = strchr(&n[1], '#');
    // handle `#f...##...#...`
    if (end != NULL && end[1] == '#')
        end = strchr(&end[2], '#');
    int32_t len;
    if (end == NULL || end == n+1)
        len = strlen(n) - 1;
    else
        len = (end-n) - 1;  // extract `f` from `#f#...`
    if (isdigit(n[1]) || is_canonicalized_anonfn_typename(n))
        return _jl_symbol(n, len+1);
    return _jl_symbol(&n[1], len);
}

JL_DLLEXPORT jl_methcache_t *jl_new_method_cache(void)
{
    jl_task_t *ct = jl_current_task;
    jl_methcache_t *mc =
        (jl_methcache_t*)jl_gc_alloc(ct->ptls, sizeof(jl_methcache_t),
                                     jl_methcache_type);
    jl_atomic_store_relaxed(&mc->leafcache, (jl_genericmemory_t*)jl_an_empty_memory_any);
    jl_atomic_store_relaxed(&mc->cache, jl_nothing);
    JL_MUTEX_INIT(&mc->writelock, "methodtable->writelock");
    return mc;
}

JL_DLLEXPORT jl_methtable_t *jl_new_method_table(jl_sym_t *name, jl_module_t *module)
{
    jl_methcache_t *mc = jl_new_method_cache();
    JL_GC_PUSH1(&mc);
    jl_task_t *ct = jl_current_task;
    jl_methtable_t *mt =
        (jl_methtable_t*)jl_gc_alloc(ct->ptls, sizeof(jl_methtable_t), jl_methtable_type);
    jl_atomic_store_relaxed(&mt->defs, jl_nothing);
    mt->cache = mc;
    mt->name = name;
    mt->module = module;
    JL_GC_POP();
    return mt;
}

JL_DLLEXPORT jl_typename_t *jl_new_typename_in(jl_sym_t *name, jl_module_t *module, int abstract, int mutabl)
{
    jl_task_t *ct = jl_current_task;
    jl_typename_t *tn =
        (jl_typename_t*)jl_gc_alloc(ct->ptls, sizeof(jl_typename_t),
                                    jl_typename_type);
    tn->name = name;
    tn->module = module;
    tn->wrapper = NULL;
    tn->singletonname = jl_demangle_typename(name);
    jl_atomic_store_relaxed(&tn->Typeofwrapper, NULL);
    jl_atomic_store_relaxed(&tn->cache, jl_emptysvec);
    jl_atomic_store_relaxed(&tn->linearcache, jl_emptysvec);
    tn->names = NULL;
    tn->hash = bitmix(bitmix(module ? module->build_id.lo : 0, name->hash), 0xa1ada1da);
    tn->_unused = 0;
    tn->abstract = abstract;
    tn->mutabl = mutabl;
    tn->mayinlinealloc = 0;
    tn->partial = NULL;
    tn->atomicfields = NULL;
    tn->constfields = NULL;
    tn->backedges = NULL;
    tn->max_methods = 0;
    jl_atomic_store_relaxed(&tn->max_args, 0);
    jl_atomic_store_relaxed(&tn->cache_entry_count, 0);
    tn->constprop_heustic = 0;
    return tn;
}

// allocating DataTypes -----------------------------------------------------------

jl_datatype_t *jl_new_abstracttype(jl_value_t *name, jl_module_t *module, jl_datatype_t *super, jl_svec_t *parameters)
{
    return jl_new_datatype((jl_sym_t*)name, module, super, parameters, jl_emptysvec, jl_emptysvec, jl_emptysvec, 1, 0, 0);
}

jl_datatype_t *jl_new_uninitialized_datatype(void)
{
    jl_task_t *ct = jl_current_task;
    jl_datatype_t *t = (jl_datatype_t*)jl_gc_alloc(ct->ptls, sizeof(jl_datatype_t), jl_datatype_type);
    jl_set_typetagof(t, jl_datatype_tag, 0);
    t->hash = 0;
    t->hasfreetypevars = 0;
    t->isdispatchtuple = 0;
    t->isbitstype = 0;
    t->isprimitivetype = 0;
    t->zeroinit = 0;
    t->has_concrete_subtype = 1;
    t->maybe_subtype_of_cache = 1;
    t->ismutationfree = 0;
    t->isidentityfree = 0;
    t->smalltag = 0;
    t->name = NULL;
    t->super = NULL;
    t->parameters = NULL;
    t->layout = NULL;
    t->types = NULL;
    t->instance = NULL;
    return t;
}

#include "support/htable.inc"

static uint32_t _hash_djb2(uint32_t hash, const char *mem, size_t s) JL_NOTSAFEPOINT
{
    for (size_t i = 0; i < s; i++)
        hash = ((hash << 5) + hash) + mem[i];
    return hash;
}

static uint32_t _hash_layout_djb2(uintptr_t _layout, void *unused) JL_NOTSAFEPOINT
{
    (void)unused;
    jl_datatype_layout_t* layout = (jl_datatype_layout_t *)_layout;
    assert(layout);
    size_t own_size = sizeof(jl_datatype_layout_t);
    const char *fields = jl_dt_layout_fields(layout);
    assert(fields);
    size_t fields_size = layout->nfields * jl_fielddesc_size(layout->flags.fielddesc_type);
    const char *pointers = jl_dt_layout_ptrs(layout);
    assert(pointers);
    size_t pointers_size = layout->first_ptr < 0 ? 0 : (layout->npointers << layout->flags.fielddesc_type);

    uint_t hash = 5381;
    hash = _hash_djb2(hash, (char *)layout, own_size);
    hash = _hash_djb2(hash, fields, fields_size);
    hash = _hash_djb2(hash, pointers, pointers_size);
    return hash;
}

static int layout_eq(void *_l1, void *_l2, void *unused) JL_NOTSAFEPOINT
{
    (void)unused;
    jl_datatype_layout_t *l1 = (jl_datatype_layout_t *)_l1;
    jl_datatype_layout_t *l2 = (jl_datatype_layout_t *)_l2;
    if (memcmp(l1, l2, sizeof(jl_datatype_layout_t)))
        return 0;
    const char *f1 = jl_dt_layout_fields(l1);
    const char *f2 = jl_dt_layout_fields(l2);
    size_t fields_size = l1->nfields * jl_fielddesc_size(l1->flags.fielddesc_type);
    if (memcmp(f1, f2, fields_size))
        return 0;
    const char *p1 = jl_dt_layout_ptrs(l1);
    const char *p2 = jl_dt_layout_ptrs(l2);
    size_t pointers_size = l1->first_ptr < 0 ? 0 : (l1->npointers << l1->flags.fielddesc_type);
    if (memcmp(p1, p2, pointers_size))
        return 0;
    return 1;
}

//HTPROT(layoutcache)
static void **layoutcache_lookup_bp_r(htable_t *h, void *key, void *ctx) JL_NOTSAFEPOINT;
static void **layoutcache_peek_bp_r(htable_t *h, void *key, void *ctx) JL_NOTSAFEPOINT;
HTPROT_R(layoutcache)
HTIMPL_R(layoutcache, _hash_layout_djb2, layout_eq)
static htable_t layoutcache;
static int layoutcache_initialized = 0;

static jl_datatype_layout_t *jl_get_layout(uint32_t sz,
                                           uint32_t nfields,
                                           uint32_t npointers,
                                           uint32_t alignment,
                                           int haspadding,
                                           int isbitsegal,
                                           int arrayelem,
                                           jl_fielddesc32_t desc[],
                                           uint32_t pointers[]) JL_NOTSAFEPOINT
{
    assert(alignment); // should have been verified by caller

    // compute the smallest fielddesc type that can hold the layout description
    int fielddesc_type = 0;
    uint32_t max_size = 0;
    uint32_t max_offset = 0;
    if (nfields > 0) {
        max_offset = desc[nfields - 1].offset;
        for (size_t i = 0; i < nfields; i++) {
            if (desc[i].size > max_size)
                max_size = desc[i].size;
        }
    }
    if (npointers > 0 && pointers[npointers - 1] > max_offset)
        max_offset = pointers[npointers - 1];
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
    int32_t first_ptr = (npointers > 0 ? (int32_t)pointers[0] : -1);

    // allocate a new descriptor, on the stack if possible.
    size_t fields_size = nfields * jl_fielddesc_size(fielddesc_type);
    size_t pointers_size = first_ptr < 0 ? 0 : (npointers << fielddesc_type);
    size_t flddesc_sz = sizeof(jl_datatype_layout_t) + fields_size + pointers_size;
    int should_malloc = flddesc_sz >= jl_page_size;
    jl_datatype_layout_t *mallocmem = (jl_datatype_layout_t *)(should_malloc ? malloc(flddesc_sz) : NULL);
    jl_datatype_layout_t *allocamem = (jl_datatype_layout_t *)(should_malloc ? NULL : alloca(flddesc_sz));
    jl_datatype_layout_t *flddesc = should_malloc ? mallocmem : allocamem;
    assert(flddesc);
    flddesc->size = sz;
    flddesc->nfields = nfields;
    flddesc->alignment = alignment;
    flddesc->flags.haspadding = haspadding;
    flddesc->flags.isbitsegal = isbitsegal;
    flddesc->flags.fielddesc_type = fielddesc_type;
    flddesc->flags.arrayelem_isboxed = arrayelem == 1;
    flddesc->flags.arrayelem_isunion = arrayelem == 2;
    flddesc->flags.padding = 0;
    flddesc->npointers = npointers;
    flddesc->first_ptr = first_ptr;

    // fill out the fields of the new descriptor
    jl_fielddesc8_t *desc8 = (jl_fielddesc8_t *)jl_dt_layout_fields(flddesc);
    jl_fielddesc16_t *desc16 = (jl_fielddesc16_t *)jl_dt_layout_fields(flddesc);
    jl_fielddesc32_t *desc32 = (jl_fielddesc32_t *)jl_dt_layout_fields(flddesc);
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
    if (first_ptr >= 0) {
        uint8_t *ptrs8 = (uint8_t *)jl_dt_layout_ptrs(flddesc);
        uint16_t *ptrs16 = (uint16_t *)jl_dt_layout_ptrs(flddesc);
        uint32_t *ptrs32 = (uint32_t *)jl_dt_layout_ptrs(flddesc);
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
    }

    if (__unlikely(!layoutcache_initialized)) {
        htable_new(&layoutcache, 4096);
        layoutcache_initialized = 1;
    }

    // Check the cache to see if this object already exists.
    // Add to cache if not present, free temp buffer, return.
    jl_datatype_layout_t *ret =
            (jl_datatype_layout_t *)layoutcache_get_r(&layoutcache, flddesc, NULL);
    if ((void*)ret == HT_NOTFOUND) {
        if (!should_malloc) {
            char *perm_mem = (char *)jl_gc_perm_alloc(flddesc_sz, 0, 4, 0);
            assert(perm_mem);
            ret = (jl_datatype_layout_t *)perm_mem;
            memcpy(perm_mem, flddesc, flddesc_sz);
        }
        else {
            ret = mallocmem;
        }
        layoutcache_put_r(&layoutcache, ret, ret, NULL);
        return ret;
    }

    if (should_malloc) free(flddesc);
    return ret;
}

// Determine if homogeneous tuple with fields of type t will have
// a special alignment and vector-ABI beyond normal rules for aggregates.
// Return special alignment if one exists, 0 if normal alignment rules hold.
// A non-zero result *must* match the LLVM rules for a vector type <nfields x t>.
// Matching the compiler's `__attribute__ vector_size` behavior.
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
    if (next_power_of_two(elsz) != elsz)
        // Only handle power-of-two-sized elements (for now), since other
        // lengths may be packed into very complicated arrangements (llvm pads
        // extra bits on most platforms when computing alignment but not when
        // computing type size, but adds no extra bytes for each element, so
        // their effect on offsets are never what you may naturally expect).
        return 0;
    size_t size = nfields * elsz;
    // Use natural alignment for this vector: this matches LLVM and clang.
    return next_power_of_two(size);
}

STATIC_INLINE int jl_is_datatype_make_singleton(jl_datatype_t *d) JL_NOTSAFEPOINT
{
    return d->isconcretetype && jl_datatype_size(d) == 0 && d->layout->npointers == 0 && !d->name->mutabl; // implies jl_is_layout_opaque
}

STATIC_INLINE void jl_maybe_allocate_singleton_instance(jl_datatype_t *st) JL_NOTSAFEPOINT
{
    // It's possible for st to already have an ->instance if it was redefined
    if (st->instance)
        return;
    if (jl_is_datatype_make_singleton(st)) {
        st->instance = jl_gc_permobj(0, st, 0);
    }
}

// return whether all concrete subtypes of this type have the same layout
int jl_struct_try_layout(jl_datatype_t *dt)
{
    if (dt->layout || jl_is_genericmemory_type(dt))
        return 1;
    else if (!jl_has_fixed_layout(dt))
        return 0;
    // jl_has_fixed_layout also ensured that dt->types is assigned now
    jl_compute_field_offsets(dt);
    assert(dt->layout);
    return 1;
}

int jl_datatype_isinlinealloc(jl_datatype_t *ty, int pointerfree)
{
    if (jl_typeofbottom_type && ty == jl_typeofbottom_type->super)
        ty = jl_typeofbottom_type;
    if (ty->name->mayinlinealloc && jl_struct_try_layout(ty)) {
        if (ty->layout->npointers > 0) {
            if (pointerfree)
                return 0;
            if (ty->name->n_uninitialized != 0)
                return 0;
            if (ty->layout->flags.fielddesc_type > 1) // GC only implements support for 8 and 16 (not array32)
                return 0;
        }
        return 1;
    }
    return 0;
}

static unsigned union_isinlinable(jl_value_t *ty, int pointerfree, size_t *nbytes, size_t *align, int asfield)
{
    if (jl_is_uniontype(ty)) {
        unsigned na = union_isinlinable(((jl_uniontype_t*)ty)->a, 1, nbytes, align, asfield);
        if (na == 0)
            return 0;
        unsigned nb = union_isinlinable(((jl_uniontype_t*)ty)->b, 1, nbytes, align, asfield);
        if (nb == 0)
            return 0;
        return na + nb;
    }
    if (jl_is_datatype(ty) && jl_datatype_isinlinealloc((jl_datatype_t*)ty, pointerfree)) {
        size_t sz = jl_datatype_size(ty);
        size_t al = jl_datatype_align(ty);
        // primitive types in struct slots need their sizes aligned. issue #37974
        if (asfield && jl_is_primitivetype(ty))
            sz = LLT_ALIGN(sz, al);
        if (*nbytes < sz)
            *nbytes = sz;
        if (*align < al)
            *align = al;
        return 1;
    }
    return 0;
}

int jl_uniontype_size(jl_value_t *ty, size_t *sz)
{
    size_t al = 0;
    return union_isinlinable(ty, 0, sz, &al, 0) != 0;
}

JL_DLLEXPORT int jl_islayout_inline(jl_value_t *eltype, size_t *fsz, size_t *al)
{
    unsigned countbits = union_isinlinable(eltype, 0, fsz, al, 1);
    return (countbits > 0 && countbits < 127) ? countbits : 0;
}

JL_DLLEXPORT int jl_stored_inline(jl_value_t *eltype)
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
    if (t == (jl_value_t*)jl_bool_type)
        return 1;
    if (jl_is_mutable_datatype(jl_unwrap_unionall(t)) && // excludes abstract types
        t != (jl_value_t*)jl_string_type && // technically mutable, but compared by contents
        t != (jl_value_t*)jl_simplevector_type &&
        !jl_is_kind(t))
        return 1;
    if ((jl_is_datatype(t) && jl_is_datatype_singleton((jl_datatype_t*)t)) ||
        t == (jl_value_t*)jl_typeofbottom_type->super)
        return 1;
    if (jl_is_type_type(t) && jl_is_datatype(jl_tparam0(t))) {
        // need to use typeseq for most types
        // but can compare some types by pointer
        jl_datatype_t *dt = (jl_datatype_t*)jl_tparam0(t);
        // `Core.TypeofBottom` and `Type{Union{}}` are used interchangeably
        // with different pointer values even though `Core.TypeofBottom` is a concrete type.
        // See `Core.Compiler.hasuniquerep`
        if (dt != jl_typeofbottom_type &&
            (dt->isconcretetype || jl_svec_len(dt->parameters) == 0)) {
            // Concrete types have unique pointer values
            // If the type has zero type parameters it'll also have only one possible
            // pointer value.
            return 1;
        }
    }
    if (jl_is_uniontype(t)) {
        jl_uniontype_t *u = (jl_uniontype_t*)t;
        return jl_pointer_egal(u->a) && jl_pointer_egal(u->b);
    }
    return 0;
}

static void throw_ovf(int should_malloc, void *desc, jl_datatype_t* st, int offset)
{
    if (should_malloc)
        free(desc);
    jl_errorf("type %s has field offset %d that exceeds the page size", jl_symbol_name(st->name->name), offset);
}

static int is_type_mutationfree(jl_value_t *t)
{
    t = jl_unwrap_unionall(t);
    if (jl_is_uniontype(t)) {
        jl_uniontype_t *u = (jl_uniontype_t*)t;
        return is_type_mutationfree(u->a) && is_type_mutationfree(u->b);
    }
    if (jl_is_datatype(t)) {
        return ((jl_datatype_t*)t)->ismutationfree;
    }
    // Free tvars, etc.
    return 0;
}

static int is_type_identityfree(jl_value_t *t)
{
    t = jl_unwrap_unionall(t);
    if (jl_is_uniontype(t)) {
        jl_uniontype_t *u = (jl_uniontype_t*)t;
        return is_type_identityfree(u->a) && is_type_identityfree(u->b);
    }
    if (jl_is_datatype(t)) {
        return ((jl_datatype_t*)t)->isidentityfree;
    }
    // Free tvars, etc.
    return 0;
}

// make a copy of the layout of st, but with nfields=0
void jl_get_genericmemory_layout(jl_datatype_t *st)
{
    jl_value_t *kind = jl_tparam0(st);
    jl_value_t *eltype = jl_tparam1(st);
    jl_value_t *addrspace = jl_tparam2(st);
    if (!st->isconcretetype) {
        // Since parent dt has an opaque layout, we may end up here being asked to copy that layout to subtypes,
        // but we don't actually want to do that unless this object is constructable (or at least has a layout).
        // The real layout is stored only on the wrapper.
        return;
    }
    if (!jl_is_type(eltype)) {
        // this is expected to have a layout, but since it is not constructable, we don't care too much what it is
        static const jl_datatype_layout_t opaque_ptr_layout = {0, 0, 1, -1, sizeof(void*), {0}};
        st->layout = &opaque_ptr_layout;
        st->has_concrete_subtype = 0;
        return;
    }

    size_t elsz = 0, al = 1;
    int isunboxed = jl_islayout_inline(eltype, &elsz, &al) && (kind != (jl_value_t*)jl_atomic_sym || jl_is_datatype(eltype));
    int isunion = isunboxed && jl_is_uniontype(eltype);
    int haspadding = 1; // we may want to eventually actually compute this more precisely
    int isbitsegal = 0;
    int nfields = 0; // aka jl_is_layout_opaque
    int npointers = 1;
    int zi;
    uint32_t first_ptr = -1;
    uint32_t *pointers = &first_ptr;
    int needlock = 0;

    if (isunboxed) {
        elsz = LLT_ALIGN(elsz, al);
        if (kind == (jl_value_t*)jl_atomic_sym) {
            if (elsz > MAX_ATOMIC_SIZE)
                needlock = 1;
            else if (elsz > 0)
                al = elsz = next_power_of_two(elsz);
        }
        if (isunion) {
            zi = 1;
        }
        else {
            assert(jl_is_datatype(eltype));
            zi = ((jl_datatype_t*)eltype)->zeroinit;
            const jl_datatype_layout_t *layout = ((jl_datatype_t*)eltype)->layout;
            if (layout->first_ptr >= 0) {
                first_ptr = layout->first_ptr;
                npointers = layout->npointers;
                if (layout->flags.fielddesc_type == 2) {
                    pointers = (uint32_t*)jl_dt_layout_ptrs(layout);
                }
                else {
                    pointers = (uint32_t*)alloca(npointers * sizeof(uint32_t));
                    for (int j = 0; j < npointers; j++) {
                        pointers[j] = jl_ptr_offset((jl_datatype_t*)eltype, j);
                    }
                }
            }
        }
        if (needlock) {
            assert(al <= JL_SMALL_BYTE_ALIGNMENT);
            size_t offset = LLT_ALIGN(sizeof(jl_mutex_t), JL_SMALL_BYTE_ALIGNMENT);
            elsz += offset;
            haspadding = 1;
            zi = 1;
        }
    }
    else {
        elsz = sizeof(void*);
        al = elsz;
        zi = 1;
    }

    int arrayelem;
    if (!isunboxed)
        arrayelem = 1;
    else if (isunion)
        arrayelem = 2;
    else
        arrayelem = 0;
    assert(!st->layout);
    st->layout = jl_get_layout(elsz, nfields, npointers, al, haspadding, isbitsegal, arrayelem, NULL, pointers);
    st->zeroinit = zi;
    //st->has_concrete_subtype = 1;
    //st->isbitstype = 0;
    //st->ismutationfree = 0;
    //st->isidentityfree = 0;

    if (jl_is_addrspacecore(addrspace) && jl_unbox_uint8(addrspace) == 0) {
        if (kind == (jl_value_t*)jl_not_atomic_sym || kind == (jl_value_t*)jl_atomic_sym) {
            jl_genericmemory_t *zeroinst = (jl_genericmemory_t*)jl_gc_permobj(LLT_ALIGN(sizeof(jl_genericmemory_t), JL_SMALL_BYTE_ALIGNMENT) + (elsz ? elsz : isunion), st, 0);
            zeroinst->length = 0;
            zeroinst->ptr = (char*)zeroinst + JL_SMALL_BYTE_ALIGNMENT;
            memset(zeroinst->ptr, 0, elsz ? elsz : isunion);
            assert(!st->instance);
            st->instance = (jl_value_t*)zeroinst;
        }
    }
}

void jl_compute_field_offsets(jl_datatype_t *st)
{
    const uint64_t max_offset = (((uint64_t)1) << 32) - 1;
    const uint64_t max_size = max_offset >> 1;

    if (st->name->wrapper == NULL)
        return; // we got called too early--we'll be back
    jl_datatype_t *w = (jl_datatype_t*)jl_unwrap_unionall(st->name->wrapper);
    if (st == w && st->layout) {
        // this check allows us to force re-computation of the layout for some types during init
        st->layout = NULL;
        st->zeroinit = 0;
        st->has_concrete_subtype = 1;
    }
    if (st->name == jl_genericmemory_typename) {
        jl_get_genericmemory_layout(st);
        return;
    }
    int isbitstype = st->isconcretetype && st->name->mayinlinealloc;
    int ismutationfree = !w->layout || !jl_is_layout_opaque(w->layout);
    int isidentityfree = !st->name->mutabl;
    // If layout doesn't depend on type parameters, it's stored in st->name->wrapper
    // and reused by all subtypes.
    if (w->layout) {
        st->layout = w->layout;
        st->zeroinit = w->zeroinit;
        st->has_concrete_subtype = w->has_concrete_subtype;
        if (!jl_is_layout_opaque(st->layout)) { // e.g. jl_simplevector_type
            st->isbitstype = isbitstype && st->layout->npointers == 0;
            jl_maybe_allocate_singleton_instance(st);
        }
        return;
    }
    assert(st->types && w->types);
    size_t i, nfields = jl_svec_len(st->types);
    assert(st->name->n_uninitialized <= nfields);
    if (nfields == 0) {
        // if we have no fields, we can trivially skip the rest
        if (st == jl_symbol_type || st == jl_string_type) {
            // opaque layout - heap-allocated blob
            static const jl_datatype_layout_t opaque_byte_layout = {0, 0, 1, -1, 1, { .haspadding = 0, .fielddesc_type=0, .isbitsegal=1, .arrayelem_isboxed=0, .arrayelem_isunion=0 }};
            st->layout = &opaque_byte_layout;
            return;
        }
        else if (st == jl_simplevector_type || st == jl_module_type) {
            static const jl_datatype_layout_t opaque_ptr_layout = {0, 0, 1, -1, sizeof(void*), { .haspadding = 0, .fielddesc_type=0, .isbitsegal=1, .arrayelem_isboxed=0, .arrayelem_isunion=0 }};
            st->layout = &opaque_ptr_layout;
            return;
        }
        else {
            static const jl_datatype_layout_t singleton_layout = {0, 0, 0, -1, 1, { .haspadding = 0, .fielddesc_type=0, .isbitsegal=1, .arrayelem_isboxed=0, .arrayelem_isunion=0 }};
            st->layout = &singleton_layout;
        }
    }
    else {
        // compute a conservative estimate of whether there could exist an instance of a subtype of this
        for (i = 0; st->has_concrete_subtype && i < nfields - st->name->n_uninitialized; i++) {
            jl_value_t *fld = jl_svecref(st->types, i);
            if (fld == jl_bottom_type)
                st->has_concrete_subtype = 0;
            else
                st->has_concrete_subtype = !jl_is_datatype(fld) || ((jl_datatype_t *)fld)->has_concrete_subtype;
        }
        // compute layout for the wrapper object if the field types have no free variables
        if (!st->isconcretetype && !jl_has_fixed_layout(st)) {
            assert(st == w); // otherwise caller should not have requested this layout
            return;
        }
    }

    for (i = 0; (isbitstype || isidentityfree || ismutationfree) && i < nfields; i++) {
        jl_value_t *fld = jl_field_type(st, i);
        isbitstype &= jl_isbits(fld);
        ismutationfree &= (!st->name->mutabl || jl_field_isconst(st, i)) && is_type_mutationfree(fld);
        isidentityfree &= is_type_identityfree(fld);
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
        int isbitsegal = 1;
        int homogeneous = 1;
        int needlock = 0;
        uint32_t npointers = 0;
        jl_value_t *firstty = jl_field_type(st, 0);
        for (i = 0; i < nfields; i++) {
            jl_value_t *fld = jl_field_type(st, i);
            int isatomic = jl_field_isatomic(st, i);
            size_t fsz = 0, al = 1;
            if (jl_islayout_inline(fld, &fsz, &al) && (!isatomic || jl_is_datatype(fld))) { // aka jl_datatype_isinlinealloc
                if (__unlikely(fsz > max_size))
                    // Should never happen
                    throw_ovf(should_malloc, desc, st, fsz);
                desc[i].isptr = 0;
                if (jl_is_uniontype(fld)) {
                    fsz += 1; // selector byte
                    zeroinit = 1;
                    // TODO: Some unions could be bits comparable.
                    isbitsegal = 0;
                }
                else {
                    uint32_t fld_npointers = ((jl_datatype_t*)fld)->layout->npointers;
                    if (((jl_datatype_t*)fld)->layout->flags.haspadding)
                        haspadding = 1;
                    if (!((jl_datatype_t*)fld)->layout->flags.isbitsegal)
                        isbitsegal = 0;
                    if (i >= nfields - st->name->n_uninitialized && fld_npointers &&
                        fld_npointers * sizeof(void*) != fsz) {
                        // For field types that contain pointers, we allow inlinealloc
                        // as long as the field type itself is always fully initialized.
                        // In such a case, we use the first pointer in the inlined field
                        // as the #undef marker (if it is zero, we treat the whole inline
                        // struct as #undef). However, we do not zero-initialize the whole
                        // struct, so the non-pointer parts of the inline allocation may
                        // be arbitrary, but still need to compare egal (because all #undef)
                        // representations are egal. Because of this, we cannot bitscompare
                        // them.
                        // TODO: Consider zero-initializing the whole struct.
                        isbitsegal = 0;
                    }
                    if (!zeroinit)
                        zeroinit = ((jl_datatype_t*)fld)->zeroinit;
                    npointers += fld_npointers;
                }
            }
            else {
                fsz = sizeof(void*);
                al = fsz;
                if (al > MAX_ALIGN)
                    al = MAX_ALIGN;
                desc[i].isptr = 1;
                zeroinit = 1;
                npointers++;
                if (!jl_pointer_egal(fld)) {
                    isbitsegal = 0;
                }
            }
            if (isatomic && fsz > MAX_ATOMIC_SIZE)
                needlock = 1;
            if (isatomic && fsz <= MAX_ATOMIC_SIZE)
                al = fsz = next_power_of_two(fsz);
            if (al != 0) {
                size_t alsz = LLT_ALIGN(sz, al);
                if (alsz != sz)
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
        if (needlock) {
            size_t offset = LLT_ALIGN(sizeof(jl_mutex_t), alignm);
            for (i = 0; i < nfields; i++) {
                desc[i].offset += offset;
            }
            if (__unlikely(max_offset - sz < offset))
                throw_ovf(should_malloc, desc, st, sz);
            sz += offset;
            haspadding = 1;
        }
        if (homogeneous && jl_is_tuple_type(st)) {
            // Some tuples become LLVM vectors with stronger alignment than what was calculated above.
            unsigned al = jl_special_vector_alignment(nfields, firstty);
            assert(al % alignm == 0);
            if (al > alignm)
                alignm = al;
        }
        if (LLT_ALIGN(sz, alignm) > sz) {
            haspadding = 1;
            sz = LLT_ALIGN(sz, alignm);
        }
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
        st->layout = jl_get_layout(sz, nfields, npointers, alignm, haspadding, isbitsegal, 0, desc, pointers);
        if (should_malloc) {
            free(desc);
            if (npointers)
                free(pointers);
        }
        st->zeroinit = zeroinit;
    }
    // now finish deciding if this instantiation qualifies for special properties
    assert(!isbitstype || st->layout->npointers == 0); // the definition of isbits
    st->isbitstype = isbitstype;
    st->ismutationfree = ismutationfree;
    st->isidentityfree = isidentityfree;
    jl_maybe_allocate_singleton_instance(st);
    return;
}

JL_DLLEXPORT jl_datatype_t *jl_new_datatype(
        jl_sym_t *name,
        jl_module_t *module,
        jl_datatype_t *super,
        jl_svec_t *parameters,
        jl_svec_t *fnames,
        jl_svec_t *ftypes,
        jl_svec_t *fattrs,
        int abstract, int mutabl,
        int ninitialized)
{
    jl_datatype_t *t = NULL;
    jl_typename_t *tn = NULL;
    JL_GC_PUSH2(&t, &tn);

    assert(parameters && fnames);

    // init enough before possibly calling jl_new_typename_in
    t = jl_new_uninitialized_datatype();
    t->super = super;
    if (super != NULL) jl_gc_wb(t, t->super);
    t->parameters = parameters;
    jl_gc_wb(t, t->parameters);
    t->types = ftypes;
    if (ftypes != NULL) jl_gc_wb(t, t->types);

    t->name = NULL;
    if (jl_is_typename(name)) {
        // This code-path is used by the Serialization module to bypass normal expectations
        tn = (jl_typename_t*)name;
        tn->abstract = abstract;
        tn->mutabl = mutabl;
    }
    else {
        tn = jl_new_typename_in((jl_sym_t*)name, module, abstract, mutabl);
    }
    t->name = tn;
    jl_gc_wb(t, t->name);
    t->name->names = fnames;
    jl_gc_wb(t->name, t->name->names);
    tn->n_uninitialized = jl_svec_len(fnames) - ninitialized;

    uint32_t *volatile atomicfields = NULL;
    uint32_t *volatile constfields = NULL;
    int i;
    JL_TRY {
        for (i = 0; i + 1 < jl_svec_len(fattrs); i += 2) {
            jl_value_t *fldi = jl_svecref(fattrs, i);
            jl_sym_t *attr = (jl_sym_t*)jl_svecref(fattrs, i + 1);
            JL_TYPECHK(typeassert, long, fldi);
            JL_TYPECHK(typeassert, symbol, (jl_value_t*)attr);
            size_t fldn = jl_unbox_long(fldi);
            if (fldn < 1 || fldn > jl_svec_len(fnames))
                jl_errorf("invalid field attribute %lld", (long long)fldn);
            fldn--;
            if (attr == jl_atomic_sym) {
                if (!mutabl)
                    jl_errorf("invalid field attribute atomic for immutable struct");
                if (atomicfields == NULL) {
                    size_t nb = (jl_svec_len(fnames) + 31) / 32 * sizeof(uint32_t);
                    atomicfields = (uint32_t*)malloc_s(nb);
                    memset(atomicfields, 0, nb);
                }
                atomicfields[fldn / 32] |= 1 << (fldn % 32);
            }
            else if (attr == jl_const_sym) {
                if (!mutabl)
                    jl_errorf("invalid field attribute const for immutable struct");
                if (constfields == NULL) {
                    size_t nb = (jl_svec_len(fnames) + 31) / 32 * sizeof(uint32_t);
                    constfields = (uint32_t*)malloc_s(nb);
                    memset(constfields, 0, nb);
                }
                constfields[fldn / 32] |= 1 << (fldn % 32);
            }
            else {
                jl_errorf("invalid field attribute %s", jl_symbol_name(attr));
            }
        }
    }
    JL_CATCH {
        free(atomicfields);
        free(constfields);
        jl_rethrow();
    }
    tn->atomicfields = atomicfields;
    tn->constfields = constfields;

    if (t->name->wrapper == NULL) {
        t->name->wrapper = (jl_value_t*)t;
        jl_gc_wb(t->name, t);
        int i, np = jl_svec_len(parameters);
        for (i = np - 1; i >= 0; i--) {
            t->name->wrapper = jl_new_struct(jl_unionall_type, jl_svecref(parameters, i), t->name->wrapper);
            jl_gc_wb(t->name, t->name->wrapper);
        }
        if (!mutabl && !abstract && ftypes != NULL)
            tn->mayinlinealloc = 1;
    }
    jl_precompute_memoized_dt(t, 0);

    if (!abstract && t->types != NULL)
        jl_compute_field_offsets(t);

    JL_GC_POP();
    return t;
}

JL_DLLEXPORT jl_datatype_t *jl_new_primitivetype(jl_value_t *name, jl_module_t *module,
                                                 jl_datatype_t *super,
                                                 jl_svec_t *parameters, size_t nbits)
{
    jl_datatype_t *bt = jl_new_datatype((jl_sym_t*)name, module, super, parameters,
                                        jl_emptysvec, jl_emptysvec, jl_emptysvec, 0, 0, 0);
    uint32_t nbytes = (nbits + 7) / 8;
    uint32_t alignm = next_power_of_two(nbytes);
# if defined(_CPU_X86_) && !defined(_OS_WINDOWS_)
    // datalayout strings are often weird: on 64-bit they usually follow fairly simple rules,
    // but on x86 32 bit platforms, sometimes 5 to 8 byte types are
    // 32-bit aligned even though the MAX_ALIGN (for types 9+ bytes) is 16
    // (except for f80 which is align 4 on Mingw, Linux, and BSDs--but align 16 on MSVC and Darwin)
    // https://llvm.org/doxygen/ARMTargetMachine_8cpp.html#adb29b487708f0dc2a940345b68649270
    // https://llvm.org/doxygen/AArch64TargetMachine_8cpp.html#a003a58caf135efbf7273c5ed84e700d7
    // https://llvm.org/doxygen/X86TargetMachine_8cpp.html#aefdbcd6131ef195da070cef7fdaf0532
    // 32-bit alignment is weird
    if (alignm == 8)
        alignm = 4;
# endif
    if (alignm > MAX_ALIGN)
        alignm = MAX_ALIGN;
    // memoize isprimitivetype, since it is much easier than checking
    // (dta->name->names == svec() && dta->layout && dta->layout->size != 0)
    // and we easily have a free bit for it in the DataType flags
    bt->isprimitivetype = 1;
    bt->ismutationfree = 1;
    bt->isidentityfree = 1;
    bt->isbitstype = (parameters == jl_emptysvec);
    bt->layout = jl_get_layout(nbytes, 0, 0, alignm, 0, 1, 0, NULL, NULL);
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
      jl_emptysvec, jl_emptysvec, jl_emptysvec, jl_emptysvec, 0, 1, 0);
    jl_datatype_layout_t *layout = (jl_datatype_layout_t *)
      jl_gc_perm_alloc(sizeof(jl_datatype_layout_t) + sizeof(jl_fielddescdyn_t),
        0, 4, 0);
    layout->size = large ? GC_MAX_SZCLASS+1 : 0;
    layout->nfields = 0;
    layout->alignment = sizeof(void *);
    layout->npointers = haspointers;
    layout->flags.haspadding = 1;
    layout->flags.isbitsegal = 0;
    layout->flags.fielddesc_type = 3;
    layout->flags.padding = 0;
    layout->flags.arrayelem_isboxed = 0;
    layout->flags.arrayelem_isunion = 0;
    jl_fielddescdyn_t * desc =
      (jl_fielddescdyn_t *) ((char *)layout + sizeof(*layout));
    desc->markfunc = markfunc;
    desc->sweepfunc = sweepfunc;
    bt->layout = layout;
    bt->instance = NULL;
    return bt;
}

JL_DLLEXPORT int jl_reinit_foreign_type(jl_datatype_t *dt,
                                        jl_markfunc_t markfunc,
                                        jl_sweepfunc_t sweepfunc)
{
    if (!jl_is_foreign_type(dt))
        return 0;
    const jl_datatype_layout_t *layout = dt->layout;
    jl_fielddescdyn_t * desc =
      (jl_fielddescdyn_t *) ((char *)layout + sizeof(*layout));
    assert(!desc->markfunc);
    assert(!desc->sweepfunc);
    desc->markfunc = markfunc;
    desc->sweepfunc = sweepfunc;
    return 1;
}

JL_DLLEXPORT int jl_is_foreign_type(jl_datatype_t *dt)
{
    return jl_is_datatype(dt) && dt->layout && dt->layout->flags.fielddesc_type == 3;
}

// bits constructors ----------------------------------------------------------

#if MAX_ATOMIC_SIZE > MAX_POINTERATOMIC_SIZE
#error MAX_ATOMIC_SIZE too large
#endif
#if MAX_ATOMIC_SIZE >= 16 && !defined(_P64)
#error 12 byte GC pool size alignment unimplemented for 32-bit
#endif
#if MAX_POINTERATOMIC_SIZE > 16
#error MAX_POINTERATOMIC_SIZE too large
#endif
#if BYTE_ORDER != LITTLE_ENDIAN
#error using masks for atomics (instead of memcpy like nb == 16) assumes little endian
#endif

#if MAX_POINTERATOMIC_SIZE >= 16
typedef struct _jl_uint128_t {
    alignas(16) uint64_t a;
    uint64_t b;
} jl_uint128_t;
#endif

static inline uint32_t zext_read32(const jl_value_t *x, size_t nb) JL_NOTSAFEPOINT
{
    uint32_t y = *(uint32_t*)x;
    if (nb == 4)
        return y;
    else // if (nb == 3)
        return 0xffffffu & y;
}

#if MAX_POINTERATOMIC_SIZE >= 8
static inline uint64_t zext_read64(const jl_value_t *x, size_t nb) JL_NOTSAFEPOINT
{
    uint64_t y = *(uint64_t*)x;
    if (nb == 8)
        return y;
    else if (nb == 7)
        return 0xffffffffffffffu & y;
    else if (nb == 6)
        return 0xffffffffffffu & y;
    else // if (nb == 5)
        return 0xffffffffffu & y;
}
#endif

#if MAX_POINTERATOMIC_SIZE >= 16
static inline jl_uint128_t zext_read128(const jl_value_t *x, size_t nb) JL_NOTSAFEPOINT
{
    jl_uint128_t y = {0};
    if (nb == 16)
        y = *(jl_uint128_t*)x;
    else
        memcpy(&y, x, nb);
    return y;
}
#endif

JL_DLLEXPORT jl_value_t *jl_new_bits(jl_value_t *dt, const void *data)
{
    // data may not have the alignment required by the size
    // but will always have the alignment required by the datatype
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

    assert(!bt->smalltag);
    jl_task_t *ct = jl_current_task;
    jl_value_t *v = jl_gc_alloc(ct->ptls, nb, bt);
    // TODO: make this a memmove_refs if relevant
    memcpy(jl_assume_aligned(v, sizeof(void*)), data, nb);
    return v;
}

JL_DLLEXPORT jl_value_t *jl_atomic_new_bits(jl_value_t *dt, const char *data)
{
    // data must have the required alignment for an atomic of the given size
    assert(jl_is_datatype(dt));
    jl_datatype_t *bt = (jl_datatype_t*)dt;
    size_t nb = jl_datatype_size(bt);
    // some types have special pools to minimize allocations
    if (nb == 0)               return jl_new_struct_uninit(bt); // returns bt->instance
    if (bt == jl_bool_type)    return (1 & jl_atomic_load((_Atomic(int8_t)*)data)) ? jl_true : jl_false;
    if (bt == jl_uint8_type)   return jl_box_uint8(jl_atomic_load((_Atomic(uint8_t)*)data));
    if (bt == jl_int64_type)   return jl_box_int64(jl_atomic_load((_Atomic(int64_t)*)data));
    if (bt == jl_int32_type)   return jl_box_int32(jl_atomic_load((_Atomic(int32_t)*)data));
    if (bt == jl_int8_type)    return jl_box_int8(jl_atomic_load((_Atomic(int8_t)*)data));
    if (bt == jl_int16_type)   return jl_box_int16(jl_atomic_load((_Atomic(int16_t)*)data));
    if (bt == jl_uint64_type)  return jl_box_uint64(jl_atomic_load((_Atomic(uint64_t)*)data));
    if (bt == jl_uint32_type)  return jl_box_uint32(jl_atomic_load((_Atomic(uint32_t)*)data));
    if (bt == jl_uint16_type)  return jl_box_uint16(jl_atomic_load((_Atomic(uint16_t)*)data));
    if (bt == jl_char_type)    return jl_box_char(jl_atomic_load((_Atomic(uint32_t)*)data));

    assert(!bt->smalltag);
    jl_task_t *ct = jl_current_task;
    jl_value_t *v = jl_gc_alloc(ct->ptls, nb, bt);
    // data is aligned to the power of two,
    // we will write too much of v, but the padding should exist
    if (nb == 1)
        *(uint8_t*) v = jl_atomic_load((_Atomic(uint8_t)*)data);
    else if (nb <= 2)
        *(uint16_t*)v = jl_atomic_load((_Atomic(uint16_t)*)data);
    else if (nb <= 4)
        *(uint32_t*)v = jl_atomic_load((_Atomic(uint32_t)*)data);
#if MAX_POINTERATOMIC_SIZE >= 8
    else if (nb <= 8)
        *(uint64_t*)v = jl_atomic_load((_Atomic(uint64_t)*)data);
#endif
#if MAX_POINTERATOMIC_SIZE >= 16
    else if (nb <= 16)
        *(jl_uint128_t*)v = jl_atomic_load((_Atomic(jl_uint128_t)*)data);
#endif
    else
        abort();
    return v;
}

JL_DLLEXPORT void jl_atomic_store_bits(char *dst, const jl_value_t *src, int nb)
{
    // dst must have the required alignment for an atomic of the given size
    // src must be aligned by the GC
    // we may therefore read too much from src, but will zero the excess bits
    // before the store (so that we can get faster cmpswap later)
    if (nb == 0)
        ;
    else if (nb == 1)
        jl_atomic_store((_Atomic(uint8_t)*)dst, *(uint8_t*)src);
    else if (nb == 2)
        jl_atomic_store((_Atomic(uint16_t)*)dst, *(uint16_t*)src);
    else if (nb <= 4)
        jl_atomic_store((_Atomic(uint32_t)*)dst, zext_read32(src, nb));
#if MAX_POINTERATOMIC_SIZE >= 8
    else if (nb <= 8)
        jl_atomic_store((_Atomic(uint64_t)*)dst, zext_read64(src, nb));
#endif
#if MAX_POINTERATOMIC_SIZE >= 16
    else if (nb <= 16)
        jl_atomic_store((_Atomic(jl_uint128_t)*)dst, zext_read128(src, nb));
#endif
    else
        abort();
}

JL_DLLEXPORT jl_value_t *jl_atomic_swap_bits(jl_value_t *dt, char *dst, const jl_value_t *src, int nb)
{
    // dst must have the required alignment for an atomic of the given size
    assert(jl_is_datatype(dt));
    jl_datatype_t *bt = (jl_datatype_t*)dt;
    // some types have special pools to minimize allocations
    if (nb == 0)               return jl_new_struct_uninit(bt); // returns bt->instance
    if (bt == jl_bool_type)    return (1 & jl_atomic_exchange((_Atomic(int8_t)*)dst, 1 & *(int8_t*)src)) ? jl_true : jl_false;
    if (bt == jl_uint8_type)   return jl_box_uint8(jl_atomic_exchange((_Atomic(uint8_t)*)dst, *(int8_t*)src));
    if (bt == jl_int64_type)   return jl_box_int64(jl_atomic_exchange((_Atomic(int64_t)*)dst, *(int64_t*)src));
    if (bt == jl_int32_type)   return jl_box_int32(jl_atomic_exchange((_Atomic(int32_t)*)dst, *(int32_t*)src));
    if (bt == jl_int8_type)    return jl_box_int8(jl_atomic_exchange((_Atomic(int8_t)*)dst, *(int8_t*)src));
    if (bt == jl_int16_type)   return jl_box_int16(jl_atomic_exchange((_Atomic(int16_t)*)dst, *(int16_t*)src));
    if (bt == jl_uint64_type)  return jl_box_uint64(jl_atomic_exchange((_Atomic(uint64_t)*)dst, *(uint64_t*)src));
    if (bt == jl_uint32_type)  return jl_box_uint32(jl_atomic_exchange((_Atomic(uint32_t)*)dst, *(uint32_t*)src));
    if (bt == jl_uint16_type)  return jl_box_uint16(jl_atomic_exchange((_Atomic(uint16_t)*)dst, *(uint16_t*)src));
    if (bt == jl_char_type)    return jl_box_char(jl_atomic_exchange((_Atomic(uint32_t)*)dst, *(uint32_t*)src));

    assert(!bt->smalltag);
    jl_task_t *ct = jl_current_task;
    jl_value_t *v = jl_gc_alloc(ct->ptls, jl_datatype_size(bt), bt);
    if (nb == 1)
        *(uint8_t*)v = jl_atomic_exchange((_Atomic(uint8_t)*)dst, *(uint8_t*)src);
    else if (nb == 2)
        *(uint16_t*)v = jl_atomic_exchange((_Atomic(uint16_t)*)dst, *(uint16_t*)src);
    else if (nb <= 4)
        *(uint32_t*)v = jl_atomic_exchange((_Atomic(uint32_t)*)dst, zext_read32(src, nb));
#if MAX_POINTERATOMIC_SIZE >= 8
    else if (nb <= 8)
        *(uint64_t*)v = jl_atomic_exchange((_Atomic(uint64_t)*)dst, zext_read64(src, nb));
#endif
#if MAX_POINTERATOMIC_SIZE >= 16
    else if (nb <= 16)
        *(jl_uint128_t*)v = jl_atomic_exchange((_Atomic(jl_uint128_t)*)dst, zext_read128(src, nb));
#endif
    else
        abort();
    return v;
}

JL_DLLEXPORT int jl_atomic_bool_cmpswap_bits(char *dst, const jl_value_t *expected, const jl_value_t *src, int nb)
{
    // dst must have the required alignment for an atomic of the given size
    // n.b.: this can spuriously fail if there are padding bits, the caller should deal with that
    int success;
    if (nb == 0) {
        success = 1;
    }
    else if (nb == 1) {
        uint8_t y = *(uint8_t*)expected;
        success = jl_atomic_cmpswap((_Atomic(uint8_t)*)dst, &y, *(uint8_t*)src);
    }
    else if (nb == 2) {
        uint16_t y = *(uint16_t*)expected;
        success = jl_atomic_cmpswap((_Atomic(uint16_t)*)dst, &y, *(uint16_t*)src);
    }
    else if (nb <= 4) {
        uint32_t y = zext_read32(expected, nb);
        uint32_t z = zext_read32(src, nb);
        success = jl_atomic_cmpswap((_Atomic(uint32_t)*)dst, &y, z);
    }
#if MAX_POINTERATOMIC_SIZE >= 8
    else if (nb <= 8) {
        uint64_t y = zext_read64(expected, nb);
        uint64_t z = zext_read64(src, nb);
        success = jl_atomic_cmpswap((_Atomic(uint64_t)*)dst, &y, z);
    }
#endif
#if MAX_POINTERATOMIC_SIZE >= 16
    else if (nb <= 16) {
        jl_uint128_t y = zext_read128(expected, nb);
        jl_uint128_t z = zext_read128(src, nb);
        success = jl_atomic_cmpswap((_Atomic(jl_uint128_t)*)dst, &y, z);
    }
#endif
    else {
        abort();
    }
    return success;
}

JL_DLLEXPORT int jl_atomic_cmpswap_bits(jl_datatype_t *dt, jl_value_t *y /* pre-allocated output */, char *dst, const jl_value_t *expected, const jl_value_t *src, int nb)
{
    // dst must have the required alignment for an atomic of the given size
    // n.b.: this does not spuriously fail if there are padding bits
    int success;
    jl_datatype_t *et = (jl_datatype_t*)jl_typeof(expected);
    if (nb == 0) {
        success = (dt == et);
    }
    else if (nb == 1) {
        uint8_t *y8 = (uint8_t*)y;
        assert(dt->layout->flags.isbitsegal && !dt->layout->flags.haspadding);
        if (dt == et) {
            *y8 = *(uint8_t*)expected;
            uint8_t z8 = *(uint8_t*)src;
            success = jl_atomic_cmpswap((_Atomic(uint8_t)*)dst, y8, z8);
        }
        else {
            *y8 = jl_atomic_load((_Atomic(uint8_t)*)dst);
            success = 0;
        }
    }
    else if (nb == 2) {
        uint16_t *y16 = (uint16_t*)y;
        assert(dt->layout->flags.isbitsegal && !dt->layout->flags.haspadding);
        if (dt == et) {
            *y16 = *(uint16_t*)expected;
            uint16_t z16 = *(uint16_t*)src;
            success = jl_atomic_cmpswap((_Atomic(uint16_t)*)dst, y16, z16);
        }
        else {
            *y16 = jl_atomic_load((_Atomic(uint16_t)*)dst);
            success = 0;
        }
    }
    else if (nb <= 4) {
        uint32_t *y32 = (uint32_t*)y;
        if (dt == et) {
            *y32 = zext_read32(expected, nb);
            uint32_t z32 = zext_read32(src, nb);
            while (1) {
                success = jl_atomic_cmpswap((_Atomic(uint32_t)*)dst, y32, z32);
                if (success || (dt->layout->flags.isbitsegal && !dt->layout->flags.haspadding) || !jl_egal__bits(y, expected, dt))
                    break;
            }
        }
        else {
            *y32 = jl_atomic_load((_Atomic(uint32_t)*)dst);
            success = 0;
        }
    }
#if MAX_POINTERATOMIC_SIZE >= 8
    else if (nb <= 8) {
        uint64_t *y64 = (uint64_t*)y;
        if (dt == et) {
            *y64 = zext_read64(expected, nb);
            uint64_t z64 = zext_read64(src, nb);
            while (1) {
                success = jl_atomic_cmpswap((_Atomic(uint64_t)*)dst, y64, z64);
                if (success || (dt->layout->flags.isbitsegal && !dt->layout->flags.haspadding) || !jl_egal__bits(y, expected, dt))
                    break;
            }
        }
        else {
            *y64 = jl_atomic_load((_Atomic(uint64_t)*)dst);
            success = 0;
        }
    }
#endif
#if MAX_POINTERATOMIC_SIZE >= 16
    else if (nb <= 16) {
        jl_uint128_t *y128 = (jl_uint128_t*)y;
        if (dt == et) {
            *y128 = zext_read128(expected, nb);
            jl_uint128_t z128 = zext_read128(src, nb);
            while (1) {
                success = jl_atomic_cmpswap((_Atomic(jl_uint128_t)*)dst, y128, z128);
                if (success || (dt->layout->flags.isbitsegal && !dt->layout->flags.haspadding) || !jl_egal__bits(y, expected, dt))
                    break;
            }
        }
        else {
            *y128 = jl_atomic_load((_Atomic(jl_uint128_t)*)dst);
            success = 0;
        }
    }
#endif
    else {
        abort();
    }
    return success;
}

JL_DLLEXPORT int jl_atomic_storeonce_bits(jl_datatype_t *dt, char *dst, const jl_value_t *src, int nb)
{
    // dst must have the required alignment for an atomic of the given size
    // n.b.: this does not spuriously fail
    // n.b.: hasptr == 1 therefore nb >= sizeof(void*), because ((jl_datatype_t*)ty)->layout->has_ptr >= 0
    int success;
#ifdef _P64
    if (nb <= 4) {
        uint32_t y32 = 0;
        uint32_t z32 = zext_read32(src, nb);
        success = jl_atomic_cmpswap((_Atomic(uint32_t)*)dst, &y32, z32);
    }
#if MAX_POINTERATOMIC_SIZE >= 8
    else if (nb <= 8) {
        uint64_t y64 = 0;
        uint64_t z64 = zext_read64(src, nb);
        while (1) {
            success = jl_atomic_cmpswap((_Atomic(uint64_t)*)dst, &y64, z64);
            if (success || undefref_check(dt, (jl_value_t*)&y64) != NULL)
                break;
        }
    }
#endif
#else
    if (nb <= 8) {
        uint64_t y64 = 0;
        uint64_t z64 = zext_read64(src, nb);
        success = jl_atomic_cmpswap((_Atomic(uint64_t)*)dst, &y64, z64);
    }
#endif
#if MAX_POINTERATOMIC_SIZE >= 16
    else if (nb <= 16) {
        jl_uint128_t y128 = {0};
        jl_uint128_t z128 = zext_read128(src, nb);
        while (1) {
            success = jl_atomic_cmpswap((_Atomic(jl_uint128_t)*)dst, &y128, z128);
            if (success || undefref_check(dt, (jl_value_t*)&y128) != NULL)
                break;
        }
    }
#endif
    else {
        abort();
    }
    return success;
}

#define PERMBOXN_FUNC(nb)                                                  \
    jl_value_t *jl_permbox##nb(jl_datatype_t *t, uintptr_t tag, uint##nb##_t x) \
    {   /* n.b. t must be a concrete isbits datatype of the right size */  \
        jl_value_t *v = jl_gc_permobj(LLT_ALIGN(nb, sizeof(void*)), t, 0); \
        if (tag) jl_set_typetagof(v, tag, GC_OLD_MARKED);                  \
        *(uint##nb##_t*)jl_data_ptr(v) = x;                                \
        return v;                                                          \
    }
PERMBOXN_FUNC(8)
PERMBOXN_FUNC(16)
PERMBOXN_FUNC(32)
PERMBOXN_FUNC(64)

#define UNBOX_FUNC(j_type,c_type)                                       \
    JL_DLLEXPORT c_type jl_unbox_##j_type(jl_value_t *v)                \
    {                                                                   \
        assert(jl_is_primitivetype(jl_typeof(v)));                      \
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

#define BOX_FUNC(typ,c_type,pfx)                                        \
    JL_DLLEXPORT jl_value_t *pfx##_##typ(c_type x)                      \
    {                                                                   \
        jl_task_t *ct = jl_current_task;                                \
        jl_value_t *v = jl_gc_alloc(ct->ptls, LLT_ALIGN(sizeof(x), sizeof(void*)), \
                                    jl_##typ##_type);                   \
        *(c_type*)jl_data_ptr(v) = x;                                   \
        return v;                                                       \
    }
BOX_FUNC(float32, float,  jl_box)
BOX_FUNC(float64, double, jl_box)
BOX_FUNC(voidpointer, void*,  jl_box)
BOX_FUNC(uint8pointer, uint8_t*,  jl_box)

#define NBOX_C 1024

// some shims to support UIBOX_FUNC definition
#define jl_ssavalue_tag (((uintptr_t)jl_ssavalue_type) >> 4)
#define jl_slotnumber_tag (((uintptr_t)jl_slotnumber_type) >> 4)

#define SIBOX_FUNC(typ,c_type)                                          \
    static jl_value_t *boxed_##typ##_cache[NBOX_C];                     \
    JL_DLLEXPORT jl_value_t *jl_box_##typ(c_type x)                     \
    {                                                                   \
        jl_task_t *ct = jl_current_task;                                \
        c_type idx = x+NBOX_C/2;                                        \
        if ((u##c_type)idx < (u##c_type)NBOX_C)                         \
            return boxed_##typ##_cache[idx];                            \
        jl_value_t *v = jl_gc_alloc(ct->ptls, LLT_ALIGN(sizeof(x), sizeof(void*)), \
                                    jl_##typ##_type);                   \
        jl_set_typetagof(v, jl_##typ##_tag, 0);                         \
        *(c_type*)jl_data_ptr(v) = x;                                   \
        return v;                                                       \
    }
#define UIBOX_FUNC(typ,c_type)                                          \
    static jl_value_t *boxed_##typ##_cache[NBOX_C];                     \
    JL_DLLEXPORT jl_value_t *jl_box_##typ(c_type x)                     \
    {                                                                   \
        jl_task_t *ct = jl_current_task;                                \
        if (x < NBOX_C)                                                 \
            return boxed_##typ##_cache[x];                              \
        jl_value_t *v = jl_gc_alloc(ct->ptls, LLT_ALIGN(sizeof(x), sizeof(void*)), \
                                    jl_##typ##_type);                   \
        jl_set_typetagof(v, jl_##typ##_tag, 0);                         \
        *(c_type*)jl_data_ptr(v) = x;                                   \
        return v;                                                       \
    }
SIBOX_FUNC(int16,  int16_t)
SIBOX_FUNC(int32,  int32_t)
UIBOX_FUNC(uint16, uint16_t)
UIBOX_FUNC(uint32, uint32_t)
UIBOX_FUNC(ssavalue, size_t)
UIBOX_FUNC(slotnumber, size_t)
SIBOX_FUNC(int64,  int64_t)
UIBOX_FUNC(uint64, uint64_t)

static jl_value_t *boxed_char_cache[128];
JL_DLLEXPORT jl_value_t *jl_box_char(uint32_t x)
{
    jl_task_t *ct = jl_current_task;
    uint32_t u = bswap_32(x);
    if (u < 128)
        return boxed_char_cache[(uint8_t)u];
    jl_value_t *v = jl_gc_alloc(ct->ptls, sizeof(void*), jl_char_type);
    jl_set_typetagof(v, jl_char_tag, 0);
    *(uint32_t*)jl_data_ptr(v) = x;
    return v;
}

JL_DLLEXPORT jl_value_t *jl_boxed_int8_cache[256];
JL_DLLEXPORT jl_value_t *jl_box_int8(int8_t x)
{
    return jl_boxed_int8_cache[(uint8_t)x];
}
JL_DLLEXPORT jl_value_t *jl_boxed_uint8_cache[256];
JL_DLLEXPORT jl_value_t *jl_box_uint8(uint8_t x)
{
    return jl_boxed_uint8_cache[x];
}

void jl_init_int32_int64_cache(void)
{
    int64_t i;
    for(i=0; i < NBOX_C; i++) {
        boxed_int32_cache[i]  = jl_permbox32(jl_int32_type, jl_int32_tag, i-NBOX_C/2);
        boxed_int64_cache[i]  = jl_permbox64(jl_int64_type, jl_int64_tag, i-NBOX_C/2);
        boxed_uint16_cache[i] = jl_permbox16(jl_uint16_type, jl_uint16_tag, i);
        boxed_uint64_cache[i] = jl_permbox64(jl_uint64_type, jl_uint64_tag, i);
        boxed_uint32_cache[i] = jl_permbox32(jl_uint32_type, jl_uint32_tag, i);
#ifdef _P64
        boxed_ssavalue_cache[i] = jl_permbox64(jl_ssavalue_type, 0, i);
        boxed_slotnumber_cache[i] = jl_permbox64(jl_slotnumber_type, 0, i);
#else
        boxed_ssavalue_cache[i] = jl_permbox32(jl_ssavalue_type, 0, i);
        boxed_slotnumber_cache[i] = jl_permbox32(jl_slotnumber_type, 0, i);
#endif
    }
    for(i=0; i < 256; i++) {
        jl_boxed_uint8_cache[i] = jl_permbox8(jl_uint8_type, jl_uint8_tag, i);
    }
}

void jl_init_box_caches(void)
{
    uint32_t i;
    for (i = 0; i < 128; i++) {
        boxed_char_cache[i] = jl_permbox32(jl_char_type, jl_char_tag, i << 24);
    }
    for (i = 0; i < 256; i++) {
        jl_boxed_int8_cache[i] = jl_permbox8(jl_int8_type, jl_int8_tag, i);
    }
    for (i = 0; i < NBOX_C; i++) {
        boxed_int16_cache[i]  = jl_permbox16(jl_int16_type, jl_int16_tag, i-NBOX_C/2);
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
    jl_task_t *ct = jl_current_task;
    if (!jl_is_datatype(type) || !type->isconcretetype || type->layout == NULL || jl_is_layout_opaque(type->layout)) {
        jl_type_error("new", (jl_value_t*)jl_datatype_type, (jl_value_t*)type);
    }
    if (type->instance != NULL)
        return type->instance;
    va_list args;
    size_t i, nf = jl_datatype_nfields(type);
    va_start(args, type);
    jl_value_t *jv = jl_gc_alloc(ct->ptls, jl_datatype_size(type), type);
    if (type->smalltag) // TODO: move to callers?
        jl_set_typetagof(jv, type->smalltag, 0);
    if (nf > 0 && jl_field_offset(type, 0) != 0) {
        memset(jv, 0, jl_field_offset(type, 0));
    }
    for (i = 0; i < nf; i++) {
        set_nth_field(type, jv, i, va_arg(args, jl_value_t*), 0);
    }
    va_end(args);
    return jv;
}

JL_DLLEXPORT jl_value_t *jl_new_structv(jl_datatype_t *type, jl_value_t **args, uint32_t na)
{
    jl_task_t *ct = jl_current_task;
    if (!jl_is_datatype(type) || !type->isconcretetype || type->layout == NULL || jl_is_layout_opaque(type->layout)) {
        jl_type_error("new", (jl_value_t*)jl_datatype_type, (jl_value_t*)type);
    }
    size_t nf = jl_datatype_nfields(type);
    if (nf - type->name->n_uninitialized > na || na > nf)
        jl_error("invalid struct allocation");
    for (size_t i = 0; i < na; i++) {
        jl_value_t *ft = jl_field_type_concrete(type, i);
        if (!jl_isa(args[i], ft))
            jl_type_error("new", ft, args[i]);
    }
    if (type->instance != NULL)
        return type->instance;
    jl_value_t *jv = jl_gc_alloc(ct->ptls, jl_datatype_size(type), type);
    if (type->smalltag) // TODO: do we need this?
        jl_set_typetagof(jv, type->smalltag, 0);
    if (jl_datatype_nfields(type) > 0) {
        if (jl_field_offset(type, 0) != 0) {
            memset(jl_data_ptr(jv), 0, jl_field_offset(type, 0));
        }
        JL_GC_PUSH1(&jv);
        for (size_t i = 0; i < na; i++) {
            set_nth_field(type, jv, i, args[i], 0);
        }
        if (na < jl_datatype_nfields(type)) {
            char *data = (char*)jl_data_ptr(jv);
            size_t offs = jl_field_offset(type, na);
            memset(data + offs, 0, jl_datatype_size(type) - offs);
        }
        JL_GC_POP();
    }
    return jv;
}

JL_DLLEXPORT jl_value_t *jl_new_structt(jl_datatype_t *type, jl_value_t *tup)
{
    jl_task_t *ct = jl_current_task;
    if (!jl_is_tuple(tup))
        jl_type_error("new", (jl_value_t*)jl_tuple_type, tup);
    if (!jl_is_datatype(type) || !type->isconcretetype || type->layout == NULL || jl_is_layout_opaque(type->layout))
        jl_type_error("new", (jl_value_t *)jl_datatype_type, (jl_value_t *)type);
    size_t nargs = jl_nfields(tup);
    size_t nf = jl_datatype_nfields(type);
    JL_NARGS(new, nf, nf);
    if (type->instance != NULL) {
        jl_datatype_t *tupt = (jl_datatype_t*)jl_typeof(tup);
        for (size_t i = 0; i < nargs; i++) {
            jl_value_t *ft = jl_field_type_concrete(type, i);
            jl_value_t *et = jl_field_type_concrete(tupt, i);
            assert(jl_is_concrete_type(ft) && jl_is_concrete_type(et));
            if (et != ft)
                jl_type_error("new", ft, jl_get_nth_field(tup, i));
        }
        return type->instance;
    }
    size_t size = jl_datatype_size(type);
    jl_value_t *jv = jl_gc_alloc(ct->ptls, size, type);
    if (type->smalltag) // TODO: do we need this?
        jl_set_typetagof(jv, type->smalltag, 0);
    if (nf == 0)
        return jv;
    jl_value_t *fi = NULL;
    if (type->zeroinit) {
        // if there are references, zero the space first to prevent the GC
        // from seeing uninitialized references during jl_get_nth_field and jl_isa,
        // which can allocate.
        memset(jl_data_ptr(jv), 0, size);
    }
    else if (jl_field_offset(type, 0) != 0) {
        memset(jl_data_ptr(jv), 0, jl_field_offset(type, 0));
    }
    JL_GC_PUSH2(&jv, &fi);
    for (size_t i = 0; i < nargs; i++) {
        jl_value_t *ft = jl_field_type_concrete(type, i);
        fi = jl_get_nth_field(tup, i);
        if (!jl_isa(fi, ft))
            jl_type_error("new", ft, fi);
        set_nth_field(type, jv, i, fi, 0);
    }
    JL_GC_POP();
    return jv;
}

JL_DLLEXPORT jl_value_t *jl_new_struct_uninit(jl_datatype_t *type)
{
    jl_task_t *ct = jl_current_task;
    if (!jl_is_datatype(type) || !type->isconcretetype || type->layout == NULL || jl_is_layout_opaque(type->layout)) {
        if (type == jl_typeofbottom_type->super)
            return jl_bottom_type; // ::Type{Union{}} is an abstract type, but is also a singleton when used as a field type
        jl_type_error("new", (jl_value_t*)jl_datatype_type, (jl_value_t*)type);
    }
    if (type->instance != NULL)
        return type->instance;
    size_t size = jl_datatype_size(type);
    jl_value_t *jv = jl_gc_alloc(ct->ptls, size, type);
    if (type->smalltag) // TODO: do we need this?
        jl_set_typetagof(jv, type->smalltag, 0);
    if (size > 0)
        memset(jl_data_ptr(jv), 0, size);
    return jv;
}

// field access ---------------------------------------------------------------

// TODO(jwn): these lock/unlock pairs must be full seq-cst fences
JL_DLLEXPORT void jl_lock_value(jl_mutex_t *v) JL_NOTSAFEPOINT
{
    JL_LOCK_NOGC(v);
}

JL_DLLEXPORT void jl_unlock_value(jl_mutex_t *v) JL_NOTSAFEPOINT
{
    JL_UNLOCK_NOGC(v);
}

JL_DLLEXPORT void jl_lock_field(jl_mutex_t *v) JL_NOTSAFEPOINT
{
    JL_LOCK_NOGC(v);
}

JL_DLLEXPORT void jl_unlock_field(jl_mutex_t *v) JL_NOTSAFEPOINT
{
    JL_UNLOCK_NOGC(v);
}

static inline char *lock(char *p, jl_value_t *parent, int needlock, enum atomic_kind isatomic) JL_NOTSAFEPOINT
{
    if (needlock) {
        if (isatomic == isatomic_object) {
            jl_lock_value((jl_mutex_t*)parent);
        }
        else {
            jl_lock_field((jl_mutex_t*)p);
            return p + LLT_ALIGN(sizeof(jl_mutex_t), JL_SMALL_BYTE_ALIGNMENT);
        }
    }
    return p;
}

static inline void unlock(char *p, jl_value_t *parent, int needlock, enum atomic_kind isatomic) JL_NOTSAFEPOINT
{
    if (needlock) {
        if (isatomic == isatomic_object) {
            jl_unlock_value((jl_mutex_t*)parent);
        }
        else {
            jl_unlock_field((jl_mutex_t*)p);
        }
    }
}

JL_DLLEXPORT int jl_field_index(jl_datatype_t *t, jl_sym_t *fld, int err)
{
    if (jl_is_namedtuple_type(t)) {
        jl_value_t *ns = jl_tparam0(t);
        if (jl_is_tuple(ns)) {
            size_t i, n = jl_nfields(ns);
            for (i = 0; i < n; i++) {
                if (jl_get_nth_field(ns, i) == (jl_value_t*)fld) {
                    return (int)i;
                }
            }
        }
    }
    else {
        jl_svec_t *fn = jl_field_names(t);
        size_t i, n = jl_svec_len(fn);
        for (i = 0; i < n; i++) {
            if (jl_svecref(fn, i) == (jl_value_t*)fld) {
                return (int)i;
            }
        }
    }
    if (err)
        jl_has_no_field_error(t, fld);
    return -1;
}

JL_DLLEXPORT jl_value_t *jl_get_nth_field(jl_value_t *v, size_t i)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    if (i >= jl_datatype_nfields(st))
        jl_bounds_error_int(v, i + 1);
    size_t offs = jl_field_offset(st, i);
    if (jl_field_isptr(st, i)) {
        return jl_atomic_load_relaxed((_Atomic(jl_value_t*)*)((char*)v + offs));
    }
    jl_value_t *ty = jl_field_type_concrete(st, i);
    int isatomic = jl_field_isatomic(st, i);
    if (jl_is_uniontype(ty)) {
        assert(!isatomic);
        size_t fsz = jl_field_size(st, i);
        uint8_t sel = ((uint8_t*)v)[offs + fsz - 1];
        ty = jl_nth_union_component(ty, sel);
        if (jl_is_datatype_singleton((jl_datatype_t*)ty))
            return ((jl_datatype_t*)ty)->instance;
    }
    jl_value_t *r;
    size_t fsz = jl_datatype_size(ty);
    int needlock = (isatomic && fsz > MAX_ATOMIC_SIZE);
    if (isatomic && !needlock) {
        r = jl_atomic_new_bits(ty, (char*)v + offs);
    }
    else if (needlock) {
        jl_task_t *ct = jl_current_task;
        r = jl_gc_alloc(ct->ptls, fsz, ty);
        jl_lock_value((jl_mutex_t*)v);
        memcpy((char*)r, (char*)v + offs, fsz);
        jl_unlock_value((jl_mutex_t*)v);
    }
    else {
        // TODO: a finalizer here could make the isunion case not quite right
        r = jl_new_bits(ty, (char*)v + offs);
    }
    return undefref_check((jl_datatype_t*)ty, r);
}

JL_DLLEXPORT jl_value_t *jl_get_nth_field_noalloc(jl_value_t *v JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    assert(i < jl_datatype_nfields(st));
    size_t offs = jl_field_offset(st,i);
    assert(jl_field_isptr(st,i));
    return jl_atomic_load_relaxed((_Atomic(jl_value_t*)*)((char*)v + offs));
}

JL_DLLEXPORT jl_value_t *jl_get_nth_field_checked(jl_value_t *v, size_t i)
{
    jl_value_t *r = jl_get_nth_field(v, i);
    if (__unlikely(r == NULL))
        jl_throw(jl_undefref_exception);
    return r;
}

inline void set_nth_field(jl_datatype_t *st, jl_value_t *v, size_t i, jl_value_t *rhs, int isatomic) JL_NOTSAFEPOINT
{
    size_t offs = jl_field_offset(st, i);
    if (rhs == NULL) { // TODO: this should be invalid, but it happens frequently in ircode.c
        assert(jl_field_isptr(st, i) && *(jl_value_t**)((char*)v + offs) == NULL);
        return;
    }
    if (jl_field_isptr(st, i)) {
        jl_atomic_store_release((_Atomic(jl_value_t*)*)((char*)v + offs), rhs);
        jl_gc_wb(v, rhs);
    }
    else {
        jl_value_t *ty = jl_field_type_concrete(st, i);
        jl_value_t *rty = jl_typeof(rhs);
        int hasptr;
        int isunion = jl_is_uniontype(ty);
        if (isunion) {
            assert(!isatomic);
            size_t fsz = jl_field_size(st, i);
            uint8_t *psel = &((uint8_t*)v)[offs + fsz - 1];
            unsigned nth = 0;
            if (!jl_find_union_component(ty, rty, &nth))
                assert(0 && "invalid field assignment to isbits union");
            *psel = nth;
            if (jl_is_datatype_singleton((jl_datatype_t*)rty))
                return;
            hasptr = 0;
        }
        else {
            hasptr = ((jl_datatype_t*)ty)->layout->first_ptr >= 0;
        }
        size_t fsz = jl_datatype_size((jl_datatype_t*)rty); // need to shrink-wrap the final copy
        assert(!isatomic || jl_typeis(rhs, ty));
        int needlock = (isatomic && fsz > MAX_ATOMIC_SIZE);
        if (isatomic && !needlock) {
            jl_atomic_store_bits((char*)v + offs, rhs, fsz);
        }
        else if (needlock) {
            jl_lock_value((jl_mutex_t*)v);
            memcpy((char*)v + offs, (char*)rhs, fsz);
            jl_unlock_value((jl_mutex_t*)v);
        }
        else {
            memassign_safe(hasptr, (char*)v + offs, rhs, fsz);
        }
        if (hasptr)
            jl_gc_multi_wb(v, rhs); // rhs is immutable
    }
}

inline jl_value_t *swap_bits(jl_value_t *ty, char *v, uint8_t *psel, jl_value_t *parent, jl_value_t *rhs, enum atomic_kind isatomic)
{
    jl_value_t *rty = jl_typeof(rhs);
    int hasptr;
    int isunion = psel != NULL;
    if (isunion) {
        assert(!isatomic);
        hasptr = 0;
    }
    else {
        hasptr = ((jl_datatype_t*)ty)->layout->first_ptr >= 0;
    }
    size_t fsz = jl_datatype_size((jl_datatype_t*)rty); // need to shrink-wrap the final copy
    int needlock = (isatomic && fsz > MAX_ATOMIC_SIZE);
    assert(!isatomic || jl_typeis(rhs, ty));
    jl_value_t *r;
    if (isatomic && !needlock) {
        r = jl_atomic_swap_bits(rty, v, rhs, fsz);
    }
    else {
        if (needlock) {
            jl_task_t *ct = jl_current_task;
            r = jl_gc_alloc(ct->ptls, fsz, ty);
            char *px = lock(v, parent, needlock, isatomic);
            memcpy((char*)r, px, fsz);
            memcpy(px, (char*)rhs, fsz);
            unlock(v, parent, needlock, isatomic);
        }
        else {
            r = jl_new_bits(isunion ? jl_nth_union_component(ty, *psel) : ty, v);
            if (isunion) {
                unsigned nth = 0;
                if (!jl_find_union_component(ty, rty, &nth))
                    assert(0 && "invalid field assignment to isbits union");
                *psel = nth;
                if (jl_is_datatype_singleton((jl_datatype_t*)rty))
                    return r;
            }
            memassign_safe(hasptr, v, rhs, fsz);
        }
    }
    if (!isunion)
        r = undefref_check((jl_datatype_t*)ty, r);
    if (hasptr)
        jl_gc_multi_wb(parent, rhs); // rhs is immutable
    if (__unlikely(r == NULL))
        jl_throw(jl_undefref_exception);
    return r;
}

jl_value_t *swap_nth_field(jl_datatype_t *st, jl_value_t *v, size_t i, jl_value_t *rhs, int isatomic)
{
    jl_value_t *ty = jl_field_type_concrete(st, i);
    if (!jl_isa(rhs, ty))
       jl_type_error("swapfield!", ty, rhs);
    size_t offs = jl_field_offset(st, i);
    jl_value_t *r;
    char *p = (char*)v + offs;
    if (jl_field_isptr(st, i)) {
        if (isatomic)
            r = jl_atomic_exchange((_Atomic(jl_value_t*)*)p, rhs);
        else
            r = jl_atomic_exchange_release((_Atomic(jl_value_t*)*)p, rhs);
        jl_gc_wb(v, rhs);
        if (__unlikely(r == NULL))
            jl_throw(jl_undefref_exception);
        return r;
    }
    else {
        uint8_t *psel = jl_is_uniontype(ty) ? (uint8_t*)&p[jl_field_size(st, i) - 1] : NULL;
        return swap_bits(ty, p, psel, v, rhs, isatomic ? isatomic_object : isatomic_none);
    }
}

inline jl_value_t *modify_value(jl_value_t *ty, _Atomic(jl_value_t*) *p, jl_value_t *parent, jl_value_t *op, jl_value_t *rhs, int isatomic, jl_module_t *mod, jl_sym_t *name)
{
    jl_value_t *r = isatomic ? jl_atomic_load(p) : jl_atomic_load_relaxed(p);
    if (__unlikely(r == NULL)) {
        if (mod && name)
            jl_undefined_var_error(name, (jl_value_t*)mod);
        jl_throw(jl_undefref_exception);
    }
    jl_value_t **args;
    JL_GC_PUSHARGS(args, 2);
    args[0] = r;
    while (1) {
        args[1] = rhs;
        jl_value_t *y = jl_apply_generic(op, args, 2);
        args[1] = y;
        if (!jl_isa(y, ty)) {
            if (mod && name)
                jl_errorf("cannot assign an incompatible value to the global %s.%s.", jl_symbol_name(mod->name), jl_symbol_name(name));
            jl_type_error(jl_is_genericmemory(parent) ? "memoryrefmodify!" : "modifyfield!", ty, y);
        }
        if (isatomic ? jl_atomic_cmpswap(p, &r, y) : jl_atomic_cmpswap_release(p, &r, y)) {
            jl_gc_wb(parent, y);
            break;
        }
        args[0] = r;
        jl_gc_safepoint();
    }
    // args[0] == r (old)
    // args[1] == y (new)
    jl_datatype_t *rettyp = jl_apply_modify_type(ty);
    JL_GC_PROMISE_ROOTED(rettyp); // (JL_ALWAYS_LEAFTYPE)
    args[0] = jl_new_struct(rettyp, args[0], args[1]);
    JL_GC_POP();
    return args[0];
}

inline jl_value_t *modify_bits(jl_value_t *ty, char *p, uint8_t *psel, jl_value_t *parent, jl_value_t *op, jl_value_t *rhs, enum atomic_kind isatomic)
{
    int hasptr;
    int isunion = psel != NULL;
    if (isunion) {
        assert(!isatomic);
        hasptr = 0;
    }
    else {
        hasptr = ((jl_datatype_t*)ty)->layout->first_ptr >= 0;
    }
    jl_value_t **args;
    JL_GC_PUSHARGS(args, 2);
    while (1) {
        jl_value_t *r;
        jl_value_t *rty = isunion ? jl_nth_union_component(ty, *psel) : ty;
        size_t fsz = jl_datatype_size((jl_datatype_t*)rty); // need to shrink-wrap the initial copy
        int needlock = (isatomic && fsz > MAX_ATOMIC_SIZE);
        if (isatomic && !needlock) {
            r = jl_atomic_new_bits(rty, p);
        }
        else if (needlock) {
            jl_task_t *ct = jl_current_task;
            r = jl_gc_alloc(ct->ptls, fsz, rty);
            char *px = lock(p, parent, needlock, isatomic);
            memcpy((char*)r, px, fsz);
            unlock(p, parent, needlock, isatomic);
        }
        else {
            r = jl_new_bits(rty, p);
        }
        r = undefref_check((jl_datatype_t*)rty, r);
        if (__unlikely(r == NULL))
            jl_throw(jl_undefref_exception);
        args[0] = r;
        args[1] = rhs;
        jl_value_t *y = jl_apply_generic(op, args, 2);
        args[1] = y;
        if (!jl_isa(y, ty)) {
            jl_type_error(jl_is_genericmemory(parent) ? "memoryrefmodify!" : "modifyfield!", ty, y);
        }
        jl_value_t *yty = jl_typeof(y);
        if (isatomic && !needlock) {
            assert(yty == rty);
            if (jl_atomic_bool_cmpswap_bits(p, r, y, fsz)) {
                if (hasptr)
                    jl_gc_multi_wb(parent, y); // y is immutable
                break;
            }
        }
        else {
            char *px = lock(p, parent, needlock, isatomic);
            int success = memcmp(px, (char*)r, fsz) == 0;
            if (!success && (!((jl_datatype_t*)rty)->layout->flags.isbitsegal || ((jl_datatype_t*)rty)->layout->flags.haspadding))
                success = jl_egal__bits((jl_value_t*)px, r, (jl_datatype_t*)rty);
            if (success) {
                if (isunion) {
                    success = (rty == jl_nth_union_component(ty, *psel));
                    if (success) {
                        unsigned nth = 0;
                        if (!jl_find_union_component(ty, yty, &nth))
                            assert(0 && "invalid field assignment to isbits union");
                        *psel = nth;
                        if (jl_is_datatype_singleton((jl_datatype_t*)yty))
                            break;
                    }
                    fsz = jl_datatype_size((jl_datatype_t*)yty); // need to shrink-wrap the final copy
                }
                else {
                    assert(yty == ty && rty == ty);
                }
                memassign_safe(hasptr, px, y, fsz);
            }
            unlock(p, parent, needlock, isatomic);
            if (success) {
                if (hasptr)
                    jl_gc_multi_wb(parent, y); // y is immutable
                break;
            }
        }
        jl_gc_safepoint();
    }
    // args[0] == r (old)
    // args[1] == y (new)
    jl_datatype_t *rettyp = jl_apply_modify_type(ty);
    JL_GC_PROMISE_ROOTED(rettyp); // (JL_ALWAYS_LEAFTYPE)
    args[0] = jl_new_struct(rettyp, args[0], args[1]);
    JL_GC_POP();
    return args[0];
}

jl_value_t *modify_nth_field(jl_datatype_t *st, jl_value_t *v, size_t i, jl_value_t *op, jl_value_t *rhs, int isatomic)
{
    size_t offs = jl_field_offset(st, i);
    jl_value_t *ty = jl_field_type_concrete(st, i);
    char *p = (char*)v + offs;
    if (jl_field_isptr(st, i)) {
        return modify_value(ty, (_Atomic(jl_value_t*)*)p, v, op, rhs, isatomic, NULL, NULL);
    }
    else {
        uint8_t *psel = jl_is_uniontype(ty) ? (uint8_t*)&p[jl_field_size(st, i) - 1] : NULL;
        return modify_bits(ty, p, psel, v, op, rhs, isatomic ? isatomic_object : isatomic_none);
    }
}

inline jl_value_t *replace_value(jl_value_t *ty, _Atomic(jl_value_t*) *p, jl_value_t *parent, jl_value_t *expected, jl_value_t *rhs, int isatomic, jl_module_t *mod, jl_sym_t *name)
{
    jl_datatype_t *rettyp = jl_apply_cmpswap_type(ty);
    JL_GC_PROMISE_ROOTED(rettyp); // (JL_ALWAYS_LEAFTYPE)
    jl_value_t *r = expected;
    int success;
    while (1) {
        success = isatomic ? jl_atomic_cmpswap(p, &r, rhs) : jl_atomic_cmpswap_release(p, &r, rhs);
        if (success)
            jl_gc_wb(parent, rhs);
        if (__unlikely(r == NULL)) {
            if (mod && name)
                jl_undefined_var_error(name, (jl_value_t*)mod);
            jl_throw(jl_undefref_exception);
        }
        if (success || !jl_egal(r, expected))
            break;
    }
    JL_GC_PUSH1(&r);
    r = jl_new_struct(rettyp, r, success ? jl_true : jl_false);
    JL_GC_POP();
    return r;
}

inline jl_value_t *replace_bits(jl_value_t *ty, char *p, uint8_t *psel, jl_value_t *parent, jl_value_t *expected, jl_value_t *rhs, enum atomic_kind isatomic)
{
    jl_datatype_t *rettyp = jl_apply_cmpswap_type(ty);
    JL_GC_PROMISE_ROOTED(rettyp); // (JL_ALWAYS_LEAFTYPE)
    int hasptr;
    int isunion = psel != NULL;
    size_t fsz = jl_field_size(rettyp, 0);
    int needlock = (isatomic && fsz > MAX_ATOMIC_SIZE);
    assert(jl_field_offset(rettyp, 1) == fsz);
    jl_value_t *rty = ty;
    if (isunion) {
        assert(!isatomic);
        hasptr = 0;
        isatomic = isatomic_none; // this makes GCC happy
    }
    else {
        hasptr = ((jl_datatype_t*)ty)->layout->first_ptr >= 0;
        assert(jl_typeis(rhs, ty));
    }
    int success;
    jl_task_t *ct = jl_current_task;
    assert(!jl_field_isptr(rettyp, 0));
    jl_value_t *r = jl_gc_alloc(ct->ptls, jl_datatype_size(rettyp), rettyp);
    if (isatomic && !needlock) {
        size_t rsz = jl_datatype_size((jl_datatype_t*)rty); // need to shrink-wrap the compare
        success = jl_atomic_cmpswap_bits((jl_datatype_t*)rty, r, p, expected, rhs, rsz);
        *((uint8_t*)r + fsz) = success ? 1 : 0;
    }
    else {
        char *px = lock(p, parent, needlock, isatomic);
        if (isunion)
            rty = jl_nth_union_component(rty, *psel);
        size_t rsz = jl_datatype_size((jl_datatype_t*)rty); // need to shrink-wrap the compare
        memcpy((char*)r, px, rsz); // copy field // TODO: make this a memmove_refs if relevant
        if (isunion)
            *((uint8_t*)r + fsz - 1) = *psel; // copy union bits
        success = (rty == jl_typeof(expected));
        if (success) {
            success = memcmp((char*)r, (char*)expected, rsz) == 0;
            if (!success && (!((jl_datatype_t*)rty)->layout->flags.isbitsegal || ((jl_datatype_t*)rty)->layout->flags.haspadding))
                success = jl_egal__bits(r, expected, (jl_datatype_t*)rty);
        }
        *((uint8_t*)r + fsz) = success ? 1 : 0;
        if (success) {
            jl_value_t *rty = jl_typeof(rhs);
            if (isunion) {
                rsz = jl_datatype_size((jl_datatype_t*)rty); // need to shrink-wrap the final copy
                unsigned nth = 0;
                if (!jl_find_union_component(ty, rty, &nth))
                    assert(0 && "invalid field assignment to isbits union");
                *psel = nth;
                if (jl_is_datatype_singleton((jl_datatype_t*)rty))
                    return r;
            }
            memassign_safe(hasptr, px, rhs, rsz);
        }
        unlock(p, parent, needlock, isatomic);
    }
    if (success && hasptr)
        jl_gc_multi_wb(parent, rhs); // rhs is immutable
    if (!isunion) {
        r = undefref_check((jl_datatype_t*)rty, r);
        if (__unlikely(r == NULL))
            jl_throw(jl_undefref_exception);
    }
    return r;
}

jl_value_t *replace_nth_field(jl_datatype_t *st, jl_value_t *v, size_t i, jl_value_t *expected, jl_value_t *rhs, int isatomic)
{
    jl_value_t *ty = jl_field_type_concrete(st, i);
    if (!jl_isa(rhs, ty))
        jl_type_error("replacefield!", ty, rhs);
    size_t offs = jl_field_offset(st, i);
    char *p = (char*)v + offs;
    if (jl_field_isptr(st, i)) {
        return replace_value(ty, (_Atomic(jl_value_t*)*)p, v, expected, rhs, isatomic, NULL, NULL);
    }
    else {
        size_t fsz = jl_field_size(st, i);
        int isunion = jl_is_uniontype(ty);
        uint8_t *psel = isunion ? (uint8_t*)&p[fsz - 1] : NULL;
        return replace_bits(ty, p, psel, v, expected, rhs, isatomic ? isatomic_object : isatomic_none);
    }
}

inline int setonce_bits(jl_datatype_t *rty, char *p, jl_value_t *parent, jl_value_t *rhs, enum atomic_kind isatomic)
{
    size_t fsz = jl_datatype_size((jl_datatype_t*)rty); // need to shrink-wrap the final copy
    assert(rty->layout->first_ptr >= 0);
    int hasptr = 1;
    int needlock = (isatomic && fsz > MAX_ATOMIC_SIZE);
    int success;
    if (isatomic && !needlock) {
        success = jl_atomic_storeonce_bits(rty, p, rhs, fsz);
    }
    else {
        char *px = lock(p, parent, needlock, isatomic);
        success = undefref_check(rty, (jl_value_t*)px) == NULL;
        if (success)
            memassign_safe(hasptr, px, rhs, fsz);
        unlock(p, parent, needlock, isatomic);
    }
    if (success)
        jl_gc_multi_wb(parent, rhs); // rhs is immutable
    return success;
}

int set_nth_fieldonce(jl_datatype_t *st, jl_value_t *v, size_t i, jl_value_t *rhs, int isatomic)
{
    jl_value_t *ty = jl_field_type_concrete(st, i);
    if (!jl_isa(rhs, ty))
        jl_type_error("setfieldonce!", ty, rhs);
    size_t offs = jl_field_offset(st, i);
    int success;
    char *p = (char*)v + offs;
    if (jl_field_isptr(st, i)) {
        _Atomic(jl_value_t*) *px = (_Atomic(jl_value_t*)*)p;
        jl_value_t *r = NULL;
        success = isatomic ? jl_atomic_cmpswap(px, &r, rhs) : jl_atomic_cmpswap_release(px, &r, rhs);
        if (success)
            jl_gc_wb(v, rhs);
    }
    else {
        int isunion = jl_is_uniontype(ty);
        if (isunion)
            return 0;
        int hasptr = ((jl_datatype_t*)ty)->layout->first_ptr >= 0;
        if (!hasptr)
            return 0;
        assert(ty == jl_typeof(rhs));
        success = setonce_bits((jl_datatype_t*)ty, p, v, rhs, isatomic ? isatomic_object : isatomic_none);
    }
    return success;
}

JL_DLLEXPORT int jl_field_isdefined(jl_value_t *v, size_t i) JL_NOTSAFEPOINT
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t offs = jl_field_offset(st, i);
    _Atomic(jl_value_t*) *fld = (_Atomic(jl_value_t*)*)((char*)v + offs);
    if (!jl_field_isptr(st, i)) {
        jl_datatype_t *ft = (jl_datatype_t*)jl_field_type_concrete(st, i);
        if (!jl_is_datatype(ft) || ft->layout->first_ptr < 0)
            return 2; // isbits are always defined
        fld += ft->layout->first_ptr;
    }
    jl_value_t *fval = jl_atomic_load_relaxed(fld);
    return fval != NULL ? 1 : 0;
}

JL_DLLEXPORT int jl_field_isdefined_checked(jl_value_t *v, size_t i)
{
    if (jl_is_module(v)) {
        jl_type_error("isdefined", (jl_value_t*)jl_symbol_type, jl_box_long(i + 1));
    }
    if (i >= jl_nfields(v))
        return 0;
    return !!jl_field_isdefined(v, i);
}

JL_DLLEXPORT size_t jl_get_field_offset(jl_datatype_t *ty, int field)
{
    if (!jl_struct_try_layout(ty) || field > jl_datatype_nfields(ty) || field < 1)
        jl_bounds_error_int((jl_value_t*)ty, field);
    return jl_field_offset(ty, field - 1);
}

jl_value_t *get_nth_pointer(jl_value_t *v, size_t i)
{
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(v);
    const jl_datatype_layout_t *ly = dt->layout;
    uint32_t npointers = ly->npointers;
    if (i >= npointers)
        jl_bounds_error_int(v, i);
    const uint8_t *ptrs8 = (const uint8_t *)jl_dt_layout_ptrs(ly);
    const uint16_t *ptrs16 = (const uint16_t *)jl_dt_layout_ptrs(ly);
    const uint32_t *ptrs32 = (const uint32_t*)jl_dt_layout_ptrs(ly);
    uint32_t fld;
    if (ly->flags.fielddesc_type == 0)
        fld = ptrs8[i];
    else if (ly->flags.fielddesc_type == 1)
        fld = ptrs16[i];
    else
        fld = ptrs32[i];
    return jl_atomic_load_relaxed((_Atomic(jl_value_t*)*)(&((jl_value_t**)v)[fld]));
}

JL_DLLEXPORT jl_value_t *jl_get_nth_pointer(jl_value_t *v, size_t i)
{
    jl_value_t *ptrf = get_nth_pointer(v, i);
    if (__unlikely(ptrf == NULL))
        jl_throw(jl_undefref_exception);
    return ptrf;
}

JL_DLLEXPORT int jl_nth_pointer_isdefined(jl_value_t *v, size_t i)
{
    return get_nth_pointer(v, i) != NULL;
}

#ifdef __cplusplus
}
#endif
