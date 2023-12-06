// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  GenericMemory{kind, T} constructors and primitives
*/
#include <stdlib.h>
#include <string.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

static inline void genericmemoryassign_safe(int hasptr, jl_value_t *parent, char *dst, const jl_value_t *src) JL_NOTSAFEPOINT
{
    size_t nb = jl_datatype_size(jl_typeof(src)); // make sure to shrink-wrap this copy
    if (hasptr) {
        size_t nptr = nb / sizeof(void*);
        memmove_refs((_Atomic(void*)*)dst, (_Atomic(void*)*)src, nptr);
        jl_gc_multi_wb(parent, src);
    }
    else {
        // genericmemory can assume more alignment than a field would normally have
        switch (nb) {
        case  0: break;
        case  1: *(uint8_t*)dst  = *(uint8_t*)src;  break;
        case  2: *(uint16_t*)dst = *(uint16_t*)src; break;
        case  4: *(uint32_t*)dst = *(uint32_t*)src; break;
        case  8: *(uint64_t*)dst = *(uint64_t*)src; break;
        case 16:
            memcpy(jl_assume_aligned(dst, 16), jl_assume_aligned(src, 16), 16);
            break;
        default: memcpy(dst, src, nb);
        }
    }
}

// genericmemory constructors ---------------------------------------------------------
JL_DLLEXPORT char *jl_genericmemory_typetagdata(jl_genericmemory_t *m) JL_NOTSAFEPOINT
{
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m))->layout;
    assert(layout->flags.arrayelem_isunion);
    return (char*)m->ptr + m->length * layout->size;
}

#if defined(_P64) && defined(UINT128MAX)
typedef __uint128_t wideint_t;
#else
typedef uint64_t wideint_t;
#endif

#define MAXINTVAL (((size_t)-1)>>1)

jl_genericmemory_t *_new_genericmemory_(jl_value_t *mtype, size_t nel, int8_t isunion, int8_t zeroinit, size_t elsz)
{
    jl_task_t *ct = jl_current_task;
    char *data;
    jl_genericmemory_t *m;
    if (nel == 0) // zero-sized allocation optimization
        return (jl_genericmemory_t*)((jl_datatype_t*)mtype)->instance;
    wideint_t prod = (wideint_t)nel * elsz;
    if (isunion) {
        // an extra byte for each isbits union memory element, stored at m->ptr + m->length
        prod += nel;
    }
    if (nel >= MAXINTVAL || prod >= (wideint_t) MAXINTVAL)
        jl_exceptionf(jl_argumenterror_type, "invalid GenericMemory size");
    size_t tot = (size_t)prod + LLT_ALIGN(sizeof(jl_genericmemory_t),JL_SMALL_BYTE_ALIGNMENT);

    int pooled = tot <= GC_MAX_SZCLASS;
    if (!pooled) {
        data = (char*)jl_gc_managed_malloc(prod);
        tot = sizeof(jl_genericmemory_t) + sizeof(void*);
    }
    m = (jl_genericmemory_t*)jl_gc_alloc(ct->ptls, tot, mtype);
    if (pooled) {
        data = (char*)m + JL_SMALL_BYTE_ALIGNMENT;
    }
    else {
        int isaligned = 1; // jl_gc_managed_malloc is always aligned
        jl_gc_track_malloced_genericmemory(ct->ptls, m, isaligned);
        jl_genericmemory_data_owner_field(m) = (jl_value_t*)m;
    }
    m->length = nel;
    m->ptr = data;

    if (zeroinit)
        memset(data, 0, (size_t)prod);
    return m;
}

JL_DLLEXPORT jl_genericmemory_t *jl_alloc_genericmemory(jl_value_t *mtype, size_t nel)
{
    assert(jl_is_datatype(mtype));
    jl_genericmemory_t *m = (jl_genericmemory_t*)((jl_datatype_t*)mtype)->instance;
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)mtype)->layout;
    if (m == NULL) {
        if (jl_tparam0((jl_datatype_t*)mtype) != (jl_value_t*)jl_not_atomic_sym)
            jl_error("GenericMemory kind must be :not_atomic");
        jl_value_t *addrspace = jl_tparam2((jl_datatype_t*)mtype);
        if (!jl_is_addrspacecore(addrspace) || jl_unbox_uint8(addrspace) != 0)
            jl_error("GenericMemory addrspace must be Core.CPU");
        if (!((jl_datatype_t*)mtype)->has_concrete_subtype || layout == NULL)
            jl_type_error_rt("GenericMemory", "element type", (jl_value_t*)jl_type_type, jl_tparam1(mtype));
        abort(); // this is checked already by jl_get_genericmemory_layout
    }
    assert(jl_tparam0((jl_datatype_t*)mtype) == (jl_value_t*)jl_not_atomic_sym);
    assert(((jl_datatype_t*)mtype)->has_concrete_subtype && layout != NULL);
    if (nel == 0) // zero-sized allocation optimization fast path
        return m;

    size_t elsz = layout->size;
    int isboxed = layout->flags.arrayelem_isboxed;
    int isunion = layout->flags.arrayelem_isunion;
    int zi = ((jl_datatype_t*)mtype)->zeroinit;
    if (isboxed)
        elsz = sizeof(void*);
    return _new_genericmemory_(mtype, nel, isunion, zi, elsz);
}

JL_DLLEXPORT jl_genericmemory_t *jl_string_to_genericmemory(jl_value_t *str)
{
    jl_task_t *ct = jl_current_task;
    int tsz = sizeof(jl_genericmemory_t) + sizeof(void*);
    jl_genericmemory_t *m = (jl_genericmemory_t*)jl_gc_alloc(ct->ptls, tsz, jl_memory_uint8_type);
    m->length = jl_string_len(str);
    m->ptr = jl_string_data(str);
    jl_genericmemory_data_owner_field(m) = str;
    return m;
}

// own_buffer != 0 iff GC should call free() on this pointer eventually
JL_DLLEXPORT jl_genericmemory_t *jl_ptr_to_genericmemory(jl_value_t *mtype, void *data,
                                                         size_t nel, int own_buffer)
{
    jl_task_t *ct = jl_current_task;
    assert(jl_is_datatype(mtype));
    jl_genericmemory_t *m = (jl_genericmemory_t*)((jl_datatype_t*)mtype)->instance;
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)mtype)->layout;
    if (m == NULL) {
        if (jl_tparam0((jl_datatype_t*)mtype) != (jl_value_t*)jl_not_atomic_sym)
            jl_error("GenericMemory kind must be :not_atomic");
        jl_value_t *addrspace = jl_tparam2((jl_datatype_t*)mtype);
        if (!jl_is_addrspacecore(addrspace) || jl_unbox_uint8(addrspace) != 0)
            jl_error("GenericMemory addrspace must be Core.CPU");
        if (!((jl_datatype_t*)mtype)->has_concrete_subtype || layout == NULL)
            jl_type_error_rt("GenericMemory", "element type", (jl_value_t*)jl_type_type, jl_tparam1(mtype));
        abort();
    }
    assert(jl_tparam0((jl_datatype_t*)mtype) == (jl_value_t*)jl_not_atomic_sym);
    assert(((jl_datatype_t*)mtype)->has_concrete_subtype && layout != NULL);
    //if (nel == 0) {// zero-sized allocation optimization fast path
    //    if (own_buffer)
    //        free(data);
    //    return m;
    //}

    size_t elsz = layout->size;
    size_t align = layout->alignment;
    int isboxed = layout->flags.arrayelem_isboxed;
    int isunion = layout->flags.arrayelem_isunion;
    if (isboxed)
        elsz = sizeof(void*);
    if (isunion)
        jl_exceptionf(jl_argumenterror_type,
                      "unsafe_wrap: unspecified layout for union element type");
    if (((uintptr_t)data) & ((align > JL_HEAP_ALIGNMENT ? JL_HEAP_ALIGNMENT : align) - 1))
        jl_exceptionf(jl_argumenterror_type,
                      "unsafe_wrap: pointer %p is not properly aligned to %u bytes", data, align);
    wideint_t prod = (wideint_t)nel * elsz;
    if (isunion) {
        // an extra byte for each isbits union memory element, stored at m->ptr + m->length
        prod += nel;
    }
    if (nel >= MAXINTVAL || prod >= (wideint_t) MAXINTVAL)
        jl_exceptionf(jl_argumenterror_type, "invalid GenericMemory size");
    int tsz = sizeof(jl_genericmemory_t) + sizeof(void*);
    m = (jl_genericmemory_t*)jl_gc_alloc(ct->ptls, tsz, mtype);
    m->ptr = data;
    m->length = nel;
    jl_genericmemory_data_owner_field(m) = NULL;
    int isaligned = 0;  // TODO: allow passing memalign'd buffers
    if (own_buffer) {
        jl_gc_track_malloced_genericmemory(ct->ptls, m, isaligned);
        jl_gc_count_allocd(nel*elsz);
    }
    return m;
}

JL_DLLEXPORT jl_genericmemory_t *jl_new_genericmemory(jl_value_t *mtype, jl_value_t *nel)
{
    return jl_alloc_genericmemory(mtype, jl_unbox_long(nel));
}

JL_DLLEXPORT jl_genericmemory_t *jl_pchar_to_genericmemory(const char *str, size_t len)
{
    jl_genericmemory_t *m = jl_alloc_genericmemory(jl_memory_uint8_type, len);
    memcpy(m->ptr, str, len);
    return m;
}

JL_DLLEXPORT jl_value_t *jl_genericmemory_to_string(jl_genericmemory_t *m, size_t len)
{
    assert(len <= m->length);
    if (len == 0) {
        // this may seem like purely an optimization (which it also is), but it
        // also ensures that calling `String(m)` doesn't corrupt a previous
        // string also created the same way, where `m = StringVector(_)`.
        return jl_an_empty_string;
    }
    int how = jl_genericmemory_how(m);
    size_t mlength = m->length;
    m->length = 0;
    if (how != 0) {
        jl_value_t *o = jl_genericmemory_data_owner_field(m);
        jl_genericmemory_data_owner_field(m) = NULL;
        if (how == 3 &&
             ((mlength + sizeof(void*) + 1 <= GC_MAX_SZCLASS) == (len + sizeof(void*) + 1 <= GC_MAX_SZCLASS))) {
            if (jl_string_data(o)[len] != '\0')
                jl_string_data(o)[len] = '\0';
            if (*(size_t*)o != len)
                *(size_t*)o = len;
            return o;
        }
        JL_GC_PUSH1(&o);
        jl_value_t *str = jl_pchar_to_string((const char*)m->ptr, len);
        JL_GC_POP();
        return str;
    }
    return jl_pchar_to_string((const char*)m->ptr, len);
}

JL_DLLEXPORT jl_genericmemory_t *jl_alloc_memory_any(size_t n)
{
    return jl_alloc_genericmemory(jl_memory_any_type, n);
}

JL_DLLEXPORT jl_genericmemory_t *jl_genericmemory_slice(jl_genericmemory_t *mem, void *data, size_t len)
{
    // Given a GenericMemoryRef represented as `jl_genericmemory_ref ref = {data, mem}`,
    // return a new GenericMemory that only accesses the slice from the given GenericMemoryRef to
    // the given length if this is possible to return. This allows us to make
    // `length(Array)==length(Array.ref.mem)`, for simplification of this.
    jl_datatype_t *dt = (jl_datatype_t*)jl_typetagof(mem);
    const jl_datatype_layout_t *layout = dt->layout;
    // repeated checks here ensure the values cannot overflow, since we know mem->length is a reasonable value
    if (len > mem->length)
        jl_exceptionf(jl_argumenterror_type, "invalid GenericMemory slice"); // TODO: make a BoundsError
    if (layout->flags.arrayelem_isunion) {
        if (!((size_t)data == 0 && mem->length == len))
            jl_exceptionf(jl_argumenterror_type, "invalid GenericMemory slice"); // only exact slices are supported
        data = mem->ptr;
    }
    else if (layout->size == 0) {
        if ((size_t)data > mem->length || (size_t)data + len > mem->length)
            jl_exceptionf(jl_argumenterror_type, "invalid GenericMemory slice"); // TODO: make a BoundsError
        data = mem->ptr;
    }
    else {
        if (data < mem->ptr || (char*)data > (char*)mem->ptr + mem->length * layout->size || (char*)data + len * layout->size > (char*)mem->ptr + mem->length * layout->size)
            jl_exceptionf(jl_argumenterror_type, "invalid GenericMemory slice"); // TODO: make a BoundsError
    }
    jl_task_t *ct = jl_current_task;
    jl_genericmemory_t *newmem = (jl_genericmemory_t*)jl_gc_alloc(ct->ptls, sizeof(jl_genericmemory_t) + sizeof(void*), dt);
    newmem->length = len;
    newmem->ptr = data;
    jl_genericmemory_data_owner_field(newmem) = jl_genericmemory_owner(mem);
    return newmem;
}

JL_DLLEXPORT void jl_genericmemory_copyto(jl_genericmemory_t *dest, char* destdata,
                                          jl_genericmemory_t *src, char* srcdata,
                                          size_t n) JL_NOTSAFEPOINT
{
    jl_datatype_t *dt = (jl_datatype_t*)jl_typetagof(dest);
    if (dt != (jl_datatype_t*)jl_typetagof(src))
        jl_exceptionf(jl_argumenterror_type, "jl_genericmemory_copyto requires source and dest to have same type");
    const jl_datatype_layout_t *layout = dt->layout;
    if (layout->flags.arrayelem_isboxed) {
        _Atomic(void*) * dest_p = (_Atomic(void*)*)destdata;
        _Atomic(void*) * src_p = (_Atomic(void*)*)srcdata;
        jl_value_t *owner = jl_genericmemory_owner(dest);
        if (__unlikely(jl_astaggedvalue(owner)->bits.gc == GC_OLD_MARKED)) {
            jl_value_t *src_owner = jl_genericmemory_owner(src);
            ssize_t done = 0;
            if (jl_astaggedvalue(src_owner)->bits.gc != GC_OLD_MARKED) {
                if (dest_p < src_p || dest_p > src_p + n) {
                    for (; done < n; done++) { // copy forwards
                        void *val = jl_atomic_load_relaxed(src_p + done);
                        jl_atomic_store_release(dest_p + done, val);
                        // `val` is young or old-unmarked
                        if (val && !(jl_astaggedvalue(val)->bits.gc & GC_MARKED)) {
                            jl_gc_queue_root(owner);
                            break;
                        }
                    }
                    src_p += done;
                    dest_p += done;
                } else {
                    for (; done < n; done++) { // copy backwards
                        void *val = jl_atomic_load_relaxed(src_p + n - done - 1);
                        jl_atomic_store_release(dest_p + n - done - 1, val);
                        // `val` is young or old-unmarked
                        if (val && !(jl_astaggedvalue(val)->bits.gc & GC_MARKED)) {
                            jl_gc_queue_root(owner);
                            break;
                        }
                    }
                }
                n -= done;
            }
        }
        return memmove_refs(dest_p, src_p, n);
    }
    size_t elsz = layout->size;
    char *src_p = srcdata;
    int isbitsunion = layout->flags.arrayelem_isunion;
    if (isbitsunion) {
        char *sourcetypetagdata = jl_genericmemory_typetagdata(src);
        char *desttypetagdata = jl_genericmemory_typetagdata(dest);
        memmove(desttypetagdata+(size_t)destdata, sourcetypetagdata+(size_t)srcdata, n);
        srcdata = (char*)src->ptr + elsz*(size_t)srcdata;
        destdata = (char*)dest->ptr + elsz*(size_t)destdata;
    }
    if (layout->first_ptr != -1) {
        memmove_refs((_Atomic(void*)*)destdata, (_Atomic(void*)*)srcdata, n * elsz / sizeof(void*));
        jl_value_t *owner = jl_genericmemory_owner(dest);
        if (__unlikely(jl_astaggedvalue(owner)->bits.gc == GC_OLD_MARKED)) {
            jl_value_t *src_owner = jl_genericmemory_owner(src);
            if (jl_astaggedvalue(src_owner)->bits.gc != GC_OLD_MARKED) {
                dt = (jl_datatype_t*)jl_tparam1(dt);
                for (size_t done = 0; done < n; done++) { // copy forwards
                    char* s = (char*)src_p+done*elsz;
                    if (*((jl_value_t**)s+layout->first_ptr) != NULL)
                        jl_gc_queue_multiroot(owner, s, dt);
                }
            }
        }
    }
    else {
        memmove(destdata, srcdata, n * elsz);
    }
}


// genericmemory primitives -----------------------------------------------------------

JL_DLLEXPORT jl_value_t *jl_ptrmemref(jl_genericmemory_t *m JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT
{
    assert(i < m->length);
    assert(((jl_datatype_t*)jl_typetagof(m))->layout->flags.arrayelem_isboxed);
    jl_value_t *elt = jl_atomic_load_relaxed(((_Atomic(jl_value_t*)*)m->ptr) + i);
    if (elt == NULL)
        jl_throw(jl_undefref_exception);
    return elt;
}

JL_DLLEXPORT jl_value_t *jl_genericmemoryref(jl_genericmemory_t *m, size_t i)
{
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m))->layout;
    if (layout->flags.arrayelem_isboxed)
        return jl_ptrmemref(m, i);
    assert(i < m->length);
    jl_value_t *isatomic = jl_tparam0(jl_typetagof(m)); (void)isatomic; // TODO
    jl_value_t *eltype = jl_tparam1(jl_typetagof(m));
    if (layout->flags.arrayelem_isunion) {
        // isbits union selector bytes are always stored directly after the last memory element
        uint8_t sel = jl_genericmemory_typetagdata(m)[i];
        eltype = jl_nth_union_component(eltype, sel);
        if (jl_is_datatype_singleton((jl_datatype_t*)eltype))
            return ((jl_datatype_t*)eltype)->instance;
    }
    jl_value_t *r = undefref_check((jl_datatype_t*)eltype, jl_new_bits(eltype, &((char*)m->ptr)[i * layout->size]));
    if (__unlikely(r == NULL))
        jl_throw(jl_undefref_exception);
    return r;
}

JL_DLLEXPORT int jl_genericmemory_isassigned(jl_genericmemory_t *m, size_t i)
{
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m))->layout;
    if (layout->flags.arrayelem_isboxed) {
        return jl_atomic_load_relaxed(((_Atomic(jl_value_t*)*)m->ptr) + i) != NULL;
    }
    else if (layout->first_ptr >= 0) {
         jl_value_t **elem = (jl_value_t**)((char*)m->ptr + i * layout->size);
         return elem[layout->first_ptr] != NULL;
    }
    return 1;
}

JL_DLLEXPORT void jl_genericmemoryset(jl_genericmemory_t *m JL_ROOTING_ARGUMENT, jl_value_t *rhs JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED, size_t i)
{
    assert(i < m->length);
    jl_value_t *isatomic = jl_tparam0(jl_typetagof(m)); (void)isatomic; // TODO
    jl_value_t *eltype = jl_tparam1(jl_typetagof(m));
    if (eltype != (jl_value_t*)jl_any_type && !jl_typeis(rhs, eltype)) {
        JL_GC_PUSH1(&rhs);
        if (!jl_isa(rhs, eltype))
            jl_type_error("genericmemoryset", eltype, rhs);
        JL_GC_POP();
    }
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m))->layout;
    if (layout->flags.arrayelem_isboxed) {
        jl_atomic_store_release(((_Atomic(jl_value_t*)*)m->ptr) + i, rhs);
        jl_gc_wb(jl_genericmemory_owner(m), rhs);
    }
    else {
        int hasptr;
        if (jl_is_uniontype(eltype)) {
            uint8_t *psel = &((uint8_t*)jl_genericmemory_typetagdata(m))[i];
            unsigned nth = 0;
            if (!jl_find_union_component(eltype, jl_typeof(rhs), &nth))
                assert(0 && "invalid genericmemoryset to isbits union");
            *psel = nth;
            if (jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(rhs)))
                return;
            hasptr = 0;
        }
        else {
            hasptr = layout->first_ptr >= 0;
        }
        genericmemoryassign_safe(hasptr, jl_genericmemory_owner(m), &((char*)m->ptr)[i * layout->size], rhs);
    }
}

JL_DLLEXPORT void jl_genericmemoryunset(jl_genericmemory_t *m, size_t i)
{
    if (i >= m->length)
        jl_bounds_error_int((jl_value_t*)m, i + 1);
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m))->layout;
    if (layout->flags.arrayelem_isboxed)
        jl_atomic_store_relaxed(((_Atomic(jl_value_t*)*)m->ptr) + i, NULL);
    else if (layout->first_ptr >= 0) {
        size_t elsize = layout->size;
        jl_assume(elsize >= sizeof(void*) && elsize % sizeof(void*) == 0);
        memset((char*)m->ptr + elsize * i, 0, elsize);
    }
}

JL_DLLEXPORT jl_genericmemory_t *jl_genericmemory_copy_slice(jl_genericmemory_t *mem, void *data, size_t len)
{
    jl_value_t *mtype = (jl_value_t*)jl_typetagof(mem);
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)mtype)->layout;
    size_t elsz = layout->size;
    int isunion = layout->flags.arrayelem_isunion;
    jl_genericmemory_t *new_mem = _new_genericmemory_(mtype, len, isunion, 0, elsz);
    if (isunion) {
        memcpy(new_mem->ptr, (char*)mem->ptr + (size_t)data * elsz, len * elsz);
        memcpy(jl_genericmemory_typetagdata(new_mem), jl_genericmemory_typetagdata(mem) + (size_t)data, len);
    }
    else if (layout->first_ptr != -1) {
        memmove_refs((_Atomic(void*)*)new_mem->ptr, (_Atomic(void*)*)data, len * elsz / sizeof(void*));
    }
    else if (data != NULL) {
        memcpy(new_mem->ptr, data, len * elsz);
    }
    return new_mem;
}

JL_DLLEXPORT jl_genericmemory_t *jl_genericmemory_copy(jl_genericmemory_t *mem)
{
    jl_value_t *mtype = (jl_value_t*)jl_typetagof(mem);
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)mtype)->layout;
    return jl_genericmemory_copy_slice(mem, layout->flags.arrayelem_isunion || layout->size == 0 ? (void*)0 : mem->ptr, mem->length);
}

JL_DLLEXPORT jl_value_t *(jl_genericmemory_data_owner)(jl_genericmemory_t *m) JL_NOTSAFEPOINT
{
    return jl_genericmemory_data_owner_field(m);
}

jl_genericmemoryref_t *jl_new_memoryref(jl_value_t *typ, jl_genericmemory_t *mem, void *data)
{
    jl_task_t *ct = jl_current_task;
    jl_genericmemoryref_t *m = (jl_genericmemoryref_t*)jl_gc_alloc(ct->ptls, sizeof(jl_genericmemoryref_t), typ);
    m->mem = mem;
    m->ptr_or_offset = data;
    return m;
}

// memoryref primitives
JL_DLLEXPORT jl_genericmemoryref_t jl_memoryrefindex(jl_genericmemoryref_t m JL_ROOTING_ARGUMENT, size_t idx)
{
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m.mem))->layout;
    if ((layout->flags.arrayelem_isboxed || !layout->flags.arrayelem_isunion) && layout->size != 0) {
        m.ptr_or_offset = (void*)((char*)m.ptr_or_offset + idx * layout->size);
        assert((char*)m.ptr_or_offset - (char*)m.mem->ptr < layout->size * m.mem->length);
    }
    else {
        m.ptr_or_offset = (void*)((size_t)m.ptr_or_offset + idx);
        assert((size_t)m.ptr_or_offset < m.mem->length);
    }
    return m;
}

JL_DLLEXPORT jl_value_t *jl_ptrmemrefget(jl_genericmemoryref_t m JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    assert((char*)m.ptr_or_offset - (char*)m.mem->ptr < sizeof(jl_value_t*) * m.mem->length);
    assert(((jl_datatype_t*)jl_typetagof(m.mem))->layout->flags.arrayelem_isboxed);
    jl_value_t *elt = jl_atomic_load_relaxed((_Atomic(jl_value_t*)*)m.ptr_or_offset);
    if (elt == NULL)
        jl_throw(jl_undefref_exception);
    return elt;
}

JL_DLLEXPORT jl_value_t *jl_memoryrefget(jl_genericmemoryref_t m)
{
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m.mem))->layout;
    if (layout->flags.arrayelem_isboxed)
        return jl_ptrmemrefget(m);
    jl_value_t *isatomic = jl_tparam0(jl_typetagof(m.mem)); (void)isatomic; // TODO
    jl_value_t *eltype = jl_tparam1(jl_typetagof(m.mem));
    char *data = (char*)m.ptr_or_offset;
    if (layout->flags.arrayelem_isunion) {
        assert(jl_is_uniontype(eltype));
        size_t i = (size_t)data;
        assert(i < m.mem->length);
        // isbits union selector bytes are always stored directly after the last memory element
        uint8_t sel = jl_genericmemory_typetagdata(m.mem)[i];
        eltype = jl_nth_union_component(eltype, sel);
        data = (char*)m.mem->ptr + i * layout->size;
    }
    if (layout->size == 0) {
        assert(jl_is_datatype_singleton((jl_datatype_t*)eltype));
        return ((jl_datatype_t*)eltype)->instance;
    }
    assert(data - (char*)m.mem->ptr < layout->size * m.mem->length);
    jl_value_t *r = undefref_check((jl_datatype_t*)eltype, jl_new_bits(eltype, data));
    if (__unlikely(r == NULL))
        jl_throw(jl_undefref_exception);
    return r;
}

static int _jl_memoryref_isassigned(jl_genericmemoryref_t m)
{
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m.mem))->layout;
    if (layout->flags.arrayelem_isboxed) {
        return jl_atomic_load_relaxed((_Atomic(jl_value_t*)*)m.ptr_or_offset) != NULL;
    }
    else if (layout->first_ptr >= 0) {
         jl_value_t **elem = (jl_value_t**)m.ptr_or_offset;
         return elem[layout->first_ptr] != NULL;
    }
    return 1;
}

JL_DLLEXPORT jl_value_t *jl_memoryref_isassigned(jl_genericmemoryref_t m)
{
    return _jl_memoryref_isassigned(m) ? jl_true : jl_false;
}

JL_DLLEXPORT void jl_memoryrefset(jl_genericmemoryref_t m JL_ROOTING_ARGUMENT, jl_value_t *rhs JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED)
{
    jl_value_t *isatomic = jl_tparam0(jl_typetagof(m.mem)); (void)isatomic; // TODO
    jl_value_t *eltype = jl_tparam1(jl_typetagof(m.mem));
    if (eltype != (jl_value_t*)jl_any_type && !jl_typeis(rhs, eltype)) {
        JL_GC_PUSH1(&rhs);
        if (!jl_isa(rhs, eltype))
            jl_type_error("memoryrefset!", eltype, rhs);
        JL_GC_POP();
    }
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m.mem))->layout;
    if (layout->flags.arrayelem_isboxed) {
        assert((char*)m.ptr_or_offset - (char*)m.mem->ptr < sizeof(jl_value_t*) * m.mem->length);
        jl_atomic_store_release((_Atomic(jl_value_t*)*)m.ptr_or_offset, rhs);
        jl_gc_wb(jl_genericmemory_owner(m.mem), rhs);
    }
    else {
        int hasptr;
        char *data = (char*)m.ptr_or_offset;
        if (layout->flags.arrayelem_isunion) {
            assert(jl_is_uniontype(eltype));
            size_t i = (size_t)data;
            assert(i < m.mem->length);
            uint8_t *psel = (uint8_t*)jl_genericmemory_typetagdata(m.mem) + i;
            unsigned nth = 0;
            if (!jl_find_union_component(eltype, jl_typeof(rhs), &nth))
                assert(0 && "invalid genericmemoryset to isbits union");
            *psel = nth;
            hasptr = 0;
            data = (char*)m.mem->ptr + i * layout->size;
        }
        else {
            hasptr = layout->first_ptr >= 0;
        }
        if (layout->size != 0) {
            assert(data - (char*)m.mem->ptr < layout->size * m.mem->length);
            genericmemoryassign_safe(hasptr, jl_genericmemory_owner(m.mem), data, rhs);
        }
    }
}

JL_DLLEXPORT void jl_memoryrefunset(jl_genericmemoryref_t m)
{
    if (m.mem->length == 0)
        jl_bounds_error_int((jl_value_t*)m.mem, 1);
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m.mem))->layout;
    if (layout->flags.arrayelem_isboxed) {
        jl_atomic_store_relaxed((_Atomic(jl_value_t*)*)m.ptr_or_offset, NULL);
    }
    else if (layout->first_ptr >= 0) {
        size_t elsize = layout->size;
        jl_assume(elsize >= sizeof(void*) && elsize % sizeof(void*) == 0);
        memset(m.ptr_or_offset, 0, elsize);
    }
}

JL_DLLEXPORT jl_value_t *ijl_genericmemory_owner(jl_genericmemory_t *m JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    return jl_genericmemory_owner(m);
}
#ifdef __cplusplus
}
#endif
