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

// genericmemory constructors ---------------------------------------------------------
JL_DLLEXPORT char *jl_genericmemory_typetagdata(jl_genericmemory_t *m) JL_NOTSAFEPOINT
{
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m))->layout;
    assert(layout->flags.arrayelem_isunion);
    return (char*)m->ptr + m->length * layout->size;
}

#define MAXINTVAL (((size_t)-1)>>1)

// ONLY USE FROM CODEGEN. It only partially initializes the mem
JL_DLLEXPORT jl_genericmemory_t *jl_alloc_genericmemory_unchecked(jl_ptls_t ptls, size_t nbytes, jl_datatype_t *mtype)
{
    size_t tot = nbytes + LLT_ALIGN(sizeof(jl_genericmemory_t),JL_SMALL_BYTE_ALIGNMENT);

    int pooled = tot <= GC_MAX_SZCLASS;
    char *data;
    jl_genericmemory_t *m;
    if (!pooled) {
        data = (char*)jl_gc_managed_malloc(nbytes);
        tot = sizeof(jl_genericmemory_t) + sizeof(void*);
    }
    m = (jl_genericmemory_t*)jl_gc_alloc(ptls, tot, mtype);
    if (pooled) {
        data = (char*)m + JL_SMALL_BYTE_ALIGNMENT;
    }
    else {
        int isaligned = 1; // jl_gc_managed_malloc is always aligned
        jl_gc_track_malloced_genericmemory(ptls, m, isaligned);
        jl_genericmemory_data_owner_field(m) = (jl_value_t*)m;
    }
    // length set by codegen
    m->ptr = data;
    return m;
}

jl_genericmemory_t *_new_genericmemory_(jl_value_t *mtype, size_t nel, int8_t isunion, int8_t zeroinit, size_t elsz)
{
    if (nel == 0) // zero-sized allocation optimization
        return (jl_genericmemory_t*)((jl_datatype_t*)mtype)->instance;
    size_t nbytes;
    int overflow = __builtin_mul_overflow(nel, elsz, &nbytes);
    if (isunion) {
        // an extra byte for each isbits union memory element, stored at m->ptr + m->length
        overflow |= __builtin_add_overflow(nel, nbytes, &nbytes);
    }
    if ((nel >= MAXINTVAL-1) || (nbytes >= MAXINTVAL-1) || overflow)
        jl_exceptionf(jl_argumenterror_type, "invalid GenericMemory size: the number of elements is either negative or too large for system address width");
    jl_task_t *ct = jl_current_task;
    jl_genericmemory_t *m = jl_alloc_genericmemory_unchecked((jl_ptls_t) ct->ptls, nbytes, (jl_datatype_t*)mtype);
    m->length = nel;
    if (zeroinit)
        memset((char*)m->ptr, 0, nbytes);
    return m;
}

JL_DLLEXPORT jl_genericmemory_t *jl_alloc_genericmemory(jl_value_t *mtype, size_t nel)
{
    assert(jl_is_datatype(mtype));
    jl_genericmemory_t *m = (jl_genericmemory_t*)((jl_datatype_t*)mtype)->instance;
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)mtype)->layout;
    if (m == NULL) {
        jl_value_t *kind = jl_tparam0((jl_datatype_t*)mtype);
        if (kind != (jl_value_t*)jl_not_atomic_sym && kind != (jl_value_t*)jl_atomic_sym)
            jl_error("GenericMemory kind must be :not_atomic or :atomic");
        jl_value_t *addrspace = jl_tparam2((jl_datatype_t*)mtype);
        if (!jl_is_addrspacecore(addrspace) || jl_unbox_uint8(addrspace) != 0)
            jl_error("GenericMemory addrspace must be Core.CPU");
        if (!((jl_datatype_t*)mtype)->has_concrete_subtype || layout == NULL)
            jl_type_error_rt("GenericMemory", "element type", (jl_value_t*)jl_type_type, jl_tparam1(mtype));
        abort(); // this is checked already by jl_get_genericmemory_layout
    }
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
    if (jl_string_len(str) == 0)
        return (jl_genericmemory_t*)((jl_datatype_t*)jl_memory_uint8_type)->instance;
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
        jl_value_t *kind = jl_tparam0((jl_datatype_t*)mtype);
        if (kind != (jl_value_t*)jl_not_atomic_sym && kind != (jl_value_t*)jl_atomic_sym)
            jl_error("GenericMemory kind must be :not_atomic or :atomic");
        jl_value_t *addrspace = jl_tparam2((jl_datatype_t*)mtype);
        if (!jl_is_addrspacecore(addrspace) || jl_unbox_uint8(addrspace) != 0)
            jl_error("GenericMemory addrspace must be Core.CPU");
        if (!((jl_datatype_t*)mtype)->has_concrete_subtype || layout == NULL)
            jl_type_error_rt("GenericMemory", "element type", (jl_value_t*)jl_type_type, jl_tparam1(mtype));
        abort();
    }
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
    size_t nbytes;
    int overflow = __builtin_mul_overflow(nel, elsz, &nbytes);
    if (isunion) {
        // an extra byte for each isbits union memory element, stored at m->ptr + m->length
        overflow |= __builtin_add_overflow(nel, nbytes, &nbytes);
    }
    if ((nel >= MAXINTVAL) || (nbytes >= MAXINTVAL) || overflow)
        jl_exceptionf(jl_argumenterror_type, "invalid GenericMemory size: the number of elements is either negative or too large for system address width");
    int tsz = sizeof(jl_genericmemory_t) + sizeof(void*);
    m = (jl_genericmemory_t*)jl_gc_alloc(ct->ptls, tsz, mtype);
    m->ptr = data;
    m->length = nel;
    jl_genericmemory_data_owner_field(m) = own_buffer ? (jl_value_t*)m : NULL;
    if (own_buffer) {
        int isaligned = 0;  // TODO: allow passing memalign'd buffers
        jl_gc_track_malloced_genericmemory(ct->ptls, m, isaligned);
        size_t allocated_bytes = memory_block_usable_size(data, isaligned);
        jl_gc_count_allocd(allocated_bytes);
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
        if (how == 3 && // implies jl_is_string(o)
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
    // n.b. how == 0 is always pool-allocated, so the freed bytes are computed from the pool not the object
    return jl_pchar_to_string((const char*)m->ptr, len);
}

JL_DLLEXPORT jl_genericmemory_t *jl_alloc_memory_any(size_t n)
{
    return jl_alloc_genericmemory(jl_memory_any_type, n);
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
        jl_gc_wb_genericmemory_copy_boxed(owner, dest_p, src, src_p, &n);
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
        jl_gc_wb_genericmemory_copy_ptr(owner, src, src_p, n, dt);
    }
    else {
        memmove(destdata, srcdata, n * elsz);
    }
}


// genericmemory primitives -----------------------------------------------------------

JL_DLLEXPORT jl_value_t *jl_genericmemoryref(jl_genericmemory_t *mem, size_t i)
{
    int isatomic = (jl_tparam0(jl_typetagof(mem)) == (jl_value_t*)jl_atomic_sym);
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(mem))->layout;
    jl_genericmemoryref_t m;
    m.mem = mem;
    m.ptr_or_offset = (layout->flags.arrayelem_isunion || layout->size == 0) ? (void*)i : (void*)((char*)mem->ptr + layout->size * i);
    return jl_memoryrefget(m, isatomic);
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

static jl_value_t *jl_ptrmemrefget(jl_genericmemoryref_t m JL_PROPAGATES_ROOT, int isatomic) JL_NOTSAFEPOINT
{
    assert((char*)m.ptr_or_offset - (char*)m.mem->ptr < sizeof(jl_value_t*) * m.mem->length);
    assert(((jl_datatype_t*)jl_typetagof(m.mem))->layout->flags.arrayelem_isboxed);
    _Atomic(jl_value_t*) *ptr = (_Atomic(jl_value_t*)*)m.ptr_or_offset;
    jl_value_t *elt = isatomic ? jl_atomic_load(ptr) : jl_atomic_load_relaxed(ptr);
    if (elt == NULL)
        jl_throw(jl_undefref_exception);
    return elt;
}

JL_DLLEXPORT jl_value_t *jl_memoryrefget(jl_genericmemoryref_t m, int isatomic)
{
    assert(isatomic == (jl_tparam0(jl_typetagof(m.mem)) == (jl_value_t*)jl_atomic_sym));
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m.mem))->layout;
    if (layout->flags.arrayelem_isboxed)
        return jl_ptrmemrefget(m, isatomic);
    jl_value_t *eltype = jl_tparam1(jl_typetagof(m.mem));
    char *data = (char*)m.ptr_or_offset;
    if (layout->flags.arrayelem_isunion) {
        assert(!isatomic);
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
    jl_value_t *r;
    size_t fsz = jl_datatype_size(eltype);
    int needlock = isatomic && fsz > MAX_ATOMIC_SIZE;
    if (isatomic && !needlock) {
        r = jl_atomic_new_bits(eltype, data);
    }
    else if (needlock) {
        jl_task_t *ct = jl_current_task;
        r = jl_gc_alloc(ct->ptls, fsz, eltype);
        jl_lock_field((jl_mutex_t*)data);
        memcpy((char*)r, data + LLT_ALIGN(sizeof(jl_mutex_t), JL_SMALL_BYTE_ALIGNMENT), fsz);
        jl_unlock_field((jl_mutex_t*)data);
    }
    else {
        // TODO: a finalizer here could make the isunion case not quite right
        r = jl_new_bits(eltype, data);
    }
    r = undefref_check((jl_datatype_t*)eltype, r);
    if (__unlikely(r == NULL))
        jl_throw(jl_undefref_exception);
    return r;
}

static int _jl_memoryref_isassigned(jl_genericmemoryref_t m, int isatomic)
{
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m.mem))->layout;
    _Atomic(jl_value_t*) *elem = (_Atomic(jl_value_t*)*)m.ptr_or_offset;
    if (layout->flags.arrayelem_isboxed) {
    }
    else if (layout->first_ptr >= 0) {
        int needlock = isatomic && layout->size > MAX_ATOMIC_SIZE;
        if (needlock)
            elem = elem + LLT_ALIGN(sizeof(jl_mutex_t), JL_SMALL_BYTE_ALIGNMENT) / sizeof(jl_value_t*);
        elem = &elem[layout->first_ptr];
    }
    else {
        return 1;
    }
    return (isatomic ? jl_atomic_load(elem) : jl_atomic_load_relaxed(elem)) != NULL;
}

JL_DLLEXPORT jl_value_t *jl_memoryref_isassigned(jl_genericmemoryref_t m, int isatomic)
{
    return _jl_memoryref_isassigned(m, isatomic) ? jl_true : jl_false;
}

JL_DLLEXPORT void jl_memoryrefset(jl_genericmemoryref_t m JL_ROOTING_ARGUMENT, jl_value_t *rhs JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED, int isatomic)
{
    assert(isatomic == (jl_tparam0(jl_typetagof(m.mem)) == (jl_value_t*)jl_atomic_sym));
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
        if (isatomic)
            jl_atomic_store((_Atomic(jl_value_t*)*)m.ptr_or_offset, rhs);
        else
            jl_atomic_store_release((_Atomic(jl_value_t*)*)m.ptr_or_offset, rhs);
        jl_gc_wb(jl_genericmemory_owner(m.mem), rhs);
        return;
    }
    int hasptr;
    char *data = (char*)m.ptr_or_offset;
    if (layout->flags.arrayelem_isunion) {
        assert(!isatomic);
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
        int needlock = isatomic && layout->size > MAX_ATOMIC_SIZE;
        size_t fsz = jl_datatype_size((jl_datatype_t*)jl_typeof(rhs)); // need to shrink-wrap the final copy
        if (isatomic && !needlock) {
            jl_atomic_store_bits(data, rhs, fsz);
        }
        else if (needlock) {
            jl_lock_field((jl_mutex_t*)data);
            memassign_safe(hasptr, data + LLT_ALIGN(sizeof(jl_mutex_t), JL_SMALL_BYTE_ALIGNMENT), rhs, fsz);
            jl_unlock_field((jl_mutex_t*)data);
        }
        else {
            memassign_safe(hasptr, data, rhs, fsz);
        }
        if (hasptr)
            jl_gc_multi_wb(jl_genericmemory_owner(m.mem), rhs); // rhs is immutable
    }
}

JL_DLLEXPORT jl_value_t *jl_memoryrefswap(jl_genericmemoryref_t m, jl_value_t *rhs, int isatomic)
{
    jl_value_t *eltype = jl_tparam1(jl_typetagof(m.mem));
    if (eltype != (jl_value_t*)jl_any_type && !jl_typeis(rhs, eltype)) {
        if (!jl_isa(rhs, eltype))
            jl_type_error("memoryrefswap!", eltype, rhs);
    }
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m.mem))->layout;
    jl_value_t *owner = jl_genericmemory_owner(m.mem);
    char *data = (char*)m.ptr_or_offset;
    if (layout->flags.arrayelem_isboxed) {
        assert(data - (char*)m.mem->ptr < sizeof(jl_value_t*) * m.mem->length);
        jl_value_t *r;
        if (isatomic)
            r = jl_atomic_exchange((_Atomic(jl_value_t*)*)data, rhs);
        else
            r = jl_atomic_exchange_release((_Atomic(jl_value_t*)*)data, rhs);
        jl_gc_wb(owner, rhs);
        if (__unlikely(r == NULL))
            jl_throw(jl_undefref_exception);
        return r;
    }
    uint8_t *psel = NULL;
    if (layout->flags.arrayelem_isunion) {
        assert(!isatomic);
        assert(jl_is_uniontype(eltype));
        size_t i = (size_t)data;
        assert(i < m.mem->length);
        psel = (uint8_t*)jl_genericmemory_typetagdata(m.mem) + i;
        data = (char*)m.mem->ptr + i * layout->size;
    }
    return swap_bits(eltype, data, psel, owner, rhs, isatomic ? isatomic_field : isatomic_none);
}

JL_DLLEXPORT jl_value_t *jl_memoryrefmodify(jl_genericmemoryref_t m, jl_value_t *op, jl_value_t *rhs, int isatomic)
{
    jl_value_t *eltype = jl_tparam1(jl_typetagof(m.mem));
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m.mem))->layout;
    jl_value_t *owner = jl_genericmemory_owner(m.mem);
    char *data = (char*)m.ptr_or_offset;
    if (layout->flags.arrayelem_isboxed) {
        assert(data - (char*)m.mem->ptr < sizeof(jl_value_t*) * m.mem->length);
        return modify_value(eltype, (_Atomic(jl_value_t*)*)data, owner, op, rhs, isatomic, NULL, NULL);
    }
    size_t fsz = layout->size;
    uint8_t *psel = NULL;
    if (layout->flags.arrayelem_isunion) {
        assert(!isatomic);
        assert(jl_is_uniontype(eltype));
        size_t i = (size_t)data;
        assert(i < m.mem->length);
        psel = (uint8_t*)jl_genericmemory_typetagdata(m.mem) + i;
        data = (char*)m.mem->ptr + i * fsz;
    }
    return modify_bits(eltype, data, psel, owner, op, rhs, isatomic ? isatomic_field : isatomic_none);
}

JL_DLLEXPORT jl_value_t *jl_memoryrefreplace(jl_genericmemoryref_t m, jl_value_t *expected, jl_value_t *rhs, int isatomic)
{
    jl_value_t *eltype = jl_tparam1(jl_typetagof(m.mem));
    if (eltype != (jl_value_t*)jl_any_type && !jl_typeis(rhs, eltype)) {
        if (!jl_isa(rhs, eltype))
            jl_type_error("memoryrefreplace!", eltype, rhs);
    }
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m.mem))->layout;
    jl_value_t *owner = jl_genericmemory_owner(m.mem);
    char *data = (char*)m.ptr_or_offset;
    if (layout->flags.arrayelem_isboxed) {
        assert(data - (char*)m.mem->ptr < sizeof(jl_value_t*) * m.mem->length);
        return replace_value(eltype, (_Atomic(jl_value_t*)*)data, owner, expected, rhs, isatomic, NULL, NULL);
    }
    uint8_t *psel = NULL;
    if (layout->flags.arrayelem_isunion) {
        assert(!isatomic);
        assert(jl_is_uniontype(eltype));
        size_t i = (size_t)data;
        assert(i < m.mem->length);
        psel = (uint8_t*)jl_genericmemory_typetagdata(m.mem) + i;
        data = (char*)m.mem->ptr + i * layout->size;
    }
    return replace_bits(eltype, data, psel, owner, expected, rhs, isatomic ? isatomic_field : isatomic_none);
}

JL_DLLEXPORT jl_value_t *jl_memoryrefsetonce(jl_genericmemoryref_t m, jl_value_t *rhs, int isatomic)
{
    jl_value_t *eltype = jl_tparam1(jl_typetagof(m.mem));
    if (eltype != (jl_value_t*)jl_any_type && !jl_typeis(rhs, eltype)) {
        if (!jl_isa(rhs, eltype))
            jl_type_error("memoryrefsetonce!", eltype, rhs);
    }
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m.mem))->layout;
    jl_value_t *owner = jl_genericmemory_owner(m.mem);
    char *data = (char*)m.ptr_or_offset;
    int success;
    if (layout->flags.arrayelem_isboxed) {
        assert(data - (char*)m.mem->ptr < sizeof(jl_value_t*) * m.mem->length);
        jl_value_t *r = NULL;
        _Atomic(jl_value_t*) *px = (_Atomic(jl_value_t*)*)data;
        success = isatomic ? jl_atomic_cmpswap(px, &r, rhs) : jl_atomic_cmpswap_release(px, &r, rhs);
        if (success)
            jl_gc_wb(owner, rhs);
    }
    else {
        if (layout->flags.arrayelem_isunion) {
            assert(!isatomic);
            assert(jl_is_uniontype(eltype));
            size_t i = (size_t)data;
            assert(i < m.mem->length);
            (void)i;
            success = 0;
        }
        else if (layout->first_ptr < 0) {
            success = 0;
        }
        else {
            success = setonce_bits((jl_datatype_t*)eltype, data, owner, rhs, isatomic ? isatomic_field : isatomic_none);
        }
    }
    return success ? jl_true : jl_false;
}

JL_DLLEXPORT jl_value_t *ijl_genericmemory_owner(jl_genericmemory_t *m JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    return jl_genericmemory_owner(m);
}
#ifdef __cplusplus
}
#endif
