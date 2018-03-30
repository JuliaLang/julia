// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  saving and restoring system images
*/
#include <stdlib.h>
#include <string.h>
#include <stdio.h> // printf

#include "julia.h"
#include "julia_internal.h"
#include "builtin_proto.h"
#include "processor.h"

#ifndef _OS_WINDOWS_
#include <dlfcn.h>
#endif

#ifndef _COMPILER_MICROSOFT_
#include "valgrind.h"
#else
#define RUNNING_ON_VALGRIND 0
#endif
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// TODO: put WeakRefs on the weak_refs list during deserialization
// TODO: handle finalizers

// hash of definitions for predefined tagged object
static htable_t sertag_table;
static htable_t symbol_table;
static uintptr_t nsym_tag;
// array of definitions for the predefined tagged object types
// (reverse of sertag_table and symbol_table)
static arraylist_t deser_tag;
static arraylist_t deser_sym;

// table of all objects that are serialized
static htable_t backref_table;
static int backref_table_numel;
static arraylist_t layout_table;

// list of (size_t pos, (void *f)(jl_value_t*)) entries
// for the serializer to mark values in need of rework by function f
// during deserialization later
static arraylist_t reinit_list;

// list of modules being deserialized with __init__ methods
// (not used in MODE_AST)
jl_array_t *jl_module_init_order;

// hash of definitions for predefined function pointers
static htable_t fptr_to_id;
// array of definitions for the predefined function pointers
// (reverse of fptr_to_id)
static const jl_fptr_t id_to_fptrs[] = {
    NULL, NULL,
    jl_f_throw, jl_f_is, jl_f_typeof, jl_f_issubtype, jl_f_isa,
    jl_f_typeassert, jl_f__apply, jl_f__apply_pure, jl_f__apply_latest, jl_f_isdefined,
    jl_f_tuple, jl_f_svec, jl_f_intrinsic_call, jl_f_invoke_kwsorter,
    jl_f_getfield, jl_f_setfield, jl_f_fieldtype, jl_f_nfields,
    jl_f_arrayref, jl_f_arrayset, jl_f_arraysize, jl_f_apply_type,
    jl_f_applicable, jl_f_invoke, jl_f_sizeof, jl_f__expr,
    NULL };

typedef enum _DUMP_MODES {
    // not in the serializer at all, or
    // something is seriously wrong
    MODE_INVALID = 0,

    // jl_restore_system_image
    // restoring an entire system image from disk
    MODE_SYSTEM_IMAGE,
} DUMP_MODES;

typedef struct {
    ios_t *s;
    ios_t *const_data;
    ios_t *symbols;
    ios_t *relocs;
    ios_t *gvar_record;
    ios_t *fptr_record;
    arraylist_t relocs_list;
    arraylist_t gctags_list;
    DUMP_MODES mode;
    jl_ptls_t ptls;
} jl_serializer_state;

static jl_value_t *jl_idtable_type = NULL;
static jl_typename_t *jl_idtable_typename = NULL;
static arraylist_t builtin_typenames;

enum RefTags {
    DataRef,
    ConstDataRef,
    TagRef,
    SymbolRef,
    BindingRef
};

// this supports up to 1 GB images and 16 RefTags
// if a larger size is required, will need to add support for writing larger relocations in many cases below
#define RELOC_TAG_OFFSET 28


/* read and write in host byte order */

#define write_uint8(s, n) ios_putc((n), (s))
#define read_uint8(s) ((uint8_t)ios_getc((s)))

static void write_uint32(ios_t *s, uint32_t i)
{
    ios_write(s, (char*)&i, 4);
}

static uint32_t read_uint32(ios_t *s)
{
    uint32_t x = 0;
    ios_read(s, (char*)&x, 4);
    return x;
}


// --- Static Compile ---

extern int globalUnique;
static void *jl_sysimg_handle = NULL;
static uint64_t sysimage_base = 0;
static uintptr_t *sysimg_gvars_base = NULL;
static const int32_t *sysimg_gvars_offsets = NULL;
static jl_sysimg_fptrs_t sysimg_fptrs;

static inline uintptr_t *sysimg_gvars(uintptr_t *base, size_t idx)
{
    return base + sysimg_gvars_offsets[idx] / sizeof(base[0]);
}

JL_DLLEXPORT int jl_running_on_valgrind(void)
{
    return RUNNING_ON_VALGRIND;
}

static void jl_load_sysimg_so(void)
{
    int imaging_mode = jl_generating_output() && !jl_options.incremental;
    // in --build mode only use sysimg data, not precompiled native code
    if (!imaging_mode && jl_options.use_sysimage_native_code==JL_OPTIONS_USE_SYSIMAGE_NATIVE_CODE_YES) {
        sysimg_gvars_base = (uintptr_t*)jl_dlsym(jl_sysimg_handle, "jl_sysimg_gvars_base");
        sysimg_gvars_offsets = (const int32_t*)jl_dlsym(jl_sysimg_handle,
                                                        "jl_sysimg_gvars_offsets");
        sysimg_gvars_offsets += 1;
        assert(sysimg_fptrs.base);
        globalUnique = *(size_t*)jl_dlsym(jl_sysimg_handle, "jl_globalUnique");
#ifdef JULIA_ENABLE_THREADING
        uintptr_t *tls_getter_slot = (uintptr_t*)jl_dlsym(jl_sysimg_handle,
                                                          "jl_get_ptls_states_slot");
        *tls_getter_slot = (uintptr_t)jl_get_ptls_states_getter();
        size_t *tls_offset_idx = (size_t*)jl_dlsym(jl_sysimg_handle, "jl_tls_offset");
        *tls_offset_idx = (uintptr_t)(jl_tls_offset == -1 ? 0 : jl_tls_offset);
#endif

#ifdef _OS_WINDOWS_
        sysimage_base = (intptr_t)jl_sysimg_handle;
#else
        Dl_info dlinfo;
        if (dladdr((void*)sysimg_gvars_base, &dlinfo) != 0) {
            sysimage_base = (intptr_t)dlinfo.dli_fbase;
        }
        else {
            sysimage_base = 0;
        }
#endif
    }
    else {
        memset(&sysimg_fptrs, 0, sizeof(sysimg_fptrs));
    }
    const char *sysimg_data = (const char*)jl_dlsym(jl_sysimg_handle, "jl_system_image_data");
    size_t len = *(size_t*)jl_dlsym(jl_sysimg_handle, "jl_system_image_size");
    jl_restore_system_image_data(sysimg_data, len);
}


// --- serializer ---

static uintptr_t jl_fptr_id(void *fptr)
{
    void **pbp = ptrhash_bp(&fptr_to_id, fptr);
    if (*pbp == HT_NOTFOUND || fptr == NULL)
        return 1;
    else
        return *(uintptr_t*)pbp;
}

int32_t jl_jlcall_api(const char *fname)
{
    // give the function an index in the constant lookup table
    if (fname == NULL)
        return 0;
    if (!strncmp(fname, "japi3_", 6)) // jlcall abi 3 from JIT
        return JL_API_WITH_PARAMETERS;
    assert(!strncmp(fname, "japi1_", 6) ||  // jlcall abi 1 from JIT
           !strncmp(fname, "jsys1_", 6) ||  // jlcall abi 1 from sysimg
           !strncmp(fname, "jlcall_", 7) || // jlcall abi 1 from JIT wrapping a specsig method
           !strncmp(fname, "jlsysw_", 7));  // jlcall abi 1 from sysimg wrapping a specsig method
    return JL_API_GENERIC;
}


#define jl_serialize_value(s, v) jl_serialize_value_(s,(jl_value_t*)(v))
static void jl_serialize_value_(jl_serializer_state *s, jl_value_t *v);


static void jl_serialize_module(jl_serializer_state *s, jl_module_t *m)
{
    jl_serialize_value(s, m->name);
    jl_serialize_value(s, m->parent);
    size_t i;
    void **table = m->bindings.table;
    for (i = 1; i < m->bindings.size; i += 2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m || m != jl_main_module) {
                jl_serialize_value(s, b->name);
                jl_serialize_value(s, b->value);
                jl_serialize_value(s, b->globalref);
                jl_serialize_value(s, b->owner);
            }
        }
    }

    if (m != jl_main_module) {
        for (i = 0; i < m->usings.len; i++) {
            jl_serialize_value(s, (jl_value_t*)m->usings.items[i]);
        }
    }
}


static void jl_serialize_value_(jl_serializer_state *s, jl_value_t *v)
{
    if (v == NULL || jl_is_symbol(v)) {
        return;
    }

    void *builtin = ptrhash_get(&sertag_table, v);
    if (builtin != HT_NOTFOUND) {
        return;
    }

    void **bp = ptrhash_bp(&backref_table, v);
    if (*bp != HT_NOTFOUND) {
        return;
    }

    size_t item = ++backref_table_numel;
    assert(item < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "too many items to serialize");
    char *pos = (char*)HT_NOTFOUND + item;
    *bp = (void*)pos;

    // some values have special representations
    jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
    jl_serialize_value(s, t);

    if (t->layout->npointers == 0) {
        // skip it
    }
    else if (jl_is_svec(v)) {
        size_t i, l = jl_svec_len(v);
        jl_value_t **data = jl_svec_data(v);
        for (i = 0; i < l; i++) {
            jl_serialize_value(s, data[i]);
        }
    }
    else if (jl_is_array(v)) {
        jl_array_t *ar = (jl_array_t*)v;
        jl_serialize_value(s, jl_typeof(ar));
        if (ar->flags.ptrarray) {
            size_t i, l = jl_array_len(ar);
            for (i = 0; i < l; i++) {
                jl_serialize_value(s, jl_array_ptr_ref(ar, i));
            }
        }
    }
    else if (jl_typeis(v, jl_module_type)) {
        jl_serialize_module(s, (jl_module_t*)v);
    }
    else if (jl_typeis(v, jl_task_type)) {
        jl_error("Task cannot be serialized");
    }
    else {
        char *data = (char*)jl_data_ptr(v);
        size_t i, nf = jl_datatype_nfields(t);
        for (i = 0; i < nf; i++) {
            if (jl_field_isptr(t, i)) {
                char *slot = data + jl_field_offset(t, i);
                jl_value_t *fld = *(jl_value_t**)slot;
                jl_serialize_value(s, fld);
            }
        }
    }
}

static void ios_ensureroom(ios_t *s, size_t newsize)
{
    size_t prevsize = s->size;
    if (prevsize < newsize) {
        ios_trunc(s, newsize);
        assert(s->size == newsize);
        memset(&s->buf[prevsize], 0, newsize - prevsize);
    }
}

static void record_gvar(jl_serializer_state *s, int gid, uintptr_t reloc_id)
{
    if (gid == 0)
        return;
    ios_ensureroom(s->gvar_record, gid * sizeof(uint32_t));
    ios_seek(s->gvar_record, (gid - 1) * sizeof(uint32_t));
    assert(reloc_id < UINT32_MAX);
    write_uint32(s->gvar_record, reloc_id);
}


static void write_padding(ios_t *s, size_t nb)
{
    static const char zeros[16] = {0};
    while (nb > 16) {
        ios_write(s, zeros, 16);
        nb -= 16;
    }
    if (nb != 0)
        ios_write(s, zeros, nb);
}


static void write_pointer(ios_t *s)
{
    assert((ios_pos(s) & (sizeof(void*) - 1)) == 0 && "stream misaligned for writing a word-sized value");
    write_padding(s, sizeof(void*));
}


#define backref_id(s, v) _backref_id(s, (jl_value_t*)(v))
static uintptr_t _backref_id(jl_serializer_state *s, jl_value_t *v)
{
    assert(v != NULL && "cannot get backref to NULL object");
    void *idx = HT_NOTFOUND;
    if (jl_is_symbol(v)) {
        void **pidx = ptrhash_bp(&symbol_table, v);
        idx = *pidx;
        if (idx == HT_NOTFOUND) {
            size_t l = strlen(jl_symbol_name((jl_sym_t*)v));
            write_uint32(s->symbols, l);
            ios_write(s->symbols, jl_symbol_name((jl_sym_t*)v), l + 1);
            size_t offset = ++nsym_tag;
            assert(offset < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "too many symbols");
            idx = (void*)((char*)HT_NOTFOUND + ((uintptr_t)SymbolRef << RELOC_TAG_OFFSET) + offset);
            *pidx = idx;
        }
    }
    else {
        idx = ptrhash_get(&sertag_table, v);
    }
    if (idx == HT_NOTFOUND) {
        idx = ptrhash_get(&backref_table, v);
        assert(idx != HT_NOTFOUND && "object missed during jl_serialize_value pass");
    }
    return (char*)idx - 1 - (char*)HT_NOTFOUND;
}


static void write_pointerfield(jl_serializer_state *s, jl_value_t *fld)
{
    if (fld != NULL) {
        arraylist_push(&s->relocs_list, (void*)(uintptr_t)ios_pos(s->s));
        arraylist_push(&s->relocs_list, (void*)backref_id(s, fld));
    }
    write_pointer(s->s);
}

static void write_gctaggedfield(jl_serializer_state *s, uintptr_t ref)
{
    arraylist_push(&s->gctags_list, (void*)(uintptr_t)ios_pos(s->s));
    arraylist_push(&s->gctags_list, (void*)ref);
    write_pointer(s->s);
}


static void jl_write_module(jl_serializer_state *s, uintptr_t item, jl_module_t *m)
{
    size_t reloc_offset = ios_pos(s->s);
    size_t tot = sizeof(jl_module_t);
    ios_write(s->s, (char*)m, tot);

    jl_module_t *newm = (jl_module_t*)&s->s->buf[reloc_offset];
    newm->name = NULL;
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, name)));
    arraylist_push(&s->relocs_list, (void*)backref_id(s, m->name));
    newm->parent = NULL;
    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, parent)));
    arraylist_push(&s->relocs_list, (void*)backref_id(s, m->parent));
    newm->primary_world = jl_world_counter;

    // write out the bindings table as a list
    // immediately after jl_module_t
    // (the ptrhash will need to be recreated on load)
    size_t count = 0;
    size_t i;
    void **table = m->bindings.table;
    for (i = 1; i < m->bindings.size; i += 2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m || m != jl_main_module) {
                write_gctaggedfield(s, (uintptr_t)BindingRef << RELOC_TAG_OFFSET);
                tot += sizeof(void*);
                size_t binding_reloc_offset = ios_pos(s->s);
                record_gvar(s, jl_get_llvm_gv((jl_value_t*)b), ((uintptr_t)DataRef << RELOC_TAG_OFFSET) + binding_reloc_offset);
                write_pointerfield(s, (jl_value_t*)b->name);
                write_pointerfield(s, b->value);
                write_pointerfield(s, b->globalref);
                write_pointerfield(s, (jl_value_t*)b->owner);
                size_t flag_offset = offsetof(jl_binding_t, owner) + sizeof(b->owner);
                ios_write(s->s, (char*)b + flag_offset, sizeof(*b) - flag_offset);
                tot += sizeof(jl_binding_t);
                count += 1;
            }
        }
    }
    assert(ios_pos(s->s) - reloc_offset == tot);
    newm = (jl_module_t*)&s->s->buf[reloc_offset];
    newm->bindings.size = count; // stash the count in newm->size
    newm->bindings.table = NULL;
    memset(&newm->bindings._space, 0, sizeof(newm->bindings._space));

    // write out the usings list
    memset(&newm->usings._space, 0, sizeof(newm->usings._space));
    if (m == jl_main_module) {
        newm->usings.len = 1;
        newm->usings.max = AL_N_INLINE;
        newm->usings.items = (void**)offsetof(jl_module_t, usings._space);
        arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings.items)));
        arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item));
        arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings._space[0])));
        arraylist_push(&s->relocs_list, (void*)backref_id(s, jl_core_module));
    }
    else {
        if (newm->usings.items == &newm->usings._space[0]) {
            m->usings.max = AL_N_INLINE;
            newm->usings.items = (void**)offsetof(jl_module_t, usings._space);
            arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings.items)));
            arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item));
            size_t i;
            for (i = 0; i < m->usings.len; i++) {
                arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings._space[i])));
                arraylist_push(&s->relocs_list, (void*)backref_id(s, m->usings._space[i]));
            }
        }
        else {
            newm->usings.items = (void**)tot;
            arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_module_t, usings.items)));
            arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item));
            size_t i;
            for (i = 0; i < m->usings.len; i++) {
                write_pointerfield(s, (jl_value_t*)m->usings.items[i]);
                tot += sizeof(void*);
            }
            for (; i < m->usings.max; i++) {
                write_pointer(s->s);
                tot += sizeof(void*);
            }
            newm = (jl_module_t*)&s->s->buf[reloc_offset];
        }
    }
}

#if 0
static size_t jl_sort_size(jl_datatype_t *dt)
{
    if (dt == jl_simplevector_type)
        return SIZE_MAX - 5;
    if (dt == jl_string_type)
        return SIZE_MAX - 4;
    if (dt->name == jl_array_typename)
        return SIZE_MAX - 3;
    if (dt == jl_datatype_type)
        return SIZE_MAX - 2;
    if (dt == jl_module_type)
        return SIZE_MAX - 1;
    return jl_datatype_size(dt);
}
#endif

static int sysimg_sort_order(const void *pa, const void *pb)
{
    uintptr_t sa = ((uintptr_t*)pa)[1];
    uintptr_t sb = ((uintptr_t*)pb)[1];
    return (sa > sb ? 1 : (sa < sb ? -1 : 0));
#if 0
    jl_value_t *a = *(jl_value_t**)pa;
    jl_datatype_t *tya = (jl_datatype_t*)jl_typeof(a);
    size_t sa = jl_sort_size(tya);
    jl_value_t *b = *(jl_value_t**)pb;
    jl_datatype_t *tyb = (jl_datatype_t*)jl_typeof(b);
    size_t sb = jl_sort_size(tyb);
    if (sa == sb) {
        sa = tya->uid;
        sb = tyb->uid;
    }
    return (sa > sb ? 1 : (sa < sb ? -1 : 0));
#endif
}

jl_value_t *jl_find_ptr = NULL;
static void jl_write_values(jl_serializer_state *s)
{
    arraylist_t objects_list;
    arraylist_new(&objects_list, backref_table_numel * 2);

    arraylist_new(&layout_table, 0);
    arraylist_grow(&layout_table, backref_table_numel);
    memset(layout_table.items, 0, backref_table_numel * sizeof(void*));

    size_t i, len = backref_table.size;
    void **p = backref_table.table;
    for (i = 0; i < len; i += 2) {
        char *reloc_id = (char*)p[i + 1];
        if (reloc_id != HT_NOTFOUND) {
            jl_value_t *v = (jl_value_t*)p[i];
            uintptr_t item = reloc_id - 1 - (char*)HT_NOTFOUND;
            objects_list.items[objects_list.len++] = (void*)v;
            objects_list.items[objects_list.len++] = (void*)item;
        }
    }
    assert(backref_table_numel * 2 == objects_list.len);
    qsort(objects_list.items, backref_table_numel, sizeof(void*) * 2, sysimg_sort_order);

    for (i = 0, len = backref_table_numel * 2; i < len; i += 2) {
        jl_value_t *v = (jl_value_t*)objects_list.items[i];
        uintptr_t item = (uintptr_t)objects_list.items[i + 1];
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
        assert((t->instance == NULL || t->instance == v) && "detected singleton construction corruption");
        // realign stream to expected gc alignment (16 bytes)
        uintptr_t skip_header_pos = ios_pos(s->s) + sizeof(jl_taggedvalue_t);
        write_padding(s->s, LLT_ALIGN(skip_header_pos, 16) - skip_header_pos);
        // write header
        write_gctaggedfield(s, backref_id(s, t));
        size_t reloc_offset = ios_pos(s->s);
        assert(item < layout_table.len && layout_table.items[item] == NULL);
        layout_table.items[item] = (void*)reloc_offset;
        record_gvar(s, jl_get_llvm_gv(v), ((uintptr_t)DataRef << RELOC_TAG_OFFSET) + reloc_offset);

        // write data
        if (jl_is_cpointer(v)) {
            write_pointer(s->s);
        }
        else if (jl_is_array(v)) {
#define JL_ARRAY_ALIGN(jl_value, nbytes) LLT_ALIGN(jl_value, nbytes)
            jl_array_t *ar = (jl_array_t*)v;
            int ndimwords = jl_array_ndimwords(ar->flags.ndims);
            size_t tsz = JL_ARRAY_ALIGN(sizeof(jl_array_t) + ndimwords * sizeof(size_t), JL_CACHE_BYTE_ALIGNMENT);
            // copy header
            ios_write(s->s, (char*)v, tsz);
            // make some header modifications in-place
            jl_array_t *newa = (jl_array_t*)&s->s->buf[reloc_offset];
            size_t alen = jl_array_len(ar);
            size_t extra = (!ar->flags.ptrarray && jl_is_uniontype(jl_tparam0(jl_typeof(ar)))) ? alen : 0;
            size_t tot = alen * ar->elsize + extra;
            if (newa->flags.ndims == 1)
                newa->maxsize = alen;
            newa->offset = 0;
            newa->flags.how = 0;
            newa->flags.pooled = 0;
            newa->flags.isshared = 0;

            // write data
            if (!ar->flags.ptrarray) {
                uintptr_t data = LLT_ALIGN(ios_pos(s->const_data), 16);
                // realign stream to max(data-align(array), sizeof(void*))
                write_padding(s->const_data, data - ios_pos(s->const_data));
                // write data and relocations
                newa->data = NULL; // relocation offset
                data /= sizeof(void*);
                assert(data < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "offset to constant data too large");
                arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_array_t, data))); // relocation location
                arraylist_push(&s->relocs_list, (void*)(((uintptr_t)ConstDataRef << RELOC_TAG_OFFSET) + data)); // relocation target
                if (ar->elsize == 1)
                    tot += 1;
                ios_write(s->const_data, (char*)jl_array_data(ar), tot);
            }
            else {
                newa->data = (void*)tsz; // relocation offset
                arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_array_t, data))); // relocation location
                arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item)); // relocation target
                size_t i;
                for (i = 0; i < alen; i++) {
                    write_pointerfield(s, jl_array_ptr_ref(v, i));
                }
            }
        }
        else if (jl_typeis(v, jl_module_type)) {
            jl_write_module(s, item, (jl_module_t*)v);
            // will need to recreate the binding table for this
            arraylist_push(&reinit_list, (void*)item);
            arraylist_push(&reinit_list, (void*)5);
        }
        else if (jl_typeis(v, jl_task_type)) {
            jl_error("Task cannot be serialized");
        }
        else if (jl_is_svec(v)) {
            ios_write(s->s, (char*)v, sizeof(void*));
            size_t i, l = jl_svec_len(v);
            assert(l > 0);
            for (i = 0; i < l; i++) {
                write_pointerfield(s, jl_svecref(v, i));
            }
        }
        else if (jl_is_string(v)) {
            ios_write(s->s, (char*)v, sizeof(void*));
            ios_write(s->s, jl_string_data(v), jl_string_len(v));
            write_uint8(s->s, '\0'); // null-terminated strings for easier C-compatibility
        }
        else if (jl_datatype_nfields(t) == 0) {
            assert(t->layout->npointers == 0);
            if (t->size > 0)
                ios_write(s->s, (char*)v, t->size);
        }
        else {
            size_t i, nf = jl_datatype_nfields(t);
            size_t tot = 0;
            for (i = 0; i < nf; i++) {
                size_t offset = jl_field_offset(t, i);
                char *slot = (char*)v + offset;
                write_padding(s->s, offset - tot);
                tot = offset;
                size_t fsz = jl_field_size(t, i);
                if (jl_field_isptr(t, i) > 0) {
                    write_pointerfield(s, *(jl_value_t**)slot);
                }
                else if (t->mutabl && jl_is_cpointer_type(jl_field_type(t, i))) {
                    write_pointer(s->s);
                }
                else if (fsz > 0) {
                    ios_write(s->s, slot, fsz);
                }
                tot += fsz;
            }

            if (jl_is_method(v)) {
                write_padding(s->s, sizeof(jl_method_t) - tot);
            }
            else if (jl_is_method_instance(v)) {
                jl_method_instance_t *m = (jl_method_instance_t*)v;
                jl_method_instance_t *newm = (jl_method_instance_t*)&s->s->buf[reloc_offset];
                newm->fptr = NULL;
                newm->unspecialized_ducttape = NULL;
                if (jl_is_method(m->def.method)) {
                    uintptr_t fptr_id = jl_fptr_id((void*)(uintptr_t)m->fptr);
                    if (m->jlcall_api == JL_API_CONST) {
                    }
                    else if (fptr_id >= 2) {
                        //write_int8(s->s, -li->jlcall_api);
                        //write_uint16(s->s, fptr_id);
                        newm->fptr = (jl_fptr_t)fptr_id;
                        arraylist_push(&reinit_list, (void*)item);
                        arraylist_push(&reinit_list, (void*)6);
                    }
                    else if (m->functionObjectsDecls.functionObject) {
                        int jlcall_api = jl_jlcall_api(m->functionObjectsDecls.functionObject);
                        assert(jlcall_api);
                        newm->jlcall_api = jlcall_api;
                        // save functionObject pointers
                        int cfunc = jl_assign_functionID(m->functionObjectsDecls.specFunctionObject);
                        int func = jl_assign_functionID(m->functionObjectsDecls.functionObject);
                        assert(reloc_offset < INT32_MAX);
                        if (cfunc != 0) {
                            ios_ensureroom(s->fptr_record, cfunc * sizeof(void*));
                            ios_seek(s->fptr_record, (cfunc - 1) * sizeof(void*));
                            write_uint32(s->fptr_record, ~reloc_offset);
#ifdef _P64
                            write_padding(s->fptr_record, 4);
#endif
                        }
                        if (func != 0) {
                            ios_ensureroom(s->fptr_record, func * sizeof(void*));
                            ios_seek(s->fptr_record, (func - 1) * sizeof(void*));
                            write_uint32(s->fptr_record, reloc_offset);
#ifdef _P64
                            write_padding(s->fptr_record, 4);
#endif
                        }
                    }
                    else {
                        newm->jlcall_api = 0;
                    }
                }
                newm->functionObjectsDecls.functionObject = NULL;
                newm->functionObjectsDecls.specFunctionObject = NULL;
            }
            else if (jl_is_datatype(v)) {
                jl_datatype_t *dt = (jl_datatype_t*)v;
                jl_datatype_t *newdt = (jl_datatype_t*)&s->s->buf[reloc_offset];
                newdt->struct_decl = NULL;
                newdt->ditype = NULL;
                if (dt->layout != NULL) {
                    size_t nf = dt->layout->nfields;
                    size_t fieldsize = jl_fielddesc_size(dt->layout->fielddesc_type);
                    int has_padding = dt->layout->npointers && nf;
                    char *flddesc = (char*)dt->layout;
                    size_t fldsize = sizeof(jl_datatype_layout_t) + nf * fieldsize;
                    uintptr_t layout_unaligned = LLT_ALIGN(ios_pos(s->const_data), sizeof(uint32_t));
                    uintptr_t layout = LLT_ALIGN(ios_pos(s->const_data), sizeof(void*));
                    if (has_padding) {
                        if (layout == layout_unaligned) {
                            layout += sizeof(void*);
                            layout_unaligned = layout - sizeof(uint32_t);
                        }
                        flddesc -= sizeof(uint32_t);
                        fldsize += sizeof(uint32_t);
                        write_padding(s->const_data, layout_unaligned - ios_pos(s->const_data)); // realign stream
                    }
                    else {
                        write_padding(s->const_data, layout - ios_pos(s->const_data)); // realign stream
                    }
                    newdt->layout = NULL; // relocation offset
                    layout /= sizeof(void*);
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_datatype_t, layout))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)ConstDataRef << RELOC_TAG_OFFSET) + layout)); // relocation target
                    ios_write(s->const_data, flddesc, fldsize);
                }
            }
            else if (((jl_datatype_t*)(jl_typeof(v)))->name == jl_idtable_typename) {
                // will need to rehash this, later (after types are fully constructed)
                arraylist_push(&reinit_list, (void*)item);
                arraylist_push(&reinit_list, (void*)1);
            }
            else {
                write_padding(s->s, t->size - tot);
            }
        }
    }
}


static void jl_write_gv_syms(jl_serializer_state *s, jl_sym_t *v)
{
    // since symbols are static, they might not have had a
    // reference anywhere in the code image other than here
    int32_t gv = jl_get_llvm_gv((jl_value_t*)v);
    if (gv != 0) {
        uintptr_t item = backref_id(s, v);
        assert(item >> RELOC_TAG_OFFSET == SymbolRef);
        record_gvar(s, gv, item);
    }
    if (v->left)
        jl_write_gv_syms(s, v->left);
    if (v->right)
        jl_write_gv_syms(s, v->right);
}


static inline uint32_t load_uint32(uintptr_t *base)
{
    uint32_t v = **(uint32_t**)base;
    *base += 4;
    return v;
}


static void jl_read_symbols(jl_serializer_state *s)
{
    assert(deser_sym.len == nsym_tag);
    uintptr_t base = (uintptr_t)&s->symbols->buf[0];
    uintptr_t end = base + s->symbols->size;
    while (base < end) {
        uint32_t len = load_uint32(&base);
        const char *str = (const char*)base;
        base += len + 1;
        //printf("symbol %3d: %s\n", len, str);
        jl_sym_t *sym = jl_symbol_n(str, len);
        arraylist_push(&deser_sym, (void*)sym);
    }
}


static uintptr_t get_reloc_for_item(uintptr_t reloc_item, size_t reloc_offset)
{
    enum RefTags tag = (enum RefTags)(reloc_item >> RELOC_TAG_OFFSET);
    if (tag == DataRef) {
        // need to compute the final relocation offset via the layout table
        assert(reloc_item < layout_table.len);
        uintptr_t reloc_base = (uintptr_t)layout_table.items[reloc_item];
        assert(reloc_base != 0 && "layout offset missing for relocation item");
        // write reloc_offset into s->s at pos
        return reloc_base + reloc_offset;
    }
    else {
        // just write the item reloc_id directly
#ifndef JL_NDEBUG
        assert(reloc_offset == 0 && "offsets for relocations to builtin objects should be precomposed in the reloc_item");
        size_t offset = (reloc_item & (((uintptr_t)1 << RELOC_TAG_OFFSET) - 1));
        switch (tag) {
        case ConstDataRef:
            break;
        case TagRef:
            assert(offset >= 2 && offset < deser_tag.len && deser_tag.items[offset] && "corrupt relocation item id");
            break;
        case SymbolRef:
            assert(offset < nsym_tag && "corrupt relocation item id");
            break;
        case BindingRef:
            assert(offset == 0 && "corrupt relocation offset");
            break;
        case DataRef:
        default:
            assert("corrupt relocation item id");
        }
#endif
        return reloc_item; // pre-composed relocation + offset
    }
}


static inline uintptr_t get_item_for_reloc(jl_serializer_state *s, uintptr_t base, size_t size, uint32_t reloc_id)
{
    enum RefTags tag = (enum RefTags)(reloc_id >> RELOC_TAG_OFFSET);
    size_t offset = (reloc_id & (((uintptr_t)1 << RELOC_TAG_OFFSET) - 1));
    switch (tag) {
    case DataRef:
        assert(offset < size);
        return base + offset;
    case ConstDataRef:
        return (uintptr_t)deser_tag.items[0] + (offset * sizeof(void*));
    case TagRef:
        assert(offset < deser_tag.len && deser_tag.items[offset] && "corrupt relocation item id");
        return (uintptr_t)deser_tag.items[offset];
    case SymbolRef:
        assert(offset < deser_sym.len && deser_sym.items[offset] && "corrupt relocation item id");
        return (uintptr_t)deser_sym.items[offset];
    case BindingRef:
        return jl_buff_tag | GC_OLD_MARKED;
    }
    abort();
}


static void jl_write_skiplist(ios_t *s, char *base, size_t size, arraylist_t *list)
{
    size_t i;
    for (i = 0; i < list->len; i += 2) {
        size_t pos = (size_t)list->items[i];
        size_t item = (size_t)list->items[i + 1];
        uintptr_t *pv = (uintptr_t*)(base + pos);
        assert(pos < size && pos != 0);
        *pv = get_reloc_for_item(item, *pv);
        // record pos in relocations list
        // TODO: save space by using delta-compression
        assert(pos < UINT32_MAX);
        write_uint32(s, pos);
    }
    write_uint32(s, 0);
}


static void jl_write_relocations(jl_serializer_state *s)
{
    char *base = &s->s->buf[0];
    jl_write_skiplist(s->relocs, base, s->s->size, &s->gctags_list);
    jl_write_skiplist(s->relocs, base, s->s->size, &s->relocs_list);
}


static void jl_read_relocations(jl_serializer_state *s, uint8_t bits)
{
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    size_t size = s->s->size;
    while (1) {
        uintptr_t val = (uintptr_t)&s->relocs->buf[s->relocs->bpos];
        uint32_t offset = load_uint32(&val);
        s->relocs->bpos += sizeof(uint32_t);
        if (offset == 0)
            break;
        uintptr_t *pv = (uintptr_t*)(base + offset);
        uintptr_t v = *pv;
        v = get_item_for_reloc(s, base, size, v);
        *pv = v | bits;
    }
}

static char* sysimg_base;
static char* sysimg_relocs;
void gc_sweep_sysimg(void)
{
    uintptr_t base = (uintptr_t)sysimg_base;
    uintptr_t relocs = (uintptr_t)sysimg_relocs;
    if (relocs == 0)
        return;
    while (1) {
        uint32_t offset = load_uint32(&relocs);
        if (offset == 0)
            break;
        jl_taggedvalue_t *o = (jl_taggedvalue_t*)(base + offset);
        o->bits.gc = GC_OLD;
    }
}

#define jl_write_value(s, v) _jl_write_value((s), (jl_value_t*)(v))
static void _jl_write_value(jl_serializer_state *s, jl_value_t *v)
{
    if (v == NULL) {
        write_uint32(s->s, 0);
        return;
    }
    uintptr_t item = backref_id(s, v);
    uintptr_t reloc = get_reloc_for_item(item, 0);
    assert(reloc < UINT32_MAX);
    write_uint32(s->s, reloc);
}


static jl_value_t *jl_read_value(jl_serializer_state *s)
{
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    size_t size = s->s->size;
    uintptr_t val = base + s->s->bpos;
    uint32_t offset = load_uint32(&val);
    s->s->bpos += sizeof(uint32_t);
    if (offset == 0)
        return NULL;
    return (jl_value_t*)get_item_for_reloc(s, base, size, offset);
}


static void jl_update_all_fptrs(jl_serializer_state *s)
{
    jl_sysimg_fptrs_t fvars = sysimg_fptrs;
    // make these NULL now so we skip trying to restore GlobalVariable pointers later
    sysimg_gvars_base = NULL;
    sysimg_fptrs.base = NULL;
    int sysimg_fvars_max = s->fptr_record->size / sizeof(void*);
    size_t i;
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    jl_method_instance_t **linfos = (jl_method_instance_t**)&s->fptr_record->buf[0];
    uint32_t clone_idx = 0;
    for (i = 0; i < sysimg_fvars_max; i++) {
        uintptr_t val = (uintptr_t)&linfos[i];
        uint32_t offset = load_uint32(&val);
        if (offset != 0) {
            int cfunc = 0;
            if (offset & ((uintptr_t)1 << (8 * sizeof(uint32_t) - 1))) {
                // if high bit is set, this is cfunc, not func
                cfunc = 1;
                offset = ~offset;
            }
            jl_method_instance_t *li = (jl_method_instance_t*)(base + offset);
            if (fvars.base == NULL) {
                li->jlcall_api = 0;
            }
            else {
                uintptr_t base = (uintptr_t)fvars.base;
                assert(jl_is_method(li->def.method) && li->jlcall_api && li->jlcall_api != JL_API_CONST);
                linfos[i] = li;
                int32_t offset = fvars.offsets[i];
                for (; clone_idx < fvars.nclones; clone_idx++) {
                    uint32_t idx = fvars.clone_idxs[clone_idx] & jl_sysimg_val_mask;
                    if (idx < i)
                        continue;
                    if (idx == i)
                        offset = fvars.clone_offsets[clone_idx];
                    break;
                }
                jl_fptr_to_llvm((jl_fptr_t)(base + offset), li, cfunc);
            }
        }
    }
    if (fvars.base) {
        jl_register_fptrs(sysimage_base, &fvars, linfos, sysimg_fvars_max);
    }
}


static void jl_update_all_gvars(jl_serializer_state *s)
{
    if (sysimg_gvars_base == NULL)
        return;
    size_t gvname_index = 0;
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    size_t size = s->s->size;
    uintptr_t gvars = (uintptr_t)&s->gvar_record->buf[0];
    uintptr_t end = gvars + s->gvar_record->size;
    while (gvars < end) {
        uint32_t offset = load_uint32(&gvars);
        if (offset) {
            uintptr_t v = get_item_for_reloc(s, base, size, offset);
            *sysimg_gvars(sysimg_gvars_base, gvname_index) = v;
        }
        gvname_index += 1;
    }
}


static void jl_finalize_serializer(jl_serializer_state *s)
{
    size_t i, l;
    // save module initialization order
    if (jl_module_init_order != NULL) {
        l = jl_array_len(jl_module_init_order);
        for (i = 0; i < l; i++) {
            // verify that all these modules were saved
            assert(ptrhash_get(&backref_table, jl_array_ptr_ref(jl_module_init_order, i)) != HT_NOTFOUND);
        }
    }
    jl_write_value(s, jl_module_init_order);

    // record list of reinitialization functions
    l = reinit_list.len;
    for (i = 0; i < l; i += 2) {
        size_t item = (size_t)reinit_list.items[i];
        size_t reloc_offset = (size_t)layout_table.items[item];
        assert(reloc_offset != 0);
        write_uint32(s->s, (uint32_t)reloc_offset);
        write_uint32(s->s, (uint32_t)((uintptr_t)reinit_list.items[i + 1]));
    }
    write_uint32(s->s, 0);
}


void jl_typemap_rehash(union jl_typemap_t ml, int8_t offs);
static void jl_reinit_item(jl_value_t *v, int how, arraylist_t *tracee_list)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_TRY {
        switch (how) {
            case 1: { // rehash IdDict
                jl_array_t **a = (jl_array_t**)v;
                assert(jl_is_array(*a));
                // Assume *a don't need a write barrier
                *a = jl_idtable_rehash(*a, jl_array_len(*a));
                jl_gc_wb(v, *a);
                break;
            }
            case 2: { // reinsert module v into parent (const)
                jl_module_t *mod = (jl_module_t*)v;
                assert(jl_is_module(mod));
                jl_binding_t *b = jl_get_binding_wr(mod->parent, mod->name, 1);
                jl_declare_constant(b); // this can throw
                if (b->value != NULL) {
                    if (!jl_is_module(b->value)) {
                        jl_errorf("Invalid redefinition of constant %s.",
                                  jl_symbol_name(mod->name)); // this also throws
                    }
                    if (jl_generating_output() && jl_options.incremental) {
                        jl_errorf("Cannot replace module %s during incremental precompile.", jl_symbol_name(mod->name));
                    }
                    jl_printf(JL_STDERR, "WARNING: replacing module %s.\n",
                              jl_symbol_name(mod->name));
                }
                b->value = v;
                jl_gc_wb_binding(b, v);
                break;
            }
            case 3: { // rehash MethodTable
                jl_methtable_t *mt = (jl_methtable_t*)v;
                assert(jl_is_mtable(mt));
                jl_typemap_rehash(mt->defs, 0);
                // TODO: consider reverting this when we can split on Type{...} better
                jl_typemap_rehash(mt->cache, 1); //(mt == jl_type_typename->mt) ? 0 : 1);
                if (tracee_list)
                    arraylist_push(tracee_list, mt);
                break;
            }
            case 4: { // rehash specializations tfunc
                jl_method_t *m = (jl_method_t*)v;
                assert(jl_is_method(m));
                jl_typemap_rehash(m->specializations, 0);
                break;
            }
            case 5: { // rebuild the binding table for module v
                jl_module_t *mod = (jl_module_t*)v;
                assert(jl_is_module(mod));
                size_t nbindings = mod->bindings.size;
                htable_new(&mod->bindings, nbindings);
                struct binding {
                    uintptr_t tag;
                    jl_binding_t b;
                } *b;
                b = (struct binding*)&mod[1];
                while (nbindings > 0) {
                    ptrhash_put(&mod->bindings, (char*)b->b.name, &b->b);
                    b += 1;
                    nbindings -= 1;
                }
                break;
            }
            case 6: { // assign the real fptr for m
                jl_method_instance_t *m = (jl_method_instance_t*)v;
                uintptr_t fptr_id = (uintptr_t)m->fptr;
                assert(fptr_id <= sizeof(id_to_fptrs) / sizeof(*id_to_fptrs) &&
                       fptr_id >= 2 && "unknown function pointer ID");
                m->fptr = id_to_fptrs[fptr_id];
                break;
            }
            default:
                assert(0 && "corrupt deserialization state");
                abort();
        }
    }
    JL_CATCH {
        jl_printf(JL_STDERR, "WARNING: error while reinitializing value ");
        jl_static_show(JL_STDERR, v);
        jl_printf(JL_STDERR, ":\n");
        jl_static_show(JL_STDERR, ptls->exception_in_transit);
        jl_printf(JL_STDERR, "\n");
    }
}


static jl_array_t *jl_finalize_deserializer(jl_serializer_state *s, arraylist_t *tracee_list)
{
    jl_array_t *init_order = (jl_array_t*)jl_read_value(s);

    // run reinitialization functions
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    while (1) {
        size_t offset = read_uint32(s->s);
        if (offset == 0)
            break;
        jl_value_t *v = (jl_value_t*)(base + offset);
        jl_reinit_item(v, read_uint32(s->s), tracee_list);
    }
    return init_order;
}



// --- helper functions ---

// remove cached types not referenced in the stream
static void jl_prune_type_cache(jl_svec_t *cache)
{
    size_t l = jl_svec_len(cache), ins = 0, i;
    for (i = 0; i < l; i++) {
        jl_value_t *ti = jl_svecref(cache, i);
        if (ti == NULL)
            break;
        if (ptrhash_get(&backref_table, ti) != HT_NOTFOUND || jl_get_llvm_gv(ti) != 0)
            jl_svecset(cache, ins++, ti);
        else if (jl_is_datatype(ti)) {
            jl_value_t *singleton = ((jl_datatype_t*)ti)->instance;
            if (singleton && (ptrhash_get(&backref_table, singleton) != HT_NOTFOUND || jl_get_llvm_gv(singleton) != 0))
                jl_svecset(cache, ins++, ti);
        }
    }
    if (i > ins) {
        memset(&jl_svec_data(cache)[ins], 0, (i - ins) * sizeof(jl_value_t*));
    }
}


// --- entry points ---

static void jl_init_serializer2(int);
static void jl_cleanup_serializer2(void);

static void jl_save_system_image_to_stream(ios_t *f)
{
    jl_gc_collect(1); // full
    jl_gc_collect(0); // incremental (sweep finalizers)
    JL_TIMING(SYSIMG_DUMP);
    int en = jl_gc_enable(0);
    jl_init_serializer2(1);
    htable_reset(&backref_table, 250000);
    arraylist_new(&reinit_list, 0);
    backref_table_numel = 0;
    ios_t sysimg, const_data, symbols, relocs, gvar_record, fptr_record;
    ios_mem(&sysimg,     1000000);
    ios_mem(&const_data,  100000);
    ios_mem(&symbols,     100000);
    ios_mem(&relocs,      100000);
    ios_mem(&gvar_record, 100000);
    ios_mem(&fptr_record, 100000);
    jl_serializer_state s;
    s.s = &sysimg;
    s.const_data = &const_data;
    s.symbols = &symbols;
    s.relocs = &relocs;
    s.gvar_record = &gvar_record;
    s.fptr_record = &fptr_record;
    s.mode = MODE_SYSTEM_IMAGE;
    s.ptls = jl_get_ptls_states();
    arraylist_new(&s.relocs_list, 0);
    arraylist_new(&s.gctags_list, 0);

    // empty!(Core.ARGS)
    if (jl_core_module != NULL) {
        jl_array_t *args = (jl_array_t*)jl_get_global(jl_core_module, jl_symbol("ARGS"));
        if (args != NULL) {
            jl_array_del_end(args, jl_array_len(args));
        }
    }

    jl_idtable_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("IdDict")) : NULL;
    jl_idtable_typename = jl_base_module ? ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_idtable_type))->name : NULL;

    { // step 1: record values (recursively) that need to go in the image
        jl_serialize_value(&s, jl_core_module);
        jl_serialize_value(&s, jl_main_module);
        jl_serialize_value(&s, jl_top_module);
        jl_serialize_value(&s, jl_typeinf_func);
        jl_serialize_value(&s, jl_module_init_order);

        // serialize method tables of builtin types
        jl_serialize_value(&s, jl_type_typename->mt);
        jl_serialize_value(&s, jl_intrinsic_type->name->mt);
        jl_serialize_value(&s, jl_sym_type->name->mt);
        jl_serialize_value(&s, jl_array_typename->mt);
        jl_serialize_value(&s, jl_module_type->name->mt);

        jl_prune_type_cache(jl_tuple_typename->cache);
        jl_prune_type_cache(jl_tuple_typename->linearcache);
        jl_prune_type_cache(jl_type_typename->cache);

        uintptr_t i;
        for (i = 0; i < builtin_typenames.len; i++) {
            jl_serialize_value(&s, ((jl_typename_t*)builtin_typenames.items[i])->cache);
            jl_serialize_value(&s, ((jl_typename_t*)builtin_typenames.items[i])->linearcache);
        }
    }

    { // step 2: build all the sysimg sections
        write_padding(&sysimg, sizeof(uint32_t));
        jl_write_values(&s);
        jl_write_relocations(&s);
        jl_write_gv_syms(&s, jl_get_root_symbol());
        // ensure everything in deser_tag are reassociated with their GlobalValue
        uintptr_t i;
        for (i = 0; i < deser_tag.len; i++) {
            jl_value_t *v = (jl_value_t*)deser_tag.items[i];
            record_gvar(&s, jl_get_llvm_gv(v), ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + i);
        }
    }

    // step 3: combine all of the sections into one file
    write_uint32(f, sysimg.size - sizeof(uint32_t));
    ios_seek(&sysimg, sizeof(uint32_t));
    ios_copyall(f, &sysimg);
    ios_close(&sysimg);

    write_uint32(f, const_data.size);
    // realign stream to max-alignment for data
    write_padding(f, LLT_ALIGN(ios_pos(f), 16) - ios_pos(f));
    ios_seek(&const_data, 0);
    ios_copyall(f, &const_data);
    ios_close(&const_data);

    write_uint32(f, symbols.size);
    ios_seek(&symbols, 0);
    ios_copyall(f, &symbols);
    ios_close(&symbols);

    write_uint32(f, relocs.size);
    ios_seek(&relocs, 0);
    ios_copyall(f, &relocs);
    ios_close(&relocs);

    write_uint32(f, gvar_record.size);
    ios_seek(&gvar_record, 0);
    ios_copyall(f, &gvar_record);
    ios_close(&gvar_record);

    write_uint32(f, fptr_record.size);
    ios_seek(&fptr_record, 0);
    ios_copyall(f, &fptr_record);
    ios_close(&fptr_record);

    { // step 4: record locations of special roots
        s.s = f;
        jl_finalize_serializer(&s);
        jl_write_value(&s, jl_main_module);
        jl_write_value(&s, jl_top_module);
        jl_write_value(&s, jl_typeinf_func);
        write_uint32(f, jl_typeinf_world);
        jl_write_value(&s, jl_type_typename->mt);
        jl_write_value(&s, jl_intrinsic_type->name->mt);
        jl_write_value(&s, jl_sym_type->name->mt);
        jl_write_value(&s, jl_array_typename->mt);
        jl_write_value(&s, jl_module_type->name->mt);
        uintptr_t i;
        for (i = 0; i < builtin_typenames.len; i++) {
            jl_write_value(&s, ((jl_typename_t*)builtin_typenames.items[i])->cache);
            jl_write_value(&s, ((jl_typename_t*)builtin_typenames.items[i])->linearcache);
        }
        write_uint32(f, jl_get_t_uid_ctr());
        write_uint32(f, jl_get_gs_ctr());
        write_uint32(f, jl_world_counter);
    }

    arraylist_free(&layout_table);
    arraylist_free(&reinit_list);
    arraylist_free(&s.relocs_list);
    arraylist_free(&s.gctags_list);
    jl_cleanup_serializer2();

    jl_gc_enable(en);
}

JL_DLLEXPORT ios_t *jl_create_system_image(void)
{
    ios_t *f = (ios_t*)malloc(sizeof(ios_t));
    ios_mem(f, 0);
    jl_save_system_image_to_stream(f);
    return f;
}

JL_DLLEXPORT size_t ios_write_direct(ios_t *dest, ios_t *src);
JL_DLLEXPORT void jl_save_system_image(const char *fname)
{
    ios_t f;
    if (ios_file(&f, fname, 1, 1, 1, 1) == NULL) {
        jl_errorf("cannot open system image file \"%s\" for writing", fname);
    }
    JL_SIGATOMIC_BEGIN();
    jl_save_system_image_to_stream(&f);
    ios_close(&f);
    JL_SIGATOMIC_END();
}

extern void jl_get_builtins(void);
extern void jl_get_builtin_hooks(void);
extern void jl_gc_set_permalloc_region(void *start, void *end);

// Takes in a path of the form "usr/lib/julia/sys.so" (jl_restore_system_image should be passed the same string)
JL_DLLEXPORT void jl_preload_sysimg_so(const char *fname)
{
    if (jl_sysimg_handle)
        return; // embedded target already called jl_set_sysimg_so

    char *dot = (char*) strrchr(fname, '.');
    int is_ji = (dot && !strcmp(dot, ".ji"));

    // Get handle to sys.so
    if (!is_ji) // .ji extension => load .ji file only
        jl_set_sysimg_so(jl_load_dynamic_library(fname, JL_RTLD_LOCAL | JL_RTLD_NOW));
}

// Allow passing in a module handle directly, rather than a path
JL_DLLEXPORT void jl_set_sysimg_so(void *handle)
{
    void* *jl_RTLD_DEFAULT_handle_pointer = (void**)jl_dlsym_e(handle, "jl_RTLD_DEFAULT_handle_pointer");
    if (!jl_RTLD_DEFAULT_handle_pointer || (void*)&jl_RTLD_DEFAULT_handle != *jl_RTLD_DEFAULT_handle_pointer)
        jl_error("System image file failed consistency check: maybe opened the wrong version?");
    if (jl_options.cpu_target == NULL)
        jl_options.cpu_target = "native";
    jl_sysimg_handle = handle;
    sysimg_fptrs = jl_init_processor_sysimg(handle);
}

static void jl_restore_system_image_from_stream(ios_t *f)
{
    JL_TIMING(SYSIMG_LOAD);
    jl_ptls_t ptls = jl_get_ptls_states();
    int en = jl_gc_enable(0);
    jl_init_serializer2(0);
    ios_t sysimg, const_data, symbols, relocs, gvar_record, fptr_record;
    jl_serializer_state s;
    s.s = &sysimg;
    s.const_data = &const_data;
    s.symbols = &symbols;
    s.relocs = &relocs;
    s.gvar_record = &gvar_record;
    s.fptr_record = &fptr_record;
    s.mode = MODE_SYSTEM_IMAGE;
    s.ptls = jl_get_ptls_states();
    arraylist_new(&s.relocs_list, 0);
    arraylist_new(&s.gctags_list, 0);

    // step 1: read section map and apply relocations
    assert(ios_pos(f) == 0 && f->bm == bm_mem);
    size_t sizeof_sysimg = read_uint32(f);
    ios_static_buffer(&sysimg, f->buf, sizeof_sysimg + sizeof(uint32_t));
    ios_skip(f, sizeof_sysimg);

    size_t sizeof_constdata = read_uint32(f);
    // realign stream to max-alignment for data
    ios_seek(f, LLT_ALIGN(ios_pos(f), 16));
    ios_static_buffer(&const_data, f->buf + f->bpos, sizeof_constdata);
    ios_skip(f, sizeof_constdata);

    size_t sizeof_symbols = read_uint32(f);
    ios_static_buffer(&symbols, f->buf + f->bpos, sizeof_symbols);
    ios_skip(f, sizeof_symbols);

    size_t sizeof_relocations = read_uint32(f);
    assert(!ios_eof(f));
    ios_static_buffer(&relocs, f->buf + f->bpos, sizeof_relocations);
    ios_skip(f, sizeof_relocations);

    size_t sizeof_gvar_record = read_uint32(f);
    assert(!ios_eof(f));
    ios_static_buffer(&gvar_record, f->buf + f->bpos, sizeof_gvar_record);
    ios_skip(f, sizeof_gvar_record);

    size_t sizeof_fptr_record = read_uint32(f);
    assert(!ios_eof(f));
    ios_static_buffer(&fptr_record, f->buf + f->bpos, sizeof_fptr_record);
    ios_skip(f, sizeof_fptr_record);

    assert(!ios_eof(f));
    jl_read_symbols(&s);
    ios_close(&symbols);

    sysimg_base = &sysimg.buf[0];
    sysimg_relocs = &relocs.buf[0];
    jl_gc_set_permalloc_region((void*)sysimg_base, (void*)(sysimg_base + sysimg.size));

    deser_tag.items[0] = (void*)const_data.buf;
    jl_read_relocations(&s, GC_OLD_MARKED); // gctags
    size_t sizeof_tags = ios_pos(&relocs);
    jl_read_relocations(&s, 0); // general relocs
    ios_close(&relocs);
    ios_close(&const_data);
    ios_close(&sysimg);

    if (0) {
        printf("sysimg size breakdown:\n"
               "     sys data: %8u\n"
               "  isbits data: %8u\n"
               "      symbols: %8u\n"
               "    tags list: %8u\n"
               "   reloc list: %8u\n"
               "    gvar list: %8u\n"
               "    fptr list: %8u\n",
            (unsigned)sizeof_sysimg,
            (unsigned)sizeof_constdata,
            (unsigned)sizeof_symbols,
            (unsigned)sizeof_tags,
            (unsigned)(sizeof_relocations - sizeof_tags),
            (unsigned)sizeof_gvar_record,
            (unsigned)sizeof_fptr_record);
    }

    // step 2: get special values
    s.s = f;
    jl_module_init_order = jl_finalize_deserializer(&s, NULL);
    jl_main_module = (jl_module_t*)jl_read_value(&s);
    jl_top_module = (jl_module_t*)jl_read_value(&s);
    jl_internal_main_module = jl_main_module;

    jl_typeinf_func = (jl_function_t*)jl_read_value(&s);
    jl_typeinf_world = read_uint32(f);
    jl_type_type_mt = (jl_methtable_t*)jl_read_value(&s);
    jl_type_typename->mt = jl_type_type_mt;
    jl_unionall_type->name->mt = jl_type_type_mt;
    jl_uniontype_type->name->mt = jl_type_type_mt;
    jl_datatype_type->name->mt = jl_type_type_mt;
    jl_intrinsic_type->name->mt = (jl_methtable_t*)jl_read_value(&s);
    jl_sym_type->name->mt = (jl_methtable_t*)jl_read_value(&s);
    jl_array_typename->mt = (jl_methtable_t*)jl_read_value(&s);
    jl_module_type->name->mt = (jl_methtable_t*)jl_read_value(&s);

    uintptr_t i;
    for (i = 0; i < builtin_typenames.len; i++) {
        jl_typename_t *tn = (jl_typename_t*)builtin_typenames.items[i];
        tn->cache = (jl_svec_t*)jl_read_value(&s);
        jl_gc_wb(tn, tn->cache);
        tn->linearcache = (jl_svec_t*)jl_read_value(&s);
        jl_gc_wb(tn, tn->linearcache);
        jl_resort_type_cache(tn->cache);
    }

    jl_core_module = (jl_module_t*)jl_get_global(jl_main_module, jl_symbol("Core"));
    jl_base_module = (jl_module_t*)jl_get_global(jl_main_module, jl_symbol("Base"));
    ptls->current_module = jl_base_module; // run start_image in Base

    uint32_t uid_ctr = read_uint32(f);
    uint32_t gs_ctr = read_uint32(f);
    jl_world_counter = read_uint32(f);

    jl_set_t_uid_ctr(uid_ctr);
    jl_set_gs_ctr(gs_ctr);

    jl_get_builtins();
    jl_get_builtin_hooks();
    jl_init_box_caches();

    jl_update_all_gvars(&s);
    ios_close(&gvar_record);
    jl_update_all_fptrs(&s);
    ios_close(&fptr_record);

    jl_gc_reset_alloc_count();
    jl_gc_enable(en);
    jl_cleanup_serializer2();
}

// TODO: need to enforce that the alignment of the buffer is suitable for vectors
JL_DLLEXPORT void jl_restore_system_image(const char *fname)
{
#ifndef JL_NDEBUG
    char *dot = fname ? (char*)strrchr(fname, '.') : NULL;
    int is_ji = (dot && !strcmp(dot, ".ji"));
    assert((is_ji || jl_sysimg_handle) && "System image file not preloaded");
#endif

    if (jl_sysimg_handle) {
        // load the pre-compiled sysimage from jl_sysimg_handle
        jl_load_sysimg_so();
    }
    else {
        ios_t f;
        if (ios_file(&f, fname, 1, 0, 0, 0) == NULL)
            jl_errorf("System image file \"%s\" not found.", fname);
        ios_bufmode(&f, bm_none);
        JL_SIGATOMIC_BEGIN();
        ios_seek_end(&f);
        size_t len = ios_pos(&f);
        char *sysimg = (char*)jl_gc_perm_alloc(len, 0, 64, 0);
        ios_seek(&f, 0);
        if (ios_readall(&f, sysimg, len) != len)
            jl_errorf("Error reading system image file.");
        ios_close(&f);
        ios_static_buffer(&f, sysimg, len);
        jl_restore_system_image_from_stream(&f);
        ios_close(&f);
        JL_SIGATOMIC_END();
    }
}

JL_DLLEXPORT void jl_restore_system_image_data(const char *buf, size_t len)
{
    ios_t f;
    JL_SIGATOMIC_BEGIN();
    ios_static_buffer(&f, (char*)buf, len);
    jl_restore_system_image_from_stream(&f);
    ios_close(&f);
    JL_SIGATOMIC_END();
}

// --- init ---

static void jl_init_serializer2(int for_serialize)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    arraylist_new(&builtin_typenames, 0);
    if (for_serialize) {
        htable_new(&sertag_table, 0);
        htable_new(&symbol_table, 0);
        htable_new(&fptr_to_id, sizeof(id_to_fptrs) / sizeof(*id_to_fptrs));
        htable_new(&backref_table, 0);
    }
    else {
        arraylist_new(&deser_tag, 0);
        arraylist_new(&deser_sym, 0);
    }
    uintptr_t i;

    void *tags[] = { ptls->root_task,
                     jl_symbol_type, jl_ssavalue_type, jl_datatype_type, jl_slotnumber_type,
                     jl_simplevector_type, jl_array_type, jl_typedslot_type,
                     jl_expr_type, jl_globalref_type, jl_string_type,
                     jl_module_type, jl_tvar_type, jl_method_instance_type, jl_method_type,
                     jl_emptysvec, jl_emptytuple, jl_false, jl_true, jl_nothing, jl_any_type,
                     call_sym, invoke_sym, goto_ifnot_sym, return_sym, body_sym, line_sym,
                     unreachable_sym,
                     lambda_sym, jl_symbol("tuple"), assign_sym,
                     jl_labelnode_type, jl_linenumbernode_type,
                     jl_gotonode_type, jl_quotenode_type,
                     jl_pinode_type, jl_phinode_type,
                     jl_type_type, jl_bottom_type, jl_ref_type, jl_pointer_type,
                     jl_vararg_type, jl_abstractarray_type,
                     jl_densearray_type, jl_void_type, jl_function_type, jl_typeofbottom_type,
                     jl_unionall_type, jl_typename_type, jl_builtin_type, jl_code_info_type,
                     jl_task_type, jl_uniontype_type, jl_typetype_type, jl_abstractstring_type,
                     jl_ANY_flag, jl_array_any_type, jl_intrinsic_type, jl_abstractslot_type,
                     jl_methtable_type, jl_typemap_level_type, jl_typemap_entry_type,
                     jl_voidpointer_type, jl_newvarnode_type,
                     jl_array_symbol_type, jl_anytuple_type, jl_tparam0(jl_anytuple_type),
                     jl_emptytuple_type, jl_array_uint8_type,
                     jl_symbol_type->name, jl_ssavalue_type->name, jl_tuple_typename,
                     ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_ref_type))->name,
                     jl_pointer_typename, jl_simplevector_type->name,
                     jl_datatype_type->name, jl_uniontype_type->name, jl_array_typename,
                     jl_expr_type->name, jl_typename_type->name, jl_type_typename,
                     jl_methtable_type->name, jl_typemap_level_type->name, jl_typemap_entry_type->name, jl_tvar_type->name,
                     ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_abstractarray_type))->name,
                     ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_densearray_type))->name,
                     jl_vararg_typename, jl_void_type->name, jl_method_instance_type->name, jl_method_type->name,
                     jl_module_type->name, jl_function_type->name, jl_typedslot_type->name,
                     jl_abstractslot_type->name, jl_slotnumber_type->name,
                     jl_unionall_type->name, jl_intrinsic_type->name, jl_task_type->name,
                     jl_labelnode_type->name, jl_linenumbernode_type->name, jl_builtin_type->name,
                     jl_gotonode_type->name, jl_quotenode_type->name,
                     jl_pinode_type->name, jl_phinode_type->name,
                     jl_globalref_type->name, jl_typeofbottom_type->name,
                     jl_string_type->name, jl_abstractstring_type->name,
                     jl_namedtuple_type, jl_namedtuple_typename,

                     jl_int32_type, jl_int64_type, jl_bool_type, jl_uint8_type,

                     // empirical list of very common symbols
                     #include "common_symbols1.inc"

                     NULL };

    arraylist_push(&deser_tag, NULL);
    arraylist_push(&deser_tag, NULL);
    for (i = 0; tags[i] != NULL; i++) {
        void *v = tags[i];
        if (!for_serialize) {
            // some builtins are only rooted through a type cache or Main.Core binding,
            // but were allocated young, so we force the gc to change their tag here
            jl_gc_force_mark_old(ptls, (jl_value_t*)v);
        }
        if (jl_is_symbol(v)) {
            arraylist_push(&deser_sym, v);
            if (for_serialize)
                ptrhash_put(&symbol_table, v, (void*)((char*)HT_NOTFOUND + ((uintptr_t)SymbolRef << RELOC_TAG_OFFSET) + deser_sym.len));
        }
        else {
            arraylist_push(&deser_tag, v);
            if (for_serialize)
                ptrhash_put(&sertag_table, v, (void*)((char*)HT_NOTFOUND + ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + deser_tag.len));
        }
    }
    assert(i + 1 == sizeof(tags) / sizeof(tags[0]));
    assert(!for_serialize || ptrhash_get(&sertag_table, ptls->root_task) == (char*)HT_NOTFOUND + ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + 3);
    nsym_tag = deser_sym.len;

    // this also ensures all objects referenced in the code have
    // references in the system image to their global variable
    // since codegen knows that some integer boxes are static,
    // they might not have had a reference anywhere in the code
    // image other than here
#define NBOX_C 1024
    for (i = 0; i < NBOX_C; i++) {
        jl_value_t *v32 = jl_box_int32(i - NBOX_C / 2);
        arraylist_push(&deser_tag, v32);
        if (for_serialize)
            ptrhash_put(&sertag_table, v32, (void*)((char*)HT_NOTFOUND + ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + deser_tag.len));

        jl_value_t *v64 = jl_box_int64(i - NBOX_C / 2);
        arraylist_push(&deser_tag, v64);
        if (for_serialize)
            ptrhash_put(&sertag_table, v64, (void*)((char*)HT_NOTFOUND + ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + deser_tag.len));
    }

    if (for_serialize) {
        i = 2;
        while (id_to_fptrs[i] != NULL) {
            ptrhash_put(&fptr_to_id, (void*)(uintptr_t)id_to_fptrs[i], (void*)i);
            i += 1;
        }
    }

    arraylist_push(&builtin_typenames, jl_array_typename);
    arraylist_push(&builtin_typenames, ((jl_datatype_t*)jl_ref_type->body)->name);
    arraylist_push(&builtin_typenames, jl_pointer_typename);
    arraylist_push(&builtin_typenames, jl_type_typename);
    arraylist_push(&builtin_typenames, ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_abstractarray_type))->name);
    arraylist_push(&builtin_typenames, ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_densearray_type))->name);
    arraylist_push(&builtin_typenames, jl_tuple_typename);
    arraylist_push(&builtin_typenames, jl_vararg_typename);
    arraylist_push(&builtin_typenames, jl_namedtuple_typename);
}

static void jl_cleanup_serializer2(void)
{
    htable_reset(&sertag_table, 0);
    htable_reset(&symbol_table, 0);
    htable_reset(&fptr_to_id, 0);
    htable_reset(&backref_table, 0);
    arraylist_free(&deser_tag);
    arraylist_free(&deser_sym);
    arraylist_free(&builtin_typenames);
    nsym_tag = 0;
}

#ifdef __cplusplus
}
#endif
