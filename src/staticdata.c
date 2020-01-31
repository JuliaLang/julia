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

// An array of references that need to be restored from the sysimg
// This is a manually constructed dual of the gvars array, which would be produced by codegen for Julia code, for C.
static void *const _tags[] = {
         // builtin types
         &jl_any_type, &jl_symbol_type, &jl_ssavalue_type, &jl_datatype_type, &jl_slotnumber_type,
         &jl_simplevector_type, &jl_array_type, &jl_typedslot_type,
         &jl_expr_type, &jl_globalref_type, &jl_string_type,
         &jl_module_type, &jl_tvar_type, &jl_method_instance_type, &jl_method_type, &jl_code_instance_type,
         &jl_linenumbernode_type, &jl_lineinfonode_type,
         &jl_gotonode_type, &jl_quotenode_type,
         &jl_pinode_type, &jl_phinode_type, &jl_phicnode_type, &jl_upsilonnode_type,
         &jl_type_type, &jl_bottom_type, &jl_ref_type, &jl_pointer_type,
         &jl_vararg_type, &jl_abstractarray_type,
         &jl_densearray_type, &jl_nothing_type, &jl_function_type, &jl_typeofbottom_type,
         &jl_unionall_type, &jl_typename_type, &jl_builtin_type, &jl_code_info_type,
         &jl_task_type, &jl_uniontype_type, &jl_typetype_type, &jl_abstractstring_type,
         &jl_array_any_type, &jl_intrinsic_type, &jl_abstractslot_type,
         &jl_methtable_type, &jl_typemap_level_type, &jl_typemap_entry_type,
         &jl_voidpointer_type, &jl_newvarnode_type,
         &jl_anytuple_type_type, &jl_anytuple_type, &jl_namedtuple_type, &jl_emptytuple_type,
         &jl_array_symbol_type, &jl_array_uint8_type, &jl_array_int32_type,
         &jl_int32_type, &jl_int64_type, &jl_bool_type, &jl_uint8_type,
         &jl_uint32_type, &jl_uint64_type, &jl_char_type, &jl_weakref_type,
         &jl_int8_type, &jl_int16_type, &jl_uint16_type,
         &jl_float16_type, &jl_float32_type, &jl_float64_type, &jl_floatingpoint_type,
         &jl_number_type, &jl_signed_type,
         // special typenames
         &jl_tuple_typename, &jl_pointer_typename, &jl_array_typename, &jl_type_typename,
         &jl_vararg_typename, &jl_namedtuple_typename,
         &jl_vecelement_typename,
         // special exceptions
         &jl_errorexception_type, &jl_argumenterror_type, &jl_typeerror_type,
         &jl_methoderror_type, &jl_loaderror_type, &jl_initerror_type,
         &jl_undefvarerror_type, &jl_stackovf_exception, &jl_diverror_exception,
         &jl_interrupt_exception, &jl_boundserror_type, &jl_memory_exception,
         &jl_undefref_exception, &jl_readonlymemory_exception,
#ifdef SEGV_EXCEPTION
         &jl_segv_exception,
#endif
         // other special values
         &jl_emptysvec, &jl_emptytuple, &jl_false, &jl_true, &jl_nothing, &jl_an_empty_string,
         &jl_module_init_order, &jl_core_module, &jl_base_module, &jl_main_module, &jl_top_module,
         &jl_typeinf_func, &jl_type_type_mt, &jl_nonfunction_mt,
         // some Core.Builtin Functions that we want to be able to reference:
         &jl_builtin_throw, &jl_builtin_is, &jl_builtin_typeof, &jl_builtin_sizeof,
         &jl_builtin_issubtype, &jl_builtin_isa, &jl_builtin_typeassert, &jl_builtin__apply,
         &jl_builtin__apply_iterate,
         &jl_builtin_isdefined, &jl_builtin_nfields, &jl_builtin_tuple, &jl_builtin_svec,
         &jl_builtin_getfield, &jl_builtin_setfield, &jl_builtin_fieldtype, &jl_builtin_arrayref,
         &jl_builtin_const_arrayref, &jl_builtin_arrayset, &jl_builtin_arraysize,
         &jl_builtin_apply_type, &jl_builtin_applicable, &jl_builtin_invoke,
         &jl_builtin__expr, &jl_builtin_ifelse,
         NULL };
static jl_value_t **const*const tags = (jl_value_t**const*const)_tags;

// hash of definitions for predefined tagged object
static htable_t symbol_table;
static uintptr_t nsym_tag;
// array of definitions for the predefined tagged object types
// (reverse of symbol_table)
static arraylist_t deser_sym;

// table of all objects that are serialized
static htable_t backref_table;
static int backref_table_numel;
static arraylist_t layout_table;
static arraylist_t builtin_typenames;

// list of (size_t pos, (void *f)(jl_value_t*)) entries
// for the serializer to mark values in need of rework by function f
// during deserialization later
static arraylist_t reinit_list;

// hash of definitions for predefined function pointers
static htable_t fptr_to_id;
// array of definitions for the predefined function pointers
// (reverse of fptr_to_id)
// This is a manually constructed dual of the fvars array, which would be produced by codegen for Julia code, for C.
static const jl_fptr_args_t id_to_fptrs[] = {
    &jl_f_throw, &jl_f_is, &jl_f_typeof, &jl_f_issubtype, &jl_f_isa,
    &jl_f_typeassert, &jl_f__apply, &jl_f__apply_iterate, &jl_f__apply_pure, &jl_f__apply_latest, &jl_f_isdefined,
    &jl_f_tuple, &jl_f_svec, &jl_f_intrinsic_call, &jl_f_invoke_kwsorter,
    &jl_f_getfield, &jl_f_setfield, &jl_f_fieldtype, &jl_f_nfields,
    &jl_f_arrayref, &jl_f_const_arrayref, &jl_f_arrayset, &jl_f_arraysize, &jl_f_apply_type,
    &jl_f_applicable, &jl_f_invoke, &jl_f_sizeof, &jl_f__expr, &jl_f__typevar,
    &jl_f_ifelse,
    NULL };

typedef struct {
    ios_t *s;
    ios_t *const_data;
    ios_t *symbols;
    ios_t *relocs;
    ios_t *gvar_record;
    ios_t *fptr_record;
    arraylist_t relocs_list;
    arraylist_t gctags_list;
    jl_ptls_t ptls;
} jl_serializer_state;

static jl_value_t *jl_idtable_type = NULL;
static jl_typename_t *jl_idtable_typename = NULL;
static jl_value_t *jl_bigint_type = NULL;
static int gmp_limb_size = 0;

enum RefTags {
    DataRef,
    ConstDataRef,
    TagRef,
    SymbolRef,
    BindingRef,
    FunctionRef,
    BuiltinFunctionRef
};

// calling conventions for internal entry points.
// this is used to set the method-instance->invoke field
typedef enum {
    JL_API_NULL,
    JL_API_BOXED,
    JL_API_CONST,
    JL_API_WITH_PARAMETERS,
    JL_API_INTERPRETED,
    JL_API_BUILTIN,
    JL_API_MAX
} jl_callingconv_t;


// this supports up to 1 GB images and 16 RefTags
// if a larger size is required, will need to add support for writing larger relocations in many cases below
#define RELOC_TAG_OFFSET 28


/* read and write in host byte order */

#define write_uint8(s, n) ios_putc((n), (s))
#define read_uint8(s) ((uint8_t)ios_getc((s)))

static void write_uint32(ios_t *s, uint32_t i) JL_NOTSAFEPOINT
{
    ios_write(s, (char*)&i, 4);
}

static uint32_t read_uint32(ios_t *s) JL_NOTSAFEPOINT
{
    uint32_t x = 0;
    ios_read(s, (char*)&x, 4);
    return x;
}


// --- Static Compile ---

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
        jl_dlsym(jl_sysimg_handle, "jl_sysimg_gvars_base", (void **)&sysimg_gvars_base, 1);
        jl_dlsym(jl_sysimg_handle, "jl_sysimg_gvars_offsets", (void **)&sysimg_gvars_offsets, 1);
        sysimg_gvars_offsets += 1;
        assert(sysimg_fptrs.base);
        uintptr_t *tls_getter_slot;
        jl_dlsym(jl_sysimg_handle, "jl_get_ptls_states_slot", (void **)&tls_getter_slot, 1);
        *tls_getter_slot = (uintptr_t)jl_get_ptls_states_getter();
        size_t *tls_offset_idx;
        jl_dlsym(jl_sysimg_handle, "jl_tls_offset", (void **)&tls_offset_idx, 1);
        *tls_offset_idx = (uintptr_t)(jl_tls_offset == -1 ? 0 : jl_tls_offset);

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
    const char *sysimg_data;
    jl_dlsym(jl_sysimg_handle, "jl_system_image_data", (void **)&sysimg_data, 1);
    size_t *plen;
    jl_dlsym(jl_sysimg_handle, "jl_system_image_size", (void **)&plen, 1);
    jl_restore_system_image_data(sysimg_data, *plen);
}


// --- serializer ---

static uintptr_t jl_fptr_id(void *fptr)
{
    void **pbp = ptrhash_bp(&fptr_to_id, fptr);
    if (*pbp == HT_NOTFOUND || fptr == NULL)
        return 0;
    else
        return *(uintptr_t*)pbp;
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


#define NBOX_C 1024

static void jl_serialize_value_(jl_serializer_state *s, jl_value_t *v)
{
    // ignore items that are given a special representation
    if (v == NULL || jl_is_symbol(v)) {
        return;
    }
    else if (jl_typeis(v, jl_task_type)) {
        if (v == (jl_value_t*)s->ptls->root_task) {
            jl_serialize_value(s, ((jl_task_t*)v)->tls);
            return;
        }
    }
    else if (jl_typeis(v, jl_int64_type)) {
        int64_t i64 = *(int64_t*)v + NBOX_C / 2;
        if ((uint64_t)i64 < NBOX_C)
            return;
    }
    else if (jl_typeis(v, jl_int32_type)) {
        int32_t i32 = *(int32_t*)v + NBOX_C / 2;
        if ((uint32_t)i32 < NBOX_C)
            return;
    }
    else if (jl_typeis(v, jl_uint8_type)) {
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
        else if (ar->flags.hasptr) {
            const char *data = (const char*)jl_array_data(ar);
            uint16_t elsz = ar->elsize;
            size_t i, l = jl_array_len(ar);
            jl_datatype_t *et = (jl_datatype_t*)jl_tparam0(jl_typeof(ar));
            size_t j, np = et->layout->npointers;
            for (i = 0; i < l; i++) {
                for (j = 0; j < np; j++) {
                    uint32_t ptr = jl_ptr_offset(et, j);
                    jl_value_t *fld = ((jl_value_t**)data)[ptr];
                    JL_GC_PROMISE_ROOTED(fld);
                    jl_serialize_value(s, fld);
                }
                data += elsz;
            }
        }
    }
    else if (jl_typeis(v, jl_module_type)) {
        jl_serialize_module(s, (jl_module_t*)v);
    }
    else if (t->layout->nfields > 0) {
        char *data = (char*)jl_data_ptr(v);
        size_t i, np = t->layout->npointers;
        for (i = 0; i < np; i++) {
            uint32_t ptr = jl_ptr_offset(t, i);
            jl_value_t *fld = ((jl_value_t* const*)data)[ptr];
            jl_serialize_value(s, fld);
        }
    }
}

static void ios_ensureroom(ios_t *s, size_t newsize) JL_NOTSAFEPOINT
{
    size_t prevsize = s->size;
    if (prevsize < newsize) {
        ios_trunc(s, newsize);
        assert(s->size == newsize);
        memset(&s->buf[prevsize], 0, newsize - prevsize);
    }
}

static void record_gvar(jl_serializer_state *s, int gid, uintptr_t reloc_id) JL_NOTSAFEPOINT
{
    if (gid == 0)
        return;
    ios_ensureroom(s->gvar_record, gid * sizeof(uint32_t));
    ios_seek(s->gvar_record, (gid - 1) * sizeof(uint32_t));
    assert(reloc_id < UINT32_MAX);
    write_uint32(s->gvar_record, reloc_id);
}


static void write_padding(ios_t *s, size_t nb) JL_NOTSAFEPOINT
{
    static const char zeros[16] = {0};
    while (nb > 16) {
        ios_write(s, zeros, 16);
        nb -= 16;
    }
    if (nb != 0)
        ios_write(s, zeros, nb);
}


static void write_pointer(ios_t *s) JL_NOTSAFEPOINT
{
    assert((ios_pos(s) & (sizeof(void*) - 1)) == 0 && "stream misaligned for writing a word-sized value");
    write_padding(s, sizeof(void*));
}


#define backref_id(s, v) _backref_id(s, (jl_value_t*)(v))
static uintptr_t _backref_id(jl_serializer_state *s, jl_value_t *v) JL_NOTSAFEPOINT
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
    else if (v == (jl_value_t*)s->ptls->root_task) {
        return (uintptr_t)TagRef << RELOC_TAG_OFFSET;
    }
    else if (jl_typeis(v, jl_int64_type)) {
        int64_t i64 = *(int64_t*)v + NBOX_C / 2;
        if ((uint64_t)i64 < NBOX_C)
            return ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + i64 + 1;
    }
    else if (jl_typeis(v, jl_int32_type)) {
        int32_t i32 = *(int32_t*)v + NBOX_C / 2;
        if ((uint32_t)i32 < NBOX_C)
            return ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + i32 + 1 + NBOX_C;
    }
    else if (jl_typeis(v, jl_uint8_type)) {
        uint8_t u8 = *(uint8_t*)v;
        return ((uintptr_t)TagRef << RELOC_TAG_OFFSET) + u8 + 1 + NBOX_C + NBOX_C;
    }
    if (idx == HT_NOTFOUND) {
        idx = ptrhash_get(&backref_table, v);
        assert(idx != HT_NOTFOUND && "object missed during jl_serialize_value pass");
    }
    return (char*)idx - 1 - (char*)HT_NOTFOUND;
}


static void write_pointerfield(jl_serializer_state *s, jl_value_t *fld) JL_NOTSAFEPOINT
{
    if (fld != NULL) {
        arraylist_push(&s->relocs_list, (void*)(uintptr_t)ios_pos(s->s));
        arraylist_push(&s->relocs_list, (void*)backref_id(s, fld));
    }
    write_pointer(s->s);
}

static void write_gctaggedfield(jl_serializer_state *s, uintptr_t ref) JL_NOTSAFEPOINT
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
            // newm = (jl_module_t*)&s->s->buf[reloc_offset];
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
        JL_GC_PROMISE_ROOTED(v);
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
            jl_value_t *et = jl_tparam0(jl_typeof(v));
            int ndimwords = jl_array_ndimwords(ar->flags.ndims);
            size_t tsz = JL_ARRAY_ALIGN(sizeof(jl_array_t) + ndimwords * sizeof(size_t), JL_CACHE_BYTE_ALIGNMENT);
            // copy header
            ios_write(s->s, (char*)v, tsz);
            // make some header modifications in-place
            jl_array_t *newa = (jl_array_t*)&s->s->buf[reloc_offset];
            size_t alen = jl_array_len(ar);
            size_t tot = alen * ar->elsize;
            if (newa->flags.ndims == 1)
                newa->maxsize = alen;
            newa->offset = 0;
            newa->flags.how = 0;
            newa->flags.pooled = 0;
            newa->flags.isshared = 0;

            // write data
            if (!ar->flags.ptrarray && !ar->flags.hasptr) {
                uintptr_t data = LLT_ALIGN(ios_pos(s->const_data), 16);
                // realign stream to max(data-align(array), sizeof(void*))
                write_padding(s->const_data, data - ios_pos(s->const_data));
                // write data and relocations
                newa->data = NULL; // relocation offset
                data /= sizeof(void*);
                assert(data < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "offset to constant data too large");
                arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_array_t, data))); // relocation location
                arraylist_push(&s->relocs_list, (void*)(((uintptr_t)ConstDataRef << RELOC_TAG_OFFSET) + data)); // relocation target
                if (jl_is_cpointer_type(et)) {
                    // reset Ptr elements to C_NULL
                    size_t i;
                    for (i = 0; i < alen; i++)
                        write_pointer(s->const_data);
                }
                else {
                    int isbitsunion = jl_array_isbitsunion(ar);
                    if (ar->elsize == 1 && !isbitsunion)
                        tot += 1;
                    ios_write(s->const_data, (char*)jl_array_data(ar), tot);
                    if (isbitsunion)
                        ios_write(s->const_data, jl_array_typetagdata(ar), alen);
                }
            }
            else {
                newa->data = (void*)tsz; // relocation offset
                arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_array_t, data))); // relocation location
                arraylist_push(&s->relocs_list, (void*)(((uintptr_t)DataRef << RELOC_TAG_OFFSET) + item)); // relocation target
                if (ar->flags.hasptr) {
                    // copy all of the data first
                    const char *data = (const char*)jl_array_data(ar);
                    ios_write(s->s, data, tot);
                    // the rewrite all of the embedded pointers to null+relocation
                    uint16_t elsz = ar->elsize;
                    size_t j, np = ((jl_datatype_t*)et)->layout->npointers;
                    size_t i;
                    for (i = 0; i < alen; i++) {
                        for (j = 0; j < np; j++) {
                            size_t offset = i * elsz + jl_ptr_offset(((jl_datatype_t*)et), j) * sizeof(jl_value_t*);
                            jl_value_t *fld = *(jl_value_t**)&data[offset];
                            if (fld != NULL) {
                                arraylist_push(&s->relocs_list, (void*)(uintptr_t)(reloc_offset + tsz + offset)); // relocation location
                                arraylist_push(&s->relocs_list, (void*)backref_id(s, fld)); // relocation target
                                memset(&s->s->buf[reloc_offset + tsz + offset], 0, sizeof(fld)); // relocation offset (none)
                            }
                            else {
                                assert(*(jl_value_t**)&s->s->buf[reloc_offset + tsz + offset] == NULL);
                            }
                        }
                    }
                }
                else {
                    size_t i;
                    for (i = 0; i < alen; i++) {
                        jl_value_t *e = jl_array_ptr_ref(v, i);
                        write_pointerfield(s, e);
                    }
                }
            }
        }
        else if (jl_typeis(v, jl_module_type)) {
            jl_write_module(s, item, (jl_module_t*)v);
            // will need to recreate the binding table for this
            arraylist_push(&reinit_list, (void*)item);
            arraylist_push(&reinit_list, (void*)2);
        }
        else if (jl_typeis(v, jl_task_type)) {
            jl_error("Task cannot be serialized");
        }
        else if (jl_is_svec(v)) {
            ios_write(s->s, (char*)v, sizeof(void*));
            size_t i, l = jl_svec_len(v);
            assert(l > 0 || (jl_svec_t*)v == jl_emptysvec);
            for (i = 0; i < l; i++) {
                write_pointerfield(s, jl_svecref(v, i));
            }
        }
        else if (jl_is_string(v)) {
            ios_write(s->s, (char*)v, sizeof(void*) + jl_string_len(v));
            write_uint8(s->s, '\0'); // null-terminated strings for easier C-compatibility
        }
        else if (jl_datatype_nfields(t) == 0) {
            assert(t->layout->npointers == 0);
            if (t->size > 0)
                ios_write(s->s, (char*)v, t->size);
        }
        else if (jl_bigint_type && jl_typeis(v, jl_bigint_type)) {
            jl_value_t *sizefield = jl_get_nth_field(v, 1);
            int32_t sz = jl_unbox_int32(sizefield);
            int32_t nw = (sz == 0 ? 1 : (sz < 0 ? -sz : sz));
            size_t nb = nw * gmp_limb_size;
            ios_write(s->s, (char*)&nw, sizeof(int32_t));
            ios_write(s->s, (char*)&sz, sizeof(int32_t));
            uintptr_t data = LLT_ALIGN(ios_pos(s->const_data), 8);
            write_padding(s->const_data, data - ios_pos(s->const_data));
            data /= sizeof(void*);
            assert(data < ((uintptr_t)1 << RELOC_TAG_OFFSET) && "offset to constant data too large");
            arraylist_push(&s->relocs_list, (void*)(reloc_offset + 8)); // relocation location
            arraylist_push(&s->relocs_list, (void*)(((uintptr_t)ConstDataRef << RELOC_TAG_OFFSET) + data)); // relocation target
            void *pdata = jl_unbox_voidpointer(jl_get_nth_field(v, 2));
            ios_write(s->const_data, (char*)pdata, nb);
            write_pointer(s->s);
        }
        else {
            const char *data = (const char*)v;
            size_t i, nf = jl_datatype_nfields(t);
            size_t tot = 0;
            for (i = 0; i < nf; i++) {
                size_t offset = jl_field_offset(t, i);
                const char *slot = data + offset;
                write_padding(s->s, offset - tot);
                tot = offset;
                size_t fsz = jl_field_size(t, i);
                if (t->mutabl && jl_is_cpointer_type(jl_field_type(t, i))) {
                    // reset Ptr fields to C_NULL
                    assert(!jl_field_isptr(t, i));
                    write_pointer(s->s);
                }
                else if (fsz > 0) {
                    ios_write(s->s, slot, fsz);
                }
                tot += fsz;
            }

            size_t np = t->layout->npointers;
            for (i = 0; i < np; i++) {
                size_t offset = jl_ptr_offset(t, i) * sizeof(jl_value_t*);
                jl_value_t *fld = *(jl_value_t**)&data[offset];
                if (fld != NULL) {
                    arraylist_push(&s->relocs_list, (void*)(uintptr_t)(offset + reloc_offset)); // relocation location
                    arraylist_push(&s->relocs_list, (void*)backref_id(s, fld)); // relocation target
                    memset(&s->s->buf[offset + reloc_offset], 0, sizeof(fld)); // relocation offset (none)
                }
            }

            if (jl_is_method(v)) {
                write_padding(s->s, sizeof(jl_method_t) - tot);
            }
            else if (jl_is_code_instance(v)) {
                jl_code_instance_t *m = (jl_code_instance_t*)v;
                jl_code_instance_t *newm = (jl_code_instance_t*)&s->s->buf[reloc_offset];

                newm->invoke = NULL;
                newm->specptr.fptr = NULL;
                newm->functionObjectsDecls.functionObject = NULL;
                newm->functionObjectsDecls.specFunctionObject = NULL;
                uintptr_t fptr_id = JL_API_NULL;
                uintptr_t specfptr_id = 0;
                if (m->invoke == jl_fptr_const_return) {
                    fptr_id = JL_API_CONST;
                }
                else {
                    if (jl_is_method(m->def->def.method)) {
                        specfptr_id = jl_fptr_id(m->specptr.fptr);
                        const char *fname = m->functionObjectsDecls.functionObject;
                        if (specfptr_id) { // found in the table of builtins
                            assert(specfptr_id >= 2);
                            fptr_id = JL_API_BUILTIN;
                        }
                        else if (fname) {
                            assert(reloc_offset < INT32_MAX);
                            if (!strcmp(fname, "jl_fptr_args")) {
                                fptr_id = JL_API_BOXED;
                            }
                            else if (!strcmp(fname, "jl_fptr_sparam")) {
                                fptr_id = JL_API_WITH_PARAMETERS;
                            }
                            else {
                                int func = jl_assign_functionID(fname);
                                assert(func > 0);
                                ios_ensureroom(s->fptr_record, func * sizeof(void*));
                                ios_seek(s->fptr_record, (func - 1) * sizeof(void*));
                                write_uint32(s->fptr_record, ~reloc_offset);
#ifdef _P64
                                write_padding(s->fptr_record, 4);
#endif
                            }
                            fname = m->functionObjectsDecls.specFunctionObject;
                            if (fname) {
                                int cfunc = jl_assign_functionID(fname);
                                assert(cfunc > 0);
                                ios_ensureroom(s->fptr_record, cfunc * sizeof(void*));
                                ios_seek(s->fptr_record, (cfunc - 1) * sizeof(void*));
                                write_uint32(s->fptr_record, reloc_offset);
#ifdef _P64
                                write_padding(s->fptr_record, 4);
#endif
                            }
                        }
                    }
                }
                newm->invoke = NULL; // relocation offset
                if (fptr_id != JL_API_NULL) {
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_code_instance_t, invoke))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)FunctionRef << RELOC_TAG_OFFSET) + fptr_id)); // relocation target
                }
                if (specfptr_id >= 2) {
                    arraylist_push(&s->relocs_list, (void*)(reloc_offset + offsetof(jl_code_instance_t, specptr.fptr))); // relocation location
                    arraylist_push(&s->relocs_list, (void*)(((uintptr_t)BuiltinFunctionRef << RELOC_TAG_OFFSET) + specfptr_id - 2)); // relocation target
                }
            }
            else if (jl_is_datatype(v)) {
                jl_datatype_t *dt = (jl_datatype_t*)v;
                jl_datatype_t *newdt = (jl_datatype_t*)&s->s->buf[reloc_offset];
                newdt->struct_decl = NULL;
                newdt->ditype = NULL;
                if (dt->layout != NULL) {
                    size_t nf = dt->layout->nfields;
                    size_t np = dt->layout->npointers;
                    size_t fieldsize = jl_fielddesc_size(dt->layout->fielddesc_type);
                    char *flddesc = (char*)dt->layout;
                    size_t fldsize = sizeof(jl_datatype_layout_t) + nf * fieldsize;
                    if (dt->layout->first_ptr != -1)
                        fldsize += np << dt->layout->fielddesc_type;
                    uintptr_t layout = LLT_ALIGN(ios_pos(s->const_data), sizeof(void*));
                    write_padding(s->const_data, layout - ios_pos(s->const_data)); // realign stream
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

static void jl_write_gv_int(jl_serializer_state *s, jl_value_t *v)
{
    int32_t gv = jl_get_llvm_gv((jl_value_t*)v);
    if (gv != 0) {
        uintptr_t item = backref_id(s, v);
        assert(item >> RELOC_TAG_OFFSET == TagRef);
        record_gvar(s, gv, item);
    }
}
static void jl_write_gv_ints(jl_serializer_state *s)
{
    // this also ensures all objects referenced in the code have
    // references in the system image to their global variable
    // since codegen knows that some integer boxes are static,
    // they might not have had a reference anywhere in the code
    // image other than here
    size_t i;
    for (i = 0; i < NBOX_C; i++) {
        jl_write_gv_int(s, jl_box_int32((int32_t)i - NBOX_C / 2));
        jl_write_gv_int(s, jl_box_int64((int64_t)i - NBOX_C / 2));
    }
    for (i = 0; i < 256; i++) {
        jl_write_gv_int(s, jl_box_uint8(i));
    }
}

static inline uint32_t load_uint32(uintptr_t *base)
{
    uint32_t v = jl_load_unaligned_i32((void*)*base);
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
        case SymbolRef:
            assert(offset < nsym_tag && "corrupt relocation item id");
            break;
        case TagRef:
            assert(offset < 2 * NBOX_C + 257 && "corrupt relocation item id");
            break;
        case BindingRef:
            assert(offset == 0 && "corrupt relocation offset");
            break;
        case BuiltinFunctionRef:
            assert(offset < sizeof(id_to_fptrs) / sizeof(*id_to_fptrs) && "unknown function pointer id");
            break;
        case FunctionRef:
            assert(offset < JL_API_MAX && "unknown function pointer id");
            break;
        case DataRef:
        default:
            assert(0 && "corrupt relocation item id");
            abort();
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
        assert(offset <= size);
        return base + offset;
    case ConstDataRef:
        return (uintptr_t)s->const_data->buf + (offset * sizeof(void*));
    case SymbolRef:
        assert(offset < deser_sym.len && deser_sym.items[offset] && "corrupt relocation item id");
        return (uintptr_t)deser_sym.items[offset];
    case BindingRef:
        return jl_buff_tag | GC_OLD_MARKED;
    case TagRef:
        if (offset == 0)
            return (uintptr_t)s->ptls->root_task;
        offset -= 1;
        if (offset < NBOX_C)
            return (uintptr_t)jl_box_int64((int64_t)offset - NBOX_C / 2);
        offset -= NBOX_C;
        if (offset < NBOX_C)
            return (uintptr_t)jl_box_int32((int32_t)offset - NBOX_C / 2);
        offset -= NBOX_C;
        if (offset < 256)
            return (uintptr_t)jl_box_uint8(offset);
        // offset -= 256;
        assert(0 && "corrupt relocation item id");
    case BuiltinFunctionRef:
        assert(offset < sizeof(id_to_fptrs) / sizeof(*id_to_fptrs) && "unknown function pointer ID");
        return (uintptr_t)id_to_fptrs[offset];
    case FunctionRef:
        switch ((jl_callingconv_t)offset) {
        case JL_API_BOXED:
            if (sysimg_fptrs.base)
                return (uintptr_t)jl_fptr_args;
            JL_FALLTHROUGH;
        case JL_API_WITH_PARAMETERS:
            if (sysimg_fptrs.base)
                return (uintptr_t)jl_fptr_sparam;
            return (uintptr_t)NULL;
        case JL_API_CONST:
            return (uintptr_t)jl_fptr_const_return;
        case JL_API_INTERPRETED:
            return (uintptr_t)jl_fptr_interpret_call;
        case JL_API_BUILTIN:
            return (uintptr_t)jl_fptr_args;
        case JL_API_NULL:
        case JL_API_MAX:
        //default:
            assert("corrupt relocation item id");
        }
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
    if (fvars.base == NULL)
        return;
    int sysimg_fvars_max = s->fptr_record->size / sizeof(void*);
    size_t i;
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    jl_method_instance_t **linfos = (jl_method_instance_t**)&s->fptr_record->buf[0];
    uint32_t clone_idx = 0;
    for (i = 0; i < sysimg_fvars_max; i++) {
        uintptr_t val = (uintptr_t)&linfos[i];
        uint32_t offset = load_uint32(&val);
        linfos[i] = NULL;
        if (offset != 0) {
            int specfunc = 1;
            if (offset & ((uintptr_t)1 << (8 * sizeof(uint32_t) - 1))) {
                // if high bit is set, this is the func wrapper, not the specfunc
                specfunc = 0;
                offset = ~offset;
            }
            jl_code_instance_t *codeinst = (jl_code_instance_t*)(base + offset);
            uintptr_t base = (uintptr_t)fvars.base;
            assert(jl_is_method(codeinst->def->def.method) && codeinst->invoke != jl_fptr_const_return);
            assert(specfunc ? codeinst->invoke != NULL : codeinst->invoke == NULL);
            linfos[i] = codeinst->def;
            int32_t offset = fvars.offsets[i];
            for (; clone_idx < fvars.nclones; clone_idx++) {
                uint32_t idx = fvars.clone_idxs[clone_idx] & jl_sysimg_val_mask;
                if (idx < i)
                    continue;
                if (idx == i)
                    offset = fvars.clone_offsets[clone_idx];
                break;
            }
            void *fptr = (void*)(base + offset);
            if (specfunc)
                codeinst->specptr.fptr = fptr;
            else
                codeinst->invoke = (jl_callptr_t)fptr;
            jl_fptr_to_llvm(fptr, codeinst, specfunc);
        }
    }
    jl_register_fptrs(sysimage_base, &fvars, linfos, sysimg_fvars_max);
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


static void jl_reinit_item(jl_value_t *v, int how)
{
    switch (how) {
        case 1: { // rehash IdDict
            jl_array_t **a = (jl_array_t**)v;
            assert(jl_is_array(*a));
            // Assume *a don't need a write barrier
            *a = jl_idtable_rehash(*a, jl_array_len(*a));
            jl_gc_wb(v, *a);
            break;
        }
        case 2: { // rebuild the binding table for module v
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
        default:
            assert(0 && "corrupt deserialization state");
            abort();
    }
}


static void jl_finalize_deserializer(jl_serializer_state *s)
{
    // run reinitialization functions
    uintptr_t base = (uintptr_t)&s->s->buf[0];
    while (1) {
        size_t offset = read_uint32(s->s);
        if (offset == 0)
            break;
        jl_value_t *v = (jl_value_t*)(base + offset);
        jl_reinit_item(v, read_uint32(s->s));
    }
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
    jl_gc_collect(JL_GC_FULL);
    jl_gc_collect(JL_GC_INCREMENTAL);   // sweep finalizers
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
    jl_bigint_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("BigInt")) : NULL;
    if (jl_bigint_type) {
        gmp_limb_size = jl_unbox_long(jl_get_global((jl_module_t*)jl_get_global(jl_base_module, jl_symbol("GMP")),
                                                    jl_symbol("BITS_PER_LIMB"))) / 8;
    }

    { // step 1: record values (recursively) that need to go in the image
        size_t i;
        for (i = 0; tags[i] != NULL; i++) {
            jl_value_t *tag = *tags[i];
            jl_serialize_value(&s, tag);
        }
        for (i = 0; i < builtin_typenames.len; i++) {
            jl_typename_t *tn = (jl_typename_t*)builtin_typenames.items[i];
            jl_prune_type_cache(tn->cache);
            jl_prune_type_cache(tn->linearcache);
        }
        for (i = 0; i < builtin_typenames.len; i++) {
            jl_typename_t *tn = (jl_typename_t*)builtin_typenames.items[i];
            jl_serialize_value(&s, tn->cache);
            jl_serialize_value(&s, tn->linearcache);
        }
    }

    { // step 2: build all the sysimg sections
        write_padding(&sysimg, sizeof(uint32_t));
        jl_write_values(&s);
        jl_write_relocations(&s);
        jl_write_gv_syms(&s, jl_get_root_symbol());
        jl_write_gv_ints(&s);
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
        size_t i;
        for (i = 0; tags[i] != NULL; i++) {
            jl_value_t *tag = *tags[i];
            jl_write_value(&s, tag);
        }
        jl_write_value(&s, s.ptls->root_task->tls);
        write_uint32(f, jl_get_t_uid_ctr());
        write_uint32(f, jl_get_gs_ctr());
        write_uint32(f, jl_world_counter);
        write_uint32(f, jl_typeinf_world);
        jl_finalize_serializer(&s);
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
    ios_t *f = (ios_t*)malloc_s(sizeof(ios_t));
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

extern void jl_init_int32_int64_cache(void);
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
        jl_set_sysimg_so(jl_load_dynamic_library(fname, JL_RTLD_LOCAL | JL_RTLD_NOW, 1));
}

// Allow passing in a module handle directly, rather than a path
JL_DLLEXPORT void jl_set_sysimg_so(void *handle)
{
    void* *jl_RTLD_DEFAULT_handle_pointer;
    int symbol_found = jl_dlsym(handle, "jl_RTLD_DEFAULT_handle_pointer", (void **)&jl_RTLD_DEFAULT_handle_pointer, 0);
    if (!symbol_found || (void*)&jl_RTLD_DEFAULT_handle != *jl_RTLD_DEFAULT_handle_pointer)
        jl_error("System image file failed consistency check: maybe opened the wrong version?");
    if (jl_options.cpu_target == NULL)
        jl_options.cpu_target = "native";
    jl_sysimg_handle = handle;
    sysimg_fptrs = jl_init_processor_sysimg(handle);
}

static void jl_restore_system_image_from_stream(ios_t *f)
{
    JL_TIMING(SYSIMG_LOAD);
    int en = jl_gc_enable(0);
    jl_init_serializer2(0);
    ios_t sysimg, const_data, symbols, relocs, gvar_record, fptr_record;
    jl_serializer_state s;
    s.s = NULL;
    s.const_data = &const_data;
    s.symbols = &symbols;
    s.relocs = &relocs;
    s.gvar_record = &gvar_record;
    s.fptr_record = &fptr_record;
    s.ptls = jl_get_ptls_states();
    arraylist_new(&s.relocs_list, 0);
    arraylist_new(&s.gctags_list, 0);

    // step 1: read section map
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

    // step 2: get references to special values
    s.s = f;
    size_t i;
    for (i = 0; tags[i] != NULL; i++) {
        jl_value_t **tag = tags[i];
        *tag = jl_read_value(&s);
    }
    s.ptls->root_task = (jl_task_t*)jl_gc_alloc(s.ptls, sizeof(jl_task_t), jl_task_type);
    memset(s.ptls->root_task, 0, sizeof(jl_task_t));
    s.ptls->root_task->tls = jl_read_value(&s);
    jl_init_int32_int64_cache();
    jl_init_box_caches();

    uint32_t uid_ctr = read_uint32(f);
    uint32_t gs_ctr = read_uint32(f);
    jl_world_counter = read_uint32(f);
    jl_typeinf_world = read_uint32(f);
    jl_set_t_uid_ctr(uid_ctr);
    jl_set_gs_ctr(gs_ctr);
    s.s = NULL;

    // step 3: apply relocations
    assert(!ios_eof(f));
    jl_read_symbols(&s);
    ios_close(&symbols);

    sysimg_base = &sysimg.buf[0];
    sysimg_relocs = &relocs.buf[0];
    jl_gc_set_permalloc_region((void*)sysimg_base, (void*)(sysimg_base + sysimg.size));

    s.s = &sysimg;
    jl_read_relocations(&s, GC_OLD_MARKED); // gctags
    size_t sizeof_tags = ios_pos(&relocs);
    (void)sizeof_tags;
    jl_read_relocations(&s, 0); // general relocs
    ios_close(&relocs);
    ios_close(&const_data);
    jl_update_all_gvars(&s); // gvars relocs
    ios_close(&gvar_record);
    s.s = NULL;

    s.s = f;
    jl_finalize_deserializer(&s);
    s.s = NULL;

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

    s.s = &sysimg;
    jl_init_codegen();
    jl_update_all_fptrs(&s); // fptr relocs and registration
    ios_close(&fptr_record);
    ios_close(&sysimg);
    s.s = NULL;

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
    if (for_serialize) {
        htable_new(&symbol_table, 0);
        htable_new(&fptr_to_id, sizeof(id_to_fptrs) / sizeof(*id_to_fptrs));
        htable_new(&backref_table, 0);
        arraylist_new(&builtin_typenames, 0);
        uintptr_t i;
        for (i = 0; id_to_fptrs[i] != NULL; i++) {
            ptrhash_put(&fptr_to_id, (void*)(uintptr_t)id_to_fptrs[i], (void*)(i + 2));
        }
    }
    else {
        arraylist_new(&deser_sym, 0);
    }
    nsym_tag = 0;
}

static void jl_cleanup_serializer2(void)
{
    htable_reset(&symbol_table, 0);
    htable_reset(&fptr_to_id, 0);
    htable_reset(&backref_table, 0);
    arraylist_free(&deser_sym);
    arraylist_free(&builtin_typenames);
}

#ifdef __cplusplus
}
#endif
