// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  saving and restoring system images
*/
#include <stdlib.h>
#include <string.h>

#include "julia.h"
#include "julia_internal.h"
#include "builtin_proto.h"

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
static htable_t ser_tag;
// array of definitions for the predefined tagged object types
// (reverse of ser_tag)
static jl_value_t *deser_tag[256];
// hash of some common symbols, encoded as CommonSym_tag plus 1 byte
static htable_t common_symbol_tag;
static jl_value_t *deser_symbols[256];

// table of all objects that have been deserialized, indexed by pos
// (the order in the serializer stream) in MODE_MODULE, the low
// bit is reserved for flagging certain entries and pos is
// left shift by 1
// (not used in MODE_AST)
static htable_t backref_table;
static int backref_table_numel;
static arraylist_t backref_list;

// list of (jl_value_t **loc, size_t pos) entries
// for anything that was flagged by the deserializer for later
// type-rewriting of some sort
// (not used in MODE_AST)
static arraylist_t flagref_list;

// list of (size_t pos, (void *f)(jl_value_t*)) entries
// for the serializer to mark values in need of rework by function f
// during deserialization later
// (not used in MODE_AST)
static arraylist_t reinit_list;

// list of stuff that is being serialized
// (only used by the incremental serializer in MODE_MODULE)
static jl_array_t *serializer_worklist;

// inverse of backedges tree
// (only used by the incremental serializer in MODE_MODULE)
htable_t edges_map;

// list of modules being deserialized with __init__ methods
// (not used in MODE_AST)
extern jl_array_t *jl_module_init_order;

static const intptr_t LongSymbol_tag   = 23;
static const intptr_t LongSvec_tag     = 24;
static const intptr_t LongExpr_tag     = 25;
static const intptr_t LiteralVal_tag   = 26;
static const intptr_t SmallInt64_tag   = 27;
static const intptr_t SmallDataType_tag= 28;
static const intptr_t Int32_tag        = 29;
static const intptr_t Array1d_tag      = 30;
static const intptr_t Singleton_tag    = 31;
static const intptr_t CommonSym_tag    = 32;
static const intptr_t NearbyGlobal_tag = 33;  // a GlobalRef pointing to tree_enclosing_module
static const intptr_t CoreMod_tag      = 34;
static const intptr_t BaseMod_tag      = 35;
static const intptr_t BITypeName_tag   = 36;  // builtin TypeName
static const intptr_t Null_tag         = 253;
static const intptr_t ShortBackRef_tag = 254;
static const intptr_t BackRef_tag      = 255;

static intptr_t VALUE_TAGS;

typedef enum _DUMP_MODES {
    // not in the serializer at all, or
    // something is seriously wrong
    MODE_INVALID = 0,

    // jl_uncompress_ast
    // compressing / decompressing an AST Expr in a MethodInstance
    MODE_AST,

    // jl_restore_new_module
    // restoring a single module from disk for integration
    // into the currently running system image / environment
    MODE_MODULE
} DUMP_MODES;

typedef struct {
    ios_t *s;
    DUMP_MODES mode;
    // pointers to non-AST-ish objects in a compressed tree
    // (only used in MODE_AST)
    jl_array_t *tree_literal_values;
    jl_module_t *tree_enclosing_module;
    jl_ptls_t ptls;
    jl_array_t *loaded_modules_array;
} jl_serializer_state;

static jl_value_t *jl_idtable_type = NULL;
static jl_typename_t *jl_idtable_typename = NULL;
static arraylist_t builtin_typenames;

// mark symbols for gen_sysimg_symtab.jl
//#define GEN_SYMTAB_MODE

#define write_uint8(s, n) ios_putc((n), (s))
#define read_uint8(s) ((uint8_t)ios_getc(s))
#define write_int8(s, n) write_uint8(s, n)
#define read_int8(s) read_uint8(s)

/* read and write in host byte order */

static void write_int32(ios_t *s, int32_t i)
{
    ios_write(s, (char*)&i, 4);
}

static int32_t read_int32(ios_t *s)
{
    int32_t x = 0;
    ios_read(s, (char*)&x, 4);
    return x;
}

static void write_uint64(ios_t *s, uint64_t i)
{
    ios_write(s, (char*)&i, 8);
}

static uint64_t read_uint64(ios_t *s)
{
    uint64_t x = 0;
    ios_read(s, (char*)&x, 8);
    return x;
}

static void write_int64(ios_t *s, int64_t i)
{
    ios_write(s, (char*)&i, 8);
}

static void write_uint16(ios_t *s, uint16_t i)
{
    ios_write(s, (char*)&i, 2);
}

static uint16_t read_uint16(ios_t *s)
{
    int16_t x = 0;
    ios_read(s, (char*)&x, 2);
    return x;
}

static void writetag(ios_t *s, void *v)
{
    write_uint8(s, (uint8_t)(intptr_t)ptrhash_get(&ser_tag, v));
}

static void write_as_tag(ios_t *s, uint8_t tag)
{
    if (tag < VALUE_TAGS) {
        write_uint8(s, 0);
    }
    write_uint8(s, tag);
}

static void write_float64(ios_t *s, double x)
{
    write_uint64(s, *((uint64_t*)&x));
}

// --- Static Compile ---

#define jl_serialize_value(s, v) jl_serialize_value_((s), (jl_value_t*)(v), 0)
static void jl_serialize_value_(jl_serializer_state *s, jl_value_t *v, int as_literal);
static jl_value_t *jl_deserialize_value(jl_serializer_state *s, jl_value_t **loc);

// --- serialize ---

static int module_in_worklist(jl_module_t *mod)
{
    int i, l = jl_array_len(serializer_worklist);
    for (i = 0; i < l; i++) {
        jl_module_t *workmod = (jl_module_t*)jl_array_ptr_ref(serializer_worklist, i);
        if (jl_is_module(workmod) && jl_is_submodule(mod, workmod))
            return 1;
    }
    return 0;
}

// compute whether a type references something internal to worklist
// and thus could not have existed before deserialize
// and thus does not need delayed unique-ing
static int type_in_worklist(jl_datatype_t *dt)
{
    if (module_in_worklist(dt->name->module))
        return 1;
    int i, l = jl_svec_len(dt->parameters);
    for (i = 0; i < l; i++) {
        jl_value_t *p = jl_unwrap_unionall(jl_tparam(dt, i));
        if (type_in_worklist((jl_datatype_t*)(jl_is_datatype(p) ? p : jl_typeof(p))))
            return 1;
    }
    return 0;
}

static int type_recursively_external(jl_datatype_t *dt);

static int type_parameter_recursively_external(jl_value_t *p0)
{
    jl_datatype_t *p = (jl_datatype_t*)p0;
    while (jl_is_unionall(p)) {
        if (!type_parameter_recursively_external(((jl_unionall_t*)p)->var->lb))
            return 0;
        if (!type_parameter_recursively_external(((jl_unionall_t*)p)->var->ub))
            return 0;
        p = (jl_datatype_t*)((jl_unionall_t*)p)->body;
    }
    if (!jl_is_datatype(p) || p->uid == 0)
        return 0;
    if (module_in_worklist(p->name->module))
        return 0;
    if (p->name->wrapper != (jl_value_t*)p0) {
        if (!type_recursively_external(p))
            return 0;
    }
    return 1;
}

// returns true if all of the parameters are tag 6 or 7
static int type_recursively_external(jl_datatype_t *dt)
{
    if (dt->uid == 0)
        return 0;
    if (jl_svec_len(dt->parameters) == 0)
        return 1;

    int i, l = jl_svec_len(dt->parameters);
    for (i = 0; i < l; i++) {
        if (!type_parameter_recursively_external(jl_tparam(dt, i)))
            return 0;
    }
    return 1;
}


static void jl_serialize_datatype(jl_serializer_state *s, jl_datatype_t *dt)
{
    int tag = 0;
    int internal = module_in_worklist(dt->name->module);
    if (!internal && jl_unwrap_unionall(dt->name->wrapper) == (jl_value_t*)dt) {
        tag = 6; // external primary type
    }
    else if (dt->uid == 0) {
        tag = 0; // normal struct
    }
    else if (internal) {
        if (jl_unwrap_unionall(dt->name->wrapper) == (jl_value_t*)dt) // comes up often since functions create types
            tag = 5; // internal, and not in the typename cache (just needs uid reassigned)
        else
            tag = 10; // anything else that's internal (just needs uid reassigned and possibly recaching)
    }
    else if (type_recursively_external(dt)) {
        tag = 7; // external type that can be immediately recreated (with apply_type)
    }
    else if (type_in_worklist(dt)) {
        tag = 10; // external, but definitely new (still needs uid and caching, but not full unique-ing)
    }
    else {
        // this'll need a uid and unique-ing later
        // flag this in the backref table as special
        uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, dt);
        assert(*bp != (uintptr_t)HT_NOTFOUND);
        *bp |= 1;
        tag = 10;
    }

    if (strncmp(jl_symbol_name(dt->name->name), "#kw#", 4) == 0) {
        /* XXX: yuck, but the auto-generated kw types from the serializer isn't a real type, so we *must* be very careful */
        assert(tag == 0 || tag == 5 || tag == 6 || tag == 10);
        if (tag == 6) {
            jl_methtable_t *mt = dt->name->mt;
            jl_datatype_t *primarydt = (jl_datatype_t*)jl_unwrap_unionall(jl_get_global(mt->module, mt->name));
            assert(jl_is_datatype(primarydt));
            assert(jl_typeof(primarydt->name->mt->kwsorter) == (jl_value_t*)dt);
            dt = primarydt;
            tag = 9;
        }
    }

    writetag(s->s, (jl_value_t*)SmallDataType_tag);
    write_uint8(s->s, 0); // virtual size
    jl_serialize_value(s, (jl_value_t*)jl_datatype_type);
    write_uint8(s->s, tag);
    if (tag == 6) {
        jl_serialize_value(s, dt->name);
        return;
    }
    if (tag == 7) {
        jl_serialize_value(s, dt->name);
        jl_serialize_value(s, dt->parameters);
        return;
    }
    if (tag == 9) {
        jl_serialize_value(s, dt);
        return;
    }

    write_int32(s->s, dt->size);
    int has_instance = (dt->instance != NULL);
    int has_layout = (dt->layout != NULL);
    write_uint8(s->s, dt->abstract | (dt->mutabl << 1) | (has_layout << 2) | (has_instance << 3));
    write_uint8(s->s, dt->hasfreetypevars
            | (dt->isconcretetype << 1)
            | (dt->isdispatchtuple << 2)
            | (dt->isbitstype << 3)
            | (dt->zeroinit << 4)
            | (dt->isinlinealloc << 5));
    if (!dt->abstract) {
        write_uint16(s->s, dt->ninitialized);
    }

    if (has_layout) {
        uint8_t layout = 0;
        if (dt->layout == ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_array_type))->layout) {
            layout = 1;
        }
        else if (dt->layout == jl_void_type->layout) {
            layout = 2;
        }
        else if (dt->layout == ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_pointer_type))->layout) {
            layout = 3;
        }
        write_uint8(s->s, layout);
        if (layout == 0) {
            uint32_t nf = dt->layout->nfields;
            write_int32(s->s, nf);
            uint32_t alignment = ((uint32_t*)dt->layout)[1];
            write_int32(s->s, alignment);
            if (dt->layout->npointers && nf)
                write_int32(s->s, ((uint32_t*)dt->layout)[-1]);
            size_t fieldsize = jl_fielddesc_size(dt->layout->fielddesc_type);
            ios_write(s->s, (char*)(&dt->layout[1]), nf * fieldsize);
        }
    }

    if (has_instance)
        jl_serialize_value(s, dt->instance);
    jl_serialize_value(s, dt->name);
    jl_serialize_value(s, dt->names);
    jl_serialize_value(s, dt->parameters);
    jl_serialize_value(s, dt->super);
    jl_serialize_value(s, dt->types);
}

static void jl_serialize_module(jl_serializer_state *s, jl_module_t *m)
{
    writetag(s->s, jl_module_type);
    jl_serialize_value(s, m->name);
    size_t i;
    if (!module_in_worklist(m)) {
        if (m == m->parent) {
            // top-level module
            write_int8(s->s, 2);
            int j = 0;
            for (i = 0; i < jl_array_len(s->loaded_modules_array); i++) {
                jl_module_t *mi = (jl_module_t*)jl_array_ptr_ref(s->loaded_modules_array, i);
                if (!module_in_worklist(mi)) {
                    if (m == mi) {
                        write_int32(s->s, j);
                        return;
                    }
                    j++;
                }
            }
            assert(0 && "top level module not found in modules array");
        }
        else {
            write_int8(s->s, 1);
            jl_serialize_value(s, m->parent);
        }
        return;
    }
    write_int8(s->s, 0);
    jl_serialize_value(s, m->parent);
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m || m != jl_main_module) {
                jl_serialize_value(s, b->name);
                jl_serialize_value(s, b->value);
                jl_serialize_value(s, b->globalref);
                jl_serialize_value(s, b->owner);
                write_int8(s->s, (b->deprecated<<3) | (b->constp<<2) | (b->exportp<<1) | (b->imported));
            }
        }
    }
    jl_serialize_value(s, NULL);
    if (m == jl_main_module) {
        write_int32(s->s, 1);
        jl_serialize_value(s, (jl_value_t*)jl_core_module);
    }
    else {
        write_int32(s->s, m->usings.len);
        for(i=0; i < m->usings.len; i++) {
            jl_serialize_value(s, (jl_value_t*)m->usings.items[i]);
        }
    }
    write_uint8(s->s, m->istopmod);
    write_uint64(s->s, m->uuid.hi);
    write_uint64(s->s, m->uuid.lo);
    write_uint64(s->s, m->build_id);
    write_int32(s->s, m->counter);
}

static int is_ast_node(jl_value_t *v)
{
    // TODO: this accidentally copies QuoteNode(Expr(...)) and QuoteNode(svec(...))
    return jl_is_symbol(v) || jl_is_slot(v) || jl_is_ssavalue(v) ||
        jl_is_uniontype(v) || jl_is_expr(v) || jl_is_newvarnode(v) ||
        jl_is_svec(v) || jl_is_tuple(v) || ((jl_datatype_t*)jl_typeof(v))->instance ||
        jl_is_int32(v) || jl_is_int64(v) || jl_is_bool(v) ||
        jl_is_quotenode(v) || jl_is_gotonode(v) ||
        jl_is_labelnode(v) || jl_is_linenode(v) || jl_is_globalref(v);
}

static int literal_val_id(jl_serializer_state *s, jl_value_t *v)
{
    int i, l = jl_array_len(s->tree_literal_values);
    for (i = 0; i < l; i++) {
        if (jl_egal(jl_array_ptr_ref(s->tree_literal_values, i), v))
            return i;
    }
    jl_array_ptr_1d_push(s->tree_literal_values, v);
    return jl_array_len(s->tree_literal_values) - 1;
}

static void jl_serialize_value_(jl_serializer_state *s, jl_value_t *v, int as_literal)
{
    if (v == NULL) {
        write_uint8(s->s, Null_tag);
        return;
    }

    void **bp = ptrhash_bp(&ser_tag, v);
    if (*bp != HT_NOTFOUND) {
        write_as_tag(s->s, (uint8_t)(intptr_t)*bp);
        return;
    }
    if (jl_is_symbol(v)) {
        void *idx = ptrhash_get(&common_symbol_tag, v);
        if (idx != HT_NOTFOUND) {
            writetag(s->s, (jl_value_t*)CommonSym_tag);
            write_uint8(s->s, (uint8_t)(size_t)idx);
            return;
        }
    }

    if (s->mode == MODE_AST) {
        // compressing tree
        if (v == (jl_value_t*)jl_core_module) {
            writetag(s->s, (jl_value_t*)CoreMod_tag);
            return;
        }
        else if (v == (jl_value_t*)jl_base_module) {
            writetag(s->s, (jl_value_t*)BaseMod_tag);
            return;
        }
        else if (!as_literal && !is_ast_node(v)) {
            writetag(s->s, (jl_value_t*)LiteralVal_tag);
            int id = literal_val_id(s, v);
            assert(id >= 0 && id < UINT16_MAX);
            write_uint16(s->s, id);
            return;
        }
    }
    else {
        bp = ptrhash_bp(&backref_table, v);
        if (*bp != HT_NOTFOUND) {
            uintptr_t pos = (char*)*bp - (char*)HT_NOTFOUND - 1;
            if (pos < 65536) {
                write_uint8(s->s, ShortBackRef_tag);
                write_uint16(s->s, pos);
            }
            else {
                write_uint8(s->s, BackRef_tag);
                write_int32(s->s, pos);
            }
            return;
        }
        intptr_t pos = backref_table_numel++;
        if (((jl_datatype_t*)(jl_typeof(v)))->name == jl_idtable_typename) {
            // will need to rehash this, later (after types are fully constructed)
            arraylist_push(&reinit_list, (void*)pos);
            arraylist_push(&reinit_list, (void*)1);
        }
        if (jl_is_module(v)) {
            jl_module_t *m = (jl_module_t*)v;
            if (module_in_worklist(m) && !module_in_worklist(m->parent)) {
                // will need to reinsert this into parent bindings, later (in case of any errors during reinsert)
                arraylist_push(&reinit_list, (void*)pos);
                arraylist_push(&reinit_list, (void*)2);
            }
        }
        // TypeMapLevels need to be rehashed
        if (jl_is_mtable(v)) {
            arraylist_push(&reinit_list, (void*)pos);
            arraylist_push(&reinit_list, (void*)3);
        }
        if (jl_is_method(v) && jl_typeof(((jl_method_t*)v)->specializations.unknown) == (jl_value_t*)jl_typemap_level_type) {
            arraylist_push(&reinit_list, (void*)pos);
            arraylist_push(&reinit_list, (void*)4);
        }
        pos <<= 1;
        ptrhash_put(&backref_table, v, (char*)HT_NOTFOUND + pos + 1);
    }

    size_t i;
    if (jl_is_svec(v)) {
        size_t l = jl_svec_len(v);
        if (l <= 255) {
            writetag(s->s, jl_simplevector_type);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            writetag(s->s, (jl_value_t*)LongSvec_tag);
            write_int32(s->s, l);
        }
        for(i=0; i < l; i++) {
            jl_serialize_value(s, jl_svecref(v, i));
        }
    }
    else if (jl_is_symbol(v)) {
        size_t l = strlen(jl_symbol_name((jl_sym_t*)v));
        if (l <= 255) {
            writetag(s->s, jl_symbol_type);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            writetag(s->s, (jl_value_t*)LongSymbol_tag);
            write_int32(s->s, l);
        }
#ifdef GEN_SYMTAB_MODE
        write_uint8(s->s, 0);
        ios_write(s->s, "JJJ", 3);
#endif
        ios_write(s->s, jl_symbol_name((jl_sym_t*)v), l);
#ifdef GEN_SYMTAB_MODE
        write_uint8(s->s, 0);
#endif
    }
    else if (jl_is_globalref(v)) {
        if (s->mode == MODE_AST && jl_globalref_mod(v) == s->tree_enclosing_module) {
            writetag(s->s, (jl_value_t*)NearbyGlobal_tag);
            jl_serialize_value(s, jl_globalref_name(v));
        }
        else {
            writetag(s->s, (jl_value_t*)jl_globalref_type);
            jl_serialize_value(s, jl_globalref_mod(v));
            jl_serialize_value(s, jl_globalref_name(v));
        }
    }
    else if (jl_is_ssavalue(v) && ((jl_ssavalue_t*)v)->id < 65536) {
        writetag(s->s, (jl_value_t*)jl_ssavalue_type);
        write_uint16(s->s, ((jl_ssavalue_t*)v)->id);
    }
    else if (jl_typeis(v,jl_slotnumber_type) && jl_slot_number(v) < 65536) {
        writetag(s->s, (jl_value_t*)jl_slotnumber_type);
        write_uint16(s->s, jl_slot_number(v));
    }
    else if (jl_is_array(v)) {
        jl_array_t *ar = (jl_array_t*)v;
        if (ar->flags.ndims == 1 && ar->elsize < 128) {
            writetag(s->s, (jl_value_t*)Array1d_tag);
            write_uint8(s->s, (ar->flags.ptrarray<<7) | (ar->elsize & 0x7f));
        }
        else {
            writetag(s->s, (jl_value_t*)jl_array_type);
            write_uint16(s->s, ar->flags.ndims);
            write_uint16(s->s, (ar->flags.ptrarray<<15) | (ar->elsize & 0x7fff));
        }
        for (i=0; i < ar->flags.ndims; i++)
            jl_serialize_value(s, jl_box_long(jl_array_dim(ar,i)));
        jl_serialize_value(s, jl_typeof(ar));
        if (!ar->flags.ptrarray) {
            size_t extra = jl_is_uniontype(jl_tparam0(jl_typeof(ar))) ? jl_array_len(ar) : 0;
            size_t tot = jl_array_len(ar) * ar->elsize + extra;
            ios_write(s->s, (char*)jl_array_data(ar), tot);
        }
        else {
            for(i=0; i < jl_array_len(ar); i++) {
                jl_serialize_value(s, jl_array_ptr_ref(v, i));
            }
        }
    }
    else if (jl_is_expr(v)) {
        jl_expr_t *e = (jl_expr_t*)v;
        size_t l = jl_array_len(e->args);
        if (l <= 255) {
            writetag(s->s, jl_expr_type);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            writetag(s->s, (jl_value_t*)LongExpr_tag);
            write_int32(s->s, l);
        }
        jl_serialize_value(s, e->head);
        jl_serialize_value(s, e->etype);
        for (i = 0; i < l; i++) {
            jl_serialize_value(s, jl_exprarg(e, i));
        }
    }
    else if (jl_is_datatype(v)) {
        jl_serialize_datatype(s, (jl_datatype_t*)v);
    }
    else if (jl_is_typevar(v)) {
        writetag(s->s, jl_tvar_type);
        jl_serialize_value(s, ((jl_tvar_t*)v)->name);
        jl_serialize_value(s, ((jl_tvar_t*)v)->lb);
        jl_serialize_value(s, ((jl_tvar_t*)v)->ub);
    }
    else if (jl_is_method(v)) {
        writetag(s->s, jl_method_type);
        jl_method_t *m = (jl_method_t*)v;
        int internal = 1;
        int external_mt = 0;
        internal = module_in_worklist(m->module);
        if (!internal) {
            // flag this in the backref table as special
            uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, v);
            assert(*bp != (uintptr_t)HT_NOTFOUND);
            *bp |= 1;
        }
        jl_serialize_value(s, (jl_value_t*)m->sig);
        write_uint8(s->s, internal);
        if (!internal)
            return;
        jl_datatype_t *gf = jl_first_argument_datatype((jl_value_t*)m->sig);
        assert(jl_is_datatype(gf) && gf->name->mt);
        external_mt = !module_in_worklist(gf->name->mt->module);
        union jl_typemap_t *tf = &m->specializations;
        jl_serialize_value(s, tf->unknown);
        jl_serialize_value(s, (jl_value_t*)m->name);
        jl_serialize_value(s, (jl_value_t*)m->file);
        write_int32(s->s, m->line);
        if (external_mt)
            jl_serialize_value(s, jl_nothing);
        else
            jl_serialize_value(s, (jl_value_t*)m->ambig);
        write_int8(s->s, m->called);
        write_int32(s->s, m->nargs);
        write_int8(s->s, m->isva);
        write_int8(s->s, m->pure);
        jl_serialize_value(s, (jl_value_t*)m->module);
        jl_serialize_value(s, (jl_value_t*)m->sparam_syms);
        jl_serialize_value(s, (jl_value_t*)m->roots);
        jl_serialize_value(s, (jl_value_t*)m->source);
        jl_serialize_value(s, (jl_value_t*)m->unspecialized);
        jl_serialize_value(s, (jl_value_t*)m->generator);
        jl_serialize_value(s, (jl_value_t*)m->invokes.unknown);
    }
    else if (jl_is_method_instance(v)) {
        writetag(s->s, jl_method_instance_type);
        jl_method_instance_t *li = (jl_method_instance_t*)v;
        int internal = 0;
        if (li->max_world == 0 && li->min_world == 0) {
            internal = 1; // not world-tracked
        }
        else if (!jl_is_method(li->def.method) || module_in_worklist(li->def.method->module)) {
            if (li->max_world == ~(size_t)0) {
                internal = 2; // update world on deserialization
            }
            else {
                internal = 3; // garbage object :(
            }
        }
        if (!internal) {
            // also flag this in the backref table as special
            uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, v);
            assert(*bp != (uintptr_t)HT_NOTFOUND);
            *bp |= 1;
        }
        jl_serialize_value(s, (jl_value_t*)li->specTypes);
        if (!internal)
            jl_serialize_value(s, (jl_value_t*)li->def.method->sig);
        else
            jl_serialize_value(s, li->def.value);
        write_uint8(s->s, internal);
        if (!internal)
            return;
        jl_serialize_value(s, li->inferred);
        jl_serialize_value(s, li->inferred_const);
        jl_serialize_value(s, li->rettype);
        jl_serialize_value(s, (jl_value_t*)li->sparam_vals);
        jl_array_t *backedges = li->backedges;
        if (s->mode == MODE_MODULE && backedges) {
            // filter backedges to only contain pointers
            // to items that we will actually store (internal == 2)
            size_t ins, i, l = jl_array_len(backedges);
            jl_method_instance_t **b_edges = (jl_method_instance_t**)jl_array_data(backedges);
            for (ins = i = 0; i < l; i++) {
                jl_method_instance_t *backedge = b_edges[i];
                if (module_in_worklist(backedge->def.method->module)) {
                    b_edges[ins++] = backedge;
                }
            }
            if (ins != l)
                jl_array_del_end(backedges, l - ins);
            if (ins == 0)
                backedges = NULL;
        }
        jl_serialize_value(s, (jl_value_t*)backedges);
        write_uint8(s->s, li->jlcall_api == JL_API_CONST ? JL_API_CONST : 0);
    }
    else if (jl_typeis(v, jl_module_type)) {
        jl_serialize_module(s, (jl_module_t*)v);
    }
    else if (jl_typeis(v, jl_task_type)) {
        jl_error("Task cannot be serialized");
    }
    else if (jl_typeis(v, jl_string_type)) {
        writetag(s->s, jl_string_type);
        write_int32(s->s, jl_string_len(v));
        ios_write(s->s, jl_string_data(v), jl_string_len(v));
    }
    else if (jl_typeis(v, jl_typemap_entry_type)) {
        writetag(s->s, jl_typemap_entry_type);
        size_t n = 0;
        jl_typemap_entry_t *te = (jl_typemap_entry_t*)v;
        while ((jl_value_t*)te != jl_nothing) {
            n++; te = te->next;
        }
        write_int32(s->s, n);
        te = (jl_typemap_entry_t*)v;
        size_t i, nf = jl_datatype_nfields(jl_typemap_entry_type);
        while ((jl_value_t*)te != jl_nothing) {
            for (i = 1; i < nf; i++) {
                if (jl_field_size(jl_typemap_entry_type, i) > 0) {
                    jl_serialize_value(s, jl_get_nth_field((jl_value_t*)te, i));
                    if (!jl_field_isptr(jl_typemap_entry_type, i))
                        write_int8(s->s, 0);
                }
            }
            te = te->next;
        }
    }
    else {
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
        void *data = jl_data_ptr(v);
        if (t == jl_int64_type &&
            *(int64_t*)data >= S32_MIN && *(int64_t*)data <= S32_MAX) {
            writetag(s->s, (jl_value_t*)SmallInt64_tag);
            write_int32(s->s, (int32_t)*(int64_t*)data);
        }
        else if (t == jl_int32_type) {
            writetag(s->s, (jl_value_t*)Int32_tag);
            write_int32(s->s, (int32_t)*(int32_t*)data);
        }
        else {
            if (v == t->instance) {
                if (s->mode == MODE_MODULE && !type_in_worklist(t)) {
                    // also flag this in the backref table as special
                    // if it might not be unique (is external)
                    uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, v);
                    assert(*bp != (uintptr_t)HT_NOTFOUND);
                    *bp |= 1;
                }
                writetag(s->s, (jl_value_t*)Singleton_tag);
                jl_serialize_value(s, t);
                return;
            }
            assert(!t->instance && "detected singleton construction corruption");

            if (t == jl_typename_type) {
                void **bp = ptrhash_bp(&ser_tag, ((jl_typename_t*)t)->wrapper);
                if (*bp != HT_NOTFOUND) {
                    writetag(s->s, (jl_value_t*)BITypeName_tag);
                    write_uint8(s->s, (uint8_t)(intptr_t)*bp);
                    return;
                }
            }
            if (t->size <= 255) {
                writetag(s->s, (jl_value_t*)SmallDataType_tag);
                write_uint8(s->s, t->size);
            }
            else {
                writetag(s->s, (jl_value_t*)jl_datatype_type);
                write_int32(s->s, t->size);
            }
            jl_serialize_value(s, t);
            if (t == jl_typename_type) {
                if (module_in_worklist(((jl_typename_t*)v)->module)) {
                    write_uint8(s->s, 0);
                }
                else {
                    write_uint8(s->s, 1);
                    jl_typename_t *tn = (jl_typename_t*)v;
                    jl_serialize_value(s, tn->module);
                    jl_serialize_value(s, tn->name);
                    return;
                }
            }
            if (t == jl_unionall_type) {
                jl_datatype_t *d = (jl_datatype_t*)jl_unwrap_unionall(v);
                if (jl_is_datatype(d) && d->name->wrapper == v &&
                    !module_in_worklist(d->name->module)) {
                    write_uint8(s->s, 1);
                    jl_serialize_value(s, d->name->module);
                    jl_serialize_value(s, d->name->name);
                    return;
                }
                else {
                    write_uint8(s->s, 0);
                }
            }
            if (t == jl_typemap_level_type) {
                // perform some compression on the typemap levels
                // (which will need to be rehashed during deserialization anyhow)
                jl_typemap_level_t *node = (jl_typemap_level_t*)v;
                assert( // make sure this type has the expected ordering
                    offsetof(jl_typemap_level_t, arg1) == 0 * sizeof(jl_value_t*) &&
                    offsetof(jl_typemap_level_t, targ) == 2 * sizeof(jl_value_t*) &&
                    offsetof(jl_typemap_level_t, linear) == 4 * sizeof(jl_value_t*) &&
                    offsetof(jl_typemap_level_t, any) == 5 * sizeof(jl_value_t*) &&
                    offsetof(jl_typemap_level_t, key) == 6 * sizeof(jl_value_t*) &&
                    sizeof(jl_typemap_level_t) == 7 * sizeof(jl_value_t*));
                jl_serialize_value(s, jl_nothing);
                jl_serialize_value(s, node->arg1.values);
                jl_serialize_value(s, jl_nothing);
                jl_serialize_value(s, node->targ.values);
                jl_serialize_value(s, node->linear);
                jl_serialize_value(s, node->any.unknown);
                jl_serialize_value(s, node->key);
                return;
            }
            size_t nf = jl_datatype_nfields(t);
            if (nf == 0 && jl_datatype_size(t) > 0) {
                if (t->name == jl_pointer_typename && jl_unbox_voidpointer(v) != (void*)-1) {
                    // normalize most pointers to NULL, to help catch memory errors
                    // but permit MAP_FAILED / INVALID_HANDLE to be stored unchanged
                    write_int32(s->s, 0);
#ifdef _P64
                    write_int32(s->s, 0);
#endif
                }
                else {
                    ios_write(s->s, (char*)data, jl_datatype_size(t));
                }
            }
            else {
                size_t i;
                for (i = 0; i < nf; i++) {
                    size_t offs = jl_field_offset(t, i);
                    size_t fsz = jl_field_size(t, i);
                    if (fsz > 0) {
                        jl_serialize_value(s, jl_get_nth_field(v, i));
                        if (!jl_field_isptr(t, i)) {
                            uint8_t sel = 0;
                            if (jl_is_uniontype(jl_field_type(t, i))) {
                                sel = ((uint8_t*)v)[offs + fsz - 1];
                            }
                            write_int8(s->s, sel);
                        }
                    }
                }
            }
        }
    }
}

static void jl_collect_missing_backedges_to_mod(jl_methtable_t *mt)
{
    jl_array_t *backedges = mt->backedges;
    if (backedges) {
        size_t i, l = jl_array_len(backedges);
        for (i = 1; i < l; i += 2) {
            jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(backedges, i);
            if (caller->max_world == ~(size_t)0) {
                jl_value_t *missing_callee = jl_array_ptr_ref(backedges, i - 1);
                jl_array_t **edges = (jl_array_t**)ptrhash_bp(&edges_map, (void*)caller);
                if (*edges == HT_NOTFOUND)
                    *edges = jl_alloc_vec_any(0);
                jl_array_ptr_1d_push(*edges, missing_callee);
            }
        }
    }
}

// the intent of this function is to invert the backedges tree
// for anything that points to a method not part of the worklist
static void collect_backedges(jl_method_instance_t *callee)
{
    jl_array_t *backedges = callee->backedges;
    if (backedges) {
        assert(callee->max_world == ~(size_t)0);
        size_t i, l = jl_array_len(backedges);
        for (i = 0; i < l; i++) {
            jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(backedges, i);
            if (caller->max_world == ~(size_t)0) {
                jl_array_t **edges = (jl_array_t**)ptrhash_bp(&edges_map, caller);
                if (*edges == HT_NOTFOUND)
                    *edges = jl_alloc_vec_any(0);
                jl_array_ptr_1d_push(*edges, (jl_value_t*)callee);
            }
        }
    }
}


static int jl_collect_backedges_to_mod(jl_typemap_entry_t *ml, void *closure)
{
    (void)(jl_array_t*)closure;
    jl_method_instance_t *callee = ml->func.linfo;
    collect_backedges(callee);
    return 1;
}

static int jl_collect_methcache_from_mod(jl_typemap_entry_t *ml, void *closure)
{
    jl_array_t *s = (jl_array_t*)closure;
    jl_method_t *m = ml->func.method;
    if (module_in_worklist(m->module)) {
        jl_array_ptr_1d_push(s, (jl_value_t*)m);
        jl_array_ptr_1d_push(s, (jl_value_t*)ml->simplesig);
    }
    else {
        jl_typemap_visitor(m->specializations, jl_collect_backedges_to_mod, closure);
    }
    return 1;
}

static void jl_collect_methtable_from_mod(jl_array_t *s, jl_typename_t *tn)
{
    jl_typemap_visitor(tn->mt->defs, jl_collect_methcache_from_mod, (void*)s);
}

static void jl_collect_lambdas_from_mod(jl_array_t *s, jl_module_t *m)
{
    if (module_in_worklist(m))
        return;
    size_t i;
    void **table = m->bindings.table;
    for (i = 1; i < m->bindings.size; i += 2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m && b->value && b->constp) {
                jl_value_t *bv = jl_unwrap_unionall(b->value);
                if (jl_is_datatype(bv)) {
                    jl_typename_t *tn = ((jl_datatype_t*)bv)->name;
                    if (tn->module == m && tn->name == b->name && tn->wrapper == b->value) {
                        jl_methtable_t *mt = tn->mt;
                        if (mt != NULL &&
                                (jl_value_t*)mt != jl_nothing &&
                                (mt != jl_type_type_mt || tn == jl_type_typename)) {
                            jl_collect_methtable_from_mod(s, tn);
                            jl_collect_missing_backedges_to_mod(mt);
                        }
                    }
                }
                else if (jl_is_module(b->value)) {
                    jl_module_t *child = (jl_module_t*)b->value;
                    if (child != m && child->parent == m && child->name == b->name) {
                        // this is the original/primary binding for the submodule
                        jl_collect_lambdas_from_mod(s, (jl_module_t*)b->value);
                    }
                }
            }
        }
    }
}

// flatten the backedge map reachable from caller into callees
static void jl_collect_backedges_to(jl_method_instance_t *caller, jl_array_t *direct_callees, arraylist_t *to_restore)
{
    jl_array_t **pcallees = (jl_array_t**)ptrhash_bp(&edges_map, (void*)caller),
                *callees = *pcallees;
    if (callees != HT_NOTFOUND) {
        arraylist_push(to_restore, (void*)callees);
        arraylist_push(to_restore, (void*)pcallees);
        *pcallees = (jl_array_t*) HT_NOTFOUND;
        jl_array_ptr_1d_append(direct_callees, callees);
        size_t i, l = jl_array_len(callees);
        for (i = 0; i < l; i++) {
            jl_value_t *c = jl_array_ptr_ref(callees, i);
            if (jl_is_method_instance(c)) {
                jl_collect_backedges_to((jl_method_instance_t*)c, direct_callees, to_restore);
            }
        }
    }
}

static void jl_collect_backedges(jl_array_t *s)
{
    arraylist_t to_restore;
    arraylist_new(&to_restore, 0);
    size_t i;
    void **table = edges_map.table;
    for (i = 0; i < edges_map.size; i += 2) {
        jl_method_instance_t *caller = (jl_method_instance_t*)table[i];
        jl_array_t *callees = (jl_array_t*)table[i + 1];
        if (callees != HT_NOTFOUND && module_in_worklist(caller->def.method->module)) {
            size_t i, l = jl_array_len(callees); // length may change during iteration
            for (i = 0; i < l; i++) {           // only consider the initial list
                jl_value_t *c = jl_array_ptr_ref(callees, i);
                if (jl_is_method_instance(c)) {
                    jl_collect_backedges_to((jl_method_instance_t*)c, callees, &to_restore);
                }
            }
            jl_array_ptr_1d_push(s, (jl_value_t*)caller);
            jl_array_ptr_1d_push(s, (jl_value_t*)callees);
            while (to_restore.len) {
                void **pp = (void**)arraylist_pop(&to_restore);
                *pp = arraylist_pop(&to_restore);
            }
        }
    }
}

// serialize information about all loaded modules
static void write_mod_list(ios_t *s, jl_array_t *a)
{
    size_t i;
    size_t len = jl_array_len(a);
    for (i = 0; i < len; i++) {
        jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(a, i);
        assert(jl_is_module(m));
        if (!module_in_worklist(m)) {
            const char *modname = jl_symbol_name(m->name);
            size_t l = strlen(modname);
            write_int32(s, l);
            ios_write(s, modname, l);
            write_uint64(s, m->uuid.hi);
            write_uint64(s, m->uuid.lo);
            write_uint64(s, m->build_id);
        }
    }
    write_int32(s, 0);
}

// "magic" string and version header of .ji file
static const int JI_FORMAT_VERSION = 6;
static const char JI_MAGIC[] = "\373jli\r\n\032\n"; // based on PNG signature
static const uint16_t BOM = 0xFEFF; // byte-order marker
static void write_header(ios_t *s)
{
    ios_write(s, JI_MAGIC, strlen(JI_MAGIC));
    write_uint16(s, JI_FORMAT_VERSION);
    ios_write(s, (char *) &BOM, 2);
    write_uint8(s, sizeof(void*));
    ios_write(s, JL_BUILD_UNAME, strlen(JL_BUILD_UNAME)+1);
    ios_write(s, JL_BUILD_ARCH, strlen(JL_BUILD_ARCH)+1);
    ios_write(s, JULIA_VERSION_STRING, strlen(JULIA_VERSION_STRING)+1);
    const char *branch = jl_git_branch(), *commit = jl_git_commit();
    ios_write(s, branch, strlen(branch)+1);
    ios_write(s, commit, strlen(commit)+1);
}

// serialize information about the result of deserializing this file
static void write_work_list(ios_t *s)
{
    int i, l = jl_array_len(serializer_worklist);
    for (i = 0; i < l; i++) {
        jl_module_t *workmod = (jl_module_t*)jl_array_ptr_ref(serializer_worklist, i);
        if (workmod->parent == jl_main_module || workmod->parent == workmod) {
            size_t l = strlen(jl_symbol_name(workmod->name));
            write_int32(s, l);
            ios_write(s, jl_symbol_name(workmod->name), l);
            write_uint64(s, workmod->uuid.hi);
            write_uint64(s, workmod->uuid.lo);
            write_uint64(s, workmod->build_id);
        }
    }
    write_int32(s, 0);
}

static void write_module_path(ios_t *s, jl_module_t *depmod)
{
    if (depmod->parent == jl_main_module || depmod->parent == depmod)
        return;
    const char *mname = jl_symbol_name(depmod->name);
    size_t slen = strlen(mname);
    write_module_path(s, depmod->parent);
    write_int32(s, slen);
    ios_write(s, mname, slen);
}

// serialize the global _require_dependencies array of pathnames that
// are include depenencies
static int64_t write_dependency_list(ios_t *s, jl_array_t **udepsp, jl_array_t *mod_array)
{
    int64_t initial_pos = 0;
    int64_t pos = 0;
    static jl_array_t *deps = NULL;
    if (!deps)
        deps = (jl_array_t*)jl_get_global(jl_base_module, jl_symbol("_require_dependencies"));

    // unique(deps) to eliminate duplicates while preserving order:
    // we preserve order so that the topmost included .jl file comes first
    static jl_value_t *unique_func = NULL;
    if (!unique_func)
        unique_func = jl_get_global(jl_base_module, jl_symbol("unique"));
    jl_value_t *uniqargs[2] = {unique_func, (jl_value_t*)deps};
    size_t last_age = jl_get_ptls_states()->world_age;
    jl_get_ptls_states()->world_age = jl_world_counter;
    jl_array_t *udeps = (*udepsp = deps && unique_func ? (jl_array_t*)jl_apply(uniqargs, 2) : NULL);
    jl_get_ptls_states()->world_age = last_age;

    // write a placeholder for total size so that we can quickly seek past all of the
    // dependencies if we don't need them
    initial_pos = ios_pos(s);
    write_uint64(s, 0);
    if (udeps) {
        size_t i, l = jl_array_len(udeps);
        for (i = 0; i < l; i++) {
            jl_value_t *deptuple = jl_array_ptr_ref(udeps, i);
            jl_value_t *dep = jl_fieldref(deptuple, 1);              // file abspath
            size_t slen = jl_string_len(dep);
            write_int32(s, slen);
            ios_write(s, jl_string_data(dep), slen);
            write_float64(s, jl_unbox_float64(jl_fieldref(deptuple, 2)));  // mtime
            jl_module_t *depmod = (jl_module_t*)jl_fieldref(deptuple, 0);  // evaluating module
            jl_module_t *depmod_top = depmod;
            while (depmod_top->parent != jl_main_module && depmod_top->parent != depmod_top)
                depmod_top = depmod_top->parent;
            unsigned provides = 0;
            size_t j, lj = jl_array_len(serializer_worklist);
            for (j = 0; j < lj; j++) {
                jl_module_t *workmod = (jl_module_t*)jl_array_ptr_ref(serializer_worklist, j);
                if (workmod->parent == jl_main_module || workmod->parent == workmod) {
                    ++provides;
                    if (workmod == depmod_top) {
                        write_int32(s, provides);
                        write_module_path(s, depmod);
                        break;
                    }
                }
            }
            write_int32(s, 0);
        }
        write_int32(s, 0); // terminator, for ease of reading
        // write a dummy file position to indicate the beginning of the source-text
        pos = ios_pos(s);
        ios_seek(s, initial_pos);
        write_uint64(s, pos - initial_pos);
        ios_seek(s, pos);
        write_int64(s, 0);
    }
    return pos;
}

// --- deserialize ---

static jl_value_t *jl_deserialize_datatype(jl_serializer_state *s, int pos, jl_value_t **loc)
{
    int tag = read_uint8(s->s);
    if (tag == 6 || tag == 7) {
        jl_typename_t *name = (jl_typename_t*)jl_deserialize_value(s, NULL);
        jl_value_t *dtv = name->wrapper;
        if (tag == 7) {
            jl_svec_t *parameters = (jl_svec_t*)jl_deserialize_value(s, NULL);
            dtv = jl_apply_type(dtv, jl_svec_data(parameters), jl_svec_len(parameters));
        }
        else {
            dtv = jl_unwrap_unionall(dtv);
        }
        backref_list.items[pos] = dtv;
        return dtv;
    }
    if (tag == 9) {
        jl_datatype_t *primarydt = (jl_datatype_t*)jl_deserialize_value(s, NULL);
        jl_value_t *dtv = jl_typeof(jl_get_kwsorter((jl_value_t*)primarydt));
        backref_list.items[pos] = dtv;
        return dtv;
    }
    size_t size = read_int32(s->s);
    uint8_t flags = read_uint8(s->s);
    uint8_t memflags = read_uint8(s->s);
    jl_datatype_t *dt = NULL;
    if (tag == 0 || tag == 5 || tag == 10)
        dt = jl_new_uninitialized_datatype();
    else {
        assert(0 && "corrupt deserialization state");
        abort();
    }
    assert(s->tree_literal_values==NULL && s->mode != MODE_AST && "no new data-types expected during MODE_AST");
    assert(pos == backref_list.len - 1 && "nothing should have been deserialized since assigning pos");
    backref_list.items[pos] = dt;
    dt->size = size;
    dt->struct_decl = NULL;
    dt->instance = NULL;
    dt->ditype = NULL;
    dt->abstract = flags & 1;
    dt->mutabl = (flags >> 1) & 1;
    int has_layout = (flags >> 2) & 1;
    int has_instance = (flags >> 3) & 1;
    dt->hasfreetypevars = memflags & 1;
    dt->isconcretetype = (memflags >> 1) & 1;
    dt->isdispatchtuple = (memflags >> 2) & 1;
    dt->isbitstype = (memflags >> 3) & 1;
    dt->zeroinit = (memflags >> 4) & 1;
    dt->isinlinealloc = (memflags >> 5) & 1;
    dt->types = NULL;
    dt->parameters = NULL;
    dt->name = NULL;
    dt->super = NULL;
    dt->layout = NULL;
    if (!dt->abstract) {
        dt->ninitialized = read_uint16(s->s);
        dt->uid = 0;
    }
    else {
        dt->ninitialized = 0;
        dt->uid = 0;
    }

    if (has_layout) {
        uint8_t layout = read_uint8(s->s);
        if (layout == 1) {
            dt->layout = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_array_type))->layout;
        }
        else if (layout == 2) {
            dt->layout = jl_void_type->layout;
        }
        else if (layout == 3) {
            dt->layout = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_pointer_type))->layout;
        }
        else {
            assert(layout == 0);
            uint32_t nf = read_int32(s->s);
            uint32_t alignment = read_int32(s->s);
            union {
                struct {
                    uint32_t nf;
                    uint32_t alignment;
                } buffer;
                jl_datatype_layout_t layout;
            } header;
            header.buffer.nf = nf;
            header.buffer.alignment = alignment;
            int has_padding = header.layout.npointers && nf;
            uint8_t fielddesc_type = header.layout.fielddesc_type;
            size_t fielddesc_size = nf > 0 ? jl_fielddesc_size(fielddesc_type) : 0;
            jl_datatype_layout_t *layout = (jl_datatype_layout_t*)jl_gc_perm_alloc(
                    sizeof(jl_datatype_layout_t) + nf * fielddesc_size +
                    (has_padding ? sizeof(uint32_t) : 0), 0, 4, 0);
            if (has_padding) {
                layout = (jl_datatype_layout_t*)(((char*)layout) + sizeof(uint32_t));
                jl_datatype_layout_n_nonptr(layout) = read_int32(s->s);
            }
            *layout = header.layout;
            ios_read(s->s, (char*)&layout[1], nf * fielddesc_size);
            dt->layout = layout;
        }
    }

    if (tag == 5) {
        dt->uid = jl_assign_type_uid();
    }
    else if (tag == 10) {
        assert(pos > 0);
        arraylist_push(&flagref_list, loc == HT_NOTFOUND ? NULL : loc);
        arraylist_push(&flagref_list, (void*)(uintptr_t)pos);
        dt->uid = -1; // mark that this type needs a new uid
    }

    if (has_instance) {
        assert(dt->uid != 0 && "there shouldn't be an instance on a type with uid = 0");
        dt->instance = jl_deserialize_value(s, &dt->instance);
        jl_gc_wb(dt, dt->instance);
    }
    dt->name = (jl_typename_t*)jl_deserialize_value(s, (jl_value_t**)&dt->name);
    jl_gc_wb(dt, dt->name);
    dt->names = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&dt->names);
    jl_gc_wb(dt, dt->names);
    dt->parameters = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&dt->parameters);
    jl_gc_wb(dt, dt->parameters);
    dt->super = (jl_datatype_t*)jl_deserialize_value(s, (jl_value_t**)&dt->super);
    jl_gc_wb(dt, dt->super);
    dt->types = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&dt->types);
    jl_gc_wb(dt, dt->types);

    return (jl_value_t*)dt;
}

static jl_value_t *jl_deserialize_value_(jl_serializer_state *s, jl_value_t *vtag, jl_value_t **loc);
static jl_value_t *jl_deserialize_value(jl_serializer_state *s, jl_value_t **loc)
{
    assert(!ios_eof(s->s));
    uint8_t tag = read_uint8(s->s);
    if (tag == Null_tag)
        return NULL;
    if (tag == 0) {
        tag = read_uint8(s->s);
        jl_value_t *v = deser_tag[tag];
        assert(v != NULL);
        return v;
    }
    if (tag == BackRef_tag || tag == ShortBackRef_tag) {
        assert(s->tree_literal_values == NULL && s->mode != MODE_AST);
        uintptr_t offs = (tag == BackRef_tag) ? read_int32(s->s) : read_uint16(s->s);
        int isflagref = 0;
        isflagref = !!(offs & 1);
        offs >>= 1;
        // assert(offs >= 0); // offs is unsigned so this is always true
        assert(offs < backref_list.len);
        jl_value_t *bp = (jl_value_t*)backref_list.items[offs];
        assert(bp);
        if (isflagref && loc != HT_NOTFOUND) {
            assert(loc != NULL);
            arraylist_push(&flagref_list, loc);
            arraylist_push(&flagref_list, (void*)(uintptr_t)-1);
        }
        return (jl_value_t*)bp;
    }

    jl_value_t *vtag = deser_tag[tag];
    if (tag >= VALUE_TAGS) {
        return vtag;
    }
    else if (vtag == (jl_value_t*)LiteralVal_tag) {
        return jl_array_ptr_ref(s->tree_literal_values, read_uint16(s->s));
    }
    jl_value_t *v = jl_deserialize_value_(s, vtag, loc);
    return v;
}

static jl_value_t *jl_deserialize_value_svec(jl_serializer_state *s, jl_value_t *vtag)
{
    int usetable = (s->mode != MODE_AST);
    size_t i, len;
    if (vtag == (jl_value_t*)jl_simplevector_type)
        len = read_uint8(s->s);
    else
        len = read_int32(s->s);
    jl_svec_t *sv = jl_alloc_svec_uninit(len);
    if (usetable)
        arraylist_push(&backref_list, (jl_value_t*)sv);
    jl_value_t **data = jl_svec_data(sv);
    for (i = 0; i < len; i++) {
        data[i] = jl_deserialize_value(s, &data[i]);
    }
    return (jl_value_t*)sv;
}

static jl_value_t *jl_deserialize_value_symbol(jl_serializer_state *s, jl_value_t *vtag)
{
    int usetable = (s->mode != MODE_AST);
    size_t len;
    if (vtag == (jl_value_t*)jl_symbol_type)
        len = read_uint8(s->s);
    else
        len = read_int32(s->s);
    char *name = (char*)(len >= 256 ? malloc(len + 1) : alloca(len + 1));
#ifdef GEN_SYMTAB_MODE
    (void)read_uint8(s->s); (void)read_uint8(s->s); (void)read_uint8(s->s); (void)read_uint8(s->s);
#endif
    ios_read(s->s, name, len);
#ifdef GEN_SYMTAB_MODE
    (void)read_uint8(s->s);
#endif
    name[len] = '\0';
    jl_value_t *sym = (jl_value_t*)jl_symbol(name);
    if (len >= 256)
        free(name);
    if (usetable)
        arraylist_push(&backref_list, sym);
    return sym;
}

static jl_value_t *jl_deserialize_value_array(jl_serializer_state *s, jl_value_t *vtag)
{
    int usetable = (s->mode != MODE_AST);
    int16_t i, ndims;
    int isunboxed, elsize;
    if (vtag == (jl_value_t*)Array1d_tag) {
        ndims = 1;
        elsize = read_uint8(s->s);
        isunboxed = !(elsize >> 7);
        elsize = elsize & 0x7f;
    }
    else {
        ndims = read_uint16(s->s);
        elsize = read_uint16(s->s);
        isunboxed = !(elsize >> 15);
        elsize = elsize & 0x7fff;
    }
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, NULL);
    size_t *dims = (size_t*)alloca(ndims * sizeof(size_t));
    for (i = 0; i < ndims; i++) {
        dims[i] = jl_unbox_long(jl_deserialize_value(s, NULL));
    }
    jl_array_t *a = jl_new_array_for_deserialization((jl_value_t*)NULL, ndims, dims, isunboxed, elsize);
    if (usetable)
        backref_list.items[pos] = a;
    jl_value_t *aty = jl_deserialize_value(s, &jl_astaggedvalue(a)->type);
    jl_set_typeof(a, aty);
    if (!a->flags.ptrarray) {
        size_t extra = jl_is_uniontype(jl_tparam0(aty)) ? jl_array_len(a) : 0;
        size_t tot = jl_array_len(a) * a->elsize + extra;
        ios_read(s->s, (char*)jl_array_data(a), tot);
    }
    else {
        jl_value_t **data = (jl_value_t**)jl_array_data(a);
        size_t i, numel = jl_array_len(a);
        for (i = 0; i < numel; i++) {
            data[i] = jl_deserialize_value(s, &data[i]);
            if (data[i])
                jl_gc_wb(a, data[i]);
        }
    }
    return (jl_value_t*)a;
}

static jl_value_t *jl_deserialize_value_expr(jl_serializer_state *s, jl_value_t *vtag)
{
    int usetable = (s->mode != MODE_AST);
    size_t i, len;
    if (vtag == (jl_value_t*)jl_expr_type)
        len = read_uint8(s->s);
    else
        len = read_int32(s->s);
    int pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, NULL);
    jl_expr_t *e = jl_exprn((jl_sym_t*)jl_deserialize_value(s, NULL), len);
    if (usetable)
        backref_list.items[pos] = e;
    e->etype = jl_deserialize_value(s, &e->etype);
    jl_gc_wb(e, e->etype);
    jl_value_t **data = (jl_value_t**)(e->args->data);
    for (i = 0; i < len; i++) {
        data[i] = jl_deserialize_value(s, &data[i]);
    }
    return (jl_value_t*)e;
}

static jl_value_t *jl_deserialize_value_method(jl_serializer_state *s, jl_value_t **loc)
{
    int usetable = (s->mode != MODE_AST);
    jl_method_t *m =
        (jl_method_t*)jl_gc_alloc(s->ptls, sizeof(jl_method_t),
                                  jl_method_type);
    memset(m, 0, sizeof(jl_method_type));
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, m);
    m->sig = (jl_value_t*)jl_deserialize_value(s, (jl_value_t**)&m->sig);
    jl_gc_wb(m, m->sig);
    int internal = read_uint8(s->s);
    if (!internal) {
        assert(loc != NULL && loc != HT_NOTFOUND);
        arraylist_push(&flagref_list, loc);
        arraylist_push(&flagref_list, (void*)pos);
        return (jl_value_t*)m;
    }
    m->specializations.unknown = jl_deserialize_value(s, (jl_value_t**)&m->specializations);
    jl_gc_wb(m, m->specializations.unknown);
    m->name = (jl_sym_t*)jl_deserialize_value(s, NULL);
    jl_gc_wb(m, m->name);
    m->file = (jl_sym_t*)jl_deserialize_value(s, NULL);
    m->line = read_int32(s->s);
    m->min_world = jl_world_counter;
    m->ambig = jl_deserialize_value(s, (jl_value_t**)&m->ambig);
    jl_gc_wb(m, m->ambig);
    m->called = read_int8(s->s);
    m->nargs = read_int32(s->s);
    m->isva = read_int8(s->s);
    m->pure = read_int8(s->s);
    m->module = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&m->module);
    jl_gc_wb(m, m->module);
    m->sparam_syms = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&m->sparam_syms);
    jl_gc_wb(m, m->sparam_syms);
    m->roots = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&m->roots);
    if (m->roots)
        jl_gc_wb(m, m->roots);
    m->source = jl_deserialize_value(s, &m->source);
    if (m->source)
        jl_gc_wb(m, m->source);
    m->unspecialized = (jl_method_instance_t*)jl_deserialize_value(s, (jl_value_t**)&m->unspecialized);
    if (m->unspecialized)
        jl_gc_wb(m, m->unspecialized);
    m->generator = jl_deserialize_value(s, (jl_value_t**)&m->generator);
    if (m->generator)
        jl_gc_wb(m, m->generator);
    m->invokes.unknown = jl_deserialize_value(s, (jl_value_t**)&m->invokes);
    jl_gc_wb(m, m->invokes.unknown);
    m->traced = 0;
    JL_MUTEX_INIT(&m->writelock);
    return (jl_value_t*)m;
}

static jl_value_t *jl_deserialize_value_method_instance(jl_serializer_state *s, jl_value_t **loc)
{
    int usetable = (s->mode != MODE_AST);
    jl_method_instance_t *li =
        (jl_method_instance_t*)jl_gc_alloc(s->ptls, sizeof(jl_method_instance_t),
                                       jl_method_instance_type);
    memset(li, 0, sizeof(jl_method_instance_t));
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, li);

    li->specTypes = (jl_value_t*)jl_deserialize_value(s, (jl_value_t**)&li->specTypes);
    if (li->specTypes)
        jl_gc_wb(li, li->specTypes);
    li->def.value = jl_deserialize_value(s, &li->def.value);
    if (li->def.value)
        jl_gc_wb(li, li->def.value);

    int internal = read_uint8(s->s);
    if (!internal) {
        assert(loc != NULL && loc != HT_NOTFOUND);
        arraylist_push(&flagref_list, loc);
        arraylist_push(&flagref_list, (void*)pos);
        return (jl_value_t*)li;
    }

    li->inferred = jl_deserialize_value(s, &li->inferred);
    jl_gc_wb(li, li->inferred);
    li->inferred_const = jl_deserialize_value(s, &li->inferred_const);
    if (li->inferred_const)
        jl_gc_wb(li, li->inferred_const);
    li->rettype = jl_deserialize_value(s, &li->rettype);
    jl_gc_wb(li, li->rettype);
    li->sparam_vals = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&li->sparam_vals);
    jl_gc_wb(li, li->sparam_vals);
    li->backedges = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&li->backedges);
    if (li->backedges)
        jl_gc_wb(li, li->backedges);
    li->unspecialized_ducttape = NULL;
    if (internal == 1) {
        li->min_world = 0;
        li->max_world = 0;
    }
    else if (internal == 2) {
        li->min_world = jl_world_counter;
        li->max_world = ~(size_t)0;
    }
    else if (internal == 3) {
        li->min_world = 1;
        li->max_world = 0;
    }
    else {
        assert(0 && "corrupt deserialization state");
        abort();
    }
    li->functionObjectsDecls.functionObject = NULL;
    li->functionObjectsDecls.specFunctionObject = NULL;
    li->inInference = 0;
    li->fptr = NULL;
    li->jlcall_api = read_int8(s->s);
    li->compile_traced = 0;
    return (jl_value_t*)li;
}

static jl_value_t *jl_deserialize_value_module(jl_serializer_state *s)
{
    int usetable = (s->mode != MODE_AST);
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, NULL);
    jl_sym_t *mname = (jl_sym_t*)jl_deserialize_value(s, NULL);
    int ref_only = read_uint8(s->s);
    if (ref_only) {
        jl_value_t *m_ref;
        if (ref_only == 1)
            m_ref = jl_get_global((jl_module_t*)jl_deserialize_value(s, NULL), mname);
        else
            m_ref = jl_array_ptr_ref(s->loaded_modules_array, read_int32(s->s));
        if (usetable)
            backref_list.items[pos] = m_ref;
        return m_ref;
    }
    jl_module_t *m = jl_new_module(mname);
    if (usetable)
        backref_list.items[pos] = m;
    m->parent = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&m->parent);
    jl_gc_wb(m, m->parent);

    while (1) {
        jl_sym_t *name = (jl_sym_t*)jl_deserialize_value(s, NULL);
        if (name == NULL)
            break;
        jl_binding_t *b = jl_get_binding_wr(m, name, 1);
        b->value = jl_deserialize_value(s, &b->value);
        jl_gc_wb_buf(m, b, sizeof(jl_binding_t));
        if (b->value != NULL) jl_gc_wb(m, b->value);
        b->globalref = jl_deserialize_value(s, &b->globalref);
        if (b->globalref != NULL) jl_gc_wb(m, b->globalref);
        b->owner = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&b->owner);
        if (b->owner != NULL) jl_gc_wb(m, b->owner);
        int8_t flags = read_int8(s->s);
        b->deprecated = (flags>>3) & 1;
        b->constp = (flags>>2) & 1;
        b->exportp = (flags>>1) & 1;
        b->imported = (flags) & 1;
    }
    size_t i = m->usings.len;
    size_t ni = read_int32(s->s);
    arraylist_grow(&m->usings, ni);
    ni += i;
    while (i < ni) {
        m->usings.items[i] = jl_deserialize_value(s, (jl_value_t**)&m->usings.items[i]);
        i++;
    }
    m->istopmod = read_uint8(s->s);
    m->uuid.hi = read_uint64(s->s);
    m->uuid.lo = read_uint64(s->s);
    m->build_id = read_uint64(s->s);
    m->primary_world = jl_world_counter;
    m->counter = read_int32(s->s);
    return (jl_value_t*)m;
}

static jl_value_t *jl_deserialize_value_globalref(jl_serializer_state *s)
{
    int usetable = (s->mode != MODE_AST);
    if (usetable) {
        jl_value_t *v = jl_new_struct_uninit(jl_globalref_type);
        arraylist_push(&backref_list, v);
        jl_value_t **data = jl_data_ptr(v);
        data[0] = jl_deserialize_value(s, &data[0]);
        data[1] = jl_deserialize_value(s, &data[1]);
        return v;
    }
    else {
        jl_value_t *mod = jl_deserialize_value(s, NULL);
        jl_value_t *var = jl_deserialize_value(s, NULL);
        return jl_module_globalref((jl_module_t*)mod, (jl_sym_t*)var);
    }
}

static jl_value_t *jl_deserialize_value_singleton(jl_serializer_state *s, jl_value_t **loc)
{
    if (s->mode == MODE_AST) {
        jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s, NULL);
        return dt->instance;
    }
    jl_value_t *v = (jl_value_t*)jl_gc_alloc(s->ptls, 0, NULL);
    uintptr_t pos = backref_list.len;
    arraylist_push(&backref_list, (void*)v);
    if (s->mode == MODE_MODULE) {
        // TODO: optimize the case where the value can easily be obtained
        // from an external module (tag == 6) as dt->instance
        assert(loc != NULL && loc != HT_NOTFOUND);
        arraylist_push(&flagref_list, loc);
        arraylist_push(&flagref_list, (void*)pos);
    }
    jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s, (jl_value_t**)HT_NOTFOUND); // no loc, since if dt is replaced, then dt->instance would be also
    jl_set_typeof(v, dt);
    return v;
}

static void jl_deserialize_struct(jl_serializer_state *s, jl_value_t *v, size_t startfield)
{
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(v);
    size_t i, nf = jl_datatype_nfields(dt);
    char *data = (char*)jl_data_ptr(v);
    for (i = startfield; i < nf; i++) {
        size_t offs = jl_field_offset(dt, i);
        size_t fsz = jl_field_size(dt, i);
        jl_value_t **fld = (jl_value_t**)(data + offs);
        if (fsz > 0) {
            if (jl_field_isptr(dt, i)) {
                *fld = jl_deserialize_value(s, fld);
            }
            else {
                jl_value_t *fldval = jl_deserialize_value(s, NULL);
                jl_assign_bits((char*)fld, fldval);
                uint8_t union_selector = read_uint8(s->s);
                if (union_selector) {
                    uint8_t *psel = (uint8_t*)fld + fsz - 1;
                    *psel = union_selector - 1;
                }
            }
        }
    }
    if (dt == jl_typename_type) {
        jl_typename_t *tn = (jl_typename_t*)v;
        tn->cache = jl_emptysvec; // the cache is refilled later (tag 5)
        tn->linearcache = jl_emptysvec; // the cache is refilled later (tag 5)
    }
    if (dt == jl_typemap_entry_type) {
        if (((jl_typemap_entry_t*)v)->max_world == ~(size_t)0) {
            // update world validity to reflect current state of the counter
            ((jl_typemap_entry_t*)v)->min_world = jl_world_counter;
        }
        else {
            // garbage entry - delete it :(
            ((jl_typemap_entry_t*)v)->min_world = ((jl_typemap_entry_t*)v)->max_world - 1;
        }
    }
}

static jl_value_t *jl_deserialize_typemap_entry(jl_serializer_state *s)
{
    int N = read_int32(s->s); int n = N;
    jl_value_t *te = jl_nothing;
    jl_value_t **pn = &te;
    while (n > 0) {
        jl_value_t *v = jl_gc_alloc(s->ptls, jl_datatype_size(jl_typemap_entry_type), jl_typemap_entry_type);
        if (n == N && s->mode != MODE_AST)
            arraylist_push(&backref_list, v);
        jl_deserialize_struct(s, v, 1);
        ((jl_typemap_entry_t*)v)->next = (jl_typemap_entry_t*)jl_nothing;
        *pn = v;
        pn = (jl_value_t**)&((jl_typemap_entry_t*)v)->next;
        n--;
    }
    return te;
}

static jl_value_t *jl_deserialize_value_any(jl_serializer_state *s, jl_value_t *vtag, jl_value_t **loc)
{
    int usetable = (s->mode != MODE_AST);
    int32_t sz = (vtag == (jl_value_t*)SmallDataType_tag ? read_uint8(s->s) : read_int32(s->s));
    jl_value_t *v = jl_gc_alloc(s->ptls, sz, NULL);
    jl_set_typeof(v, (void*)(intptr_t)0x50);
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, v);
    jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s, &jl_astaggedvalue(v)->type);
    if (dt == jl_datatype_type) {
        return jl_deserialize_datatype(s, pos, loc);
    }
    assert(s->mode == MODE_AST || sz != 0 || loc);
    if (s->mode == MODE_MODULE && dt == jl_typename_type) {
        int ref_only = read_uint8(s->s);
        if (ref_only) {
            jl_module_t *m = (jl_module_t*)jl_deserialize_value(s, NULL);
            jl_sym_t *sym = (jl_sym_t*)jl_deserialize_value(s, NULL);
            jl_datatype_t *dt = (jl_datatype_t*)jl_unwrap_unionall(jl_get_global(m, sym));
            assert(jl_is_datatype(dt));
            jl_value_t *v = (jl_value_t*)dt->name;
            if (usetable)
                backref_list.items[pos] = v;
            return v;
        }
    }
    if (s->mode == MODE_MODULE && dt == jl_unionall_type) {
        int ref_only = read_uint8(s->s);
        if (ref_only) {
            jl_module_t *m = (jl_module_t*)jl_deserialize_value(s, NULL);
            jl_sym_t *sym = (jl_sym_t*)jl_deserialize_value(s, NULL);
            jl_value_t *v = jl_get_global(m, sym);
            assert(jl_is_unionall(v));
            if (usetable)
                backref_list.items[pos] = v;
            return v;
        }
    }
    jl_set_typeof(v, dt);
    if (jl_datatype_nfields(dt) == 0 && jl_datatype_size(dt) > 0) {
        int nby = jl_datatype_size(dt);
        ios_read(s->s, (char*)jl_data_ptr(v), nby);
    }
    else {
        jl_deserialize_struct(s, v, 0);
    }
    return v;
}

static jl_value_t *jl_deserialize_value_(jl_serializer_state *s, jl_value_t *vtag, jl_value_t **loc)
{
    int usetable = (s->mode != MODE_AST);
    if (vtag == (jl_value_t*)jl_simplevector_type ||
        vtag == (jl_value_t*)LongSvec_tag) {
        return jl_deserialize_value_svec(s, vtag);
    }
    else if (vtag == (jl_value_t*)CommonSym_tag) {
        return deser_symbols[read_uint8(s->s)];
    }
    else if (vtag == (jl_value_t*)jl_symbol_type ||
             vtag == (jl_value_t*)LongSymbol_tag) {
        return jl_deserialize_value_symbol(s, vtag);
    }
    else if (vtag == (jl_value_t*)jl_ssavalue_type) {
        jl_value_t *v = jl_box_ssavalue(read_uint16(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    }
    else if (vtag == (jl_value_t*)jl_slotnumber_type) {
        jl_value_t *v = jl_box_slotnumber(read_uint16(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    }
    else if (vtag == (jl_value_t*)jl_array_type ||
             vtag == (jl_value_t*)Array1d_tag) {
        return jl_deserialize_value_array(s, vtag);
    }
    else if (vtag == (jl_value_t*)jl_expr_type ||
             vtag == (jl_value_t*)LongExpr_tag) {
        return jl_deserialize_value_expr(s, vtag);
    }
    else if (vtag == (jl_value_t*)jl_tvar_type) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_gc_alloc(s->ptls, sizeof(jl_tvar_t), jl_tvar_type);
        if (usetable)
            arraylist_push(&backref_list, tv);
        tv->name = (jl_sym_t*)jl_deserialize_value(s, NULL);
        jl_gc_wb(tv, tv->name);
        tv->lb = jl_deserialize_value(s, &tv->lb);
        jl_gc_wb(tv, tv->lb);
        tv->ub = jl_deserialize_value(s, &tv->ub);
        jl_gc_wb(tv, tv->ub);
        return (jl_value_t*)tv;
    }
    else if (vtag == (jl_value_t*)jl_method_type) {
        return jl_deserialize_value_method(s, loc);
    }
    else if (vtag == (jl_value_t*)jl_method_instance_type) {
        return jl_deserialize_value_method_instance(s, loc);
    }
    else if (vtag == (jl_value_t*)jl_module_type) {
        return jl_deserialize_value_module(s);
    }
    else if (vtag == (jl_value_t*)SmallInt64_tag) {
        jl_value_t *v = jl_box_int64(read_int32(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    }
    else if (vtag == (jl_value_t*)Int32_tag) {
        jl_value_t *v = jl_box_int32(read_int32(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    }
    else if (vtag == (jl_value_t*)NearbyGlobal_tag) {
        assert(s->tree_enclosing_module != NULL);
        jl_value_t *sym = jl_deserialize_value(s, NULL);
        return jl_module_globalref(s->tree_enclosing_module, (jl_sym_t*)sym);
    }
    else if (vtag == (jl_value_t*)jl_globalref_type) {
        return jl_deserialize_value_globalref(s);
    }
    else if (vtag == (jl_value_t*)Singleton_tag) {
        return jl_deserialize_value_singleton(s, loc);
    }
    else if (vtag == (jl_value_t*)CoreMod_tag) {
        return (jl_value_t*)jl_core_module;
    }
    else if (vtag == (jl_value_t*)BaseMod_tag) {
        return (jl_value_t*)jl_base_module;
    }
    else if (vtag == (jl_value_t*)BITypeName_tag) {
        jl_value_t *ty = deser_tag[read_uint8(s->s)];
        jl_datatype_t *dt = (jl_datatype_t*)jl_unwrap_unionall(ty);
        return (jl_value_t*)dt->name;
    }
    else if (vtag == (jl_value_t*)jl_string_type) {
        size_t n = read_int32(s->s);
        jl_value_t *str = jl_alloc_string(n);
        if (usetable)
            arraylist_push(&backref_list, str);
        ios_read(s->s, jl_string_data(str), n);
        return str;
    }
    else if (vtag == (jl_value_t*)jl_typemap_entry_type) {
        return jl_deserialize_typemap_entry(s);
    }
    else {
        assert(vtag == (jl_value_t*)jl_datatype_type || vtag == (jl_value_t*)SmallDataType_tag);
        return jl_deserialize_value_any(s, vtag, loc);
    }
}

static void jl_insert_methods(jl_array_t *list)
{
    size_t i, l = jl_array_len(list);
    for (i = 0; i < l; i += 2) {
        jl_method_t *meth = (jl_method_t*)jl_array_ptr_ref(list, i);
        jl_tupletype_t *simpletype = (jl_tupletype_t*)jl_array_ptr_ref(list, i + 1);
        assert(jl_is_method(meth));
        jl_datatype_t *gf = jl_first_argument_datatype((jl_value_t*)meth->sig);
        assert(jl_is_datatype(gf) && gf->name->mt);
        jl_method_table_insert(gf->name->mt, meth, simpletype);
    }
}

static size_t lowerbound_dependent_world_set(size_t world, arraylist_t *dependent_worlds)
{
    size_t i, l = dependent_worlds->len;
    if (world <= (size_t)dependent_worlds->items[l - 1])
        return world;
    for (i = 0; i < l; i++) {
        size_t depworld = (size_t)dependent_worlds->items[i];
        if (depworld <= world)
            return depworld;
    }
    abort(); // unreachable
}

void jl_method_instance_delete(jl_method_instance_t *mi);
static void jl_insert_backedges(jl_array_t *list, arraylist_t *dependent_worlds)
{
    size_t i, l = jl_array_len(list);
    for (i = 0; i < l; i += 2) {
        jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(list, i);
        assert(jl_is_method_instance(caller));
        assert(caller->min_world == jl_world_counter); // caller should be new
        jl_array_t *callees = (jl_array_t*)jl_array_ptr_ref(list, i + 1);
        assert(jl_is_array(callees));
        int valid = 1;
        size_t j;
        for (j = 0; valid && j < jl_array_len(callees); j++) {
            jl_value_t *callee = jl_array_ptr_ref(callees, j);
            jl_method_instance_t *callee_mi = (jl_method_instance_t*)callee;
            jl_value_t *sig;
            if (jl_is_method_instance(callee)) {
                sig = callee_mi->specTypes;
                assert(!module_in_worklist(callee_mi->def.method->module));
                if (callee_mi->max_world != ~(size_t)0) {
                    valid = 0;
                    break;
                }
            }
            else {
                sig = callee;
            }
            // verify that this backedge doesn't intersect with any new methods
            size_t min_valid = 0;
            size_t max_valid = ~(size_t)0;
            jl_value_t *matches = jl_matching_methods((jl_tupletype_t*)sig, 50, 1, jl_world_counter, &min_valid, &max_valid);
            if (matches == jl_false)
                valid = 0;
            size_t k;
            for (k = 0; valid && k < jl_array_len(matches); k++) {
                jl_method_t *m = (jl_method_t*)jl_svecref(jl_array_ptr_ref(matches, k), 2);
                if (lowerbound_dependent_world_set(m->min_world, dependent_worlds) != m->min_world) {
                    // intersection has a new method and is now probably no good, just invalidate everything now
                    valid = 0;
                }
            }
        }
        if (valid) {
            // if this callee is still valid, add all the backedges
            for (j = 0; j < jl_array_len(callees); j++) {
                jl_value_t *callee = jl_array_ptr_ref(callees, j);
                if (jl_is_method_instance(callee)) {
                    jl_method_instance_add_backedge((jl_method_instance_t*)callee, caller);
                }
                else {
                    jl_datatype_t *ftype = jl_first_argument_datatype(callee);
                    jl_methtable_t *mt = ftype->name->mt;
                    assert(jl_is_datatype(ftype) && mt);
                    jl_method_table_add_backedge(mt, callee, (jl_value_t*)caller);
                }
            }
        }
        else {
            // otherwise delete it
            jl_method_instance_delete(caller);
        }
    }
}


static int size_isgreater(const void *a, const void *b)
{
    return *(size_t*)b - *(size_t*)a;
}

static jl_value_t *read_verify_mod_list(ios_t *s, arraylist_t *dependent_worlds, jl_array_t *mod_list)
{
    if (!jl_main_module->build_id) {
        return jl_get_exceptionf(jl_errorexception_type,
                "Main module uuid state is invalid for module deserialization.");
    }
    size_t i, l = jl_array_len(mod_list);
    for (i = 0; ; i++) {
        size_t len = read_int32(s);
        if (len == 0 && i == l)
            return NULL; // success
        if (len == 0 || i == l)
            return jl_get_exceptionf(jl_errorexception_type, "Wrong number of entries in module list.");
        char *name = (char*)alloca(len + 1);
        ios_read(s, name, len);
        name[len] = '\0';
        jl_uuid_t uuid;
        uuid.hi = read_uint64(s);
        uuid.lo = read_uint64(s);
        uint64_t build_id = read_uint64(s);
        jl_sym_t *sym = jl_symbol_n(name, len);
        jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(mod_list, i);
        if (!m || !jl_is_module(m) || m->uuid.hi != uuid.hi || m->uuid.lo != uuid.lo || m->name != sym || m->build_id != build_id) {
            return jl_get_exceptionf(jl_errorexception_type,
                "Invalid input in module list: expected %s.", name);
        }
        if (m->primary_world > jl_main_module->primary_world)
            arraylist_push(dependent_worlds, (void*)m->primary_world);
    }
}

static int readstr_verify(ios_t *s, const char *str)
{
    size_t i, len = strlen(str);
    for (i = 0; i < len; ++i)
        if ((char)read_uint8(s) != str[i])
            return 0;
    return 1;
}

JL_DLLEXPORT int jl_read_verify_header(ios_t *s)
{
    uint16_t bom;
    return (readstr_verify(s, JI_MAGIC) &&
            read_uint16(s) == JI_FORMAT_VERSION &&
            ios_read(s, (char *) &bom, 2) == 2 && bom == BOM &&
            read_uint8(s) == sizeof(void*) &&
            readstr_verify(s, JL_BUILD_UNAME) && !read_uint8(s) &&
            readstr_verify(s, JL_BUILD_ARCH) && !read_uint8(s) &&
            readstr_verify(s, JULIA_VERSION_STRING) && !read_uint8(s) &&
            readstr_verify(s, jl_git_branch()) && !read_uint8(s) &&
            readstr_verify(s, jl_git_commit()) && !read_uint8(s));
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
    jl_serialize_value(s, jl_module_init_order);

    // record list of reinitialization functions
    l = reinit_list.len;
    for (i = 0; i < l; i += 2) {
        write_int32(s->s, (int)((uintptr_t) reinit_list.items[i]));
        write_int32(s->s, (int)((uintptr_t) reinit_list.items[i+1]));
    }
    write_int32(s->s, -1);
}

void jl_typemap_rehash(union jl_typemap_t ml, int8_t offs);
static void jl_reinit_item(jl_value_t *v, int how, arraylist_t *tracee_list)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_TRY {
        switch (how) {
            case 1: { // rehash IdDict
                jl_array_t **a = (jl_array_t**)v;
                // Assume *a don't need a write barrier
                *a = jl_idtable_rehash(*a, jl_array_len(*a));
                jl_gc_wb(v, *a);
                break;
            }
            case 2: { // reinsert module v into parent (const)
                jl_module_t *mod = (jl_module_t*)v;
                if (mod->parent == mod) // top level modules handled by loader
                    break;
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
                    jl_printf(JL_STDERR, "WARNING: replacing module %s.\n", jl_symbol_name(mod->name));
                }
                b->value = v;
                jl_gc_wb_binding(b, v);
                break;
            }
            case 3: { // rehash MethodTable
                jl_methtable_t *mt = (jl_methtable_t*)v;
                jl_typemap_rehash(mt->defs, 0);
                // TODO: consider reverting this when we can split on Type{...} better
                jl_typemap_rehash(mt->cache, 1); //(mt == jl_type_typename->mt) ? 0 : 1);
                if (tracee_list)
                    arraylist_push(tracee_list, mt);
                break;
            }
            case 4: { // rehash specializations tfunc
                jl_method_t *m = (jl_method_t*)v;
                jl_typemap_rehash(m->specializations, 0);
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
    jl_array_t *init_order = (jl_array_t*)jl_deserialize_value(s, NULL);

    // run reinitialization functions
    int pos = read_int32(s->s);
    while (pos != -1) {
        jl_reinit_item((jl_value_t*)backref_list.items[pos], read_int32(s->s), tracee_list);
        pos = read_int32(s->s);
    }
    return init_order;
}

static void jl_init_restored_modules(jl_array_t *init_order)
{
    if (!init_order)
        return;
    int i, l = jl_array_len(init_order);
    for (i = 0; i < l; i++) {
        jl_value_t *mod = jl_array_ptr_ref(init_order, i);
        if (!jl_generating_output() || jl_options.incremental) {
            jl_module_run_initializer((jl_module_t*)mod);
        }
        else {
            if (jl_module_init_order == NULL)
                jl_module_init_order = jl_alloc_vec_any(0);
            jl_array_ptr_1d_push(jl_module_init_order, mod);
        }
    }
}


// --- entry points ---

JL_DLLEXPORT jl_array_t *jl_compress_ast(jl_method_t *m, jl_code_info_t *code)
{
    JL_TIMING(AST_COMPRESS);
    JL_LOCK(&m->writelock); // protect the roots array (Might GC)
    assert(jl_is_method(m));
    assert(jl_is_code_info(code));
    ios_t dest;
    ios_mem(&dest, 0);
    int en = jl_gc_enable(0); // Might GC
    size_t i;

    if (m->roots == NULL) {
        m->roots = jl_alloc_vec_any(0);
        jl_gc_wb(m, m->roots);
    }
    jl_serializer_state s = {
        &dest, MODE_AST,
        m->roots, m->module,
        jl_get_ptls_states(),
        NULL
    };

    uint8_t flags = (code->inferred << 3)
                  | (code->inlineable << 2)
                  | (code->propagate_inbounds << 1)
                  | (code->pure << 0);
    write_uint8(s.s, flags);

    size_t nsyms = jl_array_len(code->slotnames);
    assert(nsyms >= m->nargs && nsyms < INT32_MAX); // required by generated functions
    write_int32(s.s, nsyms);
    for (i = 0; i < nsyms; i++) {
        jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(code->slotnames, i);
        assert(jl_is_symbol(name));
        char *namestr = jl_symbol_name(name);
        size_t namelen = strlen(namestr);
        ios_write(s.s, namestr, namelen + 1); // include nul-byte
    }

    size_t nf = jl_datatype_nfields(jl_code_info_type);
    for (i = 0; i < nf - 5; i++) {
        jl_serialize_value_(&s, jl_get_nth_field((jl_value_t*)code, i), 1);
    }

    ios_putc('\0', s.s);
    ios_flush(s.s);
    jl_array_t *v = jl_take_buffer(&dest);
    ios_close(s.s);
    if (jl_array_len(m->roots) == 0) {
        m->roots = NULL;
    }
    JL_GC_PUSH1(&v);
    jl_gc_enable(en);
    JL_UNLOCK(&m->writelock); // Might GC
    JL_GC_POP();
    return v;
}

JL_DLLEXPORT jl_code_info_t *jl_uncompress_ast(jl_method_t *m, jl_array_t *data)
{
    if (jl_is_code_info(data))
        return (jl_code_info_t*)data;
    JL_TIMING(AST_UNCOMPRESS);
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_LOCK(&m->writelock); // protect the roots array (Might GC)
    assert(jl_is_method(m));
    assert(jl_typeis(data, jl_array_uint8_type));
    assert(jl_array_len(data) > 2 && ((uint8_t*)data->data)[jl_array_len(data) - 1] == 0);
    size_t i;
    ios_t src;
    ios_mem(&src, 0);
    ios_setbuf(&src, (char*)data->data, jl_array_len(data), 0);
    src.size = jl_array_len(data);
    int en = jl_gc_enable(0); // Might GC
    jl_serializer_state s = {
        &src, MODE_AST,
        m->roots, m->module,
        jl_get_ptls_states(),
        NULL
    };

    jl_code_info_t *code =
        (jl_code_info_t*)jl_gc_alloc(ptls, sizeof(jl_code_info_t),
                                       jl_code_info_type);
    uint8_t flags = read_uint8(s.s);
    code->inferred = !!(flags & (1 << 3));
    code->inlineable = !!(flags & (1 << 2));
    code->propagate_inbounds = !!(flags & (1 << 1));
    code->pure = !!(flags & (1 << 0));

    size_t nslots = read_int32(&src);
    jl_array_t *syms = jl_alloc_vec_any(nslots);
    code->slotnames = syms;
    for (i = 0; i < nslots; i++) {
        char *namestr = s.s->buf + s.s->bpos;
        size_t namelen = strlen(namestr);
        jl_sym_t *name = jl_symbol_n(namestr, namelen);
        jl_array_ptr_set(syms, i, name);
        ios_skip(s.s, namelen + 1);
    }

    size_t nf = jl_datatype_nfields(jl_code_info_type);
    for (i = 0; i < nf - 5; i++) {
        assert(jl_field_isptr(jl_code_info_type, i));
        jl_value_t **fld = (jl_value_t**)((char*)jl_data_ptr(code) + jl_field_offset(jl_code_info_type, i));
        *fld = jl_deserialize_value(&s, fld);
    }

    assert(ios_getc(s.s) == '\0' && ios_getc(s.s) == -1);
    ios_close(s.s);
    JL_GC_PUSH1(&code);
    jl_gc_enable(en);
    JL_UNLOCK(&m->writelock); // Might GC
    JL_GC_POP();
    return code;
}

JL_DLLEXPORT uint8_t jl_ast_flag_inferred(jl_array_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->inferred;
    assert(jl_typeis(data, jl_array_uint8_type));
    assert(jl_array_len(data) > 2 && ((uint8_t*)data->data)[jl_array_len(data) - 1] == 0);
    uint8_t flags = ((uint8_t*)data->data)[0];
    return !!(flags & (1 << 3));
}

JL_DLLEXPORT uint8_t jl_ast_flag_inlineable(jl_array_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->inlineable;
    assert(jl_typeis(data, jl_array_uint8_type));
    assert(jl_array_len(data) > 2 && ((uint8_t*)data->data)[jl_array_len(data) - 1] == 0);
    uint8_t flags = ((uint8_t*)data->data)[0];
    return !!(flags & (1 << 2));
}

JL_DLLEXPORT uint8_t jl_ast_flag_pure(jl_array_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->pure;
    assert(jl_typeis(data, jl_array_uint8_type));
    assert(jl_array_len(data) > 2 && ((uint8_t*)data->data)[jl_array_len(data) - 1] == 0);
    uint8_t flags = ((uint8_t*)data->data)[0];
    return !!(flags & (1 << 0));
}

JL_DLLEXPORT void jl_fill_argnames(jl_array_t *data, jl_array_t *names)
{
    size_t i, nargs = jl_array_len(names);
    if (jl_is_code_info(data)) {
        jl_code_info_t *func = (jl_code_info_t*)data;
        assert(jl_array_len(func->slotnames) >= nargs);
        for (i = 0; i < nargs; i++) {
            jl_value_t *name = jl_array_ptr_ref(func->slotnames, i);
            jl_array_ptr_set(names, i, name);
        }
    }
    else {
        uint8_t *d = (uint8_t*)data->data;
        assert(jl_typeis(data, jl_array_uint8_type));
        int b3 = d[1];
        int b2 = d[2];
        int b1 = d[3];
        int b0 = d[4];
        int nslots = b0 | (b1<<8) | (b2<<16) | (b3<<24);
        assert(nslots >= nargs);
        (void)nslots;
        char *namestr = (char*)d + 5;
        for (i = 0; i < nargs; i++) {
            size_t namelen = strlen(namestr);
            jl_sym_t *name = jl_symbol_n(namestr, namelen);
            jl_array_ptr_set(names, i, name);
            namestr += namelen + 1;
        }
    }
}

JL_DLLEXPORT int jl_save_incremental(const char *fname, jl_array_t *worklist)
{
    char *tmpfname = strcat(strcpy((char *) alloca(strlen(fname)+8), fname), ".XXXXXX");
    ios_t f;
    jl_array_t *mod_array = NULL, *udeps = NULL;
    if (ios_mkstemp(&f, tmpfname) == NULL) {
        jl_printf(JL_STDERR, "Cannot open cache file \"%s\" for writing.\n", tmpfname);
        return 1;
    }
    JL_GC_PUSH2(&mod_array, &udeps);
    mod_array = jl_get_loaded_modules();

    serializer_worklist = worklist;
    write_header(&f);
    // write description on contents
    write_work_list(&f);
    // write binary blob from caller
    int64_t srctextpos = write_dependency_list(&f, &udeps, mod_array);
    // write description of requirements for loading
    // this can return errors during deserialize,
    // best to keep it early (before any actual initialization)
    write_mod_list(&f, mod_array);

    arraylist_new(&reinit_list, 0);
    htable_new(&edges_map, 0);
    htable_new(&backref_table, 5000);
    ptrhash_put(&backref_table, jl_main_module, (char*)HT_NOTFOUND + 1);
    backref_table_numel = 1;
    jl_idtable_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("IdDict")) : NULL;
    jl_idtable_typename = jl_base_module ? ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_idtable_type))->name : NULL;

    int en = jl_gc_enable(0); // edges map is not gc-safe
    jl_array_t *lambdas = jl_alloc_vec_any(0);
    jl_array_t *edges = jl_alloc_vec_any(0);

    size_t i;
    size_t len = jl_array_len(mod_array);
    for (i = 0; i < len; i++) {
        jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(mod_array, i);
        assert(jl_is_module(m));
        jl_collect_lambdas_from_mod(lambdas, m);
    }

    jl_collect_backedges(edges);

    jl_serializer_state s = {
        &f, MODE_MODULE,
        NULL, NULL,
        jl_get_ptls_states(),
        mod_array
    };
    jl_serialize_value(&s, worklist);
    jl_serialize_value(&s, lambdas);
    jl_serialize_value(&s, edges);
    jl_finalize_serializer(&s);
    serializer_worklist = NULL;

    jl_gc_enable(en);
    htable_reset(&edges_map, 0);
    htable_reset(&backref_table, 0);
    arraylist_free(&reinit_list);

    // Write the source-text for the dependent files
    if (udeps) {
        // Go back and update the source-text position to point to the current position
        int64_t posfile = ios_pos(&f);
        ios_seek(&f, srctextpos);
        write_int64(&f, posfile);
        ios_seek_end(&f);
        // Each source-text file is written as
        //   int32: length of abspath
        //   char*: abspath
        //   uint64: length of src text
        //   char*: src text
        // At the end we write int32(0) as a terminal sentinel.
        len = jl_array_len(udeps);
        ios_t srctext;
        for (i = 0; i < len; i++) {
            jl_value_t *deptuple = jl_array_ptr_ref(udeps, i);
            jl_value_t *depmod = jl_fieldref(deptuple, 0);  // module
            // Dependencies declared with `include_dependency` are excluded
            // because these may not be Julia code (and could be huge)
            if (depmod != (jl_value_t*)jl_main_module) {
                jl_value_t *dep = jl_fieldref(deptuple, 1);  // file abspath
                const char *depstr = jl_string_data(dep);
                if (!depstr[0])
                    continue;
                ios_t *srctp = ios_file(&srctext, depstr, 1, 0, 0, 0);
                if (!srctp) {
                    jl_printf(JL_STDERR, "WARNING: could not cache source text for \"%s\".\n",
                              jl_string_data(dep));
                    continue;
                }
                size_t slen = jl_string_len(dep);
                write_int32(&f, slen);
                ios_write(&f, depstr, slen);
                posfile = ios_pos(&f);
                write_uint64(&f, 0);   // placeholder for length of this file in bytes
                uint64_t filelen = (uint64_t) ios_copyall(&f, &srctext);
                ios_close(&srctext);
                ios_seek(&f, posfile);
                write_uint64(&f, filelen);
                ios_seek_end(&f);
            }
        }
    }
    write_int32(&f, 0); // mark the end of the source text
    ios_close(&f);

    JL_GC_POP();
    if (jl_fs_rename(tmpfname, fname) < 0) {
        jl_printf(JL_STDERR, "Cannot write cache file \"%s\".\n", fname);
        return 1;
    }

    return 0;
}

#ifndef JL_NDEBUG
// skip the performance optimizations of jl_types_equal and just use subtyping directly
// one of these types is invalid - that's why we're doing the recache type operation
static int jl_invalid_types_equal(jl_datatype_t *a, jl_datatype_t *b)
{
    return jl_subtype((jl_value_t*)a, (jl_value_t*)b) && jl_subtype((jl_value_t*)b, (jl_value_t*)a);
}
#endif

static jl_datatype_t *jl_recache_type(jl_datatype_t *dt, size_t start, jl_value_t *v)
{
    if (v == NULL)
        v = dt->instance; // the instance before unique'ing
    jl_svec_t *tt = dt->parameters;
    if (dt->uid == 0 || dt->uid == -1) {
        // recache all type parameters
        size_t i, l = jl_svec_len(tt);
        for (i = 0; i < l; i++) {
            jl_datatype_t *p = (jl_datatype_t*)jl_svecref(tt, i);
            if (jl_is_datatype(p)) {
                if (p->uid == -1 || p->uid == 0) {
                    jl_datatype_t *cachep = jl_recache_type(p, start, NULL);
                    if (p != cachep) {
                        assert(jl_invalid_types_equal(p, cachep));
                        jl_svecset(tt, i, cachep);
                    }
                }
            }
            else {
                jl_datatype_t *tp = (jl_datatype_t*)jl_typeof(p);
                assert(tp->uid != 0);
                if (tp->uid == -1) {
                    tp = jl_recache_type(tp, start, NULL);
                }
                if (tp->instance && (jl_value_t*)p != tp->instance)
                    jl_svecset(tt, i, tp->instance);
            }
        }
    }

    jl_datatype_t *t; // the type after unique'ing
    if (dt->uid == 0) {
        return dt;
    }
    else if (dt->uid == -1) {
        if (jl_svec_len(tt) == 0) { // jl_cache_type doesn't work if length(parameters) == 0
            dt->uid = jl_assign_type_uid();
            t = dt;
        }
        else {
            dt->uid = 0;
            t = (jl_datatype_t*)jl_cache_type_(dt);
            assert(jl_invalid_types_equal(t, dt));
        }
    }
    else {
        t = dt;
    }
    assert(t->uid != 0);
    if (t == dt && v == NULL)
        return t;
    // delete / replace any other usages of this type in the backref list
    // with the newly constructed object
    size_t i = start;
    while (i < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i + 0];
        int offs = (int)(intptr_t)flagref_list.items[i + 1];
        jl_value_t *o = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        if ((jl_value_t*)dt == o) {
            if (t != dt) {
                if (loc)
                    *loc = (jl_value_t*)t;
                if (offs > 0)
                    backref_list.items[offs] = t;
            }
        }
        else if (v == o) {
            if (t->instance != v) {
                *loc = t->instance;
                if (offs > 0)
                    backref_list.items[offs] = t->instance;
            }
        }
        else {
            i += 2;
            continue;
        }
        // delete this item from the flagref list, so it won't be re-encountered later
        flagref_list.len -= 2;
        if (i >= flagref_list.len)
            break;
        flagref_list.items[i + 0] = flagref_list.items[flagref_list.len + 0];
        flagref_list.items[i + 1] = flagref_list.items[flagref_list.len + 1];
    }
    return t;
}

static void jl_recache_types(void)
{
    size_t i = 0;
    while (i < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i + 0];
        int offs = (int)(intptr_t)flagref_list.items[i + 1];
        jl_value_t *o = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        if (jl_is_method(o) || jl_is_method_instance(o)) {
            i += 2;
        }
        else {
            jl_value_t *v;
            jl_datatype_t *dt, *t;
            if (jl_is_datatype(o)) {
                dt = (jl_datatype_t*)o;
                v = dt->instance;
                t = dt->uid == -1 ? jl_recache_type(dt, i + 2, NULL) : dt;
            }
            else {
                dt = (jl_datatype_t*)jl_typeof(o);
                v = o;
                assert(dt->instance);
                t = jl_recache_type(dt, i + 2, v);
            }
            assert(dt);
            if (t != dt) {
                assert(!type_in_worklist(t));
                jl_set_typeof(dt, (void*)(intptr_t)0x10); // invalidate the old datatype to help catch errors
                if ((jl_value_t*)dt == o) {
                    if (loc)
                        *loc = (jl_value_t*)t;
                    if (offs > 0)
                        backref_list.items[offs] = t;
                }
            }
            if (t->instance != v) {
                jl_set_typeof(v, (void*)(intptr_t)0x20); // invalidate the old value to help catch errors
                if (v == o) {
                    *loc = t->instance;
                    if (offs > 0)
                        backref_list.items[offs] = t->instance;
                }
            }
            // delete this item from the flagref list, so it won't be re-encountered later
            flagref_list.len -= 2;
            if (i >= flagref_list.len)
                break;
            flagref_list.items[i + 0] = flagref_list.items[flagref_list.len + 0];
            flagref_list.items[i + 1] = flagref_list.items[flagref_list.len + 1];
        }
    }
}

static void jl_update_backref_list(jl_value_t *old, jl_value_t *_new, size_t start)
{
    // update the backref list
    size_t i = start;
    while (i < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i + 0];
        int offs = (int)(intptr_t)flagref_list.items[i + 1];
        jl_value_t *v = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        if ((jl_value_t*)v == old) { // same item, update this entry
            if (loc)
                *loc = (jl_value_t*)_new;
            if (offs > 0)
                backref_list.items[offs] = _new;
            // delete this item from the flagref list, so it won't be re-encountered later
            flagref_list.len -= 2;
            if (i >= flagref_list.len)
                break;
            flagref_list.items[i + 0] = flagref_list.items[flagref_list.len + 0];
            flagref_list.items[i + 1] = flagref_list.items[flagref_list.len + 1];
        }
        else {
            i += 2;
        }
    }
}

// repeatedly look up older methods until we come to one that existed
// at the time this module was serialized
static jl_method_t *jl_lookup_method_worldset(jl_methtable_t *mt, jl_datatype_t *sig, arraylist_t *dependent_worlds, size_t *max_world)
{
    size_t world = jl_world_counter;
    jl_typemap_entry_t *entry;
    jl_method_t *_new;
    while (1) {
        entry = jl_typemap_assoc_by_type(
            mt->defs, (jl_value_t*)sig, NULL, /*subtype*/0, /*offs*/0, world, /*max_world_mask*/0);
        if (!entry)
            break;
        _new = (jl_method_t*)entry->func.value;
        world = lowerbound_dependent_world_set(_new->min_world, dependent_worlds);
        if (world == _new->min_world) {
            *max_world = entry->max_world;
            return _new;
        }
    }
    // If we failed to find a method (perhaps due to method deletion),
    // grab anything
    entry = jl_typemap_assoc_by_type(
        mt->defs, (jl_value_t*)sig, NULL, /*subtype*/0, /*offs*/0, /*world*/jl_world_counter, /*max_world_mask*/(~(size_t)0) >> 1);
    assert(entry);
    assert(entry->max_world != ~(size_t)0);
    *max_world = entry->max_world;
    return (jl_method_t*)entry->func.value;
}

static jl_method_t *jl_recache_method(jl_method_t *m, size_t start, arraylist_t *dependent_worlds)
{
    jl_datatype_t *sig = (jl_datatype_t*)m->sig;
    jl_datatype_t *ftype = jl_first_argument_datatype((jl_value_t*)sig);
    jl_methtable_t *mt = ftype->name->mt;
    size_t max_world = 0;
    jl_set_typeof(m, (void*)(intptr_t)0x30); // invalidate the old value to help catch errors
    jl_method_t *_new = jl_lookup_method_worldset(mt, sig, dependent_worlds, &max_world);
    jl_update_backref_list((jl_value_t*)m, (jl_value_t*)_new, start);
    return _new;
}

static jl_method_instance_t *jl_recache_method_instance(jl_method_instance_t *li, size_t start, arraylist_t *dependent_worlds)
{
    jl_datatype_t *sig = (jl_datatype_t*)li->def.value;
    assert(jl_is_datatype(sig) || jl_is_unionall(sig));
    jl_datatype_t *ftype = jl_first_argument_datatype((jl_value_t*)sig);
    jl_methtable_t *mt = ftype->name->mt;
    size_t max_world = 0;
    jl_method_t *m = jl_lookup_method_worldset(mt, sig, dependent_worlds, &max_world);
    jl_datatype_t *argtypes = (jl_datatype_t*)li->specTypes;
    jl_set_typeof(li, (void*)(intptr_t)0x40); // invalidate the old value to help catch errors
    jl_svec_t *env = jl_emptysvec;
    jl_value_t *ti = jl_type_intersection_env((jl_value_t*)argtypes, (jl_value_t*)m->sig, &env);
    //assert(ti != jl_bottom_type); (void)ti;
    if (ti == jl_bottom_type)
        env = jl_emptysvec; // the intersection may fail now if the type system had made an incorrect subtype env in the past
    jl_method_instance_t *_new = jl_specializations_get_linfo(m, (jl_value_t*)argtypes, env, jl_world_counter);
    _new->max_world = max_world;
    jl_update_backref_list((jl_value_t*)li, (jl_value_t*)_new, start);
    return _new;
}

static void jl_recache_other(arraylist_t *dependent_worlds)
{
    size_t i = 0;
    while (i < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i + 0];
        int offs = (int)(intptr_t)flagref_list.items[i + 1];
        jl_value_t *_new, *o = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        i += 2;
        if (jl_is_method(o)) {
            // lookup the real Method based on the placeholder sig
            _new = (jl_value_t*)jl_recache_method((jl_method_t*)o, i, dependent_worlds);
        }
        else if (jl_is_method_instance(o)) {
            // lookup the real MethodInstance based on the placeholder specTypes
            _new = (jl_value_t*)jl_recache_method_instance((jl_method_instance_t*)o, i, dependent_worlds);
        }
        else {
            abort();
        }
        if (loc)
            *loc = _new;
        if (offs > 0)
            backref_list.items[offs] = _new;
    }
}

extern tracer_cb jl_newmeth_tracer;
static int trace_method(jl_typemap_entry_t *entry, void *closure)
{
    jl_call_tracer(jl_newmeth_tracer, (jl_value_t*)entry->func.method);
    return 1;
}

static jl_value_t *_jl_restore_incremental(ios_t *f, jl_array_t *mod_array)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ios_eof(f) || !jl_read_verify_header(f)) {
        ios_close(f);
        return jl_get_exceptionf(jl_errorexception_type,
                "Precompile file header verification checks failed.");
    }
    { // skip past the mod list
        size_t len;
        while ((len = read_int32(f)))
            ios_skip(f, len + 3 * sizeof(uint64_t));
    }
    { // skip past the dependency list
        size_t deplen = read_uint64(f);
        ios_skip(f, deplen);
    }

    // list of world counters of incremental dependencies
    arraylist_t dependent_worlds;
    arraylist_new(&dependent_worlds, 0);

    // verify that the system state is valid
    jl_value_t *verify_fail = read_verify_mod_list(f, &dependent_worlds, mod_array);
    if (verify_fail) {
        arraylist_free(&dependent_worlds);
        ios_close(f);
        return verify_fail;
    }

    // prepare to deserialize
    int en = jl_gc_enable(0);
    jl_gc_enable_finalizers(ptls, 0);
    ++jl_world_counter; // reserve a world age for the deserialization

    arraylist_new(&backref_list, 4000);
    arraylist_push(&backref_list, jl_main_module);
    arraylist_new(&flagref_list, 0);
    arraylist_push(&dependent_worlds, (void*)jl_world_counter);
    arraylist_push(&dependent_worlds, (void*)jl_main_module->primary_world);
    qsort(dependent_worlds.items, dependent_worlds.len, sizeof(size_t), size_isgreater);

    jl_serializer_state s = {
        f, MODE_MODULE,
        NULL, NULL,
        ptls,
        mod_array
    };
    jl_array_t *restored = (jl_array_t*)jl_deserialize_value(&s, (jl_value_t**)&restored);
    serializer_worklist = restored;

    // get list of external generic functions
    jl_value_t *external_methods = jl_deserialize_value(&s, &external_methods);
    jl_value_t *external_backedges = jl_deserialize_value(&s, &external_backedges);

    arraylist_t *tracee_list = NULL;
    if (jl_newmeth_tracer)
        tracee_list = arraylist_new((arraylist_t*)malloc(sizeof(arraylist_t)), 0);

    // at this point, the AST is fully reconstructed, but still completely disconnected
    // now all of the interconnects will be created
    jl_recache_types(); // make all of the types identities correct
    jl_insert_methods((jl_array_t*)external_methods); // hook up methods of external generic functions (needs to be after recache types)
    jl_recache_other(&dependent_worlds); // make all of the other objects identities correct (needs to be after insert methods)
    jl_array_t *init_order = jl_finalize_deserializer(&s, tracee_list); // done with f and s (needs to be after recache)

    JL_GC_PUSH3(&init_order, &restored, &external_backedges);
    jl_gc_enable(en); // subtyping can allocate a lot, not valid before recache-other

    jl_insert_backedges((jl_array_t*)external_backedges, &dependent_worlds); // restore external backedges (needs to be last)

    serializer_worklist = NULL;
    arraylist_free(&flagref_list);
    arraylist_free(&backref_list);
    arraylist_free(&dependent_worlds);
    ios_close(f);

    jl_gc_enable_finalizers(ptls, 1); // make sure we don't run any Julia code concurrently before this point
    if (tracee_list) {
        jl_methtable_t *mt;
        while ((mt = (jl_methtable_t*)arraylist_pop(tracee_list)) != NULL)
            jl_typemap_visitor(mt->defs, trace_method, NULL);
        arraylist_free(tracee_list);
        free(tracee_list);
    }
    jl_init_restored_modules(init_order);
    JL_GC_POP();

    return (jl_value_t*)restored;
}

JL_DLLEXPORT jl_value_t *jl_restore_incremental_from_buf(const char *buf, size_t sz, jl_array_t *mod_array)
{
    ios_t f;
    ios_static_buffer(&f, (char*)buf, sz);
    return _jl_restore_incremental(&f, mod_array);
}

JL_DLLEXPORT jl_value_t *jl_restore_incremental(const char *fname, jl_array_t *mod_array)
{
    ios_t f;
    if (ios_file(&f, fname, 1, 0, 0, 0) == NULL) {
        return jl_get_exceptionf(jl_errorexception_type,
            "Cache file \"%s\" not found.\n", fname);
    }
    return _jl_restore_incremental(&f, mod_array);
}

// --- init ---

void jl_init_serializer(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    htable_new(&ser_tag, 0);
    htable_new(&common_symbol_tag, 0);
    htable_new(&backref_table, 0);

    void *tags[] = { jl_symbol_type, jl_ssavalue_type, jl_datatype_type, jl_slotnumber_type,
                     jl_simplevector_type, jl_array_type, jl_typedslot_type,
                     jl_expr_type, (void*)LongSymbol_tag, (void*)LongSvec_tag,
                     (void*)LongExpr_tag, (void*)LiteralVal_tag, jl_string_type,
                     (void*)SmallInt64_tag, (void*)SmallDataType_tag, jl_typemap_entry_type,
                     (void*)Int32_tag, (void*)Array1d_tag, (void*)Singleton_tag,
                     jl_module_type, jl_tvar_type, jl_method_instance_type, jl_method_type,
                     (void*)CommonSym_tag, (void*)NearbyGlobal_tag, jl_globalref_type,
                     (void*)CoreMod_tag, (void*)BaseMod_tag, (void*)BITypeName_tag,
                     // everything above here represents a class of object rather than only a literal

                     jl_emptysvec, jl_emptytuple, jl_false, jl_true, jl_nothing, jl_any_type,
                     call_sym, invoke_sym, goto_ifnot_sym, return_sym, body_sym, line_sym,
                     lambda_sym, jl_symbol("tuple"), assign_sym, isdefined_sym, boundscheck_sym,
                     unreachable_sym,

                     // empirical list of very common symbols
                     #include "common_symbols1.inc"

                     jl_box_int32(0), jl_box_int32(1), jl_box_int32(2),
                     jl_box_int32(3), jl_box_int32(4), jl_box_int32(5),
                     jl_box_int32(6), jl_box_int32(7), jl_box_int32(8),
                     jl_box_int32(9), jl_box_int32(10), jl_box_int32(11),
                     jl_box_int32(12), jl_box_int32(13), jl_box_int32(14),
                     jl_box_int32(15), jl_box_int32(16), jl_box_int32(17),
                     jl_box_int32(18), jl_box_int32(19), jl_box_int32(20),
                     jl_box_int32(21), jl_box_int32(22), jl_box_int32(23),
                     jl_box_int32(24), jl_box_int32(25), jl_box_int32(26),
                     jl_box_int32(27), jl_box_int32(28), jl_box_int32(29),

                     jl_box_int64(0), jl_box_int64(1), jl_box_int64(2),
                     jl_box_int64(3), jl_box_int64(4), jl_box_int64(5),
                     jl_box_int64(6), jl_box_int64(7), jl_box_int64(8),
                     jl_box_int64(9), jl_box_int64(10), jl_box_int64(11),
                     jl_box_int64(12), jl_box_int64(13), jl_box_int64(14),
                     jl_box_int64(15), jl_box_int64(16), jl_box_int64(17),
                     jl_box_int64(18), jl_box_int64(19), jl_box_int64(20),
                     jl_box_int64(21), jl_box_int64(22), jl_box_int64(23),
                     jl_box_int64(24), jl_box_int64(25), jl_box_int64(26),
                     jl_box_int64(27), jl_box_int64(28), jl_box_int64(29),

                     jl_bool_type, jl_int32_type, jl_int64_type,
                     jl_labelnode_type, jl_linenumbernode_type, jl_gotonode_type,
                     jl_quotenode_type, jl_pinode_type, jl_phinode_type,
                     jl_type_type, jl_bottom_type, jl_ref_type,
                     jl_pointer_type, jl_vararg_type, jl_abstractarray_type, jl_void_type,
                     jl_densearray_type, jl_function_type, jl_unionall_type, jl_typename_type,
                     jl_builtin_type, jl_task_type, jl_uniontype_type, jl_typetype_type,
                     jl_ANY_flag, jl_array_any_type, jl_intrinsic_type,
                     jl_abstractslot_type, jl_methtable_type, jl_typemap_level_type,
                     jl_voidpointer_type, jl_newvarnode_type, jl_abstractstring_type,
                     jl_array_symbol_type, jl_anytuple_type, jl_tparam0(jl_anytuple_type),
                     jl_emptytuple_type, jl_array_uint8_type, jl_code_info_type,
                     jl_typeofbottom_type, jl_namedtuple_type,

                     ptls->root_task,

                     NULL };

    // more common symbols, less common than those above. will get 2-byte encodings.
    void *common_symbols[] = {
        #include "common_symbols2.inc"
        NULL
    };

    intptr_t i=2;
    while (tags[i-2] != NULL) {
        ptrhash_put(&ser_tag, tags[i-2], (void*)i);
        deser_tag[i] = (jl_value_t*)tags[i-2];
        i += 1;
    }
    assert(i <= Null_tag);
    VALUE_TAGS = (intptr_t)ptrhash_get(&ser_tag, jl_emptysvec);

    i=2;
    while (common_symbols[i-2] != NULL) {
        ptrhash_put(&common_symbol_tag, common_symbols[i-2], (void*)i);
        deser_symbols[i] = (jl_value_t*)common_symbols[i-2];
        i += 1;
    }
    assert(i <= 256);

    arraylist_new(&builtin_typenames, 0);
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

#ifdef __cplusplus
}
#endif
