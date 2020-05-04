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
// (not used in MODE_IR)
static htable_t backref_table;
static int backref_table_numel;
static arraylist_t backref_list;

// list of (jl_value_t **loc, size_t pos) entries
// for anything that was flagged by the deserializer for later
// type-rewriting of some sort
// (not used in MODE_IR)
static arraylist_t flagref_list;
static htable_t uniquing_table;

// list of (size_t pos, (void *f)(jl_value_t*)) entries
// for the serializer to mark values in need of rework by function f
// during deserialization later
// (not used in MODE_IR)
static arraylist_t reinit_list;

// list of stuff that is being serialized
// (only used by the incremental serializer in MODE_MODULE)
// This is not quite globally rooted, but we take care to only
// ever assigned rooted values here.
static jl_array_t *serializer_worklist JL_GLOBALLY_ROOTED;

// inverse of backedges tree
// (only used by the incremental serializer in MODE_MODULE)
htable_t edges_map;

#define TAG_SYMBOL              2
#define TAG_SSAVALUE            3
#define TAG_DATATYPE            4
#define TAG_SLOTNUMBER          5
#define TAG_SVEC                6
#define TAG_ARRAY               7
#define TAG_NULL                8
#define TAG_EXPR                9
#define TAG_PHINODE            10
#define TAG_PHICNODE           11
#define TAG_LONG_SYMBOL        12
#define TAG_LONG_SVEC          13
#define TAG_LONG_EXPR          14
#define TAG_LONG_PHINODE       15
#define TAG_LONG_PHICNODE      16
#define TAG_METHODROOT         17
#define TAG_STRING             18
#define TAG_SHORT_INT64        19
#define TAG_SHORT_GENERAL      20
#define TAG_CNULL              21
#define TAG_ARRAY1D            22
#define TAG_SINGLETON          23
#define TAG_MODULE             24
#define TAG_TVAR               25
#define TAG_METHOD_INSTANCE    26
#define TAG_METHOD             27
#define TAG_CODE_INSTANCE      28
#define TAG_COMMONSYM          29
#define TAG_NEARBYGLOBAL       30
#define TAG_GLOBALREF          31
#define TAG_CORE               32
#define TAG_BASE               33
#define TAG_BITYPENAME         34
#define TAG_NEARBYMODULE       35
#define TAG_INT32              36
#define TAG_INT64              37
#define TAG_UINT8              38
#define TAG_VECTORTY           39
#define TAG_PTRTY              40
#define TAG_LONG_SSAVALUE      41
#define TAG_LONG_METHODROOT    42
#define TAG_SHORTER_INT64      43
#define TAG_SHORT_INT32        44
#define TAG_CALL1              45
#define TAG_CALL2              46
#define TAG_LINEINFO           47
#define TAG_SHORT_BACKREF      48
#define TAG_BACKREF            49
#define TAG_UNIONALL           50
#define TAG_GOTONODE           51
#define TAG_QUOTENODE          52
#define TAG_GENERAL            53

#define LAST_TAG 53

typedef enum _DUMP_MODES {
    // not in the serializer at all, or
    // something is seriously wrong
    MODE_INVALID = 0,

    // compressing / decompressing a CodeInfo
    MODE_IR,

    // jl_restore_new_module
    // restoring a single module from disk for integration
    // into the currently running system image / environment
    MODE_MODULE
} DUMP_MODES;

typedef struct {
    ios_t *s;
    DUMP_MODES mode;
    // method we're compressing for in MODE_IR
    jl_method_t *method;
    jl_ptls_t ptls;
    jl_array_t *loaded_modules_array;
} jl_serializer_state;

static jl_value_t *jl_idtable_type = NULL;
static jl_typename_t *jl_idtable_typename = NULL;
static jl_value_t *jl_bigint_type = NULL;
static int gmp_limb_size = 0;
static arraylist_t builtin_typenames;

#define write_uint8(s, n) ios_putc((n), (s))
#define read_uint8(s) ((uint8_t)ios_getc(s))
#define write_int8(s, n) write_uint8(s, n)
#define read_int8(s) read_uint8(s)

/* read and write in host byte order */

static void write_int32(ios_t *s, int32_t i) JL_NOTSAFEPOINT
{
    ios_write(s, (char*)&i, 4);
}

static int32_t read_int32(ios_t *s) JL_NOTSAFEPOINT
{
    int32_t x = 0;
    ios_read(s, (char*)&x, 4);
    return x;
}

static void write_uint64(ios_t *s, uint64_t i) JL_NOTSAFEPOINT
{
    ios_write(s, (char*)&i, 8);
}

static uint64_t read_uint64(ios_t *s) JL_NOTSAFEPOINT
{
    uint64_t x = 0;
    ios_read(s, (char*)&x, 8);
    return x;
}

static void write_int64(ios_t *s, int64_t i) JL_NOTSAFEPOINT
{
    ios_write(s, (char*)&i, 8);
}

static void write_uint16(ios_t *s, uint16_t i) JL_NOTSAFEPOINT
{
    ios_write(s, (char*)&i, 2);
}

static uint16_t read_uint16(ios_t *s) JL_NOTSAFEPOINT
{
    int16_t x = 0;
    ios_read(s, (char*)&x, 2);
    return x;
}

static void write_float64(ios_t *s, double x) JL_NOTSAFEPOINT
{
    write_uint64(s, *((uint64_t*)&x));
}

//#ifdef _P64
//#define write_ulong(s, x) (write_uint64((s), (x)))
//#define read_ulong(s, x) (read_uint64((s), (x)))
//#else
//#define write_ulong(s, x) (write_uint32((s), (x)))
//#define read_ulong(s, x) (read_uint32((s), (x)))
//#endif

// --- serialize ---

#define jl_serialize_value(s, v) jl_serialize_value_((s), (jl_value_t*)(v), 0)
static void jl_serialize_value_(jl_serializer_state *s, jl_value_t *v, int as_literal) JL_GC_DISABLED;
static jl_value_t *jl_deserialize_value(jl_serializer_state *s, jl_value_t **loc) JL_GC_DISABLED;

static void jl_serialize_cnull(jl_serializer_state *s, jl_value_t *t)
{
    backref_table_numel++;
    write_uint8(s->s, TAG_CNULL);
    jl_serialize_value(s, t);
}

static int module_in_worklist(jl_module_t *mod) JL_NOTSAFEPOINT
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
static int type_in_worklist(jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    if (module_in_worklist(dt->name->module))
        return 1;
    int i, l = jl_svec_len(dt->parameters);
    for (i = 0; i < l; i++) {
        jl_value_t *p = jl_unwrap_unionall(jl_tparam(dt, i));
        // XXX: what about Union and TypeVar??
        if (type_in_worklist((jl_datatype_t*)(jl_is_datatype(p) ? p : jl_typeof(p))))
            return 1;
    }
    return 0;
}

static int type_recursively_external(jl_datatype_t *dt);

static int type_parameter_recursively_external(jl_value_t *p0) JL_NOTSAFEPOINT
{
    if (!jl_is_concrete_type(p0))
        return 0;
    jl_datatype_t *p = (jl_datatype_t*)p0;
    //while (jl_is_unionall(p)) {
    //    if (!type_parameter_recursively_external(((jl_unionall_t*)p)->var->lb))
    //        return 0;
    //    if (!type_parameter_recursively_external(((jl_unionall_t*)p)->var->ub))
    //        return 0;
    //    p = (jl_datatype_t*)((jl_unionall_t*)p)->body;
    //}
    if (module_in_worklist(p->name->module))
        return 0;
    if (p->name->wrapper != (jl_value_t*)p0) {
        if (!type_recursively_external(p))
            return 0;
    }
    return 1;
}

// returns true if all of the parameters are tag 6 or 7
static int type_recursively_external(jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    if (!dt->isconcretetype)
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


static void jl_serialize_datatype(jl_serializer_state *s, jl_datatype_t *dt) JL_GC_DISABLED
{
    int tag = 0;
    int internal = module_in_worklist(dt->name->module);
    if (!internal && jl_unwrap_unionall(dt->name->wrapper) == (jl_value_t*)dt) {
        tag = 6; // external primary type
    }
    else if (!dt->isconcretetype) {
        tag = 0; // normal struct
    }
    else if (internal) {
        if (jl_unwrap_unionall(dt->name->wrapper) == (jl_value_t*)dt) // comes up often since functions create types
            tag = 5; // internal, and not in the typename cache
        else
            tag = 10; // anything else that's internal (just may need recaching)
    }
    else if (type_recursively_external(dt)) {
        tag = 7; // external type that can be immediately recreated (with apply_type)
    }
    else if (type_in_worklist(dt)) {
        tag = 11; // external, but definitely new (still needs caching, but not full unique-ing)
    }
    else {
        // this'll need unique-ing later
        // flag this in the backref table as special
        uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, dt);
        assert(*bp != (uintptr_t)HT_NOTFOUND);
        *bp |= 1;
        tag = 12;
    }

    char *dtname = jl_symbol_name(dt->name->name);
    size_t dtnl = strlen(dtname);
    if (dtnl > 4 && strcmp(&dtname[dtnl - 4], "##kw") == 0 && !internal && tag != 0) {
        /* XXX: yuck, this is horrible, but the auto-generated kw types from the serializer isn't a real type, so we *must* be very careful */
        assert(tag == 6); // other struct types should never exist
        tag = 9;
        if (jl_type_type_mt->kwsorter != NULL && dt == (jl_datatype_t*)jl_typeof(jl_type_type_mt->kwsorter)) {
            dt = jl_datatype_type; // any representative member with this MethodTable
        }
        else if (jl_nonfunction_mt->kwsorter != NULL && dt == (jl_datatype_t*)jl_typeof(jl_nonfunction_mt->kwsorter)) {
            dt = jl_symbol_type; // any representative member with this MethodTable
        }
        else {
            // search for the representative member of this MethodTable
            jl_methtable_t *mt = dt->name->mt;
            size_t l = strlen(jl_symbol_name(mt->name));
            char *prefixed;
            prefixed = (char*)malloc_s(l + 2);
            prefixed[0] = '#';
            strcpy(&prefixed[1], jl_symbol_name(mt->name));
            // remove ##kw suffix
            prefixed[l-3] = 0;
            jl_sym_t *tname = jl_symbol(prefixed);
            free(prefixed);
            jl_value_t *primarydt = jl_get_global(mt->module, tname);
            if (!primarydt)
                primarydt = jl_get_global(mt->module, mt->name);
            primarydt = jl_unwrap_unionall(primarydt);
            assert(jl_is_datatype(primarydt));
            assert(primarydt == (jl_value_t*)jl_any_type || jl_typeof(((jl_datatype_t*)primarydt)->name->mt->kwsorter) == (jl_value_t*)dt);
            dt = (jl_datatype_t*)primarydt;
        }
    }

    write_uint8(s->s, TAG_DATATYPE);
    write_uint8(s->s, tag);
    if (tag == 6 || tag == 7) {
        // for tag==6, copy its typevars in case there are references to them elsewhere
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
            | (dt->isinlinealloc << 5)
            | (dt->has_concrete_subtype << 6));
    if (!dt->abstract) {
        write_uint16(s->s, dt->ninitialized);
    }
    write_int32(s->s, dt->hash);

    if (has_layout) {
        uint8_t layout = 0;
        if (dt->layout == ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_array_type))->layout) {
            layout = 1;
        }
        else if (dt->layout == jl_nothing_type->layout) {
            layout = 2;
        }
        else if (dt->layout == ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_pointer_type))->layout) {
            layout = 3;
        }
        write_uint8(s->s, layout);
        if (layout == 0) {
            uint32_t nf = dt->layout->nfields;
            uint32_t np = dt->layout->npointers;
            size_t fieldsize = jl_fielddesc_size(dt->layout->fielddesc_type);
            ios_write(s->s, (const char*)dt->layout, sizeof(*dt->layout));
            size_t fldsize = nf * fieldsize;
            if (dt->layout->first_ptr != -1)
                fldsize += np << dt->layout->fielddesc_type;
            ios_write(s->s, (const char*)(dt->layout + 1), fldsize);
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
    write_uint8(s->s, TAG_MODULE);
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
    for (i = 1; i < m->bindings.size; i += 2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            jl_serialize_value(s, b->name);
            jl_value_t *e = b->value;
            if (!b->constp && e && jl_is_cpointer(e) && jl_unbox_voidpointer(e) != (void*)-1 && jl_unbox_voidpointer(e) != NULL)
                // reset Ptr fields to C_NULL (but keep MAP_FAILED / INVALID_HANDLE)
                jl_serialize_cnull(s, jl_typeof(e));
            else
                jl_serialize_value(s, e);
            jl_serialize_value(s, b->globalref);
            jl_serialize_value(s, b->owner);
            write_int8(s->s, (b->deprecated<<3) | (b->constp<<2) | (b->exportp<<1) | (b->imported));
        }
    }
    jl_serialize_value(s, NULL);
    write_int32(s->s, m->usings.len);
    for(i=0; i < m->usings.len; i++) {
        jl_serialize_value(s, (jl_value_t*)m->usings.items[i]);
    }
    write_uint8(s->s, m->istopmod);
    write_uint64(s->s, m->uuid.hi);
    write_uint64(s->s, m->uuid.lo);
    write_uint64(s->s, m->build_id);
    write_int32(s->s, m->counter);
    write_int32(s->s, m->nospecialize);
    write_int32(s->s, m->optlevel);
}

static int is_ir_node(jl_value_t *v)
{
    // TODO: this accidentally copies QuoteNode(Expr(...)) and QuoteNode(svec(...))
    return jl_is_slot(v) || jl_is_ssavalue(v) ||
        jl_is_uniontype(v) || jl_is_expr(v) || jl_is_newvarnode(v) ||
        jl_is_svec(v) || jl_is_tuple(v) || ((jl_datatype_t*)jl_typeof(v))->instance ||
        jl_is_int32(v) || jl_is_int64(v) || jl_is_bool(v) || jl_is_uint8(v) ||
        jl_is_quotenode(v) || jl_is_gotonode(v) || jl_is_linenode(v) || jl_is_globalref(v) ||
        jl_is_phinode(v) || jl_is_phicnode(v) || jl_is_upsilonnode(v) || jl_is_pinode(v) ||
        jl_typeis(v, jl_lineinfonode_type);
}

static int literal_val_id(jl_serializer_state *s, jl_value_t *v) JL_GC_DISABLED
{
    jl_array_t *rs = s->method->roots;
    int i, l = jl_array_len(rs);
    if (jl_is_symbol(v) || jl_is_concrete_type(v)) {
        for (i = 0; i < l; i++) {
            if (jl_array_ptr_ref(rs, i) == v)
                return i;
        }
    }
    else {
        for (i = 0; i < l; i++) {
            if (jl_egal(jl_array_ptr_ref(rs, i), v))
                return i;
        }
    }
    jl_array_ptr_1d_push(rs, v);
    return jl_array_len(rs) - 1;
}

static void jl_serialize_value_(jl_serializer_state *s, jl_value_t *v, int as_literal) JL_GC_DISABLED
{
    if (v == NULL) {
        write_uint8(s->s, TAG_NULL);
        return;
    }

    void *tag = ptrhash_get(&ser_tag, v);
    if (tag != HT_NOTFOUND) {
        uint8_t t8 = (intptr_t)tag;
        if (t8 <= LAST_TAG)
            write_uint8(s->s, 0);
        write_uint8(s->s, t8);
        return;
    }
    if (jl_is_symbol(v)) {
        void *idx = ptrhash_get(&common_symbol_tag, v);
        if (idx != HT_NOTFOUND) {
            write_uint8(s->s, TAG_COMMONSYM);
            write_uint8(s->s, (uint8_t)(size_t)idx);
            return;
        }
    }
    else if (v == (jl_value_t*)jl_core_module) {
        write_uint8(s->s, TAG_CORE);
        return;
    }
    else if (v == (jl_value_t*)jl_base_module) {
        write_uint8(s->s, TAG_BASE);
        return;
    }

    if (s->mode == MODE_IR) {
        if (v == (jl_value_t*)s->method->module) {
            write_uint8(s->s, TAG_NEARBYMODULE);
            return;
        }
        else if (jl_is_datatype(v) && ((jl_datatype_t*)v)->name == jl_array_typename &&
                 jl_is_long(jl_tparam1(v)) && jl_unbox_long(jl_tparam1(v)) == 1 &&
                 !((jl_datatype_t*)v)->hasfreetypevars) {
            write_uint8(s->s, TAG_VECTORTY);
            jl_serialize_value(s, jl_tparam0(v));
            return;
        }
        else if (jl_is_datatype(v) && ((jl_datatype_t*)v)->name == jl_pointer_typename &&
                 !((jl_datatype_t*)v)->hasfreetypevars) {
            write_uint8(s->s, TAG_PTRTY);
            jl_serialize_value(s, jl_tparam0(v));
            return;
        }
        else if (!as_literal && !is_ir_node(v)) {
            int id = literal_val_id(s, v);
            assert(id >= 0);
            if (id < 256) {
                write_uint8(s->s, TAG_METHODROOT);
                write_uint8(s->s, id);
            }
            else {
                assert(id <= UINT16_MAX);
                write_uint8(s->s, TAG_LONG_METHODROOT);
                write_uint16(s->s, id);
            }
            return;
        }
    }
    else if (jl_typeis(v, jl_string_type) && jl_string_len(v) == 0) {
        jl_serialize_value(s, jl_an_empty_string);
        return;
    }
    else if (!jl_is_uint8(v)) {
        void **bp = ptrhash_bp(&backref_table, v);
        if (*bp != HT_NOTFOUND) {
            uintptr_t pos = (char*)*bp - (char*)HT_NOTFOUND - 1;
            if (pos < 65536) {
                write_uint8(s->s, TAG_SHORT_BACKREF);
                write_uint16(s->s, pos);
            }
            else {
                write_uint8(s->s, TAG_BACKREF);
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
        pos <<= 1;
        ptrhash_put(&backref_table, v, (char*)HT_NOTFOUND + pos + 1);
    }

    size_t i;
    if (jl_is_svec(v)) {
        size_t l = jl_svec_len(v);
        if (l <= 255) {
            write_uint8(s->s, TAG_SVEC);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            write_uint8(s->s, TAG_LONG_SVEC);
            write_int32(s->s, l);
        }
        for (i = 0; i < l; i++) {
            jl_serialize_value(s, jl_svecref(v, i));
        }
    }
    else if (jl_is_symbol(v)) {
        size_t l = strlen(jl_symbol_name((jl_sym_t*)v));
        if (l <= 255) {
            write_uint8(s->s, TAG_SYMBOL);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            write_uint8(s->s, TAG_LONG_SYMBOL);
            write_int32(s->s, l);
        }
        ios_write(s->s, jl_symbol_name((jl_sym_t*)v), l);
    }
    else if (jl_is_globalref(v)) {
        if (s->mode == MODE_IR && jl_globalref_mod(v) == s->method->module) {
            write_uint8(s->s, TAG_NEARBYGLOBAL);
            jl_serialize_value(s, jl_globalref_name(v));
        }
        else {
            write_uint8(s->s, TAG_GLOBALREF);
            jl_serialize_value(s, jl_globalref_mod(v));
            jl_serialize_value(s, jl_globalref_name(v));
        }
    }
    else if (jl_is_ssavalue(v) && ((jl_ssavalue_t*)v)->id < 256 && ((jl_ssavalue_t*)v)->id >= 0) {
        write_uint8(s->s, TAG_SSAVALUE);
        write_uint8(s->s, ((jl_ssavalue_t*)v)->id);
    }
    else if (jl_is_ssavalue(v) && ((jl_ssavalue_t*)v)->id <= UINT16_MAX && ((jl_ssavalue_t*)v)->id >= 0) {
        write_uint8(s->s, TAG_LONG_SSAVALUE);
        write_uint16(s->s, ((jl_ssavalue_t*)v)->id);
    }
    else if (jl_typeis(v, jl_slotnumber_type) && jl_slot_number(v) <= UINT16_MAX && jl_slot_number(v) >= 0) {
        write_uint8(s->s, TAG_SLOTNUMBER);
        write_uint16(s->s, jl_slot_number(v));
    }
    else if (jl_is_array(v)) {
        jl_array_t *ar = (jl_array_t*)v;
        jl_value_t *et = jl_tparam0(jl_typeof(ar));
        int isunion = jl_is_uniontype(et);
        if (ar->flags.ndims == 1 && ar->elsize <= 0x1f) {
            write_uint8(s->s, TAG_ARRAY1D);
            write_uint8(s->s, (ar->flags.ptrarray << 7) | (ar->flags.hasptr << 6) | (isunion << 5) | (ar->elsize & 0x1f));
        }
        else {
            write_uint8(s->s, TAG_ARRAY);
            write_uint16(s->s, ar->flags.ndims);
            write_uint16(s->s, (ar->flags.ptrarray << 15) | (ar->flags.hasptr << 14) | (isunion << 13) | (ar->elsize & 0x1fff));
        }
        for (i = 0; i < ar->flags.ndims; i++)
            jl_serialize_value(s, jl_box_long(jl_array_dim(ar,i)));
        jl_serialize_value(s, jl_typeof(ar));
        size_t l = jl_array_len(ar);
        if (ar->flags.ptrarray) {
            for (i = 0; i < l; i++) {
                jl_value_t *e = jl_array_ptr_ref(v, i);
                if (e && jl_is_cpointer(e) && jl_unbox_voidpointer(e) != (void*)-1 && jl_unbox_voidpointer(e) != NULL)
                    // reset Ptr elements to C_NULL (but keep MAP_FAILED / INVALID_HANDLE)
                    jl_serialize_cnull(s, jl_typeof(e));
                else
                    jl_serialize_value(s, e);
            }
        }
        else if (ar->flags.hasptr) {
            const char *data = (const char*)jl_array_data(ar);
            uint16_t elsz = ar->elsize;
            size_t j, np = ((jl_datatype_t*)et)->layout->npointers;
            for (i = 0; i < l; i++) {
                const char *start = data;
                for (j = 0; j < np; j++) {
                    uint32_t ptr = jl_ptr_offset((jl_datatype_t*)et, j);
                    const jl_value_t *const *fld = &((const jl_value_t *const *)data)[ptr];
                    if ((const char*)fld != start)
                        ios_write(s->s, start, (const char*)fld - start);
                    JL_GC_PROMISE_ROOTED(*fld);
                    jl_serialize_value(s, *fld);
                    start = (const char*)&fld[1];
                }
                data += elsz;
                if (data != start)
                    ios_write(s->s, start, data - start);
            }
        }
        else if (jl_is_cpointer_type(et)) {
            // reset Ptr elements to C_NULL
            const void **data = (const void**)jl_array_data(ar);
            for (i = 0; i < l; i++) {
                const void *e = data[i];
                if (e != (void*)-1)
                    e = NULL;
                ios_write(s->s, (const char*)&e, sizeof(e));
            }
        }
        else {
            ios_write(s->s, (char*)jl_array_data(ar), l * ar->elsize);
            if (jl_array_isbitsunion(ar))
                ios_write(s->s, jl_array_typetagdata(ar), l);
        }
    }
    else if (jl_is_expr(v)) {
        jl_expr_t *e = (jl_expr_t*)v;
        size_t l = jl_array_len(e->args);
        if (e->head == call_sym) {
            if (l == 2) {
                write_uint8(s->s, TAG_CALL1);
                jl_serialize_value(s, jl_exprarg(e, 0));
                jl_serialize_value(s, jl_exprarg(e, 1));
                return;
            }
            else if (l == 3) {
                write_uint8(s->s, TAG_CALL2);
                jl_serialize_value(s, jl_exprarg(e, 0));
                jl_serialize_value(s, jl_exprarg(e, 1));
                jl_serialize_value(s, jl_exprarg(e, 2));
                return;
            }
        }
        if (l <= 255) {
            write_uint8(s->s, TAG_EXPR);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            write_uint8(s->s, TAG_LONG_EXPR);
            write_int32(s->s, l);
        }
        jl_serialize_value(s, e->head);
        for (i = 0; i < l; i++) {
            jl_serialize_value(s, jl_exprarg(e, i));
        }
    }
    else if (jl_is_phinode(v)) {
        jl_array_t *edges = (jl_array_t*)jl_fieldref_noalloc(v, 0);
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(v, 1);
        size_t l = jl_array_len(edges);
        if (l <= 255 && jl_array_len(values) == l) {
            write_uint8(s->s, TAG_PHINODE);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            write_uint8(s->s, TAG_LONG_PHINODE);
            write_int32(s->s, l);
            write_int32(s->s, jl_array_len(values));
        }
        for (i = 0; i < l; i++) {
            jl_serialize_value(s, jl_array_ptr_ref(edges, i));
        }
        l = jl_array_len(values);
        for (i = 0; i < l; i++) {
            jl_serialize_value(s, jl_array_ptr_ref(values, i));
        }
    }
    else if (jl_is_phicnode(v)) {
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(v, 0);
        size_t l = jl_array_len(values);
        if (l <= 255) {
            write_uint8(s->s, TAG_PHICNODE);
            write_uint8(s->s, (uint8_t)l);
        }
        else {
            write_uint8(s->s, TAG_LONG_PHICNODE);
            write_int32(s->s, l);
        }
        for (i = 0; i < l; i++) {
            jl_serialize_value(s, jl_array_ptr_ref(values, i));
        }
    }
    else if (jl_is_gotonode(v)) {
        write_uint8(s->s, TAG_GOTONODE);
        jl_serialize_value(s, jl_get_nth_field(v, 0));
    }
    else if (jl_is_quotenode(v)) {
        write_uint8(s->s, TAG_QUOTENODE);
        jl_serialize_value(s, jl_get_nth_field(v, 0));
    }
    else if (jl_is_datatype(v)) {
        jl_serialize_datatype(s, (jl_datatype_t*)v);
    }
    else if (jl_is_unionall(v)) {
        write_uint8(s->s, TAG_UNIONALL);
        jl_datatype_t *d = (jl_datatype_t*)jl_unwrap_unionall(v);
        if (jl_is_datatype(d) && d->name->wrapper == v &&
            !module_in_worklist(d->name->module)) {
            write_uint8(s->s, 1);
            jl_serialize_value(s, d->name->module);
            jl_serialize_value(s, d->name->name);
        }
        else {
            write_uint8(s->s, 0);
            jl_serialize_value(s, ((jl_unionall_t*)v)->var);
            jl_serialize_value(s, ((jl_unionall_t*)v)->body);
        }
    }
    else if (jl_is_typevar(v)) {
        write_uint8(s->s, TAG_TVAR);
        jl_serialize_value(s, ((jl_tvar_t*)v)->name);
        jl_serialize_value(s, ((jl_tvar_t*)v)->lb);
        jl_serialize_value(s, ((jl_tvar_t*)v)->ub);
    }
    else if (jl_is_method(v)) {
        write_uint8(s->s, TAG_METHOD);
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
        jl_serialize_value(s, (jl_value_t*)m->module);
        write_uint8(s->s, internal);
        if (!internal)
            return;
        jl_methtable_t *mt = jl_method_table_for((jl_value_t*)m->sig);
        assert((jl_value_t*)mt != jl_nothing);
        external_mt = !module_in_worklist(mt->module);
        jl_serialize_value(s, m->specializations);
        jl_serialize_value(s, m->speckeyset);
        jl_serialize_value(s, (jl_value_t*)m->name);
        jl_serialize_value(s, (jl_value_t*)m->file);
        write_int32(s->s, m->line);
        if (external_mt) {
            jl_serialize_value(s, jl_nothing);
            jl_serialize_value(s, jl_nothing);
        }
        else {
            jl_serialize_value(s, (jl_value_t*)m->ambig);
            jl_serialize_value(s, (jl_value_t*)m->resorted);
        }
        write_int32(s->s, m->called);
        write_int32(s->s, m->nargs);
        write_int32(s->s, m->nospecialize);
        write_int32(s->s, m->nkw);
        write_int8(s->s, m->isva);
        write_int8(s->s, m->pure);
        jl_serialize_value(s, (jl_value_t*)m->slot_syms);
        jl_serialize_value(s, (jl_value_t*)m->roots);
        jl_serialize_value(s, (jl_value_t*)m->source);
        jl_serialize_value(s, (jl_value_t*)m->unspecialized);
        jl_serialize_value(s, (jl_value_t*)m->generator);
        jl_serialize_value(s, (jl_value_t*)m->invokes);
    }
    else if (jl_is_method_instance(v)) {
        write_uint8(s->s, TAG_METHOD_INSTANCE);
        jl_method_instance_t *mi = (jl_method_instance_t*)v;
        int internal = 0;
        if (!jl_is_method(mi->def.method))
            internal = 1;
        else if (module_in_worklist(mi->def.method->module))
            internal = 2;
        write_uint8(s->s, internal);
        if (!internal) {
            // also flag this in the backref table as special
            uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, v);
            assert(*bp != (uintptr_t)HT_NOTFOUND);
            *bp |= 1;
        }
        if (internal == 1)
            jl_serialize_value(s, (jl_value_t*)mi->uninferred);
        jl_serialize_value(s, (jl_value_t*)mi->specTypes);
        jl_serialize_value(s, mi->def.value);
        if (!internal)
            return;
        jl_serialize_value(s, (jl_value_t*)mi->sparam_vals);
        jl_array_t *backedges = mi->backedges;
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
        jl_serialize_value(s, (jl_value_t*)mi->cache);
    }
    else if (jl_is_code_instance(v)) {
        write_uint8(s->s, TAG_CODE_INSTANCE);
        jl_code_instance_t *codeinst = (jl_code_instance_t*)v;
        int validate = 0;
        if (codeinst->max_world == ~(size_t)0)
            validate = 1; // can check on deserialize if this cache entry is still valid
        int flags = validate << 0;
        if (codeinst->invoke == jl_fptr_const_return)
            flags |= 1 << 2;
        write_uint8(s->s, flags);
        jl_serialize_value(s, (jl_value_t*)codeinst->def);
        if (validate || codeinst->min_world == 0) {
            jl_serialize_value(s, codeinst->inferred);
            jl_serialize_value(s, codeinst->rettype_const);
            jl_serialize_value(s, codeinst->rettype);
        }
        else {
            // skip storing useless data
            jl_serialize_value(s, NULL);
            jl_serialize_value(s, NULL);
            jl_serialize_value(s, jl_any_type);
        }
        jl_serialize_value(s, codeinst->next);
    }
    else if (jl_typeis(v, jl_module_type)) {
        jl_serialize_module(s, (jl_module_t*)v);
    }
    else if (jl_typeis(v, jl_task_type)) {
        jl_error("Task cannot be serialized");
    }
    else if (jl_typeis(v, jl_string_type)) {
        write_uint8(s->s, TAG_STRING);
        write_int32(s->s, jl_string_len(v));
        ios_write(s->s, jl_string_data(v), jl_string_len(v));
    }
    else if (jl_typeis(v, jl_int64_type)) {
        void *data = jl_data_ptr(v);
        if (*(int64_t*)data >= INT16_MIN && *(int64_t*)data <= INT16_MAX) {
            write_uint8(s->s, TAG_SHORTER_INT64);
            write_uint16(s->s, (uint16_t)*(int64_t*)data);
        }
        else if (*(int64_t*)data >= S32_MIN && *(int64_t*)data <= S32_MAX) {
            write_uint8(s->s, TAG_SHORT_INT64);
            write_int32(s->s, (int32_t)*(int64_t*)data);
        }
        else {
            write_uint8(s->s, TAG_INT64);
            write_int64(s->s, *(int64_t*)data);
        }
    }
    else if (jl_typeis(v, jl_int32_type)) {
        void *data = jl_data_ptr(v);
        if (*(int32_t*)data >= INT16_MIN && *(int32_t*)data <= INT16_MAX) {
            write_uint8(s->s, TAG_SHORT_INT32);
            write_uint16(s->s, (uint16_t)*(int32_t*)data);
        }
        else {
            write_uint8(s->s, TAG_INT32);
            write_int32(s->s, *(int32_t*)data);
        }
    }
    else if (jl_typeis(v, jl_uint8_type)) {
        write_uint8(s->s, TAG_UINT8);
        write_int8(s->s, *(int8_t*)jl_data_ptr(v));
    }
    else if (jl_is_cpointer(v) && jl_unbox_voidpointer(v) == NULL) {
        write_uint8(s->s, TAG_CNULL);
        jl_serialize_value(s, jl_typeof(v));
        return;
    }
    else if (jl_typeis(v, jl_lineinfonode_type)) {
        write_uint8(s->s, TAG_LINEINFO);
        for (i = 0; i < jl_datatype_nfields(jl_lineinfonode_type); i++)
            jl_serialize_value(s, jl_get_nth_field(v, i));
    }
    else if (jl_bigint_type && jl_typeis(v, jl_bigint_type)) {
        write_uint8(s->s, TAG_SHORT_GENERAL);
        write_uint8(s->s, jl_datatype_size(jl_bigint_type));
        jl_serialize_value(s, jl_bigint_type);
        jl_value_t *sizefield = jl_get_nth_field(v, 1);
        jl_serialize_value(s, sizefield);
        void *data = jl_unbox_voidpointer(jl_get_nth_field(v, 2));
        int32_t sz = jl_unbox_int32(sizefield);
        size_t nb = (sz == 0 ? 1 : (sz < 0 ? -sz : sz)) * gmp_limb_size;
        ios_write(s->s, (char*)data, nb);
    }
    else {
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
        if (v == t->instance) {
            if (s->mode == MODE_MODULE && !type_in_worklist(t)) {
                // also flag this in the backref table as special
                // if it might not be unique (is external)
                uintptr_t *bp = (uintptr_t*)ptrhash_bp(&backref_table, v);
                assert(*bp != (uintptr_t)HT_NOTFOUND);
                *bp |= 1;
            }
            write_uint8(s->s, TAG_SINGLETON);
            jl_serialize_value(s, t);
            return;
        }
        assert(!t->instance && "detected singleton construction corruption");

        if (t == jl_typename_type) {
            void *bttag = ptrhash_get(&ser_tag, ((jl_typename_t*)t)->wrapper);
            if (bttag != HT_NOTFOUND) {
                write_uint8(s->s, TAG_BITYPENAME);
                write_uint8(s->s, (uint8_t)(intptr_t)bttag);
                return;
            }
        }
        if (t->size <= 255) {
            write_uint8(s->s, TAG_SHORT_GENERAL);
            write_uint8(s->s, t->size);
        }
        else {
            write_uint8(s->s, TAG_GENERAL);
            write_int32(s->s, t->size);
        }
        jl_serialize_value(s, t);
        if (t == jl_typename_type) {
            jl_typename_t *tn = (jl_typename_t*)v;
            int internal = module_in_worklist(tn->module);
            write_uint8(s->s, internal);
            jl_serialize_value(s, tn->module);
            jl_serialize_value(s, tn->name);
            if (internal) {
                jl_serialize_value(s, tn->names);
                jl_serialize_value(s, tn->wrapper);
                jl_serialize_value(s, tn->mt);
                ios_write(s->s, (char*)&tn->hash, sizeof(tn->hash));
            }
            return;
        }
        if (t == jl_typemap_level_type) {
            // perform some compression on the typemap levels
            // (which will need to be rehashed during deserialization anyhow)
            jl_typemap_level_t *node = (jl_typemap_level_t*)v;
            assert( // make sure this type has the expected ordering and layout
                offsetof(jl_typemap_level_t, arg1) == 0 * sizeof(jl_value_t*) &&
                offsetof(jl_typemap_level_t, targ) == 1 * sizeof(jl_value_t*) &&
                offsetof(jl_typemap_level_t, linear) == 2 * sizeof(jl_value_t*) &&
                offsetof(jl_typemap_level_t, any) == 3 * sizeof(jl_value_t*) &&
                sizeof(jl_typemap_level_t) == 4 * sizeof(jl_value_t*));
            jl_serialize_value(s, node->arg1);
            jl_serialize_value(s, node->targ);
            jl_serialize_value(s, node->linear);
            jl_serialize_value(s, node->any);
            return;
        }

        char *data = (char*)jl_data_ptr(v);
        size_t i, j, np = t->layout->npointers;
        uint32_t nf = t->layout->nfields;
        char *last = data;
        for (i = 0, j = 0; i < nf+1; i++) {
            char *ptr = data + (i < nf ? jl_field_offset(t, i) : jl_datatype_size(t));
            if (j < np) {
                char *prevptr = (char*)&((jl_value_t**)data)[jl_ptr_offset(t, j)];
                while (ptr > prevptr) {
                    // previous field contained pointers; write them and their interleaved data
                    if (prevptr > last)
                        ios_write(s->s, last, prevptr - last);
                    jl_value_t *e = *(jl_value_t**)prevptr;
                    JL_GC_PROMISE_ROOTED(e);
                    if (t->mutabl && e && jl_field_isptr(t, i - 1) && jl_is_cpointer(e) &&
                        jl_unbox_voidpointer(e) != (void*)-1 && jl_unbox_voidpointer(e) != NULL)
                        // reset Ptr fields to C_NULL (but keep MAP_FAILED / INVALID_HANDLE)
                        jl_serialize_cnull(s, jl_typeof(e));
                    else
                        jl_serialize_value(s, e);
                    last = prevptr + sizeof(jl_value_t*);
                    j++;
                    if (j < np)
                        prevptr = (char*)&((jl_value_t**)data)[jl_ptr_offset(t, j)];
                    else
                        break;
                }
            }
            if (i == nf)
                break;
            if (t->mutabl && jl_is_cpointer_type(jl_field_type(t, i)) && *(void**)ptr != (void*)-1) {
                if (ptr > last)
                    ios_write(s->s, last, ptr - last);
                char *n = NULL;
                ios_write(s->s, (char*)&n, sizeof(n));
                last = ptr + sizeof(n);
            }
        }
        char *ptr = data + jl_datatype_size(t);
        if (ptr > last)
            ios_write(s->s, last, ptr - last);
    }
}

static void jl_collect_missing_backedges_to_mod(jl_methtable_t *mt)
{
    jl_array_t *backedges = mt->backedges;
    if (backedges) {
        size_t i, l = jl_array_len(backedges);
        for (i = 1; i < l; i += 2) {
            jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(backedges, i);
            jl_value_t *missing_callee = jl_array_ptr_ref(backedges, i - 1);
            jl_array_t **edges = (jl_array_t**)ptrhash_bp(&edges_map, (void*)caller);
            if (*edges == HT_NOTFOUND)
                *edges = jl_alloc_vec_any(0);
            jl_array_ptr_1d_push(*edges, missing_callee);
        }
    }
}

// the intent of this function is to invert the backedges tree
// for anything that points to a method not part of the worklist
static void collect_backedges(jl_method_instance_t *callee) JL_GC_DISABLED
{
    jl_array_t *backedges = callee->backedges;
    if (backedges) {
        size_t i, l = jl_array_len(backedges);
        for (i = 0; i < l; i++) {
            jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(backedges, i);
            jl_array_t **edges = (jl_array_t**)ptrhash_bp(&edges_map, caller);
            if (*edges == HT_NOTFOUND)
                *edges = jl_alloc_vec_any(0);
            jl_array_ptr_1d_push(*edges, (jl_value_t*)callee);
        }
    }
}


static int jl_collect_methcache_from_mod(jl_typemap_entry_t *ml, void *closure) JL_GC_DISABLED
{
    jl_array_t *s = (jl_array_t*)closure;
    jl_method_t *m = ml->func.method;
    if (module_in_worklist(m->module)) {
        jl_array_ptr_1d_push(s, (jl_value_t*)m);
        jl_array_ptr_1d_push(s, (jl_value_t*)ml->simplesig);
    }
    else {
        jl_svec_t *specializations = m->specializations;
        size_t i, l = jl_svec_len(specializations);
        for (i = 0; i < l; i++) {
            jl_method_instance_t *callee = (jl_method_instance_t*)jl_svecref(specializations, i);
            if (callee != NULL)
                collect_backedges(callee);
        }
    }
    return 1;
}

static void jl_collect_methtable_from_mod(jl_array_t *s, jl_methtable_t *mt) JL_GC_DISABLED
{
    jl_typemap_visitor(mt->defs, jl_collect_methcache_from_mod, (void*)s);
}

static void jl_collect_lambdas_from_mod(jl_array_t *s, jl_module_t *m) JL_GC_DISABLED
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
                                (mt != jl_type_type_mt && mt != jl_nonfunction_mt)) {
                            jl_collect_methtable_from_mod(s, mt);
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
static void jl_collect_backedges_to(jl_method_instance_t *caller, htable_t *all_callees) JL_GC_DISABLED
{
    jl_array_t **pcallees = (jl_array_t**)ptrhash_bp(&edges_map, (void*)caller),
                *callees = *pcallees;
    if (callees != HT_NOTFOUND) {
        *pcallees = (jl_array_t*) HT_NOTFOUND;
        size_t i, l = jl_array_len(callees);
        for (i = 0; i < l; i++) {
            jl_value_t *c = jl_array_ptr_ref(callees, i);
            ptrhash_put(all_callees, c, c);
            if (jl_is_method_instance(c)) {
                jl_collect_backedges_to((jl_method_instance_t*)c, all_callees);
            }
        }
    }
}

static void jl_collect_backedges(jl_array_t *s, jl_array_t *t)
{
    htable_t all_targets;
    htable_t all_callees;
    htable_new(&all_targets, 0);
    htable_new(&all_callees, 0);
    size_t i;
    void **table = edges_map.table;
    for (i = 0; i < edges_map.size; i += 2) {
        jl_method_instance_t *caller = (jl_method_instance_t*)table[i];
        jl_array_t *callees = (jl_array_t*)table[i + 1];
        if (callees != HT_NOTFOUND && module_in_worklist(caller->def.method->module)) {
            size_t i, l = jl_array_len(callees);
            for (i = 0; i < l; i++) {
                jl_value_t *c = jl_array_ptr_ref(callees, i);
                ptrhash_put(&all_callees, c, c);
                if (jl_is_method_instance(c)) {
                    jl_collect_backedges_to((jl_method_instance_t*)c, &all_callees);
                }
            }
            callees = jl_alloc_array_1d(jl_array_int32_type, 0);
            void **pc = all_callees.table;
            size_t j;
            int valid = 1;
            for (j = 0; valid && j < all_callees.size; j += 2) {
                if (pc[j + 1] != HT_NOTFOUND) {
                    jl_value_t *callee = (jl_value_t*)pc[j];
                    void *target = ptrhash_get(&all_targets, (void*)callee);
                    if (target == HT_NOTFOUND) {
                        jl_method_instance_t *callee_mi = (jl_method_instance_t*)callee;
                        jl_value_t *sig;
                        if (jl_is_method_instance(callee)) {
                            sig = callee_mi->specTypes;
                        }
                        else {
                            sig = callee;
                        }
                        size_t min_valid = 0;
                        size_t max_valid = ~(size_t)0;
                        jl_value_t *matches = jl_matching_methods((jl_tupletype_t*)sig, -1, 0, jl_world_counter, &min_valid, &max_valid);
                        if (matches == jl_false) {
                            valid = 0;
                            break;
                        }
                        size_t k;
                        for (k = 0; k < jl_array_len(matches); k++) {
                            jl_array_ptr_set(matches, k, jl_svecref(jl_array_ptr_ref(matches, k), 2));
                        }
                        jl_array_ptr_1d_push(t, callee);
                        jl_array_ptr_1d_push(t, matches);
                        target = (char*)HT_NOTFOUND + jl_array_len(t) / 2;
                        ptrhash_put(&all_targets, (void*)callee, target);
                    }
                    jl_array_grow_end(callees, 1);
                    ((int32_t*)jl_array_data(callees))[jl_array_len(callees) - 1] = (char*)target - (char*)HT_NOTFOUND - 1;
                }
            }
            htable_reset(&all_callees, 100);
            if (valid) {
                jl_array_ptr_1d_push(s, (jl_value_t*)caller);
                jl_array_ptr_1d_push(s, (jl_value_t*)callees);
            }
        }
    }
    htable_free(&all_targets);
    htable_free(&all_callees);
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
static const int JI_FORMAT_VERSION = 11;
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

static void write_module_path(ios_t *s, jl_module_t *depmod) JL_NOTSAFEPOINT
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
// are include dependencies
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

static jl_value_t *jl_deserialize_datatype(jl_serializer_state *s, int pos, jl_value_t **loc) JL_GC_DISABLED
{
    int tag = read_uint8(s->s);
    if (tag == 6 || tag == 7) {
        jl_typename_t *name = (jl_typename_t*)jl_deserialize_value(s, NULL);
        jl_value_t *dtv = name->wrapper;
        jl_svec_t *parameters = (jl_svec_t*)jl_deserialize_value(s, NULL);
        dtv = jl_apply_type(dtv, jl_svec_data(parameters), jl_svec_len(parameters));
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
    if (tag == 0 || tag == 5 || tag == 10 || tag == 11 || tag == 12)
        dt = jl_new_uninitialized_datatype();
    else {
        assert(0 && "corrupt deserialization state");
        abort();
    }
    assert(s->method == NULL && s->mode != MODE_IR && "no new data-types expected during MODE_IR");
    assert(pos == backref_list.len - 1 && "nothing should have been deserialized since assigning pos");
    backref_list.items[pos] = dt;
    dt->size = size;
    dt->instance = NULL;
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
    dt->has_concrete_subtype = (memflags >> 6) & 1;
    dt->types = NULL;
    dt->parameters = NULL;
    dt->name = NULL;
    dt->super = NULL;
    dt->layout = NULL;
    if (!dt->abstract)
        dt->ninitialized = read_uint16(s->s);
    else
        dt->ninitialized = 0;
    dt->hash = read_int32(s->s);

    if (has_layout) {
        uint8_t layout = read_uint8(s->s);
        if (layout == 1) {
            dt->layout = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_array_type))->layout;
        }
        else if (layout == 2) {
            dt->layout = jl_nothing_type->layout;
        }
        else if (layout == 3) {
            dt->layout = ((jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)jl_pointer_type))->layout;
        }
        else {
            assert(layout == 0);
            jl_datatype_layout_t buffer;
            ios_readall(s->s, (char*)&buffer, sizeof(buffer));
            uint32_t nf = buffer.nfields;
            uint32_t np = buffer.npointers;
            uint8_t fielddesc_type = buffer.fielddesc_type;
            size_t fielddesc_size = nf > 0 ? jl_fielddesc_size(fielddesc_type) : 0;
            size_t fldsize = nf * fielddesc_size;
            if (buffer.first_ptr != -1)
                fldsize += np << fielddesc_type;
            jl_datatype_layout_t *layout = (jl_datatype_layout_t*)jl_gc_perm_alloc(
                    sizeof(jl_datatype_layout_t) + fldsize,
                    0, 4, 0);
            *layout = buffer;
            ios_readall(s->s, (char*)(layout + 1), fldsize);
            dt->layout = layout;
        }
    }

    if (tag == 10 || tag == 11 || tag == 12) {
        assert(pos > 0);
        arraylist_push(&flagref_list, loc == HT_NOTFOUND ? NULL : loc);
        arraylist_push(&flagref_list, (void*)(uintptr_t)pos);
        ptrhash_put(&uniquing_table, dt, NULL);
    }

    if (has_instance) {
        assert(dt->isconcretetype && "there shouldn't be an instance on an abstract type");
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
    if (dt->types) jl_gc_wb(dt, dt->types);

    return (jl_value_t*)dt;
}

static jl_value_t *jl_deserialize_value_svec(jl_serializer_state *s, uint8_t tag) JL_GC_DISABLED
{
    int usetable = (s->mode != MODE_IR);
    size_t i, len;
    if (tag == TAG_SVEC)
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

static jl_value_t *jl_deserialize_value_symbol(jl_serializer_state *s, uint8_t tag) JL_GC_DISABLED
{
    int usetable = (s->mode != MODE_IR);
    size_t len;
    if (tag == TAG_SYMBOL)
        len = read_uint8(s->s);
    else
        len = read_int32(s->s);
    char *name = (char*)(len >= 256 ? malloc_s(len + 1) : alloca(len + 1));
    ios_readall(s->s, name, len);
    name[len] = '\0';
    jl_value_t *sym = (jl_value_t*)jl_symbol(name);
    if (len >= 256)
        free(name);
    if (usetable)
        arraylist_push(&backref_list, sym);
    return sym;
}

static jl_value_t *jl_deserialize_value_array(jl_serializer_state *s, uint8_t tag) JL_GC_DISABLED
{
    int usetable = (s->mode != MODE_IR);
    int16_t i, ndims;
    int isptr, isunion, hasptr, elsize;
    if (tag == TAG_ARRAY1D) {
        ndims = 1;
        elsize = read_uint8(s->s);
        isptr = (elsize >> 7) & 1;
        hasptr = (elsize >> 6) & 1;
        isunion = (elsize >> 5) & 1;
        elsize = elsize & 0x1f;
    }
    else {
        ndims = read_uint16(s->s);
        elsize = read_uint16(s->s);
        isptr = (elsize >> 15) & 1;
        hasptr = (elsize >> 14) & 1;
        isunion = (elsize >> 13) & 1;
        elsize = elsize & 0x3fff;
    }
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, NULL);
    size_t *dims = (size_t*)alloca(ndims * sizeof(size_t));
    for (i = 0; i < ndims; i++) {
        dims[i] = jl_unbox_long(jl_deserialize_value(s, NULL));
    }
    jl_array_t *a = jl_new_array_for_deserialization(
            (jl_value_t*)NULL, ndims, dims, !isptr, hasptr, isunion, elsize);
    if (usetable)
        backref_list.items[pos] = a;
    jl_value_t *aty = jl_deserialize_value(s, &jl_astaggedvalue(a)->type);
    jl_set_typeof(a, aty);
    if (a->flags.ptrarray) {
        jl_value_t **data = (jl_value_t**)jl_array_data(a);
        size_t i, numel = jl_array_len(a);
        for (i = 0; i < numel; i++) {
            data[i] = jl_deserialize_value(s, &data[i]);
            //if (data[i]) // not needed because `a` is new (gc is disabled)
            //    jl_gc_wb(a, data[i]);
        }
        assert(jl_astaggedvalue(a)->bits.gc == GC_CLEAN); // gc is disabled
    }
    else if (a->flags.hasptr) {
        size_t i, numel = jl_array_len(a);
        char *data = (char*)jl_array_data(a);
        uint16_t elsz = a->elsize;
        jl_datatype_t *et = (jl_datatype_t*)jl_tparam0(jl_typeof(a));
        size_t j, np = et->layout->npointers;
        for (i = 0; i < numel; i++) {
            char *start = data;
            for (j = 0; j < np; j++) {
                uint32_t ptr = jl_ptr_offset(et, j);
                jl_value_t **fld = &((jl_value_t**)data)[ptr];
                if ((char*)fld != start)
                    ios_readall(s->s, start, (const char*)fld - start);
                *fld = jl_deserialize_value(s, fld);
                //if (*fld) // not needed because `a` is new (gc is disabled)
                //    jl_gc_wb(a, *fld);
                start = (char*)&fld[1];
            }
            data += elsz;
            if (data != start)
                ios_readall(s->s, start, data - start);
        }
        assert(jl_astaggedvalue(a)->bits.gc == GC_CLEAN); // gc is disabled
    }
    else {
        size_t extra = jl_array_isbitsunion(a) ? jl_array_len(a) : 0;
        size_t tot = jl_array_len(a) * a->elsize + extra;
        ios_readall(s->s, (char*)jl_array_data(a), tot);
    }
    return (jl_value_t*)a;
}

static jl_value_t *jl_deserialize_value_expr(jl_serializer_state *s, uint8_t tag) JL_GC_DISABLED
{
    int usetable = (s->mode != MODE_IR);
    size_t i, len;
    jl_sym_t *head = NULL;
    if (tag == TAG_EXPR) {
        len = read_uint8(s->s);
    }
    else if (tag == TAG_CALL1) {
        len = 2;
        head = call_sym;
    }
    else if (tag == TAG_CALL2) {
        len = 3;
        head = call_sym;
    }
    else {
        len = read_int32(s->s);
    }
    int pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, NULL);
    if (head == NULL)
        head = (jl_sym_t*)jl_deserialize_value(s, NULL);
    jl_expr_t *e = jl_exprn(head, len);
    if (usetable)
        backref_list.items[pos] = e;
    jl_value_t **data = (jl_value_t**)(e->args->data);
    for (i = 0; i < len; i++) {
        data[i] = jl_deserialize_value(s, &data[i]);
    }
    return (jl_value_t*)e;
}

static jl_value_t *jl_deserialize_value_phi(jl_serializer_state *s, uint8_t tag) JL_GC_DISABLED
{
    int usetable = (s->mode != MODE_IR);
    size_t i, len_e, len_v;
    if (tag == TAG_PHINODE) {
        len_e = len_v = read_uint8(s->s);
    }
    else {
        len_e = read_int32(s->s);
        len_v = read_int32(s->s);
    }
    jl_array_t *e = jl_alloc_vec_any(len_e);
    jl_array_t *v = jl_alloc_vec_any(len_v);
    jl_value_t *phi = jl_new_struct(jl_phinode_type, e, v);
    if (usetable)
        arraylist_push(&backref_list, phi);
    jl_value_t **data_e = (jl_value_t**)(e->data);
    for (i = 0; i < len_e; i++) {
        data_e[i] = jl_deserialize_value(s, &data_e[i]);
    }
    jl_value_t **data_v = (jl_value_t**)(v->data);
    for (i = 0; i < len_v; i++) {
        data_v[i] = jl_deserialize_value(s, &data_v[i]);
    }
    return phi;
}

static jl_value_t *jl_deserialize_value_phic(jl_serializer_state *s, uint8_t tag) JL_GC_DISABLED
{
    int usetable = (s->mode != MODE_IR);
    size_t i, len;
    if (tag == TAG_PHICNODE)
        len = read_uint8(s->s);
    else
        len = read_int32(s->s);
    jl_array_t *v = jl_alloc_vec_any(len);
    jl_value_t *phic = jl_new_struct(jl_phicnode_type, v);
    if (usetable)
        arraylist_push(&backref_list, phic);
    jl_value_t **data = (jl_value_t**)(v->data);
    for (i = 0; i < len; i++) {
        data[i] = jl_deserialize_value(s, &data[i]);
    }
    return phic;
}

static jl_value_t *jl_deserialize_value_method(jl_serializer_state *s, jl_value_t **loc) JL_GC_DISABLED
{
    int usetable = (s->mode != MODE_IR);
    jl_method_t *m =
        (jl_method_t*)jl_gc_alloc(s->ptls, sizeof(jl_method_t),
                                  jl_method_type);
    memset(m, 0, sizeof(jl_method_t));
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, m);
    m->sig = (jl_value_t*)jl_deserialize_value(s, (jl_value_t**)&m->sig);
    jl_gc_wb(m, m->sig);
    m->module = (jl_module_t*)jl_deserialize_value(s, (jl_value_t**)&m->module);
    jl_gc_wb(m, m->module);
    int internal = read_uint8(s->s);
    if (!internal) {
        assert(loc != NULL && loc != HT_NOTFOUND);
        arraylist_push(&flagref_list, loc);
        arraylist_push(&flagref_list, (void*)pos);
        return (jl_value_t*)m;
    }
    m->specializations = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&m->specializations);
    jl_gc_wb(m, m->specializations);
    m->speckeyset = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&m->speckeyset);
    jl_gc_wb(m, m->speckeyset);
    m->name = (jl_sym_t*)jl_deserialize_value(s, NULL);
    jl_gc_wb(m, m->name);
    m->file = (jl_sym_t*)jl_deserialize_value(s, NULL);
    m->line = read_int32(s->s);
    m->primary_world = jl_world_counter;
    m->deleted_world = ~(size_t)0;
    m->ambig = jl_deserialize_value(s, (jl_value_t**)&m->ambig);
    jl_gc_wb(m, m->ambig);
    m->resorted = jl_deserialize_value(s, (jl_value_t**)&m->resorted);
    jl_gc_wb(m, m->resorted);
    m->called = read_int32(s->s);
    m->nargs = read_int32(s->s);
    m->nospecialize = read_int32(s->s);
    m->nkw = read_int32(s->s);
    m->isva = read_int8(s->s);
    m->pure = read_int8(s->s);
    m->slot_syms = jl_deserialize_value(s, (jl_value_t**)&m->slot_syms);
    jl_gc_wb(m, m->slot_syms);
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
    m->invokes = jl_deserialize_value(s, (jl_value_t**)&m->invokes);
    jl_gc_wb(m, m->invokes);
    JL_MUTEX_INIT(&m->writelock);
    return (jl_value_t*)m;
}

static jl_value_t *jl_deserialize_value_method_instance(jl_serializer_state *s, jl_value_t **loc) JL_GC_DISABLED
{
    int usetable = (s->mode != MODE_IR);
    jl_method_instance_t *mi =
        (jl_method_instance_t*)jl_gc_alloc(s->ptls, sizeof(jl_method_instance_t),
                                       jl_method_instance_type);
    memset(mi, 0, sizeof(jl_method_instance_t));
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, mi);
    int internal = read_uint8(s->s);
    mi->specTypes = (jl_value_t*)jl_deserialize_value(s, (jl_value_t**)&mi->specTypes);
    jl_gc_wb(mi, mi->specTypes);
    mi->def.value = jl_deserialize_value(s, &mi->def.value);
    jl_gc_wb(mi, mi->def.value);

    if (!internal) {
        assert(loc != NULL && loc != HT_NOTFOUND);
        arraylist_push(&flagref_list, loc);
        arraylist_push(&flagref_list, (void*)pos);
        return (jl_value_t*)mi;
    }

    if (internal == 1) {
        mi->uninferred = jl_deserialize_value(s, &mi->uninferred);
        jl_gc_wb(mi, mi->uninferred);
    }
    mi->sparam_vals = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&mi->sparam_vals);
    jl_gc_wb(mi, mi->sparam_vals);
    mi->backedges = (jl_array_t*)jl_deserialize_value(s, (jl_value_t**)&mi->backedges);
    if (mi->backedges)
        jl_gc_wb(mi, mi->backedges);
    mi->cache = (jl_code_instance_t*)jl_deserialize_value(s, (jl_value_t**)&mi->cache);
    if (mi->cache)
        jl_gc_wb(mi, mi->cache);
    return (jl_value_t*)mi;
}

static jl_value_t *jl_deserialize_value_code_instance(jl_serializer_state *s, jl_value_t **loc) JL_GC_DISABLED
{
    int usetable = (s->mode != MODE_IR);
    jl_code_instance_t *codeinst =
        (jl_code_instance_t*)jl_gc_alloc(s->ptls, sizeof(jl_code_instance_t), jl_code_instance_type);
    memset(codeinst, 0, sizeof(jl_code_instance_t));
    if (usetable)
        arraylist_push(&backref_list, codeinst);
    int flags = read_uint8(s->s);
    int validate = (flags >> 0) & 3;
    int constret = (flags >> 2) & 1;
    codeinst->def = (jl_method_instance_t*)jl_deserialize_value(s, (jl_value_t**)&codeinst->def);
    jl_gc_wb(codeinst, codeinst->def);
    codeinst->inferred = jl_deserialize_value(s, &codeinst->inferred);
    jl_gc_wb(codeinst, codeinst->inferred);
    codeinst->rettype_const = jl_deserialize_value(s, &codeinst->rettype_const);
    if (codeinst->rettype_const)
        jl_gc_wb(codeinst, codeinst->rettype_const);
    codeinst->rettype = jl_deserialize_value(s, &codeinst->rettype);
    jl_gc_wb(codeinst, codeinst->rettype);
    if (constret)
        codeinst->invoke = jl_fptr_const_return;
    codeinst->next = (jl_code_instance_t*)jl_deserialize_value(s, (jl_value_t**)&codeinst->next);
    jl_gc_wb(codeinst, codeinst->next);
    if (validate)
        codeinst->min_world = jl_world_counter;
    return (jl_value_t*)codeinst;
}

static jl_value_t *jl_deserialize_value_module(jl_serializer_state *s) JL_GC_DISABLED
{
    int usetable = (s->mode != MODE_IR);
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
    m->counter = read_int32(s->s);
    m->nospecialize = read_int32(s->s);
    m->optlevel = read_int32(s->s);
    m->primary_world = jl_world_counter;
    return (jl_value_t*)m;
}

static jl_value_t *jl_deserialize_value_globalref(jl_serializer_state *s) JL_GC_DISABLED
{
    int usetable = (s->mode != MODE_IR);
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

static jl_value_t *jl_deserialize_value_singleton(jl_serializer_state *s, jl_value_t **loc) JL_GC_DISABLED
{
    if (s->mode == MODE_IR) {
        jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s, NULL);
        return dt->instance;
    }
    jl_value_t *v = (jl_value_t*)jl_gc_alloc(s->ptls, 0, NULL);
    uintptr_t pos = backref_list.len;
    arraylist_push(&backref_list, (void*)v);
    if (s->mode == MODE_MODULE) {
        // TODO: optimize the case where the value can easily be obtained
        // from an external module (tag == 6) as dt->instance
        assert(loc != HT_NOTFOUND);
        // if loc == NULL, then the caller can't provide the address where the instance will be
        // stored. this happens if a field might store a 0-size value, but the field itself is
        // not 0 size, e.g. `::Union{Int,Nothing}`
        if (loc != NULL) {
            arraylist_push(&flagref_list, loc);
            arraylist_push(&flagref_list, (void*)pos);
        }
    }
    jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s, (jl_value_t**)HT_NOTFOUND); // no loc, since if dt is replaced, then dt->instance would be also
    jl_set_typeof(v, dt);
    if (dt->instance == NULL)
        return v;
    return dt->instance;
}

static void jl_deserialize_struct(jl_serializer_state *s, jl_value_t *v) JL_GC_DISABLED
{
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(v);
    char *data = (char*)jl_data_ptr(v);
    size_t i, np = dt->layout->npointers;
    char *start = data;
    for (i = 0; i < np; i++) {
        uint32_t ptr = jl_ptr_offset(dt, i);
        jl_value_t **fld = &((jl_value_t**)data)[ptr];
        if ((char*)fld != start)
            ios_readall(s->s, start, (const char*)fld - start);
        *fld = jl_deserialize_value(s, fld);
        //if (*fld)// a is new (gc is disabled)
        //    jl_gc_wb(a, *fld);
        start = (char*)&fld[1];
    }
    data += jl_datatype_size(dt);
    if (data != start)
        ios_readall(s->s, start, data - start);
    if (dt == jl_typemap_entry_type) {
        jl_typemap_entry_t *entry = (jl_typemap_entry_t*)v;
        if (entry->max_world == ~(size_t)0) {
            if (entry->min_world > 1) {
                // update world validity to reflect current state of the counter
                entry->min_world = jl_world_counter;
            }
        }
        else {
            // garbage entry - delete it :(
            entry->min_world = 1;
            entry->max_world = 0;
        }
    }
}

static jl_value_t *jl_deserialize_value_any(jl_serializer_state *s, uint8_t tag, jl_value_t **loc) JL_GC_DISABLED
{
    int usetable = (s->mode != MODE_IR);
    int32_t sz = (tag == TAG_SHORT_GENERAL ? read_uint8(s->s) : read_int32(s->s));
    jl_value_t *v = jl_gc_alloc(s->ptls, sz, NULL);
    jl_set_typeof(v, (void*)(intptr_t)0x50);
    uintptr_t pos = backref_list.len;
    if (usetable)
        arraylist_push(&backref_list, v);
    jl_datatype_t *dt = (jl_datatype_t*)jl_deserialize_value(s, &jl_astaggedvalue(v)->type);
    assert(s->mode == MODE_IR || sz != 0 || loc);
    if (s->mode == MODE_MODULE && dt == jl_typename_type) {
        int internal = read_uint8(s->s);
        jl_typename_t *tn;
        if (internal) {
            tn = (jl_typename_t*)jl_gc_alloc(
                    s->ptls, sizeof(jl_typename_t), jl_typename_type);
            memset(tn, 0, sizeof(jl_typename_t));
            tn->cache = jl_emptysvec; // the cache is refilled later (tag 5)
            tn->linearcache = jl_emptysvec; // the cache is refilled later (tag 5)
            tn->partial = NULL;
            if (usetable)
                backref_list.items[pos] = tn;
        }
        jl_module_t *m = (jl_module_t*)jl_deserialize_value(s, NULL);
        jl_sym_t *sym = (jl_sym_t*)jl_deserialize_value(s, NULL);
        if (internal) {
            tn->module = m;
            tn->name = sym;
            tn->names = (jl_svec_t*)jl_deserialize_value(s, (jl_value_t**)&tn->names);
            jl_gc_wb(tn, tn->names);
            tn->wrapper = jl_deserialize_value(s, &tn->wrapper);
            jl_gc_wb(tn, tn->wrapper);
            tn->mt = (jl_methtable_t*)jl_deserialize_value(s, (jl_value_t**)&tn->mt);
            jl_gc_wb(tn, tn->mt);
            ios_read(s->s, (char*)&tn->hash, sizeof(tn->hash));
        }
        else {
            jl_datatype_t *dt = (jl_datatype_t*)jl_unwrap_unionall(jl_get_global(m, sym));
            assert(jl_is_datatype(dt));
            tn = dt->name;
            if (usetable)
                backref_list.items[pos] = tn;
        }
        return (jl_value_t*)tn;
    }
    jl_set_typeof(v, dt);
    if ((jl_value_t*)dt == jl_bigint_type) {
        jl_value_t *sizefield = jl_deserialize_value(s, NULL);
        int32_t sz = jl_unbox_int32(sizefield);
        int32_t nw = (sz == 0 ? 1 : (sz < 0 ? -sz : sz));
        size_t nb = nw * gmp_limb_size;
        void *buf = jl_gc_counted_malloc(nb);
        if (buf == NULL)
            jl_throw(jl_memory_exception);
        ios_readall(s->s, (char*)buf, nb);
        jl_set_nth_field(v, 0, jl_box_int32(nw));
        jl_set_nth_field(v, 1, sizefield);
        jl_set_nth_field(v, 2, jl_box_voidpointer(buf));
    }
    else {
        jl_deserialize_struct(s, v);
    }
    return v;
}

static jl_value_t *jl_deserialize_value(jl_serializer_state *s, jl_value_t **loc) JL_GC_DISABLED
{
    assert(!ios_eof(s->s));
    jl_value_t *v;
    size_t i, n;
    uintptr_t pos;
    uint8_t tag = read_uint8(s->s);
    if (tag > LAST_TAG)
        return deser_tag[tag];
    int usetable = (s->mode != MODE_IR);
    switch (tag) {
    case TAG_NULL: return NULL;
    case 0:
        tag = read_uint8(s->s);
        return deser_tag[tag];
    case TAG_BACKREF: JL_FALLTHROUGH; case TAG_SHORT_BACKREF:
        assert(s->method == NULL && s->mode != MODE_IR);
        uintptr_t offs = (tag == TAG_BACKREF) ? read_int32(s->s) : read_uint16(s->s);
        int isflagref = 0;
        isflagref = !!(offs & 1);
        offs >>= 1;
        // assert(offs >= 0); // offs is unsigned so this is always true
        assert(offs < backref_list.len);
        jl_value_t *bp = (jl_value_t*)backref_list.items[offs];
        assert(bp);
        if (isflagref && loc != HT_NOTFOUND) {
            if (loc != NULL) {
                // as in jl_deserialize_value_singleton, the caller won't have a place to
                // store this reference given a field type like Union{Int,Nothing}
                arraylist_push(&flagref_list, loc);
                arraylist_push(&flagref_list, (void*)(uintptr_t)-1);
            }
        }
        return (jl_value_t*)bp;
    case TAG_METHODROOT:
        return jl_array_ptr_ref(s->method->roots, read_uint8(s->s));
    case TAG_LONG_METHODROOT:
        return jl_array_ptr_ref(s->method->roots, read_uint16(s->s));
    case TAG_SVEC: JL_FALLTHROUGH; case TAG_LONG_SVEC:
        return jl_deserialize_value_svec(s, tag);
    case TAG_COMMONSYM:
        return deser_symbols[read_uint8(s->s)];
    case TAG_SYMBOL: JL_FALLTHROUGH; case TAG_LONG_SYMBOL:
        return jl_deserialize_value_symbol(s, tag);
    case TAG_SSAVALUE:
        v = jl_box_ssavalue(read_uint8(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    case TAG_LONG_SSAVALUE:
        v = jl_box_ssavalue(read_uint16(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    case TAG_SLOTNUMBER:
        v = jl_box_slotnumber(read_uint16(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    case TAG_ARRAY: JL_FALLTHROUGH; case TAG_ARRAY1D:
        return jl_deserialize_value_array(s, tag);
    case TAG_EXPR:      JL_FALLTHROUGH;
    case TAG_LONG_EXPR: JL_FALLTHROUGH;
    case TAG_CALL1:     JL_FALLTHROUGH;
    case TAG_CALL2:
        return jl_deserialize_value_expr(s, tag);
    case TAG_PHINODE: JL_FALLTHROUGH; case TAG_LONG_PHINODE:
        return jl_deserialize_value_phi(s, tag);
    case TAG_PHICNODE: JL_FALLTHROUGH; case TAG_LONG_PHICNODE:
        return jl_deserialize_value_phic(s, tag);
    case TAG_GOTONODE: JL_FALLTHROUGH; case TAG_QUOTENODE:
        v = jl_new_struct_uninit(tag == TAG_GOTONODE ? jl_gotonode_type : jl_quotenode_type);
        if (usetable)
            arraylist_push(&backref_list, v);
        set_nth_field(tag == TAG_GOTONODE ? jl_gotonode_type : jl_quotenode_type, (void*)v, 0, jl_deserialize_value(s, NULL));
        return v;
    case TAG_UNIONALL:
        pos = backref_list.len;
        if (usetable)
            arraylist_push(&backref_list, NULL);
        if (read_uint8(s->s)) {
            jl_module_t *m = (jl_module_t*)jl_deserialize_value(s, NULL);
            jl_sym_t *sym = (jl_sym_t*)jl_deserialize_value(s, NULL);
            jl_value_t *v = jl_get_global(m, sym);
            assert(jl_is_unionall(v));
            if (usetable)
                backref_list.items[pos] = v;
            return v;
        }
        v = jl_gc_alloc(s->ptls, sizeof(jl_unionall_t), jl_unionall_type);
        if (usetable)
            backref_list.items[pos] = v;
        ((jl_unionall_t*)v)->var = (jl_tvar_t*)jl_deserialize_value(s, (jl_value_t**)&((jl_unionall_t*)v)->var);
        jl_gc_wb(v, ((jl_unionall_t*)v)->var);
        ((jl_unionall_t*)v)->body = jl_deserialize_value(s, &((jl_unionall_t*)v)->body);
        jl_gc_wb(v, ((jl_unionall_t*)v)->body);
        return v;
    case TAG_TVAR:
        v = jl_gc_alloc(s->ptls, sizeof(jl_tvar_t), jl_tvar_type);
        jl_tvar_t *tv = (jl_tvar_t*)v;
        if (usetable)
            arraylist_push(&backref_list, tv);
        tv->name = (jl_sym_t*)jl_deserialize_value(s, NULL);
        jl_gc_wb(tv, tv->name);
        tv->lb = jl_deserialize_value(s, &tv->lb);
        jl_gc_wb(tv, tv->lb);
        tv->ub = jl_deserialize_value(s, &tv->ub);
        jl_gc_wb(tv, tv->ub);
        return (jl_value_t*)tv;
    case TAG_METHOD:
        return jl_deserialize_value_method(s, loc);
    case TAG_METHOD_INSTANCE:
        return jl_deserialize_value_method_instance(s, loc);
    case TAG_CODE_INSTANCE:
        return jl_deserialize_value_code_instance(s, loc);
    case TAG_MODULE:
        return jl_deserialize_value_module(s);
    case TAG_SHORTER_INT64:
        v = jl_box_int64((int16_t)read_uint16(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    case TAG_SHORT_INT64:
        v = jl_box_int64(read_int32(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    case TAG_INT64:
        v = jl_box_int64((int64_t)read_uint64(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    case TAG_SHORT_INT32:
        v = jl_box_int32((int16_t)read_uint16(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    case TAG_INT32:
        v = jl_box_int32(read_int32(s->s));
        if (usetable)
            arraylist_push(&backref_list, v);
        return v;
    case TAG_UINT8:
        return jl_box_uint8(read_uint8(s->s));
    case TAG_NEARBYGLOBAL:
        assert(s->method != NULL);
        v = jl_deserialize_value(s, NULL);
        return jl_module_globalref(s->method->module, (jl_sym_t*)v);
    case TAG_NEARBYMODULE:
        assert(s->method != NULL);
        return (jl_value_t*)s->method->module;
    case TAG_GLOBALREF:
        return jl_deserialize_value_globalref(s);
    case TAG_SINGLETON:
        return jl_deserialize_value_singleton(s, loc);
    case TAG_CORE:
        return (jl_value_t*)jl_core_module;
    case TAG_BASE:
        return (jl_value_t*)jl_base_module;
    case TAG_VECTORTY:
        v = jl_deserialize_value(s, NULL);
        return jl_apply_type2((jl_value_t*)jl_array_type, v, jl_box_long(1));
    case TAG_PTRTY:
        v = jl_deserialize_value(s, NULL);
        return jl_apply_type1((jl_value_t*)jl_pointer_type, v);
    case TAG_CNULL:
        v = jl_gc_alloc(s->ptls, sizeof(void*), NULL);
        jl_set_typeof(v, (void*)(intptr_t)0x50);
        *(void**)v = NULL;
        uintptr_t pos = backref_list.len;
        if (usetable)
            arraylist_push(&backref_list, v);
        jl_set_typeof(v, jl_deserialize_value(s, &jl_astaggedvalue(v)->type));
        return v;
    case TAG_BITYPENAME:
        v = deser_tag[read_uint8(s->s)];
        return (jl_value_t*)((jl_datatype_t*)jl_unwrap_unionall(v))->name;
    case TAG_STRING:
        n = read_int32(s->s);
        v = jl_alloc_string(n);
        if (usetable)
            arraylist_push(&backref_list, v);
        ios_readall(s->s, jl_string_data(v), n);
        return v;
    case TAG_LINEINFO:
        v = jl_new_struct_uninit(jl_lineinfonode_type);
        if (usetable)
            arraylist_push(&backref_list, v);
        for (i = 0; i < jl_datatype_nfields(jl_lineinfonode_type); i++) {
            size_t offs = jl_field_offset(jl_lineinfonode_type, i);
            set_nth_field(jl_lineinfonode_type, (void*)v, i, jl_deserialize_value(s, (jl_value_t**)((char*)v + offs)));
        }
        return v;
    case TAG_DATATYPE:
        pos = backref_list.len;
        if (usetable)
            arraylist_push(&backref_list, NULL);
        return jl_deserialize_datatype(s, pos, loc);
    default:
        assert(tag == TAG_GENERAL || tag == TAG_SHORT_GENERAL);
        return jl_deserialize_value_any(s, tag, loc);
    }
}

static void jl_insert_methods(jl_array_t *list)
{
    size_t i, l = jl_array_len(list);
    for (i = 0; i < l; i += 2) {
        jl_method_t *meth = (jl_method_t*)jl_array_ptr_ref(list, i);
        jl_tupletype_t *simpletype = (jl_tupletype_t*)jl_array_ptr_ref(list, i + 1);
        assert(jl_is_method(meth));
        jl_methtable_t *mt = jl_method_table_for((jl_value_t*)meth->sig);
        assert((jl_value_t*)mt != jl_nothing);
        jl_method_table_insert(mt, meth, simpletype);
    }
}

extern int jl_debug_method_invalidation;

// verify that these edges intersect with the same methods as before
static void jl_verify_edges(jl_array_t *targets, jl_array_t **pvalids)
{
    size_t i, l = jl_array_len(targets) / 2;
    jl_array_t *valids = jl_alloc_array_1d(jl_array_uint8_type, l);
    memset(jl_array_data(valids), 1, l);
    *pvalids = valids;
    for (i = 0; i < l; i++) {
        jl_value_t *callee = jl_array_ptr_ref(targets, i * 2);
        jl_method_instance_t *callee_mi = (jl_method_instance_t*)callee;
        jl_value_t *sig;
        if (jl_is_method_instance(callee)) {
            sig = callee_mi->specTypes;
        }
        else {
            sig = callee;
        }
        jl_array_t *expected = (jl_array_t*)jl_array_ptr_ref(targets, i * 2 + 1);
        assert(jl_is_array(expected));
        int valid = 1;
        size_t min_valid = 0;
        size_t max_valid = ~(size_t)0;
        jl_value_t *matches = jl_matching_methods((jl_tupletype_t*)sig, -1, 0, jl_world_counter, &min_valid, &max_valid);
        if (matches == jl_false || jl_array_len(matches) != jl_array_len(expected)) {
            valid = 0;
        }
        else {
            size_t j, k, l = jl_array_len(expected);
            for (k = 0; k < jl_array_len(matches); k++) {
                jl_method_t *m = (jl_method_t*)jl_svecref(jl_array_ptr_ref(matches, k), 2);
                for (j = 0; j < l; j++) {
                    if (m == (jl_method_t*)jl_array_ptr_ref(expected, j))
                        break;
                }
                if (j == l) {
                    // intersection has a new method or a method was
                    // deleted--this is now probably no good, just invalidate
                    // everything about it now
                    valid = 0;
                    break;
                }
            }
        }
        jl_array_uint8_set(valids, i, valid);
    }
}

static void jl_insert_backedges(jl_array_t *list, jl_array_t *targets)
{
    // map(enable, ((list[i] => targets[list[i + 1] .* 2]) for i in 1:2:length(list) if all(valids[list[i + 1]])))
    size_t i, l = jl_array_len(list);
    jl_array_t *valids = NULL;
    JL_GC_PUSH1(&valids);
    jl_verify_edges(targets, &valids);
    for (i = 0; i < l; i += 2) {
        jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(list, i);
        assert(jl_is_method_instance(caller) && jl_is_method(caller->def.method));
        assert(caller->def.method->primary_world == jl_world_counter); // caller should be new
        jl_array_t *idxs_array = (jl_array_t*)jl_array_ptr_ref(list, i + 1);
        assert(jl_isa((jl_value_t*)idxs_array, jl_array_int32_type));
        int32_t *idxs = (int32_t*)jl_array_data(idxs_array);
        int valid = 1;
        size_t j;
        for (j = 0; valid && j < jl_array_len(idxs_array); j++) {
            int32_t idx = idxs[j];
            valid = jl_array_uint8_ref(valids, idx);
        }
        if (valid) {
            // if this callee is still valid, add all the backedges
            for (j = 0; j < jl_array_len(idxs_array); j++) {
                int32_t idx = idxs[j];
                jl_value_t *callee = jl_array_ptr_ref(targets, idx * 2);
                if (jl_is_method_instance(callee)) {
                    jl_method_instance_add_backedge((jl_method_instance_t*)callee, caller);
                }
                else {
                    jl_methtable_t *mt = jl_method_table_for(callee);
                    assert((jl_value_t*)mt != jl_nothing);
                    jl_method_table_add_backedge(mt, callee, (jl_value_t*)caller);
                }
            }
            // then enable it
            jl_code_instance_t *codeinst = caller->cache;
            while (codeinst) {
                if (codeinst->min_world > 0)
                    codeinst->max_world = ~(size_t)0;
                codeinst = codeinst->next;
            }
        }
        else {
            if (jl_debug_method_invalidation) {
                jl_static_show(JL_STDOUT, (jl_value_t*)caller);
                jl_uv_puts(JL_STDOUT, "<<<\n", 4);
            }
        }
    }
    JL_GC_POP();
}


static jl_value_t *read_verify_mod_list(ios_t *s, jl_array_t *mod_list)
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
        ios_readall(s, name, len);
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

static void jl_reinit_item(jl_value_t *v, int how, arraylist_t *tracee_list)
{
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
                if (tracee_list)
                    arraylist_push(tracee_list, mt);
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
        jl_static_show(JL_STDERR, jl_current_exception());
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

JL_DLLEXPORT void jl_init_restored_modules(jl_array_t *init_order)
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

JL_DLLEXPORT jl_array_t *jl_compress_ir(jl_method_t *m, jl_code_info_t *code)
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
        &dest, MODE_IR,
        m,
        jl_get_ptls_states(),
        NULL
    };

    uint8_t flags = (code->inferred << 3)
                  | (code->inlineable << 2)
                  | (code->propagate_inbounds << 1)
                  | (code->pure << 0);
    write_uint8(s.s, flags);

    size_t nslots = jl_array_len(code->slotflags);
    assert(nslots >= m->nargs && nslots < INT32_MAX); // required by generated functions
    write_int32(s.s, nslots);
    ios_write(s.s, (char*)jl_array_data(code->slotflags), nslots);

    // N.B.: The layout of everything before this point is explicitly referenced
    // by the various jl_ir_ accessors. Make sure to adjust those if you change
    // the data layout.

    for (i = 0; i < 6; i++) {
        int copy = 1;
        if (i == 1) { // skip codelocs
            assert(jl_field_offset(jl_code_info_type, i) == offsetof(jl_code_info_t, codelocs));
            continue;
        }
        if (i == 4) { // don't copy contents of method_for_inference_limit_heuristics field
            assert(jl_field_offset(jl_code_info_type, i) == offsetof(jl_code_info_t, method_for_inference_limit_heuristics));
            copy = 0;
        }
        jl_serialize_value_(&s, jl_get_nth_field((jl_value_t*)code, i), copy);
    }

    if (m->generator)
        // can't optimize generated functions
        jl_serialize_value_(&s, (jl_value_t*)jl_compress_argnames(code->slotnames), 1);
    else
        jl_serialize_value(&s, jl_nothing);

    size_t nstmt = jl_array_len(code->code);
    assert(nstmt == jl_array_len(code->codelocs));
    if (jl_array_len(code->linetable) < 256) {
        for (i = 0; i < nstmt; i++) {
            write_uint8(s.s, ((int32_t*)jl_array_data(code->codelocs))[i]);
        }
    }
    else if (jl_array_len(code->linetable) < 65536) {
        for (i = 0; i < nstmt; i++) {
            write_uint16(s.s, ((int32_t*)jl_array_data(code->codelocs))[i]);
        }
    }
    else {
        ios_write(s.s, (char*)jl_array_data(code->codelocs), nstmt * sizeof(int32_t));
    }

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

JL_DLLEXPORT jl_code_info_t *jl_uncompress_ir(jl_method_t *m, jl_code_instance_t *metadata, jl_array_t *data)
{
    if (jl_is_code_info(data))
        return (jl_code_info_t*)data;
    JL_TIMING(AST_UNCOMPRESS);
    JL_LOCK(&m->writelock); // protect the roots array (Might GC)
    assert(jl_is_method(m));
    assert(jl_typeis(data, jl_array_uint8_type));
    size_t i;
    ios_t src;
    ios_mem(&src, 0);
    ios_setbuf(&src, (char*)data->data, jl_array_len(data), 0);
    src.size = jl_array_len(data);
    int en = jl_gc_enable(0); // Might GC
    jl_serializer_state s = {
        &src, MODE_IR,
        m,
        jl_get_ptls_states(),
        NULL
    };

    jl_code_info_t *code = jl_new_code_info_uninit();
    uint8_t flags = read_uint8(s.s);
    code->inferred = !!(flags & (1 << 3));
    code->inlineable = !!(flags & (1 << 2));
    code->propagate_inbounds = !!(flags & (1 << 1));
    code->pure = !!(flags & (1 << 0));

    size_t nslots = read_int32(&src);
    code->slotflags = jl_alloc_array_1d(jl_array_uint8_type, nslots);
    ios_readall(s.s, (char*)jl_array_data(code->slotflags), nslots);

    for (i = 0; i < 6; i++) {
        if (i == 1)  // skip codelocs
            continue;
        assert(jl_field_isptr(jl_code_info_type, i));
        jl_value_t **fld = (jl_value_t**)((char*)jl_data_ptr(code) + jl_field_offset(jl_code_info_type, i));
        *fld = jl_deserialize_value(&s, fld);
    }

    jl_value_t *slotnames = jl_deserialize_value(&s, NULL);
    if (!jl_is_string(slotnames))
        slotnames = m->slot_syms;
    code->slotnames = jl_uncompress_argnames(slotnames);

    size_t nstmt = jl_array_len(code->code);
    code->codelocs = (jl_value_t*)jl_alloc_array_1d(jl_array_int32_type, nstmt);
    if (jl_array_len(code->linetable) < 256) {
        for (i = 0; i < nstmt; i++) {
            ((int32_t*)jl_array_data(code->codelocs))[i] = read_uint8(s.s);
        }
    }
    else if (jl_array_len(code->linetable) < 65536) {
        for (i = 0; i < nstmt; i++) {
            ((int32_t*)jl_array_data(code->codelocs))[i] = read_uint16(s.s);
        }
    }
    else {
        ios_readall(s.s, (char*)jl_array_data(code->codelocs), nstmt * sizeof(int32_t));
    }

    assert(ios_getc(s.s) == -1);
    ios_close(s.s);
    JL_GC_PUSH1(&code);
    jl_gc_enable(en);
    JL_UNLOCK(&m->writelock); // Might GC
    JL_GC_POP();
    if (metadata) {
        code->min_world = metadata->min_world;
        code->max_world = metadata->max_world;
        code->rettype = metadata->rettype;
        code->parent = metadata->def;
    }
    return code;
}

JL_DLLEXPORT uint8_t jl_ir_flag_inferred(jl_array_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->inferred;
    assert(jl_typeis(data, jl_array_uint8_type));
    uint8_t flags = ((uint8_t*)data->data)[0];
    return !!(flags & (1 << 3));
}

JL_DLLEXPORT uint8_t jl_ir_flag_inlineable(jl_array_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->inlineable;
    assert(jl_typeis(data, jl_array_uint8_type));
    uint8_t flags = ((uint8_t*)data->data)[0];
    return !!(flags & (1 << 2));
}

JL_DLLEXPORT uint8_t jl_ir_flag_pure(jl_array_t *data)
{
    if (jl_is_code_info(data))
        return ((jl_code_info_t*)data)->pure;
    assert(jl_typeis(data, jl_array_uint8_type));
    uint8_t flags = ((uint8_t*)data->data)[0];
    return !!(flags & (1 << 0));
}

JL_DLLEXPORT jl_value_t *jl_compress_argnames(jl_array_t *syms)
{
    size_t nsyms = jl_array_len(syms);
    size_t i, len = 0;
    for (i = 0; i < nsyms; i++) {
        jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(syms, i);
        assert(jl_is_symbol(name));
        char *namestr = jl_symbol_name(name);
        size_t namelen = strlen(namestr) + 1;
        len += namelen;
    }
    jl_value_t *str = jl_alloc_string(len);
    len = 0;
    for (i = 0; i < nsyms; i++) {
        jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(syms, i);
        assert(jl_is_symbol(name));
        char *namestr = jl_symbol_name(name);
        size_t namelen = strlen(namestr) + 1; // include nul-byte
        assert(len + namelen <= jl_string_len(str));
        memcpy(jl_string_data(str) + len, namestr, namelen);
        len += namelen;
    }
    assert(len == jl_string_len(str));
    return str;
}

JL_DLLEXPORT ssize_t jl_ir_nslots(jl_array_t *data)
{
    if (jl_is_code_info(data)) {
        jl_code_info_t *func = (jl_code_info_t*)data;
        return jl_array_len(func->slotnames);
    }
    else {
        assert(jl_typeis(data, jl_array_uint8_type));
        int nslots = jl_load_unaligned_i32((char*)data->data + 1);
        return nslots;
    }
}

JL_DLLEXPORT uint8_t jl_ir_slotflag(jl_array_t *data, size_t i)
{
    assert(i < jl_ir_nslots(data));
    if (jl_is_code_info(data))
        return ((uint8_t*)((jl_code_info_t*)data)->slotflags->data)[i];
    assert(jl_typeis(data, jl_array_uint8_type));
    return ((uint8_t*)data->data)[1 + sizeof(int32_t) + i];
}

JL_DLLEXPORT jl_array_t *jl_uncompress_argnames(jl_value_t *syms)
{
    assert(jl_is_string(syms));
    char *namestr;
    namestr = jl_string_data(syms);
    size_t remaining = jl_string_len(syms);
    size_t i, len = 0;
    while (remaining) {
        size_t namelen = strlen(namestr);
        len += 1;
        namestr += namelen + 1;
        remaining -= namelen + 1;
    }
    namestr = jl_string_data(syms);
    jl_array_t *names = jl_alloc_array_1d(jl_array_symbol_type, len);
    JL_GC_PUSH1(&names);
    for (i = 0; i < len; i++) {
        size_t namelen = strlen(namestr);
        jl_sym_t *name = jl_symbol_n(namestr, namelen);
        jl_array_ptr_set(names, i, name);
        namestr += namelen + 1;
    }
    JL_GC_POP();
    return names;
}

JL_DLLEXPORT jl_value_t *jl_uncompress_argname_n(jl_value_t *syms, size_t i)
{
    assert(jl_is_string(syms));
    char *namestr = jl_string_data(syms);
    size_t remaining = jl_string_len(syms);
    while (remaining) {
        size_t namelen = strlen(namestr);
        if (i-- == 0) {
            jl_sym_t *name = jl_symbol_n(namestr, namelen);
            return (jl_value_t*)name;
        }
        namestr += namelen + 1;
        remaining -= namelen + 1;
    }
    return jl_nothing;
}


JL_DLLEXPORT int jl_save_incremental(const char *fname, jl_array_t *worklist)
{
    JL_TIMING(SAVE_MODULE);
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
    jl_bigint_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("BigInt")) : NULL;
    if (jl_bigint_type) {
        gmp_limb_size = jl_unbox_long(jl_get_global((jl_module_t*)jl_get_global(jl_base_module, jl_symbol("GMP")),
                                                    jl_symbol("BITS_PER_LIMB"))) / 8;
    }

    int en = jl_gc_enable(0); // edges map is not gc-safe
    jl_array_t *lambdas = jl_alloc_vec_any(0);
    jl_array_t *edges = jl_alloc_vec_any(0);
    jl_array_t *targets = jl_alloc_vec_any(0);

    size_t i;
    size_t len = jl_array_len(mod_array);
    for (i = 0; i < len; i++) {
        jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(mod_array, i);
        assert(jl_is_module(m));
        if (m->parent == m) // some toplevel modules (really just Base) aren't actually
            jl_collect_lambdas_from_mod(lambdas, m);
    }
    jl_collect_methtable_from_mod(lambdas, jl_type_type_mt);
    jl_collect_missing_backedges_to_mod(jl_type_type_mt);
    jl_collect_methtable_from_mod(lambdas, jl_nonfunction_mt);
    jl_collect_missing_backedges_to_mod(jl_nonfunction_mt);

    jl_collect_backedges(edges, targets);

    jl_serializer_state s = {
        &f, MODE_MODULE,
        NULL,
        jl_get_ptls_states(),
        mod_array
    };
    jl_serialize_value(&s, worklist);
    jl_serialize_value(&s, lambdas);
    jl_serialize_value(&s, edges);
    jl_serialize_value(&s, targets);
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
STATIC_INLINE jl_value_t *verify_type(jl_value_t *v) JL_NOTSAFEPOINT
{
    assert(v && jl_typeof(v) && jl_typeof(jl_typeof(v)) == (jl_value_t*)jl_datatype_type);
    return v;
}
#endif

jl_datatype_t *jl_lookup_cache_type_(jl_datatype_t *type);
void jl_cache_type_(jl_datatype_t *type);

static jl_datatype_t *jl_recache_type(jl_datatype_t *dt) JL_GC_DISABLED
{
    jl_datatype_t *t; // the type after unique'ing
    assert(verify_type((jl_value_t*)dt));
    t = (jl_datatype_t*)ptrhash_get(&uniquing_table, dt);
    if (t == HT_NOTFOUND)
        return dt;
    if (t != NULL)
        return t;

    jl_svec_t *tt = dt->parameters;
    // recache all type parameters
    size_t i, l = jl_svec_len(tt);
    for (i = 0; i < l; i++) {
        jl_datatype_t *p = (jl_datatype_t*)jl_svecref(tt, i);
        if (jl_is_datatype(p)) {
            jl_datatype_t *cachep = jl_recache_type(p);
            if (p != cachep)
                jl_svecset(tt, i, cachep);
        }
        // XXX: else if (jl_is_typevar(p))
        // XXX: else if (jl_is_uniontype(p))
        // XXX: else if (jl_is_unionall(p))
        else {
            p = (jl_datatype_t*)jl_typeof(p);
            jl_datatype_t *cachep = jl_recache_type(p);
            if (p != cachep) {
                if (cachep->instance)
                    jl_svecset(tt, i, cachep->instance);
                else
                    jl_set_typeof(jl_svecref(tt, i), cachep);
            }
        }
    }

    // then recache the type itself
    if (jl_svec_len(tt) == 0) { // jl_cache_type doesn't work if length(parameters) == 0
        t = dt;
    }
    else {
        t = jl_lookup_cache_type_(dt);
        if (t == NULL) {
            jl_cache_type_(dt);
            t = dt;
        }
        assert(t->hash == dt->hash);
        assert(jl_invalid_types_equal(t, dt));
    }
    ptrhash_put(&uniquing_table, dt, t);
    return t;
}

static void jl_recache_types(void) JL_GC_DISABLED
{
    size_t i;
    // first rewrite all the unique'd objects
    for (i = 0; i < flagref_list.len; i += 2) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i + 0];
        int offs = (int)(intptr_t)flagref_list.items[i + 1];
        jl_value_t *o = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        if (!jl_is_method(o) && !jl_is_method_instance(o)) {
            jl_datatype_t *dt;
            jl_value_t *v;
            if (jl_is_datatype(o)) {
                dt = (jl_datatype_t*)o;
                v = dt->instance;
            }
            else {
                dt = (jl_datatype_t*)jl_typeof(o);
                v = o;
            }
            jl_datatype_t *t = jl_recache_type(dt);
            if ((jl_value_t*)dt == o && t != dt) {
                assert(!type_in_worklist(dt));
                if (loc)
                    *loc = (jl_value_t*)t;
                if (offs > 0)
                    backref_list.items[offs] = t;
            }
            if (v == o && t->instance != v) {
                assert(t->instance);
                assert(loc);
                *loc = t->instance;
                if (offs > 0)
                    backref_list.items[offs] = t->instance;
            }
        }
    }
    // invalidate the old datatypes to help catch errors
    for (i = 0; i < uniquing_table.size; i += 2) {
        jl_datatype_t *o = (jl_datatype_t*)uniquing_table.table[i];
        jl_datatype_t *t = (jl_datatype_t*)uniquing_table.table[i + 1];
        if (o != t) {
            assert(t != NULL && jl_is_datatype(o));
            if (t->instance != o->instance)
                jl_set_typeof(o->instance, (void*)(intptr_t)0x20);
            jl_set_typeof(o, (void*)(intptr_t)0x10);
        }
    }
    // then do a cleanup pass to drop these from future iterations of flagref_list
    i = 0;
    while (i < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i + 0];
        int offs = (int)(intptr_t)flagref_list.items[i + 1];
        jl_value_t *o = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        if (jl_is_method(o) || jl_is_method_instance(o)) {
            i += 2;
        }
        else {
            // delete this item from the flagref list, so it won't be re-encountered later
            flagref_list.len -= 2;
            if (i >= flagref_list.len)
                break;
            flagref_list.items[i + 0] = flagref_list.items[flagref_list.len + 0];
            flagref_list.items[i + 1] = flagref_list.items[flagref_list.len + 1];
        }
    }
}

// repeatedly look up older methods until we come to one that existed
// at the time this module was serialized (e.g. ignoring deletion)
static jl_method_t *jl_lookup_method(jl_methtable_t *mt, jl_datatype_t *sig, size_t world)
{
    if (world < jl_main_module->primary_world)
        world = jl_main_module->primary_world;
    struct jl_typemap_assoc search = {(jl_value_t*)sig, world, NULL, 0, ~(size_t)0};
    jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(mt->defs, &search, /*offs*/0, /*subtype*/0);
    return (jl_method_t*)entry->func.value;
}

static jl_method_t *jl_recache_method(jl_method_t *m)
{
    jl_datatype_t *sig = (jl_datatype_t*)m->sig;
    jl_methtable_t *mt = jl_method_table_for((jl_value_t*)m->sig);
    assert((jl_value_t*)mt != jl_nothing);
    jl_set_typeof(m, (void*)(intptr_t)0x30); // invalidate the old value to help catch errors
    jl_method_t *_new = jl_lookup_method(mt, sig, m->module->primary_world);
    return _new;
}

static jl_value_t *jl_recache_other_(jl_value_t *o);

static jl_method_instance_t *jl_recache_method_instance(jl_method_instance_t *mi)
{
    jl_method_t *m = mi->def.method;
    m = (jl_method_t*)jl_recache_other_((jl_value_t*)m);
    assert(jl_is_method(m));
    jl_datatype_t *argtypes = (jl_datatype_t*)mi->specTypes;
    jl_set_typeof(mi, (void*)(intptr_t)0x40); // invalidate the old value to help catch errors
    jl_svec_t *env = jl_emptysvec;
    jl_value_t *ti = jl_type_intersection_env((jl_value_t*)argtypes, (jl_value_t*)m->sig, &env);
    //assert(ti != jl_bottom_type); (void)ti;
    if (ti == jl_bottom_type)
        env = jl_emptysvec; // the intersection may fail now if the type system had made an incorrect subtype env in the past
    jl_method_instance_t *_new = jl_specializations_get_linfo(m, (jl_value_t*)argtypes, env);
    return _new;
}

static jl_value_t *jl_recache_other_(jl_value_t *o)
{
    jl_value_t *newo = (jl_value_t*)ptrhash_get(&uniquing_table, o);
    if (newo != HT_NOTFOUND)
        return newo;
    if (jl_is_method(o)) {
        // lookup the real Method based on the placeholder sig
        newo = (jl_value_t*)jl_recache_method((jl_method_t*)o);
        ptrhash_put(&uniquing_table, newo, newo);
    }
    else if (jl_is_method_instance(o)) {
        // lookup the real MethodInstance based on the placeholder specTypes
        newo = (jl_value_t*)jl_recache_method_instance((jl_method_instance_t*)o);
    }
    else {
        abort();
    }
    ptrhash_put(&uniquing_table, o, newo);
    return newo;
}

static void jl_recache_other(void)
{
    size_t i = 0;
    while (i < flagref_list.len) {
        jl_value_t **loc = (jl_value_t**)flagref_list.items[i + 0];
        int offs = (int)(intptr_t)flagref_list.items[i + 1];
        jl_value_t *o = loc ? *loc : (jl_value_t*)backref_list.items[offs];
        i += 2;
        jl_value_t *newo = jl_recache_other_(o);
        if (loc)
            *loc = newo;
        if (offs > 0)
            backref_list.items[offs] = newo;
    }
    flagref_list.len = 0;
}

extern tracer_cb jl_newmeth_tracer;
static int trace_method(jl_typemap_entry_t *entry, void *closure)
{
    jl_call_tracer(jl_newmeth_tracer, (jl_value_t*)entry->func.method);
    return 1;
}

static jl_value_t *_jl_restore_incremental(ios_t *f, jl_array_t *mod_array)
{
    JL_TIMING(LOAD_MODULE);
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

    jl_bigint_type = jl_base_module ? jl_get_global(jl_base_module, jl_symbol("BigInt")) : NULL;
    if (jl_bigint_type) {
        gmp_limb_size = jl_unbox_long(jl_get_global((jl_module_t*)jl_get_global(jl_base_module, jl_symbol("GMP")),
                                                    jl_symbol("BITS_PER_LIMB"))) / 8;
    }

    // verify that the system state is valid
    jl_value_t *verify_fail = read_verify_mod_list(f, mod_array);
    if (verify_fail) {
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
    htable_new(&uniquing_table, 0);

    jl_serializer_state s = {
        f, MODE_MODULE,
        NULL,
        ptls,
        mod_array
    };
    jl_array_t *restored = (jl_array_t*)jl_deserialize_value(&s, (jl_value_t**)&restored);
    serializer_worklist = restored;
    assert(jl_isa((jl_value_t*)restored, jl_array_any_type));

    // get list of external generic functions
    jl_value_t *external_methods = jl_deserialize_value(&s, &external_methods);
    jl_value_t *external_backedges = jl_deserialize_value(&s, &external_backedges);
    jl_value_t *external_edges = jl_deserialize_value(&s, &external_edges);

    arraylist_t *tracee_list = NULL;
    if (jl_newmeth_tracer)
        tracee_list = arraylist_new((arraylist_t*)malloc_s(sizeof(arraylist_t)), 0);

    // at this point, the AST is fully reconstructed, but still completely disconnected
    // now all of the interconnects will be created
    jl_recache_types(); // make all of the types identities correct
    htable_reset(&uniquing_table, 0);
    jl_insert_methods((jl_array_t*)external_methods); // hook up methods of external generic functions (needs to be after recache types)
    jl_recache_other(); // make all of the other objects identities correct (needs to be after insert methods)
    htable_free(&uniquing_table);
    jl_array_t *init_order = jl_finalize_deserializer(&s, tracee_list); // done with f and s (needs to be after recache)

    JL_GC_PUSH4(&init_order, &restored, &external_backedges, &external_edges);
    jl_gc_enable(en); // subtyping can allocate a lot, not valid before recache-other

    jl_insert_backedges((jl_array_t*)external_backedges, (jl_array_t*)external_edges); // restore external backedges (needs to be last)

    serializer_worklist = NULL;
    arraylist_free(&flagref_list);
    arraylist_free(&backref_list);
    ios_close(f);

    jl_gc_enable_finalizers(ptls, 1); // make sure we don't run any Julia code concurrently before this point
    if (tracee_list) {
        jl_methtable_t *mt;
        while ((mt = (jl_methtable_t*)arraylist_pop(tracee_list)) != NULL) {
            JL_GC_PROMISE_ROOTED(mt);
            jl_typemap_visitor(mt->defs, trace_method, NULL);
        }
        arraylist_free(tracee_list);
        free(tracee_list);
    }
    jl_value_t *ret = (jl_value_t*)jl_svec(2, restored, init_order);
    JL_GC_POP();

    return (jl_value_t*)ret;
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

    void *vals[] = { jl_emptysvec, jl_emptytuple, jl_false, jl_true, jl_nothing, jl_any_type,
                     call_sym, invoke_sym, goto_ifnot_sym, return_sym, jl_symbol("tuple"),
                     unreachable_sym, jl_an_empty_string, jl_an_empty_vec_any,

                     // empirical list of very common symbols
                     #include "common_symbols1.inc"

                     jl_box_int32(0), jl_box_int32(1), jl_box_int32(2),
                     jl_box_int32(3), jl_box_int32(4), jl_box_int32(5),
                     jl_box_int32(6), jl_box_int32(7), jl_box_int32(8),
                     jl_box_int32(9), jl_box_int32(10), jl_box_int32(11),
                     jl_box_int32(12), jl_box_int32(13), jl_box_int32(14),
                     jl_box_int32(15), jl_box_int32(16), jl_box_int32(17),
                     jl_box_int32(18), jl_box_int32(19), jl_box_int32(20),
                     jl_box_int32(21),

                     jl_box_int64(0), jl_box_int64(1), jl_box_int64(2),
                     jl_box_int64(3), jl_box_int64(4), jl_box_int64(5),
                     jl_box_int64(6), jl_box_int64(7), jl_box_int64(8),
                     jl_box_int64(9), jl_box_int64(10), jl_box_int64(11),
                     jl_box_int64(12), jl_box_int64(13), jl_box_int64(14),
                     jl_box_int64(15), jl_box_int64(16), jl_box_int64(17),
                     jl_box_int64(18), jl_box_int64(19), jl_box_int64(20),
                     jl_box_int64(21), jl_box_int64(22),

                     jl_bool_type, jl_linenumbernode_type, jl_pinode_type,
                     jl_upsilonnode_type, jl_type_type, jl_bottom_type, jl_ref_type,
                     jl_pointer_type, jl_vararg_type, jl_abstractarray_type, jl_nothing_type,
                     jl_densearray_type, jl_function_type, jl_typename_type,
                     jl_builtin_type, jl_task_type, jl_uniontype_type, jl_typetype_type,
                     jl_array_any_type, jl_intrinsic_type,
                     jl_abstractslot_type, jl_methtable_type, jl_typemap_level_type,
                     jl_voidpointer_type, jl_newvarnode_type, jl_abstractstring_type,
                     jl_array_symbol_type, jl_anytuple_type, jl_tparam0(jl_anytuple_type),
                     jl_emptytuple_type, jl_array_uint8_type, jl_code_info_type,
                     jl_typeofbottom_type, jl_typeofbottom_type->super,
                     jl_namedtuple_type, jl_array_int32_type,
                     jl_typedslot_type, jl_uint32_type, jl_uint64_type,
                     jl_type_type_mt, jl_nonfunction_mt,

                     ptls->root_task,

                     NULL };

    // more common symbols, less common than those above. will get 2-byte encodings.
    void *common_symbols[] = {
        #include "common_symbols2.inc"
        NULL
    };

    deser_tag[TAG_SYMBOL] = (jl_value_t*)jl_symbol_type;
    deser_tag[TAG_SSAVALUE] = (jl_value_t*)jl_ssavalue_type;
    deser_tag[TAG_DATATYPE] = (jl_value_t*)jl_datatype_type;
    deser_tag[TAG_SLOTNUMBER] = (jl_value_t*)jl_slotnumber_type;
    deser_tag[TAG_SVEC] = (jl_value_t*)jl_simplevector_type;
    deser_tag[TAG_ARRAY] = (jl_value_t*)jl_array_type;
    deser_tag[TAG_EXPR] = (jl_value_t*)jl_expr_type;
    deser_tag[TAG_PHINODE] = (jl_value_t*)jl_phinode_type;
    deser_tag[TAG_PHICNODE] = (jl_value_t*)jl_phicnode_type;
    deser_tag[TAG_STRING] = (jl_value_t*)jl_string_type;
    deser_tag[TAG_MODULE] = (jl_value_t*)jl_module_type;
    deser_tag[TAG_TVAR] = (jl_value_t*)jl_tvar_type;
    deser_tag[TAG_METHOD_INSTANCE] = (jl_value_t*)jl_method_instance_type;
    deser_tag[TAG_METHOD] = (jl_value_t*)jl_method_type;
    deser_tag[TAG_CODE_INSTANCE] = (jl_value_t*)jl_code_instance_type;
    deser_tag[TAG_GLOBALREF] = (jl_value_t*)jl_globalref_type;
    deser_tag[TAG_INT32] = (jl_value_t*)jl_int32_type;
    deser_tag[TAG_INT64] = (jl_value_t*)jl_int64_type;
    deser_tag[TAG_UINT8] = (jl_value_t*)jl_uint8_type;
    deser_tag[TAG_LINEINFO] = (jl_value_t*)jl_lineinfonode_type;
    deser_tag[TAG_UNIONALL] = (jl_value_t*)jl_unionall_type;
    deser_tag[TAG_GOTONODE] = (jl_value_t*)jl_gotonode_type;
    deser_tag[TAG_QUOTENODE] = (jl_value_t*)jl_quotenode_type;

    intptr_t i = 0;
    while (vals[i] != NULL) {
        deser_tag[LAST_TAG+1+i] = (jl_value_t*)vals[i];
        i += 1;
    }
    assert(LAST_TAG+1+i < 256);

    for (i = 2; i < 256; i++) {
        if (deser_tag[i])
            ptrhash_put(&ser_tag, deser_tag[i], (void*)i);
    }

    i = 2;
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
