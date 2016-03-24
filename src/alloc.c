// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  object constructors
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

JL_DLLEXPORT jl_value_t *jl_true;
JL_DLLEXPORT jl_value_t *jl_false;

jl_tvar_t     *jl_typetype_tvar;
jl_datatype_t *jl_typetype_type;
jl_value_t    *jl_ANY_flag;

jl_datatype_t *jl_typector_type;

jl_datatype_t *jl_array_type;
jl_typename_t *jl_array_typename;
jl_value_t *jl_array_uint8_type;
jl_value_t *jl_array_any_type=NULL;
jl_value_t *jl_array_symbol_type;
jl_datatype_t *jl_weakref_type;
jl_datatype_t *jl_ascii_string_type;
jl_datatype_t *jl_utf8_string_type;
jl_datatype_t *jl_expr_type;
jl_datatype_t *jl_globalref_type;
jl_datatype_t *jl_linenumbernode_type;
jl_datatype_t *jl_labelnode_type;
jl_datatype_t *jl_gotonode_type;
jl_datatype_t *jl_quotenode_type;
jl_datatype_t *jl_newvarnode_type;
jl_datatype_t *jl_topnode_type;
jl_datatype_t *jl_intrinsic_type;
jl_datatype_t *jl_methtable_type;
jl_datatype_t *jl_method_type;
jl_datatype_t *jl_lambda_info_type;
jl_datatype_t *jl_module_type;
jl_datatype_t *jl_errorexception_type=NULL;
jl_datatype_t *jl_argumenterror_type;
jl_datatype_t *jl_typeerror_type;
jl_datatype_t *jl_methoderror_type;
jl_datatype_t *jl_loaderror_type;
jl_datatype_t *jl_initerror_type;
jl_datatype_t *jl_undefvarerror_type;
jl_datatype_t *jl_ref_type;
jl_datatype_t *jl_pointer_type;
jl_datatype_t *jl_void_type;
jl_datatype_t *jl_voidpointer_type;
jl_value_t *jl_an_empty_cell=NULL;
jl_value_t *jl_stackovf_exception;
#ifdef SEGV_EXCEPTION
jl_value_t *jl_segv_exception;
#endif
JL_DLLEXPORT jl_value_t *jl_diverror_exception;
JL_DLLEXPORT jl_value_t *jl_domain_exception;
JL_DLLEXPORT jl_value_t *jl_overflow_exception;
JL_DLLEXPORT jl_value_t *jl_inexact_exception;
JL_DLLEXPORT jl_value_t *jl_undefref_exception;
jl_value_t *jl_interrupt_exception;
jl_datatype_t *jl_boundserror_type;
jl_value_t *jl_memory_exception;
jl_value_t *jl_readonlymemory_exception;

jl_sym_t *call_sym;    jl_sym_t *dots_sym;
jl_sym_t *module_sym;  jl_sym_t *slot_sym;
jl_sym_t *export_sym;  jl_sym_t *import_sym;
jl_sym_t *importall_sym; jl_sym_t *toplevel_sym;
jl_sym_t *quote_sym;   jl_sym_t *amp_sym;
jl_sym_t *top_sym;     jl_sym_t *colons_sym;
jl_sym_t *line_sym;    jl_sym_t *jl_incomplete_sym;
// head symbols for each expression type
jl_sym_t *goto_sym;    jl_sym_t *goto_ifnot_sym;
jl_sym_t *label_sym;   jl_sym_t *return_sym;
jl_sym_t *lambda_sym;  jl_sym_t *assign_sym;
jl_sym_t *null_sym;    jl_sym_t *body_sym;
jl_sym_t *method_sym;
jl_sym_t *enter_sym;   jl_sym_t *leave_sym;
jl_sym_t *exc_sym;     jl_sym_t *error_sym;
jl_sym_t *static_typeof_sym;
jl_sym_t *new_sym;     jl_sym_t *using_sym;
jl_sym_t *const_sym;   jl_sym_t *thunk_sym;
jl_sym_t *anonymous_sym;  jl_sym_t *underscore_sym;
jl_sym_t *abstracttype_sym; jl_sym_t *bitstype_sym;
jl_sym_t *compositetype_sym; jl_sym_t *type_goto_sym;
jl_sym_t *global_sym; jl_sym_t *list_sym;
jl_sym_t *dot_sym;    jl_sym_t *newvar_sym;
jl_sym_t *boundscheck_sym; jl_sym_t *inbounds_sym;
jl_sym_t *copyast_sym; jl_sym_t *fastmath_sym;
jl_sym_t *pure_sym; jl_sym_t *simdloop_sym;
jl_sym_t *meta_sym; jl_sym_t *compiler_temp_sym;
jl_sym_t *inert_sym; jl_sym_t *vararg_sym;
jl_sym_t *unused_sym; jl_sym_t *static_parameter_sym;

typedef struct {
    int64_t a;
    int64_t b;
} bits128_t;

// Note that this function updates len
static jl_value_t *jl_new_bits_internal(jl_value_t *dt, void *data, size_t *len)
{
    if (jl_is_ntuple_type(dt)) {
        jl_value_t *lenvar = jl_tparam0(dt);
        jl_value_t *elty = jl_tparam1(dt);
        assert(jl_is_datatype(elty));
        size_t alignment = ((jl_datatype_t*)elty)->alignment;
        *len = LLT_ALIGN((*len), alignment);
        assert(jl_is_long(lenvar));
        size_t l = jl_unbox_long(lenvar);
        size_t nb = l*LLT_ALIGN(jl_datatype_size(elty), alignment);
        jl_value_t *v = (jl_value_t*)newobj(dt, NWORDS(nb));
        memcpy(jl_data_ptr(v), data, nb);
        return v;
    }

    assert(jl_is_datatype(dt));
    jl_datatype_t *bt = (jl_datatype_t*)dt;
    size_t nb = jl_datatype_size(bt);
    if (nb == 0)
        return jl_new_struct_uninit(bt);
    *len = LLT_ALIGN(*len, bt->alignment);
    data = (char*)data + (*len);
    *len += nb;
    if (bt == jl_uint8_type)   return jl_box_uint8(*(uint8_t*)data);
    if (bt == jl_int64_type)   return jl_box_int64(*(int64_t*)data);
    if (bt == jl_bool_type)    return (*(int8_t*)data) ? jl_true:jl_false;
    if (bt == jl_int32_type)   return jl_box_int32(*(int32_t*)data);
    if (bt == jl_float64_type) return jl_box_float64(*(double*)data);

    jl_value_t *v = (jl_value_t*)newobj((jl_value_t*)bt, NWORDS(nb));
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

JL_DLLEXPORT jl_value_t *jl_new_struct(jl_datatype_t *type, ...)
{
    if (type->instance != NULL) return type->instance;
    va_list args;
    size_t nf = jl_datatype_nfields(type);
    va_start(args, type);
    jl_value_t *jv = newstruct(type);
    for(size_t i=0; i < nf; i++) {
        jl_set_nth_field(jv, i, va_arg(args, jl_value_t*));
    }
    va_end(args);
    return jv;
}

JL_DLLEXPORT jl_value_t *jl_new_structv(jl_datatype_t *type, jl_value_t **args,
                                        uint32_t na)
{
    if (type->instance != NULL) return type->instance;
    size_t nf = jl_datatype_nfields(type);
    jl_value_t *jv = newstruct(type);
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
    if (type->instance != NULL) return type->instance;
    jl_value_t *jv = newstruct(type);
    if (type->size > 0)
        memset(jl_data_ptr(jv), 0, type->size);
    return jv;
}

JL_DLLEXPORT void jl_lambda_info_init_properties(jl_lambda_info_t *li)
{
    int i;
    uint8_t called=0;
    for(i=1; i < li->nargs && i <= 8; i++) {
        jl_value_t *ai = jl_cellref(li->slotnames,i);
        if (ai == (jl_value_t*)unused_sym) continue;
        if (jl_array_uint8_ref(li->slotflags,i)&64)
            called |= (1<<(i-1));
    }
    li->called = called;
}

JL_DLLEXPORT void jl_lambda_info_set_ast(jl_lambda_info_t *li, jl_value_t *ast)
{
    assert(jl_is_expr(ast));
    jl_array_t *body = jl_lam_body((jl_expr_t*)ast)->args;
    li->code = body; jl_gc_wb(li, li->code);
    if (has_meta(body, pure_sym))
        li->pure = 1;
    jl_value_t *body1 = skip_meta(body);
    if (jl_is_linenode(body1)) {
        li->file = jl_linenode_file(body1);
        li->line = jl_linenode_line(body1);
    }
    else if (jl_is_expr(body1) && ((jl_expr_t*)body1)->head == line_sym) {
        li->file = (jl_sym_t*)jl_exprarg(body1, 1);
        li->line = jl_unbox_long(jl_exprarg(body1, 0));
    }
    jl_array_t *vis = jl_lam_vinfo((jl_expr_t*)ast);
    jl_array_t *args = jl_lam_args((jl_expr_t*)ast);
    size_t nslots = jl_array_len(vis);
    size_t narg = jl_array_len(args);
    li->nargs = narg;
    li->isva = narg>0 && jl_is_rest_arg(jl_cellref(args, narg-1));
    jl_value_t *gensym_types = jl_lam_gensyms((jl_expr_t*)ast);
    size_t ngensym = (jl_is_array(gensym_types) ? jl_array_len(gensym_types) : jl_unbox_long(gensym_types));
    li->slotnames = jl_alloc_cell_1d(nslots);
    li->slottypes = jl_nothing;
    li->slotflags = jl_alloc_array_1d(jl_array_uint8_type, nslots);
    li->gensymtypes = jl_box_long(ngensym);
    int i;
    for(i=0; i < nslots; i++) {
        jl_value_t *vi = jl_cellref(vis, i);
        jl_sym_t *name = (jl_sym_t*)jl_cellref(vi, 0);
        assert(jl_is_symbol(name));
        char *str = jl_symbol_name(name);
        if (i > 0 && name != unused_sym) {
            if (str[0] == '#') {
                // convention for renamed variables: #...#original_name
                char *nxt = strchr(str + 1, '#');
                if (nxt)
                    name = jl_symbol(nxt+1);
                else if (str[1] == 's')  // compiler-generated temporaries, #sXXX
                    name = compiler_temp_sym;
            }
        }
        jl_cellset(li->slotnames, i, name);
        jl_array_uint8_set(li->slotflags, i, jl_unbox_long(jl_cellref(vi, 2)));
    }
    jl_lambda_info_init_properties(li);
}

JL_DLLEXPORT
jl_lambda_info_t *jl_new_lambda_info(jl_value_t *ast, jl_svec_t *tvars, jl_svec_t *sparams,
                                     jl_module_t *ctx)
{
    jl_lambda_info_t *li =
        (jl_lambda_info_t*)newobj((jl_value_t*)jl_lambda_info_type,
                                  NWORDS(sizeof(jl_lambda_info_t)));
    li->code = NULL;
    li->slotnames = li->slotflags = NULL;
    li->slottypes = li->gensymtypes = NULL;
    li->rettype = (jl_value_t*)jl_any_type;
    li->file = null_sym;
    li->module = ctx;
    li->sparam_syms = tvars;
    li->sparam_vals = sparams;
    li->tfunc = jl_nothing;
    li->fptr = NULL;
    li->jlcall_api = 0;
    li->roots = NULL;
    li->functionObjects.functionObject = NULL;
    li->functionObjects.specFunctionObject = NULL;
    li->functionObjects.cFunctionList = NULL;
    li->functionID = 0;
    li->specFunctionID = 0;
    li->specTypes = NULL;
    li->inferred = 0;
    li->inInference = 0;
    li->inCompile = 0;
    li->unspecialized = NULL;
    li->specializations = NULL;
    li->name = anonymous_sym;
    li->def = li;
    li->line = 0;
    li->pure = 0;
    li->called = 0xff;
    li->needs_sparam_vals_ducttape = 2;
    if (ast != NULL) {
        JL_GC_PUSH1(&li);
        jl_lambda_info_set_ast(li, ast);
        JL_GC_POP();
    }
    return li;
}

JL_DLLEXPORT jl_lambda_info_t *jl_copy_lambda_info(jl_lambda_info_t *linfo)
{
    jl_lambda_info_t *new_linfo =
        jl_new_lambda_info(NULL, linfo->sparam_syms, linfo->sparam_vals, linfo->module);
    new_linfo->code = linfo->code;
    new_linfo->slotnames = linfo->slotnames;
    new_linfo->slottypes = linfo->slottypes;
    new_linfo->slotflags = linfo->slotflags;
    new_linfo->gensymtypes = linfo->gensymtypes;
    new_linfo->called = linfo->called;
    new_linfo->nargs = linfo->nargs;
    new_linfo->isva = linfo->isva;
    new_linfo->pure = linfo->pure;
    new_linfo->rettype = linfo->rettype;
    new_linfo->tfunc = linfo->tfunc;
    new_linfo->name = linfo->name;
    new_linfo->roots = linfo->roots;
    new_linfo->specTypes = linfo->specTypes;
    new_linfo->unspecialized = linfo->unspecialized;
    new_linfo->specializations = linfo->specializations;
    new_linfo->def = linfo->def;
    new_linfo->file = linfo->file;
    new_linfo->line = linfo->line;
    new_linfo->fptr = linfo->fptr;
    new_linfo->jlcall_api = linfo->jlcall_api;
    new_linfo->functionObjects.functionObject = linfo->functionObjects.functionObject;
    new_linfo->functionObjects.specFunctionObject = linfo->functionObjects.specFunctionObject;
    new_linfo->functionID = linfo->functionID;
    new_linfo->specFunctionID = linfo->specFunctionID;
    new_linfo->needs_sparam_vals_ducttape = linfo->needs_sparam_vals_ducttape;
    return new_linfo;
}

// symbols --------------------------------------------------------------------

JL_DEFINE_MUTEX(symbol_table)

static jl_sym_t *volatile symtab = NULL;

static uintptr_t hash_symbol(const char *str, size_t len)
{
    return memhash(str, len) ^ ~(uintptr_t)0/3*2;
}

#define SYM_POOL_SIZE 524288

static size_t symbol_nbytes(size_t len)
{
    return (sizeof_jl_taggedvalue_t + sizeof(jl_sym_t) + len + 1 + 7) & -8;
}

static jl_sym_t *mk_symbol(const char *str, size_t len)
{
#ifndef MEMDEBUG
    static char *sym_pool = NULL;
    static char *pool_ptr = NULL;
#endif
    jl_sym_t *sym;
    size_t nb = symbol_nbytes(len);

    if (nb >= SYM_POOL_SIZE) {
        jl_exceptionf(jl_argumenterror_type, "Symbol length exceeds maximum length");
    }

#ifdef MEMDEBUG
    sym = (jl_sym_t*)jl_valueof(malloc(nb));
#else
    if (sym_pool == NULL || pool_ptr+nb > sym_pool+SYM_POOL_SIZE) {
        sym_pool = (char*)malloc(SYM_POOL_SIZE);
        pool_ptr = sym_pool;
    }
    sym = (jl_sym_t*)jl_valueof(pool_ptr);
    pool_ptr += nb;
#endif
    jl_set_typeof(sym, jl_sym_type);
    sym->left = sym->right = NULL;
    sym->hash = hash_symbol(str, len);
    memcpy(jl_symbol_name(sym), str, len);
    jl_symbol_name(sym)[len] = 0;
    return sym;
}

static jl_sym_t *symtab_lookup(jl_sym_t *volatile *ptree, const char *str,
                               size_t len, jl_sym_t *volatile **slot)
{
    jl_sym_t *node = jl_atomic_load_acquire(ptree);
    uintptr_t h = hash_symbol(str, len);

    // Tree nodes sorted by major key of (int(hash)) and minor key of (str).
    while (node != NULL) {
        intptr_t x = (intptr_t)(h - node->hash);
        if (x == 0) {
            x = strncmp(str, jl_symbol_name(node), len);
            if (x == 0 && jl_symbol_name(node)[len] == 0) {
                if (slot != NULL)
                    *slot = ptree;
                return node;
            }
        }
        if (x < 0)
            ptree = &node->left;
        else
            ptree = &node->right;
        node = jl_atomic_load_acquire(ptree);
    }
    if (slot != NULL)
        *slot = ptree;
    return node;
}

static jl_sym_t *_jl_symbol(const char *str, size_t len)
{
    jl_sym_t *volatile *slot;
    jl_sym_t *node = symtab_lookup(&symtab, str, len, &slot);
    if (node == NULL) {
        JL_LOCK(symbol_table); // Might GC
        // Someone might have updated it, check and look up again
        if (*slot != NULL && (node = symtab_lookup(slot, str, len, &slot))) {
            JL_UNLOCK(symbol_table);
            return node;
        }
        node = mk_symbol(str, len);
        jl_atomic_store_release(slot, node);
        JL_UNLOCK(symbol_table);
    }
    return node;
}

JL_DLLEXPORT jl_sym_t *jl_symbol(const char *str)
{
    return _jl_symbol(str, strlen(str));
}

JL_DLLEXPORT jl_sym_t *jl_symbol_lookup(const char *str)
{
    return symtab_lookup(&symtab, str, strlen(str), NULL);
}

JL_DLLEXPORT jl_sym_t *jl_symbol_n(const char *str, int32_t len)
{
    if (memchr(str, 0, len))
        jl_exceptionf(jl_argumenterror_type, "Symbol name may not contain \\0");
    return _jl_symbol(str, len);
}

JL_DLLEXPORT jl_sym_t *jl_get_root_symbol(void)
{
    return symtab;
}

static uint32_t gs_ctr = 0;  // TODO: per-thread
uint32_t jl_get_gs_ctr(void) { return gs_ctr; }
void jl_set_gs_ctr(uint32_t ctr) { gs_ctr = ctr; }

JL_DLLEXPORT jl_sym_t *jl_gensym(void)
{
    static char name[16];
    char *n;
    n = uint2str(&name[2], sizeof(name)-2, gs_ctr, 10);
    *(--n) = '#'; *(--n) = '#';
    gs_ctr++;
    return jl_symbol(n);
}

JL_DLLEXPORT jl_sym_t *jl_tagged_gensym(const char *str, int32_t len)
{
    static char gs_name[14];
    if (symbol_nbytes(len) >= SYM_POOL_SIZE)
        jl_exceptionf(jl_argumenterror_type, "Symbol length exceeds maximum");
    if (memchr(str, 0, len))
        jl_exceptionf(jl_argumenterror_type, "Symbol name may not contain \\0");
    char *name = (char*) (len >= 256 ? malloc(sizeof(gs_name)+len+3) :
                          alloca(sizeof(gs_name)+len+3));
    char *n;
    name[0] = '#'; name[1] = '#'; name[2+len] = '#';
    memcpy(name+2, str, len);
    n = uint2str(gs_name, sizeof(gs_name), gs_ctr, 10);
    memcpy(name+3+len, n, sizeof(gs_name)-(n-gs_name));
    gs_ctr++;
    jl_sym_t *sym = _jl_symbol(name, len+3+sizeof(gs_name)-(n-gs_name)-1);
    if (len >= 256) free(name);
    return sym;
}

// allocating types -----------------------------------------------------------

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
    jl_methtable_t *mt = (jl_methtable_t*)jl_gc_allocobj(sizeof(jl_methtable_t));
    jl_set_typeof(mt, jl_methtable_type);
    mt->name = jl_demangle_typename(name);
    mt->module = module;
    mt->defs = (jl_methlist_t*)jl_nothing;
    mt->cache = (jl_methlist_t*)jl_nothing;
    mt->cache_arg1 = (jl_array_t*)jl_nothing;
    mt->cache_targ = (jl_array_t*)jl_nothing;
    mt->max_args = 0;
    mt->kwsorter = NULL;
#ifdef JL_GF_PROFILE
    mt->ncalls = 0;
#endif
    return mt;
}

JL_DLLEXPORT jl_typename_t *jl_new_typename_in(jl_sym_t *name, jl_module_t *module)
{
    jl_typename_t *tn=(jl_typename_t*)newobj((jl_value_t*)jl_typename_type, NWORDS(sizeof(jl_typename_t)));
    tn->name = name;
    tn->module = module;
    tn->primary = NULL;
    tn->cache = jl_emptysvec;
    tn->linearcache = jl_emptysvec;
    tn->names = NULL;
    tn->uid = jl_assign_type_uid();
    tn->mt = NULL;
    JL_GC_PUSH1(&tn);
    tn->mt = NULL;
    JL_GC_POP();
    return tn;
}

JL_DLLEXPORT jl_typename_t *jl_new_typename(jl_sym_t *name)
{
    return jl_new_typename_in(name, jl_current_module);
}

jl_datatype_t *jl_new_abstracttype(jl_value_t *name, jl_datatype_t *super,
                                   jl_svec_t *parameters)
{
    jl_datatype_t *dt = jl_new_datatype((jl_sym_t*)name, super, parameters, jl_emptysvec, jl_emptysvec, 1, 0, 0);
    dt->pointerfree = 0;
    return dt;
}

JL_DLLEXPORT jl_datatype_t *jl_new_uninitialized_datatype(size_t nfields, int8_t fielddesc_type)
{
    // fielddesc_type is specified manually for builtin types
    // and is (will be) calculated automatically for user defined types.
    uint32_t fielddesc_size = jl_fielddesc_size(fielddesc_type);
    jl_datatype_t *t = (jl_datatype_t*)
        newobj((jl_value_t*)jl_datatype_type,
               NWORDS(sizeof(jl_datatype_t) + nfields * fielddesc_size));
    // fielddesc_type should only be assigned here. It can cause data
    // corruption otherwise.
    t->fielddesc_type = fielddesc_type;
    t->nfields = nfields;
    t->haspadding = 0;
    t->pointerfree = 0;
    t->depth = 0;
    return t;
}

void jl_compute_field_offsets(jl_datatype_t *st)
{
    size_t sz = 0, alignm = 1;
    int ptrfree = 1;

    assert(0 <= st->fielddesc_type && st->fielddesc_type <= 2);

    uint64_t max_offset = (((uint64_t)1) <<
                           (1 << (3 + st->fielddesc_type))) - 1;
    uint64_t max_size = max_offset >> 1;

    for(size_t i=0; i < jl_datatype_nfields(st); i++) {
        jl_value_t *ty = jl_field_type(st, i);
        size_t fsz, al;
        if (jl_isbits(ty) && jl_is_leaf_type(ty)) {
            fsz = jl_datatype_size(ty);
            // Should never happen
            if (__unlikely(fsz > max_size))
                jl_throw(jl_overflow_exception);
            al = ((jl_datatype_t*)ty)->alignment;
            jl_field_setisptr(st, i, 0);
            if (((jl_datatype_t*)ty)->haspadding)
                st->haspadding = 1;
        }
        else {
            fsz = sizeof(void*);
            if (fsz > MAX_ALIGN)
                fsz = MAX_ALIGN;
            al = fsz;
            jl_field_setisptr(st, i, 1);
            ptrfree = 0;
        }
        if (al != 0) {
            size_t alsz = LLT_ALIGN(sz, al);
            if (sz & (al - 1))
                st->haspadding = 1;
            sz = alsz;
            if (al > alignm)
                alignm = al;
        }
        jl_field_setoffset(st, i, sz);
        jl_field_setsize(st, i, fsz);
        if (__unlikely(max_offset - sz < fsz))
            jl_throw(jl_overflow_exception);
        sz += fsz;
    }
    st->alignment = alignm;
    st->size = LLT_ALIGN(sz, alignm);
    if (st->size > sz)
        st->haspadding = 1;
    st->pointerfree = ptrfree && !st->abstract;
}

extern int jl_boot_file_loaded;

JL_DLLEXPORT jl_datatype_t *jl_new_datatype(jl_sym_t *name, jl_datatype_t *super,
                                            jl_svec_t *parameters,
                                            jl_svec_t *fnames, jl_svec_t *ftypes,
                                            int abstract, int mutabl,
                                            int ninitialized)
{
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
        t = jl_new_uninitialized_datatype(jl_svec_len(fnames), 2); // TODO
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
    t->pointerfree = 0;
    t->ninitialized = ninitialized;
    t->instance = NULL;
    t->struct_decl = NULL;
    t->ditype = NULL;
    t->size = 0;
    t->alignment = 1;
    t->haspadding = 0;

    if (tn == NULL) {
        t->name = NULL;
        if (jl_is_typename(name)) {
            tn = (jl_typename_t*)name;
        }
        else {
            tn = jl_new_typename((jl_sym_t*)name);
            if (!abstract) {
                tn->mt = jl_new_method_table(name, jl_current_module);
                jl_gc_wb(tn, tn->mt);
            }
        }
        t->name = tn;
        jl_gc_wb(t, t->name);
    }
    t->name->names = fnames;
    jl_gc_wb(t->name, t->name->names);

    if (t->name->primary == NULL) {
        t->name->primary = (jl_value_t*)t;
        jl_gc_wb(t->name, t);
    }

    if (abstract || jl_svec_len(parameters) > 0) {
        t->uid = 0;
    }
    else {
        t->uid = jl_assign_type_uid();
        if (t->types != NULL)
            jl_compute_field_offsets(t);
    }
    JL_GC_POP();
    return t;
}

JL_DLLEXPORT jl_datatype_t *jl_new_bitstype(jl_value_t *name, jl_datatype_t *super,
                                            jl_svec_t *parameters, size_t nbits)
{
    jl_datatype_t *bt = jl_new_datatype((jl_sym_t*)name, super, parameters,
                                        jl_emptysvec, jl_emptysvec, 0, 0, 0);
    bt->size = nbits/8;
    bt->alignment = bt->size;
    if (bt->alignment > MAX_ALIGN)
        bt->alignment = MAX_ALIGN;
    bt->pointerfree = 1;
    return bt;
}

// type constructor -----------------------------------------------------------

jl_typector_t *jl_new_type_ctor(jl_svec_t *params, jl_value_t *body)
{
    jl_typector_t *tc = (jl_typector_t*)newobj((jl_value_t*)jl_typector_type,NWORDS(sizeof(jl_typector_t)));
    tc->parameters = params;
    tc->body = body;
    return (jl_typector_t*)tc;
}

// bits constructors ----------------------------------------------------------

#define BOXN_FUNC(nb,nw)                                                \
    JL_DLLEXPORT jl_value_t *jl_box##nb(jl_datatype_t *t, int##nb##_t x) \
    {                                                                   \
        assert(jl_isbits(t));                                           \
        assert(jl_datatype_size(t) == sizeof(x));                       \
        jl_value_t *v = (jl_value_t*)jl_gc_alloc_##nw##w();             \
        jl_set_typeof(v, t);                                            \
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
UNBOX_FUNC(gensym, ssize_t)

#define BOX_FUNC(typ,c_type,pfx,nw)                         \
    JL_DLLEXPORT jl_value_t *pfx##_##typ(c_type x)          \
    {                                                       \
        jl_value_t *v = (jl_value_t*)jl_gc_alloc_##nw##w(); \
        jl_set_typeof(v, jl_##typ##_type);                  \
        *(c_type*)jl_data_ptr(v) = x;                       \
        return v;                                           \
    }
BOX_FUNC(float32, float,  jl_box, 1)
BOX_FUNC(voidpointer, void*,  jl_box, 1)
#ifdef _P64
BOX_FUNC(float64, double, jl_box, 1)
#else
BOX_FUNC(float64, double, jl_box, 2)
#endif

#define NBOX_C 1024

#define SIBOX_FUNC(typ,c_type,nw)                             \
    static jl_value_t *boxed_##typ##_cache[NBOX_C];           \
    JL_DLLEXPORT jl_value_t *jl_box_##typ(c_type x)           \
    {                                                         \
        c_type idx = x+NBOX_C/2;                              \
        if ((u##c_type)idx < (u##c_type)NBOX_C)               \
            return boxed_##typ##_cache[idx];                  \
        jl_value_t *v = (jl_value_t*)jl_gc_alloc_##nw##w();   \
        jl_set_typeof(v, jl_##typ##_type);                    \
        *(c_type*)jl_data_ptr(v) = x;                         \
        return v;                                             \
    }
#define UIBOX_FUNC(typ,c_type,nw)                               \
    static jl_value_t *boxed_##typ##_cache[NBOX_C];             \
    JL_DLLEXPORT jl_value_t *jl_box_##typ(c_type x)             \
    {                                                           \
        if (x < NBOX_C)                                         \
            return boxed_##typ##_cache[x];                      \
        jl_value_t *v = (jl_value_t*)jl_gc_alloc_##nw##w();     \
        jl_set_typeof(v, jl_##typ##_type);                      \
        *(c_type*)jl_data_ptr(v) = x;                           \
        return v;                                               \
    }
SIBOX_FUNC(int16,  int16_t, 1)
SIBOX_FUNC(int32,  int32_t, 1)
UIBOX_FUNC(uint16, uint16_t, 1)
UIBOX_FUNC(uint32, uint32_t, 1)
UIBOX_FUNC(char,   uint32_t, 1)
UIBOX_FUNC(gensym, size_t, 1)
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
        boxed_gensym_cache[i] = jl_box64(jl_gensym_type, i);
#else
        boxed_gensym_cache[i] = jl_box32(jl_gensym_type, i);
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

void jl_mark_box_caches(void)
{
    int64_t i;
    for(i=0; i < 256; i++) {
        jl_gc_setmark(boxed_int8_cache[i]);
        jl_gc_setmark(boxed_uint8_cache[i]);
    }
    for(i=0; i < NBOX_C; i++) {
        jl_gc_setmark(boxed_int16_cache[i]);
        jl_gc_setmark(boxed_int32_cache[i]);
        jl_gc_setmark(boxed_int64_cache[i]);
        jl_gc_setmark(boxed_uint16_cache[i]);
        jl_gc_setmark(boxed_uint32_cache[i]);
        jl_gc_setmark(boxed_char_cache[i]);
        jl_gc_setmark(boxed_uint64_cache[i]);
        jl_gc_setmark(boxed_gensym_cache[i]);
    }
}

JL_DLLEXPORT jl_value_t *jl_box_bool(int8_t x)
{
    if (x)
        return jl_true;
    return jl_false;
}

// Expr constructor for internal use ------------------------------------------

jl_expr_t *jl_exprn(jl_sym_t *head, size_t n)
{
    jl_array_t *ar = n==0 ? (jl_array_t*)jl_an_empty_cell : jl_alloc_cell_1d(n);
    JL_GC_PUSH1(&ar);
    jl_expr_t *ex = (jl_expr_t*)jl_gc_alloc_3w(); assert(NWORDS(sizeof(jl_expr_t))==3);
    jl_set_typeof(ex, jl_expr_type);
    ex->head = head;
    ex->args = ar;
    ex->etype = (jl_value_t*)jl_any_type;
    JL_GC_POP();
    return ex;
}

JL_CALLABLE(jl_f__expr)
{
    JL_NARGSV(Expr, 1);
    JL_TYPECHK(Expr, symbol, args[0]);
    jl_array_t *ar = jl_alloc_cell_1d(nargs-1);
    JL_GC_PUSH1(&ar);
    for(size_t i=0; i < nargs-1; i++)
        jl_cellset(ar, i, args[i+1]);
    jl_expr_t *ex = (jl_expr_t*)jl_gc_alloc_3w(); assert(NWORDS(sizeof(jl_expr_t))==3);
    jl_set_typeof(ex, jl_expr_type);
    ex->head = (jl_sym_t*)args[0];
    ex->args = ar;
    ex->etype = (jl_value_t*)jl_any_type;
    JL_GC_POP();
    return (jl_value_t*)ex;
}

#ifdef __cplusplus
}
#endif
