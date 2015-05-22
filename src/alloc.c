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

jl_value_t *jl_true;
jl_value_t *jl_false;

jl_tvar_t     *jl_typetype_tvar;
jl_datatype_t *jl_typetype_type;
jl_value_t    *jl_ANY_flag;
jl_datatype_t *jl_function_type;
jl_datatype_t *jl_box_type;
jl_value_t *jl_box_any_type;
jl_typename_t *jl_box_typename;

jl_datatype_t *jl_typector_type;

jl_datatype_t *jl_array_type;
jl_typename_t *jl_array_typename;
jl_value_t *jl_array_uint8_type;
jl_value_t *jl_array_any_type=NULL;
jl_value_t *jl_array_symbol_type;
jl_function_t *jl_bottom_func;
jl_datatype_t *jl_weakref_type;
jl_datatype_t *jl_ascii_string_type;
jl_datatype_t *jl_utf8_string_type;
jl_datatype_t *jl_expr_type;
jl_datatype_t *jl_symbolnode_type;
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
jl_datatype_t *jl_undefvarerror_type;
jl_datatype_t *jl_ref_type;
jl_datatype_t *jl_pointer_type;
jl_datatype_t *jl_void_type;
jl_datatype_t *jl_voidpointer_type;
jl_value_t *jl_an_empty_cell=NULL;
jl_value_t *jl_stackovf_exception;
jl_value_t *jl_diverror_exception;
jl_value_t *jl_domain_exception;
jl_value_t *jl_overflow_exception;
jl_value_t *jl_inexact_exception;
jl_value_t *jl_undefref_exception;
jl_value_t *jl_interrupt_exception;
jl_datatype_t *jl_boundserror_type;
jl_value_t *jl_memory_exception;

jl_sym_t *call_sym;    jl_sym_t *dots_sym;
jl_sym_t *call1_sym;   jl_sym_t *module_sym;
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
jl_sym_t *macro_sym;   jl_sym_t *method_sym;
jl_sym_t *enter_sym;   jl_sym_t *leave_sym;
jl_sym_t *exc_sym;     jl_sym_t *error_sym;
jl_sym_t *kw_sym;      jl_sym_t *compositetype_sym;
jl_sym_t *new_sym;     jl_sym_t *using_sym;
jl_sym_t *const_sym;   jl_sym_t *thunk_sym;
jl_sym_t *anonymous_sym;  jl_sym_t *underscore_sym;
jl_sym_t *abstracttype_sym; jl_sym_t *bitstype_sym;
jl_sym_t *global_sym; jl_sym_t *tuple_sym;
jl_sym_t *dot_sym;    jl_sym_t *newvar_sym;
jl_sym_t *boundscheck_sym; jl_sym_t *copyast_sym;
jl_sym_t *fastmath_sym;
jl_sym_t *simdloop_sym; jl_sym_t *meta_sym;
jl_sym_t *arrow_sym;  jl_sym_t *inert_sym;
jl_sym_t *vararg_sym;

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

jl_value_t *jl_new_bits(jl_value_t *bt, void *data)
{
    size_t len = 0;
    return jl_new_bits_internal(bt, data, &len);
}

// run time version of pointerref intrinsic
DLLEXPORT jl_value_t *jl_pointerref(jl_value_t *p, jl_value_t *i)
{
    JL_TYPECHK(pointerref, pointer, p);
    JL_TYPECHK(pointerref, long, i);
    jl_value_t *ety = jl_tparam0(jl_typeof(p));
    if (ety == (jl_value_t*)jl_any_type) {
        jl_value_t **pp = (jl_value_t**)(jl_unbox_long(p) + (jl_unbox_long(i)-1)*sizeof(void*));
        return *pp;
    }
    else {
        if (!jl_is_datatype(ety))
            jl_error("pointerref: invalid pointer");
        size_t nb = LLT_ALIGN(jl_datatype_size(ety), ((jl_datatype_t*)ety)->alignment);
        char *pp = (char*)jl_unbox_long(p) + (jl_unbox_long(i)-1)*nb;
        return jl_new_bits(ety, pp);
    }
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

// run time version of pointerset intrinsic
DLLEXPORT void jl_pointerset(jl_value_t *p, jl_value_t *x, jl_value_t *i)
{
    JL_TYPECHK(pointerset, pointer, p);
    JL_TYPECHK(pointerset, long, i);
    jl_value_t *ety = jl_tparam0(jl_typeof(p));
    if (ety == (jl_value_t*)jl_any_type) {
        jl_value_t **pp = (jl_value_t**)(jl_unbox_long(p) + (jl_unbox_long(i)-1)*sizeof(void*));
        *pp = x;
    }
    else {
        if (!jl_is_datatype(ety))
            jl_error("pointerset: invalid pointer");
        size_t nb = LLT_ALIGN(jl_datatype_size(ety), ((jl_datatype_t*)ety)->alignment);
        char *pp = (char*)jl_unbox_long(p) + (jl_unbox_long(i)-1)*nb;
        if (jl_typeof(x) != ety)
            jl_error("pointerset: type mismatch in assign");
        jl_assign_bits(pp, x);
    }
}

int jl_field_index(jl_datatype_t *t, jl_sym_t *fld, int err)
{
    jl_svec_t *fn = t->name->names;
    for(size_t i=0; i < jl_svec_len(fn); i++) {
        if (jl_svecref(fn,i) == (jl_value_t*)fld) {
            return (int)i;
        }
    }
    if (err)
        jl_errorf("type %s has no field %s", t->name->name->name, fld->name);
    return -1;
}

jl_value_t *jl_get_nth_field(jl_value_t *v, size_t i)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    assert(i < jl_datatype_nfields(st));
    size_t offs = jl_field_offset(st,i);
    if (jl_field_isptr(st,i)) {
        return *(jl_value_t**)((char*)v + offs);
    }
    return jl_new_bits(jl_field_type(st,i), (char*)v + offs);
}

jl_value_t *jl_get_nth_field_checked(jl_value_t *v, size_t i)
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

void jl_set_nth_field(jl_value_t *v, size_t i, jl_value_t *rhs)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t offs = jl_field_offset(st,i);
    if (jl_field_isptr(st,i)) {
        *(jl_value_t**)((char*)v + offs) = rhs;
        if (rhs != NULL) gc_wb(v, rhs);
    }
    else {
        jl_assign_bits((char*)v + offs, rhs);
    }
}

int jl_field_isdefined(jl_value_t *v, size_t i)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t offs = jl_field_offset(st,i);
    if (jl_field_isptr(st,i)) {
        return *(jl_value_t**)((char*)v + offs) != NULL;
    }
    return 1;
}

DLLEXPORT jl_value_t *jl_new_struct(jl_datatype_t *type, ...)
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

DLLEXPORT jl_value_t *jl_new_structv(jl_datatype_t *type, jl_value_t **args, uint32_t na)
{
    if (type->instance != NULL) return type->instance;
    size_t nf = jl_datatype_nfields(type);
    jl_value_t *jv = newstruct(type);
    for(size_t i=0; i < na; i++) {
        jl_set_nth_field(jv, i, args[i]);
    }
    for(size_t i=na; i < nf; i++) {
        if (type->fields[i].isptr)
            *(jl_value_t**)((char*)jl_data_ptr(jv)+jl_field_offset(type,i)) = NULL;
    }
    return jv;
}

DLLEXPORT jl_value_t *jl_new_struct_uninit(jl_datatype_t *type)
{
    if (type->instance != NULL) return type->instance;
    jl_value_t *jv = newstruct(type);
    if (type->size > 0)
        memset(jl_data_ptr(jv), 0, type->size);
    return jv;
}

DLLEXPORT jl_function_t *jl_new_closure(jl_fptr_t fptr, jl_value_t *env,
                                        jl_lambda_info_t *linfo)
{
    jl_function_t *f = (jl_function_t*)alloc_3w(); assert(NWORDS(sizeof(jl_function_t))==3);
    jl_set_typeof(f, jl_function_type);
    f->fptr = (fptr!=NULL ? fptr : linfo->fptr);
    f->env = env;
    f->linfo = linfo;
    return f;
}

DLLEXPORT
jl_lambda_info_t *jl_new_lambda_info(jl_value_t *ast, jl_svec_t *sparams)
{
    jl_lambda_info_t *li =
        (jl_lambda_info_t*)newobj((jl_value_t*)jl_lambda_info_type,
                                  NWORDS(sizeof(jl_lambda_info_t)));
    li->ast = ast;
    li->file = null_sym;
    li->line = 0;
    if (ast != NULL && jl_is_expr(ast)) {
        jl_value_t *body1 = skip_meta(jl_lam_body((jl_expr_t*)ast)->args);
        if (jl_is_expr(body1) && ((jl_expr_t*)body1)->head == line_sym) {
            li->file = (jl_sym_t*)jl_exprarg(body1, 1);
            li->line = jl_unbox_long(jl_exprarg(body1, 0));
        }
    }
    li->module = jl_current_module;
    li->sparams = sparams;
    li->tfunc = jl_nothing;
    li->fptr = &jl_trampoline;
    li->roots = NULL;
    li->functionObject = NULL;
    li->specFunctionObject = NULL;
    li->cFunctionList = NULL;
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
    li->capt = NULL;
    return li;
}

// symbols --------------------------------------------------------------------

static jl_sym_t *symtab = NULL;

static uptrint_t hash_symbol(const char *str, size_t len)
{
    return memhash(str, len) ^ ~(uptrint_t)0/3*2;
}

#define SYM_POOL_SIZE 524288

static jl_sym_t *mk_symbol(const char *str)
{
#ifndef MEMDEBUG
    static char *sym_pool = NULL;
    static char *pool_ptr = NULL;
#endif
    jl_sym_t *sym;
    size_t len = strlen(str);
    size_t nb = (sizeof(jl_taggedvalue_t)+sizeof(jl_sym_t)+len+1+7)&-8;

    if (nb >= SYM_POOL_SIZE) {
        jl_exceptionf(jl_argumenterror_type, "Symbol length exceeds maximum length");
    }

#ifdef MEMDEBUG
    sym = (jl_sym_t*)&((jl_taggedvalue_t*)malloc(nb))->value;
#else
    if (sym_pool == NULL || pool_ptr+nb > sym_pool+SYM_POOL_SIZE) {
        sym_pool = (char*)malloc(SYM_POOL_SIZE);
        pool_ptr = sym_pool;
    }
    sym = (jl_sym_t*)&((jl_taggedvalue_t*)pool_ptr)->value;
    pool_ptr += nb;
#endif
    jl_set_typeof(sym, jl_sym_type);
    sym->left = sym->right = NULL;
    sym->hash = hash_symbol(str, len);
    strcpy(&sym->name[0], str);
    return sym;
}

static void unmark_symbols_(jl_sym_t *root)
{
    while (root != NULL) {
        jl_set_typeof(root, jl_sym_type);
        unmark_symbols_(root->left);
        root = root->right;
    }
}

void jl_unmark_symbols(void) { unmark_symbols_(symtab); }

static jl_sym_t **symtab_lookup(jl_sym_t **ptree, const char *str, jl_sym_t **parent)
{
    int x;
    if (parent != NULL) *parent = NULL;
    uptrint_t h = hash_symbol(str, strlen(str));

    // Tree nodes sorted by major key of (int(hash)) and minor key o (str).
    while (*ptree != NULL) {
        x = (int)(h-(*ptree)->hash);
        if (x == 0) {
            x = strcmp(str, (*ptree)->name);
            if (x == 0)
                return ptree;
        }
        if (parent != NULL) *parent = *ptree;
        if (x < 0)
            ptree = &(*ptree)->left;
        else
            ptree = &(*ptree)->right;
    }
    return ptree;
}

jl_sym_t *jl_symbol(const char *str)
{
    jl_sym_t **pnode;
    jl_sym_t *parent;
    pnode = symtab_lookup(&symtab, str, &parent);
    if (*pnode == NULL) {
        *pnode = mk_symbol(str);
        if (parent != NULL)
            gc_wb(parent, *pnode);
    }
    return *pnode;
}

jl_sym_t *jl_symbol_lookup(const char *str)
{
    return *symtab_lookup(&symtab, str, NULL);
}

DLLEXPORT jl_sym_t *jl_symbol_n(const char *str, int32_t len)
{
    char *name = (char*)alloca(len+1);
    memcpy(name, str, len);
    name[len] = '\0';
    if (strlen(name) != len)
        jl_exceptionf(jl_argumenterror_type, "Symbol name may not contain \\0");
    return jl_symbol(name);
}

DLLEXPORT jl_sym_t *jl_get_root_symbol() { return symtab; }

static uint32_t gs_ctr = 0;  // TODO: per-thread
uint32_t jl_get_gs_ctr(void) { return gs_ctr; }
void jl_set_gs_ctr(uint32_t ctr) { gs_ctr = ctr; }

DLLEXPORT jl_sym_t *jl_gensym(void)
{
    static char name[16];
    char *n;
    n = uint2str(&name[2], sizeof(name)-2, gs_ctr, 10);
    *(--n) = '#'; *(--n) = '#';
    gs_ctr++;
    return jl_symbol(n);
}

DLLEXPORT jl_sym_t *jl_tagged_gensym(const char *str, int32_t len)
{
    static char gs_name[14];
    char *name = (char*)alloca(sizeof(gs_name)+len+3);
    char *n;
    name[0] = '#'; name[1] = '#'; name[2+len] = '#';
    memcpy(name+2, str, len);
    n = uint2str(gs_name, sizeof(gs_name), gs_ctr, 10);
    memcpy(name+3+len, n, sizeof(gs_name)-(n-gs_name));
    if (strlen(name) != len+3+sizeof(gs_name)-(n-gs_name)-1)
        jl_exceptionf(jl_argumenterror_type, "Symbol name may not contain \\0");
    gs_ctr++;
    return jl_symbol(name);
}

// allocating types -----------------------------------------------------------

jl_typename_t *jl_new_typename(jl_sym_t *name)
{
    jl_typename_t *tn=(jl_typename_t*)newobj((jl_value_t*)jl_typename_type, NWORDS(sizeof(jl_typename_t)));
    tn->name = name;
    tn->module = jl_current_module;
    tn->primary = NULL;
    tn->cache = jl_emptysvec;
    tn->linearcache = jl_emptysvec;
    tn->names = NULL;
    tn->uid = jl_assign_type_uid();
    return tn;
}

jl_datatype_t *jl_new_abstracttype(jl_value_t *name, jl_datatype_t *super,
                                   jl_svec_t *parameters)
{
    jl_datatype_t *dt = jl_new_datatype((jl_sym_t*)name, super, parameters, jl_emptysvec, jl_emptysvec, 1, 0, 0);
    dt->pointerfree = 0;
    return dt;
}

jl_datatype_t *jl_new_uninitialized_datatype(size_t nfields)
{
    jl_datatype_t *t = (jl_datatype_t*)
        newobj((jl_value_t*)jl_datatype_type,
               NWORDS(sizeof(jl_datatype_t) + nfields*sizeof(jl_fielddesc_t)));
    t->nfields = nfields;
    return t;
}

void jl_compute_field_offsets(jl_datatype_t *st)
{
    size_t sz = 0, alignm = 1;
    int ptrfree = 1;

    for(size_t i=0; i < jl_datatype_nfields(st); i++) {
        jl_value_t *ty = jl_field_type(st, i);
        size_t fsz, al;
        if (jl_isbits(ty) && jl_is_leaf_type(ty)) {
            fsz = jl_datatype_size(ty);
            if (__unlikely(fsz > JL_FIELD_MAX_SIZE))
                jl_throw(jl_overflow_exception);
            al = ((jl_datatype_t*)ty)->alignment;
            st->fields[i].isptr = 0;
        }
        else {
            fsz = sizeof(void*);
            if (fsz > MAX_ALIGN)
                fsz = MAX_ALIGN;
            al = fsz;
            st->fields[i].isptr = 1;
            ptrfree = 0;
        }
        if (al != 0) {
            sz = LLT_ALIGN(sz, al);
            if (al > alignm)
                alignm = al;
        }
        if (__unlikely(sz > JL_FIELD_MAX_OFFSET))
            jl_throw(jl_overflow_exception);
        st->fields[i].offset = sz;
        st->fields[i].size = fsz;
        sz += fsz;
    }
    st->alignment = alignm;
    st->size = LLT_ALIGN(sz, alignm);
    st->pointerfree = ptrfree && !st->abstract;
}

extern int jl_boot_file_loaded;

jl_datatype_t *jl_new_datatype(jl_sym_t *name, jl_datatype_t *super,
                               jl_svec_t *parameters,
                               jl_svec_t *fnames, jl_svec_t *ftypes,
                               int abstract, int mutabl, int ninitialized)
{
    jl_datatype_t *t=NULL;
    jl_typename_t *tn=NULL;
    JL_GC_PUSH2(&t, &tn);

    if (!jl_boot_file_loaded && jl_is_symbol(name)) {
        // hack to avoid making two versions of basic types needed
        // during bootstrapping
        if (!strcmp(((jl_sym_t*)name)->name, "Int32"))
            t = jl_int32_type;
        else if (!strcmp(((jl_sym_t*)name)->name, "Int64"))
            t = jl_int64_type;
        else if (!strcmp(((jl_sym_t*)name)->name, "Bool"))
            t = jl_bool_type;
    }
    if (t == NULL)
        t = jl_new_uninitialized_datatype(jl_svec_len(fnames));
    else
        tn = t->name;
    // init before possibly calling jl_new_typename
    t->super = super;
    if (super != NULL) gc_wb(t, t->super);
    t->parameters = parameters;
    gc_wb(t, t->parameters);
    t->types = ftypes;
    if (ftypes != NULL) gc_wb(t, t->types);
    t->abstract = abstract;
    t->mutabl = mutabl;
    t->pointerfree = 0;
    t->ninitialized = ninitialized;
    t->instance = NULL;
    t->struct_decl = NULL;
    t->ditype = NULL;
    t->size = 0;
    t->alignment = 1;

    if (tn == NULL) {
        t->name = NULL;
        if (jl_is_typename(name))
            tn = (jl_typename_t*)name;
        else
            tn = jl_new_typename((jl_sym_t*)name);
        t->name = tn;
        gc_wb(t, t->name);
    }
    t->name->names = fnames;
    gc_wb(t->name, t->name->names);

    if (t->name->primary == NULL) {
        t->name->primary = (jl_value_t*)t;
        gc_wb(t->name, t);
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

jl_datatype_t *jl_new_bitstype(jl_value_t *name, jl_datatype_t *super,
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

jl_uniontype_t *jl_new_uniontype(jl_svec_t *types)
{
    jl_uniontype_t *t = (jl_uniontype_t*)newobj((jl_value_t*)jl_uniontype_type,NWORDS(sizeof(jl_uniontype_t)));
    // don't make unions of 1 type; Union(T)==T
    assert(jl_svec_len(types) != 1);
    t->types = types;
    return t;
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

#define BOXN_FUNC(nb,nw)                                       \
jl_value_t *jl_box##nb(jl_datatype_t *t, int##nb##_t x)        \
{                                                              \
    assert(jl_isbits(t));                                      \
    assert(jl_datatype_size(t) == sizeof(x));                  \
    jl_value_t *v = (jl_value_t*)alloc_##nw##w();              \
    jl_set_typeof(v, t);                                       \
    *(int##nb##_t*)jl_data_ptr(v) = x;                         \
    return v;                                                  \
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
c_type jl_unbox_##j_type(jl_value_t *v)                                 \
{                                                                       \
    assert(jl_is_bitstype(jl_typeof(v)));                               \
    assert(jl_datatype_size(jl_typeof(v)) == sizeof(c_type));           \
    return *(c_type*)jl_data_ptr(v);                                    \
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

#define BOX_FUNC(typ,c_type,pfx,nw)               \
jl_value_t *pfx##_##typ(c_type x)                 \
{                                                 \
    jl_value_t *v = (jl_value_t*)alloc_##nw##w(); \
    jl_set_typeof(v, jl_##typ##_type);            \
    *(c_type*)jl_data_ptr(v) = x;                 \
    return v;                                     \
}
BOX_FUNC(float32, float,  jl_box, 1)
BOX_FUNC(voidpointer, void*,  jl_box, 1)
#ifdef _P64
BOX_FUNC(float64, double, jl_box, 1)
#else
BOX_FUNC(float64, double, jl_box, 2)
#endif

#define NBOX_C 1024

#define SIBOX_FUNC(typ,c_type,nw)                       \
static jl_value_t *boxed_##typ##_cache[NBOX_C];         \
jl_value_t *jl_box_##typ(c_type x)                      \
{                                                       \
    c_type idx = x+NBOX_C/2;                            \
    if ((u##c_type)idx < (u##c_type)NBOX_C)             \
        return boxed_##typ##_cache[idx];                \
    jl_value_t *v = (jl_value_t*)alloc_##nw##w();       \
    jl_set_typeof(v, jl_##typ##_type);                  \
    *(c_type*)jl_data_ptr(v) = x;                       \
    return v;                                           \
}
#define UIBOX_FUNC(typ,c_type,nw)                  \
static jl_value_t *boxed_##typ##_cache[NBOX_C];    \
jl_value_t *jl_box_##typ(c_type x)                 \
{                                                  \
    if (x < NBOX_C)                                \
        return boxed_##typ##_cache[x];             \
    jl_value_t *v = (jl_value_t*)alloc_##nw##w();  \
    jl_set_typeof(v, jl_##typ##_type);             \
    *(c_type*)jl_data_ptr(v) = x;                  \
    return v;                                      \
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
jl_value_t *jl_box_int8(int8_t x)
{
    return boxed_int8_cache[(uint8_t)x];
}
static jl_value_t *boxed_uint8_cache[256];
jl_value_t *jl_box_uint8(uint8_t x)
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
}

void jl_init_box_caches(void)
{
    int64_t i;
    for(i=0; i < 256; i++) {
        boxed_int8_cache[i]  = jl_box8(jl_int8_type, i);
        boxed_uint8_cache[i] = jl_box8(jl_uint8_type, i);
    }
    for(i=0; i < NBOX_C; i++) {
        boxed_int16_cache[i]  = jl_box16(jl_int16_type, i-NBOX_C/2);
        boxed_uint16_cache[i] = jl_box16(jl_uint16_type, i);
        boxed_uint32_cache[i] = jl_box32(jl_uint32_type, i);
        boxed_char_cache[i]   = jl_box32(jl_char_type, i);
        boxed_uint64_cache[i] = jl_box64(jl_uint64_type, i);
    }
}

#ifdef JL_GC_MARKSWEEP
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
#endif

jl_value_t *jl_box_bool(int8_t x)
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
    jl_expr_t *ex = (jl_expr_t*)alloc_3w(); assert(NWORDS(sizeof(jl_expr_t))==3);
    jl_set_typeof(ex, jl_expr_type);
    ex->head = head;
    ex->args = ar;
    ex->etype = (jl_value_t*)jl_any_type;
    JL_GC_POP();
    return ex;
}

JL_CALLABLE(jl_f_new_expr)
{
    JL_NARGSV(Expr, 1);
    JL_TYPECHK(Expr, symbol, args[0]);
    jl_array_t *ar = jl_alloc_cell_1d(nargs-1);
    JL_GC_PUSH1(&ar);
    for(size_t i=0; i < nargs-1; i++)
        jl_cellset(ar, i, args[i+1]);
    jl_expr_t *ex = (jl_expr_t*)alloc_3w(); assert(NWORDS(sizeof(jl_expr_t))==3);
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
