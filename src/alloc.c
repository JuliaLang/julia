/*
  object constructors
*/
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include "julia.h"
#include "newobj_internal.h"
#include "builtin_proto.h"

jl_value_t *jl_true;
jl_value_t *jl_false;

jl_datatype_t *jl_undef_type;
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
jl_datatype_t *jl_getfieldnode_type;
jl_datatype_t *jl_linenumbernode_type;
jl_datatype_t *jl_labelnode_type;
jl_datatype_t *jl_gotonode_type;
jl_datatype_t *jl_quotenode_type;
jl_datatype_t *jl_topnode_type;
jl_datatype_t *jl_intrinsic_type;
jl_datatype_t *jl_methtable_type;
jl_datatype_t *jl_method_type;
jl_datatype_t *jl_lambda_info_type;
jl_datatype_t *jl_module_type;
jl_datatype_t *jl_errorexception_type=NULL;
jl_datatype_t *jl_typeerror_type;
jl_datatype_t *jl_methoderror_type;
jl_datatype_t *jl_loaderror_type;
jl_datatype_t *jl_pointer_type;
jl_datatype_t *jl_voidpointer_type;
jl_value_t *jl_an_empty_cell=NULL;
jl_value_t *jl_stackovf_exception;
jl_value_t *jl_divbyzero_exception;
jl_value_t *jl_domain_exception;
jl_value_t *jl_overflow_exception;
jl_value_t *jl_inexact_exception;
jl_value_t *jl_undefref_exception;
jl_value_t *jl_interrupt_exception;
jl_value_t *jl_bounds_exception;
jl_value_t *jl_memory_exception;

jl_sym_t *call_sym;    jl_sym_t *dots_sym;
jl_sym_t *call1_sym;   jl_sym_t *module_sym;
jl_sym_t *export_sym;  jl_sym_t *import_sym;
jl_sym_t *importall_sym; jl_sym_t *toplevel_sym;
jl_sym_t *quote_sym;   jl_sym_t *amp_sym;
jl_sym_t *top_sym;     jl_sym_t *colons_sym;
jl_sym_t *line_sym;    jl_sym_t *jl_continue_sym;
// head symbols for each expression type
jl_sym_t *goto_sym;    jl_sym_t *goto_ifnot_sym;
jl_sym_t *label_sym;   jl_sym_t *return_sym;
jl_sym_t *lambda_sym;  jl_sym_t *assign_sym;
jl_sym_t *null_sym;    jl_sym_t *body_sym;
jl_sym_t *macro_sym;   jl_sym_t *method_sym;
jl_sym_t *enter_sym;   jl_sym_t *leave_sym;
jl_sym_t *exc_sym;     jl_sym_t *error_sym;
jl_sym_t *static_typeof_sym;
jl_sym_t *new_sym;     jl_sym_t *using_sym;
jl_sym_t *const_sym;   jl_sym_t *thunk_sym;
jl_sym_t *anonymous_sym;  jl_sym_t *underscore_sym;
jl_sym_t *abstracttype_sym; jl_sym_t *bitstype_sym;
jl_sym_t *compositetype_sym; jl_sym_t *type_goto_sym;
jl_sym_t *global_sym; jl_sym_t *tuple_sym;

typedef struct {
    int64_t a;
    int64_t b;
} bits128_t;

jl_value_t *jl_new_bits(jl_datatype_t *bt, void *data)
{
    if (bt == jl_uint8_type)        return jl_box_uint8(*(uint8_t*)data);
    else if (bt == jl_int64_type)   return jl_box_int64(*(int64_t*)data);
    else if (bt == jl_bool_type)    return (*(int8_t*)data) ? jl_true:jl_false;
    else if (bt == jl_int32_type)   return jl_box_int32(*(int32_t*)data);
    else if (bt == jl_float64_type) return jl_box_float64(*(double*)data);
    
    size_t nb = jl_datatype_size(bt);
    jl_value_t *v = 
        (jl_value_t*)allocobj((NWORDS(LLT_ALIGN(nb,sizeof(void*)))+1)*
                              sizeof(void*));
    v->type = (jl_value_t*)bt;
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

void jl_assign_bits(void *dest, jl_value_t *bits)
{
    size_t nb = jl_datatype_size(jl_typeof(bits));
    switch (nb) {
    case  1: *(int8_t*)dest    = *(int8_t*)jl_data_ptr(bits);    break;
    case  2: *(int16_t*)dest   = *(int16_t*)jl_data_ptr(bits);   break;
    case  4: *(int32_t*)dest   = *(int32_t*)jl_data_ptr(bits);   break;
    case  8: *(int64_t*)dest   = *(int64_t*)jl_data_ptr(bits);   break;
    case 16: *(bits128_t*)dest = *(bits128_t*)jl_data_ptr(bits); break;
    default: memcpy(dest, jl_data_ptr(bits), nb);
    }
}

int jl_field_index(jl_datatype_t *t, jl_sym_t *fld, int err)
{
    jl_tuple_t *fn = t->names;
    for(size_t i=0; i < jl_tuple_len(fn); i++) {
        if (jl_tupleref(fn,i) == (jl_value_t*)fld) {
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
    size_t offs = jl_field_offset(st,i) + sizeof(void*);
    if (st->fields[i].isptr) {
        return *(jl_value_t**)((char*)v + offs);
    }
    return jl_new_bits((jl_datatype_t*)jl_tupleref(st->types,i),
                       (char*)v + offs);
}

int jl_field_isdefined(jl_value_t *v, jl_sym_t *fld, int err)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    int i = jl_field_index(st, fld, err);
    if (i == -1) return 0;
    size_t offs = jl_field_offset(st,i) + sizeof(void*);
    if (st->fields[i].isptr) {
        return *(jl_value_t**)((char*)v + offs) != NULL;
    }
    return 1;
}

jl_value_t *jl_set_nth_field(jl_value_t *v, size_t i, jl_value_t *rhs)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t offs = jl_field_offset(st,i) + sizeof(void*);
    if (st->fields[i].isptr) {
        *(jl_value_t**)((char*)v + offs) = rhs;
    }
    else {
        jl_assign_bits((char*)v + offs, rhs);
    }
    return rhs;
}

DLLEXPORT jl_value_t *jl_new_struct(jl_datatype_t *type, ...)
{
    if (type->instance != NULL) return type->instance;
    va_list args;
    size_t nf = jl_tuple_len(type->names);
    va_start(args, type);
    jl_value_t *jv = newstruct(type);
    for(size_t i=0; i < nf; i++) {
        jl_set_nth_field(jv, i, va_arg(args, jl_value_t*));
    }
    if (type->size == 0) type->instance = jv;
    va_end(args);
    return jv;
}

DLLEXPORT jl_value_t *jl_new_structv(jl_datatype_t *type, jl_value_t **args, uint32_t na)
{
    if (type->instance != NULL) return type->instance;
    size_t nf = jl_tuple_len(type->names);
    jl_value_t *jv = newstruct(type);
    for(size_t i=0; i < na; i++) {
        jl_set_nth_field(jv, i, args[i]);
    }
    for(size_t i=na; i < nf; i++) {
        if (type->fields[i].isptr)
            *(jl_value_t**)((char*)jv+jl_field_offset(type,i)+sizeof(void*)) = NULL;
    }
    if (type->size == 0) type->instance = jv;
    return jv;
}

DLLEXPORT jl_value_t *jl_new_struct_uninit(jl_datatype_t *type)
{
    if (type->instance != NULL) return type->instance;
    jl_value_t *jv = newstruct(type);
    if (type->size == 0) type->instance = jv;
    else memset(&((void**)jv)[1], 0, type->size);
    return jv;
}

jl_tuple_t *jl_tuple(size_t n, ...)
{
    va_list args;
    if (n == 0) return jl_null;
    va_start(args, n);
    jl_tuple_t *jv = (jl_tuple_t*)newobj((jl_value_t*)jl_tuple_type, n+1);
    jl_tuple_set_len_unsafe(jv, n);
    for(size_t i=0; i < n; i++) {
        jl_tupleset(jv, i, va_arg(args, jl_value_t*));
    }
    va_end(args);
    return jv;
}

jl_tuple_t *jl_tuple1(void *a)
{
    jl_tuple_t *t = (jl_tuple_t*)alloc_3w();
    t->type = (jl_value_t*)jl_tuple_type;
    jl_tuple_set_len_unsafe(t, 1);
    jl_tupleset(t, 0, a);
    return t;
}

jl_tuple_t *jl_tuple2(void *a, void *b)
{
    jl_tuple_t *t = (jl_tuple_t*)alloc_4w();
    t->type = (jl_value_t*)jl_tuple_type;
    jl_tuple_set_len_unsafe(t, 2);
    jl_tupleset(t, 0, a);
    jl_tupleset(t, 1, b);
    return t;
}

jl_tuple_t *jl_alloc_tuple_uninit(size_t n)
{
    if (n == 0) return jl_null;
    jl_tuple_t *jv = (jl_tuple_t*)newobj((jl_value_t*)jl_tuple_type, n+1);
    jl_tuple_set_len_unsafe(jv, n);
    return jv;
}

jl_tuple_t *jl_alloc_tuple(size_t n)
{
    if (n == 0) return jl_null;
    jl_tuple_t *jv = jl_alloc_tuple_uninit(n);
    for(size_t i=0; i < n; i++) {
        jl_tupleset(jv, i, NULL);
    }
    return jv;
}

jl_tuple_t *jl_tuple_append(jl_tuple_t *a, jl_tuple_t *b)
{
    jl_tuple_t *c = jl_alloc_tuple_uninit(jl_tuple_len(a) + jl_tuple_len(b));
    size_t i=0, j;
    for(j=0; j < jl_tuple_len(a); j++) {
        jl_tupleset(c, i, jl_tupleref(a,j));
        i++;
    }
    for(j=0; j < jl_tuple_len(b); j++) {
        jl_tupleset(c, i, jl_tupleref(b,j));
        i++;
    }
    return c;
}

jl_tuple_t *jl_tuple_fill(size_t n, jl_value_t *v)
{
    if (n==0) return jl_null;
    jl_tuple_t *tup = jl_alloc_tuple_uninit(n);
    for(size_t i=0; i < n; i++) {
        jl_tupleset(tup, i, v);
    }
    return tup;
}

DLLEXPORT jl_function_t *jl_new_closure(jl_fptr_t fptr, jl_value_t *env,
                                        jl_lambda_info_t *linfo)
{
    jl_function_t *f = (jl_function_t*)alloc_4w();
    f->type = (jl_value_t*)jl_function_type;
    f->fptr = (fptr!=NULL ? fptr : linfo->fptr);
    f->env = env;
    f->linfo = linfo;
    return f;
}

DLLEXPORT
jl_lambda_info_t *jl_new_lambda_info(jl_value_t *ast, jl_tuple_t *sparams)
{
    jl_lambda_info_t *li =
        (jl_lambda_info_t*)newobj((jl_value_t*)jl_lambda_info_type,
                                  LAMBDA_INFO_NW);
    li->ast = ast;
    li->file = null_sym;
    li->line = 0;
    if (ast != NULL && jl_is_expr(ast)) {
        jl_expr_t *body1 = (jl_expr_t*)jl_exprarg(jl_lam_body((jl_expr_t*)ast),0);
        if (jl_is_expr(body1) && ((jl_expr_t*)body1)->head == line_sym) {
            li->file = (jl_sym_t*)jl_exprarg(body1, 1);
            li->line = jl_unbox_long(jl_exprarg(body1, 0));
        }
    }
    li->module = jl_current_module;
    li->sparams = sparams;
    li->tfunc = (jl_value_t*)jl_null;
    li->fptr = &jl_trampoline;
    li->roots = NULL;
    li->functionObject = NULL;
    li->cFunctionObject = NULL;
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

static jl_sym_t *mk_symbol(const char *str)
{
    jl_sym_t *sym;
    size_t len = strlen(str);

    sym = (jl_sym_t*)malloc((sizeof(jl_sym_t)-sizeof(void*)+len+1+7)&-8);
    sym->type = (jl_value_t*)jl_sym_type;
    sym->left = sym->right = NULL;
#ifdef _P64
    sym->hash = memhash(str, len)^0xAAAAAAAAAAAAAAAAL;
#else
    sym->hash = memhash32(str, len)^0xAAAAAAAA;
#endif
    strcpy(&sym->name[0], str);
    return sym;
}

static void unmark_symbols_(jl_sym_t *root)
{
    while (root != NULL) {
        root->type = (jl_value_t*)(((uptrint_t)root->type)&~1UL);
        unmark_symbols_(root->left);
        root = root->right;
    }
}

void jl_unmark_symbols(void) { unmark_symbols_(symtab); }

static jl_sym_t **symtab_lookup(jl_sym_t **ptree, const char *str)
{
    int x;

    while(*ptree != NULL) {
        x = strcmp(str, (*ptree)->name);
        if (x == 0)
            return ptree;
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

    pnode = symtab_lookup(&symtab, str);
    if (*pnode == NULL)
        *pnode = mk_symbol(str);
    return *pnode;
}

jl_sym_t *jl_symbol_lookup(const char *str)
{
    return *symtab_lookup(&symtab, str);
}

DLLEXPORT jl_sym_t *jl_symbol_n(const char *str, int32_t len)
{
    char name[len+1];
    memcpy(name, str, len);
    name[len] = '\0';
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

DLLEXPORT jl_sym_t *jl_tagged_gensym(const char* str, int32_t len)
{
    static char gs_name[14];
    char name[sizeof(gs_name)+len+3];
    char *n;
    name[0] = '#'; name[1] = '#'; name[2+len] = '#';
    memcpy(name+2, str, len);
    n = uint2str(gs_name, sizeof(gs_name), gs_ctr, 10);
    memcpy(name+3+len, n, sizeof(gs_name)-(n-gs_name));
    gs_ctr++;
    return jl_symbol(name);
}

// allocating types -----------------------------------------------------------

jl_typename_t *jl_new_typename(jl_sym_t *name)
{
    jl_typename_t *tn=(jl_typename_t*)newobj((jl_value_t*)jl_typename_type, 4);
    tn->name = name;
    tn->module = jl_current_module;
    tn->primary = NULL;
    tn->cache = (jl_value_t*)jl_null;
    return tn;
}

jl_datatype_t *jl_new_abstracttype(jl_value_t *name, jl_datatype_t *super,
                                   jl_tuple_t *parameters)
{
    jl_datatype_t *dt = jl_new_datatype((jl_sym_t*)name, super, parameters, jl_null, jl_null, 1, 0);
    dt->pointerfree = 0;
    return dt;
}

jl_function_t *jl_instantiate_method(jl_function_t *f, jl_tuple_t *sp);

void jl_add_constructors(jl_datatype_t *t)
{
    if (t->name == jl_array_typename) {
        t->fptr = jl_f_no_function;
        return;
    }

    jl_initialize_generic_function((jl_function_t*)t, t->name->name);

    if (t->ctor_factory == (jl_value_t*)jl_nothing ||
        t->ctor_factory == (jl_value_t*)jl_null) {
    }
    else {
        assert(jl_tuple_len(t->parameters) > 0);
        if (t != (jl_datatype_t*)t->name->primary) {
            // instantiating
            assert(jl_is_function(t->ctor_factory));
            
            // add type's static parameters to the ctor factory
            size_t np = jl_tuple_len(t->parameters);
            jl_tuple_t *sparams = jl_alloc_tuple_uninit(np*2);
            jl_function_t *cfactory = NULL;
            JL_GC_PUSH(&sparams, &cfactory);
            for(size_t i=0; i < np; i++) {
                jl_tupleset(sparams, i*2+0,
                            jl_tupleref(((jl_datatype_t*)t->name->primary)->parameters, i));
                jl_tupleset(sparams, i*2+1,
                            jl_tupleref(t->parameters, i));
            }
            cfactory = jl_instantiate_method((jl_function_t*)t->ctor_factory,
                                             sparams);
            cfactory->linfo->ast = jl_prepare_ast(cfactory->linfo,
                                                  cfactory->linfo->sparams);
            
            // call user-defined constructor factory on (type,)
            jl_value_t *cfargs[1] = { (jl_value_t*)t };
            jl_apply(cfactory, cfargs, 1);
            JL_GC_POP();
        }
    }
}

JL_CALLABLE(jl_f_ctor_trampoline)
{
    jl_add_constructors((jl_datatype_t*)F);
    return jl_apply((jl_function_t*)F, args, nargs);
}

jl_datatype_t *jl_new_uninitialized_datatype(size_t nfields)
{
    return (jl_datatype_t*)
        newobj((jl_value_t*)jl_datatype_type,
               NWORDS(sizeof(jl_datatype_t) - sizeof(void*) +
                      (nfields-1)*sizeof(jl_fielddesc_t)));
}

void jl_compute_field_offsets(jl_datatype_t *st)
{
    size_t sz = 0, alignm = 0;
    int ptrfree = 1;

    for(size_t i=0; i < jl_tuple_len(st->types); i++) {
        jl_value_t *ty = jl_tupleref(st->types, i);
        size_t fsz, al;
        if (jl_is_bitstype(ty)) {
            fsz = jl_datatype_size(ty);
            al = fsz;   // alignment == size for bits types
            st->fields[i].isptr = 0;
        }
        else {
            fsz = sizeof(void*);
            al = fsz;
            st->fields[i].isptr = 1;
            ptrfree = 0;
        }
        sz = LLT_ALIGN(sz, al);
        if (al > alignm)
            alignm = al;
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
                               jl_tuple_t *parameters,
                               jl_tuple_t *fnames, jl_tuple_t *ftypes,
                               int abstract, int mutabl)
{
    jl_datatype_t *t=NULL;
    jl_typename_t *tn=NULL;
    JL_GC_PUSH(&t, &tn);

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
    if (t == NULL) {
        t = jl_new_uninitialized_datatype(jl_tuple_len(fnames));
        if (jl_is_typename(name))
            tn = (jl_typename_t*)name;
        else
            tn = jl_new_typename((jl_sym_t*)name);
        t->name = tn;
    }

    if (t->name->primary == NULL)
        t->name->primary = (jl_value_t*)t;
    t->super = super;
    t->parameters = parameters;
    t->names = fnames;
    t->types = ftypes;
    t->abstract = abstract;
    t->mutabl = mutabl;
    t->pointerfree = 0;
    t->fptr = jl_f_no_function;
    t->env = (jl_value_t*)t;
    t->linfo = NULL;
    t->ctor_factory = (jl_value_t*)jl_null;
    t->instance = NULL;
    t->struct_decl = NULL;
    t->size = 0;
    if (abstract || parameters->length > 0) {
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
                               jl_tuple_t *parameters, size_t nbits)
{
    jl_datatype_t *bt = jl_new_datatype((jl_sym_t*)name, super, parameters,
                                        jl_null, jl_null, 0, 0);
    bt->size = nbits/8;
    bt->pointerfree = 1;
    return bt;
}

jl_uniontype_t *jl_new_uniontype(jl_tuple_t *types)
{
    jl_uniontype_t *t = (jl_uniontype_t*)newobj((jl_value_t*)jl_uniontype_type,1);
    // don't make unions of 1 type; Union(T)==T
    assert(jl_tuple_len(types) != 1);
    t->types = types;
    return t;
}

// type constructor -----------------------------------------------------------

jl_typector_t *jl_new_type_ctor(jl_tuple_t *params, jl_value_t *body)
{
    jl_typector_t *tc = (jl_typector_t*)newobj((jl_value_t*)jl_typector_type,2);
    tc->parameters = params;
    tc->body = body;
    return (jl_typector_t*)tc;
}

// bits constructors ----------------------------------------------------------

#define BOXN_FUNC(nb,nw)                                        \
jl_value_t *jl_box##nb(jl_datatype_t *t, int##nb##_t x)         \
{                                                               \
    assert(jl_is_bitstype(t));                                  \
    assert(jl_datatype_size(t) == sizeof(x));                   \
    jl_value_t *v = alloc_##nw##w();                            \
    v->type = (jl_value_t*)t;                                   \
    *(int##nb##_t*)jl_data_ptr(v) = x;                          \
    return v;                                                   \
}
BOXN_FUNC(8,  2)
BOXN_FUNC(16, 2)
BOXN_FUNC(32, 2)
#ifdef _P64
BOXN_FUNC(64, 2)
#else
BOXN_FUNC(64, 3)
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

#define BOX_FUNC(typ,c_type,pfx,nw)             \
jl_value_t *pfx##_##typ(c_type x)               \
{                                               \
    jl_value_t *v = alloc_##nw##w();            \
    v->type = (jl_value_t*)jl_##typ##_type;     \
    *(c_type*)jl_data_ptr(v) = x;               \
    return v;                                   \
}
BOX_FUNC(float32, float,  jl_box, 2)
BOX_FUNC(voidpointer, void*,  jl_box, 2) //2 pointers == two words on all platforms
#ifdef _P64
BOX_FUNC(float64, double, jl_box, 2)
#else
BOX_FUNC(float64, double, jl_box, 3)
#endif

#define NBOX_C 1024

#define SIBOX_FUNC(typ,c_type,nw)                       \
static jl_value_t *boxed_##typ##_cache[NBOX_C];         \
jl_value_t *jl_box_##typ(c_type x)                      \
{                                                       \
    c_type idx = x+NBOX_C/2;                            \
    if ((u##c_type)idx < (u##c_type)NBOX_C)             \
        return boxed_##typ##_cache[idx];                \
    jl_value_t *v = alloc_##nw##w();                    \
    v->type = (jl_value_t*)jl_##typ##_type;             \
    *(c_type*)jl_data_ptr(v) = x;                       \
    return v;                                           \
}
#define UIBOX_FUNC(typ,c_type,nw)               \
static jl_value_t *boxed_##typ##_cache[NBOX_C]; \
jl_value_t *jl_box_##typ(c_type x)              \
{                                               \
    if (x < NBOX_C)                             \
        return boxed_##typ##_cache[x];          \
    jl_value_t *v = alloc_##nw##w();            \
    v->type = (jl_value_t*)jl_##typ##_type;     \
    *(c_type*)jl_data_ptr(v) = x;               \
    return v;                                   \
}
SIBOX_FUNC(int16,  int16_t, 2)
SIBOX_FUNC(int32,  int32_t, 2)
UIBOX_FUNC(uint16, uint16_t, 2)
UIBOX_FUNC(uint32, uint32_t, 2)
UIBOX_FUNC(char,   uint32_t, 2)
#ifdef _P64
SIBOX_FUNC(int64,  int64_t, 2)
UIBOX_FUNC(uint64, uint64_t, 2)
#else
SIBOX_FUNC(int64,  int64_t, 3)
UIBOX_FUNC(uint64, uint64_t, 3)
#endif

static jl_value_t *boxed_int8_cache[256];
jl_value_t *jl_box_int8(int32_t x)
{
    return boxed_int8_cache[(uint8_t)x];
}
static jl_value_t *boxed_uint8_cache[256];
jl_value_t *jl_box_uint8(uint32_t x)
{
    return boxed_uint8_cache[(uint8_t)x];
}

void jl_init_int32_int64_cache(void)
{
    int64_t i;
    for(i=0; i < NBOX_C; i++) {
        boxed_int32_cache[i]  = jl_box32(jl_int32_type, i-NBOX_C/2);
        boxed_int64_cache[i]  = jl_box64(jl_int64_type, i-NBOX_C/2);
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
    JL_GC_PUSH(&ar);
    jl_expr_t *ex = (jl_expr_t*)alloc_4w();
    ex->type = (jl_value_t*)jl_expr_type;
    ex->head = head;
    ex->args = ar;
    ex->etype = (jl_value_t*)jl_any_type;
    JL_GC_POP();
    return ex;
}

// this constructor has to be built-in for bootstrapping, because we can't
// do anything without being able to make Exprs.
JL_CALLABLE(jl_f_new_expr)
{
    JL_NARGSV(Expr, 1);
    JL_TYPECHK(Expr, symbol, args[0]);
    jl_array_t *ar = jl_alloc_cell_1d(nargs-1);
    JL_GC_PUSH(&ar);
    for(size_t i=1; i < nargs; i++)
        jl_cellset(ar, i-1, args[i]);
    jl_expr_t *ex = (jl_expr_t*)alloc_4w();
    ex->type = (jl_value_t*)jl_expr_type;
    ex->head = (jl_sym_t*)args[0];
    ex->args = ar;
    ex->etype = (jl_value_t*)jl_any_type;
    JL_GC_POP();
    return (jl_value_t*)ex;
}

JL_CALLABLE(jl_f_new_box)
{
    JL_NARGS(Box, 1, 1);
    jl_value_t *box = (jl_value_t*)alloc_2w();
    box->type = jl_box_any_type;
    ((jl_value_t**)box)[1] = args[0];
    return box;
}
