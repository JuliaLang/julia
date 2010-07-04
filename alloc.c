/*
  object constructors
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <sys/types.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#ifdef BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"
#include "newobj_internal.h"

jl_value_t *jl_true;
jl_value_t *jl_false;

jl_tag_type_t *jl_undef_type;
jl_tag_type_t *jl_typetype_type;
jl_typector_t *jl_functype_ctor;
jl_struct_type_t *jl_box_type;
jl_type_t *jl_box_any_type;
jl_typename_t *jl_box_typename;

jl_struct_type_t *jl_typector_type;

jl_struct_type_t *jl_array_type;
jl_typename_t *jl_array_typename;
jl_type_t *jl_array_uint8_type;
jl_type_t *jl_array_any_type;
jl_struct_type_t *jl_expr_type;;
jl_bits_type_t *jl_intrinsic_type;
jl_struct_type_t *jl_methtable_type;
jl_struct_type_t *jl_lambda_info_type;

jl_bits_type_t *jl_pointer_type;
jl_bits_type_t *jl_pointer_void_type;
jl_bits_type_t *jl_pointer_uint8_type;

jl_sym_t *call_sym;    jl_sym_t *dots_sym;
jl_sym_t *dollar_sym;  jl_sym_t *quote_sym;
jl_sym_t *tuple_sym;   jl_sym_t *top_sym;
jl_sym_t *expr_sym;
jl_sym_t *line_sym;    jl_sym_t *continue_sym;
// head symbols for each expression type
jl_sym_t *goto_sym;    jl_sym_t *goto_ifnot_sym;
jl_sym_t *label_sym;   jl_sym_t *return_sym;
jl_sym_t *lambda_sym;  jl_sym_t *assign_sym;
jl_sym_t *null_sym;    jl_sym_t *body_sym;
jl_sym_t *unbound_sym; jl_sym_t *boxunbound_sym;
jl_sym_t *locals_sym;  jl_sym_t *colons_sym;
jl_sym_t *closure_ref_sym;
jl_sym_t *symbol_sym;
jl_sym_t *Any_sym;

// NOTE: does not work for TagKind or its subtypes
jl_value_t *jl_new_struct(jl_struct_type_t *type, ...)
{
    va_list args;
    size_t nf = type->names->length;
    size_t i;
    va_start(args, type);
    jl_value_t *jv = newobj((jl_type_t*)type, nf);
    for(i=0; i < nf; i++) {
        ((jl_value_t**)jv)[i+1] = va_arg(args, jl_value_t*);
    }
    va_end(args);
    return jv;
}

jl_tuple_t *jl_tuple(size_t n, ...)
{
    va_list args;
    size_t i;
    if (n == 0) return jl_null;
    va_start(args, n);
    jl_tuple_t *jv = (jl_tuple_t*)newobj((jl_type_t*)jl_tuple_type, n+1);
    jv->length = n;
    for(i=0; i < n; i++) {
        ((jl_value_t**)jv)[i+2] = va_arg(args, jl_value_t*);
    }
    va_end(args);
    return jv;
}

jl_tuple_t *jl_alloc_tuple(size_t n)
{
    if (n == 0) return jl_null;
    jl_tuple_t *jv = (jl_tuple_t*)newobj((jl_type_t*)jl_tuple_type, n+1);
    jv->length = n;
    return jv;
}

jl_tuple_t *jl_tuple_append(jl_tuple_t *a, jl_tuple_t *b)
{
    jl_tuple_t *c = jl_alloc_tuple(a->length + b->length);
    size_t i=0, j;
    for(j=0; j < a->length; j++) {
        jl_tupleset(c, i, jl_tupleref(a,j));
        i++;
    }
    for(j=0; j < b->length; j++) {
        jl_tupleset(c, i, jl_tupleref(b,j));
        i++;
    }
    return c;
}

// convert (a, b, (c, d, (... ()))) to (a, b, c, d, ...)
jl_tuple_t *jl_flatten_pairs(jl_tuple_t *t)
{
    size_t i, n = 0;
    jl_tuple_t *t0 = t;
    while (t != jl_null) {
        n++;
        t = (jl_tuple_t*)jl_nextpair(t);
    }
    jl_tuple_t *nt = jl_alloc_tuple(n*2);
    t = t0;
    for(i=0; i < n*2; i+=2) {
        jl_tupleset(nt, i,   jl_t0(t));
        jl_tupleset(nt, i+1, jl_t1(t));
        t = (jl_tuple_t*)jl_nextpair(t);
    }
    return nt;
}

jl_function_t *jl_new_closure(jl_fptr_t proc, jl_value_t *env)
{
    jl_function_t *f = (jl_function_t*)newobj((jl_type_t*)jl_any_func, 3);
    f->fptr = proc;
    f->env = env;
    f->linfo = NULL;
    return f;
}

jl_lambda_info_t *jl_new_lambda_info(jl_value_t *ast, jl_tuple_t *sparams)
{
    jl_lambda_info_t *li =
        (jl_lambda_info_t*)newobj((jl_type_t*)jl_lambda_info_type, 9);
    li->ast = ast;
    li->sparams = sparams;
    li->tfunc = (jl_value_t*)jl_null;
    li->fptr = NULL;
    li->roots = jl_null;
    li->functionObject = NULL;
    li->inInference = 0;
    li->unspecialized = NULL;
    li->name = jl_symbol("anonymous");
    return li;
}

// symbols --------------------------------------------------------------------

static jl_sym_t *symtab = NULL;

static jl_sym_t *mk_symbol(const char *str)
{
    jl_sym_t *sym;
    size_t len = strlen(str);

    sym = (jl_sym_t*)allocb(sizeof(jl_sym_t)-sizeof(void*) + len + 1);
    sym->type = (jl_type_t*)jl_sym_type;
    sym->left = sym->right = NULL;
#ifdef BITS64
    sym->hash = memhash(str, len)^0xAAAAAAAAAAAAAAAAL;
#else
    sym->hash = memhash32(str, len)^0xAAAAAAAA;
#endif
    strcpy(&sym->name[0], str);
    return sym;
}

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

DLLEXPORT jl_sym_t *jl_symbol(const char *str)
{
    jl_sym_t **pnode;

    pnode = symtab_lookup(&symtab, str);
    if (*pnode == NULL)
        *pnode = mk_symbol(str);
    return *pnode;
}

jl_sym_t *jl_gensym()
{
    static uint32_t gs_ctr = 0;  // TODO: per-thread
    char name[32];
    char *n;
    n = uint2str(name, sizeof(name)-1, gs_ctr, 10);
    *(--n) = 'g';
    gs_ctr++;
    return mk_symbol(n);
}

// allocating types -----------------------------------------------------------

jl_typename_t *jl_new_typename(jl_sym_t *name)
{
    jl_typename_t *tn=(jl_typename_t*)newobj((jl_type_t*)jl_typename_type, 2);
    tn->name = name;
    tn->primary = NULL;
    return tn;
}

static void unbind_tvars(jl_tuple_t *parameters)
{
    size_t i;
    for(i=0; i < parameters->length; i++) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_tupleref(parameters, i);
        if (jl_is_typevar(tv))
            tv->unbound = 1;
    }
}

jl_tag_type_t *jl_new_tagtype(jl_value_t *name, jl_tag_type_t *super,
                              jl_tuple_t *parameters)
{
    jl_tag_type_t *t = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind,
                                              TAG_TYPE_NW);
    if (jl_is_typename(name))
        t->name = (jl_typename_t*)name;
    else
        t->name = jl_new_typename((jl_sym_t*)name);
    t->super = super;
    unbind_tvars(parameters);
    t->parameters = parameters;
    if (t->name->primary == NULL)
        t->name->primary = (jl_value_t*)t;
    return t;
}

jl_func_type_t *jl_new_functype(jl_type_t *a, jl_type_t *b)
{
    jl_func_type_t *t = (jl_func_type_t*)newobj((jl_type_t*)jl_func_kind, 2);
    if (!jl_is_tuple(a) && !jl_is_typevar(a))
        a = (jl_type_t*)jl_tuple(1, a);
    t->from = a;
    t->to = b;
    return t;
}

JL_CALLABLE(jl_new_struct_internal);
JL_CALLABLE(jl_generic_ctor);

void jl_initialize_generic_function(jl_function_t *f, jl_sym_t *name);

static void add_generic_ctor(jl_function_t *gf, jl_struct_type_t *t)
{
    jl_function_t *gmeth = jl_new_closure(jl_generic_ctor,(jl_value_t*)jl_null);
    jl_tuple_t *ntvs = jl_alloc_tuple(t->parameters->length);
    size_t i;
    // create new typevars, so the function has its own constraint
    // environment.
    for(i=0; i < t->parameters->length; i++) {
        jl_value_t *tv = jl_tupleref(t->parameters, i);
        if (jl_is_typevar(tv)) {
            jl_tupleset(ntvs, i,
                        (jl_value_t*)jl_new_typevar(((jl_tvar_t*)tv)->name,
                                                    ((jl_tvar_t*)tv)->lb,
                                                    ((jl_tvar_t*)tv)->ub));
        }
        else {
            jl_tupleset(ntvs, i, tv);
        }
    }
    t = (jl_struct_type_t*)jl_apply_type((jl_value_t*)t, ntvs);
    gmeth->linfo = jl_new_lambda_info(NULL, jl_null);
    jl_add_method(gf, t->types, gmeth);
    gmeth->env = (jl_value_t*)jl_pair((jl_value_t*)gmeth, (jl_value_t*)t);
    if (!jl_is_struct_type(gf)) {
        gf->type =
            (jl_type_t*)jl_new_functype((jl_type_t*)t->types, (jl_type_t*)t);
    }
}

JL_CALLABLE(jl_new_array_internal);

void jl_specialize_ast(jl_lambda_info_t *li);

void jl_add_constructors(jl_struct_type_t *t)
{
    if (t->name == jl_array_typename) {
        t->fptr = jl_new_array_internal;
        return;
    }
    if (t->ctor_factory == (jl_value_t*)jl_null) {
        // no user-defined constructors
        if (t->parameters->length>0 && (jl_value_t*)t==t->name->primary) {
            jl_function_t *tf = (jl_function_t*)t;
            jl_initialize_generic_function(tf, t->name->name);
            add_generic_ctor(tf, t);
            jl_gf_mtable(tf)->sealed = 1;
        }
        else {
            t->fptr = jl_new_struct_internal;
        }
    }
    else {
        // call user-defined constructor factory on (type, new)
        // where new is a default constructor
        jl_function_t *fnew;
        if (t->parameters->length>0 && (jl_value_t*)t==t->name->primary) {
            fnew = jl_new_generic_function(t->name->name);
            add_generic_ctor(fnew, t);
            jl_gf_mtable(fnew)->sealed = 1;
        }
        else {
            fnew = jl_new_closure(jl_new_struct_internal, (jl_value_t*)t);
            fnew->type =
                (jl_type_t*)jl_new_functype((jl_type_t*)t->types,
                                            (jl_type_t*)t);
        }
        // in this case the type itself always works as a generic function,
        // to accomodate the user's various definitions
        jl_initialize_generic_function((jl_function_t*)t, t->name->name);
        assert(jl_is_function(t->ctor_factory));
        jl_value_t *cfargs[2];
        cfargs[0] = (jl_value_t*)t;
        cfargs[1] = (jl_value_t*)fnew;
        jl_apply((jl_function_t*)t->ctor_factory, cfargs, 2);
        // TODO: if we want to, here we could feed the type's static parameters
        // to the methods for their use.
        jl_gf_mtable(t)->sealed = 1;

        // calling ctor_factory binds the type of new() to a static parameter
        // visible to each of the constructor methods. eagerly specialize
        // the ASTs for all constructor methods so that type inference can
        // see the type of new() even before any constructors have been called.
        jl_methlist_t *ml = jl_gf_mtable(t)->defs;
        while (ml != NULL) {
            jl_specialize_ast(ml->func->linfo);
            ml = ml->next;
        }
    }
}

JL_CALLABLE(jl_constructor_factory_trampoline)
{
    jl_struct_type_t *t = (jl_struct_type_t*)env;
    jl_add_constructors(t);
    return jl_apply((jl_function_t*)t, args, nargs);
}

jl_struct_type_t *jl_new_struct_type(jl_sym_t *name, jl_tag_type_t *super,
                                     jl_tuple_t *parameters,
                                     jl_tuple_t *fnames, jl_tuple_t *ftypes)
{
    jl_struct_type_t *t = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind,
                                                    STRUCT_TYPE_NW);
    t->name = jl_new_typename(name);
    t->name->primary = (jl_value_t*)t;
    t->super = super;
    unbind_tvars(parameters);
    t->parameters = parameters;
    t->names = fnames;
    t->types = ftypes;
    t->fptr = jl_constructor_factory_trampoline;
    t->env = (jl_value_t*)t;
    t->linfo = NULL;
    t->ctor_factory = (jl_value_t*)jl_null;
    t->instance = NULL;
    if (jl_has_typevars((jl_value_t*)parameters))
        t->uid = 0;
    else
        t->uid = jl_assign_type_uid();
    return t;
}

jl_bits_type_t *jl_new_bitstype(jl_value_t *name, jl_tag_type_t *super,
                                jl_tuple_t *parameters, size_t nbits)
{
    jl_bits_type_t *t = (jl_bits_type_t*)newobj((jl_type_t*)jl_bits_kind,
                                                BITS_TYPE_NW);
    if (jl_is_typename(name))
        t->name = (jl_typename_t*)name;
    else
        t->name = jl_new_typename((jl_sym_t*)name);
    t->super = super;
    unbind_tvars(parameters);
    t->parameters = parameters;
    t->nbits = nbits;
    if (jl_has_typevars((jl_value_t*)parameters))
        t->uid = 0;
    else
        t->uid = jl_assign_type_uid();
    if (t->name->primary == NULL)
        t->name->primary = (jl_value_t*)t;
    return t;
}

DLLEXPORT
int jl_union_too_complex(jl_tuple_t *types)
{
    size_t i, j;
    for(i=0; i < types->length; i++) {
        for(j=0; j < types->length; j++) {
            if (j != i) {
                jl_value_t *a = jl_tupleref(types, i);
                jl_value_t *b = jl_tupleref(types, j);
                if (jl_has_typevars(b) &&
                    (!jl_is_typevar(b) || jl_has_typevars(a))) {
                    jl_value_t *env =
                        jl_type_match((jl_type_t*)a, (jl_type_t*)b);
                    if (env != jl_false && env != (jl_value_t*)jl_null)
                        return 1;
                }
            }
        }
    }
    return 0;
}

jl_uniontype_t *jl_new_uniontype(jl_tuple_t *types)
{
    if (jl_union_too_complex(types)) {
        jl_errorf("union type pattern too complex: %s",
                  jl_print_to_string((jl_value_t*)types));
    }
    jl_uniontype_t *t = (jl_uniontype_t*)newobj((jl_type_t*)jl_union_kind, 1);
    // don't make unions of 1 type; Union(T)==T
    assert(types->length != 1);
    t->types = types;
    return t;
}

// type constructor -----------------------------------------------------------

jl_typector_t *jl_new_type_ctor(jl_tuple_t *params, jl_type_t *body)
{
    jl_typector_t *tc = (jl_typector_t*)newobj((jl_type_t*)jl_typector_type,2);
    unbind_tvars(params);
    tc->parameters = params;
    tc->body = body;
    return (jl_typector_t*)tc;
}

// struct constructors --------------------------------------------------------

// this one is for fully-instantiated types where we know the field types,
// and arguments are converted to the field types.
JL_CALLABLE(jl_new_struct_internal)
{
    jl_struct_type_t *t = (jl_struct_type_t*)env;
    size_t nf = t->names->length;
    if (nargs < nf)
        jl_error("too few arguments to constructor");
    else if (nargs > nf)
        jl_error("too many arguments to constructor");
    if (t->instance != NULL)
        return t->instance;
    jl_value_t *v = newobj((jl_type_t*)t, nf);
    size_t i;
    for(i=0; i < nargs; i++) {
        ((jl_value_t**)v)[i+1] = jl_convert((jl_type_t*)jl_tupleref(t->types,i),
                                            args[i]);
    }
    if (nf == 0)
        t->instance = v;
    return v;
}

// this one infers type parameters from the arguments and instantiates
// (with caching) the right type, then constructs an instance.
JL_CALLABLE(jl_generic_ctor)
{
    jl_function_t *self  = (jl_function_t*)jl_t0(env);
    jl_struct_type_t *ty = (jl_struct_type_t*)jl_t1(env);
    jl_lambda_info_t *li = self->linfo;
    // we cache the instantiated type in li->ast
    if (li->ast == NULL) {
        if (li->sparams->length != ty->parameters->length*2) {
            jl_errorf("%s: type parameters cannot be inferred from arguments",
                      ty->name->name->name);
        }
        li->ast =
            (jl_value_t*)jl_instantiate_type_with((jl_type_t*)ty,
                                                  &jl_tupleref(li->sparams,0),
                                                  li->sparams->length/2);
    }
    jl_struct_type_t *tp = (jl_struct_type_t*)li->ast;
    if (tp->instance != NULL)
        return tp->instance;
    jl_value_t *v = newobj((jl_type_t*)tp, tp->names->length);
    size_t i;
    for(i=0; i < nargs; i++) {
        ((jl_value_t**)v)[i+1] = args[i];
    }
    if (nargs == 0)
        tp->instance = v;
    return v;
}

// bits constructors ----------------------------------------------------------

#define BOX_FUNC(type,c_type,pfx)                                       \
jl_value_t *pfx##_##type(c_type x)                                      \
{                                                                       \
    jl_value_t *v = newobj((jl_type_t*)jl_##type##_type,                \
                           NWORDS(LLT_ALIGN(sizeof(c_type),sizeof(void*)))); \
    *(c_type*)jl_bits_data(v) = x;                                      \
    return v;                                                           \
}
BOX_FUNC(int8,    int8_t,   jl_new_box)
BOX_FUNC(uint8,   uint8_t,  jl_new_box)
BOX_FUNC(int16,   int16_t,  jl_new_box)
BOX_FUNC(uint16,  uint16_t, jl_new_box)
BOX_FUNC(int32,   int32_t,  jl_new_box)
BOX_FUNC(uint32,  uint32_t, jl_new_box)
BOX_FUNC(int64,   int64_t,  jl_new_box)
BOX_FUNC(uint64,  uint64_t, jl_new_box)
BOX_FUNC(float32, float,    jl_box)
BOX_FUNC(float64, double,   jl_box)

#define SIBOX_FUNC(type,c_type)                                         \
static jl_value_t *boxed_##type##_cache[1024];                          \
jl_value_t *jl_box_##type(c_type x)                                     \
{                                                                       \
    if ((u##c_type)(x+512) < 1024)                                      \
        return boxed_##type##_cache[(x+512)];                           \
    jl_value_t *v = newobj((jl_type_t*)jl_##type##_type,                \
                           NWORDS(LLT_ALIGN(sizeof(c_type),sizeof(void*)))); \
    *(c_type*)jl_bits_data(v) = x;                                      \
    return v;                                                           \
}
#define UIBOX_FUNC(type,c_type)                                         \
static jl_value_t *boxed_##type##_cache[1024];                          \
jl_value_t *jl_box_##type(c_type x)                                     \
{                                                                       \
    if (x < 1024)                                                       \
        return boxed_##type##_cache[x];                                 \
    jl_value_t *v = newobj((jl_type_t*)jl_##type##_type,                \
                           NWORDS(LLT_ALIGN(sizeof(c_type),sizeof(void*)))); \
    *(c_type*)jl_bits_data(v) = x;                                      \
    return v;                                                           \
}
SIBOX_FUNC(int16,  int16_t)
SIBOX_FUNC(int32,  int32_t)
SIBOX_FUNC(int64,  int64_t)
UIBOX_FUNC(uint16, uint16_t)
UIBOX_FUNC(uint32, uint32_t)
UIBOX_FUNC(uint64, uint64_t)

static jl_value_t *boxed_int8_cache[256];
jl_value_t *jl_box_int8(int8_t x)
{
    return boxed_int8_cache[((int32_t)x)+128];
}
static jl_value_t *boxed_uint8_cache[256];
jl_value_t *jl_box_uint8(uint8_t x)
{
    return boxed_uint8_cache[x];
}

static void init_box_caches()
{
    int64_t i;
    for(i=0; i < 256; i++) {
        boxed_int8_cache[i]  = jl_new_box_int8((int8_t)(i-128));
        boxed_uint8_cache[i] = jl_new_box_uint8(i);
    }
    for(i=0; i < 1024; i++) {
        boxed_int16_cache[i]  = jl_new_box_int16(i-512);
        boxed_int32_cache[i]  = jl_new_box_int32(i-512);
        boxed_int64_cache[i]  = jl_new_box_int64(i-512);
        boxed_uint16_cache[i] = jl_new_box_uint16(i);
        boxed_uint32_cache[i] = jl_new_box_uint32(i);
        boxed_uint64_cache[i] = jl_new_box_uint64(i);
    }
}

jl_value_t *jl_box_bool(int8_t x)
{
    if (x)
        return jl_true;
    return jl_false;
}

#define UNBOX_FUNC(j_type,c_type)                       \
c_type jl_unbox_##j_type(jl_value_t *v)                 \
{                                                       \
    assert(v->type == (jl_type_t*)jl_##j_type##_type);  \
    return *(c_type*)jl_bits_data(v);                   \
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

jl_value_t *jl_box_pointer(jl_bits_type_t *ty, void *p)
{
    jl_value_t *v = newobj((jl_type_t*)ty, 1);
    *(void**)jl_bits_data(v) = p;
    return v;
}

void *jl_unbox_pointer(jl_value_t *v)
{
    assert(jl_is_cpointer(v));
    return *(void**)jl_bits_data(v);
}

// array constructors ---------------------------------------------------------

jl_array_t *jl_new_array(jl_type_t *atype, jl_value_t **dimargs, size_t ndims)
{
    size_t nel=1;
    jl_array_t *a = allocb(sizeof(jl_array_t) +
                           (ndims?(ndims-1):0)*sizeof(jl_value_t*));
    a->type = atype;
    a->dims = &a->_dims;
    a->_dims.type = (jl_type_t*)jl_tuple_type;
    a->_dims.length = ndims;
    size_t i;
    if (ndims == 0) nel = 0;
    for(i=0; i < ndims; i++) {
        assert(jl_is_int32(dimargs[i]));
        size_t d = jl_unbox_int32(dimargs[i]);
        nel *= d;
        jl_tupleset(a->dims, i, dimargs[i]);
    }
    a->length = nel;

    jl_type_t *el_type = (jl_type_t*)jl_tparam0(atype);
    void *data;

    if (nel > 0) {
        if (jl_is_bits_type(el_type)) {
            size_t tot = ((jl_bits_type_t*)el_type)->nbits/8 * nel;
            if (tot <= ARRAY_INLINE_NBYTES) {
                data = &a->_space[0];
            }
            else {
                data = alloc_pod(tot);
            }
        }
        else {
            size_t tot = sizeof(void*) * nel;
            if (tot <= ARRAY_INLINE_NBYTES) {
                data = &a->_space[0];
            }
            else {
#ifdef BOEHM_GC
                if (tot >= 100000)
                    data = GC_malloc_ignore_off_page(tot);
                else
#endif
                    data = allocb(tot);
            }
            memset(data, 0, tot);
        }
    }
    else {
        data = NULL;
    }
    a->data = data;

    return a;
}

JL_CALLABLE(jl_new_array_internal)
{
    jl_struct_type_t *atype = (jl_struct_type_t*)env;
    jl_value_t *ndims = jl_tupleref(atype->parameters,1);
    if (!jl_is_int32(ndims))
        jl_errorf("Array: incomplete type %s",
                  jl_print_to_string((jl_value_t*)atype));
    size_t nd = jl_unbox_int32(ndims);
    JL_NARGS(Array, nd, nd);
    size_t i;
    for(i=0; i < nargs; i++) {
        JL_TYPECHK(Array, int32, args[i]);
    }
    return (jl_value_t*)jl_new_array((jl_type_t*)atype, args, nargs);
}

JL_CALLABLE(jl_generic_array_ctor)
{
    JL_NARGSV(Array, 1);
    JL_TYPECHK(Array, type, args[0]);
    if (nargs==2 && jl_is_tuple(args[1])) {
        jl_tuple_t *d = (jl_tuple_t*)args[1];
        return jl_new_array_internal
            ((jl_value_t*)
             jl_apply_type((jl_value_t*)jl_array_type,
                           jl_tuple(2, args[0],
                                    jl_box_int32(d->length))),
             &jl_tupleref(d,0), d->length);
    }
    return jl_new_array_internal((jl_value_t*)
                                 jl_apply_type((jl_value_t*)jl_array_type,
                                               jl_tuple(2, args[0],
                                                        jl_box_int32(nargs-1))),
                                 &args[1], nargs-1);
}

jl_array_t *jl_cstr_to_array(char *str)
{
    size_t n = strlen(str);
    jl_value_t *dims = jl_box_int32(n+1);
    jl_array_t *a = jl_new_array(jl_array_uint8_type, &dims, 1);
    strcpy(a->data, str);
    // '\0' terminator is there, but hidden from julia
    a->length--;
    jl_tupleset(a->dims, 0, jl_box_int32(n));
    return a;
}

jl_array_t *jl_alloc_cell_1d(size_t n)
{
    jl_value_t *sz = jl_box_int32(n);
    return jl_new_array(jl_array_any_type, &sz, 1);
}

// initialization -------------------------------------------------------------

void jl_init_builtin_types()
{
    jl_false = jl_new_box_int8(0); jl_false->type = (jl_type_t*)jl_bool_type;
    jl_true  = jl_new_box_int8(1); jl_true->type  = (jl_type_t*)jl_bool_type;

    init_box_caches();

    jl_tuple_t *tv;
    tv = jl_typevars(2, "T", "N");
    jl_array_type = 
        jl_new_struct_type(jl_symbol("Array"),
                           (jl_tag_type_t*)
                           jl_apply_type((jl_value_t*)jl_tensor_type, tv),
                           tv,
                           jl_tuple(1, jl_symbol("dims")),
                           jl_tuple(1, jl_apply_type((jl_value_t*)jl_ntuple_type,
                                                     jl_tuple(2, jl_tupleref(tv,1),
                                                              jl_int32_type))));
    jl_array_typename = jl_array_type->name;
    jl_array_type->fptr = jl_generic_array_ctor;
    jl_array_type->env = NULL;
    jl_array_type->linfo = NULL;

    jl_array_uint8_type =
        (jl_type_t*)jl_apply_type((jl_value_t*)jl_array_type,
                                  jl_tuple(2, jl_uint8_type,
                                           jl_box_int32(1)));

    jl_array_any_type =
        (jl_type_t*)jl_apply_type((jl_value_t*)jl_array_type,
                                  jl_tuple(2, jl_any_type,
                                           jl_box_int32(1)));

    jl_expr_type =
        jl_new_struct_type(jl_symbol("Expr"),
                           jl_any_type, jl_null,
                           jl_tuple(3, jl_symbol("head"), jl_symbol("args"),
                                    jl_symbol("type")),
                           jl_tuple(3, jl_sym_type, jl_array_any_type,
                                    jl_any_type));
    jl_add_constructors(jl_expr_type);

    tv = jl_typevars(1, "T");
    jl_box_type =
        jl_new_struct_type(jl_symbol("Box"),
                           jl_any_type, tv,
                           jl_tuple(1, jl_symbol("contents")), tv);
    jl_add_constructors(jl_box_type);
    jl_box_typename = jl_box_type->name;
    jl_box_any_type =
        (jl_type_t*)jl_apply_type((jl_value_t*)jl_box_type,
                                  jl_tuple(1, jl_any_type));

    jl_lambda_info_type =
        jl_new_struct_type(jl_symbol("LambdaStaticData"),
                           jl_any_type, jl_null,
                           jl_tuple(3, jl_symbol("ast"), jl_symbol("sparams"),
                                    jl_symbol("tfunc")),
                           jl_tuple(3, jl_expr_type, jl_tuple_type,
                                    jl_any_type));
    jl_lambda_info_type->fptr = jl_f_no_function;

    jl_typector_type =
        jl_new_struct_type(jl_symbol("TypeConstructor"),
                           jl_any_type, jl_null,
                           jl_tuple(2, jl_symbol("parameters"),
                                    jl_symbol("body")),
                           jl_tuple(2, jl_tuple_type, jl_any_type));

    tv = jl_typevars(2, "A", "B");
    jl_functype_ctor =
        jl_new_type_ctor(tv,
                         (jl_type_t*)jl_new_functype((jl_type_t*)jl_tupleref(tv,0),
                                                     (jl_type_t*)jl_tupleref(tv,1)));

    jl_intrinsic_type = jl_new_bitstype((jl_value_t*)jl_symbol("IntrinsicFunction"),
                                        jl_any_type, jl_null, 32);

    tv = jl_typevars(1, "T");
    jl_pointer_type =
        jl_new_bitstype((jl_value_t*)jl_symbol("Ptr"), jl_any_type, tv,
#ifdef BITS64
                        64
#else
                        32
#endif
                        );
    jl_pointer_void_type =
        (jl_bits_type_t*)jl_apply_type((jl_value_t*)jl_pointer_type,
                                       jl_tuple(1, jl_bottom_type));
    jl_pointer_uint8_type =
        (jl_bits_type_t*)jl_apply_type((jl_value_t*)jl_pointer_type,
                                       jl_tuple(1, jl_uint8_type));

    jl_undef_type = jl_new_tagtype((jl_value_t*)jl_symbol("Undef"),
                                   jl_any_type, jl_null);

    // Type{Type}
    jl_typetype_type = (jl_tag_type_t*)jl_apply_type((jl_value_t*)jl_type_type,
                                                     jl_tuple(1,jl_type_type));

    call_sym = jl_symbol("call");
    quote_sym = jl_symbol("quote");
    top_sym = jl_symbol("top");
    dots_sym = jl_symbol("...");
    expr_sym = jl_symbol("expr");
    tuple_sym = jl_symbol("tuple");
    dollar_sym = jl_symbol("$");
    line_sym = jl_symbol("line");
    continue_sym = jl_symbol("continue");
    goto_sym = jl_symbol("goto");
    goto_ifnot_sym = jl_symbol("gotoifnot");
    label_sym = jl_symbol("label");
    return_sym = jl_symbol("return");
    lambda_sym = jl_symbol("lambda");
    assign_sym = jl_symbol("=");
    null_sym = jl_symbol("null");
    unbound_sym = jl_symbol("unbound");
    boxunbound_sym = jl_symbol("box-unbound");
    closure_ref_sym = jl_symbol("closure-ref");
    symbol_sym = jl_symbol("symbol");
    body_sym = jl_symbol("body");
    locals_sym = jl_symbol("locals");
    colons_sym = jl_symbol("::");
    Any_sym = jl_symbol("Any");
}
