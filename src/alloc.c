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
#include "llt.h"
#include "julia.h"
#include "newobj_internal.h"

jl_value_t *jl_true;
jl_value_t *jl_false;

jl_tag_type_t *jl_undef_type;
jl_tag_type_t *jl_typetype_type;
jl_typector_t *jl_function_type;
jl_struct_type_t *jl_box_type;
jl_type_t *jl_box_any_type;
jl_typename_t *jl_box_typename;

jl_struct_type_t *jl_typector_type;

jl_struct_type_t *jl_array_type;
jl_typename_t *jl_array_typename;
jl_type_t *jl_array_uint8_type;
jl_type_t *jl_array_any_type;
jl_struct_type_t *jl_weakref_type;
jl_tag_type_t *jl_string_type;
jl_struct_type_t *jl_ascii_string_type;
jl_struct_type_t *jl_utf8_string_type;
jl_struct_type_t *jl_expr_type;
jl_bits_type_t *jl_intrinsic_type;
jl_struct_type_t *jl_methtable_type;
jl_struct_type_t *jl_lambda_info_type;
jl_struct_type_t *jl_errorexception_type=NULL;
jl_struct_type_t *jl_typeerror_type;
jl_struct_type_t *jl_loaderror_type;
jl_struct_type_t *jl_uniontoocomplex_type;
jl_value_t *jl_an_empty_cell=NULL;
jl_value_t *jl_stackovf_exception;
jl_value_t *jl_divbyzero_exception;
jl_value_t *jl_undefref_exception;

jl_bits_type_t *jl_pointer_type;
jl_bits_type_t *jl_pointer_void_type;

jl_sym_t *call_sym;    jl_sym_t *dots_sym;
jl_sym_t *call1_sym;
jl_sym_t *dollar_sym;  jl_sym_t *quote_sym;
jl_sym_t *top_sym;     jl_sym_t *vinf_sym;
jl_sym_t *line_sym;    jl_sym_t *jl_continue_sym;
// head symbols for each expression type
jl_sym_t *goto_sym;    jl_sym_t *goto_ifnot_sym;
jl_sym_t *label_sym;   jl_sym_t *return_sym;
jl_sym_t *lambda_sym;  jl_sym_t *assign_sym;
jl_sym_t *null_sym;    jl_sym_t *body_sym;
jl_sym_t *isbound_sym; jl_sym_t *macro_sym;
jl_sym_t *locals_sym;  jl_sym_t *colons_sym;
jl_sym_t *symbol_sym;  jl_sym_t *unexpanded_sym;
jl_sym_t *Any_sym;     jl_sym_t *method_sym;
jl_sym_t *enter_sym;   jl_sym_t *leave_sym;
jl_sym_t *exc_sym;     jl_sym_t *error_sym;
jl_sym_t *static_typeof_sym;
jl_sym_t *new_sym;

/*
static int sizebins[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

// distribution of object sizes
(gdb) p sizebins[0]   $1 = 0
(gdb) p sizebins[1]   $2 = 0
(gdb) p sizebins[2]   $3 = 2
(gdb) p sizebins[3]   $4 = 762210
(gdb) p sizebins[4]   $5 = 2575571
(gdb) p sizebins[5]   $6 = 365939
(gdb) p sizebins[6]   $7 = 3015
(gdb) p sizebins[7]   $8 = 28131
(gdb) p sizebins[8]   $9 = 85
(gdb) p sizebins[9]   $10 = 19
(gdb) p sizebins[10]  $11 = 2
(gdb) p sizebins[11]  $12 = 0
(gdb) p sizebins[12]  $13 = 1
(gdb) p sizebins[13]  $14 = 0
(gdb) p sizebins[14]  $15 = 0
(gdb) p sizebins[15]  $16 = 1
(gdb) p sizebins[16]  $17 = 0
void *allocb(size_t nb)
{
    int i = 30;
    while (1<<i > nb && i>0) {
        i--;
    }
    sizebins[i]++;
    return GC_MALLOC(nb);
}
*/

DLLEXPORT
jl_value_t *jl_new_struct(jl_struct_type_t *type, ...)
{
    if (type->instance != NULL) return type->instance;
    va_list args;
    size_t nf = type->names->length;
    size_t i;
    va_start(args, type);
    jl_value_t *jv = newobj((jl_type_t*)type, nf);
    for(i=0; i < nf; i++) {
        ((jl_value_t**)jv)[i+1] = va_arg(args, jl_value_t*);
    }
    if (nf == 0) type->instance = jv;
    va_end(args);
    return jv;
}

DLLEXPORT
jl_value_t *jl_new_struct_uninit(jl_struct_type_t *type)
{
    if (type->instance != NULL) return type->instance;
    size_t nf = type->names->length;
    size_t i;
    jl_value_t *jv = newobj((jl_type_t*)type, nf);
    for(i=0; i < nf; i++) {
        ((jl_value_t**)jv)[i+1] = NULL;
    }
    if (nf == 0) type->instance = jv;
    return jv;
}

DLLEXPORT
jl_value_t *jl_new_structt(jl_struct_type_t *type, jl_tuple_t *t)
{
    if (type->instance != NULL) return type->instance;
    size_t nf = type->names->length;
    assert(nf == t->length);
    size_t i;
    jl_value_t *jv = newobj((jl_type_t*)type, nf);
    for(i=0; i < nf; i++) {
        ((jl_value_t**)jv)[i+1] = jl_tupleref(t, i);
    }
    if (nf == 0) type->instance = jv;
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
        jl_tupleset(jv, i, va_arg(args, jl_value_t*));
    }
    va_end(args);
    return jv;
}

jl_tuple_t *jl_tuple1(void *a)
{
    jl_tuple_t *t = (jl_tuple_t*)alloc_3w();
    t->type = (jl_type_t*)jl_tuple_type;
    t->length = 1;
    jl_tupleset(t, 0, a);
    return t;
}

jl_tuple_t *jl_tuple2(void *a, void *b)
{
    jl_tuple_t *t = (jl_tuple_t*)alloc_4w();
    t->type = (jl_type_t*)jl_tuple_type;
    t->length = 2;
    jl_tupleset(t, 0, a);
    jl_tupleset(t, 1, b);
    return t;
}

jl_tuple_t *jl_tuple3(void *a, void *b, void *c)
{
    jl_tuple_t *t = (jl_tuple_t*)allocobj(5*sizeof(void*));
    t->type = (jl_type_t*)jl_tuple_type;
    t->length = 3;
    jl_tupleset(t, 0, a);
    jl_tupleset(t, 1, b);
    jl_tupleset(t, 2, c);
    return t;
}

jl_tuple_t *jl_alloc_tuple_uninit(size_t n)
{
    if (n == 0) return jl_null;
    jl_tuple_t *jv = (jl_tuple_t*)newobj((jl_type_t*)jl_tuple_type, n+1);
    jv->length = n;
    return jv;
}

jl_tuple_t *jl_alloc_tuple(size_t n)
{
    if (n == 0) return jl_null;
    jl_tuple_t *jv = jl_alloc_tuple_uninit(n);
    size_t i;
    for(i=0; i < n; i++) {
        jl_tupleset(jv, i, NULL);
    }
    return jv;
}

jl_tuple_t *jl_tuple_append(jl_tuple_t *a, jl_tuple_t *b)
{
    jl_tuple_t *c = jl_alloc_tuple_uninit(a->length + b->length);
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

jl_tuple_t *jl_tuple_fill(size_t n, jl_value_t *v)
{
    if (n==0) return jl_null;
    jl_tuple_t *tup = jl_alloc_tuple_uninit(n);
    size_t i;
    for(i=0; i < n; i++) {
        jl_tupleset(tup, i, v);
    }
    return tup;
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
    jl_tuple_t *nt = jl_alloc_tuple_uninit(n*2);
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
#ifdef JL_GC_MARKSWEEP
    jl_function_t *f = (jl_function_t*)alloc_4w();
    f->type = (jl_type_t*)jl_any_func;
#else
    jl_function_t *f = (jl_function_t*)newobj((jl_type_t*)jl_any_func, 3);
#endif
    f->fptr = proc;
    f->env = env;
    f->linfo = NULL;
    return f;
}

DLLEXPORT
jl_lambda_info_t *jl_new_lambda_info(jl_value_t *ast, jl_tuple_t *sparams)
{
    jl_lambda_info_t *li =
        (jl_lambda_info_t*)newobj((jl_type_t*)jl_lambda_info_type, 13);
    li->ast = ast;
    li->sparams = sparams;
    li->tfunc = (jl_value_t*)jl_null;
    li->fptr = NULL;
    li->roots = jl_null;
    li->functionObject = NULL;
    li->specTypes = NULL;
    li->inferred = 0;
    li->inInference = 0;
    li->inCompile = 0;
    li->unspecialized = NULL;
    li->specializations = jl_null;
    li->name = jl_symbol("anonymous");
    return li;
}

// symbols --------------------------------------------------------------------

static jl_sym_t *symtab = NULL;

static jl_sym_t *mk_symbol(const char *str)
{
    jl_sym_t *sym;
    size_t len = strlen(str);

    sym = (jl_sym_t*)malloc(sizeof(jl_sym_t)-sizeof(void*) + len + 1);
    sym->type = (jl_type_t*)jl_sym_type;
    sym->left = sym->right = NULL;
#ifdef __LP64__
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
        root->type = (jl_type_t*)(((uptrint_t)root->type)&~1UL);
        unmark_symbols_(root->left);
        root = root->right;
    }
}

void jl_unmark_symbols() { unmark_symbols_(symtab); }

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

DLLEXPORT jl_sym_t *jl_symbol_n(const char *str, int32_t len)
{
    char name[len+1];
    memcpy(name, str, len);
    name[len] = '\0';
    return jl_symbol(name);
}

DLLEXPORT jl_sym_t *jl_gensym()
{
    static uint32_t gs_ctr = 0;  // TODO: per-thread
    char name[32];
    char *n;
    n = uint2str(name, sizeof(name)-1, gs_ctr, 10);
    *(--n) = '#';
    gs_ctr++;
    return jl_symbol(n);
}

// allocating types -----------------------------------------------------------

jl_typename_t *jl_new_typename(jl_sym_t *name)
{
    jl_typename_t *tn=(jl_typename_t*)newobj((jl_type_t*)jl_typename_type, 3);
    tn->name = name;
    tn->primary = NULL;
    tn->cache = NULL;
    return tn;
}

static void unbind_tvars(jl_tuple_t *parameters)
{
    size_t i;
    for(i=0; i < parameters->length; i++) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_tupleref(parameters, i);
        if (jl_is_typevar(tv))
            tv->bound = 0;
    }
}

jl_tag_type_t *jl_new_tagtype(jl_value_t *name, jl_tag_type_t *super,
                              jl_tuple_t *parameters)
{
    jl_typename_t *tn=NULL;
    JL_GC_PUSH(&tn);

    if (jl_is_typename(name))
        tn = (jl_typename_t*)name;
    else
        tn = jl_new_typename((jl_sym_t*)name);
    jl_tag_type_t *t = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind,
                                              TAG_TYPE_NW);
    t->name = tn;
    t->super = super;
    unbind_tvars(parameters);
    t->parameters = parameters;
    t->fptr = NULL;
    t->env = NULL;
    t->linfo = NULL;
    if (t->name->primary == NULL)
        t->name->primary = (jl_value_t*)t;
    JL_GC_POP();
    return t;
}

jl_func_type_t *jl_new_functype(jl_type_t *a, jl_type_t *b)
{
    JL_GC_PUSH(&a);
    if (a != (jl_type_t*)jl_bottom_type &&
        !jl_subtype((jl_value_t*)a, (jl_value_t*)jl_tuple_type, 0) &&
        !jl_subtype((jl_value_t*)jl_tuple_type, (jl_value_t*)a, 0))
        a = (jl_type_t*)jl_tuple1(a);
    jl_func_type_t *t = (jl_func_type_t*)newobj((jl_type_t*)jl_func_kind, 2);
    t->from = a;
    t->to = b;
    JL_GC_POP();
    return t;
}

// instantiate a type with new variables
jl_value_t *jl_new_type_instantiation(jl_value_t *t)
{
    jl_tuple_t *tp;
    if (jl_is_typector(t)) {
        tp = ((jl_typector_t*)t)->parameters;
    }
    else if (jl_is_some_tag_type(t)) {
        tp = ((jl_tag_type_t*)t)->parameters;
    }
    else {
        tp = NULL;
        jl_error("not supported");
    }
    jl_tuple_t *ntvs = jl_alloc_tuple(tp->length);
    JL_GC_PUSH(&ntvs);
    size_t i;
    for(i=0; i < tp->length; i++) {
        jl_value_t *tv = jl_tupleref(tp, i);
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
    jl_value_t *nt = (jl_value_t*)jl_apply_type((jl_value_t*)t, ntvs);
    JL_GC_POP();
    return nt;
}

JL_CALLABLE(jl_new_array_internal);

jl_function_t *jl_instantiate_method(jl_function_t *f, jl_tuple_t *sp);
void jl_specialize_ast(jl_lambda_info_t *li);

void jl_add_constructors(jl_struct_type_t *t)
{
    if (t->name == jl_array_typename) {
        if (!jl_has_typevars(jl_tparam0(t)) &&
            jl_is_int32(jl_tupleref(t->parameters,1)))
            t->fptr = jl_new_array_internal;
        else
            t->fptr = jl_f_no_function;
        return;
    }

    jl_initialize_generic_function((jl_function_t*)t, t->name->name);

    if (t->ctor_factory == (jl_value_t*)jl_nothing ||
        t->ctor_factory == (jl_value_t*)jl_null) {
        assert(t->parameters->length == 0);
    }
    else {
        assert(t->parameters->length > 0);
        if (t != (jl_struct_type_t*)t->name->primary) {
            // instantiating
            assert(jl_is_function(t->ctor_factory));
            
            // add type's static parameters to the ctor factory
            size_t np = t->parameters->length;
            jl_tuple_t *sparams = jl_alloc_tuple_uninit(np*2);
            jl_function_t *cfactory = NULL;
            JL_GC_PUSH(&sparams, &cfactory);
            size_t i;
            for(i=0; i < np; i++) {
                jl_tupleset(sparams, i*2+0,
                            jl_tupleref(((jl_struct_type_t*)t->name->primary)->parameters, i));
                jl_tupleset(sparams, i*2+1,
                            jl_tupleref(t->parameters, i));
            }
            cfactory = jl_instantiate_method((jl_function_t*)t->ctor_factory,
                                             sparams);
            jl_specialize_ast(cfactory->linfo);
            
            // call user-defined constructor factory on (type,)
            jl_value_t *cfargs[1] = { (jl_value_t*)t };
            jl_apply(cfactory, cfargs, 1);
            JL_GC_POP();
        }
    }
}

jl_struct_type_t *jl_new_struct_type(jl_sym_t *name, jl_tag_type_t *super,
                                     jl_tuple_t *parameters,
                                     jl_tuple_t *fnames, jl_tuple_t *ftypes)
{
    jl_typename_t *tn = jl_new_typename(name);
    JL_GC_PUSH(&tn);
    jl_struct_type_t *t = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind,
                                                    STRUCT_TYPE_NW);
    t->name = tn;
    t->name->primary = (jl_value_t*)t;
    t->super = super;
    unbind_tvars(parameters);
    t->parameters = parameters;
    t->names = fnames;
    t->types = ftypes;
    t->fptr = jl_f_no_function;
    t->env = (jl_value_t*)t;
    t->linfo = NULL;
    t->ctor_factory = (jl_value_t*)jl_null;
    t->instance = NULL;
    if (jl_has_typevars((jl_value_t*)parameters))
        t->uid = 0;
    else
        t->uid = jl_assign_type_uid();
    JL_GC_POP();
    return t;
}

extern int jl_boot_file_loaded;

jl_bits_type_t *jl_new_bitstype(jl_value_t *name, jl_tag_type_t *super,
                                jl_tuple_t *parameters, size_t nbits)
{
    jl_bits_type_t *t=NULL;
    jl_typename_t *tn=NULL;
    JL_GC_PUSH(&t, &tn);

    if (!jl_boot_file_loaded && jl_is_symbol(name)) {
        // hack to avoid making two versions of basic types needed
        // during bootstrapping
        if (!strcmp(((jl_sym_t*)name)->name, "Int32"))
            t = jl_int32_type;
        else if (!strcmp(((jl_sym_t*)name)->name, "Bool"))
            t = jl_bool_type;
    }
    int makenew = (t==NULL);
    if (makenew) {
        t = (jl_bits_type_t*)newobj((jl_type_t*)jl_bits_kind, BITS_TYPE_NW);
        if (jl_is_typename(name))
            tn = (jl_typename_t*)name;
        else
            tn = jl_new_typename((jl_sym_t*)name);
        t->name = tn;
    }
    t->super = super;
    unbind_tvars(parameters);
    t->parameters = parameters;
    if (jl_int32_type != NULL)
        t->bnbits = jl_box_int32(nbits);
    else
        t->bnbits = (jl_value_t*)jl_null;
    t->nbits = nbits;
    if (jl_has_typevars((jl_value_t*)parameters))
        t->uid = 0;
    else if (makenew)
        t->uid = jl_assign_type_uid();
    t->fptr = NULL;
    t->env = NULL;
    t->linfo = NULL;
    if (t->name->primary == NULL)
        t->name->primary = (jl_value_t*)t;
    JL_GC_POP();
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
                    jl_value_t *env = jl_type_match(a, b);
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
        jl_raise(jl_new_struct(jl_uniontoocomplex_type, (jl_value_t*)types));
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

JL_CALLABLE(jl_weakref_ctor)
{
    if (nargs > 1) {
        JL_NARGS(WeakRef, 1, 1);
    }
    if (nargs == 1)
        return (jl_value_t*)jl_gc_new_weakref(args[0]);
    return (jl_value_t*)jl_gc_new_weakref((jl_value_t*)jl_nothing);
}

// bits constructors ----------------------------------------------------------

#ifdef JL_GC_MARKSWEEP
#define BOX_FUNC(typ,c_type,pfx,nw)             \
jl_value_t *pfx##_##typ(c_type x)               \
{                                               \
    jl_value_t *v = alloc_##nw##w();            \
    v->type = (jl_type_t*)jl_##typ##_type;      \
    *(c_type*)jl_bits_data(v) = x;              \
    return v;                                   \
}
#else
#define BOX_FUNC(type,c_type,pfx,nw)                                    \
jl_value_t *pfx##_##type(c_type x)                                      \
{                                                                       \
    jl_value_t *v = newobj((jl_type_t*)jl_##type##_type,                \
                           NWORDS(LLT_ALIGN(sizeof(c_type),sizeof(void*)))); \
    *(c_type*)jl_bits_data(v) = x;                                      \
    return v;                                                           \
}
#endif
BOX_FUNC(int8,    int8_t,   jl_new_box, 2)
BOX_FUNC(uint8,   uint8_t,  jl_new_box, 2)
BOX_FUNC(int16,   int16_t,  jl_new_box, 2)
BOX_FUNC(uint16,  uint16_t, jl_new_box, 2)
BOX_FUNC(int32,   int32_t,  jl_new_box, 2)
BOX_FUNC(uint32,  uint32_t, jl_new_box, 2)
#ifdef __LP64__
BOX_FUNC(int64,   int64_t,  jl_new_box, 2)
BOX_FUNC(uint64,  uint64_t, jl_new_box, 2)
BOX_FUNC(float32, float,    jl_box, 2)
BOX_FUNC(float64, double,   jl_box, 2)
#else
BOX_FUNC(int64,   int64_t,  jl_new_box, 3)
BOX_FUNC(uint64,  uint64_t, jl_new_box, 3)
BOX_FUNC(float32, float,    jl_box, 2)
BOX_FUNC(float64, double,   jl_box, 3)
#endif

#define NBOX_C 2048

#ifdef JL_GC_MARKSWEEP
#define SIBOX_FUNC(typ,c_type,nw)                       \
static jl_value_t *boxed_##typ##_cache[NBOX_C];         \
jl_value_t *jl_box_##typ(c_type x)                      \
{                                                       \
    if ((u##c_type)(x+NBOX_C/2) < NBOX_C)               \
        return boxed_##typ##_cache[(x+NBOX_C/2)];       \
    jl_value_t *v = alloc_##nw##w();                    \
    v->type = (jl_type_t*)jl_##typ##_type;              \
    *(c_type*)jl_bits_data(v) = x;                      \
    return v;                                           \
}
#define UIBOX_FUNC(typ,c_type,nw)               \
static jl_value_t *boxed_##typ##_cache[NBOX_C]; \
jl_value_t *jl_box_##typ(c_type x)              \
{                                               \
    if (x < NBOX_C)                             \
        return boxed_##typ##_cache[x];          \
    jl_value_t *v = alloc_##nw##w();            \
    v->type = (jl_type_t*)jl_##typ##_type;      \
    *(c_type*)jl_bits_data(v) = x;              \
    return v;                                   \
}
#else
#define SIBOX_FUNC(typ,c_type,nw)                                       \
static jl_value_t *boxed_##typ##_cache[NBOX_C];                         \
jl_value_t *jl_box_##typ(c_type x)                                      \
{                                                                       \
    if ((u##c_type)(x+NBOX_C/2) < NBOX_C)                               \
        return boxed_##typ##_cache[(x+NBOX_C/2)];                       \
    jl_value_t *v = newobj((jl_type_t*)jl_##typ##_type,                 \
                           NWORDS(LLT_ALIGN(sizeof(c_type),sizeof(void*)))); \
    *(c_type*)jl_bits_data(v) = x;                                      \
    return v;                                                           \
}
#define UIBOX_FUNC(typ,c_type,nw)                                       \
static jl_value_t *boxed_##typ##_cache[NBOX_C];                         \
jl_value_t *jl_box_##typ(c_type x)                                      \
{                                                                       \
    if (x < NBOX_C)                                                     \
        return boxed_##typ##_cache[x];                                  \
    jl_value_t *v = newobj((jl_type_t*)jl_##typ##_type,                 \
                           NWORDS(LLT_ALIGN(sizeof(c_type),sizeof(void*)))); \
    *(c_type*)jl_bits_data(v) = x;                                      \
    return v;                                                           \
}
#endif
SIBOX_FUNC(int16,  int16_t, 2)
SIBOX_FUNC(int32,  int32_t, 2)
UIBOX_FUNC(uint16, uint16_t, 2)
UIBOX_FUNC(uint32, uint32_t, 2)
#ifdef __LP64__
SIBOX_FUNC(int64,  int64_t, 2)
UIBOX_FUNC(uint64, uint64_t, 2)
#else
SIBOX_FUNC(int64,  int64_t, 3)
UIBOX_FUNC(uint64, uint64_t, 3)
#endif

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

void jl_init_int32_cache()
{
    int64_t i;
    for(i=0; i < NBOX_C; i++) {
        boxed_int32_cache[i]  = jl_new_box_int32(i-NBOX_C/2);
    }
}

void jl_init_box_caches()
{
    int64_t i;
    for(i=0; i < 256; i++) {
        boxed_int8_cache[i]  = jl_new_box_int8((int8_t)(i-128));
        boxed_uint8_cache[i] = jl_new_box_uint8(i);
    }
    for(i=0; i < NBOX_C; i++) {
        boxed_int16_cache[i]  = jl_new_box_int16(i-NBOX_C/2);
        boxed_int64_cache[i]  = jl_new_box_int64(i-NBOX_C/2);
        boxed_uint16_cache[i] = jl_new_box_uint16(i);
        boxed_uint32_cache[i] = jl_new_box_uint32(i);
        boxed_uint64_cache[i] = jl_new_box_uint64(i);
    }
}

#ifdef JL_GC_MARKSWEEP
void jl_mark_box_caches()
{
    int64_t i;
    for(i=0; i < 256; i++) {
        jl_gc_markval(boxed_int8_cache[i]);
        jl_gc_markval(boxed_uint8_cache[i]);
    }
    for(i=0; i < NBOX_C; i++) {
        jl_gc_markval(boxed_int16_cache[i]);
        jl_gc_markval(boxed_int32_cache[i]);
        jl_gc_markval(boxed_int64_cache[i]);
        jl_gc_markval(boxed_uint16_cache[i]);
        jl_gc_markval(boxed_uint32_cache[i]);
        jl_gc_markval(boxed_uint64_cache[i]);
    }
}
#endif

jl_value_t *jl_box_bool(int8_t x)
{
    if (x)
        return jl_true;
    return jl_false;
}

#ifdef JL_GC_MARKSWEEP
#define BOXN_FUNC(nb,nw)                                        \
jl_value_t *jl_box##nb(jl_bits_type_t *t, int##nb##_t x)        \
{                                                               \
    assert(jl_is_bits_type(t));                                 \
    assert(jl_bitstype_nbits(t)/8 == sizeof(x));                \
    jl_value_t *v = alloc_##nw##w();                            \
    v->type = (jl_type_t*)t;                                    \
    *(int##nb##_t*)jl_bits_data(v) = x;                         \
    return v;                                                   \
}
#else
#define BOXN_FUNC(nb,nw)                                                \
jl_value_t *jl_box##nb(jl_bits_type_t *t, int##nb##_t x)                \
{                                                                       \
    assert(jl_is_bits_type(t));                                         \
    assert(jl_bitstype_nbits(t)/8 == sizeof(x));                        \
    jl_value_t *v = newobj((jl_type_t*)t,                               \
                           NWORDS(LLT_ALIGN(sizeof(x),sizeof(void*)))); \
    *(int##nb##_t*)jl_bits_data(v) = x;                                 \
    return v;                                                           \
}
#endif
BOXN_FUNC(8,  2)
BOXN_FUNC(16, 2)
BOXN_FUNC(32, 2)
#ifdef __LP64__
BOXN_FUNC(64, 2)
#else
BOXN_FUNC(64, 3)
#endif

#define UNBOX_FUNC(j_type,c_type)                                       \
c_type jl_unbox_##j_type(jl_value_t *v)                                 \
{                                                                       \
    assert(jl_is_bits_type(v->type));                                   \
    assert(jl_bitstype_nbits(v->type)/8 == sizeof(c_type));             \
    return *(c_type*)jl_bits_data(v);                                   \
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
#ifdef JL_GC_MARKSWEEP
    jl_value_t *v = alloc_2w();
    v->type = (jl_type_t*)ty;
#else
    jl_value_t *v = newobj((jl_type_t*)ty, 1);
#endif
    *(void**)jl_bits_data(v) = p;
    return v;
}

void *jl_unbox_pointer(jl_value_t *v)
{
    assert(jl_is_cpointer(v));
    return *(void**)jl_bits_data(v);
}

// Expr constructor for internal use ------------------------------------------

jl_expr_t *jl_exprn(jl_sym_t *head, size_t n)
{
    jl_array_t *ar = jl_alloc_cell_1d(n);
    JL_GC_PUSH(&ar);
#ifdef JL_GC_MARKSWEEP
    jl_expr_t *ex = (jl_expr_t*)alloc_4w();
#else
    jl_expr_t *ex = (jl_expr_t*)allocobj(sizeof(jl_expr_t));
#endif
    ex->type = (jl_type_t*)jl_expr_type;
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
    JL_NARGS(Expr, 3, 3);
    JL_TYPECHK(Expr, symbol, args[0]);
    if (!jl_typeis(args[1], (jl_value_t*)jl_array_any_type)) {
        jl_type_error("Expr", (jl_value_t*)jl_array_any_type, args[1]);
    }
#ifdef JL_GC_MARKSWEEP
    jl_expr_t *ex = (jl_expr_t*)alloc_4w();
#else
    jl_expr_t *ex = (jl_expr_t*)allocobj(sizeof(jl_expr_t));
#endif
    ex->type = (jl_type_t*)jl_expr_type;
    ex->head = (jl_sym_t*)args[0];
    ex->args = (jl_array_t*)args[1];
    ex->etype = args[2];
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
