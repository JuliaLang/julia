/*
  Types
  . type constructors
  . type predicates (subtype)
  . constructors
  . builtin type definitions
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
#include <gc.h>
#include "llt.h"
#include "julia.h"

jl_module_t *jl_system;
jl_module_t *jl_user;

jl_tag_type_t *jl_any_type;
jl_tag_type_t *jl_type_type;
jl_struct_type_t *jl_typename_type;
jl_struct_type_t *jl_sym_type;
jl_tag_type_t *jl_tuple_type;
jl_struct_type_t *jl_tvar_type;
jl_struct_type_t *jl_typector_type;

jl_struct_type_t *jl_func_kind;
jl_struct_type_t *jl_union_kind;
jl_struct_type_t *jl_tag_kind;
jl_struct_type_t *jl_struct_kind;
jl_struct_type_t *jl_bits_kind;

jl_type_t *jl_bottom_type;
jl_typector_t *jl_buffer_type;
jl_typector_t *jl_seq_type;
jl_typector_t *jl_tensor_type;
jl_tag_type_t *jl_scalar_type;
jl_tag_type_t *jl_number_type;
jl_tag_type_t *jl_real_type;
jl_tag_type_t *jl_int_type;
jl_tag_type_t *jl_float_type;

jl_bits_type_t *jl_bool_type;
jl_bits_type_t *jl_int8_type;
jl_bits_type_t *jl_uint8_type;
jl_bits_type_t *jl_int16_type;
jl_bits_type_t *jl_uint16_type;
jl_bits_type_t *jl_int32_type;
jl_bits_type_t *jl_uint32_type;
jl_bits_type_t *jl_int64_type;
jl_bits_type_t *jl_uint64_type;
jl_bits_type_t *jl_float32_type;
jl_bits_type_t *jl_float64_type;

jl_type_t *jl_buffer_uint8_type;
jl_type_t *jl_buffer_any_type;
jl_struct_type_t *jl_expr_type;;

jl_tuple_t *jl_null;
jl_value_t *jl_true;
jl_value_t *jl_false;

jl_func_type_t *jl_any_func;

static inline jl_value_t *newobj(jl_type_t *type, size_t nfields)
{
    jl_value_t *jv = (jl_value_t*)allocb((1+nfields) * sizeof(void*));
    jv->type = type;
    return jv;
}

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

JL_CALLABLE(jl_f_tuple)
{
    size_t i;
    if (nargs == 0) return (jl_value_t*)jl_null;
    jl_tuple_t *t = (jl_tuple_t*)newobj((jl_type_t*)jl_tuple_type, nargs+1);
    t->length = nargs;
    for(i=0; i < nargs; i++) {
        ((jl_value_t**)t)[i+2] = args[i];
    }
    return (jl_value_t*)t;
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

static jl_sym_t *symtab = NULL;

static jl_sym_t *mk_symbol(char *str)
{
    jl_sym_t *sym;
    size_t len = strlen(str);

    sym = (jl_sym_t*)allocb(sizeof(jl_sym_t)-sizeof(void*) + len + 1);
    sym->type = (jl_type_t*)jl_sym_type;
    sym->left = sym->right = NULL;
    sym->hash = memhash32(str, len)^0xAAAAAAAA;
    strcpy(&sym->name[0], str);
    return sym;
}

static jl_sym_t **symtab_lookup(jl_sym_t **ptree, char *str)
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

jl_sym_t *jl_symbol(char *str)
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

// the Type type --------------------------------------------------------------

jl_typename_t *jl_new_typename(jl_sym_t *name)
{
    jl_typename_t *tn=(jl_typename_t*)newobj((jl_type_t*)jl_typename_type, 3);
    tn->name = name;
    tn->ctor = NULL;
    return tn;
}

#define TAG_TYPE_NW (NWORDS(sizeof(jl_tag_type_t))-1)
#define STRUCT_TYPE_NW (NWORDS(sizeof(jl_struct_type_t))-1)
#define BITS_TYPE_NW (NWORDS(sizeof(jl_bits_type_t))-1)
static int t_uid_ctr = 1;  // TODO: lock

jl_tag_type_t *jl_new_tagtype(jl_sym_t *name, jl_tag_type_t *super,
                             jl_tuple_t *parameters)
{
    jl_tag_type_t *t = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind,
                                            TAG_TYPE_NW);
    t->name = jl_new_typename(name);
    t->super = super;
    t->parameters = parameters;
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

jl_function_t *jl_new_closure(jl_fptr_t proc, jl_value_t *env)
{
    jl_function_t *f = (jl_function_t*)newobj((jl_type_t*)jl_any_func, 2);
    f->fptr = proc;
    f->env = env;
    return f;
}

JL_CALLABLE(jl_new_struct_internal)
{
    jl_struct_type_t *t = (jl_struct_type_t*)env;
    size_t nf = t->names->length;
    if (nargs < nf)
        jl_error("Too few arguments to constructor");
    else if (nargs > nf)
        jl_error("Too many arguments to constructor");
    jl_value_t *v = newobj((jl_type_t*)t, nf);
    size_t i;
    for(i=0; i < nargs; i++) {
        ((jl_value_t**)v)[i+1] = args[i];
    }
    return v;
}

jl_struct_type_t *jl_new_struct_type(jl_sym_t *name, jl_tag_type_t *super,
                                     jl_tuple_t *parameters,
                                     jl_tuple_t *fnames, jl_tuple_t *ftypes)
{
    jl_struct_type_t *t = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind,
                                                    STRUCT_TYPE_NW);
    t->name = jl_new_typename(name);
    t->super = super;
    t->parameters = parameters;
    t->names = fnames;
    t->types = ftypes;
    t->fnew = jl_new_closure(jl_new_struct_internal, (jl_value_t*)t);
    t->fconvert = NULL; //TODO
    if (jl_has_typevars((jl_value_t*)parameters))
        t->uid = 0;
    else
        t->uid = t_uid_ctr++;
    return t;
}

jl_bits_type_t *jl_new_bitstype(jl_sym_t *name, jl_tag_type_t *super,
                                jl_tuple_t *parameters, size_t nbits)
{
    jl_bits_type_t *t = (jl_bits_type_t*)newobj((jl_type_t*)jl_bits_kind,
                                                BITS_TYPE_NW);
    t->name = jl_new_typename(name);
    t->super = super;
    t->parameters = parameters;
    t->nbits = nbits;
    if (jl_has_typevars((jl_value_t*)parameters))
        t->uid = 0;
    else
        t->uid = t_uid_ctr++;
    return t;
}

jl_uniontype_t *jl_new_uniontype(jl_tuple_t *types)
{
    jl_uniontype_t *t = (jl_uniontype_t*)newobj((jl_type_t*)jl_union_kind, 1);
    // TODO: enforce non-overlapping restriction

    // don't make unions of 1 type; Union(T)==T
    assert(types->length != 1);
    t->types = types;
    return t;
}

// --- type constructors ---

jl_typector_t *jl_new_type_ctor(jl_tuple_t *params, jl_type_t *body)
{
    jl_typector_t *tc = (jl_typector_t*)newobj((jl_type_t*)jl_typector_type, 2);
    tc->parameters = params;
    tc->body = body;
    if (jl_is_tag_type(body) || jl_is_struct_type(body) ||
        jl_is_bits_type(body)) {
        jl_typename_t *tn = jl_tname((jl_value_t*)body);
        if (tn->ctor == NULL)
            tn->ctor = tc;
    }
    return tc;
}

static jl_value_t *tvar(char *name)
{
    return jl_new_struct(jl_tvar_type, jl_symbol(name),
                         jl_bottom_type, jl_any_type);
}

static jl_tuple_t *typevars(size_t n, ...)
{
    va_list args;
    va_start(args, n);
    jl_tuple_t *t = jl_alloc_tuple(n);
    size_t i;
    for(i=0; i < n; i++) {
        jl_tupleset(t, i, tvar(va_arg(args, char*)));
    }
    va_end(args);
    return t;
}

// --- type properties and predicates ---

int jl_is_type(jl_value_t *v)
{
    if (jl_is_tuple(v)) {
        jl_tuple_t *t = (jl_tuple_t*)v;
        int i;
        for(i=0; i < t->length; i++) {
            jl_value_t *vv = jl_tupleref(t, i);
            if (!jl_is_typevar(vv) && !jl_is_type(vv))
                return 0;
        }
        return 1;
    }
    return (jl_typeis(v, jl_union_kind) ||
            jl_typeis(v, jl_struct_kind) ||
            jl_typeis(v, jl_func_kind) ||
            jl_typeis(v, jl_tag_kind) ||
            jl_typeis(v, jl_bits_kind));
}

jl_typename_t *jl_tname(jl_value_t *v)
{
    if (jl_is_tuple(v))
        return jl_tuple_type->name;
    if (jl_is_tag_type(v) || jl_is_struct_type(v) || jl_is_bits_type(v))
        return ((jl_tag_type_t*)v)->name;
    if (jl_is_typector(v))
        return jl_tname((jl_value_t*)((jl_typector_t*)v)->body);
    return ((jl_tag_type_t*)(v->type))->name;
}

jl_tag_type_t *jl_tsuper(jl_value_t *v)
{
    if (jl_is_tuple(v))
        return jl_tuple_type;
    if (jl_is_tag_type(v) || jl_is_struct_type(v) || jl_is_bits_type(v))
        return ((jl_tag_type_t*)v)->super;
    return jl_any_type;
}

jl_tuple_t *jl_tparams(jl_value_t *v)
{
    if (jl_is_tuple(v))
        return (jl_tuple_t*)v;
    if (jl_is_tag_type(v) || jl_is_struct_type(v) || jl_is_bits_type(v))
        return ((jl_tag_type_t*)v)->parameters;
    if (jl_is_union_type(v))
        return ((jl_uniontype_t*)v)->types;
    return jl_null;
}

int jl_has_typevars(jl_value_t *v)
{
    size_t i;
    if (jl_typeis(v, jl_tvar_type))
        return 1;
    if (jl_is_tuple(v)) {
        jl_tuple_t *t = (jl_tuple_t*)v;
        for(i=0; i < t->length; i++) {
            if (jl_has_typevars(jl_tupleref(t, i)))
                return 1;
        }
        return 0;
    }
    if (jl_is_func_type(v))
        return jl_has_typevars((jl_value_t*)((jl_func_type_t*)v)->from) ||
            jl_has_typevars((jl_value_t*)((jl_func_type_t*)v)->to);

    jl_tuple_t *t = jl_tparams(v);
    for(i=0; i < t->length; i++) {
        jl_value_t *elt = jl_tupleref(t, i);
        if (elt != v) {
            if (jl_has_typevars(elt))
                return 1;
        }
    }
    return 0;
}

// constructors for some normal types -----------------------------------------

#define BOX_FUNC(type,c_type)                                           \
jl_value_t *jl_box_##type(c_type x)                                     \
{                                                                       \
    jl_value_t *v = newobj((jl_type_t*)jl_##type##_type,                \
                           NWORDS(LLT_ALIGN(sizeof(c_type),sizeof(void*)))); \
    *(c_type*)(&((void**)v)[1]) = x;                                    \
    return v;                                                           \
}
BOX_FUNC(int8,   int8_t)
BOX_FUNC(uint8,  uint8_t)
BOX_FUNC(int16,  int16_t)
BOX_FUNC(uint16, uint16_t)
BOX_FUNC(int32,  int32_t)
BOX_FUNC(uint32, uint32_t)
BOX_FUNC(int64,  int64_t)
BOX_FUNC(uint64, uint64_t)
BOX_FUNC(bool,   int32_t)
BOX_FUNC(float32, float)
BOX_FUNC(float64, double)

#define UNBOX_FUNC(j_type,c_type)                       \
c_type jl_unbox_##j_type(jl_value_t *v)                 \
{                                                       \
    assert(v->type == (jl_type_t*)jl_##j_type##_type);  \
    return *(c_type*)(&((void**)v)[1]);                 \
}
UNBOX_FUNC(int8,   int8_t)
UNBOX_FUNC(uint8,  uint8_t)
UNBOX_FUNC(int16,  int16_t)
UNBOX_FUNC(uint16, uint16_t)
UNBOX_FUNC(int32,  int32_t)
UNBOX_FUNC(uint32, uint32_t)
UNBOX_FUNC(int64,  int64_t)
UNBOX_FUNC(uint64, uint64_t)
UNBOX_FUNC(bool,   int32_t)
UNBOX_FUNC(float32, float)
UNBOX_FUNC(float64, double)

jl_buffer_t *jl_new_buffer(jl_type_t *buf_type, size_t nel)
{
    jl_type_t *el_type = (jl_type_t*)jl_tparam0(buf_type);
    void *data;
    if (nel > 0) {
        if (jl_is_bits_type(el_type)) {
            data = alloc_pod(((jl_bits_type_t*)el_type)->nbits/8 * nel);
        }
        else {
            data = allocb(sizeof(void*) * nel);
        }
    }
    else {
        data = NULL;
    }
    jl_buffer_t *b = (jl_buffer_t*)newobj((jl_type_t*)buf_type, 2);
    b->length = nel;
    b->data = data;
    return (jl_value_t*)b;
}

JL_CALLABLE(jl_new_buffer_internal)
{
    jl_struct_type_t *buf_type = (jl_struct_type_t*)env;
    if (nargs != 1)
        jl_error("Buffer.new: Wrong number of arguments");
    size_t nel=0;
    if (jl_is_int32(args[0]))
        nel = (size_t)jl_unbox_int32(args[0]);
    else
        jl_error("Bufer.new: Expected integer");
    return jl_new_buffer(buf_type, nel);
}

jl_buffer_t *jl_cstr_to_buffer(char *str)
{
    size_t n = strlen(str);
    jl_buffer_t *b = jl_new_buffer(jl_buffer_uint8_type, n+1);
    strcpy(b->data, str);
    // '\0' terminator is there, but hidden from julia
    b->length--;
    return b;
}

// --- type instantiation and cache ---

static int extensionally_same_type(jl_value_t *a, jl_value_t *b)
{
    return (jl_subtype(a, b, 0, 0) && jl_subtype(b, a, 0, 0));
}

static int type_eqv_(jl_value_t *a, jl_value_t *b, jl_value_pair_t *stack)
{
    jl_value_pair_t top;
    if (a == b) return 1;
    if (jl_is_typevar(a) || jl_is_typevar(b)) return 0;
    if (jl_is_int32(a) && jl_is_int32(b))
        return (jl_unbox_int32(a) == jl_unbox_int32(b));
    if ((jl_is_tuple(a) && jl_is_tuple(b)) ||
        (jl_is_union_type(a) && jl_is_union_type(b))) {
        return extensionally_same_type(a, b);
    }
    jl_value_pair_t *p = stack;
    while (p != NULL) {
        if (p->a == a && p->b == b)
            return 1;
        p = p->next;
    }
    top.a = a;
    top.b = b;
    top.next = stack;
    stack = &top;
    if (jl_is_func_type(a)) {
        if (jl_is_func_type(b)) {
            return (type_eqv_(((jl_func_type_t*)a)->from,
                              ((jl_func_type_t*)b)->from, stack) &&
                    type_eqv_(((jl_func_type_t*)a)->to,
                              ((jl_func_type_t*)b)->to, stack));
        }
        return 0;
    }
    if (jl_tname(a) != jl_tname(b)) return 0;
    jl_tuple_t *ap = jl_tparams(a);
    jl_tuple_t *bp = jl_tparams(b);
    if (ap->length != bp->length) return 0;
    size_t i;
    for(i=0; i < ap->length; i++) {
        if (!type_eqv_(jl_tupleref(ap,i), jl_tupleref(bp,i), stack))
            return 0;
    }
    return 1;
}

int jl_types_equal(jl_value_t *a, jl_value_t *b)
{
    return type_eqv_(a, b, NULL);
}

static jl_type_t *apply_type_ctor_(jl_typector_t *tc, jl_value_t **params,
                                   size_t n)
{
    size_t i;
    char *tname = jl_tname(tc)->name->name;
    for(i=0; i < n; i++) {
        jl_value_t *pi = params[i];
        if (!jl_is_type(pi) && !jl_is_int32(pi) && !jl_is_typevar(pi))
            jl_errorf("Invalid parameter for type %s", tname);
    }
    jl_tuple_t *tp = tc->parameters;
    if (n < tp->length)
        jl_errorf("Too few parameters for type %s", tname);
    else if (n > tp->length)
        jl_errorf("Too many parameters for type %s", tname);
    jl_value_t **env = alloca(2 * n * sizeof(jl_value_t*));
    for(i=0; i < n; i++) {
        env[i*2+0] = jl_tupleref(tp,i);
        env[i*2+1] = params[i];
    }
    return jl_instantiate_type_with(tc->body, env, n);
}

jl_type_t *jl_apply_type_ctor(jl_typector_t *tc, jl_tuple_t *params)
{
    return apply_type_ctor_(tc, &jl_tupleref(params,0), params->length);
}

typedef struct _typekey_stack_t {
    jl_value_t **key;
    size_t n;  // key length
    jl_type_t *type;
    struct _typekey_stack_t *next;
} typekey_stack_t;

static jl_type_t *lookup_type(typekey_stack_t *table, jl_value_t **key,
                              size_t n)
{
    assert(n > 0);
    while (table != NULL) {
        assert(table->n > 0);
        if (table->n == n && table->key[0] == key[0]) {
            size_t i;
            for(i=1; i < n; i++) {
                if (!jl_types_equal(table->key[i], key[i]))
                    break;
            }
            if (i==n) return table->type;
        }
        table = table->next;
    }
    return NULL;
}

// TODO: synchronize
// TODO: convert to hash table
static typekey_stack_t *Type_Cache = NULL;
static void cache_type_(jl_value_t **key, size_t n, jl_type_t *type)
{
    // only cache concrete types
    if (jl_has_typevars((jl_value_t*)type))
        return;
    // assign uid
    if (jl_is_struct_type(type))
        ((jl_struct_type_t*)type)->uid = t_uid_ctr++;
    else if (jl_is_bits_type(type)) 
        ((jl_bits_type_t*)type)->uid = t_uid_ctr++;
    typekey_stack_t *tk = (typekey_stack_t*)allocb(sizeof(typekey_stack_t));
    tk->key = (jl_value_t**)allocb(n * sizeof(void*));
    size_t i;
    for(i=0; i < n; i++) tk->key[i] = key[i];
    tk->n = n;
    tk->type = type;
    tk->next = Type_Cache;
}

static jl_type_t *inst_type_w_(jl_value_t *t, jl_value_t **env, size_t n,
                               typekey_stack_t *stack)
{
    typekey_stack_t top;
    size_t i;
    if (n == 0) return t;
    if (jl_is_typevar(t)) {
        for(i=0; i < n; i++) {
            if (env[i*2] == t)
                return env[i*2+1];
        }
        return t;
    }
    if (jl_is_tuple(t)) {
        jl_tuple_t *p = (jl_tuple_t*)t;
        jl_tuple_t *nt = jl_alloc_tuple(p->length);
        for(i=0; i < p->length; i++) {
            jl_tupleset(nt, i, inst_type_w_(jl_tupleref(p,i), env, n, stack));
        }
        return nt;
    }
    if (jl_is_union_type(t)) {
        return jl_new_uniontype((jl_tuple_t*)inst_type_w_((jl_value_t*)((jl_uniontype_t*)t)->types, env, n, stack));
    }
    if (jl_is_func_type(t)) {
        jl_func_type_t *ft = (jl_func_type_t*)t;
        return jl_new_functype(inst_type_w_(ft->from, env, n, stack),
                               inst_type_w_(ft->to  , env, n, stack));
    }
    if (jl_is_tag_type(t) || jl_is_struct_type(t) || jl_is_bits_type(t)) {
        jl_tag_type_t *tt = (jl_tag_type_t*)t;
        jl_tuple_t *tp = tt->parameters;
        if (jl_is_null(tp))
            return t;
        size_t ntp = tp->length;
        jl_value_t **iparams = (jl_value_t**)alloca((ntp+1) * sizeof(void*));
        for(i=0; i < ntp; i++) {
            jl_value_t *elt = jl_tupleref(tp, i);
            if (elt == t)
                iparams[i+1] = t;
            else
                iparams[i+1] = (jl_value_t*)inst_type_w_(elt, env, n, stack);
        }
        jl_typename_t *tn = jl_tname(t);
        iparams[0] = (jl_value_t*)tn;

        // if an identical instantiation is already in process somewhere
        // up the stack, return it. this computes a fixed point for
        // recursive types.
        jl_type_t *lkup = lookup_type(stack, iparams, ntp+1);
        if (lkup != NULL) return lkup;

        // check type cache
        lkup = lookup_type(Type_Cache, iparams, ntp+1);
        if (lkup != NULL) return lkup;

        // always use original type constructor
        jl_typector_t *tc = tn->ctor;
        if (tc != NULL && tc->body != (jl_type_t*)t)
            return apply_type_ctor_(tc, &iparams[1], ntp);

        // move array of instantiated parameters to heap; we need to keep it
        jl_tuple_t *iparams_tuple = jl_alloc_tuple(ntp);
        for(i=0; i < ntp; i++)
            jl_tupleset(iparams_tuple, i, iparams[i+1]);
        if (jl_is_tag_type(t)) {
            jl_tag_type_t *tagt = (jl_tag_type_t*)t;
            return jl_new_tagtype(tn, inst_type_w_(tagt->super, env,n,stack),
                                  iparams_tuple);
        }
        else if (jl_is_bits_type(t)) {
            jl_bits_type_t *bitst = (jl_bits_type_t*)t;
            return jl_new_bitstype(tn, inst_type_w_(bitst->super, env,n,stack),
                                   iparams_tuple, bitst->nbits);
        }
        else {
            assert(jl_is_struct_type(t));
            jl_struct_type_t *st = (jl_struct_type_t*)t;
            // create and initialize new struct type
            jl_struct_type_t *nst =
                (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind,
                                          STRUCT_TYPE_NW);
            nst->name = tn;
            nst->super = inst_type_w_(st->super, env,n,stack);
            nst->parameters = iparams_tuple;
            nst->names = st->names;
            nst->types = jl_null; // to be filled in below
            nst->fnew = jl_new_closure(st->fnew->fptr, (jl_value_t*)nst);
            nst->fconvert = NULL;
            nst->uid = 0;
            // associate these parameters with the new struct type on
            // the stack, in case one of its field types references it.
            top.key = iparams;
            top.n = ntp+1;
            top.type = (jl_type_t*)nst;
            top.next = stack;
            stack = &top;
            jl_tuple_t *ftypes = st->types;
            if (ftypes != NULL) {
                // recursively instantiate the types of the fields
                jl_tuple_t *nftypes = jl_alloc_tuple(ftypes->length);
                for(i=0; i < ftypes->length; i++) {
                    jl_tupleset(nftypes, i,
                                inst_type_w_(jl_tupleref(ftypes,i),
                                             env,n,stack));
                }
                nst->types = nftypes;
            }
            // TODO: instantiate st->fconvert
            cache_type_(iparams, ntp+1, (jl_type_t*)nst);
            return (jl_type_t*)nst;
        }
    }
    return t;
}

jl_type_t *jl_instantiate_type_with(jl_type_t *t, jl_value_t **env, size_t n)
{
    return inst_type_w_((jl_value_t*)t, env, n, NULL);
}

int jl_tuple_subtype(jl_value_t **child, size_t cl,
                     jl_value_t **parent, size_t pl, int ta, int tb)
{
    size_t ci=0, pi=0;
    while(1) {
        int cseq = (ci<cl) && jl_is_seq_type(child[ci]);
        int pseq = (pi<pl) && jl_is_seq_type(parent[pi]);
        if (ci >= cl)
            return (pi>=pl || pseq);
        if (cseq && !pseq)
            return 0;
        if (pi >= pl)
            return 0;
        jl_value_t *ce = child[ci];
        jl_value_t *pe = parent[pi];
        if (cseq) ce = jl_tparam0(ce);
        if (pseq) pe = jl_tparam0(pe);

        if (!jl_subtype(ta ? (jl_value_t*)jl_typeof(ce) : ce,
                        tb ? (jl_value_t*)jl_typeof(pe) : pe, 0, 0))
            return 0;

        if (cseq && pseq) return 1;
        if (!cseq) ci++;
        if (!pseq) pi++;
    }
    return 0;
}

#define jl_is_some_tag_type(v) (jl_is_tag_type(v)||jl_is_struct_type(v)||jl_is_bits_type(v))

/*
  ta and tb specify whether typeof() should be implicitly applied
  to the arguments a and b. this is used for tuple types to avoid
  allocating them explicitly.
*/
int jl_subtype(jl_value_t *a, jl_value_t *b, int ta, int tb)
{
    size_t i;
    if (jl_is_tuple(a)) {
        if (b == jl_tuple_type) return 1;
        if (jl_is_tuple(b)) {
            return jl_tuple_subtype(&jl_tupleref(a,0), ((jl_tuple_t*)a)->length,
                                    &jl_tupleref(b,0), ((jl_tuple_t*)b)->length,
                                    ta, tb);
        }
    }
    if (jl_is_union_type(a)) {
        assert(!ta);
        jl_tuple_t *ap = ((jl_uniontype_t*)a)->types;
        for(i=0; i < ap->length; i++) {
            if (!jl_subtype(jl_tupleref(ap,i), b, 0, tb))
                return 0;
        }
        return 1;
    }
    if (jl_is_union_type(b)) {
        assert(!tb);
        jl_tuple_t *bp = ((jl_uniontype_t*)b)->types;
        for(i=0; i < bp->length; i++) {
            if (jl_subtype(a, jl_tupleref(bp,i), ta, 0))
                return 1;
        }
        return 0;
    }

    if (jl_is_tuple(a)) return 0;
    assert(!ta && !tb);
    if (a == b) return 1;
    if (jl_is_typevar(b)) return 1;
    if (jl_is_typevar(a)) return 0;
    if (b == jl_any_type) return 1;
    if (a == jl_any_type) return 0;
    if (jl_is_int32(a) && jl_is_int32(b))
        return (jl_unbox_int32(a)==jl_unbox_int32(b));

    if (jl_is_func_type(a) && jl_is_func_type(b)) {
        jl_func_type_t *fa = (jl_func_type_t*)a;
        jl_func_type_t *fb = (jl_func_type_t*)b;
        return ( (jl_is_typevar(fb->from) ||
                  jl_subtype(fb->from, fa->from, 0, 0)) &&
                 jl_subtype(fa->to, fb->to, 0, 0) );
    }

    assert(jl_is_some_tag_type(a));
    assert(jl_is_some_tag_type(b));
    jl_tag_type_t *tta = (jl_tag_type_t*)a;
    jl_tag_type_t *ttb = (jl_tag_type_t*)b;

    if (tta->name == ttb->name) {
        assert(tta->parameters->length == ttb->parameters->length);
        for(i=0; i < tta->parameters->length; i++) {
            if (!jl_types_equal(jl_tupleref(tta->parameters,i),
                                jl_tupleref(ttb->parameters,i)))
                return 0;
        }
        return 1;
    }

    return jl_subtype(tta->super, ttb, 0, 0);
}

static jl_value_pair_t *type_conform_(jl_type_t *child, jl_type_t *parent,
                                      jl_value_pair_t *env);

static jl_value_pair_t *tuple_conform(jl_tuple_t *child, jl_tuple_t *parent,
                                      jl_value_pair_t *env)
{
    size_t ci=0, pi=0;
    size_t cl = child->length;
    size_t pl = parent->length;
    while(1) {
        int cseq = (ci<cl) && jl_is_seq_type(jl_tupleref(child,ci));
        int pseq = (pi<pl) && jl_is_seq_type(jl_tupleref(parent,pi));
        if (ci >= cl)
            return (pi>=pl || pseq) ? env : NULL;
        if (cseq && !pseq)
            return NULL;
        if (pi >= pl)
            return NULL;
        jl_value_t *ce = jl_tupleref(child,ci);
        jl_value_t *pe = jl_tupleref(parent,pi);
        if (cseq) ce = jl_tparam0(ce);
        if (pseq) pe = jl_tparam0(pe);

        env = type_conform_(ce, pe, env);
        if (env == NULL) return NULL;

        if (cseq && pseq) return env;
        if (!cseq) ci++;
        if (!pseq) pi++;
    }
    return env;
}

static jl_value_pair_t *type_conform_(jl_type_t *child, jl_type_t *parent,
                                      jl_value_pair_t *env)
{
    size_t i;
    if (jl_is_typevar(parent)) {
        jl_value_pair_t *p = env;
        while (p != NULL) {
            if (p->a == (jl_value_t*)parent) {
                if (jl_types_equal(child, (jl_type_t*)p->b))
                    return env;
                else
                    return NULL;
            }
            p = p->next;
        }
        jl_value_pair_t *np = (jl_value_pair_t*)allocb(sizeof(jl_value_pair_t));
        np->a = (jl_value_t*)parent;
        np->b = (jl_value_t*)child;
        np->next = env;
        return np;
    }
    if (jl_is_typevar(child)) return NULL;
    if (jl_is_int32(child) && jl_is_int32(parent)) {
        if (jl_unbox_int32(child) == jl_unbox_int32(parent))
            return env;
        return NULL;
    }
    if (child == parent) return env;
    if (parent == (jl_type_t*)jl_any_type) return env;
    if (child  == (jl_type_t*)jl_any_type) return env;

    if (jl_is_union_type(child)) {
        jl_tuple_t *t = ((jl_uniontype_t*)child)->types;
        for(i=0; i < t->length; i++) {
            env = type_conform_(jl_tupleref(t,i), parent, env);
            if (env == NULL) return NULL;
        }
        return env;
    }
    if (jl_is_union_type(parent)) {
        jl_tuple_t *t = ((jl_uniontype_t*)parent)->types;
        for(i=0; i < t->length; i++) {
            jl_value_pair_t *p =
                type_conform_(child, jl_tupleref(parent,i), env);
            if (p != NULL) return p;
        }
        return NULL;
    }

    if (jl_is_func_type(parent)) {
        if (jl_is_func_type(child)) {
            env = type_conform_(((jl_func_type_t*)child)->from,
                                ((jl_func_type_t*)parent)->from, env);
            if (env == NULL) return NULL;
            return type_conform_(((jl_func_type_t*)child)->to,
                                 ((jl_func_type_t*)parent)->to, env);
        }
        return NULL;
    }

    if (jl_is_tuple(child)) {
        if (jl_is_tuple(parent)) {
            return tuple_conform((jl_tuple_t*)child, (jl_tuple_t*)parent, env);
        }
        return NULL;
    }

    assert(jl_is_some_tag_type(child));
    assert(jl_is_some_tag_type(parent));
    jl_tag_type_t *tta = (jl_tag_type_t*)child;
    jl_tag_type_t *ttb = (jl_tag_type_t*)parent;

    if (tta->name == ttb->name) {
        assert(tta->parameters->length == ttb->parameters->length);
        for(i=0; i < tta->parameters->length; i++) {
            env = type_conform_(jl_tupleref(tta->parameters,i),
                                jl_tupleref(ttb->parameters,i), env);
            if (env == NULL) return NULL;
        }
        return env;
    }

    return type_conform_((jl_type_t*)tta->super, parent, env);
}

static jl_value_pair_t Empty_Env = {NULL,NULL,NULL};

jl_value_pair_t *jl_type_conform(jl_type_t *a, jl_type_t *b)
{
    return type_conform_(a, b, &Empty_Env);
}

static jl_bits_type_t *make_scalar_type(char *name, jl_tag_type_t *super,
                                        int nbits)
{
    return jl_new_bitstype(jl_symbol(name), super, jl_null, nbits);
}

void jl_init_types()
{
    // create base objects
    jl_struct_kind = (jl_struct_type_t*)newobj(NULL, STRUCT_TYPE_NW);
    jl_struct_kind->type = (jl_type_t*)jl_struct_kind;
    jl_tag_kind = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCT_TYPE_NW);
    jl_func_kind = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCT_TYPE_NW);

    jl_typename_type = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCT_TYPE_NW);
    jl_sym_type = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCT_TYPE_NW);

    jl_any_type = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAG_TYPE_NW);
    jl_type_type = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAG_TYPE_NW);
    jl_tuple_type = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAG_TYPE_NW);

    jl_null = (jl_tuple_t*)newobj((jl_type_t*)jl_tuple_type, 1);
    jl_null->length = 0;

    jl_any_func = jl_new_functype((jl_type_t*)jl_any_type, (jl_type_t*)jl_any_type);

    // initialize them. lots of cycles.
    jl_struct_kind->name = jl_new_typename(jl_symbol("StructKind"));
    jl_struct_kind->super = (jl_tag_type_t*)jl_tag_kind;
    jl_struct_kind->parameters = jl_null;
    jl_struct_kind->names = jl_tuple(7, jl_symbol("name"), jl_symbol("super"),
                                     jl_symbol("parameters"),
                                     jl_symbol("names"), jl_symbol("types"),
                                     jl_symbol("new"), jl_symbol("convert"));
    jl_struct_kind->types = jl_tuple(7, jl_typename_type, jl_type_type,
                                     jl_tuple_type, jl_tuple_type,
                                     jl_tuple_type, jl_any_func, jl_any_func);
    jl_struct_kind->fnew = jl_struct_kind->fconvert = NULL;
    jl_struct_kind->uid = t_uid_ctr++;

    jl_tag_kind->name = jl_new_typename(jl_symbol("TagKind"));
    jl_tag_kind->super = jl_type_type;
    jl_tag_kind->parameters = jl_null;
    jl_tag_kind->names = jl_tuple(3, jl_symbol("name"), jl_symbol("super"),
                                  jl_symbol("parameters"));
    jl_tag_kind->types = jl_tuple(3, jl_typename_type, jl_type_type,
                                  jl_tuple_type);
    jl_tag_kind->fnew = jl_tag_kind->fconvert = NULL;
    jl_tag_kind->uid = t_uid_ctr++;

    jl_func_kind->name = jl_new_typename(jl_symbol("FuncKind"));
    jl_func_kind->super = jl_type_type;
    jl_func_kind->parameters = jl_null;
    jl_func_kind->names = jl_tuple(2, jl_symbol("from"), jl_symbol("to"));
    jl_func_kind->types = jl_tuple(2, jl_type_type, jl_type_type);
    jl_func_kind->fnew = jl_func_kind->fconvert = NULL;
    jl_func_kind->uid = t_uid_ctr++;

    jl_typename_type->name = jl_new_typename(jl_symbol("TypeName"));
    jl_typename_type->super = jl_any_type;
    jl_typename_type->parameters = jl_null;
    jl_typename_type->names = jl_tuple(1, jl_symbol("name"));
    jl_typename_type->types = jl_tuple(1, jl_sym_type);
    jl_typename_type->fnew = jl_typename_type->fconvert = NULL;
    jl_typename_type->uid = t_uid_ctr++;

    jl_sym_type->name = jl_new_typename(jl_symbol("Symbol"));
    jl_sym_type->super = jl_any_type;
    jl_sym_type->parameters = jl_null;
    jl_sym_type->names = jl_null;
    jl_sym_type->types = jl_null;
    jl_sym_type->fnew = jl_sym_type->fconvert = NULL;
    jl_sym_type->uid = t_uid_ctr++;

    jl_any_type->name = jl_new_typename(jl_symbol("Any"));
    jl_any_type->super = jl_any_type;
    jl_any_type->parameters = jl_null;

    jl_type_type->name = jl_new_typename(jl_symbol("Type"));
    jl_type_type->super = jl_any_type;
    jl_type_type->parameters = jl_null;

    jl_tuple_type->name = jl_new_typename(jl_symbol("Tuple"));
    jl_tuple_type->super = jl_any_type;
    jl_tuple_type->parameters = jl_null;

    // now they can be used to create the remaining base kinds and types
    jl_union_kind = jl_new_struct_type(jl_symbol("Union"),
                                      jl_type_type, jl_null,
                                      jl_tuple(1, jl_symbol("types")),
                                      jl_tuple(1, jl_tuple_type));

    jl_bottom_type = (jl_type_t*)jl_new_struct(jl_union_kind, jl_null);

    jl_bits_kind =
        jl_new_struct_type(jl_symbol("BitsKind"), (jl_tag_type_t*)jl_tag_kind,
                          jl_null,
                          jl_tuple(4, jl_symbol("name"), jl_symbol("super"),
                                   jl_symbol("parameters"),
                                   jl_symbol("convert")),
                          jl_tuple(4, jl_typename_type, jl_type_type,
                                   jl_tuple_type, jl_any_func));
    // cannot be created with normal constructor due to hidden fields
    jl_bits_kind->fnew = NULL;
    
    jl_tvar_type = jl_new_struct_type(jl_symbol("TypeVar"),
                                     jl_any_type, jl_null,
                                     jl_tuple(3, jl_symbol("name"),
                                              jl_symbol("lb"),
                                              jl_symbol("ub")),
                                     jl_tuple(3, jl_sym_type, jl_type_type,
                                              jl_type_type));

    jl_typector_type = jl_new_struct_type(jl_symbol("TypeConstructor"),
                                         jl_any_type, jl_null,
                                         jl_tuple(2, jl_symbol("parameters"),
                                                  jl_symbol("body")),
                                         jl_tuple(2, jl_tuple_type,
                                                  jl_type_type));

    jl_tuple_t *tv;
    tv = typevars(1, "T");
    jl_seq_type =
        jl_new_type_ctor(tv, (jl_type_t*)jl_new_tagtype(jl_symbol("..."), jl_any_type, tv));

    tv = typevars(2, "T", "n");
    jl_tensor_type =
        jl_new_type_ctor(tv,
                         (jl_type_t*)jl_new_tagtype(jl_symbol("Tensor"), jl_any_type, tv));

    jl_scalar_type =
        (jl_tag_type_t*)jl_apply_type_ctor(jl_tensor_type,
                                           jl_tuple(2, jl_bottom_type,
                                                    jl_bottom_type));

    jl_number_type = jl_new_tagtype(jl_symbol("Number"), jl_scalar_type,
                                    jl_null);
    jl_real_type = jl_new_tagtype(jl_symbol("Real"), jl_number_type, jl_null);
    jl_int_type = jl_new_tagtype(jl_symbol("Int"), jl_real_type, jl_null);
    jl_float_type = jl_new_tagtype(jl_symbol("Float"), jl_real_type, jl_null);

    jl_bool_type    = make_scalar_type("Bool"  , jl_scalar_type, 32);
    jl_int8_type    = make_scalar_type("Int8"  , jl_int_type, 8);
    jl_uint8_type   = make_scalar_type("Uint8" , jl_int_type, 8);
    jl_int16_type   = make_scalar_type("Int16" , jl_int_type, 16);
    jl_uint16_type  = make_scalar_type("Uint16", jl_int_type, 16);
    jl_int32_type   = make_scalar_type("Int32" , jl_int_type, 32);
    jl_uint32_type  = make_scalar_type("Uint32", jl_int_type, 32);
    jl_int64_type   = make_scalar_type("Int64" , jl_int_type, 64);
    jl_uint64_type  = make_scalar_type("Uint64", jl_int_type, 64);

    jl_float32_type = make_scalar_type("Float32", jl_float_type, 32);
    jl_float64_type = make_scalar_type("Float64", jl_float_type, 64);

    jl_tupleset(jl_scalar_type->parameters, 0, (jl_value_t*)jl_scalar_type);
    jl_tupleset(jl_scalar_type->parameters, 1, jl_box_int32(0));

    jl_false = jl_box_bool(0);
    jl_true = jl_box_bool(1);

    tv = typevars(1, "T");
    jl_struct_type_t *bufstruct = 
        jl_new_struct_type(jl_symbol("Buffer"),
                           jl_any_type, tv, jl_null, jl_null);
    bufstruct->fnew->fptr = jl_new_buffer_internal;
    jl_buffer_type = jl_new_type_ctor(tv, (jl_type_t*)bufstruct);

    jl_buffer_uint8_type =
        (jl_type_t*)jl_apply_type_ctor(jl_buffer_type,
                                       jl_tuple(1, jl_uint8_type));

    jl_buffer_any_type =
        (jl_type_t*)jl_apply_type_ctor(jl_buffer_type,
                                       jl_tuple(1, jl_any_type));

    jl_expr_type =
        jl_new_struct_type(jl_symbol("Expr"),
                           jl_any_type, jl_null,
                           jl_tuple(2, jl_symbol("head"), jl_symbol("args")),
                           jl_tuple(2, jl_sym_type, jl_buffer_any_type));
}
