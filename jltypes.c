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
#ifndef NO_BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

jl_tag_type_t *jl_any_type;
jl_tag_type_t *jl_type_type;
jl_struct_type_t *jl_typename_type;
jl_struct_type_t *jl_sym_type;
jl_tuple_t *jl_tuple_type;
jl_typename_t *jl_tuple_typename;
jl_typector_t *jl_ntuple_type;
jl_typename_t *jl_ntuple_typename;
jl_struct_type_t *jl_tvar_type;
jl_struct_type_t *jl_typector_type;

jl_struct_type_t *jl_func_kind;
jl_struct_type_t *jl_union_kind;
jl_struct_type_t *jl_tag_kind;
jl_struct_type_t *jl_struct_kind;
jl_struct_type_t *jl_bits_kind;

jl_type_t *jl_bottom_type;
jl_typector_t *jl_buffer_type;
jl_typename_t *jl_buffer_typename;
jl_struct_type_t *jl_lambda_info_type;
jl_typector_t *jl_seq_type;
jl_typector_t *jl_functype_ctor;
jl_typector_t *jl_tensor_type;
jl_typector_t *jl_scalar_type;
jl_typector_t *jl_number_type;
jl_typector_t *jl_real_type;
jl_typector_t *jl_int_type;
jl_typector_t *jl_float_type;

jl_typector_t *jl_box_type;
jl_type_t *jl_box_any_type;
jl_typename_t *jl_box_typename;

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
jl_bits_type_t *jl_intrinsic_type;

jl_tuple_t *jl_null;
jl_value_t *jl_true;
jl_value_t *jl_false;

jl_func_type_t *jl_any_func;
jl_function_t *jl_bottom_func;
jl_function_t *jl_identity_func;
jl_buffer_t *jl_the_empty_buffer;

jl_sym_t *call_sym;
jl_sym_t *dots_sym;
jl_sym_t *dollar_sym;
jl_sym_t *quote_sym;
jl_sym_t *tuple_sym;
jl_sym_t *top_sym;
jl_sym_t *expr_sym;
jl_sym_t *list_sym;
jl_sym_t *line_sym;
jl_sym_t *continue_sym;

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

static jl_sym_t *symtab = NULL;

static jl_sym_t *mk_symbol(const char *str)
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
    jl_function_t *f = (jl_function_t*)newobj((jl_type_t*)jl_any_func, 3);
    f->fptr = proc;
    f->env = env;
    f->linfo = NULL;
    return f;
}

jl_lambda_info_t *jl_new_lambda_info(jl_value_t *ast, jl_tuple_t *sparams)
{
    jl_lambda_info_t *li =
        (jl_lambda_info_t*)newobj((jl_type_t*)jl_lambda_info_type, 3);
    li->fptr = NULL;
    li->ast = ast;
    li->sparams = sparams;
    return li;
}

JL_CALLABLE(jl_new_struct_internal)
{
    jl_struct_type_t *t = (jl_struct_type_t*)env;
    size_t nf = t->names->length;
    if (nargs < nf)
        jl_error("too few arguments to constructor");
    else if (nargs > nf)
        jl_error("too many arguments to constructor");
    jl_value_t *v = newobj((jl_type_t*)t, nf);
    size_t i;
    for(i=0; i < nargs; i++) {
        ((jl_value_t**)v)[i+1] = args[i];
    }
    return v;
}

JL_CALLABLE(jl_f_no_function)
{
    jl_error("function not defined");
    return (jl_value_t*)jl_null;
}

JL_CALLABLE(jl_f_identity);

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
    t->fconvert = jl_new_generic_function(jl_symbol("convert"));
    // add identity conversion, convert(x::this) = x
    jl_add_method(t->fconvert, jl_tuple(1, t), jl_identity_func);
    if (jl_has_typevars((jl_value_t*)parameters))
        t->uid = 0;
    else
        t->uid = t_uid_ctr++;
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
    t->parameters = parameters;
    t->fconvert = jl_new_generic_function(jl_symbol("convert"));
    // add identity conversion, convert(x::this) = x
    jl_add_method(t->fconvert, jl_tuple(1, t), jl_identity_func);
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
    if (jl_is_some_tag_type(body)) {
        jl_typename_t *tn = jl_tname((jl_value_t*)body);
        if (tn->ctor == NULL)
            tn->ctor = tc;
    }
    return tc;
}

static jl_value_t *tvar(const char *name)
{
    return jl_new_struct(jl_tvar_type, jl_symbol(name),
                         jl_bottom_type, jl_any_type);
}

jl_tvar_t *jl_typevar(jl_sym_t *name)
{
    return (jl_tvar_t*)jl_new_struct(jl_tvar_type, name,
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
            if (!jl_is_typevar(vv) && !jl_is_type(vv) &&
                !jl_is_typector(vv))
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
        return jl_tuple_typename;
    if (jl_is_some_tag_type(v))
        return ((jl_tag_type_t*)v)->name;
    if (jl_is_typector(v))
        return jl_tname((jl_value_t*)((jl_typector_t*)v)->body);
    return ((jl_tag_type_t*)(v->type))->name;
}

jl_tuple_t *jl_tparams(jl_value_t *v)
{
    if (jl_is_tuple(v))
        return (jl_tuple_t*)v;
    if (jl_is_some_tag_type(v))
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

// construct the full type of a value, possibly making a tuple type
jl_value_t *jl_full_type(jl_value_t *v)
{
    if (!jl_is_tuple(v))
        return (jl_value_t*)jl_typeof(v);
    jl_tuple_t *in = (jl_tuple_t*)v;
    jl_tuple_t *out = jl_alloc_tuple(in->length);
    size_t i;
    for(i=0; i < in->length; i++) {
        jl_tupleset(out, i, jl_full_type(jl_tupleref(in, i)));
    }
    return (jl_value_t*)out;
}

// constructors for some normal types -----------------------------------------

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

jl_buffer_t *jl_new_buffer(jl_type_t *buf_type, size_t nel)
{
    jl_type_t *el_type = (jl_type_t*)jl_tparam0(buf_type);
    jl_buffer_t *b = (jl_buffer_t*)allocb(sizeof(jl_buffer_t));
    b->type = buf_type;
    void *data;
    if (nel > 0) {
        if (jl_is_bits_type(el_type)) {
            size_t tot = ((jl_bits_type_t*)el_type)->nbits/8 * nel;
            if (tot <= 13*sizeof(void*))
                data = &b->_space[0];
            else
                data = alloc_pod(tot);
        }
        else {
            size_t tot = sizeof(void*) * nel;
            if (nel <= 13)
                data = &b->_space[0];
            else
                data = allocb(tot);
            memset(data, 0, tot);
        }
    }
    else {
        data = NULL;
    }
    b->length = nel;
    b->data = data;
    return (jl_buffer_t*)b;
}

JL_CALLABLE(jl_new_buffer_internal)
{
    jl_struct_type_t *buf_type = (jl_struct_type_t*)env;
    if (nargs != 1)
        jl_error("Buffer.new: wrong number of arguments");
    size_t nel=0;
    if (jl_is_int32(args[0]))
        nel = (size_t)jl_unbox_int32(args[0]);
    else
        jl_error("Bufer.new: expected integer");
    return (jl_value_t*)jl_new_buffer((jl_type_t*)buf_type, nel);
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
            return (type_eqv_((jl_value_t*)((jl_func_type_t*)a)->from,
                              (jl_value_t*)((jl_func_type_t*)b)->from, stack) &&
                    type_eqv_((jl_value_t*)((jl_func_type_t*)a)->to,
                              (jl_value_t*)((jl_func_type_t*)b)->to, stack));
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

static jl_value_pair_t Empty_Env = {NULL,NULL,NULL};

static int type_le_generic(jl_value_t *a, jl_value_t *b)
{
    jl_value_pair_t *env = jl_type_conform((jl_type_t*)a, (jl_type_t*)b);
    if (env == NULL) return 0;
    jl_value_pair_t *vp = env;
    jl_value_pair_t *x;
    // make sure all typevars correspond to other unique typevars
    while (vp != &Empty_Env) {
        if (!jl_is_typevar(vp->b))
            return 0;
        x = env;
        while (x != &Empty_Env) {
            if (x != vp) {
                if (x->b == vp->b)
                    return 0;
            }
            x = x->next;
        }
        vp = vp->next;
    }
    return 1;
}

int jl_types_equal_generic(jl_value_t *a, jl_value_t *b)
{
    return type_le_generic(a, b) && type_le_generic(b, a);
}

static jl_type_t *apply_type_ctor_(jl_typector_t *tc, jl_value_t **params,
                                   size_t n)
{
    size_t i;
    char *tname = jl_tname((jl_value_t*)tc)->name->name;
    for(i=0; i < n; i++) {
        jl_value_t *pi = params[i];
        if (!jl_is_typector(pi) && !jl_is_type(pi) && !jl_is_int32(pi) &&
            !jl_is_typevar(pi))
            jl_errorf("invalid parameter for type %s", tname);
    }
    if (tc == jl_ntuple_type && (n==1||n==2) && jl_is_int32(params[0])) {
        size_t nt = jl_unbox_int32(params[0]);
        if (nt == 0) return (jl_type_t*)jl_null;
        if (n==2) {
            jl_tuple_t *tup = jl_alloc_tuple(nt);
            // extract the T from T...
            jl_value_t *eltype = jl_tparam0(params[1]);
            for(i=0; i < nt; i++) {
                jl_tupleset(tup, i, eltype);
            }
            return (jl_type_t*)tup;
        }
    }
    jl_tuple_t *tp = tc->parameters;
    if (n > tp->length)
        jl_errorf("too many parameters for type %s", tname);
    jl_value_t **env = alloca(2 * tp->length * sizeof(jl_value_t*));
    for(i=0; i < tp->length; i++) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_tupleref(tp,i);
        assert(jl_is_typevar(tv));
        env[i*2+0] = (jl_value_t*)tv;
        if (i >= n) {
            env[i*2+1] = jl_new_struct(jl_tvar_type, jl_gensym(),
                                       tv->lb, tv->ub);
        }
        else {
            env[i*2+1] = jl_add_dummy_type_vars(params[i]);
        }
    }
    return jl_instantiate_type_with(tc->body, env, tp->length);
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

JL_CALLABLE(jl_f_tuple);

static jl_type_t *inst_type_w_(jl_value_t *t, jl_value_t **env, size_t n,
                               typekey_stack_t *stack);

static jl_function_t *instantiate_gf(jl_function_t *f,
                                     jl_value_t **env, size_t n,
                                     typekey_stack_t *stack)
{
    if (f == NULL || !jl_is_gf(f)) return f;
    jl_function_t *newgf = jl_new_generic_function(jl_gf_name(f));
    jl_methlist_t *ml = jl_gf_mtable(f)->mlist;
    jl_tuple_t *sp = (jl_tuple_t*)jl_f_tuple(NULL, env, n*2);
    size_t i;
    for(i=0; i < sp->length; i+=2) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_tupleref(sp,i);
        jl_tupleset(sp, i, (jl_value_t*)tv->name);
    }
    while (ml != NULL) {
        jl_add_method(newgf, (jl_tuple_t*)ml->sig,
                      jl_instantiate_method(ml->func, sp));
        ml = ml->next;
    }
    ml = jl_gf_mtable(f)->generics;
    while (ml != NULL) {
        jl_function_t *meth = jl_instantiate_method(ml->func, sp);
        jl_add_method(newgf, (jl_tuple_t*)inst_type_w_((jl_value_t*)ml->sig, env, n, stack), meth);
        ml = ml->next;
    }
    return newgf;
}

static jl_type_t *inst_type_w_(jl_value_t *t, jl_value_t **env, size_t n,
                               typekey_stack_t *stack)
{
    typekey_stack_t top;
    size_t i;
    if (n == 0) return (jl_type_t*)t;
    if (jl_is_typevar(t)) {
        for(i=0; i < n; i++) {
            if (env[i*2] == t)
                return (jl_type_t*)env[i*2+1];
        }
        return (jl_type_t*)t;
    }
    if (jl_is_tuple(t)) {
        jl_tuple_t *p = (jl_tuple_t*)t;
        jl_tuple_t *nt = jl_alloc_tuple(p->length);
        for(i=0; i < p->length; i++) {
            jl_tupleset(nt, i, (jl_value_t*)inst_type_w_(jl_tupleref(p,i), env, n, stack));
        }
        return (jl_type_t*)nt;
    }
    if (jl_is_union_type(t)) {
        jl_tuple_t *tw = (jl_tuple_t*)inst_type_w_((jl_value_t*)((jl_uniontype_t*)t)->types,
                                                   env, n, stack);
        return (jl_type_t*)jl_new_uniontype(tw);
    }
    if (jl_is_func_type(t)) {
        jl_func_type_t *ft = (jl_func_type_t*)t;
        return (jl_type_t*)jl_new_functype(inst_type_w_((jl_value_t*)ft->from, env, n, stack),
                                           inst_type_w_((jl_value_t*)ft->to  , env, n, stack));
    }
    if (jl_is_some_tag_type(t)) {
        jl_tag_type_t *tt = (jl_tag_type_t*)t;
        jl_tuple_t *tp = tt->parameters;
        if (jl_is_null(tp))
            return (jl_type_t*)t;
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
            jl_tag_type_t *ntt =
                (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind,
                                       TAG_TYPE_NW);
            top.key = iparams;
            top.n = ntp+1;
            top.type = (jl_type_t*)ntt;
            top.next = stack;
            stack = &top;
            ntt->name = tn;
            ntt->super = (jl_tag_type_t*)inst_type_w_((jl_value_t*)tagt->super,env,n,stack);
            ntt->parameters = iparams_tuple;
            return (jl_type_t*)ntt;
        }
        else if (jl_is_bits_type(t)) {
            jl_bits_type_t *bitst = (jl_bits_type_t*)t;
            jl_bits_type_t *nbt =
                (jl_bits_type_t*)newobj((jl_type_t*)jl_bits_kind,
                                        BITS_TYPE_NW);
            top.key = iparams;
            top.n = ntp+1;
            top.type = (jl_type_t*)nbt;
            top.next = stack;
            stack = &top;
            nbt->name = tn;
            nbt->super = (jl_tag_type_t*)inst_type_w_((jl_value_t*)bitst->super, env, n, stack);
            nbt->parameters = iparams_tuple;
            nbt->nbits = bitst->nbits;
            nbt->fconvert = instantiate_gf(bitst->fconvert, env, n, stack);
            return (jl_type_t*)nbt;
        }
        else {
            assert(jl_is_struct_type(t));
            jl_struct_type_t *st = (jl_struct_type_t*)t;
            // create and initialize new struct type
            jl_struct_type_t *nst =
                (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind,
                                          STRUCT_TYPE_NW);
            // associate these parameters with the new struct type on
            // the stack, in case one of its field types references it.
            top.key = iparams;
            top.n = ntp+1;
            top.type = (jl_type_t*)nst;
            top.next = stack;
            stack = &top;
            nst->name = tn;
            nst->super = (jl_tag_type_t*)inst_type_w_((jl_value_t*)st->super, env,n,stack);
            nst->parameters = iparams_tuple;
            nst->names = st->names;
            nst->types = jl_null; // to be filled in below
            nst->fnew = jl_new_closure(st->fnew->fptr, (jl_value_t*)nst);
            nst->fconvert = jl_bottom_func;
            nst->uid = 0;
            jl_tuple_t *ftypes = st->types;
            if (ftypes != NULL) {
                // recursively instantiate the types of the fields
                jl_tuple_t *nftypes = jl_alloc_tuple(ftypes->length);
                for(i=0; i < ftypes->length; i++) {
                    jl_tupleset(nftypes, i,
                                (jl_value_t*)inst_type_w_(jl_tupleref(ftypes,i),
                                                          env,n,stack));
                }
                nst->types = nftypes;
            }
            nst->fconvert = instantiate_gf(st->fconvert, env, n, stack);
            cache_type_(iparams, ntp+1, (jl_type_t*)nst);
            return (jl_type_t*)nst;
        }
    }
    return (jl_type_t*)t;
}

jl_type_t *jl_instantiate_type_with(jl_type_t *t, jl_value_t **env, size_t n)
{
    return inst_type_w_((jl_value_t*)t, env, n, NULL);
}

int jl_subtype_le(jl_value_t *a, jl_value_t *b, int ta, int tb, int morespecific);

int jl_tuple_subtype(jl_value_t **child, size_t cl,
                     jl_value_t **parent, size_t pl, int ta, int tb,
                     int morespecific)
{
    size_t ci=0, pi=0;
    while(1) {
        int cseq = !ta && (ci<cl) && jl_is_seq_type(child[ci]);
        int pseq = !tb && (pi<pl) && jl_is_seq_type(parent[pi]);
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

        if (!jl_subtype_le((ta&&!jl_is_tuple(ce)) ?
                           (jl_value_t*)jl_typeof(ce) : ce,
                           (tb&&!jl_is_tuple(pe)) ?
                           (jl_value_t*)jl_typeof(pe) : pe,
                           ta&&jl_is_tuple(ce),
                           tb&&jl_is_tuple(pe),
                           morespecific))
            return 0;

        if (cseq && pseq) return 1;
        if (!cseq) ci++;
        if (!pseq) pi++;
    }
    return 0;
}

/*
  ta and tb specify whether typeof() should be implicitly applied
  to the arguments a and b. this is used for tuple types to avoid
  allocating them explicitly.
  morespecific means we only care whether a is more specific than b,
  not necessarily a strict subtype
*/
int jl_subtype_le(jl_value_t *a, jl_value_t *b, int ta, int tb, int morespecific)
{
    if (jl_is_typector(a)) a = (jl_value_t*)((jl_typector_t*)a)->body;
    if (jl_is_typector(b)) b = (jl_value_t*)((jl_typector_t*)b)->body;
    size_t i;
    if (jl_is_tuple(a)) {
        if ((jl_tuple_t*)b == jl_tuple_type) return 1;
        if (jl_is_tag_type(b) &&
            ((jl_tag_type_t*)b)->name == jl_ntuple_typename) {
            jl_tuple_t *tp = ((jl_tag_type_t*)b)->parameters;
            size_t alen = ((jl_tuple_t*)a)->length;
            if (jl_is_seq_type(jl_tupleref(a,alen-1)))
                return 0;
            jl_value_t *nt_len = jl_tupleref(tp,0);
            if (jl_is_int32(nt_len)) {
                if (alen != jl_unbox_int32(nt_len))
                    return 0;
            }
            return jl_subtype_le(a, jl_tupleref(tp,1), ta, tb, morespecific);
        }
        if (jl_is_tuple(b)) {
            return jl_tuple_subtype(&jl_tupleref(a,0), ((jl_tuple_t*)a)->length,
                                    &jl_tupleref(b,0), ((jl_tuple_t*)b)->length,
                                    ta, tb, morespecific);
        }
    }
    if (jl_is_union_type(a)) {
        assert(!ta);
        jl_tuple_t *ap = ((jl_uniontype_t*)a)->types;
        for(i=0; i < ap->length; i++) {
            if (!jl_subtype_le(jl_tupleref(ap,i), b, 0, tb, morespecific))
                return 0;
        }
        return 1;
    }
    if (jl_is_union_type(b)) {
        assert(!tb);
        jl_tuple_t *bp = ((jl_uniontype_t*)b)->types;
        for(i=0; i < bp->length; i++) {
            if (jl_subtype_le(a, jl_tupleref(bp,i), ta, 0, morespecific))
                return 1;
        }
        return 0;
    }

    if ((jl_tag_type_t*)b == jl_any_type) return 1;
    if (a == b) return 1;
    if (jl_is_typevar(a)) return 0;
    if (jl_is_typevar(b)) return 1;
    if ((jl_tag_type_t*)a == jl_any_type) return 0;
    if (jl_is_tuple(b)) {
        if (jl_is_tag_type(a) &&
            ((jl_tag_type_t*)a)->name == jl_ntuple_typename) {
            return jl_subtype_le(jl_tupleref(((jl_tag_type_t*)a)->parameters,
                                             1),
                                 b, 0, tb, morespecific);
        }
        return 0;
    }
    if (jl_is_tuple(a)) return 0;
    assert(!ta && !tb);

    if (jl_is_int32(a) && jl_is_int32(b))
        return (jl_unbox_int32(a)==jl_unbox_int32(b));

    if (jl_is_func_type(a)) {
        if (jl_is_func_type(b)) {
            jl_func_type_t *fa = (jl_func_type_t*)a;
            jl_func_type_t *fb = (jl_func_type_t*)b;
            return ( (jl_is_typevar(fb->from) ||
                      jl_subtype_le((jl_value_t*)fb->from,
                                    (jl_value_t*)fa->from, 0, 0, morespecific)) &&
                      jl_subtype_le((jl_value_t*)fa->to,
                                    (jl_value_t*)fb->to,   0, 0, morespecific) );
        }
        return 0;
    }
    else if (jl_is_func_type(b)) {
        return 0;
    }

    assert(jl_is_some_tag_type(a));
    assert(jl_is_some_tag_type(b));
    jl_tag_type_t *tta = (jl_tag_type_t*)a;
    jl_tag_type_t *ttb = (jl_tag_type_t*)b;
    int super=0;
    while (tta != (jl_tag_type_t*)jl_any_type) {
        if (tta->name == ttb->name) {
            if (super && morespecific)
                return 1;
            if (tta->name == jl_ntuple_typename) {
                // NTuple must be covariant
                return jl_subtype_le(jl_tupleref(tta->parameters,1),
                                     jl_tupleref(ttb->parameters,1),
                                     0, 0, morespecific);
            }
            assert(tta->parameters->length == ttb->parameters->length);
            for(i=0; i < tta->parameters->length; i++) {
                jl_value_t *apara = jl_tupleref(tta->parameters,i);
                jl_value_t *bpara = jl_tupleref(ttb->parameters,i);
                if (jl_is_typevar(bpara) && (!jl_is_typevar(apara) || super))
                    continue;
                if (!jl_types_equal(apara, bpara))
                    return 0;
            }
            return 1;
        }
        tta = tta->super; super = 1;
    }

    return 0;
}

int jl_subtype(jl_value_t *a, jl_value_t *b, int ta, int tb)
{
    return jl_subtype_le(a, b, ta, tb, 0);
}

int jl_type_morespecific(jl_value_t *a, jl_value_t *b, int ta, int tb)
{
    return jl_subtype_le(a, b, ta, tb, 1);
}

static jl_value_pair_t *type_conform_(jl_type_t *child, jl_type_t *parent,
                                      jl_value_pair_t *env,
                                      int morespecific);

static jl_value_pair_t *tuple_conform(jl_tuple_t *child, jl_tuple_t *parent,
                                      jl_value_pair_t *env,
                                      int morespecific)
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

        env = type_conform_((jl_type_t*)ce, (jl_type_t*)pe, env, morespecific);
        if (env == NULL) return NULL;

        if (cseq && pseq) return env;
        if (!cseq) ci++;
        if (!pseq) pi++;
    }
    return env;
}

jl_value_pair_t *jl_pair(jl_value_t *a, jl_value_t *b)
{
    jl_value_pair_t *np = (jl_value_pair_t*)allocb(sizeof(jl_value_pair_t));
    np->a = a;
    np->b = b;
    np->next = NULL;
    return np;
}

static jl_value_pair_t *type_conform_(jl_type_t *child, jl_type_t *parent,
                                      jl_value_pair_t *env,
                                      int morespecific)
{
    size_t i;
    if (jl_is_typevar(parent)) {
        jl_value_pair_t *p = env;
        while (p != NULL) {
            if (p->a == (jl_value_t*)parent) {
                if (jl_types_equal((jl_value_t*)child, (jl_value_t*)p->b))
                    return env;
                else
                    return NULL;
            }
            p = p->next;
        }
        jl_value_pair_t *np = jl_pair((jl_value_t*)parent, (jl_value_t*)child);
        np->next = env;
        return np;
    }
    if (jl_is_typevar(child)) return NULL;
    if (jl_is_int32(child) && jl_is_int32(parent)) {
        if (jl_unbox_int32((jl_value_t*)child) == jl_unbox_int32((jl_value_t*)parent))
            return env;
        return NULL;
    }
    if (child == parent) return env;
    if (parent == (jl_type_t*)jl_any_type) return env;
    if (child  == (jl_type_t*)jl_any_type) return NULL;

    if (jl_is_union_type(child)) {
        jl_tuple_t *t = ((jl_uniontype_t*)child)->types;
        for(i=0; i < t->length; i++) {
            env = type_conform_((jl_type_t*)jl_tupleref(t,i),
                                (jl_type_t*)parent, env, morespecific);
            if (env == NULL) return NULL;
        }
        return env;
    }
    if (jl_is_union_type(parent)) {
        jl_tuple_t *t = ((jl_uniontype_t*)parent)->types;
        for(i=0; i < t->length; i++) {
            jl_value_pair_t *p =
                type_conform_((jl_type_t*)child,
                              (jl_type_t*)jl_tupleref(parent,i),
                              env, morespecific);
            if (p != NULL) return p;
        }
        return NULL;
    }

    if (jl_is_func_type(parent)) {
        if (jl_is_func_type(child)) {
            env = type_conform_(((jl_func_type_t*)child)->from,
                                ((jl_func_type_t*)parent)->from, env,
                                morespecific);
            if (env == NULL) return NULL;
            return type_conform_(((jl_func_type_t*)child)->to,
                                 ((jl_func_type_t*)parent)->to, env,
                                 morespecific);
        }
        return NULL;
    }
    else if (jl_is_func_type(child)) {
        return NULL;
    }

    if (jl_is_tuple(child)) {
        if (jl_is_tag_type(parent) &&
            ((jl_tag_type_t*)parent)->name == jl_ntuple_typename) {
            jl_tuple_t *tp = ((jl_tag_type_t*)parent)->parameters;
            size_t alen = ((jl_tuple_t*)child)->length;
            if (jl_is_seq_type(jl_tupleref(child,alen-1)))
                return NULL;
            jl_value_t *nt_len = jl_tupleref(tp,0);
            /*
            if (jl_is_int32(nt_len)) {
                if (alen != jl_unbox_int32(nt_len))
                    return NULL;
            }
            */
            jl_value_t *childlen = jl_box_int32(((jl_tuple_t*)child)->length);
            if (jl_is_typevar(nt_len)) {
                env = type_conform_((jl_type_t*)childlen, (jl_type_t*)nt_len,
                                    env, morespecific);
                if (env == NULL) return NULL;
            }
            else {
                return NULL;
            }
            return tuple_conform((jl_tuple_t*)child,
                                 (jl_tuple_t*)jl_tupleref(tp, 1), env,
                                 morespecific);
        }

        if (jl_is_tuple(parent)) {
            return tuple_conform((jl_tuple_t*)child, (jl_tuple_t*)parent, env,
                                 morespecific);
        }
        return NULL;
    }
    if (jl_is_tuple(parent)) {
        if (jl_is_tag_type(child) &&
            ((jl_tag_type_t*)child)->name == jl_ntuple_typename) {
            return tuple_conform((jl_tuple_t*)jl_tupleref(((jl_tag_type_t*)child)->parameters,
                                                          1),
                                 (jl_tuple_t*)parent, env, morespecific);
        }
        return NULL;
    }

    assert(jl_is_some_tag_type(child));
    assert(jl_is_some_tag_type(parent));
    jl_tag_type_t *tta = (jl_tag_type_t*)child;
    jl_tag_type_t *ttb = (jl_tag_type_t*)parent;
    int super = 0;
    while (tta != (jl_tag_type_t*)jl_any_type) {
        if (tta->name == ttb->name) {
            if (super && morespecific)
                return env;
            assert(tta->parameters->length == ttb->parameters->length);
            for(i=0; i < tta->parameters->length; i++) {
                env = type_conform_((jl_type_t*)jl_tupleref(tta->parameters,i),
                                    (jl_type_t*)jl_tupleref(ttb->parameters,i),
                                    env, morespecific);
                if (env == NULL) return NULL;
            }
            return env;
        }
        tta = tta->super; super = 1;
    }

    return NULL;
}

/*
  typically a is a concrete type and b is a type containing typevars.
  this function tries to find a typevar assignment such that "a" is a subtype
  of "b".
  returns a linked list of (typevar,type) pairs.
  used to infer static parameter values in generic method definitions.
*/
jl_value_pair_t *jl_type_conform(jl_type_t *a, jl_type_t *b)
{
    return type_conform_(a, b, &Empty_Env, 0);
}

jl_value_pair_t *jl_type_conform_morespecific(jl_type_t *a, jl_type_t *b)
{
    return type_conform_(a, b, &Empty_Env, 1);
}

static jl_bits_type_t *make_scalar_type(const char *name,
                                        jl_typector_t *super,
                                        int nbits)
{
    jl_bits_type_t *bt =
        jl_new_bitstype((jl_value_t*)jl_symbol(name), jl_any_type,
                        jl_null, nbits);
    bt->super = (jl_tag_type_t*)jl_apply_type_ctor(super, jl_tuple(1, bt));
    assert(jl_is_tag_type(bt->super));
    return bt;
}

static jl_typector_t *make_scalar_subtype(const char *name,
                                          jl_typector_t *super)
{
    jl_typector_t *t;
    jl_tuple_t *tv = typevars(1, "T");
    t = jl_new_type_ctor(tv,
                         (jl_type_t*)jl_new_tagtype((jl_value_t*)jl_symbol(name),
                                                    (jl_tag_type_t*)jl_apply_type_ctor(super, tv),
                                                    tv));
    return t;
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
    jl_tuple_type = jl_alloc_tuple(1);
    jl_tuple_type->type = (jl_type_t*)jl_tuple_type;

    jl_null = (jl_tuple_t*)newobj((jl_type_t*)jl_tuple_type, 1);
    jl_null->length = 0;

    jl_any_func = jl_new_functype((jl_type_t*)jl_any_type, (jl_type_t*)jl_any_type);

    jl_bottom_func = jl_new_closure(jl_f_no_function, NULL);
    jl_identity_func = jl_new_closure(jl_f_identity, NULL);

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
    jl_struct_kind->fnew = jl_struct_kind->fconvert = jl_bottom_func;
    jl_struct_kind->uid = t_uid_ctr++;

    jl_tag_kind->name = jl_new_typename(jl_symbol("TagKind"));
    jl_tag_kind->super = jl_type_type;
    jl_tag_kind->parameters = jl_null;
    jl_tag_kind->names = jl_tuple(3, jl_symbol("name"), jl_symbol("super"),
                                  jl_symbol("parameters"));
    jl_tag_kind->types = jl_tuple(3, jl_typename_type, jl_type_type,
                                  jl_tuple_type);
    jl_tag_kind->fnew = jl_tag_kind->fconvert = jl_bottom_func;
    jl_tag_kind->uid = t_uid_ctr++;

    jl_func_kind->name = jl_new_typename(jl_symbol("FuncKind"));
    jl_func_kind->super = jl_type_type;
    jl_func_kind->parameters = jl_null;
    jl_func_kind->names = jl_tuple(2, jl_symbol("from"), jl_symbol("to"));
    jl_func_kind->types = jl_tuple(2, jl_type_type, jl_type_type);
    jl_func_kind->fnew = jl_func_kind->fconvert = jl_bottom_func;
    jl_func_kind->uid = t_uid_ctr++;

    jl_typename_type->name = jl_new_typename(jl_symbol("TypeName"));
    jl_typename_type->super = jl_any_type;
    jl_typename_type->parameters = jl_null;
    jl_typename_type->names = jl_tuple(1, jl_symbol("name"));
    jl_typename_type->types = jl_tuple(1, jl_sym_type);
    jl_typename_type->fnew = jl_typename_type->fconvert = jl_bottom_func;
    jl_typename_type->uid = t_uid_ctr++;

    jl_sym_type->name = jl_new_typename(jl_symbol("Symbol"));
    jl_sym_type->super = jl_any_type;
    jl_sym_type->parameters = jl_null;
    jl_sym_type->names = jl_null;
    jl_sym_type->types = jl_null;
    jl_sym_type->fnew = jl_sym_type->fconvert = jl_bottom_func;
    jl_sym_type->uid = t_uid_ctr++;

    jl_any_type->name = jl_new_typename(jl_symbol("Any"));
    jl_any_type->super = jl_any_type;
    jl_any_type->parameters = jl_null;

    jl_type_type->name = jl_new_typename(jl_symbol("Type"));
    jl_type_type->super = jl_any_type;
    jl_type_type->parameters = jl_null;

    jl_tuple_typename = jl_new_typename(jl_symbol("Tuple"));

    // now they can be used to create the remaining base kinds and types
    jl_union_kind = jl_new_struct_type(jl_symbol("UnionKind"),
                                       jl_type_type, jl_null,
                                       jl_tuple(1, jl_symbol("types")),
                                       jl_tuple(1, jl_tuple_type));
    jl_union_kind->fconvert = jl_bottom_func;

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
    jl_bits_kind->fnew = jl_bits_kind->fconvert = jl_bottom_func;
    
    jl_tvar_type = jl_new_struct_type(jl_symbol("TypeVar"),
                                      jl_any_type, jl_null,
                                      jl_tuple(3, jl_symbol("name"),
                                               jl_symbol("lb"),
                                               jl_symbol("ub")),
                                      jl_tuple(3, jl_sym_type, jl_type_type,
                                               jl_type_type));
    jl_tvar_type->fconvert = jl_bottom_func;

    jl_typector_type = jl_new_struct_type(jl_symbol("TypeConstructor"),
                                          jl_any_type, jl_null,
                                          jl_tuple(2, jl_symbol("parameters"),
                                                   jl_symbol("body")),
                                          jl_tuple(2, jl_tuple_type,
                                                   jl_type_type));
    jl_typector_type->fconvert = jl_bottom_func;

    jl_tuple_t *tv;
    tv = typevars(1, "T");
    jl_seq_type =
        jl_new_type_ctor(tv,
                         (jl_type_t*)jl_new_tagtype((jl_value_t*)jl_symbol("..."),
                                                    jl_any_type, tv));

    jl_tupleset(jl_tuple_type, 0,
                (jl_value_t*)jl_apply_type_ctor(jl_seq_type,
                                                jl_tuple(1,jl_any_type)));

    tv = typevars(2, "N", "T");
    // NTuple[N,T] expands to NTuple[N,(T...)], so it matches the same
    // as (T...) plus a length constraint
    jl_ntuple_type =
        jl_new_type_ctor(tv,
                         (jl_type_t*)
                         jl_new_tagtype((jl_value_t*)jl_symbol("NTuple"),
                                        jl_any_type,
                                        jl_tuple(2,jl_tupleref(tv,0),
                                                 jl_tuple(1,
                                                          jl_apply_type_ctor(jl_seq_type,
                                                                             jl_tuple(1,jl_tupleref(tv,1)))))));
    jl_ntuple_typename = ((jl_tag_type_t*)jl_ntuple_type->body)->name;

    tv = typevars(2, "T", "N");
    jl_tensor_type =
        jl_new_type_ctor(tv,
                         (jl_type_t*)jl_new_tagtype((jl_value_t*)jl_symbol("Tensor"),
                                                    jl_any_type, tv));

    tv = typevars(1, "T");
    jl_scalar_type =
        jl_new_type_ctor(tv,
                         jl_apply_type_ctor(jl_tensor_type,
                                            jl_tuple(2, jl_tupleref(tv,0),
                                                     jl_bottom_type)));

    jl_number_type = make_scalar_subtype("Number", jl_scalar_type);
    jl_real_type = make_scalar_subtype("Real", jl_number_type);
    jl_int_type = make_scalar_subtype("Int", jl_real_type);

    jl_int32_type = make_scalar_type("Int32", jl_int_type, 32);
    jl_value_t *zero = jl_new_box_int32(0);
    jl_tupleset(((jl_tag_type_t*)jl_scalar_type->body)->parameters, 1, zero);
    jl_tupleset(((jl_tag_type_t*)jl_number_type->body)->super->parameters, 1,
                zero);
    jl_tupleset(((jl_tag_type_t*)jl_real_type->body)->super->super->parameters, 1,
                zero);
    jl_tupleset(((jl_tag_type_t*)jl_int_type->body)->super->super->super->parameters, 1,
                zero);
    jl_tupleset(jl_int32_type->super->super->super->super->parameters, 1,
                zero);

    jl_float_type = make_scalar_subtype("Float", jl_real_type);

    jl_bool_type    = make_scalar_type("Bool"  , jl_scalar_type, 8);
    jl_int8_type    = make_scalar_type("Int8"  , jl_int_type, 8);
    jl_uint8_type   = make_scalar_type("Uint8" , jl_int_type, 8);
    jl_int16_type   = make_scalar_type("Int16" , jl_int_type, 16);
    jl_uint16_type  = make_scalar_type("Uint16", jl_int_type, 16);
    jl_uint32_type  = make_scalar_type("Uint32", jl_int_type, 32);
    jl_int64_type   = make_scalar_type("Int64" , jl_int_type, 64);
    jl_uint64_type  = make_scalar_type("Uint64", jl_int_type, 64);

    jl_float32_type = make_scalar_type("Float32", jl_float_type, 32);
    jl_float64_type = make_scalar_type("Float64", jl_float_type, 64);

    jl_false = jl_new_box_int8(0); jl_false->type = (jl_type_t*)jl_bool_type;
    jl_true  = jl_new_box_int8(1); jl_true->type  = (jl_type_t*)jl_bool_type;

    tv = typevars(1, "T");
    jl_struct_type_t *bufstruct = 
        jl_new_struct_type(jl_symbol("Buffer"),
                           jl_any_type, tv, jl_null, jl_null);
    jl_buffer_typename = bufstruct->name;
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

    jl_struct_type_t *boxstruct =
        jl_new_struct_type(jl_symbol("Box"),
                           jl_any_type, tv,
                           jl_tuple(1, jl_symbol("contents")), tv);
    jl_box_typename = boxstruct->name;
    jl_box_type = jl_new_type_ctor(tv, (jl_type_t*)boxstruct);
    jl_box_any_type =
        (jl_type_t*)jl_apply_type_ctor(jl_box_type,
                                       jl_tuple(1, jl_any_type));

    jl_lambda_info_type =
        jl_new_struct_type(jl_symbol("LambdaStaticData"),
                           jl_any_type, jl_null, jl_null, jl_null);
    jl_lambda_info_type->fnew = jl_bottom_func;
    jl_lambda_info_type->fconvert = jl_bottom_func;

    jl_the_empty_buffer = jl_new_buffer(jl_buffer_any_type, 0);

    tv = typevars(2, "A", "B");
    jl_functype_ctor =
        jl_new_type_ctor(tv,
                         (jl_type_t*)jl_new_functype((jl_type_t*)jl_tupleref(tv,0),
                                                     (jl_type_t*)jl_tupleref(tv,1)));

    jl_intrinsic_type = jl_new_bitstype((jl_value_t*)jl_symbol("IntrinsicFunction"),
                                        jl_any_type, jl_null, 32);
    jl_intrinsic_type->fconvert = jl_bottom_func;

    call_sym = jl_symbol("call");
    quote_sym = jl_symbol("quote");
    top_sym = jl_symbol("top");
    list_sym = jl_symbol("list");
    dots_sym = jl_symbol("...");
    expr_sym = jl_symbol("expr");
    tuple_sym = jl_symbol("tuple");
    dollar_sym = jl_symbol("$");
    line_sym = jl_symbol("line");
    continue_sym = jl_symbol("continue");

    init_box_caches();
}
