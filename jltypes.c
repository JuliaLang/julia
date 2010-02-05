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

jl_struct_type_t *jl_buffer_type;
jl_tag_type_t *jl_seq_type;
jl_type_t *jl_bottom_type;
jl_tag_type_t *jl_tensor_type;
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

jl_tuple_t *jl_null;
jl_value_t *jl_true;
jl_value_t *jl_false;

jl_func_type_t *jl_any_func;

#ifdef BITS64
#define NWORDS(sz) (((sz)+7)>>3)
#else
#define NWORDS(sz) (((sz)+3)>>2)
#endif

#define allocb(nb) GC_MALLOC(nb)
#define alloc_pod(nb) GC_MALLOC_ATOMIC(nb)

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

#define jl_tupleref(t,i) (((jl_value_t**)(t))[2+(i)])
#define jl_tupleset(t,i,x) ((((jl_value_t**)(t))[2+(i)])=(x))

// the Type type --------------------------------------------------------------

#define jl_tparam0(t) jl_tupleref(((jl_tag_type_t*)(t))->parameters, 0)

#define jl_typeof(v) (((jl_value_t*)(v))->type)
#define jl_typeis(v,t) (jl_typeof(v)==(jl_type_t*)(t))

#define jl_tuplep(v) (((jl_value_t*)(v))->type == (jl_type_t*)jl_tuple_type)

#define jl_tagtypep(v)    (((jl_value_t*)(v))->type==(jl_type_t*)jl_tag_kind)
#define jl_bitstypep(v)   (((jl_value_t*)(v))->type==(jl_type_t*)jl_bits_kind)
#define jl_structtypep(v) (((jl_value_t*)(v))->type==(jl_type_t*)jl_struct_kind)
#define jl_functypep(v)   (((jl_value_t*)(v))->type==(jl_type_t*)jl_func_kind)
#define jl_uniontypep(v)  (((jl_value_t*)(v))->type==(jl_type_t*)jl_union_kind)

#define jl_typevarp(v)  (((jl_value_t*)(v))->type==(jl_type_t*)jl_tvar_type)
#define jl_typectorp(v) (((jl_value_t*)(v))->type==(jl_type_t*)jl_typector_type)

jl_typename_t *jl_new_typename(jl_sym_t *name)
{
    return (jl_typename_t*)jl_new_struct(jl_typename_type, name);
}

#define TAGTYPE_NW (NWORDS(sizeof(jl_tag_type_t))-1)
#define STRUCTTYPE_NW (NWORDS(sizeof(jl_struct_type_t))-1)
#define BITSTYPE_NW (NWORDS(sizeof(jl_bits_type_t))-1)
static int t_uid_ctr = 0;  // TODO: lock

jl_tag_type_t *jl_new_tagtype(jl_sym_t *name, jl_tag_type_t *super,
                             jl_tuple_t *parameters)
{
    jl_tag_type_t *t = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind,
                                            TAGTYPE_NW);
    t->name = jl_new_typename(name);
    t->super = super;
    t->parameters = parameters;
    return t;
}

jl_func_type_t *jl_new_functype(jl_type_t *a, jl_type_t *b)
{
    jl_func_type_t *t = (jl_func_type_t*)newobj((jl_type_t*)jl_func_kind, 2);
    if (!jl_tuplep(a) && !jl_typevarp(a))
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

void jl_error(char *str)
{
    fprintf(stderr, "%s", str);
    exit(1);
}

JL_CALLABLE(jl_new_struct_internal)
{
    jl_struct_type_t *t = (jl_struct_type_t*)clo;
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
                                                  STRUCTTYPE_NW);
    t->name = jl_new_typename(name);
    t->super = super;
    t->parameters = parameters;
    t->names = fnames;
    t->types = ftypes;
    t->fnew = jl_new_closure(jl_new_struct_internal, (jl_value_t*)t);
    t->fconvert = NULL; //TODO
    t->uid = t_uid_ctr++;
    return t;
}

jl_bits_type_t *jl_new_bitstype(jl_sym_t *name, jl_tag_type_t *super,
                               jl_tuple_t *parameters, size_t nbits)
{
    jl_bits_type_t *t = (jl_bits_type_t*)newobj((jl_type_t*)jl_bits_kind,
                                              BITSTYPE_NW);
    t->name = jl_new_typename(name);
    t->super = super;
    t->parameters = parameters;
    t->nbits = nbits;
    t->uid = t_uid_ctr++;
    return t;
}

jl_uniontype_t *jl_new_uniontype(jl_tuple_t *types)
{
    jl_uniontype_t *t = (jl_uniontype_t*)newobj((jl_type_t*)jl_union_kind, 1);
    t->types = types;
    return t;
}

// --- type constructors ---

jl_typector_t *jl_new_type_ctor(jl_tuple_t *params, jl_type_t *body)
{
    jl_typector_t *tc = (jl_typector_t*)newobj((jl_type_t*)jl_typector_type, 2);
    tc->parameters = params;
    tc->body = body;
    return tc;
}

static jl_value_t *tvar(char *name)
{
    return jl_new_struct(jl_tvar_type, jl_symbol(name),
                         jl_bottom_type, jl_any_type);
}

// --- type properties and predicates ---

int jl_typep(jl_value_t *v)
{
    if (jl_tuplep(v)) {
        jl_tuple_t *t = (jl_tuple_t*)v;
        int i;
        for(i=0; i < t->length; i++) {
            jl_value_t *vv = jl_tupleref(t, i);
            if (!jl_typevarp(vv) && !jl_typep(vv))
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
    if (jl_tuplep(v))
        return jl_tuple_type->name;
    if (jl_tagtypep(v) || jl_structtypep(v) || jl_bitstypep(v))
        return ((jl_tag_type_t*)v)->name;
    if (jl_typectorp(v))
        return jl_tname((jl_value_t*)((jl_typector_t*)v)->body);
    return ((jl_tag_type_t*)(v->type))->name;
}

jl_tag_type_t *jl_tsuper(jl_value_t *v)
{
    if (jl_tuplep(v))
        return jl_tuple_type;
    if (jl_tagtypep(v) || jl_structtypep(v) || jl_bitstypep(v))
        return ((jl_tag_type_t*)v)->super;
    return jl_any_type;
}

jl_tuple_t *jl_tparams(jl_value_t *v)
{
    if (jl_tuplep(v))
        return (jl_tuple_t*)v;
    if (jl_tagtypep(v) || jl_structtypep(v) || jl_bitstypep(v))
        return ((jl_tag_type_t*)v)->parameters;
    if (jl_uniontypep(v))
        return ((jl_uniontype_t*)v)->types;
    return jl_null;
}

int jl_has_typevarsp(jl_value_t *v)
{
    if (jl_typeis(v, jl_tvar_type))
        return 1;
    if (jl_tuplep(v)) {
        jl_tuple_t *t = (jl_tuple_t*)v;
        int i;
        for(i=0; i < t->length; i++) {
            if (jl_has_typevarsp(jl_tupleref(t, i)))
                return 1;
        }
        return 0;
    }
    if (v == (jl_value_t*)jl_scalar_type || jl_tsuper(v) == jl_scalar_type)
        return 0;
    if (jl_functypep(v))
        return jl_has_typevarsp((jl_value_t*)((jl_func_type_t*)v)->from) ||
            jl_has_typevarsp((jl_value_t*)((jl_func_type_t*)v)->to);
    return jl_has_typevarsp((jl_value_t*)jl_tparams(v));
}

#define jl_int32p(v) (((jl_value_t*)(v))->type == jl_int32_type)
#define jl_boolp(v) (((jl_value_t*)(v))->type == jl_bool_type)

static int tuple_memq(jl_tuple_t *t, int n, jl_value_t *v)
{
    int i;
    for(i=0; i < t->length && i < n; i++) {
        if (jl_tupleref(t, i) == v)
            return 1;
    }
    return 0;
}

static void tuple_copy(jl_tuple_t *dest, jl_tuple_t *src, int n)
{
    int i;
    for(i=0; i < n; i++) {
        jl_tupleset(dest, i, jl_tupleref(src, i));
    }
}

static jl_tuple_t *tuple_adjoinq(jl_tuple_t *t, int *n, jl_value_t *v)
{
    if (tuple_memq(t, *n, v))
        return t;
    if (*n >= t->length) {
        jl_tuple_t *nt = jl_alloc_tuple((*n)*2);
        tuple_copy(nt, t, t->length);
        t = nt;
    }
    jl_tupleset(t, *n, v);
    (*n)++;
    return t;
}

static jl_tuple_t *find_tvars(jl_tuple_t *dest, jl_value_t *v, int *pos)
{
    if (jl_typeis(v, jl_tvar_type))
        return tuple_adjoinq(dest, pos, v);
    if (jl_functypep(v)) {
        dest = find_tvars(dest, (jl_value_t*)((jl_func_type_t*)v)->from, pos);
        dest = find_tvars(dest, (jl_value_t*)((jl_func_type_t*)v)->to  , pos);
        return dest;
    }
    jl_tuple_t *params = jl_tparams(v);
    int i;
    for(i=0; i < params->length; i++) {
        jl_value_t *p = jl_tupleref(params, i);
        if (p != v)
            dest = find_tvars(dest, p, pos);
    }
    return dest;
}

jl_tuple_t *jl_find_tvars(jl_value_t *v)
{
    jl_tuple_t *t = jl_alloc_tuple(2);
    int pos=0;
    t = find_tvars(t, v, &pos);
    if (pos != t->length) {
        jl_tuple_t *nt = jl_alloc_tuple(pos);
        tuple_copy(nt, t, pos);
        return nt;
    }
    return t;
}

// constructors for some normal types -----------------------------------------

#define BOX_FUNC(type,c_type)                                            \
jl_value_t *jl_box_##type(c_type x)                                      \
{                                                                       \
    jl_value_t *v = newobj((jl_type_t*)jl_##type##_type,                \
                           NWORDS(LLT_ALIGN(sizeof(c_type),sizeof(void*)))); \
    *(c_type*)(&((void**)v)[1]) = x;                                     \
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

#define UNBOX_FUNC(type,c_type)                  \
c_type jl_unbox_##type(jl_value_t *v)            \
{                                               \
    assert(v->type == jl_##type##_type);        \
    return *(c_type*)(&((void**)v)[1]);          \
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

jl_buffer_t *jl_new_buffer(jl_struct_type_t *buf_type, size_t nel)
{
    assert(buf_type->name == jl_buffer_type->name);
    jl_type_t *el_type = (jl_type_t*)jl_tparam0(buf_type);
    void *data;
    if (jl_bitstypep(el_type)) {
        data = alloc_pod(((jl_bits_type_t*)el_type)->nbits/8 * nel);
    }
    else {
        data = allocb(sizeof(void*) * nel);
    }
    jl_buffer_t *b = (jl_buffer_t*)newobj((jl_type_t*)buf_type, 2);
    b->length = nel;
    b->data = data;
    return b;
}

void jl_init_types()
{
    // create base objects
    jl_struct_kind = (jl_struct_type_t*)newobj(NULL, STRUCTTYPE_NW);
    jl_struct_kind->type = (jl_type_t*)jl_struct_kind;
    jl_tag_kind = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCTTYPE_NW);
    jl_func_kind = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCTTYPE_NW);

    jl_typename_type = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCTTYPE_NW);
    jl_sym_type = (jl_struct_type_t*)newobj((jl_type_t*)jl_struct_kind, STRUCTTYPE_NW);

    jl_any_type = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAGTYPE_NW);
    jl_type_type = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAGTYPE_NW);
    jl_tuple_type = (jl_tag_type_t*)newobj((jl_type_t*)jl_tag_kind, TAGTYPE_NW);

    jl_null = jl_tuple(0);

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
    
}
