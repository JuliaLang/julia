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

jl_type_t *jl_any_type;
jl_type_t *jl_type_type;
jl_type_t *jl_typename_type;
jl_type_t *jl_sym_type;
jl_type_t *jl_tuple_type;
jl_type_t *jl_tvar_type;

jl_type_t *jl_buffer_type;
jl_type_t *jl_function_type;
jl_type_t *jl_tensor_type;
jl_type_t *jl_seq_type;
jl_type_t *jl_union_type;
jl_type_t *jl_bottom_type;
jl_type_t *jl_scalar_type;
jl_type_t *jl_number_type;
jl_type_t *jl_real_type;
jl_type_t *jl_int_type;
jl_type_t *jl_float_type;

jl_type_t *jl_bool_type;
jl_type_t *jl_int8_type;
jl_type_t *jl_uint8_type;
jl_type_t *jl_int16_type;
jl_type_t *jl_uint16_type;
jl_type_t *jl_int32_type;
jl_type_t *jl_uint32_type;
jl_type_t *jl_int64_type;
jl_type_t *jl_uint64_type;
jl_type_t *jl_single_type;
jl_type_t *jl_double_type;

jl_value_t *jl_null;
jl_value_t *jl_true;
jl_value_t *jl_false;

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

jl_value_t *jl_new_struct(jl_type_t *type, ...)
{
    va_list args;
    size_t nf = type->fields->length;
    size_t i;
    va_start(args, type);
    jl_value_t *jv = newobj(type, type->nw);
    for(i=0; i < nf; i++) {
        ((jl_value_t**)jv)[i+1] = va_arg(args, jl_value_t*);
    }
    va_end(args);
    return jv;
}

JL_CALLABLE(jl_tuple)
{
    size_t i;
    jl_tuple_t *t = (jl_tuple_t*)newobj(jl_tuple_type, nargs+1);
    t->length = nargs;
    for(i=0; i < nargs; i++) {
        ((jl_value_t**)t)[i+2] = args[i];
    }
    return (jl_value_t*)t;
}

jl_tuple_t *jltuple(size_t n, ...)
{
    va_list args;
    size_t i;
    va_start(args, n);
    jl_tuple_t *jv = (jl_tuple_t*)newobj(jl_tuple_type, n+1);
    jv->length = n;
    for(i=0; i < n; i++) {
        ((jl_value_t**)jv)[i+2] = va_arg(args, jl_value_t*);
    }
    va_end(args);
    return jv;
}

static jl_sym_t *symtab = NULL;

static jl_sym_t *mk_symbol(char *str)
{
    jl_sym_t *sym;
    size_t len = strlen(str);

    sym = (jl_sym_t*)allocb(sizeof(jl_sym_t)-sizeof(void*) + len + 1);
    sym->type = jl_sym_type;
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
#define jl_tparam0(t) jl_tupleref(((jl_type_t*)(t))->parameters, 0)
#define jl_tparam(t, n) jl_tupleref(((jl_type_t*)(t))->parameters, (n))

#define jl_tuplep(v) (((jl_value_t*)(v))->type->name==jl_tuple_type->name)

jl_buffer_t *jl_new_buffer(jl_type_t *buftype, size_t nel)
{
    assert(buftype->name == jl_buffer_type->name);
    jl_type_t *eltype = (jl_type_t*)jl_tparam0(buftype);
    void *data;
    if (eltype->nbytes) {
        data = alloc_pod(eltype->nbytes * nel);
    }
    else {
        data = allocb(sizeof(void*) * nel);
    }
    jl_buffer_t *b = (jl_buffer_t*)newobj(buftype, 2);
    b->length = nel;
    b->data = data;
    return b;
}

jl_typename_t *jl_new_typename(jl_sym_t *name)
{
    return (jl_typename_t*)jl_new_struct(jl_typename_type, name);
}

#define TYPE_NW 9
static int t_uid_ctr = 0;  // TODO: lock

jl_type_t *jl_new_type(jl_sym_t *name, jl_type_t *super,
                       jl_tuple_t *parameters,
                       jl_tuple_t *fields, int abstract)
{
    jl_type_t *t = (jl_type_t*)newobj(jl_type_type, TYPE_NW);
    t->name = jl_new_typename(name);
    t->super = super;
    t->parameters = parameters;
    t->fields = fields;
    t->abstract = abstract;
    t->nw = fields->length;
    t->numtype = N_NUMTYPES;
    t->nbytes = 0;
    t->uid = t_uid_ctr++;
    return t;
}

static jl_value_t *tvar(char *name)
{
    return jl_new_struct(jl_tvar_type, jl_symbol(name),
                         jl_bottom_type, jl_any_type);
}

#define BOX_FUNC(type,ctype)                                            \
jl_value_t *jl_box_##type(ctype x)                                      \
{                                                                       \
    jl_value_t *v = newobj(jl_##type##_type,                            \
                           NWORDS(LLT_ALIGN(sizeof(ctype),sizeof(void*)))); \
    *(ctype*)(&((void**)v)[1]) = x;                                     \
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
BOX_FUNC(single, float)
BOX_FUNC(double, double)

#define UNBOX_FUNC(type,ctype)                  \
ctype jl_unbox_##type(jl_value_t *v)            \
{                                               \
    assert(v->type == jl_##type##_type);        \
    return *(ctype*)(&((void**)v)[1]);          \
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
UNBOX_FUNC(single, float)
UNBOX_FUNC(double, double)

void jl_init_types()
{
    jl_type_type = (jl_type_t*)newobj(NULL, TYPE_NW);
    jl_type_type->type = jl_type_type;
    jl_type_type->nw = TYPE_NW;
    jl_type_type->numtype = N_NUMTYPES;
    jl_type_type->nbytes = 0;
    jl_type_type->uid = t_uid_ctr++;
    jl_type_type->abstract = 0;

    jl_tuple_type = (jl_type_t*)newobj(jl_type_type, TYPE_NW);
    jl_null = (jl_value_t*)jltuple(0);

    jl_sym_type = (jl_type_t*)newobj(jl_type_type, TYPE_NW);

    jl_typename_type = (jl_type_t*)newobj(jl_type_type, TYPE_NW);
    jl_typename_type->name = jl_new_typename(jl_symbol("Typename"));
    jl_typename_type->parameters = jl_null;
    jl_typename_type->fields = jltuple(1, jltuple(2, jl_symbol("name"),
                                                  jl_sym_type));
    jl_typename_type->nw = 1;
    jl_typename_type->numtype = N_NUMTYPES;
    jl_typename_type->nbytes = 0;
    jl_typename_type->uid = t_uid_ctr++;
    jl_typename_type->abstract = 0;

    jl_any_type = jl_new_type(jl_symbol("Any"), NULL, jl_null, jl_null, 1);
    jl_any_type->super = jl_any_type;

    jl_type_type->name = jl_new_typename(jl_symbol("Type"));
    jl_type_type->super = jl_any_type;
    jl_type_type->parameters = jl_null;
    jl_type_type->fields = jltuple(4, jltuple(2, jl_symbol("name"),
                                              jl_typename_type),
                                   jltuple(2, jl_symbol("super"),
                                           jl_type_type),
                                   jltuple(2, jl_symbol("parameters"),
                                           jl_tuple_type),
                                   jltuple(2, jl_symbol("fields"),
                                           jl_tuple_type));

    jl_tuple_type->name = jl_new_typename(jl_symbol("Tuple"));
    jl_tuple_type->super = jl_any_type;
    jl_tuple_type->parameters = jl_null;
    jl_tuple_type->fields = jl_null;
    jl_tuple_type->nw = 1;
    jl_tuple_type->numtype = N_NUMTYPES;
    jl_tuple_type->nbytes = 0;
    jl_tuple_type->uid = t_uid_ctr++;
    jl_tuple_type->abstract = 1;

    jl_typename_type->super = jl_any_type;

    jl_sym_type->name = jl_new_typename(jl_symbol("Symbol"));
    jl_sym_type->super = jl_any_type;
    jl_sym_type->parameters = jl_null;
    jl_sym_type->fields = jl_null;
    jl_sym_type->nw = 4;
    jl_sym_type->numtype = N_NUMTYPES;
    jl_sym_type->nbytes = 0;
    jl_sym_type->uid = t_uid_ctr++;
    jl_sym_type->abstract = 0;

    jl_union_type = jl_new_type(jl_symbol("Union"), jl_any_type,
                                jl_null, jl_null, 1);
    jl_bottom_type = jl_union_type;

    jl_tvar_type = jl_new_type(jl_symbol("TypeVar"), jl_any_type,
                               jl_null,
                               jltuple(4, jltuple(2, jl_symbol("name"),
                                                  jl_sym_type),
                                       jltuple(2, jl_symbol("lb"),
                                               jl_type_type),
                                       jltuple(2, jl_symbol("ub"),
                                               jl_type_type)), 0);

    jl_seq_type = jl_new_type(jl_symbol("..."), jl_any_type,
                              jltuple(1,tvar("T")),
                              jl_null, 1);

    jl_tensor_type = jl_new_type(jl_symbol("Tensor"), jl_any_type,
                                 jltuple(2,tvar("T"),
                                         tvar("n")), jl_null, 1);

    jl_function_type = jl_new_type(jl_symbol("Function"), jl_any_type,
                                   jltuple(2,tvar("A"),
                                           tvar("B")), jl_null, 0);
    jl_function_type->nw = 2;

    // jl_scalar_type =

    jl_buffer_type = jl_new_type(jl_symbol("Buffer"), jl_any_type,
                                 jltuple(1,tvar("T")),
                                 jltuple(1,jltuple(2,jl_symbol("length"),
                                                   jl_int64_type)), 0);
}
