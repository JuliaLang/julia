#ifndef _JULIA_H_
#define _JULIA_H_

#include "htable.h"
#include "arraylist.h"

#define JL_VALUE_STRUCT \
    struct _jl_type_t *type;

typedef struct _jl_value_t {
    JL_VALUE_STRUCT
} jl_value_t;

typedef struct _jl_sym_t {
    JL_VALUE_STRUCT
    struct _jl_sym_t *left;
    struct _jl_sym_t *right;
    uptrint_t hash;    // precomputed hash value
    union {
        char name[1];
        void *_pad;    // ensure field aligned to pointer size
    };
} jl_sym_t;

typedef struct {
    JL_VALUE_STRUCT
    size_t length;
    jl_value_t *data[1];
} jl_tuple_t;

typedef struct {
    JL_VALUE_STRUCT
    size_t length;
    void *data;
} jl_buffer_t;

typedef struct {
    JL_VALUE_STRUCT
    jl_sym_t *name;
    // if this is the name of a parametric type, ctor points to the
    // original typector for the type.
    // a type alias, for example, might make a type constructor that is
    // not the original.
    struct _jl_typector_t *ctor;
} jl_typename_t;

typedef struct _jl_type_t {
    JL_VALUE_STRUCT
} jl_type_t;

typedef jl_value_t *(*jl_fptr_t)(jl_value_t*, jl_value_t**, uint32_t);

typedef struct {
    JL_VALUE_STRUCT
    jl_fptr_t fptr;
    jl_value_t *env;
} jl_function_t;

typedef struct {
    JL_VALUE_STRUCT
    jl_type_t *from;
    jl_type_t *to;
} jl_func_type_t;

typedef struct {
    JL_VALUE_STRUCT
    jl_tuple_t *types;
} jl_uniontype_t;

typedef struct _jl_tag_type_t {
    JL_VALUE_STRUCT
    jl_typename_t *name;
    struct _jl_tag_type_t *super;
    jl_tuple_t *parameters;
} jl_tag_type_t;

typedef struct {
    JL_VALUE_STRUCT
    jl_typename_t *name;
    jl_tag_type_t *super;
    jl_tuple_t *parameters;
    jl_tuple_t *names;
    jl_tuple_t *types;
    jl_function_t *fnew;
    jl_function_t *fconvert;
    // hidden fields:
    uptrint_t uid;
} jl_struct_type_t;

typedef struct {
    JL_VALUE_STRUCT
    jl_typename_t *name;
    jl_tag_type_t *super;
    jl_tuple_t *parameters;
    jl_function_t *fconvert;
    // hidden fields:
    size_t nbits;
    uptrint_t uid;
} jl_bits_type_t;

typedef struct _jl_typector_t {
    JL_VALUE_STRUCT
    jl_tuple_t *parameters;
    jl_type_t *body;
} jl_typector_t;

typedef struct {
    JL_VALUE_STRUCT
    jl_sym_t *name;
    jl_type_t *lb;  // lower bound
    jl_type_t *ub;  // upper bound
} jl_tvar_t;

typedef struct {
    // not first-class
    jl_sym_t *name;
    jl_value_t *value;
    jl_type_t *type;
    int constp;
    int exportp;
} jl_binding_t;

typedef struct _jl_module_t {
    // not first-class
    jl_sym_t *name;
    htable_t bindings;
    htable_t modules;
    arraylist_t imports;
} jl_module_t;

typedef struct _jl_value_pair_t {
    jl_value_t *a;
    jl_value_t *b;
    struct _jl_value_pair_t *next;
} jl_value_pair_t;

typedef struct _jl_methlist_t {
    jl_type_t *sig;
    jl_function_t *func;
    struct _jl_methlist_t *next;
} jl_methlist_t;

typedef struct _jl_methtable_t {
    jl_methlist_t *mlist;
    jl_methlist_t *generics;
} jl_methtable_t;

typedef struct {
    JL_VALUE_STRUCT
    jl_sym_t *head;
    jl_buffer_t *args;
} jl_expr_t;

extern jl_module_t *jl_system;
extern jl_module_t *jl_user;

extern jl_tag_type_t *jl_any_type;
extern jl_tag_type_t *jl_type_type;
extern jl_struct_type_t *jl_typename_type;
extern jl_struct_type_t *jl_sym_type;
extern jl_tag_type_t *jl_tuple_type;
extern jl_struct_type_t *jl_tvar_type;
extern jl_struct_type_t *jl_typector_type;

extern jl_struct_type_t *jl_func_kind;
extern jl_struct_type_t *jl_union_kind;
extern jl_struct_type_t *jl_tag_kind;
extern jl_struct_type_t *jl_struct_kind;
extern jl_struct_type_t *jl_bits_kind;

extern jl_type_t *jl_bottom_type;
extern jl_typector_t *jl_buffer_type;
extern jl_typector_t *jl_seq_type;
extern jl_typector_t *jl_tensor_type;
extern jl_tag_type_t *jl_scalar_type;
extern jl_tag_type_t *jl_number_type;
extern jl_tag_type_t *jl_real_type;
extern jl_tag_type_t *jl_int_type;
extern jl_tag_type_t *jl_float_type;

extern jl_bits_type_t *jl_bool_type;
extern jl_bits_type_t *jl_int8_type;
extern jl_bits_type_t *jl_uint8_type;
extern jl_bits_type_t *jl_int16_type;
extern jl_bits_type_t *jl_uint16_type;
extern jl_bits_type_t *jl_int32_type;
extern jl_bits_type_t *jl_uint32_type;
extern jl_bits_type_t *jl_int64_type;
extern jl_bits_type_t *jl_uint64_type;
extern jl_bits_type_t *jl_float32_type;
extern jl_bits_type_t *jl_float64_type;

extern jl_type_t *jl_buffer_uint8_type;
extern jl_type_t *jl_buffer_any_type;
extern jl_struct_type_t *jl_expr_type;

extern jl_tuple_t *jl_null;
extern jl_value_t *jl_true;
extern jl_value_t *jl_false;

extern jl_func_type_t *jl_any_func;

extern jl_function_t *jl_print_gf;

#ifdef BITS64
#define NWORDS(sz) (((sz)+7)>>3)
#else
#define NWORDS(sz) (((sz)+3)>>2)
#endif

#ifndef NO_BOEHM_GC
#define allocb(nb)    GC_MALLOC(nb)
#define alloc_pod(nb) GC_MALLOC_ATOMIC(nb)
#else
#define allocb(nb)    malloc(nb)
#define alloc_pod(nb) malloc(nb)
#endif

#define jl_tupleref(t,i) (((jl_value_t**)(t))[2+(i)])
#define jl_tupleset(t,i,x) ((((jl_value_t**)(t))[2+(i)])=(x))

#define jl_tparam0(t) jl_tupleref(((jl_tag_type_t*)(t))->parameters, 0)

#define jl_typeof(v) (((jl_value_t*)(v))->type)
#define jl_typeis(v,t) (jl_typeof(v)==(jl_type_t*)(t))

#define jl_is_null(v)        (((jl_value_t*)(v)) == ((jl_value_t*)jl_null))
#define jl_is_tuple(v)       jl_typeis(v,jl_tuple_type)
#define jl_is_tag_type(v)    jl_typeis(v,jl_tag_kind)
#define jl_is_bits_type(v)   jl_typeis(v,jl_bits_kind)
#define jl_is_struct_type(v) jl_typeis(v,jl_struct_kind)
#define jl_is_func_type(v)   jl_typeis(v,jl_func_kind)
#define jl_is_union_type(v)  jl_typeis(v,jl_union_kind)
#define jl_is_typevar(v)     jl_typeis(v,jl_tvar_type)
#define jl_is_typector(v)    jl_typeis(v,jl_typector_type)
#define jl_is_int32(v)       jl_typeis(v,jl_int32_type)
#define jl_is_bool(v)        jl_typeis(v,jl_bool_type)
#define jl_is_func(v)        (jl_is_func_type(jl_typeof(v)))
#define jl_is_gf(f)          (((jl_function_t*)(f))->fptr==jl_apply_generic)

#define jl_gf_mtable(f) ((jl_methtable_t*)(((jl_value_pair_t*)((jl_function_t*)(f))->env)->a))

// get a pointer to the data in a value of bits type
#define jl_bits_data(v) (&((void**)(v))[1])

static inline int jl_is_seq_type(jl_value_t *v)
{
    return (jl_is_tag_type(v) &&
            ((jl_tag_type_t*)jl_typeof(v))->name ==
            ((jl_tag_type_t*)jl_seq_type->body)->name);
}

// type info accessors
jl_typename_t *jl_tname(jl_value_t *v);
jl_tag_type_t *jl_tsuper(jl_value_t *v);
jl_tuple_t *jl_tparams(jl_value_t *v);
jl_value_t *jl_full_type(jl_value_t *v);

// type predicates
int jl_is_type(jl_value_t *v);
int jl_has_typevars(jl_value_t *v);
int jl_tuple_subtype(jl_value_t **child, size_t cl,
                     jl_value_t **parent, size_t pl, int ta, int tb);
int jl_subtype(jl_value_t *a, jl_value_t *b, int ta, int tb);
jl_value_pair_t *jl_type_conform(jl_type_t *a, jl_type_t *b);
int jl_types_equal(jl_value_t *a, jl_value_t *b);

// type constructors
jl_type_t *jl_apply_type_ctor(jl_typector_t *tc, jl_tuple_t *params);
jl_type_t *jl_instantiate_type_with(jl_type_t *t, jl_value_t **env, size_t n);
jl_uniontype_t *jl_new_uniontype(jl_tuple_t *types);
jl_func_type_t *jl_new_functype(jl_type_t *a, jl_type_t *b);

// constructors
jl_value_t *jl_new_struct(jl_struct_type_t *type, ...);
jl_function_t *jl_new_closure(jl_fptr_t proc, jl_value_t *env);
jl_tuple_t *jl_tuple(size_t n, ...);
jl_tuple_t *jl_alloc_tuple(size_t n);
jl_sym_t *jl_symbol(char *str);
jl_sym_t *jl_gensym();
jl_buffer_t *jl_new_buffer(jl_type_t *buf_type, size_t nel);
jl_buffer_t *jl_cstr_to_buffer(char *str);
jl_expr_t *jl_expr(jl_sym_t *head, size_t n, ...);
jl_expr_t *jl_exprn(jl_sym_t *head, size_t n);
jl_function_t *jl_new_generic_function(jl_sym_t *name);
void jl_add_method(jl_function_t *gf, jl_tuple_t *types, jl_function_t *meth);
jl_value_t *jl_box_int8(int8_t x);
jl_value_t *jl_box_uint8(uint8_t x);
jl_value_t *jl_box_int16(int16_t x);
jl_value_t *jl_box_uint16(uint16_t x);
jl_value_t *jl_box_int32(int32_t x);
jl_value_t *jl_box_uint32(uint32_t x);
jl_value_t *jl_box_int64(int64_t x);
jl_value_t *jl_box_uint64(uint64_t x);
jl_value_t *jl_box_float32(float x);
jl_value_t *jl_box_float64(double x);
int8_t jl_unbox_int8(jl_value_t *v);
uint8_t jl_unbox_uint8(jl_value_t *v);
int16_t jl_unbox_int16(jl_value_t *v);
uint16_t jl_unbox_uint16(jl_value_t *v);
int32_t jl_unbox_int32(jl_value_t *v);
uint32_t jl_unbox_uint32(jl_value_t *v);
int64_t jl_unbox_int64(jl_value_t *v);
uint64_t jl_unbox_uint64(jl_value_t *v);
float jl_unbox_float32(jl_value_t *v);
double jl_unbox_float64(jl_value_t *v);

// exceptions
void jl_error(char *str);
void jl_errorf(char *fmt, ...);
void jl_too_few_args(char *fname, int min);
void jl_too_many_args(char *fname, int max);

// initialization functions
void jl_init_types();
void jl_init_frontend();
void jl_shutdown_frontend();
void jl_init_builtins();
void jl_init_modules();

// parsing
jl_value_t *jl_parse_input_line(char *str);
jl_value_t *jl_parse_file(char *fname);

// some useful functions
void jl_print(jl_value_t *v);

// modules
extern jl_module_t *jl_system_module;
extern jl_module_t *jl_user_module;
jl_module_t *jl_new_module(jl_sym_t *name);
jl_binding_t *jl_get_binding(jl_module_t *m, jl_sym_t *var);
jl_binding_t *jl_add_binding(jl_module_t *m, jl_sym_t *var);
int jl_boundp(jl_module_t *m, jl_sym_t *var);
jl_module_t *jl_add_module(jl_module_t *m, jl_module_t *child);
jl_module_t *jl_get_module(jl_module_t *m, jl_sym_t *name);
jl_module_t *jl_import_module(jl_module_t *to, jl_module_t *from);

// for writing julia functions in C
#define JL_CALLABLE(name) \
    jl_value_t *name(jl_value_t *env, jl_value_t **args, uint32_t nargs)

static inline
jl_value_t *jl_apply(jl_function_t *f, jl_value_t **args, uint32_t nargs)
{
    return f->fptr(f->env, args, nargs);
}

JL_CALLABLE(jl_apply_generic);

#define JL_NARGS(fname, min, max)                               \
    if (nargs < min) jl_too_few_args(#fname, min);              \
    else if (nargs > max) jl_too_many_args(#fname, max);

#define JL_NARGSV(fname, min)                           \
    if (nargs < min) jl_too_few_args(#fname, min);

#endif
