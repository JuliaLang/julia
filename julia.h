#ifndef _JULIA_H_
#define _JULIA_H_

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
} jl_typename_t;

typedef struct _jl_type_t {
    JL_VALUE_STRUCT
    jl_typename_t *name;
    struct _jl_type_t *super;
    jl_tuple_t *parameters;
    jl_tuple_t *fields;
    // hidden fields:
    ptrint_t abstract;
    ptrint_t generic;
    size_t nw;      // # of words
    numerictype_t numtype;
    size_t nbytes;  // # of bytes for unboxed, plain-old-data types
    uptrint_t uid;
} jl_type_t;

typedef struct {
    JL_VALUE_STRUCT
    jl_sym_t *name;
    jl_type_t *lb;  // lower bound
    jl_type_t *ub;  // upper bound
} jl_tvar_t;

typedef jl_value_t *(*jl_fptr_t)(jl_value_t*, jl_value_t**, uint32_t);

typedef struct {
    JL_VALUE_STRUCT
    jl_fptr_t fptr;
    jl_value_t *env;
} jl_function_t;

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
    int nimports;
    struct _jl_module_t **imports;
} jl_module_t;

extern jl_module_t *jl_system;
extern jl_module_t *jl_user;

extern jl_type_t *jl_any_type;
extern jl_type_t *jl_type_type;
extern jl_type_t *jl_typename_type;
extern jl_type_t *jl_sym_type;
extern jl_type_t *jl_tuple_type;
extern jl_type_t *jl_tvar_type;

extern jl_type_t *jl_buffer_type;
extern jl_type_t *jl_function_type;
extern jl_type_t *jl_tensor_type;
extern jl_type_t *jl_seq_type;
extern jl_type_t *jl_union_type;
extern jl_type_t *jl_bottom_type;
extern jl_type_t *jl_scalar_type;
extern jl_type_t *jl_number_type;
extern jl_type_t *jl_real_type;
extern jl_type_t *jl_int_type;
extern jl_type_t *jl_float_type;

extern jl_type_t *jl_bool_type;
extern jl_type_t *jl_int8_type;
extern jl_type_t *jl_uint8_type;
extern jl_type_t *jl_int16_type;
extern jl_type_t *jl_uint16_type;
extern jl_type_t *jl_int32_type;
extern jl_type_t *jl_uint32_type;
extern jl_type_t *jl_int64_type;
extern jl_type_t *jl_uint64_type;
extern jl_type_t *jl_float32_type;
extern jl_type_t *jl_float64_type;

extern jl_value_t *jl_null;
extern jl_value_t *jl_true;
extern jl_value_t *jl_false;

#define JL_CALLABLE(name) \
    jl_value_t *name(jl_value_t *clo, jl_value_t **args, uint32_t nargs)

#endif
