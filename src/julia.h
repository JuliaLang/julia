#ifndef _JULIA_H_
#define _JULIA_H_

#include "libsupport.h"

#define JL_GC_MARKSWEEP

#include "htable.h"
#include "arraylist.h"
#include <setjmp.h>

#define JL_STRUCT_TYPE \
    struct _jl_type_t *type;

typedef struct _jl_value_t {
    JL_STRUCT_TYPE
} jl_value_t;

typedef struct _jl_sym_t {
    JL_STRUCT_TYPE
    struct _jl_sym_t *left;
    struct _jl_sym_t *right;
    uptrint_t hash;    // precomputed hash value
    union {
        char name[1];
        void *_pad;    // ensure field aligned to pointer size
    };
} jl_sym_t;

typedef struct {
    JL_STRUCT_TYPE
    size_t length;
    jl_value_t *data[1];
} jl_tuple_t;

// how much space we're willing to waste if an array outgrows its
// original object
#define ARRAY_INLINE_NBYTES (1024*sizeof(void*))

typedef struct {
    JL_STRUCT_TYPE
    void *data;
    size_t length;
    uint16_t ndims;
    uint16_t elsize;
    uint32_t offset;  // for 1-d only. does not need to get big.
    size_t nrows;
    union {
        struct {
            // 1d
            size_t maxsize;
        };
        struct {
            // Nd
            size_t ncols;
        };
    };
    union {
        char _space[1];
        void *_pad;
    };
} jl_array_t;

typedef struct _jl_type_t {
    JL_STRUCT_TYPE
} jl_type_t;

typedef jl_value_t *(*jl_fptr_t)(jl_value_t*, jl_value_t**, uint32_t);

typedef struct _jl_lambda_info_t {
    JL_STRUCT_TYPE
    // this holds the static data for a function:
    // a syntax tree, static parameters, and (if it has been compiled)
    // a function pointer.
    // this is the stuff that's shared among different instantiations
    // (different environments) of a closure.
    jl_value_t *ast;
    // sparams is a tuple (symbol, value, symbol, value, ...)
    jl_tuple_t *sparams;
    jl_value_t *tfunc;
    jl_sym_t *name;  // for error reporting
    jl_tuple_t *roots;  // pointers in generated code
    jl_value_t *specTypes;  // argument types this is specialized for
    // a slower-but-works version of this function as a fallback
    struct _jl_function_t *unspecialized;
    // pairlist of all lambda infos with code generated from this one
    jl_tuple_t *specializations;

    // hidden fields:
    jl_fptr_t fptr;
    void *functionObject;
    uptrint_t inferred;
    // flag telling if inference is running on this function
    // used to avoid infinite recursion
    uptrint_t inInference;
    uptrint_t inCompile;
} jl_lambda_info_t;

#define JL_FUNC_FIELDS                          \
    jl_fptr_t fptr;                             \
    jl_value_t *env;                            \
    jl_lambda_info_t *linfo;

typedef struct _jl_function_t {
    JL_STRUCT_TYPE
    JL_FUNC_FIELDS
} jl_function_t;

typedef struct {
    JL_STRUCT_TYPE
    jl_tuple_t *parameters;
    jl_type_t *body;
} jl_typector_t;

typedef struct {
    JL_STRUCT_TYPE
    jl_sym_t *name;
    // if this is the name of a parametric type, this field points to the
    // original type.
    // a type alias, for example, might make a type constructor that is
    // not the original.
    jl_value_t *primary;
    void *cache;
} jl_typename_t;

typedef struct {
    JL_STRUCT_TYPE
    jl_type_t *from;
    jl_type_t *to;
} jl_func_type_t;

typedef struct {
    JL_STRUCT_TYPE
    jl_tuple_t *types;
} jl_uniontype_t;

typedef struct _jl_tag_type_t {
    JL_STRUCT_TYPE
    JL_FUNC_FIELDS
    jl_typename_t *name;
    struct _jl_tag_type_t *super;
    jl_tuple_t *parameters;
} jl_tag_type_t;

typedef struct {
    JL_STRUCT_TYPE
    JL_FUNC_FIELDS
    jl_typename_t *name;
    jl_tag_type_t *super;
    jl_tuple_t *parameters;
    jl_tuple_t *names;
    jl_tuple_t *types;
    // hidden fields:
    uptrint_t uid;
    // to create a set of constructors for this sort of type
    jl_value_t *ctor_factory;
    jl_value_t *instance;  // for singletons
} jl_struct_type_t;

typedef struct {
    JL_STRUCT_TYPE
    JL_FUNC_FIELDS
    jl_typename_t *name;
    jl_tag_type_t *super;
    jl_tuple_t *parameters;
    jl_value_t *bnbits;
    // hidden fields:
    size_t nbits;
    uptrint_t uid;   // must be at same offset as in jl_struct_type_t
} jl_bits_type_t;

typedef struct {
    JL_STRUCT_TYPE
    jl_sym_t *name;
    jl_value_t *lb;   // lower bound
    jl_value_t *ub;   // upper bound
    uptrint_t bound;  // part of a constraint environment
} jl_tvar_t;

typedef struct {
    JL_STRUCT_TYPE
    jl_value_t *value;
} jl_weakref_t;

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
    htable_t macros;
    htable_t modules;
    arraylist_t imports;
} jl_module_t;

typedef struct _jl_methlist_t {
    // not first-class
    jl_tuple_t *sig;
    int has_tvars;
    int va;
    jl_tuple_t *tvars;
    jl_function_t *func;
    // cache of specializations of this method for invoke(), i.e.
    // cases where this method was called even though it was not necessarily
    // the most specific for the argument types.
    struct _jl_methtable_t *invokes;
    // TODO: pointer from specialized to original method
    //jl_function_t *orig_method;
    struct _jl_methlist_t *next;
} jl_methlist_t;

//#define JL_GF_PROFILE

typedef struct _jl_methtable_t {
    JL_STRUCT_TYPE
    jl_methlist_t *defs;
    jl_methlist_t *cache;
    jl_array_t *cache_1arg;
    int sealed;
    int max_args;  // max # of non-vararg arguments in a signature
#ifdef JL_GF_PROFILE
    int ncalls;
#endif
} jl_methtable_t;

typedef struct {
    JL_STRUCT_TYPE
    jl_sym_t *head;
    jl_array_t *args;
    jl_value_t *etype;
} jl_expr_t;

extern jl_tag_type_t *jl_any_type;
extern jl_tag_type_t *jl_type_type;
extern jl_tvar_t     *jl_typetype_tvar;
extern jl_tag_type_t *jl_typetype_type;
extern jl_value_t    *jl_ANY_flag;
extern jl_tag_type_t *jl_undef_type;
extern jl_struct_type_t *jl_typename_type;
extern jl_struct_type_t *jl_typector_type;
extern jl_struct_type_t *jl_sym_type;
extern jl_struct_type_t *jl_symbol_type;
extern jl_tuple_t *jl_tuple_type;
extern jl_tag_type_t *jl_ntuple_type;
extern jl_typename_t *jl_ntuple_typename;
extern jl_struct_type_t *jl_tvar_type;
extern jl_struct_type_t *jl_task_type;

extern DLLEXPORT jl_struct_type_t *jl_func_kind;
extern jl_struct_type_t *jl_union_kind;
extern jl_struct_type_t *jl_tag_kind;
extern jl_struct_type_t *jl_tag_type_type;
extern DLLEXPORT jl_struct_type_t *jl_struct_kind;
extern jl_struct_type_t *jl_bits_kind;

extern jl_type_t *jl_bottom_type;
extern jl_struct_type_t *jl_lambda_info_type;
extern jl_tag_type_t *jl_seq_type;
extern jl_typector_t *jl_function_type;
extern jl_tag_type_t *jl_abstractarray_type;
extern jl_struct_type_t *jl_array_type;
extern jl_typename_t *jl_array_typename;
extern jl_struct_type_t *jl_weakref_type;
extern jl_tag_type_t *jl_string_type;
extern jl_struct_type_t *jl_ascii_string_type;
extern jl_struct_type_t *jl_utf8_string_type;
extern jl_struct_type_t *jl_errorexception_type;
extern jl_struct_type_t *jl_typeerror_type;
extern jl_struct_type_t *jl_loaderror_type;
extern jl_struct_type_t *jl_uniontoocomplex_type;
extern jl_struct_type_t *jl_backtrace_type;
extern jl_value_t *jl_stackovf_exception;
extern jl_value_t *jl_memory_exception;
extern jl_value_t *jl_divbyzero_exception;
extern jl_value_t *jl_undefref_exception;
extern jl_value_t *jl_interrupt_exception;
extern jl_value_t *jl_an_empty_cell;

extern jl_struct_type_t *jl_box_type;
extern jl_type_t *jl_box_any_type;
extern jl_typename_t *jl_box_typename;

extern jl_bits_type_t *jl_bool_type;
extern jl_bits_type_t *jl_char_type;
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

extern jl_bits_type_t *jl_pointer_type;
extern jl_bits_type_t *jl_pointer_void_type;

extern jl_type_t *jl_array_uint8_type;
extern jl_type_t *jl_array_any_type;
extern DLLEXPORT jl_struct_type_t *jl_expr_type;
extern jl_struct_type_t *jl_symbolnode_type;
extern jl_struct_type_t *jl_linenumbernode_type;
extern jl_struct_type_t *jl_labelnode_type;
extern jl_bits_type_t *jl_intrinsic_type;
extern jl_struct_type_t *jl_methtable_type;
extern jl_struct_type_t *jl_task_type;

extern jl_tuple_t *jl_null;
extern jl_value_t *jl_true;
extern jl_value_t *jl_false;
DLLEXPORT extern jl_value_t *jl_nothing;

extern jl_func_type_t *jl_any_func;

extern jl_function_t *jl_show_gf;
extern jl_function_t *jl_convert_gf;
extern jl_function_t *jl_bottom_func;
extern DLLEXPORT jl_function_t *jl_memio_func;
extern jl_function_t *jl_append_any_func;
extern jl_function_t *jl_method_missing_func;
extern jl_function_t *jl_unprotect_stack_func;

extern void *jl_dl_handle;

// some important symbols
extern jl_sym_t *call_sym;
extern jl_sym_t *call1_sym;
extern jl_sym_t *dots_sym;
extern jl_sym_t *dollar_sym;
extern jl_sym_t *quote_sym;
extern jl_sym_t *top_sym;
extern jl_sym_t *line_sym;
extern jl_sym_t *multivalue_sym;
extern DLLEXPORT jl_sym_t *jl_continue_sym;
extern jl_sym_t *error_sym;
extern jl_sym_t *vinf_sym;
extern jl_sym_t *goto_sym;    extern jl_sym_t *goto_ifnot_sym;
extern jl_sym_t *label_sym;   extern jl_sym_t *return_sym;
extern jl_sym_t *lambda_sym;  extern jl_sym_t *assign_sym;
extern jl_sym_t *null_sym;    extern jl_sym_t *body_sym;
extern jl_sym_t *isbound_sym; extern jl_sym_t *macro_sym;
extern jl_sym_t *locals_sym;  extern jl_sym_t *colons_sym;
extern jl_sym_t *unexpanded_sym;
extern jl_sym_t *Any_sym;     extern jl_sym_t *method_sym;
extern jl_sym_t *enter_sym;   extern jl_sym_t *leave_sym;
extern jl_sym_t *exc_sym;     extern jl_sym_t *new_sym;
extern jl_sym_t *static_typeof_sym;

#ifdef __LP64__
#define NWORDS(sz) (((sz)+7)>>3)
#else
#define NWORDS(sz) (((sz)+3)>>2)
#endif

#ifdef JL_GC_MARKSWEEP
void *allocb(size_t sz);
void *allocobj(size_t sz);
#define alloc_pod(nb) allocb(nb)
void *allocb_permanent(size_t sz);
#else
#define allocb(nb)    malloc(nb)
#define allocobj(nb)  malloc(nb)
#define alloc_pod(nb) malloc(nb)
#define allocb_permanent(nb) malloc(nb)
#endif

#define jl_tupleref(t,i) (((jl_value_t**)(t))[2+(i)])
#define jl_tupleset(t,i,x) ((((jl_value_t**)(t))[2+(i)])=(x))
#define jl_t0(t) jl_tupleref(t,0)
#define jl_t1(t) jl_tupleref(t,1)
#define jl_t2(t) jl_tupleref(t,2)
#define jl_nextpair(p) jl_t2(p)

#define jl_cellref(a,i) (((jl_value_t**)((jl_array_t*)a)->data)[(i)])
#define jl_cellset(a,i,x) ((((jl_value_t**)((jl_array_t*)a)->data)[(i)])=((jl_value_t*)(x)))

#define jl_exprarg(e,n) jl_cellref(((jl_expr_t*)(e))->args,n)

#define jl_fieldref(s,i) (((jl_value_t**)(s))[1+(i)])

#define jl_symbolnode_sym(s) ((jl_sym_t*)jl_fieldref(s,0))
#define jl_symbolnode_type(s) (jl_fieldref(s,1))
#define jl_linenode_line(x) jl_unbox_long(jl_fieldref(x,0))
#define jl_labelnode_label(x) jl_unbox_long(jl_fieldref(x,0))

#define jl_tparam0(t) jl_tupleref(((jl_tag_type_t*)(t))->parameters, 0)
#define jl_tparam1(t) jl_tupleref(((jl_tag_type_t*)(t))->parameters, 1)

#define jl_typeof(v) (((jl_value_t*)(v))->type)
#define jl_typeis(v,t) (jl_typeof(v)==(jl_type_t*)(t))

#define jl_is_null(v)        (((jl_value_t*)(v)) == ((jl_value_t*)jl_null))
#define jl_is_tuple(v)       jl_typeis(v,jl_tuple_type)
#define jl_is_tag_type(v)    jl_typeis(v,jl_tag_kind)
#define jl_is_some_tag_type(v) (jl_is_tag_type(v)||jl_is_struct_type(v)||jl_is_bits_type(v))
#define jl_is_bits_type(v)   jl_typeis(v,jl_bits_kind)
#define jl_bitstype_nbits(t) (((jl_bits_type_t*)t)->nbits)
#define jl_is_struct_type(v) jl_typeis(v,jl_struct_kind)
#define jl_is_func_type(v)   jl_typeis(v,jl_func_kind)
#define jl_is_union_type(v)  jl_typeis(v,jl_union_kind)
#define jl_is_typevar(v)     jl_typeis(v,jl_tvar_type)
#define jl_is_typector(v)    jl_typeis(v,jl_typector_type)
#define jl_is_TypeConstructor(v)    jl_typeis(v,jl_typector_type)
#define jl_is_typename(v)    jl_typeis(v,jl_typename_type)
#define jl_is_int32(v)       jl_typeis(v,jl_int32_type)
#define jl_is_int64(v)       jl_typeis(v,jl_int64_type)
#define jl_is_uint32(v)      jl_typeis(v,jl_uint32_type)
#define jl_is_uint64(v)      jl_typeis(v,jl_uint64_type)
#define jl_is_float32(v)     jl_typeis(v,jl_float32_type)
#define jl_is_float64(v)     jl_typeis(v,jl_float64_type)
#define jl_is_bool(v)        jl_typeis(v,jl_bool_type)
#define jl_is_symbol(v)      jl_typeis(v,jl_sym_type)
#define jl_is_expr(v)        jl_typeis(v,jl_expr_type)
#define jl_is_symbolnode(v)  jl_typeis(v,jl_symbolnode_type)
#define jl_is_labelnode(v)   jl_typeis(v,jl_labelnode_type)
#define jl_is_linenode(v)    jl_typeis(v,jl_linenumbernode_type)
#define jl_is_lambda_info(v) jl_typeis(v,jl_lambda_info_type)
#define jl_is_mtable(v)      jl_typeis(v,jl_methtable_type)
#define jl_is_task(v)        jl_typeis(v,jl_task_type)
#define jl_is_func(v)        (jl_is_func_type(jl_typeof(v)) || jl_is_struct_type(v))
#define jl_is_function(v)    jl_is_func(v)
#define jl_is_ascii_string(v) jl_typeis(v,jl_ascii_string_type)
#define jl_is_utf8_string(v) jl_typeis(v,jl_utf8_string_type)
#define jl_is_byte_string(v) (jl_is_ascii_string(v) || jl_is_utf8_string(v))
#define jl_is_string(v)      jl_subtype(v,(jl_value_t*)jl_string_type,1)
#define jl_is_cpointer(v)    jl_is_cpointer_type(jl_typeof(v))
#define jl_is_pointer(v)     jl_is_cpointer_type(jl_typeof(v))
#define jl_is_gf(f)          (((jl_function_t*)(f))->fptr==jl_apply_generic)

#define jl_array_len(a)   (((jl_array_t*)(a))->length)
#define jl_array_data(a)  ((void*)((jl_array_t*)(a))->data)
#define jl_array_dim(a,i) ((&((jl_array_t*)(a))->nrows)[i])
#define jl_array_ndims(a) ((int32_t)(((jl_array_t*)a)->ndims))
#define jl_cell_data(a)   ((jl_value_t**)((jl_array_t*)a)->data)
#define jl_string_data(s) ((char*)((jl_array_t*)((jl_value_t**)(s))[1])->data)

#define jl_gf_mtable(f) ((jl_methtable_t*)jl_t0(((jl_function_t*)(f))->env))
#define jl_gf_name(f) ((jl_sym_t*)jl_t1(((jl_function_t*)(f))->env))

// get a pointer to the data in a value of bits type
#define jl_bits_data(v) (&((void**)(v))[1])

static inline int jl_is_array_type(void *t)
{
    return (jl_is_struct_type(t) &&
            ((jl_struct_type_t*)(t))->name == jl_array_typename);
}

static inline int jl_is_array(void *v)
{
    jl_type_t *t = jl_typeof(v);
    return jl_is_array_type(t);
}

static inline int jl_is_box(void *v)
{
    jl_type_t *t = jl_typeof(v);
    return (jl_is_struct_type(t) &&
            ((jl_struct_type_t*)(t))->name == jl_box_typename);
}

static inline int jl_is_cpointer_type(void *t)
{
    return (jl_is_bits_type(t) &&
            ((jl_bits_type_t*)(t))->name == jl_pointer_void_type->name);
}

static inline int jl_is_seq_type(jl_value_t *v)
{
    return (jl_is_tag_type(v) &&
            ((jl_tag_type_t*)(v))->name == jl_seq_type->name);
}

static inline int jl_is_ntuple_type(jl_value_t *v)
{
    return (jl_is_tag_type(v) &&
            ((jl_tag_type_t*)v)->name == jl_ntuple_typename);
}

static inline int jl_is_nontuple_type(jl_value_t *v)
{
    return (jl_typeis(v, jl_union_kind) ||
            jl_typeis(v, jl_struct_kind) ||
            jl_typeis(v, jl_func_kind) ||
            jl_typeis(v, jl_tag_kind) ||
            jl_typeis(v, jl_bits_kind));
}

// type info accessors
jl_value_t *jl_full_type(jl_value_t *v);
size_t jl_field_offset(jl_struct_type_t *t, jl_sym_t *fld);

// type predicates
int jl_is_type(jl_value_t *v);
DLLEXPORT int jl_is_leaf_type(jl_value_t *v);
int jl_has_typevars(jl_value_t *v);
int jl_tuple_subtype(jl_value_t **child, size_t cl,
                     jl_value_t **parent, size_t pl, int ta, int morespecific);
int jl_subtype(jl_value_t *a, jl_value_t *b, int ta);
int jl_type_morespecific(jl_value_t *a, jl_value_t *b, int ta);
DLLEXPORT jl_value_t *jl_type_match(jl_value_t *a, jl_value_t *b);
jl_value_t *jl_type_match_invariant(jl_value_t *a, jl_value_t *b);
jl_value_t *jl_type_match_morespecific(jl_value_t *a, jl_value_t *b);
DLLEXPORT int jl_types_equal(jl_value_t *a, jl_value_t *b);
int jl_types_equal_generic(jl_value_t *a, jl_value_t *b);
jl_value_t *jl_type_union(jl_tuple_t *types);
jl_value_t *jl_type_intersection_matching(jl_value_t *a, jl_value_t *b,
                                          jl_tuple_t **penv, jl_tuple_t *tvars);
DLLEXPORT jl_value_t *jl_type_intersection(jl_value_t *a, jl_value_t *b);

// type constructors
jl_typename_t *jl_new_typename(jl_sym_t *name);
jl_tvar_t *jl_new_typevar(jl_sym_t *name,jl_value_t *lb,jl_value_t *ub);
jl_typector_t *jl_new_type_ctor(jl_tuple_t *params, jl_type_t *body);
jl_value_t *jl_apply_type(jl_value_t *tc, jl_tuple_t *params);
jl_type_t *jl_instantiate_type_with(jl_type_t *t, jl_value_t **env, size_t n);
jl_uniontype_t *jl_new_uniontype(jl_tuple_t *types);
jl_func_type_t *jl_new_functype(jl_type_t *a, jl_type_t *b);
jl_tag_type_t *jl_new_tagtype(jl_value_t *name, jl_tag_type_t *super,
                              jl_tuple_t *parameters);
jl_struct_type_t *jl_new_struct_type(jl_sym_t *name, jl_tag_type_t *super,
                                     jl_tuple_t *parameters,
                                     jl_tuple_t *fnames, jl_tuple_t *ftypes);
jl_bits_type_t *jl_new_bitstype(jl_value_t *name, jl_tag_type_t *super,
                                jl_tuple_t *parameters, size_t nbits);
jl_tag_type_t *jl_wrap_Type(jl_value_t *t);  // x -> Type{x}

// constructors
DLLEXPORT jl_value_t *jl_new_struct(jl_struct_type_t *type, ...);
DLLEXPORT jl_value_t *jl_new_struct_uninit(jl_struct_type_t *type);
DLLEXPORT jl_value_t *jl_new_structt(jl_struct_type_t *type, jl_tuple_t *t);
jl_function_t *jl_new_closure(jl_fptr_t proc, jl_value_t *env);
jl_lambda_info_t *jl_new_lambda_info(jl_value_t *ast, jl_tuple_t *sparams);
jl_tuple_t *jl_tuple(size_t n, ...);
jl_tuple_t *jl_tuple1(void *a);
jl_tuple_t *jl_tuple2(void *a, void *b);
jl_tuple_t *jl_tuple3(void *a, void *b, void *c);
jl_tuple_t *jl_alloc_tuple(size_t n);
jl_tuple_t *jl_alloc_tuple_uninit(size_t n);
jl_tuple_t *jl_tuple_append(jl_tuple_t *a, jl_tuple_t *b);
jl_tuple_t *jl_tuple_fill(size_t n, jl_value_t *v);
jl_tuple_t *jl_flatten_pairs(jl_tuple_t *t);
DLLEXPORT jl_sym_t *jl_symbol(const char *str);
DLLEXPORT jl_sym_t *jl_symbol_n(const char *str, int32_t len);
DLLEXPORT jl_sym_t *jl_gensym();
jl_expr_t *jl_exprn(jl_sym_t *head, size_t n);
jl_function_t *jl_new_generic_function(jl_sym_t *name);
void jl_initialize_generic_function(jl_function_t *f, jl_sym_t *name);
void jl_add_method(jl_function_t *gf, jl_tuple_t *types, jl_function_t *meth);
jl_value_t *jl_method_def(jl_sym_t *name, jl_value_t **bp,
                          jl_tuple_t *argtypes, jl_function_t *f);
jl_value_t *jl_box_bool(int8_t x);
jl_value_t *jl_box_int8(int8_t x);
jl_value_t *jl_box_uint8(uint8_t x);
jl_value_t *jl_box_int16(int16_t x);
jl_value_t *jl_box_uint16(uint16_t x);
DLLEXPORT jl_value_t *jl_box_int32(int32_t x);
jl_value_t *jl_box_uint32(uint32_t x);
jl_value_t *jl_new_box_int8(int8_t x);
jl_value_t *jl_new_box_int32(int32_t x);
jl_value_t *jl_box_int64(int64_t x);
jl_value_t *jl_box_uint64(uint64_t x);
jl_value_t *jl_box_float32(float x);
jl_value_t *jl_box_float64(double x);
jl_value_t *jl_box8 (jl_bits_type_t *t, int8_t  x);
jl_value_t *jl_box16(jl_bits_type_t *t, int16_t x);
jl_value_t *jl_box32(jl_bits_type_t *t, int32_t x);
jl_value_t *jl_box64(jl_bits_type_t *t, int64_t x);
int8_t jl_unbox_bool(jl_value_t *v);
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
jl_value_t *jl_box_pointer(jl_bits_type_t *ty, void *p);
void *jl_unbox_pointer(jl_value_t *v);

#ifdef __LP64__
#define jl_box_long(x)   jl_box_int64(x)
#define jl_unbox_long(x) jl_unbox_int64(x)
#define jl_is_long(x)    jl_is_int64(x)
#define jl_long_type     jl_int64_type
#else
#define jl_box_long(x)   jl_box_int32(x)
#define jl_unbox_long(x) jl_unbox_int32(x)
#define jl_is_long(x)    jl_is_int32(x)
#define jl_long_type     jl_int32_type
#endif

// arrays
DLLEXPORT jl_array_t *jl_new_array(jl_type_t *atype, jl_tuple_t *dims);
jl_array_t *jl_new_array_(jl_type_t *atype, uint32_t ndims, size_t *dims);
DLLEXPORT jl_array_t *jl_alloc_array_1d(jl_type_t *atype, size_t nr);
DLLEXPORT jl_array_t *jl_alloc_array_2d(jl_type_t *atype, size_t nr, size_t nc);
DLLEXPORT jl_array_t *jl_pchar_to_array(char *str, size_t len);
DLLEXPORT jl_array_t *jl_cstr_to_array(char *str);
DLLEXPORT jl_value_t *jl_pchar_to_string(char *str, size_t len);
DLLEXPORT jl_value_t *jl_cstr_to_string(char *str);
DLLEXPORT jl_value_t *jl_array_to_string(jl_array_t *a);
DLLEXPORT jl_array_t *jl_alloc_cell_1d(size_t n);
DLLEXPORT jl_value_t *jl_arrayref(jl_array_t *a, size_t i);  // 0-indexed
DLLEXPORT void jl_arrayset(jl_array_t *a, size_t i, jl_value_t *v);  // 0-indexed
DLLEXPORT void *jl_array_ptr(jl_array_t *a);
DLLEXPORT void jl_array_grow_end(jl_array_t *a, size_t inc);
DLLEXPORT void jl_array_del_end(jl_array_t *a, size_t dec);
DLLEXPORT void jl_array_grow_beg(jl_array_t *a, size_t inc);
DLLEXPORT void jl_array_del_beg(jl_array_t *a, size_t dec);

// system information
DLLEXPORT int jl_errno();
DLLEXPORT jl_value_t *jl_strerror(int errnum);

// child process status
DLLEXPORT int jl_process_exited(int status);
DLLEXPORT int jl_process_signaled(int status);
DLLEXPORT int jl_process_stopped(int status);

DLLEXPORT int jl_process_exit_status(int status);
DLLEXPORT int jl_process_term_signal(int status);
DLLEXPORT int jl_process_stop_signal(int status);

// access to std filehandles
DLLEXPORT int jl_stdin();
DLLEXPORT int jl_stdout();
DLLEXPORT int jl_stderr();

// exceptions
void jl_error(const char *str);
void jl_errorf(const char *fmt, ...);
void jl_too_few_args(const char *fname, int min);
void jl_too_many_args(const char *fname, int max);
void jl_type_error(const char *fname, jl_value_t *expected, jl_value_t *got);
void jl_type_error_rt(const char *fname, const char *context,
                      jl_value_t *ty, jl_value_t *got);
jl_value_t *jl_no_method_error(jl_function_t *f, jl_value_t **args, size_t na);
void jl_undef_ref_error();
void jl_divide_by_zero_error();

// initialization functions
DLLEXPORT void julia_init(char *imageFile);
DLLEXPORT
int julia_trampoline(int argc, char *argv[], int (*pmain)(int ac,char *av[]));
void jl_init_types();
void jl_init_box_caches();
void jl_init_frontend();
void jl_shutdown_frontend();
void jl_init_primitives();
void jl_init_builtins();
void jl_init_modules();
void jl_init_codegen();
void jl_init_intrinsic_functions();
void jl_init_tasks(void *stack, size_t ssize);
void jl_init_serializer();
void jl_load_boot_j();

void jl_save_system_image(char *fname, char *startscriptname);
void jl_restore_system_image(char *fname);

// front end interface
DLLEXPORT jl_value_t *jl_parse_input_line(const char *str);
jl_value_t *jl_parse_file(const char *fname);
jl_value_t *jl_parse_file_string(const char *text);
jl_value_t *jl_expand(jl_value_t *expr);
jl_lambda_info_t *jl_wrap_expr(jl_value_t *expr);

// some useful functions
DLLEXPORT void jl_show(jl_value_t *v);
jl_value_t *jl_convert(jl_type_t *to, jl_value_t *x);

// modules
extern DLLEXPORT jl_module_t *jl_system_module;
extern DLLEXPORT jl_module_t *jl_user_module;
jl_module_t *jl_new_module(jl_sym_t *name);
jl_binding_t *jl_get_binding(jl_module_t *m, jl_sym_t *var);
jl_value_t **jl_get_bindingp(jl_module_t *m, jl_sym_t *var);
int jl_boundp(jl_module_t *m, jl_sym_t *var);
DLLEXPORT jl_value_t *jl_get_global(jl_module_t *m, jl_sym_t *var);
DLLEXPORT void jl_set_global(jl_module_t *m, jl_sym_t *var, jl_value_t *val);
DLLEXPORT void jl_set_const(jl_module_t *m, jl_sym_t *var, jl_value_t *val);
jl_module_t *jl_add_module(jl_module_t *m, jl_module_t *child);
jl_module_t *jl_get_module(jl_module_t *m, jl_sym_t *name);
jl_module_t *jl_import_module(jl_module_t *to, jl_module_t *from);
jl_function_t *jl_get_expander(jl_module_t *m, jl_sym_t *macroname);
void jl_set_expander(jl_module_t *m, jl_sym_t *macroname, jl_function_t *f);

// external libraries
DLLEXPORT void *jl_load_dynamic_library(char *fname);
DLLEXPORT void *jl_dlsym(void *handle, char *symbol);

// compiler
void jl_compile(jl_function_t *f);
void jl_generate_fptr(jl_function_t *f);
DLLEXPORT jl_value_t *jl_toplevel_eval(jl_value_t *v);
char *jl_find_file_in_path(const char *fname);
DLLEXPORT void jl_load(const char *fname);
void jl_load_file_expr(char *fname, jl_value_t *ast);
jl_value_t *jl_interpret_toplevel_thunk(jl_lambda_info_t *lam);
jl_value_t *jl_interpret_toplevel_expr(jl_value_t *e);
jl_value_t *jl_interpret_toplevel_expr_with(jl_value_t *e,
                                            jl_value_t **locals, size_t nl);

void jl_show_method_table(jl_function_t *gf);
DLLEXPORT void jl_show_full_function(jl_value_t *v);
jl_lambda_info_t *jl_add_static_parameters(jl_lambda_info_t *l, jl_tuple_t *sp);
jl_function_t *jl_method_lookup_by_type(jl_methtable_t *mt, jl_tuple_t *types,
                                        int cache);
jl_function_t *jl_method_lookup(jl_methtable_t *mt, jl_value_t **args, size_t nargs, int cache);
jl_value_t *jl_gf_invoke(jl_function_t *gf, jl_tuple_t *types,
                         jl_value_t **args, size_t nargs);

// AST access
jl_array_t *jl_lam_args(jl_expr_t *l);
jl_array_t *jl_lam_locals(jl_expr_t *l);
jl_array_t *jl_lam_vinfo(jl_expr_t *l);
jl_array_t *jl_lam_capt(jl_expr_t *l);
jl_expr_t *jl_lam_body(jl_expr_t *l);
jl_sym_t *jl_decl_var(jl_value_t *ex);
DLLEXPORT int jl_is_rest_arg(jl_value_t *ex);

// for writing julia functions in C
#define JL_CALLABLE(name) \
    jl_value_t *name(jl_value_t *env, jl_value_t **args, uint32_t nargs)

static inline
jl_value_t *jl_apply(jl_function_t *f, jl_value_t **args, uint32_t nargs)
{
    return f->fptr(f->env, args, nargs);
}

void jl_add_builtin(const char *name, jl_value_t *v);
void jl_add_builtin_func(const char *name, jl_fptr_t f);

JL_CALLABLE(jl_f_no_function);
JL_CALLABLE(jl_f_tuple);
JL_CALLABLE(jl_f_arrayset);
DLLEXPORT JL_CALLABLE(jl_apply_generic);

#define JL_NARGS(fname, min, max)                               \
    if (nargs < min) jl_too_few_args(#fname, min);              \
    else if (nargs > max) jl_too_many_args(#fname, max);

#define JL_NARGSV(fname, min)                           \
    if (nargs < min) jl_too_few_args(#fname, min);

#define JL_TYPECHK(fname, type, v)                                      \
    if (!jl_is_##type(v)) {                                             \
        jl_type_error(#fname, (jl_value_t*)jl_##type##_type, (v));      \
    }

// gc

#ifdef JL_GC_MARKSWEEP
typedef struct _jl_gcframe_t {
    jl_value_t ***roots;
    size_t nroots;
    int indirect;
    struct _jl_gcframe_t *prev;
} jl_gcframe_t;

// NOTE: it is the caller's responsibility to make sure arguments are
// rooted. foo(f(), g()) will not work, and foo can't do anything about it,
// so the caller must do
// jl_value_t *x, *y; JL_GC_PUSH(&x, &y);
// x = f(); y = g(); foo(x, y)

extern DLLEXPORT jl_gcframe_t ** volatile jl_pgcstack;

#define JL_GC_PUSH(...)                                                 \
  void *__gc_rts[] = {__VA_ARGS__};                                     \
  jl_gcframe_t __gc_stkf_ = { (jl_value_t***)__gc_rts, VA_NARG(__VA_ARGS__), \
                              1, *jl_pgcstack };                        \
  *jl_pgcstack = &__gc_stkf_;

#define JL_GC_PUSHARGS(rts,n)                           \
  jl_gcframe_t __gc_stkf_ = { (jl_value_t***)rts, (n),  \
                              0, *jl_pgcstack };        \
  *jl_pgcstack = &__gc_stkf_;

#define JL_GC_POP() \
    (*jl_pgcstack = __gc_stkf_.prev)

void jl_gc_init();
void jl_gc_markval(jl_value_t *v);
void jl_gc_enable();
void jl_gc_disable();
int jl_gc_is_enabled();
DLLEXPORT void jl_gc_collect();
void jl_gc_preserve(jl_value_t *v);
void jl_gc_unpreserve();
int jl_gc_n_preserved_values();
DLLEXPORT void jl_gc_add_finalizer(jl_value_t *v, jl_function_t *f);
jl_weakref_t *jl_gc_new_weakref(jl_value_t *value);
#define jl_gc_setmark(v) (((uptrint_t*)(v))[-1]|=1)
void jl_gc_acquire_buffer(void *b);
void *alloc_2w();
void *alloc_3w();
void *alloc_4w();

#else

#define JL_GC_PUSH(...) ;
#define JL_GC_PUSHARGS(rts,n) ;
#define JL_GC_POP()

#define jl_gc_preserve(v) ((void)(v))
#define jl_gc_unpreserve()
#define jl_gc_n_preserved_values() (0)
#endif

// asynch signal handling

#include <signal.h>

DLLEXPORT extern volatile sig_atomic_t jl_signal_pending;
DLLEXPORT extern volatile sig_atomic_t jl_defer_signal;

#define JL_SIGATOMIC_BEGIN() (jl_defer_signal++)
#define JL_SIGATOMIC_END()                                      \
    do {                                                        \
        jl_defer_signal--;                                      \
        if (jl_defer_signal == 0 && jl_signal_pending != 0)     \
            raise(jl_signal_pending);                           \
    } while(0)

// tasks and exceptions

// context that needs to be restored around a try block
typedef struct _jl_savestate_t {
    // eh_task is who I yield to for exception handling
    struct _jl_task_t *eh_task;
    // eh_ctx is where I go to handle an exception yielded to me
    jmp_buf *eh_ctx;
    ptrint_t err;
    jl_value_t *ostream_obj;
    ios_t *current_output_stream;
#ifdef JL_GC_MARKSWEEP
    jl_gcframe_t *gcstack;
#endif
    struct _jl_savestate_t *prev;
} jl_savestate_t;

typedef struct _jl_task_t {
    JL_STRUCT_TYPE
    struct _jl_task_t *on_exit;
    jmp_buf ctx;
    union {
        void *stackbase;
        void *stack;
    };
    jmp_buf base_ctx;
    size_t bufsz;
    void *stkbuf;
    size_t ssize;
    jl_function_t *start;
    int done;
    jl_value_t *result;
    // exception state and per-task dynamic parameters
    jl_savestate_t state;
} jl_task_t;

extern DLLEXPORT jl_task_t * volatile jl_current_task;
extern DLLEXPORT jl_task_t *jl_root_task;
extern DLLEXPORT jl_value_t *jl_exception_in_transit;

jl_task_t *jl_new_task(jl_function_t *start, size_t ssize);
jl_value_t *jl_switchto(jl_task_t *t, jl_value_t *arg);
DLLEXPORT void jl_raise(jl_value_t *e);
DLLEXPORT void jl_register_toplevel_eh();

DLLEXPORT jl_value_t *jl_current_output_stream_obj();
DLLEXPORT ios_t *jl_current_output_stream();
DLLEXPORT void jl_set_current_output_stream_obj(jl_value_t *v);

DLLEXPORT jl_array_t *jl_takebuf_array(ios_t *s);
DLLEXPORT jl_value_t *jl_takebuf_string(ios_t *s);
DLLEXPORT jl_array_t *jl_readuntil(ios_t *s, uint8_t delim);

static inline void jl_eh_restore_state(jl_savestate_t *ss)
{
    JL_SIGATOMIC_BEGIN();
    jl_current_task->state.eh_task = ss->eh_task;
    jl_current_task->state.eh_ctx = ss->eh_ctx;
    jl_current_task->state.ostream_obj = ss->ostream_obj;
    jl_current_task->state.current_output_stream = ss->current_output_stream;
    jl_current_task->state.prev = ss->prev;
#ifdef JL_GC_MARKSWEEP
    jl_current_task->state.gcstack = ss->gcstack;
#endif
    JL_SIGATOMIC_END();
}

DLLEXPORT void jl_enter_handler(jl_savestate_t *ss, jmp_buf *handlr);
DLLEXPORT void jl_pop_handler(int n);

#define JL_TRY                                                          \
    int i__tr, i__ca; jl_savestate_t __ss; jmp_buf __handlr;            \
    jl_enter_handler(&__ss, &__handlr);                                 \
    if (!setjmp(__handlr))                                              \
        for (i__tr=1; i__tr; i__tr=0, jl_eh_restore_state(&__ss))

#define JL_EH_POP() jl_eh_restore_state(&__ss)

#define JL_CATCH                                                \
    else                                                        \
        for (i__ca=1, jl_current_task->state.err = 0,           \
             jl_eh_restore_state(&__ss); i__ca; i__ca=0)

#endif
