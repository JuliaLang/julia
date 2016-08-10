// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef JULIA_H
#define JULIA_H

//** Configuration options that affect the Julia ABI **//
// if this is not defined, only individual dimension sizes are
// stored and not total length, to save space.
#define STORE_ARRAY_LEN
//** End Configuration options **//

#include "libsupport.h"
#include <stdint.h>
#include <string.h>

#include "htable.h"
#include "arraylist.h"

#include <setjmp.h>
#ifndef _OS_WINDOWS_
#  define jl_jmp_buf sigjmp_buf
#  if defined(_CPU_ARM_) || defined(_CPU_PPC_)
#    define MAX_ALIGN 8
#  elif defined(_CPU_AARCH64_)
// int128 is 16 bytes aligned on aarch64
#    define MAX_ALIGN 16
#  else
#    define MAX_ALIGN sizeof(void*)
#  endif
#else
#  define jl_jmp_buf jmp_buf
#  include <malloc.h> //for _resetstkoflw
#  define MAX_ALIGN 8
#endif

#ifdef _P64
#define NWORDS(sz) (((sz)+7)>>3)
#else
#define NWORDS(sz) (((sz)+3)>>2)
#endif

#if defined(__GNUC__)
#  define JL_NORETURN __attribute__ ((noreturn))
#  define JL_CONST_FUNC __attribute__((const))
#elif defined(_COMPILER_MICROSOFT_)
#  define JL_NORETURN __declspec(noreturn)
// This is the closest I can find for __attribute__((const))
#  define JL_CONST_FUNC __declspec(noalias)
#else
#  define JL_NORETURN
#  define JL_CONST_FUNC
#endif

#define container_of(ptr, type, member) \
    ((type *) ((char *)(ptr) - offsetof(type, member)))

typedef struct _jl_taggedvalue_t jl_taggedvalue_t;

#include <julia_threads.h>

#ifdef __cplusplus
extern "C" {
#endif

// core data types ------------------------------------------------------------

// the common fields are hidden before the pointer, but the following macro is
// used to indicate which types below are subtypes of jl_value_t
#define JL_DATA_TYPE

typedef struct _jl_value_t jl_value_t;

struct _jl_taggedvalue_bits {
    uintptr_t gc:2;
};

struct _jl_taggedvalue_t {
    union {
        uintptr_t header;
        jl_taggedvalue_t *next;
        jl_value_t *type; // 16-byte aligned
        struct _jl_taggedvalue_bits bits;
    };
    // jl_value_t value;
};

#define jl_astaggedvalue(v)                                             \
    ((jl_taggedvalue_t*)((char*)(v) - sizeof(jl_taggedvalue_t)))
#define jl_valueof(v)                                           \
    ((jl_value_t*)((char*)(v) + sizeof(jl_taggedvalue_t)))
#define jl_typeof(v)                                                    \
    ((jl_value_t*)(jl_astaggedvalue(v)->header & ~(uintptr_t)15))
static inline void jl_set_typeof(void *v, void *t)
{
    // Do not call this on a value that is already initialized.
    jl_taggedvalue_t *tag = jl_astaggedvalue(v);
    tag->type = (jl_value_t*)t;
}
#define jl_typeis(v,t) (jl_typeof(v)==(jl_value_t*)(t))

// Symbols are interned strings (hash-consed) stored as an invasive binary tree.
// The string data is nul-terminated and hangs off the end of the struct.
typedef struct _jl_sym_t {
    JL_DATA_TYPE
    struct _jl_sym_t *left;
    struct _jl_sym_t *right;
    uintptr_t hash;    // precomputed hash value
    // JL_ATTRIBUTE_ALIGN_PTRSIZE(char name[]);
} jl_sym_t;

// A numbered SSA value, for optimized code analysis and generation
// the `id` is a unique, small number
typedef struct _jl_ssavalue_t {
    JL_DATA_TYPE
    ssize_t id;
} jl_ssavalue_t;

// A SimpleVector is an immutable pointer array
// Data is stored at the end of this variable-length struct.
typedef struct {
    JL_DATA_TYPE
    size_t length;
    // pointer size aligned
    // jl_value_t *data[];
} jl_svec_t;

typedef struct {
    /*
      how - allocation style
      0 = data is inlined, or a foreign pointer we don't manage
      1 = julia-allocated buffer that needs to be marked
      2 = malloc-allocated pointer this array object manages
      3 = has a pointer to the Array that owns the data
    */
    uint16_t how:2;
    uint16_t ndims:10;
    uint16_t pooled:1;
    uint16_t ptrarray:1;  // representation is pointer array
    uint16_t isshared:1;  // data is shared by multiple Arrays
    uint16_t isaligned:1; // data allocated with memalign
} jl_array_flags_t;

typedef struct {
    JL_DATA_TYPE
    void *data;
#ifdef STORE_ARRAY_LEN
    size_t length;
#endif
    jl_array_flags_t flags;
    uint16_t elsize;
    uint32_t offset;  // for 1-d only. does not need to get big.
    size_t nrows;
    union {
        // 1d
        size_t maxsize;
        // Nd
        size_t ncols;
    };
    // other dim sizes go here for ndims > 2

    // followed by alignment padding and inline data, or owner pointer
} jl_array_t;

// compute # of extra words needed to store dimensions
STATIC_INLINE int jl_array_ndimwords(uint32_t ndims)
{
    return (ndims < 3 ? 0 : ndims-2);
}

typedef struct _jl_datatype_t jl_tupletype_t;
struct _jl_lambda_info_t;

// TypeMap is an implicitly defined type
// that can consist of any of the following nodes:
//   typedef TypeMap Union{TypeMapLevel, TypeMapEntry, Void}
// it forms a roughly tree-shaped structure, consisting of nodes of TypeMapLevels
// which split the tree when possible, for example based on the key into the tuple type at `offs`
// when key is a leaftype, (but only when the tree has enough entries for this to be
// more efficient than storing them sorted linearly)
// otherwise the leaf entries are stored sorted, linearly
union jl_typemap_t {
    struct _jl_typemap_level_t *node;
    struct _jl_typemap_entry_t *leaf;
    struct _jl_value_t *unknown; // nothing
};

// "jlcall" calling convention signatures.
// This defines the default ABI used by compiled julia functions.
typedef jl_value_t *(*jl_fptr_t)(jl_value_t*, jl_value_t**, uint32_t);
typedef jl_value_t *(*jl_fptr_sparam_t)(jl_svec_t*, jl_value_t*, jl_value_t**, uint32_t);
typedef jl_value_t *(*jl_fptr_linfo_t)(struct _jl_lambda_info_t*, jl_value_t**, uint32_t, jl_svec_t*);

typedef struct _jl_llvm_functions_t {
    void *functionObject;     // jlcall llvm Function
    void *specFunctionObject; // specialized llvm Function
} jl_llvm_functions_t;

// This type describes a single method definition, and stores data
// shared by the specializations of a function.
typedef struct _jl_method_t {
    JL_DATA_TYPE
    jl_sym_t *name;  // for error reporting
    struct _jl_module_t *module;
    jl_sym_t *file;
    int32_t line;

    // method's type signature. partly redundant with lambda_template->specTypes
    jl_tupletype_t *sig;
    // bound type variables (static parameters). redundant with TypeMapEntry->tvars
    jl_svec_t *tvars;
    // list of potentially-ambiguous methods (nothing = none, Vector{Any} of Methods otherwise)
    jl_value_t *ambig;

    // table of all argument types for which we've inferred or compiled this code
    union jl_typemap_t specializations;

    // the AST template (or, for isstaged, code for the generator)
    struct _jl_lambda_info_t *lambda_template;
    jl_array_t *roots;  // pointers in generated code (shared to reduce memory)

    // cache of specializations of this method for invoke(), i.e.
    // cases where this method was called even though it was not necessarily
    // the most specific for the argument types.
    union jl_typemap_t invokes;

    int32_t called;  // bit flags: whether each of the first 8 arguments is called
    int8_t isstaged;
    // if there are intrinsic calls, sparams are probably required to compile successfully,
    // and so unspecialized will be created for each linfo instead of using linfo->def->template
    // 0 = no, 1 = yes, 2 = not yet known
    uint8_t needs_sparam_vals_ducttape;
    uint8_t traced;

// hidden fields:
    // lock for modifications to the method
    jl_mutex_t writelock;
} jl_method_t;

// This holds data for a single executable function body:
// code in Julia IR, static parameters, and (if it has been compiled)
// a function pointer.
typedef struct _jl_lambda_info_t {
    JL_DATA_TYPE
    jl_value_t *rettype;
    jl_svec_t *sparam_syms; // sparams is a vector of values indexed by symbols
    jl_svec_t *sparam_vals;
    jl_tupletype_t *specTypes;  // argument types this was specialized for
    jl_value_t *code;  // compressed uint8 array, or Any array of statements
    jl_value_t *slottypes;
    jl_value_t *ssavaluetypes;  // types of ssa values
    jl_array_t *slotnames; // names of local variables
    jl_array_t *slotflags;  // local var bit flags
    struct _jl_lambda_info_t *unspecialized_ducttape; // if template can't be compiled due to intrinsics, an un-inferred executable copy may get stored here
    jl_method_t *def; // method this is specialized from, (null if this is a toplevel thunk)
    jl_value_t *constval;  // value of the function if jlcall_api==2
    int32_t nargs;
    int8_t isva;
    int8_t inferred;
    int8_t pure;
    int8_t inlineable;
    int8_t inInference; // flags to tell if inference is running on this function
    int8_t inCompile; // flag to tell if codegen is running on this function
    int8_t jlcall_api; // the c-abi for fptr; 0 = jl_fptr_t, 1 = jl_fptr_sparam_t, 2 = constval
    int8_t compile_traced; // if set will notify callback if this linfo is compiled
    jl_fptr_t fptr; // jlcall entry point

// hidden fields:
    // On the old JIT, handles to all Functions generated for this linfo
    // For the new JITs, handles to declarations in the shadow module
    // with the same name as the generated functions for this linfo, suitable
    // for referencing in LLVM IR
    jl_llvm_functions_t functionObjectsDecls;
} jl_lambda_info_t;

// all values are callable as Functions
typedef jl_value_t jl_function_t;

// a TypeConstructor (typealias)
// for example, Vector{T}:
//   body is the Vector{T} <: Type
//   parameters is the set {T}, the bound TypeVars in body
typedef struct {
    JL_DATA_TYPE
    jl_svec_t *parameters;
    jl_value_t *body;
} jl_typector_t;

// represents the "name" part of a DataType, describing the syntactic structure
// of a type and storing all data common to different instantiations of the type,
// including a cache for hash-consed allocation of DataType objects.
typedef struct {
    JL_DATA_TYPE
    jl_sym_t *name;
    struct _jl_module_t *module;
    jl_svec_t *names;  // field names
    // if this is the name of a parametric type, this field points to the
    // original type.
    // a type alias, for example, might make a type constructor that is
    // not the original.
    jl_value_t *primary;
    jl_svec_t *cache;        // sorted array
    jl_svec_t *linearcache;  // unsorted array
    intptr_t hash;
    struct _jl_methtable_t *mt;
} jl_typename_t;

typedef struct {
    JL_DATA_TYPE
    jl_svec_t *types;
} jl_uniontype_t;

// in little-endian, isptr is always the first bit, avoiding the need for a branch in computing isptr
typedef struct {
    uint8_t isptr:1;
    uint8_t size:7;
    uint8_t offset;   // offset relative to data start, excluding type tag
} jl_fielddesc8_t;

typedef struct {
    uint16_t isptr:1;
    uint16_t size:15;
    uint16_t offset;   // offset relative to data start, excluding type tag
} jl_fielddesc16_t;

typedef struct {
    uint32_t isptr:1;
    uint32_t size:31;
    uint32_t offset;   // offset relative to data start, excluding type tag
} jl_fielddesc32_t;

typedef struct {
    uint32_t nfields;
    uint32_t alignment : 28;  // strictest alignment over all fields
    uint32_t haspadding : 1;  // has internal undefined bytes
    uint32_t pointerfree : 1; // has any julia gc pointers
    uint32_t fielddesc_type : 2; // 0 -> 8, 1 -> 16, 2 -> 32
    // union {
    //     jl_fielddesc8_t field8[];
    //     jl_fielddesc16_t field16[];
    //     jl_fielddesc32_t field32[];
    // };
} jl_datatype_layout_t;

typedef struct _jl_datatype_t {
    JL_DATA_TYPE
    jl_typename_t *name;
    struct _jl_datatype_t *super;
    jl_svec_t *parameters;
    jl_svec_t *types;
    jl_value_t *instance;  // for singletons
    const jl_datatype_layout_t *layout;
    int32_t size; // TODO: move to _jl_datatype_layout_t
    int32_t ninitialized;
    uint32_t uid;
    uint8_t abstract;
    uint8_t mutabl;
    // memoized properties
    void *struct_decl;  //llvm::Type*
    void *ditype; // llvm::MDNode* to be used as llvm::DIType(ditype)
    int32_t depth;
    int8_t hastypevars; // bound
    int8_t haswildcard; // unbound
    int8_t isleaftype;
} jl_datatype_t;

typedef struct {
    JL_DATA_TYPE
    jl_sym_t *name;
    jl_value_t *lb;   // lower bound
    jl_value_t *ub;   // upper bound
    uint8_t bound;    // part of a constraint environment
} jl_tvar_t;

typedef struct {
    JL_DATA_TYPE
    jl_value_t *value;
} jl_weakref_t;

typedef struct {
    // not first-class
    jl_sym_t *name;
    jl_value_t *value;
    jl_value_t *globalref;  // cached GlobalRef for this binding
    struct _jl_module_t *owner;  // for individual imported bindings
    unsigned constp:1;
    unsigned exportp:1;
    unsigned imported:1;
    unsigned deprecated:1;
} jl_binding_t;

typedef struct _jl_module_t {
    JL_DATA_TYPE
    jl_sym_t *name;
    struct _jl_module_t *parent;
    htable_t bindings;
    arraylist_t usings;  // modules with all bindings potentially imported
    uint8_t istopmod;
    uint64_t uuid;
    uint32_t counter;
} jl_module_t;

// one Type-to-Value entry
typedef struct _jl_typemap_entry_t {
    JL_DATA_TYPE
    struct _jl_typemap_entry_t *next; // invasive linked list
    jl_tupletype_t *sig; // the type signature for this entry
    jl_svec_t *tvars; // the bound type variables for sig
    jl_tupletype_t *simplesig; // a simple signature for fast rejection
    jl_svec_t *guardsigs;
    union {
        jl_value_t *value;
        jl_lambda_info_t *linfo; // [nullable] for guard entries
        jl_method_t *method;
    } func;
    // memoized properties of sig:
    int8_t isleafsig; // isleaftype(sig) & !any(isType, sig) : unsorted and very fast
    int8_t issimplesig; // all(isleaftype | isAny | isType | isVararg, sig) : sorted and fast
    int8_t va; // isVararg(sig)
} jl_typemap_entry_t;

// one level in a TypeMap tree
// indexed by key if it is a sublevel in an array
struct jl_ordereddict_t {
    jl_array_t *indexes; // Array{Int{8,16,32}}
    jl_array_t *values; // Array{union jl_typemap_t}
};
typedef struct _jl_typemap_level_t {
    JL_DATA_TYPE
    struct jl_ordereddict_t arg1;
    struct jl_ordereddict_t targ;
    jl_typemap_entry_t *linear; // union jl_typemap_t (but no more levels)
    union jl_typemap_t any; // type at offs is Any
    jl_value_t *key; // [nullable]
} jl_typemap_level_t;

// contains the TypeMap for one Type
typedef struct _jl_methtable_t {
    JL_DATA_TYPE
    jl_sym_t *name;
    union jl_typemap_t defs;
    union jl_typemap_t cache;
    intptr_t max_args;  // max # of non-vararg arguments in a signature
    jl_value_t *kwsorter;  // keyword argument sorter function
    jl_module_t *module; // used for incremental serialization to locate original binding
    jl_mutex_t writelock;
} jl_methtable_t;

typedef struct {
    JL_DATA_TYPE
    jl_sym_t *head;
    jl_array_t *args;
    jl_value_t *etype;
} jl_expr_t;

// constants and type objects -------------------------------------------------

extern JL_DLLEXPORT jl_datatype_t *jl_any_type;
extern JL_DLLEXPORT jl_datatype_t *jl_type_type;
extern JL_DLLEXPORT jl_tvar_t     *jl_typetype_tvar;
extern JL_DLLEXPORT jl_datatype_t *jl_typetype_type;
extern JL_DLLEXPORT jl_value_t    *jl_ANY_flag;
extern JL_DLLEXPORT jl_datatype_t *jl_typename_type;
extern JL_DLLEXPORT jl_datatype_t *jl_typector_type;
extern JL_DLLEXPORT jl_datatype_t *jl_sym_type;
extern JL_DLLEXPORT jl_datatype_t *jl_symbol_type;
extern JL_DLLEXPORT jl_datatype_t *jl_ssavalue_type;
extern JL_DLLEXPORT jl_datatype_t *jl_abstractslot_type;
extern JL_DLLEXPORT jl_datatype_t *jl_slotnumber_type;
extern JL_DLLEXPORT jl_datatype_t *jl_typedslot_type;
extern JL_DLLEXPORT jl_datatype_t *jl_simplevector_type;
extern JL_DLLEXPORT jl_typename_t *jl_tuple_typename;
extern JL_DLLEXPORT jl_typename_t *jl_vecelement_typename;
extern JL_DLLEXPORT jl_datatype_t *jl_anytuple_type;
#define jl_tuple_type jl_anytuple_type
extern JL_DLLEXPORT jl_datatype_t *jl_anytuple_type_type;
extern JL_DLLEXPORT jl_datatype_t *jl_vararg_type;
extern JL_DLLEXPORT jl_datatype_t *jl_tvar_type;
extern JL_DLLEXPORT jl_datatype_t *jl_task_type;
extern JL_DLLEXPORT jl_datatype_t *jl_function_type;
extern JL_DLLEXPORT jl_datatype_t *jl_builtin_type;

extern JL_DLLEXPORT jl_datatype_t *jl_uniontype_type;
extern JL_DLLEXPORT jl_datatype_t *jl_datatype_type;

extern JL_DLLEXPORT jl_value_t *jl_bottom_type;
extern JL_DLLEXPORT jl_datatype_t *jl_lambda_info_type;
extern JL_DLLEXPORT jl_datatype_t *jl_method_type;
extern JL_DLLEXPORT jl_datatype_t *jl_module_type;
extern JL_DLLEXPORT jl_datatype_t *jl_abstractarray_type;
extern JL_DLLEXPORT jl_datatype_t *jl_densearray_type;
extern JL_DLLEXPORT jl_datatype_t *jl_array_type;
extern JL_DLLEXPORT jl_typename_t *jl_array_typename;
extern JL_DLLEXPORT jl_datatype_t *jl_weakref_type;
extern JL_DLLEXPORT jl_datatype_t *jl_string_type;
extern JL_DLLEXPORT jl_datatype_t *jl_errorexception_type;
extern JL_DLLEXPORT jl_datatype_t *jl_argumenterror_type;
extern JL_DLLEXPORT jl_datatype_t *jl_loaderror_type;
extern JL_DLLEXPORT jl_datatype_t *jl_initerror_type;
extern JL_DLLEXPORT jl_datatype_t *jl_typeerror_type;
extern JL_DLLEXPORT jl_datatype_t *jl_methoderror_type;
extern JL_DLLEXPORT jl_datatype_t *jl_undefvarerror_type;
extern JL_DLLEXPORT jl_value_t *jl_stackovf_exception;
extern JL_DLLEXPORT jl_value_t *jl_memory_exception;
extern JL_DLLEXPORT jl_value_t *jl_readonlymemory_exception;
extern JL_DLLEXPORT jl_value_t *jl_diverror_exception;
extern JL_DLLEXPORT jl_value_t *jl_domain_exception;
extern JL_DLLEXPORT jl_value_t *jl_overflow_exception;
extern JL_DLLEXPORT jl_value_t *jl_inexact_exception;
extern JL_DLLEXPORT jl_value_t *jl_undefref_exception;
extern JL_DLLEXPORT jl_value_t *jl_interrupt_exception;
extern JL_DLLEXPORT jl_datatype_t *jl_boundserror_type;
extern JL_DLLEXPORT jl_value_t *jl_an_empty_vec_any;

extern JL_DLLEXPORT jl_datatype_t *jl_bool_type;
extern JL_DLLEXPORT jl_datatype_t *jl_char_type;
extern JL_DLLEXPORT jl_datatype_t *jl_int8_type;
extern JL_DLLEXPORT jl_datatype_t *jl_uint8_type;
extern JL_DLLEXPORT jl_datatype_t *jl_int16_type;
extern JL_DLLEXPORT jl_datatype_t *jl_uint16_type;
extern JL_DLLEXPORT jl_datatype_t *jl_int32_type;
extern JL_DLLEXPORT jl_datatype_t *jl_uint32_type;
extern JL_DLLEXPORT jl_datatype_t *jl_int64_type;
extern JL_DLLEXPORT jl_datatype_t *jl_uint64_type;
extern JL_DLLEXPORT jl_datatype_t *jl_float16_type;
extern JL_DLLEXPORT jl_datatype_t *jl_float32_type;
extern JL_DLLEXPORT jl_datatype_t *jl_float64_type;
extern JL_DLLEXPORT jl_datatype_t *jl_floatingpoint_type;
extern JL_DLLEXPORT jl_datatype_t *jl_number_type;
extern JL_DLLEXPORT jl_datatype_t *jl_void_type;
extern JL_DLLEXPORT jl_datatype_t *jl_complex_type;
extern JL_DLLEXPORT jl_datatype_t *jl_signed_type;
extern JL_DLLEXPORT jl_datatype_t *jl_voidpointer_type;
extern JL_DLLEXPORT jl_datatype_t *jl_pointer_type;
extern JL_DLLEXPORT jl_datatype_t *jl_ref_type;

extern JL_DLLEXPORT jl_value_t *jl_array_uint8_type;
extern JL_DLLEXPORT jl_value_t *jl_array_any_type;
extern JL_DLLEXPORT jl_value_t *jl_array_symbol_type;
extern JL_DLLEXPORT jl_datatype_t *jl_expr_type;
extern JL_DLLEXPORT jl_datatype_t *jl_globalref_type;
extern JL_DLLEXPORT jl_datatype_t *jl_linenumbernode_type;
extern JL_DLLEXPORT jl_datatype_t *jl_labelnode_type;
extern JL_DLLEXPORT jl_datatype_t *jl_gotonode_type;
extern JL_DLLEXPORT jl_datatype_t *jl_quotenode_type;
extern JL_DLLEXPORT jl_datatype_t *jl_newvarnode_type;
extern JL_DLLEXPORT jl_datatype_t *jl_intrinsic_type;
extern JL_DLLEXPORT jl_datatype_t *jl_methtable_type;
extern JL_DLLEXPORT jl_datatype_t *jl_typemap_level_type;
extern JL_DLLEXPORT jl_datatype_t *jl_typemap_entry_type;

extern JL_DLLEXPORT jl_svec_t *jl_emptysvec;
extern JL_DLLEXPORT jl_value_t *jl_emptytuple;
extern JL_DLLEXPORT jl_value_t *jl_true;
extern JL_DLLEXPORT jl_value_t *jl_false;
extern JL_DLLEXPORT jl_value_t *jl_nothing;

// some important symbols
extern jl_sym_t *call_sym;    extern jl_sym_t *invoke_sym;
extern jl_sym_t *empty_sym;   extern jl_sym_t *body_sym;
extern jl_sym_t *dots_sym;    extern jl_sym_t *vararg_sym;
extern jl_sym_t *quote_sym;   extern jl_sym_t *newvar_sym;
extern jl_sym_t *top_sym;     extern jl_sym_t *dot_sym;
extern jl_sym_t *line_sym;    extern jl_sym_t *toplevel_sym;
extern jl_sym_t *core_sym;    extern jl_sym_t *globalref_sym;
extern JL_DLLEXPORT jl_sym_t *jl_incomplete_sym;
extern jl_sym_t *error_sym;   extern jl_sym_t *amp_sym;
extern jl_sym_t *module_sym;  extern jl_sym_t *colons_sym;
extern jl_sym_t *export_sym;  extern jl_sym_t *import_sym;
extern jl_sym_t *importall_sym; extern jl_sym_t *using_sym;
extern jl_sym_t *goto_sym;    extern jl_sym_t *goto_ifnot_sym;
extern jl_sym_t *label_sym;   extern jl_sym_t *return_sym;
extern jl_sym_t *lambda_sym;  extern jl_sym_t *assign_sym;
extern jl_sym_t *method_sym;  extern jl_sym_t *slot_sym;
extern jl_sym_t *enter_sym;   extern jl_sym_t *leave_sym;
extern jl_sym_t *exc_sym;     extern jl_sym_t *new_sym;
extern jl_sym_t *compiler_temp_sym;
extern jl_sym_t *const_sym;   extern jl_sym_t *thunk_sym;
extern jl_sym_t *anonymous_sym;  extern jl_sym_t *underscore_sym;
extern jl_sym_t *abstracttype_sym; extern jl_sym_t *bitstype_sym;
extern jl_sym_t *compositetype_sym;
extern jl_sym_t *global_sym; extern jl_sym_t *unused_sym;
extern jl_sym_t *boundscheck_sym; extern jl_sym_t *inbounds_sym;
extern jl_sym_t *copyast_sym; extern jl_sym_t *fastmath_sym;
extern jl_sym_t *pure_sym; extern jl_sym_t *simdloop_sym;
extern jl_sym_t *meta_sym; extern jl_sym_t *list_sym;
extern jl_sym_t *inert_sym; extern jl_sym_t *static_parameter_sym;
extern jl_sym_t *polly_sym; extern jl_sym_t *inline_sym;

// gc -------------------------------------------------------------------------

typedef struct _jl_gcframe_t {
    size_t nroots;
    struct _jl_gcframe_t *prev;
    // actual roots go here
} jl_gcframe_t;

// NOTE: it is the caller's responsibility to make sure arguments are
// rooted such that the gc can see them on the stack.
// `foo(f(), g())` is not safe,
// since the result of `f()` is not rooted during the call to `g()`,
// and the arguments to foo are not gc-protected during the call to foo.
// foo can't do anything about it, so the caller must do:
// jl_value_t *x=NULL, *y=NULL; JL_GC_PUSH2(&x, &y);
// x = f(); y = g(); foo(x, y)

#define jl_pgcstack (jl_get_ptls_states()->pgcstack)

#define JL_GC_PUSH1(arg1)                                                 \
  void *__gc_stkf[] = {(void*)3, jl_pgcstack, arg1};                      \
  jl_pgcstack = (jl_gcframe_t*)__gc_stkf;

#define JL_GC_PUSH2(arg1, arg2)                                           \
  void *__gc_stkf[] = {(void*)5, jl_pgcstack, arg1, arg2};                \
  jl_pgcstack = (jl_gcframe_t*)__gc_stkf;

#define JL_GC_PUSH3(arg1, arg2, arg3)                                     \
  void *__gc_stkf[] = {(void*)7, jl_pgcstack, arg1, arg2, arg3};          \
  jl_pgcstack = (jl_gcframe_t*)__gc_stkf;

#define JL_GC_PUSH4(arg1, arg2, arg3, arg4)                               \
  void *__gc_stkf[] = {(void*)9, jl_pgcstack, arg1, arg2, arg3, arg4};    \
  jl_pgcstack = (jl_gcframe_t*)__gc_stkf;

#define JL_GC_PUSH5(arg1, arg2, arg3, arg4, arg5)                               \
  void *__gc_stkf[] = {(void*)11, jl_pgcstack, arg1, arg2, arg3, arg4, arg5};    \
  jl_pgcstack = (jl_gcframe_t*)__gc_stkf;

#define JL_GC_PUSHARGS(rts_var,n)                               \
  rts_var = ((jl_value_t**)alloca(((n)+2)*sizeof(jl_value_t*)))+2;    \
  ((void**)rts_var)[-2] = (void*)(((size_t)(n))<<1);                  \
  ((void**)rts_var)[-1] = jl_pgcstack;                          \
  memset((void*)rts_var, 0, (n)*sizeof(jl_value_t*));           \
  jl_pgcstack = (jl_gcframe_t*)&(((void**)rts_var)[-2])

#define JL_GC_POP() (jl_pgcstack = jl_pgcstack->prev)

JL_DLLEXPORT int jl_gc_enable(int on);
JL_DLLEXPORT int jl_gc_is_enabled(void);
JL_DLLEXPORT int64_t jl_gc_total_bytes(void);
JL_DLLEXPORT uint64_t jl_gc_total_hrtime(void);
JL_DLLEXPORT int64_t jl_gc_diff_total_bytes(void);

JL_DLLEXPORT void jl_gc_collect(int);

JL_DLLEXPORT void jl_gc_add_finalizer(jl_value_t *v, jl_function_t *f);
JL_DLLEXPORT void jl_finalize(jl_value_t *o);
JL_DLLEXPORT jl_weakref_t *jl_gc_new_weakref(jl_value_t *value);
JL_DLLEXPORT jl_value_t *jl_gc_alloc_0w(void);
JL_DLLEXPORT jl_value_t *jl_gc_alloc_1w(void);
JL_DLLEXPORT jl_value_t *jl_gc_alloc_2w(void);
JL_DLLEXPORT jl_value_t *jl_gc_alloc_3w(void);
JL_DLLEXPORT jl_value_t *jl_gc_allocobj(size_t sz);

JL_DLLEXPORT void jl_clear_malloc_data(void);

// GC write barriers
JL_DLLEXPORT void jl_gc_queue_root(jl_value_t *root); // root isa jl_value_t*

// Do NOT put a safepoint here
STATIC_INLINE void jl_gc_wb(void *parent, void *ptr)
{
    // parent and ptr isa jl_value_t*
    if (__unlikely(jl_astaggedvalue(parent)->bits.gc == 3 &&
                   (jl_astaggedvalue(ptr)->bits.gc & 1) == 0))
        jl_gc_queue_root((jl_value_t*)parent);
}

STATIC_INLINE void jl_gc_wb_back(void *ptr) // ptr isa jl_value_t*
{
    // if ptr is old
    if (__unlikely(jl_astaggedvalue(ptr)->bits.gc == 3)) {
        jl_gc_queue_root((jl_value_t*)ptr);
    }
}

JL_DLLEXPORT void *jl_gc_managed_malloc(size_t sz);
JL_DLLEXPORT void *jl_gc_managed_realloc(void *d, size_t sz, size_t oldsz,
                                         int isaligned, jl_value_t *owner);

// object accessors -----------------------------------------------------------

#define jl_svec_len(t)              (((jl_svec_t*)(t))->length)
#define jl_svec_set_len_unsafe(t,n) (((jl_svec_t*)(t))->length=(n))
#define jl_svec_data(t) ((jl_value_t**)((char*)(t) + sizeof(jl_svec_t)))

STATIC_INLINE jl_value_t *jl_svecref(void *t, size_t i)
{
    assert(jl_typeis(t,jl_simplevector_type));
    assert(i < jl_svec_len(t));
    return jl_svec_data(t)[i];
}
STATIC_INLINE jl_value_t *jl_svecset(void *t, size_t i, void *x)
{
    assert(jl_typeis(t,jl_simplevector_type));
    assert(i < jl_svec_len(t));
    jl_svec_data(t)[i] = (jl_value_t*)x;
    if (x) jl_gc_wb(t, x);
    return (jl_value_t*)x;
}

#ifdef STORE_ARRAY_LEN
#define jl_array_len(a)   (((jl_array_t*)(a))->length)
#else
JL_DLLEXPORT size_t jl_array_len_(jl_array_t *a);
#define jl_array_len(a)   jl_array_len_((jl_array_t*)(a))
#endif
#define jl_array_data(a)  ((void*)((jl_array_t*)(a))->data)
#define jl_array_dim(a,i) ((&((jl_array_t*)(a))->nrows)[i])
#define jl_array_dim0(a)  (((jl_array_t*)(a))->nrows)
#define jl_array_nrows(a) (((jl_array_t*)(a))->nrows)
#define jl_array_ndims(a) ((int32_t)(((jl_array_t*)a)->flags.ndims))
#define jl_array_data_owner_offset(ndims) (offsetof(jl_array_t,ncols) + sizeof(size_t)*(1+jl_array_ndimwords(ndims))) // in bytes
#define jl_array_data_owner(a) (*((jl_value_t**)((char*)a + jl_array_data_owner_offset(jl_array_ndims(a)))))

STATIC_INLINE jl_value_t *jl_array_ptr_ref(void *a, size_t i)
{
    assert(i < jl_array_len(a));
    return ((jl_value_t**)(jl_array_data(a)))[i];
}
STATIC_INLINE jl_value_t *jl_array_ptr_set(void *a, size_t i, void *x)
{
    assert(i < jl_array_len(a));
    ((jl_value_t**)(jl_array_data(a)))[i] = (jl_value_t*)x;
    if (x) {
        if (((jl_array_t*)a)->flags.how == 3) {
            a = jl_array_data_owner(a);
        }
        jl_gc_wb(a, x);
    }
    return (jl_value_t*)x;
}

STATIC_INLINE uint8_t jl_array_uint8_ref(void *a, size_t i)
{
    assert(i < jl_array_len(a));
    assert(jl_typeis(a, jl_array_uint8_type));
    return ((uint8_t*)(jl_array_data(a)))[i];
}
STATIC_INLINE void jl_array_uint8_set(void *a, size_t i, uint8_t x)
{
    assert(i < jl_array_len(a));
    assert(jl_typeis(a, jl_array_uint8_type));
    ((uint8_t*)(jl_array_data(a)))[i] = x;
}

#define jl_exprarg(e,n) (((jl_value_t**)jl_array_data(((jl_expr_t*)(e))->args))[n])
#define jl_exprargset(e, n, v) jl_array_ptr_set(((jl_expr_t*)(e))->args, n, v)
#define jl_expr_nargs(e) jl_array_len(((jl_expr_t*)(e))->args)

#define jl_fieldref(s,i) jl_get_nth_field(((jl_value_t*)s),i)
#define jl_nfields(v)    jl_datatype_nfields(jl_typeof(v))

// Not using jl_fieldref to avoid allocations
#define jl_linenode_line(x) (((intptr_t*)x)[0])
#define jl_labelnode_label(x) (((intptr_t*)x)[0])
#define jl_slot_number(x) (((intptr_t*)x)[0])
#define jl_typedslot_get_type(x) (((jl_value_t**)x)[1])
#define jl_gotonode_label(x) (((intptr_t*)x)[0])
#define jl_globalref_mod(s) (*(jl_module_t**)s)
#define jl_globalref_name(s) (((jl_sym_t**)s)[1])

#define jl_nparams(t)  jl_svec_len(((jl_datatype_t*)(t))->parameters)
#define jl_tparam0(t)  jl_svecref(((jl_datatype_t*)(t))->parameters, 0)
#define jl_tparam1(t)  jl_svecref(((jl_datatype_t*)(t))->parameters, 1)
#define jl_tparam(t,i) jl_svecref(((jl_datatype_t*)(t))->parameters, i)

// get a pointer to the data in a datatype
#define jl_data_ptr(v)  ((jl_value_t**)v)

#define jl_array_ptr_data(a)   ((jl_value_t**)((jl_array_t*)a)->data)
#define jl_string_data(s) ((char*)((jl_array_t*)jl_data_ptr(s)[0])->data)
#define jl_string_len(s)  (jl_array_len((jl_array_t*)(jl_data_ptr(s)[0])))
#define jl_iostr_data(s)  ((char*)((jl_array_t*)jl_data_ptr(s)[0])->data)

#define jl_gf_mtable(f) (((jl_datatype_t*)jl_typeof(f))->name->mt)
#define jl_gf_name(f)   (jl_gf_mtable(f)->name)

// struct type info
#define jl_field_name(st,i)    (jl_sym_t*)jl_svecref(((jl_datatype_t*)st)->name->names, (i))
#define jl_field_type(st,i)    jl_svecref(((jl_datatype_t*)st)->types, (i))
#define jl_field_count(st)     jl_svec_len(((jl_datatype_t*)st)->types)
#define jl_datatype_size(t)    (((jl_datatype_t*)t)->size)
#define jl_datatype_nfields(t) (((jl_datatype_t*)(t))->layout->nfields)

// inline version with strong type check to detect typos in a `->name` chain
STATIC_INLINE char *jl_symbol_name_(jl_sym_t *s)
{
    return (char*)s + LLT_ALIGN(sizeof(jl_sym_t), sizeof(void*));
}
#define jl_symbol_name(s) jl_symbol_name_(s)

#define jl_dt_layout_fields(d) ((const char*)(d) + sizeof(jl_datatype_layout_t))

#define DEFINE_FIELD_ACCESSORS(f)                                             \
    static inline uint32_t jl_field_##f(jl_datatype_t *st, int i)             \
    {                                                                         \
        const jl_datatype_layout_t *ly = st->layout;                          \
        assert(i >= 0 && (size_t)i < ly->nfields);                            \
        if (ly->fielddesc_type == 0) {                                        \
            return ((const jl_fielddesc8_t*)jl_dt_layout_fields(ly))[i].f;    \
        }                                                                     \
        else if (ly->fielddesc_type == 1) {                                   \
            return ((const jl_fielddesc16_t*)jl_dt_layout_fields(ly))[i].f;   \
        }                                                                     \
        else {                                                                \
            return ((const jl_fielddesc32_t*)jl_dt_layout_fields(ly))[i].f;   \
        }                                                                     \
    }                                                                         \

DEFINE_FIELD_ACCESSORS(offset)
DEFINE_FIELD_ACCESSORS(size)
static inline int jl_field_isptr(jl_datatype_t *st, int i)
{
    const jl_datatype_layout_t *ly = st->layout;
    assert(i >= 0 && (size_t)i < ly->nfields);
    return ((const jl_fielddesc8_t*)(jl_dt_layout_fields(ly) + (i << (ly->fielddesc_type + 1))))->isptr;
}

static inline uint32_t jl_fielddesc_size(int8_t fielddesc_type)
{
    if (fielddesc_type == 0) {
        return sizeof(jl_fielddesc8_t);
    }
    else if (fielddesc_type == 1) {
        return sizeof(jl_fielddesc16_t);
    }
    else {
        return sizeof(jl_fielddesc32_t);
    }
}

#undef DEFINE_FIELD_ACCESSORS

// basic predicates -----------------------------------------------------------
#define jl_is_nothing(v)     (((jl_value_t*)(v)) == ((jl_value_t*)jl_nothing))
#define jl_is_tuple(v)       (((jl_datatype_t*)jl_typeof(v))->name == jl_tuple_typename)
#define jl_is_svec(v)        jl_typeis(v,jl_simplevector_type)
#define jl_is_simplevector(v) jl_is_svec(v)
#define jl_is_datatype(v)    jl_typeis(v,jl_datatype_type)
#define jl_is_mutable(t)     (((jl_datatype_t*)t)->mutabl)
#define jl_is_mutable_datatype(t) (jl_is_datatype(t) && (((jl_datatype_t*)t)->mutabl))
#define jl_is_immutable(t)   (!((jl_datatype_t*)t)->mutabl)
#define jl_is_immutable_datatype(t) (jl_is_datatype(t) && (!((jl_datatype_t*)t)->mutabl))
#define jl_is_uniontype(v)   jl_typeis(v,jl_uniontype_type)
#define jl_is_typevar(v)     jl_typeis(v,jl_tvar_type)
#define jl_is_typector(v)    jl_typeis(v,jl_typector_type)
#define jl_is_TypeConstructor(v)    jl_typeis(v,jl_typector_type)
#define jl_is_typename(v)    jl_typeis(v,jl_typename_type)
#define jl_is_int8(v)        jl_typeis(v,jl_int8_type)
#define jl_is_int16(v)       jl_typeis(v,jl_int16_type)
#define jl_is_int32(v)       jl_typeis(v,jl_int32_type)
#define jl_is_int64(v)       jl_typeis(v,jl_int64_type)
#define jl_is_uint8(v)       jl_typeis(v,jl_uint8_type)
#define jl_is_uint16(v)      jl_typeis(v,jl_uint16_type)
#define jl_is_uint32(v)      jl_typeis(v,jl_uint32_type)
#define jl_is_uint64(v)      jl_typeis(v,jl_uint64_type)
#define jl_is_float(v)       jl_subtype(v,(jl_value_t*)jl_floatingpoint_type,1)
#define jl_is_floattype(v)   jl_subtype(v,(jl_value_t*)jl_floatingpoint_type,0)
#define jl_is_float32(v)     jl_typeis(v,jl_float32_type)
#define jl_is_float64(v)     jl_typeis(v,jl_float64_type)
#define jl_is_bool(v)        jl_typeis(v,jl_bool_type)
#define jl_is_symbol(v)      jl_typeis(v,jl_sym_type)
#define jl_is_ssavalue(v)    jl_typeis(v,jl_ssavalue_type)
#define jl_is_slot(v)        (jl_typeis(v,jl_slotnumber_type) || jl_typeis(v,jl_typedslot_type))
#define jl_is_expr(v)        jl_typeis(v,jl_expr_type)
#define jl_is_globalref(v)   jl_typeis(v,jl_globalref_type)
#define jl_is_labelnode(v)   jl_typeis(v,jl_labelnode_type)
#define jl_is_gotonode(v)    jl_typeis(v,jl_gotonode_type)
#define jl_is_quotenode(v)   jl_typeis(v,jl_quotenode_type)
#define jl_is_newvarnode(v)  jl_typeis(v,jl_newvarnode_type)
#define jl_is_linenode(v)    jl_typeis(v,jl_linenumbernode_type)
#define jl_is_lambda_info(v) jl_typeis(v,jl_lambda_info_type)
#define jl_is_method(v)      jl_typeis(v,jl_method_type)
#define jl_is_module(v)      jl_typeis(v,jl_module_type)
#define jl_is_mtable(v)      jl_typeis(v,jl_methtable_type)
#define jl_is_task(v)        jl_typeis(v,jl_task_type)
#define jl_is_string(v)      jl_typeis(v,jl_string_type)
#define jl_is_cpointer(v)    jl_is_cpointer_type(jl_typeof(v))
#define jl_is_pointer(v)     jl_is_cpointer_type(jl_typeof(v))

STATIC_INLINE int jl_is_bitstype(void *v)
{
    return (jl_is_datatype(v) && jl_is_immutable(v) &&
            ((jl_datatype_t*)(v))->layout &&
            jl_datatype_nfields(v) == 0 &&
            ((jl_datatype_t*)(v))->size > 0);
}

STATIC_INLINE int jl_is_structtype(void *v)
{
    return (jl_is_datatype(v) &&
            (jl_field_count(v) > 0 ||
             ((jl_datatype_t*)(v))->size == 0) &&
            !((jl_datatype_t*)(v))->abstract);
}

STATIC_INLINE int jl_isbits(void *t)   // corresponding to isbits() in julia
{
    return (jl_is_datatype(t) && ((jl_datatype_t*)t)->layout &&
            !((jl_datatype_t*)t)->mutabl && ((jl_datatype_t*)t)->layout->pointerfree);
}

STATIC_INLINE int jl_is_datatype_singleton(jl_datatype_t *d)
{
    return (d->instance != NULL);
}

STATIC_INLINE int jl_is_datatype_make_singleton(jl_datatype_t *d)
{
    return (!d->abstract && d->size == 0 && d != jl_sym_type && d->name != jl_array_typename &&
            d->uid != 0 && (d->name->names == jl_emptysvec || !d->mutabl));
}

STATIC_INLINE int jl_is_abstracttype(void *v)
{
    return (jl_is_datatype(v) && ((jl_datatype_t*)(v))->abstract);
}

STATIC_INLINE int jl_is_array_type(void *t)
{
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == jl_array_typename);
}

STATIC_INLINE int jl_is_array(void *v)
{
    jl_value_t *t = jl_typeof(v);
    return jl_is_array_type(t);
}

STATIC_INLINE int jl_is_cpointer_type(jl_value_t *t)
{
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == jl_pointer_type->name);
}

STATIC_INLINE int jl_is_abstract_ref_type(jl_value_t *t)
{
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == jl_ref_type->name);
}

STATIC_INLINE jl_value_t *jl_is_ref_type(jl_value_t *t)
{
    if (!jl_is_datatype(t)) return 0;
    jl_datatype_t *dt = (jl_datatype_t*)t;
    while (dt != jl_any_type && dt->name != dt->super->name) {
        if (dt->name == jl_ref_type->name)
            return (jl_value_t*)dt;
        dt = dt->super;
    }
    return 0;
}

STATIC_INLINE int jl_is_tuple_type(void *t)
{
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == jl_tuple_typename);
}

STATIC_INLINE int jl_is_vecelement_type(jl_value_t* t)
{
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == jl_vecelement_typename);
}

STATIC_INLINE int jl_is_type_type(jl_value_t *v)
{
    return (jl_is_datatype(v) &&
            ((jl_datatype_t*)(v))->name == jl_type_type->name);
}

// object identity
JL_DLLEXPORT int jl_egal(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT uintptr_t jl_object_id(jl_value_t *v);

// type predicates and basic operations
JL_DLLEXPORT int jl_is_leaf_type(jl_value_t *v);
JL_DLLEXPORT int jl_has_typevars(jl_value_t *v);
JL_DLLEXPORT int jl_subtype(jl_value_t *a, jl_value_t *b, int ta);
JL_DLLEXPORT int jl_types_equal(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_type_union(jl_svec_t *types);
JL_DLLEXPORT jl_value_t *jl_type_intersection(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT int jl_args_morespecific(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT const char *jl_typename_str(jl_value_t *v);
JL_DLLEXPORT const char *jl_typeof_str(jl_value_t *v);
JL_DLLEXPORT int jl_type_morespecific(jl_value_t *a, jl_value_t *b);

#ifdef NDEBUG
STATIC_INLINE int jl_is_leaf_type_(jl_value_t *v)
{
    return jl_is_datatype(v) && ((jl_datatype_t*)v)->isleaftype;
}
#define jl_is_leaf_type(v) jl_is_leaf_type_(v)
#endif

// type constructors
JL_DLLEXPORT jl_typename_t *jl_new_typename(jl_sym_t *name);
JL_DLLEXPORT jl_tvar_t *jl_new_typevar(jl_sym_t *name,jl_value_t *lb,jl_value_t *ub);
JL_DLLEXPORT jl_value_t *jl_apply_type(jl_value_t *tc, jl_svec_t *params);
JL_DLLEXPORT jl_tupletype_t *jl_apply_tuple_type(jl_svec_t *params);
JL_DLLEXPORT jl_tupletype_t *jl_apply_tuple_type_v(jl_value_t **p, size_t np);
JL_DLLEXPORT jl_datatype_t *jl_new_datatype(jl_sym_t *name, jl_datatype_t *super,
                                            jl_svec_t *parameters,
                                            jl_svec_t *fnames, jl_svec_t *ftypes,
                                            int abstract, int mutabl,
                                            int ninitialized);
JL_DLLEXPORT jl_datatype_t *jl_new_bitstype(jl_value_t *name,
                                            jl_datatype_t *super,
                                            jl_svec_t *parameters, size_t nbits);

// constructors
JL_DLLEXPORT jl_value_t *jl_new_bits(jl_value_t *bt, void *data);
JL_DLLEXPORT jl_value_t *jl_new_struct(jl_datatype_t *type, ...);
JL_DLLEXPORT jl_value_t *jl_new_structv(jl_datatype_t *type, jl_value_t **args,
                                        uint32_t na);
JL_DLLEXPORT jl_value_t *jl_new_struct_uninit(jl_datatype_t *type);
JL_DLLEXPORT jl_lambda_info_t *jl_new_lambda_info_uninit(void);
JL_DLLEXPORT jl_lambda_info_t *jl_new_lambda_info_from_ast(jl_expr_t *ast);
JL_DLLEXPORT jl_method_t *jl_new_method(jl_lambda_info_t *definition, jl_sym_t *name, jl_tupletype_t *sig, jl_svec_t *tvars, int isstaged);
JL_DLLEXPORT jl_svec_t *jl_svec(size_t n, ...);
JL_DLLEXPORT jl_svec_t *jl_svec1(void *a);
JL_DLLEXPORT jl_svec_t *jl_svec2(void *a, void *b);
JL_DLLEXPORT jl_svec_t *jl_alloc_svec(size_t n);
JL_DLLEXPORT jl_svec_t *jl_alloc_svec_uninit(size_t n);
JL_DLLEXPORT jl_svec_t *jl_svec_copy(jl_svec_t *a);
JL_DLLEXPORT jl_svec_t *jl_svec_fill(size_t n, jl_value_t *x);
JL_DLLEXPORT jl_value_t *jl_tupletype_fill(size_t n, jl_value_t *v);
JL_DLLEXPORT jl_sym_t *jl_symbol(const char *str);
JL_DLLEXPORT jl_sym_t *jl_symbol_lookup(const char *str);
JL_DLLEXPORT jl_sym_t *jl_symbol_n(const char *str, int32_t len);
JL_DLLEXPORT jl_sym_t *jl_gensym(void);
JL_DLLEXPORT jl_sym_t *jl_tagged_gensym(const char *str, int32_t len);
JL_DLLEXPORT jl_sym_t *jl_get_root_symbol(void);
JL_DLLEXPORT jl_value_t *jl_generic_function_def(jl_sym_t *name, jl_value_t **bp,
                                                 jl_value_t *bp_owner,
                                                 jl_binding_t *bnd);
JL_DLLEXPORT void jl_method_def(jl_svec_t *argdata, jl_lambda_info_t *f, jl_value_t *isstaged);
JL_DLLEXPORT jl_function_t *jl_get_kwsorter(jl_typename_t *tn);
JL_DLLEXPORT jl_value_t *jl_box_bool(int8_t x);
JL_DLLEXPORT jl_value_t *jl_box_int8(int8_t x);
JL_DLLEXPORT jl_value_t *jl_box_uint8(uint8_t x);
JL_DLLEXPORT jl_value_t *jl_box_int16(int16_t x);
JL_DLLEXPORT jl_value_t *jl_box_uint16(uint16_t x);
JL_DLLEXPORT jl_value_t *jl_box_int32(int32_t x);
JL_DLLEXPORT jl_value_t *jl_box_uint32(uint32_t x);
JL_DLLEXPORT jl_value_t *jl_box_char(uint32_t x);
JL_DLLEXPORT jl_value_t *jl_box_int64(int64_t x);
JL_DLLEXPORT jl_value_t *jl_box_uint64(uint64_t x);
JL_DLLEXPORT jl_value_t *jl_box_float32(float x);
JL_DLLEXPORT jl_value_t *jl_box_float64(double x);
JL_DLLEXPORT jl_value_t *jl_box_voidpointer(void *x);
JL_DLLEXPORT jl_value_t *jl_box_ssavalue(size_t x);
JL_DLLEXPORT jl_value_t *jl_box_slotnumber(size_t x);
JL_DLLEXPORT jl_value_t *jl_box8 (jl_datatype_t *t, int8_t  x);
JL_DLLEXPORT jl_value_t *jl_box16(jl_datatype_t *t, int16_t x);
JL_DLLEXPORT jl_value_t *jl_box32(jl_datatype_t *t, int32_t x);
JL_DLLEXPORT jl_value_t *jl_box64(jl_datatype_t *t, int64_t x);
JL_DLLEXPORT int8_t jl_unbox_bool(jl_value_t *v);
JL_DLLEXPORT int8_t jl_unbox_int8(jl_value_t *v);
JL_DLLEXPORT uint8_t jl_unbox_uint8(jl_value_t *v);
JL_DLLEXPORT int16_t jl_unbox_int16(jl_value_t *v);
JL_DLLEXPORT uint16_t jl_unbox_uint16(jl_value_t *v);
JL_DLLEXPORT int32_t jl_unbox_int32(jl_value_t *v);
JL_DLLEXPORT uint32_t jl_unbox_uint32(jl_value_t *v);
JL_DLLEXPORT int64_t jl_unbox_int64(jl_value_t *v);
JL_DLLEXPORT uint64_t jl_unbox_uint64(jl_value_t *v);
JL_DLLEXPORT float jl_unbox_float32(jl_value_t *v);
JL_DLLEXPORT double jl_unbox_float64(jl_value_t *v);
JL_DLLEXPORT void *jl_unbox_voidpointer(jl_value_t *v);

JL_DLLEXPORT int jl_get_size(jl_value_t *val, size_t *pnt);

#ifdef _P64
#define jl_box_long(x)   jl_box_int64(x)
#define jl_box_ulong(x)  jl_box_uint64(x)
#define jl_unbox_long(x) jl_unbox_int64(x)
#define jl_is_long(x)    jl_is_int64(x)
#define jl_long_type     jl_int64_type
#else
#define jl_box_long(x)   jl_box_int32(x)
#define jl_box_ulong(x)  jl_box_uint32(x)
#define jl_unbox_long(x) jl_unbox_int32(x)
#define jl_is_long(x)    jl_is_int32(x)
#define jl_long_type     jl_int32_type
#endif

// Each tuple can exist in one of 4 Vararg states:
//   NONE: no vararg                            Tuple{Int,Float32}
//   INT: vararg with integer length            Tuple{Int,Vararg{Float32,2}}
//   BOUND: vararg with bound TypeVar length    Tuple{Int,Vararg{Float32,N}}
//   UNBOUND: vararg with unbound length        Tuple{Int,Vararg{Float32}}
typedef enum {
    JL_VARARG_NONE    = 0,
    JL_VARARG_INT     = 1,
    JL_VARARG_BOUND   = 2,
    JL_VARARG_UNBOUND = 3
} jl_vararg_kind_t;

STATIC_INLINE int jl_is_vararg_type(jl_value_t *v)
{
    return (jl_is_datatype(v) &&
            ((jl_datatype_t*)(v))->name == jl_vararg_type->name);
}

STATIC_INLINE jl_vararg_kind_t jl_vararg_kind(jl_value_t *v)
{
    if (!jl_is_vararg_type(v))
        return JL_VARARG_NONE;
    jl_value_t *lenv = jl_tparam1(v);
    if (jl_is_long(lenv))
        return JL_VARARG_INT;
    if (jl_is_typevar(lenv))
        return ((jl_tvar_t*)lenv)->bound ? JL_VARARG_BOUND : JL_VARARG_UNBOUND;
    return JL_VARARG_UNBOUND;
}

STATIC_INLINE int jl_is_va_tuple(jl_datatype_t *t)
{
    assert(jl_is_tuple_type(t));
    size_t l = jl_svec_len(t->parameters);
    return (l>0 && jl_is_vararg_type(jl_tparam(t,l-1)));
}

STATIC_INLINE jl_vararg_kind_t jl_va_tuple_kind(jl_datatype_t *t)
{
    assert(jl_is_tuple_type(t));
    size_t l = jl_svec_len(t->parameters);
    if (l == 0)
        return JL_VARARG_NONE;
    return jl_vararg_kind(jl_tparam(t,l-1));
}

// structs
JL_DLLEXPORT int         jl_field_index(jl_datatype_t *t, jl_sym_t *fld, int err);
JL_DLLEXPORT jl_value_t *jl_get_nth_field(jl_value_t *v, size_t i);
JL_DLLEXPORT jl_value_t *jl_get_nth_field_checked(jl_value_t *v, size_t i);
JL_DLLEXPORT void        jl_set_nth_field(jl_value_t *v, size_t i,
                                          jl_value_t *rhs);
JL_DLLEXPORT int         jl_field_isdefined(jl_value_t *v, size_t i);
JL_DLLEXPORT jl_value_t *jl_get_field(jl_value_t *o, const char *fld);
JL_DLLEXPORT jl_value_t *jl_value_ptr(jl_value_t *a);

// arrays
JL_DLLEXPORT jl_array_t *jl_new_array(jl_value_t *atype, jl_value_t *dims);
JL_DLLEXPORT jl_array_t *jl_reshape_array(jl_value_t *atype, jl_array_t *data,
                                          jl_value_t *dims);
JL_DLLEXPORT jl_array_t *jl_ptr_to_array_1d(jl_value_t *atype, void *data,
                                            size_t nel, int own_buffer);
JL_DLLEXPORT jl_array_t *jl_ptr_to_array(jl_value_t *atype, void *data,
                                         jl_value_t *dims, int own_buffer);

JL_DLLEXPORT jl_array_t *jl_alloc_array_1d(jl_value_t *atype, size_t nr);
JL_DLLEXPORT jl_array_t *jl_alloc_array_2d(jl_value_t *atype, size_t nr,
                                           size_t nc);
JL_DLLEXPORT jl_array_t *jl_alloc_array_3d(jl_value_t *atype, size_t nr,
                                           size_t nc, size_t z);
JL_DLLEXPORT jl_array_t *jl_pchar_to_array(const char *str, size_t len);
JL_DLLEXPORT jl_value_t *jl_pchar_to_string(const char *str, size_t len);
JL_DLLEXPORT jl_value_t *jl_cstr_to_string(const char *str);
JL_DLLEXPORT jl_value_t *jl_array_to_string(jl_array_t *a);
JL_DLLEXPORT jl_array_t *jl_alloc_vec_any(size_t n);
JL_DLLEXPORT jl_value_t *jl_arrayref(jl_array_t *a, size_t i);  // 0-indexed
JL_DLLEXPORT void jl_arrayset(jl_array_t *a, jl_value_t *v, size_t i);  // 0-indexed
JL_DLLEXPORT void jl_arrayunset(jl_array_t *a, size_t i);  // 0-indexed
JL_DLLEXPORT void jl_array_grow_end(jl_array_t *a, size_t inc);
JL_DLLEXPORT void jl_array_del_end(jl_array_t *a, size_t dec);
JL_DLLEXPORT void jl_array_grow_beg(jl_array_t *a, size_t inc);
JL_DLLEXPORT void jl_array_del_beg(jl_array_t *a, size_t dec);
JL_DLLEXPORT void jl_array_sizehint(jl_array_t *a, size_t sz);
JL_DLLEXPORT void jl_array_ptr_1d_push(jl_array_t *a, jl_value_t *item);
JL_DLLEXPORT void jl_array_ptr_1d_push2(jl_array_t *a, jl_value_t *b, jl_value_t *c);
JL_DLLEXPORT jl_value_t *jl_apply_array_type(jl_datatype_t *type, size_t dim);
// property access
JL_DLLEXPORT void *jl_array_ptr(jl_array_t *a);
JL_DLLEXPORT void *jl_array_eltype(jl_value_t *a);
JL_DLLEXPORT int jl_array_rank(jl_value_t *a);
JL_DLLEXPORT size_t jl_array_size(jl_value_t *a, int d);

// strings
JL_DLLEXPORT const char *jl_string_ptr(jl_value_t *s);

// modules and global variables
extern JL_DLLEXPORT jl_module_t *jl_main_module;
extern JL_DLLEXPORT jl_module_t *jl_internal_main_module;
extern JL_DLLEXPORT jl_module_t *jl_core_module;
extern JL_DLLEXPORT jl_module_t *jl_base_module;
extern JL_DLLEXPORT jl_module_t *jl_top_module;
JL_DLLEXPORT jl_module_t *jl_new_module(jl_sym_t *name);
// get binding for reading
JL_DLLEXPORT jl_binding_t *jl_get_binding(jl_module_t *m, jl_sym_t *var);
JL_DLLEXPORT jl_binding_t *jl_get_binding_or_error(jl_module_t *m, jl_sym_t *var);
JL_DLLEXPORT jl_value_t *jl_module_globalref(jl_module_t *m, jl_sym_t *var);
// get binding for assignment
JL_DLLEXPORT jl_binding_t *jl_get_binding_wr(jl_module_t *m, jl_sym_t *var);
JL_DLLEXPORT jl_binding_t *jl_get_binding_for_method_def(jl_module_t *m,
                                                         jl_sym_t *var);
JL_DLLEXPORT int jl_boundp(jl_module_t *m, jl_sym_t *var);
JL_DLLEXPORT int jl_defines_or_exports_p(jl_module_t *m, jl_sym_t *var);
JL_DLLEXPORT int jl_binding_resolved_p(jl_module_t *m, jl_sym_t *var);
JL_DLLEXPORT int jl_is_const(jl_module_t *m, jl_sym_t *var);
JL_DLLEXPORT jl_value_t *jl_get_global(jl_module_t *m, jl_sym_t *var);
JL_DLLEXPORT void jl_set_global(jl_module_t *m, jl_sym_t *var, jl_value_t *val);
JL_DLLEXPORT void jl_set_const(jl_module_t *m, jl_sym_t *var, jl_value_t *val);
JL_DLLEXPORT void jl_checked_assignment(jl_binding_t *b, jl_value_t *rhs);
JL_DLLEXPORT void jl_declare_constant(jl_binding_t *b);
JL_DLLEXPORT void jl_module_using(jl_module_t *to, jl_module_t *from);
JL_DLLEXPORT void jl_module_use(jl_module_t *to, jl_module_t *from, jl_sym_t *s);
JL_DLLEXPORT void jl_module_import(jl_module_t *to, jl_module_t *from,
                                   jl_sym_t *s);
JL_DLLEXPORT void jl_module_importall(jl_module_t *to, jl_module_t *from);
JL_DLLEXPORT void jl_module_export(jl_module_t *from, jl_sym_t *s);
JL_DLLEXPORT int jl_is_imported(jl_module_t *m, jl_sym_t *s);
JL_DLLEXPORT jl_module_t *jl_new_main_module(void);
JL_DLLEXPORT void jl_add_standard_imports(jl_module_t *m);
STATIC_INLINE jl_function_t *jl_get_function(jl_module_t *m, const char *name)
{
    return (jl_function_t*)jl_get_global(m, jl_symbol(name));
}
int jl_is_submodule(jl_module_t *child, jl_module_t *parent);

// eq hash tables
JL_DLLEXPORT jl_array_t *jl_eqtable_put(jl_array_t *h, void *key, void *val);
JL_DLLEXPORT jl_value_t *jl_eqtable_get(jl_array_t *h, void *key,
                                        jl_value_t *deflt);

// system information
JL_DLLEXPORT int jl_errno(void);
JL_DLLEXPORT void jl_set_errno(int e);
JL_DLLEXPORT int32_t jl_stat(const char *path, char *statbuf);
JL_DLLEXPORT int jl_cpu_cores(void);
JL_DLLEXPORT long jl_getpagesize(void);
JL_DLLEXPORT long jl_getallocationgranularity(void);
JL_DLLEXPORT int jl_is_debugbuild(void);
JL_DLLEXPORT jl_sym_t *jl_get_UNAME(void);
JL_DLLEXPORT jl_sym_t *jl_get_ARCH(void);

// environment entries
JL_DLLEXPORT jl_value_t *jl_environ(int i);

// throwing common exceptions
JL_DLLEXPORT void JL_NORETURN jl_error(const char *str);
JL_DLLEXPORT void JL_NORETURN jl_errorf(const char *fmt, ...);
JL_DLLEXPORT void JL_NORETURN jl_exceptionf(jl_datatype_t *ty,
                                            const char *fmt, ...);
JL_DLLEXPORT void JL_NORETURN jl_too_few_args(const char *fname, int min);
JL_DLLEXPORT void JL_NORETURN jl_too_many_args(const char *fname, int max);
JL_DLLEXPORT void JL_NORETURN jl_type_error(const char *fname,
                                            jl_value_t *expected,
                                            jl_value_t *got);
JL_DLLEXPORT void JL_NORETURN jl_type_error_rt(const char *fname,
                                               const char *context,
                                               jl_value_t *ty, jl_value_t *got);
JL_DLLEXPORT void JL_NORETURN jl_undefined_var_error(jl_sym_t *var);
JL_DLLEXPORT void JL_NORETURN jl_bounds_error(jl_value_t *v, jl_value_t *t);
JL_DLLEXPORT void JL_NORETURN jl_bounds_error_v(jl_value_t *v,
                                                jl_value_t **idxs, size_t nidxs);
JL_DLLEXPORT void JL_NORETURN jl_bounds_error_int(jl_value_t *v, size_t i);
JL_DLLEXPORT void JL_NORETURN jl_bounds_error_tuple_int(jl_value_t **v,
                                                        size_t nv, size_t i);
JL_DLLEXPORT void JL_NORETURN jl_bounds_error_unboxed_int(void *v, jl_value_t *vt, size_t i);
JL_DLLEXPORT void JL_NORETURN jl_bounds_error_ints(jl_value_t *v, size_t *idxs, size_t nidxs);
JL_DLLEXPORT void JL_NORETURN jl_eof_error(void);
JL_DLLEXPORT jl_value_t *jl_exception_occurred(void);
JL_DLLEXPORT void jl_exception_clear(void);

#define JL_NARGS(fname, min, max)                               \
    if (nargs < min) jl_too_few_args(#fname, min);              \
    else if (nargs > max) jl_too_many_args(#fname, max);

#define JL_NARGSV(fname, min)                           \
    if (nargs < min) jl_too_few_args(#fname, min);

#define JL_TYPECHK(fname, type, v)                                      \
    if (!jl_is_##type(v)) {                                             \
        jl_type_error(#fname, (jl_value_t*)jl_##type##_type, (v));      \
    }
#define JL_TYPECHKS(fname, type, v)                                     \
    if (!jl_is_##type(v)) {                                             \
        jl_type_error(fname, (jl_value_t*)jl_##type##_type, (v));       \
    }

// initialization functions
typedef enum {
    JL_IMAGE_CWD = 0,
    JL_IMAGE_JULIA_HOME = 1,
    //JL_IMAGE_LIBJULIA = 2,
} JL_IMAGE_SEARCH;
JL_DLLEXPORT void julia_init(JL_IMAGE_SEARCH rel);
JL_DLLEXPORT void jl_init(const char *julia_home_dir);
JL_DLLEXPORT void jl_init_with_image(const char *julia_home_dir,
                                     const char *image_relative_path);
JL_DLLEXPORT int jl_is_initialized(void);
JL_DLLEXPORT void jl_atexit_hook(int status);
JL_DLLEXPORT void JL_NORETURN jl_exit(int status);

JL_DLLEXPORT int jl_deserialize_verify_header(ios_t *s);
JL_DLLEXPORT void jl_preload_sysimg_so(const char *fname);
JL_DLLEXPORT ios_t *jl_create_system_image(void);
JL_DLLEXPORT void jl_save_system_image(const char *fname);
JL_DLLEXPORT void jl_restore_system_image(const char *fname);
JL_DLLEXPORT void jl_restore_system_image_data(const char *buf, size_t len);
JL_DLLEXPORT int jl_save_incremental(const char *fname, jl_array_t *worklist);
JL_DLLEXPORT jl_value_t *jl_restore_incremental(const char *fname);
JL_DLLEXPORT jl_value_t *jl_restore_incremental_from_buf(const char *buf, size_t sz);

// front end interface
JL_DLLEXPORT jl_value_t *jl_parse_input_line(const char *str, size_t len,
                                             const char *filename, size_t filename_len);
JL_DLLEXPORT jl_value_t *jl_parse_string(const char *str, size_t len,
                                         int pos0, int greedy);
JL_DLLEXPORT int jl_parse_depwarn(int warn);
JL_DLLEXPORT jl_value_t *jl_load_file_string(const char *text, size_t len,
                                             char *filename);
JL_DLLEXPORT jl_value_t *jl_expand(jl_value_t *expr);
JL_DLLEXPORT jl_value_t *jl_eval_string(const char *str);

// external libraries
enum JL_RTLD_CONSTANT {
     JL_RTLD_LOCAL=1U,
     JL_RTLD_GLOBAL=2U,
     JL_RTLD_LAZY=4U,
     JL_RTLD_NOW=8U,
     /* Linux/glibc and MacOS X: */
     JL_RTLD_NODELETE=16U,
     JL_RTLD_NOLOAD=32U,
     /* Linux/glibc: */
     JL_RTLD_DEEPBIND=64U,
     /* MacOS X 10.5+: */
     JL_RTLD_FIRST=128U
};
#define JL_RTLD_DEFAULT (JL_RTLD_LAZY | JL_RTLD_DEEPBIND)

typedef void *jl_uv_libhandle; // compatible with dlopen (void*) / LoadLibrary (HMODULE)
JL_DLLEXPORT jl_uv_libhandle jl_load_dynamic_library(const char *fname, unsigned flags);
JL_DLLEXPORT jl_uv_libhandle jl_load_dynamic_library_e(const char *fname, unsigned flags);
JL_DLLEXPORT jl_uv_libhandle jl_dlopen(const char *filename, unsigned flags);
JL_DLLEXPORT int jl_dlclose(jl_uv_libhandle handle);
JL_DLLEXPORT void *jl_dlsym_e(jl_uv_libhandle handle, const char *symbol);
JL_DLLEXPORT void *jl_dlsym(jl_uv_libhandle handle, const char *symbol);

#if defined(__linux__) || defined(__FreeBSD__)
JL_DLLEXPORT const char *jl_lookup_soname(const char *pfx, size_t n);
#endif

// compiler
JL_DLLEXPORT jl_value_t *jl_toplevel_eval(jl_value_t *v);
JL_DLLEXPORT jl_value_t *jl_toplevel_eval_in(jl_module_t *m, jl_value_t *ex);
JL_DLLEXPORT jl_value_t *jl_load(const char *fname);
JL_DLLEXPORT jl_value_t *jl_interpret_toplevel_expr_in(jl_module_t *m, jl_value_t *e,
                                                       jl_lambda_info_t *lam);
JL_DLLEXPORT jl_module_t *jl_base_relative_to(jl_module_t *m);

// tracing
JL_DLLEXPORT void jl_trace_method(jl_method_t *m);
JL_DLLEXPORT void jl_untrace_method(jl_method_t *m);
JL_DLLEXPORT void jl_trace_linfo(jl_lambda_info_t *linfo);
JL_DLLEXPORT void jl_untrace_linfo(jl_lambda_info_t *linfo);
JL_DLLEXPORT void jl_register_linfo_tracer(void (*callback)(jl_lambda_info_t *tracee));
JL_DLLEXPORT void jl_register_method_tracer(void (*callback)(jl_lambda_info_t *tracee));
JL_DLLEXPORT void jl_register_newmeth_tracer(void (*callback)(jl_method_t *tracee));

// AST access
JL_DLLEXPORT int jl_is_rest_arg(jl_value_t *ex);

JL_DLLEXPORT jl_value_t *jl_copy_ast(jl_value_t *expr);

JL_DLLEXPORT jl_array_t *jl_compress_ast(jl_lambda_info_t *li, jl_array_t *ast);
JL_DLLEXPORT jl_array_t *jl_uncompress_ast(jl_lambda_info_t *li, jl_array_t *data);

JL_DLLEXPORT int jl_is_operator(char *sym);
JL_DLLEXPORT int jl_operator_precedence(char *sym);

STATIC_INLINE int jl_vinfo_sa(uint8_t vi)
{
    return (vi&16)!=0;
}

STATIC_INLINE int jl_vinfo_usedundef(uint8_t vi)
{
    return (vi&32)!=0;
}

// calling into julia ---------------------------------------------------------

JL_DLLEXPORT jl_value_t *jl_apply_generic(jl_value_t **args, uint32_t nargs);
JL_DLLEXPORT jl_value_t *jl_invoke(jl_lambda_info_t *meth, jl_value_t **args, uint32_t nargs);

STATIC_INLINE
jl_value_t *jl_apply(jl_value_t **args, uint32_t nargs)
{
    return jl_apply_generic(args, nargs);
}

JL_DLLEXPORT jl_value_t *jl_call(jl_function_t *f, jl_value_t **args, int32_t nargs);
JL_DLLEXPORT jl_value_t *jl_call0(jl_function_t *f);
JL_DLLEXPORT jl_value_t *jl_call1(jl_function_t *f, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_call2(jl_function_t *f, jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_call3(jl_function_t *f, jl_value_t *a,
                                  jl_value_t *b, jl_value_t *c);

// interfacing with Task runtime
JL_DLLEXPORT void jl_yield(void);

// async signal handling ------------------------------------------------------

JL_DLLEXPORT void jl_install_sigint_handler(void);
JL_DLLEXPORT void jl_sigatomic_begin(void);
JL_DLLEXPORT void jl_sigatomic_end(void);

// tasks and exceptions -------------------------------------------------------

typedef struct _jl_timing_block_t jl_timing_block_t;
// info describing an exception handler
typedef struct _jl_handler_t {
    jl_jmp_buf eh_ctx;
    jl_gcframe_t *gcstack;
    struct _jl_handler_t *prev;
    int8_t gc_state;
#ifdef JULIA_ENABLE_THREADING
    size_t locks_len;
#endif
    sig_atomic_t defer_signal;
    int finalizers_inhibited;
    jl_timing_block_t *timing_stack;
} jl_handler_t;

typedef struct _jl_task_t {
    JL_DATA_TYPE
    struct _jl_task_t *parent;
    jl_value_t *tls;
    jl_sym_t *state;
    jl_value_t *consumers;
    jl_value_t *donenotify;
    jl_value_t *result;
    jl_value_t *exception;
    jl_value_t *backtrace;
    jl_function_t *start;
    jl_jmp_buf ctx;
    size_t bufsz;
    void *stkbuf;

    size_t ssize;
    size_t started:1;

    // current exception handler
    jl_handler_t *eh;
    // saved gc stack top for context switches
    jl_gcframe_t *gcstack;
    // current module, or NULL if this task has not set one
    jl_module_t *current_module;

    // id of owning thread
    // does not need to be defined until the task runs
    int16_t tid;
#ifdef JULIA_ENABLE_THREADING
    // This is statically initialized when the task is not holding any locks
    arraylist_t locks;
#endif
    jl_timing_block_t *timing_stack;
} jl_task_t;

JL_DLLEXPORT jl_task_t *jl_new_task(jl_function_t *start, size_t ssize);
JL_DLLEXPORT jl_value_t *jl_switchto(jl_task_t *t, jl_value_t *arg);
JL_DLLEXPORT void JL_NORETURN jl_throw(jl_value_t *e);
JL_DLLEXPORT void JL_NORETURN jl_rethrow(void);
JL_DLLEXPORT void JL_NORETURN jl_rethrow_other(jl_value_t *e);
JL_DLLEXPORT void JL_NORETURN jl_no_exc_handler(jl_value_t *e);

#ifdef JULIA_ENABLE_THREADING
static inline void jl_lock_frame_push(jl_mutex_t *lock)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    // For early bootstrap
    if (__unlikely(!ptls->current_task))
        return;
    arraylist_t *locks = &ptls->current_task->locks;
    size_t len = locks->len;
    if (__unlikely(len >= locks->max)) {
        arraylist_grow(locks, 1);
    }
    else {
        locks->len = len + 1;
    }
    locks->items[len] = (void*)lock;
}
static inline void jl_lock_frame_pop(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (__likely(ptls->current_task)) {
        ptls->current_task->locks.len--;
    }
}
#else
static inline void jl_lock_frame_push(jl_mutex_t *lock)
{
    (void)lock;
}
static inline void jl_lock_frame_pop(void)
{
}
#endif // ifndef JULIA_ENABLE_THREADING

STATIC_INLINE void jl_eh_restore_state(jl_handler_t *eh)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_task_t *current_task = ptls->current_task;
    // `eh` may not be `ptls->current_task->eh`. See `jl_pop_handler`
    // This function should **NOT** have any safepoint before the ones at the
    // end.
    sig_atomic_t old_defer_signal = ptls->defer_signal;
    int8_t old_gc_state = ptls->gc_state;
    current_task->eh = eh->prev;
    ptls->pgcstack = eh->gcstack;
#ifdef JULIA_ENABLE_THREADING
    arraylist_t *locks = &current_task->locks;
    if (locks->len > eh->locks_len) {
        for (size_t i = locks->len;i > eh->locks_len;i--)
            jl_mutex_unlock_nogc((jl_mutex_t*)locks->items[i - 1]);
        locks->len = eh->locks_len;
    }
#endif
    ptls->defer_signal = eh->defer_signal;
    ptls->gc_state = eh->gc_state;
    ptls->finalizers_inhibited = eh->finalizers_inhibited;
    if (old_gc_state && !eh->gc_state) {
        jl_gc_safepoint_(ptls);
    }
    if (old_defer_signal && !eh->defer_signal) {
        jl_sigint_safepoint(ptls);
    }
}

JL_DLLEXPORT void jl_enter_handler(jl_handler_t *eh);
JL_DLLEXPORT void jl_pop_handler(int n);

#if defined(_OS_WINDOWS_)
#if defined(_COMPILER_MINGW_)
int __attribute__ ((__nothrow__,__returns_twice__)) jl_setjmp(jmp_buf _Buf);
__declspec(noreturn) __attribute__ ((__nothrow__)) void jl_longjmp(jmp_buf _Buf,int _Value);
#else
int jl_setjmp(jmp_buf _Buf);
void jl_longjmp(jmp_buf _Buf,int _Value);
#endif
#define jl_setjmp_f jl_setjmp
#define jl_setjmp_name "jl_setjmp"
#define jl_setjmp(a,b) jl_setjmp(a)
#define jl_longjmp(a,b) jl_longjmp(a,b)
#else
// determine actual entry point name
#if defined(sigsetjmp)
#define jl_setjmp_f    __sigsetjmp
#define jl_setjmp_name "__sigsetjmp"
#else
#define jl_setjmp_f    sigsetjmp
#define jl_setjmp_name "sigsetjmp"
#endif
#define jl_setjmp(a,b) sigsetjmp(a,b)
#define jl_longjmp(a,b) siglongjmp(a,b)
#endif

#define JL_TRY                                                    \
    int i__tr, i__ca; jl_handler_t __eh;                          \
    jl_enter_handler(&__eh);                                      \
    if (!jl_setjmp(__eh.eh_ctx,0))                                \
        for (i__tr=1; i__tr; i__tr=0, jl_eh_restore_state(&__eh))

#define JL_EH_POP() jl_eh_restore_state(&__eh)

#ifdef _OS_WINDOWS_
#define JL_CATCH                                                \
    else                                                        \
        for (i__ca=1, jl_eh_restore_state(&__eh); i__ca; i__ca=0) \
            if (((jl_get_ptls_states()->exception_in_transit==jl_stackovf_exception) && _resetstkoflw()) || 1)
#else
#define JL_CATCH                                                \
    else                                                        \
        for (i__ca=1, jl_eh_restore_state(&__eh); i__ca; i__ca=0)
#endif

// I/O system -----------------------------------------------------------------

#define JL_STREAM uv_stream_t
#define JL_STDOUT jl_uv_stdout
#define JL_STDERR jl_uv_stderr
#define JL_STDIN  jl_uv_stdin

JL_DLLEXPORT void jl_run_event_loop(uv_loop_t *loop);
JL_DLLEXPORT int jl_run_once(uv_loop_t *loop);
JL_DLLEXPORT int jl_process_events(uv_loop_t *loop);

JL_DLLEXPORT uv_loop_t *jl_global_event_loop(void);

JL_DLLEXPORT void jl_close_uv(uv_handle_t *handle);

JL_DLLEXPORT int jl_tcp_bind(uv_tcp_t *handle, uint16_t port, uint32_t host,
                             unsigned int flags);

JL_DLLEXPORT int jl_sizeof_ios_t(void);

JL_DLLEXPORT jl_array_t *jl_takebuf_array(ios_t *s);
JL_DLLEXPORT jl_value_t *jl_takebuf_string(ios_t *s);
JL_DLLEXPORT void *jl_takebuf_raw(ios_t *s);
JL_DLLEXPORT jl_value_t *jl_readuntil(ios_t *s, uint8_t delim);

typedef struct {
    void *data;
    uv_loop_t *loop;
    uv_handle_type type;
    uv_file file;
} jl_uv_file_t;

#ifdef __GNUC__
#define _JL_FORMAT_ATTR(type, str, arg) \
    __attribute__((format(type, str, arg)))
#else
#define _JL_FORMAT_ATTR(type, str, arg)
#endif

JL_DLLEXPORT int jl_printf(uv_stream_t *s, const char *format, ...)
    _JL_FORMAT_ATTR(printf, 2, 3);
JL_DLLEXPORT int jl_vprintf(uv_stream_t *s, const char *format, va_list args)
    _JL_FORMAT_ATTR(printf, 2, 0);
JL_DLLEXPORT void jl_safe_printf(const char *str, ...)
    _JL_FORMAT_ATTR(printf, 1, 2);

extern JL_DLLEXPORT JL_STREAM *JL_STDIN;
extern JL_DLLEXPORT JL_STREAM *JL_STDOUT;
extern JL_DLLEXPORT JL_STREAM *JL_STDERR;

JL_DLLEXPORT JL_STREAM *jl_stdout_stream(void);
JL_DLLEXPORT JL_STREAM *jl_stdin_stream(void);
JL_DLLEXPORT JL_STREAM *jl_stderr_stream(void);

// showing and std streams
JL_DLLEXPORT void jl_show(jl_value_t *stream, jl_value_t *v);
JL_DLLEXPORT void jl_flush_cstdio(void);
JL_DLLEXPORT jl_value_t *jl_stdout_obj(void);
JL_DLLEXPORT jl_value_t *jl_stderr_obj(void);
JL_DLLEXPORT size_t jl_static_show(JL_STREAM *out, jl_value_t *v);
JL_DLLEXPORT size_t jl_static_show_func_sig(JL_STREAM *s, jl_value_t *type);
JL_DLLEXPORT void jlbacktrace(void);
// Mainly for debugging, use `void*` so that no type cast is needed in C++.
JL_DLLEXPORT void jl_(void *jl_value);

// julia options -----------------------------------------------------------
// NOTE: This struct needs to be kept in sync with JLOptions type in base/options.jl
typedef struct {
    int8_t quiet;
    const char *julia_home;
    const char *julia_bin;
    const char *eval;
    const char *print;
    const char *postboot;
    const char *load;
    const char *image_file;
    const char *cpu_target;
    int32_t nprocs;
    const char *machinefile;
    int8_t isinteractive;
    int8_t color;
    int8_t historyfile;
    int8_t startupfile;
    int8_t compile_enabled;
    int8_t code_coverage;
    int8_t malloc_log;
    int8_t opt_level;
    int8_t check_bounds;
    int8_t depwarn;
    int8_t can_inline;
    int8_t fast_math;
    const char *worker;
    int8_t handle_signals;
    int8_t use_precompiled;
    int8_t use_compilecache;
    const char *bindto;
    const char *outputbc;
    const char *outputo;
    const char *outputji;
    int8_t incremental;
    int8_t image_file_specified;
} jl_options_t;

extern JL_DLLEXPORT jl_options_t jl_options;

// Parse an argc/argv pair to extract general julia options, passing back out
// any arguments that should be passed on to the script.
JL_DLLEXPORT void jl_parse_opts(int *argcp, char ***argvp);

// Set julia-level ARGS array according to the arguments provided in
// argc/argv
JL_DLLEXPORT void jl_set_ARGS(int argc, char **argv);

JL_DLLEXPORT int jl_generating_output(void);

// Settings for code_coverage and malloc_log
// NOTE: if these numbers change, test/cmdlineargs.jl will have to be updated
#define JL_LOG_NONE 0
#define JL_LOG_USER 1
#define JL_LOG_ALL  2

#define JL_OPTIONS_CHECK_BOUNDS_DEFAULT 0
#define JL_OPTIONS_CHECK_BOUNDS_ON 1
#define JL_OPTIONS_CHECK_BOUNDS_OFF 2

#define JL_OPTIONS_COMPILE_DEFAULT 1
#define JL_OPTIONS_COMPILE_OFF 0
#define JL_OPTIONS_COMPILE_ON  1
#define JL_OPTIONS_COMPILE_ALL 2
#define JL_OPTIONS_COMPILE_MIN 3

#define JL_OPTIONS_COLOR_ON 1
#define JL_OPTIONS_COLOR_OFF 2

#define JL_OPTIONS_HISTORYFILE_ON 1
#define JL_OPTIONS_HISTORYFILE_OFF 0

#define JL_OPTIONS_STARTUPFILE_ON 1
#define JL_OPTIONS_STARTUPFILE_OFF 2

#define JL_OPTIONS_DEPWARN_OFF 0
#define JL_OPTIONS_DEPWARN_ON 1
#define JL_OPTIONS_DEPWARN_ERROR 2

#define JL_OPTIONS_FAST_MATH_ON 1
#define JL_OPTIONS_FAST_MATH_OFF 2
#define JL_OPTIONS_FAST_MATH_DEFAULT 0

#define JL_OPTIONS_HANDLE_SIGNALS_ON 1
#define JL_OPTIONS_HANDLE_SIGNALS_OFF 0

#define JL_OPTIONS_USE_PRECOMPILED_YES 1
#define JL_OPTIONS_USE_PRECOMPILED_NO 0

#define JL_OPTIONS_USE_COMPILECACHE_YES 1
#define JL_OPTIONS_USE_COMPILECACHE_NO 0

// Version information
#include <julia_version.h>

JL_DLLEXPORT extern int jl_ver_major(void);
JL_DLLEXPORT extern int jl_ver_minor(void);
JL_DLLEXPORT extern int jl_ver_patch(void);
JL_DLLEXPORT extern int jl_ver_is_release(void);
JL_DLLEXPORT extern const char *jl_ver_string(void);
JL_DLLEXPORT const char *jl_git_branch(void);
JL_DLLEXPORT const char *jl_git_commit(void);

// nullable struct representations
typedef struct {
    uint8_t isnull;
    double value;
} jl_nullable_float64_t;

typedef struct {
    uint8_t isnull;
    float value;
} jl_nullable_float32_t;

#define jl_current_module (jl_get_ptls_states()->current_module)
#define jl_current_task (jl_get_ptls_states()->current_task)
#define jl_root_task (jl_get_ptls_states()->root_task)
#define jl_exception_in_transit (jl_get_ptls_states()->exception_in_transit)
#define jl_task_arg_in_transit (jl_get_ptls_states()->task_arg_in_transit)

#ifdef __cplusplus
}
#endif

#endif
