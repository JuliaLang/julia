// This file is a part of Julia. License is MIT: https://julialang.org/license

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
#include "analyzer_annotations.h"

#include <setjmp.h>
#ifndef _OS_WINDOWS_
#  define jl_jmp_buf sigjmp_buf
#  if defined(_CPU_ARM_) || defined(_CPU_PPC_) || defined(_CPU_WASM_)
#    define MAX_ALIGN 8
#  elif defined(_CPU_AARCH64_)
// int128 is 16 bytes aligned on aarch64
#    define MAX_ALIGN 16
#  elif defined(_P64)
// Generically we assume MAX_ALIGN is sizeof(void*)
#    define MAX_ALIGN 8
#  else
#    define MAX_ALIGN 4
#  endif
#else
#  include "win32_ucontext.h"
#  define jl_jmp_buf jmp_buf
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
#  define JL_USED_FUNC __attribute__((used))
#  define JL_SECTION(name) __attribute__((section(name)))
#  define JL_THREAD_LOCAL __thread
#elif defined(_COMPILER_MICROSOFT_)
#  define JL_NORETURN __declspec(noreturn)
// This is the closest I can find for __attribute__((const))
#  define JL_CONST_FUNC __declspec(noalias)
// Does MSVC have this?
#  define JL_USED_FUNC
// TODO: Figure out what to do on MSVC
#  define JL_SECTION(x)
#  define JL_THREAD_LOCAL __declspec(threaD)
#else
#  define JL_NORETURN
#  define JL_CONST_FUNC
#  define JL_USED_FUNC
#  define JL_THREAD_LOCAL
#endif

#define container_of(ptr, type, member) \
    ((type *) ((char *)(ptr) - offsetof(type, member)))

typedef struct _jl_taggedvalue_t jl_taggedvalue_t;

#include "atomics.h"
#include "tls.h"
#include "julia_threads.h"
#include "julia_assert.h"

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

JL_EXTENSION struct _jl_taggedvalue_t {
    union {
        uintptr_t header;
        jl_taggedvalue_t *next;
        jl_value_t *type; // 16-byte aligned
        struct _jl_taggedvalue_bits bits;
    };
    // jl_value_t value;
};

#ifdef __clang_analyzer__
JL_DLLEXPORT jl_taggedvalue_t *_jl_astaggedvalue(jl_value_t *v JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
#define jl_astaggedvalue(v) _jl_astaggedvalue((jl_value_t*)(v))
jl_value_t *_jl_valueof(jl_taggedvalue_t *tv JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
#define jl_valueof(v) _jl_valueof((jl_taggedvalue_t*)(v))
JL_DLLEXPORT jl_value_t *_jl_typeof(jl_value_t *v JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
#define jl_typeof(v) _jl_typeof((jl_value_t*)(v))
#else
#define jl_astaggedvalue(v)                                             \
    ((jl_taggedvalue_t*)((char*)(v) - sizeof(jl_taggedvalue_t)))
#define jl_valueof(v)                                           \
    ((jl_value_t*)((char*)(v) + sizeof(jl_taggedvalue_t)))
#define jl_typeof(v)                                                    \
    ((jl_value_t*)(jl_astaggedvalue(v)->header & ~(uintptr_t)15))
#endif
static inline void jl_set_typeof(void *v, void *t) JL_NOTSAFEPOINT
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
      3 = has a pointer to the object that owns the data
    */
    uint16_t how:2;
    uint16_t ndims:9;
    uint16_t pooled:1;
    uint16_t ptrarray:1; // representation is pointer array
    uint16_t hasptr:1; // representation has embedded pointers
    uint16_t isshared:1; // data is shared by multiple Arrays
    uint16_t isaligned:1; // data allocated with memalign
} jl_array_flags_t;

JL_EXTENSION typedef struct {
    JL_DATA_TYPE
    void *data;
#ifdef STORE_ARRAY_LEN
    size_t length;
#endif
    jl_array_flags_t flags;
    uint16_t elsize;  // element size including alignment (dim 1 memory stride)
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
STATIC_INLINE int jl_array_ndimwords(uint32_t ndims) JL_NOTSAFEPOINT
{
    return (ndims < 3 ? 0 : ndims-2);
}

typedef struct _jl_datatype_t jl_tupletype_t;
struct _jl_code_instance_t;

// TypeMap is an implicitly defined type
// that can consist of any of the following nodes:
//   typedef TypeMap Union{TypeMapLevel, TypeMapEntry, Nothing}
// it forms a roughly tree-shaped structure, consisting of nodes of TypeMapLevels
// which split the tree when possible, for example based on the key into the tuple type at `offs`
// when key is a leaftype, (but only when the tree has enough entries for this to be
// more efficient than storing them sorted linearly)
// otherwise the leaf entries are stored sorted, linearly
typedef jl_value_t jl_typemap_t;

typedef jl_value_t *(jl_call_t)(jl_value_t*, jl_value_t**, uint32_t, struct _jl_code_instance_t*);
typedef jl_call_t *jl_callptr_t;

// "speccall" calling convention signatures.
// This describes some of the special ABI used by compiled julia functions.
JL_DLLEXPORT extern jl_call_t jl_fptr_args;
typedef jl_value_t *(*jl_fptr_args_t)(jl_value_t*, jl_value_t**, uint32_t);

JL_DLLEXPORT extern jl_call_t jl_fptr_const_return;

JL_DLLEXPORT extern jl_call_t jl_fptr_sparam;
typedef jl_value_t *(*jl_fptr_sparam_t)(jl_value_t*, jl_value_t**, uint32_t, jl_svec_t*);

JL_DLLEXPORT extern jl_call_t jl_fptr_interpret_call;

JL_EXTENSION typedef union {
    void* fptr;
    jl_fptr_args_t fptr1;
    // 2 constant
    jl_fptr_sparam_t fptr3;
    // 4 interpreter
} jl_generic_specptr_t;

typedef struct _jl_method_instance_t jl_method_instance_t;

typedef struct _jl_line_info_node_t {
    jl_value_t *method;
    jl_sym_t *file;
    intptr_t line;
    intptr_t inlined_at;
} jl_line_info_node_t;

// This type describes a single function body
typedef struct _jl_code_info_t {
    // ssavalue-indexed arrays of properties:
    jl_array_t *code;  // Any array of statements
    jl_value_t *codelocs; // Int32 array of indicies into the line table
    jl_value_t *ssavaluetypes; // types of ssa values (or count of them)
    jl_array_t *ssaflags; // flags associated with each statement:
        // 0 = inbounds
        // 1,2 = <reserved> inlinehint,always-inline,noinline
        // 3 = <reserved> strict-ieee (strictfp)
        // 4-6 = <unused>
        // 7 = has out-of-band info
    // miscellaneous data:
    jl_value_t *method_for_inference_limit_heuristics; // optional method used during inference
    jl_value_t *linetable; // Table of locations [TODO: make this volatile like slotnames]
    jl_array_t *slotnames; // names of local variables
    jl_array_t *slotflags;  // local var bit flags
    // the following are optional transient properties (not preserved by compression--as they typically get stored elsewhere):
    jl_value_t *slottypes; // inferred types of slots
    jl_value_t *rettype;
    jl_method_instance_t *parent; // context (optionally, if available, otherwise nothing)
    jl_value_t *edges; // forward edges to method instances that must be invalidated
    size_t min_world;
    size_t max_world;
    // various boolean properties:
    uint8_t inferred;
    uint8_t inlineable;
    uint8_t propagate_inbounds;
    uint8_t pure;
} jl_code_info_t;

// This type describes a single method definition, and stores data
// shared by the specializations of a function.
typedef struct _jl_method_t {
    JL_DATA_TYPE
    jl_sym_t *name;  // for error reporting
    struct _jl_module_t *module;
    jl_sym_t *file;
    int32_t line;
    size_t primary_world;
    size_t deleted_world;

    // method's type signature. redundant with TypeMapEntry->specTypes
    jl_value_t *sig;

    // list of potentially-ambiguous methods (nothing = none, Vector{Any} of TypeMapEntry otherwise)
    jl_value_t *ambig;
    // forward references to later items (typemap entries) which might sort before this one
    jl_value_t *resorted;

    // table of all jl_method_instance_t specializations we have
    jl_svec_t *specializations; // allocated as [hashable, ..., NULL, linear, ....]
    jl_array_t *speckeyset; // index lookup by hash into specializations

    jl_value_t *slot_syms; // compacted list of slot names (String)
    jl_value_t *source;  // original code template (jl_code_info_t, but may be compressed), null for builtins
    struct _jl_method_instance_t *unspecialized;  // unspecialized executable method instance, or null
    jl_value_t *generator;  // executable code-generating function if available
    jl_array_t *roots;  // pointers in generated code (shared to reduce memory), or null
    jl_svec_t *ccallable; // svec(rettype, sig) if a ccallable entry point is requested for this

    // cache of specializations of this method for invoke(), i.e.
    // cases where this method was called even though it was not necessarily
    // the most specific for the argument types.
    jl_typemap_t *invokes;

    int32_t nargs;
    int32_t called;        // bit flags: whether each of the first 8 arguments is called
    int32_t nospecialize;  // bit flags: which arguments should not be specialized
    int32_t nkw;           // # of leading arguments that are actually keyword arguments
                           // of another method.
    uint8_t isva;
    uint8_t pure;

// hidden fields:
    // lock for modifications to the method
    jl_mutex_t writelock;
} jl_method_t;

// This type is a placeholder to cache data for a specType signature specialization of a Method
// can can be used as a unique dictionary key representation of a call to a particular Method
// with a particular set of argument types
struct _jl_method_instance_t {
    JL_DATA_TYPE
    union {
        jl_value_t *value; // generic accessor
        struct _jl_module_t *module; // this is a toplevel thunk
        jl_method_t *method; // method this is specialized from
    } def; // pointer back to the context for this code
    jl_value_t *specTypes;  // argument types this was specialized for
    jl_svec_t *sparam_vals; // static parameter values, indexed by def.method->sparam_syms
    jl_value_t *uninferred; // cached uncompressed code, for generated functions, top-level thunks, or the interpreter
    jl_array_t *backedges; // list of method-instances which contain a call into this method-instance
    struct _jl_code_instance_t *cache;
    uint8_t inInference; // flags to tell if inference is running on this object
};

// This type represents an executable operation
typedef struct _jl_code_instance_t {
    JL_DATA_TYPE
    jl_method_instance_t *def; // method this is specialized from
    struct _jl_code_instance_t *next; // pointer to the next cache entry

    // world range for which this object is valid to use
    size_t min_world;
    size_t max_world;

    // inference state cache
    jl_value_t *rettype; // return type for fptr
    jl_value_t *rettype_const; // inferred constant return value, or null
    jl_value_t *inferred; // inferred jl_code_info_t, or jl_nothing, or null
    //TODO: jl_array_t *edges; // stored information about edges from this object
    //TODO: uint8_t absolute_max; // whether true max world is unknown

    // compilation state cache
    uint8_t isspecsig; // if specptr is a specialized function signature for specTypes->rettype
    jl_callptr_t invoke; // jlcall entry point
    jl_generic_specptr_t specptr; // private data for `jlcall entry point`
} jl_code_instance_t;

// all values are callable as Functions
typedef jl_value_t jl_function_t;

typedef struct {
    JL_DATA_TYPE
    jl_sym_t *name;
    jl_value_t *lb;   // lower bound
    jl_value_t *ub;   // upper bound
} jl_tvar_t;

// UnionAll type (iterated union over all values of a variable in certain bounds)
// written `body where lb<:var<:ub`
typedef struct {
    JL_DATA_TYPE
    jl_tvar_t *var;
    jl_value_t *body;
} jl_unionall_t;

// represents the "name" part of a DataType, describing the syntactic structure
// of a type and storing all data common to different instantiations of the type,
// including a cache for hash-consed allocation of DataType objects.
typedef struct {
    JL_DATA_TYPE
    jl_sym_t *name;
    struct _jl_module_t *module;
    jl_svec_t *names;  // field names
    // `wrapper` is either the only instantiation of the type (if no parameters)
    // or a UnionAll accepting parameters to make an instantiation.
    jl_value_t *wrapper;
    jl_svec_t *cache;        // sorted array
    jl_svec_t *linearcache;  // unsorted array
    intptr_t hash;
    struct _jl_methtable_t *mt;
    jl_array_t *partial;     // incomplete instantiations of this type
} jl_typename_t;

typedef struct {
    JL_DATA_TYPE
    jl_value_t *a;
    jl_value_t *b;
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
    uint32_t npointers; // number of pointers embedded inside
    int32_t first_ptr; // index of the first pointer (or -1)
    uint16_t alignment; // strictest alignment over all fields
    uint16_t haspadding : 1; // has internal undefined bytes
    uint16_t fielddesc_type : 2; // 0 -> 8, 1 -> 16, 2 -> 32
    // union {
    //     jl_fielddesc8_t field8[nfields];
    //     jl_fielddesc16_t field16[nfields];
    //     jl_fielddesc32_t field32[nfields];
    // };
    // union { // offsets relative to data start in words
    //     uint8_t ptr8[npointers];
    //     uint16_t ptr16[npointers];
    //     uint32_t ptr32[npointers];
    // };
} jl_datatype_layout_t;

typedef struct _jl_datatype_t {
    JL_DATA_TYPE
    jl_typename_t *name;
    struct _jl_datatype_t *super;
    jl_svec_t *parameters;
    jl_svec_t *types;
    jl_svec_t *names;
    jl_value_t *instance;  // for singletons
    const jl_datatype_layout_t *layout;
    int32_t size; // TODO: move to _jl_datatype_layout_t
    int32_t ninitialized;
    uint32_t hash;
    uint8_t abstract;
    uint8_t mutabl;
    // memoized properties
    uint8_t hasfreetypevars; // majority part of isconcrete computation
    uint8_t isconcretetype; // whether this type can have instances
    uint8_t isdispatchtuple; // aka isleaftupletype
    uint8_t isbitstype; // relevant query for C-api and type-parameters
    uint8_t zeroinit; // if one or more fields requires zero-initialization
    uint8_t isinlinealloc; // if this is allocated inline
    uint8_t has_concrete_subtype; // If clear, no value will have this datatype
} jl_datatype_t;

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
    uint8_t constp;
    uint8_t exportp:1;
    uint8_t imported:1;
    uint8_t deprecated:2; // 0=not deprecated, 1=renamed, 2=moved to another package
} jl_binding_t;

typedef struct {
    uint64_t hi;
    uint64_t lo;
} jl_uuid_t;

typedef struct _jl_module_t {
    JL_DATA_TYPE
    jl_sym_t *name;
    struct _jl_module_t *parent;
    // hidden fields:
    htable_t bindings;
    arraylist_t usings;  // modules with all bindings potentially imported
    uint64_t build_id;
    jl_uuid_t uuid;
    size_t primary_world;
    uint32_t counter;
    int32_t nospecialize;  // global bit flags: initialization for new methods
    int32_t optlevel;
    uint8_t istopmod;
    jl_mutex_t lock;
} jl_module_t;

// one Type-to-Value entry
typedef struct _jl_typemap_entry_t {
    JL_DATA_TYPE
    struct _jl_typemap_entry_t *next; // invasive linked list
    jl_tupletype_t *sig; // the type signature for this entry
    jl_tupletype_t *simplesig; // a simple signature for fast rejection
    jl_svec_t *guardsigs;
    size_t min_world;
    size_t max_world;
    union {
        jl_value_t *value; // generic accessor
        jl_method_instance_t *linfo; // [nullable] for guard entries
        jl_method_t *method;
    } func;
    // memoized properties of sig:
    int8_t isleafsig; // isleaftype(sig) & !any(isType, sig) : unsorted and very fast
    int8_t issimplesig; // all(isleaftype | isAny | isType | isVararg, sig) : sorted and fast
    int8_t va; // isVararg(sig)
} jl_typemap_entry_t;

// one level in a TypeMap tree
// indexed by key if it is a sublevel in an array
typedef struct _jl_typemap_level_t {
    JL_DATA_TYPE
    jl_array_t *arg1;
    jl_array_t *targ;
    jl_typemap_entry_t *linear; // jl_typemap_t * (but no more levels)
    jl_typemap_t *any; // type at offs is Any
} jl_typemap_level_t;

// contains the TypeMap for one Type
typedef struct _jl_methtable_t {
    JL_DATA_TYPE
    jl_sym_t *name; // sometimes a hack used by serialization to handle kwsorter
    jl_typemap_t *defs;
    jl_typemap_t *cache;
    intptr_t max_args;  // max # of non-vararg arguments in a signature
    jl_value_t *kwsorter;  // keyword argument sorter function
    jl_module_t *module; // used for incremental serialization to locate original binding
    jl_array_t *backedges;
    jl_mutex_t writelock;
    uint8_t offs;  // 0, or 1 to skip splitting typemap on first (function) argument
    uint8_t frozen; // whether this accepts adding new methods
} jl_methtable_t;

typedef struct {
    JL_DATA_TYPE
    jl_sym_t *head;
    jl_array_t *args;
} jl_expr_t;

// constants and type objects -------------------------------------------------

// kinds
extern JL_DLLEXPORT jl_datatype_t *jl_typeofbottom_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_datatype_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_uniontype_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_unionall_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_tvar_type JL_GLOBALLY_ROOTED;

extern JL_DLLEXPORT jl_datatype_t *jl_any_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_unionall_t *jl_type_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_unionall_t *jl_typetype_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_typename_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_typename_t *jl_type_typename JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_symbol_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_ssavalue_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_abstractslot_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_slotnumber_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_typedslot_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_argument_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_simplevector_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_typename_t *jl_tuple_typename JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_typename_t *jl_vecelement_typename JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_anytuple_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_emptytuple_type JL_GLOBALLY_ROOTED;
#define jl_tuple_type jl_anytuple_type
extern JL_DLLEXPORT jl_unionall_t *jl_anytuple_type_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_unionall_t *jl_vararg_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_typename_t *jl_vararg_typename JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_function_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_builtin_type JL_GLOBALLY_ROOTED;

extern JL_DLLEXPORT jl_value_t *jl_bottom_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_method_instance_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_code_instance_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_code_info_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_method_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_module_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_unionall_t *jl_abstractarray_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_unionall_t *jl_densearray_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_unionall_t *jl_array_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_typename_t *jl_array_typename JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_weakref_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_abstractstring_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_string_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_errorexception_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_argumenterror_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_loaderror_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_initerror_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_typeerror_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_methoderror_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_undefvarerror_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_lineinfonode_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_stackovf_exception JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_memory_exception JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_readonlymemory_exception JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_diverror_exception JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_undefref_exception JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_interrupt_exception JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_boundserror_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_an_empty_vec_any JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_an_empty_string JL_GLOBALLY_ROOTED;

extern JL_DLLEXPORT jl_datatype_t *jl_bool_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_char_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_int8_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_uint8_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_int16_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_uint16_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_int32_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_uint32_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_int64_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_uint64_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_float16_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_float32_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_float64_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_floatingpoint_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_number_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_void_type JL_GLOBALLY_ROOTED;  // deprecated
extern JL_DLLEXPORT jl_datatype_t *jl_nothing_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_signed_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_voidpointer_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_uint8pointer_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_unionall_t *jl_pointer_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_unionall_t *jl_llvmpointer_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_unionall_t *jl_ref_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_typename_t *jl_pointer_typename JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_typename_t *jl_llvmpointer_typename JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_typename_t *jl_namedtuple_typename JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_unionall_t *jl_namedtuple_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_task_type JL_GLOBALLY_ROOTED;

extern JL_DLLEXPORT jl_value_t *jl_array_uint8_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_array_any_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_array_symbol_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_array_int32_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_expr_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_globalref_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_linenumbernode_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_gotonode_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_gotoifnot_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_returnnode_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_phinode_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_pinode_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_phicnode_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_upsilonnode_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_quotenode_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_newvarnode_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_intrinsic_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_methtable_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_typemap_level_type JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_datatype_t *jl_typemap_entry_type JL_GLOBALLY_ROOTED;

extern JL_DLLEXPORT jl_svec_t *jl_emptysvec JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_emptytuple JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_true JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_false JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_value_t *jl_nothing JL_GLOBALLY_ROOTED;

// some important symbols
extern JL_DLLEXPORT jl_sym_t *jl_incomplete_sym;

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

#define JL_GC_ENCODE_PUSHARGS(n)   (((size_t)(n))<<2)
#define JL_GC_ENCODE_PUSH(n)       ((((size_t)(n))<<2)|1)

#ifdef __clang_analyzer__

// When running with the analyzer make these real function calls, that are
// easier to detect in the analyzer
extern void JL_GC_PUSH1(void *) JL_NOTSAFEPOINT;
extern void JL_GC_PUSH2(void *, void *) JL_NOTSAFEPOINT;
extern void JL_GC_PUSH3(void *, void *, void *)  JL_NOTSAFEPOINT;
extern void JL_GC_PUSH4(void *, void *, void *, void *)  JL_NOTSAFEPOINT;
extern void JL_GC_PUSH5(void *, void *, void *, void *, void *)  JL_NOTSAFEPOINT;
extern void JL_GC_PUSH6(void *, void *, void *, void *, void *, void *)  JL_NOTSAFEPOINT;
extern void _JL_GC_PUSHARGS(jl_value_t **, size_t) JL_NOTSAFEPOINT;
// This is necessary, because otherwise the analyzer considers this undefined
// behavior and terminates the exploration
#define JL_GC_PUSHARGS(rts_var, n)     \
  rts_var = (jl_value_t **)alloca(sizeof(void*) * (n)); \
  memset(rts_var, 0, sizeof(void*) * (n)); \
  _JL_GC_PUSHARGS(rts_var, (n));

extern void JL_GC_POP() JL_NOTSAFEPOINT;

#else

#define JL_GC_PUSH1(arg1)                                                                               \
  void *__gc_stkf[] = {(void*)JL_GC_ENCODE_PUSH(1), jl_pgcstack, arg1};                                 \
  jl_pgcstack = (jl_gcframe_t*)__gc_stkf;

#define JL_GC_PUSH2(arg1, arg2)                                                                         \
  void *__gc_stkf[] = {(void*)JL_GC_ENCODE_PUSH(2), jl_pgcstack, arg1, arg2};                           \
  jl_pgcstack = (jl_gcframe_t*)__gc_stkf;

#define JL_GC_PUSH3(arg1, arg2, arg3)                                                                   \
  void *__gc_stkf[] = {(void*)JL_GC_ENCODE_PUSH(3), jl_pgcstack, arg1, arg2, arg3};                     \
  jl_pgcstack = (jl_gcframe_t*)__gc_stkf;

#define JL_GC_PUSH4(arg1, arg2, arg3, arg4)                                                             \
  void *__gc_stkf[] = {(void*)JL_GC_ENCODE_PUSH(4), jl_pgcstack, arg1, arg2, arg3, arg4};               \
  jl_pgcstack = (jl_gcframe_t*)__gc_stkf;

#define JL_GC_PUSH5(arg1, arg2, arg3, arg4, arg5)                                                       \
  void *__gc_stkf[] = {(void*)JL_GC_ENCODE_PUSH(5), jl_pgcstack, arg1, arg2, arg3, arg4, arg5};         \
  jl_pgcstack = (jl_gcframe_t*)__gc_stkf;

#define JL_GC_PUSH6(arg1, arg2, arg3, arg4, arg5, arg6)                                                 \
  void *__gc_stkf[] = {(void*)JL_GC_ENCODE_PUSH(6), jl_pgcstack, arg1, arg2, arg3, arg4, arg5, arg6};   \
  jl_pgcstack = (jl_gcframe_t*)__gc_stkf;

#define JL_GC_PUSHARGS(rts_var,n)                                                                       \
  rts_var = ((jl_value_t**)alloca(((n)+2)*sizeof(jl_value_t*)))+2;                                      \
  ((void**)rts_var)[-2] = (void*)JL_GC_ENCODE_PUSHARGS(n);                                              \
  ((void**)rts_var)[-1] = jl_pgcstack;                                                                  \
  memset((void*)rts_var, 0, (n)*sizeof(jl_value_t*));                                                   \
  jl_pgcstack = (jl_gcframe_t*)&(((void**)rts_var)[-2])

#define JL_GC_POP() (jl_pgcstack = jl_pgcstack->prev)

#endif

JL_DLLEXPORT int jl_gc_enable(int on);
JL_DLLEXPORT int jl_gc_is_enabled(void);

typedef enum {
    JL_GC_AUTO = 0,         // use heuristics to determine the collection type
    JL_GC_FULL = 1,         // force a full collection
    JL_GC_INCREMENTAL = 2,  // force an incremental collection
} jl_gc_collection_t;

JL_DLLEXPORT void jl_gc_collect(jl_gc_collection_t);

JL_DLLEXPORT void jl_gc_add_finalizer(jl_value_t *v, jl_function_t *f);
JL_DLLEXPORT void jl_finalize(jl_value_t *o);
JL_DLLEXPORT jl_weakref_t *jl_gc_new_weakref(jl_value_t *value);
JL_DLLEXPORT jl_value_t *jl_gc_alloc_0w(void);
JL_DLLEXPORT jl_value_t *jl_gc_alloc_1w(void);
JL_DLLEXPORT jl_value_t *jl_gc_alloc_2w(void);
JL_DLLEXPORT jl_value_t *jl_gc_alloc_3w(void);
JL_DLLEXPORT jl_value_t *jl_gc_allocobj(size_t sz);
JL_DLLEXPORT void *jl_malloc_stack(size_t *bufsz, struct _jl_task_t *owner) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_free_stack(void *stkbuf, size_t bufsz);
JL_DLLEXPORT void jl_gc_use(jl_value_t *a);

JL_DLLEXPORT void jl_clear_malloc_data(void);

// GC write barriers
JL_DLLEXPORT void jl_gc_queue_root(jl_value_t *root) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_gc_queue_multiroot(jl_value_t *root, jl_value_t *stored) JL_NOTSAFEPOINT;

STATIC_INLINE void jl_gc_wb(void *parent, void *ptr) JL_NOTSAFEPOINT
{
    // parent and ptr isa jl_value_t*
    if (__unlikely(jl_astaggedvalue(parent)->bits.gc == 3 && // parent is old and not in remset
                   (jl_astaggedvalue(ptr)->bits.gc & 1) == 0)) // ptr is young
        jl_gc_queue_root((jl_value_t*)parent);
}

STATIC_INLINE void jl_gc_wb_back(void *ptr) JL_NOTSAFEPOINT // ptr isa jl_value_t*
{
    // if ptr is old
    if (__unlikely(jl_astaggedvalue(ptr)->bits.gc == 3)) {
        jl_gc_queue_root((jl_value_t*)ptr);
    }
}

STATIC_INLINE void jl_gc_multi_wb(void *parent, jl_value_t *ptr) JL_NOTSAFEPOINT
{
    // ptr is an immutable object
    if (__likely(jl_astaggedvalue(parent)->bits.gc != 3))
        return; // parent is young or in remset
    if (__likely(jl_astaggedvalue(ptr)->bits.gc == 3))
        return; // ptr is old and not in remset (thus it does not point to young)
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(ptr);
    const jl_datatype_layout_t *ly = dt->layout;
    if (ly->npointers)
        jl_gc_queue_multiroot((jl_value_t*)parent, ptr);
}

JL_DLLEXPORT void *jl_gc_managed_malloc(size_t sz);
JL_DLLEXPORT void *jl_gc_managed_realloc(void *d, size_t sz, size_t oldsz,
                                         int isaligned, jl_value_t *owner);

// object accessors -----------------------------------------------------------

#define jl_svec_len(t)              (((jl_svec_t*)(t))->length)
#define jl_svec_set_len_unsafe(t,n) (((jl_svec_t*)(t))->length=(n))
#define jl_svec_data(t) ((jl_value_t**)((char*)(t) + sizeof(jl_svec_t)))

#ifdef __clang_analyzer__
STATIC_INLINE jl_value_t *jl_svecref(void *t JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT;
STATIC_INLINE jl_value_t *jl_svecset(
    void *t JL_ROOTING_ARGUMENT JL_PROPAGATES_ROOT,
    size_t i, void *x JL_ROOTED_ARGUMENT) JL_NOTSAFEPOINT;
#else
STATIC_INLINE jl_value_t *jl_svecref(void *t JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT
{
    assert(jl_typeis(t,jl_simplevector_type));
    assert(i < jl_svec_len(t));
    return jl_svec_data(t)[i];
}
STATIC_INLINE jl_value_t *jl_svecset(
    void *t JL_ROOTING_ARGUMENT JL_PROPAGATES_ROOT,
    size_t i, void *x JL_ROOTED_ARGUMENT) JL_NOTSAFEPOINT
{
    assert(jl_typeis(t,jl_simplevector_type));
    assert(i < jl_svec_len(t));
    jl_svec_data(t)[i] = (jl_value_t*)x;
    if (x) jl_gc_wb(t, x);
    return (jl_value_t*)x;
}
#endif

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

JL_DLLEXPORT char *jl_array_typetagdata(jl_array_t *a);

#ifdef __clang_analyzer__
jl_value_t **jl_array_ptr_data(jl_array_t *a JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
STATIC_INLINE jl_value_t *jl_array_ptr_ref(void *a JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT;
STATIC_INLINE jl_value_t *jl_array_ptr_set(
    void *a JL_ROOTING_ARGUMENT, size_t i,
    void *x JL_ROOTED_ARGUMENT) JL_NOTSAFEPOINT;
#else
#define jl_array_ptr_data(a)  ((jl_value_t**)((jl_array_t*)(a))->data)
STATIC_INLINE jl_value_t *jl_array_ptr_ref(void *a JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT
{
    assert(((jl_array_t*)a)->flags.ptrarray);
    assert(i < jl_array_len(a));
    return ((jl_value_t**)(jl_array_data(a)))[i];
}
STATIC_INLINE jl_value_t *jl_array_ptr_set(
    void *a JL_ROOTING_ARGUMENT, size_t i,
    void *x JL_ROOTED_ARGUMENT) JL_NOTSAFEPOINT
{
    assert(((jl_array_t*)a)->flags.ptrarray);
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
#endif

STATIC_INLINE uint8_t jl_array_uint8_ref(void *a, size_t i) JL_NOTSAFEPOINT
{
    assert(i < jl_array_len(a));
    assert(jl_typeis(a, jl_array_uint8_type));
    return ((uint8_t*)(jl_array_data(a)))[i];
}
STATIC_INLINE void jl_array_uint8_set(void *a, size_t i, uint8_t x) JL_NOTSAFEPOINT
{
    assert(i < jl_array_len(a));
    assert(jl_typeis(a, jl_array_uint8_type));
    ((uint8_t*)(jl_array_data(a)))[i] = x;
}

#define jl_exprarg(e,n) jl_array_ptr_ref(((jl_expr_t*)(e))->args, n)
#define jl_exprargset(e, n, v) jl_array_ptr_set(((jl_expr_t*)(e))->args, n, v)
#define jl_expr_nargs(e) jl_array_len(((jl_expr_t*)(e))->args)

#define jl_fieldref(s,i) jl_get_nth_field(((jl_value_t*)(s)),i)
#define jl_fieldref_noalloc(s,i) jl_get_nth_field_noalloc(((jl_value_t*)(s)),i)
#define jl_nfields(v)    jl_datatype_nfields(jl_typeof(v))

// Not using jl_fieldref to avoid allocations
#define jl_linenode_line(x) (((intptr_t*)(x))[0])
#define jl_linenode_file(x) (((jl_value_t**)(x))[1])
#define jl_slot_number(x) (((intptr_t*)(x))[0])
#define jl_typedslot_get_type(x) (((jl_value_t**)(x))[1])
#define jl_gotonode_label(x) (((intptr_t*)(x))[0])
#define jl_gotoifnot_cond(x) (((jl_value_t**)(x))[0])
#define jl_gotoifnot_label(x) (((intptr_t*)(x))[1])
#define jl_globalref_mod(s) (*(jl_module_t**)(s))
#define jl_globalref_name(s) (((jl_sym_t**)(s))[1])
#define jl_quotenode_value(x) (((jl_value_t**)x)[0])
#define jl_returnnode_value(x) (((jl_value_t**)x)[0])

#define jl_nparams(t)  jl_svec_len(((jl_datatype_t*)(t))->parameters)
#define jl_tparam0(t)  jl_svecref(((jl_datatype_t*)(t))->parameters, 0)
#define jl_tparam1(t)  jl_svecref(((jl_datatype_t*)(t))->parameters, 1)
#define jl_tparam(t,i) jl_svecref(((jl_datatype_t*)(t))->parameters, i)

// get a pointer to the data in a datatype
#define jl_data_ptr(v)  ((jl_value_t**)v)

#define jl_string_data(s) ((char*)s + sizeof(void*))
#define jl_string_len(s)  (*(size_t*)s)

#define jl_gf_mtable(f) (((jl_datatype_t*)jl_typeof(f))->name->mt)
#define jl_gf_name(f)   (jl_gf_mtable(f)->name)

// struct type info
JL_DLLEXPORT jl_svec_t *jl_compute_fieldtypes(jl_datatype_t *st JL_PROPAGATES_ROOT, void *stack);
#define jl_get_fieldtypes(st) ((st)->types ? (st)->types : jl_compute_fieldtypes((st), NULL))
STATIC_INLINE jl_svec_t *jl_field_names(jl_datatype_t *st) JL_NOTSAFEPOINT
{
    jl_svec_t *names = st->names;
    if (!names)
        names = st->name->names;
    return names;
}
STATIC_INLINE jl_sym_t *jl_field_name(jl_datatype_t *st, size_t i) JL_NOTSAFEPOINT
{
    return (jl_sym_t*)jl_svecref(jl_field_names(st), i);
}
STATIC_INLINE jl_value_t *jl_field_type(jl_datatype_t *st JL_PROPAGATES_ROOT, size_t i)
{
    return jl_svecref(jl_get_fieldtypes(st), i);
}
STATIC_INLINE jl_value_t *jl_field_type_concrete(jl_datatype_t *st JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT
{
    assert(st->types);
    return jl_svecref(st->types, i);
}

#define jl_datatype_size(t)    (((jl_datatype_t*)t)->size)
#define jl_datatype_align(t)   (((jl_datatype_t*)t)->layout->alignment)
#define jl_datatype_nbits(t)   ((((jl_datatype_t*)t)->size)*8)
#define jl_datatype_nfields(t) (((jl_datatype_t*)(t))->layout->nfields)
#define jl_datatype_isinlinealloc(t) (((jl_datatype_t *)(t))->isinlinealloc)

// inline version with strong type check to detect typos in a `->name` chain
STATIC_INLINE char *jl_symbol_name_(jl_sym_t *s) JL_NOTSAFEPOINT
{
    return (char*)s + LLT_ALIGN(sizeof(jl_sym_t), sizeof(void*));
}
#define jl_symbol_name(s) jl_symbol_name_(s)

static inline uint32_t jl_fielddesc_size(int8_t fielddesc_type) JL_NOTSAFEPOINT
{
    return 2 << fielddesc_type;
    //if (fielddesc_type == 0) {
    //    return sizeof(jl_fielddesc8_t);
    //}
    //else if (fielddesc_type == 1) {
    //    return sizeof(jl_fielddesc16_t);
    //}
    //else {
    //    return sizeof(jl_fielddesc32_t);
    //}
}

#define jl_dt_layout_fields(d) ((const char*)(d) + sizeof(jl_datatype_layout_t))
static inline const char *jl_dt_layout_ptrs(const jl_datatype_layout_t *l) JL_NOTSAFEPOINT
{
    return jl_dt_layout_fields(l) + jl_fielddesc_size(l->fielddesc_type) * l->nfields;
}

#define DEFINE_FIELD_ACCESSORS(f)                                             \
    static inline uint32_t jl_field_##f(jl_datatype_t *st,                    \
                                        int i) JL_NOTSAFEPOINT                \
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
#undef DEFINE_FIELD_ACCESSORS

static inline int jl_field_isptr(jl_datatype_t *st, int i) JL_NOTSAFEPOINT
{
    const jl_datatype_layout_t *ly = st->layout;
    assert(i >= 0 && (size_t)i < ly->nfields);
    return ((const jl_fielddesc8_t*)(jl_dt_layout_fields(ly) + jl_fielddesc_size(ly->fielddesc_type) * i))->isptr;
}

static inline uint32_t jl_ptr_offset(jl_datatype_t *st, int i) JL_NOTSAFEPOINT
{
    const jl_datatype_layout_t *ly = st->layout;
    assert(i >= 0 && (size_t)i < ly->npointers);
    const void *ptrs = jl_dt_layout_ptrs(ly);
    if (ly->fielddesc_type == 0) {
        return ((const uint8_t*)ptrs)[i];
    }
    else if (ly->fielddesc_type == 1) {
        return ((const uint16_t*)ptrs)[i];
    }
    else {
        assert(ly->fielddesc_type == 2);
        return ((const uint32_t*)ptrs)[i];
    }
}

static inline int jl_is_layout_opaque(const jl_datatype_layout_t *l) JL_NOTSAFEPOINT
{
    return l->nfields == 0 && l->npointers > 0;
}

// basic predicates -----------------------------------------------------------
#define jl_is_nothing(v)     (((jl_value_t*)(v)) == ((jl_value_t*)jl_nothing))
#define jl_is_tuple(v)       (((jl_datatype_t*)jl_typeof(v))->name == jl_tuple_typename)
#define jl_is_namedtuple(v)  (((jl_datatype_t*)jl_typeof(v))->name == jl_namedtuple_typename)
#define jl_is_svec(v)        jl_typeis(v,jl_simplevector_type)
#define jl_is_simplevector(v) jl_is_svec(v)
#define jl_is_datatype(v)    jl_typeis(v,jl_datatype_type)
#define jl_is_mutable(t)     (((jl_datatype_t*)t)->mutabl)
#define jl_is_mutable_datatype(t) (jl_is_datatype(t) && (((jl_datatype_t*)t)->mutabl))
#define jl_is_immutable(t)   (!((jl_datatype_t*)t)->mutabl)
#define jl_is_immutable_datatype(t) (jl_is_datatype(t) && (!((jl_datatype_t*)t)->mutabl))
#define jl_is_uniontype(v)   jl_typeis(v,jl_uniontype_type)
#define jl_is_typevar(v)     jl_typeis(v,jl_tvar_type)
#define jl_is_unionall(v)    jl_typeis(v,jl_unionall_type)
#define jl_is_typename(v)    jl_typeis(v,jl_typename_type)
#define jl_is_int8(v)        jl_typeis(v,jl_int8_type)
#define jl_is_int16(v)       jl_typeis(v,jl_int16_type)
#define jl_is_int32(v)       jl_typeis(v,jl_int32_type)
#define jl_is_int64(v)       jl_typeis(v,jl_int64_type)
#define jl_is_uint8(v)       jl_typeis(v,jl_uint8_type)
#define jl_is_uint16(v)      jl_typeis(v,jl_uint16_type)
#define jl_is_uint32(v)      jl_typeis(v,jl_uint32_type)
#define jl_is_uint64(v)      jl_typeis(v,jl_uint64_type)
#define jl_is_bool(v)        jl_typeis(v,jl_bool_type)
#define jl_is_symbol(v)      jl_typeis(v,jl_symbol_type)
#define jl_is_ssavalue(v)    jl_typeis(v,jl_ssavalue_type)
#define jl_is_slot(v)        (jl_typeis(v,jl_slotnumber_type) || jl_typeis(v,jl_typedslot_type))
#define jl_is_expr(v)        jl_typeis(v,jl_expr_type)
#define jl_is_globalref(v)   jl_typeis(v,jl_globalref_type)
#define jl_is_gotonode(v)    jl_typeis(v,jl_gotonode_type)
#define jl_is_gotoifnot(v)   jl_typeis(v,jl_gotoifnot_type)
#define jl_is_returnnode(v)  jl_typeis(v,jl_returnnode_type)
#define jl_is_argument(v)    jl_typeis(v,jl_argument_type)
#define jl_is_pinode(v)      jl_typeis(v,jl_pinode_type)
#define jl_is_phinode(v)     jl_typeis(v,jl_phinode_type)
#define jl_is_phicnode(v)    jl_typeis(v,jl_phicnode_type)
#define jl_is_upsilonnode(v) jl_typeis(v,jl_upsilonnode_type)
#define jl_is_quotenode(v)   jl_typeis(v,jl_quotenode_type)
#define jl_is_newvarnode(v)  jl_typeis(v,jl_newvarnode_type)
#define jl_is_linenode(v)    jl_typeis(v,jl_linenumbernode_type)
#define jl_is_method_instance(v) jl_typeis(v,jl_method_instance_type)
#define jl_is_code_instance(v) jl_typeis(v,jl_code_instance_type)
#define jl_is_code_info(v)   jl_typeis(v,jl_code_info_type)
#define jl_is_method(v)      jl_typeis(v,jl_method_type)
#define jl_is_module(v)      jl_typeis(v,jl_module_type)
#define jl_is_mtable(v)      jl_typeis(v,jl_methtable_type)
#define jl_is_task(v)        jl_typeis(v,jl_task_type)
#define jl_is_string(v)      jl_typeis(v,jl_string_type)
#define jl_is_cpointer(v)    jl_is_cpointer_type(jl_typeof(v))
#define jl_is_pointer(v)     jl_is_cpointer_type(jl_typeof(v))
#define jl_is_uint8pointer(v)jl_typeis(v,jl_uint8pointer_type)
#define jl_is_llvmpointer(v) jl_typeis(v,jl_llvmpointer_type)
#define jl_is_intrinsic(v)   jl_typeis(v,jl_intrinsic_type)
#define jl_array_isbitsunion(a) (!(((jl_array_t*)(a))->flags.ptrarray) && jl_is_uniontype(jl_tparam0(jl_typeof(a))))

JL_DLLEXPORT int jl_subtype(jl_value_t *a, jl_value_t *b);

STATIC_INLINE int jl_is_kind(jl_value_t *v) JL_NOTSAFEPOINT
{
    return (v==(jl_value_t*)jl_uniontype_type || v==(jl_value_t*)jl_datatype_type ||
            v==(jl_value_t*)jl_unionall_type || v==(jl_value_t*)jl_typeofbottom_type);
}

STATIC_INLINE int jl_is_type(jl_value_t *v) JL_NOTSAFEPOINT
{
    return jl_is_kind(jl_typeof(v));
}

STATIC_INLINE int jl_is_primitivetype(void *v) JL_NOTSAFEPOINT
{
    return (jl_is_datatype(v) && jl_is_immutable(v) &&
            ((jl_datatype_t*)(v))->layout &&
            jl_datatype_nfields(v) == 0 &&
            jl_datatype_size(v) > 0);
}

STATIC_INLINE int jl_is_structtype(void *v) JL_NOTSAFEPOINT
{
    return (jl_is_datatype(v) &&
            !((jl_datatype_t*)(v))->abstract &&
            !jl_is_primitivetype(v));
}

STATIC_INLINE int jl_isbits(void *t) JL_NOTSAFEPOINT // corresponding to isbits() in julia
{
    return (jl_is_datatype(t) && ((jl_datatype_t*)t)->isbitstype);
}

STATIC_INLINE int jl_is_datatype_singleton(jl_datatype_t *d) JL_NOTSAFEPOINT
{
    return (d->instance != NULL);
}

STATIC_INLINE int jl_is_abstracttype(void *v) JL_NOTSAFEPOINT
{
    return (jl_is_datatype(v) && ((jl_datatype_t*)(v))->abstract);
}

STATIC_INLINE int jl_is_array_type(void *t) JL_NOTSAFEPOINT
{
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == jl_array_typename);
}

STATIC_INLINE int jl_is_array(void *v) JL_NOTSAFEPOINT
{
    jl_value_t *t = jl_typeof(v);
    return jl_is_array_type(t);
}

STATIC_INLINE int jl_is_cpointer_type(jl_value_t *t) JL_NOTSAFEPOINT
{
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == ((jl_datatype_t*)jl_pointer_type->body)->name);
}

STATIC_INLINE int jl_is_llvmpointer_type(jl_value_t *t) JL_NOTSAFEPOINT
{
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == jl_llvmpointer_typename);
}

STATIC_INLINE int jl_is_abstract_ref_type(jl_value_t *t) JL_NOTSAFEPOINT
{
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == ((jl_datatype_t*)jl_ref_type->body)->name);
}

STATIC_INLINE int jl_is_tuple_type(void *t) JL_NOTSAFEPOINT
{
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == jl_tuple_typename);
}

STATIC_INLINE int jl_is_namedtuple_type(void *t) JL_NOTSAFEPOINT
{
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == jl_namedtuple_typename);
}

STATIC_INLINE int jl_is_vecelement_type(jl_value_t* t) JL_NOTSAFEPOINT
{
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == jl_vecelement_typename);
}

STATIC_INLINE int jl_is_type_type(jl_value_t *v) JL_NOTSAFEPOINT
{
    return (jl_is_datatype(v) &&
            ((jl_datatype_t*)(v))->name == ((jl_datatype_t*)jl_type_type->body)->name);
}

// object identity
JL_DLLEXPORT int jl_egal(jl_value_t *a JL_MAYBE_UNROOTED, jl_value_t *b JL_MAYBE_UNROOTED) JL_NOTSAFEPOINT;
JL_DLLEXPORT uintptr_t jl_object_id(jl_value_t *v) JL_NOTSAFEPOINT;

// type predicates and basic operations
JL_DLLEXPORT int jl_has_free_typevars(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT int jl_has_typevar(jl_value_t *t, jl_tvar_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT int jl_has_typevar_from_unionall(jl_value_t *t, jl_unionall_t *ua);
JL_DLLEXPORT int jl_subtype_env_size(jl_value_t *t);
JL_DLLEXPORT int jl_subtype_env(jl_value_t *x, jl_value_t *y, jl_value_t **env, int envsz);
JL_DLLEXPORT int jl_isa(jl_value_t *a, jl_value_t *t);
JL_DLLEXPORT int jl_types_equal(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT int jl_is_not_broken_subtype(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_type_union(jl_value_t **ts, size_t n);
JL_DLLEXPORT jl_value_t *jl_type_intersection(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT int jl_has_empty_intersection(jl_value_t *x, jl_value_t *y);
JL_DLLEXPORT jl_value_t *jl_type_unionall(jl_tvar_t *v, jl_value_t *body);
JL_DLLEXPORT const char *jl_typename_str(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT const char *jl_typeof_str(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT int jl_type_morespecific(jl_value_t *a, jl_value_t *b);
jl_value_t *jl_unwrap_unionall(jl_value_t *v JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
jl_value_t *jl_rewrap_unionall(jl_value_t *t, jl_value_t *u);

STATIC_INLINE int jl_is_dispatch_tupletype(jl_value_t *v) JL_NOTSAFEPOINT
{
    return jl_is_datatype(v) && ((jl_datatype_t*)v)->isdispatchtuple;
}

STATIC_INLINE int jl_is_concrete_type(jl_value_t *v) JL_NOTSAFEPOINT
{
    return jl_is_datatype(v) && ((jl_datatype_t*)v)->isconcretetype;
}

JL_DLLEXPORT int jl_isa_compileable_sig(jl_tupletype_t *type, jl_method_t *definition);

// type constructors
JL_DLLEXPORT jl_typename_t *jl_new_typename_in(jl_sym_t *name, jl_module_t *inmodule);
JL_DLLEXPORT jl_tvar_t *jl_new_typevar(jl_sym_t *name, jl_value_t *lb, jl_value_t *ub);
JL_DLLEXPORT jl_value_t *jl_instantiate_unionall(jl_unionall_t *u, jl_value_t *p);
JL_DLLEXPORT jl_value_t *jl_apply_type(jl_value_t *tc, jl_value_t **params, size_t n);
JL_DLLEXPORT jl_value_t *jl_apply_type1(jl_value_t *tc, jl_value_t *p1);
JL_DLLEXPORT jl_value_t *jl_apply_type2(jl_value_t *tc, jl_value_t *p1, jl_value_t *p2);
JL_DLLEXPORT jl_tupletype_t *jl_apply_tuple_type(jl_svec_t *params);
JL_DLLEXPORT jl_tupletype_t *jl_apply_tuple_type_v(jl_value_t **p, size_t np);
JL_DLLEXPORT jl_datatype_t *jl_new_datatype(jl_sym_t *name,
                                            jl_module_t *module,
                                            jl_datatype_t *super,
                                            jl_svec_t *parameters,
                                            jl_svec_t *fnames, jl_svec_t *ftypes,
                                            int abstract, int mutabl,
                                            int ninitialized);
JL_DLLEXPORT jl_datatype_t *jl_new_primitivetype(jl_value_t *name,
                                                 jl_module_t *module,
                                                 jl_datatype_t *super,
                                                 jl_svec_t *parameters, size_t nbits);
jl_datatype_t *jl_new_abstracttype(jl_value_t *name, jl_module_t *module,
                                   jl_datatype_t *super, jl_svec_t *parameters);

// constructors
JL_DLLEXPORT jl_value_t *jl_new_bits(jl_value_t *bt, void *data);
JL_DLLEXPORT jl_value_t *jl_new_struct(jl_datatype_t *type, ...);
JL_DLLEXPORT jl_value_t *jl_new_structv(jl_datatype_t *type, jl_value_t **args, uint32_t na);
JL_DLLEXPORT jl_value_t *jl_new_structt(jl_datatype_t *type, jl_value_t *tup);
JL_DLLEXPORT jl_value_t *jl_new_struct_uninit(jl_datatype_t *type);
JL_DLLEXPORT jl_method_instance_t *jl_new_method_instance_uninit(void);
JL_DLLEXPORT jl_svec_t *jl_svec(size_t n, ...) JL_MAYBE_UNROOTED;
JL_DLLEXPORT jl_svec_t *jl_svec1(void *a);
JL_DLLEXPORT jl_svec_t *jl_svec2(void *a, void *b);
JL_DLLEXPORT jl_svec_t *jl_alloc_svec(size_t n);
JL_DLLEXPORT jl_svec_t *jl_alloc_svec_uninit(size_t n);
JL_DLLEXPORT jl_svec_t *jl_svec_copy(jl_svec_t *a);
JL_DLLEXPORT jl_svec_t *jl_svec_fill(size_t n, jl_value_t *x);
JL_DLLEXPORT jl_value_t *jl_tupletype_fill(size_t n, jl_value_t *v);
JL_DLLEXPORT jl_sym_t *jl_symbol(const char *str) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_sym_t *jl_symbol_lookup(const char *str) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_sym_t *jl_symbol_n(const char *str, size_t len) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_sym_t *jl_gensym(void);
JL_DLLEXPORT jl_sym_t *jl_tagged_gensym(const char *str, int32_t len);
JL_DLLEXPORT jl_sym_t *jl_get_root_symbol(void);
JL_DLLEXPORT jl_value_t *jl_generic_function_def(jl_sym_t *name,
                                                 jl_module_t *module,
                                                 jl_value_t **bp, jl_value_t *bp_owner,
                                                 jl_binding_t *bnd);
JL_DLLEXPORT void jl_method_def(jl_svec_t *argdata, jl_code_info_t *f, jl_module_t *module);
JL_DLLEXPORT jl_code_info_t *jl_code_for_staged(jl_method_instance_t *linfo);
JL_DLLEXPORT jl_code_info_t *jl_copy_code_info(jl_code_info_t *src);
JL_DLLEXPORT size_t jl_get_world_counter(void);
JL_DLLEXPORT jl_function_t *jl_get_kwsorter(jl_value_t *ty);
JL_DLLEXPORT jl_value_t *jl_box_bool(int8_t x) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_value_t *jl_box_int8(int8_t x) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_value_t *jl_box_uint8(uint8_t x) JL_NOTSAFEPOINT;
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
JL_DLLEXPORT jl_value_t *jl_box_uint8pointer(uint8_t *x);
JL_DLLEXPORT jl_value_t *jl_box_ssavalue(size_t x);
JL_DLLEXPORT jl_value_t *jl_box_slotnumber(size_t x);
JL_DLLEXPORT int8_t jl_unbox_bool(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT int8_t jl_unbox_int8(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint8_t jl_unbox_uint8(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT int16_t jl_unbox_int16(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint16_t jl_unbox_uint16(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT int32_t jl_unbox_int32(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint32_t jl_unbox_uint32(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT int64_t jl_unbox_int64(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint64_t jl_unbox_uint64(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT float jl_unbox_float32(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT double jl_unbox_float64(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT void *jl_unbox_voidpointer(jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint8_t *jl_unbox_uint8pointer(jl_value_t *v) JL_NOTSAFEPOINT;

JL_DLLEXPORT int jl_get_size(jl_value_t *val, size_t *pnt);

#ifdef _P64
#define jl_box_long(x)   jl_box_int64(x)
#define jl_box_ulong(x)  jl_box_uint64(x)
#define jl_unbox_long(x) jl_unbox_int64(x)
#define jl_unbox_ulong(x) jl_unbox_uint64(x)
#define jl_is_long(x)    jl_is_int64(x)
#define jl_is_ulong(x)   jl_is_uint64(x)
#define jl_long_type     jl_int64_type
#define jl_ulong_type    jl_uint64_type
#else
#define jl_box_long(x)   jl_box_int32(x)
#define jl_box_ulong(x)  jl_box_uint32(x)
#define jl_unbox_long(x) jl_unbox_int32(x)
#define jl_unbox_ulong(x) jl_unbox_uint32(x)
#define jl_is_long(x)    jl_is_int32(x)
#define jl_is_ulong(x)   jl_is_uint32(x)
#define jl_long_type     jl_int32_type
#define jl_ulong_type    jl_uint32_type
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

STATIC_INLINE int jl_is_vararg_type(jl_value_t *v) JL_NOTSAFEPOINT
{
    v = jl_unwrap_unionall(v);
    return (jl_is_datatype(v) &&
            ((jl_datatype_t*)(v))->name == jl_vararg_typename);
}

STATIC_INLINE jl_value_t *jl_unwrap_vararg(jl_value_t *v) JL_NOTSAFEPOINT
{
    return jl_tparam0(jl_unwrap_unionall(v));
}

STATIC_INLINE size_t jl_vararg_length(jl_value_t *v) JL_NOTSAFEPOINT
{
    assert(jl_is_vararg_type(v));
    jl_value_t *len = jl_tparam1(jl_unwrap_unionall(v));
    assert(jl_is_long(len));
    return jl_unbox_long(len);
}

STATIC_INLINE jl_vararg_kind_t jl_vararg_kind(jl_value_t *v) JL_NOTSAFEPOINT
{
    if (!jl_is_vararg_type(v))
        return JL_VARARG_NONE;
    jl_tvar_t *v1=NULL, *v2=NULL;
    if (jl_is_unionall(v)) {
        v1 = ((jl_unionall_t*)v)->var;
        v = ((jl_unionall_t*)v)->body;
        if (jl_is_unionall(v)) {
            v2 = ((jl_unionall_t*)v)->var;
            v = ((jl_unionall_t*)v)->body;
        }
    }
    assert(jl_is_datatype(v));
    jl_value_t *lenv = jl_tparam1(v);
    if (jl_is_long(lenv))
        return JL_VARARG_INT;
    if (jl_is_typevar(lenv) && lenv != (jl_value_t*)v1 && lenv != (jl_value_t*)v2)
        return JL_VARARG_BOUND;
    return JL_VARARG_UNBOUND;
}

STATIC_INLINE int jl_is_va_tuple(jl_datatype_t *t) JL_NOTSAFEPOINT
{
    assert(jl_is_tuple_type(t));
    size_t l = jl_svec_len(t->parameters);
    return (l>0 && jl_is_vararg_type(jl_tparam(t,l-1)));
}

STATIC_INLINE jl_vararg_kind_t jl_va_tuple_kind(jl_datatype_t *t) JL_NOTSAFEPOINT
{
    t = (jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)t);
    assert(jl_is_tuple_type(t));
    size_t l = jl_svec_len(t->parameters);
    if (l == 0)
        return JL_VARARG_NONE;
    return jl_vararg_kind(jl_tparam(t,l-1));
}

// structs
JL_DLLEXPORT int         jl_field_index(jl_datatype_t *t, jl_sym_t *fld, int err);
JL_DLLEXPORT jl_value_t *jl_get_nth_field(jl_value_t *v, size_t i);
// Like jl_get_nth_field above, but asserts if it needs to allocate
JL_DLLEXPORT jl_value_t *jl_get_nth_field_noalloc(jl_value_t *v JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_value_t *jl_get_nth_field_checked(jl_value_t *v, size_t i);
JL_DLLEXPORT void        jl_set_nth_field(jl_value_t *v, size_t i,
                                          jl_value_t *rhs) JL_NOTSAFEPOINT;
JL_DLLEXPORT int         jl_field_isdefined(jl_value_t *v, size_t i);
JL_DLLEXPORT jl_value_t *jl_get_field(jl_value_t *o, const char *fld);
JL_DLLEXPORT jl_value_t *jl_value_ptr(jl_value_t *a);
JL_DLLEXPORT int jl_islayout_inline(jl_value_t *eltype, size_t *fsz, size_t *al) JL_NOTSAFEPOINT;

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
JL_DLLEXPORT jl_value_t *jl_alloc_string(size_t len);
JL_DLLEXPORT jl_value_t *jl_array_to_string(jl_array_t *a);
JL_DLLEXPORT jl_array_t *jl_alloc_vec_any(size_t n);
JL_DLLEXPORT jl_value_t *jl_arrayref(jl_array_t *a, size_t i);  // 0-indexed
JL_DLLEXPORT jl_value_t *jl_ptrarrayref(jl_array_t *a JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT;  // 0-indexed
JL_DLLEXPORT void jl_arrayset(jl_array_t *a JL_ROOTING_ARGUMENT, jl_value_t *v JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED, size_t i);  // 0-indexed
JL_DLLEXPORT void jl_arrayunset(jl_array_t *a, size_t i);  // 0-indexed
JL_DLLEXPORT int jl_array_isassigned(jl_array_t *a, size_t i);  // 0-indexed
JL_DLLEXPORT void jl_array_grow_end(jl_array_t *a, size_t inc);
JL_DLLEXPORT void jl_array_del_end(jl_array_t *a, size_t dec);
JL_DLLEXPORT void jl_array_grow_beg(jl_array_t *a, size_t inc);
JL_DLLEXPORT void jl_array_del_beg(jl_array_t *a, size_t dec);
JL_DLLEXPORT void jl_array_sizehint(jl_array_t *a, size_t sz);
JL_DLLEXPORT void jl_array_ptr_1d_push(jl_array_t *a, jl_value_t *item);
JL_DLLEXPORT void jl_array_ptr_1d_append(jl_array_t *a, jl_array_t *a2);
JL_DLLEXPORT jl_value_t *jl_apply_array_type(jl_value_t *type, size_t dim);
// property access
JL_DLLEXPORT void *jl_array_ptr(jl_array_t *a);
JL_DLLEXPORT void *jl_array_eltype(jl_value_t *a);
JL_DLLEXPORT int jl_array_rank(jl_value_t *a);
JL_DLLEXPORT size_t jl_array_size(jl_value_t *a, int d);

// strings
JL_DLLEXPORT const char *jl_string_ptr(jl_value_t *s);

// modules and global variables
extern JL_DLLEXPORT jl_module_t *jl_main_module JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_module_t *jl_core_module JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_module_t *jl_base_module JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_module_t *jl_top_module JL_GLOBALLY_ROOTED;
JL_DLLEXPORT jl_module_t *jl_new_module(jl_sym_t *name);
JL_DLLEXPORT void jl_set_module_nospecialize(jl_module_t *self, int on);
JL_DLLEXPORT void jl_set_module_optlevel(jl_module_t *self, int lvl);
JL_DLLEXPORT int jl_get_module_optlevel(jl_module_t *m);
// get binding for reading
JL_DLLEXPORT jl_binding_t *jl_get_binding(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var);
JL_DLLEXPORT jl_binding_t *jl_get_binding_or_error(jl_module_t *m, jl_sym_t *var);
JL_DLLEXPORT jl_value_t *jl_module_globalref(jl_module_t *m, jl_sym_t *var);
// get binding for assignment
JL_DLLEXPORT jl_binding_t *jl_get_binding_wr(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var, int error);
JL_DLLEXPORT jl_binding_t *jl_get_binding_for_method_def(jl_module_t *m JL_PROPAGATES_ROOT,
                                                         jl_sym_t *var);
JL_DLLEXPORT int jl_boundp(jl_module_t *m, jl_sym_t *var);
JL_DLLEXPORT int jl_defines_or_exports_p(jl_module_t *m, jl_sym_t *var) JL_NOTSAFEPOINT;
JL_DLLEXPORT int jl_binding_resolved_p(jl_module_t *m, jl_sym_t *var) JL_NOTSAFEPOINT;
JL_DLLEXPORT int jl_is_const(jl_module_t *m, jl_sym_t *var);
JL_DLLEXPORT jl_value_t *jl_get_global(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var);
JL_DLLEXPORT void jl_set_global(jl_module_t *m JL_ROOTING_ARGUMENT, jl_sym_t *var, jl_value_t *val JL_ROOTED_ARGUMENT);
JL_DLLEXPORT void jl_set_const(jl_module_t *m JL_ROOTING_ARGUMENT, jl_sym_t *var, jl_value_t *val JL_ROOTED_ARGUMENT);
JL_DLLEXPORT void jl_checked_assignment(jl_binding_t *b JL_ROOTING_ARGUMENT, jl_value_t *rhs JL_ROOTED_ARGUMENT);
JL_DLLEXPORT void jl_declare_constant(jl_binding_t *b);
JL_DLLEXPORT void jl_module_using(jl_module_t *to, jl_module_t *from);
JL_DLLEXPORT void jl_module_use(jl_module_t *to, jl_module_t *from, jl_sym_t *s);
JL_DLLEXPORT void jl_module_import(jl_module_t *to, jl_module_t *from,
                                   jl_sym_t *s);
JL_DLLEXPORT void jl_module_export(jl_module_t *from, jl_sym_t *s);
JL_DLLEXPORT int jl_is_imported(jl_module_t *m, jl_sym_t *s);
JL_DLLEXPORT int jl_module_exports_p(jl_module_t *m, jl_sym_t *var) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_add_standard_imports(jl_module_t *m);
STATIC_INLINE jl_function_t *jl_get_function(jl_module_t *m, const char *name)
{
    return (jl_function_t*)jl_get_global(m, jl_symbol(name));
}
int jl_is_submodule(jl_module_t *child, jl_module_t *parent) JL_NOTSAFEPOINT;

// eq hash tables
JL_DLLEXPORT jl_array_t *jl_eqtable_put(jl_array_t *h, jl_value_t *key, jl_value_t *val, int *inserted);
JL_DLLEXPORT jl_value_t *jl_eqtable_get(jl_array_t *h, jl_value_t *key, jl_value_t *deflt) JL_NOTSAFEPOINT;
jl_value_t **jl_table_peek_bp(jl_array_t *a, jl_value_t *key) JL_NOTSAFEPOINT;

// system information
JL_DLLEXPORT int jl_errno(void);
JL_DLLEXPORT void jl_set_errno(int e);
JL_DLLEXPORT int32_t jl_stat(const char *path, char *statbuf);
JL_DLLEXPORT int jl_cpu_threads(void);
JL_DLLEXPORT long jl_getpagesize(void);
JL_DLLEXPORT long jl_getallocationgranularity(void);
JL_DLLEXPORT int jl_is_debugbuild(void);
JL_DLLEXPORT jl_sym_t *jl_get_UNAME(void);
JL_DLLEXPORT jl_sym_t *jl_get_ARCH(void);

// environment entries
JL_DLLEXPORT jl_value_t *jl_environ(int i);

// throwing common exceptions
JL_DLLEXPORT jl_value_t *jl_vexceptionf(jl_datatype_t *exception_type,
                                        const char *fmt, va_list args);
JL_DLLEXPORT void JL_NORETURN jl_error(const char *str);
JL_DLLEXPORT void JL_NORETURN jl_errorf(const char *fmt, ...);
JL_DLLEXPORT void JL_NORETURN jl_exceptionf(jl_datatype_t *ty,
                                            const char *fmt, ...);
JL_DLLEXPORT void JL_NORETURN jl_too_few_args(const char *fname, int min);
JL_DLLEXPORT void JL_NORETURN jl_too_many_args(const char *fname, int max);
JL_DLLEXPORT void JL_NORETURN jl_type_error(const char *fname,
                                            jl_value_t *expected,
                                            jl_value_t *got JL_MAYBE_UNROOTED);
JL_DLLEXPORT void JL_NORETURN jl_type_error_rt(const char *fname,
                                               const char *context,
                                               jl_value_t *ty JL_MAYBE_UNROOTED,
                                               jl_value_t *got JL_MAYBE_UNROOTED);
JL_DLLEXPORT void JL_NORETURN jl_undefined_var_error(jl_sym_t *var);
JL_DLLEXPORT void JL_NORETURN jl_bounds_error(jl_value_t *v JL_MAYBE_UNROOTED,
                                              jl_value_t *t JL_MAYBE_UNROOTED);
JL_DLLEXPORT void JL_NORETURN jl_bounds_error_v(jl_value_t *v JL_MAYBE_UNROOTED,
                                                jl_value_t **idxs, size_t nidxs);
JL_DLLEXPORT void JL_NORETURN jl_bounds_error_int(jl_value_t *v JL_MAYBE_UNROOTED,
                                                  size_t i);
JL_DLLEXPORT void JL_NORETURN jl_bounds_error_tuple_int(jl_value_t **v,
                                                        size_t nv, size_t i);
JL_DLLEXPORT void JL_NORETURN jl_bounds_error_unboxed_int(void *v, jl_value_t *vt, size_t i);
JL_DLLEXPORT void JL_NORETURN jl_bounds_error_ints(jl_value_t *v JL_MAYBE_UNROOTED,
                                                   size_t *idxs, size_t nidxs);
JL_DLLEXPORT void JL_NORETURN jl_eof_error(void);

// Return the exception currently being handled, or `jl_nothing`.
//
// The catch scope is determined dynamically so this works in functions called
// from a catch block.  The returned value is gc rooted until we exit the
// enclosing JL_CATCH.
// FIXME: Teach the static analyzer about this rather than using
// JL_GLOBALLY_ROOTED which is far too optimistic.
JL_DLLEXPORT jl_value_t *jl_current_exception(void) JL_GLOBALLY_ROOTED;
JL_DLLEXPORT jl_value_t *jl_exception_occurred(void);
JL_DLLEXPORT void jl_exception_clear(void) JL_NOTSAFEPOINT;

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
// this helps turn threading compilation mismatches into linker errors
#define julia_init julia_init__threading
#define jl_init jl_init__threading
#define jl_init_with_image jl_init_with_image__threading

JL_DLLEXPORT void julia_init(JL_IMAGE_SEARCH rel);
JL_DLLEXPORT void jl_init(void);
JL_DLLEXPORT void jl_init_with_image(const char *julia_bindir,
                                     const char *image_relative_path);
JL_DLLEXPORT const char *jl_get_default_sysimg_path(void);
JL_DLLEXPORT int jl_is_initialized(void);
JL_DLLEXPORT void jl_atexit_hook(int status);
JL_DLLEXPORT void JL_NORETURN jl_exit(int status);
JL_DLLEXPORT const char *jl_pathname_for_handle(void *handle);

JL_DLLEXPORT int jl_deserialize_verify_header(ios_t *s);
JL_DLLEXPORT void jl_preload_sysimg_so(const char *fname);
JL_DLLEXPORT void jl_set_sysimg_so(void *handle);
JL_DLLEXPORT ios_t *jl_create_system_image(void *);
JL_DLLEXPORT void jl_save_system_image(const char *fname);
JL_DLLEXPORT void jl_restore_system_image(const char *fname);
JL_DLLEXPORT void jl_restore_system_image_data(const char *buf, size_t len);
JL_DLLEXPORT int jl_save_incremental(const char *fname, jl_array_t *worklist);
JL_DLLEXPORT jl_value_t *jl_restore_incremental(const char *fname, jl_array_t *depmods);
JL_DLLEXPORT jl_value_t *jl_restore_incremental_from_buf(const char *buf, size_t sz, jl_array_t *depmods);

// parsing
JL_DLLEXPORT jl_value_t *jl_parse_all(const char *text, size_t text_len,
                                      const char *filename, size_t filename_len);
JL_DLLEXPORT jl_value_t *jl_parse_string(const char *text, size_t text_len,
                                         int offset, int greedy);
// lowering
JL_DLLEXPORT jl_value_t *jl_expand(jl_value_t *expr, jl_module_t *inmodule);
JL_DLLEXPORT jl_value_t *jl_expand_with_loc(jl_value_t *expr, jl_module_t *inmodule,
                                            const char *file, int line);
JL_DLLEXPORT jl_value_t *jl_expand_with_loc_warn(jl_value_t *expr, jl_module_t *inmodule,
                                                 const char *file, int line);
JL_DLLEXPORT jl_value_t *jl_expand_stmt(jl_value_t *expr, jl_module_t *inmodule);
JL_DLLEXPORT jl_value_t *jl_expand_stmt_with_loc(jl_value_t *expr, jl_module_t *inmodule,
                                                 const char *file, int line);
// deprecated; use jl_parse_all
JL_DLLEXPORT jl_value_t *jl_parse_input_line(const char *text, size_t text_len,
                                             const char *filename, size_t filename_len);

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
JL_DLLEXPORT jl_uv_libhandle jl_load_dynamic_library(const char *fname, unsigned flags, int throw_err);
JL_DLLEXPORT jl_uv_libhandle jl_dlopen(const char *filename, unsigned flags);
JL_DLLEXPORT int jl_dlclose(jl_uv_libhandle handle);
JL_DLLEXPORT int jl_dlsym(jl_uv_libhandle handle, const char *symbol, void ** value, int throw_err);

// evaluation
JL_DLLEXPORT jl_value_t *jl_toplevel_eval(jl_module_t *m, jl_value_t *v);
JL_DLLEXPORT jl_value_t *jl_toplevel_eval_in(jl_module_t *m, jl_value_t *ex);
// code loading (parsing + evaluation)
JL_DLLEXPORT jl_value_t *jl_eval_string(const char *str); // embedding interface
JL_DLLEXPORT jl_value_t *jl_load_file_string(const char *text, size_t len,
                                             char *filename, jl_module_t *module);
JL_DLLEXPORT jl_value_t *jl_load(jl_module_t *module, const char *fname);

JL_DLLEXPORT jl_module_t *jl_base_relative_to(jl_module_t *m JL_PROPAGATES_ROOT);

// tracing
JL_DLLEXPORT void jl_register_newmeth_tracer(void (*callback)(jl_method_t *tracee));

// AST access
JL_DLLEXPORT jl_value_t *jl_copy_ast(jl_value_t *expr JL_MAYBE_UNROOTED);

// IR representation
JL_DLLEXPORT jl_array_t *jl_compress_ir(jl_method_t *m, jl_code_info_t *code);
JL_DLLEXPORT jl_code_info_t *jl_uncompress_ir(jl_method_t *m, jl_code_instance_t *metadata, jl_array_t *data);
JL_DLLEXPORT uint8_t jl_ir_flag_inferred(jl_array_t *data) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint8_t jl_ir_flag_inlineable(jl_array_t *data) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint8_t jl_ir_flag_pure(jl_array_t *data) JL_NOTSAFEPOINT;
JL_DLLEXPORT ssize_t jl_ir_nslots(jl_array_t *data) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint8_t jl_ir_slotflag(jl_array_t *data, size_t i) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_value_t *jl_compress_argnames(jl_array_t *syms);
JL_DLLEXPORT jl_array_t *jl_uncompress_argnames(jl_value_t *syms);
JL_DLLEXPORT jl_value_t *jl_uncompress_argname_n(jl_value_t *syms, size_t i);

JL_DLLEXPORT int jl_is_operator(char *sym);
JL_DLLEXPORT int jl_is_unary_operator(char *sym);
JL_DLLEXPORT int jl_is_unary_and_binary_operator(char *sym);
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

JL_DLLEXPORT jl_value_t *jl_apply_generic(jl_value_t *F, jl_value_t **args, uint32_t nargs);
JL_DLLEXPORT jl_value_t *jl_invoke(jl_value_t *F, jl_value_t **args, uint32_t nargs, jl_method_instance_t *meth);
JL_DLLEXPORT int32_t jl_invoke_api(jl_code_instance_t *linfo);

STATIC_INLINE
jl_value_t *jl_apply(jl_value_t **args, uint32_t nargs)
{
    return jl_apply_generic(args[0], &args[1], nargs - 1);
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
typedef struct _jl_excstack_t jl_excstack_t;

// info describing an exception handler
typedef struct _jl_handler_t {
    jl_jmp_buf eh_ctx;
    jl_gcframe_t *gcstack;
    struct _jl_handler_t *prev;
    int8_t gc_state;
    size_t locks_len;
    sig_atomic_t defer_signal;
    int finalizers_inhibited;
    jl_timing_block_t *timing_stack;
    size_t world_age;
} jl_handler_t;

typedef struct _jl_task_t {
    JL_DATA_TYPE
    jl_value_t *next; // invasive linked list for scheduler
    jl_value_t *queue; // invasive linked list for scheduler
    jl_value_t *tls;
    jl_sym_t *state;
    jl_value_t *donenotify;
    jl_value_t *result;
    jl_value_t *exception;
    jl_value_t *backtrace;
    jl_value_t *logstate;
    jl_function_t *start;
    uint8_t sticky; // record whether this Task can be migrated to a new thread

// hidden state:
    jl_ucontext_t ctx; // saved thread state
    void *stkbuf; // malloc'd memory (either copybuf or stack)
    size_t bufsz; // actual sizeof stkbuf
    unsigned int copy_stack:31; // sizeof stack for copybuf
    unsigned int started:1;

    // current exception handler
    jl_handler_t *eh;
    // saved gc stack top for context switches
    jl_gcframe_t *gcstack;
    // saved exception stack
    jl_excstack_t *excstack;
    // current world age
    size_t world_age;

    // id of owning thread
    // does not need to be defined until the task runs
    int16_t tid;
    /* for the multiqueue */
    int16_t prio;
    // This is statically initialized when the task is not holding any locks
    arraylist_t locks;
    jl_timing_block_t *timing_stack;
} jl_task_t;

JL_DLLEXPORT jl_task_t *jl_new_task(jl_function_t*, jl_value_t*, size_t);
JL_DLLEXPORT void jl_switchto(jl_task_t **pt);
JL_DLLEXPORT void JL_NORETURN jl_throw(jl_value_t *e JL_MAYBE_UNROOTED);
JL_DLLEXPORT void JL_NORETURN jl_rethrow(void);
JL_DLLEXPORT void JL_NORETURN jl_sig_throw(void);
JL_DLLEXPORT void JL_NORETURN jl_rethrow_other(jl_value_t *e JL_MAYBE_UNROOTED);
JL_DLLEXPORT void JL_NORETURN jl_no_exc_handler(jl_value_t *e);

#include "locks.h"   // requires jl_task_t definition

JL_DLLEXPORT void jl_enter_handler(jl_handler_t *eh);
JL_DLLEXPORT void jl_eh_restore_state(jl_handler_t *eh);
JL_DLLEXPORT void jl_pop_handler(int n);
JL_DLLEXPORT size_t jl_excstack_state(void);
JL_DLLEXPORT void jl_restore_excstack(size_t state);

#if defined(_OS_WINDOWS_)
#if defined(_COMPILER_GCC_)
int __attribute__ ((__nothrow__,__returns_twice__)) (jl_setjmp)(jmp_buf _Buf);
__declspec(noreturn) __attribute__ ((__nothrow__)) void (jl_longjmp)(jmp_buf _Buf, int _Value);
#else
int (jl_setjmp)(jmp_buf _Buf);
void (jl_longjmp)(jmp_buf _Buf, int _Value);
#endif
#define jl_setjmp_f jl_setjmp
#define jl_setjmp_name "jl_setjmp"
#define jl_setjmp(a,b) jl_setjmp(a)
#define jl_longjmp(a,b) jl_longjmp(a,b)
#elif defined(_OS_EMSCRIPTEN_)
#define jl_setjmp(a,b) setjmp(a)
#define jl_longjmp(a,b) longjmp(a,b)
#define jl_setjmp_f    setjmp
#define jl_setjmp_name "setjmp"
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


#ifdef __clang_analyzer__

// This is hard. Ideally we'd teach the static analyzer about the extra control
// flow edges. But for now, just hide this as best we can
extern int had_exception;
#define JL_TRY if (1)
#define JL_CATCH if (had_exception)

#else

#define JL_TRY                                                    \
    int i__tr, i__ca; jl_handler_t __eh;                          \
    size_t __excstack_state = jl_excstack_state();                \
    jl_enter_handler(&__eh);                                      \
    if (!jl_setjmp(__eh.eh_ctx,0))                                \
        for (i__tr=1; i__tr; i__tr=0, jl_eh_restore_state(&__eh))

#define JL_CATCH                                                \
    else                                                        \
        for (i__ca=1, jl_eh_restore_state(&__eh); i__ca; i__ca=0, jl_restore_excstack(__excstack_state))

#endif

// I/O system -----------------------------------------------------------------

#define JL_STREAM uv_stream_t
#define JL_STDOUT jl_uv_stdout
#define JL_STDERR jl_uv_stderr
#define JL_STDIN  jl_uv_stdin

JL_DLLEXPORT int jl_process_events(void);

JL_DLLEXPORT uv_loop_t *jl_global_event_loop(void);

JL_DLLEXPORT void jl_close_uv(uv_handle_t *handle);

JL_DLLEXPORT jl_array_t *jl_take_buffer(ios_t *s);

typedef struct {
    void *data;
    uv_loop_t *loop;
    uv_handle_type type;
    uv_os_fd_t file;
} jl_uv_file_t;

#ifdef __GNUC__
#define _JL_FORMAT_ATTR(type, str, arg) \
    __attribute__((format(type, str, arg)))
#else
#define _JL_FORMAT_ATTR(type, str, arg)
#endif

JL_DLLEXPORT void jl_uv_puts(uv_stream_t *stream, const char *str, size_t n);
JL_DLLEXPORT int jl_printf(uv_stream_t *s, const char *format, ...)
    _JL_FORMAT_ATTR(printf, 2, 3);
JL_DLLEXPORT int jl_vprintf(uv_stream_t *s, const char *format, va_list args)
    _JL_FORMAT_ATTR(printf, 2, 0);
JL_DLLEXPORT void jl_safe_printf(const char *str, ...) JL_NOTSAFEPOINT
    _JL_FORMAT_ATTR(printf, 1, 2);

extern JL_DLLEXPORT JL_STREAM *JL_STDIN;
extern JL_DLLEXPORT JL_STREAM *JL_STDOUT;
extern JL_DLLEXPORT JL_STREAM *JL_STDERR;

JL_DLLEXPORT JL_STREAM *jl_stdout_stream(void);
JL_DLLEXPORT JL_STREAM *jl_stdin_stream(void);
JL_DLLEXPORT JL_STREAM *jl_stderr_stream(void);

// showing and std streams
JL_DLLEXPORT void jl_flush_cstdio(void) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_value_t *jl_stdout_obj(void) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_value_t *jl_stderr_obj(void) JL_NOTSAFEPOINT;
JL_DLLEXPORT size_t jl_static_show(JL_STREAM *out, jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT size_t jl_static_show_func_sig(JL_STREAM *s, jl_value_t *type) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jlbacktrace(void) JL_NOTSAFEPOINT;
// Mainly for debugging, use `void*` so that no type cast is needed in C++.
JL_DLLEXPORT void jl_(void *jl_value) JL_NOTSAFEPOINT;

// julia options -----------------------------------------------------------
// NOTE: This struct needs to be kept in sync with JLOptions type in base/options.jl
typedef struct {
    int8_t quiet;
    int8_t banner;
    const char *julia_bindir;
    const char *julia_bin;
    const char **cmds;
    const char *image_file;
    const char *cpu_target;
    int32_t nthreads;
    int32_t nprocs;
    const char *machine_file;
    const char *project;
    int8_t isinteractive;
    int8_t color;
    int8_t historyfile;
    int8_t startupfile;
    int8_t compile_enabled;
    int8_t code_coverage;
    int8_t malloc_log;
    int8_t opt_level;
    int8_t debug_level;
    int8_t check_bounds;
    int8_t depwarn;
    int8_t warn_overwrite;
    int8_t can_inline;
    int8_t polly;
    const char *trace_compile;
    int8_t fast_math;
    int8_t worker;
    const char *cookie;
    int8_t handle_signals;
    int8_t use_sysimage_native_code;
    int8_t use_compiled_modules;
    const char *bindto;
    const char *outputbc;
    const char *outputunoptbc;
    const char *outputo;
    const char *outputasm;
    const char *outputji;
    const char *output_code_coverage;
    int8_t incremental;
    int8_t image_file_specified;
    int8_t warn_scope;
} jl_options_t;

extern JL_DLLEXPORT jl_options_t jl_options;
JL_DLLEXPORT ssize_t jl_sizeof_jl_options(void);

// Parse an argc/argv pair to extract general julia options, passing back out
// any arguments that should be passed on to the script.
JL_DLLEXPORT void jl_parse_opts(int *argcp, char ***argvp);
JL_DLLEXPORT char *jl_format_filename(const char *output_pattern);

// Set julia-level ARGS array according to the arguments provided in
// argc/argv
JL_DLLEXPORT void jl_set_ARGS(int argc, char **argv);

JL_DLLEXPORT int jl_generating_output(void) JL_NOTSAFEPOINT;

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

#define JL_OPTIONS_COLOR_AUTO 0
#define JL_OPTIONS_COLOR_ON 1
#define JL_OPTIONS_COLOR_OFF 2

#define JL_OPTIONS_HISTORYFILE_ON 1
#define JL_OPTIONS_HISTORYFILE_OFF 0

#define JL_OPTIONS_STARTUPFILE_ON 1
#define JL_OPTIONS_STARTUPFILE_OFF 2

#define JL_LOGLEVEL_BELOWMIN -1000001
#define JL_LOGLEVEL_DEBUG    -1000
#define JL_LOGLEVEL_INFO      0
#define JL_LOGLEVEL_WARN      1000
#define JL_LOGLEVEL_ERROR     2000
#define JL_LOGLEVEL_ABOVEMAX  1000001

#define JL_OPTIONS_DEPWARN_OFF 0
#define JL_OPTIONS_DEPWARN_ON 1
#define JL_OPTIONS_DEPWARN_ERROR 2

#define JL_OPTIONS_WARN_OVERWRITE_OFF 0
#define JL_OPTIONS_WARN_OVERWRITE_ON 1

#define JL_OPTIONS_WARN_SCOPE_OFF 0
#define JL_OPTIONS_WARN_SCOPE_ON 1

#define JL_OPTIONS_POLLY_ON 1
#define JL_OPTIONS_POLLY_OFF 0

#define JL_OPTIONS_FAST_MATH_ON 1
#define JL_OPTIONS_FAST_MATH_OFF 2
#define JL_OPTIONS_FAST_MATH_DEFAULT 0

#define JL_OPTIONS_HANDLE_SIGNALS_ON 1
#define JL_OPTIONS_HANDLE_SIGNALS_OFF 0

#define JL_OPTIONS_USE_SYSIMAGE_NATIVE_CODE_YES 1
#define JL_OPTIONS_USE_SYSIMAGE_NATIVE_CODE_NO 0

#define JL_OPTIONS_USE_COMPILED_MODULES_YES 1
#define JL_OPTIONS_USE_COMPILED_MODULES_NO 0

// Version information
#include "julia_version.h"

JL_DLLEXPORT extern int jl_ver_major(void);
JL_DLLEXPORT extern int jl_ver_minor(void);
JL_DLLEXPORT extern int jl_ver_patch(void);
JL_DLLEXPORT extern int jl_ver_is_release(void);
JL_DLLEXPORT extern const char *jl_ver_string(void);
JL_DLLEXPORT const char *jl_git_branch(void);
JL_DLLEXPORT const char *jl_git_commit(void);

// nullable struct representations
typedef struct {
    uint8_t hasvalue;
    double value;
} jl_nullable_float64_t;

typedef struct {
    uint8_t hasvalue;
    float value;
} jl_nullable_float32_t;

#define jl_current_task (jl_get_ptls_states()->current_task)
#define jl_root_task (jl_get_ptls_states()->root_task)

// codegen interface ----------------------------------------------------------
// The root propagation here doesn't have to be literal, but callers should
// ensure that the return value outlives the MethodInstance
typedef jl_value_t *(*jl_codeinstance_lookup_t)(jl_method_instance_t *mi JL_PROPAGATES_ROOT,
    size_t min_world, size_t max_world);
typedef struct {
    int track_allocations;  // can we track allocations?
    int code_coverage;      // can we measure coverage?
    int static_alloc;       // is the compiler allowed to allocate statically?
    int prefer_specsig;     // are specialized function signatures preferred?

    // controls the emission of debug-info. mirrors the clang options
    int gnu_pubnames;       // can we emit the gnu pubnames debuginfo
    int debug_info_kind; // Enum for line-table-only, line-directives-only,
                            // limited, standalone

    // hooks

    // module setup: prepare a module for code emission (data layout, DWARF version, ...)
    // parameters: LLVMModuleRef as Ptr{Cvoid}
    // return value: none
    jl_value_t *module_setup;

    // module activation: registers debug info, adds module to JIT
    // parameters: LLVMModuleRef as Ptr{Cvoid}
    // return value: none
    jl_value_t *module_activation;

    // exception raising: emit LLVM instructions to raise an exception
    // parameters: LLVMBasicBlockRef as Ptr{Cvoid}, LLVMValueRef as Ptr{Cvoid}
    // return value: none
    jl_value_t *raise_exception;

    // emit function: start emission of a new function
    // parameters: MethodInstance, CodeInfo, world age as UInt
    // return value: none
    jl_value_t *emit_function;

    // emitted function: end emission of a new function
    // parameters: MethodInstance, CodeInfo, world age as UInt
    // return value: none
    jl_value_t *emitted_function;

    // Cache access. Default: jl_rettype_inferred.
    jl_codeinstance_lookup_t lookup;
} jl_cgparams_t;
extern JL_DLLEXPORT jl_cgparams_t jl_default_cgparams;
extern JL_DLLEXPORT int jl_default_debug_info_kind;

#if !defined(_OS_DARWIN_) && !defined(_OS_WINDOWS_)
#define JULIA_DEFINE_FAST_TLS()                                                             \
JL_DLLEXPORT JL_CONST_FUNC jl_ptls_t jl_get_ptls_states_static(void)                        \
{                                                                                           \
    static __attribute__((tls_model("local-exec"))) __thread jl_tls_states_t tls_states;    \
    return &tls_states;                                                                     \
}                                                                                           \
__attribute__((constructor)) void jl_register_ptls_states_getter(void)                      \
{                                                                                           \
    /* We need to make sure this function is called before any reference to */              \
    /* TLS variables. */                                                                    \
    jl_set_ptls_states_getter(jl_get_ptls_states_static);                                   \
}
#else
#define JULIA_DEFINE_FAST_TLS()
#endif

#ifdef __cplusplus
}
#endif

#endif
