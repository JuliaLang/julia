// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_GC_OBJECT_LAYOUT_H
#define JL_GC_OBJECT_LAYOUT_H

#include <stdint.h>

#include "dtypes.h"
#include "julia_atomics.h"
#include "julia_threads.h"

#ifdef __cplusplus
extern "C" {
#endif

// =======
// object layout
// =======

// the common fields are hidden before the pointer, but the following macro is
// used to indicate which types below are subtypes of jl_value_t
#define JL_DATA_TYPE

typedef struct {
    uint32_t size;
    uint32_t nfields;
    uint32_t npointers; // number of pointers embedded inside
    int32_t first_ptr; // index of the first pointer (or -1)
    uint16_t alignment; // strictest alignment over all fields
    struct { // combine these fields into a struct so that we can take addressof them
        uint16_t haspadding : 1; // has internal undefined bytes
        uint16_t fielddesc_type : 2; // 0 -> 8, 1 -> 16, 2 -> 32, 3 -> foreign type
        // metadata bit only for GenericMemory eltype layout
        uint16_t arrayelem_isboxed : 1;
        uint16_t arrayelem_isunion : 1;
        uint16_t padding : 11;
    } flags;
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

struct _jl_typename_t;
struct _jl_svec_t;
struct _jl_value_t;

typedef struct _jl_datatype_t {
    JL_DATA_TYPE
    struct _jl_typename_t *name;
    struct _jl_datatype_t *super;
    struct _jl_svec_t *parameters;
    struct _jl_svec_t *types;
    struct _jl_value_t *instance;  // for singletons
    const jl_datatype_layout_t *layout;
    // memoized properties (set on construction)
    uint32_t hash;
    uint16_t hasfreetypevars:1; // majority part of isconcrete computation
    uint16_t isconcretetype:1; // whether this type can have instances
    uint16_t isdispatchtuple:1; // aka isleaftupletype
    uint16_t isbitstype:1; // relevant query for C-api and type-parameters
    uint16_t zeroinit:1; // if one or more fields requires zero-initialization
    uint16_t has_concrete_subtype:1; // If clear, no value will have this datatype
    uint16_t maybe_subtype_of_cache:1; // Computational bit for has_concrete_supertype. See description in jltypes.c.
    uint16_t isprimitivetype:1; // whether this is declared with 'primitive type' keyword (sized, no fields, and immutable)
    uint16_t ismutationfree:1; // whether any mutable memory is reachable through this type (in the type or via fields)
    uint16_t isidentityfree:1; // whether this type or any object reachable through its fields has non-content-based identity
    uint16_t smalltag:6; // whether this type has a small-tag optimization
} jl_datatype_t;

typedef struct _jl_value_t jl_value_t;

struct _jl_taggedvalue_bits {
    uintptr_t gc:2;
    uintptr_t in_image:1;
    uintptr_t unused:1;
#ifdef _P64
    uintptr_t tag:60;
#else
    uintptr_t tag:28;
#endif
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

static inline jl_value_t *jl_to_typeof(uintptr_t t) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT;
#ifdef __clang_gcanalyzer__
JL_DLLEXPORT jl_taggedvalue_t *_jl_astaggedvalue(jl_value_t *v JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
#define jl_astaggedvalue(v) _jl_astaggedvalue((jl_value_t*)(v))
jl_value_t *_jl_valueof(jl_taggedvalue_t *tv JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
#define jl_valueof(v) _jl_valueof((jl_taggedvalue_t*)(v))
JL_DLLEXPORT jl_value_t *_jl_typeof(jl_value_t *v JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
#define jl_typeof(v) (_jl_typeof((jl_value_t*)(v)))
#define jl_typetagof(v) ((uintptr_t)_jl_typeof((jl_value_t*)(v)))
#else
#define jl_astaggedvalue(v)                                             \
    ((jl_taggedvalue_t*)((char*)(v) - sizeof(jl_taggedvalue_t)))
#define jl_valueof(v)                                                   \
    ((jl_value_t*)((char*)(v) + sizeof(jl_taggedvalue_t)))
#define jl_typeof(v)                                                    \
    jl_to_typeof(jl_typetagof(v))
#define jl_typetagof(v)                                                 \
    ((jl_astaggedvalue(v)->header) & ~(uintptr_t)15)
#endif
static inline void jl_set_typeof(void *v, void *t) JL_NOTSAFEPOINT
{
    // Do not call this on a value that is already initialized.
    jl_taggedvalue_t *tag = jl_astaggedvalue(v);
    jl_atomic_store_relaxed((_Atomic(jl_value_t*)*)&tag->type, (jl_value_t*)t);
}

#ifdef __cplusplus
}
#endif

#endif // JL_GC_OBJECT_LAYOUT_H
