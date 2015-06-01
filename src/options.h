// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef JL_OPTIONS_H
#define JL_OPTIONS_H

// Build-time options for debugging, tweaking, and selecting alternative
// implementations of core features.

// object layout options ------------------------------------------------------

#ifdef _P64
// a risky way to save 8 bytes per tuple, by storing the length in the
// top bits of the type tag. only possible on 64-bit.
//#define OVERLAP_TUPLE_LEN
#endif

// if this is not defined, only individual dimension sizes are
// stored and not total length, to save space.
#define STORE_ARRAY_LEN

// how much space we're willing to waste if an array outgrows its
// original object
#define ARRAY_INLINE_NBYTES (2048*sizeof(void*))

// codegen options ------------------------------------------------------------

// (Experimental) codegen support for thread-local storage
// #define CODEGEN_TLS

// (Experimental) Use MCJIT ELF, even where it's not the native format
// #define FORCE_ELF

// with KEEP_BODIES, we keep LLVM function bodies around for later debugging
// #define KEEP_BODIES

// GC options -----------------------------------------------------------------

// only one GC is supported at this time
#define JL_GC_MARKSWEEP

// debugging options

// with MEMDEBUG, every object is allocated explicitly with malloc, and
// filled with 0xbb before being freed. this helps tools like valgrind
// catch invalid accesses.
// #define MEMDEBUG

// GC_VERIFY force a full verification gc along with every quick gc to ensure no
// reachable memory is freed
//#define GC_VERIFY

// SEGV_EXCEPTION turns segmentation faults into catchable julia exceptions.
// This is not recommended, as the memory state after such an exception should
// be considered untrusted, but can be helpful during development
// #define SEGV_EXCEPTION

// profiling options

// GC_FINAL_STATS prints total GC stats at exit
//#define GC_FINAL_STATS

// MEMPROFILE prints pool summary statistics after every GC
//#define MEMPROFILE

// GCTIME prints time taken by each phase of GC
//#define GC_TIME

// OBJPROFILE counts objects by type
//#define OBJPROFILE


// method dispatch profiling --------------------------------------------------

// turn type inference on/off. this is for internal debugging only, and must be
// turned on for all practical purposes.
#define ENABLE_INFERENCE

// print all signatures type inference is invoked on
//#define TRACE_INFERENCE

// print all generic method dispatches (excludes inlined and specialized call
// sites). this generally prints too much output to be useful.
//#define JL_TRACE

// count generic (not inlined or specialized) calls to each function. recorded
// in the `ncalls` field of jl_methtable_t.
//#define JL_GF_PROFILE


// task options ---------------------------------------------------------------

// select an implementation of stack switching.
// currently only COPY_STACKS is recommended.
#ifndef COPY_STACKS
#define COPY_STACKS
#endif

// sanitizer defaults ---------------------------------------------------------

// Automatically enable MEMDEBUG and KEEP_BODIES for the sanitizers
#if defined(__has_feature)
#  if __has_feature(address_sanitizer) || __has_feature(memory_sanitizer)
#  define MEMDEBUG
#  define KEEP_BODIES
#  endif
// Memory sanitizer also needs thread-local storage
#  if __has_feature(memory_sanitizer)
#  define CODEGEN_TLS
#  endif
#endif


#endif
