// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_OPTIONS_H
#define JL_OPTIONS_H

// Options in here are NOT allowed to affect the jlapi, since that would require this header to be installed

// Build-time options for debugging, tweaking, and selecting alternative
// implementations of core features.

#define N_CALL_CACHE 4096

// object layout options ------------------------------------------------------

// how much space we're willing to waste if an array outgrows its
// original object
#define ARRAY_INLINE_NBYTES (2048*sizeof(void*))

// codegen options ------------------------------------------------------------

// (Experimental) Use MCJIT ELF, even where it's not the native format
//#define FORCE_ELF

// with KEEP_BODIES, we keep LLVM function bodies around for later debugging
// #define KEEP_BODIES

// delete julia IR for non-inlineable functions after they're codegen'd
#define JL_DELETE_NON_INLINEABLE 1

// fill in the jl_all_methods in world-counter order
// so that it is possible to map (in a debugger) from
// an inferred world validity range back to the offending definition
// #define RECORD_METHOD_ORDER

// GC options -----------------------------------------------------------------

// debugging options

// with MEMDEBUG, every object is allocated explicitly with malloc, and
// filled with 0xbb before being freed. this helps tools like valgrind
// catch invalid accesses.
// #define MEMDEBUG

// with MEMFENCE, the object pool headers are verified during sweep
// to help detect corruption due to fence-post write errors
// #define MEMFENCE


// GC_VERIFY force a full verification gc along with every quick gc to ensure no
// reachable memory is freed
#ifndef GC_VERIFY
#ifdef GC_DEBUG_ENV
#define GC_VERIFY
#else
// It is recommanded to use the WITH_GC_VERIFY make option to turn on this
// option. Keep the document here before a better build system is ready.
// #define GC_VERIFY
#endif
#endif

// SEGV_EXCEPTION turns segmentation faults into catchable julia exceptions.
// This is not recommended, as the memory state after such an exception should
// be considered untrusted, but can be helpful during development
// #define SEGV_EXCEPTION

// profiling options

// GC_FINAL_STATS prints total GC stats at exit
// #define GC_FINAL_STATS

// MEMPROFILE prints pool summary statistics after every GC
//#define MEMPROFILE

// GC_TIME prints time taken by each phase of GC
// #define GC_TIME

// OBJPROFILE counts objects by type
// #define OBJPROFILE

// Automatic Instrumenting Profiler
//#define ENABLE_TIMINGS


// method dispatch profiling --------------------------------------------------

// turn type inference on/off. this is for internal debugging only, and must be
// turned on for all practical purposes.
#define ENABLE_INFERENCE

// print all signatures type inference is invoked on
//#define TRACE_INFERENCE
//#define TRACE_COMPILE

// print all generic method dispatches (excludes inlined and specialized call
// sites). this generally prints too much output to be useful.
//#define JL_TRACE

// profile generic (not inlined or specialized) calls to each function
//#define JL_GF_PROFILE


// task and threading options -------------------------------------------------

// controls for when threads sleep
#define THREAD_SLEEP_THRESHOLD_NAME     "JULIA_THREAD_SLEEP_THRESHOLD"
#define DEFAULT_THREAD_SLEEP_THRESHOLD  1e9    // cycles (1e9==1sec@1GHz)

// defaults for # threads
#define NUM_THREADS_NAME                "JULIA_NUM_THREADS"
#ifndef JULIA_NUM_THREADS
#  define JULIA_NUM_THREADS 1
#endif

// affinitization behavior
#define MACHINE_EXCLUSIVE_NAME          "JULIA_EXCLUSIVE"
#define DEFAULT_MACHINE_EXCLUSIVE       0

// threading infrastructure selection
#ifndef JULIA_ENABLE_PARTR
#define JULIA_ENABLE_FORKJOIN_TI        1

// select an implementation of stack switching.
// currently only COPY_STACKS is recommended.
#ifndef COPY_STACKS
#define COPY_STACKS
#endif
#endif // !JULIA_ENABLE_PARTR


// sanitizer defaults ---------------------------------------------------------

// XXX: these macros are duplicated from julia_internal.h
#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#define JL_ASAN_ENABLED
#endif
#elif defined(__SANITIZE_ADDRESS__)
#define JL_ASAN_ENABLED
#endif
#if defined(__has_feature)
#if __has_feature(memory_sanitizer)
#define JL_MSAN_ENABLED
#endif
#endif

// Automatically enable MEMDEBUG and KEEP_BODIES for the sanitizers
#if defined(JL_ASAN_ENABLED) || defined(JL_MSAN_ENABLED)
#define MEMDEBUG
#define KEEP_BODIES
#endif

// Memory sanitizer needs TLS, which llvm only supports for the small memory model
#if defined(JL_MSAN_ENABLED)
// todo: fix the llvm MemoryManager to work with small memory model
#endif

#endif
