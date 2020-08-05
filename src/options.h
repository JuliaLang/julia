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
// It is recommended to use the WITH_GC_VERIFY make option to turn on this
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

// print all generic method dispatches (excludes inlined and specialized call
// sites). this generally prints too much output to be useful.
//#define JL_TRACE

// profile generic (not inlined or specialized) calls to each function
//#define JL_GF_PROFILE


// task options ---------------------------------------------------------------

// select whether to allow the COPY_STACKS stack switching implementation
#define COPY_STACKS
// select whether to use COPY_STACKS for new Tasks by default
//#define ALWAYS_COPY_STACKS

// When not using COPY_STACKS the task-system is less memory efficient so
// you probably want to choose a smaller default stack size (factor of 8-10)
#ifdef _P64
#define JL_STACK_SIZE (4*1024*1024)
#else
#define JL_STACK_SIZE (2*1024*1024)
#endif

// allow a suspended Task to restart on a different thread
//#define MIGRATE_TASKS

// threading options ----------------------------------------------------------

// controls for when threads sleep
#define THREAD_SLEEP_THRESHOLD_NAME     "JULIA_THREAD_SLEEP_THRESHOLD"
#define DEFAULT_THREAD_SLEEP_THRESHOLD  4*1000*1000 // nanoseconds (4ms)

// defaults for # threads
#define NUM_THREADS_NAME                "JULIA_NUM_THREADS"
#ifndef JULIA_NUM_THREADS
#  define JULIA_NUM_THREADS 1
#endif

// affinitization behavior
#define MACHINE_EXCLUSIVE_NAME          "JULIA_EXCLUSIVE"
#define DEFAULT_MACHINE_EXCLUSIVE       0

// partr -- parallel tasks runtime options ------------------------------------

// multiq
    // number of heaps = MULTIQ_HEAP_C * nthreads
#define MULTIQ_HEAP_C                   4
    // how many in each heap
#define MULTIQ_TASKS_PER_HEAP           129

// parfor
    // tasks = niters / (GRAIN_K * nthreads)
#define GRAIN_K                         4

// synchronization
    // narrivers = ((GRAIN_K * nthreads) ^ ARRIVERS_P) + 1
    // limit for number of recursive parfors
#define ARRIVERS_P                      2
    // nreducers = narrivers * REDUCERS_FRAC
#define REDUCERS_FRAC                   1


// sanitizer defaults ---------------------------------------------------------

#ifndef JULIA_H
#error "Must be included after julia.h"
#endif

// Automatically enable MEMDEBUG and KEEP_BODIES for the sanitizers
#if defined(JL_ASAN_ENABLED) || defined(JL_MSAN_ENABLED)
#define MEMDEBUG
#define KEEP_BODIES
#endif

// TSAN doesn't like COPY_STACKS
#if defined(JL_TSAN_ENABLED) && defined(COPY_STACKS)
#undef COPY_STACKS
#endif

// Memory sanitizer needs TLS, which llvm only supports for the small memory model
#if defined(JL_MSAN_ENABLED)
// todo: fix the llvm MemoryManager to work with small memory model
#endif

#endif
