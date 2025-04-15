// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_INTERNAL_H
#define JL_INTERNAL_H

#include "options.h"
#include "julia_assert.h"
#include "julia_locks.h"
#include "julia_threads.h"
#include "support/utils.h"
#include "support/hashing.h"
#include "support/ptrhash.h"
#include "support/strtod.h"
#include "gc-alloc-profiler.h"
#include "support/rle.h"
#include <ctype.h>
#include <stdint.h>
#include <uv.h>
#include <llvm-c/Types.h>
#include <llvm-c/Orc.h>
#include <llvm-version.h>

#if !defined(_WIN32)
#include <unistd.h>
#else
#define sleep(x) Sleep(1000*x)
#endif
#if defined(_CPU_ARM_)
#include <sys/time.h>
#endif

// pragma visibility is more useful than -fvisibility
#pragma GCC visibility push(hidden)

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _COMPILER_ASAN_ENABLED_
#if defined(__GLIBC__) && defined(_CPU_X86_64_)
/* TODO: This is terrible - we're reaching deep into glibc internals here.
   We should probably just switch to our own setjmp/longjmp implementation. */
#define JB_RSP 6
static inline uintptr_t demangle_ptr(uintptr_t var)
{
    asm ("ror $17, %0\n\t"
         "xor %%fs:0x30, %0\n\t"
        : "=r" (var)
        : "0" (var));
    return var;
}
static inline uintptr_t jmpbuf_sp(jl_jmp_buf *buf)
{
    return demangle_ptr((uintptr_t)(*buf)[0].__jmpbuf[JB_RSP]);
}
#else
#error Need to implement jmpbuf_sp for this architecture
#endif
JL_DLLIMPORT void __sanitizer_start_switch_fiber(void**, const void*, size_t);
JL_DLLIMPORT void __sanitizer_finish_switch_fiber(void*, const void**, size_t*);
JL_DLLIMPORT void __asan_unpoison_stack_memory(uintptr_t addr, size_t size);
static inline void asan_unpoison_task_stack(jl_task_t *ct, jl_jmp_buf *buf)
{
    if (!ct)
        return;
    /* Unpoison everything from the base of the stack allocation to the address
       that we're resetting to. The idea is to remove the poison from the frames
       that we're skipping over, since they won't be unwound. */
    uintptr_t top = jmpbuf_sp(buf);
    uintptr_t bottom = (uintptr_t)(ct->ctx.copy_stack ? (char*)ct->ptls->stackbase  - ct->ptls->stacksize : (char*)ct->ctx.stkbuf);
    //uintptr_t bottom = (uintptr_t)&top;
    __asan_unpoison_stack_memory(bottom, top - bottom);
}
static inline void asan_unpoison_stack_memory(uintptr_t addr, size_t size) {
    __asan_unpoison_stack_memory(addr, size);
}
#else
static inline void asan_unpoison_task_stack(jl_task_t *ct, jl_jmp_buf *buf) JL_NOTSAFEPOINT {}
static inline void asan_unpoison_stack_memory(uintptr_t addr, size_t size) JL_NOTSAFEPOINT {}
#endif
#ifdef _COMPILER_MSAN_ENABLED_
JL_DLLIMPORT void __msan_unpoison(const volatile void *a, size_t size) JL_NOTSAFEPOINT;
JL_DLLIMPORT void __msan_allocated_memory(const volatile void *a, size_t size) JL_NOTSAFEPOINT;
JL_DLLIMPORT void __msan_unpoison_string(const volatile char *a) JL_NOTSAFEPOINT;
static inline void msan_allocated_memory(const volatile void *a, size_t size) JL_NOTSAFEPOINT {
    __msan_allocated_memory(a, size);
}
static inline void msan_unpoison(const volatile void *a, size_t size) JL_NOTSAFEPOINT {
    __msan_unpoison(a, size);
}
static inline void msan_unpoison_string(const volatile char *a) JL_NOTSAFEPOINT {
    __msan_unpoison_string(a);
}
#else
static inline void msan_unpoison(const volatile void *a, size_t size) JL_NOTSAFEPOINT {}
static inline void msan_allocated_memory(const volatile void *a, size_t size) JL_NOTSAFEPOINT {}
static inline void msan_unpoison_string(const volatile char *a) JL_NOTSAFEPOINT {}
#endif
#ifdef _COMPILER_TSAN_ENABLED_
JL_DLLIMPORT void *__tsan_create_fiber(unsigned flags);
JL_DLLIMPORT void *__tsan_get_current_fiber(void);
JL_DLLIMPORT void __tsan_destroy_fiber(void *fiber);
JL_DLLIMPORT void __tsan_switch_to_fiber(void *fiber, unsigned flags);
#endif

#ifndef _OS_WINDOWS_
    #if defined(_CPU_ARM_) || defined(_CPU_PPC_) || defined(_CPU_WASM_)
        #define MAX_ALIGN 8
    #elif defined(_CPU_AARCH64_) || defined(_CPU_RISCV64_) || (JL_LLVM_VERSION >= 180000 && (defined(_CPU_X86_64_) || defined(_CPU_X86_)) || (JL_LLVM_VERSION >= 200000 && defined(_CPU_PPC64_)))
    // int128 is 16 bytes aligned on aarch64 and riscv, and on x86 with LLVM >= 18 and on ppc64 with LLVM >= 20
        #define MAX_ALIGN 16
    #elif defined(_P64)
    // Generically we assume MAX_ALIGN is sizeof(void*)
        #define MAX_ALIGN 8
    #else
        #define MAX_ALIGN 4
    #endif
#else
    #if JL_LLVM_VERSION >= 180000
        #define MAX_ALIGN 16
    #else
        #define MAX_ALIGN 8
    #endif
#endif

#ifndef alignof
#  ifndef __cplusplus
#    ifdef __GNUC__
#      define alignof _Alignof
#    else
#      define alignof(...) 1
#    endif
#  endif
#endif

#if defined(__GLIBC__) && defined(JULIA_HAS_IFUNC_SUPPORT)
// Make sure both the compiler and the glibc supports it.
// Only enable this on known working glibc versions.
#  if (defined(_CPU_X86_) || defined(_CPU_X86_64_)) && __GLIBC_PREREQ(2, 12)
#    define JL_USE_IFUNC 1
#  elif (defined(_CPU_ARM_) || defined(_CPU_AARCH64_)) && __GLIBC_PREREQ(2, 18)
// This is the oldest tested version that supports ifunc.
#    define JL_USE_IFUNC 1
#  endif
// TODO: PPC probably supports ifunc on some glibc versions too
#endif
// Make sure JL_USE_IFUNC is always defined to catch include errors.
#ifndef JL_USE_IFUNC
#  define JL_USE_IFUNC 0
#endif

// If we've smashed the stack, (and not just normal NORETURN)
// this will smash stack-unwind too
#ifdef _OS_WINDOWS_
#if defined(_CPU_X86_64_)
    // install the unhandled exception handler at the top of our stack
    // to call directly into our personality handler
#define CFI_NORETURN \
    asm volatile ("\t.seh_handler __julia_personality, @except\n\t.text");
#else
#define CFI_NORETURN
#endif
#else
// wipe out the call-stack unwind capability beyond this function
// (we are noreturn, so it is not a total lie)
#if defined(_CPU_X86_64_)
// per nongnu libunwind: "x86_64 ABI specifies that end of call-chain is marked with a NULL RBP or undefined return address"
// so we do all 3, to be extra certain of it
#define CFI_NORETURN \
    asm volatile ("\t.cfi_undefined rip"); \
    asm volatile ("\t.cfi_undefined rbp"); \
    asm volatile ("\t.cfi_return_column rbp");
#else
    // per nongnu libunwind: "DWARF spec says undefined return address location means end of stack"
    // we use whatever happens to be register 1 on this platform for this
#define CFI_NORETURN \
    asm volatile ("\t.cfi_undefined 1"); \
    asm volatile ("\t.cfi_return_column 1");
#endif
#endif

extern JL_DLLEXPORT uintptr_t __stack_chk_guard;

// If this is detected in a backtrace of segfault, it means the functions
// that use this value must be reworked into their async form with cb arg
// provided and with JL_UV_LOCK used around the calls
static uv_loop_t *const unused_uv_loop_arg = (uv_loop_t *)0xBAD10;

extern jl_mutex_t jl_uv_mutex;
extern _Atomic(int) jl_uv_n_waiters;
void JL_UV_LOCK(void);
#define JL_UV_UNLOCK() JL_UNLOCK(&jl_uv_mutex)
extern _Atomic(unsigned) _threadedregion;
extern _Atomic(uint16_t) io_loop_tid;

int jl_running_under_rr(int recheck) JL_NOTSAFEPOINT;

//--------------------------------------------------
// timers
// Returns time in nanosec
JL_DLLEXPORT uint64_t jl_hrtime(void) JL_NOTSAFEPOINT;

JL_DLLEXPORT double jl_get_profile_peek_duration(void);
JL_DLLEXPORT void jl_set_profile_peek_duration(double);

JL_DLLEXPORT void jl_init_profile_lock(void);
JL_DLLEXPORT uintptr_t jl_lock_profile_rd_held(void) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_lock_profile(void) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER;
JL_DLLEXPORT void jl_unlock_profile(void) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE;
JL_DLLEXPORT void jl_lock_profile_wr(void) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER;
JL_DLLEXPORT void jl_unlock_profile_wr(void) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE;
void jl_with_stackwalk_lock(void (*f)(void*) JL_NOTSAFEPOINT, void *ctx) JL_NOTSAFEPOINT;

arraylist_t *jl_get_all_tasks_arraylist(void) JL_NOTSAFEPOINT;
typedef struct {
    size_t bt_size;
    int tid;
} jl_record_backtrace_result_t;
JL_DLLEXPORT JL_DLLEXPORT size_t jl_try_record_thread_backtrace(jl_ptls_t ptls2, struct _jl_bt_element_t *bt_data,
                                                                size_t max_bt_size) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_record_backtrace_result_t jl_record_backtrace(jl_task_t *t, struct _jl_bt_element_t *bt_data,
                                                              size_t max_bt_size, int all_tasks_profiler) JL_NOTSAFEPOINT;
extern volatile struct _jl_bt_element_t *profile_bt_data_prof;
extern volatile size_t profile_bt_size_max;
extern volatile size_t profile_bt_size_cur;
extern volatile int profile_running;
extern volatile int profile_all_tasks;
// Ensures that we can safely read the `live_tasks`field of every TLS when profiling.
// We want to avoid the case that a GC gets interleaved with `jl_profile_task` and shrinks
// the `live_tasks` array while we are reading it or frees tasks that are being profiled.
// Because of that, this lock must be held in `jl_profile_task` and `jl_gc_sweep_stack_pools_and_mtarraylist_buffers`.
extern uv_mutex_t live_tasks_lock;
// Ensures that we can safely write to `profile_bt_data_prof` and `profile_bt_size_cur`.
// We want to avoid the case that:
// - We start to profile a task very close to the profiling time window end.
// - The profiling time window ends and we start to read the profile data in a compute thread.
// - We write to the profile in a profiler thread while the compute thread is reading it.
// Locking discipline: `bt_data_prof_lock` must be held inside the scope of `live_tasks_lock`.
extern uv_mutex_t bt_data_prof_lock;
#define PROFILE_STATE_THREAD_NOT_SLEEPING (1)
#define PROFILE_STATE_THREAD_SLEEPING (2)
#define PROFILE_STATE_WALL_TIME_PROFILING (3)
void jl_profile_task(void);

// number of cycles since power-on
static inline uint64_t cycleclock(void) JL_NOTSAFEPOINT
{
#if defined(_CPU_X86_64_)
    // This is nopl 0(%rax, %rax, 1), but assembler are inconsistent about whether
    // they emit that as a 4 or 5 byte sequence and we need to be guaranteed to use
    // the 5 byte one.
#define NOP5_OVERRIDE_NOP ".byte 0x0f, 0x1f, 0x44, 0x00, 0x00\n\t"
    uint64_t low, high;
    // This instruction sequence is promised by rr to be patchable. rr can usually
    // also patch `rdtsc` in regular code, but without the preceding nop, there could
    // be an interfering branch into the middle of rr's patch region. Using this
    // sequence prevents a massive rr-induced slowdown if the compiler happens to emit
    // an unlucky pattern. See https://github.com/rr-debugger/rr/pull/3580.
    __asm__ volatile(NOP5_OVERRIDE_NOP "rdtsc" : "=a"(low), "=d"(high));
    return (high << 32) | low;
#elif defined(_CPU_X86_)
    int64_t ret;
    __asm__ volatile("rdtsc" : "=A"(ret));
    return ret;
#elif defined(_CPU_AARCH64_)
    // System timer of ARMv8 runs at a different frequency than the CPU's.
    // The frequency is fixed, typically in the range 1-50MHz.  It can be
    // read at CNTFRQ special register.  We assume the OS has set up
    // the virtual timer properly.
    int64_t virtual_timer_value;
    __asm__ volatile("mrs %0, cntvct_el0" : "=r"(virtual_timer_value));
    return virtual_timer_value;
#elif defined(_CPU_ARM_)
    // V6 is the earliest arch that has a standard cyclecount
#if (__ARM_ARCH >= 6)
    uint32_t pmccntr;
    uint32_t pmuseren;
    uint32_t pmcntenset;
    // Read the user mode perf monitor counter access permissions.
    asm volatile("mrc p15, 0, %0, c9, c14, 0" : "=r"(pmuseren));
    if (pmuseren & 1) {  // Allows reading perfmon counters for user mode code.
        asm volatile("mrc p15, 0, %0, c9, c12, 1" : "=r"(pmcntenset));
        if (pmcntenset & 0x80000000ul) {  // Is it counting?
            asm volatile("mrc p15, 0, %0, c9, c13, 0" : "=r"(pmccntr));
            // The counter is set up to count every 64th cycle
            return (int64_t)(pmccntr) * 64;  // Should optimize to << 6
        }
    }
#endif
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (int64_t)(tv.tv_sec) * 1000000 + tv.tv_usec;
#elif defined(_CPU_RISCV64_)
    // taken from https://github.com/google/benchmark/blob/3b3de69400164013199ea448f051d94d7fc7d81f/src/cycleclock.h#L190
    uint64_t ret;
    __asm__ volatile("rdcycle %0" : "=r"(ret));
    return ret;
#elif defined(_CPU_PPC64_)
    // This returns a time-base, which is not always precisely a cycle-count.
    // https://reviews.llvm.org/D78084
    int64_t tb;
    asm volatile("mfspr %0, 268" : "=r" (tb));
    return tb;
#else
    #warning No cycleclock() definition for your platform
    // copy from https://github.com/google/benchmark/blob/v1.5.0/src/cycleclock.h
    return 0;
#endif
}

#include "timing.h"

extern JL_DLLEXPORT uint64_t jl_typeinf_timing_begin(void) JL_NOTSAFEPOINT;
extern JL_DLLEXPORT void jl_typeinf_timing_end(uint64_t start, int is_recompile) JL_NOTSAFEPOINT;

// Global *atomic* integers controlling *process-wide* measurement of compilation time.
extern JL_DLLEXPORT _Atomic(uint8_t) jl_measure_compile_time_enabled;
extern JL_DLLEXPORT _Atomic(uint64_t) jl_cumulative_compile_time;
extern JL_DLLEXPORT _Atomic(uint64_t) jl_cumulative_recompile_time;

// Global *atomic* integer controlling *process-wide* task timing.
extern JL_DLLEXPORT _Atomic(uint8_t) jl_task_metrics_enabled;

#define jl_return_address() ((uintptr_t)__builtin_return_address(0))

STATIC_INLINE uint32_t jl_int32hash_fast(uint32_t a)
{
//    a = (a+0x7ed55d16) + (a<<12);
//    a = (a^0xc761c23c) ^ (a>>19);
//    a = (a+0x165667b1) + (a<<5);
//    a = (a+0xd3a2646c) ^ (a<<9);
//    a = (a+0xfd7046c5) + (a<<3);
//    a = (a^0xb55a4f09) ^ (a>>16);
    return a;  // identity hashing seems to work well enough here
}


// this is a version of memcpy that preserves atomic memory ordering
// which makes it safe to use for objects that can contain memory references
// without risk of creating pointers out of thin air
// TODO: replace with LLVM's llvm.memmove.element.unordered.atomic.p0i8.p0i8.i32
//       aka `__llvm_memmove_element_unordered_atomic_8` (for 64 bit)
static inline void memmove_refs(_Atomic(void*) *dstp, _Atomic(void*) *srcp, size_t n) JL_NOTSAFEPOINT
{
    size_t i;
    if (dstp < srcp || dstp > srcp + n) {
        for (i = 0; i < n; i++) {
            jl_atomic_store_release(dstp + i, jl_atomic_load_relaxed(srcp + i));
        }
    }
    else {
        for (i = 0; i < n; i++) {
            jl_atomic_store_release(dstp + n - i - 1, jl_atomic_load_relaxed(srcp + n - i - 1));
        }
    }
}

static inline void memassign_safe(int hasptr, char *dst, const jl_value_t *src, size_t nb) JL_NOTSAFEPOINT
{
    assert(nb == jl_datatype_size(jl_typeof(src)));
    if (hasptr) {
        size_t nptr = nb / sizeof(void*);
        memmove_refs((_Atomic(void*)*)dst, (_Atomic(void*)*)src, nptr);
        nb -= nptr * sizeof(void*);
        if (__likely(nb == 0))
            return;
        src = (jl_value_t*)((char*)src + nptr * sizeof(void*));
        dst = dst + nptr * sizeof(void*);
    }
    else if (nb >= 16) {
        memcpy(dst, jl_assume_aligned(src, 16), nb);
        return;
    }
    memcpy(dst, jl_assume_aligned(src, sizeof(void*)), nb);
}

// -- GC -- //

#define GC_CLEAN  0 // freshly allocated
#define GC_MARKED 1 // reachable and young
#define GC_OLD    2 // if it is reachable it will be marked as old
#define GC_OLD_MARKED (GC_OLD | GC_MARKED) // reachable and old
#define GC_IN_IMAGE 4

// useful constants
extern JL_DLLIMPORT jl_methtable_t *jl_type_type_mt JL_GLOBALLY_ROOTED;
extern JL_DLLIMPORT jl_methtable_t *jl_nonfunction_mt JL_GLOBALLY_ROOTED;
extern jl_methtable_t *jl_kwcall_mt JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT jl_method_t *jl_opaque_closure_method JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT _Atomic(size_t) jl_world_counter;
extern jl_debuginfo_t *jl_nulldebuginfo JL_GLOBALLY_ROOTED;

typedef void (*tracer_cb)(jl_value_t *tracee);
extern tracer_cb jl_newmeth_tracer;
void jl_call_tracer(tracer_cb callback, jl_value_t *tracee);
void print_func_loc(JL_STREAM *s, jl_method_t *m);
extern jl_array_t *_jl_debug_method_invalidation JL_GLOBALLY_ROOTED;
JL_DLLEXPORT extern arraylist_t jl_linkage_blobs; // external linkage: sysimg/pkgimages
JL_DLLEXPORT extern arraylist_t jl_image_relocs;  // external linkage: sysimg/pkgimages
JL_DLLEXPORT extern arraylist_t jl_top_mods;  // external linkage: sysimg/pkgimages
extern arraylist_t eytzinger_image_tree;
extern arraylist_t eytzinger_idxs;

extern JL_DLLEXPORT size_t jl_page_size;
extern JL_DLLEXPORT jl_function_t *jl_typeinf_func JL_GLOBALLY_ROOTED;
extern JL_DLLEXPORT size_t jl_typeinf_world;
extern _Atomic(jl_typemap_entry_t*) call_cache[N_CALL_CACHE] JL_GLOBALLY_ROOTED;

void free_stack(void *stkbuf, size_t bufsz) JL_NOTSAFEPOINT;

JL_DLLEXPORT extern int jl_lineno;
JL_DLLEXPORT extern const char *jl_filename;

jl_value_t *jl_gc_small_alloc_noinline(jl_ptls_t ptls, int offset,
                                   int osize);
jl_value_t *jl_gc_big_alloc_noinline(jl_ptls_t ptls, size_t allocsz);
JL_DLLEXPORT int jl_gc_classify_pools(size_t sz, int *osize) JL_NOTSAFEPOINT;
void gc_sweep_sysimg(void);


// pools are 16376 bytes large (GC_POOL_SZ - GC_PAGE_OFFSET)
static const int jl_gc_sizeclasses[] = {
#ifdef _P64
    8,
#elif MAX_ALIGN > 4
    // ARM and PowerPC have max alignment larger than pointer,
    // make sure allocation of size 8 has that alignment.
    4, 8,
#else
    4, 8, 12,
#endif

    // 16 pools at 8-byte spacing
    // the 8-byte aligned pools are only used for Strings
    16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120, 128, 136,
    // 8 pools at 16-byte spacing
    144, 160, 176, 192, 208, 224, 240, 256,

    // the following tables are computed for maximum packing efficiency via the formula:
    // pg = GC_SMALL_PAGE ? 2^12 : 2^14
    // sz = (div.(pg-8, rng).÷16)*16; hcat(sz, (pg-8).÷sz, pg .- (pg-8).÷sz.*sz)'

#ifdef GC_SMALL_PAGE
    // rng = 15:-1:2 (14 pools)
    272, 288, 304, 336, 368, 400, 448, 496, 576, 672, 816, 1008, 1360, 2032
//  15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, /pool
//  16, 64, 144, 64, 48, 96, 64, 128, 64, 64, 16, 64, 16, 32, bytes lost
#else
    // rng = 60:-4:32 (8 pools)
    272, 288, 304, 336, 368, 400, 448, 496,
//  60, 56, 53, 48, 44, 40, 36, 33, /pool
//  64, 256, 272, 256, 192, 384, 256,  16, bytes lost

    // rng = 30:-2:16 (8 pools)
    544, 576, 624, 672, 736, 816, 896, 1008,
//  30, 28, 26, 24, 22, 20, 18, 16, /pool
//  64, 256, 160, 256, 192,  64, 256, 256, bytes lost

    // rng = 15:-1:8 (8 pools)
    1088, 1168, 1248, 1360, 1488, 1632, 1808, 2032
//   15, 14, 13, 12, 11, 10, 9, 8, /pool
//   64, 32, 160, 64, 16, 64, 112,  128, bytes lost
#endif
};
#ifdef GC_SMALL_PAGE
#ifdef _P64
#  define JL_GC_N_POOLS 39
#elif MAX_ALIGN > 4
#  define JL_GC_N_POOLS 40
#else
#  define JL_GC_N_POOLS 41
#endif
#else
#ifdef _P64
#  define JL_GC_N_POOLS 49
#elif MAX_ALIGN > 4
#  define JL_GC_N_POOLS 50
#else
#  define JL_GC_N_POOLS 51
#endif
#endif
static_assert(sizeof(jl_gc_sizeclasses) / sizeof(jl_gc_sizeclasses[0]) == JL_GC_N_POOLS, "");

STATIC_INLINE int jl_gc_alignment(size_t sz) JL_NOTSAFEPOINT
{
    if (sz == 0)
        return sizeof(void*);
#ifdef _P64
    (void)sz;
    return 16;
#elif MAX_ALIGN > 4
    return sz <= 4 ? 8 : 16;
#else
    // szclass 8
    if (sz <= 4)
        return 8;
    // szclass 12
    if (sz <= 8)
        return 4;
    // szclass 16+
    return 16;
#endif
}
JL_DLLEXPORT int jl_alignment(size_t sz) JL_NOTSAFEPOINT;

// the following table is computed as:
// [searchsortedfirst(jl_gc_sizeclasses, i) - 1 for i = 0:16:jl_gc_sizeclasses[end]]
static const uint8_t szclass_table[] =
#ifdef GC_SMALL_PAGE
    {0,1,3,5,7,9,11,13,15,17,18,19,20,21,22,23,24,25,26,27,28,28,29,29,30,30,31,31,31,32,32,32,33,33,33,33,33,34,34,34,34,34,34,35,35,35,35,35,35,35,35,35,36,36,36,36,36,36,36,36,36,36,36,36,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38};
#else
    {0,1,3,5,7,9,11,13,15,17,18,19,20,21,22,23,24,25,26,27,28,28,29,29,30,30,31,31,31,32,32,32,33,33,33,34,34,35,35,35,36,36,36,37,37,37,37,38,38,38,38,38,39,39,39,39,39,40,40,40,40,40,40,40,41,41,41,41,41,42,42,42,42,42,43,43,43,43,43,44,44,44,44,44,44,44,45,45,45,45,45,45,45,45,46,46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,48};
#endif
static_assert(sizeof(szclass_table) == 128, "");

STATIC_INLINE uint8_t JL_CONST_FUNC jl_gc_szclass(unsigned sz) JL_NOTSAFEPOINT
{
    assert(sz <= 2032);
#ifdef _P64
    if (sz <= 8)
        return 0;
    const int N = 0;
#elif MAX_ALIGN > 4
    if (sz <= 8)
        return (sz >= 4 ? 1 : 0);
    const int N = 1;
#else
    if (sz <= 12)
        return (sz >= 8 ? 2 : (sz >= 4 ? 1 : 0));
    const int N = 2;
#endif
    uint8_t klass = szclass_table[(sz + 15) / 16];
    return klass + N;
}

STATIC_INLINE uint8_t JL_CONST_FUNC jl_gc_szclass_align8(unsigned sz) JL_NOTSAFEPOINT
{
    if (sz >= 16 && sz <= 152) {
#ifdef _P64
        const int N = 0;
#elif MAX_ALIGN > 4
        const int N = 1;
#else
        const int N = 2;
#endif
        return (sz + 7)/8 - 1 + N;
    }
    return jl_gc_szclass(sz);
}

#define JL_SMALL_BYTE_ALIGNMENT 16
// JL_HEAP_ALIGNMENT is the maximum alignment that the GC can provide
#define JL_HEAP_ALIGNMENT JL_SMALL_BYTE_ALIGNMENT
#define GC_MAX_SZCLASS (2032-sizeof(void*))
static_assert(ARRAY_CACHE_ALIGN_THRESHOLD > GC_MAX_SZCLASS, "");

/* Programming style note: When using jl_gc_alloc, do not JL_GC_PUSH it into a
 * gc frame, until it has been fully initialized. An uninitialized value in a
 * gc frame can crash upon encountering the first safepoint. By delaying use of
 * the JL_GC_PUSH macro until the value has been initialized, any accidental
 * safepoints will be caught by the GC analyzer.
 */
JL_DLLEXPORT jl_value_t *jl_gc_alloc(jl_ptls_t ptls, size_t sz, void *ty);
// On GCC, only inline when sz is constant
#ifdef __GNUC__
#  define jl_gc_alloc(ptls, sz, ty)  \
    (__builtin_constant_p(sz) ?      \
     jl_gc_alloc_(ptls, sz, ty) :    \
     (jl_gc_alloc)(ptls, sz, ty))
#else
#  define jl_gc_alloc(ptls, sz, ty) jl_gc_alloc_(ptls, sz, ty)
#endif

// jl_buff_tag must be an actual pointer here, so it cannot be confused for an actual type reference.
// defined as uint64_t[3] so that we can get the right alignment of this and a "type tag" on it
const extern uint64_t _jl_buff_tag[3];
#define jl_buff_tag ((uintptr_t)LLT_ALIGN((uintptr_t)&_jl_buff_tag[1],16))
JL_DLLEXPORT uintptr_t jl_get_buff_tag(void) JL_NOTSAFEPOINT;

typedef void jl_gc_tracked_buffer_t; // For the benefit of the static analyzer
STATIC_INLINE jl_gc_tracked_buffer_t *jl_gc_alloc_buf(jl_ptls_t ptls, size_t sz)
{
    return jl_gc_alloc(ptls, sz, (void*)jl_buff_tag);
}

jl_value_t *jl_permbox8(jl_datatype_t *t, uintptr_t tag, uint8_t x);
jl_value_t *jl_permbox32(jl_datatype_t *t, uintptr_t tag, uint32_t x);
jl_svec_t *jl_perm_symsvec(size_t n, ...);

// this sizeof(__VA_ARGS__) trick can't be computed until C11, but that only matters to Clang in some situations
#if !defined(__clang_analyzer__) && !(defined(_COMPILER_ASAN_ENABLED_) || defined(_COMPILER_TSAN_ENABLED_))
#ifdef _COMPILER_GCC_
#define jl_perm_symsvec(n, ...) \
    (jl_perm_symsvec)(__extension__({                                         \
            static_assert(                                                    \
                n == sizeof((char *[]){ __VA_ARGS__ })/sizeof(char *),        \
                "Number of passed arguments does not match expected number"); \
            n;                                                                \
        }), __VA_ARGS__)
#ifdef jl_svec
#undef jl_svec
#define jl_svec(n, ...) \
    (ijl_svec)(__extension__({                                                \
            static_assert(                                                    \
                n == sizeof((void *[]){ __VA_ARGS__ })/sizeof(void *),        \
                "Number of passed arguments does not match expected number"); \
            n;                                                                \
        }), __VA_ARGS__)
#else
#define jl_svec(n, ...) \
    (jl_svec)(__extension__({                                                 \
            static_assert(                                                    \
                n == sizeof((void *[]){ __VA_ARGS__ })/sizeof(void *),        \
                "Number of passed arguments does not match expected number"); \
            n;                                                                \
        }), __VA_ARGS__)
#endif
#endif
#endif

void jl_gc_track_malloced_genericmemory(jl_ptls_t ptls, jl_genericmemory_t *m, int isaligned) JL_NOTSAFEPOINT;
size_t jl_genericmemory_nbytes(jl_genericmemory_t *a) JL_NOTSAFEPOINT;
size_t memory_block_usable_size(void *mem, int isaligned) JL_NOTSAFEPOINT;
void jl_gc_count_allocd(size_t sz) JL_NOTSAFEPOINT;
void jl_gc_run_all_finalizers(jl_task_t *ct);
void jl_release_task_stack(jl_ptls_t ptls, jl_task_t *task);
void jl_gc_add_finalizer_(jl_ptls_t ptls, void *v, void *f) JL_NOTSAFEPOINT;

void jl_gc_debug_print_status(void) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_gc_debug_critical_error(void) JL_NOTSAFEPOINT;
void jl_print_gc_stats(JL_STREAM *s);
void jl_gc_reset_alloc_count(void);
uint32_t jl_get_gs_ctr(void);
void jl_set_gs_ctr(uint32_t ctr);

typedef struct _jl_static_show_config_t { uint8_t quiet; } jl_static_show_config_t;
size_t jl_static_show_func_sig_(JL_STREAM *s, jl_value_t *type, jl_static_show_config_t ctx) JL_NOTSAFEPOINT;

STATIC_INLINE jl_value_t *undefref_check(jl_datatype_t *dt, jl_value_t *v) JL_NOTSAFEPOINT
{
     if (dt->layout->first_ptr >= 0) {
        jl_value_t *nullp = ((jl_value_t**)v)[dt->layout->first_ptr];
        if (__unlikely(nullp == NULL))
            return NULL;
    }
    return v;
}

// -- helper types -- //

typedef struct {
    uint16_t propagate_inbounds:1;
    uint16_t has_fcall:1;
    uint16_t has_image_globalref:1;
    uint16_t nospecializeinfer:1;
    uint16_t isva:1;
    uint16_t nargsmatchesmethod:1;
    uint16_t inlining:2; // 0 = use heuristic; 1 = aggressive; 2 = none
    uint16_t constprop:2; // 0 = use heuristic; 1 = aggressive; 2 = none
    uint16_t has_ssaflags:1;
} jl_code_info_flags_bitfield_t;

typedef union {
    jl_code_info_flags_bitfield_t bits;
    uint16_t packed;
} jl_code_info_flags_t;

// -- functions -- //

// Also defined in typeinfer.jl - See documentation there.
#define SOURCE_MODE_NOT_REQUIRED            0x0
#define SOURCE_MODE_ABI                     0x1

JL_DLLEXPORT jl_code_instance_t *jl_engine_reserve(jl_method_instance_t *m, jl_value_t *owner);
JL_DLLEXPORT void jl_engine_fulfill(jl_code_instance_t *ci, jl_code_info_t *src);
void jl_engine_sweep(jl_ptls_t *gc_all_tls_states) JL_NOTSAFEPOINT;
int jl_engine_hasreserved(jl_method_instance_t *m, jl_value_t *owner) JL_NOTSAFEPOINT;

JL_DLLEXPORT jl_code_instance_t *jl_type_infer(jl_method_instance_t *li JL_PROPAGATES_ROOT, size_t world, uint8_t source_mode);
JL_DLLEXPORT jl_code_info_t *jl_gdbcodetyped1(jl_method_instance_t *mi, size_t world);
JL_DLLEXPORT jl_code_instance_t *jl_compile_method_internal(jl_method_instance_t *meth JL_PROPAGATES_ROOT, size_t world);
JL_DLLEXPORT jl_code_instance_t *jl_get_method_inferred(
        jl_method_instance_t *mi JL_PROPAGATES_ROOT, jl_value_t *rettype,
        size_t min_world, size_t max_world, jl_debuginfo_t *di, jl_svec_t *edges);
JL_DLLEXPORT jl_method_instance_t *jl_get_unspecialized(jl_method_t *def JL_PROPAGATES_ROOT);
JL_DLLEXPORT void jl_read_codeinst_invoke(jl_code_instance_t *ci, uint8_t *specsigflags, jl_callptr_t *invoke, void **specptr, int waitcompile);
JL_DLLEXPORT jl_method_instance_t *jl_method_match_to_mi(jl_method_match_t *match, size_t world, size_t min_valid, size_t max_valid, int mt_cache);
JL_DLLEXPORT void jl_add_codeinst_to_jit(jl_code_instance_t *codeinst, jl_code_info_t *src);
JL_DLLEXPORT void jl_add_codeinst_to_cache(jl_code_instance_t *codeinst, jl_code_info_t *src);

JL_DLLEXPORT jl_code_instance_t *jl_new_codeinst_uninit(jl_method_instance_t *mi, jl_value_t *owner);
JL_DLLEXPORT jl_code_instance_t *jl_new_codeinst(
        jl_method_instance_t *mi, jl_value_t *owner,
        jl_value_t *rettype, jl_value_t *exctype,
        jl_value_t *inferred_const, jl_value_t *inferred,
        int32_t const_flags, size_t min_world, size_t max_world,
        uint32_t effects, jl_value_t *analysis_results,
        jl_debuginfo_t *di, jl_svec_t *edges /* , int absolute_max*/);
JL_DLLEXPORT jl_code_instance_t *jl_get_ci_equiv(jl_code_instance_t *ci JL_PROPAGATES_ROOT, size_t target_world) JL_NOTSAFEPOINT;

STATIC_INLINE jl_method_instance_t *jl_get_ci_mi(jl_code_instance_t *ci JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    jl_value_t *def = ci->def;
    if (jl_is_abioverride(def))
        return ((jl_abi_override_t*)def)->def;
    assert(jl_is_method_instance(def));
    return (jl_method_instance_t*)def;
}

JL_DLLEXPORT const char *jl_debuginfo_file(jl_debuginfo_t *debuginfo) JL_NOTSAFEPOINT;
JL_DLLEXPORT const char *jl_debuginfo_file1(jl_debuginfo_t *debuginfo) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_module_t *jl_debuginfo_module1(jl_value_t *debuginfo_def) JL_NOTSAFEPOINT;
JL_DLLEXPORT const char *jl_debuginfo_name(jl_value_t *func) JL_NOTSAFEPOINT;

JL_DLLEXPORT int jl_is_compiled_codeinst(jl_code_instance_t *codeinst) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_compile_method_instance(jl_method_instance_t *mi, jl_tupletype_t *types, size_t world);
JL_DLLEXPORT void jl_compile_method_sig(jl_method_t *m, jl_value_t *types, jl_svec_t *sparams, size_t world);
JL_DLLEXPORT int jl_compile_hint(jl_tupletype_t *types);
JL_DLLEXPORT int jl_add_entrypoint(jl_tupletype_t *types);
jl_code_info_t *jl_code_for_interpreter(jl_method_instance_t *lam JL_PROPAGATES_ROOT, size_t world);
jl_value_t *jl_code_or_ci_for_interpreter(jl_method_instance_t *lam JL_PROPAGATES_ROOT, size_t world);
int jl_code_requires_compiler(jl_code_info_t *src, int include_force_compile);
jl_code_info_t *jl_new_code_info_from_ir(jl_expr_t *ast);
JL_DLLEXPORT jl_code_info_t *jl_new_code_info_uninit(void);
JL_DLLEXPORT void jl_resolve_definition_effects_in_ir(jl_array_t *stmts, jl_module_t *m, jl_svec_t *sparam_vals, jl_value_t *binding_edge,
                                           int binding_effects);
JL_DLLEXPORT int jl_maybe_add_binding_backedge(jl_binding_t *b, jl_value_t *edge, jl_method_t *in_method);
JL_DLLEXPORT void jl_add_binding_backedge(jl_binding_t *b, jl_value_t *edge);

static const uint8_t MI_FLAG_BACKEDGES_INUSE = 0b0100;
static const uint8_t MI_FLAG_BACKEDGES_DIRTY = 0b1000;
static const uint8_t MI_FLAG_BACKEDGES_ALL = 0b1100;

STATIC_INLINE jl_array_t *jl_mi_get_backedges_mutate(jl_method_instance_t *mi JL_PROPAGATES_ROOT, uint8_t *flags) {
    *flags = jl_atomic_load_relaxed(&mi->flags) & (MI_FLAG_BACKEDGES_ALL);
    jl_array_t *ret = mi->backedges;
    if (ret)
        jl_atomic_fetch_or_relaxed(&mi->flags, MI_FLAG_BACKEDGES_INUSE);
    return ret;
}

STATIC_INLINE jl_array_t *jl_mi_get_backedges(jl_method_instance_t *mi JL_PROPAGATES_ROOT) {
    assert(!(jl_atomic_load_relaxed(&mi->flags) & MI_FLAG_BACKEDGES_ALL));
    jl_array_t *ret = mi->backedges;
    return ret;
}

int get_next_edge(jl_array_t *list, int i, jl_value_t** invokesig, jl_code_instance_t **caller) JL_NOTSAFEPOINT;
int set_next_edge(jl_array_t *list, int i, jl_value_t *invokesig, jl_code_instance_t *caller);
int clear_next_edge(jl_array_t *list, int i, jl_value_t *invokesig, jl_code_instance_t *caller);
void push_edge(jl_array_t *list, jl_value_t *invokesig, jl_code_instance_t *caller);
void jl_mi_done_backedges(jl_method_instance_t *mi JL_PROPAGATES_ROOT, uint8_t old_flags);

JL_DLLEXPORT void jl_add_method_root(jl_method_t *m, jl_module_t *mod, jl_value_t* root);
void jl_append_method_roots(jl_method_t *m, uint64_t modid, jl_array_t* roots);
int get_root_reference(rle_reference *rr, jl_method_t *m, size_t i) JL_NOTSAFEPOINT;
jl_value_t *lookup_root(jl_method_t *m, uint64_t key, int index) JL_NOTSAFEPOINT;
int nroots_with_key(jl_method_t *m, uint64_t key) JL_NOTSAFEPOINT;

int jl_valid_type_param(jl_value_t *v);

JL_DLLEXPORT jl_value_t *jl_apply_2va(jl_value_t *f, jl_value_t **args, uint32_t nargs);

void JL_NORETURN jl_method_error(jl_value_t *F, jl_value_t **args, size_t na, size_t world);
JL_DLLEXPORT jl_value_t *jl_get_exceptionf(jl_datatype_t *exception_type, const char *fmt, ...);

JL_DLLEXPORT void jl_typeassert(jl_value_t *x, jl_value_t *t);

#define JL_CALLABLE(name)                                               \
    JL_DLLEXPORT jl_value_t *name(jl_value_t *F, jl_value_t **args, uint32_t nargs)

JL_CALLABLE(jl_f_svec);
JL_CALLABLE(jl_f_tuple);
JL_CALLABLE(jl_f_intrinsic_call);
JL_CALLABLE(jl_f_opaque_closure_call);
void jl_install_default_signal_handlers(void);
void restore_signals(void);
void jl_install_thread_signal_handler(jl_ptls_t ptls);

JL_DLLEXPORT jl_fptr_args_t jl_get_builtin_fptr(jl_datatype_t *dt);

extern uv_loop_t *jl_io_loop;
JL_DLLEXPORT void jl_uv_flush(uv_stream_t *stream);

typedef struct jl_typeenv_t {
    jl_tvar_t *var;
    jl_value_t *val;
    struct jl_typeenv_t *prev;
} jl_typeenv_t;

int jl_tuple_isa(jl_value_t **child, size_t cl, jl_datatype_t *pdt);
int jl_tuple1_isa(jl_value_t *child1, jl_value_t **child, size_t cl, jl_datatype_t *pdt);

enum atomic_kind {
    isatomic_none = 0,
    isatomic_object = 1,
    isatomic_field = 2
};

JL_DLLEXPORT int jl_has_intersect_type_not_kind(jl_value_t *t);
int jl_subtype_invariant(jl_value_t *a, jl_value_t *b, int ta);
int jl_has_concrete_subtype(jl_value_t *typ);
jl_tupletype_t *jl_inst_arg_tuple_type(jl_value_t *arg1, jl_value_t **args, size_t nargs, int leaf);
jl_tupletype_t *jl_lookup_arg_tuple_type(jl_value_t *arg1 JL_PROPAGATES_ROOT, jl_value_t **args, size_t nargs, int leaf);
JL_DLLEXPORT void jl_method_table_insert(jl_methtable_t *mt, jl_method_t *method, jl_tupletype_t *simpletype);
void jl_method_table_activate(jl_methtable_t *mt, jl_typemap_entry_t *newentry);
jl_typemap_entry_t *jl_method_table_add(jl_methtable_t *mt, jl_method_t *method, jl_tupletype_t *simpletype);
jl_datatype_t *jl_mk_builtin_func(jl_datatype_t *dt, const char *name, jl_fptr_args_t fptr) JL_GC_DISABLED;
int jl_obviously_unequal(jl_value_t *a, jl_value_t *b);
int jl_has_bound_typevars(jl_value_t *v, jl_typeenv_t *env) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_array_t *jl_find_free_typevars(jl_value_t *v);
int jl_has_fixed_layout(jl_datatype_t *t);
JL_DLLEXPORT int jl_struct_try_layout(jl_datatype_t *dt);
JL_DLLEXPORT int jl_type_mappable_to_c(jl_value_t *ty);
jl_svec_t *jl_outer_unionall_vars(jl_value_t *u);
jl_value_t *jl_type_intersection_env_s(jl_value_t *a, jl_value_t *b, jl_svec_t **penv, int *issubty);
jl_value_t *jl_type_intersection_env(jl_value_t *a, jl_value_t *b, jl_svec_t **penv);
int jl_subtype_matching(jl_value_t *a, jl_value_t *b, jl_svec_t **penv);
JL_DLLEXPORT int jl_types_egal(jl_value_t *a, jl_value_t *b) JL_NOTSAFEPOINT;
// specificity comparison assuming !(a <: b) and !(b <: a)
JL_DLLEXPORT int jl_type_morespecific_no_subtype(jl_value_t *a, jl_value_t *b);
jl_value_t *jl_instantiate_type_with(jl_value_t *t, jl_value_t **env, size_t n);
JL_DLLEXPORT jl_value_t *jl_instantiate_type_in_env(jl_value_t *ty, jl_unionall_t *env, jl_value_t **vals);
jl_value_t *jl_substitute_var(jl_value_t *t, jl_tvar_t *var, jl_value_t *val);
jl_value_t *jl_substitute_var_nothrow(jl_value_t *t, jl_tvar_t *var, jl_value_t *val, int nothrow);
jl_unionall_t *jl_rename_unionall(jl_unionall_t *u);
JL_DLLEXPORT jl_value_t *jl_unwrap_unionall(jl_value_t *v JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_value_t *jl_rewrap_unionall(jl_value_t *t, jl_value_t *u);
JL_DLLEXPORT jl_value_t *jl_rewrap_unionall_(jl_value_t *t, jl_value_t *u);
jl_value_t* jl_substitute_datatype(jl_value_t *t, jl_datatype_t * x, jl_datatype_t * y);
int jl_count_union_components(jl_value_t *v);
JL_DLLEXPORT jl_value_t *jl_nth_union_component(jl_value_t *v JL_PROPAGATES_ROOT, int i) JL_NOTSAFEPOINT;
int jl_find_union_component(jl_value_t *haystack, jl_value_t *needle, unsigned *nth) JL_NOTSAFEPOINT;
jl_datatype_t *jl_new_abstracttype(jl_value_t *name, jl_module_t *module,
                                   jl_datatype_t *super, jl_svec_t *parameters);
jl_datatype_t *jl_new_uninitialized_datatype(void);
void jl_precompute_memoized_dt(jl_datatype_t *dt, int cacheable);
JL_DLLEXPORT jl_datatype_t *jl_wrap_Type(jl_value_t *t);  // x -> Type{x}
jl_vararg_t *jl_wrap_vararg(jl_value_t *t, jl_value_t *n, int check, int nothrow);
void jl_reinstantiate_inner_types(jl_datatype_t *t);
jl_datatype_t *jl_lookup_cache_type_(jl_datatype_t *type);
void jl_cache_type_(jl_datatype_t *type);
jl_svec_t *cache_rehash_set(jl_svec_t *a, size_t newsz);
void set_nth_field(jl_datatype_t *st, jl_value_t *v, size_t i, jl_value_t *rhs, int isatomic) JL_NOTSAFEPOINT;
jl_value_t *swap_nth_field(jl_datatype_t *st, jl_value_t *v, size_t i, jl_value_t *rhs, int isatomic);
jl_value_t *modify_nth_field(jl_datatype_t *st, jl_value_t *v, size_t i, jl_value_t *op, jl_value_t *rhs, int isatomic);
jl_value_t *replace_nth_field(jl_datatype_t *st, jl_value_t *v, size_t i, jl_value_t *expected, jl_value_t *rhs, int isatomic);
int set_nth_fieldonce(jl_datatype_t *st, jl_value_t *v, size_t i, jl_value_t *rhs, int isatomic);
jl_value_t *swap_bits(jl_value_t *ty, char *v, uint8_t *psel, jl_value_t *parent, jl_value_t *rhs, enum atomic_kind isatomic);
jl_value_t *replace_value(jl_value_t *ty, _Atomic(jl_value_t*) *p, jl_value_t *parent, jl_value_t *expected, jl_value_t *rhs, int isatomic, jl_module_t *mod, jl_sym_t *name);
jl_value_t *replace_bits(jl_value_t *ty, char *p, uint8_t *psel, jl_value_t *parent, jl_value_t *expected, jl_value_t *rhs, enum atomic_kind isatomic);
jl_value_t *modify_value(jl_value_t *ty, _Atomic(jl_value_t*) *p, jl_value_t *parent, jl_value_t *op, jl_value_t *rhs, int isatomic, jl_module_t *mod, jl_sym_t *name);
jl_value_t *modify_bits(jl_value_t *ty, char *p, uint8_t *psel, jl_value_t *parent, jl_value_t *op, jl_value_t *rhs, enum atomic_kind isatomic);
int setonce_bits(jl_datatype_t *rty, char *p, jl_value_t *owner, jl_value_t *rhs, enum atomic_kind isatomic);
jl_expr_t *jl_exprn(jl_sym_t *head, size_t n);
jl_function_t *jl_new_generic_function(jl_sym_t *name, jl_module_t *module, size_t new_world);
jl_function_t *jl_new_generic_function_with_supertype(jl_sym_t *name, jl_module_t *module, jl_datatype_t *st, size_t new_world);
int jl_foreach_reachable_mtable(int (*visit)(jl_methtable_t *mt, void *env), void *env);
int foreach_mtable_in_module(jl_module_t *m, int (*visit)(jl_methtable_t *mt, void *env), void *env);
void jl_init_main_module(void);
JL_DLLEXPORT int jl_is_submodule(jl_module_t *child, jl_module_t *parent) JL_NOTSAFEPOINT;
jl_array_t *jl_get_loaded_modules(void);
JL_DLLEXPORT int jl_datatype_isinlinealloc(jl_datatype_t *ty, int pointerfree);
int jl_type_equality_is_identity(jl_value_t *t1, jl_value_t *t2) JL_NOTSAFEPOINT;

JL_DLLEXPORT void jl_eval_const_decl(jl_module_t *m, jl_value_t *arg, jl_value_t *val);
void jl_binding_set_type(jl_binding_t *b, jl_module_t *mod, jl_sym_t *sym, jl_value_t *ty);
void jl_eval_global_expr(jl_module_t *m, jl_expr_t *ex, int set_type);
JL_DLLEXPORT void jl_declare_global(jl_module_t *m, jl_value_t *arg, jl_value_t *set_type, int strong);
JL_DLLEXPORT jl_binding_partition_t *jl_declare_constant_val3(jl_binding_t *b JL_ROOTING_ARGUMENT, jl_module_t *mod, jl_sym_t *var, jl_value_t *val JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED, enum jl_partition_kind, size_t new_world) JL_GLOBALLY_ROOTED;
JL_DLLEXPORT jl_value_t *jl_toplevel_eval_flex(jl_module_t *m, jl_value_t *e, int fast, int expanded, const char **toplevel_filename, int *toplevel_lineno);

void jl_module_initial_using(jl_module_t *to, jl_module_t *from);
STATIC_INLINE struct _jl_module_using *module_usings_getidx(jl_module_t *m JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT;
STATIC_INLINE jl_module_t *module_usings_getmod(jl_module_t *m JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT;
void jl_add_usings_backedge(jl_module_t *from, jl_module_t *to);
typedef struct _modstack_t {
    jl_binding_t *b;
    struct _modstack_t *prev;
} modstack_t;

#ifndef __clang_gcanalyzer__
// The analyzer doesn't like looking through the arraylist, so just model the
// access for it using this function
STATIC_INLINE struct _jl_module_using *module_usings_getidx(jl_module_t *m JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT {
    return (struct _jl_module_using *)&(m->usings.items[3*i]);
}
STATIC_INLINE jl_module_t *module_usings_getmod(jl_module_t *m JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT {
    return module_usings_getidx(m, i)->mod;
}
#endif

STATIC_INLINE size_t module_usings_length(jl_module_t *m) JL_NOTSAFEPOINT {
    return m->usings.len/3;
}

STATIC_INLINE size_t module_usings_max(jl_module_t *m) JL_NOTSAFEPOINT {
    return m->usings.max/3;
}

JL_DLLEXPORT jl_sym_t *jl_module_name(jl_module_t *m) JL_NOTSAFEPOINT;
void jl_add_scanned_method(jl_module_t *m, jl_method_t *meth);
jl_value_t *jl_eval_global_var(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *e);
jl_value_t *jl_interpret_opaque_closure(jl_opaque_closure_t *clos, jl_value_t **args, size_t nargs);
jl_value_t *jl_interpret_toplevel_thunk(jl_module_t *m, jl_code_info_t *src);
jl_value_t *jl_interpret_toplevel_expr_in(jl_module_t *m, jl_value_t *e,
                                          jl_code_info_t *src,
                                          jl_svec_t *sparam_vals);
JL_DLLEXPORT int jl_is_toplevel_only_expr(jl_value_t *e) JL_NOTSAFEPOINT;
jl_value_t *jl_call_scm_on_ast_and_loc(const char *funcname, jl_value_t *expr,
                                       jl_module_t *inmodule, const char *file, int line);

JL_DLLEXPORT jl_value_t *jl_method_lookup_by_tt(jl_tupletype_t *tt, size_t world, jl_value_t *_mt);
JL_DLLEXPORT jl_method_instance_t *jl_method_lookup(jl_value_t **args, size_t nargs, size_t world);

jl_value_t *jl_gf_invoke_by_method(jl_method_t *method, jl_value_t *gf, jl_value_t **args, size_t nargs);
jl_value_t *jl_gf_invoke(jl_value_t *types, jl_value_t *f, jl_value_t **args, size_t nargs);
JL_DLLEXPORT jl_value_t *jl_gf_invoke_lookup_worlds(jl_value_t *types, jl_value_t *mt, size_t world, size_t *min_world, size_t *max_world);
JL_DLLEXPORT jl_value_t *jl_matching_methods(jl_tupletype_t *types, jl_value_t *mt, int lim, int include_ambiguous,
                                             size_t world, size_t *min_valid, size_t *max_valid, int *ambig);
JL_DLLEXPORT jl_value_t *jl_gf_invoke_lookup_worlds(jl_value_t *types, jl_value_t *mt, size_t world, size_t *min_world, size_t *max_world);


jl_datatype_t *jl_nth_argument_datatype(jl_value_t *argtypes JL_PROPAGATES_ROOT, int n) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_value_t *jl_argument_datatype(jl_value_t *argt JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_methtable_t *jl_method_table_for(
    jl_value_t *argtypes JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
jl_methtable_t *jl_kwmethod_table_for(
    jl_value_t *argtypes JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_methtable_t *jl_method_get_table(
    jl_method_t *method JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;

JL_DLLEXPORT int jl_pointer_egal(jl_value_t *t);
JL_DLLEXPORT jl_value_t *jl_nth_slot_type(jl_value_t *sig JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT;
void jl_compute_field_offsets(jl_datatype_t *st);
void jl_module_run_initializer(jl_module_t *m);
JL_DLLEXPORT jl_binding_t *jl_get_module_binding(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var, int alloc);
JL_DLLEXPORT void jl_binding_deprecation_warning(jl_binding_t *b);
JL_DLLEXPORT jl_binding_partition_t *jl_replace_binding_locked(jl_binding_t *b JL_PROPAGATES_ROOT,
    jl_binding_partition_t *old_bpart, jl_value_t *restriction_val, enum jl_partition_kind kind, size_t new_world) JL_GLOBALLY_ROOTED;
JL_DLLEXPORT jl_binding_partition_t *jl_replace_binding_locked2(jl_binding_t *b JL_PROPAGATES_ROOT,
    jl_binding_partition_t *old_bpart, jl_value_t *restriction_val, size_t kind, size_t new_world) JL_GLOBALLY_ROOTED;
JL_DLLEXPORT void jl_update_loaded_bpart(jl_binding_t *b, jl_binding_partition_t *bpart);
extern jl_array_t *jl_module_init_order JL_GLOBALLY_ROOTED;
extern htable_t jl_current_modules JL_GLOBALLY_ROOTED;
extern jl_module_t *jl_precompile_toplevel_module JL_GLOBALLY_ROOTED;
extern jl_genericmemory_t *jl_global_roots_list JL_GLOBALLY_ROOTED;
extern jl_genericmemory_t *jl_global_roots_keyset JL_GLOBALLY_ROOTED;
extern arraylist_t *jl_entrypoint_mis;
JL_DLLEXPORT extern size_t jl_require_world;
JL_DLLEXPORT int jl_is_globally_rooted(jl_value_t *val JL_MAYBE_UNROOTED) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_value_t *jl_as_global_root(jl_value_t *val, int insert) JL_GLOBALLY_ROOTED;
extern jl_svec_t *precompile_field_replace JL_GLOBALLY_ROOTED;
JL_DLLEXPORT void jl_set_precompile_field_replace(jl_value_t *val, jl_value_t *field, jl_value_t *newval) JL_GLOBALLY_ROOTED;

jl_opaque_closure_t *jl_new_opaque_closure(jl_tupletype_t *argt, jl_value_t *rt_lb, jl_value_t *rt_ub,
    jl_value_t *source,  jl_value_t **env, size_t nenv, int do_compile);
jl_method_t *jl_make_opaque_closure_method(jl_module_t *module, jl_value_t *name,
    int nargs, jl_value_t *functionloc, jl_code_info_t *ci, int isva, int isinferred);
JL_DLLEXPORT int jl_is_valid_oc_argtype(jl_tupletype_t *argt, jl_method_t *source);

STATIC_INLINE int jl_bkind_is_some_import(enum jl_partition_kind kind) JL_NOTSAFEPOINT {
    return kind == PARTITION_KIND_IMPLICIT_CONST || kind == PARTITION_KIND_IMPLICIT_GLOBAL || kind == PARTITION_KIND_EXPLICIT || kind == PARTITION_KIND_IMPORTED;
}

STATIC_INLINE int jl_bkind_is_some_explicit_import(enum jl_partition_kind kind) JL_NOTSAFEPOINT {
    return kind == PARTITION_KIND_EXPLICIT || kind == PARTITION_KIND_IMPORTED;
}

STATIC_INLINE int jl_bkind_is_some_guard(enum jl_partition_kind kind) JL_NOTSAFEPOINT {
    return kind == PARTITION_KIND_FAILED || kind == PARTITION_KIND_GUARD;
}

STATIC_INLINE int jl_bkind_is_some_implicit(enum jl_partition_kind kind) JL_NOTSAFEPOINT {
    return kind == PARTITION_KIND_IMPLICIT_CONST || kind == PARTITION_KIND_IMPLICIT_GLOBAL || jl_bkind_is_some_guard(kind);
}

STATIC_INLINE int jl_bkind_is_some_constant(enum jl_partition_kind kind) JL_NOTSAFEPOINT {
    return kind == PARTITION_KIND_IMPLICIT_CONST || kind == PARTITION_KIND_CONST || kind == PARTITION_KIND_CONST_IMPORT || kind == PARTITION_KIND_UNDEF_CONST || kind == PARTITION_KIND_BACKDATED_CONST;
}

STATIC_INLINE int jl_bkind_is_defined_constant(enum jl_partition_kind kind) JL_NOTSAFEPOINT {
    return kind == PARTITION_KIND_IMPLICIT_CONST || kind == PARTITION_KIND_CONST || kind == PARTITION_KIND_CONST_IMPORT || kind == PARTITION_KIND_BACKDATED_CONST;
}

JL_DLLEXPORT jl_binding_partition_t *jl_get_binding_partition(jl_binding_t *b JL_PROPAGATES_ROOT, size_t world) JL_GLOBALLY_ROOTED;
JL_DLLEXPORT jl_binding_partition_t *jl_get_binding_partition_with_hint(jl_binding_t *b JL_PROPAGATES_ROOT, jl_binding_partition_t *previous_part, size_t world) JL_GLOBALLY_ROOTED;
JL_DLLEXPORT jl_binding_partition_t *jl_get_binding_partition_all(jl_binding_t *b JL_PROPAGATES_ROOT, size_t min_world, size_t max_world) JL_GLOBALLY_ROOTED;

struct restriction_kind_pair {
    jl_binding_t *binding_if_global;
    jl_value_t *restriction;
    enum jl_partition_kind kind;
    int maybe_depwarn;
};
JL_DLLEXPORT int jl_get_binding_leaf_partitions_restriction_kind(jl_binding_t *b JL_PROPAGATES_ROOT, struct restriction_kind_pair *rkp, size_t min_world, size_t max_world) JL_GLOBALLY_ROOTED;
JL_DLLEXPORT jl_value_t *jl_get_binding_leaf_partitions_value_if_const(jl_binding_t *b JL_PROPAGATES_ROOT, int *maybe_depwarn, size_t min_world, size_t max_world);

EXTERN_INLINE_DECLARE uint8_t jl_bpart_get_kind(jl_binding_partition_t *bpart) JL_NOTSAFEPOINT {
    return (uint8_t)(bpart->kind & 0xf);
}

STATIC_INLINE void jl_walk_binding_inplace(jl_binding_t **bnd, jl_binding_partition_t **bpart JL_PROPAGATES_ROOT, size_t world) JL_NOTSAFEPOINT;
STATIC_INLINE void jl_walk_binding_inplace_depwarn(jl_binding_t **bnd, jl_binding_partition_t **bpart, size_t world, int *depwarn) JL_NOTSAFEPOINT;
STATIC_INLINE void jl_walk_binding_inplace_all(jl_binding_t **bnd, jl_binding_partition_t **bpart JL_PROPAGATES_ROOT, int *depwarn, size_t min_world, size_t max_world) JL_NOTSAFEPOINT;
STATIC_INLINE void jl_walk_binding_inplace_worlds(jl_binding_t **bnd, jl_binding_partition_t **bpart, size_t *min_world, size_t *max_world, int *depwarn, size_t world) JL_NOTSAFEPOINT;

#ifndef __clang_analyzer__
STATIC_INLINE void jl_walk_binding_inplace(jl_binding_t **bnd, jl_binding_partition_t **bpart, size_t world) JL_NOTSAFEPOINT
{
    while (1) {
        enum jl_partition_kind kind = jl_binding_kind(*bpart);
        if (!jl_bkind_is_some_explicit_import(kind) && kind != PARTITION_KIND_IMPLICIT_GLOBAL)
            return;
        *bnd = (jl_binding_t*)(*bpart)->restriction;
        *bpart = jl_get_binding_partition(*bnd, world);
    }
}

STATIC_INLINE void jl_walk_binding_inplace_depwarn(jl_binding_t **bnd, jl_binding_partition_t **bpart, size_t world, int *depwarn) JL_NOTSAFEPOINT
{
    int passed_explicit = 0;
    while (1) {
        enum jl_partition_kind kind = jl_binding_kind(*bpart);
        if (!jl_bkind_is_some_explicit_import(kind) && kind != PARTITION_KIND_IMPLICIT_GLOBAL) {
            if (!passed_explicit && depwarn)
                *depwarn |= (*bpart)->kind & PARTITION_FLAG_DEPWARN;
            return;
        }
        if (!passed_explicit && depwarn)
            *depwarn |= (*bpart)->kind & PARTITION_FLAG_DEPWARN;
        if (kind != PARTITION_KIND_IMPLICIT_GLOBAL)
            passed_explicit = 1;
        *bnd = (jl_binding_t*)(*bpart)->restriction;
        *bpart = jl_get_binding_partition(*bnd, world);
    }
}


STATIC_INLINE void jl_walk_binding_inplace_all(jl_binding_t **bnd, jl_binding_partition_t **bpart, int *depwarn, size_t min_world, size_t max_world) JL_NOTSAFEPOINT
{
    int passed_explicit = 0;
    while (*bpart) {
        enum jl_partition_kind kind = jl_binding_kind(*bpart);
        if (!jl_bkind_is_some_explicit_import(kind) && kind != PARTITION_KIND_IMPLICIT_GLOBAL) {
            if (!passed_explicit && depwarn)
                *depwarn |= (*bpart)->kind & PARTITION_FLAG_DEPWARN;
            return;
        }
        if (!passed_explicit && depwarn)
            *depwarn |= (*bpart)->kind & PARTITION_FLAG_DEPWARN;
        if (kind != PARTITION_KIND_IMPLICIT_GLOBAL)
            passed_explicit = 1;
        *bnd = (jl_binding_t*)(*bpart)->restriction;
        *bpart = jl_get_binding_partition_all(*bnd, min_world, max_world);
    }
}

STATIC_INLINE void jl_walk_binding_inplace_worlds(jl_binding_t **bnd, jl_binding_partition_t **bpart, size_t *min_world, size_t *max_world, int *depwarn, size_t world) JL_NOTSAFEPOINT
{
    int passed_explicit = 0;
    while (*bpart) {
        if (*min_world < (*bpart)->min_world)
            *min_world = (*bpart)->min_world;
        size_t bpart_max_world = jl_atomic_load_relaxed(&(*bpart)->max_world);
        if (*max_world > bpart_max_world)
            *max_world = bpart_max_world;
        enum jl_partition_kind kind = jl_binding_kind(*bpart);
        if (!jl_bkind_is_some_explicit_import(kind) && kind != PARTITION_KIND_IMPLICIT_GLOBAL) {
            if (!passed_explicit && depwarn)
                *depwarn |= (*bpart)->kind & PARTITION_FLAG_DEPWARN;
            return;
        }
        if (!passed_explicit && depwarn)
            *depwarn |= (*bpart)->kind & PARTITION_FLAG_DEPWARN;
        if (kind != PARTITION_KIND_IMPLICIT_GLOBAL)
            passed_explicit = 1;
        *bnd = (jl_binding_t*)(*bpart)->restriction;
        *bpart = jl_get_binding_partition(*bnd, world);
    }
}
#endif

STATIC_INLINE int is10digit(char c) JL_NOTSAFEPOINT
{
    return (c >= '0' && c <= '9');
}

STATIC_INLINE int is_anonfn_typename(char *name)
{
    if (name[0] != '#' || name[1] == '#')
        return 0;
    char *other = strrchr(name, '#');
    return other > &name[1] && is10digit(other[1]);
}

// Returns true for typenames of anounymous functions that have been canonicalized (i.e.
// we mangled the name of the outermost enclosing function in their name).
STATIC_INLINE int is_canonicalized_anonfn_typename(char *name) JL_NOTSAFEPOINT
{
    char *delim = strchr(&name[1], '#');
    if (delim == NULL)
        return 0;
    if (delim[1] != '#')
        return 0;
    if (!is10digit(delim[2]))
        return 0;
    return 1;
}

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

STATIC_INLINE jl_value_t *jl_unwrap_vararg(jl_vararg_t *v JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    assert(jl_is_vararg((jl_value_t*)v));
    jl_value_t *T = ((jl_vararg_t*)v)->T;
    return T ? T : (jl_value_t*)jl_any_type;
}
#define jl_unwrap_vararg(v) (jl_unwrap_vararg)((jl_vararg_t *)v)

STATIC_INLINE jl_value_t *jl_unwrap_vararg_num(jl_vararg_t *v JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    assert(jl_is_vararg((jl_value_t*)v));
    return ((jl_vararg_t*)v)->N;
}
#define jl_unwrap_vararg_num(v) (jl_unwrap_vararg_num)((jl_vararg_t *)v)

STATIC_INLINE jl_vararg_kind_t jl_vararg_kind(jl_value_t *v) JL_NOTSAFEPOINT
{
    if (!jl_is_vararg(v))
        return JL_VARARG_NONE;
    jl_vararg_t *vm = (jl_vararg_t *)v;
    if (!vm->N)
        return JL_VARARG_UNBOUND;
    if (jl_is_long(vm->N))
        return JL_VARARG_INT;
    return JL_VARARG_BOUND;
}

STATIC_INLINE int jl_is_va_tuple(jl_datatype_t *t) JL_NOTSAFEPOINT
{
    assert(jl_is_tuple_type(t));
    size_t l = jl_svec_len(t->parameters);
    return (l>0 && jl_is_vararg(jl_tparam(t,l-1)));
}

STATIC_INLINE size_t jl_vararg_length(jl_value_t *v) JL_NOTSAFEPOINT
{
    assert(jl_is_vararg(v));
    jl_value_t *len = jl_unwrap_vararg_num(v);
    assert(jl_is_long(len));
    return jl_unbox_long(len);
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

// -- init.c -- //

void jl_init_types(void) JL_GC_DISABLED;
void jl_init_box_caches(void);
void jl_init_flisp(void);
void jl_init_common_symbols(void);
void jl_init_primitives(void) JL_GC_DISABLED;
void jl_init_llvm(void);
void jl_init_runtime_ccall(void);
void jl_init_intrinsic_functions(void);
void jl_init_intrinsic_properties(void);
void jl_init_tasks(void) JL_GC_DISABLED;
void jl_init_stack_limits(int ismaster, void **stack_hi, void **stack_lo) JL_NOTSAFEPOINT;
jl_task_t *jl_init_root_task(jl_ptls_t ptls, void *stack_lo, void *stack_hi);
void jl_init_serializer(void);
void jl_init_uv(void);
void jl_init_int32_int64_cache(void);
JL_DLLEXPORT void jl_init_options(void);

void jl_set_base_ctx(char *__stk);

extern JL_DLLEXPORT ssize_t jl_tls_offset;
extern JL_DLLEXPORT const int jl_tls_elf_support;
void jl_init_threading(void);
void jl_start_threads(void);

// Whether the GC is running
extern uv_mutex_t safepoint_lock;
extern char *jl_safepoint_pages;
STATIC_INLINE int jl_addr_is_safepoint(uintptr_t addr)
{
    uintptr_t safepoint_addr = (uintptr_t)jl_safepoint_pages;
    return addr >= safepoint_addr && addr < safepoint_addr + jl_page_size * 4;
}
extern _Atomic(uint32_t) jl_gc_running;
extern _Atomic(uint32_t) jl_gc_disable_counter;
// All the functions are safe to be called from within a signal handler
// provided that the thread will not be interrupted by another asynchronous
// signal.
// Initialize the safepoint
void jl_safepoint_init(void);
// Start the GC, return `1` if the thread should be running the GC.
// Otherwise, the thread will wait in this function until the GC finishes on
// another thread and return `0`.
// The caller should have saved the `gc_state` and set it to `WAITING`
// before calling this function. If the calling thread is to run the GC,
// it should also wait for the mutator threads to hit a safepoint **AFTER**
// this function returns
int jl_safepoint_start_gc(jl_task_t *ct);
// Can only be called by the thread that have got a `1` return value from
// `jl_safepoint_start_gc()`. This disables the safepoint (for GC,
// the `mprotect` may not be removed if there's pending SIGINT) and wake
// up waiting threads if there's any.
// The caller should restore `gc_state` **AFTER** calling this function.
void jl_safepoint_end_gc(void);
// Wait for the GC to finish
// This function does **NOT** modify the `gc_state` to inform the GC thread
// The caller should set it **BEFORE** calling this function.
void jl_safepoint_wait_gc(jl_task_t *ct) JL_NOTSAFEPOINT;
void jl_safepoint_wait_thread_resume(jl_task_t *ct) JL_NOTSAFEPOINT;
int8_t jl_safepoint_take_sleep_lock(jl_ptls_t ptls) JL_NOTSAFEPOINT_ENTER;
// Set pending sigint and enable the mechanisms to deliver the sigint.
void jl_safepoint_enable_sigint(void);
// If the safepoint is enabled to deliver sigint, disable it
// so that the thread won't repeatedly trigger it in a sigatomic region
// while not being able to actually throw the exception.
void jl_safepoint_defer_sigint(void);
// Clear the sigint pending flag and disable the mechanism to deliver sigint.
// Return `1` if the sigint should be delivered and `0` if there's no sigint
// to be delivered.
int jl_safepoint_consume_sigint(void);
void jl_wake_libuv(void) JL_NOTSAFEPOINT;

void jl_set_pgcstack(jl_gcframe_t **) JL_NOTSAFEPOINT;
#if defined(_OS_WINDOWS_)
typedef DWORD jl_pgcstack_key_t;
#else
typedef jl_gcframe_t ***(*jl_pgcstack_key_t)(void) JL_NOTSAFEPOINT;
#endif
JL_DLLEXPORT void jl_pgcstack_getkey(jl_get_pgcstack_func **f, jl_pgcstack_key_t *k) JL_NOTSAFEPOINT;

#if !defined(_OS_WINDOWS_) && !defined(__APPLE__) && !defined(JL_DISABLE_LIBUNWIND)
extern pthread_mutex_t in_signal_lock;
#endif

void jl_set_gc_and_wait(jl_task_t *ct); // n.b. not used on _OS_DARWIN_

// Query if a Julia object is if a permalloc region (due to part of a sys- pkg-image)
STATIC_INLINE size_t n_linkage_blobs(void) JL_NOTSAFEPOINT
{
    return jl_image_relocs.len;
}

size_t external_blob_index(jl_value_t *v) JL_NOTSAFEPOINT;

// Query if this object is perm-allocated in an image.
JL_DLLEXPORT uint8_t jl_object_in_image(jl_value_t* v) JL_NOTSAFEPOINT;

// the first argument to jl_idtable_rehash is used to return a value
// make sure it is rooted if it is used after the function returns
JL_DLLEXPORT jl_genericmemory_t *jl_idtable_rehash(jl_genericmemory_t *a, size_t newsz);
_Atomic(jl_value_t*) *jl_table_peek_bp(jl_genericmemory_t *a, jl_value_t *key) JL_NOTSAFEPOINT;

JL_DLLEXPORT jl_method_t *jl_new_method_uninit(jl_module_t*);

jl_module_t *jl_new_module_(jl_sym_t *name, jl_module_t *parent, uint8_t default_using_core, uint8_t self_name);
jl_module_t *jl_add_standard_imports(jl_module_t *m);
JL_DLLEXPORT jl_methtable_t *jl_new_method_table(jl_sym_t *name, jl_module_t *module);
JL_DLLEXPORT jl_method_instance_t *jl_get_specialization1(jl_tupletype_t *types JL_PROPAGATES_ROOT, size_t world, int mt_cache);
jl_method_instance_t *jl_get_specialized(jl_method_t *m, jl_value_t *types, jl_svec_t *sp) JL_PROPAGATES_ROOT;
JL_DLLEXPORT jl_value_t *jl_rettype_inferred(jl_value_t *owner, jl_method_instance_t *li JL_PROPAGATES_ROOT, size_t min_world, size_t max_world);
JL_DLLEXPORT jl_value_t *jl_rettype_inferred_native(jl_method_instance_t *mi, size_t min_world, size_t max_world) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_code_instance_t *jl_method_compiled(jl_method_instance_t *mi JL_PROPAGATES_ROOT, size_t world) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_value_t *jl_methtable_lookup(jl_methtable_t *mt JL_PROPAGATES_ROOT, jl_value_t *type, size_t world);
JL_DLLEXPORT jl_method_instance_t *jl_specializations_get_linfo(
    jl_method_t *m JL_PROPAGATES_ROOT, jl_value_t *type, jl_svec_t *sparams);
jl_method_instance_t *jl_specializations_get_or_insert(jl_method_instance_t *mi_ins JL_PROPAGATES_ROOT);
JL_DLLEXPORT void jl_method_instance_add_backedge(jl_method_instance_t *callee, jl_value_t *invokesig, jl_code_instance_t *caller);
JL_DLLEXPORT void jl_method_table_add_backedge(jl_methtable_t *mt, jl_value_t *typ, jl_code_instance_t *caller);
JL_DLLEXPORT void jl_mi_cache_insert(jl_method_instance_t *mi JL_ROOTING_ARGUMENT,
                                     jl_code_instance_t *ci JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED);
JL_DLLEXPORT int jl_mi_try_insert(jl_method_instance_t *mi JL_ROOTING_ARGUMENT,
                                   jl_code_instance_t *expected_ci,
                                   jl_code_instance_t *ci JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED);
JL_DLLEXPORT jl_code_instance_t *jl_cached_uninferred(jl_code_instance_t *codeinst, size_t world);
JL_DLLEXPORT jl_code_instance_t *jl_cache_uninferred(jl_method_instance_t *mi, jl_code_instance_t *checked, size_t world, jl_code_instance_t *newci JL_MAYBE_UNROOTED);
JL_DLLEXPORT jl_code_instance_t *jl_new_codeinst_for_uninferred(jl_method_instance_t *mi, jl_code_info_t *src);
JL_DLLEXPORT extern jl_value_t *(*const jl_rettype_inferred_addr)(jl_method_instance_t *mi JL_PROPAGATES_ROOT, size_t min_world, size_t max_world) JL_NOTSAFEPOINT;

JL_DLLEXPORT void jl_force_trace_compile_timing_enable(void);
JL_DLLEXPORT void jl_force_trace_compile_timing_disable(void);

JL_DLLEXPORT void jl_force_trace_dispatch_enable(void);
JL_DLLEXPORT void jl_force_trace_dispatch_disable(void);

JL_DLLEXPORT void jl_tag_newly_inferred_enable(void);
JL_DLLEXPORT void jl_tag_newly_inferred_disable(void);

uint32_t jl_module_next_counter(jl_module_t *m) JL_NOTSAFEPOINT;
jl_tupletype_t *arg_type_tuple(jl_value_t *arg1, jl_value_t **args, size_t nargs);

JL_DLLEXPORT int jl_has_meta(jl_array_t *body, jl_sym_t *sym) JL_NOTSAFEPOINT;

JL_DLLEXPORT jl_value_t *jl_parse(const char *text, size_t text_len, jl_value_t *filename,
                                  size_t lineno, size_t offset, jl_value_t *options);
jl_code_info_t *jl_inner_ctor_body(jl_array_t *fieldkinds, jl_module_t *inmodule, const char *file, int line);
jl_code_info_t *jl_outer_ctor_body(jl_value_t *thistype, size_t nfields, size_t nsparams, jl_module_t *inmodule, const char *file, int line);
void jl_ctor_def(jl_value_t *ty, jl_value_t *functionloc);

//--------------------------------------------------
// Backtraces

// Backtrace buffers:
//
// A backtrace buffer conceptually contains a stack of instruction pointers
// ordered from the inner-most frame to the outermost. We store them in a
// special raw format for two reasons:
//
//   * Efficiency: Every `throw()` must populate the trace so it must be as
//     efficient as possible.
//   * Signal safety: For signal-based exceptions such as StackOverflowError
//     the trace buffer needs to be filled from a signal handler where most
//     operations are not allowed (including malloc) so we choose a flat
//     preallocated buffer.
//
// The raw buffer layout contains "frame entries" composed of one or several
// values of type `jl_bt_element_t`. From the point of view of the GC, an entry
// is either:
//
// 1. A single instruction pointer to native code, not GC-managed.
// 2. An "extended entry": a mixture of raw data and pointers to julia objects
//    which must be treated as GC roots.
//
// A single extended entry is serialized using multiple elements from the raw
// buffer; if `e` is the pointer to the first slot we have:
//
//   e[0]  JL_BT_NON_PTR_ENTRY  - Special marker to distinguish extended entries
//   e[1]  descriptor           - A bit packed uintptr_t containing a tag and
//                                the number of GC- managed and non-managed values
//   e[2+j]                     - GC managed data
//   e[2+ngc+i]                 - Non-GC-managed data
//
// The format of `descriptor` is, from LSB to MSB:
//   0:2     ngc     Number of GC-managed pointers for this frame entry
//   3:5     nptr    Number of non-GC-managed buffer elements
//   6:9     tag     Entry type
//   10:...  header  Entry-specific header data
typedef struct _jl_bt_element_t {
    union {
        uintptr_t   uintptr; // Metadata or native instruction ptr
        jl_value_t* jlvalue; // Pointer to GC-managed value
    };
} jl_bt_element_t;

#define JL_BT_NON_PTR_ENTRY (((uintptr_t)0)-1)
// Maximum size for an extended backtrace entry (likely significantly larger
// than the actual size of 3-4 for an interpreter frame)
#define JL_BT_MAX_ENTRY_SIZE 16

STATIC_INLINE int jl_bt_is_native(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    return bt_entry[0].uintptr != JL_BT_NON_PTR_ENTRY;
}

// Extended backtrace entry header packing; the bit packing is done manually
// for precise layout control for interop with julia side.
STATIC_INLINE uintptr_t jl_bt_entry_descriptor(int ngc, int nptr,
                                               int tag, uintptr_t header) JL_NOTSAFEPOINT
{
    assert(((ngc & 0x7) == ngc) && ((nptr & 0x7) == nptr) && ((tag & 0xf) == tag));
    return (ngc & 0x7) | (nptr & 0x7) << 3 | (tag & 0xf) << 6 | header << 10;
}

// Unpacking of extended backtrace entry data
STATIC_INLINE size_t jl_bt_num_jlvals(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    assert(!jl_bt_is_native(bt_entry));
    return bt_entry[1].uintptr & 0x7;
}
STATIC_INLINE size_t jl_bt_num_uintvals(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    assert(!jl_bt_is_native(bt_entry));
    return (bt_entry[1].uintptr >> 3) & 0x7;
}
STATIC_INLINE int jl_bt_entry_tag(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    assert(!jl_bt_is_native(bt_entry));
    return (bt_entry[1].uintptr >> 6) & 0xf;
}
STATIC_INLINE uintptr_t jl_bt_entry_header(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    assert(!jl_bt_is_native(bt_entry));
    return bt_entry[1].uintptr >> 10;
}

// Return `i`th GC-managed pointer for extended backtrace entry
// The returned value is rooted for the lifetime of the parent exception stack.
STATIC_INLINE jl_value_t *jl_bt_entry_jlvalue(jl_bt_element_t *bt_entry, size_t i) JL_NOTSAFEPOINT
{
    return bt_entry[2 + i].jlvalue;
}

#define JL_BT_INTERP_FRAME_TAG    1  // An interpreter frame

// Number of bt elements in frame.
STATIC_INLINE size_t jl_bt_entry_size(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    return jl_bt_is_native(bt_entry) ?
        1 : 2 + jl_bt_num_jlvals(bt_entry) + jl_bt_num_uintvals(bt_entry);
}

//------------------------------
// Stack walking and debug info lookup

// Function metadata arising from debug info lookup of instruction pointer
typedef struct {
    char *func_name;
    char *file_name;
    int line;
    jl_code_instance_t *ci;
    int fromC;
    int inlined;
} jl_frame_t;

#ifdef _OS_WINDOWS_
#include <dbghelp.h>
JL_DLLEXPORT EXCEPTION_DISPOSITION NTAPI __julia_personality(
        PEXCEPTION_RECORD ExceptionRecord, void *EstablisherFrame, PCONTEXT ContextRecord, void *DispatcherContext);
extern HANDLE hMainThread;
typedef CONTEXT bt_context_t;
#if defined(_CPU_X86_64_)
typedef CONTEXT bt_cursor_t;
#else
typedef struct {
    STACKFRAME64 stackframe;
    CONTEXT context;
} bt_cursor_t;
#endif
extern JL_DLLEXPORT uv_mutex_t jl_in_stackwalk;
#elif !defined(JL_DISABLE_LIBUNWIND)
// This gives unwind only local unwinding options ==> faster code
#  define UNW_LOCAL_ONLY
#pragma GCC visibility push(default)
#  include <libunwind.h>
#pragma GCC visibility pop
typedef unw_context_t bt_context_t;
typedef unw_cursor_t bt_cursor_t;
#  if (!defined(SYSTEM_LIBUNWIND) || UNW_VERSION_MAJOR > 1 ||   \
       (UNW_VERSION_MAJOR == 1 && UNW_VERSION_MINOR != 0 && UNW_VERSION_MINOR != 1))
// Enable our memory manager only for libunwind with our patch or
// on a newer release
#    define JL_UNW_HAS_FORMAT_IP 1
#  endif
#else
// Unwinding is disabled
typedef int bt_context_t;
typedef int bt_cursor_t;
#endif
size_t rec_backtrace(jl_bt_element_t *bt_data, size_t maxsize, int skip) JL_NOTSAFEPOINT;
// Record backtrace from a signal handler. `ctx` is the context of the code
// which was asynchronously interrupted.
size_t rec_backtrace_ctx(jl_bt_element_t *bt_data, size_t maxsize, bt_context_t *ctx,
                         jl_gcframe_t *pgcstack) JL_NOTSAFEPOINT;
#ifdef LLVMLIBUNWIND
size_t rec_backtrace_ctx_dwarf(jl_bt_element_t *bt_data, size_t maxsize, bt_context_t *ctx, jl_gcframe_t *pgcstack) JL_NOTSAFEPOINT;
#endif
JL_DLLEXPORT jl_value_t *jl_get_backtrace(void);
void jl_critical_error(int sig, int si_code, bt_context_t *context, jl_task_t *ct);
JL_DLLEXPORT void jl_raise_debugger(void) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_gdblookup(void* ip) JL_NOTSAFEPOINT;
void jl_print_native_codeloc(uintptr_t ip) JL_NOTSAFEPOINT;
void jl_print_bt_entry_codeloc(jl_bt_element_t *bt_data) JL_NOTSAFEPOINT;
#ifdef _OS_WINDOWS_
JL_DLLEXPORT void jl_refresh_dbg_module_list(void);
#endif
int jl_thread_suspend_and_get_state(int tid, int timeout, bt_context_t *ctx) JL_NOTSAFEPOINT;
void jl_thread_resume(int tid) JL_NOTSAFEPOINT;

// *to is NULL or malloc'd pointer, from is allowed to be NULL
STATIC_INLINE char *jl_copy_str(char **to, const char *from) JL_NOTSAFEPOINT
{
    if (!from) {
        free(*to);
        *to = NULL;
        return NULL;
    }
    size_t len = strlen(from) + 1;
    *to = (char*)realloc_s(*to, len);
    memcpy(*to, from, len);
    return *to;
}

JL_DLLEXPORT size_t jl_capture_interp_frame(jl_bt_element_t *bt_data,
        void *frameend, size_t space_remaining) JL_NOTSAFEPOINT;

//--------------------------------------------------
// Exception stack access and manipulation

// Exception stack: a stack of pairs of (exception,raw_backtrace).
// The stack may be traversed and accessed with the functions below.
struct _jl_excstack_t { // typedef in julia.h
    size_t top;
    size_t reserved_size;
    // Pack all stack entries into a growable buffer to amortize allocation
    // across repeated exception handling.
    // Layout: [bt_data1... bt_size1 exc1  bt_data2... bt_size2 exc2  ..]
    // jl_bt_element_t data[]; // Access with jl_excstack_raw
};

STATIC_INLINE jl_bt_element_t *jl_excstack_raw(jl_excstack_t *stack) JL_NOTSAFEPOINT
{
    return (jl_bt_element_t*)(stack + 1);
}

// Exception stack access
STATIC_INLINE jl_value_t *jl_excstack_exception(jl_excstack_t *stack JL_PROPAGATES_ROOT,
                                                size_t itr) JL_NOTSAFEPOINT
{
    return jl_excstack_raw(stack)[itr-1].jlvalue;
}
STATIC_INLINE size_t jl_excstack_bt_size(jl_excstack_t *stack, size_t itr) JL_NOTSAFEPOINT
{
    return jl_excstack_raw(stack)[itr-2].uintptr;
}
STATIC_INLINE jl_bt_element_t *jl_excstack_bt_data(jl_excstack_t *stack, size_t itr) JL_NOTSAFEPOINT
{
    return jl_excstack_raw(stack) + itr-2 - jl_excstack_bt_size(stack, itr);
}
// Exception stack iteration (start at itr=stack->top, stop at itr=0)
STATIC_INLINE size_t jl_excstack_next(jl_excstack_t *stack, size_t itr) JL_NOTSAFEPOINT
{
    return itr-2 - jl_excstack_bt_size(stack, itr);
}
// Exception stack manipulation
void jl_push_excstack(jl_task_t *ct, jl_excstack_t **stack JL_REQUIRE_ROOTED_SLOT JL_ROOTING_ARGUMENT,
                      jl_value_t *exception JL_ROOTED_ARGUMENT,
                      jl_bt_element_t *bt_data, size_t bt_size);

// System util to get maximum RSS
JL_DLLEXPORT size_t jl_maxrss(void);

//--------------------------------------------------
// congruential random number generator
// for a small amount of thread-local randomness

//TODO: utilize https://github.com/openssl/openssl/blob/master/crypto/rand/rand_uniform.c#L13-L99
// for better performance, it does however require making users expect a 32bit random number.

STATIC_INLINE uint64_t cong(uint64_t max, uint64_t *seed) JL_NOTSAFEPOINT // Open interval [0, max)
{
    if (max < 2)
        return 0;
    uint64_t mask = ~(uint64_t)0;
    int zeros = __builtin_clzll(max);
    int bits = CHAR_BIT * sizeof(uint64_t) - zeros;
    mask = mask >> zeros;
    do {
        uint64_t value = 69069 * (*seed) + 362437;
        *seed = value;
        uint64_t x = value & mask;
        if (x < max) {
            return x;
        }
        int bits_left = zeros;
        while (bits_left >= bits) {
            value >>= bits;
            x = value & mask;
            if (x < max) {
                return x;
            }
            bits_left -= bits;
        }
    } while (1);
}

JL_DLLEXPORT uint64_t jl_rand(void) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_srand(uint64_t) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_init_rand(void);

JL_DLLEXPORT extern void *jl_exe_handle;
JL_DLLEXPORT extern void *jl_libjulia_handle;
JL_DLLEXPORT extern void *jl_libjulia_internal_handle;
JL_DLLEXPORT extern void *jl_RTLD_DEFAULT_handle;

#if defined(_OS_WINDOWS_)
JL_DLLEXPORT extern const char *jl_crtdll_basename;
extern void *jl_ntdll_handle;
extern void *jl_kernel32_handle;
extern void *jl_crtdll_handle;
extern void *jl_winsock_handle;
void win32_formatmessage(DWORD code, char *reason, int len) JL_NOTSAFEPOINT;
#endif

JL_DLLEXPORT void *jl_get_library_(const char *f_lib, int throw_err);
void *jl_find_dynamic_library_by_addr(void *symbol, int throw_err);
#define jl_get_library(f_lib) jl_get_library_(f_lib, 1)
JL_DLLEXPORT void *jl_load_and_lookup(const char *f_lib, const char *f_name, _Atomic(void*) *hnd);
JL_DLLEXPORT void *jl_lazy_load_and_lookup(jl_value_t *lib_val, const char *f_name);
JL_DLLEXPORT jl_value_t *jl_get_cfunction_trampoline(
    jl_value_t *fobj, jl_datatype_t *result, htable_t *cache, jl_svec_t *fill,
    void *(*init_trampoline)(void *tramp, void **nval),
    jl_unionall_t *env, jl_value_t **vals);
JL_DLLEXPORT void *jl_get_abi_converter(jl_task_t *ct, _Atomic(void*) *fptr, _Atomic(size_t) *last_world, void *data);
JL_DLLIMPORT void *jl_jit_abi_converter(jl_task_t *ct, void *unspecialized, jl_value_t *declrt, jl_value_t *sigt, size_t nargs, int specsig,
    jl_code_instance_t *codeinst, jl_callptr_t invoke, void *target, int target_specsig);


// Special filenames used to refer to internal julia libraries
#define JL_EXE_LIBNAME                  ((const char*)1)
#define JL_LIBJULIA_DL_LIBNAME          ((const char*)2)
#define JL_LIBJULIA_INTERNAL_DL_LIBNAME ((const char*)3)
JL_DLLEXPORT const char *jl_dlfind(const char *name);

// libuv wrappers:
JL_DLLEXPORT int jl_fs_rename(const char *src_path, const char *dst_path);

// -- Runtime intrinsics -- //
JL_DLLEXPORT const char *jl_intrinsic_name(int f) JL_NOTSAFEPOINT;
JL_DLLEXPORT unsigned jl_intrinsic_nargs(int f) JL_NOTSAFEPOINT;

STATIC_INLINE int is_valid_intrinsic_elptr(jl_value_t *ety)
{
    return ety == (jl_value_t*)jl_any_type || (jl_is_concrete_type(ety) && !jl_is_layout_opaque(((jl_datatype_t*)ety)->layout) && !jl_is_array_type(ety));
}
JL_DLLEXPORT jl_value_t *jl_bitcast(jl_value_t *ty, jl_value_t *v);
JL_DLLEXPORT jl_value_t *jl_pointerref(jl_value_t *p, jl_value_t *i, jl_value_t *align);
JL_DLLEXPORT jl_value_t *jl_pointerset(jl_value_t *p, jl_value_t *x, jl_value_t *align, jl_value_t *i);
JL_DLLEXPORT jl_value_t *jl_atomic_fence(jl_value_t *order);
JL_DLLEXPORT jl_value_t *jl_atomic_pointerref(jl_value_t *p, jl_value_t *order);
JL_DLLEXPORT jl_value_t *jl_atomic_pointerset(jl_value_t *p, jl_value_t *x, jl_value_t *order);
JL_DLLEXPORT jl_value_t *jl_atomic_pointerswap(jl_value_t *p, jl_value_t *x, jl_value_t *order);
JL_DLLEXPORT jl_value_t *jl_atomic_pointermodify(jl_value_t *p, jl_value_t *f, jl_value_t *x, jl_value_t *order);
JL_DLLEXPORT jl_value_t *jl_atomic_pointerreplace(jl_value_t *p, jl_value_t *x, jl_value_t *expected, jl_value_t *success_order, jl_value_t *failure_order);
JL_DLLEXPORT jl_value_t *jl_cglobal(jl_value_t *v, jl_value_t *ty);
JL_DLLEXPORT jl_value_t *jl_cglobal_auto(jl_value_t *v);

JL_DLLEXPORT jl_value_t *jl_neg_int(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_add_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_sub_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_mul_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_sdiv_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_udiv_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_srem_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_urem_int(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_add_ptr(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_sub_ptr(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_neg_float(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_add_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_sub_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_mul_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_div_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_min_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_max_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_fma_float(jl_value_t *a, jl_value_t *b, jl_value_t *c);
JL_DLLEXPORT jl_value_t *jl_muladd_float(jl_value_t *a, jl_value_t *b, jl_value_t *c);

JL_DLLEXPORT jl_value_t *jl_eq_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_ne_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_slt_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_ult_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_sle_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_ule_int(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_eq_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_ne_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_lt_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_le_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_fpiseq(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_not_int(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_and_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_or_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_xor_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_shl_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_lshr_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_ashr_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_bswap_int(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_ctpop_int(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_ctlz_int(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_cttz_int(jl_value_t *a);

JL_DLLEXPORT jl_value_t *jl_sext_int(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_zext_int(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_trunc_int(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_sitofp(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_uitofp(jl_value_t *ty, jl_value_t *a);

JL_DLLEXPORT jl_value_t *jl_fptoui(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_fptosi(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_fptrunc(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_fpext(jl_value_t *ty, jl_value_t *a);

JL_DLLEXPORT jl_value_t *jl_checked_sadd_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_uadd_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_ssub_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_usub_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_smul_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_umul_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_sdiv_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_udiv_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_srem_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_urem_int(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_ceil_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_floor_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_trunc_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_rint_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_sqrt_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_sqrt_llvm_fast(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_abs_float(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_copysign_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_flipsign_int(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_have_fma(jl_value_t *a);
JL_DLLEXPORT int jl_stored_inline(jl_value_t *el_type);
JL_DLLEXPORT jl_value_t *(jl_array_data_owner)(jl_array_t *a);
JL_DLLEXPORT jl_array_t *jl_array_copy(jl_array_t *ary);

JL_DLLEXPORT uintptr_t jl_object_id_(uintptr_t tv, jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_set_next_task(jl_task_t *task) JL_NOTSAFEPOINT;

JL_DLLEXPORT uint16_t julia_double_to_half(double param) JL_NOTSAFEPOINT;
JL_DLLEXPORT uint16_t julia_float_to_half(float param) JL_NOTSAFEPOINT;
JL_DLLEXPORT float julia_half_to_float(uint16_t param) JL_NOTSAFEPOINT;

// -- synchronization utilities -- //

extern jl_mutex_t typecache_lock;
extern jl_mutex_t world_counter_lock;

#if defined(__APPLE__)
void jl_mach_gc_end(void) JL_NOTSAFEPOINT;
void jl_safepoint_resume_thread_mach(jl_ptls_t ptls2, int16_t tid2) JL_NOTSAFEPOINT;
#endif

// -- smallintset.c -- //

typedef uint_t (*smallintset_hash)(size_t val, jl_value_t *data);
typedef int (*smallintset_eq)(size_t val, const void *key, jl_value_t *data, uint_t hv);
ssize_t jl_smallintset_lookup(jl_genericmemory_t *cache, smallintset_eq eq, const void *key, jl_value_t *data, uint_t hv, int pop);
void jl_smallintset_insert(_Atomic(jl_genericmemory_t*) *pcache, jl_value_t *parent, smallintset_hash hash, size_t val, jl_value_t *data);
jl_genericmemory_t* smallintset_rehash(jl_genericmemory_t* a, smallintset_hash hash, jl_value_t *data, size_t newsz, size_t np);
void smallintset_empty(const jl_genericmemory_t *a) JL_NOTSAFEPOINT;

JL_DLLEXPORT jl_genericmemory_t *jl_idset_rehash(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, size_t newsz);
JL_DLLEXPORT ssize_t jl_idset_peek_bp(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, jl_value_t *key) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_value_t *jl_idset_get(jl_genericmemory_t *keys JL_PROPAGATES_ROOT, jl_genericmemory_t *idxs, jl_value_t *key) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_genericmemory_t *jl_idset_put_key(jl_genericmemory_t *keys, jl_value_t *key, ssize_t *newidx);
JL_DLLEXPORT jl_genericmemory_t *jl_idset_put_idx(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, ssize_t idx);
JL_DLLEXPORT ssize_t jl_idset_pop(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, jl_value_t *key) JL_NOTSAFEPOINT;

// -- typemap.c -- //

void jl_typemap_insert(_Atomic(jl_typemap_t*) *cache, jl_value_t *parent,
        jl_typemap_entry_t *newrec, int8_t offs);
jl_typemap_entry_t *jl_typemap_alloc(
        jl_tupletype_t *type, jl_tupletype_t *simpletype, jl_svec_t *guardsigs,
        jl_value_t *newvalue, size_t min_world, size_t max_world);

struct jl_typemap_assoc {
    // inputs
    jl_value_t *const types;
    size_t const world;
    // outputs
    jl_svec_t *env; // subtype env (initialize to null to perform intersection without an environment)
};

jl_typemap_entry_t *jl_typemap_assoc_by_type(
        jl_typemap_t *ml_or_cache JL_PROPAGATES_ROOT,
        struct jl_typemap_assoc *search,
        int8_t offs, uint8_t subtype);

jl_typemap_entry_t *jl_typemap_level_assoc_exact(jl_typemap_level_t *cache, jl_value_t *arg1, jl_value_t **args, size_t n, int8_t offs, size_t world);
jl_typemap_entry_t *jl_typemap_entry_assoc_exact(jl_typemap_entry_t *mn, jl_value_t *arg1, jl_value_t **args, size_t n, size_t world);
STATIC_INLINE jl_typemap_entry_t *jl_typemap_assoc_exact(
    jl_typemap_t *ml_or_cache JL_PROPAGATES_ROOT,
    jl_value_t *arg1, jl_value_t **args, size_t n, int8_t offs, size_t world)
{
    // NOTE: This function is a huge performance hot spot!!
    if (jl_typeof(ml_or_cache) == (jl_value_t *)jl_typemap_entry_type) {
        return jl_typemap_entry_assoc_exact(
            (jl_typemap_entry_t *)ml_or_cache, arg1, args, n, world);
    }
    else if (jl_typeof(ml_or_cache) == (jl_value_t*)jl_typemap_level_type) {
        return jl_typemap_level_assoc_exact(
            (jl_typemap_level_t *)ml_or_cache, arg1, args, n, offs, world);
    }
    return NULL;
}
typedef int (*jl_typemap_visitor_fptr)(jl_typemap_entry_t *l, void *closure);
int jl_typemap_visitor(jl_typemap_t *a, jl_typemap_visitor_fptr fptr, void *closure);

struct typemap_intersection_env;
typedef int (*jl_typemap_intersection_visitor_fptr)(jl_typemap_entry_t *l, struct typemap_intersection_env *closure);
struct typemap_intersection_env {
    // input values
    jl_typemap_intersection_visitor_fptr const fptr; // fptr to call on a match
    jl_value_t *const type; // type to match
    jl_value_t *const va; // the tparam0 for the vararg in type, if applicable (or NULL)
    size_t search_slurp;
    // output values
    size_t min_valid;
    size_t max_valid;
    jl_value_t *ti; // intersection type
    jl_svec_t *env; // intersection env (initialize to null to perform intersection without an environment)
    int issubty;    // if `a <: b` is true in `intersect(a,b)`
};
int jl_typemap_intersection_visitor(jl_typemap_t *a, int offs, struct typemap_intersection_env *closure);
void typemap_slurp_search(jl_typemap_entry_t *ml, struct typemap_intersection_env *closure);

// -- simplevector.c -- //

// check whether the specified number of arguments is compatible with the
// specified number of parameters of the tuple type
JL_DLLEXPORT int jl_tupletype_length_compat(jl_value_t *v, size_t nargs) JL_NOTSAFEPOINT;

JL_DLLEXPORT jl_value_t *jl_argtype_with_function(jl_value_t *f, jl_value_t *types0);
JL_DLLEXPORT jl_value_t *jl_argtype_with_function_type(jl_value_t *ft JL_MAYBE_UNROOTED, jl_value_t *types0);
JL_DLLEXPORT jl_value_t *jl_argtype_without_function(jl_value_t *ftypes);

JL_DLLEXPORT unsigned jl_special_vector_alignment(size_t nfields, jl_value_t *field_type);

void register_eh_frames(uint8_t *Addr, size_t Size) JL_NOTSAFEPOINT;
void deregister_eh_frames(uint8_t *Addr, size_t Size) JL_NOTSAFEPOINT;

STATIC_INLINE void *jl_get_frame_addr(void) JL_NOTSAFEPOINT
{
#ifdef __GNUC__
    return __builtin_frame_address(0);
#else
    void *dummy = NULL;
    // The mask is to suppress the compiler warning about returning
    // address of local variable
    return (void*)((uintptr_t)&dummy & ~(uintptr_t)15);
#endif
}

// Log `msg` to the current logger by calling CoreLogging.logmsg_shim() on the
// julia side. If any of module, group, id, file or line are NULL, these will
// be passed to the julia side as `nothing`.  If `kwargs` is NULL an empty set
// of keyword arguments will be passed.
void jl_log(int level, jl_value_t *module, jl_value_t *group, jl_value_t *id,
            jl_value_t *file, jl_value_t *line, jl_value_t *kwargs,
            jl_value_t *msg);

JL_DLLEXPORT int jl_isabspath(const char *in) JL_NOTSAFEPOINT;

extern JL_DLLEXPORT jl_sym_t *jl_call_sym;
extern JL_DLLEXPORT jl_sym_t *jl_invoke_sym;
extern JL_DLLEXPORT jl_sym_t *jl_invoke_modify_sym;
extern JL_DLLEXPORT jl_sym_t *jl_empty_sym;
extern JL_DLLEXPORT jl_sym_t *jl_top_sym;
extern JL_DLLEXPORT jl_sym_t *jl_module_sym;
extern JL_DLLEXPORT jl_sym_t *jl_slot_sym;
extern JL_DLLEXPORT jl_sym_t *jl_export_sym;
extern JL_DLLEXPORT jl_sym_t *jl_public_sym;
extern JL_DLLEXPORT jl_sym_t *jl_import_sym;
extern JL_DLLEXPORT jl_sym_t *jl_toplevel_sym;
extern JL_DLLEXPORT jl_sym_t *jl_quote_sym;
extern JL_DLLEXPORT jl_sym_t *jl_line_sym;
extern JL_DLLEXPORT jl_sym_t *jl_incomplete_sym;
extern JL_DLLEXPORT jl_sym_t *jl_goto_sym;
extern JL_DLLEXPORT jl_sym_t *jl_goto_ifnot_sym;
extern JL_DLLEXPORT jl_sym_t *jl_return_sym;
extern JL_DLLEXPORT jl_sym_t *jl_lineinfo_sym;
extern JL_DLLEXPORT jl_sym_t *jl_lambda_sym;
extern JL_DLLEXPORT jl_sym_t *jl_assign_sym;
extern JL_DLLEXPORT jl_sym_t *jl_binding_sym;
extern JL_DLLEXPORT jl_sym_t *jl_globalref_sym;
extern JL_DLLEXPORT jl_sym_t *jl_do_sym;
extern JL_DLLEXPORT jl_sym_t *jl_method_sym;
extern JL_DLLEXPORT jl_sym_t *jl_core_sym;
extern JL_DLLEXPORT jl_sym_t *jl_enter_sym;
extern JL_DLLEXPORT jl_sym_t *jl_leave_sym;
extern JL_DLLEXPORT jl_sym_t *jl_pop_exception_sym;
extern JL_DLLEXPORT jl_sym_t *jl_exc_sym;
extern JL_DLLEXPORT jl_sym_t *jl_error_sym;
extern JL_DLLEXPORT jl_sym_t *jl_new_sym;
extern JL_DLLEXPORT jl_sym_t *jl_using_sym;
extern JL_DLLEXPORT jl_sym_t *jl_splatnew_sym;
extern JL_DLLEXPORT jl_sym_t *jl_block_sym;
extern JL_DLLEXPORT jl_sym_t *jl_new_opaque_closure_sym;
extern JL_DLLEXPORT jl_sym_t *jl_opaque_closure_method_sym;
extern JL_DLLEXPORT jl_sym_t *jl_const_sym;
extern JL_DLLEXPORT jl_sym_t *jl_thunk_sym;
extern JL_DLLEXPORT jl_sym_t *jl_foreigncall_sym;
extern JL_DLLEXPORT jl_sym_t *jl_as_sym;
extern JL_DLLEXPORT jl_sym_t *jl_global_sym;
extern JL_DLLEXPORT jl_sym_t *jl_globaldecl_sym;
extern JL_DLLEXPORT jl_sym_t *jl_local_sym;
extern JL_DLLEXPORT jl_sym_t *jl_list_sym;
extern JL_DLLEXPORT jl_sym_t *jl_dot_sym;
extern JL_DLLEXPORT jl_sym_t *jl_newvar_sym;
extern JL_DLLEXPORT jl_sym_t *jl_boundscheck_sym;
extern JL_DLLEXPORT jl_sym_t *jl_inbounds_sym;
extern JL_DLLEXPORT jl_sym_t *jl_copyast_sym;
extern JL_DLLEXPORT jl_sym_t *jl_cfunction_sym;
extern JL_DLLEXPORT jl_sym_t *jl_loopinfo_sym;
extern JL_DLLEXPORT jl_sym_t *jl_meta_sym;
extern JL_DLLEXPORT jl_sym_t *jl_inert_sym;
extern JL_DLLEXPORT jl_sym_t *jl_polly_sym;
extern JL_DLLEXPORT jl_sym_t *jl_unused_sym;
extern JL_DLLEXPORT jl_sym_t *jl_static_parameter_sym;
extern JL_DLLEXPORT jl_sym_t *jl_inline_sym;
extern JL_DLLEXPORT jl_sym_t *jl_noinline_sym;
extern JL_DLLEXPORT jl_sym_t *jl_generated_sym;
extern JL_DLLEXPORT jl_sym_t *jl_generated_only_sym;
extern JL_DLLEXPORT jl_sym_t *jl_isdefined_sym;
extern JL_DLLEXPORT jl_sym_t *jl_propagate_inbounds_sym;
extern JL_DLLEXPORT jl_sym_t *jl_specialize_sym;
extern JL_DLLEXPORT jl_sym_t *jl_aggressive_constprop_sym;
extern JL_DLLEXPORT jl_sym_t *jl_no_constprop_sym;
extern JL_DLLEXPORT jl_sym_t *jl_purity_sym;
extern JL_DLLEXPORT jl_sym_t *jl_nospecialize_sym;
extern JL_DLLEXPORT jl_sym_t *jl_nospecializeinfer_sym;
extern JL_DLLEXPORT jl_sym_t *jl_macrocall_sym;
extern JL_DLLEXPORT jl_sym_t *jl_colon_sym;
extern JL_DLLEXPORT jl_sym_t *jl_hygienicscope_sym;
extern JL_DLLEXPORT jl_sym_t *jl_throw_undef_if_not_sym;
extern JL_DLLEXPORT jl_sym_t *jl_getfield_undefref_sym;
extern JL_DLLEXPORT jl_sym_t *jl_gc_preserve_begin_sym;
extern JL_DLLEXPORT jl_sym_t *jl_gc_preserve_end_sym;
extern JL_DLLEXPORT jl_sym_t *jl_coverageeffect_sym;
extern JL_DLLEXPORT jl_sym_t *jl_escape_sym;
extern JL_DLLEXPORT jl_sym_t *jl_aliasscope_sym;
extern JL_DLLEXPORT jl_sym_t *jl_popaliasscope_sym;
extern JL_DLLEXPORT jl_sym_t *jl_optlevel_sym;
extern JL_DLLEXPORT jl_sym_t *jl_thismodule_sym;
extern JL_DLLEXPORT jl_sym_t *jl_eval_sym;
extern JL_DLLEXPORT jl_sym_t *jl_include_sym;
extern JL_DLLEXPORT jl_sym_t *jl_atom_sym;
extern JL_DLLEXPORT jl_sym_t *jl_statement_sym;
extern JL_DLLEXPORT jl_sym_t *jl_all_sym;
extern JL_DLLEXPORT jl_sym_t *jl_compile_sym;
extern JL_DLLEXPORT jl_sym_t *jl_force_compile_sym;
extern JL_DLLEXPORT jl_sym_t *jl_infer_sym;
extern JL_DLLEXPORT jl_sym_t *jl_max_methods_sym;
extern JL_DLLEXPORT jl_sym_t *jl_atomic_sym;
extern JL_DLLEXPORT jl_sym_t *jl_not_atomic_sym;
extern JL_DLLEXPORT jl_sym_t *jl_unordered_sym;
extern JL_DLLEXPORT jl_sym_t *jl_monotonic_sym;
extern JL_DLLEXPORT jl_sym_t *jl_acquire_sym;
extern JL_DLLEXPORT jl_sym_t *jl_release_sym;
extern JL_DLLEXPORT jl_sym_t *jl_acquire_release_sym;
extern JL_DLLEXPORT jl_sym_t *jl_sequentially_consistent_sym;
extern JL_DLLEXPORT jl_sym_t *jl_uninferred_sym;
extern JL_DLLEXPORT jl_sym_t *jl_latestworld_sym;

JL_DLLEXPORT enum jl_memory_order jl_get_atomic_order(jl_sym_t *order, char loading, char storing);
JL_DLLEXPORT enum jl_memory_order jl_get_atomic_order_checked(jl_sym_t *order, char loading, char storing);

struct _jl_image_fptrs_t;

JL_DLLEXPORT void jl_write_coverage_data(const char*);
void jl_write_malloc_log(void);

#if jl_has_builtin(__builtin_unreachable) || defined(_COMPILER_GCC_) || defined(_COMPILER_INTEL_)
#  define jl_unreachable() __builtin_unreachable()
#else
#  define jl_unreachable() ((void)jl_assume(0))
#endif

extern uv_mutex_t symtab_lock;
jl_sym_t *_jl_symbol(const char *str, size_t len) JL_NOTSAFEPOINT;

// Tools for locally disabling spurious compiler warnings
//
// Particular calls which are used elsewhere in the code include:
//
// * JL_GCC_IGNORE_START(-Wclobbered) - gcc misidentifies some variables which
//   are used inside a JL_TRY as being "clobbered" if JL_CATCH is entered. This
//   warning is spurious if the variable is not modified inside the JL_TRY.
//   See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=65041
#ifdef _COMPILER_GCC_
#define JL_DO_PRAGMA(s) _Pragma(#s)
#define JL_GCC_IGNORE_START(warning) \
    JL_DO_PRAGMA(GCC diagnostic push) \
    JL_DO_PRAGMA(GCC diagnostic ignored warning)
#define JL_GCC_IGNORE_STOP \
    JL_DO_PRAGMA(GCC diagnostic pop)
#else
#define JL_GCC_IGNORE_START(w)
#define JL_GCC_IGNORE_STOP
#endif // _COMPILER_GCC_

#ifdef __clang_gcanalyzer__
  // Not a safepoint (so it doesn't free other values), but an artificial use.
  // Usually this is unnecessary because the analyzer can see all real uses,
  // but sometimes real uses are harder for the analyzer to see, or it may
  // give up before it sees it, so this can be helpful to be explicit.
  void JL_GC_ASSERT_LIVE(jl_value_t *v) JL_NOTSAFEPOINT;
#else
  #define JL_GC_ASSERT_LIVE(x) (void)(x)
#endif

JL_DLLEXPORT uint32_t jl_crc32c(uint32_t crc, const char *buf, size_t len);

// -- exports from codegen -- //

#define IR_FLAG_INBOUNDS 0x01

JL_DLLIMPORT void jl_generate_fptr_for_unspecialized(jl_code_instance_t *unspec);
JL_DLLIMPORT int jl_compile_codeinst(jl_code_instance_t *unspec);
JL_DLLIMPORT void jl_emit_codeinst_to_jit(jl_code_instance_t *codeinst, jl_code_info_t *src);

typedef struct {
    LLVMOrcThreadSafeModuleRef TSM;
    LLVMValueRef F;
} jl_llvmf_dump_t;

JL_DLLIMPORT jl_value_t *jl_dump_method_asm(jl_method_instance_t *linfo, size_t world,
        char emit_mc, char getwrapper, const char* asm_variant, const char *debuginfo, char binary);
JL_DLLIMPORT void jl_get_llvmf_defn(jl_llvmf_dump_t* dump, jl_method_instance_t *linfo, jl_code_info_t *src, char getwrapper, char optimize, const jl_cgparams_t params);
JL_DLLIMPORT jl_value_t *jl_dump_fptr_asm(uint64_t fptr, char emit_mc, const char* asm_variant, const char *debuginfo, char binary);
JL_DLLIMPORT jl_value_t *jl_dump_function_ir(jl_llvmf_dump_t *dump, char strip_ir_metadata, char dump_module, const char *debuginfo);
JL_DLLIMPORT jl_value_t *jl_dump_function_asm(jl_llvmf_dump_t *dump, char emit_mc, const char* asm_variant, const char *debuginfo, char binary, char raw);

typedef jl_value_t *(*jl_codeinstance_lookup_t)(jl_method_instance_t *mi JL_PROPAGATES_ROOT, size_t min_world, size_t max_world);
JL_DLLIMPORT void *jl_create_native(jl_array_t *methods, LLVMOrcThreadSafeModuleRef llvmmod, int trim, int cache, size_t world);
JL_DLLIMPORT void *jl_emit_native(jl_array_t *codeinfos, LLVMOrcThreadSafeModuleRef llvmmod, const jl_cgparams_t *cgparams, int _external_linkage);
JL_DLLIMPORT void jl_dump_native(void *native_code,
        const char *bc_fname, const char *unopt_bc_fname, const char *obj_fname, const char *asm_fname,
        ios_t *z, ios_t *s, jl_emission_params_t *params);
JL_DLLIMPORT void jl_get_llvm_gvs(void *native_code, size_t *num_els, void **gvs);
JL_DLLIMPORT void jl_get_llvm_external_fns(void *native_code, size_t *num_els,
                                           jl_code_instance_t *gvs);
JL_DLLIMPORT void jl_get_function_id(void *native_code, jl_code_instance_t *ncode,
        int32_t *func_idx, int32_t *specfunc_idx);
JL_DLLIMPORT void jl_register_fptrs(uint64_t image_base, const struct _jl_image_fptrs_t *fptrs,
                                    jl_method_instance_t **linfos, size_t n);
JL_DLLIMPORT void jl_get_llvm_mis(void *native_code, size_t *num_els,
                                  jl_method_instance_t *MIs);
JL_DLLIMPORT void jl_init_codegen(void);
JL_DLLIMPORT void jl_teardown_codegen(void) JL_NOTSAFEPOINT;
JL_DLLIMPORT int jl_getFunctionInfo(jl_frame_t **frames, uintptr_t pointer, int skipC, int noInline) JL_NOTSAFEPOINT;
// n.b. this might be called from unmanaged thread:
JL_DLLIMPORT uint64_t jl_getUnwindInfo(uint64_t dwBase);

#ifdef __cplusplus
}
#endif

#pragma GCC visibility pop


#ifdef USE_DTRACE
// Generated file, needs to be searched in include paths so that the builddir
// retains priority
#include <uprobes.h.gen>

// uprobes.h.gen on systems with DTrace, is auto-generated to include
// `JL_PROBE_{PROBE}` and `JL_PROBE_{PROBE}_ENABLED()` macros for every probe
// defined in uprobes.d
//
// If the arguments to `JL_PROBE_{PROBE}` are expensive to compute, the call to
// these functions must be guarded by a JL_PROBE_{PROBE}_ENABLED() check, to
// minimize performance impact when probing is off. As an example:
//
//    if (JL_PROBE_GC_STOP_THE_WORLD_ENABLED())
//        JL_PROBE_GC_STOP_THE_WORLD();

#else
// define a dummy version of the probe functions
#define JL_PROBE_GC_BEGIN(collection) do ; while (0)
#define JL_PROBE_GC_STOP_THE_WORLD() do ; while (0)
#define JL_PROBE_GC_MARK_BEGIN() do ; while (0)
#define JL_PROBE_GC_MARK_END(scanned_bytes, perm_scanned_bytes) do ; while (0)
#define JL_PROBE_GC_SWEEP_BEGIN(full) do ; while (0)
#define JL_PROBE_GC_SWEEP_END() do ; while (0)
#define JL_PROBE_GC_END() do ; while (0)
#define JL_PROBE_GC_FINALIZER() do ; while (0)
#define JL_PROBE_RT_RUN_TASK(task) do ; while (0)
#define JL_PROBE_RT_PAUSE_TASK(task) do ; while (0)
#define JL_PROBE_RT_NEW_TASK(parent, child) do ; while (0)
#define JL_PROBE_RT_START_TASK(task) do ; while (0)
#define JL_PROBE_RT_FINISH_TASK(task) do ; while (0)
#define JL_PROBE_RT_START_PROCESS_EVENTS(task) do ; while (0)
#define JL_PROBE_RT_FINISH_PROCESS_EVENTS(task) do ; while (0)
#define JL_PROBE_RT_TASKQ_INSERT(ptls, task) do ; while (0)
#define JL_PROBE_RT_TASKQ_GET(ptls, task) do ; while (0)
#define JL_PROBE_RT_SLEEP_CHECK_WAKE(other, old_state) do ; while (0)
#define JL_PROBE_RT_SLEEP_CHECK_WAKEUP(ptls) do ; while (0)
#define JL_PROBE_RT_SLEEP_CHECK_SLEEP(ptls) do ; while (0)
#define JL_PROBE_RT_SLEEP_CHECK_TASKQ_WAKE(ptls) do ; while (0)
#define JL_PROBE_RT_SLEEP_CHECK_TASK_WAKE(ptls) do ; while (0)
#define JL_PROBE_RT_SLEEP_CHECK_UV_WAKE(ptls) do ; while (0)

#define JL_PROBE_GC_BEGIN_ENABLED() (0)
#define JL_PROBE_GC_STOP_THE_WORLD_ENABLED() (0)
#define JL_PROBE_GC_MARK_BEGIN_ENABLED() (0)
#define JL_PROBE_GC_MARK_END_ENABLED() (0)
#define JL_PROBE_GC_SWEEP_BEGIN_ENABLED() (0)
#define JL_PROBE_GC_SWEEP_END_ENABLED()  (0)
#define JL_PROBE_GC_END_ENABLED() (0)
#define JL_PROBE_GC_FINALIZER_ENABLED() (0)
#define JL_PROBE_RT_RUN_TASK_ENABLED() (0)
#define JL_PROBE_RT_PAUSE_TASK_ENABLED() (0)
#define JL_PROBE_RT_NEW_TASK_ENABLED() (0)
#define JL_PROBE_RT_START_TASK_ENABLED() (0)
#define JL_PROBE_RT_FINISH_TASK_ENABLED() (0)
#define JL_PROBE_RT_START_PROCESS_EVENTS_ENABLED() (0)
#define JL_PROBE_RT_FINISH_PROCESS_EVENTS_ENABLED() (0)
#define JL_PROBE_RT_TASKQ_INSERT_ENABLED() (0)
#define JL_PROBE_RT_TASKQ_GET_ENABLED() (0)
#define JL_PROBE_RT_SLEEP_CHECK_WAKE_ENABLED() (0)
#define JL_PROBE_RT_SLEEP_CHECK_WAKEUP_ENABLED() (0)
#define JL_PROBE_RT_SLEEP_CHECK_SLEEP_ENABLED() (0)
#define JL_PROBE_RT_SLEEP_CHECK_TASKQ_WAKE_ENABLED() (0)
#define JL_PROBE_RT_SLEEP_CHECK_TASK_WAKE_ENABLED() (0)
#define JL_PROBE_RT_SLEEP_CHECK_UV_WAKE_ENABLED() (0)
#endif

#endif
