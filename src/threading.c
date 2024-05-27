// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef USE_ITTAPI
#include "ittapi/ittnotify.h"
#endif

// Ref https://www.uclibc.org/docs/tls.pdf
// For variant 1 JL_ELF_TLS_INIT_SIZE is the size of the thread control block (TCB)
// For variant 2 JL_ELF_TLS_INIT_SIZE is 0
#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
#  if defined(_CPU_X86_64_) || defined(_CPU_X86_)
#    define JL_ELF_TLS_VARIANT 2
#    define JL_ELF_TLS_INIT_SIZE 0
#  elif defined(_CPU_AARCH64_)
#    define JL_ELF_TLS_VARIANT 1
#    define JL_ELF_TLS_INIT_SIZE 16
#  elif defined(__ARM_ARCH) && __ARM_ARCH >= 7
#    define JL_ELF_TLS_VARIANT 1
#    define JL_ELF_TLS_INIT_SIZE 8
#  endif
#endif

#ifdef JL_ELF_TLS_VARIANT
#  include <link.h>
#endif

// `ElfW` was added to FreeBSD in 12.3 but we still support 12.2
#if defined(_OS_FREEBSD_) && !defined(ElfW)
#  define ElfW(x) __ElfN(x)
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include "threading.h"

JL_DLLEXPORT _Atomic(uint8_t) jl_measure_compile_time_enabled = 0;
JL_DLLEXPORT _Atomic(uint64_t) jl_cumulative_compile_time = 0;
JL_DLLEXPORT _Atomic(uint64_t) jl_cumulative_recompile_time = 0;

JL_DLLEXPORT void *jl_get_ptls_states(void)
{
    // mostly deprecated: use current_task instead
    return jl_current_task->ptls;
}

static void jl_delete_thread(void*);

#if !defined(_OS_WINDOWS_)
static pthread_key_t jl_task_exit_key;
static pthread_key_t jl_safe_restore_key;

__attribute__((constructor)) void _jl_init_safe_restore(void)
{
    pthread_key_create(&jl_safe_restore_key, NULL);
    pthread_key_create(&jl_task_exit_key, jl_delete_thread);
}

JL_DLLEXPORT jl_jmp_buf *jl_get_safe_restore(void)
{
    return (jl_jmp_buf*)pthread_getspecific(jl_safe_restore_key);
}

JL_DLLEXPORT void jl_set_safe_restore(jl_jmp_buf *sr)
{
    pthread_setspecific(jl_safe_restore_key, (void*)sr);
}
#endif


// The tls_states buffer:
//
// On platforms that do not use ELF (i.e. where `__thread` is emulated with
// lower level API) (Mac, Windows), we use the platform runtime API to create
// TLS variable directly.
// This is functionally equivalent to using `__thread` but can be
// more efficient since we can have better control over the creation and
// initialization of the TLS buffer.
//
// On platforms that use ELF (Linux, FreeBSD), we use a `__thread` variable
// as the fallback in the shared object. For better efficiency, we also
// create a `__thread` variable in the main executable using a static TLS
// model.
#if defined(_OS_DARWIN_)
// Mac doesn't seem to have static TLS model so the runtime TLS getter
// registration will only add overhead to TLS access. The `__thread` variables
// are emulated with `pthread_key_t` so it is actually faster to use it directly.
static pthread_key_t jl_pgcstack_key;

__attribute__((constructor)) void jl_init_tls(void)
{
    pthread_key_create(&jl_pgcstack_key, NULL);
}

JL_CONST_FUNC jl_gcframe_t **jl_get_pgcstack(void) JL_NOTSAFEPOINT
{
    return (jl_gcframe_t**)pthread_getspecific(jl_pgcstack_key);
}

void jl_set_pgcstack(jl_gcframe_t **pgcstack) JL_NOTSAFEPOINT
{
    pthread_setspecific(jl_pgcstack_key, (void*)pgcstack);
}

void jl_pgcstack_getkey(jl_get_pgcstack_func **f, pthread_key_t *k)
{
    // for codegen
    *f = pthread_getspecific;
    *k = jl_pgcstack_key;
}


JL_DLLEXPORT void jl_pgcstack_setkey(jl_get_pgcstack_func *f, pthread_key_t k)
{
    jl_safe_printf("ERROR: Attempt to change TLS address.\n");
}

#elif defined(_OS_WINDOWS_)
// Apparently windows doesn't have a static TLS model (or one that can be
// reliably used from a shared library) either..... Use `TLSAlloc` instead.

static DWORD jl_pgcstack_key;
static DWORD jl_safe_restore_key;

// Put this here for now. We can move this out later if we find more use for it.
BOOLEAN WINAPI DllMain(IN HINSTANCE hDllHandle, IN DWORD nReason,
                       IN LPVOID Reserved)
{
    jl_task_t *ct;
    switch (nReason) {
    case DLL_PROCESS_ATTACH:
        jl_pgcstack_key = TlsAlloc();
        assert(jl_pgcstack_key != TLS_OUT_OF_INDEXES);
        jl_safe_restore_key = TlsAlloc();
        assert(jl_safe_restore_key != TLS_OUT_OF_INDEXES);
        break;
    case DLL_PROCESS_DETACH:
        TlsFree(jl_pgcstack_key);
        TlsFree(jl_safe_restore_key);
        break;
    case DLL_THREAD_ATTACH:
        // will call jl_adopt_thread lazily on-demand
        break;
    case DLL_THREAD_DETACH:
        ct = jl_get_current_task();
        if (ct != NULL)
            jl_delete_thread((void*)ct->ptls);
        break;
    }
    return 1; // success
}

#if defined(_CPU_X86_64_)
#define SAVE_ERRNO \
    DWORD *plast_error = (DWORD*)(__readgsqword(0x30) + 0x68); \
    DWORD last_error = *plast_error
#define LOAD_ERRNO \
    *plast_error = last_error
#elif defined(_CPU_X86_)
#define SAVE_ERRNO \
    DWORD *plast_error = (DWORD*)(__readfsdword(0x18) + 0x34); \
    DWORD last_error = *plast_error
#define LOAD_ERRNO \
    *plast_error = last_error
#else
#define SAVE_ERRNO \
    DWORD last_error = GetLastError()
#define LOAD_ERRNO \
    SetLastError(last_error)
#endif

JL_DLLEXPORT jl_jmp_buf *jl_get_safe_restore(void)
{
    SAVE_ERRNO;
    jl_jmp_buf *sr = (jl_jmp_buf*)TlsGetValue(jl_safe_restore_key);
    LOAD_ERRNO;
    return sr;
}

JL_DLLEXPORT void jl_set_safe_restore(jl_jmp_buf *sr)
{
    SAVE_ERRNO;
    TlsSetValue(jl_safe_restore_key, (void*)sr);
    LOAD_ERRNO;
}

JL_CONST_FUNC jl_gcframe_t **jl_get_pgcstack(void) JL_NOTSAFEPOINT
{
    SAVE_ERRNO;
    jl_gcframe_t **pgcstack = (jl_gcframe_t**)TlsGetValue(jl_pgcstack_key);
    LOAD_ERRNO;
    return pgcstack;
}

void jl_set_pgcstack(jl_gcframe_t **pgcstack) JL_NOTSAFEPOINT
{
    // n.b.: this smashes GetLastError
    TlsSetValue(jl_pgcstack_key, (void*)pgcstack);
}

void jl_pgcstack_getkey(jl_get_pgcstack_func **f, DWORD *k)
{
    // for codegen
    *f = jl_get_pgcstack;
    *k = jl_pgcstack_key;
}

JL_DLLEXPORT void jl_pgcstack_setkey(jl_get_pgcstack_func *f, DWORD k)
{
    jl_safe_printf("ERROR: Attempt to change TLS address.\n");
}


#else
// We use the faster static version in the main executable to replace
// the slower version in the shared object. The code in different libraries
// or executables, however, have to agree on which version to use.
// The general solution is to add one more indirection in the C entry point.
//
// When `ifunc` is available, we can use it to trick the linker to use the
// real address (`jl_get_pgcstack_static`) directly as the symbol address.
//
// However, since the detection of the static version in `ifunc`
// is not guaranteed to be reliable, we still need to fallback to the wrapper
// version as the symbol address if we didn't find the static version in `ifunc`.

// fallback provided for embedding
static jl_pgcstack_key_t jl_pgcstack_key;
static __thread jl_gcframe_t **pgcstack_;
static jl_gcframe_t **jl_get_pgcstack_fallback(void) JL_NOTSAFEPOINT
{
    return pgcstack_;
}
static jl_gcframe_t ***jl_pgcstack_addr_fallback(void) JL_NOTSAFEPOINT
{
    return &pgcstack_;
}
void jl_set_pgcstack(jl_gcframe_t **pgcstack) JL_NOTSAFEPOINT
{
    *jl_pgcstack_key() = pgcstack;
}
#  if JL_USE_IFUNC
JL_DLLEXPORT __attribute__((weak))
void jl_register_pgcstack_getter(void);
#  endif
static jl_gcframe_t **jl_get_pgcstack_init(void);
static jl_get_pgcstack_func *jl_get_pgcstack_cb = jl_get_pgcstack_init;
static jl_gcframe_t **jl_get_pgcstack_init(void)
{
    // This 2-step initialization is used to detect calling
    // `jl_pgcstack_getkey` after the address of the TLS variables
    // are used. Since the address of TLS variables should be constant,
    // changing the getter address can result in weird crashes.

    // This is clearly not thread-safe but should be fine since we
    // make sure the tls states callback is finalized before adding
    // multiple threads
#  if JL_USE_IFUNC
    if (jl_register_pgcstack_getter)
        jl_register_pgcstack_getter();
    else
#  endif
    {
        jl_get_pgcstack_cb = jl_get_pgcstack_fallback;
        jl_pgcstack_key = &jl_pgcstack_addr_fallback;
    }
    return jl_get_pgcstack_cb();
}

JL_DLLEXPORT void jl_pgcstack_setkey(jl_get_pgcstack_func *f, jl_pgcstack_key_t k)
{
    if (f == jl_get_pgcstack_cb || !f)
        return;
    // only allow setting this once
    if (jl_get_pgcstack_cb != jl_get_pgcstack_init) {
        jl_safe_printf("ERROR: Attempt to change TLS address.\n");
        exit(1);
    }
    jl_get_pgcstack_cb = f;
    jl_pgcstack_key = k;
}

JL_DLLEXPORT jl_gcframe_t **jl_get_pgcstack(void) JL_GLOBALLY_ROOTED
{
#ifndef __clang_gcanalyzer__
    return jl_get_pgcstack_cb();
#endif
}

void jl_pgcstack_getkey(jl_get_pgcstack_func **f, jl_pgcstack_key_t *k)
{
#ifndef __clang_gcanalyzer__
    if (jl_get_pgcstack_cb == jl_get_pgcstack_init)
        jl_get_pgcstack_init();
#endif
    // for codegen
    *f = jl_get_pgcstack_cb;
    *k = jl_pgcstack_key;
}
#endif

static uv_mutex_t tls_lock; // controls write-access to these variables:
_Atomic(jl_ptls_t*) jl_all_tls_states JL_GLOBALLY_ROOTED;
int jl_all_tls_states_size;
static uv_cond_t cond;
// concurrent reads are permitted, using the same pattern as mtsmall_arraylist
// it is implemented separately because the API of direct jl_all_tls_states use is already widely prevalent

// return calling thread's ID
JL_DLLEXPORT int16_t jl_threadid(void)
{
    return jl_atomic_load_relaxed(&jl_current_task->tid);
}

JL_DLLEXPORT int8_t jl_threadpoolid(int16_t tid) JL_NOTSAFEPOINT
{
    int nthreads = jl_atomic_load_acquire(&jl_n_threads);
    if (tid < 0 || tid >= nthreads)
        jl_error("invalid tid");
    int n = 0;
    for (int i = 0; i < jl_n_threadpools; i++) {
        n += jl_n_threads_per_pool[i];
        if (tid < n)
            return (int8_t)i;
    }
    return -1; // everything else uses threadpool -1 (does not belong to any threadpool)
}

jl_ptls_t jl_init_threadtls(int16_t tid)
{
#ifndef _OS_WINDOWS_
    if (pthread_getspecific(jl_task_exit_key))
        abort();
#endif
    if (jl_get_pgcstack() != NULL)
        abort();
    jl_ptls_t ptls = (jl_ptls_t)calloc(1, sizeof(jl_tls_states_t));
#ifndef _OS_WINDOWS_
    pthread_setspecific(jl_task_exit_key, (void*)ptls);
#endif
    ptls->system_id = uv_thread_self();
    ptls->rngseed = jl_rand();
    if (tid == 0)
        ptls->disable_gc = 1;
#ifdef _OS_WINDOWS_
    if (tid == 0) {
        if (!DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                             GetCurrentProcess(), &hMainThread, 0,
                             FALSE, DUPLICATE_SAME_ACCESS)) {
            jl_printf(JL_STDERR, "WARNING: failed to access handle to main thread\n");
            hMainThread = INVALID_HANDLE_VALUE;
        }
    }
#endif
    jl_atomic_store_relaxed(&ptls->gc_state, 0); // GC unsafe
    // Conditionally initialize the safepoint address. See comment in
    // `safepoint.c`
    if (tid == 0) {
        ptls->safepoint = (size_t*)(jl_safepoint_pages + jl_page_size);
    }
    else {
        ptls->safepoint = (size_t*)(jl_safepoint_pages + jl_page_size * 2 +
                                    sizeof(size_t));
    }
    jl_bt_element_t *bt_data = (jl_bt_element_t*)
        malloc_s(sizeof(jl_bt_element_t) * (JL_MAX_BT_SIZE + 1));
    memset(bt_data, 0, sizeof(jl_bt_element_t) * (JL_MAX_BT_SIZE + 1));
    ptls->bt_data = bt_data;
    small_arraylist_new(&ptls->locks, 0);
    jl_init_thread_heap(ptls);

    uv_mutex_init(&ptls->sleep_lock);
    uv_cond_init(&ptls->wake_signal);

    uv_mutex_lock(&tls_lock);
    if (tid == -1)
        tid = jl_atomic_load_relaxed(&jl_n_threads);
    ptls->tid = tid;
    jl_ptls_t *allstates = jl_atomic_load_relaxed(&jl_all_tls_states);
    if (jl_all_tls_states_size <= tid) {
        int i, newsize = jl_all_tls_states_size + tid + 2;
        jl_ptls_t *newpptls = (jl_ptls_t*)calloc(newsize, sizeof(jl_ptls_t));
        for (i = 0; i < jl_all_tls_states_size; i++) {
            newpptls[i] = allstates[i];
        }
        jl_atomic_store_release(&jl_all_tls_states, newpptls);
        jl_all_tls_states_size = newsize;
        jl_gc_add_quiescent(ptls, (void**)allstates, free);
        allstates = newpptls;
    }
    allstates[tid] = ptls;
    if (jl_atomic_load_relaxed(&jl_n_threads) < tid + 1)
        jl_atomic_store_release(&jl_n_threads, tid + 1);
    jl_fence();
    uv_mutex_unlock(&tls_lock);

    return ptls;
}

JL_DLLEXPORT jl_gcframe_t **jl_adopt_thread(void)
{
    // `jl_init_threadtls` puts us in a GC unsafe region, so ensure GC isn't running.
    // we can't use a normal safepoint because we don't have signal handlers yet.
    // we also can't use jl_safepoint_wait_gc because that assumes we're in a task.
    jl_atomic_fetch_add(&jl_gc_disable_counter, 1);
    while (jl_atomic_load_acquire(&jl_gc_running)) {
        jl_cpu_pause();
    }
    // this check is coupled with the one in `jl_safepoint_wait_gc`, where we observe if a
    // foreign thread has asked to disable the GC, guaranteeing the order of events.

    // initialize this thread (assign tid, create heap, set up root task)
    jl_ptls_t ptls = jl_init_threadtls(-1);
    void *stack_lo, *stack_hi;
    jl_init_stack_limits(0, &stack_lo, &stack_hi);

    // warning: this changes `jl_current_task`, so be careful not to call that from this function
    jl_task_t *ct = jl_init_root_task(ptls, stack_lo, stack_hi); // assumes the GC is disabled
    JL_GC_PROMISE_ROOTED(ct);
    uv_random(NULL, NULL, &ct->rngState, sizeof(ct->rngState), 0, NULL);
    jl_atomic_fetch_add(&jl_gc_disable_counter, -1);
    return &ct->gcstack;
}

void jl_task_frame_noreturn(jl_task_t *ct) JL_NOTSAFEPOINT;

static void jl_delete_thread(void *value) JL_NOTSAFEPOINT_ENTER
{
#ifndef _OS_WINDOWS_
    pthread_setspecific(jl_task_exit_key, NULL);
#endif
    jl_ptls_t ptls = (jl_ptls_t)value;
    // safepoint until GC exit, in case GC was running concurrently while in
    // prior unsafe-region (before we let it release the stack memory)
    (void)jl_gc_unsafe_enter(ptls);
    jl_atomic_store_relaxed(&ptls->sleep_check_state, 2); // dead, interpreted as sleeping and unwakeable
    jl_fence();
    jl_wakeup_thread(0); // force thread 0 to see that we do not have the IO lock (and am dead)
    // Acquire the profile write lock, to ensure we are not racing with the `kill`
    // call in the profile code which will also try to look at this thread.
    // We have no control over when the user calls pthread_join, so we must do
    // this here by blocking. This also synchronizes our read of `current_task`
    // (which is the flag we currently use to check the liveness state of a thread).
#ifdef _OS_WINDOWS_
    jl_lock_profile_wr();
#elif defined(JL_DISABLE_LIBUNWIND)
    // nothing
#elif defined(__APPLE__)
    jl_lock_profile_wr();
#else
    pthread_mutex_lock(&in_signal_lock);
#endif
    // need to clear pgcstack and eh, but we can clear everything now too
    jl_task_frame_noreturn(jl_atomic_load_relaxed(&ptls->current_task));
    if (jl_set_task_tid(ptls->root_task, ptls->tid)) {
        // the system will probably free this stack memory soon
        // so prevent any other thread from accessing it later
        jl_task_frame_noreturn(ptls->root_task);
    }
    else {
        // Uh oh. The user cleared the sticky bit so it started running
        // elsewhere, then called pthread_exit on this thread. This is not
        // recoverable. Though we could just hang here, a fatal message is better.
        jl_safe_printf("fatal: thread exited from wrong Task.\n");
        abort();
    }
    jl_atomic_store_relaxed(&ptls->current_task, NULL); // dead
    // finally, release all of the locks we had grabbed
#ifdef _OS_WINDOWS_
    jl_unlock_profile_wr();
#elif defined(JL_DISABLE_LIBUNWIND)
    // nothing
#elif defined(__APPLE__)
    jl_unlock_profile_wr();
#else
    pthread_mutex_unlock(&in_signal_lock);
#endif
    // then park in safe-region
    (void)jl_gc_safe_enter(ptls);
}

//// debugging hack: if we are exiting too fast for error message printing on threads,
//// enabling this will stall that first thread just before exiting, to give
//// the other threads time to fail and emit their failure message
//__attribute__((destructor)) static void _waitthreaddeath(void) { sleep(1); }

JL_DLLEXPORT jl_mutex_t jl_codegen_lock;
jl_mutex_t typecache_lock;

JL_DLLEXPORT ssize_t jl_tls_offset = -1;

#ifdef JL_ELF_TLS_VARIANT
JL_DLLEXPORT const int jl_tls_elf_support = 1;
// Optimize TLS access in codegen if the TLS buffer is using a IE or LE model.
// To detect such case, we find the size of the TLS segment in the main
// executable and the thread pointer (TP) and then see if the TLS pointer on the
// current thread is in the right range.
// This can in principle be extended to the case where the TLS buffer is
// in the shared library but is part of the static buffer but that seems harder
// to detect.
#  if JL_ELF_TLS_VARIANT == 1
// In Variant 1, the static TLS buffer comes after a fixed size TCB.
// The alignment needs to be applied to the original size.
static inline size_t jl_add_tls_size(size_t orig_size, size_t size, size_t align)
{
    return LLT_ALIGN(orig_size, align) + size;
}
static inline ssize_t jl_check_tls_bound(void *tp, jl_gcframe_t ***k0, size_t tls_size)
{
    ssize_t offset = (char*)k0 - (char*)tp;
    if (offset < JL_ELF_TLS_INIT_SIZE ||
        (size_t)offset + sizeof(*k0) > tls_size)
        return -1;
    return offset;
}
#  elif JL_ELF_TLS_VARIANT == 2
// In Variant 2, the static TLS buffer comes before a unknown size TCB.
// The alignment needs to be applied to the new size.
static inline size_t jl_add_tls_size(size_t orig_size, size_t size, size_t align)
{
    return LLT_ALIGN(orig_size + size, align);
}
static inline ssize_t jl_check_tls_bound(void *tp, jl_gcframe_t ***k0, size_t tls_size)
{
    ssize_t offset = (char*)tp - (char*)k0;
    if (offset < sizeof(*k0) || offset > tls_size)
        return -1;
    return -offset;
}
#  else
#    error "Unknown static TLS variant"
#  endif

// Find the size of the TLS segment in the main executable
typedef struct {
    size_t total_size;
} check_tls_cb_t;

static int check_tls_cb(struct dl_phdr_info *info, size_t size, void *_data)
{
    check_tls_cb_t *data = (check_tls_cb_t*)_data;
    const ElfW(Phdr) *phdr = info->dlpi_phdr;
    unsigned phnum = info->dlpi_phnum;
    size_t total_size = JL_ELF_TLS_INIT_SIZE;

    for (unsigned i = 0; i < phnum; i++) {
        const ElfW(Phdr) *seg = &phdr[i];
        if (seg->p_type != PT_TLS)
            continue;
        // There should be only one TLS segment
        // Variant II
        total_size = jl_add_tls_size(total_size, seg->p_memsz, seg->p_align);
    }
    data->total_size = total_size;
    // only run once (on the main executable)
    return 1;
}

static void jl_check_tls(void)
{
    jl_get_pgcstack_func *f;
    jl_gcframe_t ***(*k)(void);
    jl_pgcstack_getkey(&f, &k);
    jl_gcframe_t ***k0 = k();
    if (k0 == NULL)
        return;
    check_tls_cb_t data = {0};
    dl_iterate_phdr(check_tls_cb, &data);
    if (data.total_size == 0)
        return;
    void *tp; // Thread pointer
#if defined(_CPU_X86_64_)
    asm("movq %%fs:0, %0" : "=r"(tp));
#elif defined(_CPU_X86_)
    asm("movl %%gs:0, %0" : "=r"(tp));
#elif defined(_CPU_AARCH64_)
    asm("mrs %0, tpidr_el0" : "=r"(tp));
#elif defined(__ARM_ARCH) && __ARM_ARCH >= 7
    asm("mrc p15, 0, %0, c13, c0, 3" : "=r"(tp));
#else
#  error "Cannot emit thread pointer for this architecture."
#endif
    ssize_t offset = jl_check_tls_bound(tp, k0, data.total_size);
    if (offset == -1)
        return;
    jl_tls_offset = offset;
}
#else
// !JL_ELF_TLS_VARIANT
JL_DLLEXPORT const int jl_tls_elf_support = 0;
#endif

extern int jl_n_markthreads;
extern int jl_n_sweepthreads;
extern int gc_first_tid;

// interface to Julia; sets up to make the runtime thread-safe
void jl_init_threading(void)
{
    char *cp;

    uv_mutex_init(&tls_lock);
    uv_cond_init(&cond);
#ifdef JL_ELF_TLS_VARIANT
    jl_check_tls();
#endif

    // Determine how many threads and pools are requested. This may have been
    // specified on the command line (and so are in `jl_options`) or by the
    // environment variable. Set the globals `jl_n_threadpools`, `jl_n_threads`
    // and `jl_n_threads_per_pool`.
    jl_n_threadpools = 2;
    int16_t nthreads = JULIA_NUM_THREADS;
    int16_t nthreadsi = 0;
    char *endptr, *endptri;

    if (jl_options.nthreads != 0) { // --threads specified
        nthreads = jl_options.nthreads_per_pool[0];
        if (nthreads < 0)
            nthreads = jl_effective_threads();
        if (jl_options.nthreadpools == 2)
            nthreadsi = jl_options.nthreads_per_pool[1];
    }
    else if ((cp = getenv(NUM_THREADS_NAME))) { // ENV[NUM_THREADS_NAME] specified
        if (!strncmp(cp, "auto", 4)) {
            nthreads = jl_effective_threads();
            cp += 4;
        }
        else {
            errno = 0;
            nthreads = strtol(cp, &endptr, 10);
            if (errno != 0 || endptr == cp || nthreads <= 0)
                nthreads = 1;
            cp = endptr;
        }
        if (*cp == ',') {
            cp++;
            if (!strncmp(cp, "auto", 4))
                nthreadsi = 1;
            else {
                errno = 0;
                nthreadsi = strtol(cp, &endptri, 10);
                if (errno != 0 || endptri == cp || nthreadsi < 0)
                    nthreadsi = 0;
            }
        }
    }

    int cpu = jl_cpu_threads();
    jl_n_markthreads = jl_options.nmarkthreads - 1;
    jl_n_sweepthreads = jl_options.nsweepthreads;
    if (jl_n_markthreads == -1) { // --gcthreads not specified
        if ((cp = getenv(NUM_GC_THREADS_NAME))) { // ENV[NUM_GC_THREADS_NAME] specified
            errno = 0;
            jl_n_markthreads = (uint64_t)strtol(cp, &endptr, 10) - 1;
            if (errno != 0 || endptr == cp || nthreads <= 0)
                jl_n_markthreads = 0;
            cp = endptr;
            if (*cp == ',') {
                cp++;
                errno = 0;
                jl_n_sweepthreads = strtol(cp, &endptri, 10);
                if (errno != 0 || endptri == cp || jl_n_sweepthreads < 0) {
                    jl_n_sweepthreads = 0;
                }
            }
        }
        else {
            // if `--gcthreads` or ENV[NUM_GCTHREADS_NAME] was not specified,
            // set the number of GC threads to the number of compute threads
            if (nthreads <= 1) {
                jl_n_markthreads = 0;
            }
            else {
                jl_n_markthreads = nthreads - 1;
            }
            // if `--gcthreads` or ENV[NUM_GCTHREADS_NAME] was not specified,
            // cap the number of threads that may run the mark phase to
            // the number of CPU cores
            if (jl_n_markthreads + 1 >= cpu) {
                jl_n_markthreads = cpu - 1;
            }
        }
    }
    // warn the user if they try to run with a number
    // of GC threads which is larger than the number
    // of physical cores
    if (jl_n_markthreads + 1 > cpu) {
        jl_safe_printf("WARNING: running Julia with %d GC threads on %d CPU cores\n", jl_n_markthreads + 1, cpu);
    }
    int16_t ngcthreads = jl_n_markthreads + jl_n_sweepthreads;

    jl_all_tls_states_size = nthreads + nthreadsi + ngcthreads;
    jl_n_threads_per_pool = (int*)malloc_s(2 * sizeof(int));
    jl_n_threads_per_pool[0] = nthreadsi;
    jl_n_threads_per_pool[1] = nthreads;

    jl_atomic_store_release(&jl_all_tls_states, (jl_ptls_t*)calloc(jl_all_tls_states_size, sizeof(jl_ptls_t)));
    jl_atomic_store_release(&jl_n_threads, jl_all_tls_states_size);
    jl_n_gcthreads = ngcthreads;
    gc_first_tid = nthreads;
}

static uv_barrier_t thread_init_done;

void jl_start_threads(void)
{
    int nthreads = jl_atomic_load_relaxed(&jl_n_threads);
    int ngcthreads = jl_n_gcthreads;
    int cpumasksize = uv_cpumask_size();
    char *cp;
    int i, exclusive;
    uv_thread_t uvtid;
    if (cpumasksize < nthreads) // also handles error case
        cpumasksize = nthreads;
    char *mask = (char*)alloca(cpumasksize);

    // do we have exclusive use of the machine? default is no
    exclusive = DEFAULT_MACHINE_EXCLUSIVE;
    cp = getenv(MACHINE_EXCLUSIVE_NAME);
    if (cp && strcmp(cp, "0") != 0)
        exclusive = 1;

    // exclusive use: affinitize threads, master thread on proc 0, rest
    // according to a 'compact' policy
    // non-exclusive: no affinity settings; let the kernel move threads about
    if (exclusive) {
        if (nthreads > jl_cpu_threads()) {
            jl_printf(JL_STDERR, "ERROR: Too many threads requested for %s option.\n", MACHINE_EXCLUSIVE_NAME);
            exit(1);
        }
        memset(mask, 0, cpumasksize);
        mask[0] = 1;
        uvtid = uv_thread_self();
        uv_thread_setaffinity(&uvtid, mask, NULL, cpumasksize);
        mask[0] = 0;
    }

    // create threads
    uv_barrier_init(&thread_init_done, nthreads);

    // GC/System threads need to be after the worker threads.
    int nworker_threads = nthreads - ngcthreads;

    for (i = 1; i < nthreads; ++i) {
        jl_threadarg_t *t = (jl_threadarg_t *)malloc_s(sizeof(jl_threadarg_t)); // ownership will be passed to the thread
        t->tid = i;
        t->barrier = &thread_init_done;
        if (i < nworker_threads) {
            uv_thread_create(&uvtid, jl_threadfun, t);
            if (exclusive) {
                mask[i] = 1;
                uv_thread_setaffinity(&uvtid, mask, NULL, cpumasksize);
                mask[i] = 0;
            }
        }
        else if (i == nthreads - 1 && jl_n_sweepthreads == 1) {
            uv_thread_create(&uvtid, jl_concurrent_gc_threadfun, t);
        }
        else {
            uv_thread_create(&uvtid, jl_parallel_gc_threadfun, t);
        }
        uv_thread_detach(&uvtid);
    }

    uv_barrier_wait(&thread_init_done);
}

_Atomic(unsigned) _threadedregion; // HACK: keep track of whether to prioritize IO or threading

JL_DLLEXPORT int jl_in_threaded_region(void)
{
    return jl_atomic_load_relaxed(&_threadedregion) != 0;
}

JL_DLLEXPORT void jl_enter_threaded_region(void)
{
    jl_atomic_fetch_add(&_threadedregion, 1);
}

JL_DLLEXPORT void jl_exit_threaded_region(void)
{
    if (jl_atomic_fetch_add(&_threadedregion, -1) == 1) {
        // make sure no more callbacks will run while user code continues
        // outside thread region and might touch an I/O object.
        JL_UV_LOCK();
        JL_UV_UNLOCK();
        // make sure thread 0 is not using the sleep_lock
        // so that it may enter the libuv event loop instead
        jl_wakeup_thread(0);
    }
}

// Profiling stubs

void _jl_mutex_init(jl_mutex_t *lock, const char *name) JL_NOTSAFEPOINT
{
    jl_atomic_store_relaxed(&lock->owner, (jl_task_t*)NULL);
    lock->count = 0;
    jl_profile_lock_init(lock, name);
}

void _jl_mutex_wait(jl_task_t *self, jl_mutex_t *lock, int safepoint)
{
    jl_task_t *owner = jl_atomic_load_relaxed(&lock->owner);
    if (owner == self) {
        lock->count++;
        return;
    }
    // Don't use JL_TIMING for instant acquires, results in large blowup of events
    jl_profile_lock_start_wait(lock);
    if (owner == NULL && jl_atomic_cmpswap(&lock->owner, &owner, self)) {
        lock->count = 1;
        jl_profile_lock_acquired(lock);
        return;
    }
    JL_TIMING(LOCK_SPIN, LOCK_SPIN);
    while (1) {
        if (owner == NULL && jl_atomic_cmpswap(&lock->owner, &owner, self)) {
            lock->count = 1;
            jl_profile_lock_acquired(lock);
            return;
        }
        if (safepoint) {
            jl_gc_safepoint_(self->ptls);
        }
        if (jl_running_under_rr(0)) {
            // when running under `rr`, use system mutexes rather than spin locking
            uv_mutex_lock(&tls_lock);
            if (jl_atomic_load_relaxed(&lock->owner))
                uv_cond_wait(&cond, &tls_lock);
            uv_mutex_unlock(&tls_lock);
        }
        jl_cpu_suspend();
        owner = jl_atomic_load_relaxed(&lock->owner);
    }
}

static void jl_lock_frame_push(jl_task_t *self, jl_mutex_t *lock)
{
    jl_ptls_t ptls = self->ptls;
    small_arraylist_t *locks = &ptls->locks;
    uint32_t len = locks->len;
    if (__unlikely(len >= locks->max)) {
        small_arraylist_grow(locks, 1);
    }
    else {
        locks->len = len + 1;
    }
    locks->items[len] = (void*)lock;
}

static void jl_lock_frame_pop(jl_task_t *self)
{
    jl_ptls_t ptls = self->ptls;
    assert(ptls->locks.len > 0);
    ptls->locks.len--;
}

void _jl_mutex_lock(jl_task_t *self, jl_mutex_t *lock)
{
    JL_SIGATOMIC_BEGIN_self();
    _jl_mutex_wait(self, lock, 1);
    jl_lock_frame_push(self, lock);
}

int _jl_mutex_trylock_nogc(jl_task_t *self, jl_mutex_t *lock)
{
    jl_task_t *owner = jl_atomic_load_acquire(&lock->owner);
    if (owner == self) {
        lock->count++;
        return 1;
    }
    if (owner == NULL && jl_atomic_cmpswap(&lock->owner, &owner, self)) {
        lock->count = 1;
        return 1;
    }
    return 0;
}

int _jl_mutex_trylock(jl_task_t *self, jl_mutex_t *lock)
{
    int got = _jl_mutex_trylock_nogc(self, lock);
    if (got) {
        JL_SIGATOMIC_BEGIN_self();
        jl_lock_frame_push(self, lock);
    }
    return got;
}

void _jl_mutex_unlock_nogc(jl_mutex_t *lock)
{
#ifndef __clang_gcanalyzer__
    assert(jl_atomic_load_relaxed(&lock->owner) == jl_current_task &&
           "Unlocking a lock in a different thread.");
    if (--lock->count == 0) {
        jl_profile_lock_release_start(lock);
        jl_atomic_store_release(&lock->owner, (jl_task_t*)NULL);
        jl_cpu_wake();
        if (jl_running_under_rr(0)) {
            // when running under `rr`, use system mutexes rather than spin locking
            uv_mutex_lock(&tls_lock);
            uv_cond_broadcast(&cond);
            uv_mutex_unlock(&tls_lock);
        }
        jl_profile_lock_release_end(lock);
    }
#endif
}

void _jl_mutex_unlock(jl_task_t *self, jl_mutex_t *lock)
{
    _jl_mutex_unlock_nogc(lock);
    jl_lock_frame_pop(self);
    JL_SIGATOMIC_END_self();
    if (jl_atomic_load_relaxed(&jl_gc_have_pending_finalizers)) {
        jl_gc_run_pending_finalizers(self); // may GC
    }
}


// Make gc alignment available for threading
// see threads.jl alignment
JL_DLLEXPORT int jl_alignment(size_t sz)
{
    return jl_gc_alignment(sz);
}

// Heartbeat mechanism for Julia's task scheduler
// ---
// Start a thread that does not participate in running Julia's tasks. This
// thread simply sleeps until the heartbeat mechanism is enabled. When
// enabled, the heartbeat thread enters a loop in which it blocks waiting
// for the specified heartbeat interval. If, within that interval,
// `jl_heartbeat()` is *not* called at least once, then the thread calls
// `jl_print_task_backtraces(0)`.

#ifdef JL_HEARTBEAT_THREAD

#include <time.h>

volatile int heartbeat_enabled;
uv_sem_t heartbeat_on_sem,              // jl_heartbeat_enable -> thread
         heartbeat_off_sem;             // thread -> jl_heartbeat_enable
int heartbeat_interval_s,
    tasks_after_n,
    reset_tasks_after_n;
int tasks_showed, n_hbs_missed, n_hbs_recvd;
_Atomic(int) heartbeats;

JL_DLLEXPORT void jl_print_task_backtraces(int show_done) JL_NOTSAFEPOINT;
void jl_heartbeat_threadfun(void *arg);

// start the heartbeat thread with heartbeats disabled
void jl_init_heartbeat(void)
{
    uv_thread_t uvtid;
    heartbeat_enabled = 0;
    uv_sem_init(&heartbeat_on_sem, 0);
    uv_sem_init(&heartbeat_off_sem, 0);
    uv_thread_create(&uvtid, jl_heartbeat_threadfun, NULL);
    uv_thread_detach(&uvtid);
}

// enable/disable heartbeats
// heartbeat_s: interval within which jl_heartbeat() must be called
// show_tasks_after_n: number of heartbeats missed before printing task backtraces
// reset_after_n: number of heartbeats after which to reset
//
// When disabling heartbeats, the heartbeat thread must wake up,
// find out that heartbeats are now diabled, and reset. For now, we
// handle this by preventing re-enabling of heartbeats until this
// completes.
JL_DLLEXPORT int jl_heartbeat_enable(int heartbeat_s, int show_tasks_after_n,
                                     int reset_after_n)
{
    if (heartbeat_s <= 0) {
        heartbeat_enabled = 0;
        heartbeat_interval_s = tasks_after_n = reset_tasks_after_n = 0;
    }
    else {
        // must disable before enabling
        if (heartbeat_enabled) {
            return -1;
        }
        // heartbeat thread must be ready
        if (uv_sem_trywait(&heartbeat_off_sem) != 0) {
            return -1;
        }

        jl_atomic_store_relaxed(&heartbeats, 0);
        heartbeat_interval_s = heartbeat_s;
        tasks_after_n = show_tasks_after_n;
        reset_tasks_after_n = reset_after_n;
        tasks_showed = 0;
        n_hbs_missed = 0;
        n_hbs_recvd = 0;
        heartbeat_enabled = 1;
        uv_sem_post(&heartbeat_on_sem); // wake the heartbeat thread
    }
    return 0;
}

// heartbeat
JL_DLLEXPORT void jl_heartbeat(void)
{
    jl_atomic_fetch_add(&heartbeats, 1);
}

// sleep the thread for the specified interval
void sleep_for(int secs, int nsecs)
{
    struct timespec rqtp, rmtp;
    rqtp.tv_sec = secs;
    rqtp.tv_nsec = nsecs;
    rmtp.tv_sec = 0;
    rmtp.tv_nsec = 0;
    for (; ;) {
        // this suspends the thread so we aren't using CPU
        if (nanosleep(&rqtp, &rmtp) == 0) {
            return;
        }
        // TODO: else if (errno == EINTR)
        // this could be SIGTERM and we should shutdown but how to find out?
        rqtp = rmtp;
    }
}

// check for heartbeats and maybe report loss
uint8_t check_heartbeats(uint8_t gc_state)
{
    int hb = jl_atomic_exchange(&heartbeats, 0);

    if (hb <= 0) {
        // we didn't get a heartbeat
        n_hbs_recvd = 0;
        n_hbs_missed++;

        // if we've printed task backtraces already, do nothing
        if (!tasks_showed) {
            // otherwise, at least show this message
            jl_safe_printf("==== heartbeat loss (%ds) ====\n",
                           n_hbs_missed * heartbeat_interval_s);
            // if we've missed enough heartbeats, print task backtraces
            if (n_hbs_missed >= tasks_after_n) {
                jl_task_t *ct = jl_current_task;
                jl_ptls_t ptls = ct->ptls;

                // exit GC-safe region to report then re-enter
                jl_gc_safe_leave(ptls, gc_state);
                jl_print_task_backtraces(0);
                gc_state = jl_gc_safe_enter(ptls);

                // we printed task backtraces
                tasks_showed = 1;
            }
        }
    }
    else {
        // got a heartbeat
        n_hbs_recvd++;
        // if we'd printed task backtraces, check for reset
        if (tasks_showed && n_hbs_recvd >= reset_tasks_after_n) {
            tasks_showed = 0;
            jl_safe_printf("==== heartbeats recovered (lost for %ds) ====\n",
                           n_hbs_missed * heartbeat_interval_s);
        }
        n_hbs_missed = 0;
    }

    return gc_state;
}

// heartbeat thread function
void jl_heartbeat_threadfun(void *arg)
{
    int s = 59, ns = 1e9 - 1, rs;
    uint64_t t0, tchb;

    // We need a TLS because backtraces are accumulated into ptls->bt_size
    // and ptls->bt_data, so we need to call jl_adopt_thread().
    jl_adopt_thread();
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;

    // Don't hold up GC, this thread doesn't participate.
    uint8_t gc_state = jl_gc_safe_enter(ptls);

    for (;;) {
        if (!heartbeat_enabled) {
            // post the off semaphore to indicate we're ready to enable
            uv_sem_post(&heartbeat_off_sem);

            // sleep the thread here; this semaphore is posted in
            // jl_heartbeat_enable()
            uv_sem_wait(&heartbeat_on_sem);

            // Set the sleep duration.
            s = heartbeat_interval_s - 1;
            ns = 1e9 - 1;
            continue;
        }

        // heartbeat is enabled; sleep, waiting for the desired interval
        sleep_for(s, ns);

        // if heartbeats were turned off while we were sleeping, reset
        if (!heartbeat_enabled) {
            continue;
        }

        // check if any heartbeats have happened, report as appropriate
        t0 = jl_hrtime();
        gc_state = check_heartbeats(gc_state);
        tchb = jl_hrtime() - t0;

        // adjust the next sleep duration based on how long the heartbeat
        // check took
        rs = 1;
        while (tchb > 1e9) {
            rs++;
            tchb -= 1e9;
        }
        s = heartbeat_interval_s - rs;
        ns = 1e9 - tchb;
    }
}

#else // !JL_HEARTBEAT_THREAD

void jl_init_heartbeat(void)
{
}

JL_DLLEXPORT int jl_heartbeat_enable(int heartbeat_s, int show_tasks_after_n,
                                     int reset_after_n)
{
    return -1;
}

JL_DLLEXPORT void jl_heartbeat(void)
{
}

#endif // JL_HEARTBEAT_THREAD

#ifdef __cplusplus
}
#endif
