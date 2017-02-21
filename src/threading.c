// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  threading infrastructure
  . thread and threadgroup creation
  . thread function
  . invoke Julia function from multiple threads

TODO:
  . fix interface to properly support thread groups
  . add queue per thread for tasks
  . add reduction; reduce values returned from thread function
  . make code generation thread-safe and remove the lock
*/

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include "julia.h"
#include "julia_internal.h"

#ifdef _OS_LINUX_
#  if defined(_CPU_X86_64_) || defined(_CPU_X86_)
#    define JL_ELF_TLS_VARIANT 2
#    define JL_ELF_TLS_INIT_SIZE 0
#  endif
#  if defined(_CPU_AARCH64_)
#    define JL_ELF_TLS_VARIANT 1
#    define JL_ELF_TLS_INIT_SIZE 16
#  endif
#endif

#ifdef JL_ELF_TLS_VARIANT
#  include <link.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include "threadgroup.h"
#include "threading.h"

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
#ifdef JULIA_ENABLE_THREADING
#  if defined(_OS_DARWIN_)
// Mac doesn't seem to have static TLS model so the runtime TLS getter
// registration will only add overhead to TLS access. The `__thread` variables
// are emulated with `pthread_key_t` so it is actually faster to use it directly.
static pthread_key_t jl_tls_key;

__attribute__((constructor)) void jl_mac_init_tls(void)
{
    pthread_key_create(&jl_tls_key, NULL);
}

JL_DLLEXPORT JL_CONST_FUNC jl_ptls_t (jl_get_ptls_states)(void)
{
    void *ptls = pthread_getspecific(jl_tls_key);
    if (__unlikely(!ptls)) {
        ptls = calloc(1, sizeof(jl_tls_states_t));
        pthread_setspecific(jl_tls_key, ptls);
    }
    return (jl_ptls_t)ptls;
}

// This is only used after the tls is already initialized on the thread
static JL_CONST_FUNC jl_ptls_t jl_get_ptls_states_fast(void)
{
    return (jl_ptls_t)pthread_getspecific(jl_tls_key);
}

jl_get_ptls_states_func jl_get_ptls_states_getter(void)
{
    // for codegen
    return &jl_get_ptls_states_fast;
}
#  elif defined(_OS_WINDOWS_)
// Apparently windows doesn't have a static TLS model (or one that can be
// reliably used from a shared library) either..... Use `TLSAlloc` instead.

static DWORD jl_tls_key;

// Put this here for now. We can move this out later if we find more use for it.
BOOLEAN WINAPI DllMain(IN HINSTANCE hDllHandle, IN DWORD nReason,
                       IN LPVOID Reserved)
{
    switch (nReason) {
    case DLL_PROCESS_ATTACH:
        jl_tls_key = TlsAlloc();
        assert(jl_tls_key != TLS_OUT_OF_INDEXES);
        // Fall through
    case DLL_THREAD_ATTACH:
        TlsSetValue(jl_tls_key, calloc(1, sizeof(jl_tls_states_t)));
        break;
    case DLL_THREAD_DETACH:
        free(TlsGetValue(jl_tls_key));
        TlsSetValue(jl_tls_key, NULL);
        break;
    case DLL_PROCESS_DETACH:
        free(TlsGetValue(jl_tls_key));
        TlsFree(jl_tls_key);
        break;
    }
    return 1; // success
}

JL_DLLEXPORT JL_CONST_FUNC jl_ptls_t (jl_get_ptls_states)(void)
{
    return (jl_ptls_t)TlsGetValue(jl_tls_key);
}

jl_get_ptls_states_func jl_get_ptls_states_getter(void)
{
    // for codegen
    return &jl_get_ptls_states;
}
#  else
// We use the faster static version in the main executable to replace
// the slower version in the shared object. The code in different libraries
// or executables, however, have to agree on which version to use.
// The general solution is to add one more indirection in the C entry point
// (see `jl_get_ptls_states_wrapper`).
//
// When `ifunc` is available, we can use it to trick the linker to use the
// real address (`jl_get_ptls_states_static`) directly as the symbol address.
// (see `jl_get_ptls_states_resolve`).
//
// However, since the detection of the static version in `ifunc`
// is not guaranteed to be reliable, we still need to fallback to the wrapper
// version as the symbol address if we didn't find the static version in `ifunc`.
#if defined(__GLIBC__) && defined(JULIA_HAS_IFUNC_SUPPORT)
// Make sure both the compiler and the glibc supports it.
// Only enable this on known working glibc versions.
#  if (defined(_CPU_X86_) || defined(_CPU_X86_64_)) && __GLIBC_PREREQ(2, 12)
#    define JL_TLS_USE_IFUNC
#  elif (defined(_CPU_ARM_) || defined(_CPU_AARCH64_)) && __GLIBC_PREREQ(2, 18)
// This is the oldest tested version that supports ifunc.
#    define JL_TLS_USE_IFUNC
#  endif
// TODO: PPC probably supports ifunc on some glibc versions too
#endif
// fallback provided for embedding
static JL_CONST_FUNC jl_ptls_t jl_get_ptls_states_fallback(void)
{
    static __thread jl_tls_states_t tls_states;
    return &tls_states;
}
#ifdef JL_TLS_USE_IFUNC
JL_DLLEXPORT JL_CONST_FUNC __attribute__((weak))
jl_ptls_t jl_get_ptls_states_static(void);
#endif
static jl_ptls_t jl_get_ptls_states_init(void);
static jl_get_ptls_states_func jl_tls_states_cb = jl_get_ptls_states_init;
static jl_ptls_t jl_get_ptls_states_init(void)
{
    // This 2-step initialization is used to detect calling
    // `jl_set_ptls_states_getter` after the address of the TLS variables
    // are used. Since the address of TLS variables should be constant,
    // changing the getter address can result in wierd crashes.

    // This is clearly not thread safe but should be fine since we
    // make sure the tls states callback is finalized before adding
    // multiple threads
    jl_get_ptls_states_func cb = jl_get_ptls_states_fallback;
#ifdef JL_TLS_USE_IFUNC
    if (jl_get_ptls_states_static)
        cb = jl_get_ptls_states_static;
#endif
    jl_tls_states_cb = cb;
    return cb();
}

static JL_CONST_FUNC jl_ptls_t jl_get_ptls_states_wrapper(void)
{
    return (*jl_tls_states_cb)();
}

JL_DLLEXPORT void jl_set_ptls_states_getter(jl_get_ptls_states_func f)
{
    if (f == jl_tls_states_cb || !f)
        return;
    // only allow setting this once
    if (jl_tls_states_cb == jl_get_ptls_states_init) {
        jl_tls_states_cb = f;
    }
    else {
        jl_safe_printf("ERROR: Attempt to change TLS address.\n");
        exit(1);
    }
}

#ifdef JL_TLS_USE_IFUNC
static jl_get_ptls_states_func jl_get_ptls_states_resolve(void)
{
    if (jl_tls_states_cb != jl_get_ptls_states_init)
        return jl_tls_states_cb;
    // If we can't find the static version, return the wrapper instead
    // of the slow version so that we won't resolve to the slow version
    // due to issues in the relocation order.
    // This may not be necessary once `ifunc` support in glibc is more mature.
    if (!jl_get_ptls_states_static)
        return jl_get_ptls_states_wrapper;
    jl_tls_states_cb = jl_get_ptls_states_static;
    return jl_tls_states_cb;
}

JL_DLLEXPORT JL_CONST_FUNC jl_ptls_t (jl_get_ptls_states)(void)
    __attribute__((ifunc ("jl_get_ptls_states_resolve")));
#else // JL_TLS_USE_IFUNC
JL_DLLEXPORT JL_CONST_FUNC jl_ptls_t (jl_get_ptls_states)(void)
{
    return jl_get_ptls_states_wrapper();
}
#endif // JL_TLS_USE_IFUNC
jl_get_ptls_states_func jl_get_ptls_states_getter(void)
{
    if (jl_tls_states_cb == jl_get_ptls_states_init)
        jl_get_ptls_states_init();
    // for codegen
    return jl_tls_states_cb;
}
#  endif
#else
JL_DLLEXPORT jl_tls_states_t jl_tls_states;
JL_DLLEXPORT JL_CONST_FUNC jl_ptls_t (jl_get_ptls_states)(void)
{
    return &jl_tls_states;
}
#endif

// thread ID
JL_DLLEXPORT int jl_n_threads;     // # threads we're actually using
jl_ptls_t *jl_all_tls_states;

// return calling thread's ID
// Also update the suspended_threads list in signals-mach when changing the
// type of the thread id.
JL_DLLEXPORT int16_t jl_threadid(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return ptls->tid;
}

static void ti_initthread(int16_t tid)
{
    jl_ptls_t ptls = jl_get_ptls_states();
#ifndef _OS_WINDOWS_
    ptls->system_id = pthread_self();
#endif
    ptls->tid = tid;
    ptls->pgcstack = NULL;
    ptls->gc_state = 0; // GC unsafe
    ptls->gc_cache.perm_scanned_bytes = 0;
    ptls->gc_cache.scanned_bytes = 0;
    ptls->gc_cache.nbig_obj = 0;
    // Conditionally initialize the safepoint address. See comment in
    // `safepoint.c`
    if (tid == 0) {
        ptls->safepoint = (size_t*)(jl_safepoint_pages + jl_page_size);
    }
    else {
        ptls->safepoint = (size_t*)(jl_safepoint_pages + jl_page_size * 2 +
                                    sizeof(size_t));
    }
    ptls->defer_signal = 0;
    ptls->current_module = NULL;
    void *bt_data = malloc(sizeof(uintptr_t) * (JL_MAX_BT_SIZE + 1));
    if (bt_data == NULL) {
        jl_printf(JL_STDERR, "could not allocate backtrace buffer\n");
        gc_debug_critical_error();
        abort();
    }
    ptls->bt_data = (uintptr_t*)bt_data;
    jl_mk_thread_heap(ptls);
    jl_install_thread_signal_handler(ptls);

    jl_all_tls_states[tid] = ptls;
}

static void ti_init_master_thread(void)
{
#ifdef _OS_WINDOWS_
    if (!DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                         GetCurrentProcess(), &hMainThread, 0,
                         TRUE, DUPLICATE_SAME_ACCESS)) {
        jl_printf(JL_STDERR, "WARNING: failed to access handle to main thread\n");
        hMainThread = INVALID_HANDLE_VALUE;
    }
#endif
    ti_initthread(0);
}

// all threads call this function to run user code
static jl_value_t *ti_run_fun(jl_svec_t *args)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_TRY {
        jl_apply(jl_svec_data(args), jl_svec_len(args));
    }
    JL_CATCH {
        return ptls->exception_in_transit;
    }
    return jl_nothing;
}


// lock for code generation
jl_mutex_t codegen_lock;
jl_mutex_t typecache_lock;

#ifdef JULIA_ENABLE_THREADING

// only one thread group for now
static ti_threadgroup_t *tgworld;

// for broadcasting work to threads
static ti_threadwork_t threadwork;

#if PROFILE_JL_THREADING
uint64_t prep_ns;
uint64_t *fork_ns;
uint64_t *user_ns;
uint64_t *join_ns;
#endif

static uv_barrier_t thread_init_done;

// thread function: used by all except the main thread
void ti_threadfun(void *arg)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    ti_threadarg_t *ta = (ti_threadarg_t *)arg;
    ti_threadgroup_t *tg;
    ti_threadwork_t *work;

    // initialize this thread (set tid, create heap, etc.)
    ti_initthread(ta->tid);
    jl_init_stack_limits(0);

    // set up tasking
    jl_init_root_task(ptls->stack_lo, ptls->stack_hi - ptls->stack_lo);
#ifdef COPY_STACKS
    jl_set_base_ctx((char*)&arg);
#endif

    // set the thread-local tid and wait for a thread group
    while (jl_atomic_load_acquire(&ta->state) == TI_THREAD_INIT)
        jl_cpu_pause();

    // Assuming the functions called below doesn't contain unprotected GC
    // critical region. In general, the following part of this function
    // shouldn't call any managed code without calling `jl_gc_unsafe_enter`
    // first.
    jl_gc_state_set(ptls, JL_GC_STATE_SAFE, 0);
    uv_barrier_wait(&thread_init_done);
    // initialize this thread in the thread group
    tg = ta->tg;
    ti_threadgroup_initthread(tg, ptls->tid);

    // free the thread argument here
    free(ta);

    // work loop
    for (; ;) {
#if PROFILE_JL_THREADING
        uint64_t tstart = uv_hrtime();
#endif

        ti_threadgroup_fork(tg, ptls->tid, (void **)&work);

#if PROFILE_JL_THREADING
        uint64_t tfork = uv_hrtime();
        fork_ns[ptls->tid] += tfork - tstart;
#endif

        if (work) {
            if (work->command == TI_THREADWORK_DONE) {
                break;
            }
            else if (work->command == TI_THREADWORK_RUN) {
                // TODO: return value? reduction?
                // TODO: before we support getting return value from
                //       the work, and after we have proper GC transition
                //       support in the codegen and runtime we don't need to
                //       enter GC unsafe region when starting the work.
                int8_t gc_state = jl_gc_unsafe_enter(ptls);
                // This is probably always NULL for now
                jl_module_t *last_m = ptls->current_module;
                size_t last_age = ptls->world_age;
                JL_GC_PUSH1(&last_m);
                ptls->current_module = work->current_module;
                ptls->world_age = work->world_age;
                ti_run_fun(work->args);
                ptls->current_module = last_m;
                ptls->world_age = last_age;
                JL_GC_POP();
                jl_gc_unsafe_leave(ptls, gc_state);
            }
        }

#if PROFILE_JL_THREADING
        uint64_t tuser = uv_hrtime();
        user_ns[ptls->tid] += tuser - tfork;
#endif

        ti_threadgroup_join(tg, ptls->tid);

#if PROFILE_JL_THREADING
        uint64_t tjoin = uv_hrtime();
        join_ns[ptls->tid] += tjoin - tuser;
#endif

        // TODO:
        // nowait should skip the join, but confirm that fork is reentrant
    }
}

#if PROFILE_JL_THREADING
void ti_reset_timings(void);
#endif

ssize_t jl_tls_offset = -1;

#ifdef JL_ELF_TLS_VARIANT
// Optimize TLS access in codegen if the TLS buffer is using a IE or LE model.
// To detect such case, we find the size of the TLS segment in the main
// executable and the TIB pointer and then see if the TLS pointer on the
// current thread is in the right range.
// This can in principle be extended to the case where the TLS buffer is
// in the shared library but is part of the static buffer but that seems harder
// to detect.
#  if JL_ELF_TLS_VARIANT == 1
// In Variant 1, the static TLS buffer comes after a fixed size TIB.
// The alignment needs to be applied to the original size.
static inline size_t jl_add_tls_size(size_t orig_size, size_t size, size_t align)
{
    return LLT_ALIGN(orig_size, align) + size;
}
static inline ssize_t jl_check_tls_bound(void *tp, void *ptls, size_t tls_size)
{
    ssize_t offset = (char*)ptls - (char*)tp;
    if (offset < JL_ELF_TLS_INIT_SIZE ||
        (size_t)offset + sizeof(jl_tls_states_t) > tls_size)
        return -1;
    return offset;
}
#  elif JL_ELF_TLS_VARIANT == 2
// In Variant 2, the static TLS buffer comes before a unknown size TIB.
// The alignment needs to be applied to the new size.
static inline size_t jl_add_tls_size(size_t orig_size, size_t size, size_t align)
{
    return LLT_ALIGN(orig_size + size, align);
}
static inline ssize_t jl_check_tls_bound(void *tp, void *ptls, size_t tls_size)
{
    ssize_t offset = (char*)tp - (char*)ptls;
    if (offset < sizeof(jl_tls_states_t) || offset > tls_size)
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
    jl_ptls_t ptls = jl_get_ptls_states();
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
#else
#  error "Cannot emit thread pointer for this architecture."
#endif
    ssize_t offset = jl_check_tls_bound(tp, ptls, data.total_size);
    if (offset == -1)
        return;
    jl_tls_offset = offset;
}
#endif

// interface to Julia; sets up to make the runtime thread-safe
void jl_init_threading(void)
{
    char *cp;

#ifdef JL_ELF_TLS_VARIANT
    jl_check_tls();
#endif

    // how many threads available, usable
    int max_threads = jl_cpu_cores();
    jl_n_threads = JULIA_NUM_THREADS;
    cp = getenv(NUM_THREADS_NAME);
    if (cp) {
        jl_n_threads = (uint64_t)strtol(cp, NULL, 10);
    }
    if (jl_n_threads > max_threads)
        jl_n_threads = max_threads;
    if (jl_n_threads <= 0)
        jl_n_threads = 1;

    jl_all_tls_states = (jl_ptls_t*)malloc(jl_n_threads * sizeof(void*));

#if PROFILE_JL_THREADING
    // set up space for profiling information
    fork_ns = (uint64_t*)jl_malloc_aligned(jl_n_threads * sizeof(uint64_t), 64);
    user_ns = (uint64_t*)jl_malloc_aligned(jl_n_threads * sizeof(uint64_t), 64);
    join_ns = (uint64_t*)jl_malloc_aligned(jl_n_threads * sizeof(uint64_t), 64);
    ti_reset_timings();
#endif

    // initialize this master thread (set tid, create heap, etc.)
    ti_init_master_thread();
}

void jl_start_threads(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    char *cp, mask[UV_CPU_SETSIZE];
    int i, exclusive;
    uv_thread_t uvtid;
    ti_threadarg_t **targs;

    // do we have exclusive use of the machine? default is no
    exclusive = DEFAULT_MACHINE_EXCLUSIVE;
    cp = getenv(MACHINE_EXCLUSIVE_NAME);
    if (cp)
        exclusive = strtol(cp, NULL, 10);

    // exclusive use: affinitize threads, master thread on proc 0, rest
    // according to a 'compact' policy
    // non-exclusive: no affinity settings; let the kernel move threads about
    if (exclusive) {
        memset(mask, 0, UV_CPU_SETSIZE);
        mask[0] = 1;
        uvtid = (uv_thread_t)uv_thread_self();
        uv_thread_setaffinity(&uvtid, mask, NULL, UV_CPU_SETSIZE);
    }

    // create threads
    targs = (ti_threadarg_t **)malloc((jl_n_threads - 1) * sizeof (ti_threadarg_t *));

    uv_barrier_init(&thread_init_done, jl_n_threads);

    for (i = 0;  i < jl_n_threads - 1;  ++i) {
        targs[i] = (ti_threadarg_t *)malloc(sizeof (ti_threadarg_t));
        targs[i]->state = TI_THREAD_INIT;
        targs[i]->tid = i + 1;
        uv_thread_create(&uvtid, ti_threadfun, targs[i]);
        if (exclusive) {
            memset(mask, 0, UV_CPU_SETSIZE);
            mask[i+1] = 1;
            uv_thread_setaffinity(&uvtid, mask, NULL, UV_CPU_SETSIZE);
        }
        uv_thread_detach(&uvtid);
    }

    // set up the world thread group
    ti_threadgroup_create(1, jl_n_threads, 1, &tgworld);
    for (i = 0;  i < jl_n_threads;  ++i)
        ti_threadgroup_addthread(tgworld, i, NULL);
    ti_threadgroup_initthread(tgworld, ptls->tid);

    // give the threads the world thread group; they will block waiting for fork
    for (i = 0;  i < jl_n_threads - 1;  ++i) {
        targs[i]->tg = tgworld;
        jl_atomic_store_release(&targs[i]->state, TI_THREAD_WORK);
    }

    uv_barrier_wait(&thread_init_done);

    // free the argument array; the threads will free their arguments
    free(targs);
}

// TODO: is this needed? where/when/how to call it?
void jl_shutdown_threading(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    // stop the spinning threads by sending them a command
    ti_threadwork_t *work = &threadwork;

    work->command = TI_THREADWORK_DONE;
    ti_threadgroup_fork(tgworld, ptls->tid, (void **)&work);

    sleep(1);

    // destroy the world thread group
    ti_threadgroup_destroy(tgworld);

#if PROFILE_JL_THREADING
    jl_free_aligned(join_ns);
    jl_free_aligned(user_ns);
    jl_free_aligned(fork_ns);
    fork_ns = user_ns = join_ns = NULL;
#endif
}

// return thread's thread group
JL_DLLEXPORT void *jl_threadgroup(void) { return (void *)tgworld; }

// interface to user code: specialize and compile the user thread function
// and run it in all threads
JL_DLLEXPORT jl_value_t *jl_threading_run(jl_svec_t *args)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    // GC safe
#if PROFILE_JL_THREADING
    uint64_t tstart = uv_hrtime();
#endif

    jl_tupletype_t *argtypes = NULL;
    JL_TYPECHK(jl_threading_run, simplevector, (jl_value_t*)args);

    int8_t gc_state = jl_gc_unsafe_enter(ptls);
    JL_GC_PUSH1(&argtypes);
    argtypes = arg_type_tuple(jl_svec_data(args), jl_svec_len(args));
    jl_compile_hint(argtypes);

    threadwork.command = TI_THREADWORK_RUN;
    // TODO jb/functions: lookup and store jlcall fptr here
    threadwork.fun = NULL;
    threadwork.args = args;
    threadwork.ret = jl_nothing;
    threadwork.current_module = ptls->current_module;
    threadwork.world_age = ptls->world_age;

#if PROFILE_JL_THREADING
    uint64_t tcompile = uv_hrtime();
    prep_ns += (tcompile - tstart);
#endif

    // fork the world thread group
    ti_threadwork_t *tw = &threadwork;
    ti_threadgroup_fork(tgworld, ptls->tid, (void **)&tw);

#if PROFILE_JL_THREADING
    uint64_t tfork = uv_hrtime();
    fork_ns[ptls->tid] += (tfork - tcompile);
#endif

    // this thread must do work too (TODO: reduction?)
    tw->ret = ti_run_fun(args);

#if PROFILE_JL_THREADING
    uint64_t trun = uv_hrtime();
    user_ns[ptls->tid] += (trun - tfork);
#endif

    // wait for completion (TODO: nowait?)
    ti_threadgroup_join(tgworld, ptls->tid);

#if PROFILE_JL_THREADING
    uint64_t tjoin = uv_hrtime();
    join_ns[ptls->tid] += (tjoin - trun);
#endif

    JL_GC_POP();
    jl_gc_unsafe_leave(ptls, gc_state);

    return tw->ret;
}

#if PROFILE_JL_THREADING

void ti_reset_timings(void)
{
    int i;
    prep_ns = 0;
    for (i = 0;  i < jl_n_threads;  i++)
        fork_ns[i] = user_ns[i] = join_ns[i] = 0;
}

void ti_timings(uint64_t *times, uint64_t *min, uint64_t *max, uint64_t *avg)
{
    int i;
    *min = UINT64_MAX;
    *max = *avg = 0;
    for (i = 0;  i < jl_n_threads;  i++) {
        if (times[i] < *min)
            *min = times[i];
        if (times[i] > *max)
            *max = times[i];
        *avg += times[i];
    }
    *avg /= jl_n_threads;
}

#define NS_TO_SECS(t)        ((t) / (double)1e9)

JL_DLLEXPORT void jl_threading_profile(void)
{
    if (!fork_ns) return;

    printf("\nti profile:\n");
    printf("prep: %g (%" PRIu64 ")\n", NS_TO_SECS(prep_ns), prep_ns);

    uint64_t min, max, avg;
    ti_timings(fork_ns, &min, &max, &avg);
    printf("fork: %g (%g - %g)\n", NS_TO_SECS(min), NS_TO_SECS(max),
            NS_TO_SECS(avg));
    ti_timings(user_ns, &min, &max, &avg);
    printf("user: %g (%g - %g)\n", NS_TO_SECS(min), NS_TO_SECS(max),
            NS_TO_SECS(avg));
    ti_timings(join_ns, &min, &max, &avg);
    printf("join: %g (%g - %g)\n", NS_TO_SECS(min), NS_TO_SECS(max),
            NS_TO_SECS(avg));
}

#else //!PROFILE_JL_THREADING

JL_DLLEXPORT void jl_threading_profile(void)
{
}

#endif //!PROFILE_JL_THREADING

#else // !JULIA_ENABLE_THREADING

JL_DLLEXPORT jl_value_t *jl_threading_run(jl_svec_t *args)
{
    JL_TYPECHK(jl_threading_run, simplevector, (jl_value_t*)args);
    return ti_run_fun(args);
}

void jl_init_threading(void)
{
    static jl_ptls_t _jl_all_tls_states;
    jl_all_tls_states = &_jl_all_tls_states;
    jl_n_threads = 1;
    ti_init_master_thread();
}

void jl_start_threads(void) { }

#endif // !JULIA_ENABLE_THREADING

// Make gc alignment available for threading
// see threads.jl alignment
JL_DLLEXPORT int jl_alignment(size_t sz)
{
    return jl_gc_alignment(sz);
}

#ifdef __cplusplus
}
#endif
