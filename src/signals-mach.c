// This file is a part of Julia. License is MIT: https://julialang.org/license

// Note that this file is `#include`d by "signals-unix.c"

#include <mach/clock.h>
#include <mach/clock_types.h>
#include <mach/clock_reply.h>
#include <mach/mach_traps.h>
#include <mach/task.h>
#include <mach/mig_errors.h>
#include <AvailabilityMacros.h>
#include "mach_excServer.c"

#ifdef MAC_OS_X_VERSION_10_9
#include <sys/_types/_ucontext64.h>
#else
#define __need_ucontext64_t
#include <sys/_structs.h>
#endif

#include "julia_assert.h"

// private keymgr stuff
#define KEYMGR_GCC3_DW2_OBJ_LIST 302
enum {
  NM_ALLOW_RECURSION = 1,
  NM_RECURSION_ILLEGAL = 2
};
extern void _keymgr_set_and_unlock_processwide_ptr(unsigned int key, void *ptr);
extern int _keymgr_unlock_processwide_ptr(unsigned int key);
extern void *_keymgr_get_and_lock_processwide_ptr(unsigned int key);
extern int _keymgr_get_and_lock_processwide_ptr_2(unsigned int key, void **result);
extern int _keymgr_set_lockmode_processwide_ptr(unsigned int key, unsigned int mode);

// private dyld3/dyld4 stuff
extern void _dyld_atfork_prepare(void) __attribute__((weak_import));
extern void _dyld_atfork_parent(void) __attribute__((weak_import));
//extern void _dyld_fork_child(void) __attribute__((weak_import));

static void attach_exception_port(thread_port_t thread, int segv_only);

// low 16 bits are the thread id, the next 8 bits are the original gc_state
static arraylist_t suspended_threads;
extern uv_mutex_t safepoint_lock;
extern uv_cond_t safepoint_cond;
void jl_mach_gc_end(void)
{
    // Requires the safepoint lock to be held
    for (size_t i = 0; i < suspended_threads.len; i++) {
        uintptr_t item = (uintptr_t)suspended_threads.items[i];
        int16_t tid = (int16_t)item;
        int8_t gc_state = (int8_t)(item >> 8);
        jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
        jl_atomic_store_release(&ptls2->gc_state, gc_state);
        thread_resume(pthread_mach_thread_np(ptls2->system_id));
    }
    suspended_threads.len = 0;
}

// Suspend the thread and return `1` if the GC is running.
// Otherwise return `0`
static int jl_mach_gc_wait(jl_ptls_t ptls2,
                           mach_port_t thread, int16_t tid)
{
    uv_mutex_lock(&safepoint_lock);
    if (!jl_atomic_load_relaxed(&jl_gc_running)) {
        // relaxed, since gets set to zero only while the safepoint_lock was held
        // this means we can tell if GC is done before we got the message or
        // the safepoint was enabled for SIGINT.
        uv_mutex_unlock(&safepoint_lock);
        return 0;
    }
    // Otherwise, set the gc state of the thread, suspend and record it
    // TODO: TSAN will complain that it never saw the faulting task do an
    // atomic release (it was in the kernel). And our attempt here does
    // nothing, since we are a different thread, and it is not transitive).
    //
    // This also means we are not making this thread available for GC work.
    // Eventually, we should probably release this signal to the original
    // thread, (return KERN_FAILURE instead of KERN_SUCCESS) so that it
    // triggers a SIGSEGV and gets handled by the usual codepath for unix.
    int8_t gc_state = ptls2->gc_state;
    jl_atomic_store_release(&ptls2->gc_state, JL_GC_STATE_WAITING);
    uintptr_t item = tid | (((uintptr_t)gc_state) << 16);
    arraylist_push(&suspended_threads, (void*)item);
    thread_suspend(thread);
    uv_mutex_unlock(&safepoint_lock);
    return 1;
}

static mach_port_t segv_port = 0;

#define STR(x) #x
#define XSTR(x) STR(x)
#define HANDLE_MACH_ERROR(msg, retval) \
    if (retval != KERN_SUCCESS) { mach_error(msg XSTR(: __FILE__:__LINE__:), (retval)); jl_exit(1); }

void *mach_segv_listener(void *arg)
{
    (void)arg;
    while (1) {
        int ret = mach_msg_server(mach_exc_server, 2048, segv_port, MACH_MSG_TIMEOUT_NONE);
        jl_safe_printf("mach_msg_server: %s\n", mach_error_string(ret));
        jl_exit(128 + SIGSEGV);
    }
}


static void allocate_mach_handler()
{
    // ensure KEYMGR_GCC3_DW2_OBJ_LIST is initialized, as this requires malloc
    // and thus can deadlock when used without first initializing it.
    // Apple caused this problem in their libunwind in 10.9 (circa keymgr-28)
    // when they removed this part of the code from keymgr.
    // Much thanks to Apple for providing source code, or this would probably
    // have simply remained unsolved forever on their platform.
    // This is similar to just calling checkKeyMgrRegisteredFDEs
    // (this is quite thread-unsafe)
    if (_keymgr_set_lockmode_processwide_ptr(KEYMGR_GCC3_DW2_OBJ_LIST, NM_ALLOW_RECURSION))
        jl_error("_keymgr_set_lockmode_processwide_ptr failed");

    int16_t nthreads = jl_atomic_load_acquire(&jl_n_threads);
    arraylist_new(&suspended_threads, nthreads); // we will resize later (inside safepoint_lock), if needed
    pthread_t thread;
    pthread_attr_t attr;
    kern_return_t ret;
    mach_port_t self = mach_task_self();
    ret = mach_port_allocate(self, MACH_PORT_RIGHT_RECEIVE, &segv_port);
    HANDLE_MACH_ERROR("mach_port_allocate",ret);
    ret = mach_port_insert_right(self, segv_port, segv_port, MACH_MSG_TYPE_MAKE_SEND);
    HANDLE_MACH_ERROR("mach_port_insert_right",ret);
    // Alright, create a thread to serve as the listener for exceptions
    if (pthread_attr_init(&attr) != 0) {
        jl_error("pthread_attr_init failed");
    }
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    if (pthread_create(&thread, &attr, mach_segv_listener, NULL) != 0) {
        jl_error("pthread_create failed");
    }
    pthread_attr_destroy(&attr);
}

#ifdef LLVMLIBUNWIND
volatile mach_port_t mach_profiler_thread = 0;
static kern_return_t profiler_segv_handler(
    mach_port_t exception_port,
    mach_port_t thread,
    mach_port_t task,
    exception_type_t exception,
    mach_exception_data_t code,
    mach_msg_type_number_t codeCnt);
#endif

#if defined(_CPU_X86_64_)
typedef x86_thread_state64_t host_thread_state_t;
typedef x86_exception_state64_t host_exception_state_t;
#define MACH_THREAD_STATE x86_THREAD_STATE64
#define MACH_THREAD_STATE_COUNT x86_THREAD_STATE64_COUNT
#define HOST_EXCEPTION_STATE x86_EXCEPTION_STATE64
#define HOST_EXCEPTION_STATE_COUNT x86_EXCEPTION_STATE64_COUNT

#elif defined(_CPU_AARCH64_)
typedef arm_thread_state64_t host_thread_state_t;
typedef arm_exception_state64_t host_exception_state_t;
#define MACH_THREAD_STATE ARM_THREAD_STATE64
#define MACH_THREAD_STATE_COUNT ARM_THREAD_STATE64_COUNT
#define HOST_EXCEPTION_STATE ARM_EXCEPTION_STATE64
#define HOST_EXCEPTION_STATE_COUNT ARM_EXCEPTION_STATE64_COUNT
#endif

static void jl_call_in_state(jl_ptls_t ptls2, host_thread_state_t *state,
                             void (*fptr)(void))
{
#ifdef _CPU_X86_64_
    uintptr_t rsp = state->__rsp;
#elif defined(_CPU_AARCH64_)
    uintptr_t rsp = state->__sp;
#else
#error "julia: throw-in-context not supported on this platform"
#endif
    if (ptls2 == NULL || ptls2->signal_stack == NULL || is_addr_on_sigstack(ptls2, (void*)rsp)) {
        rsp = (rsp - 256) & ~(uintptr_t)15; // redzone and re-alignment
    }
    else {
        rsp = (uintptr_t)ptls2->signal_stack + sig_stack_size;
    }
    assert(rsp % 16 == 0);

#ifdef _CPU_X86_64_
    rsp -= sizeof(void*);
    state->__rsp = rsp; // set stack pointer
    state->__rip = (uint64_t)fptr; // "call" the function
#elif defined(_CPU_AARCH64_)
    state->__sp = rsp;
    state->__pc = (uint64_t)fptr;
    state->__lr = 0;
#else
#error "julia: throw-in-context not supported on this platform"
#endif
}

#ifdef _CPU_X86_64_
int is_write_fault(host_exception_state_t exc_state) {
    return exc_reg_is_write_fault(exc_state.__err);
}
#elif defined(_CPU_AARCH64_)
int is_write_fault(host_exception_state_t exc_state) {
    return exc_reg_is_write_fault(exc_state.__esr);
}
#else
#warning Implement this query for consistent PROT_NONE handling
int is_write_fault(host_exception_state_t exc_state) {
    return 0;
}
#endif

static void jl_throw_in_thread(jl_ptls_t ptls2, mach_port_t thread, jl_value_t *exception)
{
    unsigned int count = MACH_THREAD_STATE_COUNT;
    host_thread_state_t state;
    kern_return_t ret = thread_get_state(thread, MACH_THREAD_STATE, (thread_state_t)&state, &count);
    HANDLE_MACH_ERROR("thread_get_state", ret);
    if (1) { // XXX: !jl_has_safe_restore(ptls2)
        assert(exception);
        ptls2->bt_size =
            rec_backtrace_ctx(ptls2->bt_data, JL_MAX_BT_SIZE, (bt_context_t *)&state,
                              NULL /*current_task?*/);
        ptls2->sig_exception = exception;
    }
    jl_call_in_state(ptls2, &state, &jl_sig_throw);
    ret = thread_set_state(thread, MACH_THREAD_STATE, (thread_state_t)&state, count);
    HANDLE_MACH_ERROR("thread_set_state", ret);
}

static void segv_handler(int sig, siginfo_t *info, void *context)
{
    assert(sig == SIGSEGV || sig == SIGBUS);
    if (jl_get_safe_restore()) { // restarting jl_ or jl_unwind_stepn
        jl_task_t *ct = jl_get_current_task();
        jl_ptls_t ptls = ct == NULL ? NULL : ct->ptls;
        jl_call_in_state(ptls, (host_thread_state_t*)jl_to_bt_context(context), &jl_sig_throw);
    }
    else {
        sigdie_handler(sig, info, context);
    }
}

// n.b. mach_exc_server expects us to define this symbol locally
/* The documentation for catch_exception_raise says: A return value of
 * KERN_SUCCESS indicates that the thread is to continue from the point of
 * exception. A return value of MIG_NO_REPLY indicates that the exception was
 * handled directly and the thread was restarted or terminated by the exception
 * handler. A return value of MIG_DESTROY_REQUEST causes the kernel to try
 * another exception handler (or terminate the thread). Any other value will
 * cause mach_msg_server to remove the task and thread port references.
 *
 * However MIG_DESTROY_REQUEST does not exist, not does it appear the source
 * code for mach_msg_server ever destroy those references (only the message
 * itself).
 */
kern_return_t catch_mach_exception_raise(
    mach_port_t exception_port,
    mach_port_t thread,
    mach_port_t task,
    exception_type_t exception,
    mach_exception_data_t code,
    mach_msg_type_number_t codeCnt)
{
    unsigned int exc_count = HOST_EXCEPTION_STATE_COUNT;
    host_exception_state_t exc_state;
#ifdef LLVMLIBUNWIND
    if (thread == mach_profiler_thread) {
        return profiler_segv_handler(exception_port, thread, task, exception, code, codeCnt);
    }
#endif
    int16_t tid;
    jl_ptls_t ptls2 = NULL;
    int nthreads = jl_atomic_load_acquire(&jl_n_threads);
    for (tid = 0; tid < nthreads; tid++) {
        jl_ptls_t _ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
        if (pthread_mach_thread_np(_ptls2->system_id) == thread) {
            ptls2 = _ptls2;
            break;
        }
    }
    if (!ptls2 || ptls2->current_task == NULL) {
        // We don't know about this thread, let the kernel try another handler
        // instead. This shouldn't actually happen since we only register the
        // handler for the threads we know about.
        jl_safe_printf("ERROR: Exception handler triggered on unmanaged thread.\n");
        return KERN_INVALID_ARGUMENT;
    }
    // XXX: jl_throw_in_thread or segv_handler will eventually check this, but
    //      we would like to avoid some of this work if we could detect this earlier
    // if (jl_has_safe_restore(ptls2)) {
    //     jl_throw_in_thread(ptls2, thread, jl_stackovf_exception);
    //     return KERN_SUCCESS;
    // }
    if (ptls2->gc_state == JL_GC_STATE_WAITING)
        return KERN_FAILURE;
    if (exception == EXC_ARITHMETIC) {
        jl_throw_in_thread(ptls2, thread, jl_diverror_exception);
        return KERN_SUCCESS;
    }
    assert(exception == EXC_BAD_ACCESS); // SIGSEGV or SIGBUS
    if (codeCnt < 2 || code[0] != KERN_PROTECTION_FAILURE) // SEGV_ACCERR or BUS_ADRERR or BUS_ADRALN
        return KERN_FAILURE;
    uint64_t fault_addr = code[1];
    kern_return_t ret = thread_get_state(thread, HOST_EXCEPTION_STATE, (thread_state_t)&exc_state, &exc_count);
    HANDLE_MACH_ERROR("thread_get_state", ret);
    if (jl_addr_is_safepoint(fault_addr) && !is_write_fault(exc_state)) {
        if (jl_mach_gc_wait(ptls2, thread, tid))
            return KERN_SUCCESS;
        if (ptls2->tid != 0)
            return KERN_SUCCESS;
        if (ptls2->defer_signal) {
            jl_safepoint_defer_sigint();
        }
        else if (jl_safepoint_consume_sigint()) {
            jl_clear_force_sigint();
            jl_throw_in_thread(ptls2, thread, jl_interrupt_exception);
        }
        return KERN_SUCCESS;
    }
    if (ptls2->current_task->eh == NULL)
        return KERN_FAILURE;
    jl_value_t *excpt;
    if (is_addr_on_stack(jl_atomic_load_relaxed(&ptls2->current_task), (void*)fault_addr)) {
        excpt = jl_stackovf_exception;
    }
    else if (is_write_fault(exc_state)) // false for alignment errors
        excpt = jl_readonlymemory_exception;
    else
        return KERN_FAILURE;
    jl_throw_in_thread(ptls2, thread, excpt);
    return KERN_SUCCESS;
}

//mach_exc_server expects us to define this symbol locally
kern_return_t catch_mach_exception_raise_state(
    mach_port_t exception_port,
    exception_type_t exception,
    const mach_exception_data_t code,
    mach_msg_type_number_t codeCnt,
    int *flavor,
    const thread_state_t old_state,
    mach_msg_type_number_t old_stateCnt,
    thread_state_t new_state,
    mach_msg_type_number_t *new_stateCnt)
{
    return KERN_INVALID_ARGUMENT; // we only use EXCEPTION_DEFAULT
}

//mach_exc_server expects us to define this symbol locally
kern_return_t catch_mach_exception_raise_state_identity(
    mach_port_t exception_port,
    mach_port_t thread,
    mach_port_t task,
    exception_type_t exception,
    mach_exception_data_t code,
    mach_msg_type_number_t codeCnt,
    int *flavor,
    thread_state_t old_state,
    mach_msg_type_number_t old_stateCnt,
    thread_state_t new_state,
    mach_msg_type_number_t *new_stateCnt)
{
    return KERN_INVALID_ARGUMENT; // we only use EXCEPTION_DEFAULT
}

static void attach_exception_port(thread_port_t thread, int segv_only)
{
    kern_return_t ret;
    // http://www.opensource.apple.com/source/xnu/xnu-2782.1.97/osfmk/man/thread_set_exception_ports.html
    exception_mask_t mask = EXC_MASK_BAD_ACCESS;
    if (!segv_only)
        mask |= EXC_MASK_ARITHMETIC;
    ret = thread_set_exception_ports(thread, mask, segv_port, EXCEPTION_DEFAULT | MACH_EXCEPTION_CODES, MACH_THREAD_STATE);
    HANDLE_MACH_ERROR("thread_set_exception_ports", ret);
}

static int jl_thread_suspend_and_get_state2(int tid, host_thread_state_t *ctx)
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    if (ptls2 == NULL) // this thread is not alive
        return 0;
    jl_task_t *ct2 = ptls2 ? jl_atomic_load_relaxed(&ptls2->current_task) : NULL;
    if (ct2 == NULL) // this thread is already dead
        return 0;

    mach_port_t thread = pthread_mach_thread_np(ptls2->system_id);

    kern_return_t ret = thread_suspend(thread);
    HANDLE_MACH_ERROR("thread_suspend", ret);

    // Do the actual sampling
    unsigned int count = MACH_THREAD_STATE_COUNT;
    memset(ctx, 0, sizeof(*ctx));

    // Get the state of the suspended thread
    ret = thread_get_state(thread, MACH_THREAD_STATE, (thread_state_t)ctx, &count);
    return 1;
}

static void jl_thread_suspend_and_get_state(int tid, unw_context_t **ctx)
{
    static host_thread_state_t state;
    if (!jl_thread_suspend_and_get_state2(tid, &state)) {
        *ctx = NULL;
        return;
    }
    *ctx = (unw_context_t*)&state;
}

static void jl_thread_resume(int tid, int sig)
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[tid];
    mach_port_t thread = pthread_mach_thread_np(ptls2->system_id);
    kern_return_t ret = thread_resume(thread);
    HANDLE_MACH_ERROR("thread_resume", ret);
}

// Throw jl_interrupt_exception if the master thread is in a signal async region
// or if SIGINT happens too often.
static void jl_try_deliver_sigint(void)
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
    mach_port_t thread = pthread_mach_thread_np(ptls2->system_id);

    kern_return_t ret = thread_suspend(thread);
    HANDLE_MACH_ERROR("thread_suspend", ret);

    // This aborts `sleep` and other syscalls.
    ret = thread_abort(thread);
    HANDLE_MACH_ERROR("thread_abort", ret);

    jl_safepoint_enable_sigint();
    int force = jl_check_force_sigint();
    if (force || (!ptls2->defer_signal && ptls2->io_wait)) {
        jl_safepoint_consume_sigint();
        if (force)
            jl_safe_printf("WARNING: Force throwing a SIGINT\n");
        jl_clear_force_sigint();
        jl_throw_in_thread(ptls2, thread, jl_interrupt_exception);
    }
    else {
        jl_wake_libuv();
    }

    ret = thread_resume(thread);
    HANDLE_MACH_ERROR("thread_resume", ret);
}

static void JL_NORETURN jl_exit_thread0_cb(int exitstate)
{
CFI_NORETURN
    jl_critical_error(exitstate - 128, 0, NULL, jl_current_task);
    jl_exit(exitstate);
}

static void jl_exit_thread0(int exitstate, jl_bt_element_t *bt_data, size_t bt_size)
{
    jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[0];
    mach_port_t thread = pthread_mach_thread_np(ptls2->system_id);

    host_thread_state_t state;
    if (!jl_thread_suspend_and_get_state2(0, &state))
        return;
    unw_context_t *uc = (unw_context_t*)&state;

    // This aborts `sleep` and other syscalls.
    kern_return_t ret = thread_abort(thread);
    HANDLE_MACH_ERROR("thread_abort", ret);

    if (bt_data == NULL) {
        // Must avoid extended backtrace frames here unless we're sure bt_data
        // is properly rooted.
        ptls2->bt_size = rec_backtrace_ctx(ptls2->bt_data, JL_MAX_BT_SIZE, uc, NULL);
    }
    else {
        ptls2->bt_size = bt_size; // <= JL_MAX_BT_SIZE
        memcpy(ptls2->bt_data, bt_data, ptls2->bt_size * sizeof(bt_data[0]));
    }

    void (*exit_func)(int) = &_exit;
    if (thread0_exit_count <= 1) {
        exit_func = &jl_exit_thread0_cb;
    }
    else if (thread0_exit_count == 2) {
        exit_func = &exit;
    }
    else {
        exit_func = &_exit;
    }

#ifdef _CPU_X86_64_
    // First integer argument. Not portable but good enough =)
    state.__rdi = exitstate;
#elif defined(_CPU_AARCH64_)
    state.__x[0] = exitstate;
#else
#error Fill in first integer argument here
#endif
    jl_call_in_state(ptls2, &state, (void (*)(void))exit_func);
    unsigned int count = MACH_THREAD_STATE_COUNT;
    ret = thread_set_state(thread, MACH_THREAD_STATE, (thread_state_t)&state, count);
    HANDLE_MACH_ERROR("thread_set_state", ret);

    ret = thread_resume(thread);
    HANDLE_MACH_ERROR("thread_resume", ret);
}

static int profile_started = 0;
mach_timespec_t timerprof;
static pthread_t profiler_thread;
clock_serv_t clk;
static mach_port_t profile_port = 0;

#ifdef LLVMLIBUNWIND
volatile static int forceDwarf = -2;
static unw_context_t profiler_uc;

static kern_return_t profiler_segv_handler(
    mach_port_t exception_port,
    mach_port_t thread,
    mach_port_t task,
    exception_type_t exception,
    mach_exception_data_t code,
    mach_msg_type_number_t codeCnt)
{
    assert(thread == mach_profiler_thread);
    host_thread_state_t state;

    // Not currently unwinding. Raise regular segfault
    if (forceDwarf == -2)
        return KERN_FAILURE;

    if (forceDwarf == 0)
        forceDwarf = 1;
    else
        forceDwarf = -1;

    unsigned int count = MACH_THREAD_STATE_COUNT;

    thread_get_state(thread, MACH_THREAD_STATE, (thread_state_t)&state, &count);

#ifdef _CPU_X86_64_
    // don't change cs fs gs rflags
    uint64_t cs = state.__cs;
    uint64_t fs = state.__fs;
    uint64_t gs = state.__gs;
    uint64_t rflags = state.__rflags;
#elif defined(_CPU_AARCH64_)
    uint64_t cpsr = state.__cpsr;
#else
#error Unknown CPU
#endif

    memcpy(&state, &profiler_uc, sizeof(state));

#ifdef _CPU_X86_64_
    state.__cs = cs;
    state.__fs = fs;
    state.__gs = gs;
    state.__rflags = rflags;
#else
    state.__cpsr = cpsr;
#endif

    kern_return_t ret = thread_set_state(thread, MACH_THREAD_STATE, (thread_state_t)&state, count);
    HANDLE_MACH_ERROR("thread_set_state", ret);

    return KERN_SUCCESS;
}
#endif

// WARNING: we are unable to handle sigsegv while the dlsymlock is held
static int jl_lock_profile_mach(int dlsymlock)
{
    jl_lock_profile();
    // workaround for old keymgr bugs
    void *unused = NULL;
    int keymgr_locked = _keymgr_get_and_lock_processwide_ptr_2(KEYMGR_GCC3_DW2_OBJ_LIST, &unused) == 0;
    // workaround for new dlsym4 bugs (API and bugs introduced in macOS 12.1)
    if (dlsymlock && _dyld_atfork_prepare != NULL && _dyld_atfork_parent != NULL)
        _dyld_atfork_prepare();
    return keymgr_locked;
}

static void jl_unlock_profile_mach(int dlsymlock, int keymgr_locked)
{
    if (dlsymlock && _dyld_atfork_prepare != NULL && _dyld_atfork_parent != NULL) \
        _dyld_atfork_parent(); \
    if (keymgr_locked)
        _keymgr_unlock_processwide_ptr(KEYMGR_GCC3_DW2_OBJ_LIST);
    jl_unlock_profile();
}

#define jl_lock_profile()       int keymgr_locked = jl_lock_profile_mach(1)
#define jl_unlock_profile()     jl_unlock_profile_mach(1, keymgr_locked)

void *mach_profile_listener(void *arg)
{
    (void)arg;
    const int max_size = 512;
    attach_exception_port(mach_thread_self(), 1);
#ifdef LLVMLIBUNWIND
    mach_profiler_thread = mach_thread_self();
#endif
    mig_reply_error_t *bufRequest = (mig_reply_error_t*)malloc_s(max_size);
    while (1) {
        kern_return_t ret = mach_msg(&bufRequest->Head, MACH_RCV_MSG,
                                     0, max_size, profile_port,
                                     MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
        HANDLE_MACH_ERROR("mach_msg", ret);
        // sample each thread, round-robin style in reverse order
        // (so that thread zero gets notified last)
        int keymgr_locked = jl_lock_profile_mach(0);

        int nthreads = jl_atomic_load_acquire(&jl_n_threads);
        int *randperm = profile_get_randperm(nthreads);
        for (int idx = nthreads; idx-- > 0; ) {
            // Stop the threads in the random or reverse round-robin order.
            int i = randperm[idx];
            // if there is no space left, break early
            if (jl_profile_is_buffer_full()) {
                jl_profile_stop_timer();
                break;
            }

            if (_dyld_atfork_prepare != NULL && _dyld_atfork_parent != NULL)
                _dyld_atfork_prepare(); // briefly acquire the dlsym lock
            host_thread_state_t state;
            if (!jl_thread_suspend_and_get_state2(i, &state))
                continue;
            unw_context_t *uc = (unw_context_t*)&state;
            if (_dyld_atfork_prepare != NULL && _dyld_atfork_parent != NULL)
                _dyld_atfork_parent(); // quickly release the dlsym lock

            if (running) {
#ifdef LLVMLIBUNWIND
                /*
                 *  Unfortunately compact unwind info is incorrectly generated for quite a number of
                 *  libraries by quite a large number of compilers. We can fall back to DWARF unwind info
                 *  in some cases, but in quite a number of cases (especially libraries not compiled in debug
                 *  mode, only the compact unwind info may be available). Even more unfortunately, there is no
                 *  way to detect such bogus compact unwind info (other than noticing the resulting segfault).
                 *  What we do here is ugly, but necessary until the compact unwind info situation improves.
                 *  We try to use the compact unwind info and if that results in a segfault, we retry with DWARF info.
                 *  Note that in a small number of cases this may result in bogus stack traces, but at least the topmost
                 *  entry will always be correct, and the number of cases in which this is an issue is rather small.
                 *  Other than that, this implementation is not incorrect as the other thread is paused while we are profiling
                 *  and during stack unwinding we only ever read memory, but never write it.
                 */

                forceDwarf = 0;
                unw_getcontext(&profiler_uc); // will resume from this point if the next lines segfault at any point

                if (forceDwarf == 0) {
                    // Save the backtrace
                    bt_size_cur += rec_backtrace_ctx((jl_bt_element_t*)bt_data_prof + bt_size_cur, bt_size_max - bt_size_cur - 1, uc, NULL);
                }
                else if (forceDwarf == 1) {
                    bt_size_cur += rec_backtrace_ctx_dwarf((jl_bt_element_t*)bt_data_prof + bt_size_cur, bt_size_max - bt_size_cur - 1, uc, NULL);
                }
                else if (forceDwarf == -1) {
                    jl_safe_printf("WARNING: profiler attempt to access an invalid memory location\n");
                }

                forceDwarf = -2;
#else
                bt_size_cur += rec_backtrace_ctx((jl_bt_element_t*)bt_data_prof + bt_size_cur, bt_size_max - bt_size_cur - 1, uc, NULL);
#endif
                jl_ptls_t ptls = jl_atomic_load_relaxed(&jl_all_tls_states)[i];

                // store threadid but add 1 as 0 is preserved to indicate end of block
                bt_data_prof[bt_size_cur++].uintptr = ptls->tid + 1;

                // store task id (never null)
                bt_data_prof[bt_size_cur++].jlvalue = (jl_value_t*)jl_atomic_load_relaxed(&ptls->current_task);

                // store cpu cycle clock
                bt_data_prof[bt_size_cur++].uintptr = cycleclock();

                // store whether thread is sleeping but add 1 as 0 is preserved to indicate end of block
                bt_data_prof[bt_size_cur++].uintptr = jl_atomic_load_relaxed(&ptls->sleep_check_state) + 1;

                // Mark the end of this block with two 0's
                bt_data_prof[bt_size_cur++].uintptr = 0;
                bt_data_prof[bt_size_cur++].uintptr = 0;
            }
            // We're done! Resume the thread.
            jl_thread_resume(i, 0);
        }
        jl_unlock_profile_mach(0, keymgr_locked);
        if (running) {
            jl_check_profile_autostop();
            // Reset the alarm
            kern_return_t ret = clock_alarm(clk, TIME_RELATIVE, timerprof, profile_port);
            HANDLE_MACH_ERROR("clock_alarm", ret)
        }
    }
}

JL_DLLEXPORT int jl_profile_start_timer(void)
{
    kern_return_t ret;
    if (!profile_started) {
        mach_port_t self = mach_task_self();

        ret = host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, (clock_serv_t *)&clk);
        HANDLE_MACH_ERROR("host_get_clock_service", ret);

        ret = mach_port_allocate(self, MACH_PORT_RIGHT_RECEIVE, &profile_port);
        HANDLE_MACH_ERROR("mach_port_allocate", ret);

        // Alright, create a thread to serve as the listener for exceptions
        pthread_attr_t attr;
        if (pthread_attr_init(&attr) != 0) {
            jl_error("pthread_attr_init failed");
        }
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        if (pthread_create(&profiler_thread, &attr, mach_profile_listener, NULL) != 0) {
            jl_error("pthread_create failed");
        }
        pthread_attr_destroy(&attr);

        profile_started = 1;
    }

    timerprof.tv_sec = nsecprof/GIGA;
    timerprof.tv_nsec = nsecprof%GIGA;

    running = 1;
    // ensure the alarm is running
    ret = clock_alarm(clk, TIME_RELATIVE, timerprof, profile_port);
    HANDLE_MACH_ERROR("clock_alarm", ret);

    return 0;
}

JL_DLLEXPORT void jl_profile_stop_timer(void)
{
    running = 0;
}
