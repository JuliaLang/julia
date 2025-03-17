// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  init.c
  system initialization and global state
*/
#include "platform.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <libgen.h> // defines dirname

#if !defined(_OS_WINDOWS_) || defined(_COMPILER_GCC_)
#include <getopt.h>
#endif

#if defined(_OS_FREEBSD_)
#include <pthread_np.h>
#endif

#include "julia.h"
#include "julia_internal.h"
#define DEFINE_BUILTIN_GLOBALS
#include "builtin_proto.h"
#undef DEFINE_BUILTIN_GLOBALS
#include "threading.h"
#include "julia_assert.h"
#include "processor.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _OS_WINDOWS_
extern int needsSymRefreshModuleList;
extern BOOL (WINAPI *hSymRefreshModuleList)(HANDLE);
#else
#include <sys/resource.h>
#include <unistd.h>
#endif

// list of modules being deserialized with __init__ methods
jl_array_t *jl_module_init_order;
arraylist_t *jl_entrypoint_mis;

JL_DLLEXPORT size_t jl_page_size;

void jl_init_stack_limits(int ismaster, void **stack_lo, void **stack_hi)
{
#ifdef _OS_WINDOWS_
    (void)ismaster;
    // https://en.wikipedia.org/wiki/Win32_Thread_Information_Block
#  ifdef _P64
    *stack_hi = (void**)__readgsqword(0x08); // Stack Base / Bottom of stack (high address)
    *stack_lo = (void**)__readgsqword(0x10); // Stack Limit / Ceiling of stack (low address)
#  else // !_P64
    *stack_hi = (void**)__readfsdword(0x04); // Stack Base / Bottom of stack (high address)
    *stack_lo = (void**)__readfsdword(0x08); // Stack Limit / Ceiling of stack (low address)
#  endif // _P64
#else // !_OS_WINDOWS_
    // Only use pthread_*_np functions to get stack address for non-master
    // threads since it seems to return bogus values for master thread on Linux
    // and possibly OSX.
    if (!ismaster) {
#  if defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
        pthread_attr_t attr;
#if defined(_OS_FREEBSD_)
        pthread_attr_init(&attr);
        pthread_attr_get_np(pthread_self(), &attr);
#else
        pthread_getattr_np(pthread_self(), &attr);
#endif
        void *stackaddr;
        size_t stacksize;
        pthread_attr_getstack(&attr, &stackaddr, &stacksize);
        pthread_attr_destroy(&attr);
        *stack_lo = stackaddr;
        *stack_hi = (char*)stackaddr + stacksize;
        return;
#  elif defined(_OS_DARWIN_)
        extern void *pthread_get_stackaddr_np(pthread_t thread);
        extern size_t pthread_get_stacksize_np(pthread_t thread);
        pthread_t thread = pthread_self();
        void *stackaddr = pthread_get_stackaddr_np(thread);
        size_t stacksize = pthread_get_stacksize_np(thread);
        *stack_lo = (char*)stackaddr - stacksize;
        *stack_hi = stackaddr;
        return;
#  else
#      warning "Getting precise stack size for thread is not supported."
#  endif
    }
    struct rlimit rl;
    getrlimit(RLIMIT_STACK, &rl);
    size_t stacksize = rl.rlim_cur;
    *stack_hi = __builtin_frame_address(0);
    *stack_lo = (void*)((char*)*stack_hi - stacksize);
#endif
}

static void jl_prep_sanitizers(void)
{
#if !defined(_OS_WINDOWS_)
#if defined(_COMPILER_ASAN_ENABLED_) || defined(_COMPILER_MSAN_ENABLED_)
    struct rlimit rl;

    // When using the sanitizers, increase stack size because they bloat
    // stack usage
    const rlim_t kStackSize = 64 * 1024 * 1024;   // 64MiB stack
    int result;

    result = getrlimit(RLIMIT_STACK, &rl);
    if (result == 0) {
        if (rl.rlim_cur < kStackSize) {
            rl.rlim_cur = kStackSize;
            result = setrlimit(RLIMIT_STACK, &rl);
            if (result != 0) {
                fprintf(stderr, "setrlimit returned result = %d\n", result);
            }
        }
    }
#endif
#endif
}

struct uv_shutdown_queue_item { uv_handle_t *h; struct uv_shutdown_queue_item *next; };
struct uv_shutdown_queue { struct uv_shutdown_queue_item *first; struct uv_shutdown_queue_item *last; };

static void jl_uv_exitcleanup_add(uv_handle_t *handle, struct uv_shutdown_queue *queue)
{
    struct uv_shutdown_queue_item *item = (struct uv_shutdown_queue_item*)malloc_s(sizeof(struct uv_shutdown_queue_item));
    item->h = handle;
    item->next = NULL;
    if (queue->last)
        queue->last->next = item;
    if (!queue->first)
        queue->first = item;
    queue->last = item;
}

static void jl_uv_exitcleanup_walk(uv_handle_t *handle, void *arg)
{
    jl_uv_exitcleanup_add(handle, (struct uv_shutdown_queue*)arg);
}

static struct uv_shutdown_queue_item *next_shutdown_queue_item(struct uv_shutdown_queue_item *item)
{
    struct uv_shutdown_queue_item *rv = item->next;
    free(item);
    return rv;
}

static void jl_close_item_atexit(uv_handle_t *handle)
{
    if (handle->type != UV_FILE && uv_is_closing(handle))
        return;
    switch(handle->type) {
    case UV_PROCESS:
        // cause Julia to forget about the Process object
        handle->data = NULL;
        // and make libuv think it is already dead
        ((uv_process_t*)handle)->pid = 0;
        // fall-through
    case UV_TTY:
    case UV_UDP:
    case UV_TCP:
    case UV_NAMED_PIPE:
    case UV_POLL:
    case UV_TIMER:
    case UV_ASYNC:
    case UV_FS_EVENT:
    case UV_FS_POLL:
    case UV_IDLE:
    case UV_PREPARE:
    case UV_CHECK:
    case UV_SIGNAL:
    case UV_FILE:
        // These will be shutdown as appropriate by jl_close_uv
        jl_close_uv(handle);
        break;
    case UV_HANDLE:
    case UV_STREAM:
    default:
        assert(0 && "not a valid libuv handle");
    }
}

// This prevents `ct` from returning via error handlers or other unintentional
// means by destroying some old state before we start destroying that state in atexit hooks.
void jl_task_frame_noreturn(jl_task_t *ct) JL_NOTSAFEPOINT;

// cause this process to exit with WEXITSTATUS(signo), after waiting to finish all julia, C, and C++ cleanup
JL_DLLEXPORT void jl_exit(int exitcode)
{
    jl_atexit_hook(exitcode);
    exit(exitcode);
}

// cause this process to exit with WTERMSIG(signo),
// fairly aggressively (flushing stderr a bit, and doing a little bit of other
// external cleanup, but no internal cleanup)
JL_DLLEXPORT void jl_raise(int signo)
{
    uv_tty_reset_mode();
    fflush(NULL);
#ifdef _OS_WINDOWS_
    if (signo == SIGABRT) {
        signal(signo, SIG_DFL);
        abort();
    }
    // the exit status could also potentially be set to an NTSTATUS value
    // corresponding to a signal number, but this seems somewhat is uncommon on Windows
    TerminateProcess(GetCurrentProcess(), 3); // aka _exit
    abort(); // prior call does not return, because we passed GetCurrentProcess()
#else
    signal(signo, SIG_DFL);
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, signo);
    pthread_sigmask(SIG_UNBLOCK, &sset, NULL);
    raise(signo); // aka pthread_kill(pthread_self(), signo);
    if (signo == SIGABRT)
        abort();
    _exit(128 + signo);
#endif
}

JL_DLLEXPORT void jl_atexit_hook(int exitcode) JL_NOTSAFEPOINT_ENTER
{
    uv_tty_reset_mode();

    if (jl_atomic_load_relaxed(&jl_all_tls_states) == NULL)
        return;

    jl_task_t *ct = jl_get_current_task();

    if (ct == NULL && jl_base_module) {
        ct = container_of(jl_adopt_thread(), jl_task_t, gcstack);
    }
    else if (ct != NULL) {
        // we are about to start tearing everything down, so lets try not to get
        // upset by the local mess of things when we run the user's _atexit hooks
        // this also forces us into a GC-unsafe region without a safepoint
        jl_task_frame_noreturn(ct);
        jl_gc_safepoint_(ct->ptls);
    }

    if (jl_base_module) {
        size_t last_age = ct->world_age;
        ct->world_age = jl_get_world_counter();
        jl_value_t *f = jl_get_global(jl_base_module, jl_symbol("_atexit"));
        if (f != NULL) {
            jl_value_t **fargs;
            JL_GC_PUSHARGS(fargs, 2);
            fargs[0] = f;
            fargs[1] = jl_box_int32(exitcode);
            JL_TRY {
                assert(ct);
                jl_apply(fargs, 2);
            }
            JL_CATCH {
                jl_printf((JL_STREAM*)STDERR_FILENO, "\natexit hook threw an error: ");
                jl_static_show((JL_STREAM*)STDERR_FILENO, jl_current_exception(ct));
                jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
                jlbacktrace(); // written to STDERR_FILENO
            }
            JL_GC_POP();
        }
        ct->world_age = last_age;
    }

    if (ct && exitcode == 0) {
        size_t last_age = ct->world_age;
        ct->world_age = jl_get_world_counter();
        jl_write_compiler_output();
        ct->world_age = last_age;
    }

    jl_print_gc_stats(JL_STDERR);
    if (jl_options.code_coverage)
        jl_write_coverage_data(jl_options.output_code_coverage);
    if (jl_options.malloc_log)
        jl_write_malloc_log();

    // replace standard output streams with something that we can still print to
    // after the finalizers from base/stream.jl close the TTY
    JL_STDOUT = (uv_stream_t*) STDOUT_FILENO;
    JL_STDERR = (uv_stream_t*) STDERR_FILENO;

    if (ct)
        jl_gc_run_all_finalizers(ct);

    uv_loop_t *loop = jl_global_event_loop();
    if (loop != NULL) {
        struct uv_shutdown_queue queue = {NULL, NULL};
        JL_UV_LOCK();
        uv_walk(loop, jl_uv_exitcleanup_walk, &queue);
        struct uv_shutdown_queue_item *item = queue.first;
        if (ct) {
            while (item) {
                JL_TRY {
                    while (item) {
                        jl_close_item_atexit(item->h);
                        item = next_shutdown_queue_item(item);
                    }
                }
                JL_CATCH {
                    //error handling -- continue cleanup, as much as possible
                    assert(item);
                    uv_unref(item->h);
                    jl_printf((JL_STREAM*)STDERR_FILENO, "error during exit cleanup: close: ");
                    jl_static_show((JL_STREAM*)STDERR_FILENO, jl_current_exception(ct));
                    jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
                    jlbacktrace(); // written to STDERR_FILENO
                    item = next_shutdown_queue_item(item);
                }
            }
        }
        else {
            while (item) {
                jl_close_item_atexit(item->h);
                item = next_shutdown_queue_item(item);
            }
        }

        // force libuv to spin until everything has finished closing
        loop->stop_flag = 0;
        while (uv_run(loop, UV_RUN_DEFAULT)) { }
        jl_wake_libuv(); // set the async pending flag, so that future calls are immediate no-ops on other threads
                         // we would like to guarantee this, but cannot currently, so there is still a small race window
                         // that needs to be fixed in libuv
    }
    if (loop != NULL) {
        // TODO: consider uv_loop_close(loop) here, before shutdown?
        uv_library_shutdown();
        // no JL_UV_UNLOCK(), since it is now torn down
    }
    if (ct)
        jl_safepoint_suspend_all_threads(ct); // Destroy other threads, so that they don't segfault
    if (ct)
        (void)jl_gc_safe_enter(ct->ptls); // park in gc-safe

    jl_destroy_timing(); // cleans up the current timing_stack for noreturn
#ifdef USE_TIMING_COUNTS
    jl_print_timings();
#endif
    jl_teardown_codegen(); // prints stats
}

JL_DLLEXPORT void jl_postoutput_hook(void)
{
    if (jl_atomic_load_relaxed(&jl_all_tls_states) == NULL)
        return;

    if (jl_base_module) {
        jl_task_t *ct = jl_get_current_task();
        jl_value_t *f = jl_get_global(jl_base_module, jl_symbol("_postoutput"));
        if (f != NULL) {
            JL_TRY {
                size_t last_age = ct->world_age;
                ct->world_age = jl_get_world_counter();
                jl_apply(&f, 1);
                ct->world_age = last_age;
            }
            JL_CATCH {
                jl_printf((JL_STREAM*)STDERR_FILENO, "\npostoutput hook threw an error: ");
                jl_static_show((JL_STREAM*)STDERR_FILENO, jl_current_exception(ct));
                jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
                jlbacktrace(); // written to STDERR_FILENO
            }
        }
    }
    return;
}

void post_boot_hooks(void);
void post_image_load_hooks(void);

JL_DLLEXPORT void *jl_libjulia_internal_handle;
JL_DLLEXPORT void *jl_libjulia_handle;
JL_DLLEXPORT void *jl_RTLD_DEFAULT_handle;
JL_DLLEXPORT void *jl_exe_handle;
#ifdef _OS_WINDOWS_
void *jl_ntdll_handle;
void *jl_kernel32_handle;
void *jl_crtdll_handle;
void *jl_winsock_handle;
extern const char *jl_crtdll_name;
#endif

uv_loop_t *jl_io_loop;

#ifdef _OS_WINDOWS_
static int uv_dup(uv_os_fd_t fd, uv_os_fd_t* dupfd) {
    HANDLE current_process;

    if (fd == UV_STDIN_FD || fd == UV_STDOUT_FD || fd == UV_STDERR_FD)
        fd = GetStdHandle((DWORD)(uintptr_t) fd);

    /* _get_osfhandle will sometimes return -2 in case of an error. This seems */
    /* to happen when fd <= 2 and the process' corresponding stdio handle is */
    /* set to NULL. Unfortunately DuplicateHandle will happily duplicate */
    /* (HANDLE) -2, so this situation goes unnoticed until someone tries to */
    /* use the duplicate. Therefore we filter out known-invalid handles here. */
    if (fd == INVALID_HANDLE_VALUE ||
        fd == NULL ||
        fd == (HANDLE) -2) {
        *dupfd = INVALID_HANDLE_VALUE;
        return 0; // allow the execution to continue even if stdio is not available as in batchmode or without a console
    }

    current_process = GetCurrentProcess();

    if (!DuplicateHandle(current_process,
                         fd,
                         current_process,
                         dupfd,
                         0,
                         TRUE,
                         DUPLICATE_SAME_ACCESS)) {
        *dupfd = INVALID_HANDLE_VALUE;
        return GetLastError();
    }

    return 0;
}
#else
static int uv_dup(uv_os_fd_t fd, uv_os_fd_t* dupfd) {
    if ((*dupfd = fcntl(fd, F_DUPFD_CLOEXEC, 3)) == -1)
        return -errno;
    return 0;
}
#endif

static void *init_stdio_handle(const char *stdio, uv_os_fd_t fd, int readable)
{
    void *handle;
    int err;
    // Duplicate the file descriptor so we can later dup it over if we want to redirect
    // STDIO without having to worry about closing the associated libuv object.
    // This also helps limit the impact other libraries can cause on our file handle.
    if ((err = uv_dup(fd, &fd)))
        jl_errorf("error initializing %s in uv_dup: %s (%s %d)", stdio, uv_strerror(err), uv_err_name(err), err);
    assert(fd != -1); // This avoids a bug in clang's static analyzer, if an error did not occur, fd != -1
    switch(uv_guess_handle(fd)) {
    case UV_TTY:
        handle = malloc_s(sizeof(uv_tty_t));
        if ((err = uv_tty_init(jl_io_loop, (uv_tty_t*)handle, fd, 0))) {
            jl_errorf("error initializing %s in uv_tty_init: %s (%s %d)", stdio, uv_strerror(err), uv_err_name(err), err);
        }
        ((uv_tty_t*)handle)->data = NULL;
        uv_tty_set_mode((uv_tty_t*)handle, UV_TTY_MODE_NORMAL); // initialized cooked stdio
        break;
    default:
        assert(0 && "missing case for uv_guess_handle return handling");
        JL_FALLTHROUGH;
    case UV_UDP:
        JL_FALLTHROUGH;
    case UV_UNKNOWN_HANDLE:
        // dup the descriptor with a new one pointing at the bit bucket ...
#if defined(_OS_WINDOWS_)
        CloseHandle(fd);
        fd = CreateFile("NUL", readable ? FILE_GENERIC_READ : FILE_GENERIC_WRITE | FILE_READ_ATTRIBUTES,
                FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, NULL);
#else
        {
            int nullfd;
            nullfd = open("/dev/null", O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH /* 0666 */);
            assert(nullfd != -1);
            dup2(nullfd, fd);
            close(nullfd);
        }
#endif
        // ...and continue on as in the UV_FILE case
        JL_FALLTHROUGH;
    case UV_FILE:
        handle = malloc_s(sizeof(jl_uv_file_t));
        {
            jl_uv_file_t *file = (jl_uv_file_t*)handle;
            file->loop = jl_io_loop;
            file->type = UV_FILE;
            file->file = fd;
            file->data = NULL;
        }
        break;
    case UV_NAMED_PIPE:
        handle = malloc_s(sizeof(uv_pipe_t));
        if ((err = uv_pipe_init(jl_io_loop, (uv_pipe_t*)handle, 0))) {
            jl_errorf("error initializing %s in uv_pipe_init: %s (%s %d)", stdio, uv_strerror(err), uv_err_name(err), err);
        }
        if ((err = uv_pipe_open((uv_pipe_t*)handle, fd))) {
            jl_errorf("error initializing %s in uv_pipe_open: %s (%s %d)", stdio, uv_strerror(err), uv_err_name(err), err);
        }
        ((uv_pipe_t*)handle)->data = NULL;
        break;
    case UV_TCP:
        handle = malloc_s(sizeof(uv_tcp_t));
        if ((err = uv_tcp_init(jl_io_loop, (uv_tcp_t*)handle))) {
            jl_errorf("error initializing %s in uv_tcp_init: %s (%s %d)", stdio, uv_strerror(err), uv_err_name(err), err);
        }
        if ((err = uv_tcp_open((uv_tcp_t*)handle, (uv_os_sock_t)fd))) {
            jl_errorf("error initializing %s in uv_tcp_open: %s (%s %d)", stdio, uv_strerror(err), uv_err_name(err), err);
        }
        ((uv_tcp_t*)handle)->data = NULL;
        break;
    }
    return handle;
}

static void init_stdio(void)
{
    JL_STDIN  = (uv_stream_t*)init_stdio_handle("stdin", UV_STDIN_FD, 1);
    JL_STDOUT = (uv_stream_t*)init_stdio_handle("stdout", UV_STDOUT_FD, 0);
    JL_STDERR = (uv_stream_t*)init_stdio_handle("stderr", UV_STDERR_FD, 0);
    jl_flush_cstdio();
}

int jl_isabspath(const char *in) JL_NOTSAFEPOINT
{
#ifdef _OS_WINDOWS_
    char c0 = in[0];
    if (c0 == '/' || c0 == '\\') {
        return 1; // absolute path relative to %CD% (current drive), or UNC
    }
    else if (c0 && in[1] == ':') {
        char c2 = in[2];
        return c2 == '/' || c2 == '\\'; // absolute path with drive name
    }
#else
    if (in[0] == '/') return 1; // absolute path
#endif
    return 0; // relative path
}

static char *absrealpath(const char *in, int nprefix)
{ // compute an absolute realpath location, so that chdir doesn't change the file reference
  // ignores (copies directly over) nprefix characters at the start of abspath
#ifndef _OS_WINDOWS_
    char *out = realpath(in + nprefix, NULL);
    if (out) {
        if (nprefix > 0) {
            size_t sz = strlen(out) + 1;
            char *cpy = (char*)malloc_s(sz + nprefix);
            memcpy(cpy, in, nprefix);
            memcpy(cpy + nprefix, out, sz);
            free(out);
            out = cpy;
        }
    }
    else {
        size_t sz = strlen(in + nprefix) + 1;
        if (in[nprefix] == PATHSEPSTRING[0]) {
            out = (char*)malloc_s(sz + nprefix);
            memcpy(out, in, sz + nprefix);
        }
        else {
            size_t path_size = JL_PATH_MAX;
            char *path = (char*)malloc_s(JL_PATH_MAX);
            if (uv_cwd(path, &path_size)) {
                jl_error("fatal error: unexpected error while retrieving current working directory");
            }
            out = (char*)malloc_s(path_size + 1 + sz + nprefix);
            memcpy(out, in, nprefix);
            memcpy(out + nprefix, path, path_size);
            out[nprefix + path_size] = PATHSEPSTRING[0];
            memcpy(out + nprefix + path_size + 1, in + nprefix, sz);
            free(path);
        }
    }
#else
    // GetFullPathName intentionally errors if given an empty string so manually insert `.` to invoke cwd
    char *in2 = (char*)malloc_s(JL_PATH_MAX);
    if (strlen(in) - nprefix == 0) {
        memcpy(in2, in, nprefix);
        in2[nprefix] = '.';
        in2[nprefix+1] = '\0';
        in = in2;
    }
    DWORD n = GetFullPathName(in + nprefix, 0, NULL, NULL);
    if (n <= 0) {
        jl_error("fatal error: jl_options.image_file path too long or GetFullPathName failed");
    }
    char *out = (char*)malloc_s(n + nprefix);
    DWORD m = GetFullPathName(in + nprefix, n, out + nprefix, NULL);
    if (n != m + 1) {
        jl_error("fatal error: jl_options.image_file path too long or GetFullPathName failed");
    }
    memcpy(out, in, nprefix);
    free(in2);
#endif
    return out;
}

// create an absolute-path copy of the input path format string
// formed as `joinpath(replace(pwd(), "%" => "%%"), in)`
// unless `in` starts with `%`
static const char *absformat(const char *in)
{
    if (in[0] == '%' || jl_isabspath(in))
        return in;
    // get an escaped copy of cwd
    size_t path_size = JL_PATH_MAX;
    char path[JL_PATH_MAX];
    if (uv_cwd(path, &path_size)) {
        jl_error("fatal error: unexpected error while retrieving current working directory");
    }
    size_t sz = strlen(in) + 1;
    size_t i, fmt_size = 0;
    for (i = 0; i < path_size; i++)
        fmt_size += (path[i] == '%' ? 2 : 1);
    char *out = (char*)malloc_s(fmt_size + 1 + sz);
    fmt_size = 0;
    for (i = 0; i < path_size; i++) { // copy-replace pwd portion
        char c = path[i];
        out[fmt_size++] = c;
        if (c == '%')
            out[fmt_size++] = '%';
    }
    out[fmt_size++] = PATHSEPSTRING[0]; // path sep
    memcpy(out + fmt_size, in, sz); // copy over format, including nul
    return out;
}

static void jl_resolve_sysimg_location(JL_IMAGE_SEARCH rel)
{
    // this function resolves the paths in jl_options to absolute file locations as needed
    // and it replaces the pointers to `julia_bindir`, `julia_bin`, `image_file`, and output file paths
    // it may fail, print an error, and exit(1) if any of these paths are longer than JL_PATH_MAX
    //
    // note: if you care about lost memory, you should call the appropriate `free()` function
    // on the original pointer for each `char*` you've inserted into `jl_options`, after
    // calling `julia_init()`
    char *free_path = (char*)malloc_s(JL_PATH_MAX);
    size_t path_size = JL_PATH_MAX;
    if (uv_exepath(free_path, &path_size)) {
        jl_error("fatal error: unexpected error while retrieving exepath");
    }
    if (path_size >= JL_PATH_MAX) {
        jl_error("fatal error: jl_options.julia_bin path too long");
    }
    jl_options.julia_bin = (char*)malloc_s(path_size + 1);
    memcpy((char*)jl_options.julia_bin, free_path, path_size);
    ((char*)jl_options.julia_bin)[path_size] = '\0';
    if (!jl_options.julia_bindir) {
        jl_options.julia_bindir = getenv("JULIA_BINDIR");
        if (!jl_options.julia_bindir) {
            jl_options.julia_bindir = dirname(free_path);
        }
    }
    if (jl_options.julia_bindir)
        jl_options.julia_bindir = absrealpath(jl_options.julia_bindir, 0);
    free(free_path);
    free_path = NULL;
    if (jl_options.image_file) {
        if (rel == JL_IMAGE_JULIA_HOME && !jl_isabspath(jl_options.image_file)) {
            // build time path, relative to JULIA_BINDIR
            free_path = (char*)malloc_s(JL_PATH_MAX);
            int n = snprintf(free_path, JL_PATH_MAX, "%s" PATHSEPSTRING "%s",
                             jl_options.julia_bindir, jl_options.image_file);
            if (n >= JL_PATH_MAX || n < 0) {
                jl_error("fatal error: jl_options.image_file path too long");
            }
            jl_options.image_file = free_path;
        }
        if (jl_options.image_file)
            jl_options.image_file = absrealpath(jl_options.image_file, 0);
        if (free_path) {
            free(free_path);
            free_path = NULL;
        }
    }
    if (jl_options.outputo)
        jl_options.outputo = absrealpath(jl_options.outputo, 0);
    if (jl_options.outputji)
        jl_options.outputji = absrealpath(jl_options.outputji, 0);
    if (jl_options.outputbc)
        jl_options.outputbc = absrealpath(jl_options.outputbc, 0);
    if (jl_options.outputasm)
        jl_options.outputasm = absrealpath(jl_options.outputasm, 0);
    if (jl_options.machine_file)
        jl_options.machine_file = absrealpath(jl_options.machine_file, 0);
    if (jl_options.output_code_coverage)
        jl_options.output_code_coverage = absformat(jl_options.output_code_coverage);
    if (jl_options.tracked_path)
        jl_options.tracked_path = absrealpath(jl_options.tracked_path, 0);

    const char **cmdp = jl_options.cmds;
    if (cmdp) {
        for (; *cmdp; cmdp++) {
            const char *cmd = *cmdp;
            if (cmd[0] == 'L') {
                *cmdp = absrealpath(cmd, 1);
            }
        }
    }
}

JL_DLLEXPORT int jl_is_file_tracked(jl_sym_t *path)
{
    const char* path_ = jl_symbol_name(path);
    int tpath_len = strlen(jl_options.tracked_path);
    return (strlen(path_) >= tpath_len) && (strncmp(path_, jl_options.tracked_path, tpath_len) == 0);
}

static void jl_set_io_wait(int v)
{
    jl_task_t *ct = jl_current_task;
    ct->ptls->io_wait = v;
}

extern jl_mutex_t jl_modules_mutex;
extern jl_mutex_t precomp_statement_out_lock;
extern jl_mutex_t newly_inferred_mutex;
extern jl_mutex_t global_roots_lock;
extern jl_mutex_t profile_show_peek_cond_lock;

static void restore_fp_env(void)
{
    if (jl_set_zero_subnormals(0) || jl_set_default_nans(0)) {
        jl_error("Failed to configure floating point environment");
    }
}

static NOINLINE void _finish_julia_init(JL_IMAGE_SEARCH rel, jl_ptls_t ptls, jl_task_t *ct);

JL_DLLEXPORT int jl_default_debug_info_kind;
JL_DLLEXPORT jl_cgparams_t jl_default_cgparams = {
        /* track_allocations */ 1,
        /* code_coverage */ 1,
        /* prefer_specsig */ 0,
#ifdef _OS_WINDOWS_
        /* gnu_pubnames */ 0,
#else
        /* gnu_pubnames */ 1,
#endif
        /* debug_info_kind */ 0, // later DICompileUnit::DebugEmissionKind::FullDebug,
        /* debug_info_level */ 0, // later jl_options.debug_level,
        /* safepoint_on_entry */ 1,
        /* gcstack_arg */ 1,
        /* use_jlplt*/ 1 };

static void init_global_mutexes(void) {
    JL_MUTEX_INIT(&jl_modules_mutex, "jl_modules_mutex");
    JL_MUTEX_INIT(&precomp_statement_out_lock, "precomp_statement_out_lock");
    JL_MUTEX_INIT(&newly_inferred_mutex, "newly_inferred_mutex");
    JL_MUTEX_INIT(&global_roots_lock, "global_roots_lock");
    JL_MUTEX_INIT(&typecache_lock, "typecache_lock");
    JL_MUTEX_INIT(&profile_show_peek_cond_lock, "profile_show_peek_cond_lock");
}

JL_DLLEXPORT void julia_init(JL_IMAGE_SEARCH rel)
{
    // initialize many things, in no particular order
    // but generally running from simple platform things to optional
    // configuration features
    jl_init_timing();
    // Make sure we finalize the tls callback before starting any threads.
    (void)jl_get_pgcstack();

    // initialize symbol-table lock
    uv_mutex_init(&symtab_lock);
    // initialize the live tasks lock
    uv_mutex_init(&live_tasks_lock);
    // initialize the profiler buffer lock
    uv_mutex_init(&bt_data_prof_lock);

    // initialize backtraces
    jl_init_profile_lock();
#ifdef _OS_WINDOWS_
    uv_mutex_init(&jl_in_stackwalk);
    SymSetOptions(SYMOPT_UNDNAME | SYMOPT_DEFERRED_LOADS | SYMOPT_LOAD_LINES | SYMOPT_IGNORE_CVREC);
    if (!SymInitialize(GetCurrentProcess(), "", 1)) {
        jl_safe_printf("WARNING: failed to initialize stack walk info\n");
    }
    needsSymRefreshModuleList = 0;
#else
    // nongnu libunwind initialization is only threadsafe on architecture where the
    // author could access TSAN, per https://github.com/libunwind/libunwind/pull/109
    // so we need to do this once early (before threads)
    rec_backtrace(NULL, 0, 0);
#endif

    libsupport_init();
    jl_safepoint_init();
    jl_page_size = jl_getpagesize();
    htable_new(&jl_current_modules, 0);
    init_global_mutexes();
    jl_precompile_toplevel_module = NULL;
    ios_set_io_wait_func = jl_set_io_wait;
    jl_io_loop = uv_default_loop(); // this loop will internal events (spawning process etc.),
                                    // best to call this first, since it also initializes libuv
    jl_init_uv();
    init_stdio();
    restore_fp_env();
    if (jl_options.handle_signals == JL_OPTIONS_HANDLE_SIGNALS_ON)
        restore_signals();

    jl_init_intrinsic_properties();

    // Important offset for external codegen.
    jl_task_gcstack_offset = offsetof(jl_task_t, gcstack);
    jl_task_ptls_offset = offsetof(jl_task_t, ptls);

    jl_prep_sanitizers();
    void *stack_lo, *stack_hi;
    jl_init_stack_limits(1, &stack_lo, &stack_hi);

    jl_libjulia_internal_handle = jl_find_dynamic_library_by_addr(&jl_load_dynamic_library, /* throw_err */ 1);
    jl_libjulia_handle = jl_find_dynamic_library_by_addr(&jl_any_type, /* throw_err */ 1);
#ifdef _OS_WINDOWS_
    jl_exe_handle = GetModuleHandleA(NULL);
    jl_RTLD_DEFAULT_handle = jl_libjulia_internal_handle;
    jl_ntdll_handle = jl_dlopen("ntdll.dll", JL_RTLD_NOLOAD); // bypass julia's pathchecking for system dlls
    jl_kernel32_handle = jl_dlopen("kernel32.dll", JL_RTLD_NOLOAD);
    jl_crtdll_handle = jl_dlopen(jl_crtdll_name, JL_RTLD_NOLOAD);
    jl_winsock_handle = jl_dlopen("ws2_32.dll", JL_RTLD_NOLOAD);
    HMODULE jl_dbghelp = (HMODULE) jl_dlopen("dbghelp.dll", JL_RTLD_NOLOAD);
    needsSymRefreshModuleList = 0;
    if (jl_dbghelp)
        jl_dlsym(jl_dbghelp, "SymRefreshModuleList", (void **)&hSymRefreshModuleList, 1);
#else
    jl_exe_handle = jl_dlopen(NULL, JL_RTLD_NOW);
#ifdef RTLD_DEFAULT
    jl_RTLD_DEFAULT_handle = RTLD_DEFAULT;
#else
    jl_RTLD_DEFAULT_handle = jl_exe_handle;
#endif
#endif

    jl_init_rand();
    jl_init_runtime_ccall();
    jl_init_tasks();
    jl_init_threading();
    jl_init_threadinginfra();
    if (jl_options.handle_signals == JL_OPTIONS_HANDLE_SIGNALS_ON)
        jl_install_default_signal_handlers();

    jl_gc_init();

    arraylist_new(&jl_linkage_blobs, 0);
    arraylist_new(&jl_image_relocs, 0);
    arraylist_new(&jl_top_mods, 0);
    arraylist_new(&eytzinger_image_tree, 0);
    arraylist_new(&eytzinger_idxs, 0);
    arraylist_push(&eytzinger_idxs, (void*)0);
    arraylist_push(&eytzinger_image_tree, (void*)1); // outside image

    jl_ptls_t ptls = jl_init_threadtls(0);
#pragma GCC diagnostic push
#if defined(_COMPILER_GCC_) && __GNUC__ >= 12
#pragma GCC diagnostic ignored "-Wdangling-pointer"
#endif
    if (jl_options.task_metrics == JL_OPTIONS_TASK_METRICS_ON) {
        // enable before creating the root task so it gets timings too.
        jl_atomic_fetch_add(&jl_task_metrics_enabled, 1);
    }
    // warning: this changes `jl_current_task`, so be careful not to call that from this function
    jl_task_t *ct = jl_init_root_task(ptls, stack_lo, stack_hi);
#pragma GCC diagnostic pop
    JL_GC_PROMISE_ROOTED(ct);
    _finish_julia_init(rel, ptls, ct);
}

static NOINLINE void _finish_julia_init(JL_IMAGE_SEARCH rel, jl_ptls_t ptls, jl_task_t *ct)
{
    JL_TIMING(JULIA_INIT, JULIA_INIT);
    jl_resolve_sysimg_location(rel);

    // loads sysimg if available, and conditionally sets jl_options.cpu_target
    jl_image_buf_t sysimage = { JL_IMAGE_KIND_NONE };
    if (rel == JL_IMAGE_IN_MEMORY) {
        sysimage = jl_set_sysimg_so(jl_exe_handle);
        jl_options.image_file = jl_options.julia_bin;
    }
    else if (jl_options.image_file)
        sysimage = jl_preload_sysimg(jl_options.image_file);

    if (sysimage.kind == JL_IMAGE_KIND_SO)
        jl_gc_notify_image_load(sysimage.data, sysimage.size);

    if (jl_options.cpu_target == NULL)
        jl_options.cpu_target = "native";

    // Parse image, perform relocations, and init JIT targets, etc.
    jl_image_t parsed_image = jl_init_processor_sysimg(sysimage, jl_options.cpu_target);

    jl_init_codegen();
    jl_init_common_symbols();

    if (sysimage.kind != JL_IMAGE_KIND_NONE) {
        // Load the .ji or .so sysimage
        jl_restore_system_image(&parsed_image, sysimage);
    } else {
        // No sysimage provided, init a minimal environment
        jl_init_types();
        jl_global_roots_list = (jl_genericmemory_t*)jl_an_empty_memory_any;
        jl_global_roots_keyset = (jl_genericmemory_t*)jl_an_empty_memory_any;
    }

    jl_init_flisp();
    jl_init_serializer();

    if (sysimage.kind == JL_IMAGE_KIND_NONE) {
        jl_top_module = jl_core_module;
        jl_init_intrinsic_functions();
        jl_init_primitives();
        jl_init_main_module();
        jl_load(jl_core_module, "boot.jl");
        jl_current_task->world_age = jl_atomic_load_acquire(&jl_world_counter);
        post_boot_hooks();
    }

    if (jl_base_module == NULL) {
        // nthreads > 1 requires code in Base
        jl_atomic_store_relaxed(&jl_n_threads, 1);
        jl_n_markthreads = 0;
        jl_n_sweepthreads = 0;
        jl_n_gcthreads = 0;
        jl_n_threads_per_pool[JL_THREADPOOL_ID_INTERACTIVE] = 0;
        jl_n_threads_per_pool[JL_THREADPOOL_ID_DEFAULT] = 1;
    } else {
        jl_current_task->world_age = jl_atomic_load_acquire(&jl_world_counter);
        post_image_load_hooks();
    }
    jl_start_threads();
    jl_start_gc_threads();
    uv_barrier_wait(&thread_init_done);

    jl_gc_enable(1);

    if ((sysimage.kind != JL_IMAGE_KIND_NONE) &&
            (!jl_generating_output() || jl_options.incremental) && jl_module_init_order) {
        jl_array_t *init_order = jl_module_init_order;
        JL_GC_PUSH1(&init_order);
        jl_module_init_order = NULL;
        int i, l = jl_array_nrows(init_order);
        for (i = 0; i < l; i++) {
            jl_value_t *mod = jl_array_ptr_ref(init_order, i);
            jl_module_run_initializer((jl_module_t*)mod);
        }
        JL_GC_POP();
    }

    if (jl_options.trim) {
        jl_entrypoint_mis = (arraylist_t *)malloc_s(sizeof(arraylist_t));
        arraylist_new(jl_entrypoint_mis, 0);
    }

    if (jl_options.handle_signals == JL_OPTIONS_HANDLE_SIGNALS_ON)
        jl_install_sigint_handler();
}

#ifdef __cplusplus
}
#endif
