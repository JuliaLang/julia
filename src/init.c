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

#if !defined(_OS_WINDOWS_) || defined(_COMPILER_MINGW_)
#include <getopt.h>
#endif

#include "julia.h"
#include "julia_internal.h"
#define DEFINE_BUILTIN_GLOBALS
#include "builtin_proto.h"
#undef DEFINE_BUILTIN_GLOBALS
#include "threading.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _MSC_VER
JL_DLLEXPORT char *dirname(char *);
#else
#include <libgen.h>
#endif

#ifdef _OS_WINDOWS_
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <io.h>
extern int needsSymRefreshModuleList;
extern BOOL (WINAPI *hSymRefreshModuleList)(HANDLE);
#else
#include <sys/resource.h>
#include <unistd.h>
#endif

#ifdef JL_ASAN_ENABLED
JL_DLLEXPORT const char* __asan_default_options() {
    return "allow_user_segv_handler=1:detect_leaks=0";
    // FIXME: enable LSAN after fixing leaks & defining __lsan_default_suppressions(),
    //        or defining __lsan_default_options = exitcode=0 once publicly available
    //        (here and in flisp/flmain.c)
}
#endif

size_t jl_page_size;

void jl_init_stack_limits(int ismaster)
{
    jl_ptls_t ptls = jl_get_ptls_states();
#ifdef _OS_WINDOWS_
    (void)ismaster;
#  ifdef _COMPILER_MICROSOFT_
#    ifdef _P64
    void **tib = (void**)__readgsqword(0x30);
#    else
    void **tib = (void**)__readfsdword(0x18);
#    endif
#  else
    void **tib;
#    ifdef _P64
    __asm__("movq %%gs:0x30, %0" : "=r" (tib) : : );
#    else
    __asm__("movl %%fs:0x18, %0" : "=r" (tib) : : );
#    endif
#  endif
    // https://en.wikipedia.org/wiki/Win32_Thread_Information_Block
    ptls->stack_hi = (char*)tib[1]; // Stack Base / Bottom of stack (high address)
    ptls->stack_lo = (char*)tib[2]; // Stack Limit / Ceiling of stack (low address)
#else
#  ifdef JULIA_ENABLE_THREADING
    // Only use pthread_*_np functions to get stack address for non-master
    // threads since it seems to return bogus values for master thread on Linux
    // and possibly OSX.
    if (!ismaster) {
#    if defined(_OS_LINUX_)
        pthread_attr_t attr;
        pthread_getattr_np(pthread_self(), &attr);
        void *stackaddr;
        size_t stacksize;
        pthread_attr_getstack(&attr, &stackaddr, &stacksize);
        pthread_attr_destroy(&attr);
        ptls->stack_lo = (char*)stackaddr;
        ptls->stack_hi = (char*)stackaddr + stacksize;
        return;
#    elif defined(_OS_DARWIN_)
        extern void *pthread_get_stackaddr_np(pthread_t thread);
        extern size_t pthread_get_stacksize_np(pthread_t thread);
        pthread_t thread = pthread_self();
        void *stackaddr = pthread_get_stackaddr_np(thread);
        size_t stacksize = pthread_get_stacksize_np(thread);
        ptls->stack_lo = (char*)stackaddr;
        ptls->stack_hi = (char*)stackaddr + stacksize;
        return;
#    elif defined(_OS_FREEBSD_)
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        pthread_attr_get_np(pthread_self(), &attr);
        void *stackaddr;
        size_t stacksize;
        pthread_attr_getstack(&attr, &stackaddr, &stacksize);
        pthread_attr_destroy(&attr);
        ptls->stack_lo = (char*)stackaddr;
        ptls->stack_hi = (char*)stackaddr + stacksize;
        return;
#    else
#      warning "Getting stack size for thread is not supported."
#    endif
    }
#  else
    (void)ismaster;
#  endif
    struct rlimit rl;
    getrlimit(RLIMIT_STACK, &rl);
    size_t stack_size = rl.rlim_cur;
    ptls->stack_hi = (char*)&stack_size;
    ptls->stack_lo = ptls->stack_hi - stack_size;
#endif
}

static void jl_find_stack_bottom(void)
{
#if !defined(_OS_WINDOWS_)
#if defined(JL_ASAN_ENABLED) || defined(JL_MSAN_ENABLED)
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
    jl_init_stack_limits(1);
}

struct uv_shutdown_queue_item { uv_handle_t *h; struct uv_shutdown_queue_item *next; };
struct uv_shutdown_queue { struct uv_shutdown_queue_item *first; struct uv_shutdown_queue_item *last; };

static void jl_uv_exitcleanup_add(uv_handle_t *handle, struct uv_shutdown_queue *queue)
{
    struct uv_shutdown_queue_item *item = (struct uv_shutdown_queue_item*)malloc(sizeof(struct uv_shutdown_queue_item));
    item->h = handle;
    item->next = NULL;
    if (queue->last) queue->last->next = item;
    if (!queue->first) queue->first = item;
    queue->last = item;
}

static void jl_uv_exitcleanup_walk(uv_handle_t *handle, void *arg)
{
    jl_uv_exitcleanup_add(handle, (struct uv_shutdown_queue*)arg);
}

void jl_write_coverage_data(void);
void jl_write_malloc_log(void);
void jl_write_compiler_output(void);

static struct uv_shutdown_queue_item *next_shutdown_queue_item(struct uv_shutdown_queue_item *item)
{
    struct uv_shutdown_queue_item *rv = item->next;
    free(item);
    return rv;
}

void jl_init_timing(void);
void jl_destroy_timing(void);
void jl_uv_call_close_callback(jl_value_t *val);

static void jl_close_item_atexit(uv_handle_t *handle)
{
    if (handle->type != UV_FILE && uv_is_closing(handle))
        return;
    switch(handle->type) {
    case UV_PROCESS:
        // cause Julia to forget about the Process object
        if (handle->data)
            jl_uv_call_close_callback((jl_value_t*)handle->data);
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

JL_DLLEXPORT void jl_atexit_hook(int exitcode)
{
    jl_ptls_t ptls = jl_get_ptls_states();

    if (exitcode == 0)
        jl_write_compiler_output();
    jl_print_gc_stats(JL_STDERR);
    if (jl_options.code_coverage)
        jl_write_coverage_data();
    if (jl_options.malloc_log)
        jl_write_malloc_log();
    if (jl_base_module) {
        jl_value_t *f = jl_get_global(jl_base_module, jl_symbol("_atexit"));
        if (f != NULL) {
            JL_TRY {
                size_t last_age = ptls->world_age;
                ptls->world_age = jl_get_world_counter();
                jl_apply(&f, 1);
                ptls->world_age = last_age;
            }
            JL_CATCH {
                jl_printf(JL_STDERR, "\natexit hook threw an error: ");
                jl_static_show(JL_STDERR, ptls->exception_in_transit);
            }
        }
    }

    // replace standard output streams with something that we can still print to
    // after the finalizers from base/stream.jl close the TTY
    JL_STDOUT = (uv_stream_t*) STDOUT_FILENO;
    JL_STDERR = (uv_stream_t*) STDERR_FILENO;

    jl_gc_run_all_finalizers(ptls);

    uv_loop_t *loop = jl_global_event_loop();

    if (loop == NULL) {
        return;
    }

    struct uv_shutdown_queue queue = {NULL, NULL};
    uv_walk(loop, jl_uv_exitcleanup_walk, &queue);
    struct uv_shutdown_queue_item *item = queue.first;
    if (ptls->current_task != NULL) {
        while (item) {
            JL_TRY {
                while (item) {
                    jl_close_item_atexit(item->h);
                    item = next_shutdown_queue_item(item);
                }
            }
            JL_CATCH {
                //error handling -- continue cleanup, as much as possible
                uv_unref(item->h);
                jl_printf(JL_STDERR, "error during exit cleanup: close: ");
                jl_static_show(JL_STDERR, ptls->exception_in_transit);
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

    jl_destroy_timing();
#ifdef ENABLE_TIMINGS
    jl_print_timings();
#endif
}

void jl_get_builtin_hooks(void);
void jl_get_builtins(void);

JL_DLLEXPORT void *jl_dl_handle;
void *jl_RTLD_DEFAULT_handle;
JL_DLLEXPORT void *jl_exe_handle;
#ifdef _OS_WINDOWS_
void *jl_ntdll_handle;
void *jl_kernel32_handle;
void *jl_crtdll_handle;
void *jl_winsock_handle;
#endif

uv_loop_t *jl_io_loop;

#ifndef _OS_WINDOWS_
#define UV_STREAM_READABLE 0x20   /* The stream is readable */
#define UV_STREAM_WRITABLE 0x40   /* The stream is writable */
#endif

#ifdef _OS_WINDOWS_
int uv_dup(uv_os_fd_t fd, uv_os_fd_t* dupfd) {
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
        return ERROR_INVALID_HANDLE;
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
int uv_dup(uv_os_fd_t fd, uv_os_fd_t* dupfd) {
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
    switch(uv_guess_handle(fd)) {
    case UV_TTY:
        handle = malloc(sizeof(uv_tty_t));
        if ((err = uv_tty_init(jl_io_loop, (uv_tty_t*)handle, fd, readable))) {
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
            dup2(nullfd, fd);
            close(nullfd);
        }
#endif
        // ...and continue on as in the UV_FILE case
        JL_FALLTHROUGH;
    case UV_FILE:
        handle = malloc(sizeof(jl_uv_file_t));
        {
            jl_uv_file_t *file = (jl_uv_file_t*)handle;
            file->loop = jl_io_loop;
            file->type = UV_FILE;
            file->file = fd;
            file->data = NULL;
        }
        break;
    case UV_NAMED_PIPE:
        handle = malloc(sizeof(uv_pipe_t));
        if ((err = uv_pipe_init(jl_io_loop, (uv_pipe_t*)handle, 0))) {
            jl_errorf("error initializing %s in uv_pipe_init: %s (%s %d)", stdio, uv_strerror(err), uv_err_name(err), err);
        }
        if ((err = uv_pipe_open((uv_pipe_t*)handle, fd))) {
            jl_errorf("error initializing %s in uv_pipe_open: %s (%s %d)", stdio, uv_strerror(err), uv_err_name(err), err);
        }
#ifndef _OS_WINDOWS_
        // remove flags set erroneously by libuv:
        if (readable)
            ((uv_pipe_t*)handle)->flags &= ~UV_STREAM_WRITABLE;
        else
            ((uv_pipe_t*)handle)->flags &= ~UV_STREAM_READABLE;
#endif
        ((uv_pipe_t*)handle)->data = NULL;
        break;
    case UV_TCP:
        handle = malloc(sizeof(uv_tcp_t));
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

void init_stdio(void)
{
    JL_STDIN  = (uv_stream_t*)init_stdio_handle("stdin", UV_STDIN_FD, 1);
    JL_STDOUT = (uv_stream_t*)init_stdio_handle("stdout", UV_STDOUT_FD, 0);
    JL_STDERR = (uv_stream_t*)init_stdio_handle("stderr", UV_STDERR_FD, 0);
    jl_flush_cstdio();
}

#ifdef JL_USE_INTEL_JITEVENTS
char jl_using_intel_jitevents; // Non-zero if running under Intel VTune Amplifier
#endif

#ifdef JL_USE_OPROFILE_JITEVENTS
char jl_using_oprofile_jitevents = 0; // Non-zero if running under OProfile
#endif

#ifdef JL_USE_PERF_JITEVENTS
char jl_using_perf_jitevents = 0;
#endif

int isabspath(const char *in)
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

static char *abspath(const char *in, int nprefix)
{ // compute an absolute path location, so that chdir doesn't change the file reference
  // ignores (copies directly over) nprefix characters at the start of abspath
#ifndef _OS_WINDOWS_
    char *out = realpath(in + nprefix, NULL);
    if (out) {
        if (nprefix > 0) {
            size_t sz = strlen(out) + 1;
            char *cpy = (char*)malloc(sz + nprefix);
            if (!cpy)
                jl_errorf("fatal error: failed to allocate memory: %s", strerror(errno));
            memcpy(cpy, in, nprefix);
            memcpy(cpy + nprefix, out, sz);
            free(out);
            out = cpy;
        }
    }
    else {
        size_t sz = strlen(in + nprefix) + 1;
        if (in[nprefix] == PATHSEPSTRING[0]) {
            out = (char*)malloc(sz + nprefix);
            if (!out)
                jl_errorf("fatal error: failed to allocate memory: %s", strerror(errno));
            memcpy(out, in, sz + nprefix);
        }
        else {
            size_t path_size = PATH_MAX;
            char *path = (char*)malloc(PATH_MAX);
            if (!path)
                jl_errorf("fatal error: failed to allocate memory: %s", strerror(errno));
            if (uv_cwd(path, &path_size)) {
                jl_error("fatal error: unexpected error while retrieving current working directory");
            }
            out = (char*)malloc(path_size + 1 + sz + nprefix);
            memcpy(out, in, nprefix);
            memcpy(out + nprefix, path, path_size);
            out[nprefix + path_size] = PATHSEPSTRING[0];
            memcpy(out + nprefix + path_size + 1, in + nprefix, sz);
            free(path);
        }
    }
#else
    DWORD n = GetFullPathName(in + nprefix, 0, NULL, NULL);
    if (n <= 0) {
        jl_error("fatal error: jl_options.image_file path too long or GetFullPathName failed");
    }
    char *out = (char*)malloc(n + nprefix);
    DWORD m = GetFullPathName(in + nprefix, n, out + nprefix, NULL);
    if (n != m + 1) {
        jl_error("fatal error: jl_options.image_file path too long or GetFullPathName failed");
    }
    memcpy(out, in, nprefix);
#endif
    return out;
}

static void jl_resolve_sysimg_location(JL_IMAGE_SEARCH rel)
{   // this function resolves the paths in jl_options to absolute file locations as needed
    // and it replaces the pointers to `julia_bindir`, `julia_bin`, `image_file`, and output file paths
    // it may fail, print an error, and exit(1) if any of these paths are longer than PATH_MAX
    //
    // note: if you care about lost memory, you should call the appropriate `free()` function
    // on the original pointer for each `char*` you've inserted into `jl_options`, after
    // calling `julia_init()`
    char *free_path = (char*)malloc(PATH_MAX);
    size_t path_size = PATH_MAX;
    if (uv_exepath(free_path, &path_size)) {
        jl_error("fatal error: unexpected error while retrieving exepath");
    }
    if (path_size >= PATH_MAX) {
        jl_error("fatal error: jl_options.julia_bin path too long");
    }
    jl_options.julia_bin = (char*)malloc(path_size+1);
    memcpy((char*)jl_options.julia_bin, free_path, path_size);
    ((char*)jl_options.julia_bin)[path_size] = '\0';
    if (!jl_options.julia_bindir) {
        jl_options.julia_bindir = getenv("JULIA_BINDIR");
        if (!jl_options.julia_bindir) {
            jl_options.julia_bindir = dirname(free_path);
        }
    }
    if (jl_options.julia_bindir)
        jl_options.julia_bindir = abspath(jl_options.julia_bindir, 0);
    free(free_path);
    free_path = NULL;
    if (jl_options.image_file) {
        if (rel == JL_IMAGE_JULIA_HOME && !isabspath(jl_options.image_file)) {
            // build time path, relative to JULIA_BINDIR
            free_path = (char*)malloc(PATH_MAX);
            int n = snprintf(free_path, PATH_MAX, "%s" PATHSEPSTRING "%s",
                             jl_options.julia_bindir, jl_options.image_file);
            if (n >= PATH_MAX || n < 0) {
                jl_error("fatal error: jl_options.image_file path too long");
            }
            jl_options.image_file = free_path;
        }
        if (jl_options.image_file)
            jl_options.image_file = abspath(jl_options.image_file, 0);
        if (free_path) {
            free(free_path);
            free_path = NULL;
        }
    }
    if (jl_options.outputo)
        jl_options.outputo = abspath(jl_options.outputo, 0);
    if (jl_options.outputji)
        jl_options.outputji = abspath(jl_options.outputji, 0);
    if (jl_options.outputbc)
        jl_options.outputbc = abspath(jl_options.outputbc, 0);
    if (jl_options.machine_file)
        jl_options.machine_file = abspath(jl_options.machine_file, 0);
    if (jl_options.project && strncmp(jl_options.project, "@.", strlen(jl_options.project)) != 0)
        jl_options.project = abspath(jl_options.project, 0);

    const char **cmdp = jl_options.cmds;
    if (cmdp) {
        for (; *cmdp; cmdp++) {
            const char *cmd = *cmdp;
            if (cmd[0] == 'L') {
                *cmdp = abspath(cmd, 1);
            }
        }
    }
}

static void jl_set_io_wait(int v)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    ptls->io_wait = v;
}

void _julia_init(JL_IMAGE_SEARCH rel)
{
    jl_init_timing();
#ifdef JULIA_ENABLE_THREADING
    // Make sure we finalize the tls callback before starting any threads.
    jl_get_ptls_states_getter();
#endif
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_safepoint_init();
    libsupport_init();
    ios_set_io_wait_func = jl_set_io_wait;
    jl_io_loop = uv_default_loop(); // this loop will internal events (spawning process etc.),
                                    // best to call this first, since it also initializes libuv
    jl_init_signal_async();
    restore_signals();

    jl_resolve_sysimg_location(rel);
    // loads sysimg if available, and conditionally sets jl_options.cpu_target
    if (jl_options.image_file)
        jl_preload_sysimg_so(jl_options.image_file);
    if (jl_options.cpu_target == NULL)
        jl_options.cpu_target = "native";

    jl_page_size = jl_getpagesize();
    uint64_t total_mem = uv_get_total_memory();
    if (total_mem >= (size_t)-1) {
        total_mem = (size_t)-1;
    }
    jl_arr_xtralloc_limit = total_mem / 100;  // Extra allocation limited to 1% of total RAM
    jl_find_stack_bottom();
    jl_dl_handle = jl_load_dynamic_library(NULL, JL_RTLD_DEFAULT);
#ifdef _OS_WINDOWS_
    jl_ntdll_handle = jl_dlopen("ntdll.dll", 0); // bypass julia's pathchecking for system dlls
    jl_kernel32_handle = jl_dlopen("kernel32.dll", 0);
#if defined(_MSC_VER) && _MSC_VER == 1800
    jl_crtdll_handle = jl_dlopen("msvcr120.dll", 0);
#else
    jl_crtdll_handle = jl_dlopen("msvcrt.dll", 0);
#endif
    jl_winsock_handle = jl_dlopen("ws2_32.dll", 0);
    jl_exe_handle = GetModuleHandleA(NULL);
    SymSetOptions(SYMOPT_UNDNAME | SYMOPT_DEFERRED_LOADS | SYMOPT_LOAD_LINES);
    if (!SymInitialize(GetCurrentProcess(), NULL, 1)) {
        jl_printf(JL_STDERR, "WARNING: failed to initialize stack walk info\n");
    }
    needsSymRefreshModuleList = 0;
    HMODULE jl_dbghelp = (HMODULE) jl_dlopen("dbghelp.dll", 0);
    if (jl_dbghelp)
        hSymRefreshModuleList = (BOOL (WINAPI*)(HANDLE)) jl_dlsym(jl_dbghelp, "SymRefreshModuleList");
#else
    jl_exe_handle = jl_dlopen(NULL, JL_RTLD_NOW);
#ifdef RTLD_DEFAULT
    jl_RTLD_DEFAULT_handle = RTLD_DEFAULT;
#else
    jl_RTLD_DEFAULT_handle = jl_exe_handle;
#endif
#endif

#if defined(JL_USE_INTEL_JITEVENTS)
    const char *jit_profiling = getenv("ENABLE_JITPROFILING");
    if (jit_profiling && atoi(jit_profiling)) {
        jl_using_intel_jitevents = 1;
    }
#endif

#if defined(JL_USE_OPROFILE_JITEVENTS)
    const char *jit_profiling = getenv("ENABLE_JITPROFILING");
    if (jit_profiling && atoi(jit_profiling)) {
        jl_using_oprofile_jitevents = 1;
    }
#endif

#if defined(JL_USE_PERF_JITEVENTS)
    const char *jit_profiling = getenv("ENABLE_JITPROFILING");
    if (jit_profiling && atoi(jit_profiling)) {
        jl_using_perf_jitevents= 1;
    }
#endif

#if defined(__linux__)
    int ncores = jl_cpu_threads();
    if (ncores > 1) {
        cpu_set_t cpumask;
        CPU_ZERO(&cpumask);
        for(int i=0; i < ncores; i++) {
            CPU_SET(i, &cpumask);
        }
        sched_setaffinity(0, sizeof(cpu_set_t), &cpumask);
    }
#endif

    jl_init_threading();

    jl_gc_init();
    jl_gc_enable(0);
    jl_init_types();
    jl_init_frontend();
    jl_init_tasks();
    jl_init_root_task(ptls->stack_lo, ptls->stack_hi-ptls->stack_lo);

#ifdef ENABLE_TIMINGS
    jl_root_task->timing_stack = jl_root_timing;
#endif

    init_stdio();
    // libuv stdio cleanup depends on jl_init_tasks() because JL_TRY is used in jl_atexit_hook()

    if ((jl_options.outputo || jl_options.outputbc) &&
        (jl_options.code_coverage || jl_options.malloc_log)) {
        jl_error("cannot generate code-coverage or track allocation information while generating a .o or .bc output file");
    }

    jl_init_codegen();

    jl_start_threads();

    jl_an_empty_vec_any = (jl_value_t*)jl_alloc_vec_any(0);
    jl_init_serializer();
    jl_init_intrinsic_properties();

    if (!jl_options.image_file) {
        jl_core_module = jl_new_module(jl_symbol("Core"));
        jl_type_typename->mt->module = jl_core_module;
        jl_top_module = jl_core_module;
        ptls->current_module = jl_core_module;
        jl_init_intrinsic_functions();
        jl_init_primitives();
        jl_get_builtins();

        jl_new_main_module();
        jl_internal_main_module = jl_main_module;

        ptls->current_module = jl_core_module;
        for (int t = 0; t < jl_n_threads; t++) {
            jl_all_tls_states[t]->root_task->current_module = jl_core_module;
        }

        jl_load(jl_core_module, "boot.jl");
        jl_get_builtin_hooks();
        jl_init_box_caches();
    }

    if (jl_options.image_file) {
        JL_TRY {
            jl_restore_system_image(jl_options.image_file);
        }
        JL_CATCH {
            jl_printf(JL_STDERR, "error during init:\n");
            jl_static_show(JL_STDERR, ptls->exception_in_transit);
            jl_printf(JL_STDERR, "\n");
            jl_exit(1);
        }
    }

    // set module field of primitive types
    int i;
    void **table = jl_core_module->bindings.table;
    for(i=1; i < jl_core_module->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            jl_value_t *v = b->value;
            if (v) {
                if (jl_is_unionall(v))
                    v = jl_unwrap_unionall(v);
                if (jl_is_datatype(v)) {
                    jl_datatype_t *tt = (jl_datatype_t*)v;
                    tt->name->module = jl_core_module;
                    if (tt->name->mt)
                        tt->name->mt->module = jl_core_module;
                }
            }
        }
    }

    // the Main module is the one which is always open, and set as the
    // current module for bare (non-module-wrapped) toplevel expressions.
    // it does "using Base" if Base is available.
    if (jl_base_module != NULL) {
        jl_add_standard_imports(jl_main_module);
    }
    ptls->current_module = jl_main_module;
    for (int t = 0; t < jl_n_threads; t++) {
        jl_all_tls_states[t]->root_task->current_module = jl_main_module;
    }

    // This needs to be after jl_start_threads
    if (jl_options.handle_signals == JL_OPTIONS_HANDLE_SIGNALS_ON)
        jl_install_default_signal_handlers();

    jl_gc_enable(1);

    if (jl_options.image_file && (!jl_generating_output() || jl_options.incremental) && jl_module_init_order) {
        jl_array_t *init_order = jl_module_init_order;
        JL_GC_PUSH1(&init_order);
        jl_module_init_order = NULL;
        int i, l = jl_array_len(init_order);
        for (i = 0; i < l; i++) {
            jl_value_t *mod = jl_array_ptr_ref(init_order, i);
            jl_module_run_initializer((jl_module_t*)mod);
        }
        JL_GC_POP();
    }

    if (jl_options.handle_signals == JL_OPTIONS_HANDLE_SIGNALS_ON)
        jl_install_sigint_handler();
}

static jl_value_t *core(const char *name)
{
    return jl_get_global(jl_core_module, jl_symbol(name));
}

// fetch references to things defined in boot.jl
void jl_get_builtin_hooks(void)
{
    int t;
    for (t = 0; t < jl_n_threads; t++) {
        jl_ptls_t ptls2 = jl_all_tls_states[t];
        ptls2->root_task->tls = jl_nothing;
        ptls2->root_task->donenotify = jl_nothing;
        ptls2->root_task->exception = jl_nothing;
        ptls2->root_task->result = jl_nothing;
    }

    jl_char_type    = (jl_datatype_t*)core("Char");
    jl_int8_type    = (jl_datatype_t*)core("Int8");
    jl_int16_type   = (jl_datatype_t*)core("Int16");
    jl_uint16_type  = (jl_datatype_t*)core("UInt16");

    jl_float16_type = (jl_datatype_t*)core("Float16");
    jl_float32_type = (jl_datatype_t*)core("Float32");
    jl_float64_type = (jl_datatype_t*)core("Float64");
    jl_floatingpoint_type = (jl_datatype_t*)core("AbstractFloat");
    jl_number_type = (jl_datatype_t*)core("Number");
    jl_signed_type = (jl_datatype_t*)core("Signed");
    jl_datatype_t *jl_unsigned_type = (jl_datatype_t*)core("Unsigned");
    jl_datatype_t *jl_integer_type = (jl_datatype_t*)core("Integer");
    jl_bool_type->super = jl_integer_type;
    jl_uint8_type->super = jl_unsigned_type;
    jl_int32_type->super = jl_signed_type;
    jl_int64_type->super = jl_signed_type;
    jl_uint32_type->super = jl_unsigned_type;
    jl_uint64_type->super = jl_unsigned_type;

    jl_errorexception_type = (jl_datatype_t*)core("ErrorException");
    jl_stackovf_exception  = jl_new_struct_uninit((jl_datatype_t*)core("StackOverflowError"));
    jl_diverror_exception  = jl_new_struct_uninit((jl_datatype_t*)core("DivideError"));
    jl_undefref_exception  = jl_new_struct_uninit((jl_datatype_t*)core("UndefRefError"));
    jl_undefvarerror_type  = (jl_datatype_t*)core("UndefVarError");
    jl_interrupt_exception = jl_new_struct_uninit((jl_datatype_t*)core("InterruptException"));
    jl_boundserror_type    = (jl_datatype_t*)core("BoundsError");
    jl_memory_exception    = jl_new_struct_uninit((jl_datatype_t*)core("OutOfMemoryError"));
    jl_readonlymemory_exception = jl_new_struct_uninit((jl_datatype_t*)core("ReadOnlyMemoryError"));
    jl_typeerror_type = (jl_datatype_t*)core("TypeError");

#ifdef SEGV_EXCEPTION
    jl_segv_exception      = jl_new_struct_uninit((jl_datatype_t*)core("SegmentationFault"));
#endif

    jl_weakref_type = (jl_datatype_t*)core("WeakRef");
    jl_vecelement_typename = ((jl_datatype_t*)jl_unwrap_unionall(core("VecElement")))->name;

    jl_argumenterror_type = (jl_datatype_t*)core("ArgumentError");
    jl_methoderror_type = (jl_datatype_t*)core("MethodError");
    jl_loaderror_type = (jl_datatype_t*)core("LoadError");
    jl_initerror_type = (jl_datatype_t*)core("InitError");
}

void jl_get_builtins(void)
{
    jl_builtin_throw = core("throw");           jl_builtin_is = core("===");
    jl_builtin_typeof = core("typeof");         jl_builtin_sizeof = core("sizeof");
    jl_builtin_issubtype = core("<:");          jl_builtin_isa = core("isa");
    jl_builtin_typeassert = core("typeassert"); jl_builtin__apply = core("_apply");
    jl_builtin_isdefined = core("isdefined");   jl_builtin_nfields = core("nfields");
    jl_builtin_tuple = core("tuple");           jl_builtin_svec = core("svec");
    jl_builtin_getfield = core("getfield");     jl_builtin_setfield = core("setfield!");
    jl_builtin_fieldtype = core("fieldtype");   jl_builtin_arrayref = core("arrayref");
    jl_builtin_arrayset = core("arrayset");     jl_builtin_arraysize = core("arraysize");
    jl_builtin_apply_type = core("apply_type"); jl_builtin_applicable = core("applicable");
    jl_builtin_invoke = core("invoke");         jl_builtin__expr = core("_expr");
    jl_builtin_ifelse = core("ifelse");
}

#ifdef __cplusplus
}
#endif
