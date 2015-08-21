// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  init.c
  system initialization and global state
*/
#include "platform.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <fcntl.h>

#include <errno.h>

#if !defined(_OS_WINDOWS_) || defined(_COMPILER_MINGW_)
#include <getopt.h>
#endif

#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _MSC_VER
DLLEXPORT char * dirname(char *);
#else
#include <libgen.h>
#endif

#ifdef _OS_WINDOWS_
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <dbghelp.h>
#include <io.h>
extern int needsSymRefreshModuleList;
extern BOOL (WINAPI *hSymRefreshModuleList)(HANDLE);
#else
#include <sys/resource.h>
#include <unistd.h>
#endif

static const char system_image_path[256] = "\0" JL_SYSTEM_IMAGE_PATH;

jl_options_t jl_options = { 0,    // quiet
                            NULL, // julia_home
                            NULL, // julia_bin
                            NULL, // eval
                            NULL, // print
                            NULL, // postboot
                            NULL, // load
                            &system_image_path[1], // image_file
                            NULL, // cpu_taget ("native", "core2", etc...)
                            0,    // nprocs
                            NULL, // machinefile
                            0,    // isinteractive
                            0,    // color
                            JL_OPTIONS_HISTORYFILE_ON, // historyfile
                            0,    // startupfile
                            JL_OPTIONS_COMPILE_DEFAULT, // compile_enabled
                            0,    // code_coverage
                            0,    // malloc_log
                            0,    // opt_level
                            JL_OPTIONS_CHECK_BOUNDS_DEFAULT, // check_bounds
                            1,    // depwarn
                            1,    // can_inline
                            JL_OPTIONS_FAST_MATH_DEFAULT,
                            0,    // worker
                            JL_OPTIONS_HANDLE_SIGNALS_ON,
#ifdef _OS_WINDOWS_
// TODO remove this when using LLVM 3.5+
                            JL_OPTIONS_USE_PRECOMPILED_NO,
#else
                            JL_OPTIONS_USE_PRECOMPILED_YES,
#endif
                            NULL, // bindto
                            NULL, // outputbc
                            NULL, // outputo
                            NULL, // outputji
                            0, // incremental
};

int jl_boot_file_loaded = 0;
char *jl_stack_lo;
char *jl_stack_hi;
size_t jl_page_size;

static void jl_find_stack_bottom(void)
{
    size_t stack_size;
#ifndef _OS_WINDOWS_
    struct rlimit rl;

    // When using memory sanitizer, increase stack size because msan bloats stack usage
#if defined(__has_feature)
#if __has_feature(memory_sanitizer)
    const rlim_t kStackSize = 32 * 1024 * 1024;   // 32MB stack
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

    getrlimit(RLIMIT_STACK, &rl);
    stack_size = rl.rlim_cur;
#else
    stack_size = 262144;  // guess
#endif
    jl_stack_hi = (char*)&stack_size;
    jl_stack_lo = jl_stack_hi - stack_size;
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
    if (handle != (uv_handle_t*)JL_STDOUT && handle != (uv_handle_t*)JL_STDERR)
        jl_uv_exitcleanup_add(handle, (struct uv_shutdown_queue*)arg);
}

void jl_write_coverage_data(void);
void jl_write_malloc_log(void);
static void julia_save(void);

static struct uv_shutdown_queue_item *next_shutdown_queue_item(struct uv_shutdown_queue_item *item)
{
    struct uv_shutdown_queue_item *rv = item->next;
    free(item);
    return rv;
}

DLLEXPORT void jl_atexit_hook(int exitcode)
{
    if (exitcode == 0) julia_save();
    jl_print_gc_stats(JL_STDERR);
    if (jl_options.code_coverage)
        jl_write_coverage_data();
    if (jl_options.malloc_log)
        jl_write_malloc_log();
    if (jl_base_module) {
        jl_value_t *f = jl_get_global(jl_base_module, jl_symbol("_atexit"));
        if (f!=NULL && jl_is_function(f)) {
            JL_TRY {
                jl_apply((jl_function_t*)f, NULL, 0);
            }
            JL_CATCH {
                jl_printf(JL_STDERR, "\natexit hook threw an error: ");
                jl_static_show(JL_STDERR, jl_exception_in_transit);
            }
        }
    }

    jl_gc_run_all_finalizers();

    uv_loop_t *loop = jl_global_event_loop();

    if (loop == NULL) {
        return;
    }

    struct uv_shutdown_queue queue = {NULL, NULL};
    uv_walk(loop, jl_uv_exitcleanup_walk, &queue);
    // close stdout and stderr last, since we like being
    // able to show stuff (incl. printf's)
    if (JL_STDOUT != (void*) STDOUT_FILENO &&
        ((uv_handle_t*)JL_STDOUT)->type < UV_HANDLE_TYPE_MAX)
        jl_uv_exitcleanup_add((uv_handle_t*)JL_STDOUT, &queue);
    if (JL_STDERR != (void*) STDERR_FILENO &&
        ((uv_handle_t*)JL_STDERR)->type < UV_HANDLE_TYPE_MAX)
        jl_uv_exitcleanup_add((uv_handle_t*)JL_STDERR, &queue);
    //uv_unref((uv_handle_t*)JL_STDOUT);
    //uv_unref((uv_handle_t*)JL_STDERR);
    struct uv_shutdown_queue_item *item = queue.first;
    while (item) {
        JL_TRY {
            while (item) {
                uv_handle_t *handle = item->h;
                if (handle->type != UV_FILE && uv_is_closing(handle)) {
                    item = next_shutdown_queue_item(item);
                    continue;
                }
                switch(handle->type) {
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
                case UV_PROCESS:
                case UV_FILE:
                    // These will be shutdown as appropriate by jl_close_uv
                    jl_close_uv(handle);
                    break;
                case UV_HANDLE:
                case UV_STREAM:
                case UV_UNKNOWN_HANDLE:
                case UV_HANDLE_TYPE_MAX:
                case UV_RAW_FD:
                case UV_RAW_HANDLE:
                default:
                    assert(0);
                }
                item = next_shutdown_queue_item(item);
            }
        }
        JL_CATCH {
            //error handling -- continue cleanup, as much as possible
            uv_unref(item->h);
            jl_printf(JL_STDERR, "error during exit cleanup: close: ");
            jl_static_show(JL_STDERR, jl_exception_in_transit);
            item = next_shutdown_queue_item(item);
        }
    }
    // force libuv to spin until everything has finished closing
    loop->stop_flag = 0;
    while (uv_run(loop,UV_RUN_DEFAULT)) {}
}

void jl_get_builtin_hooks(void);

DLLEXPORT uv_lib_t *jl_dl_handle;
uv_lib_t _jl_RTLD_DEFAULT_handle;
uv_lib_t *jl_RTLD_DEFAULT_handle=&_jl_RTLD_DEFAULT_handle;
#ifdef _OS_WINDOWS_
uv_lib_t _jl_ntdll_handle;
uv_lib_t _jl_exe_handle;
uv_lib_t _jl_kernel32_handle;
uv_lib_t _jl_crtdll_handle;
uv_lib_t _jl_winsock_handle;

DLLEXPORT uv_lib_t *jl_exe_handle=&_jl_exe_handle;
uv_lib_t *jl_ntdll_handle=&_jl_ntdll_handle;
uv_lib_t *jl_kernel32_handle=&_jl_kernel32_handle;
uv_lib_t *jl_crtdll_handle=&_jl_crtdll_handle;
uv_lib_t *jl_winsock_handle=&_jl_winsock_handle;
#endif

uv_loop_t *jl_io_loop;

void *init_stdio_handle(uv_file fd,int readable)
{
    void *handle;
    uv_handle_type type = uv_guess_handle(fd);
    jl_uv_file_t *file;
#ifndef _OS_WINDOWS_
    // Duplicate the file descriptor so we can later dup it over if we want to redirect
    // STDIO without having to worry about closing the associated libuv object.
    // On windows however, libuv objects remember streams by their HANDLE, so this is
    // unnecessary.
    fd = dup(fd);
#endif
    //jl_printf(JL_STDOUT, "%d: %d -- %d\n", fd, type, 0);
    switch(type) {
        case UV_TTY:
            handle = malloc(sizeof(uv_tty_t));
            if (uv_tty_init(jl_io_loop,(uv_tty_t*)handle,fd,readable)) {
                jl_errorf("error initializing stdio in uv_tty_init (%d, %d)", fd, type);
            }
            ((uv_tty_t*)handle)->data=0;
            uv_tty_set_mode((uv_tty_t*)handle,0); //cooked stdio
            break;
        case UV_UNKNOWN_HANDLE:
            // dup the descriptor with a new one pointing at the bit bucket ...
#if defined(_OS_WINDOWS_)
            _dup2(_open("NUL", O_RDWR | O_BINARY, _S_IREAD | _S_IWRITE), fd);
#else
            dup2(open("/dev/null", O_RDWR, S_IRUSR | S_IWUSR /* 0600 */ | S_IRGRP | S_IROTH /* 0644 */), fd);
#endif
            // ...and continue on as in the UV_FILE case
        case UV_FILE:
            file = (jl_uv_file_t*)malloc(sizeof(jl_uv_file_t));
            file->loop = jl_io_loop;
            file->type = UV_FILE;
            file->file = fd;
            file->data = 0;
            handle = file;
            break;
        case UV_NAMED_PIPE:
            handle = malloc(sizeof(uv_pipe_t));
            if (uv_pipe_init(jl_io_loop, (uv_pipe_t*)handle, (readable?UV_PIPE_READABLE:UV_PIPE_WRITABLE))) {
                jl_errorf("error initializing stdio in uv_pipe_init (%d, %d)", fd, type);
            }
            if (uv_pipe_open((uv_pipe_t*)handle,fd)) {
                jl_errorf("error initializing stdio in uv_pipe_open (%d, %d)", fd, type);
            }
            ((uv_pipe_t*)handle)->data=0;
            break;
        case UV_TCP:
            handle = malloc(sizeof(uv_tcp_t));
            if (uv_tcp_init(jl_io_loop, (uv_tcp_t*)handle)) {
                jl_errorf("error initializing stdio in uv_tcp_init (%d, %d)", fd, type);
            }
            if (uv_tcp_open((uv_tcp_t*)handle,fd)) {
                jl_errorf("error initializing stdio in uv_tcp_open (%d, %d)", fd, type);
            }
            ((uv_tcp_t*)handle)->data=0;
            break;
        case UV_UDP:
        default:
            jl_errorf("this type of handle for stdio is not yet supported (%d, %d)", fd, type);
            break;
    }
    return handle;
}

void init_stdio()
{   //order must be 2,1,0
    JL_STDERR = (uv_stream_t*)init_stdio_handle(STDERR_FILENO,0);
    JL_STDOUT = (uv_stream_t*)init_stdio_handle(STDOUT_FILENO,0);
    JL_STDIN  = (uv_stream_t*)init_stdio_handle(STDIN_FILENO,1);

    jl_flush_cstdio();
}

#ifdef JL_USE_INTEL_JITEVENTS
char jl_using_intel_jitevents; // Non-zero if running under Intel VTune Amplifier
#endif

int isabspath(const char *in)
{
#ifdef _OS_WINDOWS_
    char c0 = in[0];
    if (c0 == '/' || c0 == '\\') {
        return 1; // absolute path relative to %CD% (current drive), or UNC
    }
    else {
        int s = strlen(in);
        if (s > 2) {
            char c1 = in[1];
            char c2 = in[2];
            if (c1 == ':' && (c2 == '/' || c2 == '\\')) return 1; // absolute path
        }
    }
#else
    if (in[0] == '/') return 1; // absolute path
#endif
    return 0; // relative path
}

static char *abspath(const char *in)
{ // compute an absolute path location, so that chdir doesn't change the file reference
#ifndef _OS_WINDOWS_
    char *out = realpath(in, NULL);
    if (!out) {
        if (in[0] == PATHSEPSTRING[0]) {
            out = strdup(in);
        }
        else {
            size_t path_size = PATH_MAX;
            size_t len = strlen(in);
            char *path = (char*)malloc(PATH_MAX);
            if (uv_cwd(path, &path_size)) {
                jl_error("fatal error: unexpected error while retrieving current working directory");
            }
            if (path_size + len + 1 >= PATH_MAX) {
                jl_error("fatal error: current working directory path too long");
            }
            path[path_size-1] = PATHSEPSTRING[0];
            memcpy(path+path_size, in, len+1);
            out = strdup(path);
            free(path);
        }
    }
#else
    DWORD n = GetFullPathName(in, 0, NULL, NULL);
    if (n <= 0) {
        jl_error("fatal error: jl_options.image_file path too long or GetFullPathName failed");
    }
    char *out = (char*)malloc(n);
    DWORD m = GetFullPathName(in, n, out, NULL);
    if (n != m + 1) {
        jl_error("fatal error: jl_options.image_file path too long or GetFullPathName failed");
    }
#endif
    return out;
}

static void jl_resolve_sysimg_location(JL_IMAGE_SEARCH rel)
{ // this function resolves the paths in jl_options to absolute file locations as needed
  // and it replaces the pointers to `julia_home`, `julia_bin`, `image_file`, and output file paths
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
    jl_options.julia_bin = strdup(free_path);
    if (!jl_options.julia_home) {
        jl_options.julia_home = getenv("JULIA_HOME");
        if (!jl_options.julia_home) {
            jl_options.julia_home = dirname(free_path);
        }
    }
    if (jl_options.julia_home)
        jl_options.julia_home = abspath(jl_options.julia_home);
    free(free_path);
    free_path = NULL;
    if (jl_options.image_file) {
        if (rel == JL_IMAGE_JULIA_HOME && !isabspath(jl_options.image_file)) {
            // build time path, relative to JULIA_HOME
            free_path = (char*)malloc(PATH_MAX);
            int n = snprintf(free_path, PATH_MAX, "%s" PATHSEPSTRING "%s",
                             jl_options.julia_home, jl_options.image_file);
            if (n >= PATH_MAX || n < 0) {
                jl_error("fatal error: jl_options.image_file path too long");
            }
            jl_options.image_file = free_path;
        }
        if (jl_options.image_file)
            jl_options.image_file = abspath(jl_options.image_file);
        if (free_path) {
            free(free_path);
            free_path = NULL;
        }
    }
    if (jl_options.outputo)
        jl_options.outputo = abspath(jl_options.outputo);
    if (jl_options.outputji)
        jl_options.outputji = abspath(jl_options.outputji);
    if (jl_options.outputbc)
        jl_options.outputbc = abspath(jl_options.outputbc);
    if (jl_options.machinefile)
        jl_options.machinefile = abspath(jl_options.machinefile);
    if (jl_options.load)
        jl_options.load = abspath(jl_options.load);
}

void _julia_init(JL_IMAGE_SEARCH rel)
{
    libsupport_init();
    jl_io_loop = uv_default_loop(); // this loop will internal events (spawning process etc.),
                                    // best to call this first, since it also initializes libuv
    restore_signals();
    jl_resolve_sysimg_location(rel);
    // loads sysimg if available, and conditionally sets jl_options.cpu_target
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
    jl_dl_handle = (uv_lib_t *) jl_load_dynamic_library(NULL, JL_RTLD_DEFAULT);
#ifdef RTLD_DEFAULT
    jl_RTLD_DEFAULT_handle->handle = RTLD_DEFAULT;
#else
    jl_RTLD_DEFAULT_handle->handle = jl_dl_handle->handle;
#endif
#ifdef _OS_WINDOWS_
    uv_dlopen("ntdll.dll", jl_ntdll_handle); // bypass julia's pathchecking for system dlls
    uv_dlopen("kernel32.dll", jl_kernel32_handle);
#if _MSC_VER == 1800
    uv_dlopen("msvcr120.dll", jl_crtdll_handle);
#else
    uv_dlopen("msvcrt.dll", jl_crtdll_handle);
#endif
    uv_dlopen("ws2_32.dll", jl_winsock_handle);
    _jl_exe_handle.handle = GetModuleHandleA(NULL);
    if (!DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                         GetCurrentProcess(), (PHANDLE)&hMainThread, 0,
                         TRUE, DUPLICATE_SAME_ACCESS)) {
        jl_printf(JL_STDERR, "WARNING: failed to access handle to main thread\n");
    }
    SymSetOptions(SYMOPT_UNDNAME | SYMOPT_DEFERRED_LOADS | SYMOPT_LOAD_LINES);
    if (!SymInitialize(GetCurrentProcess(), NULL, 1)) {
        jl_printf(JL_STDERR, "WARNING: failed to initialize stack walk info\n");
    }
    needsSymRefreshModuleList = 0;
    uv_lib_t jl_dbghelp;
    uv_dlopen("dbghelp.dll",&jl_dbghelp);
    if (uv_dlsym(&jl_dbghelp, "SymRefreshModuleList", (void**)&hSymRefreshModuleList))
        hSymRefreshModuleList = 0;
#endif

#if defined(JL_USE_INTEL_JITEVENTS)
    const char *jit_profiling = getenv("ENABLE_JITPROFILING");
    if (jit_profiling && atoi(jit_profiling)) {
        jl_using_intel_jitevents = 1;
    }
#endif

    jl_gc_init();
    jl_gc_enable(0);
    jl_init_frontend();
    jl_init_types();
    jl_init_tasks();
    jl_init_root_task(jl_stack_lo, jl_stack_hi-jl_stack_lo);

    init_stdio();
    // libuv stdio cleanup depends on jl_init_tasks() because JL_TRY is used in jl_atexit_hook()

    jl_init_codegen();
    jl_an_empty_cell = (jl_value_t*)jl_alloc_cell_1d(0);
    jl_init_serializer();

    if (!jl_options.image_file) {
        jl_core_module = jl_new_module(jl_symbol("Core"));
        jl_top_module = jl_core_module;
        jl_init_intrinsic_functions();
        jl_init_primitives();

        jl_new_main_module();
        jl_internal_main_module = jl_main_module;

        jl_current_module = jl_core_module;
        jl_root_task->current_module = jl_current_module;

        jl_load("boot.jl", sizeof("boot.jl"));
        jl_get_builtin_hooks();
        jl_boot_file_loaded = 1;
        jl_init_box_caches();
    }

    if (jl_options.image_file) {
        JL_TRY {
            jl_restore_system_image(jl_options.image_file);
        }
        JL_CATCH {
            jl_printf(JL_STDERR, "error during init:\n");
            jl_static_show(JL_STDERR, jl_exception_in_transit);
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
            if (b->value && jl_is_datatype(b->value)) {
                jl_datatype_t *tt = (jl_datatype_t*)b->value;
                tt->name->module = jl_core_module;
            }
        }
    }

    // the Main module is the one which is always open, and set as the
    // current module for bare (non-module-wrapped) toplevel expressions.
    // it does "using Base" if Base is available.
    if (jl_base_module != NULL) {
        jl_add_standard_imports(jl_main_module);
    }
    // eval() uses Main by default, so Main.eval === Core.eval
    jl_module_import(jl_main_module, jl_core_module, jl_symbol("eval"));
    jl_current_module = jl_main_module;
    jl_root_task->current_module = jl_current_module;

    if (jl_options.handle_signals == JL_OPTIONS_HANDLE_SIGNALS_ON)
        jl_install_default_signal_handlers();

    jl_gc_enable(1);

    if (jl_options.image_file) {
        jl_array_t *temp = jl_module_init_order;
        JL_GC_PUSH1(&temp);
        jl_module_init_order = NULL;
        jl_init_restored_modules(temp);
        JL_GC_POP();
    }

    if (jl_options.handle_signals == JL_OPTIONS_HANDLE_SIGNALS_ON)
        jl_install_sigint_handler();
}

extern int asprintf(char **str, const char *fmt, ...);

DLLEXPORT int jl_generating_output()
{
    return jl_options.outputo || jl_options.outputbc || jl_options.outputji;
}

void jl_compile_all(void);

static void julia_save()
{
    if (jl_options.compile_enabled == JL_OPTIONS_COMPILE_ALL)
        jl_compile_all();

    if (jl_options.incremental) {
        jl_array_t *worklist = jl_module_init_order;
        if (!worklist) {
            jl_printf(JL_STDERR, "WARNING: incremental output requested, but no modules defined during run\n");
            return;
        }
        if (jl_options.outputji)
            if (jl_save_incremental(jl_options.outputji, worklist))
                jl_exit(1);
        if (jl_options.outputbc)
            jl_printf(JL_STDERR, "WARNING: incremental output to a .bc file is not implemented\n");
        if (jl_options.outputo)
            jl_printf(JL_STDERR, "WARNING: incremental output to a .o file is not implemented\n");
    }
    else {
        ios_t *s = NULL;
        if (jl_options.outputo || jl_options.outputbc)
            s = jl_create_system_image();

        if (jl_options.outputji) {
            if (s == NULL) {
                jl_save_system_image(jl_options.outputji);
            }
            else {
                ios_t f;
                if (ios_file(&f, jl_options.outputji, 1, 1, 1, 1) == NULL)
                    jl_errorf("cannot open system image file \"%s\" for writing", jl_options.outputji);
                ios_write(&f, (const char*)s->buf, s->size);
                ios_close(&f);
            }
        }

        if (jl_options.outputbc)
            jl_dump_bitcode((char*)jl_options.outputbc, (const char*)s->buf, s->size);

        if (jl_options.outputo)
            jl_dump_objfile((char*)jl_options.outputo, 0, (const char*)s->buf, s->size);
    }
}

jl_function_t *jl_typeinf_func=NULL;

DLLEXPORT void jl_set_typeinf_func(jl_value_t* f)
{
    if (!jl_is_function(f))
        jl_error("jl_set_typeinf_func must set a jl_function_t*");
    jl_typeinf_func = (jl_function_t*)f;
}

static jl_value_t *core(char *name)
{
    return jl_get_global(jl_core_module, jl_symbol(name));
}

static jl_value_t *basemod(char *name)
{
    return jl_get_global(jl_base_module, jl_symbol(name));
}

// fetch references to things defined in boot.jl
void jl_get_builtin_hooks(void)
{
    jl_root_task->tls = jl_nothing;
    jl_root_task->consumers = jl_nothing;
    jl_root_task->donenotify = jl_nothing;
    jl_root_task->exception = jl_nothing;
    jl_root_task->result = jl_nothing;

    jl_char_type    = (jl_datatype_t*)core("Char");
    jl_int8_type    = (jl_datatype_t*)core("Int8");
    jl_uint8_type   = (jl_datatype_t*)core("UInt8");
    jl_int16_type   = (jl_datatype_t*)core("Int16");
    jl_uint16_type  = (jl_datatype_t*)core("UInt16");
    jl_uint32_type  = (jl_datatype_t*)core("UInt32");
    jl_uint64_type  = (jl_datatype_t*)core("UInt64");

    jl_float32_type = (jl_datatype_t*)core("Float32");
    jl_float64_type = (jl_datatype_t*)core("Float64");
    jl_floatingpoint_type = (jl_datatype_t*)core("AbstractFloat");
    jl_number_type = (jl_datatype_t*)core("Number");
    jl_signed_type = (jl_datatype_t*)core("Signed");

    jl_stackovf_exception  = jl_new_struct_uninit((jl_datatype_t*)core("StackOverflowError"));
    jl_diverror_exception  = jl_new_struct_uninit((jl_datatype_t*)core("DivideError"));
    jl_domain_exception    = jl_new_struct_uninit((jl_datatype_t*)core("DomainError"));
    jl_overflow_exception  = jl_new_struct_uninit((jl_datatype_t*)core("OverflowError"));
    jl_inexact_exception   = jl_new_struct_uninit((jl_datatype_t*)core("InexactError"));
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

    jl_utf8_string_type = (jl_datatype_t*)core("UTF8String");
    jl_symbolnode_type = (jl_datatype_t*)core("SymbolNode");
    jl_weakref_type = (jl_datatype_t*)core("WeakRef");

    jl_array_uint8_type = jl_apply_type((jl_value_t*)jl_array_type,
                                        jl_svec2(jl_uint8_type, jl_box_long(1)));
}

DLLEXPORT void jl_get_system_hooks(void)
{
    if (jl_errorexception_type) return; // only do this once

    jl_errorexception_type = (jl_datatype_t*)basemod("ErrorException");
    jl_argumenterror_type = (jl_datatype_t*)basemod("ArgumentError");
    jl_methoderror_type = (jl_datatype_t*)basemod("MethodError");
    jl_loaderror_type = (jl_datatype_t*)basemod("LoadError");
    jl_initerror_type = (jl_datatype_t*)basemod("InitError");
    jl_complex_type = (jl_datatype_t*)basemod("Complex");
}

#ifdef __cplusplus
}
#endif
