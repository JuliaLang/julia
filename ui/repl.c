// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  repl.c
  system startup, main(), and console interaction
*/

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <signal.h>
#include <assert.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <ctype.h>

#ifndef _MSC_VER
#include <unistd.h>
#include <getopt.h>
#else
#include "getopt.h"
#endif

#include "uv.h"
#define WHOLE_ARCHIVE
#include "../src/julia.h"

#ifndef JL_SYSTEM_IMAGE_PATH
#error "JL_SYSTEM_IMAGE_PATH not defined!"
#endif

#ifdef __cplusplus
extern "C" {
#endif

static int lisp_prompt = 0;
static int codecov  = JL_LOG_NONE;
static int malloclog= JL_LOG_NONE;
static int imagepathspecified = 0;

static const char usage[] = "julia [options] [program] [args...]\n";
static const char opts[]  =
    " -v, --version            Display version information\n"
    " -h, --help               Print this message\n"
    " -q, --quiet              Quiet startup without banner\n"
    " -H, --home <dir>         Set location of julia executable\n\n"

    " -e, --eval <expr>         Evaluate <expr>\n"
    " -E, --print <expr>        Evaluate and show <expr>\n"
    " -P, --post-boot <expr>    Evaluate <expr>, but don't disable interactive mode\n"
    " -L, --load <file>         Load <file> immediately on all processors\n"
    " -J, --sysimage <file>     Start up with the given system image file\n"
    " -C, --cpu-target <target> Limit usage of cpu features up to <target>\n\n"

    " -p, --procs {N|auto}      Integer value N launches N additional local worker processes\n"
    "                           'auto' launches as many workers as the number of local cores\n"
    " --machinefile <file>      Run processes on hosts listed in <file>\n\n"

    " -i                        Force isinteractive() to be true\n"
    " --color={yes|no}          Enable or disable color text\n\n"
    " --history-file={yes|no}   Load or save history\n"
    " --no-history-file         Don't load history file (deprecated, use --history-file=no)\n"
    " --startup-file={yes|no}   Load ~/.juliarc.jl\n"
    " -f, --no-startup          Don't load ~/.juliarc (deprecated, use --startup-file=no)\n"
    " -F                        Load ~/.juliarc (deprecated, use --startup-file=yes)\n\n"

    " --compile={yes|no|all}    Enable or disable compiler, or request exhaustive compilation\n\n"

    " --code-coverage={none|user|all}, --code-coverage\n"
    "                           Count executions of source lines (omitting setting is equivalent to 'user')\n\n"

    " --track-allocation={none|user|all}, --track-allocation\n"
    "                           Count bytes allocated by each source line\n\n"
    " -O, --optimize\n"
    "                           Run time-intensive code optimizations\n"
    " --check-bounds={yes|no}   Emit bounds checks always or never (ignoring declarations)\n"
    " --dump-bitcode={yes|no}   Dump bitcode for the system image (used with --build)\n"
    " --depwarn={yes|no}        Enable or disable syntax and method deprecation warnings\n"
    " --inline={yes|no}         Control whether inlining is permitted (overrides functions declared as @inline)\n";

void parse_opts(int *argcp, char ***argvp)
{
    enum { opt_machinefile = 300,
           opt_color,
           opt_history_file,
           opt_no_history_file,
           opt_startup_file,
           opt_compile,
           opt_code_coverage,
           opt_track_allocation,
           opt_check_bounds,
           opt_dump_bitcode,
           opt_depwarn,
           opt_inline,
           opt_math_mode,
           opt_worker,
           opt_bind_to
    };
    static char* shortopts = "+vhqFfH:e:E:P:L:J:C:ip:Ob:";
    static struct option longopts[] = {
        // exposed command line options
        // NOTE: This set of required arguments need to be kept in sync
        // with the required arguments defined in base/client.jl `process_options()`
        { "version",         no_argument,       0, 'v' },
        { "help",            no_argument,       0, 'h' },
        { "quiet",           no_argument,       0, 'q' },
        { "home",            required_argument, 0, 'H' },
        { "eval",            required_argument, 0, 'e' },
        { "print",           required_argument, 0, 'E' },
        { "post-boot",       required_argument, 0, 'P' },
        { "load",            required_argument, 0, 'L' },
        { "sysimage",        required_argument, 0, 'J' },
        { "cpu-target",      required_argument, 0, 'C' },
        { "procs",           required_argument, 0, 'p' },
        { "machinefile",     required_argument, 0, opt_machinefile },
        { "color",           required_argument, 0, opt_color },
        { "history-file",    required_argument, 0, opt_history_file },
        { "no-history-file", no_argument,       0, opt_no_history_file }, // deprecated
        { "startup-file",    required_argument, 0, opt_startup_file },
        { "no-startup",      no_argument,       0, 'f' },                 // deprecated
        { "compile",         required_argument, 0, opt_compile },
        { "code-coverage",   optional_argument, 0, opt_code_coverage },
        { "track-allocation",optional_argument, 0, opt_track_allocation },
        { "optimize",        no_argument,       0, 'O' },
        { "check-bounds",    required_argument, 0, opt_check_bounds },
        { "dump-bitcode",    required_argument, 0, opt_dump_bitcode },
        { "depwarn",         required_argument, 0, opt_depwarn },
        { "inline",          required_argument, 0, opt_inline },
        { "math-mode",       required_argument, 0, opt_math_mode },
        // hidden command line options
        { "build",           required_argument, 0, 'b' },
        { "worker",          no_argument,       0, opt_worker },
        { "bind-to",         required_argument, 0, opt_bind_to },
        { "lisp",            no_argument,       &lisp_prompt, 1 },
        { 0, 0, 0, 0 }
    };
    // getopt handles argument parsing up to -- delineator
    int lastind = optind;
    int argc = *argcp;
    if (argc > 0) {
        for (int i=0; i < argc; i++) {
            if (!strcmp((*argvp)[i], "--")) {
                argc = i;
                break;
            }
        }
    }
    int c;
    char *endptr;
    opterr = 0;
    int skip = 0;
    while ((c = getopt_long(argc,*argvp,shortopts,longopts,0)) != -1) {
        switch (c) {
        case 0:
            break;
        case '?':
        if (optind != lastind) skip++;
            lastind = optind;
            break;
        case 'v': // version
            jl_printf(JL_STDOUT, "julia version %s\n", JULIA_VERSION_STRING);
            jl_exit(0);
        case 'h': // help
            jl_printf(JL_STDOUT, "%s%s", usage, opts);
            jl_exit(0);
        case 'q': // quiet
            jl_options.quiet = 1;
            break;
        case 'H': // home
            jl_options.julia_home = strdup(optarg);
            break;
        case 'e': // eval
            jl_options.eval = strdup(optarg);
            break;
        case 'E': // print
            jl_options.print = strdup(optarg);
            break;
        case 'P': // post-boot
            jl_options.postboot = strdup(optarg);
            break;
        case 'L': // load
            jl_options.load = strdup(optarg);
            break;
        case 'J': // sysimage
            jl_options.image_file = strdup(optarg);
            imagepathspecified = 1;
            break;
        case 'C': // cpu-target
            jl_options.cpu_target = strdup(optarg);
            break;
        case 'p': // procs
            errno = 0;
            if (!strcmp(optarg,"auto")) {
                jl_options.nprocs = jl_cpu_cores();
            }
            else {
                long nprocs = strtol(optarg, &endptr, 10);
                if (errno != 0 || optarg == endptr || *endptr != 0 || nprocs < 1 || nprocs >= INT_MAX)
                    jl_errorf("julia: -p,--procs=<n> must be an integer >= 1\n");
                jl_options.nprocs = (int)nprocs;
            }
            break;
        case opt_machinefile:
            jl_options.machinefile = strdup(optarg);
            break;
        case opt_color:
            if (!strcmp(optarg,"yes"))
                jl_options.color = JL_OPTIONS_COLOR_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.color = JL_OPTIONS_COLOR_OFF;
            else
                jl_errorf("julia: invalid argument to --color={yes|no} (%s)\n", optarg);
            break;
        case opt_history_file:
            if (!strcmp(optarg,"yes"))
                jl_options.historyfile = JL_OPTIONS_HISTORYFILE_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.historyfile = JL_OPTIONS_HISTORYFILE_OFF;
            else
                jl_errorf("julia: invalid argument to --history-file={yes|no} (%s)\n", optarg);
            break;
        case opt_no_history_file:
            jl_options.historyfile = JL_OPTIONS_HISTORYFILE_OFF;
            break;
        case opt_startup_file:
            if (!strcmp(optarg,"yes"))
                jl_options.startupfile = JL_OPTIONS_STARTUPFILE_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.startupfile = JL_OPTIONS_STARTUPFILE_OFF;
            else
                jl_errorf("julia: invalid argument to --startup-file={yes|no} (%s)\n", optarg);
            break;
        case 'f':
            jl_options.startupfile = JL_OPTIONS_STARTUPFILE_OFF;
            break;
        case 'F':
            jl_options.startupfile = JL_OPTIONS_STARTUPFILE_ON;
            break;
        case opt_compile:
            if (!strcmp(optarg,"yes"))
                jl_options.compile_enabled = JL_OPTIONS_COMPILE_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.compile_enabled = JL_OPTIONS_COMPILE_OFF;
            else if (!strcmp(optarg,"all"))
                jl_options.compile_enabled = JL_OPTIONS_COMPILE_ALL;
            else
                jl_errorf("julia: invalid argument to --compile (%s)\n", optarg);
            break;
        case opt_code_coverage:
            if (optarg != NULL) {
                if (!strcmp(optarg,"user"))
                    codecov = JL_LOG_USER;
                else if (!strcmp(optarg,"all"))
                    codecov = JL_LOG_ALL;
                else if (!strcmp(optarg,"none"))
                    codecov = JL_LOG_NONE;
                break;
            }
            else {
                codecov = JL_LOG_USER;
            }
            break;
        case opt_track_allocation:
            if (optarg != NULL) {
                if (!strcmp(optarg,"user"))
                    malloclog = JL_LOG_USER;
                else if (!strcmp(optarg,"all"))
                    malloclog = JL_LOG_ALL;
                else if (!strcmp(optarg,"none"))
                    malloclog = JL_LOG_NONE;
                break;
            }
            else {
                malloclog = JL_LOG_USER;
            }
            break;
        case 'O': // optimize
            jl_options.opt_level = 1;
            break;
        case 'i': // isinteractive
            jl_options.isinteractive = 1;
            break;
        case opt_check_bounds:
            if (!strcmp(optarg,"yes"))
                jl_options.check_bounds = JL_OPTIONS_CHECK_BOUNDS_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.check_bounds = JL_OPTIONS_CHECK_BOUNDS_OFF;
            else
                jl_errorf("julia: invalid argument to --check-bounds={yes|no} (%s)\n", optarg);
            break;
        case opt_dump_bitcode:
            if (!strcmp(optarg,"yes"))
                jl_options.dumpbitcode = JL_OPTIONS_DUMPBITCODE_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.dumpbitcode = JL_OPTIONS_DUMPBITCODE_OFF;
            break;
        case opt_depwarn:
            if (!strcmp(optarg,"yes"))
                jl_options.depwarn = 1;
            else if (!strcmp(optarg,"no"))
                jl_options.depwarn = 0;
            else
                jl_errorf("julia: invalid argument to --depwarn={yes|no} (%s)\n", optarg);
            break;
        case opt_inline:
            if (!strcmp(optarg,"yes"))
                jl_options.can_inline = 1;
            else if (!strcmp(optarg,"no"))
                jl_options.can_inline = 0;
            else {
                jl_errorf("julia: invalid argument to --inline (%s)\n", optarg);
            }
            break;
        case opt_math_mode:
            if (!strcmp(optarg,"ieee"))
                jl_options.fast_math = JL_OPTIONS_FAST_MATH_OFF;
            else if (!strcmp(optarg,"user"))
                jl_options.fast_math = JL_OPTIONS_FAST_MATH_DEFAULT;
            else
                jl_errorf("julia: invalid argument to --math-mode (%s)\n", optarg);
            break;
        case 'b': // build
            jl_options.build_path = strdup(optarg);
            if (!imagepathspecified)
                jl_options.image_file = NULL;
            break;
        case opt_worker:
            jl_options.worker = 1;
            break;
        case opt_bind_to:
            jl_options.bindto = strdup(optarg);
            break;
        default:
            jl_errorf("julia: unhandled option -- %c\n"
                      "This is a bug, please report it.\n", c);
        }
    }
    jl_options.code_coverage = codecov;
    jl_options.malloc_log = malloclog;
    optind -= skip;
    *argvp += optind;
    *argcp -= optind;
}

static int exec_program(char *program)
{
    int err = 0;
 again: ;
    JL_TRY {
        if (err) {
            jl_value_t *errs = jl_stderr_obj();
            jl_value_t *e = jl_exception_in_transit;
            if (errs != NULL) {
                jl_show(jl_stderr_obj(), e);
            }
            else {
                jl_printf(JL_STDERR, "error during bootstrap:\n");
                jl_static_show(JL_STDERR, e);
                jl_printf(JL_STDERR, "\n");
                jlbacktrace();
            }
            jl_printf(JL_STDERR, "\n");
            JL_EH_POP();
            return 1;
        }
        jl_load(program);
    }
    JL_CATCH {
        err = 1;
        goto again;
    }
    return 0;
}

void jl_lisp_prompt();

#ifndef _WIN32
int jl_repl_raise_sigtstp(void)
{
    return raise(SIGTSTP);
}
#endif

#ifdef JL_GF_PROFILE
static void print_profile(void)
{
    size_t i;
    void **table = jl_base_module->bindings.table;
    for(i=1; i < jl_base_module->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->value != NULL && jl_is_function(b->value) &&
                jl_is_gf(b->value)) {
                jl_printf(JL_STDERR, "%d\t%s\n",
                           jl_gf_mtable(b->value)->ncalls,
                           jl_gf_name(b->value)->name);
            }
        }
    }
}
#endif

static int true_main(int argc, char *argv[])
{
    if (jl_base_module != NULL) {
        jl_array_t *args = (jl_array_t*)jl_get_global(jl_base_module, jl_symbol("ARGS"));
        if (args == NULL) {
            args = jl_alloc_cell_1d(0);
            jl_set_const(jl_base_module, jl_symbol("ARGS"), (jl_value_t*)args);
        }
        assert(jl_array_len(args) == 0);
        jl_array_grow_end(args, argc);
        int i;
        for (i=0; i < argc; i++) {
            jl_value_t *s = (jl_value_t*)jl_cstr_to_string(argv[i]);
            jl_set_typeof(s,jl_utf8_string_type);
            jl_arrayset(args, s, i);
        }
    }

    jl_function_t *start_client = jl_base_module ?
        (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("_start")) : NULL;

    if (start_client) {
        jl_apply(start_client, NULL, 0);
        return 0;
    }

    // run program if specified, otherwise enter REPL
    if (argc > 0) {
        if (strcmp(argv[0], "-")) {
            return exec_program(argv[0]);
        }
    }

    ios_puts("warning: Base._start not defined, falling back to economy mode repl.\n", ios_stdout);
    if (!jl_errorexception_type)
        ios_puts("warning: jl_errorexception_type not defined; any errors will be fatal.\n", ios_stdout);

    while (!ios_eof(ios_stdin)) {
        char *volatile line = NULL;
        JL_TRY {
            ios_puts("\njulia> ", ios_stdout);
            ios_flush(ios_stdout);
            line = ios_readline(ios_stdin);
            jl_value_t *val = jl_eval_string(line);
            if (jl_exception_occurred()) {
                jl_printf(JL_STDERR, "error during run:\n");
                jl_static_show(JL_STDERR, jl_exception_in_transit);
                jl_exception_clear();
            } else if (val) {
                jl_static_show(JL_STDOUT, val);
            }
            jl_printf(JL_STDOUT, "\n");
            free(line);
            line = NULL;
            uv_run(jl_global_event_loop(),UV_RUN_NOWAIT);
        }
        JL_CATCH {
            if (line) {
                free(line);
                line = NULL;
            }
            jl_printf(JL_STDERR, "\nparser error:\n");
            jl_static_show(JL_STDERR, jl_exception_in_transit);
            jl_printf(JL_STDERR, "\n");
            jlbacktrace();
        }
    }
    return 0;
}

DLLEXPORT extern void julia_save();

#ifndef _OS_WINDOWS_
int main(int argc, char *argv[])
{
    uv_setup_args(argc, argv); // no-op on Windows
#else
int wmain(int argc, wchar_t *argv[], wchar_t *envp[])
{
    int i;
    for (i=0; i<argc; i++) { // write the command line to UTF8
        wchar_t *warg = argv[i];
        size_t len = WideCharToMultiByte(CP_UTF8, 0, warg, -1, NULL, 0, NULL, NULL);
        if (!len) return 1;
        char *arg = (char*)alloca(len);
        if (!WideCharToMultiByte(CP_UTF8, 0, warg, -1, arg, len, NULL, NULL)) return 1;
        argv[i] = (wchar_t*)arg;
    }
#endif
    libsupport_init();
    parse_opts(&argc, (char***)&argv);
    if (lisp_prompt) {
        jl_lisp_prompt();
        return 0;
    }
    julia_init(imagepathspecified ? JL_IMAGE_CWD : JL_IMAGE_JULIA_HOME);
    int ret = true_main(argc, (char**)argv);
    jl_atexit_hook();
    julia_save();
    return ret;
}

#ifdef __cplusplus
}
#endif
