// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <limits.h>

#include "julia.h"

#ifndef _MSC_VER
#include <unistd.h>
#include <getopt.h>
#else
#include "getopt.h"
#endif
#include "julia_assert.h"

#ifdef _OS_WINDOWS_
char *shlib_ext = ".dll";
#elif defined(_OS_DARWIN_)
char *shlib_ext = ".dylib";
#else
char *shlib_ext = ".so";
#endif

static const char system_image_path[256] = "\0" JL_SYSTEM_IMAGE_PATH;
JL_DLLEXPORT const char *jl_get_default_sysimg_path(void)
{
    return &system_image_path[1];
}


jl_options_t jl_options = { 0,    // quiet
                            -1,   // banner
                            NULL, // julia_bindir
                            NULL, // julia_bin
                            NULL, // cmds
                            NULL, // image_file (will be filled in below)
                            NULL, // cpu_target ("native", "core2", etc...)
                            0,    // nprocs
                            NULL, // machine_file
                            NULL, // project
                            0,    // isinteractive
                            0,    // color
                            JL_OPTIONS_HISTORYFILE_ON, // history file
                            0,    // startup file
                            JL_OPTIONS_COMPILE_DEFAULT, // compile_enabled
                            0,    // code_coverage
                            0,    // malloc_log
                            2,    // opt_level
#ifdef JL_DEBUG_BUILD
                            2,    // debug_level [debug build]
#else
                            1,    // debug_level [release build]
#endif
                            JL_OPTIONS_CHECK_BOUNDS_DEFAULT, // check_bounds
                            JL_OPTIONS_DEPWARN_ON,    // deprecation warning
                            0,    // method overwrite warning
                            1,    // can_inline
                            JL_OPTIONS_POLLY_ON, // polly
                            JL_OPTIONS_FAST_MATH_DEFAULT,
                            0,    // worker
                            NULL, // cookie
                            JL_OPTIONS_HANDLE_SIGNALS_ON,
                            JL_OPTIONS_USE_SYSIMAGE_NATIVE_CODE_YES,
                            JL_OPTIONS_USE_COMPILED_MODULES_YES,
                            NULL, // bind-to
                            NULL, // output-bc
                            NULL, // output-unopt-bc
                            NULL, // output-jit-bc
                            NULL, // output-o
                            NULL, // output-ji
                            0, // incremental
                            0 // image_file_specified
};

static const char usage[] = "julia [switches] -- [programfile] [args...]\n";
static const char opts[]  =
    " -v, --version             Display version information\n"
    " -h, --help                Print this message\n\n"

    // startup options
    " -J, --sysimage <file>     Start up with the given system image file\n"
    " -H, --home <dir>          Set location of `julia` executable\n"
    " --startup-file={yes|no}   Load `~/.julia/config/startup.jl`\n"
    " --handle-signals={yes|no} Enable or disable Julia's default signal handlers\n"
    " --sysimage-native-code={yes|no}\n"
    "                           Use native code from system image if available\n"
    " --compiled-modules={yes|no}\n"
    "                           Enable or disable incremental precompilation of modules\n\n"

    // actions
    " -e, --eval <expr>         Evaluate <expr>\n"
    " -E, --print <expr>        Evaluate <expr> and display the result\n"
    " -L, --load <file>         Load <file> immediately on all processors\n\n"

    // parallel options
    " -p, --procs {N|auto}      Integer value N launches N additional local worker processes\n"
    "                           \"auto\" launches as many workers as the number of local cores\n"
    " --machine-file <file>     Run processes on hosts listed in <file>\n\n"

    // interactive options
    " -i                        Interactive mode; REPL runs and isinteractive() is true\n"
    " -q, --quiet               Quiet startup: no banner, suppress REPL warnings\n"
    " --banner={yes|no|auto}    Enable or disable startup banner\n"
    " --color={yes|no|auto}     Enable or disable color text\n"
    " --history-file={yes|no}   Load or save history\n\n"

    // error and warning options
    " --depwarn={yes|no|error}  Enable or disable syntax and method deprecation warnings (\"error\" turns warnings into errors)\n"
    " --warn-overwrite={yes|no} Enable or disable method overwrite warnings\n\n"

    // code generation options
    //" --compile={yes|no|all|min}Enable or disable JIT compiler, or request exhaustive compilation\n"
    " -C, --cpu-target <target> Limit usage of cpu features up to <target>; set to \"help\" to see the available options\n"
    " -O, --optimize={0,1,2,3}  Set the optimization level (default level is 2 if unspecified or 3 if used without a level)\n"
    " -g, -g <level>            Enable / Set the level of debug info generation"
#ifdef JL_DEBUG_BUILD
        " (default level for julia-debug is 2 if unspecified or if used without a level)\n"
#else
        " (default level is 1 if unspecified or 2 if used without a level)\n"
#endif
    " --inline={yes|no}         Control whether inlining is permitted, including overriding @inline declarations\n"
    " --check-bounds={yes|no}   Emit bounds checks always or never (ignoring declarations)\n"
#ifdef USE_POLLY
    " --polly={yes|no}          Enable or disable the polyhedral optimizer Polly (overrides @polly declaration)\n"
#endif
    " --math-mode={ieee,fast}   Disallow or enable unsafe floating point optimizations (overrides @fastmath declaration)\n\n"

    // instrumentation options
    " --code-coverage={none|user|all}, --code-coverage\n"
    "                           Count executions of source lines (omitting setting is equivalent to \"user\")\n"
    " --track-allocation={none|user|all}, --track-allocation\n"
    "                           Count bytes allocated by each source line\n\n"

    // compiler output options
    //" --output-o name           Generate an object file (including system image data)\n"
    //" --output-ji name          Generate a system image data file (.ji)\n"
// These are for compiler debugging purposes only and should not be otherwise
// used, so don't show them here. See the devdocs for tips on using these
// options for debugging the compiler.
//  " --output-unopt-bc name    Generate unoptimized LLVM bitcode (.bc)\n"
//  " --output-jit-bc name      Dump all IR generated by the frontend (not including system image)\n"
    //" --output-bc name          Generate LLVM bitcode (.bc)\n"
    //" --output-incremental=no   Generate an incremental output file (rather than complete)\n\n"
;

JL_DLLEXPORT void jl_parse_opts(int *argcp, char ***argvp)
{
    enum { opt_machinefile = 300,
           opt_color,
           opt_history_file,
           opt_startup_file,
           opt_compile,
           opt_code_coverage,
           opt_track_allocation,
           opt_check_bounds,
           opt_output_jit_bc,
           opt_output_unopt_bc,
           opt_output_bc,
           opt_depwarn,
           opt_warn_overwrite,
           opt_inline,
           opt_polly,
           opt_math_mode,
           opt_worker,
           opt_bind_to,
           opt_handle_signals,
           opt_output_o,
           opt_output_ji,
           opt_use_precompiled,
           opt_use_compilecache,
           opt_incremental,
           opt_banner,
           opt_sysimage_native_code,
           opt_compiled_modules,
           opt_machine_file,
           opt_project,
    };
    static const char* const shortopts = "+vhqH:e:E:L:J:C:ip:O:g:";
    static const struct option longopts[] = {
        // exposed command line options
        // NOTE: This set of required arguments need to be kept in sync
        // with the required arguments defined in base/client.jl `process_options()`
        { "version",         no_argument,       0, 'v' },
        { "help",            no_argument,       0, 'h' },
        { "quiet",           no_argument,       0, 'q' },
        { "banner",          required_argument, 0, opt_banner },
        { "home",            required_argument, 0, 'H' },
        { "eval",            required_argument, 0, 'e' },
        { "print",           required_argument, 0, 'E' },
        { "load",            required_argument, 0, 'L' },
        { "sysimage",        required_argument, 0, 'J' },
        { "precompiled",     required_argument, 0, opt_use_precompiled },   // deprecated
        { "sysimage-native-code", required_argument, 0, opt_sysimage_native_code },
        { "compilecache",    required_argument, 0, opt_use_compilecache },  // deprecated
        { "compiled-modules",    required_argument, 0, opt_compiled_modules },
        { "cpu-target",      required_argument, 0, 'C' },
        { "procs",           required_argument, 0, 'p' },
        { "machinefile",     required_argument, 0, opt_machinefile },   // deprecated
        { "machine-file",    required_argument, 0, opt_machine_file },
        { "project",         optional_argument, 0, opt_project },
        { "color",           required_argument, 0, opt_color },
        { "history-file",    required_argument, 0, opt_history_file },
        { "startup-file",    required_argument, 0, opt_startup_file },
        { "compile",         required_argument, 0, opt_compile },
        { "code-coverage",   optional_argument, 0, opt_code_coverage },
        { "track-allocation",optional_argument, 0, opt_track_allocation },
        { "optimize",        optional_argument, 0, 'O' },
        { "check-bounds",    required_argument, 0, opt_check_bounds },
        { "output-bc",       required_argument, 0, opt_output_bc },
        { "output-unopt-bc", required_argument, 0, opt_output_unopt_bc },
        { "output-jit-bc",   required_argument, 0, opt_output_jit_bc },
        { "output-o",        required_argument, 0, opt_output_o },
        { "output-ji",       required_argument, 0, opt_output_ji },
        { "output-incremental",required_argument, 0, opt_incremental },
        { "depwarn",         required_argument, 0, opt_depwarn },
        { "warn-overwrite",  required_argument, 0, opt_warn_overwrite },
        { "inline",          required_argument, 0, opt_inline },
        { "polly",           required_argument, 0, opt_polly },
        { "math-mode",       required_argument, 0, opt_math_mode },
        { "handle-signals",  required_argument, 0, opt_handle_signals },
        // hidden command line options
        { "worker",          optional_argument, 0, opt_worker },
        { "bind-to",         required_argument, 0, opt_bind_to },
        { "lisp",            no_argument,       0, 1 },
        { 0, 0, 0, 0 }
    };

    // If CPUID specific binaries are enabled, this varies between runs, so initialize
    // it here, rather than as part of the static initialization above.
    jl_options.image_file = jl_get_default_sysimg_path();
    jl_options.cmds = NULL;

    int ncmds = 0;
    const char **cmds = NULL;
    int codecov = JL_LOG_NONE;
    int malloclog = JL_LOG_NONE;
    // getopt handles argument parsing up to -- delineator
    int argc = *argcp;
    char **argv = *argvp;
    if (argc > 0) {
        for (int i = 0; i < argc; i++) {
            if (!strcmp(argv[i], "--")) {
                argc = i;
                break;
            }
        }
    }
    char *endptr;
    opterr = 0; // suppress getopt warning messages
    while (1) {
        int lastind = optind;
        int c = getopt_long(argc, argv, shortopts, longopts, 0);
        if (c == -1) break;
restart_switch:
        switch (c) {
        case 0:
            break;
        case 1:
            jl_errorf("--lisp must be specified as the first argument");
            break;
        case '?':
        case ':':
            if (optopt) {
                if (optopt == 'g') {
                    c = 'g';
                    goto restart_switch;
                }
                for (const struct option *o = longopts; o->val; o++) {
                    if (optopt == o->val) {
                        if (o->has_arg == optional_argument) {
                            c = o->val;
                            goto restart_switch;
                        }
                        else if (strchr(shortopts, o->val)) {
                            jl_errorf("option `-%c/--%s` is missing an argument", o->val, o->name);
                        }
                        else {
                            jl_errorf("option `--%s` is missing an argument", o->name);
                        }
                    }
                }
                jl_errorf("unknown option `-%c`", optopt);
            }
            else {
                jl_errorf("unknown option `%s`", argv[lastind]);
            }
            break;
        case 'v': // version
            jl_printf(JL_STDOUT, "julia version %s\n", JULIA_VERSION_STRING);
            jl_exit(0);
        case 'h': // help
            jl_printf(JL_STDOUT, "%s%s", usage, opts);
            jl_exit(0);
        case 'g': // debug info
            if (optarg != NULL) {
                if (!strcmp(optarg,"0"))
                    jl_options.debug_level = 0;
                else if (!strcmp(optarg,"1"))
                    jl_options.debug_level = 1;
                else if (!strcmp(optarg,"2"))
                    jl_options.debug_level = 2;
                else
                    jl_errorf("julia: invalid argument to -g (%s)", optarg);
                break;
            }
            else {
                jl_options.debug_level = 2;
            }
            break;
        case 'H': // home
            jl_options.julia_bindir = strdup(optarg);
            if (!jl_options.julia_bindir)
                jl_errorf("fatal error: failed to allocate memory: %s", strerror(errno));
            break;
        case 'e': // eval
        case 'E': // print
        case 'L': // load
        {
            size_t sz = strlen(optarg) + 1;
            char *arg = (char*)malloc(sz + 1);
            const char **newcmds;
            if (!arg)
                jl_errorf("fatal error: failed to allocate memory: %s", strerror(errno));
            arg[0] = c;
            memcpy(arg + 1, optarg, sz);
            newcmds = (const char**)realloc(cmds, (ncmds + 2) * sizeof(char*));
            if (!newcmds) {
                free(cmds);
                jl_errorf("fatal error: failed to allocate memory: %s", strerror(errno));
            }
            cmds = newcmds;
            cmds[ncmds] = arg;
            ncmds++;
            cmds[ncmds] = 0;
            jl_options.cmds = cmds;
            break;
        }
        case 'J': // sysimage
            jl_options.image_file = strdup(optarg);
            if (!jl_options.image_file)
                jl_errorf("fatal error: failed to allocate memory: %s", strerror(errno));
            jl_options.image_file_specified = 1;
            break;
        case 'q': // quiet
            jl_options.quiet = 1;
            if (jl_options.banner < 0)
                jl_options.banner = 0;
            break;
        case opt_banner: // banner
            if (!strcmp(optarg, "yes"))
                jl_options.banner = 1;
            else if (!strcmp(optarg, "no"))
                jl_options.banner = 0;
            else if (!strcmp(optarg, "auto"))
                jl_options.banner = -1;
            else
                jl_errorf("julia: invalid argument to --banner={yes|no|auto} (%s)", optarg);
            break;
        case opt_use_precompiled:
            jl_printf(JL_STDOUT, "WARNING: julia --precompiled option is deprecated, use --sysimage-native-code instead.\n");
            // fall through
        case opt_sysimage_native_code:
            if (!strcmp(optarg,"yes"))
                jl_options.use_sysimage_native_code = JL_OPTIONS_USE_SYSIMAGE_NATIVE_CODE_YES;
            else if (!strcmp(optarg,"no"))
                jl_options.use_sysimage_native_code = JL_OPTIONS_USE_SYSIMAGE_NATIVE_CODE_NO;
            else
                jl_errorf("julia: invalid argument to --sysimage-native-code={yes|no} (%s)", optarg);
            break;
        case opt_use_compilecache:
            jl_printf(JL_STDOUT, "WARNING: julia --compilecache option is deprecated, use --compiled-modules instead.\n");
            // fall through
        case opt_compiled_modules:
            if (!strcmp(optarg,"yes"))
                jl_options.use_compiled_modules = JL_OPTIONS_USE_COMPILED_MODULES_YES;
            else if (!strcmp(optarg,"no"))
                jl_options.use_compiled_modules = JL_OPTIONS_USE_COMPILED_MODULES_NO;
            else
                jl_errorf("julia: invalid argument to --compiled-modules={yes|no} (%s)", optarg);
            break;
        case 'C': // cpu-target
            jl_options.cpu_target = strdup(optarg);
            if (!jl_options.cpu_target)
                jl_error("julia: failed to allocate memory");
            break;
        case 'p': // procs
            errno = 0;
            if (!strcmp(optarg,"auto")) {
                jl_options.nprocs = jl_cpu_cores();
            }
            else {
                long nprocs = strtol(optarg, &endptr, 10);
                if (errno != 0 || optarg == endptr || *endptr != 0 || nprocs < 1 || nprocs >= INT_MAX)
                    jl_errorf("julia: -p,--procs=<n> must be an integer >= 1");
                jl_options.nprocs = (int)nprocs;
            }
            break;
        case opt_machinefile:
            jl_printf(JL_STDOUT, "WARNING: julia --machinefile option is deprecated, use --machine-file instead.\n");
            // fall through
        case opt_machine_file:
            jl_options.machine_file = strdup(optarg);
            if (!jl_options.machine_file)
                jl_error("julia: failed to allocate memory");
            break;
        case opt_project:
            jl_options.project = optarg ? strdup(optarg) : "@.";
            break;
        case opt_color:
            if (!strcmp(optarg, "yes"))
                jl_options.color = JL_OPTIONS_COLOR_ON;
            else if (!strcmp(optarg, "no"))
                jl_options.color = JL_OPTIONS_COLOR_OFF;
            else if (!strcmp(optarg, "auto"))
                jl_options.color = JL_OPTIONS_COLOR_AUTO;
            else
                jl_errorf("julia: invalid argument to --color={yes|no|auto} (%s)", optarg);
            break;
        case opt_history_file:
            if (!strcmp(optarg,"yes"))
                jl_options.historyfile = JL_OPTIONS_HISTORYFILE_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.historyfile = JL_OPTIONS_HISTORYFILE_OFF;
            else
                jl_errorf("julia: invalid argument to --history-file={yes|no} (%s)", optarg);
            break;
        case opt_startup_file:
            if (!strcmp(optarg,"yes"))
                jl_options.startupfile = JL_OPTIONS_STARTUPFILE_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.startupfile = JL_OPTIONS_STARTUPFILE_OFF;
            else
                jl_errorf("julia: invalid argument to --startup-file={yes|no} (%s)", optarg);
            break;
        case opt_compile:
            if (!strcmp(optarg,"yes"))
                jl_options.compile_enabled = JL_OPTIONS_COMPILE_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.compile_enabled = JL_OPTIONS_COMPILE_OFF;
            else if (!strcmp(optarg,"all"))
                jl_options.compile_enabled = JL_OPTIONS_COMPILE_ALL;
            else if (!strcmp(optarg,"min"))
                jl_options.compile_enabled = JL_OPTIONS_COMPILE_MIN;
            else
                jl_errorf("julia: invalid argument to --compile (%s)", optarg);
            break;
        case opt_code_coverage:
            if (optarg != NULL) {
                if (!strcmp(optarg,"user"))
                    codecov = JL_LOG_USER;
                else if (!strcmp(optarg,"all"))
                    codecov = JL_LOG_ALL;
                else if (!strcmp(optarg,"none"))
                    codecov = JL_LOG_NONE;
                else
                    jl_errorf("julia: invalid argument to --code-coverage (%s)", optarg);
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
                else
                    jl_errorf("julia: invalid argument to --track-allocation (%s)", optarg);
                break;
            }
            else {
                malloclog = JL_LOG_USER;
            }
            break;
        case 'O': // optimize
            if (optarg != NULL) {
                if (!strcmp(optarg,"0"))
                    jl_options.opt_level = 0;
                else if (!strcmp(optarg,"1"))
                    jl_options.opt_level = 1;
                else if (!strcmp(optarg,"2"))
                    jl_options.opt_level = 2;
                else if (!strcmp(optarg,"3"))
                    jl_options.opt_level = 3;
                else
                    jl_errorf("julia: invalid argument to -O (%s)", optarg);
                break;
            }
            else {
                jl_options.opt_level = 3;
            }
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
                jl_errorf("julia: invalid argument to --check-bounds={yes|no} (%s)", optarg);
            break;
        case opt_output_bc:
            jl_options.outputbc = optarg;
            if (!jl_options.image_file_specified) jl_options.image_file = NULL;
            break;
        case opt_output_jit_bc:
            jl_options.outputjitbc = optarg;
            break;
        case opt_output_unopt_bc:
            jl_options.outputunoptbc = optarg;
            if (!jl_options.image_file_specified) jl_options.image_file = NULL;
            break;
        case opt_output_o:
            jl_options.outputo = optarg;
            if (!jl_options.image_file_specified) jl_options.image_file = NULL;
            break;
        case opt_output_ji:
            jl_options.outputji = optarg;
            if (!jl_options.image_file_specified) jl_options.image_file = NULL;
            break;
        case opt_incremental:
            if (!strcmp(optarg,"yes"))
                jl_options.incremental = 1;
            else if (!strcmp(optarg,"no"))
                jl_options.incremental = 0;
            else
                jl_errorf("julia: invalid argument to --output-incremental={yes|no} (%s)", optarg);
            break;
        case opt_depwarn:
            if (!strcmp(optarg,"yes"))
                jl_options.depwarn = JL_OPTIONS_DEPWARN_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.depwarn = JL_OPTIONS_DEPWARN_OFF;
            else if (!strcmp(optarg,"error"))
                jl_options.depwarn = JL_OPTIONS_DEPWARN_ERROR;
            else
                jl_errorf("julia: invalid argument to --depwarn={yes|no|error} (%s)", optarg);
            break;
        case opt_warn_overwrite:
            if (!strcmp(optarg,"yes"))
                jl_options.warn_overwrite = JL_OPTIONS_WARN_OVERWRITE_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.warn_overwrite = JL_OPTIONS_WARN_OVERWRITE_OFF;
            else
                jl_errorf("julia: invalid argument to --warn-overwrite={yes|no|} (%s)", optarg);
            break;
        case opt_inline:
            if (!strcmp(optarg,"yes"))
                jl_options.can_inline = 1;
            else if (!strcmp(optarg,"no"))
                jl_options.can_inline = 0;
            else {
                jl_errorf("julia: invalid argument to --inline (%s)", optarg);
            }
            break;
       case opt_polly:
            if (!strcmp(optarg,"yes"))
                jl_options.polly = JL_OPTIONS_POLLY_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.polly = JL_OPTIONS_POLLY_OFF;
            else {
                jl_errorf("julia: invalid argument to --polly (%s)", optarg);
            }
            break;
        case opt_math_mode:
            if (!strcmp(optarg,"ieee"))
                jl_options.fast_math = JL_OPTIONS_FAST_MATH_OFF;
            else if (!strcmp(optarg,"fast"))
                jl_options.fast_math = JL_OPTIONS_FAST_MATH_ON;
            else if (!strcmp(optarg,"user"))
                jl_options.fast_math = JL_OPTIONS_FAST_MATH_DEFAULT;
            else
                jl_errorf("julia: invalid argument to --math-mode (%s)", optarg);
            break;
        case opt_worker:
            jl_options.worker = 1;
            if (optarg != NULL) {
                jl_options.cookie = strdup(optarg);
                if (!jl_options.cookie)
                    jl_error("julia: failed to allocate memory");
            }
            break;
        case opt_bind_to:
            jl_options.bindto = strdup(optarg);
            if (!jl_options.bindto)
                jl_error("julia: failed to allocate memory");
            break;
        case opt_handle_signals:
            if (!strcmp(optarg,"yes"))
                jl_options.handle_signals = JL_OPTIONS_HANDLE_SIGNALS_ON;
            else if (!strcmp(optarg,"no"))
                jl_options.handle_signals = JL_OPTIONS_HANDLE_SIGNALS_OFF;
            else
                jl_errorf("julia: invalid argument to --handle-signals (%s)", optarg);
            break;
        default:
            jl_errorf("julia: unhandled option -- %c\n"
                      "This is a bug, please report it.", c);
        }
    }
    jl_options.code_coverage = codecov;
    jl_options.malloc_log = malloclog;
    *argvp += optind;
    *argcp -= optind;
}

JL_DLLEXPORT void jl_set_ARGS(int argc, char **argv)
{
    if (jl_core_module != NULL) {
        jl_array_t *args = (jl_array_t*)jl_get_global(jl_core_module, jl_symbol("ARGS"));
        if (args == NULL) {
            args = jl_alloc_vec_any(0);
            JL_GC_PUSH1(&args);
            jl_set_const(jl_core_module, jl_symbol("ARGS"), (jl_value_t*)args);
            JL_GC_POP();
        }
        assert(jl_array_len(args) == 0);
        jl_array_grow_end(args, argc);
        int i;
        for (i=0; i < argc; i++) {
            jl_value_t *s = (jl_value_t*)jl_cstr_to_string(argv[i]);
            jl_arrayset(args, s, i);
        }
    }
}

JL_DLLEXPORT ssize_t jl_sizeof_jl_options(void)
{
    return sizeof(jl_options_t);
}
