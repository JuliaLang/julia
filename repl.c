/*
  repl.c
  system startup, main(), and console interaction
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <libgen.h>
#ifndef NO_BOEHM_GC
#include <gc.h>
#endif
#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif
#include "llt.h"
#include "julia.h"

static char jl_banner_plain[] =
    "               _      \n"
    "   _       _ _(_)_     |\n"
    "  (_)     | (_) (_)    |  pre-release version\n"
    "   _ _   _| |_  __ _   |\n"
    "  | | | | | | |/ _` |  |\n"
    "  | | |_| | | | (_| |  |  \302\2512009-2010 contributors\n"
    " _/ |\\__'_|_|_|\\__'_|  |  \n"
    "|__/                   |\n\n";

static char jl_banner_color[] =
    "\033[1m               \033[32m_\033[37m      \n"
    "   \033[37m_\033[37m       _ \033[31m_\033[32m(_)\033[35m_\033[37m     |\n"
    "  \033[37m(_)\033[37m     | \033[31m(_) \033[35m(_)\033[37m    |  pre-release version\n"
    "   _ _   _| |_  __ _   |\n"
    "  | | | | | | |/ _` |  |\n"
    "  | | |_| | | | (_| |  |  \302\2512009-2010 contributors\n"
    " _/ |\\__'_|_|_|\\__'_|  |  \n"
    "|__/                   |\033[0m\n\n";

static char jl_prompt_plain[] = "julia> ";
static char jl_prompt_color[] = "\001\033[1m\033[32m\002julia> \001\033[37m\002";
static char jl_answer_color[] = "\033[0m\033[37m";
static char jl_color_normal[] = "\033[0m\033[37m";

static char jl_history_file[] = ".julia_history";

char *julia_home; // load is relative to here

void julia_init()
{
    llt_init();
    jl_init_frontend();
    jl_init_types();
    jl_init_modules();
    jl_init_builtins();
    jl_init_codegen();
    jl_init_intrinsic_functions();
}

static int detect_color()
{
#ifdef WIN32
    return 0;
#else
    int tput = system("tput setaf 0 >&/dev/null");
    if (tput == 0) return 1;
    if (tput == 1) return 0;
    char *term = getenv("TERM");
    if (term == NULL) return 0;
    return (!strcmp(term,"xterm") || !strcmp(term,"xterm-color"));
#endif
}

#ifndef USE_READLINE
static char *ios_readline(ios_t *s)
{
    ios_t dest;
    ios_mem(&dest, 0);
    ios_copyuntil(&dest, s, '\n');
    size_t n;
    return ios_takebuf(&dest, &n);
}
#endif

JL_CALLABLE(jl_f_new_closure);

jl_value_t *jl_toplevel_eval(jl_value_t *ast)
{
    //jl_print(ast);
    //ios_printf(ios_stdout, "\n");
    // ast is of the form (quote <lambda-info>)
    jl_value_t *args[2];
    assert(jl_is_expr(ast));
    jl_lambda_info_t *li =
        ((jl_lambda_info_t**)((jl_expr_t*)ast)->args->data)[0];
    assert(jl_typeof(li) == (jl_type_t*)jl_lambda_info_type);
    args[0] = (jl_value_t*)li;
    args[1] = (jl_value_t*)jl_null;
    jl_value_t *thunk = jl_f_new_closure(NULL, args, 2);
    return jl_apply((jl_function_t*)thunk, NULL, 0);
}

static int have_color;

jmp_buf ExceptionHandler;
jmp_buf *CurrentExceptionHandler = &ExceptionHandler;

int main(int argc, char *argv[])
{
    julia_home = getenv("JULIA_HOME");
    if (!julia_home) {
        char *julia_path = (char*)malloc(PATH_MAX);
        get_exename(julia_path, PATH_MAX);
        julia_home = strdup(dirname(julia_path));
        free(julia_path);
    }

    julia_init();

    have_color = detect_color();
    char *banner = have_color ? jl_banner_color : jl_banner_plain;
    char *prompt = have_color ? jl_prompt_color : jl_prompt_plain;

    int print_banner = 1;
    if (argc > 1) {
        if (!strncmp(argv[1], "-q", 2)) {
            print_banner = 0;
        }
    }

    if (!setjmp(ExceptionHandler)) {
        jl_load("start.j");
    }
    else {
        ios_printf(ios_stderr, "error during startup.\n");
        return 1;
    }

    if (print_banner)
        ios_printf(ios_stdout, "%s", banner);

#ifdef USE_READLINE
    using_history();
    struct stat stat_info;
    if (!stat(jl_history_file, &stat_info)) {
        read_history(jl_history_file);
    } else if (errno == ENOENT) {
        write_history(jl_history_file);
    } else {
        jl_errorf("history file error: %s", strerror(errno));
    }
#endif

    while (1) {
        ios_flush(ios_stdout);

#ifdef USE_READLINE
        char *input = readline(prompt);
#else
        char *input = ios_printf(ios_stdout, prompt);
#endif

        ios_flush(ios_stdout);
#ifdef USE_READLINE
        if (input && *input) add_history(input);
#else
        input = ios_readline(ios_stdin);
#endif
        ios_purge(ios_stdin);

        if (!input || ios_eof(ios_stdin) || !strcmp(input, "Quit\n")) {
            ios_printf(ios_stdout, "\n");
            break;
        }
#ifdef USE_READLINE
        append_history(1, jl_history_file);
#endif

        if (have_color) {
            ios_printf(ios_stdout, jl_answer_color);
            ios_flush(ios_stdout);
        }

        // process input
        if (!setjmp(ExceptionHandler)) {
            jl_value_t *ast = jl_parse_input_line(input);
            int print_value = 1;
            char *p = strrchr(input, ';');
            if (p++) {
                while (isspace(*p)) p++;
                if (*p == '\0' || *p == '#')
                    print_value = 0;
            }
            if (ast != NULL) {
                jl_value_t *value = jl_toplevel_eval(ast);
                if (print_value) {
                    jl_print(value);
                    ios_printf(ios_stdout, "\n");
                }
            }
        }

        ios_printf(ios_stdout, "\n");
#ifdef USE_READLINE
        // readline allocates with system malloc
        if (input) free(input);
#endif
    }
    if (have_color) {
        ios_printf(ios_stdout, jl_color_normal);
        ios_flush(ios_stdout);
    }

    return 0;
}
