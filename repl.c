/*
  repl.c
  system startup, main(), and console interaction
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <sys/types.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#ifndef NO_BOEHM_GC
#include <gc.h>
#endif
#ifdef USE_READLINE
#include <readline/readline.h>
#endif
#include "llt.h"
#include "julia.h"

static char jl_banner_plain[] =
    "               _      \n"
    "   _       _ _(_)_     |\n"
    "  (_)     | (_) (_)    |  pre-release version\n"
    "   _ _   _| |_  __ _   |\n"
    "  | | | | | | |/ _` |  |\n"
    "  | | |_| | | | (_| |  |  \302\2512010 contributors\n"
    " _/ |\\__'_|_|_|\\__'_|  |  \n"
    "|__/                   |\n\n";

static char jl_banner_color[] =
    "\033[1m               \033[32m_\033[37m      \n"
    "   \033[36m_\033[37m       _ \033[31m_\033[32m(_)\033[35m_\033[37m     |\n"
    "  \033[36m(_)\033[37m     | \033[31m(_) \033[35m(_)\033[37m    |  pre-release version\n"
    "   _ _   _| |_  __ _   |\n"
    "  | | | | | | |/ _` |  |\n"
    "  | | |_| | | | (_| |  |  \302\2512010 contributors\n"
    " _/ |\\__'_|_|_|\\__'_|  |  \n"
    "|__/                   |\033[0m\n\n";

void julia_init()
{
    llt_init();
    jl_init_frontend();
    jl_init_types();
    jl_init_builtins();
    jl_init_modules();
    jl_init_codegen();
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

char *ios_readline(ios_t *s)
{
    ios_t dest;
    ios_mem(&dest, 0);
    ios_copyuntil(&dest, s, '\n');
    size_t n;
    return ios_takebuf(&dest, &n);
}

JL_CALLABLE(jl_f_new_closure);

jl_value_t *jl_toplevel_eval(jl_value_t *ast)
{
    // ast is of the form (quote <lambda-info>)
    jl_value_t *args[2];
    assert(jl_is_expr(ast));
    jl_lambda_info_t *li =
        ((jl_lambda_info_t**)((jl_expr_t*)ast)->args->data)[0];
    assert(jl_typeof(li) == jl_lambda_info_type);
    args[0] = (jl_value_t*)li;
    args[1] = (jl_value_t*)jl_null;
    jl_value_t *thunk = jl_f_new_closure(NULL, args, 2);
    return jl_apply((jl_function_t*)thunk, NULL, 0);
}

static int have_color;

int main(int argc, char *argv[])
{
    julia_init();
    
    have_color = detect_color();
    int print_banner = 1;

    if (argc > 1) {
        if (!strncmp(argv[1], "-q", 2)) {
            print_banner = 0;
        }
    }

    if (print_banner) {
        char *banner = have_color ? jl_banner_color : jl_banner_plain;
        ios_printf(ios_stdout, "%s", banner);
    }

    while (1) {
	char *input;

        ios_flush(ios_stdout);

        if (have_color) {
#ifdef USE_READLINE
            input = readline ("\033[32mjulia> \033[0m");
#else
            input = ios_printf(ios_stdout, "\033[32mjulia> \033[0m");
#endif
        } else {
#ifdef USE_READLINE
            input = readline ("julia> ");
#else
            ios_printf(ios_stdout, "julia> ");
#endif
        }

        ios_flush(ios_stdout);
#ifdef USE_READLINE
        if (input && *input) add_history(input);
#else
        input = ios_readline(ios_stdin);
#endif
        ios_purge(ios_stdin);

        if (!input) return 0;

        if (ios_eof(ios_stdin) || !strcmp(input, "Quit\n")) {
            ios_printf(ios_stdout, "\n");
            break;
        }
        
        if (have_color)
            ios_printf(ios_stdout, "\033[1m\033[36m");
        
        // process input
        jl_value_t *ast = jl_parse_input_line(input);
        if (ast != NULL) {
            jl_print(ast);
            ios_printf(ios_stdout, "\n");
            jl_print(jl_toplevel_eval(ast));
        }
        
        ios_printf(ios_stdout, "\n");
        if (input) free(input);
    }
    
    return 0;
}
