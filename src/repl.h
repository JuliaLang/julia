#ifndef JL_REPL_H

#define JL_REPL_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <signal.h>
#include <assert.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <libgen.h>
#include <getopt.h>

#ifdef BOEHM_GC
#include <gc.h>
#endif

#if defined(USE_READLINE)
#include <readline/readline.h>
#include <readline/history.h>
#elif defined(USE_EDITLINE)
#include <editline/readline.h>
#else
#include <ctype.h>
#endif

#include "llt.h"
#include "julia.h"

extern int no_readline;
extern char jl_prompt_color[];
extern int prompt_length;
extern int have_color;
extern int tab_width;
extern jl_value_t *rl_ast;
extern char *jl_answer_color;

extern void init_repl_environment();
extern void exit_repl_environment();
extern void read_expr(char *prompt);
DLLEXPORT extern void jl_input_line_callback(char *input);
extern void handle_input(jl_value_t *ast, int end, int show_value);
extern int ends_with_semicolon(const char *input);
extern char *ios_readline(ios_t *s);

#endif // JL_REPL_H
