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
#include <ctype.h>

#include "llt.h"
#include "julia.h"

extern char jl_prompt_color[];
extern int prompt_length;
extern int have_color;
extern int tab_width;
extern jl_value_t *rl_ast;
extern char *jl_answer_color;
extern char *prompt_string;
extern char *julia_home;
extern int jl_have_event_loop;

extern void init_repl_environment();
extern void exit_repl_environment();
extern void read_expr(char *prompt);
DLLEXPORT extern void jl_input_line_callback(char *input);
extern void handle_input(jl_value_t *ast, int end, int show_value);
extern int ends_with_semicolon(const char *input);
extern char *ios_readline(ios_t *s);
extern void repl_callback_enable();
extern void repl_callback_disable();
extern void repl_stdin_callback();
extern void repl_print_prompt();


#ifdef CLOUD_REPL
extern char *repl_result;
#endif

#endif // JL_REPL_H
