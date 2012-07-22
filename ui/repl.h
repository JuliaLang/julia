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

#include "julia.h"

extern int tab_width;
extern DLLEXPORT char *julia_home;

extern void init_repl_environment(int argc, char *argv[]);
extern char *read_expr(char *prompt);
extern void handle_input(jl_value_t *ast, int end, int show_value);
extern int ends_with_semicolon(const char *input);
extern char *ios_readline(ios_t *s);
DLLEXPORT extern void repl_callback_enable();
DLLEXPORT extern void jl_enable_color();
DLLEXPORT extern void jl_stdin_callback();
DLLEXPORT extern void jl_readBuffer(char *base, ssize_t nread);
extern void parseAndExecute(char *str);
DLLEXPORT extern void jl_clear_input(void);

#endif // JL_REPL_H
