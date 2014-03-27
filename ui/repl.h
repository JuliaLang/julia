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
#ifndef _MSC_VER
#include <unistd.h>
#include <libgen.h>
#endif
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <getopt.h>
#include <ctype.h>

#include "julia.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _MSC_VER
DLLEXPORT char * basename(char *);
DLLEXPORT char * dirname(char *);
#endif

extern int tab_width;
extern DLLEXPORT char *julia_home;

extern void handle_input(jl_value_t *ast, int end, int show_value);
extern int ends_with_semicolon(const char *input);
extern void repl_callback_enable(char *prompt);
extern void jl_read_buffer(unsigned char *base, ssize_t nread);
extern void jl_init_repl(int history);

#ifdef __cplusplus
}
#endif

#endif // JL_REPL_H
