/*
  AST
  interface to front-end, obtains and translates syntax trees
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
#include <gc.h>
#include "llt.h"
#include "julia.h"

#define ___VERSION 405001
#include "gambit.h"

#include "jlfrontend.h"

// gambit boilerplate. just look at all those underscores...
#define SCHEME_LIBRARY_LINKER ____20_jlfrontend__
___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state_struct*);
___END_C_LINKAGE

void jl_init_frontend()
{
    ___setup_params_struct setup_params;
    ___setup_params_reset (&setup_params);
    setup_params.version = ___VERSION;
    setup_params.linker  = SCHEME_LIBRARY_LINKER;
    ___setup (&setup_params);
}

void jl_shutdown_frontend()
{
    ___cleanup();
}
