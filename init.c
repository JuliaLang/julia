/*
  init.c
  system initialization and global state
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <assert.h>
#if defined(LINUX) || defined(MACOSX)
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/mman.h>
#endif
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <libgen.h>
#include <getopt.h>
#ifdef BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

jmp_buf ExceptionHandler;
jmp_buf *CurrentExceptionHandler = &ExceptionHandler;

char *jl_stack_bottom;
char *jl_stack_top;

static void jl_find_stack_bottom()
{
    size_t stack_size;
#if defined(LINUX) || defined(MACOSX)
    struct rlimit rl;
    getrlimit(RLIMIT_STACK, &rl);
    stack_size = rl.rlim_cur;
#else
    stack_size = 262144;  // guess
#endif
    jl_stack_top = (char*)&stack_size;
    jl_stack_bottom = jl_stack_top - stack_size;
}

void fpe_handler(int arg)
{
    (void)arg;
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, SIGFPE);
    sigprocmask(SIG_UNBLOCK, &sset, NULL);

    jl_error("error: integer divide by zero");
}

void segv_handler(int sig, siginfo_t *info, void *context)
{
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, SIGSEGV);
    sigprocmask(SIG_UNBLOCK, &sset, NULL);

    if ((char*)info->si_addr > jl_stack_bottom-8192 &&
        (char*)info->si_addr < jl_stack_top) {
        jl_error("error: stack overflow");
    }
    else {
        signal(SIGSEGV, SIG_DFL);
    }
}

void julia_init()
{
    jl_find_stack_bottom();
#ifdef BOEHM_GC
    GC_expand_hp(12000000);  //shaves a tiny bit off startup time
#endif
    jl_init_frontend();
    jl_init_types();
    jl_init_builtin_types();
    jl_init_modules();
    jl_init_builtins();
    jl_init_codegen();
    jl_init_tasks(jl_stack_bottom, jl_stack_top-jl_stack_bottom);

    signal(SIGFPE, fpe_handler);

    stack_t ss;
    ss.ss_flags = 0;
    ss.ss_size = SIGSTKSZ;
    ss.ss_sp = malloc(ss.ss_size);
    if (sigaltstack(&ss, NULL) < 0) {
        ios_printf(ios_stderr, "sigaltstack: %s\n", strerror(errno));
        exit(1);
    }
    struct sigaction act;
    memset(&act, 0, sizeof(struct sigaction));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = segv_handler;
    act.sa_flags = SA_ONSTACK | SA_SIGINFO;
    if (sigaction(SIGSEGV, &act, NULL) < 0) {
        ios_printf(ios_stderr, "sigaction: %s\n", strerror(errno));
        exit(1);
    }
}

jl_function_t *jl_typeinf_func=NULL;

static void clear_tfunc_caches()
{
    htable_t *t = &jl_system_module->bindings;
    size_t i;
    for(i=0; i < t->size; i+=2) {
        if (t->table[i+1] == HT_NOTFOUND)
            continue;
        jl_binding_t *b = (jl_binding_t*)t->table[i+1];
        if (b->value != NULL && jl_is_func(b->value) && jl_is_gf(b->value)) {
            jl_function_t *f = (jl_function_t*)b->value;
            jl_methtable_t *mt = jl_gf_mtable(f);
            //mt->cache = NULL;
            jl_methlist_t *ml = mt->defs;
            while (ml != NULL) {
                if (ml->func != NULL && ml->func->linfo != NULL)
                    ml->func->linfo->tfunc = (jl_value_t*)jl_null;
                ml = ml->next;
            }
        }
    }
}

int jl_load_startup_file()
{
    if (!setjmp(ExceptionHandler)) {
        jl_load("start.j");
        if (jl_boundp(jl_system_module, jl_symbol("typeinf_ext"))) {
            jl_typeinf_func =
                (jl_function_t*)*(jl_get_bindingp(jl_system_module,
                                                  jl_symbol("typeinf_ext")));
            // warm up type inference to put the latency up front
            jl_value_t *one = jl_box_int32(1);
            jl_apply((jl_function_t*)*(jl_get_bindingp(jl_system_module,
                                                       jl_symbol("fact"))),
                     &one, 1);
            /*
              cached t-functions and inferred ASTs need to be cleared at
              this point, because during bootstrapping we might not be
              able to inline optimally. the reason is that we cache an AST
              before doing inlining, to prevent infinite recursion.
              for example, consider this function:

              > (x::Real, y::Real) = (y < x)

              after doing inference on this >, we cache its AST "(y < x)".
              now we begin inlining. the problem is that the inlining code
              itself will trigger compilation of functions that use >, so
              "(y < x)" will be inlined into those functions. ultimately
              we inline the definition of < into "(y < x)", but by then it's
              too late, since "(y < x)" has already been inlined into some
              functions.

              to fix this, we clear the t-function cache after all
              type-inference related code has been compiled. now we can
              inline everything fully without compilation of the compiler
              itself interfering.
            */
            clear_tfunc_caches();
        }
    } else {
        ios_printf(ios_stderr, "error during startup.\n");
        return 1;
    }
#ifdef BOEHM_GC
    GC_gcollect();
#endif
    return 0;
}
