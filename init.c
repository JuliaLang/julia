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
#include <unistd.h>
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

char *jl_stack_lo;
char *jl_stack_hi;
int jl_fpe_err_msg = 0;
size_t jl_page_size;

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
    jl_stack_hi = (char*)&stack_size;
    jl_stack_lo = jl_stack_hi - stack_size;
}

void fpe_handler(int arg)
{
    (void)arg;
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, SIGFPE);
    sigprocmask(SIG_UNBLOCK, &sset, NULL);

    if (jl_fpe_err_msg)
        jl_error("error: integer divide by zero");
    else
        jl_raise();
}

void segv_handler(int sig, siginfo_t *info, void *context)
{
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, SIGSEGV);
    sigprocmask(SIG_UNBLOCK, &sset, NULL);

    if ((char*)info->si_addr > (char*)jl_current_task->stack-8192 &&
        (char*)info->si_addr <
        (char*)jl_current_task->stack+jl_current_task->ssize) {
        jl_error("error: stack overflow");
    }
    else {
        signal(SIGSEGV, SIG_DFL);
    }
}

static void jl_get_builtin_hooks();

void julia_init()
{
    jl_page_size = sysconf(_SC_PAGESIZE);
    jl_find_stack_bottom();
#ifdef BOEHM_GC
    GC_expand_hp(12000000);  //shaves a tiny bit off startup time
    GC_time_limit = GC_TIME_UNLIMITED;
#endif
    jl_init_frontend();
    jl_init_types();
    jl_init_modules();
    jl_init_tasks(jl_stack_lo, jl_stack_hi-jl_stack_lo);
    jl_init_primitives();
    jl_init_codegen();
    jl_load_boot_j();
    jl_get_builtin_hooks();
    jl_init_builtin_types();
    jl_init_builtins();

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
    JL_TRY {
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
    }
    JL_CATCH {
        ios_printf(ios_stderr, "error during startup.\n");
        return 1;
    }
#ifdef BOEHM_GC
    GC_gcollect();
#endif
    return 0;
}

static jl_value_t *global(char *name)
{
    return *jl_get_bindingp(jl_system_module, jl_symbol(name));
}

void jl_get_builtin_hooks()
{
    // update bootstrapping versions of Bool and Int32
    jl_bits_type_t *i32t = (jl_bits_type_t*)global("Int32");
    *jl_int32_type = *i32t;
    jl_set_const(jl_system_module, jl_symbol("Int32"),
                 (jl_value_t*)jl_int32_type);

    jl_bits_type_t *bt   = (jl_bits_type_t*)global("Bool");
    *jl_bool_type = *bt;
    jl_set_const(jl_system_module, jl_symbol("Bool"),
                 (jl_value_t*)jl_bool_type);

    // fetch references to things defined in boot.j
    jl_char_type    = (jl_bits_type_t*)global("Char");
    jl_int8_type    = (jl_bits_type_t*)global("Int8");
    jl_uint8_type   = (jl_bits_type_t*)global("Uint8");
    jl_int16_type   = (jl_bits_type_t*)global("Int16");
    jl_uint16_type  = (jl_bits_type_t*)global("Uint16");
    jl_uint32_type  = (jl_bits_type_t*)global("Uint32");
    jl_int64_type   = (jl_bits_type_t*)global("Int64");
    jl_uint64_type  = (jl_bits_type_t*)global("Uint64");

    jl_float32_type = (jl_bits_type_t*)global("Float32");
    jl_float64_type = (jl_bits_type_t*)global("Float64");

    jl_array_type = (jl_struct_type_t*)global("Array");
    jl_arraystring_type = (jl_struct_type_t*)global("ArrayString");
}
