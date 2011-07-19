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
#if defined(__linux) || defined(__APPLE__)
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
#include "llt.h"
#include "julia.h"

int jl_boot_file_loaded = 0;

char *jl_stack_lo;
char *jl_stack_hi;
size_t jl_page_size;

static void jl_find_stack_bottom()
{
    size_t stack_size;
#if defined(__linux) || defined(__APPLE__)
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

    jl_divide_by_zero_error();
}

void segv_handler(int sig, siginfo_t *info, void *context)
{
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, SIGSEGV);
    sigprocmask(SIG_UNBLOCK, &sset, NULL);

#ifdef COPY_STACKS
    if ((char*)info->si_addr > (char*)jl_stack_lo-3000000 &&
        (char*)info->si_addr < (char*)jl_stack_hi) {
#else
    if ((char*)info->si_addr > (char*)jl_current_task->stack-8192 &&
        (char*)info->si_addr <
        (char*)jl_current_task->stack+jl_current_task->ssize) {
#endif
        jl_raise(jl_stackovf_exception);
    }
    else {
        signal(SIGSEGV, SIG_DFL);
    }
}

volatile sig_atomic_t jl_signal_pending = 0;
volatile sig_atomic_t jl_defer_signal = 0;

void sigint_handler(int sig, siginfo_t *info, void *context)
{
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, SIGINT);
    sigprocmask(SIG_UNBLOCK, &sset, NULL);

    if (jl_defer_signal) {
        jl_signal_pending = sig;
    }
    else {
        jl_signal_pending = 0;
        jl_raise(jl_interrupt_exception);
    }
}

static void jl_get_builtin_hooks();

void *jl_dl_handle;

static jl_value_t *global(char *name)
{
    return *jl_get_bindingp(jl_system_module, jl_symbol(name));
}

#ifdef COPY_STACKS
void jl_switch_stack(jl_task_t *t, jmp_buf *where);
extern jmp_buf * volatile jl_jmp_target;
#endif

void julia_init(char *imageFile)
{
    jl_page_size = sysconf(_SC_PAGESIZE);
    jl_find_stack_bottom();
    jl_dl_handle = jl_load_dynamic_library(NULL);
#ifdef JL_GC_MARKSWEEP
    jl_gc_init();
    jl_gc_disable();
#endif
    jl_init_frontend();
    jl_init_types();
    jl_init_modules();
    jl_init_tasks(jl_stack_lo, jl_stack_hi-jl_stack_lo);
    jl_init_codegen();
    jl_an_empty_cell = (jl_value_t*)jl_alloc_cell_1d(0);

    jl_init_serializer();

    if (!imageFile) {
        jl_init_primitives();
        jl_load_boot_j();
        jl_get_builtin_hooks();
        jl_boot_file_loaded = 1;
        jl_init_builtins();
        jl_init_box_caches();
    }

    if (imageFile) {
        JL_TRY {
            jl_restore_system_image(imageFile);
        }
        JL_CATCH {
            ios_printf(ios_stderr, "error during init:\n");
            jl_show(jl_exception_in_transit);
            ios_printf(ios_stdout, "\n");
            exit(1);
        }
    }

    struct sigaction actf;
    memset(&actf, 0, sizeof(struct sigaction));
    sigemptyset(&actf.sa_mask);
    actf.sa_handler = fpe_handler;
    actf.sa_flags = 0;
    if (sigaction(SIGFPE, &actf, NULL) < 0) {
        ios_printf(ios_stderr, "sigaction: %s\n", strerror(errno));
        exit(1);
    }

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

    memset(&act, 0, sizeof(struct sigaction));
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = sigint_handler;
    act.sa_flags = SA_SIGINFO;
    if (sigaction(SIGINT, &act, NULL) < 0) {
        ios_printf(ios_stderr, "sigaction: %s\n", strerror(errno));
        exit(1);
    }

#ifdef JL_GC_MARKSWEEP
    jl_gc_enable();
#endif
}

DLLEXPORT
int julia_trampoline(int argc, char *argv[], int (*pmain)(int ac,char *av[]))
{
#ifdef COPY_STACKS
    // initialize base context of root task
    jl_root_task->stackbase = (char*)&argc;
    if (setjmp(jl_root_task->base_ctx)) {
        jl_switch_stack(jl_current_task, jl_jmp_target);
    }
#endif
    return pmain(argc, argv);
}

jl_function_t *jl_typeinf_func=NULL;
jl_function_t *jl_memio_func=NULL;
jl_function_t *jl_append_any_func=NULL;
jl_function_t *jl_method_missing_func=NULL;

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
/*
static void clear_method_caches()
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
            mt->cache = NULL;
        }
    }
}
*/
DLLEXPORT void jl_enable_inference()
{
    if (jl_boundp(jl_system_module, jl_symbol("typeinf_ext"))) {
        //clear_method_caches();
        jl_typeinf_func =
            (jl_function_t*)*(jl_get_bindingp(jl_system_module,
                                              jl_symbol("typeinf_ext")));
        // warm up type inference to put the latency up front
        jl_value_t *one = jl_box_int32(1);
        jl_apply((jl_function_t*)*(jl_get_bindingp(jl_system_module,
                                                   jl_symbol("+"))),
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

DLLEXPORT void jl_set_memio_func()
{
    jl_memio_func = (jl_function_t*)global("memio");
}

JL_CALLABLE(jl_weakref_ctor);

// fetch references to things defined in boot.j
void jl_get_builtin_hooks()
{
    jl_nothing      = global("nothing");

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

    jl_weakref_type = (jl_struct_type_t*)global("WeakRef");
    jl_weakref_type->fptr = jl_weakref_ctor;
    jl_weakref_type->env = NULL;
    jl_weakref_type->linfo = NULL;
    jl_string_type = (jl_tag_type_t*)global("String");
    jl_ascii_string_type = (jl_struct_type_t*)global("ASCIIString");
    jl_utf8_string_type = (jl_struct_type_t*)global("UTF8String");
    jl_errorexception_type = (jl_struct_type_t*)global("ErrorException");
    jl_typeerror_type = (jl_struct_type_t*)global("TypeError");
    jl_loaderror_type = (jl_struct_type_t*)global("LoadError");
    jl_uniontoocomplex_type = (jl_struct_type_t*)global("UnionTooComplexError");
    jl_backtrace_type = (jl_struct_type_t*)global("BackTrace");

    jl_stackovf_exception =
        jl_apply((jl_function_t*)global("StackOverflowError"), NULL, 0);
    jl_divbyzero_exception =
        jl_apply((jl_function_t*)global("DivideByZeroError"), NULL, 0);
    jl_undefref_exception =
        jl_apply((jl_function_t*)global("UndefRefError"),NULL,0);
    jl_interrupt_exception =
        jl_apply((jl_function_t*)global("InterruptException"),NULL,0);

    jl_append_any_func = (jl_function_t*)global("append_any");
    jl_method_missing_func = (jl_function_t*)global("method_missing");

    jl_array_uint8_type =
        (jl_type_t*)jl_apply_type((jl_value_t*)jl_array_type,
                                  jl_tuple2(jl_uint8_type,
                                            jl_box_int32(1)));
}
