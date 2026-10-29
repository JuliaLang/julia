// This file is a part of Julia. License is MIT: https://julialang.org/license

// Tests the libjulia forwarding trampolines (cli/arch/<arch>/trampolines.S):
// each exported function jumps through its `<name>_addr` slot (defined in
// cli/jl_exports.h and filled in by cli/loader_lib.c), and must pass every
// argument and return value through untouched, keep the stack aligned, and not
// get in the way of unwinding back into the caller, from any of its
// instructions.

#define _GNU_SOURCE
#include <stdarg.h>
#include <string.h>
#include "archtest.h"
#include "archstep.h"
#include "../../jl_exported_funcs.inc"

// The slots the trampolines jump through, as in cli/jl_exports.h.
#ifdef _WIN32
#define XX(name) void *name##_addr = NULL;
#else
#define XX(name) __attribute__((visibility("hidden"))) void *name##_addr = NULL;
#endif
JL_RUNTIME_EXPORTED_FUNCS(XX)
#ifdef _WIN32
JL_RUNTIME_EXPORTED_FUNCS_WIN(XX)
#endif
JL_CODEGEN_EXPORTED_FUNCS(XX)
#undef XX

// The trampolines take whatever arguments their target does. Call a few of
// them with signatures that cover the argument and return registers.
typedef struct { int64_t a, b, c, d; } big_t;
extern int64_t jl_alloc_svec(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t,
                             int64_t, int64_t, int64_t, int64_t);
extern double jl_box_int64(double, double, double, double, double, double,
                           double, double, double, double, float, int32_t);
extern big_t jl_cstr_to_string(big_t, int8_t, int16_t);
extern int jl_printf(void *, const char *, ...);
extern void jl_gc_collect(void);
extern int jl_exit(int);

// in another shared library (trampolines_other_lib.c)
extern int other_lib_target(int);
// The caller's own data, read after calling into the other library: the call
// must leave the caller able to address it.
static volatile int caller_data = 42;

static backtrace_t bt, fp_bt;
static uintptr_t caller_ip;
static void (*volatile backtrace_fn)(void);
static volatile int aligned;

static __attribute__((noinline)) int64_t target_ints(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e,
                                                     int64_t f, int64_t g, int64_t h, int64_t i, int64_t j)
{
    aligned = STACK_ALIGNED_AT_ENTRY();
    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j;
}

static __attribute__((noinline)) double target_floats(double a, double b, double c, double d, double e,
                                                      double f, double g, double h, double i, double j,
                                                      float k, int32_t l)
{
    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j + 11*k + 12*l;
}

static __attribute__((noinline)) big_t target_struct(big_t x, int8_t y, int16_t z)
{
    big_t r = {x.d + y, x.c + z, x.b, x.a};
    return r;
}

static __attribute__((noinline)) int target_varargs(void *p, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int i = va_arg(ap, int);
    double d = va_arg(ap, double);
    long l = va_arg(ap, long);
    va_end(ap);
    return p == (void*)fmt && strcmp(fmt, "%d %f %ld") == 0 && i == 7 && d == 2.5 && l == 1L << 30;
}

static __attribute__((noinline)) void target_backtrace(void)
{
    caller_ip = RETURN_IP();
    get_backtrace(&bt);
#ifdef HAVE_FP_BACKTRACE
    get_fp_backtrace(&fp_bt);
#endif
}

static __attribute__((noinline)) void call_backtrace(void)
{
    // Exercise a return address beyond the small fixed windows formerly used here.
    __asm__ volatile (".rept 256\n\tnop\n\t.endr" ::: "memory");
    backtrace_fn();
    __asm__ volatile ("" ::: "memory"); // keep the call out of tail position
}

#ifdef HAVE_ARCHSTEP
// Unwinding from every instruction of a trampoline, which only jumps on to its
// target, must find its caller and all of that's callers.
static step_fn_t tramp_fn;
static step_chain_t callers, callers_lu;
static int step_bad;

static __attribute__((noinline)) void step_target(void)
{
    __asm__ volatile ("" ::: "memory");
}

// After the return address into step_run, which is that of this call and not
// of the trampoline's (and whose stack pointer may differ).
static void add_callers(step_chain_t *c, const backtrace_t *cap)
{
    int i = 0;
    while (i < cap->n && !step_chain_has(c, 0, cap->ips[i]))
        i++;
    step_chain_add_found(c, cap, i + 1);
}

static __attribute__((noinline)) void capture_callers(void)
{
    backtrace_t cap;
    get_backtrace(&cap);
    add_callers(&callers, &cap);
#ifdef HAVE_STEP_LIBUNWIND
    step_libunwind_here(&cap);
    add_callers(&callers_lu, &cap);
#endif
}

static __attribute__((noinline)) void step_run(void)
{
    if (callers.n == 1) {
        step_pause();
        capture_callers();
        step_resume();
    }
    jl_gc_collect();
    __asm__ volatile ("" ::: "memory"); // keep the call out of tail position
}

static void step_sample_trampoline(ucontext_t *uc, step_fn_t *fn)
{
    int ok = callers.n > 2;
    backtrace_t dw, lu, fp;
    dw.n = lu.n = fp.n = 0;
    dw.ended = lu.ended = 1;
#ifdef HAVE_STEP_UNWIND
    step_unwind(uc, &dw);
    ok &= step_chain_ok(&dw, &callers, callers.n);
#endif
#ifdef HAVE_STEP_LIBUNWIND
    step_libunwind(uc, &lu);
    ok &= callers_lu.n > 2 && step_chain_ok(&lu, &callers_lu, callers_lu.n);
#endif
#ifdef HAVE_FP_BACKTRACE
    // the trampoline keeps the caller's frame pointer: from it, the walk
    // finds the caller's callers
    step_fp_unwind(uc, &fp);
    ok &= step_fp_chain_ok(&fp, &callers) && fp.n > 0 && step_chain_has(&callers, 1, fp.ips[0]);
#endif
    if (!ok && step_bad++ < 8) {
        fprintf(stderr, "  unwinding from %s+%#lx:\n", fn->name, (unsigned long)(UC_PC(uc) - fn->lo));
        step_print_chain("unwind info (libgcc)", &dw, 1);
        step_print_chain("unwind info (libunwind)", &lu, 1);
        step_print_chain("frame pointers", &fp, 0);
        step_print_expected(&callers);
        step_print_expected(&callers_lu);
    }
}

static void step_trampoline(void)
{
    step_print_unwinders("trampolines");
    step_set_stacks(0, 0);
    step_fn_t run_fn;
    if (!step_find_function(&tramp_fn, "jl_gc_collect") ||
        !step_find_function(&run_fn, "step_run")) {
        archtest_failures++;
        return;
    }
#if defined(__powerpc64__)
    // the trampoline ends in data (the offset to its slot), after bctr
    for (uintptr_t pc = tramp_fn.lo; pc < tramp_fn.hi; pc += 4) {
        if (*(const uint32_t*)pc == 0x4e800420) {
            tramp_fn.hi = pc + 4;
            break;
        }
    }
#endif
    step_chain_add_return_into(&callers, &run_fn);
    step_chain_add_return_into(&callers_lu, &run_fn);
    void *old = jl_gc_collect_addr;
    jl_gc_collect_addr = (void*)step_target;
    step_functions(&tramp_fn, 1, step_run, step_sample_trampoline);
    jl_gc_collect_addr = old;
    CHECK(tramp_fn.samples > 0);
    CHECK(step_bad == 0);
    if (step_bad)
        fprintf(stderr, "  (%d of %d instructions sampled)\n", step_bad, tramp_fn.samples);
}
#endif

int main(void)
{
    jl_alloc_svec_addr = (void*)target_ints;
    jl_box_int64_addr = (void*)target_floats;
    jl_cstr_to_string_addr = (void*)target_struct;
    jl_printf_addr = (void*)target_varargs;
    jl_gc_collect_addr = (void*)target_backtrace;
    jl_exit_addr = (void*)other_lib_target;

    CHECK(jl_alloc_svec(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) == 385);
    CHECK(jl_exit(1) == 1001);
    CHECK(caller_data == 42);
    CHECK(aligned);
    CHECK(jl_box_int64(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0.5f, -1) == 385 + 5.5 - 12);
    big_t x = {1, 2, 3, 4};
    big_t r = jl_cstr_to_string(x, -3, 300);
    CHECK(r.a == 1 && r.b == 303 && r.c == 2 && r.d == 1);
    const char *fmt = "%d %f %ld";
    CHECK(jl_printf((void*)fmt, fmt, 7, 2.5, 1L << 30));
    // the trampoline is a tail call: the target returns straight to the caller
    backtrace_fn = target_backtrace;
    call_backtrace();
    uintptr_t direct_caller_ip = caller_ip;
    backtrace_fn = jl_gc_collect;
    call_backtrace();
    CHECK(caller_ip == direct_caller_ip);
    CHECK(backtrace_index(&bt, direct_caller_ip) >= 0);
#ifdef HAVE_FP_BACKTRACE
    CHECK(backtrace_index(&fp_bt, direct_caller_ip) >= 0);
#endif
#ifdef HAVE_ARCHSTEP
    step_trampoline();
#endif
    ARCHTEST_DONE("trampolines");
}
