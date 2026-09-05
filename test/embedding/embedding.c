// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <julia.h>
#include <stdio.h>
#include <math.h>

JULIA_DEFINE_FAST_TLS // only define this once, in an executable

#ifdef _OS_WINDOWS_
__declspec(dllexport) __cdecl
#endif
double my_c_sqrt(double x)
{
    return sqrt(x);
}

jl_value_t *checked_eval_string(const char* code)
{
    jl_value_t *result = jl_eval_string(code);
    if (jl_exception_occurred()) {
        // none of these allocate, so a gc-root (JL_GC_PUSH) is not necessary
        jl_call2(jl_get_function(jl_base_module, "showerror"),
                 jl_stderr_obj(),
                 jl_exception_occurred());
        jl_printf(jl_stderr_stream(), "\n");
        jl_atexit_hook(1);
        exit(1);
    }
    assert(result && "Missing return value but no exception occurred!");
    return result;
}

static volatile int tagged_root_fin_ran = 0;

static void tagged_root_finalizer(void *o)
{
    (void)o;
    tagged_root_fin_ran = 1;
}

// Tagged pointers -- immediate values stored in the low bits of a pointer,
// e.g. introduced by a foreign runtime sharing Julia's GC -- may be stored
// in JL_GC_PUSH*/JL_GC_PUSHARGS roots. The GC must skip them without
// disturbing the marking of neighboring roots.
//
// Detection: a fresh object whose only reference is a frame slot next to a
// tagged pointer. If marking mishandles the tagged pointer, the object is
// swept and its finalizer runs while the frame is still pushed.
static void test_tagged_pointer_roots(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    const uintptr_t large_imm = (uintptr_t)1 << 40;

    // Direct-layout frame (JL_GC_PUSHARGS).
    jl_value_t **args;
    JL_GC_PUSHARGS(args, 4);
    args[0] = (jl_value_t *)0x5;
    args[1] = jl_box_int64(42424242);
    args[2] = (jl_value_t *)0x7;
    args[3] = (jl_value_t *)(large_imm | 0x1);
    jl_gc_add_ptr_finalizer(ptls, args[1], (void *)tagged_root_finalizer);
    jl_gc_collect(JL_GC_FULL);
    if (tagged_root_fin_ran) {
        fprintf(stderr, "tagged pointer in JL_GC_PUSHARGS frame un-rooted its neighbor\n");
        exit(1);
    }

    // Large tagged pointers in even slots must simply be skipped, not
    // interpreted as object references.
    args[0] = (jl_value_t *)(large_imm | 0x1);
    args[2] = (jl_value_t *)(large_imm | 0x3);
    jl_gc_collect(JL_GC_FULL);
    JL_GC_POP();

    jl_gc_collect(JL_GC_FULL);
    if (!tagged_root_fin_ran) {
        fprintf(stderr, "finalizer never ran after JL_GC_PUSHARGS frame was popped\n");
        exit(1);
    }
    tagged_root_fin_ran = 0;

    // Indirect-layout frame (JL_GC_PUSH2): locals holding tagged pointers.
    jl_value_t *tagged = (jl_value_t *)0x5;
    jl_value_t *obj = NULL;
    JL_GC_PUSH2(&tagged, &obj);
    obj = jl_box_int64(24242424);
    jl_gc_add_ptr_finalizer(ptls, obj, (void *)tagged_root_finalizer);
    jl_gc_collect(JL_GC_FULL);
    if (tagged_root_fin_ran) {
        fprintf(stderr, "tagged pointer in JL_GC_PUSH frame un-rooted its neighbor\n");
        exit(1);
    }
    tagged = (jl_value_t *)(large_imm | 0x3);
    jl_gc_collect(JL_GC_FULL);
    JL_GC_POP();

    jl_gc_collect(JL_GC_FULL);
    if (!tagged_root_fin_ran) {
        fprintf(stderr, "finalizer never ran after JL_GC_PUSH frame was popped\n");
        exit(1);
    }
}

int main()
{
    // check that setting options works
    jl_options.opt_level = 1;

    jl_init();

    {
        // Simple running of Julia code

        checked_eval_string("println(sqrt(2.0))");
    }

    if (jl_options.opt_level != 1) {
        jl_printf(jl_stderr_stream(), "setting jl_options didn't work\n");
        jl_atexit_hook(1);
        exit(1);
    }

    {
        // Accessing the return value

        jl_value_t *ret = checked_eval_string("sqrt(2.0)");
        double retDouble = jl_unbox_float64(ret);
        printf("sqrt(2.0) in C: %e\n", retDouble);
        fflush(stdout);
    }

    {
        // Same as above but with function handle (more flexible)

        jl_value_t *func = jl_get_function(jl_base_module, "sqrt");
        jl_value_t* argument = jl_box_float64(2.0);
        jl_value_t* ret = jl_call1(func, argument);
        double retDouble = jl_unbox_float64(ret);
        printf("sqrt(2.0) in C: %e\n", retDouble);
        fflush(stdout);
    }

    {
        // Same as above but using `@cfunction`
        double (*sqrt_jl)(double) = jl_unbox_voidpointer(jl_eval_string("@cfunction(sqrt, Float64, (Float64,))"));
        double retDouble = sqrt_jl(2.0);
        printf("sqrt(2.0) in C: %e\n", retDouble);
        fflush(stdout);
    }

    {
        // 1D arrays

        jl_value_t* array_type = jl_apply_array_type((jl_value_t*)jl_float64_type, 1);
        jl_array_t* x          = jl_alloc_array_1d(array_type, 10);
        // JL_GC_PUSH* is required here to ensure that `x` is not deleted before
        // (aka, is gc-rooted until) the program reaches the corresponding JL_GC_POP()
        JL_GC_PUSH1(&x);

        double* xData = jl_array_data(x, double);

        size_t i;
        for (i = 0; i < jl_array_nrows(x); i++)
            xData[i] = i;

        jl_value_t *func  = jl_get_function(jl_base_module, "reverse!");
        jl_call1(func, (jl_value_t*) x);

        printf("x = [");
        for (i = 0; i < jl_array_nrows(x); i++)
            printf("%e ", xData[i]);
        printf("]\n");
        fflush(stdout);

        JL_GC_POP();
    }

    {
        // Defining a Julia function and calling it

        checked_eval_string("my_func(x) = 2 * x");

        jl_value_t *func = jl_get_function(jl_main_module, "my_func");
        jl_value_t* arg = jl_box_float64(5.0);
        double ret = jl_unbox_float64(jl_call1(func, arg));

        printf("my_func(5.0) = %f\n", ret);
        fflush(stdout);
    }

    {
        // Calling a C function from Julia (from C)

        // in a shared library (exported, by name)
        checked_eval_string("println( ccall(:my_c_sqrt, Float64, (Float64,), 2.0) )");

        // or via a pointer
        jl_value_t *call_by_ptr = checked_eval_string(
                "my_c_sqrt -> println( ccall(my_c_sqrt, Float64, (Float64,), 2.0) )");
        jl_call1(call_by_ptr, jl_box_voidpointer(my_c_sqrt));
    }

    {
        // Handling exceptions gracefully

        jl_value_t *f = checked_eval_string("function this_function_has_no_methods end");
        jl_call0(f);

        if (jl_exception_occurred()) {
            jl_call2(jl_get_function(jl_base_module, "showerror"),
                     jl_stderr_obj(),
                     jl_exception_occurred());
            jl_printf(jl_stderr_stream(), "\n");
        }

    }

    {
        // Creating and using a native C function handle
        // to a Julia function signature

        checked_eval_string(
        "function bar()\n"
        "    println(\"called bar\")\n"
        "    random_return_value = 42\n"
        "end"
        );

        checked_eval_string(
        "function bar_from_c()\n"
        "    bar()\n"
        "    nothing\n"
        "end"
        );

        typedef void (*Func_VOID__VOID)(void);
        jl_value_t *pbar = jl_eval_string("@cfunction(bar_from_c, Cvoid, ())");
        Func_VOID__VOID bar = (Func_VOID__VOID)jl_unbox_voidpointer(pbar);
        bar();
        checked_eval_string("bar() = println(\"calling new bar\")");
        bar();
    }

    {
        // Importing a Julia package

        checked_eval_string(
        "let dir = dirname(unsafe_string(Base.JLOptions().julia_bin))\n"
        // disable the package manager
        "    ENV[\"JULIA_PKGDIR\"] = joinpath(dir, \"disabled\")\n"
        // locate files relative to the "embedding" executable
        "    stdlib = filter(env -> startswith(Base.find_package(\"Distributed\"), env), Base.load_path())[end]\n"
        "    push!(empty!(LOAD_PATH), dir, stdlib)\n"
        "end"
        );
        checked_eval_string("import LocalModule");
        checked_eval_string("LocalModule.myapp()");
    }

    {
        // Main.include and Main.eval exist (#28825)
        checked_eval_string("include(\"include_and_eval.jl\")");
        checked_eval_string("f28825()");
    }

    {
        // jl_typeof works (#50714)
        jl_value_t *v = checked_eval_string("sqrt(2.0)");
        jl_value_t *t = jl_typeof(v);
    }

    JL_TRY {
        jl_error("exception thrown");
    }
    JL_CATCH {
        jl_printf(jl_stderr_stream(), "exception caught from C\n");
    }

    test_tagged_pointer_roots();

    int ret = 0;
    jl_atexit_hook(ret);
    return ret;
}
