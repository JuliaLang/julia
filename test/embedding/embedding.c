// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <julia.h>
#include <stdio.h>
#include <math.h>

JULIA_DEFINE_FAST_TLS() // only define this once, in an executable

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

int main()
{
    jl_init();

    {
        // Simple running of Julia code

        checked_eval_string("println(sqrt(2.0))");
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

        jl_function_t *func = jl_get_function(jl_base_module, "sqrt");
        jl_value_t* argument = jl_box_float64(2.0);
        jl_value_t* ret = jl_call1(func, argument);
        double retDouble = jl_unbox_float64(ret);
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

        double* xData = jl_array_data(x);

        size_t i;
        for (i = 0; i < jl_array_len(x); i++)
            xData[i] = i;

        jl_function_t *func  = jl_get_function(jl_base_module, "reverse!");
        jl_call1(func, (jl_value_t*) x);

        printf("x = [");
        for (i = 0; i < jl_array_len(x); i++)
            printf("%e ", xData[i]);
        printf("]\n");
        fflush(stdout);

        JL_GC_POP();
    }

    {
        // Defining a Julia function and calling it

        checked_eval_string("my_func(x) = 2 * x");

        jl_function_t *func = jl_get_function(jl_current_module, "my_func");
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
        jl_value_t *pbar = jl_eval_string("cfunction(bar_from_c, Cvoid, Tuple{})");
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
        "    stdlib = filter(env -> startswith(Base.find_package(Base, \"Distributed\"), env), Base.load_path())[end]\n"
        "    push!(empty!(LOAD_PATH), dir, stdlib)\n"
        "end"
        );
        checked_eval_string("import LocalModule");
        checked_eval_string("LocalModule.myapp()");
    }

    int ret = 0;
    jl_atexit_hook(ret);
    return ret;
}
