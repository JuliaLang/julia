#include <julia.h>
#include <stdio.h>
#include <math.h>

double my_c_sqrt(double x)
{
    return sqrt(x);
}

int main()
{
    jl_init(NULL);

    {
        // Simple running Julia code

        jl_eval_string("println(sqrt(2.0))");
    }

    {
        // Accessing the return value

        jl_value_t *ret = jl_eval_string("sqrt(2.0)");

        if (jl_is_float64(ret)) {
            double retDouble = jl_unbox_float64(ret);
            printf("sqrt(2.0) in C: %e\n", retDouble);
        }
    }

    {
        // Same as above but with function handle (more flexible)

        jl_function_t *func = jl_get_function(jl_base_module, "sqrt");
        jl_value_t* argument = jl_box_float64(2.0);
        jl_value_t* ret = jl_call1(func, argument);

        if (jl_is_float64(ret)) {
            double retDouble = jl_unbox_float64(ret);
            printf("sqrt(2.0) in C: %e\n", retDouble);
        }
    }

    {
        // 1D arrays

        jl_value_t* array_type = jl_apply_array_type( jl_float64_type, 1 );
        jl_array_t* x          = jl_alloc_array_1d(array_type , 10);
        JL_GC_PUSH1(&x);

        double* xData = jl_array_data(x);

        size_t i;
        for(i=0; i<jl_array_len(x); i++)
            xData[i] = i;

        jl_function_t *func  = jl_get_function(jl_base_module, "reverse!");
        jl_call1(func, (jl_value_t*) x);

        printf("x = [");
        for(i=0; i<jl_array_len(x); i++)
            printf("%e ", xData[i]);
        printf("]\n");

        JL_GC_POP();
    }

    {
        // define julia function and call it

        jl_eval_string("my_func(x) = 2*x");

        jl_function_t *func = jl_get_function(jl_current_module, "my_func");
        jl_value_t* arg = jl_box_float64(5.0);
        double ret = jl_unbox_float64(jl_call1(func, arg));

        printf("my_func(5.0) = %f\n", ret);
    }

    {
        // call c function

        jl_eval_string("println( ccall( :my_c_sqrt, Float64, (Float64,), 2.0 ) )");
    }

    {
        // check for exceptions

        jl_eval_string("this_function_does_not_exist()");

        if (jl_exception_occurred()) {
            jl_show(jl_stderr_obj(), jl_exception_occurred());
            JL_PRINTF(jl_stderr_stream(), "\n");
        }
    }

    jl_atexit_hook();
    return 0;
}
