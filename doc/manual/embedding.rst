.. _man-embedding:

.. highlight:: c

**************************
 Embedding Julia
**************************

As we have seen (:ref:`man-calling-c-and-fortran-code`) Julia has a very simple and efficient way to call functions that are written in the C programming language. But there are various situations where actually the opposite is needed: calling Julia function from C code. This can for instance be used to integrate code that has been prototyped in Julia into a larger C/C++ project, without the need to rewrite everything in C/C++. To make this possible Julia features a C API that can be used to embed Julia into a C/C++ program. As almost all programming languages have some way to call C functions, the Julia C-API can also be used to build further language bridges (E.g. calling Julia from Python or C#).


High-Level Embedding
=====================

We start with a very simple C program that initializes Julia and calls some Julia code without the need to share data between Julia and C::

  #include <julia.h>

  int main(int argc, char *argv[])
  {
    jl_init(NULL);
    jl_eval_string("print(sqrt(2.0))");

    return 0;
  }

In order to build this program you have to put the path to the Julia header into the include path and link against libjulia. For instance, when Julia is installed to $JULIA_DIR, one can compile the above test program test.c with gcc using::

    gcc -o test -I$JULIA_DIR/include/julia -L$JULIA_DIR/usr/lib -ljulia test.c

Alternatively, please have a look at the ``embedding.c`` program that can be found in the julia source tree in the ``examples/`` folder.

The first thing that has do be done before calling any other Julia C function is to initialize Julia. This is done by calling ``jl_init``, which takes as argument a C string (``const char*``) to the location where Julia is installed. When the argument is NULL, a standard Julia location is assumed. The second statement in the test program evaluates a Julia statement using a call to ``jl_eval_string``.

Converting Types
========================

While it is very nice to be able to execute a command in the Julia interpreter, it would be even more interesting to return the value of the expression to the host program. As Julia is a dynamically typed language and C is a statically typed language, we have to convert data between the type systems. Converting C values into Julia values is called `boxing`, while converting the other way around is called `unboxing`. Our improved sample program that calculates the square root of 2 in Julia and reads back the result in C looks as follows::

    jl_value_t* ret = jl_eval_string("sqrt(2.0)");

    if(jl_is_float64(ret))
    {
        double ret_unboxed = jl_unbox_float64(ret);
        printf("sqrt(2.0) in C: %e \n", ret_unboxed);
    }

The return value of ``jl_eval_string`` is a pointer of type ``jl_value_t*``. This is the C type that holds Julia values of any type. In order to check whether ``ret`` is of a specific C type, we can use the ``jl_is_...`` functions. By typing ``typeof(sqrt(2.0))`` into the Julia shell we can see that the return type is float64 (i.e. double). To convert the boxed Julia value into a C double the ``jl_unbox_float64`` function is used in the above code snippet.

Converting C values into Julia values is as simple as the other way around. One can just use the ``jl_box_...`` functions::

    jl_value_t* a = jl_box_float64(3.0);
    jl_value_t* b = jl_box_float32(3.0f);
    jl_value_t* c = jl_box_int32(3);

As we will see next, boxing is required to call Julia functions with specific arguments.

Calling Julia Functions
========================

Calling Julia function can be done with the ``jl_eval_string`` function has has been described before. While ``jl_eval_string`` can call Julia functions and access the return value, there is a more flexible way for this, which allows to easily pass arguments to the Julia function. The following code does the same as ``jl_value_t* ret = jl_eval_string("sqrt(2.0)")``::

    jl_function_t *func = jl_get_function(jl_base_module, "sqrt");
    jl_value_t* argument = jl_box_float64(2.0);
    jl_value_t* ret = jl_call1(func, argument);

In the first step, a handle to the Julia function ``sqrt`` is retrieved by calling ``jl_get_function``. The first argument passed to ``jl_get_function`` is a global pointer to the Base module in which ``sqrt`` is defined. Then, the double value is boxed using the ``jl_box_float64`` function. Finally, in the last step, the function is called by using the ``jl_call1`` function. The first argument of ``jl_call1`` is the Julia function handle while the second argument is the actual argument for the Julia function. Note, that there are also, ``jl_call0``, ``jl_call2``, and ``jl_call3`` functions for calling Julia functions without, with 2 or with 3 arguments. The general ``jl_call`` function has the signature::

    jl_value_t *jl_call(jl_function_t *f, jl_value_t **args, int32_t nargs)

Its second argument ``args`` is an array of ``jl_value_t*`` arguments while ``nargs`` is the number of arguments.

Working with Arrays
========================

In next example, it is shown how to exchange arrays between Julia back and forth. In order to make this highly performant, the array data will be shared between C and Julia.
Julia arrays are represented in C by the datatype ``jl_array_t*``. Basically, ``jl_array_t`` is a struct that contains:

- Information about the datatype
- A void pointer to the data block
- Information about the sizes of the array

To keep things simple, we start with a 1D array. Creating an array containing Float64 elements of length 10 is done by::

    jl_value_t* array_type = jl_apply_array_type( jl_float64_type, 1 );
    jl_array_t* x          = jl_alloc_array_1d(array_type , 10);

Alternatively, if you have already allocated the array you can generate a thin wrapper around that data::

    double* existingArray = (double*) malloc(sizeof(double)*10);
    jl_array_t* x = jl_ptr_to_array_1d(array_type, existingArray, 10, 0);
    
The last parameter is a boolean indicating whether Julia should take over the ownership of the data (only usefull for dynamic arrays). In order to access the data of x, we can use ``jl_array_data``::

    double* xData = (double*) jl_array_data(x);
    
This is obviously more important when letting Julia allocate the array for us. Now we can fill the array::

    for(size_t i=0; i<jl_array_len(x); i++)
      xData[i] = i;
      
Now let us call a Julia function that performs an in-place operation on ``x``::      

    jl_function_t* func  = jl_get_function(jl_base_module, "reverse!");
    jl_call1(func, (jl_value_t *) x);

By printing the array, one can verify that the elements of ``x`` are now reversed.

Accessing Returned Arrays
---------------------------------
If a Julia function returns an array, the return value of ``jl_eval_string`` and ``jl_call`` can be casted into a ``jl_array_t*`` type::

    jl_function_t* func  = jl_get_function(jl_base_module, "reverse");
    jl_array_t* y = (jl_array_t*)  jl_call1(func, (jl_value_t *) x);

Now the content of ``y`` can be accessed as before using ``jl_array_data``.

TODO: Whats up with memory management here?

Multidimensional Arrays
---------------------------------
Julia supports multidimensional arrays. In memory, the entries are stored in a linearised form, where Julia uses the column-major data format. Here is some code that creates a 2D array and uses some functions to access the array properties::

    // Create 2D array of float64 type
    jl_value_t* array_type = jl_apply_array_type( jl_float64_type, 1 );
    jl_array_t* x  = jl_alloc_array_2d(array_type , 10, 5);

    // Get array pointer
    double* p = (double*) jl_array_data(x);
    // Get number of dimensions
    int ndims = jl_array_ndims(x)
    // Get the size of the i-th dim
    size_t size0 =  jl_array_dim(x,0)
    size_t size1 =  jl_array_dim(x,1)

    // Fill array with data
    for(size_t i=0; i<size1; i++)
        for(size_t j=0; j<size0; j++)
            p[ j + size0* i] = i + j; 

Calling Non-Base Julia Code
===========================

In the examples discussed until now, only Julia functions from the Base module were used. In order to call either a self written function, module or an existing Julia package, one has to first bring the function/module into the current scope of Julia. 

Defining Julia Functions in C Code
-----------------------------------------------

One way to introduce new Julia function is to define them inside of a ``jl_eval_string`` call::
 
    jl_eval_string("my_func(x) = 2*x");

Now the function can be called either in a ``jl_eval_string`` call, or using the handle of our function::

    jl_function_t *func = jl_get_function(jl_current_module, "my_func");
    jl_value_t* arg = jl_box_float64(5.0);
    double ret = jl_unbox_float64(jl_call1(func, arg));

Note, that we now have to use the ``jl_current_module`` module pointer as the function ``my_func`` has been added to the current module scope.

Using Non-Standard Modules
-----------------------------------------

In order to call functions from non-standard modules, one first has to import the module using e.g.::

    jl_eval_string("using MyModule");

Then, function handles can be retrieved as before using the ``jl_current_module`` module pointer.


Julia Callable C Functions
=====================================

When embedding Julia into a C/C++ application, there sometimes is the need to call C code from Julia. Imagine, for instance, that we have developed some C/C++ game and want to let the user develop Julia scripts that can enhance/modify some behavior within our game. There are basically two different possibilities to achieve this task:

- The scripting API is developed in C and provided in form of a shared library that can be called from Julia using ``ccall``. The raw ``ccall`` will then have to be wrapped in Julia to perform type and dimension checks.
-  Alternatively, we can develop Julia callable C functions that have a special form  and perform the type and dimension checks in C. These, functions have to be registered to be callable in C.

As the first way has been already discussed in the section :ref:`man-calling-c-and-fortran-code`, we will now focus on Julia callable C functions here.

Julia Callable C Functions
-------------------------------------------

In order to make a C function Julia callable it must have the following signature::

    jl_value_t* julia_callable(jl_value_t* F, jl_value_t** args, uint32_t nargs)

The number of arguments that are passed from Julia to this function is ``nargs``. The arguments itself are passed in an array of ``jl_value_t*`` arguments (``args``). The function can return a result in form of a ``jl_value_t*``. Lets have a look at an example of a Julia callable C function::

    jl_value_t* my_c_sqrt(jl_value_t* F, jl_value_t** args, uint32_t nargs)
    {
        double x = jl_unbox_float64(args[0]);
        x = sqrt(x);
        return jl_box_float64(x);
    }

As one can see, the function arguments first have to be unboxed to access their value. The return value has to be boxed before returning it to Julia. In order to ensure that the function signature is correct, one can use the ``JL_CALLABLE`` macro. The function ``my_c_sqrt`` can be equivalently defined as::

    JL_CALLABLE(my_c_sqrt)
    {
        double x = jl_unbox_float64(args[0]);
        x = sqrt(x);
        return jl_box_float64(x);
    }    

Registering Julia C Functions
-----------------------------------------

In order to make the Julia callable function accessible from Julia, we have to add it to the current module scope. This can be done by calling::

    jl_add_new_closure(jl_current_module, my_c_sqrt, "my_c_sqrt");

Now we can use ``my_c_sqrt`` in Julia::

    jl_eval_string("println( my_c_sqrt(2.0) )");

Exceptions
===========

One important question is what happens if Julia is throwing an exception. This can be for instance tested by calling::

      jl_eval_string("this_function_does_not_exist()");

As one can verify nothing happens. This is of course very problematic as such silent errors are very hard to debug. The solution is, to ask Julia whether an exception has been thrown::

    if (jl_exception_occurred())
        printf("%s \n", jl_get_exception_str( jl_exception_occurred() ) );

If you are using the Julia C API from a higher level programming language (Python, C#, C++) that supports exceptions, it makes a lot of sense to wrap each call into libjulia into a function which

- First checks, whether an error has occurred
- Then throws an exception in the programming language used


Throwing Julia Exceptions
-----------------------------------------

When writting Julia callable functions, one has to check the input arguments for their type and dimensionality.
If the type or dimensionality is wrong we somehow have to tell Julia that an error occurred. This can be done by throwing a Julia exception. A typical dimension check looks like::

    if (!jl_is_float64(args[0])) {
        jl_type_error(function_name, (jl_value_t*)jl_float64_type, args[0]);
    }

Here ``args`` is input argument array (``jl_value_t**``). To shorten these type checks, there is a macro that can be used as::

   JL_TYPECHK(function_name, float64, args[0])

When to few or to many arguments are passed to the function one can throw the following exceptions::

    if (nargs < min) 
        jl_too_few_args(function_name, min);
    else if (nargs > max) 
        jl_too_many_args(function_name, max);

or equivalently ``JL_NARGS(function_name,min,max)``. General exception that are not type or argument related can be raised using the funtions::

    void jl_error(const char *str);
    void jl_errorf(const char *fmt, ...);

While ``jl_error`` takes a simple C string, ``jl_errorf`` can be used like a ``printf`` function with variable arguments::

    jl_errorf("An error occurred as x = %d is to large", x);

where in this example ``x`` is assumed to be an integer.

