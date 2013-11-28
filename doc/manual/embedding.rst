.. _man-embedding:

**************************
 Embedding Julia
**************************

As we have seen (ref:`man-calling-c-and-fortran-code`) Julia has a very simple and efficient way to call functions that are written in the C programming language. But there are various situations where actually the opposite is needed: calling Julia function from C code. This can for instance be used to integrate code that has been prototyped in Julia into a larger C/C++ project, without the need to rewrite everything into C/C++. To make this possible Julia features a C API that can be used to embed Julia into a C/C++ program. As almost all programming languages have some way to call C functions, the Julia C-API can also be used to build further language bridges (E.g. calling julia from Python or C#). 


High-Level Embedding
=====================

We start with a very simple C program that initializes Julia and calls some Julia code without the need to share data between Julia and C::

  #include <julia.h>

  int main(int argc, char *argv[])
  {
    jl_init("/Applications/Julia-0.2.0.app/Contents/Resources/julia/lib/");
    jl_eval_string("print(pi/2)");

    return 0;
  }

In order to build this program you have to put the path to the Julia header into the include path and link against libjulia. For instance, when Julia is installed to $JULIA_DIR, one can compile the above test program test.c with gcc as::

    gcc -o test -I$JULIA_DIR/include/julia -L$JULIA_DIR/usr/lib -ljulia test.c

The first thing that has do be done before calling any other Julia C function is to initialize Julia. This is done by calling ``jl_init``, which takes as argument as String to the location where Julia is installed. The second statement in the test program evaluates a Julia statement using a call to ``jl_eval_string``.

Calling Julia Functions
========================

While it is very nice to be able to execute a command in the Julia interpreter, it would be even more interesting to return the value of the statement to the host program. As Julia is a dynamically typed language and C is a statically typed language, we have to convert data between the type systems. Converting C values into Julia values is called boxing, while converting the other way around is called unboxing. Our improved sample program that calculates pi/2 in Julia and reads back the result in C looks as follows::

    jl_value_t *ret = jl_eval_string("pi/2");

    if(jl_is_float64(ret))
    {
        double ret_unboxed = jl_unbox_float64(ret);
        printf("Pi by 2 in C: %e \n", ret_unboxed);
    }

The return value of ``jl_eval_string`` is a pointer of type ``jl_value_t*``. This is the C type that holds Julia values of any type. In order to check whether ``ret`` is of a specific C type, we can use the ``jl_is_...`` functions. By typing ``typeof(pi/2)`` into the Julia shell we can see that the return type us float64 (i.e. double). To convert the boxed Julia value into a C double we use the ``jl_unbox_float64`` function.


