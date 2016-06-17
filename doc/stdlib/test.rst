*****************************
 Unit and Functional Testing
*****************************

Testing Base Julia
------------------

Julia is under rapid development and has an extensive test suite to
verify functionality across multiple platforms. If you build Julia
from source, you can run this test suite with ``make test``. In a
binary install, you can run the test suite using ``Base.runtests()``.

.. currentmodule:: Base

.. function:: runtests([tests=["all"] [, numcores=iceil(CPU_CORES/2) ]])

   .. Docstring generated from Julia source

   Run the Julia unit tests listed in ``tests``\ , which can be either a string or an array of strings, using ``numcores`` processors. (not exported)

.. module:: Base.Test

Test Framework
--------------

The ``Test`` module contains macros and functions related to testing.
A default handler is provided to run the tests, and a custom one can be
provided by the user by using the :func:`registerhandler` function.

To use the default handler, the macro :func:`@test` can be used directly::

  julia> using Base.Test

  julia> @test 1 == 1

  julia> @test 1 == 0
  ERROR: test failed: 1 == 0
   in error at error.jl:21
   in default_handler at test.jl:19
   in do_test at test.jl:39

  julia> @test error("This is what happens when a test fails")
  ERROR: test error during error("This is what happens when a test fails")
  This is what happens when a test fails
   in error at error.jl:21
   in anonymous at test.jl:62
   in do_test at test.jl:37

As seen in the examples above, failures or errors will print the abstract
syntax tree of the expression in question.

Another macro is provided to check if the given expression throws an exception of type ``extype``,
:func:`@test_throws`::

  julia> @test_throws ErrorException error("An error")
  ErrorException("An error")

  julia> @test_throws BoundsError error("An error")
  ERROR: test failed: error("An error")
   in error at error.jl:21
   in default_handler at test.jl:19
   in do_test_throws at test.jl:55

  julia> @test_throws DomainError throw(DomainError())
  DomainError()

  julia> @test_throws DomainError throw(EOFError())
  ERROR: test failed: throw(EOFError())
   in error at error.jl:21
   in default_handler at test.jl:19
   in do_test_throws at test.jl:55


As floating-point values can be imprecise, you can perform approximate
equality checks using either ``@test a ≈ b`` (where ``≈``, typed via
tab completion of ``\approx``, is the ``isapprox`` function) or use
the macros ``@test_approx_eq`` macro (which differs from ``isapprox``
in that it treats NaN values as equal and has a smaller default
tolerance) or ``@test_approx_eq_eps`` (which takes an extra argument
indicating the relative tolerance)::

  julia> @test 1 ≈ 0.999999999

  julia> @test 1 ≈ 0.999999
  ERROR: test failed: 1 isapprox 0.999999
   in expression: 1 ≈ 0.999999
   in error at error.jl:21
   in default_handler at test.jl:30
   in do_test at test.jl:53

  julia> @test_approx_eq 1. 0.999999999
  ERROR: assertion failed: |1.0 - 0.999999999| < 2.220446049250313e-12
    1.0 = 1.0
    0.999999999 = 0.999999999
   in test_approx_eq at test.jl:75
   in test_approx_eq at test.jl:80

  julia> @test_approx_eq 1. 0.9999999999999

  julia> @test_approx_eq_eps 1. 0.999 1e-2

  julia> @test_approx_eq_eps 1. 0.999 1e-3
  ERROR: assertion failed: |1.0 - 0.999| <= 0.001
    1.0 = 1.0
    0.999 = 0.999
    difference = 0.0010000000000000009 > 0.001
   in error at error.jl:22
   in test_approx_eq at test.jl:68

Handlers
--------

A handler is a function defined for three kinds of arguments: ``Success``, ``Failure``, ``Error``::

  # An example definition of a test handler
  test_handler(r::Success) = nothing
  test_handler(r::Failure) = error("test failed: $(r.expr)")
  test_handler(r::Error)   = rethrow(r)

A different handler can be used for a block (with :func:`with_handler`)::

  julia> using Base.Test

  julia> custom_handler(r::Test.Success) = println("Success on $(r.expr)")
  custom_handler (generic function with 1 method)

  julia> custom_handler(r::Test.Failure) = error("Error on custom handler: $(r.expr)")
  custom_handler (generic function with 2 methods)

  julia> custom_handler(r::Test.Error) = rethrow(r)
  custom_handler (generic function with 3 methods)

  julia> Test.with_handler(custom_handler) do
           @test 1 == 1
           @test 1 != 1
         end
  Success on :((1==1))
  ERROR: Error on custom handler: :((1!=1))
   in error at error.jl:21
   in custom_handler at none:1
   in do_test at test.jl:39
   in anonymous at no file:3
   in task_local_storage at task.jl:28
   in with_handler at test.jl:24

The ``Success`` and ``Failure`` types include an additonal field, ``resultexpr``, which is a partially evaluated expression. For example, in a comparison it will contain an expression with the left and right sides evaluated.

Macros
------

.. function:: @test(ex)

   .. Docstring generated from Julia source

   Test the expression ``ex`` and calls the current handler to handle the result.

.. function:: @test_throws(extype, ex)

   .. Docstring generated from Julia source

   Test that the expression ``ex`` throws an exception of type ``extype`` and calls the current handler to handle the result.

.. function:: @test_approx_eq(a, b)

   .. Docstring generated from Julia source

   Test two floating point numbers ``a`` and ``b`` for equality taking in account small numerical errors.

.. function:: @test_approx_eq_eps(a, b, tol)

   .. Docstring generated from Julia source

   Test two floating point numbers ``a`` and ``b`` for equality taking in account a margin of tolerance given by ``tol``\ .

.. function:: @inferred f(x)

   .. Docstring generated from Julia source

   Tests that the call expression ``f(x)`` returns a value of the same type inferred by the compiler. It's useful to check for type stability.

   ``f(x)`` can be any call expression. Returns the result of ``f(x)`` if the types match, and an ``Error`` ``Result`` if it finds different types.

   .. doctest::

       julia> using Base.Test

       julia> f(a,b,c) = b > 1 ? 1 : 1.0
       f (generic function with 1 method)

       julia> typeof(f(1,2,3))
       Int64

       julia> @code_warntype f(1,2,3)
       Variables:
         a::Int64
         b::Int64
         c::Int64
       <BLANKLINE>
       Body:
         begin  # none, line 1:
             unless (Base.slt_int)(1,b::Int64)::Bool goto 0
             return 1
             0:
             return 1.0
         end::UNION{FLOAT64,INT64}

       julia> @inferred f(1,2,3)
       ERROR: return type Int64 does not match inferred return type Union{Float64,Int64}
        in error at ./error.jl:21

       julia> @inferred max(1,2)
       2

Functions
---------

.. function:: with_handler(f, handler)

   .. Docstring generated from Julia source

   Run the function ``f`` using the ``handler`` as the handler.

