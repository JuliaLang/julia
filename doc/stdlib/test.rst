.. module:: Base.Test

Unit and Functional Testing
===========================

The ``Test`` module contains macros and functions related to testing.
A default handler is provided to run the tests, and a custom one can be
provided by the user by using the :func:`registerhandler` function.


Overview
________

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

  julia> @test_throws BoundsError error("An error")
  ERROR: test failed: error("An error")
   in error at error.jl:21
   in default_handler at test.jl:19
   in do_test_throws at test.jl:55

  julia> @test_throws DomainError throw(DomainError())

  julia> @test_throws DomainError throw(EOFError())
  ERROR: test failed: throw(EOFError())
   in error at error.jl:21
   in default_handler at test.jl:19
   in do_test_throws at test.jl:55


As floating point comparisons can be imprecise, two additional macros exist taking in account small numerical errors::

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
________

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
______

.. function:: @test(ex)

   Test the expression ``ex`` and calls the current handler to handle the result.

.. function:: @test_throws(extype, ex)

   Test that the expression ``ex`` throws an exception of type ``extype`` and calls the current handler to handle the result.

.. function:: @test_approx_eq(a, b)

   Test two floating point numbers ``a`` and ``b`` for equality taking in account
   small numerical errors.

.. function:: @test_approx_eq_eps(a, b, tol)

   Test two floating point numbers ``a`` and ``b`` for equality taking in account
   a margin of tolerance given by ``tol``.

Functions
_________

.. function:: with_handler(f, handler)

   Run the function ``f`` using the ``handler`` as the handler.


Testing Base Julia
==================

Julia is under rapid development and has an extensive test suite to
verify functionality across multiple platforms. If you build Julia
from source, you can run this test suite with ``make test``. In a
binary install, you can run the test suite using ``Base.runtests()``.

.. currentmodule:: Base

.. function:: runtests([tests=["all"] [, numcores=iceil(CPU_CORES/2) ]])

   Run the Julia unit tests listed in ``tests``, which can be either a
   string or an array of strings, using ``numcores`` processors.
