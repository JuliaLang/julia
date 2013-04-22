:mod:`Base.Test` --- Routines related to testing
------------------------------------------------

.. module:: Base.Test
   :synopsis: Test and related routines

The `Test` module contains macros and functions related to testing. 
A default handler is provided to run the tests, and a custom one can be
provided by the user by using the :func:`registerhandler` function.


Overview
________

To use the default handler, the macro :func:`\@test` can be used directly::

  # Julia code
  julia> @test 1 == 1

  julia> @test 1 == 0
  ERROR: test failed: :((1==0))
   in default_handler at test.jl:20
   in do_test at test.jl:37

  julia> @test error("This is what happens when a test fails")
  ERROR: test error during :(error("This is what happens when a test fails"))
  This is what happens when a test fails
   in error at error.jl:21
   in anonymous at test.jl:62
   in do_test at test.jl:35

As seen in the examples above, failures or errors will print the abstract
syntax tree of the expression in question.

Another macro is provided to check if the given expression throws an error,
:func:`@test_fails`::

  julia> @test_fails error("An error")

  julia> @test_fails 1 == 1
  ERROR: test failed: :((1==1))
   in default_handler at test.jl:20
   in do_test_fails at test.jl:46

  julia> @test_fails 1 != 1
  ERROR: test failed: :((1!=1))
   in default_handler at test.jl:20
   in do_test_fails at test.jl:46

As floating point comparisons can be imprecise, two additional macros exist taking in account small numerical errors::

  julia> @test_approx_eq 1. 0.999999999
  ERROR: assertion failed: |1.0 - 0.999999999| < 2.220446049250313e-12
    1.0 = 1.0
    0.999999999 = 0.999999999
   in test_approx_eq at test.jl:75
   in test_approx_eq at test.jl:80

  julia> @test_approx_eq 1. 0.9999999999999

  julia> @test_approx_eq_eps 1. 0.999 e-2

  julia> @test_approx_eq_eps 1. 0.999 e-3
  ERROR: assertion failed: |1.0 - 0.999| < -0.2817181715409549
    1.0 = 1.0
    0.999 = 0.999
   in test_approx_eq at test.jl:75

Handlers
________

A handler is a function defined for three kinds of arguments: `Success`, `Failure`, `Error`::

  # The definition of the default handler
  default_handler(r::Success) = nothing
  default_handler(r::Failure) = error("test failed: $(r.expr)")
  default_handler(r::Error)   = rethrow(r)

A different handler can be used for a block (with :func:`withhandler`)::

  julia> handler(r::Success) = println("Success on $(r.expr)")
  # methods for generic function handler
  handler(r::Success) at none:1

  julia> handler(r::Failure) = error("Error on custom handler: $(r.expr)")
  # methods for generic function handler
  handler(r::Success) at none:1
  handler(r::Failure) at none:1

  julia> handler(r::Error)   = rethrow(r)
  # methods for generic function handler
  handler(r::Success) at none:1
  handler(r::Failure) at none:1
  handler(r::Error) at none:1

  julia> withhandler(handler) do
           @test 1 == 1
           @test 1 != 1
         end
  Success on :((1==1))
  ERROR: Error on custom handler: :((1!=1))
   in handler at none:1
   in do_test at test.jl:38
   in anonymous at no file:3
   in withhandler at test.jl:57

or globally redefined (with :func:`registerhandler`)::

  julia> registerhandler(handler)
  # methods for generic function handler
  handler(r::Success) at none:1
  handler(r::Failure) at none:1
  handler(r::Error) at none:1

  julia> @test 1 == 1
  Success on :((1==1))

Macros
______

.. function:: @test ex

   Test the expression `ex` and calls the current handler to handle the result.

.. function:: @test_fails ex

   Test the expression `ex` and calls the current handler to handle the result in the following manner:

   * If the test doesn't throw an error, the `Failure` case is called.
   * If the test throws an error, the `Success` case is called.

.. function:: @test_approx_eq a b

   Test two floating point numbers `a` and `b` for equality taking in account
   small numerical errors.

.. function:: @test_approx_eq_eps a b c
   
   Test two floating point numbers `a` and `b` for equality taking in account
   a margin of tolerance given by `c`.

Functions
_________
.. function:: registerhandler(handler)
   
   Change the handler function used globally to `handler`.

.. function:: withhandler(f, handler)

   Run the function `f` using the `handler` as the handler.
