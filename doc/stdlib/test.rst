**************
 Unit Testing
**************

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

Basic Unit Tests
----------------

The ``Base.Test`` module provides simple *unit testing* functionality.
Unit testing is a way to see if your code is correct by checking that
the results are what you expect. It can be helpful to ensure your code
still works after you make changes, and can be used when developing as
a way of specifying the behaviors your code should have when complete.

Simple unit testing can be performed with the :func:`@test` and
:func:`@test_throws` macros:

.. function:: @test ex

   .. Docstring generated from Julia source

   Tests that the expression ``ex`` evaluates to ``true``\ . Returns a ``Pass`` ``Result`` if it does, a ``Fail`` ``Result`` if it is ``false``\ , and an ``Error`` ``Result`` if it could not be evaluated.

.. function:: @test_throws extype ex

   .. Docstring generated from Julia source

   Tests that the expression ``ex`` throws an exception of type ``extype``\ .

For example, suppose we want to check our new function ``foo(x)`` works
as expected::

    julia> using Base.Test

    julia> foo(x) = length(x)^2
    foo (generic function with 1 method)

If the condition is true, a ``Pass`` is returned::

    julia> @test foo("bar") == 9
    Test Passed
      Expression: foo("bar") == 9
       Evaluated: 9 == 9

    julia> @test foo("fizz") >= 10
    Test Passed
      Expression: foo("fizz") >= 10
       Evaluated: 16 >= 10

If the condition is false, then a ``Fail`` is returned and an
exception is thrown::

    julia> @test foo("f") == 20
    Test Failed
      Expression: foo("f") == 20
       Evaluated: 1 == 20
    ERROR: There was an error during testing
     in record at test.jl:268
     in do_test at test.jl:191

If the condition could not be evaluated because an exception was thrown,
which occurs in this case because :func:`length` is not defined for
symbols, an ``Error`` object is returned and an exception is thrown::

    julia> @test foo(:cat) == 1
    Error During Test
      Test threw an exception of type MethodError
      Expression: foo(:cat) == 1
      MethodError: `length` has no method matching length(::Symbol)
       in foo at none:1
       in anonymous at test.jl:159
       in do_test at test.jl:180
    ERROR: There was an error during testing
     in record at test.jl:268
     in do_test at test.jl:191

If we expect that evaluating an expression *should* throw an exception,
then we can use :func:`@test_throws` to check this occurs::

    julia> @test_throws MethodError foo(:cat)
    Test Passed
      Expression: foo(:cat)
       Evaluated: MethodError


Working with Test Sets
----------------------

Typically a large of number of tests are used to make sure functions
work correctly over a range of inputs. In the event a test fails, the
default behavior is to throw an exception immediately. However, it is
normally preferrable to run the rest of the tests first to get a
better picture of how many errors there are in the code being tested.

The :func:`@testset` and :func:`@testloop` macros can be used to
group tests into *sets*. All the tests in a test set will be run,
and at the end of the test set a summary will be printed. If any of
the tests failed, or could not be evaluated due to an error, the
test set will then throw a ``TestSetException``.


.. function:: @testset "description" let ... end
              @testset let ... end

   .. Docstring generated from Julia source

   Starts a new test set. The test results will be recorded, and if there are any ``Fail``\ s or ``Error``\ s, an exception will be thrown only at the end, along with a summary of the test results. Wrapping your test in a ``let`` block creates a new scope that isolates your tests from other test sets.

.. function:: @testloop "description $v" for v in (...) ... end
              @testloop for x in (...), y in (...) ... end

   .. Docstring generated from Julia source

   Starts a new test set for each iteration of the loop. The description string accepts interpolation from the loop indices. If no description is provided, one is constructed based on the variables.

We can put our tests for the ``foo(x)`` function in a test set::

    julia> @testset "Foo Tests" let
               @test foo("a")   == 1
               @test foo("ab")  == 4
               @test foo("abc") == 9
           end
    Test Summary: | Pass  Total
    Foo Tests     |    3      3

Test sets can all also be nested::

    julia> @testset "Foo Tests" let
               @testset "Animals" let
                   @test foo("cat") == 9
                   @test foo("dog") == foo("cat")
               end
               @testloop "Arrays $i" for i in 1:3
                   @test foo(zeros(i)) == i^2
                   @test foo(ones(i)) == i^2
               end
           end
    Test Summary: | Pass  Total
    Foo Tests     |    8      8

In the event that a nested test set has no failures, as happened here,
it will be hidden in the summary. If we do have a test failure, only
the details for the failed test sets will be shown::

    julia> @testset "Foo Tests" let
               @testset "Animals" let
                   @testset "Felines" let
                       @test foo("cat") == 9
                   end
                   @testset "Canines" let
                       @test foo("dog") == 9
                   end
               end
               @testset "Arrays" let
                   @test foo(zeros(2)) == 4
                   @test foo(ones(4)) == 15
               end
           end

    Arrays: Test Failed
      Expression: foo(ones(4)) == 15
       Evaluated: 16 == 15
     in record at test.jl:297
     in do_test at test.jl:191
    Test Summary: | Pass  Fail  Total
    Foo Tests     |    3     1      4
      Animals     |    2            2
      Arrays      |    1     1      2
    ERROR: Some tests did not pass: 3 passed, 1 failed, 0 errored.
     in finish at test.jl:362


Other Test Macros
-----------------

As calculations on floating-point values can be imprecise, you can
perform approximate equality checks using either ``@test a ≈ b``
(where ``≈``, typed via tab completion of ``\approx``,
is the :func:`isapprox` function) or use :func:`isapprox` directly.

An alternative is the ``@test_approx_eq`` macro (which differs from
``isapprox`` in that it treats NaN values as equal and has a smaller
default tolerance) or ``@test_approx_eq_eps`` (which takes an extra
argument indicating the relative tolerance)::

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

Note that these macros will fail immediately, and are not compatible
with :func:`@testset`, so using `@test isapprox` is encouraged when
writing new tests.

.. function:: @test_approx_eq(a, b)

   .. Docstring generated from Julia source

   Test two floating point numbers ``a`` and ``b`` for equality taking in account small numerical errors.

.. function:: @test_approx_eq_eps(a, b, tol)

   .. Docstring generated from Julia source

   Test two floating point numbers ``a`` and ``b`` for equality taking in account a margin of tolerance given by ``tol``\ .

