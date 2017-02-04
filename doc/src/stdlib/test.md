# Unit Testing

## Testing Base Julia

Julia is under rapid development and has an extensive test suite to verify functionality across
multiple platforms. If you build Julia from source, you can run this test suite with `make test`.
In a binary install, you can run the test suite using `Base.runtests()`.

```@docs
Base.runtests
```

## Basic Unit Tests

The `Base.Test` module provides simple *unit testing* functionality. Unit testing is a way to
see if your code is correct by checking that the results are what you expect. It can be helpful
to ensure your code still works after you make changes, and can be used when developing as a way
of specifying the behaviors your code should have when complete.

Simple unit testing can be performed with the `@test()` and `@test_throws()` macros:

```@docs
Base.Test.@test
Base.Test.@test_throws
```

For example, suppose we want to check our new function `foo(x)` works as expected:

```julia
julia> using Base.Test

julia> foo(x) = length(x)^2
foo (generic function with 1 method)
```

If the condition is true, a `Pass` is returned:

```julia
julia> @test foo("bar") == 9
Test Passed
  Expression: foo("bar") == 9
   Evaluated: 9 == 9

julia> @test foo("fizz") >= 10
Test Passed
  Expression: foo("fizz") >= 10
   Evaluated: 16 >= 10
```

If the condition is false, then a `Fail` is returned and an exception is thrown:

```julia
julia> @test foo("f") == 20
Test Failed
  Expression: foo("f") == 20
   Evaluated: 1 == 20
ERROR: There was an error during testing
 in record at test.jl:268
 in do_test at test.jl:191
```

If the condition could not be evaluated because an exception was thrown, which occurs in this
case because `length()` is not defined for symbols, an `Error` object is returned and an exception
is thrown:

```julia
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
```

If we expect that evaluating an expression *should* throw an exception, then we can use `@test_throws()`
to check that this occurs:

```julia
julia> @test_throws MethodError foo(:cat)
Test Passed
  Expression: foo(:cat)
   Evaluated: MethodError
```

## Working with Test Sets

Typically a large of number of tests are used to make sure functions work correctly over a range
of inputs. In the event a test fails, the default behavior is to throw an exception immediately.
However, it is normally preferable to run the rest of the tests first to get a better picture
of how many errors there are in the code being tested.

The `@testset()` macro can be used to group tests into *sets*. All the tests in a test set will
be run, and at the end of the test set a summary will be printed. If any of the tests failed,
or could not be evaluated due to an error, the test set will then throw a `TestSetException`.

```@docs
Base.Test.@testset
```

We can put our tests for the `foo(x)` function in a test set:

```julia
julia> @testset "Foo Tests" begin
           @test foo("a")   == 1
           @test foo("ab")  == 4
           @test foo("abc") == 9
       end
Test Summary: | Pass  Total
Foo Tests     |    3      3
```

Test sets can also be nested:

```julia
julia> @testset "Foo Tests" begin
           @testset "Animals" begin
               @test foo("cat") == 9
               @test foo("dog") == foo("cat")
           end
           @testset "Arrays $i" for i in 1:3
               @test foo(zeros(i)) == i^2
               @test foo(ones(i)) == i^2
           end
       end
Test Summary: | Pass  Total
Foo Tests     |    8      8
```

In the event that a nested test set has no failures, as happened here, it will be hidden in the
summary. If we do have a test failure, only the details for the failed test sets will be shown:

```julia
julia> @testset "Foo Tests" begin
           @testset "Animals" begin
               @testset "Felines" begin
                   @test foo("cat") == 9
               end
               @testset "Canines" begin
                   @test foo("dog") == 9
               end
           end
           @testset "Arrays" begin
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
ERROR: Some tests did not pass: 3 passed, 1 failed, 0 errored, 0 broken.
 in finish at test.jl:362
```

## Other Test Macros

As calculations on floating-point values can be imprecise, you can perform approximate equality
checks using either `@test a ≈ b` (where `≈`, typed via tab completion of `\approx`, is the
[`isapprox()`](@ref) function) or use [`isapprox()`](@ref) directly.

```julia
julia> @test 1 ≈ 0.999999999

julia> @test 1 ≈ 0.999999
ERROR: test failed: 1 isapprox 0.999999
 in expression: 1 ≈ 0.999999
 in error at error.jl:21
 in default_handler at test.jl:30
 in do_test at test.jl:53
```

```@docs
Base.Test.@inferred
Base.Test.@test_warn
Base.Test.@test_nowarn
```

## Broken Tests

If a test fails consistently it can be changed to use the `@test_broken()` macro. This will denote
the test as `Broken` if the test continues to fail and alerts the user via an `Error` if the test
succeeds.

```@docs
Base.Test.@test_broken
```

`@test_skip()` is also available to skip a test without evaluation, but counting the skipped test
in the test set reporting. The test will not run but gives a `Broken` `Result`.

```@docs
Base.Test.@test_skip
```

## Creating Custom `AbstractTestSet` Types

Packages can create their own `AbstractTestSet` subtypes by implementing the `record` and `finish`
methods. The subtype should have a one-argument constructor taking a description string, with
any options passed in as keyword arguments.

```@docs
Base.Test.record
Base.Test.finish
```

`Base.Test` takes responsibility for maintaining a stack of nested testsets as they are executed,
but any result accumulation is the responsibility of the `AbstractTestSet` subtype. You can access
this stack with the `get_testset` and `get_testset_depth` methods. Note that these functions are
not exported.

```@docs
Base.Test.get_testset
Base.Test.get_testset_depth
```

`Base.Test` also makes sure that nested `@testset` invocations use the same `AbstractTestSet`
subtype as their parent unless it is set explicitly. It does not propagate any properties of the
testset. Option inheritance behavior can be implemented by packages using the stack infrastructure
that `Base.Test` provides.

Defining a basic `AbstractTestSet` subtype might look like:

```julia
import Base.Test: record, finish
using Base.Test: AbstractTestSet, Result, Pass, Fail, Error
using Base.Test: get_testset_depth, get_testset
immutable CustomTestSet <: Base.Test.AbstractTestSet
    description::AbstractString
    foo::Int
    results::Vector
    # constructor takes a description string and options keyword arguments
    CustomTestSet(desc; foo=1) = new(desc, foo, [])
end

record(ts::CustomTestSet, child::AbstractTestSet) = push!(ts.results, child)
record(ts::CustomTestSet, res::Result) = push!(ts.results, res)
function finish(ts::CustomTestSet)
    # just record if we're not the top-level parent
    if get_testset_depth() > 0
        record(get_testset(), ts)
    end
    ts
end
```

And using that testset looks like:

```julia
@testset CustomTestSet foo=4 "custom testset inner 2" begin
    # this testset should inherit the type, but not the argument.
    @testset "custom testset inner" begin
        @test true
    end
end
```
