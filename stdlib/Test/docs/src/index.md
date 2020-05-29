# Unit Testing

```@meta
DocTestSetup = :(using Test)
```

## Testing Base Julia

Julia is under rapid development and has an extensive test suite to verify functionality across
multiple platforms. If you build Julia from source, you can run this test suite with `make test`.
In a binary install, you can run the test suite using `Base.runtests()`.

```@docs
Base.runtests
```

## Basic Unit Tests

The `Test` module provides simple *unit testing* functionality. Unit testing is a way to
see if your code is correct by checking that the results are what you expect. It can be helpful
to ensure your code still works after you make changes, and can be used when developing as a way
of specifying the behaviors your code should have when complete.

Simple unit testing can be performed with the `@test` and `@test_throws` macros:

```@docs
Test.@test
Test.@test_throws
```

For example, suppose we want to check our new function `foo(x)` works as expected:

```jldoctest testfoo
julia> using Test

julia> foo(x) = length(x)^2
foo (generic function with 1 method)
```

If the condition is true, a `Pass` is returned:

```jldoctest testfoo
julia> @test foo("bar") == 9
Test Passed

julia> @test foo("fizz") >= 10
Test Passed
```

If the condition is false, then a `Fail` is returned and an exception is thrown:

```jldoctest testfoo
julia> @test foo("f") == 20
Test Failed at none:1
  Expression: foo("f") == 20
   Evaluated: 1 == 20
ERROR: There was an error during testing
```

If the condition could not be evaluated because an exception was thrown, which occurs in this
case because `length` is not defined for symbols, an `Error` object is returned and an exception
is thrown:

```julia-repl
julia> @test foo(:cat) == 1
Error During Test
  Test threw an exception of type MethodError
  Expression: foo(:cat) == 1
  MethodError: no method matching length(::Symbol)
  Closest candidates are:
    length(::SimpleVector) at essentials.jl:256
    length(::Base.MethodList) at reflection.jl:521
    length(::MethodTable) at reflection.jl:597
    ...
  Stacktrace:
  [...]
ERROR: There was an error during testing
```

If we expect that evaluating an expression *should* throw an exception, then we can use `@test_throws`
to check that this occurs:

```jldoctest testfoo
julia> @test_throws MethodError foo(:cat)
Test Passed
      Thrown: MethodError
```

## Working with Test Sets

Typically a large number of tests are used to make sure functions work correctly over a range
of inputs. In the event a test fails, the default behavior is to throw an exception immediately.
However, it is normally preferable to run the rest of the tests first to get a better picture
of how many errors there are in the code being tested.

The `@testset` macro can be used to group tests into *sets*. All the tests in a test set will
be run, and at the end of the test set a summary will be printed. If any of the tests failed,
or could not be evaluated due to an error, the test set will then throw a `TestSetException`.

```@docs
Test.@testset
```

We can put our tests for the `foo(x)` function in a test set:

```jldoctest testfoo
julia> @testset "Foo Tests" begin
           @test foo("a")   == 1
           @test foo("ab")  == 4
           @test foo("abc") == 9
       end;
Test Summary: | Pass  Total
Foo Tests     |    3      3
```

Test sets can also be nested:

```jldoctest testfoo
julia> @testset "Foo Tests" begin
           @testset "Animals" begin
               @test foo("cat") == 9
               @test foo("dog") == foo("cat")
           end
           @testset "Arrays $i" for i in 1:3
               @test foo(zeros(i)) == i^2
               @test foo(fill(1.0, i)) == i^2
           end
       end;
Test Summary: | Pass  Total
Foo Tests     |    8      8
```

In the event that a nested test set has no failures, as happened here, it will be hidden in the
summary. If we do have a test failure, only the details for the failed test sets will be shown:

```julia-repl
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
               @test foo(fill(1.0, 4)) == 15
           end
       end

Arrays: Test Failed
  Expression: foo(fill(1.0, 4)) == 15
   Evaluated: 16 == 15
[...]
Test Summary: | Pass  Fail  Total
Foo Tests     |    3     1      4
  Animals     |    2            2
  Arrays      |    1     1      2
ERROR: Some tests did not pass: 3 passed, 1 failed, 0 errored, 0 broken.
```

## Other Test Macros

As calculations on floating-point values can be imprecise, you can perform approximate equality
checks using either `@test a ≈ b` (where `≈`, typed via tab completion of `\approx`, is the
[`isapprox`](@ref) function) or use [`isapprox`](@ref) directly.

```jldoctest
julia> @test 1 ≈ 0.999999999
Test Passed

julia> @test 1 ≈ 0.999999
Test Failed at none:1
  Expression: 1 ≈ 0.999999
   Evaluated: 1 ≈ 0.999999
ERROR: There was an error during testing
```

```@docs
Test.@inferred
Test.@test_logs
Test.@test_deprecated
Test.@test_warn
Test.@test_nowarn
```

## Broken Tests

If a test fails consistently it can be changed to use the `@test_broken` macro. This will denote
the test as `Broken` if the test continues to fail and alerts the user via an `Error` if the test
succeeds.

```@docs
Test.@test_broken
```

`@test_skip` is also available to skip a test without evaluation, but counting the skipped test
in the test set reporting. The test will not run but gives a `Broken` `Result`.

```@docs
Test.@test_skip
```

## Creating Custom `AbstractTestSet` Types

Packages can create their own `AbstractTestSet` subtypes by implementing the `record` and `finish`
methods. The subtype should have a one-argument constructor taking a description string, with
any options passed in as keyword arguments.

```@docs
Test.record
Test.finish
```

`Test` takes responsibility for maintaining a stack of nested testsets as they are executed,
but any result accumulation is the responsibility of the `AbstractTestSet` subtype. You can access
this stack with the `get_testset` and `get_testset_depth` methods. Note that these functions are
not exported.

```@docs
Test.get_testset
Test.get_testset_depth
```

`Test` also makes sure that nested `@testset` invocations use the same `AbstractTestSet`
subtype as their parent unless it is set explicitly. It does not propagate any properties of the
testset. Option inheritance behavior can be implemented by packages using the stack infrastructure
that `Test` provides.

Defining a basic `AbstractTestSet` subtype might look like:

```julia
import Test: Test, record, finish
using Test: AbstractTestSet, Result, Pass, Fail, Error
using Test: get_testset_depth, get_testset
struct CustomTestSet <: Test.AbstractTestSet
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

```@meta
DocTestSetup = nothing
```
