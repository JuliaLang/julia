```@meta
EditURL = "https://github.com/JuliaLang/julia/blob/master/stdlib/Test/docs/src/index.md"
```

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
of specifying the behaviors your code should have when complete. You may also want to look at the
documentation for [adding tests to your Julia Package](https://pkgdocs.julialang.org/dev/creating-packages/#adding-tests-to-packages).

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
  The function `length` exists, but no method is defined for this combination of argument types.

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

!!! note
    The `@testset` will create a local scope of its own when running the tests in it.

The `@testset` macro can be used to group tests into *sets*. All the tests in a test set will
be run, and at the end of the test set a summary will be printed. If any of the tests failed,
or could not be evaluated due to an error, the test set will then throw a `TestSetException`.

```@docs
Test.@testset
Test.TestSetException
```

We can put our tests for the `foo(x)` function in a test set:

```jldoctest testfoo; filter = r"[0-9\.]+s"
julia> @testset "Foo Tests" begin
           @test foo("a")   == 1
           @test foo("ab")  == 4
           @test foo("abc") == 9
       end;
Test Summary: | Pass  Total  Time
Foo Tests     |    3      3  0.0s
```

Test sets can also be nested:

```jldoctest testfoo; filter = r"[0-9\.]+s"
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
Test Summary: | Pass  Total  Time
Foo Tests     |    8      8  0.0s
```

As well as call functions:

```jldoctest testfoo; filter = r"[0-9\.]+s"
julia> f(x) = @test isone(x)
f (generic function with 1 method)

julia> @testset f(1);
Test Summary: | Pass  Total  Time
f             |    1      1  0.0s
```

This can be used to allow for factorization of test sets, making it easier to run individual
test sets by running the associated functions instead.
Note that in the case of functions, the test set will be given the name of the called function.
In the event that a nested test set has no failures, as happened here, it will be hidden in the
summary, unless the `verbose=true` option is passed:

```jldoctest testfoo; filter = r"[0-9\.]+s"
julia> @testset verbose = true "Foo Tests" begin
           @testset "Animals" begin
               @test foo("cat") == 9
               @test foo("dog") == foo("cat")
           end
           @testset "Arrays $i" for i in 1:3
               @test foo(zeros(i)) == i^2
               @test foo(fill(1.0, i)) == i^2
           end
       end;
Test Summary: | Pass  Total  Time
Foo Tests     |    8      8  0.0s
  Animals     |    2      2  0.0s
  Arrays 1    |    2      2  0.0s
  Arrays 2    |    2      2  0.0s
  Arrays 3    |    2      2  0.0s
```

If we do have a test failure, only the details for the failed test sets will be shown:

```julia-repl; filter = r"[0-9\.]+s"
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
Test Summary: | Pass  Fail  Total  Time
Foo Tests     |    3     1      4  0.0s
  Animals     |    2            2  0.0s
  Arrays      |    1     1      2  0.0s
ERROR: Some tests did not pass: 3 passed, 1 failed, 0 errored, 0 broken.
```

## Testing Log Statements

One can use the [`@test_logs`](@ref) macro to test log statements, or use a [`TestLogger`](@ref).

```@docs
Test.@test_logs
Test.TestLogger
Test.LogRecord
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

ERROR: There was an error during testing
```
You can specify relative and absolute tolerances by setting the `rtol` and `atol` keyword arguments of `isapprox`, respectively,
after the `≈` comparison:
```jldoctest
julia> @test 1 ≈ 0.999999  rtol=1e-5
Test Passed
```
Note that this is not a specific feature of the `≈` but rather a general feature of the `@test` macro: `@test a <op> b key=val` is transformed by the macro into `@test op(a, b, key=val)`. It is, however, particularly useful for `≈` tests.

```@docs
Test.@inferred
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

## Test result types

```@docs
Test.Result
Test.Pass
Test.Fail
Test.Error
Test.Broken
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
        return ts
    end

    # so the results are printed if we are at the top level
    Test.print_test_results(ts)
    return ts
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

In order to use a custom testset and have the recorded results printed as part of any outer default testset,
also define `Test.get_test_counts`. This might look like so:

```julia
using Test: AbstractTestSet, Pass, Fail, Error, Broken, get_test_counts, TestCounts, format_duration

function Test.get_test_counts(ts::CustomTestSet)
    passes, fails, errors, broken = 0, 0, 0, 0
    # cumulative results
    c_passes, c_fails, c_errors, c_broken = 0, 0, 0, 0

    for t in ts.results
        # count up results
        isa(t, Pass)   && (passes += 1)
        isa(t, Fail)   && (fails  += 1)
        isa(t, Error)  && (errors += 1)
        isa(t, Broken) && (broken += 1)
        # handle children
        if isa(t, AbstractTestSet)
            tc = get_test_counts(t)::TestCounts
            c_passes += tc.passes + tc.cumulative_passes
            c_fails  += tc.fails + tc.cumulative_fails
            c_errors += tc.errors + tc.cumulative_errors
            c_broken += tc.broken + tc.cumulative_broken
        end
    end
    # get a duration, if we have one
    duration = format_duration(ts)
    return TestCounts(true, passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken, duration)
end
```

```@docs
Test.TestCounts
Test.get_test_counts
Test.format_duration
Test.print_test_results
```

## Test utilities

```@docs
Test.GenericArray
Test.GenericDict
Test.GenericOrder
Test.GenericSet
Test.GenericString
Test.detect_ambiguities
Test.detect_unbound_args
```

## Workflow for Testing Packages

Using the tools available to us in the previous sections, here is a potential workflow of creating a package and adding tests to it.

### Generating an Example Package

For this workflow, we will create a package called `Example`:

```julia
pkg> generate Example
shell> cd Example
shell> mkdir test
pkg> activate .
```

### Creating Sample Functions

The number one requirement for testing a package is to have functionality to test.
For that, we will add some simple functions to `Example` that we can test.
Add the following to `src/Example.jl`:

```julia
module Example

export greet, simple_add, type_multiply

function greet()
    "Hello world!"
end

function simple_add(a, b)
    a + b
end

function type_multiply(a::Float64, b::Float64)
    a * b
end

end
```

### Creating a Test Environment

From within the root of the `Example` package, navigate to the `test` directory, activate a new environment there, and add the `Test` package to the environment:

```julia
shell> cd test
pkg> activate .
(test) pkg> add Test
```

### Testing Our Package

Now, we are ready to add tests to `Example`.
It is standard practice to create a file within the `test` directory called `runtests.jl` which contains the test sets we want to run.
Go ahead and create that file within the `test` directory and add the following code to it:

```julia
using Example
using Test

@testset "Example tests" begin

    @testset "Math tests" begin
        include("math_tests.jl")
    end

    @testset "Greeting tests" begin
        include("greeting_tests.jl")
    end
end
```

We will need to create those two included files, `math_tests.jl` and `greeting_tests.jl`, and add some tests to them.

> **Note:** Notice how we did not have to specify add `Example` into the `test` environment's `Project.toml`.
> This is a benefit of Julia's testing system that you could [read about more here](https://pkgdocs.julialang.org/dev/creating-packages/).

#### Writing Tests for `math_tests.jl`

Using our knowledge of `Test.jl`, here are some example tests we could add to `math_tests.jl`:

```julia
@testset "Testset 1" begin
    @test 2 == simple_add(1, 1)
    @test 3.5 == simple_add(1, 2.5)
        @test_throws MethodError simple_add(1, "A")
        @test_throws MethodError simple_add(1, 2, 3)
end

@testset "Testset 2" begin
    @test 1.0 == type_multiply(1.0, 1.0)
        @test isa(type_multiply(2.0, 2.0), Float64)
    @test_throws MethodError type_multiply(1, 2.5)
end
```

#### Writing Tests for `greeting_tests.jl`

Using our knowledge of `Test.jl`, here are some example tests we could add to `greeting_tests.jl`:

```julia
@testset "Testset 3" begin
    @test "Hello world!" == greet()
    @test_throws MethodError greet("Antonia")
end
```

### Testing Our Package

Now that we have added our tests and our `runtests.jl` script in `test`, we can test our `Example` package by going back to the root of the `Example` package environment and reactivating the `Example` environment:

```julia
shell> cd ..
pkg> activate .
```

From there, we can finally run our test suite as follows:

```julia
(Example) pkg> test
     Testing Example
      Status `/tmp/jl_Yngpvy/Project.toml`
  [fa318bd2] Example v0.1.0 `/home/src/Projects/tmp/errata/Example`
  [8dfed614] Test `@stdlib/Test`
      Status `/tmp/jl_Yngpvy/Manifest.toml`
  [fa318bd2] Example v0.1.0 `/home/src/Projects/tmp/errata/Example`
  [2a0f44e3] Base64 `@stdlib/Base64`
  [b77e0a4c] InteractiveUtils `@stdlib/InteractiveUtils`
  [56ddb016] Logging `@stdlib/Logging`
  [d6f4376e] Markdown `@stdlib/Markdown`
  [9a3f8284] Random `@stdlib/Random`
  [ea8e919c] SHA `@stdlib/SHA`
  [9e88b42a] Serialization `@stdlib/Serialization`
  [8dfed614] Test `@stdlib/Test`
     Testing Running tests...
Test Summary: | Pass  Total
Example tests |    9      9
     Testing Example tests passed
```

And if all went correctly, you should see a similar output as above.
Using `Test.jl`, more complicated tests can be added for packages but this should ideally point developers in the direction of how to get started with testing their own created packages.

```@meta
DocTestSetup = nothing
```

### Code Coverage

Code coverage tracking during tests can be enabled using the `pkg> test --coverage` flag (or at a lower level using the
[`--code-coverage`](@ref command-line-interface) julia arg). This is on by default in the
[julia-runtest](https://github.com/julia-actions/julia-runtest) GitHub action.

To evaluate coverage either manually inspect the `.cov` files that are generated beside the source files locally,
or in CI use the [julia-processcoverage](https://github.com/julia-actions/julia-processcoverage) GitHub action.

!!! compat "Julia 1.11"
    Since Julia 1.11, coverage is not collected during the package precompilation phase.
