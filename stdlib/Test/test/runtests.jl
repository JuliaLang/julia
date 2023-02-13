# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random
using Test: guardseed
using Serialization
using Distributed: RemoteException

import Logging: Debug, Info, Warn, with_logger

@testset "@test" begin
    atol = 1
    a = (; atol=2)
    @test true
    @test 1 == 1
    @test 1 != 2
    @test ==(1, 1)
    @test ==((1, 1)...)
    @test 1 ≈ 2 atol=1
    @test strip("\t  hi   \n") == "hi"
    @test strip("\t  this should fail   \n") != "hi"
    @test isequal(1, 1)
    @test isapprox(1, 1, atol=0.1)
    @test isapprox(1, 1; atol=0.1)
    @test isapprox(1, 1; [(:atol, 0)]...)
    @test isapprox(1, 2; atol)
    @test isapprox(1, 3; a.atol)
end
@testset "@test with skip/broken kwargs" begin
    # Make sure the local variables can be used in conditions
    a = 1
    @test 2 + 2 == 4 broken=false
    @test error() broken=true
    @test !Sys.iswindows() broken=Sys.iswindows()
    @test 1 ≈ 2 atol=1 broken=a==2
    @test false skip=true
    @test true skip=false
    @test Grogu skip=isone(a)
    @test 41 ≈ 42 rtol=1 skip=false
end
@testset "@test keyword precedence" begin
    atol = 2
    # post-semicolon keyword, suffix keyword, pre-semicolon keyword
    @test isapprox(1, 2, atol=0) atol=1
    @test isapprox(1, 3, atol=0; atol=2) atol=1
    @test isapprox(1, 2, atol=0; atol)
    @test isapprox(1, 3, atol=0; atol) atol=1
end
@testset "@test should only evaluate the arguments once" begin
    g = Int[]
    f = (x) -> (push!(g, x); x)
    @test f(1) == 1
    @test g == [1]

    empty!(g)
    @test isequal(f(2), 2)
    @test g == [2]
end

@testset "@test_broken with fail" begin
    @test_broken false
    @test_broken 1 == 2
    @test_broken 1 != 1
    @test_broken strip("\t  hi   \n") != "hi"
    @test_broken strip("\t  this should fail   \n") == "hi"
end
@testset "@test_broken with errors" begin
    @test_broken error()
    @test_broken absolute_nonsense
end
@testset "@test_skip" begin
    @test_skip error()
    @test_skip true
    @test_skip false
    @test_skip gobbeldygook
end
@testset "@test_warn" begin
    @test 1234 === @test_nowarn(1234)
    @test 5678 === @test_warn("WARNING: foo", begin println(stderr, "WARNING: foo"); 5678; end)
    let a
        @test_throws UndefVarError(:a) a
        @test_nowarn a = 1
        @test a === 1
    end
end
@testset "@test and elements of an array" begin
    a = Array{Float64, 5}(undef, 2, 2, 2, 2, 2)
    a[1, 1, 1, 1, 1] = 10
    @test a[1, 1, 1, 1, 1] == 10
    @test a[1, 1, 1, 1, 1] != 2
end

@test rand() != rand()

@testset "Pass - exception" begin
    @test endswith(sprint(show, @test_throws ErrorException error()),
                   "Thrown: ErrorException")
    @test endswith(sprint(show, @test_throws ErrorException("test") error("test")),
                   "Thrown: ErrorException")
    @test endswith(sprint(show, @test_throws "a test" error("a test")),
                   "Message: \"a test\"")
    @test occursin("Message: \"DomainError",
                   sprint(show, @test_throws r"sqrt\([Cc]omplex" sqrt(-1)))
    @test endswith(sprint(show, @test_throws str->occursin("a t", str) error("a test")),
                   "Message: \"a test\"")
    @test endswith(sprint(show, @test_throws ["BoundsError", "access", "1-element", "at index [2]"] [1][2]),
                   "Message: \"BoundsError: attempt to access 1-element Vector{$Int} at index [2]\"")
    @test_throws "\"" throw("\"")
    @test_throws Returns(false) throw(Returns(false))
end
# Test printing of Fail results
include("nothrow_testset.jl")

let fails = @testset NoThrowTestSet begin
        # 1 - Fail - wrong exception
        @test_throws OverflowError error()
        # 2 - Fail - no exception
        @test_throws OverflowError 1 + 1
        # 3 - Fail - comparison
        @test 1+1 == 2+2
        # 4 - Fail - approximate comparison
        @test 1/1 ≈ 2/1
        # 5 - Fail - chained comparison
        @test 1+0 == 2+0 == 3+0
        # 6 - Fail - comparison call
        @test ==(1 - 2, 2 - 1)
        # 7 - Fail - splatting
        @test ==(1:2...)
        # 8 - Fail - isequal
        @test isequal(0 / 0, 1 / 0)
        # 9 - Fail - function splatting
        @test isequal(1:2...)
        # 10 - Fail - isapprox
        @test isapprox(0 / 1, -1 / 0)
        # 11 & 12 - Fail - function with keyword
        @test isapprox(1 / 2, 2 / 1, atol=1 / 1)
        @test isapprox(1 - 2, 2 - 1; atol=1 - 1)
        # 13 - Fail - function keyword splatting
        k = [(:atol, 0), (:nans, true)]
        @test isapprox(1, 2; k...)
        # 14 - Fail - call negation
        @test !isequal(1, 2 - 1)
        # 15 - Fail - comparison negation
        @test !(2 + 3 == 1 + 4)
        # 16 - Fail - chained negation
        @test !(2 + 3 == 1 + 4 == 5)
        # 17 - Fail - isempty
        nonempty = [1, 2, 3]
        @test isempty(nonempty)
        str1 = "Hello"
        str2 = "World"
        # 18 - Fail - occursin
        @test occursin(str1, str2)
        # 19 - Fail - startswith
        @test startswith(str1, str2)
        # 20 - Fail - endswith
        @test endswith(str1, str2)
        # 21 - Fail - contains
        @test contains(str1, str2)
        # 22 - Fail - Type Comparison
        @test typeof(1) <: typeof("julia")
        # 23 - 26 - Fail - wrong message
        @test_throws "A test" error("a test")
        @test_throws r"sqrt\([Cc]omplx" sqrt(-1)
        @test_throws str->occursin("a T", str) error("a test")
        @test_throws ["BoundsError", "aquire", "1-element", "at index [2]"] [1][2]
    end
    for fail in fails
        @test fail isa Test.Fail
    end

    let str = sprint(show, fails[1])
        @test occursin("Expression: error()", str)
        @test occursin("Thrown: ErrorException", str)
    end

    let str = sprint(show, fails[2])
        @test occursin("Expression: 1 + 1", str)
        @test occursin("No exception thrown", str)
    end

    let str = sprint(show, fails[3])
        @test occursin("Expression: 1 + 1 == 2 + 2", str)
        @test occursin("Evaluated: 2 == 4", str)
    end

    let str = sprint(show, fails[4])
        @test occursin("Expression: 1 / 1 ≈ 2 / 1", str)
        @test occursin("Evaluated: 1.0 ≈ 2.0", str)
    end

    let str = sprint(show, fails[5])
        @test occursin("Expression: 1 + 0 == 2 + 0 == 3 + 0", str)
        @test occursin("Evaluated: 1 == 2 == 3", str)
    end

    let str = sprint(show, fails[6])
        @test occursin("Expression: 1 - 2 == 2 - 1", str)
        @test occursin("Evaluated: -1 == 1", str)
    end

    let str = sprint(show, fails[7])
        @test occursin("Expression: (==)(1:2...)", str)
        @test !occursin("Evaluated", str)
    end

    let str = sprint(show, fails[8])
        @test occursin("Expression: isequal(0 / 0, 1 / 0)", str)
        @test occursin("Evaluated: isequal(NaN, Inf)", str)
    end

    let str = sprint(show, fails[9])
        @test occursin("Expression: isequal(1:2...)", str)
        @test occursin("Evaluated: isequal(1, 2)", str)
    end

    let str = sprint(show, fails[10])
        @test occursin("Expression: isapprox(0 / 1, -1 / 0)", str)
        @test occursin("Evaluated: isapprox(0.0, -Inf)", str)
    end

    let str = sprint(show, fails[11])
        @test occursin("Expression: isapprox(1 / 2, 2 / 1, atol = 1 / 1)", str)
        @test occursin("Evaluated: isapprox(0.5, 2.0; atol = 1.0)", str)
    end

    let str = sprint(show, fails[12])
        @test occursin("Expression: isapprox(1 - 2, 2 - 1; atol = 1 - 1)", str)
        @test occursin("Evaluated: isapprox(-1, 1; atol = 0)", str)
    end

    let str = sprint(show, fails[13])
        @test occursin("Expression: isapprox(1, 2; k...)", str)
        @test occursin("Evaluated: isapprox(1, 2; atol = 0, nans = true)", str)
    end

    let str = sprint(show, fails[14])
        @test occursin("Expression: !(isequal(1, 2 - 1))", str)
        @test occursin("Evaluated: !(isequal(1, 1))", str)
    end

    let str = sprint(show, fails[15])
        @test occursin("Expression: !(2 + 3 == 1 + 4)", str)
        @test occursin("Evaluated: !(5 == 5)", str)
    end

    let str = sprint(show, fails[16])
        @test occursin("Expression: !(2 + 3 == 1 + 4 == 5)", str)
        @test occursin("Evaluated: !(5 == 5 == 5)", str)
    end

    let str = sprint(show, fails[17])
        @test occursin("Expression: isempty(nonempty)", str)
        @test occursin("Evaluated: isempty([1, 2, 3])", str)
    end

    let str = sprint(show, fails[18])
        @test occursin("Expression: occursin(str1, str2)", str)
        @test occursin("Evaluated: occursin(\"Hello\", \"World\")", str)
    end

    let str = sprint(show, fails[19])
        @test occursin("Expression: startswith(str1, str2)", str)
        @test occursin("Evaluated: startswith(\"Hello\", \"World\")", str)
    end

    let str = sprint(show, fails[20])
        @test occursin("Expression: endswith(str1, str2)", str)
        @test occursin("Evaluated: endswith(\"Hello\", \"World\")", str)
    end

    let str = sprint(show, fails[21])
        @test occursin("Expression: contains(str1, str2)", str)
        @test occursin("Evaluated: contains(\"Hello\", \"World\")", str)
    end

    let str = sprint(show, fails[22])
        @test occursin("Expression: typeof(1) <: typeof(\"julia\")", str)
        @test occursin("Evaluated: $(typeof(1)) <: $(typeof("julia"))", str)
    end

    let str = sprint(show, fails[23])
        @test occursin("Expected: \"A test\"", str)
        @test occursin("Message: \"a test\"", str)
    end

    let str = sprint(show, fails[24])
        @test occursin("Expected: r\"sqrt\\([Cc]omplx\"", str)
        @test occursin(r"Message: .*Try sqrt\(Complex", str)
    end

    let str = sprint(show, fails[25])
        @test occursin("Expected: < match function >", str)
        @test occursin("Message: \"a test\"", str)
    end

    let str = sprint(show, fails[26])
        @test occursin("Expected: [\"BoundsError\", \"aquire\", \"1-element\", \"at index [2]\"]", str)
        @test occursin(r"Message: \"BoundsError.* 1-element.*at index \[2\]", str)
    end

end

struct BadError <: Exception end
Base.show(io::IO, ::BadError) = throw("I am a bad error")
let errors = @testset NoThrowTestSet begin
        # 1 - Error - unexpected pass
        @test_broken true
        # 2 - Error - converting a call into a comparison
        @test ==(1, 1:2...)
        # 3 - Error - objects with broken show
        @test throw(BadError())
        @test BadError()
        throw(BadError())
    end

    for err in errors
        @test err isa Test.Error
    end

    let str = sprint(show, errors[1])
        @test occursin("Unexpected Pass", str)
        @test occursin("Expression: true", str)
    end

    let str = sprint(show, errors[2])
        @test occursin("Expression: ==(1, 1:2...)", str)
        @test occursin("MethodError: no method matching ==(::$Int, ::$Int, ::$Int)", str)
    end

    let str = sprint(show, errors[3])
        @test occursin("Expression: throw(BadError())\n  #=ERROR showing exception stack=# \"I am a bad error\"\n  Stacktrace:\n", str)
    end

    let str = sprint(show, errors[4])
        @test occursin("Expression: BadError()\n       Value: #=ERROR showing error of type $BadError=# \"I am a bad error\"\nStacktrace:\n", str)
    end

    let str = sprint(show, errors[5])
        @test occursin("Got exception outside of a @test\n  #=ERROR showing exception stack=# \"I am a bad error\"\n  Stacktrace:\n", str)
    end
end

let retval_tests = @testset NoThrowTestSet begin
        ts = Test.DefaultTestSet("Mock for testing retval of record(::DefaultTestSet, ::T <: Result) methods")
        pass_mock = Test.Pass(:test, 1, 2, 3, LineNumberNode(0, "A Pass Mock"))
        @test Test.record(ts, pass_mock) isa Test.Pass
        error_mock = Test.Error(:test, 1, 2, 3, LineNumberNode(0, "An Error Mock"))
        @test Test.record(ts, error_mock) isa Test.Error
        fail_mock = Test.Fail(:test, 1, 2, 3, nothing, LineNumberNode(0, "A Fail Mock"), false)
        @test Test.record(ts, fail_mock) isa Test.Fail
        broken_mock = Test.Broken(:test, LineNumberNode(0, "A Broken Mock"))
        @test Test.record(ts, broken_mock) isa Test.Broken
    end
    for retval_test in retval_tests
        @test retval_test isa Test.Pass
    end
end

@testset "printing of a TestSetException" begin
    tse_str = sprint(show, Test.TestSetException(1, 2, 3, 4, Vector{Union{Test.Error, Test.Fail}}()))
    @test occursin("1 passed", tse_str)
    @test occursin("2 failed", tse_str)
    @test occursin("3 errored", tse_str)
    @test occursin("4 broken", tse_str)
end

@test Test.finish(Test.FallbackTestSet()) !== nothing

OLD_STDOUT = stdout
OLD_STDERR = stderr
catch_out = IOStream("")
catch_err = IOStream("")
rde, wre = redirect_stderr()
rdo, wro = redirect_stdout()

# test that FallbackTestSet will throw immediately
cmd = `$(Base.julia_cmd()) --startup-file=no --depwarn=error test_exec.jl`
@test !success(pipeline(cmd))

@testset "no errors" begin
    @test true
    @test 1 == 1
end

# Test entirely empty test set
@testset "outer" begin
    @testset "inner" begin
    end
end

@testset "testset types" begin
    ts = @testset "@testset should return the testset" begin
        @test true
    end
    @test typeof(ts) == Test.DefaultTestSet
    @test ts.n_passed == 1
    tss = @testset "@testset/for should return an array of testsets: $i" for i in 1:3
        @test true
    end
    @test length(tss) == 3
    @test typeof(tss[1]) == Test.DefaultTestSet
    @test tss[1].n_passed == 1
end
@testset "accounting" begin
    local ts, fails
    try
        ts = @testset "outer" begin
            @testset "inner1" begin
                @test true
                @test false
                @test 1 == 1
                @test 2 === :foo
                @test 3 == 3
                @testset "d" begin
                    @test 4 == 4
                end
                @testset begin
                    @test :blank !== :notblank
                end
            end
            @testset "inner1" begin
                @test 1 == 1
                @test 2 == 2
                @test 3 === :bar
                @test 4 == 4
                @test_throws ErrorException 1+1
                @test_throws ErrorException error()
                @test_throws RemoteException error()
                @testset "errrrr" begin
                    @test "not bool"
                    @test error()
                end

                error("exceptions in testsets should be caught")
                @test 1 == 1 # this test will not be run
            end

            @testset "loop with desc" begin
                @testset "loop1 $T" for T in (Float32, Float64)
                    @test 1 == T(1)
                end
            end
            @testset "loops without desc" begin
                @testset for T in (Float32, Float64)
                    @test 1 == T(1)
                end
                @testset for T in (Float32, Float64), S in (Int32,Int64)
                    @test S(1) == T(1)
                end
            end
            @testset "some loops fail" begin
                @testset for i in 1:5
                    @test i <= 4
                end
                # should add 3 errors and 3 passing tests
                @testset for i in 1:6
                    iseven(i) || error("error outside of test")
                    @test true # only gets run if the above passed
                end
            end
        end
        # These lines shouldn't be called
        error("No exception was thrown!")
    catch ex
        redirect_stdout(OLD_STDOUT)
        redirect_stderr(OLD_STDERR)
        ex
    end
    @testset "ts results" begin
        @test isa(ts, Test.DefaultTestSet)
        passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken = Test.get_test_counts(ts)
        total_pass   = passes + c_passes
        total_fail   = fails  + c_fails
        total_error  = errors + c_errors
        total_broken = broken + c_broken
        @test total_pass   == 24
        @test total_fail   == 6
        @test total_error  == 6
        @test total_broken == 0
    end
    ts.anynonpass = false
    deleteat!(Test.get_testset().results, 1)
end

@test .1+.1+.1 ≈ .3
@test .1+.1+.1 ≉ .4

ts = @testset "@testset should return the testset" begin
    @test true
end
@test typeof(ts) == Test.DefaultTestSet
@test ts.n_passed == 1

tss = @testset "@testset/for should return an array of testsets: $i" for i in 1:3
    @test true
end
@test length(tss) == 3
@test typeof(tss[1]) == Test.DefaultTestSet
@test tss[1].n_passed == 1

# Issue #17908 (return)
testset_depth17908 = Test.get_testset_depth()
@testset for i in 1:3
    i > 1 && return
    @test i == 1
end
# The return aborts the control flow so the expression above doesn't return a
# value. The only thing we can test is whether the testset is properly popped.
# Do not use `@test` since the issue this is testing will swallow the error.
@assert testset_depth17908 == Test.get_testset_depth()

# Issue #17462 and Issue #17908 (break, continue)
testset_depth17462 = Test.get_testset_depth()
counter_17462_pre = 0
counter_17462_post = 0
tss17462 = @testset for x in [1,2,3,4]
    global counter_17462_pre, counter_17462_post
    counter_17462_pre += 1
    if x == 1
        @test counter_17462_pre == x
        continue
        @test false
    elseif x == 3
        @test counter_17462_pre == x
        break
        @test false
    elseif x == 4
        @test false
    else
        @test counter_17462_pre == x
        @test x == 2
        @test counter_17462_post == 0
    end
    counter_17462_post += 1
end
# Do not use `@test` since the issue this is testing will swallow the error.
# Same for the `@assert` in the for loop below
@assert testset_depth17462 == Test.get_testset_depth()
@assert length(tss17462) == 3
for ts17462 in tss17462
    @assert isa(ts17462, Test.DefaultTestSet)
end
@test counter_17462_pre == 3
@test counter_17462_post == 1

# Issue #21008
ts = try
    @testset "@test_broken and @test_skip should not give an exception" begin
        @test_broken false
        @test_skip true
        @test_skip false
    end
catch
    nothing # Shouldn't get here
end
@test ts isa Test.DefaultTestSet

# now we're done running tests with DefaultTestSet so we can go back to stdout
redirect_stdout(OLD_STDOUT)
redirect_stderr(OLD_STDERR)

# import the methods needed for defining our own testset type
import Test: record, finish
using Test: get_testset_depth, get_testset
using Test: AbstractTestSet, Result, Pass, Fail, Error
struct CustomTestSet <: Test.AbstractTestSet
    description::String
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

ts = @testset CustomTestSet "Testing custom testsets" begin
    # this testset should inherit the parent testset type
    @testset "custom testset inner 1" begin
        @test true
        @test false
        @test error("this error will be reported as an error")
        @test_throws ErrorException nothing
        @test_throws ErrorException error("this error is a success")
    end
    # this testset has its own testset type
    @testset CustomTestSet foo=4 "custom testset inner 2" begin
        # this testset should inherit the type, but not the argument. If a particular
        # testset type wants inheritance behavior they should implement it themselves
        # using get_testset() in the constructor
        @testset "custom testset inner 2 inner 1" begin
            @test true
        end
        # make sure the RHS can use computed values, also tests options without
        # specifying the testset type
        @testset foo=(1+2) "custom testset inner 2 inner 2" begin
            @test true
        end
    end
end

@test typeof(ts) == CustomTestSet
@test ts.foo == 1
@test ts.description == "Testing custom testsets"
@test typeof(ts.results[1]) == CustomTestSet
@test ts.results[1].description == "custom testset inner 1"
@test ts.results[1].foo == 1
@test typeof(ts.results[1].results[1]) == Pass
@test typeof(ts.results[1].results[2]) == Fail
@test typeof(ts.results[1].results[3]) == Error
@test typeof(ts.results[1].results[4]) == Fail
@test typeof(ts.results[1].results[5]) == Pass

@test typeof(ts.results[2]) == CustomTestSet
@test ts.results[2].description == "custom testset inner 2"
@test ts.results[2].foo == 4
@test typeof(ts.results[2].results[1]) == CustomTestSet
@test ts.results[2].results[1].foo == 1
@test typeof(ts.results[2].results[1].results[1]) == Pass
@test typeof(ts.results[2].results[2]) == CustomTestSet
@test ts.results[2].results[2].foo == 3

# test custom testset types on testset/for
tss = @testset CustomTestSet foo=3 "custom testset $i" for i in 1:6
    @testset "inner testset $i-$j" for j in 1:3
        @test iseven(i + j)
    end
    # make sure a testset within a testset/for works
    @testset "inner testset $i" begin
        @test iseven(i)
    end
end


for i in 1:6
    @test typeof(tss[i]) == CustomTestSet
    @test tss[i].foo == 3
    for j in 1:3
        @test typeof(tss[i].results[j]) == CustomTestSet
        @test tss[i].results[j].foo == 1
        @test typeof(tss[i].results[j].results[1]) == (iseven(i+j) ? Pass : Fail)
    end
    @test typeof(tss[i].results[4]) == CustomTestSet
    @test typeof(tss[i].results[4].results[1]) == (iseven(i) ? Pass : Fail)
end

# test that second argument is escaped correctly
foo = 3
tss = @testset CustomTestSet foo=foo "custom testset - escaping" begin
    @test true
end
@test tss.foo == 3

# test @inferred
uninferrable_function(i) = (1, "1")[i]
uninferrable_small_union(i) = (1, nothing)[i]
@test_throws ErrorException @inferred(uninferrable_function(1))
@test @inferred(identity(1)) == 1
@test @inferred(Nothing, uninferrable_small_union(1)) === 1
@test @inferred(Nothing, uninferrable_small_union(2)) === nothing
@test_throws ErrorException @inferred(Missing, uninferrable_small_union(1))
@test_throws ErrorException @inferred(Missing, uninferrable_small_union(2))
@test_throws ArgumentError @inferred(nothing, uninferrable_small_union(1))

# Ensure @inferred only evaluates the arguments once
inferred_test_global = 0
function inferred_test_function()
    global inferred_test_global
    inferred_test_global += 1
    true
end
@test @inferred inferred_test_function()
@test inferred_test_global == 1

struct SillyArray <: AbstractArray{Float64,1} end
Base.getindex(a::SillyArray, i) = rand() > 0.5 ? 0 : false
@testset "@inferred works with A[i] expressions" begin
    @test (@inferred (1:3)[2]) == 2
    test_result = @test_throws ErrorException (@inferred SillyArray()[2])
    @test occursin("Bool", test_result.value.msg)
end
# Issue #14928
# Make sure abstract error type works.
@test_throws Exception error("")

# Issue #17105
# @inferred with kwargs
inferrable_kwtest(x; y=1) = 2x
uninferrable_kwtest(x; y=1) = 2x+y
@test (@inferred inferrable_kwtest(1)) == 2
@test (@inferred inferrable_kwtest(1; y=1)) == 2
@test (@inferred uninferrable_kwtest(1)) == 3
@test (@inferred uninferrable_kwtest(1; y=2)) == 4

@test_throws ErrorException @testset "$(error())" for i in 1:10
end
@test_throws ErrorException @testset "$(error())" begin
end

@testset "backtraces in test errors" begin
    local f = tempname()
    write(f,
    """
    using Test
    @testset begin
        @test 1==2
        @test_throws MethodError 1
    end
    """)
    local msg = read(pipeline(ignorestatus(`$(Base.julia_cmd()) --startup-file=no --color=no $f`), stderr=devnull), String)
    @test !occursin("do_test(", msg)
    @test !occursin("include(", msg)
    @test occursin("at " * f * ":3", msg)
    @test occursin("at " * f * ":4", msg)
    rm(f; force=true)
end

let io = IOBuffer()
    exc = Test.TestSetException(1,2,3,4,Vector{Union{Test.Error, Test.Fail}}())
    Base.showerror(io, exc, backtrace())
    @test !occursin("backtrace()", String(take!(io)))
end

@testset "#19750" begin
    io = IOBuffer()
    exc = Test.TestSetException(1,2,3,4,Vector{Union{Test.Error, Test.Fail}}())
    Base.showerror(io, exc, backtrace())
    @test !occursin("backtrace()", String(take!(io)))

    exc = Test.FallbackTestSetException("msg")
    Base.showerror(io, exc, backtrace())
    str = String(take!(io))
    @test occursin("msg", str)
    @test !occursin("backtrace()", str)
end

let msg = read(pipeline(ignorestatus(`$(Base.julia_cmd()) --startup-file=no --color=no -e '
        using Test

        foo(x) = length(x)^2

        @testset "Foo Tests" begin
            @testset "Animals" begin
                @testset "Felines" begin
                    @test foo("cat") == 9
                end
                @testset "Canines" begin
                    @test foo("dog") == 11
                end
            end
            @testset "Arrays" begin
                @test foo(zeros(2)) == 4
                @test foo(fill(1., 4)) == 15
            end
        end'`), stderr=devnull), String)
    @test occursin(r"""
        Test Summary: | Pass  Fail  Total  Time
        Foo Tests     |    2     2      4  \s*\d*.\ds
          Animals     |    1     1      2  \s*\d*.\ds
            Felines   |    1            1  \s*\d*.\ds
            Canines   |          1      1  \s*\d*.\ds
          Arrays      |    1     1      2  \s*\d*.\ds
        """, msg)
end

# 20489
let msg = split(read(pipeline(ignorestatus(`$(Base.julia_cmd()) --startup-file=no --color=no -e '
        Test.print_test_results(Test.DefaultTestSet(""))'`), stderr=devnull), String), "\n")[1]
    @test msg == rstrip(msg)
end

@testset "test guarded Random.seed!" begin
    seed = rand(UInt)
    orig = copy(Random.default_rng())
    @test guardseed(()->rand(), seed) == guardseed(()->rand(), seed)
    @test guardseed(()->rand(Int), seed) == guardseed(()->rand(Int), seed)
    r1, r2 = MersenneTwister(0), MersenneTwister(0)
    a, b = guardseed(r1) do
        Random.seed!(r1, 0)
        rand(r1), rand(r1, Int)
    end::Tuple{Float64,Int}
    c, d = guardseed(r2) do
        Random.seed!(r2, 0)
        rand(r2), rand(r2, Int)
    end::Tuple{Float64,Int}
    @test a == c == rand(r1) == rand(r2)
    @test b == d == rand(r1, Int) == rand(r2, Int)
    @test orig == Random.default_rng()
    @test rand(orig) == rand()
end

@testset "file info in test errors" begin
    local f = tempname()

    write(f,
    """
    using Test
    @testset begin
        @test 1==2
        @test_throws UndefVarError 1
        @test_broken 1 == 1
    end
    """)

    local msg = read(pipeline(ignorestatus(`$(Base.julia_cmd()) --startup-file=no --color=no $f`), stderr=devnull), String)
    @test occursin("at " * f * ":" * "3", msg)
    @test occursin("at " * f * ":" * "4", msg)
    @test occursin("at " * f * ":" * "5", msg)

    rm(f; force=true)
end

# issue #24919
@testset "≈ with atol" begin
    local cmd = `$(Base.julia_cmd()) --startup-file=no --color=no`
    f(src) = read(pipeline(ignorestatus(`$cmd -e $src`), stderr=devnull), String)

    msg = f("""
    using Test
    x, y = 0.9, 0.1
    @test x ≈ y atol=0.01
    """)
    @test occursin("Evaluated: 0.9 ≈ 0.1 (atol=0.01)", msg)

    msg = f("""
    using Test
    x, y = 0.9, 0.1
    @test x ≈ y nans=true atol=0.01
    """)
    @test occursin("Evaluated: 0.9 ≈ 0.1 (nans=true, atol=0.01)", msg)
end

erronce() = @error "an error" maxlog=1

@testset "@test_logs" begin
    function foo(n)
        @info "Doing foo with n=$n"
        for i=1:n
            @debug "Iteration $i"
        end
    end

    @test_logs (Info,"Doing foo with n=2") foo(2)

    # Log pattern matching
    # Regex
    @test_logs (Info,r"^Doing foo with n=[0-9]+$") foo(10)
    @test_logs (Info,r"^Doing foo with n=[0-9]+$") foo(1)
    # Level symbols
    @test_logs (:debug,) min_level=Debug @debug "foo"
    @test_logs (:info,)  @info  "foo"
    @test_logs (:warn,)  @warn  "foo"
    @test_logs (:error,) @error "foo"

    # Pass through so the value of the expression can also be tested
    @test (@test_logs (Info,"blah") (@info "blah"; 42)) == 42

    # Debug level log collection
    @test_logs (Info,"Doing foo with n=2") (Debug,"Iteration 1") (Debug,"Iteration 2") min_level=Debug foo(2)

    @test_logs (Debug,"Iteration 5") min_level=Debug match_mode=:any foo(10)

    # Respect `maxlog` (#41625). We check we only find one logging message.
    @test_logs (:error, "an error") (erronce(); erronce())

    # Test `respect_maxlog=false`:
    test_logger = Test.TestLogger(; respect_maxlog=false)
    with_logger(test_logger) do
        erronce()
        erronce()
    end
    @test length(test_logger.logs) == 2

    # Test failures
    fails = @testset NoThrowTestSet "check that @test_logs detects bad input" begin
        @test_logs (Warn,) foo(1)
        @test_logs (Warn,) match_mode=:any @info "foo"
        @test_logs (Debug,) @debug "foo"
        @test_logs (Warn,) error()
    end
    @test length(fails) == 4
    @test fails[1] isa Test.LogTestFailure
    @test fails[2] isa Test.LogTestFailure
    @test fails[3] isa Test.LogTestFailure
    @test fails[4] isa Test.Error
    @test startswith(fails[4].value, "ErrorException")
end

let code = quote
        function newfunc()
            42
        end
        @deprecate oldfunc newfunc

        @testset "@test_deprecated" begin
            @test_deprecated oldfunc()
            @test Base.JLOptions().depwarn == 1

            @test (@test_deprecated oldfunc()) == 42

            fails = @testset NoThrowTestSet "check that @test_deprecated detects bad input" begin
                @test_deprecated newfunc()
                @test_deprecated r"Not found in message" oldfunc()
            end
            @test length(fails) == 2
            @test fails[1] isa Test.LogTestFailure
            @test fails[2] isa Test.LogTestFailure
        end
    end
    incl = "include($(repr(joinpath(@__DIR__, "nothrow_testset.jl"))))"
    cmd = `$(Base.julia_cmd()) --startup-file=no --depwarn=yes -e 'using Test' -e $incl -e $code`
    @test success(pipeline(cmd))
end

@testset "@testset preserves GLOBAL_RNG's state, and re-seeds it" begin
    # i.e. it behaves as if it was wrapped in a `guardseed(GLOBAL_SEED)` block
    seed = rand(UInt128)
    Random.seed!(seed)
    a = rand()
    @testset begin
        # global RNG must re-seeded at the beginning of @testset
        @test a == rand()
    end
    @testset for i=1:3
        @test a == rand()
    end
    # the @testset's above must have no consequence for rand() below
    b = rand()
    Random.seed!(seed)
    @test a == rand()
    @test b == rand()

    # Even when seed!() is called within a testset A, subsequent testsets
    # should start with the same "global RNG state" as what A started with,
    # such that the test `refvalue == rand(Int)` below succeeds.
    # Currently, this means that Random.GLOBAL_SEED has to be restored,
    # in addition to the state of Random.default_rng().
    GLOBAL_SEED_orig = Random.GLOBAL_SEED
    local refvalue
    @testset "GLOBAL_SEED is also preserved (setup)" begin
        @test GLOBAL_SEED_orig == Random.GLOBAL_SEED
        refvalue = rand(Int)
        Random.seed!()
        @test GLOBAL_SEED_orig != Random.GLOBAL_SEED
    end
    @test GLOBAL_SEED_orig == Random.GLOBAL_SEED
    @testset "GLOBAL_SEED is also preserved (forloop)" for _=1:3
        @test refvalue == rand(Int)
        Random.seed!()
    end
    @test GLOBAL_SEED_orig == Random.GLOBAL_SEED
    @testset "GLOBAL_SEED is also preserved (beginend)" begin
        @test refvalue == rand(Int)
    end
end

@testset "InterruptExceptions #21043" begin
    @test_throws InterruptException (@test 1 == throw(InterruptException()))

    @testset begin
        @test_throws InterruptException throw(InterruptException())
    end

    mktemp() do f, _
        write(f,
        """
        using Test
        @testset begin
            try
                @test_throws ErrorException throw(InterruptException())
            catch e
                @test e isa InterruptException
            end
        end

        try
            @testset begin
                @test 1 == 1
                throw(InterruptException())
            end
        catch e
            @test e isa InterruptException
        end

        try
            @testset for i in 1:1
                @test 1 == 1
                throw(InterruptException())
            end
        catch e
            @test e isa InterruptException
        end
        """)
        cmd = `$(Base.julia_cmd()) --startup-file=no --color=no $f`
        msg = success(pipeline(ignorestatus(cmd), stderr=devnull))
    end
end

@testset "non AbstractTestSet as testset" begin
    local f, err = tempname(), tempname()
    write(f,
    """
    using Test
    desc = "description"
    @testset desc begin
        @test 1==1
    end
    """)
    run(pipeline(ignorestatus(`$(Base.julia_cmd()) --startup-file=no --color=no $f`), stderr=err))
    msg = read(err, String)
    @test occursin("Expected `desc` to be an AbstractTestSet, it is a String", msg)
    rm(f; force=true)
    rm(err, force=true)
end

f25835(;x=nothing) = _f25835(x)
_f25835(::Nothing) = ()
_f25835(x) = (x,)
# A keyword function that is never type stable
g25835(;x=1) = rand(Bool) ? 1.0 : 1
# A keyword function that is sometimes type stable
h25835(;x=1,y=1) = x isa Int ? x*y : (rand(Bool) ? 1.0 : 1)
@testset "keywords in @inferred" begin
    @test @inferred(f25835()) == ()
    @test @inferred(f25835(x=nothing)) == ()
    @test @inferred(f25835(x=1)) == (1,)

    # A global argument should make this uninferrable
    global y25835 = 1
    @test f25835(x=y25835) == (1,)
    @test_throws ErrorException @inferred((()->f25835(x=y25835))()) == (1,)

    @test_throws ErrorException @inferred(g25835()) == 1
    @test_throws ErrorException @inferred(g25835(x=1)) == 1

    @test @inferred(h25835()) == 1
    @test @inferred(h25835(x=2,y=3)) == 6
    @test_throws ErrorException @inferred(h25835(x=1.0,y=1.0)) == 1
end

@testset "splatting in isapprox" begin
    a = [1, 2, 3]
    @test isapprox(identity.((a, a))...)
end

@testset "treat NaN and missing in exception fields" begin
    struct Exception31219{T}
        value::T
    end
    f31219(x) = throw(Exception31219(x))

    @testset "exception field '$(repr(x))'" for x in ("ok", nothing, NaN, missing)
        @test_throws Exception31219(x) f31219(x)
    end
end

# Issue 20620
@test @inferred(.![true, false]) == [false, true]
@test @inferred([3, 4] .- [1, 2] .+ [-2, -2]) == [0, 0]

@testset "push/pop_testset invariance (Issue 32937)" begin
    io = IOBuffer()
    path = joinpath(@__DIR__(), "test_pop_testset_exec.jl")
    cmd = `$(Base.julia_cmd()) $path`
    ok = !success(pipeline(cmd; stdout = io, stderr = io))
    if !ok
        @error "push/pop_testset invariance test failed" cmd Text(String(take!(io)))
    end
    @test ok
end

let ex = :(something_complex + [1, 2, 3])
    b = PipeBuffer()
    let t = Test.Pass(:test, (ex, 1), (ex, 2), (ex, 3), LineNumberNode(@__LINE__, @__FILE__))
        serialize(b, t)
        @test string(t) == string(deserialize(b))
        @test eof(b)
    end
    let t = Test.Broken(:test, ex)
        serialize(b, t)
        @test string(t) == string(deserialize(b))
        @test eof(b)
    end
end

@testset "verbose option" begin
    expected = r"""
    Test Summary:             | Pass  Total  Time
    Parent                    |    9      9  \s*\d*.\ds
      Child 1                 |    3      3  \s*\d*.\ds
        Child 1.1 (long name) |    1      1  \s*\d*.\ds
        Child 1.2             |    1      1  \s*\d*.\ds
        Child 1.3             |    1      1  \s*\d*.\ds
      Child 2                 |    3      3  \s*\d*.\ds
      Child 3                 |    3      3  \s*\d*.\ds
        Child 3.1             |    1      1  \s*\d*.\ds
        Child 3.2             |    1      1  \s*\d*.\ds
        Child 3.3             |    1      1  \s*\d*.\ds
    """

    mktemp() do f, _
        write(f,
        """
        using Test

        @testset "Parent" verbose = true begin
            @testset "Child 1" verbose = true begin
                @testset "Child 1.1 (long name)" begin
                    @test 1 == 1
                end

                @testset "Child 1.2" begin
                    @test 1 == 1
                end

                @testset "Child 1.3" begin
                    @test 1 == 1
                end
            end

            @testset "Child 2" begin
                @testset "Child 2.1" begin
                    @test 1 == 1
                end

                @testset "Child 2.2" begin
                    @test 1 == 1
                end

                @testset "Child 2.3" begin
                    @test 1 == 1
                end
            end

            @testset "Child 3" verbose = true begin
                @testset "Child 3.1" begin
                    @test 1 == 1
                end

                @testset "Child 3.2" begin
                    @test 1 == 1
                end

                @testset "Child 3.3" begin
                    @test 1 == 1
                end
            end
        end
        """)
        cmd    = `$(Base.julia_cmd()) --startup-file=no --color=no $f`
        result = read(pipeline(ignorestatus(cmd), stderr=devnull), String)
        @test occursin(expected, result)
    end
end

@testset "failfast option" begin
    @testset "non failfast (default)" begin
        expected = r"""
        Test Summary: | Pass  Fail  Error  Total  Time
        Foo           |    1     2      1      4  \s*\d*.\ds
          Bar         |    1     1             2  \s*\d*.\ds
        """

        mktemp() do f, _
            write(f,
            """
            using Test

            @testset "Foo" begin
                @test false
                @test error()
                @testset "Bar" begin
                    @test false
                    @test true
                end
            end
            """)
            cmd    = `$(Base.julia_cmd()) --startup-file=no --color=no $f`
            result = read(pipeline(ignorestatus(cmd), stderr=devnull), String)
            @test occursin(expected, result)
        end
    end
    @testset "failfast" begin
        expected = r"""
        Test Summary: | Fail  Total  Time
        Foo           |    1      1  \s*\d*.\ds
        """

        mktemp() do f, _
            write(f,
            """
            using Test

            @testset "Foo" failfast=true begin
                @test false
                @test error()
                @testset "Bar" begin
                    @test false
                    @test true
                end
            end
            """)
            cmd    = `$(Base.julia_cmd()) --startup-file=no --color=no $f`
            result = read(pipeline(ignorestatus(cmd), stderr=devnull), String)
            @test occursin(expected, result)
        end
    end
    @testset "failfast passes to child testsets" begin
        expected = r"""
        Test Summary: | Fail  Total  Time
        PackageName   |    1      1  \s*\d*.\ds
          1           |    1      1  \s*\d*.\ds
        """

        mktemp() do f, _
            write(f,
            """
            using Test

            @testset "Foo" failfast=true begin
                @testset "1" begin
                   @test false
                end
                @testset "2" begin
                   @test true
                end
            end
            """)
            cmd    = `$(Base.julia_cmd()) --startup-file=no --color=no $f`
            result = read(pipeline(ignorestatus(cmd), stderr=devnull), String)
            @test occursin(expected, result)
        end
    end
    @testset "failfast via env var" begin
        expected = r"""
        Test Summary: | Fail  Total  Time
        Foo           |    1      1  \s*\d*.\ds
        """

        mktemp() do f, _
            write(f,
            """
            using Test
            ENV["JULIA_TEST_FAILFAST"] = true
            @testset "Foo" begin
                @test false
                @test error()
                @testset "Bar" begin
                    @test false
                    @test true
                end
            end
            """)
            cmd    = `$(Base.julia_cmd()) --startup-file=no --color=no $f`
            result = read(pipeline(ignorestatus(cmd), stderr=devnull), String)
            @test occursin(expected, result)
        end
    end
end

# Non-booleans in @test (#35888)
struct T35888 end
Base.isequal(::T35888, ::T35888) = T35888()
Base.:!(::T35888) = missing
let errors = @testset NoThrowTestSet begin
        # 1 - evaluates to non-Boolean
        @test missing
        # 2 - evaluates to non-Boolean
        @test !missing
        # 3 - evaluates to non-Boolean
        @test isequal(5)
        # 4 - evaluates to non-Boolean
        @test !isequal(5)
        # 5 - evaluates to non-Boolean
        @test isequal(T35888(), T35888())
        # 6 - evaluates to non-Boolean
        @test !isequal(T35888(), T35888())
        # 7 - evaluates to non-Boolean
        @test 1 < 2 < missing
        # 8 - evaluates to non-Boolean
        @test !(1 < 2 < missing)
        # 9 - TypeError in chained comparison
        @test 1 < 2 < missing < 4
        # 10 - TypeError in chained comparison
        @test !(1 < 2 < missing < 4)
    end

    for err in errors
        @test err isa Test.Error
    end

    let str = sprint(show, errors[1])
        @test occursin("Expression evaluated to non-Boolean", str)
        @test occursin("Expression: missing", str)
        @test occursin("Value: missing", str)
    end

    let str = sprint(show, errors[2])
        @test occursin("Expression evaluated to non-Boolean", str)
        @test occursin("Expression: !missing", str)
        @test occursin("Value: missing", str)
    end

    let str = sprint(show, errors[3])
        @test occursin("Expression evaluated to non-Boolean", str)
        @test occursin("Expression: isequal(5)", str)
    end

    let str = sprint(show, errors[4])
        @test occursin("Expression evaluated to non-Boolean", str)
        @test occursin("Expression: !(isequal(5))", str)
    end

    let str = sprint(show, errors[5])
        @test occursin("Expression evaluated to non-Boolean", str)
        @test occursin("Expression: isequal(T35888(), T35888())", str)
        @test occursin("Value: $T35888()", str)
    end

    let str = sprint(show, errors[6])
        @test occursin("Expression evaluated to non-Boolean", str)
        @test occursin("Expression: !(isequal(T35888(), T35888()))", str)
        @test occursin("Value: missing", str)
    end

    let str = sprint(show, errors[7])
        @test occursin("Expression evaluated to non-Boolean", str)
        @test occursin("Expression: 1 < 2 < missing", str)
        @test occursin("Value: missing", str)
    end

    let str = sprint(show, errors[8])
        @test occursin("Expression evaluated to non-Boolean", str)
        @test occursin("Expression: !(1 < 2 < missing)", str)
        @test occursin("Value: missing", str)
    end

    let str = sprint(show, errors[9])
        @test occursin("TypeError: non-boolean (Missing) used in boolean context", str)
        @test occursin("Expression: 1 < 2 < missing < 4", str)
    end

    let str = sprint(show, errors[10])
        @test occursin("TypeError: non-boolean (Missing) used in boolean context", str)
        @test occursin("Expression: !(1 < 2 < missing < 4)", str)
    end
end

macro test_macro_throw_1()
    throw(ErrorException("Real error"))
end
macro test_macro_throw_2()
    throw(LoadError("file", 111, ErrorException("Real error")))
end

@testset "Soft deprecation of @test_throws LoadError [@]macroexpand[1]" begin
    # If a macroexpand was detected, undecorated LoadErrors can stand in for any error.
    # This will throw a deprecation warning.
    @test_deprecated (@test_throws LoadError macroexpand(@__MODULE__, :(@test_macro_throw_1)))
    @test_deprecated (@test_throws LoadError @macroexpand @test_macro_throw_1)
    # Decorated LoadErrors are unwrapped if the actual exception matches the inner, but not the outer, exception, regardless of whether or not a macroexpand is detected.
    # This will not throw a deprecation warning.
    @test_throws LoadError("file", 111, ErrorException("Real error")) macroexpand(@__MODULE__, :(@test_macro_throw_1))
    @test_throws LoadError("file", 111, ErrorException("Real error")) @macroexpand @test_macro_throw_1
    # Decorated LoadErrors are not unwrapped if a LoadError was thrown.
    @test_throws LoadError("file", 111, ErrorException("Real error")) @macroexpand @test_macro_throw_2
end

# Issue 25483
mutable struct PassInformationTestSet <: Test.AbstractTestSet
    results::Vector
    PassInformationTestSet(desc) = new([])
end
Test.record(ts::PassInformationTestSet, t::Test.Result) = (push!(ts.results, t); t)
Test.finish(ts::PassInformationTestSet) = ts
@testset "Information in Pass result (Issue 25483)" begin
    ts = @testset PassInformationTestSet begin
        @test 1 == 1
        @test_throws ErrorException throw(ErrorException("Msg"))
    end
    test_line_number = (@__LINE__) - 3
    test_throws_line_number =  (@__LINE__) - 3
    @test ts.results[1].test_type === :test
    @test ts.results[1].orig_expr == :(1 == 1)
    @test ts.results[1].data == Expr(:comparison, 1, :(==), 1)
    @test ts.results[1].value == true
    @test ts.results[1].source == LineNumberNode(test_line_number, @__FILE__)
    @test ts.results[2].test_type === :test_throws
    @test ts.results[2].orig_expr == :(throw(ErrorException("Msg")))
    @test ts.results[2].data == ErrorException
    @test ts.results[2].value == ErrorException("Msg")
    @test ts.results[2].source == LineNumberNode(test_throws_line_number, @__FILE__)
end

let
    f(x) = @test isone(x)
    function h(x)
        @testset f(x)
        @testset "success" begin @test true end
        @testset for i in 1:3
            @test !iszero(i)
        end
    end
    tret = @testset h(1)
    tdesc = @testset "description" h(1)
    @testset "Function calls" begin
        @test tret.description == "h"
        @test tdesc.description == "description"
        @test length(tret.results) == 5
        @test tret.results[1].description == "f"
        @test tret.results[2].description == "success"
        for i in 1:3
            @test tret.results[2+i].description == "i = $i"
        end
    end
end
