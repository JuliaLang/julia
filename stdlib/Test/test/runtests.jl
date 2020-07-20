# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Distributed, Random
using Test: guardseed

import Logging: Debug, Info, Warn

@testset "@test" begin
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
end
@testset "@test keyword precedence" begin
    # post-semicolon keyword, suffix keyword, pre-semicolon keyword
    @test isapprox(1, 2, atol=0) atol=1
    @test isapprox(1, 3, atol=0; atol=2) atol=1
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
end
# Test printing of Fail results
mutable struct NoThrowTestSet <: Test.AbstractTestSet
    results::Vector
    NoThrowTestSet(desc) = new([])
end
Test.record(ts::NoThrowTestSet, t::Test.Result) = (push!(ts.results, t); t)
Test.finish(ts::NoThrowTestSet) = ts.results
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
end

let errors = @testset NoThrowTestSet begin
        # 1 - Error - unexpected pass
        @test_broken true
        # 2 - Error - converting a call into a comparison
        @test ==(1, 1:2...)
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
end

let retval_tests = @testset NoThrowTestSet begin
        ts = Test.DefaultTestSet("Mock for testing retval of record(::DefaultTestSet, ::T <: Result) methods")
        pass_mock = Test.Pass(:test, 1, 2, LineNumberNode(0, "A Pass Mock"))
        @test Test.record(ts, pass_mock) isa Test.Pass
        error_mock = Test.Error(:test, 1, 2, 3, LineNumberNode(0, "An Error Mock"))
        @test Test.record(ts, error_mock) isa Test.Error
        fail_mock = Test.Fail(:test, 1, 2, 3, LineNumberNode(0, "A Fail Mock"))
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
                @test 2 == :foo
                @test 3 == 3
                @testset "d" begin
                    @test 4 == 4
                end
                @testset begin
                    @test :blank != :notblank
                end
            end
            @testset "inner1" begin
                @test 1 == 1
                @test 2 == 2
                @test 3 == :bar
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
    deleteat!(Test.get_testset().results,1)
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
    @test occursin("""
        Test Summary: | Pass  Fail  Total
        Foo Tests     |    2     2      4
          Animals     |    1     1      2
            Felines   |    1            1
            Canines   |          1      1
          Arrays      |    1     1      2
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

function newfunc()
    42
end
@deprecate oldfunc newfunc

@testset "@test_deprecated" begin
    @test_deprecated oldfunc()

    # Expression passthrough
    if Base.JLOptions().depwarn != 2
        @test (@test_deprecated oldfunc()) == 42

        fails = @testset NoThrowTestSet "check that @test_deprecated detects bad input" begin
            @test_deprecated newfunc()
            @test_deprecated r"Not found in message" oldfunc()
        end
        @test length(fails) == 2
        @test fails[1] isa Test.LogTestFailure
        @test fails[2] isa Test.LogTestFailure
    else
        @warn """Omitting `@test_deprecated` tests which can't yet
                 be tested in --depwarn=error mode"""
    end
end

@testset "@testset preserves GLOBAL_RNG's state, and re-seeds it" begin
    # i.e. it behaves as if it was wrapped in a `guardseed(GLOBAL_RNG.seed)` block
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
