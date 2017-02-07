# This file is a part of Julia. License is MIT: http://julialang.org/license

# Test @test
@test true
@test 1 == 1
@test 1 != 2
@test strip("\t  hi   \n") == "hi"
@test strip("\t  this should fail   \n") != "hi"

# Test @test_broken with fail
@test_broken false
@test_broken 1 == 2
@test_broken 1 != 1
@test_broken strip("\t  hi   \n") != "hi"
@test_broken strip("\t  this should fail   \n") == "hi"
# Test @test_broken with errors
@test_broken error()
@test_broken absolute_nonsense

#Test @test_skip
@test_skip error()
@test_skip true
@test_skip false
@test_skip gobbeldygook

# Test @test_warn
@test 1234 === @test_nowarn(1234)
@test 5678 === @test_warn("WARNING: foo", begin warn("foo"); 5678; end)
let a
    # Broken
    # @test_throws UndefVarError a
    # Replace with the previous line when #20016 is fixed
    @test_throws UndefRefError a
    @test_nowarn a = 1
    @test a === 1
end

a = Array{Float64,5}(2, 2, 2, 2, 2)
a[1,1,1,1,1] = 10
@test a[1,1,1,1,1] == 10
@test a[1,1,1,1,1] != 2

@test rand() != rand()

# Pass - exception
@test contains(sprint(show, @test_throws ErrorException error()),
                "Thrown: ErrorException")

# Test printing of Fail results
type NoThrowTestSet <: Base.Test.AbstractTestSet
    results::Vector
    NoThrowTestSet(desc) = new([])
end
Base.Test.record(ts::NoThrowTestSet, t::Base.Test.Result) = (push!(ts.results, t); t)
Base.Test.finish(ts::NoThrowTestSet) = ts.results
fails = @testset NoThrowTestSet begin
    # Fail - wrong exception
    @test_throws OverflowError error()
    # Fail - no exception
    @test_throws OverflowError 1 + 1
    # Fail - comparison
    @test 1+1 == 2+2
    # Error - unexpected pass
    @test_broken true
end
for i in 1:3
    @test isa(fails[i], Base.Test.Fail)
end
@test contains(sprint(show, fails[1]), "Thrown: ErrorException")
@test contains(sprint(show, fails[2]), "No exception thrown")
@test contains(sprint(show, fails[3]), "Evaluated: 2 == 4")
@test contains(sprint(show, fails[4]), "Unexpected Pass")

# Test printing of a TestSetException
tse_str = sprint(show, Test.TestSetException(1,2,3,4,Vector{Union{Base.Test.Error, Base.Test.Fail}}()))
@test contains(tse_str, "1 passed")
@test contains(tse_str, "2 failed")
@test contains(tse_str, "3 errored")
@test contains(tse_str, "4 broken")

@test Test.finish(Test.FallbackTestSet()) !== nothing

OLD_STDOUT = STDOUT
OLD_STDERR = STDERR
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
    @test typeof(ts) == Base.Test.DefaultTestSet
    @test ts.n_passed == 1
    tss = @testset "@testset/for should return an array of testsets: $i" for i in 1:3
        @test true
    end
    @test length(tss) == 3
    @test typeof(tss[1]) == Base.Test.DefaultTestSet
    @test tss[1].n_passed == 1
end
@testset "accounting" begin
    local ts
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
            srand(123)
            @testset "some loops fail" begin
                @testset for i in 1:5
                    @test i <= rand(1:10)
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
        passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken = Base.Test.get_test_counts(ts)
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
    deleteat!(Base.Test.get_testset().results,1)
end

@test .1+.1+.1 ≈ .3
@test .1+.1+.1 ≉ .4

ts = @testset "@testset should return the testset" begin
    @test true
end
@test typeof(ts) == Base.Test.DefaultTestSet
@test ts.n_passed == 1

tss = @testset "@testset/for should return an array of testsets: $i" for i in 1:3
    @test true
end
@test length(tss) == 3
@test typeof(tss[1]) == Base.Test.DefaultTestSet
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
    @assert isa(ts17462, Base.Test.DefaultTestSet)
end
@test counter_17462_pre == 3
@test counter_17462_post == 1

# now we're done running tests with DefaultTestSet so we can go back to STDOUT
redirect_stdout(OLD_STDOUT)
redirect_stderr(OLD_STDERR)

# import the methods needed for defining our own testset type
import Base.Test: record, finish
using Base.Test: get_testset_depth, get_testset
using Base.Test: AbstractTestSet, Result, Pass, Fail, Error
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
function uninferrable_function(i)
    q = [1, "1"]
    return q[i]
end

@test_throws ErrorException @inferred(uninferrable_function(1))
@test @inferred(identity(1)) == 1

# Ensure @inferred only evaluates the arguments once
inferred_test_global = 0
function inferred_test_function()
    global inferred_test_global
    inferred_test_global += 1
    true
end
@test @inferred inferred_test_function()
@test inferred_test_global == 1

# Test that @inferred works with A[i] expressions
@test @inferred((1:3)[2]) == 2
immutable SillyArray <: AbstractArray{Float64,1} end
Base.getindex(a::SillyArray, i) = rand() > 0.5 ? 0 : false
test_result = @test_throws ErrorException @inferred(SillyArray()[2])
@test contains(test_result.value.msg, "Bool")

# Issue #14928
# Make sure abstract error type works.
@test_throws Exception error("")

# Issue #17105
# @inferred with kwargs
function inferrable_kwtest(x; y=1)
    2x
end
function uninferrable_kwtest(x; y=1)
    2x+y
end
@test @inferred(inferrable_kwtest(1)) == 2
@test @inferred(inferrable_kwtest(1; y=1)) == 2
@test @inferred(uninferrable_kwtest(1)) == 3
@test_throws ErrorException @inferred(uninferrable_kwtest(1; y=2)) == 2

@test_throws ErrorException @testset "$(error())" for i in 1:10
end
@test_throws ErrorException @testset "$(error())" begin
end

io = IOBuffer()
@test (print(io, Base.Test.Error(:test_error, "woot", 5, backtrace())); 1) == 1
str = String(take!(io))
@test contains(str, "test.jl")
@test !contains(str, "boot.jl")

let io = IOBuffer()
    exc = Test.TestSetException(1,2,3,4,Vector{Union{Base.Test.Error, Base.Test.Fail}}())
    Base.showerror(io, exc, backtrace())
    @test !contains(String(take!(io)), "backtrace()")
end

# 19750
let io = IOBuffer()
    exc = Test.TestSetException(1,2,3,4,Vector{Union{Base.Test.Error, Base.Test.Fail}}())
    Base.showerror(io, exc, backtrace())
    @test !contains(String(take!(io)), "backtrace()")

    exc = Test.FallbackTestSetException("msg")
    Base.showerror(io, exc, backtrace())
    str = String(take!(io))
    @test contains(str, "msg")
    @test !contains(str, "backtrace()")
end

msg = readstring(pipeline(ignorestatus(`$(Base.julia_cmd()) --startup-file=no --color=no -e '
using Base.Test

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
        @test foo(ones(4)) == 15
    end
end'`), stderr=DevNull))

@test contains(msg,
"""
Test Summary: | Pass  Fail  Total
Foo Tests     |    2     2      4
  Animals     |    1     1      2
    Felines   |    1            1
    Canines   |          1      1
  Arrays      |    1     1      2
""")

# 20489
msg = split(readstring(pipeline(ignorestatus(`$(Base.julia_cmd()) --startup-file=no --color=no -e '
Test.print_test_results(Test.DefaultTestSet(""))'`), stderr=DevNull)), "\n")[1]

@test msg == rstrip(msg)
