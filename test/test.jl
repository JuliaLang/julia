# This file is a part of Julia. License is MIT: http://julialang.org/license

using Test

macro pass(body)
    :(@assert(isa(@test($body), Test.Pass)))
end

macro pass_throws(t, body)
    :(@assert(isa(@test_throws($t, $body), Test.Pass)))
end

# test file to test testing

# Test @test
@pass true
@pass 1 == 1
@pass 1 != 2
@pass strip("\t  hi   \n") == "hi"
@pass strip("\t  this should fail   \n") != "hi"

a = Array(Float64, 2, 2, 2, 2, 2)
a[1,1,1,1,1] = 10
@pass a[1,1,1,1,1] == 10
@pass a[1,1,1,1,1] != 2

@pass rand() != rand()

# Test printing of Pass results
# Pass - constant
@pass contains(sprint(show, @test true), "Expression: true")
# Pass - expression
@pass contains(sprint(show, @test 10 == 2*5), "Evaluated: 10 == 10")
@pass contains(sprint(show, @test !false), "Expression: !false")
# Pass - exception
@pass contains(sprint(show, @test_throws ErrorException error()),
                "Thrown: ErrorException")

ts = @testset begin
    # Fail - wrong exception
    @test_throws OverflowError error()
    # Fail - no exception
    @test_throws OverflowError 1 + 1
    # Fail - const
    @test false
    # Fail - comparison
    @test 1+1 == 2+2
end
for result in ts.results
    @assert isa(result, Test.Fail)
end
@pass contains(sprint(show, ts.results[1]), "Thrown: ErrorException")
@pass contains(sprint(show, ts.results[2]), "No exception thrown")
@pass contains(sprint(show, ts.results[3]), "Evaluated: false")
@pass contains(sprint(show, ts.results[4]), "Evaluated: 2 == 4")

@pass Test.finish(Test.FallbackTestSet()) != nothing

# Test entirely empty test set
ts = @testset "outer" begin
    @testset "inner" begin
    end
end

@pass length(ts.results) == 1
@pass isempty(ts.results[1].results)

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

count = Test.get_test_counts(ts)
@assert count[4] == 24 # passes
@assert count[5] == 6  # fails
@assert count[6] == 6  # errors

# Test @test_approx_eq
# TODO
@assert isapprox(.1+.1+.1, .3)
@assert !isapprox(.1+.1+.1, .4)

@pass_throws ErrorException Test.test_approx_eq(ones(10),ones(11),1e-8,"a","b")
@pass_throws ErrorException Test.test_approx_eq(ones(10),zeros(10),1e-8,"a","b")

# Test @test_approx_eq_eps
# TODO

tss = @testset "@testset/for should return an array of testsets: $i" for i in 1:3
    @test true
end
@assert length(tss) == 3
@assert typeof(tss[1]) == Test.DefaultTestSet
@assert typeof(tss[1].results[1]) == Test.Pass

# import the methods needed for defining our own testset type
import Test: record, finish
using Test: get_testset_depth, get_testset
using Test: AbstractTestSet, Result, Pass, Fail, Error
immutable CustomTestSet <: Test.AbstractTestSet
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

@assert typeof(ts) == CustomTestSet
@assert ts.foo == 1
@assert ts.description == "Testing custom testsets"
@assert typeof(ts.results[1]) == CustomTestSet
@assert ts.results[1].description == "custom testset inner 1"
@assert ts.results[1].foo == 1
@assert typeof(ts.results[1].results[1]) == Pass
@assert typeof(ts.results[1].results[2]) == Fail
@assert typeof(ts.results[1].results[3]) == Error
@assert typeof(ts.results[1].results[4]) == Fail
@assert typeof(ts.results[1].results[5]) == Pass

@assert typeof(ts.results[2]) == CustomTestSet
@assert ts.results[2].description == "custom testset inner 2"
@assert ts.results[2].foo == 4
@assert typeof(ts.results[2].results[1]) == CustomTestSet
@assert ts.results[2].results[1].foo == 1
@assert typeof(ts.results[2].results[1].results[1]) == Pass
@assert typeof(ts.results[2].results[2]) == CustomTestSet
@assert ts.results[2].results[2].foo == 3

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
    @assert typeof(tss[i]) == CustomTestSet
    @assert tss[i].foo == 3
    for j in 1:3
        @assert typeof(tss[i].results[j]) == CustomTestSet
        @assert tss[i].results[j].foo == 1
        @assert typeof(tss[i].results[j].results[1]) == (iseven(i+j) ? Pass : Fail)
    end
    @assert typeof(tss[i].results[4]) == CustomTestSet
    @assert typeof(tss[i].results[4].results[1]) == (iseven(i) ? Pass : Fail)
end

# result stream
task = @schedule Test.results |> collect
sleep(0) # let iterator start so it doesn't miss any results

@testset "result stream" begin
    @test true
    @testset "nested" for i in 1:3
        @test 0 < i < 4
    end
    @test false
end

close(Test.results)
buffer = wait(task)
@assert length(buffer) == 9
@assert isa(buffer[1], Test.Pass)
