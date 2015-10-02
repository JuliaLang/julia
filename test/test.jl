# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

# test file to test testing

# Test @test
@test true
@test 1 == 1
@test 1 != 2
@test strip("\t  hi   \n") == "hi"
@test strip("\t  this should fail   \n") != "hi"

a = Array(Float64, 2, 2, 2, 2, 2)
a[1,1,1,1,1] = 10
@test a[1,1,1,1,1] == 10
@test a[1,1,1,1,1] != 2

@test rand() != rand()

sprint(show, @test true)
sprint(show, @test 10 == 2*5)
sprint(show, @test !false)

OLD_STDOUT = STDOUT
catch_out = IOStream("")
rd, wr = redirect_stdout()

@testset "no errors" begin
    @test true
    @test 1 == 1
end

try

@testset "outer" begin
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
        @testset "errrrr" begin
            @test "not bool"
            @test error()
        end

        error("exceptions in testsets should be caught")
        @test 1 == 1 # this test will not be run
    end

    @testset "loop with desc" begin
        @testloop "loop1 $T" for T in (Float32, Float64)
            @test 1 == T(1)
        end
    end
    @testset "loops without desc" begin
        @testloop for T in (Float32, Float64)
            @test 1 == T(1)
        end
        @testloop for T in (Float32, Float64), S in (Int32,Int64)
            @test S(1) == T(1)
        end
    end
    srand(123)
    @testset "some loops fail" begin
        @testloop for i in 1:5
            @test i <= rand(1:10)
        end
        # should add 3 errors and 3 passing tests
        @testloop for i in 1:6
            iseven(i) || error("error outside of test")
            @test true # only gets run if the above passed
        end
    end
end
    # These lines shouldn't be called
    redirect_stdout(OLD_STDOUT)
    error("No exception was thrown!")
catch ex

    @test isa(ex, Test.TestSetException)
    @test ex.pass  == 24
    @test ex.fail  == 5
    @test ex.error == 6
end

# Test @test_approx_eq
# TODO
@test isapprox(.1+.1+.1, .3)
@test !isapprox(.1+.1+.1, .4)

@test_throws ErrorException Test.test_approx_eq(ones(10),ones(11),1e-8,"a","b")
@test_throws ErrorException Test.test_approx_eq(ones(10),zeros(10),1e-8,"a","b")

# Test @test_approx_eq_eps
# TODO

ts = @testset "@testset should return the testset" begin
    @test true
end
@test typeof(ts) == Base.Test.DefaultTestSet
@test typeof(ts.results[1]) == Base.Test.Pass

tss = @testloop "@testloop should return an array of testsets: $i" for i in 1:3
    @test true
end
@test length(tss) == 3
@test typeof(tss[1]) == Base.Test.DefaultTestSet
@test typeof(tss[1].results[1]) == Base.Test.Pass

# now we're done running tests with DefaultTestSet so we can go back to STDOUT
redirect_stdout(OLD_STDOUT)

immutable CustomTestSet <: Base.Test.AbstractTestSet
    description::AbstractString
    foo::Int
    results::Vector
    CustomTestSet(desc; foo=1) = new(desc, foo, [])
end

Base.Test.record(ts::CustomTestSet, child::Base.Test.AbstractTestSet) = push!(ts.results, child)
Base.Test.record(ts::CustomTestSet, res::Base.Test.Result) = push!(ts.results, res)
Base.Test.finish(ts::CustomTestSet) = ts

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
        # this testset should inherit the type and arguments
        @testset "custom testset inner 2 inner" begin
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
@test typeof(ts.results[1].results[1]) == Base.Test.Pass
@test typeof(ts.results[1].results[2]) == Base.Test.Fail
@test typeof(ts.results[1].results[3]) == Base.Test.Error
@test typeof(ts.results[1].results[4]) == Base.Test.Fail
@test typeof(ts.results[1].results[5]) == Base.Test.Pass

@test typeof(ts.results[2]) == CustomTestSet
@test ts.results[2].description == "custom testset inner 1"
@test ts.results[2].foo == 4
@test typeof(ts.results[2].results[1]) == CustomTestSet
@test ts.results[2].results[1].foo == 4
@test typeof(ts.results[2].results[1].results[1]) == Base.Test.Pass

# # test custom testset types on testloops
# tss = @testloop CustomTestSet "custom testloop $i" for i in 1:6
#     @test iseven(i)
# end
#
# for i in 1:6
#     @test typeof(tss[i]) == CustomTestSet
#     if iseven(i)
#         @test typeof(tss[i].results[1]) == Base.Test.Pass
#     else
#         @test typeof(tss[i].results[1]) == Base.Test.Fail
#     end
# end
