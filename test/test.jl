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
    end
end
    # These lines shouldn't be called
    redirect_stdout(OLD_STDOUT)
    error("No exception was thrown!")
catch ex
    redirect_stdout(OLD_STDOUT)

    @test isa(ex, Test.TestSetException)
    @test ex.pass  == 21
    @test ex.fail  == 5
    @test ex.error == 2
end

# Test @test_approx_eq
# TODO
@test isapprox(.1+.1+.1, .3)
@test !isapprox(.1+.1+.1, .4)

@test_throws ErrorException Test.test_approx_eq(ones(10),ones(11),1e-8,"a","b")
@test_throws ErrorException Test.test_approx_eq(ones(10),zeros(10),1e-8,"a","b")

# Test @test_approx_eq_eps
# TODO