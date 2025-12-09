# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for declared exceptions (Except type, postfix ?, match?)

@testset "Except type" begin
    # Success case
    e = Except{Int, BoundsError}(42)
    @test !is_exception(e)
    @test unwrap(e) == 42
    @test get_exception(e) === nothing

    # Exception case
    ex = Base.except_exception(Except{Int, BoundsError}, BoundsError([1,2,3], 5))
    @test is_exception(ex)
    @test get_exception(ex) isa BoundsError
    @test_throws BoundsError unwrap(ex)
end

@testset "AnyExcept alias" begin
    e = AnyExcept{KeyError}("test value")
    @test typeof(e) == Except{Any, KeyError}
    @test !is_exception(e)
    @test unwrap(e) == "test value"
end

@testset "except_value and except_exception" begin
    # except_value creates success
    e1 = Base.except_value(Except{Int, KeyError}, 42)
    @test !is_exception(e1)
    @test unwrap(e1) == 42

    # except_exception creates exception
    e2 = Base.except_exception(Except{Int, KeyError}, KeyError(:foo))
    @test is_exception(e2)
    @test get_exception(e2) isa KeyError
end

@testset "postfix ? operator" begin
    # Success case - unwraps value
    e1 = Except{Int, BoundsError}(42)
    @test e1? == 42

    # Exception case - forwards exception (returns Except)
    e2 = Base.except_exception(Except{Int, BoundsError}, BoundsError([1,2], 3))
    result = e2?
    @test result isa Except
    @test is_exception(result)
end

@testset "match? basic" begin
    # Success case returns unwrapped value
    e1 = Except{Int, BoundsError}(42)
    result = match? e1
        ::BoundsError -> "bounds"
        _ -> "other"
    end
    @test result == 42

    # Exception case matches pattern
    e2 = Base.except_exception(Except{Int, BoundsError}, BoundsError([1,2], 3))
    result = match? e2
        ::BoundsError -> "bounds"
        _ -> "other"
    end
    @test result == "bounds"
end

@testset "match? multiple exception types" begin
    e_bounds = Base.except_exception(Except{Int, Exception}, BoundsError([1,2], 3))
    e_key = Base.except_exception(Except{Int, Exception}, KeyError(:foo))
    e_other = Base.except_exception(Except{Int, Exception}, ErrorException("test"))

    function classify(e)
        match? e
            ::BoundsError -> "bounds"
            ::KeyError -> "key"
            _ -> "other"
        end
    end

    @test classify(e_bounds) == "bounds"
    @test classify(e_key) == "key"
    @test classify(e_other) == "other"
end

@testset "match? with capture" begin
    e = Base.except_exception(Except{Int, BoundsError}, BoundsError([1,2,3], 5))

    # Capture exception value
    result = match? e
        err::BoundsError -> (err.a, err.i)
        _ -> nothing
    end
    @test result == ([1,2,3], 5)
end

@testset "match? wildcard" begin
    e = Base.except_exception(Except{Int, ErrorException}, ErrorException("test"))

    result = match? e
        _ -> "caught any"
    end
    @test result == "caught any"
end

@testset "match? no match rethrows" begin
    e = Base.except_exception(Except{Int, ErrorException}, ErrorException("test"))

    # Only BoundsError arm - should rethrow ErrorException
    @test_throws ErrorException match? e
        ::BoundsError -> "bounds"
    end
end

@testset "Except show" begin
    e1 = Except{Int, BoundsError}(42)
    @test contains(sprint(show, e1), "42")
    @test contains(sprint(show, e1), "Except")

    e2 = Base.except_exception(Except{Int, BoundsError}, BoundsError([1], 2))
    @test contains(sprint(show, e2), "exception")
    @test contains(sprint(show, e2), "BoundsError")
end
