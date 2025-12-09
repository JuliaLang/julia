# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for the match statement and @match macro

@testset "MatchError" begin
    @test MatchError <: Exception
    e = MatchError(42)
    @test e.value == 42
    @test sprint(showerror, e) == "MatchError: no pattern matched value 42"
end

@testset "Wildcard matcher" begin
    m = Base.Wildcard()
    @test Base.match(m, 1) == Dict{Symbol,Any}()
    @test Base.match(m, "hello") == Dict{Symbol,Any}()
    @test Base.match(m, nothing) == Dict{Symbol,Any}()
end

@testset "Literal matcher" begin
    m = Base.Literal(42)
    @test Base.match(m, 42) == Dict{Symbol,Any}()
    @test Base.match(m, 42.0) == Dict{Symbol,Any}()  # 42.0 == 42
    @test Base.match(m, 43) === nothing
    @test Base.match(m, "42") === nothing

    # String literals
    m_str = Base.Literal("hello")
    @test Base.match(m_str, "hello") == Dict{Symbol,Any}()
    @test Base.match(m_str, "world") === nothing
end

@testset "Capture matcher" begin
    m = Base.Capture(:x)
    @test Base.match(m, 42) == Dict{Symbol,Any}(:x => 42)
    @test Base.match(m, "hello") == Dict{Symbol,Any}(:x => "hello")
end

@testset "TypeMatcher" begin
    m = Base.TypeMatcher(Int)
    @test Base.match(m, 42) == Dict{Symbol,Any}()
    @test Base.match(m, 42.0) === nothing
    @test Base.match(m, "42") === nothing

    m_num = Base.TypeMatcher(Number)
    @test Base.match(m_num, 42) == Dict{Symbol,Any}()
    @test Base.match(m_num, 3.14) == Dict{Symbol,Any}()
    @test Base.match(m_num, "42") === nothing
end

@testset "TypedCapture matcher" begin
    m = Base.TypedCapture(:x, Int)
    @test Base.match(m, 42) == Dict{Symbol,Any}(:x => 42)
    @test Base.match(m, 42.0) === nothing

    m_num = Base.TypedCapture(:n, Number)
    @test Base.match(m_num, 42) == Dict{Symbol,Any}(:n => 42)
    @test Base.match(m_num, 3.14) == Dict{Symbol,Any}(:n => 3.14)
    @test Base.match(m_num, "42") === nothing
end

@testset "TupleMatcher" begin
    m = Base.TupleMatcher(Base.Capture(:a), Base.Capture(:b))
    @test Base.match(m, (1, 2)) == Dict{Symbol,Any}(:a => 1, :b => 2)
    @test Base.match(m, (1, 2, 3)) === nothing  # wrong length
    @test Base.match(m, [1, 2]) === nothing  # not a tuple
    @test Base.match(m, 1) === nothing  # not a tuple

    # Nested tuple
    m_nested = Base.TupleMatcher(
        Base.TupleMatcher(Base.Capture(:a), Base.Capture(:b)),
        Base.Capture(:c)
    )
    @test Base.match(m_nested, ((1, 2), 3)) == Dict{Symbol,Any}(:a => 1, :b => 2, :c => 3)

    # With literals
    m_lit = Base.TupleMatcher(Base.Literal(1), Base.Capture(:x))
    @test Base.match(m_lit, (1, 42)) == Dict{Symbol,Any}(:x => 42)
    @test Base.match(m_lit, (2, 42)) === nothing
end

@testset "Alternation matcher" begin
    m = Base.Alternation(Base.Literal(1), Base.Literal(2))
    @test Base.match(m, 1) == Dict{Symbol,Any}()
    @test Base.match(m, 2) == Dict{Symbol,Any}()
    @test Base.match(m, 3) === nothing

    # With captures
    m_cap = Base.Alternation(
        Base.TupleMatcher(Base.Literal(:a), Base.Capture(:x)),
        Base.TupleMatcher(Base.Literal(:b), Base.Capture(:x))
    )
    @test Base.match(m_cap, (:a, 1)) == Dict{Symbol,Any}(:x => 1)
    @test Base.match(m_cap, (:b, 2)) == Dict{Symbol,Any}(:x => 2)
    @test Base.match(m_cap, (:c, 3)) === nothing
end

@testset "CallMatcher" begin
    # Test with Some type
    m = Base.CallMatcher(Some, Base.Capture(:x))
    @test Base.match(m, Some(42)) == Dict{Symbol,Any}(:x => 42)
    @test Base.match(m, 42) === nothing
    @test Base.match(m, nothing) === nothing

    # With literal
    m_lit = Base.CallMatcher(Some, Base.Literal(1))
    @test Base.match(m_lit, Some(1)) == Dict{Symbol,Any}()
    @test Base.match(m_lit, Some(2)) === nothing
end

@testset "@match macro basics" begin
    # Literal matching
    @test (@match 1 begin
        1 -> "one"
        2 -> "two"
        _ -> "other"
    end) == "one"

    @test (@match 2 begin
        1 -> "one"
        2 -> "two"
        _ -> "other"
    end) == "two"

    @test (@match 3 begin
        1 -> "one"
        2 -> "two"
        _ -> "other"
    end) == "other"
end

@testset "@match variable capture" begin
    result = @match 42 begin
        x -> x + 1
    end
    @test result == 43

    result = @match (1, 2) begin
        (a, b) -> a + b
    end
    @test result == 3
end

@testset "@match type constraints" begin
    test_type_match(x) = @match x begin
        n::Int -> "int: $n"
        s::String -> "string: $s"
        _ -> "other"
    end

    @test test_type_match(42) == "int: 42"
    @test test_type_match("hello") == "string: hello"
    @test test_type_match(3.14) == "other"
end

@testset "@match no match throws MatchError" begin
    @test_throws MatchError @match 3 begin
        1 -> "one"
        2 -> "two"
    end
end

@testset "@match value escaping" begin
    expected = 42
    result = @match 42 begin
        $expected -> "matched"
        _ -> "not matched"
    end
    @test result == "matched"

    result = @match 43 begin
        $expected -> "matched"
        _ -> "not matched"
    end
    @test result == "not matched"
end

@testset "@match tuple destructuring" begin
    result = @match (1, (2, 3)) begin
        (a, (b, c)) -> a + b + c
    end
    @test result == 6

    result = @match (1, 2, 3) begin
        (1, x, y) -> x + y
        _ -> 0
    end
    @test result == 5
end

@testset "match statement if guards" begin
    # Basic guard
    result = match 5
        n if n > 0 -> "positive"
        n if n < 0 -> "negative"
        _ -> "zero"
    end
    @test result == "positive"

    result = match -3
        n if n > 0 -> "positive"
        n if n < 0 -> "negative"
        _ -> "zero"
    end
    @test result == "negative"

    result = match 0
        n if n > 0 -> "positive"
        n if n < 0 -> "negative"
        _ -> "zero"
    end
    @test result == "zero"

    # Guard with tuple destructuring
    result = match (3, 4)
        (a, b) if a + b > 5 -> "sum > 5"
        (a, b) -> "sum <= 5"
    end
    @test result == "sum > 5"

    result = match (1, 2)
        (a, b) if a + b > 5 -> "sum > 5"
        (a, b) -> "sum <= 5"
    end
    @test result == "sum <= 5"

    # Guard that fails, falling through to next arm
    result = match 10
        x if x < 5 -> "small"
        x if x < 15 -> "medium"
        _ -> "large"
    end
    @test result == "medium"
end

@testset "match statement inline destructuring" begin
    # Basic inline match-destructuring
    match (x, y) = (10, 20)
    @test x == 10
    @test y == 20

    # Nested tuple destructuring
    match (a, (b, c)) = (1, (2, 3))
    @test a == 1
    @test b == 2
    @test c == 3

    # MatchError when pattern doesn't match
    @test_throws MatchError match (p, q) = (1, 2, 3)

    # With type constraint
    match (m::Int, n::Int) = (5, 6)
    @test m == 5
    @test n == 6

    # Type constraint mismatch throws MatchError
    @test_throws MatchError match (r::String, s) = (1, 2)
end

