# Tests for budget-limited type printing in a stacktrace-like context.
include("../show_type.jl")

using Test

Base.@kwdef struct Nested{A,B}
    num::Int = 1
end
nest_val(na, nb, ::Val{1}) = Nested{na, nb}()
nest_val(na, nb, ::Val{n}) where {n} = nest_val(Nested{na, nb}, Nested{na, nb}, Val(n-1))
nest_val(na, nb, n::Int) = nest_val(na, nb, Val(n))
nest_val(n) = nest_val(1, 1, n)

# Helper: show a type with a given budget and return the string.
function show_with_budget(T, budget::Int)
    buf = IOBuffer()
    io = IOContext(buf, :type_budget => Ref(budget))
    Base_show(io, T)
    return String(take!(buf))
end

@testset "budget-limited type printing in stacktrace context" begin
    @testset "deeply nested type without budget is fully expanded" begin
        T = typeof(nest_val(5))
        str_full = sprint(Base_show, T)
        @test !contains(str_full, "…")
        @test contains(str_full, "Nested{")
    end

    @testset "deeply nested type with budget is truncated" begin
        T = typeof(nest_val(10))
        str_limited = show_with_budget(T, 80)
        @test contains(str_limited, "…")
        @test contains(str_limited, "Nested{")
    end

    @testset "budget limits output size for very deep nesting" begin
        T = typeof(nest_val(10))
        str_full = sprint(Base_show, T)
        str_limited = show_with_budget(T, 80)
        @test sizeof(str_limited) < sizeof(str_full)
    end

    @testset "simulated stacktrace: type_depth_limit after budgeted show" begin
        # In real stacktrace display, show_tuple_as_call would set up the budget,
        # then type_limited_string_from_context would apply type_depth_limit.
        # Here we simulate this two-step pipeline.
        T = typeof(nest_val(8))

        # Step 1: Budget-limited show (prevents exponential blowup)
        str = show_with_budget(T, 80)
        @test contains(str, "…")

        # Step 2: type_depth_limit for width fitting (cosmetic)
        str_fitted = Base_type_depth_limit(str, 60)
        @test sizeof(str_fitted) <= sizeof(str)
    end

    @testset "SubArray in stacktrace context" begin
        T = typeof(view(rand(3,3), 1:2, :))

        str_full = sprint(Base_show, T)
        @test !contains(str_full, "…")

        # SubArray has many type parameters; use a budget of 3 to force truncation
        str_limited = show_with_budget(T, 3)
        @test contains(str_limited, "…")
        @test sizeof(str_limited) < sizeof(str_full)
    end
end
