# Performance tests for budget-limited type printing.
include("../show_type.jl")

using Test

Base.@kwdef struct Nested{A,B}
    num::Int = 1
end
nest_val(na, nb, ::Val{1}) = Nested{na, nb}()
nest_val(na, nb, ::Val{n}) where {n} = nest_val(Nested{na, nb}, Nested{na, nb}, Val(n-1))
nest_val(na, nb, n::Int) = nest_val(na, nb, Val(n))
nest_val(n) = nest_val(1, 1, n)

# Helper: show a type with a given budget, writing to devnull.
function show_budgeted_devnull(T, budget::Int)
    io = IOContext(devnull, :type_budget => Ref(budget))
    Base_show(io, T)
end

@testset "deeply nested type printing scales well with budget" begin
    # Warmup
    for i in 1:20
        show_budgeted_devnull(typeof(nest_val(i)), 80)
    end

    times = Float64[]
    for i in 1:20
        t = @elapsed show_budgeted_devnull(typeof(nest_val(i)), 80)
        push!(times, t)
    end

    # All 20 levels should complete within a reasonable time bound.
    # Without the budget, nest_val(20) alone takes > 3 seconds and
    # allocates over 1 GiB; with the budget it should be sub-millisecond.
    @test all(t -> t < 0.1, times)

    # The deepest levels should not be dramatically slower than the
    # shallowest (certainly not exponentially so).
    @test times[end] / max(times[1], 1e-9) < 1000
end

@testset "unbounded printing of shallow types is fast" begin
    # Without any budget, simple types should still print quickly.
    for _ in 1:100
        sprint(Base_show, Dict{String, Any})
    end
    t = @elapsed for _ in 1:1000
        sprint(Base_show, Dict{String, Any})
    end
    @test t < 1.0
end
