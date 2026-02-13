include("../show_type.jl")

using Test

Base.@kwdef struct Nested{A,B}
    num::Int = 1
end
nest_val(na, nb, ::Val{1}) = Nested{na, nb}()
nest_val(na, nb, ::Val{n}) where {n} = nest_val(Nested{na, nb}, Nested{na, nb}, Val(n-1))
nest_val(na, nb, n::Int) = nest_val(na, nb, Val(n))
nest_val(n) = nest_val(1, 1, n)
foo1(t::Nested) = error("oops")
foo2(t::Nested) = foo1(t)
foo3(t::Nested) = foo2(t)
foo4(t::Nested) = foo3(t)
foo5(t::Nested) = foo4(t)
foo6(t::Nested) = foo5(t)
foo7(t::Nested) = foo6(t)
foo8(t::Nested) = foo7(t)
foo9(t::Nested) = foo8(t)
foo10(t::Nested) = foo9(t)

@testset "deeply nested type printing scales well" begin
    # Warmup: run a range of nesting depths so all code paths are compiled
    for i in 1:20
        Base_show(devnull, typeof(nest_val(i)))
    end

    times = Float64[]
    for i in 1:20
        t = @elapsed Base_show(devnull, typeof(nest_val(i)))
        push!(times, t)
    end

    # All 20 levels should complete within a reasonable time bound.
    # Without the fix, nest_val(20) alone takes > 3 seconds and
    # allocates over 1 GiB; with the fix it should be sub-millisecond.
    @test all(t -> t < 0.1, times)

    # The deepest levels should not be dramatically slower than the
    # shallowest (certainly not exponentially so).
    @test times[end] / max(times[1], 1e-9) < 1000
end

using BenchmarkTools
@btime Base.show(devnull, Dict{String, Any})
# @btime Base_show(devnull, Dict{String, Any})
