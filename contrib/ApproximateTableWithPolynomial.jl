module ApproximateTableWithPolynomial
    using FindMinimaxPolynomial.PolynomialPassingThroughIntervals:  # v0.4.0
        polynomial_passing_through_intervals!
    using MathOptInterface:  # v1.41.0
        MathOptInterface as MOI
    using Tulip:  # v0.9.7
        Optimizer
    function trunc_interval(::Type{T}, m::Integer, denominator::Integer) where {T <: AbstractFloat}
        function f(m::Int128, denom::Int)
            i = 1 // denom
            lower = m + i
            upper = (m + 1) - i
            map(T, (lower, upper))
        end
        f(Int128(m)::Int128, Int(denominator)::Int)
    end
    function trunc_intervals(::Type{T}, collection, denominator::Integer) where {T <: AbstractFloat}
        denom = Int(denominator)::Int
        function f(m::Integer)
            trunc_interval(T, m, denom)
        end
        it = Iterators.map(f, collection)
        collect(NTuple{2, T}, it)
    end
    function make_lp_optimizer()
      lp = Optimizer{BigFloat}()

      # Increase the Tulip iteration limit, just in case. The default limit is not good for
      # `BigFloat` problems.
      MOI.set(lp, MOI.RawOptimizerAttribute("IPM_IterationsLimit"), 1000)

      # Disable presolve
      MOI.set(lp, MOI.RawOptimizerAttribute("Presolve_Level"), 0)

      lp
    end
    function approximate_table_with_polynomial(
        monomials::AbstractVector{<:Integer},
        index_offset::Integer,
        values::AbstractVector{<:Integer},
        denominator::Integer,
    )
        polynomial_domain_without_offset = Base.OneTo(Int(length(values))::Int)
        polynomial_domain = polynomial_domain_without_offset .+ (Int(index_offset)::Int - 1)
        polynomial_domain_float = BigFloat.(polynomial_domain)
        target_intervals = trunc_intervals(BigFloat, values, denominator)
        lp = make_lp_optimizer()
        polynomial_passing_through_intervals!(
            lp, monomials, polynomial_domain_float, target_intervals,
        )
    end
end

function f(::Type{T}, n::Int, values; prec::Int = 1000) where {T <: AbstractFloat}
    function g()
        ApproximateTableWithPolynomial.approximate_table_with_polynomial(0:n, 0, values, 2^12)
    end
    pol_coefs = setprecision(g, BigFloat, prec)
    pol_coefs_f32 = T.(pol_coefs)
    (pol_coefs_f32...,)
end

# For `Base.Ryu`

const POW10_OFFSET_POLYNOMIAL = f(Float32, 8, Base.Ryu.POW10_OFFSET)

const POW10_OFFSET_2_POLYNOMIAL = f(Float64, 15, Base.Ryu.POW10_OFFSET_2)

function _pow10_offset(n::Integer, polynomial_coefficients)
    x = evalpoly(n, polynomial_coefficients)
    unsafe_trunc(UInt16, x)
end
function pow10_offset(n::Integer)
    _pow10_offset(n, POW10_OFFSET_POLYNOMIAL)
end
function pow10_offset_2(n::Integer)
    _pow10_offset(n, POW10_OFFSET_2_POLYNOMIAL)
end

using Test

@testset "`POW10_OFFSET` via polynomial approximation" begin
    @testset "f: $f" for (f, t) ∈ ((pow10_offset, Base.Ryu.POW10_OFFSET), (pow10_offset_2, Base.Ryu.POW10_OFFSET_2))
        for i ∈ 0:(length(t) - 1)
            @test f(i) === t[begin + i]
        end
    end
end

print("const ")
@show POW10_OFFSET_POLYNOMIAL
print("const ")
@show POW10_OFFSET_2_POLYNOMIAL
