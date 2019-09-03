# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Base.MathConstants

Module containing the mathematical constants.
See [`π`](@ref), [`ℯ`](@ref), [`γ`](@ref), [`φ`](@ref) and [`catalan`](@ref).
"""
module MathConstants

export π, pi, ℯ, e, γ, eulergamma, catalan, φ, golden

Base.@irrational π        3.14159265358979323846  pi
Base.@irrational ℯ        2.71828182845904523536  exp(big(1))
Base.@irrational γ        0.57721566490153286061  euler
Base.@irrational φ        1.61803398874989484820  (1+sqrt(big(5)))/2
Base.@irrational catalan  0.91596559417721901505  catalan

# aliases
"""
    π
    pi

The constant pi.

# Examples
```jldoctest
julia> pi
π = 3.1415926535897...
```
"""
π, const pi = π

"""
    ℯ
    e

The constant ℯ.

# Examples
```jldoctest
julia> ℯ
ℯ = 2.7182818284590...
```
"""
ℯ, const e = ℯ

"""
    γ
    eulergamma

Euler's constant.

# Examples
```jldoctest
julia> Base.MathConstants.eulergamma
γ = 0.5772156649015...
```
"""
γ, const eulergamma = γ

"""
    φ
    golden

The golden ratio.

# Examples
```jldoctest
julia> Base.MathConstants.golden
φ = 1.6180339887498...
```
"""
φ, const golden = φ

"""
    catalan

Catalan's constant.

# Examples
```jldoctest
julia> Base.MathConstants.catalan
catalan = 0.9159655941772...
```
"""
catalan

# loop over types to prevent ambiguities for ^(::Number, x)
for T in (AbstractIrrational, Rational, Integer, Number, Complex)
    Base.:^(::Irrational{:ℯ}, x::T) = exp(x)
end
Base.literal_pow(::typeof(^), ::Irrational{:ℯ}, ::Val{p}) where {p} = exp(p)

Base.log(::Irrational{:ℯ}) = 1 # use 1 to correctly promote expressions like log(x)/log(ℯ)
Base.log(::Irrational{:ℯ}, x::Number) = log(x)

end # module
