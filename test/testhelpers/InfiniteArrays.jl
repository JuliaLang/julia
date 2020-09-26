# This file is a part of Julia. License is MIT: https://julialang.org/license

# InfiniteArrays (arrays with infinite size)

# This test file is designed to exercise support for generic sizing,
# even though infinite arrays aren't implemented in Base.

module InfiniteArrays

export OneToInf, Infinity

"""
   Infinity()

represents infinite cardinality. Note that `Infinity <: Integer` to support
being treated as an index.
"""
struct Infinity <: Integer end

"""
    OneToInf(n)

Define an `AbstractInfUnitRange` that behaves like `1:∞`, with the added
distinction that the limits are guaranteed (by the type system) to
be 1 and ∞.
"""
struct OneToInf{T<:Integer} <: AbstractUnitRange{T} end

OneToInf() = OneToInf{Int}()

Base.axes(r::OneToInf) = (r,)
Base.unsafe_indices(r::OneToInf) = (r,)
Base.unsafe_length(r::OneToInf) = Infinity()
Base.size(r::OneToInf) = (Infinity(),)
Base.first(r::OneToInf{T}) where {T} = oneunit(T)

end