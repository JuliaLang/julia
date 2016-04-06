# This file is a part of Julia. License is MIT: http://julialang.org/license

###### Function Objects ("Functors") ######

# Note that function objects are merely used as internal machinery to
# enhance code reuse and improve performance of map/reduce.
# They are not exported.
# When function arguments can be inlined, the use of function objects
# can be removed.

abstract Func{N}

# Special purpose functors

immutable Predicate{F} <: Func{1}
    f::F
end
(pred::Predicate)(x) = pred.f(x)::Bool

immutable EqX{T} <: Func{1}
    x::T
end

(f::EqX)(y) = f.x == y

# More promote_op rules

promote_op{T<:Integer}(::typeof(^), ::Type{Bool}, ::Type{T}) = Bool
