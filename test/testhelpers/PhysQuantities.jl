# This file is a part of Julia. License is MIT: https://julialang.org/license

module PhysQuantities

export PhysQuantity

# Mimic a quantity with a physical unit that is not convertible to a real number
struct PhysQuantity{n,T}   # n is like the exponent of the unit
    val::T
end
PhysQuantity{n}(x::T) where {n,T} = PhysQuantity{n,T}(x)
Base.zero(::Type{PhysQuantity{n,T}}) where {n,T} = PhysQuantity{n,T}(zero(T))
Base.zero(x::PhysQuantity) = zero(typeof(x))
Base.:+(x::PhysQuantity{n}, y::PhysQuantity{n}) where n = PhysQuantity{n}(x.val + y.val)
Base.:-(x::PhysQuantity{n}, y::PhysQuantity{n}) where n = PhysQuantity{n}(x.val - y.val)
Base.:*(x::PhysQuantity{n,T}, y::Int) where {n,T} = PhysQuantity{n}(x.val*y)
Base.:/(x::PhysQuantity{n,T}, y::Int) where {n,T} = PhysQuantity{n}(x.val/y)
Base.:*(x::PhysQuantity{n1,S}, y::PhysQuantity{n2,T}) where {n1,n2,S,T} =
    PhysQuantity{n1+n2}(x.val*y.val)
Base.:/(x::PhysQuantity{n1,S}, y::PhysQuantity{n2,T}) where {n1,n2,S,T} =
    PhysQuantity{n1-n2}(x.val/y.val)
Base.convert(::Type{PhysQuantity{0,T}}, x::Int) where T = PhysQuantity{0}(convert(T, x))
Base.convert(::Type{P}, ::Int) where P<:PhysQuantity =
    error("Int is incommensurate with PhysQuantity")

end #module
