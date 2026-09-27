# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
A 20-bit unsigned integer. Its `sizeof` exceeds its width, so code that takes a
bit width from `8 * sizeof` rather than `Core.bitsizeof` gets it wrong.
"""
module UInt20s
    export UInt20

    using Core.Intrinsics: trunc_int, zext_int, add_int, sub_int, mul_int, udiv_int, urem_int,
        ult_int, ule_int, shl_int, lshr_int, ctlz_int

    primitive type UInt20 <: Unsigned 20 end

    UInt20(x::UInt64) = trunc_int(UInt20, x)
    UInt20(x::Integer) = UInt20(x % UInt64)
    Base.UInt64(x::UInt20) = zext_int(UInt64, x)
    Base.Int(x::UInt20) = Int(UInt64(x))
    Base.rem(x::Integer, ::Type{UInt20}) = UInt20(x)
    Base.rem(x::UInt20, ::Type{T}) where {T<:Integer} = UInt64(x) % T
    Base.rem(x::UInt20, ::Type{UInt20}) = x
    Base.promote_rule(::Type{UInt20}, ::Type{Int}) = Int
    Base.promote_rule(::Type{UInt20}, ::Type{UInt64}) = UInt64
    Base.widen(::Type{UInt20}) = UInt64
    Base.typemin(::Type{UInt20}) = UInt20(0)
    Base.typemax(::Type{UInt20}) = UInt20(0xfffff)
    Base.hash(x::UInt20, h::UInt) = hash(UInt64(x), h)

    for (f, op) in ((:+, add_int), (:-, sub_int), (:*, mul_int), (:div, udiv_int), (:rem, urem_int),
                    (:+%, add_int), (:-%, sub_int), (:*%, mul_int), (:<, ult_int), (:<=, ule_int))
        @eval Base.$f(a::UInt20, b::UInt20) = $op(a, b)
    end
    Base.:(<<)(x::UInt20, n::UInt) = n < 20 ? shl_int(x, n) : UInt20(0)
    Base.:(>>)(x::UInt20, n::UInt) = n < 20 ? lshr_int(x, n) : UInt20(0)
    Base.:(>>>)(x::UInt20, n::UInt) = x >> n
    Base.leading_zeros(x::UInt20) = Int(ctlz_int(x))
    Base.top_set_bit(x::UInt20) = 20 - leading_zeros(x)
end
