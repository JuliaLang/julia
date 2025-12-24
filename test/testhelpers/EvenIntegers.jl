"""
The even integers, an example of set with an additive identity and closed under
addition and multiplication, but lacking a multiplicative identity, a
[*rng*](https://en.wikipedia.org/wiki/Rng_(algebra)).
"""
module EvenIntegers
    export EvenInteger

    struct EvenInteger{T <: Integer} <: Integer
        x::T
        function EvenInteger(x::Integer)
            if isodd(x)
                throw(ArgumentError("can't convert odd integer to even integer"))
            end
            new{typeof(x)}(x)
        end
    end
    function EvenInteger(x::EvenInteger)
        x
    end
    function EvenInteger{T}(x::EvenInteger{T}) where {T <: Integer}
        x
    end
    function EvenInteger{T}(x::T) where {T <: Integer}
        EvenInteger(x)
    end
    function EvenInteger{T}(x::Integer) where {T <: Integer}
        throw(ArgumentError("not implemented"))
    end
    function Base.Int(n::EvenInteger)
        Int(n.x)
    end
    function Base.iseven(::EvenInteger)
        true
    end
    function Base.isodd(::EvenInteger)
        false
    end
    function Base.iszero(n::EvenInteger)
        iszero(n.x)
    end
    function Base.isone(::EvenInteger)
        false
    end
    function Base.zero(n::EvenInteger)
        EvenInteger(zero(n.x))
    end
    function Base.zero(::Type{EvenInteger{T}}) where {T <: Integer}
        EvenInteger(zero(T))
    end
    function Base.:(==)(l::EvenInteger, r::EvenInteger)
        l.x == r.x
    end
    function Base.:(<)(l::EvenInteger, r::EvenInteger)
        l.x < r.x
    end
    function Base.promote_rule(::Type{EvenInteger{L}}, ::Type{EvenInteger{R}}) where {L <: Integer, R <: Integer}
        EvenInteger{promote_type(L, R)}
    end
    function Base.promote_rule(::Type{EvenInteger{L}}, ::Type{R}) where {L <: Integer, R <: Integer}
        promote_type(L, R)
    end
    function Base.:(+)(l::EvenInteger, r::EvenInteger)
        EvenInteger(l.x + r.x)
    end
    function Base.:(*)(l::EvenInteger, r::EvenInteger)
        EvenInteger(l.x * r.x)
    end
    function Base.:(-)(n::EvenInteger)
        EvenInteger(-n.x)
    end
    function Base.:(-)(l::EvenInteger, r::EvenInteger)
        l + (-r)
    end
    function right_shift(l::EvenInteger, r::Integer)
        l.x >> r
    end
    function Base.:(>>)(l::EvenInteger, r::Integer)
        right_shift(l, r)
    end
    function Base.:(>>)(l::EvenInteger, r::Int)  # resolve dispatch ambiguity
        right_shift(l, r)
    end
    function Base.trailing_zeros(n::EvenInteger)
        trailing_zeros(n.x)
    end
end
