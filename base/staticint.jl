
"""
    StaticInt(N::Int) -> StaticInt{N}()

A statically sized `Int`. Use `StaticInt(N)` instead of `Val(N)` when you want it to behave like a number.
"""
struct StaticInt{N} <: Integer
    StaticInt{N}() where {N} = new{N::Int}()
end

const Zero = StaticInt{0}
const One = StaticInt{1}

Base.show(io::IO, ::StaticInt{N}) where {N} = print(io, "static($N)")

StaticInt(N::Int) = StaticInt{N}()
StaticInt(N::Integer) = StaticInt(convert(Int, N))
StaticInt(::StaticInt{N}) where {N} = StaticInt{N}()
StaticInt(::Val{N}) where {N} = StaticInt{N}()
# Base.Val(::StaticInt{N}) where {N} = Val{N}()
Base.convert(::Type{T}, ::StaticInt{N}) where {T<:Number,N} = convert(T, N)
Base.Bool(x::StaticInt{N}) where {N} = Bool(N)
Base.BigInt(x::StaticInt{N}) where {N} = BigInt(N)
Base.Integer(x::StaticInt{N}) where {N} = x
(::Type{T})(x::StaticInt{N}) where {T<:Integer,N} = T(N)
(::Type{T})(x::Int) where {T<:StaticInt} = StaticInt(x)
Base.convert(::Type{StaticInt{N}}, ::StaticInt{N}) where {N} = StaticInt{N}()

Base.promote_rule(::Type{<:StaticInt}, ::Type{T}) where {T<:Number} = promote_type(Int, T)
function Base.promote_rule(::Type{<:StaticInt}, ::Type{T}) where {T<:AbstractIrrational}
    return promote_type(Int, T)
end
# Base.promote_rule(::Type{T}, ::Type{<:StaticInt}) where {T <: AbstractIrrational} = promote_rule(T, Int)
for (S, T) in [(:Complex, :Real), (:Rational, :Integer), (:(Base.TwicePrecision), :Any)]
    @eval function Base.promote_rule(::Type{$S{T}}, ::Type{<:StaticInt}) where {T<:$T}
        return promote_type($S{T}, Int)
    end
end
function Base.promote_rule(::Type{Union{Nothing,Missing}}, ::Type{<:StaticInt})
    return Union{Nothing,Missing,Int}
end
function Base.promote_rule(::Type{T}, ::Type{<:StaticInt}) where {T>:Union{Missing,Nothing}}
    return promote_type(T, Int)
end
Base.promote_rule(::Type{T}, ::Type{<:StaticInt}) where {T>:Nothing} = promote_type(T, Int)
Base.promote_rule(::Type{T}, ::Type{<:StaticInt}) where {T>:Missing} = promote_type(T, Int)
for T in [:Bool, :Missing, :BigFloat, :BigInt, :Nothing, :Any]
    # let S = :Any
    @eval begin
        function Base.promote_rule(::Type{S}, ::Type{$T}) where {S<:StaticInt}
            return promote_type(Int, $T)
        end
        function Base.promote_rule(::Type{$T}, ::Type{S}) where {S<:StaticInt}
            return promote_type($T, Int)
        end
    end
end
Base.promote_rule(::Type{<:StaticInt}, ::Type{<:StaticInt}) = Int
Base.:(%)(::StaticInt{N}, ::Type{Integer}) where {N} = N

Base.eltype(::Type{T}) where {T<:StaticInt} = Int
Base.iszero(::Zero) = true
Base.iszero(::StaticInt) = false
Base.isone(::One) = true
Base.isone(::StaticInt) = false
Base.zero(::Type{T}) where {T<:StaticInt} = Zero()
Base.one(::Type{T}) where {T<:StaticInt} = One()

for T in [:Real, :Rational, :Integer]
    @eval begin
        @inline Base.:(+)(i::$T, ::Zero) = i
        @inline Base.:(+)(i::$T, ::StaticInt{M}) where {M} = i + M
        @inline Base.:(+)(::Zero, i::$T) = i
        @inline Base.:(+)(::StaticInt{M}, i::$T) where {M} = M + i
        @inline Base.:(-)(i::$T, ::Zero) = i
        @inline Base.:(-)(i::$T, ::StaticInt{M}) where {M} = i - M
        @inline Base.:(*)(i::$T, ::Zero) = Zero()
        @inline Base.:(*)(i::$T, ::One) = i
        @inline Base.:(*)(i::$T, ::StaticInt{M}) where {M} = i * M
        @inline Base.:(*)(::Zero, i::$T) = Zero()
        @inline Base.:(*)(::One, i::$T) = i
        @inline Base.:(*)(::StaticInt{M}, i::$T) where {M} = M * i
    end
end
@inline Base.:(+)(::Zero, ::Zero) = Zero()
@inline Base.:(+)(::Zero, ::StaticInt{M}) where {M} = StaticInt{M}()
@inline Base.:(+)(::StaticInt{M}, ::Zero) where {M} = StaticInt{M}()

@inline Base.:(-)(::StaticInt{M}) where {M} = StaticInt{-M}()
@inline Base.:(-)(::StaticInt{M}, ::Zero) where {M} = StaticInt{M}()

@inline Base.:(*)(::Zero, ::Zero) = Zero()
@inline Base.:(*)(::One, ::Zero) = Zero()
@inline Base.:(*)(::Zero, ::One) = Zero()
@inline Base.:(*)(::One, ::One) = One()
@inline Base.:(*)(::StaticInt{M}, ::Zero) where {M} = Zero()
@inline Base.:(*)(::Zero, ::StaticInt{M}) where {M} = Zero()
@inline Base.:(*)(::StaticInt{M}, ::One) where {M} = StaticInt{M}()
@inline Base.:(*)(::One, ::StaticInt{M}) where {M} = StaticInt{M}()
for f in [:(+), :(-), :(*), :(/), :(÷), :(%), :(<<), :(>>), :(>>>), :(&), :(|), :(⊻)]
    @eval @generated function Base.$f(::StaticInt{M}, ::StaticInt{N}) where {M,N}
        return Expr(:call, Expr(:curly, :StaticInt, $f(M, N)))
    end
end
for f in [:(<<), :(>>), :(>>>)]
    @eval begin
        @inline Base.$f(::StaticInt{M}, x::UInt) where {M} = $f(M, x)
        @inline Base.$f(x::Integer, ::StaticInt{M}) where {M} = $f(x, M)
    end
end
for f in [:(==), :(!=), :(<), :(≤), :(>), :(≥)]
    @eval begin
        @inline Base.$f(::StaticInt{M}, ::StaticInt{N}) where {M,N} = $f(M, N)
        @inline Base.$f(::StaticInt{M}, x::Int) where {M} = $f(M, x)
        @inline Base.$f(x::Int, ::StaticInt{M}) where {M} = $f(x, M)
    end
end

@inline function maybe_static(f::F, g::G, x) where {F,G}
    L = f(x)
    if L === nothing
        return g(x)
    else
        return static(L)
    end
end

@inline Base.widen(::StaticInt{N}) where {N} = widen(N)

Base.UnitRange{T}(start::StaticInt, stop) where {T<:Real} = UnitRange{T}(T(start), T(stop))
Base.UnitRange{T}(start, stop::StaticInt) where {T<:Real} = UnitRange{T}(T(start), T(stop))
function Base.UnitRange{T}(start::StaticInt, stop::StaticInt) where {T<:Real}
    return UnitRange{T}(T(start), T(stop))
end

Base.UnitRange(start::StaticInt, stop) = UnitRange(Int(start), stop)
Base.UnitRange(start, stop::StaticInt) = UnitRange(start, Int(stop))
Base.UnitRange(start::StaticInt, stop::StaticInt) = UnitRange(Int(start), Int(stop))
